;;; xpath.el --- XPATH implementation

;; Copyright (C) 2001  Alex Schroeder <alex@gnu.org>

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Oliver Scholz <epameinondas@gmx.de>
;; Version: 1.0.0
;; Keywords: xml
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?XmlParser
;; Version: $Id: xpath.el,v 1.1 2003/12/16 00:32:00 egoge Exp egoge $

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; If you are working with XML documents, you may parse the documents
;; using the XML parser included with Emacs (xml.el), and pass the data
;; structure to the DOM implementation (dom.el).  You can then use XPATH
;; to find DOM nodes.

;;; Test:

;; The test code assumes a file named sample.xml with the following
;; content:

;;   <book id="compiler">
;;     <bookinfo>
;;       <bookbiblio>
;;         <title>My own book!</title>
;;         <edition>First</edition>
;;         <authorgroup>
;;           <author>
;;             <firstname>John</firstname>
;;             <surname>Wiegley</surname>
;;           </author>
;;         </authorgroup>
;;       </bookbiblio>
;;     </bookinfo>
;;     <chapter>
;;       <title>A very small chapter</title>
;;       <para>Wonder where the content is...</para>
;;     </chapter>
;;   </book>

;;; Code:

(require 'cl)
(require 'dom)
(require 'xpath-parser)

;; Axes

(defun xpath-follow-axis (node axis)
  "Return all the nodes on AXIS relative to node.
AXIS must be a string used in `xpath-axes'."
  (let ((func (cadr (assoc axis xpath-axes))))
    (if func
	(funcall func node)
      (error "Unknown axis: " axis))))

(defun xpath-ancestor-axis (node)
  "Return the elements on the ancestor axis.
The ancestor axis contains the ancestors of the context node.  The
ancestors of the context node consist of the parent of context node and
the parent's parent and so on.  Thus, the ancestor axis will always
include the root node, unless the context node is the root node.

See `dom-node-parent-node'."
  (let ((parent (dom-node-parent-node node))
	result)
    (while parent
      (setq result (cons parent result)
	    parent (dom-node-parent-node parent)))
    (nreverse result)))

(defun xpath-ancestor-or-self-axis (node)
  "Return NODE and the elements of the ancestor axis.
The ancestor-or-self axis contains the context node and the ancestors of
the context node.  Thus, the ancestor axis will always include the root
node.

See `xpath-ancestor-axis'."
  (cons node (xpath-ancestor-axis node)))

(defun xpath-attribute-axis (node)
  "Return the elements of the attribute axis.
The attribute axis contains the attributes of the context node.  The
axis will be empty unless the context node is an element.

See `dom-node-attributes'."
  (dom-node-attributes node))

(defun xpath-child-axis (node)
  "Return the elements of the child axis.
The child axis contains the children of the context node.

See `dom-node-child-nodes'."
  (dom-node-child-nodes node))

(defun xpath-descendant-axis (node)
  "Return the elements of the descendant axis.
The descendant axis contains the descendants of the context node.  A
descendant is a child or a child of a child and so on.  Thus the
descendant axis never contains attribute or namespace nodes."
  ;; We don't want to call this recursively because of performance.
  (setq node (dom-node-first-child node))
  (let (stack result)
    (while node
      (setq result (cons node result)
	    node (cond ((dom-node-first-child node)
			(when (dom-node-next-sibling node)
			  (push (dom-node-next-sibling node) stack))
			(dom-node-first-child node))
		       ((dom-node-next-sibling node))
		       (t (pop stack)))))
    (nreverse result)))

(defun xpath-descendant-or-self-axis (node)
  "Return the elements of the descendant-or-self axis.
The descendant-or-self axis contains the context node and the
descendants of the context node.

See `xpath-descendant-axis'."
  (cons node (xpath-descendant-axis node)))

(defun xpath-following-axis (node)
  "Return the elements of the following axis.
The following axis contains all nodes in the same document as the
context node that are after the context node in document order,
excluding any descendants and excluding attribute nodes and namespace
nodes."
  ;; We don't want to call this recursively because of performance.
  (let ((ancestors (xpath-ancestor-or-self-axis node))
	stack result)
    ;; The stack holds all the ancestors which have a next sibling.
    ;; Note that this is very very inefficient if dom-node-next-sibling
    ;; is very inefficient (as it currently is).
    (dolist (ancestor ancestors)
      (let ((next-sibling (dom-node-next-sibling ancestor)))
	(when next-sibling
	  (setq stack (cons next-sibling stack)))))
    (setq stack (nreverse stack)
	  node (pop stack))
    (while node
      (setq result (cons node result)
	    node (cond ((dom-node-first-child node)
			(when (dom-node-next-sibling node)
			  (push (dom-node-next-sibling node) stack))
			(dom-node-first-child node))
		       ((dom-node-next-sibling node))
		       (t (pop stack)))))
    (nreverse result)))

(defun xpath-following-sibling-axis (node)
  "Return the elements of the following-sibling axis.
The following-sibling axis contains all the following siblings of the
context node.  If the context node is an attribute node or namespace
node, the following-sibling axis is empty."
  (let ((parent (dom-node-parent-node node)))
    (when parent
      (cdr (memq node (dom-node-child-nodes parent))))))

(defun xpath-parent-axis (node)
  "Return the only element of the parent-axis.
The parent axis contains the parent of the context node, if there is
one.

See `dom-node-parent'."
  (list (dom-node-parent-node node)))

(defun xpath-preceding-axis (node)
  "Return the elements of the preceding axis.
The preceding axis contains all nodes in the same document as the
context node that are before the context node in document order,
excluding any ancestors and excluding attribute nodes and namespace
nodes."
  ;; We don't want to call this recursively because of performance.
  (let ((ancestors (xpath-ancestor-or-self-axis node))
	(context-node node)
	stack result)
    ;; We just add the elements in document order, skipping ancestors,
    ;; until we reach the context node.
    (setq node (dom-document-element (dom-node-owner-document context-node)))
    (while node
      (when (not (memq node ancestors))
	(setq result (cons node result)))
      (setq node (cond ((dom-node-first-child node)
			(when (dom-node-next-sibling node)
			  (push (dom-node-next-sibling node) stack))
			(dom-node-first-child node))
		       ((dom-node-next-sibling node))
		       (t (pop stack))))
      (when (eq node context-node)
	(setq node nil)))
    result))

(defun xpath-preceding-sibling-axis (node)
  "Return the elements on the preceding-sibling axis.
The preceding-sibling axis contains all the preceding siblings of the
context node.  If the context node is an attribute node or namespace
node, the preceding-sibling axis is empty."
  (let ((parent (dom-node-parent-node node)))
    (when parent
      (let ((list (dom-node-child-nodes parent))
	    result)
	(while (and list (not (eq (car list) node)))
	  (setq result (cons (car list) result)
		list (cdr list)))
	result))))

;; FIXME: Namespaces not implemented.
;; The namespace axis contains the namespace nodes of the context node.
;; The axis will be empty unless the context node is an element.

(defun xpath-self-axis (node)
  "Return the element on the self axis.
The self axis contains just the context node itself."
  (list node))

;; Node tests

(defun xpath-name-filter (nodes name)
  "Filter NODES by NAME.
If NAME is \"*\", return NODES."
  (if (string= name "*")
      nodes
    (let (result)
      (dolist (node nodes)
	(when (string= name (dom-node-name node))
	  (setq result (cons node result))))
      (nreverse result))))

(defun xpath-text-filter (nodes)
  "Filter NODES, retaining only text nodes."
  (let (result)
    (dolist (node nodes)
      (when (eq (dom-node-type node) dom-text-node)
	(setq result (cons node result))))
    (nreverse result)))

;; FIXME: xpath-comment-filter and xpath-processing-instruction-filter
;; are not implemented, yet.

;;; Node Set Functions

;; For each node in the node-set to be filtered, the PredicateExpr is
;; evaluated with that node as the context node, with the number of
;; nodes in the node-set as the context size, and with the proximity
;; position of the node in the node-set with respect to the axis as the
;; context position; if PredicateExpr evaluates to true for that node,
;; the node is included in the new node-set; otherwise, it is not
;; included.

(defvar xpath-context-node)
(defvar xpath-context-size)
(defvar xpath-context-position)

;; A PredicateExpr is evaluated by evaluating the Expr and converting
;; the result to a boolean. If the result is a number, the result will
;; be converted to true if the number is equal to the context position
;; and will be converted to false otherwise; if the result is not a
;; number, then the result will be converted as if by a call to the
;; boolean function. Thus a location path para[3] is equivalent to
;; para[position()=3].

;; FIXME: Function related stuff is not implemented.

(defun xpath-function/last ()
  "Return a number equal to the context size from the expression
evaluation context."
  xpath-context-size)

(defun xpath-function/position ()
  "Return a number equal to the context position from the expression
evaluation context."
  xpath-context-position)

(defun xpath-function/count (node-set)
  "Return the number of nodes in NODE-SET."
  (length node-set))

(defun xpath-function/name (&optional node-set)
  "Return the name of the first element in NODE-SET.
If optional argument NODE-SET is not given, return the name
of the context-node."
  (if node-set
      (dom-node-name (car node-set))
    (dom-node-name xpath-context-node)))

;; Operations

(defun xpath-number (&optional obj)
  "Return the numeric value for OBJ."
  (unless obj
    (setq obj xpath-context-node))
  (cond ((and (listp obj)
	      (dom-element-p (car obj)))
	 (setq obj (xpath-string obj)))
	((dom-element-p obj); This is not in the spec!
	 (setq obj (xpath-string obj))))
  (cond ((numberp obj)
	 obj)
	((stringp obj)
	 (if (string-match "[^0-9.eE- \t\n\r\l]" obj)
	     'NaN
	   (string-to-number obj)))
	((eq obj nil)
	 0)
	((eq obj t)
	 1)
	(t (error "Cannot convert %S to a string" obj))))

(defun xpath-string (obj)
  "Return the string-value for OBJ.
This is computed as follows:
No computation is necessary for strings.
Numbers are passed to `string-to-number'.
nil is \"false\".  t is \"true\".
A DOM Element is passed to `dom-element-text-content'.
A DOM Node List gets the string value of its first element.
A DOM Attribute is passed to `dom-attr-value'."
  (cond ((stringp obj)
	 obj)
	((numberp obj)
	 (string-to-number obj))
	((eq obj nil)
	 "false")
	((eq obj t)
	 "true")
	((and (listp obj)
	      (dom-element-p (car obj)))
	 (dom-element-text-content (car obj)))
	((dom-element-p obj)
	 (dom-element-text-content (car obj)))
	((dom-attr-p obj)
	 (dom-attr-value obj))
	(t (error "Cannot convert %S to a string" obj))))

;; A little evaluator.
(defun xpath-eval (expression)
  "Evaluate EXPRESSION."
  (if (and (listp expression)
	      (functionp (car expression)))
      (eval expression)
    expression))

(defun xpath-equal (nodes a b)
  "Filter nodes, retaining nodes where A and B are equal.
Equality is determined as follows:
If either A or B are booleans, compare booleans.
If either A or B are numbers, compare numbers.
Else, compare strings.  See `xpath-string'."
  ;; FIXME: Needs more work to be really compliant.
  (let ((xpath-context-position 0)
	(xpath-context-size (length nodes))
	result)
    (dolist (node nodes)
      (setq xpath-context-position (1+ xpath-context-position))
      (let* ((xpath-context-node node)
	     (a (xpath-eval a))
	     (b (xpath-eval b)))
	(when (cond ((listp a)
		     (let (result)
		       (while (and (not result) a)
			 (setq result (xpath-equal nodes (car a) b)
			       a (cdr a)))
		       result))
		    ((listp b)
		     (let (result)
		       (while (and (not result) b)
			 (setq result (xpath-equal nodes a (car b))
			       b (cdr b)))
		       result))
;; 		    ((or (boolean-p a) (boolean-p b))
;; 		     ;; The following trick treats any non-nil value as t.
;; 		     (eq (not a) (not b)))
		    ((or (eq a t) (eq b t))
		     ;; The following trick treats any non-nil value as t.
		     (eq (not a) (not b)))
		    ((or (numberp a) (numberp b))
		     (= (xpath-number a) (xpath-number b)))
		    ((or (stringp a) (stringp b))
		     (string= (xpath-string a) (xpath-string b)))
		    (t
		     (equal a b)))
	  (setq result (cons node result)))))
    (nreverse result)))

;; Resolving an XPath

(defun xpath-resolve-axis (nodes func)
  "Apply FUNC to every node in NODES and return the concatenation."
  ;; Use append instead of nconc, because if this is the child axis, for
  ;; example, then the list returned will be the original list of
  ;; children.
  (apply 'append (mapcar (lambda (node) (funcall func node))
			nodes)))

(defun xpath-resolve (node xpath)
  "Resolve XPATH relative to NODE.
XPATH is a string, NODE is the context node.
This returns a list of nodes."
  (let ((steps (xpath-steps xpath)))
    ;; If XPATH is an absolute location path, then the car of STEPS is
    ;; the (uninterned) symbol in the variable
    ;; `xpath-document-root-symbol'. In this case we must start from
    ;; the root element.
    (when (eq (car steps) xpath-document-root-symbol)
      (setq node (dom-document-element
		  (dom-node-owner-document node))
	    steps (cdr steps)))
    (xpath-resolve-steps node steps)))

(defun xpath-resolve-steps (node steps)
  "Resolve STEPS relative to NODE.
STEPS is a parsed XPATH.
See `xpath-resolve' and `xpath-steps'."
  (let ((nodes (list node)))
    (dolist (step steps)
      ;; For each node, get the nodes on the axis and concatenate the result.
      (let ((func (car step)))
	(setq nodes (xpath-resolve-axis nodes func)))
      ;; Apply each of the predicates.
      (let ((predicates (cdr step))
	    predicate)
	(while (and nodes predicates)
	  (setq predicate (car predicates)
		predicates (cdr predicates))
	  (let ((func (car predicate))
		(args (cdr predicate)))
	    (setq nodes (apply func nodes args))))))
    nodes))

;;; Test stuff

(defmacro xpath-assert (expr)
  `(unless ,expr
     (error "Test failed: %S" ',expr)))

(defun xpath-test-clean-xml (obj)
  (cond ((null obj) nil)
	((atom obj) obj)
	((consp (car obj))
	 (cons (xpath-test-clean-xml (car obj))
	       (xpath-test-clean-xml (cdr obj))))
	(t (cond
	    ((stringp (car obj))
	     (if (string-match "\\`[\n\t ]+\\'" (car obj))
		 (xpath-test-clean-xml (cdr obj))
	       (cons
		(and
		 (string-match "\\`[\n\t ]*\\(.*\\)[\n\t ]*\\'" (car obj))
		 (match-string 1 (car obj)))
		(xpath-test-clean-xml (cdr obj)))))
	    (t (cons (car obj) (xpath-test-clean-xml (cdr obj))))))))

(when (and nil
	   (file-readable-p "sample.xml"))

  (require 'xml)
  (defvar xpath-test-data
    (xpath-test-clean-xml
     (car (xml-parse-file "sample.xml"))))

  (defvar xpath-test-document
    (dom-make-document-from-xml xpath-test-data))

  ;;     (defvar xpath-test-node
  ;;       (car (dom-document-get-elements-by-tag-name
  ;; 	    xpath-test-document
  ;; 	    'title)))

  ;;     (xpath-resolve xpath-test-node "descendant::title")
  ;;     (xpath-resolve xpath-test-node "child::bookbiblio/child::title")
  ;;     (xpath-resolve xpath-test-node "/child::chapter/child::title")

  ;; (setq data (car (xml-parse-file "sample.xml")))
  (let ((title (car (dom-document-get-elements-by-tag-name
		     xpath-test-document
		     'title))))
    (xpath-assert (equal (mapcar 'dom-node-name
				 (xpath-ancestor-axis title))
			 '(bookbiblio bookinfo book)))
    (xpath-assert (equal (mapcar 'dom-node-name
				 (xpath-ancestor-or-self-axis title))
			 '(title bookbiblio bookinfo book)))
    (xpath-assert (equal (mapcar 'dom-node-name
				 (xpath-attribute-axis (dom-document-element
							xpath-test-document)))
			 '(id)))
    (xpath-assert (equal (mapcar 'dom-node-name
				 (xpath-child-axis (dom-document-element
						    xpath-test-document)))
			 '(bookinfo chapter)))
    (xpath-assert (equal (mapcar 'dom-node-name
				 (xpath-descendant-axis
				  (dom-element-last-child
				   (dom-document-element
				    xpath-test-document))))
			 '(title \#text para \#text)))
    (xpath-assert (equal (mapcar 'dom-node-name
				 (xpath-descendant-or-self-axis
				  (dom-element-last-child
				   (dom-document-element
				    xpath-test-document))))
			 '(chapter title \#text para \#text)))
    (xpath-assert (equal (mapcar 'dom-node-name
				 (xpath-descendant-axis
				  (dom-document-element xpath-test-document)))
			 '(bookinfo bookbiblio title \#text edition \#text
				    authorgroup author firstname \#text surname
				    \#text chapter title \#text para \#text)))
    (xpath-assert (equal (mapcar 'dom-node-name
				 (xpath-following-axis
				  (dom-element-first-child
				   (dom-document-element
				    xpath-test-document))))
			 '(chapter title \#text para \#text)))
    (xpath-assert (equal (mapcar 'dom-node-name (xpath-following-axis title))
			 '(edition \#text authorgroup author firstname
				   \#text surname \#text chapter title 
				   \#text para \#text)))
    (xpath-assert (equal (mapcar 'dom-node-name
				 (xpath-following-sibling-axis title))
			 '(edition authorgroup)))
    (xpath-assert (equal (mapcar 'dom-node-name (xpath-parent-axis title))
			 '(bookbiblio)))
    (xpath-assert (equal (mapcar 'dom-node-name
				 (xpath-preceding-axis
				  (dom-node-last-child
				   (dom-document-element
				    xpath-test-document))))
			 '(\#text surname \#text firstname author
			   authorgroup \#text edition \#text title
			   bookbiblio bookinfo)))
    (xpath-assert (equal (mapcar 'dom-node-name
				 (xpath-preceding-sibling-axis
				  (dom-node-last-child
				   (dom-document-element
				    xpath-test-document))))
			 '(bookinfo)))
    (xpath-assert (equal
		   (mapcar 'dom-node-name
			   (xpath-preceding-sibling-axis
			    (dom-node-last-child ; authorgroup
			     (dom-node-first-child ; bookbiblio
			      (dom-node-first-child ; bookinfo
			       (dom-document-element xpath-test-document)))))) ; book
		   '(edition title)))
    (xpath-assert (equal (xpath-self-axis title) (list title))))

  (let ((node-list (dom-document-get-elements-by-tag-name
		    xpath-test-document '*)))
    (xpath-assert (equal (mapcar 'dom-node-name
				 (xpath-name-filter node-list 'title))
			 '(title title)))
    (xpath-assert (equal (mapcar 'dom-node-value
				 (xpath-text-filter node-list))
			 '("My own book!" "First" "John" "Wiegley"
			   "A very small chapter"
			   "Wonder where the content is..."))))

  (let ((root (dom-document-element xpath-test-document)))
    (xpath-assert (equal (mapcar 'dom-node-name
				 (xpath-resolve-axis (list root)
						     'xpath-child-axis))
			 '(bookinfo chapter)))
    (xpath-assert (equal (mapcar 'dom-node-name
				 (xpath-resolve-axis
				  (dom-node-child-nodes root)
				  'xpath-child-axis))
			 '(bookbiblio title para)))
    (xpath-assert (equal (mapcar 'dom-node-text-content
				 (xpath-resolve root "descendant::title"))
			 '("My own book!"
			   "A very small chapter")))
    (xpath-assert (equal
		   (mapcar 'dom-node-text-content
			   (xpath-resolve root
					  "descendant::chapter/child::title"))
		   '("A very small chapter"))))

  (xpath-assert (xpath-equal '(yes) t 5))
  (xpath-assert (not (xpath-equal '(yes) nil 5)))
  (xpath-assert (xpath-equal '(yes) 5.0 5))
  (xpath-assert (not (xpath-equal '(yes) 4 5)))
  (xpath-assert (xpath-equal '(yes) "5.0" 5))
  (xpath-assert (xpath-equal '(yes) '(+ 3 2) 5))
  ;; What is `xpath-resolve-args'??
  ;; Is here some problem lurking?
  ;;     (xpath-assert (equal (xpath-resolve-args '(1 (= 1 1) 3))
  ;; 			 '(1 t 3)))
  ;;     (xpath-assert (equal (xpath-resolve-args '(4 (+ 3 2) 6))
  ;; 			 '(4 5 6)))
  (xpath-assert (xpath-equal '(yes) '(1 2 3) 3))
  (xpath-assert (not (xpath-equal '(yes) '(1 2 3) 4)))
  (xpath-assert (xpath-equal '(yes) 3 '(1 2 3)))
  (xpath-assert (not (xpath-equal '(yes) 4 '(1 2 3))))
  (xpath-assert (xpath-equal '(yes) '(1 2 3) '(3 4 5)))
  (xpath-assert (not (xpath-equal '(yes) '(1 2 3) '(4 5 6))))

  (let ((root (dom-document-element xpath-test-document)))
    (xpath-assert (equal
		   (mapcar 'dom-node-name
			   (xpath-resolve root "child::*[position()=1]"))
		   '(bookinfo)))
    (xpath-assert (equal
		   (mapcar 'dom-node-name
			   (xpath-resolve root "child::*[position()=2]"))
		   '(chapter)))
    (xpath-assert (null
		   (xpath-resolve root
				  "child::*[attribute::id=\"compiler\"]"))))

  (let ((root (dom-document-element xpath-test-document)))
    (xpath-assert
     (equal
      (mapcar
       'dom-node-name
       (xpath-resolve root "self::*[attribute::id=\"compiler\"]"))
      '(book))))

  ;; Absolute Paths.
  (let ((node (car (dom-document-get-elements-by-tag-name
		    xpath-test-document 'edition))))
    (xpath-assert (equal
		   (mapcar 'dom-node-name
			   (xpath-resolve node "/descendant::title"))
		   '(title title))))

  ;; Abbreviated syntax.
  (let ((root (dom-document-element xpath-test-document)))
    (xpath-assert
     (equal
      (mapcar
       'dom-node-text-content
       (xpath-resolve root "descendant::authorgroup/author/firstname"))
      '("John"))))

  ;;     (let ((root (dom-document-element xpath-test-document)))
  ;;       (xpath-resolve root "descendant::authorgroup/author/firstname[position()]"))


  (let ((node (car (dom-document-get-elements-by-tag-name
		    xpath-test-document 'edition))))
    (xpath-assert (equal (mapcar 'dom-node-text-content
				 (xpath-resolve node "/chapter/title"))
			 '("A very small chapter"))))

  )

(provide 'xpath)

;;; xpath.el ends here.

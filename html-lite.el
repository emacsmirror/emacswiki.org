;;; html-lite.el --- Construct html with sexp

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Mario Domgörgen <kanaldrache@gmx.de>
;; Keywords: hypermedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides procedures to construct an HTML document easily.  For
;; example, you can construct an HTML table by the following code:

;; (html-table :border "1" 
;;  (html-tr (html-th "Item No") (html-th "Quantity"))
;;  (html-tr (html-td "1") (html-td "120")) 
;;  (html-tr (html-td "2") (html-td "30")) 
;;  (html-tr (html-td "3") (html-td "215")))

;; There are mainly two functions to access the constructed
;; tree. `html-lite-write-tree' writes the tree indented to the
;; current ouput stream and `html-lite-browse-tree' use
;; `browse-url-browser-function' to browse the tree.
;;
;; You can construct complete html tree by using:
;;
;; (append
;;  (html-doctype)
;;  (html-html
;;   (html-head
;;    (html-title "Title"))
;;   (html-body "Content")))
;;
;; But the simpler way would be jsut to use `with-html-lite-header' like 
;;
;; (with-html-lite-header "Title" 
;;   (html-p "foo") (html-p "bar"))
;;
;; The code is ported from html-lite.scm in the Gauche
;; distribution. Some things are missings but will be added asap.
;;
;; I wrote this just for the fun of it and hope someone will find it
;; useful.
;;
;;; Code:

(defgroup html-lite nil
  "*Library for construction of html documents with sexps."
  :prefix "html-lite-"
  :group 'hypermedia)


(defcustom html-lite-doctype 'strict
  "If non-nil specify the standard doctype for your html document"
  :group 'html-lite
  :type '(choice (const :tag "Strict" strict)
		(const :tag "Transitional" transitional)
		(const :tag "Frameset" frameset)
		(const :tag "XHTML 1.0 Strict" xhtml-1.0-strict)
		(const :tag "XHTML 1.0 Transitional" xhtml-1.0-transitional)
		(const :tag "XHTML 1.0 Frameset" xhtml-1.0-frameset)
		(const :tag "XHTML 1.1" xhtml-1.1)))

(defconst html-lite-version "0.3")

(defconst html-lite-doctype-alist
  '((strict
     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
       \"http://www.w3.org/TR/html4/strict.dtd\">\n")
    (transitional
     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
       \"http://www.w3.org/TR/html4/loose.dtd\">\n")
    (frameset
     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
       \"http://www.w3.org/TR/html4/frameset.dtd\">\n")
    (xhtml-1.0-strict
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n")
    (xhtml-1.0-transitional
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n")
    (xhtml-1.0-frameset
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">\n")
    (xhtml-1.1
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
       \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n")
    ))

(defmacro html-lite-define-elements (&rest elements)
  "Define ELEMENTS as funcs that eval to their html-tree element.
ELEMENST must be a string. When surroundes by parens make the
element a emtpy one."
  (declare (indent defun))
  `(progn
     ,@(mapcar 
	(lambda (element)
	  (if (listp element)
	      (list 'defun (html-lite-make-name (car element)) 
		    '(&rest args)
		    (concat "Produce tree element " (car element))
		    (list 'html-lite-make-element (car element) t 'args))
	    (list 'defun (html-lite-make-name element) 
		  '(&rest args)
		  (concat "Produce tree element " element)
		  (list 'html-lite-make-element element nil 'args))))
	elements)))


(defun html-lite-make-element (name empty args)
  (let ((propstring "")
	(tags))
    (while args
      (let ((arg (car args)))	
	(setq args (cdr args))
	(cond ((keywordp arg)
	       (let ((keyword arg)
		     (value (car args)))
		 (unless args
		   (error "Keyword %s is missing an argument" keyword))
		 (setq args (cdr args))
		 (setq propstring (concat propstring 
					  " "
					  (substring (symbol-name arg) 1)
					  "="
					  "\"" value "\""))))
	      ((stringp arg)
	       (setq tags (append tags 
				  (list (html-lite-escape-string arg)))))
	      ((listp arg)
	       (setq tags (append tags arg))))))
    
    (cond (empty
	   (list (concat "<" name propstring "/>")))
	  ((null tags)
	   (error "Non empty element must have content: %s" name))
	  (t
	   `(,(concat "<" name propstring ">")
	     ,tags
	     ,(concat "</" name ">"))))))


(defmacro with-html-lite-header (title &rest body)
  "Construct html with doctype and head and eval BODY as content.
TITLE must be a string and is used as content in the title-tag."
  `(append
    (html-doctype)
    (html-html 
    (html-head
     (html-title ,title))
    (html-body
     ,@body))))
     
(defun html-lite-escape-string (string)
  "Escape dangerous character in html strings."
  (mapc (lambda (elt) 
	  (setq string 
		(replace-regexp-in-string (car elt) (cdr elt) string)))
	'(("&" .  "&amp;")
	  ("<" . "&lt;")
	  (">" . "&gt;")	  
	  ("\"" . "&quot;")))
  string)

(defun html-doctype (&optional type)
  "Return the doctype for TYPE.
TYPE can be one of the following symbols: strict, transitional,
frameset, xhtml-1.0-strict, xhtml-1.0-transitional,
xhtml-1.0-frameset and xhtml-1.1."
  (let ((type (or type html-lite-doctype)))
    (cdr (assoc type html-lite-doctype-alist))))


(defun html-lite-make-name (name)
  "Return the function-name for NAME."
  (intern (format "html-%s" name)))


(defun html-lite-write-tree (tree &optional indent)
  "Write tree to buffer.
Indent tag to column INDENT."
  (let ((indent (or indent 0)))
    (mapc
     (lambda (element)
       (cond ((listp element)
	      (html-lite-write-tree element (+ indent 2)))
	     (t
	      (indent-to-column indent) 
	      (insert element "\n"))))
     tree)))
		
(defun html-lite-browse-tree (tree)
  "Browse TREE.
Variable`browse-url-browser-function' says which browser to use."
  (let ((file (make-temp-file "html-lite" nil ".html")))
    (with-temp-file file (html-lite-write-tree tree))
    (browse-url-of-file file)))
  
;; http://www.w3.org/TR/html4/sgml/dtd.html

;; TEXT MARKUP

;; %fontstyle
(html-lite-define-elements 
  "tt" "i" "b" "big" "small")

;; %phrase
(html-lite-define-elements 
  "em" "strong" "dfn" "code" "samp" 
  "kbd" "var" "cite" "abbr" "acronym")

(html-lite-define-elements "sub" "sup" "span" "bdo" ("br"))

;; HTML CONTENT MODELS

;; DOCUMENT BODY

(html-lite-define-elements "body" "addres" "div")

;; THE ANCHOR ELEMENT
(html-lite-define-elements "a")

;; cLIENT-SIDE IMAGE MAPS
(html-lite-define-elements "map" ("area"))

;; THE LINK EKEMENT
(html-lite-define-elements ("link"))

;; IMAGES
(html-lite-define-elements ("img"))

;; OBJECT
(html-lite-define-elements ("object") ("param"))

;; ;; HORIZONTAL RULE
(html-lite-define-elements ("hr"))

;; PARAGRAPHS
(html-lite-define-elements "p")

;; HEADINGS
(html-lite-define-elements "h1" "h2" "h3" "h4" "h5" "h6")

;; PREFORMATTED
(html-lite-define-elements "pre")

;; INLINE QUOTES
(html-lite-define-elements "q")

;; BLOCK-LIKE QUOTES
(html-lite-define-elements "blockquote")

;; INSERTED/DELETED TEXT
(html-lite-define-elements "ins" "del")

;; LISTS
(html-lite-define-elements "dl" "dt" "dd" "ol" "ul" "li")

;; FORMS
(html-lite-define-elements "form" "label" ("input") "select" 
			   "optgroup" "option" "textarea" 
			   "fieldset" "legend" "button")

;; TABLES
(html-lite-define-elements "table" "caption" "thead" "tfoot" 
			   "tbody" "colgroup" ("col") 
			   "tr" "th" "td")

;; DOCUMENT HEAD
(html-lite-define-elements "head" "title" ("base") ("meta")
			   "style" "script" "noscript")

;; DOCUMENT STRUCTURE
(html-lite-define-elements "html")


(provide 'html-lite)
;;; html-lite.el ends here

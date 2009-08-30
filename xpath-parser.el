;;; xpath-parser.el --- XPATH parser

;; Copyright (C) 2001  Alex Schroeder <alex@gnu.org>

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Oliver Scholz <epameinondas@gmx.de>
;; Version: 1.0.0
;; Keywords: xml
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?XmlParser
;; Version: $Id: xpath-parser.el,v 1.6 2003/12/16 00:27:36 egoge Exp egoge $

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

;; Used by xpath.el, tables created automatically from xpath.bnf.  The
;; main entry points are `xpath-lex-string' and `xpath-lex-region'.
;; These two functions prepare a list of preliminary tokens and store
;; them in the variable `xpath-token-input'.  Next, call `wisent-parse'
;; using `xpath-tables' and `xpath-pop-input' and an error function of
;; your liking: (wisent-parse xpath-tables #'xpath-pop-input #'error)
;;
;; `wisent-parse' then returns a list of elements STEP.  Each STEP has
;; the form (TEST PREDICATE).  Both TEST and PREDICATE have the form
;; (FUNC PARAMS...).  FUNC is always a function which must accept all
;; the PARAMS as arguments, plus a node.  The TEST FUNC must then return
;; a list of nodes, the PREDICATE must return either nil or non-nil.
;; The PREDICATE is used for filtering the list returned by TEST FUNC.
;;
;; See xpath.el for more information on all the functions used.

;;; Code:

(require 'wisent)
(eval-when-compile (require 'wisent-comp))

;; (setq wisent-verbose-flag t)
(defconst xpath-document-root-symbol
  (make-symbol "document-root")
  "Symbol used to indicate the document root.
This is used to specify that a query should start from the owner
document. This is necessary for absolute location paths.")

(defvar xpath-tables
  (wisent-compile-grammar
     '((NCNAME LITERAL NUMBER VARIABLEREFERENCE 
	       AND OR DIV MOD
	       COLON AXISSUF DOTDOT LT GT LE GE NE STAR PLUS MINUS SLASH
	       UNION LPAREN RPAREN LBRACK RBRACK AT DOT EQ COMMA
	       NODETYPE FUNCTIONNAME
	       ANCESTOR ANCESTOR-OR-SELF ATTRIBUTE CHILD DESCENDANT
	       DESCENDANT-OR-SELF FOLLOWING FOLLOWING-SIBLING NAMESPACE
	       PARENT PRECEDING PRECEDING-SIBLING SELF)
       nil
       (TopExpr
	((LocationPath)))
       (LocationPath
	((RelativeLocationPath))
	((AbsoluteLocationPath)))
       (AbsoluteLocationPath
	;;      ((SLASH))
	((SLASH RelativeLocationPath)
	 (append (list xpath-document-root-symbol) $2))
	((AbbreviatedAbsoluteLocationPath)))
       (RelativeLocationPath
	((Step) $1)
	((RelativeLocationPath SLASH Step)
	 (append $1 $3 nil))
	((AbbreviatedRelativeLocationPath)))
       (Step
	((Basis predicates)
	 (list
	  (append $1 $2)))
	((AbbreviatedStep)))
       (predicates
	(nil)
	((predicates Predicate)
	 (append $1 $2)))
       (Basis
	((AxisName AXISSUF NodeTest)
	 (list $1 $3))
	((AbbreviatedBasis)
	 (list 'xpath-child-axis $1)))
       (AxisName
	((ANCESTOR)
	 'xpath-ancestor-axis)
	((ANCESTOR-OR-SELF)
	 'xpath-ancestor-or-self-axis)
	((ATTRIBUTE)
	 'xpath-attribute-axis)
	((CHILD)
	 'xpath-child-axis)
	((DESCENDANT)
	 'xpath-descendant-axis)
	((DESCENDANT-OR-SELF)
	 'xpath-descendant-or-self-axis)
	((FOLLOWING)
	 'xpath-following-axis)
	((FOLLOWING-SIBLING)
	 'xpath-following-sibling-axis)
	((NAMESPACE)
	 'xpath-namespace-axis)
	((PARENT)
	 'xpath-parent-axis)
	((PRECEDING)
	 'xpath-preceding-axis)
	((PRECEDING-SIBLING)
	 'xpath-sibling-axis)
	((SELF)
	 'xpath-self-axis))
       (NodeTest
	((NameTest)
	 (list 'xpath-name-filter $1))
	((NODETYPE LPAREN Arglist RPAREN)
	 (list 'xpath-node-type-filter $1))
	;;       ((PROCESSING-INSTRUCTION LPAREN LITERAL RPAREN))
	)
       (Predicate
	((LBRACK PredicateExpr RBRACK)
	 (list $2)))
       (PredicateExpr
	((Expr)))
       (AbbreviatedAbsoluteLocationPath
	((SLASH SLASH RelativeLocationPath)))
       (AbbreviatedRelativeLocationPath
	((RelativeLocationPath SLASH SLASH Step)))
       (AbbreviatedStep
	((DOT))
	((DOTDOT)))
       (AbbreviatedBasis
	((NodeTest))
	((AT NodeTest)))
       (Expr
	((OrExpr)))
       (PrimaryExpr
	((VARIABLEREFERENCE))
	((LPAREN Expr RPAREN))
	((LITERAL))
	((NUMBER))
	((FunctionCall)))
       (FunctionCall
	((FUNCTIONNAME LPAREN Arglist RPAREN)
	 (append
	  (list (intern (concat "xpath-function/" $1)))
	  $3)))
       ;;      (FunctionName
       ;;       ((POSITION)
       ;;        'xpath-position-function)
       ;;       ((LAST)
       ;;        'xpath-last-function)
       ;;       ((COUNT)
       ;;        'xpath-count-function)
       ;;       ((NAME)
       ;;        'xpath-name-function))
       (Arglist
	(nil)
	((Arguments)))
       (Arguments
	((Argument)
	 (list $1))
	((Arguments COMMA Argument)
	 (append $1
		 (list $3))))
       (Argument
	((Expr)))
       (UnionExpr
	((PathExpr))
	((UnionExpr UNION PathExpr)))
       (PathExpr
	((LocationPath)
	 (list 'xpath-resolve-steps 'xpath-context-node
	       (list 'quote $1)))
	((FilterExpr))
	((FilterExpr SLASH RelativeLocationPath))
	((FilterExpr SLASH SLASH RelativeLocationPath)))
       (FilterExpr
	((PrimaryExpr))
	((FilterExpr Predicate)))
       (OrExpr
	((AndExpr))
	((OrExpr OR AndExpr)))
       (AndExpr
	((EqualityExpr))
	((AndExpr AND EqualityExpr)))
       (EqualityExpr
	((RelationalExpr))
	((EqualityExpr EQ RelationalExpr)
	 (list 'xpath-equal $1 $3))
	((EqualityExpr NE RelationalExpr)))
       (RelationalExpr
	((AdditiveExpr))
	((RelationalExpr LT AdditiveExpr))
	((RelationalExpr GT AdditiveExpr))
	((RelationalExpr LE AdditiveExpr))
	((RelationalExpr GE AdditiveExpr)))
       (AdditiveExpr
	((MultiplicativeExpr))
	((AdditiveExpr PLUS MultiplicativeExpr))
	((AdditiveExpr MINUS MultiplicativeExpr)))
       (MultiplicativeExpr
	((UnaryExpr))
	((MultiplicativeExpr STAR UnaryExpr))
	((MultiplicativeExpr DIV UnaryExpr))
	((MultiplicativeExpr MOD UnaryExpr)))
       (UnaryExpr
	((UnionExpr))
	((MINUS UnaryExpr)))
       (NameTest
	((STAR))
	((NCNAME COLON STAR))
	((NCNAME COLON NCNAME))
	((NCNAME)))
       ;;      (NodeType
       ;;       ((COMMENT))
       ;;       ((TEXT))
       ;;       ((PROCESSING-INSTRUCTION))
       ;;       ((NODE)))
       )
     nil
     ;;    '(LocationPath)
     )
  "Parser automaton for XPath.")

(eval-and-compile
  (defconst xpath-operator-names
    '(("and" . AND)
      ("or" . OR)
      ("div" . DIV)
      ("mod" . MOD)))

  (defconst xpath-other-operators
    '(("/" . SLASH)
      ;;    ("//" . DSLASH)
      ("|" . UNION)
      ("-" . MINUS)
      ("+" . PLUS)
      ("=" . EQ)
      ("!=" . NE)
      (">=" . GE)
      ("<=" . LE)
      (">" . GT)
      ("<" . LT)))

  (defvar xpath-other-tokens
    '((":" . COLON)			; CAVEAT: to resolve QNames
      ("*" . STAR)			; CAVEAT
      ("]" . RBRACK)
      (")" . RPAREN)
      ("[" . LBRACK)
      ("(" . LPAREN)
      ("," . COMMA)
      ("." . DOT)
      ("@" . AT)
      ;;    ("|" . UNION)
      ;;    ("/" . SLASH)
      ;;     ("-" . MINUS)
      ;;     ("+" . PLUS)
      ;;     ("!=" . NE)
      ;;     (">=" . GE)
      ;;     ("<=" . LE)
      ;;     (">" . GT)
      ;;     ("<" . LT)
      (".." . DOTDOT)
      ("::" . AXISSUF)))

  (defconst xpath-node-types
    '(("comment" . COMMENT)
      ("text" . TEXT)
      ("processing-instruction" . PROCESSING-INSTRUCTION)
      ("node" . NODE)))

  (defconst xpath-axes
    '(("ancestor" . ANCESTOR)
      ("ancestor-or-self" . ANCESTOR-OR-SELF)
      ("attribute" . ATTRIBUTE)
      ("child" . CHILD)
      ("descendant" . DESCENDANT)
      ("descendant-or-self" . DESCENDANT-OR-SELF)
      ("following" . FOLLOWING)
      ("following-sibling" . FOLLOWING-SIBLING)
      ("namespace" . NAMESPACE)
      ("parent" . PARENT)
      ("preceding" . PRECEDING)
      ("preceding-sibling" . PRECEDING-SIBLING)
      ("self" . SELF)))
  )					; End: `eval-and-compile'

(defconst xpath-lexer-obarray
  (let ((xpath-obarray (make-vector 13 0)))
  ;; We need this only for non-letter tokens, because we return a
  ;; letter-keyword (like an axis name) by interning the match-string.
    (dolist (elt (append xpath-other-tokens xpath-other-operators))
      (set (intern (car elt) xpath-obarray) (cdr elt)))
    xpath-obarray)
  "Obarray to lookup some token classes.")

(defsubst xpath-lookup-token (str)
  "Return the tokenclass of token string STR."
  (symbol-value (intern str xpath-lexer-obarray)))

(eval-when-compile
  (defconst xpath-ncname-rx
    `(and (or letter ?_) (zero-or-more (or letter digit ?. ?- ?_)))
    "Symbolic regexp matching NCnames."))

(defconst xpath-ncname-regexp
  (eval-when-compile
    (rx-to-string xpath-ncname-rx))
  "Regexp matching NCNames.")

(defconst xpath-number-regexp
  (rx (or (and (one-or-more digit) 
	       (optional (and ?. (zero-or-more digit))))
	  (and ?. (one-or-more digit))))
  "Regexp matching numbers.")

(defconst xpath-variable-reference-regexp
  (eval-when-compile
    (rx-to-string `(and ?$
			(optional (and ,xpath-ncname-rx
				       ?:))
			,xpath-ncname-rx)))
  "Regexp matching VariableReferences.")

(defsubst xpath-lex-advance-and-return (token &optional return step)
  "Move forward and return the token as appropriate for parser.
This function assumes that the match-data are set appropriately.

See `xpath-next-token' for a description of the format of the
return value.

RETURN is the number of the submatch which determines parts of
the value returned. It defaults to 0. STEP is the submatch to
whose end point will move, it defaults to the value of RETURN."
  (or return (setq return 0))
  (goto-char (match-end (or step return)))
  (let ((str (match-string-no-properties return)))
    (nconc
     (list token
	   str)
     (cons (match-beginning return)
	   (match-end return)))))

(defsubst xpath-operator-allowed-p (preceding)
  "Return non-nil, if an OPERATOR token is allowed at this point.
See XPath specification section 3.7, bullet point #1.
PRECEDING is a symbol, the last token previously returned by the
lexer."
  (and preceding
       (not (memq preceding
		  (eval-when-compile
		    (append '(AT DOTDOT LPAREN LBRACK COMMA) 
			    (mapcar 'cdr
				    (append xpath-operator-names
					    xpath-other-operators
					    ))))))))

(defsubst xpath-lex-resolve-ncname (str beg end preceding)
  "Return the appropriate token value for NCName string STR.

There are special lexical conventions for OperatorName,
FunctionName, NodeType and AxisName. This function resolves these
conventions. See XPath specification section 3.7, bullet points
2-4.

BEG and END are the begin and end of STR in the buffer
respectively. PRECEDING is the last token class previously
returned by the lexer.

See `xpath-next-token' for a description of the format of the
return value."
  (let (token)
    (cond
     ;; OperatorName
     ((and (member str (eval-when-compile
			 (mapcar 'car xpath-operator-names)))
	   (xpath-operator-allowed-p preceding))
      (setq token (intern (upcase str))))
     ;; FunctionName or NodeType
     ((eq (char-after) ?\()
      (setq token
	    (if (member str (eval-when-compile
			      (mapcar 'car xpath-node-types)))
		'NODETYPE
	      'FUNCTIONNAME)))
     ;; AxisName
     ((looking-at "::")
      (setq token (intern (upcase str))))
     ;; Other
     (t (setq token 'NCNAME)))
    (nconc (list token str) (cons beg end))))

(defun xpath-next-token (preceding)
  "Return the next XPath token from the buffer.

PRECEDING should be the last token-class returned by this
function on a previous call or nil.

The return value is a list of the form 
\(TOKEN-CLASS VALUE START . END)
as a Wisent parser automaton expects it from its lexing
function."
  (skip-chars-forward "\x20\x9\xd\xa") ; ExprWhitespace
  (cond
   ;; End of input.
   ((eobp)
    (list wisent-eoi-term))

   ;; '*', other tokens like '(', other operators like '/'.
   ;; See XPath spec section 3.7: `ExprToken' and `Operator'.
   ;;
   ;; The question whether a '*' is a `MultiplyOperator' or part of a
   ;; `NodeTest' is resolved by the Wisent parser automaton.
   ((looking-at (eval-when-compile
		  (regexp-opt (mapcar 'car 
				      (append xpath-other-operators
					      xpath-other-tokens)))))
    (xpath-lex-advance-and-return (xpath-lookup-token (match-string 0))))

   ;; NCName: AxisName, NodeType, FunctionName or NameTest.
   ;;
   ;; We deal with `NodeTest's in the parser automaton. Therefore we
   ;; have a token class for NCNames: NCNAME. We return this class if
   ;; the NCName at hand is neither AxisName nor NodeType nor
   ;; FunctionName.
   ((looking-at xpath-ncname-regexp)
    (goto-char (match-end 0))
    (skip-chars-forward "\x20\x9\xd\xa") ; ExprWhitespace
    (xpath-lex-resolve-ncname (match-string 0)
			      (match-beginning 0)
			      (match-end 0)
			      preceding))

   ;; Literal
   ((looking-at (rx (or (and ?\" (submatch (zero-or-more (not (any "\"")))) ?\")
			(and ?\' (submatch (zero-or-more (not (any "\'")))) ?\'))))
    (xpath-lex-advance-and-return 'LITERAL 1 0))

   ;; Number
   ((looking-at xpath-number-regexp)
    (xpath-lex-advance-and-return 'NUMBER))

   ;; VariableReference
   ((looking-at xpath-variable-reference-regexp)
    (xpath-lex-advance-and-return 'VARIABLEREFERENCE))

   (t (error "Could not reckognize token: %s"
	     (buffer-substring (point) (point-max))))))
				

(defun xpath-steps (str)
  "Return the XPATH steps for string STR."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let (last-token)
      (wisent-parse xpath-tables
		    (lambda ()
		      (let ((token (xpath-next-token last-token)))
			(setq last-token (car token))
			token))))))

;;; Test stuff

;; (defvar egoge-test nil)
;; (defun egoge-test ()
;;   (interactive)
;;   (let ((token (xpath-next-token egoge-test)))
;;     (setq egoge-test (car token))
;;     (print token)))

;; (xpath-test-lex-string "node1/node2")
;; (xpath-steps "child::node1/child::node2")
;; (xpath-test-lex-string "/node1")
;; (xpath-steps "/node1")
;; (xpath-steps "node1/node2")
;; (xpath-steps "child::para[position()=2]")

(defmacro xpath-assert (expr)
  `(unless ,expr
     (error "Test failed: %S" ',expr)))

(defun xpath-test-lex-string (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let (last-token list)
      (while (not (eobp))
	(let ((token (xpath-next-token last-token)))
	  (setq last-token (car token))
	  (push token list)))
      (nreverse list))))


(when nil

  (xpath-assert (equal (xpath-steps "child::para")
		       '((xpath-child-axis (xpath-name-filter "para")))))
  (xpath-assert (equal (xpath-steps "child::para/parent::*")
		       '((xpath-child-axis (xpath-name-filter "para"))
			 (xpath-parent-axis (xpath-name-filter "*")))))
  (xpath-assert (equal (xpath-steps "child::para/parent::text()")
		       '((xpath-child-axis (xpath-name-filter "para"))
			 (xpath-parent-axis (xpath-node-type-filter "text")))))
  (xpath-assert (equal (xpath-steps "child::*")
		       '((xpath-child-axis (xpath-name-filter "*")))))
  (xpath-assert (equal (xpath-steps "child::foo/child::bar/child::test")
		       '((xpath-child-axis (xpath-name-filter "foo"))
			 (xpath-child-axis (xpath-name-filter "bar"))
			 (xpath-child-axis (xpath-name-filter "test")))))
  (xpath-assert (equal (xpath-test-lex-string "child::*[position() = 1]")
		       '((CHILD "child" 1 . 6)
			 (AXISSUF "::" 6 . 8)
			 (STAR "*" 8 . 9)
			 (LBRACK "[" 9 . 10)
			 (FUNCTIONNAME "position" 10 . 18)
			 (LPAREN "(" 18 . 19)
			 (RPAREN ")" 19 . 20)
			 (EQ "=" 21 . 22)
			 (NUMBER "1" 23 . 24)
			 (RBRACK "]" 24 . 25))))
  (xpath-assert (equal (xpath-steps "child::*[position() = 1]")
		       '((xpath-child-axis (xpath-name-filter "*") 
					   (xpath-equal (xpath-function/position) "1")))))
  (xpath-assert (equal (xpath-steps "child::*[position(1,2,3,4) = 1]")
		       '((xpath-child-axis (xpath-name-filter "*")
					   (xpath-equal (xpath-function/position
							 "1" "2" "3" "4")
							"1")))))
  (xpath-assert (equal (xpath-steps "child::*[attribute::type=\"id\"]")
		       '((xpath-child-axis (xpath-name-filter "*")
					   (xpath-equal (xpath-resolve-steps
							 xpath-context-node
							 (quote ((xpath-attribute-axis
								  (xpath-name-filter "type")))))
							"id")))))
  )

(provide 'xpath-parser)

;;; xpath-parser.el ends here

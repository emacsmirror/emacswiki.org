;;; rewrite.el
;; This file is free software
;-*-Lisp-*-
; Copyright (c) 1997-2004 Mutsumi Komuro
;
; rewrite.el  --- Utility to rewrite text files with easier
;                 regular expression handling.
; Version 0.93
; Author: Mutsumi Komuro <komuro@cs.stanford.edu>
;
; Revision History
;
; 0.93 Fixed a bug concerning 'rewrite-pre-search' command.
; 0.92 Fixed a bug concerning 'bind' command.
; 0.91 Added 'insert' command.
;
;;This program is free software; you can redistribute it and/or
;;modify it under the terms of the GNU General Public License
;;as published by the Free Software Foundation; either version 2
;;of the License, or (at your option) any later version.
;;This program is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU General Public License for more details.
;;You should have received a copy of the GNU General Public License
;;along with this program; if not, write to the Free Software
;;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,USA.
;
;Bug descriptions, use reports, comments or suggestions are welcome.
;Send them to komuro@cs.stanford.edu.
;
;;Commentary:
;; Emacs supports regular expression and matching function.
;; One interesting feature is that you can refer to the m-th matched
;; subexpression specified by \(..\) as \m.
;; Although this feature is useful, it does not seems to me very
;; convenient because you have to count the subexpressions.
;; It is error prone especially when the regular expression
;; becomes complicated.
;;
;; What I wanted to realize in this package is to extend the above
;; feature so that you can NAME each subexpression.
;;
;; This package provides you a simple text rewriting system
;; in which you can write a rewriting rule
;; using matched subtext with names.
;;
;; The basic idea is the following.
;; (i)  We use a sequence of strings instead of a single string of
;;      regular expression.
;; (ii) Toghether with a suitable delimiter these strings 
;;      are concatenated.
;; (iii)We can name a string by parenthesizing it. The name is a symbol
;;      at the 'car' part of the parenthesized string.
;; (iv) The name given in (iii) can later be used to refer to the
;;      matched string.
;;
;; For example the following list represents a line of text
;; beginning with "Your response:" and the symbol 'response' matches
;; any subsequent text before a newline (and after a suitable delimiter
;; whose default value is 'white' i.e., space or tab):
;;
;;    ("Your response:" (response ".*"))
;;
;; Then you can use the symbol 'response' to refer to the matched string
;; for later text rewriting.
;; In other words the above list matches the same text as the string
;; "Your response:[ \t]*\(.*\)" does. And you can refere to the part of
;; the text matching ".*" by the name 'response' instead of '\1'.
;;
;;Rewriting System:
;; After loading this package, you can use the rewriting system
;; via 'rewrite-prog' macro. Inside this macro you can use the commands
;; as follows.
;;; (rewrite-prog <command>*)
;;; where
;;;  <command> ::= (search <regular expr seq> <search options>?)|
;;;                (replace-match <newtext seq> <replace options>?)|
;;;                (rewrite <regular expr seq> <result seq>)|
;;;                (call <s-expression>*)|
;;;                (bind (<variable name> <s-expression>)*)|
;;;                (if <condition> <s-expression> <s-expression>?)|
;;;                (while <condition> <s-expression>*)|
;;;                (progn <s-expression>*)|
;;;                (update <variable name> <s-expression>)
;;;                (insert . <regular expr seq>)
;;;
;;Definition (Environment):
;; The rewriting system maintains pairs of a symbol and the string
;; matched with it. We call these pairs the environment of the system.
;;
;;Commands:
;; Currently the rewriting system accepts the following commands:
;;   search  -  syntax: (search <regular expr seq>
;;                              <var-alist> <limit> <noerror> <repeat>),
;;              where the last four arguments are optional.
;;      This command searches the given regular expression sequence
;;      <regular expr seq>. If a matching is found, it updates the
;;      environment.
;;      The behavior when the matching is not found can be specified
;;      by one of optional arguments
;;      which are the same as those of 're-search-forward.'
;;      Its return value is also similar to 're-search-forward'.
;;   replace-match - syntax:
;;                    (replace-match <newtext seq> <fixedcase> <literal>)
;;      This command replaces the matched text in the same manner as
;;      the lisp function of the same name 'replace-match'.
;;      The difference is that the first argument <newtext seq> 
;;      is not a singlestring but a list of strings or 
;;      symbols refering to previously matched subexpressions.
;;      This command alwasy returns 'nil'.
;;   rewrite - syntax: (rewrite <regular expr seq> <newtext seq>).
;;      This command rewrites the text in the buffer matching
;;      <regular expr seq> into <newtext seq>, which is a sequence of
;;      strings or symbols representing matched subtext.
;;      It always returns 'nil'.
;;      This command is equivalent to
;;           (progn (call (beginning-of-buffer))
;;                  (while (search <regular expr seq> nil t nil)
;;                         (replace-match <newtext seq> t t))).
;;   call    - syntax: (call <s-expression>*).
;;      This command first substitutes the matched strings
;;      appearing in <s-expression>'s into the named symbols
;;      and then evaluate them.
;;      It returns the value of the last <s-expression>.
;;   bind    - syntax: (bind ((<variable name> <s-expression>)*)).
;;      This command adds a new biniding to the environemt.
;;      You can write <s-expression> to create a string
;;      or an integer to be bound to <variable name>.
;;      This command always returns 't'.
;;   if      - syntax: (if <conditional command> 
;;                          <then command> <else command>),
;;              where <else command> is optional.
;;      This command first interpretes <conditional command> 
;;      under thecurrent environemt. If the return value of <command> 
;;      is not nil, <then command> will be executed. 
;;      Otherwise <else command> will be.
;;   while   - syntax: (while <conditional command> <command>*).
;;      This command first interpretes <conditional command> 
;;      under the current environment. If the return value of 
;;      <conditional command> is not nil,
;;      then evaluate the series of <command>'s.
;;      It will repeat this procedure while <conditional command> 
;;      is not nil.
;;   progn   - syntax: (progn <command>*).
;;      This command simply interpretes each <command> in turn.
;;      It is used to turn a series of <command>'s into a single
;;      command.
;;   update  - syntax: (update <variable name> <s-expression>).
;;      This command updates the environment 
;;      so that <variable name> will be associated with 
;;      the value of <s-expression> in the new environment.
;;      If <variable name> is not found in the current environment,
;;      an error is signaled. This command returns a pair
;;      (<variable name> <value>) where <value> is a string 
;;      or an integer obtained by evaluating <s-expression>.
;;   insert  - syntax: (insert . <regular expr seq>).
;;      This command insert the string represented by <regular expr seq>
;;      in the buffer. It returns a pair of nil and the current
;;      environemnt.
;;      Note that the use of dot pair in the syntax,
;;      which means that we need no parentheses in the arguments.
;;
;; Adding Your Own Commands:
;;  You can add your own command by 'rewrite-method-def' whose syntax is
;;  given as follows:
;;    (rewrite-method-def <command name> <lambda expression>),
;;  where <lambda expression> is a lambda expression with two arguments.
;;  The first one represents the argument list for <command name>.
;;  The second one represents the environment of the rewriting system.

(defconst rewrite-method 'rewrite-method
"Constant to define the name of the attribute to
 which each command stores its rewriting method.")

;;; Macro to define method for rewriting.
;;; The method should be a lambda expression.
(defmacro rewrite-method-def (sym method)
  (list 'put
        (list 'quote sym)
        (list 'quote rewrite-method)
        (list 'quote method)))

;;; Function to retrieve the rewriting method.
(defun rewrite-method-get (sym)
  "Retrieve the method for rewriting"
  (if (symbolp sym)
      (get sym rewrite-method)
      nil))

(defvar rewrite-delimiter "[ \t]*"
  "Default delimiter for rewriting rule")

(defconst rewrite-metacharacters '(* \? +)
"In Emacs the grouping construct \(...\) of regular expression
 has the following three purposes.
   To enclose a set of \| alternatives for other operations.
   To record a matched substring for future reference.
   To enclose an expression for suffix operator such as '*' to act on.
 The first and second purposes can be supported by
 our named regular expression construct. (You can use a dummy name
 in the first case.) In order to support the third case we
 treat the symbols in this constant specially. For example
 the grouping construct of the form \"(ab)*\" can be written
 (* \"ab\") in our case. It follows that we cannot use these symbols
 as names of matched subexpressions.")

;;; Interface macro.
;;; Please use this macro to call the rewriting system.
;;; Its syntax is given as follows.
;;; (rewrite-prog <command>*)
;;; where
;;;  <command> ::= (search <regular expr seq> <search options>?)|
;;;                (replace-match <newtext seq> <replace options>?)|
;;;                (rewrite <regular expr seq> <result seq>)|
;;;                (call <s-expression>*)|
;;;                (bind (<variable name> <s-expression>)*)
;;;                (if <condition> <s-expression> <s-expression>?)|
;;;                (while <condition> <s-expression>*)|
;;;                (progn <s-expression>*)|
;;;                (update <variable name> <s-expression>)
;;;                (insert . <regular expr seq>)
(defmacro rewrite-prog (&rest rule-list)
  (list 'rewrite-interpret-prog
        (list 'quote rule-list)
        '()))

;;; Main function for the rewriting system.
;;; Rewrite interpreter simply calls each 'rewrite method'
;;; stored in the 'rewrite-method'
;;; attribute of each command symbol.
(defun rewrite-interpret-prog (rules var-alist)
  (if (null rules)
      (rewrite-return nil var-alist)
    (rewrite-interpret-prog-cont (rewrite-rest rules)
                                  (rewrite-interpreter (rewrite-first rules)
                                                       var-alist))))

(defun rewrite-interpret-prog-cont (rules result-list)
  (if (null rules)
      result-list
    (let ((new-return-list
           (rewrite-interpreter (rewrite-first rules)
                                (rewrite-second result-list))))
      (rewrite-interpret-prog-cont (rewrite-rest rules)
                                    new-return-list))))

;;; Function to interpret each command.
;;; It simply calls the method stored in each symbol.
(defun rewrite-interpreter (rule var-alist)
  (let ((method (rewrite-method-get (rewrite-first rule))))
    (if (rewrite-value-existp method)
        (funcall method
                 (cdr rule)
                 var-alist)
      (error "Unknown Command: %s" (rewrite-first (car rules))))))

;;; Definition of 'search' command.
;;; Search command accepts the same optional arguments as
;;; 're-search-forward' i.e., limit noerror repeat.
;;; If these options are not specified, nil is assumed to be 
;;; the default value.
;;; In the program below we made use of the fact that
;;; both (car nil) and (cdr nil) are equal to nil in Emacs Lisp.
(rewrite-method-def search
  (lambda (rule-body var-alist)
    (let ((regexpr-seq (rewrite-first rule-body))
          (limit (rewrite-second rule-body));; 1st option
          (noerror (rewrite-third rule-body)) ;; 2nd option
          (repeat (rewrite-fourth rule-body))) ;; 3rd option
      (rewrite-pre-search 
	(rewrite-construct-search-regexpr regexpr-seq
	var-alist)
                          var-alist
                          limit
                          noerror
                          repeat))))

;;; Function to construct a regular expression string.
;;; It returns a pair of a regular expression string
;;; and an association list consisting of variable name and an integer.
;;; If the integer is n, it means that the variable is the n-th variable
;;; appearing in the regular expression sequence.
;;; For example, if the 1st argument 'string-list' is equal to
;;; ((word1 "[^ \t\n]+") "and" (word2 "[^ \t\n]+")),
;;; then the return value would be something like
;;; ("\\([^ \t\n]+\\)[ \t]*and[ \t]*\\([^ \t\n]+\\)"
;;;  ((word1 1)(word2 2))),
;;;  where [ \t]* is a default delimiter defined by rewrite-delimiter.
(defsubst rewrite-construct-search-regexpr (string-list env)
  (rewrite-construct-search-regexpr-aux string-list "" 1 '() env))

(defun rewrite-construct-search-regexpr-aux
  (string-list regexpr-string count result env)
  (if (null string-list)
      (list regexpr-string result)
    (let ((delimiter (if (null (cdr string-list))
                         ""
                       rewrite-delimiter)))
      (cond
        ((stringp (car string-list))
         (rewrite-construct-search-regexpr-aux (cdr string-list)
                                               (concat regexpr-string
                                                       (car string-list)
                                                       delimiter)
                                               count
                                               result
                                               env))
        ((listp (car string-list)) ;;; first element should be a variable
         (let* ((sub-result
                 (rewrite-construct-search-regexpr-aux
                  (cdr (car string-list))
                  ""
                  (+ count 1)
                  (cons (list (car (car string-list))
                              count)
                        result)
                  env))
                (regexpr-substring (rewrite-first sub-result))
                (new-result (rewrite-second sub-result))
                (delimiter-or-metachar (if (memq (car (car string-list))
                                                 rewrite-metacharacters)
                                           (symbol-name
                                            (car (car string-list)))
                                         delimiter)))
           (rewrite-construct-search-regexpr-aux
            (cdr string-list)
            (concat regexpr-string
                    "\\("
                    regexpr-substring
                    "\\)"
                    delimiter-or-metachar)
            (+ 1 (length new-result))
            new-result
            env)))
        ((symbolp (car string-list))
         (let ((count-value (rewrite-look-up (car string-list) result)))
           (if (rewrite-value-existp count-value)
               (rewrite-construct-search-regexpr-aux (cdr string-list)
                                                     (concat regexpr-string
                                                             "\\"
                                                             count-value
                                                             delimiter)
                                                     count
                                                     result
                                                     env)
             (let ((value (rewrite-look-up (car string-list) env)))
               (if (rewrite-value-existp value)
                   (rewrite-construct-search-regexpr-aux (cdr string-list)
                                                         (concat regexpr-string
                                                                 value
                                                                 delimiter)
                                                         count
                                                         result
                                                         env)
                 (error "Unknown Variable %s" (car string-list)))))))
        (t (error "Unknown Regular Expression %s" (car string-list)))))))

;;; Function to associate each varible with the matched substring.
;;; It first searches the buffer using 're-search-forward
;;; and then makes an association list of variable 
;;; and the matched substring.
(defun rewrite-pre-search
  (regexpr-and-name-list var-alist limit noerror repeat)
;;; last 3 arguments are options to re-search-forward
  (let ((regexpr-string (rewrite-first regexpr-and-name-list))
        (var-and-number-alist (rewrite-second regexpr-and-name-list)))
    (let ((search-result
	   (re-search-forward regexpr-string limit noerror repeat)))
      (cond ((not (null search-result))
	     (rewrite-return
	      search-result
	      (rewrite-add-new-var var-and-number-alist var-alist)))
	    ((null var-and-number-alist)
	     (rewrite-return
	      search-result
	      var-alist))
	    (t
	     (rewrite-return
	      search-result ;;; nil
	      nil))))))

(defun rewrite-add-new-var (var-and-number-alist env)
  (if (null var-and-number-alist)
      env
    (rewrite-add-new-var
     (cdr var-and-number-alist)
     (cons (list
            (rewrite-first (car var-and-number-alist))
            (buffer-substring
             (match-beginning
              (rewrite-second (car var-and-number-alist)))
             (match-end
              (rewrite-second (car var-and-number-alist)))))
           env))))

(defsubst rewrite-return (return-value env) (list return-value env))
;;; Note that the evaluation in a list is performed 
;;; from left to right order.

;;; Definition of 'replace-match' command.
(rewrite-method-def replace-match
  (lambda (rule-body env)
    (let ((newtext
           (rewrite-make-result-string (rewrite-first rule-body)
                                       ""
                                       ; '()
                                       env))
          (fixedcase (rewrite-second rule-body));; 1st option
          (literal (rewrite-third rule-body))) ;; 2nd option
      (rewrite-return
       (replace-match newtext
                      fixedcase
                      literal)
       env)))) ;;; env is not modified.

;;; Definition of rewrite command.
(rewrite-method-def rewrite
  (lambda (rule-body var-alist)
    (let ((regexpr-seq (rewrite-first rule-body))
          (newtext-seq (rewrite-second rule-body)))
      (beginning-of-buffer)
      (let ((result-list
             (rewrite-pre-search (rewrite-construct-search-regexpr regexpr-seq
                                                                   var-alist)
                                 var-alist
                                 nil ;; whole buffer
                                 t   ;; no error
                                 nil)))  ;; search the 1st match
;	(print result-list)
        (while (rewrite-first result-list)
           (replace-match (rewrite-make-result-string newtext-seq
                                                      ""
                                                      (rewrite-second result-list))
                          t ;;; fixedcase
                          t);;; literal
           (setq result-list
                 (rewrite-pre-search
                  (rewrite-construct-search-regexpr regexpr-seq
                                                    var-alist)
                  var-alist
                  nil ;; whole buffer
                  t   ;; no error
                  nil)))  ;; search the 1st match
        result-list))))

;;; (rewrite <search-regexpr-seq> <result-string-seq>) is equivalent to
;;; the following:
;;; (progn (call (beginning-of-buffer))
;;;        (while (search <search-regexpr-seq> nil t nil)
;;;               (replace-match <result-string-seq> t t)))

(defun rewrite-make-result-string
  (result-pattern-list result-pattern-string env)
  (cond ((null result-pattern-list) result-pattern-string)
        ((stringp (car result-pattern-list))
         (rewrite-make-result-string (cdr result-pattern-list)
                                     (concat result-pattern-string
                                             (car result-pattern-list))
                                     env))
        (t
         (let ((result-string (rewrite-look-up (car result-pattern-list) env)))
           (if (rewrite-value-existp result-string)
               (rewrite-make-result-string (cdr result-pattern-list)
                                           (concat result-pattern-string
                                                   result-string)
                                               env)
             (error "Unknown Variable: %s" (car result-pattern-list)))))))

;;; Definition of 'call' command.
(rewrite-method-def call
  (lambda (rule-body var-alist)
    (rewrite-return
     (eval (cons 'progn (rewrite-instantiate rule-body var-alist)))
     var-alist)))

(defun rewrite-look-up (expr var-alist)
  (let ((a-value (assq expr var-alist)))
    (if (null a-value)
        nil
      (rewrite-second a-value))))

(defsubst rewrite-value-existp (value) value)

;; Definition of 'bind' command.
(rewrite-method-def bind
  (lambda (rule-body var-alist)
    (rewrite-return
     t
     (rewrite-add-bind rule-body var-alist))))

(defun rewrite-add-bind (bind-list var-alist)
  (if (null bind-list)
      var-alist
    (rewrite-add-bind (cdr bind-list)
              (cons (list (rewrite-first (car bind-list))
                          (eval (rewrite-second (car bind-list))))
                    var-alist))))

;;; Definition of 'if' command.
(rewrite-method-def if
  (lambda (rule-body var-alist)
    (let ((result-list
           (rewrite-interpreter (rewrite-first rule-body) var-alist))
          (then-part (rewrite-second rule-body))
          (else-or-nil (cdr (cdr rule-body))))
      (if (rewrite-value-existp (rewrite-first result-list))
          (rewrite-interpreter then-part (rewrite-second result-list))
        (rewrite-interpret-prog else-or-nil (rewrite-second result-list))))))

;;; Definition of 'progn' command.
(rewrite-method-def progn
  (lambda (rule-body var-alist)
    (rewrite-interpret-prog rule-body var-alist)))

(defun rewrite-instantiate (expr var-alist)
  (cond ((null expr)
         '())
        ((atom expr)
         (let ((value (rewrite-look-up expr var-alist)))
           (if (rewrite-value-existp value)
               value
             expr)))
        (t (cons (rewrite-instantiate (car expr) var-alist)
                 (rewrite-instantiate (cdr expr) var-alist)))))

;;; Definition of 'while' command.
(rewrite-method-def while
  (lambda (rule-body var-alist)
    (rewrite-interpret-while (rewrite-first rule-body)
                              (rewrite-rest rule-body)
                              var-alist)))

(defun rewrite-interpret-while (condition body env)
  ;;; I prefer recursive call. But emacs lisp does not seem to
  ;;; optimize tail recursion.
  (let ((condition-result
         (rewrite-interpreter condition env))
        (while-body (rewrite-rest rule-body)))
    (while (rewrite-value-existp (rewrite-first condition-result))
      (let ((result-list
             (rewrite-interpret-prog while-body
                                      (rewrite-second condition-result))))
        (setq condition-result
              (rewrite-interpreter condition
                                   (rewrite-second result-list)))))))

;;; Definition of 'update' command.
(rewrite-method-def update
  (lambda (rule-body var-alist)
    (rewrite-update (rewrite-first rule-body)
                    (eval (rewrite-instantiate (rewrite-second rule-body)
                                         var-alist))
                    var-alist)))

(defsubst rewrite-update (var value var-alist)
  (rewrite-return
   (rewrite-update-aux var value var-alist)
   var-alist))

(defun rewrite-update-aux (var value var-alist)
  (cond ((null var-alist)
         (error "Unknown Varible: %s" var))
        ((eql var (car (car var-alist)))
         (setcar var-alist
                 (list var value)))
        (t (rewrite-update-aux var value (cdr var-alist)))))

;;; Definition of 'insert' command.
(rewrite-method-def insert
  (lambda (rule-body var-alist)
    (rewrite-return
     (insert
      (apply (function concat)
             (rewrite-instantiate rule-body var-alist)))
      ;; 'rule-body' may contain integer valued variables.
      ;; We use 'concat' to convert these integers.
     var-alist)))

;;; usual utilities
;;; Note that these functions will return nil,
;;; when the argument is equal to nil.
(defsubst rewrite-first (L) (car L))
(defsubst rewrite-second (L) (car (cdr L)))
(defsubst rewrite-third (L) (car (cdr (cdr L))))
(defsubst rewrite-fourth (L) (car (cdr (cdr (cdr L)))))

(defsubst rewrite-rest (x) (cdr x))

(provide 'rewrite)

; <<Small samples>>
;; Converting tab separated lines into a table in html.
;  (setq rewrite-delimiter "[\t]+")
;  (rewrite-prog
;   (insert "<TABLE BORDER=2>\n")
;   (rewrite ((date "[^\t\n]+")(title "[^\t\n]+")
;             (count "[^\t\n]+")(name "[^\t\n]+"))
;            ("<TR>\n"
;             "<TD>" date "</TD>\n"
;             "<TD>" title "</TD>\n"
;             "<TD>" count "</TD>\n"
;             "<TD>" name "</TD>\n"
;             "</TR>\n"))
;   (insert "</TABLE>"))
;------
;; Adding counting numbers to first fields
; (setq rewrite-delimiter "[ \t]+")
; (rewrite-prog
;  (call (beginning-of-buffer))
;  (bind (count 1))
;  (while (search ((a "[^ \t\n]+")(b "[^ \t\n]+")
;                  (c "[^ \t\n]+")(d "[^\t\n]+")))
;    (replace-match (a count " "  b " " c " " d))
;    (update count (+ count 1))))
; This program will convert a text of four columns
;a b c d
;d e f g
;x y z w
; into the following.
;a1 b c d
;d2 e f g
;x3 y z w
;------
;; Making URL string clickable.
;(rewrite-prog
; (while
;   (search
;    ((url
;      "http:/" (+ "/"  (name "[^/\n]+") ) "/" (name1 "[^/\n]*")))
;    nil t nil)
;  (if (call (member name1 '("" "index.html" "index.htm")))
;      (replace-match
;       ("<a href=" "\"" url "\">" url "</a>") t t)
;      ;; use the whole url as a reference name.
;    (replace-match
;     ("<a href=" "\"" url "\">" name1 "</a>") t t))))
;      ;; use the file name as a reference name.

;; More sample programs are found at the following URL.
;; http://math-sci.hp.infoseek.co.jp/Lisp/EmacsLisp/

;;; rewrite.el ends here

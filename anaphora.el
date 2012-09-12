;;; anaphora.el --- anaphoric macros providing implicit temp variables
;;
;; This code is in the public domain.
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/anaphora
;; URL: http://raw.github.com/rolandwalker/anaphora/master/anaphora.el
;; Version: 0.0.1
;; Last-Updated: 12 Sep 2012
;; EmacsWiki: Anaphora
;; Keywords: extensions
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'anaphora)
;;
;;     (awhen (big-long-calculation)
;;       (foo it)      ; `it' is provided as
;;       (bar it))     ; a temporary variable
;;
;;     ;; anonymous function to compute factorial using `self'
;;     (alambda (x) (if (= x 0) 1 (* x (self (1- x)))))
;;
;; Explanation
;;
;; Anaphoric expressions implicitly create one or more temporary
;; variables which can be referred to during the expression.  This
;; technique can improve clarity in certain cases.  It also enables
;; recursion for anonymous functions.
;;
;; To use anaphora, place the anaphora.el library somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'anaphora)
;;
;; The following macros are made available
;;
;;     `aif'
;;     `aprog1'
;;     `awhen'
;;     `awhile'
;;     `aand'
;;     `acond'
;;     `alambda'
;;     `ablock'
;;     `acase'
;;     `aecase'
;;     `atypecase'
;;     `aetypecase'
;;
;; See Also
;;
;;     M-x customize-group RET anaphora RET
;;     http://en.wikipedia.org/wiki/On_Lisp
;;     http://en.wikipedia.org/wiki/Anaphoric_macro
;;
;; Notes
;;
;; Principally based on examples from the book "On Lisp", by Paul
;; Graham.
;;
;; When this library is loaded, the provided anaphoric forms are
;; registered as keywords in font-lock. This may be disabled via
;; customize.
;;
;; Compatibility and Requirements
;;
;;     Tested on GNU Emacs versions 23.3 and 24.1
;;
;; Bugs
;;
;;     (acond (1)) should evaluate to 1 as does (cond (1))
;;
;; TODO
;;
;;; License
;;
;; This code is in the public domain.  It is provided without
;; any express or implied warranties.
;;
;;; Code:
;;

;;; requires

(eval-when-compile
  ;; for declare, labels, do, block, case, ecase, typecase, etypecase
  (require 'cl))

(declare-function gensym  "cl-macs.el")

;;; customizable variables

;;;###autoload
(defgroup anaphora nil
  "Anaphoric macros providing implicit temp variables"
  :version "0.0.1"
  :link '(emacs-commentary-link "anaphora")
  :prefix "anaphora-"
  :group 'extensions)

(defcustom anaphora-add-font-lock-keywords t
  "Add anaphora macros to font-lock keywords when editing Emacs Lisp."
  :type 'boolean
  :group 'anaphora)

;;; font-lock

(when anaphora-add-font-lock-keywords
  (eval-after-load "lisp-mode"
    '(progn
       (let ((new-keywords '(
                             "aif"
                             "aprog1"
                             "awhen"
                             "awhile"
                             "aand"
                             "acond"
                             "alambda"
                             "ablock"
                             "acase"
                             "aecase"
                             "atypecase"
                             "aetypecase"
                             )))
         (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "(\\s-*" (regexp-opt new-keywords 'paren) "\\>")
                                                     1 font-lock-keyword-face)) 'append))
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (and (eq major-mode 'emacs-lisp-mode)
                      (boundp 'font-lock-mode)
                      font-lock-mode)
             (font-lock-refresh-defaults)))))))

;;; macros

;;;###autoload
(defmacro aif (cond then &rest else)
  "Like `if', except that the value of COND is bound to `it'.

The variable `it' is available within THEN and ELSE."
  (declare (indent 2))
  `(let ((it ,cond))
     (if it ,then ,@else)))

;;;###autoload
(defmacro aprog1 (first &rest body)
  "Like `prog1', except that the value of FIRST is bound to `it'.

The variable `it' is available within BODY."
  (declare (indent 1))
  `(let ((it ,first))
     (progn ,@body)
     it))

;;;###autoload
(defmacro awhen (cond &rest body)
  "Like `when', except that the value of COND is bound to `it'.

The variable `it' is available within BODY."
  (declare (indent 1))
  `(aif ,cond
       (progn ,@body)))

;;;###autoload
(defmacro awhile (test &rest body)
  "Like `while', except that the value of TEST is bound to `it'.

The variable `it' is available within BODY."
  (declare (indent 1))
  `(do ((it ,test ,test))
       ((not it))
     ,@body))

;;;###autoload
(defmacro aand (&rest conditions)
  "Like `and', except that the value of the previous condition is bound to `it'.

The variable `it' is available within all CONDITIONS after the
initial one.

Note that some implementations of `aand' bind only the first
condition to `it', rather than each successive condition."
  (cond
    ((null conditions)
     t)
    ((null (cdr conditions))
     (car conditions))
    (t
     `(aif ,(car conditions) (aand ,@(cdr conditions))))))

;;;###autoload
(defmacro acond (&rest clauses)
  "Like `cond', except that the value of each condition is bound to `it'.

The variable `it' is available within the remainder of each of CLAUSES."
  (declare (indent 0))
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
             (let ((it ,sym)) ,@(cdr cl1))
           (acond ,@(cdr clauses)))))))

;;;###autoload
(defmacro alambda (args &rest body)
  "Like `lambda', except that the function may refer to itself as `self'.

ARGS and BODY are as documented for `lambda'."
  (declare (indent defun))
  `(labels ((self ,args ,@body))
     #'self))

;;;###autoload
(defmacro ablock (name &rest body)
  "Like `block', except that the value of the previous expression is bound to `it'.

The variable `it' is available within all expressions of BODY
except the initial one."
  (declare (indent 1))
  `(block ,name
     ,(funcall (alambda (body)
                        (case (length body)
                          (0 nil)
                          (1 (car body))
                          (t `(let ((it ,(car body)))
                                ,(self (cdr body))))))
               body)))

;;;###autoload
(defmacro acase (expr &rest clauses)
  "Like `case', except that the value EXPR is bound to `it'.

The variable `it' is available within CLAUSES."
  (declare (indent 1))
  `(let ((it ,expr))
     (case it ,@clauses)))

;;;###autoload
(defmacro aecase (expr &rest clauses)
  "Like `ecase', except that the value EXPR is bound to `it'.

The variable `it' is available within CLAUSES."
  (declare (indent 1))
  `(let ((it ,expr))
     (ecase it ,@clauses)))

;;;###autoload
(defmacro atypecase (expr &rest clauses)
  "Like `typecase', except that the value EXPR is bound to `it'.

The variable `it' is available within CLAUSES."
  (declare (indent 1))
  `(let ((it ,expr))
     (typecase it ,@clauses)))

;;;###autoload
(defmacro aetypecase (expr &rest clauses)
  "Like `etypecase', except that the value EXPR is bound to `it'.

The variable `it' is available within CLAUSES."
  (declare (indent 1))
  `(let ((it ,expr))
     (etypecase it ,@clauses)))

(provide 'anaphora)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;
;; LocalWords: Anaphora EXPR awhen COND ARGS
;;

;;; anaphora.el ends here

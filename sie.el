;;; sie.el --- Interpreter of a Scheme-like language

;; Author: Kalle Niemitalo <tosi@stekt.oulu.fi>
;; Keywords: local, lisp

;; This file is not part of GNU Emacs, but the same conditions apply.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file implements an interpreter of a Scheme-like language.
;; But unlike real Scheme, it isn't properly tail-recursive.

;; Scheme functions supported
;; --------------------------
;;
;; lambda quote if set! + - * / car cdr

;; Representation of types
;; -----------------------
;;
;; Symbols, numbers, strings and pairs are directly represented as the
;; corresponding Emacs Lisp types.
;;
;; Vectors are represented as Emacs Lisp vectors whose first element
;; is not `eq' to the value of the variable `sie-magic'.
;;
;; Other types are represented as vectors whose first element is `eq'
;; to the value of the variable `sie-magic'.  Cons cells could be used
;; instead, but vectors are rarer so we don't have to check them as
;; often.
;;
;; Booleans:
;;   #f:  `[,sie-magic sie-false]
;;   #t:  `[,sie-magic sie-true]
;;   These vectors are kept in variables `sie-false' and `sie-true' so
;;   SIE Booleans can be compared with `eq'.
;;
;; Characters:
;;   `[,sie-magic sie-char ,code]
;;
;; Lambda procedures:
;;   `[,sie-magic sie-lambda ,args+body ,environment]
;;
;; Subroutines:
;;   `[,sie-magic sie-subr ,elisp-function]
;;   The elisp-function is called with the evaluated arguments.
;;
;; Syntaxes:
;;   `[,sie-magic sie-syntax ,elisp-function]
;;   The elisp-function is called with two arguments: list of
;;   unevaluated arguments in the call, and the environment.
;;
;; Environments are represented by simple alists.  Scheme code cannot
;; access them directly.

;;; Code:

(defconst sie-magic
  (vector 'sie-magic)
  "Magic object for used for encoding Scheme values.
If the first element of a vector is `eq' to `sie-magic', that vector
represents a Scheme object of a type which does not exist in Emacs
Lisp.")

(defun sie-magical-p (object)
  "Return t if OBJECT is a vector but represents something else."
  (and (vectorp object)
       (eq (aref object 0) sie-magic)))

(defconst sie-false
  (vector sie-magic 'sie-false)
  "Emacs representation of Scheme #f.")

(defconst sie-true
  (vector sie-magic 'sie-true)
  "Emacs representation of Scheme #t.")
  
(defalias 'sie-number-p 'numberp)
(defalias 'sie-string-p 'stringp)
(defalias 'sie-symbol-p 'symbolp)
(defalias 'sie-pair-p 'consp)
(defalias 'sie-car 'car)
(defalias 'sie-cdr 'cdr)

(defun sie-vector-p (object)
  "Return t if OBJECT represents a Scheme vector."
  (and (vectorp object)
       (not (eq (aref object 0) sie-magic))))

(defun sie-bool-p (object)
  "Return t if OBJECT represents Scheme #f or #t."
  (or (eq object sie-false)
      (eq object sie-true)))

(defun sie-char-p (object)
  "Return t if OBJECT represents a Scheme character."
  (and (sie-magical-p object)
       (eq (aref object 1) 'sie-char)))

(defun sie-lambda-p (object)
  "Return t if OBJECT is a SIE lambda procedure."
  (and (sie-magical-p object)
       (eq (aref object 1) 'sie-lambda)))

(defun sie-lambda-args (lambda-procedure)
  "Return the arguments of LAMBDA-PROCEDURE.
LAMBDA-PROCEDURE must be sie-lambda-p."
  (car (aref lambda-procedure 2)))

(defun sie-lambda-body (lambda-procedure)
  "Return the body forms of LAMBDA-PROCEDURE.
LAMBDA-PROCEDURE must be sie-lambda-p."
  (cdr (aref lambda-procedure 2)))

(defun sie-lambda-environment (lambda-procedure)
  "Return the environment of LAMBDA-PROCEDURE.
LAMBDA-PROCEDURE must be sie-lambda-p."
  (aref lambda-procedure 3))

(defun sie-subr-p (object)
  "Return t if OBJECT is a SIE subroutine."
  (and (sie-magical-p object)
       (eq (aref object 1) 'sie-subr)))

(defun sie-subr-function (subr)
  "Return the Emacs Lisp function corresponding to SUBR.
SUBR must be sie-subr-p."
  (aref subr 2))

(defun sie-syntax-p (object)
  "Return t if OBJECT is a SIE syntax."
  (and (sie-magical-p object)
       (eq (aref object 1) 'sie-syntax)))

(defun sie-syntax-function (syntax)
  "Return the Emacs Lisp function corresponding to SYNTAX.
SYNTAX must be sie-syntax-p."
  (aref syntax 2))

 
(defun sie-eval (form environment)
  "Evaluate Scheme FORM in ENVIRONMENT and return the result."
  (cond
   ((sie-number-p form) form)
   ((sie-string-p form) form)
   ((sie-bool-p form) form)
   ((sie-char-p form) form)
   ((sie-symbol-p form) (sie-symbol-value form environment))
   ((sie-pair-p form)
    (let ((proc (sie-eval (sie-car form) environment))
	  (raw-args (sie-cdr form)))
      (if (sie-syntax-p proc)
	  (funcall (sie-syntax-function proc) raw-args environment)
	(sie-apply proc
		   (mapcar (lambda (elt) (sie-eval elt environment))
			   raw-args)))))
   (t (error "sie-eval: can't handle %S" form))))
    
(defun sie-symbol-value (symbol environment)
  "Return the value SYMBOL has in ENVIRONMENT."
  (cdr (sie-symbol-binding symbol environment)))

(defun sie-symbol-binding (symbol environment)
  "Return the binding of SYMBOL in ENVIRONMENT.
The car is the symbol and the cdr is the value."
  (let ((binding (assq symbol environment)))
    (or binding (error "SIE symbol not bound: %S" symbol))
    binding))

(defun sie-apply (proc args)
  "Call Scheme PROC with already evaluated ARGS."
  (cond ((sie-subr-p proc)
	 (apply (sie-subr-function proc) args))
	((sie-lambda-p proc)
	 (let* ((lambda-env (sie-lambda-environment proc))
		(formal-args (sie-lambda-args proc))
		(body (sie-lambda-body proc))
		(new-env (sie-build-environment formal-args args lambda-env)))
	   (while (not (null (cdr body)))
	     (sie-eval (car body) new-env)
	     (setq body (cdr body)))
	   (sie-eval (car body) new-env)))
	(t (error "Invalid SIE procedure: %s" (sie-prin1-to-string proc)))))

(defun sie-build-environment (formal-args actual-args environment)
  "Return a new environment which binds FORMAL-ARGS to ACTUAL-ARGS
and inherits ENVIRONMENT."
  (while (consp formal-args)
    (setq environment (cons (cons (car formal-args) (car actual-args))
			    environment)
	  formal-args (cdr formal-args)
	  actual-args (cdr actual-args)))
  (if formal-args			; rest argument
      (setq environment (cons (cons formal-args actual-args))))
  environment)

 
(defun sie-syn-lambda (raw-args environment)
  "Create a Scheme procedure."
  (vector sie-magic 'sie-lambda raw-args environment))

(defun sie-syn-quote (raw-args environment)
  "Return the unevaluated argument."
  (car raw-args))

(defun sie-syn-if (raw-args environment)
  (if (eq (sie-eval (nth 0 raw-args) environment) sie-false)
      (sie-eval (nth 2 raw-args) environment)
    (sie-eval (nth 1 raw-args) environment)))

(defun sie-syn-set! (raw-args environment)
  (setcdr (sie-symbol-binding (nth 0 raw-args) environment)
	  (sie-eval (nth 1 raw-args) environment)))

 
(defconst sie-minimal-environment
  `((lambda . [,sie-magic sie-syntax sie-syn-lambda])
    (quote . [,sie-magic sie-syntax sie-syn-quote])
    (if . [,sie-magic sie-syntax sie-syn-if])
    (set! . [,sie-magic sie-syntax sie-syn-set!])))

(defconst sie-basic-environment
  `((+ . [,sie-magic sie-subr +])
    (- . [,sie-magic sie-subr -])
    (* . [,sie-magic sie-subr *])
    (/ . [,sie-magic sie-subr /])
    (car . [,sie-magic sie-subr car])
    (cdr . [,sie-magic sie-subr cdr])
    ,@sie-minimal-environment))

 
(defun sie-prin1-to-string (object &optional noescape)
  (cond
   ((sie-pair-p object)
    (let ((str (concat "(" (sie-prin1-to-string (sie-car object)))))
      (setq object (sie-cdr object))
      (while (sie-pair-p object)
	(setq str (concat str " "
			  (sie-prin1-to-string (sie-car object)))
	      object (sie-cdr object)))
      (if object
	  (concat str " . " (sie-prin1-to-string object) ")")
	(concat str ")"))))
   ((sie-bool-p object)
    (if (eq object sie-false) "#f" "#t"))
   ((sie-lambda-p object)
    (concat "#<sie-lambda args: "
	    (sie-prin1-to-string (sie-lambda-args object) noescape)
	    " body-forms: "
	    (sie-prin1-to-string (sie-lambda-body object) noescape)
	    ;; " bindings: "
	    ;; (sie-prin1-to-string (sie-lambda-environment object) noescape)
	    ">"))
   ((sie-subr-p object)
    (concat "#<sie-subr "
	    (prin1-to-string (sie-subr-function object) noescape)
	    ">"))
   ((sie-syntax-p object)
    (concat "#<sie-syntax "
	    (prin1-to-string (sie-syntax-function object) noescape)
	    ">"))
   (t
    (prin1-to-string object noescape))))

(provide 'sie)
;;; sie.el ends here

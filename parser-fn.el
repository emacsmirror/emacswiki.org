;;;
;;----------------------------------------------------------------------
;; parser-fn.el
;; Primary Author: Mike Mattie (codermattie@gmail.com)
;; Copyright (C) 2008 Mike Mattie
;; License: LGPL
;;----------------------------------------------------------------------

;; Generic library routines used by parser.el.

;;----------------------------------------------------------------------
;; m-list.el
;;----------------------------------------------------------------------

(defun list-filter-nil ( list )
  "Filter nil symbols from a list"
  (remq 'nil list))

(defun seq-filter-nil ( &rest list-seq )
  "Filter nil symbols from a sequence."
  (list-filter-nil list-seq))

(defun precise-list-p ( x )
  "precise-list-p X

   more precise listp predicate. Checks that x is both a cons,
   and the cdr of x is a cons.
  "
  (and
    (consp x)
    (consp (cdr x))))

(defun consume-list ( list consume )
  "consume-list LIST CONSUME

   use a consume function to consume a list. unlike mapc instead
   of consuming only a single element of the list at a time the
   head, and the tail are passed to CONSUME so that a single call
   can consume a variable n elements from list.

   This function amounts to a TCO kludge allowing the function to
   be expressed in a recursive form."
  (when list
    (while (setq list (funcall consume (car list) (cdr list)))))
  nil)

(defun apply-n-times ( func n x )
  "apply-n-times FUNC N X

   apply FUNC to X N times, With X set to the return
   value of FUNC for each iteration.
  "
  (while (> n 0)
    (setq x (funcall func x))
    (decf n))
  x)

(defun split-list ( n list )
  "split-list N LIST
   return a cons of LIST split into two lists at index N.
  "
  (if (> n 1)
    (lexical-let
      ((a-list list)
        (b-list nil)
        (before-split (apply-n-times 'cdr (- n 1) list)))

      (setq b-list (cdr before-split))
      (setcdr before-split nil)

      (cons a-list b-list))
    (cons (cons (car list) nil) (cdr list))))

(defun or-fn-list ( list )
  "iterate through the list of functions. If a function returns t for
   success terminate the iteration. It's a fancy or that assumes a list
   of functions."
  (catch 'terminate
    (dolist (func list)
      (if (funcall func)
        (throw 'terminate t)))
    nil))

;;----------------------------------------------------------------------
;; tail iterator
;;----------------------------------------------------------------------

(defun tail-iterator-merge ( a b )
  (setcdr a b)

  (do ((x b))
    ((null (cdr x)) x)
    (setq x (cdr x))))

(defun tail-iterator ( bind-to )
  (set bind-to (cons nil nil))

  (lexical-let
    ((tail (symbol-value bind-to)))

    (lambda ( x )
      (if (precise-list-p x)
        (setq tail (tail-iterator-merge tail x))
        (progn
          (setcdr tail (cons x nil))
          (setq tail (cdr tail))) )) ))

(defun tail-list ( list )
  (cdr list))

;;----------------------------------------------------------------------
;; cm-lisp.el
;;----------------------------------------------------------------------

(defun eqn ( a b )
  "eqn A B

   A and B are equal if their symbol names are a string match.
  "
  (string-equal (symbol-name a) (symbol-name b)))

(defun bind-eval-lambda ( name sexp )
  "bind-eval-lambda NAME SEXP
   bind the function of a un-interned symbol named NAME to an evaluation
   of a SEXP."
  (let
    ;; this would be cooler if it used one of the unique algorithms.
    ((anon-func (make-symbol name)))
    (fset anon-func (eval sexp))
    anon-func))

;;----------------------------------------------------------------------
;; function-arity
;;----------------------------------------------------------------------

;; copied from the page:
;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg00056.html
;; Author: Kevin Rodgers, kevin.d.rodgers@gmail.com

(require 'help-fns)

(defun lambda-arity (function)
  "Return minimum and maximum number of args allowed for FUNCTION.
FUNCTION must be a symbol whose function binding is a lambda expression
or a macro.
The returned value is a pair (MIN . MAX).  MIN is the minimum number
of args.  MAX is the maximum number or the symbol `many', for a lambda
or macro with `&rest' args."
  (let* ((arglist (help-function-arglist function))
         (optional-arglist (memq '&optional arglist))
         (rest-arglist (memq '&rest arglist)))
    (cons (- (length arglist)
             (cond (optional-arglist (length optional-arglist))
                   (rest-arglist (length rest-arglist))
                   (t 0)))
          (cond (rest-arglist 'many)
                (optional-arglist (+ (length arglist)
                                     (length optional-arglist)
                                     -1))
                (t (length arglist))))))

(defun function-arity ( function )
  (if (subrp function)
    (subr-arity function)
    (lambda-arity function)))

;; define-error originated in XEmacs. This implementation shares the
;; same name, but not the interface. I need to clone or copy the
;; XEmacs version.

(defmacro define-error ( symbol message &rest isa-list )
  "define a error symbol with a isa list and a error message"
  `(progn
     (put ',symbol
       'error-conditions (append '(error ,symbol) ',isa-list))
     (put ',symbol 'error-message ,message) ))

(provide 'parser-fn)

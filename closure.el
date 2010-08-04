;;; closure.el --- Experimental closure prototypes

;; Copyright (C) 2008 Mike Mattie

;; Author: Mike Mattie
;; Version: 961
;; License: LGPL-V3

;;; Commentary:

;; these are experimental prototypes. The final versions need to be
;; implemented at the C level.

;;; Code:

;;----------------------------------------------------------------------
;; definition and creation
;;----------------------------------------------------------------------

(defmacro closure-define ( symbol &rest definitions )
  (set symbol
    (mapcar
      (lambda ( def )
        (cons (car def) (cadr def)))
      definitions))
  symbol)

(defvar closure-objarray-bucket-tuning 13
  "objarray creation requires a tuning value.")

(defun closure-copy ( closure )
  "copy CLOSURE an objarray so that the values are not shared unlike copy-sequence."
  (lexical-let
    ((copy (make-vector closure-objarray-bucket-tuning 0)))

    (mapatoms
      (lambda ( s )
        (lexical-let
          ((name (symbol-name s)))
          (set (intern name copy) (symbol-value (intern name closure))))) closure)
    copy))

(defun closure-create ( definition )
  "create a symbol table initializing SYMBOL with eval'd VALUE"
  (lexical-let
    ((table (make-vector closure-objarray-bucket-tuning 0)))

    (mapc (lambda ( pair )
            (set (intern (symbol-name (car pair)) table) (eval (cdr pair)))) definition)
    table))

;;----------------------------------------------------------------------
;; binding
;;----------------------------------------------------------------------

(defun closure-bind-scope ( closure body )
  "traverse the tree depth first pre-binding any symbol found in closure."
  ;; might be better to just use a cl low level library function.
  (if (consp body)
    (lexical-let
      ((atom (car body)))

      (cons
        (if (listp atom)
          (closure-bind-scope closure atom)

          (if (symbolp atom)
            (or
              (intern-soft (symbol-name atom) closure)
              atom)
            atom))

        (closure-bind-scope closure (cdr body))))
    body))

(defmacro save-lexical-closure ( closure &rest body )
  "a persistent lexical binding. The objarray CLOSURE appears lexically
   scoped in that a recursive traversal binds symbols of equal name
   in CLOSURE. altering these pre-bound symbols with setq changes the
   value in CLOSURE allowing the values to persist beyond the form in
   objarray CLOSURE.

   Currently this is a experimental hack so it incurs the cost
   of a recursive pre-bind in addition to eval each time evaluated."
  (declare (debug (symbolp body)))
  `(eval (closure-bind-scope ,closure ',(if (eq 'lambda (car body))
                                          body
                                          (cons 'progn body)))))

(defun closure-let-binding ( s closure )
  `(,(read (symbol-name s)) ,(closure-symbol s closure)))

(defmacro use-dynamic-closure ( with-def &rest body )
  "use a saved closure as a dynamic scope with private copy."
  (declare (debug (form body)))
  (lexical-let
    ((definition    (eval (car with-def)))
     (closure       (eval (cadr with-def)))
     (bindings      nil))

    `(let
       ,(progn
          (mapc
            (lambda ( def )
              (push (closure-let-binding (car def) closure) bindings))
            definition)
          bindings)
       ,@body)))

(defmacro use-dynamic-closure-with ( with-def let-spec &rest body )
  "FIXME"
  (declare (debug (form form body)))

  (lexical-let
    ((definition    (eval (car with-def)))
     (closure       (eval (cadr with-def)))
     (bindings      nil))

    `(let
       ,(progn
          (mapc
            (lambda ( def )
              (push (closure-let-binding (car def) closure) bindings))
            definition)
          (append
            let-spec
            bindings))
       ,@body)))

;;----------------------------------------------------------------------
;; direct access
;;----------------------------------------------------------------------

(defun closure-value ( symbol closure )
  "closure-value SYMBOL CLOSURE

   return the value of SYMBOL in CLOSURE.
  "
  (symbol-value (intern (symbol-name symbol) closure)))

(defun closure-symbol ( symbol closure )
  "closure-symbol SYBMOL CLOSURE

   return SYMBOL from closure.
  "
  (intern (symbol-name symbol) closure))

;;----------------------------------------------------------------------
;; utilities
;;----------------------------------------------------------------------

(defun pp-closure ( closure )
  "pretty print a closure returning a string."
  (lexical-let
    ((strings nil))

    (mapatoms
      (lambda ( s )
        (push (format "symbol: %s = %s\n"
                (symbol-name s)
                (pp-to-string (symbol-value (intern (symbol-name s) closure)))) strings)) closure)
    (apply 'concat strings)))

(defun pp-closure-filtered ( filter closure )
  "pretty print a closure returning a string with filtering."
  (lexical-let
    ((strings nil))

    (mapatoms
      (lambda ( s )
        (lexical-let*
          ((name (symbol-name s))
           (value (symbol-value s)))

          (unless (funcall filter value)
            (push (format "symbol: %s = %s\n"
                    name
                    (pp-to-string value)) strings)) )) closure)
    (apply 'concat strings)))

(provide 'closure)

;;; closure.el --- closures for elisp
;;  Author: ap
;;  $Rev: 588 $

;;; Commentary:
;;  This is much like CL's `lexical-let', but different.

;;; Code:

;; Maybe it would be best, to exclude all symbols starting with & ?
(defvar closure-unsafe-symbols
  '(nil t lambda &rest &body &optional &key &aux
        &allow-other-keys &whole &environment)
  "Symbols which need to keep their identity in a closure,
  because various elisp code depends on it.")

(defmacro with-make-symbols (sym-list &rest body)
  "Let-bind symbols in sym-list to new, uninterned symbols."
  (declare (debug (sexp &rest form)) (indent 1))
  `(let ,(mapcar (lambda (sym)
                   `(,sym (make-symbol ,(format "%S" sym))))
                 sym-list)
     ,@body))

(with-make-symbols (sym copy form obarr include-p
                        exclude-symbols include-symbols)
  ;; Create a private set of symbols, so `boundp' works as expected.
  (mapc (lambda (form)
          (byte-compile (eval form)))
        (list
         `(defun intern-soft-copy (,sym ,obarr)
            "Get SYMBOL from OBARRAY or intern a copy.

If SYMBOL is already interned in OBARRAY, return it.  Otherwise
make a copy of the current binding of SYMBOL and intern it in
OBARRAY.  As opposed to `intern-soft', SYMBOL may not be a
string."
            (or (intern-soft ,sym ,obarr)
                ;; Copy symbols name.  It could point to purespace, in
                ;; which case `copy' will never be gc'ed.
                (let ((,copy (intern (copy-sequence (symbol-name ,sym))
                                     ,obarr)))
                  (when (boundp ,sym)
                    (set ,copy (symbol-value ,sym)))
                  (when (fboundp ,sym)
                    (fset ,copy (symbol-function ,sym)))
                  (setplist ,copy (symbol-plist ,sym))
                  ,copy)))

         `(defun make-closure (,form ,obarr &optional ,include-p)
            "Replace symbols in FORM with new symbols.

Use OBARR as a source and cache.  The new symbols will be copys
of the old ones. If INCLUDE-P is non-nil, it should be a
predicate to decide, whether a symbol should be copied or not."
            (cond
             ((consp ,form)
              (mapcar (lambda (,form)
                        (make-closure ,form ,obarr ,include-p)) ,form))
             ((and (symbolp ,form)
                   (and (or (not ,include-p)
                            (funcall ,include-p ,form))))
              (intern-soft-copy ,form ,obarr))
             (t ,form)))

         `(defun closure-safe-symbol-p (,sym)
            "Return t if sym is safe to copy in a closure.

Certain symbols, like `lambda' or `nil', would render the
constructed code broken, if they were copied."

            (not (or (keywordp ,sym)
                     (memq ,sym closure-unsafe-symbols))))


         `(defun make-safe-closure (,form ,obarr
                                          &optional
                                          ,exclude-symbols
                                          ,include-symbols)
            (make-closure
             ,form ,obarr
             (lambda (,sym)
               (and (if (closure-safe-symbol-p ,sym)
                        t
                      (when (memq ,sym ,include-symbols)
                        (error "Symbol is not safe in a closure : `%S'"
                               ,sym))
                      nil)
                    (not (memq ,sym ,exclude-symbols))
                    (or (not ,include-symbols)
                        (memq ,sym ,include-symbols))))))))
  nil)

(defmacro closure (&rest body)
  "Retain a copy of the (runtime-)bindings inside BODY.

Example :

\(defun make-counter \(\)
  \(let \(\(value 0\)\)
    \(flet \(\(myadd \(&rest args\)
             \(apply '+ args\)\)\)
      \(closure
       \(lambda \(&optional n\)
         \(setq value \(funcall 'myadd value \(or n 1\)\)\)\)\)\)\)\)

\(fset 'counter \(make-counter\)\)
\(counter\) => 1
\(counter 2\) => 3
\(boundp 'value\) => nil
\(fboundp 'myadd\) => nil

You can also use `closure-over', where you can specify, which
symbols should have a local binding.

See also `closure-let', `closure-let*' and `clambda'."
  (declare (debug (&rest form)))
  `(closure-over nil ,@body))

;; This version only operates inside lambda and defun forms.
;; (require 'cl)
;; (defmacro closure-over (symbols &rest body)
;;   "This is like `closure', but only \"closes\" over symbols
;; listed in SYMBOLS."
;;   (declare (indent 1)
;;            (debug (sexp &rest form)))
;;   (with-make-symbols (obarr)
;;     `(let ((,obarr (make-vector 13 0)))
;;        (macrolet ((lambda (args &rest body)
;;                     (list 'cons ''lambda
;;                           (list 'cons (list 'quote args)
;;                                 (list 'make-safe-closure
;;                                       (list 'quote body)
;;                                       ',obarr
;;                                       (list 'quote
;;                                             args)
;;                                       '',symbols))))
;;                   (defun (name args &rest body)
;;                     `(progn
;;                        (fset ',name (lambda ,args ,@body))
;;                        ',name)))
;;          ,@body))))

(defmacro closure-over (symbols &rest body)
  "This is like `closure', but only \"closes\" over symbols
listed in SYMBOLS."
  (declare (indent 1)
           (debug (sexp &rest form)))
  (with-make-symbols (obarr)
    `(let ((,obarr (make-vector 13 0)))
       (eval
        (cons 'progn
              (make-safe-closure
               ',body ,obarr nil ',symbols))))))

(defmacro clambda (args &rest body)
  "Like `lambda', but with a `closure'."
  (declare (indent defun)
           (debug lambda))
  `(closure
    (lambda ,args ,@body)))

(defmacro closure-let (bindings &rest body)
  "Like `let', but \"close\" over the bindings.
See `closure'."
  (declare (indent 1))
  (let ((vars (mapcar (lambda (sym)
                        (if (consp sym)
                            (car sym)
                          sym)) bindings)))
    `(let ,bindings
       (closure-over ,vars
         ,@body))))

(defmacro closure-let* (bindings &rest body)
  "Like `let*', but \"close\" over the bindings.
See `closure'."
  (declare (indent 1))
  (let ((vars (mapcar (lambda (sym)
                        (if (consp sym)
                            (car sym)
                          sym)) bindings)))
    `(let* ,bindings
       (closure-over ,vars
         ,@body))))

(provide 'closure)

;;; closure.el ends here

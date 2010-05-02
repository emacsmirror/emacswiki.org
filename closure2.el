;;; closure.el --- closures for elisp
;;  Author: politza@fh-trier.de

;;; Commentary:
;;  This is much like CL's `lexical-let', but different.

(require 'cl)

;;; Code:

(defmacro with-make-symbols (sym-list &rest body)
  "Let-bind symbols in sym-list to new, uninterned symbols."
  (declare (debug (sexp &rest form)) (indent 1))
  `(let ,(mapcar (lambda (sym)
                   `(,sym (make-symbol ,(format "%S" sym))))
                 sym-list)
     ,@body))

(defmacro intern-soft-copy (symbol obarray)
  "Get SYMBOL from OBARRAY or intern a copy.

If SYMBOL is already interned in OBARRAY, return it.  Otherwise
make a copy of the current binding of SYMBOL and intern it in
OBARRAY.  As opposed to `intern-soft', SYMBOL may not be a
string."
  (with-make-symbols (symbol* obarray* copy)
    `(let ((,symbol* ,symbol)
           (,obarray* ,obarray))
       (or (intern-soft ,symbol* ,obarray*)
           ;; Copy symbols name.  It could point to purespace, in
           ;; which case `copy' will never be gc'ed.
           (let ((,copy (intern (copy-seq (symbol-name ,symbol*))
                                ,obarray*)))
             (when (boundp ,symbol*)
               (set ,copy (symbol-value ,symbol*)))
             (when (fboundp ,symbol*)
               (fset ,copy (symbol-function ,symbol*)))
             (setplist ,copy (symbol-plist ,symbol*))
             ,copy)))))

(defun make-closure (form obarr &optional exclude-symbols)
  "Replace symbols in FORM with new symbols.

Use OBARR as a source and cache.  The new symbols will be copys
of the old ones.  Ignore keywords and symbols in EXCLUDE-SYMBOLS,
which should be a list of symbols."
  (cond
   ((consp form)
    (mapcar (lambda (form)
              (make-closure form obarr
                            exclude-symbols)) form))
   ((and (symbolp form)
         (not (keywordp form))
         (not (memq form exclude-symbols)))
    (intern-soft-copy form obarr))
   (t form)))

(defmacro closure (&rest body)
  "Retain a copy of the (runtime-)bindings inside lambda-forms in BODY.

These are `(lambda ...)' and `(defun ...)', but not any kind of
quoted `lambda'.  Example :

    \(defun make-counter \(\)
      \(let \(\(value 0\)\)
        \(closure
          \(lambda nil
            \(incf value\)\)\)\)\)

    \(fset 'counter \(make-counter\)\)
    \(counter\) => 1
    \(counter\) => 2
    \(boundp 'value\) => nil

All `lambda's and `defun's in BODY share a common environment."

  (declare (debug (&rest form)))
  (with-make-symbols (obarr)
    `(let ((,obarr (make-vector 13 0)))
       (macrolet ((lambda (args &rest body)
                    (list 'cons ''lambda
                          (list 'cons (list 'quote args)
                                (list 'make-closure
                                      (list 'quote body)
                                      ',obarr
                                      (list 'quote args)))))
                  (defun (name args &rest body)
                    `(progn
                       (fset ',name (lambda ,args ,@body))
                       ',name)))
         ,@body))))

(defmacro clambda (args &rest body)
  "Like `lambda', but with `closure'."
  (declare (indent defun))
  `(closure
     (lambda ,args ,@body)))

(provide 'closure)

;;; closure.el ends here

;;; macro-utils.el --- Utilities for writing macros.

;; Copyright (C) 2005 Lars Brinkhoff.
;; This file is in the public domain.

(require 'cl)

(defmacro with-gensyms (symbols &rest body)
  "Execute BODY in a context where the variables in SYMBOLS are bound to
fresh gensyms."
  (assert (every #'symbolp symbols))
  `(let ,(mapcar* #'list symbols '#1=((gensym) . #1#))
    ,@body))

(defmacro once-only (symbols &rest body)
  "Execute BODY in a context where the values bound to the variables in
SYMBOLS are bound to fresh gensyms, and the variables in SYMBOLS are bound
to the corresponding gensym."
  (assert (every #'symbolp symbols))
  (let ((gensyms (mapcar (lambda (x) (gensym)) symbols)))
    `(with-gensyms ,gensyms
       (list 'let (mapcar* #'list (list ,@gensyms) (list ,@symbols))
        ,(list* 'let (mapcar* #'list symbols gensyms)
           body)))))

(put 'once-only 'lisp-indent-function 1)

;;; A small test.
(when nil
  (let* ((form0 '(once-only (x) (list 'foo x)))
         (form1 (macroexpand form0))
         (sym1 (caaadr form1))
         (form2 (eval `(let ((x '(bar))) ,form)))
         (sym2 (caaadr form2)))
    (assert (equal form1
                   `(let ((,sym1 (gensym)))
                     (list 'let (mapcar* #'list (list ,sym1) (list x))
                       (let ((x ,sym1))
                         (list 'foo x))))))
    (assert (equal form2
                   `(let ((,sym2 (bar)))
                     (foo ,sym2))))))

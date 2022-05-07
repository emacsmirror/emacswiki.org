;;; macro-utils.el --- Utilities for writing macros.

;; Copyright (C) 2022 Umut Tuna Akgül <thuna.cing@gmail.com>
;; Copyright (C) 2005 Lars Brinkhoff. (Original Author)
;; This file is in the public domain.

;; Note that Emacs 29+ has both macros built-in as `cl-with-gensyms',
;; and `cl-only-once'.

(require 'cl-lib)

(defmacro with-gensyms (symbols &rest body)
  "Execute BODY in a context where the variables in SYMBOLS are bound to
fresh gensyms."
  (declare (indent 1))
  (cl-assert (cl-every #'symbolp symbols))
  `(let ,(cl-mapcar #'list symbols '#1=((gensym) . #1#))
     ,@body))

(defmacro once-only (symbols &rest body)
  "Execute BODY in a context where the values bound to the variables in
SYMBOLS are bound to fresh gensyms, and the variables in SYMBOLS are bound
to the corresponding gensym."
  (declare (indent 1))
  (cl-assert (cl-every #'symbolp symbols))
  (let ((gensyms (mapcar #'gensym symbols)))
    `(with-gensyms ,gensyms
       (list 'let (cl-mapcar #'list (list ,@gensyms) (list ,@symbols))
             ,(cl-list* 'let (cl-mapcar #'list symbols gensyms)
			body)))))

;;; A small test.
(when nil
  (let* ((form0 '(once-only (x) (list 'foo x)))
         (form1 (macroexpand form0))
         (sym1 (caaadr form1))
         (form2 (eval `(let ((x '(bar))) ,form)))
         (sym2 (caaadr form2)))
    (cl-assert
     (equal form1
            `(let ((,sym1 (gensym)))
	       (list 'let (cl-mapcar #'list (list ,sym1) (list x))
		     (let ((x ,sym1))
		       (list 'foo x))))))
    (cl-assert
     (equal form2
            `(let ((,sym2 (bar)))
               (foo ,sym2))))))

(provide 'macro-utils)

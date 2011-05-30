;;; -*- lexical-binding: t -*-

;; Lisp streams
;;
;; Copyright (C) 2011 by Nic Ferrier


;; Helpers


(defun sweat--stream-from-list (func lst)
  "Make a stream from the lst.

For example:

 (let ((s (sweat--stream-from-list 
           (lambda (x) (list :name (car x) :value (cdr x)))
     	   '((\"name\" . \"nic\") (\"age\" . 30)))))
   (list (funcall s) (funcall s)))

Although this is included in sweat it's a lisp fundamental and
seems like it should be generally included somewhere. It requires
lexical scope though.
"
  (let ((list-to-stream lst))
    (lambda ()
      (if list-to-stream
          (let* ((retval (funcall func (car list-to-stream))))
            (setq list-to-stream (cdr-safe list-to-stream))
            retval)
        (throw 'stream-empty nil)
        )
      )
    )
  )

(defun sweat--assoc (template binding-list)
  "Top level worker function for sweat templates"
  (let ((tmpl template))
    (replace-regexp-in-string
     "::\\(.*?\\)::" 
     (lambda (r)
       ;; Needs improving - need error checking etc..
       (let ((a (assoc (intern (match-string 1 r)) binding-list)))
         (cond
          ((atom (cdr-safe a))
           (cdr-safe a))
          ((listp a)
           (cadr a)))))
     tmpl
     )
    )
  )



;; Interface functions/macros

(defmacro sweat-* (stream template)
  "Iterate over a stream defined in the bindings of sweat-let.

At the moment we return a single concat value of everything
generated. In the future I'd like to make an optional function to
handle the result (so, for example, the result can be sent by
elnode as an HTTP chunk)."
  `(let ((result '())) ;; need gensym here
     (catch 'stream-empty
       (while 't
         (let ((_ (funcall ,stream))) ;; and gensym here as well
           ;; setting the result... could be replaced by calling a function with the result
           ;; (funcall receiver (append result (list (sweat--assoc ,template _))))
           ;; where receiver is:
           ;;   (lambda (v)
           ;;     (setq result (concat result v)))
           (setq result (append result (list (sweat--assoc ,template _))))
           )
         )
       )
     (apply 'concat result) ;; 
     )
  )

(defmacro sweat-let (bindings &rest forms)
  `(let ,bindings
     (let ((_ (quote ,bindings)))
       (mapconcat 
        (lambda (form)
          (cond
           ((stringp form)
            (sweat--assoc form _))
           ((functionp form)
            (funcall form))
           ('t
            (message "whoops!")
            "")))
        (list ,@forms)
        "")
       )
     )
  )

;; Demo
(sweat-let ((title "nic's demo")
            (items (stream-from-list
                    (lambda (item)
                      `((name . ,(car item))
                        (value . ,(cdr item))))
                    '(("username" . "nicferrier")
                      ("firstname" . "nic")))))
           "<html><head><title>::title::</title><head><body><ul>"
           (sweat-* items "<li>::name:: - ::value::</li>")
           "</ul></body></html>")

;; End

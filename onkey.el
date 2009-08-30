;;; A complicated macro to simplify global key (un)binding.
;   sburke@cpan.org

(defmacro onkey (key &rest body)
 "A complicated macro to simplify global key (un)binding.

Uses:
(onkey [f5])  => unbinds [f5]

(onkey [f5] \'recenter)   => binds [f5] to that command

(onkey [f5]  (message \"Hi!\") (insert \"Hooboy.\"))
  =>  to bind f5 to a new lambda consisting of those actions

(onkey [f5] \"A greeting\" (message \"Hi!\") (insert \"Hooboy.\"))
  =>  to bind f5 to a new lambda consisting of
        those actions, with that given description
" ; ()'"   sburke@cpan.org

 (cond

  ;Input:  (onkey [f5])
  ;Output: (global-unset-key [f5])

  ;Input:  (onkey [f5] nil)
  ;Output: (global-unset-key [f5])
  ((or
    (zerop (length body)) ; no params at all
    (and (= 1 (length body)) (null (car body)))) ; one param: nil
  `(global-unset-key ,key))

  ;Input:  (onkey [f5] 'recenter)
  ;Output: (global-set-key [f5] 'recenter)
  ((and
     ; symbolp doesn't work nicely in macros, it seems.
    (= 1 (length body))
    (listp (car body))
    (string-equal (caar body) 'quote))
  `(global-set-key ,key ,(car body)))

  ; Common cases follow:

  ; Input:  (onkey [f5] "A greeting" (message "Hi!"))
  ; Output: (global-set-key [f5]
  ;           (lambda () "A greeting"
  ;             (interactive) (message "Hi!")))
  ((and
    (= 2 (length body))
    (stringp (car  body))
    (listp   (cadr body))
   )
  `(global-set-key ,key
       ;(byte-compile
	 (lambda () ,(car body) (interactive) ,(cadr body) )));)
          
  ; Input:  (onkey [f5] (message "Hi!"))
  ; Output: (global-set-key [f5]
  ;           (lambda () "<keyfunction>"  ;[no real description]
  ;             (interactive) (message "Hi there!")))
  ((listp (car body))
   `(global-set-key ,key
       ;(byte-compile
	 (lambda () "<keyfunction>" (interactive) ,(car body))));)

  ;Otherwise...
  (t (error "I don't understand this onkey parameter: %s" body))))

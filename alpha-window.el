;;; alpha-window.el set and control window's alpha value
;; transform window
;; Anchor: March Liu (刘鑫) <march.liu@gmail.com>
;; 
;; This is a script to set emacs window's alpha value.
;; It work well on windows xp and vista with EmacsWin32
;; useage: add below line in your .emacs
;;
;; (load-file "path/alpha-window.el")
;; 
;; you can define your alpha-list to set the transform combine
;; bind key with below code:
;;
;; (global-set-key [(f11)] 'loop-alpha)

(setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))

(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))                ;; head value will set to
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))
    )
)

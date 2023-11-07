;; -*- lexical-binding: t -*-

;;; english-mode.el --- speak english after type a word

(defun english-mode-map-fun (key)
  (lambda ()
    (interactive)
    (let ((word (word-at-point)))
          (if word
              (call-process "espeak-ng" nil 0 nil (word-at-point)))
          (insert-char key))))

(define-minor-mode english-mode
  "Toggles global english-mode."
  nil
  :global nil
  :group 'dotfiles
  :lighter " En"
  :keymap
  (list (cons (kbd "SPC") (english-mode-map-fun 32))
        (cons (kbd ")") (english-mode-map-fun 41))
        (cons (kbd ",") (english-mode-map-fun 44))
        (cons (kbd ".") (english-mode-map-fun 46))
        (cons (kbd "<return>") (english-mode-map-fun 10)))

  (if english-mode
      (message "english-mode activated!")
    (message "english-mode deactivated!")))

(add-hook 'english-mode-hook (lambda () (message "Hook was executed!")))
(add-hook 'english-mode-on-hook (lambda () (message "english turned on!")))
(add-hook 'english-mode-off-hook (lambda () (message "english turned off!")))
;;; english-mode.el ends here

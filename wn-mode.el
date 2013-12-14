;;; wn-model.el -- numeric window switching shortcuts
;;; Version: 0.1
;;; 

;;; Window number
(defun wn-window-list ()
  "Return a list of ordered windows on the current frame"
  (window-list (selected-frame) 0 (minibuffer-window)))

(defun wn-select-nth (n)
  "Select window number N in current frame"
  (interactive "P")
  (let (window)
    (or (and (or (integerp n)
                 (setq n (read-number "Window number: ")))
             (setq window (nth n (wn-window-list)))
             (select-window window))
        (error "No such window."))))

(defun wn-selected-window-number ()
  "Return the number of the selected window"
  (1- (length (memq (selected-window) (reverse (wn-window-list))))))

(defun wn-selected-window-modeline ()
  "Return the string for the current window modeline"
  (propertize (format " #%s" (wn-selected-window-number))
              'face 'wn-modeline-face))

(defvar wn-mode-map
  (if (boundp 'wn-mode-map)
      wn-mode-map
    (let ((map (make-sparse-keymap)))
      (mapc (lambda (keydef) (define-key map (car keydef) (cadr keydef)))
            '(("\C-c0" (lambda nil (interactive) (wn-select-nth 0)))
              ("\C-c1" (lambda nil (interactive) (wn-select-nth 1)))
              ("\C-c2" (lambda nil (interactive) (wn-select-nth 2)))
              ("\C-c3" (lambda nil (interactive) (wn-select-nth 3)))
              ("\C-c4" (lambda nil (interactive) (wn-select-nth 4)))
              ("\C-c5" (lambda nil (interactive) (wn-select-nth 5)))
              ("\C-c6" (lambda nil (interactive) (wn-select-nth 6)))
              ("\C-c7" (lambda nil (interactive) (wn-select-nth 7)))
              ("\C-c8" (lambda nil (interactive) (wn-select-nth 8)))
              ("\C-c9" (lambda nil (interactive) (wn-select-nth 9)))
              ("\C-c#" wn-select-nth)))
      map))
  "Wn-mode map")

(define-minor-mode wn-mode
  "A minor mode that enables selection of windows according to
       numbers with the C-c number"
  :group 'windows
  :global t
  :init-value nil
  :lighter ( :eval (wn-selected-window-modeline)))

(defface wn-modeline-face
  '((t ( :inherit mode-line)))
  "wn-mode modeline face")

(provide 'wn-mode)

;;; wn-mode.el ends here

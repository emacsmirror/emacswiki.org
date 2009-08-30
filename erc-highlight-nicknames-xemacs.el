;;;
(defun erc-highlight-nicknames ()
  "Searches for nicknames and highlights them. Uses the first
twelve digits of the MD5 message digest of the nickname as
color (#rrrrggggbbbb)."
  (with-syntax-table erc-button-syntax-table
    (let (bounds word)
      (goto-char (point-min))
      (while (forward-word 1)
        (setq bounds (bounds-of-thing-at-point 'word))
        (setq word (buffer-substring-no-properties
                    (car bounds) (cdr bounds)))
        (when (erc-get-server-user word)
          (setq color (concat "#" (substring (md5 (downcase word)) 0 12)))
          (if (equal (cdr (assoc 'background-mode (frame-parameters)))
                     'dark)
              ;; if too dark for background
              (when (< (hexcolor-luminance color) 85)
                (setq color (invert-color color)))
            ;; if to bright for background
            (when (> (hexcolor-luminance color) 170)
              (setq color (invert-color color))))
	  (setq foo (make-symbol (concat "erc-highlight-" word "-face")))
	  (make-face foo)
	  (make-face-bold foo)
	  (set-face-foreground foo color)
	  (put-text-property
	   (car bounds) (cdr bounds)
	   'face foo))))))

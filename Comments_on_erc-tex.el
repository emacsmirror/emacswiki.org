The code has an error:
{{{
Debugger entered--Lisp error: (args-out-of-range #<buffer #emacs> 0 27)
  delete-region(0 27)
  (let (start end) (delete-region (match-beginning 0) (match-end 0)) (setq start (point)) (insert-image descp match) (setq end (point)) (put-text-property start end 'keymap erc-tex-image-keymap))
  (progn (let (start end) (delete-region (match-beginning 0) (match-end 0)) (setq start (point)) (insert-image descp match) (setq end (point)) (put-text-property start end 'keymap erc-tex-image-keymap)))
  (if descp (progn (let (start end) (delete-region (match-beginning 0) (match-end 0)) (setq start (point)) (insert-image descp match) (setq end (point)) (put-text-property start end 'keymap erc-tex-image-keymap))))
  (let* ((match (match-string-no-properties 0)) (descp (erc-tex-make-image match fg bg))) (if descp (progn (let (start end) (delete-region (match-beginning 0) (match-end 0)) (setq start (point)) (insert-image descp match) (setq end (point)) (put-text-property start end 'keymap erc-tex-image-keymap)))))
  (while (re-search-forward "\\$[^$]*\\$" nil t) (let* ((match (match-string-no-properties 0)) (descp (erc-tex-make-image match fg bg))) (if descp (progn (let (start end) (delete-region (match-beginning 0) (match-end 0)) (setq start (point)) (insert-image descp match) (setq end (point)) (put-text-property start end 'keymap erc-tex-image-keymap))))))
  (let ((fg (or fg (face-foreground 'default))) (bg (or bg (face-background 'default)))) (goto-char (point-min)) (while (re-search-forward "\\$[^$]*\\$" nil t) (let* ((match (match-string-no-properties 0)) (descp (erc-tex-make-image match fg bg))) (if descp (progn (let (start end) (delete-region (match-beginning 0) (match-end 0)) (setq start (point)) (insert-image descp match) (setq end (point)) (put-text-property start end 'keymap erc-tex-image-keymap)))))))
  erc-tex-render("brown" nil)
  erc-tex-render-send()
  run-hooks(erc-send-modify-hook)
  erc-display-msg("Test ERC TeX fontify: $y = sin(x)$ ")
}}}

-- [https://github.io/stardiviner stardiviner] 2017-06-28 15:45 UTC


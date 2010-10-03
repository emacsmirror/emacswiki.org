;;; customize of elisp-mode

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(defun my-emacs-lisp-mode-hook()
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?+ "w")
  (local-set-key [(tab)] 'indent-or-complete)
  (local-set-key [(home)] 'back-to-indentation-or-beginning-of-line)
  (local-set-key [(shift home)] 'back-to-indentation-or-beginning-of-line-with-shift)

  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (setq require-final-newline t)

  (local-set-key [(return)]    'newline-and-indent)

  (local-set-key [(f3)]  'wuxch-imenu-tree-open)
  (local-set-key [(meta f3)]  'wuxch-imenu-tree-kill)

  ;; (local-set-key [double-mouse-1] 'wuxch-elisp-hs-toggle-hiding)

  (if (not-linux-terminal)
      (progn
        ;; 存盘的时候检查括号是否匹配
        (add-hook 'local-write-file-hooks 'check-parens)
        (add-hook 'local-write-file-hooks 'delete-trailing-whitespace)
        (turn-on-show-trailing-whitespace)

        (local-set-key [(f9)] 'edebug-defun)
        (local-set-key [(control f9)] 'eval-defun)
        (local-set-key [(f7)] 'wuxch-elisp-build-tag)
        (local-set-key [(control f7)] 'add-elisp-file-name-to-makefile)


        (which-func-mode 1)
        (abbrev-mode 1)
        ;; 打开hs-minor-mode
        (hs-minor-mode 1)
        (local-set-key [(control meta left)] 'hs-hide-all)
        (local-set-key [(control meta right)] 'hs-show-all)
        (local-set-key [(control x)(left)] 'hs-hide-block)
        (local-set-key [(control x)(right)] 'hs-show-block)

        )
    )

;;;   (local-set-key [(f3)] 'list-func-mode-toggle)
;;;   (local-set-key [(control f3)] 'list-func-adjust-window)
;;;   (local-set-key [(control x)(k)] 'list-func-kill-source-buffer-and-delete-corresponding)

  (local-set-key [(f5)] 'run-current-funtion)
  (local-set-key [(control f5)] 'copy-current-funtion-name-to-clipboard)

  ;;  (paredit-mode +1)
  )

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)


(defun copy-current-funtion-name-to-clipboard()
  "copy-current-funtion-name-to-clipboard:"
  (interactive)
  (let ((function-name (which-function)))
    (kill-new function-name)
    (message "copy \"%s\" to clipboard" function-name)
    )
  )

(defun run-current-funtion()
  "run-current-funtion:"
  (interactive)
  (let ((function-name (which-function)))
    (funcall (symbol-function (intern function-name)))
    )
  )

(require 'wuxch-elisp-tag)

(provide 'wuxch-elisp)

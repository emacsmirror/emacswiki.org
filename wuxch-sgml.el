;;; configures and codes for sgml mode.
(require 'sgml-mode)

(defun my-sgml-style()

  (add-hook 'local-write-file-hooks 'delete-trailing-whitespace)
  ;; (add-hook 'local-write-file-hooks 'indent-dwim)
  (turn-on-show-trailing-whitespace)

  ;;输入左边的括号，就会自动补全右边的部分.包括(), "", [] , {} , 等等。
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist  '(
                               (?\(  _ ")")
                               (?\[  _ "]")
                               (?{  _ "}")
                               (?\" _ "\"")
                               (?\'  _ "'")
                               ))

  (setq skeleton-pair t)

  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)

  (setq require-final-newline t)

  (local-set-key [(tab)] 'indent-or-complete)
  (local-set-key [(home)] 'back-to-indentation-or-beginning-of-line)
  (local-set-key [(shift home)] 'back-to-indentation-or-beginning-of-line-with-shift)

  (local-set-key [(f7)] 'compile)

  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w")

  (setq sgml-markup-faces '(
                            (start-tag . font-lock-keyword-face)
                            (end-tag . font-lock-keyword-face)
                            (comment . font-lock-comment-face)
                            (pi . font-lock-constant-face) ;; <?xml?>
                            (sgml . font-lock-type-face)
                            (doctype . bold)
                            (entity . italic)
                            (shortref . font-lock-reference-face)))

  )

(defun my-sgml-mode-hook ()
  ""
  (my-sgml-style)
  )

(add-hook 'sgml-mode-hook 'my-sgml-mode-hook)

(provide 'wuxch-sgml)

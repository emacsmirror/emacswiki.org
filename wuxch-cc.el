;;; cc-mode customize

(require 'hideshow)

(setq tab-always-indent nil)

(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    (indent-for-tab-command)
    ))

(defun my-c-java-style()
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist  '(
                               (?\(  _ ")")
                               (?\[  _ "]")
                               (?\" _ "\"")
                               (?\'  _ "'")
                               (?{ \n > _ \n ?} >)))

  (setq skeleton-pair t)
  ;; (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

  ;; (c-toggle-auto-newline 1)
  (c-toggle-hungry-state 1)

  (setq require-final-newline t)

  (local-set-key [(tab)] 'indent-or-complete)
  (local-set-key [(home)] 'back-to-indentation-or-beginning-of-line)
  (local-set-key [(shift home)] 'back-to-indentation-or-beginning-of-line-with-shift)

  (hs-minor-mode 1)
  (local-set-key [(control meta left)] 'hs-hide-all)
  (local-set-key [(control meta right)] 'hs-show-all)
  (local-set-key [(control x)(left)] 'hs-hide-block)
  (local-set-key [(control x)(right)] 'hs-show-block)

  ;; (local-set-key [double-mouse-1] 'wuxch-c-java-hs-toggle-hiding)

  ;; (local-set-key [(control =)] 'wuxch-count-lines)
  (local-set-key [(return)]    'newline-and-indent)

  (local-set-key [(control c)(control c)] 'ignore)
  (local-set-key [(control c)(control c)] 'wuxch-insert-windows-config-to-register)

;;;   (local-set-key [(f3)] 'list-func-mode-toggle)
;;;   (local-set-key [(control f3)] 'list-func-adjust-window)
;;;   (local-set-key [(control x)(k)] 'list-func-kill-source-buffer-and-delete-corresponding)

  (local-set-key [(control c)(\,)] 'ignore)
  (local-set-key [(control c)(\.)] 'ignore)
  (local-set-key [(control c)(\,)] 'wuxch-point-stack-push)
  (local-set-key [(control c)(\.)] 'wuxch-point-stack-pop)
  ;; (which-func-mode 1)
  )

;; 基本的设置方法是：
;; C-c C-s看一下当前是什么语句，与哪里对齐(高亮部分)
;; 可以通过C-c C-o设定这个预计的对齐方式
;; 可以通过s-set-offset在代码中写上。
;; 偏移可以是`+', `-', `++', `--', `*', or `/'
;; 分别对应:
    ;; `+'          `c-basic-offset' times 1
    ;; `-'          `c-basic-offset' times -1
    ;; `++'          `c-basic-offset' times 2
    ;; `--'          `c-basic-offset' times -2
    ;; `*'          `c-basic-offset' times 0.5
    ;; `/'          `c-basic-offset' times -0.5
;; 还可以是数字，如0
;; 可以检查变量c-offsets-alist

(defun c-set-my-style ()
  "c-set-my-style:"
  (interactive)
  (c-set-style "java")
  (c-set-offset 'friend '-)
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open '0)
  )

(defun my-c-mode-hook()
  ;; (setq tab-width 4 indent-tabs-mode nil)
  (c-set-my-style)

  ;;   (define-key c-mode-map [(control m)] 'newline-and-indent)
  ;;   (define-key c++-mode-map [(control m)] 'newline-and-indent)
  ;; set "_" is part if word
  (modify-syntax-entry ?_ "w")

  ;;   (imenu-add-menubar-index)
  (setq c-tab-always-indent nil)
  (my-c-java-style)
  ;; list-func
  (require 'wuxch-list-func)
  (local-set-key [(f4)] 'ignore)
  (local-set-key [(f4)] 'list-func-mode-toggle)
  (local-set-key [(control f4)] 'list-func-adjust-window)
  (local-set-key [(f7)] 'compile)
  ;; (list-func-mode-update)
  ;; (if (not buffer-read-only)
  ;;       (progn
  ;;         (turn-on-show-trailing-whitespace)
  ;;         )
  ;;     )
  ;; (which-func-mode 1)
  )

(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)


(defun my-makefile-mode-hook()
  (modify-syntax-entry ?_ "w")
  )
(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)


(global-set-key [(control x)(\;)] 'ignore)
(global-set-key [(control x)(\;)] 'comment-current-line-or-region)
(global-set-key [(meta \;)] 'ignore)
(global-set-key [(meta \;)] 'comment-current-line-or-region)

(defun comment-current-line-or-region(arg)
  "Comment current line. If current line is emply line, add
comment, if current line is commentted, uncomment it"
  (interactive "*P")
  ;; mark current line if there's not any acitve mark.
  (if (not mark-active)
      (let ((begin-pos)(end-pos))
        (back-to-indentation)
        (setq begin-pos (point))
        (cua-set-mark)
        (move-end-of-line arg)
        (setq end-pos (point))
        (if (eq begin-pos end-pos)
            (deactivate-mark))
        )
    )
  (comment-dwim arg)
  )


(provide 'wuxch-cc)

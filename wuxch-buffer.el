;;; config about ibuffer manage.

;; 保证文件名相同的时候buffer名称是目录名+文件名
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; enhanced change buffer
(iswitchb-mode t)
(defun wuxch-iswitch-mode-map-hook ()
  "添加此操作，这样iswitch的时候可以通过箭头选择上一个或者下一个匹配的buffer"
  (let (map)
    (setq map (copy-keymap minibuffer-local-map))
    (define-key map [(right)] 'iswitchb-next-match)
    (define-key map [(down)] 'iswitchb-next-match)
    (define-key map [(left)] 'iswitchb-prev-match)
    (define-key map [(up)] 'iswitchb-prev-match)
    (setq iswitchb-mode-map map))
  )

(add-hook 'iswitchb-define-mode-map-hook 'wuxch-iswitch-mode-map-hook)


;; control x k由缺省的kill-buffer改为kill-this-buffer，不需要确认，需要确认的命令是kill-buffer
(global-set-key [(control x)(k)] 'ignore)
(global-set-key [(control x)(k)] 'kill-this-buffer-and-delete-other-windows)
(defun kill-this-buffer-and-delete-other-windows ()
  (interactive)
  (kill-this-buffer)
  ;; 暂时把关闭本窗口的操作注释
;;   (delete-other-windows)
  )

;; 新建一个buffer的快捷方式
(defconst wuxch-new-buffer-name "new")
(defvar wuxch-new-buffer-sequence 0)
(defun wuxch-create-new-buffer ()
  "Create new buffer, default name is new[0-9]"
  (interactive)
  (let ((w))
    (setq w (concat wuxch-new-buffer-name
                    (number-to-string
                     (setq wuxch-new-buffer-sequence
                           (1+ wuxch-new-buffer-sequence)))))
    (get-buffer-create w)
    (switch-to-buffer w)
    (text-mode)
    ;; (clipboard-yank)
    )
  )

(global-set-key [(control x)(f)] 'ignore)
(global-set-key [(control x)(f)] 'wuxch-create-new-buffer)


;; meta f4关闭emacs，不提示确认信息。需要确认的命令是kill-buffer
(global-set-key [(meta f4)] 'save-buffers-kill-emacs)


;; 启动ibuffer
(require 'ibuffer)
;; (global-set-key [(control x)(control b)] 'ibuffer)

(defun wuxch-pop-ibuffer-one-window ()
  ""
  (interactive)
  (ibuffer)
  (let ((buf "*Ibuffer*"))
    (switch-to-buffer buf)
    )
  (delete-other-windows)
  )


(global-set-key [(f12)] 'ignore)
(global-set-key [(f12)] 'wuxch-pop-ibuffer-one-window)

(global-set-key [(control c)(f12)]  'ibuffer)

;; ibuffer操作不需要确认
(setq ibuffer-expert t)

(require 'wuxch-bookmark)
(defun wuxch-ibuffer-mode-hook ()
  ""
  (setq ibuffer-sorting-mode major-mode)
  (setq ibuffer-show-empty-filter-groups nil)
  (define-key ibuffer-mode-map [tab] 'ibuffer-forward-line)
  (define-key ibuffer-mode-map [delete] 'ignore)
  (define-key ibuffer-mode-map [delete] 'wuxch-ibuffer-do-delete)
  (define-key ibuffer-mode-map [(control up)] 'ibuffer-backward-filter-group)
  (define-key ibuffer-mode-map [(control down)] 'ibuffer-forward-filter-group)
  (define-key ibuffer-mode-map (kbd "* n") 'ibuffer-mark-by-name-regexp)
  (define-key ibuffer-mode-map (kbd "* m") 'ibuffer-mark-by-mode)
  (define-key ibuffer-mode-map (kbd "* M") 'ibuffer-mark-modified-buffers)

  ;; 用U作为unmark all的快捷键，和dired一样。
  (define-key ibuffer-mode-map (kbd "U") 'ignore)
  (define-key ibuffer-mode-map (kbd "U") 'wuxch-ibuffer-unmark-all)

  (define-key ibuffer-mode-map (kbd "=") 'ignore)
  (define-key ibuffer-mode-map (kbd "=") 'wuxch-ibuffer-compair-marked-buffer)

  (define-key ctl-x-4-map "i" 'wuxch-ibuffer-other-window)

  (define-key ibuffer-mode-map (kbd "g") 'ignore)
  (define-key ibuffer-mode-map (kbd "g") 'wuxch-ibuffer-update)

  (define-key ibuffer-mode-map (kbd "l") 'ignore)
  (define-key ibuffer-mode-map (kbd "l") 'wuxch-bookmark-bmenu-list)


  ;;   (ibuffer-do-sort-by-major-mode)
  ;; 每个buffer有变量major-mode标识当前的mode
  (setq ibuffer-filter-groups
        '(
          ("dired" (mode . dired-mode))
          ("org" (or (mode . org-agenda-mode)
                     (mode . org-mode)
                     ))
          ("elisp" (mode . emacs-lisp-mode))
          ("c & java" (or (mode . cc-mode)
                          (mode . c++-mode)
                          (mode . c-mode)
                          (mode . java-mode)
                          ))
          ("txt & outline & tex & wiki" (or (mode . text-mode)
                                            (mode . outline-mode)
                                            (mode . diary-mode)
                                            (mode . any-ini-mode)
                                            ))
          ("tex & wiki & metapost" (or (mode . latex-mode)
                                       (mode . doctex-mode)
                                       (mode . emacs-wiki-mode)
                                       (mode . metapost-mode)
                                       ))
          ("xml" (or (mode . sgml-mode)
                     (mode . xml-mode)
                     (mode . sgml-xml-mode)
                     ))
          ("compilation" (or (mode . compilation-mode)
                             (mode . makefile-mode)
                             ))
          ("bat" (mode . bat-mode))
          ("*buffer*" (or (name . "\\*.*\\*")
                          (mode . lisp-interaction-mode)
                          ))
          ("image" (mode . image-mode))
          )
        )

  (setq ibuffer-fontification-alist
        (append ibuffer-fontification-alist
                (list (list 45 '(eq major-mode (quote emacs-lisp-mode)) font-lock-doc-face))))
  (setq ibuffer-fontification-alist
        (append ibuffer-fontification-alist
                (list (list 45 '(eq major-mode (quote org-mode)) font-lock-constant-face))))

  )

(add-hook 'ibuffer-mode-hooks 'wuxch-ibuffer-mode-hook)

(defun wuxch-ibuffer-do-delete ()
  "原有的ibuffer-do-delete关闭一个buffer之后跳到上一个buffer，不习惯，改为到下一个buffer。"
  (interactive)
  (ibuffer-do-delete)
  (ibuffer-forward-line))


;; 启动ibs，使用control tab可以使用数字快速选择
;; (require 'ebs)
;; (ebs-initialize)
;; (global-set-key [(control tab)] 'ebs-switch-buffer)


;; switch next-buffer and previous-buffer key defination
;; (global-set-key [(control x)(left)] 'ignore)
;; (global-set-key [(control x)(left)] 'next-buffer)
;; (global-set-key [(control tab)] 'ignore)
;; (global-set-key [(control tab)] 'iswitchb-buffer)
;; (global-set-key [(control x)(right)] 'ignore)
;; (global-set-key [(control x)(right)] 'previous-buffer)

;; 设置buf之间切换的时候显示buf内部的文件。在swbuff.el中修改了buf跳转的定义如下：
;; 设置control PgUp和control PgDn在buffer中跳转
;; (global-set-key [(control next)] 'previous-buffer)
;; (global-set-key [(control prior)] 'next-buffer)
;;(require 'swbuff)

;; 添加有关frame的操作，添加control -和control +快捷键
;; (require 'frame-cmds)
;; (require 'zoom-frm)
;; (define-key global-map [(control =)] 'zoom-frm-in)
;; (define-key global-map [(control -)] 'zoom-frm-out)


;; 替换原有的unmark-all，原有的需要输入return
(defun wuxch-ibuffer-unmark-all ()
  "Unmark all buffers with mark MARK."
  (interactive)
  (if (= (ibuffer-count-marked-lines t) 0)
      (message "No buffers marked; use 'm' to mark a buffer")
    (ibuffer-map-lines
       #'(lambda (buf mark)
	   (when (not (char-equal mark ?\s))
	     (ibuffer-set-mark-1 ?\s))
	   t)))
  (ibuffer-redisplay t))

(defun wuxch-ibuffer-other-window ()
  ""
  (interactive)
  (if (one-window-p)
      (split-window-horizontally))
  (ibuffer-other-window)
  )


(defun wuxch-ibuffer-update ()
  ""
  (interactive)
  (wuxch-mark-useless-buffer)
  (ibuffer-update nil)
  )

(defun wuxch-ibuffer-compair-marked-buffer ()
  (interactive)
  (let* ((buf (ibuffer-get-marked-buffers))(buf-num (safe-length buf)))
    (cond
     ;; <2 buffers
     ((< buf-num 2)
      (message "please marked 2 or 3 buffers to compare" )
      )
     ;; 2 buffers
     ((eq buf-num 2)
      (ediff-buffers (nth 0 buf) (nth 1 buf))
      )
     ;; 3 buffers
     ((eq buf-num 3)
      (ediff-buffers3 (nth 0 buf) (nth 1 buf) (nth 2 buf))
      )
     ;; others
     (t
      (message "too many marked buffers. only 2 or 3 marked buffers should be compared" ))
     )
    )
  )


(defvar wuxch-useless-buffer-regexp nil)

(defun build-useless-buffer-regexp ()
  "build-useless-buffer-regexp:"
  (interactive)
  (let ((temp_buffer_name)(star-regexp "\\*"))
    (setq temp_buffer_name "\\(")
    ;; wrap with star
    (setq temp_buffer_name (concat temp_buffer_name star-regexp "info" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Help" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Tex Help" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "grep" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "igrep" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Occur" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Moccur" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Bookmark List" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Kill Ring" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "w32-find-dired" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Calendar" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Async Shell Command" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "org-to-html" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "compilation" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Compile-Log" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "shell" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Backtrace" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Colors" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Dired log" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Calculator" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Calc Trail" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Ediff .*" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "ediff-diff" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "ediff-errors" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "ediff-fine-diff" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp ".* output" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Buffer List" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Org Agenda" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "etags-select" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Color Theme Selection" star-regexp))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" star-regexp "Faces" star-regexp))
    ;; not wrap with star
    (setq temp_buffer_name (concat temp_buffer_name "\\|" ".*\\.log"))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" "phone\\.xml"))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" "^diary$"))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" "^TAGS$"))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" "new[0-9]+"))
    (setq temp_buffer_name (concat temp_buffer_name "\\|" ".*\\*ftp.*"))

    (setq temp_buffer_name (concat temp_buffer_name "\\)"))

    (setq wuxch-useless-buffer-regexp temp_buffer_name)
    )
  )

(defun wuxch-mark-useless-buffer ()
  ;; singelon pattern.  :)
  (if (eq wuxch-useless-buffer-regexp nil)
      (build-useless-buffer-regexp))

  (ibuffer-mark-on-buffer
   #'(lambda (buf)
       (string-match wuxch-useless-buffer-regexp
                     (buffer-name buf))))


  )

(defalias 'readonly 'toggle-read-only)

(provide 'wuxch-buffer)

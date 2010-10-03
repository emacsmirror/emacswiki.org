;;; configure of CUA key definition, and some function about edit.
(require 'extraedit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-enable-cua-keys t)
 '(cua-highlight-region-shift-only nil)
 '(cua-mode t nil (cua-base))
 '(cvs-dired-use-hook nil)
 '(font-lock-maximum-size 256000000)
 '(ibuffer-default-sorting-mode (quote major-mode))
 '(vc-cvs-use-edit nil)
 )

;; control-s: save-buffer
(global-set-key [(control s)] 'save-buffer)

;; meta-s: save-as
(global-set-key [(meta s)]    'write-file)

;; ignoring control-shift and control-space to avoid confict with IME input keymap.
(global-set-key [(control shift)] 'ignore)
(global-set-key [(control space)] 'ignore)

;; control a: mark-all
(global-set-key [(control a)] 'mark-whole-buffer)


(defun copy-current-word ()
  "put the current word where cursor is into killing-ring, even the cursor is not at the beginning of word"
  (interactive)
  (let ((w (bounds-of-thing-at-point 'word)))
    (kill-ring-save (car w)(cdr w))
    (message (concat "word:[" (buffer-substring (car w)(cdr w)) "] copied"))))

;; (global-set-key [(control w)] 'ignore)
;; (global-set-key [(control w)] 'copy-current-word)
;; 缺省的control w操作是kill region，现改为mark-word，可以重复使用，和isearch里的一致
(global-set-key [(control w)] 'ignore)
(global-set-key [(control w)] 'mark-word)

(if (equal 'windows-nt system-type)
    (progn
      ;; control x m: maxmize windows
      (global-set-key [(control x)(m)] 'ignore)
      (global-set-key [(control x)(m)]
                      (lambda ()
                        (interactive)
                        (w32-restore-frame)
                        (w32-maximize-frame)
                        (wuxch-set-default-theme)
                        ))))


;; meta h :Set mark after end of following balanced expression (mark-sexp),原为C-M-@
(global-set-key [(meta h)] 'ignore)
(global-set-key [(meta h)] 'mark-sexp)

;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
(setq suggest-key-bindings 1)

;; context-free tabstops in text mode
(define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop)

;; 调整行间距，0为缺省行间距
(defun line-space (n)
  "adjust line-space"
  (interactive "nEnter line-spacing(0,3,5,...):")
  (setq-default line-spacing n)
  )

;; gnuserv进程的名称是server，设定关闭emacs时直接关闭gnuserv，不用询问。
;; 显示所有进程的命令是：meta x list-processes
(if (not-linux)
    (set-process-query-on-exit-flag (get-process "server") nil)
  )


;; winner-mode，可以使用control-c + 左右箭头恢复上一次的windows操作
;; (winner-mode)

;; 高亮显示当前行 meta-x hl-line-mode

;; word-wrap
(require 'word-wrap)

;; ;; comment short key
;; (global-set-key [(control c)(c)] 'comment-region)
;; (global-set-key [(control c)(u)] 'uncomment-region)

;; 关于tab
;; 缺省模式，emacs输入tab键产生的操作是indent。如果确实希望输入tab，方法是：ctrl-q ctrl-tab
;; 不用 TAB 字符来indent, 这会引起很多奇怪的错误。编辑 Makefile 的时候也不用担心，
;; 因为 makefile-mode 会把 TAB 键设置成真正的 TAB 字符，并且加亮显示的
(setq-default indent-tabs-mode nil)
;; tab键宽度
(setq default-tab-width 4)

;; 可以直接修改或者删除选择内容
(delete-selection-mode t)

;; _是word的一部分
(setq dabbrev-abbrev-char-regexp "\\s_")

;; (modify-syntax-entry ?\$ "." text-mode-syntax-table)
;; (modify-syntax-entry ?\t "." text-mode-syntax-table)

;; 防止不小心按到菜单中的 print 时，emacs 死掉
(fset 'print-buffer 'ignore)
(setq lpr-command "")
(setq printer-name "")

(global-set-key [(meta delete)] 'delete-horizontal-space)

(global-set-key [(control w)]  'ignore)
(global-set-key [(control w)]  'wuxch-mark-word)

(defun wuxch-mark-word (&optional arg allow-extend)
  "goto the beginning of the word and mark it"
  (interactive "P\np")
  (if  (and (not mark-active) (not (eq (point) (point-min))))
      (if (wuxch-char-before-is-alnum)
          (backward-word))
    )
  ;; the following codes are copied from orginal mark-word function.
  (cond ((and allow-extend (or (and (eq last-command this-command) (mark t))
                               (and transient-mark-mode mark-active)))
         (setq arg (if arg (prefix-numeric-value arg)
                     (if (< (mark) (point)) -1 1)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (forward-word arg)
            (point))))
        (t
         (push-mark
          (save-excursion
            (forward-word (prefix-numeric-value arg))
            (point))
          nil t)))
  )

(defun wuxch-char-before-is-alnum ()
  (if (string-match "[[:alnum:]]" (char-to-string (char-before (point))))
      t
    nil)
  )

;; 自带的delete-blank-line实在不好用，写一个简单的，直接删除所有的空行
;; 因为有一个remove-duplicate-lines，所以直接叫remove-blank-lines
(defun remove-blank-lines ()
  ""
  (interactive "*")
  (goto-char (point-min))
  (let ((count 0)(buffername (buffer-name)))
    (while (re-search-forward "^[ \t]*\n" nil t)
      (progn
        (replace-match "")
        (setq count (+ count 1))
        )
      )
    (if (> count 0)
        (progn
          (message "remove %d line(s) in buffer %s" count buffername)
          (goto-char (point-min))
          )
      (message "no blank line in buffer %s" buffername)
      )
    )
  )
;; 把simple.el里面的 delete-blank-lines 屏蔽掉
(defalias 'delete-blank-lines 'remove-blank-lines)

(defun remove-useless-lines ()
  "useless lines means blank lines and duplicate lines"
  (interactive "*")
  (remove-duplicate-lines)
  (remove-blank-lines)
  )

(defun move-to-end-of-word ()
  "move-to-end-of-word:"
  (if (looking-at "[[:alnum:]]")
      (forward-word)
    )
  )

(global-set-key [(control backspace)] 'ignore)
(global-set-key [(control backspace)] 'backward-kill-word-or-delete-horizontal-space)
(defun backward-kill-word-or-delete-horizontal-space (arg)
  ""
  (interactive "p")
  (if (or (string= (char-to-string (char-before)) " " )
          (string= (char-to-string (char-before)) "\n" )
          (string= (char-to-string (char-before)) "\t" ))
      (delete-space)
    (backward-kill-word arg))
  )

(defun delete-space (&optional backward-only)
  "Delete all spaces and tabs around point,including \n
If BACKWARD-ONLY is non-nil, only delete them before point."
  (interactive "*P")
  (let ((orig-pos (point)))
    (delete-region
     (if backward-only
         orig-pos
       (progn
         (skip-chars-forward " \t\n")
         (constrain-to-field nil orig-pos t)))
     (progn
       (skip-chars-backward " \t\n")
       (constrain-to-field nil orig-pos)))))

(defun copy-word (&optional arg)
  "Copy words at point"
  (interactive "P")
  (let ((beg (progn (if (looking-back "[a-zA-Z0-9]" 1) (backward-word 1)) (point)))
      	(end (progn (forward-word arg) (point))))
    (copy-region-as-kill beg end))
  (message "copying current word to killing-ring")
  )

(global-set-key [(control c)(w)]   'copy-word)

(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (let ((beg (line-beginning-position))
      	(end (line-end-position)))
    (copy-region-as-kill beg end))
  (message "copying current line to killing-ring")
  )

;; (global-set-key [(control c)(l)]   'copy-line)

(defun copy-paragraph (&optional arg)
  "Copy paragraphes at point"
  (interactive "P")
  (let ((beg (progn (backward-paragraph 1) (point)))
      	(end (progn (forward-paragraph arg) (point))))
    (copy-region-as-kill beg end))
  (message "copying current paragraph to killing-ring")
  )

;; (global-set-key [(control c)(p)]   'copy-paragraph)
(global-set-key [(meta n)]  'pager-page-down)
(global-set-key [(meta p)]  'pager-page-up)

;; some key define for linux terminal
(if (is-linux-terminal)
    (progn
      (global-set-key [find]                'beginning-of-line)
      (global-set-key [select]              'end-of-line)
      (global-set-key [(control x)(find)] 	'beginning-of-buffer)
      (global-set-key [(control x)(select)] 'end-of-buffer)
      (global-set-key [(control c)(find)] 	'beginning-of-buffer)
      (global-set-key [(control c)(select)] 'end-of-buffer)
      )
  )

(global-set-key [(control /)]   'ignore)
(global-set-key [(control /)]   'repeat)

(provide 'wuxch-cua)

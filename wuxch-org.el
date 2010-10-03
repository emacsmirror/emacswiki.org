;;; config for org-mode
(add-to-list 'load-path (concat emacs-site-lisp-dir "org/lisp"))

;; info-path
(add-to-list 'Info-default-directory-list (concat emacs-site-lisp-dir "org/doc"))

(require 'org-install)
(setq org-log-done '(logdone))
(setq org-return-follows-link t)
(setq org-mouse-1-follows-link nil)
(setq org-table-default-size "3x2")
(defun wuxch-org-mode-hook ()
  ""

  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist  '(
                               (?\(  _ ")")
                               (?\[  _ "]")
                               (?\" _ "\"")
                               (?\'  _ "'")
                               (?{  _ ?} )))

  (setq skeleton-pair t)
  ;; (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

  (turn-on-font-lock) ;; org-mode buffers only
  ;; (flyspell-mode)
  ;; (setq org-CUA-compatible t)
  ;; S-RET → C-S-RET
  ;; S-up → M-p
  ;; S-down → M-n
  ;; S-left → M--
  ;; S-right → M-+
  ;; (add-to-list 'session-globals-exclude 'org-mark-ring)
  (define-key org-mode-map [(shift left)]   'ignore)
  (define-key org-mode-map [(shift left)]   'backward-char)
  (define-key org-mode-map [(shift right)]  'ignore)
  (define-key org-mode-map [(shift right)]  'forward-char)
  (define-key org-mode-map [(shift up)]     'ignore)
  (define-key org-mode-map [(shift up)]     'previous-line)
  (define-key org-mode-map [(shift down)]   'ignore)
  (define-key org-mode-map [(shift down)]   'next-line)
  (define-key org-mode-map [(control left)] 'ignore)
  (define-key org-mode-map [(control left)] 'backward-word)
  (define-key org-mode-map [(control right)]  'ignore)
  (define-key org-mode-map [(control right)]  'forward-word)
  ;; meta left 和 meta right保留用于改变标题级别
  (define-key org-mode-map [(control c)(control e)]  'ignore)
  (define-key org-mode-map [(control c)(control e)]  'org-table-to-excel)
  (define-key org-mode-map [(control e)]  'ignore)
  (define-key org-mode-map [(control e)]  'wuxch-export-dwim)
  (define-key org-mode-map [(meta h)]  'ignore)
  (define-key org-mode-map [(meta h)]  'org-mark-dwim)
  (define-key org-mode-map [(control k)] 'ignore)
  (define-key org-mode-map [(control k)] 'wuxch-org-kill-line)
  (define-key org-mode-map [(control w)]    'ignore)
  (define-key org-mode-map [(control w)]    'wuxch-org-select-dwim)
  (define-key org-mode-map [(meta delete)]  'wuxch-org-delete-dwim)

  (define-key org-mode-map [(control a)]  'ignore)
  (define-key org-mode-map [(control a)]  'mark-whole-buffer)


  (define-key org-mode-map [(control \,)] 'ignore)
  (define-key org-mode-map [(control \,)] 'wuxch-point-stack-push)
  (define-key org-mode-map [(control .)] 'ignore)
  (define-key org-mode-map [(control .)] 'wuxch-point-stack-pop)
  ;; (copy-face 'font-lock-keyword-face 'org-level-1)
  ;; (defface org-level-1 '((t (:inherit font-lock-keyword-face))) "")
  ;; level org-level-2 unchange
  ;; (copy-face 'font-lock-comment-face 'org-level-3)
  )

(defun org-mark-dwim (&optional arg allow-extend)
  (interactive "P\np")
  (cond
   ((org-on-heading-p) (wuxch-outline-mark-subtree-extend arg allow-extend))
   (t (mark-sexp arg allow-extend))
   )
  )

(defun wuxch-org-kill-line (&optional arg)
  "wuxch-org-kill-line:"
  (interactive "P")
  (beginning-of-line)
  (org-kill-line arg)
  ;; (kill-whole-line)
  )
(add-hook 'org-mode-hook 'wuxch-org-mode-hook)
(defun wuxch-org-agenda-switch-to ()
  "Go to the Org-mode file which contains the item at point."
  (interactive)
  (org-agenda-switch-to t)
  )
(defun wuxch-org-agenda-mode-hook ()
  ""
  (define-key org-agenda-mode-map [return]      'ignore)
  (define-key org-agenda-mode-map [return]      'wuxch-org-agenda-switch-to)
  )
(add-hook 'org-agenda-mode-hook 'wuxch-org-agenda-mode-hook)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(defun wuxch-export-dwim ()
  "wuxch-export-org-content-daim:
    - if the cursor is in a headline, export headline to html
    - if the cursor is in a table, export the table to excel"
  (interactive)
  (cond
   (mark-active (call-interactively 'org-region-to-html-and-browse))
   ((org-on-heading-p) (org-headline-to-html))
   ((org-at-table-p)(org-table-to-html))
   )
  )

(defun wuxch-org-select-dwim ()
  "wuxch-org-select-dwim:"
  (interactive)
  (cond
   ((org-at-table-p)(wuxch-org-select-block))
   (t (wuxch-mark-word))
   )
  )

(defun wuxch-org-delete-dwim ()
  "wuxch-org-delete-dwim:"
  (interactive)
  (cond
   ((org-at-table-p)
    (progn
      (wuxch-org-select-block)
      (cua-delete-region)
      ))
   (t (delete-horizontal-space))
   )
  )

(defun wuxch-forward-skip-whitespace ()
  "test:"
  (while (eq (char-after) #x20)
    (forward-char 1)
    )
  )

(defun wuxch-backward-skip-whitespace ()
  "test:"
  (while (eq (char-before) #x20)
    (backward-char 1)
    )
  )

(defun wuxch-org-select-block ()
  "wuxch-org-select-block:"
  (let ((current-pos (point))(begin-pos)(end-pos))
    (re-search-backward "|" nil t)
    (forward-char 1)
    (wuxch-forward-skip-whitespace)
    (setq begin-pos (point))
    (goto-char current-pos)
    (re-search-forward "|" nil t)
    (backward-char 1)
    (wuxch-backward-skip-whitespace)
    (setq end-pos (point))
    (push-mark current-pos)
    (push-mark begin-pos nil t)
    (goto-char end-pos)
    )
  )

(defun org-region-to-html-and-browse ()
  "org-region-to-html:"
  (interactive)
  (let ((html-buffer "*org-to-html*"))
    (org-export-region-as-html (region-beginning) (region-end) nil html-buffer)
    (browse-url-of-buffer html-buffer)
    )
  )

(defun org-headline-to-html ()
  "org-headline-to-html:export current outline content to html file and display it"
  ;; (interactive)
  (if (eq mark-active 'nil)
      (outline-mark-subtree))
  (org-region-to-html-and-browse)
  )

(defun org-table-to-html ()
  "org-table-to-html:"
  (if (org-at-table-p)
      (progn
        (push-mark (org-table-begin) nil t)
        (goto-char (org-table-end))
        (org-region-to-html-and-browse)
        )
    )
  )


(defun org-table-to-excel ()
  "可以把表格输出到文件，然后在excel打开，这样可以有更好的外观。"
  (interactive)
  (if (org-at-table-p)
      (let ((file org-table-to-excel-file-name)(excel-command))
        (org-table-to-cvs-file file)
        (setq excel-command (concat excel-exec-string file))
        (wuxch-shell-command-background excel-command)
        ;; (message "command is :%s" excel-command)
        )
    )
  )
(defun org-table-to-cvs-file (file-name)
  ""
  (let* ((beg (org-table-begin))
         (end (org-table-end))
         (table (buffer-substring beg end))
         (file file-name)
         buf)
    ;; (unless (or (not (file-exists-p file))
    ;;                 (y-or-n-p (format "Overwrite file %s? " file)))
    ;;       (error "Abort"))
    ;; 不用提示，直接删除重建
    (if (file-exists-p file)
        (delete-file file))
    (with-current-buffer (find-file-noselect file)
      (setq buf (current-buffer))
      (erase-buffer)
      (fundamental-mode)
      (insert table)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*|[ \t]*" nil t)
        (replace-match "" t t)
        (end-of-line 1))
      (goto-char (point-min))
      (while (re-search-forward "[ \t]*|[ \t]*$" nil t)
        (replace-match "" t t)
        (goto-char (min (1+ (point)) (point-max))))
      (goto-char (point-min))
      (while (re-search-forward "^-[-+]*$" nil t)
        (replace-match "")
        (if (looking-at "\n")
            (delete-char 1)))
      (goto-char (point-min))
      (while (re-search-forward "[ \t]*|[ \t]*" nil t)
        (replace-match "\t" t t))
      (save-buffer))
    (kill-buffer buf)
    )
  )

(custom-set-variables
 ;; 基本HTML输出配置
 '(org-export-html-style "<style type=\"text/css\">
  html {
	font-family: Times, serif;
	font-size: 10pt;
  }
  .title { text-align: center; }
  .todo  { color: red; }
  .done { color: green; }
  .timestamp { color: blue }
  .timestamp-kwd { color: CadetBlue }
  .tag { background-color:lightblue; font-weight:normal }
  .target { background-color: lavender; }
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #F3F5F7;
	padding: 5pt;
	font-family: courier, monospace;
  }
  td, th {
	vertical-align: middle;
  }
</style>")
 ;;
 '(org-export-html-toplevel-hlevel 3)
 ;; 不输出目录链接
 '(org-export-with-toc nil)
 ;; 不输出auth和时间
 '(org-export-author-info nil)
 '(org-export-with-timestamps t)
 ;; '(org-export-html-table-tag "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">")
 '(org-export-html-table-tag "<table border=\"1\" cellspacing=\"1\" cellpadding=\"1\" rules=\"all\" frame=\"hsides\">")
 '(org-export-table-header-tags '("<th align=center bgcolor=#BCD2EE>" . "</th>"))
 '(org-export-table-data-tags '("<td>" . "</td>"))
 '(org-export-with-emphasize nil)
 '(org-export-with-footnotes nil)
 '(org-export-with-sub-superscripts nil)
 '(org-export-with-TeX-macros nil)
 )


(require 'wuxch-remember)
(provide 'wuxch-org)

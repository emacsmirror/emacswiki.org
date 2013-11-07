;;; wuxch-dired.el

(if (equal 'windows-nt system-type)
    (progn
      (require 'w32-symlinks)
      (require 'w32-browser)
      )
  )

(require 'dired-aux)
(require 'dired+)
;; use '(' key or  ')' key to toogle detail display mode
(require 'dired-details+)
(require 'wdired)
;; (require 'dired-isearch)
(require 'find-dired+)
(require 'cl)

(define-key dired-mode-map "r" 'wuxch-wdired-change-to-wdired-mode)
(define-key wdired-mode-map [return]   'ignore)
(define-key wdired-mode-map [return]   'wuxch-wdired-finish-edit)
(define-key wdired-mode-map [(control g)]    'wuxch-wdired-abort-changes)

;; 以下三个函数：在调用真正的函数之前改变光标样式
(defun wuxch-set-cursor-wdired-mode ()
  (bar-cursor-mode -1)
  ;; (set-cursor-color "red")
  )

(defun wuxch-reset-cursor-wdired-mode ()
  (bar-cursor-mode 1)
  ;; (set-cursor-color "black")
  (wuxch-set-default-theme)
  )


(defun wuxch-wdired-change-to-wdired-mode ()
  ""
  (interactive)
  (wuxch-set-cursor-wdired-mode)
  (wdired-change-to-wdired-mode)
  )

(defun wuxch-wdired-finish-edit ()
  ""
  (interactive)
  (wuxch-reset-cursor-wdired-mode)
  (wdired-finish-edit)
  (wuxch-dired-up-directory)
  (diredp-find-file-reuse-dir-buffer)
  )

(defun wuxch-wdired-abort-changes ()
  ""
  (interactive)
  (wuxch-reset-cursor-wdired-mode)
  (wdired-abort-changes)
  )



;; 进入子目录的时候使用同一个buffer
(toggle-diredp-find-file-reuse-dir 1)
;; 让 dired 可以递归的拷贝和删除目录
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
;; 首先显示目录
(setq ls-lisp-dirs-first t)

;; 以下几个操作和totalcommand一致
(define-key dired-mode-map [tab] 'wuxch-dired-tab-process)
(define-key dired-mode-map [return] 'wuxch-dired-w32-browser)
(define-key dired-mode-map [backspace] 'wuxch-dired-up-directory)
(define-key dired-mode-map [delete] 'dired-do-delete)
(define-key dired-mode-map [double-down-mouse-1] 'wuxch-diredp-mouse-find-file)
(define-key dired-mode-map (kbd "<C-down-mouse-1>") 'ignore)
(define-key dired-mode-map (kbd "<C-mouse-1>") 'ignore)
(define-key dired-mode-map (kbd "<C-down-mouse-1>") 'diredp-mouse-mark/unmark)
;; 缺省的*m和m命令都是一致的，调用mark命令。现在把*m命令映射到多文件mark命令。
(define-key dired-mode-map "*m" 'ignore)
(define-key dired-mode-map "*m" 'dired-mark-files-regexp)
(define-key dired-mode-map "*n" 'ignore)
(define-key dired-mode-map "*n" 'dired-mark-files-regexp)

;; 这个命令好像有问题。
;; (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward-regexp)
;; (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward-regexp)

;; 当目录内容改变之后需要重新计算最大行，after-revert-hook好像没有在dired里面起作用，干脆自己写了一个
;; revert，强制加上更新最大行变量的操作。
(define-key dired-mode-map "g" 'ignore)
(define-key dired-mode-map "g" 'wuxch-dired-revert-and-goto-marked-file)
;; (define-key dired-mode-map [(control o)] 'ignore)
;; (define-key dired-mode-map [(control o)]    'other-window)

(define-key dired-mode-map [(control end)]    'wuxch-dired-goto-last-line)
(define-key dired-mode-map [(control home)]     'wuxch-dired-goto-first-line)
(define-key dired-mode-map [(control down)]    'wuxch-dired-goto-last-line)
(define-key dired-mode-map [(control up)]     'wuxch-dired-goto-first-line)

(define-key dired-mode-map [(control a)]    'wuxch-mark-all-files-directories)
(define-key dired-mode-map "A"    'wuxch-mark-all-files-directories)
(define-key dired-mode-map "I"  'wuxch-dired-open-info-file)
;; (define-key dired-mode-map "F"  'wuxch-foobar-add-to-list)
;; (define-key dired-mode-map "X"  'wuxch-uncompress-file)

(define-key dired-mode-map "=" 'ignore)
(define-key dired-mode-map "=" 'wuxch-dired-equal-process)

;; same key assignment as totalcommand
(define-key dired-mode-map [(control \1)] 'ignore)
(define-key dired-mode-map [(control \1)] 'wuxch-get-file-name-only-path)
(define-key dired-mode-map [(control \2)] 'ignore)
(define-key dired-mode-map [(control \2)] 'wuxch-get-file-name-without-path)
(define-key dired-mode-map [(control \3)] 'ignore)
(define-key dired-mode-map [(control \3)] 'wuxch-get-file-name-with-path)
(define-key dired-mode-map [(control c)(control \3)] 'wuxch-get-file-name-with-path-with-double-slash)
(define-key dired-mode-map [(control x)(control \3)] 'wuxch-get-file-name-with-path-with-unix-style)
(define-key dired-mode-map [f5] 'wuxch-dired-do-copy)
(define-key dired-mode-map [f6] 'wuxch-dired-do-rename)

(define-key dired-mode-map [f3]  'wuxch-w32-find-dired)
;; (define-key dired-mode-map [(control f3)] 'wuxch-find-grep-dired)
;; wuxch-dired-open-ie
(define-key dired-mode-map [(control x)(i)] 'wuxch-dired-open-ie)

(define-key dired-mode-map [home]    'wuxch-dired-move-beginning-of-line)
(define-key dired-mode-map [end]     'wuxch-dired-move-end-of-line)

(define-key dired-mode-map [(control meta up)]     'wuxch-move-to-up-dir)
(define-key dired-mode-map [(control meta down)]     'wuxch-move-to-down-dir)

(define-key dired-mode-map [(meta o)] 'nil)

(define-key dired-mode-map "l"  'wuxch-bookmark-bmenu-list)

(define-key dired-mode-map "a"  'wuxch-dired-tag)

(define-key dired-mode-map "P"  'ignore)
(define-key dired-mode-map "P"  'wuxch-dired-play)

(define-key dired-mode-map [(\ )]  'ignore)
(define-key dired-mode-map [(\ )]  'wuxch-dired-play-concole)

(define-key dired-mode-map [(control c)(\ )]  'ignore)
(define-key dired-mode-map [(control c)(\ )]  'wuxch-dired-play-gui)

(defun wuxch-do-dired-get-subtitle ()
  "wuxch-do-dired-get-subtitle:"
  (let ((subtitle-ext "srt")
        (subtitle-files)
        (single-subtitle-file)
        (subtitle-string "")
        (is-first-subtitle t)
        (comma-string)
        )
    (dired-unmark-all-marks)
    (when (diredp-mark/unmark-extension subtitle-ext nil)
      (setq subtitle-files (dired-get-marked-files t))
      (if (listp subtitle-files)
          (progn
            (setq subtitle-string " -sub ")
            (dolist (element subtitle-files)

              (setq single-subtitle-file
                    (double-quote-file-name
                     (convert-standard-filename
                      (concat (dired-current-directory) element)))
                    )

              (if is-first-subtitle
                  (progn
                    (setq comma-string "")
                    (setq is-first-subtitle nil))
                (progn
                  (setq comma-string ",")
                  )
                )
              (setq subtitle-string (concat subtitle-string comma-string single-subtitle-file))
              )
            )
        )
      (dired-unmark-all-marks)
      )
    subtitle-string
    )
  )

(defun wuxch-dired-file-is-video-file (file-name)
  "wuxch-dired-file-is-video-file:"
  (if (file-directory-p file-name)
      nil
    (let ((ext (downcase (file-name-extension file-name))))
      (or (string= ext "avi")(string= ext "mkv")(string= ext "m4v")
          (string= ext "mp4")(string= ext "mpg")(string= ext "mpeg")
          (string= ext "rmvb")(string= ext "wmv")(string= ext "mp3")
          (string= ext "flv")(string= ext "wma")(string= ext "wmv")
          )
      )
    )
  )

(defun wuxch-dired-file-is-audio-file (file-name)
  "wuxch-dired-file-is-video-file:"
  (if (file-directory-p file-name)
      nil
    (let ((ext (file-name-extension file-name)))
      (or (string= ext "mp3")
          )
      )
    )
  )

;; file is f:/AUDIO_TS
;; file is f:/VIDEO_TS

(defun wuxch-dired-file-is-dvd-file (file-name)
  "wuxch-dired-file-is-dvd-file:
result-str = nil : the file-name is not a dvd directroy.
result-str = not nil : the file-name is a dvd directory. and result-str is the driver str. for example: f:/
"
  (let* ((replace-str nil)
         (result-str nil)
         )
    (setq replace-str (replace-regexp-in-string "\\(.*:/\\)\\(AUDIO_TS\\|VIDEO_TS\\)" "\\1" file-name))
    (if (string= replace-str file-name)
        (setq result-str nil)
      (setq result-str replace-str)
      )
    result-str
    )
  )

(defun wuxch-do-dired-play (concole)
  "wuxch-do-dired-play:"
  (let* ((play-command)
         (player)
         (movie-file (dired-get-file-for-visit))
         (ext (file-name-extension movie-file))
         ;; chinese support config
         (coding-system-for-read (coding-system-from-name "chinese-gbk-dos"))
         (coding-system-for-write (coding-system-from-name "chinese-gbk-dos"))
         (coding-system-require-warning t)
         )

    (if concole
        (setq player "mplayer")
      (setq player "gmplayer")
      )

    (cond
     ((wuxch-dired-file-is-video-file movie-file)
      (progn
        (setq movie-file (double-quote-file-name (convert-standard-filename movie-file)))
        (setq play-command (concat player " " movie-file (wuxch-do-dired-get-subtitle) " &"))
        ))

     ((wuxch-dired-file-is-audio-file movie-file)
      (progn
        (setq movie-file (double-quote-file-name (convert-standard-filename movie-file)))
        (setq play-command (concat "cmd " player " " movie-file ""))
        ))
     ((wuxch-dired-file-is-dvd-file movie-file)
      (progn
        (setq play-command (concat player " dvd://2 -dvd-device " (wuxch-dired-file-is-dvd-file movie-file) ""))
        ))
     (t
      (setq play-command nil)
      )
     )

    (if play-command
        (progn
          (message "command is :%s" play-command)
          (shell-command play-command)
          )
      (progn
        (message "%s can not be played by mplayer." (file-name-nondirectory movie-file))
        )

      )
    )
  )

(defun double-quote-file-name (file-name)
  "double-quote-file-name:"
  (concat "\"" file-name "\"")
  )

(defun wuxch-dired-play-concole ()
  "wuxch-dired-play:"
  (interactive)
  (wuxch-do-dired-play t)
  )

(defun wuxch-dired-play-gui ()
  "wuxch-dired-play:"
  (interactive)
  (wuxch-do-dired-play nil)
  )

(defun wuxch-dired-open-info-file ()
  ""
  (interactive)
  (info (dired-get-filename))
  )

;; (define-key dired-mode-map (kbd "?") 'dired-get-size)
;; 覆盖原有的函数，不高亮显示当前行，原函数在dired.el和dired+.el都有
(fset 'dired-insert-set-properties 'wuxch-dired-insert-set-properties)

(defun wuxch-dired-insert-set-properties (beg end)
  "Make the file names highlight when the mouse is on them."
  )

(defun wuxch-dired-move-beginning-of-line (arg)
  ""
  (interactive "p")
  (move-beginning-of-line arg)
  (dired-move-to-filename))

(defun wuxch-dired-move-end-of-line (arg)
  ""
  (interactive "p")
  (move-end-of-line arg)
  ;; (dired-move-to-filename)
  )

;; 拷贝之后，最好光标跳到目标目录，因此在dired-do-copy外面再打一个包
(defun wuxch-dired-do-copy(&optional arg)
  ""
  (interactive "P")
  (dired-do-copy arg)
  (if (not (one-window-p))
      (other-window 1))
  (wuxch-dired-revert)
  ;; (wuxch-dired-revert-and-goto-marked-file)
  ;;   (dired-previous-line 1)
  )

;; 移动之后，最好光标跳到目标目录，因此在dired-do-copy外面再打一个包
(defun wuxch-dired-do-rename(&optional arg)
  ""
  (interactive "P")
  (dired-do-rename arg)
  (wuxch-dired-revert)
  (if (not (one-window-p))
      (other-window 1))
  (wuxch-dired-revert)
  ;; (wuxch-dired-revert-and-goto-marked-file)
  ;; (dired-previous-line 1)
  )

;; 到上级目录总是打开一个新的buffer，这里采用buffer操作修正
(defun wuxch-dired-up-directory ()
  "Dired to up directory, reuse the current buffer"
  (interactive)
  (let ((temp-previous-buffer)(up-directory-buffer)(should-kill-temp-buffer))
    (setq should-kill-temp-buffer (not (wuxch-other-windows-has-same-buffer)))
    (setq temp-previous-buffer (current-buffer))
    (dired-up-directory nil)
    (setq up-directory-buffer (current-buffer))
    ;; 如果同时打开多个窗口，而且有2个窗口的buffer内容一样，那么就不必把原目录的buffer删掉
    (if should-kill-temp-buffer
        (kill-buffer temp-previous-buffer))
    (set-buffer up-directory-buffer)
    )
  )

(defun wuxch-other-windows-has-same-buffer ()
  ""
  (if (one-window-p)
      (null '(1))
    (progn
      (let ((other-side-window-buffer (window-buffer (next-window))))
        (if (eq other-side-window-buffer (current-buffer))
            (null '())
          (null '(1))))
      )
    )
  )

(defun wuxch-diredp-mouse-find-file (event)
  "Replace dired in its window by this file or directory."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file (dired-get-filename))))
    (if (file-directory-p file)
        (progn
          (diredp-find-file-reuse-dir-buffer))
      (w32-browser (convert-standard-filename file)))))

;; 修改dired-w32-browser,进入子目录的时候调用diredp-find-file-reuse-dir-buffer，这样可以不需要新开一
;; 个buffer。
(defun wuxch-dired-w32-browser ()
  "Run default Windows application associated with current line's file.
If file is a directory, then `dired-find-file' instead.
If no application is associated with file, then `find-file'."
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
        (progn
          (if (not (wuxch-other-windows-has-same-buffer))
              (diredp-find-file-reuse-dir-buffer)
            (find-file (dired-get-file-for-visit)))
          )
      (progn
        ;; 如果是.某些文件，那么直接打开，不需要调用ie了
        (let ((ext (file-name-extension file)))
          (if (or (string= ext "el") (string= ext "c") (string= ext "h") (string= ext "outline"))
              (find-file (dired-get-file-for-visit))
            (w32-browser (convert-standard-filename file))
            )
          )
        )
      )
    )
  )

(defun wuxch-dired-open-ie ()
  "open directory by IE. If cursor is on some sub-directory, open this sub-directory,
else open the current directory.06/11/2007 10:54:28 wuxch"
  (interactive)
  (let ((directory-path (dired-current-directory)))
    ;; 如果当前光标没有任何内容，则直接使用当前的目录
    ;; (if (not directory-path)
    ;;         (setq directory-path (dired-current-directory)))
    ;; 如果当前的光标是目录，那么打开此目录。如果是文件，那么打开当前目录
    ;; 10/28/2008 09:20:42,即使是目录，还是打开当前目录比较方便，所以改回去。
    ;; (if (not (file-directory-p directory-path))
    ;;         (setq directory-path (dired-current-directory)))
    (w32-shell-execute "open" (convert-standard-filename directory-path))
    )
  ;; (message "directory-path is %s" (dired-get-filename nil t))
  )



(defun wuxch-dired-tag()
  "wuxch-dired-tag:"
  (interactive)
  (let* ((files (dired-get-marked-files))
         (number-of-files (safe-length files)))
    (cond
     ;; not mark any one
     ((eq number-of-files 0)
      (let ((file-dir-name (dired-get-filename nil t)))
        (if file-dir-name
            (if (file-directory-p file-dir-name)
                (make-elisp-tag t file-dir-name t)
              (make-elisp-tag nil file-dir-name t)
              )
          )
        )
      )
     ;; have marked some
     (t
      (progn
        (while files
          (let ((file (car files)))
            (if (file-directory-p file)
                (make-elisp-tag t file nil)
              (make-elisp-tag nil file nil)
              )
            )
          (setq files (cdr files))
          )
        )
      )
     )
    )
  )

(defun wuxch-dired-hook ()
  ""
  ;;   (setq hl-line-sticky-flag nil)
  ;;   (hl-line-mode)
  )
(add-hook 'dired-load-hook 'wuxch-dired-hook)

(defun wuxch-dired-mode-hook-fun ()
  ""
  ;; 修改buffer名称，便于识别是目录
  ;; (let ((new-buffer-name (concat "<" (buffer-name) ">"))(buffer-seq 0))
  ;;     ;; 需要先看看是否已经有同名的目录buffer，如果有的话，在后面加上数字序号
  ;;     (while  (bufferp (get-buffer new-buffer-name))
  ;;       (setq new-buffer-name (concat "<" (buffer-name) (number-to-string buffer-seq) ">"))
  ;;       (setq buffer-seq (1+ buffer-seq))
  ;;       )
  ;;     (rename-buffer new-buffer-name)
  ;;     )
  (wuxch-dired-set-doc-face)
  (wuxch-dired-set-elisp-face)
  (wuxch-dired-set-exe-face)
  (wuxch-dired-set-avi-face)
  )

(add-hook 'dired-mode-hook 'wuxch-dired-mode-hook-fun)

(defface wuxch-dired-doc-face   '((t (:inherit font-lock-warning-face))) "doc files")
(defface wuxch-dired-elisp-face '((t (:inherit font-lock-keyword-face))) "elisp files")
(defface wuxch-dired-exe-face   '((t (:inherit font-lock-function-name-face))) "exe files")
(defface wuxch-dired-avi-face   '((t (:inherit font-lock-variable-name-face))) "avi files")

(defun wuxch-dired-set-doc-face ()
  "wuxch-dired-set-doc-face:"
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(tex\\|doc\\|docx\\|xls\\|xlsx\\|txt\\|org\\|ppt\\|pptx\\|html\\|xml\\|xsl\\|xsd\\|sty\\|mod\\|dtd\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'wuxch-dired-doc-face)))))
  )

(defun wuxch-dired-set-elisp-face ()
  "wuxch-dired-set-elisp-face:"
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(el\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'wuxch-dired-elisp-face)))))
  )

(defun wuxch-dired-set-exe-face ()
  "wuxch-dired-set-exe-face:"
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(exe\\|EXE\\|bat\\|BAT\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'wuxch-dired-exe-face)))))
  )

(defun wuxch-dired-set-avi-face ()
  "wuxch-dired-set-avi-face:"
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(pdf\\|chm\\|flv\\|avi\\|AVI\\|mkv\\|rmvb\\|mpeg\\|mpg\\|MPG\\|rm\\|mp4\\|mp3\\|MP3\\|wmv\\|wma\\|m4v\\|mov\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'wuxch-dired-avi-face)))))
  )

(defun wuxch-dired (dirname &optional switches)
  "修改此函数的定义，目的是调用dired的时候缺省目录使用我自己需要的目录"
  (interactive (wuxch-dired-read-dir-and-switches ""))
  (switch-to-buffer (dired-noselect dirname switches)))

(defvar wuxch-dired-default-directory "C:/Work")
(defun wuxch-dired-read-dir-and-switches (str)
  ;; For use in interactive.
  (reverse (list
            (if current-prefix-arg
                (read-string "Dired listing switches: "
                             dired-listing-switches))
            ;; If a dialog is about to be used, call read-directory-name so
            ;; the dialog code knows we want directories.  Some dialogs can
            ;; only select directories or files when popped up, not both.
            (if (next-read-file-uses-dialog-p)
                (read-directory-name (format "Dired %s(directory): " str)
                                     nil wuxch-dired-default-directory nil)
              (read-file-name (format "Dired %s(directory): " str)
                              nil default-directory nil)))))

(global-set-key [(control x)(d)] 'ignore)
(global-set-key [(control x)(d)] 'wuxch-dired)

;; 这个参数比较重要，当同时打开2个buffer时，复制的缺省路径就是另一个buffer的dired路径。使用起来有些像
;; totalcommand
(setq dired-dwim-target t)

(defun do-wuxch-get-file-name (with-full-path only-path)
  ""
  (let ((clipboard))
    (if only-path
        ;; 只要路径，不关心文件名
        (progn
          (setq clipboard (convert-standard-filename (dired-current-directory)))
          )
      ;; 需要文件名
      (progn
        (let ((file (dired-get-file-for-visit)))
          (if with-full-path
              (progn
                ;; 如果是路径，那么在最后补一个"/"
                (if (file-directory-p file)
                    (setq file (concat file "/")))
                ;; 需要包括路径的文件名
                (setq clipboard (convert-standard-filename file))
                )
            ;; 不包括路径的文件名（或者是目录名）
            (progn
              (if (file-directory-p file)
                  (progn
                    (setq clipboard (file-name-nondirectory file))
                    )
                (progn
;;;                 ;; 如果是普通文件名，那么不需要扩展名
;;;                 (setq clipboard (file-name-sans-extension (file-name-nondirectory file))))))
                  (setq clipboard (file-name-nondirectory file))
                  )
                )
              )
            )
          )
        )
      )

    (kill-new clipboard)
    (message "copy string \"%s\" to clipboard" clipboard)
    clipboard
    )
  )



;; 写一个函数，可以复制当前文件的名称和路径到剪贴板。
(defun wuxch-get-file-name-with-path ()
  ""
  (interactive)
  (do-wuxch-get-file-name t nil)
  )

(defun wuxch-get-file-name-with-path-with-unix-style ()
  ""
  (interactive)
  (let ((full-string (do-wuxch-get-file-name t nil))
        (clipboard))
    (setq clipboard (replace-regexp-in-string "\\\\" "/" full-string))
    (kill-new clipboard)
    (message "copy string \"%s\" to clipboard" clipboard)
    )
  )

(defun wuxch-get-file-name-with-path-with-double-slash ()
  ""
  (interactive)
  (let ((full-string (do-wuxch-get-file-name t nil))
        (clipboard))
    (setq clipboard (replace-regexp-in-string "\\\\" "\\\\\\\\" full-string))
    (kill-new clipboard)
    (message "copy string \"%s\" to clipboard" clipboard)
    )
  )


(defun wuxch-get-file-name-without-path ()
  ""
  (interactive)
  (do-wuxch-get-file-name nil nil)
  )

(defun wuxch-get-file-name-only-path ()
  ""
  (interactive)
  (do-wuxch-get-file-name t t)
  )


(defun wuxch-dired-tab-process ()
  ""
  (interactive)
  (let ((buf (current-buffer)))
    (if (one-window-p)
        ;; 如果只有一个窗口，打开一个新的窗口，内容和本目录的一样
        (progn
          (split-window-horizontally)
          (other-window 1)
          (set-window-buffer (selected-window) buf))
      ;; 如果不只一个窗口，调到另一个窗口
      (other-window 1))
    )
  )

(defun wuxch-dired-equal-process ()
  ""
  (interactive)
  (if (not (one-window-p))
      (let ((buf (current-buffer)))
        ;; 跳到另一个窗口，同时设定2个窗口的内容一样
        (other-window 1)
        (set-window-buffer (selected-window) buf))))

(define-key dired-mode-map "n" 'ignore)
(define-key dired-mode-map "n" 'wuxch-dired-next-line)
(define-key dired-mode-map [down] 'ignore)
(define-key dired-mode-map [down] 'wuxch-dired-next-line)

;; 终于清楚local-variable的用法了。
;; 先使用global-variable，注意使用setq-default设置，一旦make-local-variable之后就不在改变了。
;; (defvar wuxch-temp-buffer-local-value 0)
;; (defun wuxch-temp-use-local-value ()
;;   ""
;;   (interactive)
;;   (if (local-variable-p 'wuxch-temp-buffer-local-value)
;;       (progn
;;         (message "variable is local, value is %d" wuxch-temp-buffer-local-value))
;;     (progn
;;       (setq-default wuxch-temp-buffer-local-value (+ wuxch-temp-buffer-local-value 1))
;;       (make-local-variable 'wuxch-temp-buffer-local-value)
;;       (message "make variable locally, and set it to %d" wuxch-temp-buffer-local-value))
;;     )
;;   )

(defvar static-wuxch-first-line-of-buffer)
(defun wuxch-get-first-line-of-dired ()
  ""
  (if (local-variable-p 'static-wuxch-first-line-of-buffer)
      (progn
        ;; (message "------%d" static-wuxch-first-line-of-buffer )
        )
    (progn
      (setq-default static-wuxch-first-line-of-buffer (wuxch-get-first-line-of-dired-by-search-double-dot))
      (make-local-variable 'static-wuxch-first-line-of-buffer)
      ;; (message "++++++%d" static-wuxch-first-line-of-buffer)
      )
    )
  static-wuxch-first-line-of-buffer
  )

(defvar static-wuxch-max-line-of-buffer)
(defun update-dired-static-variables ()
  ""
  (if (local-variable-p 'static-wuxch-first-line-of-buffer)
      (kill-local-variable 'static-wuxch-first-line-of-buffer))
  (if (local-variable-p 'static-wuxch-max-line-of-buffer)
      (kill-local-variable 'static-wuxch-max-line-of-buffer))

  (setq-default static-wuxch-first-line-of-buffer (wuxch-get-first-line-of-dired-by-search-double-dot))
  (make-local-variable 'static-wuxch-first-line-of-buffer)
  (setq-default static-wuxch-max-line-of-buffer (wuxch-dired-max-line-by-count))
  (make-local-variable 'static-wuxch-max-line-of-buffer)
  )

(defun wuxch-dired-revert ()
  ""
  (revert-buffer)
  (update-dired-static-variables)
  (goto-line (wuxch-get-first-line-of-dired))
  (dired-move-to-filename)
  )

(defun wuxch-dired-revert-and-goto-marked-file (arg)
  ""
  (interactive "p")
  (wuxch-dired-revert)
  ;; go to marked file if there is any
  (dired-next-marked-file arg)
  )


(defun wuxch-dired-max-line ()
  ""
  (if (local-variable-p 'static-wuxch-max-line-of-buffer)
      (progn
        ;; (message "------%d" static-wuxch-max-line-of-buffer )
        )
    (progn
      (setq-default static-wuxch-max-line-of-buffer (wuxch-dired-max-line-by-count))
      (make-local-variable 'static-wuxch-max-line-of-buffer)
      ;; (message "++++++%d" static-wuxch-max-line-of-buffer)
      )
    )
  (+ static-wuxch-max-line-of-buffer 0)
  )


;; 就是以下这两个函数，考虑到每次调用会消耗不少的CPU，因此使用类似于c的静态变量方式，
;; 做过一次就保存数值，之后不断地重复调用，除非更新目录内容。

;; 每个dired buffer里面最后一行会多一个空行，因此这里多加一行。
(defconst wuxch-dired-add-addtional-line 1)
(defun wuxch-dired-max-line-by-count ()
  ""
  (+ (count-lines (point-min) (point-max)) wuxch-dired-add-addtional-line))

(defun wuxch-get-first-line-of-dired-by-search-double-dot ()
  ""
  (goto-char (point-min))
  (if (search-forward ".." nil t)
      ;; 找到了。此时光标所在的第一行就是..的下一行
      (+ (line-number-at-pos) 1)
    ;; 没有找到 ..，测试光标所在的第一行是第二行
    (+ (line-number-at-pos) 1)
    )
  )

(defun wuxch-dired-next-line (arg)
  "moving to the next line with wrapping"
  (interactive "p")
  (dired-next-line arg)
  (let ((temp-current-line (line-number-at-pos))
        (temp-max-line (wuxch-dired-max-line)))
    (if (eq temp-current-line temp-max-line)
        (progn
          (goto-line (wuxch-get-first-line-of-dired))
          (dired-move-to-filename)))
    )
  )

(defun wuxch-dired-goto-last-line ()
  "moving to the last line"
  (interactive)
  (goto-line (- (wuxch-dired-max-line) 1))
  (dired-move-to-filename))

(defun wuxch-dired-goto-first-line ()
  "moving to the last line"
  (interactive)
  (goto-line (wuxch-get-first-line-of-dired))
  (dired-move-to-filename))

(define-key dired-mode-map "p" 'ignore)
(define-key dired-mode-map "p" 'wuxch-dired-previous-line)
(define-key dired-mode-map [up] 'ignore)
(define-key dired-mode-map [up] 'wuxch-dired-previous-line)

(defun wuxch-dired-previous-line (arg)
  "moving to the previous line with wrapping"
  (interactive "p")
  (dired-previous-line arg)
  (let ((temp-current-line (line-number-at-pos))
        (temp-max-line (wuxch-dired-max-line))
        (temp-first-line-of-dried (wuxch-get-first-line-of-dired)))
    ;; 因为wuxch-get-first-line-of-dired会跳到第一行，因此在做下一行操作的时候需要先恢复光标到开始的位置。
    (goto-line temp-current-line)
    (dired-move-to-filename)
    (if (eq temp-current-line (- temp-first-line-of-dried 1))
        (progn
          (goto-line (- temp-max-line 1))
          (dired-move-to-filename)))
    )
  )

;; (setq dired-listing-switches "-alh")

;; 几个常用的文件
;; (defun cq-update ()
;;   ""
;;   (interactive)
;;   (let ((from-file "y:/cq/cq_raw_data.txt")
;;         (to-file "c:/work/Oversea/Version/cq_raw_data.txt")
;;         (xlsx_file "c:/work/Oversea/Version/cq.xlsx"))
;;     (dired-rename-file from-file to-file t)
;;     (w32-browser (dired-replace-in-string "/" "\\" xlsx_file))
;;     )
;;   )

;; (defun xt ()
;;   ""
;;   (interactive)
;;   (let ((dest-file "d:\\wuxch\\xt.xlsx"))
;;     (w32-browser (dired-replace-in-string "/" "\\" dest-file))
;;     )
;;   )

;; (defun change-ftp-coding-system (coding)
;;   (interactive "zFtp Coding System: ")
;;   (when (and coding (coding-system-p coding))
;;     (let ((parsed (ange-ftp-ftp-name (expand-file-name default-directory))))
;;       (set-process-coding-system
;;        (get-buffer-process (ange-ftp-ftp-process-buffer
;;                             (nth 0 parsed) (nth 1 parsed)))
;;        coding coding))))

(defun wuxch-mark-all-files-directories ()
  "mark all files and directoies"
  (interactive)
  (dired-mark-files-regexp ".*"))

;; (defun wuxch-foobar-add-to-list (&optional arg)
;;   "add marked mp3 files to foobar play list"
;;   (interactive "P")
;;   (let ((mp3-file)(mp3-add-command)(file-list (dired-get-marked-files t arg)))
;;     ;; (message "file-list is %s" file-list)
;;     (while (not (null file-list))
;;       (setq mp3-file (concat (dired-current-directory) (pop file-list)))
;;       ;; (message "mp3-file is %s" mp3-file)
;;       ;; 注意，对于program files这样的路径，需要加上双引号。文件名也可能有空格，所以也添加双引号。
;;       (setq mp3-add-command (concat "C:\\\"Program Files\"\\foobar2000\\foobar2000.exe /add " "\"" (dired-replace-in-string "/" "\\" mp3-file) "\"" ))
;;       (message "command is %s" mp3-add-command)
;;       (shell-command mp3-add-command)
;;       )
;;     )
;;   )

;; (defun wuxch-uncompress-file (&optional arg)
;;   "uncompress rar file by winrar"
;;   (interactive "P")
;;   (let ((rar-file)(rar-file-dos-format)(winrar-uncompress-command)(file-list (dired-get-marked-files t arg)))
;;     (while (not (null file-list))
;;       (setq rar-file (concat (dired-current-directory) (pop file-list)))
;;       ;; 注意，对于program files这样的路径，需要加上双引号。文件名也可能有空格，所以也添加双引号。
;;       (setq rar-file-dos-format (concat "\"" (dired-replace-in-string "/" "\\" rar-file) "\""))

;;       (setq winrar-uncompress-command (concat "C:\\\"Program Files\"\\WinRAR\\WinRAR.exe e " rar-file-dos-format))
;;       ;; (message "winrar-uncompress-command is:%s" winrar-uncompress-command)
;;       (message "uncompressing:%s" rar-file-dos-format)
;;       (shell-command winrar-uncompress-command)
;;       )
;;     )
;;   )

(defun wuxch-move-to-up-dir (&optional arg)
  "move marked files to up directory"
  (interactive "P")
  ;; (wuxch-mark-all-files-directories)
  (let ((up-dir (file-name-directory (directory-file-name (dired-current-directory))))
        (file-list (dired-get-marked-files t arg))
        (file-name-only)
        (old-file-name)
        (new-file-name))
    (while (not (null file-list))
      (setq file-name-only (pop file-list))
      (setq old-file-name (concat (dired-current-directory) file-name-only) )
      (setq new-file-name (concat up-dir file-name-only) )
      ;; (message "old name:%s, new name:%s" old-file-name new-file-name)
      (dired-rename-file old-file-name new-file-name nil)
      )
    )
  (wuxch-dired-up-directory)
  (wuxch-dired-revert)
  )

(defun wuxch-move-to-down-dir (&optional arg)
  "move marked files to down directory"
  (interactive "P")
  ;; (wuxch-mark-all-files-directories)
  (let ((down-dir (dired-get-file-for-visit))
        (file-list (dired-get-marked-files t arg))
        (file-name-only)
        (old-file-name)
        (new-file-name))
    (while (not (null file-list))
      (setq file-name-only (pop file-list))
      (setq old-file-name (concat (dired-current-directory) file-name-only) )
      (setq new-file-name (concat down-dir "/" file-name-only) )
      ;; (message "old name:%s, new name:%s" old-file-name new-file-name)
      (dired-rename-file old-file-name new-file-name nil)
      )
    )
  (wuxch-dired-revert)
  )

(define-key dired-mode-map [(control =)] 'ignore)
(define-key dired-mode-map [(control =)] 'wuxch-dired-compare-files)
(define-key dired-mode-map [(control c)(=)] 'wuxch-dired-compare-files)


(defun wuxch-compare-two-directories-of-current-2-windows ()
  (let ((dir1)(dir2))
    (if (or (one-window-p) (wuxch-other-windows-has-same-buffer))
        (message "please split windows or set diffenent directory in tow windows")
      (progn
        (setq dir1 (dired-current-directory))
        (other-window 1)
        (setq dir2 (dired-current-directory))
        (other-window 1)
        (ediff-directories dir1 dir2 nil)
        )
      )
    )
  )

(defun wuxch-dired-compare-2 (a b)
  (cond
   ( ;; both are directories
    (and (file-directory-p a)(file-directory-p b))
    (ediff-directories a b))
   ( ;; both are files
    (and (not (file-directory-p a))(not (file-directory-p b)))
    (ediff-files a b))
   (t
    (message "can not compare because 2 marked things have different type" ))
   )
  )

(defun wuxch-dired-compare-3 (a b c)
  (cond
   ( ;; both are directories
    (and (file-directory-p a)(file-directory-p b)(file-directory-p c))
    (ediff-directories3 a b))
   ( ;; both are files
    (and (not (file-directory-p a))(not (file-directory-p b))(not (file-directory-p c)))
    (ediff-files a b))
   (t
    (message "can not compare because 3 marked things have different type" ))
   )
  )

(defun wuxch-dired-compare-files ()
  "wuxch-dired-compare-files:"
  (interactive)
  (let* ((files (dired-get-marked-files))
         (number-of-files (safe-length files)))
    (cond
     ((eq number-of-files 2)
      (wuxch-dired-compare-2 (nth 0 files) (nth 1 files))
      )
     ((eq number-of-files 3)
      (wuxch-dired-compare-3 (nth 0 files) (nth 1 files) (nth 2 files))
      )
     (t
      (wuxch-compare-two-directories-of-current-2-windows))
     )
    )
  )


;; (defun wuxch-compare-two-directories ()
;;   "compare tow directories by Araxis Merge"
;;   (interactive)
;;   (let ((quoto-string "\"")(compare-command)(dir1)(dir2))
;;     (if (or (one-window-p) (wuxch-other-windows-has-same-buffer))
;;         (message "please split windows or set diffenent directory in tow windows")
;;       (progn
;;         ;; (setq dir1 (concat quoto-string (dired-replace-in-string "/" "\\" (dired-current-directory)) quoto-string))
;;         (setq dir1 (wuxch-quoto-string (dired-replace-in-string "/" "\\" (dired-current-directory))))
;;         (other-window 1)
;;         ;; (setq dir2 (concat quoto-string (dired-replace-in-string "/" "\\" (dired-current-directory)) quoto-string))
;;         (setq dir1 (wuxch-quoto-string (dired-replace-in-string "/" "\\" (dired-current-directory))))
;;         (other-window 1)
;;         (message "dir1:%s" dir1)
;;         (setq compare-command (concat merge-exec-string dir1 " " dir2 " &"))
;;         (message "command is:%s" compare-command)
;;         ;; (shell-command compare-command)
;;         (wuxch-shell-command-background compare-command)
;;         )
;;       )
;;     )
;;   )

(defun wuxch-quoto-string (arg-string)
  "给arg-string添加引号，主要用于一些目录中有空格"
  (let ((quoto-string "\""))
    (concat quoto-string arg-string quoto-string)
    )
  )

(defun netdir()
  (interactive)
  (require 'widget)
  (let* ((drvL))
    (with-temp-buffer
      (let ((out (shell-command "net use" (current-buffer))))
        (if (eq out 0)
            (while (re-search-forward "[A-Z]: +\\\\\\\\[^ ]+" nil t nil)
              (setq drvL (cons (split-string (match-string 0)) drvL)))
          (error "Unable to issue the NET USE command"))))
    (pop-to-buffer "*NET DIR LIST*")
    (erase-buffer)
    (widget-minor-mode 1)
    (mapcar
     (lambda (x)
       (lexical-let ((x x))
         (widget-create 'push-button
                        :notify (lambda (widget &rest ignore)
                                  (kill-buffer (current-buffer))
                                  (dired (car x)))
                        (concat (car x) "  " (cadr x))))
       (widget-insert "\n")
       (goto-char (point-min))
       )
     (reverse drvL))))

(defun wuxch-w32-find-dired (pattern)
  "Use cmd.exe's `dir' command to find files recursively, and go
into Dired mode on a buffer of the output. The command run (after
changing into DIR) is

    dir /s /b DIR\\PATTERN

   --wuxch: change pattern, add *, such as *pattern*
"

  (interactive (list (read-string "Search for: " w32-find-dired-pattern
                                  '(w32-find-dired-pattern-history . 1))))
  (let ((dir (dired-current-directory))
        (dired-buffers dired-buffers)(pattern_around_star (concat "*" pattern "*")))

    ;; Expand DIR ("" means default-directory)
    (setq dir (abbreviate-file-name
               (file-name-as-directory (expand-file-name dir))))
    ;; Check that it's really a directory.
    (or (file-directory-p dir)
        (error "w32-find-dired needs a directory: %s" dir))

    (switch-to-buffer (get-buffer-create "*w32-find-dired*"))

    ;; See if there's still a process running, and offer to kill it
    ;; first, if it is.
    (let ((find (get-buffer-process (current-buffer))))
      (when find
        (if (or (not (eq (process-status find) 'run))
                (yes-or-no-p "A `for' process is running; kill it? "))
            (condition-case nil
                (progn
                  (interrupt-process find)
                  (sit-for 1)
                  (delete-process find))
              (error nil))
          (error "Cannot have two processes in `%s' at once" (buffer-name)))))
    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory dir
          w32-find-dired-pattern pattern) ; save for next interactive
                                        ; call
    ;; The next statement will bomb in classic dired (no optional arg
    ;; allowed)
    (dired-mode dir (cdr find-ls-option))
    ;; This really should rerun the find command, but I don't
    ;; have time for that.
    (use-local-map (append (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "g" 'undefined)
    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
        ;; will work even with nested dired format (dired-nstd.el,v
        ;; 1.15 and later)
        (dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm)
      (set (make-local-variable 'dired-subdir-alist)
           (list (cons default-directory (point-min-marker)))))
    (setq buffer-read-only nil)
    ;; Subdir headlerline must come first because the first marker in
    ;; subdir-alist points there.
    (insert "  " dir ":\n")
    ;; Make second line a ``dir'' line in analogy to the ``total'' or
    ;; ``wildcard'' line.
    (insert "  " pattern_around_star "\n")
    ;; Start the dir process.
    (let ((proc (start-process
                 "dir"
                 (current-buffer)
                 "cmd"
                 "/c" "dir" "/b" "/s"
                 (concat (substitute ?\\ ?/ dir) pattern_around_star))))
      (set-process-filter proc (function w32-find-dired-filter))
      (set-process-sentinel proc (function w32-find-dired-sentinel))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) 1 (current-buffer))
      )
    (setq mode-line-process '(":%s"))
    )

  )

(define-key dired-mode-map [(control c)(+)] 'ignore)
(define-key dired-mode-map [(control c)(+)] 'wuxch-dired-create-directory)
(defun wuxch-dired-create-directory (directory)
  "Create a directory called DIRECTORY, with current date as suffix"
  (interactive
   (list (read-file-name "Create directory(with Date as suffix)): " (dired-current-directory))))
  (let ((expanded (directory-file-name (expand-file-name (concat directory (format-time-string "%Y%m%d"))))))
    (make-directory expanded)
    (dired-add-file expanded)
    (dired-move-to-filename)
    )
  )

(provide 'wuxch-dired)

;;; bookmark config file.
(require 'bm)
(require 'bookmark+)


;; 设置bookmark，可以使用VC类似的命令，如Ctrl-F2,F2,Meta-F2
(setq bm-restore-repository-on-load t)
(add-hook 'after-init-hook 'bm-repository-load)
;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)
;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)
;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
;; (add-hook 'kill-emacs-hook '(lambda nil
;;                               (bm-buffer-save-all)
;;                               (bm-repository-save)))
;; Update bookmark repository when saving the file.
(add-hook 'after-save-hook 'bm-buffer-save)
;; Restore bookmarks when buffer is reverted.
(add-hook 'after-revert-hook 'bm-buffer-restore)
;; 设置bookmark可以保存，命令bm-toggle-buffer-persistence(meta f2)用于切换是否永久保存
(setq bm-buffer-persistence t)
(global-set-key [(control f2)] 'bm-toggle)
(global-set-key [(f2)]   'bm-next)
(global-set-key [(shift f2)] 'bm-previous)
(global-set-key [(meta f2)] 'bm-toggle-buffer-persistence)

(setq bookmark-sort-flag nil)

(global-set-key [(control x)(l)] 'wuxch-bookmark-bmenu-list)
;; (define-key bookmark-bmenu-mode-map [tab] 'next-line)

(defun my-bookmark-bmenu-mode-hook-fun ()
  ""
  (define-key bookmark-bmenu-mode-map [(\2)] 'ignore)
  (define-key bookmark-bmenu-mode-map [(\1)] 'ignore)

  (define-key bookmark-bmenu-mode-map [(\0)] 'my-bookmark-open-key)
  (define-key bookmark-bmenu-mode-map [(\1)] 'my-bookmark-open-key)
  (define-key bookmark-bmenu-mode-map [(\2)] 'my-bookmark-open-key)
  (define-key bookmark-bmenu-mode-map [(\3)] 'my-bookmark-open-key)
  (define-key bookmark-bmenu-mode-map [(\4)] 'my-bookmark-open-key)
  (define-key bookmark-bmenu-mode-map [(\5)] 'my-bookmark-open-key)
  (define-key bookmark-bmenu-mode-map [(\6)] 'my-bookmark-open-key)
  (define-key bookmark-bmenu-mode-map [(\7)] 'my-bookmark-open-key)
  (define-key bookmark-bmenu-mode-map [(\8)] 'my-bookmark-open-key)
  (define-key bookmark-bmenu-mode-map [(\9)] 'my-bookmark-open-key)

  (define-key bookmark-bmenu-mode-map [return] 'ignore)
  (define-key bookmark-bmenu-mode-map [return] 'wuxch-bookmark-bmenu-this-window)

  (define-key bookmark-bmenu-mode-map [(control x)(i)] 'wuxch-bookmark-open-ie)

  (define-key bookmark-bmenu-mode-map [(e)] 'ignore)
  (define-key bookmark-bmenu-mode-map [(e)] 'wuxch-open-bookmark-for-edit)

  (define-key bookmark-bmenu-mode-map [(g)] 'ignore)
  (define-key bookmark-bmenu-mode-map [(g)] 'wuxch-bookmark-update)

  )

(add-hook 'bookmark-bmenu-mode-hook 'my-bookmark-bmenu-mode-hook-fun)

(defconst wuxch-bookmark-first-line-of-insert 3)
(defun my-bookmark-open-key ()
  "my-bookmark-open-key:"
  (interactive)
  (let ((line (string-to-number (this-command-keys))))
    (goto-line (+ wuxch-bookmark-first-line-of-insert line))
    (wuxch-bookmark-bmenu-this-window)
    )
  )

(defun my-bookmark-add-sequence ()
  ""
  (let ((sequence 0)(temp-save-buffer-read-only buffer-read-only))
    (setq buffer-read-only nil)
    (let ((temp-max-line (count-lines (point-min) (point-max)))(over-lay-pos-start)(over-lay-pos-end))
      (while (< sequence 10)
        (if (< (+ sequence wuxch-bookmark-first-line-of-insert) (+ temp-max-line 1))
            (progn
              (goto-line (+ sequence wuxch-bookmark-first-line-of-insert))
              (forward-char 2)
              (setq over-lay-pos-start (+ (point) 0))
              (insert (concat "[" (number-to-string sequence) "] "))
              (setq over-lay-pos-end (- (point) 1))
              (overlay-put (make-overlay over-lay-pos-start over-lay-pos-end) 'face 'highlight)
              )
          )
        (setq sequence (+ sequence 1))
        )
      )
    (setq buffer-read-only temp-save-buffer-read-only)
    (goto-line  wuxch-bookmark-first-line-of-insert)
    ;; 光标到[]里面。
    (forward-char 3)
    (set-buffer-modified-p nil)
    )
  )


(defun wuxch-bookmark-bmenu-list ()
  ""
  (interactive)
  (bookmark-bmenu-list)
  (if (interactive-p)
      (switch-to-buffer (get-buffer-create "*Bookmark List*"))
    (set-buffer (get-buffer-create "*Bookmark List*")))
  (my-bookmark-add-sequence)
  )

(defun wuxch-bookmark-bmenu-this-window ()
  ""
  (interactive)
  (bookmark-bmenu-this-window)
  (kill-buffer "*Bookmark List*")
  )

;; 共享的路径需要先用IE打开，以便于输入访问的用户名和密码。
(defun wuxch-bookmark-open-ie ()
  ""
  (interactive)
  (if (bookmark-bmenu-check-position)
      (progn
        (w32-shell-execute "open" (dired-replace-in-string "/" "\\" (bookmark-get-filename (bookmark-bmenu-bookmark))))
        (kill-buffer "*Bookmark List*")))
  )

(defun wuxch-open-bookmark-for-edit ()
  ""
  (interactive)
  (bookmark-save)
  ;; (message "bookmark file name is %s" bookmark-default-file)
  (find-file bookmark-default-file)
  )

(defun wuxch-bookmark-update ()
  ""
  (interactive)
  (bookmark-bmenu-surreptitiously-rebuild-list)
  (my-bookmark-add-sequence)
  (message "bookmark updated OK!")
  )


(defun wuxch-bookmark-store (name alist no-overwrite)
  "Store the bookmark NAME with data ALIST.
If NO-OVERWRITE is non-nil and another bookmark of the same name already
exists in `bookmark-alist', record the new bookmark without throwing away the
old one."
  (bookmark-maybe-load-default-file)
  (let ((stripped-name (copy-sequence name)))
    (or (featurep 'xemacs)
        ;; XEmacs's `set-text-properties' doesn't work on
        ;; free-standing strings, apparently.
        (set-text-properties 0 (length stripped-name) nil stripped-name))
    (if (and (bookmark-get-bookmark stripped-name) (not no-overwrite))
        ;; already existing bookmark under that name and
        ;; no prefix arg means just overwrite old bookmark
        (setcdr (bookmark-get-bookmark stripped-name) (list alist))

      ;; otherwise just cons it onto the front (either the bookmark
      ;; doesn't exist already, or there is no prefix arg.  In either
      ;; case, we want the new bookmark consed onto the alist...)
      ;; (message "bookmark-alias is %s" bookmark-alist)
      ;; (message "new list is :%s" (list stripped-name alist))
      ;; 需要改的地方
      ;; This is equivalent to (setq LISTNAME (cons NEWELT LISTNAME)).
      ;; (push (list stripped-name alist) bookmark-alist)

      (setq bookmark-alist (append bookmark-alist (list (list stripped-name alist))))


      ;; (message "bookmark-alias is %s" bookmark-alist)
      ;; Added by db
      (setq bookmark-current-bookmark stripped-name)
      (setq bookmark-alist-modification-count
            (1+ bookmark-alist-modification-count))
      (if (bookmark-time-to-save-p)
          (bookmark-save))

      (setq bookmark-current-bookmark stripped-name)
      (bookmark-bmenu-surreptitiously-rebuild-list)
      )
    )
  )

;; 使用自己的bookmark-store替换原来的，主要目的是使用append，不用原来的push
(defalias 'bookmark-store 'wuxch-bookmark-store)


(provide 'wuxch-bookmark)


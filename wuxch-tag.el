;;; configures and codes for tag.

;; 关于TAGS
;; 生成TAGS文件的方法：gfind . -regex ".*\.\(h\|c\|cpp\)" -exec etags -a -o "c:\Work\Code\TAGS" {} ;

;;(global-set-key [(f12)] 'visit-tags-table)
;; meta . 打开新窗口显示TAG
;; meta , 关闭打开的新窗口
(global-set-key [(meta .)] 'wuxch-find-tag-show-only)
(global-set-key [(meta \,)] 'delete-other-windows)

(require 'etags-select)
;; control . 跳转到新窗口显示TAG
;; control , 跳转回来
;; (global-set-key [(control .)] 'wuxch-find-tag)
(global-set-key [(control c)(.)] 'etags-select-find-tag-at-point)
(global-set-key [(control c)(\,)] 'pop-tag-mark)
(global-set-key [(control meta \,)] 'find-tag)

(defun append-tag-current-buffer-lisp()
  "append-tag-current-buffer-lisp:"
  (interactive)
  (let ((current-file (buffer-file-name))(tag-command))
    (make-elisp-tag nil current-file t)
    )
  )

(defun update-tag-current-buffer-lisp ()
  "update-tag-current-buffer-lisp:"
  (interactive)
  (wuxch-delete-tag-file)
  (append-tag-current-buffer-lisp)
  )

;; 生成TAGS文件的方法：gfind . -regex ".*\.\(h\|c\|cpp\)" -exec ctags -e -a -o "c:\Work\Code\TAGS" {} ;
(defun make-elisp-tag (is-dir dir-or-file-name run-in-background)
  "append elisp files of dir to my tag file"
  (let ((tag-command))
    (if is-dir
        (setq tag-command (build-tag-command-dir dir-or-file-name))
      (setq tag-command (build-tag-command-file dir-or-file-name))
      )
    (if run-in-background
        (wuxch-shell-command-background tag-command)
      (shell-command tag-command))
    )
  )

(defun build-tag-command-file (file-name)
  "build-tag-command-file :"
  (concat etags-exec-file SPACE etags-exec-para SPACE tag-file SPACE "\"" file-name "\"")
  )

(defun wuxch-delete-tag-file ()
  "wuxch-delete-tag-file:"
  (delete-file tag-file)
  )

(defun build-tag-command-dir (dir-name)
  "build-tag-command:"
  ;; (concat "gfind . -regex \".*\.\\(el\\)\" -exec etags -a -o " tag-file " {} ;")
  (concat find-exec-file SPACE dir-name SPACE find-elisp-file-para SPACE
          "-exec" SPACE etags-exec-file SPACE etags-exec-para SPACE tag-file SPACE "{} ;")
  )


(defun wuxch-find-tag-show-only ()
  "wuxch-find-tag-show-only:"
  (interactive)
  (wuxch-find-tag t)
  )

(defun wuxch-find-tag (&optional show-only)
  "Show tag in other window with no prompt in minibuf."
  (interactive)
  (let ((default (funcall (or find-tag-default-function
                              (get major-mode 'find-tag-default-function)
                              'find-tag-default))))
    (if show-only
        (progn (find-tag-other-window default)
               (shrink-window (- (window-height) 12)) ;; 限制为 12 行
               (recenter 1)
               (other-window 1))
      (find-tag default))))

;; (setq tags-file-name tag-file)

(defun list-current-file-tags ()
  "list-current-file-tags:"
  (interactive)
  (list-tags (buffer-file-name))
  )

(provide 'wuxch-tag)

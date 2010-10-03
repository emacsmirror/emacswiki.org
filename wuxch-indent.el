;;; do customized process when save file.

(defun indent-dwim ()
  "Indent function which current cursor belong to,
    remain cursor's posion instead of moving to the beginning of function.
	    --wuxch,04/04/2007 11:43:511 If code is emacs-lisp-mode,evaluate it. --wuxch 05/29/2007 16:40:29
    "
  (interactive)
  (set-register 'a (point))
  (let ((need-indent-region nil))
    (cond ((equal major-mode 'emacs-lisp-mode)
           (progn
             (mark-defun)
             (setq need-indent-region t)
             ))
          ((or (equal major-mode 'c-mode)
               (equal major-mode 'c++-mode)
               (equal major-mode 'java-mode))
           (progn
             (ignore-errors
               (c-mark-function))
             (setq need-indent-region t)
             ))
          ((equal major-mode 'nxml-mode)
           ;; (unless (string= (file-name-extension (buffer-file-name (current-buffer))) "xsl")
           (progn
             (while (re-search-forward "><" nil t)
               (replace-match ">\n<"))
             (mark-whole-buffer)
             (setq need-indent-region t)
             )
           ;; )
           )
          ((or (equal major-mode 'sgml-mode)
               (equal major-mode 'html-mode))
           (progn
             (mark-whole-buffer)
             (setq need-indent-region t)
             )
           )

          ((equal major-mode 'metapost-mode)
           (progn
             (meta-indent-defun)
             (setq need-indent-region t)))
          ;; ((equal major-mode 'latex-mode)
          ;;  (fill-paragraph t))
          )
    (if need-indent-region
        (progn  (indent-region (point) (mark))
                (if (equal major-mode 'emacs-lisp-mode)
                    (progn
                      (eval-region (point) (mark))
                      (message "indent and evaluate function OK!"))
                  (message "indent function OK!"))
                (goto-char (get-register 'a))
                (deactivate-mark)
                )
      (message "function can not be indented in mode:%s." major-mode)
      )
    )
  )

(global-set-key [(control x)(i)] 'indent-dwim)

(provide 'wuxch-indent)

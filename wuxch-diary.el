;;; staff about diary function.

(require 'diary-lib)

(defun wuxch-diary-hook ()
  (auto-fill-mode)
  (define-key diary-mode-map [return] 'ignore)
  (define-key diary-mode-map [return] 'wuxch-diary-new-line)
  )

(add-hook 'diary-mode-hook 'wuxch-diary-hook)

(defun wuxch-diary-show-all-entries ()
  ""
  (interactive)
  (diary-show-all-entries)
  (delete-window)
  (end-of-buffer)
  )


(defun wuxch-diary-new-line ()
  ""
  (interactive)
  (newline)
  (newline)
  )

(defun wuxch-calendar-mode-hook ()
  (define-key calendar-mode-map [(control c)(d)] 'ignore)
  (define-key calendar-mode-map [(control c)(d)] 'wuxch-diary-show-all-entries)
  (define-key calendar-mode-map [return] 'ignore)
  (define-key calendar-mode-map [return] 'wuxch-jump-to-diary)
  )

(add-hook 'calendar-mode-hook 'wuxch-calendar-mode-hook)


(defun wuxch-jump-to-diary()
  "test-diary-function:"
  (interactive)
  ;; (message "string is:%s" (calendar-date-string (calendar-cursor-to-date t) t t))
  (wuxch-locate-diary-entry-by-date (calendar-cursor-to-date t))
  )

(defun wuxch-locate-diary-entry-by-date (date)
  ""
  (find-file-other-window diary-file)
  (goto-char (point-min))

  (if  (eq (search-forward (calendar-date-string date t t) nil t) nil)
      (progn
        (goto-char (point-max))
        nil
        )
    t
    )
  )

(add-hook 'list-diary-entries-hook 'sort-diary-entries t)
(add-hook 'diary-display-hook 'fancy-diary-display)

(provide 'wuxch-diary)


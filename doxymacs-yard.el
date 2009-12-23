;;;
;; You can use this snippet to create YARD comments in Ruby buffers and
;; highlight them to the extent the YARD tags match with the Doxygen ones.
;; Any files opened in this buffer after a ruby file was loaded will retain
;; the settings, but this is still the least intrusive method I could find.
;; Another solution would be extending doxymacs itself with another style.

(add-hook 'ruby-mode-hook 'doxymacs-yard)

(defun doxymacs-yard ()
  (doxymacs-mode)
  (font-lock-add-keywords 'ruby-mode doxymacs-doxygen-keywords)
  ;; Only in effect for this buffer
  (make-variable-buffer-local 'doxymacs-file-comment-template)
  (make-variable-buffer-local 'doxymacs-blank-multiline-comment-template)
  (make-variable-buffer-local 'doxymacs-blank-singleline-comment-template)
  (make-variable-buffer-local 'doxymacs-function-comment-template)
  (make-variable-buffer-local 'doxymacs-member-comment-start)
  (make-variable-buffer-local 'doxymacs-member-comment-end)
  (make-variable-buffer-local 'doxymacs-group-comment-start)
  (make-variable-buffer-local 'doxymacs-group-comment-end)
  (make-variable-buffer-local 'doxymacs-parm-tempo-element)

  ;; The templates
  (setq doxymacs-file-comment-template '(
         "#" > n
         "# " (doxymacs-doxygen-command-char) "file   "
         (if (buffer-file-name)
             (file-name-nondirectory (buffer-file-name))
           "") > n
           "# " (doxymacs-doxygen-command-char) "author " (user-full-name)
           (doxymacs-user-mail-address)
           > n
           "# " (doxymacs-doxygen-command-char) "date   " (current-time-string) > n
           "# " > n
           "# " (doxymacs-doxygen-command-char) "brief  " (p "Brief description of this file: ") > n
           "# " > n
           "# " p > n
           "#" > n
           ))

  (setq doxymacs-blank-multiline-comment-templave  '("#" > n "# " p > n "#" > n))
  (setq doxymacs-blank-singleline-comment-template '("# " > p))

  (setq doxymacs-function-comment-template
        '((let ((next-func (doxymacs-find-next-func)))
            (if next-func
                (list
                 'l
                 "# " 'p '> 'n
                 "#" '> 'n
                 (doxymacs-parm-tempo-element (cdr (assoc 'args next-func)))
                 (unless (string-match
                          (regexp-quote (cdr (assoc 'return next-func)))
                          doxymacs-void-types)
                   '(l "#" > n "# " (doxymacs-doxygen-command-char)
                       "return " (p "Returns: ") > n))
                 "#" '>)
              (progn
                (error "Can't find next function declaraton.")
                nil)))))

                                        ; Called when inserting function comments
  (defun doxymacs-parm-tempo-element (parms)
    (if parms
        (let ((prompt (concat "Parameter " (car parms) ": ")))
          (list 'l "# " (doxymacs-doxygen-command-char)
                "param [mixed] " (car parms) " " (list 'p prompt) '> 'n
                (doxymacs-parm-tempo-element (cdr parms))))
      nil))

  (setq doxymacs-member-comment-start '("#< "))
  (setq doxymacs-member-comment-end '(""))

  (setq doxymacs-group-comment-start '("# @{"))
  (setq doxymacs-group-comment-end '("# @}"))
  )

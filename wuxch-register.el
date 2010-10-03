;;; store and get windows layout to and from registers

(defvar wuxch-perspective-list nil)
(defun wuxch-new-perspective ()
  "wuxch-new-perspective:"
  (interactive)
  (let ((perspective-name (buffer-name)))
    (set-register perspective-name (list (current-window-configuration) (point-marker)))
    (add-to-list 'wuxch-perspective-list perspective-name)
    (message "new perspective:%s" perspective-name)
    )
  )

(defun wuxch-next-perspective ()
  "wuxch-next-perspective:"
  (interactive)
  (if wuxch-perspective-list
      (let ((current-perspective (pop wuxch-perspective-list))
            (next-perspective)
            (main-buffer-point))
        ;; put current-perspective to list
        (setq wuxch-perspective-list (append wuxch-perspective-list (list current-perspective)))
        (setq next-perspective (car wuxch-perspective-list))
        (set-buffer next-perspective)
        ;; what we realy need is the windows layout, the point is not useful, use current point instead.
        (setq main-buffer-point (point))
        (jump-to-register next-perspective)
        (message "using perspective:%s" next-perspective)
        (goto-char main-buffer-point)
        )
    (message "perspective is nil!")
    )
  )

(defun wuxch-remove-recent-perspective ()
  "wuxch-remove-recent-perspective:"
  (interactive)
  (let ((current-perspective (pop wuxch-perspective-list)))
    (message "removing perspective:%s" current-perspective)
    )
  )

(defun wuxch-list-perspective ()
  "wuxch-list-perspective:"
  (interactive)
  (message "perspective list is:%s" wuxch-perspective-list)
  )

(global-set-key [(control f8)] 'wuxch-new-perspective)
(global-set-key [(f8)] 'wuxch-next-perspective)
(global-set-key [(meta f8)] 'wuxch-remove-recent-perspective)
(global-set-key [(control meta f8)] 'wuxch-list-perspective)

(provide 'wuxch-register)

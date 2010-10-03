;;; display information of current point

(global-set-key [(control c)(=)] 'ignore)
(global-set-key [(control c)(=)] 'wuxch-point-info)

(defun wuxch-point-info ()
  (interactive)
  (let* ((pos (point))
         (current-char (char-after pos)))
    (message "char[%s] ascii[%d,0x%x] pos[%d] line[%d] line-beg[%d] line-end[%d]"
             (char-to-string current-char)
             current-char
             current-char
             pos
             (line-number-at-pos)
             (line-beginning-position)
             (line-end-position)
             )
    )
  )

(provide 'wuxch-current-point)

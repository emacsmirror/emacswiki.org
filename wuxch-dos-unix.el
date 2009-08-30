;;; functions about dos and unix file format transport.

(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match ""))
  (goto-char (point-min))
  )

(defun unix-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n"))
  (goto-char (point-min))
  )

(defun ^m-buffer ()
  "Remove all ^M's from the buffer."
  (interactive)
  (^m-region (point-min) (point-max)))

(defun ^m-region (min max)
  "Remove all ^M's from the region."
  (interactive "r")
  (save-excursion
    (goto-char max)
    (while (re-search-backward "\C-m$" min t)
      (delete-char 1))))

(defalias '^m '^m-buffer)
;; with a few alii in case my fingers are too slow with the shift key:
(defalias '^M '^m-buffer)
(defalias '6m '^m-buffer)

(defun unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix t))
(defun dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos t))
(defun mac-file ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-mac t))

(provide 'wuxch-dos-unix)


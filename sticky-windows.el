;;; sticky-windows.el --- Make windows stay visible
;;;
;;; Commentary:
;;;
;;; `sticky-window-keep-window-visible' function marks a specific window as being dedicated with `set-window-dedicated-p'.
;;; However, that does not prevent that window from being deleted with `delete-window' or `delete-other-windows'.  Below are
;;; wrappers for `delete-window' and `delete-other-windows' that respect the "dedicated window" state, and are typically to be
;;; bound to the same keybindings as they are by default in Emacs as follows:
;;;
;;; (global-set-key     [(control x) (?0)]        'sticky-window-delete-window)
;;; (global-set-key     [(control x) (?1)]        'sticky-window-delete-other-windows)
;;;
;;; In addition, `sticky-window-keep-window-visible' might be bound to the currently unused C-x 9 key binding:
;;;
;;; (global-set-key     [(control x) (?9)]        'sticky-window-keep-window-visible)
;;;
;;; Change Log: (optional)

;;;###autoload
(defun sticky-window-keep-window-visible ()
  "Insure the buffer associated with the current window stays visible.
This is handy for ERC buffers where you would like to see the
conversation while you work in other windows within the frame. 
This is intended to be used with `sticky-window-delete-window'.
A prefix arg reverses this operation."
  (interactive)
  (set-window-dedicated-p (selected-window) (not current-prefix-arg)))

;;;###autoload
(defun sticky-window-delete-window ()
  "This is intended to be a replacement for `delete-window', but
that avoids deleting windows that have been marked as dedicated
with `sticky-window-keep-window-visible'."
  (interactive)
  (let ((window (selected-window)))
	(if (and (not current-prefix-arg) (window-dedicated-p window))
		(error "This is a dedicated window. Use C-u prefix on this keybinding to really delete it.")
	  (set-window-dedicated-p (selected-window) nil)
	  (delete-window window))))

;;;###autoload
(defun sticky-window-delete-other-windows ()
  "Delete all other windows that are not marked to be visible with `sticky-window-keep-window-visible'."
  (interactive)
  (mapcar (lambda (window)
			(if (not (window-dedicated-p window))
				(delete-window window)))
		  (cdr (window-list))))

(provide 'sticky-windows)

;;; sticky-windows.el ends here

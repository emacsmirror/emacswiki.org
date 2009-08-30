;;; A simple addon for emacs to focus a window in a frame similar to how Blender (http://www.blender.org) does it.
;;; If you like it take it, use it, abuse it, but please attribute me.
;;; Author: Krister Svanlund

(defvar previous-window-layout nil
  "This variable contains the old window configuration.")

(defvar frame-focused nil
  "non-nil if a frame currently is focused.")

(defun is-focused()
  (save-window-excursion
	(setq curr-layout (current-window-configuration))
	(delete-other-windows)
	(compare-window-configurations curr-layout (current-window-configuration))))

(defun save-window-layout()
  "Save the current layout."
  (interactive)
  (setq previous-window-layout (current-window-configuration))
  (setq frame-focused t)
  (message "Current layout saved."))

(defun restore-window-layout()
  "Restore previous layout."
  (interactive)
  (remove-hook 'window-configuration-change-hook 'restore-window-layout)
  (if frame-focused
	  (if previous-window-layout
		  (let ()
			(set-window-configuration previous-window-layout)
			(setq previous-window-layout nil)
			(setq frame-focused nil)
			(message "Old layout restored."))
		(message "No old frame stored."))
	(message "Frame is not focused.")))

(defun focus-frame()
  "Focus the current frame."
  (interactive)
  (if frame-focused
	  ;; Unfocus
	  (if previous-window-layout
		(restore-window-layout)
		(let ()
		  (message "No previous layout.")
		  (setq frame-focused nil)))
	;; Focus
	(let ()
	  (if (is-focused)
		  (message "Windows fills frame.")
		  (let ()
			(setq frame-focused t)
			(save-window-layout)
			(delete-other-windows)
			(add-hook 'window-configuration-change-hook 'restore-window-layout)
			(message "Focus current frame."))))))

(provide 'bfocus)

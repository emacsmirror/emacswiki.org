;;; man-page-fix.el
;; man-page-fix.el is free software
;; Fix so that man-pages display nicely when the window-width is less than 
;; the frame width. 
(global-set-key [(control ?c) ?m]
				(lambda()
				  (interactive)
				  (other-window '1)
				  (setq pref-man-width 80)
				  (if (> pref-man-width (window-width))
					  (setq pref-man-width (window-width)))
				  ;; This should make it so the pages break at end of window.
				  ;; I.e.: MANWIDTH=50 man man
				  (setenv "MANWIDTH" (format "%d" pref-man-width))
				  (other-window '-1)
				  (if (equal (word-at-point) nil) 
					  (call-interactively 'manual-entry)
					(manual-entry (word-at-point)))
				  )
				)
;;; man-page-fix.el ends here

;;; rcirc-auto-away.el

(provide 'rcirc-auto-away)

(defvar rcirc-auto-away-server-regexps nil
  "List of regexps to match servers for auto-away.")

(defvar rcirc-auto-away-after 3600
  "Auto-away after this many seconds.")

(defvar rcirc-auto-away-reason "idle"
  "Reason sent to server when auto-away.")

(defun rcirc-auto-away ()
  (message "rcirc-auto-away")
  (rcirc-auto-away-1 rcirc-auto-away-reason)
  (add-hook 'post-command-hook 'rcirc-auto-unaway))

(defun rcirc-auto-away-1 (reason)
  (let ((regexp (mapconcat (lambda (x) (concat "\\(" x "\\)")) 
			   rcirc-auto-away-server-regexps "\\|")))
    (dolist (process (rcirc-process-list))
      (when (string-match regexp (process-name process))
	(rcirc-send-string process (concat "AWAY :" reason))))))

(defun rcirc-auto-unaway ()
  (remove-hook 'post-command-hook 'rcirc-auto-unaway)
  (rcirc-auto-away-1 ""))

(run-with-idle-timer rcirc-auto-away-after t 'rcirc-auto-away)
;;(cancel-function-timers 'rcirc-auto-away)

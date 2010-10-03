;;; shortcut to hex mode
(defun hex ()
  ""
  (interactive)
  (hexl-mode)
  (message "Use Control-C Control-C to go back")
  )

(provide 'wuxch-hex)

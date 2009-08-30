;;; gmail-message-send-mail.el - advice for that function to use gmail smtp when appropriate

(defadvice message-send-mail (around gmail-message-send-mail protect activate)
  "Set up SMTP settings to use Gmail's server when mail is from a gmail.com address."
  (interactive "P")
  (if (save-restriction
        (message-narrow-to-headers)
        (string-match "gmail.com" (message-fetch-field "from")))

      (let ((message-send-mail-function 'smtpmail-send-it)
            ;; gmail says use port 465 or 587, but 25 works and those don't, go figure
            (smtpmail-starttls-credentials '(("smtp.gmail.com" 25 nil nil)))
            (smtpmail-auth-credentials '(("smtp.gmail.com" 25 "username@gmail.com" nil)))
            (smtpmail-default-smtp-server "smtp.gmail.com")
            (smtpmail-smtp-server "smtp.gmail.com")
            (smtpmail-smtp-service 25)
            (smtpmail-local-domain "yourdomain.com"))
        ad-do-it)
    ad-do-it))

;; end of gmail-message-send-mail.el

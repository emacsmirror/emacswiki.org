Here is a sample ~/.wl config file for WanderLust with some useful snippets.
<pre>

(setq wl-summary-toggle-mime "mime")
(require 'mime-w3m)
(setq mime-edit-split-message nil)
(setq wl-draft-reply-buffer-style 'full)

(autoload 'wl-user-agent-compose "wl-draft" nil t) 
  (if (boundp 'mail-user-agent)                      
      (setq mail-user-agent 'wl-user-agent))         
  (if (fboundp 'define-mail-user-agent)              
      (define-mail-user-agent                        
        'wl-user-agent                               
        'wl-user-agent-compose                       
        'wl-draft-send                               
        'wl-draft-kill                               
        'mail-send-hook))   

(setq elmo-imap4-default-authenticate-type 'clear)

(setq wl-smtp-posting-server "smtp.mail.com")
(setq wl-message-id-domain "smtp.mail.com")
(setq wl-from "Jim Burton <j.burton@mail.com>")
(setq wl-user-mail-address-list
      (list (wl-address-header-extract-address wl-from)
            "Jim Burton <jim@email2.com>"))
(setq wl-local-domain "smtp.mail.com")

(setq wl-draft-always-delete-myself t)
(setq wl-fcc "+~/Mail/outbox/out")

;;; @ bbdb
(setq mime-bbdb/use-mail-extr nil)
(require 'bbdb-wl)
(bbdb-wl-setup)
(require 'mime-bbdb)

(autoload 'bbdb         "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-name    "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-company "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-net     "bbdb-com" "Insidious Big Brother Database" t)

(add-hook 'wl-mail-setup-hook 'bbdb-insinuate-sendmail)

(setq bbdb-use-alternate-names t)

(setq bbdb-file "~/.bbdb") 
(setq bbdb/mail-auto-create-p   'bbdb-ignore-some-messages-hook)

;; add record to .bbdb manually
(setq bbdb-new-nets-always-primary t)
(setq bbdb/mail-auto-create-p nil) 

;;; height of BBDB's window
(setq bbdb-pop-up-target-lines 7)

;;; popup display of record in the .bbdb
(setq bbdb-use-pop-up t)
(setq signature-use-bbdb t)
(setq bbdb-north-american-phone-numbers-p nil)

;;; automatic adding to ML field
(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)

;;; hide bbdb field while wl-folder-suspend
(defadvice wl-folder-suspend (after wl-bbdb-suspend activate compile)
  (interactive)
(bbdb-wl-exit-2))
(defadvice wl-exit (after wl-bbdb-suspend activate compile)
  (interactive)
(bbdb-wl-exit-2))

(put 'ML 'field-separator "\n")
(put 'User-Agent 'field-separator "\n")

(setq bbdb-auto-notes-alist
       '(
       ("X-ML-Name" (".*$" ML 0))
       ("X-Mailinglist" (".*$" ML 0))
       ("X-Ml-Name" (".*$" ML 0))
       ("X-Mailer" (".*$" User-Agent 0))
       ("X-Newsreader" (".*$" User-Agent 0))
       ("User-Agent" (".*$" User-Agent 0))
       ("X-Face" ("[ \t\n]*\\([^ \t\n]*\\)\\([ \t\n]+\\([^ \t\n]+\\)\\)?\\([ \t\n]+\\([^ \t\n]+\\)\\)?\\([ \t\n]+\\([^ \t\n]+\\)\\)?"
                                 face "\\1\\3\\5\\7"))
       ))

;; expiry
(setq wl-expire-alist                                                       
        '(("^\\+trash$"   (date 14) remove)                                   
                                    ;; delete                                 
          ("^\\+tmp$"     (date 7) trash)                                     
                                    ;; re-file to wl-trash-folder             
          ("^\\%inbox"    (date 30) wl-expire-archive-date)                   
                             ;; archive by year and month (numbers discarded)
			     ))

;;auto-fill 
(setq mime-edit-mode-hook
      '(lambda ()
      	 (auto-fill-mode 1)))

(setq wl-message-visible-field-list '("^To" "^Subject" "^From" "^Date" "^Cc"))
(setq wl-message-ignored-field-list '("^"))

;;look in zip files as if they were folders
(setq elmo-archive-treat-file t)

;;show sent mail by who it was to
(setq wl-summary-showto-folder-regexp ".*")
(setq wl-summary-from-function 'wl-summary-default-from)

;; refiling

(setq wl-refile-rule-alist
      '((("To" "Cc")
         ("^wl-en@lists.airs.net" . "+mlists"))))
</pre>

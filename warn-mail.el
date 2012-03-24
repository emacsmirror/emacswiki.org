;;; warn-mail.el --- warn for mails incoming
;;

;; Author: Thierry Volpiatto
;; Contact: thierry dot volpiatto from gmail dot com

;; Copyright 2008 Thierry Volpiatto

;; This file is part of warn-mail.

;; warn-mail is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; warn-mail is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with warn-mail; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

;	$Id: warn-mail.el,v 1.2 2008/01/18 12:31:39 thierry Exp $	


;;; Commentary:

;; Check the size of all files in a repertory
;; and warn if a file is bigger than 0
;; launch tv-launch-mail-system if you want to start fetchmail
;; and then warn-mail.
;; if your system is always running fetchmail from boot
;; run only tv-launch-warn-without-fetch 
;; Quick start:(set to your convenience)

;; Copy the file somewhere in your path
;; add these lines to your .emacs

;; (require 'warn-mail)
;; (tv-launch-warn-without-fetch)

;; set mail-list-to-watch in customize or ex:
;; (setq mail-list-to-watch (list 
;;			     "/home/you/incoming/default"
;;                           "/home/you/incoming/friends"))

;; If you want to be notified in the mode-line, you have to set in .emacs or customize:
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
;; (display-time)
;; (setq message-mode-line t)

;; If you want an icon to show when there is no mails add
;; (setq display-time-use-mail-icon t) 

;;keybindings you can use:

;; (global-set-key (kbd "C-c f m") 'tv-launch-mail-system)
;; (global-set-key (kbd "C-c q f m") 'tv-stop-warn-and-fetch)

;; You will find all the config in customize==>applications==>mail==>|display-time
;;                                                                   |warn-mail 


;;; code:

(defgroup warn-mail nil
  "warn-mail provides a system to warn for mails incoming"
  :group 'mail)

(defcustom mail-list-to-watch
  nil
  "List of files to look for in the Mail directory"
  :type '(repeat string)
  :group 'warn-mail)

(defcustom start-warn-mail-in-time
  "5"
  "In how much time mail-warn will start ; double-quoted it
if you set it in .emacs, it's a string"
  :type 'string
  :group 'warn-mail)

(defcustom repeat-warn-mail-every
  120
  "Check mail every n seconds (it's an integer: ex 120)"
  :type 'integer
  :group 'warn-mail)

(defcustom message-new-mails
  "You have new mails!"
  "message showed when you have new mails"
  :type 'string
  :group 'warn-mail)

(defcustom message-no-more-mails
  "No more mails!"
  "Message showed when you have no mails"
  :type 'string
  :group 'warn-mail)

(defcustom fetchmail-command
  "/usr/bin/fetchmail"
  "path to fetchmail command"
  :type 'string
  :group 'warn-mail)

(defcustom message-time
  1
  "The message is erased after 1 seconds"
  :type 'integer
  :group 'warn-mail)

(defcustom message-mode-line
  nil
  "if non nil display the message in the mode line"
  :type 'boolean
  :group 'warn-mail)

(defcustom message-mode-line-icon
  nil
  "if non nil display an icon in mode-line when no mails
the variable display-time-use-mail-icon 
must be set to t in .emacs or customize"
  :type 'boolean
  :group 'warn-mail)

(defvar session-warn-mail
  nil
  "this must exist to stop fetchmail if
emacs have been quit and restarted")

;;;;;;;;

(defun test-existing-mf (inlist)
  "test if mailbox exist"
  (setq realmflist ())
  (dolist (i inlist)
    (and (file-exists-p i)
	 (push i realmflist)))
  realmflist)

(defun test-size-mf-and-warn (liste)
  "check the size of each mailbox and add this
box to a list if it is bigger than 0 ; if mail, display a message
in mini-buffer or mode-line depending on the value of message-mode-line"
  (setq limr ()) 
  (dolist (i liste)
    (if (> (nth 7 (file-attributes i 'string)) 0)
	(push i limr)))
  (if (> (length limr) 0)
      (progn
	(if message-mode-line
	    (if display-time-use-mail-icon
		(progn
		  (setq display-time-use-mail-icon nil)
		  (setq display-time-mail-string message-new-mails))
	      (setq display-time-mail-string message-new-mails))
	  (progn
	    (message "%s" message-new-mails)
	    (sit-for message-time)
	    (message nil))))
    (progn
      (if message-mode-line
	  (if message-mode-line-icon
	      (setq display-time-use-mail-icon t)
	    (setq display-time-mail-string message-no-more-mails))
	(progn
	  (message "%s" message-no-more-mails)
	  (sit-for message-time)
	  (message nil))))))

(defun tv-warn-mail (inlist)
  "start warn-mail"
  (let ((outlist (test-existing-mf inlist)))
  (test-size-mf-and-warn outlist)
  (setq outlist ())))

(defun test-ps ()
  "test if fetchmail is alive"
  (if (not (equal (shell-command-to-string (format "ps -u %s | grep fetchmail" user-login-name)) ""))
      t
    nil))

(defun tv-launch-warn-without-fetch ()
  "For a system that launch fetchmail on boot"
  (interactive)
  (setq session-warn-mail (run-with-timer 
			   start-warn-mail-in-time repeat-warn-mail-every 
			   (quote tv-warn-mail) mail-list-to-watch)))

(defun tv-launch-mail-system ()
  "If fetchmail is alive don't try to start it and
launch only timer else start fetchmail and timer"
  (interactive)
  (or (and (test-ps)
	   (tv-launch-warn-without-fetch))
  (and (shell-command fetchmail-command)
       (tv-launch-warn-without-fetch))))

(defun tv-stop-warn-mail ()
  "Stop warning for new mails but leave fetchmail alive"
  (interactive)
  (when session-warn-mail
    (message "Warn-mail stopped!")
    (sit-for message-time)
    (message nil)
    (cancel-timer session-warn-mail)
    (setq session-warn-mail nil)
    t))

(defun tv-stop-warn-and-fetch ()
  "Stop warning for new mails and stop fetchmail
with the shell-command fetchmail -q"
  (interactive)
  (if session-warn-mail
      (and (tv-stop-warn-mail)
	   (and (test-ps)
		(shell-command (format "%s -q" fetchmail-command))))
    (and (test-ps)
	 (shell-command (format "%s -q" fetchmail-command)))))
    

(provide 'warn-mail)

;;;warn-mail.el ends here


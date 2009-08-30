;;; popbiff2.el --- Checks for mail using POP3
;; Copyright (C) 1996 by Free Software Foundation, Inc.
;; Copyright (C) 2004-2005 Frederik Fouvry

;; Author: Dominique de Waleffe <ddw@acm.org>
;;         Updated by Frederik Fouvry <fouvry@coli.uni-sb.de>
;; Keywords: mail, tools

;; This file is free software.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file checks for mail using the POP3 protocol.
;;
;; Usage:
;; In your .emacs put
;;
;;  (require 'popbiff)
;;
;; or
;; 
;; (autoload 'popbiff-mode "popbiff")
;; (autoload 'popbiff-display-time-mail-function "popbiff")
;;
;; and then 
;;
;;  (popbiff 1)
;;
;; To show the mail icon in the mode line:
;; 
;; (setq display-time-mail-function 'popbiff-display-time-mail-function)
;; 
;; Or set it in Customize.


;;; History:
;; 2005-4-20: Inserted the history.  The mode is a modification and
;;            update of Dominique de Waleffe's popbiff.el (now using
;;            customize, and pop3 utilities provided by Emacs).  In
;;            fact, everything has been rewritten (only in hindsight
;;            from scratch).

;;; Code:

(require 'pop3)
(require 'rmail)

(defsubst popbiff-cancel-timer ()
  "Cancel `popbiff-timer'."
  (setq popbiff-timer (when popbiff-timer
			(cancel-timer popbiff-timer))))

(defsubst popbiff-initialise-timer ()
  "Initialise `popbiff-timer'."
  (popbiff-cancel-timer)
  (when (> popbiff-check-frequency 0)
    (setq popbiff-timer
	  (run-at-time nil (* 60 popbiff-check-frequency)
		       'popbiff-check-messages))))

; TODO: update modeline after switching off mode
;;;###autoload
(define-minor-mode popbiff-mode
  "Check for new mail using the POP3 protocol.
The mode sets up a connection every `popbiff-check-frequency'
minutes, and reports the number of mails in the POP3 mailbox."
  :group 'mail
  :global t
  :version "22.0"
  (if popbiff-mode
      (progn
	(add-hook 'rmail-after-get-new-mail-hook 'popbiff-check-messages)
	(popbiff-initialise-timer))
    (progn
      (remove-hook 'rmail-after-get-new-mail-hook 'popbiff-check-messages)
      (popbiff-cancel-timer)
      (setq popbiff-new-messages -1)))
  popbiff-mode)


(defcustom popbiff-check-frequency (or (getenv "MAILCHECK") 15)
  "*Frequency of mail check \(in minutes).
A value of `0' switches off the check \(but leaves the mode on)."
  :type 'number
  :group 'popbiff
  :set (lambda (sym val)
	 (set-default sym val)
	 (popbiff-initialise-timer)))

;; Is assumed to be nil or `timerp'
(defvar popbiff-timer nil)

(defvar popbiff-new-messages -1)

;; For `display-time-mail-function'
;; TODO: Make it possible to check all mail files (set-rmail-primary-inbox-list)
;;       - Is not quite a task for popbiff ...
;;;###autoload
(defun popbiff-display-time-mail-function ()
  "Return t if there are messages in the POP3 mailbox."
  (or (> popbiff-new-messages 0)))

;; add tunnel option
;; combine matching options into one
(defun popbiff-check-messages ()
  "Set `popbiff-new-messages' to the number of messages in the POP3 mailbox."
  (when popbiff-mode
    (let* ((rmail-parsed-url
	    (if (and (fboundp 'rmail-parse-url)
		     (stringp (getenv "MAIL")))
		(rmail-parse-url (getenv "MAIL"))
	      ;; return format of `rmail-parse-url'
	      (list "" nil pop3-password nil)))
	   (pop3-password
	    (cond ((nth 2 rmail-parsed-url))
		  ((save-match-data (string-match "^po:" (car rmail-parsed-url)))
		   (rmail-get-remote-password nil))
		  (t
		   (require 'ange-ftp)
		   (ange-ftp-get-passwd pop3-mailhost pop3-maildrop)))))
      (when (and (fboundp 'ssh-forward-forward-port)
		 (ssh-forward-forward-port 11110 pop3-mailhost pop3-port))
	(while (not (ssh-forward-port-available-p 11110)) (sit-for 0.08))
	(setq pop3-port 11110
	      pop3-mailhost "localhost"))
      (setq popbiff-new-messages (pop3-get-message-count)))))

(provide 'popbiff)

;;; popbiff2.el ends here

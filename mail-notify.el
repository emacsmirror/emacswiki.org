;;; mail-notify.el --- Notify you when have new mail.

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-06-17 09:32:37
;; Version: 1.0
;; Last-Updated: 2008-06-17 09:32:39
;; URL:  http://www.emacswiki.org/emacs/download/mail-notify.el
;; Keywords: mail, notify
;; Compatibility: GNU Emacs 23.0.60.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Features that might be required by this library:
;;
;;
;;

;;; Installation:
;;
;; Copy mail-notify.el to your load-path and add to your ~/.emacs
;;
;;  (require 'mail-notify)
;;
;; Startup mail-notify timer, add blow in your ~/.emacs
;; (run-with-timer 0 mail-notify-repeat 'mail-notify)
;;
;; Set new mail storage directory
;; (setq mail-notify-directory "YourNewMailDirectory")

;;; Commentary:
;;
;; This package is for notify you when have new mail.
;; And it just do a sentinel for mail receive directory.
;; When find have new file in receive directory, and notify you.
;;
;; This extension use command `notify-send' to notify.
;; So make you have install `notify-send' in your system.
;;

;;; Change log:
;;
;; 2008/06/17
;;         First release.
;;

;;; Acknowledgments:
;;
;; Not yet
;;

;;; TODO
;;
;; None
;;

;;; Code:

(defgroup mail-notify nil
  "Mail notify"
  :group 'mail)

(defcustom mail-notify-repeat 60
  "Default repeat seconds for check new mail."
  :type 'number
  :group 'mail-notify)

(defcustom mail-notify-directory nil
  "The check directory that new mail storage."
  :type 'string
  :group 'mail-notify)

(defcustom mail-notify-command "notify-send"
  "The command of mail notify."
  :type 'string
  :group 'mail-notify)

(defcustom mail-notify-message "You have a new mail."
  "The message of mail notify."
  :type 'string
  :group 'mail-notify)

(defcustom mail-notify-icon "~/MyEmacs/Image/NewMail.png"
  "The icon of mail notify."
  :type 'string
  :group 'mail-notify)

(defcustom mail-notify-timeout 10000
  "The time out of mail notify."
  :type 'number
  :group 'mail-notify)

(defcustom mail-notify-pre-hook nil
  "The hook run before do `mail-notify'."
  :type 'hook
  :group 'mail-notify)

(defvar mail-current-count 0
  "Count current new mail number.")

(defvar mail-notify-status t
  "Default open mail notify status.")

(defun mail-notify()
  "Notify you when have new mail."
  (interactive)
  (let* ((mail-count (length (directory-files mail-notify-directory))))
    (if (> mail-count 2)                ;2 total number of '.' and '..'
        (progn
          ;; Run pre hook
          (run-hooks 'mail-notify-pre-hook)
          (setq display-time-use-mail-icon t)
          (when (> mail-count (+ mail-current-count 2))

            (setq mail-current-count (- mail-count 2))
            (shell-command (concat
                            mail-notify-command     ;command
                            " -i " mail-notify-icon ;icon
                            " -t " (int-to-string   ;timeout
                                    mail-notify-timeout)
                            " -- "                ;parse-line
                            (format " 'Hey, %s!'" ;name
                                    (user-login-name))
                            (format " '\n   %s'" ;message
                                    mail-notify-message)
                            ))))
      (setq mail-current-count 0)
      (setq display-time-use-mail-icon nil)
      (setq display-time-mail-string ""))))

(run-with-timer 0 mail-notify-repeat 'mail-notify) ;startup mail notify

(defun mail-notify-toggle ()
  "Toggle mail notify status."
  (interactive)
  (if mail-notify-status
      (progn
        (setq mail-notify-status nil)
        (message "Mail notify closed"))
    (setq mail-notify-status t)
    (message "Mail notify opened")))

(provide 'mail-notify)

;;; mail-notify.el ends here

;;; LocalWords:  YourNewMailDirectory pre login

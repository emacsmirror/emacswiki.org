;;; rcirc-notify+.el --- Rcirc notify library

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-06-08 12:45:24
;; Version: 1.1
;; Last-Updated: 2018-09-16 21:32:24
;; URL:  http://www.emacswiki.org/emacs/download/rcirc-notify+.el
;; Keywords: rcirc, notify
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
;;  `rcirc'
;;

;;; Commentary:
;;
;; Notify popup for rcirc
;;
;; This extension use `notify-send' for notify.
;; So make you have install `notify-send' in your system.
;;

;;; Installation:
;;
;; Copy rcirc-notify+.el to your load-path and add to your ~/.emacs
;;
;;  (require 'rcirc-notify+)
;;
;; Rcirc will notify you automatically when have a message is reach, blow is open
;; rcirc notify switcher:
;;  (setq rcirc-notify+-open t)
;;
;; Little tips:
;;  Function `rcirc-notify+-jump-last-message-channel' can jump last channel that
;;  message notify you.
;;  And feel free to binding it to you like. ^_^
;;

;;; Change log:
;;
;; 2019/09/16
;;      * Support MacOS now.
;;
;; 2008/06/08
;;         * First release.
;;

;;; Acknowledgments:
;;
;;      Will Farrington     for rcirc-notify+.el
;;

;;; TODO
;;
;; None
;;

;; Require
(require 'rcirc)

;;; Code:
(defgroup rcirc-notify+ nil
  "Notify popup for ircirc."
  :group 'rcirc)

(defcustom rcirc-notify+-open t
  "The switcher that notify me."
  :type 'boolean
  :group 'rcirc-notify+)

(defcustom rcirc-notify+-delay 1
  "Number of seconds that will elapse between notifications from the same person."
  :type 'integer
  :group 'rcirc-notify+)

(defcustom rcirc-notify+-cmd "notify-send"
  "The command that use for notify.

This option just for linux, MacOS user don't need this."
  :type 'string
  :group 'rcirc-notify+)

(defcustom rcirc-notify+-icon "/usr/share/deepin-emacs/Image/Irc.png"
  "Specifies an icon filename or stock icon to display.

This option just for linux, MacOS user don't need this."
  :type 'string
  :group 'rcirc-notify+)

(defcustom rcirc-notify+-timeout 10000
  "Specifies the timeout in milliseconds at which to expire the notification.

This option just for linux, MacOS user don't need this."
  :type 'number
  :group 'rcirc-notify+)

(defcustom rcirc-notify+-urgency "low"
  "Specifies the urgency level (low, normal, critical).

This option just for linux, MacOS user don't need this."
  :type 'string
  :group 'rcirc-notify+)

(defcustom rcirc-notify+-category "im.received"
  "Specifies the notification category.

This option just for linux, MacOS user don't need this."
  :type 'string
  :group 'rcirc-notify+)

(defvar rcirc-notify+-nick-alist nil
  "An alist of nicks and the last time they tried to trigger a notification.")

(defvar rcirc-last-position nil
  "The last message position in rcirc buffers.")

(defun rcirc-notify+ (sender &optional private)
  "Rcirc notify."
  (interactive)
  (let ((last-channel nil)
        (last-server nil))
    (when (and rcirc-notify+-open       ;if notify switcher is open
               rcirc-target) ;if is a null channel (ignore the first message from server)
      (setq last-channel rcirc-target) ;get channel name or use name (from private message)
      (setq last-server (with-rcirc-server-buffer rcirc-server-name)) ;get random server name
      (string-match "^[^.]*" last-server) ;replace random server name use string @irc
      (setq last-server (replace-match "@irc" nil nil last-server 0))
      (setq rcirc-last-position (concat last-channel last-server)) ;general irc buffer name that last message to me
      (if private
          (rcirc-notify+-popup (format "%s send a private message." sender))
        (rcirc-notify+-popup (format "%s send message." sender))))))

(defun rcirc-notify+-popup (msg)
  "Popup notify window."
  (if (featurep 'cocoa)
      (ns-do-applescript (format "display notification \"%s\"" msg))
    (shell-command (concat rcirc-notify+-cmd
                           " -i " rcirc-notify+-icon
                           " -t " (int-to-string
                                   rcirc-notify+-timeout)
                           " -u " rcirc-notify+-urgency
                           " -c " rcirc-notify+-category
                           " -- "
                           " \"" rcirc-last-position "\""
                           " \""
                           (if (boundp 'msg)
                               msg "")
                           "\""))))

(defun rcirc-notify+-jump-last-message-channel()
  "Jump to last message that call you."
  (interactive)
  (if rcirc-last-position
      (switch-to-buffer rcirc-last-position)
    (cycle-buffer-in-special-mode 'rcirc-mode)))

(defun rcirc-notify+-toggle()
  "Toggle rcirc notify."
  (interactive)
  (if rcirc-notify+-open
      (progn
        (setq rcirc-notify+-open nil)
        (message "Closed IRC Notify"))
    (setq rcirc-notify+-open t)
    (message "Opened IRC Notify")))

(defun rcirc-notify+-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`rcirc-notify+-delay'."
  (unless delay (setq delay rcirc-notify+-delay))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick rcirc-notify+-nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) rcirc-notify+-nick-alist)
      t)))

(defun rcirc-notify+-me (proc sender response target text)
  "Notify the current user when someone sends a message that matches a regexp in `rcirc-keywords'."
  (interactive)
  (when (and (string-match (rcirc-nick proc) text)
             (not (string= (rcirc-nick proc) sender))
             (not (string= (rcirc-server-name proc) sender))
             (rcirc-notify+-allowed sender))
    (rcirc-notify+ sender nil)))

(defun rcirc-notify+-privmsg (proc sender response target text)
  "Notify the current user when someone sends a private message to them."
  (interactive)
  (when (and (string= response "PRIVMSG")
             (not (string= sender (rcirc-nick proc)))
             (not (rcirc-channel-p target)))
    (rcirc-notify+ sender t)))

(add-hook 'rcirc-print-hooks 'rcirc-notify+-privmsg)
(add-hook 'rcirc-print-hooks 'rcirc-notify+-me)


(provide 'rcirc-notify+)

;;; rcirc-notify+.el ends here

;;; LocalWords:  Farrington cmd im msg privmsg

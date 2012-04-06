;;;; rcirc-notify.el -- libnotify popups
;; Copyright (c) 2008 Will Farrington
;; Copyright (c) 2009, 2011 Alex Schroeder <alex@gnu.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;;; Changelog:
;; * 2012/04/06 - Fix bug breaking PRIVMSG notifications,
;;                indicate each of the keywords mentioned, and
;;                implement settings as customizations. -Ken Manheimer
;;
;; * 2011/04/13 - Support for Growl on Windows; support for
;;                rcirc-keywords.
;;
;; * 2011/01/31 - Fix two warnings -Stefan Kangas
;;
;; * 2009/10/17 - Added support for osascript which is a Mac OS X
;;                what Mac OS 10.4 and Growl 1.1.6 require.
;;
;; * 2009/02/23 - Added support for growlnotify which is a Mac OS X
;;                notification tool.  http://growl.info -Shane Celis
;;
;; * 2008/12/29 - Fix annoying bug where the user gets notified
;;                for every PRIVMSG and added new variable specifying
;;                format of message when a PRIVMSG is received
;;
;; * 2008/02/11 - Fix an annoying bug where the user got
;;                notified every message
;;
;; * 2008/02/11 - First release
;;
;;;; Commentary:
;;
;; This code is inspired in part by erc-page-me.el and offers
;; the same functionality as it, but for rcirc.
;;
;;
;; * `my-rcirc-notify-message` contains the message contents for
;;    the notification
;;
;; * `my-rcirc-notify-message-private` contains the message
;;    contents for a private message notification
;;
;; * `my-rcirc-notify-nick-alist` is a list containing the last
;;    folks who notified you, and what times they did it at
;;
;; * `my-rcirc-notify-timeout` controls the number of seconds
;;    in between notifications from the same nick.

;; Grow For Windows
;; Run something like this from eshell before you use rcirc-notify:
;; /Programme/Growl\ for\ Windows/growlnotify.com /t:IRC \
;; /ai:http://www.emacswiki.org/pics/static/CarbonEmacsPackageIcon.png \
;; /a:Emacs /r:IRC /n:IRC foo

(require 'rcirc)

(defgroup rcirc-notify nil
  "Libnotify popups for rcirc irc mode."
  :group 'rcirc)

(defcustom my-rcirc-notify-message "%s mentioned you: %s"
  "Format of the message to display in the popup.
The first %s will expand to the nick that notified you,
the second %s (if any) will expand to the message text itself."
  :type 'string
  :group 'rcirc-notify)

(defcustom my-rcirc-notify-keywords t
  "Non-nil means matches of `rcirc-keywords' will result in notification.
See `my-rcirc-notify-keyword' for the message format to use."
  :type 'boolean
  :group 'rcirc-notify)

(defcustom my-rcirc-notify-keyword "%s mentioned the keyword(s) %s: %s"
  "Format of the message to display in the popup.
The first %s will expand to the nick that mentioned the keyword,
the second %s (if any) will expand to the keyword used,
the third %s (if any) will expand to the message text itself.
This only happens if `my-rcirc-notify-keywords' is non-nil."
  :type 'string
  :group 'rcirc-notify)

(defcustom my-rcirc-notify-message-private "%s sent a private message: %s"
  "Format of the message to display in the popup.
The first %s will expand to the nick that notified you,
the second %s (if any) will expand to the message text itself."
  :type 'string
  :group 'rcirc-notify)

(defvar my-rcirc-notify-nick-alist nil
  "An alist of nicks and the last time they tried to trigger a
notification.")

(defcustom my-rcirc-notify-timeout 60
  "Number of seconds that will elapse between notifications from the
same person."
  :type 'integer
  :group 'rcirc-notify)

(defun my-page-me (msg)
  (cond
    ((executable-find "notify-send")
     (start-process "page-me" nil
                    ;; 8640000 ms = 1 day
                    "notify-send" "-u" "normal" "-i" "gtk-dialog-info"
                    "-t" "8640000" "rcirc"
                    msg))
    ((executable-find "growlnotify.com")
     (start-process "page-me" "*debug*" "growlnotify.com" "/a:Emacs" "/n:IRC" msg))
    ((executable-find "growlnotify")
     (start-process "page-me" "*debug*" "growlnotify" "-a" "Emacs" "-m" msg))
    ((executable-find "osascript")
     (apply 'start-process `("page-me" nil
			     "osascript"
			     "-e" "tell application \"GrowlHelperApp\""
			     "-e" "register as application \"Emacs\" all notifications {\"rcirc\"} default notifications {\"rcirc\"}"
			     "-e" ,(concat "notify with name \"rcirc\" title \"rcirc\" description \""
					   msg "\" application name \"Emacs\"")
			     "-e" "end tell")))
    (t (error "No method available to page you."))))

(defun my-rcirc-notify (sender &optional text)
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (my-page-me (format my-rcirc-notify-message sender text)))))

(defun my-rcirc-notify-keyword (sender &optional keyword text)
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (if (listp keyword)
          (setq keyword (mapconcat 'identity keyword ", ")))
      (my-page-me (format my-rcirc-notify-keyword sender keyword text)))))

(defun my-rcirc-notify-private (sender &optional text)
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (my-page-me (format my-rcirc-notify-message-private sender text)))))

(defun my-rcirc-notify-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`my-rcirc-notify-timeout'."
  (unless delay (setq delay my-rcirc-notify-timeout))
  (let ((cur-time (float-time (current-time)))
        (cur-assoc (assoc nick my-rcirc-notify-nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) my-rcirc-notify-nick-alist)
      t)))

(defun my-rcirc-notify-me (proc sender response target text)
  "Notify the current user when someone sends a message that
matches the current nick or keywords."
  (interactive)
  (when (and (not (string= (rcirc-nick proc) sender))
	     (not (string= (rcirc-server-name proc) sender)))
    (cond ((and (string-match (concat "\\b" (rcirc-nick proc) "\\b") text)
                (my-rcirc-notify-allowed sender))
	   (my-rcirc-notify sender text))
	  (my-rcirc-notify-keywords
	   (let (keywords)
             (dolist (key rcirc-keywords keywords)
               (when (string-match (concat "\\<" key "\\>")
                                   text)
                 (push key keywords)))
	     (when keywords
               (if (my-rcirc-notify-allowed sender)
                   (my-rcirc-notify-keyword sender keywords text))))))))

(defun my-rcirc-notify-privmsg (proc sender response target text)
  "Notify the current user when someone sends a private message
to them."
  (interactive)
  (when (and (string= response "PRIVMSG")
             (not (string= sender (rcirc-nick proc)))
             (not (rcirc-channel-p target))
             (my-rcirc-notify-allowed sender))
    (my-rcirc-notify-private sender text)))

(add-hook 'rcirc-print-hooks 'my-rcirc-notify-privmsg)
(add-hook 'rcirc-print-hooks 'my-rcirc-notify-me)

(provide 'rcirc-notify)

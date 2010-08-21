;;; notify.el --- notification front-end

;; Copyright (C) 2008  Mark A. Hershberger

;; Original Author: Mark A. Hershberger <mhersberger@intrahealth.org>
;; Modified by Andrey Kotlarski <m00naticus@gmail.com>
;; Modified by Andrew Gwozdziewycz <git@apgwoz.com>
;; Keywords: extensions, convenience, lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This provides a single function, `notify', that will produce a notify
;; pop-up via D-Bus, libnotify, simple message or growl.
;; To use, just put (autoload 'notify "notify" "Notify TITLE, BODY.")
;;  in your init file.  You may override default chosen notification
;;  method by assigning `notify-method' to one of 'notify-via-dbus
;; 'notify-via-libnotify or 'notify-via-message
;;; Code:

(defvar notify-defaults (list :app "Emacs" :icon "emacs" :timeout 5000
			      :urgency "low"
			      :category "emacs.message")
  "Notification settings' defaults.
May be overridden with key-value additional arguments to `notify'.")
(defvar notify-delay '(0 5 0)
  "Minimum time allowed between notifications in time format.")
(defvar notify-last-notification '(0 0 0) "Time of last notification.")
(defvar notify-method nil "Notification method among
'notify-via-dbus, 'notify-via-libnotify, 'notify-via-message or 
'notify-via-growl")

;; determine notification method unless already set
;; prefer growl > D-Bus > libnotify > message
(cond
 ((null notify-method)
  (setq notify-method
	(cond
        ((executable-find "growlnotify") 'notify-via-growl)
	 ((and (require 'dbus nil t)
	       (dbus-ping :session "org.freedesktop.Notifications"))
	  (defvar notify-id 0 "Current D-Bus notification id.")
	  'notify-via-dbus)
	 ((executable-find "notify-send") 'notify-via-libnotify)
	 (t 'notify-via-message))))
 ((eq notify-method 'notify-via-dbus) ;housekeeping for pre-chosen DBus
  (if (and (require 'dbus nil t)
	   (dbus-ping :session "org.freedesktop.Notifications"))
      (defvar notify-id 0 "Current D-Bus notification id.")
    (setq notify-method (if (executable-find "notify-send")
			    'notify-via-libnotify
			  'notify-via-message))))
 ((and (eq notify-method 'notify-via-libnotify)
       (not (executable-find "notify-send"))) ;housekeeping for pre-chosen libnotify
  (setq notify-method
	(if (and (require 'dbus nil t)
		 (dbus-ping :session "org.freedesktop.Notifications"))
	    (progn
	      (defvar notify-id 0 "Current D-Bus notification id.")
	      'notify-via-dbus)
	  'notify-via-message)))
 ((and (eq notify-method 'notify-via-growl)
       (not (executable-find "growlnotify")))
  (setq notify-method 'notify-via-message)))

(defun notify-via-dbus (title body)
  "Send notification with TITLE, BODY `D-Bus'."
  (dbus-call-method :session "org.freedesktop.Notifications"
		    "/org/freedesktop/Notifications"
		    "org.freedesktop.Notifications" "Notify"
		    (get 'notify-defaults :app)
		    (setq notify-id (+ notify-id 1))
		    (get 'notify-defaults :icon) title body '(:array)
		    '(:array :signature "{sv}") ':int32
		    (get 'notify-defaults :timeout)))

(defun notify-via-libnotify-escape (str)
  "Escape special STR characters before passing to a shell command."
  (replace-regexp-in-string "[&<]" (lambda (m)
				     (cond ((equal m "&") " and ")
					   ((equal m "<") "{")))
			    str))

(defun notify-via-libnotify (title body)
  "Notify with TITLE, BODY via `libnotify'."
  (call-process "notify-send" nil 0 nil
		title (notify-via-libnotify-escape body) "-t"
		(number-to-string (get 'notify-defaults :timeout))
		"-i" (get 'notify-defaults :icon)
		"-u" (get 'notify-defaults :urgency)
		"-c" (get 'notify-defaults :category)))

(defun notify-via-message (title body)
  "Notify TITLE, BODY with a simple message."
  (message "%s: %s" title body))

(defun notify-via-growl (title body)
  "Notify TITLE, BODY with a growl"
  (call-process "growlnotify" nil 0 nil
                "-a" (get 'notify-defaults :app)
                "-t" (notify-via-growl-stringify title)
                "-m" (notify-via-growl-stringify body)))

(defun notify-via-growl-stringify (thing)
  (cond ((null thing) "")
        ((stringp thing) thing)
        (t (format "%s" thing))))

(defun keywords-to-properties (symbol args &optional defaults)
  "Add to SYMBOL's property list key-values from ARGS and DEFAULTS."
  (when (consp defaults)
    (keywords-to-properties symbol defaults))
  (while args
    (put symbol (car args) (cadr args))
    (setq args (cddr args))))


;;;###autoload
(defun notify (title body &rest args)
  "Notify TITLE, BODY via `notify-method'.
ARGS may be amongst :timeout, :icon, :urgency, :app and :category."
  (when (time-less-p notify-delay
		     (time-since notify-last-notification))
    (or (eq notify-method 'notify-via-message)
	(keywords-to-properties 'notify-defaults args
				notify-defaults))
    (setq notify-last-notification (current-time))
    (funcall notify-method title body)))

(provide 'notify)

;;; notify.el ends here

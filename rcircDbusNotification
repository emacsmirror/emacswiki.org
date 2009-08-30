;;; rcirc-dbus.el --- rcirc dbus notifications

;;; (C) Frederico Muñoz 2009

;;; This file is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; Author: Frederico Muñoz <fsmunoz@gmail.com>
;;; Keywords: rcirc, dbus

;;; Emacs 23 (Emacs CVS as of Feb 2009 comes with dbus bindinds. This
;;; allows some interesting interaction in terms of popup
;;; notifications similar to the ones provided by libnotify
;;; This example notifies via dbus when an "interesting" message
;;; occurs in some buffer (interesting meaning that your nick is
;;; mentioned in a non-server channel by someone other than yourself)
;;; that if clicked and not dismissed changes to the buffer with the
;;; corresponding activity. If in X or other graphical system it also
;;; raises the frame.
;;;
;;; Some initial functions to detect X active window - the window that
;;; has focus - are also included, they could be useful in other settings.
;;;
;;; The following behaviour is expected: when there is activity and
;;; the Emacs frame hasn't got focus then fire up a notification. Note
;;; that if the frame is in focus no notification is sent even if the
;;; activity buffer is not visible: this is by design since this kind
;;; of notifications are are very visible and as such they should be
;;; used sparringly: if Emacs is being used there are other
;;; notification methods with a much lighter footprint.

(require 'dbus)

(defvar my-rcirc-dbus-notification-ids '()
  "List with the dbus notification IDs from rcirc")

;;; Frame-related function

(defun fsm-x-active-window ()
  "Return the window ID of the current active window in X, as
given by the _NET_ACTIVE_WINDOW of the root window set by the
window-manager, or nil if not able to"
  (if (eq (window-system) 'x)
      (let ((x-active-window (x-window-property "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t)))
	(string-to-number (format "%x00%x" (car x-active-window) (cdr x-active-window))
			  16))
    nil))

(defun fsm-frame-outer-window-id (frame)
  "Return the frame outer-window-id property, or nil if FRAME not of the correct type"
  (if (framep frame)
      (string-to-number 
       (frame-parameter frame 'outer-window-id))
    nil))


(defun fsm-frame-x-active-window-p (frame)
  "Check if FRAME is is the X active windows
Returns t if frame has focus or nil if"
  (if (framep frame)
      (progn
	(if (eq (fsm-frame-outer-window-id frame)
		(fsm-x-active-window))
	    t
	  nil))
    nil))

;;; dbus related functions

(defun my-rcirc-dbus-notification (proc sender response target text)
  (when (and (string-match (rcirc-nick proc) text)
	     (not (string= (rcirc-nick proc) sender))
	     (not (string= (rcirc-server-name proc) sender))
	     (not (fsm-frame-x-active-window-p (selected-frame))))
    (push
     (list
      (dbus-call-method
       :session                         ;; bus		    
       "org.freedesktop.Notifications"  ;; service	    
       "/org/freedesktop/Notifications" ;; path		    
       "org.freedesktop.Notifications"  ;; interface	    
       "Notify"			 ;; method	    
       "GNU Emacs"			 ;; Application name
       0 ;; No replacement of other notifications.
       "/usr/share/icons/hicolor/48x48/apps/emacs.png" ;; Icon
       (format "%s: IRC activity" target)		;; Summary.
       (format "%s" text)				;; Body.
       '(:array) ;; No actions 
       '(:array :signature "{sv}") ;; No hints
       ':int32 -1)                 ;; Default timeout.
      proc
      target)
     my-rcirc-dbus-notification-ids)))

(defun my-dbus-signal-handler-action-invoked (id action-key)
  "Signal handler for when an action is invoked"
  (let ((rcirc-data (assq id my-rcirc-dbus-notification-ids))
	(rcirc-buffer)
	(rcirc-window)
	(rcirc-frame))
    (setq rcirc-buffer (rcirc-get-buffer (nth 1 rcirc-data) (nth 2 rcirc-data)))
    (switch-to-buffer rcirc-buffer)
    (setq rcirc-window (get-buffer-window rcirc-buffer))
    (setq rcirc-frame (window-frame rcirc-window))
    (when (window-system)
      (raise-frame rcirc-frame)
      (x-focus-frame rcirc-frame))
    (setq my-rcirc-dbus-notification-ids (assq-delete-all id my-rcirc-dbus-notification-ids))))


(defun my-dbus-signal-handler-notification-closed (id &optional reason)
  "Signal handler for when a notification is closed. REASON is
&optional since in some configurations dbus ommits it, and we
do not need it anyway given that we're only cleaning up the notification list"
  (setq my-rcirc-dbus-notification-ids (assq-delete-all id my-rcirc-dbus-notification-ids)))


;; Register for the ActionInvoked and NotificationClosed methods of
;; the Notifications services

(dbus-register-signal
 :session			  ;; bus
 "org.freedesktop.Notifications"  ;; service
 "/org/freedesktop/Notifications" ;; path
 "org.freedesktop.Notifications"  ;; interface 
 "ActionInvoked"		  ;; method
 'my-dbus-signal-handler-action-invoked)

(dbus-register-signal
 :session			  ;; bus
 "org.freedesktop.Notifications"  ;; service
 "/org/freedesktop/Notifications" ;; path
 "org.freedesktop.Notifications"  ;; interface 
 "NotificationClosed"		  ;; method
 'my-dbus-signal-handler-notification-closed)


;; Add hook
(add-hook 'rcirc-print-hooks 'my-rcirc-dbus-notification)

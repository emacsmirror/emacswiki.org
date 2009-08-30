;;; jabber-libnotify.el - emacs-jabber interface to libnotify

;; Copyright (C) 2007 - Rodrigo Lazo - rlazo.paz@gmail.com

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


(defvar jabber-libnotify-icon ""
  "*Icon to be used on the notification pop-up. Default is empty")


(defvar jabber-libnotify-timeout "2500"
  "*Specifies the timeout of the pop up window in millisecond")

(defvar jabber-libnotify-message-header "Jabber message"
  "*Defines the header of the pop up")


(defun jabber-libnotify-message (msg)
  "Show MSG using libnotify"
  (let ((process-connection-type nil))
    (start-process "notification" nil "notify-send" 
		   "-t" jabber-libnotify-timeout 
		   "-i" jabber-libnotify-icon 
		   jabber-libnotify-message-header msg)))

  
(define-jabber-alert libnotify "Show a message through the libnotify interface"
  'jabber-libnotify-message)

(provide 'jabber-libnotify)


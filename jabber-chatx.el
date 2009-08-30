;;; jabber-chatx.el --- Add the ability to remember and extract old messages

;; Copyright (C) 2008  Rodrigo Lazo

;; Author: Rodrigo Lazo <rlazo.paz AT gmail.com>
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Adds the ability to cicle over your previous send messages on the
;; chat buffer so you don't have to write them again.

;;; Code:

(require 'jabber)

(defvar jabber-chat-input-ring nil
  "List of lastest messages sent.")

(defvar jabber-chat-input-ring-max 20
  "*Maximun length of the input ring. If the ring reaches this
  size old elements will be thrown away as new come.")

(defun jabber-chat-input-initialize()
  (make-local-variable 'jabber-chat-input-ring))
(add-hook 'jabber-chat-mode-hook 'jabber-chat-input-initialize)

(defun jabber-chat-input-ring-new (string)
  "Make STRING the latest input stored on the ring. "
  (if (not (string-equal (car jabber-chat-input-ring) string))
      (progn
	(push string jabber-chat-input-ring)
	(if (> (length jabber-chat-input-ring) jabber-chat-input-ring-max)
	    (setcdr (nthcdr (1- jabber-chat-input-ring-max) jabber-chat-input-ring) nil)))))

(defun jabber-chat-input-remember (body id)
  (set-text-properties 0 (length body) nil body)
  (jabber-chat-input-ring-new body)
  nil)
(add-hook 'jabber-chat-send-hooks 'jabber-chat-input-remember)

;; Because jabber-chat-input-current-position is always reset if
;; called after another command, there is no need to make it local
(defun jabber-chat-input-cycle-input ()
  "Inserts into the chat buffer previous sent messages."
  (interactive)
  (if (eq last-command 'jabber-chat-input-cycle-input)
      (progn
	(setq jabber-chat-input-current-position 
	      (mod (1+ jabber-chat-input-current-position) 
		   (length jabber-chat-input-ring)))
	(jabber-chat-input-clean))
    (setq jabber-chat-input-current-position 0))
  (insert (car (nthcdr jabber-chat-input-current-position 
		  jabber-chat-input-ring))))

(defun jabber-chat-input-clean ()
  (save-excursion
    (delete-region jabber-point-insert (point-max))))

(provide 'jabber-chatx)
;;; jabber-chatx.el ends here

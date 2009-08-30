;;; rcirc-pounce.el -- maintain a message queue for offline nicks
;; Copyright 2007  Alex Schroeder

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Use /POUNCE to list the queues.
;; Use /POUNCE NICK to list all messages queued for NICK
;; Use /POUNCE NICK MESSAGE to add message to the queue for NICK
;; Use /UNPOUNCE NICK to clear the queue for NICK

;; The queue will be processed when NICK joins.

;;; Code:

(require 'rcirc)

(defvar rcirc-pounces nil
  "Alist with a message queue per nick.")

;; Make it persistent with session.el if possible:
(when (boundp 'session-globals-include)
  (add-to-list 'session-globals-include 'rcirc-pounces))

;; Make it persisten with desktop.el if possible:
(when (boundp 'desktop-globals-to-save)
  (add-to-list 'desktop-globals-to-save 'rcirc-pounces))

(defun rcirc-add-pounce (nick message)
  "Add MESSAGE for NICK to `rcirc-pounces'."
  (let ((cell (assoc nick rcirc-pounces)))
    (if cell
	(setcdr cell (cons message (cdr cell)))
      (setq rcirc-pounces (cons (list nick message) rcirc-pounces)))))

(defun rcirc-pounce-queue (nick)
  "Show queue for NICK."
  (dolist (msg (or (cdr (assoc nick rcirc-pounces)) '("No messages")))
    (rcirc-print process nick "NOTICE" target msg)))

(defun rcirc-pounce-queue-overview ()
  "Show nicks in queue."
  (rcirc-print process (rcirc-nick process) "NOTICE" target
	       (or (mapconcat 'identity (mapcar 'car rcirc-pounces) " ")
		   "Empty queue")))

(defun-rcirc-command pounce (message)
  "Add private MESSAGE for TARGET to its queue."
  (interactive "i")
  (if (not (string-match "\\([^ ]+\\)\\( \\(.+\\)\\)?" message))
      (rcirc-pounce-queue-overview)
    (let ((nick (match-string 1 message))
	  (text (match-string 3 message)))
      (if text
	  (rcirc-add-pounce nick text)
	(rcirc-pounce-queue nick)))))

(defun-rcirc-command unpounce (nick)
  "Clear queue for NICK."
  (interactive "i")
  (setq rcirc-pounces (delete (assoc nick rcirc-pounces) rcirc-pounces)))

(defun rcirc-pounce-process (sender)
  "Send all messages for SENDER."
  (let ((nick (rcirc-user-nick sender)))
    (dolist (msg (cdr (assoc nick rcirc-pounces)))
      (rcirc-send-message process nick msg))
    (rcirc-cmd-unpounce nick)))

(defadvice rcirc-handler-JOIN (after rcirc-handler-JOIN-pounce activate)
  "Process pounces in `rcirc-pounces'."
  (rcirc-pounce-process sender))

(defadvice rcirc-handler-MODE (after rcirc-handler-MODE-pounce activate)
  "Process pounces in `rcirc-pounces' on mode change."
  (rcirc-pounce-process sender))

(provide 'rcirc-pounce)

;;; rcirc-pounce.el ends here

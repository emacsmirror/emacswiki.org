;;; byrd-biff.el -- Mail notification
;; Author          : Mike Williams <byrd@comp.vuw.ac.nz>
;; Created On      : Thu Feb 21 11:06:33 1991
;; Last Modified By: Mike Williams
;; Last Modified On: Thu Oct 31 10:23:58 1991
;; $Id: byrd-biff.el,v 1.0 1991/10/30 22:22:07 mike-w Exp $
;; RCS Info        : $Revision: 1.0 $ $Locker:  $
;; ========================================================================
;; NOTE: this file must be recompiled if changed.
;;
;; Copyright (C) Mike Williams 1990, 1991
;;
;; This is free software.
;; This file is not part of GNU Emacs, but is made available under the
;; same conditions.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
;; to anyone for the consequences of using it or for whether it serves
;; any particular purpose or works at all, unless he says so in writing.
;; Refer to the GNU Emacs General Public License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights and
;; responsibilities.  It should be in a file named COPYING.  Among other
;; things, the copyright notice and this notice must be preserved on all
;; copies.
 
(require 'timer)
 
(provide 'byrd-biff)
 
;;=== Installation ========================================================
;;
;; (autoload 'biff "byrd-biff" nil t)
;; (biff t)
;;
;;=========================================================================
 
(autoload 'Electric-pop-up-window "electric")
 
(defvar biff-check-mail-command-list '("from")
    "Command + arguments to execute to find out whether there is new mail.
Mail notification is given if the output from this command matches
biff-new-mail-re.")
 
(defvar biff-new-mail-re "From \\(.*\\)$"
  "*String used to determine whether the output of biff-check-mail-command
indicates that new mail has arrived.")
 
(defvar biff-notification-string "New mail from %s"
  "*String used to notify user of incoming mail.
It is used in conjunction with the first substring matched by
biff-new-mail-re to create a message to display in the minibuffer.
If it's value is nil, no minibuffer message is displayed, but the *Biff*
buffer is displayed for a short time.")
 
(defvar biff-time-interval 30
  "*The time (in seconds) that biff waits before looking for mail")
 
(defvar biff-new-mail nil
  "Non-nil when new mail has arrived.
May be used in the mode-line to signal arival of new mail.")
 
(defun biff (arg)
  "Notify me when new mail arrives by writing a message in the minibuffer
or popping up a buffer (if biff-notification-string is nil).  This is
useful when you spend long periods inside emacs so that the normal biff
doesn't help.  Kills the current biff, and then starts a new one if ARG is
non-nil.  The status of your mailbox is examined every biff-time-interval
seconds (default: 30)"
  (interactive (list (y-or-n-p "Run biff? ")))
  (let ((old-timer (get-timer "biff")))
    (if old-timer (delete-timer old-timer)))
  (if arg
      (start-timer "biff" 'biff-check-mail 5 biff-time-interval)))
 
(defun biff-check-mail ()
  "Check for new mail"
  (interactive)
  (setq biff-new-mail nil)
  (let ((biff-check-mail-process
  (apply 'start-process "biff-check-mail" nil
  biff-check-mail-command-list)))
    (set-process-filter biff-check-mail-process 'biff-check-mail-filter)))
 
(defun biff-check-mail-filter (PROC STR)
  (save-excursion
    (set-buffer (get-buffer-create " *Biff*"))
    (goto-char (point-max))
    (setq biff-new-mail t)
    (if (and (not (search-backward STR nil t))
      (string-match biff-new-mail-re STR))
 (progn
   (insert STR)
   (ding)
   (if biff-notification-string
       (let ((notification
       (if (match-beginning 1)
    (substring STR (match-beginning 1) (match-end 1))
         (substring STR (match-beginning 0) (match-end 0)))))
  (message biff-notification-string notification))
     (save-window-excursion
       (Electric-pop-up-window (current-buffer))
       (sleep-for 2) (sit-for 30)))))))
   
;;; byrd-biff.el ends here

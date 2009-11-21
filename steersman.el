;;; steersman.el --- Control Emacs over XMPP/Jabber

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/Steersman
;; Version: 0.5
;; Created: 2008-07-15
;; Keywords: jabber xmpp remote
;; EmacsWiki: Steersman

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The word "Cybernetics" comes from the Greek word for
;; steersman. Steersman is a library for remotely controlling an Emacs
;; instance.

;; Inspired by and based on Barman Brakjoller's Jabber Remote Control:
;; (http://sourceforge.net/forum/forum.php?thread_id=1092503&forum_id=303383)

;;; Usage:

;; Set your connection parameters:

;;   (setq steersman-password "secret"
;;         steersman-master "phil@hagelb.org"
;;         jabber-server "hagelb.org"
;;         jabber-username "agent"
;;         jabber-resource
;;         (concat "steersman-"
;;                 (substring (shell-command-to-string "hostname") 0 -1)))

;; Connect via M-x jabber-connect or (jabber-connect) in your init.

;; Talk to it from the Jabber ID set in `steersman-master'.

;; You'll first need to activate it by saying `steersman-password'.
;; Commands beginning with an open parentheses will be evaluated as
;; Lisp code; commands beginning with a dollar sign will be executed
;; in the shell. Commands for which a function exists named
;; steersman-$COMMAND will execute that function with no
;; arguments. Anything else will be psychoanalyzed.

;;; Code:

(require 'jabber)
(require 'doctor)

(defvar steersman-activated nil)
(defvar steersman-password nil)
(defvar steersman-master nil
  "Jabber user allowed to control the steersman.")

;;; Core functions

(defun steersman-reply (mess &optional to)
  (jabber-send-message (or to steersman-master) nil mess nil))

(defun steersman-message (from buffer text proposed-alert)
  "Respond to a Jabber message from the master."
  (if (equal steersman-master (jabber-jid-user from))
    (if steersman-activated
        (cond ((functionp (intern (concat "steersman-" text)))
               (funcall (intern (concat "steersman-" text))))
              ((string-match "^(.*)" text)
               (steersman-reply (pp (eval (read text)))))
              ((string-match "^$" text)
               (steersman-reply (shell-command-to-string (substring text 1))))
              ((string-match "^!" text)
               (steersman-reply (format "Yes master! I *will* %s right away!"
                                        (substring text 1))))
              (t (steersman-reply (doctor-reply text))))

      (if (not (equal text steersman-password))
          (steersman-reply "Zzzzz...")
        (setq steersman-activated t)
        (steersman-reply
         "Awakening. Your command, master?")))
    (steersman-reply "Begone." from)))

(add-hook 'jabber-alert-message-hooks 'steersman-message)

;;; Commands

;; TODO: check the first word for commands; use the rest as args

(defun steersman-update ()
  "Get and use the latest version of steersman.el from the EmacsWiki."
  (steersman-reply "Updating from EmacsWiki; be sure you trust the code.")
  (let ((install-elisp-confirm-flag nil))
    (install-elisp-from-emacswiki "steersman.el"))
  (save-excursion (find-file (concat install-elisp-repository-directory
                                     "steersman.el"))
                  (eval-buffer)
                  (kill-buffer))
  "Updated.")

(defun steersman-restart ()
  "Launch a new Emacs instance with a steersman and kill the current one.")

(defun steersman-sleep ()
  "Deactivate steersman until `steersman-password' is spoken."
  (steersman-reply "For a time... we slumber.")
  (setq steersman-activated nil))

;;; Psychoanalysis

;; The doctor.el library seems to suffer from fairly poor coupling; we
;; need to be able to use its functionality non-interactively.

(defun doctor-sentence-from-string (sentence)
  "Turn a string into a list of tokens."
  (mapcar 'intern (mapcar 'downcase
                          (remove "" (split-string sentence "\\W+")))))

(defun doctor-reply (input)
  "Wrap the `doctor' library in a function for non-interactive use."
  (save-window-excursion
    (switch-to-buffer "*doctor*")
    (unless (boundp 'howareyoulst) (make-doctor-variables))
    (let ((begin-point (point)))
      (doctor-doc (doctor-sentence-from-string input))
      (buffer-substring begin-point (- (point-max) 1)))))

(provide 'steersman)
;;; steersman.el ends here

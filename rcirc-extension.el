;;; rcirc-extension.el --- The extensions that i collect.

;; Author: Emacs Guys
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Emacs Guys, all rights reserved.
;; Created: 2008-06-08 12:01:29
;; Version: 0.1.1
;; Last-Updated: 2008-08-29 03:28:08
;; URL:
;; Keywords: rcirc
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
;;  None
;;

;;; Installation:
;;
;; Copy rcirc-extension.el to your load-path and add to your ~/.emacs
;;
;;  (require 'rcirc-extension)
;;
;; No need more

;;; Commentary:
;;
;; These extensions is i collects from Internet, just make rcirc more powerful.
;;

;;; Change log:
;;
;; 2008/08/29
;;         Add function `rcirc-cmd-read-current-message'.
;;
;;  2008/09/25
;;         Clean up these extensions
;;

;;; Acknowledgments:
;;
;;      All Emacs guys  (guys@emacs.org)    for freedom
;;

;;; TODO
;;
;; None
;;

;;; Code:

(eval-after-load 'rcirc                 ;smiley
  '(add-to-list 'rcirc-markup-text-functions 'rcirc-smileys))

(eval-after-load 'rcirc                 ;/all command
  '(defun-rcirc-command all (input)
     "Run the arguments as a command for all connections.
Example use: /all away food or /all quit zzzz."
     (interactive "s")
     (let ((buffers (mapcar 'process-buffer (rcirc-process-list))))
       (dolist (buf buffers)
         (with-current-buffer buf
           (goto-char (point-max))
           (insert "/" input)
           (rcirc-send-input))))))

;; don't display user list when login
(defvar rcirc-hide-names-on-join t
  "Non-nil if nick names list should be hidden when joining a channel.")
(defadvice rcirc-handler-353 (around my-aad-rcirc-handler-353 activate)
  "Do not render NICK list on join when `rcirc-hide-names-on-join' is non-nil.
 RPL_NAMREPLY."
  (when (not rcirc-hide-names-on-join)
    ad-do-it))
(defadvice rcirc-handler-366 (around my-aad-rcirc-handler-366 activate)
  "Do not render NICK list on join when `rcirc-hide-names-on-join' is non-nil.
 RPL_ENDOFNAMES."
  (when (not rcirc-hide-names-on-join)
    ad-do-it))
(defadvice rcirc-handler-JOIN (around my-before-ad-rcirc-handler-join-no-names activate)
  "Set `rcirc-hide-names-on-join' to `t'."
  ad-do-it
  (setq rcirc-hide-names-on-join t))
(defadvice rcirc-cmd-names (before my-ad-rcirc-cmd-names-no-list activate)
  "Reset rcirc-hide-names-on-join to nil after the JOIN step."
  (setq rcirc-hide-names-on-join nil))

;; mode
(defalias 'rcirc-cmd-opme
  '(lambda (&optional args process target)
     (interactive)
     (rcirc-cmd-op rcirc-nick))
  "Request a ChanServ OP on my current nick in the current channel.")
(defalias 'rcirc-cmd-deop
  '(lambda (nicks &optional process target)
     (interactive (list (completing-read "De-op nick: "
                                         (with-rcirc-server-buffer rcirc-nick-table))))
     (let ((nicks (concat "-" (mapconcat 'identity (split-string
                                                    nicks) " -"))))
       (rcirc-cmd-op nicks)))
  "Send DE-OP for `nicks'.
    Limitation: in its interactive form, you can only de-op one nick.")
(defalias 'rcirc-cmd-deopme
  '(lambda (&optional args process target)
     (interactive)
     (rcirc-cmd-deop rcirc-nick)))

;; /reconnect
(eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            channels query-buffers)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      rcirc-default-user-name
                      rcirc-default-user-full-name
                      channels))))

;; /help
(eval-after-load 'rcirc
  '(defun-rcirc-command help (arg)
     "List rcirc commands or print their doc-string."
     (if (string= arg "")
         (rcirc-print process (rcirc-nick process) "NOTICE" target
                      (mapconcat 'identity (rcirc-commands) " "))
       (let ((command (intern-soft (concat "rcirc-cmd-" (downcase arg)))))
         (if (fboundp command)
             (rcirc-print process (rcirc-nick process) "NOTICE" target
                          (documentation command))
           (rcirc-print process (rcirc-nick process) "NOTICE" target
                        (concat "/" (upcase arg) " is undefined")))))))

;; /pounce
(eval-after-load 'rcirc '(require 'rcirc-pounce))

(defun login-rcirc()
  "Login rcirc."
  (interactive)
  (save-window-excursion
    (rcirc nil)))

(defun rcirc-smileys (&rest ignore)
  "Run smiley-buffer on the buffer
but add a temporary space at the end to ensure matches of smiley
regular expressions."
  (goto-char (point-max))
  (insert " ")
  (smiley-buffer)
  (delete-char -1))

(defun rcirc-command-op (nicks)
  "Send OP for `nicks'.
    Limitation: in its interactive form, you can only op one nick."
  (interactive (list (completing-read "Op nick: "
                                      (with-rcirc-server-buffer rcirc-nick-table))))
  (let (process
        target)
    (dolist (nick (split-string nicks " "))
      (rcirc-send-string process
                         (format "ChanServ OP %s %s" target nick)))))

(defun rcirc-commands ()
  "Return a list of defined IRC commands.
If a command called rcirc-cmd-foo exists, the IRC command /FOO
will be part of the list returned."
  (let ((commands))
    (mapatoms (lambda (sym)
                (let ((name (symbol-name sym)))
                  (when (and (commandp sym)
                             (string= (substring name 0 (min (length name) 10))
                                      "rcirc-cmd-"))
                    (setq commands (cons (concat"/" (downcase (substring name 10)))
                                         commands))))))
    commands))

(defun rcirc-write-log (process sender response target text)
  "Record the log of rcirc."
  (let (buffer)
    (when rcirc-log-directory
      (with-temp-buffer
        ;; Sometimes TARGET is a buffer :-(
        (when (bufferp target)
          (setq target (with-current-buffer buffer rcirc-target)))
        ;; Sometimes buffer is not anything at all!
        (unless (or (null target) (string= target ""))
          ;; Print the line into the temp buffer.
          (insert (format-time-string "%Y-%m-%d %H:%M "))
          (insert (format "%-16s " (rcirc-user-nick sender)))
          (unless (string= response "PRIVMSG")
            (insert "/" (downcase response) " "))
          (insert text "\n")
          ;; Append the line to the appropriate logfile.
          (let ((coding-system-for-write 'no-conversion))
            (write-region (point-min) (point-max)
                          (concat rcirc-log-directory "/" (downcase target))
                          t 'quietly)))))))

(defun rcirc-cmd-read-current-message ()
  "Read current message."
  (interactive)
  (let (current-message)
    (save-excursion
      (beginning-of-line)
      (rcirc-send-input)
      (call-interactively 'end-of-buffer)
      (move-beginning-of-line 1)
      (setq current-message (buffer-substring
                             (point)
                             (progn
                               (call-interactively 'end-of-buffer)
                               (point))))
      (move-beginning-of-line 1)
      (kill-line))
    (when current-message
      (emms-stop)
      (festival-say current-message))))

(defun rcirc-cmd-talk-to (nick)
  "Talk to someone."
  (interactive (list
                (completing-read "Talk to: "
                                 (with-rcirc-server-buffer rcirc-nick-table))))
  (insert (concat nick ": ")))

(provide 'rcirc-extension)

;;; rcirc-extension.el ends here

;;; LocalWords:  cmd zzzz buf login aad RPL NAMREPLY ENDOFNAMES opme args deop
;;; LocalWords:  ChanServ de deopme mapatoms sym PRIVMSG emms

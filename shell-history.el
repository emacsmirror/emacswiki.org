;;; shell-history.el --- integration with shell history
;; $Id: shell-history.el,v 1.6 2010/05/04 08:50:28 rubikitch Exp $

;; Copyright (C) 2008, 2009, 2010 rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: processes, convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/shell-history.el

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Add command of `shell-command', `shell-command-on-region',
;; `compile', `grep', and `background' to shell history file,
;; eg. ~/.zsh_history.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `shell-add-to-history'
;;    Add this command line to shell history in `shell-mode'.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; `shell-add-to-history' adds current command line in *shell* buffer
;; into shell history file.

;; (require 'shell-history)
;; (define-key shell-mode-map "\M-m" 'shell-add-to-history)

;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x shell-history-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of shell-history.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "shell-history.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x shell-history-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:

;; $Log: shell-history.el,v $
;; Revision 1.6  2010/05/04 08:50:28  rubikitch
;; Added bug report command
;;
;; Revision 1.5  2010/03/30 23:37:25  rubikitch
;; Fix for C-x C-m c
;;
;; Revision 1.4  2010/03/26 06:33:08  rubikitch
;; Do not add same entry as previous entry to history.
;;
;; Revision 1.3  2009/10/21 09:43:55  rubikitch
;; New command: `shell-add-to-history'
;;
;; Revision 1.2  2009/02/20 06:11:23  rubikitch
;; New variable: `shell-history-add-ascii-only'
;;
;; Revision 1.1  2008/09/01 02:46:48  rubikitch
;; Initial revision
;;

;;; Code:

(defvar shell-history-version "$Id: shell-history.el,v 1.6 2010/05/04 08:50:28 rubikitch Exp $")
(eval-when-compile (require 'cl))

(defvar shell-history-file
  (if (string-match "/zsh" shell-file-name)
      "~/.zsh_history"
    "~/.bash_history")
  "Shell history file name.")

(defvar shell-history-add-ascii-only t
  "If non-nil, add shell history only when the command line is ascii-only.")

(defun shell-history-buffer ()
  (or (get-file-buffer shell-history-file)
      (prog1 (find-file-noselect shell-history-file)
        (auto-revert-mode -1)
        (buffer-disable-undo))))

(defun shell-history-zsh-extended-history-p ()
  (save-excursion (goto-char (point-min))
                  (re-search-forward "^: [0-9]+:" (point-at-eol) t)))

(defvar shell-history-previous-entry nil)
(defun add-to-shell-history (entry)
  (when (and (not (equal entry shell-history-previous-entry))
             (or (not shell-history-add-ascii-only)
                 (string-match "^[\000-\177]+$" entry)))
    (with-current-buffer (shell-history-buffer)
      (setq shell-history-previous-entry entry)
      (revert-buffer t t)
      (goto-char (point-max))
      (when (shell-history-zsh-extended-history-p)
        (insert (format-time-string ": %s:0;" (current-time))))
      (insert entry "\n")
      ;; prevent from displaying message.
      (let (coding-system-for-write)
        (write-region (point-min) (point-max) shell-history-file nil 'silently))
      (set-visited-file-modtime (current-time))
      (set-buffer-modified-p nil))))

;; (add-to-shell-history "test")
;; (let ((shell-history-file "~/.bash_history")) (add-to-shell-history "test"))

(defadvice shell-command-on-region (after add-to-shell-history activate)
  (add-to-shell-history command))
;; (progn (ad-disable-advice 'shell-command-on-region 'after 'add-to-shell-history) (ad-update 'shell-command-on-region)) 
(defadvice background (after add-to-shell-history activate)
  (add-to-shell-history command))
;; (progn (ad-disable-advice 'background 'after 'add-to-shell-history) (ad-update 'background)) 
(defadvice compilation-start (after add-to-shell-history activate)
  (add-to-shell-history command))
;; (progn (ad-disable-advice 'compilation-start 'after 'add-to-shell-history) (ad-update 'compilation-start)) 

(defun shell-add-to-history ()
  "Add this command line to shell history in `shell-mode'."
  (interactive)
  (beginning-of-line)
  (add-to-shell-history (buffer-substring (point) (point-at-eol)))
  (delete-region (point) (point-at-eol))
  (message "Added to shell history"))

;;;; Bug report
(defvar shell-history-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar shell-history-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of shell-history.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"shell-history.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun shell-history-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   shell-history-maintainer-mail-address
   "shell-history.el"
   (apropos-internal "^shell-history-" 'boundp)
   nil nil
   shell-history-bug-report-salutation))

(provide 'shell-history)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "shell-history.el")
;;; shell-history.el ends here

;;; space-chord.el --- key chord with Space
;; $Id: space-chord.el,v 1.3 2008/11/05 03:38:22 rubikitch Exp $

;; Copyright (C) 2008  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/space-chord.el

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

;; A thumb is a finger of the strongest! Let's utilize a thumb.
;; This package defines key-chord starting with Space.
;; This package depends on key-chord.el:
;;  http://www.emacswiki.org/cgi-bin/wiki/download/key-chord.el

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `space-chord-define-global'
;;    Define a key-chord of KEY with space starting a COMMAND.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Usage:

;; Bind Space-f to `find-file' in global-map
;;   (space-chord-define-global "f" 'find-file)
;;      or
;;   (space-chord-define-global ?f 'find-file)

;; Bind Space-c to `compile' in c-mode-map
;;   (space-chord-define c-mode-map "c" 'compile)
;;      or
;;   (space-chord-define c-mode-map ?c 'compile)

;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x space-chord-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of space-chord.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "space-chord.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x space-chord-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:

;; $Log: space-chord.el,v $
;; Revision 1.3  2008/11/05 03:38:22  rubikitch
;; Set `space-chord-delay' to 0.08 by default
;;
;; Revision 1.2  2008/11/05 03:34:37  rubikitch
;; commentary, docstring
;;
;; Revision 1.1  2008/11/05 02:28:18  rubikitch
;; Initial revision
;;

;;; Code:

(defvar space-chord-version "$Id: space-chord.el,v 1.3 2008/11/05 03:38:22 rubikitch Exp $")
(require 'key-chord)
(defvar space-chord-delay 0.08
  "Max time delay between two key press to be considered a key chord.
`key-chord-two-keys-delay' for space-chord.")

(defun space-chord-define-global (key command)
  "Define a key-chord of KEY with space starting a COMMAND.
KEY is a character or a 1-length string.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed."
  (interactive "sSet space chord globally (1 key): \nCSet chord \"%s\" to command: ")
  (space-chord-define (current-global-map) key command))

(defun space-chord-define (keymap key command)
  "Define in KEYMAP, a key-chord of KEY with space starting a COMMAND.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed."
  (define-key keymap (vector 'key-chord ? (if (stringp key) (aref key 0) key)) command))

(defadvice key-chord-input-method (around space-chord activate)
  "Set `key-chord-two-keys-delay' to `space-chord-delay' when starting a key-chord with Space."
  (if (eq (ad-get-arg 0)  ? )
      (let ((key-chord-two-keys-delay space-chord-delay))
        ad-do-it)
    ad-do-it))
;; (progn (ad-disable-advice 'key-chord-input-method 'around 'space-chord) (ad-update 'key-chord-input-method)) 
;; (define-key (current-global-map) (vector 'key-chord ?  ) nil)
;; (define-key (current-global-map) (vector 'key-chord ?f ) nil)
;; (space-chord-define-global "f" 'find-file)
;; (space-chord-define-global ?f 'view-file)
;; (space-chord-define-global "f" nil)

;;;; Bug report
(defvar space-chord-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar space-chord-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of space-chord.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"space-chord.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun space-chord-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   space-chord-maintainer-mail-address
   "space-chord.el"
   (apropos-internal "^space-chord-" 'boundp)
   nil nil
   space-chord-bug-report-salutation))

(provide 'space-chord)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "space-chord.el")
;;; space-chord.el ends here

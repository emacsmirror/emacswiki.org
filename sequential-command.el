;;; sequential-command.el --- Many commands into one command
;; $Id: sequential-command.el,v 1.3 2010/05/04 08:55:35 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: convenience, lisp
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/sequential-command.el

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

;; Integrating multiple commands into one command is sometimes
;; useful. Pressing C-e at the end of line is useless and adding the
;; other behavior in this situation is safe.
;; 
;; For example, defining `my-end': if point is at the end of line, go
;; to the end of buffer, otherwise go to the end of line. Just evaluate it!
;;
;; (define-sequential-command my-end  end-of-line end-of-buffer)
;; (global-set-key "\C-e" 'my-end)
;;
;; Consequently, pressing C-e C-e is `end-of-buffer'!
;;
;; `define-sequential-command' is a macro that defines a command whose
;; behavior is changed by sequence of calls of the same command.
;;
;; `seq-return' is a command to return to the position when sequence
;; of calls of the same command was started.
;;
;; See sequential-command-config.el if you want examples.
;;
;; http://www.emacswiki.org/cgi-bin/wiki/download/sequential-command-config.el

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `seq-return'
;;    Return to the position when sequence of calls of the same command was started.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Demonstration:

;; Execute M-x seq-demo. And press C-x C-z many times.

;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x seq-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of sequential-command.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "sequential-command.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x seq-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:

;; $Log: sequential-command.el,v $
;; Revision 1.3  2010/05/04 08:55:35  rubikitch
;; Added bug report command
;;
;; Revision 1.2  2009/02/17 03:04:18  rubikitch
;; * Add demo.
;; * Rename file name.
;; * New macro: `define-sequential-command'.
;; * New command: `seq-return'.
;;
;; Revision 1.1  2009/02/17 01:24:04  rubikitch
;; Initial revision
;;

;;; Code:

(defvar sequential-command-version "$Id: sequential-command.el,v 1.3 2010/05/04 08:55:35 rubikitch Exp $")
(eval-when-compile (require 'cl))

(defvar seq-store-count 0)
(defvar seq-start-position nil
  "Stores `point' and `window-start' when sequence of calls of the same
 command was started. This variable is updated by `seq-count'")

(defun seq-count ()
  "Returns number of times `this-command' was executed.
It also updates `seq-start-position'."
  (if (eq last-command this-command)
      (incf seq-store-count)
    (setq seq-start-position  (cons (point) (window-start))
          seq-store-count     0)))

(defmacro define-sequential-command (name &rest commands)
  "Define a command whose behavior is changed by sequence of calls of the same command."
  (let ((cmdary (apply 'vector commands)))
    `(defun ,name ()
       ,(concat "Sequential command of "
                (mapconcat
                 (lambda (cmd) (format "`%s'" (symbol-name cmd)))
                 commands " and ")
                ".")
       (interactive)
       (call-interactively
        (aref ,cmdary (mod (seq-count) ,(length cmdary)))))))
;; (macroexpand '(define-sequential-command foo beginning-of-line beginning-of-buffer))

(defun seq-return ()
  "Return to the position when sequence of calls of the same command was started."
  (interactive)
  (goto-char (car seq-start-position))
  (set-window-start (selected-window) (cdr seq-start-position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  demonstration                                                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun seq-demo ()
  (interactive)
  (global-set-key "\C-x\C-z" 'seq-count-test)
  (message "Press C-x C-z repeatedly"))

(defun seq-count-test ()
  (interactive)
  (message "seq-count: %d" (seq-count)))

(define-sequential-command seq-home
  beginning-of-line back-to-indentation beginning-of-buffer seq-return)

;;;; Bug report
(defvar seq-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar seq-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of sequential-command.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"sequential-command.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun seq-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   seq-maintainer-mail-address
   "sequential-command.el"
   (apropos-internal "^seq" 'boundp)
   nil nil
   seq-bug-report-salutation))

(provide 'sequential-command)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "sequential-command.el")
;;; sequential-command.el ends here

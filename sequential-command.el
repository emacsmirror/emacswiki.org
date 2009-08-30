;;; sequential-command.el --- Many commands into one command
;; $Id: sequential-comand.el,v 1.2 2009/02/17 03:04:18 rubikitch Exp $

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

;;; Demonstration:

;; Execute M-x seq-demo. And press C-x C-z many times.

;;; History:

;; $Log: sequential-comand.el,v $
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

(defvar sequential-command-version "$Id: sequential-comand.el,v 1.2 2009/02/17 03:04:18 rubikitch Exp $")
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


(provide 'sequential-command)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "sequential-command.el")
;;; sequential-command.el ends here

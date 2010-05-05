;;; d-insert-import.el --- insert import statement for D language
;; $Id: d-insert-import.el 1553 2010-05-04 08:38:13Z rubikitch $

;; Copyright (C) 2007  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: D, import, convenience

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

;; `d-insert-import' prepends appropriate import statement for the
;; function at point.

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:

;; (require 'd-insert-import)
;; (defun d-mode-hook0 ()
;;   (define-key d-mode-map "\C-c\C-i" 'd-insert-import-at-point))
;; (add-hook 'd-mode-hook 'd-mode-hook0)


;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x d-insert-import-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of d-insert-import.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "d-insert-import.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x d-insert-import-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;; 
;;; History:

;; ------------------------------------------------------------------------
;; r1499 | rubikitch | 2007-07-15 16:57:52 +0900 (Sun, 15 Jul 2007) | 2 lines

;; skip module declaration.

;; ------------------------------------------------------------------------
;; r1457 | rubikitch | 2007-01-24 04:33:44 +0900 (Wed, 24 Jan 2007) | 3 lines

;; updated Commentary.
;; rename function: d-insert-import -> d-insert-import-at-point

;; ------------------------------------------------------------------------
;; r1456 | rubikitch | 2007-01-24 01:11:27 +0900 (Wed, 24 Jan 2007) | 2 lines

;; import -> private import

;; ------------------------------------------------------------------------
;; r1453 | rubikitch | 2007-01-18 20:48:12 +0900 (Thu, 18 Jan 2007) | 1 line

;; *** empty log message ***
;; ------------------------------------------------------------------------
;; r1452 | rubikitch | 2007-01-18 20:46:04 +0900 (Thu, 18 Jan 2007) | 2 lines

;; changed message

;; ------------------------------------------------------------------------
;; r1451 | rubikitch | 2007-01-18 20:34:26 +0900 (Thu, 18 Jan 2007) | 2 lines

;; initial

;; ------------------------------------------------------------------------


;;; Code:

(require 'd-insert-import-data)
(defvar d-import-token-hash)

(defun d-insert-import-at-point (sym)
  (interactive (list (word-at-point)))
  (let ((importlib (gethash sym d-import-token-hash)))
    (if importlib
        (let ((import-stmt (concat "import " importlib ";\n")))
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^module.+\n" nil t)
            (cond ((search-forward import-stmt nil t)
                   (message "found:%s" import-stmt)
                   t)
                  (t
                   (insert "private " import-stmt)
                   (message "inserted:%s" import-stmt)
                   importlib))))
      (error "cannot find library for %s" sym))))

;; (equal "std.cstream" (with-temp-buffer (d-insert-import-at-point "dout")))
;; (eq t (with-temp-buffer (insert "private import std.cstream;\n") (d-insert-import-at-point "dout")))
;; (with-temp-buffer (d-insert-import-at-point "notfound"))

;;;; Bug report
(defvar d-insert-import-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar d-insert-import-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of d-insert-import.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"d-insert-import.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun d-insert-import-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   d-insert-import-maintainer-mail-address
   "d-insert-import.el"
   (apropos-internal "^d-insert-import-" 'boundp)
   nil nil
   d-insert-import-bug-report-salutation))

(provide 'd-insert-import)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "d-insert-import.el")
;; prepare (find-sh0 "ruby generate-el.rb > d-insert-import-data.el")
;;; d-insert-import.el ends here

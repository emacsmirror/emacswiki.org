;;; sticky.el --- Sticky key for capital letters
;; $Id: sticky.el,v 1.7 2010/05/04 09:03:00 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/sticky.el

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
;;
;; Input capital letters and some symbols with not shift key but sticky key.
;; See `use-sticky-key' docstring.
;; [EVAL IT] (describe-function 'use-sticky-key)

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
;;
;; Put sticky.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'sticky)
;; 
;; (use-sticky-key ?\; sticky-alist:en)    ; for english keyboards
;;   OR
;; (use-sticky-key ?\; sticky-alist:ja)    ; for japanese keyboards
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET sticky RET
;;


;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x sticky-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of sticky.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "sticky.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x sticky-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:

;; $Log: sticky.el,v $
;; Revision 1.7  2010/05/04 09:03:00  rubikitch
;; Added bug report command
;;
;; Revision 1.6  2009/05/17 21:00:18  rubikitch
;; typo
;;
;; Revision 1.5  2009/05/17 16:15:35  rubikitch
;; typo
;;
;; Revision 1.4  2009/05/17 11:07:24  rubikitch
;; docstring
;;
;; Revision 1.3  2009/05/17 10:50:05  rubikitch
;; Bug fix when sticky-key == noshift
;;
;; Revision 1.2  2009/05/17 10:44:14  rubikitch
;; Rename variable: sticky-alist -> `sticky-alist:ja`
;; New variable: `sticky-alist:en`
;; Change `use-sticky-key' arguments
;;
;; Revision 1.1  2009/05/17 03:26:13  rubikitch
;; Initial revision
;;

;;; Code:

;; (find-sh0 "emacs -Q -L ~/emacs/lisp -l sticky --eval '(use-sticky-key \";\" sticky-alist:ja)'")
(defvar sticky-version "$Id: sticky.el,v 1.7 2010/05/04 09:03:00 rubikitch Exp $")
(eval-when-compile (require 'cl))
(defgroup sticky nil
  "sticky"
  :group 'emacs)

(defvar sticky-alist:ja
  '((?1 . ?!) (?2 . ?\") (?3 . ?#) (?4 . ?$) (?5 . ?%) (?6 . ?&) (?7 . ?')
    (?8 . ?() (?9 . ?)) (?0 . ?~)
    (?@ . ?`) (?[ . ?{) (?] . ?}) (?- . ?=) (?^ . ?~) (?, . ?<) (?. . ?>)
    (?/ . ??) (?: . ?*) (?\; . ?+) (?\\ . ?_)))

(defvar sticky-alist:en
  '((?1 . ?!) (?2 . ?@) (?3 . ?#) (?4 . ?$) (?5 . ?%) (?6 . ?^) (?7 . ?&)
    (?8 . ?*) (?9 . ?\() (?0 . ?\))
    (?` . ?~) (?[ . ?{) (?] . ?}) (?- . ?_) (?, . ?<) (?. . ?>)
    (?/ . ??) (?\; . ?:) (?\\ . ?|) (?' . ?\") (?= . ?+)))

(defun use-sticky-key (sticky-key sticky-alist)
  "Enable sticky key for capital letters (and some symbols) input.

STICKY-KEY is a sticky key specified by char(integer)/symbol/string.
For example:
- ?\; or \";\" is semicolon key.
- 'muhenkan is muhenkan key.  (X11 only)

STICKY-ALIST is an alist whose element is a pair of character and shifted character.
`sticky-alist:ja' and `sticky-key:en' are example of STICKY-ALIST."
  (when (stringp sticky-key)
    (setq sticky-key (aref sticky-key 0)))
  (global-unset-key (vector sticky-key))
  (unless (keymapp key-translation-map)
    (setq key-translation-map (make-sparse-keymap)))
  (loop for i from ?a to ?z do
        (define-key key-translation-map (vector sticky-key i) (vector (- i 32))))
  (loop for (noshift . shift) in sticky-alist
        unless (= sticky-key noshift)
        do (define-key key-translation-map (vector sticky-key noshift) (vector shift)))
  (when (integerp sticky-key)
    (global-set-key (vector sticky-key sticky-key)
                    `(lambda () (interactive) (insert ,sticky-key)))))

;;;; Bug report
(defvar sticky-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar sticky-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of sticky.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"sticky.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun sticky-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   sticky-maintainer-mail-address
   "sticky.el"
   (apropos-internal "^sticky-" 'boundp)
   nil nil
   sticky-bug-report-salutation))

(provide 'sticky)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "sticky.el")
;;; sticky.el ends here

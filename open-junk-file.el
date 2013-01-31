;;; open-junk-file.el --- Open a junk (memo) file to try-and-error

;; $Id: open-junk-file.el,v 1.3 2010/06/18 20:07:49 rubikitch Exp rubikitch $

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: convenience, tools
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/open-junk-file.el

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
;; M-x `open-junk-file' opens a new file whose filename is derived from
;; current time. You can write short program in it. It helps to
;; try-and-error programs.
;;
;; For example, in Emacs Lisp programming, use M-x `open-junk-file'
;; instead of *scratch* buffer. The junk code is SEARCHABLE.
;;
;; In Ruby programming, use M-x `open-junk-file' and
;; write a script with xmpfilter annotations. It is the FASTEST
;; methodology to try Ruby methods, Irb is not needed anymore.
;; Xmpfilter is available at http://eigenclass.org/hiki/rcodetools

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `open-junk-file'
;;    Open a new file whose filename is derived from current time.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `open-junk-file-format'
;;    *File format to put junk files with directory.
;;    default = "~/junk/%Y/%m/%d-%H%M%S."
;;  `open-junk-file-find-file-function'
;;    *Function to open junk files.
;;    default = (quote find-file-other-window)

;;; Installation:
;;
;; Put open-junk-file.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'open-junk-file)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET open-junk-file RET
;;


;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x open-junk-file-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of open-junk-file.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "open-junk-file.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x open-junk-file-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:

;; $Log: open-junk-file.el,v $
;; Revision 1.3  2010/06/18 20:07:49  rubikitch
;; Rename variable: `open-junk-file-directory' -> `open-junk-file-format'
;; `open-junk-file-directory' is an alias now, so you do not have to modify your .emacs.
;;
;; Revision 1.2  2010/05/04 09:08:48  rubikitch
;; Added bug report command
;;
;; Revision 1.1  2010/03/05 22:17:32  rubikitch
;; Initial revision
;;

;;; Code:

(defvar open-junk-file-version "$Id: open-junk-file.el,v 1.3 2010/06/18 20:07:49 rubikitch Exp rubikitch $")
(eval-when-compile (require 'cl))
(defgroup open-junk-file nil
  "open-junk-file"
  :group 'emacs)
(defcustom open-junk-file-format "~/junk/%Y/%m/%d-%H%M%S."
  "*File format to put junk files with directory.
It can include `format-time-string' format specifications."
  :type 'string  
  :group 'open-junk-file)
(defvaralias 'open-junk-file-format 'open-junk-file-directory)
(defcustom open-junk-file-find-file-function 'find-file-other-window
  "*Function to open junk files."
  :type 'function  
  :group 'open-junk-file)

(defun open-junk-file ()
  "Open a new file whose filename is derived from current time.
 You can write short program in it. It helps to try-and-error programs.

For example, in Emacs Lisp programming, use M-x `open-junk-file'
instead of *scratch* buffer. The junk code is SEARCHABLE."
  (interactive)
  (let* ((file (format-time-string open-junk-file-format (current-time)))
         (dir (file-name-directory file)))
    (make-directory dir t)
    (funcall open-junk-file-find-file-function (read-string "Junk Code (Enter extension): " file))))

;;;; Bug report
(defvar open-junk-file-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar open-junk-file-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of open-junk-file.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"open-junk-file.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun open-junk-file-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   open-junk-file-maintainer-mail-address
   "open-junk-file.el"
   (apropos-internal "^open-junk-file-" 'boundp)
   nil nil
   open-junk-file-bug-report-salutation))

(provide 'open-junk-file)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "open-junk-file.el")
;;; open-junk-file.el ends here

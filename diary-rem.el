;;; diary-rem.el --- send mail showing upcoming diary entries

;; Copyright (C) 1997 Stephen Eglen

;; Author: Stephen Eglen stephen@anc.ed.ac.uk
;; Maintainer: Stephen Eglen stephen@anc.ed.ac.uk
;; Created: 19 Nov 1997
;; Version: 1.0
;; Keywords: diary calendar
;; location: http://www.anc.ed.ac.uk/~stephen/emacs
;; src: https://web.archive.org/web/20060720125152/http://www.damtp.cam.ac.uk/user/sje30/emacs/diary-rem.el [2026-03-14]
;; RCS: $Id: diary-rem.el,v 1.5 2000/06/19 09:52:09 stephen Exp $
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Find out what's coming up in the next few days in your diary file
;; by getting Emacs to mail you the diary entries.  This can be called
;; interactively from within Emacs, but was  designed for being
;; run daily from a cron job.

;; Tested on Emacs 19.34 and XEmacs 19.16
;; This has been submitted to RMS as a patch to diary-lib.el beyond 
;; Emacs 20.2

;;; Installation

;; Check the values of the variables:
;; - diary-mail-addr		
;; - diary-mail-days
;; - european-calendar-style	



;;; Variables


(defcustom diary-mail-addr 
  (if (boundp 'user-mail-address) user-mail-address nil)
  "*Email address that `diary-mail-entries' will send email to."
  :group 'diary
  :type 'string)

(defcustom diary-mail-days 7
  "*Number of days for `diary-mail-entries' to check."
  :group 'diary
  :type 'integer)

(defun diary-mail-entries (&optional ndays)
  "Send a mail message showing diary entries for next NDAYS days.
If no prefix argument is given, NDAYS is set to `diary-mail-days'.

You can call `diary-mail-entries' every night using an at/cron job.
For example, this script will run the program at 2am daily.  Since
`emacs -batch' does not load your `.emacs 'file, you
must ensure that all relevant variables are set, as done here.

#!/bin/sh
# diary-rem.sh -- repeatedly run the Emacs diary-reminder
emacs -batch \\
-eval \"(setq diary-mail-days 3 \\
             european-calendar-style t \\
             diary-mail-addr \\\"user@host.name\\\" )\" \\
-l diary-lib -f diary-mail-entries 
at -f diary-rem.sh 0200 tomorrow

You may have to tweak the syntax of the `at' command to suit your
system.  Alternatively, you can specify a cron entry:
0 1 * * * diary-rem.sh
to run it every morning at 1am."
  (interactive "p")
  (let ((text nil)
	;; Use the fancy-diary-display as it doesn't hide rest of
	;; diary file with ^M characters.  It also looks nicer.
	(diary-display-hook 'fancy-diary-display))	
    (if (not current-prefix-arg)
	(setq ndays diary-mail-days))
    (calendar)
    (view-diary-entries ndays)
    (set-buffer fancy-diary-buffer)
    (setq text (buffer-substring (point-min) (point-max)))

    ;; Now send text as a mail message.
    (mail)
    (mail-to)
    (insert diary-mail-addr)
    (mail-subject)
    (insert "Diary entries generated ")
    (insert (format-time-string "%a %d %b %y" (current-time)))
    (mail-text)
    (insert text)
    (mail-send-and-exit nil)))

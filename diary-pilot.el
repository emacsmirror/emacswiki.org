;;; diary-pilot.el --- export Emacs diary to Palm Pilot.

;; Copyright (C) 2000, 2001 Martin Schwenke

;; Maintainer: martin@meltin.net
;; Version: $Id: diary-pilot.el,v 1.8 2002/07/03 23:54:40 martins Exp $
;; Keywords: palm pilot diary
;; Requires: diary-lib, calendar

;; This is loosely based on Jamie Zawinski's bbdb-pilot.el.  Some of
;; the code is based on bits of diary-lib.el from Emacs 20.6.

;; This file is distributed under the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;  This program installs all of the events from the Emacs diary for
;;  the next N days (default 365) to the pilot datebook and then
;;  removes duplicates from the pilot datebook.  Yep, very hacky!

;;  Note that most versions of install-datebook have a bug that stops
;;  them handling events without an end time.  Bummer, huh?  If yours
;;  is broken and you want to know the fix, then please send me
;;  e-mail.

;;  INSTALLATION
;;    To install, simply copy this file into a directory in your
;;    load-path and add the following command in your .emacs file:
;;
;;    (autoload 'diary-to-pilot "diary-pilot" "Export Emacs diary to palm pilot" t)

;;; History:
;; 

;; Add history stuff here!!!
;; src: https://web.archive.org/web/20060721090212/http://meltin.net/hacks/emacs/src/diary-pilot.el [2026-03-14]

;;; Code:

(require 'diary-lib)
(require 'calendar)

(defgroup diary-pilot nil
  "Diary to Palm Pilot support"
  :group 'diary)

(defcustom diary-pilot-numdays 365
  "*Numbers of days of diary entries to export to pilot."
  :type 'integer
  :group 'diary-pilot)

(defcustom diary-pilot-alarm-time 10
  "*Alarm time for events exported to pilot."
  :type 'integer
  :group 'diary-pilot)

(defcustom diary-pilot-file (expand-file-name "diary-pilot" (temp-directory))
  "*File to use for exporting diary to pilot."
  :type 'string
  :group 'diary-pilot)

(defcustom diary-pilot-device "/dev/pilot"
  "*Device to use for exporting diary to pilot."
  :type 'string
  :group 'diary-pilot)

(defcustom diary-pilot-database "DatebookDB"
  "*Database to flush duplicates from after exporting diary to pilot."
  :type 'string
  :group 'diary-pilot)

(defvar diary-pilot-list-entries-hook '(sort-diary-entries)
  "*List of functions to call when processing diary to export it to pilot.
See `list-diary-entries-hook'.")

(defun diary-pilot-end-time (s)
  "Similar to function \\[diary-entry-time], except gets end time for event.
Assumes that start and end time are given in same format."
  (let ((case-fold-search nil))
    (cond ((string-match;; Hour and minute  XX:XXam or XX:XXpm
	    (concat 
	     "^[ \t]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>"
	     "[ \t]*-[ \t]*"
	     "\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>")
	     s)
	   (+ (* 100 (% (string-to-int
			   (substring s (match-beginning 4) (match-end 4)))
			  12))
	      (string-to-int (substring s (match-beginning 5) (match-end 5)))
	      (if (equal ?a (downcase (aref s (match-beginning 6))))
		  0 1200)))
	  ((string-match;; Military time  
	    (concat 
	     "^[ \t]*\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)"
	     "[ \t]*-[ \t]*"
	     "\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)")
	    s)
	   (+ (* 100 (string-to-int
		      (substring s (match-beginning 3) (match-end 3))))
	      (string-to-int (substring s (match-beginning 4) (match-end 4)))))
	  ((string-match;; Hour only  XXam or XXpm
	    (concat
	     "^[ \t]*\\([0-9]?[0-9]\\)\\([ap]\\)m\\>"
	     "[ \t]*-[ \t]*"
	     "\\([0-9]?[0-9]\\)\\([ap]\\)m\\>")
	    s)
	   (+ (* 100 (% (string-to-int
			 (substring s (match-beginning 3) (match-end 3)))
			12))
	      (if (equal ?a (downcase (aref s (match-beginning 4))))
		  0 1200)))
	  (t diary-unknown-time))));; Unrecognizable


(defun diary-pilot-text (s)
  "Similar to function \\[diary-entry-time], except gets text after times.
Assumes that start and end time are given in same format."
  (let ((case-fold-search nil))
    (subst-char-in-string
     ?\t ?\040
     (cond ((string-match;; Hour and minute  XX:XXam or XX:XXpm
	     (concat 
	      "^[ \t]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>"
	      "\\(" "[ \t]*-[ \t]*"
	      "\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>"
	      "\\)?[ \t]*\\(.*\\)$")
	     s)
	    (substring s (match-beginning 8) (match-end 8)))
	   ((string-match;; Military time  
	     (concat 
	      "^[ \t]*\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)"
	      "\\(" "[ \t]*-[ \t]*"
	      "\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)"
	      "\\)?[ \t]*\\(.*\\)$")
	     s)
	    (substring s (match-beginning 6) (match-end 6)))
	   ((string-match;; Hour only  XXam or XXpm
	     (concat
	      "^[ \t]*\\([0-9]?[0-9]\\)\\([ap]\\)m\\>"
	      "\\(" "[ \t]*-[ \t]*"
	      "\\([0-9]?[0-9]\\)\\([ap]\\)m\\>"
	      "\\)?[ \t]*\\(.*\\)$")
	     s)
	    (substring s (match-beginning 6) (match-end 6)))
	   (t
	    s)))))
  
  (defun diary-pilot-format (diary)
  "Inserts an `install-datebook'-compatible description of the value
of DIARY into the current buffer."

  (let* ((calendar-date-display-form '((format "%04d/%02d/%02d"
					       (string-to-int year)
					       (string-to-int month)
					       (string-to-int day))))
	 (thang (cadr diary))
	 (date  (calendar-date-string (car diary) ))
	 (ndate (calendar-date-string
		 (calendar-gregorian-from-absolute
		  (1+ (calendar-absolute-from-gregorian (car diary))))))
	 (start (diary-entry-time thang))
	 (end   (diary-pilot-end-time thang))
	 (text  (diary-pilot-text thang))
	 (print-escape-newlines nil)
	 (startstring date)
	 (endstring   "")
	 (alarmstring "")
	 (stdalarm    (format "%dm" diary-pilot-alarm-time)))

    (if (/= start diary-unknown-time)
	(progn
	  ;; Start time is valid, incorporate into startstring.
	  (setq startstring
		(format "%s %02d:%02d" date
			(/ start 100) (% start 100)))
	  (setq alarmstring stdalarm)
	  (if (= end diary-unknown-time)
	      ;; End time is bogus/missing.  Use startstring.
	      (setq endstring startstring)
	    ;; End time is cool.  If it is greater than start time
	    ;; then things are as expected, otherwise we need to hack
	    ;; in the next day!
	    (if (> end start)
		(setq endstring
		      (format "%s %02d:%02d" date
			      (/ end 100) (% end 100)))
	      (setq endstring
		    (format "%s %02d:%02d" ndate
			    (/ end 100) (% end 100)))))))
    (insert (format "%s\t%s\t\%s\t%s\n"
		    startstring
		    endstring
		    alarmstring
		    text))))

(defun diary-to-pilot-file (filename)
  (interactive "FWrite install-datebook file: ")

  (message "Entries...")
  (let* ((diary-display-hook      'ignore)
	 (diary-hook              nil)
	 (list-diary-entries-hook diary-pilot-list-entries-hook)
	 (records                 (list-diary-entries
				   (calendar-current-date)
				   diary-pilot-numdays)))
    (save-excursion
      (show-all-diary-entries)
      (set-buffer (find-file-noselect filename))
      (erase-buffer)
      (let ((len (length records))
	    (i 0))
	(while records
	  (message "%d%%..." (/ (* 100 i) len))
	  (diary-pilot-format (car records))
	  (setq records (cdr records)
		i (1+ i))))
      (save-buffer)
      (kill-buffer (current-buffer)))
    filename))

(defun diary-to-pilot ()
  "Push the diary out to the Pilot."
  (interactive)
  (message "Selecting records...")
    
  (diary-to-pilot-file diary-pilot-file)
  (let ((tz (getenv "TZ")))
    (setenv "TZ" "GMT")
    (shell-command
     (concat
      "echo \"Press the HotSync button now to export diary.\""
      " ; "
      "install-datebook " diary-pilot-device " " diary-pilot-file
      " ; "
      ;;" ; rm " file
      "echo \"Press the HotSync button again to remove duplicates.\""
      " ; "
      "pilot-dedupe " diary-pilot-device " " diary-pilot-database
      " &"))
    (setenv "TZ" tz)))

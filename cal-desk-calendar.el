;;; cal-desk-calendar.el --- Desk calendar style extensions to Emacs' Calendar/Diary

;; Copyright (C) 1995, 1999 D. Dale Gulledge.
;;
;; Author: D. Dale Gulledge <dsplat@rochester.rr.com>
;; Version: 0.8 (1999/08/11)
;; Keywords: calendar
;; Human-Keywords: desk calendar, diary

;; This file is derived from functions in the Calendar/Diary facility
;; of GNU Emacs.  The copyright is currently held by the author,
;; D. Dale Gulledge, pending assignment to the Free Software
;; Foundation.  It may be used under the terms of the GNU General
;; Public License (also known as the GPL or GNU Copyleft).
;;
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

;; To activate this feature, include the following lines in your
;; .emacs: 
;;
;; (load-library "cal-desk-calendar")
;; (add-hook 'diary-display-hook 'sort-diary-entries)
;; (add-hook 'diary-display-hook 'fancy-schedule-display-desk-calendar t)
;;
;; It will replace the normal diary display mode when you run either the d
;; command from the Calendar buffer after M-X calendar, or when you directly
;; format your diary using M-x diary.

;; To Do:
;;
;; 1) Rewrite display engine into two separate passes.  The first
;;    builds a sorted list of appointments and the second displays it
;;    in the preferred style.  This will allow the first pass to be
;;    used for various different displays, including generation of
;;    LaTeX source for various formats.
;; 2) Write some comments describing some of the obscure portions, if
;;    they survive the rewrite.
;; 3) Add a variable to specify the increments in which the grid will
;;    be extended instead of simply using
;;    diary-schedule-interval-time.  That would allow, for example, an
;;    interval of 30 minutes, but extending the calendar in increments
;;    of an hour. 
;; 4) Add some functions that directly implement some of the recurring
;;    event options described in RFC 2446 (especially section
;;    4.8.5.4).  All of these can be constructed with sexp entries,
;;    but some of the code gets a bit hairy to embed in the diary file
;;    for each entry.  It seems better to have a library of functions
;;    like the ones that are still tagging along at the end of this
;;    file.
;; 5) Implement handling of multiple timezones explicitly.  The real
;;    problem is when the calendar dates for a particular moment in
;;    time differ between the zones.  This will probably require
;;    grabbing the events for the previous and following dates and
;;    checking their times after the TZ calculations.
;; 6) Add an escape mechanism for the time format strings, or better still,
;;    convert to the mechanism that Ed is already using in the diary code.
;; 7) Prep for internationalization.  Pull text strings out into variables.
;;    Doctor the main diary code to allow internationalization of the
;;    sunrise/sunset messages, lunar phases, solstices and equinoxes.
;; 8) Add an option for a double | for periods containing overlapping events.
;; 9) Clean up the code by making variables local where possible.
;; 10) Pull the common code for dealing with the out of range events into a
;;    separate function instead of having two identical blocks.
;; x 11) Make the | configurable.
;; 12) Add an option to allow a fixed string to replace appointment text for
;;    diary entries.  This would allow producing a calendar that indicated
;;    which times were committed and which were free without revealing the
;;    details of those appointments, suitable for mailing to someone for
;;    setting up meetings.
;; x 13) Make special time patterns (Workday, Morning, etc.) configurable.  This
;;    is important for internationalization anyway.

(require 'calendar)
;;(require 'diary)
(require 'diary-lib)
;;(require 'diary-ins)
(require 'solar)
(require 'lunar)

(defconst diary-subsequent-date-prefix-string "\n\f"
  "The string which preceeds each day\'s diary except the first in the Fancy Diary Buffer.")

(defvar diary-default-schedule-start-time 800
  "*The time to which diary-schedule-start-time is set for each day\'s schedule.")

(defvar diary-default-schedule-stop-time 1730
  "*The time to which diary-schedule-stop-time is set for each day\'s schedule.")

(setq diary-schedule-start-time diary-default-schedule-start-time
      diary-schedule-stop-time diary-default-schedule-stop-time)

(defvar diary-schedule-interval-time 30
  "*The number of minutes per interval in the day\'s schedule.")

(defvar diary-schedule-line-offset 2
  "The line in the fancy diary buffer on which the schedule starts.")

(defvar diary-schedule-expand-grid t
  "*Determines whether the grid will be expanded to fit appointments outside the range.")

(defvar diary-morning-times '(800 1200)
  "*The times that appointments labelled Morning begin and end.")

(defvar diary-afternoon-times '(1300 1700)
  "*The times that appointments labelled Afternoon begin and end.")

(defvar diary-workday-times '(800 1200 1300 1700)
  "*The times that appointments labelled Workday begin and end.")

(defvar diary-all-day-times '(800 1700)
  "*The times that appointments labelled All Day begin and end.")

(defvar diary-schedule-time-display-format "24:mm"
  "*The format in which to print the times on the fancy schedule.
Options are 24:mm, 12:mm or \"12:mm ap\".")

(defvar diary-schedule-time-overflow-display-format "  :mm"
  "*The format in which to print the times on the fancy schedule.
Options are 24:mm, 12:mm or \"12:mm ap\".")

(defvar diary-schedule-appointment-separator "|"
  "*This string will be used to separate the time from the text of each diary
entry.")

(defvar diary-schedule-time-odd-end-time-format
  (concat "  :mm " diary-schedule-appointment-separator " ")
  "*The format in which odd end times are printed.  Should include the trailing
\"  | \" and may include time formats to the right of that as well.")

(defvar diary-schedule-format-odd-end-time-with-hours nil
  "*Whether to force hours to be printed in odd end times even when the end
time does not fall on an hour boundary.")

(defvar diary-schedule-fill-prefix-for-broken-lines
  (concat "      " diary-schedule-appointment-separator "   ")
  "*The fill prefix to use at the beginning of long lines that are broken to fit.
The suggested value for this is the same as the beginning of all of the other lines
with the time replaced by spaces and a couple of extra spaces after the |.")

(defvar diary-am-string "am"
  "*The string to display for morning times when the am format option is chosen.")

(defvar diary-pm-string "pm"
  "*The string to display for afternoon times when the am format option is chosen.")

(defvar diary-AM-string "AM"
  "*The string to display for morning times when the AM format option is chosen.")

(defvar diary-PM-string "PM"
  "*The string to display for afternoon times when the AM format option is chosen.")

(defvar diary-schedule-first-time-format nil
  "*The format for the first time of the day to be printed in.
nil indicates that the old method of using either
diary-schedule-time-display-format or
diary-schedule-time-overflow-display-format should be employed.  Otherwise,
the same formatting options that are used for those variables may be employed.")

(defvar diary-duplicate-time-display nil
  "*Whether to display the time on second and subsequent lines when the time is
the same.")

(defvar diary-schedule-odd-times-get-separate-entry t
  "*Whether times that do not fall on an interval boundary get a separate line.")

(defvar diary-schedule-print-odd-end-time t
  "*Whether the end time of a period that does not fall on an interval boundary
gets printed on a separate line.")

(defvar diary-schedule-print-minute-after-odd-end-time nil
  "*Whether the first minute after an end time that doesn't fall on an interval
boundary gets printed on a separate line.")

(defvar diary-schedule-first-time-always-has-hours t
  "*Whether the time printed for the first interval of the day should contain the
hour regardless of whether it falls on an hour boundary.")

(defconst diary-schedule-entry-separator-string "%%DIARY-ENTRY-SEPARATOR%%"
  "Used internally to separate entries during formatting to allow easy
calculation of line numbers.")

(defvar diary-schedule-sunrise-sunset-pattern "^[ 	]*Sunrise \\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\> *\\(([A-Za-z 0-9+-]*)\\)?, sunset \\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\> (\\([A-Za-z 0-9+-]*)\\)?"
  "*Pattern to match for %%(diary-sunrise-sunset) diary entries.")

(defvar diary-schedule-lunar-phase-pattern "^[ 	]*\\(New\\|First Quarter\\|Full\\|Last Quarter\\) Moon \\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\> ([A-Z0-9+-]*)"
  "*Pattern to match for %%(diary-phases-of-moon) diary entries")

(defvar diary-schedule-equinox-solstice-pattern "^[ 	]*\\(Vernal Equinox\\|Summer Solstice\\|Autumnal Equinox\\|Winter Solstice\\) \\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\> ([A-Z0-9+-]*)"
  "*Pattern to match for %%(diary-equinoxes-solstices) diary entries")

(defvar diary-schedule-morning-pattern "^[ 	]*\\([Mm][oO][rR][nN][iI][nN][gG]\\)"
  "*Pattern to match for Morning diary entries.")

(defvar diary-schedule-afternoon-pattern "^[ 	]*\\([Aa][fF][tT][eE][rR][nN][oO][oO][nN]\\)"
  "*Pattern to match for Afternoon diary entries.")

(defvar diary-schedule-workday-pattern "^[ 	]*\\([Ww][oO][rR][kK][dD][aA][yY]\\)"
  "*Pattern to match for Workday diary entries.")

(defvar diary-schedule-all-day-pattern "^[ 	]*\\([Aa][lL][lL] [Dd][aA][yY]\\)"
  "*Pattern to match for All Day diary entries.")

(defvar diary-schedule-place-out-of-bounds-entries-last t
  "*Whether to place entries which fall outside the schedule range or which
have no specified time after the grid for the day.
t indicates after, nil indicates before")

(defvar diary-schedule-sunrise-string "Sunrise"
  "*A string containing the word for sunrise in the user\'s preferred language.")

(defvar diary-schedule-sunset-string "Sunset"
  "*A string containing the word for sunset in the user\'s preferred language.")

(defvar diary-schedule-astronomical-event-time-format "HH:MM"
  "*The format for times in sunrise/sunset, lunar phase, and equinox/solstice
diary entries.")

(defvar diary-schedule-daylight-hours-string "hours of daylight"
  "*A string containing the phrase for \"hours of daylight\" in the user\'s
preferred language.")

(setq calendar-holidays
  (append general-holidays local-holidays other-holidays solar-holidays))

(defun within-3-month-range (entry)
  "Determine if a date falls within a month either way of the current month."
  (if (and (= (nth 2 (car entry)) displayed-year)
	   (<= (abs (- (nth 1 (car entry)) displayed-month)) 1))
      entry
    nil))

(defun diary-entry-time (s)
  "Time at the beginning of the string S in a military-style integer.
For example, returns 1325 for 1:25pm.  Returns -9999 if no time is recognized.
The recognized forms are XXXX or X:XX or XX:XX (military time), XXam or XXpm,
and XX:XXam or XX:XXpm."
  (cond ((string-match;; Military time  
          "^[ 	]*\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)\\(\\>\\|[^ap]\\)" s)
         (+ (* 100 (string-to-int
                    (substring s (match-beginning 1) (match-end 1))))
            (string-to-int (substring s (match-beginning 2) (match-end 2)))))
        ((string-match;; Hour only  XXam or XXpm
          "^[ 	]*\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" s)
         (+ (* 100 (% (string-to-int
                         (substring s (match-beginning 1) (match-end 1)))
                        12))
            (if (string-equal "a"
                              (substring s (match-beginning 2) (match-end 2)))
                0 1200)))
        ((string-match;; Hour and minute  XX:XXam or XX:XXpm
          "^[ 	]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>" s)
         (+ (* 100 (% (string-to-int
                         (substring s (match-beginning 1) (match-end 1)))
                        12))
            (string-to-int (substring s (match-beginning 2) (match-end 2)))
            (if (string-equal "a"
                              (substring s (match-beginning 3) (match-end 3)))
                0 1200)))
        (t -9999)));; Unrecognizable

(defun diary-format-time (format time display-hour)
  "Using FORMAT produce string with formatted TIME.  DISPLAY_HOUR determines whether to display the hour.
  Options:
    hh, 12	Hour on 12 hour clock with leading space.
    0h		Hour on 12 hour clock with leading 0.
    HH, 24	Hour on 24 hour clock with leading space.
    0H		Hour on 24 hour clock with leading 0.
    mm, MM	Minutes with leading 0.
    ap, am, pm	am|pm
    AP, AM, PM	AM|PM"
  (let ((hour (/ time 100))
	(minute (% time 100)))
    (if (< (length format) 2)
	format
      (let ((form (substring format 0 2)))
	(cond
	 ((or (string= form "hh") (string= form "12"))
	  (concat (if display-hour (format "%2d" (1+ (% (1- hour) 12))) "  ")
		  (diary-format-time (substring format 2) time display-hour)))
	 ((or (string= form "HH") (string= form "24"))
	  (concat (if display-hour (format "%2d" hour) "  ")
		  (diary-format-time (substring format 2) time display-hour)))
	 ((string= form "0h")
	  (concat (if display-hour (format "%02d" (1+ (% (1- hour) 12))) "  ")
		  (diary-format-time (substring format 2) time display-hour)))
	 ((string= form "0H")
	  (concat (if display-hour (format "%02d" hour) "  ")
		  (diary-format-time (substring format 2) time display-hour)))
	 ((or (string= form "mm") (string= form "MM"))
	  (concat (format "%02d" minute)
		  (diary-format-time (substring format 2) time display-hour)))
	 ((or (string= form "ap") (string= form "am") (string= form "pm"))
	  (concat (if (>= hour 12) diary-pm-string diary-am-string)
		  (diary-format-time (substring format 2) time display-hour)))
	 ((or (string= form "AP") (string= form "AM") (string= form "PM"))
	  (concat (if (>= hour 12) diary-PM-string diary-AM-string)
		  (diary-format-time (substring format 2) time display-hour)))
	 (t (concat (substring format 0 1)
		    (diary-format-time (substring format 1) time display-hour))))))))

(defun diary-display-grid (start stop by display-first-hour)
  "Display a schedule of diary times from START to STOP in BY minute increments.
It is not inclusive of the STOP time.  START and STOP are military time
expressed as integers.  This a a fancy display style based on a desk calendar."
  (message (format "(diary-display-grid start=%d stop=%d by=%d display-first-hour=%s)" start stop by display-first-hour))
  (let ((hour (/ start 100))
	(minute (% start 100)))
    (insert (diary-format-time diary-schedule-time-display-format
			 start
			 (or (= minute 0) display-first-hour))
	    " \n")
    (if (>= (setq minute (+ minute by)) 60)
	(setq minute (- minute 60)
	      hour (1+ hour)))
    (while (< (+ (* hour 100) minute) stop)
      (insert (diary-format-time diary-schedule-time-display-format
			   (+ (* hour 100) minute)
			   (= minute 0))
	      " \n")
      (if (>= (setq minute (+ minute by)) 60)
	  (setq minute (- minute 60)
		hour (1+ hour))))))
;;    (insert "\n")))

(defun diary-calc-display-line (start time stop by)
  "Calculate the line number within the diary buffer on which an event at TIME
will appear given a schedule starting time of START and an interval of BY minutes.
START and TIME are military time expressed as integers."
  (let ((minutes (- (% time 100) (% start 100))))
    (if (> time 0)
	(+
	 1
	 (*
	  (/ 60 by)
	  (- (/ time 100) (/ start 100)))
	 (/
	  (if (< minutes 0)
	      (- minutes by -1)
	    minutes)
	  by))
      time)))

(defun diary-minutes-since-midnight (military-time)
  (+ (% military-time 100)
     (* (/ military-time 100) 60)))

(defun diary-military-time (minutes-since-midnight)
  (+ (% minutes-since-midnight 60)
     (* (/ minutes-since-midnight 60) 100)))

(defun diary-display-at (start stop by begin end text offset)
  "Display a diary entry with the text TEXT running from BEGIN to END on a
schedule running from START to STOP in intervals of BY minutes.  All times
are in military time expressed as integers."
  (let* ((start-line (+ offset (diary-calc-display-line start begin stop by)))
	 (line (1+ start-line))
	 (original-begin begin)
	 (original-end end)
	 (end-line (+ offset (diary-calc-display-line start end stop by))))
    (if (> (% (diary-minutes-since-midnight end) by) 0)
	(setq end-line (1+ end-line)))
    (cond ((= begin -9999)
	   (message (setq diary-schedule-out-of-bounds-entry-text
		 (concat diary-schedule-out-of-bounds-entry-text
			 "\n"
			 text))))
;	   (progn
;	     (goto-char (point-max))
;	     (insert text ?\n)))
	  ((< begin start)
	   (setq begin-in-minutes-from-midnight
		 (* (/ (diary-minutes-since-midnight begin) by) by)
		 begin (diary-military-time begin-in-minutes-from-midnight))
	   (if diary-schedule-expand-grid
	       (progn
		 (goto-line (1+ offset))
		 (beginning-of-line)
		 (diary-display-grid begin
				     diary-schedule-start-time
				     diary-schedule-interval-time
				     nil)
		 (setq diary-schedule-start-time begin)
		 (diary-display-at begin stop by original-begin end text offset))
	     (progn
	       (goto-char (point-max))
	       (insert text ?\n))))
	  ((>= end stop)
	   (setq end-in-minutes-from-midnight
; (if (= end stop) by 0) extracted from + below.
		 (* (/ (+ (* (/ end 100) 60) (% end 100) by) by) by))
	   (setq new-end (diary-military-time end-in-minutes-from-midnight))
	   (if diary-schedule-expand-grid
	       (progn
		 (goto-line (+ offset (diary-calc-display-line start stop stop by)))
		 (beginning-of-line)
		 (diary-display-grid diary-schedule-stop-time
				     new-end
				     diary-schedule-interval-time
				     nil)
		 (insert ?\n)
		 (setq diary-schedule-stop-time new-end)
		 (diary-display-at start diary-schedule-stop-time by begin end text offset))
	     (progn
	       (goto-char (point-max))
	       (insert text ?\n))))
	  (t
	   (let ((start-column (1+ (length diary-schedule-time-display-format))))
	     (goto-line start-line)
	     (end-of-line)
	     (message "  original-begin = %d" original-begin)
	     (if (and (<= (current-column) start-column)
		      (or
		       (not diary-schedule-odd-times-get-separate-entry)
		       (= 0 (% (+
				(* (/ original-begin 100) 60)
				(% original-begin 100)) by))))
		 (insert diary-schedule-appointment-separator " " text)

	       ; Let's take a little of the mystery out of this.  The string
	       ; %%DIARY-ENTRY-SEPARATOR%% was chosen as an arbitrary string
	       ; that is unlikely to appear in a diary entry.  It is placed on
	       ; a line to indicate a need for later splitting.  This permits
	       ; easy calculation of the lines to place entries on without
	       ; knowledge of how the lines may have already been split.  The
	       ; split itself is performed in
	       ; fancy-schedule-display-desk-calendar.

	       (insert diary-schedule-entry-separator-string
		       (diary-format-time
			diary-schedule-time-overflow-display-format
			begin
			nil)
		       " " diary-schedule-appointment-separator " "
		       text))
	     (while (< line end-line)
	       (forward-line 1)
	       (end-of-line)
	       (if (= (current-column) start-column)
		   (insert diary-schedule-appointment-separator " "))
	       (setq line (1+ line)))
	     (setq original-end-minutes-from-midnight
		   (+ (* (/ original-end 100) 60) (% original-end 100)))
	     (if (and diary-schedule-odd-times-get-separate-entry
		      (/= original-begin original-end)
		      (/= 0 (% original-end-minutes-from-midnight by)))
		 (progn
		   (if diary-schedule-print-odd-end-time
		       (insert diary-schedule-entry-separator-string
			       (diary-format-time
				diary-schedule-time-odd-end-time-format
				original-end
				diary-schedule-format-odd-end-time-with-hours)))
		   (if (and diary-schedule-print-minute-after-odd-end-time
			    (/= 0
				(% (1+ original-end-minutes-from-midnight)
				   by)))
		       (progn
			 (setq one-minute-after
			       (+
				(* (/ (1+ original-end-minutes-from-midnight)
				      60)
				   100)
				(% (1+ original-end-minutes-from-midnight) 60)))
			 (insert diary-schedule-entry-separator-string
				 (diary-format-time
				  diary-schedule-time-overflow-display-format
				  one-minute-after
				  nil)
				 " "))))))))))

(defun display-schedule-entry (start stop by entry-text offset)
  (let ((times (diary-entry-times entry-text)))
    (cond ((= (length times) 3)
	   (diary-display-at start stop by
			     (car times) (car (cdr times)) (car (cdr (cdr times)))
			     offset))
	  ((= (length times) 6)
	   (progn
	     (diary-display-at diary-schedule-start-time diary-schedule-stop-time by
			       (car times) (car (cdr times)) (car (cdr (cdr times)))
			       offset)
	     (diary-display-at diary-schedule-start-time diary-schedule-stop-time by
			       (nth 3 times) (nth 4 times) (nth 5 times)
			       offset))))))

(defun diary-schedule-display (entry-text)
  (if (string-match "\\(\n\\)" entry-text)
      (let ((line-end-position (match-end 1)))
	(display-schedule-entry diary-schedule-start-time
				diary-schedule-stop-time
				diary-schedule-interval-time
				(substring entry-text 0 (1- line-end-position))
				diary-schedule-line-offset)
	(diary-schedule-display (substring entry-text (1+ line-end-position))))
    (display-schedule-entry diary-schedule-start-time
			    diary-schedule-stop-time
			    diary-schedule-interval-time
			    entry-text
			    diary-schedule-line-offset)))

(defun diary-build-time-list (times s)
  "Build the result of diary-entry-times from a list of 2 or 4 TIMES and a string S
describing the event.  The times are in start stop pairs."
  (cond ((= (length times) 2)
	 (list (car times) (nth 1 times) s))
	((= (length times) 4)
	 (list (car times) (nth 1 times) s (nth 2 times) (nth 3 times) s))
	(t (list -9999 -9999 s))))

(defun diary-entry-times (s)
  "List of times at the beginning of the string S in military-style integers.
For example, returns 1325 for 1:25pm.  Returns -9999 if no time is recognized.
The recognized forms are XXXX or X:XX or XX:XX (military time), XXam or XXpm,
and XX:XXam or XX:XXpm.  If a range is given, the list contains two elements
which will be the start and end of the range.  If only one time is given, both
elements of the list will contain the time given."
  (cond
   ;; Hour and minute range XX:XX-XX:XX[ap]m
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)-\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>"
     s)
    (list
     (+ (* 100 (% (string-to-int
		   (substring s (match-beginning 1) (match-end 1)))
		  12))
	(string-to-int (substring s (match-beginning 2) (match-end 2)))
	(if (string-equal "a"
			  (substring s (match-beginning 5) (match-end 5)))
	    0 1200))
     (+ (* 100 (% (string-to-int
		   (substring s (match-beginning 3) (match-end 3)))
		  12))
	(string-to-int (substring s (match-beginning 4) (match-end 4)))
	(if (string-equal "a"
			  (substring s (match-beginning 5) (match-end 5)))
	    0 1200))
     (substring s (+ 2 (match-end 5)))))

   ;; Military time range
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)-\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)\\(\\|[^ap]\\)"
     s)
    (list
     (+ (* 100 (string-to-int
	       (substring s (match-beginning 1) (match-end 1))))
       (string-to-int (substring s (match-beginning 2) (match-end 2))))
     (+ (* 100 (string-to-int
	       (substring s (match-beginning 3) (match-end 3))))
       (string-to-int (substring s (match-beginning 4) (match-end 4))))
     (substring s (1+ (match-end 4)))))

   ;; Hour range HH[ap]m-HH[ap]m
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\)\\([ap]\\)m-\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" s)
    (list
     (+ (* 100 (% (string-to-int
		  (substring s (match-beginning 1) (match-end 1)))
		 12))
       (if (string-equal "a"
			 (substring s (match-beginning 2) (match-end 2)))
	   0 1200))
     (+ (* 100 (% (string-to-int
		  (substring s (match-beginning 3) (match-end 3)))
		 12))
       (if (string-equal "a"
			 (substring s (match-beginning 4) (match-end 4)))
	   0 1200))
     (substring s (+ 2 (match-end 4)))))

   ;; Hour range HH-HH[ap]m
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\)-\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" s)
    (list
     (+ (* 100 (% (string-to-int
		  (substring s (match-beginning 1) (match-end 1)))
		 12))
       (if (string-equal "a"
			 (substring s (match-beginning 3) (match-end 3)))
	   0 1200))
     (+ (* 100 (% (string-to-int
		  (substring s (match-beginning 2) (match-end 2)))
		 12))
       (if (string-equal "a"
			 (substring s (match-beginning 3) (match-end 3)))
	   0 1200))
     (substring s (+ 2 (match-end 3)))))

   ;; Hour and minute range XX:XX[ap]m-XX:XX[ap]m
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m-\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>"
     s)
    (list
     (+ (* 100 (% (string-to-int
		   (substring s (match-beginning 1) (match-end 1)))
		  12))
	(string-to-int (substring s (match-beginning 2) (match-end 2)))
	(if (string-equal "a"
			  (substring s (match-beginning 3) (match-end 3)))
	    0 1200))
     (+ (* 100 (% (string-to-int
		   (substring s (match-beginning 4) (match-end 4)))
		  12))
	(string-to-int (substring s (match-beginning 5) (match-end 5)))
	(if (string-equal "a"
			  (substring s (match-beginning 6) (match-end 6)))
	    0 1200))
     (substring s (+ 2 (match-end 6)))))

   ;; Military time
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)\\(\\>\\|[^ap]\\)" s)
    (let ((time (+ (* 100 (string-to-int
			   (substring s (match-beginning 1) (match-end 1))))
		   (string-to-int (substring s (match-beginning 2) (match-end 2))))))
      (list time time (substring s (1+ (match-end 2))))))

   ;; Hour only XXam or XXpm
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" s)
    (let ((time (+ (* 100 (% (string-to-int
			      (substring s (match-beginning 1) (match-end 1)))
			     12))
		   (if (string-equal "a"
				     (substring s (match-beginning 2) (match-end 2)))
		       0 1200))))
      (list time time (substring s (+ 2 (match-end 2))))))

   ;; Hour and minute XX:XXam or XX:XXpm
   ((string-match
     "^[ 	]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>" s)
    (let ((time (+ (* 100 (% (string-to-int
			      (substring s (match-beginning 1) (match-end 1)))
			     12))
		   (string-to-int (substring s (match-beginning 2) (match-end 2)))
		   (if (string-equal "a"
				     (substring s (match-beginning 3) (match-end 3)))
		       0 1200))))
      (list time time (substring s (+ 2 (match-end 3))))))

   ;; Sunrise/sunset produced by %%(diary-sunrise-sunset)
   ((string-match
;;     "^[ 	]*Sunrise \\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\> ([A-Za-z 0-9+-]*), sunset \\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\> \\(([A-Za-z 0-9+-]*)\\)"
     diary-schedule-sunrise-sunset-pattern s)
    (let ((sunrise-time (+ (* 100 (% (string-to-int
				      (substring s (match-beginning 1) (match-end 1)))
				     12))
			   (string-to-int (substring s (match-beginning 2) (match-end 2)))
			   (if (string-equal "a"
					     (substring s (match-beginning 3) (match-end 3)))
			       0 1200)))
	  (sunset-time (+ (* 100 (% (string-to-int
				     (substring s (match-beginning 5) (match-end 5)))
				    12))
			  (string-to-int (substring s (match-beginning 6) (match-end 6)))
			  (if (string-equal "a"
					    (substring s (match-beginning 7) (match-end 7)))
			      0 1200))))
;      (list sunrise-time sunrise-time (concat "Sunrise "
;					      (substring s (match-beginning 1) (match-end 2)) "am "
;					      (substring s (1+ (match-end 7))))
;	    sunset-time sunset-time (concat "Sunset "
;					    (substring s (match-beginning 5) (match-end 6)) "pm "
;					    (substring s (1+ (match-end 7)))))))
      (list sunrise-time sunrise-time
	    (diary-desk-sunrise-sunset-entry-text "Sunrise"
						  sunrise-time
						  (substring s (match-beginning 4) (match-end 4))
						  (substring s (match-beginning 8) (match-end 8)))
	    sunset-time sunset-time
	    (diary-desk-sunrise-sunset-entry-text "Sunset"
						  sunset-time
						  (substring s (match-beginning 4) (match-end 4))
						  (substring s (match-beginning 8) (match-end 8))))))						  

   ;; Lunar phase produced by %%(diary-phases-of-moon)
   ((string-match
;;     "^[ 	]*\\(New\\|First Quarter\\|Full\\|Last Quarter\\) Moon \\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\> ([A-Z0-9+-]*)" s)
     diary-schedule-lunar-phase-pattern s)
    (let ((time (+ (* 100 (% (string-to-int
			      (substring s (match-beginning 2) (match-end 2)))
			     12))
		   (string-to-int (substring s (match-beginning 3) (match-end 3)))
		   (if (string-equal "a"
				     (substring s (match-beginning 4) (match-end 4)))
		       0 1200))))
      (list time time s)))

   ;; Equinox/Solstice produced by %%(diary-equinoxes-solstices)
   ((string-match
;;     "^[ 	]*\\(Vernal Equinox\\|Summer Solstice\\|Autumnal Equinox\\|Winter Solstice\\) \\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\> ([A-Z0-9+-]*)" s)
     diary-schedule-equinox-solstice-pattern s)
    (let ((time  (+ (* 100 (% (string-to-int
			      (substring s (match-beginning 2) (match-end 2)))
			     12))
		   (string-to-int (substring s (match-beginning 3) (match-end 3)))
		   (if (string-equal "a"
				     (substring s (match-beginning 4) (match-end 4)))
		       0 1200))))
      (list time time s)))

   ;; Morning
   ((string-match ;; "^[ 	]*\\([Mm][oO][rR][nN][iI][nN][gG]\\)" s)
     diary-schedule-morning-pattern s)
    (diary-build-time-list diary-morning-times (substring s (1+ (match-end 1)))))

   ;; Afternoon
   ((string-match ;; "^[ 	]*\\([Aa][fF][tT][eE][rR][nN][oO][oO][nN]\\)" s)
     diary-schedule-afternoon-pattern s)
    (diary-build-time-list diary-afternoon-times (substring s (1+ (match-end 1)))))

   ;; Workday
   ((string-match ;; "^[ 	]*\\([Ww][oO][rR][kK][dD][aA][yY]\\)" s)
     diary-schedule-workday-pattern s)
    (diary-build-time-list diary-workday-times (substring s (1+ (match-end 1)))))

   ;; All Day
   ((string-match ;; "^[ 	]*\\([Aa][lL][lL] [Dd][aA][yY]\\)" s)
     diary-schedule-all-day-pattern s)
    (diary-build-time-list diary-all-day-times (substring s (1+ (match-end 1)))))

   ;; Unrecognizable
   (t (list -9999 -9999 s))))

(defun fancy-schedule-display-desk-calendar ()
  "Prepare a diary buffer with relevant entries in a fancy, noneditable form.
This function is provided for optional use as the `diary-display-hook'."
  (setq diary-schedule-out-of-bounds-entry-text "")
  (if (or (not diary-entries-list)
          (and (not (cdr diary-entries-list))
               (string-equal (car (cdr (car diary-entries-list))) "")))
      (let* ((holiday-list (if holidays-in-diary-buffer
                               (check-calendar-holidays original-date)))
             (msg (format "No diary entries for %s %s"
                          (concat date-string (if holiday-list ":" ""))
                          (mapconcat 'identity holiday-list "; "))))
        (if (<= (length msg) (frame-width))
            (message msg)
          (set-buffer (get-buffer-create holiday-buffer))
          (setq buffer-read-only nil)
          (calendar-set-mode-line date-string)
          (erase-buffer)
          (insert (mapconcat 'identity holiday-list "\n"))
          (goto-char (point-min))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)
          (display-buffer holiday-buffer)
          (message  "No diary entries for %s" date-string)))
    (save-excursion;; Turn off selective-display in the diary file's buffer.
      (set-buffer (find-buffer-visiting (substitute-in-file-name diary-file)))
      (let ((diary-modified (buffer-modified-p)))
        (subst-char-in-region (point-min) (point-max) ?\^M ?\n t)
        (setq selective-display nil)
        (kill-local-variable 'mode-line-format)
        (set-buffer-modified-p diary-modified)))
    (save-excursion;; Prepare the fancy diary buffer.
      (set-buffer (get-buffer-create fancy-diary-buffer))
      (setq buffer-read-only nil)
      (make-local-variable 'mode-line-format)
      (calendar-set-mode-line " Desk Calendar ")
      (erase-buffer)
      (let ((entry-list diary-entries-list)
            (holiday-list)
            (holiday-list-last-month 1)
            (holiday-list-last-year 1)
            (date (list 0 0 0)))
        (while entry-list
          (if (not (calendar-date-equal date (car (car entry-list))))
              (progn
		(message "Adding %s" diary-schedule-out-of-bounds-entry-text)
		(if (> (length diary-schedule-out-of-bounds-entry-text) 0)
		    (progn
		      (if diary-schedule-place-out-of-bounds-entries-last
			  (goto-char (point-max))
			(goto-line (1+ diary-schedule-line-offset)))
		      (insert diary-schedule-out-of-bounds-entry-text ?\n ?\n)
		      (if (not diary-schedule-place-out-of-bounds-entries-last)
			  (setq diary-schedule-line-offset (1- (current-line))))
		      (setq diary-schedule-out-of-bounds-entry-text "")))
                (setq date (car (car entry-list)))
                (and holidays-in-diary-buffer
                     (calendar-date-compare
                      (list (list holiday-list-last-month
                                  (calendar-last-day-of-month
                                   holiday-list-last-month
                                   holiday-list-last-year)
                                  holiday-list-last-year))
                      (list date))
                     ;; We need to get the holidays for the next 3 months.
                     (setq holiday-list-last-month
                           (extract-calendar-month date))
                     (setq holiday-list-last-year
                           (extract-calendar-year date))
                     (increment-calendar-month
                      holiday-list-last-month holiday-list-last-year 1)
                     (setq holiday-list
                           (let ((displayed-month holiday-list-last-month)
                                 (displayed-year holiday-list-last-year))
                             (calendar-holiday-list)))
                     (increment-calendar-month
                      holiday-list-last-month holiday-list-last-year 1))
                (let* ((date-string (calendar-date-string date))
                       (date-holiday-list
                        (let ((h holiday-list)
                              (d))
                          ;; Make a list of all holidays for date.
                          (while h
                            (if (calendar-date-equal date (car (car h)))
                                (setq d (append d (cdr (car h)))))
                            (setq h (cdr h)))
                          d)))
		  (goto-char (point-max))
                  (insert (if (= (point) (point-min))
			      ""
			    diary-subsequent-date-prefix-string)
			  date-string)
                  (if date-holiday-list (insert ":  "))
                  (let ((l (current-column)))
                    (insert (mapconcat 'identity date-holiday-list
                                       (concat "\n" (make-string l ? )))))
                  (let ((l (current-column)))
                    (insert ?\n (make-string l ?=) ?\n)))

		;; Massage the time format for the first interval of the day.

		(if (and diary-schedule-first-time-always-has-hours
			 (> (current-line) 3))
		    (progn
		      (goto-line (1+ diary-schedule-line-offset))
		      (delete-char (length diary-schedule-time-display-format))
		      (insert (diary-format-time
			       (or diary-schedule-first-time-format
				   diary-schedule-time-display-format)
			       diary-schedule-start-time
			       t))
		      (goto-char (point-max))))
		(setq diary-schedule-line-offset (1- (current-line))
		      diary-schedule-start-time
		      (round-to-nearest-interval
		       diary-default-schedule-start-time
		       diary-schedule-interval-time
		       t)
		      diary-schedule-stop-time
		      (round-to-nearest-interval
		       diary-default-schedule-stop-time
		       diary-schedule-interval-time
		       nil))
		(diary-display-grid diary-schedule-start-time
				    diary-schedule-stop-time
				    diary-schedule-interval-time
				    nil)
		(insert ?\n)))
          (if (< 0 (length (car (cdr (car entry-list)))))
	      (diary-schedule-display (car (cdr (car entry-list)))))
          (setq entry-list (cdr entry-list))))
      (set-buffer-modified-p nil)

      (message "Adding %s" diary-schedule-out-of-bounds-entry-text)
      (if (> (length diary-schedule-out-of-bounds-entry-text) 0)
	  (progn
	    (if diary-schedule-place-out-of-bounds-entries-last
		(goto-char (point-max))
	      (goto-line (1+ diary-schedule-line-offset)))
	    (insert diary-schedule-out-of-bounds-entry-text ?\n ?\n)
	    (if (not diary-schedule-place-out-of-bounds-entries-last)
		(setq diary-schedule-line-offset (1- (current-line))))
	    (setq diary-schedule-out-of-bounds-entry-text "")))

      ;; Massage the time format for the first interval of the final day.

      (if (and diary-schedule-first-time-always-has-hours
	       (> (current-line) 3))
	  (progn
	    (goto-line (1+ diary-schedule-line-offset))
	    (delete-char (length diary-schedule-time-display-format))
	    (insert (diary-format-time
		     (or diary-schedule-first-time-format
			 diary-schedule-time-display-format)
		     diary-schedule-start-time
		     t))))

      ;; Split lines containing multiple entries.

      (goto-char (point-min))
      (perform-replace diary-schedule-entry-separator-string "\n" nil nil nil)
      (goto-char (point-min))

      ;; Eliminate duplicate times from grid if desired.

      (if (not diary-duplicate-time-display)
	  (progn
	    (setq time-on-previous-line
		  (buffer-substring
		   (point)
		   (+ (point) (length diary-schedule-time-display-format))))
	    (while (< (point) (point-max))
	      (forward-line 1)
	      (let ((start-of-current-line (point)))
		(if (< (+ (point) (length diary-schedule-time-display-format))
		       (point-max))
		    (progn
		      (setq time-on-current-line
			    (buffer-substring
			     (point)
			     (+ (point) (length diary-schedule-time-display-format))))
		      (if (string-equal-by-format
			   time-on-previous-line
			   time-on-current-line
			   (if (= 0 (% diary-schedule-interval-time 60))
			       diary-schedule-time-display-format
			     diary-schedule-time-overflow-display-format))
			  (progn
			    (delete-char (length diary-schedule-time-display-format))
			    (insert (make-string
				     (length diary-schedule-time-display-format)
				     ?\ ))
			    (goto-char start-of-current-line)))
		    (setq time-on-previous-line time-on-current-line))
		(goto-char (point-max)))))))

      ;; Break long lines.

      (goto-char (point-min))
      (save-excursion
	(setq old-fill-prefix fill-prefix
	      fill-prefix diary-schedule-fill-prefix-for-broken-lines)
	(let ((start-of-current-line (point)))
	  (while (< (point) (point-max))
	    (if (looking-at "[^0-9 ]")
		(setq fill-prefix ""))
	    (forward-line 1)
	    (fill-region start-of-current-line (point))
	    (setq start-of-current-line (point))
	    (setq fill-prefix diary-schedule-fill-prefix-for-broken-lines)))
	(setq fill-prefix old-fill-prefix))

      (goto-char (point-min))
      (setq buffer-read-only t)
      (display-buffer fancy-diary-buffer)
      (message "Preparing diary...done"))))

(defun string-equal-by-format (s1 s2 format)
  "Compare the characters in S1 and S2 which correspond to non-blank characters
in FORMAT."
  (let ((i 0)
	(flag nil))
    (while (and (< i (length format)) (not flag))
      (let ((f (string-to-char (substring format i (1+ i))))
	    (c1 (string-to-char (substring s1 i (1+ i))))
	    (c2 (string-to-char (substring s2 i (1+ i)))))
	(if (not (= f ?\ ))
	    (if (or (/= c1 c2)
		    (and (= f ?1)
			 (not (or (= c1 ?\ ) (= c1 ?0) (= c1 ?1))))
		    (and (or (= f ?2) (= f ?4) (= f ?h) (= f ?H) (= f ?m) (= f ?M))
			 (not (or (= c1 ?\ )
				  (and (>= c1 ?0) (<= c1 ?9)))))
		    (and (= f ?:) (/= c1 ?:)))
		(setq flag t)
	      (setq i (1+ i)))
	  (setq i (1+ i)))))
    (not flag)))

(defun diary-equinoxes-solstices ()
  "Equinox and solstice diary entry."
  (let* ((displayed-month (car date))
	 (displayed-year (car (cdr (cdr date))))
	 (equinox (solar-equinoxes-solstices)))
    (if (calendar-date-equal (car (car equinox)) date)
	(car (cdr (car equinox))))))

;(defun diary-phases-of-moon ()
;  "Lunar phases diary entry."
;  (let* ((displayed-month (car date))
;	 (displayed-year (car (cdr (cdr date))))
;	 (phases (lunar-phase-list displayed-month displayed-year)))
;    (if (calendar-date-equal (car (car phases)) date)
;	(car (cdr (car phases))))))

(defun diary-today ()
  "Current day diary entry."
  (equal date (calendar-current-date)))

(defun diary-relative (n)
  "Diary entry that will always appear N days from today"
  (=
   (calendar-absolute-from-gregorian date)
   (+ n (calendar-absolute-from-gregorian (calendar-current-date)))))

(defun diary-tomorrow ()
  "Diary entry for tomorrow."
  (diary-relative 1))

(defun diary-yesterday ()
  "Diary entry for yesterday."
  (diary-relative -1))

(defun diary-days-of-week (days)
  "Diary entry for specified days of the week.
See calendar-day-of-week for numbers."
  (if (memq (calendar-day-of-week date) days) t nil))

(defun diary-weekday ()
  "Diary entry to appear every weekday."
  (diary-days-of-week '(1 2 3 4 5)))

(defun diary-weekend ()
  "Diary entry to appear every weekday."
  (diary-days-of-week '(0 6)))

(defun diary-mwf ()
  "Diary entry to appear every Mon, Wed and Fri."
  (diary-days-of-week '(1 3 5)))

(defun diary-tt ()
  "Diary entry to appear every Tue & Thu."
  (diary-days-of-week '(2 4)))

(defun diary-twf ()
  "Diary entry to appear every Tue, Wed and Fri."
  (diary-days-of-week '(2 3 5)))

(defun diary-mt ()
  "Diary entry to appear every Mon & Thu."
  (diary-days-of-week '(1 4)))
  
(defun diary-desk-sunrise-sunset-entry-text (rise-set time timezone day-duration)
  "Produce a string suitable for the desktop calendar for the sunrise or
sunset.  RISE-SET is a string containing \"Sunrise\" or \"Sunset\".  TIME is
the time as an integer in the internal format used by the desk calendar
(hour*100 + minutes).  DAY-DURATION is the length of the day as a string
(e.g., \"15:16\")"
  (interactive)
  (concat
   (if (string= rise-set "Sunrise")
       diary-schedule-sunrise-string
     diary-schedule-sunset-string)
   " "
   (diary-format-time diary-schedule-astronomical-event-time-format time t)
   " "
   timezone
   " "
   calendar-location-name))

(defun sunrises-and-sunsets (start-date end-date)
  (interactive)
  (let ((date start-date)
	(times "")
	(stop-date (calendar-absolute-from-gregorian end-date)))
    (while (<= (calendar-absolute-from-gregorian date) stop-date)
      (progn
	(setq times (concat times
			    (calendar-date-string date t) ", "
			    (solar-sunrise-sunset date) "\n"))
	(setq date (calendar-gregorian-from-absolute
		    (1+ (calendar-absolute-from-gregorian date))))))
    times))

;; (defun diary-task (status id start-month start-date start-year duration completed dependencies)
;;  "Diary entry for a task.  These do not necessarily appear as schedule items."
;;  )

(defun round-to-nearest-interval (time interval downp)
  "Round TIME up or down to the nearest INTERVAL number of minutes since midnight."
  (let ((minutes-since-midnight (+ (* (/ time 100) 60) (% time 100))))
    (if (= 0 (% minutes-since-midnight interval))
	time
      (progn
	(setq minutes-since-midnight
	      (+ (* interval (/ minutes-since-midnight interval))
		 (if downp 0 interval)))
	(+ (% minutes-since-midnight 60)
	   (* 100 (/ minutes-since-midnight 60)))))))

(defun current-line ()
  "Get the current line number (in the buffer) of point."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(provide 'cal-desk-calendar)

;; cal-desktop-calendar.el ends here

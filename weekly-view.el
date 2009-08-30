;;; weekly-view.el --- bar graph view of the week's diary events

;; Copyright (C) 2003 Doug Alcorn <doug@lathi.net>

;; Author: Doug Alcorn <doug@lathi.net>
;; Keywords: calendar, diary, week, view

;; This program is NOT part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Version: 2
;; X-URL: http://www.lathi.net/twiki-bin/view/Main/EmacsWeeklyView

;; This package makes a bar graph of the week's diary events
;;
;; Usage:
;;   (require 'weekly-view)
;;   (add-hook 'diary-display-hook 'fancy-diary-display-week-graph)

;; Now when you view your diary it will display the weekly graph.

;;; History:
;; version 1: original code, mostly worked
;; version 2: patch from Nemeth Felician <f_nemeth@index.hu> fixes lots of bugs

;;; Code:

(require 'cal-desk-calendar)

(define-key calendar-mode-map "w" 'week-graph-view-diary-entries)


;;; User Variables:

(defcustom week-graph-work-week nil
  "*If non-nil, only the 5 workdays are displayed (instead of 7)."
  :type 'boolean)

(defcustom week-graph-dynamic-width t
  "*If non-nil, the graph fully fills the window.
If nil, `weekly-graph-day-width' is used."
  :type 'boolean)


;;; Compatibility:
;; 
;; emacs doesn't have replace-in-string
;; from gnus-util.el
(eval-and-compile
  (cond
   ((fboundp 'replace-in-string)
    (defalias 'weekly-view-replace-in-string 'replace-in-string))
   ((fboundp 'replace-regexp-in-string)
    (defun replace-in-string  (string regexp newtext &optional literal)
      (replace-regexp-in-string regexp newtext string nil literal)))
   (t
    (defun replace-in-string (string regexp newtext &optional literal)
      (let ((start 0) tail)
	(while (string-match regexp string start)
	  (setq tail (- (length string) (match-end 0)))
	  (setq string (replace-match newtext nil literal string))
	  (setq start (- (length string) tail))))
      string))))


;;;###autoload
(defun week-graph-view-diary-entries ()
  "Prepare and display a buffer with diary entries of the current week.
See `view-diary-entries' for more."
  (interactive)
  (save-excursion
    (calendar-cursor-to-nearest-date)
    (let ((diary-display-hook 'fancy-diary-display-week-graph)
	  (day (calendar-day-of-week (calendar-cursor-to-date))))
      (unless (= day calendar-week-start-day)
	(calendar-beginning-of-week 1))
      (view-diary-entries 7))))

(defun week-graph-convert-time (time)
  (let ((hour (/ time 100))
	(mins (% time 100)))
    (+ (* hour 3600) (* mins 60))))

(defun weekly-graph-display-time-format (time)
  (format "%2d:%02d" (/ cur 3600) (% (/ cur 60) 60)))

(defun week-graph-day-titles (&optional width)
  "Return of a vector whose members are the day names.  If WIDTH is
non-nil and non-zero, truncate the day name to that width."
  (mapcar
   (lambda (s)
     (let* ((extra (- width (length s)))
	    (pre (/ extra 2))
	    (post (- extra pre)))
       (cond
	((= extra 0) s)
	((> extra 0) (format (format "%%%ds%%%ds" (- width post) post) s ""))
	(t (substring s 0 width)))))
   calendar-day-name-array))

(defvar weekly-graph-day-width 10
  "Width of the each day in the bar graph")

(defvar weekly-graph-day-start  800
  "Time to start each day's view in military style integers (i.e. \"8:00am\" is 800)")
(defvar weekly-graph-day-end 1800
  "Time to end each day's view in military style integers (i.e. \"8:00am\" is 800)")
(defvar weekly-graph-increment 30
  "Increment in minutes for each row on the weekly graph of diary entries")
(defvar weekly-graph-use-hash-marks t
  "If non-nill use hash marks '\#' to graph events rather than pertinent event text")
(defvar weekly-graph-default-event-length 60
  "Length of events that don't specify an end time in minutes.")

(defvar diary-time-regexp-list
  `(

    "^[ 	]*\\([0-9]?[0-9]\\)-\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" ; HH-HH[ap]m
    "^[ 	]*\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)-\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)\\(\\|[^ap]\\)" ; military range
    "^[ 	]*\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)\\(\\>\\|[^ap]\\)"	; military time
    "^[ 	]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)-\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>" ; hh:mm range
    "^[ 	]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m-\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>"
    "^[ 	]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m-\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>" ; h/m range
    "^[ 	]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>" ; hh:mm
    "^[ 	]*\\([0-9]?[0-9]\\)\\([ap]\\)m-\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" ; HH[ap]m-HH[ap]m
    "^[ 	]*\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" ; hour only
    ,diary-schedule-sunrise-sunset-pattern
    ,diary-schedule-lunar-phase-pattern
    ,diary-schedule-equinox-solstice-pattern
    ,diary-schedule-morning-pattern
    ;,diary-schedule-afternoon-patern
    ,diary-schedule-workday-pattern
    ,diary-schedule-all-day-pattern))

(defun fancy-diary-display-week-graph ()
  "Given a set of appointment DATA, draw a weekly bar graph of it.
DATA must be a vector of seven elements (for each day of the week),
where each element is a list of cons cells representing the beginning
and ending seconds of each appointment within the day, where midnight
begins at 0 seconds.  For example, an appointment for one hour on
Monday at 10am would look like:

  [nil ((36000 . 39600)) nil nil nil nil nil]"
  (save-excursion ;; Prepare the fancy diary buffer.
    ;; setup
    (message "Preparing diary...")
    (set-buffer (get-buffer-create fancy-diary-buffer))
    (setq buffer-read-only nil)
    (make-local-variable 'mode-line-format)
    (calendar-set-mode-line " Desk Calendar ")
    (erase-buffer)

    ;; get ready to insert schedule
    (let* ((num-days (if week-graph-work-week 5 7))
 	   (day-width (if week-graph-dynamic-width
 			  (/ (- (window-width) 7 num-days) num-days)
 			weekly-graph-day-width))
	   (begin (week-graph-convert-time weekly-graph-day-start))
	   (end (week-graph-convert-time weekly-graph-day-end))
 	   (incr (* weekly-graph-increment 60))
	   (cur begin)
	   (day-titles (vconcat (week-graph-day-titles day-width)))
	   (data (make-vector 7 nil)))
      ;; "cook" the diary-entries-list into a more regular form
      (dolist (elem diary-entries-list)
	(let* ((dow (calendar-day-of-week (car elem)))
	       (day-of-week-ref (aref data dow))
	       (entry-times (diary-entry-times (cadr elem)))
	       (entry-text (cadr elem))
	       (beginning-time (week-graph-convert-time (car entry-times)))
	       (ending-time (week-graph-convert-time (cadr entry-times)))
	       entry-alist)
	  (if (= beginning-time ending-time)
	      (setq ending-time (+ ending-time (* weekly-graph-default-event-length 60))))
	  (mapcar (lambda (regexp) (setq entry-text (replace-in-string entry-text regexp "")))
		  diary-time-regexp-list)
	  (setq entry-text (replace-in-string entry-text "^[ 	]+" ""))
	  (setq entry-text (replace-in-string entry-text "[ \t\n]+" " "))
	  (setq entry-text (replace-in-string entry-text "[ \t]+$" ""))
	  (setq entry-plist `(:beginning ,beginning-time
			      :ending ,ending-time
			      :text ,entry-text
			      :marker ,(nth 3 elem)))
	  (unless (= (car entry-times) -9999)
	    (if day-of-week-ref
		(nconc day-of-week-ref (list entry-plist))
	      (aset data dow (list entry-plist))))))

      ;; insert day of week headers
      (dotimes (j 7)
	(if (= j 0)
	    (insert "     "))
	(setq i (% (+ j calendar-week-start-day) 7))
	(unless (and week-graph-work-week (or (= i 0) (= i 6)))
	  (insert "|" (aref day-titles i))))
      (insert "|\n"
	      (make-string (+ 6 (* day-width num-days) num-days) ?-) ?\n)

      ;; now handle entries
      (while (< cur end)
	(dotimes (j 7)
	  (setq i (% (+ j calendar-week-start-day) 7))
	  ;; insert time on lieft
	  (if (= j 0) (insert (weekly-graph-display-time-format cur)))
	  (unless (and week-graph-work-week (or (= i 0) (= i 6)))
	    (insert "|")
	    (let ((day-of-week-list (aref data i)) (print-text nil) (print-marker nil))
	      (setq day-of-week-list
		    (mapcar
		     (lambda (appt)
		       (let* ((appt-text (plist-get appt :text))
			      (beginning (plist-get appt :beginning))
			      (ending (plist-get appt :ending))
			      (times-printed (plist-get appt :times-printed))
			      (start (if times-printed (* times-printed day-width) 0))
			      (end (if times-printed (* (1+ times-printed) day-width) day-width)))
			 (message "times printed: %s start: %s end: %s appt: %s" times-printed start end appt-text)
			 (if (and (>= cur beginning) (< cur ending))
			     (progn
			       (unless times-printed
				 (setq print-marker (plist-get appt :marker)))
			       (setq print-text
				     (if (> (length appt-text) end)
					 (substring appt-text start end)
				       (if (> start (length appt-text))
					   (make-string day-width ?#)
					 (concat
					  (substring appt-text start (length appt-text))
					  (make-string (- day-width (- (length appt-text) start)) ?#)))))
			       
			       (setq appt (plist-put appt :times-printed (if times-printed (1+ times-printed) 1))))
					;(setq print-text nil)
			   )
			 appt))
		     day-of-week-list))
	      (aset data i day-of-week-list)
	      (if print-text
		  (if print-marker
		      (insert-button print-text 'marker print-marker :type 'diary-entry)
		    (insert print-text))
		(insert (make-string day-width ? ))))))
	(insert "|\n")
	(setq cur (+ cur incr)))
      (insert (make-string (+ 6 (* day-width num-days) num-days) ?-) ?\n))
    
    ;; clean-up
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (display-buffer fancy-diary-buffer)
    (message "Preparing diary...done")))

(provide 'weekly-view)

;;; weekly-view.el ends here

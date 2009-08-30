;;; cal-html.el --- calendar functions for printing calendars with HTML.

;;;------------------------------------------------------------
;; Filename: cal-html.el
;; Author: Anna M. Bigatti
;; Description: Calendar, diary and appointments stuff.
;; Maintainer: Anna M. Bigatti
;; Copyright (C) 2002-2006, Anna Bigatti, all rights reserved.
;; Created: 23 Aug 2002
;; Last-Updated: 12 Apr 2006
;; Keywords: calendar, html
;; Compatibility: GNU Emacs 20.x
;;;------------------------------------------------------------
;;; Commentary:
;;;------------------------------------------------------------
;; This collection of functions implements the creation of HTML
;; calendars based on the user's diary file.
;;--------------------
;; customizable variables in user's .emacs:
;;
;; (setq diary-html-dir "~/Agenda")     ; default is ~/public_html
;; (setq diary-html-year-index-cols 2)  ; default is 3
;; 
;;--------------------
;;---- Add these to your .emacs
;; (autoload 'cal-html-cursor-month "cal-html.el" "cal-html loaded" 't)
;; (autoload 'cal-html-cursor-year  "cal-html.el" "cal-html loaded" 't)
;; (require 'calendar)
;; (define-key calendar-mode-map "Hm" 'cal-html-cursor-month) 
;; (define-key calendar-mode-map "Hy" 'cal-html-cursor-year) 
;;---- to have:
;; `H m'  (cal-html-cursor-month)
;; generate a calendar for the current month with the user's diary
;; entries: YEARMONTH.html in "HTML diary directory" 
;;
;; `H y'  (cal-html-cursor-year):
;; generate a calendar for the current year: index.html and month
;; pages in "HTML diary directory"
;;--------------------
;; In order to make it easy to update the pages, the files are
;; created/overwritten without asking for confirmation. The user will only
;; be asked for confirmation of the name of the "HTML diary directory":
;; the default is YEAR in diary-html-dir
;;   e.g.   ~/public_html/2002
;; some links between pages might not work if this default directory
;; is not chosen.
;; The "HTML diary directory" will be created is it doesn't exist.
;;--------------------
;; The characters `<' and `>' in the diary are interpreted as HTML
;; code to allow coloured/formatted entries.
;;--------------------
;; The default style will be overwritten by user defined file
;; `cal.css' in the "HTML diary directory": e.g.
;;;;
;;;;    BODY { background: #06a; }
;;;;    H1   { color: #fff; }
;;;;
;;;;    TABLE.year { padding: 0pt 0pt 20pt 0pt; }   
;;;;    TABLE.year TD { vertical-align:top; }   
;;;;    
;;;;    TABLE.header { }
;;;;
;;;;    TABLE.minical { }   
;;;;    TABLE.minical TH { background: #cff; }   
;;;;    TABLE.minical TD { background: #006; }   
;;;;    
;;;;    TABLE.agenda { }
;;;;    TABLE.agenda TH { background: #cff; }
;;;;    TABLE.agenda TD { background: #fff; }
;;;;
;;;;    A { text-decoration: underline; }   
;;;;    A:link      { color: #ffa; } 
;;;;    A:visited   { color: #aff; } 
;;;;    A:hover     { color: #000; background: #aff; }
;;;;
;;;;    SPAN.NO-YEAR  { color: green; font-weight: bold}
;;;;    SPAN.ANN     { color: red; font-weight: bold}
;;;;    SPAN.BLOCK   { color: #048; font-weight: bold; font-style:italic;}
;;;------------------------------------------------------------
;;; Code:
;;;------------------------------------------------------------

(require 'calendar)
(require 'cal-tex)

; if  diary-html-dir  not defined set default
(if (boundp 'diary-html-dir) 
    ()   (setq diary-html-dir "~/public_html"))
; if  diary-html-year-index-cols  not defined set default
(if (boundp 'diary-html-year-index-cols) 
    ()   (setq diary-html-year-index-cols 3))


(autoload 'list-diary-entries "diary-lib" nil t)
(autoload 'calendar-iso-from-absolute "cal-iso" nil t)


(defvar calendar-day-abbrname-array
  ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"])


(defun cal-html-year-dir-ask-user (year)
  (let* (
	 (cal-html-dir-default 
	  (concat diary-html-dir "/" (number-to-string year) ))
	 (cal-html-dir 
	  (if (read-file-name 
	       "Enter name of the HTML diary directory: (default)" 
	       cal-html-dir-default)
	      cal-html-dir-default))
	 )
    (if (file-directory-p cal-html-dir) () (make-directory cal-html-dir))
    cal-html-dir ))


;;;------------------------------------------------------------
;;;  Yearly calendars
;;;------------------------------------------------------------

(defun cal-html-cursor-year (&optional arg)
  "Creates the file `index.html' in the HTML diary directory.
   Creates the month pages with the user's diary entries."
  (interactive "P")
  (let* (
	 (date  (calendar-cursor-to-date t))
         (year  (extract-calendar-year  date))
	 (cal-html-year-dir (cal-html-year-dir-ask-user year))
	 (cal-html-file (concat cal-html-year-dir "/index.html"))
	 (cal-html-file-buffer-name (find-file-noselect cal-html-file))
	 )
    (set-buffer (get-buffer-create cal-html-file-buffer-name))
    (cal-html-year-minicals year diary-html-year-index-cols)
    (save-buffer)
    (kill-buffer cal-html-file-buffer-name)  
    (run-hooks 'cal-html-hook)
    (calendar-for-loop i from 1 to 12 do
		       (cal-html-one-month i year cal-html-year-dir)
		       )))


(defun cal-html-year-minicals (year cols)
  "Make a one page yearly mini-calendar of YEAR; 
There are  12/cols  rows of  cols  months each."
  (erase-buffer)
  (insert cal-html-preamble-string
	  cal-html-b-document-string)
  (cal-html-insert-header 0 year)
  (insert (cal-html-b-table "class=year")
	  cal-html-b-tablerow-string)
  (calendar-for-loop i from 1 to 12 do
     (progn 
       (insert cal-html-b-tabledata-string)
       (cal-html-insert-link-monthpage i year)
       (cal-html-insert-minical i year)
       (insert cal-html-e-tabledata-string)
       (if (zerop (mod i cols))
	   (insert cal-html-e-tablerow-string
		   cal-html-b-tablerow-string))))
  (insert  cal-html-e-tablerow-string
	   cal-html-e-table-string
	   cal-html-e-document-string))


;;;------------------------------------------------------------
;;;  Monthly calendar
;;;------------------------------------------------------------

(defun cal-html-monthpage-name (month year)
 (concat (number-to-string (+ (* year 100) month)) ".html"))


(defun cal-html-cursor-month (&optional arg)
  "Make an html-file for the month cursor is on."
  (interactive "P")
;; set date, month and year, and ask for calendar directory name
  (let* (
	 (date  (calendar-cursor-to-date t))
         (month (extract-calendar-month date))
         (year  (extract-calendar-year  date))
	 (cal-html-year-dir (cal-html-year-dir-ask-user year))
	 )
    (cal-html-one-month month year cal-html-year-dir)))


(defun cal-html-one-month (month year cal-html-year-dir)
  "Make an html-file for MONTH."
  (let* (
	 (cal-html-file (concat cal-html-year-dir  "/"
				(cal-html-monthpage-name month year)))
	 (cal-html-file-buffer-name (find-file-noselect cal-html-file))
	 (diary-list (cal-tex-list-diary-entries
		      (calendar-absolute-from-gregorian (list month 1 year))
		      (calendar-absolute-from-gregorian 
		       (list month 
			     (calendar-last-day-of-month month year)
			     year))))
	 )
;;;;    (message diary-list)
;; start writing
    (set-buffer (get-buffer-create cal-html-file-buffer-name))
    (erase-buffer)
    (insert cal-html-preamble-string
	    cal-html-b-document-string)
    (cal-html-insert-header month year)
    (cal-html-insert-agenda-days month year diary-list)
    (insert cal-html-e-document-string)
;; end writing
    (save-buffer)
    (kill-buffer cal-html-file-buffer-name)
    (run-hooks 'cal-html-hook)))


(defun cal-html-insert-agenda-days (month year diary-list) 
  "Insert HTML commands for a range of days in monthly calendars.
HTML commands are inserted for the days of the MONTH in YEAR.
Diary entries on DIARY-LIST are included."
  (let* (
	 (blank-days ;; at start of month
          (mod  (- (calendar-day-of-week (list month 1 year))
		   calendar-week-start-day)
		7))
         (date)
         (last (calendar-last-day-of-month month year))
	 )
    (insert "<a name=0>\n")
    (insert (cal-html-b-table "class=agenda border=1"))
    (calendar-for-loop i from 1 to last do
		       (setq date (list month i year))
		       (insert 
			cal-html-b-tablerow-string
; --- number & day name ---
			cal-html-b-tableheader-string
			(format   "<a name=%s></a>%s " i i)
			(aref calendar-day-name-array 
			      (calendar-day-of-week date))
			cal-html-e-tableheader-string
; --- diary entries ---
			cal-html-b-tabledata-string
			(cal-html-htmlify-list diary-list date)
			cal-html-e-tabledata-string
			cal-html-e-tablerow-string)
;      if end of the week and not end of the month, then make a new table
       (if (and (zerop (mod (+ i blank-days) 7)) (/= i last))
           (insert cal-html-e-table-string
		   (cal-html-b-table "class=agenda border=1")) )))
  (insert cal-html-e-table-string) )


;;;------------------------------------------------------------
;;; page header
;;;------------------------------------------------------------

(defun cal-html-insert-header (month year)
  "Insert the title for a calendar"
  (if (= month 0)
; ----------    header for the year page    ----------
      (insert (cal-html-h1 (number-to-string year)))
; ----------    header for the month page    ----------
; links to previous and next month and year; current minical
    (progn
      (insert (cal-html-b-table "class=header"))
      (insert cal-html-b-tablerow-string) 
; ------- month links -------
      (insert cal-html-b-tabledata-string)
; previous month
      (increment-calendar-month month year -1)
      (cal-html-insert-link-monthpage month year t) ; t --> change-dir
; current month
      (increment-calendar-month month year 1)           
      (cal-html-insert-link-year-page month year)
; next month
      (increment-calendar-month month year 1)           
      (cal-html-insert-link-monthpage month year t) ; t --> change-dir
      (insert cal-html-e-tabledata-string)
; ------- minical -------
      (insert cal-html-b-tabledata-string)
      (increment-calendar-month month year -1)          
      (cal-html-insert-minical month year) 
      (insert cal-html-e-tabledata-string)
; ------- end -------
      (insert cal-html-e-tablerow-string)
      (insert cal-html-e-table-string)  )))


(defun cal-html-insert-link-monthpage (month year &optional change-dir) 
  (insert (cal-html-h3
	   (cal-html-href  
	    (cond ((and change-dir (or (= month 1) (= month 12)) )
		   (concat "../" (number-to-string year) 
			   "/"   (cal-html-monthpage-name month year)))
		  (t (cal-html-monthpage-name month year)) )
	    (calendar-month-name month)))))


(defun cal-html-insert-link-year-page (month year) 
  (insert (cal-html-h1
	   (concat 
	    (calendar-month-name month)
	    " "
	    (cal-html-href "index.html" (number-to-string year))))))


;;;------------------------------------------------------------
;;; minical
;;;------------------------------------------------------------

(defun cal-html-insert-minical (month year) 
  (let* (
	 (blank-days ;; at start of month
          (mod (- (calendar-day-of-week (list month 1 year))
		  calendar-week-start-day)
	       7))
         (date)
         (last (calendar-last-day-of-month month year))
	 (end-blank-days ;; at end of month
          (mod (- 6 (+ (calendar-day-of-week (list month last year))
		       calendar-week-start-day))
	       7))
	 (monthpage-name (cal-html-monthpage-name month year))
	 )
;   start writing table
    (insert (cal-html-comment "MINICAL") 
	    (cal-html-b-table "class=minical border=1 align=center"))
;   -- weekdays row --
    (insert cal-html-b-tablerow-string)
    (calendar-for-loop i from 0 to 6 do 
		       (insert (cal-html-th
				(aref calendar-day-abbrname-array i))))
    (insert cal-html-e-tablerow-string)
;   -- initial empty slots --
    (insert cal-html-b-tablerow-string)
    (calendar-for-loop i from 0 to (1- blank-days) do 
		       (insert 
			cal-html-b-tabledata-string
			cal-html-e-tabledata-string) )
;   -- numbers --
    (calendar-for-loop i from 1 to last do
       (insert (format cal-html-minical-day-format monthpage-name (- i 1) i))
;      - ? new row -
       (if (and (zerop (mod (+ i blank-days) 7))   (/= i last))
	     (insert cal-html-e-tablerow-string
		     cal-html-b-tablerow-string) ))
;   -- end empty slots --  (for some browsers like konqueror)
    (calendar-for-loop i from 0 to (1- end-blank-days) do 
		       (insert 
			cal-html-b-tabledata-string
			cal-html-e-tabledata-string) ) )
  (insert cal-html-e-tablerow-string
	  cal-html-e-table-string
	  (cal-html-comment "MINICAL end")) )

;;;------------------------------------------------------------
;;; 
;;;------------------------------------------------------------

(defun cal-html-htmlify-list (date-list date)
  "Return string with concatenated, HTMLified entries in DATE_LIST for DATE."
  (mapconcat '(lambda (x) (cal-html-htmlify-entry  x))
             (let ((result)
                   (p date-list))
               (while p
                 (and (caar p)
                      (calendar-date-equal date (caar p))
                      (setq result (cons (car p) result)))
                 (setq p (cdr p)))
               (reverse result))
             "<BR>\n     "))


(defun cal-html-htmlify-entry (entry)
  (cond
   ((string-match "block" (car(cddr entry)))
    (concat "<span class=BLOCK>" (cal-html-htmlify-string (cadr entry)) "</span>"))
   ((string-match "anniversary" (car(cddr entry))) 
    (concat "<span class=ANN>" (cal-html-htmlify-string (cadr entry)) "</span>"))
    ((not(string-match (number-to-string (car(cddr(car entry)))) (car(cddr entry)))) 
     (concat "<span class=NO-YEAR>" (cal-html-htmlify-string (cadr entry)) "</span>"))
   ('t (cal-html-htmlify-string (cadr entry)))
   )
  )

(defun cal-html-htmlify-string (string)
  "Protect special characters in STRING from HTML."
  (if (not string)
      ""
    (let ((head "")
          (tail string)
          (list cal-html-html-subst-list))
      (while (not (string-equal tail ""))
        (let* ((ch (substring tail 0 1))
               (pair (assoc ch list)))
;          (if (and pair (string-equal ch "\"")) ;; "TeX-style" quotes:
;              (setq list (reverse list)));; Quote changes meaning each time.
          (setq tail (substring tail 1))
          (setq head (concat head (if pair (cdr pair) ch)))))
      head)))


;;;------------------------------------------------------------
;;;  HTML
;;;------------------------------------------------------------

(defvar cal-html-html-subst-list
  '(
    ("&"  . "&amp;") 
    ("\n" . "<BR>\n"))
  "List of symbols and their replacements.")


(defvar cal-html-preamble-string
  (concat
   "<HTML>\n"
   "<HEAD>\n"
   "<TITLE>Calendar</TITLE>\n"
   "<!--This buffer was produced by cal-html.el-->\n\n"
; default style
   "<STYLE TYPE=\"text/css\">\n" 
   "  BODY { background: #bde; }\n"
   "  H1   { text-align:center; }\n"
   "  TABLE  { padding:2pt; }\n"
   "  TH { background:#dee; }\n"
   "  TABLE.year   { width:100%; }\n"
   "  TABLE.agenda { width:100%; }\n"
   "  TABLE.header { width:100%; text-align:center; }\n"
   "  TABLE.minical TD { background: white; text-align:center; }\n"
   "  TABLE.agenda TD  { background: white; text-align:left; }\n"
   "  TABLE.agenda TH   { text-align: left; width:20%; }\n"
   "  SPAN.NO-YEAR  { color: #0b3; font-weight: bold}\n"
   "  SPAN.ANN      { color: #0bb; font-weight: bold}\n"
   "  SPAN.BLOCK    { color: 048; font-style:italic;}\n"
   "</STYLE>\n\n" 
; end default style: can be overwritten by a file called  cal.css
   "<LINK REL=\"stylesheet\" TYPE=\"text/css\" HREF=\"cal.css\">\n"
   "</HEAD>\n\n")
)

(defvar cal-html-b-document-string "<BODY>\n")
(defvar cal-html-e-document-string "<BR><BR>\n</BODY>\n</HTML>")

(defvar cal-html-b-tablerow-string    "<TR>\n")
(defvar cal-html-e-tablerow-string    "</TR>\n")
(defvar cal-html-b-tabledata-string   "  <TD>")
(defvar cal-html-e-tabledata-string   "  </TD>\n")
(defvar cal-html-b-tableheader-string "  <TH>")
(defvar cal-html-e-tableheader-string "  </TH>\n")

(defvar cal-html-e-table-string  
  "</TABLE>\n<!-- ================================================== -->\n")

(defvar  cal-html-minical-day-format "  <TD><a href=%s#%d>%d</TD>\n"
  "The initial HTML code for a day.  
The diary entries, bottom string, and the text follow.")
 

(defun cal-html-comment (&optional comment)
  (concat "<!--  ======  " comment "  ======  -->\n"))

(defun cal-html-href (link string)
  "returns STRING as link to LINK"
  (concat "<A HREF=\"" link "\">" string "</A>"))

(defun cal-html-h3 (string)
  "returns STRING as header3"
  (concat "\n        <H3>" string "</H3>\n"))

(defun cal-html-h1 (string)
  "returns STRING as header1"
  (concat "\n        <H1>" string "</H1>\n"))

(defun cal-html-th (string)
  "returns STRING as table header"
  (concat cal-html-b-tableheader-string string cal-html-e-tableheader-string))

(defun cal-html-b-table (&optional arg)
  (concat "\n<TABLE " arg ">\n"))

;;;------------------------------------------------------------

(provide 'cal-html)

;;; cal-html.el ends here


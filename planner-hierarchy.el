;;; planner-hierarchy.el --- Hierarchical Planning

;; Copyright (C) 2006 Greg Novak <novak@ucolick.org>

;; Code released under the Gnu General Public Liscense

;; Hierarchical Planning

;; One of the things I like about using PlannerMode is that it gets me into
;; the habit of, at the beginning of the day, deciding what I'm going to
;; do and, at the end of the day, evaluating whether or not I achieved my
;; goals.  I'd like to do this same thing at the week level, the month
;; level, the quarter level, and the year level.  This way each time
;; period breaks down into 3-4 smaller time periods, and I can keep an
;; eye on larger, longer-term goals.  (I've posted one or two messages
;; about this before).

;; To this end, I've put together a little code that lets you skip around
;; on pages that correspond to the different time intervals.  When I'm
;; looking at how I did over the past month, I want an easy way to look
;; at how I did for the weeks of that month.  Typing out all the page
;; names is tedious and time consuming, so I've created four functions
;; zoom-iup (for interactive-up), zoom-idown, zoom-inext, and zoom-iprev
;; (which I bind to Shift-up, Shift-down, etc).  

;; The naming convention for pages is:
;; year - "2006.Year"
;; quarter - "2006.Quarter2"
;; month - "2006.January"
;; week - "2006.January.Week3"
;; day - "2006.01.02"  
;; (this can be changed by changing zoom-regexps)

;; So typically I would look at the page named "2006.January" and then
;; hit 'C-u S-down' which shows me 2006.January.Week1 in the other
;; buffer.  Then I can hit S-left and S-right to look at
;; 2006.January.Week2, 2006.January.Week3, etc.  

;; I determine the month to which each week belongs by the month which
;; contains the zoom-first-day-of-week'th day of that week.  Zero is
;; Sunday, one is Monday, etc.  Therefore the March 1, 2006, would
;; typically be fall into "2006.February.Week4"

;; Config
(defvar zoom-first-day-of-week 1 "What day should be considered the first of the week.  Zero for Sunday, one for Monday, etc")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Guts
(defvar zoom-months '(("January" . 1)
		      ("February" . 2)
		      ("March" . 3)
		      ("April" . 4)
		      ("May" . 5)
		      ("June" . 6)
		      ("July" . 7)
		      ("August" . 8)
		      ("September" . 9)
		      ("October" . 10)
		      ("November" . 11)
		      ("December" . 12)
		      ("Month" . 13)) ; Extra invalid value
  "Alist associating month names with numbers.")

(defvar zoom-month-regexp (concat "\\(" 
				  (reduce (lambda (x y) (concat x "\\|" y)) 
					  (mapcar 'car zoom-months)) 
				  "\\)")
  "Regexp matching any month name given in zoom-months")

(defvar zoom-regexps (list '("^\\([0-9]\\{4\\}\\).Year$" 
			     . year) ; (year)
			   '("^\\([0-9]\\{4\\}\\).Quarter\\([0-5]\\)$" 
			     . quarter) ; (year, quarter)
			   (cons (concat "^\\([0-9]\\{4\\}\\)."
					 zoom-month-regexp
					 "$") 
				 'month) ; (year, month)
			   (cons (concat "^\\([0-9]\\{4\\}\\)."
				       zoom-month-regexp
				       ".Week\\([0-6]\\)$")
				 'week); year, month, week
			   '("^\\([0-9]\\{4\\}\\).\\([0-9]\\{1,2\\}\\).\\([0-9]\\{1,2\\}\\)$" 
			     . day)) ; year, month, day
  "Alist of regexps that match names of years, quarters, months,
  weeks, and days")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heavy lifting functions
(defun zoom-parse-to-strings (name &optional type)
  "Parse a string NAME, into a period of time given by zoom-regexps.  

  If TYPE is given, it is a symbol specifying the
  type of time-period which NAME should be parsed as (one of
  'day, 'week, 'month, 'quarter, or 'year.

  Return a four element list consisting of the type of time
  period and then a list of strings containing the subexpressions
  matched by the successful regexp.  Eg, 
  (zoom-parse-to-strings \"2006.Year\") 
  returns (year \"2006\" nil nil) 
  and (zoom-parse-to-strings \"2006.January.Week1\") 
  returns (week \"2006\" \"January\" \"1\")"
  (setq type (or type (assoc-default name zoom-regexps 'string-match)))  
  ;; Make sure the match data is for the right search
  (unless (string-match (car (rassoc type zoom-regexps)) name)
    (error "Zoom: Couldn't Parse Name"))
  (cons type (list (match-string 1 name) 
		   (match-string 2 name) 
		   (match-string 3 name))))

(defun zoom-parse (&rest args)
  "Parse a string NAME, into a period of time given by zoom-regexps.  

  If TYPE is given, it is a symbol specifying the
  type of time-period which NAME should be parsed as (one of
  'day, 'week, 'month, 'quarter, or 'year.

  Return a four element list consisting of the type of time
  period and then numerical representations of the subexpressions
  matched by the successful regexp.  Eg, 
  (zoom-parse \"2006.Year\") returns (year 2006 nil nil)
  and (zoom-parse \"2006.January.Week1\") returns (week 2006 1 1)" 

  (let* ((result (apply 'zoom-parse-to-strings args))
	 (type (car result))
	 (strings (cdr result))
	 numbers)
    (dotimes (i (length strings))
      (setq numbers (cons (when (not (null (nth i strings)))
			    (if (or (and (eq type 'month) (= i 1))
				    (and (eq type 'week) (= i 1)))
				(cdr (assoc (nth i strings) zoom-months))
			      (string-to-number (nth i strings))))
			  numbers)))
    (cons type (reverse numbers))))

(defun zoom-string (type &rest changes)
  "Convert time-range info into a string name.  You can specify
   numerical values or strings.  So
  (zoom-string 'year 2006) -> \"2006.Year\"
  (zoom-string 'year \"2006\") -> \"2006.Year\"
  (zoom-string 'week 2006 \"February\" 3) -> \"2006.February.Week3\"
  (zoom-string 'week 2006 2 3) -> \"2006.February.Week3\""
  ;; use a template
  (let ((name (cdr (assoc type '((year . "1000.Year")
				 (quarter . "1000.Quarter5")
				 (month . "1000.Month")
				 (week . "1000.Month.Week6")
				 (day . "1000.99.99"))))))
	
    ;; Make sure changes are strings
    (let (result)
      (dotimes (i (length changes))
	(setq result (cons (if (not (numberp (nth i changes)))
			       (nth i changes)
			       (if (or (and (eq type 'month) (= i 1))
				       (and (eq type 'week) (= i 1)))
				   (car (rassoc (nth 1 changes) zoom-months))
				   (number-to-string (nth i changes))))
			   result)))
      (setq changes (reverse result)))
    
    ;; Special handling for days + months in 'day strings: make sure
    ;; they're two digits
    (when (eq type 'day)
      (setq changes (mapcar (lambda (x) (if (= (length x) 1) 
					    (concat "0" x)
					    x))
			    changes)))
    
    (dotimes (i (length changes))
      (zoom-parse name type)    ; make sure match data is correct each time
      (setq name (replace-match (nth i changes) t t name (1+ i))))
    name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive

(add-hook 'planner-mode-hook (lambda ()
			       (local-set-key (kbd "<S-up>") 'zoom-iup)
			       (local-set-key (kbd "<S-down>") 'zoom-idown)
			       (local-set-key (kbd "<S-left>") 'zoom-iprev)
			       (local-set-key (kbd "<S-right>") 'zoom-inext)))

(defun zoom-iup (name other-window) 
  "Move to the next higher level in the hierarchy."
  (interactive (list (buffer-name)
		    current-prefix-arg))
  (when other-window (other-window 1))
  (emacs-wiki-find-file (zoom-up name))
  (when other-window (other-window 1)))

(defun zoom-idown (name other-window) 
  "Move to the next lower level in the hierarchy.  If the current
  date is within the higher-level time range, zoom to the lower
  level time range that also contains today.  Otherwise, just go
  to the first lower-level time range."
  (interactive (list (buffer-name)
		     current-prefix-arg))
  (when other-window (other-window 1))
  (emacs-wiki-find-file (zoom-down name))
  (when other-window (other-window 1)))

(defun zoom-inext (name num other-window)
  "Move to the next time range at the same level in the
  hierarchy.  With a numeric prefix arg, move by that number of
  time ranges.  With a non-numeric prefix arg, show the desired
  page in the other window."
  (interactive (list (buffer-name)
		     (if (numberp current-prefix-arg) 
			 current-prefix-arg
		       1)
		     (consp current-prefix-arg)))
  (when other-window (other-window 1))
  (emacs-wiki-find-file (zoom-next name num))
  (when other-window (other-window 1)))

(defun zoom-iprev (name num other-window)
  "Move to the previous time range at the same level in the
  hierarchy.  With a numeric prefix arg, move by that number of
  time ranges.  With a non-numeric prefix arg, show the desired
  page in the other window."
  (interactive (list (buffer-name)
		     (if (numberp current-prefix-arg) 
			 current-prefix-arg
		       1)
		     (consp current-prefix-arg)))
  (when other-window (other-window 1))
  (emacs-wiki-find-file (zoom-next name (- num)))
  (when other-window (other-window 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-period-specific stuff
(defun zoom-contains (name &optional today)
  "Test if TODAY is contained within the time period specified by
  string NAME.  If TODAY is not given, use the current date"
  (setq today (or today (gsn/calendar-today-absolute)))
  (and (<= (zoom-beg name) today)
       (>= (zoom-end name) today)))

(defun zoom-beg (name)
  "Return the absolute date of the beginning of the time period
  specified by string NAME."
  (funcall 
   ;; This is basically do-it-yourself object orientation.  Times are
   ;; lists where the first element is the type and the other elements
   ;; are type-specific information.  This function call dispatches on
   ;; the type, so it's basically a method call on a time range.
   (cdr (assoc (car (zoom-parse name)) '((year . zoom-year-beg)
					 (quarter . zoom-quarter-beg)
					 (month . zoom-month-beg)
					 (week . zoom-week-beg)
					 (day . zoom-day-beg))))
   name))

(defun zoom-end (name)
  "Return the absolute date of the end of the time period
  specified by string NAME."
  (funcall 
   ;; See zoom-beg comments
   (cdr (assoc (car (zoom-parse name)) '((year . zoom-year-end)
					 (quarter . zoom-quarter-end)
					 (month . zoom-month-end)
					 (week . zoom-week-end)
					 (day . zoom-day-end))))
   name))

(defun zoom-up (name)
  "For time range given by string NAME, return a string
  representiang the next higher enclosing time range in the
  heirarchy"
  (funcall 
   ;; See zoom-beg comments
   (cdr (assoc (car (zoom-parse name)) '((year . zoom-up-year)
					 (quarter . zoom-up-quarter)
					 (month . zoom-up-month)
					 (week . zoom-up-week)
					 (day . zoom-up-day))))
   name))

(defun zoom-down (name)
  "For time range given by string NAME, return a string
  representiang the next lower time range in the heirarchy.  If
  the current date is within the higher-level time range, choose
  the lower-level time range that also includes the current date.
  Otherwise, just retturn the first lower-level time range"
  (funcall 
   ;; See zoom-beg comments
   (cdr (assoc (car (zoom-parse name)) '((year . zoom-down-year)
					 (quarter . zoom-down-quarter)
					 (month . zoom-down-month)
					 (week . zoom-down-week)
					 (day . zoom-down-day))))
   name))

(defun zoom-next (name num)
  "For time range given by string NAME, return a string
  representiang the next time range at the same level in the heirarchy."
  (funcall 
   ;; See zoom-beg comments
   (cdr (assoc (car (zoom-parse name)) '((year . zoom-next-year)
					 (quarter . zoom-next-quarter)
					 (month . zoom-next-month)
					 (week . zoom-next-week)
					 (day . zoom-next-day))))
   name num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Year
(defun zoom-year-beg (name)
  "Return the absolute date of the beginning of the year
  specified by string NAME"
  (multiple-value-bind (type year) (zoom-parse name 'year)
    (calendar-absolute-from-gregorian (list 1 1 year))))

(defun zoom-year-end (name)
  "Return the absolute date of the end of the year
  specified by string NAME"
  (multiple-value-bind (type year) (zoom-parse name 'year)
    (calendar-absolute-from-gregorian (list 12 31 year))))

(defun zoom-up-year (name) 
  "Error: there's nothing above year in the heirarchy"
  nil)

(defun zoom-next-year (name num)
  "Return a string NUM years after the one given by string NAME."
  (multiple-value-bind (type year) (zoom-parse name 'year)
    (zoom-string 'year (+ num year))))

(defun zoom-down-year (name &optional today)
  "If the absolute date TODAY is within the year specified by
  NAME, return a string for the quarter that also contains TODAY.
  Otherwise, return the a string for the first quarter in the
  year.  If TODAY is not given, use the current date."
  (multiple-value-bind (junk year) (zoom-parse name 'year)
    (if (not (zoom-contains name today)) 
	(zoom-string 'quarter year 1)
      (car (remove-if-not (lambda (p) (zoom-contains p today))
			  (mapcar (lambda (n) (zoom-string 'quarter year n))
				  (range 1 4)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quarter
(defun zoom-up-quarter (name)
  "Return a string for the year containing the quarter specified
  by string NAME."
  (multiple-value-bind (type year quarter) (zoom-parse name 'quarter)
    (zoom-string 'year year)))

(defun zoom-quarter-beg (name)
  "Return the absolute date of the first day of the quarter given
  by string NAME"
  (multiple-value-bind (type year quarter) (zoom-parse name 'quarter)
    (calendar-absolute-from-gregorian (list (1+ (* 3 (1- quarter))) 1 year))))

(defun zoom-quarter-end (name)
  "Return the absolute date of the last day of the quarter given
  by string NAME"
  (multiple-value-bind (type year quarter) (zoom-parse name 'quarter)
    (cond ((= 1 quarter) (calendar-absolute-from-gregorian (list 3 31 year)))
	  ((= 2 quarter) (calendar-absolute-from-gregorian (list 6 30 year)))
	  ((= 3 quarter) (calendar-absolute-from-gregorian (list 9 30 year)))
	  ((= 4 quarter) (calendar-absolute-from-gregorian (list 12 31 year))))))

(defun zoom-next-quarter (name num)
  "Return a string for the name of the NUMth quarter after the
  one given by string NAME."
  (multiple-value-bind (type year quarter) (zoom-parse name 'quarter)  
    (let ((new-year (+ year (floor (/ (1- (float (+ quarter num))) 4))))
	  (new-quarter (1+ (mod (1- (+ quarter num)) 4))))
      (zoom-string 'quarter new-year new-quarter))))

(defun zoom-down-quarter (name &optional today)
  "If the absolute TODAY is within the quarter given by string
  NAME, return a string for the month that also contains TODAY.
  Otherwise, return a string for the first month in the
  quarter.  If TODAY is not given, use the current date."
  (multiple-value-bind (type year quarter) (zoom-parse name 'quarter) 
    (if (not (zoom-contains name today)) 
	(zoom-string 'month year (1+ (* (1- quarter) 3)))
	;; inefficient, but correct, to just include all months in the
	;; test since we know that the current quarter contains today,
	;; therefore some month in another quarter _cannot_ contain
	;; today
      (car (remove-if-not (lambda (p) (zoom-contains p today)) 
			  (mapcar (lambda (n) (zoom-string 'month year n))
				  (range 1 12)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Month

(defun zoom-month-beg (name)
  "Return the absolute date of the first day of the month given
  by the string NAME."
  (multiple-value-bind (type year month) (zoom-parse name 'month)
    (calendar-absolute-from-gregorian (list month 1 year))))

(defun zoom-month-end (name)
  "Return the absolute date of the last day of the month given
  by the string NAME."
  (multiple-value-bind (type year month) (zoom-parse name 'month)
    (calendar-absolute-from-gregorian (list month (calendar-last-day-of-month month year) year))))

(defun zoom-up-month (name)
  "Return a string for the quarter containing the month given by string NAME."
  (multiple-value-bind (type year month) (zoom-parse name)
    (let ((quarter (1+ (/ (1- month) 3))))
      (zoom-string 'quarter year quarter))))
	    	   
(defun zoom-next-month (name num)
  "Return a string for the NUMth month after the one given by the
  string NAME"
  (multiple-value-bind (type year month) (zoom-parse name 'month)
    (let ((new-year (+ year (floor (/ (1- (float (+ month num))) 12))))
	  (new-month (1+ (mod (1- (+ month num)) 12))))
      (zoom-string 'month new-year new-month))))

(defun zoom-down-month (name &optional today)
  "If the absolute date TODAY is within the month given by the
  string NAME, return a string for the week that also contains
  TODAY.  Otherwise, return a string for the first week in the
  month.  If TODAY is not given, use the current date."
  (multiple-value-bind (type year month) (zoom-parse name 'month)  
    (if (not (zoom-contains name today)) 
	(zoom-string 'week year month 1)
      (car (remove-if-not (lambda (p) (zoom-contains p today))
			  (mapcar (lambda (n) (zoom-string 'week year month n))
				  (range 1 5)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Week

(defun zoom-week-beg (name)
  "Return the absolute date of the first day of the week given by string NAME."
  (multiple-value-bind (type year month week) (zoom-parse name 'week)
    (calendar-absolute-from-gregorian 
     (calendar-nth-named-day week zoom-first-day-of-week month year))))

(defun zoom-week-end (name)
  "Return the absolute date of the last day of the week given by string NAME."
  (+ 6 (zoom-week-beg name)))

(defun zoom-up-week (name)
  "Return a string for the month containing the week given by string NAME."
  (multiple-value-bind (type year month week) (zoom-parse name 'week)
    (zoom-string 'month year month)))

(defun zoom-next-week (name num)
  "Return a string for the NUMth week after the one specified by
  the string NAME."
  (multiple-value-bind (type year month week) (zoom-parse name 'week)
    ;; New week <= 0 leads to problems with nth-named-day... try to fix them?
    (let* ((new-week (if (> (+ week num) 0) 
			 (+ week num)
		       (1- (+ week num))))
	   (new-date (calendar-nth-named-day new-week zoom-first-day-of-week month year 1))
	   (new-year (extract-calendar-year new-date))
	   (new-month (extract-calendar-month new-date))
	   (new-day (extract-calendar-day new-date))	   
	   (first-date (calendar-nth-named-day 1 zoom-first-day-of-week new-month new-year 1))
	   (first-day (extract-calendar-day first-date))
	   (new-week (1+ (/ (- new-day first-day) 7))))
      (zoom-string 'week new-year new-month new-week))))

(defun zoom-down-week (name &optional today)
  "If the absolute date TODAY is within the week specified by
  string NAME, return a string for TODAY.  Otherwise, return the
  first day in the week.  If TODAY is not given, use the current date."
  (setq today (or today (gsn/calendar-today-absolute)))
  (multiple-value-bind (type year month week) (zoom-parse name 'week)
    (if (not (zoom-contains name today)) 	
	(zoom-string 'day year month 
		     (extract-calendar-day 
		      (calendar-nth-named-day week zoom-first-day-of-week month year)))
      (let* ((today (calendar-gregorian-from-absolute today))
	     (year (extract-calendar-year today))
	     (month (extract-calendar-month today))
	     (day (extract-calendar-day today)))
	(zoom-string 'day year month day)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Day

(defun zoom-day-beg (name)
  "Return the absolute date of the day given by the string NAME."
  (multiple-value-bind (type year month day) (zoom-parse name 'day)
    (calendar-absolute-from-gregorian (list month day year))))

(defun zoom-day-end (name)
  "Return the absolute date of the day given by the string NAME."
  (zoom-day-beg name))

(defun zoom-up-day (name)
  "Return a string for the week that contains the day given by
  the string NAME."
  (multiple-value-bind (type year month day) (zoom-parse name 'day)
    (let* ((first-date (calendar-nth-named-day 1 zoom-first-day-of-week month year))
	   (first-day (extract-calendar-day first-date))
	   (week (1+ (/ (- day first-day) 7))))
      (zoom-string 'week year month week))))

(defun zoom-next-day (name num)
  "Return the NUMth day after the one given by the string NAME."
  (let ((new-date (calendar-gregorian-from-absolute (+ (zoom-day-beg name) num))))
    (zoom-string 'day 
		 (extract-calendar-year new-date)
		 (extract-calendar-month new-date)
		 (extract-calendar-day new-date))))

(defun zoom-down-day (name) 
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar zoom-tests
      '((zoom-parse-to-strings ("2006.Year")  (year "2006" nil nil))
	(zoom-parse-to-strings ("2006.January") (month "2006" "January" nil))
	(zoom-parse-to-strings ("2006.Quarter1") (quarter "2006" "1" nil))
	(zoom-parse-to-strings ("2006.January.Week1") (week "2006" "January" "1"))
	(zoom-parse-to-strings ("2006.01.03") (day "2006" "01" "03"))
	
	(zoom-parse ("2006.Year") (year 2006 nil nil))
	(zoom-parse ("2006.January") (month 2006 1 nil))
	(zoom-parse ("2006.Quarter1") (quarter 2006 1 nil))
	(zoom-parse ("2006.January.Week1") (week 2006 1 1))
	(zoom-parse ("2006.01.03") (day 2006 1 3))
	
	(zoom-string (year 2007) "2007.Year")
	(zoom-string (year "2007") "2007.Year")
	(zoom-string (quarter 2007 2) "2007.Quarter2")
	(zoom-string (quarter "2007" "2") "2007.Quarter2")
	(zoom-string (month 2007 2) "2007.February")
	(zoom-string (month "2007" "February") "2007.February")
	(zoom-string (week 2007 2 2) "2007.February.Week2")
	(zoom-string (week "2007" "February" "2") "2007.February.Week2")
	(zoom-string (day 2007 2 2) "2007.02.02")
	(zoom-string (day "2007" "2" "2") "2007.02.02")
	
	(zoom-contains ("2006.Year" 732311) nil)
	(zoom-contains ("2006.Year" 732312) t)
	(zoom-contains ("2006.Year" 732463) t)
	(zoom-contains ("2006.Year" 732676) t)
	(zoom-contains ("2006.Year" 732677) nil)
	
	(zoom-year-beg ("2006.Year") 732312)
	(zoom-quarter-beg ("2006.Quarter1") 732312)
	(zoom-quarter-beg ("2006.Quarter2") 732402)
	(zoom-quarter-beg ("2006.Quarter3") 732493)
	(zoom-quarter-beg ("2006.Quarter4") 732585)
	(zoom-month-beg ("2006.January") 732312)
	(zoom-week-beg ("2006.January.Week1") 732313)
	(zoom-week-beg ("2006.January.Week2") 732320)
	(zoom-week-beg ("2006.January.Week3") 732327)
	(zoom-week-beg ("2006.January.Week4") 732334)
	(zoom-week-beg ("2006.January.Week5") 732341)
	(zoom-week-beg ("2006.January.Week6") 732348)
	(zoom-day-beg ("2006.02.03") 732345)
	
	(zoom-year-end ("2006.Year") 732676)
	(zoom-quarter-end ("2006.Quarter1") 732401)
	(zoom-quarter-end ("2006.Quarter2") 732492)
	(zoom-quarter-end ("2006.Quarter3") 732584)
	(zoom-quarter-end ("2006.Quarter4") 732676)
	(zoom-month-end ("2006.January") 732342)
	(zoom-week-end ("2006.January.Week1") 732319)
	(zoom-week-end ("2006.January.Week2") 732326)
	(zoom-week-end ("2006.January.Week3") 732333)
	(zoom-week-end ("2006.January.Week4") 732340)
	(zoom-week-end ("2006.January.Week5") 732347)
	(zoom-week-end ("2006.January.Week6") 732354)
	(zoom-day-end ("2006.01.01")  732312)
	
	(zoom-next-year ("2006.Year" 2) "2008.Year")
	(zoom-next-year ("2006.Year" -2) "2004.Year")
	(zoom-next-year ("2006.Year" 0) "2006.Year")
	(zoom-next-quarter ("2006.Quarter2" 5) "2007.Quarter3")
	(zoom-next-quarter ("2006.Quarter2" -5) "2005.Quarter1")
	(zoom-next-quarter ("2006.Quarter2" 0) "2006.Quarter2")
	(zoom-next-month ("2006.June" 13) "2007.July")
	(zoom-next-month ("2006.June" -13) "2005.May")
	(zoom-next-month ("2006.June" 0) "2006.June")
	(zoom-next-week ("2006.April.Week2" 3) "2006.May.Week1")
	(zoom-next-week ("2006.April.Week2" -2) "2006.March.Week4")
	(zoom-next-week ("2006.April.Week2" 0) "2006.April.Week2")
	(zoom-next-day ("2006.04.03" -7) "2006.03.27")
	(zoom-next-day ("2006.04.03" -1) "2006.04.02")
	(zoom-next-day ("2006.04.03" 0) "2006.04.03")
	(zoom-next-day ("2006.04.03" 1) "2006.04.04")
	(zoom-next-day ("2006.04.03" 28) "2006.05.01")
	
	(zoom-up-quarter ("2006.Quarter1") "2006.Year")
	(zoom-up-month ("2006.April") "2006.Quarter2")
	(zoom-up-week ("2006.April.Week1") "2006.April")
	(zoom-up-day ("2006.04.10") "2006.April.Week2")

 	;(calendar-absolute-from-gregorian (4 30 2006) 732431)
	;(calendar-absolute-from-gregorian (4 30 2005) 732066)
	
	;; April 30th, 2006: Should zoom down to Q2, Month 4, Week 4, day 4.30.2006
	(zoom-down-year ("2006.Year" 732431) "2006.Quarter2")
	(zoom-down-quarter ("2006.Quarter2" 732431) "2006.April")
	(zoom-down-month ("2006.April" 732431) "2006.April.Week4")
	(zoom-down-week ("2006.April.Week4" 732431) "2006.04.30")
	
	;; April 30th, 2005: Should zoom down to Q1, January, Week 1, 1.1.2006
	(zoom-down-year ("2006.Year" 732066) "2006.Quarter1")
	(zoom-down-quarter ("2006.Quarter1" 732066) "2006.January")
	(zoom-down-month ("2006.January" 732066) "2006.January.Week1")
	(zoom-down-week ("2006.January.Week1" 732066) "2006.01.02"))
  "A list of lists of the form (function-name function-arguments
  desired-result) which is used to test the functions in the zoom
  package")

(defun zoom-test ()
  "Run all the tests in zoom-tests."
  (dolist (test zoom-tests)
    (let* ((fn (first test))
	   (fn-args (second test))
	   (desired-result (third test))
	   (result (apply fn fn-args)))
      (when (not (equal desired-result result))
	(error "Failed test!"))))
  t)
  


(defun gsn/calendar-today-gregorian ()
  (multiple-value-bind (junk junk junk day month year) (decode-time)
    (list month day year)))

(defun gsn/calendar-today-absolute ()
  (calendar-absolute-from-gregorian (gsn/calendar-today-gregorian)))

(provide 'planner-hierarchy)
;;; planner-hierarchy.el ends here

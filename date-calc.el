;;; date-calc.el --- date calculation and parsing routines

;; Copyright (C) 2003 Doug Alcorn, <doug@lathi.net>

;; Version: 0.1
;; Date: 03 March 2003
;; Author: Doug Alcorn <doug@lathi.net>
;; Keywords: rfc 2445, iCal, calendar, schedule

;; This file is not part of GNU Emacs.

;; date-calc.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; date-calc.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; After several times wishing for the Perl Date::Calc module in
;; elisp, I finally decided to just re-write the code.  I'm not done
;; yet; however, I wanted to get something "published" sooner rather
;; than later.  I'll continue working on this as I can.  It's
;; important to me.  This will serve as a stepping stone to writing an
;; iCalendar/iTIP (rfc 2445, 2446) compliant scheduler.  If you have
;; any problems with the interface to this code, let me know.  I'm
;; certainly willing to be flexible.  What I really want is the
;; functionality.  I'm not too hung up on the actual interface.

;;; Code:
(defconst date-calc-year-of-epoc 70 "Year of reference (epoc)")
(defconst date-calc-century-of-epoc 1900 "Century of reference (epoc)")
(defconst date-calc-eopoc (+ date-calc-year-of-epoc date-calc-century-of-epoc) "reference year (epoc)")

(defconst date-calc-days-in-year-list '((0 0 31 59 90 120 151 181 212 243 273 304 334 365)
					(0 0 31 60 91 121 152 182 213 244 274 305 335 366)))

(defconst date-calc-days-in-month-list '((0 31 28 31 30 31 30 31 31 30 31 30 31)
					 (0 31 29 31 30 31 30 31 31 30 31 30 31)))

(defconst date-calc-languages 11)
(defconst date-calc-language 1)

(defconst date-calc-month-to-text
  '(("???" "???" "???" "???" "???" "???" "???" "???" "???" "???" "???" "???" "???" )
    ("???" "January" "February" "March" "April" "May" "June" "July" "August" "September"
     "October" "November" "December")
    ("???" "janvier" "février" "mars" "avril" "mai" "juin" "juillet" "août" "septembre"
     "octobre" "novembre" "décembre")
    ("???" "Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September"
     "Oktober" "November" "Dezember")
    ("???" "enero" "febrero" "marzo" "abril" "mayo" "junio" "julio" "agosto" "septiembre"
     "octubre" "noviembre" "diciembre")
    ("???" "janeiro" "fevereiro" "março" "abril" "maio" "junho" "julho" "agosto" "setembro" 
     "outubro" "novembro" "dezembro")
    ("???" "januari" "februari" "maart" "april" "mei" "juni" "juli" "augustus" "september"
     "october" "november" "december")
    ("???" "Gennaio" "Febbraio" "Marzo" "Aprile" "Maggio" "Giugno" "Luglio" "Agosto" "Settembre"
     "Ottobre" "Novembre" "Dicembre")
    ("???" "januar" "februar" "mars" "april" "mai" "juni" "juli" "august" "september"
     "oktober" "november" "desember")
    ("???" "januari" "februari" "mars" "april" "maj" "juni" "juli" "augusti" "september"
     "oktober" "november" "december")
    ("???" "januar" "februar" "marts" "april" "maj" "juni" "juli" "august" "september"
     "oktober" "november" "december")
    ("???" "tammikuu" "helmikuu" "maaliskuu" "huhtikuu" "toukokuu" "kesäkuu" "heinäkuu" "elokuu"
     "syyskuu" "lokakuu" "marraskuu" "joulukuu")))

(defconst date-calc-day-of-week-to-text
  '(("???" "???" "???" "???" "???" "???" "???" "???")
    ("???" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
    ("???" "Lundi" "Mardi" "Mercredi" "Jeudi" "Vendredi" "Samedi" "Dimanche")
    ("???" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag" "Sonntag")
    ("???" "Lunes" "Martes" "Miércoles" "Jueves" "Viernes" "Sábado" "Domingo")
    ("???" "Segunda-feira" "Terça-feira" "Quarta-feira" "Quinta-feira" "Sexta-feira" "Sábado" "Domingo")
    ("???" "Maandag" "Dinsdag" "Woensdag" "Donderdag" "Vrijdag" "Zaterdag" "Zondag")
    ("???" "Lunedì" "Martedì" "Mercoledì" "Giovedì" "Venerdì" "Sabato" "Domenica")
    ("???" "mandag" "tirsdag" "onsdag" "torsdag" "fredag" "lørdag" "søndag")
    ("???" "måndag" "tisdag" "onsdag" "torsdag" "fredag" "lördag" "söndag")
    ("???" "mandag" "tirsdag" "onsdag" "torsdag" "fredag" "lørdag" "søndag")
    ("???" "maanantai" "tiistai" "keskiviikko" "torstai" "perjantai" "lauantai" "sunnuntai")))

(defconst date-calc-day-of-week-abbreviation
  '(("" "" "" "" "" "" "" "")
    ("" "" "" "" "" "" "" "")
    ("" "" "" "" "" "" "" "")
    ("" "" "" "" "" "" "" "")
    ("" "" "" "" "" "" "" "")
    ("???" "2ª" "3ª" "4ª" "5ª" "6ª" "Sáb" "Dom")
    ("" "" "" "" "" "" "" "")
    ("" "" "" "" "" "" "" "")
    ("" "" "" "" "" "" "" "")
    ("" "" "" "" "" "" "" "")
    ("" "" "" "" "" "" "" "")
    ("" "" "" "" "" "" "" "")))

(defconst date-calc-english-ordinals '("th" "st" "nd" "rd"))

(defconst date-calc-long-format
  '("%s, %d %s %d"                     ;   0  Default     
    "%s, %s %s %d"                     ;   1  English     
    "%s %d %s %d"                      ;   2  Français    
    "%s, den %d. %s %d"                ;   3  Deutsch     
    "%s, %d de %s de %d"               ;   4  Español     
    "%s, dia %d de %s de %d"           ;   5  Português   
    "%s, %d %s %d"                     ;   6  Nederlands  
    "%s, %d %s %d"                     ;   7  Italiano    
    "%s, %d. %s %d"                    ;   8  Norsk       
    "%s, %d %s %d"                     ;   9  Svenska     
    "%s, %d. %s %d"                    ;  10  Dansk       
    "%s, %d. %sta %d"                   ;  11  suomi       
    ))

(defconst date-calc-language-to-text 
  '("???" "English" "Français" "Deutsch" "Español""Português" "Nederlands" 
    "Italiano" "Norsk" "Svenska""Dansk" "suomi"))

(defun date-calc-is-digit (char)
  (if (string-match "^[0-9]$" char) t nil))

(defun date-calc-is-alnum (char)
  (if (string-match "^[a-zA-Z0-9]$" char) t nil))

(defun date-calc-iso-lc (char)
  (let ((int (char-int char)))
    (if (or (and (>= int #x41) (<= int #x5a))
	    (and (>= int #xc0) (<= int #xd6))
	    (and (>= int #xd8) (<= int #xde)))
	(+ int #x20))))

(defun date-calc-iso-uc (char)
  (let ((int (char-int char)))
    (if (or (and (>= int #x61) (<= int #x7a))
	    (and (>= int #xe0) (<= int #xf6))
	    (and (>= int #xf8) (<= int #xfe)))
	(- int #x20))))

(defun date-calc-year-to-days (year)
  (let ((days (* year 365)))
    (setq days (+ days (setq year (lsh year -2))))
    (setq days (- days (setq year (/ year 25))))
    (setq days (+ days (lsh year -2)))))

(defun date-calc-scan9 (str len idx neg)
  (if (and (stringp str)
	   (>= idx 0)
	   (< idx len))
      (date-calc-is-digit (logxor neg (aref str idx)))
    nil))

(defun date-calc-scanx (str len idx neg)
  (if (and (stringp str)
	   (>= idx 0)
	   (< idx len))
      (date-calc-is-alnum (logxor neg (aref str idx)))))

;(defalias string-to-int date-calc-string-to-number)

(defun date-calc-center (string width)
  "return a string that is WIDTH long with string centered in it"
  (let ((pad (- width (length string)))
	(lpad (/ pad 2))
	(rpad (- pad (/ pad 2))))
    (if (<= pad 0)
	string
      (concat (make-string lpad " ") string (make-string rpad " ")))))

(defun date-calc-blank (width)
  (make-string width "\n"))

(defun date-calc-normalize-time (dd dh dm ds)
  "Normalize the values of the TIME list.  TIME should be of the form (dd dh dm ds).  Returns a list of the same form."
  (let ((quot (/ ds 60)))
    (setq ds (- ds (* quot 60))
	  dm (+ dm quot)
	  quot (/ dm 60))
    (setq dm (- dm (* quot 60))
	  dh (+ dh quot)
	  quot (/ dh 24))
    (setq dh (- dh (* quot 24))
	  dd (+ dd quot))
    (list dd dh dm ds)))

(defun date-calc-normalize-ranges (dd dh dm ds)
  "Normalize the values of the TIME list.  TIME should be of the form (dd dh dm ds).  Returns a list of the same form.  This funciton prevents overflow errors on systems with short longs (e.g. 32-bits)"
  (let (quot)
    (setq quot (/ dh 24))
    (setq dh (- dh (* quot 24)))
    (setq dd (+ dd quot))
    (setq quot (/ dm 60))
    (setq dm (- dm (* quot 60)))
    (setq dh (+ dh quot))
    (date-calc-normalize-time dd dh dm ds)))

(defun date-calc-normalize-signs (dd dh dm ds)
  (let (quot)
    (setq quot (/ ds 86400))
    (setq ds (- ds (* quot 86400)))
    (setq dd (+ dd quot))
    (if (not (= dd 0))
	(if (> dd 0)
	    (if (< ds 0)
		(setq ds (+ ds 86400)
		      dd (1- dd)))
	  (if (> ds 0)
	      (setq ds (- ds 86400)
		    dd (1+ dd)))))
    (setq dh 0 dm 0)
    (if (not (= ds 0))
	(date-calc-normalize-time dd dh dm ds)
      (list dd dh dm ds))))

(defun date-calc-valid-year (year) (>= year 1))
(defun date-calc-valid-month (month) (and month (>= month 1) (<= month 12)))

(defun date-calc-leap-year (year)
  "This function returns 1 if the given YEAR is a leap year and 0 otherwise."
  (if (or (and (zerop (% year 4))
	       (not (zerop (% year 100))))
	  (zerop (% year 400)))
      1 0))

(defun date-calc-days-in-month (year month)
  "This function returns the number of days in the given MONTH of the given YEAR."
  (if (and (date-calc-valid-year year)
	   (date-calc-valid-month month))
      (car (nthcdr month 
		   (car (nthcdr (date-calc-leap-year year) date-calc-days-in-month-list))))))

(defun date-calc-days-in-year (year &optional month)
  "This function returns the number of days in the given YEAR.  If MONTH is [1..12], return the number of days in that YEAR as of the first of that MONTH."
  (car (nthcdr (if (date-calc-valid-month month) month 13)
	       (car (nthcdr (date-calc-leap-year year) date-calc-days-in-year-list)))))

(defun date-calc-check-date (year month day)
  "This function returns true if the given three numerical values YEAR MONTH DAY constitute a valid
    date, and false otherwise."
  (and (date-calc-valid-year year)
       (date-calc-valid-month month)
       (>= day 1)
       (<= day (date-calc-days-in-month year month))))

(defun date-calc-check-time (hour min sec)
  "    This function returns true if the given three numerical values HOUR MIN SEC constitute a valid
    time, and false otherwise."
    (and (>= hour 0) (< hour 24)
	 (>= min 0) (< min 60)
	 (>= sec 0) (< sec 60)))

(defun date-calc-check-business-date (year, week, dow)
    "This function returns true if the given three numerical values YEAR WEEK DOW constitute a valid
    date in business format, and false otherwise.
   
    Beware that this function does NOT compute whether a given date is a business day (i.e., Monday to Friday)!
   
    To do so, use (< (date-calc-day-of-week year month day) 6) instead."
  (and (>= year 1)
       (>= week 1)
       (<= week (date-calc-weeks-in-year year))
       (>= dow 1)
       (<= dow 7)))

(defun date-calc-day-of-year (year month day)
  "This function returns the sum of the number of days in the months starting with January up to and including MONTH in
    the given year YEAR."
  (let ((leap (date-calc-leap-year year)))
    (if (date-calc-check-date year month day)
	(+ day (car (nthcdr month (car (nthcdr leap date-calc-days-in-year-list)))))
      0)))

(defun date-calc-date-to-days (year month day)
  "This function returns the (absolute) number of the day of the given date, where counting starts at the 1st of January
    of the year 1 A.D.

    I.e., (date-calc-date-to-days 1 1 1) returns 1, (date-calc-date-to-days 1 12 31) returns 365, (date-calc-date-to-days 2 1 1) returns 366, etc.
   
    This is sometimes also referred to (not quite correctly) as the Julian date (or day). This may cause confusion, because
    also the number of the day in a year (from 1 to 365 or 366) is frequently called the \"Julian date\".
   
    In fact the calendar that was used BEFORE the Gregorian calendar was the Julian calendar - named after famous Julius
    Caesar, who had instituted it in Roman times. The Julian calendar was less precise because it had too many leap years
    compared to the true mean length of a year, and because rulers often changed it arbitrarily, in order to lengthen their
    own reign, for instance."
(if (date-calc-check-date year month day)
      (+ (date-calc-year-to-days (1- year))
	 (date-calc-day-of-year year month day))
    0))

(defun date-calc-day-of-week (year month day)
  "This function returns the number of the day of week of the given date.
   
    The function returns 1 for Monday, 2 for Tuesday and so on until 7 for Sunday.
   
    Note that in the Hebrew calendar (on which the Christian calendar is based), the week starts with Sunday and ends with
    the Sabbath or Saturday (where according to the Genesis (as described in the Bible) the Lord rested from creating the
    world).
   
    In medieval times, Catholic Popes have decreed the Sunday to be the official day of rest, in order to dissociate the
    Christian from the Hebrew belief.
   
    Nowadays, the Sunday AND the Saturday are commonly considered (and used as) days of rest, usually referred to as the
    \"week-end\".
   
    Consistent with this practice, current norms and standards (such as ISO/R 2015-1971, DIN 1355 and ISO 8601) define the
    Monday as the first day of the week."

  (let ((days (date-calc-date-to-days year month day)))
    (unless (= days 0)
      (setq days (1- days))
      (setq days (% days 7))
      (setq days (1+ days)))
    days))

(defun date-calc-weeks-in-year (year)
  "This function returns the number of weeks in the given YEAR, i.e., either 52 or 53."
  (if (or (= 4 (date-calc-day-of-week year 1 1))
	  (= 4 (date-calc-day-of-week year 12 31)))
      53 52))

(defun date-calc-week-number (year month day)
  "This function returns the number of the week the given date lies in.  If the given date lies in the LAST week of the PREVIOUS year, \"0\" is returned."
  (let* ((first (1- (date-calc-day-of-week year 1 1)))
	 (week (/ (+ first (date-calc-delta-days year 1 1 year month day)) 7)))
    (if (< first 4)
	(1+ week)
      week)))

(defun date-calc-week-of-year (year month day)
  "return '(WEEK YEAR) where WEEK is the week number of YEAR"
  (let (week)
    (if (not (date-calc-check-date year month day))
	nil
      (setq week (date-calc-week-number year month day))
      (if (= week 0)
	  (setq week (date-calc-weeks-in-year (1- year)))
	(if (> week (date-calc-weeks-in-year year))
	    (setq week 1 year (1+ year))))
      (list week year))))

(defun date-calc-monday-of-week (week year)
  "return '(YEAR MONTH DAY) where MONTH and DAY correspond to the Monday of WEEK in YEAR"
  (let ((first (1- (date-calc-day-of-week year 1 1))))
    (if (< first 4)
	(setq week (1- week)))
    (date-calc-add-delta-days year 1 1 (- (* week 7) first))))

(defun date-calc-nth-weekday-of-month-year (year month dow n)
  "This function returns the (year month day) of the N-th day of week DOW in the given MONTH and YEAR; such as, for example, the 3rd Thursday of a given month and year."
  (let ((first (date-calc-day-of-week year month 1))
	delta date)
    (if (or (not (date-calc-check-date year month 1))
	    (< dow 1) (> dow 7)
	    (< n 1) (> n 5))
	nil				; params not valid, error
      (if (< dow first)
	  (setq dow (+ dow 7)))		;the first occurance of dow is in the second week
      (setq delta (+ (- dow first) (* (1- n) 7)))
      (setq date (date-calc-add-delta-days year month 1 delta))
      (if (= month (cadr date))
	  date))))

(defun date-calc-standard-to-business (year month day)
  "This function converts a given date from standard notation (YEAR MONTH DAY (of month)) to business notation (year, week, day of week)."
  (let* ((l (date-calc-week-of-year year month day))
	 (week (if l (car l)))
	 (yy (if (> (length l) 1) (cadr l)))
	 (dow (if (and week yy) (date-calc-day-of-week year month day))))
    (if (and week yy dow)
	(list yy week dow)
      nil)))

(defun date-calc-business-to-standard (year week dow)
  "This function converts a given date from business notation (year, week, day of week) to standard notation (year, month,day (of month))."
  (let* ((first (if (date-calc-check-business-date year week dow)
		    (date-calc-day-of-week year 1 1)))
	
	 (delta (+ (- dow first) (* 7 (1- (+ week (if (> first 4) 1 0))))))
	 (l (date-calc-add-delta-days year 1 1 delta)))
    l))

(defun date-calc-delta-days (year1 month1 day1 year2 month2 day2)
  "This function returns the difference in days between the two given dates. The result is positive if the two dates are in chronological order, i.e., if date #1 comes chronologically BEFORE date #2, and negative if the order of the two dates is reversed."
  (- (date-calc-date-to-days year2 month2 day2)
     (date-calc-date-to-days year1 month1 day1)))

(defun date-calc-delta-hms (hour1 min1 sec1 hour2 min2 sec2)
  "This function returns the difference in days, hours, minutes and seconds between the two given times."
  (let* ((ss (if (and (date-calc-check-time hour1 min1 sec1)
		      (date-calc-check-time hour2 min2 sec2))
		 (- (+ sec2 (* 60 (+ min2 (* 60 hour2))))
		    (+ sec1 (* 60 (+ min1 (* 60 hour1)))))))
	 (l (date-calc-normalize-signs 0 0 0 ss)))
    l))

(defun date-calc-delta-dhms (year1 month1 day1 hour1 min1 sec1
				   year2 month2 day2 hour2 min2 sec2)
  "This function returns the difference in days, hours, minutes and seconds between the two given dates with times."
  (let ((dd (date-calc-delta-days year1 month1 day1 year2 month2 day2))
	(dhms (date-calc-delta-hms hour1 min1 sec1 hour2 min2 sec2)))
    (if (> (length dhms) 0)
	(setcar dhms (+ (aref dmhs 0) dd)))
    dhms))

(defun date-calc-delta-ymd (year1 month1 day1 year2 month2 day2)
  "This function returns the difference between the two dates"
  (let ((yy (- year2 year1))
	(mm (- month2 month1))
	(dd (- day2 day1)))
    (if (and (date-calc-check-date year1 month1 day1)
	     (date-calc-check-date year2 month2 day2))
      (list yy mm dd)
      nil)))

(defun date-calc-delta-ymdhms (year1 month1 day1 hour1 min1 sec1
				     year2 month2 day2 hour2 min2 sec2)
  "This function is based on the function date-calc-delta-ymd above but additionally calculates the time difference. When a carry over from the time difference occurs, the delta day value is adjusted accordingly, thus giving the correct total date/time difference."
  (let* ((dymd (date-calc-delta-ymd year1 month1 day1 year2 month2 day2))
	 (dd (if (> (length dymd) 0) (caddr dymd)))
	 (dhms (date-calc-delta-hms hour1 min1 sec1 hour2 min2 sec2))) 
    (if (and dymd dhms dd)
	(list (car dymd) (cadr dymd) (+ dd (car dhms)) (cadr dhms) (caddr dhms) (cadddr dhms))
      nil)))

(defun date-calc-normalize-dhms (day hour min sec)
  "This function takes four arbitrary values for days, hours, minutes and seconds (which may have different signs) and renormalizes them so that the values for hours, minutes and seconds will lie in the ranges [-23..23], [-59..59] and [-59..59], respectively, and so that all four values have the same sign (or are zero)."
  (let* ((dhms (date-calc-normalize-ranges day hour min sec))
	 (dd (if dhms (car dhms)))
	 (dh (if dhms (cadr dhms)))
	 (dm (if dhms (caddr dhms)))
	 (ds (if dhms (cadddr dhms))))
    (if ds
	(setq ds (+ ds (* 60 (+ dm (* 60 dh))))))
    (if dhms
	(date-calc-normalize-signs dd dh dm ds))))

(defun date-calc-add-delta-days (year month day delta)
  "This function returns (year month day) such that it is YEAR MONTH DAY plus DELTA days"
  (let ((days (date-calc-date-to-days year month day))
	leap)
    (unless (and (not days) (> delta days))
      (setq days (+ days delta))
      (setq year (floor (/ days 365.2425)))
      (setq day (- days (date-calc-year-to-days year)))
      (if (< day 1)
	  (setq day (- days (date-calc-year-to-days (1- year))))
	(setq year (1+ year)))
      (setq leap (date-calc-leap-year year))
      (if (> day (date-calc-days-in-year year))
	  (setq* day (- day (dat-calc-days-in-year year))
		year (1+ year)
		leap (date-calc-leap-year year)))
      (setq month 12)
      (while (and (date-calc-valid-month month)
		  (not (date-calc-check-date year month day)))
	(if (> day (date-calc-days-in-year year month))
	    (setq day (- day (date-calc-days-in-year year month))))
	(unless (date-calc-check-date year month day)
	  (setq month (1- month)))))
    (if (date-calc-check-date year month day)
	(list year month day))))

(defun date-calc-add-delta-dhms (year month day hour min sec dd dh dm ds)
  "This function serves to add a days, hours, minutes and seconds offset to a given date and time, in order to answer questions like \"today and now plus 7 days but minus 5 hours and then plus 30 minutes, what date and time gives that?\""
  (let* ((dhms (date-calc-normalize-ranges dd dh dm ds))
	 (dd (if dhms (car dhms)))
	 (dh (if dhms (cadr dhms)))
	 (dm (if dhms (caddr dhms)))
	 (ds (if dhms (cadddr dhms))))
    (unless (not (and dhms
		      (date-calc-check-date year month day)
		      (date-calc-check-time hour min sec)))
      (setq ds (+ ds (* 60 (+ dm (* 60 dh)))
		  (+ sec (* 60 (+ min (* 60 hour))))))
      (while (< ds 0)
	(setq ds (= ds 86400)
	      dd (1- dd)))
      (if (> ds 0)
	  (setq dh 0 dm 0
	      dmhs (date-calc-normalize-time dd dh dm ds)
	      dd (if dhms (car dhms))
	      hour (if dhms (cadr dhms))
	      min (if dhms (caddr dhms))
	      sec (if dhms (cadddr dhms)))
	(setq hour 0 min 0 sec 0))
      (append (date-calc-add-delta-days year month day dd) (list hour min sec)))))
      
(defun date-calc-add-year-month (year month dy dm)
  "This function returns (year month) as the YEAR and MONTH plus the delta DY year and delta DM month"
  (let (quot)
    (unless (not (or (date-check-valid-year year)
		     (date-check-valid-month month)))
      (unless (= dm 0)
	(setq dm (+ dm (1- month)))
	(setq quot (/ dm 12)
	      dm (- (* quot 12)))
	(if (< dm 0)
	    (setq dm (+ dm 12)
		  quot (1- quot)))
	(setq month (1+ month)
	      dy (+ dy quot)))
      (unless (= dy 0)
	(setq dy (+ dy year)
	      year dy))
      (unless (< year 1)
	(list year month)))))

(defun date-calc-add-delta-ym (year month day dy dm)
  "This function can be used to add a year and/or month offset to a given date."
  (let* ((dym (if (date-calc-check-date year month day)
		 (date-calc-add-year-month year month dy dm)))
	 (dd (if dym (date-calc-days-in-month year month))))
    (if (and dd (> day dd))
	(setq day dd))
    (if dym
	(list (car dym) (cadr dym) day))))

(defun date-calc-add-delta-ymd (year month day dy dm dd)
  "This function serves to add a years, months and days offset to a given date."
  (let ((dym (if (date-calc-check-date year month day)
		 (date-calc-add-year-month year month dy dm))))
    (unless (not dym)
      (setq dd (+ dd (1- day))
	    day 1)
      (if (not (= dd 0))
	  (date-calc-add-delta-days (car dym) (cadr dym) day dd)
	(list (car dym) (cadr dym) day)))))

(defun date-calc-add-delta-ymdhms (year month day hour min sec dyear dmonth dday dh dm ds)
  "This function is the same as date-calc-add-delta-ymd except that a time offset may be given in addition to the year, month and day offset"
  (let* ((dym (if (and (date-calc-check-date year month day)
		       (date-calc-check-time hour min sec))
		  (date-calc-add-year-month year month dyear dmonth))))
    (unless (not dym)
      (setq dday (+ dday (1- day))
	    day 1)
      (date-calc-add-delta-dhms (car dym) (cadr dym) day hour min sec dday dh dm ds))))

(defun date-calc-system-clock (gmt &optional time)
  "This function returns (year month day hour min sec doy dow dst) based on current system clock.  If GMT is non-nil, them gmtime is returned instead of localtime.  Month is a value between 1 and 12; day is between 1 and 31, hour is between 0 and 23, min and sec is between 0 and 59, doy is between 1 and 366, dow is between 1 and 7 and dst is either -1 (for info not available), 0 (dst not in effect), or 1 (dst is in effect)."
  (let* ((system-time (decode-time time))
	 (year (if system-time (nth 5 system-time)))
	 (month (if system-time (nth 4 system-time)))
	 (day (if system-time (nth 3 system-time)))
	 (zone (if system-time (/ (nth 9 system-time) 84600)))
	 (hour (if system-time (nth 2 system-time)))
	 (minute (if system-time (nth 1 system-time)))
	 (second (if system-time (car system-time)))
	 (doy (if system-time (date-calc-date-to-days year month day)))
	 (dow (if system-time (date-calc-day-of-week year month day)))
	 (dst (if system-time (nth 7 system-time))))
    (if gmt
	(append (date-calc-add-delta-dhms year month day hour min sec 0 0 zone 0) doy dow dst)
	(list year month day hour min sec doy dow dst))))

(defun date-calc-gmtime (&optional time)
  (date-calc-system-clock t time))

(defun date-calc-localtime (&optional time)
  (date-calc-system-clock nil time))

(defun date-calc-today (gmt)
  "This function returns (year month day) for today.  If GMT is non-nil, the will be reported for UTC instead of localtime."
  (let ((system-time (date-calc-system-clock gmt)))
    (let (car system-time) (cadr system-time) (caddr system-time))))

(defun date-calc-now (gmt)
  "This function returns (hour minute second) for right now.  If GMT is nil, returns time as local time"
  (let ((system-time (date-calc-system-clock gmt)))
    (list (cadddr system-time) (caddddr system-time) (cadddddr system-time))))

(defun date-calc-today-and-now (gmt)
  "This function returns (year month day hour minute second) for the current date and time.  If GMT is nil, time is expressed as localtime"
  (let ((system-time (date-calc-system-clock gmt)))
    (setcdr (nthcdr 5 system-time) nil)))

(defun date-calc-this-year (gmt)
  "This function returns the current year in localtime.  If GMT is non-nil, use GMT instead of localtime."
  (let ((system-time (date-calc-system-clock gmt)))
    (car system-time)))

(defun date-calc-mktime (year month day hour min sec)
  (let* ((now (current-time))
	(ton (date-calc-today-and-now))
	(delta (date-calc-delta-ymdhms year month day hour min sec
				       (car ton) (cadr ton) (caddr ton)
				       (cadddr ton) (caddddr ton) (cadddddr ton))))))
    
(provide 'date-calc)
;;; date-calc.el ends here

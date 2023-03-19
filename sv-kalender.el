;;; sv-kalender.el --- Swedish calendar for Emacs

;; Copyright (C) 2002-2021 Daniel Jensen

;; Author: Daniel Jensen <daniel@bigwalter.net>
;; Version: 1.11
;; Keywords: calendar swedish localization

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Kommentarer:

;; Swedish calendar localization. Note: Only a few comments in this
;; file are in English. The rest is in Swedish.

;; Denna fil modifierar hur Emacs kalender ser ut. Den byter namn på
;; veckodagar, månader etc., samt inför svenska helgdagar och högtider
;; i stället för de amerikanska.
;;
;; För att använda den svenska kalendern, spara filen i din load-path
;; och använd (load "sv-kalender") i din ~/.emacs.

;;; History (ändringar):

;; 1.11 - UTF-8, feature, fix warnings. (Arthur Miller)
;; 1.10 - Fix abbrev names. (Thanks, Arthur Miller!)
;; 1.9  - Update for Emacs 25
;; 1.8  - Emacs 23 support. GPLv3.
;; 1.6  - Lunar phase names, sunrise/sunset
;;        (månfasernas namn, soluppgång och -nedgång)
;; 1.5  - Cleanup, introduce sv prefix (städning, nytt sv-prefix)
;; 1.4  - Months and days use lower-case initials
;;        (månader och dagar med små begynnelsebokstäver)
;; 1.3  - New flag days, Easter bug fixes
;;        (nya flaggdagar och en bugg i "mer påsk" fixad) (Alan Campbell)
;; 1.2  - Advent Sundays fixed (adventsdagarna justerade) (Alan Campbell)
;; 1.1  - Fat Tuesday moved back a week (fettisdagen flyttad en vecka bakåt).

;;; Code:

(require 'calendar)
(require 'holidays)
(require 'solar)
(require 'lunar)

;; Veckan börjar med en måndag
(setq calendar-week-start-day 1)

;; Använd "europeiska" datum (dag/måndad)
(setq calendar-date-style 'european)

;; Datumformat
(setq calendar-date-display-form
      '((if dayname
            (concat dayname ", "))
        day " " monthname " " year))

;; 24-timmarsklocka utan tidszon
(setq calendar-time-display-form
      '(24-hours ":" minutes))

;; Dagarnas namn
(setq calendar-day-name-array
      ["söndag" "måndag" "tisdag" "onsdag" "torsdag" "fredag" "lördag"]
      calendar-day-abbrev-array
      ["sön" "mån" "tis" "ons" "tors" "fre" "lör"]
      calendar-day-header-array
      ["sö" "må" "ti" "on" "to" "fr" "lö"])

;; Månadernas namn
(setq calendar-month-name-array
      ["januari" "februari" "mars" "april" "maj" "juni"
       "juli" "augusti" "september" "oktober" "november" "december"]
      calendar-month-abbrev-array
      ["jan" "feb" "mar" "apr" "maj" "jun"
       "jul" "aug" "sep" "okt" "nov" "dec"])

;; Årets vändpunkter
(eval-after-load "solar"
  '(setq solar-n-hemi-seasons
         '("Vårdagjämningen" "Sommarsolståndet"
           "Höstdagjämningen" "Vintersolståndet")))

;; Månfaser
(defadvice lunar-phase-name (around sv-lunar-phase-name activate)
  "Månfasernas namn på svenska."
  (setq ad-return-value
	(let ((phase (ad-get-arg 0)))
	  (cond ((= 0 phase) "Nymåne")
		((= 1 phase) "Växande halvmåne")
		((= 2 phase) "Fullmåne")
		((= 3 phase) "Avtagande halvmåne")))))

;; Soluppgång och -nedgång
(defadvice solar-sunrise-sunset-string (around sv-solar-sunrise-sunset-string
                                               activate)
  "Soluppgång och solnedgång på svenska."
  (setq ad-return-value
        (let ((l (solar-sunrise-sunset date)))
          (format
           "%s, %s vid %s (%s timmar dagsljus)"
           (if (car l)
               (concat "Sol upp " (apply 'solar-time-string (car l)))
             "Ingen soluppgång")
           (if (car (cdr l))
               (concat "ned " (apply 'solar-time-string (car (cdr l))))
             "ingen solnedgång")
           (eval calendar-location-name)
           (car (cdr (cdr l)))))))

;; Göm vissa helgdagar?
(defvar sv-hide-some-holidays nil
  "Non-nil means some holidays won't show in the calendar.
Om icke-nil, göm vissa helgdagar i kalendern.")

;; Påskdagen (from holiday-easter-etc)
(defun sv-easter (year)
  "Calculate the date for Easter in YEAR.
Beräkna påskdagen för år YEAR."
  (let* ((century (1+ (/ year 100)))
         (shifted-epact (% (+ 14 (* 11 (% year 19))
                              (- (/ (* 3 century) 4))
                              (/ (+ 5 (* 8 century)) 25)
                              (* 30 century))
                           30))
         (adjusted-epact (if (or (= shifted-epact 0)
                                 (and (= shifted-epact 1)
                                      (< 10 (% year 19))))
                             (1+ shifted-epact)
                           shifted-epact))
         (paschal-moon (- (calendar-absolute-from-gregorian
                           (list 4 19 year))
                          adjusted-epact)))
    (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))

;; Helgdagar
(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Nyårsdagen")
        (holiday-fixed 1 6 "Trettondedag jul")

        ;; Påsk och pingst
        (holiday-filter-visible-calendar
         (mapcar
          (lambda (dag)
            (list (calendar-gregorian-from-absolute
                   (+ (sv-easter displayed-year) (car dag)))
                  (cadr dag)))
          '((  -2 "Långfredagen")
            (  -1 "Påskafton")
            (   0 "Påskdagen")
            (  +1 "Annandag påsk")
            ( +39 "Kristi himmelfärdsdag")
            ( +49 "Pingstdagen")
            ( +50 "Annandag pingst"))))

        (holiday-fixed 5 1 "Första maj")

        (let ((midsommar-d (calendar-dayname-on-or-before
                            6 (calendar-absolute-from-gregorian
                               (list 6 26 displayed-year)))))
          ;; Midsommar
          (holiday-filter-visible-calendar
           (list
            (list
             (calendar-gregorian-from-absolute (1- midsommar-d))
             "Midsommarafton")
            (list
             (calendar-gregorian-from-absolute midsommar-d)
             "Midsommardagen")
            ;; Alla helgons dag
            (list
             (calendar-gregorian-from-absolute
              (calendar-dayname-on-or-before
               6 (calendar-absolute-from-gregorian
                  (list 11 6 displayed-year))))
             "Alla helgons dag"))))

        (holiday-fixed 12 25 "Juldagen")
        (holiday-fixed 12 26 "Annandag jul")))

;; Andra högtider
(setq holiday-other-holidays
      '((holiday-fixed 1 13 "Tjugondag Knut")
        (unless sv-hide-some-holidays
          (holiday-fixed 1 28 "Konungens namnsdag"))
        (unless sv-hide-some-holidays
          (holiday-fixed 2 2 "Kyndelsmässodagen"))
        (holiday-fixed 2 14 "Alla hjärtans dag")

        ;; Fettisdagen
        (holiday-filter-visible-calendar
         (list
          (list
           (calendar-gregorian-from-absolute
            (calendar-dayname-on-or-before
             2 (- (sv-easter displayed-year) 47)))
           "Fettisdagen")))

        (holiday-fixed 3 8 "Internationella kvinnodagen")
        (unless sv-hide-some-holidays
          (holiday-fixed 3 12 "Kronprinsessans namnsdag"))
        (holiday-fixed 3 25 "Vårfrudagen")

        ;; Mer påsk
        (holiday-filter-visible-calendar
         (mapcar
          (lambda (dag)
            (list (calendar-gregorian-from-absolute
                   (+ (sv-easter displayed-year) (car dag)))
                  (cadr dag)))
          (if sv-hide-some-holidays
              '(( -3 "Skärtorsdagen"))
            '(( -7 "Palmsöndagen")
              ( -4 "Dymmelonsdagen")
              ( -3 "Skärtorsdagen")))))

        (unless sv-hide-some-holidays
          (holiday-fixed 4 30 "Konungens födelsedag"))
        (unless sv-hide-some-holidays
          (holiday-fixed 4 1 "Första april"))
        (holiday-fixed 4 30 "Valborgsmässoafton")
        (holiday-float 5 0 -1 "Mors dag")
        (holiday-fixed 6 6 "Sveriges nationaldag")
        (unless sv-hide-some-holidays
          (holiday-fixed 7 14 "Kronprinsessans födelsedag"))
        (unless sv-hide-some-holidays
          (holiday-fixed 8 8 "Drottningens namnsdag"))
        (holiday-fixed 10 24 "FN-dagen")
        (holiday-float 11 0 2 "Fars dag")
        (unless sv-hide-some-holidays
          (holiday-fixed 11 6 "Gustaf Adolfsdagen"))
        (holiday-fixed 11 10 "Mårtensafton")
        (holiday-float 12 0 -4 "Första advent" 24)
        (holiday-float 12 0 -3 "Andra advent" 24)
        (holiday-float 12 0 -2 "Tredje advent" 24)
        (holiday-float 12 0 -1 "Fjärde advent" 24)
        (holiday-fixed 12 10 "Nobeldagen")
        (holiday-fixed 12 13 "Lucia")
        (unless sv-hide-some-holidays
          (holiday-fixed 12 23 "Drottningens födelsedag"))
        (holiday-fixed 12 24 "Julafton")
        (holiday-fixed 12 31 "Nyårsafton")))

;; Solstånd, dagjämningar, vinter- och sommartid
(setq holiday-solar-holidays
      (if sv-hide-some-holidays
          nil
        '((if (fboundp 'atan)
              (solar-equinoxes-solstices))
          (if (progn
                (require 'cal-dst)
                t)
              (funcall 'holiday-sexp calendar-daylight-savings-starts
                       '(format "Sommartid börjar %s"
                                (if
                                    (fboundp 'atan)
                                    (solar-time-string
                                     (/ calendar-daylight-savings-starts-time
                                        (float 60))
                                     calendar-standard-time-zone-name)
                                  ""))))
          (funcall 'holiday-sexp calendar-daylight-savings-ends
                   '(format "Vintertid börjar %s"
                            (if
                                (fboundp 'atan)
                                (solar-time-string
                                 (/ calendar-daylight-savings-ends-time
                                    (float 60))
                                 calendar-daylight-time-zone-name)
                              ""))))))

;; Listan med kalenderns helgdagar
(setq calendar-holidays
      (append holiday-general-holidays holiday-local-holidays
              holiday-other-holidays holiday-solar-holidays))

(provide 'sv-kalender)

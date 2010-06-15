;;; naf-mode-dates.el --- regexp variables for matching dates in naf-mode
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2008-2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: naf-mode-dates.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2008
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: matching, abbrev, il8n, naf-mode, name authority file

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; naf-mode-dates provides regexp variables for matching dates in `naf-mode'
;; name authority files.
;;
;; FUNCTIONS:►►►
;;
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;; `naf-weekday-alist', `naf-month-abbrev-alist', `naf-mode-simple-date',
;; `naf-mode-circa-dates', `naf-mode-year-range', `naf-mode-french-dates',
;; `naf-mode-french-days', `naf-mode-english-days', `naf-mode-date-string',
;; `naf-mode-lifespan', `naf-mode-english-dates',
;; `naf-mode-active-date', `naf-mode-active-date-flags-solo',
;;
;; FACES:
;;
;; VARIABLES:
;; `*naf-mode-dates-xrefs*', 
;; `naf-active-date-flags', `naf-active-date-flags-solo',
;; `naf-active-date-flags-paren',
;; `*regexp-french-date-prefix*', `*regexp-french-date-siecle*',
;; `naf-mode-benezit-date',
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `naf-mode-active-date'            <- naf-mode-db-flags.el
;; `naf-mode-active-date-flags-solo' <- naf-mode-db-flags.el
;; `naf-active-date-flags'           <- naf-mode-db-flags.el
;; `naf-active-date-flags-solo'      <- naf-mode-db-flags.el
;; `*regexp-french-date-prefix*'     <- naf-mode-db-flags.el
;; `*regexp-french-date-siecle*'     <- naf-mode-db-flags.el
;; `naf-mode-benezit-date'           <- naf-mode-db-flags.el
;; `naf-active-date-flags-paren'     <- naf-mode-db-flags.el
;;
;; TODO:
;; Following need to be :RENAMED   -> cannonical name
;; `naf-mode-benezit-date'         -> :RENAME-ME -> -benezit-date-flag
;;
;; :SEE Bottom of file for enemeration of date related strings as yet
;; un-implemented by the regexps `*regexp-french-date-siecle*' and
;; `*regexp-french-date-prefix*' which are currently still lists.
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/naf-mode-dates.el
;; FIRST-PUBLISHED: <Timestamp: #{2009-11-21T20:45:38-05:00Z}#{09477} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing naf-mode-dates. }
;;
;; FILE-CREATED: 2008
;; HEADER-ADDED: <Timestamp: #{2009-08-22T12:24:17-04:00Z}#{09346} - by MON>
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2008-2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

;;; ==============================
(provide 'naf-mode-dates)
;;; ==============================

;;; ==============================
;;; :CHANGESET 1855
;;; :CREATED <Timestamp: #{2010-06-14T12:59:31-04:00Z}#{10241} - by MON KEY>
(defvar *naf-mode-date-xrefs* nil 
  "*List of  variables which xref each other in naf-mode-dates package.\n
:SEE :FILE naf-mode-dates.el")
;;
(unless (bound-and-true-p *naf-mode-date-xrefs*)
  (setq *naf-mode-date-xrefs*
        '(naf-weekday-alist
          naf-month-abbrev-alist
          naf-mode-simple-date
          naf-mode-circa-dates
          naf-mode-year-range
          naf-mode-french-dates
          naf-mode-french-days
          naf-mode-english-days
          naf-mode-date-string
          naf-mode-lifespan
          naf-mode-benezit-date
          naf-active-date-flags
          naf-mode-active-date
          naf-active-date-flags-paren
          naf-mode-active-date-flags-paren
          naf-active-date-flags-solo
          naf-mode-active-date-flags-solo
          naf-mode-english-dates
          *regexp-french-date-prefix*
          *regexp-french-date-siecle*
          mon-help-naf-mode-faces
          )))

;;; ==============================
(defconst naf-weekday-alist
  '(("Sunday" . 0)
    ("Sun" . 0) ("Sun." . 0) ("Sund." 0) 
    ("Dimanche" . 0)  ("dimanche" . 0)
    ;;
    ("Monday" . 1) ("monday" . 1)  
    ("Mon" . 1) ("Mon." . 1) 
    ("Lundi" . 1)  ("lundi" . 1)
    ;;
    ("Tuesday" . 2) ("tuesday" . 2)  
    ("Tue" . 2) ("Tue." . 2) ("Tues" . 2) 
    ("Mardi" . 2)  ("mardi" . 2)
    ;;
    ("Wednesday" . 3) ("wednesday" . 3) 
    ("Wed" . 3) ("Wed." . 3) ("Wedn." . 3) 
    ("Mercredi" . 3)  ("mercredi" . 3)
    ;;
    ("Thursday" . 4) ("thursday" . 4) 
    ("Thu" . 4) ("Thu." . 4) ("Thurs" . 4) ("thurs." . 4) 
    ("Jeudi" . 4)  ("jeudi" . 4)
    ;;
    ("Friday" . 5) ("friday" . 5) 
    ("Fri" . 5) ("Fri." . 5)  ("fri." . 5) 
    ("Vendredi" . 5)  ("vendredi" . 5)
    ;;
    ("Saturday" . 6) ("saturday" . 6) 
    ("Sat" . 6) ("Sat." . 6) 
    ("Samedi" . 6)  ("samedi" . 6))
  "A list mapping string representation of weekdays to their indexed location.\n
Included abbreviated and downcased forms. Index is 0 based beginning with Sunday.\n
:EXAMPLE\n\n\(assoc-string \"Sun.\" naf-weekday-alist\)\n
\(assoc-string \"dimanche\" naf-weekday-alist\)\n
\(eq \(cdr \(assoc-string \"jeudi\" naf-weekday-alist\)\)
    \(cdr \(assoc-string \"Thursday\" naf-weekday-alist\)\)\)\n
:SEE-ALSO `naf-mode-french-days', `naf-mode-english-days', `naf-month-abbrev-alist'.
:USED-IN `naf-mode'.\n►►►")
;;
;;; :TEST-ME (eq (cdr (assoc-string "jeudi" naf-weekday-alist))
;;;              (cdr (assoc-string "Thursday" naf-weekday-alist)))
;;
;;;(progn (makunbound 'naf-weekday-alist) (unintern 'naf-weekday-alist) )

;;; ==============================
(defconst naf-month-abbrev-alist 
  '(("January" . 1) ("january" . 1) ("Janvier" . 1)
    ("janvier" . 1)
    ("Jan" . 1) ("Jan." . 1) ("jan" . 1) ("jan." . 1)
    ;;
    ("February" . 2) ("february" . 2)
    ("Février" . 2) ("février" . 2) ("Fevrier" . 2) ("fevrier" . 2)
    ("Feb" . 2) ("Feb." . 2)  ("feb" . 2) ("feb." . 2)
    ;;
    ("March" . 3) ("march" . 3) ("Mars" . 3) ("mars" . 3)
    ("Mar" . 3) ("Mar." . 3)  ("mar" . 3) ("mar." . 3)
    ;;
    ("April" . 4) ("april" . 4) ("Avril" . 4) ("avril" . 4)
    ("Apr" . 4) ("Apr." . 4) ("apr" . 4) ("apr." . 4)
    ;;
    ("May" . 5) ("may" . 5) ("Mai" . 5) ("mai" . 5)
    ;;
    ("June" . 6) ("june" . 6) ("Juin" . 6) ("juin" . 6) 
    ("Jun" . 6) ("Jun." . 6)  ("jun" . 6) ("jun." . 6)
    ;;
    ("July" . 7) ("july" . 7) ("Juillet" . 7) ("juillet" . 7) 
    ("Jul" . 7) ("Jul." . 7)  ("jul" . 7) ("jul." . 7)
    ;;
    ("August" . 8) ("august" . 8)
    ("Août" . 8) ("Aout" . 8) ("août" . 8) ("aout" . 8) 
    ("Aug" . 8) ("Aug." . 8) ("aug" . 8) ("aug." . 8)
    ;;
    ("September" . 9) ("september" . 9) ("Septembre" . 9) ("septembre" . 9) 
    ("Sep" . 9) ("Sep." . 9) ("Sept" . 9) ("sep" . 9) ("sep." . 9) ("sept" . 9)
    ;;
    ("October" . 10) ("october" . 10) 
    ("Octobre" . 10) ("octobre" . 11) 
    ("Oct" . 10) ("Oct." . 10) ("oct" . 10) ("oct." . 10)
    ;;
    ("November" . 11) ("november" . 11)
    ("Novembre" . 11) ("novembre" . 11) 
    ("Nov" . 11) ("Nov." . 11) ("nov" . 11) ("nov." . 11)
    ;;
    ("December" . 12) ("december" . 12) 
    ("Décembre" . 12) ("décembre" . 12) 
    ("Dec" . 12) ("Dec." . 12)  ("dec" . 12) ("dec." . 12))
  "A list of English Months mappe to their indexed calendar location.\n
Included abbrevieated and downcased forms. 
Index is 1 based beginning with January.\n
:EXAMPLE\n\n\(assoc-string \"décembre\" naf-month-abbrev-alist\)\n
\(assoc-string \"December\" naf-month-abbrev-alist\)\n
\(eq \(cdr \(assoc-string \"février\" naf-month-abbrev-alist\)\)
     \(cdr \(assoc-string \"February\" naf-month-abbrev-alist\)\)\)\n
:SEE-ALSO `naf-mode-french-dates', `naf-weekday-alist'.
:USED-IN `naf-mode'.\n►►►")
;;
;;; :TEST-ME (eq (cdr (assoc-string "février" naf-month-abbrev-alist))
;;;              (cdr (assoc-string "February" naf-month-abbrev-alist)))
;;
;;;(progn (makunbound 'naf-month-abbrev-alist) (unintern 'naf-month-abbrev-alist) )

;;; ==============================
;;; :ENGLISH-MONTHS
;;; (concat
;;;  "\\(A\\(pr\\(\\.\\|il\\)\\|ug\\(\\.\\|ust\\)\\)\\|Dec\\(\\.\\|ember\\)"
;;;  "\\|Feb\\(\\.\\|ruary\\)\\|J\\(an\\(\\.\\|uary\\)\\|u\\(l[.y]\\|n[.e]\\)\\)"
;;;  "\\|Ma\\(r\\(\\.\\|ch\\)\\|y\\)\\|Nov\\(\\.\\|ember\\)\\|Oct\\(\\.\\|ober\\)"
;;;  "\\|Sep\\(\\.\\|t\\(\\.\\|ember\\)\\)\\)")
;;; 
(defconst naf-mode-english-dates
  (concat
   ;; :ENGLISH-MONTHS
   "\\(A\\(pr\\(\\.\\|il\\)\\|ug\\(\\.\\|ust\\)\\)\\|Dec\\(\\.\\|ember\\)"
   "\\|Feb\\(\\.\\|ruary\\)\\|J\\(an\\(\\.\\|uary\\)\\|u\\(l[.y]\\|n[.e]\\)\\)"
   "\\|Ma\\(r\\(\\.\\|ch\\)\\|y\\)\\|Nov\\(\\.\\|ember\\)\\|Oct\\(\\.\\|ober\\)"
   "\\|Sep\\(\\.\\|t\\(\\.\\|ember\\)\\)\\)"
   ;; :DATE-ORDINALS
   " \\([0-3]?+[0-9]\\)\\(:?[rd\|th\|st\|,]+\\) \\<[0-9]\\{4\\}\\>")
  "Manually generated regexp for font-locking English datestrings.\n
:EXAMPLE\n\n\(let \(\(cnt 0\)\)
  \(dotimes \(e-mon 25 cnt\)
    \(search-forward-regexp naf-mode-english-dates nil t\)
    \(when \(match-string 0\) \(incf cnt 1\)\)\)\)\n
 Jan. 01, 1899\n January 10, 1946
 Feb. 22, 1922\n February 29, 1704
 Mar. 15, 1917\n March 5, 1933
 Apr. 20, 1902\n April 20, 1884
 May 9, 1934
 Jun. 30, 1895\n June 29, 1946
 Jul. 31, 1901\n July 30, 1975
 Aug. 6th, 1855\n August 24, 1995
 Sep. 16th, 1992\n Sept. 11th, 2001\n September 03, 1929 
 Oct. 24th, 1928\n October 19, 1987\n October 3rd, 2008
 Nov. 4th, 1901\n November 21st, 1920
 Dec. 3rd, 1901\n December 14, 1825\n
:SEE-ALSO `naf-mode-french-dates', `naf-month-abbrev-alist', `naf-mode-english-days'.
:USED-IN `naf-mode'.\n►►►")
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (let ((cnt 0))
;; |   (dotimes (e-mon 7 cnt)
;; |     (search-forward-regexp naf-mode-english-dates nil t)
;; |     (when (match-string 0) (incf cnt 1))))
;; | 
;; | Jan. 01, 1899 January 10, 1946
;; | Feb. 22, 1922 February 29, 1704
;; | Mar. 15, 1917 March 5, 1933
;; | Apr. 20, 1902 April 20, 1884
;; | May 9, 1934
;; | Jun. 30, 1895 June 29, 1946
;; | Jul. 31, 1901 July 30, 1975
;; | Aug. 6th, 1855 August 24, 1995
;; | Sep. 16th, 1992 Sept. 11th, 2001 September 03, 1929
;; | Oct. 24th, 1928 October 19, 1987 October 3rd, 2008
;; | Nov. 4th, 1901 November 21st, 1920
;; | Dec. 3rd, 1901 December 14, 1825
;; `----
;;
;;;(progn (makunbound 'naf-mode-english-dates) (unintern 'naf-mode-english-dates) )

;;; ==============================
;;; :ENGLISH-WEEKDAYS
;;; "\\<\\(Friday\\|Monday\\|S\\(aturday\\|unday\\)\\|T\\(hursday\\|uesday\\)\\|Wednesday\\)\\>"
(defconst naf-mode-english-days
  "\\<[MTWFS]\\(on\\|ues\\|ednes\\|hurs\\|ri\\|atur\\|un\\)\\(day\\)\\>"
  "Manually generated regexp for font-locking full spelling of English days of week.\n
:EXAMPLE\n\n\(let \(\(cnt 0\)\)
  \(dotimes \(days 7 cnt\) 
    \(search-forward-regexp naf-mode-english-days nil t\)
    \(when \(eq \(match-end 0\) \(point\)\) \(incf cnt 1\)\)\)\)\n
 Monday\n Tuesday\n Wednesday\n Thursday\n Friday\n Saturday\n Sunday\n
:SEE-ALSO `naf-mode-french-days', `naf-weekday-alist', `naf-mode-english-dates'.
:USED-IN `naf-mode'.\n►►►")
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| (let ((cnt 0))
;;|   (dotimes (days 7 cnt) 
;;|     (search-forward-regexp naf-mode-english-days nil t)
;;|     (when (eq (match-end 0) (point)) (incf cnt 1))))
;;| 
;;|  Monday
;;|  Tuesday
;;|  Wednesday
;;|  Thursday
;;|  Friday
;;|  Saturday
;;|  Sunday
;;`----
;;
;;;(progn (makunbound 'naf-mode-english-days) (unintern 'naf-mode-english-days) )

;;; ==============================
;;; :FRENCH-MONTHS
;;; (concat "[A-Za-z]"
;;;         "\\(\\(\\(oût\\|vril\\|ai\\|ars\\)\\)\\|\\(\\(anv\\|évr\\)\\(ier\\)\\)\\|"
;;;         "\\(\\(cto\\|epte\\|ove\\|éce\\)\\(m?+bre\\)\\)"
;;;         "\\|\\(\\(ui\\)\\(n\\|l+et\\)\\)\\)")
(defconst naf-mode-french-dates
  (concat "\\<[0-9]\\{1,2\\}\\> "
          "\\(A\\(o\\(ut\\|ût\\)\\|vr\\(\\.\\|il\\)\\)\\|"
          "Déc\\(\\.\\|embre\\)\\|Fév\\(\\.\\|rier\\)\\|"
          "J\\(an\\(\\.\\|vier\\)\\|ui\\(l\\(\\.\\|let\\)\\|n\\)\\)\\|"
          "Ma\\(i\\|rs\\)\\|Novembre\\|Octobre\\|Sep\\(\\.\\|tembre\\)\\|"
          "a\\(o\\(ut\\(\\)?\\|ût\\(\\)?\\)\\|vr\\(\\.\\|il\\)\\)\\|"
          "déc\\(\\.\\|embre\\)\\|fév\\(\\.\\|rier\\)\\|j\\(an\\(\\.\\|vier\\)\\|"
          "ui\\(l\\(\\.\\|let\\)\\|[n]\\)\\)\\|ma\\(rs\\(\\)?\\|[i]\\)\\|"
          "nov\\(\\.\\|embre\\)\\|oct\\(\\.\\|obre\\)\\|sep\\(\\.\\|tembre\\)\\) "
          "\\<[0-9]\\{4\\}\\>")
  "Manually generated regexp for French style dates of form NN Mmmm.\n
:EXAMPLE\n\n\(let \(\(cnt 0\)\)
  \(dotimes \(fdt 12 cnt\)
    \(search-forward-regexp naf-mode-french-dates nil t\)
    \(when \(eq \(match-end 0\) \(point\)\) \(incf cnt\)\)\)\)\n
 01 Janvier 1901\n 2 Février 1902\n 03 Mars 1903
 4 Avril 1904\n 05 Mai 1905\n 6 Juin 1906
 07 Juillet 1907\n 8 Août 1908\n 9 Septembre 1909
 10 Octobre 1910\n 21 Novembre 1921\n 31 Décembre 1931\n
:SEE-ALSO `naf-mode-english-dates', `naf-month-abbrev-alist',
`naf-mode-french-days'.\n:USED-IN `naf-mode'.\n►►►")
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| (let ((cnt 0))
;;|   (dotimes (fdt 12 cnt)
;;|     (search-forward-regexp naf-mode-french-dates nil t)
;;|     (when (eq (match-end 0) (point)) (incf cnt))))
;;| 
;;|  01 Janvier 1901
;;|  2 Février 1902
;;|  03 Mars 1903
;;|  4 Avril 1904
;;|  05 Mai 1905
;;|  6 Juin 1906
;;|  07 Juillet 1907
;;|  8 Août 1908
;;|  9 Septembre 1909
;;|  10 Octobre 1910
;;|  21 Novembre 1921
;;|  31 décembre 1931
;;`----
;;
;;;(progn (makunbound 'naf-mode-french-dates) (unintern 'naf-mode-french-dates) )

;;; ==============================
;;; :FRENCH-WEEKDAYS
;;;"\\<\\(Dimanche\\|Jeudi\\|Lundi\\|M\\(ardi\\|ercredi\\)\\|Samedi\\|Vendredi\\)\\>")
(defconst naf-mode-french-days
  "\\<\\([JLMSV]\\(ar\\|ame\\|endre\\|ercre\\|eu\\|un\\)\\(di\\)\\)\\|\\(Dimanche\\)\\>"
  "Manually generated regexp for French days of week.\n
:EXAMPLE\n\n\(let \(\(cnt 0\)\)
   \(dotimes \(fday 7 cnt\) 
     \(search-forward-regexp naf-mode-french-days nil t\)
     \(when \(eq \(match-end 0\) \(point\)\) \(incf cnt 1\)\)\)\)\n
 Dimanche\n Lundi\n Mardi\n Mercredi\n Jeudi\n Vendredi\n Samedi\n
:SEE-ALSO `naf-mode-english-days', `naf-weekday-alist', `naf-mode-french-dates'.
:USED-IN `naf-mode'.\n►►►")
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| (let ((cnt 0))
;;|   (dotimes (fday 7 cnt) 
;;|     (search-forward-regexp naf-mode-french-days nil t)
;;|     (when (eq (match-end 0) (point)) (incf cnt 1))))
;;| 
;;|  Dimanche
;;|  Lundi
;;|  Mardi
;;|  Mercredi
;;|  Jeudi
;;|  Vendredi
;;|  Samedi
;;`----
;;
;;;(progn (makunbound 'naf-mode-french-days) (unintern 'naf-mode-french-days) )

;;; ==============================
;;; "\_<[0-9]\{4\}\_>"
(defconst naf-mode-simple-date "\\_<[0-9]\\{4\\}\\_>"
  "Manually generated regexp simple year datestrings.\n
:EXAMPLE\n\n\(search-forward-regexp naf-mode-simple-date\)\n\n ' 1999 '\n
:SEE-ALSO `naf-mode-lifespan',`naf-mode-year-range', `naf-mode-date-string'.
:USED-IN `naf-mode'.\n►►►")
;;
;;; :TEST-ME (search-forward-regexp naf-mode-simple-date) ' 1999 '
;;;(progn (makunbound 'naf-mode-simple-date) (unintern 'naf-mode-simple-date) )

;;; ==============================
;;; :NOTE Following regex works - don't alter - make copies!
;;; \(\((\)\([0-9]\{4\}?\)\(-?\)\([0-9]\{4\}?\)\()\)\)
(defconst naf-mode-year-range
  '"\\(\\((\\)\\([0-9]\\{4\\}?\\)\\(-?\\)\\([0-9]\\{4\\}?\\)\\()\\)\\)"
  "Manually generated regexp for date-strings nested inside parens.\n
:EXAMPLE\n\n\(let \(\(cnt 0\)\)
   \(dotimes \(yrng 4 cnt\)
     \(search-forward-regexp naf-mode-year-range nil t\)
     \(when \(eq \(match-end 0\) \(point\)\) \(incf cnt 1\)\)\)\)\n
 \(1899-\) \(-1999\) \(1899\) \(1899-1999\)\n
:SEE-ALSO `naf-mode-simple-date', `naf-mode-date-string'.\n:USED-IN `naf-mode'.\n►►►")
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| (let ((cnt 0))
;;|   (dotimes (yrng 4 cnt)
;;|     (search-forward-regexp naf-mode-year-range nil t)
;;|     (when (eq (match-end 0) (point)) (incf cnt 1))))
;;| 
;;|  (1899-) 
;;|  (-1999)
;;|  (1899)
;;|  (1899-1999)
;;`----
;;
;;;(progn (makunbound 'naf-mode-year-range) (unintern 'naf-mode-year-range) )

;;; ==============================
(defconst naf-mode-date-string
  '"\\([0-9]\\{2,4\\}\\(-\\|/\\)[0-9]?+\\(-\\|/\\)[0-9]\\{2,4\\}\\)"
   "Manually generated regexp for font-locking '-' and '/' delimited datestrings.\n
:EXAMPLE\n\n\(let \(\(cnt 0\)\)
  \(dotimes \(dt-str 4 cnt\) 
    \(search-forward-regexp naf-mode-date-string nil t\)
    \(when \(match-string 0\) \(incf cnt 1\)\)\)\)\n\n
 08-07-1998\n 08/07/1998\n 1998-08-07\n 1998/08/07\n
:SEE-ALSO `naf-mode-simple-date', `naf-mode-year-range', `naf-mode-lifespan',
`naf-mode-circa-dates'.\n:USED-IN `naf-mode'.\n►►►")
;; 
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;|(let ((cnt 0))
;;|  (dotimes (dt-str 4 cnt) 
;;|    (search-forward-regexp naf-mode-date-string nil t)
;;|    (match-string 0)
;;|    (when (match-string 0) (incf cnt 1))))
;;|
;;|08-07-1998 08/07/1998 1998-08-07 1998/08/07
;;`----
;;
;;;(progn (makunbound 'naf-mode-date-string) (unintern 'naf-mode-date-string) )

;;; ==============================
;;; :LIFESPAN-WITH-WORD-BOUNDARIES
;;;   '"\\(\\<[(]?+[0-9]\\{4\\}-[0-9]\\{4\\}[)]?+\\>\\)" )
;;
;;; :LIFESPAN-WITHOUT-WORD-BOUNDARIES
;;;  '"\\([(]?+[0-9]\\{4\\}-[0-9]\\{4\\}[)]?+\\)" )
(defconst naf-mode-lifespan "\\([(]?+[0-9]\\{4\\}-[0-9]\\{4\\}[)]?+\\)"
  "Manually generated regexp for font-locking lifespans formatted as \(1900-1899\).\n
:EXAMPLE\n\n\(search-forward-regexp naf-mode-lifespan nil t\)\n\(1800-1901\)\n
:SEE-ALSO `naf-mode-circa-dates', `naf-mode-simple-date', `naf-mode-year-range',
`naf-mode-date-string'.\n:USED-IN `naf-mode'.\n►►►")
;;
;;; :TEST-ME (search-forward-regexp naf-mode-lifespan nil t) (1800-1901)
;;
;;;(progn (makunbound 'naf-mode-lifespan) (unintern 'naf-mode-lifespan) )

;;; ==============================
;;; \\<\\(active ca\\.\\|b\\.\\|c\\(a\\.\\|irca\\)\\|d\\.\\) [0-9]\\{4\\}\\>
(defconst naf-mode-circa-dates
  "\\<\\(active ca\\.\\|b\\.\\|c\\(a\\.\\|irca\\)\\|d\\.\\) [0-9]\\{4\\}\\>"
  "Manually generated regexp for circa datestrings.\n
:EXAMPLE\n\n\(let \(\(cnt 0\)\)
   \(dotimes \(circa 5 cnt\)
     \(search-forward-regexp naf-mode-circa-dates nil t\)
     \(when \(eq \(match-end 0\) \(point\)\) \(incf cnt 1\)\)\)\)\n
 active ca.\n ca.\n circa\n d. 1888\n b. 1999\n
:SEE-ALSO `naf-mode-lifespan', `naf-mode-simple-date', `naf-mode-year-range',
`naf-mode-date-string'.\n:USED-IN `naf-mode'.\n►►►")
;;
;;; :TEST-ME naf-mode-circa-dates
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (let ((cnt 0))
;; |   (dotimes (circa 5 cnt)
;; |     (search-forward-regexp naf-mode-circa-dates nil t)
;; |     (when (eq (match-end 0) (point)) (incf cnt 1))))
;; | 
;; |  active ca. 1800
;; |  ca. 1900
;; |  circa 1901
;; |  d. 1888
;; |  b. 1999
;; `----
;;
;;;(progn (makunbound 'naf-mode-circa-dates) (unintern 'naf-mode-circa-dates) )

;;; ==============================
;;; :TODO '"\\(\\(\\(Né\\)\\|\\(Mort\\)\\) le \\)"
;;;        '"\\<Né le\\>\\|\\<Mort le\\>"
;;; :NOTE Not case-sensitive.
;;; :CREATED <Timestamp: Monday June 23, 2008 @ 12:35.49 PM - by MON KEY>
(defconst naf-mode-benezit-date  "\\<Né\\>\\|\\<Née\\>\\|\\<Mort\\>"
  "*Regexp to matchthe beginning of the Benezit lifespan string in `naf-mode'.\n
:FACE-FONT-LOCKING-WITH `naf-mode-date-face'.
:FACE-DOCUMENTED-IN `naf-mode-date-fface'.
:SEE-ALSO \n:USED-IN `naf-mode'.\n►►►")
;;
;;;(progn (makunbound 'naf-mode-benezit-date) 
;;;        (unintern 'naf-mode-benezit-date) )

;;; ==============================
;;; `naf-mode-active-date'
(defvar naf-active-date-flags nil
  "*List of strings commonly used to designate when an entity was active.\n
:REGEXPS-IN `naf-mode-active-date'.
:FACE-FONT-LOCKING-WITH `naf-mode-date-face'. 
:FACE-DOCUMENTED-IN `naf-mode-date-fface'.
:SEE-ALSO .\n
:USED-IN `naf-mode'.\n►►►")
;;
(unless (bound-and-true-p naf-active-date-flags)
  (setq naf-active-date-flags
        '("Actif en"
          "Actif à"
          "actif en"
          "actif à"
          "active circa"
          "active Circa"
          "active c."
          "active ca."
          "active ca"
          "active cca."
          "Active c."
          "Active ca."
          "Active ca"
          "Active cca.")))
;;
(defconst naf-mode-active-date
  (concat "\\<" (regexp-opt naf-active-date-flags 'paren) )
  "*Regexp to match the active period string in `naf-mode'.\n
:KEYWORD-LISTS-IN `naf-active-date-flags'.
:FACE-FONT-LOCKING-WITH `naf-mode-date-face'. 
:FACE-DOCUMENTED-IN `naf-mode-date-fface'.\n
:SEE-ALSO `naf-mode-active-date-flags-paren',
`naf-mode-active-date-flags-solo'.\n►►►")
;;
;;;(progn (makunbound 'naf-mode-active-date) (unintern 'naf-mode-active-date) )
;;;        

;;; ==============================
;;; `naf-mode-active-date-flags-paren'
(defvar naf-active-date-flags-paren nil
  "*List of strings the active period string in `naf-mode'.\n
:REGEXPS-IN `naf-mode-active-date-flags-paren'
:FACE-FONT-LOCKING-WITH `naf-mode-date-face'. 
:FACE-DOCUMENTED-IN `naf-mode-date-fface'.\n
:SEE-ALSO \n
:USED-IN `naf-mode'.\n►►►")
;;
(unless (bound-and-true-p naf-active-date-flags-paren)
  (setq naf-active-date-flags-paren
        '("(active circa"
          "(active Circa"
          "(active c."
          "(active ca."
          "(active ca"
          "(active cca."
          "(Active c."
          "(Active ca."
          "(Active ca"
          "(Active cca."
          "(c"
          "(c."
          "(ca."
          "(ca"
          "(cca."
          "(ca."
          "(ca"
          "(cca.")))
;;
(defconst naf-mode-active-date-flags-paren (regexp-opt naf-active-date-flags-paren 'paren)
  "*Regexp to match the active date and periods in `naf-mode'.
:KEYWORD-LISTS-IN `naf-active-date-flags-paren'
:FACE-FONT-LOCKING-WITH `naf-mode-date-face'. 
:FACE-DOCUMENTED-IN `naf-mode-date-fface'.\n
:EXAMPLE\n
\(active circa\n\(active Circa\n\(active c.\n\(active ca.\n\(active ca
\(active cca.\n\(Active c.\n\(Active ca.\n\(Active ca\n\(Active cca.
\(c\n\(c.\n\(ca.\n\(ca\n\(cca.\n\(ca.\n\(ca\n\(cca.\n
:SEE-ALSO `naf-mode-active-date', `naf-mode-active-date-flags-solo'.\n►►►")
;;
;;;(progn (makunbound 'naf-mode-active-date-flags-paren) 
;;;       (unintern 'naf-mode-active-date-flags-paren) )

;;; ==============================
;;; :MODIFICATIONS <Timestamp: Wednesday July 29, 2009 @ 04:12.28 PM  - by MON KEY>
;;; `naf-mode-active-date-flags-solo'
(defvar naf-active-date-flags-solo nil
  "*List for matching the circa period string in `naf-mode'.
:REGEXPS-IN naf-mode-active-date-flags-solo
:FACE-FONT-LOCKING-WITH `naf-mode-date-face'
:FACE-DOCUMENTED-IN `naf-mode-date-fface'.\n
:SEE-ALSO `naf-mode-active-date-flags-paren',`naf-mode-active-date'.\n►►►")
;;
(unless (bound-and-true-p naf-active-date-flags-solo)
  (setq naf-active-date-flags-solo
        '(" c. " 
          " ca. " 
          " cca. " 
          " ca. " 
          " cca. ")))
;;
(defconst naf-mode-active-date-flags-solo 
  (regexp-opt naf-active-date-flags-solo 'paren)
  "*Regexp to match the active period string in `naf-mode'.\n
:KEYWORD-LISTS-IN `naf-active-date-flags-solo'
:FACE-FONT-LOCKING-WITH `naf-mode-date-face'.
:FACE-DOCUMENTED-IN `naf-mode-date-fface'.\n
:EXAMPLE\n
:SEE-ALSO `naf-mode-active-date-flags-paren' , `naf-mode-active-date'.\n►►►")
;;
;;;(progn (makunbound 'naf-mode-active-date-flags-solo)
;;;       (unintern 'naf-mode-active-date-flags-solo) )

;;; ==============================
(defvar *regexp-french-date-prefix* nil
  "*List of Benezit styled French birth/deatch strings.\n
:SEE-ALSO `*regexp-french-date-prefix*'.\n
:USED-IN `naf-mode'.\n►►►")
;;
(unless (bound-and-true-p *regexp-french-date-prefix*)
  (setq *regexp-french-date-prefix*
        '(("Mort en")
          ("Mort vers")
          ("Mort le")
          ("Mort à")
          ("Né en")
          ("Né le")
          ("Né à")
          ("Née en")
          ("Née à")
          ("Née le")) ;; Century
          ))

;;; ==============================
:;; :TODO Add a translation routine for this alist:
;;; :CREATED <Timestamp: #{2009-09-17T13:55:32-04:00Z}#{09384} - by MON>
;;;
;;; ("Mort en" "Died in")
;;; ("Mort vers" "Died circa")
;;; ("Mort le" "Died")
;;; ("Mort à" "Died in")
;;; ("Né en" "Born in")
;;; ("Né le" "Born")
;;; ("Né à" "Born in")
;;; ("Née en" "Born in")
;;; ("Née à" "Born in")
;;; ("Née le" "Born")

;;; ==============================
(defvar *regexp-french-date-siecle* nil
  "*List of Benezit styled French century and century range strings.\n
:NOTE List includes Roman numeral suffixed by char #x364: `\x364'.\n
:SEE-ALSO `*regexp-french-date-prefix*'.\n
:USED-IN `naf-mode'.\n►►►")
;;
(unless (bound-and-true-p *regexp-french-date-siecle*)
  (setq *regexp-french-date-siecle*
        '("xix siècle"
          "XIX siècle"
          "xix siècle"
          "xvii-xix siècles"
          "xvii siècle"
          "xviiͤ siècle"
          "XVIIͤ siècle"
          "xixͤ siècle"
          "XIXͤ siècle"
          "xixͤ siècle"
          "xviiͤ-xixͤ siècles")))

;;; ==============================
;;; :UNIMPLIMENTED
;;; :CENTURIES-AND-PERIODS
;; 20th century
;; 20th Century
;; 19th-20th century
;; 19th-20th Century
;; active late 20th century
;; active late 20th Century
;; active mid 20th century
;; active mid-20th century
;; active mid-20th Century
;; active 20th century
;; active 20th Century
;; active late 19th-early 20th centuries
;; active nnth century
;; xxe siècle . 20th Century.
;; XXe siècle . 20th Century.
;; XIXe-XXe siècles . 19th-20th Century.
;; xixe-xxe siècles
;;; twentieth nineteenth eighteenth

;;; ==============================
;;; :UNIMPLIMENTED
;;; :CENTURYS-AND-PERIODS
;;; Actif aussi
;;; Actif entre
;;; 20th century
;;; 20th Century
;;; 19th-20th century
;;; 19th-20th Century
;;; active late 20th century
;;; active late 20th Century
;;; active mid 20th century
;;; active mid-20th century
;;; active mid-20th Century
;;; active 20th century
;;; active 20th Century
;;; active late 19th-early 20th centuries
;;; active nnth century
;;; xxe siècle . 20th Century  
;;; XXe siècle . 20th Century  
;;; XIXe-XXe siècles . 19th-20th Century 
;;; xixe-xxe siècles
;;; twentieth nineteenth eighteenth

;;; ==============================
;;; :TODO Finish the Benezit siecle flags.
;;; :CREATED <Timestamp: #{2009-09-15T13:03:06-04:00Z}#{09382} - by MON>
;;(defvar *regexp-french-date-siecle*

;; ;; :DOWNCASE-NO-SUPERSCRIPT
;; xv
;; xvi
;; xvii
;; xviii
;; xix
;; xx

;; ;; :UPCASE-NO-SUPERSCRIPT
;; XV
;; XVI
;; XVII
;; XVIII
;; XIX
;; XX

;; ;; :DOWNCASE-W-COMBINING-LATIN-SMALL-LETTER-E ;; code point: 0x0364: ͤ
;; xvͤ
;; xviͤ
;; xviiͤ
;; xviiiͤ
;; xixͤ
;; xxͤ

;; ;; :UPCASE-W-COMBINING-LATIN-SMALL-LETTER-E ;; code point: 0x0364
;; XVͤ
;; XVIͤ
;; XVIIͤ
;; XVIIIͤ
;; XIXͤ
;; XXͤ

;; ;; :DOWNCASE-W-COMBINING-LATIN-SMALL-LETTER-E ;; code point: 0x0364
;; xve
;; xvie
;; xviie
;; xviiie
;; xixe
;; xxe

;; ;; :UPCASE-W-COMBINING-LATIN-SMALL-LETTER-E ;; code point: 0x0364
;; XVe
;; XVIe
;; XVIIe
;; XVIIIe
;; XIXe
;; XXe

;; ;; :DOWNCASE-NO-SUPERSCRIPT-W-SIÈCLE-DIACRITIC ;; 'è'
;; xv siècle
;; xvi siècle
;; xvii siècle
;; xviii siècle
;; xix siècle
;; xx siècle

;; ;; :UPCASE-NO-SUPERSCRIPT-W-SIÈCLE-DIACRITIC ;; 'è'
;; XV siècle
;; XVI siècle
;; XVII siècle
;; XVIII siècle
;; XIX siècle
;; XX siècle

;; ;; :DOWNCASE-NO-SUPERSCRIPT-W-SIECLE ;; ASCII `e' no diacritic
;; xv siecle
;; xvi siecle
;; xvii siecle
;; xviii siecle
;; xix siecle
;; xx siecle

;; ;; :UPCASE-NO-SUPERSCRIPT-W-SIECLE ;;ASCII `e' no diacritic
;; XV siecle
;; XVI siecle
;; XVII siecle
;; XVIII siecle
;; XIX siecle
;; XX siecle

;; ;; :DOWNCASE-W-COMBINING-LATIN-SMALL-LETTER-E ;; code point: 0x0364: ͤ w/ siècle diacrital 'è'
;; xvͤ siècle
;; xviͤ siècle
;; xviiͤ siècle
;; xviiiͤ siècle
;; xixͤ siècle
;; xxͤ siècle

;; ;; :UPCASE-W-COMBINING-LATIN-SMALL-LETTER-E  ;; code point: 0x0364: ͤ w/ siècle diacrital 'è'
;; XVͤ siècle
;; XVIͤ siècle
;; XVIIͤ siècle
;; XVIIIͤ siècle
;; XIXͤ siècle
;; XXͤ siècle

;; ;; :DOWNCASE-W-SUPERSCIRPT-IS-ASCII-E-NO-DIACRITIC-W-SIECLE ;; ASCII `e' no diacritic
;; xve siecle
;; xvie siecle
;; xviie siecle
;; xviiie siecle
;; xixe siecle
;; xxe siecle

;; ;;; :UPCASE-W-DOWNCASE--W-SUPERSCIRPT-IS-ASCII-E-NO-DIACRITIC-W-SIECLE ;; ASCII `e' no diacritic
;; XVe siecle
;; XVIe siecle
;; XVIIe siecle
;; XVIIIe siecle
;; XIXe siecle
;; XXe siecle

;; ;; :DOWNCASE-W-SUPERSCIRPT-IS-ASCII-E-NO-DIACRITIC-W-SIÈCLE-DIACRITIC-E
;; xve siècle
;; xvie siècle
;; xviie siècle
;; xviiie siècle
;; xixe siècle
;; xxe siècle

;; ;; :UPCASE-W-SUPERSCIRPT-IS-ASCII-E-NO-DIACRITIC-W-SIÈCLE-DIACRITIC-E
;; XVe siècle
;; XVIe siècle
;; XVIIe siècle
;; XVIIIe siècle
;; XIXe siècle
;; XXe siècle

;; ;; :DOWNCASE-W-SUPERSCIRPT-IS-ASCII-E-NO-DIACRITIC-W-SIECLE ;; ASCII `e' no diacritic
;; xve siecle
;; xvie siecle
;; xviie siecle
;; xviiie siecle
;; xixe siecle
;; xxe siecle

;; ;; :MIXED-CASE-W-SUPERSCIRPT-IS-ASCII-E-NO-DIACRITIC-W-SIECLE ;; ASCII `e' no diacritic
;; XVe siecle
;; XVIe siecle
;; XVIIe siecle
;; XVIIIe siecle
;; XIXe siecle
;; XXe siecle

;; ;; :RANGES:
;; ;;; w/ siècles | siecles
;; ;;; w/ e | ͤ

;; ;;; *******
;; xv-xvi     xv-XVI     XV-xvi     XV-XVI	   
;; xvi-xvii   xvi-XVII   XVI-xvii   XVI-XVII   
;; xvii-xviii xvii-XVIII XVII-xviii XVII-XVIII 
;; xviii-xix  xviii-XIX  XVIII-xix  XVIII-XIX  
;; xix-xx	   xix-XX     XIX-xx     XIX-XX     

;; ;;; *******
;; xv-xvi     xv-XVI     XV-xvi     XV-XVI	   
;; xvi-xvii   xvi-XVII   XVI-xvii   XVI-XVII   
;; xvii-xviii xvii-XVIII XVII-xviii XVII-XVIII 
;; xviii-xix  xviii-XIX  XVIII-xix  XVIII-XIX  
;; xix-xx	   xix-XX     XIX-xx     XIX-XX     

;; ;;; *******
;; xv-xvi siecles       xv-xvi siècles      	  
;; xvi-xvii siecles     xvi-xvii siècles    
;; xvii-xviii siecles   xvii-xviii siècles  
;; xviii-xix siecles    xviii-xix siècles   
;; xix-xx siecles       xix-xx  siècles	    

;; ;;; *******
;; XV-XVI siecles       XV-XVI siècles      	   
;; XVI-XVII siecles     XVI-XVII siècles    
;; XVII-XVIII siecles   XVII-XVIII siècles  
;; XVIII-XIX siecles    XVIII-XIX siècles   
;; XIX-XX siecles       XIX-XX siècles	    

;; ;;; *******
;; xvͤ-xviͤ siecles     xvͤ-xviͤ siècles     	   
;; xviͤ-xviiͤ siecles   xviͤ-xviiͤ siècles   
;; xviiͤ-xviiiͤ siecles xviiͤ-xviiiͤ siècles 
;; xviiiͤ-xixͤ siecles  xviiiͤ-xixͤ siècles  
;; xixͤ-xxͤ siecles     xixͤ-xxͤ siècles     

;; ;;; *******
;; XVͤ-XVIͤ siecles     XVͤ-XVIͤ siècles     	   
;; XVIͤ-XVIIͤ siecles   XVIͤ-XVIIͤ siècles    
;; XVIIͤ-XVIIIͤ siecles XVIIͤ-XVIIIͤ siècles  
;; XVIIIͤ-XIXͤ siecles  XVIIIͤ-XIXͤ siècles   
;; XIXͤ-XXͤ siecles     XIXͤ-XXͤ siècles     

;; ;;; *******
;; xve-xvie siecles     xve-xvie siècles     
;; xvie-xviie siecles   xvie-xviie siècles   
;; xviie-xviiie siecles xviie-xviiie siècles 
;; xviiie-xixe siecles  xviiie-xixe siècles  
;; xixe-xxe siecles     xixe-xxe siècles     

;; ;;; *******
;; XVe-XVIe siecles     XVe-XVIe siècles      
;; XVIe-XVIIe siecles   XVIe-XVIIe siècles    
;; XVIIe-XVIIIe siecles XVIIe-XVIIIe siècles  
;; XVIIIe-XIXe siecles  XVIIIe-XIXe siècles   
;; XIXe-XXe siecles     XIXe-XXe siècles

;;; ==============================
(require 'naf-mode-dates)
;;; ==============================

;;; ==============================
;;; (provide 'naf-mode-dates)
;;; ==============================

;;; =====================================================================
;;; naf-mode-dates.el ends here
;;; EOF


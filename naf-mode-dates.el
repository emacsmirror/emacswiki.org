;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-dates.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-dates
;;;
;;; FUNCTIONS:
;;;
;;; CONSTANTS:
;;; `naf-weekday-alist', `naf-month-abbrev-alist', `naf-mode-simple-date',
;;; `naf-mode-circa-dates', `naf-mode-year-range', `naf-mode-french-dates',
;;; `naf-mode-french-days', `naf-mode-english-days', `naf-mode-date-string',
;;; `naf-mode-lifespan', `naf-mode-english-dates',
;;;
;;; VARIABLES:
;;;
;;; MACROS:
;;;
;;; SUBST or ALIASES:
;;;
;;; DEPRECATED, RENAMED, OR MOVED:
;;;
;;; REQUIRES:
;;;
;;; TODO:
;;;
;;; NOTES:
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-dates.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-11-21T20:45:38-05:00Z}#{09477} - by MON>
;;;
;;; FILE-CREATED:
;;; HEADER-ADDED: <Timestamp: #{2009-08-22T12:24:17-04:00Z}#{09346} - by MON KEY>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; Permission is granted to copy, distribute and/or modify this
;;; document under the terms of the GNU Free Documentation License,
;;; Version 1.3 or any later version published by the Free Software
;;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;;; and no Back-Cover Texts. A copy of the license is included in
;;; the section entitled "GNU Free Documentation License".
;;; A copy of the license is also available from the Free Software
;;; Foundation Web site at:
;;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ================================================================
;;; Copyright © 2009 MON KEY 
;;; ==============================
;;; CODE:

;;; ==============================
(provide 'naf-mode-dates)
;;; ==============================

;;; ==============================
(defconst naf-weekday-alist
  '(("Sunday" . 0)("Sun" . 0) ("Sun." . 0) ("Sund." 0) ("Dimanche" . 0)  ("dimanche" . 0)
    ("Monday" . 1) ("monday" . 1)  ("Mon" . 1) ("Mon." . 1) ("Lundi" . 1)  ("lundi" . 1)
    ("Tuesday" . 2) ("tuesday" . 2)  ("Tue" . 2) ("Tue." . 2) ("Tues" . 2) ("Mardi" . 2)  ("mardi" . 2)
    ("Wednesday" . 3) ("wednesday" . 3) ("Wed" . 3) ("Wed." . 3) ("Wedn." . 3) ("Mercredi" . 3)  ("mercredi" . 3)
    ("Thursday" . 4) ("thursday" . 4)  ("Thu" . 4) ("Thu." . 4) ("Thurs" . 4) ("thurs." . 4) ("Jeudi" . 4)  ("jeudi" . 4)
    ("Friday" . 5) ("friday" . 5) ("Fri" . 5) ("Fri." . 5)   ("fri." . 5) ("Vendredi" . 5)  ("vendredi" . 5)
    ("Saturday" . 6) ("saturday" . 6) ("Sat" . 6) ("Sat." . 6) ("Samedi" . 6)  ("samedi" . 6))
  "Alist mapping string representation of weekdays to their indexed location.
Included abbrevieated and downcased forms. Index is 0 based beginning with Sunday.
See also; `naf-mode-french-days', `naf-mode-english-days', `naf-month-abbrev-alist'.
Used in `naf-mode'.")

;;;test-me; naf-weekday-alist
;;;(progn (makunbound 'naf-weekday-alist) (unintern 'naf-weekday-alist))

;;; ==============================
(defconst naf-month-abbrev-alist 
  '(("January" . 1) ("january" . 1) ("Janvier" . 1) ("janvier" . 1) 
    ("Jan" . 1) ("Jan." . 1) ("jan" . 1) ("jan." . 1) 
    ("February" . 2) ("february" . 2) ("Février" . 2) ("février" . 2) ("Fevrier" . 2) ("fevrier" . 2)
    ("Feb" . 2) ("Feb." . 2)  ("feb" . 2) ("feb." . 2) 
    ("March" . 3) ("march" . 3) ("Mars" . 3) ("mars" . 3) 
    ("Mar" . 3) ("Mar." . 3)  ("mar" . 3) ("mar." . 3) 
    ("April" . 4) ("april" . 4) ("Avril" . 4) ("avril" . 4) 
    ("Apr" . 4) ("Apr." . 4) ("apr" . 4) ("apr." . 4) 
    ("May" . 5) ("may" . 5) ("Mai" . 5) ("mai" . 5)
    ("June" . 6) ("june" . 6) ("Juin" . 6) ("juin" . 6) 
    ("Jun" . 6) ("Jun." . 6)  ("jun" . 6) ("jun." . 6)
    ("July" . 7) ("july" . 7) ("Juillet" . 7) ("juillet" . 7) 
    ("Jul" . 7) ("Jul." . 7)  ("jul" . 7) ("jul." . 7)
    ("August" . 8) ("august" . 8) ("Août" . 8) ("Aout" . 8) ("août" . 8) ("aout" . 8) 
    ("Aug" . 8) ("Aug." . 8) ("aug" . 8) ("aug." . 8)
    ("September" . 9) ("september" . 9) ("Septembre" . 9) ("septembre" . 9) 
    ("Sep" . 9) ("Sep." . 9) ("Sept" . 9) ("sep" . 9) ("sep." . 9) ("sept" . 9)
    ("October" . 10) ("october" . 10) ("Octobre" . 10) ("octobre" . 11) 
    ("Oct" . 10) ("Oct." . 10) ("oct" . 10) ("oct." . 10)
    ("November" . 11) ("november" . 11) ("Novembre" . 11) ("novembre" . 11) 
    ("Nov" . 11) ("Nov." . 11) ("nov" . 11) ("nov." . 11)
    ("December" . 12) ("december" . 12) ("Décembre" . 12) ("décembre" . 12) 
    ("Dec" . 12) ("Dec." . 12)  ("dec" . 12) ("dec." . 12))
  "Alist of English Months mappe to their indexed calendar location.
Included abbrevieated and downcased forms. Index is 1 based beginning with
January.\n
See also; `naf-mode-french-dates', `naf-weekday-alist'.\nUsed in `naf-mode'.")

;;;test-me; naf-month-abbrev-alist
;;;(progn (makunbound 'naf-month-abbrev-alist) (unintern 'naf-month-abbrev-alist))

;;; ==============================
(defconst naf-mode-english-dates
  '"\\(A\\(pr\\(\\.\\|il\\)\\|ug\\(\\.\\|ust\\)\\)\\|Dec\\(\\.\\|ember\\)\\|Feb\\(\\.\\|ruary\\)\\|J\\(an\\(\\.\\|uary\\)\\|u\\(l[.y]\\|n[.e]\\)\\)\\|Ma\\(r\\(\\.\\|ch\\)\\|y\\)\\|Nov\\(\\.\\|ember\\)\\|Oct\\(\\.\\|ober\\)\\|Sep\\(\\.\\|t\\(\\.\\|ember\\)\\)\\) \\([0-3]?+[0-9]\\)\\(:?[rd\|th\|st\|,]+\\) \\<[0-9]\\{4\\}\\>"
  "Manually generated regexp for font-locking English datestrings.\n
EXAMPLE:\n
\"Jan.\" \"January\"\n\"Feb.\" \"February\"\n\"Mar.\" \"March\"
\"Apr.\" \"April\"\n\"May\"\n\"Jun.\" \"June\"\n\"Jul.\" \"July\"
\"Aug.\" \"August\"\n\"Sep.\" \"Sept.\" \"September\"
\"Oct.\" \"October\"\n\"Nov.\" \"November\"\n\"Dec.\" \"December\"\n
See also; `naf-mode-french-dates', `naf-month-abbrev-alist', `naf-mode-english-days'.
Used in `naf-mode'.")
;;
;;; ENGLISH MONTHS:
;;; "\\(A\\(pr\\(\\.\\|il\\)\\|ug\\(\\.\\|ust\\)\\)\\|Dec\\(\\.\\|ember\\)\\|Feb\\(\\.\\|ruary\\)\\|J\\(an\\(\\.\\|uary\\)\\|u\\(l[.y]\\|n[.e]\\)\\)\\|Ma\\(r\\(\\.\\|ch\\)\\|y\\)\\|Nov\\(\\.\\|ember\\)\\|Oct\\(\\.\\|ober\\)\\|Sep\\(\\.\\|t\\(\\.\\|ember\\)\\)\\)"

;;;test-me; naf-mode-english-dates
;;;(progn (makunbound 'naf-mode-english-dates) (unintern 'naf-mode-english-dates))

;;; ==============================
(defconst naf-mode-english-days
  '"\\<[MTWFS]\\(on\\|ues\\|ednes\\|hurs\\|ri\\|atur\\|un\\)\\(day\\)\\>"
  "Manually generated regexp for font-locking full spelling of English days of week.
EXAMPLE:\nMonday Tuesday Wednesday Thursday Friday Saturday Sunday
See also; `naf-mode-french-days', `naf-weekday-alist', `naf-mode-english-dates'.
\nUsed in `naf-mode'.")
;;
;;;ENGLISH WEEKDAYS:
;;; "\\<\\(Friday\\|Monday\\|S\\(aturday\\|unday\\)\\|T\\(hursday\\|uesday\\)\\|Wednesday\\)\\>"

;;;test-me; naf-mode-english-days
;;;(progn (makunbound 'naf-mode-english-days) (unintern 'naf-mode-english-days))


;;; ==============================
(defconst naf-mode-french-dates
  '"\\<[0-9]\\{1,2\\}\\> \\(A\\(o\\(ut\\|ût\\)\\|vr\\(\\.\\|il\\)\\)\\|Déc\\(\\.\\|embre\\)\\|Fév\\(\\.\\|rier\\)\\|J\\(an\\(\\.\\|vier\\)\\|ui\\(l\\(\\.\\|let\\)\\|n\\)\\)\\|Ma\\(i\\|rs\\)\\|Novembre\\|Octobre\\|Sep\\(\\.\\|tembre\\)\\|a\\(o\\(ut\\(\\)?\\|ût\\(\\)?\\)\\|vr\\(\\.\\|il\\)\\)\\|déc\\(\\.\\|embre\\)\\|fév\\(\\.\\|rier\\)\\|j\\(an\\(\\.\\|vier\\)\\|ui\\(l\\(\\.\\|let\\)\\|[n]\\)\\)\\|ma\\(rs\\(\\)?\\|[i]\\)\\|nov\\(\\.\\|embre\\)\\|oct\\(\\.\\|obre\\)\\|sep\\(\\.\\|tembre\\)\\) \\<[0-9]\\{4\\}\\>"
  "Manually generated regexp for French style dates of form NN Mmmm.\n
EXAMPLE:
\"Janvier\" \"Février\" \"Mars\" \"Avril\" \"Mai\" \"Juin\"
\"Juillet\" \"Août\" \"Septembre\" \"Octobre\" \"Novembre\" \"Décembre\"
See also; `naf-mode-english-dates', `naf-month-abbrev-alist', 
`naf-mode-french-days'.\nUsed in `naf-mode'.")
;;
;;; FRENCH MONTHS:
;;; "[A-Za-z]\\(\\(\\(oût\\|vril\\|ai\\|ars\\)\\)\\|\\(\\(anv\\|évr\\)\\(ier\\)\\)\\|\\(\\(cto\\|epte\\|ove\\|éce\\)\\(m?+bre\\)\\)\\|\\(\\(ui\\)\\(n\\|l+et\\)\\)\\)"

;;;test-me; naf-mode-french-dates
;;;(progn (makunbound 'naf-mode-french-dates) (unintern 'naf-mode-french-dates))

;;; ==============================
(defconst naf-mode-french-days
  '"\\<\\([JLMSV]\\(ar\\|ame\\|endre\\|ercre\\|eu\\|un\\)\\(di\\)\\)\\|\\(Dimanche\\)\\>"
  "Manually generated regexp for French days of week.\n
EXAMPLE:\"Dimanche\" \"Lundi\" \"Mardi\" \"Mercredi\" \"Jeudi\" \"Vendredi\" \"Samedi\"
See also; `naf-mode-english-days', `naf-weekday-alist', `naf-mode-french-dates'.
Used in `naf-mode'.")
;;
;;;FRENCH WEEkDAYS:
;;;"\\<\\(Dimanche\\|Jeudi\\|Lundi\\|M\\(ardi\\|ercredi\\)\\|Samedi\\|Vendredi\\)\\>")

;;;test-me; naf-mode-french-days
;;;(progn (makunbound 'naf-mode-french-days) (unintern 'naf-mode-french-days))

;;; ==============================
(defconst naf-mode-simple-date '"\\_<[0-9]\\{4\\}\\_>"
  "Manually generated regexp simple year datestrings.\n
EXAMPLE: ' 1999 '\n
See also; `naf-mode-lifespan',`naf-mode-year-range', `naf-mode-date-string'.
Used in `naf-mode'.")
;;
;;; \_<[0-9]\{4\}\_>

;;;test-me; naf-mode-simple-date
;;;(progn (makunbound 'naf-mode-simple-date) (unintern 'naf-mode-simple-date))

;;; ==============================
(defconst naf-mode-year-range
  '"\\(\\((\\)\\([0-9]\\{4\\}?\\)\\(-?\\)\\([0-9]\\{4\\}?\\)\\()\\)\\)"
  "Manually generated regexp for date-strings nested inside parens.
EXAMPLE:\n\(1899-\) \(-1999\) \(1899\) \(1899-1999\)\n
See also; `naf-mode-simple-date', `naf-mode-date-string'.\nUsed in `naf-mode'.")
;;
;;; NOTE: Following regex works - don't alter - make copies!
;;; \(\((\)\([0-9]\{4\}?\)\(-?\)\([0-9]\{4\}?\)\()\)\)

;;;test-me; naf-mode-year-range
;;;(progn (makunbound 'naf-mode-year-range) (unintern 'naf-mode-year-range))

;;; ==============================
(defconst naf-mode-date-string  '"\\([0-9]\\{2,4\\}\\(-\\|/\\)[0-9]?+\\(-\\|/\\)[0-9]\\{2,4\\}\\)"
   "Manually generated regexp for font-locking '-' and '/' delimited datestrings.
EXAMPLE:\n12-3-1975 12-03-1975 1975/12/03 1975-12-03.\n
See also; `naf-mode-simple-date', `naf-mode-year-range', `naf-mode-lifespan',
`naf-mode-circa-dates'.\nUsed in `naf-mode'.")
;;
;;\\([0-9]\\{2,4\\}\\(-\\|/\\)[0-9]?+\\(-\\|/\\)[0-9]\\{2,4\\}\\)"

;;;test-me; naf-mode-date-string
;;;(progn (makunbound 'naf-mode-date-string) (unintern 'naf-mode-date-string))


;;; ==============================
(defconst naf-mode-lifespan
  '"\\([(]?+[0-9]\\{4\\}-[0-9]\\{4\\}[)]?+\\)"
  "Manually generated regexp for font-locking lifespans formatted as (1900-1899).\n
See also; `naf-mode-circa-dates', `naf-mode-simple-date', `naf-mode-year-range',
`naf-mode-date-string'.\nUsed in `naf-mode'.")
;;
;;; LIFESPAN WITH WORD BOUNDARIES:
;;;   '"\\(\\<[(]?+[0-9]\\{4\\}-[0-9]\\{4\\}[)]?+\\>\\)" )
;;
;;; LIFESPAN WITHOUT WORD BOUNDARIES:
;;;  '"\\([(]?+[0-9]\\{4\\}-[0-9]\\{4\\}[)]?+\\)" )

;;;test-me; naf-mode-lifespan
;;;(progn (makunbound 'naf-mode-lifespan) (unintern 'naf-mode-lifespan))

;;; ==============================
(defconst naf-mode-circa-dates
 '"\\<\\(active ca\\.\\|b\\.\\|c\\(a\\.\\|irca\\)\\|d\\.\\) [0-9]\\{4\\}\\>"
"Manually generated regexp for circa datestrings.\n
EXAMPLE:\nactive ca.\nca.\ncirca\nd. 1888\nb. 1999\n
See also; `naf-mode-lifespan', `naf-mode-simple-date', `naf-mode-year-range',
`naf-mode-date-string'.\nUsed in `naf-mode'.")
;;
;;; \\<\\(active ca\\.\\|b\\.\\|c\\(a\\.\\|irca\\)\\|d\\.\\) [0-9]\\{4\\}\\>

;;;test-me; naf-mode-circa-dates
;;;(progn (makunbound 'naf-mode-circa-dates) (unintern 'naf-mode-circa-dates))

;;; ==============================
;;; UNIMPLIMENTED:
;;; CENTURYS AND PERIODS:
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
(require 'naf-mode-dates)
;;; ==============================

;;; ==============================
;;; (provide 'naf-mode-dates)
;;; ==============================

;;; =====================================================================
;;; naf-mode-dates.el ends here
;;; EOF

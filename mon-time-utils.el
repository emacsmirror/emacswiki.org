;;; mon-time-utils.el --- utilities for working with time.
;; -*- mode: EMACS-LISP; coding: utf-8 -*-

;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-time-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-07-29T19:46:454-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, calendar, convenience, data

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION: 
;; MON utilities for working with time. In particular Routines for converting
;; from 'eBay official time' to 'YOUR Official Time®'
;;
;; FUNCTIONS:►►►
;; `mon-get-current-year', `mon-format-iso-8601-time',
;; `mon-file-older-than-file-p', `mon-get-file-mod-times',
;; `mon-conv-time-flt-pnt', `mon-comp-times-flt-pnt',
;; `mon-accessed-time-stamp', `mon-stamp', `mon-accessed-stamp',
;; `mon-lisp-stamp', `mon-convert-ebay-time', `mon-convert-ebay-time-mvb',
;; `mon-cln-ebay-time-string', `mon-calculate-ebay-timezone-diff',
;; `mon-date-stamp', `mon-file-stamp-vrfy-put-eof', `mon-comment-divider-w-len',
;; `mon-file-stamp', `mon-file-stamp-buffer-filename-TEST',
;; `mon-file-stamp-minibuffer', `mon-file-stamp-buffer-filename',
;; `calendar-goto-doomsday', `mon-stamp-in-context'
;;
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS
;;
;; CLASSES:
;; 
;; CONSTANTS:
;;
;; VARIABLES:
;; `*mon-default-comment-divider*', 
;; `*mon-timestamp-cond*' 
;;
;; ALIASES/ADVISED/SUBST'D:
;; `mon-stamp-date-only' -> `mon-date-stamp'
;; `mon-today-stamp'     -> `mon-date-stamp'
;;
;; MOVED:
;; `mon-comp-times-flt-pnt'      <- mon-dir-utils.el
;; `mon-conv-time-flt-pnt'       <- mon-dir-utils.el
;; `mon-file-older-than-file-p'  <- mon-dir-utils.el
;; `mon-get-file-mod-times'      <- mon-dir-utils.el
;; `mon-accessed-stamp'          <- mon-insertion-utils.el
;; `mon-stamp'                   <- mon-insertion-utils.el
;; `mon-timestamp'               <- mon-insertion-utils.el
;; `mon-accessed-time-stamp'     <- mon-insertion-utils.el
;; `*mon-timestamp-cond*'        <- mon-dir-locals-alist.el
;; `*mon-default-comment-start*' -> mon-utils.el
;;
;; DEPRECATED:
;; `mon-accessed-stamp' -> 
;;
;; RENAMED:
;;
;; REQUIRES:
;; :FILE mon-regexp-symbols.el
;;  |-> `*regexp-clean-ebay-month->canonical-style1*'
;;  |-> `*regexp-clean-ebay-month->canonical-style2*'
;;  |-> `*regexp-clean-ebay-month->canonical-style3*'
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-regexp-symbols.el')
;;
;; `mon-convert-ebay-time-mvb' cl.el
;;                             |-> `multiple-value-bind' 
;;
;; `mon-date-stamp' -> :FILE mon-utils.el 
;;                 |-> `mon-string-to-symbol'
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-utils.el')
;; 
;; `mon-timestamp' -> :FILE mon-dir-utils.el
;;                     |-> `mon-get-buffer-parent-dir'
;;                     |-> `mon-buffer-written-p'
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-dir-utils.el')
;;                                     
;; :FILE mon-default-loads.el
;;  |-> `IS-MON-P', `IS-MON-P-W32', `IS-MON-P-GNU' 
;;  |-> `IS-BUG-P',`IS-BUG-P-REMOTE'
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-default-loads.el')
;;
;; The value of the above vars and const are returned in lieu of:
;; :FILE mon-site-local-defaults.el
;;  |-> `mon-user-name-conditionals' 
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-site-local-defaults.el')
;
;; :FILE parse-time.el ../lisp/calendar/parse-time.el (locate-library "parse-time")
;; `mon-convert-ebay-time'            -> `parse-time-string' 
;; `mon-convert-ebay-time-mvb'        -> `parse-time-string' 
;; `mon-calculate-ebay-timezone-diff' -> `parse-time-string' 
;;
;; That package also provides the constants: `*MON-NAME*', `*BUG-NAME*'
;; These hold alists of system conditional key value pairs.
;; These will need to be present on your system for this pkg to work properly.
;; The links to the files above provide filler slots that you can modify to 
;; fit your environment. I provide these as a solution for dealing with
;; defcustom silliness...
;; 
;; TODO:
;; Consider providing an optional arg `mon-cln-ebay-time-string'
;; to return the stamp without the call to `mon-convert-ebay-time'
;;
;; NOTES:
;; Originally motivated by a distaste for Ebay's promotion of EbayTime as
;; `official' time - Official my ass! 
;; :SEE (URL `http://viv.ebay.com/ws/eBayISAPI.dll?EbayTime')
;;
;; There are at least three styles of ebay timestamp. 
;; Apparently each is 'official'... 
;; 
;; Clean up ebay timestrings three different ways. 
;; * Style1 from eBay webpage i.e. copy/paste
;; * Style2 from eBay listing manager
;; * Style3 from eBay post listing email confirmations
;;
;; None of these 3(three) styles can be processed with 'parse-time-string'
;; without some cleaning. 
;;
;; The function `mon-cln-ebay-time-string' returns an extended timestamp 
;; formatted to allow further processing according to ISO 8601 spec. 
;;
;; The three ebay timestring styles are cleaned as follows:
;;
;; (Aug 07, 200913:52:24 PDT) <-style1
;; <Timestamp: #{2009-08-07T16:52:24-04:00Z}#{09325} - by Ebay>
;;
;; Jul-29 11:05                 <-style2
;; -> <Timestamp: #{2009-07-29T14:05:00-04:00Z}#{09313} - by Ebay>
;;
;; Aug-10-09 09:16:14 PDT       <-style3
;; -> <Timestamp: #{2009-08-06T21:17:10-04:00Z}#{09325} - by Ebay>
;;
;; If you don't like the default timestamp adjust 
;; format-time-string in tail of `mon-convert-ebay-time'
;;
;; The form: `(put-tz (if (nth 7 (decode-time)) "PDT" "PST"))'
;; in `mon-cln-ebay-time-string' assumes your Daylight-savings flag 
;; is in synch with the Northern Hemisphere. 
;; If it is Summer in Califonria and Winter where you are you may want to
;; swap PDT <-> PST
;;
;; ==============================
;; The time-stamps generated w/ functions herein have the form:
;;
;; <Timestamp: #{YYYY-MM-DDTHH:MM:SS+/-HH:MMZ}#{yyWwD} - by NAME>
;; <Timestamp: #{2009-08-06T21:17:10-04:00Z}#{09325} - by Ebay>
;;
;; This format is not in keeping with RFC-3339 take on ISO-8601 
;; Specifically, it disregards the discussion presented in 
;; Section 5.4. Redundant Information:
;;
;;   "If a date/time format includes redundant information, that introduces
;;    the possibility that the redundant information will not correlate.
;;    For example, including the day of the week in a date/time format
;;    introduces the possibility that the day of week is incorrect but the
;;    date is correct, or vice versa.  Since it is not difficult to compute
;;    the day of week from a date (see Appendix B), the day of week should
;;    not be included in a date/time format."
;;
;; We do not share this position. While it may be true that redundancies can
;; present errors whereby a YYYY-Www-D date calculation doesn't jibe with the
;; YYYY-MM-DD it is incorrect to assume then that we are somehow better off
;; omitting the Www-D data. This is bogus reasoning as it assumes that the
;; YYYY-MM-DD date stamp is of itself somehow MORE correct than a YYYY-Www-D
;; stamp. It is not. Rather than assume that one or another of these data will
;; become compromised it is safer to assume either that both are potentially
;; compromised or that both are correct. In either case provision of this
;; 'redundancy' allows for a check against what would otherwise be
;; uncorroborated time. Moreover, so long as the information is presented
;; according to the standards specification it is quite legitimate to simply
;; omit the redundancy via heuristics (regexps for example). Doing so allows us
;; the ability to calculate multiple different types of date intervals in an
;; adhoc 'loosey' manner whilst retaining the ability to perform more targeted
;; calculations where necessary. This can be esp. important/useful for
;; indicating and reasoning about time/date ranges by field as opposed by
;; duration. E.g. "Show me all date stamps on a Tuesday." whether the date is
;; _correct_ may be completely irrelevant to the query and it is certainly the
;; case that the heuristics/calculations needed for locating such stamps under
;; our regime will be quite a bit quicker and more intelligible than those
;; which would require numerous encode->decode->calculate->encode operations
;; just to find what may or may not be a Tuesday... 
;;
;; SNIPPETS:
;; Other date & time related functions:
;; `mon-num-to-month'             -> mon-replacement-utils.el
;; `mon-abr-to-month'             -> mon-replacement-utils.el
;; `mon-month-to-num'             -> mon-replacement-utils.el
;; `mon-num-to-month-whitespace'  -> mon-replacement-utils.el
;; `mon-ital-date-to-eng'         -> mon-replacement-utils.el
;; `mon-defranc-dates'            -> mon-replacement-utils.el
;;
;; For maintaining timestamp consistency with your shell put these in your ~/.bashrc
;; ,----
;; | alias ls="ls --time-style=long-iso"
;; | alias ls-iso="ls -l --color=auto --human-readable --time-style=+%Y-%m-%dT%H:%M:%S%:zZ"
;; | alias date-iso="date '+%FT%T%:zZ'"
;; | alias mon-stamp="date '+<Timestamp: #{%FT%T%:zZ}#{%y%V%u} - by MON>'" ;; - by `id -un'>
;; `----
;;
;; THIRD PARTY CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-time-utils.el
;; FILE-PUBLISHED: <Timestamp: #{2009-08-06} - by MON KEY>
;;
;; FILE-CREATED:
;; <Timestamp: #{2009-07-29T19:46:454-04:00Z} MON KEY>
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
;; Copyright © 2009, 2010 MON KEY 
;;; ==============================

;;; CODE:

;;; `mon-convert-ebay-time-mvb'        <- `multiple-value-bind'
;;; `mon-convert-ebay-time'            <- `parse-time-string' 
;;; `mon-convert-ebay-time-mvb'        <- `parse-time-string' 
;;; `mon-calculate-ebay-timezone-diff' <- `parse-time-string' 
(eval-when-compile 
  (require 'cl)
  (require 'parse-time)
  ) ;; (require 'mon-cl-compat nil t)  

;; :NOTE Using `edmacro-subseq' now which is a line for line duplicate of CL
;;       packages `subseq'.  Apparently Emacs keyboard macro editor interface
;;       can't even get by without CL features.  
;;       THE CL RUNTIME BAN ON NON-KEYWORD FNCNS IS A STUPID FUCKING POLICY!!!
;;
;; (declare-function cl::subseq "mon-cl-compat" t t) ;; <- `mon-cln-ebay-time-string'
;;
;; (eval-when-compile 
;;   (unless (featurep 'mon-cl-compat)
;;     (with-no-warnings (fset 'cl::subseq  'subseq))))

(require 'mon-regexp-symbols)
;;
(eval-when (compile load)
  (unless (featurep 'mon-utils)
    (require 'mon-dir-utils)
    (eval-after-load "mon-time-utils"
      '(message 
        (concat 
         ":PACKAGE `mon-time-utils' "
         "-- :REQUIRES :FUNCTION `mon-string-to-symbol' in :PACKAGE `mon-utils' "
         "which :REQUIRES :FUNCTION `mon-get-buffer-parent-dir' and "
         ":FUNCTION `mon-buffer-written-p' in :PACKAGE `mon-dir-utils'\n"
         "were :PACKAGE `mon-utils' provided :PACKAGE `mon-dir-utils' "
         "would satisfy predicate `featurep', instead it was required by "
         ":PACKAGE `mon-time-utils' at compile/load time")))))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-17T15:49:27-05:00Z}#{10073} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-10-24T12:51:14-04:00Z}#{09436} - by MON KEY>
(defun mon-comment-divider-w-len (divider-length 
                                  &optional alt-comment-start alt-divide-char)
  "Return a mon-comment-divider with DIVIDER-LENGTH.\n
DIVIDER-LENGTH is an integer which specifies the number of divider chars with
which suffix `*mon-default-comment-start*' which is the default comment-start
value if when optional ALT-COMMENT-START is ommitted.\n
When DIVIDER-LENGTH is >= 80 return value will be 80 chars long - no more.\n
If ALT-COMMENT-START is non-nil use that string as the comment divider prefix.\n
ALT-COMMENT-START is a three character string, longer strings are truncated.\n
When optional ALT-DIVIDE-CHAR is ommitted the divider char is ``='' \(char 61\).
It is used to pad divider to DIVIDER-LENGTH.\n
If ALT-DIVIDE-CHAR \(a character or string\) is non-nil use it to instead.\n
:EXAMPLE\n\n\(mon-comment-divider-w-len 72\)\n
\(mon-comment-divider-w-len 72 \"_\"\)\n
\(mon-comment-divider-w-len 72 \"_\" \"-\"\)\n
\(mon-comment-divider-w-len 72\)\n
\(mon-comment-divider-w-len 72 95 \"-\"\)\n
\(mon-comment-divider-w-len 72 \"_\" 45\)\n        
\(mon-comment-divider-w-len 72 95 45\)\n 
\(mon-comment-divider-w-len 72 #o137 45\)\n
\(mon-comment-divider-w-len 72 #x5f  ?\-\)\n
\(length \(mon-comment-divider-w-len 85 95 45\)\)\n
:SEE-ALSO `*mon-default-comment-divider*', `mon-comment-divider',
`mon-comment-lisp-to-col', `mon-comment-divider-to-col',
`mon-comment-divider-w-len'.\n►►►"
  (let ((d-char (if alt-divide-char
                    (cond ((stringp alt-divide-char)
                           ;; :WAS (aref (string-to-vector alt-divide-char) 0))
                           (aref alt-divide-char 0))
                          ((and (characterp alt-divide-char) alt-divide-char)))
                  61))
        (*mon-default-comment-start*
         (cond ((stringp alt-comment-start)
                (cond ((>= (length alt-comment-start) 1)
                       (if (= (length alt-comment-start) 3)
                           (concat alt-comment-start " ")
                         ;; :WAS (concat (make-string 3 (aref (string-to-vector alt-comment-start) 0)) " ")))
                         (concat (make-string 3 (aref alt-comment-start 0)) " ")))
                      ((= (length alt-comment-start) 0) *mon-default-comment-start*)))
               ((characterp alt-comment-start) 
                (concat (make-string 3 alt-comment-start) " "))
               (t *mon-default-comment-start*)))
        (d-len (if (> divider-length 76) 76 divider-length)))
    (concat *mon-default-comment-start* (make-string d-len d-char))))
;;
;;; :TEST-ME (mon-comment-divider-w-len 72)
;;; :TEST-ME (mon-comment-divider-w-len 72 "_")
;;; :TEST-ME (mon-comment-divider-w-len 72 "_" "-")
;;; :TEST-ME (mon-comment-divider-w-len 72)
;;; :TEST-ME (mon-comment-divider-w-len 72 95 "-")
;;; :TEST-ME (mon-comment-divider-w-len 72 "_" 45)
;;; :TEST-ME (mon-comment-divider-w-len 72 95 45)
;;; :TEST-ME (length (mon-comment-divider-w-len 85 95 45))

;;; ==============================
;;; :CHANGESET 1711 <Timestamp: #{2010-05-06T15:28:04-04:00Z}#{10184} - by MON KEY>
;;; :RENAMED :FUNCTION `mon-today' -> `mon-date-stamp' 
;;; :ADDED keywords INSRTP and INTRP
;;; :CREATED <Timestamp: #{2009-10-23T20:57:47-04:00Z}#{09436} - by MON KEY>
(defun* mon-date-stamp (&key as-string as-symbol as-list-str as-list-num
                             as-vec-str as-vec-num insrtp intrp)
  "Return today's date as YYYY-MM-DD.\n
When keyword AS-STRING is non-nil return date as a string.\n
When keyword INTRP is non-nil or called-interactivley a string at point.\n
When keyword AS-SYMBOL is non-nil return date as a symbol.\n
When keyword AS-LIST-STR is non-nil return date as a list of three strings.\n
When keyword AS-LIST-NUM is non-nil return date as a list of three numbers.\n
When keyword AS-VEC-STR is non-nil return date as a vector of three strings.\n
When keyword AS-VEC-NUM is non-nil return date as a vector of three numbers.\n
When keyword INSRTP is non-nil insert return value at point as if with `prin1'\n
:NOTE The AS-*-NUM keywords return in decimal \(radix 10\).\n
:EXAMPLE\n\n\(mon-date-stamp :as-string t\)\n\n\(mon-date-stamp :as-symbol t\)\n
\(mon-date-stamp :as-list-str t\)\n\n\(mon-date-stamp :as-list-num t\)\n
\(mon-date-stamp :as-vec-str t\)\n\n\(mon-date-stamp :as-vec-num t\)\n
:ALIASED-BY `mon-stamp-date-only'
:ALIASED-BY `mon-today-stamp'\n
:SEE-ALSO `mon-get-current-year', `mon-format-iso-8601-time', `mon-stamp',
`mon-stamp-in-context', `mon-lisp-stamp', `mon-accessed-stamp', `mon-timestamp',
`mon-lisp-stamp', `mon-file-stamp', `mon-file-stamp-buffer-filename',
`mon-file-stamp-minibuffer', `mon-help-time-functions',
`mon-help-mon-time-functions', `mon-help-iso-8601', `mon-help-CL-time',
`mon-help-CL-local-time'.\n►►►"
  (interactive (list :intrp t))
  (let ((mds-2day (format-time-string "%Y-%m-%d")))
    (setq mds-2day
          (cond ((or intrp as-string) mds-2day)
                (as-symbol 
                 (eval-when (compile load eval)
                   (if (fboundp 'mon-string-to-symbol)
                       (mon-string-to-symbol mds-2day)
                     (car (read-from-string mds-2day)))))
                (as-list-str  `(,(substring mds-2day 0 4)
                                ,(substring mds-2day 5 7)
                                ,(substring mds-2day 8 10)))
                (as-list-num  (mapcar 'string-to-number
                                      (mon-date-stamp :as-list-str t)))
                (as-vec-str (apply 'vector (mon-date-stamp :as-list-str t)))
                (as-vec-num (apply 'vector (mon-date-stamp :as-list-num t)))))
    
    (cond (intrp (insert mds-2day))
          (insrtp (prin1 mds-2day (current-buffer)))
          (t mds-2day))))
;;
;;; :TEST-ME (mon-date-stamp :intrp t)
;;; :TEST-ME (mon-date-stamp :as-string t)
;;; :TEST-ME (mon-date-stamp :as-string t :insrtp t)
;;; :TEST-ME (mon-date-stamp :as-symbol t)
;;; :TEST-ME (mon-date-stamp :as-symbol t :insrtp t)
;;; :TEST-ME (mon-date-stamp :as-list-str t )
;;; :TEST-ME (mon-date-stamp :as-list-str t :insrtp t)
;;; :TEST-ME (mon-date-stamp :as-list-num t)
;;; :TEST-ME (mon-date-stamp :as-list-num t :insrtp t)
;;; :TEST-ME (mon-date-stamp :as-vec-str t)
;;; :TEST-ME (mon-date-stamp :as-vec-str t :insrtp t)
;;; :TEST-ME (mon-date-stamp :as-vec-num t)
;;; :TEST-ME (mon-date-stamp :as-vec-num t :insrtp t)

;;; ==============================
;;; :CREATED <Timestamp: Wednesday July 29, 2009 @ 06:24.19 PM - by MON KEY>
(defun mon-get-current-year (&optional insrtp intrp)
  "Return or insert current year at point.\n
:EXAMPLE\n\n\(mon-get-current-year\)\n
:SEE-ALSO `mon-date-stamp', `mon-format-iso-8601-time',
`mon-help-time-functions', `mon-help-mon-time-functions', `mon-help-iso-8601',
`mon-help-CL-time', `mon-help-CL-local-time'.\n►►►"
  (interactive "i\np")
  (let ((cy (format "%s" (nth 5 (decode-time (current-time))))))
    ;; :WAS (setq cy (format "%s" (nth 5 (decode-time (current-time)))))
    (if (or insrtp intrp)
        (princ cy (current-buffer))
      cy)))
;;
;;; :TEST-ME (mon-get-current-year) 
;;; :TEST-ME (mon-get-current-year t)2010
;;; :TEST-ME (call-interactively 'mon-get-current-year)

;;; ==============================
;;; :COURTESY Edward O'Connor <hober0@gmail.com> 
;;; :SUBJECT Re: Can `format-time-string' produce full/extended ISO 8601 times?
;;; :MAIL-LIST emacs-devel@gnu.org
;;; :DATE 2009-08-06-W32-4T14:51:02-0400Z
;;; :CREATED <Timestamp: 2009-08-06-W32-4T14:55:01-0400Z - by MON KEY>
(defun mon-format-iso-8601-time (&optional time insrtp intrp)
  "Return an ISO-8601 compliant timestring in 'Extended' format.\n
:EXAMPLE\n\n\(mon-format-iso-8601-time\)\n
When optional arg TIME is non-nil, it is a string format spec suitable for use
with `format-time-string'.\n
When INSRTP is non-nil or called interactively insert timestring at point.
Vanilla `format-time-string' returns 'UTC offset/ZONE' in ISO-8601 'Basic format:\n
 +/-time-hourtime-minute ->  +/- hhmm  -> \"-0500\"\n
The ISO standard's grammar spec is unclear w/re time-numoffset, and Emacs' has
interpreted this as permiting mixtures of Extended/Basic format e.g.:\n
 time-numoffset    = \(\"+\" / \"-\"\) time-hour [[\":\"] time-minute]\n
Where the \":\" delimiter has been deemed 'optional' to compliance and thereby
ommitted.\n
This function extends Emacs' interpretation by colon delimiting time offset, e.g:\n
 +/-time-hour:time-minute ->  +/-hh:mm  -> \"-05:00\"\n
Additionally, the \"Zulu\" or trailing Z of ISO-8601/rfc3339 appended to the UTC.\n
To this a second time format approximating the ISO-8601 week date is appended.
This second timestring has the form, \"#{YWD}\" where:
 Y -> Year within the century.
 W -> ISO-8601 week number.
 D -> Day of the week 1-7. Indexed from 1 with Monday as day 1.\n
This timestring is per the return value of the following format spec:
 (format-time-string \"#{%y%V%u}\"\n
:EXAMPLE\n\n\(mon-format-iso-8601-time\)\n
\(mon-format-iso-8601-time \(nth 5 get-attr\)\)\n
:NOTE `#{' is a user reserved dispatching macro char in Common-Lisp.\n
:SEE \(URL `http://isotc.iso.org/livelink/livelink/4021199/ISO_8601_2004_E.zip?func=doc.Fetch&nodeid=4021199')
:SEE \(URL `http://www.ietf.org/rfc/rfc3339.txt') <- Date and Time on the Internet.
:SEE \(URL `http://www.w3.org/TR/NOTE-datetime')  <- W3C Specification about UTC Date and Time.
:SEE \(URL `http://en.wikipedia.org/wiki/ISO-8601')
:SEE \(URL `http://en.wikipedia.org/wiki/ISO_8601_usage')
:SEE (URL `http://en.wikipedia.org/wiki/ISO_week_date')\n
:SEE-ALSO `mon-conv-time-flt-pnt', `mon-comp-times-flt-pnt', `mon-get-file-mod-times',
`mon-convert-ebay-time', `mon-timestamp', `mon-date-stamp',
`mon-get-current-year', `file-newer-than-file-p', `file-attributes',
`current-time', `decode-time', `encode-time', `with-decoded-time-value',
`encode-time-value', `time-subtract', `current-time-string',
`format-time-string', `format-seconds', `float-time',
`mon-help-time-functions', `mon-help-mon-time-functions', `mon-help-iso-8601',
`mon-help-CL-time', `mon-help-CL-local-time'.\n►►►"
  (interactive "i\ni\np")
  (let* ((colon-z-stamp 
          ;; (format-time-string "%Y-%m-%d-W%V-%uT%T%zZ" time))
          ;; (format-time-string "%Y-%m-%d-W%V-%uT%T%zZ" time))
          ;; :NOTE `#{' is a reserved user dispatching macro char in CL
           (format-time-string "#{%Y-%m-%dT%T%zZ}#{%y%V%u}" time)) 
         (iso-8601-colon-stamp
          (format "%s:%s%s"  ;"%s:%s" 
                  (substring colon-z-stamp  0 -12) ;0 -3) 
                  (substring colon-z-stamp 24 28)
                  (substring colon-z-stamp 28) ;-3)
                  )))
    (if (or insrtp intrp)
        (insert iso-8601-colon-stamp)
      iso-8601-colon-stamp)))
;;
;;; :TEST-ME (mon-format-iso-8601-time)
;;; :TEST-ME (mon-format-iso-8601-time nil t)
;;; :TEST-ME (mon-format-iso-8601-time (nth 5 get-attr))
;;; :TEST-ME (apply 'mon-format-iso-8601-time nil '(t))

;;; ==============================
;;; :NOTE Why this isn't already a default function I'll never know...
;;; :CREATED <Timestamp: Wednesday June 03, 2009 @ 08:07.05 PM - by MON KEY>
(defun mon-file-older-than-file-p (file1 file2)
  "Return t when FILE1 is older than FILE2.\n
:SEE-ALSO `file-newer-than-file-p', `mon-get-file-mod-times',
`mon-help-time-functions', `mon-help-mon-time-functions', `mon-help-iso-8601',
`mon-help-CL-time', `mon-help-CL-local-time'.\n►►►"
  (let ((fepf1 (if (file-exists-p file1)
                   t 
                 (error (concat ":FUNCTION `mon-file-older-than-file-p' " 
                                "-- arg FILE1 does not exist :\n%38c%s") 32 file1)))
        (fepf2 (if (file-exists-p file2) 
               t 
                 (error (concat ":FUNCTION `mon-file-older-than-file-p' " 
                                "-- arg FILE2 does not exist :\n%38c%s") 32 file2))))
    (not (file-newer-than-file-p file1 file2))))
;;
;;; :TEST-ME (let ((tt--mfotfp buffer-file-name))
;;;               `(:FILE-1 ,tt--mfotfp 
;;;                 :FILE-2 ,(concat tt--mfotfp "c") 
;;;                 :FILE-1-OLDR ,(mon-file-older-than-file-p tt--mfotfp (concat tt--mfotfp "c"))
;;;                 :FILE-2-OLDR ,(mon-file-older-than-file-p (concat tt--mfotfp "c") tt--mfotfp )))

;;; ==============================
;;; :NOTE (file-newer-than-file-p filename1 filename2)
;;; :MODIFICATIONS <Timestamp: #{2010-03-11T12:01:56-05:00Z}#{10104} - by MON KEY>
;;; :CREATED <Timestamp: Friday May 15, 2009 @ 01:16.43 PM - by MON KEY>
(defun mon-get-file-mod-times (file-or-dir &optional as-list)
  "Return FILE-OR-DIR's modification and accessed time as a list or string.\n
When optional arg AS-LIST is non-nil return vale as a plist. Default is as a string.
Accessed modified times are as per return value of `mon-format-iso-8601-time'.\n
If FILE-OR-DIR is a file or directory this is indicated by the first list elt or
string range of return value as either:\n\n `:FILE' or `:DIRECTORY'\n
When AS-LIST is ommitted return value is a string with the form:\n
 \"[:DIRECTORY|:FILE] <FILE-OR-DIR>
   :LAST-MODIFIED #{2010-02-08T21:40:22-05:00Z}#{10062}
   :LAST-ACCESSED #{2010-03-10T18:57:13-05:00Z}#{10103}\"\n
When AS-LIST is non-nil return value is a list with the form:\n
 \([:DIRECTORY|:FILE] \"<FILE-OR-DIR>\" 
   :LAST-MODIFIED \"#{2010-02-08T21:40:22-05:00Z}#{10062}\" 
   :LAST-ACCESSED \"#{2010-03-10T18:57:13-05:00Z}#{10103}\"\)\n
:EXAMPLE\n\n\(mon-get-file-mod-times user-emacs-directory\)\n
\(mon-get-file-mod-times user-emacs-directory t\)\n
\(mon-get-file-mod-times \(file-truename \(locate-user-emacs-file \"~/.emacs\"\)\)\)\n
\(mon-get-file-mod-times \(file-truename \(locate-user-emacs-file \"~/.emacs\"\)\) t\)\n\n
:NOTE on w32 return value may be affected by `w32-get-true-file-attributes'.\n
:SEE-ALSO `file-newer-than-file-p', `file-attributes', `current-time',
`decode-time', `encode-time', `with-decoded-time-value' `encode-time-value',
`time-subtract', `current-time-string', `format-time-string', `format-seconds',
`float-time', `mon-help-time-functions', `mon-help-mon-time-functions',
`mon-help-iso-8601', `mon-help-CL-time', `mon-help-CL-local-time'.\n►►►"
  (let* ((get-attr (file-attributes file-or-dir))
	 (last-mod (mon-format-iso-8601-time (nth 5 get-attr)))
	 (last-acc (mon-format-iso-8601-time (nth 4 get-attr)))
	 (f-type (nth 0 get-attr)))
    (setq f-type
          (if f-type
              (if as-list 
                  `(,:DIRECTORY ,file-or-dir 
                    ,:LAST-MODIFIED ,last-mod 
                    ,:LAST-ACCESSED ,last-acc)
                  (format ":DIRECTORY %s\n:LAST-MODIFIED %s\n:LAST-ACCESSED %s" 
                          file-or-dir last-mod last-acc))
              (if as-list 
                  `(,:FILE ,file-or-dir 
                    ,:LAST-MODIFIED ,last-mod 
                    ,:LAST-ACCESSED ,last-acc)
                  (format ":FILE %s\n:LAST-MODIFIED %s\n:LAST-ACCESSED %s" 
                          file-or-dir last-mod last-acc))))))
;;
;;; :TEST-ME (mon-get-file-mod-times user-emacs-directory)
;;; :TEST-ME (mon-get-file-mod-times user-emacs-directory t)
;;; :TEST-ME (mon-get-file-mod-times (file-truename (locate-user-emacs-file "~/.emacs")))
;;; :TEST-ME (mon-get-file-mod-times (file-truename (locate-user-emacs-file "~/.emacs")) t)

;;; ==============================
;;; :COURTESY :FILE ido.el :WAS `ido-time-stamp'
;;; :CREATED <Timestamp: Wednesday May 20, 2009 @ 04:54.25 PM - by MON KEY>
(defun mon-conv-time-flt-pnt (&optional time)
  "Time is a floating point number \(fractions of 1 hour\).\n
Conversion  is calculated from the high and low bits of `current-time' e.g.:\n
  \(/ (+ (* HIGH-bit  65536.0) LOW-bit) 3600.0)\n
:EXAMPLE\n\(mon-conv-time-flt-pnt (nth 4 (file-attributes \"~/.emacs\")))
  ;=>345113.5183333333\n
:NOTE Not acccurate to microsecond. Take care comparing object timestamps.\n
:SEE-ALSO `mon-comp-times-flt-pnt', `mon-get-file-mod-times',
`mon-file-older-than-file-p', `file-newer-than-file-p',
`mon-help-time-functions', `mon-help-mon-time-functions', `mon-help-iso-8601',
`mon-help-CL-time', `mon-help-CL-local-time'.\n►►►"
  (let (op-time)
    (setq op-time (or time (current-time)))
    (/ (+ (* (car op-time) 65536.0)  (cadr op-time)) 3600.0)))
;;
;;; :TEST-ME (mon-conv-time-flt-pnt (current-time))
;;; :TEST-ME (mon-conv-time-flt-pnt)
;;; :TEST-ME (mon-conv-time-flt-pnt (nth 4 (file-attributes "~/.emacs")))

;;; ==============================
;;; :CREATED <Timestamp: Wednesday May 20, 2009 @ 04:54.25 PM - by MON KEY>
(defun mon-comp-times-flt-pnt (&optional t1 t2)
  "Test if time T1 is greater than time T2.\n
Return t when T1 is more recent than T2.\n
When T1 or T2 is nil value defaults to `current-time'.\n
T1 and T2 are the first _two_ integers of time-value list per three integer list
of `current-time's return value e.g.:\n \(18963 22549 531000).\n
:NOTE Where third integer microsecond count \(cddr\) of `current-time' is
returned it is dropped.\n
:EXAMPLE\n\(mon-comp-times-flt-pnt 
  (current-time) ;<--T1 is now!
  '(18963 22549 531000)) ;<--T2 is in the past.\n ;=> t
\(mon-comp-times-flt-pnt '\(18963 22549 531000\) \(current-time\)\) ;=> nil\n
\(mon-comp-times-flt-pnt '\(18963 22549 531000\)\)\n
  \(current-time-string '\(18963 22549 531000\)\)
 ;=>\"Tue May 19 21:08:37\" ;<-- T2's value converted to human speak.\n
:SEE-ALSO `mon-conv-time-flt-pnt', `mon-get-file-mod-times',
`mon-file-older-than-file-p', `file-newer-than-file-p',
`mon-help-time-functions', `mon-help-mon-time-functions', `mon-help-iso-8601',
`mon-help-CL-time', `mon-help-CL-local-time'.\n►►►"
  (let ((got-t1 (if t1 t1	nil))
        (got-t2 (if t2 t2	nil)))
    (> (mon-conv-time-flt-pnt got-t1) (mon-conv-time-flt-pnt got-t2))))

;;; =======================
;;; :DEPRECATED
(defun mon-accessed-time-stamp ()
  ":DEPRECATED Build the function for inserting shortform time-stamps.\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-accessed-stamp', `mon-timestamp', `mon-stamp',
`mon-stamp-in-context', `mon-lisp-stamp', `mon-help-time-functions',
`mon-help-mon-time-functions', `mon-help-iso-8601', `mon-help-CL-time',
`mon-help-CL-local-time'\n►►►"
  (insert (format-time-string "%A %B %d, %Y")))
;;
;;; :TEST-ME (mon-accessed-time-stamp)

;;; ==============================
;;; :CHANGESET 1712 <Timestamp: #{2010-05-06T16:23:37-04:00Z}#{10184} - by MON KEY>
;;; :CREATED <Timestamp: Saturday July 18, 2009 @ 05:35.09 PM - by MON KEY>
(defun* mon-timestamp (&key insrtp accessed naf intrp)
  "Core timestamping function generates conditional timestamps.\n
Builds extended ISO-8601 timestamp using `mon-format-iso-8601-time'.\n
When kewyord ACCESSED is non-nil generates and accessed style timestamp.\n
When kewyord NAF is non-nil generates a naf-mode style timestamp.\n
When kewyord INSRTP or INTRP is non-nil insert style of timestamp at point.\n
Function optimized to take advantage unique stamp NAME using file alist lookups
in variable `*mon-timestamp-cond*'.\n
When filename is a member of that list keyword args are still in effect but use
the NAME value associated with buffer-filename.\n
:EXAMPLE\n\n\(mon-timestamp :naf t\)\n\n\(mon-timestamp :accessed t\)\n
:SEE-ALSO `mon-accessed-time-stamp', `mon-timestamp', `mon-stamp',
`mon-stamp-in-context', `mon-lisp-stamp', `mon-help-time-functions',
`mon-help-mon-time-functions', `mon-help-iso-8601', `mon-help-CL-time',
`mon-help-CL-local-time'.\n►►►"
  (interactive (list :intrp t))
  (let* ((mt-TS (cond 
                 (accessed (mon-format-iso-8601-time))
                 (naf (mon-format-iso-8601-time))
                 (t (mon-format-iso-8601-time))))
         ;; If we are in a buffer with a filename to associate, does it associate?
         (mt-COND-NAME
          (let (mt-CN)
            (when (mon-buffer-written-p)
              (cond ((assoc (mon-get-buffer-parent-dir) *mon-timestamp-cond*)
                     (setq mt-CN (cadr (assoc (mon-get-buffer-parent-dir) *mon-timestamp-cond*))))
                    ((assoc (file-name-nondirectory (buffer-file-name)) *mon-timestamp-cond*)
                     (setq mt-CN (cadr (assoc (file-name-nondirectory (buffer-file-name)) *mon-timestamp-cond*))))))
            (unless (mon-buffer-written-p)
              (cond  ((assoc (mon-get-buffer-parent-dir) *mon-timestamp-cond*)
                      (setq mt-CN (cadr (assoc (mon-get-buffer-parent-dir) *mon-timestamp-cond*))))
                     ((assoc (buffer-name) *mon-timestamp-cond*)
                      (setq mt-CN (cadr (assoc (buffer-name) *mon-timestamp-cond*))))))
            mt-CN))
         (mt-NAME (cond
                   (mt-COND-NAME
                    (cond (accessed (concat " - " mt-COND-NAME))
                          (naf (concat " - by " mt-COND-NAME ">"))
                          (t (concat " - by " mt-COND-NAME ">"))))
                   ((or IS-MON-P-W32 (equal user-real-login-name  (cadr (assoc 5 *MON-NAME*))))
                    (let ((mt-MN-W32 (upcase (cadr (assoc 7 *MON-NAME*)))))
                      (cond (accessed ;; :NOTE Do not replace me! naf-mode regexps keyed on this.
                             (concat " - " mt-MN-W32)) ;; :WAS (cadr (assoc 7 *MON-NAME*)))) 
                            ;; :NOTE Do not replace me! naf-mode regexps keyed on this.
                            (naf  (concat " - by " mt-MN-W32 ">")) ;; :WAS (cadr (assoc 7 *MON-NAME*)) ">"))
                            (t ""))))
                   ((or IS-MON-P-GNU  
                        (and (equal system-type 'gnu/linux)
                             (equal user-real-login-name (cadr (assoc 5 *MON-NAME*)))))
                    (let ((mt-MN (upcase (cadr (assoc 7 *MON-NAME*)))))
                      (cond (accessed (concat " - " mt-MN)) ;; :WAS (cadr (assoc 7 *MON-NAME*))
                            (naf (concat " - by " mt-MN ">")) ;; :WAS (cadr (assoc 7 *MON-NAME*)) ">"))
                            (t ""))))
                   ((or IS-BUG-P IS-BUG-P-REMOTE
                        (equal user-real-login-name  (cadr (assoc 6 *BUG-NAME*))))
                    (let ((mt-BN (concat " - "(upcase (cadr (assoc 7 *BUG-NAME*))))))
                      (cond (accessed (concat " - " mt-BN)) ;; :WAS (upcase (cadr (assoc 7 *BUG-NAME*)))))
                            (naf (concat " - by "mt-BN ">"))
                            (t ""))))
                   (t (cond (accessed " - ANONYMOUS")
                            (naf " - by ANONYMOUS>")
                            (t "")))))
         (mt-PUT-STAMP (concat mt-TS mt-NAME)))
    (if (or intrp insrtp) 
        (insert mt-PUT-STAMP)
      mt-PUT-STAMP)))
;;
;;; :TEST-ME (mon-timestamp)
;;; :TEST-ME (mon-timestamp :insrtp t)
;;;          ;=> #{2009-08-24T15:43:45-04:00Z}#{09351} - by MON KEY>
;;; :TEST-ME (mon-timestamp :accessed t)
;;; :TEST-ME (mon-timestamp :accessed t :insrtp t)
;;;          ;=> #{2009-08-24T15:43:36-04:00Z}#{09351} - MON KEY
;;; :TEST-ME (mon-timestamp :naf t)
;;; :TEST-ME (mon-timestamp :naf t :insrtp t) 
;;;          ;=> #{2009-08-24T15:43:21-04:00Z}#{09351} - by MON KEY>
;;; :TEST-ME (mon-timestamp)
;;; :TEST-ME (call-interactively 'mon-timestamp)
;;; :TEST-ME (cadr (assoc "mon-doc-help-utils.el" *mon-timestamp-cond*))
;;;
;;; :TEST-ME (let ((IS-MON-P-GNU (IS-BUG-P t)) (mon-timestamp))

;;; =======================
;;; :NOTE Use with (add-hook 'write-file-hooks 'mon-stamp) ??
;;; :CREATED <Timestamp: Thursday February 19, 2009 @ 06:31.47 PM - by MON KEY>
(defun mon-stamp (&optional insrtp intrp)
  "Return timestamp per a conditional test on user-name and/or system-type.\n
Insert following formatted date/time at point:\n
\"<Timestamp: #{2009-08-11T14:42:37-04:00Z}#{09332} - by MON KEY>\"\n
:EXAMPLE\n\n(mon-stamp)\n
:NOTE Stamp is fontlocked when invoked from a `naf-mode' buffer.\n
:SEE-ALSO `mon-timestamp', `mon-accessed-time-stamp', `mon-accessed-stamp',
`mon-lisp-stamp', `mon-stamp-in-context', `*mon-timestamp-cond*',
`mon-file-stamp', `mon-get-new-buffer-w-stamp', `mon-help-time-functions',
`mon-help-mon-time-functions', `mon-help-iso-8601', `mon-help-CL-time',
`mon-help-CL-local-time'.\n►►►"
  (interactive "i\np")
  (let ((tstmp (format  "<Timestamp: %s" (mon-timestamp :naf t))))
    (if (or insrtp intrp)
        (insert tstmp)
      tstmp)))
;;
;;; :TEST-ME (mon-stamp) 
;;; :TEST-ME (mon-stamp t)
;;; :TEST-ME (call-interactively 'mon-stamp) 

;;; ==============================
;;; :NOTE adjust code that calls this programatically to use t.
;;; :CREATED <Timestamp: Saturday July 18, 2009 @ 05:23.47 PM - by MON KEY>
(defun mon-accessed-stamp (&optional insrtp commented intrp)
  "Return an 'accessed date' timestamp string.\n
When INSRTP or called interactively insert at accessed stamp at point.\n
If COMMENTED is non-nil or called interactively with prefix arg returns with a 
';;; :ACCESSED ' Prefix - default is 'accessed: '.
Invoked from a `naf-mode' buffer acessed-stamp fontlocked by
`naf-mode-accessed-by-flag'.\n
:EXAMPLE\n\n\(mon-accessed-stamp\)\n\(mon-accessed-stamp nil t\)\n
:SEE-ALSO `mon-accessed-time-stamp', `mon-timestamp', `mon-stamp',
`mon-stamp-in-context', `mon-lisp-stamp', `mon-file-stamp',
`mon-get-new-buffer-w-stamp', `mon-help-time-functions',
`mon-help-mon-time-functions', `mon-help-iso-8601', `mon-help-CL-time',
`mon-help-CL-local-time'.\n►►►"
  (interactive "i\nP\np")
  (let ((tstmp (if commented
                   (concat ";;; :ACCESSED " (mon-timestamp :accessed t))
                 (concat "accessed: " (mon-timestamp :accessed t)))))
    (if (or insrtp intrp)
        (insert tstmp)
      tstmp)))
;;
;;; :TEST-ME (mon-accessed-stamp)
;;; :TEST-ME (mon-accessed-stamp t)
;;; :TEST-ME (mon-accessed-stamp t t)
;;; :TEST-ME (mon-accessed-stamp nil t)
;;; :TEST-ME (call-interactively 'mon-accessed-stamp)

;;; ==============================
;;; :CHANGESET 2155 <Timestamp: #{2010-09-27T13:07:03-04:00Z}#{10391} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-09-26T09:40:49-04:00Z}#{10387} - by MON>
(defun* mon-stamp-in-context (&optional insrtp intrp 
                                        &key 
                                        w-changed
                                        w-comment-start 
                                        w-newline                                        
                                        w-divider
                                        w-alt-div-len
                                        w-alt-div-char-str)
  "Like `mon-lisp-stamp' but for non lisp-mode modes.\n
When Keyword W-CHANGED is non-nil and `vc-working-revision' returns for
`current-buffer's `buffer-file-name' return:
 \";;; :CHANGESET <CHANGESET-NUMBER>\"
If buffer is not under version-control return: 
 \";;; :CHANGED <TIMESTAMP>\"
Keyword W-COMMENT-START specifies an alternative comment prefix string.  When
`comment-start' is non-nil use its first char when constructing a comment
prefix, else if `*mon-default-comment-start*' is non-nil use its value.\n
When keyword W-NEWLINE is non-nil ensure a trailing newline is concatenated to
return value when neccsary.
When a delimiting string is desider and it can be determined that one is
\"appropriate\" to the return value \(the heuristic pivots on return value of
`vc-working-revision'\) the following keywords take effect: 
Keyword W-DIVIDER when non-nil concatenates a comment divider as needed.
Generation of comment divider is as if by `mon-comment-divider-w-len'.\n
When keyword W-ALT-DIV-LEN is non-nil it is an positive integer value for
the DIVIDER-LENGTH arg to `mon-comment-divider-w-len'. Defalut is 30.\n
When keyword W-ALT-DIV-CHAR-STR is non-nil it is a character or string arg for
the ALT-DIVIDE-CHAR to `mon-comment-divider-w-len'. Default is \"=\" \(char 61\).\n
:EXAMPLE\n\n\(mon-stamp-in-context\)
\(mon-stamp-in-context nil nil :w-changed t\)
\(mon-stamp-in-context nil nil :w-changed t :w-newline t\)
\(mon-stamp-in-context nil nil :w-changed t :w-divider t\)
\(mon-stamp-in-context nil nil :w-comment-start \"*\"
\(mon-stamp-in-context nil nil :w-comment-start \"~\" :w-alt-div-char-str \"+\"\)
\(mon-stamp-in-context nil nil :w-comment-start \"#\" 
                               :w-alt-div-char-str \"_\" 
                               :w-alt-div-len 68\)\n
\(mon-stamp-in-context nil nil :w-comment-start \"#\" 
                               :w-alt-div-char-str \"-\" 
                               :w-alt-div-len 3 :w-newline t\)\n
:NOTE Dynamically binds the variables:\n
 `*mon-default-comment-start*' `*mon-default-comment-divider*'\n
:SEE-ALSO `mon-lisp-stamp', `mon-insert-lisp-stamp', `mon-accessed-stamp',
`mon-date-stamp', `mon-timestamp', `mon-get-new-buffer-w-stamp',
`mon-help-time-functions', `mon-help-mon-time-functions', `mon-help-iso-8601',
`mon-help-CL-time', `mon-help-CL-local-time'.\n►►►"
  (interactive "i\np")
  (let* ((w-divider :w-divider)
         (cur-cmnt-start (or (and (stringp w-comment-start)
                                  (aref w-comment-start 0))
                             (and (not (null comment-start)) 
                                  (aref comment-start 0))
                             59))
         (*mon-default-comment-start* (concat (make-string 3 cur-cmnt-start) " "))
         ;; :NOTE `mon-comment-divider-w-len' dynamically binds `*mon-default-comment-start*'
         (cur-cmnt-dvdr 
          (mon-comment-divider-w-len (or w-alt-div-len 30) cur-cmnt-start w-alt-div-char-str))
         (*mon-default-comment-divider* cur-cmnt-dvdr)
         (cur-chngst (when (and buffer-file-name (vc-working-revision buffer-file-name))
                       (concat  *mon-default-comment-start* 
                                ":CHANGESET "(vc-working-revision buffer-file-name))))
         (tstmp-naf (mon-timestamp :naf t))
         (w-nl (when w-newline "\n"))
         (w-mods (or cur-chngst
                     (and w-changed (not cur-chngst)
                          (concat *mon-default-comment-start* 
                                  ":CHANGED <Timestamp: " tstmp-naf))))
         (tstmp (cond ((and w-mods w-changed)
                       (concat w-mods w-nl))
                      (cur-chngst (setq w-divider t)
                                  (concat cur-chngst "\n"
                                          *mon-default-comment-start* 
                                          ":CREATED <Timestamp: "  tstmp-naf w-nl))
                      (t (concat *mon-default-comment-start*
                                 ":CREATED <Timestamp: " tstmp-naf w-nl))))
         (f-tstmp (let ((mk-div (and w-divider (concat *mon-default-comment-divider* "\n"))))
                    (or (and mk-div (not w-mods) (concat mk-div tstmp))
                        (and mk-div cur-chngst (not w-changed) (concat mk-div tstmp))
                        tstmp))))
    (if (or insrtp intrp) 
        (insert f-tstmp)
      f-tstmp))) 

(declare-function "vc-working-revision" "vc-hooks")
;;; ==============================
;;; :NOTE The above fncn `mon-stamp-in-context' allows rewriting/simplifiying
;;; `mon-lisp-stamp' w/ e.g.:
;;;  (mon-stamp-in-context t nil :w-changed t :w-divider t :w-newline t)
;;; But, the newline handling/comment dividers can be orthogonal.
;;; :CHANGESET 1701 <Timestamp: #{2010-04-06T15:48:44-04:00Z}#{10142} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-23T18:06:13-04:00Z}#{09435} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-06-W32-4T15:18:23-0400Z} - by MON KEY>
(defun mon-lisp-stamp (&optional insrtp intrp w-modifications)
  "Return or insert at point a `mon-comment-divider' newline and `mon-stamp'.\n
When INSRTP is non-nil or called interactively timestap insert at point.\n
When W-MODIFICATIONS is non-nil or called interactively with Prefix Arg and
buffer-file is under version control return:\n
 ;;; :CHANGESET REV <Timestamp: #{ISO-TIME}#{ISO-WEEK} - by USER>\n
If not, return:\n
 ;;; :CHANGED <Timestamp: #{ISO-TIME}#{ISO-WEEK} - by USER>\n
Else, if buffer-file is under version control default return value is:\n
 ;;; :CHANGESET 1701
 ;;; :CREATED <Timestamp: #{ISO-TIME}#{ISO-WEEK} - by USER>\n
Otherwise, the default return value is:\n
 ;;; :CREATED <Timestamp: #{ISO-TIME}#{ISO-WEEK} - by USER>\n
Use after creating new elisp functions to delimit and date them.\n
:EXAMPLE\n\n\(mon-insert-lisp-stamp\)\n\(mon-insert-lisp-stamp nil nil t\)\n
:SEE-ALSO `mon-insert-lisp-stamp', `mon-file-stamp',
`mon-get-new-buffer-w-stamp', `mon-insert-copyright', `mon-insert-lisp-testme',
`mon-insert-lisp-CL-file-template', `*mon-default-comment-divider*',
`mon-comment-divider', `mon-comment-divider-to-col-four',
`mon-insert-lisp-evald', `mon-insert-naf-mode-face-template',
`mon-insert-naf-mode-xref-template' `mon-insert-naf-mode-var-const-template',
`mon-insert-naf-mode-constant-template', `mon-help-time-functions',
`mon-help-mon-time-functions', `mon-help-iso-8601', `mon-help-CL-time',
`mon-help-CL-local-time'.\n►►►"
  (interactive "i\np\nP")
  (let* ((cur-chngst (if buffer-file-name
                         (when (vc-working-revision buffer-file-name)
                           (concat ":CHANGESET " (vc-working-revision buffer-file-name)))))
         (tstmp (cond (w-modifications
                       (concat ";;; " (or cur-chngst ":CHANGED") 
                               " <Timestamp: "(mon-timestamp :naf t )))
                      (cur-chngst
                       (concat ";;; " cur-chngst "\n"
                               ";;; :CREATED <Timestamp: " (mon-timestamp :naf t )))
                      (t (concat ";;; :CREATED <Timestamp: " (mon-timestamp :naf t )))))
         (f-tstmp (if (or insrtp intrp)
                      (concat "\n" *mon-default-comment-divider* "\n" tstmp)
                      (concat *mon-default-comment-divider* "\n" tstmp))))
    (if (or insrtp intrp) 
        (insert f-tstmp) 
        f-tstmp)))
;;
;;; :TEST-ME (mon-lisp-stamp)
;;; :TEST-ME (mon-lisp-stamp t)
;;; :TEST-ME (mon-lisp-stamp nil nil t)
;;; :TEST-ME (mon-lisp-stamp t nil t)
;;; :TEST-ME (call-interactively 'mon-lisp-stamp)


;;; ==============================
;;; :NOTE This is not correct for EOF MON's existing elisp footers with format:
;;;           ;;; ============================
;;;           ;;; <PACKAGE>.el ends here
;;;           EOF
;;;     Also, the EOF comment divider string may not always have length 32.
;;;     e.g. (length *mon-default-comment-divider*)
;;;     :SEE `mon-comment-divider-w-len' for possible workarounds.
;;; :MODIFICATIONS <Timestamp: #{2009-12-17T17:35:35-05:00Z}#{09514} - by MON>
;;; :CREATED <Timestamp: #{2009-10-24T11:52:41-04:00Z}#{09436} - by MON>
(defun mon-file-stamp-vrfy-put-eof (&optional insrtp)
  "Return the preferred EOF delimiter if not present in current-buffer.\n
When INSRTP is non-nil and the delimiter wasn't found insert it.
Does not move point.\n
:EXAMPLE\n\n\(mon-file-stamp-vrfy-put-eof nil\)\n
:SEE-ALSO `mon-file-stamp', `mon-get-new-buffer-w-stamp',
`mon-insert-file-template', `mon-insert-lisp-CL-file-template',
`*mon-default-comment-divider*', `mon-comment-divider-w-len',
`mon-help-CL-time', `mon-help-iso-8601', `mon-help-CL-local-time',
`mon-help-mon-time-functions', `mon-help-time-functions'.\n►►►"
  (interactive "p")
  (let  (;; :NOTE Use of *mon-default-comment-divider* gives granularity/portablility.
         (end-of-dlm *mon-default-comment-divider*) 
         (end-of ";;; EOF")
         (do-EOF t))
    (save-excursion
      (goto-char (buffer-end 1))
      ;; :WAS (when (search-backward-regexp 
      ;;        (concat "^" end-of-dlm "\n" end-of "$") (line-end-position -5) t)
      ;;      (setq do-EOF nil))
      (when (search-backward-regexp 
             (concat "^\\(" (substring end-of-dlm 0 4) "=\\{26,68\\}\\)\n"
                     "\\(;;; .* ends here\n\\)?"
                     ";;; EOF") (line-end-position -5) t)
        (setq do-EOF nil))
      (when do-EOF
        ;; :WAS (when (search-backward-regexp (concat "^" end-of-dlm "\n" end-of "$") nil t)
        ;;      (setq do-EOF nil))
        ;;; There isn't a footer in the last 5 lines what about further up?
        (when (search-backward-regexp 
               (concat "^\\(" (substring end-of-dlm 0 4) "=\\{26,36\\}\\)\n"
                       ";;; EOF") nil t)
          (setq do-EOF nil))
        (cond ((and insrtp do-EOF)
               (goto-char (buffer-end 1))
               (princ (concat "\n" end-of-dlm "\n" end-of) (current-buffer)))
              (do-EOF (concat "\n" end-of-dlm "\n" end-of)))))))
;;
;;; :TEST-ME (with-temp-buffer (mon-file-stamp-vrfy-put-eof nil))

;;; ==============================
;;; :NOTE The comment prefix `;;;' uses \x3b to prevent false positives with
;;;       other source processing regexps from aid examples.
;;; :MODIFICATIONS <Timestamp: #{2009-12-17T19:56:13-05:00Z}#{09515} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-24T12:20:13-04:00Z}#{09436} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-24T15:46:26-04:00Z}#{09351} - by MON KEY>
(defun mon-file-stamp (&optional insrtp intrp w/url at-point)
  "Return MON's default `time-stamp'd file header with EOF footer.\n
Find first line matching the file created time-stamp prefixed with:\n
\x3b;; :FILE-CREATED\n
When present do not alter existing stamp, else return a time-stamp as:\n
\x3b;; :FILE-CREATED <Timestamp: #{2009-12-17T18:49:01-05:00Z}#{09514} - by MON>\n
Locate first line beginning matching either of the formats:\n
\x3b;; :FILE\n\x3b;; :FILE :DIRECTORY\n
When either is present do not alter the existing file or directory name.
If neither is present, and buffer-filename is t return value of filename as:\n
\x3b;; :FILE /some/path/to/file\n
When buffer-file-name is nil, return expanded value of default-directory as:\n
\x3b;; :FILE :DIRECTORY /some/path/to/buffers/dir/\n
When optional arg W/URL is non-nil, find first line matching an external
reference with one of the three formats:\n
\x3b;; :SOURCE\n;;; :SEE\n;;; \(URL\n
When present do not alter. Else, return the wrapped URL of arg to W/URL as:\n
\x3b;; :SOURCE \(URL `http://some-url.com'\)\n
Find the \"EOF\" string and commented delimiter around last 4 lines of buffer.
When present do nothing. When an EOF file footer is not present return one as:\n
\x3b;; ==============================\n;;; EOF\n
When INSRTP or INTRP is non-nil defualt behaviour is to insert return value in
buffer at BOF and EOF. Does not move point.\n
When INSRTP or INTRP is non-nil, and AT-POINT is non-nil insert return prepended
with a newline and insert at point. Does not move point.\n
When AT-POINT is non-nil insert return value at point. Does not move point.
:NOTE The AT-POINT arg overrides a nil arg of either INSRTP or INTRP.\n
:EXAMPLE\n
\(mon-file-stamp nil nil\)\n
\(mon-file-stamp nil nil \"http://www.emacswiki.org\")\n
:NOTE Following illustrates adding a URL :SOURCE line to an existing file stamp.
      Watch the URL added at line 3 while other existing lines remain unchanged.\n
:EXAMPLE\n\n\(save-excursion
  \(with-output-to-temp-buffer \"*MON-TEST-MFS*\"
    \(set-buffer \"*MON-TEST-MFS*\"\)
    \(display-buffer \"*MON-TEST-MFS*\" t\)
    \(mon-file-stamp t\)
    \(sit-for 3\)
    \(mon-file-stamp t nil \"http://wikipedia.com\"\)\)
  \(sit-for 3\)
  \(kill-buffer  \"*MON-TEST-MFS*\"\)\)\n
:SEE-ALSO `mon-file-stamp-vrfy-put-eof', `mon-file-stamp-minibuffer',
`mon-file-stamp-buffer-filename', `mon-get-new-buffer-w-stamp',
`mon-file-dir-attributes->plist', `mon-lisp-stamp', `mon-timestamp',
`mon-stamp', `mon-accessed-stamp', `*mon-default-comment-divider*',
`mon-help-time-functions', `mon-help-mon-time-functions', `mon-help-iso-8601',
`mon-help-CL-time', `mon-help-CL-local-time'.\n►►►"
  (interactive "i\np\nP")
  (let* ((the-url (cond ((and intrp w/url)
                         (concat ";;; :SOURCE (URL `" (read-string "URL to wrap :") "')"))
                        ((and (not intrp) w/url)
                         (concat ";;; :SOURCE (URL `"  w/url "')"))))
         (the-file-or-dir
          (if (buffer-file-name)
              (concat ";;; :FILE " (buffer-file-name))
              (concat ";;; :FILE :DIRECTORY " (expand-file-name default-directory))))
         (fc-stmp (concat ";;; :FILE-CREATED <Timestamp: " (mon-timestamp :naf t )))
         (f-tstmp (concat fc-stmp "\n" 
                          the-file-or-dir 
                          (if w/url 
                              (concat "\n" the-url "\n") 
                              "\n")
                          *mon-default-comment-divider* "\n")))
    (save-excursion    
      (cond ( ;; Return the standard `f-tstmp' w/ footer
             (and (not intrp) (not insrtp))
             (format "%s%s" f-tstmp 
                     (concat "\n" *mon-default-comment-divider* "\n;;; EOF\n")))
            (at-point
             (if (= (point) (point-min))
                 ;; If if there is an existing Created don't overwrite at point.
                 (if (looking-at "\\(;;; :FILE-CREATED\\|;;; FILE-CREATED:\\)")
                     (progn
                       (end-of-line) 
                       (if (= (point) (buffer-end 1))
                           (progn (open-line 1) (forward-char 1))
                           (forward-char 1))
                       (princ (format "%s%s%s"
                                      the-file-or-dir 
                                      (if the-url (concat "\n" the-url) "")
                                      ;; We want to insert the EOF template but check anyways.
                                      (if (not (mon-file-stamp-vrfy-put-eof))
                                          (concat "\n"    *mon-default-comment-divider* "\n;;; EOF\n")
                                          (prog1 "" (mon-file-stamp-vrfy-put-eof t))))
                              (current-buffer))
                       (princ (format "%s%s" f-tstmp 
                                      (concat "\n" *mon-default-comment-divider* "\n;;; EOF\n"))
                              (current-buffer)))
                     ;; else
                     (princ 
                      (format "\n%s%s" f-tstmp (concat "\n" *mon-default-comment-divider* "\n;;; EOF\n"))
                      (current-buffer)))))
            ((or intrp insrtp t)
             (unless (= (point) (buffer-end 0)) 
               (goto-char (buffer-end 0)))
             (cond (;; We're finished. Insert the full template and go.
                    (= (buffer-end 0) (line-end-position))
                    (princ (format "%s" f-tstmp)(current-buffer))
                    (mon-file-stamp-vrfy-put-eof t))
                   (;; One or more existing `;;; ' are present. Begin checking for each.
                    (<= (line-end-position)  (buffer-end 1))
                    (let ((started-from (make-marker))
                          (cmt-delim ;;=> "^\\(;;; =\\{26,68\\}\\)"
                           (concat "\\(" 
                                   (substring *mon-default-comment-divider* 0 4)
                                   "=\\{26,68\\}\\)")))
                      (set-marker started-from (point))
                      (search-forward-regexp "\\(;;; :FILE-CREATED\\|;;; FILE-CREATED:\\)" 
                                             (line-end-position) t)
                      (cond ((> (point) started-from)
                             (progn
                               (goto-char started-from) 
                               (end-of-line) (forward-char 1)
                               (set-marker started-from (point))))
                            ((= (point) started-from)
                             (cond ((looking-at "^$")
                                    (princ fc-stmp (current-buffer))
                                    (forward-char 1)
                                    (set-marker started-from (point)))
                                   ((looking-at "^;;; [^=].*$")
                                    (open-line 1)
                                    (princ fc-stmp (current-buffer))
                                    (forward-char 1)
                                    (set-marker started-from (point)))
                                   (;; This is a screwy buffer, insert the template now.
                                    (looking-at (concat "^" cmt-delim "\n"))
                                    (princ f-tstmp (current-buffer))
                                    (mon-file-stamp-vrfy-put-eof t)
                                    (goto-char started-from) 
                                    (end-of-line)(forward-char 1)
                                    (set-marker started-from (point))))))
                      (search-forward-regexp ";;; :FILE \\(:DIRECTORY\\)?" (line-end-position) t)
                      (if (> (point) started-from)
                          (progn
                            (goto-char started-from) 
                            (end-of-line) (forward-char 1)
                            (set-marker started-from (point)))
                          (progn 
                            (princ the-file-or-dir (current-buffer))
                            (cond ((looking-at ";;; \\(:SOURCE \\((URL\\)?\\|:SEE \\((URL\\)?\\|\\((URL\\)\\)" )
                                   (open-line 1)
                                   (forward-char 1)
                                   (set-marker started-from (point)))
                                  ((looking-at (concat cmt-delim "\n"))
                                   (open-line 1)
                                   (forward-char 1)
                                   (set-marker started-from (point)))
                                  ((= (point) (line-end-position))
                                   (forward-char 1)
                                   (set-marker started-from (point))))))
                      (search-forward-regexp 
                       ";;; \\(:SOURCE \\((URL\\)?\\|:SEE \\((URL\\)?\\|\\((URL\\)\\)" 
                       (line-end-position) t)
                      (cond ;; `f-tstmp', `the-file-or-dir', and `the-url' are here. Jump to EOF.
                        ((> (point) started-from)
                         (end-of-line) (forward-char)
                         ;; Make sure we have an divider block.
                         (unless (looking-at (concat "^" cmt-delim))
                           (open-line 1) 
                           (princ *mon-default-comment-divider* (current-buffer)))
                           (mon-file-stamp-vrfy-put-eof t))
                        ;; `f-tstmp' and `the-file-or-dir' here. Insert `the-url' and jump to EOF.
                        ((search-backward-regexp "^;;; :FILE \\(:DIRECTORY\\)?" nil t)
                         (end-of-line) (forward-char 1)
                         (princ (format "%s" (if the-url (concat the-url "\n") "")) (current-buffer))
                         (unless (looking-at (concat "^" cmt-delim))
                           (open-line 1) 
                           (princ *mon-default-comment-divider* (current-buffer)))
                         (mon-file-stamp-vrfy-put-eof t))
                        ;; `f-tstmp' is here. Insert `the-file-or-dir' and `the-url', then jump to EOF.
                        ((search-backward-regexp "^\\(;;; :FILE-CREATED\\|;;; FILE-CREATED:\\)" nil t)
                         (end-of-line) (forward-char 1)
                         (princ (format "%s%s" the-file-or-dir (if the-url (concat the-url "\n") ""))
                                (current-buffer))
                         (unless (looking-at (concat "^" cmt-delim))
                           (open-line 1) 
                           (princ *mon-default-comment-divider* (current-buffer)))
                         (mon-file-stamp-vrfy-put-eof t))
                        ;; Nothing found. We shouldn't be here. Logic is screwy. :(
                        ;; Put everything now; `f-tstmp', `the-file-or-dir', `the-url'. Jump to EOF.
                        ((or (= started-from (buffer-end 0)) t)
                         (princ (format "%s" f-tstmp)(current-buffer))
                         (mon-file-stamp-vrfy-put-eof t)))))))))))
;;
;;; :NOTE Following 4(four) tests need to be performed in a separate buffer.
;;; :TEST-ME (mon-file-stamp) 
;;; :TEST-ME (mon-file-stamp nil nil "http://wikipedia.com") 
;;; :TEST-ME (mon-file-stamp t) 
;;; :TEST-ME (mon-file-stamp nil t "http://wikipedia.com")
;;
;;; :TEST-ME (mon-file-stamp t nil "http://wikipedia.com" t)
;;
;;; :TEST-ME 
;;; (save-excursion
;;;   (with-output-to-temp-buffer "*MON-TEST-MFS*"
;;;     (set-buffer "*MON-TEST-MFS*")
;;;     (display-buffer "*MON-TEST-MFS*" t)
;;;     (mon-file-stamp t)
;;;     (sit-for 3)
;;;     (mon-file-stamp t nil "http://wikipedia.com"))
;;;   (sit-for 3)
;;;   (kill-buffer  "*MON-TEST-MFS*"))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-11T14:20:25-04:00Z}#{10147} - by MON>
(defun mon-file-stamp-minibuffer ()
  "Insert a timestamp in minibuffer at point.\n
Timestamp is formatted as: YYYY-MM-DD\n
Return value of timestamp depends on value of char-before and char-after point.\n
Prepend `-' to timestamp unless char-before point is:\n\n `-' `\\' `/'\n
Append `-' to timestamp unless char-after timestamp is:\n
 null `-' `\\' `/' or `.'\n
:SEE-ALSO `mon-file-stamp-buffer-filename', `mon-get-new-buffer-w-stamp',
`mon-file-stamp', `mon-timestamp', `mon-help-time-functions',
`mon-help-mon-time-functions', `mon-help-iso-8601', `mon-help-CL-time',
`mon-help-CL-local-time'.\n►►►"
  (interactive)
  (if (minibufferp)
      (progn
        (when (get-text-property (point) 'read-only)
          (goto-char (minibuffer-prompt-end))
          (while (not (null (char-after)))
            (unless (eobp) (forward-char))))
        (let ((mb-time (format-time-string "%Y-%m-%d")))
          (if (memq (char-before) '(45 92 47))
              (princ mb-time (current-buffer))
            (princ (concat "-" mb-time) (current-buffer)))
          (unless (or (null (char-after)) 
                      (memq (char-after) '(46 45 47 92)))
            (princ "-" (current-buffer)))))
    (message (concat ":FUNCTION `mon-file-stamp-minibuffer' "
                     "-- evaluated when not minibufferp"))))
;;
;;; :TEST-ME (mon-file-stamp-minibuffer)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-11T20:13:58-04:00Z}#{10147} - by MON>
(defun mon-file-stamp-buffer-filename ()
  "Rename current-buffer and buffer-file-name with appended timestamp.\n
If buffer is visiting a file that file will be renamed on save as if by
`set-visited-file-name'.\n\nSignal an error when buffer is buffer-read-only.\n
Timestamp is formatted as:\n
 \"-YYYY-MM-DD\"\n
When buffer is visiting a file and that file has a file-name-extension timestamp
will appear before the extension.\n
:EXAMPLE\n\n(mon-file-stamp-buffer-filename-TEST)\n
:SEE-ALSO `mon-file-stamp-buffer-filename-TEST', `mon-file-stamp-minibuffer',
`mon-get-new-buffer-w-stamp', `mon-file-stamp', `mon-file-dir-attributes->plist',
`mon-help-time-functions', `mon-help-mon-time-functions', `mon-help-iso-8601',
`mon-help-CL-time', `mon-help-CL-local-time'.\n►►►"
  (interactive)
  (let* ((bfn (buffer-name)) ;; (window-buffer (minibuffer-selected-window))))
         (bfn-fl-p (buffer-file-name (get-buffer bfn))) 
         (bfn-fl-ext (if (null bfn-fl-p)
                         (progn ;; Bail on `buffer-read-only'.
                           (when (buffer-local-value 'buffer-read-only (get-buffer bfn))
                             (error (concat ":FUNCTION `mon-file-stamp-buffer-filename'"
                                            " -- won't rename the read-only buffer: %s") bfn))
                           ;; Check if we've named a buffer _with_ a file extension.
                           ;; If so grab it. Else make sure to return nil _not_ "".
                           (let ((empt-str-p (file-name-extension (buffer-name) t)))
                             (if (equal empt-str-p "") 
                                 nil 
                                 empt-str-p)))
                         ;; Ensure we get all of the file extension, ".gz" causes problems.
                         (let ((fne (file-name-extension bfn-fl-p t)))
                           (if (equal fne ".gz") ;; Extend for other extensions with `member' if necessary.
                               (concat (file-name-extension (file-name-sans-extension bfn-fl-p) t) ".gz")
                               fne))))
         (bfn-w-dir-only (if (null bfn-fl-p)
                             default-directory
                             (file-name-directory bfn-fl-p)))
          (bfn-w-no-dir  (if (null bfn-fl-p)
                            bfn
                            (file-name-nondirectory bfn-fl-p)))
         (bfn-split (if bfn-fl-ext 
                        (format "\\(-?[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-?\\|%s\\)"
                                (regexp-quote bfn-fl-ext))
                        "\\(-?[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-?\\)"))
         (bfn-new (apply 'concat 
                         `(,bfn-w-dir-only 
                           ,(car (save-match-data (split-string bfn-w-no-dir bfn-split t)))
                           ,(format-time-string "-%Y-%m-%d")
                           ,bfn-fl-ext))))
    (with-current-buffer (get-buffer bfn)
      (set-visited-file-name bfn-new nil t))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-11T20:15:00-04:00Z}#{10147} - by MON>
(defun mon-file-stamp-buffer-filename-TEST ()
  "Test function for `mon-file-stamp-buffer-filename'.\n
:SEE-ALSO `mon-file-stamp', `mon-file-stamp-minibuffer',
`mon-help-time-functions', `mon-help-mon-time-functions', `mon-help-iso-8601',
`mon-help-CL-time', `mon-help-CL-local-time'.\n►►►"
  (dolist (i `("" ".el" ".gz" 
                  ,(format-time-string "-%Y-%m-%d.el")
                  ,(format-time-string "-%Y-%m-%d.el.gz"))
           (dired-other-window temporary-file-directory))
    (let* ((mfsbft (concat "MON-FILE-STAMP-BUFFER-FILENAME-TEST" i))
           (mfsbft-tmp temporary-file-directory))
      (with-current-buffer (get-buffer-create mfsbft)
        (cd mfsbft-tmp)
        (when (equal (file-name-extension i t) ".gz")
          (write-file (concat mfsbft-tmp (buffer-name))))
        (mon-file-stamp-buffer-filename)
        (insert mfsbft)
        (if (equal i "")
            (write-file mfsbft-tmp)
            (save-buffer))
        (kill-buffer (get-buffer mfsbft))))))

;;; ==============================
;;; :CREATED <Timestamp: 2009-07-30-W31-4T20:03:12-0400Z - by MON KEY>
(defun mon-convert-ebay-time (time-string)
  "Parse an ebay-time-string encode -> decode it.\n
Used as a helper funcition with `mon-cln-ebay-time-string' to return an
timestamp with a timestring appropriate to `current-time-zone'.\n
:EXAMPLE\n\n\(mon-convert-ebay-time \"29 Jul 2009 Tuesday 11:05:27 PDT\"\)\n
:NOTE The function `encode-time' wants time values formatted as:\n
second, minute, hour, day, month, year, and optional zone\n
But, `decode-time' and `parse-time-string' both return:\n
 \(SEC MINUTE HOUR DAY MONTH YEAR DOW DST ZONE\)
 \(37  9      15   29  7     2009  3  t   -14400\)
 \(0   1       2    3  4        5  6  7   8\)\n
:SEE-ALSO `mon-format-iso-8601-time', `mon-cln-ebay-time-string',
`mon-calculate-ebay-timezone-diff', `mon-convert-ebay-time',
`mon-convert-ebay-time-mvb', `mon-help-time-functions',
`mon-help-mon-time-functions', `mon-help-iso-8601', `mon-help-CL-time',
`mon-help-CL-local-time'.\n►►►"
  (let* ((ts (parse-time-string time-string))
         (sec (nth 0 ts))               ;; <- SEC
         (min (nth 1 ts))               ;; <- MINUTE
         (hr  (nth 2 ts))               ;; <- HOUR
         (day (nth 3 ts))               ;; <- DAY
         (mon (nth 4 ts))               ;; <- MONTH
         (yr  (nth 5 ts))               ;; <- YEAR
         (dow (nth 6 ts))               ;; <- DOW
         (dst (nth 7 ts))               ;; <- DST
         (tz  (nth 8 ts))               ;; <- TZ
         (et)) ;; `(,sec ,min ,hr ,day ,mon ,yr ,dow ,dst ,tz))
    (setq et (encode-time sec min hr day mon yr dow tz))
    ;; (format-time-string "<Timestamp: %Y-%m-%d-W%V-%uT%T%zZ - by Ebay>" et)))
    (concat "<Timestamp: "
            (mon-format-iso-8601-time et)
            " - by Ebay>")))
;;
;;; :TEST-ME (mon-convert-ebay-time "29 Jul 2009 Tuesday 11:05:27 PDT")

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-04-W32-2T11:58:14-0400Z - by MON KEY>
(defun mon-convert-ebay-time-mvb (time-string)
  "Return TIME-STRING after parsing it with `mon-format-iso-8601-time'.\n
:EXAMPLE\n\n\(mon-convert-ebay-time-mvb \)\n
:SEE-ALSO `mon-convert-ebay-time', `parse-time-string', `encode-time',
`mon-help-time-functions', `mon-help-mon-time-functions', `mon-help-iso-8601',
`mon-help-CL-time', `mon-help-CL-local-time'\n►►►"
  (multiple-value-bind 
      (sec min hr day mon yr dow dst tz) 
      (parse-time-string time-string)
    ;; (format-time-string
    ;;  "<Timestamp: %Y-%m-%d-W%V-%uT%T%zZ - by Ebay>"
    ;;  (encode-time sec min hr day mon yr dow tz))))
    (concat "<Timestamp: " 
            (mon-format-iso-8601-time (encode-time sec min hr day mon yr dow tz))
            " - by Ebay>")))
;;
;;; :TEST-ME (mon-convert-ebay-time-mvb "29 Jul 2009 Tuesday 11:05:27 PDT")

;;; ==============================
;;; :FIXME Call to `mon-convert-ebay-time' at botomm of fncn is recieving bogus
;;;        timestrings. adjsut regexps.
;;; (mon-date-stamp :as-string t)""
;;; :TODO Consider providing an optional arg to return the stamp without the call to
;;; `mon-convert-ebay-time'???
;;; :CREATED <Timestamp: Wednesday July 29, 2009 @ 06:36.54 PM - by MON>
(defun mon-cln-ebay-time-string (&optional time-string start end insrtp intrp)
  "Clean an ebay timestring and process with `mon-convert-ebay-time'.\n
Called programmatically processes and eBay TIME-STRING of form:\n
\(Aug 07, 200913:52:24 PDT\) <-style1, from eBay webpage i.e. copy/paste
Jul-29 11:05                 <-style2, from eBay listing manager
Aug-10-09 09:16:14 PDT       <-style3, from eBay post listing email confirm
replacing string with a canonically formatted timestamp.
When TIME-STRING has form of style1 following occur:
 o '(Mmm' -> Month
 o parens are removed from head and tail of string;
 o Comma after DD is removed; 
 o Whitespace is inserted after YYYY.\n
When TIME-STRING has form of style2 following occur:
 o  'Mmm-' -> Month  
 o  Current year is concatenated to string;
 o  An eBay time-zone is inserted, either PDT or PST
    This is calculated against `current-time-zone' and may return
    incorrect results if your environment's timezone doesn't track 
    daylight-savings-time in a similiar manner as the U.S. (most should).\n
When TIME-STRING has form of style3 following occur:
 o 'Mmm-' -> Month  
 o Hyphenation removed from 'Mmm-DD-YY'
 o Current year is converted from YY -> YYYY\n
For all 3\(three\) styles returns an extended timestamp formatted to allow
further processing according to ISO 8601 spec. The timestamp provides conversion of
eBay PDT or PST timezone to local time \(not everyone lives on West Coast of 
United States!\)
When optional arg INSRTP is non-nil converted timestring is inserted at point.
Called intereactively processes the ebay time-string in region as per above.
Replaces existing time-string in region with converted form.\n
:EXAMPLE
\(mon-cln-ebay-time-string \"\(Jul 29, 200911:05 PDT\)   ; <- style1\n
\(mon-cln-ebay-time-string \"Jul-29 11:05\"\)            ; <- style2\n
\(mon-cln-ebay-time-string \"Aug-10-09 09:16:14 PDT\")   ; <- style3\n
:SEE-ALSO: `mon-cln-ebay-time-string', `mon-calculate-ebay-timezone-diff',
`mon-convert-ebay-time', `mon-convert-ebay-time-mvb',
`*regexp-clean-ebay-time-chars*', `*regexp-clean-ebay-month->canonical-style1*',
`*regexp-clean-ebay-month->canonical-style2*',
`*regexp-clean-ebay-month->canonical-style3*', `mon-help-time-functions',
`mon-help-mon-time-functions', `mon-help-iso-8601', `mon-help-CL-time',
`mon-help-CL-local-time'.\n►►►"
  (interactive "\i\nr\ni\np")
  (let ((cln-ebay *regexp-clean-ebay-month->canonical-style1*)
        (curr-yr (mon-get-current-year))
        (put-tz (if (nth 7 (decode-time)) "PDT" "PST"))
        (start-marker (make-marker))
        (end-marker (make-marker))        
        (rep-str)
        (found-match)
        (match-style)
        (rep-match)
        (frn-yr)
        (bak-yr))
    (setq frn-yr (concat " " curr-yr))
    (setq bak-yr (concat frn-yr " "))
    (cond ((and start end) ;;(or intrp insrtp)
           (set-marker start-marker start)
           (set-marker end-marker end)
           (setq rep-str (buffer-substring-no-properties start-marker end-marker)))
          (time-string (setq rep-str time-string)))
    (mapc (lambda (eb-mon)
            (let ((fnd-mon (car eb-mon))
                  (rep-mon (cadr eb-mon)))
              (when (string-match fnd-mon rep-str)
                (setq found-match (match-string 0 rep-str))
                (setq rep-match rep-mon)
                (setq match-style 1) ;;\(Aug 07, 200913:52:24 PDT\) <- style1 
                (setq rep-str (replace-regexp-in-string  fnd-mon rep-mon rep-str)))))
          cln-ebay)
    (unless match-style
      (setq cln-ebay *regexp-clean-ebay-month->canonical-style3*)
      (mapc #'(lambda (eb-mon)
                (let ((fnd-mon (car eb-mon))
                      (rep-mon (cadr eb-mon))
                      ;; :NOTE Use `edmacro-subseq' instead.
                      ;; (sub-cent (concat " " (if (intern-soft "cl::subseq")
                      ;;                           (cl::subseq (mon-get-current-year) 0 2)
                      ;;                         (subseq (mon-get-current-year) 0 2))))
                      (sub-cent (concat " " (edmacro-subseq (mon-get-current-year) 0 2)))
                      rep-str-hndl)
                  (when (string-match fnd-mon rep-str)
                    (setq found-match (match-string 0 rep-str))
                    (setq rep-match rep-mon)
                    (setq match-style 3) ;; "Aug-10-09 09:16:14 PDT" <- style3
                    (setq rep-str-hndl
                          (concat rep-mon                ;2: Mmm-  -> "Month "
                                  (match-string 3 rep-str) ;3: DD
                                  (concat 
                                   sub-cent 
                                   (match-string 5 rep-str)) ;5: YY -> " YYYY"
                                  (match-string 6 rep-str)   ;6: " HH:MM:SS "   
                                  (match-string 7 rep-str))) ;7: ZON
                    (setq rep-str rep-str-hndl))))
            cln-ebay))
    (unless match-style
      (setq cln-ebay *regexp-clean-ebay-month->canonical-style2*)
      (mapc #'(lambda (eb-mon)
                (let ((fnd-mon (car eb-mon))
                      (rep-mon (cadr eb-mon)))
                  (when (string-match fnd-mon rep-str)
                    (setq found-match (match-string 0 rep-str))
                    (setq rep-match rep-mon)
                    (setq match-style 2) ;; Jul-29 11:05 <- style2
                    (setq rep-str (replace-regexp-in-string  fnd-mon rep-mon rep-str))))
                ) cln-ebay))
    (if found-match
        (progn
          (let (pop-list)
            (setq pop-list *regexp-clean-ebay-time-chars*)
            (while pop-list
              (let* ((head-char (pop pop-list))
                     (subst (car head-char))
                     (w/ (cadr head-char)))
                (subst-char-in-string subst w/ rep-str t))))
          (cond ((= match-style 1) ;; (August 07 200913:52:24 PDT\) <- style1 Fix year whitespace
                 (setq rep-str (replace-regexp-in-string frn-yr bak-yr rep-str)))
                ((= match-style 2) ;; July 29 11:05 <- style2 Add year add timezone 
                 (setq rep-str 
                       (replace-regexp-in-string
                        (concat  "\\("                  ;1
                                 "\\([A-Z]*\\)"         ;2 -> MONTH
                                 "\\( \\)"              ;3 -> wspc after MONTH 
                                 "\\([0-9]\\{2,2\\}\\)" ;4 -> DAY
                                 "\\( \\)"              ;5 -> wspc after DAY
                                 "\\([0-9]\\{2,2\\}:[0-9]\\{2,2\\}\\)" ;6 HH:MM
                                 "\\)") bak-yr rep-str nil nil 5))
                 (setq rep-str (concat rep-str " " put-tz)))
                ((= match-style 3)
                 rep-str) ;; "August 10 09 09:16:14 PDT" <- style3 fix year -> NN09
                (t (message "Could Not match an ebay timestamp for %s" time-string)))
          (setq found-match (format "Found: %s\nReplaced: %s\nWith: %s " 
                                    found-match
                                    (if time-string
                                        time-string
                                      (buffer-substring-no-properties start-marker end-marker))
                                    rep-match)))
      (setq found-match (format "Couldn't find match for: %s" rep-str)))
    ;; :NOTE Consider providing an optional arg to return the stamp without the
    ;;       call to `mon-convert-ebay-time'???
    (cond ((and start end (or insrtp intrp))
           (progn    
             (delete-and-extract-region start end)
             (goto-char start-marker) 
             (insert (mon-convert-ebay-time rep-str))
             (message found-match)))
          ((and time-string insrtp)
           (insert (mon-convert-ebay-time rep-str)))
          (t (mon-convert-ebay-time rep-str)))))
;;
;;; :TEST-ME (mon-cln-ebay-time-string "(Jul 29, 200911:05 PDT)")
;;; :TEST-ME (mon-cln-ebay-time-string "(Jul 29, 200911:05 PDT)"  nil nil t)
;;; :TEST-ME (mon-cln-ebay-time-string "Jul-29 11:05")
;;; :TEST-ME (mon-cln-ebay-time-string "Jul-29 11:05" nil nil t)
;;; :TEST-ME (mon-cln-ebay-time-string "Jul-29-09 11:05:14 PDT")
;;; :TEST-ME (mon-cln-ebay-time-string "Jul-29-09 11:05:14 PDT" nil nil t)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: Tuesday July 28, 2009 @ 08:28.46 PM - by MON KEY>
(defun mon-calculate-ebay-timezone-diff (ebay-time-string)
  "Return hour difference between ebay-time (PDT or PST) and `current-time-zone'.\n
EBAY-TIME-STRING should have the format:\n
 \"28 July 2009 17:08:26 PDT\"\n
 --------------------------------------
                 GMT  GMT-DT  PST   PDT
 US/CA Pacific   -08   -07    +00   +00
 US/CA Central   -06   -05    +02   +02
 US/CA Eastern   -05   -04    +03   +03
 --------------------------------------\n
 -----------------------------
 Zone   SECS-From     GMT-DIFF
 EST    -> -18000     -> 5
 EDT    -> -14400     -> 4
 PST    -> -28800     -> 8 
 PDT    -> -25200     -> 7
 -----------------------------\n
:NOTE Starting in 2007, most of the United States and Canada observe DST from
the second Sunday in March to the first Sunday in November, almost two-thirds of
the year. The 2007 U.S. change was part of the Energy Policy Act of 2005;
previously, from 1987 through 2006, the start and end dates were the first
Sunday in April and the last Sunday in October, and Congress retains the right
to go back to the previous dates now that an energy-consumption study has been
done.\n
:SEE \(URL `http://en.wikipedia.org/wiki/Daylight_saving_time')
:SEE \(URL `http://isotc.iso.org/livelink/livelink/4021199/ISO_8601_2004_E.zip?func=doc.Fetch&nodeid=4021199')\n
:SEE-ALSO `mon-cln-ebay-time-string', `mon-convert-ebay-time',
`mon-convert-ebay-time-mvb', `current-time-zone', `parse-time-string'
`mon-help-time-functions', `mon-help-mon-time-functions', `mon-help-iso-8601',
`mon-help-CL-time', `mon-help-CL-local-time'.\n►►►"
  (let ((ets ebay-time-string))
    (/ (- (abs (car (last (parse-time-string ets))))
          (abs (car (current-time-zone))))
       3600)))
;;
;;; :TEST-ME (mon-calculate-ebay-timezone-diff "28 July 2009 17:08:26 PST") ;=> 4
;;; :TEST-ME (mon-calculate-ebay-timezone-diff "28 July 2009 17:08:26 PDT") ;=> 3
;;; :TEST-ME (mon-calculate-ebay-timezone-diff "28 July 2009 17:08:26 EST") ;=> 1
;;; :TEST-ME (mon-calculate-ebay-timezone-diff "28 July 2009 17:08:26 EDT") ;=> 0

(eval-when-compile (require 'warnings))
;;; ==============================
;;; :SEE (URL `http://lists.gnu.org/archive/html/emacs-devel/2010-08/msg00317.html')
;;; :SEE (URL `http://lists.gnu.org/archive/html/emacs-devel/2010-08/msg00372.html')
;;; :CREATED <Timestamp: #{2010-08-08T18:59:25-04:00Z}#{10317} - by MON>
;;;###autoload
(defun calendar-goto-doomsday ()
  "Reposition calendar window to next known doomsday warn of pending doom.
:EXAMPLE\n\n\(calendar-goto-doomsday\)\n
:SEE-ALSO `mon-cln-ebay-time-string', `mon-convert-ebay-time',
`mon-convert-ebay-time-mvb', `current-time-zone', `parse-time-string'
`mon-help-time-functions', `mon-help-mon-time-functions', `mon-help-iso-8601',
`mon-help-CL-time', `mon-help-CL-local-time'.\n►►►"
  (interactive)
  (let* ((cnt-dwn (format-seconds "%Y %D %H %M and %S"
                                  (1- most-positive-fixnum)))
         (cnt-from (format-time-string 
                    "%B %d, day %j \(a %A\) in week %U of %Y"))
         (w-buffer "*Warning-Doomsday-Pending*")
         (tm-left  (format-seconds "%y" (1- most-positive-fixnum)))
         (w-type '(Husserlian-Temporal-Disconnect . alarm))
         (warning-minimum-level (cdr (assq (cdr w-type) warning-level-aliases)))
         (warning-prefix-function 
          #'(lambda (lvl lvl-inf)
              (list lvl "DOOMSDAY-ALERT!%s:" 
                    #'(lambda () 
                        ;;(font-lock-fontify-buffer)
                        (save-excursion
                          (goto-char (buffer-end 0))
                          (when 
                              (add-text-properties
                               (point)
                               (or (and (search-forward-regexp 
                                         "DOOMSDAY-ALERT!" (line-end-position) t)
                                        (point))
                                   (point))
                               '(face font-lock-warning-face fontified t))
                            (add-text-properties 
                             (progn (forward-char 2) (point))
                             (or (and (> (skip-chars-forward "^:") 0)(1- (point)))
                                 (point))
                             '(face font-lock-preprocessor-face fontified t))))))))
         (warning-fill-prefix (concat "\n" (make-string 17 32)))
         (w-msg (concat warning-fill-prefix 
                        "By Emacs' best calculations (all 29 bits worth),"
                        warning-fill-prefix 
                        "passage of time will cease in approximately:"
                        warning-fill-prefix cnt-dwn " from now." 
                        warning-fill-prefix "Today is " cnt-from "..."
                        warning-fill-prefix 
                        "Pragmatists should now take a moment to update their "
                        tm-left " year plan."))         
         warning-suppress-types
         warning-suppress-log-types
         warning-series)
    (with-current-buffer (get-buffer-create w-buffer)
      (erase-buffer)
      (display-warning (car w-type) w-msg (cdr w-type) w-buffer))
    "Danger! Danger!"))
;;
;; :TEST-ME (calendar-goto-doomsday)

;;; ==============================
(provide 'mon-time-utils)
;;; ==============================

;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; coding: utf-8
;; End:

;;; ==============================
;;; mon-time-utils.el ends here
;;; EOF

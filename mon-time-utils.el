;;; this is mon-time-utils.el
;;; ================================================================
;;; DESCRIPTION: 
;;; MON utilities for working with time. 
;;; Routines for converting from 'eBay official time' to 'YOUR Official Time ®'
;;;
;;; FUNCTIONS:►►►
;;; `mon-get-current-year' `mon-format-iso-8601-time'
;;; `mon-file-older-than-file-p' `mon-get-file-mod-times'
;;; `mon-conv-time-flt-pnt' `mon-comp-times-flt-pnt'
;;; `mon-accessed-time-stamp' `mon-stamp' `mon-accessed-stamp'
;;; `mon-lisp-stamp' `mon-convert-ebay-time' `mon-convert-ebay-time-mvb'
;;; `mon-cln-ebay-time-string' `mon-calculate-ebay-timezone-diff'
;;; `mon-today', `mon-file-stamp-vrfy-put-eof', `mon-comment-divider-w-len'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; METHODS
;;;
;;; CLASSES:
;;; 
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;; `*mon-default-comment-divider*', `*mon-default-comment-start*'
;;; `*mon-timestamp-cond-alist*' 
;;;
;;; ALIASES/ADVISED/SUBST'D:
;;;
;;; MOVED:
;;; `mon-comp-times-flt-pnt'     <- ./mon-dir-utils.el
;;; `mon-conv-time-flt-pnt'      <- ./mon-dir-utils.el
;;; `mon-file-older-than-file-p' <- ./mon-dir-utils.el
;;; `mon-get-file-mod-times'     <- ./mon-dir-utils.el
;;; `mon-accessed-stamp'         <- ./mon-insertion-utils.el
;;; `mon-stamp'                  <- ./mon-insertion-utils.el
;;; `mon-timestamp'              <- ./mon-insertion-utils.el
;;; `mon-accessed-time-stamp'    <- ./mon-insertion-utils.el
;;; `*mon-timestamp-cond-alist*' <- ./mon-dir-locals-alist.el
;;;
;;; DEPRECATED:
;;; `mon-accessed-stamp' -> 
;;;
;;; RENAMED:
;;;
;;; REQUIRES:
;;; mon-regexp-symbols.el
;;; |-> `regexp-clean-ebay-month2canonical-style1'
;;; |-> `regexp-clean-ebay-month2canonical-style2'
;;; |-> `regexp-clean-ebay-month2canonical-style3'
;;;
;;; `mon-convert-ebay-time-mvb' cl.el
;;;                             |-> `multiple-value-bind' 
;;;
;;; `mon-today' mon-utils.el 
;;;             |-> `mon-string-to-symbol'
;;; 
;;; `mon-timestamp'-> mon-dir-utils.el
;;;                   |-> `mon-get-buffers-parent-dir'
;;;                   |-> `mon-buffer-written-p'
;;;                                     
;;; The variables and constants:
;;; `IS-MON-P', `IS-MON-P-W32', `IS-MON-P-GNU' 
;;; `IS-BUG-P',`IS-BUG-P-REMOTE'
;;; 
;;; Are provided in:
;;; :FILE mon-default-loads.el
;;; :LINK (URL `http://www.emacswiki.org/emacs/mon-default-loads.el')
;;;
;;; The value of the above vars and const are returned in lieu of:
;;; :FUNCTION `mon-user-name-conditionals' which is defined in:
;;; :FILE mon-site-local-defaults.el
;;; :LINK (URL `http://www.emacswiki.org/emacs/mon-site-local-defaults.el')
;;
;;; That package also provides the constants: `*MON-NAME*', `*BUG-NAME*'
;;; These hold alists of system conditional key value pairs.
;;; These will need to be present on your system for this pkg to work properly.
;;; The links to the files above provide filler slots that you can modify to 
;;; fit your environment. I provide these as a solution for dealing with
;;; defcustom silliness...
;;; 
;;; TODO:
;;; Consider providing an optional arg `mon-cln-ebay-time-string'
;;; to return the stamp without the call to `mon-convert-ebay-time'
;;;
;;; NOTES:
;;; Originally motivated by a distaste for Ebay's promotion of EbayTime as
;;; `official' time - Official my ass! 
;;; :SEE (URL `http://viv.ebay.com/ws/eBayISAPI.dll?EbayTime')
;;;
;;; There are at least three styles of ebay timestamp. 
;;; Apparently each is 'official'... 
;;; 
;;; Clean up ebay timestrings three different ways. 
;;; * Style1 from eBay webpage i.e. copy/paste
;;; * Style2 from eBay listing manager
;;; * Style3 from eBay post listing email confirmations
;;;
;;; None of these 3(three) styles can be processed with 'parse-time-string'
;;; without some cleaning. 
;;;
;;; The function `mon-cln-ebay-time-string' returns an extended timestamp 
;;; formatted to allow further processing according to ISO 8601 spec. 
;;;
;;; The three ebay timestring styles are cleaned as follows:
;;;
;;; (Aug 07, 200913:52:24 PDT) <-style1
;;; <Timestamp: #{2009-08-07T16:52:24-04:00Z}#{09325} - by Ebay>
;;;
;;; Jul-29 11:05                 <-style2
;;; -> <Timestamp: #{2009-07-29T14:05:00-04:00Z}#{09313} - by Ebay>
;;;
;;; Aug-10-09 09:16:14 PDT       <-style3
;;; -> <Timestamp: #{2009-08-06T21:17:10-04:00Z}#{09325} - by Ebay>
;;;
;;; If you don't like the default timestamp adjust 
;;; format-time-string in tail of `mon-convert-ebay-time'
;;;
;;; The form: `(put-tz (if (nth 7 (decode-time)) "PDT" "PST"))'
;;; in `mon-cln-ebay-time-string' assumes your Daylight-savings flag 
;;; is in synch with the Northern Hemisphere. 
;;; If it is Summer in Califonria and Winter where you are you may want to
;;; swap PDT <-> PST
;;;
;;; ==============================
;;; The timestamps generate by functions herein are of the form:
;;;
;;; <Timestamp: #{YYYY-MM-DDTHH:MM:SS+/-HH:MMZ}#{yyWwD} - by NAME>
;;; <Timestamp: #{2009-08-06T21:17:10-04:00Z}#{09325} - by Ebay>
;;;
;;; This format is not in keeping with RFC-3339 take on ISO-8601 
;;; Specifically, it disregards the discussion presented in 
;;; Section 5.4. Redundant Information:
;;;
;;;   "If a date/time format includes redundant information, that introduces
;;;    the possibility that the redundant information will not correlate.
;;;    For example, including the day of the week in a date/time format
;;;    introduces the possibility that the day of week is incorrect but the
;;;    date is correct, or vice versa.  Since it is not difficult to compute
;;;    the day of week from a date (see Appendix B), the day of week should
;;;    not be included in a date/time format."
;;;
;;; We do not share this position. While it may be true that redundancies can
;;; present errors whereby a YYYY-Www-D date calculation doesn't jibe with the
;;; YYYY-MM-DD it is incorrect to assume then that we are somehow better off
;;; omitting the Www-D data. This is bogus reasoning as it assumes that the
;;; YYYY-MM-DD date stamp is of itself somehow MORE correct than a YYYY-Www-D
;;; stamp. It is not. Rather than assume that one or another of these data will
;;; become compromised it is safer to assume either that both are potentially
;;; compromised or that both are correct. In either case provision of this
;;; 'redundancy' allows for a check against what would otherwise be
;;; uncorroborated time. Moreover, so long as the information is presented
;;; according to the standards specification it is quite legitimate to simply
;;; omit the redundancy via heuristics (regexps for example). Doing so allows us
;;; the ability to calculate multiple different types of date intervals in an
;;; adhoc 'loosey' manner whilst retaining the ability to perform more targeted
;;; calculations where necessary. This can be esp. important/useful for
;;; indicating and reasoning about time/date ranges by field as opposed by
;;; duration. E.g. "Show me all date stamps on a Tuesday." whether the date is
;;; _correct_ may be completely irrelevant to the query and it is certainly the
;;; case that the heuristics/calculations needed for locating such stamps under
;;; our regime will be quite a bit quicker and more intelligible than those
;;; which would require numerous encode->decode->calculate->encode operations
;;; just to find what may or may not be a Tuesday... 
;;;
;;; SNIPPETS:
;;; Other date & time related functions:
;;; `mon-num-to-month'             -> ./naf-mode-replacements.el
;;; `mon-abr-to-month'             -> ./naf-mode-replacements.el
;;; `mon-month-to-num'             -> ./naf-mode-replacements.el
;;; `mon-num-to-month-whitespace'  -> ./naf-mode-replacements.el
;;; `mon-ital-date-to-eng'         -> ./naf-mode-replacements.el
;;; `mon-defranc-dates'            -> ./naf-mode-replacements.el
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-time-utils.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-08-06} - by MON KEY>
;;;
;;; FILE-CREATED:
;;; <Timestamp: Wednesday July 29, 2009 @ 07:46.54 PM - by MON KEY>
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

;;;`mon-convert-ebay-time-mvb' -> `multiple-value-bind'
(eval-when (compile load eval) 
  (require 'cl))

;;; ==============================
(require 'mon-regexp-symbols)
;;
(eval-when 
    (unless (featurep 'mon-utils)
      (require 'mon-dir-utils)
      (message (concat 
                "This :PACKAGE `mon-time-uitls' wants the :FUNCTION \n"
                "`mon-string-to-symbol' from :PACKAGE `mon-utils' \n"
                "Had that that package been loaded already, the :FUNCTIONS \n"
                "`mon-get-buffers-parent-dir', `mon-buffer-written-p' \n"
                "from :PACKAGE `mon-dir-utils' would have been loaded already \n" 
                "In any event this :PACKAGE `mon-time-uitls' " 
                ":REQUIRES `mon-dir-utils'\nSo, we are requiring it now."))))
                

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-24T14:12:12-04:00Z}#{09436} - by MON KEY>
(defvar *mon-default-comment-start* ";;; "
 "*Comment prefix for `mon-comment-divider-w-len'.
 This is a cheap around so we don't have to deal with `comment-start' with 
mon-comment-* functions which might rely on or calculate a string/substring
inidex per the value of this var.
:EXAMPLE\n*mon-default-comment-start*\n
\(let \(\(*mon-default-comment-start* \"%% \"\)\)
  *mon-default-comment-start*\)\n
:CALLED-BY `mon-comment-divider-w-len', `comment-divider-to-col'
:SEE-ALSO `*mon-default-comment-divider*'.\n►►►")
;;
;;; :TEST-ME *mon-default-comment-start*

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-24T12:51:14-04:00Z}#{09436} - by MON KEY>
(defun mon-comment-divider-w-len (len)
  "Return a comment-divider with LEN number of `=' prefixed with 
value of `*mon-default-comment-start*'.\n
:EXAMPLE\n(mon-comment-divider-w-len 30)\n(mon-comment-divider-w-len 40)\n
:SEE-ALSO `*mon-default-comment-divider*', `comment-divider',
`mon-lisp-comment-to-col',`comment-divider-to-col'.\n►►►"
  (concat *mon-default-comment-start* (make-string len 61)))
;;
;;; :TEST-ME (mon-comment-divider-w-len 0)
;;; :TEST-ME (mon-comment-divider-w-len 30)
;;; :TEST-ME (mon-comment-divider-w-len 40)
;;; :TEST-ME  (let ((*mon-default-comment-start* "%% "))
;;;             (mon-comment-divider-w-len 30))         

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-24T12:07:10-04:00Z}#{09436} - by MON KEY>
(defvar *mon-default-comment-divider* (mon-comment-divider-w-len 30)
  "*Preferred comment-divider for lisp source sectioning.
:CALLED-BY `comment-divider', `comment-divider-to-col'
:SEE-ALSO `mon-comment-divider-w-len', `comment-divider-to-col'\n►►►")
;;
;;; :TEST-ME *mon-default-comment-divider*
;;;(progn (makunbound '*mon-default-comment-divider*)
;;;       (unintern '*mon-default-comment-divider*))

;;; ==============================
;;; :CREATED <Timestamp: Saturday July 18, 2009 @ 11:59.08 AM - by MON KEY>
(defvar *mon-timestamp-cond-alist* nil
  "*Alist of filenames which get alternative timestamp name strings.
Value should be a string without whitespace, delemters or punctuation these will
get added by the calling function according to heuristics there.\n
Useful KEY VALUE pairs might include:
 '\(\(\"Monkey-Related.fname\"  \"MON KEY\"\)
 \(\"Mon-Related.fname\"  \"MON\"\)
 \(\"Mon-Related.fname\"  \"Mon\"\)
 \(\"Mon-Laptop.fname\"  \"Mon-Laptop\"\)
 \(\"Bug-Related.fname\"  \"Bug\"\)\)\n
Useful for file based conditional timestamping and obfuscationgs files/source posted
to Internet.
:SEE-ALSO `mon-timestamp', `mon-accessed-time-stamp',`mon-accessed-stamp'.\n►►►")
;;
(when (not (bound-and-true-p *mon-timestamp-cond-alist*))
  (setq *mon-timestamp-cond-alist*
        '(("emacs-load-files" "MON KEY")
          ("naf-mode" "MON KEY")
          ("mon-time-utils.el" "MON KEY"))))
;;
;;; :TEST-ME  *mon-timestamp-cond-alist*
;;; :TEST-ME (assoc "reference-sheet-help-utils.el" *mon-timestamp-cond-alist*)
;;;(progn (makunbound '*mon-timestamp-cond-alist*)
;;;       (unintern '*mon-timestamp-cond-alist*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-23T20:57:47-04:00Z}#{09436} - by MON KEY>
(defun* mon-today (&key as-string as-symbol as-list-str 
                        as-list-num as-vec-str as-vec-num)
  "Return today's date as YYYY-MM-DD
When keyword AS-STRING is non-nil return date as a string.
When keyword AS-SYMBOL is non-nil return date as a symbol.
When keyword AS-LIST-STR is non-nil return date as a list of three strings.
When keyword AS-LIST-NUM is non-nil return date as a list of three numbers.
When keyword AS-VEC-STR is non-nil return date as a vector of three strings.
When keyword AS-VEC-NUM is non-nil return date as a vector of three numbers.
:EXAMPLE\n\(mon-today :as-string t\)\n\(mon-today :as-symbol t\)  
\(mon-today :as-list-str t\)\n\(mon-today :as-list-num t\)
\(mon-today :as-vec-str t\)\(mon-today :as-vec-num t\)
:SEE-ALSO `mon-get-current-year', `mon-format-iso-8601-time'.\n►►►"
  (let ((2day (format-time-string "%Y-%m-%d")))
    (cond (as-string 2day)
          (as-symbol 
           (eval-when (compile load eval)
             (if (fboundp 'mon-string-to-symbol)
                 (mon-string-to-symbol 2day)
                 (car (read-from-string str 2day)))))
          (as-list-str  `(,(substring 2day 0 4)
                           ,(substring 2day 5 7)
                           ,(substring 2day 8 10)))
          (as-list-num  (mapcar 'string-to-number
                                (mon-today :as-list-str t)))
          (as-vec-str (apply 'vector (mon-today :as-list-str t)))
          (as-vec-num (apply 'vector (mon-today :as-list-num t))))))
;;
;;; :TEST-ME (mon-today :as-string t)
;;; :TEST-ME (mon-today :as-symbol t)
;;; :TEST-ME (mon-today :as-list-str t)
;;; :TEST-ME (mon-today :as-list-num t)
;;; :TEST-ME (mon-today :as-vec-str t)
;;; :TEST-ME (mon-today :as-vec-num t)

;;; ==============================
;;; <Timestamp: Wednesday July 29, 2009 @ 06:24.19 PM - by MON KEY>
(defun mon-get-current-year (&optional insertp intrp)
  "Return or insert current year at point.\n
:SEE-ALSO `mon-today', `mon-format-iso-8601-time'.\n►►►"
  (interactive "i\np")
  (let (cy)
    (setq cy (format "%s" (nth 5 (decode-time (current-time)))))
    (if (or insertp intrp)
        (princ cy (current-buffer))
      cy)))
;;
;;; :TEST-ME (mon-get-current-year) 
;;; :TEST-ME (mon-get-current-year t)
;;; :TEST-ME (call-interactively 'mon-get-current-year)

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-06-W32-4T14:55:01-0400Z - by MON KEY>
;;; :COURTESY Edward O'Connor <hober0@gmail.com> 
;;; SUBJECT: Re: Can `format-time-string' produce full/extended ISO 8601 times?
;;; LIST: emacs-devel@gnu.org
;;; DATE: 2009-08-06-W32-4T14:51:02-0400Z
(defun mon-format-iso-8601-time (&optional time insertp intrp)
  "Returns ISO-8601 compliant timestring in 'Extended' format.
:EXAMPLE\n\(mon-format-iso-8601-time\)
=> #{2009-08-06T21:27:13-04:00Z}#{09325}
NOTE: `#{' is a reserved user dispatching macro char in CL
When optional arg TIME is non-nil, it should have a format suitable for 
processing with `format-time-string'. 
When INSERTP is non-nil or called interactively insert timestring at point.
Vanilla `format-time-string' returns 'UTC offset/ZONE' in ISO-8601 'Basic format:\n
 +/-time-hourtime-minute ->  +/- hhmm  -> \"-0500\"
The standard's grammar spec is unclear w/re time-numoffset, and Emacs'
has interpreted this as permiting mixtures of Extended/Basic format e.g.:\n
 time-numoffset    = \(\"+\" / \"-\"\) time-hour [[\":\"] time-minute]\n
where the \":\" delimiter has been deemed 'optional' to compliance and thereby
ommitted.\n
The current function extends Emacs' interpretation by colon delimiting the 
time-numoffset:
 +/-time-hour:time-minute ->  +/-hh:mm  -> \"-05:00\"\n
Additionally, the \"Zulu\" or trailing Z of ISO-8601/rfc3339 appended to the UTC.\n
:SEE
\(URL `http://isotc.iso.org/livelink/livelink/4021199/ISO_8601_2004_E.zip?func=doc.Fetch&nodeid=4021199')
\(URL `http://www.ietf.org/rfc/rfc3339.txt') ;Date and Time on the Internet.
\(URL `http://www.w3.org/TR/NOTE-datetime') ;W3C Specification about UTC Date and Time.
\(URL `http://en.wikipedia.org/wiki/ISO-8601').
\(URL `http://en.wikipedia.org/wiki/ISO_8601_usage').\n►►►"
  (interactive "i\ni\np")
  (let* ((colon-z-stamp 
          ;;(format-time-string "%Y-%m-%d-W%V-%uT%T%zZ" time))
          ;;(format-time-string "%Y-%m-%d-W%V-%uT%T%zZ" time))
          ;; NOTE: `#{' is a reserved user dispatching macro char in CL
           (format-time-string"#{%Y-%m-%dT%T%zZ}#{%y%V%u}" time)) 
         (iso-8601-colon-stamp  
          (format "%s:%s%s"  ;"%s:%s" 
                  (substring colon-z-stamp  0 -12) ;0 -3) 
                  (substring colon-z-stamp 24 28)
                  (substring colon-z-stamp 28) ;-3)
                  )))
    (if (or insertp intrp)
        (insert iso-8601-colon-stamp)
      iso-8601-colon-stamp)))
;;
;;; :TEST-ME (mon-format-iso-8601-time)
;;; :TEST-ME (mon-format-iso-8601-time nil t)
;;; :TEST-ME (call-interactively 'mon-format-iso-8601-time)

;;; ==============================
;;; Why this isn't already a default function I'll never know...
;;; :CREATED <Timestamp: Wednesday June 03, 2009 @ 08:07.05 PM - by MON KEY>
(defun mon-file-older-than-file-p (file1 file2)
  "t when FILE1 is older than FILE2.\n
:SEE-ALSO `file-newer-than-file-p'.\n►►►"
  (let ((fepf1 (if (file-exists-p file1) t (error (format "file does not exist: %s" file1))))
	(fepf2 (if (file-exists-p file1) t (error (format "file does not exist: %s" file1)))))
    (not (file-newer-than-file-p file2 file2))))

;;; ==============================
;;; :CREATED <Timestamp: Friday May 15, 2009 @ 01:16.43 PM - by MON KEY>
(defun mon-get-file-mod-times (file-or-dir)
  "Return formatted string for file FILE-OR-DIR's modification and accessed time.
Formatted string also informs if FILE-OR-DIR is a dir.\n
:EXAMPLE\n\(mon-get-file-mod-times mon-user-emacsd\)\n
\"file is directory: last-modified: Tue May 19 19:37:14 2009 last-accessed: Wed May 20 10:27:44 2009\"\n
:SEE-ALSO `file-newer-than-file-p', `file-attributes', `current-time', 
`decode-time', `encode-time', `with-decoded-time-value' `encode-time-value',
`time-subtract', `current-time-string', `format-time-string',`format-seconds', 
`float-time'.\n►►►"
  (let* ((get-attr (file-attributes file-or-dir))
	 (last-acc (nth 4 get-attr))
	 (last-mod (nth 5 get-attr))
	 (last-modified (current-time-string last-mod))
	 (last-accessed (current-time-string last-acc))
	 (f-type (if (nth 0 get-attr)
		     "is directory"
		   "not directory")))
    (format "file %s: last-modified: %s last-accessed: %s" f-type last-modified last-accessed)))
;;
;;; :TEST-ME (mon-get-file-mod-times mon-user-emacsd)
;;; :TEST-ME (mon-get-file-mod-times "u:/NEFS_PHOTOS/Nef_Drive2/EBAY/BMP-Scans/e1038/")
;;; :TEST-ME (mon-get-file-mod-times "u:/NEFS_PHOTOS/Nef_Drive2/EBAY/BMP-Scans/e1038/e1038-0.bmp")
;;; file-newer-than-file-p filename1 filename2

;;; ==============================
;;; :CREATED <Timestamp: Wednesday May 20, 2009 @ 04:54.25 PM - by MON KEY>
(defun mon-conv-time-flt-pnt (&optional time)
  "Time is a floating point number (fractions of 1 hour).
e.g. \(/ (+ (* HIGH-bit  65536.0) LOW-bit) 3600.0)\n
NOTE: Not acccurate to microsecond. Take care comparing object timestamps.
Function lifted from `ido.el's `ido-time-stamp'.\n
:EXAMPLE\n\(mon-conv-time-flt-pnt (nth 4 (file-attributes \"~/.emacs\")))
  ;=>345113.5183333333\n►►►"
  (let (op-time)
    (setq op-time (or time (current-time)))
    (/ (+ (* (car op-time) 65536.0)  (cadr op-time)) 3600.0)))
;;
;;; :TEST-ME (mon-conv-time-flt-pnt (current-time))=>345237.03194444446
;;; :TEST-ME (mon-conv-time-flt-pnt)
;;; :TEST-ME (mon-conv-time-flt-pnt (nth 4 (file-attributes "~/.emacs")))

;;; ==============================
;;; :CREATED <Timestamp: Wednesday May 20, 2009 @ 04:54.25 PM - by MON KEY>
(defun mon-comp-times-flt-pnt (&optional t1 t2)
  "Test if time T1 is greater than time T2.
T indicates that T1 is more rerent than T2.
When T1 or T2 is nil value defaults to `current-time'.\n
T1 and T2 are the first _two_ integers of time-value list
per three integer list returned from current-time e.g.\(18963 22549 531000).
Where it is supplied the third integer  microsecond count (cddr) is dropped.\n
:EXAMPLE
\(mon-comp-times-flt-pnt 
  (current-time) ;<--T1 is now!
  '(18963 22549 531000)) ;<--T2 is in the past.\n; => t
\(mon-comp-times-flt-pnt '(18963 22549 531000) (current-time)) ;=> nil 
\(mon-comp-times-flt-pnt '(18963 22549 531000))
\(current-time-string '(18963 22549 531000))
 ;=>\"Tue May 19 21:08:37\" <--T2's value converted to human speak.\n►►►"
(let ((got-t1 (if t1 t1	nil))
      (got-t2 (if t2 t2	nil)))
  (> (mon-conv-time-flt-pnt got-t1) (mon-conv-time-flt-pnt got-t2))))

;;; =======================
;;; :DEPRECATED
(defun mon-accessed-time-stamp ()
  ":DEPRECATED Build the function for inserting shortform time-stamps.\n
:SEE-ALSO `mon-accessed-stamp', `mon-timestamp', `mon-stamp'.\n
Used in `naf-mode'.\n►►►"
  (insert (format-time-string "%A %B %d, %Y")))
;;
;;; :TEST-ME (mon-accessed-time-stamp)

;;; ==============================
;;; :CREATED <Timestamp: Saturday July 18, 2009 @ 05:35.09 PM - by MON KEY>
(defun* mon-timestamp (&key intrp accessed naf insertp) 
  "Core timestamp generating conditional timestamps for insertion from calling
functions. Builds Extended ISO 8601 Timestamp using `mon-format-iso-8601-time'.\n
When kewyord :ACCESSED is non-nil generates and accessed style timestamp.
When kewyord :NAF is non-nil generates a naf-mode style timestamp.
When kewyord :INSERTP is non-nil insert style of timestamp at point. Default 
is used when called interactively: \"Saturday January 10, 2009 @ 06:43.25 PM\"\n
Function is optimized to take advantage unique stamp NAME using file alist lookups to 
`*mon-timestamp-cond-alist*'. When filename is a member of that list keyword args
are still in effect but use the NAME value associated with buffer-filename.\n
:SEE-ALSO `mon-accessed-time-stamp', `mon-timestamp', `mon-stamp'.\n►►►"
  (interactive (list :intrp t))
  (let* ((ts (cond 
              (accessed (mon-format-iso-8601-time))
              (naf (mon-format-iso-8601-time))
              (t (mon-format-iso-8601-time))))
         ;; If we are in a buffer with a filename to associate, does it associate?
         (cond-name
           (let (cn)
             (when (mon-buffer-written-p)
               (cond ((assoc (mon-get-buffers-parent-dir) *mon-timestamp-cond-alist*)
                      (setq cn (cadr (assoc (mon-get-buffers-parent-dir) *mon-timestamp-cond-alist*))))
                     ((assoc (file-name-nondirectory (buffer-file-name)) *mon-timestamp-cond-alist*)
                      (setq cn (cadr (assoc (file-name-nondirectory (buffer-file-name)) *mon-timestamp-cond-alist*))))))
             (unless (mon-buffer-written-p)
               (cond  ((assoc (mon-get-buffers-parent-dir) *mon-timestamp-cond-alist*)
                       (setq cn (cadr (assoc (mon-get-buffers-parent-dir) *mon-timestamp-cond-alist*))))
                      ((assoc (buffer-name) *mon-timestamp-cond-alist*)
                       (setq cn (cadr (assoc (buffer-name) *mon-timestamp-cond-alist*))))))
             cn))
         (name (cond
                (cond-name
                 (cond (accessed (concat " - " cond-name))
                      (naf (concat " - by " cond-name ">"))
                      (t (concat " - by " cond-name ">"))))
                ((or IS-MON-P-W32 (equal user-real-login-name  (cadr (assoc 5 *MON-NAME*))))
                 (cond (accessed " - MON") ;!dont replace me
                       (naf " - by MON>") ;!dont replace me
                       (t "")))
                ((or IS-MON-P-GNU 
                     (and (equal system-type 'gnu/linux)
                      (equal user-real-login-name (cadr (assoc 5 *MON-NAME*)))))
                 (cond (accessed " - MON-Laptop") 
                       (naf " - by MON-Laptop>") 
                       (t "")))
                ((or IS-BUG-P 
                     IS-BUG-P-REMOTE
                     (equal user-real-login-name  (cadr (assoc 6 *BUG-NAME*))))
                 (cond (accessed " - BUG")
                       (naf " - by BUG>")
                       (t "")))
                (t (cond (accessed " - ANONYMOUS")
                        (naf " - by ANONYMOUS>")
                       (t "")))))
        (put-stamp (concat ts name)))
    (if (or intrp insertp) 
        (insert put-stamp)
      put-stamp)))
;;
;;; :TEST-ME (mon-timestamp)
;;; :TEST-ME (mon-timestamp :insertp t)#{2009-08-24T15:43:45-04:00Z}#{09351} - by MON KEY>
;;; :TEST-ME (mon-timestamp :accessed t)
;;; :TEST-ME (mon-timestamp :accessed t :insertp t)#{2009-08-24T15:43:36-04:00Z}#{09351} - MON KEY
;;; :TEST-ME (mon-timestamp :naf t)
;;; :TEST-ME (mon-timestamp :naf t :insertp t) ;-> #{2009-08-24T15:43:21-04:00Z}#{09351} - by MON KEY>
;;; :TEST-ME (mon-timestamp)
;;; :TEST-ME (call-interactively 'mon-timestamp)
;;; :TEST-ME (cadr (assoc "reference-sheet-help-utils.el" *mon-timestamp-cond-alist*))

;;; =======================
;;; :NOTE Use with (add-hook 'write-file-hooks 'mon-stamp) ??
;;; :CREATED <Timestamp: Thursday February 19, 2009 @ 06:31.47 PM - by MON KEY>
(defun mon-stamp (&optional insertp intrp)
  "Return timestamp per a conditional test on user-name and/or system-type.
Insert following formatted date/time at point:
\"<Timestamp: #{2009-08-11T14:42:37-04:00Z}#{09332} - by by MON KEY>\"\n
Stamp is fontlocked when invoked from a `naf-mode' buffer.\n
:SEE-ALSO `mon-timestamp', `mon-accessed-time-stamp', `mon-accessed-stamp',
`mon-file-stamp', `mon-lisp-stamp', `*mon-timestamp-cond-alist*'.\n►►►"
  (interactive "i\np")
  (let ((tstmp (format  "<Timestamp: %s" (mon-timestamp :naf t))))
    (if (or insertp intrp)
        (insert tstmp)
      tstmp)))
;;
;;; :TEST-ME (mon-stamp) 
;;; :TEST-ME (mon-stamp t)
;;; :TEST-ME (call-interactively 'mon-stamp) 

;;; ==============================
;;; :CREATED <Timestamp: Saturday July 18, 2009 @ 05:23.47 PM - by MON KEY>
;;; :NOTE adjust code that calls this programatically to use t
(defun mon-accessed-stamp (&optional insertp commented intrp)
  "Return an 'accessed date' timestamp string.
When INSERTP or called interactively insert at accessed stamp at point.
If COMMENTED is non-nil or called interactively with prefix arg returns with a 
';;; ACCESSED: ' Prefix - default is 'accessed: '.
Invoked from a `naf-mode' buffer acessed-stamp fontlocked by
`naf-mode-accessed-by-flag'.\n
:EXAMPLE\n\(mon-accessed-stamp\)\n\(mon-accessed-stamp nil t\)\n
:SEE-ALSO `mon-accessed-time-stamp', `mon-timestamp', `mon-stamp',
`mon-lisp-stamp', `mon-file-stamp'.\n►►►"
  (interactive "i\nP\np")
  (let ((tstmp (if commented
                   (concat ";;; ACCESSED: " (mon-timestamp :accessed t))
                 (concat "accessed: " (mon-timestamp :accessed t)))))
    (if (or insertp intrp)
        (insert tstmp)
      tstmp)))
;;
;;; :TEST-ME (mon-accessed-stamp)
;;; :TEST-ME (mon-accessed-stamp t)
;;; :TEST-ME (mon-accessed-stamp t t)
;;; :TEST-ME (mon-accessed-stamp nil t)
;;; :TEST-ME (call-interactively 'mon-accessed-stamp)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-10-23T18:06:13-04:00Z}#{09435} - by MON KEY>
;;; :CREATED <Timestamp: 2009-08-06-W32-4T15:18:23-0400Z - by MON KEY>
(defun mon-lisp-stamp (&optional insertp intrp modifications)
  "Return or insert at point a `comment-divider' newline and `mon-stamp'.
When INSERTP is non-nil or called interactively timestap insert at point.
When MODIFICATIONS is non-nil or called interactively with Prefix Arg.
Returns with a ';;; MODIFICATIONS: ' prefix -default is ';;; CREATED: '
Use after creating new elisp functions to delimit and date them.\n
:EXAMPLE\n\(mon-insert-lisp-stamp\)\n\(mon-insert-lisp-stamp nil nil t\)\n
:SEE-ALSO `mon-insert-lisp-stamp', `mon-file-stamp', `mon-insert-copyright',
`mon-insert-lisp-testme', `mon-insert-lisp-CL-file-template', 
`*mon-default-comment-divider*',`comment-divider',`comment-divider-to-col-four'
`mon-insert-lisp-evald'.\n►►►"
  (interactive "i\np\nP")
  (let* ((tstmp (if modifications
                    (concat ";;; :MODIFICATIONS <Timestamp: "(mon-timestamp :naf t ))
                    (concat ";;; :CREATED <Timestamp: "(mon-timestamp :naf t ))))
         (f-tstmp (if (or insertp intrp)
                      (concat "\n" *mon-default-comment-divider* "\n" tstmp)
                      (concat *mon-default-comment-divider* "\n" tstmp))))
    (if (or insertp intrp) 
        (insert f-tstmp) 
        f-tstmp)))
;;
;;; :TEST-ME (mon-lisp-stamp)
;;; :TEST-ME (mon-lisp-stamp t)
;;; :TEST-ME (mon-lisp-stamp nil nil t)
;;; :TEST-ME (mon-lisp-stamp t nil t)
;;; :TEST-ME (call-interactively 'mon-lisp-stamp)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-24T11:52:41-04:00Z}#{09436} - by MON>
(defun mon-file-stamp-vrfy-put-eof (insertp)
  "Return the preferred EOF delimiter if not present in current-buffer
When INSERTP is non-nil and the delimiter wasn't found insert it.
Does not move point.\n
:EXAMPLE\n\(mon-file-stamp-vrfy-put-eof nil\)\n
:SEE-ALSO `mon-file-stamp', `*mon-default-comment-divider*'.\n►►►"
  (let  ((end-of-dlm *mon-default-comment-divider*)
         (end-of ";;; EOF")
         (eof-dlm-mk (make-marker))
         (eof-mk (make-marker)))
    (save-excursion
      (goto-char (buffer-end 1))
      (set-marker eof-mk (point))
      (set-marker eof-dlm-mk (point))
      ;; We do these checks b/c there may newlines etc. after the EOF.
      (search-backward-regexp (concat "^" end-of "$") nil t)
      (unless
          (when (looking-at-p (concat "^" end-of "$"))
            ;; Make a bounds for next so we don't over-search.
            (set-marker eof-mk (point)) 
            (set-marker eof-dlm-mk (- eof-mk (1+ (length end-of-dlm))))
            (search-backward-regexp (concat "^" end-of-dlm "$") eof-dlm-mk t)
            (and (= eof-dlm-mk (point)) (not (= eof-mk eof-dlm-mk))))
        (if insertp 
            (princ (concat "\n" end-of-dlm "\n" end-of) (current-buffer))
            (concat "\n" end-of-dlm "\n" end-of))))))
;;
;;; :TEST-ME (with-temp-buffer (mon-file-stamp-vrfy-put-eof nil))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-10-24T12:20:13-04:00Z}#{09436} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-24T15:46:26-04:00Z}#{09351} - by MON KEY>
(defun mon-file-stamp (&optional insertp intrp w/url)
  "Return timestamped file header and EOF footer.
Retrun value of header's timestamp prepended with \";;; FILE-CREATED:\".
If a file footer is not present it is concatted to header as:\n
 \";;; ==============================\n  ;;; EOF\"\n
When optional arg W/URL is non-nil wrap the URL with \(URL `'\) and include it 
on a second line as: \";;; \(URL `http://some-url.com'\)\". When insertp is 
non-nil or called interactively insert string(s) in buffer, don't move point.
When inserting if point is not equal point-min prepends a newline prior to 
inserting.\n
:EXAMPLE
\(mon-file-stamp nil nil\)\n\(mon-file-stamp nil nil \"http://emacswiki.com\")\n
:SEE-ALSO `mon-file-stamp-vrfy-put-eof' `*mon-default-comment-divider*'
`mon-lisp-stamp', `mon-timestamp', `mon-stamp', `mon-accessed-stamp'.\n►►►"
  (interactive "i\np\nP")
  (let* ((the-url (cond ((and intrp w/url)
                         (concat "\n;;; SOURCE: (URL `" (read-string "URL to wrap :") "')"))
                        ((and (not intrp) w/url) 
                         (concat "\n;;; SOURCE: (URL `"  w/url "')"))
                        (t "")))
         (f-tstmp (concat ";;; FILE-CREATED: <Timestamp: "(mon-timestamp :naf t )
                          the-url "\n" *mon-default-comment-divider* "\n")))
    (if (or intrp insertp) 
        (save-excursion 
          (if (= (point) (point-min))
              (insert f-tstmp)
            (progn (newline) (insert f-tstmp)))
          (mon-file-stamp-vrfy-put-eof t))
        (concat f-tstmp (mon-file-stamp-vrfy-put-eof nil)))))
;;
;;; :TEST-ME (mon-file-stamp)
;;; :TEST-ME (mon-file-stamp nil nil "http://wikipedia.com")
;;; :TEST-ME (mon-file-stamp t)
;;; :TEST-ME (mon-file-stamp nil t "http://wikipedia.com")
;;; :TEST-ME (call-interactively 'mon-file-stamp)

;;; ==============================
;;; <Timestamp: 2009-07-30-W31-4T20:03:12-0400Z - by MON KEY>
(defun mon-convert-ebay-time (time-string)
  "Parse an ebay-time-string encode -> decode it.
Used as a helper funcition with `mon-cln-ebay-time-string'
to return an timestamp with a timestring appropriate to `current-time-zone'.\n
:EXAMPLE\n\(mon-convert-ebay-time \"29 Jul 2009 Tuesday 11:05:27 PDT\")\n
:NOTE(S) `encode-time' wants time values as:
second, minute, hour, day, month, year, and optional zone
but, `decode-time' and `parse-time-string' both return: 
\(SEC MINUTE HOUR DAY MONTH YEAR DOW DST ZONE\)
\(37  9      15   29  7     2009  3  t   -14400\)
\(0   1       2    3  4        5  6  7   8\)\n►►►"
  (let* ((ts (parse-time-string time-string))
         (sec (nth 0 ts))               ;SEC
         (min (nth 1 ts))               ;MINUTE
         (hr  (nth 2 ts))               ;HOUR
         (day (nth 3 ts))               ;DAY
         (mon (nth 4 ts))               ;MONTH
         (yr  (nth 5 ts))               ;YEAR
         (dow (nth 6 ts))               ;DOW
         (dst (nth 7 ts))               ;DST
         (tz  (nth 8 ts))               ;TZ
         (et)) ;;`(,sec ,min ,hr ,day ,mon ,yr ,dow ,dst ,tz))
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
;;; <Timestamp: Wednesday July 29, 2009 @ 06:36.54 PM - by MON KEY>
;;; TODO: consider providing an optional arg to return the stamp without the call to
;;; `mon-convert-ebay-time'???
(defun mon-cln-ebay-time-string (&optional time-string start end insertp intrp)
  "Clean an ebay timestring and process with `mon-convert-ebay-time'.
Called programmatically processes and eBay TIME-STRING of form:\n
\(Aug 07, 200913:52:24 PDT\) <-style1, from eBay webpage i.e. copy/paste
Jul-29 11:05                 <-style2, from eBay listing manager
Aug-10-09 09:16:14 PDT       <-style3, from eBay post listing email confirm
replacing string with a canonically formatted timestamp.
When TIME-STRING has form of style1 following occur:
 * '(Mmm' -> Month
 * parens are removed from head and tail of string;
 * Comma after DD is removed; 
 * Whitespace is inserted after YYYY.\n
When TIME-STRING has form of style2 following occur:
 *  'Mmm-' -> Month  
 *  Current year is concatenated to string;
 *  An eBay time-zone is inserted, either PDT or PST
    This is calculated against `current-time-zone' and may return
    incorrect results if your environment's timezone doesn't track 
    daylight-savings-time in a similiar manner as the U.S. (most should).\n
When TIME-STRING has form of style3 following occur:
 * 'Mmm-' -> Month  
 * Hyphenation removed from 'Mmm-DD-YY'
 * Current year is converted from YY -> YYYY\n
For all 3\(three\) styles returns an extended timestamp formatted to allow
further processing according to ISO 8601 spec. The timestamp provides conversion of
eBay PDT or PST timezone to local time \(not everyone lives on West Coast of 
United States!\)
When optional ARG insertp is non-nil converted timestring is inserted at point.
Called intereactively processes the ebay time-string in region as per above.
Replaces existing time-string in region with converted form.\n
:EXAMPLE
\(mon-cln-ebay-time-string \"\(Jul 29, 200911:05 PDT\)   <-style1
\(mon-cln-ebay-time-string \"Jul-29 11:05\"\)   <-style2
\(mon-cln-ebay-time-string \"Aug-10-09 09:16:14 PDT\") <- style3
:SEE-ALSO: `regexp-clean-ebay-time-chars', `regexp-clean-ebay-month2canonical-style1',
`regexp-clean-ebay-month2canonical-style2',`regexp-clean-ebay-month2canonical-style3'.
►►►"
(interactive "\i\nr\ni\np")
  (let ((cln-ebay regexp-clean-ebay-month2canonical-style1)
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
    (cond ((and start end)  ;;(or intrp insertp)
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
      (setq cln-ebay regexp-clean-ebay-month2canonical-style3)
      (mapc (lambda (eb-mon)
              (let ((fnd-mon (car eb-mon))
                    (rep-mon (cadr eb-mon))
                    (sub-cent (concat " " (subseq (mon-get-current-year) 0 2)))
                    (rep-str-hndl))
                (when (string-match fnd-mon rep-str)
                  (setq found-match (match-string 0 rep-str))
                  (setq rep-match rep-mon)
                  (setq match-style 3) ;; "Aug-10-09 09:16:14 PDT" <- style3
                  (setq rep-str-hndl
                        (concat rep-mon                     ;2: Mmm-  -> "Month "
                                (match-string 3 rep-str)    ;3: DD
                                (concat 
                                 sub-cent 
                                 (match-string 5 rep-str))  ;5: YY -> " YYYY"
                                (match-string 6 rep-str)    ;6: " HH:MM:SS "   
                                (match-string 7 rep-str)))  ;7: ZON
                  (setq rep-str rep-str-hndl))))
                   cln-ebay))
    (unless match-style
      (setq cln-ebay regexp-clean-ebay-month2canonical-style2)
      (mapc (lambda (eb-mon)
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
            (setq pop-list regexp-clean-ebay-time-chars)
            (while pop-list
              (let* ((head-char (pop pop-list))
                     (subst (car head-char))
                     (w/ (cadr head-char)))
                (subst-char-in-string subst w/ rep-str t))))
          (cond ((= match-style 1)   ;;\(August 07 200913:52:24 PDT\) <- style1, fix year whitespace
                 (setq rep-str (replace-regexp-in-string frn-yr bak-yr rep-str)))
                ((= match-style 2) ;; July 29 11:05 <- style2, add year add timezone 
                 (setq rep-str 
                       (replace-regexp-in-string
                        (concat  "\\("                  ;1
                                 "\\([A-Z]*\\)"         ;2 -> MONTH
                                 "\\( \\)"              ;3 -> wsp after MONTH 
                                 "\\([0-9]\\{2,2\\}\\)" ;4 -> DAY
                                 "\\( \\)"              ;5 -> wsp after DAY
                                 "\\([0-9]\\{2,2\\}:[0-9]\\{2,2\\}\\)" ;6 HH:MM
                                 "\\)") bak-yr rep-str nil nil 5))
                 (setq rep-str (concat rep-str " " put-tz)))
                ((= match-style 3)
                 rep-str);; "August 10 09 09:16:14 PDT" <- style3 fix year ->NN09                
                (t (message "Could Not match an ebay timestamp for %s" time-string)))
          (setq found-match (format "Found: %s\nReplaced: %s\nWith: %s " 
                                    found-match
                                    (if time-string
                                        time-string
                                      (buffer-substring-no-properties start-marker end-marker))
                                    rep-match)))
      (setq found-match (format "Couldn't find match for: %s" rep-str)))
    ;; Consider providing an optional arg to return the stamp without the call to
    ;; `mon-convert-ebay-time'??? 
    (cond ((and start end (or insertp intrp))
           (progn    
             (delete-and-extract-region start end)
             (goto-char start-marker) 
             (insert (mon-convert-ebay-time rep-str))
             (message found-match)))
          ((and time-string insertp)
           (insert (mon-convert-ebay-time rep-str)))
          (t (mon-convert-ebay-time rep-str)))))
;;
;;; :TEST-ME (mon-cln-ebay-time-string "(Jul 29, 200911:05 PDT)")
;;; :TEST-ME (mon-cln-ebay-time-string "(Jul 29, 200911:05 PDT)"  nil nil t)
;;; :TEST-ME (mon-cln-ebay-time-string "Jul-29 11:05")
;;; :TEST-ME (mon-cln-ebay-time-string "Jul-29 11:05" nil nil t)
;;; :TEST-ME (mon-cln-ebay-time-string "Jul-29-09 11:05:14 PDT")
;;; :TEST-ME (mon-cln-ebay-time-string "Jul-29-09 11:05:14 PDT" nil nil t)
;;;

;;; ==============================
;;; <Timestamp: Tuesday July 28, 2009 @ 08:28.46 PM - by MON KEY>
(defun mon-calculate-ebay-timezone-diff (ebay-time-string)
  "Return time difference in hours between ebay-time (PDT or PST)
and `current-time-zone'.
EBAY-TIME-STRING should have the format:
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
oStarting in 2007, most of the United States and Canada observe DST from the
second Sunday in March to the first Sunday in November, almost two-thirds of the
year. The 2007 U.S. change was part of the Energy Policy Act of 2005;
previously, from 1987 through 2006, the start and end dates were the first
Sunday in April and the last Sunday in October, and Congress retains the right
to go back to the previous dates now that an energy-consumption study has been
done. :SEE\n\(URL `http://en.wikipedia.org/wiki/Daylight_saving_time').
\(URL `http://isotc.iso.org/livelink/livelink/4021199/ISO_8601_2004_E.zip?func=doc.Fetch&nodeid=4021199').
►►►"
  (let ((ets ebay-time-string))
    (/ (- (abs (car (last (parse-time-string ets))))
          (abs (car (current-time-zone))))
       3600)))
;;
;;; :TEST-ME (mon-calculate-ebay-timezone-diff "28 July 2009 17:08:26 PST") ;-> 4
;;; :TEST-ME (mon-calculate-ebay-timezone-diff "28 July 2009 17:08:26 PDT") ;-> 3
;;; :TEST-ME (mon-calculate-ebay-timezone-diff "28 July 2009 17:08:26 EST") ;-> 1
;;; :TEST-ME (mon-calculate-ebay-timezone-diff "28 July 2009 17:08:26 EDT") ;-> 0

;;; ==============================
(provide 'mon-time-utils)
;;; ==============================

;;; ============================
;;; mon-time-utils.el ends here
;;; EOF

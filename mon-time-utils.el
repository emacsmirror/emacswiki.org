;;; this is mon-time-utils.el
;;; ================================================================
;;; DESCRIPTION: 
;;; Official my ass! (URL `http://viv.ebay.com/ws/eBayISAPI.dll?EbayTime')
;;; Convert from 'eBay official time' to 'YOUR Official Time ®'
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
;;; The function `mon-cln-ebay-time-string' returns an extended timestamp formatted
;;; such as to allow multiple paths to further processing per ISO 8601 spec. 
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
;;; The form: `(put-tz (if (nth 7 (decode-time)) "PDT" "PST"))'
;;; in `mon-cln-ebay-time-string' assumes your Daylight-savings flag 
;;; is in synch with the Northern Hemisphere. If it Summer in California
;;; but Winter where you are you may want to swap PDT <-> PST
;;; ...I hope i got that right :P
;;;
;;; FUNCTIONS:
;;; `mon-get-current-year'
;;; `mon-convert-ebay-time'
;;; `mon-convert-ebay-time-mvb'
;;; `mon-cln-ebay-time-string'
;;; `mon-calculate-ebay-timezone-diff'
;;; `mon-format-iso-8601-time'
;;;
;;; CONSTANTS or VARIABLES:
;;; `regexp-clean-ebay-time-chars'
;;; `regexp-clean-ebay-month2canonical-style1'
;;; `regexp-clean-ebay-month2canonical-style2'
;;; `regexp-clean-ebay-month2canonical-style3'
;;;
;;; MACROS:
;;;
;;; SUBST or ALIASES:
;;;
;;; DEPRECATED, RENAMED, OR MOVED:
;;;
;;; REQUIRES:
;;; 'cl `mon-convert-ebay-time-mvb' -> `multiple-value-bind'
;;;
;;; TODO:
;;; Consider providing an optional arg `mon-cln-ebay-time-string'
;;; to return the stamp without the call to `mon-convert-ebay-time'
;;;
;;; NOTES:
;;; If you should happen to find this code useful please drop eBay a line
;;; and kindly request them to pry their Java loving heads out of their 
;;; asses! What possible reason could there be to maintain at least 3(three)
;;; different timestamp formats??? Also, consider appending a postscript 
;;; requesting a reasonable explanation as to why in this _the 21st C_
;;; (i.e. our glorious technological future) does eBay insist that the 
;;; passage of time continue to be skewed towards PST and PDT... WTF!!!
;;;
;;; ==============================
;;;
;;; The timestamps generate by functions herein are of the form:
;;;
;;; <Timestamp: #{YYYY-MM-DDTHH:MM:SS+/-HH:MMZ}#{yyWwD} - by NAME>
;;; <Timestamp: #{2009-08-06T21:17:10-04:00Z}#{09325} - by Ebay>
;;;
;;; NOTE: `#{' is a reserved user dispatching macro char in CL
;;;
;;; This format is not at all in keeping with RFC-3339 or ISO-8601 w/re
;;; the discussion presented in Section 5.4. Redundant Information:
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
;;; 'redundancy' allows for a check against what would otherwise be uncorroborated
;;; time. Moreover, so long as the information is presented according to the
;;; standards specification it is quite legitimate to simply omit the redundancy via
;;; heuristics (regexps for example). Doing so allows us the ability to calculate
;;; multiple different types of date intervals in an adhoc 'loosey' manner whilst
;;; retaining the ability to perform more targeted calculations where
;;; necessary. This can be esp. important/useful for indicating and reasoning about
;;; time/date ranges by field as opposed by duration. E.g. "Show me all date stamps
;;; on a Tuesday." whether the date is _correct_ may be completely irrelevant to the
;;; query and it is certainly the case that the heuristics/calculations needed for
;;; locating such stamps under our regime will be quite a bit quicker and more
;;; intelligible than those which would require numerous
;;; encode->decode->calculate->encode operations just to find what may or may not be 
;;; a Tuesday... 
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; FILE CREATED:
;;; <Timestamp: Wednesday July 29, 2009 @ 07:46.54 PM - by MON KEY>
;;;
;;; KEYWORDS: time
;;;
;;; ================================================================
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;; ================================================================
;;; CODE:

;;; ==============================
;;; `mon-convert-ebay-time-mvb' -> `multiple-value-bind'
(eval-when-compile (require 'cl)) 

;;; ==============================
;;; <Timestamp: Wednesday July 29, 2009 @ 06:19.33 PM - by MON KEY>
(defvar regexp-clean-ebay-time-chars
  '((44  32)
    (40  32)
    (41  32))
  "Alist of chars to replace in ebay times.
Used-by: `mon-cln-ebay-time-string'.
Chars are all associated with char 32 SPC.
44 -> ,\n40 -> (\n41 -> )\n
EXAMPLE:
\(August 07, 200913:52:24 PDT\) -> Aug 07  200913:52:24 PDT
This type of string corresponds to the one corrected with regexps in:
`regexp-clean-ebay-month2canonical-style1'.
See also: `regexp-clean-ebay-month2canonical',`regexp-clean-ebay-month2canonical-style1',
`regexp-clean-ebay-month2canonical-style2'.")

;;test-me:
;;(let ((pop-list))
;;   (setq pop-list regexp-clean-ebay-time-chars)
;;   (while pop-list
;;     (princ (char-to-string (car (pop pop-list))) (current-buffer))))

;;(progn (makunbound 'regexp-clean-ebay-time-chars)
;; (unintern 'regexp-clean-ebay-time-chars))

;;; ==============================
;;; <Timestamp: Wednesday July 29, 2009 @ 06:58.52 PM - by MON KEY>
(defvar regexp-clean-ebay-month2canonical-style1
  '(("(Jan " "January ") 
    ("(Feb " "February ") 
    ("(Mar " "March ") 
    ("(Apr " "April ") 
    ("(Jun " "June ") 
    ("(Jul " "July ") 
    ("(Aug " "August ") 
    ("(Sep " "September ") 
    ("(Sept " "September ") 
    ("(Oct " "October ") 
    ("(Nov " "November ")
    ("(Dec " "December "))
"Alist to clean ebay timestrings from eBay webpage.\n
EXAMPLE:
\(Aug 07, 200913:52:24 PDT\)\n
Used-by: `mon-cln-ebay-time-string'.
See also: `regexp-clean-ebay-time-chars' `regexp-clean-ebay-month2canonical',
`regexp-clean-ebay-month2canonical-style2'.")

;;;testme: regexp-clean-ebay-month2canonical-style1

;;(progn (makunbound 'regexp-clean-ebay-month2canonical-style1) 
;;       (unintern 'regexp-clean-ebay-month2canonical-style1))

;;; ==============================
;;; <Timestamp: Wednesday July 29, 2009 @ 06:58.46 PM - by MON KEY>
(defvar regexp-clean-ebay-month2canonical-style2 
  '(("Jan-" "January ") 
    ("Feb-" "February ") 
    ("Mar-" "March ") 
    ("Apr-" "April ") 
    ("Jun-" "June ") 
    ("Jul-" "July ") 
    ("Aug-" "August ") 
    ("Sept-" "September ") 
    ("Sep-" "September ") 
    ("Oct-" "October ") 
    ("Nov-" "November ") 
    ("Dec-" "December "))
"Alist to clean ebay timestrings from eBay.
Style2 from eBay listing manager.

EXAMPLE:
Jul-29 11:05                 <-style2
Used-by: `mon-cln-ebay-time-string'
See also: `regexp-clean-ebay-time-chars' `regexp-clean-ebay-month2canonical-style3',
`regexp-clean-ebay-month2canonical-style1'.")

;;;testme: regexp-clean-ebay-month2canonical-style2 

;;(progn (makunbound 'regexp-clean-ebay-month2canonical-style2) 
;;       (unintern 'regexp-clean-ebay-month2canonical-style2))

;;; ==============================
;;; <Timestamp: Wednesday July 29, 2009 @ 06:24.19 PM - by MON KEY>
(defun mon-get-current-year (&optional insertp intrp)
(interactive "i\np")
  (let (cy)
    (setq cy (format "%s" (nth 5 (decode-time (current-time)))))
    (if (or insertp intrp)
        (princ cy (current-buffer))
      cy)))

;;;test-me;(mon-get-current-year) 
;;;test-me;(mon-get-current-year t)
;;;test-me;(call-interactively 'mon-get-current-year)

;;; ==============================
;;; <Timestamp: Wednesday July 29, 2009 @ 05:12.02 PM - by MON KEY>
(defvar regexp-clean-ebay-month2canonical-style3 'nil
  "Alist to clean ebay timestrings from eBay.
Used-by: `mon-cln-ebay-time-string'
Style3 from eBay post listing email confirmations.\n
Aug-10-09 09:16:14 PDT       <-style3
See also: `regexp-clean-ebay-time-chars',`regexp-clean-ebay-month2canonical-style1',
`regexp-clean-ebay-month2canonical-style2'.")
;;
(eval-when-compile ;;?needed?
  (when (not (bound-and-true-p regexp-clean-ebay-month2canonical-style3))
    (setq regexp-clean-ebay-month2canonical-style3
          (let ((from-style2 regexp-clean-ebay-month2canonical-style2)
                (sub-yr (subseq (mon-get-current-year) 2 4))
                (Mmm-))
            (setq Mmm- '())
            (mapcar (lambda (x)
                      (let* ((bld-rgxp (concat "\\("     ; 1 
                                               "\\(%s\\)" ; 2 -> Mmm-
                                               "\\([0-9]\\{2,2\\}\\)" ; 3 -> DD
                                               "\\(-\\)" ; 4 -> hyphen post DD
                                               "\\(%s\\)" ; 5 -> sub-yr
                                               "\\( [0-9]\\{2,2\\}:[0-9]\\{2,2\\}:[0-9]\\{2,2\\} \\)" ; 6 -> " HH:MM:SS "
                                               "\\(.*\\)" ; 7 -> TimeZone/ whatevers left 
                                               "\\)"))
                             (splc-rgxp (format bld-rgxp (car x) sub-yr)))
                        (setq Mmm- 
                              (cons (list splc-rgxp (cadr x)) Mmm-))))
                    from-style2)
          (nreverse Mmm-)))))


;;;testme; regexp-clean-ebay-month2canonical-style3
;;;testme;(car regexp-clean-ebay-month2canonical-style3) 
;;;testme;(caar regexp-clean-ebay-month2canonical-style3) 
;;;testme;(cdar regexp-clean-ebay-month2canonical-style3) 

;;(progn (makunbound 'regexp-clean-ebay-month2canonical-style3) 
;; (unintern 'regexp-clean-ebay-month2canonical-style3))
(defun mon-format-iso-8601-time (&optional time insertp intrp)
  "Returns ISO-8601 compliant timestring in 'Extended' format.
EXAMPLE: \(mon-format-iso-8601-time\) 
=> #{2009-08-06T21:17:10-04:00Z}#{09325}
When optional arg TIME is non-nil, it should have a format suitable for 
processing with `format-time-string'.
When INSERTP is non-nil or called interactively insert timestring at point.
Vanilla format-time-string returns 'UTC offset/ZONE' in ISO-8601 'Basic format:\n
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
See: 
\(URL `http://isotc.iso.org/livelink/livelink/4021199/ISO_8601_2004_E.zip?func=doc.Fetch&nodeid=4021199')
\(URL `http://www.ietf.org/rfc/rfc3339.txt') ;Date and Time on the Internet.
\(URL `http://www.w3.org/TR/NOTE-datetime') ;W3C Specification about UTC Date and Time.
\(URL `http://en.wikipedia.org/wiki/ISO-8601').
\(URL `http://en.wikipedia.org/wiki/ISO_8601_usage')."
  (interactive "i\ni\np")
  (let* ((colon-z-stamp 
          ;;(format-time-string "%Y-%m-%d-W%V-%uT%T%zZ" time))
          ;;(format-time-string "%Y-%m-%dT%T%zZ" time))
          ;; `#{' is a reserved user dispatching macro in CL      
           (format-time-string"#{%Y-%m-%dT%T%zZ}#{%y%V%u}" time)) 
   (iso-8601-colon-stamp  
          (format "%s:%s%s" 
                  (substring colon-z-stamp  0 -12) ;0 -3) 
                  (substring colon-z-stamp 24 28)
                  (substring colon-z-stamp 28)
                  )))
    (if (or insertp intrp)
        (insert iso-8601-colon-stamp)
      iso-8601-colon-stamp)))

;;;test-me;(mon-format-iso-8601-time)
;;;test-me;(mon-format-iso-8601-time nil t) 
;;;test-me;(call-interactively 'mon-format-iso-8601-time)
;;;#{2009-08-06T21:17:10-04:00Z}#{09325}

;;; ==============================
;;; <Timestamp: 2009-07-30-W31-4T20:03:12-0400Z - by MON KEY>
(defun mon-convert-ebay-time (time-string)
  "Parse an ebay-time-string encode -> decode it.
Used as a helper function with `mon-cln-ebay-time-string'
to return an timestamp with a timestring appropriate to `current-time-zone'.\n
EXAMPLE:
\(mon-convert-ebay-time \"29 Jul 2009 Tuesday 11:05:27 PDT\")\n
Background:
`encode-time' wants time values as:
second, minute, hour, day, month, year, and optional zone
but, `decode-time' and `parse-time-string' both return: 
\(SEC MINUTE HOUR DAY MONTH YEAR DOW DST  ZONE\)
\(37  9      15   29  7     2009  3   t   -14400\)"
  (let* ((ts (parse-time-string time-string))
         (sec (car ts)) ;SEC            ;
         (min (cadr ts)) ;MINUTE        ;
         (hr (caddr ts)) ;HOUR          ;
         (day (cadddr ts)) ;DAY         ;
         (mon (car (cddddr ts))) ;MONTH ;
         (yr (cadr (cddddr ts))) ;YEAR  ;
         (dow (caddr (cddddr ts))) ;DOW ;
         (dst (cadddr (cddddr ts))) ;DST ;
         (tz (car (last ts))) ;ZONE     ;
         (et)
         (dt))
    (setq et (encode-time sec min hr day mon yr dow tz))
    ;; (format-time-string "<Timestamp: %Y-%m-%d-W%V-%uT%T%zZ - by Ebay>" et)))
    (concat "<Timestamp: " (mon-format-iso-8601-time et) " - by Ebay>")))
            
;;;test-me;(mon-convert-ebay-time "29 Jul 2009 Tuesday 11:05:27 PDT")

;;; ==============================
;;; CREATED: <Timestamp: 2009-08-04-W32-2T12:43:38-0400Z - by MON KEY>
(defun mon-convert-ebay-time-mvb (time-string)
"Like `mon-convert-ebay-time' but uses CL's `multiple-value-bind'."
  (multiple-value-bind 
      (sec min hr day mon yr dow dst tz) 
      (parse-time-string time-string)
    ;; (format-time-string "<Timestamp: %Y-%m-%d-W%V-%uT%T%zZ - by Ebay>"
    ;; (encode-time sec min hr day mon yr dow tz)
    ;; )))
    (concat "<Timestamp: " 
            (mon-format-iso-8601-time (encode-time sec min hr day mon yr dow tz))
            " - by Ebay>")))

;;;test-me;(mon-convert-ebay-time-mvb "29 Jul 2009 Tuesday 11:05:27 PDT")

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
EXAMPLE:
\(mon-cln-ebay-time-string \"\(Jul 29, 200911:05 PDT\)   <-style1
\(mon-cln-ebay-time-string \"Jul-29 11:05\"\)   <-style2
\(mon-cln-ebay-time-string \"Aug-10-09 09:16:14 PDT\") <- style3
See also: `regexp-clean-ebay-time-chars', `regexp-clean-ebay-month2canonical-style1',
`regexp-clean-ebay-month2canonical-style2',`regexp-clean-ebay-month2canonical-style3'."
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
    ;; consider providing an optional arg to return the stamp without the call to
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

;;;test-me;(mon-cln-ebay-time-string "(Jul 29, 200911:05 PDT)")
;;;test-me;(mon-cln-ebay-time-string "(Jul 29, 200911:05 PDT)"  nil nil t)
;;;test-me;(mon-cln-ebay-time-string "Jul-29 11:05")
;;;test-me;(mon-cln-ebay-time-string "Jul-29 11:05" nil nil t)
;;;test-me;(mon-cln-ebay-time-string "Jul-29-09 11:05:14 PDT")
;;;test-me;(mon-cln-ebay-time-string "Jul-29-09 11:05:14 PDT" nil nil t)

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
o Starting in 2007, most of the United States and Canada observe DST from the
second Sunday in March to the first Sunday in November, almost two-thirds of the
year. The 2007 U.S. change was part of the Energy Policy Act of 2005;
previously, from 1987 through 2006, the start and end dates were the first
Sunday in April and the last Sunday in October, and Congress retains the right
to go back to the previous dates now that an energy-consumption study has been
done. \(URL `http://en.wikipedia.org/wiki/Daylight_saving_time').
\(URL `http://isotc.iso.org/livelink/livelink/4021199/ISO_8601_2004_E.zip?func=doc.Fetch&nodeid=4021199')."
  (let ((ets ebay-time-string))
    (/ (- (abs (car (last (parse-time-string ets))))
          (abs (car (current-time-zone))))
       3600)))

;;;test-me;(mon-calculate-ebay-timezone-diff "28 July 2009 17:08:26 PST") ;-> 4
;;;test-me;(mon-calculate-ebay-timezone-diff "28 July 2009 17:08:26 PDT") ;-> 3
;;;test-me;(mon-calculate-ebay-timezone-diff "28 July 2009 17:08:26 EST") ;-> 1
;;;test-me;(mon-calculate-ebay-timezone-diff "28 July 2009 17:08:26 EDT") ;-> 0

;;; ==============================
(provide 'mon-time-utils)
;;; ==============================

;;; ============================
;;; mon-time-utils.el ends here
;;; EOF

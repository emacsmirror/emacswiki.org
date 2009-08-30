;;; strptime.el -- partial implementation of POSIX date and time parsing
;;;
;;; Copyright (C) 2006 by Adrian Aichner <adrian@xemacs.org>, The
;;; XEmacs Project, 2006-11-05.
;;;
;;; Implemented (partially, specifically without locale support)
;;; according to
;;; http://www.opengroup.org/onlinepubs/009695399/functions/strptime.html

(require 'regexp-opt)

; (dotimes (m 12) (insert (format "(\"\" . %d)\n" (+ m 1))))
(defconst strptime-month-assoc
  '(
    ("January" . 1)
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
    ("Januar" . 1)
    ("Jänner" . 1)
    ("Februar" . 2)
    ("Feber" . 2)
    ("März" . 3)
    ("April" . 4)
    ("Mai" . 5)
    ("Juni" . 6)
    ("Juli" . 7)
    ("Oktober" . 10)
    ("November" . 11)
    ("Dezember" . 12)
    ("Jan" . 1)
    ("Feb" . 2)
    ("Mar" . 3)
    ("Mär" . 3)
    ("Apr" . 4)
    ("May" . 5)
    ("Jun" . 6)
    ("Jul" . 7)
    ("Aug" . 8)
    ("Sep" . 9)
    ("Oct" . 10)
    ("Okt" . 10)
    ("Nov" . 11)
    ("Dec" . 12)
    ("Dez" . 12)
    )
  "Association list of month names and their abbreviations in various
language to their numeric values."
  )

; (dotimes (wd 7) (insert (format "(\"\" . %d)\n" wd)))
(defconst strptime-weekday-assoc
  '(
    ("Sunday" . 0)
    ("Monday" . 1)
    ("Tuesday" . 2)
    ("Wednesday" . 3)
    ("Thursday" . 4)
    ("Friday" . 5)
    ("Saturday" . 6)
    ("Sonntag" . 0)
    ("Montag" . 1)
    ("Dienstag" . 2)
    ("Mittwoch" . 3)
    ("Donnerstag" . 4)
    ("Freitag" . 5)
    ("Samstag" . 6)
    ("Son" . 0)
    ("Mon" . 1)
    ("Die" . 2)
    ("Mit" . 3)
    ("Don" . 4)
    ("Fre" . 5)
    ("Sam" . 6)
    ("Sun" . 0)
    ("Mon" . 1)
    ("Tue" . 2)
    ("Wed" . 3)
    ("Thu" . 4)
    ("Fri" . 5)
    ("Sat" . 6)
    )
  "Association list of weekday names and their abbreviations in
  various language to their numeric values, with 0 representing
  Sunday."
  )

(defconst strptime-format-data-assoc
  `(
    ("%Y" "[0-9]\\{4\\}" 5)
    ("%y" "[0-9]\\{1,2\\}" 5)
    ;; Alternative producing longest possible match must
    ;; come first! i.e. Keep possible zero-length matches
    ;; 0? or [01]? as last alternative.
    ("%m" "1[0-2]\\|0?[1-9]" 4)
    ;; Minimal implementation of weekday names and
    ;; abbreviations in languages most important to me
    ;; personally (APA) .
    ("%a"
     ,(regexp-opt
      (mapcar
       (lambda (weekday)
         (car weekday)
         )
       strptime-weekday-assoc))
     6)
    ;; Minimal implementation of month names and
    ;; abbreviations in languages most important to me
    ;; personally (APA) .
    ("%b" 
     ,(regexp-opt
      (mapcar
       (lambda (month)
         (car month)
         )
       strptime-month-assoc))
     4)
    ("%d" "[12][0-9]\\|3[01]\\|0?[1-9]" 3)
    ("%I" "1[0-2]\\|0?[0-9]" 2)
    ("%H" "2[0-3]\\|[01]?[0-9]" 2)
    ("%M" "[0-5]?[0-9]" 1)
    ("%S" "6[01]\\|[0-5]?[0-9]" 0)
    ("%p" "\\([aApP]\\.?[mM]\\.?\\)" nil)
    ("%%" "%" nil)
    ;; Whitespace seems to be optional, although the
    ;; documentation isn't extremely clear on this.
    ("%n" "[ \n\t]*" nil)
    ("%t" "[ \n\t]*" nil)
    )
  "Association list of strptime formats and their data regexps and
index into decoded time list."
  )

;;;###autoload
(defun strptime (time format &optional extended)
  "
Return a nine element list of TIME, parsed according to FORMAT.

The elements of the list are of the same form as those returned by
`decode-time' and may be used as aguments to `encode-time'.

When TIME is a buffer instead of a string then text at point in that
buffer is parsed according to FORMAT.

\(SEC MINUTE HOUR DAY MONTH YEAR DOW DST ZONE\)

The returned date may be outside the range `encode-time' can handle.

This is a partial implementation of the POSIX `strptime' function.

Optinol argument EXTENDED causes the return value to be extended to
\(\(SEC MINUTE HOUR DAY MONTH YEAR DOW DST ZONE) BEGIN END\)\)

Where BEGIN and END are the buffer locations indicating the begin and
end of the date/time parsed.  This is useful when TIME is a buffer.

Following directives are supported:

%d
    The day of the month [01,31]; leading zeros are permitted but not
    required.
%D
    The date as %m / %d / %y.
%e
    Equivalent to %d.
%H
    The hour (24-hour clock) [00,23]; leading zeros are permitted but
    not required.
%I
    The hour (12-hour clock) [01,12]; leading zeros are permitted but
    not required.
%m
    The month number [01,12]; leading zeros are permitted but not
    required.
%M
    The minute [00,59]; leading zeros are permitted but not required.
%n
    Any white space.
%p
    The locale's equivalent of a.m or p.m.
%R
    The time as %H : %M.
%S
    The seconds [00,60]; leading zeros are permitted but not required.
%t
    Any white space.
%T
    The time as %H : %M : %S.
%y
    The year within century. When a century is not otherwise
    specified, values in the range [69,99] shall refer to years 1969
    to 1999 inclusive, and values in the range [00,68] shall refer to
    years 2000 to 2068 inclusive; leading zeros shall be permitted but
    shall not be required.

    Note:
        It is expected that in a future version of IEEE Std
        1003.1-2001 the default century inferred from a 2-digit year
        will change. (This would apply to all commands accepting a
        2-digit year as input.)

%Y
    The year, including the century (for example, 1988).
%%
    Replaced by %.
"
  (interactive "stime: \nsformat: ")
  (let (time-buffer
        time-buffer-point
        case-fold-search
        case-replace
        date-time-start-position
        date-time-end-position
        ;; This retains values from previous invocations!
        ;; (result '[0 0 0 0 0 0 0 0 0])
        ;; make a new vector!
        ;; Initializing all elements to 0 makes the result usable,
        ;; but will lead to incorrect time values in case the
        ;; date/time is incomplete or not parsed completely.
        (result (make-vector 9 0))
        handle-item-match-data)
    (flet
        ((strptime-internal ()
           (with-temp-buffer
             ;; (message (buffer-name))
             (insert format)
             (goto-char (point-min))
             (if
                 (catch 'cannot-parse-date-time
                   (while
                       (and (not (eobp))
                            (not (eobp time-buffer)))
                     (cond
                      ((handle-item) t)
                      (t
                       ;; Restore point in case of error
                       (if (bufferp time)
                           (goto-char time-buffer-point time-buffer))
                       (if (bufferp time)
                           (throw 'cannot-parse-date-time t)
                         (error "cannot parse date %S with %S at %S"
                                time format
                                (buffer-substring
                                 (point-min time-buffer)
                                 (point time-buffer)
                                 time-buffer))
                         )
                       ))))
                 nil
               ;; Create list to be returned from result vector.
               (cons (append result nil)
                     (list date-time-start-position date-time-end-position)))))
         (handle-item ()
           ;; Bind case-fold-search to nil to distinguish %m and %M!
           (let (case-fold-search text format-data)
             ;; Are we looking at any specific conversion specification?
             ;; FIXME: What about %% ?
             (cond
              ((looking-at "%%\\|%[EO]?[A-Za-z]")
               (setq format-data
                     (buffer-substring
                      (match-beginning 0)
                      (match-end 0)))
               (goto-char (match-end 0))
               (setq format-data-index
                     (assoc format-data strptime-format-data-assoc))
               (setq data-regex (nth 1 format-data-index))
               (setq index (nth 2 format-data-index))
               (if data-regex
                   (if (looking-at data-regex time-buffer)
                       (progn
                         (unless date-time-start-position
                           (setq date-time-start-position (match-beginning 0)))
                         (setq date-time-end-position (match-end 0))
                         (setq text
                               (buffer-substring
                                (match-beginning 0)
                                (match-end 0) time-buffer))

                         (if index
                             (progn
                               (aset result index
                                     (string-to-number text))
                               ;; Handle weekday names and abbreviations
                               (if (string-equal format-data "%a")
                                   (aset result index
                                         (cdr
                                          (assoc text strptime-weekday-assoc))))
                               ;; Handle month names and abbreviations
                               (if (string-equal format-data "%b")
                                   (aset result index
                                         (cdr
                                          (assoc text strptime-month-assoc))))
                               ;; Handle year windowing according to
                               ;; specification:
                               (if (string-equal format-data "%y")
                                   (if (< (aref result 5) 69)
                                       (aset result 5 (+ (aref result 5) 2000))
                                     (aset result 5 (+ (aref result 5) 1900)))))
                           (if (string-equal format-data "%p")
                               (save-match-data
                                 (cond
                                  ((string-match "\\`A\\.?M\\.?\\'" (upcase text))
                                   (if (= (aref result 2) 12)
                                       (aset result 2 0)))
                                  ((string-match "\\`P\\.?M\\.?\\'" (upcase text))
                                   (if (< (aref result 2) 12)
                                       (aset result 2 (+ (aref result 2) 12))))))))
                         (goto-char (match-end 0) time-buffer)
                         (not (string-equal text "")))
                     ;; Restore point in case of error
                     (if (bufferp time)
                         (goto-char time-buffer-point time-buffer))
                     ;; FIXME: Is this an error in this
                     ;; implementation?
                     (if (bufferp time)
                         nil
                       (error "failed to parse %S by %S in %S at %S"
                              format-data data-regex time
                              (buffer-substring
                               (point-min)
                               (point time-buffer)
                               time-buffer)))
                     )
                 (goto-char
                  (+ (point time-buffer)
                     (- (match-end 0) (match-beginning 0))) time-buffer)))
              ((looking-at "[^% \n\r\t]")
               (setq format-data
                     (buffer-substring
                      (match-beginning 0)
                      (match-end 0)))
               (goto-char (match-end 0))
               (looking-at "[^%]" time-buffer)
               (setq text
                     (buffer-substring
                      (match-beginning 0)
                      (match-end 0) time-buffer))
               (cond
                ((not
                  (string-equal format-data text))
                 ;; Restore point in case of error
                 (if (bufferp time)
                     (progn
                       (goto-char time-buffer-point time-buffer)
                       nil)
                   (error
                    (format
                     "\"%s\" does not match specification \"%s\"\n"
                     text format-data))))
                (t
                 (goto-char
                  (+ (point time-buffer)
                     (- (match-end 0) (match-beginning 0))) time-buffer))))
              ((looking-at "[ \n\r\t]+")
               (setq format-data
                     (buffer-substring
                      (match-beginning 0)
                      (match-end 0)))
               (goto-char (match-end 0))
               (looking-at "[ \n\r\t]+" time-buffer)
               (goto-char
                (+ (point time-buffer)
                   (- (match-end 0) (match-beginning 0))) time-buffer)
               )
              (t
               ;; Restore point in case of error
               (if (bufferp time)
                   (progn
                     (goto-char time-buffer-point time-buffer)
                     nil)
                 (error "cannot parse date %S with %S at %S"
                        time format
                        (buffer-substring
                         (point-min time-buffer)
                         (point time-buffer)
                         time-buffer))))))))
      (setq handle-item-match-data (match-data))
      ;; Replace directive aliases
      (setq format (replace-in-string format "%e" "%d"))
      (setq format (replace-in-string format "%A" "%a"))
      (setq format (replace-in-string format "%B" "%b"))
      (setq format (replace-in-string format "%h" "%b"))
      ;; Break down composite cases to individual components.
      (setq format (replace-in-string format "%D" "%m/%d/%y"))
      (setq format (replace-in-string format "%T" "%H:%M:%S"))
      (setq format (replace-in-string format "%R" "%H:%M"))
      ;; prog1 is needed to return value of cond, not of the
      ;; final `store-match-data', which is needed to avoid
      ;; side-effects of `looking-at'.
      (prog1
          (cond
           ((stringp time)
            (with-temp-buffer
              (setq time-buffer (current-buffer))
              (insert time)
              (goto-char (point-min) time-buffer)
              (if extended
                  (strptime-internal)
                (car (strptime-internal)))))
           ((bufferp time)
            (setq time-buffer (current-buffer))
            (setq time-buffer-point (point time-buffer))
            (if extended
                (strptime-internal)
              (car (strptime-internal))))
           (t
            (store-match-data handle-item-match-data)
            (error "Invalid TIME argument")))
        (store-match-data handle-item-match-data)))))

;;; Manual testing
(when nil
  (and
   (equal
    (strptime "2008-10-24T213508+0200"
              "%Y-%m-%dT%H%M%S")
    '(8 35 21 24 10 2008 0 0 0))
   (equal
    (strptime "03:49 a.m. 11/07/2008"
              "%R %p %m/%d/%Y")
    '(0 49 3 7 11 2008 0 0 0))
   (equal
    (strptime "October 24, 2008 2:24 PM ET"
              "%b %d, %Y %R %p")
    '(0 24 14 24 10 2008 0 0 0))
   (equal
    (strptime "2006-11-05"
              "%Y-%m-%d")
    '(0 0 0 5 11 2006 0 0 0))
   (equal
    (strptime "2006-11-05 6:19 PM"
              "%Y-%m-%d %R %p")
    '(0 19 18 5 11 2006 0 0 0))
   (equal
    (strptime "2006-11-05 6:19 PM"
              "%Y-%m-%d %R %p")
    '(0 19 18 5 11 2006 0 0 0))
   (equal
    (strptime "2006-11-05 6:19 PM"
              "%Y-%m-%d %H:%M %p")
    '(0 19 18 5 11 2006 0 0 0))
   (equal
    (strptime "2006-11-05 6:19 % PM"
              "%Y-%m-%d %H:%M %% %p")
    '(0 19 18 5 11 2006 0 0 0))
   (equal
    (strptime "2006 -11\t-05 6:19 % PM"
              "%Y%n-%m%t-%d %H:%M %% %p")
    '(0 19 18 5 11 2006 0 0 0))
   (equal
    (strptime "2006-11-5 12:19 PM"
              "%Y-%m-%d %R %p")
    '(0 19 12 5 11 2006 0 0 0))
   (equal
    (strptime "2006-11-5 12:19 am"
              "%Y-%m-%d %H:%M %p")
    '(0 19 0 5 11 2006 0 0 0))
   (equal
    (strptime "11/5/68 12:19 am"
              "%m/%d/%y %H:%M %p")
    '(0 19 0 5 11 2068 0 0 0))
   (equal
    (strptime "12:43 PM 9/29/2007"
              "%H:%M %p %m/%d/%Y")
    '(0 43 12 29 9 2007 0 0 0))
   (equal
    (strptime "11/5/68 12:19 am"
              "%D %H:%M %p")
    '(0 19 0 5 11 2068 0 0 0))
   (equal
    (strptime "11/5/69 12:19 am"
              "%D %H:%M %p")
    '(0 19 0 5 11 1969 0 0 0))
; (current-time-string)
   (equal
    (strptime "Sat Aug 16 15:44:41 2008"
              "%a %b %d %T %Y")
    '(41 44 15 16 8 2008 6 0 0))
   (equal
    (strptime
     "Fri Aug 15 13:13:14 2008 +0200 (Westeuropäische Sommerzeit)"
     "%a %b %d %T %Y")
    '(14 13 13 15 8 2008 5 0 0))
; R E W E Rechnung
   (equal
    (strptime
     "21.07.2008 13:01"
     "%d.%m.%Y %R")
    '(0 1 13 21 7 2008 0 0 0))
; VollCorner Rechnung
   (equal
    (strptime
     "12:52 21.7.2008"
     "%R %d.%m.%Y")
    '(0 52 12 21 7 2008 0 0 0)))
  )

;;; The Single UNIX ® Specification, Version 2
;;; Copyright © 1997 The Open Group
;;;  NAME

;;;     strptime - date and time conversion 

;;;  SYNOPSIS



;;;     #include <time.h>

;;;     char *strptime(const char *buf, const char *format, struct tm *tm);

;;;  DESCRIPTION

;;;     The strptime() function converts the character string pointed to by buf to values which are stored in the tm structure pointed to by tm, using the format specified by format.

;;;     The format is composed of zero or more directives. Each directive is composed of one of the following: one or more white-space characters (as specified by isspace(); an ordinary character (neither % nor a white-space character); or a conversion specification. Each conversion specification is composed of a % character followed by a conversion character which specifies the replacement required. There must be white-space or other non-alphanumeric characters between any two conversion specifications. The following conversion specifications are supported:

;;;     %a
;;;         is the day of week, using the locale's weekday names; either the abbreviated or full name may be specified. 
;;;     %A
;;;         is the same as %a. 
;;;     %b
;;;         is the month, using the locale's month names; either the abbreviated or full name may be specified. 
;;;     %B
;;;         is the same as %b. 
;;;     %c
;;;         is replaced by the locale's appropriate date and time representation. 
;;;     %C
;;;         is the century number [0,99]; leading zeros are permitted but not required. 
;;;     %d
;;;         is the day of month [1,31]; leading zeros are permitted but not required. 
;;;     %D
;;;         is the date as %m/%d/%y. 
;;;     %e
;;;         is the same as %d. 
;;;     %h
;;;         is the same as %b. 
;;;     %H
;;;         is the hour (24-hour clock) [0,23]; leading zeros are permitted but not required. 
;;;     %I
;;;         is the hour (12-hour clock) [1,12]; leading zeros are permitted but not required. 
;;;     %j
;;;         is the day number of the year [1,366]; leading zeros are permitted but not required. 
;;;     %m
;;;         is the month number [1,12]; leading zeros are permitted but not required. 
;;;     %M
;;;         is the minute [0-59]; leading zeros are permitted but not required. 
;;;     %n
;;;         is any white space. 
;;;     %p
;;;         is the locale's equivalent of a.m or p.m. 
;;;     %r
;;;         12-hour clock time using the AM/PM notation if t_fmt_ampm is not an empty string in the LC_TIME portion of the current locale; in the POSIX locale, this will be equivalent to %I:%M:%S %p. 
;;;     %R
;;;         is the time as %H:%M. 
;;;     %S
;;;         is the seconds [0,61]; leading zeros are permitted but not required. 
;;;     %t
;;;         is any white space. 
;;;     %T
;;;         is the time as %H:%M:%S. 
;;;     %U
;;;         is the week number of the year (Sunday as the first day of the week) as a decimal number [00,53]; leading zeros are permitted but not required. 
;;;     %w
;;;         is the weekday as a decimal number [0,6], with 0 representing Sunday; leading zeros are permitted but not required. 
;;;     %W
;;;         is the the week number of the year (Monday as the first day of the week) as a decimal number [00,53]; leading zeros are permitted but not required. 
;;;     %x
;;;         is the date, using the locale's date format. 
;;;     %X
;;;         is the time, using the locale's time format. 
;;;     %y
;;;         is the year within century. When a century is not otherwise specified, values in the range 69-99 refer to years in the twentieth century (1969 to 1999 inclusive); values in the range 00-68 refer to years in the twenty-first century (2000 to 2068 inclusive). Leading zeros are permitted but not required. 
;;;     %Y
;;;         is the year, including the century (for example, 1988). 
;;;     %%
;;;         is replaced by %. 

;;;      Modified Directives
;;;     Some directives can be modified by the E and O modifier characters to indicate that an alternative format or specification should be used rather than the one normally used by the unmodified directive. If the alternative format or specification does not exist in the current locale, the behaviour will be as if the unmodified directive were used.

;;;     %Ec
;;;         is the locale's alternative appropriate date and time representation. 
;;;     %EC
;;;         is the name of the base year (period) in the locale's alternative representation. 
;;;     %Ex
;;;         is the locale's alternative date representation. 
;;;     %EX
;;;         is the locale's alternative time representation. 
;;;     %Ey
;;;         is the offset from %EC (year only) in the locale's alternative representation. 
;;;     %EY
;;;         is the full alternative year representation. 
;;;     %Od
;;;         is the day of the month using the locale's alternative numeric symbols; leading zeros are permitted but not required. 
;;;     %Oe
;;;         is the same as %Od. 
;;;     %OH
;;;         is the hour (24-hour clock) using the locale's alternative numeric symbols. 
;;;     %OI
;;;         is the hour (12-hour clock) using the locale's alternative numeric symbols. 
;;;     %Om
;;;         is the month using the locale's alternative numeric symbols. 
;;;     %OM
;;;         is the minutes using the locale's alternative numeric symbols. 
;;;     %OS
;;;         is the seconds using the locale's alternative numeric symbols. 
;;;     %OU
;;;         is the week number of the year (Sunday as the first day of the week) using the locale's alternative numeric symbols. 
;;;     %Ow
;;;         is the number of the weekday (Sunday=0) using the locale's alternative numeric symbols. 
;;;     %OW
;;;         is the week number of the year (Monday as the first day of the week) using the locale's alternative numeric symbols. 
;;;     %Oy
;;;         is the year (offset from %C) using the locale's alternative numeric symbols. 

;;;     A directive composed of white-space characters is executed by scanning input up to the first character that is not white-space (which remains unscanned), or until no more characters can be scanned.

;;;     A directive that is an ordinary character is executed by scanning the next character from the buffer. If the character scanned from the buffer differs from the one comprising the directive, the directive fails, and the differing and subsequent characters remain unscanned.

;;;     A series of directives composed of %n, %t, white-space characters or any combination is executed by scanning up to the first character that is not white space (which remains unscanned), or until no more characters can be scanned.

;;;     Any other conversion specification is executed by scanning characters until a character matching the next directive is scanned, or until no more characters can be scanned. These characters, except the one matching the next directive, are then compared to the locale values associated with the conversion specifier. If a match is found, values for the appropriate tm structure members are set to values corresponding to the locale information. Case is ignored when matching items in buf such as month or weekday names. If no match is found, strptime() fails and no more characters are scanned.

;;;  RETURN VALUE

;;;     Upon successful completion, strptime() returns a pointer to the character following the last character parsed. Otherwise, a null pointer is returned. 

;;;  ERRORS

;;;     No errors are defined. 

;;;  EXAMPLES

;;;     None. 

;;;  APPLICATION USAGE

;;;     Several "same as" formats, and the special processing of white-space characters are provided in order to ease the use of identical format strings for strftime() and strptime().

;;;     Applications should use %Y (4-digit years) in preference to %y (2-digit years).

;;;     It is unspecified whether multiple calls to strptime() using the same tm structure will update the current contents of the structure or overwrite all contents of the structure. Portable applications should make a single call to strptime() with a format and all data needed to completely specify the date and time being converted. 

;;;  FUTURE DIRECTIONS

;;;     This function is expected to be mandatory in the next issue of this specification. 

;;;  SEE ALSO

;;;     scanf(), strftime(), time(), <time.h>. 

;;; UNIX ® is a registered Trademark of The Open Group.
;;; Copyright © 1997 The Open Group
;;; [ Main Index | XSH | XCU | XBD | XCURSES | XNS ]

(provide 'strptime)

;;; strptime.el ends here

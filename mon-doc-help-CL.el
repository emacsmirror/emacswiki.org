;;; this is mon-doc-help-CL.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-doc-help-CL.el
;;; 
;;; FUNCTIONS:►►►
;;; `mon-help-CL:LOCAL-TIME', `mon-help-CL:LOOP', `mon-help-CL:TIME'
;;; `mon-help-slime-keys'
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS or VARIABLES:
;;; 
;;; MACROS:
;;; 
;;; SUBST or ALIASES:
;;; 
;;; MOVED:
;;; `mon-help-CL-time', `mon-help-CL-loop', `mon-help-slime-keys' -> mon-doc-help-CL.el
;;; `mon-help-CL:TIME', `mon-help-CL:LOOP',
;;;
;;; RENAMED:
;;; `mon-help-CL-loop' -> `mon-help-CL:LOOP'
;;; `mon-help-CL-time' -> `mon-help-CL:TIME' 
;;; 
;;; REQUIRES:
;;; mon-doc-help-utils.el
;;;
;;; TODO:
;;; Import dpans utility dpans2texi.el (URL `http://www.phys.au.dk/~harder/dpans.html')
;;; or (URL `ftp://ftp.ma.utexas.edu/pub/gcl/gcl-info+texi.tgz')
;;; This really needs to be done on Gnu/linux and transfered in as its a w32 headache.
;;; 
;;; NOTES:
;;; 
;;; SNIPPETS:
;;;(defun mon-insert-doc-help-tail (&optional fname)
;;; (interactive "P")
;;; (if intrp 
;;;     (reference-sheet-help-function-spit-doc 'xxxxxxxxxxx)
;;;   (message "pass non-nil for optional arg INTRP")))
;;;
;;; THIRD PARTY CODE:
;;; Pascal Bourguignon his: `pjb-cl.el' 
;;; WAS: `loop-doc'->`reference-sheet-help-loop'
;;; (URL `http://www.informatimago.com/develop/emacs/index.html')
;;; 
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;; 
;;; FILE-CREATED:
;;; <Timestamp: Thursday July 16, 2009 @ 10:56.13 AM - by MON KEY>
;;; ================================================================
;;; Copyright (C) 2009 MON KEY
;;; ==========================
;;; CODE:

;;; ==============================
;;; COURTESY: Pascal Bourguignon HIS: `pjb-cl.el' WAS: `loop-doc'
;;; ADDED: <Timestamp: Tuesday June 23, 2009 @ 03:22.54 PM - by MON KEY>
;;; MODIFICATIONS: REPLACED: empty lines with '\n' escaped lisp forms in docstring
(defun mon-help-CL:LOOP (&optional intrp)
  "(loop CLAUSE...): The Common Lisp `loop' macro.
See info node `(cl)Loop Facility'\n
Valid clauses are:\n
    for VAR from/upfrom/downfrom NUM to/upto/downto/above/below NUM by NUM
    for VAR in LIST by FUNC
    for VAR on LIST by FUNC
    for VAR = INIT then EXPR
    for VAR across ARRAY
    with VAR = INIT\n
 Miscellaneous Clauses:\n
    named NAME
    initially EXPRS...\n
 Accumulation Clauses:\n
    collect EXPR into VAR
    append EXPR into VAR
    nconc EXPR into VAR
    sum EXPR into VAR
    count EXPR into VAR
    maximize EXPR into VAR
    minimize EXPR into VAR\n
 Termination Test Clauses:\n
    repeat NUM
    while COND
    until COND
    always COND
    never COND
    thereis COND\n
 Unconditional Execution Clause:\n
    do EXPRS...\n
 Conditional Execution Clauses:\n
    if COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]
    unless COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]\n
 Miscellaneous Clauses:\n
    finally EXPRS...
    return EXPR
    finally return EXPR ►►►\n
;;; \(loop for i in '\(1 2 3 4\)
;;;       collect  i into col
;;;       append   i into app
;;;       nconc    i into nco
;;;       sum      i into sum
;;;       count    i into cnt
;;;       maximize i into max
;;;       minimize i into min
;;;       do \(printf \\\"%d \\\" i\)
;;;       return \(progn \(printf \\\"\\n\\\" i\)
;;;                     \(values col app nco sum cnt max min\)\)\)"
(interactive "P")
(if intrp 
    (mon-help-function-spit-doc 'mon-help-CL:LOOP)
  (message "pass non-nil for optional arg INTRP")))

;;;test-me;(describe-function 'mon-help-CL:LOOP)

;;; ==============================
(defun mon-help-CL:DO (&optional intrp)
(interactive "P")
"DO
{VARIABLE*} 
DO's variable bindings are like LET's with a twist. 
With LET you set up vars eg \(let \(\(i 0\) \(j 1\) \(k 2\)\) {...} \)
With DO you set initial values of VAR\(s\) _and_ dynamic 'step-values' e.g.
\(do \(\(i 0 \(+ i 1\)\) \(j 1 \(+ j 5\)\) {...} \)
As with LET, DO's return values accumulate within the body except, DO 
includes built-in support for iterative testing and returning within the 
DO form itself. Basically what you would otherwise do inside the body of 
LET get's DOne in the test-form result-form section \(which is why it looks
so much like the body of a LET and why it is in it's own list \(smacks head!\)

{TEST-FORM RESULT-FORM*}
`DO' has a step test with each pass. This is like Elisp's `while' but the test 
is for NIL rather than T. i.e. a while form which loops if TEST-FORM yields NIL.
IOW, "While it don't test true - keep on DOing what needs to get DOne!".
The RESULT-FORM\(s\) are the DOings to be DOne. 
RESULT-FORM\(s\) are given after the TEST-FORM.

{STATEMENT*} 
This is the return phase. With LET you would do TEST-FORM RESULT-FORM
mojo here as well - instead DO allows for multiple statements to occur here 
this is cool because we can reflect back into the DO bindings without needing to 
bind additional values just to pass around our results. This is also why DO
often doesn't have a statement body - all the work is already finished.►►►\n
\(let \(k\)
  \(do \(\(i 0 \(+ i 1\)\)
       \(j 0 \(+ 80 j\)\)\)
      \(\(> i 7\) j\)
    \(setq k \(cons j k\)\)\)
  \(nreverse k\)\)
;=>\(0 80 160 240 320 400 480 560\)\n
\(let \(k\)
\(do \(;begin DO's var bidings
     \(i 0 \(+ i 1\)\) ;start var I at 0 - with each pass step I by 1
     \(j 0 \(+ 80 j\)\) ;start var J at 1 - with each pass add 80 to J's previous value
     \) ;end DO's variable bindings
    \(;;Begin `DO's {test-form restult-form}
     \(> i 7\) ; We're DOing something if TEST-FORM yields NIL
     j ;RESULT-FORM gets DOne as long as stepper is not true. In this case J gets DOne
     \) ;DOing is DOne 
  ;; STATEMENT - cons up the results of DOing - this is side-effect oriented.
  \(setq k \(cons j k\)\)\)
;; DOne DOing
;; Now LET us destructively reverse the list that DO built :P
\(nreverse k\)\)"
(mon-help-function-spit-doc 'mon-help-CL:DO)
           (message "pass non-nil for optional arg INTRP"))

;;;test-me;(mon-help-CL:DO )


;;; ==============================
;;; CREATED: <Timestamp: Wednesday July 15, 2009 @ 12:50.16 PM - by MON KEY>
(defun mon-help-CL:TIME (&optional intrp)
"CL: `GET-DECODED-TIME' 
Return nine values specifying the current time as follows:
second, minute, hour, date, month, year, day of week \(0 = Monday\), T
\(daylight savings times\) or NIL \(standard time\), and timezone.►►►
\(get-decoded-time\) =>
14     ;second\n44     ;minute\n12     ;hour\n15     ;date\n7      ;month
2009   ;year\n2      ;day\nT      ;dayligt-p\n5      ;zone"
(interactive "P")
(if intrp 
    (mon-help-function-spit-doc 'mon-help-CL:TIME)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(describe-function 'mon-help-CL:TIME)

;;; ==============================
;; (while (search-forward-regexp 
;;        ;;....1..2.........3.........
;;        "^\\(\\(.*\t\\)\\(.*\\)\\)$")
;;  (replace-match "\\2`\\3'"))
;;; ==============================
;;; CRATED: <Timestamp: Wednesday July 08, 2009 @ 06:11.12 PM - by MON KEY>
(defun mon-help-slime-keys (&optional intrp)
  "See also; `slime-cheat-sheet' ►►►
SLIME REPL mode keys: 
key              binding
---              -------
C-a		`slime-repl-bol''
C-c		 Prefix Command
TAB		`slime-indent-and-complete-symbol'
C-j		`slime-repl-newline-and-indent'
RET		`slime-repl-return'
C-x		 Prefix Command
ESC		 Prefix Command
SPC		`slime-space'
,		`slime-handle-repl-shortcut'
DEL		`backward-delete-char-untabify'
<C-down>	`slime-repl-forward-input'
<C-return>	`slime-repl-closing-return'
<C-up>		`slime-repl-backward-input'
<home>		`slime-repl-bol'
<return>	`slime-repl-return'
C-c C-b .. C-c C-c  `slime-interrupt'
C-c C-d		`slime-doc-map'
C-c C-e		`slime-interactive-eval'
C-c TAB		`slime-complete-symbol'
C-c C-l		`slime-load-file'
C-c RET		`slime-macroexpand-1'
C-c C-n		`slime-repl-next-prompt'
C-c C-o		`slime-repl-clear-output'
C-c C-p		`slime-repl-previous-prompt'
C-c C-r		`slime-eval-region'
C-c C-t		`slime-toggle-trace-fdefinition'
C-c C-u		`slime-repl-kill-input'
C-c C-w		`slime-who-map'
C-c C-x		 Prefix Command
C-c C-z		`slime-nop'
C-c ESC		 Prefix Command
C-c :		`slime-interactive-eval'
C-c <		`slime-list-callers'
C-c >		`slime-list-callees'
C-c E		`slime-edit-value'
C-c I		`slime-inspect'
M-TAB		`slime-complete-symbol'
M-RET		`slime-repl-closing-return'
C-M-q		`indent-sexp'
C-M-x		`slime-eval-defun'
M-*		`slime-edit-definition'
M-,		`slime-pop-find-definition-stack'
M-.		`slime-edit-definition'
M-n		`slime-repl-next-input'
M-p		`slime-repl-previous-input'
M-r		`slime-repl-previous-matching-input'
M-s		`slime-repl-next-matching-input'
C-M-.		`slime-next-location'
C-c C-d		`slime-doc-map'
C-c C-w		`slime-who-map'
C-c C-x		 Prefix Command
C-c C-z		`slime-switch-to-output-buffer'
C-c ESC		 Prefix Command
C-x C-e		`slime-eval-last-expression'
C-x 4		 Prefix Command
C-x 5		 Prefix Command
C-c C-z		`run-lisp'
C-M-x		`lisp-eval-defun'
C-c M-d		`slime-disassemble-symbol'
C-c M-m		`slime-macroexpand-all'
C-c M-o		`slime-repl-clear-buffer'
C-c M-p		`slime-repl-set-package'
C-c C-w C-a	`slime-who-specializes'
C-c C-w C-b	`slime-who-binds'
C-c C-w C-c	`slime-who-calls'
C-c C-w RET	`slime-who-macroexpands'
C-c C-w C-r	`slime-who-references'
C-c C-w C-s	`slime-who-sets'
C-c C-w C-w	`slime-calls-who'
C-c C-w a	`slime-who-specializes'
C-c C-w b	`slime-who-binds'
C-c C-w c	`slime-who-calls'
C-c C-w m	`slime-who-macroexpands'
C-c C-w r	`slime-who-references'
C-c C-w s	`slime-who-sets'
C-c C-w w	`slime-calls-who'
C-c C-d C-a	`slime-apropos'
C-c C-d C-d	`slime-describe-symbol'
C-c C-d C-f	`slime-describe-function'
C-c C-d C-p	`slime-apropos-package'
C-c C-d C-z	`slime-apropos-all'
C-c C-d #	`common-lisp-hyperspec-lookup-reader-macro'
C-c C-d a	`slime-apropos'
C-c C-d d	`slime-describe-symbol'
C-c C-d f	`slime-describe-function'
C-c C-d h	`slime-hyperspec-lookup'
C-c C-d p	`slime-apropos-package'
C-c C-d z	`slime-apropos-all'
C-c C-d ~	`common-lisp-hyperspec-format'
C-c C-d C-#	`common-lisp-hyperspec-lookup-reader-macro'
C-c C-d C-~	`common-lisp-hyperspec-format'
C-c C-x c	`slime-list-connections'
C-c C-x t	`slime-list-threads'
C-x 5 .		`slime-edit-definition-other-frame'
C-x 4 .		`slime-edit-definition-other-window'"
  (interactive "P")
  (if intrp 
      (mon-help-function-spit-doc 'mon-help-slime-keys)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-slime-keys)
;;;test-me;(mon-help-slime-keys t)

;;; ==============================
(defun mon-help-CL:LOCAL-TIME (&optional intrp)
(interactive "P")
"LOCAL-TIME:*DEFAULT-TIMEZONE*
 Variable: \(not documented\)
LOCAL-TIME:+ASCTIME-FORMAT+
 Variable: \(not documented\)
LOCAL-TIME:+DAY-NAMES+
 Variable: \(not documented\)
LOCAL-TIME:+DAYS-PER-WEEK+
 Variable: \(not documented\)
LOCAL-TIME:+GMT-ZONE+
 Variable: \(not documented\)
LOCAL-TIME:+HOURS-PER-DAY+
 Variable: \(not documented\)
LOCAL-TIME:+ISO-8601-FORMAT+
 Variable: \(not documented\)
LOCAL-TIME:+MINUTES-PER-DAY+
 Variable: \(not documented\)
LOCAL-TIME:+MINUTES-PER-HOUR+
 Variable: \(not documented\)
LOCAL-TIME:+MONTH-NAMES+
 Variable: \(not documented\)
LOCAL-TIME:+RFC-1123-FORMAT+
 Variable: Please note that you should use the +GMT-ZONE+ timezone to format a
 proper RFC 1123 timestring. See the RFC for the details about the possible
 values of the timezone field.
LOCAL-TIME:+RFC3339-FORMAT+
 Variable: \(not documented\)
LOCAL-TIME:+RFC3339-FORMAT/DATE-ONLY+
 Variable: \(not documented\)
LOCAL-TIME:+SECONDS-PER-DAY+
 Variable: \(not documented\)
LOCAL-TIME:+SECONDS-PER-HOUR+
 Variable: \(not documented\)
LOCAL-TIME:+SECONDS-PER-MINUTE+
 Variable: \(not documented\)
LOCAL-TIME:+SHORT-DAY-NAMES+
 Variable: \(not documented\)
LOCAL-TIME:+SHORT-MONTH-NAMES+
 Variable: \(not documented\)
LOCAL-TIME:+UTC-ZONE+
 Variable: \(not documented\)
LOCAL-TIME:ADJUST-TIMESTAMP
 Macro: \(not documented\)
LOCAL-TIME:ADJUST-TIMESTAMP!
 Macro: \(not documented\)
LOCAL-TIME:ASTRONOMICAL-JULIAN-DATE
 Function: Returns the astronomical julian date referred to by the timestamp.
LOCAL-TIME:DAY-OF
 Generic Function: \(not documented\)
LOCAL-TIME:DAYS-IN-MONTH
 Function: Returns the number of days in the given month of the specified year.
LOCAL-TIME:DECODE-TIMESTAMP
 Function: Returns the decoded time as multiple values: nsec, ss, mm, hh, day,
 month, year, day-of-week
LOCAL-TIME:DEFINE-TIMEZONE
 Macro: Define zone-name \(a symbol or a string\) as a new timezone, lazy-loaded
 from zone-file \(a pathname designator relative to the zoneinfo directory on
 this system.  If load is true, load immediately.
LOCAL-TIME:ENABLE-READ-MACROS
 Function: Enables the local-time reader macros for literal timestamps and
 universal time.
LOCAL-TIME:ENCODE-TIMESTAMP
 Function: Return a new TIMESTAMP instance corresponding to the specified time
 elements.
LOCAL-TIME:FIND-TIMEZONE-BY-LOCATION-NAME
 Function: \(not documented\)
LOCAL-TIME:FORMAT-HTTP-TIMESTRING
 Function: \(not documented\)
LOCAL-TIME:FORMAT-RFC3339-TIMESTRING
 Function: Formats a timestring in the RFC 3339 format, a restricted form of
 the ISO-8601 timestring specification for Internet timestamps.
LOCAL-TIME:FORMAT-TIMESTRING
 Function: Constructs a string representation of TIMESTAMP according to FORMAT
 and returns it.  If destination is T, the string is written to
 *standard-output*.  If destination is a stream, the string is written to the
 stream.
LOCAL-TIME:MAKE-TIMESTAMP
 Macro: \(not documented\)
LOCAL-TIME:MODIFIED-JULIAN-DATE
 Function: Returns the modified julian date referred to by the timestamp.
LOCAL-TIME:NOW
 Function: Returns a timestamp representing the present moment.
LOCAL-TIME:NSEC-OF
 Generic Function: \(not documented\)
LOCAL-TIME:PARSE-RFC3339-TIMESTRING
 Function: \(not documented\)
LOCAL-TIME:PARSE-TIMESTRING
 Function: Parse a timestring and return the corresponding TIMESTAMP.
See split-timestring for details. Unspecified fields in the timestring
are initialized to their lowest possible value, and timezone offset is
0 \(UTC\) unless explicitly specified in the input string.
LOCAL-TIME:SEC-OF
 Generic Function: \(not documented\)
LOCAL-TIME:TIMESTAMP
 Type: \(not documented\)
LOCAL-TIME:TIMESTAMP+
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-CENTURY
 Function: Returns the ordinal century upon which the timestamp falls.
LOCAL-TIME:TIMESTAMP-DAY
 Function: Returns the day of the month upon which the timestamp falls.
LOCAL-TIME:TIMESTAMP-DAY-OF-WEEK
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-DECADE
 Function: Returns the cardinal decade upon which the timestamp falls.
LOCAL-TIME:TIMESTAMP-DIFFERENCE
 Function: Returns the difference between TIME-A and TIME-B in seconds
LOCAL-TIME:TIMESTAMP-HOUR
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-MAXIMIZE-PART
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-MAXIMUM
 Function: Returns the latest timestamp
LOCAL-TIME:TIMESTAMP-MICROSECOND
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-MILLENNIUM
 Function: Returns the ordinal millennium upon which the timestamp falls.
LOCAL-TIME:TIMESTAMP-MILLISECOND
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-MINIMIZE-PART
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-MINIMUM
 Function: Returns the earliest timestamp
LOCAL-TIME:TIMESTAMP-MINUTE
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-MONTH
 Function: Returns the month upon which the timestamp falls.
LOCAL-TIME:TIMESTAMP-SECOND
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-SUBTIMEZONE
 Function: Return as multiple values the time zone as the number of seconds
east of UTC, a boolean daylight-saving-p, and the customary abbreviation of the
timezone.
LOCAL-TIME:TIMESTAMP-TO-UNIVERSAL
 Function: Return the UNIVERSAL-TIME corresponding to the TIMESTAMP
LOCAL-TIME:TIMESTAMP-TO-UNIX
 Function: Return the Unix time corresponding to the TIMESTAMP
LOCAL-TIME:TIMESTAMP-WHOLE-YEAR-DIFFERENCE
 Function: Returns the number of whole years elapsed between time-a and time-b
 \(hint: anniversaries\).
LOCAL-TIME:TIMESTAMP-YEAR
 Function: Returns the cardinal year upon which the timestamp falls.
LOCAL-TIME:TIMESTAMP/=
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP<
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP<=
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP=
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP>
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP>=
 Function: \(not documented\)
LOCAL-TIME:TO-HTTP-TIMESTRING
 Function: \(not documented\)
LOCAL-TIME:TODAY
 Function: Returns a timestamp representing the present day.
LOCAL-TIME:UNIVERSAL-TO-TIMESTAMP
 Function: Returns a timestamp corresponding to the given universal time.
LOCAL-TIME:UNIX-TO-TIMESTAMP
 Function: Return a TIMESTAMP corresponding to UNIX,
which is the number of seconds since the unix epoch, 1970-01-01T00:00:00Z.
LOCAL-TIME:WITH-DECODED-TIMESTAMP
 Macro: This macro binds variables to the decoded elements of TIMESTAMP.
The TIMEZONE argument is used for decoding the timestamp, and is not bound by
the macro.  The value of DAY-OF-WEEK starts from 0 which means Sunday."
(interactive "P")
(if intrp
   (mon-help-function-spit-doc 'mon-help-CL-loop)
 (message "pass non-nil for optional arg INTRP")))

;;; ==============================
(provide 'mon-doc-help-CL)
;;; ==============================

;;; ================================================================
;;; mon-doc-help-CL.el ends here
;;; EOF

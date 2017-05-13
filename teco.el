;;; teco.el --- Dale Worley's teco-in-elisp interpreter

;; -*-byte-compile-dynamic-docstrings: nil;-*-
;;; Commentary
;;; Teco interpreter for Gnu Emacs, version 7.

;; LCD Archive Entry:
;; teco|Dale R. Worley|worley@alum.mit.edu
;; |Teco interpreter
;; |96-09-02|version 7|~/packages/teco.el.Z

;; This code has been tested some, but no doubt contains a zillion bugs.
;; You have been warned.

;; Some byte-compilers will not compile the function definitions for the
;; Teco commands because the defuns are created by macros.  If you have this
;; problem, I can send you the byte-compiled version.

;; Written by Dale R. Worley based on a C implementation by Matt Fichtenbaum.
;; Please send comments, bug fixes, enhancements, etc. to worley@alum.mit.edu.

;; WARRANTY DISCLAIMER

;; This software was created by Dale R. Worley and is
;; distributed free of charge.  It is placed in the public domain and
;; permission is granted to anyone to use, duplicate, modify and redistribute
;; it provided that this notice is attached.

;; Dale R. Worley provides absolutely NO WARRANTY OF ANY KIND
;; with respect to this software.  The entire risk as to the quality and
;; performance of this software is with the user.  IN NO EVENT WILL DALE R.
;; WORLEY BE LIABLE TO ANYONE FOR ANY DAMAGES ARISING OUT THE
;; USE OF THIS SOFTWARE, INCLUDING, WITHOUT LIMITATION, DAMAGES RESULTING FROM
;; LOST DATA OR LOST PROFITS, OR FOR ANY SPECIAL, INCIDENTAL OR CONSEQUENTIAL
;; DAMAGES.

;; Since much of this code is translated from the C version by
;; Matt Fichtenbaum, I include his copyright notice:
;; TECO for Ultrix.   Copyright 1986 Matt Fichtenbaum.
;; This program and its components belong to GenRad Inc, Concord MA 01742.
;; They may be copied if this copyright notice is included.

;; Change log:

;; Version 1
;; Original implementation

;; Version 2
;; Fix bugs found by Alan Katz in S.

;; Version 3
;; Fix bugs found by Lum Johnson in key-binding code.
;; Fix handling of ^C, ^G, and ^L in command input.
;; Fix <...> so it iterates indefinitely.  (Found by Mark Henderson.)
;; Fix ; so it exits from iterations correctly.
;; Add FR and FS commands.
;; Make commands that are supposed to set ^S do so.
;; Make flow-control commands clear the @-flag, so the @-flag can be
;; statically predicted during skipping of code.
;; Set up immediate-action commands ?, /, and *q.

;; Version 4
;; The T, D, and K commands weren't clearing their arguments.
;; Since ! is a flow-control command (O can go to it), it must clear the
;; @-flag.

;; Version 5
;; Fix bug found by karl@kelp.boston.ma.us in teco:output, causing trace output
;; to generate errors.  It also caused =, ==, and === operators to generate
;; errors.
;; Put in improved disclaimer of copyright and warranty.
;; Added teco:version variable and function.

;; Version 6
;; Add attribution to "Philosophy".
;; Fix handling of negative arguments to S.  (Fix due to Bill Freeman.)
;; Fix order of arguments produced by ^Y.  (Fix due to Bill Freeman.)
;; Add ES flag to control placement of cursor after reverse searches.
;; (because different Tecos handle cursor placement differently)
;; Altered expression computation so that "-" alone yields -1 as a value.
;; (useful for -Sabc$)
;; Add n,mL command, mostly for use with FW.
;; Add FL and FW commands.
;; Add FR and FS to list of commands.
;; Add FE to execute Emacs lisp code.
;; Add teco:copy-to-q-reg to make loading q-regs easier.
;; Function 'teco' added as alias for 'teco:command', to make invocation with
;; M-x easier.
;; Fix an assortment of bugs and bad coding found by karl@kelp.boston.ma.us.

;; Version 7
;; Changed construction of teco:command-keymap to work in Emacs 19.30.
;; (This may be non-compatible with Emacs 18.)
;; Fixed minor bugs revealed by byte-compile.

;; Version 8
;; Change `last-command-char' to new `last-command-event'.
;; Fixed some bugs revealed by byte-compile.
;; Added support for evil, try to add the ability to remap
;; teco:command-escape, which is one of the original feature
;; of TECO.

;; To be able to invoke Teco directly, do:
;; (global-set-key "\C-z" 'teco:command)
;;					; or whatever key binding you want
;; (autoload 'teco:command "teco"
;;   "Read and execute a Teco command string."
;;   t nil)
;; (autoload 'teco "teco"
;;   "Read and execute a Teco command string."
;;   t nil)
;;					; 'teco' is an alias for 'teco:command'
;; This can be useful for loading q-regs from an Emacs buffer:
;; (global-set-key "\C-xy" 'teco:copy-to-q-reg)
;;					; or whatever key binding you want
;; (autoload 'teco:copy-to-q-reg "teco"
;;   "Copy region into Teco q-reg REG."
;;   t nil)

;; Differences from other Tecos:
;; Character positions in the buffer are numbered in the Emacs way:  The first
;; character is numbered 1 (or (point-min) if narrowing is in effect).  The
;; B command returns that number.
;; Ends of lines are represented by a single character (newline), so C and R
;; skip over them, rather than 2C and 2R.
;; All file I/O is left to the underlying Emacs.  Thus, almost all Ex commands
;; are omitted.
;; Immediate action commands are ?, /, and *q.

;; Command set:
;;	NUL	Not a command.
;;	^A	Output message to terminal (argument ends with ^A)
;;	^C	Exit macro
;;	^C^C	Stop execution
;;	^C	(type-in) abort command
;;	^D	Set radix to decimal
;;	^EA	(match char) Match alphabetics
;;	^EC	(match char) Match symbol constituents
;;	^ED	(match char) Match numerics
;;	^EGq	(match char) Match any char in q-reg
;;	^EL	(match char) Match line terminators
;;	^EQq	(string char) Use contents of q-reg
;;	^ER	(match char) Match alphanumerics
;;	^ES	(match char) Match non-null space/tab
;;	^EV	(match char) Match lower case alphabetic
;;	^EW	(match char) Match upper case alphabetic
;;	^EX	(match char) Match any char
;;	^G	(type-in) abort command
;;	TAB	Insert tab and text
;;	LF	Line terminator; Ignored in commands
;;	VT	Ignored in commands
;;	FF	Ignored in commands
;;	FF	(type-in) redraw screen
;;	CR	Ignored in commands
;;	^Nx	(match char) Match all but x
;;	^O	Set radix to octal
;;	^Q	Convert line argument into character argument
;;	^Qx	(string char) Use x literally
;;	n^R	Set radix to n
;;	:^R	Enter recursive edit
;;	^S	-(length of last referenced string)
;;			set by S, I, TAB, G, FR, FS, and \
;;	^S	(match char) match separator char
;;	^T	Ascii value of next character typed
;;	n^T	Output Ascii character with value n
;;	^U	(type-in) Kill command line
;;	^Uq	Put text argument into q-reg
;;	n^Uq	Put Ascii character 'n' into q-reg
;;	:^Uq	Append text argument to q-reg
;;	n:^Uq	Append character 'n' to q-reg
;;	^X	Set/get search mode flag
;;	^X	(match char) Match any character
;;	^Y	Equivalent to '.+^S,.'
;;	^Z	Not a Teco command
;;	ESC	String terminator; absorbs arguments
;;	ESC ESC	(type-in) End command
;;	ESC ESC Exit from macro
;;	^\	Not a Teco command
;;	^]	Not a Teco command
;;	^^x	Ascii value of the character x
;;	^_	One's complement (logical NOT)
;;	!	Define label (argument ends with !)
;;	"	Start conditional
;;	n"<	Test for less than zero
;;	n">	Test for greater than zero
;;	n"=	Test for equal to zero
;;	n"A	Test for alphabetic
;;	n"C	Test for symbol constituent
;;	n"D	Test for numeric
;;	n"E	Test for equal to zero
;;	n"F	Test for false
;;	n"G	Test for greater than zero
;;	n"L	Test for less than zero
;;	n"N	Test for not equal to zero
;;	n"R	Test for alphanumeric
;;	n"S	Test for successful
;;	n"T	Test for true
;;	n"U	Test for unsuccessful
;;	n"V	Test for lower case
;;	n"W	Test for upper case
;;	#	Logical OR
;;	$	Not a Teco command
;;	n%q	Add n to q-reg and return result
;;	&	Logical AND
;;	'	End conditional
;;	(	Expression grouping
;;	)	Expression grouping
;;	*	Multiplication
;;	*q	(immediate action) Copy last command into q-reg
;;	+	Addition
;;	,	Argument separator
;;	-	Subtraction or negation
;;	.	Current pointer position
;;	/	Division
;;	/	(immediate action) Insert last command into command buffer
;;	0-9	Digit
;;	n<	Iterate n times
;;	=	Type in decimal
;;	:=	Type in decimal, no newline
;;	==	Type in octal
;;	:==	Type in octal, no newline
;;	===	Type in hexadecimal
;;	:===	Type in hexadecimal, no newline
;;	::	Make next search a compare
;;	>	End iteration
;;	?	Toggle tracing
;;	?	(immediate action) Insert command string or macro that reported
;;		last error, up to point of error, into command buffer
;;	n:A	Get Ascii code of character at relative position n
;;	B	Character position of beginning of buffer
;;	nC	Advance n characters
;;	nD	Delete n characters
;;	n,mD	Delete characters between n and m
;;	ET	Typeout control flag
;;			8	no echo for ^T
;;			32	no wait for ^T
;;	ES	Search control flag
;;			1	leave pointer at end of found string
;;				after reverse search, rather than at beginning
;;	FE	Execute text argument as Emacs lisp.  Uses string chars but
;;		not match chars, so FE^EQq$ executes contents of q-reg
;;	FL	Searches right or left over balanced parenthesized strings,
;;		in the same way as FW searches over words
;;		Syntax is defined by Emacs' syntax table
;;	FR	Replace string found by previous match with text argument
;;	FSaaa$bbb$
;;		Search for string aaa and replace it with bbb.
;;	nFW	Return arguments .,x where x is the other side of the n-th
;;		word from point.  Main uses:  FWL moves right one word,
;;		-FWL moves left one word, FWK deletes one word to right,
;;		nFWXq puts n words into q-reg
;;		"word" is defined by Emacs' syntax table
;;	:FW	Like FW, but goes to near side of the n-th word
;;	Gq	Get string from q-reg into buffer
;;	:Gq	Type out q-reg
;;	H	Equivalent to 'B,Z'
;;	I	Insert text argument
;;	nJ	Move pointer to character n
;;	nK	Kill n lines
;;	n,mK	Kill characters between n and m
;;	nL	Advance n lines
;;	n,mL	Same as n+m-.J, mostly for use by FW
;;	Mq	Execute string in q-reg
;;	O	Goto label
;;	nO	Go to n-th label in list (0-origin)
;;	Qq	Number in q-reg
;;	nQq	Ascii value of n-th character in q-reg
;;	:Qq	Size of text in q-reg
;;	nR	Back up n characters
;;	nS	Search
;;	nT	Type n lines
;;	n,mT	Type chars from n to m
;;	nUq	Put number n into q-reg
;;	nV	Type n lines around pointer
;;	W	Redraw the display
;;	nW	Put point on line n of the display
;;	nXq	Put n lines into q-reg
;;	n,mXq	Put characters from n to m into q-reg
;;	n:Xq	Append n lines to q-reg q
;;	n,m:Xq	Append characters from n to m into q-reg
;;	Z 	Pointer position at end of buffer
;;	[q	Put q-reg on stack
;;	\	Value of digit string in buffer
;;	n\	Convert n to digits and insert in buffer
;;	]q	Pop q-reg from stack
;;	:]q	Test whether stack is empty and return value
;;	`	Not a Teco command
;;	a-z	Treated the same as A-Z
;;	{	Not a Teco command
;;	|	Conditional 'else'
;;	}	Not a Teco comand
;;	~	Not a Teco command
;;	DEL	Delete last character typed in

;; Special q-register names:
;;
;;	_	last search string
;;	#	the current command string
;;	*	last command string
;;	%	the last command string or macro that returned an error,
;;		to the point at which the error was found

;; Philosophy:
;;   Real programmers don't want "what you see is what you get", they want
;;   "you asked for it, you got it".  They want editors that are terse,
;;   powerful, cryptic, and unforgiving.  In a word, Teco.
;; after Ed Post, "Real Programmers Don't Use Pascal", in Datamation,
;; July 1983, p. 264

;;; Code:
;; (require 'backquote)

;; The version number
(defvar teco-version "7.9"
  "The version of Teco.")

(defun teco-version ()
  "Return string describing the version of Teco.
When called interactively, displays the version."
  (interactive)
  (if (called-interactively-p)
      (message "Teco version %s" (teco-version))
    teco-version))

;; set a range of elements of an array to a value
(defun teco:set-elements (array start end value)
  (let ((i start))
    (while (<= i end)
      (aset array i value)
      (setq i (1+ i)))))

;; set a range of elements of an array to their indexes plus an offset
(defun teco:set-elements-index (array start end offset)
  (let ((i start))
    (while (<= i end)
      (aset array i (+ i offset))
      (setq i (1+ i)))))

(defvar teco:command-string ""
  "The current command string being executed.")

(defvar teco:command-pointer nil
  "Pointer into teco:command-string showing next character to be executed.")

(defvar teco:ctrl-r 10
  "Current number radix.")

(defvar teco:et-flag 0
  "ET flags:
8	do not echo ^T input
32	do not wait for ^T input if no character is available.")

(defvar teco:es-flag 0
  "ES flags:
1	leave pointer at end of found string after reverse search,
rather than at beginning.")

(defvar teco:ctrl-s 0
  "The negative of the length of the last string inserted or searched for.
Set by the S, I, TAB, G, FR, FS, and \\ commands.")

(defvar teco:digit-switch nil
  "Set if we have just executed a digit.")

(defvar teco:exp-exp nil
  "Expression value preceeding operator.")

(defvar teco:exp-val1 nil
  "Current argument value.")

(defvar teco:exp-val2 nil
  "Argument before comma.")

(defvar teco:exp-flag1 nil
  "t if argument is present.")

(defvar teco:exp-flag2 nil
  "t if argument before comma is present.")

(defvar teco:exp-op nil
  "Pending arithmetic operation on argument.")

(defvar teco:exp-stack nil
  "Stack for parenthesized expressions.")

(defvar teco:macro-stack nil
  "Stack for macro invocations.")

(defvar teco:mapch-l nil
  "Translation table to lower-case letters.")

    (setq teco:mapch-l (make-vector 256 0))
    (teco:set-elements-index teco:mapch-l 0 255 0)
    (teco:set-elements-index teco:mapch-l ?A ?Z (- ?a ?A))

(defvar teco:trace nil
  "t if tracing is on.")

(defvar teco:at-flag nil
  "t if an @ flag is pending.")

(defvar teco:colon-flag nil
  "1 if a : flag is pending, 2 if a :: flag is pending.")

(defvar teco:qspec-valid nil
  "Flags describing whether a character is a vaid q-register name.
3 means yes, 2 means yes but only for file and search operations.")

    (setq teco:qspec-valid (make-vector 256 0))
    (teco:set-elements teco:qspec-valid ?a ?z 3)
    (teco:set-elements teco:qspec-valid ?0 ?9 3)
    (aset teco:qspec-valid ?_ 2)
    (aset teco:qspec-valid ?# 2)
    (aset teco:qspec-valid ?* 2)
    (aset teco:qspec-valid ?% 2)

(defvar teco:iteration-stack nil
  "Iteration list.")

(defvar teco:cond-stack nil
  "Conditional stack.")

(defvar teco:qreg-text (make-vector 256 "")
  "The text contents of the q-registers.")

(defvar teco:qreg-number (make-vector 256 0)
  "The number contents of the q-registers.")

(defvar teco:qreg-stack nil
  "The stack of saved q-registers.")

(defconst teco:prompt "*"
  "*Prompt to be used when inputting Teco command.")

(defconst teco:exec-1 (make-vector 256 nil)
  "Names of routines handling type 1 characters (characters that are
part of expression processing).")

(defconst teco:exec-2 (make-vector 256 nil)
  "Names of routines handling type 2 characters (characters that are
not part of expression processing).")

(defvar teco:last-search-regexp ""
  "Regexp version of last search string (q-reg '_').")

(defvar teco:search-result 0
  "Result of the last search.")

(defmacro teco:define-type-1 (char &rest body)
  "Define the code to process a type 1 character.
Transforms
  (teco:define-type-1 ?x
    code ...)
into
        (defun teco:type-1-x ()
    code ...)
and does
  (aset teco:exec-1 ?x 'teco:type-1-x)"
  (let ((s (intern (concat "teco:type-1-" (char-to-string char)))))
    `(progn
       (defun ,s ()
         ,@body)
       (aset teco:exec-1 ,char (quote ,s)))))

(defmacro teco:define-type-2 (char &rest body)
  "Define the code to process a type 2 character.
Transforms
  (teco:define-type-2 ?x
    code ...)
into
        (defun teco:type-2-x ()
    code ...)
and does
  (aset teco:exec-2 ?x 'teco:type-2-x)"
  (let ((s (intern (concat "teco:type-2-" (char-to-string char)))))
    `(progn
       (defun ,s ()
         ,@body)
       (aset teco:exec-2 ,char (quote ,s)))))

(defconst teco:char-types (make-vector 256 0)
  "Define the characteristics of characters, as tested by \":
  1	alphabetic
  2	alphabetic, $, or .
  4	digit
  8	alphabetic or digit
  16	lower-case alphabetic
  32	upper-case alphabetic")

(teco:set-elements teco:char-types ?0 ?9 (+ 4 8))
(teco:set-elements teco:char-types ?A ?Z (+ 1 2 8 32))
(teco:set-elements teco:char-types ?a ?z (+ 1 2 8 16))
(aset teco:char-types ?$ 2)
(aset teco:char-types ?. 2)

(defconst teco:error-texts '(("BNI" . "> not in iteration")
                             ("CPQ" . "Can't pop Q register")
                             ("COF" . "Can't open output file ")
                             ("FNF" . "File not found ")
                             ("IEC" . "Invalid E character")
                             ("IFC" . "Invalid F character")
                             ("IIA" . "Invalid insert arg")
                             ("ILL" . "Invalid command")
                             ("ILN" . "Invalid number")
                             ("IPA" . "Invalid P arg")
                             ("IQC" . "Invalid \" character")
                             ("IQN" . "Invalid Q-reg name")
                             ("IRA" . "Invalid radix arg")
                             ("ISA" . "Invalid search arg")
                             ("ISS" . "Invalid search or text string")
                             ("IUC" . "Invalid ^ character")
                             ("LNF" . "Label not found")
                             ("MEM" . "Insufficient memory available")
                             ("MRP" . "Missing )")
                             ("NAB" . "No arg before ^_")
                             ("NAC" . "No arg before ,")
                             ("NAE" . "No arg before =")
                             ("NAP" . "No arg before )")
                             ("NAQ" . "No arg before \"")
                             ("NAS" . "No arg before ;")
                             ("NAU" . "No arg before U")
                             ("NFI" . "No file for input")
                             ("NFO" . "No file for output")
                             ("NYA" . "Numeric arg with Y")
                             ("OFO" . "Output file already open")
                             ("PDO" . "Pushdown list overflow")
                             ("POP" . "Pointer off page")
                             ("SNI" . "; not in iteration")
                             ("SRH" . "Search failure ")
                             ("STL" . "String too long")
                             ("UTC" . "Unterminated command")
                             ("UTM" . "Unterminated macro")
                             ("XAB" . "Execution interrupted")
                             ("YCA" . "Y command suppressed")
                             ("IWA" . "Invalid W arg")
                             ("NFR" . "Numeric arg with FR")
                             ("INT" . "Internal error")
                             ("EFI" . "EOF read from std input")
                             ("IAA" . "Invalid A arg")
                             ))

(defconst teco:spec-chars
  [0          1          0          0	; ^@ ^A ^B ^C
              0          64         0          0	; ^D ^E ^F ^G
              0          2          128        128	; ^H ^I ^J ^K
              128        0          64         0	; ^L ^M ^N ^O
              0          64         64         64	; ^P ^Q ^R ^S
              0          34         0          0	; ^T ^U ^V ^W
              64         0          0          0	; ^X ^Y ^Z ^\[
              0          0          1          0	; ^\ ^\] ^^ ^_
              0          1025       1040       0	;    !  \"  #
              0          0          0          1040; $  %  &  '
              0          0          0          0	; \(  \)  *  +
              0          0          0          0	; ,  -  .  /
              0          0          0          0	; 0  1  2  3
              0          0          0          0	; 4  5  6  7
              0          0          0          0	; 8  9  :  ;
              1040       0          1040       0	; <  =  >  ?
              1          0          12         0	; @  A  B  C
              0          9          1          32	; D  E  F  G
              0          2          0          0	; H  I  J  K
              0          32         10         1026; L  M  N  O
              0          32         8          514	; P  Q  R  S
              0          32         0          4	; T  U  V  W
              32         0          0          32	; X  Y  Z  \[
              0          32         1          2	; \  \]  ^  _
              0          0          12         0	; `  a  b  c
              0          9          1          32	; d  e  f  g
              0          2          0          0	; h  i  j  k
              0          32         10         1026; l  m  n  o
              0          32         8          514	; p  q  r  s
              0          32         0          4	; t  u  v  w
              32         0          0          0	; x  y  z  {
              16         0          0          0	; |  }  ~  DEL
              ]
  "The special properties of characters:
  1	skipto() special character
  2	command with std text argument
  4	E<char> takes a text argument
  8	F<char> takes a text argument
  16	char causes skipto() to exit
  32	command with q-register argument
  64	special char in search string
  128	character is a line separator
  256	command with a double text argument
  512	F<char> takes a double text argument
  1024	transfer of control command")


(defun teco:execute-command (string)
  "Execute teco command string."
  ;; Initialize everything
  (let ((teco:command-string string)
        (teco:command-pointer 0)
        (teco:digit-switch nil)
        (teco:exp-exp nil)
        (teco:exp-val1 nil)
        (teco:exp-val2 nil)
        (teco:exp-flag1 nil)
        (teco:exp-flag2 nil)
        (teco:exp-op 'start)
        (teco:trace nil)
        (teco:at-flag nil)
        (teco:colon-flag nil)
        (teco:iteration-stack nil)
        (teco:cond-stack nil)
        (teco:exp-stack nil)
        (teco:macro-stack nil)
        (teco:qreg-stack nil)
        (teco:search-result 0))
    ;; save command string
    (aset teco:qreg-text ?* (aref teco:qreg-text ?#))
    (aset teco:qreg-text ?# string)
    ;; initialize output
    (teco:out-init)
    ;; execute commands
    (catch 'teco:exit
      (while t
        ;; get next command character
        (let ((cmdc (teco:get-command0 teco:trace)))
          ;; if it's ^, interpret the next character as a control character
          (if (eq cmdc ?^)
              (setq cmdc (logand (teco:get-command teco:trace) 31)))
          (if (and (<= ?0 cmdc) (<= cmdc ?9))
              ;; process a number
              (progn
                (setq cmdc (- cmdc ?0))
                ;; check for invalid digit
                (if (>= cmdc teco:ctrl-r)
                    (teco:error "ILN"))
                (if teco:digit-switch
                    ;; later digits
                    (setq teco:exp-val1
                          (+ (* teco:exp-val1 teco:ctrl-r) cmdc))
                  ;; first digit
                  (setq teco:exp-val1 cmdc)
                  (setq teco:digit-switch t))
                ;; indicate a value was read in
                (setq teco:exp-flag1 t))
            ;; not a digit
            (setq teco:digit-switch nil)
            ;; cannonicalize the case
            (setq cmdc (aref teco:mapch-l cmdc))
            ;; dispatch on the character, if it is a type 1 character
            (let ((r (aref teco:exec-1 cmdc)))
              (if r
                  (funcall r)
                ;; if a value has been entered, process any pending operation
                (if teco:exp-flag1
                    (cond ((eq teco:exp-op 'start)
                           nil)
                          ((eq teco:exp-op 'add)
                           (setq teco:exp-val1 (+ teco:exp-exp teco:exp-val1))
                           (setq teco:exp-op 'start))
                          ((eq teco:exp-op 'sub)
                           (setq teco:exp-val1 (- teco:exp-exp teco:exp-val1))
                           (setq teco:exp-op 'start))
                          ((eq teco:exp-op 'mult)
                           (setq teco:exp-val1 (* teco:exp-exp teco:exp-val1))
                           (setq teco:exp-op 'start))
                          ((eq teco:exp-op 'div)
                           (setq teco:exp-val1
                                 (if (/= teco:exp-val1 0)
                                     (/ teco:exp-exp teco:exp-val1)
                                   0))
                           (setq teco:exp-op 'start))
                          ((eq teco:exp-op 'and)
                           (setq teco:exp-val1
                                 (logand teco:exp-exp teco:exp-val1))
                           (setq teco:exp-op 'start))
                          ((eq teco:exp-op 'or)
                           (setq teco:exp-val1
                                 (logior teco:exp-exp teco:exp-val1))
                           (setq teco:exp-op 'start)))
                  ;; a solitary '-' yields -1
                  (if (eq teco:exp-op 'sub)
                      (setq teco:exp-val1 -1
                            teco:exp-op 'start
                            teco:exp-flag1 t)))
                ;; dispatch on a type 2 character
                (let ((r (aref teco:exec-2 cmdc)))
                  (if r
                      (funcall r)
                    (teco:error "ILL")))))))))))

;; Type 1 commands

(teco:define-type-1
 ?\^m					; CR
 nil)

(teco:define-type-1
 ?\n					; LF
 nil)

(teco:define-type-1
 ?\^k					; VT
 nil)

(teco:define-type-1
 ?\^l					; FF
 nil)

(teco:define-type-1
 32					; SPC
 nil)

(teco:define-type-1
 ?\e					; ESC
 (if (teco:peek-command ?\e)
     ;; ESC ESC terminates macro or command
     (teco:pop-macro-stack)
   ;; otherwise, consume arguments
   (setq teco:exp-flag1 nil
         teco:exp-flag2 nil
         teco:exp-op 'start)))

(teco:define-type-1
 ?!					; !
 (while (/= (teco:get-command teco:trace) ?!)
   nil)
 (setq teco:at-flag nil))

(teco:define-type-1
 ?@					; @
 ;; set at-flag
 (setq teco:at-flag t))

(teco:define-type-1
 ?:					; :
 ;; is it '::'?
 (if (teco:peek-command ?:)
     (progn
       ;; skip second colon
       (teco:get-command teco:trace)
       ;; set flag to show two colons
       (setq teco:colon-flag 2))
   ;; set flag to show one colon
   (setq teco:colon-flag 1)))

(teco:define-type-1
 ??					; ?
 ;; toggle trace
 (setq teco:trace (not teco:trace)))

(teco:define-type-1
 ?.					; .
 ;; value is point
 (setq teco:exp-val1 (point)
       teco:exp-flag1 t))

(teco:define-type-1
 ?z					; z
 ;; value is point-max
 (setq teco:exp-val1 (point-max)
       teco:exp-flag1 t))

(teco:define-type-1
 ?b					; b
 ;; value is point-min
 (setq teco:exp-val1 (point-min)
       teco:exp-flag1 t))

(teco:define-type-1
 ?h					; h
 ;; value is b,z
 (setq teco:exp-val1 (point-max)
       teco:exp-val2 (point-min)
       teco:exp-flag1 t
       teco:exp-flag2 t
       teco:exp-op 'start))

(teco:define-type-1
 ?\^s					; ^s
 ;; value is - length of last insert, etc.
 (setq teco:exp-val1 teco:ctrl-s
       teco:exp-flag1 t))

(teco:define-type-1
 ?\^y					; ^y
 ;; value is .+^S,.
 (setq teco:exp-val1 (point)
       teco:exp-val2 (+ (point) teco:ctrl-s)
       teco:exp-flag1 t
       teco:exp-flag2 t
       teco:exp-op 'start))

(teco:define-type-1
 ?\(					; \(
 ;; push expression stack
 (teco:push-exp-stack)
 (setq teco:exp-flag1 nil
       teco:exp-flag2 nil
       teco:exp-op 'start))

(teco:define-type-1
 ?\C-^					; ^^
 ;; get next command character
 (setq teco:exp-val1 (teco:get-command teco:trace)
       teco:exp-flag1 t))


;; Type 2 commands
(teco:define-type-2
 ?+					; +
 (setq teco:exp-exp (if teco:exp-flag1 teco:exp-val1 0)
       teco:exp-flag1 nil
       teco:exp-op 'add))

(teco:define-type-2
 ?-					; -
 (setq teco:exp-exp (if teco:exp-flag1 teco:exp-val1 0)
       teco:exp-flag1 nil
       teco:exp-op 'sub))

(teco:define-type-2
 ?*					; *
 (setq teco:exp-exp (if teco:exp-flag1 teco:exp-val1 0)
       teco:exp-flag1 nil
       teco:exp-op 'mult))

(teco:define-type-2
 ?/					; /
 (setq teco:exp-exp (if teco:exp-flag1 teco:exp-val1 0)
       teco:exp-flag1 nil
       teco:exp-op 'div))

(teco:define-type-2
 ?&					; &
 (setq teco:exp-exp (if teco:exp-flag1 teco:exp-val1 0)
       teco:exp-flag1 nil
       teco:exp-op 'and))

(teco:define-type-2
 ?#					; #
 (setq teco:exp-exp (if teco:exp-flag1 teco:exp-val1 0)
       teco:exp-flag1 nil
       teco:exp-op 'or))

(teco:define-type-2
 ?\)					; \)
 (if (or (not teco:exp-flag1) (not teco:exp-stack))
     (teco:error "NAP"))
 (let ((v teco:exp-val1))
   (teco:pop-exp-stack)
   (setq teco:exp-val1 v
         teco:exp-flag1 t)))

(teco:define-type-2
 ?,					; ,
 (if (not teco:exp-flag1)
     (teco:error "NAC"))
 (setq teco:exp-val2 teco:exp-val1
       teco:exp-flag2 t
       teco:exp-flag1 nil))

(teco:define-type-2
 ?\^_					; ^_
 (if (not teco:exp-flag1)
     (teco:error "NAB")
   (setq teco:exp-val1 (lognot teco:exp-val1))))

(teco:define-type-2
 ?\^d					; ^d
 (setq teco:ctrl-r 10
       teco:exp-flag1 nil
       teco:exp-op 'start))

(teco:define-type-2
 ?\^o					; ^o
 (setq teco:ctrl-r 8
       teco:exp-flag1 nil
       teco:exp-op 'start))

(teco:define-type-2
 ?\^r					; ^r
 (if teco:colon-flag
     (progn
       (recursive-edit)
       (setq teco:colon-flag nil))
   (if teco:exp-flag1
       ;; set radix
       (progn
         (if (and (/= teco:exp-val1 8)
                  (/= teco:exp-val1 10)
                  (/= teco:exp-val1 16))
             (teco:error "IRA"))
         (setq teco:ctrl-r teco:exp-val1
               teco:exp-flag1 nil
               teco:exp-op 'start))
     ;; get radix
     (setq teco:exp-val1 teco:ctrl-r
           teco:exp-flag1 t))))

(teco:define-type-2
 ?\^c					; ^c
 (if (teco:peek-command ?\^c)
     ;; ^C^C stops execution
     (throw 'teco:exit nil)
   (if teco:macro-stack
       ;; ^C inside macro exits macro
       (teco:pop-macro-stack)
     ;; ^C in command stops execution
     (throw 'teco:exit nil))))

(teco:define-type-2
 ?\^x					; ^x
 ;; set/get search mode flag, which is case-fold-search
 (let ((x-flag (if case-fold-search 0 -1)))
   (teco:set-var 'x-flag)
   (setq case-fold-search (= x-flag 0))))

(teco:define-type-2
 ?m					; m
 (let ((macro-name (teco:get-qspec nil
                                   (teco:get-command teco:trace))))
   (teco:push-macro-stack)
   (setq teco:command-string (aref teco:qreg-text macro-name)
         teco:command-pointer 0)))

(teco:define-type-2
 ?<					; <
 ;; begin iteration
 (if (and teco:exp-flag1 (<= teco:exp-val1 0))
     ;; if this is not to be executed, just skip the
     ;; intervening stuff
     (teco:find-enditer)
   ;; push iteration stack
   (teco:push-iter-stack teco:command-pointer
                         teco:exp-flag1 teco:exp-val1)
   ;; consume the argument
   (setq teco:exp-flag1 nil
         teco:at-flag nil)))

(teco:define-type-2
 ?>					; >
 ;; end iteration
 (if (not teco:iteration-stack)
     (teco:error "BNI"))
 ;; decrement count and pop conditionally
 (teco:pop-iter-stack nil)
 ;; consume arguments
 (setq teco:exp-flag1 nil
       teco:exp-flag2 nil
       teco:exp-op 'start
       teco:at-flag nil))

(teco:define-type-2
 59					; ;
 ;; semicolon iteration exit
 (if (not teco:iteration-stack)
     (teco:error "SNI"))
 ;; if exit
 (if (if (>= (if teco:exp-flag1
                 teco:exp-val1
               teco:search-result) 0)
         (not teco:colon-flag)
       teco:colon-flag)
     (progn
       (teco:find-enditer)
       (teco:pop-iter-stack t)))
 ;; consume argument and colon
 (setq teco:exp-flag1 nil
       teco:colon-flag nil
       teco:exp-op 'start))

(teco:define-type-2
 ?\"					; \"
 ;; must be an argument
 (if (not teco:exp-flag1)
     (teco:error "NAQ"))
 ;; consume argument
 (setq teco:exp-flag1 nil
       teco:exp-op 'start)
 (let* (;; get the test specification
        (c (aref teco:mapch-l (teco:get-command teco:trace)))
        ;; determine whether the test is true
        (test (cond ((eq c ?a)
                     (/= (logand (aref teco:char-types teco:exp-val1)
                                 1) 0))
                    ((eq c ?c)
                     (/= (logand (aref teco:char-types teco:exp-val1)
                                 2) 0))
                    ((eq c ?d)
                     (/= (logand (aref teco:char-types teco:exp-val1)
                                 4) 0))
                    ((or (eq c ?e) (eq c ?f) (eq c ?u) (eq c ?=))
                     (= teco:exp-val1 0))
                    ((or (eq c ?g) (eq c ?>))
                     (> teco:exp-val1 0))
                    ((or (eq c ?l) (eq c ?s) (eq c ?t) (eq c ?<))
                     (< teco:exp-val1 0))
                    ((eq c ?n)
                     (/= teco:exp-val1 0))
                    ((eq c ?r)
                     (/= (logand (aref teco:char-types teco:exp-val1)
                                 8) 0))
                    ((eq c ?v)
                     (/= (logand (aref teco:char-types teco:exp-val1)
                                 16) 0))
                    ((eq c ?w)
                     (/= (logand (aref teco:char-types teco:exp-val1)
                                 32) 0))
                    (t
                     (teco:error "IQC")))))
   (if (not test)
       ;; if the conditional isn't satisfied, read
       ;; to matching | or '
       ;; ll counts the number of conditionals we are inside of
       (let ((ll 1)
             c)
         (while (> ll 0)
           ;; skip to the next significant character
           (while (progn (setq c (teco:skipto))
                         (and (/= c ?\")
                              (/= c ?|)
                              (/= c ?\')))
             nil)
           (if (= c ?\")
               (setq ll (1+ ll))
             (if (= c ?\')
                 (setq ll (1- ll))
               (if (= ll 1)
                   (setq ll 0)		; for immediate exit if | at ll=1
                 ))))))
   ;; clear at-flag
   (setq teco:at-flag nil)))

(teco:define-type-2
 ?'					; '
 ;; no effect if executing
 ;; clear at-flag
 (setq teco:at-flag nil))

(teco:define-type-2
 ?|					; |
 (let ((ll 1)
       c)
   (while (> ll 0)
     (while (progn (setq c (teco:skipto))
                   (and (/= c ?\")
                        (/= c ?\')))
       nil)
     (if (= c ?\")
         (setq ll (1+ ll))
       (setq ll (1- ll))))))

(teco:define-type-2
 ?u					; u
 (if (not teco:exp-flag1)
     (teco:error "NAU"))
 (aset teco:qreg-number
       (teco:get-qspec 0 (teco:get-command teco:trace))
       teco:exp-val1)
 (setq teco:exp-flag1 teco:exp-flag2	; command's value is second arg
       teco:exp-val1 teco:exp-val2
       teco:exp-flag2 nil
       teco:exp-op 'start))

(teco:define-type-2
 ?q					; q
 ;; Qn is numeric val, :Qn is # of chars, mQn is mth char
 (let ((mm (teco:get-qspec (or teco:colon-flag teco:exp-flag1)
                           (teco:get-command teco:trace))))
   (if (not teco:exp-flag1)
       (setq teco:exp-val1 (if teco:colon-flag
                               ;; :Qn
                               (length (aref teco:qreg-text mm))
                             ;; Qn
                             (aref teco:qreg-number mm))
             teco:exp-flag1 t)
     ;; mQn
     (let ((v (aref teco:qreg-text mm)))
       (setq teco:exp-val1 (condition-case nil
                               (aref v teco:exp-val1)
                             (error -1))
             teco:exp-op 'start)))
   (setq teco:colon-flag nil)))

(teco:define-type-2
 ?%					; %
 (let* ((mm (teco:get-qspec nil (teco:get-command teco:trace)))
        (v (+ (aref teco:qreg-number mm) (teco:get-value 1))))
   (aset teco:qreg-number mm v)
   (setq teco:exp-val1 v
         teco:exp-flag1 t)))

(teco:define-type-2
 ?c					; c
 (let ((p (+ (point) (teco:get-value 1))))
   (if (or (< p (point-min)) (> p (point-max)))
       (teco:error "POP")
     (goto-char p)
     (setq teco:exp-flag2 nil))))

(teco:define-type-2
 ?r					; r
 (let ((p (- (point) (teco:get-value 1))))
   (if (or (< p (point-min)) (> p (point-max)))
       (teco:error "POP")
     (goto-char p)
     (setq teco:exp-flag2 nil))))

(teco:define-type-2
 ?j					; j
 (let ((p (teco:get-value (point-min))))
   (if (or (< p (point-min)) (> p (point-max)))
       (teco:error "POP")
     (goto-char p)
     (setq teco:exp-flag1 nil
           teco:exp-flag2 nil))))

(teco:define-type-2
 ?l					; l
 ;; move forward by lines
 (let* ((ll (teco:line-args))
        (p (+ (car ll) (cdr ll) (- (point)))))
   (if (or (< p (point-min)) (> p (point-max)))
       (teco:error "POP")
     (goto-char p))))

(teco:define-type-2
 ?\C-q					; ^q
 ;; number of characters until the nth line feed
 (setq teco:exp-val1 (teco:lines (teco:get-value 1))
       teco:exp-flag1 t))

(teco:define-type-2
 ?=					; =
 ;; print numeric value
 (if (not teco:exp-flag1)
     (teco:error "NAE"))
 (teco:output (format
               (if (teco:peek-command ?=)
                   ;; at least one more =
                   (progn
                     ;; read past it
                     (teco:get-command teco:trace)
                     (if (teco:peek-command ?=)
                         ;; another?
                         (progn
                           ;; read it too
                           (teco:get-command teco:trace)
                           ;; print in hex
                           "%x")
                       ;; print in octal
                       "%o"))
                 ;; print in decimal
                 "%d")
               teco:exp-val1))
 ;; add newline if no colon
 (if (not teco:colon-flag)
     (teco:output ?\n))
 ;; absorb argument, etc.
 (setq teco:exp-flag1 nil
       teco:exp-flag2 nil
       teco:colon-flag nil
       teco:exp-op 'start))

(teco:define-type-2
 ?\t					; TAB
 (if teco:exp-flag1
     (teco:error "IIA"))
 (let ((text (teco:get-text-arg)))
   (insert ?\t text)
   (setq teco:ctrl-s (- (1+ (length text)))))
 ;; clear arguments
 (setq teco:colon-flag nil
       teco:exp-flag1 nil
       teco:exp-flag2 nil))

(teco:define-type-2
 ?i					; i
 (let ((text (teco:get-text-arg)))
   (if teco:exp-flag1
       ;; if a nI$ command
       (progn
         ;; text argument must be null
         (or (string-equal text "") (teco:error "IIA"))
         ;; insert the character
         (insert teco:exp-val1)
         (setq teco:ctrl-s -1)
         ;; consume argument
         (setq teco:exp-op 'start))
     ;; otherwise, insert the text
     (insert text)
     (setq teco:ctrl-s (- (length text))))
   ;; clear arguments
   (setq teco:colon-flag nil
         teco:exp-flag1 nil
         teco:exp-flag2 nil)))

(teco:define-type-2
 ?t					; t
 (let ((args (teco:line-args)))
   (teco:output (buffer-substring (car args) (cdr args)))))

(teco:define-type-2
 ?v					; v
 (let ((ll (teco:get-value 1)))
   (teco:output (buffer-substring (+ (point) (teco:lines (- 1 ll)))
                                  (+ (point) (teco:lines ll))))))

(teco:define-type-2
 ?\C-a					; ^a
 (teco:output (teco:get-text-arg nil ?\C-a))
 (setq teco:at-flag nil
       teco:colon-flag nil
       teco:exp-flag1 nil
       teco:exp-flag2 nil
       teco:exp-op 'start))

(teco:define-type-2
 ?d					; d
 (if (not teco:exp-flag2)
     ;; if only one argument
     (delete-char (teco:get-value 1))
   ;; if two arguments, treat as n,mK
   (let ((ll (teco:line-args)))
     (delete-region (car ll) (cdr ll)))))

(teco:define-type-2
 ?k					; k
 (let ((ll (teco:line-args)))
   (delete-region (car ll) (cdr ll))))

(teco:define-type-2
 ?\C-u					; ^u
 (let* ((mm (teco:get-qspec nil (teco:get-command teco:trace)))
        (text-arg (teco:get-text-arg))
        (text (if (not teco:exp-flag1)
                  text-arg
                (if (string-equal text-arg "")
                    (char-to-string teco:exp-val1)
                  (teco:error "IIA")))))
   ;; if :, append to the register
   (aset teco:qreg-text mm (if teco:colon-flag
                               (concat (aref teco:qreg-text mm) text)
                             text))
   ;; clear various flags
   (setq teco:exp-flag1 nil
         teco:at-flag nil
         teco:colon-flag nil
         teco:exp-flag1 nil)))

(teco:define-type-2
 ?x					; x
 (let* ((mm (teco:get-qspec nil (teco:get-command teco:trace)))
        (args (teco:line-args))
        (text (buffer-substring (car args) (cdr args))))
   ;; if :, append to the register
   (aset teco:qreg-text mm (if teco:colon-flag
                               (concat (aref teco:qreg-text mm) text)
                             text))
   ;; clear various flags
   (setq teco:at-flag nil
         teco:colon-flag nil)))

(teco:define-type-2
 ?g					; g
 (let ((mm (teco:get-qspec t (teco:get-command teco:trace))))
   (if teco:colon-flag
       (teco:output (aref teco:qreg-text mm))
     (let ((text (aref teco:qreg-text mm)))
       (insert text)
       (setq teco:ctrl-s (- (length text)))))
   (setq teco:colon-flag nil)))

(teco:define-type-2
 ?\[					; \[
 (let ((mm (teco:get-qspec t (teco:get-command teco:trace))))
   (setq teco:qreg-stack
         (cons (cons (aref teco:qreg-text mm)
                     (aref teco:qreg-number mm))
               teco:qreg-stack))))

(teco:define-type-2
 ?\]					; \]
 (let ((mm (teco:get-qspec t (teco:get-command teco:trace))))
   (if teco:colon-flag
       (setq teco:exp-flag1 t
             teco:exp-val1 (if teco:qreg-stack -1 0))
     (if teco:qreg-stack
         (let ((pop (car teco:qreg-stack)))
           (aset teco:qreg-text mm (car pop))
           (aset teco:qreg-number mm (cdr pop))
           (setq teco:qreg-stack (cdr teco:qreg-stack)))
       (teco:error "CPQ")))
   (setq teco:colon-flag nil)))

(teco:define-type-2
 ?\\					; \
 (if (not teco:exp-flag1)
     ;; no argument; read number
     (let ((p (point))
           (sign +1)
           (n 0)
           c)
       (setq c (char-after p))
       (if c
           (if (= c ?+)
               (setq p (1+ p))
             (if (= c ?-)
                 (setq p (1+ p)
                       sign -1))))
       (cond
        ((= teco:ctrl-r 8)
         (while (progn
                  (setq c (char-after p))
                  (and c (>= c ?0) (<= c ?7)))
           (setq p (1+ p)
                 n (+ c -48 (* n 8)))))
        ((= teco:ctrl-r 10)
         (while (progn
                  (setq c (char-after p))
                  (and c (>= c ?0) (<= c ?9)))
           (setq p (1+ p)
                 n (+ c -48 (* n 10)))))
        (t
         (while (progn
                  (setq c (char-after p))
                  (and c
                       (or
                        (and (>= c ?0) (<= c ?9))
                        (and (>= c ?a) (<= c ?f))
                        (and (>= c ?A) (<= c ?F)))))
           (setq p (1+ p)
                 n (+ c (if (> c ?F)
                            ;; convert 'a' to 10
                            -87
                          (if (> c ?9)
                              ;; convert 'A' to 10
                              -55
                            ;; convert '0' to 0
                            -48))
                      (* n 16))))))
       (setq teco:exp-val1 (* n sign)
             teco:exp-flag1 t
             teco:ctrl-s (- (point) p))
       (goto-char p))
   ;; argument: insert it as a digit string
   (insert (format (cond
                    ((= teco:ctrl-r 8) "%o")
                    ((= teco:ctrl-r 10) "%d")
                    (t "%x"))
                   teco:exp-val1))
   (setq teco:exp-flag1 nil
         teco:exp-op 'start)))

(teco:define-type-2
 ?\C-t					; ^t
 (if teco:exp-flag1
     ;; type a character
     (progn
       (teco:output teco:exp-val1)
       (setq teco:exp-flag1 nil))
   ;; input a character
   (if (or (= (logand teco:et-flag 32) 0)
           (input-pending-p))
       ;; input is pending, or we must wait
       (let* ((echo-keystrokes 0)
              (c (read-char)))
         (if (= (logand teco:et-flag 8) 0)
             (teco:output c))
         (setq teco:exp-val1 c
               teco:exp-flag1 t))
     ;; no input is pending, and ET bit 32 is set
     (setq teco:exp-val1 -1
           teco:exp-flag1 t))))

(teco:define-type-2
 ?w					; w
 (if teco:exp-flag1
     (progn
       (recenter teco:exp-val1)
       (setq teco:exp-flag1 nil))
   (redraw-display)))

(teco:define-type-2
 ?s					; s
 (let ((arg (teco:get-text-arg))
       (count (if teco:exp-flag1 teco:exp-val1 1))
       regexp)
   (if (string-equal arg "")
       ;; Retrieve last search string
       (setq regexp teco:last-search-regexp
             arg (aref teco:qreg-text ?_))
     ;; Store this search string
     (setq regexp (teco:parse-search-string arg)
           teco:last-search-regexp regexp)
     (aset teco:qreg-text ?_ arg))
   (let ((result (cond
                  ((eq teco:colon-flag 2)
                   (looking-at regexp))
                  ((> count 0)
                   (re-search-forward regexp nil t count))
                  ((< count 0)
                   (re-search-backward regexp nil t (- count)))
                  (t
                   ;; 0s is always successful
                   t))))
     (if (and result
              (< count 0)
              (= (logand teco:es-flag 1) 1))
         (goto-char (match-end 0)))
     ;; set ctrl-s, if the match was successful
     (if (and result
              (or (/= count 0) (eq teco:colon-flag 2)))
         (setq teco:ctrl-s (- (match-beginning 0) (match-end 0))))
     ;; save result for later ';'
     (setq teco:search-result (if result -1 0))
     ;; if no real or implied colon, error if not found
     (if (and (not result)
              (not teco:colon-flag)
              (not (teco:peek-command 59)))
         (teco:error "SRH"))
     ;; set return results
     (if teco:colon-flag
         (setq teco:exp-flag1 t
               teco:exp-val1 teco:search-result)
       (setq teco:exp-flag1 nil))
     ;; clear other flags
     (setq teco:exp-flag2 nil
           teco:colon-flag nil
           teco:at-flag nil
           teco:exp-op 'start))))

(defun teco:parse-search-string (s)
  (let ((i 0)
        (l (length s))
        (r "")
        c)
    (while (< i l)
      (setq r (concat r (teco:parse-search-string-1))))
    r))

(defun teco:parse-search-string-1 ()
  (if (>= i l)
      (teco:error "ISS"))
  (setq c (aref s i))
  (setq i (1+ i))
  (cond
   ((eq c ?\C-e)			; ^E - special match characters
    (teco:parse-search-string-e))
   ((eq c ?\C-n)			; ^Nx - match all but x
    (teco:parse-search-string-n))
   ((eq c ?\C-q)			; ^Qx - use x literally
    (teco:parse-search-string-q))
   ((eq c ?\C-s)			; ^S - match separator chars
    "[^A-Za-z0-9]")
   ((eq c ?\C-x)			; ^X - match any character
    "[\000-\377]")
   (t					; ordinary character
    (teco:parse-search-string-char c))))

(defun teco:parse-search-string-char (c)
  (regexp-quote (char-to-string c)))

(defun teco:parse-search-string-q ()
  (if (>= i l)
      (teco:error "ISS"))
  (setq c (aref s i))
  (setq i (1+ i))
  (teco:parse-search-string-char c))

(defun teco:parse-search-string-e ()
  (if (>= i l)
      (teco:error "ISS"))
  (setq c (aref s i))
  (setq i (1+ i))
  (cond
   ((or (eq c ?a) (eq c ?A))		; ^EA - match alphabetics
    "[A-Za-z]")
   ((or (eq c ?c) (eq c ?C))		; ^EC - match symbol constituents
    "[A-Za-z.$]")
   ((or (eq c ?d) (eq c ?D))		; ^ED - match numerics
    "[0-9]")
   ((eq c ?g)				; ^EGq - match any char in q-reg
    (teco:parse-search-string-e-g))
   ((or (eq c ?l) (eq c ?L))		; ^EL - match line terminators
    "[\012\013\014]")
   ((eq c ?q)				; ^EQq - use contents of q-reg
    (teco:parse-search-string-e-q))
   ((eq c ?r)				; ^ER - match alphanumerics
    "[A-Za-z0-9]")
   ((eq c ?s)				; ^ES - match non-null space/tab seq
    "[ \t]+")
   ((eq c ?v)				; ^EV - match lower case alphabetic
    "[a-z]")
   ((eq c ?w)				; ^EW - match upper case alphabetic
    "[A-Z]")
   ((eq c ?x)				; ^EX - match any character
    "[\000-\377]")
   (t
    (teco:error "ISS"))))

(defun teco:parse-search-string-e-q ()
  (if (>= i l)
      (teco:error "ISS"))
  (setq c (aref s i))
  (setq i (1+ i))
  (regexp-quote (aref teco:q-reg-text c)))

(defun teco:parse-search-string-e-g ()
  (if (>= i l)
      (teco:error "ISS"))
  (setq c (aref s i))
  (setq i (1+ i))
  (let* ((q (aref teco:qreg-text c))
         (len (length q))
         (null (= len 0))
         (one-char (= len 1))
         (dash-present (string-match "-" q))
         (caret-present (string-match "\\^" q))
         (outbracket-present (string-match "]" q))
         p)
    (cond
     (null
      "[^\000-\377]")
     (one-char
      (teco:parse-search-string-char c))
     (t
      (while (setq p (string-match "^]\\^" q)) ;FIXME
        (setq q (concat (substring q 1 p) (substring q (1+ p)))))
      (concat
       "["
       (if outbracket-present "]" "")
       (if dash-present "---" "")
       q
       (if caret-present "^" ""))))))

(defun teco:parse-search-string-n ()
  (let ((p (teco:parse-search-string-1)))
    (cond
     ((= (aref p 0) ?\[)
      (if (= (aref p 1) ?^)
          ;; complement character set
          (if (= (length p) 4)
              ;; complement of one character
              (teco:parse-search-string-char (aref p 2))
            ;; complement of more than one character
            (concat "[" (substring p 2)))
        ;; character set - invert it
        (concat "[^" (substring p 1))))
     ((= (aref p 0) ?\\)
      ;; single quoted character
      (concat "[^" (substring p 1) "]"))
     (t
      ;; single character
      (if (string-equal p "-")
          "[^---]"
        (concat "[^" p "]"))))))

(defun teco:substitute-text-string (s)
  (let ((i 0)
        (l (length s))
        (r "")
        c)
    (while (< i l)
      (setq r (concat r (teco:substitute-text-string-1))))
    r))

(defun teco:substitute-text-string-1 ()
  (if (>= i l)
      (teco:error "ISS"))
  (setq c (aref s i))
  (setq i (1+ i))
  (cond
   ((eq c ?\C-e)			; ^E - special string characters
    (teco:substitute-text-string-e))
   ((eq c ?\C-q)			; ^Qx - use x literally
    (teco:substitute-text-string-q))
   (t					; ordinary character
    (char-to-string c))))

(defun teco:substitute-text-string-q ()
  (if (>= i l)
      (teco:error "ISS"))
  (setq c (aref s i))
  (setq i (1+ i))
  (char-to-string c))

(defun teco:substitute-text-string-e ()
  (if (>= i l)
      (teco:error "ISS"))
  (setq c (aref s i))
  (setq i (1+ i))
  (cond
   ((eq c ?q)				; ^EQq - use contents of q-reg
    (teco:substitute-text-string-e-q))
   (t
    (teco:error "ISS"))))

(defun teco:substitute-text-string-e-q ()
  (if (>= i l)
      (teco:error "ISS"))
  (setq c (aref s i))
  (setq i (1+ i))
  (aref teco:q-reg-text c))

(teco:define-type-2
 ?o					; o
 (let ((label (teco:get-text-arg))
       (index (and teco:exp-flag1 teco:exp-val1)))
   (setq teco:exp-flag1 nil)
   ;; handle computed goto by extracting the proper label
   (if index
       (if (< index 0)
           ;; argument < 0 is a noop
           (setq label "")
         ;; otherwise, find the n-th label (0-origin)
         (setq label (concat label ","))
         (let ((p 0)
               q)
           (while (and (> index 0)
                       (setq p (string-match "," label p))
                       (setq p (1+ p)))
             (setq index (1- index)))
           (setq q (string-match "," label p))
           (setq label (substring label p q)))))
   ;; if the label is non-null, find the correct label
   ;; start from beginning of iteration or macro, and look for tag
   (setq teco:command-pointer
         (if teco:iteration-stack
             ;; if in iteration, start at beginning of iteration
             (aref (car teco:iteration-stack) 0)
           ;; if not in iteration, start at beginning of command or macro
           0))
   ;; search for tag
   (catch 'label
     (let ((level 0)
           c p l)
       ;; look for interesting things, including !
       (while t
         (setq c (teco:skipto t))
         (cond
          ((= c ?<)			; start of iteration
           (setq level (1+ level)))
          ((= c ?>)			; end of iteration
           (if (= level 0)
               (teco:pop-iter-stack t)
             (setq level (1- level))))
          ((= c ?!)			; start of tag
           (setq p (string-match "!" teco:command-string teco:command-pointer))
           (if (and p
                    (string-equal label (substring teco:command-string
                                                   teco:command-pointer
                                                   p)))
               (progn
                 (setq teco:command-pointer (1+ p))
                 (throw 'label nil))))))))))

(teco:define-type-2
 ?a					; :a
 ;; 'a' must be used as ':a'
 (if (and teco:exp-flag1 teco:colon-flag)
     (let ((char (+ (point) teco:exp-val1)))
       (setq teco:exp-val1
             (if (and (>= char (point-min))
                      (< char (point-max)))
                 (char-after char)
               -1)
             teco:colon-flag nil))
   (teco:error "ILL")))

(teco:define-type-2
 ?e					; e
 (let ((c (teco:get-command teco:trace)))
   (cond
    ((= c ?t) (teco:set-var 'teco:et-flag))
    ((= c ?s) (teco:set-var 'teco:es-flag))
    (t (teco:error "IEC")))))

(teco:define-type-2
 ?f					; f
 (let ((c (teco:get-command teco:trace)))
   (cond
    ((= c ?e) (teco:fe-command))
    ((= c ?l) (teco:fl-command))
    ((= c ?r) (teco:fr-command))
    ((= c ?s) (teco:fs-command))
    ((= c ?w) (teco:fw-command))
    (t (teco:error "IFC")))))

(defun teco:fe-command ()
  "Aka fe."
  (let ((text (teco:substitute-text-string (teco:get-text-arg))))
    (prin1-to-string (eval (read text)))))

(defun teco:fl-command ()
  "Aka fl."
  (let ((count (teco:get-value 1))
        (start (point)))
    ;; argument 0 is a no-op
    (if (/= count 0)
        (forward-sexp count))
    ;; get the result values into the arguments
    (setq teco:exp-val2 start
          teco:exp-flag2 t
          teco:exp-val1 (point)
          teco:exp-flag1 t
          teco:colon-flag nil)
    ;; don't move the point
    (goto-char start)))

(defun teco:fr-command ()
  "Aka fr."
  (let ((text (teco:get-text-arg)))
    ;; delete the previous match
    (delete-char teco:ctrl-s)
    ;; insert the argument
    (insert text)
    ;; set ^S for the insertion
    (setq teco:ctrl-s (- (length text)))))

(defun teco:fs-command ()
  "Aka fs."
  (let* ((args (teco:get-two-text-args))
         (search (car args))
         (replace (car (cdr args)))
         regexp)
    (if (string-equal search "")
        ;; Retrieve last search string
        (setq regexp teco:last-search-regexp
              search (aref teco:qreg-text ?_))
      ;; Store this search string
      (setq regexp (teco:parse-search-string search)
            teco:last-search-regexp regexp)
      (aset teco:qreg-text ?_ search))
    (let ((result (re-search-forward regexp nil t)))
      ;; save result for later ';'
      (setq teco:search-result (if result -1 0))
      ;; if no real or implied colon, error if not found
      (if (and (not result)
               (not teco:colon-flag)
               (not (teco:peek-command 59)))
          (teco:error "SRH"))
      ;; set return results
      (if teco:colon-flag
          (setq teco:exp-flag1 t
                teco:exp-val1 teco:search-result)
        (setq teco:exp-flag1 nil))
      ;; clear other flags
      (setq teco:exp-flag2 nil
            teco:colon-flag nil
            teco:at-flag nil
            teco:exp-op 'start)
      (if result
          (progn
            ;; delete the match
            (delete-char (- (match-end 0) (match-beginning 0)))
            ;; insert the argument
            (insert replace)
            ;; set ^S for the insertion
            (setq teco:ctrl-s (- (length replace))))))))

(defun teco:fw-command ()
  "Aka fw."
  (let ((count (teco:get-value 1))
        (start (point)))
    ;; argument 0 is a no-op
    (if (/= count 0)
        (progn
          (forward-word count)
          ;; If : is present, back off to the near side of the last word
          ;; found.  Make sure we don't run past the starting position.
          (if teco:colon-flag
              (if (> count 0)
                  ;; Searching forward
                  (progn
                    (forward-word -1)
                    (if (< (point) start)
                        (goto-char start)))
                ;; Searching backward
                (progn
                  (forward-word 1)
                  (if (> (point) start)
                      (goto-char start)))))))
    ;; get the result values into the arguments
    (setq teco:exp-val2 start
          teco:exp-flag2 t
          teco:exp-val1 (point)
          teco:exp-flag1 t
          teco:colon-flag nil)
    ;; don't move the point
    (goto-char start)))

;; Routines to get next character from command buffer
;; getcmdc0, when reading beyond command string, pops
;; macro stack and continues.
;; getcmdc, in similar circumstances, reports an error.
;; If pushcmdc() has returned any chars, read them first
;; routines type characters as read, if argument != 0.

(defun teco:get-command0 (trace)
  "Get the next character TRACE."
  (let (char)
    (while (not (condition-case nil
                    (setq char (aref teco:command-string teco:command-pointer))
                  ;; if we've exhausted the string, pop the macro stack
                  ;; if we exhaust the macro stack, exit
                  (error (teco:pop-macro-stack)
                         nil))))
    ;; bump the command pointer
    (setq teco:command-pointer (1+ teco:command-pointer))
    ;; trace, if requested
    (and trace (teco:trace-type char))
    ;; return the character
    char))

(defun teco:get-command (trace)
  "Get the next character TRACE."
  (let ((char (condition-case nil
                  (aref teco:command-string teco:command-pointer)
                ;; if we've exhausted the string, give error
                (error
                 (teco:error (if teco:macro-stack "UTM" "UTC"))))))
    ;; bump the command pointer
    (setq teco:command-pointer (1+ teco:command-pointer))
    ;; trace, if requested
    (and trace (teco:trace-type char))
    ;; return the character
    char))

;; peek at next char in command string, return 1 if it is equal
;; (case independent) to argument

(defun teco:peek-command (arg)
  (condition-case nil
      (eq (aref teco:mapch-l (aref teco:command-string teco:command-pointer))
          (aref teco:mapch-l arg))
    (error nil)))

(defun teco:get-text-arg (&optional term-char default-term-char)
  ;; figure out what the terminating character is
  (setq teco:term-char (or term-char
                           (if teco:at-flag
                               (teco:get-command teco:trace)
                             (or default-term-char
                                 ?\e)))
        teco:at-flag nil)
  (let ((s "")
        c)
    (while (progn
             (setq c (teco:get-command teco:trace))
             (/= c teco:term-char))
      (setq s (concat s (char-to-string c))))
    s))

(defun teco:get-two-text-args (&optional term-char default-term-char)
  ;; figure out what the terminating character is
  (setq teco:term-char (or term-char
                           (if teco:at-flag
                               (teco:get-command teco:trace)
                             (or default-term-char
                                 ?\e)))
        teco:at-flag nil)
  (let ((s1 "")
        (s2 "")
        c)
    (while (progn
             (setq c (teco:get-command teco:trace))
             (/= c teco:term-char))
      (setq s1 (concat s1 (char-to-string c))))
    (while (progn
             (setq c (teco:get-command teco:trace))
             (/= c teco:term-char))
      (setq s2 (concat s2 (char-to-string c))))
    (list s1 s2)))

;; Routines to manipulate the stacks

;; Pop the macro stack.  Throw to 'teco:exit' if the stack is empty.
(defun teco:pop-macro-stack ()
  (if teco:macro-stack
      (let ((frame (car teco:macro-stack)))
        (setq teco:macro-stack (cdr teco:macro-stack)
              teco:command-string (aref frame 0)
              teco:command-pointer (aref frame 1)
              teco:iteration-stack (aref frame 2)
              teco:cond-stack (aref frame 3)
              teco:at-flag nil))
    (throw 'teco:exit nil)))

;; Push the macro stack.
(defun teco:push-macro-stack ()
  (setq teco:macro-stack
        (cons (vector teco:command-string
                      teco:command-pointer
                      teco:iteration-stack
                      teco:cond-stack)
              teco:macro-stack)))

;; Pop the expression stack.
(defun teco:pop-exp-stack ()
  (let ((frame (car teco:exp-stack)))
    (setq teco:exp-stack (cdr teco:exp-stack)
          teco:exp-val1 (aref frame 0)
          teco:exp-flag1 (aref frame 1)
          teco:exp-val2 (aref frame 2)
          teco:exp-flag2 (aref frame 3)
          teco:exp-exp (aref frame 4)
          teco:exp-op (aref frame 5))))

;; Push the expression stack.
(defun teco:push-exp-stack ()
  (setq teco:exp-stack
        (cons (vector teco:exp-val1
                      teco:exp-flag1
                      teco:exp-val2
                      teco:exp-flag2
                      teco:exp-exp
                      teco:exp-op)
              teco:exp-stack)))

;; Pop the iteration stack
;; if arg t, exit unconditionally
;; else check exit conditions and exit or reiterate
(defun teco:pop-iter-stack (arg)
  (let ((frame (car teco:iteration-stack)))
    (if (or arg
            (and ;; without argument, iterate indefinitely
             (aref frame 1)
             ;; test against 1, since one iteration has already been done
             (<= (aref frame 2) 1)))
        ;; exit iteration
        (setq teco:iteration-stack (cdr teco:iteration-stack))
      ;; continue with iteration
      ;; decrement count
      (and (aref frame 1)
           (aset frame 2 (1- (aref frame 2))))
      ;; reset command pointer
      (setq teco:command-pointer (aref frame 0)))))

;; Push the iteration stack
(defun teco:push-iter-stack (pointer flag count)
  (setq teco:iteration-stack
        (cons (vector pointer
                      flag
                      count)
              teco:iteration-stack)))

(defun teco:find-enditer ()
  (let ((icnt 1)
        c)
    (while (> icnt 0)
      (while (progn (setq c (teco:skipto))
                    (and (/= c ?<)
                         (/= c ?>))))
      (if (= c ?<)
          (setq icnt (1+ icnt))
        (setq icnt (1- icnt))))))


;; I/O routines

(defvar teco:output-buffer (get-buffer-create "*Teco Output*")
  "The buffer into which Teco output is written.")

(defun teco:out-init ()
  "Recreate the teco output buffer, if necessary."
  (setq teco:output-buffer (get-buffer-create "*Teco Output*"))
  (with-current-buffer teco:output-buffer
    ;; get a fresh line in output buffer
    (goto-char (point-max))
    (insert ?\n)
    ;; remember where to start displaying
    (setq teco:output-start (point))
    ;; clear minibuffer, in case we have to display in it
    (save-window-excursion
      (select-window (minibuffer-window))
      (erase-buffer))
    ;; if output is visible, position it correctly
    (let ((w (get-buffer-window teco:output-buffer)))
      (if w
          (progn
            (set-window-start w teco:output-start)
            (set-window-point w teco:output-start))))))

(defun teco:output (s)
  ;; Do no work if output is "".  Also, this avoids an error condition.
  (if (not (and (stringp s) (string-equal s "")))
      (let ((w (get-buffer-window teco:output-buffer))
            (b (current-buffer))
            (sw (selected-window)))
        ;; Put the text in the output buffer
        (set-buffer teco:output-buffer)
        (goto-char (point-max))
        (insert s)
        (let ((p (point)))
          (set-buffer b)
          (if w
              ;; if output is visible, move the window point to the end
              (set-window-point w p)
            ;; Otherwise, we have to figure out how to display the text
            ;; Has a newline followed by another character been added to the
            ;; output buffer?  If so, we have to make the output buffer
            ;; visible.
            (if (with-current-buffer teco:output-buffer
                  (backward-char 1)
                  (search-backward "\n" teco:output-start t))
                ;; a newline has been seen, clear the minibuffer and make the
                ;; output buffer visible
                (progn
                  (save-window-excursion
                    (select-window (minibuffer-window))
                    (erase-buffer))
                  (let ((pop-up-windows t))
                    (pop-to-buffer teco:output-buffer)
                    (goto-char p)
                    (set-window-start w teco:output-start)
                    (set-window-point w p)
                    (select-window sw)))
              ;; a newline has not been seen, add output to minibuffer
              (save-window-excursion
                (select-window (minibuffer-window))
                (goto-char (point-max))
                (insert s))))))))

;; Output a character of tracing information
(defun teco:trace-type (c)
  (teco:output (if (= c ?\e)
                   ?$
                 c)))

;; Report an error
(defun teco:error (code)
  ;; save the command with the error
  (aset teco:qreg-text ?%
        (substring teco:command-string 0 teco:command-pointer))
  (let ((text (cdr (assoc code teco:error-texts))))
    (teco:output (concat (if (with-current-buffer teco:output-buffer
                               (/= (point) teco:output-start))
                             "\n"
                           "")
                         ;; due to the test in teco:output and Emacs' handling
                         ;; of trailing newlines in the minibuffer-window,
                         ;; we can have a newline at the end of the error
                         ;; message and it will not force the output display
                         ;; from the minibuffer into the Teco output buffer
                         "? " code " " text "\n"))
    (beep)
    (if debug-on-error (debug nil code text))
    (throw 'teco:exit nil)))


;; Utility routines

;; Convert character to q-register name
;; If file-or-search is t, allow special q-reg names
(defun teco:get-qspec (file-or-search char)
  ;; lower-case char
  (setq char (aref teco:mapch-l char))
  ;; test that it's valid
  (if (= (logand (aref teco:qspec-valid char) (if file-or-search 2 1)) 0)
      (teco:error "IQN"))
  char)

;; Set or get value of a variable
(defun teco:set-var (variable)
  ;; If there is an argument, then set the variable
  (if teco:exp-flag1
      (progn
        (set variable
             (if teco:exp-flag2
                 ;; If there are two arguments, then they are bits to clear
                 ;; and bits to set
                 (logior (logand (lognot (symbol-value variable))
                                 teco:exp-val2)
                         teco:exp-val1)
               ;; One argument is the new value alone
               teco:exp-val1))
        (setq teco:exp-flag1 nil
              teco:exp-flag2 nil))
    ;; No arguments mean to fetch the variable's value
    (setq teco:exp-val1 (symbol-value variable)
          teco:exp-flag1 t)))

;; Get numeric argument
(defun teco:get-value (default)
  (prog1
      (if teco:exp-flag1
          teco:exp-val1
        (if (eq teco:exp-op 'sub)
            (- default)
          default))
    ;; consume argument
    (setq teco:exp-flag1 nil
          teco:exp-op 'start)))

;; Get argument measuring in lines
(defun teco:lines (r)
  (- (save-excursion
       (if (> r 0)
           (if (search-forward "\n" nil t r)
               (point)
             (point-max))
         (if (search-backward "\n" nil t (- 1 r))
             (1+ (point))
           (point-min))))
     (point)))

;; routine to handle args for K, T, X, etc.
;; if two args, 'char x' to 'char y'
;; if just one arg, then n lines (default 1)
(defun teco:line-args ()
  (prog1
      (if teco:exp-flag2
          (cons teco:exp-val1 teco:exp-val2)
        (cons (point) (+ (point) (teco:lines (if teco:exp-flag1
                                                 teco:exp-val1
                                               1)))))
    (setq teco:exp-flag1 nil
          teco:exp-flag2 nil)))

;; routine to skip to next ", ', |, <, or >
;; skips over these chars embedded in text strings
;; stops in ! if argument is t
;; returns character found
(defun teco:skipto (&optional arg)
  (catch 'teco:skip
    (let (;; "at" prefix
          (atsw nil)
          ;; temp attributes
          ta
          ;; terminator
          term
          skipc)
      (while t				; forever
        (while (progn
                 (setq skipc (teco:get-command nil)
                       ta (aref teco:spec-chars skipc))
                 (cond
                  ;; if char is ^, treat next char as control
                  ((eq skipc ?^)
                   (setq skipc (logand 31 (teco:get-command nil))
                         ta (aref teco:spec-chars skipc)))
                  ;; if char is E or F, pick up next char and interpret the
                  ;; two-character sequence
                  ((eq skipc ?e)
                   (setq skipc (teco:get-command nil)
                         ta (logand -259 (aref teco:spec-chars skipc)))
                   (if (/= (logand ta 4) 0)
                       (setq ta (logior ta 2))))
                  ((eq skipc ?f)
                   (setq skipc (teco:get-command nil)
                         ta (logand -259 (aref teco:spec-chars skipc)))
                   (if (/= (logand ta 8) 0)
                       (setq ta (logior ta 2)))
                   (if (/= (logand ta 512) 0)
                       (setq ta (logior ta 256)))))
                 (= (logand ta 307) 0))	; read until something interesting
                                        ; found
          nil)
        (if (/= (logand ta 32) 0)
            (teco:get-command nil))	; if command takes a Q spec,
                                        ; skip the spec
        (if (/= (logand ta 16) 0)	; sought char found: quit
            (progn
              (if (= skipc ?\")		; quote must skip next char
                  (teco:get-command nil))
              (throw 'teco:skip skipc)))
        (if (/= (logand ta 1) 0)	; other special char
            (cond
             ((eq skipc ?@)		; use alternative text terminator
              (setq atsw t))
             ((eq skipc ?\C-^)		; ^^ is value of next char
                                        ; skip that char
              (teco:get-command nil))
             ((eq skipc ?\C-a)		; type text
              (setq term (if atsw (teco:get-command nil) ?\C-a)
                    atsw nil)
              (while (/= (teco:get-command nil) term)
                nil))			; skip text
             ((eq skipc ?!)		; tag
              (if arg
                  (throw 'teco:skip skipc))
              (while (/= (teco:get-command nil) ?!)
                nil))			; skip until next !
             ((or (eq skipc ?e)
                  (eq skipc ?f))	; first char of two-letter E or F
                                        ; command
              nil)))			; not implemented
        (if (/= (logand ta 2) 0)	; command with a text
                                        ; argument
            (progn
              (setq term (if atsw (teco:get-command nil) ?\e)
                    atsw nil)
              (while (/= (teco:get-command nil) term)
                nil)			; skip text
              ))
        (if (/= (logand ta 256) 0)	; command with a double text
                                        ; argument
            (progn
              (setq term (if atsw (teco:get-command nil) ?\e)
                    atsw nil)
              (while (/= (teco:get-command nil) term)
                nil)			; skip text
              (while (/= (teco:get-command nil) term)
                nil)			; skip second text
              (if (/= (logand ta 1024) 0)
                  (setq atsw nil))		; transfer command clears @-flag
                                        ; after executing
              ))))))


;; Input handling

(defvar teco:command-keymap
  (list 'keymap (make-vector 128 'teco:command-self-insert))
  "Keymap used while reading teco commands.")

(define-key teco:command-keymap "\^c" 'teco:command-quit)
(define-key teco:command-keymap "\^g" 'teco:command-quit)
(define-key teco:command-keymap "\^l" 'teco:command-ctrl-l)
(define-key teco:command-keymap "\^m" 'teco:command-return)
(define-key teco:command-keymap "\^u" 'teco:command-ctrl-u)
(define-key teco:command-keymap "\e" 'teco:command-escape)
(define-key teco:command-keymap "\^?" 'teco:command-delete)
(define-key teco:command-keymap "?" 'teco:command-query)
(define-key teco:command-keymap "/" 'teco:command-slash)
(define-key teco:command-keymap "*" 'teco:command-star)

(defvar teco:command-display-table
  (let ((table (make-display-table)))
    (aset table ?\e [?$])
    table)
  "Display table used while reading teco commands.")

(defun teco:copy-to-q-reg (char start end)
  "Copy region into Teco q-reg REG.
When called from program, takes three args: REG, START, and END.
START and END are buffer positions indicating what to copy."
  (interactive "cCopy region to q-reg: \nr")
  (setq char (aref teco:mapch-l char))
  ;; test that it's valid
  (if (= (logand (aref teco:qspec-valid char) 1) 0)
      (error "? IQN Invalid Q-reg name"))
  (aset teco:qreg-text char (buffer-substring start end)))

(defun teco:command ()
  "Read and execute a Teco command string."
  (interactive)
  (let ((command (teco:read-command)))
    (if command
        (progn
          (setq teco:output-buffer (get-buffer-create "*Teco Output*"))
          (with-current-buffer teco:output-buffer
            (goto-char (point-max))
            (insert teco:prompt command))
          (teco:execute-command command)))))
(defalias 'teco 'teco:command)

(defun teco:read-command ()
  "Read a teco command string from the user."
  (minibuffer-with-setup-hook
      (lambda ()
        (setq buffer-display-table teco:command-display-table))
    (catch 'teco:command-quit
      (read-from-minibuffer teco:prompt nil teco:command-keymap))))

(defun teco:command-self-insert ()
  (interactive)
  (teco:command-insert-character last-command-event))

(defun teco:command-quit ()
  (interactive)
  (beep)
  (throw 'teco:command-quit nil))

(defun teco:command-ctrl-l ()
  (interactive)
  (redraw-display))

(defun teco:command-return ()
  (interactive)
  (setq last-command-event ?\n)
  (teco:command-self-insert))

(defun teco:command-escape ()
  (interactive)
  ;; Two ESCs in a row terminate the command string
  (if (eq last-command 'teco:command-escape)
      (throw 'teco:command-quit (minibuffer-contents-no-properties)))
  (teco:command-insert-character 27))

(defun teco:command-ctrl-u ()
  (interactive)
  ;; delete the characters
  (kill-line 0)
  ;; decide whether to shrink the window
  (while (let ((a (insert ?\n))
               (b (pos-visible-in-window-p))
               (c (backward-delete-char 1)))
           b)
    (shrink-window 1)))

(defun teco:command-delete ()
  (interactive)
  ;; delete the character
  (backward-delete-char 1)
  ;; decide whether to shrink the window
  (insert ?\n)
  (if (prog1 (pos-visible-in-window-p)
        (backward-delete-char 1))
      (shrink-window 1)))

(defun teco:command-query ()
  (interactive)
  ;; first input char sees last-command equal to 't
  (if (eq last-command t)
      ;; if first character of command, insert erroneous command
      (let* ((s (aref teco:qreg-text ?%))
             (l (length s))
             (i 0))
        (while (< i l)
          (teco:command-insert-character (aref s i))
          (setq i (1+ i))))
    ;; otherwise, just insert the character
    (teco:command-self-insert)))

(defun teco:command-slash ()
  (interactive)
  ;; first input char sees last-command equal to 't
  (if (eq last-command t)
      ;; if first character of command, insert last command
      (let* ((s (aref teco:qreg-text ?#))
             (l (length s))
             (i 0))
        (while (< i l)
          (teco:command-insert-character (aref s i))
          (setq i (1+ i))))
    ;; otherwise, just insert the character
    (teco:command-self-insert)))

(defun teco:command-star ()
  (interactive)
  ;; first input char sees last-command equal to 't
  (if (eq last-command t)
      ;; if first character of command, offer to save previous command in
      ;; q-register
      (progn
        ;; insert the * into the buffer
        (teco:command-insert-character last-command-event)
        ;; read the next character
        (let ((c (read-char))
              c1)
          ;; test if it is a valid q-reg name
          (setq c1 (aref teco:mapch-l c))
          (if (/= (logand (aref teco:qspec-valid c1) 1) 0)
              ;; if so, store the command, give a message, and abort command
              (progn
                (aset teco:qreg-text c1 (aref teco:qreg-text ?#))
                (message "Last Teco command stored in q-register %c" c1)
                (throw 'teco:command-quit nil))
            ;; if q-reg name is invalid, just insert the character
            (beep)
            (teco:command-insert-character c))))
    ;; otherwise, just insert the character
    (teco:command-self-insert)))

;; Insert a single command character
(defun teco:command-insert-character (c)
  (insert c)
  (if (not (pos-visible-in-window-p))
      (enlarge-window 1)))

(provide 'teco)
;;; teco.el ends here

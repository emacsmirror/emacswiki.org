;;; cobol-mode.el --- Handles only what little Cobol I happen to know

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Rick Bielawski <rbielaws@i1.net>
;; Keywords: languages, COBOL, Tandem, Guardian, NSK
;; Maintainer: Rick Bielawski <rbielaws@i1.net>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; COBOL -- COmmon Business Oriented Language. ()

;; This is currently a work-in-progress.  I believe comments and strings are
;; being handled correctly and some keywords are highlighted.  I'm only
;; posting this in such a crude stage as a method of synchronizing changes
;; among the various computers I might work on this from.  You are welcome to
;; try it and submit changes or report bugs but there is no point trying to
;; report lack of features or missing keywords unless you are including
;; patches that address such shortcomings.
;;
;; Since I use this mode on Tandem/Guardian/NSK source files for Scobol and
;; Cobol this mode handles the extensions implemented by those compilers.
;; It may not correctly handle standard features that are not implemented
;; by those compilers.
;;
;; I wrote this because I didn't like the only other Cobol mode I could find.
;; The other one is on the Azundris site.  It deletes periods and commas if
;; you don't change their syntax properties.  It also doesn't have anything
;; close to this version's handling of comments and strings.  But it does do
;; many things this one doesn't.  A given individual may decide either is the
;; lesser of two evils.  This one is certainly better than nothing.

;;; Installing:

;; Before you can use cobol-mode, emacs needs to be able to find it.  Place
;; the cobol-mode.el file in a directory on the load-path; typically the
;; .../site-lisp or perhaps .../lisp/progmods directory.  Usually you would
;; also want to byte compile cobol-mode.el but this is not required.  To do
;; this, visit the cobol-mode.el file, type: M-x emacs-lisp-byte-compile <ret>
;; There should be no warnings or errors during byte compilation.
;;
;; There are 4 basic ways to use COBOL-MODE on a file.  The first method
;; manually selects cobol-mode as the editing mode.  The other 3 cause emacs
;; to recognize automatically that you want to visit the file using
;; cobol-mode.
;;
;; Pick one:
;; 1. While visiting a file, type: M-x cobol-mode <ret>
;; 2. Put the string -*-cobol-*- in a comment on the first line of the file.
;;    Save the file and close it.  Now any time you open it cobol-mode starts.
;; 3. Create an association between a particular file naming convention and
;;    cobol-mode.  This is done by adding an association to auto-mode-alist.
;; For example:
;; (setq auto-mode-alist
;;   (append
;;     '(("\\.cob\\'" . cobol-mode)         ;extension of .cob means cobol-mode
;;       ("\\([\\/]\\|^\\)[^.]+$" . cobol-mode)) ;so does no extension at all.
;;    auto-mode-alist))
;; 4. Advise set-auto-mode to look at the buffer contents upon loading.
;;
;; The above all tell emacs that you want to use cobol-mode but you must load
;; cobol-mode before you can use it.  There are 2 methods of telling emacs to
;; load the cobol-mode routines.  The first unconditionally loads cobol-mode
;; definitions immediately.  The second tells emacs to automatically load
;; cobol-mode only when you try to use it.  Add one of the following lines to
;; your .emacs file.
;;
;;(require 'cobol-mode)      ; Unconditional load
;;(autoload 'cobol-mode "cobol-mode" "Major mode for Tandem COBOL files." t nil)
;;
;; Please report any bugs!

;;; History:

;; 2005-10-10 RGB Started writing this mode using my tal-mode as skeleton.
;;                Vestiges of that mode are still scattered throughout.
;; 2005-10-19 RGB Removed column-marker & popup-ruler stuff.
;;                These are now standalone packages here:
;;                http://www.emacswiki.org/cgi-bin/emacs/column-marker.el
;;                http://www.emacswiki.org/cgi-bin/emacs/popup-ruler.el
;;                Added automatic detection and support for above packages.

;;; Code:

(defgroup cobol nil
  "Major mode for editing COBOL source files in Emacs.
While in cobol-mode use C-h m for a description of the mode's features."
  :prefix 'cobol-
  :group 'languages)

;;; KEY MAP

(defvar cobol-skeleton-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?i]   'cobol-if-skel)
    map)
  "Keymap for `cobol-mode'.")

(defvar cobol-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab]           'indent-according-to-mode)
    (define-key map [?\C-c ?\C-c]   'column-marker-here)
    (define-key map [?\C-c ?\C-f]   'auto-fill-mode)
    (define-key map [?\C-c ?\C-r]   'popup-ruler)
    (define-key map [?\C-c ?\C-s]    cobol-skeleton-map)
    (define-key map [?\C-c return]  'comment-indent-new-line)
    map)
  "Keymap for `cobol-mode'.")

(defvar cobol-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?\! "." st)
    (modify-syntax-entry ?\" "." st)  ; wiki \" bug workaround comment
    (modify-syntax-entry ?\# "w" st)
    (modify-syntax-entry ?\$ "w" st)
    (modify-syntax-entry ?\% "'" st)
    (modify-syntax-entry ?\& "'" st)
    (modify-syntax-entry ?\' "." st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\* "." st)
    (modify-syntax-entry ?\+ "." st)
    (modify-syntax-entry ?\, "." st)
    (modify-syntax-entry ?\- "w" st)
    (modify-syntax-entry ?\. "." st)
    (modify-syntax-entry ?\/ "." st)
    (modify-syntax-entry ?\: "." st)
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\< "." st)
    (modify-syntax-entry ?\= "." st)
    (modify-syntax-entry ?\> "." st)
    (modify-syntax-entry ?\? "." st)
    (modify-syntax-entry ?\@ "." st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?^  "w" st)
    (modify-syntax-entry ?\_ "w" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\| "." st)
    (modify-syntax-entry ?\} "){" st)
    st)
  "Syntax table for `cobol-mode'.")

;; All keyword lists get sorted so new words can be anywhere within the
;; appropriate list.  The keywords are currently only used for highlighting but
;; more uses such as abbrev-mode are in progress.

(defvar cobol-keywords-directives
  '( "ANSI"                   "BLANK"                  "NOBLANK"
     "CALL-SHARED"            "CANCEL"                 "NOCANCEL"
     "CHECK"                  "CODE"                   "NOCODE"
     "COLUMNS"                "COMPACT"                "NOCOMPACT"
     "COMPILE"                "CONSULT"                "NOCONSULT"
     "CROSSREF"               "NOCROSSREF"             "DIAGNOSE-74"
     "NODIAGNOSE-74"          "DIAGNOSE-85"            "NODIAGNOSE-85"
     "DIAGNOSEALL"            "NODIAGNOSEALL"          "ENDIF"
     "ENDUNIT"                "ENV"                    "ERRORFILE"
     "ERRORS"                 "FIPS"                   "NOFIPS"
     "FMAP"                   "HEADING"                "HEAP"
     "HIGHPIN"                "HIGHREQUESTERS"         "ICODE"
     "NOICODE"                "IF"                     "IFNOT"
     "INNERLIST"              "NOINNERLIST"            "INSPECT"
     "NOINSPECT"              "LARGEDATA"              "LD"
     "LESS-CODE"              "LIBRARY"                "LINES"
     "LIST"                   "NOLIST"                 "LMAP"
     "NOLMAP"                 "MAIN"                   "MAP"
     "NOMAP"                  "NLD"                    "NONSTOP"
     "NON-SHARED"             "OPTIMIZE"               "PERFORM-TRACE"
     "PORT"                   "NOPORT"                 "RESETTOG"
     "RUNNABLE"               "RUNNAMED"               "SAVE"
     "SAVEABEND"              "NOSAVEABEND"            "SEARCH"
     "NOSEARCH"               "SECTION"                "SETTOG"
     "SHARED"                 "SHOWCOPY"               "NOSHOWCOPY"
     "SHOWFILE"               "NOSHOWFILE"             "SOURCE"
     "SQL"                    "NOSQL"                  "SQLMEM"
     "SUBSET"                 "SUBTYPE"                "SUPPRESS"
     "NOSUPPRESS"             "SYMBOLS"                "NOSYMBOLS"
     "SYNTAX"                 "TANDEM"                 "TRAP2"
     "NOTRAP2"                "TRAP2-74"               "NOTRAP2-74"
     "UL"                     "WARN"                   "NOWARN"
   )
  "List of COBOL compiler directives.
Used to create the `font-lock-keywords' table.")

(defvar cobol-keywords-statements
  '( "ACCEPT"                "ADD"                   "ADD TO"
     "ADD GIVING"            "ADD CORRESPONDING"     "ALTER"
     "CALL"                  "CANCEL"                "CHECKPOINT"
     "CLOSE"                 "COMPUTE"               "CONTINUE"
     "COPY"                  "DELETE"                "DISPLAY"
     "DIVIDE"                "DIVIDE INTO"           "DIVIDE GIVING"
     "DIVIDE GIVING REMAINDER"                       "ENTER"
     "ENTER COBOL"           "EVALUATE"              "EXIT"
     "GO TO"                 "IF"                    "INITIALIZE"
     "INSPECT"               "INSPECT TALLYING"      "INSPECT REPLACING"
     "INSPECT TALLYING REPLACING"                    "INSPECT CONVERTING"
     "LOCKFILE"              "MERGE"                 "MOVE"
     "MOVE TO"               "MOVE CORRESPONDING"    "MULTIPLY"
     "MULTIPLY BY"           "MULTIPLY GIVING"       "OPEN"
     "PERFORM"               "PERFORM TIMES"         "PERFORM UNTIL"
     "PERFORM VARYING"       "READ"                  "RELEASE"
     "REPLACE"               "RETURN"                "REWRITE"
     "SEARCH"                "SEARCH VARYING"        "SEARCH ALL"
     "SET"                   "SET TO"                "SET UP"
     "SET DOWN"              "SORT"                  "START"
     "STARTBACKUP"           "STOP"                  "STRING"
     "SUBTRACT"              "SUBTRACT FROM"         "SUBTRACT GIVING"
     "SUBTRACT CORRESPONDING"                        "UNLOCKFILE"
     "UNLOCKRECORD"          "UNSTRING"              "USE"
     "USE DEBUGGING"         "USE AFTER EXCEPTION"   "WRITE"
   )
  "List of COBOL statement keywords.
Used to create the `font-lock-keywords' table.")

(defvar cobol-keywords-deprecated
  '( )
  "List of COBOL keywords and Builtin functions now deprecated.
Used to create the `font-lock-keywords' table")

(defvar cobol-keywords-reserved
  '( "ACCEPT"                 "ACCESS"                 "ADD"
     "ADDRESS"                "ADVANCING"              "AFTER"
     "ALL"                    "ALPHABET"               "ALPHABETIC"
     "ALPHABETIC-LOWER"       "ALPHABETIC-UPPER"       "ALPHANUMERIC"
     "ALPHANUMERIC-EDITED"    "ALSO"                   "ALTER"
     "ALTERNATE"              "AND"                    "ANY"
     "APPROXIMATE"            "AREA"                   "AREAS"
     "ASCENDING"              "ASSIGN"                 "AT"
     "AUTHOR"                 "BEFORE"                 "BINARY"
     "BLANK"                  "BLOCK"                  "BOTTOM"
     "BY"                     "CALL"                   "CANCEL"
     "CD"                     "CF"                     "CH"
     "CHARACTER"              "CHARACTERS"             "CHARACTER-SET"
     "CHECKPOINT"             "CLASS"                  "CLOCK-UNITS"
     "CLOSE"                  "COBOL"                  "CODE"
     "CODE-SET"               "COLLATING"              "COLUMN"
     "COMMA"                  "COMMON"                 "COMMUNICATION"
     "COMP"                   "COMP-3"                 "COMP-5"
     "COMPUTATIONAL"          "COMPUTATIONAL-3"        "COMPUTATIONAL-5"
     "COMPUTE"                "CONFIGURATION"          "CONTAINS"
     "CONTENT"                "CONTINUE"               "CONTROL"
     "CONTROLS"               "CONVERTING"             "COPY"
     "CORR"                   "CORRESPONDING"          "COUNT"
     "CURRENCY"               "DATA"                   "DATE"
     "DATE-COMPILED"          "DATE-WRITTEN"           "DAY"
     "DAY-OF-WEEK"            "DE"                     "DEBUG-CONTENTS"
     "DEBUG-ITEM"             "DEBUG-LINE"             "DEBUG-SUB-2"
     "DEBUG-SUB-3"            "DEBUGGING"              "DECIMAL-POINT"
     "DECLARATIVES"           "DEBUG-NAME"             "DEBUG-SUB-1"
     "DELETE"                 "DELIMITED"              "DELIMITER"
     "DEPENDING"              "DESCENDING"             "DESTINATION"
     "DETAIL"                 "DISABLE"                "DISPLAY"
     "DIVIDE"                 "DIVISION"               "DOWN"
     "DUPLICATES"             "DYNAMIC"                "EGI"
     "ELSE"                   "EMI"                    "ENABLE"
     "END"                    "END-ADD"                "END-COMPUTE"
     "END-DELETE"             "END-DIVIDE"             "END-EVALUATE"
     "END-IF"                 "END-MULTIPLY"           "END-OF-PAGE"
     "END-PERFORM"            "END-READ"               "END-RECEIVE"
     "END-RETURN"             "END-REWRITE"            "END-SEARCH"
     "END-START"              "END-STRING"             "END-SUBTRACT"
     "END-UNSTRING"           "END-WRITE"              "ENTER"
     "EOP"                    "EQUAL"                  "ERROR"
     "ESI"                    "EVALUATE"               "EVERY"
     "EXCEPTION"              "EXCLUSIVE"              "EXIT"
     "EXTEND"                 "EXTENDED-STORAGE"       "EXTERNAL"
     "FALSE"                  "FD"                     "FILE"
     "FILE-CONTROL"           "FILLER"                 "FINAL"
     "FIRST"                  "FOOTING"                "FOR"
     "FROM"                   "FUNCTION"               "GENERATE"
     "GENERIC"                "GIVING"                 "GLOBAL"
     "GO"                     "GREATER"                "GROUP"
     "GUARDIAN-ERR"           "HEADING"                "HIGH-VALUE"
     "HIGH-VALUES"            "I-O"                    "I-O-CONTROL"
     "IDENTIFICATION"         "IF"                     "IN"
     "INDEX"                  "INDEXED"                "INDICATE"
     "INITIAL"                "INITIALIZE"             "INITIATE"
     "INPUT"                  "INPUT-OUTPUT"           "INSPECT"
     "INSTALLATION"           "INTO"                   "INVALID"
     "IS"                     "JUST"                   "JUSTIFIED"
     "KEY"                    "LABEL"                  "LAST"
     "LEADING"                "LEFT"                   "LENGTH"
     "LESS"                   "LIMIT"                  "LIMITS"
     "LINAGE"                 "LINAGE-COUNTER"         "LINE"
     "LINE-COUNTER"           "LINKAGE"                "LOCK"
     "LOCKFILE"               "LOW-VALUE"              "LOW-VALUES"
     "MEMORY"                 "MERGE"                  "MESSAGE"
     "MODE"                   "MODULES"                "MOVE"
     "MULTIPLE"               "MULTIPLY"               "NATIVE"
     "NEGATIVE"               "NEXT"                   "NO"
     "NOT"                    "NULL"                   "NULLS"
     "NUMBER"                 "NUMERIC"                "NUMERIC-EDITED"
     "OBJECT-COMPUTER"        "OCCURS"                 "OF"
     "OFF"                    "OMITTED"                "ON"
     "OPEN"                   "OPTIONAL"               "OR"
     "ORDER"                  "ORGANIZATION"           "OTHER"
     "OUTPUT"                 "OVERFLOW"               "PACKED-DECIMAL"
     "PADDING"                "PAGE"                   "PAGE-COUNTER"
     "PERFORM"                "PF"                     "PH"
     "PIC"                    "PICTURE"                "PLUS"
     "POINTER"                "POSITION"               "POSITIVE"
     "PRINTING"               "PROCEDURE"              "PROCEDURES"
     "PROCEED"                "PROGRAM"                "PROGRAM-ID"
     "PROGRAM-STATUS"         "PROGRAM-STATUS-1"       "PROGRAM-STATUS-2"
     "PROMPT"                 "PROTECTED"              "PURGE"
     "QUEUE"                  "QUOTE"                  "QUOTES"
     "RANDOM"                 "RD"                     "READ"
     "RECEIVE"                "RECEIVE-CONTROL"        "RECORD"
     "RECORDS"                "REDEFINES"              "REEL"
     "REFERENCE"              "REFERENCES"             "RELATIVE"
     "RELEASE"                "REMAINDER"              "REMOVAL"
     "RENAMES"                "REPLACE"                "REPLACING"
     "REPLY"                  "REPORT"                 "REPORTING"
     "REPORTS"                "RERUN"                  "RESERVE"
     "RESET"                  "RETURN"                 "REVERSED"
     "REWIND"                 "REWRITE"                "RF"
     "RH"                     "RIGHT"                  "ROUNDED"
     "RUN"                    "SAME"                   "SD"
     "SEARCH"                 "SECTION"                "SECURITY"
     "SEGMENT"                "SEGMENT-LIMIT"          "SELECT"
     "SEND"                   "SENTENCE"               "SEPARATE"
     "SEQUENCE"               "SEQUENTIAL"             "SET"
     "SHARED"                 "SIGN"                   "SIZE"
     "SORT"                   "SORT-MERGE"             "SOURCE"
     "SOURCE-COMPUTER"        "SPACE"                  "SPACES"
     "SPECIAL-NAMES"          "STANDARD"               "STANDARD-1"
     "STANDARD-2"             "START"                  "STARTBACKUP"
     "STATUS"                 "STOP"                   "STRING"
     "SUB-QUEUE-1"            "SUB-QUEUE-2"            "SUB-QUEUE-3"
     "SUBTRACT"               "SUM"                    "SUPPRESS"
     "SYMBOLIC"               "SYNC"                   "SYNCDEPTH"
     "SYNCHRONIZED"           "TABLE"                  "TAL"
     "TALLYING"               "TAPE"                   "TERMINAL"
     "TERMINATE"              "TEST"                   "TEXT"
     "THAN"                   "THEN"                   "THROUGH"
     "THRU"                   "TIME"                   "TIMES"
     "TO"                     "TOP"                    "TRAILING"
     "TRUE"                   "TYPE"                   "UNIT"
     "UNLOCK"                 "UNLOCKFILE"             "UNLOCKRECORD"
     "UNSTRING"               "UNTIL"                  "UP"
     "UPON"                   "USAGE"                  "USE"
     "USING"                  "VALUE"                  "VALUES"
     "VARYING"                "WHEN"                   "WITH"
     "WORDS"                  "WORKING-STORAGE"        "WRITE"
     "ZERO"                   "ZEROES"
     )
  "List of COBOL keywords reserved only in certain language contexts.
Used to create the `font-lock-keywords' table.")

(defvar cobol-keywords-std-fcns
  '( "ACOS"                   "ANNUITY"                "ASIN"
     "ATAN"                   "CHAR"                   "COS"
     "CURRENT-DATE"           "DATE-OF-INTEGER"        "DAY-OF-INTEGER"
     "FACTORIAL"              "INTEGER"                "INTEGER-OF-DATE"
     "INTEGER-OF-DAY"         "INTEGER-PART"           "LENGTH"
     "LOG"                    "LOG10"                  "LOWER-CASE"
     "MAX"                    "MEAN"                   "MEDIAN"
     "MIDRANGE"               "MIN"                    "MOD"
     "NUMVAL"                 "NUMVAL-C"               "ORD"
     "ORD-MAX"                "ORD-MIN"                "PRESENT-VALUE"
     "RANDOM"                 "RANGE"                  "REM"
     "REVERSE"                "SIN"                    "SQRT"
     "STANDARD-DEVIATION"     "SUM"                    "TAN"
     "UPPER-CASE"             "VARIANCE"               "WHEN-COMPILED"
   )
  "List of COBOL standard functions.
Used to create the `font-lock-keywords' table.")

(defvar cobol-keywords-privileged
  '( )
  "List of COBOL privileged functions.
Used to create the `font-lock-keywords' table.")

(defvar cobol-keywords-builtin
  '( "#IN"                             "#OUT"
     "#TERM"                           "#TEMP"
     "#DYNAMIC"                        "COBOL85^ARMTRAP"
     "COBOL85^COMPLETION"              "COBOL_COMPLETION_"
     "COBOL_CONTROL_"                  "COBOL_GETENV_"
     "COBOL_PUTENV_"                   "COBOL85^RETURN^SORT^ERRORS"
     "COBOL_RETURN_SORT_ERRORS_"       "COBOL85^REWIND^SEQUENTIAL"
     "COBOL_REWIND_SEQUENTIAL_"        "COBOL85^SET^SORT^PARAM^TEXT"
     "COBOL_SET_SORT_PARAM_TEXT_"      "COBOL85^SET^SORT^PARAM^VALUE"
     "COBOL_SET_SORT_PARAM_VALUE_"     "COBOL_SET_MAX_RECORD_"
     "COBOL_SETMODE_"                  "COBOL85^SPECIAL^OPEN"
     "COBOL_SPECIAL_OPEN_"             "COBOLASSIGN"
     "COBOL_ASSIGN_"                   "COBOLFILEINFO"
     "COBOL_FILE_INFO_"                "COBOLSPOOLOPEN"
     "CREATEPROCESS"                   "ALTERPARAMTEXT"
     "CHECKLOGICALNAME"                "CHECKMESSAGE"
     "DELETEASSIGN"                    "DELETEPARAM"
     "DELETESTARTUP"                   "GETASSIGNTEXT"
     "GETASSIGNVALUE"                  "GETBACKUPCPU"
     "GETPARAMTEXT"                    "GETSTARTUPTEXT"
     "PUTASSIGNTEXT"                   "PUTASSIGNVALUE"
     "PUTPARAMTEXT"                    "PUTSTARTUPTEXT"
     )
  "List of COBOL privileged builtin functions.
Used to create the `font-lock-keywords' table.")

(defvar cobol-keyword-fcn-names-regexp
  "^.\\{6\\}\\s-\\{1,4\\}\\(\\w+\\)\\s-*\\."
  "Defines a regexp that finds the names of paragraphs.
Used to create the `font-lock-keywords' table.")
(defvar cobol-keyword-section-names-regexp
  "^.\\{6\\}\\s-\\{1,4\\}\\(\\w+\\s-+\\(division\\|section\\)\\)\\."
  "Defines a regexp that finds the names of paragraphs.
Used to create the `font-lock-keywords' table.")

;;; Paren matching

(defcustom cobol-begin-matches-semi t
  "If not nil, the b of begin matches the semicolon in end;
Otherwise it matches the d of end.  It always matches the d when no ;"
  :type 'boolean
  :group 'cobol)

;;; Font lock (highlighting)

(defcustom cobol-font-lock-always t
  "`cobol-mode' makes sure `font-lock-mode' is on for cobol-mode buffers.
Some things don't work if it's off so insuring it's on is the default."
  :type 'boolean
  :group 'cobol)

(defcustom cobol-primecode-warning t
  "Highlight instances of ]a ]d and ]e in column 1 with a warning face.
This alerts you that submission of this file to RMS/PrimeCode will fail
due to invalid contents.  nil disables this warning."
  :type 'boolean
  :group 'cobol)

(defun cobol-keyword-anywhere-regexp ( word-list )
  "Returns a regexp that finds any of the words in WORD-LIST.
But only if the keyword is surrounded by non-word chars."
  (concat "\\<"(regexp-opt word-list t)"\\W"))

;; The next 4 def's work tightly together and, as coded, cannot be reused for
;; additional purposes.
(defvar cobol-keyword-on-directive-line-regexp () "Internal use only.")
(defun  cobol-keyword-on-directive-line-regexp ( word-list )
"Returns a function to find WORD-LIST only if line starts with ?"
  (setq cobol-keyword-on-directive-line-regexp
        (concat "\\b"(regexp-opt word-list t)"\\b"))
  'cobol-font-lock-directive-line)
(defvar cobol-amid-font-lock-excursion nil
;; Used by `cobol-font-lock-directive-line'.  When a line starting with ? in
;; column 1 is detected this variable holds the context needed to continue
;; searching for more keywords.  If nil a line starting with ? should be
;; searched for.
)
(make-variable-buffer-local 'cobol-amid-font-lock-excursion)
(defun cobol-font-lock-directive-line ( search-limit )
;; This function finds keywords only in lines starting with ?.  Valid keywords
;; are described by `cobol-keyword-on-directive-line-regexp'.  First a line
;; beginning with ? is searched for.  Once found, point is moved to the
;; beginning of that area and limit is set to the end.  Keywords are searched
;; for within that range.  If found, context is saved in
;; cobol-amid-font-lock-excursion and the match-data is returned.  If not found,
;; another line starting with ?  is searched for.  If saved context exists when
;; this function is called then another keyword is searched for in the
;; previously narrowed region.  If none is found the next region is searched
;; for.
  (let ((looking t))
    (while
        (and looking
             (or cobol-amid-font-lock-excursion
          (when (re-search-forward "^\\?.+\n" search-limit t)
            (setq cobol-amid-font-lock-excursion (point))
            (goto-char (match-beginning 0)))))
      (if (re-search-forward cobol-keyword-on-directive-line-regexp
               cobol-amid-font-lock-excursion t)
          (setq looking nil)
        (goto-char cobol-amid-font-lock-excursion)
        (setq cobol-amid-font-lock-excursion nil)))
    (not looking)))

;; Defines comments and strings which a syntax-table can't do in COBOL.
(defcustom cobol-comment-sequence-regexp nil
  "regexp matching lines whose sequence/labels to mark as comments.
An empty string marks all, nil marks none."
  :type  `(choice :tag "Sequence handling"
                  (regexp :tag "Regexp")
                  (const  :tag "All" "^")
                  (const  :tag "None" nil))
  :group 'cobol)

(defvar cobol-find-syntactic--state ()
  "Used by `cobol-find-syntactic-keywords' to find multiple syntactic
elements which all must be anchored to the beginning of a line.
nil    = no search done yet on this line.
0      = check for compiler directive line
1      = sequence/label area checked. look at body.
2      = body not a comment, any trailing comment marked, check for strings
marker = terminated string found check for more.")
(make-variable-buffer-local 'cobol-find-syntactic--state)
(defun cobol-find-syntactic-keywords ( search-limit )
  "Used by `font-lock-syntactic-keywords' to find comments and strings.
Returns t if either a comment or string is found, nil if neither is found.
match-data 1&2 are set for comments, 3&4 are set for a normal string, 5&6 are
set for eol-terminated strings.  Where the match pair mark the start character
and end character respectively.  Point is moved to the next line during this
function only after the last search completes for the current line.  A state
machine, controlled by `cobol-find-syntactic--state' sequences the searches."
  (let ((found nil)
        (save (point)))
    (while (and (< (point) search-limit)
                (not found))
           (cond
            ;; no comments or quotes? in compiler directives
            ((or (null cobol-find-syntactic--state)
                 (equal cobol-find-syntactic--state (make-marker)))
             (if (looking-at "^\\(\\?\\|......\\?\\)")
                 (forward-line 1)       ;do this state on next line
               (setq cobol-find-syntactic--state 0) ;do next state on this line
             )
            )
            ;; check for comment within sequence number area
            ((= 0 cobol-find-syntactic--state)
             (when (and cobol-comment-sequence-regexp
                        (looking-at "^.\\(?:.\\|\n\\)")
                        (looking-at cobol-comment-sequence-regexp)
                        (looking-at ".\\{1,6\\}"))
               (setq found t)
               (let* ((start (car (match-data)))
                      (mid1  (copy-marker (1+ start)))
                      (end   (cadr (match-data)))
                      mid2)
                 (if (/= end mid1)
                     (setq mid2 (copy-marker (1- end)))
                   (setq mid2 end
                         end (copy-marker (1+ end))))
                 (set-match-data
                  (list start end     ;match-string 0
                        start mid1    ;match-string 1 - comment start
                        mid2 end))))  ;match-string 2 - comment end
             (setq cobol-find-syntactic--state 1) ;next state, found or not
            )
            ;; see if entire line is a comment
            ((= 1 cobol-find-syntactic--state)
             (when (looking-at "^......\\(?:*\\|/\\)")
;                       (and (looking-at
;                             "^......\\(?:     \\{1,61\\}[^ \n]\\|-\\)")
;                            (save-excursion
;                              (if (and (= 0 (forward-line -1))
;                                       (looking-at "......."))
;                                  (equal (get-char-property
;                                          (+ 6 (point)) 'syntax-table)
;                                         '(11))))))
               (looking-at "......\\(.\\).*\\(\n\\|'\\)") ;setup match-data
               (forward-line 1)    ;next iteration looks at next line
               (setq cobol-find-syntactic--state ()
                     found t))
             ;; see if there is an implied trailing comment
             (when cobol-find-syntactic--state ;line isn't a comment
               (setq cobol-find-syntactic--state 2 ;next state, found or not
                     found (looking-at "^.\\{72\\}\\(.\\).*\\(\n\\)"))))
            ;; look for strings only within columns 8-72 inclusive
            ((= 2 cobol-find-syntactic--state)
             (if (looking-at "^......[-d D][^\"\n]\\{0,64\\}\"")
                 (let* ((open-quote (list (copy-marker (1- (match-end 0)))
                                          (copy-marker (match-end 0))))
                        (leol (copy-marker (min (+ 72 (point))
                                                (line-end-position))))
                        close-quote)
                   (setq found t)
                   (goto-char (cadr open-quote))
                   (if (search-forward "\"" leol t)
                       (progn           ; normally ending string
                         (setq close-quote (match-data)
                               cobol-find-syntactic--state (cadr close-quote))
                         (beginning-of-line)
                         (set-match-data
                          `(,(car open-quote) ,(cadr close-quote)
                            nil nil nil nil ;match-string 1&2 not found
                            ,@open-quote ,@close-quote)) ;3&4 are normal string
                       )
                     ;; implicit string end
                     (forward-line 1)   ;next iteration looks at next line
                     (setq close-quote (list (copy-marker (1- leol)) leol)
                           cobol-find-syntactic--state ())
                     (set-match-data
                      `(,(car open-quote) ,(cadr close-quote)
                        nil nil nil nil ;match-string 1&2 not found
                        nil nil nil nil ;match-string 3&4 not found
                        ,@open-quote ,@close-quote)) ;5&6 unterminated string
                   ))
               ;; no string was found.  Start new analysis on next line
               (forward-line 1)
               (setq cobol-find-syntactic--state ())))
            ;; a string has been found look for another after it
            ((markerp cobol-find-syntactic--state)
             (let ((leol (copy-marker (min (+ 72 (point))
                                           (line-end-position))))
                   open-quote close-quote)
               (goto-char cobol-find-syntactic--state)
               (if (search-forward "\"" leol t)
                   (progn
                     (setq open-quote (match-data)
                           found t)
                     (if (search-forward "\"" leol t)
                         (progn         ; normally ending string
                           (beginning-of-line) ;next iteration starts here again
                           (setq close-quote (match-data)
                                 cobol-find-syntactic--state (cadr close-quote))
                           (set-match-data
                            `(,(car open-quote) ,(cadr close-quote)
                              nil nil nil nil ;match-string 1&2 not found
                              ,@open-quote ,@close-quote)) ;3&4 normal string
                         )
                       ;; implicit string end
                       (forward-line 1) ;next iteration looks at next line
                       (setq close-quote (list (copy-marker (1- leol)) leol)
                             cobol-find-syntactic--state ())
                       (set-match-data
                        `(,(car open-quote) ,(cadr close-quote)
                          nil nil nil nil ;match-string 1&2 not found
                          nil nil nil nil ;match-string 3&4 not found
                          ,@open-quote ,@close-quote)) ;5&6 unterminated string
                     ))
                 (forward-line 1)
                 (setq cobol-find-syntactic--state ()))
             ))))
    ;; Point should not return forward of search-limit
    (and (> (point) search-limit) (goto-char search-limit))
    ;; point shouldn't move if nothing was found.
    (prog1 found (or found (goto-char save)))))

(defvar cobol-static-font-lock-keywords
  ;; font-lock-keywords is a symbol or list of symbols yielding the keywords to
  ;; be fontified.  Keywords are listed here using either (MATCHER . FACENAME)
  ;; or (MATCHER . (MATCH FACENAME)) syntax.  Other options are available but
  ;; not used here.  For simplicity, all regexp's were designed so MATCH would
  ;; be 1.  Nothing forced this but to me it makes debug/maintenance easier.
  `(("^[^\n?].\\{5\\}\\([^ ?Dd*/-]\\)" 1 font-lock-warning-face)
    ("^[^\n?].\\{5\\}\\([?Dd-]\\)"     1 font-lock-builtin-face)
    (,(cobol-keyword-on-directive-line-regexp cobol-keywords-directives)
     1 font-lock-builtin-face)
    (,(cobol-keyword-anywhere-regexp cobol-keywords-builtin)
     1 font-lock-builtin-face)
    (,(cobol-keyword-anywhere-regexp (append cobol-keywords-std-fcns
                                           cobol-keywords-statements))
     1 font-lock-keyword-face)
    (,(cobol-keyword-anywhere-regexp (append cobol-keywords-deprecated
                                           cobol-keywords-privileged))
     1 font-lock-warning-face)
    (,cobol-keyword-section-names-regexp 1 font-lock-type-face)
    (,cobol-keyword-fcn-names-regexp 1 font-lock-function-name-face)))

(defvar cobol-font-lock-keywords ())

(defun cobol-build-font-lock-keywords ()
  "Creates `font-lock-keywords' based on current customize settings."
  (append cobol-static-font-lock-keywords
          `(,(when cobol-primecode-warning
               ;; ]a  ]d or ]e cannot appear in col 1-2 if using PrimeCode.
               '("^\\][ade]" . font-lock-warning-face)))))

(defvar cobol-font-lock-syntactic-keywords
 `(
   ;; cobol-find-syntactic-keywords returns matches 1&2 for comments, 3&4
   ;; for strings.  5&6 for eol terminated strings.  I must use "|"(15)
   ;; rather than "\""(7) for eol terminated strings because the begin
   ;; and end characters must be the same when "\""(7) is used.
   (cobol-find-syntactic-keywords (1 "<" t t)  (2 ">" t t)
                                  (3 "\"" t t) (4 "\"" t t)
                                  (5 "|" t t)  (6 "|" t t))
  )
 "A list of regexp's or functions.  Used to add syntax-table properties to
characters that can't be set by the syntax-table alone.")

(defun cobol-setup-font-lock ()
  "Sets up the buffer local value for font-lock-defaults and optionally
turns on font-lock-mode"
  ;; I use font-lock-syntactic-keywords to set some properties and I
  ;; don't want them ignored.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;; I really can't imagine anyone wanting this off.
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; This allows column markers to be different in separate buffers.
  (set (make-local-variable 'cobol-font-lock-keywords)
       (cobol-build-font-lock-keywords))
  ;; make sure the parsing state is reset
  (setq cobol-find-syntactic--state ())
  ;; This is where all the font-lock stuff actually gets set up.  Once
  ;; font-lock-defaults has it's value, setting font-lock-mode true should
  ;; cause all your syntax highlighting dreams to come true.
  (setq font-lock-defaults
         ;; The first value is all the keyword expressions.
       '(cobol-font-lock-keywords
         ;; keywords-only means no strings or comments get fontified
         nil
         ;; case-fold (ignore case)
         t
         ;; syntax-alist.  Nothing I can think of...
         nil
         ;; syntax-begin - no function defined to move outside syntactic block
         nil
         ;; font-lock-syntactic-keywords
         ;; takes (matcher (match syntax override lexmatch) ...)...
         (font-lock-syntactic-keywords . cobol-font-lock-syntactic-keywords )))
  ; font lock is turned on by default in this mode. Use customize to disable.
  (when cobol-font-lock-always (font-lock-mode t)))

;;; Static Column Markers

(defcustom cobol-column-marker-1 7
  "*Turns on column-marker-1 (which see) at the specified column.
Use of this feature requires the column-marker.el package be loaded or on
the search list."
  :type 'integer
  :group 'cobol)
(make-variable-buffer-local 'cobol-column-marker-1)

(defcustom cobol-column-marker-2 73
  "*Turns on column-marker-2 (which see) at the specified column.
Use of this feature requires the column-marker.el package."
  :type 'integer
  :group 'cobol)
(make-variable-buffer-local 'cobol-column-marker-2)

(defun cobol-setup-column-markers ()
  "Turns on column markers if configured and available.
See `cobol-column-marker-1' and `cobol-column-marker-2' "
  (if (condition-case ()
          (progn (require 'column-marker) nil)
        (error t))
      (if (not (and (zerop cobol-column-marker-1)
                    (zerop cobol-column-marker-2)))
          (message "column markers are configured but %s"
                   " column-marker feature not available."))
    (setq indent-tabs-mode nil)      ;documented as buffer local
    (column-marker-1 cobol-column-marker-1)
    (column-marker-2 cobol-column-marker-2)))

;;; Imenu & Which-function

(defcustom cobol-imenu-menubar t
  "If not nil, `imenu-add-to-menubar' is called during mode initialization.
This adds a [Menu name] menu to your menu bar.  By default the menu contains a
list of all procedures, sections and pages in your program.  You can go
directly to any item on the menu by selecting it.  You can control what
appears on this menu by modifying `cobol-imenu-expression-alist'.  You must turn
imenu on for this to work.  See `imenu' in the Emacs reference manual for more
information.  Personally I recommend customizing `imenu-sort-function' to sort
by name."
  :type  '(choice :tag "Menu Name"
                  (string :tag "Menu Name")
                  (const "Index")
                  (const :tag "None" nil))
  :group 'cobol)

(defvar cobol-imenu-syntax-alist ()
  "Overrides to `cobol-mode-syntax-table' used during `imenu-generic-expression' search."
  ;;AFAIK there are no character adjustments needed during imenu search.
)

(defcustom cobol-imenu-expression-alist
  `((nil         ,cobol-keyword-fcn-names-regexp     1)
    (nil         ,cobol-keyword-section-names-regexp 1)
    ("?Sections" "^\\?section\\s-+\\(\\w+\\)\\b"     1)
    ("?Pages"    "^\\?page\\s-+\"\\s-*\\(.+?\\)\""   1)
  )
  "A list of regular expressions for creating an `imenu' index.

Each element has the form (list-name regexp num).

Where list-name is the name of the submenu under which items matching regexp
are found and num is the expression index defining the label to use for the
submenu entry.  When num = 0 the entire matching regexp text appears under
list-name.  When list-name is nil the matching entries appear in the root
imenu list rather than in a submenu.  See also `cobol-imenu-menubar'"
  :type '(repeat (list (choice :tag "Submenu Name" string (const nil))
                       regexp (integer :tag "Regexp index")))
  :group 'cobol)

(defcustom cobol-display-which-function t
  "This option turns `which-func' on for all `cobol-mode' buffers.
`which-func' is a package that causes the current function, section or
page to be displayed on the mode line.  `which-func' uses `imenu'.  Also
see `cobol-imenu-expression-alist' for more information."
  :type 'boolean
  :group 'cobol)

(defun cobol-setup-imenu ()
  "Installs cobol-imenu-generic-expression & cobol-imenu-syntax-alist."
  ;; imenu doc says these 3 are buffer-local by default
  (setq imenu-generic-expression cobol-imenu-expression-alist)
  (setq imenu-syntax-alist cobol-imenu-syntax-alist)
  (setq imenu-case-fold-search t) ;COBOL are never case sensitive
  (when cobol-imenu-menubar
    (if (condition-case ()
            (progn (require 'imenu) t)
          (error nil))
        (imenu-add-menubar-index)
      (message "cobol-imenu-menubar is set but imenu feature not available.")))
  (when cobol-display-which-function
    (if (condition-case ()
            (progn (require 'which-func) t)
          (error nil))
        (which-function-mode t)
      (message "cobol-display-which-function set but which-func not available"))))

;;; Adaptive-fill / auto-fill (needs much work but it's a start)

(defcustom cobol-restrict-auto-fill t
  "When not nil a buffer local value for `fill-nobreak-predicate' is created
to prevent code from being accidentally realligned.  The function uses syntax
highlighting to detect comments so `font-lock-mode' must be enabled to work."
  :type 'boolean
  :group 'cobol)

(defun cobol-setup-adaptive-fill ()
  "Sets up the COBOL-MODE adaptive-fill variables."
  (set (make-local-variable 'fill-individual-varying-indent)
       nil)
  (set (make-local-variable 'auto-fill-inhibit-regexp)
       "\\s-*[^*/]")
  (set (make-local-variable 'comment-use-syntax)
       t)
  (set (make-local-variable 'comment-start)
       "*")
  (set (make-local-variable 'comment-end)
       "")
  (set (make-local-variable 'comment-padding)
       " ")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\s<\\|*\\)\\s-*")
  (set (make-local-variable 'sentence-end)
       "\\(;\\|\\.[ \t\n\f]\\)")
  (set (make-local-variable 'paragraph-start)
       "^\\([\n\f]\\|\\s-*begin\\b\\)")
  (set (make-local-variable 'paragraph-separate)
       "\\(^\n\\|\\s-end\\([;\n]\\|\\s-\\)\\)")
  (set (make-local-variable 'adaptive-fill-regexp)
       "^\\s-*\\(!\\|--\\)[~%^&()_#[*|;:-=+]*\\s-*")
  (set (make-local-variable 'adaptive-fill-first-line-regexp)
       adaptive-fill-regexp)
  (when cobol-restrict-auto-fill
    ; This is supposed to restrict auto-fill to comments only
    (fset (make-local-variable 'fill-nobreak-predicate)
          (lambda ()
            (not (eq (get-text-property (point) 'face)
                     'font-lock-comment-face))))))

;;; Indentation

(defun cobol-setup-indent ()
  "Sets default indentation or sets up cobol-indent if available."
  (if (condition-case ()
          (progn (require 'cobol-indent) t)
        (error nil))
      (set (make-local-variable 'indent-line-function) 'cobol-indent-line)
    (set (make-local-variable 'indent-line-function) 'indent-relative-maybe)))

;;; Language Skeletons -- Feel free to add more of your own!

(defcustom cobol-keywords-case 'upper
  "*Indicates if keywords in skeletons should be all UPPER CASE, all lower
case or Camel Case (First Char Upper & Rest Lower)."
  :type  '(choice (const :tag "ALL CAPS"   'upper)
                  (const :tag "all small"  'lower)
                  (const :tag "Camel Case" 'camel)
                  (const :tag "DON'T Change"  ()))
  :group 'cobol)

(defun cobol-setup-skel ()
  "Configures skeleton.el functions for the COBOL environemnt."
  (set (make-local-variable 'skeleton-transformation) 'cobol-skel-transform)
  ;; This prevents abbrevs from expanding within skeletons
  (setq skeleton-further-elements '((abbrev-mode nil))))

(defun cobol-skel-transform ( element )
  "Called by `skeleton-insert'.  Gives ELEMENT `cobol-keywords-case' capitalization."
  ;; This should be made more complex to only change the case of certain words
  ;; so the user can create skeletons containing items that should not be
  ;; affected by cobol-keywords-case.  There are 3 obvious ways.  1) use the
  ;; keywords tables above. 2) add a customize to ignore words. 3) add a
  ;; customize to specify specific words to be affected.  Preferences?
  (if (stringp element)
    (cond
     ((eq cobol-keywords-case 'upper) (upcase element))
     ((eq cobol-keywords-case 'lower) (downcase element))
     ((eq cobol-keywords-case 'camel) (capitalize element))
     ( t                             element            ))
    element))

(defun cobol-set-line-syntax ()
  "Applies font-lock-syntactic-keywords to current line.
Used to set properties necessary for proper indentation."
  (if font-lock-mode
      (save-excursion    ; next stmt moves point.
        (font-lock-fontify-syntactic-keywords-region
         (line-beginning-position) (line-end-position))
        ()     ;any result is inserted into buffer
      )))

(define-skeleton cobol-if-skel
  "This is an example skeleton."
  nil                                                >
  "IF (" _ ") THEN"                                  > \n
  "BEGIN"               (cobol-set-line-syntax)      > \n
  _                                                & > \n
  "END\;"               (cobol-set-line-syntax)      > \n)


;;; Abbrev support

(defcustom cobol-abbrev-mode t
  "Sets the default value for `abbrev-mode' upon entry into `cobol-mode'."
  :type 'boolean
  :group 'cobol)

(defvar cobol-mode-abbrev-table-list
  '(("$i" "" cobol-if-skel))
  "List of pre-defined `cobol-mode' abbrev definitions.
Use \\[list-abbrevs] to see all defined abbrevs.")

(defvar cobol-mode-abbrev-table)

(defun cobol-setup-abbrevs ()
  "Installs the `cobol-mode-abbrev-table' as `local-abbrev-table'"
  (define-abbrev-table 'cobol-mode-abbrev-table cobol-mode-abbrev-table-list)
  (setq local-abbrev-table cobol-mode-abbrev-table)
  (setq skeleton-further-elements '((abbrev-mode nil)))
  (abbrev-mode cobol-abbrev-mode)    ;Setting is documented as buffer local
)

;;; Movement by ...

;(defvar cobol-outline-regexp
;...)

;;; Miscellaneous


;;;###autoload
(defun cobol-mode ()
  "A major mode for editing COBOL language program source files.
Customization options are available via
\\[customize-group] <ret> COBOL <ret>

This mode provides COBOL specific support for such packages as:
    `font-lock-mode'      `show-paren-mode'     `imenu'               
    `which-function'      `skeleton-insert'     `auto-fill-mode'      
    `adaptive-fill-mode'  `filladapt-mode'      `abbrev-mode'         

** Note ** Many things won't work correctly if `font-lock-mode' is off.

cobol-mode also implements the following \\[execute-extended-command] ... commands

`cobol-mode'             Activates this mode for the current buffer
`cobol-begin-end-skel'   Inserts a Begin/End skeleton
`cobol-case-skel'        Inserts a labeled case statement skeleton
`cobol-if-skel'          Inserts an if/then statement skeleton
`cobol-if-else-skel'     Inserts an if/then/else statement skeleton
`cobol-proc-skel'        Example of a skeleton procedure

\\{cobol-mode-map}
Use \\[describe-bindings] to see ALL key bindings.

Some settings I like:
Turn on `skeleton-pair-insert-maybe' for (), [] and \"\"
Turn on `imenu' and set `imenu-sort-function' to imenu--sort-by-name
Turn on `recentf-mode'. You might need `recentf-auto-cleanup' = 'never
Set `column-marker-1' to 79 so you can tell what TEDIT users can't see.
Load `popup-ruler' for a TEDIT F9 type ruler on steroids.
I find `transient-mark-mode' totally indespensible.
CUA mode has some really great rectangle functions."
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'major-mode) 'cobol-mode)
  (set (make-local-variable 'mode-name) "COBOL")
  (set (make-local-variable 'make-backup-files) nil) ;necessary for now
  (use-local-map cobol-mode-map)
  (set-syntax-table cobol-mode-syntax-table)
  (cobol-setup-font-lock)
  (cobol-setup-adaptive-fill)
  (cobol-setup-abbrevs)
  (cobol-setup-imenu)
  (cobol-setup-indent)
  (cobol-setup-skel)
  (cobol-setup-column-markers)
;  (set (make-local-variable 'outline-regexp) cobol-outline-regexp)
  (show-paren-mode 1)
  (run-hooks 'cobol-mode-hook))

(provide 'cobol-mode)

;;; cobol-mode.el ends here

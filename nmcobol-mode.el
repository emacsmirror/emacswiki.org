;;; nmcobol-mode.el --- For use with Tandem Cobol only - Sorry.

;; Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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

;; NMCOBOL -- COmmon Business Oriented Language - in ?TANDEM line format.

;; This is currently a work-in-progress.  You are welcome to try it and
;; submit changes or report bugs but there is no point trying to report
;; lack of features unless you are including patches that address such
;; shortcomings.
;;
;; Since Tandem's line format is not standard this mode is not likely to
;; suit the needs of most Cobol progammers.  Moreover, Tandem Cobol has
;; extensions that standard Cobol doesn't have and may not implement
;; features required or generally available by other Cobol compilers.
;; I'm happy to add submitted patches to support other compilers only if
;; such patches don't interfere with support of Tandem Cobol.

;;; Installing:

;; Before you can use nmcobol-mode, emacs needs to be able to find it.  Place
;; the nmcobol-mode.el file in a directory on the load-path; typically the
;; .../site-lisp or perhaps .../lisp/progmods directory.  Usually you would
;; also want to byte compile nmcobol-mode.el but this is not required.  To do
;; this, visit the nmcobol-mode.el file, type: M-x emacs-lisp-byte-compile <ret>
;; There should be no warnings or errors during byte compilation.
;;
;; There are 4 basic ways to use NMCOBOL-MODE on a file.  The first method
;; manually selects nmcobol-mode as the editing mode.  The other 3 cause emacs
;; to recognize automatically that you want to visit the file using
;; nmcobol-mode.
;;
;; Pick one:
;; 1. While visiting a file, type: M-x nmcobol-mode <ret>
;; 2. Put the string -*-nmcobol-*- in a comment on the first line of the file.
;;    Save the file and close it.  Now any time you open it nmcobol-mode starts.
;; 3. Create an association between a particular file naming convention and
;;    nmcobol-mode.  This is done by adding an association to auto-mode-alist.
;; For example:
;; (setq auto-mode-alist
;;   (append
;;     '(("\\.cob\\'" . nmcobol-mode)      ;extension of .cob means nmcobol-mode
;;       ("\\([\\/]\\|^\\)[^.]+$" . nmcobol-mode)) ;so does no extension at all.
;;    auto-mode-alist))
;; 4. Advise set-auto-mode to look at the buffer contents upon loading.
;;    For an example see: http://www.emacswiki.org/cgi-bin/wiki/TandemNskSupport
;;
;; The above all tell emacs that you want to use nmcobol-mode but you must load
;; nmcobol-mode before you can use it.  There are 2 methods of telling emacs to
;; load the nmcobol-mode routines.  The first unconditionally loads nmcobol-mode
;; definitions immediately.  The second tells emacs to automatically load
;; nmcobol-mode only when you try to use it.  Add one of the following lines to
;; your .emacs file.
;;
;;(require 'nmcobol-mode)      ; Unconditional load
;;(autoload 'nmcobol-mode "nmcobol-mode" "Major mode for Tandem NMCOBOL files." t nil)
;;
;;; Getting eldoc to work in nmcobol-mode:

;; Open a file containing variable declarations for which you want
;; help permanently loaded.  For example: $SYSTEM.COPYLIBS.COBOLLIB.
;; The buffer must be in nmcobol-mode or ddl-mode.
;; Use:  M-x nmcobol-mode <ret> if necessary.
;; Then: M-x nmcobol-eldoc-make-list <ret>
;; You should now be in a specially formatted buffer containing a list of
;; the variables declared and their corresponding help strings.  You can
;; modify the strings themselves if desired but do not alter anything
;; else.  Repeat these steps to add more help entries to the file.
;; Save the file somewhere on your search list like your site-lisp
;; directory. Example:
;;     C-x C-w ~/../site-lisp/extdecs-help.el <ret>
;; Now add the file you just saved to the list of nmcobol-mode eldoc help
;; files to be loaded.
;;     M-x customize-option <ret> nmcobol-eldoc-def-files <ret>
;; Save the customization for future sessions.
;; Once you have your help entries defined see help for eldoc-mode for
;; turing on the mode.  Putting (eldoc-mode t) in .emacs might do it.

;;; History:

;; 2006-11-07 RGB Started writing this mode using my cobol-mode as skeleton.
;; 2006-12-18 RGB Added eldoc support.
;; 2006-12-26 RGB Added nmcobol-addup-pics.
;; 2006-12-27 RGB Added thing-at-point support for Guardian style 'filename.
;; 2006-12-29 RGB Added an NMCobol menu and nmcobol-customize-options.
;; 2007-01-03 RGB Added movement by sentences.
;; 2007-01-04 RGB Fixed movement by sentences, added paragraphs.
;; 2007-01-04 RGB Make COUNT argument in movement by functions optional.
;; 2007-01-09 RGB Added eldoc for variables including a buffer-local obarray.
;; 2007-01-10 RGB Added Cobol specific support for anchored-transpose.
;; 2007-01-11 RGB Fixed 66 level handling in paragraph movement and eldoc-scan.
;; 2007-01-28 RGB Sentence/Paragraph movement now saves match data.
;; 2007-01-31 RGB Started adding automatic indentation.
;; 2007-02-01 RGB Made * electric.
;; 2007-02-05 RGB Auto-indent now operates on regions
;; 2007-02-07 RGB Fixed customization of nmcobol-keywords-case &
;;                nmcobol-imenu-menubar. Fixed nmcobol-pic-string-regexp
;;                not recognizing 'ZZZ-.' syntax.  Fixed
;;                nmcobol-char-maybe-comment which broke numeric
;;                prefix behavior.  Doc-string updates.
;; 2007-02-12 RGB Indentation now handles all the block statements properly.
;; 2007-02-13 RGB The beginning of secondary indentation started.  This
;;                handles alignment of things like picture clauses, TO
;;                statements etc.
;; 2007-02-19 RGB imenu now ignores xxxx-exit paragraphs.  Started trying
;;                to implement paren matching on end-<verb> keywords.
;; 2007-02-22 RGB nmcobol-get-block-type now ignores keywords in strings.
;;; Code:

(defgroup nmcobol nil
  "Major mode for editing NMCOBOL source files in Emacs.
While in nmcobol-mode use C-h m for a description of the mode's features."
  :prefix 'nmcobol-
  :group 'languages)

;;; SYNTAX TABLE

(defvar nmcobol-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?\! "." st)
    (modify-syntax-entry ?\" "." st)  ; wiki ?" bug workaround comment
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
  "Syntax table for `nmcobol-mode'.")

;;; KEY MAP

(defvar nmcobol-skeleton-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?i]   'nmcobol-if-skel)
    (define-key map [?p]   'nmcobol-paragraph-skel)
    map)
  "Keymap for `nmcobol-mode'.")

(defvar nmcobol-eldoc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?m]   'nmcobol-eldoc-make-list)
    (define-key map [?s]   'nmcobol-eldoc-scan-buffer)
    (define-key map [?v]   'nmcobol-eldoc-visit-file)
    (define-key map [?w]   'nmcobol-eldoc-where-def)
    map)
  "Keymap for `nmcobol-mode'.")

(defvar nmcobol-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab]             'indent-according-to-mode)
    (define-key map [?\C-c ?\C-c]     'column-marker-here)
    (define-key map [?\C-c ?\C-e]      nmcobol-eldoc-map)
    (define-key map [?\C-c ?\C-f]     'auto-fill-mode)
    (define-key map [?\C-c ?\C-o]     'nmcobol-customize-options)
    (define-key map [?\C-c ?\C-r]     'popup-ruler)
    (define-key map [?\C-c ?\C-s]      nmcobol-skeleton-map)
    (define-key map [?\C-c ?=]        'nmcobol-addup-pics)
    (define-key map [?\C-c return]    'comment-indent-new-line)
    (define-key map [?*]              'nmcobol-char-maybe-comment)
    (define-key map [?/]              'nmcobol-char-maybe-comment)
    map)
  "Keymap for `nmcobol-mode'.")

(defun nmcobol-setup-menu ()
  "Adds a menu of NMCOBOL specific functions to the menu bar."
  (define-key (current-local-map) [menu-bar nmcobol-menu]
    (cons "NMCobol" (make-sparse-keymap "NMCobol")))
  (define-key (current-local-map) [menu-bar nmcobol-menu customize]
    '(menu-item "Customize" nmcobol-customize-options
                :key-sequence [?\C-c ?\C-o]
                :help "Customize nmcobol-mode options"))
  (define-key (current-local-map) [menu-bar nmcobol-menu comment-eol]
    '(menu-item "Comment EOL" comment-indent-new-line
                :key-sequence [?\C-c return]
                :help "Continues comment on new line"))
  (if (featurep 'column-marker)
      (define-key (current-local-map) [menu-bar nmcobol-menu column]
        '(menu-item "Column Marker" column-marker-here
                    :key-sequence [?\C-c ?\C-c]
                    :help "Puts column marker at current column (C-u removes)")))
  (define-key (current-local-map) [menu-bar nmcobol-menu ruler]
    '(menu-item "Ruler" popup-ruler
                :key-sequence [?\C-c ?\C-r]
                :help "Inserts temporary ruler"))
  (define-key (current-local-map) [menu-bar nmcobol-menu eldoc-show]
    '(menu-item "Eldoc Where" nmcobol-eldoc-where-def
                :key-sequence [?\C-c ?\C-e ?w]
                :help "Shows Where function at point is defined"))
  (define-key (current-local-map) [menu-bar nmcobol-menu eldoc-visit]
    '(menu-item "Eldoc Visit" nmcobol-eldoc-visit-file
                :key-sequence [?\C-c ?\C-e ?v]
                :help "Visits file defining function at point"))
  (define-key (current-local-map) [menu-bar nmcobol-menu eldoc-update]
    '(menu-item "Eldoc Scan" nmcobol-eldoc-scan-buffer
                :key-sequence [?\C-c ?\C-e ?s]
                :help "Updates buffer-local eldoc entries (in memory)"))
  (define-key (current-local-map) [menu-bar nmcobol-menu eldoc-create]
    '(menu-item "Eldoc Make List" nmcobol-eldoc-make-list
                :key-sequence [?\C-c ?\C-e ?m]
                :help "Puts current file eldoc entries in a file."))
  (define-key (current-local-map) [menu-bar nmcobol-menu skeletons]
    (cons "Skeletons" (make-sparse-keymap "Skeletons")))
  (define-key (current-local-map) [menu-bar nmcobol-menu skeletons if]
    '(menu-item "If Then" nmcobol-if-skel
                :key-sequence [?\C-c ?\C-s ?i]
                :help "Inserts an If/Then statement"))
  (define-key (current-local-map) [menu-bar nmcobol-menu skeletons paragraph]
    '(menu-item "New Paragraph" nmcobol-paragraph-skel
                :key-sequence [?\C-c ?\C-s ?p]
                :help "Inserts comment bars for new paragraph name")))

;; All keyword lists get sorted so new words can be anywhere within the
;; appropriate list.  The keywords are currently only used for highlighting but
;; more uses such as abbrev-mode are in progress.

(defvar nmcobol-keywords-directives     ;font-lock-preprocessor-face
  '( "ANSI"            "BLANK"           "CALL-SHARED"     "CANCEL"
     "CHECK"           "CODE"            "COLUMNS"         "COMPACT"
     "COMPILE"         "CONSULT"         "CROSSREF"        "DIAGNOSE-74"
     "DIAGNOSE-85"     "DIAGNOSEALL"     "ENDIF"           "ENDUNIT"
     "ENV"             "ERRORFILE"       "ERRORS"          "FIPS"
     "FMAP"            "HEADING"         "HEAP"            "HIGHPIN"
     "HIGHREQUESTERS"  "ICODE"           "IF"              "IFNOT"
     "INNERLIST"       "INSPECT"         "LARGEDATA"       "LD"
     "LESS-CODE"       "LIBRARY"         "LINES"           "LIST"
     "LMAP"            "MAIN"            "MAP"             "NLD"
     "NOBLANK"         "NOCANCEL"        "NOCODE"          "NOCOMPACT"
     "NOCONSULT"       "NOCROSSREF"      "NODIAGNOSE-74"   "NODIAGNOSE-85"
     "NODIAGNOSEALL"   "NOFIPS"          "NOICODE"         "NOINNERLIST"
     "NOINSPECT"       "NOLIST"          "NOLMAP"          "NOMAP"
     "NON-SHARED"      "NONSTOP"         "NOPORT"          "NOSAVEABEND"
     "NOSEARCH"        "NOSHOWCOPY"      "NOSHOWFILE"      "NOSQL"
     "NOSUPPRESS"      "NOSYMBOLS"       "NOTRAP2"         "NOTRAP2-74"
     "NOWARN"          "OPTIMIZE"        "PERFORM-TRACE"   "PORT"
     "RESETTOG"        "RUNNABLE"        "RUNNAMED"        "SAVE"
     "SAVEABEND"       "SEARCH"          "SECTION"         "SETTOG"
     "SHARED"          "SHOWCOPY"        "SHOWFILE"        "SOURCE"
     "SQL"             "SQLMEM"          "SUBSET"          "SUBTYPE"
     "SUPPRESS"        "SYMBOLS"         "SYNTAX"          "TANDEM"
     "TRAP2"           "TRAP2-74"        "UL"              "WARN")
  "List of NMCOBOL compiler directives.
Used to create the `font-lock-keywords' table.")

(defvar nmcobol-keywords-imperatives
  '( "ACCEPT"        "DISPLAY"       "MULTIPLY"      "STOP"
     "ADD"           "DIVIDE"        "OPEN"          "STRING"
     "ALTER"         "ENTER"         "PERFORM"       "SUBTRACT"
     "CALL"          "EXIT"          "READ"          "UNLOCKFILE"
     "CANCEL"        "GO TO"         "RELEASE"       "UNLOCKRECORD"
     "INITIALIZE"    "REWRITE"       "UNSTRING"      "CLOSE"
     "INSPECT"       "SET"           "WRITE"         "COMPUTE"
     "LOCKFILE"      "SORT"          "CONTINUE"      "MERGE"
     "START"         "DELETE"        "MOVE")
  "List of NMCOBOL keywords identifying an imperative statement.
Used by indentation routines in their determination of such."
)

(defvar nmcobol-block-keywords
  '("ADD"            "COMPUTE"        "DELETE"         "DIVIDE"
    "EVALUATE"       "IF"             "MULTIPLY"       "PERFORM"
    "READ"           "REWRITE"        "SEARCH"         "START"
    "STRING"         "SUBTRACT"       "UNSTRING"       "WRITE"
   )
  "A list of words that should have open paren syntax - conditionally.
`nmcobol-get-block-type' determines if they actually require the
END-<word> statement." )

(defvar nmcobol-keywords-statements     ;font-lock-keyword-face
  '("ACCEPT"        "ADD"           "ALTER"         "CALL"          
    "CANCEL"        "CHECKPOINT"    "CLOSE"         "COMPUTE"       
    "CONTINUE"      "COPY"          "DELETE"        "DISPLAY"       
    "DIVIDE"        "ELSE"          "END"           "END-ADD"       
    "END-COMPUTE"   "END-DELETE"    "END-DIVIDE"    "END-EVALUATE"  
    "END-IF"        "END-MULTIPLY"  "END-OF-PAGE"   "END-PERFORM"   
    "END-READ"      "END-RECEIVE"   "END-RETURN"    "END-REWRITE"   
    "END-SEARCH"    "END-START"     "END-STRING"    "END-SUBTRACT"  
    "END-UNSTRING"  "END-WRITE"     "ENTER COBOL"   "ENTER"         
    "EVALUATE"      "EXIT"          "FD"            "FILE"          
    "GO TO"         "IF"            "INITIALIZE"    "INSPECT"       
    "LOCKFILE"      "MERGE"         "MOVE"          "MULTIPLY"      
    "OPEN"          "PERFORM"       "READ"          "RELEASE"       
    "REPLACE"       "RETURN"        "REWRITE"       "SD"            
    "SEARCH"        "SELECT"        "SET"           "SORT"          
    "START"         "STARTBACKUP"   "STOP"          "STRING"        
    "SUBTRACT"      "THEN"          "UNLOCKFILE"    "UNLOCKRECORD"  
    "UNSTRING"      "USE"           "WHEN"          "WRITE"         )
  "List of NMCOBOL statement keywords.
Used to create the `font-lock-keywords' table.")

(defvar nmcobol-keywords-deprecated     ;font-lock-warning-face
  '( "STARTBACKUP" "CHECKPOINT")
  "List of NMCOBOL keywords and Builtin functions now deprecated.
Used to create the `font-lock-keywords' table")

(defvar nmcobol-keywords-reserved       ;font-lock-type-face
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
     "ZERO"                   "ZEROES")
  "List of NMCOBOL keywords reserved only in certain language contexts.
Used to create the `font-lock-keywords' table.")

(defvar nmcobol-keywords-std-fcns       ;font-lock-keyword-face
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
     "UPPER-CASE"             "VARIANCE"               "WHEN-COMPILED")
  "List of NMCOBOL standard functions.
Used to create the `font-lock-keywords' table.")

(defvar nmcobol-keywords-privileged     ;font-lock-warning-face
  '( "END-EXEC" "EXEC")
  "List of NMCOBOL privileged functions.
Used to create the `font-lock-keywords' table.")

(defvar nmcobol-keywords-builtin        ;font-lock-variable-name-face
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
     "PUTPARAMTEXT"                    "PUTSTARTUPTEXT")
  "List of NMCOBOL privileged builtin functions.
Used to create the `font-lock-keywords' table.")

(defcustom nmcobol-block-always-keywords
  '("ELSE"      "EVALUATE"  "IF"        "SEARCH"    "THEN"      
    "WHEN"      )
  "List of keywords that always require and END-<word>. Used in paren matching."
  :type  '(repeat (string :tag "word"))
  :group 'nmcobol)

(defcustom nmcobol-keyword-section-names-regexp
  "^\\s-\\{1,3\\}\\(\\w+\\)\\s-+\\(division\\(\\s-+using\\s-+[^.\n]+\\)?\\|section\\) *\\."
  "Defines a regexp that finds the names of divisions & sections.
Used to create the `font-lock-keywords' table."
  :type  'regexp
  :group 'nmcobol)

(defcustom nmcobol-keyword-fcn-names-regexp
  "^\\s-\\{1,3\\}\\(\\w+\\)\\s-*\\."
  "Defines a regexp that finds the names of paragraphs.
Used by `font-lock-keywords'. See also `nmcobol-imenu-fcn-names-regexp'"
  :type  'regexp
  :group 'nmcobol)

;;; Build keyword regexp from keyword lists

(defvar nmcobol-keywords-imperatives-regexp ()
  "regexp matching `nmcobol-keywords-imperatives'")
(defvar nmcobol-keywords-statements-regexp ()
  "regexp matching `nmcobol-keywords-statements'")
(defvar nmcobol-keywords-reserved-regexp ()
  "regexp matching `nmcobol-keywords-reserved'")
(defvar nmcobol-block-always-regexp ()
  "regexp matching `nmcobol-block-always-keywords'")
(defvar nmcobol-block-begin-regexp ()
  "regexp matching `nmcobol-block-keywords'")
(defvar nmcobol-block-end-regexp ()
  "regexp matching \"END-\" + `nmcobol-block-keywords'")
(defvar nmcobol-non-cobol-regexp
  "^[*/?]"
  "Expression describing comment and compiler directive lines.")

(defun nmcobol-setup-regexp-vars ()
  "Rebuilds regexp variables from keyword lists."
  (setq nmcobol-keywords-imperatives-regexp
        (concat (regexp-opt nmcobol-keywords-imperatives t) "\\(\\s-\\|\\.\\)")
        nmcobol-keywords-statements-regexp
        (concat "^\\([ \t]+\\)"
                (regexp-opt nmcobol-keywords-statements t) "\\(\\s-\\|\\.\\)")
        nmcobol-keywords-reserved-regexp ;not used!
        (nmcobol-keyword-anywhere-regexp nmcobol-keywords-reserved)
        nmcobol-block-always-regexp
        (concat (regexp-opt nmcobol-block-always-keywords t) "\\(\\s-\\|\\.\\)")
        nmcobol-block-begin-regexp
        (regexp-opt nmcobol-block-keywords  t)
        nmcobol-block-end-regexp
        (concat "END-" (regexp-opt nmcobol-block-keywords  t))))

;;; Font lock (highlighting)

(defcustom nmcobol-font-lock-always t
  "`nmcobol-mode' makes sure `font-lock-mode' is on for nmcobol-mode buffers.
Some things don't work if it's off so insuring it's on is the default."
  :type 'boolean
  :group 'nmcobol)

(defcustom nmcobol-primecode-warning t
  "Highlight instances of ]a ]d and ]e in column 1 with a warning face.
This alerts you that submission of this file to RMS/PrimeCode will fail
due to invalid contents.  nil disables this warning."
  :type 'boolean
  :group 'nmcobol)

(defun nmcobol-keyword-special-regexp ( word-list )
  "Returns a regexp that finds any of the words in WORD-LIST.
But only if the keyword is surrounded by non-word chars."
  (concat "\\W"(regexp-opt word-list t)"\\W"))

(defun nmcobol-keyword-anywhere-regexp ( word-list )
  "Returns a regexp that finds any of the words in WORD-LIST.
But only if the keyword is surrounded by non-word chars."
  (concat "\\b"(regexp-opt word-list t)"\\b"))

;; The next 4 def's work tightly together and, as coded, cannot be reused for
;; additional purposes.
(defvar nmcobol-keyword-on-directive-line-regexp () "Internal use only.")
(defun  nmcobol-keyword-on-directive-line-regexp ( word-list )
"Returns a function to find WORD-LIST only if line starts with ?"
  (setq nmcobol-keyword-on-directive-line-regexp
        (concat "\\b"(regexp-opt word-list t)"\\b"))
  'nmcobol-font-lock-directive-line)
(defvar nmcobol-amid-font-lock-excursion nil
  "Used by `nmcobol-font-lock-directive-line'.  When a line starting with
? in column 1 is detected this variable holds the context needed to
continue searching for more keywords.  If nil a line starting with ?
should be searched for.")
(make-variable-buffer-local 'nmcobol-amid-font-lock-excursion)
(defun nmcobol-font-lock-directive-line ( search-limit )
  "This function finds keywords only in lines starting with ?.  Valid
keywords are described by `nmcobol-keyword-on-directive-line-regexp'.
First a line beginning with ? is searched for.  Once found, point is
moved to the beginning of that area and limit is set to the end.
Keywords are searched for within that range.  If found, context is saved
in nmcobol-amid-font-lock-excursion and the match-data is returned.  If
not found, another line starting with ?  is searched for.  If saved
context exists when this function is called then another keyword is
searched for in the previously narrowed region.  If none is found the
next region is searched for."
  (let ((looking t))
    (while
        (and looking
             (or nmcobol-amid-font-lock-excursion
          (when (re-search-forward "^\\?.+\n" search-limit t)
            (setq nmcobol-amid-font-lock-excursion (point))
            (goto-char (match-beginning 0)))))
      (if (re-search-forward nmcobol-keyword-on-directive-line-regexp
               nmcobol-amid-font-lock-excursion t)
          (setq looking nil)
        (goto-char nmcobol-amid-font-lock-excursion)
        (setq nmcobol-amid-font-lock-excursion nil)))
    (not looking)))

(defvar nmcobol-find-syntactic--state ()
  "Used by `nmcobol-find-syntactic-keywords' to find multiple syntactic
elements which all must be anchored to the beginning of a line.
nil    = No searches on this line yet. skip line if it's a directive.
0      = look for sequence number in col 1-6 (removed)
1      = sequence/label area checked. look at body.
2      = body not a comment, any trailing comment marked, check for strings
marker = terminated string found check for more.")
(make-variable-buffer-local 'nmcobol-find-syntactic--state)
(defun nmcobol-find-syntactic-keywords ( search-limit )
  "Used by `font-lock-syntactic-keywords' to find comments and strings.
Returns t if either a comment or string is found, nil if neither is found.
match-data 1&2 are set for comments, 3&4 are set for a normal string, 5&6 are
set for eol-terminated strings.  Where the match pair mark the start character
and end character respectively.  Point is moved to the next line during this
function only after the last search completes for the current line.  A state
machine, controlled by `nmcobol-find-syntactic--state' sequences the searches."
  (let ((found nil)
        (save (point)))
    (while (and (< (point) search-limit)
                (not found))
           (cond
            ;; no comments or quotes? in compiler directives
            ((or (null nmcobol-find-syntactic--state)
                 (equal nmcobol-find-syntactic--state (make-marker)))
             (if (looking-at "^\\?")
                 (forward-line 1)       ;do this state on next line
               (setq nmcobol-find-syntactic--state 1) ;do next state on this line
             ))
            ;; see if entire line is a comment
            ((= 1 nmcobol-find-syntactic--state)
             (if (not (looking-at "^\\(?:*\\|/\\)"))
                 (setq nmcobol-find-syntactic--state 2) ;goto next state
               ;; else set match data and point to next line
               (looking-at "\\(.\\).*\\(\n\\|\\'\\)") ;setup match-data
               (forward-line 1)    ;next iteration looks at next line
               (setq nmcobol-find-syntactic--state ()
                     found t)))
            ;; look for strings only within columns 8-72 inclusive
            ((= 2 nmcobol-find-syntactic--state)
             (if (looking-at "^[-d D][^\"\n]\\{0,130\\}\"")
                 (let* ((open-quote (list (copy-marker (1- (match-end 0)))
                                          (copy-marker (match-end 0))))
                        (leol (copy-marker (line-end-position)))
                        close-quote)
                   (setq found t)
                   (goto-char (cadr open-quote))
                   (if (search-forward "\"" leol t)
                       (progn           ; normally ending string
                         (setq close-quote (match-data)
                               nmcobol-find-syntactic--state (cadr close-quote))
                         (beginning-of-line)
                         (set-match-data
                          `(,(car open-quote) ,(cadr close-quote)
                            nil nil nil nil ;match-string 1&2 not found
                            ,@open-quote ,@close-quote)) ;3&4 are normal string
                       )
                     ;; implicit string end
                     (forward-line 1)   ;next iteration looks at next line
                     (setq close-quote (list (copy-marker (1- leol)) leol)
                           nmcobol-find-syntactic--state ())
                     (set-match-data
                      `(,(car open-quote) ,(cadr close-quote)
                        nil nil nil nil ;match-string 1&2 not found
                        nil nil nil nil ;match-string 3&4 not found
                        ,@open-quote ,@close-quote)) ;5&6 unterminated string
                   ))
               ;; no string was found.  Start new analysis on next line
               (forward-line 1)
               (setq nmcobol-find-syntactic--state ())))
            ;; a string has been found look for another after it
            ((markerp nmcobol-find-syntactic--state)
             (let ((leol (copy-marker (line-end-position)))
                   open-quote close-quote)
               (goto-char nmcobol-find-syntactic--state)
               (if (search-forward "\"" leol t)
                   (progn
                     (setq open-quote (match-data)
                           found t)
                     (if (search-forward "\"" leol t)
                         (progn         ; normally ending string
                           (beginning-of-line) ;next iteration starts here again
                           (setq close-quote (match-data)
                                 nmcobol-find-syntactic--state (cadr close-quote))
                           (set-match-data
                            `(,(car open-quote) ,(cadr close-quote)
                              nil nil nil nil ;match-string 1&2 not found
                              ,@open-quote ,@close-quote)) ;3&4 normal string
                         )
                       ;; implicit string end
                       (forward-line 1) ;next iteration looks at next line
                       (setq close-quote (list (copy-marker (1- leol)) leol)
                             nmcobol-find-syntactic--state ())
                       (set-match-data
                        `(,(car open-quote) ,(cadr close-quote)
                          nil nil nil nil ;match-string 1&2 not found
                          nil nil nil nil ;match-string 3&4 not found
                          ,@open-quote ,@close-quote)) ;5&6 unterminated string
                     ))
                 (forward-line 1)
                 (setq nmcobol-find-syntactic--state ()))))))
    ;; Point should not return forward of search-limit
    (and (> (point) search-limit) (goto-char search-limit))
    ;; point shouldn't move if nothing was found.
    (prog1 found (or found (goto-char save)))))

(defvar nmcobol-static-font-lock-keywords
  ;; font-lock-keywords is a symbol or list of symbols yielding the keywords to
  ;; be fontified.  Keywords are listed here using either (MATCHER . FACENAME)
  ;; or (MATCHER . (MATCH FACENAME)) syntax.  Other options are available but
  ;; not used here.  For simplicity, all regexp's were designed so MATCH would
  ;; be 1.  Nothing forced this but to me it makes debug/maintenance easier.
  `(("^\\([^ ?Dd*/-]\\)" 1 font-lock-warning-face)
    ("^\\([?Dd-]\\)"     1 font-lock-builtin-face)
    (,nmcobol-keyword-section-names-regexp
     1 font-lock-function-name-face)
    (,(nmcobol-keyword-on-directive-line-regexp nmcobol-keywords-directives)
     1 font-lock-preprocessor-face)
    (,(nmcobol-keyword-anywhere-regexp          nmcobol-keywords-builtin)
     1 font-lock-variable-name-face)
    (,(nmcobol-keyword-special-regexp           nmcobol-keywords-statements)
     1 font-lock-keyword-face)
    (,(nmcobol-keyword-anywhere-regexp          nmcobol-keywords-std-fcns)
     1 font-lock-keyword-face)
    (,(nmcobol-keyword-anywhere-regexp (append  nmcobol-keywords-deprecated
                                                nmcobol-keywords-privileged))
     1 font-lock-warning-face)
    (,(nmcobol-keyword-anywhere-regexp          nmcobol-keywords-reserved)
     1 font-lock-type-face)
    (,nmcobol-keyword-fcn-names-regexp
     1 font-lock-function-name-face)))

(defvar nmcobol-font-lock-keywords ())
(defun nmcobol-build-font-lock-keywords ()
  "Creates `font-lock-keywords' based on current customize settings."
  (append nmcobol-static-font-lock-keywords
          `(,(when nmcobol-primecode-warning
               ;; ]a  ]d or ]e cannot appear in col 1-2 if using PrimeCode.
               '("^\\][ade]" . font-lock-warning-face)))))

(defvar nmcobol-this-paren-type ()
  "Used internally by `nmcobol-font-lock-syntactic-keywords'"
)

(defvar nmcobol-block-symbol-prefix "nmcobol-blk-typ-"
  "Paren matching symbols are built using this prefix.
You probably don't want to change it "
)

(defcustom nmcobol-else-gets-paren-syntax t
  "When `show-paren-mode' is ON and this is nil, IF always matches END-IF only.
When this variable is non-nil, IF can match ELSE and ELSE matches END-IF."
  :type 'boolean
  :group 'nmcobol)

(defun nmcobol-find-paren-words (lim)
  "Function to set paren syntactic properties for Cobol's block keywords.
Returns t and sets both `nmcobol-this-paren-type' and appropriate match-data
when keywords requiring paren syntax are seen within range of point & lim."
  (let ((looking t)   ;looping continues until lim or found (not looking)
        open-type               ;type of paren needed to match this open
        close-type              ;type of paren needed to match this close
        open-loc
        close-loc
        word)
    ;; walk thru each statement
    (while (and looking
                (re-search-forward nmcobol-keywords-statements-regexp
                                   lim 'move-anyway))
      ;; Save the word and it's location in case paren class is to be applied
      (setq word (match-string-no-properties 2)
            open-loc (match-beginning 2)
            close-loc (1- (match-end 2)))
      ;; Look for the different types of paren class we might apply
      (cond
       ;; The ELSE keyword can be ignored or treated as both open and close
       ((and nmcobol-else-gets-paren-syntax (string= "ELSE" word))
        ;; The leading E of Else will match the I of IF and the trailing
        ;; E should match the F of END-IF.  When no ELSE is present the I
        ;; of IF will match the F of END-IF.
        (setq looking    nil
              word       open-loc
              open-loc   close-loc
              close-loc  word
              close-type (setq open-type
                               (intern
                                (concat nmcobol-block-symbol-prefix "IF")))))

       ;; close paren keywords always have close paren syntax
       ((string-match nmcobol-block-end-regexp word)
        (setq close-type
              (intern
               (concat nmcobol-block-symbol-prefix
                       (upcase (match-string-no-properties 1 word))))
              looking nil))

       ;; 'possible' open paren keywords. More checking needed.
       ((string-match nmcobol-block-begin-regexp word)
        (let ((next (point)))
          (if (re-search-forward nmcobol-keywords-statements-regexp
                                 lim 'move-anyway)
              (setq next (match-beginning 0)))
          (goto-char open-loc)
          (if (setq open-type (nmcobol-get-block-type next))
              (setq looking nil))
          (goto-char (1+ close-loc))))))
    ;; Build the response data
    (if looking
        ;; nil is returned if no paren syntax to apply
        (set-match-data ())
      ;; both the match data and the paren type data must be set
      (set-match-data
       (append (list open-loc (1+ close-loc))
               (if open-type
                   (list open-loc (1+ open-loc))
                 (list () ()))
               (if close-type
                   (list close-loc (1+ close-loc)))))
      (setq nmcobol-this-paren-type
            (cons
             ;; declare the open-paren type
             (cons 4 open-type)
             ;; declare the close-paren type
             (cons 5 close-type)))
      ;; t must be returned iff match-data should be acted upon
      t)))

(defvar nmcobol-font-lock-syntactic-keywords
 `(
   ;; nmcobol-find-syntactic-keywords returns matches 1&2 for comments, 3&4
   ;; for strings.  5&6 for eol terminated strings.  I must use "|"(15)
   ;; rather than "\""(7) for eol terminated strings because the begin
   ;; and end characters must be the same when "\""(7) is used.
   (nmcobol-find-syntactic-keywords (1 "<" t t)  (2 ">" t t)
                                    (3 "\"" t t) (4 "\"" t t)
                                    (5 "|" t t)  (6 "|" t t))
   ;; nmcobol-find-paren-words returns match 1 for open paren syntax and
   ;; match 2 for close paren syntax.  The car of nmcobol-this-paren-type
   ;; is used for open paren type and the cdr for close type.  The types
   ;; must be configurable due to the large number of them.
   (nmcobol-find-paren-words  (1 (car nmcobol-this-paren-type) t t)
                              (2 (cdr nmcobol-this-paren-type) t t)
   )
  )
 "A list of regexp's or functions.  Used to add syntax-table properties to
characters that can't be set by the syntax-table alone.")

;(defun nmcobol-start-of-statement-hook ()
;  "Used as `font-lock-extend-region-functions' hook which see."
;  ;; WARNING - any malfunction in this can cause emacs to enter a loop
;  ;; that C-g will not break out of.  Your session (and all edits) will
;  ;; be toast.
;  ;; There seems to be no good way to suppress the warning caused by
;  ;; accessing font-lock-beg.  It's required but apparently only
;  ;; defined within a let statement.
;  (goto-char font-lock-beg)
;  (unless (bolp) (beginning-of-line))
;  (unless (or (looking-at nmcobol-non-cobol-regexp)
;              (looking-at nmcobol-keywords-statements-regexp))
;      (let* ((lim (progn (nmcobol-backward-sentence 0)(point))))
;        (goto-char font-lock-beg)
;        (re-search-backward
;         nmcobol-keywords-statements-regexp lim 'move-anyway)
;        ;; If I reached the beginning of the sentence, there may be
;        ;; comments and blank lines: they don't need refontification.
;        (when (= (point) lim)
;          (while (or (looking-at "^[ \t]*$")
;                     (looking-at nmcobol-non-cobol-regexp))
;            (forward-line)))))
;    (unless (eq (point) font-lock-beg)
;      (setq font-lock-beg (point))))

(defun nmcobol-start-of-statement ()
  "Used as the syntax-begin function of `font-lock-defaults' which see."
  ;; WARNING - any malfunction in this can cause emacs to enter a loop
  ;; that C-g will not break out of.  Your session (and all edits) will
  ;; be toast.
  (unless (bolp) (beginning-of-line))
  (unless (or (looking-at nmcobol-non-cobol-regexp)
              (looking-at nmcobol-keywords-statements-regexp))
      (let* ((start (point))
             (lim (progn (nmcobol-backward-sentence 0)(point))))
        (goto-char start)
        (re-search-backward
         nmcobol-keywords-statements-regexp lim 'move-anyway)
        ;; If I reached the beginning of the sentence, there may be
        ;; comments and blank lines: they don't need refontification.
        (when (= (point) lim)
          (while (and (< (point) start)
                      (or (looking-at "^[ \t]*$")
                          (looking-at nmcobol-non-cobol-regexp)))
            (forward-line))
          (if (> (point) start)
              (goto-char start))))))

(defun nmcobol-setup-font-lock ()
  "Sets up the buffer local value for font-lock-defaults and optionally
turns on font-lock-mode"
  ;; I use font-lock-syntactic-keywords to set some properties and I
  ;; don't want them ignored.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;; I really can't imagine anyone wanting this off.
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; This allows column markers to be different in separate buffers.
  (set (make-local-variable 'nmcobol-font-lock-keywords)
       (nmcobol-build-font-lock-keywords))
  ;; make sure the parsing state is reset
  (setq nmcobol-find-syntactic--state ())
;  (setq font-lock-extend-region-functions
;        (append font-lock-extend-region-functions
;                '(nmcobol-start-of-statement-hook)))
  ;; This is where all the font-lock stuff actually gets set up.  Once
  ;; font-lock-defaults has it's value, setting font-lock-mode true should
  ;; cause all your syntax highlighting dreams to come true.
  (setq font-lock-defaults
         ;; The first value is all the keyword expressions.
       '(nmcobol-font-lock-keywords
         ;; keywords-only means no strings or comments get fontified
         nil
         ;; case-fold (ignore case)
         t
         ;; syntax-alist.  See also imenu-syntax-alist
         ;; This can't override syntax applied by font-lock-syntactic-keywords
         ;; It only overrides nmcobol-mode-syntax-table. 
         nil
         ;; syntax-begin - function to move outside syntactic block
         ;; This doesn't work for some reason.  I'm using the hook
         ;; font-lock-extend-region-functions for this.
         nil ; nmcobol-start-of-statement
         ;; font-lock-syntactic-keywords
         ;; takes (matcher (match syntax override lexmatch) ...)...
         (font-lock-syntactic-keywords . nmcobol-font-lock-syntactic-keywords)))
  ; font lock is turned on by default in this mode. Use customize to disable.
  (when nmcobol-font-lock-always (font-lock-mode t)))

;;; Static Column Markers

(defcustom nmcobol-column-marker-1 79
  "*Turns on column-marker-1 (which see) at the specified column.
Use of this feature requires the column-marker.el package be loaded or on
the search list."
  :type 'integer
  :group 'nmcobol)
(make-variable-buffer-local 'nmcobol-column-marker-1)

(defcustom nmcobol-column-marker-2 0
  "*Turns on column-marker-2 (which see) at the specified column.
Use of this feature requires the column-marker.el package."
  :type 'integer
  :group 'nmcobol)
(make-variable-buffer-local 'nmcobol-column-marker-2)

(defun nmcobol-setup-column-markers ()
  "Turns on column markers if configured and available.
See `nmcobol-column-marker-1' and `nmcobol-column-marker-2' "
  (if (not (condition-case () (require 'column-marker) (error nil)))
      (if (not (and (zerop nmcobol-column-marker-1)
                    (zerop nmcobol-column-marker-2)))
          (message "column markers are configured but %s"
                   " column-marker feature not available."))
    (setq indent-tabs-mode nil)      ;documented as buffer local
    (column-marker-1 nmcobol-column-marker-1)
    (column-marker-2 nmcobol-column-marker-2)))

;;; Imenu & Which-function

(defcustom nmcobol-imenu-menubar t
  "If not nil, `imenu-add-to-menubar' is called during mode initialization.
This adds a [Menu name] menu to your menu bar.  By default the menu
contains a list of all procedures, sections and pages in your program.
You can go directly to any item on the menu by selecting it.  You can
control what appears on this menu by modifying
`nmcobol-imenu-expression-alist'.  You must turn imenu on for this to
work.  See `imenu' in the Emacs reference manual for more information.
Personally I recommend customizing `imenu-sort-function' to sort by
name."
  :type  '(choice :tag "Menu Name"
                  (string :tag "Menu Name")
                  (const :tag "Index" t)
                  (const :tag "None" nil))
  :group 'nmcobol)

(defvar nmcobol-imenu-syntax-alist ()
  "Overrides to `nmcobol-mode-syntax-table' used during `imenu-generic-expression' search."
  ;;AFAIK there are no character adjustments needed during imenu search.
)

(defcustom nmcobol-imenu-fcn-names-regexp
  "^[ \t]\\{1,3\\}\\(\\w+\\([^T]\\|[^I]T\\|[^X]IT\\|[^E]XIT\\|[^-]EXIT\\)\\)[ \t]*\\."
  "Defines a regexp that finds the names of paragraphs.
Used to build the `imenu' index."
  :type  'regexp
  :group 'nmcobol)

(defcustom nmcobol-imenu-expression-alist
  `((nil                   ,nmcobol-imenu-fcn-names-regexp       1)
    ("Divisions/Sections"  ,nmcobol-keyword-section-names-regexp 1)
    ("?Sections"           "^\\?section\\s-+\\(\\w+\\)\\b"       1)
    ("?Pages"              "^\\?page\\s-+\"\\s-*\\(.+?\\)\""     1))
  "A list of regular expressions for creating an `imenu' index.

Each element has the form (list-name regexp num).

Where list-name is the name of the submenu under which items matching regexp
are found and num is the expression index defining the label to use for the
submenu entry.  When num = 0 the entire matching regexp text appears under
list-name.  When list-name is nil the matching entries appear in the root
imenu list rather than in a submenu.  See also `nmcobol-imenu-menubar'"
  :type '(repeat (list (choice :tag "Submenu Name" string (const nil))
                       regexp (integer :tag "Regexp index")))
  :group 'nmcobol)

(defcustom nmcobol-display-which-function t
  "This option turns `which-func' on for all `nmcobol-mode' buffers.
`which-func' is a package that causes the current function, section or
page to be displayed on the mode line.  `which-func' uses `imenu'.  Also
see `nmcobol-imenu-expression-alist' for more information."
  :type 'boolean
  :group 'nmcobol)

(defun nmcobol-setup-imenu ()
  "Installs nmcobol-imenu-generic-expression & nmcobol-imenu-syntax-alist."
  ;; imenu doc says these 3 are buffer-local by default
  (setq imenu-generic-expression nmcobol-imenu-expression-alist)
  (setq imenu-syntax-alist nmcobol-imenu-syntax-alist)
  (setq imenu-case-fold-search t) ;NMCOBOL are never case sensitive
  (when nmcobol-imenu-menubar
    (if (condition-case ()
            (progn (require 'imenu) t)
          (error nil))
        (imenu-add-menubar-index)
      (message "nmcobol-imenu-menubar is set but imenu feature not available.")))
  (when nmcobol-display-which-function
    (if (condition-case ()
            (progn (require 'which-func) t)
          (error nil))
        (which-function-mode t)
      (message "nmcobol-display-which-function set but which-func not available"))))

;;; Adaptive-fill / auto-fill (needs much work but it's a start)

(defcustom nmcobol-restrict-auto-fill t
  "When not nil a buffer local value for `fill-nobreak-predicate' is created
to prevent code from being accidentally realligned.  The function uses syntax
highlighting to detect comments so `font-lock-mode' must be enabled to work."
  :type 'boolean
  :group 'nmcobol)

(defun nmcobol-setup-adaptive-fill ()
  "Sets up the NMCOBOL-MODE adaptive-fill variables.  DOESN'T WORK!"
;; All of this section is left over from TAL mode !needs attention!
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
       "\\( +[0-9]\\| +\\w+\\( +\\(division\\(\\s-+using\\s-+[^.\n]+\\)?\\|section\\)\\) *\\.\\)")
  (set (make-local-variable 'paragraph-separate)
       "\n")
  (set (make-local-variable 'adaptive-fill-regexp)
       "^\\s-*\\(!\\|--\\)[~%^&()_#[*|;:-=+]*\\s-*")
  (set (make-local-variable 'adaptive-fill-first-line-regexp)
       adaptive-fill-regexp)
  (when nmcobol-restrict-auto-fill
    ; This is supposed to restrict auto-fill to comments only
    (fset (make-local-variable 'fill-nobreak-predicate)
          (lambda ()
            (not (eq (get-text-property (point) 'face)
                     'font-lock-comment-face))))))

;;; Indentation

(defcustom nmcobol-righthand-keyword-column nil
  "The column to which `nmcobol-secondary-indent-words' are indented.
38 is the default for DDL output so that value is mirrored here.
Setting this to nil prevents secondary indentation."
  :type '(choice (integer :tag "Indent column")
                 (const   :tag "Don't indent" nil))
  :group 'nmcobol)
(make-variable-buffer-local 'nmcobol-righthand-keyword-column)

(defcustom nmcobol-secondary-indent-words
  '("BY" "DELIMITED" "FROM" "GIVING" "IS" "OF" "PIC"
    "SHARED" "TO" "USING" "VALUE" "WITH")
  "List of keywords to be indented to `nmcobol-righthand-keyword-column' col."
  :type '(repeat (string :tag "word"))
  :group 'nmcobol
)

(defun nmcobol-char-maybe-comment (arg)
  "Removes leading white space if character is preceeded only by white space.
The behavior can be circumvented with C-u.
A numeric prefix does not prevent the removal of white space."
  (interactive "*P")
  (if (and (looking-back "^[ \t]*")
           (or (null arg)
               (numberp arg)))
      (progn
        (replace-match "")
        (self-insert-command (prefix-numeric-value arg))
        (if (null arg)(indent-relative t)))
    (self-insert-command (if (numberp arg)(prefix-numeric-value arg) 1))))

(defun nmcobol-indent-region (&optional beg end)
  "Indents all lines even partly within the selected region.
If BEG/END are nil, indent the current line."
  (interactive)
  (if (and (null beg) transient-mark-mode mark-active)
      (setq beg (min (point)(mark))
            end (max (point)(mark))
            deactivate-mark t))
  (if (equal beg end)
      (nmcobol-indent-line)
    (setq end (set-marker (make-marker) end))
    (goto-char beg)
    (while (< (point) end)
      (nmcobol-indent-line t)
      (forward-line))
    (goto-char end)
    (set-marker end nil)))

(defun nmcobol-re-search-for-keyword (regexp bound)
  "Like re-search-forward but only program lines are examined.
Matches within quoted strings are also ignored.
Obviously REGEXP can't span lines."
  (while (and (> bound (point))
              (or (looking-at nmcobol-non-cobol-regexp)
                  (not (re-search-forward regexp (line-end-position) t))
                  (eq (get-text-property (match-beginning 0) 'face)
                      'font-lock-string-face)))
    (forward-line))
  (< (point) bound))

(defun nmcobol-get-block-type (next)
  "Returns a value indicating the type of block at point.  Nil if none.
NEXT is the buffer position of the next COBOL statement.  The search for
keywords that determine if this statement starts a block, ends at NEXT.
Point is left where the search ended."
  (if (looking-at nmcobol-block-always-regexp)
      (intern (concat nmcobol-block-symbol-prefix
                      (match-string-no-properties 1)))
    (let ((loc (point))
          sym)
      (cond
       ((looking-at "PERFORM\\s-+")
        (goto-char (match-end 0))
        (if (or (looking-at nmcobol-keywords-imperatives-regexp)
                (looking-at "\\(\\s_\\|\\w\\)+\\s-+TIMES\\_>")
                (looking-at "\\(WITH\\|TEST\\|UNTIL\\|VARYING\\)\\_>"))
            (setq sym (intern (concat nmcobol-block-symbol-prefix
                                      "PERFORM")))))
       
       ((looking-at "\\(ADD\\|COMPUTE\\|DIVIDE\\|MULTIPLY\\|SUBTRACT\\)\\_>")
        (setq sym (intern (concat nmcobol-block-symbol-prefix
                                  (match-string-no-properties 0))))
        (goto-char (match-end 0))
        ;; Look for the ON ERROR clause
        (unless (nmcobol-re-search-for-keyword "\\s-ERROR\\_>" next)
          (setq sym ())))
       
       ((looking-at "\\(READ\\|START\\|DELETE\\|DELETE\\|REWRITE\\)\\_>")
        (setq sym (intern (concat nmcobol-block-symbol-prefix
                                  (match-string-no-properties 0))))
        (goto-char (match-end 0))
        ;; Look for the [NOT] AT END or INVALID KEY clause
        (unless (nmcobol-re-search-for-keyword "\\s-\\(END\\|INVALID\\)\\_>"
                                               next)
          (setq sym ())))
       
       ((looking-at "\\(UNSTRING\\|STRING\\)\\_>")
       (setq sym (intern (concat nmcobol-block-symbol-prefix
                                 (match-string-no-properties 0))))
       (goto-char (match-end 0))
       ;; Look for the [NOT] ON OVERFLOW clause
       (unless (nmcobol-re-search-for-keyword "\\s-OVERFLOW\\_>" next)
         (setq sym ())))
       
       ((looking-at "WRITE\\_>")
        (setq sym (intern (concat nmcobol-block-symbol-prefix
                                  (match-string-no-properties 0))))
        (goto-char (match-end 0))
        ;; Look for EOP, END-OF-PAGE, or [NOT] INVALID KEY clauses
        (unless (nmcobol-re-search-for-keyword
                 "\\s-\\(EOP\\|END-OF-PAGE\\|INVALID\\)\\_>" next)
          (setq sym ()))))
      (goto-char loc)
      sym)))

(defun nmcobol-indent-line (&optional batch-mode)
  "Indents the current NMCOBOL line appropriately for it's context.
BATCH-MODE is used by `nmcobol-indent-region' to prevent buffer
modifications that only consist of blanks in otherwise empty lines
and to prevent arithmetic operations involving * or / from being
converted to comment lines accidentally."
  (save-match-data
    (let* (nmcobol-sentence-includes-comments
           (loc (point-marker))         ;marker of starting cursor
           (col (current-column))       ;cursor's starting column
           (bol (progn (beginning-of-line)(point))) ;beginning of line loc
           (cur (prog2 (skip-chars-forward " \t") ;current indent amount
                    (current-column)
                  (goto-char bol))))
      ;; I'm at the beginning of the line to indent - do it
      (cond
       ;; Don't move lines starting like this
       ((looking-at "[-?/*dD]")
        ;(message "dont move")
        (goto-char loc)
        (set-marker loc nil))

       ;; Non-code lines all start in column 0
       ((and (not batch-mode)(looking-at "\\([ \t]+\\)[?/*]"))
        ;(message "col 0")
        (replace-match "" nil nil nil 1)
        (goto-char loc)
        (set-marker loc nil))

       ;; Blank lines start where prior line left off or column 1 if none
       ((looking-at "[ \t]*\n")
        ;(message "relative")
        (set-marker loc nil)
        (if batch-mode ()  ;don't indent blank lines if flag non-nil
          (let ((indent (save-excursion
                          (if (re-search-backward "^[ \t]+[^ \t\n]" nil t)
                              (skip-chars-forward " \t" (line-end-position))
                            1))))
            (end-of-line)
            (let ((cc (- indent (current-column))))
              (if (> 0 cc)               ;Move backward needed
                  (backward-delete-char-untabify (- cc))
                (indent-to indent))))))

       ;; Period only lines are like new sentences within a paragraph
       ((looking-at "[ \t]*\\.")
        ;(message "period")
        (goto-char loc)
        (set-marker loc nil)
        (unless (= 4 cur)
          (replace-match "    ." nil nil nil 0)))

       ;; This is the messy part - find out what kind of line this is
        (t
         (let* ((typ (nmcobol-beginning-of-sentence)) ; Get sentence type
                (sos (point))                         ; start of sentence loc
                (indent 4)                            ; default sentence start
                extra                   ; add to indent in special cases
                prior-typ)

            (if (= bol (point))
                ;; This is the beginning of a new sentence of type typ
                (cond
                  ((numberp typ)        ;it's a numbered sentence

                   ;(message "number")
                   (if (or (= 0 typ)(= 1 typ)(= 66 typ)(= 77 typ))
                       ;; These numbers are top level
                       (setq indent 1)

                     ;; Other numbers are relative to prior sentence.
                     (nmcobol-backward-sentence 1)
                     (setq prior-typ (nmcobol-beginning-of-sentence)
                           indent (if (looking-at "[ \t]+")
                                      (prog2 (goto-char (match-end 0))
                                             (current-column)
                                             (beginning-of-line))
                                    0)
                           extra (if (looking-at "[ \t]+[0-9]\\{1,2\\}[ \t]+")
                                     (prog2 (goto-char (match-end 0))
                                         (- (current-column) indent)
                                       (beginning-of-line))))
                     (cond
                      ;; Error case. Prior statement isn't numbered
                      ((not (numberp prior-typ))
                       ;(message "number - prior non")
                       (setq indent (+ indent 3)))

                      ;; Prior statement is same level. Use same indent.
                      ((= prior-typ typ)
                       ;(message "number - prior same")
                      )

                      ;; Current is subordinate to prior, indent deeper
                      ((> typ prior-typ)
                       ;(message "number - prior superior")
                       (setq indent (+ indent (or extra 3))))

                       ;; Prior statement is inferior to current
                      (t
                       ;(message "number - search for superior")
                       ;; find equal or superior statement
                       (while (and (= 0 (nmcobol-backward-sentence 1))
                                   (setq prior-typ
                                         (nmcobol-beginning-of-sentence))
                                   (numberp prior-typ)
                                   (< typ prior-typ)))
                       ;; That statement's indentation is what I'll use
                       (beginning-of-line) ; can't necessarily assume this.
                       (setq indent (if (looking-at "[ \t]+")
                                        (prog2 (goto-char (match-end 0))
                                            (current-column)
                                          (beginning-of-line))
                                      0)
                             extra (if (looking-at "[ \t]+[0-9]\\{1,2\\}[ \t]+")
                                       (prog2 (goto-char (match-end 0))
                                           (- (current-column) indent)
                                         (beginning-of-line))))
                       (unless (and (numberp prior-typ)
                                    (= prior-typ typ))
                         (setq indent (+ (or extra 3) indent)))))))

                  ;; Beginning of a special but non-numbered sentence.
                  ((stringp typ)
                   ;(message "special statement")
                   (setq indent 1))

                  (t              ;typ is nil - start of regular sentence
                   ;(message "Normal sentence start")
                   (setq indent 4))    ;should already be set to this...?
                )

              ;; Context is amid sentence.  See if it's amid a statement.
              (goto-char bol)
              (if (looking-at nmcobol-keywords-statements-regexp)
                  ;; Lines starting with a statement keyword indent like the
                  ;; last statement unless the last statement begins a block
                  ;; or the current statement ends a block.
                  (progn
                    ;; Get the type of statement being indented.
                    (setq typ (upcase (match-string-no-properties 2)))
                    ;(message "amid - new statement")
                    ;; find start of last statement & get it's indentation
                    (re-search-backward
                     nmcobol-keywords-statements-regexp sos 'move-anyway)
                    (setq prior-typ (upcase (or (match-string-no-properties 2)
                                                "")))
                    (skip-chars-forward " \t")
                    (setq indent (current-column))
                    ;; if prior statement is a block begin, indent from it
                    (if (setq extra (nmcobol-get-block-type bol))
                        (setq indent (+ indent 3)))
                    ;; if current line is a block end, indentation decreases
                    ;; unless it ends a statement that didn't require an end.
                    (when
                        (or
                         (string-match nmcobol-block-end-regexp typ)
                         (string-match "\\(WHEN\\|ELSE\\|THEN\\)" typ))
                      (unless (and
                               (not extra)
                               (string= prior-typ
                                        (upcase
                                         (match-string-no-properties 1 typ))))
                        (setq indent (- indent 3)))))

                ;; line must be continuation of some statement
                ;(message "amid a statement")
                (if (re-search-backward
                     nmcobol-keywords-statements-regexp sos 'move-anyway)
                    (progn (goto-char (match-end 1))
                           (setq prior-typ (match-string-no-properties 2)
                                 indent (current-column)))
                  (skip-chars-forward " \t" (line-end-position))
                  (setq prior-typ ""
                        indent (current-column)))
                (setq indent (+ 6 indent)))
              (setq typ nil))
            ;(message "typ = %s Cur = %s, new = %s" typ cur indent)
            ;; Unless it's a special type (typ) it shouldn't be indenting
            ;; less than 4 so flag an error by removing all indentation
            (if (and (not typ)
                     (< indent 4))
                (setq indent 0))
            (unless (= indent cur) ;current line is already indented correctly
              (goto-char bol)
              (looking-at "[ \t]*")
              (replace-match (make-string indent ? ) nil nil nil 0))
            (goto-char loc)
            (set-marker loc nil)
            (if (and (< 0 col)(> indent (current-column)))
                (move-to-column indent)))))))
  (nmcobol-secondary-indent nmcobol-righthand-keyword-column nil))

(defun nmcobol-secondary-indent (to-col right)
  "Indents `nmcobol-secondary-indent-words' on the current line to TO-COL.
RIGHT indicates if the left or right hand side of the word is indented to
that column.  nil means align the left hand side, any other value
indicates the right side.  If TO-COL is unreasonable (not between 20 and
120) the routine does nothing.  Comment and compiler directive lines are
always ignored."
  (when (and to-col (> to-col 20)(< to-col 120))
    (let ((start (point-marker))
          (end (line-end-position))
          (regexp (concat "\\_<"
                          (regexp-opt nmcobol-secondary-indent-words t)
                          "\\_>"))
          col)
      (beginning-of-line)
      (when (and (not (looking-at nmcobol-non-cobol-regexp))
                 (re-search-forward regexp end t)
                 (not (eq 'font-lock-string-face
                          (get-text-property (point) 'face))))
        (if right
            (progn
              (setq col (- to-col (current-column)))
              (goto-char (match-beginning 1)))
          (goto-char (match-beginning 1))
          (setq col (- to-col (current-column))))
        (unless (looking-back "^\\s-+")
          (cond
           ((< 0 col)
            (insert (make-string col ? )))
           ((> 0 col)
            (while (and (> 0 col)
                        (looking-back "\\s-\\s-"))
              (backward-delete-char-untabify 1)
              (setq col (1+ col)))))))
      (goto-char start))))

(defun nmcobol-setup-indent ()
  "Sets default indentation or sets up nmcobol-indent if available."
;  (if (condition-case ()
;          (progn (require 'nmcobol-indent) t)
;        (error nil))
  (setq indent-line-function 'nmcobol-indent-region
        indent-region-function 'nmcobol-indent-region))

;    (set (make-local-variable 'indent-line-function) 'indent-relative-maybe)))

;;; Language Skeletons -- Feel free to add more of your own!

(defcustom nmcobol-keywords-case 'upper
  "*Indicates if keywords in skeletons should be all UPPER CASE, all lower
case or Camel Case (First Char Upper & Rest Lower)."
  :type  '(choice :tag "Skeleton Case"
                  (const :tag "ALL CAPS"    upper)
                  (const :tag "all small"   lower)
                  (const :tag "Camel Case"  camel)
                  (const :tag "DON'T Change"  ()))
  :group 'nmcobol)

(defun nmcobol-setup-skel ()
  "Configures skeleton.el functions for the NMCOBOL environemnt."
  (set (make-local-variable 'skeleton-transformation) 'nmcobol-skel-transform)
  ;; This prevents abbrevs from expanding within skeletons
  (setq skeleton-further-elements '((abbrev-mode nil))))

(defun nmcobol-skel-transform ( element )
  "Called by `skeleton-insert'.  Gives ELEMENT `nmcobol-keywords-case' capitalization."
  ;; This should be made more complex to only change the case of certain words
  ;; so the user can create skeletons containing items that should not be
  ;; affected by nmcobol-keywords-case.  There are 3 obvious ways.  1) use the
  ;; keywords tables above. 2) add a customize to ignore words. 3) add a
  ;; customize to specify specific words to be affected.  Preferences?
  (if (stringp element)
    (cond
     ((eq nmcobol-keywords-case 'upper) (upcase element))
     ((eq nmcobol-keywords-case 'lower) (downcase element))
     ((eq nmcobol-keywords-case 'camel) (capitalize element))
     ( t                             element            ))
    element))

(defun nmcobol-set-line-syntax ()
  "Applies font-lock-syntactic-keywords to current line.
Used to set properties necessary for proper indentation."
  (if font-lock-mode
      (save-excursion    ; next stmt moves point.
        (font-lock-fontify-syntactic-keywords-region
         (line-beginning-position) (line-end-position))
        ()     ;any result is inserted into buffer
      )))

(define-skeleton nmcobol-if-skel
  "This is an example skeleton."
  nil
  " IF" > " " -                                \n
  _ | \n
  " END-IF"       (nmcobol-set-line-syntax)  > \n)

(define-skeleton nmcobol-paragraph-skel
  "Inserts comment bars to surround a new paragraph name."
  nil \n
  "********************************"        > \n
  " 0." > -1 _ "."                            \n
  "********************************"        > \n \n
  "."                                       > \n \n)


;;; Abbrev support

(defcustom nmcobol-abbrev-mode t
  "Sets the default value for `abbrev-mode' upon entry into `nmcobol-mode'."
  :type 'boolean
  :group 'nmcobol)

(defvar nmcobol-mode-abbrev-table-list
  '(("$i" "" nmcobol-if-skel)
    ("$p" "" nmcobol-paragraph-skel))
  "List of pre-defined `nmcobol-mode' abbrev definitions.
Use \\[list-abbrevs] to see all defined abbrevs.")

(defvar nmcobol-mode-abbrev-table)

(defun nmcobol-setup-abbrevs ()
  "Installs the `nmcobol-mode-abbrev-table' as `local-abbrev-table'"
  (define-abbrev-table
    'nmcobol-mode-abbrev-table
    nmcobol-mode-abbrev-table-list)
  (setq local-abbrev-table nmcobol-mode-abbrev-table)
  (setq skeleton-further-elements '((abbrev-mode nil)))
  (abbrev-mode nmcobol-abbrev-mode)    ;Setting is documented as buffer local
)

;;; Eldoc support

(defcustom nmcobol-eldoc-def-files ()
  "List of files containing function help strings used by `eldoc-mode'.
These are the strings eldoc-mode displays as help for functions near point.
The format of the file must be exactly as follows or who knows what happens.

   (set (intern \"<fcn-name1>\" nmcobol-eldoc-obarray) <helper string1>)
   (set (intern \"<fcn-name2>\" nmcobol-eldoc-obarray) <helper string2>)
...

Where <fcn-name> is the name of the function to which <helper string> applies.
      <helper-string> is the string to display when point is near <fcn-name>.

      Alternatively <helper-string> can be a list where the first element is
      the help string mentioned above and the second element is a string
      containing the filename of the file where <fcn-name> is defined.
      For example '(\"Help string here\" \"source\\of_help.here\")
      When present `nmcobol-eldoc-where-def' and `nmcobol-eldoc-visit-file'
      use it."
  :type '(repeat string)
  :group 'nmcobol)

(defvar nmcobol-eldoc-obarray ()
  "Global Keywords & variables and their associated help strings stored here.")

(defvar nmcobol-eldoc-local-obarray ()
  "Local variables and their associated help strings stored here.")
(make-variable-buffer-local 'nmcobol-eldoc-local-obarray)

(defcustom nmcobol-eldoc-highlight-face 'match
  "Face used to highlight a variable name when displayed by eldoc."
  :type 'face
  :group 'nmcobol)

(defun nmcobol-eldoc-context (&optional type)
  "If TYPE is nil or 'doc return the doc string for the symbol near point
or nil if none.  If TYPE is 'file return the file where the symbol was
defined or nil. If TYPE is 'word return the buffer keyword being looked up."
  (let* ((word (thing-at-point 'symbol))
         name symbol value)
    (when word
      (setq word (upcase word)
            name (concat (save-excursion
                           (beginning-of-line)
                           (char-to-string (char-after))) word)
            symbol (or (intern-soft name nmcobol-eldoc-local-obarray)
                       (intern-soft name nmcobol-eldoc-obarray)))
      (when symbol
        (setq value (symbol-value symbol))
        (cond
         ((eq type 'word) (setq value (downcase word)))
         ((eq type 'file) (setq value (if (listp value)(cadr value))))
         (type (setq value))
         (t (unless (stringp value) (setq value (car value))))))
      value)))

(defun nmcobol-eldoc-function ()
  "Returns a documentation string appropriate for the current context or nil."
  (let* ((value (nmcobol-eldoc-context))
         word)
    (when (and value (setq word (nmcobol-eldoc-context 'word)))
      ;; highlight symbol in the help string
      (if (string-match (concat "\\(?:^\\|\\.\\)" (regexp-opt (list word) t)
                                "\\( \\|\\.\\)") value)
          (put-text-property (match-beginning 1) (match-end 1)
                             'face nmcobol-eldoc-highlight-face
                             value)))
      value))

(defun nmcobol-eldoc-where-def ()
  "Displays the filename from which current eldoc string was extracted."
  (interactive)
  (let* ((fname (nmcobol-eldoc-context 'file)))
    (if fname
        (message "Symbol defined in %s" fname)
      (if (nmcobol-eldoc-context 'word)
          (message "Filename not available")
        (message "No eldoc info for context")))))

(defun nmcobol-eldoc-visit-file ()
  "Visits the file from which current eldoc string was extracted."
  (interactive)
  (let* ((fname (nmcobol-eldoc-context 'file))
         (word  (nmcobol-eldoc-context 'word)))
    (if fname
        (progn (find-file fname)
               (message "Searching for %s" word)
               (goto-char (point-min))
               (re-search-forward (concat " " word "[ ;.(\t\n)]")))
      (if word
          (message "Filename not available")
        (message "No eldoc info for context")))))

(defun nmcobol-setup-eldoc ()
  "Loads the function documentation for use with eldoc."
  (set (make-local-variable 'eldoc-documentation-function)
       'nmcobol-eldoc-function)
  (unless (vectorp nmcobol-eldoc-obarray)
    (setq nmcobol-eldoc-obarray (make-vector 41 0))
    (condition-case var (mapc 'load nmcobol-eldoc-def-files)
      (error (message "*** ERROR *** %s" var))))
  (nmcobol-eldoc-scan-buffer))

(defvar nmcobol-eldoc-definition-regexp
  (concat "^\\(?:"
          "\\?SECTION\\|"
          " +\\([0-9][0-9]\\)\\|"
          " \\{0,3\\}\\(def\\(?:inition\\)?\\|rec\\(?:ord\\)?\\)\\) +\\(\\w+\\)"
          "\\(?: +\\(\\w.+$\\)\\| *\\.\\| *\\,\\)")
  "Expression describing variable and 88 level declarations for eldoc.
Used by `nmcobol-eldoc-make-list' in creating `eldoc' entries.
Match strings are:
1 = numeric-level (if any)
2 = non numeric level (if any)
3 = data name (unless 1 and 2 both nil then this is nil too)
4 = picture, value, occurs etc doc clause (if any, else nil)")

(defun nmcobol-eldoc-next-level ()
  "Returns level number with match-data set for next data or nil."
  (and (re-search-forward nmcobol-eldoc-definition-regexp () t)
       (match-string-no-properties 3)
       (if (match-string-no-properties 1)
           (let ((num (string-to-number (match-string-no-properties 1))))
             (if (or (= 66 num)(= 77 num)) 1 num))
         1)))

(defun nmcobol-eldoc-write-entry (buf file-name context last-level)
  "Creates the eldoc entry in BUF using current match-data.
Returns `nmcobol-eldoc-next-level' data when it returns a level <= LEVEL."
  (let* ((name (upcase (match-string-no-properties 3)))
         (doc (match-string-no-properties 4))
         (next-context (concat context name "."))
         (result (concat context name " " doc))
         (level (nmcobol-eldoc-next-level)))
    (when (and (or doc context)
               (not (string= "FILLER" name)))
      (if (and level (= 88 level)(/= 88 last-level))
          (setq result (concat result " +88")))
      (set (intern (concat " " name) nmcobol-eldoc-local-obarray) result)
      (setq result (prin1-to-string result))
      (if buf
          (with-current-buffer buf
            (if file-name
                (insert (concat "(set (intern \" " name
                                "\" nmcobol-eldoc-obarray) '(" result " \""
                                file-name "\"))\n"))
              (insert (concat "(set (intern \" " name
                              "\" nmcobol-eldoc-obarray) " result ")\n"))))))
    (while (and level (> level last-level))
      (setq level (nmcobol-eldoc-write-entry
                   buf file-name next-context level)))
    level))

(defun nmcobol-eldoc-make-list ()
  "Creates a buffer of eldoc help strings for variables in this buffer.
Run this while visiting a COBOL/NMCOBOL source or DDL source file to
create a buffer of eldoc help entries.  Save this buffer somewhere on
your search path and see `nmcobol-eldoc-def-files'.  See also
`nmcobol-eldoc-scan-buffer'.
The optional arguments are for recursive calls only."
  (interactive)
  (if (not (or (eq 'nmcobol-mode major-mode) (eq 'ddl-mode major-mode)))
      (error "Buffer is not ddl or nmcobol-mode!  Can't extract help text."))
  (let ((new-buf (get-buffer-create "nmcobol-eldoc-list.el"))
        (this-buf (buffer-file-name))
        level)
    (setq nmcobol-eldoc-local-obarray (make-vector 41 0))
    (with-current-buffer new-buf
      (goto-char (point-max))
      (if this-buf (insert (concat ";; from " this-buf "\n"))))
    (save-excursion
      (message "Processing eldoc entries.")
      (goto-char (point-min))
      (setq level (nmcobol-eldoc-next-level))
      (while level
        (setq level (nmcobol-eldoc-write-entry new-buf this-buf () level))))
    (switch-to-buffer new-buf)
    (emacs-lisp-mode)))

(defun nmcobol-eldoc-scan-buffer ()
  "Rescans the current buffer, updating in-memory eldoc entries."
  (interactive)
  (setq nmcobol-eldoc-local-obarray (make-vector 41 0))
  (message "Updating in-memory eldoc entries.")
  (let (level)
    (save-excursion
      (goto-char (point-min))
      (setq level (nmcobol-eldoc-next-level))
      (while level
        (setq level (nmcobol-eldoc-write-entry () () () level)))))
  (message "nmcobol-eldoc-scan-buffer complete"))


;;; (thing-at-point 'filename) support

(defun nmcobol-setup-thing-at-point ()
  "Makes a buffer local version of `thing-at-point-file-name-chars'"
  (defvar thing-at-point-file-name-chars)
  (set (make-local-variable
        'thing-at-point-file-name-chars) "[:alnum:]=\\$.?*#_^-"))

;;; anchored-transpose support
(eval-when-compile (require 'anchored-transpose))
(defun nmcobol-setup-anchored-transpose ()
  "By default a trailing period is not included in text begin swapped."
 (set (make-local-variable 'anchored-transpose-fuzzy-r1beg) "[\t ]+")
 (set (make-local-variable 'anchored-transpose-fuzzy-r1end) "\\s *[.!?]?\\s *")
 (set (make-local-variable 'anchored-transpose-fuzzy-r2beg) "[\t ]+")
 (set (make-local-variable 'anchored-transpose-fuzzy-r2end) "\\s *[.!?]?\\s *"))

;;; Movement by ...

(defcustom nmcobol-sentence-includes-comments t
  "Movement by sentences includes immediately adjacent comments if non-nil"
  :type 'boolean
  :group 'nmcobol)

(defvar nmcobol-sentence-end-token
  "\\.\\s-"
  "Cobol's sentence end token is a period and one whitespace character.")
(defvar nmcobol-not-a-paragraph-regexp
  "\\(e\\(?:nd-if\\|xit\\)\\) *\\."
  "Match stmts confused as paragraph names by `nmcobol-num-or-paragraph-regexp'")
(defvar nmcobol-num-or-paragraph-regexp
  (concat "\\(?:\\([0-8][0-9]\\)[ \n]"
          "\\|\\(\\(?:f\\|s\\)d\\)[ \n]"
          "\\|\\(\\w+\\)"
          "\\(?: +\\(section\\|division\\)[ .\n]"
          "\\| *\\.\\)\\)")
  "Expression to find numeric or paragraph boundaries.  Match strings are:
  1 - numeric data level,
  2 - string FD or SD,
  3 - string paragraph name,
  4 - string SECTION or DIVISION.")
(defun nmcobol-beginning-of-sentence ()
  "Moves to the canonical start of a Cobol sentence.
Either beginning-of-line containing the first keyword or the
beginning of a comment section prior to the keyword depending on
the flag `nmcobol-sentence-includes-comments'
Returns the type of sentence we're at the start of.
If a numbered data definition the numeric level is returned.
If an FD or SD sentence the string FD or SD is returned.
If a section or division statement the corresponding string is returned.
If the statement is a paragraph name the name is returned.
Otherwise nil is returned."
  (save-match-data
    ;; Make room for backward sentence to see the period
    (if (looking-back "\\.") (forward-char))
    (nmcobol-backward-sentence 1 t)
    ;; Make room for next search to see a potential leading space char.
    (if (and (looking-at "\\S-")(not (bobp)))(backward-char))
    ;; I should now be at the end of the prior sentence.
    (let ((looking t))
      ;; look for a word character
      (while
          (and looking
               (re-search-forward " \\(\\w\\|\\s(\\|\\s)\\)" () 'move-anyway))
        (backward-char)
        (if (save-excursion (beginning-of-line)
                            (not (looking-at nmcobol-non-cobol-regexp)))
            (setq looking nil)
          (beginning-of-line)
          (while (looking-at nmcobol-non-cobol-regexp)
            (forward-line 1))))
      (if (looking-back "^ +" (line-beginning-position))
          (let ((type (if (looking-at nmcobol-num-or-paragraph-regexp)
                          (or (match-string-no-properties 2)
                              (match-string-no-properties 4)
                              (match-string-no-properties 3)
                              (string-to-number
                               (match-string-no-properties 1))))))
            (if (looking-at nmcobol-not-a-paragraph-regexp)
                (setq type nil))
            (beginning-of-line)
            (when (and nmcobol-sentence-includes-comments (not (bobp)))
              (while (and (= 0 (forward-line -1))
                          (looking-at nmcobol-non-cobol-regexp)))
              (forward-line))
            type)))))

(defun nmcobol-forward-sentence (count)
  "Move forward past COUNT Cobol sentence end tokens.
COUNT must be a positive number.  Called by advice on forward-sentence.
See also `nmcobol-sentence-end-token'.  Returns 0 if successful, N if
movement stopped prior to COUNT sentence end tokens were found."
  (save-match-data
    (while (and (< 0 count)
                (re-search-forward nmcobol-sentence-end-token () 'move-anyway))
      (backward-char)
      (if (save-excursion (beginning-of-line)
                          (looking-at nmcobol-non-cobol-regexp))
          (while (progn
                   (forward-line 1)
                   (looking-at nmcobol-non-cobol-regexp)))
        (setq count (1- count)))
      (forward-char)))
  count)

(defun nmcobol-backward-sentence (count &optional recursive)
  "Move backward over COUNT Cobol periods then forward outside sentence.
COUNT must be a positive number.  See `nmcobol-sentence-end-token' for
the definition of where outside a sentence is.  RECURSIVE is used
internally to see if point is within or between sentences. Called by
advice on `forward-sentence'."
  (save-match-data
    (unless recursive
      (let ((prev (point)))
        (nmcobol-beginning-of-sentence)
        (if (< (point) prev)
            (if (= 0 count)(set-match-data (list (point)(point))))
          (setq count (1+ count)))))
    (while (and (< 0 count)
                (re-search-backward nmcobol-sentence-end-token () 'move-anyway))
      (if (save-excursion (beginning-of-line)
                          (not (looking-at nmcobol-non-cobol-regexp)))
          (setq count (1- count))
        (beginning-of-line)
        (while (looking-at nmcobol-non-cobol-regexp)
          (forward-line -1))
        (forward-line)))
    (if (= 0 count)
        (goto-char (match-end 0))
      (setq count (1- count))))
  count)

(defadvice forward-sentence
  (around nmcobol-ad-forward-sentence activate)
  "In nmcobol-mode, moves by Cobol sentences."
  (if (eq major-mode 'nmcobol-mode)
      (if (and (not (eq t nmcobol-sentence-includes-comments))
               (save-excursion (beginning-of-line)
                               (save-match-data
                                 (looking-at nmcobol-non-cobol-regexp))))
          ad-do-it
        (if (or (unless (ad-get-arg 0) (ad-set-arg 0 1) nil)
                (= 0 (setq ad-return-value (ad-get-arg 0)))
                (= 0 (setq ad-return-value
                           (cond
                            ((< 0 (ad-get-arg 0))
                             (nmcobol-forward-sentence (ad-get-arg 0)))
                            ((> 0 (ad-get-arg 0))
                             (nmcobol-backward-sentence (- (ad-get-arg 0))))))))
            (if (= 0 ad-return-value)
                (nmcobol-beginning-of-sentence))))
    ad-do-it))

(defun nmcobol-forward-paragraph (count)
  "Moves forward by data levels or paragraphs; COUNT times.
Returns 0 if successful or COUNT minus the number of paragraphs moved."
  (let ((context (nmcobol-beginning-of-sentence)))
    (while (and (< 0 count)
                (not (eobp))
                (setq count (1- count)
                      context (nmcobol-next-paragraph context)))))
  count)

(defun nmcobol-backward-paragraph (count)
  "Moves backward by data levels or paragraphs; COUNT times.
Returns 0 if successful or COUNT minus the number of paragraphs moved."
  (let ((prev (point))
        (context (nmcobol-beginning-of-sentence)))
    (if (and (< (point) prev)
             context)
        (if (< 0 count) (setq count (1- count))))
    (while (and (< 0 count)
                (not (bobp))
                (setq count (1- count)
                      context (nmcobol-prior-paragraph context)))))
  count)

(defun nmcobol-next-paragraph (context)
  "Moves forward to the next data level or paragraph marker based on CONTEXT.
Returns the new context where nil means `eobp' reached before next paragraph.
See `nmcobol-beginning-of-sentence' for possible context values."
  (if (numberp context)
      ;; Numbered paragraphs are handled based on data level
      (let ((next-context (and (= 0 (nmcobol-forward-sentence 1))
                               (nmcobol-beginning-of-sentence))))
        (if (numberp next-context)
            (cond
             ;; anything besides another 77 terminates a 77
             ((= context 77)
              (while (and (numberp next-context)
                          (= next-context 77))
                (setq next-context
                      (and (= 0 (nmcobol-forward-sentence 1))
                           (nmcobol-beginning-of-sentence)))))
             ;; when next number is same as current look for a lower numbered
             ;; level, a 77, or unnumbered, to terminate 'paragraph'.
             ((= next-context context)
              (while (and
                      (numberp (setq next-context
                                     (and (= 0 (nmcobol-forward-sentence 1))
                                          (nmcobol-beginning-of-sentence))))
                      (>= next-context context)
                      (/= next-context 77)
                      (/= next-context 66))))
             ;; When next number is higher than current, look for same or lower
             ;; numbered level, a 77, or unnumbered, to terminate 'paragraph'.
             ((and (> next-context context)
                   (/= next-context 77)
                   (/= next-context 66))
              (while (and
                      (numberp (setq next-context
                                     (and (= 0 (nmcobol-forward-sentence 1))
                                          (nmcobol-beginning-of-sentence))))
                      (> next-context context)
                      (/= next-context 77)
                      (/= next-context 66))))
             ;; If next-context = 77 or is < context then we're already done.
            )
          ;; if next-context isn't numbered then we're already done
        )
        next-context)
    ;; Normal paragraph movement
    (while (and (= 0 (nmcobol-forward-sentence 1))
                (not (setq context (nmcobol-beginning-of-sentence)))))
    context))

(defun nmcobol-prior-paragraph (context)
  "Moves backward to the next data level or paragraph marker based on CONTEXT.
Returns the new context where nil means `bobp' reached before a paragraph start.
See `nmcobol-beginning-of-sentence' for possible context values."
  (if (numberp context)
      ;; Numbered paragraphs are handled based on data level
      (let ((next-context (and (= 0 (nmcobol-backward-sentence 1))
                               (nmcobol-beginning-of-sentence))))
        (if (numberp next-context)
            (cond
             ;; anything besides another 77 terminates a 77
             ((= context 77)
              (while (and (numberp next-context)
                          (= next-context 77))
                (setq next-context
                      (and (= 0 (nmcobol-backward-sentence 1))
                           (nmcobol-beginning-of-sentence)))))
             ;; when next number is same as current look for a lower numbered
             ;; level, a 77, or unnumbered, to terminate 'paragraph'.
             ((= next-context context)
              (while (and
                      (numberp (setq next-context
                                     (and (= 0 (nmcobol-backward-sentence 1))
                                          (nmcobol-beginning-of-sentence))))
                      (>= next-context context)
                      (/= next-context 77)
                      (/= next-context 66))))
             ;; When next number is higher than current, look for same or lower
             ;; numbered level, a 77, or unnumbered, to terminate 'paragraph'.
             ((and (> next-context context)
                   (/= next-context 77)
                   (/= next-context 66))
              (while (and
                      (numberp (setq next-context
                                     (and (= 0 (nmcobol-backward-sentence 1))
                                          (nmcobol-beginning-of-sentence))))
                      (> next-context context)
                      (/= next-context 77)
                      (/= next-context 66))))
             ;; If next-context = 77 or is < context then we're already done.
            )
          ;; if next-context isn't numbered then we're already done
        )
        next-context)
    ;; Normal paragraph movement
    (while (and (= 0 (nmcobol-backward-sentence 1))
                (not (stringp (setq context (nmcobol-beginning-of-sentence)))))
      (setq context nil))
    context))

(defadvice forward-paragraph
  (around nmcobol-ad-forward-paragraph activate)
  "In nmcobol-mode, moves by numbered data levels or Cobol paragraphs.
Uses `nmcobol-beginning-of-sentence' in determining how to move."
  (if (eq major-mode 'nmcobol-mode)
      (if (and (not (eq t nmcobol-sentence-includes-comments))
               (save-excursion (beginning-of-line)
                               (looking-at nmcobol-non-cobol-regexp)))
          ad-do-it
        (setq ad-return-value
              (if (or (unless (ad-get-arg 0) (ad-set-arg 0 1) nil)
                      (< 0 (ad-get-arg 0)))
                  (nmcobol-forward-paragraph (ad-get-arg 0))
              (nmcobol-backward-paragraph (- (ad-get-arg 0))))))
    ad-do-it))

;; User programming utility functions

(defcustom nmcobol-pic-string-regexp
  (concat "pic\\(?:ture\\)?\\s-+\\([-abnpsvxz0-9.,()/+crd*$]*"
          "[-abnpvxz0-9()/+crd*$]+\\)\\(\\.?[ \t]\\|\\.?$\\)")
  "Used by `nmcobol-addup-pics' to find picture strings."
  :type  'regexp
  :group 'nmcobol)

(defun nmcobol-pic-string-length(string)
  "Used internally.  Returns the length a picture string represents."
  (let ((len (length string))
        (off 0))
        (while (string-match "[abnpxz90*$](\\([0-9]+\\))" string off)
          (setq len (+ (string-to-number (match-string-no-properties 1 string))
                       (- len (length (match-string-no-properties 0 string))))
                off (match-end 0)))
        len))

(defun nmcobol-addup-pics (begin end)
  "Adds the length represented by all picture strings in the region.
Eventually I'd like for it to account for REDEFINES.  It DOESN'T now."
  (interactive "r")
  (if mark-active
      (save-excursion
        (goto-char begin)
        (let ((sum 0))
          (while (re-search-forward nmcobol-pic-string-regexp end t)
            (setq sum (+ sum (nmcobol-pic-string-length
                              (match-string-no-properties 1)))))
          (message "PIC[TURE] clauses add up to %d" sum)))
    (message "Select a region to operate upon.")))

;;; Miscellaneous

(defun nmcobol-setup-paren-mode ()
  "Adds functions to make show-paren-mode recognize END-<statement> verbs."
  (require 'paren)
;  (if (boundp show-paren-decide-dir-function)
;      (set (make-local-variable 'show-paren-decide-dir-function)
;           'nmcobol-show-paren-decide-dir))
;  (if (boundp show-paren-scan-function)
;      (set (make-local-variable 'show-paren-scan-function)
;           'nmcobol-show-paren-scan))
  (show-paren-mode 1))

(defun nmcobol-customize-options ()
  "Invokes (customize-group 'nmcobol)"
  (interactive)
  (customize-group 'nmcobol))

(defcustom nmcobol-mode-hook nil
  "Standard mode hook. Run after entering NMCobol mode."
  :type 'hook
  :group 'nmcobol)

;;;###autoload
(defun nmcobol-mode ()
  "A major mode for editing ?TANDEM format COBOL program source files.
See `cobol-mode' for ?ANSI format COBOL program source files.

Customization options are available via
\\[customize-group] <ret> NMCOBOL <ret>

This mode provides NMCOBOL specific support for such packages as:
    `abbrev-mode'          `adaptive-fill-mode'   `anchored-transpose'
    `auto-fill-mode'       `eldoc-mode'           `filladapt-mode'
    `font-lock-mode'       `imenu'                `indent-line-function'
    `show-paren-mode'      `skeleton-insert'      `thing-at-point'
    `which-function'

 (thing-at-point 'filename) will return a Guardian filename, define or
 template if present at point.

** Note ** Some things won't work correctly if `font-lock-mode' is off.

nmcobol-mode implements the following \\[execute-extended-command] ... commands

`nmcobol-mode'             Activates this mode for the current buffer
`nmcobol-if-skel'          Inserts an if/then statement skeleton

\\{nmcobol-mode-map}
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
  (set (make-local-variable 'major-mode) 'nmcobol-mode)
  (set (make-local-variable 'mode-name) "NMCOBOL")
  (set (make-local-variable 'make-backup-files) nil) ;necessary for now
  (set-syntax-table nmcobol-mode-syntax-table)
  (nmcobol-setup-regexp-vars)
  (use-local-map nmcobol-mode-map)
  (nmcobol-setup-font-lock)
  (nmcobol-setup-adaptive-fill)
  (nmcobol-setup-abbrevs)
  (nmcobol-setup-imenu)
  (nmcobol-setup-eldoc)
  (nmcobol-setup-indent)
  (nmcobol-setup-skel)
  (nmcobol-setup-column-markers)
  (nmcobol-setup-thing-at-point)
  (nmcobol-setup-anchored-transpose)
  (nmcobol-setup-paren-mode)
  (nmcobol-setup-menu)
  (run-hooks 'nmcobol-mode-hook))

(provide 'nmcobol-mode)

;;; nmcobol-mode.el ends here

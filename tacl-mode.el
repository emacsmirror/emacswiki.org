;;; tacl-mode.el --- Handles the Tandem/NSK/Guardian TACL language.
;;                   A proprietary language of Tandem/Compaq/HP computers.

;; Copyright (C) 2001, 2004 Free Software Foundation, Inc.

;; Author: Rick Bielawski <rbielaws@i1.net>
;; Keywords: languages, extensions, tandem, Guardian, nsk, tacl
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

;; TACL stands for Tandem Advanced Command Language.

;; Keywords as of G06.10 are recognized. There are 3 types of keywords.
;;   #builtin functions, #builtin variables, option keywords.
;;   Option keywords are those that are only keywords when found between
;;   slash marks or vertical bars.
;;
;; All keywords have abbrev table entries.  Use M-x list-abbrevs for a listing.
;;   Abbrevs are generated programatically from tacl-keywords-... to be the
;;   minimum number of characters to uniquely identify the keyword.
;;
;; Bracket handling is managed by emacs show-paren-mode. I just turn it on.
;;
;; imode recognizes both ?section and [#def ...] style definitions but only
;;   within the first 3 characters of a line.  This is intended to prevent
;;   large numbers of sub-definitions from making imenu huge/useless.  You can
;;   tweek this by altering tacl-imenu-expression-alist thru customize.
;;
;; eldoc is supported although there is no automated way of generating the
;; actual help files. I'm sure it can be usefull all the same.  Search for
;; tacl-syntax.el for a sizable number of help entries to get you started.
;; Customize tacl-eldoc-def-files to include this file.
;; Once you have your help entries defined see help for eldoc-mode for
;; turing on the mode.  Putting (eldoc-mode t) in .emacs might do it.
;;

;;; ToDo:

;; There is no adaptive-fill support yet.
;; Custom indentation support has not been created yet.

;;; Installing:

;; Before you can use tacl-mode, emacs needs to be able to find it.  Place the
;; tacl-mode.el file in a directory on the load-path; typically the
;; .../site-lisp or perhaps .../lisp/progmods directory.  Usually you would
;; also want to byte compile tacl-mode.el but this is not required.  To do
;; this, visit the tacl-mode.el file, type: M-x emacs-lisp-byte-compile <ret>
;; There should be no warnings or errors during byte compilation.
;;
;; There are 4 basic ways to use TACL-MODE on a file.  The first method
;; manually selects tacl-mode as the editing mode.  The other 3 cause emacs to
;; recognize automatically that you want to visit the file using tacl-mode.
;;
;; Pick one:
;; 1. While visiting a file, type: M-x tacl-mode
;; 2. Put the string -*-tacl-*- in a comment on the first line of the file.
;;    Save the file and close it.  Now any time you open it tacl-mode starts.
;; 3. Create an association between a particular file naming convention and
;;    tacl-mode.  This is done by adding an association to auto-mode-alist.
;; For example:
;; (setq auto-mode-alist
;;   (append
;;     '(("\\.tacl\\'" . tacl-mode)         ;extension of .tacl means tacl-mode
;;       ("\\([\\/]\\|^\\)[^.]+$" . tacl-mode)) ;so does no extension at all.
;;    auto-mode-alist))
;; 4. Advise set-auto-mode to look at the buffer contents upon loading.
;;
;; The above all tell emacs that you want to use tacl-mode but you must load
;; tacl-mode before you can use it.  There are 2 methods of telling emacs to
;; load the tacl-mode definitions.  The first unconditionally loads tacl-mode
;; definitions immediately.  The second tells emacs to automatically load
;; tacl-mode only when you try to use it.  Add one of the following lines to
;; your .emacs file.
;;
;;(require 'tacl-mode)      ; Unconditional load
;;(autoload 'tacl-mode "tacl-mode" "Major mode for Tandem TACL files." t nil)
;;
;; Please report any bugs!

;;; History:

;; 2004-05-26 RGB Mode is finally useable enough to start tracking.
;; 2004-06-17 RGB Mostly lots of comments and documentation updates were made
;;                Customize options were added for tacl-font-lock-always and
;;                tacl-primecode-warning.
;; 2005-02-20 RGB Some eldoc support added. Column markers are now dynamically
;;                configurable.  Added a column ruler somewhat similar to
;;                Tedit's ruler.
;; 2005-03-22 RGB Fix bug in tacl-keyword-vartype-regexp.  Add more support
;;                for eldoc lookup.
;; 2005-04-06 RGB Changed == comment from syntax table to syntactic-keyword
;;                for (hopefully) easier comment detection.
;; 2005-05-23 RGB CVS changed name of eldoc-print-current-symbol-info-function
;; 2005-06-24 RGB Reworked column marker code. Fixed comment char bug.
;; 2005-06-29 RGB Fixed ?tacl <word> not highlighting correctly.  Added more
;;                skeletons.  Replaced column-ruler with new popup-ruler.
;;                Moved skeletons to their own keymap.
;; 2005-08-03 RGB Fixed comment-start-skip. Added arg & def skeletons.
;; 2005-10-19 RGB Removed column-marker & popup-ruler stuff.
;;                These are now standalone packages here:
;;                http://www.emacswiki.org/cgi-bin/emacs/column-marker.el
;;                http://www.emacswiki.org/cgi-bin/emacs/popup-ruler.el
;;                Added automatic detection and support for above packages.
;; 2006-08-28 RGB A quote or brace not terminated on the same line is no longer
;;                highlighted as a string or comment.  Some aesthetic stuff.
;; 2006-10-20 RGB Added tacl-setup-menu.  Fixed imenu customization.

;;; Code:

(defgroup tacl nil
  "Major mode for editing Tandem TACL source files in Emacs.
While in tacl-mode use C-h m for a description of the mode's features."
  :prefix 'tacl-
  :group 'languages)

;;; KEY MAPS

(defvar tacl-skeleton-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?{]   'tacl-frame-skel)
    (define-key map [?a]   'tacl-argument-skel)
    (define-key map [?c]   'tacl-case-skel)
    (define-key map [?d]   'tacl-def-skel)
    (define-key map [?e]   'tacl-if-else-skel)
    (define-key map [?i]   'tacl-if-skel)
    (define-key map [?l]   'tacl-loop-while-skel)
    (define-key map [?u]   'tacl-loop-until-skel)
    map)
  "Keymap for `tacl-mode'.")

(defvar tacl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab]           'indent-according-to-mode)
    (define-key map [?\C-c ?\C-c]   'column-marker-here)
    (define-key map [?\C-c ?\C-f]   'auto-fill-mode)
    (define-key map [?\C-c ?\C-r]   'popup-ruler)
    (define-key map [?\C-c ?\C-s]    tacl-skeleton-map)
    (define-key map [?\C-c return]  'comment-indent-new-line)
    map)
  "Keymap for `tacl-mode'.")

(defun tacl-setup-menu ()
  "Adds a menu of TACL specific functions to the menu bar."
  (define-key (current-local-map) [menu-bar tacl-menu]
    (cons "TACL" (make-sparse-keymap "TACL")))
  (define-key (current-local-map) [menu-bar tacl-menu column]
    '(menu-item "Column Marker" column-marker-here
                :key-sequence [?\C-c ?\C-c]
                :help "Puts column marker at current column"))
  (define-key (current-local-map) [menu-bar tacl-menu ruler]
    '(menu-item "Ruler" popup-ruler
                :key-sequence [?\C-c ?\C-r]
                :help "Inserts temporary ruler"))
  (define-key (current-local-map) [menu-bar tacl-menu comment-eol]
    '(menu-item "Comment EOL" comment-indent-new-line
                :key-sequence [?\C-c return]
                :help "Continues comment on new line"))
  (define-key (current-local-map) [menu-bar tacl-menu skeletons]
    (cons "Skeletons" (make-sparse-keymap "Skeletons")))
  (define-key (current-local-map) [menu-bar tacl-menu skeletons until]
    '(menu-item "Loop Until" tacl-loop-until-skel
                :key-sequence [?\C-c ?\C-s ?u]
                :help "Inserts a #loop until statement"))
  (define-key (current-local-map) [menu-bar tacl-menu skeletons while]
    '(menu-item "Loop While" tacl-loop-while-skel
                :key-sequence [?\C-c ?\C-s ?l]
                :help "Inserts a #loop while statement"))
  (define-key (current-local-map) [menu-bar tacl-menu skeletons else]
    '(menu-item "If Else" tacl-if-else-skel
                :key-sequence [?\C-c ?\C-s ?e]
                :help "Inserts an if/then/else statement"))
  (define-key (current-local-map) [menu-bar tacl-menu skeletons if]
    '(menu-item "If Then" tacl-if-skel
                :key-sequence [?\C-c ?\C-s ?i]
                :help "Inserts an if/then statement"))
  (define-key (current-local-map) [menu-bar tacl-menu skeletons frame]
    '(menu-item "Frame" tacl-frame-skel
                :key-sequence [?\C-c ?\C-s ?{]
                :help "Insert a #FRAME/#UNFRAME pair"))
  (define-key (current-local-map) [menu-bar tacl-menu skeletons case]
    '(menu-item "Case" tacl-case-skel
                :key-sequence [?\C-c ?\C-s ?c]
                :help "Insert a TACL #CASE statement"))
)

(defvar tacl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "." st)    ;tacl-find-syntactic-keywords handles "
    (modify-syntax-entry ?\# "w" st)
    (modify-syntax-entry ?\_ "w" st)
    (modify-syntax-entry ?^  "w" st)
    (modify-syntax-entry ?\. "_" st)
    (modify-syntax-entry ?\: "_" st)
    (modify-syntax-entry ?\? "_" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\{ ". b" st)  ;tacl-find-syntactic-keywords handles {
    (modify-syntax-entry ?\} ". b" st)  ;tacl-find-syntactic-keywords handles }
    (modify-syntax-entry ?\= ". 12" st) ;tacl-find-syntactic-keywords handles ==
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\~ "/" st)
    (modify-syntax-entry ?\| "." st)
    (modify-syntax-entry ?\/ "." st)
    (modify-syntax-entry ?\% "." st)
    (modify-syntax-entry ?\$ "w" st)
    (modify-syntax-entry ?\' "_" st)
    (modify-syntax-entry ?\@ "_" st)
    (modify-syntax-entry ?\\ "_" st)
    st)
  "Syntax table for `tacl-mode'.")

; All keyword lists get sorted, so you can add new words to the end.
; NOTE: Words containing CAPITAL LETTERS will never autoexpand.
; Use M-x list-abbrevs to see the shortcuts for each word.

(defcustom tacl-user-keywords '("_debugger" "_execute")
  "Words in this list are highlighted using font-lock-type-face.
   Additionally, the words are merged into the tacl-mode-abbrev-table where
   abbreviations are generated for them.  Note that abbrev, for some reason,
   refuses to auto-expand words containing capital letters."
  :type '(repeat (string :tag "Keyword"))
  :group 'tacl)

(defvar tacl-keywords-var-types
  '( "ALIAS" "DELTA" "DIRECTORY" "MACRO" "ROUTINE" "STRUCT" "TEXT")
  "List of TACL variable types.
   Used to create the font-lock-keywords table & abbrev-table.")

(defvar tacl-keywords-?tacl-types
  '( "MACRO" "ROUTINE" "TEXT")
  "List of TACL variable types allowed after ?TACL.
   Used to create the font-lock-keywords table & abbrev-table.")

(defvar tacl-keywords-directives
  '( "BLANK" "FORMAT" "SECTION" "TACL")
  "List of TACL #informat interpreter directives.
   Used to create the font-lock-keywords table & abbrev-table.")

(defvar tacl-keywords-builtin-vars
  '( "#assign"               "#breakmode"             "#characterrules"
     "#defaults"             "#definemode"            "#errornumbers"
     "#exit"                 "#helpkey"               "#highpin"
     "#home"                 "#in"                    "#informat"
     "#inlineecho"           "#inlineout"             "#inlineprefix"
     "#inlineprocess"        "#inlineto"              "#inputeof"
     "#inspect"              "#myterm"                "#out"
     "#outformat"            "#param"                 "#pmsearchlist"
     "#pmsg"                 "#prefix"                "#processfilesecurity"
     "#prompt"               "#replyprefix"           "#routepmsg"
     "#shiftdefault"         "#taclsecurity"          "#trace"
     "#uselist"              "#wakeup"                "#width")
  "List of TACL Builtin variables.
   Used to create the font-lock-keywords table & abbrev-table.")

(defvar tacl-keywords-builtin-fcns
  '( "#abend"                "#aborttransaction"      "#activateprocess"
     "#adddsttransition"     "#alterpriority"         "#append"
     "#appendv"              "#argument"              "#backupcpu"
     "#begintransaction"     "#breakpoint"            "#builtins"
     "#case"                 "#changeuser"            "#charaddr"
     "#charbreak"            "#charcount"             "#chardel"
     "#charfind"             "#charfindr"             "#charfindrv"
     "#charfindv"            "#charget"               "#chargetv"
     "#charins"              "#charinsv"              "#coldloadtacl"
     "#comparev"             "#compute"               "#computejuliandayno"
     "#computetimestamp"     "#computetransid"        "#contime"
     "#convertphandle"       "#convertprocesstime"    "#converttimestamp"
     "#createfile"           "#createprocessname"     "#createremotename"
     "#debugprocess"         "#def"                   "#defineadd"
     "#definedelete"         "#definedeleteall"       "#defineinfo"
     "#definenames"          "#definenextname"        "#definereadattr"
     "#definerestore"        "#definerestorework"     "#definesave"
     "#definesavework"       "#definesetattr"         "#definesetlike"
     "#definevalidatework"   "#delay"                 "#delta"
     "#deviceinfo"           "#empty"                 "#emptyv"
     "#emsaddsubject"        "#emsaddsubjectv"        "#emsget"
     "#emsgetv"              "#emsinit"               "#emsinitv"
     "#emstext"              "#emstextv"              "#endoftacllocl"
     "#endtransaction"       "#eof"                   "#errortext"
     "#exception"            "#extract"               "#extractv"
     "#filegetlockinfo"      "#fileinfo"              "#filenames"
     "#filter"               "#frame"                 "#getconfiguration"
     "#getprocessstate"      "#getscan"               "#history"
     "#if"                   "#initterm"              "#inlineeof"
     "#input"                "#inputv"                "#interactive"
     "#interpretjuliandayno" "#interprettimestamp"    "#interprettransid"
     "#juliantimestamp"      "#keep"                  "#keys"
     "#lineaddr"             "#linebreak"             "#linecount"
     "#linedel"              "#linefind"              "#linefindr"
     "#linefindrv"           "#linefindv"             "#lineget"
     "#linegetv"             "#lineins"               "#lineinsv"
     "#linejoin"             "#load"                  "#lockinfo"
     "#logoff"               "#lookupprocess"         "#loop"
     "#match"                "#mom"                   "#more"
     "#mygmom"               "#mypid"                 "#mysystem"
     "#newprocess"           "#nextfilename"          "#openinfo"
     "#output"               "#outputv"               "#pause"
     "#pop"                  "#process"               "#processexists"
     "#processinfo"          "#processlaunch"         "#processorstatus"
     "#processortype"        "#purge"                 "#push"
     "#raise"                "#rename"                "#reply"
     "#replyv"               "#requester"             "#reset"
     "#rest"                 "#result"                "#return"
     "#routinename"          "#segment"               "#segmentconvert"
     "#segmentinfo"          "#segmentversion"        "#server"
     "#set"                  "#setbytes"              "#setconfiguration"
     "#setmany"              "#setprocessstate"       "#setscan"
     "#setsystemclock"       "#setv"                  "#shiftstring"
     "#sort"                 "#spiformatclose"        "#ssget"
     "#ssgetv"               "#ssinit"                "#ssmove"
     "#ssnull"               "#ssput"                 "#ssputv"
     "#stop"                 "#suspendprocess"        "#switch"
     "#system"               "#systemname"            "#systemnumber"
     "#tacloperation"        "#taclversion"           "#timestamp"
     "#tosversion"           "#unframe"               "#userid"
     "#username"             "#variableinfo"          "#variables"
     "#variablesv"           "#wait"                  "#xfileinfo"
     "#xfilenames"           "#xfiles"                "#xlogon"
     "#xppd"                 "#xstatus")
  "List of TACL Builtin functions.
   Used to create the font-lock-keywords table & abbrev-table.")

(defvar tacl-keywords-argument-types
  '( "ASSIGN"           "ATTRIBUTENAME"   "ATTRIBUTEVALUE"  "CHARACTERS"
     "CLOSEPAREN"       "COMMA"           "DEFINENAME"      "DEFINETEMPLATE"
     "DEVICE"           "END"             "FILENAME"        "GMOMJOBID"
     "JOBID"            "KEYWORD"         "NUMBER"          "OPENPAREN"
     "OTHERWISE"        "PARAM"           "PARAMVALUE"      "PROCESSID"
     "PROCESSNAME"      "SECURITY"        "SEMICOLON"       "SLASH"
     "STRING"           "SUBSYSTEM"       "SUBVOL"          "SUBVOLTEMPLATE"
     "TEMPLATE"         "TEXT"            "TOKEN"           "TRANSID"
     "USER"             "VARIABLE"        "WORD")
  "List of TACL #argument 'type' keywords.
   Used to create the font-lock-keywords table & abbrev-table.")

(defvar tacl-keywords-option-verbs
  '( "PEEK"         "TEXT"         "VALUE"        "START"        "WIDTH"
     "SEARCHLIST"   "SYNTAX"       "WORDLIST"     "MINIMUM"      "MAXIMUM"
     "TOKEN"        "ALLOW"        "FORBID"       "QUALIFIED"    "UNQUALIFIED"
     "SPACE"        "NEWSUBVOL")
  "List of TACL verbs allowed within /slash/ marks of builtins.
   Used to create the font-lock-keywords table & abbrev-table.
   This doesn't include #fileinfo/<keywords>/ at the moment.")

(defvar tacl-keywords-enclosed
  '( "THEN"         "ELSE"         "DO"           "UNTIL"        "OTHERWISE"
     "BODY"         "WHILE")
  "List of TACL verbs used within |v-bars| marks of select builtins.
   Used to create the font-lock-keywords table & abbrev-table.")

;;; Abbrev stuff

(defcustom tacl-abbrev-mode t
  "Sets the default value for abbrev-mode in TACL mode.
   Note that this does not turn abbrev-mode on or off it simply determines
   the state of the `abbrev-mode' variable when TACL mode is entered."
  :type 'boolean
  :group 'tacl)

(defun tacl-make-abbrev-table-list (&rest wordlists)
  "This function converts wordlist(s) to a list of abbrev table entries.
   All wordlists passed to this function are concatinated and sorted.
   This function returns a list of the form ((abbrev expansion)...) .
   The abbrev portion is the minimum number of characters necessary
   to identify the word uniquely among all words in the list.
   Duplicate words effectively squelch AUTO abbrev of a word"
  (let ((mixed-words)
        (all-words)
        (result)
        (prev-match 0))
    (while wordlists
      (setq all-words (append all-words (car wordlists) nil))
      (setq wordlists (cdr wordlists)))
    (setq all-words (sort all-words 'string<))
    (while all-words
      (let*((this (car all-words))
	    (this-len (length this))
	    (next (car (cdr all-words)))
	    (next-len (length next))
	    (len (if (< this-len next-len) this-len next-len ))
	    (cntr 0)
	    (this-match)) ;let variable definitions
	(while (and (< cntr len)     ;Find 1st non-matching char
		    (= (aref this cntr) (aref next cntr)))                     ;Isn't there a primitive for this?
	  (setq cntr (1+ cntr))) ; while this = next
	(setq this-match (if (> cntr prev-match) cntr prev-match))
	(setq prev-match cntr)
	(if (< this-match this-len)(setq this-match (1+ this-match)))
	(setq result (append result (list (list
			   (substring this 0 this-match) this))))
	(setq all-words (cdr all-words))) ;while's let
    ) ;while all-words
    result) ; let
)

(defvar tacl-mode-abbrev-table-list
  (tacl-make-abbrev-table-list
;   tacl-keywords-builtin-fcns
;   tacl-keywords-builtin-vars
;   tacl-keywords-directives
;   tacl-keywords-option-verbs
;   tacl-keywords-argument-types
;   tacl-keywords-var-types
;   tacl-user-keywords
;;;   tacl-keywords-squelch-abbrev
  )
  "Abbreviations for many common TACL Builtin commands")

(defvar tacl-mode-abbrev-table)

(defun tacl-setup-abbrevs ()
  "Installs the tacl-mode-abbrev-table as local-abbrev-table"
  (define-abbrev-table 'tacl-mode-abbrev-table tacl-mode-abbrev-table-list)
  (setq local-abbrev-table tacl-mode-abbrev-table)
  (setq abbrev-mode tacl-abbrev-mode))

;;; Font lock (highlighting)

(defcustom tacl-font-lock-always t
  "If true, TACL-MODE will always turn `font-lock-mode' on even if
`global-font-lock-mode' is off.  nil disables this feature."
  :type 'boolean
  :group 'tacl)

(defcustom tacl-primecode-warning t
  "When not nil, instances of ]a ]d and ]e appearing in column 1-2 are
highlighted with a warning face.  This alerts you that submission of this file
to RMS/PrimeCode will fail due to invalid contents."
  :type 'boolean
  :group 'tacl)

(defun tacl-keyword-anywhere-regexp ( word-list )
  "Returns a regexp that finds the words passed.
   But only if the keyword is surrounded by non-word chars."
  (concat "\\<"(regexp-opt word-list t)"\\>"))

(defun tacl-keyword-directives-regexp ( word-list )
  "Returns a regexp that finds ?directives."
  (concat "^\\?"(regexp-opt word-list t)"\\>"))

(defun tacl-keyword-between-bars-regexp ( word-list )
  "Returns a regexp that finds the words passed alone between | |."
  (concat "|\\s-*"(regexp-opt word-list t)"\\s-*|"))

(defun tacl-keyword-vartype-regexp ( word-list )
  "Returns a regexp that finds the words after ?section or #def syntax."
  (concat "\\(?:#def\\|?section\\) +\\S-+ +"(regexp-opt word-list t)"\\b"))

(defun tacl-keyword-?tacl-regexp ( word-list )
  "Returns a regexp that finds the words after ?tacl syntax."
  (concat "^?tacl +"(regexp-opt word-list t)" *$"))

(defun tacl-function-name-regexp ( word-list )
  "Returns a regexp to highlight the name of variables being defined."
  (concat "\\(?:#def\\|?section\\) +\\(\\S-+\\) +"
          (regexp-opt word-list t)"\\b"))

(defvar tacl-keyword-between-slashes-regexp () "internal use only")

(defun  tacl-keyword-between-slashes-regexp ( word-list )
  "Returns a function that finds the words passed only if between /  /."
  (setq tacl-keyword-between-slashes-regexp
	(concat "\\b"(regexp-opt word-list t)"\\b"))
  'tacl-font-lock-between-slashes)

(defvar tacl-amid-font-lock-excursion nil
  "Used by `tacl-font-lock-between-slashes'.
   When a pair of slashes are detected this variable holds the context
   needed to continue searching for more keywords.  If nil, slash marks
   should be searched for.")

(make-variable-buffer-local 'tacl-amid-font-lock-excursion)

(defun tacl-font-lock-between-slashes ( search-limit )
  "This function finds keywords between forward slash marks only.
   Valid keywords are described by tacl-keyword-between-slashes-regexp.
   First a line containing text between forward slashes is searched for.
   Once found, point is moved to the beginning of that area and limit
   is set to the end.  Keywords are searched for within that range.
   If found, context is saved in tacl-amid-font-lock-excursion and the
   match-data is returned.  If not found, another set of slash marks
   is searched for.  If saved context exists when this function is
   called then another keyword is searched for between the previously
   found slashes.  If none is found, more /.../ syntax is searched for."
  (let ((looking t))
    (while
	(and looking
	     (or tacl-amid-font-lock-excursion
		 (when (re-search-forward "/.+/" search-limit t)
		   (setq tacl-amid-font-lock-excursion (point))
		   (goto-char (match-beginning 0)))    )   )
      (if (re-search-forward tacl-keyword-between-slashes-regexp
			     tacl-amid-font-lock-excursion t)
	  (setq looking nil)
	(goto-char tacl-amid-font-lock-excursion)
	(setq tacl-amid-font-lock-excursion nil)))
    (not looking)))

;; This finds comments and strings because the syntax table can't handle TACL.
(defun tacl-find-syntactic-keywords ( search-limit )
  ;; Comments starting with == go to eol always, comments starting with {
  ;; are only recognized if there is a corresponding } ender before eol.
  ;; Strings are treated like {}.  That is, they start and end with " as 
  ;; usual but cannot span lines so an unterminated quote does not start
  ;; a string.  This function returns t if a comment or string is found,
  ;; nil if neither is found. match-data 1&2 are set for comments, 3&4 for
  ;; strings.  Te match-data pairs (1&2 or 3&4) mark the start character
  ;; and end character respectively.  Point is left at the end of the match
  ;; or unchanged if no match.
  (when (re-search-forward "\\(?:==\\|{\\|\"\\)" search-limit t)
    ;; either a comment or string starter was found
    (let ((start (list (car (match-data)) ; Only 1st character gets marked
                       (copy-marker (1+(car (match-data))))))
          (match (match-string-no-properties 0))
          end)
      ;; see if it was a string
      (cond
       ((equal "\"" match)
        ;; If either 'when' fails nil is returned so match data should
        ;; be ignored.  But if something tries to use it, it will be
        ;; the original search above which contains no groups.
        (when (re-search-forward "\\(?:\"\\|$\\)" search-limit t)
          (when (equal "\"" (match-string-no-properties 0))
            (setq end (match-data))
            (set-match-data
             `(,(car start) ,(cadr end) ;match-string 0
               nil nil nil nil          ;match-string 1&2 not found
               ,@start ,@end))          ;match-string 3&4
            t)))                        
       ((equal "{" match)
        ;;see above 'when' comment
        (when (re-search-forward "\\(?:}\\|$\\)" search-limit t)
          (when (equal "}" (match-string-no-properties 0))
            (setq end (match-data))
            (set-match-data
             `(,(car start) ,(cadr end) ;match-string 0 
               ,@start ,@end))          ;match-string 1&2
            t)))                        ;Return "match found"
       ((equal "==" match)
        ;;see above 'when' comment
        (when (re-search-forward "\\(?:\n\\|\\'\\)" search-limit t)
          (setq end (match-data))
          (set-match-data
           `(,(car start) ,(cadr end)   ;match-string 0
             ,@start ,@end))            ;match-string 1&2
          t))))))                       ;Return "match found"

(defvar tacl-font-lock-syntactic-keywords
  ;; tacl-find-syntactic-keywords returns match 1&2 for comments, 3&4 for strings
  '((tacl-find-syntactic-keywords (1 "<"  t t) (2 ">"  t t)
                                  (3 "\"" t t) (4 "\"" t t)))
  "A list of regexp's or functions.  Used to add syntax-table properties to
characters that can't be set by the syntax-table alone.")

(defvar tacl-static-font-lock-keywords
  ; font-lock-keywords is a symbol or list of symbols yielding the keywords to
  ; be fontified.  Keywords are listed here using either (MATCHER . FACENAME)
  ; or (MATCHER . (MATCH FACENAME)) syntax.  Other options are available but
  ; not used here.  For simplicity, all regexp's were designed so MATCH would
  ; be 1.  Nothing forced this but to me it makes debug/maintenance easier.
  `((,(tacl-keyword-anywhere-regexp tacl-keywords-builtin-fcns)
     1 font-lock-keyword-face)
    (,(tacl-keyword-anywhere-regexp tacl-keywords-builtin-vars)
     1 font-lock-builtin-face)
    (,(tacl-keyword-vartype-regexp tacl-keywords-var-types)
     1 font-lock-builtin-face)
    (,(tacl-keyword-directives-regexp tacl-keywords-directives)
     1 font-lock-warning-face)
    (,(tacl-keyword-?tacl-regexp tacl-keywords-?tacl-types)
     1 font-lock-builtin-face)
    (,(tacl-keyword-between-slashes-regexp tacl-keywords-option-verbs)
     1 font-lock-constant-face)
    (,(tacl-keyword-anywhere-regexp tacl-user-keywords)
     1 font-lock-type-face)
    (,(tacl-keyword-between-bars-regexp tacl-keywords-enclosed)
     1 font-lock-constant-face)
    (,(tacl-function-name-regexp tacl-keywords-var-types)
     1 font-lock-function-name-face)
;;;;    ("`\\(\\sw\\sw+\\)'" 1 font-lock-constant-face prepend)
    ;; The question mark must be followed by a directive or another ?.
    ("^\\(\\?\\)[^?]" (1 font-lock-warning-face nil t)))
  "Keyword highlighting specification for `tacl-mode'.")


(defvar tacl-font-lock-keywords ())

(defun tacl-build-font-lock-keywords ()
  "Used to create `font-lock-keywords' based on current customize settings."
  (append tacl-static-font-lock-keywords
          `(,(when tacl-primecode-warning
               ;; ]a  ]d or ]e cannot appear in col 1-2 if using RMS/PrimeCode.
               '("^\\][ade]" . font-lock-warning-face)))))

(defun tacl-setup-font-lock ()
  "Sets up the buffer local value for font-lock-defaults and optionally
turns on font-lock-mode"
  ;; I use font-lock-syntactic-keywords to set some properties and I
  ;; don't want them ignored.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;; Useful when commented out code might unbalance parens
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; This allows column markers to be different in seperate tacl-mode buffers.
  (set (make-local-variable 'tacl-font-lock-keywords)
       (tacl-build-font-lock-keywords))
  ; This is where all the font-lock stuff actually gets set up.  Once
  ; font-lock-defaults has it's value, setting font-lock-mode true should
  ; cause all your syntax highlighting dreams to come true.
  (set (make-local-variable 'font-lock-defaults)
         ; The first value is all the keyword expressions.
       '(tacl-font-lock-keywords
         ; keywords-only means no strings or comments get fontified
         nil
         ; case-fold (ignore case)
         t
         ; syntax-alist AFAIK nothing needs different syntax for font-lock
         ()
         ; syntax-begin - no function defined to move outside syntactic block
         nil
         ;; font-lock-syntactic-keywords
         ;; takes (matcher (match syntax override lexmatch) ...)...
         (font-lock-syntactic-keywords . tacl-font-lock-syntactic-keywords)))
  ; font lock is turned on by default in this mode. Use customize to disable.
  (when tacl-font-lock-always (font-lock-mode t)))

;;; Static Column Markers

(defcustom tacl-column-marker-1 0
  "*Turns on column-marker-1 (which see) at the specified column.
Use of this feature requires the column-marker.el package be loaded or on
the search list."
  :type 'integer
  :group 'tacl)
(make-variable-buffer-local 'tacl-column-marker-1)

(defcustom tacl-column-marker-2 79
  "*Turns on column-marker-2 (which see) at the specified column.
Use of this feature requires the column-marker.el package."
  :type 'integer
  :group 'tacl)
(make-variable-buffer-local 'tacl-column-marker-2)

(defun tacl-setup-column-markers ()
  "Turns on column markers if configured and available.
See `tacl-column-marker-1' and `tacl-column-marker-2' "
  (if (condition-case ()
          (progn (require 'column-marker) nil)
        (error t))
      (if (not (and (zerop tacl-column-marker-1)
                    (zerop tacl-column-marker-2)))
          (message "column markers are configured but %s"
                   "column-marker feature not available."))
    (setq indent-tabs-mode nil)      ;documented as buffer local
    (column-marker-1 tacl-column-marker-1)
    (column-marker-2 tacl-column-marker-2)))

;;; Imenu & Which-function

(defcustom tacl-imenu-menubar t
  "If not nil, `imenu-add-to-menubar' is called during mode initialization.
This adds a [Menu name] menu to your menu bar.  By default the menu contains a
list of all procedures, sections and pages in your program.  You can go
directly to any item on the menu by selecting it.  You can control what
appears on this menu by modifying `tacl-imenu-expression-alist'.  You must turn
imenu on for this to work.  See `imenu' in the Emacs reference manual for more
information.  Personally I recommend customizing `imenu-sort-function' to sort
by name."
  :type  '(choice :tag "Menu Name"
                  (const  :tag "No Menu" nil)
                  (const  :tag "Name=Index" t)
                  (string :tag "Menu Name"))
  :group 'tacl)

(defvar tacl-imenu-syntax-alist '((":"."w"))
  "Overrides to tacl-mode-syntax-table used during
imenu-generic-expression search.")

(defcustom tacl-imenu-expression-alist
  `(("Sections" ,(concat "^\\?SECTION +\\(\\w+\\s-+"
                         (regexp-opt tacl-keywords-var-types t)
                         "\\)\\b") 1)
    ("Defs" ,(concat "^ \\{0,2\\}\\[#def\\s-+\\(\\w+\\s-+"
		     (regexp-opt tacl-keywords-var-types t)
		     "\\)\\b") 1))
  "A list of regular expressions for creating an `imenu' index.

Each element has the form (list-name regexp num).

Where list-name is the name of the submenu under which items matching regexp
are found and num is the expression index defining the label to use for the
submenu entry.  When num = 0 the entire matching regexp text appears under
list-name.  When list-name is nil the matching entries appear in the root
imenu list rather than in a submenu.  See also `tacl-imenu-menubar'"
  :type '(repeat (list (choice :tag "Submenu Name" string (const nil))
                       regexp (integer :tag "Regexp index")))
  :group 'tacl)

(defcustom tacl-display-which-function t
  "This option depends on `imenu'.  Displays current proc on mode line.
`Which-func' is a package that causes the current function, section or page
to be displayed on the mode line.  Each imenu entry points to a position
in the current buffer.  The name associated with the greatest position
less than the current cursor point is what is displayed on the mode
line. See `tacl-imenu-expression-alist' for more information."
  :type 'boolean
  :group 'tacl)

(defun tacl-setup-imenu ()
  "Installs tacl-imenu-generic-expression & tacl-imenu-syntax-alist."
  ; imenu doc says all 3 are buffer-local by default
  (setq imenu-generic-expression tacl-imenu-expression-alist)
  (setq imenu-syntax-alist tacl-imenu-syntax-alist)
  (setq imenu-case-fold-search t)
  (when tacl-imenu-menubar
    (if (condition-case ()
            (progn (require 'imenu) t)
          (error nil))
        (if (and (stringp tacl-imenu-menubar)(not(equal tacl-imenu-menubar "")))
            (imenu-add-to-menubar tacl-imenu-menubar)
          (imenu-add-menubar-index))
      (message "tacl-imenu-menubar is set but imenu feature not available.")))
  (when tacl-display-which-function
    (if (condition-case ()
            (progn (require 'which-func) t)
          (error nil))
        (which-function-mode t)
      (message "tacl-display-which-function set but which-func not available"))))

 ;;; Adaptive fill

(defun tacl-setup-adaptive-fill ()
  "Sets up the TACL-MODE adaptive-fill variables"
  (set (make-local-variable 'comment-start) "==")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^~\n]\\)\\(~~\\)*\\)== *"))

;;; Indentation

(defun tacl-setup-indent ()
  "Sets default indentation or sets up tacl-indent if available."
  (if (condition-case ()
          (progn (require 'tacl-indent) t)
        (error nil))
      (set (make-local-variable 'indent-line-function) 'tacl-indent-line)
    (set (make-local-variable 'indent-line-function) 'indent-relative-maybe)))

(defun tacl-indent-line ()
  "Indent current line of Tacl code. - work in progress don't use"
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
	(indent (condition-case nil (max (tacl-calculate-indentation) 0)
		  (error 0))))
    (if savep (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun tacl-skip-syntax-back (syntax &optional limit)
  ;; like skip-syntax-backward but ignores strings & comments
  ;; returns t if point is at specified syntax, nil if not found.
  (unless limit (setq limit (point-min)))
  (skip-syntax-backward syntax limit)
  (let ((searching t) special)
    (while (and searching
                (> (point) limit))
      (setq special (get-text-property (point) 'face))
      (if (setq searching (or (eq special 'font-lock-comment-face)
                              (eq special 'font-lock-string-face)))
          (skip-syntax-backward syntax limit)))
    (not searching)))

(defun tacl-calculate-indentation ()
  "Return appropriate indentation for current line as TACL code.
   In usual case returns an integer: the column to indent to."
  (let ((indent 0)
        (searching t)
        (chartype 0)
        (from (point)))
    (save-excursion
      (beginning-of-line)
      (if (and (looking-at "[ \n\t]*==")
               (not (bobp)))
          (progn
            (beginning-of-line 0)
            (setq indent (current-indentation)))
        (if (looking-at "\\?\\w")
            (setq indent 0)
          (while (and searching (not (bobp)))
            (backward-char)
            (setq searching
                  (or (looking-at "[ \n\t]")
                      (eq (get-text-property (point) 'face)
                          'font-lock-comment-face))))
          (when (not (bobp))
            (setq indent (current-indentation))
            (if (equal ?& (following-char))
                (setq indent 0))))))
    indent))

;;; Language Skeletons -- Feel free to add more of your own!

(defcustom tacl-keywords-case 'lower
  "*Indicates if keywords in skeletons should be all UPPER CASE, all
lower case or Camel Case (First Char Upper & Rest Lower)."
  :type  '(choice (const :tag "ALL CAPS"   'upper)
                  (const :tag "all small"  'lower)
                  (const :tag "Camel Case" 'camel)
                  (const :tag "DON'T Change"  ()))
  :group 'tacl)

(defun tacl-setup-skel ()
  "Configures skeleton.el functions for the TACL environemnt."
  (set (make-local-variable 'skeleton-transformation) 'tacl-skel-transform)
  ;; This prevents abbrevs from expanding within skeletons
  (setq skeleton-further-elements '((abbrev-mode nil))))

(defun tacl-skel-transform ( element )
  "Used to insure skeleton's are inserted using the requested capitalization."
  ;; This should be made more complex to only change the case of certain words
  ;; so the user can create skeletons containing items that should not be
  ;; affected by tacl-keywords-case.  There are 3 obvious ways.  1) use the
  ;; keywords tables above. 2) add a customize to ignore words. 3) add a
  ;; customize to specify specific words to be affected.  Preferences?
  (if (stringp element)
    (cond
     ((eq tacl-keywords-case 'upper) (upcase element))
     ((eq tacl-keywords-case 'lower) (downcase element))
     ( t                             (capitalize element)))
    element))

(define-skeleton tacl-argument-skel     ;a  Arg
  "Inserts a TACL if [#argument/value / ]  statement skeleton."
  nil  "#if [#argument/value " _ "/ " _ "]")

(define-skeleton tacl-def-skel          ;d  Def
  "Inserts a TACL #def statement skeleton."
  nil  "[#def " _ " |body|"
  \n > " #frame"
  \n > "  " _
  \n > "#unframe   "
  \n > -1 "]" )

(define-skeleton tacl-frame-skel        ;{  block begin
  "Inserts a TACL #frame/#unframe pair."
  nil  "#frame"
  \n > "  " _
  \n > "#unframe")

(define-skeleton tacl-if-skel           ;i  If
  "Inserts a TACL if/then statement skeleton."
  nil "[#IF [" _ "]"
      \n > "  |THEN|" _ "  "
      \n > -2 "]")

(define-skeleton tacl-if-else-skel      ;e  if-then-Else
  "Inserts a TACL #if |then| |else| statement skeleton."
  nil "[#IF [" _ "]"
       \n > "  |THEN|" _
       \n >   "|ELSE|" _ "  "
       \n > -2 "]")

(define-skeleton tacl-loop-while-skel   ;l  Loop
  "Inserts a TACL #loop |while| |do| statement skeleton."
  nil "[#loop |while| " _ 
       \n > "  |do|" _
       \n > -2 "]")

(define-skeleton tacl-loop-until-skel   ;u  loop Untill
  "Inserts a TACL #loop |do| |until| statement skeleton."
  nil "[#loop "
      \n > "  |do|" _
      \n >   "|until|" _ "  "
      \n > -2 "]")

(define-skeleton tacl-case-skel         ;c  Case
  "Inserts a TACL #Case statement skeleton."
  nil "[#CASE " _
      \n > "  |" _ " |" _
      \n >   "|OTHERWISE|" _ "  "
      \n > -2 "]")

;;; Movement by ...

;(defvar tacl-outline-regexp
;...)

;;; Eldoc support

(defcustom tacl-eldoc-def-files '("tacl-syntax.el")
  "List of files containing function help strings used by `eldoc-mode'.
These are the strings eldoc-mode displays as help for functions near point.
The format of the file must be exactly as follows or who knows what happens.

   (set (intern \"<fcn-name1>\" tacl-eldoc-obarray) \"<helper string1>\")
   (set (intern \"<fcn-name2>\" tacl-eldoc-obarray) \"<helper string2>\")
...

Where <fcn-name> is the name of the function to which <helper string> applies.
      <helper-string> is the string to display when point is near <fcn-name>.

There doesn't seem to be any way of automating help strings like in TAL."
  :type '(repeat string)
  :group 'tacl)

(defvar tacl-eldoc-obarray ()
  "Array into which tacl-eldoc-def-files entries are added for use by eldoc.")

(defun tacl-eldoc-function ()
  "Returns a documentation string appropriate for the current context or nil"
  (let (word)
    (if (equal (preceding-char) ?/)
        (save-excursion
          (backward-char)
          (skip-syntax-backward "-")
          (setq word (concat (thing-at-point 'symbol) "/")))
      (setq word (thing-at-point 'symbol))
      (if word
          (or (save-excursion
                (skip-syntax-backward "w")
                (skip-syntax-backward "-")
                (skip-syntax-backward "w")
                (if (looking-at "#set")
                    (setq word (concat "#SET" word))))
              (if (or (bobp)
                      (looking-back "\\W"))
                  (setq word (concat "|" word))))))
    (if word
        (eval (intern-soft (upcase word) tacl-eldoc-obarray)))))

(defun tacl-setup-eldoc ()
  "Loads the function documentation for use with eldoc."
  ;;(set (make-local-variable 'eldoc-print-current-symbol-info-function)
  ;;     'tacl-eldoc-function)
  ;; cvs emacs change the name of the var on 04/26/05 
  ;; so I've got to specify both for a while.
  (set (make-local-variable 'eldoc-documentation-function)
       'tacl-eldoc-function)
  (unless (vectorp tacl-eldoc-obarray)
    (setq tacl-eldoc-obarray (make-vector 41 0))
    (condition-case var (mapc 'load tacl-eldoc-def-files)
      (error (message "*** ERROR *** %s" var)))))

;;; Miscellaneous


;;;###autoload
(defun tacl-mode ()
  "A major mode for editing TACL language files.
Customization options are available via
\\[customize-group] <ret> TACL <ret>

This mode provides TACL specific support for the following packages:
    `font-lock-mode'      `show-paren-mode'     `imenu'               
    `which-function'      `skeleton-insert'     `auto-fill-mode'      
    `adaptive-fill-mode'  `abbrev-mode'         `eldoc-mode'          

** Note ** Some things won't work correctly if `font-lock-mode' is off.

tacl-mode also implements the following commands \\[execute-extended-command] ... commands

`tacl-mode'            activates this mode for the current buffer
`tacl-case-skel'       inserts a labeled case statement skeleton
`tacl-frame-skel'      inserts a #frame/#unframe pair skeleton
`tacl-if-skel'         inserts an if/then statement skeleton
`tacl-if-else-skel'    inserts an if/then/else statement skeleton
`tacl-loop-while-skel' inserts a #loop while skeleton
`tacl-loop-until-skel' inserts a #loop until skeleton

\\{tacl-mode-map}
Use \\[describe-bindings] to see ALL key bindings.

Some settings I like:
Turn on `skeleton-pair-insert-maybe' for (), [] and \"\"
Turn on `imenu' and set `imenu-sort-function' to imenu--sort-by-name
Turn on `recentf-mode'. You might need `recentf-auto-cleanup' = 'never
Set `column-marker-1' to 79 so you can tell what tedit users can't see.
Load `popup-ruler' for a TEDIT F9 type ruler on steroids.
I find `transient-mark-mode' totally indespensible
CUA mode has some really great rectangle functions."
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'major-mode) 'tacl-mode)
  (set (make-local-variable 'mode-name) "TACL")
  (set (make-local-variable 'make-backup-files) nil) ;necessary for now
  (use-local-map tacl-mode-map)
  (set-syntax-table tacl-mode-syntax-table)
  (tacl-setup-menu)
  (tacl-setup-font-lock)
  (tacl-setup-adaptive-fill)
  (tacl-setup-abbrevs)
  (tacl-setup-imenu)
  (tacl-setup-eldoc)
  (tacl-setup-indent)
  (tacl-setup-skel)
  (tacl-setup-column-markers)
;  (set (make-local-variable 'outline-regexp) tacl-outline-regexp)
  (show-paren-mode 1)
  (run-mode-hooks 'tacl-mode-hook))

(provide 'tacl-mode)

;;; tacl-mode.el ends here.

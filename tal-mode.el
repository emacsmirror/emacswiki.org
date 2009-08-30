;;; tal-mode.el --- Handles the Tandem/NSK/Guardian TAL & PTAL languages.
;;                   A proprietary language of Tandem/Compaq/HP computers.

;; Copyright (C) 2001, 2004 Free Software Foundation, Inc.

;; Author: Rick Bielawski <rbielaws@i1.net>
;; Keywords: languages, extensions, Tandem, Guardian, NSK, TAL, pTAL
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

;; TAL is Tandem's Transaction Application Language.
;; pTAL is the newer 'portable' version of the language.

;; Keywords as of G06.05 are recognized by this version of tal-mode.
;; TAL words not supported in pTAL are highlighted with a warning face.
;;
;; Both ! and -- style comments are handled correctly (I think).
;;
;; Movement by balanced expressions is supported.  That is, begin/end pairs
;;   are recognized.
;;
;; imenu recognizes ?section <name>, ?page <name> and <type> PROC <name>.
;;

;;; ToDo:

;; Better movement by statements, functions etc.
;; Unterminated strings are currently terminated by EOL.  This makes viewing
;;   mangled code easier but finding unterminated strings harder.  Eventually
;;   I'd like to toggle this on/off.
;; imenu assumes  proc x;  is a fcn when it could be a pointer declaration.
;;   This affects which-function accuracy.
;; Add support for add-log-current-defun-function

;;; Installing:

;; Before you can use tal-mode, emacs needs to be able to find it.  Place the
;; tal-mode.el file in a directory on the load-path; typically the
;; .../site-lisp or perhaps .../lisp/progmods directory.  Usually you would
;; also want to byte compile tal-mode.el but this is not required.  To do
;; this, visit the tal-mode.el file, type: M-x emacs-lisp-byte-compile <ret>
;; There should be no warnings or errors during byte compilation.
;;
;; There are 4 basic ways to use TAL-MODE on a file.  The first method
;; manually selects tal-mode as the editing mode.  The other 3 cause emacs to
;; recognize automatically that you want to visit the file using tal-mode.
;;
;; Pick one:
;; 1. While visiting a file, type: M-x tal-mode <ret>
;; 2. Put the string -*-tal-*- in a comment on the first line of the file.
;;    Save the file and close it.  Now any time you open it tal-mode starts.
;; 3. Create an association between a particular file naming convention and
;;    tal-mode.  This is done by adding an association to auto-mode-alist.
;; For example:
;; (setq auto-mode-alist
;;   (append
;;     '(("\\.tal\\'" . tal-mode)         ;extension of .tal means tal-mode
;;       ("\\([\\/]\\|^\\)[^.]+$" . tal-mode)) ;so does no extension at all.
;;    auto-mode-alist))
;; 4. Advise set-auto-mode to look at the buffer contents upon loading.
;;
;; The above all tell emacs that you want to use tal-mode but you must load
;; tal-mode before you can use it.  There are 2 methods of telling emacs to
;; load the tal-mode routines.  The first unconditionally loads tal-mode
;; definitions immediately.  The second tells emacs to automatically load
;; tal-mode only when you try to use it.  Add one of the following lines to
;; your .emacs file.
;;
;;(require 'tal-mode)      ; Unconditional load
;;(autoload 'tal-mode "tal-mode" "Major mode for Tandem TAL/pTAL files." t nil)
;;
;; Please report any bugs!

;;; Getting eldoc to work in tal-mode:

;; Open a file containing procedure declarations for which you want
;; help permanently loaded.  For example: $SYSTEM.SYSTEM.EXTDECS0.
;; The buffer must be in tal-mode.  Use M-x tal-mode <ret> if necessary.
;;     M-x tal-eldoc-make-list <ret>
;; You should now be in a specially formatted buffer containing a list of
;; the functions declared and their corresponding help strings.  You can
;; modify the strings themselves if desired but do not alter anything
;; else.  Repeat these steps to add more help entries to the file.
;; Save the file somewhere on your search list like your site-lisp
;; directory. Example:
;;     C-x C-w ~/../site-lisp/extdecs-help.el <ret>
;; Now add the file you just saved to the list of tal-mode eldoc help
;; files to be loaded.
;;     M-x customize-option <ret> tal-eldoc-def-files <ret>
;; Save the customization for future sessions.
;; Once you have your help entries defined see help for eldoc-mode for
;; turing on the mode.  Putting (eldoc-mode t) in .emacs might do it.

;;; History:

;; 2004-05-26 RGB Mode is finally useable enough to start tracking.
;; 2004-06-02 RGB Prettied up some comments and code sections.
;;                Fixed minor bug turning on imenu & which-function.
;; 2004-06-17 RGB Lots of updates to documentation and comments.
;;                Added customization of tal-primecode-warning
;;                Changed how comments & strings are detected by font-lock
;;                so that more things work more smoothly (hopefully).
;;                Added/fixed adaptive-fill support.
;; 2004-08-12 RGB Finally broke down and fixed the comment/string handling the
;;                way it really should have been done in the first place.
;;                Tweaked the imenu regexp to eliminate leading whitespace
;;                in ?page "        heading strings".
;; 2004-08-25 RGB Column markers are now dynamically configurable.
;; 2004-09-15 RGB Added a column ruler somewhat similar to Tedit's ruler.
;; 2004-09-25 RGB Many comment updates.
;; 2004-12-22 RGB The beginnings of indent support. Finally.
;; 2005-01-05 RGB Moved indent support to it's own library.
;;                Added chord for tal-column-ruler.
;; 2005-01-15 RGB Fixes for bugs introduced on 01-05.
;; 2005-01-18 RGB Changes needed for better indentation support.
;; 2005-01-25 RGB Fixed bugs preventing proper recognition of the following:
;;                Fixed(*) end#, end#;  begin:disable_overflow_traps
;; 2005-01-30 RGB Removed reserved words already being highlighted
;;                by other expressions.  Added eldoc support!!!
;; 2005-01-31 RGB Got rid of compiler warnings introduced yesterday.
;;                Fixed some bugs in eldoc. Updated documentation.
;; 2005-02-01 RGB Added toggle to ignore comments in eldoc text.
;; 2005-02-25 RGB Attempt to make skeletons work with tal-indent.
;;                Lots of little doc updates.  Abbrev support added.
;; 2005-03-11 RGB Eldoc now works inside a proc argument list.
;; 2005-03-15 RGB And now handles string:len style args better.
;; 2005-03-17 RGB Fixed bug where wrong parens were found when
;;                eldoc text contained an int(32) proc(arg) type decl.
;; 2005-04-06 RGB Changed -- comment to mark only 1st char as comment
;;                start for (hopefully) easier comment detection.
;; 2005-04-13 RGB Redeveloped fixes from 2/25 lost ... sometime.
;; 2005-05-15 RGB Added support for inquiring the name and visiting the
;;                file where an eldoc help definition was extracted.
;;                Added saving of that info to tal-eldoc-make-list.
;; 2005-06-24 RGB Reworked column marker code.
;; 2005-06-29 RGB Replaced column-ruler with new popup-ruler.  Moved
;;                skeletons to their own keymap.  Added keychord for
;;                begin/end skeleton.
;; 2005-07-12 RGB changed syntax class of string markers in preperation
;;                for writing Cedet/Semantic support.
;; 2005-08-16 RGB More skeletons, keyboard tweaks. No time for Semantic.
;; 2005-10-19 RGB Removed column-marker & popup-ruler stuff.
;;                These are now standalone packages here:
;;                http://www.emacswiki.org/cgi-bin/emacs/column-marker.el
;;                http://www.emacswiki.org/cgi-bin/emacs/popup-ruler.el
;;                Added automatic detection and support for above packages.
;; 2005-11-11 RGB Reworked paren syntax of block/end block to add procptr.
;; 2006-08-29 RGB Unterminated strings are now highlighted with WARNING-FACE.
;;                Fixed bug in tal-eldoc-visit-file.  Call run-mode-hooks
;;                rather than run-hooks (per Emacs 22.0 standards).  Fixed
;;                (I think) some problems with customization options.
;; 2006-10-20 RGB Added tal-setup-menu.  Moved eldoc keys to their own map.
;;                Updated a couple doc strings and comments.
;;; Code:

(defgroup tal nil
  "Major mode for editing Tandem TAL/pTAL source files in Emacs.
While in tal-mode use C-h m for a description of the mode's features."
  :prefix 'tal-
  :group 'languages)

;;; KEY MAP

(defvar tal-skeleton-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?{]   'tal-begin-end-skel)
    (define-key map [?\[]  'tal-begin-end2-skel)
    (define-key map [?\]]  'tal-else-begin-skel)
    (define-key map [?c]   'tal-case-skel)
    (define-key map [?e]   'tal-if-else-skel)
    (define-key map [?i]   'tal-if-skel)
    (define-key map [?p]   'tal-proc-skel)
    map)
  "Keymap for `tal-mode'.")

(defvar tal-eldoc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?s]   'tal-eldoc-scan-buffer)
    (define-key map [?v]   'tal-eldoc-visit-file)
    (define-key map [?w]   'tal-eldoc-where-def)
    map)
  "Keymap for `tal-mode'.")

(defvar tal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab]           'indent-according-to-mode)
    (define-key map [?\C-c ?\C-c]   'column-marker-here)
    (define-key map [?\C-c ?\C-e]    tal-eldoc-map)
    (define-key map [?\C-c ?\C-f]   'auto-fill-mode)
    (define-key map [?\C-c ?\C-r]   'popup-ruler)
    (define-key map [?\C-c ?\C-s]    tal-skeleton-map)
    (define-key map [?\C-c return]  'comment-indent-new-line)
    map)
  "Keymap for `tal-mode'.")

(defun tal-setup-menu ()
  "Adds a menu of TAL specific functions to the menu bar."
  (define-key (current-local-map) [menu-bar tal-menu]
    (cons "TAL" (make-sparse-keymap "TAL")))
  (define-key (current-local-map) [menu-bar tal-menu eldoc-add]
    '(menu-item "Eldoc Scan" tal-eldoc-scan-buffer
                :key-sequence [?\C-c ?\C-e ?s]
                :help "Scans this buffer for eldoc entries"))
  (define-key (current-local-map) [menu-bar tal-menu eldoc-show]
    '(menu-item "Eldoc Where" tal-eldoc-where-def
                :key-sequence [?\C-c ?\C-e ?w]
                :help "Shows Where function at point is defined"))
  (define-key (current-local-map) [menu-bar tal-menu eldoc-visit]
    '(menu-item "Eldoc Visit" tal-eldoc-visit-file
                :key-sequence [?\C-c ?\C-e ?v]
                :help "Visits file defining function at point"))
  (define-key (current-local-map) [menu-bar tal-menu ruler]
    '(menu-item "Ruler" popup-ruler
                :key-sequence [?\C-c ?\C-r]
                :help "Inserts temporary ruler"))
  (define-key (current-local-map) [menu-bar tal-menu comment-eol]
    '(menu-item "Comment EOL" comment-indent-new-line
                :key-sequence [?\C-c return]
                :help "Continues comment on new line"))
  (define-key (current-local-map) [menu-bar tal-menu column]
    '(menu-item "Column Marker" column-marker-here
                :key-sequence [?\C-c ?\C-c]
                :help "Puts column marker at current column (C-u removes)"))
  (define-key (current-local-map) [menu-bar tal-menu skeletons]
    (cons "Skeletons" (make-sparse-keymap "Skeletons")))
  (define-key (current-local-map) [menu-bar tal-menu skeletons begin2]
    '(menu-item "Begin/End" tal-begin-end2-skel
                :key-sequence [?\C-c ?\C-s ?\[]
                :help "Insert a Begin/End pair (no trailing ;)"))
  (define-key (current-local-map) [menu-bar tal-menu skeletons else]
    '(menu-item "Else" tal-else-begin-skel
                :key-sequence [?\C-c ?\C-s ?\]]
                :help "Insert Else Begin / End;"))
  (define-key (current-local-map) [menu-bar tal-menu skeletons begin]
    '(menu-item "Begin/end;" tal-begin-end-skel
                :key-sequence [?\C-c ?\C-s ?{]
                :help "Insert a Begin/End; pair"))
  (define-key (current-local-map) [menu-bar tal-menu skeletons proc]
    '(menu-item "Proc Skel" tal-proc-skel
                :key-sequence [?\C-c ?\C-s ?p]
                :help "Inserts a procedure template"))
  (define-key (current-local-map) [menu-bar tal-menu skeletons if-else]
    '(menu-item "If Else" tal-if-else-skel
                :key-sequence [?\C-c ?\C-s ?e]
                :help "Inserts an If/Then/Else statement"))
  (define-key (current-local-map) [menu-bar tal-menu skeletons if]
    '(menu-item "If Then" tal-if-skel
                :key-sequence [?\C-c ?\C-s ?i]
                :help "Inserts an If/Then statement"))
  (define-key (current-local-map) [menu-bar tal-menu skeletons case]
    '(menu-item "Case" tal-case-skel
                :key-sequence [?\C-c ?\C-s ?c]
                :help "Insert a TAL labeled CASE statement")))

(defvar tal-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\! "." st)
    (modify-syntax-entry ?\" "." st)    ;wiki \ " bug workaround comment
    (modify-syntax-entry ?\# "." st)
    (modify-syntax-entry ?\$ "w" st)
    (modify-syntax-entry ?\% "'" st)
    (modify-syntax-entry ?\& "'" st)
    (modify-syntax-entry ?\' "." st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\* "." st)
    (modify-syntax-entry ?\+ "." st)
    (modify-syntax-entry ?\, "." st)
    (modify-syntax-entry ?\- "." st)
    (modify-syntax-entry ?\. "'" st)
    (modify-syntax-entry ?\/ "." st)
    (modify-syntax-entry ?\: "." st)
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\< "." st)
    (modify-syntax-entry ?\= "." st)
    (modify-syntax-entry ?\> "." st)
    (modify-syntax-entry ?\? "'" st)
    (modify-syntax-entry ?\@ "'" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?^  "w" st)
    (modify-syntax-entry ?\_ "w" st)
    (modify-syntax-entry ?\{ "." st)
    (modify-syntax-entry ?\| "." st)
    (modify-syntax-entry ?\} "." st)
    st)
  "Syntax table for `tal-mode'.")

;; All keyword lists get sorted so new words can be anywhere within the
;; appropriate list.  The keywords are currently only used for highlighting but
;; more uses such as abbrev-mode are in progress.

(defvar tal-keywords-unqualified-data-types
  '( "string" "literal"  "define" "struct" "procptr")
  "A list of keywords that denote an unqualified data type.
Used to create the `font-lock-keywords' table.  Unqualified data types
are data types which do not accept a size qualifier such as int(16),
unsigned(32) etc.  See also `tal-keywords-qualified-data-types'.")

(defvar tal-keywords-qualified-data-types
  '( "int" "unsigned" "fixed" "real" )
  "A list of keywords that denote data types which can be qualified.
Used to create the `font-lock-keywords' table.  Qualified data types are
data types that accept a size qualifier such as int(16), unsigned(32)
etc.  See also `tal-keywords-unqualified-data-types'.")

(defvar tal-keywords-address-types
  '( ".ext"       ".sg"        ".sgx"       "baddr"      "cbaddr"
     "cwaddr"     "extaddr"    "procaddr"   "sgbaddr"    "sgwaddr"
     "sgxbaddr"   "sgxwaddr"   "waddr"      )
  "List of TAL/pTAL variable types.
Used to create the `font-lock-keywords' table.")

(defvar tal-keywords-directives
  '( "abort"               "abslist"             "assertion"
     "begincompilation"    "blockglobals"        "checkshiftcount"
     "code"                "columns"             "decs"
     "defexpand"           "definetog"           "do-tns-syntax"
     "dumpcons"            "else"                "endif"
     "errorfile"           "errors"              "export_globals"
     "extendtalheap"       "fieldalign"          "fmap"
     "gmap"                "gp_ok"               "highpin"
     "highrequesters"      "icode"               "if"
     "ifnot"               "innerlist"           "inspect"
     "int32index"          "invalid-for-ptal"    "library"
     "lines"               "list"                "map"
     "noabort"             "noabslist"           "nocode"
     "nodefexpand"         "nofmap"              "nogmap"
     "noinnerlist"         "nolist"              "nomap"
     "nooverflow_traps"    "noprintsym"          "noround"
     "nosuppress"          "nosymbols"           "nowarn"
     "optimize"            "optimizefile"        "overflow_traps"
     "page"                "pep"                 "popcode"
     "popdefexpand"        "popinnerlist"        "poplist"
     "popmap"              "printsym"            "pushcode"
     "pushdefexpand"       "pushinnerlist"       "pushlist"
     "pushmap"             "refaligned"          "resettog"
     "round"               "rp"                  "saveabend"
     "saveabend"           "saveglobals"         "section"
     "settog"              "source"              "srl"
     "suppress"            "symbolpages"         "symbols"
     "syntax"              "target"              "useglobals"
     "warn"                )
  "List of TAL/pTAL compiler directives.
Used to create the `font-lock-keywords' table.")

(defvar tal-keywords-statements
  '( "and"        "assert"     "begin"      "by"         "call"
     "callable"   "case"       "do"         "downto"     "drop"
     "else"       "end"        "entry"      "external"   "fieldalign"
     "for"        "forward"    "goto"       "if"         "interrupt"
     "label"      "land"       "lor"        "main"       "not"
     "of"         "or"         "otherwise"  "priv"       "proc"
     "refaligned" "resident"   "return"     "rscan"      "scan"
     "subproc"    "then"       "to"         "until"      "use"
     "variable"   "volatile"   "while"      "xor"        )
  "List of TAL/pTAL statement keywords.
Used to create the `font-lock-keywords' table.")

(defvar tal-keywords-deprecated
  '( "$axadr"     "$carry"     "$ladr"      "$overflow"  "'g'"
     "'l'"        "'s'"        "code"       "stack"      "store"      )
  "List of TAL keywords and Builtin functions now deprecated.
Used to create the `font-lock-keywords' table")

(defvar tal-keywords-nonreserved
  '( "'p'"        "'sg'"       "at"         "below"      "bit_filler"
     "block"      "bytes"      "elements"   "filler"     "private"
     "words"
;;   "auto"        "c"           "cobol"       "ext"         "extensible"
;;   "fortran"     "language"    "name"        "nodefault"   "pascal"
;;   "returncc"    "shared2"     "shared8"     "unspecified"
   )
  "List of TAL keywords reserved only in certain language contexts.
Used to create the `font-lock-keywords' table.")

(defvar tal-keywords-std-fcns
  '( "$abs"                "$alpha"              "$baddr_to_extaddr"
     "$baddr_to_waddr"     "$bitlength"          "$bitoffset"
     "$comp"               "$dbl"                "$dbll"
     "$dblr"               "$dfix"               "$eflt"
     "$efltr"              "$extaddr"            "$extaddr_to_baddr"
     "$extaddr_to_waddr"   "$fill16"             "$fill32"
     "$fill8"              "$fix"                "$fixd"
     "$fixi"               "$fixl"               "$fixr"
     "$flt"                "$fltr"               "$high"
     "$ifix"               "$int"                "$int_ov"
     "$intr"               "$len"                "$lfix"
     "$lmax"               "$lmin"               "$max"
     "$min"                "$numeric"            "$occurs"
     "$offset"             "$optional"           "$param"
     "$point"              "$readclock"          "$rp"
     "$scale"              "$sgbaddr_to_extaddr" "$sgbaddr_to_sgwaddr"
     "$sgwaddr_to_extaddr" "$sgwaddr_to_sgbaddr" "$special"
     "$stack_allocate"     "$type"               "$udbl"
     "$usercode"           "$xadr"               "$waddr_to_baddr"
     "$waddr_to_extaddr"   )
  "List of TAL standard functions.
Used to create the `font-lock-keywords' table.")

(defvar tal-keywords-privileged
  '( "$executeio"          "$freeze"             "$halt"
     "$interrogatehio"     "$interrogateio"      "$locatespthdr"
     "$lockpage"           "$readbaselimit"      "$readspt"
     "$unlockpage"         "$writepte"           )
  "List of TAL privileged functions.
Used to create the `font-lock-keywords' table.")

(defvar tal-keywords-builtin
  '( "$asciitofixed"       "$atomic_add"         "$atomic_and"
     "$atomic_dep"         "$atomic_get"         "$atomic_or"
     "$atomic_put"         "$checksum"           "$countdups"
     "$exchange"           "$executeio"          "$fixedtoascii"
     "$fixedtosciiresidue" "$moveandcxsumbytes"  "$movenondup"
     "$readtime"           "$udivrem16"          "$udivrem32"          )
  "List of TAL privileged builtin functions.
Used to create the `font-lock-keywords' table.")

(defvar tal-keyword-fcn-names-regexp
  "^\\s-*\\(?:\\w+\\(?:\\s-*(\\w+)\\)?\\s-+\\)?\\(proc\\|subproc\\)\\s-+\\(\\w+\\)\\b"
  "Defines a regexp that finds the names of procedures & subprocedures.
Used to create the `font-lock-keywords' table and by tal-indent.el.")

;;; Paren matching

(defcustom tal-begin-matches-semi t
  "If not nil, the b of begin matches the semicolon in end;
Otherwise it matches the d of end.  It always matches the d when no ;"
  :type 'boolean
  :group 'tal)

;;; Font lock (highlighting)

(defcustom tal-font-lock-always t
  "`tal-mode' makes sure `font-lock-mode' is on for tal-mode buffers.
Some things don't work if it's off so insuring it's on is the default."
  :type 'boolean
  :group 'tal)

(defcustom tal-primecode-warning t
  "Highlight instances of ]a ]d and ]e in column 1 with a warning face.
This alerts you that submission of this file to RMS/PrimeCode will fail
due to invalid contents.  nil disables this warning."
  :type 'boolean
  :group 'tal)

(defun tal-keyword-anywhere-regexp ( word-list )
  "Returns a regexp that finds any of the words in WORD-LIST.
But only if the keyword is surrounded by non-word chars."
  (concat "\\<"(regexp-opt word-list t)"\\W"))

(defun tal-keyword-qualified-regexp ( word-list )
  "Returns a regexp that finds any of the words in WORD-LIST.
But only if the keyword is preceeded by a non-word char and optionally
followed by a '(width|fpoint)' qualifier and ends with a non-word char."
  (concat "\\<\\(?:"(regexp-opt word-list t)
          "\\(?:\\s-*\\((\\s-*\\(\\w+\\|\\*\\)\\s-*)\\)\\s-+\\|\\s-+[^()]\\)\\)"))

;; The next 4 def's work tightly together and, as coded, cannot be reused for
;; additional purposes.
(defvar tal-keyword-on-directive-line-regexp () "Internal tal-mode use only.")

(defun  tal-keyword-on-directive-line-regexp ( word-list )
"Returns a function to find WORD-LIST only if line starts with ?"
  (setq tal-keyword-on-directive-line-regexp
        (concat "\\b"(regexp-opt word-list t)"\\b"))
  'tal-font-lock-directive-line)

(defvar tal-amid-font-lock-excursion nil
;; Used by `tal-font-lock-directive-line'.  When a line starting with ? in
;; column 1 is detected this variable holds the context needed to continue
;; searching for more keywords.  If nil a line starting with ? should be
;; searched for.
)

(make-variable-buffer-local 'tal-amid-font-lock-excursion)

(defun tal-font-lock-directive-line ( search-limit )
;; This function finds keywords only in lines starting with ?.  Valid keywords
;; are described by `tal-keyword-on-directive-line-regexp'.  First a line
;; beginning with ? is searched for.  Once found, point is moved to the
;; beginning of that area and limit is set to the end.  Keywords are searched
;; for within that range.  If found, context is saved in
;; tal-amid-font-lock-excursion and the match-data is returned.  If not found,
;; another line starting with ?  is searched for.  If saved context exists when
;; this function is called then another keyword is searched for in the
;; previously narrowed region.  If none is found the next region is searched
;; for.
  (let ((looking t))
    (while
        (and looking
             (or tal-amid-font-lock-excursion
        	 (when (re-search-forward "^\\?.+\n" search-limit t)
        	   (setq tal-amid-font-lock-excursion (point))
        	   (goto-char (match-beginning 0)))))
      (if (re-search-forward tal-keyword-on-directive-line-regexp
        		     tal-amid-font-lock-excursion t)
          (setq looking nil)
        (goto-char tal-amid-font-lock-excursion)
        (setq tal-amid-font-lock-excursion nil)))
    (not looking)))

;; This finds comments and strings because the syntax table can't handle TAL.
(defun tal-find-syntactic-keywords ( search-limit )
  ;; Comments starting with -- go to eol always, while comments starting
  ;; with ! go until another ! or eol.  Strings start and end with " as
  ;; usual but cannot span lines so they are terminated by eol.  This fcn
  ;; returns t if either a comment or string is found, nil if neither is
  ;; found. match-data 1&2 are set for comments, 3&4 are set for a normal
  ;; string, 5&6 are set for eol-terminated strings.  Where the match pair
  ;; mark the start character and end character respectively.  Point is
  ;; left at the end of the match or unchanged if no match.
  (when (re-search-forward "\\(?:--\\|!\\|\"\\)" search-limit t)
    ;; either a comment or string was found
    (let ((start (match-data))
          (match (match-string-no-properties 0))
          end)
      ;; see if it was a string
      (if (equal "\"" match)
          ;; If this 'when' fails no search data will be set.  The result
          ;; is the original search above which is a shy group and so no
          ;; sub-group matches return.  Result is, nothing gets marked.
          ;; Since point is left after original match a loop won't occur.
          (when (re-search-forward "\\(?:$\\|\"\\)" search-limit t)
            (setq end (list (car (match-data))
                            (copy-marker (1+(car (match-data))))))
            (if (equal "\"" (match-string-no-properties 0))
                (set-match-data
                 `(,(car start) ,(cadr end)       ;match-string 0
                   nil nil nil nil                ;match-string 1&2 not found
                   ,@start ,@end))                ;match-string 3&4
              (set-match-data
               `(,(car start) ,(cadr end)         ;match-string 0
                 nil nil nil nil                  ;match-string 1&2 not found
                 nil nil nil nil                  ;match-string 3&4 not found
                 ,@start ,@end))))                ;5&6 is eol terminated quote
        ;; Must be a comment.  Determine type.
        (if (equal "!" match)
            (setq match "\\(?:\n\\|!\\|\\'\\)") ;! can delimit as well as eol/eob
          ;; for -- comment only 1'st char should get < syntax
            (setq start (list (car start)(copy-marker (1+(car start)))))
            (setq match "\\(?:\n\\|\\'\\)")) ;only eol or eob delimits
        ;;see above 'when' comment
        (when (re-search-forward match search-limit t)
          (setq end (match-data))
          (set-match-data
           `(,(car start) ,(cadr end)           ;match-string 0
             ,@start ,@end)))))                 ;match-string 1&2
    ;; insure t returns if a string or comment was found. nil returns by default.
    t))

(defun tal-find-end-keywords (search-limit)
  ;; Something as seemingly simple as "any occurrence of END not followed
  ;; by BLOCK" is not easy to do with a regexp.  A function makes it easier.
  ;; This accounts for tal-begin-matches-semi and returns one of the following:
  ;; match-string 1 for END or END;
  ;; match-string 2 for END BLOCK or END BLOCK;
  ;; match-string 3 for END PROCPTR or END PROCPTR;
  (when (re-search-forward
           "\\(?:\\s-\\|^\\)en\\(d\\)\\(?:\\(;\\)\\|#\\|$\\|\\s-\\)"
           search-limit t)
    (let* ((data (match-data)))
      (goto-char (nth 3 data))          ;after d of end
      (if tal-begin-matches-semi
          (cond
           ((looking-at "\\s-+block\\(;\\)")
            (set-match-data
             (list (nth 0 data) (nth 1 (match-data))
                   nil nil              ;just match 2 data
                   (nth 2 (match-data)) (nth 3 (match-data)))))
           ((looking-at "\\s-+procptr\\(;\\)")
            (set-match-data
             (list (nth 0 data) (nth 1 (match-data))
                   nil nil nil nil      ;just match 3 data
                   (nth 2 (match-data)) (nth 3 (match-data)))))
           (t (if (nth 4 data)
                  (set-match-data       ;just match 1 data
                   (list (nth 0 data) (nth 1 data)
                         (nth 4 data) (nth 5 data)))
                (set-match-data data)))) ;already only match 1
        (cond
         ((looking-at "\\s-+bloc\\(k\\);")
          (set-match-data
           (list (nth 0 data) (nth 1 (match-data))
                 nil nil                ;just match 2 data
                 (nth 2 (match-data)) (nth 3 (match-data)))))
         ((looking-at "\\s-+procpt\\(r\\);")
          (set-match-data
           (list (nth 0 data) (nth 1 (match-data))
                 nil nil nil nil        ;just match 3 data
                 (nth 2 (match-data)) (nth 3 (match-data)))))
         (t (set-match-data                  ;just match 1 data
             (list (nth 0 data) (nth 1 data) ;tosses any match 2
                   (nth 2 data) (nth 3 data)))))))
    t))


(defvar tal-static-font-lock-keywords
  ;; font-lock-keywords is a symbol or list of symbols yielding the keywords to
  ;; be fontified.  Keywords are listed here using either (MATCHER . FACENAME)
  ;; or (MATCHER . (MATCH FACENAME)) syntax.  Other options are available but
  ;; not used here.  For simplicity, all regexp's were designed so MATCH would
  ;; be 1.  Nothing forced this but to me it makes debug/maintenance easier.
  `(
    ; this is necessary because b and d in these words are not word
    ; constituent characters, they are ( ) syntax.
    (,(concat "\\(?:^\\|\\W\\)\\(\\(?:end\\s-+\\(?:block\\|procptr\\)\\)"
             "\\|block\\|define\\|procptr\\)\\(?:\\'\\|$\\|\\W\\)")
     1 font-lock-type-face)
    ("\\(?:^\\|\\W\\)\\(begin\\|end\\)\\(?:\\'\\|$\\|\\W\\)"
     1 font-lock-keyword-face)
    (,(tal-keyword-anywhere-regexp (append tal-keywords-unqualified-data-types
                                           tal-keywords-address-types))
     1 font-lock-type-face)
    (, (tal-keyword-qualified-regexp tal-keywords-qualified-data-types)
     1 font-lock-type-face)
    (, (tal-keyword-qualified-regexp tal-keywords-qualified-data-types) .
     (2 font-lock-type-face t t))
    (,(tal-keyword-on-directive-line-regexp tal-keywords-directives)
     1 font-lock-builtin-face)
    (,(tal-keyword-anywhere-regexp tal-keywords-builtin)
     1 font-lock-builtin-face)
    (,(tal-keyword-anywhere-regexp (append tal-keywords-std-fcns
                                           tal-keywords-statements))
     1 font-lock-keyword-face)
    (,(tal-keyword-anywhere-regexp tal-keywords-nonreserved)
     1 font-lock-variable-name-face)
    (,(tal-keyword-anywhere-regexp (append tal-keywords-deprecated
                                           tal-keywords-privileged))
     1 font-lock-warning-face)
    (,tal-keyword-fcn-names-regexp 2 font-lock-function-name-face)
   ))

(defvar tal-font-lock-keywords ())

(defun tal-build-font-lock-keywords ()
  "Used to create `font-lock-keywords' based on current customize settings."
  (append tal-static-font-lock-keywords
          `(,(when tal-primecode-warning
               ;; ]a  ]d or ]e cannot appear in col 1-2 if using PrimeCode.
               '("^\\][ade]" . font-lock-warning-face)))))

(defvar tal-font-lock-syntactic-keywords
 '(
   ;; tal-find-syntactic-keywords returns matches 1&2 for comments, 3&4
   ;; for strings.  5&6 for eol terminated strings.  I must use "|"(15)
   ;; rather than "\""(7) for eol terminated strings because the begin
   ;; and end characters must be the same when "\""(7) is used.
   (tal-find-syntactic-keywords (1 "<"  t t) (2 ">"  t t)
                                (3 "\"" t t) (4 "\"" t t)
                                (5 "|"  t t) (6 "|"  t t))
   ("\\(?:^\\|\\s-\\)\\(d\\)efine\\(\\s-\\|$\\)"     (1 (4 . 1004)))
   ("\\(?:^\\|\\s-\\)\\(p\\)rocptr\\(\\s-\\|$\\)"    (1 (4 . 1003)))
   ("\\(?:^\\|\\s-\\)\\(b\\)lock\\(\\s-\\|$\\)"      (1 (4 . 1002)))
   ("\\(?:^\\|\\s-\\)\\(b\\)egin\\(:\\|\\s-\\|$\\)"  (1 (4 . 1001)))
   ;; The K of END BLOCK, R of END PROCPTR or the D of END is a close paren
   ;; unless tal-begin-matches-semi is true then a ; after would be a paren.
   (tal-find-end-keywords      (1 (5 . 1001) nil t)
                               (2 (5 . 1002) nil t)
                               (3 (5 . 1003) nil t))
   ("#\\(;\\)" (1 (5 . 1004))))
 "A list of regexp's or functions.  Used to add syntax-table properties to
characters that can't be set by the syntax-table alone.")

(defun tal-syntactic-face-function (state)
  "Determines which face should highlight a particular comment or string.
See `font-lock-syntactic-face-function'"
  (let ((str (nth 3 state)))
    (cond ((null str) font-lock-comment-face)
          ((eq str t) font-lock-warning-face)
          (t font-lock-string-face))))

(defun tal-setup-font-lock ()
  "Sets up the buffer local value for font-lock-defaults and optionally
turns on font-lock-mode"
  ;; I use font-lock-syntactic-keywords to set some properties and I
  ;; don't want them ignored.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;; I really can't imagine anyone wanting this off in TAL.  It would force you
  ;; never to use the words begin or end in a comment unless you balanced them.
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; This allows column markers to be different in seperate tal-mode buffers.
  (set (make-local-variable 'tal-font-lock-keywords)
       (tal-build-font-lock-keywords))
  ;; This is where all the font-lock stuff actually gets set up.  Once
  ;; font-lock-defaults has it's value, setting font-lock-mode true should
  ;; cause all your syntax highlighting dreams to come true.
  (setq font-lock-defaults
         ;; The first value is all the keyword expressions.
       '(tal-font-lock-keywords
         ;; keywords-only means no strings or comments get fontified
         nil
         ;; case-fold (ignore case)
         t
         ;; syntax-alist, dollar sign and period need to be 'word characters'
         ((?\$ . "w")(?\. . "w"))
         ;; syntax-begin - no function defined to move outside syntactic block
         nil
         ;; font-lock-syntactic-keywords
         ;; takes (matcher (match syntax override lexmatch) ...)...
         (font-lock-syntactic-keywords . tal-font-lock-syntactic-keywords )
         ;; font-lock-syntactic-face-function names a function that tells
         ;; what face to use for specific strings or comments.
         (font-lock-syntactic-face-function . tal-syntactic-face-function)))
  ; font lock is turned on by default in this mode. Use customize to disable.
  (when tal-font-lock-always (font-lock-mode t)))

;;; Static Column Markers

(defcustom tal-column-marker-1 0
  "*Turns on column-marker-1 (which see) at the specified column.
Use of this feature requires the column-marker.el package be loaded or on
the search list."
  :type 'integer
  :group 'tal)
(make-variable-buffer-local 'tal-column-marker-1)

(defcustom tal-column-marker-2 79
  "*Turns on column-marker-2 (which see) at the specified column.
Use of this feature requires the column-marker.el package."
  :type 'integer
  :group 'tal)
(make-variable-buffer-local 'tal-column-marker-2)

(defun tal-setup-column-markers ()
  "Turns on column markers if configured and available.
See `tal-column-marker-1' and `tal-column-marker-2' "
  (if (condition-case ()
          (progn (require 'column-marker) nil)
        (error t))
      (if (not (and (zerop tal-column-marker-1)
                    (zerop tal-column-marker-2)))
          (message "column markers are configured but %s"
                   " column-marker feature not available."))
    (setq indent-tabs-mode nil)      ;documented as buffer local
    (column-marker-1 tal-column-marker-1)
    (column-marker-2 tal-column-marker-2)))

;;; Imenu & Which-function

(defcustom tal-imenu-menubar t
  "If not nil, `imenu-add-to-menubar' is called during mode initialization.
This adds a [Menu name] menu to your menu bar.  By default the menu contains a
list of all procedures, sections and pages in your program.  You can go
directly to any item on the menu by selecting it.  You can control what
appears on this menu by modifying `tal-imenu-expression-alist'.  You must turn
imenu on for this to work.  See `imenu' in the Emacs reference manual for more
information.  Personally I recommend customizing `imenu-sort-function' to sort
by name."
  :type  '(choice :tag "Menu Name"
                  (const  :tag "No Menu" nil)
                  (const  :tag "Name=Index" t)
                  (string :tag "Menu Name"))
  :group 'tal)

(defvar tal-imenu-syntax-alist ()
  "Overrides to `tal-mode-syntax-table' used during `imenu-generic-expression' search."
  ;;AFAIK there are no character adjustments needed during imenu search.
)

(defcustom tal-imenu-expression-alist
  '((nil         "^\\(?:\\w+\\(?:\\s-*(\\w+)\\)?\\s-+\\)?proc\\s-+\\(\\w+\\)\\b" 1)
    ("?Sections" "^\\?section\\s-+\\(\\w+\\)\\b"                                 1)
    ("?Pages"    "^\\?page\\s-+\"\\s-*\\(.+?\\)\""                               1)
    ;; When subprocs are 'on' imenu sees all proc code as belonging to the
    ;; last subproc defined:-(  Moreover, there is no way to tell same-name
    ;; subprocs defined in different procs apart.
    ;;("SubProc"  "^\\(?:\\w+\\(?:\\s-*(\\w+)\\)?\\s-+\\)?subproc\\s-+\\(\\w+\\)\\b" 1)
  )
  "A list of regular expressions for creating an `imenu' index.

Each element has the form (list-name regexp num).

Where list-name is the name of the submenu under which items matching regexp
are found and num is the expression index defining the label to use for the
submenu entry.  When num = 0 the entire matching regexp text appears under
list-name.  When list-name is nil the matching entries appear in the root
imenu list rather than in a submenu.  See also `tal-imenu-menubar'"
  :type '(repeat (list (choice :tag "Submenu Name" string (const nil))
                       regexp (integer :tag "Regexp index")))
  :group 'tal)

(defcustom tal-display-which-function t
  "This option turns `which-func' on for all `tal-mode' buffers.
`which-func' is a package that causes the current function, section or
page to be displayed on the mode line.  `which-func' uses `imenu'.  Also
see `tal-imenu-expression-alist' for more information."
  :type 'boolean
  :group 'tal)

(defun tal-setup-imenu ()
  "Installs tal-imenu-generic-expression & tal-imenu-syntax-alist."
  ;; imenu doc says these 3 are buffer-local by default
  (setq imenu-generic-expression tal-imenu-expression-alist)
  (setq imenu-syntax-alist tal-imenu-syntax-alist)
  (setq imenu-case-fold-search t) ;TAL/pTAL are never case sensitive
  (when tal-imenu-menubar
    (if (condition-case ()
            (progn (require 'imenu) t)
          (error nil))
        (if (and (stringp tal-imenu-menubar)(not(equal tal-imenu-menubar "")))
            (imenu-add-to-menubar tal-imenu-menubar)
          (imenu-add-menubar-index))
      (message "tal-imenu-menubar is set but imenu feature not available.")))
  (when tal-display-which-function
    (if (condition-case ()
            (progn (require 'which-func) t)
          (error nil))
        (which-function-mode t)
      (message "tal-display-which-function set but which-func not available"))))

;;; Adaptive-fill / auto-fill (needs much work but it's a start)

(defcustom tal-restrict-auto-fill t
  "When not nil a buffer local value for `fill-nobreak-predicate' is created
to prevent code from being accidentally realligned.  The function uses syntax
highlighting to detect comments so `font-lock-mode' must be enabled to work."
  :type 'boolean
  :group 'tal)

(defun tal-setup-adaptive-fill ()
  "Sets up the TAL-MODE adaptive-fill variables."
  (set (make-local-variable 'fill-individual-varying-indent)
       nil)
  (set (make-local-variable 'auto-fill-inhibit-regexp)
       "\\s-*[^!-]")
  (set (make-local-variable 'comment-use-syntax)
       t)
  ;; I don't like always using -- but ! can't be used reliably since it will
  ;; uncomment existing ! comments in the region being commented.
  (set (make-local-variable 'comment-start)
       "--")
  (set (make-local-variable 'comment-end)
       "")
  (set (make-local-variable 'comment-padding)
       "")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\s<\\|--\\)\\s-*")
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
  (when tal-restrict-auto-fill
    ; This is supposed to restrict auto-fill to comments only
    (fset (make-local-variable 'fill-nobreak-predicate)
          (lambda ()
            (not (eq (get-text-property (point) 'face)
                     'font-lock-comment-face))))))

;;; Indentation

(defun tal-setup-indent ()
  "Sets default indentation or sets up tal-indent if available."
  (if (condition-case ()
          (progn (require 'tal-indent) t)
        (error nil))
      (set (make-local-variable 'indent-line-function) 'tal-indent-line)
    (set (make-local-variable 'indent-line-function) 'indent-relative-maybe)))

;;; Language Skeletons -- Feel free to add more of your own!

(defcustom tal-keywords-case 'camel
  "*Indicates if keywords in skeletons should be all UPPER CASE, all lower
case or Camel Case (First Char Upper & Rest Lower)."
  :type  '(choice (const :tag "ALL CAPS"     upper)
                  (const :tag "all small"    lower)
                  (const :tag "Camel Case"   camel)
                  (const :tag "DON'T Change" nil))
  :group 'tal)

(defun tal-setup-skel ()
  "Configures skeleton.el functions for the TAL/pTAL environemnt."
  (set (make-local-variable 'skeleton-transformation) 'tal-skel-transform)
  ;; This prevents abbrevs from expanding within skeletons
  (setq skeleton-further-elements '((abbrev-mode nil))))

(defun tal-skel-transform ( element )
  "Called by `skeleton-insert'.  Gives ELEMENT `tal-keywords-case' capitalization."
  ;; This should be made more complex to only change the case of certain words
  ;; so the user can create skeletons containing items that should not be
  ;; affected by tal-keywords-case.  There are 3 obvious ways.  1) use the
  ;; keywords tables above. 2) add a customize to ignore words. 3) add a
  ;; customize to specify specific words to be affected.  Preferences?
  (if (stringp element)
    (cond
     ((eq tal-keywords-case 'upper) (upcase element))
     ((eq tal-keywords-case 'lower) (downcase element))
     ((eq tal-keywords-case 'camel) (capitalize element))
     ( t                             element            ))
    element))

(defun tal-set-line-syntax ()
  "Applies font-lock-syntactic-keywords to current line.
Used to set properties necessary for proper indentation."
  (if font-lock-mode
      (save-excursion    ; next stmt moves point.
        (font-lock-fontify-syntactic-keywords-region
         (line-beginning-position) (line-end-position))
        ()     ;any result is inserted into buffer
      )))

(define-skeleton tal-if-skel
  "Inserts a standard TAL/pTAL if/then statement skeleton."
  nil                                                >
  "IF (" _ ") THEN"                                  > \n
  "BEGIN"                 (tal-set-line-syntax)      > \n
  _                                                & > \n
  "END\;"                 (tal-set-line-syntax)      > \n)

(define-skeleton tal-begin-end-skel
  "Inserts a begin/end pair.  Useful for an abbrev."
  nil                                                >
  "BEGIN"                 (tal-set-line-syntax)      > \n
  _                                                & > \n
  "END\;"                 (tal-set-line-syntax)      > \n)

(define-skeleton tal-begin-end2-skel
  "Inserts a begin/end pair.  Useful for an abbrev."
  nil                                                >
  "BEGIN"                 (tal-set-line-syntax)      > \n
  _                                                & > \n
  "END"                   (tal-set-line-syntax)      > \n)

(define-skeleton tal-else-begin-skel
  "Inserts a begin/end pair.  Useful for an abbrev."
  nil                                                >
  "ELSE"                  (tal-set-line-syntax)      > \n
  "BEGIN"                 (tal-set-line-syntax)      > \n
  _                                                & > \n
  "END\;"                 (tal-set-line-syntax)      > \n)

(define-skeleton tal-if-else-skel
  "Inserts a standard TAL/pTAL if/then statement skeleton."
  nil                                                >
  "IF (" _ ") THEN"                                  > \n
  "BEGIN"                 (tal-set-line-syntax)      > \n
  _                                                & > \n
  "END"                   (tal-set-line-syntax)      > \n
  "ELSE"                  (tal-set-line-syntax)      > \n
  "BEGIN"                 (tal-set-line-syntax)      > \n
  _                                                & > \n
  "END\;"                 (tal-set-line-syntax)      > \n)

(define-skeleton tal-case-skel
  "Inserts a standard TAL/pTAL Labled Case -> statement skeleton."
  nil                                                >
  "CASE (" _ ") OF"       (tal-set-line-syntax)      > \n
  "BEGIN"                 (tal-set-line-syntax)      > \n
  "  -> " _ "\;"                                     > \n
  "OTHERWISE -> \;  "                                > \n
  "END\;"                 (tal-set-line-syntax)      > \n)

(define-skeleton tal-proc-skel
  "An example of a proc skeleton.  It prompts for the procedure name.
If any text is selected it is used for the description text."
  "Procedure Name: "
  "?page \"" str | - "\""                                                   > \n
  "!***********************************************************************!" \n
  "!*                                                                     *!" \n
  "!* Procedure: " str (make-string (- 57 (length str)) ? )              "*!" \n
  "!*                                                                     *!" \n
  "!* Description:                                                        *!" \n
  "!*                                                                     *!" \n
  "!*   " _  (make-string (max 0 (- 71 (current-column)))? )             "*!" \n
  "!*                                                                     *!" \n
  "!***********************************************************************!" \n
  "Int Proc " str "( " _ " );"                                              > \n
  "BEGIN"                                          (tal-set-line-syntax)    > \n
                                                                               \n
  "END\;  ! Proc " str " !"                        (tal-set-line-syntax)    > \n
  '(overwrite-mode t))

;;; Abbrev support

(defcustom tal-abbrev-mode t
  "Sets the default value for `abbrev-mode' upon entry into `tal-mode'."
  :type 'boolean
  :group 'tal)

(defvar tal-mode-abbrev-table-list
  '(("$b" "" tal-begin-end-skel)
    ("$i" "" tal-if-skel)
    ("$e" "" tal-if-else-skel)
    ("$c" "" tal-case-skel)
    ("$p" "" tal-proc-skel))
  "List of pre-defined `tal-mode' abbrev definitions.
Use \\[list-abbrevs] to see all defined abbrevs.")

(defvar tal-mode-abbrev-table)

(defun tal-setup-abbrevs ()
  "Installs the `tal-mode-abbrev-table' as `local-abbrev-table'"
  (define-abbrev-table 'tal-mode-abbrev-table tal-mode-abbrev-table-list)
  (setq local-abbrev-table tal-mode-abbrev-table)
  (setq skeleton-further-elements '((abbrev-mode nil)))
  (abbrev-mode tal-abbrev-mode)    ;Setting is documented as buffer local
)

;;; Eldoc support

(defcustom tal-eldoc-ignore-comments t
  "`tal-eldoc-scan-buffer' and `tal-eldoc-make-list' won't include
comments in generated eldoc entries when non-nil.  For some reason, turning
this on takes much more CPU than I can account for but it works."
  :type 'boolean
  :group 'tal)

(defcustom tal-eldoc-def-files ()
  "List of files containing function help strings used by `eldoc-mode'.
These are the strings eldoc-mode displays as help for functions near point.
The format of the file must be exactly as follows or who knows what happens.

   (set (intern \"<fcn-name1>\" tal-eldoc-obarray) <helper string1>)
   (set (intern \"<fcn-name2>\" tal-eldoc-obarray) <helper string2>)
...

Where <fcn-name> is the name of the function to which <helper string> applies.
      <helper-string> is the string to display when point is near <fcn-name>.
      for example \"fcn-name ( param1, param2 )\"
      Alternatively <helper-string> can be a list where the first element is
      the help string mentioned above and the second element is a string
      containing the filename of the file where <fcn-name> is defined.
      For example `(\"Help string here\" \"source\\of_help.here\")
      When present `tal-eldoc-where-def' and `tal-eldoc-visit-file' use it.

See `tal-eldoc-make-list' for a way to automate creation of such a file."
  :type '(repeat string)
  :group 'tal)

(defcustom tal-eldoc-function-name-regexp
  "\\(^\\w+\\(?:\\s-*(\\w+)\\)?\\s-+\\|^\\s-*\\)proc\\s-+\\(\\w+\\)\\(\\s-\\|(\\|;\\|$\\)"
  "Set this to a regexp that recognizes procedure declarations.
Used by `tal-eldoc-scan-buffer and `tal-eldoc-make-list'.
Where (match-beginning 1) must mark the start of the proc delaration to be
used as the start of the help string.  (match-string 2) must match the name
of the procedure exactly.  If the first non-whitespace character
after (match-beginning 3) is `(' then these functions find the matching `)'
and used it as the end of the help string otherwise (match-end 3) is used as
the end of the help string."
  :type 'string
  :group 'tal)

(defvar tal-eldoc-obarray ()
  "Keywords and their associated help strings stored here.")

(defun tal-eldoc-context ( &optional search )
  "Retrieves eldoc data concerning (`thing-at-point' 'symbol).
A list containing the keyword at point, it's associated symbol, the doc
string, the file where the doc string is defined, possibly more data tbd.
If no doc entry is found nil is returned.  If SEARCH is non-nil, also
look outside enclosing parens if necessary to find a documented word."
  (let* ((word (thing-at-point 'symbol))
        (symbol (if word (intern-soft (upcase word) tal-eldoc-obarray))))
    ;; if thing-at-point has a doc string, return where defined.
    (if symbol
        (if (stringp (symbol-value symbol))
            (list word symbol (symbol-value symbol))
          `(,word ,symbol ,@(symbol-value symbol)))
      ;; none found, search outside enclosing parens if asked
      (if search
          (save-excursion
            (when (and (condition-case ()
                           (or (backward-up-list) t)
                         (error nil))
                       (eq ?\( (char-after)))
              (skip-syntax-backward "-")
              (tal-eldoc-context)))))))

(defun tal-eldoc-function ()
  "Returns a documentation string appropriate for the current context or nil."
  (let ((begin (point))
        (count -1)
        (help-string (nth 2 (tal-eldoc-context))))
    ;; if thing-at-point has a doc string, return it.
    (if help-string
        help-string
      ;; Otherwise see if we're within (parens) and if so, count the
      ;; number of commas between point and the open paren.
      (save-excursion
        (when (and (condition-case ()
                       (or (backward-up-list) t)
                     (error nil))
                   (eq ?\( (char-after)))
          (skip-syntax-backward "-")
          (if (setq help-string (copy-sequence
                                 (nth 2 (tal-eldoc-context))))
              (while (progn (setq count (1+ count))
                            (search-forward "," begin t))))))
      ;; if the doc string has an open paren, highlight the word (if
      ;; any) that follows the same number of comma's counted above.
      (when (and help-string
                 (> count -1)
                 (string-match
                  "(" help-string
                  ;; this tries to skip parens from int(32) type
                  ;; proc/subproc declarations.
                  (string-match ")\\s-\\(sub\\)?proc\\b" help-string)))
        (while (and (> count 0)
                    (string-match "," help-string (match-end 0))
                    (setq count (1- count))))
        (when (= count 0)
          (if (string-match "\\w+\\(:\\w+\\)?" help-string (match-end 0))
              (put-text-property (match-beginning 0) (match-end 0)
                                 'face 'bold help-string))))
        help-string)))

(defun tal-code-only-text ( str )
  "Returns STR with comments replaced by spaces.
This should handle strings containing multiple lines correctly."
  (let (eol match end (offset 0))
    (while (and (< offset (length str)) ;find comment starters
                (setq offset (string-match "\\(--\\|!\\)" str offset)))
      (setq match (match-string 0 str)  ;save type of comment found
            end (match-end 0))          ;offset of 1st char after match
      (cond ((string= match "--")       ;only eol terminates this type
             (setq str (if (not (string-match "\n" str end))
                           (substring str 0 offset) ;truncate line
                         (setq end (match-end 0))   ;replace with blanks
                         (concat (substring str 0 offset)
                                 (make-string (- end offset) ? )
                                 (substring str end))) ;either way end points after removed text
             ))
            ((string= match "!")        ;another ! or eol terminates
             (setq str (if (not (string-match "\\(!\\|\n\\)" str end))
                           (substring str 0 offset) ;truncate line
                         (setq end (match-end 0))   ;replace with blanks
                         (concat (substring str 0 offset)
                                 (make-string (- end offset) ? )
                                 (substring str end))) ;either way end points after removed text
             ))))                       ;end while
  )                                     ;end let
  str)                                  ;return result

(defun tal-eldoc-scan-buffer ()
  "Updates in-memory table of eldoc help strings.
Run this from within your tal source file to add it's function
declarations to the memory table of tal eldoc help strings.  See also
`tal-eldoc-make-list'."
  (interactive)
  (if (not (eq 'tal-mode major-mode))
      (error "Buffer is not tal-mode!  Can't extract tal symbols"))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward tal-eldoc-function-name-regexp () t)
      (let ((start (match-beginning 1)) ;usually BOL of decl line
            (name (match-string 2))     ;function name must be match #2
            (end (match-end 3))         ;end of decl or start of arg list
            result)
        (goto-char (match-beginning 3))
        (if (looking-at "\\s-*(")
            (forward-sexp)              ;skip over arg list if it exists
          (goto-char end)               ;no args or regexp consumed them
        )
        (setq result (buffer-substring-no-properties start (point)))
        (if tal-eldoc-ignore-comments
            (setq result (tal-code-only-text result)))
        (setq result (mapconcat 'eval (split-string result) " "))
        (if (buffer-file-name)
            (setq result (list result (buffer-file-name))))
        (set (intern (upcase name) tal-eldoc-obarray) result)))))

(defun tal-eldoc-make-list ()
  "Creates a buffer of eldoc help strings for functions in this buffer.
Run this while visiting a TAL/pTAL source or external declaration file to
create a buffer of eldoc help entries.  Save this buffer somewhere on
your search path and see `tal-eldoc-def-files'.  See also
`tal-eldoc-scan-buffer'."
  (interactive)
  (if (not (eq 'tal-mode major-mode))
      (error "Buffer is not tal-mode!  Can't extract tal symbols"))
  (let ((new-buf (get-buffer-create "tal-eldoc-list.el"))
        (this-buf (buffer-file-name)))
    (if this-buf
      (with-current-buffer new-buf
        (insert (concat ";; from " this-buf "\n"))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward tal-eldoc-function-name-regexp () t)
        (let ((start (match-beginning 1)) ;usually BOL of decl line
              (name (match-string 2))     ;function name must be match #2
              (end (match-end 3))         ;end of decl or start of arg list
              result)
          (goto-char (match-beginning 3))
          (if (looking-at "\\s-*(")
              (forward-sexp)            ;skip over arg list if it exists
            (goto-char end)             ;no args or regexp consumed them
          )
          (setq result (split-string    ;list of words comprising decl
                        (tal-code-only-text ;insure no comments in list
                         (buffer-substring-no-properties start (point)))))
          (setq result (mapconcat 'eval result " "))
          (if (buffer-file-name)
              (setq result (concat "(set (intern \"" (upcase name)
                                   "\" tal-eldoc-obarray) '(\"" result "\" \""
                                   (buffer-file-name) "\"))\n"))
              (setq result (concat "(set (intern \"" (upcase name)
                                   "\" tal-eldoc-obarray) \"" result "\")\n")))
          (with-current-buffer new-buf
            (insert result)))))
    (switch-to-buffer new-buf)
    (emacs-lisp-mode)))

(defun tal-eldoc-where-def ()
  "Displays the filename from which current eldoc string was extracted."
  (interactive)
  (let* ((context (tal-eldoc-context t))
         (fname (nth 3 context)))
    (cond
     ((null context) (message "No eldoc info for context"))
     ((null fname)   (message "Filename not available"))
     (t              (message "%s" fname)))))

(defun tal-eldoc-visit-file ()
  "Visits the file from which current eldoc string was extracted."
  (interactive)
  (let* ((context (tal-eldoc-context t))
         (fname (nth 3 context))
         (word (car context)))
    (cond
     ((null context) (message "No eldoc info for context"))
     ((null fname)   (message "Filename not available"))
     (t (find-file fname)
        (message "Searching for proc %s" word)
        (goto-char (point-min))
        (re-search-forward (concat "proc\\s-*" word))
        (unless (eq major-mode 'tal-mode) (tal-mode))))))

(defun tal-setup-eldoc ()
  "Loads the function documentation for use with eldoc."
  (set (make-local-variable 'eldoc-documentation-function)
       'tal-eldoc-function)
  (unless (vectorp tal-eldoc-obarray)
    (setq tal-eldoc-obarray (make-vector 41 0))
    (condition-case var (mapc 'load tal-eldoc-def-files)
      (error (message "*** ERROR *** %s" var))))
  (tal-eldoc-scan-buffer))

;;; Movement by ...

;(defvar tal-outline-regexp
;...)

;;; Miscellaneous


;;;###autoload
(defun tal-mode ()
  "A major mode for editing TAL/pTAL language program source files.
Customization options are available via
\\[customize-group] <ret> TAL <ret>

This mode provides TAL specific support for such packages as:
    `font-lock-mode'      `show-paren-mode'     `imenu'
    `which-function'      `skeleton-insert'     `auto-fill-mode'
    `adaptive-fill-mode'  `filladapt-mode'      `eldoc-mode'
    `abbrev-mode'

** Note ** Many things won't work correctly if `font-lock-mode' is off.

tal-mode also implements the following \\[execute-extended-command] ... commands

`tal-mode'              Activates this mode for the current buffer
`tal-begin-end-skel'    Inserts a Begin/End skeleton
`tal-case-skel'         Inserts a labeled case statement skeleton
`tal-if-skel'           Inserts an if/then statement skeleton
`tal-if-else-skel'      Inserts an if/then/else statement skeleton
`tal-proc-skel'         Example of a skeleton procedure
`tal-eldoc-make-list'   Creates eldoc help file from current buffer
`tal-eldoc-scan-buffer' Adds eldoc entries of current buffer
`tal-eldoc-where-def'   Shows where eldoc entry displayed was defined
`tal-eldoc-visit-file'  Visits file where eldoc displayed was defined

\\{tal-mode-map}
Use \\[describe-bindings] to see ALL key bindings.

Some settings I like:
Turn on `skeleton-pair-insert-maybe' for (), [] and \"\"
Turn on `imenu' and set `imenu-sort-function' to imenu--sort-by-name
Turn on `eldoc-mode'.  See `tal-eldoc-make-list' for building TAL help
Turn on `recentf-mode'. You might need `recentf-auto-cleanup' = 'never
Set `column-marker-1' to 79 so you can tell what TEDIT users can't see.
Load `popup-ruler' for a TEDIT F9 type ruler on steroids.
I find `transient-mark-mode' totally indispensable.
CUA mode has some really great rectangle functions."
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'major-mode) 'tal-mode)
  (set (make-local-variable 'mode-name) "TAL")
  (set (make-local-variable 'make-backup-files) nil) ;necessary for now
  (use-local-map tal-mode-map)
  (set-syntax-table tal-mode-syntax-table)
  (tal-setup-menu)
  (tal-setup-font-lock)
  (tal-setup-adaptive-fill)
  (tal-setup-abbrevs)
  (tal-setup-imenu)
  (tal-setup-eldoc)
  (tal-setup-indent)
  (tal-setup-skel)
  (tal-setup-column-markers)
;  (set (make-local-variable 'outline-regexp) tal-outline-regexp)
  (show-paren-mode 1)
  (run-mode-hooks 'tal-mode-hook))

(provide 'tal-mode)

;;; tal-mode.el ends here

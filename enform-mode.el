;;; enform-mode.el --- Handles the Tandem/NSK/Guardian ENFORM report language.
;;                   A proprietary language of Tandem/Compaq/HP computers.

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Rick Bielawski <rbielaws@i1.net>
;; Keywords: languages, extensions, Tandem, Guardian, NSK, ENFORM
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
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; ENFORM is Tandem's Report Language.

;; Keywords as of "Third Edition - July 1991" are recognized.
;; See source file comments for installation instructions.

;;; ToDo:

;; Unterminated strings are currently terminated by EOL.  This makes viewing
;;   mangled code easier but finding unterminated strings harder.  Eventually
;;   I'd like to toggle this on/off.
;; Indentation support would be nice as would illegal syntax warnings.

;;; Installing:

;; Before you can use enform-mode, emacs needs to be able to find it.  Place the
;; enform-mode.el file in a directory on the load-path; typically the
;; .../site-lisp or perhaps .../lisp/progmods directory.  Usually you would
;; also want to byte compile enform-mode.el but this is not required.  To do
;; this, visit the enform-mode.el file, type: M-x emacs-lisp-byte-compile <ret>
;; There should be no warnings or errors during byte compilation.
;;
;; There are 4 basic ways to use ENFORM-MODE on a file.  The first method
;; manually selects enform-mode as the editing mode.  The other 3 cause emacs to
;; recognize automatically that you want to visit the file using enform-mode.
;;
;; Pick one:
;; 1. While visiting a file, type: M-x enform-mode <ret>
;; 2. Put the string -*-enform-*- in a comment on the first line of the file.
;;    Save the file and close it.  Now any time you open it enform-mode starts.
;; 3. Create an association between a particular file naming convention and
;;    enform-mode.  This is done by adding an association to auto-mode-alist.
;; For example:
;; (setq auto-mode-alist
;;   (append
;;     '(("\\.tal\\'" . enform-mode)         ;extension of .tal means enform-mode
;;       ("\\([\\/]\\|^\\)[^.]+$" . enform-mode)) ;so does no extension at all.
;;    auto-mode-alist))
;; 4. Advise set-auto-mode to look at the buffer contents upon loading.
;;
;; The above all tell emacs that you want to use enform-mode but you must load
;; enform-mode before you can use it.  There are 2 methods of telling emacs to
;; load the enform-mode routines.  The first unconditionally loads enform-mode
;; definitions immediately.  The second tells emacs to automatically load
;; enform-mode only when you try to use it.  Add one of the following lines to
;; your .emacs file.
;;
;;(require 'enform-mode)      ; Unconditional load
;;(autoload 'enform-mode "enform-mode" "Major mode for Tandem ENFORM files." t nil)
;;
;; Please report any bugs!

;;; History:

;; 2005-09-15 RGB Quickly thrown together using TAL mode as a start.
;; 2005-09-15 RGB Tweak to syntax table for string handling.
;; 2005-10-04 RGB Fix matching paren chars for {} in syntax table.

;;; Code:

(defgroup enform nil
  "Major mode for editing Tandem ENFORM source files in Emacs.
While in enform-mode use C-h m for a description of the mode's features."
  :prefix 'enform-
  :group 'languages)

;;; KEY MAP

(defvar enform-skeleton-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?e]   'enform-if-else-skel)
    (define-key map [?i]   'enform-if-skel)
    map)
  "Keymap for `enform-mode' skeletons.")

(defvar enform-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab]           'indent-according-to-mode)
    (define-key map [?\C-c ?\C-f]   'auto-fill-mode)
    (define-key map [?\C-c ?\C-r]   'enform-popup-ruler)
    (define-key map [?\C-c ?\C-s]    enform-skeleton-map)
    (define-key map [?\C-c return]  'comment-indent-new-line)
    map)
  "Keymap for `enform-mode'.")

(defvar enform-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n "-" st)
    (modify-syntax-entry ?\! "." st)
;    (modify-syntax-entry ?\" "." st)
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
    (modify-syntax-entry ?\- "w" st)
    (modify-syntax-entry ?\. "'" st)
    (modify-syntax-entry ?\/ "." st)
    (modify-syntax-entry ?\: "." st)
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\< "." st)
    (modify-syntax-entry ?\= "." st)
    (modify-syntax-entry ?\> "." st)
    (modify-syntax-entry ?\? "." st)
    (modify-syntax-entry ?\@ "w" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?^  "w" st)
    (modify-syntax-entry ?\_ "w" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\| "." st)
    (modify-syntax-entry ?\} "){" st)
    st)
  "Syntax table for `enform-mode'.")

;; All keyword lists get sorted so new words can be anywhere within the
;; appropriate list.  The keywords are currently only used for highlighting but
;; more uses such as abbrev-mode are in progress.

(defvar enform-keywords-directives
  '( "ASSIGN"      "ATTACH"      "COMPILE"     "DICTIONARY"  "EDIT"        
     "EXECUTE"     "EXIT"        "HELP"        "OUT"         "RUN"         
     "SECTION"     "SHOW"        "SOURCE"      )
  "List of ENFORM directives.
Used to create the `font-lock-keywords' table.")

(defvar enform-keywords-statements
  '( "AT END"         "AT START"       "CLOSE"          "DECLARE"        
     "DELINK"         "DICTIONARY"     "EXIT"           "FIND"           
     "FOOTING"        "LINK"           "LIST"           "OPEN"           
     "PARAM"          "SET"            "SUBFOOTING"     "SUBTITLE"       
     "TITLE"          )
  "List of ENFORM statement keywords.
Used to create the `font-lock-keywords' table.")

(defvar enform-keywords-clauses
  '( "AFTER CHANGE"   "AS"             "AS DATE"        "AS TIME"        
     "ASCD"           "AT END PRINT"   "AT START PRINT" "BEFORE CHANGE"  
     "BY DESC"        "BY"             "CENTER"         "CUM"            
     "DESC"           "FOOTING"        "FORM"           "HEADING"        
     "INTERNAL"       "JULIAN-DATE"    "NOHEAD"         "NOPRINT"        
     "PCT"            "SKIP"           "SPACE"          "SUBFOOTING"     
     "SUBTITLE"       "SUBTOTAL"       "SUPPRESS"       "TAB"            
     "TIMESTAMP-DATE" "TIMESTAMP-TIME" "TITLE"          "TOTAL"          
     "WHERE"          )
  "List of ENFORM privileged builtin functions.
Used to create the `font-lock-keywords' table.")

(defvar enform-keywords-agregates
  '( "AVG"            "COUNT"          "MAX"            "MIN"            
     "SUM"            )
  "List of ENFORM expression operator keywords.
Used to create the `font-lock-keywords' table.")

(defvar enform-keywords-literals
  '( "ALL"            "BLANK"          "BLANKS"         "ZERO"           
     "ZEROS"          )
  "List of ENFORM expression operator keywords.
Used to create the `font-lock-keywords' table.")

(defvar enform-keywords-operators
  '( "AND"            "BEGINS"         "CHANGE"         "CONTAINS"       
     "COPY"           "ELSE"           "ENTER"          "EQ"             
     "EQUAL"          "GE"             "GREATER"        "GT"             
     "IF"             "IS"             "KEY"            "LE"             
     "LESS"           "LT"             "NE"             "NOT"            
     "NULL"           "OF"             "OFF"            "ON"             
     "OPTION"         "OPTIONAL"       "OR"             "OVER"           
     "THAN"           "THEN"           "THRU"           "TO"             
     "UNIQUE"         "VIA"            "WITH"           )
  "List of ENFORM expression operator keywords.
Used to create the `font-lock-keywords' table.")

(defvar enform-keywords-builtin-files
  '( "QUERY-COMPILER-LISTING"          "QUERY-REPORT-LISTING"            
     "QUERY-STATISTICS"                "QUERY-STATUS-MESSAGES"           
     "QUERY-WORK=AREA"                 "QUERY-SORT-AREA"                 
     "QUERY-QPSTATISTICS"              "QUERY-QPSTATUS-MESSAGES"         )
  "List of ENFORM keywords reserved only in certain language contexts.
Used to create the `font-lock-keywords' table.")

(defvar enform-keywords-std-fcns
  '( "@DATE"                           "@TIME"                           
     "@LINENO"                         "@PAGENO"                         
     "@BLANK-WHEN-ZERO"                "@BREAK-KEY"                      
     "@CENTER-PAGE"                    "@HEADING"                        
     "@STATS"                          "@SUMMARY-ONLY"                   
     "@WARN"                           "@COPIES"                         
     "@COST-TOLERANCE"                 "@DISPLAY-COUNT"                  
     "@LINES"                          "@MARGIN"                         
     "@PAGES"                          "@PRIMARY-EXTENT-SIZE"            
     "@SECONDARY-EXTENT-SIZE"          "@READS"                          
     "@SPACE"                          "@TARGET-RECORDS"                 
     "@VSPACE"                         "@WIDTH"                          
     "@DECIMAL"                        "@NEWLINE"                        
     "@NONPRINT-REPLACE"               "@OVERFLOW"                       
     "@UNDERLINE"                      "@SUBTOTAL-LABEL"                 
     "@DATE-FORMAT"                    "@TIME-FORMAT"                    )
  "List of ENFORM standard functions.
Used to create the `font-lock-keywords' table.")

;;; Font lock (highlighting)

(defcustom enform-font-lock-always t
  "`enform-mode' makes sure `font-lock-mode' is on for enform-mode buffers.
Some things don't work if it's off so insuring it's on is the default."
  :type 'boolean
  :group 'enform)

(defvar enform-column-marker-face 'enform-column-marker-face)

(defface enform-column-marker-face
  '((t (:background "grey")))
  "Used for marking column 79 or whatever column is pointed to by
`enform-column-marker-1' & `enform-column-marker-2'"
  :group 'enform
  :group 'faces)

(defcustom enform-column-marker-1 0
  "*When not zero, this column is font-lock'ed to `enform-column-marker-face'.
Setting this to zero turns off the column marker.  This column
marker is useful for columnizing things or when working in
languages like COBOL where a particular column has significance.
Use `C-u <column> \\[enform-column-marker-1]' while in a `enform-mode'
buffer to change the column marker interactively in that buffer
only.  This customize option sets the default for `enform-mode'."
  :type 'integer
  :group 'enform)
(make-variable-buffer-local 'enform-column-marker-1)

(defun enform-column-marker-1 (column)
  "Set the column marker to COLUMN for the current `enform-mode' buffer.
Max value allowed is 132.  0 turns off the marker.  Set the default with
`M-x customize-option <ret> `enform-column-marker-1' <ret>'. For this buffer
only, specify the column with `C-u <column> \\[enform-column-marker-1]' or
`M-x enform-column-marker-1 <ret> COLUMN <ret>'."
  (interactive "NMarker Column: ")
  (if (not(equal major-mode 'enform-mode))
      (error "Current buffer must be enform-mode")
    (if (< column 0)(setq column 0))
    (if (> column 132)(setq column 132))
    (setq enform-column-marker-1 column)
    ;; To make it take effect immediately I turn off font-lock, make
    ;; font-lock think it has not been on in this buffer previously, then
    ;; re-do our normal font-lock initialization which typically turns it
    ;; back on.  Only this time the new config is in place.
    (font-lock-mode -1)
    (setq font-lock-set-defaults nil) ;key to it not ignoring my changes
    (enform-setup-font-lock)))

(defcustom enform-column-marker-2 79
  "*When not zero, this column is font-lock'ed to `enform-column-marker-face'.
Setting this to zero turns off the column marker.  This column
marker is useful for columnizing things or when working in
languages like COBOL where a particular column has significance.
Use `C-u <column> \\[enform-column-marker-2]' while in a `enform-mode'
buffer to change the column marker interactively in that buffer
only.  This customize option sets the default for `enform-mode'."
  :type 'integer
  :group 'enform)
(make-variable-buffer-local 'enform-column-marker-2)

(defun enform-column-marker-2 (column)
  "Set the column marker to COLUMN for the current `enform-mode' buffer.
Max value allowed is 132.  0 turns off the marker.  Set the default with
`M-x customize-option <ret> `enform-column-marker-2' <ret>'. For this buffer
only, specify the column with `C-u <column> \\[enform-column-marker-2]' or
`M-x enform-column-marker-2 <ret> COLUMN <ret>'."
  (interactive "NMarker Column: ")
  (if (not(equal major-mode 'enform-mode))
      (error "Current buffer must be enform-mode")
    (if (< column 0)(setq column 0))
    (if (> column 132)(setq column 132))
    (setq enform-column-marker-2 column)
    ;; now to make it take effect immediately I turn off font-lock, make
    ;; font-lock think it has not been on in this buffer previously, then
    ;; re-do our normal font-lock initialization which typically turns it
    ;; back on.  Only this time the new config is in place.
    (font-lock-mode -1)
    (setq font-lock-set-defaults nil) ;key to it not ignoring my changes
    (enform-setup-font-lock)))

(defun enform-column-marker-string ()
  (let ((A (min enform-column-marker-1 enform-column-marker-2))
        (B (max enform-column-marker-1 enform-column-marker-2)))
    (setq A (if (zerop A) ""
              (concat (make-string (1- A) ?.) "\\(.\\)")))
    (setq B (if (zerop B) ""
              (concat (make-string (1- B) ?.) "\\(.\\)")))
    (concat "^" A (if (zerop (length A)) B
                    (concat "\\(?:" (substring B (- (length A) 4)) "\\)?")))))

(defcustom enform-primecode-warning t
  "Highlight instances of ]a ]d and ]e in column 1 with a warning face.
This alerts you that submission of this file to RMS/PrimeCode will fail
due to invalid contents.  nil disables this warning."
  :type 'boolean
  :group 'enform)

(defun enform-keyword-anywhere-regexp ( word-list )
  "Returns a regexp that finds any of the words in WORD-LIST.
But only if the keyword is surrounded by non-word chars."
  (concat "\\<"(regexp-opt word-list t)"\\W"))

(defun enform-keyword-on-directive-line-regexp ( word-list )
  "Returns a regexp to find WORD-LIST only if line starts with ?"
  (concat "^\\?\\s *"(regexp-opt word-list t)"\\b"))

;; This finds comments and strings because the syntax table can't handle ENFORM.
(defun enform-find-syntactic-keywords ( search-limit )
  ;; Comments starting with ! go until another ! or eol.  Strings start
  ;; and end with " as usual but cannot span lines so they are terminated
  ;; by eol.  This fcn returns t if either a comment or string is found,
  ;; nil if neither is found. match-data 1&2 are set for comments, 3&4
  ;; are set for a normal string, 5&6 are set for eol-terminated strings.
  ;; Where the match pair mark the start character and end character
  ;; respectively.  Point is left at the end of the match or unchanged if
  ;; no match.
  (when (re-search-forward "\\(?:!\\|\"\\)" search-limit t)
    ;; either a comment or string was found
    (let ((start (match-data))
          (match (match-string-no-properties 0))
          end)
      ;; see if it was a string
      (if (equal "\"" match)
          ;; This 'when' can't fail since eob is part of the search but if it
          ;; does then no search data will be set and the result will be the
          ;; original search above which is a shy group and so no sub-group
          ;; matches will return.  Seemingly appropriate.
          (when (re-search-forward "\\(?:\n\\|\"\\|\\'\\)" search-limit t)
            (setq end (match-data))
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
        ;; Must be a comment.  See above 'when' comment
        (when (re-search-forward "\\(?:\n\\|!\\|\\'\\)" search-limit t)
          (setq end (match-data))
          (set-match-data
           `(,(car start) ,(cadr end)           ;match-string 0
             ,@start ,@end)))))                 ;match-string 1&2
    ;; insure t returns if a string or comment was found. nil returns by default.
    t))

(defvar enform-static-font-lock-keywords
  ;; font-lock-keywords is a symbol or list of symbols yielding the keywords to
  ;; be fontified.  Keywords are listed here using either (MATCHER . FACENAME)
  ;; or (MATCHER . (MATCH FACENAME)) syntax.  Other options are available but
  ;; not used here.  For simplicity, all regexp's were designed so MATCH would
  ;; be 1.  Nothing forced this but to me it makes debug/maintenance easier.
  `((,(enform-keyword-on-directive-line-regexp enform-keywords-directives)
     1 font-lock-builtin-face)
    (,(enform-keyword-anywhere-regexp enform-keywords-builtin-files)
     1 font-lock-builtin-face)
    (,(enform-keyword-anywhere-regexp (append enform-keywords-std-fcns
                                              enform-keywords-statements))
     1 font-lock-keyword-face)
    (,(enform-keyword-anywhere-regexp (append enform-keywords-clauses
                                              enform-keywords-literals))
     1 font-lock-type-face)
    (,(enform-keyword-anywhere-regexp enform-keywords-operators)
     1 font-lock-variable-name-face)
    (,(enform-keyword-anywhere-regexp enform-keywords-agregates)
     1 font-lock-function-name-face)))

(defvar enform-font-lock-keywords ())

(defun enform-build-font-lock-keywords ()
  "Used to create `font-lock-keywords' based on current customize settings."
  (append enform-static-font-lock-keywords
          `(,(unless (and (zerop enform-column-marker-1)
                       (zerop enform-column-marker-2))
               (list (enform-column-marker-string)
                     '(1 enform-column-marker-face prepend t)
                     '(2 enform-column-marker-face prepend t)))
            ,(when enform-primecode-warning
               ;; ]a  ]d or ]e cannot appear in col 1-2 if using PrimeCode.
               '("^\\][ade]" . font-lock-warning-face)))))

(defvar enform-font-lock-syntactic-keywords
 `(;; enform-find-syntactic-keywords returns matches 1&2 for comments, 3&4
   ;; for strings.  5&6 for eol terminated strings.  I must use "|"(15)
   ;; rather than "\""(7) for eol terminated strings because the begin
   ;; and end characters must be the same when "\""(7) is used.
   (enform-find-syntactic-keywords (1 "<" t t) (2 ">" t t)
                                (3 "\"" t t) (4 "\"" t t)
                                (5 "|" t t) (6 "|" t t)))
 "A list of regexp's or functions.  Used to add syntax-table properties to
characters that can't be set by the syntax-table alone.")

(defun enform-setup-font-lock ()
  "Sets up the buffer local value for font-lock-defaults and optionally
turns on font-lock-mode"
  ;; Column markers work by counting characters in the line.  Tabs throw the
  ;; count off and won't highlight the char in the correct column.  If there
  ;; are already tabs the column marker will look wierd but I'm not going to
  ;; mess with the users buffer unexpectedly by converting them.
  (unless (and (zerop enform-column-marker-1)
               (zerop enform-column-marker-2))
    (setq indent-tabs-mode nil)      ;documented as buffer local
  )
  ;; I use font-lock-syntactic-keywords to set some properties and I
  ;; don't want them ignored.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;; I really can't imagine anyone ever wanting this off.
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; This allows column markers to be different in seperate enform-mode buffers.
  (set (make-local-variable 'enform-font-lock-keywords)
       (enform-build-font-lock-keywords))
  ;; This is where all the font-lock stuff actually gets set up.  Once
  ;; font-lock-defaults has it's value, setting font-lock-mode true should
  ;; cause all your syntax highlighting dreams to come true.
  (setq font-lock-defaults
         ;; The first value is all the keyword expressions.
       '(enform-font-lock-keywords
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
         (font-lock-syntactic-keywords . enform-font-lock-syntactic-keywords )))
  ; font lock is turned on by default in this mode. Use customize to disable.
  (when enform-font-lock-always (font-lock-mode t)))

;;; Imenu

(defcustom enform-imenu-menubar t
  "If not nil, `imenu-add-to-menubar' is called during mode initialization.
This adds a [Menu name] menu to your menu bar.  By default the menu contains a
list of all procedures, sections and pages in your program.  You can go
directly to any item on the menu by selecting it.  You can control what
appears on this menu by modifying `enform-imenu-expression-alist'.  You must turn
imenu on for this to work.  See `imenu' in the Emacs reference manual for more
information.  Personally I recommend customizing `imenu-sort-function' to sort
by name."
  :type  '(choice :tag "Menu Name"
                  (string :tag "Menu Name")
                  (const "Index")
                  (const nil))
  :group 'enform)

(defvar enform-imenu-syntax-alist ()
  "Overrides to `enform-mode-syntax-table' used during `imenu-generic-expression' search."
  ;;AFAIK there are no character adjustments needed during imenu search.
)

(defcustom enform-imenu-expression-alist
  '((nil "^\\?section\\s-+\\(\\w+\\)\\b" 1))
  "A list of regular expressions for creating an `imenu' index.

Each element has the form (list-name regexp num).

Where list-name is the name of the submenu under which items matching regexp
are found and num is the expression index defining the label to use for the
submenu entry.  When num = 0 the entire matching regexp text appears under
list-name.  When list-name is nil the matching entries appear in the root
imenu list rather than in a submenu.  See also `enform-imenu-menubar'"
  :type '(repeat (list (choice :tag "Submenu Name" string (const nil))
                       regexp (integer :tag "Regexp index")))
  :group 'enform)

(defcustom enform-display-which-function nil
  "This option turns `which-func' on for all `enform-mode' buffers.
`which-func' is a package that causes the current function, section or
page to be displayed on the mode line.  `which-func' uses `imenu'.  Also
see `enform-imenu-expression-alist' for more information."
  :type 'boolean
  :group 'enform)

(defun enform-setup-imenu ()
  "Installs enform-imenu-generic-expression & enform-imenu-syntax-alist."
  ;; imenu doc says these 3 are buffer-local by default
  (setq imenu-generic-expression enform-imenu-expression-alist)
  (setq imenu-syntax-alist enform-imenu-syntax-alist)
  (setq imenu-case-fold-search t) ;ENFORM is never case sensitive
  (when enform-imenu-menubar
    (if (condition-case ()
            (progn (require 'imenu) t)
          (error nil))
        (imenu-add-menubar-index)
      (message "enform-imenu-menubar is set but imenu feature not available.")))
  (when enform-display-which-function
    (if (condition-case ()
            (progn (require 'which-func) t)
          (error nil))
        (which-function-mode t)
      (message "enform-display-which-function set but which-func not available"))))

;;; Adaptive-fill / auto-fill (needs much work but it's a start)

(defcustom enform-restrict-auto-fill t
  "When not nil a buffer local value for `fill-nobreak-predicate' is created
to prevent code from being accidentally realligned.  The function uses syntax
highlighting to detect comments so `font-lock-mode' must be enabled to work."
  :type 'boolean
  :group 'tal)

(defun enform-setup-adaptive-fill ()
  "Sets up the ENFORM-MODE adaptive-fill variables."
  (set (make-local-variable 'fill-individual-varying-indent)
       nil)
  (set (make-local-variable 'auto-fill-inhibit-regexp)
       "\\s-*[^!]")
  (set (make-local-variable 'comment-use-syntax)
       t)
  ;; I don't like always using -- but ! can't be used reliably since it will
  ;; uncomment existing ! comments in the region being commented.
  (set (make-local-variable 'comment-start)
       "!")
  (set (make-local-variable 'comment-end)
       "!")
  (set (make-local-variable 'comment-padding)
       " ")
  (set (make-local-variable 'comment-start-skip)
       "\\s<\\s-*")
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
  (when enform-restrict-auto-fill
    ; This is supposed to restrict auto-fill to comments only
    (fset (make-local-variable 'fill-nobreak-predicate)
          (lambda ()
            (not (eq (get-text-property (point) 'face)
                     'font-lock-comment-face))))))

;;; Indentation

(defun enform-setup-indent ()
  "Sets default indentation or sets up enform-indent if available."
  (if (condition-case ()
          (progn (require 'enform-indent) t)
        (error nil))
      (set (make-local-variable 'indent-line-function) 'enform-indent-line)
    (set (make-local-variable 'indent-line-function) 'indent-relative-maybe)))

;;; Language Skeletons -- Feel free to add more of your own!

(defcustom enform-keywords-case 'camel
  "*Indicates if keywords in skeletons should be all UPPER CASE, all lower
case or Camel Case (First Char Upper & Rest Lower)."
  :type  '(choice (const :tag "ALL CAPS"   'upper)
                  (const :tag "all small"  'lower)
                  (const :tag "Camel Case" 'camel)
                  (const :tag "DON'T Change"  ()))
  :group 'enform)

(defun enform-setup-skel ()
  "Configures skeleton.el functions for the ENFORM environemnt."
  (set (make-local-variable 'skeleton-transformation) 'enform-skel-transform)
  ;; This prevents abbrevs from expanding within skeletons
  (setq skeleton-further-elements '((abbrev-mode nil))))

(defun enform-skel-transform ( element )
  "Called by `skeleton-insert' to give ELEMENT `enform-keywords-case' capitalization."
  ;; This should be made more complex to only change the case of certain words
  ;; so the user can create skeletons containing items that should not be
  ;; affected by enform-keywords-case.  There are 3 obvious ways.  1) use the
  ;; keywords tables above. 2) add a customize to ignore words. 3) add a
  ;; customize to specify specific words to be affected.  Preferences?
  (if (stringp element)
    (cond
     ((eq enform-keywords-case 'upper) (upcase element))
     ((eq enform-keywords-case 'lower) (downcase element))
     ((eq enform-keywords-case 'camel) (capitalize element))
     ( t                             element            ))
    element))

(define-skeleton enform-if-skel
  "Inserts a standard ENFORM if/then statement skeleton."
  nil > "IF (" _ ") THEN   (  )" \n)

(define-skeleton enform-if-else-skel
  "Inserts a standard ENFORM if/then/else statement skeleton."
  nil > "IF (" _ ") THEN (  ) ELSE (  )" \n)


;;; Abbrev support

(defcustom enform-abbrev-mode t
  "Sets the default value for `abbrev-mode' upon entry into `enform-mode'."
  :type 'boolean
  :group 'enform)

(defvar enform-mode-abbrev-table-list
  '(("$i" "" enform-if-skel)
    ("$e" "" enform-if-else-skel))
  "List of pre-defined `enform-mode' abbrev definitions.
Use \\[list-abbrevs] to see all defined abbrevs.")

(defvar enform-mode-abbrev-table)

(defun enform-setup-abbrevs ()
  "Installs the `enform-mode-abbrev-table' as `local-abbrev-table'"
  (define-abbrev-table 'enform-mode-abbrev-table enform-mode-abbrev-table-list)
  (setq local-abbrev-table enform-mode-abbrev-table)
  (setq skeleton-further-elements '((abbrev-mode nil)))
  (abbrev-mode enform-abbrev-mode)    ;Setting is documented as buffer local
)

;;; Movement by ...

;(defvar enform-outline-regexp
;...)

;;; Miscellaneous

;; This ruler was inspired by the one in fortran-mode but is a vastly
;; improved implementation.  Find it on emacswiki.org under PopupRuler.
;; In fact even the old enform-column-ruler was a vast improvement.
(defun enform-popup-ruler (arg)
  "Temporarily display a ruler above the current line.
With prefix arg, make ruler measure in both directions from point."
  (interactive "P")
  (momentary-string-display
   (if arg
       (let* ((left-len (current-column))
              (right-len (- (+ (window-hscroll)(window-width)) left-len))
              (left (enform-popup-ruler-r-l left-len))
              (right (enform-popup-ruler-l-r right-len)))
       (concat (cadr left) (cadr right) "\n"
               (car left) (car right) "\n"))
     (let* ((right-len (+ (window-hscroll)(window-width)))
            (right (enform-popup-ruler-l-r right-len)))
       (concat (cadr right) "\n"
               (car right) "\n")))
   (line-beginning-position)
   nil "[space] Clears ruler"))

(defun enform-popup-ruler-r-l (len)
  "Returns right to left running ruler of length LEN.
Result is a list of 2 strings, markers and counters."
  (let* ((iterations (/ (1- (abs len)) 10))
         (short (- (* 10 (1+ iterations)) (abs len)))
         (result1 "|....|...|")
         (result2 "10   5   1")
         (inc1    "|....|....")
         (inc2    "%d0         ")
         (i 1))
    (while  (<= i iterations)
      (setq i (1+ i))
      (setq result1 (concat inc1 result1))
      (setq result2 (concat (substring (format inc2 i) 0 10) result2)))
    (list (substring result1 short) (substring result2 short))))

(defun enform-popup-ruler-l-r (len)
  "Returns left to right running ruler of length LEN.
Result is a list of 2 strings, markers and counters."
  (let* ((iterations (/ (1- (abs len)) 10))
         (result1 "|...|....|")
         (result2 "1   5   10")
         (inc1    "....|....|")
         (inc2    "        %d0")
         (i 1))
    (while  (<= i iterations)
      (setq i (1+ i))
      (setq result1 (concat result1 inc1))
      (setq result2 (concat result2 (substring (format inc2 i) -10))))
    (list (substring result1 0 len) (substring result2 0 len))))

;;;###autoload
(defun enform-mode ()
  "A major mode for editing ENFORM language source files.
Customization options are available via
\\[customize-group] <ret> ENFORM <ret>

This mode provides ENFORM specific support for such packages as:
    `font-lock-mode'            `show-paren-mode'
    `imenu'                     `which-function'
    `skeleton-insert'           `auto-fill-mode'
    `adaptive-fill-mode'        `filladapt-mode'

** Note ** Many things won't work correctly if `font-lock-mode' is off.

enform-mode also implements the following \\[execute-extended-command] ... commands

`enform-mode'             Activates this mode for the current buffer
`enform-popup-ruler'      Temporarily displays ruler in buffer
`enform-column-marker-1'  \\These move the column markers for this
`enform-column-marker-2'  /buffer only. See \\[describe-function] enform-column-marker-1
`enform-if-skel'          Inserts an if/then statement skeleton
`enform-if-else-skel'     Inserts an if/then/else statement skeleton

\\{enform-mode-map}
Use \\[describe-bindings] to see ALL key bindings.

Some settings I like:
Turn on `skeleton-pair-insert-maybe' for (), [] and \"\"
Turn on `recentf-mode'. You might need `recentf-auto-cleanup' = 'never
I find `transient-mark-mode' totally indespensible.
CUA mode has some really great rectangle functions."
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'major-mode) 'enform-mode)
  (set (make-local-variable 'mode-name) "ENFORM")
  (set (make-local-variable 'make-backup-files) nil) ;necessary for now
  (use-local-map enform-mode-map)
  (set-syntax-table enform-mode-syntax-table)
  (enform-setup-font-lock)
  (enform-setup-adaptive-fill)
  (enform-setup-abbrevs)
  (enform-setup-imenu)
   (enform-setup-indent)
  (enform-setup-skel)
;  (set (make-local-variable 'outline-regexp) enform-outline-regexp)
  (show-paren-mode 1)
  (run-hooks 'enform-mode-hook))

(provide 'enform-mode)

;;; enform-mode.el ends here

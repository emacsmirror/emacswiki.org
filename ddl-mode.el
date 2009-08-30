;;; ddl-mode.el --- Handles the Tandem-Compaq-HP DDL language.
;;                  A proprietary language of Tandem/Compaq/HP computers.

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Author: Rick Bielawski <rbielaws@i1.net>
;; Keywords: languages, extensions, tandem, compaq, hp

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

;; DDL is Tandem's DATA DEFINITION Language.

;; Keywords as of G06.05 are recognized by this version of ddl-mode.
;;
;; imenu recognizes ?section <name> and ?page <name> declarations as
;;   well as the expected RECORD <name>. & DEFINITION <name>. lines.
;;
;; Both ! and * style comments are handled correctly (I think).
;;

;;; ToDo:

;; There is no adaptive-fill support yet.
;; Custom indentation support has not been created yet.
;; There are no abbrevs defined.

;;; Installing:

;; Before you can use ddl-mode, emacs needs to be able to find it.  Place the
;; ddl-mode.el file in a directory on the load-path; typically the
;; .../site-lisp or perhaps .../lisp/progmods directory.  Usually you would
;; also want to byte compile ddl-mode.el but this is not required.  To do
;; this, visit the ddl-mode.el file, type: M-x emacs-lisp-byte-compile <ret>
;; There should be no warnings or errors during byte compilation.
;;
;; There are 4 basic ways to use DDL-MODE on a file.  The first method
;; manually selects ddl-mode as the editing mode.  The other 3 cause emacs to
;; recognize automatically that you want to visit the file using ddl-mode.
;;
;; Pick one:
;; 1. While visiting a file, type: M-x ddl-mode <ret>
;; 2. Put the string -*-ddl-*- in a comment on the first line of the file.
;;    Save the file and close it.  Now any time you open it ddl-mode starts.
;; 3. Create an association between a particular file naming convention and
;;    ddl-mode.  This is done by adding an association to auto-mode-alist.
;; For example:
;; (setq auto-mode-alist
;;   (append
;;     '(("\\.ddl\\'" . ddl-mode)         ;extension of .ddl means ddl-mode
;;       ("\\([\\/]\\|^\\)[^.]+$" . ddl-mode)) ;so does no extension at all.
;;    auto-mode-alist))
;; 4. Advise set-auto-mode to look at the buffer contents upon loading.
;;
;; The above all tell emacs that you want to use ddl-mode but you must load
;; ddl-mode before you can use it.  There are 2 methods of telling emacs to
;; load the ddl-mode routines.  The first unconditionally loads ddl-mode
;; definitions immediately.  The second tells emacs to automatically load
;; ddl-mode only when you try to use it.  Add one of the following lines to
;; your .emacs file.
;;
;;(require 'ddl-mode)      ; Unconditional load
;;(autoload 'ddl-mode "ddl-mode" "Major mode for Tandem DDL files." t nil)
;;
;; Please report any bugs!
;;
;; Either way you choose to load ddl-mode, emacs needs to be able to find it.
;; Place the ddl-mode.el file in a directory on the load-path; typically the
;; `.../lisp/progmodes' directory or maybe the `.../site-lisp' directory.

;;; History:

;; 2005-02-18 RGB Mode is finally useable enough to start tracking.
;; 2005-02-18 RGB Minor bug fix to font-lock.  Also moved close parens to
;;                where some fanatics seem to think they belong.
;; 2005-09-22 RGB Fixed font-locking of compiler directives.
;; 2005-10-04 RGB Another directive fix, column-marker updates
;;                and fixes to syntax table.
;; 2005-10-19 RGB Removed column-marker & popup-ruler stuff.
;;                These are now standalone packages here:
;;                http://www.emacswiki.org/cgi-bin/emacs/column-marker.el
;;                http://www.emacswiki.org/cgi-bin/emacs/popup-ruler.el
;;                Added automatic detection and support for above packages.
;; 2006/10/24     simplified detection of comments.
;;; Code:

(defgroup ddl nil
  "Major mode for editing Tandem DDL source files in Emacs."
  :group 'languages)

;;; KEY MAP

(defvar ddl-skeleton-map
  (let ((map (make-sparse-keymap)))
    ;; nothing here yet
    map)
  "Keymap for `ddl-mode' skeletons.")

(defvar ddl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab]           'indent-according-to-mode)
    (define-key map [?\C-c ?\C-c]   'column-marker-here)
    (define-key map [?\C-c ?\C-f]   'auto-fill-mode)
    (define-key map [?\C-c ?\C-r]   'popup-ruler)
    (define-key map [?\C-c ?\C-s]    ddl-skeleton-map)
    (define-key map [?\C-c return]  'comment-indent-new-line)
;;;    (define-key map [\C-j] 'eval-print-last-sexp)
    map)
  "Keymap for `ddl-mode'.")

(defvar ddl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?$ "'" st)
    (modify-syntax-entry ?% "'" st)
    (modify-syntax-entry ?& "'" st)
    (modify-syntax-entry ?' "." st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?, "." st)
    (modify-syntax-entry ?- "w" st)
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?/ "." st)
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?\? "'" st)
    (modify-syntax-entry ?@ "'" st)
    (modify-syntax-entry ?[ "(]" st)
    (modify-syntax-entry ?] ")[" st)
    (modify-syntax-entry ?\\ "'" st)
    (modify-syntax-entry ?^ "w" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?{ "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?} "." st)
    st)
  "Syntax table for `ddl-mode'.")

; All keyword lists get sorted, so you can add new words to the end.

(defvar ddl-keywords-directives
  '( "ansicobol"       "c00align"        "c_decimal"       
     "c_match_historic_tal"              "ccheck"          "cdefineupper"    
     "cendif"          "cfieldalign_matched2"              "cifdef"          
     "cifndef"         "clistin"         "clistout"        "clistoutdetail"  
     "cobcheck"        "coblevel"        "columns"         "comments"        
     "cpragma"         "ctokenmap_asdefine"                "cundef"          
     "deflist"         "dict"            "dictn"           "dictr"           
     "do_ptal_off"     "do_ptal_on"      "edit"            "errors"          
     "expandc"         "fieldalign_shared8"                "filler"          
     "forcheck"        "fortranunderscore"                 "help"            
     "linecount"       "list"            "nclconstant"     
     "newfup_fileformat"                 "noansicobol"     "noc00align"      
     "noc_decimal"     "noc_match_historic_tal"            "noccheck"        
     "nocdefineupper"  "noclistin"       "noclistout"      "nocobcheck"      
     "nocomments"      "nocpragma"       "noctokenmap_asdefine"              
     "nodeflist"       "nodict"          "noexpandc"       "nofileformat"    
     "noforcheck"      "nofortranunderscore"               "nolist"          
     "nonclconstant"   "nooutput_sensitive"                "nopascalcheck"   
     "nopascalnamedvariant"              "noreport"        "nosave"          
     "notalallocate"   "notalcheck"      "notalunderscore" "notimestamp"     
     "novalues"        "nowarn"          "oldfup_fileformat"                 
     "output_sensitive"                  "page"            "pascalbound"     
     "pascalcheck"     "pascalnamedvariant"                "report"          
     "reset"           "save"            "section"         "setcobol74"      
     "setcobol85"      "setlocalename"   "setsection"      "source"          
     "spacing"         "taclgen"         "talallocate"     "talbound"        
     "talcheck"        "talunderscore"   "tedit"           "timestamp"       
     "values"          "warn"            "warnings"        )
  "List of DDL compiler directives.
   Used to create the font-lock-keywords table.")
(defvar ddl-keywords-output-directives
  '( "c"         "cobol"     "ddl"       "fortran"   "fup"       "noc"       
     "nocobol"   "noddl"     "nofortran" "nofup"     "nopascal"  "notacl"    
     "notal"     "out"       "pascal"    "tacl"      "tal"       )
  "List of DDL file output control compiler directives.
   Used to create the font-lock-keywords table.")
(defvar ddl-keywords-reserved
  '( "are"       "begin"     "binary"    "character" "complex"   "end"       
     "enum"      "filler"    "float"     "is"        "logical"   "of"        
     "on"        "through"   "thru"      "time"      "timestamp" )
  "List of DDL reserved words - cannot be used as field names.
   Used to create the font-lock-keywords table.")
(defvar ddl-keywords-statements
  '( "all"             "allowed"         "as"              "ascending"       
     "assigned"        "audit"           "auditcompress"   "be"              
     "bit"             "block"           "buffered"        "buffersize"      
     "by"              "c_match_historic_tal"              
     "cfieldalign_matched2"              "character"       "code"            
     "comp"            "comp-3"          "compress"        "computational"   
     "computational-3" "constant"        "crtpid"          "current"         
     "date"            "datetime"        "day"             "dcompress"       
     "def"             "definition"      "delete"          "depending"       
     "descending"      "device"          "display"         "duplicates"      
     "edit-pic"        "entry-sequenced" "exit"            "ext"             
     "external"        "fieldalign_shared8"                "file"            
     "fname"           "fname32"         "for"             "fraction"        
     "heading"         "help"            "high-number"     "high-value"      
     "hour"            "icompress"       "index"           "indexed"         
     "interval"        "just"            "justified"       "key"             
     "key-sequenced"   "keytag"          "ln"              "low-number"      
     "low-value"       "low-values"      "maxextents"      "minute"          
     "month"           "must"            "n"               "no"              
     "not"             "novalue"         "noversion"       "null"            
     "occurs"          "oddunstr"        "output"          "packed-decimal"  
     "phandle"         "pic"             "picture"         "quote"           
     "quotes"          "record"          "redefines"       "refresh"         
     "relative"        "renames"         "right"           "second"          
     "seq"             "sequence"        "serialwrites"    "setlocalename"   
     "show"            "space"           "spaces"          "spi-null"        
     "sql"             "sql-nullable"    "sqlnull"         "ssid"            
     "subvol"          "system"          "tacl"            "talunderscore"   
     "temporary"       "time"            "times"           "to"              
     "token-code"      "token-map"       "token-type"      "transid"         
     "tstamp"          "type"            "unsigned"        "unstructured"    
     "update"          "upshift"         "usage"           "use"             
     "username"        "value"           "varchar"         
     "varifiedwritesversion"             "varying"         "year"            
     "zero"            "zeroes"          "zeros"           )
  "List of DDL statement keywords
   Used to create the font-lock-keywords table.")
(defvar ddl-keyword-fcn-names-regexp
  "^\\(record\\|definition\\|def\\)\\s-+\\(\\w+\\)\\s-*\\."
  "regexp that finds the names of record & structure definitions.")

;;; Font lock (highlighting)

(defcustom ddl-font-lock-always t
  "If non-nil, DDL-MODE will always turn `font-lock-mode' on even if
`global-font-lock-mode' is off.  nil disables this feature."
  :type 'boolean
  :group 'ddl)

(defcustom ddl-primecode-warning t
  "Highlight instances of ]a ]d and ]e in column 1 with a warning face.
This alerts you that submission of this file to RMS/PrimeCode will fail
due to invalid contents.  nil disables this warning."
  :type 'boolean
  :group 'ddl)

;;For consistancy, regexp functions all put the keyword as match #2
(defun ddl-keyword-anywhere-regexp ( word-list )
  "Returns a regexp that finds the words passed.
   But only if the keyword is surrounded by non-word chars."
  (concat "\\<"(regexp-opt word-list t)"\\>"))

;; The next 4 def's work tightly together and, as coded, cannot be reused for
;; additional purposes.
(defvar ddl-keyword-on-directive-line-regexp () "Internal use only.")
(defun  ddl-keyword-on-directive-line-regexp ( word-list )
"Returns a function to find WORD-LIST only if line starts with ?"
  (setq ddl-keyword-on-directive-line-regexp
        (concat "\\b"(regexp-opt word-list t)"\\b"))
  'ddl-font-lock-directive-line)

(defvar ddl-amid-font-lock-excursion nil
;; Used by `ddl-font-lock-directive-line'.  When a line starting with ? in
;; column 1 is detected this variable holds the context needed to continue
;; searching for more keywords.  If nil a line starting with ? should be
;; searched for.
)
(make-variable-buffer-local 'ddl-amid-font-lock-excursion)
(defun ddl-font-lock-directive-line ( search-limit )
;; This function finds keywords only in lines starting with ?.  Valid keywords
;; are described by `ddl-keyword-on-directive-line-regexp'.  First a line
;; beginning with ? is searched for.  Once found, point is moved to the
;; beginning of that line and limit is set to the end.  Keywords are searched
;; for within that range.  If found, context is saved in
;; ddl-amid-font-lock-excursion and the match-data is returned.  If not found,
;; another line starting with ?  is searched for.  If saved context exists when
;; this function is called then another keyword is searched for in the
;; previously narrowed region.  If none is found the next region is searched
;; for.
  (let ((looking t))
    (while (and looking
             (or ddl-amid-font-lock-excursion
        	 (when (re-search-forward "^\\?.+$" search-limit t)
        	   (setq ddl-amid-font-lock-excursion (point))
        	   (goto-char (match-beginning 0)))))
      (if (re-search-forward ddl-keyword-on-directive-line-regexp
        		     ddl-amid-font-lock-excursion t)
          (setq looking nil)
        (goto-char ddl-amid-font-lock-excursion)
        (setq ddl-amid-font-lock-excursion nil)))
    (not looking)))

(defun ddl-keyword-output-directive-regexp ( word-list )
  "Returns a regexp that finds the words passed.
   But only if the keyword is surrounded by non-word chars."
  (concat "^\\?.*?\\b"(regexp-opt word-list t)"\\b"))

(defvar ddl-static-font-lock-keywords
  `((,(ddl-keyword-on-directive-line-regexp ddl-keywords-directives)
     1 font-lock-builtin-face)
    (,(ddl-keyword-output-directive-regexp ddl-keywords-output-directives)
     1 font-lock-warning-face)
    ("^\\s-\\{0,2\\}\\(end\\|def\\|definition\\|record\\)\\W"
     1 font-lock-keyword-face)
    (,(ddl-keyword-anywhere-regexp ddl-keywords-reserved)
     1 font-lock-variable-name-face)
    (,(ddl-keyword-anywhere-regexp ddl-keywords-statements)
     1 font-lock-keyword-face)
    (,ddl-keyword-fcn-names-regexp 2 font-lock-function-name-face))
  "Keyword highlighting specification for `ddl-mode'.")

;; This finds comments because the syntax table can't handle DDL.
(defun ddl-find-syntactic-keywords ( search-limit )
  ;; Comments starting with -- go to eol always, while comments starting
  ;; with !  go until another ! or eol.  This fcn returns nil if no
  ;; comment is found.  It returns t and match-data 1&2 indicating the
  ;; strings to get `<' and `> syntax'.  Point is left at the end of the
  ;; match.
  (when (re-search-forward "\\(?:\"\\|!\\)" search-limit t) ; "
    ;; a comment was found
    (let ((start (match-data))
          end)
      ;; Determine comment type.
      (if  (equal "\"" (match-string-no-properties 0))         ;ignore anything
          (re-search-forward "\\(?:\"\\|\n\\)" search-limit t) ;within a string
        ;; This 'when' can't fail since eob is part of the search.  But if it
        ;; does then nil is returned as if no comment were found.
        (when (re-search-forward "\\(?:\n\\|!\\|\\'\\)" search-limit t)
          (setq end (match-data))
          (set-match-data
           `(,(car start) ,(car (cdr end))      ;match-string 0
             ,@start ,@end)                     ;match-string 1&2
          )
          t)))))

(defvar ddl-font-lock-syntactic-keywords
 `(
   (ddl-find-syntactic-keywords            (1 "<" t t) (2 ">" t t))
   ("\\(^\\*\\|--\\)"                      (0 "<" t t))
   ("^\\s-*\\(d\\)\\(ef\\|efinition\\)\\b" (1 "(d"))
   ("^\\s-*\\(r\\)ecord\\b"                (1 "(d"))
   ("^\\s-*en\\(d\\)\\.?\\>"               (1 ")d")))
 "A list of regexp's or functions.  Used to add syntax-table properties to
characters that can't be set by the syntax-table alone.")

(defvar ddl-font-lock-keywords ())

(defun ddl-build-font-lock-keywords ()
  "Used to create `font-lock-keywords' based on current customize settings."
  (append ddl-static-font-lock-keywords
          `(,(when ddl-primecode-warning
               ;; ]a  ]d or ]e cannot appear in col 1-2 if using PrimeCode.
               '("^\\][ade]" . font-lock-warning-face)))))

(defun ddl-setup-font-lock ()
  "Sets up the buffer local value for font-lock-defaults and optionally
turns on font-lock-mode"
  ;; I use font-lock-syntactic-keywords to set some properties and I don't want
  ;; them ignored.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;; I really can't imagine anyone wanting this off in DDL.  It would force you
  ;; never to use the words begin or end in a comment unless you balanced them.
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; This allows column markers to be different in seperate ddl-mode buffers.
  (set (make-local-variable 'ddl-font-lock-keywords)
       (ddl-build-font-lock-keywords))
  ;; This is where all the font-lock stuff actually gets set up.  Once
  ;; font-lock-defaults has it's value, setting font-lock-mode true should
  ;; cause all your syntax highlighting dreams to come true.
  (set (make-local-variable 'font-lock-defaults)
       '(ddl-font-lock-keywords
         ;; keywords-only means no strings or comments get fontified
         nil
         ;; case-fold (ignore case)
         t
         ;; syntax-alist, nothing needs overriding
         nil
         ;; syntax-begin - no function defined to move outside syntactic block
         nil
         ;; font-lock-syntactic-keywords
         ;; takes (matcher (match syntax override lexmatch) ...)...
         (font-lock-syntactic-keywords . ddl-font-lock-syntactic-keywords)))
  ; font lock is turned on by default in this mode. Use customize to disable.
  (when ddl-font-lock-always (font-lock-mode t)))

;;; Static Column Markers

(defcustom ddl-column-marker-1 0
  "*Turns on column-marker-1 (which see) at the specified column.
Use of this feature requires the column-marker.el package be loaded or on
the search list."
  :type 'integer
  :group 'ddl)
(make-variable-buffer-local 'ddl-column-marker-1)

(defcustom ddl-column-marker-2 79
  "*Turns on column-marker-2 (which see) at the specified column.
Use of this feature requires the column-marker.el package."
  :type 'integer
  :group 'ddl)
(make-variable-buffer-local 'ddl-column-marker-2)

(defun ddl-setup-column-markers ()
  "Turns on column markers if configured and available.
See `ddl-column-marker-1' and `ddl-column-marker-2' "
  (if (condition-case ()
          (progn (require 'column-marker) nil)
        (error t))
      (if (not (and (zerop ddl-column-marker-1)
                    (zerop ddl-column-marker-2)))
          (message "column markers are configured but %s"
                   " column-marker feature not available."))
    (setq indent-tabs-mode nil)      ;documented as buffer local
    (column-marker-1 ddl-column-marker-1)
    (column-marker-2 ddl-column-marker-2)))

;;; Imenu & Which-function

(defcustom ddl-imenu-menubar nil
  "If not nil, `imenu-add-to-menubar' is called during mode initialization.
This adds a [Menu name] menu to your menu bar.  By default the menu contains a
list of all record and structure definitions in the file.  You can go
directly to any item on the menu by selecting it.  You can control what
appears on this menu by modifying `ddl-imenu-generic-expression'.  You must turn
imenu on for this to work.  See `imenu' in the Emacs reference manual for more
information.  Personally I recommend customizing `imenu-sort-function' to sort
by name."
  :type  '(choice :tag "Menu Name"
                  (string :tag "Menu Name")
                  (const "Index")
                  (const nil))
  :group 'ddl)
(defvar ddl-imenu-syntax-alist ()
  "Overrides to ddl-mode-syntax-table used during
imenu-generic-expression search."
  ;;AFAIK there are no character adjustments needed during imenu search.
)
(defcustom ddl-imenu-expression-alist
  (list
   (list "?Sections" "^\\?SECTION +\\(\\w+\\b\\)"                  1)
   (list "?Pages"    "^\\?PAGE +\"\\(.+?\\)\""                     1)
   (list "Records"   "^\\s-*\\(record\\)\\s-+\\(\\w+\\)\\s-*\\."   2)
   (list "Def's"     "^\\s-*\\(definition\\|def\\)\\s-+\\(\\w+\\)" 2))
  "A list of regular expressions for creating an `imenu' index.

Each element has the form (list-name regexp num).

Where list-name is the name of the submenu under which items matching regexp
are found and num is the expression index defining the label to use for the
submenu entry.  When num = 0 the entire matching regexp text appears under
list-name.  When list-name is nil the matching entries appear in the root
imenu list rather than in a submenu.  See also `ddl-imenu-menubar'"
  :type '(repeat (list (choice :tag "Submenu Name" string (const nil))
                       regexp (integer :tag "Regexp index")))
  :group 'ddl)
(defcustom ddl-display-which-function t
  "This option turns `which-func' on for all ddl-mode buffers.  `which-func'
is a package that causes the current definition, section or page to be
displayed on the mode line.  `which-func' uses `imenu'.  Also see
`ddl-imenu-expression-alist' for more information."
  :type 'boolean
  :group 'ddl)
(defun ddl-setup-imenu ()
  "Installs ddl-imenu-generic-expression & ddl-imenu-syntax-alist."
  ;; Doc says all 3 are buffer-local by default
  (setq imenu-generic-expression ddl-imenu-expression-alist)
  ;; no alist is needed since - & _ should already be part of 'w'
  ;;  (setq imenu-syntax-alist ddl-imenu-syntax-alist)
  (setq imenu-case-fold-search t)
  (when ddl-imenu-menubar
    (if (condition-case ()
            (progn (require 'imenu) t)
          (error nil))
        (imenu-add-menubar-index)
      (message "ddl-imenu-menubar is set but imenu feature not available.")))
  (when ddl-display-which-function
    (if (condition-case ()
            (progn (require 'which-func) t)
          (error nil))
        (which-function-mode t)
      (message "ddl-display-which-function set but which-func not available"))))

;(defvar ddl-outline-regexp
;...)

;;;###autoload
(defun ddl-mode ()
  "A major mode for editing DDL source files.
See the customization options available via:
\\[customize-group] <ret> DDL <ret>

This mode provides DDL specific support for such packages as:
    `font-lock-mode'            `show-paren-mode'
    `imenu'                     `which-function'

** Note ** Many things won't work correctly if `font-lock-mode' is off.

ddl-mode also implements the following  \\[execute-extended-command] ... commands

`ddl-mode'	       Activates this mode for the current buffer

\\{enform-mode-map}
Use \\[describe-bindings] to see ALL key bindings.

Some settings I like:
Turn on `skeleton-pair-insert-maybe' for (), [] and \"\"
Turn on `imenu-mode' & set `imenu-sort-function' to imenu--sort-by-name
Turn on `recentf-mode'. You might need `recentf-auto-cleanup' = 'never
Set `column-marker-1' to 79 so you can tell what TEDIT users can't see.
Load `popup-ruler' for a TEDIT F9 type ruler on steroids.
I find `transient-mark-mode' totally indespensible
CUA mode has some really great rectangle functions.
"
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'major-mode) 'ddl-mode)
  (set (make-local-variable 'mode-name) "DDL")
  (set (make-local-variable 'make-backup-files) nil) ;necessary for now
  (use-local-map ddl-mode-map)
  (set-syntax-table ddl-mode-syntax-table)
  (ddl-setup-font-lock)
;  (ddl-setup-adaptive-fill)
  (show-paren-mode 1)
;  (ddl-setup-abbrevs)
  (ddl-setup-imenu)
  (imenu-add-menubar-index)
  (set (make-local-variable 'indent-line-function) 'indent-relative-maybe)
  (ddl-setup-column-markers)
;  (set (make-local-variable 'outline-regexp) ddl-outline-regexp)
  (run-hooks 'ddl-mode-hook))

(provide 'ddl-mode)

;;; ddl-mode.el ends here.

;;; gas-mode.el --- mode for editing assembler code

;; Copyright (C) 2007 Heike C. Zimmerer
;; Time-stamp: <2007-12-27 18:14:08 hcz>

;; Author: Heike C. Zimmerer <hcz@hczim.de>
;; Created: 20 Feb 2007
;; Version: 1.10  2009-2-25 hcz
;; Keywords: languages



;; This file is written for GNU Emacs, and uses the same license
;; terms; however, it is an add-on and not part of it.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; To use this mode, put gas-mode.el somewhere on your load-path.
;; Then add this to your .emacs:
;;
;;    (require 'gas-mode)
;;    (add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))

;; gas-mode recognizes gas syntax (including embedded C preprocessor
;; directives).  It does a limited amount of parsing, so it can do
;; some fancy things with syntactic elements (like labels).  It,
;; however does not know about the peculiarities of the special
;; processor you're writing code for (there are just too many of
;; them), so, for example, it can't tell register names from labels.
;; Also, its scope is limited to the file you are editing.
;;
;; Symbol highlighting: For a symbol to be recognized as such, it must
;; be in a field where (as per gas syntax) symbols can be placed
;; (i.e. label field, argument field, some directives).  If point
;; rests on such a symbol and there are more of it in the current
;; buffer, it is highlighted and you can move forward and backward
;; between all places where that symbol is referenced or defined with
;; forward-sexp and backward-sexp.  
;;
;; This is different from a simple string search in that only those
;; places are considered where the symbol is actually used.  For local
;; labels, gas-mode resolves which references are associated with
;; which location and only highlights those that fit.  The
;; highlighting is different for different types of symbols; see the
;; customization buffer for the gas-symbol-* faces for short
;; explanations of their meanings.
;;
;; Special forms of local labels (like `55$') are not (yet?)
;; supported.

;; A special feature may need some explanation: C passthroughs.
;; Assembler code is often used to write functions that are later
;; called by C programs.  For this to work, you usually have to
;; maintain two files, one containing the assembler source, the other
;; holding the C interface declaration.  C passthroughs allow you to
;; move the C declaration part into the assembler file next to the
;; function it belongs to.
;;
;; From the assembler's point of view, C passthroughs are just C syntax
;; comments with some small syntactic sugar added, like:
;;
;; /*C
;;     int a_declaration(void);
;;     extern volatile int another_declaration;
;;
;;     /# and this will be passed as comment #/
;; */
;;
;; Note the `/*C' at the beginning and the `/# ... #/' for the nested
;; comment.
;;
;; It is then up to the Make process to generate a .h file, which
;; carries the declarations and the comment, changing the "/#" and "#/"
;; into "/*" and "*/".
;;
;; This may be done by including a line similar to the following into
;; your Makefile (assuming $(ASFILES) is a list of your assembly
;; language files) (and don't forget to use a TAB for the white space
;; which introduces the action lines (<TAB> echo ..., <TAB>sed -n ...):
;;
;; asm-C-defs.h: $(ASFILES)
;;	echo '/* Definitions of assembly language functions */' > $@
;;	echo '/* (automatically created by make)            */' >> $@
;; 	sed -n '/[/][*]C/,/[*][/]/{s|/[*]C||;s|[*]/||;s|/#|/*|;s|#/|*/|;p}' \
;;                $^ >> $@
;; 
;; (Note the above code requires the "/*C" and "*/" to be on a line of
;; their own.)  gas-mode recognizes this kind of comment by proper
;; syntax highlighting.  Symbol highlighting is also supported.  For a
;; symbol to be highlighted within C passthrough code, it must be
;; defined to be global (because only then it is visible to an
;; external C program) in the same buffer.

;;
;; This mode runs `gas-mode-hook' when initialization is complete.
;;  

;; Bugs:
;;
;;    Most probably, yes.  You'll tell me (<hcz@hczim.de>).
;;
;;    This code is *not tested at all* for syntaxes where
;;    `gas-commant-char' differs from `?;'.

;;; Change Log:
;;
;;    2007-05-26  1st public release (hcz).
;;
;;    2007-05-30  docstrings, commentary, 
;;                open string recognition with gas-next-token/limit-re.
;;
;;    2007-12-05  Bug with Intel syntax in arguemnt field fixed
;;
;;    2007-12-25  Indents fixed for C comments without closing '*/'
;;                
;;; Code:

(defgroup gas nil
  "Mode for editing gas syntax assembler code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom gas-comment-char ?\;
  "The comment start character assumed by gas mode."

  :type 'character
;  :set 'gas-set-gas-comment-char
  :group 'gas)

(defcustom gas-opcode-column 14
  "The opcode column."
  :type 'integer
  :group 'gas)

(defcustom gas-argument-column 20
  "The column for the arguments (if any)."
  :type 'integer
  :group 'gas)

(defcustom gas-comment-column 36
  "The column where end of line asm comments go to."
  :type 'integer
  :group 'gas)

(defcustom gas-comment-char-starts-comment nil
  "Always jump to comment column when a variable `gas-comment-char' is typed.

It t, starts/expands a comment if appropriate.  When
unset (nil), use `gas-comment-dwim' (usually bound to M-;) to get there."
  :type 'boolean
  :group 'gas)

(defcustom gas-indent-current-field-only nil
  "If nil, 'indent' indents all fileds on the current line.
Else only the current field is affected."
  :type 'boolean
  :group 'gas)

;; (defcustom gas-preserve-trailing-whitespace nil
;;   "If nil, (re-)indenting removes trailing white space."
;;   :type 'boolean
;;   :group 'gas)
(setq gas-preserve-trailing-whitespace nil) ; (currently?) non-functional.

(defcustom gas-enable-symbol-highlight t
  "Enable symbol recognition and highlighing.

When t and if point is on a symbol, some limited parsing data is
collected and all occurences of this symbol in the buffer get
highlighted according to the results (defined, global, etc.).  As
long as a symbol is highlighted, `forward-sexp' and `backward-sexp'
move to the next/previous occurence of the same symbol in the
same buffer."
  :type 'boolean
  :group 'gas)

(defcustom gas-use-C-passthrough t
  "When true, C passthrough comments are recognized.

This kind of comment is introduced by the starting sequence
\"/*C\" and is meant to be processed later by an external
program \(see the introducing comment in gas-mode.el for an
example) into C source code.  Within these passthrough-comments,
the combination /# ... #/ is available for nested comments which
will be later changed into real C comments (/* ... */) by the
same external program."
  :type 'boolean
  :group 'gas)

(defcustom gas-defun-regexp "\n\\([;#].*\\|.*[*]/[ \t]*\\|\\|[ \t]*\\|[ \t]+\\..*\\)\n\\([^ \t\n;]+:\\)"
  "Regexp used to recognize the beginning of a defun.

The default value describes a line which is either empty, a
full-line left-justified comment or a directive, followed by a
line starting with a label.  Note that the character \";\" in the
regexp will be replaced by the actual comment character described
by variable `gas-comment-char'."
  :type 'regexp
;  :set 'gas-set-comment-regexp
  :group 'gas)

 

(defcustom gas-defun-regexp-subexp 2
  "The subexp in `gas-defun-regexp to jump to."
  :type 'integer
  :group 'gas)

(defcustom gas-C-indent 3
  "Indent to use with C style comments."
  :group 'gas
  :type 'integer)

(defcustom gas-C-comment-end-column 0
  "Where to indent a C comment end (\"*/\") if it starts a line."
  :group 'gas
  :type 'integer)

(defcustom gas-symbol-highlight-delay 0.5
  "After this many seconds symbols get highlighted.

Number of seconds of idle time (a float) to wait before a symbol
gets highlighted."
  :group 'gas
  :type 'float)

(defgroup gas-faces nil
  "Faces used by gas-mode."
  :group 'gas)

(defface gas-builtin
  '((((class color) (background light)) (:foreground "maroon"))
    (t (:foreground "yellow")))
  "Face to use for Gas buitins."
  :group 'gas-faces)

(defface gas-symbol-ok
  '((((class color) (background light)) (:background "#e0ffe0"))
    (((class color) (background dark)) (:background "#001f00"))
    (t (:foreground "yellow" :background "blue")))
  "Face to use for symbols where exactly 1 definition was found."
  :group 'gas-faces)

(defface gas-symbol-error
  '((((class color) (background light)) 
     (:background "#ffffe8" :foreground "red" :weight bold))
    (((class color) (background dark)) 
     (:background "#181800" :foreground "red" :weight bold))
    (t (:foreground "yellow" :background "red")))
  "Face to use when highlighting symbols with more than 1 definition."
  :group 'gas-faces)

(defface gas-symbol-global
  '((((class color) (background light)) (:background "#d0f8ff"))
    (((class color) (background dark)) (:background "#00383f"))
    (t (:foreground "yellow" :background "blue")))
  "Face to use when highlighting global symbols."
  :group 'gas-faces)

(defface gas-symbol-undef
  '((((class color) (background light)) 
     (:background "#ffffe8" :foreground "maroon"))
    (((class color) (background dark)) 
     (:background "#181800"))
    (t (:foreground "yellow" :background "red")))
  "Face to use for symbols defined as global when no definition
was found."
  :group 'gas-faces)

(defface gas-symbol-global-undef
  '((((class color) (background light))
     (:background "#d0f8ff" :foreground "red"))
    (((class color) (background dark))
     (:background "#002840" :foreground "red"))
    (t (:foreground "yellow" :background "red")))
  "Face to use for symbols when no definition is found."
  :group 'gas-faces)

(defface gas-passthrough-code
  '((((class color) (background light)) (:foreground "magenta4"))
    (((class color) (background dark)) (:foreground "magenta2"))
    (t (:foreground "magenta1" :background "cyan")))
  "Marks passthrough code."
  :group 'gas-faces)

(defface gas-passthrough-comment
  '((((class color) (background light)) (:foreground "turquoise4"))
    (((class color) (background dark)) (:foreground "turquoise2"))
    (t (:foreground "turquoise1")))
  "Marks passthrough comments."
  :group 'gas-faces)

(defvar gas-builtin-face 'gas-builtin)
(defvar gas-symbol-ok-face 'gas-symbol-ok)
(defvar gas-symbol-error-face 'gas-symbol-error)
(defvar gas-symbol-global-face 'gas-symbol-global)
(defvar gas-symbol-undef-face 'gas-symbol-undef)
(defvar gas-symbol-global-undef-face 'gas-symbol-global-undef)
(defvar gas-passthrough-code-face 'gas-passthrough-code)
(defvar gas-passthrough-comment-face 'gas-passthrough-comment)

(defconst gas-max-lines-in-cache 500
  "Maximum number of parsed lines in cache.  

I don't expect much impact from this on performance (the line
cache is emptied on any buffer change anyway).  Play around with
this value if you suspect memory may be your problem.")

(defconst gas-max-labels-in-cache 300
  "Maximum number of symbols in highlight cache.  
Reduce this if memory footprint grows too high (very unlikely).")

(defconst gas-re-sym "\\([_$A-Za-z][_0-9$A-Za-z]*\\)"
  "Regexp defining a valid symbol as a subexpression.")

(defconst gas-skip-sym "_0-9$A-Za-z"
  "The valid characters for a symbol as used in `skip-chars-*' functions.")

(defconst gas-re-nosym"[^_0-9$A-Za-z]"
  "Regexp defining the character set not allowed in a symbol.")

(defvar gas-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?. "_" st)
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23" st)
    st)
  "Syntax table used while in gas mode.")

(defvar gas-mode-abbrev-table nil
  "Abbrev table used while in Gas mode.")
(define-abbrev-table 'gas-mode-abbrev-table ())

(defvar gas-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Note that the comment character isn't set up until gas-mode is called.
    (define-key map ":"		'gas-colon)
    (define-key map "\M-;"	'gas-comment)
    (define-key map ";"	        'gas-comment-char)
    (define-key map "#"		'gas-hash)
    (define-key map (kbd "<S-iso-lefttab>") 'gas-indent-backward)
    (define-key map "\C-c;"	'comment-region)
    (define-key map "\C-j"	'newline-and-indent)
    (define-key map "\C-m"	'newline-and-indent)
    map)
  "Keymap for Gas mode.")

(defconst gas-equ (regexp-opt '(".equ" ".set" ".eqv" ".equiv" ".set"))
  "Regex matching all operators which define a symbol.")

(defconst gas-indents
  '((C-comment-end . gas-C-comment-end-column)
    (C-comment-start . 0)
    (C-comment . gas-get-C-relative-indent)
    (cpp-macro-def . 0)
    (cpp-argument . 0)
    (label . 0)
    (opcode . gas-opcode-column)
    (argument . gas-argument-column)
    (asm-comment . gas-get-asm-comment-column))
  "Fields and their indents.
The cdr (the indent) may either be a number, a symbol bound to a
number, or a symbol bound to a function yielding the value.")

(defconst gas-parse-sequences
  '(
    (starting-asm-line
     . ((cpp-macro-def . cpp-macro-def)
        (label . label)
        (empty-label . label)
        (asm-comment . asm-comment)
        (opcode . opcode)))
    (label 
     . ((opcode . opcode)
        (asm-comment . asm-comment)
        (garbage . garbage)))
    (opcode 
     . ((asm-comment . asm-comment)
        (argument . argument)
        (eol-ws . eol-ws)
        (garbage . garbage)))
    (argument 
     . ((asm-comment . asm-comment)
        (eol-ws . eol-ws)
        (garbage . garbage)))
    (asm-comment 
     . ((eol-ws . eol-ws)))
    (cpp-argument 
     . ((eol-ws . eol-ws)
        (garbage . garbage)))
    (cpp-macro-def
     . ((cpp-argument . cpp-argument)
        (garbage . garbage)))
    (cpp-argument
     . ((eol-ws)
        (garbage . garbage)))
    (C-comment-start 
     . ((C-comment . C-comment)))
    (starting-within-C-comment
     . ((C-comment-end . C-comment-end)
        (C-comment . C-comment)))
    (C-comment 
     . ((C-comment-end . C-comment-end)
        (eol-ws . eol-ws)
        (garbage . garbage)))
    (garbage
     . ((eol-ws . eol-ws)))
    (eol-ws 
     . fini))
  "Mapping from the field type we're on to the field types to check next.
car - type of field we're on (IOW, the one just handled)
 cdr - ordered list of (first (most special) check first): 
    car - token to match 
    cdr - next field type iff match." )

(defconst gas-patterns
  '((cpp-macro-def
     "[ \t]*\\(\\(#[^ \t\n]*\\)\\)"
      (0        1                  1 1)
      "/[*]")
    (cpp-argument
     "[ \t]*\\(\\([ \t]*\\([^ \t\n]\\)+\\)+\\)"
      (0        1                              1 1)
      "/[*]")
    (label
     "[ \t]*\\([^ :\t\n]+:\\)"
      (0        1              1 1)
      "/[*]\\|;")
    (opcode
     "[ \t]*\\([^ \t\n]+\\)"
      (0       1            1 1)
      "/[*]\\|;")
    (argument
     "[ \t]*\\(\\([ \t]*\\([^ \t\n]\\)+\\)+\\)"
      (0        1                              1 1)
      "/[*]\\|;")
    (garbage
     "[ \t]*\\(\\([ \t]*\\([^ \t\n]\\)+\\)+\\)"
      (0        1                              1 1)
      "/[*]\\|;")
    (asm-comment
     "[ \t]*\\(\\(;+\\)\\([ \t]*[^ \t\n]+\\)*\\)"
      (0       1                               1 1)
      nil)
    (C-inline-comment
     "[ \t]*\\(/[*].*?[*]/\\)"
      (0       1                  1 1)
      nil)
    (C-comment-start
     "[ \t]*\\(/[*]C?\\)"
      (0       1      1 1)
      nil)
    (empty-label
     ;; a field of at least 1 white space: no fill at start, nil
     ;; text field at start, fill at end
     " [ \t]*"
      (0 0 0 nil)
      "/[*]\\|;")
    (C-comment-end
     "[ \t]*\\([*]/\\)"
      (0       1      1 1)
      nil)
    (C-comment
     ;; match the entire line (except for trailing whitespace)
     "[ \t]*\\(\\([ \t]*[^ \t]+\\)*\\)"
      (0 1 1 1)
      "[*]/")
    ;; fill only: zero or more white space, nil text field at
    ;; end
    (eol-ws
     "[ \t]+\\( ?\\)"
      (0 1 1 1)                         ; last element always empty
      nil))
  "An alist of parse patterns.

Each entry holds 4 elements (SYMBOL REGEXP SUBEXPS TERMINATE-RE):

  SYMBOL - designator (a symbol) under which it will be
    referenced.

  REGEXP - the regexp to match against,

  SUBEXPS - a list (BEG-COL TEXT-COL END-COL END-OF-FIELD) of at
    which subexpression of REGEXP to find beg-col, text-col,
    end-col (see `gas-parse-line-really') and the end of the
    field,

  TERMINATE-RE - a regexp, the start of which (if it matches and
    if outside a \"..\" string) unconditionally terminates the
    field.

Every occurence of the character \";\" in both regexps is
replaced by variable `gas-comment-char' before use.")

(defconst gas-elmt-types
  '(type subtype beg-col text-col end-col text modified)
  "The elements of a gas syntax field.")

(defconst gas-builtin-keywords (concat "^\\(\\(\\sw\\|\\s_\\)+:?\\)?[ \t]+\\(" 
                                       (regexp-opt '(
               ".Abort" ".ABORT" ".Align" ".Altmacro" ".Ascii" ".Asciz" 
               ".Balign" ".Byte" ".Comm" 
               ".Data" ".Def" ".Desc" ".Dim" ".Double" ".Eject" 
               ".Else" ".Elseif" ".End" ".Endef" ".Endfunc" ".Endif" 
               ".Equ" ".Equiv" ".Eqv"
               ".Err" ".Error" ".Exitm" ".Extern" ".Fail"
               ".File" ".Fill" ".Float" ".Func"
               ".Global" ".Hidden" ".hword" ".Ident" 
               ".If" ".ifb" ".ifc" ".ifeq" ".ifeqs" 
               ".ifge" ".ifle" ".ifgt" ".iflt"
               ".ifnb" ".ifnc" ".ifndef" ".ifdef" ".ifnotdef" ".ifne" ".ifnes" 
               ".Incbin" ".Include" ".Int"
               ".Internal" ".Irp" ".Irpc" ".Lcomm" ".Lflags" ".Line" 
               ".Linkonce" ".List" ".Ln" 
               ".Long" ".Macro" ".MRI" ".Noaltmacro" 
               ".Nolist" ".Octa" ".Org" ".P2align" ".PopSection" ".Previous" 
               ".Print" ".Protected" ".Psize" ".Purgem" ".PushSection" 
               ".Quad" ".Rept" ".Sbttl" ".Scl" ".Section" ".Set" ".Short" 
               ".Single" ".Size" ".Skip" ".Sleb128" ".Space" ".Stab" ".String" 
               ".Struct" ".SubSection" ".Symver" ".Tag" ".Text" ".Title" ".Type" 
               ".Uleb128" ".Val" ".Version" ".VTableEntry" ".VTableInherit" 
               ".Warning" ".Weak" ".Weakref" ".Word" ".Deprecated"))
                                       "\\)[ \t\n]"))

(defvar gas-font-lock-keywords
  (append
   (list
    '(gas-return-passthrough-code-hi . (0 gas-passthrough-code-face t))
    '(gas-return-passthrough-comment-hi . (0 gas-passthrough-comment-face t))
    '(gas-return-gas-hi-ok . (0 gas-symbol-ok-face t))
    '(gas-return-gas-hi-global . (0 gas-symbol-global-face t))
    '(gas-return-gas-hi-error . (0 gas-symbol-error-face t))
    '(gas-return-gas-hi-undef . (0 gas-symbol-undef-face t))
    '(gas-return-gas-hi-global-undef . (0 gas-symbol-global-undef-face t))
    (list gas-builtin-keywords   3 'gas-builtin-face)
    '("^\\(\\(\\sw\\|\\s_\\)+\\)\\>:?"  1 font-lock-function-name-face))
    cpp-font-lock-keywords)
  "Additional expressions to highlight in gas mode.")
(put 'gas-mode 'font-lock-defaults '(gas-font-lock-keywords))

;;;###autoload

(defun gas-mode ()
  "Major mode for editing assembler code.

Commands:

\(Some of these commands may exhibit slightly different behaviour if point
is on a C syntax line.)

\\[indent-for-tab-command]  indent the field(s) point is on.  If it already is in its position, 
  move on to the next field on the line.

\\[gas-indent-backward]  Move to the previous field.

\\[gas-comment]  When no region is active, starts a comment sequence:
  - If a comment is present and point is not at its start, jump there.
  - Else start a comment.  If there already is one, increase its comment level.
    What that is and what it does, is best explained when you try
    it out: Move to en empty line, then type \\[gas-comment] and
    then repeatedly \\[gas-comment] or \\[gas-comment-char].
  Calls \\[gas-comment-dwim] (see below) if the region is active.

\\[forward-sexp]  If you're on a highlighted symbol, jump to its next 
  occurence.  Else do `forward-sexp' like in text mode.

\\[backward-sexp]  If you're on a highlighted symbol, jump to its previous
  occurence.  Else do `backward-sexp' like in text mode.

\\[gas-comment-dwim] If the region starts at the leading white space
  before a comment, all full-line comments in region will be
  removed.  If the region starts on a comment, comments will be
  removed, but comments with leading white space will be left
  untouched.  Else insert triple variable `gas-comment-char's before all
  lines in region.

\\[fill-paragraph] beautifies the paragraph around
  point, i.e. it adjusts all assembly syntax fields to their
  standard positions.

\\[indent-region]  beautifies the region, i.e., adjusts all fields in region.


The following characters have a special meaning in special cases:

\\[gas-colon]  if it terminates a label:  outdent the label and
  move to opcode column.  Else, just insert \\[gas-colon] as usual.

\\[gas-comment-char]  The value which introduces an asm style comment.
  If typed in in a row after \\[gas-comment], behaves as an alias to \\[gas-comment].  else
  just insert \\[gas-comment-char].
  Can be customized to always act as alias (`gas-comment-char-starts-comment').

  Alternatively, you may use a File Variable to make it buffer local
  (which allows you to use different syntaxes in the same session).
  Note:  Setting it to a value other than ?\; has not yet been tested.

\\[gas-hash]  If it starts a preprocessor directive:  Outdent it to first
  column.

Customization:  Entry on this mode runs `gas-mode-hook'.
The customization group is called 'gas'.

Special commands:
\\{gas-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "gas")
  (setq major-mode 'gas-mode)
  (setq local-abbrev-table gas-mode-abbrev-table)
  (setq gas-comment-string (string gas-comment-char))
  (setq gas-comment-re (regexp-quote gas-comment-string))
  (set (make-local-variable 'indent-line-function) 'gas-indent)
  (set (make-local-variable 'indent-region-function) 'gas-indent-region)
  (set (make-local-variable 'forward-sexp-function) 'gas-forward-sexp)
  (set (make-local-variable 'fill-paragraph-function) 'gas-fill-paragraph)
  (set (make-local-variable 'font-lock-defaults) '(gas-font-lock-keywords))
  (set (make-local-variable 'gas-local-comment-char) gas-comment-char)
  (set (make-local-variable 'beginning-of-defun-function)
       'gas-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'gas-end-of-defun)
  (set (make-local-variable 'font-lock-keywords-case-fold-search) t)
  (use-local-map (nconc (make-sparse-keymap) gas-mode-map))
  (local-set-key (vector gas-comment-char) 'gas-comment-char)
  (set-syntax-table (make-syntax-table gas-mode-syntax-table))
  (modify-syntax-entry	gas-comment-char "< b")
  (dolist (var '(gas-line-cache gas-globals-cache gas-locals-cache
        gas-hi-valid gas-hi-sym-list gas-hi-global gas-hi-undef
        gas-hi-error gas-hi-ok gas-changed gas-highlights gas-hi
        gas-pass-code-hi gas-pass-comment-hi gas-symbol-timer
        gas-highlights-error gas-symbol-highlight-beg
        gas-symbol-highlight-end gas-doing-comment
        after-change-functions))
    (set (make-local-variable var) nil))
  (add-to-list 'after-change-functions 'gas-after-change)
  (add-hook 'pre-command-hook 'gas-symbol-pre-command)
  (gas-start-symbol-timer)
  (run-mode-hooks 'gas-mode-hook)
  ;; scan buffer for extra regions to highlight:
  (gas-symbol-highlight))

(defun gas-dbg ()
  "You won't need this unless you're debugging `gas-mode'."
  (interactive)
  (setq debug-on-error t)
  ;(setq debug-on-quit t)
  (setq debug-items '(indent))
  (switch-to-buffer-other-frame "*Messages*")
  (switch-to-buffer-other-frame "*scratch*")
  (info "Elisp")
  (switch-to-buffer-other-frame "*scratch*")
  (find-file-other-frame "x.S")
  (switch-to-buffer-other-frame "x.S")
  (switch-to-buffer-other-frame (get-buffer-create "*gas-dbg*"))
  (find-file-other-frame "gas-mode.el")
  (column-number-mode t)
  (switch-to-buffer-other-frame "gas-mode.el"))

(defun dmsg (condition &rest args)
  "Helper function, outputs debug messages into a buffer of their own.

If DEBUG-ITEMS (a symbol or a list) has a non-empty
intersection with CONDITION (a symbol or a list)', apply `format'
to ARGS and insert the result at the end of the buffer
`gas-dbg' (which is created if non-existing).

Currently defined symbols are: 'wip (work in progress),
'hi (highlighting), 'parser, 'cursor, 'sym (symbol highlighting),
'cursor."
  (when (and (boundp 'debug-items) debug-items)
    (when (not (listp condition))
      (setq condition (list condition)))
    (when (not (listp debug-items))
      (setq debug-items (list debug-items)))
    (when (or (eq condition '(all))
              (catch 'found
                (dolist (c condition)
                  (when (member c debug-items)
                    (throw 'found t)))))
      (let ((contents (apply 'format args)))
        (save-current-buffer
          (set-buffer (get-buffer-create "*gas-dbg*"))
          (goto-char (point-max))
          (newline)
          (insert contents)
          (goto-char (point-max))
          ;(recenter -1)
                        )))))
                    
(defun gas-change-comment-regexp (str)
  "Return STR with all \";\"s replaced by (regexp-quote variable `gas-comment-char')."
  (when str
    (setq str (replace-regexp-in-string
                ".*\\[[^]]*\\(;\\)" gas-comment-string str t t 1))
    (replace-regexp-in-string ";" (regexp-quote gas-comment-string) str t t)))

(defun gas-change-comment-string (str)
  "Return STR with all \";\"s replaced by variable `gas-comment-char'."
  (when str
    (replace-regexp-in-string ";" gas-comment-string str t t)))

(defun gas-set-patterns (comment-char)
  "Replace `gas-patterns' by a copy, replacing ?\; by COMMENT-CHAR.
Also sets variable `gas-comment-char' to COMMENT-CHAR."
  (set (make-local-variable 'gas-comment-char) comment-char)
  (set (make-local-variable 'gas-comment-string) (string gas-comment-char))
  (set (make-local-variable 'gas-comment-re) (regexp-quote gas-comment-string))
  (kill-local-variable 'gas-patterns)
  (let (result)
    (dolist (pattern gas-patterns)
      (add-to-list 'result
        (list
         (car pattern)
         (gas-change-comment-regexp (nth 1 pattern))
         (nth 2 pattern)
         (gas-change-comment-regexp (nth 3 pattern)))))
  (set (make-local-variable 'gas-patterns) result)
  (dolist (sym '(gas-defun-regexp))
    (kill-local-variable sym)
    (let ((result (gas-change-comment-regexp (eval sym))))
      (set (make-local-variable sym) result)))))

(defun gas-after-change (beg end len)
  "Invalidate saved parser state.
Argument BEG BEG, END, and LEN, athough saved in `gas-changed' for debugging purposes, are not used."
  (setq gas-globals-cache nil)
  (setq gas-hi-valid nil)
  (setq gas-locals-cache nil)
  (setq gas-changed (append gas-changed (list (current-buffer) beg end len))))

(defun gas-symbol-pre-command()
  (setq gas-hi nil))

(defun gas-start-symbol-timer (&optional stop)
  "Schedule a timer for symbol highlighting (if not already scheduled).
Optional STOP, if non-nil,  means remove from schedule."
  (if (and stop gas-symbol-timer)
      (progn
        (cancel-timer gas-symbol-timer)
        (setq gas-symbol-timer nil))
    (unless (and gas-symbol-timer
                 (memq gas-symbol-timer timer-idle-list))
      (setq gas-symbol-timer
            (run-with-idle-timer gas-symbol-highlight-delay
                                 t
                                 'gas-symbol-highlight-maybe)))))

(defun gas-return-passthrough-hi (pos what)
  "Return next passthrough match (if any).
See highlight.el for POS and WHAT."
  (let ((curpoint (point)))
    (catch 'found
      (dolist (match what)
        (when (and (< curpoint (cdr match))
                   (> pos (car match)))
          (when (< curpoint (car match))
            (goto-char (car match)))
          (when (re-search-forward ".+" (min pos (cdr match)) t)
            (dmsg 'hi "match: %s, data: '%s'" match (match-string 0))
            (throw 'found t)))
        (when (>= (car match) pos)
          (throw 'found nil))))))

(defun gas-return-passthrough-code-hi (pos)
  "Return next passthrough code match (if any).
See highlight.el for documentation on POS."
  (gas-return-passthrough-hi pos gas-pass-code-hi))

(defun gas-return-passthrough-comment-hi (pos)
  "Return next passthrough comment match (if any).
See highlight.el for documentaion on POS ."
  (gas-return-passthrough-hi pos gas-pass-comment-hi))

(defun gas-passthrough-highlight ()
  "Compute blocks of C passthroughs to be highlighted."
  (goto-char (point-min))
  (setq gas-pass-code-hi nil)
  (setq gas-pass-comment-hi nil)
  (let (beg end limit)
    (while (and (re-search-forward "/[*]C" nil t)
                (setq beg (point))
                (setq limit 
                      (save-excursion 
                        (and (re-search-forward "[*]/" nil t) 
                             (- (point) 2)))))
      (while (and beg
                  (setq end (and (re-search-forward "/#" limit t)
                                 (point))))
        (add-to-list 'gas-pass-code-hi (cons beg (- end 2)) t)
        (forward-char 2)
        (when
            (setq beg (and (re-search-forward "#/" limit t) (point)))
          (add-to-list 'gas-pass-comment-hi (cons (- end 2) beg) t)))
      (when beg
        (add-to-list 'gas-pass-code-hi (cons beg limit) t)))))

(defun gas-return-highlight (pos hi-list)
  "Called through the gas-return-gas-hi-* functions by highlight.el.
If there's a match of POS against one of the entries in HI-LIST,
return match data.  Else, return nil."
  (when gas-hi-valid
    (dmsg 'hi "gas-return-highlight, pos: %s, point: %s, list: %s" pos (point) hi-list)
    (catch 'found
      (dolist (match hi-list)
        (if (< (car match) pos)
            (when (>= (car match) (point))
              (dmsg 'hi "gas-return-highlight, match: %s" match)
              (goto-char (car match))
              (throw 'found (re-search-forward ".+" (min pos (cadr match)) t)))
          (dmsg 'hi "gas-return-highlight: nope.")
          (throw 'found nil)
          nil)))))

(defun gas-return-gas-hi-global (pos)
  "Check POS against the entries in the list `gas-hi-global'.
Called by highlight.el."
  (gas-return-highlight pos gas-hi-global))

(defun gas-return-gas-hi-undef (pos)
  "Check POS against the entries in the list `gas-hi-undef'.
Called by highlight.el."
  (gas-return-highlight pos gas-hi-undef))

(defun gas-return-gas-hi-error (pos)
  "Check POS against the entries in the list `gas-hi-error'.
Called by highlight.el."
  (gas-return-highlight pos gas-hi-error))

(defun gas-return-gas-hi-ok (pos)
  "Check POS against the entries in the list `gas-hi-ok'.
Called by highlight.el."
  (gas-return-highlight pos gas-hi-ok))

(defun gas-return-gas-hi-global-undef (pos)
  "Check POS against the entries in the list `gas-hi-undef'.
Called by highlight.el."
  (gas-return-highlight pos gas-hi-global-undef))


(defun gas-qualify-symbol (sym-re slist lflags)
  "The common part of `gas-scan-global-symbol and `gas-scan-local-symbol'.

Gets called with point on a line where a label match may be
found.  It expects SYM-RE to be a regexp describing the
label.  Adds on match the match it finds to SLIST (a symbol bound
to a list of matches) and the kind of match (as a symbol, like
'def for a definition or 'duplicate for a duplicate definition)
to LFLAGS."
  (save-excursion
    (let ((eol (line-end-position))
          (bol (line-beginning-position)))
      (setq case-fold-search nil)
      (if (gas-C-comment-p)
          (progn
            (backward-char)
            (while (re-search-forward 
                    (format "%s\\(%s\\)\\(%s\\|$\\)"
                            gas-re-nosym sym-re gas-re-nosym)
                    eol t)
              (let ((beg (match-beginning 1))
                    (end (match-end 1)))
                (when (gas-C-passthrough-code-p)
                  (add-to-list lflags 'C-ref t)
                  (add-to-list slist (list 'C-ref beg end) t)))))
        ;; not C style:
        (let* ((fields (gas-parsed))
               (lbl (gas-nth 'text 'label fields))
               (arg (gas-nth 'text 'argument fields))
               (type 'ref)
               (argno 0))
          (when (and lbl 
                     (string-match (concat "^" sym-re ":?$") lbl))
            (setq nlabels (1+ nlabels))
            (if (member 'def (eval lflags))
                (add-to-list lflags 'duplicate t)
              (add-to-list lflags 'def t))
            (move-to-column 0)
            (looking-at "[^:]+:")
            (add-to-list slist  
                         (list 'def (match-beginning 0) (match-end 0)) t))
          (when arg
            (move-to-column (- (gas-nth 'text-col 'argument fields) 1))
            (let ((type 'ref)
                  (eo-arg (+ bol (gas-nth 'end-col 'argument fields) 2))
                  (opcode (gas-nth 'text 'opcode fields)))
              (while (re-search-forward 
                      (format "%s\\(%s\\)\\(%s\\|$\\)"
                              gas-re-nosym sym-re gas-re-nosym)
                      eo-arg t)
                (let ((beg (match-beginning 1))
                      (end (match-end 1)))
                  ;; first argument? 
                  (if (and (= argno 0)                  ; yes
                           (string-match gas-equ opcode)) ; assignment?
                      (progn
                        (setq type 'def)  ; yes
                        (if (member 'def (eval lflags))
                            (add-to-list lflags 'duplicate t)
                          (add-to-list lflags 'def t)))
                    (add-to-list lflags 'ref t)) ; no
                  (when (string-match ".global" opcode)
                    (add-to-list lflags 'global t))
                  (add-to-list slist (list type beg end) t)
                  (setq argno (1+ argno))))))))))
    ;; skip past parsed part:
  (end-of-line))

(defun gas-scan-global-symbol (sym)
  "Scan the buffer vor valid occurences of the global symbol SYM.
Called by `gas-symbol-highlight'."
  (goto-char (point-min))      
  (let* (sym-list 
         flags
         qualifiers
         (nlabels 0)
         (sym-re (regexp-quote sym))
         (re (format "\\(^\\|%s\\)\\(%s\\)\\(%s\\|$\\)"
                     gas-re-nosym sym-re gas-re-nosym)))
    (while (re-search-forward re nil t)
      (when (input-pending-p)
        (throw 'event-abort nil))
      (goto-char (match-beginning 2))
      (dmsg 'sym "global: qualified1: %s" sym-list)
      (gas-qualify-symbol sym-re 'sym-list 'flags))
    (dmsg 'sym "global: sym-list: %s" sym-list)
    (unless (equal sym-list '(nil))
      (list sym flags sym-list))))

(defun gas-scan-local-symbol (orig-sym)
  "Scan the buffer vor valid occurences of the local symbol SYM.
Called by `gas-symbol-highlight'.
Argument ORIG-SYM is the complete symbol (as written)."
  (when (string-match "^\\(.*\\)\\([:bf]\\)" orig-sym)
    (let* ((search-lo (point-min)) ; location of previous duplicate         
           search-mid                   ; label pos
           (search-hi (point-max))      ; location of next duplicate
           (sym (match-string 1 orig-sym))
           (sym-kind (match-string 2 orig-sym))
           (sym-re (regexp-quote sym))
           (lbl-re (concat "^\\(" sym-re ":" "\\)" ))
           (nlabels 0)
           sym-list
           flags 
           qualifiers 
           searches)
      ;; determine region where the label is valid
      (save-excursion
        (when (equal sym-kind "b")
          (re-search-backward lbl-re nil t)) ; skip label
        (when (re-search-backward lbl-re nil t)
          (setq search-lo (match-end 1)))
        (goto-char search-lo)
        (when (re-search-forward lbl-re nil t)
          (setq search-mid (match-beginning 1)))
        (when (re-search-forward lbl-re nil t)
          (setq search-hi (match-beginning 1))))
      (let ((lo search-hi)              ; first match
            (hi search-lo)              ; end of last match
            (search-params 
             (if search-mid
                 (list 
                  (list (concat  sym-re "f") search-lo search-mid)
                  (list (concat  sym-re ":")
                        search-mid  (+ search-mid (length sym) 2))
                  (list (concat  sym-re "b") search-mid search-hi))
               (list
                (list (concat  sym-re "f") search-lo search-hi)
                (list (concat  sym-re "b") search-lo search-hi)))))
        (dolist (param search-params)
          (when (input-pending-p)
            (throw 'event-abort nil))
          (let* ((sym-re (car param))
                 (search-re (concat gas-re-nosym
                                    "\\(" sym-re "\\)" gas-re-nosym))
                 (limit (caddr param)))
            (goto-char (- (cadr param) 1))
            (while (and (< (point) limit)
                        (re-search-forward search-re limit t))
              (setq lo (min lo (match-beginning 1)))
              (setq hi (max hi (match-end 1)))
              (goto-char (match-beginning 1))
              (gas-qualify-symbol sym-re 'sym-list 'flags))))
        (dmsg 'sym "local: sym-list: %s" sym-list)
        (unless (equal sym-list '(nil))
          (list sym lo hi flags sym-list))))))
            
(defun gas-sym-invalidate ()
  "Invalidate all symbol scan results."
  (setq gas-hi-sym-list nil)
  (setq gas-hi-global nil)
  (setq gas-hi-global-undef nil)
  (setq gas-hi-undef nil)
  (setq gas-hi-error nil)
  (setq gas-hi-ok nil))

(defun gas-symbol-highlight ()
  "Get symbol point is on, look for match and highlight it.
For use with the idle timer."
  ;; To debug this part, stop the idle timer mechanism first:
  ;; (gas-start-symbol-timer 'stop)
  ;; then eval it.
  (save-match-data
    (save-excursion
      (gas-passthrough-highlight))
    (save-excursion
      (let* ((fields (gas-parsed))
             (curpoint (point))
             (case-fold-orig case-fold-search)
             (pointpos (gas-get-pointpos fields))
             sym-list)
        (catch 'event-abort
          (dmsg 'hi "pointpos=%s, fields=%s" pointpos fields)
          ;; skip to the symbol's start
          (skip-chars-backward gas-skip-sym)
          (when (member (car pointpos) '(label argument C-comment))
            (setq case-fold-search nil)
            (setq sym-list
                  (catch 'found
                    ;; local label?
                    (if (and (not (eq (car pointpos) 'C-comment))
                             (looking-at (concat "\\(\\([0-9]+\\)\\([bf:]\\)\\)"
                                                 gas-re-nosym)))
                        (let ((lbl (match-string 2)))
                          ;; local label
                          (dolist (entry gas-locals-cache)
                            (when (input-pending-p)
                              (throw 'event-abort nil))
                            (and (equal (car entry) lbl)
                                 (>= (nth 1 entry) curpoint)
                                 (<= (nth 2 entry) curpoint)
                                 (dolist (sym (nthcdr 3 entry))
                                   (and (>= (nth 1 entry) curpoint)
                                        (<= (nth 2 entry) curpoint)
                                        (throw 'found (nthcdr 3 entry)))))
                            nil)
                          ;; not found in cache
                          (let ((entry (gas-scan-local-symbol (match-string 1))))
                            (when entry
                              (when (> (length gas-locals-cache)
                                       gas-max-labels-in-cache)
                                (dmsg 'sym "gas-locals-cache truncated.")
                                (nbutlast gas-locals-cache
                                          (/ (* gas-max-labels-in-cache 3) 4)))
                              (add-to-list 'gas-locals-cache entry)
                              (throw 'found (nthcdr 3 entry)))))
                      ;; global label:
                      (when (looking-at (format "\\(%s+\\)%s"
                                                gas-re-sym gas-re-nosym))
                        (setq lbl (match-string 1))
                        (when (setq entry (assoc lbl gas-globals-cache))
                          (throw 'found (cdr entry)))
                        ;; not found in cache
                        (let ((entry (gas-scan-global-symbol lbl)))
                          (when entry
                            (when (> (length gas-globals-cache)
                                     gas-max-labels-in-cache)
                              (dmsg 'sym "gas-globals-cache truncated.")
                              (nbutlast gas-globals-cache
                                        (/ (* gas-max-labels-in-cache 3) 4)))
                            (add-to-list 'gas-globals-cache entry)
                            (throw 'found  (cdr entry))))))))
        
            (dmsg 'sym "gas-symbol-highlight: Matched: %s" sym-list)
            (gas-sym-invalidate)
            (setq gas-hi-sym-list sym-list)
            (let ((flags (car sym-list))
                  target-list)
              (dolist (sym (cadr sym-list))
                (when (input-pending-p)
                  (throw 'event-abort nil))
                (setq target-list
                      (cond
                       ((member 'duplicate flags) 'gas-hi-error)
                       ((and (member 'global flags) (member 'def flags))
                        'gas-hi-global)
                       ((member 'global flags) 'gas-hi-global-undef)
                       ((eq (car sym) 'C-ref) nil)
                       ((and (member 'def flags) (member 'ref flags))
                        'gas-hi-ok)
                       ((> (length (cadr sym-list)) 1) 'gas-hi-undef)))
                (dmsg 'sym "target-list, sym: %s, list: %s" sym target-list)
                (when target-list
                  (add-to-list target-list (cdr sym) t))))
            (setq gas-hi-valid t)
            (font-lock-fontify-buffer)))
        (setq case-fold-search case-fold-orig)))))

(defun gas-symbol-highlight-maybe ()
  "Check for a symbol at point to be highlighted.
For use with the idle timer."
  (unless (or (not gas-enable-symbol-highlight ) (input-pending-p))
    (undo-boundary)                     ; probably redundant
    (gas-symbol-highlight)
    (undo-boundary)))                   ;   -"-


(defun gas-C-comment-p ()
  "True if we're editing a C syntax comment (the one enclosed in /* */)."
  (save-excursion
    (let ((current (point)))
      (cond ((not (re-search-backward "/\\*" 0 t)) nil)
            ((not (re-search-forward "\\*/" current t)))))))

;; format of a C passthrough:
;; --- asm
;; /*C
;;   --- C passthrough code
;;   /#
;;      --- C passthrough comment
;;   #/
;;   ---- C passthrough code
;;   [... passthrough code, comment as above ...]
;; */  
;; --- asm
(defun gas-C-passthrough-p ()
  "True if we're editing a C passthrough (a C style comment enclosed in /*C ...
*/)"
  (when gas-use-C-passthrough
    (save-excursion
      (let ((current (point)))
        (and (re-search-backward "/\\*C" 0 t)
             (not (re-search-forward "\\*/" current t)))))))

(defun gas-C-passthrough-comment-p ()
  "True if we're editing a C passthrough comment (/# ...
#/ within a /*C ... */ comment)"
  (when gas-use-C-passthrough
    (save-excursion
      (let ((current (point)))
        (when (gas-C-passthrough-p)
          (save-restriction
            (prog2
                (gas-narrow-to-C-comment)
                (and (re-search-backward "/\\#" 0 t)
                     (not (re-search-forward "[#*]/" current t))))))))))


(defun gas-C-passthrough-code-p ()
  "True if we're editing C passthrough code.
i.e. we're within a /*C ...*/ comment but not within /# ... #/."
  (when gas-use-C-passthrough
    (and (gas-C-passthrough-p)
         (not (gas-C-passthrough-comment-p)))))

(defun gas-C-comment-really-p ()
  "True if we're in a standard C commont or in a nested passthrough C comment."
  (and (gas-C-comment-p)
       (not (gas-C-passthrough-code-p))))

(defun gas-comment-p ()
  "True if we're editing some kind of comment."
  (or (looking-back (concat gas-comment-string ".*"))
      (gas-C-comment-p)))

(defun gas-narrow-to-C-comment ()
  "Narrow region to the C comment point resides in."
  (unless (gas-C-comment-p)
    (error "Not within C comment"))
  (save-excursion
    (let* ((beg (re-search-backward ".*/[*]"))
           (end (re-search-forward "[*]/" nil t)))
      (when end
        (narrow-to-region beg end)))))


(defun gas-token-pattern (cur-check)
  "Return a list how to parse for token `CUR-CHECK'.
The returned list holds three elements, see the documentation
of `gas-patterns' for an explanation."
  (let ((pattern (cdr (assq cur-check gas-patterns))))
    (when (not pattern)
      (t (error "Gas internal: illegal pattern %s requested"
                cur-check)))
    pattern))

(defun gas-next-token (check-list)
  "Parse for one of the tokens in list `CHECK-LIST'.
`CHECK-LIST' is a list of cons cells (TOKEN-SYMBOL . RESULTING-TYPE).

Look up each TOKEN-SYMBOL in `gas-patterns' in order, then match
the text at point against the two regexps found there.  On match,
eat the match (advance point past it) and return the resulting
field with its car set to RESULTING-TYPE (which is the same as
TOKEN-SYMBOL in nearly all cases except for the final parser
state, 'fini (and may be nil if same)).  If none matches,
return nil.

Valid TOKEN-SYMBOLS and RESULTING-TYPEs are the ones listed in
'gas-patterns'."

  (save-match-data 
    (catch 'got-token
      (dolist (cur-check check-list)
        (save-restriction
          (let* ((pattern-list (gas-token-pattern (car cur-check)))
                 (regexp (pop pattern-list))
                 (subexps (pop pattern-list))
                 (limit-re (pop pattern-list))
                 limit
                 (subtype nil))
            (setq limit (line-end-position))
            (when (and limit-re
                       (looking-at
                        (format "\\(\\(\"[^\n\"]*\"\\)\\|[^\n\"]\\)*?\\(%s\\)"
                                limit-re)))
              (setq limit (match-beginning 3)))
            (when (>= limit (point))
              (narrow-to-region (point) limit)
              (when (looking-at regexp)      ; match pattern
                (cond
                 ((eq (cdr cur-check) 'asm-comment)
                  ;; set subtype to the number of consecutive comment-chars
                  (setq subtype (- (match-end 2) (match-beginning 2))))
                 ((eq (cdr cur-check) 'argument)
                  ;; set subtype to be a list holding the positions of the
                  ;; individual subexpressions (relative to start of text)
                  (let ((arg-beg (point))
                        (arg-end (match-end 0)))
                    (setq subtype '(0))
                    (save-excursion
                      (save-match-data
                        (while (re-search-forward "," arg-end t)
                          (add-to-list 'subtype (- (point) arg-beg 1) t)))))))
                (goto-char (min limit (match-end 0)))
                (throw 'got-token
                       (list (cdr cur-check)
                             subtype
                             (- (min limit (match-beginning (pop subexps))) curbol)
                             (- (min limit (match-beginning (pop subexps))) curbol)
                             (- (min limit (match-end (pop subexps))) curbol)
                             (let ((subexp (pop subexps)))
                               (and subexp
                                    (not (equal (match-string subexp) ""))
                                    (match-string subexp)))
                           nil))))))))))






(defun gas-parse-line-really ()
  "Parse the line point is on, element by element.
The returned list holds a list of syntactic elements (fields)
found, in the order of appearance.  Each syntactic element is
represented by a list holding 7 elements:

'(TYPE SUBTYPE BEG-COL TEXT-COL END-COL TEXT MODIFIED)
    0 - 'type: ('label, 'opcode ...)
    1 - 'subtype: (additional information required by some types)
    2 - 'beg-col: first slot (column #) (usually occupied by white 
                  space before text)
    3 - 'text-col: first text slot (column #)
    4 - 'end-col: first free slot after field (column #)
    5 - 'text: text contents (nil if empty)
    6 - 'modified: nil (will later reflect if rearrangement (indentation) 
        is required / was done).

Terminology: A single list of these 7 elements is called a
`field' throughout gas-mode.el.  `fields' as symbol name (or part
of a symbol name) means it stands for a variable bound to a list
of `field' lists which, in order of appearance, describe a
complete source line.

Note that text-col may lie outside (on the right side of) the
range beg-col ... end-col if there's no actual text (it is the
column where text would have to go to).

Don't assume a fixed element position except for 'type, which is
always the car of the list.  Instead, use `gas-nth' to extract
elements by their symbolic names ('type, 'subtype, ...)."

  ;; We use syntax tables neither here nor much throughout the whole
  ;; gas-mode, since assembler code, being line oriented and based on
  ;; number and position of elements on the line, fits regexps better
  ;; than syntax tables (IMHO).  The only drawback is that this
  ;; approach makes dealing with inline C-style comments embedded
  ;; between asm fields (who the hell does that?)  somewhat clumsy,
  ;; but not by much.
  (let (field
        (f-type 'start)
        pushed-f-type
        (C-inline-comment-level 0)
        fields)

    (save-excursion
      (setq cureol (line-end-position))
      (beginning-of-line)
      (setq curbol (point))

      (setq f-type
            (if (gas-C-comment-p)
                (progn
                  (setq pushed-f-type 'starting-asm-line)
                  'starting-within-C-comment)
            'starting-asm-line))
      (while f-type
        ;; always check for C-comment first (except we're already
        ;; inside)
        (setq field 
              (and (not (gas-C-comment-p))
                   (gas-next-token 
                    '((C-inline-comment . C-inline-comment)
                      (C-comment-start . C-comment-start)))))
        (unless field
          ;; dispatch based on previous field
          (let ((next-check (cdr (assq f-type gas-parse-sequences))))
            (when (not next-check)
              (error "Gas-mode internal: gas-parse-line: %s" f-type))
            (when (not (eq next-check 'fini))
              ;; parse:
              (setq field (gas-next-token next-check)))
            (when (eq (car field) 'garbage)
              (error "Gas-mode internal: garbage: %s" field))
            (dmsg 'parser "parsed a field: %s->%s" f-type field)))
        (when (eq (car field) 'C-inline-comment)
          (setcar field (list 'C-inline-comment C-inline-comment-level))
          (setq C-inline-comment-level (1+ C-inline-comment-level)))
        (when (and (eq 'asm-comment (car field))
                   (assq 'opcode fields))
          (gas-set-nth 'subtype field 1))
        (when field
          (add-to-list 'fields field t))
        (when (eq (car field) 'C-comment-start) ; push state
          (setq pushed-f-type f-type))
        (unless (and field (listp (car field))) ; not C-inline-comment
          (setq f-type (car field)))
        (when (eq f-type 'C-comment-end)   ; pop state
          (setq f-type pushed-f-type))
        (when (listp f-type)
          (setq f-type (car f-type)))))
    
    
    (dmsg '(parser cursor) "parsed all fields: %s" fields)
    fields))


(defun gas-get-field (field-type field-list &optional offset)
  "Return field of specified type.

Return field (a list) with first element (syntactic type) eq
to FIELD-TYPE from FIELD-LIST, nil if no such field.  

Optional OFFSET >= 0: return next field, OFFSET < 0: return
previous field.  If OFFSET is given.  both fields (FIELD-TYPE and
the one before/after) must exist, else nil is returned."
  (if offset
      (let (f-types
            f-type)
        (dolist (field field-list)
          (add-to-list 'f-types (car field) t))
        (if (> offset 0)
            (while (not (eq (pop ftypes) field-type)))
          (while (and (setq f-type (nth 1 f-types))
                      f-type
                      (not (eq f-type field-type)))
            (pop f-types)))
        (gas-get-field (car f-types) field-list))
    (assq field-type field-list)))

(defun gas-nth (component field-from &optional field-list)
  "Get one of the components of a field.

COMPONENT is the component to be returned.  FIELD-FROM is either
a list containing a field, or a symbol designating a field (see
`gas-parse-line-really' for both).  

In the latter case, optional FIELD-LIST must be supplied as a
list of fields where FIELD-FROM is extracted from."
  (let ((field-from (if field-list
                        (gas-get-field field-from field-list)
                      field-from)))
    (nth (gas-elmt-n component) field-from)))

(defun gas-elmt-n (elmt)
  "Return the numeric index of field component ELMT."
  (when (symbolp elmt)
    (- (length gas-elmt-types) (length (memq elmt gas-elmt-types)))))
  
(defun gas-set-nth (elmt field val-or-fields &optional val)
"Set a single field component.

ELMT is the content to set the field to.
If FIELD is a list containing a single field, set its ELMT (a
symbol) to VAL-OR-FIELDS.  If FIELD is a symbol designating a
field and VAL-OR-FIELDS is a list of such fields (as returned by
`gas-parsed'), set ELMT in the FIELD field of VAL-OR-FIELDS to
VAL."
  (let ((curfield field))
    (if val
        (setq curfield (assq field val-or-fields))
      (setq val val-or-fields))
    (setcar (nthcdr (gas-elmt-n elmt) curfield) val)))

(defun gas-rearrange (fields &optional elmt f-type val)
  "Rearrange FIELDS so they fit together without gaps or overlapping.

FIELDS is the field list describing the current line.

Optional ELMT designates a field component in the field specified
by (eq to) F-TYPE, which has to be changed before rearrangement
to VAL.  VAL may be a variable, a symbol which evaluates to the
taget value, or bound to a function returning the target type of
the component.

If the line did change, return the field type (the car) of the
first field that has changed, else nil."
  (let ((field-slot 0)                   ; next free slot for a field
        (text-slot 0)            ; next column where text should go to
        (end-slot 0)
        did-change)
    (dolist (field fields)
      (let ((old-col (gas-nth 'text-col field))
            (text (gas-nth 'text field)))

        (gas-set-nth 'beg-col field field-slot)
        ;; insert new value if needed
        (when (and val (eq f-type (gas-nth 'type field)))
          (gas-set-nth elmt field val))
        ;; determine text column
        (let ((tcol (gas-nth 'text-col field)))
          ;; if the current text column is nil and we are reordering:
          ;; put in its default value.
          (when (or (eq elmt 'all)
                    (and (not val) (eq f-type (car field))))
            (setq tcol (cdr (assq (car field) gas-indents))))
          ;; if column is a symbol: replace it by its value
          (when (and tcol (symbolp tcol))
            (setq tcol (if (fboundp tcol)
                           (funcall tcol)
                         (eval tcol))))
          ;; if we still have got no target value: use the next free
          ;; text slot
          (unless tcol
            (setq tcol text-slot))
          ;; set text col to max (current, next-free-slot)
          (setq tcol (max tcol text-slot))
          (gas-set-nth 'text-col field tcol)
          ;; update `modified' flags:
          (when (and text
                     (not (eq tcol old-col)))
            (unless did-change
              (setq did-change (car field)))
            (gas-set-nth 'modified field t))
          (when text
            (setq field-slot (+ (gas-nth 'text-col field) (length text))))
          (setq end-slot (max end-slot (or (gas-nth 'end-col field) 0)))
          (gas-set-nth 'end-col field field-slot)
          ;; determine next free slot
          (setq text-slot (max tcol text-slot))
          (when (/= 0 field-slot)
            (setq text-slot (max text-slot  (1+ field-slot)))))))
    (gas-set-nth 'end-col 'eol-ws fields end-slot)
    did-change))


(defun gas-reset-modified (fields)
  "Clears the `modified' flag on all fields in FIELDS."
  (dolist (field fields)
    (gas-set-nth 'modified field nil)))

(defun gas-get-pointpos (ffields)
  "Determine the field in FFIELDS point is on.
Returns a list (F-TYPE SUBTYPE OFFSET-TO-FIELD-TEXT-BEGIN ABS-COLUMN EOL).
EOL may bei either the symbol 'eol or nil"
  (let* ((curcol (current-column))
         (fields (copy-tree ffields))
         (ppos (list (caar fields) curcol)))
    (catch 'pos-found
      (dolist (field fields)
        ;; first, look for a field match
        (when (and (gas-nth 'text field)
                   (>= curcol (gas-nth 'beg-col field))  ; field start
                   (<= curcol (gas-nth 'end-col field))) ; field end
          (setq ppos (list (car field) (- curcol (gas-nth 'text-col field))))
          (throw 'pos-found t)))
      ;; no field match: look at position relative to text slot
      (gas-rearrange fields 'all)
      (dolist (field fields)
        (when (or (<= curcol (gas-nth 'text-col field))
                  (eq (car field) 'eol-ws))
          (setq ppos (list (car field) (- curcol (gas-nth 'text-col field))))
          (throw 'pos-found t))))
    (setq ppos (append ppos (list curcol)))
    (if (>= curcol (gas-nth 'end-col 'eol-ws fields))
        (append ppos '(eol))
      ppos)))

(defun gas-get-C-relative-indent ()
  "Return indent of the closest previous non-blank line in current C comment."
  (if (not (gas-C-comment-p))
      gas-C-indent
    (save-restriction
      (gas-narrow-to-C-comment)
      (let ((col
             (save-excursion
               (catch 'found
                 (if (looking-at "[ \t]*#/")
                     ;; end passthrough:  same indent as start
                     (while (= 0 (forward-line -1))
                       (when (looking-at "[ \t]*/#")
                         (throw 'found (current-indentation))))
                   (while (= 0 (forward-line -1))
                     (when (looking-at "[ \t]*/[*#]")
                       (throw 'found (+ gas-C-indent (current-indentation))))
                     (when (looking-at "[ \t]*[^ \t\n]")
                       (throw 'found (current-indentation)))))
                 gas-C-indent))))
        (when (gas-C-passthrough-code-p)
          (if (looking-at "[ \t\n]*}[ \t\n]*;")
              (setq col (max 0 (- col gas-C-indent)))))
        col))))

(defun gas-add-missing-fields (parsed-fields)
  "Complete parser result by adding in empty but allowed fields.

Determine line syntax from the parse result in PARSED-FIELDS,
then add in empty fields for syntactic components not used in the
source line but allowed by the syntax, so later code can safely
assume they are present."
  ;; Let's qualify what we've got
  (let (existing-types mandatory line-syntax new-fields f-type cur-type)
    ;; determine mandatory fields:
    (setq mandatory
          (cond ((not parsed-fields)    ; empty line
                 '(label opcode argument asm-comment eol-ws))
                ((assq 'C-comment parsed-fields)
                 '(C-comment-start C-comment C-comment-end eol-ws))
                ((assq 'cpp-macro-def parsed-fields)
                 '(cpp-macro-def cpp-argument eol-ws))
                (t 
                 (let ((subtype (gas-nth 'subtype 'asm-comment parsed-fields)))
                   (cond
                    ((eq subtype 3) '(asm-comment eol-ws))
                    ((eq subtype 2) '(label asm-comment eol-ws))
                    (t '(label opcode argument asm-comment eol-ws)))))))
    (dmsg '(parser cursor) "mandatory: %s" mandatory)
    ;; We're going through all fields in `fields', inserting mandatory
    ;; elements (from `mandatory') if not present
    (dolist (field parsed-fields)
      (setq f-type (car field))
      (if (member f-type mandatory)
          ;; we have a mandatory field.  Insert missing fields (if
          ;; any) before it, then insert the field at its place.
          (progn
            ;; insert an empty field for each mandatory but
            ;; not already exisiting element before the one
            ;; we're dealing with.
            (while (progn
                     (setq cur-type (pop mandatory))
                     (not (eq cur-type f-type)))
              (dmsg 'parser "adding missing mandatory: %s" cur-type)
              (add-to-list 'new-fields
                           (list cur-type nil nil nil nil nil nil) t))
            ;; now insert field
            (dmsg 'parser "adding existing mandatory: %s" field)
            (add-to-list 'new-fields field t))
        ;; we have a non-mandatory field.  Pass it through,
        ;; keeping its place if possible.
        (add-to-list 'new-fields field t)))
    ;; done with parsed fields.
    ;; add mandatory elements left out at right
    (while (setq cur-type (pop mandatory))
      (dmsg 'rearranged "adding left-over: %s" cur-type)
      (add-to-list 'new-fields
                           (list cur-type nil nil nil nil nil nil) t))
    (dmsg 'parser "new-fields=%s" new-fields)
    new-fields))

(defun gas-parsed (&optional lineno)  
  "Return fields on line LINENO or, if nil, on current line.
Return cached data, if available.  Else, call
`gas-parse-line-really', store result into the gas-line-cache and
return it."
  (dmsg 'parser "gas-changed: %s, lenght gas-line-cache: %s"
        gas-changed (length gas-line-cache))
  (unless (and (eq gas-comment-char ?\;)
               (eq gas-local-comment-char gas-comment-char))
       (setq gas-local-comment-char gas-comment-char)
       (gas-set-patterns gas-comment-char))
  (when gas-changed
    (setq gas-line-cache nil)
    (setq gas-changed nil))
  (when (> (length gas-line-cache) gas-max-lines-in-cache)
    (dmsg 'parser "gas-line-cache truncated.")
    (nbutlast gas-line-cache (/ (* gas-max-lines-in-cache 3) 4)))
  (let* ((curline (or lineno (line-number-at-pos)))
         (cached (assq curline gas-line-cache)))
    (if cached
        (cadr cached)
      (let (fields)
        (save-excursion
          (goto-line curline)
          (setq fields (gas-parse-line-really))
          (setq fields (gas-add-missing-fields fields))
          (gas-rearrange fields)
          (dmsg 'parser "->gas-line-cache: %s" fields)
          (add-to-list 'gas-line-cache (list curline fields)))
        fields))))

(defun gas-put-parsed (fields lineno)
  "Put FIELDS into gas-line-cache at LINENO.
Overwrites the previous entry for that line (if any)."
  (gas-reset-modified fields)
  (let ((current (assq lineno gas-line-cache)))
    (if current
        (setcdr current (list fields))
      (add-to-list 'gas-line-cache (list curline fields)))))
      
(defun gas-put-out (&optional fields)
  "Replace the current scrren line by the one described by FIELDS."
  (when (not fields)
    (setq fields (gas-parsed)))
  (dmsg 'indent "put-out, fields: %s" fields)
  (combine-after-change-calls
    (save-excursion
      (beginning-of-line)
      (delete-region (point) (line-end-position))
      (dolist (field fields)
        (when (and (gas-nth 'text field)
                   (or gas-preserve-trailing-whitespace
                       (not (eq (car field) 'eol-ws))))
          (indent-to (gas-nth 'text-col field))
          (insert (gas-nth 'text field)))))))

(defun gas-get-asm-comment-column ()
  "Get comment column according to asm comment subtype on current line.
The caller must provide the variable `fields' (bound to current
line's parsed content)."
  (let ((subtype (gas-nth 'subtype 'asm-comment fields)))
    (cond ((not subtype) gas-comment-column)
          ((= subtype 1) gas-comment-column)
          ((= subtype 2) gas-opcode-column)
          (t 0))))

(defun gas-set-point-to-field (f-type fields)
  "Postition point to the start of the field of type F-TYPE.
FIELDS is a field list reflecting the current line."
  (let ((target-col (gas-nth 'text-col f-type fields)))
    (move-to-column target-col)
    (indent-to target-col)
    (when (eq f-type 'asm-comment)
      (skip-chars-forward (format " %c" gas-comment-char))
      (when (not (looking-back " "))
        (insert-char ?\s 1)))))

(defun gas-set-point (pointpos fields &optional direction always)
  "Set point to a field of the current line.

POINTPOS is the current field as returned by a call to
`gas-get-pointpos', FIELDS the parsed content of the current
line.  

If DIRECTION is nil or 'stay, set it to the beginning of
the current field.  If DIRECTION is 'end-of-field, set it to the
end of the current field.  If DIRECTION equals 'backward or
'forward, and ALWAYS is nil, set it to the beginning of the
next/previous field only if the field is unchanged, else to the
beginning of the current.  Finally, if DIRECTION equals 'backward
or'forward and ALWAYS is non-nil, move point unconditionally to
the beginning of the next/previous field."
  (let ((existing '(line-begin))
        f-type)
    (dmsg 'indent "(gas-set-point %s %s %s)" pointpos direction always)
    (dolist (field fields)
      (setq existing (append existing (list (car field)))))      
    (add-to-list 'existing 'line-end t)
    (when (eq direction 'backward)
      (setq existing (nreverse existing)))
    (setq f-type (car pointpos))
    (when (and (eq direction 'forward)
               (eq (nth 3 pointpos) 'eol))
      ;; special forward skipping rules for certain fields:
      (cond ((and (eq f-type 'C-comment-start) (= (nth 2 pointpos) 0))
             ;; move on:
             nil)
            ((memq f-type '(label opcode argument)) 
             (cond ((>= (nth 2 pointpos) gas-comment-column)
                    (setq f-type 'eol-ws)) ; next: line-end
                   ((and (not (gas-nth 'text 'opcode fields))
                         (>= (nth 2 pointpos) gas-opcode-column))
                    (setq f-type 'argument)))) ; next: comment
            (t (setq f-type 'eol-ws)))) ; next: line-end
    ;; rules for skipping backward:
    (when (eq direction 'backward)
      (when (= (nth 2 pointpos) 0)
        (setq f-type (car (last existing 2)))) ;next: line-begin
      (and (> (nth 1 pointpos) 0)
           (or (not (eq f-type 'asm-comment))
               (not (looking-back ";[ \t]")))
           (setq direction 'stay)))
    ;; move to next field if told to do so:
    (when (and (not (memq direction '(pos stay end-of-field))) ; no - forbidden
               (or always             ; yes - do always
                   (and direction     ; maybe - do only if field unchanged
                        (not (gas-nth 'modified f-type fields)))))
      (setq f-type (cadr (member f-type existing))))
    (dmsg 'indent "gas-set-point next:%s, fields: %s" f-type fields)
    ;; set point
    (if (eq f-type 'line-begin)
        (progn
          (if (bobp)
              (beep))
          (beginning-of-line))
      (when (eq f-type 'line-end)
        (when (eobp) 
          (beep) 
          (error "End of buffer"))
        (setq f-type 'eol-ws)
        (setq direction 'end-of-field))
      (gas-put-out)
      ;; and set point there:
      (let ((where (gas-nth 'text-col f-type fields)))
        (cond ((eq direction 'pos) 
               (setq where (+ (gas-nth 'text-col f-type fields)
                              (cadr pointpos))))
              ((eq direction 'end-of-field)
               (setq where (max where (gas-nth 'end-col f-type fields)))))
        (move-to-column where t)
        (indent-to where))
      (when (eq f-type 'asm-comment)
        (skip-chars-forward (format " %c" gas-comment-char))
        (when (not (looking-back " "))
          (insert-char ?\s 1))))))

(defun gas-indent (&optional direction always ffields)
  "Indent current line.

For lines carrying asm syntax, `gas-indent-current-field-only'
determines if only the current field or the entire line is
affected.

See `gas-set-point' for a description of DIRECTION and ALWAYS.
FFIELDS, if present, is a list describing the fields on the
current line as returned by `gas-parsed'."
  (let* ((fields (or ffields (gas-parsed)))
         rearranged-type
         pointpos)
    (setq pointpos (gas-get-pointpos fields))
    (dmsg 'indent "gas-indent, fields: %s" fields)
    (dmsg 'indent "gas-indent, pointpos: %s" pointpos)
    (when (and (not direction) 
               (not (gas-C-comment-p))
               (eq this-command 'newline-and-indent))
      ;; go to first non-empty field.  If none, to opcode.
      (setq pointpos
            (list
             (or (catch 'field-found
                   (dolist (field fields)
                     (when (gas-nth 'text field)
                       (throw 'field-found (car field)))))
                 'opcode)
             0 gas-opcode-column))
      (setq direction 'stay)
      (setq always t))    
    (setq rearranged-type
          (cond 
           ((and (eq (caar fields) 'C-comment-start)
                 (or (save-excursion (beginning-of-line)(looking-at "[ \t]*$"))
                     (and (eq (car pointpos) 'C-comment) (= (nth 1 pointpos) 0))))
            ;; at start of C comment text (poosibly empty): in/outdent
            (let ((n_indents (/ (nth 2 pointpos) gas-C-indent)))
              (if (eq direction 'backward)
                  (setq n_indents (max 0 (1- n_indents)))
                (setq n_indents (1+ n_indents)))
              (gas-rearrange fields 'text-col 'C-comment
                             (* gas-C-indent n_indents))
              (setq direction 'stay)
              'C-comment))
           (gas-indent-current-field-only
            (gas-rearrange fields 'text-col (car pointpos) 
                           (cdr (assq (car pointpos) gas-indents))))
           (t (gas-rearrange fields 'all))))
    (dmsg 'indent "re-indented, fields: %s" fields)
    (gas-put-parsed fields (line-number-at-pos))
    (gas-put-out)
    (cond (always (gas-set-point pointpos fields direction always))
          (rearranged-type (gas-set-point-to-field rearranged-type fields))
          ((and (eq (car pointpos) 'C-comment) (not direction)
                (> (cadr pointpos) 0))
           (gas-set-point pointpos fields 'pos always))
          ((and (not direction) (>= (cadr pointpos) 0))
           (gas-set-point pointpos fields 'forward always))
          (t (gas-set-point pointpos fields direction always)))
    fields))

(defun gas-indent-region (&optional from to)
  "Indent all fields in region.

If optional FROM and TO are given, they are used instead of point
and mark for the region's end points."
  (interactive)
  (unless from (setq from (min (point) (mark))))
  (unless to (setq to (max (point) (mark))))
  (combine-after-change-calls
    (save-excursion
      (goto-char from)
      (while (and (not (eobp)) (< (point) to))
        (dmsg 'indent "indent-region, line:%s" (line-number-at-pos))
        (if (gas-C-comment-p)
            (beginning-of-line 2)         ; skip C comments
          (let ((fields (gas-parsed)))
            (gas-rearrange fields 'all)
            (gas-put-out fields)
            (beginning-of-line 2)))))))

(defun gas-indent-backward ()
  "Indent, then move to previous field.
While in C-comment, remove one level of indentation."
  (interactive)
  (gas-indent 'backward 'always))

(defun gas-forward-sexp (n)
  "On a highlighted symbol, move to next (previous if N < 9).  
Else do `forward-sexp' as usual."
  (gas-sym-invalidate)
  (interactive)
  (gas-symbol-highlight)
  (if gas-hi-sym-list
      (unless
          (catch 'found-one
            (if (< 0 n)
                (dolist (match (cadr gas-hi-sym-list))
                  (when (> (cadr match) (point))
                    (goto-char (cadr match))
                    (throw 'found-one t)))
              (dolist (match (reverse (cadr gas-hi-sym-list)))
                (when (< (nth 2 match)  (point))
                  (goto-char (cadr match))
                  (throw 'found-one t)))))
        (beep))
    (let (forward-sexp-function)
      (forward-sexp n))))


(defun gas-beginning-of-defun ()
  "Skip to the beginning of the current block.
The block delimiter is described by `gas-defun-regexp'."
  (interactive)
  (let* ((beg (save-excursion
                (re-search-backward gas-defun-regexp nil t))))
    (when beg
      (goto-char (match-beginning gas-defun-regexp-subexp)))))

(defun gas-end-of-defun ()
  "Skip to the beginning of the next block.
The block delimiter is described by `gas-defun-regexp'."
  (interactive)
  (let ((beg (save-excursion
              (re-search-forward gas-defun-regexp nil t))))
    (when beg
      (goto-char (match-beginning gas-defun-regexp-subexp)))))


(defun gas-hash () 
  "Insert a hash mark.  If it start a macro, delete the indentaion."
  (interactive)
  (when (and (not (gas-comment-p))
             (looking-back "^[ \t]*"))
    (beginning-of-line)
    (delete-horizontal-space))
  (call-interactively 'self-insert-command))

             
(defun gas-colon ()
  "Insert a colon; if it follows a label, delete the label's indentation."
  (interactive)
  (call-interactively 'self-insert-command)
  ;; check if colon belongs to a label field
  (let* ((fields (gas-parsed))
         (pointpos (gas-get-pointpos fields)))
    (dmsg 'indent "gas-colon, fields: %s" fields)
    (dmsg 'indent "gas-colon, pointpos: %s" pointpos)
    (when (eq (car pointpos) 'label)
      (beginning-of-line)
      (gas-indent 'forward t))))


;; Obsolete since Emacs-22.1.
(defalias 'gas-newline 'newline-and-indent)

(defun gas-comment-region-dwim (&optional from to)
  "De-comment region if at start of comment, make comment out of region if not.
Optional argument FROM Optional FROM and TO mark the region."
  (unless from
    (setq from (min (point) (mark)))
    (setq to (max (point) (mark))))
  (when (save-excursion (goto-char to) (bolp))
    (setq to (1- to)))
  (save-excursion
    (let ((mmax (progn (goto-char to) (point-marker)))
          (mmin (progn (goto-char from) (point-marker))))
      
      (if (gas-C-passthrough-p)
          (let* (beg end end-match
                 (pos (looking-at "[ \t\n]*/#"))
                 (de-comment (or pos (gas-C-passthrough-comment-p))))
            (save-restriction
              (gas-narrow-to-C-comment)
              (if de-comment
                  (progn
                    (when (gas-C-passthrough-comment-p)
                      (insert " #/\n"))
                    (while (re-search-forward
                            "\\(\n[ \t]*\\)?/#\\(\n[ \t]*\\)?" mmax t)
                      (replace-match "")
                      (when (re-search-forward
                             "\\(\n[ \t]*\\)?[#]/\\(\n[ \t]*\\)?" mmax t)
                        (replace-match "")))
                    (let* ((end (save-excursion
                                  (re-search-forward "[#*]/" nil t)))
                           (end-match (match-string 0))
                           (beg (save-excursion
                                  (re-search-forward "/#" nil t))))
                      (when (and (equal end-match "#/")
                                 (or (not beg)
                                     (> beg end)))
                        ;; we have a comment close outside the selcted
                        ;; area which misses the matching comment open
                        (goto-char mmax)
                        (insert "\n/#\n"))))
                ;; do C en-comment
                (insert " /#\n ")    ;
                (while (and (save-excursion 
                              (setq end (re-search-forward "[*#]/" nil t)))
                            (setq beg (re-search-forward "/#" mmax t)))
                  (replace-match "#/ /# ")
                  (re-search-forward "[#*]/" mmax t)
                  (if (equal end-match "*/")
                      (replace-match "   #/\n*/")
                    (replace-match "#/ /#")))
                (let* ((end (save-excursion
                              (re-search-forward "[#*]/" nil t)))
                       (end-match (match-string 0))
                       (beg (save-excursion
                              (re-search-forward "/#" nil t))))
                  (when (and (equal end-match "*/")
                             (or (not beg)
                                 (> beg end)))
                    ;; we have no comment close outside the selcted
                    ;; area but comment is open
                    (goto-char mmax)
                    (backward-char 2)
                    (insert "\n   #/\n"))))))
        ;; skip to the start of the asm comment if  we're on one
        (when (looking-back (format "%c+[ \t]*" gas-comment-char))
          (goto-char (match-beginning 0))
          (skip-chars-backward gas-comment-string))
        (save-restriction
          (narrow-to-region (point) to)
          (let ((triple-comment (concat (make-string 3 gas-comment-char) " "))
                comment-pattern
                (de-comment
                 ;; de-comment if there's a comment after point or
                 ;; at the start of the next non-empty line
                 (looking-at (format "[ \t]*%c\\|.*\n[ \t\n]*\%c"
                                     gas-comment-char gas-comment-char))))
            (when de-comment
              (goto-char (match-end 0))
              (skip-chars-backward gas-comment-string)
              (looking-at (format "\n?\\([ \t]\\)*\\(%c+\\)\\([ \t]?\\)"
                                  gas-comment-char))
              (let ((pre-pattern (match-string 1))
                    (mid-pattern (regexp-quote (match-string 2)))
                    (post-pattern (match-string 3)))
                (setq pre-pattern
                      (if (and pre-pattern
                               (string-match "[ \t]" pre-pattern))
                          "\\([ \t]*"
                        "\\("))
                (setq post-pattern
                      (if (and post-pattern
                               (string-match "[ \t]" post-pattern))
                          "[ \t]?\\)"
                        (format "\\)\\([^%c]\\|$\\)" gas-comment-char)))
                (setq comment-pattern
                      (concat pre-pattern mid-pattern post-pattern))))
            (catch 'fini
              (while t
                (cond ((gas-C-comment-p) nil)
                      ((looking-at "[ \t]/[*]") nil)
                      ((not de-comment) (insert triple-comment))
                      (t (when (looking-at comment-pattern)
                           (replace-match "" nil t nil 1))))
                ;; forward-line returns t even after it just
                ;; moved to eol (end of narrowed region):
                (unless (and (= 0 (forward-line)) (bolp))
                  (throw 'fini t)))))))
      (setq mmax nil)
      (setq mmin nil))))

(defun gas-backward-indent ()
  "Skip to previous asm field.  Within C style comments, decrease indentation."
  (interactive)
  (gas-indent 'backward 'always))

(defun gas-comment ()
  "Start/expand a comment.
Suggested usage:  while writing your code, trigger `gas-comment'
repeatedly until you are satisfied with the kind of comment."
  (interactive)
  (setq gas-doing-comment t)
  (if mark-active
      (gas-comment-region-dwim)
    (let* ((fields (save-excursion (gas-parsed)))
           (curcol (current-column))
           (pointpos (gas-get-pointpos fields))
           (subtype (gas-nth 'subtype 'asm-comment fields)))
      (dmsg 'indent "gas-comment, pointpos: %s, fields. %s"
            pointpos fields)
      (cond
       ((gas-C-passthrough-comment-p) (insert-char gas-comment-char 1))
       ((gas-C-passthrough-code-p) 
        (when (looking-back "/[*]C[ \t]*")
          (insert "\n"))
        (if (looking-back "[ \t]")
            (insert "/#   #/ ")
          (insert " /#   #/ "))
        (backward-char 5))
       ((gas-C-comment-p)
        (if (and gas-use-C-passthrough
                 (looking-back "\\(/[*] ?\\)[ \t\n]*")            )
            (save-excursion (replace-match "/*C" nil nil nil 1))
          (insert-char gas-comment-char 1)))
       ((and subtype (>= subtype 3))
        (move-to-column (gas-nth 'text-col 'asm-comment fields))
        (looking-at ";+ ?")
        (replace-match "")
        (insert "/*")
        (backward-char)
        (gas-indent 'forward 'always)
        (let ((curindent (current-indentation)))
          (insert "\n\n")
          (indent-to curindent)
          (insert "*/")
          (forward-line -1)
          (indent-to (+ curindent gas-C-indent))))
       ((looking-back "[ \t]*#.*" (line-beginning-position))
        (insert "/*   */ ")
        (backward-char 5))
       (t
        (let ((target-col (gas-nth 'text-col 'asm-comment fields)))
          (when target-col
            (move-to-column target-col)))
        (when (or (not subtype)
                  (and (<= (current-column) curcol)
                       (save-excursion
                         (skip-chars-forward (format "%c \t" gas-comment-char))
                         (>= (current-column) curcol))))
          (insert-char gas-comment-char 1)
          (setq fields (gas-parsed)))
        (gas-rearrange fields 'text-col 'asm-comment)
        (gas-put-out fields)
        (gas-set-point-to-field 'asm-comment fields))))))

(defun gas-fill-paragraph (arg)
  "Beautify asm code block.  Within comments, fill current paragraph.
Calls `fill-paragraph' if within a C comment with ARG passed
through (which is ignored otherwise)."
  (let ((curcol (current-column))
        (fill-paragraph-function nil)
        ; non-empty comment:
        (asm-comment-re (format "\\([ \t]*\\(%c+\\)\\)[ \t]*[^ \t%c\n]\n?"
                                gas-comment-char gas-comment-char))
        (empty-line-re "\\([^ \t]+:\\)?[ \t]*$"))
    (cond ((gas-C-comment-really-p) 
           (let* ((beg (save-excursion 
                         (re-search-backward "/#\\|/[*][^C]\\([ \t]*\n\\)?")
                         (match-end 0)))
                  (end (save-excursion
                         (re-search-forward "\\(\n[ \t]*\\)?[*#]/")
                         (match-beginning 0))))
             (save-restriction
               (narrow-to-region beg end)
               (fill-paragraph arg))))
          ((gas-C-passthrough-code-p) 
           (let* ((beg (save-excursion 
                         (re-search-backward "\\(#/\\|/[*]C\\)\\([ \t]*\n\\)?")
                         (match-end 0)))
                  (end (save-excursion
                         (re-search-forward "\\(\n[ \t]*\\)?[*#]/")
                         (match-beginning 0))))
             (goto-char beg)
             (while (< (point) end)
               (let ((fields (gas-parsed)))
                 (gas-rearrange fields 'all)
                 (gas-put-out fields)
                 (beginning-of-line 2)))))
          ((save-excursion (beginning-of-line) (looking-at asm-comment-re))
           (let* ((fill-prefix (concat (match-string 1) " "))
                  (re (format "[^%c\n]*\\%s[ \t]*[^ \t%c\n]\n?" gas-comment-char
                               (match-string 2) gas-comment-char))
                  (beg (save-excursion
                        (beginning-of-line) 
                        (while (save-excursion
                                 (beginning-of-line) 
                                 (looking-at re))
                          (end-of-line 0))
                        (point)))
                 (end (save-excursion
                        (end-of-line)
                        (while (save-excursion
                                 (beginning-of-line 2) 
                                 (looking-at re))
                          (end-of-line 2))
                        (point))))
             (save-restriction
               (narrow-to-region beg end)
               (fill-paragraph arg))))
          (t
           (let ((beg (save-excursion
                        (beginning-of-line)
                        (while (save-excursion
                                 (beginning-of-line 0)
                                 (not (or (looking-at asm-comment-re)
                                          (looking-at empty-line-re)
                                          (gas-comment-p))))
                          (beginning-of-line 0))
                        (point)))
                 (end (save-excursion
                        (end-of-line)
                        (while (save-excursion
                                 (beginning-of-line 2)
                                 (not (or (looking-at asm-comment-re)
                                          (looking-at empty-line-re)
                                          (gas-comment-p))))
                                 (end-of-line 2))
                        (point))))
             (gas-indent-region beg end))))
    (move-to-column curcol)) t)

(defun gas-comment-char ()
  "Handle comment character.

If we're in a comment start sequence (either introduced by the
last command being `gas-comment' or by
`gas-comment-char-starts-comment' being customized to a non-nil
value and typing 'gas-comment-char'), increment the level of the
comment just started.

Else, if `gas-comment-char-starts-comment' is customized to a
non-nil value, start comment.

Else just insert variable `gas-comment-char'."
  (interactive)
  (if (and gas-comment-char-starts-comment
           (not (gas-C-comment-p)))
      (gas-comment)
    (if (and gas-doing-comment
             (member last-command '(gas-comment-char gas-comment)))
        (gas-comment)
      (setq gas-doing-comment nil)
      (insert-char gas-comment-char 1))))

(provide 'gas-mode)

;;; gas-mode.el ends here

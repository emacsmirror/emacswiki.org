;;; tie-mode.el --- Tie code editing commands for GNU Emacs

;; Copyright (C) 1990, 1994  Free Software Foundation, Inc.

;; Author: Ricardo E. Gonzalez
;; Maintainer: tie-mode at tensilica dot com
;; Adapted-By: 
;; Keywords: languages

;; Adapted from Perl code editing commands 'perl-mode.el', Copyright
;; 1987 by the Free Software Foundation, under terms of its General
;; Public License.


;;; Commentary:

;; To enter tie-mode automatically, add (autoload 'tie-mode "tie-mode")
;; to your .emacs file and add something like,
;;
;; (setq auto-mode-alist (append (list (cons "\\.tie\\'" 'tie-mode))
;;                               auto-mode-alist))
;;
;; to your .emacs file; otherwise the .pl suffix defaults to prolog-mode.

;; This code is based on the 20.7 version perl-mode.el, with extensive
;; rewriting.

;; The rest of the commentary was in the original perl-mode.el

;; I added a new feature which adds functionality to TAB; it is controlled
;; by the variable tie-tab-to-comment.  With it enabled, TAB does the
;; first thing it can from the following list:  change the indentation;
;; move past leading white space; delete an empty comment; reindent a
;; comment; move to end of line; create an empty comment; tell you that
;; the line ends in a quoted string, or has a # which should be a \#.

;; If your machine is slow, you may want to remove some of the bindings
;; to electric-tie-terminator.  I changed the indenting defaults to be
;; what Larry Wall uses in tie/lib, but left in all the options.

;; I also tuned a few things:  comments and labels starting in column
;; zero are left there by indent-tie-exp; tie-beginning-of-function
;; goes back to the first open brace/paren in column zero, the open brace
;; in 'sub ... {', or the equal sign in 'format ... ='; indent-tie-exp
;; (meta-^q) indents from the current line through the close of the next
;; brace/paren, so you don't need to start exactly at a brace or paren.

;; It may be good style to put a set of redundant braces around your
;; main program.  This will let you reindent it with meta-^q.

;; Known problems (these are all caused by limitations in the Emacs Lisp
;; parsing routine (parse-partial-sexp), which was not designed for such
;; a rich language; writing a more suitable parser would be a big job):
;; 1)  Regular expression delimiters do not act as quotes, so special
;;       characters such as `'"#:;[](){} may need to be backslashed
;;       in regular expressions and in both parts of s/// and tr///.
;; 2)  The globbing syntax <pattern> is not recognized, so special
;;       characters in the pattern string must be backslashed.
;; 3)  The q, qq, and << quoting operators are not recognized; see below.
;; 4)  \ (backslash) always quotes the next character, so '\' is
;;       treated as the start of a string.  Use "\\" as a work-around.
;; 5)  To make variables such a $' and $#array work, tie-mode treats
;;       $ just like backslash, so '$' is the same as problem 5.
;; 6)  Unfortunately, treating $ like \ makes ${var} be treated as an
;;       unmatched }.  See below.
;; 7)  When ' (quote) is used as a package name separator, tie-mode
;;       doesn't understand, and thinks it is seeing a quoted string.

;; Here are some ugly tricks to bypass some of these problems:  the tie
;; expression /`/ (that's a back-tick) usually evaluates harmlessly,
;; but will trick tie-mode into starting a quoted string, which
;; can be ended with another /`/.  Assuming you have no embedded
;; back-ticks, this can used to help solve problem 3:
;;
;;     /`/; $ugly = q?"'$?; /`/;
;;
;; To solve problem 6, add a /{/; before each use of ${var}:
;;     /{/; while (<${glob_me}>) ...
;;
;; Problem 7 is even worse, but this 'fix' does work :-(
;;     $DB'stop#'
;;         [$DB'line#'
;;          ] =~ s/;9$//;

;;; Code:

(defgroup tie nil
  "Major mode for editing Tie code."
  :prefix "tie-"
  :group 'languages)

(defvar tie-mode-abbrev-table nil
  "Abbrev table in use in tie-mode buffers.")
(define-abbrev-table 'tie-mode-abbrev-table ())

(defvar tie-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "{"        'electric-tie-terminator)
    (define-key map "}"        'electric-tie-terminator-semi)
    (define-key map ";"        'electric-tie-terminator-semi)
    (define-key map ":"        'electric-tie-terminator)
    (define-key map "\r"       'electric-tie-terminate-line)
    (define-key map "\e\C-a"   'tie-beginning-of-function)
    (define-key map "\e\C-e"   'tie-end-of-function)
    (define-key map "\e\C-h"   'tie-mark-function)
    (define-key map "\e\C-q"   'indent-tie-exp)
    (define-key map "\C-c\C-c" 'tie-mode-compile)
    (define-key map "\177"     'backward-delete-char-untabify)
    (define-key map "\t"       'tie-indent-command)
    map)
  "Keymap used in Tie mode.")

(autoload 'c-macro-expand "cmacexp"
  "Display the result of expanding all C macros occurring in the region.
The expansion is entirely correct because it uses the C preprocessor."
  t)

(defvar tie-mode-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    ;;(modify-syntax-entry ?\n ">" table)
    ;;(modify-syntax-entry ?$ "/" table)
    ;;(modify-syntax-entry ?\' "\"" table)
    ;;(modify-syntax-entry ?* "." table)
    ;;(modify-syntax-entry ?/ "." table)
    ;;(modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?` "$" table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?/  ". 124" table)
    (modify-syntax-entry ?*  ". 23b"   table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table in use in tie-mode buffers.")

(defvar tie-imenu-generic-expression
  '(
    ;; Functions
    ;(nil "^sub\\s-+\\([-A-Za-z0-9+_:]+\\)\\(\\s-\\|\n\\)*{" 1 )
    ;;Variables
    ;("Variables"  "^\\([$@%][-A-Za-z0-9+_:]+\\)\\s-*=" 1 )
    ("URegs"      "^user_register\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Tables"     "^table\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("States"     "^state\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("SRegs"      "^sys_register\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Slots"      "^slot\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Semantics"  "^semantic\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Schedules"  "^schedule\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Regfiles"   "^regfile\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("References" "^reference\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Operations" "^operation\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Packages"   "^package\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Operands"   "^operand\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Lengths"    "^length\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Labels"     "^label\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("IClasses"   "^iclass\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Functions"  "^function\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Formats"    "^format\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Exceptions" "^exception\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("ImmedRange" "^immediate_range\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("SlotOpcodes" "^slot_opcodes\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("ImportWires" "^import_wire\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Queues"      "^queue\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("Lookups"     "^lookup\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("CTypes"      "^ctype\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    ("IMaps"       "^imap\\s-+\\([-A-Za-z0-9+_:]+\\)" 1 )
    )
  "Imenu generic expression for Tie mode.  See `imenu-generic-expression'.")

(if (fboundp 'speedbar-add-supported-extension)
    (speedbar-add-supported-extension ".tie")
  (add-hook 'speedbar-load-hook
	    (lambda () (speedbar-add-supported-extension ".tie"))))

;; Find out what type of emacs we are running in. We will be using this
;; quite a few times in this program.
(defvar tie-mode-running-emacs nil
  "If the current Emacs is not XEmacs, then, this is non-nil.")

(defvar tie-mode-running-xemacs nil
  "If the current Emacs is XEmacs/Lucid, then, this is non-nil.")

(if (string-match "XEmacs\\|Lucid" emacs-version)
    (setq tie-mode-running-xemacs t)
  (setq tie-mode-running-emacs t))


(defvar tie-mode-menu-def
  '(["Lint"    	                tie-mode-lint t]
    ["Compile"   	        tie-mode-compile t]
    ["--" nil nil]
    ["Add regfile"		tie-mode-add-regfile t]
    ["Add state"	        tie-mode-add-state t]
    ["Add table"	        tie-mode-add-table t]
    ["Add operation"	        tie-mode-add-operation t]
    ["Add semantic"	        tie-mode-add-semantic t]
    ["Add ctype"	        tie-mode-add-ctype t]
    ["Add ctype protos"	        tie-mode-add-ctype-protos t]
    ["Add ctype cvts"	        tie-mode-add-ctype-conversions t]
    ["Add lookup"	        tie-mode-add-lookup t]
    ["Add queue"	        tie-mode-add-queue t]
    ["Add immediate_range"	tie-mode-add-immediate-range t]
    ["--" nil nil]
    ["Add length"		tie-mode-add-length t]
    ["Add format"		tie-mode-add-format t]
    ["Add slot"			tie-mode-add-slot t]
    ["Add slot opcodes"		tie-mode-add-slot-opcodes t]
    ["--" nil nil]
    ["What else?"	        tie-mode-suggestions t])
  "The tie-mode menu definition")

(cond (tie-mode-running-xemacs
       ;; Menu Support for XEmacs
       (require 'easymenu)
       (defun tie-mode-menu (modestr)
	 (cons modestr tie-mode-menu-def)))

      (tie-mode-running-emacs
       ;; Menu support for Emacs
       (or (lookup-key tie-mode-map [menu-bar])
	   (define-key tie-mode-map [menu-bar] (make-sparse-keymap "menu-bar")))
       (defvar menu-bar-tie-mode-menu (make-sparse-keymap "Tie"))
       (setq menu-bar-final-items (cons 'tie-mode-menu menu-bar-final-items))
       (define-key tie-mode-map [menu-bar tie-mode-menu]
	 (cons "Tie" menu-bar-tie-mode-menu))
       (let ((m (reverse tie-mode-menu-def))
	     (separator-number 0))
	 (while m
	   (let ((menu-text (elt (car m) 0))
		 (menu-action (elt (car m) 1))
		 (menu-pred (elt (car m) 2)))
	     (if menu-action
		 (progn
		   (define-key menu-bar-tie-mode-menu (vector menu-action)
		     (cons menu-text menu-action))
		   (put menu-action 'menu-enable menu-pred))
	       (define-key menu-bar-tie-mode-menu
		 (vector (make-symbol
			  (concat "separator-"
				  (int-to-string separator-number))))
		 '("--"))
	       (setq separator-number (1+ separator-number))))
	   (setq m (cdr m))))))

;; Regexps updated with help from Tom Tromey <tromey@cambric.colorado.edu> and
;; Jim Campbell <jec@murzim.ca.boeing.com>.

(defconst tie-font-lock-keywords-1
;(setq tie-font-lock-keywords-1
  '(;; fontify wire and parameter declarations
    ("^\\s-*\\(wire\\)\\s-+" 0 font-lock-type-face)
    ("\\<\\(in\\|out\\|inout\\|sout\\)\\s-+\\([a-zA-Z0-9]+\\)\\>"
     (1 font-lock-type-face) (2 font-lock-variable-name-face))
    ;; Fontify function and package names in declarations.
    ("\\<\\(package\\|endpackage\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
    )
  "Subdued level highlighting for Tie mode.")

 
(defconst tie-keyword-regexp
  (eval-when-compile
    (regexp-opt (list "function"
		    "length"
		    "format"
		    "slot"
		    "opcode"
		    "semantic"
		    "state"
		    "field"
		    "operand"
		    "imp_operand"
		    "label"
		    "table"
		    "iclass"
		    "user_register"
		    "sys_register"
		    "interface"
		    "schedule"
		    "regfile"
		    "regport"
		    "synopsis"
		    "description"
		    "package"
		    "endpackage"
		    "operation"
		    "impl_note"
		    "assm_note"
		    "ctype"
		    "reference"
		    "proto"
		    "coprocessor"
		    "test"
		    "exception"
		    "core_signal"
		    "simd"
                    "property"
                    "immediate_range"
                    "slot_opcodes"
                    "import_wire"
                    "queue"
                    "lookup"
                    "imap"
		    
                    "TIEadd"
                    "TIEaddn"
                    "TIEcsa"
                    "TIEcmp"
                    "TIElzc"
                    "TIEmac"
                    "TIEmul"
                    "TIEmulpp"
                    "TIEpsel"
                    "TIEsel"
                    "TIEprint"
		    
                    "add_read_write"
                    "export"
		    "VAddr"
		    "MemDataIn8"
		    "MemDataIn16"
		    "MemDataIn32"
		    "MemDataIn64"
		    "MemDataIn128"
		    "MemDataOut8"
		    "MemDataOut16"
		    "MemDataOut32"
		    "MemDataOut64"
		    "MemDataOut128"
		    "LoadByteDisable"
		    "StoreByteDisable"

		    "assign"
		    "wire"

		    "use"
		    "def"

		    "InstBuf"

		    "rdy"
		    "immediate"

		    "shared"
		    "slot_shared"
		    
		    )))
  )

(defconst tie-stmt-regexp
  (eval-when-compile
    (regexp-opt (list "function"
		    "length"
		    "format"
		    "slot"
		    "opcode"
		    "semantic"
		    "state"
		    "field"
		    "operand"
		    "imp_operand"
		    "label"
		    "table"
		    "iclass"
		    "user_register"
		    "sys_register"
		    "interface"
		    "schedule"
		    "regfile"
		    "regport"
		    "synopsis"
		    "description"
		    "package"
		    "endpackage"
		    "operation"
		    "impl_note"
		    "assm_note"
		    "ctype"
		    "reference"
		    "proto"
		    "coprocessor"
		    "test"
		    "exception"
		    "core_signal"
		    "simd"
                    "property"
                    "immediate_range"
                    "slot_opcodes"
                    "import_wire"
                    "queue"
                    "lookup"
                    "imap"
		    )))
  )

(defconst tie-font-lock-keywords-2
;(setq tie-font-lock-keywords-2
  (append tie-font-lock-keywords-1
   (list
    ;;
    (list
     (concat "\\<\\(" tie-keyword-regexp "\\)\\>[ \t]*\\(\\sw+\\)?")
     '(1 font-lock-keyword-face) '(2 font-lock-constant-face nil t))
    '("^[ \t]*\\(\\sw+\\)[ \t]*:[^:]" 1 font-lock-constant-face)))
  "Gaudy level highlighting for Tie mode.")

(defvar tie-font-lock-keywords tie-font-lock-keywords-1
  "Default expressions to highlight in Tie mode.")


(defcustom tie-indent-level 4
  "*Indentation of Tie statements with respect to containing block."
  :type 'integer
  :group 'tie)
(defcustom tie-continued-statement-offset 4
  "*Extra indent for lines not starting new statements."
  :type 'integer
  :group 'tie)
(defcustom tie-continued-brace-offset -4
  "*Extra indent for substatements that start with open-braces.
This is in addition to `tie-continued-statement-offset'."
  :type 'integer
  :group 'tie)
(defcustom tie-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context."
  :type 'integer
  :group 'tie)
(defcustom tie-brace-imaginary-offset 0
  "*Imagined indentation of an open brace that actually follows a statement."
  :type 'integer
  :group 'tie)
(defcustom tie-label-offset -2
  "*Offset of Tie label lines relative to usual indentation."
  :type 'integer
  :group 'tie)

(defcustom tie-tab-always-indent t
  "*Non-nil means TAB in Tie mode always indents the current line.
Otherwise it inserts a tab character if you type it past the first
nonwhite character on the line."
  :type 'boolean
  :group 'tie)

;; I changed the default to nil for consistency with general Emacs
;; conventions -- rms.
(defcustom tie-tab-to-comment nil
  "*Non-nil means TAB moves to eol or makes a comment in some cases.
For lines which don't need indenting, TAB either indents an
existing comment, moves to end-of-line, or if at end-of-line already,
create a new comment."
  :type 'boolean
  :group 'tie)

(defcustom tie-nochange ";?#\\|\f\\|\\s(\\|\\(\\w\\|\\s_\\)+:"
  "*Lines starting with this regular expression are not auto-indented."
  :type 'regexp
  :group 'tie)

 
;;;###autoload
(defun tie-mode ()
  "Major mode for editing Tie code.
Expression and list commands understand all Tie brackets.
Tab indents for Tie code.
Comments are delimited with // ... \\n or /* ... */.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{tie-mode-map}
Variables controlling indentation style:
 tie-tab-always-indent
    Non-nil means TAB in Tie mode should always indent the current line,
    regardless of where in the line point is when the TAB command is used.
 tie-tab-to-comment
    Non-nil means that for lines which don't need indenting, TAB will
    either delete an empty comment, indent an existing comment, move 
    to end-of-line, or if at end-of-line already, create a new comment.
 tie-nochange
    Lines starting with this regular expression are not auto-indented.
 tie-indent-level
    Indentation of Tie statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 tie-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 tie-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `tie-continued-statement-offset'.
 tie-brace-offset
    Extra indentation for line if it starts with an open brace.
 tie-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 tie-label-offset
    Extra indentation for line that is a label.

Various indentation styles:       K&R  BSD  BLK  GNU  LW
  tie-indent-level                5    8    0    2    4
  tie-continued-statement-offset  5    8    4    2    4
  tie-continued-brace-offset      0    0    0    0   -4
  tie-brace-offset               -5   -8    0    0    0
  tie-brace-imaginary-offset      0    0    4    0    0
  tie-label-offset               -5   -8   -2   -2   -2

Turning on Tie mode runs the normal hook `tie-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map tie-mode-map)
  (setq major-mode 'tie-mode)
  (setq mode-name "Tie")
  (setq local-abbrev-table tie-mode-abbrev-table)
  (set-syntax-table tie-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'tie-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "// ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *\\|//[\t ]*")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'tie-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  ;; Tell font-lock.el how to handle Tie.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'((tie-font-lock-keywords
	   tie-font-lock-keywords-1
	   tie-font-lock-keywords-2)
	  nil nil ((?\_ . "w"))))
  ;; Tell imenu how to handle Tie.
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression tie-imenu-generic-expression)
  (setq imenu-case-fold-search nil)
  (run-hooks 'tie-mode-hook))
 
;; This is used by indent-for-comment
;; to decide how much to indent a comment in Tie code
;; based on its context.
(defun tie-comment-indent ()
  (if (and (bolp) (not (eolp)))
      0					;Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (if (bolp)			;Else indent at comment column
	       0			; except leave at least one space if
	     (1+ (current-column)))	; not at beginning of line.
	   comment-column))))

(defun electric-tie-terminator (arg)
  "Insert character and adjust indentation.
If at end-of-line, and not in a comment or a quote, correct the's indentation."
  (interactive "P")
  (let ((insertpos (point)))
    (and (not arg)			; decide whether to indent
	 (eolp)
	 (save-excursion
	   (beginning-of-line)
	   (and (not			; eliminate comments quickly
		 (and comment-start-skip
		      (re-search-forward comment-start-skip insertpos t)) )
		(let ((pps (parse-partial-sexp 
			    (tie-beginning-of-function) insertpos)))
		  (not (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))
	 (tie-indent-line)))
  (self-insert-command (prefix-numeric-value arg)))

(defun electric-tie-terminator-semi (arg)
  "Insert character and adjust indentation.
If at end-of-line, and not in a comment or a quote, correct the's indentation."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (let ((insertpos (point)))
    (and (not arg)			; decide whether to indent
	 (eolp)
	 (save-excursion
	   (beginning-of-line)
	   (and (not			; eliminate comments quickly
		 (and comment-start-skip
		      (re-search-forward comment-start-skip insertpos t)) )
		(let ((pps (parse-partial-sexp 
			    (tie-beginning-of-function) insertpos)))
		  (not (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))
	 (tie-indent-line)))
  )

(defun electric-tie-terminate-line (arg)
  "Terminate and indent line.  With prefix arg do nothing.
If at end-of-line, and not in a comment or a quote, correct the's indentation."
  (interactive "P")
  (let ((insertpos (point)))
    (and (not arg)			; decide whether to indent
	 (eolp)
	 (save-excursion
	   (beginning-of-line)
	   (and (not			; eliminate comments quickly
		 (and comment-start-skip
		      (re-search-forward comment-start-skip insertpos t)) )
		(or (/= last-command-char ?:)
		    ;; Colon is special only after a label ....
		    (looking-at "\\s-*\\(\\w\\|\\s_\\)+$"))
		(let ((pps (parse-partial-sexp 
			    (tie-beginning-of-function) insertpos)))
		  (not (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))
	 (progn				; must insert, indent, delete
	   (insert-char last-command-char 1)
	   (tie-indent-line)
	   (delete-char -1))))
  (insert-char ?\n (prefix-numeric-value arg)))

;; not used anymore, but may be useful someday:
;;(defun tie-inside-parens-p ()
;;  (condition-case ()
;;      (save-excursion
;;	(save-restriction
;;	  (narrow-to-region (point)
;;			    (tie-beginning-of-function))
;;	  (goto-char (point-max))
;;	  (= (char-after (or (scan-lists (point) -1 1) (point-min))) ?\()))
;;    (error nil)))
 
(defun tie-indent-command (&optional arg)
  "Indent current line as Tie code, or optionally, insert a tab character.

With an argument, indent the current line, regardless of other options.

If `tie-tab-always-indent' is nil and point is not in the indentation
area at the beginning of the line, simply insert a tab.

Otherwise, indent the current line.  If point was within the indentation
area it is moved to the end of the indentation area.  If the line was
already indented protiey and point was not within the indentation area,
and if `tie-tab-to-comment' is non-nil (the default), then do the first
possible action from the following list:

  1) delete an empty comment
  2) move forward to start of comment, indenting if necessary
  3) move forward to end of line
  4) create an empty comment
  5) move backward to start of comment, indenting if necessary."
  (interactive "P")
  (if arg				; If arg, just indent this line
      (tie-indent-line "\f")
    (if (and (not tie-tab-always-indent)
	     (> (current-column) (current-indentation)))
	(insert-tab)
      (let (bof lsexp delta (oldpnt (point)))
	(beginning-of-line) 
	(setq lsexp (point))
	(setq bof (tie-beginning-of-function))
	(goto-char oldpnt)
	(setq delta (tie-indent-line "\f\\|;?#" bof))
	(and tie-tab-to-comment
	     (= oldpnt (point))		; done if point moved
	     (if (listp delta)		; if line starts in a quoted string
		 (setq lsexp (or (nth 2 delta) bof))
	       (= delta 0))		; done if indenting occurred
	     (let (eol state)
	       (end-of-line) 
	       (setq eol (point))
	       (if (= (char-after bof) ?=)
		   (if (= oldpnt eol)
		       (message "In a format statement"))     
		 (setq state (parse-partial-sexp lsexp eol))
		 (if (nth 3 state)
		     (if (= oldpnt eol)	; already at eol in a string
			 (message "In a string which starts with a %c."
				  (nth 3 state)))
		   (if (not (nth 4 state))
		       (if (= oldpnt eol) ; no comment, create one?
			   (indent-for-comment))
		     (beginning-of-line)
		     (if (and comment-start-skip
			      (re-search-forward comment-start-skip eol 'move))
			 (if (eolp)
			     (progn	; kill existing comment
			       (goto-char (match-beginning 0))
			       (skip-chars-backward " \t")
			       (kill-region (point) eol))
			   (if (or (< oldpnt (point)) (= oldpnt eol))
			       (indent-for-comment) ; indent existing comment
			     (end-of-line)))
		       (if (/= oldpnt eol)
			   (end-of-line)
			 (message "Use backslash to quote # characters.")
			 (ding t))))))))))))

(defun tie-indent-line (&optional nochange parse-start)
  "Indent current line as Tie code.
Return the amount the indentation 
changed by, or (parse-state) if line starts in a quoted string."
  (let ((case-fold-search nil)
	(pos (- (point-max) (point)))
	(bof (or parse-start (save-excursion (tie-beginning-of-function))))
	beg indent shift-amt)
    (beginning-of-line)
    (setq beg (point))
    (setq shift-amt
	  (cond ((and (not (null (char-after bof)))
		      (= (char-after bof) ?=) 0))
		((listp (setq indent (calculate-tie-indent bof))) indent)
		((looking-at (or nochange tie-nochange)) 0)
		(t
		 (skip-chars-forward " \t\f")
		 (cond ((looking-at "\\(\\w\\|\\s_\\)+:")
			(setq indent (max 1 (+ indent tie-label-offset))))
		       ((= (following-char) ?})
			(setq indent (- indent tie-indent-level)))
		       ((= (following-char) ?{)
			(setq indent (+ indent tie-brace-offset))))
		 (- indent (current-column)))))
    (skip-chars-forward " \t\f")
    (if (and (numberp shift-amt) (/= 0 shift-amt))
	(progn (delete-region beg (point))
	       (indent-to indent)))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))
    shift-amt))

(defun calculate-tie-indent (&optional parse-start)
  "Return appropriate indentation for current line as Tie code.
In usual case returns an integer: the column to indent to.
Returns (parse-state) if line starts inside a string."
  (save-excursion
    (beginning-of-line)
    (if (or (looking-at "[\t ]*\;")
	    (looking-at (concat "[\t ]*\\(" tie-stmt-regexp "\\)")))
	0
      (let ((indent-point (point))
	  (case-fold-search nil)
	  (colon-line-end 0)
	  state containing-sexp)
      (if parse-start			;used to avoid searching
	  (goto-char parse-start)
	(tie-beginning-of-function))
      (while (< (point) indent-point)	;repeat until right sexp
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
; state = (depth_in_parens innermost_containing_list last_complete_sexp
;          string_terminator_or_nil inside_commentp following_quotep
;          minimum_paren-depth_this_scan)
; Parsing stops if depth in parentheses becomes equal to third arg.
	(setq containing-sexp (nth 1 state)))
      (cond ((nth 3 state) state)	; In a quoted string?
	    ((null containing-sexp)	; Line is at top level.
	     (skip-chars-forward " \t\f")
	     (if (= (following-char) ?{)
		 0   ; move to beginning of line if it starts a function body
	       ;; indent a little if this is a continuation line
	       (tie-backward-to-noncomment)
	       (if (or (bobp)
		       (memq (preceding-char) '(?\; ?\})))
		   0 tie-continued-statement-offset)))
	    ((/= (char-after containing-sexp) ?{)
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     (goto-char (1+ containing-sexp))
	     (skip-chars-forward " \t")
	     (current-column))
	    (t
	     ;; Statement level.  Is it a continuation or a new statement?
	     ;; Find previous non-comment character.
	     (tie-backward-to-noncomment)
	     ;; Back up over label lines, since they don't
	     ;; affect whether our line is a continuation.
	     (while (or (eq (preceding-char) ?\,)
			(and (eq (preceding-char) ?:)
			     (memq (char-syntax (char-after (- (point) 2)))
				   '(?w ?_))))
	       (if (eq (preceding-char) ?\,)
		   (tie-backward-to-start-of-continued-exp containing-sexp)
		 (beginning-of-line))
	       (tie-backward-to-noncomment))
	     ;; Now we get the answer.
	     (if (not (memq (preceding-char) '(?\; ?\} ?\{)))
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  tie-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (tie-backward-to-start-of-continued-exp containing-sexp)
		   (+ tie-continued-statement-offset (current-column)
		      (if (save-excursion (goto-char indent-point)
					  (looking-at "[ \t]*{"))
			  tie-continued-brace-offset 0)))
	       ;; This line starts a new statement.
	       ;; Position at last unclosed open.
	       (goto-char containing-sexp)
	       (or
		 ;; If open paren is in col 0, close brace is special
		 (and (bolp)
		      (save-excursion (goto-char indent-point)
				      (looking-at "[ \t]*}"))
		      tie-indent-level)
		 ;; Is line first statement after an open-brace?
		 ;; If no, find that first statement and indent like it.
		 (save-excursion
		   (forward-char 1)
		   ;; Skip over comments and labels following openbrace.
		   (while (progn
			    (skip-chars-forward " \t\f\n")
			    (cond ((looking-at ";?//")
				   (forward-line 1) t)
				  ((looking-at "\\(\\w\\|\\s_\\)+:")
				   (save-excursion 
				     (end-of-line) 
				     (setq colon-line-end (point)))
				   (search-forward ":")))))
		   ;; The first following code counts
		   ;; if it is before the line we want to indent.
		   (and (< (point) indent-point)
			(if (> colon-line-end (point))
			    (- (current-indentation) tie-label-offset)
			  (current-column))))
		 ;; If no previous statement,
		 ;; indent it relative to line brace is on.
		 ;; For open paren in column zero, don't let statement
		 ;; start there too.  If tie-indent-level is zero,
		 ;; use tie-brace-offset + tie-continued-statement-offset
		 ;; For open-braces not the first thing in a line,
		 ;; add in tie-brace-imaginary-offset.
		 (+ (if (and (bolp) (zerop tie-indent-level))
			(+ tie-brace-offset tie-continued-statement-offset)
		      tie-indent-level)
		    ;; Move back over whitespace before the openbrace.
		    ;; If openbrace is not first nonwhite thing on the line,
		    ;; add the tie-brace-imaginary-offset.
		    (progn (skip-chars-backward " \t")
			   (if (bolp) 0 tie-brace-imaginary-offset))
		    ;; If the openbrace is preceded by a parenthesized exp,
		    ;; move to the beginning of that;
		    ;; possibly a different line
		    (progn
		      (if (eq (preceding-char) ?\))
			  (forward-sexp -1))
		      ;; Get initial indentation of the line we are on.
		      (current-indentation)))))))))))

(defun tie-backward-to-noncomment ()
  "Move point backward to after the first non-white-space, skipping comments."
  (interactive)
  (let (opoint stop)
    (while (not stop)
      (setq opoint (point))
      (beginning-of-line)
      (if (and comment-start-skip
	       (re-search-forward comment-start-skip opoint 'move 1))
	  (progn (goto-char (match-end 1))
		 (skip-chars-forward ";")))
      (skip-chars-backward " \t\f")
      (setq stop (or (bobp)
		     (not (bolp))
		     (forward-char -1))))))

(defun tie-backward-to-start-of-continued-exp (lim)
  (if (= (preceding-char) ?\))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t\f"))
 
;; note: this may be slower than the c-mode version, but I can understand it.
(defun indent-tie-exp ()
  "Indent each line of the Tie grouping following point."
  (interactive)
  (let* ((case-fold-search nil)
	 (oldpnt (point-marker))
	 (bof-mark (save-excursion
		     (end-of-line 2)
		     (tie-beginning-of-function)
		     (point-marker)))
	 eol last-mark lsexp-mark delta)
    (if (= (char-after (marker-position bof-mark)) ?=)
	(message "Can't indent a format statement")
      (message "Indenting Tie expression...")
      (save-excursion (end-of-line) (setq eol (point)))
      (save-excursion			; locate matching close paren
	(while (and (not (eobp)) (<= (point) eol))
	  (parse-partial-sexp (point) (point-max) 0))
	(setq last-mark (point-marker)))
      (setq lsexp-mark bof-mark)
      (beginning-of-line)
      (while (< (point) (marker-position last-mark))
	(setq delta (tie-indent-line nil (marker-position bof-mark)))
	(if (numberp delta)		; unquoted start-of-line?
	    (progn 
	      (if (eolp)
		  (delete-horizontal-space))
	      (setq lsexp-mark (point-marker))))
	(end-of-line)
	(setq eol (point))
	(if (nth 4 (parse-partial-sexp (marker-position lsexp-mark) eol))
	    (progn			; line ends in a comment
	      (beginning-of-line)
	      (if (or (not (looking-at "\\s-*;?//"))
		      (listp delta)
		      (and (/= 0 delta)
			   (= (- (current-indentation) delta) comment-column)))
		  (if (and comment-start-skip
			   (re-search-forward comment-start-skip eol t))
		      (indent-for-comment))))) ; indent existing comment
	(forward-line 1))
      (goto-char (marker-position oldpnt))
      (message "Indenting Tie expression...done"))))
 
(defun tie-beginning-of-function (&optional arg)
  "Move backward to next beginning-of-function, or as far as possible.
With argument, repeat that many times; negative args move forward.
Returns new value of point in all cases."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0) (forward-char 1))
  (and (/= arg 0)
       (re-search-backward (concat "^\\s-*\\(" tie-stmt-regexp "\\)") nil 'move arg)
       (goto-char (match-beginning 0))
       (skip-chars-forward " \t"))
  (point))

;; note: this routine is adapted directly from emacs lisp.el, end-of-defun;
;; no bugs have been removed :-)
(defun tie-end-of-function (&optional arg)
  "Move forward to next end-of-function.
The end of a function is found by moving forward from the beginning of one.
With argument, repeat that many times; negative args move backward."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((first t))
    (while (and (> arg 0) (< (point) (point-max)))
      (let ((pos (point)) npos)
	(while (progn
		(if (and first
			 (progn
			  (forward-char 1)
			  (tie-beginning-of-function 1)
			  (not (bobp))))
		    nil
		  (or (bobp) (forward-char -1))
		  (tie-beginning-of-function -1))
		(setq first nil)
		(or (and (looking-at "reference\\|\\(implicit_\\)\\?operand")
			 (forward-list 1))
		    (and (looking-at "semantic\\|exception")
			 (forward-list 2))
		    (tie-beginning-of-function -1))
		;(forward-list 1)
		(skip-chars-forward " \t")
		(if (looking-at "[//\n]")
		    (forward-line 1))
		(<= (point) pos))))
      (setq arg (1- arg)))
    (while (< arg 0)
      (let ((pos (point)))
	(tie-beginning-of-function 1)
	(or (and (looking-at "reference")
		 (forward-list 1))
	    (and (looking-at "semantic\\|exception")
		 (forward-list 2))
	    (progn
	      (tie-beginning-of-function -1)
	      (forward-line -1)
	      ))
	;(forward-sexp 1)
	(forward-line 1)
	(if (>= (point) pos)
	    (if (progn (tie-beginning-of-function 2) (not (bobp)))
		(progn
		  (forward-list 1)
		  (skip-chars-forward " \t")
		  (if (looking-at "[#\n]")
		      (forward-line 1)))
	      (goto-char (point-min)))))
      (setq arg (1+ arg)))))
 
(defun old-tie-end-of-function (&optional arg)
  "Move forward to next end-of-function.
The end of a function is found by moving forward from the beginning of one.
With argument, repeat that many times; negative args move backward."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((first t))
    (while (and (> arg 0) (< (point) (point-max)))
      (let ((pos (point)) npos)
	(while (progn
		(if (and first
			 (progn
			  (forward-char 1)
			  (tie-beginning-of-function 1)
			  (not (bobp))))
		    nil
		  (or (bobp) (forward-char -1))
		  (tie-beginning-of-function -1))
		(setq first nil)
		(forward-list 1)
		(skip-chars-forward " \t")
		(if (looking-at "[#\n]")
		    (forward-line 1))
		(<= (point) pos))))
      (setq arg (1- arg)))
    (while (< arg 0)
      (let ((pos (point)))
	(tie-beginning-of-function 1)
	(forward-sexp 1)
	(forward-line 1)
	(if (>= (point) pos)
	    (if (progn (tie-beginning-of-function 2) (not (bobp)))
		(progn
		  (forward-list 1)
		  (skip-chars-forward " \t")
		  (if (looking-at "[#\n]")
		      (forward-line 1)))
	      (goto-char (point-min)))))
      (setq arg (1+ arg)))))

(defun tie-mark-function ()
  "Put mark at end of Tie function, point at beginning."
  (interactive)
  (push-mark (point))
  (tie-end-of-function)
  (push-mark (point))
  (tie-beginning-of-function)
  (backward-paragraph))

;;
;; Provide utility functions to add tie structure elements
(defun tie-mode-add-regfile (name width entries short-name)
  "Prompt the user for the characteristics of the register file and insert
the appropriate tie code in the buffer."
  (interactive
   (list
    (read-string "Register file name: ")
    (read-string "Width (in bits): ")
    (read-string "Entries: ")
    (read-string "Short name: ")))
  (insert-string (concat "regfile " name " " width " " entries "  " short-name)))

(defun tie-mode-add-table (name width size)
  "Prompt the user for the characteristics of the table file and insert
tie code for an empty one in the buffer."
  (interactive
   (list
    (read-string "Table name: ")
    (read-string "Width (in bits): ")
    (read-string "Size (in entries): ")))
  (insert-string (concat "table " name " " width " " size " {\n// comma delimited entries here\n}\n"))
  )

(defun tie-mode-add-operation (name)
  "Prompt the user for the characteristics of the operation and insert
the appropriate tie code in the buffer."
  (interactive
   (list
    (read-string "Operation name: ")))
  (insert-string (concat "operation " name " { /* out AR a, in AR b, in AR c */ } { /* in state */ }\n{\n  // semantic goes here\n}\n"))
  )

(defun tie-mode-add-semantic (name)
  "Prompt the user for the characteristics of the operation and insert
the appropriate tie code in the buffer."
  (interactive
   (list
    (read-string "Operation name: ")))
  (insert-string (concat "semantic " name " { /* comma delimited operation list here */ } {\n  // semantic here\n}\n"))
  )

(defun tie-mode-add-ctype (name)
  "Prompt the user for the characteristics of the operation and insert
the appropriate tie code in the buffer."
  (interactive
   (list
    (read-string "CType name: ")))
  (insert-string (concat "ctype " name " /*size*/ /*alignment*/ /*regfile-name*/\n"))
  )


(defun tie-mode-add-lookup (name request-width result-width request-stage result-stage has-ready)
  "Prompt the user for the characteristics of the register file and insert
the appropriate tie code in the buffer."
  (interactive
   (list
    (read-string "Lookup name: ")
    (read-string "Request Width (in bits): ")
    (read-string "Result Width (in bits): ")
    (read-string "Request stage: ")
    (read-string "Result stage: ")
    (read-string "Has ready (rdy or empty): ")))
  (insert-string (concat "lookup " name " { " request-width ", " request-stage " } { " result-width ", " result-stage " } " has-ready "\n// interfaces " name "_Out " name "_In " name "_Rdy")))

(defun tie-mode-add-queue (name width direction)
  "Prompt the user for the characteristics of the register file and insert
the appropriate tie code in the buffer."
  (interactive
   (list
    (read-string "Queue name: ")
    (read-string "Width (in bits): ")
    (read-string "Direction (in or out): ")))
  (insert-string (concat "queue " name " " width " " direction "\n// interfaces " name " " name "_KILL " name "_NOTRDY")))

(defun tie-mode-add-immediate-range (name low-value high-value step-size)
  "Prompt the user for the characteristics of the register file and insert
the appropriate tie code in the buffer."
  (interactive
   (list
    (read-string "Immediate_range name: ")
    (read-string "Low value: ")
    (read-string "High value: ")
    (read-string "Step size: ")))
  (insert-string (concat "immediate_range " name " " low-value " " high-value " " step-size)))

(defun tie-mode-add-ctype-protos (name)
  "Prompt the user for the characteristics of the operation and insert
the appropriate tie code in the buffer."
  (interactive
   (list
    (read-string "CType name: ")))
  (insert-string (concat 
		  "proto " name "_move"
		  " { out " name " v, in " name " i }"
		  "{} {\n  /* M64I v, i; */\n}\n"

		  "proto " name "_loadi"
		  "{ out " name " v, in " name " *p, in immediate o }"
		  "{} {\n  /* L64I v, p, o; */\n}\n"
		  
		  "proto " name "_loadiu"
		  " { out " name " v, inout " name " *p, in immediate o }"
		  "{} {\n  /* L64IU v, p, o; */\n}\n"
		  
		  "proto " name "_storei"
		  "{ in " name " v, in " name " *p, in immediate o }"
		  "{} {\n  /* S64I v, p, o; */\n}\n"

		  "proto " name "_storeiu"
		  " { in " name " v, in " name " *p, in immediate o }"
		  "{} {\n  /* S64I v, p, o; */\n}\n"

		  "proto " name "_loadx"
		  " { in " name " v, in " name " *p, in int32 x }"
		  "{} {\n  /* L64X v, p, x; */\n}\n"

		  "proto " name "_loadxu"
		  " { in " name " v, in " name " *p, in int32 x }"
		  "{} {\n  /* L64XU v, p, x; */\n}\n"

		  "proto " name "_storex"
		  " { in " name " v, in " name " *p, in int32 x }"
		  "{} {\n  /* S64X v, p, x; */\n}\n"

		  "proto " name "_storexu"
		  " { in " name " v, in " name " *p, in int32 x }"
		  "{} {\n  /* S64XU v, p, x; */\n}\n"))
  )

(defun tie-mode-add-ctype-conversions (name name2)
  "Prompt the user for the characteristics of the operation and insert
the appropriate tie code in the buffer."
  (interactive
   (list
    (read-string "CType name: ")
    (read-string "Other CType name: ")))
  (insert-string (concat 
		  "proto " name "_rtor_" name2 
		  " { out " name2 " dst, in " name " src }"
		  "\n{ /* int32 tmp, uint32 tmp2 */ }\n"
		  "{\n /* cvt_src_to_dst */\n}\n"
		  
		  "proto " name "_mtor_" name2 
		  " { out " name2 " dst, in " name " *p, in immediate o }"
		  "\n{ /* int32 tmp, uint32 tmp2 */ }\n"
		  "{\n /* cvt_src_to_dst */\n}\n"

		  "proto " name "_rtom_" name2 
		  " { in " name2 " src, in " name " *p, in immediate o }"
		  "\n{ /* int32 tmp, uint32 tmp2 */ }\n"
		  "{\n /* cvt_src_to_dst */\n}\n"

		  "proto " name2 "_rtor_" name 
		  " { out " name " dst, in " name2 " src }"
		  "\n{ /* int32 tmp, uint32 tmp2 */ }\n"
		  "{\n /* cvt_src_to_dst */\n}\n"

		  "proto " name2 "_mtor_" name 
		  " { out " name " dst, in " name2 " *p, in immediate o }"
		  "\n{ /* int32 tmp, uint32 tmp2 */ }\n"
		  "{\n /* cvt_src_to_dst */\n}\n"

		  "proto " name2 "_rtom_" name 
		  " { in " name " src, in " name2 " *p, in immediate o }"
		  "\n{ /* int32 tmp, uint32 tmp2 */ }\n"
		  "{\n /* cvt_src_to_dst */\n}\n"))
  )


(defun tie-mode-add-state (name width reset)
  "Prompt the user for the characteristics of the state and insert
the appropriate tie code in the buffer."
  (interactive
   (list
    (read-string "State name: ")
    (read-string "Width (in bits): ")
    (read-string "Reset Value (e.g. 32'b0 if any): ")
    ;;; add_read_write
    ))
  (insert-string (concat "state " name " " width " " reset "/* add_read_write */ /* export */\n")) 
  )

;;
;; Provide utility functions to add tie structure elements
(defun tie-mode-add-length (name length decode-slice decode-const)
  "Prompt the user for the characteristics of the length and insert
the appropriate tie code in the buffer."
  (interactive
   (list
    (read-string "Length name: ")
    (read-string "Length (24, 32, 64): ")
    (read-string "Decode-slice (from InstBuf[]): ")
    (read-string "Decode-const (n'fxx): ")
    ))
  (insert-string (concat "length " name " " length " { InstBuf[" decode-slice "] == " decode-const " }")))

;;
;; Provide utility functions to add tie structure elements
(defun tie-mode-add-format (name length slot-list decode-expr)
  "Prompt the user for the characteristics of the format and insert
the appropriate tie code in the buffer."
  (interactive
   (list
    (read-string "Format name: ")
    (read-string "Length (in bits or length name): ")
    (read-string "Slots (comma separated): ")
    (read-string "Decode-expr? ({InstBuf[n:m] == const}): ")
    ))
  (insert-string (concat "format " name " " length " { " slot-list " } " decode-expr)))

;;
;; Provide utility functions to add tie structure elements
(defun tie-mode-add-slot (name format-expr)
  "Prompt the user for the characteristics of the slot and insert
the appropriate tie code in the buffer."
  (interactive
   (list
    (read-string "Slot name: ")
    (read-string "Format expr: ")))
  (insert-string (concat "slot " name " " format-expr )))

(defun tie-mode-add-slot-opcodes (name inst-list)
  "Prompt the user for the characteristics of the slot opcodes statement
and insert the appropriate tie code in the buffer."
  (interactive
   (list
    (read-string "Slot name: ")
    (read-string "Instruction List: (comma separated): ")
    ))
  (insert-string (concat "slot_opcodes " name " { " inst-list " }")))

 
;;;
;;; Add tc errors/file regexps to the compile regexp lists
;;;
(require 'compile)
(if (< emacs-major-version 22)
    (progn ;; for emacs 21
      (setq compilation-error-regexp-alist
	    (cons
	     '("File \\(\\w+\\), Line: \\([0-9]+\\), Warning: ([A-Z0-9_]+)" 1 2)
	     compilation-error-regexp-alist))
      (setq compilation-error-regexp-alist
	    (cons
	     '("Error ([A-Z0-9_]+): Line \\([0-9]+\\): \\(, File \\(\\w+\\):\\)?" 2 1)
	     compilation-error-regexp-alist))
      (setq compilation-error-regexp-alist
	    (cons 
	     '("Fatal ([A-Z0-9_]+): Line \\([0-9]+\\): \\(, File \\(\\w+\\):\\)?" 2 1)
	     compilation-error-regexp-alist))
      
      (cond (tie-mode-running-emacs
	     (setq compilation-file-regexp-alist
		   (cons
		    '("tc: reading \\(.+\\)$" 1)
		    compilation-file-regexp-alist)))))
  ;; For emacs-22 and later
  (push '(tie-compiler
	   "^File: \\([^,]+\\), Line: \\([0-9]+\\), \\(?:\\(Error\\)\\|\\(Warning\\)\\|\\(Message\\)\\): (\\(.+\\))," 
	   1 2 nil (4 . 5))
	compilation-error-regexp-alist-alist)
  (push  'tie-compiler compilation-error-regexp-alist))

 
;;;
;;; Integration with Tensilica tools/methodology
;;;

(defvar xttools-registry (getenv "XTENSA_SYSTEM")
  "*Where to find the XT core registry.  See the Xtensa documentation
for details.")
(defvar xttools-core-name (getenv "XTENSA_CORE")
  "*Name of the core to use for compilation.  See the Xtensa
documentation for details")
(defun xtregistry-cmd-args ()
  "Return appropriate arguments for the xtensa-registry."
  (interactive)
  (concat
   (if xttools-registry
       (concat "--xtensa-system=" xttools-registry " ")
     (and (getenv "XTENSA_SYSTEM")
	  (concat "--xtensa-system=" (getenv "XTENSA_SYSTEM") " ")))
   (if xttools-core-name
       (concat "--xtensa-core=" xttools-core-name " ")
     (and (getenv "XTENSA_CORE")
	(concat "--xtensa-core=" (getenv "XTENSA_CORE") " ")))
   ))
 
;;;
;;; Interface to tc (for lint and compile)
;;;
(defcustom tie-xttools-directory nil
  "*Location of Tensilica XT tools package."
  :type 'directory
  :group 'tie
)
(defcustom tie-compiler-default-args "-d tdk"
  "*Default arguments used when calling the TIE compiler (tc).
For example deault TDK directory, package name, etc."
  :group 'tie
  :type 'string)

(defvar tie-mode-lint-args-history nil)
(defun tie-lint ()
  "Run tc with the -lint option to generate the error/warning summary.
Use up-arrow/down-arrow to search through the compile command history."
  (interactive)
  (if (buffer-file-name)
      (let (
	    (compile-command
	     (read-string "Tc lint command: "
			  (concat tie-xttools-directory "/bin/tc -lint "
				  (xtregistry-cmd-args)
				  (buffer-file-name))
			  '(tie-mode-lint-args-history)))
	     )
	(compile compile-command))
    (message "Please save your work before linting")
    )
  )
(defvar tie-mode-compile-args-history nil)
(defun tie-mode-compile ()
  "Run tc on the current file.  Press up-arrow/down-arrow to search
through the compile command history."
  (interactive)
  (if (buffer-file-name)
      (let (
	    (compile-command
	     (read-string "Tc command: "
			  (concat (if tie-xttools-directory
				      (concat tie-xttools-directory "/bin/tc ")
				    "tc ")
				  (xtregistry-cmd-args)
				  tie-compiler-default-args
				  (buffer-file-name))
			  '(tie-mode-compile-args-history)))
	    )
	(compile compile-command))
    (message "Please save your work before linting")
    )
  )


;;
;; Give them an easy way to send suggestions
;; FIXME: create a mail 
(defun tie-mode-suggestions ()
  (interactive)
  (mail)
  (insert "tie-mode at tensilica dot com")
  (forward-line 1)
  (end-of-line)
  (insert "Suggestions for tie-mode")
  (goto-char (point-max))
  (insert "\n\n\nInsert your suggestion here.\n\n")
  (insert "Press C-c C-c when you are ready to send this message.\n\n")
  (forward-line -4)
  )

(provide 'tie-mode)

;;; tie-mode.el ends here

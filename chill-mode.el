;;; @@ Copyright
;;;
;;; Copyright (C) 1995-2000 August Hoerandl <august.hoerandl@gmx.at>
;;; Parts Copyright (C) 1995 Eva Bahr-Kitzler
;;; Parts Copyright (C) 2000 Martin G C Davies <Martin.Davies@alcatel.be>
;;;
;;; Keywords: languages
;;; This file is NOT part of XEmacs.
;;;
;;; Most of this program was written and tested during my time 
;;; at Alcatel Austria
;;; Alcatel Austria disclaims all copyright interests in the program.
;;; This software isn't released by Alcatel Austria and 
;;; Alcatel Austria won't provide any support and/or warranty.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; HOWTO:
;;; This mode should work with xemacs 
;;;
;;; Some Functions need a TAGS file - this has to be created with an 
;;; external program
;;;
;;; put this in your .emacs file
;;; 
;;; (load "chill-mode")
;;;
;;; for font lock 
;;;  (add-hook 'chill-mode-hook      'turn-on-font-lock)
;;;
;;; for function menu
;;;  (add-hook 'chill-mode-hook      'fume-add-menubar-entry)
;;;
;;; todo:
;;;  1) indent problems:
;;;    1.1) fix
;;;      a) strings with ";" 
;;;          yy := "xxx" //
;;;                "sdfgh;sdfgh";
;;;                zz := sdfgh;
;;;      b) statement 1
;;;            continue statement 1; statement 2;
;;;            wrong statement 3;
;;; 
;;;      c) first line of buffer and lines starting with <>:
;;;         okay, but done in a strange way
;;;    
;;;    1.2) <> compiler <>  should be a comment 
;;;         (but there are only two comment styles supported)
;;;
;;;    1.3) only one pass indent ( parse-partial-sexp ) 
;;;
;;;    1.4) firstline; /* comment with -- */
;;;         nextline is indented to left margin
;;;     emacs-bug ? (forward-comment -999) from nextline 
;;;                      moves to this ^ position on firstline
;;;
;;;    1.5) fast support for indent-region
;;;
;;;  2) tag
;;;  2.1) insert size
;;;      handle set-values - insert mode name
;;;      needs TAGS-File with mode name after ^a
;;;
;;;  2.2) insert size without loading file
;;;       use only info from TAGS file
;;;  
;;;  3) case
;;;     insert (xx): for all set-values
;;;
;;;  4) abbrev/completion
;;;     6.1) support abbrev as in pascal mode
;;;     6.2) support completions on local symbols & TAGS file 
;;;
;;;  6) func-menu
;;;     modeline support for nested functions
;;;
;;; history:
;;; 
;;; Wed Mar 29 1995 - added [^a-z_] before/after types
;;; Wed Mar 29 1995 - added no \n within strings
;;; Fri Mar 31 1995 - new keywords
;;; Wed Jun 14 1995 - ch-insert-seize
;;; Jul 1995 - a little indent
;;; nov 1995 - more indent 
;;; jan 1996 - new read parameter, indent: <>, END ON, comments
;;; Sun Jun 15 1997 - add support for #preproc
;;; Mai 2000 - merged code from Martin G C Davies <Martin.Davies@alcatel.be>
;;;           first public release
;;;
;;; Options
;;;

(defvar chill-mode-hook nil  
  "*List of hook functions run by `chill-mode'.")

(defvar ch-indent 2 
  "*This variable gives the indentation in CHILL-Mode")

(defvar ch-auto-indent t
  "*Indent on special characters (newline, ';' and ':')
set to nil for NO autoindent")

(defvar ch-proc-end-on-else-handler "END ON ELSE\n;"
"*END ON ELSE Handler for CHILL procedure/process")

;;;
;;; no user options beyond this point
;;;

(require 'font-lock)
(require 'func-menu)
(require 'etags)

;;;
;;; chill keywords an types
;;;  used for font lock mode 
;;; 
; my old version
;(defvar chill-keywords 
;  '("access" "after" "all" "and" "andif" "array" "assert" "at"
;    "begin" "based" "bin" "body" "bools" "buffer" "by" "case" "cause"
;    "chars" "context" "continue" "cycle" "dcl" "delay" "do"
;    "down" "dynamic" "else" "elsif" "end" "esac" "event" "ever"
;    "exceptions" "exit" "fi" "for" "forbid" "general" "goto" 
;    "grant" "if" "in" "inout" "init" "inline" "inout" "loc" "mod"
;    "module" "newmode" "nonref" "nopack" "not" "od" "of" "on"
;    "or" "orif" "out" "pack" "pos" "powerset" "prefixed"
;    "priority" "proc" "process" "range" "read" "receive" "recursive" "ref"
;    "region" "rem" "remote" "result" "return" "returns" "row"
;    "seize" "send" "set" "signal" "simple" "spec" "start"
;    "static" "step" "stop" "struct" "syn" "synmode" "text"
;    "then" "this" "timeout" "to" "up" "varying" "while" "with"
;    "xor"
;    )
;  "list of all chill keywords")

;; new version by Martin Davies
(defvar chill-keywords 
  '(
    "addr" "all" "and" "array" "asm" "assert" "based" "begin"
    "bit_string_literal" "buffer" "by" "call" "case" "cause"
    "char_string_literal" "continue" "dcl" "delay" "do" "down" "dynamic"
    "else" "elsif" "end" "esac" "event" "ever" "exceptions" "exit" "fi"
    "for" "forbid" "general" "goto" "grant" "if" "in" "init" "inline"
    "inout" "integer_literal" "loc" "long_int_literal" "long_loc"
    "long_ref" "mod" "module" "name" "newmode" "non_linked" "nopack" "not"
    "od" "of" "on" "or" "out" "pack" "pervasive" "pos" "powerset"
    "priority" "proc" "process" "range" "read" "receive" "recursive"
    "redefined" "ref" "region" "rem" "result" "return" "returns" "row"
    "seize" "send" "set" "signal" "simple" "start" "static" "step" "stop"
    "struct" "syn" "synmode" "then" "to" "up" "while" "with" "xor"

    "false" "true" "null" "abs" "pred" "size" "subbit" "substr" "succ"
    "num" "length" "sizeof" "instance" "this" "upper" "lower" "extend"
    "truncate" 
    ) 
  "list of all chill keywords")


(defvar chill-types 
  '( "byte" "ubyte" "int" "uint" "long" "ulong" "bit"
     "bin" "bit"
     "long_int" "amp_int"
     "bool" "char" "float" "real" "long_real" "instance"
     "ptr" "long_ptr" 
     "char" "float" "double" "void" "struct"
     "num" "min" "max" "card" "writetext" "readtext" "size" "length"
     "trunc" "round" "pred" "read" "readonly" "readwrite" "writeonly"
     "addr" 
     "lower" "upper"
     "true" "false"
     )
  "list of all predefined chill symbols")

(defvar ch-grt-file-ext ""
  "Extension for USE_SEIZE_FILE.
use \".grt\" for gnu-chill")

(defvar chill-mode-syntax-table nil
  "Syntax table used while in CHILL mode.")

(defvar chill-mode-abbrev-table nil
  "Abbrev table used while in CHILL mode.")

(define-abbrev-table 'chill-mode-abbrev-table ())

(defvar chill-mode-map nil
  "Mode map used while in CHILL mode.")

(defconst chill-mode-menu
  '("Chill"
    ["Comment Out Region"     comment-region (mark)]
    ("Insert"
     ["New Module" ch-definition t ]
     ["Header" ch-header t ]
     ["Message Loop" ch-message-loop t]
     ["Begin/end" ch-begin t]
     ["Case" ch-case t ]
     ["Comment" ch-add-comment t]
     ["Do for" ch-for t]
     ["Do with" ch-with t]
     ["Do while" ch-while t]
     ["If" ch-if t ]
     ["Procedure" ch-procedure t]
     ["Process" ch-process t]
     ["Get Record" ch-get-record t ]
     ["Receive Signal" ch-receive-signal t ]
     ["Start Timeout" ch-start-signal-timeout t ]
     )
    ("Goto"
     ["proc/process/module" chill-goto-PROC t]
     ["dcl/syn/synmode/newmode" chill-goto-DCL t]
     ["Function" fume-prompt-function-goto t]
     )
    ("Change"
     ("Keywords"
      ["downcase" chill-downcase-keywords t]
      ["upcase" chill-upcase-keywords t]
      ["capitalize" chill-capitalize-keywords t]
      )
     ("Modes"
      ["downcase" chill-downcase-types t]
      ["upcase" chill-upcase-types t]
      ["capitalize" chill-capitalize-types t]
      )
     )
    ("Debug"
     ["Start Debugger" start-gdb-other-window t]
     ["Start Debugger & Run" start-gdb-other-window-and-run t]
     )
    ("Tags"
     ["Insert Global Symbol" ch-insert-global-symbol t]
     ["Insert SEIZE statement" ch-insert-seize t]
     ["Find TAG" find-tag t]
     ["Find TAG other Window" find-tag-other-window t]
     )
    ["------" nil t]
    ["Refontify" font-lock-fontify-buffer t]
    )
  "XEmacs menu for Chill mode.")

;; menus for XEmacs (formerly Lucid)
(defun ch-popup-menu (e)
  "Pops up the Chill menu."
  (interactive "@e")
  (popup-menu chill-mode-menu)
  ;; do whatever is necessary to keep the region active in XEmacs
  ;; (formerly Lucid). ignore byte-compiler warnings you might see
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(if chill-mode-syntax-table
    ()
  (setq chill-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?' ".   "  chill-mode-syntax-table) ; chill uses D'000
  (modify-syntax-entry ?\\ ".   " chill-mode-syntax-table)
  ;; old comments
  (modify-syntax-entry ?/ ". 14" chill-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" chill-mode-syntax-table)
  ;; new comments
  (modify-syntax-entry ?- ". 56b" chill-mode-syntax-table)
  (modify-syntax-entry ?\^m  ">   b" chill-mode-syntax-table)
  (modify-syntax-entry ?\n   ">   b" chill-mode-syntax-table)
  ;; odd comments - <> compiler <>
  ;;(modify-syntax-entry ?< ". 58" chill-mode-syntax-table)
  ;;(modify-syntax-entry ?> ". 67" chill-mode-syntax-table)
 )

(if chill-mode-map
    ()
  (setq chill-mode-map (make-sparse-keymap))
  (define-key chill-mode-map "\177" 'backward-delete-char-untabify)
  (define-key chill-mode-map "\15"  'newline-and-indent)
  (define-key chill-mode-map ";" 'ch-electric-char)
  (define-key chill-mode-map ":" 'ch-electric-char)
   
  (define-key chill-mode-map "\C-cb" 'ch-begin)
  (define-key chill-mode-map "\C-cc" 'ch-case)
  (define-key chill-mode-map "\C-cd" 'ch-definition)
  (define-key chill-mode-map "\C-ce" 'ch-else)
  (define-key chill-mode-map "\C-cf" 'ch-for)
  (define-key chill-mode-map "\C-cg" 'ch-get-record)
  (define-key chill-mode-map "\C-ch" 'ch-header)
  (define-key chill-mode-map "\C-ci" 'ch-if)
  (define-key chill-mode-map "\C-cm" 'ch-message-loop)
  (define-key chill-mode-map "\C-cl" 'ch-loop)
  (define-key chill-mode-map "\C-co" 'ch-or)
  (define-key chill-mode-map "\C-cp" 'ch-procedure)
  (define-key chill-mode-map "\C-c\C-p" 'ch-process)
  (define-key chill-mode-map "\C-c\C-w" 'ch-with)
  (define-key chill-mode-map "\C-cr" 'ch-record)
  (define-key chill-mode-map "\C-c\C-r" 'ch-receive-signal)
  (define-key chill-mode-map "\C-cs" 'ch-insert-seize)
  (define-key chill-mode-map "\C-c\C-s" 'ch-start-signal-timeout)
  (define-key chill-mode-map "\C-ct" 'ch-type)
  (define-key chill-mode-map "\C-cv" 'ch-var)
  (define-key chill-mode-map "\C-cw" 'ch-while)
  (define-key chill-mode-map "\C-cx" 'ch-export)
  (define-key chill-mode-map "\C-cy" 'ch-import)
  (define-key chill-mode-map "\C-c{" 'ch-begin-comment)
  (define-key chill-mode-map "\C-c}" 'ch-end-comment)
  (define-key chill-mode-map "\C-m"  'ch-newline)

  ;; pop up menu
  (define-key chill-mode-map 'button3 'ch-popup-menu)

  ;; keys for funtion menu
  (define-key chill-mode-map "\C-c1" 'fume-list-functions)
  (define-key chill-mode-map "\C-cg" 'fume-prompt-function-goto)
  (define-key chill-mode-map '(shift button3) 'mouse-function-menu)
  (define-key chill-mode-map '(meta  button1) 'fume-mouse-function-goto)
  )

(defun chill-mode ()
  "chill-mode: Major mode for editing Chill code
support: indent (needs add. work)
         shortcut for standard syntax
         tagging
         function menu
         syntax highlighting
  
Key bindings:
\\{chill-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table chill-mode-syntax-table)
  (setq major-mode 'chill-mode
	mode-name "Chill"
	)
  (use-local-map chill-mode-map)
  (make-local-variable 'current-menubar)
  ;;(add-menu nil "Chill" chill-mode-menu)
  (add-submenu nil chill-mode-menu)
  ;;  (make-local-variable  'compile-command)
  ;;Extension for USE_SEIZE_FILE
  (make-local-variable  'ch-grt-file-ext)
  (if (string-match ".ch$" (buffer-name))
      (setq ch-grt-file-ext ".grt")
    (setq ch-grt-file-ext ""))
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (setq comment-start "/* "
	comment-end   " */"
	comment-column 32
	comment-start-skip "/\\*+ *"
	parse-sexp-ignore-comments t
	indent-line-function 'ch-indent-line
	indent-region-function nil)
  ;; run user definded hooks
  (run-hooks 'chill-mode-hook))

;; This is used by indent-for-comment
;; to decide how much to indent a comment in the file
;; based on its context.

(defun chill-comment-indent ()
  (if (looking-at "^/\\*")
      0				        ;Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))	;Else indent at comment column
	   comment-column))))	        ; except leave at least one space.

;;;
;;; chill - font lock mode
;;;

(defconst chill-font-lock-keywords (purecopy
  (let ((ident  "\\(\\(\\sw\\|\\s_\\)+\\)") ; indent
	(decl-1 "\\(proc\\|process\\|module\\)") 
	(decl-2 "\\(syn\\|synmode\\|newmode\\)") ; or less
 	)
    (list
     (list "\\(<>[^<\n]*<>\\)" 1 'font-lock-comment-face)
     (list (concat ident "[ \n\t]*:[ \n\t]*" decl-1) 
	   1 
	   'font-lock-function-name-face t)
     (list (concat decl-2 "[ \n\t]+" ident) 
	   2 
	   'font-lock-type-face t)
     (list (concat "\\S_\\<\\(" ; fix me - bad on line start		
		   (mapconcat 'identity  chill-keywords "\\|") 
		   "\\)\\>\\S_" )
	   1
	   'font-lock-keyword-face)
     (list (concat "\\S_\\<\\(" ; fix me - bad on line start
		   (mapconcat 'identity chill-types "\\|")
		   "\\)\\>\\S_")
	   1
	   'font-lock-type-face)
     (list (concat "dcl[ \t]+" ident) 
	   1 font-lock-variable-name-face 
	   t) ; dcl
     ;; ' is sometimes paired, sometimes not ?
     (list "[^dbhc]'\\([^'\n]*\\)'" 
	   1 'font-lock-string-face 
	   t) ; normal strings
     ;;(list "\\([dbhc]'[0-9a-f_]*\\)'" 1 'font-lock-string-face t);literals ?
     '("^[ \t]*\\(#[ \t]*[^ ]*\\)\\([^\n]*\\)"  
       (1 font-lock-preprocessor-face) 
       (2 font-lock-string-face nil t))
     )))
  "Expressions to highlight in Chill buffers.")

;; The keywords in the preceding lists assume case-insensitivity.
(put 'chill-mode 'font-lock-keywords-case-fold-search t)

;;;
;;;   chill - function menu 
;;;
(defconst fume-function-name-regexp-chill
   "\\(\\(\\sw\\|\\s_\\)+\\)[ \t\n]*:"
  "Expression to get CHILL function names
(part before process/proc")

(setq fume-function-name-regexp-alist
      ;; CHILL
      (cons '(chill-mode . fume-function-name-regexp-chill)
	    fume-function-name-regexp-alist))

(setq fume-find-function-name-method-alist
    (cons '(chill-mode      . fume-find-next-chill-function-name)
	  fume-find-function-name-method-alist))

;;; Specialised routine to get the next CHILL function name in the buffer.
;;;
(defun fume-find-next-chill-function-name (buffer)
  "Searches for the next CHILL function in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (catch 'loop
    (while (re-search-forward fume-function-name-regexp-chill nil t)
      (let ((mb (match-beginning 1))
	    (me (match-end 1)))
	(ch-forward-comment)
	(if (looking-at "proc")
	    (throw 'loop (cons (buffer-substring mb me) mb)))))))

;;;
;;; indent  CHILL 
;;;

;; move over comment and <> ... <> and #preproc 
;; would be easier if we had 4 comments in the syntax table ...
(defun ch-forward-comment ()
"move forward over comments and <> ... <>"
(forward-comment 999)
;; that was the easy part - now for <> and #
(if (looking-at "<>\\|#")
    (while
	(progn
	  (forward-char 2)
	  (search-forward-regexp "<>\\|\n" nil t)
	  (forward-comment 999)
	  (looking-at "<>\\|#")))))

(defun ch-after-directive ()
  "return t if point is after <> "
  (or (and (= (preceding-char) ?>)
	   (= (char-after (- (point) 2)) ?<))
      (save-excursion
	(beginning-of-line)
	(looking-at "[ \t]*#"))))

(defun ch-backward-comment ()
"move backward over comments and <> ... <>"
(forward-comment -999)
;; that was the easy part - now for <> 
(if (ch-after-directive)
    (while
	(progn
	  (forward-char -2)
	  (search-backward-regexp "<>\\|#" nil t)
	  (forward-comment -999)
	  (ch-after-directive)))))

(defun ch-newline ()
  "Insert a newline and indent following line like previous line."
  (interactive)
  (if ch-auto-indent
      (ch-indent-line))
  (insert "\n")
  (ch-indent-line))

(defconst ch-indent-plus
  "\\b\\(do\\|process\\|proc\\|on\\|case\\|else\\|then\\|begin\\|module\\)\\b\\S_\\|([^)]*):"
  "reg expr - indent more")

(defconst ch-indent-minus
  "\\b\\(fi\\|od\\|end\\|esac\\|else\\|elsif\\)\\b\\S_\\|([^)]*):"
  "reg expr - indent less")

(defun ch-calc-add-indent (from to)
  "calc additional indentation between FROM and TO"
  ;; set lastkw to end of last matched keyword
  ;; ignore byte compiler warnings about last-kw
  (let ((res 0) state)
    (goto-char from)
    (if (= to (point-max))
	(setq to (- to 1)))
    (while (<= (point) to)
      (setq state (parse-partial-sexp (point) to nil t state))
      (let ((plus (looking-at ch-indent-plus))
	    (minus (looking-at ch-indent-minus)))
	(if (or plus minus)
	    (let ((me (match-end 0)))
	      ;; add up indent
	      (if (and plus (< (point) to))
		  (setq res (+ res ch-indent)))
	      (if (and minus (> (point) from))
		  (setq res (- res ch-indent)))
	      (setq last-kw me)
	      (goto-char me)
	      ;; (sit-for 1)
	      )
	  (forward-sexp))))
    res))

;;;
;;; indent one line 
;;;
(defun ch-indent-line ()
  "Indent one line"
  (let ((eol (eolp)))
    (save-excursion
      ;; 1. calc start point for calc
      ;; 1.1 clear current indentation
      (beginning-of-line)
      (delete-horizontal-space)
      (if (eolp)
	  (setq eol t))
      (let ((indent-point (point))
	    (new-indent 0) (last-non-comment 0)
	    state containing-sexp start-calc 
	    last-kw  last-is-semi)
	;; 1.2 move back 
	;; 1.2.1 <> compiler <>
	(if (looking-at "<>\\|#")
	    () ; indent <> to left margin
	  ;; 1.2.2 move back  
	  (ch-backward-comment)
	  ;; look for concate line
	  (if (or (= (preceding-char) ?\;)
		  (= (preceding-char) ?\:))
	      (progn 
		(forward-char -1) 
		(setq last-is-semi t)))
	  (setq last-non-comment (point))
	  ;; move back 
	  (while
	      (progn
		(search-backward-regexp ";" nil 0)
		(forward-char 1) 
		(ch-forward-comment)
		(beginning-of-line)
		(skip-chars-forward " \t")
		(looking-at "<>\\|#"))))
	;; 1.3 got start point
	(setq start-calc (point))
	(setq last-kw start-calc)
	;;(sit-for 1)
	;; 2. calc new indent
	(setq new-indent (current-indentation))
	;; 2.1 handle 'inside comment' (strange ??)
	(if (> (point) indent-point)
	    (setq state (list t t t t))
	  ;; 2.2 parse expressions
	  (while (< (point) indent-point)
	    (setq state (parse-partial-sexp (point) indent-point))
	    (setq containing-sexp (car (cdr state)))))
	;; 2.3 result of parsing
	(goto-char indent-point)
	(cond   
	 ;; inside string or comment
	 ((or (nth 3 state) 
	     (nth 4 state))
	  ;;(forward-line -1)
	  ;;(setq new-indent (current-indentation))
	  (search-backward "/*" nil t)
	  (setq new-indent (current-column))
	  (setq last-is-semi t))
	 ;; inside ()
	 (containing-sexp (goto-char (+ containing-sexp 1))
			  (skip-chars-forward " \t")
			  (setq new-indent (current-column))
			  (setq last-is-semi t)
			  ;; align after IN within (...):
			  (if (and (search-forward-regexp "IN[ \t]+" 
							  indent-point t)
				   (save-excursion
				     (up-list 1)
				     (looking-at ":")))
			      (setq new-indent (current-column))))
	 ;; else: sum up indent
	 (t (setq new-indent (+ new-indent 
				(ch-calc-add-indent start-calc 
						    indent-point)))))
	;; 3. set up indentation
	(goto-char indent-point)
	;; 3.1 handle `continued line'
	(if last-is-semi
	    ()
	  ;;(message "kw %d >= non-com %d" last-kw last-non-comment)
	  (if (>= last-kw last-non-comment)
	      ()
	    (goto-char start-calc)
	    ;; align after := or if or with
	    (if (search-forward-regexp "\\(:=\\|\\<if\\|\\<with\\)[ \t]+" 
				       indent-point t)
		(setq new-indent (current-column))
	      (setq new-indent (+ new-indent ch-indent)))
	    (goto-char indent-point)))
	;; 3.2 indent
	(indent-to new-indent)))
    ;; if we started at the end of a line
    (if eol
	(end-of-line))))
  
;;;
;;; auto indent on some keys
;;;
(defun ch-electric-char (arg)
  "Insert a character and indent line"
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if ch-auto-indent
      (ch-indent-line)))

;;;
;;; some helpful things
;;; 
(defun ch-buffer-name-no-ext ()
"Returns the name of the current buffer without extension" 
(substring (buffer-name)
	   (string-match "\\(\\sw\\|\\s_\\)*.*" 
			 (buffer-name))
	   (match-end 1)))

(defun ch-get-name (prompt)
  "get one name (using TAGS)" 
 (let ((completion-ignore-case t)
       (def (find-tag-tag prompt)))
   (if (listp def)  
       (car def) 
     def)))

(defun ch-read-parameter ()
"Returns nil for no parameter or a list of (name spec type name spec ...)"
(catch 'loop
  (let ((parms))
    (while 
	(let ((name (read-string "Paramtername: ")))
	  (if (string-equal name "")
	      (throw 'loop parms))
	  (let ((mode (ch-get-name "Mode: "))
		(spec (completing-read
		       "Spec: "
		       '(("IN" 0) ("OUT" 1)  ("INOUT" 1) ("LOC" 1) ("" 2))
		       nil t "IN")))
	    (if parms
		(setq parms (nconc parms (list name mode spec)))
	      (setq parms (list name mode spec)))))))))

(defun ch-insert-parameter (para header fill1 fill2)
"insert list of parameters PARA (from ch-read-parameter), 
insert: HEADER name FILL1 mode FILL1 spec FILL2 mode ... spec"
(if (not para)
    ()
  (insert header)
  (while
      (progn
	;; name
	(insert (car para) fill1)
	(setq para (cdr para))
	;; mode
	(insert (car para) fill1)
	(setq para (cdr para))
	;; spec
	(insert (car para))
	(setq para (cdr-safe para))
	(if para
	    (insert fill2))
	para))))

;;;
;;; some shortcuts for CHILL
;;;

(defun ch-begin ()
  "Insert a BEGIN keyword and indent for the next line."
  (interactive)
  (insert "BEGIN")
  (ch-newline)
  (save-excursion
    (ch-newline)
    (insert "END;")
    (ch-indent-line))
  (ch-indent-line))

(defun ch-case ()
  "Build skeleton CASE statement, prompting for the <expression>."
  (interactive)
  (let ((name (read-string "Case-Expression: ")))
    (insert "CASE " name " OF")
    (ch-newline)
    (ch-newline)
    (insert "ELSE")
    (ch-newline)
    (ch-newline)
    (insert "ESAC; /* case " name " */")
    (ch-indent-line))
  (end-of-line -2)
  (ch-indent-line))

(defun ch-definition ()
  "Build skeleton MODULE, prompting for the <module name>."
  (interactive)
  (let* ((guess (substring (buffer-name)
			   (string-match "\\(\\sw\\|\\s_\\)*.*" 
					 (buffer-name))
			   (match-end 1) ) )
	 (name (read-string "Name: " (concat guess "_md") )))
    (insert name " : MODULE\n\n")
    (ch-header)
    (insert    "\n<> DEBUG_LINES <>"
	       "\n<> DEBUG_SYMBOLS <>"
	       "\n<> DEBUG_TYPES <>"
	       "\n<> MAKE_PUBLICS_FOR_DISCRETE_SYNS <>"
	       "\n<> SUPPORT_CAUSING_ADDRESS <>"
	       "\n<> LARGE <>"
	       "\n<> EVEN <>"
	       "\n<> SEND_SIGNAL_DEFAULT_PRIORITY=1<>")
    (insert "\n\n\nEND " name ";\n"))
  (previous-line 3)
  (ch-indent-line))

(defun ch-else ()
  "Insert ELSE keyword and indent for next line."
  (interactive)
  (insert "ELSE")
  (ch-indent-line)
  (insert "\n\n")
  (end-of-line 0)
  (ch-indent-line))

(defun ch-for ()
  "Build skeleton DO FOR loop statement, prompting for the loop parameters."
  (interactive)
  (let ((name (read-string "Loop Variable: ")) 
	(from (read-string "From/In: ")) 
	(to (read-string "To: ")) 
	(whilex (read-string "While: "))
	(start (point)))
    (insert "DO FOR ")
    (if (string-equal to "")
	(insert name " IN " from )
      (insert name " := " from " TO " to))
    (if (not (string-equal whilex ""))
	(insert " WHILE " whilex))
    (insert ";\n")
    (save-excursion
      (if (string-equal to "")
	  (insert "\nOD; /* for " name " in " from)
	(insert "\nOD; /* for " name " := " from " to " to ))
      (insert " */")
      (indent-region start (point) nil))
  (ch-indent-line)))

(defun ch-header ()
  "Insert a comment block containing the module title, author, etc."
  (interactive)
  (insert "/*")
  (insert "\n----------------------------------------------------------------")
  (insert "\n--    $Id: chill-mode.el,v 1.1 2001/11/19 22:59:32 hoerandl Exp $\n--")
  (insert "\n--    Title: \t")
  (insert (read-string "Title: " (ch-buffer-name-no-ext)))
  (insert "\n--    File:\t")
  (insert (buffer-name))
  (insert "\n--    Created:\t")
  (insert (current-time-string))
  (insert "\n--    Author: \t" (user-full-name))
  (insert (concat "\t<" (user-login-name) ">"))
  (insert "\n--\n--\n--    Abstract:")
  (insert "\n--\n--")
  (insert "\n--     Modifications:")
  (insert "\n--         $Log: chill-mode.el,v $
  (insert "\n--         Revision 1.1  2001/11/19 22:59:32  hoerandl
  (insert "\n--         initial release
  (insert "\n--")
  (insert "\n--")
  (insert "\n----------------------------------------------------------------")
  (insert "\n*/\n\n"))

(defun ch-if ()
  "Insert skeleton IF statment, prompting for <boolean-expression>."
  (interactive)
  (insert "IF ")
  (let ((thecondition (read-string "<boolean-expression>: ")))
    (insert "( " thecondition " )")
    (ch-newline)
    (insert "THEN")
    (ch-newline)
    (save-excursion
      (ch-newline)
      (insert "FI; /* " thecondition " */")
      (ch-indent-line))
    (ch-indent-line)))

(defun ch-elsif ()
  "Insert skeleton ELSIF statment, prompting for <boolean-expression>."
  (interactive)
  (insert "ELSIF ")
  (let ((thecondition (read-string "<boolean-expression>: ")))
    (insert thecondition "\nTHEN")
    (ch-newline)
    (ch-newline))
  (end-of-line 0)
  (ch-indent-line))

(defun ch-or ()
  (interactive)
  (ch-newline)
  (backward-delete-char-untabify ch-indent)
  (insert "OR")
  (ch-newline)
  (ch-indent-line))

(defun ch-procedure ()
  (interactive)
  (let ((name (read-string "Procedure: " ))
	(args (ch-read-parameter))
	(returns (read-string "Result Type: "))
	(handler (y-or-n-p "END ON ELSE: "))
	(grant (y-or-n-p "Grant: "))
	(start (point)))
    (insert "/*")
    (insert "\n----------------------------------------------------------------")
    (insert "\n--")
    (insert "\n-- PROCEDURE " name)
    (insert "\n--")
    (ch-insert-parameter args "\n--  Parameter " "\t" "\n--            ")
    (if (not (string-equal returns ""))
	(insert "\n--\n--  RETURNS ( " returns " )"))
    (insert "\n--")
    (insert "\n----------------------------------------------------------------")
    (insert "\n*/\n\n")

    (insert name ": PROC ( ")
    (ch-insert-parameter args "" "\t" "\n")
    (insert ")")
    (if (not (string-equal returns ""))
	(insert "\n    RETURNS ( " returns " )"))
    (insert ";\n\n")
    (save-excursion
      (insert "\n")
      (if handler
	  (insert ch-proc-end-on-else-handler))
      (insert "\nEND ")
      (insert name)
      (insert ";")
      (if grant
	  (insert "\n\nGRANT " name ";"))
      (indent-region start (point) nil))
    (ch-indent-line)
    (end-of-line)))

(defun ch-process ()
  (interactive)
  (let* ((guess (ch-buffer-name-no-ext) )
	 (name (read-string "Process: " guess))
	 (args (ch-read-parameter))
	 (handler (y-or-n-p "END ON ELSE: "))
	 (start (point)))
    (insert "/*")
    (insert "\n----------------------------------------------------------------")
    (insert "\n--")
    (insert "\n-- PROCESS " name)
    (insert "\n--")
    (ch-insert-parameter args "\n--  Parameter " "\t" "\n--            ")
    (insert "\n--")
    (insert "\n----------------------------------------------------------------")
    (insert "\n*/\n\n")
    (insert "<> PROCESS_TYPE = sy_pr_" name " <>\n")
    (insert name ": PROCESS ( ")
    (ch-insert-parameter args "" "\t" "\n")
    (insert ");\n\n")
    (save-excursion
      (insert "\n")
      (if handler
	  (insert ch-proc-end-on-else-handler))
      (insert "\nEND ")
      (insert name)
      (insert ";")
      (insert "\n\nGRANT " name ";")
      (indent-region start (point) nil))
    (ch-indent-line)
    (end-of-line)))

(defun ch-with ()
  (interactive)
  (let ((start (point))
	(name (read-string "DO WITH Record-Type: ")))
    (insert "DO WITH " name ";\n")
    (save-excursion
      (insert "\nOD; /* with " name " */\n")
      (indent-region start (point) nil))
    (ch-indent-line)))

(defun ch-mode ()
  (interactive)
  (insert "NEWMODE =")
  (ch-newline)
  (ch-indent-line))

(defun ch-var ()
  (interactive)
  (ch-newline)
  (insert "DCL")
  (ch-newline)
  (ch-indent-line))

(defun ch-while ()
  (interactive)
  (let ((start (point))
	(name (read-string "DO WHILE <boolean-expression>: ")))
    (insert "DO WHILE " name ";\n" )
    (save-excursion
      (insert "\nOD; /* while " name " */\n")
      (indent-region start (point) nil))
    (ch-indent-line)))

(defun ch-begin-comment ()
  (interactive)
  (if (not (bolp))
      (indent-to comment-column 0))
  (insert "/*  "))

(defun ch-end-comment ()
  (interactive)
  (if (not (bolp))
      (indent-to comment-column))
  (insert "*/"))

(defun ch-line-comment ()
  (interactive)
  (if (not (bolp))
      (indent-to comment-column 0))
  (insert "--  "))

(defun ch-add-comment ()
  (interactive)
  (let ((start (point)))
    (insert "/*\n"
	    "--------------------------------------------------------------"
	    "\n--"
	    "\n-- ")
    (save-excursion
      (insert "\n--"
	      "\n--------------------------------------------------------------"
	      "\n*/\n")
      (indent-region start (point) nil))))

(defun ch-get-record ()
  (interactive)
  (let ((relation (ch-get-name "Relation: ")) 
	(num (read-string "Nummer: ")) 
	(buff (read-string "Buffer: ")) 
	(sel (read-string "Selector: "))
	(start (point)))
    (insert "GET_RECORD( " relation ",\n"
	    num ",\n"
	    "ADDR(" buff "), SIZE(" buff "),\n"
	    sel ",\n"
	    "sy_db_no_wait );\n")
    (indent-region start (point) nil)
    (ch-indent-line)))

(defun ch-start-signal-timeout ()
  (interactive)
  (let ((slot (read-string "Slot: " "sy_lpr_slot")) 
	(tocl (read-string "Zeitklasse: " "sy_class_")) 
	(dur (read-string "Dauer: ")) 
	(sig (ch-get-name "Signal: "))
	(start (point)))
    (insert "START_SIGNAL_TIMEOUT( " slot ",\n"
	    "                      " tocl ",\n"
	    dur ",\n"
	    sig ");\n")
    (indent-region start (point) nil)
    (ch-indent-line)))

(defun ch-insert-global-symbol ()
  "Insert Symbol (using TAGS)"
  (interactive)
  (let ((name (ch-get-name "Symbol: "))
	(seize (y-or-n-p "Seize: ")))
    (insert name)
    (if seize
	(ch-insert-seize))))

(defun ch-receive-signal ()
  (interactive)
  (let ((sig (ch-get-name "Signal: ")) 
	(syn (read-string "Syn: " )) 
	(var (read-string "Variablen: ")) 
	(from (ch-get-name "From: "))
	(start (point)))
    (insert "/*")
    (insert "\n----------------------------------------------------------------")
    (insert "\n--  Signal: " sig)
    (insert "\n--")
    (if (not (string-equal syn ""))
	(insert "\n--     " syn))
    (insert "\n--")
    (if (not (string-equal from ""))
	  (if  (not (string-equal from "THIS"))
	      (insert "\n--  from: " from)))
    (insert "\n--")
    (insert "\n----------------------------------------------------------------")
    (insert "\n*/\n")
    (insert "  (" sig )
    (if (not (string-equal var ""))
	(insert " IN " var ))
    (insert "):\n" )
    (if (not (string-equal from ""))
	(progn
	  (if  (string-equal from "THIS")
	      (insert "\nIF ( vp_sender /= THIS )")
	    (insert "\nIF ( PROC_TYPE (vp_sender) /= " from " )"))
	  (insert "\nTHEN"
		  "\n;"
		  "\nFI;\n")))
    (insert "\n")
    (indent-region start (point) nil)
    (ch-indent-line)))

(defun ch-message-loop ()
  (interactive)
  (let ((start (point)))
    (insert "\nDCL vp_sender      INSTANCE;"
	    "\n\nDO FOR EVER;"
	    "\n\nl_receive:"
	    "\nRECEIVE CASE SET vp_sender;"
	    "\n\n\n/*"
	    "\n----------------------------------------------------------------"
	    "\n--"
	    "\n--   ELSE (RECEIVE Signal)"
	    "\n--"
	    "\n----------------------------------------------------------------"
	    "\n*/"
	    "\nELSE"
	    "\n;"
	    "\nESAC;       /*=== RECEIVE CASE ===*/"
	    "\n\nOD;         /*=== DO FOR EVER ===*/"
	    "\n")
    (indent-region start (point) nil)
    (ch-indent-line)))


(defun ch-insert-seize ()
  "Insert seize statement for TAGNAME (requires valid TAGS table)
uses ch-grt-file-ext as an extension for the grant file" 
  (interactive)
  (let* ((tagname (ch-get-name "Insert seize for: "))
	 (seize-text (concat "seize[ \t]*" tagname "[ ,;]")))  
    (save-window-excursion 
      (save-excursion
	(message "Looking for '%s' ..." tagname)
	(widen) 
	(goto-char (point-min))
	(if (search-forward-regexp seize-text nil t ) 
	    (message "'%s' already seized" tagname)
	  ;; not seized yet
	  (let* ((bn (save-excursion 
		       (find-tag (list tagname))
		       (ch-buffer-name-no-ext)))
		 (ust (concat "use_seize_file[ \t]*'" bn )))
	    (if (search-forward-regexp ust nil t) 
		(progn
		  (message "'%s' seized from '%s'" tagname bn)
		  (forward-line 1)
		  (insert "    SEIZE " tagname ";\n")
		  ) 
	      (progn 
		(message "'%s' seized from new file '%s'" tagname bn)
		(search-forward "use_seize_file" nil t) 
		(beginning-of-line)
		(insert "<> USE_SEIZE_FILE '" bn ch-grt-file-ext "' <>\n" 
			 "    SEIZE " tagname ";\n\n")))))))))

;;;
;;; change keywords
;;;
;;; from pascal mode

(defun chill-downcase-keywords ()
  "Downcase all Chill keywords in the buffer."
  (interactive)
  (chill-change-keywords 'downcase-word chill-keywords))

(defun chill-upcase-keywords ()
  "Upcase all Chill keywords in the buffer."
  (interactive)
  (chill-change-keywords 'upcase-word chill-keywords))

(defun chill-capitalize-keywords ()
  "Capitalize all Chill keywords in the buffer."
  (interactive)
  (chill-change-keywords 'capitalize-word chill-keywords))

(defun chill-downcase-types ()
  "Downcase all Chill types in the buffer."
  (interactive)
  (chill-change-keywords 'downcase-word chill-types))

(defun chill-upcase-types ()
  "Upcase all Chill types in the buffer."
  (interactive)
  (chill-change-keywords 'upcase-word chill-types))

(defun chill-capitalize-types ()
  "Capitalize all Chill types in the buffer."
  (interactive)
  (chill-change-keywords 'capitalize-word chill-types))

;; Change the keywords according to argument.
(defun chill-change-keywords (change-word expr)
  (save-excursion
    (let ((keyword-re (concat "\\S_\\<\\("
			      (mapconcat 'identity expr "\\|")
			      "\\)\\>\\S_")))
      (goto-char (point-min))
      (while (re-search-forward keyword-re nil t)
	(progn
	  (funcall change-word -1)
	  (backward-char 1))))))

;;;
;;; code copied from Martin.Davies@alcatel.be
;;;
;;; KNOWN BUGS / BUGREPORTS
;;; =======================
;;; As far as I know, there are no bugs in the current version of this
;;; package.  This may not be true however, since I never use this mode
;;; myself and therefore would never notice them anyway.   If you do
;;; find any bugs, you may submit them to: Martin.Davies@alcatel.be

;;; Code:

;;;
;;; Completion
;;;
(defvar chill-buffer-to-use nil)
(defvar chill-flag nil)
(defvar chill-all nil)
(defvar chill-str nil)

(defun chill-string-diff (str1 str2)
  "Return index of first letter where STR1 and STR2 differs."
  (catch 'done
    (let ((diff 0))
      (while t
	(if (or (> (1+ diff) (length str1))
		(> (1+ diff) (length str2)))
	    (throw 'done diff))
	(or (equal (aref str1 diff) (aref str2 diff))
	    (throw 'done diff))
	(setq diff (1+ diff))))))

(defun chill-get-default-symbol ()
  "Return symbol around current point as a string."
  (save-excursion
    (buffer-substring (progn
			(skip-chars-backward " \t")
			(skip-chars-backward "a-zA-Z0-9_")
			(point))
		      (progn
			(skip-chars-forward "a-zA-Z0-9_")
			(point)))))

(defun chill-completion-response ()
  (cond ((or (equal chill-flag 'lambda) (null chill-flag))
	 ;; This was not called by all-completions
	 (if (null chill-all)
	     ;; Return nil if there was no matching label
	     nil
	   ;; Get longest string common in the labels
	   (let* ((elm (cdr chill-all))
		  (match (car chill-all))
		  (min (length match))
		  tmp)
	     (if (string= match chill-str)
		 ;; Return t if first match was an exact match
		 (setq match t)
	       (while (not (null elm))
		 ;; Find longest common string
		 (if (< (setq tmp (chill-string-diff match (car elm))) min)
		     (progn
		       (setq min tmp)
		       (setq match (substring match 0 min))))
		 ;; Terminate with match=t if this is an exact match
		 (if (string= (car elm) chill-str)
		     (progn
		       (setq match t)
		       (setq elm nil))
		   (setq elm (cdr elm)))))
	     ;; If this is a test just for exact match, return nil ot t
	     (if (and (equal chill-flag 'lambda) (not (equal match 't)))
		 nil
	       match))))
	;; If flag is t, this was called by all-completions. Return
	;; list of all possible completions
	(chill-flag
	 chill-all)))

;;;
;;; PROC support.
;;;
(defun chill-build-PROC-re (str &optional arg)
  "Return PROC starting with STR as regular expression.
With optional second arg non-nil, STR is the complete name of the instruction."
  (if arg
      (concat "^[ \t]*\\(" str "\\)\\>[ \t]*:[ \t]*\\<\\(MODULE\\|PROCESS\\|PROC\\)\\>")
    (concat "^[ \t]*\\(" str "[a-zA-Z0-9_]*\\)\\>[ \t]*:[ \t]*\\<\\(MODULE\\|PROCESS\\|PROC\\)\\>")
    )
)

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on any function name. If
;; predicate is non-nil, it must be a function to be called for every
;; match to check if this should really be a match. If flag is t, the
;; function returns a list of all possible completions. If it is nil
;; it returns a string, the longest possible completion, or t if STR
;; is an exact match. If flag is 'lambda, the function returns t if
;; STR is an exact match, nil otherwise.

(defun chill-comp-PROC (chill-str chill-pred chill-flag)
  (save-excursion
    (let ((chill-all nil)
	  match)

      ;; Set buffer to use for searching labels. This should be set
      ;; within functins which use chill-completions
      (set-buffer chill-buffer-to-use)

      (let ((chill-str chill-str))
	;; Build regular expression for functions
	(if (string= chill-str "")
	    (setq chill-str (chill-build-PROC-re "[a-zA-Z_]"))
	  (setq chill-str (chill-build-PROC-re chill-str)))
	(goto-char (point-min))
      
	;; Build a list of all possible completions
	(while (re-search-forward chill-str nil t)
	  (setq match (buffer-substring (match-beginning 1) (match-end 1)))
	  (if (or (null chill-pred)
		  (funcall chill-pred match))
	      (setq chill-all (cons match chill-all)))))

      ;; Now we have built a list of all matches. Give response to caller
      (chill-completion-response))))

(defun chill-goto-PROC ()
  "Move to specified Chill function/procedure.
The default is a name found in the buffer around point."
  (interactive)
  (let* ((default (chill-get-default-symbol))
	 ;; The following variable is used in chill-comp-function
	 (chill-buffer-to-use (current-buffer))
	 (default (if (chill-comp-PROC default nil 'lambda)
		      default ""))
	 (label (if (not (string= default ""))
		    ;; Do completion with default
		    (completing-read (concat "Name: (default " default ") ")
				     'chill-comp-PROC nil t "")
		  ;; There is no default value. Complete without it
		  (completing-read "Name: "
				   'chill-comp-PROC nil t "")))
	 )
    ;; If there was no response on prompt, use default value
    (if (string= label "")
	(setq label default))
    ;; Goto right place in buffer if label is not an empty string
    (or (string= label "")
	(progn
	  (goto-char (point-min))
	  (re-search-forward (chill-build-PROC-re label t))
	  (beginning-of-line)))
    ))

;;;
;;; DCL support.
;;;
(defun chill-build-DCL-re (str &optional arg)
  "Return DCL starting with STR as regular expression.
With optional second arg non-nil, STR is the complete name of the instruction."
  (if arg
      (concat "^[ \t]*\\(DCL\\|SYN\\|SYNMODE\\|NEWMODE\\)[ \t]+\\(" str "\\)\\>")
    (concat "^[ \t]*\\(DCL\\|SYN\\|SYNMODE\\|NEWMODE\\)[ \t]+\\(" str "[a-zA-Z0-9_]*\\)\\>")
    )
)

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on any function name. If
;; predicate is non-nil, it must be a function to be called for every
;; match to check if this should really be a match. If flag is t, the
;; function returns a list of all possible completions. If it is nil
;; it returns a string, the longest possible completion, or t if STR
;; is an exact match. If flag is 'lambda, the function returns t if
;; STR is an exact match, nil otherwise.

(defun chill-comp-DCL (chill-str chill-pred chill-flag)
  (save-excursion
    (let ((chill-all nil)
	  match)

      ;; Set buffer to use for searching labels. This should be set
      ;; within functins which use chill-completions
      (set-buffer chill-buffer-to-use)

      (let ((chill-str chill-str))
	;; Build regular expression for functions
	(if (string= chill-str "")
	    (setq chill-str (chill-build-DCL-re "[a-zA-Z_]"))
	  (setq chill-str (chill-build-DCL-re chill-str)))
	(goto-char (point-min))
      
	;; Build a list of all possible completions
	(while (re-search-forward chill-str nil t)
	  (setq match (buffer-substring (match-beginning 2) (match-end 2)))
	  (if (or (null chill-pred)
		  (funcall chill-pred match))
	      (setq chill-all (cons match chill-all)))))

      ;; Now we have built a list of all matches. Give response to caller
      (chill-completion-response))))

(defun chill-goto-DCL ()
  "Move to specified Chill function/procedure.
The default is a name found in the buffer around point."
  (interactive)
  (let* ((default (chill-get-default-symbol))
	 ;; The following variable is used in chill-comp-function
	 (chill-buffer-to-use (current-buffer))
	 (default (if (chill-comp-DCL default nil 'lambda)
		      default ""))
	 (label (if (not (string= default ""))
		    ;; Do completion with default
		    (completing-read (concat "Name: (default " default ") ")
				     'chill-comp-DCL nil t "")
		  ;; There is no default value. Complete without it
		  (completing-read "Name: "
				   'chill-comp-DCL nil t "")))
	 )
    ;; If there was no response on prompt, use default value
    (if (string= label "")
	(setq label default))
    ;; Goto right place in buffer if label is not an empty string
    (or (string= label "")
	(progn
	  (re-search-backward (chill-build-DCL-re label t))
	  (beginning-of-line)))
    ))

;;; 
;;; other useful stuff
;;;

(defun start-gdb-other-window ()
"start gdb in other window"
(interactive)
(let ((name (ch-buffer-name-no-ext)))
  (delete-other-windows)
  (split-window-vertically)
  (other-window 1)
  (gdb name)))

(defun start-gdb-other-window-and-run ()
"start gdb in other window and run to current line"
(interactive)
(start-gdb-other-window)
(other-window 1)
(gdb-break nil)
(other-window 1)
(insert "run")
(comint-send-input))

;;;
;;; set up auto mode list 
;;;
(setq auto-mode-alist (cons '("\\.ch$" . chill-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.chill$" . chill-mode) auto-mode-alist))

;;;
;;; finally done
;;;
(provide 'chill-mode)

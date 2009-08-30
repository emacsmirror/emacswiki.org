;;; netrexx-mode.el --- highlight and indent Netrexx program files.

;; Author Arjan Bos <Arjan.Bos@icu.nl>
;; Keywords: netrexx
;; Version:
(defconst netrexx-mode-version "2.0")

;; Since this file is the, completely rewritten, follow-up to the 
;; original netrexx-mode.el which was posted to gno.emacs.sources on
;; 18 Jun 2002, its version number is 2.0.

;;
;;	Copyright (C) 2003 Arjan Bos.
;;
;;	This file is NOT part of GNU Emacs (yet).
;;
;;
;; DISTRIBUTION

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;
;;
;; Comentary: 

;; After working roughly a year with an adapted REXX-mode, I decided
;; it was time for a dedicated netrexx-mode. Particularly, the
;; indentation engine was behond my simple elisp comprehension and it
;; had a few oddities that made it less than perfect. Taking the
;; mode-tutorial on emacs wiki as a starting point, I build this mode
;; from scratch.
;;

;; Inspiration:

;; The main inspiration for writing this file as it is came from the
;; mode-tutorial by Scott Andrew Borton ( http://two-wugs.net/scott/
;; ), which I found on emacs-wiki.
;; The idea for the command netrexx-select-current-block was taken
;; from rexx-mode.el by Anders Lindgren / James Perrin.  
;; The idea for a NetRexx pull-down menu and how to implement it was
;; taken straight from fortran.el.  Stefan Monier helped when I was
;; stuck by answering some questions on news://gnu.emacs.help

;;
;; Usage:
;;	This file contains code for a GNU Emacs major mode for
;;	editing NETREXX program files.
;;
;;     Type C-h m in Emacs for information on how to configurate
;;	the netrexx-mode.
;;
;;	Put the following lines into your .emacs and rexx-mode will be
;;	automatically loaded when editing a NETREXX program.  If
;;	netrexx-mode shall be used for files with other extensions you
;;	can create more (cons ...) lines with these extensions.
;;
;;	(autoload 'netrexx-mode "netrexx-mode" "NETREXX mode" nil t)
;;	(setq auto-mode-alist
;;	      (append
;;	       (list (cons "\\.nrx$"  'netrexx-mode)
;;		     (cons "\\.nry$"  'netrexx-mode)
;;	       	     )
;;	       auto-mode-alist))
;;
;; To have netrexx-mode indent two columns a time your new lines as
;; you type them, enter the following in your .emacs
;; (setq netrexx-mode-hook '(lambda ()
;; 			(setq netrexx-indent-amount 2)
;; 			(local-set-key "\C-m" 'netrexx-indent-newline-indent)
;; 			))
;;
;; Netrexx mode can automatically insert a little comment after the
;; keyword "end" indicating what it is ending. To do this, replace
;; 'netrexx-indent-newline-indent 
;; by
;; 'netrexx-indent-newline-indent-with-end-comment
;;
;; Functions that should make live a bit easier:
;;
;; M-x netrexx-sanitize-region
;;   To make sure that there are no unintentional "trace results" or
;;   "trace methods" statements in your cvs check in, select a region
;;   and use the commant M-x netrexx-sanitize-region on it. This will
;;   also change all white lines by a single one, and it will remove
;;   all trailing whitespace.
;;
;; M-x netrexx-select-current-block
;;   This will select all lines of the "block" point is in. A block is
;;   defined to start either with "do", "loop" or "select" and end
;;   with "end", or defined to be the current method when no
;;   surrounding "do", "loop", "select" with "end" is found.  For this
;;   command, the comments right before a method are considered to be
;;   part of that method.
;;
;; A note on indentation:
;; Netrexx has a human oriented syntax, meaning basically "anything
;; goes" for the lay-out. To get the fullest of the indentation
;; engine, it makes a few assumptions about the lay-out of
;; source-code:
;; Statements like "if", "else", "end", "loop" and "catch" should always 
;; be found as the first text on a line.

;; Bugs / To do: 
;; 1. When using continued lines in combination with
;;    statements like "then do", the indentation is wrong. 
;; 2. Other combinations of continued lines might prove to be wrong
;;    too.
;; 3. Auto-fill mode does not work. However, filling of comments with
;;    M-q works.
;; 4. M-q does not split coding lines at appropriate places with a
;;    continuation character. This is something that's low on my to
;;    do list.
;; 5. netrexx-insert-javadoc-for-method goes into a loop if a
;;    parameter name ends with more than one underscore (_).

;; HISTORY
;;     14-09-03 V1.0 AB         First version. Inspired by the ModeTutorial
;;                              on emacs wiki. Version number is 2.0, since
;;                              it replaces another netrexx-mode.

(defgroup netrexx nil
  "Groups together all customization possiblities for netrexx-mode."
  :group 'languages)

(defcustom netrexx-indent-amount 2
  "*This variable contains the indentation in netrexx-mode."
  :group 'netrexx
  :type 'number)

(defcustom netrexx-end-comment-treshold 5
  "*Number of lines to search backward before an end comment is included.

For example, a value of 5 means that if the matching \"do\", \"loop\",
or \"select\" statement is 5 or more lines backward, an end-comment
gets included when either \\[netrexx-insert-end-comment] or
\\[netrexx-indent-newline-indent-with-end-comment] is called."
  :group 'netrexx
  :type 'number)

(defcustom netrexx-beep-annoyingly t
  "* If true, then beep on netrexx syntax errors.

Currently, it beeps on unmatched \"end\" and \"else\" statements
and when the point is past the last \"method\" statement and
\\[netrexx-next-method] is evaluated, or when the point is before
the first \"method\" statement and \\[netrexx-previous-method] is
evaluated."
  :group 'netrexx
  :type 'boolean)

(defcustom netrexx-auto-insert-javadoc nil
  "If true, then javadoc skeletons are inserted when a method is created.
When this variable is true, then finishing a \"method\"
statement (by pressing the enter-key) will automatically insert a
javadoc skeleton above the method."
  :group 'netrexx
  :type 'boolean)

(defcustom netrexx-package-path ""
  "Expanded path to the root of the package currently being edited.
This is used by \\[netrexx-initial-template] to re-model the
directory-name of the current file into a package statement.

It will change:
<netrexx-package-path>com/abnamro/midms/server/ 

to:
package com.abnamro.midms.server

It will need a trailing / or \\, depending on the filesystem,
 to make this work correctly."
  :group 'netrexx
  :type  'directory)


(defvar netrexx-mode-hook nil)
(defvar netrexx-mode-map nil
  "Keymap for Netrexx major mode")

(if netrexx-mode-map
    nil
  (progn
    (setq netrexx-mode-map (make-keymap))
    (define-key netrexx-mode-map "\t"   'netrexx-indent-line)
    (define-key netrexx-mode-map "\177" 'backward-delete-char-untabify)
    (define-key netrexx-mode-map "\C-c\C-p" 'netrexx-previous-method)
    (define-key netrexx-mode-map "\C-c\C-n" 'netrexx-next-method)
    (define-key netrexx-mode-map "\C-\M-a" 'netrexx-beginning-of-method)
    (define-key netrexx-mode-map "\C-\M-e" 'netrexx-end-of-method)

    (easy-menu-define netrexx-menu netrexx-mode-map "Menu for NetRexx mode."
      `("NetRexx"
	["Next Method\t\tC-c C-n" (netrexx-next-method)]
	["Previous Method\tC-c C-p" (netrexx-previous-method)]
	["Select Block" (netrexx-select-current-block)]
	["Sanitize Region" (netrexx-sanitize-region (region-beginning) (region-end))]
	["Insert End Comment" (netrexx-insert-end-comment)]
	["End Comments Region" (netrexx-insert-end-comment-region (region-beginning) (region-end))]
	["Insert javadoc" (netrexx-insert-javadoc-for-method)]
	"--"
	["Customize" (customize-group 'netrexx)]
	["Version" (netrexx-version)]
	))))

;; (setq netrexx-mode-map nil)

;; font-lock patterns
;; (defvar netrexx-font-lock-keywords nil
;;  "Expressions to highlight in V code mode")

(defvar netrexx-font-lock-keywords-1 nil
 "Level 1 expressions to highlight in V code mode")

(defvar netrexx-font-lock-keywords-2 nil
 "Level 2 expressions to highlight in V code mode")

(defvar netrexx-font-lock-keywords-3 nil
 "Level 3 expressions to highlight in V code mode")

(defcustom font-lock-netrexx-method-face 'font-lock-netrexx-method-face
  "*Specify face used to color the rexx provided method calls."
  :type 'face
  :group 'faces
  :group 'netrexx)

(defface font-lock-netrexx-method-face
  '((((class color) (background light)) (:bold t :foreground "blue")))
    "Face used to color the netrexx provided method calls.")

(defcustom font-lock-method-face 'font-lock-method-face
  "*Specify face used to color the method calls."
  :type 'face
  :group 'faces
  :group 'netrexx)

(defface font-lock-method-face
  '((((class color) (background light)) (:foreground "dark blue")))
    "Face used to color the method calls.")

;; Level 1 - comments and strings
(setq netrexx-font-lock-keywords-1 
      (list
       '("\\<\\(a\\(bstract\\|dapter\\)\\|b\\(inary\\|y\\)\\|c\\(ase\\|atch\\|lass\\|onstant\\)\\|d\\(ep\\(endent\\|recated\\)\\|igits\\|o\\)\\|e\\(lse\\|n\\(d\\|gineering\\)\\|x\\(it\\|tends\\)\\)\\|f\\(inal\\(\\|ly\\)\\|or\\(\\|\\(ever\\|m\\(\\|word\\)\\)\\)\\)\\|i\\(f\\|mp\\(lements\\|ort\\)\\|n\\(direct\\|heritable\\|terface\\)\\|terate\\)\\|l\\(abel\\|eave\\|oop\\)\\|m\\(ethod\\)\\|n\\(ative\\|op\\|umeric\\)\\|o\\(ptions\\|therwise\\|ver\\)\\|p\\(a\\(ckage\\|r\\(ent\\|se\\)\\)\\|r\\(ivate\\|o\\(perties\\|tect\\)\\)\\|ublic\\)\\|[Rr]\\(e\\(turn\\(\\|s\\)\\|set\\|xx\\)\\)\\|s\\(ay\\|cientific\\|e\\(t\\(digits\\|form\\)\\|lect\\)\\|hared\\|ignal\\(\\|s\\)\\|ourceline\\|tatic\\|uper\\)\\|t\\(h\\(en\\|is\\)\\|o\\|ra\\(ce\\|nsient\\)\\)\\|u\\(n\\(til\\|used\\)\\|pper\\)\\|v\\(olatile\\)\\|w\\(h\\(en\\|ile\\)\\)\\)\\>" 1 font-lock-keyword-face nil)
       '("\\.\\(a\\(b\\(brev\\|s\\)\\|ddlib\\)\\|b\\(2x\\)\\|c\\(2\\(d\\|x\\)\\|ent\\(re\\|er\\)\\|ha\\(ngestr\\|rat\\)\\|lose\\|o\\(mpare\\|p\\(ies\\|yindexed\\)\\|untstr\\)\\)\\|d\\(2\\(c\\|x\\)\\|at\\(atype\\|e\\)\\|el\\(str\\|word\\)\\)\\|e\\(quals\\|xists\\)\\|f\\(ormat\\)\\|h\\(ashcode\\)\\|i\\(nsert\\)\\|l\\(astpos\\|e\\(ft\\|ngth\\)\\|ower\\)\\|m\\(ax\\|in\\)\\|o\\(p\\(a\\(dd\\|nd\\)\\|cc\\|ccblank\\|div\\|divl\\|eq\\|eqs\\|gt\\|gt\\(eq\\|eqs\\|s\\)\\|lt\\|lt\\(eq\\|eqs\\|s\\)\\|m\\(inus\\|ult\\)\\|not\\|not\\(eq\\|eqs\\)\\|or\\|p\\(lus\\|ow\\)\\|rem\\|sub\\|xor\\)verlay\\)\\|p\\(os\\)\\|r\\(everse\\|ight\\)\\|s\\(equence\\|ign\\|pace\\|trip\\|ub\\(str\\|word\\)\\)\\|t\\(o\\(b\\(oolean\\|yte\\)\\|char\\|chararray\\|double\\|float\\|int\\|long\\|short\\|string\\)\\|r\\(anslate\\|unc\\)\\)\\|u\\(pper\\)\\|v\\(erify\\)\\|w\\(ord\\(\\|index\\|lengh\\|pos\\|s\\)\\|rite\\(ch\\|ln\\)\\)\\|x\\(2\\(b\\|c\\|d\\)\\)\\)\\>" 1 font-lock-netrexx-method-face nil)
    '( "\\.\\([a-zA-Z0-9_]+\\)(" 1 font-lock-method-face nil)
    ))
  
;; Level 3 -  ports
(setq netrexx-font-lock-keywords-3
  (append
    netrexx-font-lock-keywords-1
    (list
      ;; class statement
      (list
       "class *\\(\\<\\w*\\>\\)" '(1 font-lock-variable-name-face nil))
      ;; exit statement
      (list
       "exit \\(\\<.*\\>\\)" '(1 font-lock-variable-name-face nil))
      ;; extends keyword
      (list
       "extends \\(\\<.*\\>\\)" '(1 font-lock-variable-name-face nil))
      (list
       "extends \\(\\<.*\\>\\) implements \\(\\<.*\\>\\)" '(1 font-lock-variable-name-face nil))
      ;; implements keyword
      (list
       "\\(extends \\(\\<.*\\>\\)\\)* implements \\(\\<.*\\>\\)" '(3 font-lock-variable-name-face nil))
      ;; import statements
      (list
       "import \\(\\<.*\\>\\)" '(1 font-lock-constant-face nil))
      ;; user function names
      (list
       "method \\(\\<.*\\>\\)(" '(1 font-lock-function-name-face nil))
      ;; options statement (note: the binary and the trace keyword clash with the ones 
      ;; in the first list, which is needed for the class keyword.
      (list
       (concat
	"options \\(\\( ?\\|\\<\\(no\\)?"
	"\\(binary\\|"
	"c\\(o\\(m\\(ments\\|pact\\)\\|nsole\\)\\|rossref\\)\\|"
	"d\\(ecimal\\|iag\\)\\|"
	"explicit\\|"
	"java\\|"
	"format\\|"
	"logo\\|"
	"replace\\|"
	"s\\(avelog\\|ourcedir\\|trict\\(args\\|assign\\|case\\|import\\|props\\|signal\\)\\)\\|"
        "trace\\(\\|1\\|2\\)\\|"
	"utf8\\|"
	"verbose\\(\\|0\\|1\\|2\\|3\\|4\\|5\\)"
	"\\)\\>\\)+\\)")
       '(1 font-lock-variable-name-face nil))
      ;; package statement
      (list
       "\\<package \\(\\<.*\\>\\)" '(1 font-lock-constant-face nil))
      ;; returns statement
      (list
       "return[s]? \\(this\\.\\)?\\(\\<.*\\>\\)" '(2 font-lock-variable-name-face nil))
      ;; signals keyword
      (list
       "signals \\(\\<.*\\>\\)" '(1 font-lock-variable-name-face nil))
      ;; trace options
      (list
       "trace \\(\\<all\\|methods\\|off\\|results\\>\\)" '(1 font-lock-variable-name-face nil))
      )
    )
  )


(defvar netrexx-font-lock-keywords netrexx-font-lock-keywords-3
  "Default highlighting expressions for Netrexx mode")

(defun netrexx-indent-line ()
  "Indent the current line as NETREXX code.
The following rules apply:

 0- All keywords are matched from the beginning of the line.

 1- If we are at the beginning of the buffer, indent to column 0.

 2- If we see the keyword \"class\" without the keyword
    \"dependent\", then indent to column 0.

 3- If we see the keyword \"class\" with the keyword
    \"dependent\", then indent to `netrexx-indent-amount'.

 4- If we see the keyword \"method\" then indent to
    `netrexx-indent-amount'.

 5- If we see the keyword \"when\" or \"otherwise\", the indent
    amount is relative to the matching \"select\" statement, plus
    `netrexx-indent-amount'.

 6- If we look at the statement \"else\", indent it to the same
     amount as the corresponding \"if\", taking nested ifs into
     account.

 7- If we see the statement \"catch\" or \"finally\", look for
    the matching \"do\" and set the indent amount to the same as
    that \"do\". Take nested blocks into account. See rule 11 for
    a definition of a block start.

 8- \"end\" should be matched to the corresponding \"do\",
    \"loop\" or \"select\".

 9- The first line of a multi-line comment should be indented
    like a normal line. When the second line of a multi-line
    comment starts with a \"*\", then align that \"*\" with the
    first \"*\" of the first line of the multi-line comment,
    otherwise indent it so that it starts two positions after the
    \"*\" of the first comment line. This will make sure that the
    comment-text is aligned in a correct way.

10- If the previous non-empty line contains the uncommented
    keyword \"then\", then indent the next line. The line after
    that should not be indented, except when it is part of a
    \"do\"-construct.

11- If the previous line is ended with the continuation character
    \"-\", then find the first line with that continuation
    character. Find the characther \"(\" on that line and set the
    indentation to the next column. If the \"(\" couldn't be
    found, then look for the \" character and indent to there.

12- If a line follows a \"block\"-start, increase the indentation
    with `netrexx-indent-amount'.  A \"block\"-start consists of
    one of the following keywords: \"catch\", \"class\", \"do\",
    \"then\", \"else\", \"loop\", \"method\", \"select\" or
    \"otherwise\".  This should consider nested \"if\" constructs
    and do the right thing.

13- If we first see an \"do\"-construct ending, the we should
    indent the current line to the same indentation as the
    \"do\"-construct ending. Except when the keyword \"end\"
    matches a keyword \"do\" which comes right after an \"else\",
    \"then\" or \"otherwise\".

14- If no indentation rule matches, then indent the same amount
    as the previous line.

15- Javadoc comments should be indented to the same amount as the
    class or method they belong to."
  (interactive)
  ;; save current position, relative to point-max
  (let ((pos (- (point-max) (point))))
    (beginning-of-line)
    (cond ((bobp)
	   (indent-line-to 0))
	  ((looking-at "^[ \t]*class\\([ \t]\\|$\\)+") ; check for rule 2 
	   (if (looking-at "^[ \t]*class .*? dependent") ;check for rule 3
	       (indent-line-to netrexx-indent-amount)
	     (indent-line-to 0)))
	  ((netrexx-looking-at-method-p) ; check for rule 4
	   (indent-line-to netrexx-indent-amount))
	  ((looking-at "^[ \t]*/\\*\\*") ; javadoc check for rule 15
	   (if (save-excursion
		 (< (save-excursion 
		      (let ((method-point (re-search-forward "^[ \t]*method\\b" nil t 1)))
			(if method-point
			    method-point
			  (point))))
		    (let ((class-point (re-search-forward "^[ \t]*class\\b" nil t 1)))
		      (if class-point
			  class-point
			(+ (point-max) 1)))))
		 ;; javadoc belongs to a method
		 (indent-line-to netrexx-indent-amount)
	       ;; javadoc belongs to a class
	       (if (looking-at "^[ \t]*class .*? dependent")
		   (indent-line-to netrexx-indent-amount)
		 (indent-line-to 0))))
	  (t 
	   (let ((not-indented t) cur-indent)
	     ;; check for rule 5
	     ;; when commands should be aligned underneath each other and
	     ;; and they should be indented relative to the select statement
	     (when (or (looking-at "^[ \t]*when\\b")
		       (looking-at "^[ \t]*otherwise\\b"))
	       (save-excursion
		 (let ((still-looking t))
		   (while still-looking
		     (forward-line -1)
		     (when (netrexx-looking-at-end-p) 
		       (netrexx-from-end-goto-matching-do))
		     (cond ((looking-at "^[ \t]*when\\b")
			   (setq still-looking nil
			     not-indented nil
			     cur-indent (current-indentation)))
			   ((looking-at "^[ \t]*select\\b")
			    (setq still-looking nil
				  not-indented nil
				  cur-indent (+ (current-indentation) netrexx-indent-amount))))))))
	     
	     ;; check for rule 6
	     ;; else should be aligned to the correct if, taking nested ifs 
	     ;; into account.
	     (when (and not-indented
		      (netrexx-looking-at-else-p))
		 (save-excursion
		   (if (netrexx-from-else-goto-matching-if)
		       (setq cur-indent (current-indentation))
		     (netrexx-beep)
		     (message "Dangling else!")
		     (setq cur-indent 0))
		   (setq not-indented nil)))
	     
	     ;; check for rule 7
	     ;; catch and finally should be aligned to their own do, 
	     ;; taking nesting into account
	     (when (and not-indented
			(looking-at "^[ \t]*\\(catch\\|finally\\)\\b"))
	       (save-excursion
		 (let ((still-looking t)
		       (nesting-level 0))
		   (while still-looking
		     (forward-line -1)
		     (when (and (not (netrexx-looking-at-comment-p))
				(or (bobp)
				    (looking-at "^[ \t]*\\(.*\\)?\\bthen do")
				    (looking-at "^[ \t]*do[ \t]*$")
				    (looking-at "^[ \t]*else do\\b")
				    (looking-at "^[ \t]*\\(select\\|loop\\)\\b")))
		       (setq still-looking nil
			     not-indented nil
			     cur-indent (current-indentation)))
		     (when (netrexx-looking-at-end-p)
		       (netrexx-from-end-goto-matching-do))
		     ))))
	     
	     (when (and not-indented  ; check for rule 8
			(netrexx-looking-at-end-p))
	       ;; find the matching do, select, loop or catch. It should 
	       ;; take nested do / end pairs into account.
	       (save-excursion
		 (setq not-indented nil
		       cur-indent (if (netrexx-from-end-goto-matching-do)
				      (current-indentation)
				    0))))
	     
	     ;; check for rule 9
	     ;; check to see if we are within a comment
	     (when (and not-indented 
			(netrexx-looking-at-comment-p))
	       (save-excursion
		 ;; check to see if we're looking at a single line, or
		 ;; a multi-line comment.
		 (if (looking-at "^[ \t]*--") ; looking at --
		     ;; align -- to the -- on the previous line, if any
		     (let ((cur-comment (current-column))
			   (prev-comment nil)
			   (bol-pos      nil)) ;; beginning-of-line point pos
		       (forward-line -1)
		       ;; find out if there is still a -- on this line
		       (if (looking-at "^[ \t]*.*--")
			   (progn
			     (re-search-forward "--" nil t 1)
			     (setq prev-comment (- (current-column) 2))
			     (when (> prev-comment cur-comment)
			       (setq cur-indent prev-comment
				     not-indented nil))))
;; 			 ;; else, single -- line, align to the
;; 			 ;; previous, non empty, non comment line.
;; 			 (while (or (netrexx-looking-at-comment-p)
;; 				    (looking-at "^[ \t]*$"))
;; 			   (forward-line -1))
;; 			 (setq cur-indent (current-indentation)
;; 			       not-indented nil)))
		       )
		     ;; else, not lookng at --
		     (when (not (looking-at "^[ \t]*/\\*")) ; looking at /*
		       (let (extra-indent)
			 (setq extra-indent 0)
			 (if (looking-at "^[ \t]*\\*") ; looking at *
			     (setq extra-indent 1)
			   (setq extra-indent 3))
			   
			 (while (not (or (looking-at "^[ \t]*/\\*")
					 (bobp)))
			   (forward-line -1))
			   (when (looking-at "^[ \t]*/\\*")
			     (setq cur-indent (+ (current-indentation ) extra-indent)
				   not-indented nil)))))))
	     
	     (when not-indented ; check for rule 10
	       (save-excursion
		 (forward-line -1)
		 (while (or (netrexx-looking-at-comment-p)
			    (save-excursion
			      (forward-line -1)
			      (looking-at "^[ \t]*.*[^-]-[ \t]*$"))) ;;continuation
		   (forward-line -1))
		 (when (and (not (looking-at "^[ \t].*\\(--\\|/\\*\\)[ \t]*\\(else\\|then\\)"))
			    (or (looking-at "^[ \t]*\\(.*\\)?\\bthen\\( do\\|[ \t]*\\(--.*\\|/\\*.*\\)?$\\)")
				(and (looking-at "^[ \t]*\\(if\\|when\\)\\b")
				     (not (looking-at "^.*then\\b.+$")))
				(looking-at "^[ \t]*else\\( do\\|[ \t]*$\\)"))
			    (not (netrexx-looking-at-comment-p)))
		   (when (not (looking-at "^[ \t]*.*-[ \t]*$"))
		     (setq cur-indent (+ (current-indentation) netrexx-indent-amount)
			   not-indented nil)))))

	     ;; continued lines should be aligned as much as possible
	     ;; rule 11
	     (when (and not-indented
			(save-excursion
			  (forward-line -1)
			  (looking-at "^[ \t]*.*[^-]-[ \t]*$"))) ;continuation character
	       (save-excursion
		 (forward-line -1)
		 (while (looking-at "^[ \t]*.*[^-]-[ \t]*$")
		   (forward-line -1))
		 (forward-line 1) ;; we went back one row to many
		 (beginning-of-line)
		 (let ((beg (point))
		       (end (save-excursion
			      (end-of-line)
			      (point))))
		   (cond ((looking-at "^[ \t]*\\(if\\|when\\)\\b")
			  ;; indent to the first character after the
			  ;; first word
			  (beginning-of-line)
			  (forward-word 1)
			  (while (looking-at "[ \t]")
			    (forward-char 1))
			  (setq cur-indent (current-column)))
			 ((re-search-forward "(" end t 1)
			  (setq cur-indent (current-column)))
			 ((progn 
			    (goto-char beg)
			    (re-search-forward "\"" end t 1))
			  (setq cur-indent (- (current-column) 1)))
			 (t
			  (setq cur-indent (+ (current-indentation) netrexx-indent-amount))))))
	       (setq not-indented nil))

	     (if not-indented ;check for rule 12 and 13
		 (progn 
		   (save-excursion
		     (forward-line -1)
		     (while (not (or (netrexx-looking-at-end-p)
				     (and (looking-at "^[ \t]*\\(c\\(atch\\|lass\\)\\|\\(do\\|.*?then\\( do\\)?\\)\\|\\(else\\( do\\)?\\)\\|gl\\.glBegin\\|loop\\|method\\|select\\|otherwise\\)\\b")
					  (not (netrexx-looking-at-comment-p)))
				     (bobp)))
		       (forward-line -1))
		     (let ((still-looking t)
			   (nesting-level 1))
		       (if (netrexx-looking-at-end-p) ; rule 13
			   ;; now look for a matching "do" that follows an
			   ;; if or else.
			   (save-excursion
			     (if (netrexx-from-end-goto-matching-do)
				 (save-excursion
				   (if (netrexx-previous-line-else-or-then-p)
				       (progn
					 (when (netrexx-looking-at-else-p)
					   (progn
					     (netrexx-from-else-goto-matching-if)
					     (while (netrexx-previous-line-else-or-then-p))))
					 (setq still-looking nil
					       not-indented nil
					       cur-indent (current-indentation)))
				     ;; else, previous line is no else or then
				     (setq still-looking nil
					   not-indented nil
					   cur-indent (current-indentation))))
			       ;; else, no matching do found
			       (setq still-looking nil
				     not-indented nil
				     cur-indent 0))))
		       (when still-looking
			 (cond ((looking-at "^[ \t]*\\(c\\(atch\\|lass\\)\\|\\(do\\|.*?then do\\)\\|\\(else do\\)\\|gl\\.glBegin\\|loop\\|method\\|select\\|otherwise\\)") ; rule 12
				(setq cur-indent (+ (current-indentation) netrexx-indent-amount)
				      not-indented nil))
			       ((netrexx-looking-at-else-p)
				(netrexx-from-else-goto-matching-if)
				(while (netrexx-previous-line-else-or-then-p))
				(setq cur-indent (current-indentation)
				      not-indented nil))
			       
			       ((looking-at "^[ \t]*.*?then")
				(while (netrexx-previous-line-else-or-then-p))
				;; indentation
				(forward-line -1)
				(while (looking-at "^[ \t]*.*[^-]-[ \t]*$") 
				  (forward-line -1))
				(forward-line 1)
				(if (looking-at "^[ \t]*\\(if\\|when\\).*[^-]-[ \t]*$") 
				  (setq cur-indent (+ (current-indentation) netrexx-indent-amount)
					not-indented nil)
				  ;; else
				  (setq cur-indent (current-indentation) 
				      not-indented nil) ))))))))
	     
	     (when not-indented ; check for rule 14
	       (save-excursion
		 (forward-line -1)
		 (setq cur-indent (current-indentation))))
	     
	     (when (< cur-indent 0)
	       (setq cur-indent 0))
	     (indent-line-to cur-indent)
	     )))
    (when (> (- (point-max) pos) (point))
      (goto-char (- (point-max) pos)))))
  
(defun netrexx-inside-comment-p ()
  "Checks if the point is inside a comment.  
It returns true if the point is inside it, else it returns nil."
  (let ((origpoint (point))
	state)
    (save-excursion
      (goto-char 1)
      (while (> origpoint (point))
	(setq state (parse-partial-sexp (point) origpoint 0))))
      (nth 4 state)))

(defun netrexx-inside-javadoc-p ()
  "Checks if the point is inside a javadoc style comment.  
It returns true if the point is inside it, otherwise it returns
nil."
  (let ((retval (netrexx-looking-at-comment-p)))
    (when retval ;; now check to see if we are within a javadoc style
		 ;; comment.  
      ;; first we need to check if we are within
      ;; a multi-line comment
      (save-excursion
	(beginning-of-line)
	(setq retval (netrexx-inside-comment-p))
	(when retval
	  ;; we are within a multi-line comment
	  (let ((still-looking t))
	    ;; find the start of the javadoc comments
	    (while still-looking
	      (forward-line -1)
	      (setq still-looking (not (looking-at "^[ \t]*/\\*\\(\\*\\)?"))))))
	;; Are we really looking at some javadoc style comment?
	(setq retval (looking-at "^[ \t]*/\\*\\*")))) ;; javadoc start
    retval))

(defun netrexx-looking-at-comment-p ()
  "Returns true if the current line contains a comment."
  (let ((retval (netrexx-inside-comment-p)))
    (if (not retval)
	(save-excursion
	  (beginning-of-line)
	  (setq retval (or (looking-at "^[ \t]*--")
			   (looking-at "^[ \t]*/\\*"))))
    retval)))
    
(defun netrexx-indent-newline-indent ()
  "Indents the current line before doing a regular newline-and-indent.
If point is at the end of the line, or at the beginning of an
emtpy line, then it only does a \\[new-line-indent]. Otherwise it
moves the point to the beginning of the first word on the new
line."
  (interactive)
  (save-excursion
    (when (and (netrexx-looking-at-comment-p)
	       (save-excursion
		 (beginning-of-line)
		 (and (looking-at "^[ \t]*\\(\\*\\|/\\*\\)")
		      (not (looking-at "^.*\\*/")))))
      (insert "* "))
    (netrexx-indent-line))
  (when (and netrexx-auto-insert-javadoc
	     (netrexx-looking-at-method-p))
    (netrexx-insert-javadoc-for-method))
  (if (or (eolp)
	  (looking-at "[ \t]*$")
	  (netrexx-looking-at-comment-p))
      (progn
	(newline-and-indent)
	(when (looking-at "\\*")
	  (forward-char 1))
	(when (looking-at "  ")
	  (delete-char 1))
	(when (looking-at " ")
	  (forward-char 1)))
    ;; else
    (newline-and-indent)))

(defun netrexx-indent-newline-indent-with-end-comment ()
  "Performs a \\[netrexx-indent-newline-indent], but before doing
that, it checks to see if the current line contains the \"end\"
statement. If that is the case, then the function
\\[netrexx-insert-end-comment] is executed.  This results in a
small comment behind the end, showing which statement it
matches."
  (interactive)
  (when (netrexx-looking-at-end-p)
    (save-excursion
      (netrexx-insert-end-comment))
    (end-of-line))
  (netrexx-indent-newline-indent))

(defun netrexx-looking-at-method-p ()
  "Returns t if the current line is the method statement"
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*method\\b")))

(defun netrexx-looking-at-end-p ()
  "Returns t if the current line matches the regexp \"^[ \t]*end\>\""
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*\\(end\\>\\|gl\\.glEnd()\\)")))

(defun netrexx-looking-at-do-p ()
  "Returns t if the current line contains the \"do\"-statement"
  (save-excursion
    (beginning-of-line)
    (or (looking-at "^[ \t]*do[ \t]*\\(\\(--\\|/\\*\\).*\\)?$")
	(looking-at "^[ \t]*.*?\\(else\\|then\\)[ \t]do[ \t]*$"))))

(defun netrexx-looking-at-else-or-then-p ()
  "Returns t if the current line contains \"else\", \"then\" or
\"otherwise\"."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*.*?\\(else\\|then\\|otherwise\\)\\b")))

(defun netrexx-looking-at-else-p ()
  "Returns t if the current line contains \"else\"."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*else\\b")))

(defun netrexx-previous-line-else-or-then-p ()
  "Returns t if the line before the current line matches
\\[netrexx-looking-at-else-or-then-p]. If that is true, the point
will be placed on that line, otherwise, the point won't be
moved."
  (let (retval)
    (save-excursion
      (forward-line -1)
      ;; disregard comments
      (while (netrexx-looking-at-comment-p)
	(forward-line -1)) 
      (setq retval (netrexx-looking-at-else-or-then-p)))
    (if retval
	(forward-line -1))
    retval))

(defun netrexx-beep ()
  "Beeps when `netrexx-beep-annoyingly' is not nil."
  (when netrexx-beep-annoyingly
    (ding)))

(defun netrexx-looking-at-block-begin-p ()
  "Returns t if the current line contains the \"do\", \"loop\" or
\"select\" statement."
(save-excursion
  (beginning-of-line)
  (or (netrexx-looking-at-do-p)
      (looking-at "^[ \t]*gl\\.glBegin")
      (looking-at "^[ \t]*\\(select\\|loop\\)\\b"))))

(defun netrexx-from-end-goto-matching-do ()
  "Finds the \"do\" matching the current \"end\".
Starts from a line. Returns nil if the current line isn't an
\"end\" statement. If, however, it is an end statement, it moves
point to the line that contains the \"do\", \"loop\" or
\"select\" statement that the starting end closes. It returns the
number of lines searched backwards if it finds such a
statement. If it encounters the beginning of the buffer, it will
return nil and the cursor will be there, at the beginning of the
buffer."
  ;; find the matching do, select, loop or catch. It should take
  ;; nested do / end pairs into account.
  (let ( (still-looking (netrexx-looking-at-end-p))
	 (nesting-level 0)
	 (retval (if (netrexx-looking-at-end-p)
		     0
		   nil )) )
    (setq nesting-level 0)
    (while still-looking
      (forward-line -1)
      (setq retval (+ retval 1))
      ;; we started from "end", so if we encounter another end, bump
      ;; up type nesting-level
      (if (netrexx-looking-at-end-p)
	  (setq nesting-level (+ nesting-level 1))
	;; else
	(when (and (netrexx-looking-at-block-begin-p)
		   (not (netrexx-looking-at-comment-p)))
	  (if (eq nesting-level 0)
	      (setq still-looking nil)
	    ;; else
	    (setq nesting-level (- nesting-level 1)))))
      ;; test to make sure we don't run away past the
      ;; beginning of the buffer
      (when (and (or (bobp)
		     (netrexx-looking-at-method-p))
		 still-looking)
	(setq still-looking nil
	      retval nil)))
    retval))

(defun netrexx-from-else-goto-matching-if ()
  "Finds the \"if\" matching the current \"else\".
Starts from a line, returns nil if the current line isn't an
\"else\" statement. If, however it is an else statement, it moves
point to the line that contains the corresponding \"if\". It
returns t if it finds such a statement. If it encounters the
begin of the buffer, it will return nil and the cursor will be
there, at the beginning of the buffer."
  (let ((still-looking t) 
	(else-count 0)
	(retval nil))
    (while still-looking
      (if (or (bobp)
	      (and (looking-at "^[ \t]*if\\b")
		   (eq else-count 0)))
	  (setq still-looking nil
		;; return nil if we are at the beginning of buffer
		retval (not (bobp)))
	(when (and (looking-at "^[ \t]*if\\b")
		   (> else-count 0))
	  (setq else-count (- else-count 1)))
	(forward-line -1)
	(when (netrexx-looking-at-end-p) ; find the matching do
	  (netrexx-from-end-goto-matching-do))
	(when (looking-at"^[ \t]*else\\b")
	  (setq else-count (+ else-count 1)))))
    retval))

(defun netrexx-return-word ()
  "Returns the first word it encounters."
  (let ((beg) (end) (retval))
    ;; move to the beginning of the word
    (forward-word 1) 
    (backward-word 1)
    ;; determine the word boundries
    (setq beg (point))
    (forward-word 1)
    (setq end (point))
    ;; copy the word into save-text
    (setq retval (buffer-substring-no-properties beg end))
    ;; underscores are part of the word
    (when (looking-at "_")
      (setq retval (concat retval "_"))
      (forward-char 1)
      (when (looking-at "[a-zA-Z0-9_]")
	(setq retval (concat retval (netrexx-return-word)))))
    retval))

(defun netrexx-insert-end-comment ()
  "inserts a comment right after an netrexx \"end\" statement
that shows which \"do\", \"loop\" or \"select\" it matches."
  (interactive)
  (if (netrexx-looking-at-end-p)
      (let ((save-text) (lines-searched))
	(save-excursion
	  (setq lines-searched (netrexx-from-end-goto-matching-do))
	  (when (and lines-searched ;; make sure it's not nil
		     (> lines-searched netrexx-end-comment-treshold))
	    (when (netrexx-looking-at-block-begin-p)
	      (setq save-text (netrexx-return-word)))
	    (when (string= save-text "do")
	      (forward-line -1)
	      (when (netrexx-looking-at-else-or-then-p)
		(setq save-text (concat (netrexx-return-word) " " save-text)))
	      )))
	;; was there something put into save-text?
	(if save-text
	    (progn
	      (beginning-of-line)
	      (forward-word 1)
	      ;; no double end comments that are equal
	      (if (looking-at (concat " -- " save-text))
		  (progn
		    (end-of-line)
		    (message "End comment \" -- %s\" is already in place" save-text))
		;;else
		(end-of-line)
		(insert " -- " save-text)))
	  ;; else
	  (when (not lines-searched)
	    (netrexx-beep)
	    (message "No match found for current \"end\"-statement")
	    (end-of-line)
	    (insert "-- Unmatched end!"))))
    (message "Not on an line with an \"end\"-statement")) )

(defun netrexx-insert-end-comment-region (beg end )
  "Every end-statement in the region that matches
\\[netrexx-looking-at-end-p] will get an end comment.

See also \\[netrexx-insert-end-comment]."
  (interactive "*r")
  (when (> beg end) 
    (let (mid) 
      (setq mid beg 
	    beg end 
	    end mid)))
  (goto-char beg)
  (beginning-of-line)
  (while (< (point) end)
    (when (netrexx-looking-at-end-p)
      (netrexx-insert-end-comment))
    (forward-line)))

(defun netrexx-sanitize-region ( beg end )
  "Removes double empty lines and trailing whitespaces and will
comment out all \"trace results\" and \"trace methods\"
statements that are not part of an \"if\" statement. All other
lines are indented with \\[netrexx-indent-line].

All blank lines between a multi-line comment and a method are
removed."
  (interactive "r")
  (when (> beg end) 
    (let (mid) 
      (setq mid beg 
	    beg end 
	    end mid)))
  (goto-char beg)
  (beginning-of-line)
  (while (and (< (point) end) 
	     (not (eobp)))
    (when (looking-at "[ \t]*$")
      (save-excursion
	(forward-line 1)
	(when (looking-at "[ \t]*$")
	  (delete-blank-lines))))
    (when (looking-at "[ \t]*trace \\(results\\|methods\\)")
      (let ((i 0)
	    (found-if nil))
	;;look back only 5 rows
	(save-excursion
	  (while (< i 4)
	    (forward-line -1)
	    (if (netrexx-looking-at-end-p)
		(setq found-if nil
		      i 4)
	      (if (looking-at "[ \t]*if\\b")
		  (setq found-if t
			i 4)
		(setq i (+ i 1))))))
	(when (not found-if)
	  (forward-word 1)
	  (backward-word 1)
	  (insert "-- "))))
    (when (or (netrexx-looking-at-method-p)
	      (looking-at "^[ \t]*class\\b"))
      ;; see if there is a blank line above separating a multi-line comment 
      ;; and the current line.
      (save-excursion
	(when (and (save-excursion
		     (forward-line -1)
		     (looking-at "^[ \t]*$"))
		   (progn
		     (forward-line -1)
		     (while (looking-at "^[ \t]*$")
		       (forward-line -1))
		     (or (netrexx-inside-comment-p)
			 (looking-at "^[ \t]*/\\*"))))
	  (delete-blank-lines))))
    (netrexx-indent-line)    
    (forward-line 1)
    )
  (delete-trailing-whitespace))

;; used by `netrexx-next-method' and `netrexx-previous-method'
(defvar netrexx-boundry-hit nil)

(defun netrexx-next-method ()
  "Jumps to the next method definition."
  (interactive)
  (setq netrexx-boundry-hit (and (eq last-command 'netrexx-next-method)
				 netrexx-boundry-hit))
  ;; should we look forward once or twice?
  (let ((i (if (netrexx-looking-at-method-p) 2 1)))
    (if (re-search-forward "^[ \t]*method\\b" nil t i)
	(progn
	  (beginning-of-line)
	  (setq netrexx-boundry-hit nil))
      (netrexx-beep)
      (if netrexx-boundry-hit
	  (progn
	    (message "Wrapping to beginning of buffer...")
	    (goto-char (point-min))
	    (if (re-search-forward "^[ \t]*method\\b" nil t 1)
		(beginning-of-line)
	      (message "No method available in buffer")
	      (netrexx-beep))
	    (setq netrexx-boundry-hit nil))
	(message "End of buffer...")
	(setq netrexx-boundry-hit t))))
  (setq last-command 'netrexx-next-method))
  
(defun netrexx-previous-method ()
  "Jumps to the previous method definition."
  (interactive)
  (setq netrexx-boundry-hit (and (eq last-command 'netrexx-previous-method)
				 netrexx-boundry-hit))
  ;; should we look backward once or twice?
  (let ((i  1))
    (if (re-search-backward "^[ \t]*method\\b" nil t i)
	(progn
	  (beginning-of-line)
	  (setq netrexx-boundry-hit nil))
      (netrexx-beep)
      (if netrexx-boundry-hit
	  (progn
	    (message "Wrapping to end of buffer...")
	    (goto-char (point-max))
	    (if (re-search-backward "^[ \t]*method\\b" nil t 1)
		(beginning-of-line)
	      (message "No method available in buffer")
	      (netrexx-beep))
	    (setq netrexx-boundry-hit nil))
	(message "End of buffer...")
	(setq netrexx-boundry-hit t))))
  (setq last-command 'netrexx-previous-method))

(defun netrexx-beginning-of-method (&optional arg)
  "Jumps to the beginning of the method.  
ARG repeats the search ARG times. It always returns t, unless no
method is found."
  (interactive "p")
  (let ((beg (point)))
    (or (save-excursion
	  (beginning-of-line)
	  (netrexx-looking-at-method-p))
	(re-search-backward "^[ \t]*method\\b" nil t (or arg 1))
	(progn
	  (goto-char beg)
	  nil))))

(defun netrexx-end-of-method (&optional arg)
  "Jumps to the end of the method.
ARG repeats to search ARG times. It always returns t, unless no method
end is found.

Comments before the method are reckoned to be part of that method.
Meaning that if point is at a comment that describes a method, this
function will bring you to the end of that method."
  (interactive "p")
  (let ((beg (point)))
    (while (netrexx-looking-at-comment-p)
      (forward-line 1))
    (if (netrexx-beginning-of-method)
	(progn
	  (re-search-forward "^[ \t]*method\\b" nil t (+ (or arg 1) 1))
	  (beginning-of-line)
	  (forward-line -1)
	  (while (or (looking-at "^[ \t]*$")
		     (netrexx-looking-at-comment-p))
	    (forward-line -1)))
      (progn
	(goto-char beg)
	nil))))

(defun netrexx-select-current-block ()
  "Selects all lines between matching do (loop / select) and end.

It will return t if it can find an \"end\" statement below the point
and that \"end\" statement has a matching \"do\", \"loop\" or
\"select\" statement.  The matching is done with
\\[netrexx-from-end-goto-matching-do].

If it cannot find such a statement, it will select the whole method.
Belonging to that method are the comments written directly before the
method statement. Normally these are the javadoc style comments, but
it could be any kind of comment. This means that if point is at a line
that contains a comment, it will skip forward until it finds a
non-comment line. It will then select the whole method, including the
comments before the method statement.

When even that fails, it will return nil. "
  (interactive)
  (let (start)
    (setq start (point))
    (beginning-of-line)
    ;; store the current position of point
    (let ((beg (point))
	  (still-looking t)
	  (search-count 1)
	  (retval nil))
      (while still-looking
	;; find an end-statement below the point when looking back,
	;; the point should be equal to or smaller than beg
	(goto-char beg)
	;; we keep on looking until we pass another method definition
	(if (and (re-search-forward "^[ \t]*end\\b" nil t search-count)
		 (not (re-search-backward "^[ \t]*method\\b" beg t 1)))
	    (progn
	      (set-mark-command nil)
	      (if (netrexx-from-end-goto-matching-do)
		  (progn
		    (setq still-looking (< beg (point)))
		    (if still-looking
			(setq search-count (+ search-count 1))
		      (setq retval t)
		      ;; it makes more sense to have point at the
		      ;; "end" statement, so swap point and mark.
		      (exchange-point-and-mark)))
		(pop-global-mark)
		(setq retval nil)))
	  (setq still-looking nil)))
      ;; if nothing found, try to match the whole method
      (if (not retval)
	  (progn
	    (goto-char beg)
	    (if (or (netrexx-looking-at-method-p)
		     ;; include comments above the method, if found
		     ;; then return t
		     (progn
		       (while (netrexx-looking-at-comment-p)
			 (forward-line 1))
		       (netrexx-looking-at-method-p))
		     (re-search-backward "^[ \t]*method\\b" nil t 1))
		(progn ;; now select all until next method. This
		  ;; includes all comments belonging to that method,
		  ;; written directly above the method.
		  (beginning-of-line) 
		  (forward-line -1)
		  (while (netrexx-looking-at-comment-p)
		    (forward-line -1))
		  ;; we one line to far up, so compensate
		  (forward-line 1)
		  (set-mark-command nil)
		  ;; first find next method. eobp will do just fine
		  ;; too.  the first one it will find is the one we
		  ;; just jumped back to, hence the search count of 2.
		  (if (re-search-forward "^[ \t]*method\\b" nil t 2)
		      ;;select upto the next method, but not
		      ;; inclusive. Also do not select the comments
		      ;; that are direct before the method definition.
		      (progn
			(beginning-of-line) 
			(forward-line -1)
			(while (netrexx-looking-at-comment-p)
			  (forward-line -1))
			;; we one line to far up, so compensate
			(forward-line 1))
		    (goto-char (point-max)))
		  (setq retval t))
	      (setq retval nil))))
      ;; warn that something went awry
      (if (not retval) 
	  (progn
	    (netrexx-beep)
	    ;; 	  (set-mark-command nil)
	    (goto-char start)
	    (message "Not within a block!")))
      retval)))

;;; Line breaking and paragraph filling.
(defun netrexx-fill-paragraph-function (&optional arg)
  "Function to assign to `fill-paragraph-function' that fills javadocs.
It will reflow the comments listed in the javadoc. All lines that
begin with @keyword get a special indentation."
  (interactive "*P")
  (save-excursion
    (cond ((netrexx-inside-javadoc-p)
	   (netrexx-fill-comments))
	  ((save-excursion 
	     (beginning-of-line)
	     (looking-at "^[ \t]*.*--")) ;; this is done before the
				         ;; normal comments because of
				         ;; sequence errors.
	   (netrexx-fill-single-line-comments))
	  ((netrexx-inside-comment-p)
	   (netrexx-fill-comments))
	  ((netrexx-looking-at-method-p)
	   (netrexx-fill-method))
	  (t ())
	  ))
  ;; Always return true. This has the effect that if filling isn't
  ;; done above, it isn't done at all, and it's therefore effectively
  ;; disabled in normal code.
  t)

(defun netrexx-fill-comments ()
  "called from `netrexx-fill-paragraph-function' to fill comments"
   ;; first find begin and end of the region we have to fill
  (let ((beg (point))
	(end (point))
	(still-looking t))
    ;; find the beginning
    (beginning-of-line)
    (while still-looking
      (when (or (looking-at "^[ \t]*/\\*") ;; looking at comment
		;; start
		(looking-at "^[ \t]*\\(\\*\\)?[ \t]*@")) ;; looking at
	;; javadoc tag
	(setq beg (point)
	      still-looking nil))
      (forward-line -1))
    ;; find the end
    (setq still-looking t)
    (goto-char beg)
    (forward-line 1)
    (while still-looking
      (when (or (looking-at "^[ \t]*\\*/") ;; looking at comment end
		  (not (netrexx-looking-at-comment-p))
		  (looking-at "^[ \t]*\\(\\*\\)?[ \t]*@")) ;; looking at
	;; javadoc tag
	(forward-line -1)
	(end-of-line)
	(setq end (point)
	      still-looking nil))
      (forward-line 1))
    ;; now fill-out the paragraph between beg and end
    ;; mark the last line by inserting an empty one
    (goto-char end)
    (insert "\n")
    ;; strip the leading *, if any
    (replace-regexp "^[ \t]*\\*" " " nil beg end)
    ;; now join the lines together in one big line
    (goto-char beg)
    (forward-line 1)
    (while (not (looking-at "^[ \t]*$"))
      (delete-indentation)
      (forward-line 1))
    ;; now split the line at or around `fill-column'
    (goto-char beg)
    ;; are we dealing with a javadoc tag? If so, then calculate the
    ;; number of spaces to insert so we can line up after the tag.
    (let ((java-tag-p (looking-at "^[ \t]*\\(\\*\\)?[ \t]*@"))
	  (java-tag-len 5))
      (when java-tag-p
	(save-excursion
	  (let ((a (re-search-forward "@" end t 1))
		(b (re-search-forward "[ \t]" end t 1)))
	    (setq java-tag-len (+ (- b a) 1)))))
      ;; start splitting the line.
      ;; goto the end of the javadoc start definition and split from there.
      (let ((line-end (save-excursion (end-of-line) (point))))
	(cond ((re-search-forward "^[ \t]*/\\*\\*" line-end t 1)
	       (forward-char 1)
	       (netrexx-indent-newline-indent))
	      ((re-search-forward "^[ \t]*/\\*" line-end t 1)
	       (netrexx-indent-line))
	      ((re-search-forward "^[ \t]*\\*" line-end t 1)
	       (netrexx-indent-line))
	      (t
	       (netrexx-indent-line)
	       (insert "* ")
	       (netrexx-indent-line))))
      ;; now split the rest
      (while (eq (move-to-column fill-column) fill-column)
	;; find a whitespace or tab at or before point, keep on
	;; looking until you find one.
	(while (not (looking-at "[ \t]+"))
	  (backward-char 1))
	(forward-char 1)
	;; insert some spaces for the javadoc tag
	(when java-tag-p
	  (insert-char ?\  java-tag-len)
	  (backward-char java-tag-len))
	;; now indent the line. This will insert the leading *
	(netrexx-indent-newline-indent)))
      ;; remove the inserted empty line.
      (delete-blank-lines)))

(defun netrexx-fill-single-line-comments ()
  "Fills out single line comments based on the column the comment starts.

Called from \\[netrexx-fill-paragraph]."
  ;; first find begin and end of the region we have to fill
  (let ((beg (point))
	(end (point))
	(still-looking t))
    ;; find the beginning
    ;; First line might be after some code. The rest must be on
    ;; otherwise empty lines.
    (beginning-of-line)
    (while still-looking
      (if (looking-at "^[ \t]*--")
	  (forward-line -1)
	(re-search-forward "--" nil t 1)
	(setq beg (- (point) 2)
	      still-looking nil)))
    ;; find the ending
    (forward-line 1)
    (beginning-of-line)
    (setq still-looking t)
    (while still-looking
      (if (looking-at "^[ \t]*--")
	  (forward-line 1)
	(re-search-backward "--" beg t 1)
	(end-of-line)
	(setq end (point)
	      still-looking nil)))
    ;; mark the ending
    (goto-char end)
    (end-of-line)
    (insert "\n")
    ;; remove all superfluous -- characters
    (replace-regexp "^[ \t]*--" " " nil (+ beg 2) end)
    ;; now join all lines together into one big line
    (goto-char beg)
;;     (when (bolp)
;;       (delete-indentation))
    (forward-line 1)
    (while (not (looking-at "^[ \t]*$"))
      (delete-indentation)
      (forward-line 1))
    ;; now split the line at or before `fill-column'
    (goto-char beg)
    (when (not (looking-at "[ \t]*--"))
      (insert "--")
      (backward-char 2)
      (netrexx-indent-newline-indent))
    ;; while there is still text on the fill-column
    (setq still-looking  (eq (move-to-column fill-column) fill-column))
    (while still-looking
      ;; find a whitespace or tab at or before point
      (while (not (looking-at "[ \t]+"))
	(backward-char 1))
      ;; make sure we still can split the line
      (if (save-excursion
		   (backward-char 3)
		   (looking-at "[ \t]*--"))
	  (setq still-looking nil)
	;; else
	(insert "--")
	(backward-char 2)
	(netrexx-indent-newline-indent)
	(setq still-looking  (eq (move-to-column fill-column) fill-column))))
    ;; remove the inserted empty line.
    (end-of-line)
    (forward-char 1)
    (kill-line)
))

(defun netrexx-fill-method ()
  "Divides method parameters with continuation characters . 

When the method definition goes beyond `fill-column' and the
method statement has parts that can be continued on the next
line, then this is done. The result is not checked for crossing
the `fill-column' border.

Parts that can be continued on the next line are:
- Parameters, separated by comma's
- returns statement
- signals statement 
- signals list, separated by comma's"
  (interactive)
  (when (netrexx-looking-at-method-p)
    ;; we have to see if we can split the line.
    ;; It must not be continued already 
    ;; It must contain text at the fill-column
    (when (and (not (looking-at "[^-]-[ \t]*$"))
	       (eq (move-to-column fill-column) fill-column))
      ;; temporarily turn of the automatic netrexx-auto-insert-javadoc
      (let ((old-value 'netrexx-auto-insert-javadoc)
	    (end (end-of-line))
	    (beg (beginning-of-line))
	    (still-looking t))
	(setq netrexx-auto-insert-javadoc nil)
	;; start from the beginning and find the first comma or )
	(while still-looking
	  (if (re-search-forward ",\\|)" end t 1)
	      (progn
		(backward-char 1)
		(if (looking-at ",")
		    (progn
		      (forward-char 1)
		      (insert " -")
		      (newline-and-indent)
		      (save-excursion
			(setq end (end-of-line))))
		  ;; else
		  (setq still-looking nil)))
	    ;; else
	    (setq still-looking nil)))
	;; find out whether we first should re-flow 'signal' or
	;; 'returns'
	(let ((signals-pos (save-excursion (re-search-forward "signals" end t 1)))
	      (returns-pos (save-excursion (re-search-forward "returns" end t 1))))
	  (cond ((and signals-pos returns-pos)
		 (if (< signals-pos returns-pos)
		     (progn
		       (netrexx-fill-method-signals)
		       (netrexx-fill-method-returns)
		   ;; else
		   (netrexx-fill-method-returns)
		   (netrexx-fill-method-signals))))
		(signals-pos
		 (netrexx-fill-method-signals))
		(returns-pos
		 (netrexx-fill-method-returns))
		(t
		 ())))
	;; restore the netrexx-auto-insert-javadoc value
	(setq netrexx-auto-insert-javadoc old-value)
	))))

(defun netrexx-fill-method-signals ()
  "Fills out the signals clause of a method. Called from
\\[netrexx-fill-method]. Starts at the beginning of the current
line."
  (beginning-of-line)
  (let ((end (end-of-line))
	(still-looking t))
    (if (re-search-forward "signals" end t 1)
	(progn
	  (while still-looking
	    (insert " -")
	    (newline-and-indent)
	    (if (re-search-forward ",\\|returns" end t 1)
		(setq still-looking (looking-at ","))
	      ;; else
	      (setq still-looking nil))))
      ;; else
      (message "Expected \"signals\"-statement, but none found when filling"))))
		      
(defun netrexx-fill-method-returns ()
  "Fills out the returns clause of a method. Called from
\\[netrexx-fill-method]."
;; don't know how to implement this yet.
)

(defun netrexx-initial-template ()
  "Inserts default package, javadoc and class statements in a new file.

The classpath to the root of the package is stored in 
`netrexx-package-path'
It also generates a default constructor statement without any
arguments. "
  (interactive)
  (let (beg end class-name)
    (goto-char (point-min))
    (insert "package " (buffer-file-name))
    (replace-regexp netrexx-package-path "" nil (point-min) (point-max))
    (replace-regexp "/" "." nil (point-min) (point-max))
    ;; remove the last "."
    (delete-char -1)
    (insert "\n\n")
    ;; save the class name 
    (setq class-name (netrexx-return-word))
    ;; remove the current line containing the file name
    (delete-region (progn
		     (beginning-of-line)
		     (point))
		   (progn
		     (end-of-line)
		     (point)))
    (when netrexx-auto-insert-javadoc
      (insert "/**\n")
      (insert " * Class " class-name " implements... \n")
      (insert " * @version $id: $\n")
      (insert " */\n"))
    (goto-char (point-max))
    (insert "class " class-name "\n")
    (insert "\n")
    (when netrexx-auto-insert-javadoc
      (insert "/**\n")
      (insert " * Default constructor\n")
      (insert " */\n"))
    (insert "method " class-name "()\n")
    (insert "return\n")
    (indent-region (point-min) (point-max) nil)
    ))

(defun netrexx-insert-javadoc-for-method ()
  "Inserts an appropriate javadoc statement for the method.

The javadoc based on:
- the name of the method, 
- the name and type of the parameters,
- the return type of the method."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (save-excursion
      (when (netrexx-looking-at-method-p)
	(insert "/**\n")
	(insert "*\n")
	(insert "*/\n")
	;; go back to the method definition
	(re-search-forward "^[ \t]*method[ \t]+" nil t 1)
	(let ((method-name (netrexx-return-word)))
	  (forward-line -2)
	  (end-of-line)
	  (insert " Method " method-name " ..."))
	;; go back to the method definition
	(re-search-forward "^[ \t]*method[ \t]+" nil t 1)
	(let ((param-count 1)
	      (beg (point))
	      (end (progn 
		     (end-of-line)
		     (point))))
	  (goto-char beg)
	  (when (re-search-forward "-[ \t]*$" end t 1)
	    (setq end (re-search-forward ")" nil t 1)))
	  ;; repeat this for all parameters. parameters are separated
	  ;; by a , and optionally have a type
	  (let ((still-looking t))
	    (while still-looking
	      (goto-char beg)
	      (when (re-search-forward "(" end t 1)
		(if (looking-at ")") ;;  looking at a method without
		    ;;  parameters.
		    (setq still-looking nil)
		  ;; else
		  (let ((param-name nil)
			(param-type nil))
		    (or (re-search-forward "," end t param-count)
			(re-search-forward ")" end t 1))
		    (backward-char 1)
		    ;; is there a type? Or is the param alone and of type Rexx?
		    (if (and (re-search-backward "=\\|," beg t 1)
			     (looking-at "="))
			(progn
			  (backward-word 1)
			  (setq param-name (netrexx-return-word))
			  (forward-word 1)
			  (backward-word 1)
			  (setq param-type (netrexx-return-word))
			  (while (looking-at "\\.") ;; type is qualified
			    (setq param-type (concat param-type "." (netrexx-return-word)))))
		      ;; else, no parameter type, so it's Rexx
		      (when (looking-at ",\\|)")
			(setq param-type "Rexx")
			(or (looking-at ")")
			    (forward-word 1))
			(backward-word 1)
			(setq param-name (netrexx-return-word))))
		    (setq still-looking (not (looking-at ")"))
			  param-count (+ param-count 1))
		    (while (not (looking-at "^[ \t]*\\*/"))
		      (forward-line -1))
		    (insert "* @param " param-name " is a " param-type "\n")
		    ;; go back to the method definition
		    (re-search-forward "^[ \t]*method[ \t]+" nil t 1)
		    (save-excursion
		      (setq beg (progn (beginning-of-line) (point))
			    end (progn (end-of-line) (point)))
		      (goto-char beg)
		      (when (re-search-forward "-[ \t]*$" end t 1)
			(setq end (re-search-forward ")" nil t 1)))
		      )
		    )))))
	  
	  ;; next, insert the @return javadoc tag, if needed
	  (save-excursion
	    (setq end (progn (forward-line 1) (end-of-line) (point))))
	  (when (re-search-forward ") returns " end t 1)
	    (let ((return-type (netrexx-return-word)))
	      (while (looking-at "\\.") ;; return type is qualified
		(setq return-type (concat return-type "." (netrexx-return-word))))
	      (forward-line -1)
	      (insert "* @return " return-type " containing ...\n")))
	  )))
    ;; indent the new stuff
    (let ((beg (point))
	  (end (progn 
		 (re-search-forward "^[ \t]*method\\b" nil t 1)
		 (point))))
      (indent-region beg end nil))))

(defun netrexx-version ()
  "Displays the current version of netrexx mode."
  (interactive)
  (message "Netrexx-mode version %s." netrexx-mode-version))

;; ------------ speedbar additions ------------
(require 'speedbar)

(defcustom netrexx-imenu-generic-expression
  (list
   '("method" "^[ \t]*method \\([a-zA-Z0-9_]*\\)" 1)
   '("class" "^[ \t]*class \\([a-zA-Z0-9_]*\\)" 1))
  "Value for `imenu-generic-expression' in NetRexx mode."
  :type 'regexp
  :group 'netrexx)

;; (eval-when-compile (require 'speedbar))
(speedbar-add-supported-extension ".nr[xy]")
(add-to-list 'speedbar-fetch-etags-parse-list '("\\.nr[xy]\\'" . "^[ \t]*method [a-zA-Z0-9]*"))

(defun netrexx-speedbar-buttons (buffer)
  "Create a speedbar display to help navigation in a netrexx file.
BUFFER is the buffer speedbar is requesting buttons for."

)

(defvar netrexx-mode-syntax-table 
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?- ". 12b" st)
    (modify-syntax-entry ?/ ". 14a" st)
    (modify-syntax-entry ?* ". 23a" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\' "\"" st)
    st)
  "Syntax table in use in NETREXX-mode buffers.")


;; (defun netrexx-create-syntax-table ()
;;   (if netrexx-mode-syntax-table
;;       ()
;;     (setq netrexx-mode-syntax-table (make-syntax-table))
;;     (modify-syntax-entry ?. "." netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?- ". 12b" netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?/ ". 14" netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?* ". 23" netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?\n "> b" netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?\' "\"" netrexx-mode-syntax-table))

;;   (set-syntax-table netrexx-mode-syntax-table))
;;     (modify-syntax-entry ?\\ "\\" netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?/ ". 14" netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?* ". 23" netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?+ "." netrexx-mode-syntax-table)
;;     ;;   (modify-syntax-entry ?- ". 12b" netrexx-mode-syntax-table)
;;     ;;   (modify-syntax-entry ?\n "> b" netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?= "." netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?% "." netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?< "." netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?> "." netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?& "." netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?| "." netrexx-mode-syntax-table)
;;     (modify-syntax-entry ?\' "\"" netrexx-mode-syntax-table))

(defun netrexx-mode ()
"Major mode for editing NETREXX code.

Variable controlling indentation style:
 `netrexx-indent-amount'
	The basic indentation for do-blocks.

Variable controlling end-comments:
`netrexx-end-comment-treshold'
   Number of lines the matching block beginning has to be away from
   the end statement.

Turning on NETREXX mode calls the value of the variable
netrexx-mode-hook with no args, if that value is non-nil.

;;For example:
;; (setq netrexx-mode-hook  '(lambda ()
;;			 (setq netrexx-indent-amount 2)
;;			(local-set-key `\\C-m' 'netrexx-indent-newline-indent)
;;			))

Two extra keymappings are defined: 
C-c C-n maps to M-x `netrexx-next-method' and 
C-c C-p maps to M-x `netrexx-previous-method'.

For convenience it is possible to map
`netrexx-indent-newline-indent-with-end-comment' instead of
\\[netrexx-indent-newline-indent] to C-m in the above example. This
will place a small -- style comment right after every
\"end\"-statement, containing the matching do, if, et cetera
statement.

A further convenience method is \\[netrexx-sanitize-region], which
will remove all non-single blank lines from the file. It will also
look for \"trace results\" and \"trace methods\" statements and
comment them if within 4 lines back no \"if\" statement is found.
"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table netrexx-mode-syntax-table)
  (setq font-lock-keywords-case-fold-search nil)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(netrexx-font-lock-keywords))
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'netrexx-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "/*")
  (make-local-variable 'comment-end)
  (setq comment-end "*/")
  (make-local-variable 'imenu-case-fold-search)
  (setq imenu-case-fold-search t)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression netrexx-imenu-generic-expression)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'netrexx-fill-paragraph-function)
  (setq major-mode 'netrexx-mode)
  (setq mode-name "Netrexx")
  (use-local-map netrexx-mode-map)
  (imenu-add-menubar-index)
  (run-hooks 'netrexx-mode-hook))

(provide 'netrexx-mode)

;;; netrexx-mode.el ends here.

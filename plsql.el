;;; plsql.el --- Programming support for PL/SQL code

;; Copyright (C) 2001, 2002 by Free Software Foundation, Inc.

;; Author: Kahlil (Kal) HODGSON <dorge@tpg.com.au>
;; X-URL: http://www.emacswiki.org/elisp/plsql.el
;; Keywords: languages
;; Time-stamp: "2002-11-07 01:26:20 kahlil"
;; Version: 0.8.0

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;_ * Commentary

;; Provides a major mode (PL/SQL) that supports writing PL/SQL code.
;;
;; This `plsql-mode' is an extension of `sql-mode' and uses that mode
;; to provide keyword fontification, symbol-tables, and interaction
;; with the database.  In addition to foxing some bugs this mode
;; provides the following:
;;
;; 1. Fast and sophisticated indentation function `plsql-indent'
;; (normally bound to TAB) which can help to present (and maintain)
;; complex code in a readable form.
;;
;; 2. Imenu support which allows you to jump to specific modules using
;; either the "Contents" menu or speedbar.
;;
;; 3. Align support which allows you to automatically "beautify" code
;; as you write by aligning certain constructs e.g. `align-current'
;; will align all =, :=, and => tokens in the current paragraph.
;; XEmacs users may have to get a copy of the align package to use
;; this feature.

;; This mode was written for FSF Emacs but has been tested in XEmacs.

;;;_ * Installation:

;; Place this file somewhere in your load path and byte-compile (!) it.
;; Also check the customizable variables for this package
;; `plsql-indent' and `plsql-uses-font-lock'. You can then set the
;; plsql major mode interactively via the command `plsql-mode' or
;; automatically via filename extensions by adding the following to
;; your dot emacs file:
;;
;;    (setq auto-mode-alist
;;          (append
;;           '(("\\.\\(p\\(?:k[bg]\\|ls\\)\\|sql\\)\\'" . plsql-mode))
;; auto-mode-alist))
;;
;; If you use the speedbar you may want to set the following
;;
;;    (speedbar-add-supported-extension "pls")
;;    (speedbar-add-supported-extension "pkg")
;;    (speedbar-add-supported-extension "pkb")
;;    (speedbar-add-supported-extension "sql")
;;
;; as per your favorite filename extensions.

;; Enjoy!

;;;_ * Known Bugs

;; This has only been tested on a handful of programs I had to write
;; for a specific job.  That code was reasonably complex but certainly
;; did not cover every aspect of PL/SQL so there are bound to be some
;; bugs. I am not currently working with this language so further
;; development has been suspended; however, if you find a bug please
;; email me with the offending code and I'll have a go at fixing it:-)

;; (1) A comment flush against a keyword/statement end produces bad
;; indentation.  Probably too hard to fix. Does anyone actually like
;; to do this?

;; (2) Don't know if TAB gets bound properly in XEmacs. If anyone can
;; confirm this please email me (saves me installing XEmacs:-)

;; (3) The some inconsistencies will screw up the indention that follows e.g.

;;        if CONDITION then
;;           STATEMENT
;;        else STATEMENT;
;;        STATEMENT;
;;     end if;

;; is screwed up but

;;        if CONDITION then
;;           STATEMENT
;;        else
;;           STATEMENT;
;;           STATEMENT;
;;        end if;

;; is O.K.  Let me know if you really want to do the former and I'll
;; fix it :-)

;;;_ * Credits

;; Inspired by an pls-mode.el by Dmitry Nizhegorodov (Oracle) and
;; plsql-mode.el by Karel Sprenger.

;; Thanks to Arch Murphy for noting a problem with XEmacs, to Brett
;; for noting a problem with push, and Andreas Zielke for noting a
;; indentation problem with "blocks within blocks".  Thanks to Jérome
;; Haguet for spotting another indentation bug and for suggesting
;; `plsql-indent-region'. Thanks to Alex Schroeder for compilation
;; suggestions and suggestions regarding SQL level indentation. Thanks
;; to Mark Buckle for convincing me to support block-less PL/SQL and
;; inspiring a huge code cleanup.

;;;_ * To do

;; (*) Check logic on calling `plsql-sql-statement-indent' do we still
;; need to do this.

;; (1) BNF for PL/SQL to add to semantic bovinator (Eeek!)
;; (2) Imenu for local modules, global variables? (really need
;; bovinator for that)
;; (3) Object oriented extensions? What are they? Haven't used this
;; stuff yet.
;; (4) Movement functions. Does anyone actually use these? Speedbar is
;; faster?
;; (5) Filling comments? Defer to rebox.
;; (6) Set the current indentation context as a text-property. This
;; could vastly simplify the code. Perhaps a new package entirely.
;; (7) Develop a test file containing all the PL/SQL contortions I can
;; think of.
;; (8) Pass on comment-indent and string-indent to user definable
;; functions.
;; (9) Can we have anonymous blocks inside exception sections?
;; (10) Make "align" line up anything that is highlighted?

;;;_ * Code

;; Note to hackers: explanatory notes scatter throughout.
;; Suggested starting point: search for ";;;;; Top Level"

(defgroup plsql nil "")

;;;; Indentation:
;;;;; User Variables:

(defcustom plsql-indent 3
  "*Number of columns of indentation for each level."
  :group 'plsql
  :type  'number)

(defcustom plsql-uses-font-lock t
  "*Indicate whether font-lock properties can be use to speed up indentation.
This is up to 3 times faster, and hence, highly recommended."
  :group 'plsql
  :type  'boolean)

(defcustom plsql-mode-hook '()
  "*Hook for customizing `plsql-mode'."
  :type 'hook
  :group 'plsql)


;;;_  + Local Variables

(defvar plsql-debug  nil
  "If t then we rebuild everything on reload. Useful for debugging.")

(eval-when-compile (setq plsql-debug t))

(defun plsql-toggle-debug () "Toggle value of plsql-debug."
  (interactive)
  (message "plsql-debug is %s" (setq plsql-debug (not plsql-debug))))

(defun plsql-reset (variable)
  "t iff VARIABLE should be reset"
  (or plsql-debug
      (null variable)
      (string-equal variable "")))

(defvar plsql-indent-function-stack nil
  "Record the indent functions use to indent the current line.")

(defvar plsql-white-space-re  "[ \t\n\r()]"
  "Regular expression matching whitespace or anything that delimits
  identifiers.")

;; this includes () since they can delimit
(when (plsql-reset plsql-white-space-re)
  (setq plsql-white-space-re "[ \t\n\r()]" ))

;;;_  + Predicates

(defun plsql-in-comment-p (&optional limit)
  "Return non-nil iff point is in a comment.
LIMIT is a point before the current point used to limit the partial parse."
  (or (looking-at "/\\*")
      (nth 4 (parse-partial-sexp limit (point)))))

(defun plsql-in-string-p (&optional limit)
  "Return non-nil iff point is in a string.
LIMIT is a point before the current point used to limit the partial parse."
  (nth 3 (parse-partial-sexp limit (point))))

;; alternatively using font-lock faces is even faster

(defun plsql-comment-face-p (&optional limit)
  "Return non-nil iff text at point is displayed in a `font-lock-comment-face'."
  (eq (get-text-property (point) 'face) 'font-lock-comment-face))

(defun plsql-string-face-p (&optional limit)
  "Return non-nil iff text at point is displayed in a `font-lock-string-face'."
  (eq (get-text-property (point) 'face) 'font-lock-string-face))

(defvar plsql-in-comment-predicate 'plsql-in-comment-p
  "Function used to determine if point is inside a comment.")

(defvar plsql-in-string-predicate 'plsql-in-string-p
  "Function used to determine if point is inside a string.")

;;;_  + RegExp Search

;; defining the following functions in-line give a small increase in speed

(defsubst plsql-re-search-backward
  (regexp &optional bound no-error count limit)

  "Search backwards for REGEXP ignoring comments and strings.
Optional parameters BOUND, NO-ERROR and COUNT behave as for `re-search-backward'.
LIMIT is used to bound partial parse and is assumed to be before the current point."

  ;; cant think of any better logic than this

  (let ((not-found t) ;; invert the sense to minimise calls to `not'
	(limit (or limit 1)))
    (while (and not-found
		(re-search-backward regexp bound no-error count))
      (forward-char 1)  ;; point is on first char of match
      (unless (or (funcall plsql-in-comment-predicate limit)
		  (funcall plsql-in-string-predicate  limit))
	(setq not-found nil))
      (forward-char -1) ;; point is before first char of match
      )

    (not not-found)))

(defsubst plsql-re-search-forward
  (regexp &optional bound no-error count limit)

  "Search forwards for REGEXP ignoring comments and strings.
Optional parameters BOUND, NO-ERROR and COUNT behave as for `re-search-forward'.
LIMIT is used to bound partial parse and is assumed to be before the current point."

  (let ((not-found t) ;; invert the sense to minimise calls to `not'
	(limit (or limit 1)))
    (while (and not-found
		(re-search-forward regexp bound no-error count))

      (forward-char -1) ;; point is on last char of match
      (unless (or (funcall plsql-in-comment-predicate limit)
		  (funcall plsql-in-string-predicate  limit))
	(setq not-found nil))
      (forward-char 1)  ;; point is after last char of match
      )
    (not not-found)))

;;;_  + Statement Level

;; There are a few special sub-statement forms that will require slightly
;; different indentation.
;;
;;   a) expressions in parentheses e.g. function parameters
;;   b) SQL (block) statements e.g. SELECT statements in a cursor
;;   c) statements that span more then one line.

(defvar plsql-leading-identifier-re ""
  "Regexp that will match a PL/SQL identifier." )

(when (plsql-reset plsql-leading-identifier-re)
  (setq plsql-leading-identifier-re "^[ \t\n\r]*\\([_#:$a-z,A-Z]+\\)"))

;; SYMBOL TABLE?

(defvar plsql-sql-statement-re ""
  "Regexp matching key terms indicating that we are within an SQL statement.")

;; Borrowed this from sql-indent.el
;; plus some from Oracle 9i SQL reference
;; removed some that conflict with some PL/SQL keywords
;; do we need all these?

;; Exclude words longer that 8 characters.

;; This RE is usually referenced by looking at with point at the
;; beginning of the indentation so we can we don't need to check for
;; leading word boundary.

(when (plsql-reset plsql-sql-statement-re)
  (setq plsql-sql-statement-re
	(concat
	 "\\("
	 (regexp-opt
	  (list
	   "access" "add" "all" "alter" "analyse" "and" "any" "as" "asc"
	   ;;       "associate"
	   "audit"
	   ;;       "authorization"
	   "avg" "between"
	   "by"
	   ;; "char"
	   "check" "cluster"
	   ;;	 "close"
	   ;;	 "cobol"
	   "column" "comment" "commit" "compress" "connect" "continue"
	   ;;	 "count"
	   "create" "current"
	   ;; "date" "decimal"
	   "declare"
	   ;; "default"
	   "delete"
	   "desc" "distinct"
	   ;; "double"
	   "drop"
	   ;;	 "else"
	   "escape"
	   ;;       "exclusive"
	   "exec" "exists" "explain"
	   ;;	 "fetch"
	   ;;    "file"
	   "float"
	   ;;	 "for"
	   "foreign" "fortran" "found" "from" "go"
	   ;;	 "goto"
	   "grant" "group" "having" "identified" "immediate" "in" "increment"
	   "index"
	   ;;       "indicator"
	   "initial" "insert" "integer"
	   ;;       "intersect"
	   "into"
	   ;;	 "is"
	   "key"
	   ;;	 "language"
	   "level" "like" "lock" "long" "max"
	   ;;       "maxextents"
	   "min" "minus"
	   ;;	 "mlslabel"
	   "mode" "modify" "module" "noaudit"
	   ;;       "nocompress"
	   "not" "nowait" "null"
	   ;; "number"
	   "numeric" "of" "offline" "on" "online"
	   ;;	 "open"
	   "option" "or" "order"
	   ;;	 "pascal"
	   "pctfree" "pli"
	   ;;       "precision"
	   "primary" "prior"
	   ;;	 "privileges"
	   "public"
	   ;;    "raw"
	   ;;	 "references"
	   "rename" "resource" "revoke" "revoke" "rollback"
	   "row" "rowid" "rownum" "rows"
	   ;;	 "savepoint"
	   "schema" "section" "select" "session" "set" "share" "size"
	   ;; "smallint"
	   "some" "sqlcode" "sqlerror" "start"
	   ;;       "successful" "sum"
	   "synonym" "sysdate" "table"
	   ;;	 "then"
	   "to" "trigger" "truncate"
	   "uid" "union" "unique" "update" "user" "validate" "values"
	   ;;"varchar" "varchar2"
	   "view" "whenever" "where" "with" "work"
	   ))
	 "\\)"
	 plsql-white-space-re
	 )))

(defsubst plsql-sql-statement-adjust ()
  "If we are inside an SQL statement returns extra indentation required to line
  the last character of the SQL keyword that starts each line."

  ;; this is called solely to determine the real indent level of the
  ;; statement preceding the active line.

  (when plsql-debug
    (add-to-list 'plsql-indent-function-stack 'plsql-sql-statement-adjust 'append))

  (save-excursion
    (back-to-indentation)
    (if (looking-at plsql-sql-statement-re)
	(- 8 ;; length of the longest keyword
	   (length (thing-at-point 'word)))
      0)))

(defvar plsql-extra-indent-re ""
  "A couple of situations in which we might like to add a little indentation.")

(when (plsql-reset plsql-extra-indent-re)
  (setq plsql-extra-indent-re
	(concat
	 "\\b"         ;; open word boundary
;;	 "\\("         ;; open group of alternatives
	 (regexp-opt
	  (list
	   "in"
	   "default"
	   ) t )
;;	 "\\)"         ;; close group of alternatives
	 plsql-white-space-re)	 ;; minimal whitespace after key word
	))

(defun plsql-statement-level-adjust (limit indent)
  "Adjust the proposed INDENT column at the statement level.
LIMIT marks the beginning of the current statement.  Here we deal with the
indentation of SQL keywords starting an SQL block , variable declaration
keywords, and multiline statements"

  ;; PRE CONDITION:
  ;; called after a `back-to-indentation'
  ;; POST CONDITION:

  (when plsql-debug
    (add-to-list 'plsql-indent-function-stack
		 'plsql-statement-level-adjust 'append))

  (cond
   ((looking-at plsql-sql-statement-re)
    ;; line up last character of leading SQL keywords
    (+ indent
       (- 8 ;; length of CONNECT
	  (length (thing-at-point 'word)))))

   ((looking-at plsql-extra-indent-re)
    ;; so add some extra indentation
    (+ indent plsql-indent))

   ;; some times a section starter involves two words which we may
   ;; want to put on different lines e.g. if .. then
   ((looking-at (concat "then\\|loop\\|is\\|as" plsql-white-space-re))
    (if (= (point) limit) ;; loop may also start a statement
	indent
      (save-excursion
	(goto-char limit)
	(current-indentation))))

   ((save-excursion (plsql-re-search-backward "\n\\|\r" limit t nil limit))
    ;; there a line break between point and the beginning of statement so
    ;; add some extra indentation.

    (+ indent plsql-indent))
   (t indent)
   ))

;; (defun plsql-in-parenthesis-p ()
;;   (interactive)
;;   (save-excursion
;;     (let ((open 0))
;;       (while (and (< open 1)
;;		  (plsql-re-search-backward "(\\|)" nil t))
;;	(cond ((equal "(" (match-string 0))
;;	       (setq open (+ open 1)))
;;	      (t (setq open (- open 1))
;;
;;		 )))
;;       (if (> open 0)
;;	  (point)
;;	nil)
;;       )
;;     ))

;; throw and catch may well be our boys for this one
;; as might be recursion

(defun plsql-in-parenthesis-p (limit)
  "If inside a parenthesis group, return the column of the open paren,
else return nil.  Search bound by LIMIT."
  (save-excursion
    (let ((parse-sexp-ignore-comments t))

      ;; bottle-neck is really caused by this condition case

      ;;	 (result (condition-case nil
      ;;			     (scan-lists (point) -1 1)
      ;;			   (error nil))))

      (nth 1 (parse-partial-sexp (point) limit)))))

(defvar plsql-statement-end-re nil
  "Regexp matching any kind of statement end.")

(when (plsql-reset plsql-statement-end-re)
  (setq plsql-statement-end-re
	 (concat
	  "\\("          ;; mark start of statement end

	  ""     ";"     ;; generic PL/SQL statement end

;;;	 "\\|"  "/"     ;; generic SQL statement end
;;;                *don't use since "/" is also used for divison in expressions*

	  "\\|"          ;;; some keywords can also be thought of as ending
			 ;;; a statement so ...

	  plsql-white-space-re   ;;; minimal whitespace before key word --
			 ;;; needed to exclude words in identifiers

;;	  "\\("
	  (regexp-opt    ;; optimisation actually does nothing:-)
	   (list
	    "loop"       ;; delimits contents of a loop statement
	    "then"       ;; delimits contents of a conditional
	    "else"       ;; ends the previous sub-section, starts a new one
	    "as"         ;; dec section
	    "is"         ;; dec section

;;;	   "elsif"     ;; *don't use since the following "then" is the real end*

	    ) t)
;;	   "\\)"

	  "\\)"          ;;; mark end of statement excluding whitespace
	  plsql-white-space-re  ;;; minimal whitespace after key word --
			 ;;; needed to exclude words in identifiers
	  )))

(defun plsql-in-sql-block-p (limit)
  "If inside (but not just before) an SQL block, return the adjusted
  position of the start of the block the block, otherwise return nil.
  Search bound by LIMIT."
  ;; PRE: point is at start of indentation
  (save-excursion
    (let ((start (point)))
      (when (or (plsql-re-search-backward plsql-statement-end-re
					   limit t nil limit)
		 ;; very first statement is a special case
		(and (= limit 1) (goto-char 1)))

	(when (and (plsql-re-search-forward
		    ;; ensure we have a leading identifier boundary
		    (concat plsql-white-space-re plsql-sql-statement-re)
		    start t nil start)
		   (progn ;; skip bracketed sql blocks
		     (forward-char 1)
		     (not (plsql-in-parenthesis-p limit))))

	  (back-to-indentation)
	  (- (point)
	     (plsql-sql-statement-adjust))
	  )))))

(defsubst plsql-indent-to-col (target-col)
  "Ensure current indentation is TARGET-COl, but avoid altering the buffer if
  no real change need be made."
  ;; assumes point is at start of indentation
  (unless (eq (current-indentation) target-col)
    (delete-horizontal-space)
    (indent-to target-col)))

;; An SQL block is a chain of SQL statements ending in either a ")"
;; parenthesis or a ";", wiht each SQL statement starting with a key word
;; match `plsql-sql-statement-re'

(defun plsql-sql-block-indent (block-start)
  "Indent code inside sql block. BLOCK-start is the adjusted position of
  the start of the bock."
  ;; PRE CONDITION:
  ;; (1) POINT is at the current indentation of the line we are
  ;; indenting.
  ;; (2) LIMIT is the column  start of the sql statement block
  ;; (3) We are not inside a comment or a string.

  ;; POST CONDITION: line indented correctly

  (when plsql-debug
    (add-to-list 'plsql-indent-function-stack 'plsql-sql-block-indent 'append))

  (let ((block-col (save-excursion
		     (goto-char block-start)
		     (current-column))))
    (if (looking-at plsql-sql-statement-re)
	(plsql-indent-to-col (+ block-col
				(- 8 ;; length of the longest keyword

				   (length (thing-at-point 'word)))))

      ;; Indent to the same col as the first word following the previous
      ;; sql statement
      (plsql-indent-to-col (save-excursion
			     (plsql-re-search-backward
			      ;; ensure we have a leading identifier boundary
			      (concat plsql-white-space-re plsql-sql-statement-re)
			      block-start)
			     (goto-char (match-end 1))
			     (skip-chars-forward " \t")
			     (if (looking-at "[\n\r]")
				 (+ block-col plsql-indent)
			       (current-column)))))))

(defun plsql-parenthesis-indent (limit)
  "Indent code inside parenthesis group. LIMIT is the start of the group."
  ;; PRE CONDITION:
  ;; (1) POINT is at the current indentation of the line we are
  ;; indenting.
  ;; (2) LIMIT is the start of the group
  ;; (3) We are not inside a comment or a string.

  ;; POST CONDITION: line indented correctly

  (when plsql-debug
    (add-to-list 'plsql-indent-function-stack 'plsql-paren-indent 'append))

  (cond ((looking-at "(") ;; line break between statement start and open paren?
	 (plsql-indent-to-col
	  (save-excursion
	    (plsql-re-search-backward plsql-leading-identifier-re limit t nil limit)
	    ;; only fails if very first non-comment char is a (
	    (goto-char (or (match-beginning 1) 1))
	    (+ (current-column) plsql-indent))))

	((save-excursion ;; parentheses around sql block?
	   (goto-char limit)
	   (forward-char 1)
	   (skip-chars-forward " \t") ;; non-line-ending whitespace
	   ;; return column number of first non whitespace char after point.
	   ;; making an adjustment for SQL statements
	   (looking-at plsql-sql-statement-re))
	 (plsql-sql-block-indent (save-excursion
				   (goto-char (- (match-end 1) 7))
				   (current-column))))

	(t ;; plain old parentheses
	 (plsql-indent-to-col (+ (save-excursion
				   (goto-char limit)
				   (forward-char 1)
				   (skip-chars-forward " \t")
				   (current-column))
				 (save-excursion
				   (back-to-indentation)
				   (if (looking-at plsql-extra-indent-re)
				       plsql-indent
				     0)))
			      ))))

;;;_  + Section Level
;;
;; A section contains a list of statements of the following form:
;;
;;     WHITESPACE KEYWORD EXPRESSION;
;;
;; The following special cases also contain sub-sections
;;
;;     if EXPRESSION then
;;        SUB-SECTION
;;     [else]
;;        SUB-SECTION
;;     [elsif EXPRESSION then]
;;        SUB-SECTION
;;     end if;
;;
;;     [for | while] EXPRESSION loop
;;       SUB-SECTION
;;     end loop;
;;

(defvar plsql-exec-sec-stmnt-end-re ""
  "Regexp to match the end of a statements in a execution section.
The end of the 1st match should mark the boundary between statements.")

;; first call is after a call to `back-to-indentation'
;; second call immediately follows
(when (plsql-reset plsql-exec-sec-stmnt-end-re)
  (setq  plsql-exec-sec-stmnt-end-re
	 (concat
	  "\\("          ;; mark start of statement end

	  ""     ";"     ;; generic PL/SQL statement end

;;;	 "\\|"  "/"     ;; generic SQL statement end
;;;                *don't use since "/" is also used for divison in expressions*

	  "\\|"          ;;; some keywords can also be thought of as ending
			 ;;; a statement so ...

	  plsql-white-space-re   ;;; minimal whitespace before key word --
			 ;;; needed to exclude words in identifiers

;;	  "\\("
	  (regexp-opt    ;; optimisation actually does nothing:-)
	   (list
	    "loop"       ;; delimits contents of a loop statement
	    "then"       ;; delimits contents of a conditional
	    "else"       ;; ends the previous sub-section, starts a new one

;;;	   "elsif"     ;; *don't use since the following "then" is the real end*

	    ) t)
;;	   "\\)"

	  "\\)"          ;;; mark end of statement excluding whitespace
	  plsql-white-space-re  ;;; minimal whitespace after key word --
			 ;;; needed to exclude words in identifiers
	  )))


(defvar plsql-open-exec-sec-re ""
  "REGEXP matching words that start a new (sub-)section." )

;; assume that this is used by `looking-at' after a call to `back-to-indentation'
(when (plsql-reset plsql-open-exec-sec-re)
  (setq plsql-open-exec-sec-re
	(concat
;;	 "\\("
	 (regexp-opt
	  (list "if"
		"else"  ;; both an open and a close and a boundary
		"elsif"

;;;	      "when"  ;; *case statements handled by `plsql-excep-sec-indent'*

		"for"
		"while"
		"loop"  ;; infinite loops
		) t)
;;	 "\\)"
	 plsql-white-space-re  ;; minimal whitespace after key word --
	 ""             ;; needed to exclude words in identifiers
	 )))

(defvar plsql-close-exec-sec-re ""
  "REGEXP matching words that end a (sub-)section." )

;; assume that this is used by `looking-at' after a call to `back-to-indentation'
(when (plsql-reset plsql-close-exec-sec-re)
  (setq plsql-close-exec-sec-re
	(concat
	 "\\("         ;; open group of alternatives
	 "\\b"         ;; open word boundary
	 (regexp-opt
	  (list

;;;	 "when"      ;; *case statements handled by `plsql-excep-sec-indent'*

;;;	 "begin"     ;;; if we are looking at this then we should be in a
		     ;;; declaration section

	   "end"       ;;

	   "else"      ;; end conditional
	   "elsif"     ;; end conditional

	   "exception" ;; end the section  WHAT ABOUT THIS GUY?

	   ))
	 plsql-white-space-re ;; minimal whitespace after key word --
	 ""            ;; needed to exclude words in identifiers
	 "\\|"
	 "end;"        ;; one special case
	 "\\)"         ;; close group of alternatives
	 )))

(defun plsql-exec-sec-indent (limit)
  "Indent the current line given that it is inside an execution section.
LIMIT provides a bound for searches and partial scans.  The assumption
is that the context of the current line can be completely determined
without reference to any text above that point."

  ;; PRE CONDITION:
  ;; (1) POINT is at the current indentation of the line we are
  ;; indenting.
  ;; (2) The section-start at LIMIT is either:
  ;;
  ;; (a)
  ;; (b)
  ;;
  ;; POST CONDITION: current line is indented correctly.
  (when plsql-debug
    (add-to-list 'plsql-indent-function-stack 'plsql-exec-sec-indent 'append))

  ;; limit has been set to the point before the keyword "begin" or "end".

  ;; we are indenting the first statement on current line so ...
  (back-to-indentation)

  ;; First determine some properties of the previous statement

  (let ((first-statement nil)
	(prev-indent 0)            ;; indentation of the previous statement
	(prev-type 'plain)         ;; either 'open, 'close, or 'plain
	(statement-start (point))) ;; mark the beginning of this statement

    (save-excursion

      ;; The previous statement will be delimited by two preceding
      ;; statement ends, unless of course we are before the 3rd following
      ;; the limit.

      ;;---1. back up one delimiter

      (if (plsql-re-search-backward
	   plsql-exec-sec-stmnt-end-re limit t nil limit)
	  (progn
	    ;; Find the beginning of the statement we started on.
	    ;; We have to do this in two steps because of potential
	    ;; intervening comments.
	    (save-excursion
	      (goto-char (match-end 1)) ;; point is after statement delimiter
	      (when (plsql-re-search-forward
		     plsql-leading-identifier-re nil t nil limit)
		(setq statement-start (match-beginning 1))))

	    ;;---2. back up another delimiter

	    (unless (plsql-re-search-backward
		     plsql-exec-sec-stmnt-end-re limit t nil limit)

	      ;; we failed so we must have started before the 3rd statement
	      ;; after the limit

	      (goto-char limit)
	      (if (not (looking-at "end")) ;; point is before "begin"
		  (forward-word 1)         ;; set point after "begin"
		;; point is before"end"
		(setq first-statement t)

		;; oops! we really started from the 1st statement
		(setq prev-indent (- (current-indentation)
				     (plsql-sql-statement-adjust))))

	      )

	    ;; set point before the first word of "previous statement"
	    (unless first-statement
	      (if (plsql-re-search-forward
		   plsql-leading-identifier-re
		   statement-start t nil limit)
		  (progn
		    (goto-char (match-beginning 1))
		    (setq prev-indent (- (current-indentation)
					 (plsql-sql-statement-adjust)))
		    (if (looking-at plsql-open-exec-sec-re)
			(setq prev-type 'open)))

		;; Opps! First word of previous statement was block by
		;; statement-start limit.  The only case where I have seen
		;; this happen is if whe are trying to align lik the
		;; following:

		;;     if    l_inplace = 0    and l_fullscreen = 0
		;;     then  l_inplaceradio := 'inframe';
		;;     elsif l_inplace = 3    and l_fullscreen = 0
		;;     then  l_inplaceradio := 'inframe';
		;;     else  l_inplaceradio := 'fullscreen';
		;;     end if;

		(setq prev-indent (- (current-indentation)
				     (plsql-sql-statement-adjust)))
		(setq prev-type 'plain-force)
		)))

	;; so we started from the 1st statement of this section
	(goto-char limit)                 ;; point is before "begin" or "end"
	(if (looking-at "begin")
	    (setq prev-type 'open))       ;; statement begins this section

	(setq prev-indent (current-indentation))
	(forward-word 1)                  ;; point is after "begin" or "end"
	(if (plsql-re-search-forward
	     plsql-leading-identifier-re
	     nil t nil limit)
	    (goto-char (match-beginning 1)))
	(setq statement-start (point))
	))

    ;; Now we know what the previous statement was so we can indent the
    ;; current line relative to it

    (if (looking-at plsql-close-exec-sec-re)
	(if (or (eq prev-type 'open)
		(eq prev-type 'plain-force))
	    (setq prev-type 'plain) ;; one-line sub-sections
	  (setq prev-type 'close))
      (if (eq prev-type 'plain-force)
	  (setq prev-type 'plain)))

    (let ((new-indent
	   (cond ((eq prev-type 'open)  (+ prev-indent plsql-indent))
		 ((eq prev-type 'close) (- prev-indent plsql-indent))
		 (t prev-indent))))

      ;; adjust if point is on or after the start of the statement
      ;; WHEN DOES THIS FAIL?
      (when (>= (point) statement-start)
	(setq new-indent (plsql-statement-level-adjust statement-start new-indent)))

      ;; avoid altering the buffer if no real change is made
      (unless (eq (current-indentation) new-indent)
	(delete-horizontal-space)
	(indent-to new-indent)))

    )) ;; -- end plsql-exec-sec-indent

;;;_  + Declaration Section

(defvar plsql-dec-sec-stmnt-end-re ""
  "Regexp to match the end of a statements in a declaration section.
The end of the 1st match should mark the boundary between statements.")

(when (plsql-reset plsql-dec-sec-stmnt-end-re)
  (setq  plsql-dec-sec-stmnt-end-re
	 (concat
	  "\\("          ;; mark start of statement end

	  ""     ";"     ;; generic PLSQL statement end

	  ;; "/" is also used for divison in expressions
	  ;;	"\\|"  "/"     ;; generic SQL statement end

	  "\\|"          ;; some keywords also end a statement so ...

	  plsql-white-space-re  ;; minimal whitespace before key word --
	  ""             ;; needed to exclude words in identifiers

;;	  "\\("
	  (regexp-opt    ;; optimisation actually does nothing:-)
	   (list
	    "as"         ;; delimits variable declarations
	    "is"         ;; delimits variable declarations
	    ) t)
;;	  "\\)"

	  "\\)"          ;; mark end of statement excluding whitespace
	  plsql-white-space-re ;; minimal whitespace after key word --
	  ""             ;; needed to exclude words in identifiers
	  )))


(defvar plsql-dec-sec-fake-stmnt-end-re ""
  "Regexp matching the start of statements in a declaration section that are not
always ended by something that matches `plsql-dec-sec-stmnt-end-re'. Will be
compared with the first match of `plsql-leading-identifier-re'.")

(when (plsql-reset plsql-dec-sec-fake-stmnt-end-re)
  (setq plsql-dec-sec-fake-stmnt-end-re
	(concat
	 plsql-white-space-re   ;; minimal whitespace before key word --
	 ""                     ;; needed to exclude words in identifiers
	 (regexp-opt
	  (list
	   "cursor"
	   "type"
	   "subtype"
	   "procedure"
	   "function"
	   ))
	 plsql-white-space-re   ;; minimal whitespace after key word --
	 ""                     ;; needed to exclude words in identifiers
	 )))

(defvar plsql-open-dec-sec-re ""
    "REGEXP matching words that start a new declaration (sub-)section.
Note that, since there is little difference between the specification section
and the declaration section, we treat them the same way." )

(when (plsql-reset plsql-open-dec-sec-re)
  (setq plsql-open-dec-sec-re
	(concat
	 "\\b"
	 (regexp-opt
	  (list "is"
		"as"
		"procedure"
		"function"
		"trigger"
		"declare"
		))
	 plsql-white-space-re ;; minimal whitespace after key word --
	 ""             ;; needed to exclude words in identifiers
	 )))

(defvar plsql-close-dec-sec-re ""
    "REGEXP matching words that end a declaration (sub-)section.
Note that, since there is little difference between the specification section
and the declaration section, we treat them the same way.")

(when (plsql-reset plsql-close-dec-sec-re)
  (setq plsql-close-dec-sec-re
	(concat
	 "\\b"         ;; open word boundary
	 "\\("         ;; open group of alternatives
	 (regexp-opt
	  (list
	   "begin"     ;; start the actual section
	   "is"        ;; both and open and a close
	   "as"
	   ))
	 plsql-white-space-re ;; minimal whitespace after key word --
	 ""            ;; needed to exclude words in identifiers
	 "\\|"
	 "end;"        ;; one special case
	 "\\)"         ;; close group of alternatives
	 )))

(defun plsql-dec-sec-indent (limit)
  "Indent the current line given that it is inside declaration section."

  ;; PRE CONDITION:
  ;; (1) POINT is at the current indentation of the line we are
  ;; indenting.
  ;; (2) The section-start at LIMIT is either:
  ;;
  ;; (a)
  ;; (b)
  ;;
  ;; POST CONDITION: current line is indented correctly.

  (when plsql-debug
    (add-to-list 'plsql-indent-function-stack 'plsql-dec-sec-indent 'append))

  ;; This function is very similar to plsql-exec-sec-indent; separating it
  ;; out, however, vastly simplifies the logic, in particular that relating
  ;; to cursors and specification sections.

  ;; we are indenting the first statement on current line so ...
  (back-to-indentation)

  ;; First determine some properties of the previous statement

  (let ((prev-indent 0)     ;; indentation of the previous statement
	(prev-type 'plain)  ;; either 'open, 'close, or 'plain
	(statement-start (point)));; mark beginning of this statement

    (save-excursion

      ;; The previous statement is delimited by two preceding statement
      ;; ends, unless of course we are either the 1st statement in the
      ;; section

      ;;---1. back up one delimiter

      (if (and (plsql-re-search-backward
		plsql-dec-sec-stmnt-end-re limit t nil limit)
	       ;; e.g. "is" after a cursor is not a real statement delimiter
	       (if (save-excursion
		     (save-match-data
		       (and
			(plsql-re-search-backward
			 plsql-dec-sec-stmnt-end-re limit t nil limit)
			(plsql-re-search-forward
			 plsql-leading-identifier-re nil t nil limit)
			(goto-char (match-beginning 1))
			(looking-at plsql-dec-sec-fake-stmnt-end-re))))
		   (plsql-re-search-backward
		    plsql-dec-sec-stmnt-end-re limit t nil limit)
		 t))

	  (progn
	    ;; Find the statement start (we have to do this in two steps
	    ;; because of potential intervening comments).
	    (save-excursion
	      (goto-char (match-end 1)) ;; point is after statement delimiter
	      (when (plsql-re-search-forward
		     plsql-leading-identifier-re nil t nil limit)
		(setq statement-start (match-beginning 1))))

	    ;;---2. back up another delimiter

	    (if (and (plsql-re-search-backward
		      plsql-dec-sec-stmnt-end-re limit t nil limit)
		     ;; e.g. "is" after a "cursor" is not a real statement delimiter
		     (if (save-excursion
			   (and
			    (plsql-re-search-backward
			     plsql-dec-sec-stmnt-end-re limit t nil limit)
			    (plsql-re-search-forward
			     plsql-leading-identifier-re nil t nil limit)
			    (goto-char (match-beginning 1))
			    (looking-at plsql-dec-sec-fake-stmnt-end-re)))
			 (plsql-re-search-backward
			  plsql-dec-sec-stmnt-end-re limit t nil limit)
		       t))

		;; so we started after the 2nd statement of this dec lock
		(progn
		  (when (looking-at "[ \t\n\r]*[ia]s[ \t\n\r]")
		    ;; this way we end up treating specification sections
		    ;; the same as declaration sections
		    (forward-word 1))

		  ;; forward to next leading word
		  (when (plsql-re-search-forward
			 plsql-leading-identifier-re
			 statement-start t nil limit)
		    ;; if this fails we don't want any indentation:-)
		    (goto-char (match-beginning 1))
		    (setq prev-indent (- (current-indentation)
					 (plsql-sql-statement-adjust)))
		    ))

	      ;; so we started before the 3rd statement of this dec section
	      (goto-char limit)    ;; point before "is" or "as"
	      (setq prev-type 'open)
	      (setq prev-indent (current-indentation))
	      ))

	;; so we started before the first "is" or "as"
	(goto-char limit)
	(back-to-indentation)
	(setq prev-indent (current-indentation))
	(setq statement-start (point-max)) ;; don't do statement-level-indent
	(setq prev-type 'open)
	))

    ;; Now we know what the previous statement was so we can indent the
    ;; current line relative to it

    (if (looking-at plsql-close-dec-sec-re)
	(if (eq prev-type 'open)
	    (setq prev-type 'plain) ;; one-line sub-sections
	  (setq prev-type 'close)))

    (let ((new-indent
	   (cond ((eq prev-type 'open)   (+ prev-indent plsql-indent))
		 ((eq prev-type 'close)  (- prev-indent plsql-indent))
		 (t prev-indent))))

      ;; point is after the beginning of the statement
      (when (>= (point) statement-start)
	(setq new-indent (plsql-statement-level-adjust statement-start new-indent)))

      ;; avoid altering the buffer if no real change is made
      (unless (eq (current-indentation) new-indent)
	(delete-horizontal-space)
	(indent-to new-indent)))

    )) ;;-- end plsql-dec-sec-indent


;;;_  + Exception Section

;; And in the exception section:
;;
;;     when ERROR-CODE then
;;        SUB-SECTION
;;
;; The CASE statement is another special case: indentation behaves just like
;; an exception section.

(defvar plsql-except-sec-stmnt-end-re ""
  "Regexp to match the end of a statements in a case or exception section.
The end of the 1st match should mark the boundary between statements.")

(when (plsql-reset plsql-except-sec-stmnt-end-re)
  (setq plsql-except-sec-stmnt-end-re
	(concat
	 "\\("          ;; mark start of statement end

	 ""     ";"     ;; generic PLSQL statement end

	 ;; "/" is also used for divison in expressions
	 ;;	"\\|"  "/"     ;; generic SQL statement end

	 "\\|"          ;; some keywords also end a statement so ...

	 plsql-white-space-re ;; minimal whitespace before key word --
	 ""             ;; needed to exclude words in identifiers

;;	 "\\("
	 (regexp-opt    ;; optimisation actually does nothing:-)
	  (list
	   "then"       ;; delimits contents of a conditional
	   "else"       ;; delimits contents of a conditional
	   ) t)
;;	 "\\)"

	 "\\)"          ;; mark end of statement excluding whitespace
	 plsql-white-space-re  ;; minimal whitespace after key word --
	 ""             ;; needed to exclude words in identifiers
	 )))

(defvar plsql-open-execp-sec-re ""
  "REGEXP matching words that start a new case or exception (sub-)section." )

(when (plsql-reset plsql-open-execp-sec-re)
  (setq plsql-open-execp-sec-re
	(concat
;;	 "\\("
	 (regexp-opt
	  (list
	   "exception"
	   "case"
	   "when"  ;; start
	   "then"  ;; one liners
	   "if"
	   "else"
	   ) t)
;;	 "\\)"
	 plsql-white-space-re ;; minimal whitespace after key word --
	 ""             ;; needed to exclude words in identifiers
	 )))


(defvar plsql-close-except-sec-re ""
  "REGEXP matching words that end a new case or exception (sub-)section.")

(when (plsql-reset plsql-close-except-sec-re)
  (setq plsql-close-except-sec-re
	(concat
	 "\\("         ;; open group of alternatives
	 "\\b"         ;; open word boundary
	 (regexp-opt
	  (list
	   "when" ;; end one, start another
	   "else"
	   ))
	 plsql-white-space-re ;; minimal whitespace after key word --
	 ""            ;; needed to exclude words in identifiers
	 "\\)"         ;; close group of alternatives
	 )))


(defvar plsql-block-terminator ""
  "Regexp matching the term that terminates a block.")

;; assume we are after a call to `back-to-indentation'

(when (plsql-reset plsql-block-terminator)
  (setq plsql-block-terminator
	(concat
	 "end"
	 "\\("
	 ";"
	 "\\|"
	 plsql-white-space-re  ;; minimal whitespace after key word --
	 ""            ;; needed to exclude words in identifiers
	 "\\)"
	 )))

(defun plsql-except-sec-indent (limit)
  "Indent the current line given that it is inside a case or exception section."

  ;; PRE CONDITION:
  ;; (1) POINT is at the current indentation of the line we are
  ;; indenting.
  ;; (2) The section-start at LIMIT is either:
  ;;
  ;; (a)
  ;; (b)
  ;;
  ;; POST CONDITION: current line is indented correctly.

  (when plsql-debug
    (add-to-list 'plsql-indent-function-stack 'plsql-except-sec-indent 'append))

  ;; This function is very similar to plsql-exec-sec-indent; separating it
  ;; out, however, vastly simplifies the logic, in particular that relating
  ;; to ending a blocks that have exception sections.

  ;; we are indenting the first statement on current line so ...
  (back-to-indentation)

  ;; First determine some properties of the previous statement

  (let ((prev-indent 0)       ;; indentation of the previous statement
	(prev-type 'plain)    ;; either 'open, 'open-force', 'close, or 'plain
	(statement-start (point)))  ;; mark beginning of this statement

    (save-excursion
      ;; first catch the "end of block" special case;
      ;; this includes the "end of case statement" special case
      (if (looking-at plsql-block-terminator)
	  (progn
;;	    (setq statement-start (point)) ;; why?
	    (goto-char limit)
	    (setq prev-indent (current-indentation))
	    (setq prev-type 'plain))

	;; The previous statement is delimited by two preceding statement ends,
	;; unless of course we are either the 1st or 2nd statement in the
	;; section

	;;---1. back up one statement

	(if (plsql-re-search-backward
	     plsql-except-sec-stmnt-end-re limit t nil limit)
	    (progn
	      ;; Find the statement start (we have to do this in two steps
	      ;; because of potential intervening comments).
	      (save-excursion
		(goto-char (match-end 1)) ;; point is after statement delimiter
		(when (plsql-re-search-forward
		       plsql-leading-identifier-re nil t nil limit)
		  (setq statement-start (match-beginning 1))))

	      ;;--2. back up another statement above this line
	      (back-to-indentation)
	      (unless (plsql-re-search-backward
		       plsql-except-sec-stmnt-end-re limit t nil limit)
		;; we started from the 2nd statement of this section
		(goto-char limit)    ;; point before "begin"
		(forward-word 1))    ;; point is after "begin"

	      ;; set point before the first word of "previous statement"
	      (when (plsql-re-search-forward
		     plsql-leading-identifier-re statement-start t nil limit)
		;; if this fails we don't want any indentation:-)
		(goto-char (match-beginning 1))
		(setq prev-indent (- (current-indentation)
				     (plsql-sql-statement-adjust)))
		(if (looking-at plsql-open-execp-sec-re)
		    (setq prev-type 'open))
		))

	  ;; we started from the 1st statement of this section
	  (goto-char limit)
	  (skip-chars-forward " \t")      ;; point is before "case" or "exception"
	  (setq prev-indent (current-indentation))

	  (if (not (looking-at "case"))
	      (forward-word 1)            ;; point is after "exception"
	    (forward-word 1)              ;; point is after case

	    ;; This fails if you stick a comment between "case" and its
	    ;; identifier.  This case is too rare (and silly) to really
	    ;; consider:-)

	    (skip-chars-forward " \t")    ;; point is before identifier
	    (skip-chars-forward "^ \t"))  ;; point is after identifier

	  (if (plsql-re-search-forward plsql-leading-identifier-re
				       nil t nil limit)
	      (goto-char (match-beginning 1)))
	  (setq statement-start (point))
	  (setq prev-type 'open-force)    ;; previous statement begun this section
	  ))
      )

    ;; Now we know what the previous statement was so we can indent the
    ;; current line relative to it

    (if (and (looking-at plsql-close-except-sec-re))
	(if (eq prev-type 'open)
	    (setq prev-type 'plain) ;; one-line sub-sections
	  (if (eq prev-type 'open-force) ;; beginning of section never a one-liner
	      (setq prev-type 'open)
	    (setq prev-type 'close))))

    (let ((new-indent
	   (cond ((eq prev-type 'open)        (+ prev-indent plsql-indent))
		 ((eq prev-type 'open-force)  (+ prev-indent plsql-indent))
		 ((eq prev-type 'close)       (- prev-indent plsql-indent))
		 (t prev-indent))))

      ;; maybe add some extra
      (when (>= (point) statement-start)
	(setq new-indent (plsql-statement-level-adjust statement-start new-indent)))

      ;; avoid altering the buffer if no real change is made
      (unless (eq (current-indentation) new-indent)
	(delete-horizontal-space)
	(indent-to new-indent)))

    )) ;; -- end plsql-except-sec-indent

;;;_  + Package Level

(defun plsql-package-indent (limit)
  "Indent the current line given that it is outside a module
definition.  Really this is for indenting package variables, first
line of a module, and the \"begin\" at the start of a packages
executable block."

  ;; PRE CONDITION:
  ;; (1) POINT is at the current indentation of the line we are
  ;;     indenting.
  ;; (2) The section-start at LIMIT is either:
  ;;	 (a) "end" NOT followed by "if", "loop", or "case"
  ;;	 (b) "function" or "procedure" but before "is" or "as"
  ;;	 (c) "package".
  ;;
  ;; POST CONDITION: current line is indented correctly.

  (when plsql-debug
    (add-to-list 'plsql-indent-function-stack 'plsql-package-indent 'append))

  (back-to-indentation)
  (let ((new-indent
	 (save-excursion
	   (cond ((looking-at "\\<begin\\>[ \t\n\r]+\\|^[ \t]*$")
		  ;; we're starting the executable section of a package body
		  ;; containing object, so decrease the indentation level
		  (goto-char limit)
		  (- (current-indentation) plsql-indent))

		 ((looking-at "/\\|\\bend\\b")
		  ;; we're ending a package specification
		  0)

		 ((looking-at "--\\|\\b\\(procedure\\|function\\|trigger\\)\\b")

		  ;; where doing a module specification
		  ;; hmm should really indent to previous in case
		  ;; its a local procedure (god does anyone actually do that?)

		  plsql-indent)

		 ((looking-at
		   "\\<create\\>\\([ \t\n\r]+or[ \t\n\r]+replace\\)?[ \t\n\r]+\\(\\w+\\)")
		  ;; we're starting a new package body or spec
		  (if (string-match "procedure\\|function\\|trigger"
				    (match-string 1))
		      plsql-indent 0))

		 ((looking-at "package")
		  ;; old fashioned new package body or spec
		  0)

		 (t
		  ;; else we are a package variable decalaration
		  nil)))))

    (if (not new-indent)
	(plsql-dec-sec-indent limit)
      (unless (= (current-indentation) new-indent)
	(delete-horizontal-space)
	(indent-to new-indent)))
    ))

;;;_  + Top Level

;; First lets revise some PL/SQL syntax.

;; Anonymous PL/SQL blocks have the following form:
;;
;;     declare
;;        SECTION  --- variable declaration
;;     begin
;;        SECTION  --- executable statements
;;     exception
;;        SECTION  --- catch errors
;;     end;
;;
;; or minimally
;;
;;     begin
;;        SECTION  --- executable statements
;;     end;

;; Modules are essentially just named blocks --- instead of the keyword
;; "declare" they have a "specification" which takes one of the following
;; forms:

;;     [create [or replace]]
;;     procedure NAME [body] [(ARGLIST)]
;;     is
;;
;; or
;;
;;     [create [or replace]]
;;     function NAME [body] [(ARGLIST)]
;;	  return TYPE
;;     is
;;
;; (Note: "as" is a synonym for "is")

;; Now since some indentation rules only apply to specific sections, the
;; indentation logic can be vastly simplified by first establishing which
;; section the current point is in.  There are 4 sections:
;;
;; (1) specification
;; (2) declaration
;; (3) executable
;; (4) exception
;;
;; In addition there is the case when we are outside of all these
;; sections.  package level/ module level.
;; called only once but we build it outside to speed things up
(defvar plsql-top-level-flush-re ""
  "Regexp matching the start of top level statements that should be eft flush.")

(when (plsql-reset plsql-top-level-flush-re)
  (setq plsql-top-level-flush-re
	 (regexp-opt
	  (list
	   "create"
	   "begin"
	   "declare"
	   "function"
	   "procedure"
	   "trigger"
	   "package"
	   ) 'word)
	 ))

;; Not c
(defun plsql-top-level-indent (limit)
  "The last nail in the coffin for a really rare case.  Everything
outside of a package body or spec is left flushed."
  (when plsql-debug
    (add-to-list 'plsql-indent-function-stack 'plsql-top-level-indent 'append))

  (back-to-indentation)
  (plsql-indent-to-col
   (if (looking-at plsql-top-level-flush-re)
       0 ;; special case
     (plsql-statement-level-adjust (point) 0))))
;
(defvar plsql-section-start-re ""
  "Regexp matching the beginning of (or a boundary within) a new type
of section.")

;; called only once but we build it outside to speed things up
(when (plsql-reset plsql-section-start-re)
  (setq plsql-section-start-re
	(concat
	 "\\(?:"
	 "^\\|"
	 plsql-white-space-re  ;; whitespace delimits keywords
	 "\\)"
;;	 "\\("
	 (regexp-opt
	  (list
	   "end"          ;; special case: could be module, except, or exec
	   "begin"
	   "declare"
	   "function"
	   "procedure"
	   "trigger"
	   "package"
	   ;; NB: "exception" keyword is also a type so we can't use
	   ;; it as a section start keyword!
	   ) t)
;;	  "\\)"
	 "\\(;"
	 "\\|"
	 plsql-white-space-re ;; whitespace delimits keywords
	 "\\)"
	 )))

;; Not called. Could be customizable?

(defun plsql-comment-indent (&optional start)
  "Indent the current line at least as much as the previous."

  (when plsql-debug
    (add-to-list 'plsql-indent-function-stack 'plsql-comment-indent 'append))

  (let ((new-indent (save-excursion
		      (condition-case nil
			  (forward-line -1)
			(error nil))
		      (current-indentation))))
    (unless (> (current-indentation) new-indent)
      (delete-horizontal-space)
      (indent-to new-indent)))
  )

;; Modules and blocks can nest so the following needs to work as well:

;; create or replace package body NAME is
;;
;; procedure NAME
;;    is
;;
;;	 procedure NAME is
;;	 begin
;;          SECTIONS
;;	 end NAME;
;;
;;	 procedure NAME is
;;	 begin
;;          SECTIONS
;;	 end NAME;
;;
;;	 ...
;;
;;    begin
;;
;;	begin
;;         STATEMENT
;;	end;
;;
;;	begin
;;         STATEMENT
;;	end;
;;
;;	...
;;
;;    end NAME;
;;
;; end NAME;

(defun plsql-after-end-indent (limit)
  "Indent line following a an \"end\" keyword that begins at
LIMIT.  Determines the context and passes the job on to the appropriate
indentation function."

  ;; PRE CONDITION:
  ;; (1) POINT is at the current indentation of the line we are
  ;;     indenting.
  ;; (2) The word starting at LIMIT is "end"
  ;;
  ;; POST CONDITION: current line is indented correctly.

 (when plsql-debug
    (add-to-list 'plsql-indent-function-stack 'plsql-after-end-indent 'append))

  (let (indent-function match)
;;    (if (looking-at ""

    (if (save-excursion
	  (goto-char (+ limit 3)) ;; after "end"
	  (skip-chars-forward " \t\n\r") ;; whitespace
	  (looking-at "\\(if\\|loop\\|case\\)[ \t\n\r]*;"))
	;; after a compound statement (most common case)
	(setq indent-function 'plsql-exec/except-sec-indent)

      ;; find the previous section start with indentation level less
      ;; than that of the "end" keyword

      (save-excursion
	(goto-char limit)
	(let ((end-indent (current-indentation)))
	  (while (and
		  (> end-indent 0)
		  (plsql-re-search-backward plsql-section-start-re nil t nil nil)
		  (goto-char (match-beginning 1))
		  (>= (current-indentation) end-indent)))))

	(setq match (downcase (match-string 1)))
	(setq limit (match-beginning 1)) ;; reset the limit

	(cond ((string-match "begin\\|end" match)
	       ;; after an anonymous block inside an executable section
	       ;; can we have anonymous blocks inside exception sections?
	       (setq indent-function 'plsql-exec-sec-indent))

	      ((or (string-equal "procedure" match)
		   (string-equal "function" match))
	       ;; after a module declaration _inside_ a declaration section
	       (setq indent-function 'plsql-dec-sec-indent))

	      ((string-equal "package" match)
	       ;; after a module declaration _inside_ a package
	       (setq indent-function 'plsql-package-indent))

	      (t
	       ;; unpackaged block or
	       (message "unpackaged block")
	       (setq indent-function 'plsql-top-level-indent))
	      ))

    (funcall indent-function limit)))


(defun plsql-exec/except-sec-indent (limit)
  "Special case: exception keyword is also a type so we can't use it
as a section start keyword. We have to do this outside of the first
save-excursion of `plsql-indent'."

  ;; PRE CONDITION:
  ;; (1) POINT is at the current indentation of the line we are
  ;; indenting.
  ;; (2) The section-start at LIMIT is either:
  ;;
  ;; (a) "begin", or
  ;; (b) "end" followed by "if", "loop", or "case"
  ;;
  ;; so we are either in an executable or exception-like
  ;; section. Since a "case" construct behaves just like an exception
  ;; section, we pass that case (no pun intended:-) onto
  ;; `plsql-except-sec-indent'.

  ;;
  ;; POST CONDITION: current line is indented correctly.

   (when plsql-debug
    (add-to-list 'plsql-indent-function-stack 'plsql-exec/except-sec-indent 'append))

  (let ((indent-function 'plsql-exec-sec-indent)) ;; the default

    (save-excursion ;; override the default?
      (back-to-indentation)
      (when (plsql-re-search-backward "[ \t\n\r]\\(exception\\|case\\)[ \t\n\r]"
				      limit t nil limit)
	(setq limit (match-beginning 1)) ;; new limit
	(unless (and (string-equal "case" (match-string 1))
		     (back-to-indentation)
		     (looking-at "end"))
	  ;; really inside an exception like section
	  (setq indent-function 'plsql-except-sec-indent))
	))
    (funcall indent-function limit)
    ))


(defun plsql-indent ()
  "Indent the current line appropriate to the current structural unit."
  (interactive)

  (let ((target-col nil)
	(target-start nil)
	(indent-function nil)
	(limit 1)
	(match nil)
	(in-comment-p nil)
	(in-string-p nil)
	(case-fold-search t));; ignore case in searches

    ;; first guess at indentation context
    ;; and set search limits
    (save-excursion
      (back-to-indentation)
      (cond (;; skip comments
	     (and (funcall plsql-in-comment-predicate 1)
		  (not (looking-at "--"))) ;; pass this comment starter on
	     (setq in-comment-p t)) ;; do nothing for the moment

	    (;; skip strings
	     (and (funcall plsql-in-string-predicate 1)
		  (not (looking-at "'")))  ;; pass on string starters
	     (setq in-string-p t)) ;; do nothing for the moment

	    (;; indent after first block starter
	     (plsql-re-search-backward
	      plsql-section-start-re nil t nil nil)

	     ;; determine section type and pass on to appropriate
	     ;; indentation function
	     (setq match (downcase (match-string 1)))

	     ;; get a preliminary bound for searches and parsing
	     (setq limit (match-beginning 1))

	     (cond ((string-equal "end" match) ;; most common match
		    (setq indent-function 'plsql-after-end-indent))

		   ((string-equal "begin" match)
		    ;; inside an execution or exception type section
		    (setq indent-function 'plsql-exec/except-sec-indent))

		   ;; inside a declaration section
		   ((string-equal "declare" match)
		    (setq indent-function 'plsql-dec-sec-indent))

		   ;; inside either a declaration section or a
		   ;; package specification
		   ((or (string-equal "procedure" match)
			(string-equal "function" match))

		    (plsql-re-search-forward
		     plsql-dec-sec-stmnt-end-re
		     nil t nil limit)

		    (if (string-match "is\\|as" (or (match-string 1) "dummy"))
			;; it really a declaration section
			(setq indent-function 'plsql-dec-sec-indent)
		      ;; no its really a package specification
		      (setq indent-function 'plsql-package-indent)))

		   ;; inside a package definition but before any declarations
		   ((string-equal "package" match)
		    (setq indent-function 'plsql-package-indent))
		   ))

	    (t ;; indent before first block starter
	     ;; so we might be looking at block-less PL/SQL
	     (setq limit 1)
	     (cond ((plsql-re-search-backward
		     plsql-exec-sec-stmnt-end-re nil t nil nil)
		    ;; bare pl/sql
		    (setq indent-function 'plsql-exec/except-sec-indent))

		   ((looking-at plsql-section-start-re)
		    (setq indent-function 'plsql-package-indent))

		   (t ;; start of bare pl/sql
		    (setq indent-function 'plsql-top-level-indent)
		    )))
	    ))

    (setq plsql-indent-function-stack nil)
    ;; We deal with parentheses and sql blocks differently. We can't move these
    ;; tests higher because they really need the search limit to be set first.

    (save-excursion
      (back-to-indentation)
      (when (and (not in-comment-p) (not in-string-p))
	(cond (;; parenthesis
	       (setq target-col (plsql-in-parenthesis-p limit))
	       (setq indent-function 'plsql-parenthesis-indent)
	       (setq limit target-col))

	      (;; SQL block
	       (setq target-start (plsql-in-sql-block-p limit))
	       (setq indent-function 'plsql-sql-block-indent)
	       (setq limit target-start))
	      ))

      (when indent-function
	(when plsql-debug
	  (add-to-list 'plsql-indent-function-stack indent-function 'append))
	(funcall indent-function limit)))

    ;; maybe reposition cursor to the start of the indentation
    ;; (only happens when indenting a blank line)
    (if (= (current-column) 0) (skip-chars-forward " \t"))
    (when plsql-debug
      ;; print a pseudo stack trace
      (message "%s"
	       (mapconcat
		(lambda (x)
		  (when x (substring (substring (symbol-name x) 6) 0 -7)))
		plsql-indent-function-stack
		" -> ")))
    ))

;;;_  + Imenu

(eval-and-compile (require 'imenu)) ;; quieten compiler

(defvar  plsql-imenu-title "Contents"
  "*Title of the menu which will be added to the menu bar.")

(defvar plsql-imenu-regexp ""
  "*A regular expression matching a head line to be added to the menu.")

(when (plsql-reset plsql-imenu-regexp)
  (setq plsql-imenu-regexp
	(concat
	 "\\(?:"
	 "^\\|"
	 plsql-white-space-re
;;	 "[ \t\n]*" ;; leading whitespace
	 "\\)"
	 "\\("
	 ""     "package"
	 "\\|"  "function"
	 "\\|"  "procedure"
	 "\\)"
	 plsql-white-space-re ;; trailing whitespace
	 )))

;; Thanks to Bret (?) for spotting this one
(eval-when-compile (require 'cl))

;; This doesn't work? Why?
;;
;;(defun push (obj stack)
;;  "Poor man's push (rather than requiring 'cl)."
;;  (setf stack (cons obj stack)))

;; Make an index for imenu
(defun plsql-imenu-index ()
  "Return an table of contents for an PL/SQL buffer for use with Imenu."
  (interactive)
  (let ((case-fold-search t)
	(toc-string nil)
	(procedure-alist '())
	(variable-alist  '())
	(function-alist  '())
	(type-alist      '())
	(package-alist   '())
	(index-alist     '())
	(in-body nil)
	(body-defn nil)
	(spec-defn nil)
	(package nil)
	prev-pos ;; dummy used by imenu-progress-message
	match start end indent)

    (save-excursion
      (goto-char (point-max))
      (imenu-progress-message prev-pos 0)
      (while (and (plsql-re-search-backward plsql-imenu-regexp nil t)
		  (not (funcall plsql-in-string-predicate 1)))

	(imenu-progress-message prev-pos nil t)
	(setq end (match-end 1)
	      start (match-beginning 1)
	      match (downcase (match-string 1)))
	(goto-char start)
	(beginning-of-line)
	(setq indent (/ (current-indentation) plsql-indent))
	;; extra indent for local modules
	(setq indent (if (< indent 2) ""
		       (make-string (* (- indent 1) 2) ?\ )))
	(goto-char end)
	(save-excursion
	  (skip-chars-forward "[ \t\n\r]+")
	  (when (looking-at "body")
	    (setq in-body t)
	    (forward-word 1)
	    (skip-chars-forward "[ \t\n\r]+"))
	  (setq toc-string (concat indent (thing-at-point 'symbol)))
	  (cond ((string-equal match "procedure")
		 (push (cons toc-string (point)) procedure-alist))
		((string-equal match "function")
		 (push (cons toc-string (point)) function-alist))
		((string-equal match "type")
		 (push (cons toc-string (point)) type-alist))

		((string-equal match "package")

		 (if in-body
		     (progn
		       (setq package (concat toc-string" body"))
		       (setq body-defn t))
		   (setq package (concat toc-string" spec"))
		   (setq spec-defn t))

		 ;; try to minimise the number of sublists
		 (if procedure-alist
		     (if (or function-alist type-alist)
			 (push (cons "Procedures" procedure-alist) package-alist)
		       (setq package-alist procedure-alist)))

		 (if function-alist
		     (if (or procedure-alist type-alist)
			 (push (cons "Functions" function-alist) package-alist)
		       (setq package-alist function-alist)))

		 (if type-alist
		     (if (or procedure-alist function-alist)
			 (push (cons "Types" type-alist) package-alist)
		       (setq package-alist type-alist)))

		 (when package-alist ;; don't do it if empty
		   (push (cons package package-alist) index-alist))

		 (setq procedure-alist '()
		       function-alist  '()
		       type-alist      '()
		       package-alist   '()
		       in-body nil))

		(t
		 (push (cons toc-string (point)) variable-alist)
		 ))
	  )
	(goto-char start)
	))
    (imenu-progress-message prev-pos 100)

    (unless package
      ;; else objects are package less
      (if procedure-alist
	  (if (or function-alist type-alist)
	      (push (cons "Procedures" procedure-alist) index-alist)
	    (setq index-alist procedure-alist)))

      (if function-alist
	  (if (or procedure-alist type-alist)
	      (push (cons "Functions" function-alist) index-alist)
	    (setq index-alist function-alist)))

      (if type-alist
	  (if (or procedure-alist function-alist)
	      (push (cons "Types" type-alist) index-alist)
	    (setq index-alist type-alist))))

    (unless (and body-defn spec-defn)
      ;; pop off the superfluous identifier
      (setq index-alist (cdr (car index-alist))))

    index-alist))

(defun plsql-imenu-setup ()
  "*Setup the variables to support imenu."
  (interactive)
  (setq imenu-case-fold-search t)
  (setq imenu-sort-function nil) ;; sorting the menu defeats the purpose
  (setq imenu-create-index-function 'plsql-imenu-index)
  (imenu-add-to-menubar plsql-imenu-title)
  )

;;;_  + Align

;; Should I make so that anything that is highlighted will line up?
;; Should we make a block anything inside ()?

(eval-and-compile

  (defcustom plsql-align-rules-list '()  ""
    :group 'plsql
    :type  'align-rules-list-type)

  ;; Should I make so that anything that is highlighted will line up?
  ;; Should we make a block anything inside ()?

  (when (condition-case nil
	    (require 'align)
	  (error nil))

    ;; these are way too slow to use with indent before aligning
    (unless (and  plsql-align-rules-list plsql-debug)
      (setq plsql-align-rules-list
	    '(
	      (plsql-assignment
	       (regexp . "\\(\\s-*\\):=\\(\\s-*\\)")
	       (group  . (1 2))
	       (modes  . '(plsql-mode))
	       (repeat t)
	       (tab-stop  .  nil))

	      (plsql-arrorw
	       (regexp . "\\(\\s-*\\)=>\\(\\s-*\\)")
	       (group  . (1 2))
	       (modes  . '(plsql-mode))
	       (repeat t)
	       (tab-stop  .  nil))

	      (plsql-equals ;; exclude the previous two cases
	       (regexp . "\\(\\s-*[^:]\\)=\\([^>]\\s-*\\)")
	       (group  . (1 2))
	       (repeat t)
	       (tab-stop . nil)
	       (modes    . '(plsql-mode)))

	      (plsql-operator ;; watch out for comments
	       (regexp . "\\(\\s-*\\)[-+/]{1}\\(\\s-*\\)")
	       (group  . (1 2))
	       (repeat t)
	       (tab-stop . nil)
	       (modes    . '(plsql-mode)))

	      (plsql-keywords
	       (regexp . "\\(\\s-+\\)\\(in\\|default\\|number\\|varchar2\\|blob\\|raw\\)\\b")
	       (group 1)
	       (repeat t)
	       (case-fold t)
	       (tab-stop . nil)
	       (modes    . '(plsql-mode)))
	      )
	    ))

    (put 'plsql-align-rules-list 'risky-local-variable t)
    (add-to-list 'align-c++-modes 'plsql-mode) ;; eg expression functions ...
    (add-to-list 'align-sq-string-modes 'plsql-mode)
    (add-to-list 'align-open-comment-modes 'plsql-mode)

    ;; Should we re-bind new-line-and-indent to align the current
    ;; region? That sounds expensive.
    ))

;;;_  + Font-Lock

(defvar plsql-oracle-font-lock-fix-re "")

(when (or  (null plsql-oracle-font-lock-fix-re)  plsql-debug)
  (setq plsql-oracle-font-lock-fix-re
	(list (cons
	       ;; these guys would otherwise be in font-lock-function-name-face
	       (concat
		"\\b" "\\(" (regexp-opt
			     (list
			      "if"
			      "then"
			      "when"
			      "else"
			      "elsif"
			      "begin"
			      "end"
			      "loop"
			      "for"
			      "while"
			      "return"
			      "exit"
			      )) "\\)" "\\b")
	       'font-lock-keyword-face)
	      (cons
	       (concat
		"\\b" "\\(" (regexp-opt
		       (list
			"true"
			"false"
			"number"
			"raw"
			)) "\\)" "\\b")
	       'font-lock-type-face)

	      (cons
	       (concat
		"\\b" "\\(" (regexp-opt
			     (list
			      "open"
			      "fetch"
			      "close"
			      "count"
			      )) "\\)" "\\b")
	       'font-lock-builtin-face)

	      ;; types and properties
	      (cons
	       "%[_#:$a-z,A-Z]+"
	       'font-lock-constant-face)

	      ;; err should probably use an anchored highlight
	      (cons
	       (concat
		"\\b"
		"\\(" "function"
		"\\|" "procedure"
		"\\|" "package body"  ;; ick damn those groupings
		"\\|" "package"
		"\\)"
		plsql-white-space-re
		"\\([_#:$a-z,A-Z]+\\)")
	       (list
		'(1 font-lock-type-face)
		'(2 font-lock-function-name-face))
	       )

	      ;; this guy is bad
	      (cons
	       (concat
		"\\b" "\\(" (regexp-opt
			     (list
			      "language"
			      )) "\\)" "\\b")
	       ''default)
	      )))

;;;_  + Mode

(eval-when-compile (require 'sql)) ;; quieten compiler

(defun plsql-indent-region (beg end)
  "Indent the region between BEG and END with a progress display."
  (interactive "*r")
  (goto-char beg)
  (let* ((line-count (count-lines beg end))
	 (lines-indented 0)
	 (lines-remaining line-count)
	 (endmark (copy-marker end)))
    (while (< (point) endmark)
      ;; Report % progress every every 40 lines
      (when (> lines-indented 39)
	(setq lines-remaining (- lines-remaining lines-indented)
	      lines-indented 0)
	(message "Indenting region...(%d%%)"
		 (/ (* (- line-count lines-remaining) 100) line-count)))
      (plsql-indent)
      (forward-line 1)
      (setq lines-indented (1+ lines-indented)))
    (message "Indenting region...done")))

(defun plsql-mode ()
  "Programming support mode for PL/SQL code."

  (interactive)
  (require 'sql)

;;   (modify-syntax-entry ?# "w" sql-mode-syntax-table)
;;   (modify-syntax-entry ?_ "w" sql-mode-syntax-table)
;;   (modify-syntax-entry ?$ "w" sql-mode-syntax-table)

  (setq sql-mode-font-lock-keywords
	(append plsql-oracle-font-lock-fix-re ;; override some bad bits
		sql-mode-oracle-font-lock-keywords))
  (setq font-lock-mark-block-function 'mark-visible)
  (sql-mode)

  (setq major-mode 'plsql-mode)
  (setq mode-name "PL/SQL")

  (if plsql-uses-font-lock
      (progn
	(setq plsql-in-comment-predicate  'plsql-comment-face-p)
	(setq plsql-in-string-predicate   'plsql-string-face-p))
    (setq plsql-in-comment-predicate  'plsql-in-comment-p)
    (setq plsql-in-string-predicate   'plsql-in-string-p))

  (plsql-imenu-setup)

  (set (make-local-variable 'indent-line-function) 'plsql-indent)
  (set (make-local-variable 'indent-region-function) 'plsql-indent-region)
  (set (make-local-variable 'align-mode-rules-list) 'plsql-align-rules-list)
  (local-set-key [(return)] 'newline-and-indent)
  (run-hooks 'plsql-mode-hook)
  )

(provide 'plsql)

;;; plsql.el ends here

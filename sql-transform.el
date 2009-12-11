;;; sql-transform.el --- transform SQL statements

;; Copyright (C) 1998-2000  Alexander Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 2.2.0
;; Keywords: SQL

;; This file is NOT part of GNU Emacs.

;; sql-transform.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; sql-transform.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code provides three things:

;; 1. INSERT, SELECT, UPDATE, and DELETE statements can be converted
;;    into each other:  Place point within a SELECT statement and
;;    type M-x sql-to-update RET in order to transform the SELECT
;;    statement into an UPDATE statement.

;; 2. SQL statement pretty printer:  Place point within a SELECT 
;;    statement and type M-x sql-to-select RET in order to rewrite
;;    the SELECT statement.

;; This is usefull if you are developping code that needs a set of
;; INSERT, SELECT, UPDATE, and DELETE statements for the same table.
;; This set of functions will parse any of these four SQL statements and
;; rewrite them for you (if they are simple, eg. no joins to other
;; tables).  Just mark a SQL statement and call sql-to-insert,
;; sql-to-select, sql-to-update, or sql-to-delete.

;; Obviously this works best if you start with a SELECT or UPDATE
;; statement, because DELETE statements don't have columns and
;; bindvariables, and INSERT statements don't have where clauses.

;; A possible way to customize your .emacs would be to use the following
;; (assuming you are using sql-mode.el from the page mentioned above):

;; (add-hook 'sql-mode-hook
;; 	  (function (lambda ()
;; 		      (local-set-key "\C-cu" 'sql-to-update))))

 

;;; Customize

(defcustom sql-into-clause-at-the-end nil
  "*If t, add INTO clause at the end of a SELECT statement.
If nil, add INTO clause before the FROM clause."
  :type 'boolean
  :group 'SQL)

(defvar sql-keyword-regexp "[ \t\n]*\\(from\\|into\\|where\\|group\\|order\\|set\\)\\b"
  "Regexp matching SQL keywords within a SQL statement.

This is used to end the parsing of a SET clause in UPDATE statements, a
column list in SELECT statements, etc.  See also `sql-end-where-regexp'.")

(defvar sql-end-where-regexp "\\binto\\b"
  "Regexp matching the end of a WHERE clause.
A WHERE clause is also ended by `sql-statement-end-regexp'.

This doesn't include GROUP BY and ORDER BY clauses because these are
included in the WHERE clause for simplicity.")

(defvar sql-statement-start-regexp "\\b\\(select\\|insert\\|update\\|delete\\)\\b"
  "Regexp matching SQL keywords starting a SQL statement.
This is used in `sql-identify-statement'.

If you want to add more keywords, be sure to define a sql-to-KEYWORD and
sql-parse-KEYWORD function.  Those functions will be called from
`sql-parse-statement' and `sql-rewrite'")

(defvar sql-statement-end-regexp "[ \t\n]*\\([;)]\\|\n\n\\|\n\n?\\'\\|\\'\\)"
  "Regexp matching the end of a SQL statement.")

(defvar sql-insert-newline-regexp "\\b\\(and\\|or\\)\\b"
  "Regexp that matches newline insertion points within a WHERE clause.

See also `sql-insert-newline-and-indent-regexp'.  Leading whitespace is
not necessary as it is stripped in `sql-parse-where'.")

(defvar sql-insert-newline-and-indent-regexp "([ \t\n]*select\\b"
  "Regexp that matches newline-and-indent points within a WHERE clause.

See also `sql-insert-newline-regexp'.  Leading whitespace is not
necessary as it is stripped in `sql-parse-where'.  The expression will
be indented by `sql-indent-string' and rewritten later using `sql-rewrite'.")

(defvar sql-indent-string "    "
  "String used to indent an expression.
This expression is matched by `sql-insert-newline-and-indent-regexp' in
`sql-parse-where'.")

;;; Code:

(defun sql-to-update ()
  "Convert SQL statement at point to an UPDATE statement.
Any SQL statement that can by parsed by `sql-parse-statement' will do.

The boundaries of the SQL statement are guessed by
`sql-statement-find-start' and `sql-statement-find-end'."
  (interactive)
  (let* ((start (sql-statement-find-start))
	 (end (sql-statement-find-end start))
	 (statement (sql-parse-statement start end))
	 (table (car (nth 0 statement)))
	 (where (nth 1 statement))
	 (columns (nth 2 statement))
	 (bindvars (nth 3 statement))
	 (target-column))
    (goto-char start)
    (setq target-column (current-column))
    (kill-region start end)
    (insert "UPDATE " table " SET")
    (while columns
      (newline 1)
      (indent-to target-column)
      (insert (format "  %s = %s" (car columns) (car bindvars)))
      (if (cdr columns)
	  (insert ","))
      (setq columns (cdr columns)
	    bindvars (cdr bindvars)))
    (if where
	(progn
	  (newline 1)
	  (indent-to target-column)
	  (sql-insert-where-clause where target-column)))))

(defun sql-to-insert ()
  "Convert SQL statement at point to an INSERT statement.
Any SQL statement that can by parsed by `sql-parse-statement' will do.

The boundaries of the SQL statement are guessed by
`sql-statement-find-start' and `sql-statement-find-end'."
  (interactive)
  (let* ((start (sql-statement-find-start))
	 (end (sql-statement-find-end start))
	 (statement (sql-parse-statement start end))
	 (table (car (nth 0 statement)))
	 (where (nth 1 statement))
	 (columns (nth 2 statement))
	 (bindvars (nth 3 statement))
	 (no-newlines (< (length columns) 6))
	 (target-column))
    (goto-char start)
    (setq target-column (current-column))
    (kill-region start end)
    (insert "INSERT INTO " table " (")
      (if no-newlines
	  (insert " ")
	(newline 1)
	(indent-to target-column)
	(insert " "))
    (insert (car columns))
    (setq columns (cdr columns))
    (while columns
      (insert ",")
      (if no-newlines
	  (insert " ")
	(newline 1)
	(indent-to target-column)
	(insert " "))
      (insert (car columns))
      (setq columns (cdr columns)))
    (insert " )")
    (newline 1)
    (indent-to target-column)
    (insert "VALUES ( " (car bindvars))
    (setq bindvars (cdr bindvars))
    (while bindvars
      (insert ",")
      (if no-newlines
	  (insert " ")
	(newline 1)
	(indent-to target-column)
	(insert " "))
      (insert (car bindvars))
      (setq bindvars (cdr bindvars)))
    (insert " )")))

(defun sql-to-select ()
  "Convert SQL statement at point to a SELECT statement.
Any SQL statement that can by parsed by `sql-parse-statement' will do.

The boundaries of the SQL statement are guessed by
`sql-statement-find-start' and `sql-statement-find-end'."
  (interactive)
  (let* ((start (sql-statement-find-start))
	 (end (sql-statement-find-end start))
	 (statement (sql-parse-statement start end))
	 (tables (nth 0 statement))
	 (where (nth 1 statement))
	 (columns (nth 2 statement))
	 (bindvars (nth 3 statement))
	 (no-newlines (< (length columns) 6))
	 (target-column))
    (goto-char start)
    (setq target-column (current-column))
    (kill-region start end)
    (insert (format "SELECT %s" (car columns)))
    (setq columns (cdr columns))
    (while columns
      (insert ",")
      (if no-newlines
	  (insert " ")
	(newline 1)
	(indent-to target-column)
	(insert " "))
      (insert (car columns))
      (setq columns (cdr columns)))
    (newline 1)
    (indent-to target-column)
    (if (null sql-into-clause-at-the-end)
	(sql-insert-into-clause bindvars no-newlines target-column))
    (insert "FROM " (car tables))
    (setq tables (cdr tables))
    (while tables
      (insert ", " (car tables))
      (setq tables (cdr tables)))
    (if where
	(progn
	  (newline 1)
	  (indent-to target-column)
	  (sql-insert-where-clause where target-column)))
    (if sql-into-clause-at-the-end
	(progn 
	  (newline 1)
	  (indent-to target-column)
	  (sql-insert-into-clause bindvars no-newlines target-column)))))

(defun sql-insert-into-clause (bindvars no-newlines target-column)
  "Insert INTO clause for SELECT statements.
First parameter BINDVARS contains a list of bindvars to use.
If second parameter NO-NEWLINES is t, add no newlines between bindvars.
Third parameter TARGET-COLUMN says where to indent bindvars to.  Point
should be at this column when this function is called.
See `sql-to-select' for more information."
  (if bindvars
      (progn
	(insert (format "INTO %s" (car bindvars)))
	(setq bindvars (cdr bindvars))
	(while bindvars
	  (insert ",")
	  (if no-newlines
	      (insert " ")
	    (newline 1)
	    (indent-to target-column)
	    (insert " "))
	  (insert (car bindvars))
	  (setq bindvars (cdr bindvars)))
	(newline 1)
	(indent-to target-column))))

(defun sql-to-delete ()
  "Convert SQL statement at point to a DELETE statement.
Any SQL statement that can by parsed by `sql-parse-statement' will do.

The boundaries of the SQL statement are guessed by
`sql-statement-find-start' and `sql-statement-find-end'."
  (interactive)
  (let* ((start (sql-statement-find-start))
	 (end (sql-statement-find-end start))
	 (statement (sql-parse-statement start end))
	 (table (car (nth 0 statement)))
	 (where (nth 1 statement))
	 (target-column))
    (goto-char start)
    (setq target-column (current-column))
    (kill-region start end)
    (insert "DELETE FROM " table)
    (if where
	(progn
	  (newline 1)
	  (indent-to target-column)
	  (sql-insert-where-clause where target-column)))))

(defun sql-insert-where-clause (lines target-column)
  "Insert WHERE clause at point.
First parameter LINES is a list of lines, second parameter
TARGET-COLUMN is the target column at which the WHERE string
should start."
  (let ((line)
	(markers)
	(first t))
    (insert "WHERE ")
    ;; first insert all the lines, creating markers where rewrites
    ;; should take place
    (while lines
      (if first
	  (setq first nil)
	(newline 1)
	(indent-to target-column))
      (setq line (car lines)
           lines (cdr lines))
      (insert line)
      ;; upcase first word
      (save-excursion
	(beginning-of-line)
	(upcase-word 1))
      ;; remove trailing whitespace - leading whitespace is significant
      (while (char-equal (char-before) 32)
	(delete-char -1))
      ;; place marker if current line should be rewritten later
      (if (string-match (concat "^" sql-indent-string) line)
	  (add-to-list 'markers 
		       (set-marker (make-marker) 
				   (save-excursion (search-backward ")"))))))
    ;; rewrite selected parts
    (while markers
      (goto-char (car markers))
      (set-marker (car markers) nil)
      (setq markers (cdr markers))
      (sql-rewrite))))

(defun sql-rewrite ()
  "Rewrite SQL statement at point.
Any SQL statement that can by parsed by `sql-parse-statement' will do.
This works like a pretty printer of SQL statements.

The boundaries of the SQL statement are guessed by
`sql-statement-find-start' and `sql-statement-find-end'."
  (interactive)
  (let* ((start (sql-statement-find-start))
	 (end (sql-statement-find-end start))
	 (type (sql-identify-statement start end))
	 (func (intern-soft (concat "sql-to-" type))))
    (if func
	(funcall func)
      (error "%s statements cannot be rewritten" (upcase type)))))

 

(defun sql-statement-find-start ()
  "Return start of SQL statement at point.
This uses `sql-statement-start-regexp'."
  (save-excursion 
    (skip-syntax-forward " ")
    (while (and (not (looking-at sql-statement-start-regexp))
		(> (point) (point-min)))
      (forward-sexp -1))
    (if (looking-at sql-statement-start-regexp)
	(point)
      (error "Cannot find start of SQL statement."))))
      
(defun sql-statement-find-end (start)
  "Return end of SQL statement starting at START.
This uses `sql-statement-end-regexp'."
  (save-excursion
    (goto-char start)
    (while (not (looking-at sql-statement-end-regexp))
      (forward-sexp))
    (point)))

 

(defun sql-identify-statement (start end)
  "Identify the SQL statement between START and END.
The two arguments START and END are character positions.
The function returns a downcased string containing the SQL statement 
type, eg. \"select\".

This uses `sql-statement-start-regexp'."
  (goto-char start)
  (or (re-search-forward sql-statement-start-regexp end t)
      (error "No SQL statement found"))
  (downcase (match-string 1)))

(defun sql-parse-statement (start end)
  "Parse a SQL statement and return the parsed statement.
The two arguments START and END are character positions.

The value returned is a list (TABLES WHERE COLUMNS BINDVARS).  TABLES is
the list of tables, WHERE is the where clause of the statement, COLUMNS
is a list of column names, and BINDVARS is a list of bindvariables.
BINDVARS may be an empty list.

A possible result would be
\((\"EMP\") \"WHERE EMPNO = 1\" (\"EMPNO\" \"NAME\") (\"v_empno\", \"v_name\"))"
  (goto-char start)
  (let* ((type (sql-identify-statement start end))
	 (func (intern-soft (concat "sql-parse-" type))))
    (if func
	(funcall func start end)
      (error "%s statements cannot be parsed" (upcase type)))))

 

(defun sql-parse-select  (start end)
  "Parse the region for a SELECT statement and return the parsed statement.
The two arguments START and END are character positions.
See `sql-parse-statement' for more information."
  (let ((columns)
	(bindvars)
	(tables)
	(where))
    ;; columns: get comma separated list after SELECT keyword.  Use
    ;; (point) instead of START as a parameter to `sql-parse-csv' in
    ;; order to exclude the SELECT keyword.
    (goto-char start)
    (if (sql-find-regexp "[ \t\n]*\\bselect\\b" end)
	(setq columns (sql-parse-csv (point) end)))
    ;; bindvars: get comma separated list after INTO keyword.
    (goto-char start)
    (if (sql-find-regexp "[ \t\n]*\\binto\\b" end)
	(setq bindvars (sql-parse-csv (point) end)))
    ;; test for equal numbers
    (if (and bindvars
	     (not (= (length columns)
		     (length bindvars))))
	(message "Parser warning: not the same number of columns and bindvars"))
    ;; tables: get comma separated list after FROM keyword.
    (goto-char start)
    (if (sql-find-regexp "[ \t\n]*\\bfrom\\b" end)
	(setq tables (sql-parse-csv (point) end))
      (error "FROM clause missing"))
    ;; where clause: get everything after the WHERE keyword.
    (goto-char start)
    (if (sql-find-regexp "[ \t\n]*\\bwhere\\b" end)
	(setq where (sql-parse-where (point) end)))
    (list tables where columns bindvars)))

(defun sql-parse-insert  (start end)
  "Parse the region for an INSERT statement and return the parsed statement.
The two arguments START and END are character positions.
See `sql-parse-statement' for more information."
  (let ((columns)
	(bindvars)
	(table))
    ;; table: get string after INSERT INTO keywords.
    (goto-char start)
    (if (sql-find-regexp "[ \t\n]*\\binsert[ \t\n]+into[ \t\n]+\\(\\(\\sw\\|\\s_\\)+\\)" end)
	(setq table (list (match-string 1)))
      (error "INTO clause missing"))
    ;; columns: get comma separated list after the opening bracket.  Use
    ;; (point) instead of START as a parameter to `sql-parse-csv' in
    ;; order to exclude the table name.
    (if (re-search-forward "\\=[ \t\n]*(" end t)
	(setq columns (sql-parse-csv (point) end)))
    ;; bindvars: get comma separated list after "VALUES (" keyword.
    (goto-char start)
    (if (sql-find-regexp "[ \t\n]*\\bvalues[ \t\n]*(" end)
	(setq bindvars (sql-parse-csv (point) end)))
    ;; test for equal numbers
    (if (not (= (length columns)
		(length bindvars)))
	(message "Parser warning: not the same number of columns and bindvars"))
    (list table nil columns bindvars)))

(defun sql-parse-update  (start end)
  "Parse the region for an UPDATE statement and return the parsed statement.
The two arguments START and END are character positions.
See `sql-parse-statement' for more information."
  (let ((columns)
	(bindvars)
	(table)
	(where))
    ;; table: get string after UPDATE keyword.
    (goto-char start)
    (if (sql-find-regexp "[ \t\n]*\\bupdate\\b" end)
	(setq table (sql-parse-csv (point) end))
      (error "Table missing"))
    ;; columns/bindvars
    (if (sql-find-regexp "[ \t\n]*\\bset\\b" end)
	(let ((assignments (sql-parse-assignments (point) end)))
	  (setq columns (car assignments)
		bindvars (cdr assignments))))
    ;; where clause: get everything after the WHERE keyword.
    (goto-char start)
    (if (sql-find-regexp "[ \t\n]*\\bwhere\\b" end)
	(setq where (sql-parse-where (point) end)))
    (list table where columns bindvars)))

(defun sql-parse-delete  (start end)
  "Parse the region for a DELETE statement and return the parsed statement.
The two arguments START and END are character positions.
See `sql-parse-statement' for more information."
  (let ((table)
	(where))
    ;; table: get string after DELETE FROM keywords.
    (goto-char start)
    (if (sql-find-regexp "[ \t\n]*\\bdelete[ \t\n]+from\\b" end)
	(setq table (sql-parse-csv (point) end))
      (error "Table missing"))
    ;; where clause: get everything after the WHERE keyword.
    (goto-char start)
    (if (sql-find-regexp "[ \t\n]*\\bwhere\\b" end)
	(setq where (sql-parse-where (point) end)))
    (list table where nil nil)))

 

(defun sql-parse-csv (start end)
  "Parse a list of comma separated values and return them as a list.
The two arguments START and END are character positions.

A value may contain any characters, but all parenthesises must be
balanced if they are not within a string, and all quotes must balance.
This is achieved by using `forward-sexp'.  The list ends when a word
outside a string matches `sql-keyword-regexp' or END is reached."
  (goto-char start)
  (let ((words)
	(pos))
    (while (and (not (looking-at sql-keyword-regexp))
		(not (looking-at "[ \t\n]*)"))
		(<= (point) end))
      (forward-sexp)
      (if (or (looking-at "[ \t\n]*[,)]")
	      (looking-at sql-keyword-regexp)
	      (looking-at sql-statement-end-regexp)
	      (= (point) end))
	  (progn
	    ;; got to save match-end, because sql-trim uses it, too.
	    (setq pos (match-end 0))
	    (add-to-list 'words 
			 (sql-trim
			  (buffer-substring-no-properties start (point))))
	    (setq start pos))))
    (nreverse words)))

(defun sql-parse-assignments (start end)
  "Parse a list of assignments.
The region between START and END is parsed and returned as two lists
in a cons cell.

An assignment consists of a word, an equal sign, and a value.  A word is
a string consisting of word characters as well as `_' and `.'
characters.  A value may contain any characters, but all parenthesises
must be balanced if they are not within a string, and all quotes must
balance.  This is achieved by using `forward-sexp'.  The assignment ends
when a word outside a string matches `sql-keyword-regexp' or END is
reached."
  (goto-char start)
  (let* ((columns)
	 (bindvars)
	 (current-list)
	 (pos))
    (while (and (not (looking-at sql-keyword-regexp))
		(< (point) end))
      (forward-sexp)
      (cond ((looking-at "[ \t\n]*=")
	     (setq current-list 'columns))
	    ((or (looking-at "[ \t\n]*,")
		 (looking-at sql-keyword-regexp))
	     (setq current-list 'bindvars)))
      (if current-list
	  (progn
	    ;; got to save match-end, because sql-trim uses it, too.
	    (setq pos (match-end 0))
	    (add-to-list current-list
			 (sql-trim
			  (buffer-substring-no-properties start (point))))
	    (setq start pos
		  current-list nil))))
    (cons (nreverse columns) (nreverse bindvars))))

(defun sql-parse-where (start end)
  "Parse a WHERE clause into a list of lines.
The region between START and END is parsed.  A new line is started
whenever `sql-insert-newline-regexp' or
`sql-insert-newline-and-indent-regexp' matches.  
Whitespace is collapsed.  Leading whitespace is stripped."
  (goto-char start)
  (setq start (re-search-forward "\\s *"))
  (let ((lines)
	(line)
	(done nil))
    (while (and (not done)
		(<= (point) end))
      (cond (;; collapse whitespace
	     (looking-at "[ \t\n]+")
	     (setq line (concat line 
				(buffer-substring-no-properties start (point))
				" ")
		   start (match-end 0))
	     (goto-char start))
	    ;; indent subselects
	    ((looking-at sql-insert-newline-and-indent-regexp)
	     (setq line (concat line
				(buffer-substring-no-properties start (point)))
		   start (point))
	     (if (> (length line) 0)
		 (add-to-list 'lines line))
	     (setq line sql-indent-string)
	     (goto-char start)
	     (forward-sexp))
	    ;; insert newlines before AND, OR, ...
	    ((looking-at sql-insert-newline-regexp)
	     (setq line (concat line
				(buffer-substring-no-properties start (point)))
		   start (match-end 0))
	     (if (> (length line) 0)
		 (add-to-list 'lines line))
	     (setq line (match-string-no-properties 0))
	     (goto-char start))
	    ;; jump in and out of parenthetical constructs if not at the
	    ;; end of the WHERE clause
	    ((and (looking-at "[()]+")
		  (< (point) end))
	     (setq line (concat line
				(buffer-substring-no-properties start (point))
				(match-string-no-properties 0))
		   start (match-end 0))
	     (goto-char start))
	    ;; end of WHERE clause
	    ((or (looking-at sql-end-where-regexp)
		 (= (point) end))
	     (setq done t)); leave loop
	    (t; skip forward across strings (we jump into parens in a previous cond)
	     (forward-sexp))))
    (setq line (concat line
		       (buffer-substring-no-properties start (point))))
    (if (> (length line) 0)
	(add-to-list 'lines line))
    (nreverse lines)))

(defun sql-trim (str)
  "Trim left and right whitespace.
Whitespace in the middle of STR is left untouched."
  ;; modified from gnus-simplify-whitespace.
  (let ((mystr str))
    ;; Leading spaces.
    (when (string-match "^[ \t\n]+" mystr)
      (setq mystr (substring mystr (match-end 0))))
    ;; Trailing spaces.
    (when (string-match "[ \t\n]+$" mystr)
      (setq mystr (substring mystr 0 (match-beginning 0))))
    mystr))

(defun sql-find-regexp (regexp limit)
  "Find REGEXP while skipping all sexps.
Return point or nil if not found before LIMIT.
Skipping sexps will skip strings and parenthetical groups."
  (let ((found (looking-at regexp)))
    (while (and (not found)
		(<= (point) limit)
		(condition-case nil
		    (progn
		      (forward-sexp 1)
		      t)
		  (scan-error nil)))
      (setq found (looking-at regexp)))
    (if (and found
	     (<= (match-end 0) limit))
	(goto-char (match-end 0)))))

(provide 'sql-transform)

;;; sql-transform.el ends here

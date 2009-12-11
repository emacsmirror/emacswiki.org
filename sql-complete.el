;;; sql-complete.el --- provide completion for tables and columns

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 0.0.1
;; Keywords: comm languages processes

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Trying to provide a framework for completion that will eventually
;; make it into sql.el.

 

;;; Code:

(require 'sql)

(defcustom sql-oracle-data-dictionary
  "select '(\"'||table_name||'\" \"'||column_name||'\")'
   from user_tab_columns 
   order by table_name;"
  "SQL Statement to determine all tables and columns."
  :group 'SQL
  :type 'string)

;; backends

(defun sql-data-dictionary (statement)
  "Return table and columns from the Oracle Data Dictionary using SQL.
STATEMENT must be a SQL statement that returns the data dictionary
one column per line.  Each line must look like this:

\(\"table-name\" \"column-name\")

Any lines not looking like this will be skipped to allow for column
headers and other fancy markup.

This currently depends very much on a good `comint-prompt-regexp'."
  (when (null sql-buffer)
    (error "No SQLi buffer available"))
  (save-excursion
    (set-buffer sql-buffer)
    (let (result end)
      (comint-simple-send sql-buffer statement)
      (comint-previous-prompt 1)
      (while (= 0 (forward-line 1))
	(message "%S" (point))
	(when (looking-at "^(.*)$")
	  (let* ((entry (car (read-from-string (match-string 0))))
		 (table (car entry))
		 (column (cadr entry))
		 (item (cdr (assoc table result))))
	    (if item
		(nconc item (list column))
	      (setq result (append (list entry) result))))))
      result)))

;; framework

(defvar sql-data-dictionary nil
  "The data dictionary to use for completion.
Each element of the list has the form
\(TABLE COLUMN1 COLUMN2 ...)")

(defun sql-oracle-data-dictionary ()
  (interactive)
  ;; FIXME No cleanup
  (setq sql-data-dictionary
	(sql-data-dictionary sql-oracle-data-dictionary)))

(defun sql-complete ()
  (interactive)
  (let ((completions (apply 'append sql-data-dictionary)))
    (comint-dynamic-simple-complete 
     (comint-word "A-Za-z_")
     completions)))

(defun sql-complete-table ()
  (interactive)
  (let ((completions (mapcar 'car sql-data-dictionary)))
    (comint-dynamic-simple-complete 
     (comint-word "A-Za-z_")
     completions)))

;;; sql-complete.el ends here

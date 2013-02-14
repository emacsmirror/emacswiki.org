;;; sqlite.el --- 
;; 
;; Filename: sqlite.el
;; Description: 
;; Author: Christian Giménez
;; Maintainer: 
;; Created: mié feb 13 11:12:31 2013 (-0300)
;; Version: 
;; Last-Updated: jue feb 14 11:12:54 2013 (-0300)
;;           By: Christian
;;     Update #: 10
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   `cl'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;  
;; For usage and explanations see http://www.emacswiki.org/emacs/SQLite-el.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 14-Feb-2013    Christian  
;;    Last-Updated: jue feb 14 01:28:15 2013 (-0300) #4 (Christian)
;;    Added `sqlite-bye' function for finishing the sqlite process.
;; 14-Feb-2013    Christian  
;;    Last-Updated: mié feb 13 11:18:46 2013 (-0300) #3 (Christian)
;;    Now output buffer doesn't appear. Sqlite connects and still works.
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(provide 'sqlite)
(eval-when-compile
  (require 'cl))


 
;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(defconst sqlite-regexp-error "Error:\\(.*\\)$"
  "This regexp must match the error return of SQLite. There must be a parenthesis surrounding the error message for matching it with:
    `match-string' 1
This is used for `sqlite-check-errors' for raising errors with messages.")

;; Adapted from http://mysite.verizon.net/mbcladwell/sqlite.html#connect

(defvar sqlite-program "sqlite3" "Full path name of the SQLITE executable.")

(defvar sqlite-db "~/mydb" "Full path name of the SQLITE database.")

(defvar sqlite-process-buffer "*sqlite-process*" "*Name of the SQLITE process buffer. This is where SQL commands are sent.")

(defvar sqlite-output-buffer "*sqlite-output-buffer*" "Name of the buffer to which all SQLITE output is redirected.") 


(defun sqlite-init ()
  "Initialize sqlite interface.
This start the process given by `sqlite-program' and prepare it for queries."
  (apply 'make-comint "sqlite-process"  sqlite-program  nil `(,sqlite-db ))
  ;; We use CSV for parsing results.
  (comint-redirect-send-command-to-process ".mode csv" sqlite-output-buffer (get-buffer-process sqlite-process-buffer) nil t)
  ;; We remove prompt.
  (comint-redirect-send-command-to-process ".prompt \"\"" sqlite-output-buffer (get-buffer-process sqlite-process-buffer) nil t)
  ;; Add headers.
  (comint-redirect-send-command-to-process ".headers on" sqlite-output-buffer (get-buffer-process sqlite-process-buffer) nil t)
  (get-buffer-create sqlite-output-buffer)
  )

(defun sqlite-bye ()
  "Finish the sqlite process sending the \".quit\" command."
  (comint-redirect-send-command-to-process ".quit" sqlite-output-buffer (get-buffer-process sqlite-process-buffer) nil t)
  ; (message "SQLite.el finished.")
  )

(defconst sqlite-regexp-next-value "\\(\"[^\"]*\"\\|[^\"][^,]*\\)\\(,\\|$\\)"
  "Used when we want to take the next value. Must match up to the first \",\" that divides between column.")

(defun sqlite-take-next-value (line)
  "Take one value up to a \",\" from LINE. This considers \".
Return a list with two elements: (value rest-of-line)"
  (if (equal line "")
      nil
    (progn ;; Is not an empty line...
      (string-match sqlite-regexp-next-value line)
      (list
       (match-string-no-properties 1 line)
       (substring-no-properties line (match-end 0)))
      )
    )
  )

(defun sqlite-parse-line ()
  "Take one line from the current-buffer and parse it returning a list of elements per column."
  (let (
	(line (chomp (thing-at-point 'line)))
	(parsed nil)
	(next t)
	)
    (while next
      (setq next (sqlite-take-next-value line))
      (when next
	(add-to-list 'parsed (car next) t)
	(setq line (cadr next))
	)
      )
    parsed
    )	  
  )

(defun sqlite-parse-result ()
  "Parse the results to create a list of header-list plus rows-lists.

Result: (header-list row1-list row2-list row3-list) "
  (let*  ((begin (goto-char (point-min)))		;4
	  (end (goto-char (point-max)))
	  (num-lines (count-lines begin end))
	  (counter 0)
	  (results-rows ()))
    (goto-char (point-min))
    (while ( < counter num-lines)
      (add-to-list 'results-rows (sqlite-parse-line) t)
      (forward-line)
      (setq counter (+ 1 counter)))
    (car `(,results-rows))))


(defun sqlite-query (sql-command)  
  "Send a query to the SQLite process and return the result.

Result:
The result is a \"table\" like the following:

    (header-list row1-list row2-list row3-list)

"
  (with-current-buffer sqlite-output-buffer
    (erase-buffer)
    (comint-redirect-send-command-to-process sql-command sqlite-output-buffer (get-buffer-process sqlite-process-buffer) nil t) 
    (accept-process-output (get-buffer-process sqlite-process-buffer) 1)  ;need to wait to obtain results
    (let ((result (sqlite-parse-result))) ;; we want to return the result! not the erase-buffer return value
      (erase-buffer)		       
      result)
    )
  )
  

(defun chomp (str)
  "Trim whitespace from string"
  (let ((s (if (symbolp str)(symbol-name str) str)))
    (save-excursion
      (while (and
	      (not (null (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
	      (> (length s) (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
	(setq s (replace-match "" t nil s)))
      (while (and
	      (not (null (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
	      (> (length s) (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
	(setq s (replace-match "" t nil s))))
    s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sqlite.el ends here

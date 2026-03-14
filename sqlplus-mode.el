;;; sqlplus-mode.el --- Mode for editing SQL and testing via SQL*Plus.

;; Copyright (C) 1994, 1995, 1996, 1998, 1999, 2001
;; Rob Riepel, Peter D. Pezaris and Martin Schwenke

;; Authors: Rob Riepel <riepel@networking.stanford.edu>
;;          Peter D. Pezaris <pez@atlantic2.sbi.com>
;;          Martin Schwenke <martin@meltin.net>
;; Maintainer: Martin Schwenke <martin@meltin.net>
;; Keywords: sql sqlplus oracle

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;;  This file is implements a simple SQL mode for emacs.  It knows how
;;  to talk to SQL*Plus via SQL*Net.  This mode was cobbled together
;;  and cut down by Martin Schwenke from the two sql-modes written by
;;  Rob Riepel and Peter D. Pezaris.  Rob Riepel's mode was hacked up
;;  version of Lynn Slater's sql.el, and some of his comments were
;;  pinched from Jim Lange's sqlplus , and probably still survive.
;;  Most of these modes are available from the elisp archives, if it
;;  still exists!
;;
;;  sqlplus-mode is for editing and testing SQL statements in a
;;  standard text buffer.  The most useful feature of sqlplus-mode is
;;  sending SQL statements to an SQL*Plus interpreter. Font locking is
;;  also implemented.
;;
;;  The following commands can be added to a global initialization
;;  file or to any user's .emacs file to conveniently use sqlplus-mode.
;;
;;    (autoload 'sqlplus "sqlplus-mode"
;;      "Start the interactive SQL*Plus interpreter in a new buffer." t)
;;
;;    (autoload 'sqlplus-mode "sqlplus-mode"
;;      "Mode for editing SQL files and running a SQL*Plus interpretor." t)
;;
;;    (add-to-list 'auto-mode-alist '("\\.sql\\'" . sqlplus-mode))
;;
;;  Use describe-mode while in sqlplus-mode for further instructions.
;;
;; src: https://web.archive.org/web/20060720040524/http://meltin.net/hacks/emacs/src/sqlplus-mode.el [2026-03-14]

;;; Code:



;;;  Revision Information

(defconst sqlplus-revision "$Revision: 1.7 $")


;;;  Variables -

(defvar sqlplus-command "sqlplus"
  "*SQL*Plus interpreter program.")

(defvar sqlplus-command-args "/"
  "*Arguments to pass to the SQL*Plus interpreter.")

(defvar sqlplus-initial-string "set sqlnumber off;\nset sqlprompt '';\nset linesize 256;\nset pagesize 1024;\nset feedback on;"
  "*Initial string to send to interpreter.")

(defvar sqlplus-buffer-name "*sqlplus*"
  "Buffer where SQL*Plus commands are run.")

(defvar sqlplus-process-name ""
  "SQL*Plus interpreter process name.")

(defvar sqlplus-mode-map nil
  "Keymap used in SQL*Plus mode.")

(defvar sqlplus-output-separator "@--"
  "String printed between sets of SQL*Plus command output.")

(defvar sqlplus-separator-regexp ";[ \n\t]*"
  "Regexp used to seperate groups of SQL statements.")
   ;; alternative version -- "^[ \t]*go\\b\\|;[ \t]*$"

(defvar sqlplus-terminator ";"
  "String to send to the SQL*Plus interpreter to initiate execution.")


;;;  Markers -

(defvar sqlplus-buffer-mark (make-marker)
  "Marks the current SQL command in the SQL*Plus output buffer.")

(make-variable-buffer-local 'sqlplus-buffer-mark)

(defvar sqlplus-region-beginning-pos nil
  "Marks the beginning of the region to sent to the SQL*Plus process.")
(defvar sqlplus-region-end-pos nil
  "Marks the end of the region to sent to the SQL*Plus process.")


;; font-lock

(defvar sqlplus-font-lock-keywords
  '(("\\<\\(select\\|from\\|where\\|tran\\|transaction\\|commit\\|group\\|exec\\|execute\\|readtext\\|rollback\\|compute\\|union\\|by\\|order\\|having\\)\\>" 1 font-lock-keyword-face)
    ("\\<\\(set\\|update\\|delete\\|insert\\|into\\|writetext\\|values\\)\\>" 1 font-lock-reference-face)
    ("\\<\\(go\\|use\\|null\\)\\>" 1 font-lock-type-face)
    ("\\<\\(begin\\|end\\|else\\|if\\|goto\\|break\\|continue\\|raiserror\\|waitfor\\|and\\|or\\|not\\|in\\|is\\|declare\\|print\\|return\\|exists\\|like\\)\\>" 1 font-lock-function-name-face)
    ("\\<\\(sum\\|avg\\|count\\|max\\|min\\|all\\|distinct\\)\\>" 1 font-lock-function-name-face)
    ("\\<\\(to\\|alter\\|table\\|database\\|create\\|disk\\|nonclustered\\|reconfigure\\|revoke\\|override\\|procedure\\|proc\\|checkpoint\\|dump\\|drop\\|index\\|fillfactor\\|rule\\|shutdown\\|tape\\|view\\|truncate\\|kill\\|load\\|clustered\\|dbcc\\|grant\\|as\\|with\\|nowait\\|no_log\\|refit\\|reinit\\|init\\|mirror\\|unmirror\\|remirror\\|default\\|sp_[a-zA-Z]*\\|statistics\\)\\>" 1 font-lock-variable-name-face)
    ("^rem .*$" 0 font-lock-comment-face))
  "Default expressions to highlight in SQL*Plus mode.")

(defvar sqlplus-mode-menu
  '("SQL*Plus"
    ["Start SQL*Plus Session"   sqlplus t]
    "----"
    ["Evaluate Statement"       sqlplus-send-current t]
    ["Evaluate Region"          sqlplus-send-region (mark)]
    "----"
    ["Interrupt Evaluation"     sqlplus-send-interrupt t]
    "----"
    ("Output"
     ["Show window"             sqlplus-buffer-display-window t]
     "----"
     ["Redisplay"               sqlplus-buffer-redisplay-current t]
     ["Previous"                sqlplus-buffer-prev-command t]
     ["Next"                    sqlplus-buffer-next-command t]
     "----"
     ["Scroll Right"            sqlplus-buffer-scroll-right t]
     ["Scroll Left"             sqlplus-buffer-scroll-left t]
     ["Scroll Down"             sqlplus-buffer-scroll-down t]
     ["Scroll Up"               sqlplus-buffer-scroll-up t]
     "----"
     ["Bottom"                  sqlplus-buffer-bottom t]
     ["Top"                     sqlplus-buffer-top    t]
     "----"
     ["Erase"                   sqlplus-buffer-erase  t]))
  "Menu for SQL*Plus mode.")

;;;  SQLPLUS-mode Keymap -

(if (not sqlplus-mode-map)
    (progn
      (setq sqlplus-mode-map (make-sparse-keymap))
      (define-key sqlplus-mode-map "\C-c'"    'sqlplus-goto-error)
      (define-key sqlplus-mode-map "\C-c\C-w" 'sqlplus-buffer-erase)
      (define-key sqlplus-mode-map "\C-c>"    'sqlplus-buffer-bottom)
      (define-key sqlplus-mode-map "\C-c<"    'sqlplus-buffer-top)
      (define-key sqlplus-mode-map "\C-c\C-p" 'sqlplus-buffer-prev-command)
      (define-key sqlplus-mode-map "\C-c\C-n" 'sqlplus-buffer-next-command)
      (define-key sqlplus-mode-map "\C-c\C-o" 'sqlplus-buffer-display-window)
      (define-key sqlplus-mode-map "\C-c\C-l" 'sqlplus-buffer-redisplay-current)
      (define-key sqlplus-mode-map "\C-c\C-b" 'sqlplus-buffer-scroll-right)
      (define-key sqlplus-mode-map "\C-c\C-f" 'sqlplus-buffer-scroll-left)
      (define-key sqlplus-mode-map "\C-c\M-v" 'sqlplus-buffer-scroll-down)
      (define-key sqlplus-mode-map "\C-c\C-v" 'sqlplus-buffer-scroll-up)
      (define-key sqlplus-mode-map "\C-c\C-c" 'sqlplus-send-interrupt)
      (define-key sqlplus-mode-map "\C-c\C-r" 'sqlplus-send-region)
      (define-key sqlplus-mode-map "\C-c\C-e" 'sqlplus-send-current)))

(defvar sqlplus-mode-syntax-table nil
  "Syntax table used while in sqlplus-mode.")
(if sqlplus-mode-syntax-table
    ()
  (setq sqlplus-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?/ ". 14" sqlplus-mode-syntax-table) ; comment start
  (modify-syntax-entry ?* ". 23" sqlplus-mode-syntax-table)
  (modify-syntax-entry ?+ "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?- "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?= "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?% "w"    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?< "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?> "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?& "w"    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?| "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?_ "w"    sqlplus-mode-syntax-table) ; _ is word char
  (modify-syntax-entry ?\' "\"" sqlplus-mode-syntax-table))


;;;  SQL*Plus mode

(defun sqlplus-mode nil
  "Mode for editing SQL files and running a SQL*Plus interpreter.
Entry into this mode runs the hook 'sqlplus-mode-hook'.

Use \\[sqlplus-mode] to invoke SQLPLUS-mode for the current buffer.

Use \\[sqlplus] to start the SQL*Plus interpreter.

Existing SQL can be sent to the SQL*Plus interpreter using
'\\[sqlplus-send-current] (sqlplus-send-current)'.  Just position the cursor
on or near the SQL statement you wish to send and press
'\\[sqlplus-send-current]' to run it and display the results.

Mode Specific Bindings:

\\{sqlplus-mode-map}"

  (interactive)
  (setq major-mode 'sqlplus-mode)
  (setq mode-name "SQL*Plus")
  (use-local-map sqlplus-mode-map)
  (set-syntax-table sqlplus-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")

  (easy-menu-define sqlplus-menu (list sqlplus-mode-map)
		    "SQL*Plus" sqlplus-mode-menu)

  (turn-on-font-lock)

  (run-hooks 'sqlplus-mode-hook))


;;;  Utilitities

(defun sqlplus-echo-in-buffer (buffer-name string &optional force-display)
  "Displays string in the named buffer, creating the buffer if needed.
If force-display is true, the buffer will appear if not already shown."
  (let ((buffer (get-buffer-create buffer-name)))
    (if force-display (display-buffer buffer))
    (set-buffer buffer)
    (goto-char (point-max))
    (insert string)
    (if force-display
	(set-window-point (get-buffer-window buffer-name) (point-max)))))

(defun sqlplus-verify-buffer nil
  "Generates reasonable error messages about the SQL*Plus connection."
  (if (not (get-buffer sqlplus-buffer-name))
      (error "No SQL*Plus session!  Use 'M-x sqlplus' to start the SQL*Plus interpreter."))
  (if (not (get-buffer-process sqlplus-buffer-name))
      (error "Buffer '%s' is not talking to anybody!" sqlplus-buffer-name)))

(defun sqlplus-send-strings (strings)
  "Sends strings to the SQL*Plus process.
Also shows the string at the top of the SQL*Plus output buffer."
  (sqlplus-verify-buffer)
  (sqlplus-echo-in-buffer sqlplus-buffer-name
		      (concat "\n" sqlplus-output-separator "\n\n"))
  (sqlplus-buffer-bottom)
  (sqlplus-buffer-mark-current)
  (sqlplus-echo-in-buffer sqlplus-buffer-name (apply 'concat strings))
  (sqlplus-echo-in-buffer sqlplus-buffer-name "\n\n")
  (let ((string (apply 'concat strings))
	(process  (get-buffer-process sqlplus-buffer-name)))
    (send-string process (concat string "\n"))
    (if (eq (current-buffer) (process-buffer process))
	(set-marker (process-mark process) (point))))
  (sqlplus-buffer-redisplay-current))

(defun sqlplus-mark-current nil
  "Marks the current SQL for sending to the SQL*Plus process.
Marks are placed around a region defined by matching pairs
of the expression listed in 'sqlplus-seperater-regexp."
  (interactive)
  (let (beg end)
    (save-excursion
      ;; We probably want the query that's on the current line, even
      ;; if we're after the semicolon.
      (beginning-of-line)
      ;; Handle "no semicolon after point".
      (if (re-search-forward sqlplus-separator-regexp nil t)
	  ;; Have semicolon.  Pop back to just before it.
	  (progn
	    (re-search-backward sqlplus-separator-regexp nil t)
	    (if (> (point) (point-min))
		(forward-char -1)))
	;; No semicolon.  If there's one at the end of the buffer that
	;; is only followed by whitespace, then skip to just before
	;; it, otherwise stay put.
	(progn
	  (goto-char (point-max))
	  (re-search-backward (concat sqlplus-separator-regexp "\\'") nil t)
	  (if (> (point) (point-min))
	      (forward-char -1))))
      (setq beg (save-excursion (if (re-search-backward sqlplus-separator-regexp nil t)
				    (+ (point) 1)
				  (point-min))))
      (setq end (save-excursion (if (re-search-forward sqlplus-separator-regexp nil t)
				    (point)
				  (point-max)))))
    (setq sqlplus-region-beginning-pos beg)
    (setq sqlplus-region-end-pos end)))

;;;  Transmission Commands

(defun sqlplus-send-region (start end)
  "Send a region to the SQL*Plus process."
  (interactive "r")
  (save-excursion
    (let ((s (buffer-substring start end)))
      ;; Strip whitespace from beginning and end, just to be neat.
      (if (string-match "\\`[ \t\n]+" s)
	  (setq s (substring s (match-end 0))))
      (if (string-match "[ \t\n]+\\'" s)
	  (setq s (substring s 0 (match-beginning 0))))
      ;; Now the string should end with an sqlplus-terminator.
      (if (not (string-match (concat sqlplus-terminator "\\'") s))
	  (setq s (concat s sqlplus-terminator)))
      ;; Ready to send.
      (sqlplus-send-strings (list s)))))

(defun sqlplus-send-current nil
  "Send the current SQL command(s) to the SQL*Plus process."
  (interactive)
  (sqlplus-mark-current)
  (sqlplus-send-region sqlplus-region-beginning-pos sqlplus-region-end-pos))


;;;  SQLPLUS-Output Buffer Operations -

(defun sqlplus-show-buffer (&optional fcn &rest args)
  "Makes the SQL*Plus output buffer visible in the other window."
  (interactive)
  (sqlplus-verify-buffer)
  (let ((window  (selected-window)))
    (if (not (eq (window-buffer window) (get-buffer sqlplus-buffer-name)))
	(switch-to-buffer-other-window sqlplus-buffer-name))
    (if fcn (condition-case nil (apply fcn args) (error nil)))
    (select-window window)))

(fset 'sqlplus-buffer-display-window 'sqlplus-show-buffer)

(defun sqlplus-buffer-scroll-up nil
  "Scroll-up in the SQL*Plus output buffer window."
  (interactive)
  (sqlplus-show-buffer 'scroll-up))

(defun sqlplus-buffer-scroll-down nil
  "Scroll-down in the SQL*Plus output buffer window."
  (interactive)
  (sqlplus-show-buffer 'scroll-down))

(defun sqlplus-buffer-scroll-left (num)
  "Scroll-left in the SQL*Plus output buffer window."
  (interactive "p")
  (sqlplus-show-buffer 'scroll-left (* num (/ (window-width) 2))))

(defun sqlplus-buffer-scroll-right (num)
  "Scroll-right in the SQL*Plus output buffer window."
  (interactive "p")
  (sqlplus-show-buffer 'scroll-right (* num (/ (window-width) 2))))

(defun sqlplus-buffer-mark-current nil
  "Mark the current position in the SQL*Plus output window."
  (sqlplus-show-buffer 'sqlplus-buffer-make-mark))

(defun sqlplus-buffer-make-mark nil
  "Set the sqlplus-buffer-marker."
  (setq sqlplus-buffer-mark (copy-marker (point))))

(defun sqlplus-buffer-redisplay-current nil
  "Go to the current sqlplus-buffer-mark."
  (interactive)
  (sqlplus-show-buffer 'sqlplus-goto-mark))

(defun sqlplus-goto-mark nil
  (goto-char sqlplus-buffer-mark)
  (recenter 0))

(defun sqlplus-buffer-top nil
  "Goto the top of the SQL*Plus output buffer."
  (interactive)
  (sqlplus-show-buffer 'sqlplus-beginning-of-buffer))

(defun sqlplus-beginning-of-buffer nil (goto-char (point-min)))

(defun sqlplus-buffer-bottom nil
  "Goto the bottom of the SQL*Plus output buffer."
  (interactive)
  (sqlplus-show-buffer 'sqlplus-end-of-buffer))

(defun sqlplus-end-of-buffer nil (goto-char (point-max)) (recenter -1))

(defun sqlplus-buffer-erase nil
  "Clear the SQL output buffer."
  (interactive)
  (sqlplus-show-buffer 'erase-buffer))

(defun sqlplus-buffer-next-command nil
  "Search for the next command in the SQL*Plus output buffer."
  (interactive)
  (sqlplus-show-buffer 'sqlplus-next-command))

(defun sqlplus-next-command nil
  "Search for the next command in the SQL*Plus output buffer."
  (cond ((re-search-forward  sqlplus-output-separator nil t)
	 (forward-line 2)
	 (recenter 0))
	(t (beep) (message "No more commands."))))

(defun sqlplus-buffer-prev-command nil
  "Search for the previous command in the SQL*Plus output buffer."
  (interactive)
  (sqlplus-show-buffer 'sqlplus-previous-command))

(defun sqlplus-previous-command nil
  "Search for the previous command in the SQL*Plus output buffer."
  (let ((start (point)))
    (re-search-backward  sqlplus-output-separator nil t)
    (cond ((re-search-backward  sqlplus-output-separator nil t)
	   (forward-line 2)
	   (recenter 0))
	  (t
	   (message "No more commands.") (beep)
	   (goto-char start)))))

(defun sqlplus-send-interrupt nil
  "Send an interrupt the the SQL*Plus interpreter process."
  (interactive)
  (interrupt-process sqlplus-process-name))


;;;  Miscellaneous

(defun sqlplus-goto-error (n)
  "Moves to the n'th line in the most recently executed SQL."
  (interactive "NLine number of error: ")
  (goto-char sqlplus-region-beginning-pos)
  (forward-line (1- n)))

;;;  SQL Interpreter

(defun sqlplus nil
  "Start the interactive SQL*Plus interpreter in a new buffer."
  (interactive)
  (get-buffer-create sqlplus-buffer-name)            ; move to the buffer.
  (or (get-buffer-process sqlplus-buffer-name)       ; already got a process?
   (progn			                 ; no, start it up!
     (set-buffer sqlplus-buffer-name)
     (setq truncate-lines t)
     (start-process sqlplus-process-name
		    sqlplus-buffer-name
		    sqlplus-command sqlplus-command-args)))
  (set-buffer sqlplus-buffer-name)
  (sqlplus-send-strings (list sqlplus-initial-string))
  (sqlplus-show-buffer))

(require 'font-lock)
(and (boundp 'font-lock-defaults-alist)
     (or (assq 'sqlplus-mode font-lock-defaults-alist)
	 (add-to-list 'font-lock-defaults-alist
		      (cons 'sqlplus-mode
			    '(sqlplus-font-lock-keywords
			      nil t
			      ((?+ . ".") (?- . ".") (?= . ".")
			       (?% . "w") (?< . ".") (?> . ".")
			       (?& . "w") (?| . ".") (?_ . "w")
			       (?\' . "\""))
			      nil
			      (font-lock-comment-start-regexp . "/[*]"))))))

(provide 'sqlplus-mode)

;;; sqlplus-mode.el ends here

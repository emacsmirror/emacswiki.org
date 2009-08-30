;;; tsql-indent.el -- indents TSQL (and other SQL) statements

;; Copyright (C) 2003  Tom Pierce

;; Author: Tom Pierce <tom@pierceport.com>
;; Version: 1.0
;; Keywords: languages, SQL, TSQL

;; This file is not part of GNU Emacs.

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

;; tsql-indent indents SQL statements and was specifically written to
;; indent Microsoft Transact SQL code.  Indentation of the current
;; line is based upon indentation of previous lines and looking
;; backward through the code for indentation hints.

;; Lines will be indented to the same indentation level of the
;; previous line unless the previous line is a begin, is part of a
;; select statement, or the current line is an end statement.  If the
;; previous line is a begin statement, the current line will be
;; indented to the right compared to the begin statement.  If the
;; previous line is part of a select statement, the select statement
;; is indented so that all portions of the select statement line up to
;; the same tab stop.  If the current line is an end statement, then
;; it will be outdented compared to the previous line.

;; Code inspection is performed via one of several regular
;; expressions.  These regular expressions are defined in
;; customization variables.

;; Installation

;; This library should be loaded once SQL mode has finished loading.
;; One way to accomplish this is to add the following to your .emacs
;; file:

;; (eval-after-load "sql"
;;   '(load-library "tsql-indent"))

;; Once the library is loaded, all SQL mode buffers from that point
;; forward will use this indentation function.

;; Inspiration

;; The inspiration for this code came from "sql-indent" by Alex
;; Schroeder (http://www.emacswiki.org/cgi-bin/wiki.pl?SqlIndent).
;; Also, an excellent tutorial on indenation in "An Emacs language
;; mode creation tutorial"
;; (http://two-wugs.net/emacs/mode-tutorial.html).

;;; Code:

(require 'sql)

(defcustom tsql-indent-statement-regexp
  "^\\W*\\(--\\|\\(begin\\|c\\(?:\\(?:los\\|reat\\)e\\)\\|de\\(?:\\(?:allocat\\|clar\\|let\\)e\\)\\|e\\(?:nd\\|xec\\)\\|go\\|i\\(?:f\\|nsert\\)\\|se\\(?:\\(?:lec\\)?t\\)\\|while\\)\\)"
  "Regexp for matching statements that are not part of select clauses
    and should be indented relative to the previous line.  This regexp
    should be optimized with regexp-opt for performance.  By default this
    contains: if, set, declare, select, create, while, close, deallocate,
    end, begin, exec, insert, delete, go"
  :type 'regexp
  :group 'SQL)

(defcustom tsql-indent-select-regexp
  "^[ \t]*\\(\\(?:(set.*\\|.*?([ \t]*\\)?select\\)"
  "This regular expression tries to match the first line of a select
    statement.  It looks for constructs such as \"(set.*select\"
    \"select\" \".*?([ \\t]select\".  This regexp should be optimized with
    regexp-opt for performance.  "
  :type 'regexp
  :group 'SQL)

(defcustom tsql-indent-begin-regexp
  "^[ \t]*begin"
  "This regular expression tries to match a begin statement."
  :type 'regexp
  :group 'SQL)

(defcustom tsql-indent-end-regexp
  "^[ \t]*\\(end[ ]*[^)]\\|commit tran\\)"
  "This regular expression tries to match an end statement without
    matching the end of a case statement."
  :type 'regexp
  :group 'SQL)

(defcustom tsql-indent-blank-line-regexp
  "^\W*$"
  "This regular expression matches blank lines."
  :type 'regexp
  :group 'SQL)

(defcustom tsql-indent-continuation-regexp
  "^\\W*\\b\\(and\\|or\\)\\b"
  "This regular expression matches continuations of select clauses."
  :type 'regexp
  :group 'SQL)

(defun tsql-indent-line ()
  "Indents the current TSQL line."
  (interactive)
  (beginning-of-line)

  ;; If at the beginning of the buffer, don't indent
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) 
          (cur-line (buffer-substring (point-at-bol)
                                      (point-at-eol)))
          (cur-line-is-begin-stmt nil)
          (cur-line-is-continuation nil)
          cur-indent)

      ;; If we're looking at a beginning statement, set the beginning
      ;; statement flag
      (if (looking-at tsql-indent-statement-regexp)
          (setq cur-line-is-begin-stmt t))

      ;; If we're looking at a line that begins with "and" or "or",
      ;; then set the continuation flag
      (if (looking-at tsql-indent-continuation-regexp)
          (setq cur-line-is-continuation t))

      ;; If we're looking at the end of a block, decrease indentation
      (if (looking-at tsql-indent-end-regexp)	
          (progn
            (save-excursion

              ;; Find the distance between point and the nearest end
              ;; and begin.
              (let ((cur-point (point)) 
                    begin-point
                    begin-indent
                    end-point 
                    end-indent)

                (save-excursion
                  (re-search-backward tsql-indent-begin-regexp nil t)
                  (setq begin-point (point))
                  (setq begin-indent (current-indentation)))

                (save-excursion
                  (if (re-search-backward tsql-indent-end-regexp nil t)
                      (progn
                        (setq end-point (point))
                        (setq end-indent (current-indentation)))
                    (setq end-point (- begin-point 1))))

                (if (> begin-point end-point)
                    (setq cur-indent begin-indent)
                  (setq cur-indent (- end-indent 
                                      default-tab-width)))))


            ;; Don't indent past the left margin
            (if (< cur-indent 0)	  
                (setq cur-indent 0)))

        ;; Not at end, so look at prior lines for indentation hints
        (save-excursion
          (while not-indented
            (forward-line -1)

            ;; If looking at an END statement, indent the current line
            ;; to the same indentation level as the END statement line
            (if (looking-at tsql-indent-end-regexp)	
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))


              (if (looking-at tsql-indent-begin-regexp) ; If the indentation
                                        ; hint is a BEGIN statement, indent the
                                        ; current line
                  (progn
                    (setq cur-indent (+ (current-indentation) 
                                        default-tab-width))
                    (setq not-indented nil))

                ;; If we're looking at a begin statement and it's not a
                ;; select statement, then indent to the level of the
                ;; previous END statement
                (if (and (looking-at tsql-indent-statement-regexp) 
                         (not (looking-at tsql-indent-select-regexp))
                         ;; Make sure blank lines are skipped
                         (not (looking-at tsql-indent-blank-line-regexp))) 
                    (progn
                      (setq cur-indent (current-indentation))
                      (setq not-indented nil))

                  ;; Check to see if current hint is a select statement
                  (let ((hint-line-is-select (looking-at 
                                              tsql-indent-select-regexp)))
                    (if (or (and hint-line-is-select 
                                 (not cur-line-is-begin-stmt))
                            (and hint-line-is-select cur-line-is-continuation))
                        (progn
                          (beginning-of-line)
                          (if (search-forward "SELECT" (point-at-eol) t)
                              (progn
                                (forward-word -1)
                                (let ((cur-tab-div (mod (current-column) 
                                                        default-tab-width)))
                                  (if (not (eq cur-tab-div 0))
                                      ;; Insert a tab if the select
                                      ;; statement doesn't start on a
                                      ;; tab
                                      (insert "\t"))
                                  (setq cur-indent (current-column))
                                  (setq not-indented nil)))))

                      ;; If at the beginning of the buffer, don't try
                      ;; to go backward any further
                      (if (bobp)
                          (setq not-indented nil))))))))))

      ;; If there is indentation, then do it
      (if cur-indent
          (indent-line-to cur-indent)

        ;; Could not figure out indentation, so don't indent
        (indent-line-to 0))))) 

(add-hook 'sql-mode-hook
          (function (lambda ()
                      (make-local-variable 'indent-line-function)
                      (setq indent-line-function 'tsql-indent-line))))

(provide 'tsql-indent)

;;; tsql-indent.el ends here

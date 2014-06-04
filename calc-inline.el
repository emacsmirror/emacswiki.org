;;; calc-inline.el --- Evaluate calc-expression within ordinary text.

;; Copyright (C) 2010  U-ITIHQ\Tobias.Naehring

;; Author: U-ITIHQ\Tobias.Naehring <Tobias.Naehring@Rechner64.ITIHQ.local>
;; Keywords: tools, wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Matlab-like calc.

;; Main commands:
;;
;; calc-inline (evaluate one line)
;;
;; and
;;
;; calc-inline-region (evaluate active region)
;;
;; Bind this two function to the keys of your choice.
;;
;; See doc of calc-inline-mode for comments of the structure of calc sheets.
;;
;; Changes:
;; 2011-02-08: calc-inline-region also works for the whole buffer (see doc of calc-inline-region).
;;
;; 2014-06-04: calc-inline-region: Work on active region even if called non-interactively with b=e=nil.
;;  Add function calc-inline-starter-re used in calc-inline and calc-inline-region for identification of calc-inline lines/regions.
;;  Therewith also out-commented regions are recognized by calc-inline-region.

;;; Code:

(require 'calc)
(require 'calccomp)

(defvar calc-inline-input-filters '(calc-inline-measurement-units calc-inline-input-filter)
  "List of input filter functions.
Each filter is run within calc: on expr before calc-eval is
invoked. Each to-filter is run on the lhs of assignments before
a (set (intern v) ...) is executed.

Each filter should be a symbol, say 'filter of a function which takes expr as argument
and returns the transformed expression as string.")

(make-variable-buffer-local 'calc-inline-input-filters)

(defvar calc-inline-output-filters '(calc-inline-output-filter)
  "List of output filter functions.
Each filter is run on the lhs of assignments before
a (set (intern v) ...) is executed.

Each filter should be a symbol, say 'filter of a function which takes expr as argument
and returns the transformed expression as string.")

(defvar calc-inline-measurement-units nil 
  "Measurement units understood by calc in SimX-syntax. I.e., 1'mm' is 1e-3.")
(setq calc-inline-measurement-units 
      '(
	("'m'" "")
	("'mm'" "*1e-3")
	("'km'" "*1e3")
	("'km/h'" "*(1/3.6)")
	("'m/s'" "")
	("'mm/s'" "*1e-3")
	("'kg'" "")
	("'g'" "*1e-3")
	("'N'" "")
	("'N/m'" "")
	("'N/mm'" "*1000.0")
	("'kN/mm'" "*1e6")
	("'°'" "*(pi/180)")
	("'rad'" "")
	))

(defun calc-inline-measurement-units (expr)
  "Transform measurement units."
  (message "calc-inline-measurement-units(%s)" expr)
  (let ((units (mapcar 'car calc-inline-measurement-units))
	(vals (mapcar 'cadr calc-inline-measurement-units)))
    (calc-inline-filter expr "'[^']+'" units vals)))

(make-variable-buffer-local 'calc-inline-output-filters)

(defun calc-inline-compile-regexp (l)
  (apply 'concat (append '("\\(")
			 (list (car l))
			 (mapcar #'(lambda (s) (concat "\\|" s)) (cdr l))
			 '("\\)")
			 )))

(defun calc-inline-looking-at-one-of (regexp-list)
  "If we are looking at one of the regexps in REGEXP-LIST
then return its number for `nth'."
  (let ((i 0) (p regexp-list))
    (while (and p (not (looking-at (car p))))
      (setq i (1+ i))
      (setq p (cdr p)))
    (and p i)))

(defun calc-inline-search-one-of (regexp-list)
  "From point on search for a match of the regexps in REGEXP-LIST.
Set point to the end of the matching expression and return number of expression for `nth'."
  (let (match-num)
    (while (not (or (setq match-num (calc-inline-looking-at-one-of regexp-list))
		    (= (point) (point-max))))
      (forward-char)
      )
    (if match-num
	(goto-char (match-end 0)))
    match-num))

(defun calc-inline-filter (expr sub-regexp from-list to-list &optional hook)
  "First search EXPR for a string matching SUB-REGEXP.
Then apply to this substring a translation FROM-LIST -> TO-LIST.
This means each item matching the `nth' of FROM-LIST is replaced with the `nth' of TO-LIST."
  (with-temp-buffer
    (delete-region (point-min) (point-max))
    (insert expr)
    (goto-char (point-min))
    (save-match-data
      (while (search-forward-regexp sub-regexp nil 'noErr)
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (goto-char (point-min))
	  (let ((match-num (calc-inline-search-one-of from-list)))
	    (if match-num
		(progn 
		  (goto-char (point-min))
		  (if hook (apply hook '()))
		  (while (setq match-num (calc-inline-search-one-of from-list))
		    (replace-match (nth match-num to-list))
		    )))))))
    (buffer-string)))


(setq calc-inline-regexp-varname "[[:alpha:]_][[:alnum:]_.]*")

(defun string-replace-matches (str from to)
  "Replace all occurences of regular expressions FROM in STR with TO."
  (with-temp-buffer
    (insert str)
    (goto-char 1)
    (save-match-data
      (while (search-forward-regexp from nil 'noErr)
	(replace-match to 'fixedCase)))
    (buffer-string)))

(defun calc-inline-input-filter (expr)
  (message "calc-inline-input-filter(%s)" expr)
  (calc-inline-filter expr "\\(\\<\\|_\\)[[:alpha:]_][[:alnum:]_.]*" '("_" "\\.") '("#U" "#D")
		      '(lambda ()
			 (goto-char (point-min))
			 (insert "var#")
			 )))

(defun calc-inline-output-filter (expr)
  (calc-inline-filter expr "\\<var_\\(?:[[:alpha:]]\\|_U\\)\\(?:[[:alnum:]]\\|_U\\|_D\\)*"'("_U" "_D") '("_" ".")
		      '(lambda ()
			 (message "Running hook at %s" (buffer-string))
			 (goto-char (point-min))
			 (if (looking-at "var_")
			     (kill-region (point-min) (+ (point-min) 4))))))

(defun calc-inline-run-filters (expr filters)
  (let ((filter filters))
    (while filter
      (setq expr (apply (car filter) (list expr)))
      (setq filter (cdr filter))))
  expr)

(defun calc: (expr &optional no-output-filter)
  "Run `calc-inline-input-filters' on expr. Call calc-eval on the resulting expression and run `calc-inline-output-filters' on the result.
If NO-OUTPUT-FILTER is non-nil return the raw result of `calc-eval'."
  (setq expr (calc-inline-run-filters expr calc-inline-input-filters))
  (message "Running calc-eval on:%s." expr)
  (setq expr (calc-eval (concat "evalv(" expr ")")))
  (message "Result of calc-eval:%s." expr)
  (if no-output-filter
      expr
    (calc-inline-run-filters expr calc-inline-output-filters)))

(defvar calc-inline-syntax-table nil
  "")

(progn
  (setq calc-inline-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\; "w" calc-inline-syntax-table)
  (modify-syntax-entry ?{ "(" calc-inline-syntax-table)
  (modify-syntax-entry ?} ")" calc-inline-syntax-table)
  )


(defun calc-inline-command-end-position (&optional offset)
  (save-excursion
    (with-syntax-table calc-inline-syntax-table
      (if (numberp offset)
	  (forward-char offset))
      (while (not (looking-at "$")) (forward-sexp)) (point))))

(defvar var-ans nil "Default return variable of `calc-inline'.")

(defun calc-inline-starter-re (&optional full)
  "Return starter regexp for calc-inline."
  (concat "^\\([[:blank:]]*\\(?:" comment-start "[[:blank:]]*\\)?\\)" (and full "\\(calc\\|lisp\\):")))

(defun calc-inline ()
  "Evaluate calc-expression on current line.
See help of calc-inline-mode for more information.
"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((comment-regexp (calc-inline-starter-re))
	  comment-str)
      (if (looking-at (concat comment-regexp "lisp:"))
	  (progn
	    (goto-char (match-end 0))
	    (eval-region (point) (scan-sexps (point) 1)))
	(let (assignTo
	      argList
	      output
	      (printOut 't)
	      (beginExpr (if (looking-at (concat comment-regexp "calc:[^=]"))
			     (progn
			       (setq comment-str (match-string 1))
			       (match-end 0)
			       )
			   (line-beginning-position)))
	      (endExpr (calc-inline-command-end-position)))
	  (if (search-forward ":=" endExpr 'noError)
	      (progn
		(goto-char beginExpr)
		(unless (looking-at (concat " *\\(" calc-inline-regexp-varname "\\) *\\((\\(\\( *" calc-inline-regexp-varname " *,\\)* *" calc-inline-regexp-varname " *\\))\\)? *:="))
		  (error "Bad name of left-hand-side. Expression left to be parsed:\"%s\""
			 (buffer-substring beginExpr endExpr)))
		(setq beginExpr (match-end 0))
		(setq argList (match-string-no-properties 3))
		(setq assignTo (subst-char-in-string ?# ?-
						     (calc-inline-run-filters (match-string-no-properties 1) calc-inline-input-filters)))
		(unless (or (equal (string-match "var-" assignTo) 0) argList)
		  (setq assignTo (concat "var-" assignTo)))
		(setq assignTo (intern assignTo))
		))
	  (goto-char endExpr)
	  (if (progn (skip-chars-backward "[:blank:]\n") (backward-char) (looking-at ";"))
	      (progn
		(message "calc-inline: found ;")
		(setq printOut nil)
		(setq endExpr (point))
		)
	    (message "Character at end: %c." (char-after)))
	  (let ((rhs (buffer-substring-no-properties beginExpr endExpr)))
	    (if argList
		(progn
		  (setq argList (split-string argList "\\( \\|,\\)" 't))
		  (setq var-ans (symbol-name (eval (math-do-defmath assignTo (mapcar 'intern argList) (list ': rhs))))))
	      (message "right-hand side: %s" rhs)
	      (setq var-ans (apply 'calc: (list rhs 'no-output-filter)))
	      (setq output (calc-inline-run-filters var-ans calc-inline-output-filters))))
	  (unless var-ans
	    (error "Error: Cannot evaluate inline-expression"))
	  (message "ans:%s" output)
	  (if assignTo
	      (if (equal var-ans "evalv()")
		  (makunbound assignTo)
		(set assignTo (math-read-expr var-ans))))
	  (goto-char endExpr)
	  (if printOut
	      (progn
		(if (looking-at (concat "\n" comment-regexp "ans:"))
		    (kill-region (match-beginning 0) (calc-inline-command-end-position 1)))
		(insert "\n" (if comment-str comment-str "") "ans:" output)
		)))))))

(defun calc-inline-region (&optional b e)
  "Evaluate calc-lines introduced by the string \"calc:\" within current region.
Works on buffer if region is not active.
For non-interactive calls b defaults to point-min and e defaults to point-max."
  (interactive)
  (unless b (setq b (if (use-region-p) (region-beginning) (point-min))))
  (unless e (setq e (if (use-region-p) (region-end) (point-max))))
  (save-excursion (save-restriction
		    (narrow-to-region b e)
		    (goto-char (point-min))
		    (let ((re (calc-inline-starter-re 'full)))
		      (while (search-forward-regexp re nil 'noError)
			(calc-inline))
		      ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; il-language support
;; make sqrt and '/' work like in C-mode
(require 'calc)
(require 'calc-lang)
(defun calc-il-language ()
  (interactive)
  (calc-wrapper
   (calc-set-language 'il)
   (message "`IL' language mode")))

(put 'il 'math-oper-table
     '( ( "u!"    calcFunc-lnot -1 1000 )
	( "~"     calcFunc-not  -1 1000 )
	( "u+"    ident	     -1  197 )
	( "u-"    neg	     -1  197 )
	( "^"     calcFunc-pow  190 191 )
	( "*"     *	     190 191 )
	( "/"     /	     190 191 )
	( "%"     %	     190 191 )
	( "+"     +	     180 181 )
	( "-"     -	     180 181 )
	( "<<"    calcFunc-lsh  170 171 )
	( ">>"    calcFunc-rsh  170 171 )
	( "<"     calcFunc-lt   160 161 )
	( ">"     calcFunc-gt   160 161 )
	( "<="    calcFunc-leq  160 161 )
	( ">="    calcFunc-geq  160 161 )
	( "=="    calcFunc-eq   150 151 )
	( "!="    calcFunc-neq  150 151 )
	( "&"     calcFunc-and  140 141 )
	( "|"     calcFunc-or   120 121 )
	( "&&"    calcFunc-land 110 111 )
	( "||"    calcFunc-lor  100 101 )
	( "?"     (math-read-if)  91  90 )
	( "!!!"   calcFunc-pnot  -1  88 )
	( "&&&"   calcFunc-pand  85  86 )
	( "|||"   calcFunc-por   75  76 )
	( "="     calcFunc-assign 51 50 )
	( ":="    calcFunc-assign 51 50 )
	( "::"    calcFunc-condition 45 46 ))) ; should support full assignments

(put 'il 'math-function-table
     '( ( acos	   . calcFunc-arccos )
	( acosh	   . calcFunc-arccosh )
	( asin	   . calcFunc-arcsin )
	( asinh	   . calcFunc-arcsinh )
	( atan	   . calcFunc-arctan )
	( atan2	   . calcFunc-arctan2 )
	( atanh	   . calcFunc-arctanh )))

(put 'il 'math-variable-table
     '( ( M_PI	   . var-pi )
	( M_E	   . var-e )))

(put 'il 'math-vector-brackets "{}")

(put 'il 'math-radix-formatter
     (function (lambda (r s)
		 (if (= r 16) (format "0x%s" s)
		   (if (= r 8) (format "0%s" s)
		     (format "%d#%s" r s))))))

(put 'il 'math-compose-subscr
     (function
      (lambda (a)
        (let ((args (cdr (cdr a))))
          (list 'horiz
                (math-compose-expr (nth 1 a) 1000)
                "["
                (math-compose-vector args ", " 0)
                "]")))))

(add-to-list 'calc-lang-slash-idiv 'il)
(add-to-list 'calc-lang-allow-underscores 'il)
(add-to-list 'calc-lang-brackets-are-subscripts 'il)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar calc-inline-mode-hook nil "Run at start of calc-inline-mode.")

(define-minor-mode calc-inline-mode
  "Evaluate calc-expressions within ordinary text.
Expressions look like:
varname := expression;
Semicolon at the end of line means print the result just as message in the mini-buffer.
Without the semicolon a new result line is inserted after the expression. 
The result line starts with the string \"ans:\" followed by the result value.

If the expression to be printed is already followed by a result line the old result line
is replaced by the new one.
"
  :lighter " calcIL"
  :keymap (list (cons (kbd "C-c C-c") 'calc-inline)
		(cons (kbd "C-c C-r") 'calc-inline-region))
  (if calc-inline-mode
      (progn
	;; commands executed when calc-inline-mode is switched on:
	(calc-create-buffer)
	(calc-il-language)
	)
    ;; executed when calc-inline-mode is switched off:
    (calc-normal-language)
    )
  )

(define-key mode-line-mode-menu [calc-inline-mode]
  `(menu-item ,(purecopy "calc-inline") calc-inline-mode
	      :button (:toggle . calc-inline-mode)))

(provide 'calc-inline)
;;; calc-inline.el ends here

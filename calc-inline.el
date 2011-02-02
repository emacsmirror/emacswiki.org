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

;;; Code:

(defvar calc-inline-input-filters '(calc-inline-input-filter)
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

(make-variable-buffer-local 'calc-inline-output-filters)

(defun calc-inline-compile-regexp (l)
  (apply 'concat (append '("\\(")
			 (list (car l))
			 (mapcar '(lambda (s) (concat "\\|" s)) (cdr l))
			 '("\\)")
			 )))

(defun calc-inline-looking-at-one-of (regexp-list)
  (let ((i 0) (p regexp-list))
    (while (and p (not (looking-at (car p))))
      (setq i (1+ i))
      (setq p (cdr p)))
    (and p i)))

(defun calc-inline-search-one-of (regexp-list)
  (let (match-num)
    (while (not (or (setq match-num (calc-inline-looking-at-one-of regexp-list))
		    (= (point) (point-max))))
      (forward-char)
      )
    (if match-num
	(goto-char (match-end 0)))
    match-num))

(defun calc-inline-filter (expr sub-regexp from-list to-list &optional hook)
  "Example for a calc-inline-filter."
  (save-excursion
    (set-buffer (get-buffer-create "*calc-inline-buffer*"))
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

(defun calc-inline-input-filter (expr)
  (message "calc-inline-input-filter(%s)" expr)
  (calc-inline-filter expr "\\<[[:alpha:]_][[:alnum:]_.]*" '("_" "\\.") '("#U" "#D")
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

(defun calc: (expr)
  (setq expr (calc-inline-run-filters expr calc-inline-input-filters))
  (message "Running calc-eval on:%s." expr)
  (setq expr (calc-eval (concat "evalv(" expr ")")))
  (message "Result of calc-eval:%s." expr)
  (calc-inline-run-filters expr calc-inline-output-filters))

(defun calc-inline ()
  "Evaluate calc-expression on current line.
See help of calc-inline-mode for more information.
"
  (interactive)
  (save-excursion
      (beginning-of-line)
      (let (assignTo
	    argList
	    (printOut 't)
	    (beginExpr (if (looking-at "calc:[^=]")
			   (match-end 0)
			 (line-beginning-position)))
	    (endExpr (line-end-position)))
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
	(end-of-line)
	(if (search-backward-regexp "; *$" beginExpr 'noError)
	    (progn
	      (setq printOut nil)
	      (setq endExpr (point))
	      ))
	(let ((rhs (buffer-substring-no-properties beginExpr endExpr)))
	  (if argList
	      (progn
		(setq argList (split-string argList "\\( \\|,\\)" 't))
		(setq var-ans (symbol-name (eval (math-do-defmath assignTo (mapcar 'intern argList) (list ': rhs)))))
		)
	    (setq var-ans (apply 'calc: (list rhs)))))
	(unless var-ans
	  (error "Error: Cannot evaluate inline-expression"))
	(message "ans:%s" var-ans)
	(if assignTo
	    (if (equal var-ans "evalv()")
		(makunbound assignTo)
	      (set assignTo (math-read-expr var-ans))))
	(end-of-line)
	(if printOut
	    (progn
	      (if (looking-at "\nans:")
		  (kill-region (match-beginning 0) (line-end-position 2)))
	      (insert "\nans:" var-ans)
	      )))))

(defun calc-inline-region (b e)
  "Evaluate calc-lines introduced by the string \"calc:\" within current region."
  (interactive "r")
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (while (search-forward-regexp "^calc:" nil 'noError)
      (calc-inline)
      )))

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
)

(define-key mode-line-mode-menu [calc-inline-mode]
  `(menu-item ,(purecopy "calc-inline") calc-inline-mode
	      :button (:toggle . calc-inline-mode)))

(provide 'calc-inline)
;;; calc-inline.el ends here

;;; cexp.el --- Poorman's implementation of combined expressions.
;; Copyright (C) 2010  Tobias.Naehring

;; Author: Tobias.Naehring <i@tn-home.de>
;; Keywords: Combined Expressions

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

;; Poorman's implementation of combined expressions.
;; Combined expressions are combinations of regular expressions and balanced expressions.
;; You can use search-forward-cexp for searching combined expressions.
;; Some clumsy way of storing the match-data and the balanced expressions is provided.
;; That is just a test implementation.
;; Example:
;; (setq var-match-data (search-forward-cexp "foo\\!(\\`(\\([[:alpha:]][[:alnum:]]*\\)\\(,[[:alpha:]][[:alnum:]]*\\)*)\\'\\!)"))
;; This combined expression matches the following string:
;; foo(x,y34,zoo)
;; Investigate the match data:
;; (set-match-data var-match-data)
;; For an example get the function name:
;; (match-string-no-properties 0)
;; And get the balanced expression inclusive the delimiters which are assured to be `(' and `)' in this example:
;; (match-string-no-properties 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changes:
;;
;; 2012-01-25: Upload.


(defvar cexp-start "!(" "Start control string of balanced expression within regular expression.")
(defvar cexp-end "!)" "End control string of balanced expression within regular expression.")
(defun cexp-find-special (cexp &optional cexp-re start)
  "Finds balanced special strings for sexp within cexp. Returns
cons with start position of the special string and the special
string itself."
  (or cexp-re (setq cexp-re (concat "\\(" cexp-start "\\|" cexp-end "\\)")))
  (while (and (setq start (string-match (concat "\\\\" cexp-re) cexp start))
	      (> start 0)
	      (= ?\\ (elt cexp (1- start))))
    (setq start (1+ start)))
  (cons start (match-string-no-properties 1 cexp)))

(defun cexp-find-top-sexp (cexp &optional start)
  "Returns a cons (BEG . END) of the beginning position BEG and the end position END of the first
top-level sexp control string in CEXP.
The sexp control string is delimited by the strings defined in `cexp-start' and `cexp-end'.
BEG points to the start string `cexp-start' within cexp and END points at the character behind end string `cexp-end' or to the end of CEXP.
"
  (let* (
	 (cexp-re (concat "\\(" cexp-start "\\|" cexp-end "\\)"))
	 (c (cexp-find-special cexp cexp-re start))
	 (b (car c))
	 )
    (if (setq start b)
	(progn
	  (setq start (1+ start))
	  (unless (string= (cdr c) cexp-start) (error "Unbalanced cexp."))
	  (let ((cnt 1))
	    (while (and
		    (> cnt 0)
		    (car (setq c (cexp-find-special cexp cexp-re start))))
	      (if (string= (cdr c) cexp-start)
		  (setq cnt (1+ cnt))
		(setq cnt (1- cnt)))
	      (setq start (1+ (car c))))
	    (when (> cnt 0) (error "Unbalanced cexp %d" cnt)))
	  (cons b (+ (car c) 1 (length cexp-end))))
      nil)))

(defun search-forward-cexp1 (cexp &optional var-match-data)
  "Search for combined regular and balanced expressions (cexp). May fail after partial match.
The syntax of cexp is almost that of a regular expression with the exception that the string \\!( introduces a balanced expression and \\!) closes a balanced expression. The matched balanced expressions appear within the match data.
Regular expression braces \\( and \\) may not include balanced expressions (sexps)."
  (let* ((sexpBegEnd (cexp-find-top-sexp cexp))
	 (endRegexp (if sexpBegEnd (car sexpBegEnd)))
	 (re (substring cexp 0 endRegexp))
	 )
    ;; Handle leading regexp
    (when (search-forward-regexp re nil 'noError)
      (setq var-match-data (append var-match-data (match-data)))
      ;; Handle sexp and tail
      (if sexpBegEnd
	  (let
	      ((buf-sexpBeg (point))
	       (buf-sexpEnd (goto-char (scan-sexps (point) 1)))
	       )
	    (setq var-match-data (append var-match-data (list buf-sexpBeg buf-sexpEnd)))
	    ;; Handle sexp
	    (save-excursion
	      (save-restriction
		(narrow-to-region buf-sexpBeg buf-sexpEnd)
		(goto-char (point-min))
		(setq var-match-data (search-forward-cexp1 (substring cexp (+ (length cexp-start) (car sexpBegEnd) 1) (- (cdr sexpBegEnd) (length cexp-end) 1)) var-match-data))
		))
	    ;; Handle tail if there is any. (The tail may be anything...)
	    (when (and var-match-data (not (= (length cexp) (cdr sexpBegEnd))))
	      (setq var-match-data (search-forward-cexp1 (substring cexp (cdr sexpBegEnd)) var-match-data))
	      )))
      var-match-data)))

(defun search-forward-cexp (cexp)
  "Search for combined regular and balanced expressions (cexp).
The syntax of cexp is almost that of a regular expression with the exception that the string \\!( introduces a balanced expression and \\!) closes a balanced expression. The matched balanced expressions appear within the match data.
Regular expression braces \\( and \\) may not include balanced expressions (sexps)."
  (let (var-match-data)
    (while (not (or (setq var-match-data (search-forward-cexp1 cexp)) (eobp))))
    var-match-data))

(provide 'cexp)

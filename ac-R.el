;;; ac-R.el --- Autocompletion routines for R
;;
;; Filename: ac-R.el
;; Description: Autocompletion for R.
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Mon Aug 23 15:11:28 2010 (-0500)
;; Version: 0.2
;; Last-Updated:
;;           By:
;;     Update #: 46
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'ac-R)
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 17-Jan-2012 Vitalie Spinu
;;    Replaced the completion mechanism with one the function ess-R-get-rcompletions.
;;    Replaced regexp search by a function to recognize dots, brackets etc.
;;    Replaced lambdas in hooks.
;; 7-July-2011    Zigler Zhang
;;    Quote local variable to avoid "Symbol nil may not be buffer-local"
;;    when start process of ESS
;; 23-Mar-2011    Timothy C. Harper
;;    Hook up R documentation in completion popup.
;; 22-Oct-2010    Matthew L. Fidler
;;    Added caching mechanism for 3 characters or less
;; 23-Aug-2010    Matthew L. Fidler
;;    Initial Version
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
(defvar ac-R-cache '()
  "A cache of R autocompletion.")

(make-variable-buffer-local 'ac-R-cache)

(defun ac-R-add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let (
	(case-fold-search 't)
	(existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var))))))

(defun ac-R ()
  "Returns a list of completions"
  (let ( (prefix "") present ret)
    (when (looking-back "\\<[^ \t\n,=.$]*")
      (setq prefix (match-string 0)))
    (setq present (assoc prefix ac-R-cache))
    (if present
        (setq ret (assoc prefix present))
      (ess-R-get-rcompletions))
      (when (>= 3 (length prefix))
        (ac-R-add-to-alist 'ac-R-cache (list prefix ret))))
    (symbol-value 'ret))

(defun ess-get-help-text (sym)
  (interactive)
  (require 'ess-help)
  (with-temp-buffer
    (ess-command (format inferior-ess-help-command sym) (current-buffer))
    (ess-help-underline)
    (buffer-substring (point-min) (point-max))))

(defun ess-get-ac-start ()
  (let ((chars "]A-Za-z0-9.$@_:["))
    ( (string-match (format "[%s]" chars) (char-to-string (char-before)))
      (save-excursion
	(re-search-backward (format "[^%s]" chars) nil t)
	(1+ (point))) ;; else nil
      )))

(setq ac-source-R
      '((prefix     . ess-get-ac-start)
        (requires   . 1)
        (candidates . ac-R)
        (document   . ess-get-help-text)
        (cache)))


(defun ess-R-get-rcompletions ()
  "Calls R internal complation utilities for posible completions"
  (let ((beg-of-line (save-excursion (comint-bol nil) (point)))
	(line-buffer ))
    (ess-get-words-from-vector (format
				"{utils:::.assignLinebuffer('%s%')
			          utils:::.assignEnd(%d)
			          utils:::.guessTokenFromLine()
			          utils:::.completeToken()
			          utils:::.retrieveCompletions()}\n"
				(buffer-substring beg-of-line (point-at-eol))
				(- (point) beg-of-line))
			       )))


(add-to-list 'ac-modes 'ess-mode)

(defun ess-ac-init ()
  (setq ac-sources '(ac-source-R ac-source-filename))
  (make-local-variable 'ac-ignore-case)
  (setq ac-ignore-case nil))

(add-hook 'ess-mode-hook 'ess-ac-init)
(add-hook 'inferior-ess-mode-hook 'ess-ac-init)

(provide 'ac-R)

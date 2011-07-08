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
;; Most of this was hacked from the Ess sources for completion.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
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
      (if ess-use-R-completion
          (setq ret (ac-R-complete-object-name))
        (setq ret (ac-internal-complete-object-name)))
      (when (>= 3 (length prefix))
        (ac-R-add-to-alist 'ac-R-cache (list prefix ret))))
    (symbol-value 'ret)))

(defun ess-get-help-text (sym)
  (interactive)
  (require 'ess-help)
  (with-temp-buffer
    (ess-command (format inferior-ess-help-command sym) (current-buffer))
    (ess-help-underline)
    ;; Stata is clean, so we get a big BARF from this.
    (if (not (string= ess-language "STA"))
        (ess-nuke-help-bs))
    (buffer-substring (point-min) (point-max))))

(setq ac-source-R
      '((prefix . "\\<[^ \t\n,=.$]*")
        (requires . 1)
        (candidates . ac-R)
        (document . ess-get-help-text)
        (cache)))

(defun ac-R-complete-object-name ()
  ;; Modified from ESS
  "Completion in R via R's completion utilities (formerly 'rcompgen').
To be used instead of ESS' completion engine for R versions >= 2.5.0
 (or slightly older versions of R with an attached and working 'rcompgen' package)."
  (interactive)
  (ess-make-buffer-current)
  (let* ((comint-completion-addsuffix nil)
	 (beg-of-line (save-excursion (comint-bol nil) (point)))
	 (end-of-line (point-at-eol))
	 (line-buffer (buffer-substring beg-of-line end-of-line))
	 (NS (if (ess-current-R-at-least '2.7.0)
		 "utils:::"
	       "rcompgen:::"))
	 (token-string ;; setup, including computation of the token
	  (progn
	    (ess-command
	     (format (concat NS ".assignLinebuffer('%s')\n") line-buffer))
	    (ess-command (format (concat NS ".assignEnd(%d)\n")
				 (- (point) beg-of-line)))
	    (car (ess-get-words-from-vector
		  (concat NS ".guessTokenFromLine()\n")))))

	 (possible-completions ;; compute and retrieve possible completions
	  (progn
	    (ess-command (concat NS ".completeToken()\n"))
	    (ess-get-words-from-vector
	     (concat NS ".retrieveCompletions()\n")))))
    (symbol-value 'possible-completions)))

(defun ac-internal-complete-object-name (&optional listcomp)
  "Perform completion on `ess-language' object preceding point.
The object is compared against those objects known by
`ess-get-object-list' and any additional characters up to ambiguity are
inserted.  Completion only works on globally-known objects (including
elements of attached data frames), and thus is most suitable for
interactive command-line entry, and not so much for function editing
since local objects (e.g. argument names) aren't known.

Use \\[ess-resynch] to re-read the names of the attached directories.
This is done automatically (and transparently) if a directory is
modified (S only!), so the most up-to-date list of object names is always
available.  However attached dataframes are *not* updated, so this
command may be necessary if you modify an attached dataframe.

If ARG is non-nil, no completion is attempted, but the available
completions are listed [__UNIMPLEMENTED__]."
  (interactive "P");; FIXME : the `listcomp' argument is NOT used
  (ess-make-buffer-current)
  (if (memq (char-syntax (preceding-char)) '(?w ?_))
      (let* ((comint-completion-addsuffix nil)
	     (end (point))
	     (buffer-syntax (syntax-table))
	     (beg (unwind-protect
		      (save-excursion
			(set-syntax-table ess-mode-syntax-table)
			(backward-sexp 1)
			(point))
		    (set-syntax-table buffer-syntax)))
	     (full-prefix (buffer-substring beg end))
	     (pattern full-prefix)
	     ;; See if we're indexing a list with `$'
	     (listname (if (string-match "\\(.+\\)\\$\\(\\(\\sw\\|\\s_\\)*\\)$"
					 full-prefix)
			   (progn
			     (setq pattern
				   (if (not (match-beginning 2)) ""
				     (substring full-prefix
						(match-beginning 2)
						(match-end 2))))
			     (substring full-prefix (match-beginning 1)
					(match-end 1)))))
	     ;; are we trying to get a slot via `@' ?
	     (classname (if (string-match "\\(.+\\)@\\(\\(\\sw\\|\\s_\\)*\\)$"
					 full-prefix)
			   (progn
			     (setq pattern
				   (if (not (match-beginning 2)) ""
				     (substring full-prefix
						(match-beginning 2)
						(match-end 2))))
			     (ess-write-to-dribble-buffer
			      (format "(ess-C-O-Name : slots..) : patt=%s"
				      pattern))
			     (substring full-prefix (match-beginning 1)
					(match-end 1)))))
	     (components (if listname
			     (ess-object-names listname)
			   (if classname
			       (ess-slot-names classname)
			     ;; Default case: It hangs here when
			     ;;    options(error=recoves) :
			     (ess-get-object-list ess-current-process-name)))))
	;; always return a non-nil value to prevent history expansions
        (append classname components))))


(add-to-list 'ac-modes 'ess-mode)

(add-hook 'ess-mode-hook
        (lambda ()
          (add-to-list 'ac-sources 'ac-source-R)
          (make-local-variable 'ac-ignore-case)
          (setq ac-ignore-case nil)))
(add-hook 'inferior-ess-mode-hook
        (lambda ()
          (add-to-list 'ac-sources 'ac-source-R)
          (make-local-variable 'ac-ignore-case)
          (setq ac-ignore-case nil)))

(provide 'ac-R)

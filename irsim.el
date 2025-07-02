;;; A major mode for editing IRSIM netlists. Public domain. Created by
;;; Peter Scott <pjscott@iastate.edu> in 2008. To install, put
;;; irsim-mode.el somewhere that emacs will see it like you site-lisp/
;;; sirectory and stick something like this in your .emacs file:
;;;
;;; (autoload 'irsim-mode "irsim-mode" nil t)
;;; (setq auto-mode-alist
;;;       (cons '("\\.sim$" . irsim-mode) auto-mode-alist))
;;;
;;; Note that this will override the default behavior for opening .sim
;;; files, which is to go into simula-mode. This might be a problem if
;;; you use both.
;;;
;;; Note also that this mode is opinionated. It has a very specific
;;; idea of how IRSIM code ought to be indented, and if you don't like
;;; it then just set irsim-use-fancy-indent to nil. I think that the
;;; default indentation method leads to very pretty files and easy
;;; typing, but YMMV.

;;; Commentary:
;;; retrieved 2 July 2025 from 
;;; https://codesite-archive.appspot.com/archive/p/irsim-mode/downloads
;;; author: Peter Scott (2008)

(require 'font-lock)
(require 'cl)

;; Customizable variables:
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar irsim-use-fancy-indent t
  "Specifies whether irsim-mode should use fancy indentation or
  plain. Default is fancy, but set it to nil for plan.")

(defvar irsim-tab-stops '(0 3 16 32 48 51)
  "The starting column positions for each entry in a transistor
  line using the fancy indent function. Change if you like.")


;; Some global variables

(defvar irsim-mode-version "0.1"
  "The current version number of irsim-mode.")

(defvar irsim-mode-hook nil)

(defconst irsim-font-lock-keywords
  (list
   '("^[\t ]*[np]" . font-lock-builtin-face)
   '("[a-zA-Z_][_-a-zA-Z0-9.]*" . font-lock-variable-name-face))
  "Minimal highlighting expressions for IRSIM mode")

;; Define comment behavior and make underscores act as normal
;; characters for forward-word and backward-word behavior. This makes
;; a lot more sense than the default.
(defvar irsim-mode-syntax-table
  (let ((irsim-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?| "<   " irsim-mode-syntax-table)
    (modify-syntax-entry ?\n ">   " irsim-mode-syntax-table)
    (modify-syntax-entry ?\^m ">   " irsim-mode-syntax-table)
    (modify-syntax-entry ?_ "w" irsim-mode-syntax-table)
    irsim-mode-syntax-table))


;; Indentation
;;;;;;;;;;;;;;

(defun irsim-line-type ()
  "Returns the type of line in IRSIM code that the point is
  on. Valid return values are 'transistor-line, 'comment-line, or
  nil"
  (save-excursion
    (move-beginning-of-line nil)
    (if (looking-at "^[\t ]*[np]")
	'transistor-line
	(if (looking-at "^[\t ]*\\|")
	    'comment-line))))

;; To indent a transistor line, just move the columns into place and
;; put the cursor at the last empty column. Or don't move it if there
;; are no more empty columns. Comment lines should not be indented.
(defun irsim-indent-line ()
  "Indent the current line of IRSIM code properly"
  (interactive)
  (let ((at-end nil))
    (if (looking-at "[\t ]*$")
	(setq at-end t))
    (if (eql (irsim-line-type) 'transistor-line)
	(progn
	  (save-excursion
	    (save-restriction
	      ;; Narrow to just this line
	      (narrow-to-region (line-beginning-position) (line-end-position))
	      (beginning-of-line)
	      (indent-line-to 0)
	      (let ((tab-stops (copy-seq irsim-tab-stops)))
		(while (and tab-stops (not (looking-at "[\t ]*$")))
		  (while (looking-at "[\t ]")
		    (delete-char 1))
		  (indent-to-column (car tab-stops))
		  (setq tab-stops (cdr tab-stops))
		  (forward-word))
		(when (and at-end tab-stops)
		  (message "yarr")
		  (end-of-line)
		  (indent-to-column (car tab-stops))))))
	  (if at-end (end-of-line)))
	(if (eql (irsim-line-type) 'comment-line)
	    (save-excursion
	      (beginning-of-line)
	      (indent-line-to 0))))))



(defun irsim-mode ()
  "Major mode for editing IRSIM netlists."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(irsim-font-lock-keywords))
  (set-syntax-table irsim-mode-syntax-table)
  (set (make-local-variable 'comment-start) "| ")

  (when irsim-use-fancy-indent
    (set (make-local-variable 'indent-line-function) 'irsim-indent-line))

  (setq mode-name "IRSIM"
	major-mode 'irsim-mode)
  (run-hooks 'irsim-mode-hook))

(provide 'irsim-mode)

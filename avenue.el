;;; avenue.el --- major mode for editing ESRI avenue scrips
;;; Copyright (C) 2002, Agnar Renolen <agnar.renolen@emap.no>

;; avenue.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; This is version 1.1 of 20 August 2002.

;;; Comentary:

;;  The avenue-mode was originally written as an excercise to learn
;;  Emacs Lisp and how to customize emacs with modes for new file types.
;;  The avenue-mode provides syntax-hilighting (provided that
;;  font-lock-mode is switched on) and auto-indentation of the code.
;;
;; Agnar Renolen

;;; Code:

(defgroup avenue nil
  "Major mode for editing avenue code"
  :prefix "avenue-"
  :group 'languages)

; (defvar avenue-mode-hook nil
;   "Hooks called when avenue mode fires up."
;   :type 'hook
;   :group 'avenue)

(defvar avenue-mode-map nil
  "Keymap used with avenue code")

(defcustom avenue-indent-level 4
  "Amount by which avenue subexpressions are indented."
  :type 'integer
  :group 'avenue)

(defvar avenue-font-lock-keywords
  (eval-when-compile
    (list
     (concat "\\<\\(b\\(reak\\|y\\)\\|continue\\"
	     "|e\\(ach\\|lse\\(\\|if\\)\\|nd\\|xit\\)\\|"
	     "for\\|i[fn]\\|return\\|then\\|while\\)\\>")
     '("\\<\\(nil\\|true\\false\\)\\>" . font-lock-constant-face))))

;;;###autoload
(defun avenue-mode ()
  "Major mode for editing avenue scripts."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'avenue-mode)
  (setq mode-name "Avenue")
  (set (make-local-variable 'indent-line-function) 'avenue-indent-line)
  (set (make-local-variable 'comment-start) "'")
  (set (make-local-variable 'comment-start-skip) "'+[ \t]*")
  (set (make-local-variable 'font-lock-defaults)
       '(avenue-font-lock-keywords nil t nil))
;  (set (make-local-variable 'paragraph-start) "[ \t\n\']")
;  (set (make-local-variable 'paragraph-separate) "[ \t]*'$\\|[ \t\]*$")
  (modify-syntax-entry ?' "<")
  (modify-syntax-entry ?\n ">")
  (run-hooks 'avenue-mode-hook))
  
(defun avenue-indent-line ()
  "Indent current line as avenue script"
  (let ((indent (avenue-calculate-indent))
	beg shift-amt 
	(old-pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (looking-at "e\\(nd\\|lse\\(\\|if\\)\\)\\>")
	(setq indent (max (- indent avenue-indent-level))))
    (message "prev indent: %d" indent)
    (setq shift-amt (- indent (current-column)))
    (if (not (zerop shift-amt))
	(progn
	  (delete-region beg (point))
	  ; ArcView replaces tabs with single spaces, so we only insert
	  ; spaces to make indentation correct in ArcView.
	  (insert-char ?  indent)
	  (if (> (- (point-max) old-pos) (point))
	      (goto-char (- (point-max) old-pos)))))
    shift-amt))
    
(defun avenue-calculate-indent ()
  "Return appropriate indentation for the current line as avenue code."
  (save-excursion
    (beginning-of-line)
    (current-indentation)
    (if (bobp)
	0
      (if (re-search-backward "^[ \t]*[^ \t\n\r]" nil t)
	  (if (looking-at "[ \t]*\\(else\\(\\|if\\)\\|for\\|if\\|while\\)\\>")
	      (+ (current-indentation) avenue-indent-level)
	    (current-indentation))
	0))))
  
(provide 'avenue-mode)

;;; avenue.el ends here

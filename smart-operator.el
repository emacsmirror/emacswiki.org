;;; smart-operator.el --- Beautify the operators in codes

;; Copyright (C) 2004, 2005 William XWL

;; Author: William XWL <william.xwl@gmail.com>
;; $Id: smart-operator.el,v 0.93 2005/09/19 01:04:05 xwl Exp $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package can automatically add whitespaces before and after operators.
;; e.g, "=" becomes " = ", "+=" becomes " += ". which will be handy for
;; writing C-style sources.

;; To use, set all the operators smart at one time, then ajust some at
;; your wish e.g.
;;
;; (defun my-c-mode-common-hook()
;;   (smart-insert-operator-hook)
;;
;;   (local-unset-key (kbd "."))
;;   (local-unset-key (kbd ":"))
;;   (local-set-key (kbd "*") 'c-electric-star))
;;
;; (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Besides `smart-insert-operator' when writing, there's also
;; `smart-beautify-operator' to beautify ugly codes, be careful there
;; are still some unresolved issues in `smart-beautify-operator'!

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'smart-operator)

;;; Change Log:

;; v 0.93 [2005/09/19 01:00:58] Some minor improvements on
;;        `smart-insert-operator'.

;; v 0.92 [2005/08/19 19:53:22] Fix bugs when at the beginning of line
;;        or beginning of buffer. Should insert only one whitespace at back
;;        here.

;; v 0.91 [2005/08/01 17:52:09] Correct some mistakes in Commentary.

;;; Todo:

;; Some unresolved issues in `smart-beautify-operator', such as:
;; 1. #include <stdio.h>
;; 2. printf("%s")

;;; Code:

;;; Variables:

(defvar smart-operator-alist
  '( "=" "<" ">" "%" "\\+" "-" "\\*" "/" "&" "|" "!" ":"))

;;; Functions:

(defun smart-insert-operator (op &optional only-back)
  "Automatically insert whitespaces before and after '=', '>=', etc.
Make it look nicer: ' = ', ' >= '.

OP is the operator. If optional ONLY-BACK is t, only insert one
whitespace at back. When confused, try C-q."
  (interactive "s")
  (delete-horizontal-space)
  (let ((op-p nil)			; another op at front?
	(one-char-back nil))		; have we stepped back one char?
    (unless (bolp)
      (backward-char)
      (setq one-char-back t))
    (setq op-p
	  (catch 'return
	    (dolist (front smart-operator-alist)
	      (when (looking-at front)
		(throw 'return t)))))
    (when (and (or op-p (not (and (bolp) (eolp))))
	       one-char-back)
      (forward-char))
    (if (or op-p only-back (bolp))
	(insert op)
      (insert (concat " " op))))
  (delete-horizontal-space)
  (insert " "))

(defun smart-operator-replace-regexp (regexp to-string &optional enhance)
  "Faster and won't set the mark or print anything.

If optional enhance is non-nil, call replace-regexp for its \& and \D
functionity."
  (save-excursion
    (goto-char (point-min))
    (if enhance
	(replace-regexp regexp to-string nil)
      (while (re-search-forward regexp nil t)
	(replace-match to-string nil t)))))

(defun smart-operator-replace (from to)
  "Faster and won't set the mark or print anything."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward from nil t)
      (replace-match to nil t))))

(defun smart-beautify-operator (beg end)
  "Beautify the codes to my style, such as add whitespaces before
and after operators. Three steps:

    1.\" =\", \"= \" --> \" = \"
    2.\"  \" --> \" \"
    3.\"> =\" --> \">=\"

Note: As replace method has been called two times, so you have to undo
TWO times to get back to the original state! And be careful to mark whole
buffer, as there are some unresolved issues, such as \"#include <stdio.h>\"
would become \" #include < stdio.h > \" incorrectly!"
  (interactive "r")
  (save-excursion
    (narrow-to-region beg end)
    (message "beautifying operators...")
    (mapcar* '(lambda (from) (smart-operator-replace from (concat " " from " ")))
	     '("=" ">" "<" "+" "-" "*" "/" "%" "|" "&" "!" ","))
    (smart-operator-replace-regexp "\\ +" " " t)
    (mapcar* '(lambda (from to) (smart-operator-replace from to))
	     '("> =" "< =" "= =" "+ +" "- -" "+ =" "- =" "* =" "/ =" "% ="
	       "| |" "& &" "! !" "| =" "& =" "! ="
	       " ," " / /" " / *" "* / " "* *" "< <")
	     '(">=" "<=" "==" "++" "--" "+=" "-=" "*=" "/=" "%="
	       "||" "&&" "!!" "|=" "&=" "!="
	       "," "//" "/*" "*/" "**" "<<"))
    (indent-region beg end nil)
    (message "beautifying operators...done")
    (widen)))

(defun smart-insert-operator-hook ()
"Set all the operators smart at one time, then you can ajust some by hand,
e.g.

(defun my-c-mode-common-hook()
  (smart-insert-operator-hook)

  (local-unset-key (kbd \".\"))
  (local-unset-key (kbd \":\"))
  (local-set-key (kbd \"*\") 'c-electric-star))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)"

  (local-set-key (kbd "=") (lambda () (interactive) (smart-insert-operator "=")))
  (local-set-key (kbd "+") (lambda () (interactive) (smart-insert-operator "+")))
  (local-set-key (kbd "-") (lambda () (interactive) (smart-insert-operator "-")))
  (local-set-key (kbd "/") (lambda () (interactive) (smart-insert-operator "/")))
  (local-set-key (kbd "%") (lambda () (interactive) (smart-insert-operator "%")))
  (local-set-key (kbd "&") (lambda () (interactive) (smart-insert-operator "&")))
  (local-set-key (kbd "*") (lambda () (interactive) (smart-insert-operator "*")))
  (local-set-key (kbd "!") (lambda () (interactive) (smart-insert-operator "!")))
  (local-set-key (kbd "|") (lambda () (interactive) (smart-insert-operator "|")))
  (local-set-key (kbd "<") (lambda () (interactive) (smart-insert-operator "<")))
  (local-set-key (kbd ">") (lambda () (interactive) (smart-insert-operator ">")))
  (local-set-key (kbd ",") (lambda () (interactive) (smart-insert-operator "," t)))
  (local-set-key (kbd ".") (lambda () (interactive) (smart-insert-operator "." t)))
;; (local-set-key (kbd "\;") (lambda () (interactive) (smart-insert-operator "\;" t)))
  (local-set-key (kbd ":") (lambda () (interactive) (smart-insert-operator ":" t))))

(provide 'smart-operator)

;;; smart-operator.el ends here

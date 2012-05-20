;;; linum-ace.el --- display line number as character like ace-jump-line-mode

;; Copyright 2012 Yen-Chin,Lee <coldnew>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords: converience
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/linum-ace.el
(defconst linum-ace-version "0.2")

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
;;
;; Display line number as character like ace-jump-line-mode.
;;
;;; Usage:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'linum-ace)
;;   (setq linum-format 'linum-ace)

;;; Code:

(eval-when-compile (require 'cl))
(require 'linum)

;;;; Faces

(defface linum-ace-face
  '((t :inherit linum :foreground "red"))
  "Face for displaying ace-jump-line-mode like character on linum."
  :group 'linum)

;;;; Global vars

(defvar linum-ace-alist nil
  "This var save the cons cell of (line-number . char-key), which
is generate by linum-ace-search-candidate.")

(defvar linum-ace-keys
  (nconc (number-sequence ?a ?z)
	 (number-sequence ?A ?Z))
  "The keys to show on left fringe for unempty lines,
each key should only an printable character.

By default, linum-ace use the same keys as ace-jump-mode, if you
customized your ace-jump-mode-move-keys and want to make
linum-ace show the same keys as ace-jump-mode, you can use
   (setq linum-ace-keys ace-jump-mode-move-keys)
you also can build your own keys if you only want to
lower case character and digits
   (setq linum-ace-keys (nconc (number-sequence ?0 ?9)
			       (number-sequence ?a ?z))")


;;;; Advices
(defadvice linum-update (around linum-ace-update activate)
  "This advice use for update linum-ace-alist."
  (linum-ace-update)
  ad-do-it)

;;;; Functions

(defun linum-ace-search-candidate ()
  "Search NONE-EMPTY-LINE in current window, and return the cons cell of
candiate position and key."
  (let* ((start-point (window-start (selected-window) ))
	 (end-point   (window-end   (selected-window) t)))
    (save-excursion
      (goto-char start-point)
      (loop while (search-forward-regexp "^." end-point t)
	    for i from 0 to (length linum-ace-keys)
	    collect (cons (line-number-at-pos (match-beginning 0)) (nth i linum-ace-keys))))))

(defun linum-ace-update ()
  "Update linum-ace-alist."
  (setq linum-ace-alist (linum-ace-search-candidate)))

(defun linum-ace (line-number)
  (let* ((linum-ace-char
	  (or
	   (cdr-safe (assoc line-number linum-ace-alist))
	   ?\s)))
    (propertize (format "%2s " (char-to-string linum-ace-char ))
		'face 'linum-ace-face)))

;;;; Commands

(defun linum-ace-jump (char)
  (interactive "cGo to Line: ")
  (let ((line-number (car (rassoc char linum-ace-alist))))
    (if line-number
	(goto-line line-number))))

(defun linum-ace-toggle ()
  "Toggle between linum-ace or default linum-format."
  (interactive)
  (setq linum-format
	(if (eq linum-format 'dynamic) 'linum-ace 'dynamic)))



(provide 'linum-ace)
;; linum-ace.el ends here.

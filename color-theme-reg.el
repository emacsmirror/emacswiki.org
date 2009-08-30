;;; color-theme-reg.el --- is modified based on color-theme-x.el, convert color themes to win32 register settings. 
;; Modifier: Kyle M. Lee <mail2kyle@gmail.com>

;;; color-theme-x.el --- convert color themes to X11 resource settings

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Matthew Kennedy <mkennedy@killr.ath.cx>
;; Keywords: convenience, faces, frames

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; $Id: color-theme-x.el,v 1.2 2004/01/04 03:58:15 mkennedy Exp $

;; Given the name of a color theme, write out the .Xresources
;; equivalent of the theme.  You would want to do this if you want to
;; make Emacs start much faster than loading the theme from Lisp.

;; The approach taken here is to `read' the color-themes function out
;; of the color-themes source and then descend that stucture to print
;; out the .Xresource lines.

;; For those puzzling over the code:

;; The code makes a slight distiction between face settings and
;; "basic" settings.  Basic settings are: background-color,
;; foreground-color, cursor-color -- these attributes do not have same
;; possibilities that Emacs face attributes have.

;; Example usage:
;;
;;     M-x color-theme-x RET hober RET
;;     C-u M-x color-theme-x RET classic RET ~/elisp/color-theme.el RET
;;
;; Then if necessary, adjust the output in the
;; *color-theme-xresources* to taste, copy it to your ~/.Xresources
;; (don't use .Xdefaults -- its obsolete) and run:
;;
;;     xrdb -load ~/.Xresources
;; or
;;     xrdb -merge ~/.Xresources 
;;
;; (Depending on what is desired).  Then restart Emacs.
;;

;;; Code:

(require 'cl)
(require 'time-stamp)

(defvar color-theme-x-color-theme-source nil
  "*The full path to your color-theme.el.  
If this variable is nil, then the default is to discover the path
via `locate-library'")

(defvar color-theme-x-supported-attributes 
  '((:foreground . "Foreground")
    (:background . "Background")
    (:bold . "Bold")
    (:italic . "Italic")
    (:underline . "Underline")))

(defvar color-theme-x-supported-basic-attibutes
  '((background-color . "background")
    (foreground-color . "foreground")
    (mouse-color . "pointerColor")
    (cursor-color . "cursorColor")
    (border-color . "borderColor")))

(defvar color-theme-x-output-buffer-name "*color-theme-xresources*")

(defvar color-theme-x-output-buffer nil)

(defun color-theme-x-locate-color-theme-source ()
  (or color-theme-x-color-theme-source (locate-library "color-theme.el")))

(defun color-theme-x-read-theme (name &optional source)
  (save-excursion
    (with-temp-buffer 
      (insert-file-contents-literally (or source (color-theme-x-locate-color-theme-source)))
      (goto-char 0)
      (when (and (search-forward-regexp (concat "^(defun color-theme-" name) (point-max) t)
		 (search-forward "color-theme-install")) 
	(next-line 0)
	(let ((function (read (current-buffer))))
	  ;; muahahahaaa
	  (values (cdadr function)
		  (cadadr function)))))))

(defun color-theme-x-list-to-paired-list (list)
  (let ((l list)
	(resultant nil))
    (while l
      (let ((attribute (car l))
	    (value (cadr l)))
	(setq resultant (cons (cons attribute value) resultant))
	(setq l (cddr l))))
    (nreverse resultant)))

(defun color-theme-x-traverse-theme (theme function)
  (dolist (e theme)
    (ignore-errors 
      (destructuring-bind (face-name ((true face-attributes))) e
	(if (and (symbolp face-name)
		 (eq true t)
		 (listp face-attributes))
	    ;; it looks like we have found something like 
	    ;; (face-name ((t (:foreground "white"))))
	    (funcall function 
		     (symbol-name face-name)
		     (color-theme-x-list-to-paired-list face-attributes)))))))

(defun color-theme-x-traverse-basic-theme (theme function)
  (dolist (e theme)
    (let ((name (cdr (assoc (car e) color-theme-x-supported-basic-attibutes)))
	  (value (cdr e)))
      (when name
	(funcall function name (cdr e))))))

(defun color-theme-x-lisp-to-resource (value)
  "Convert lisp symbols to X resource values."
  (cond ((eq value t)
	 "on")
	(t 
	 value)))
    
(defun color-theme-x-xresource-writer (face-name attributes)
  (dolist (a attributes)
    (let ((attribute (car a))
	  (value (cdr a)))
      (let ((xresource-attribute (cdr (assoc attribute color-theme-x-supported-attributes))))
	(when xresource-attribute
	  (insert (format "\"Emacs.%s.attribute%s\"=\"%s\"\n" 
			  face-name 
			  xresource-attribute 
			  (color-theme-x-lisp-to-resource value))))))))

(defun color-theme-x-basic-xresource-writer (name value)
  (insert (format "\"Emacs.%s\"=\"%s\"\n" name value)))

(defun color-theme-reg (theme-name &optional theme-source)
  (interactive
   (list (read-string "Name of theme: ")
	 (and current-prefix-arg (read-file-name "Path to theme source: "))))
  (setq theme-source (or theme-source (color-theme-x-locate-color-theme-source)))
  (save-excursion
    (setq color-theme-x-output-buffer 
	  (get-buffer-create (or color-theme-x-output-buffer-name "*color-theme-xresources*")))
    (set-buffer color-theme-x-output-buffer)
    (goto-char (point-max))
    (beginning-of-line)
    (insert "Windows Registry Editor Version 5.00")
    (insert "\n# X resources for color-theme-" theme-name)
    (insert "\n# Generated by ctresource " (time-stamp-string) "\n\n")
    (insert "\n[HKEY_LOCAL_MACHINE\\SOFTWARE\\GNU\\Emacs]\n")
    (multiple-value-bind (face-resources basic-resources)
	(color-theme-x-read-theme theme-name theme-source)
      (color-theme-x-traverse-basic-theme basic-resources 'color-theme-x-basic-xresource-writer)
      (color-theme-x-traverse-theme face-resources 'color-theme-x-xresource-writer))
    (pop-to-buffer color-theme-x-output-buffer)))

(provide 'color-theme-reg)
;;; color-theme-reg.el ends here

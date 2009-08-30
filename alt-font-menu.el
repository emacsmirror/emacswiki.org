;;; alt-font-menu.el --- alternative (auto generated) font menu for x

;; Copyright (c) 2001, 2002  Free Software Foundation, Inc.

;; Author: kahlil (kal) hodgson <dorge@tpg.com.au>
;; Keywords: mouse, convenience
;; Time-stamp: <2002-09-30 13:22:54 kahlil>
;; X-URL: http://www.emacswiki.org/elisp/alt-font-menu.el
;; Version: 0.4

;; This file is NOT part of GNU Emacs.

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

;; This file is not part of gnu emacs (yet).

;; Automatically generate a menu of available fonts constrained by
;; personal preferences, and without increasing frame size. This menu
;; is perpended to the options menu but may also be accessed as a
;; pop-up bound to shift-mouse-1 (see below). Requires at least gnu
;; emacs 21.1 and probably wont work on xemacs:-(

;;; Installation:

;; Place this file somewhere in your load path and the following
;; somewhere in your .emacs file

;;   (autoload 'alt-mouse-set-font "alt-font-menu"
;;       "interactively choose font using mouse" t)
;;   (global-set-key [(shift down-mouse-1)] 'alt-mouse-set-font)

;; and set the parameter constraint variables to your personal
;; preferences e.g.

;; (setq afm-max-pixels 18)
;; (setq afm-min-pixels 10)
;; (setq afm-weight "medium")
;; (setq afm-slant "r")
;; (setq afm-width "normal")
;; (setq afm-charset "iso8859-1")
;;
;; These may be accessed via m-x customize-group <ret> afm <ret>

;;; code:

(defgroup afm nil
  "Variables used to hold default font spec constraints. Only fonts
matching these constraints will be added to your menu.")

(defcustom afm-foundry	"*"
  "Font foundry constraint for font menu."
  :group 'afm)
(defcustom afm-family	"*"
  "Font family constraint for font menu."
  :group 'afm)
(defcustom afm-weight	"medium"
  "Font weight constraint for font menu."
  :group 'afm)
(defcustom afm-slant	"r"
  "Font slant constraint for font menu."
  :group 'afm)
(defcustom afm-width	"normal"
  "Font width constraint for font menu."
  :group 'afm)
(defcustom afm-add-style	"*"
  "Font add-style constraint for font menu."
  :group 'afm)
(defcustom afm-point-size	"*"
  "Font point-size constraint for font menu.
best to leave this if you are using `afm-max-pixels' and `afm-min-pixels'."
  :group 'afm)
(defcustom afm-pixel-size	"*"
  "Font pixel-size constraint for font menu."
  :group 'afm)
(defcustom afm-resx	"*"
  "Font x resolution constraint for font menu."
  :group 'afm)
(defcustom afm-resy	"*"
  "Font y resolution constraint for font menu."
  :group 'afm)
(defcustom afm-spacing	"*"
  "Font spacing constraint for font menu."
  :group 'afm)
(defcustom afm-av-width	"*"
  "Font average width constraint for font menu."
  :group 'afm)
(defcustom afm-charset	"iso8859-1"
  "Font character set constraint for font menu."
  :group 'afm)

(defcustom afm-max-pixels 20
  "Maximum pixel size of font presented in the font menu. "
  :group 'afm)

(defcustom afm-min-pixels 12
  "Minimum pixel size of font presented in the font menu. "
  :group 'afm)

(defsubst afm-leader
  (&optional foundry family weight slant width add-style)
  "Return the portion of the font spec preceded the pixel size."
  (mapconcat 'concat
	     (list
	      ""
	      (or foundry   afm-foundry)
	      (or family    afm-family)
	      (or weight    afm-weight)
	      (or slant     afm-slant)
	      (or width     afm-width)
	      (or add-style afm-add-style)
	      ""
	      )
	     "-" ))

(defsubst afm-trailer
  (&optional point-size resx resy spacing av-width charset)
  "Return the portion of the font spec following the pixel size."
  (mapconcat 'concat
	     (list
	      ""
	      (or point-size afm-point-size)
	      (or resx	     afm-resx)
	      (or resy	     afm-resy)
	      (or spacing    afm-spacing)
	      (or av-width   afm-av-width)
	      (or charset    afm-charset)
	      )
	     "-"))

(defun afm-set-font  (font)
"Change this frames default font to font.  Attempts to resize the
frame so that it is no larger than the original."
  (let ((old-pix-height (* (frame-char-height) (frame-height)))
	(old-pix-width  (* (frame-char-width) (frame-width))))
    ;; remember frame size
    ;; change the font
    (set-default-font font)

    ;; resize the frame
    (let* ((new-char-height (frame-char-height))
	   (new-char-width  (frame-char-width))
	   (new-width  (/ old-pix-width new-char-width))
	   (new-height (/ old-pix-height new-char-height)))

      ;; adjust out ensuring new frame is no larger then the original
      (if (<= (* (1+ new-width) new-char-width) old-pix-width)
	  (setq new-width (1+ new-width)))

      (if (<= (* (1+ new-height) new-char-height) old-pix-height)
	  (setq new-height (1+ new-height)))

      (set-frame-size (selected-frame) new-width new-height)
      )))

;;;
;;; set up a pop-up menu for the mouse
;;;

(defun afm-fonts (&optional
		  foundry family weight slant width add-style
		  point-size resx resy spacing av-width charset)
  "Return a structured listing of fonts from family.
Only existing fonts between `afm-min-pixels' and `afm-max-pixels'
are returned."
  (let ((size afm-min-pixels)
	(leader (afm-leader foundry family weight slant width add-style))
	(trailer (afm-trailer point-size resx resy spacing av-width charset))
	(font-list))

    (while (<= size afm-max-pixels)
      (let ((matching-fonts
	     (x-list-fonts (concat leader (number-to-string size) trailer))))
	(while (car matching-fonts)
	  (setq font-list
		(append font-list
			(list (list (concat (number-to-string size) " pixels")
				    (car matching-fonts)))))
	  (setq matching-fonts (cdr matching-fonts)))
	(setq size (1+ size))))
    font-list))

(defvar x-font-alist "")

;; construct a menu of fonts subject to the given constraints
(progn
  (setq x-font-alist '())
  (let ((families (x-font-family-list))
	(family)
	(font-list)
	(p-font-alist)
	(np-font-alist))
    (while (setq family (car families))
      (if (setq font-list (afm-fonts nil (car family)))
	  (if (cdr family)
	      ;; proportional fonts
	      (setq p-font-alist
		    (append p-font-alist
			    (list (cons (car family) font-list))))
	    ;; non-proportional fonts
	    (setq np-font-alist
		  (append np-font-alist
			  (list (cons (car family) font-list))))
	    ))
      (setq families (cdr families)))
    (setq x-font-alist
	  (cons "font menu"
		(append '(("proportional fonts" nil))
			'(("----" nil))
			p-font-alist
			'(("----" nil))
			'(("non-proportional fonts" nil))
			'(("----" nil))
			np-font-alist)))
    ))

(defun alt-mouse-set-font (&rest fonts)
  "Select an emacs font from a list of existent fonts and fontsets.
Attempts to resize the frame so that it is no larger than the
original."
  (interactive
   (x-popup-menu
    last-nonmenu-event x-font-alist))
  (if fonts
      (let (font)
	(while fonts
	  (condition-case nil
	      (progn
		;; change the font
		(afm-set-font (car fonts))
		(setq font (car fonts))
		(setq fonts nil))
	    (error
	     (setq fonts (cdr fonts)))))
	(if (null font)
	    (error "font not found")))))

;;;
;;; set up a menu for the "options" menu
;;;

(defun afm-menu-fonts (&optional
		  foundry family weight slant width add-style
		  point-size resx resy spacing av-width charset)
  "Return a list of menu items for fonts from family.
Only existing fonts between `afm-min-pixels' and `afm-max-pixels'
are returned."
  (let ((size afm-min-pixels)
	(leader (afm-leader foundry family weight slant width add-style))
	(trailer (afm-trailer point-size resx resy spacing av-width charset))
	(font-list))

    (while (<= size afm-max-pixels)
      (let ((matching-fonts
	     (x-list-fonts (concat leader
				   (number-to-string size) trailer))))
	(while (car matching-fonts)
	  (setq font-list
		(append font-list
			(list
			 (vector
			  (concat (number-to-string size) " pixels")
			  (list 'afm-set-font (car matching-fonts))))))
	  (setq matching-fonts (cdr matching-fonts)))
	(setq size (1+ size))))
    font-list))

(defvar afm-menu-symbol nil)
(defvar afm-menu nil)

;; construct a menu of fonts subject to the given constraints
;; small structural difference for menu bar menus
(progn
  (setq afm-menu '())
  (let ((families (x-font-family-list))
	(family)
	(font-list)
	(p-font-alist)
	(np-font-alist))
    (while (setq family (car families))
      (if (setq font-list (afm-menu-fonts nil (car family)))
	  (if (cdr family)
	      ;; proportional fonts
	      (setq p-font-alist
		    (append p-font-alist
			    (list (cons (car family) font-list))))
	    ;; non-proportional fonts
	    (setq np-font-alist
		  (append np-font-alist
			  (list (cons (car family) font-list))))
	    ))
      (setq families (cdr families)))
    (setq afm-menu (append '("proportional fonts")
			   '("----")
			   p-font-alist
			   '("----")
			   '("non-proportional fonts")
			   '("----")
			   np-font-alist))
    ))

(condition-case nil
    (progn (require 'easymenu)
	   (easy-menu-change '("Options") "Change default font" afm-menu
			     "toggle-global-lazy-font-lock-mode"))
  (error
   (message
    "afm requires the `easymenu' package in order to alter options menu.")))

(provide 'alt-font-menu)

;;; alt-font-menu.el ends here

;;; pos-tip.el -- Show tooltip at point

;; Copyright (C) 2010 S. Irie

;; Author: S. Irie
;; Maintainer: S. Irie
;; Keywords: Tooltip

(defconst pos-tip-version "0.0.1")

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; The standard library tooltip.el provides the function for displaying
;; a tooltip at mouse position which allows users to easily show it.
;; However, locating tooltip at arbitrary buffer position in window
;; is not easy. This program provides such function to be used by other
;; frontend programs.

;;
;; Installation:
;;
;; First, save this file as pos-tip.el and byte-compile in
;; a directory that is listed in load-path.
;;
;; Put the following in your .emacs file:
;;
;;   (require 'pos-tip)
;;
;; We can display a tooltip at POS in WINDOW by following:
;;
;;   (pos-tip "foo bar" POS WINDOW)
;;
;; Here, POS and WINDOW can be omitted, means use current position
;; and selected window, respectively.
;;

;;; History:
;; 2010-03-07  S. Irie
;;         * First release
;;         * Version 0.0.1

;; ToDo:

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pos-tip-border-width 1
  "Outer border width of pos-tip's tooltip.")
(defvar pos-tip-internal-border-width 2
  "Text margin of pos-tip's tooltip.")

(defface pos-tip
  '((((class color))
     :foreground "black"
     :background "lightyellow"
     :inherit variable-pitch)
    (t
     :inherit variable-pitch))
  "Face for pos-tip's tooltip."
  :group 'pos-tip)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pos-tip-saved-frame-coordinates '(0 . 0))

(defun pos-tip-frame-top-left-coordinates (&optional frame)
  "Return the pixel coordinates of FRAME as a cons cell (LEFT . TOP),
which are relative to top left corner of screen.

If FRAME is omitted, use selected-frame."
  (with-current-buffer (get-buffer-create "*xwininfo*")
    (let ((case-fold-search nil))
      (buffer-disable-undo)
      (erase-buffer)
      (call-process shell-file-name nil t nil shell-command-switch
		    (concat "xwininfo -id " (frame-parameter frame 'window-id)))
      (goto-char (point-min))
      (setq pos-tip-saved-frame-coordinates
	    (cons (progn (string-to-number (buffer-substring-no-properties
					    (search-forward "X: ")
					    (line-end-position))))
		  (progn (string-to-number (buffer-substring-no-properties
					    (search-forward "Y: ")
					    (line-end-position)))))))))

(defun pos-tip-compute-pixel-position
  (&optional pos window pixel-width pixel-height frame-coordinates)
  "Return the screen pixel position of POS in WINDOW as a cons cell (X . Y).
Its values show the coordinates of lower left corner of the character.

Omitting POS and WINDOW means use current position and selected window,
respectively.

If PIXEL-WIDTH and PIXEL-HEIGHT are given, this function assumes these
values as the size of small window like tooltip which is located around the
point character. These value are used to adjust the position in order that
the window doesn't disappear by sticking out of the display.

FRAME-COORDINATES specifies the pixel coordinates of top left corner of the
target frame as a cons cell like (LEFT . TOP). If omitted, it's automatically
obtained by `pos-tip-frame-top-left-coordinates', but slightly slower than
when explicitly specified. Users can get the latest frame coordinates for
next call by referring the variable `pos-tip-saved-frame-coordinates' just
after calling this function."
  (unless frame-coordinates
    (pos-tip-frame-top-left-coordinates
     (window-frame (or window (selected-window)))))
  (let* ((x-y (or (pos-visible-in-window-p (or pos (window-point window)) window t)
		  '(0 0)))
	 (ax (+ (car pos-tip-saved-frame-coordinates)
		(car (window-inside-pixel-edges))
		(car x-y)))
	 (ay (+ (cdr pos-tip-saved-frame-coordinates)
		(cadr (window-pixel-edges))
		(cadr x-y)))
	 ;; `posn-object-width-height' returns an incorrect value
	 ;; when the header line is displayed (Emacs bug #4426).
	 ;; In this case, `frame-header-height' is used substitutively,
	 ;; but this function doesn't return actual character height.
	 (char-height (or (and header-line-format
			       (frame-char-height))
			  (cdr (posn-object-width-height
				(posn-at-x-y (max (car x-y) 0) (cadr x-y)))))))
    (cons (min ax (- (x-display-pixel-width) (or pixel-width 0)))
	  (if (> (+ ay char-height (or pixel-height 0)) (x-display-pixel-height))
	      (- ay (or pixel-height 0))
	    (+ ay char-height)))))

(defun pos-tip-cancel-timer ()
  "Cancel timeout of tooltip."
  (mapc (lambda (timer)
	  (if (eq (aref timer 5) 'x-hide-tip)
	      (cancel-timer timer)))
	timer-list))

(defun pos-tip-show-no-propertize
  (string &optional tip-color pos window timeout pixel-width pixel-height frame-coordinates)
  "Show STRING in a tooltip at POS in WINDOW.
Analogous to `pos-tip-show' except don't propertize STRING by `pos-tip' face.

TIP-COLOR is a face or a cons cell like (FOREGROUND-COLOR . BACKGROUND-COLOR)
used to specify *only* foreground-color and background-color of tooltip.
If omitted, use sysytem's default colors instead.

Example:

 (defface my-tooltip
   '((t
      :background \"gray85\"
      :foreground \"black\"
      :inherit variable-pitch))
   \"Face for my tooltip.\")

 (defface my-tooltip-highlight
   '((t
      :background \"blue\"
      :foreground \"white\"
      :inherit my-tooltip))
   \"Face for my tooltip highlighted.\")

 (let ((str (propertize \" foo \\n bar \\n baz \" 'face 'my-tooltip)))
   (put-text-property 6 11 'face 'my-tooltip-highlight str)
   (pos-tip-show-no-propertize str 'my-tooltip))

See `pos-tip-show' for details."
  (let* ((x-y (pos-tip-compute-pixel-position pos window pixel-width pixel-height
					      frame-coordinates))
	 (ax (car x-y))
	 (ay (cdr x-y))
	 (fg (or (car-safe tip-color)
		 (face-attribute tip-color :foreground)))
	 (bg (or (cdr-safe tip-color)
		 (face-attribute tip-color :background)))
	 (frame (window-frame (or window (selected-window))))
	 (x-max-tooltip-size
	  (cons (or (and pixel-width
			 (1+ (/ pixel-width (frame-char-width frame))))
		    (car x-max-tooltip-size))
		(max (1+ (/ (x-display-pixel-height)
			    (+ (frame-char-height frame)
			       (or (default-value 'line-spacing) 0))))
		     (cdr x-max-tooltip-size)))))
    (x-show-tip string
		(window-frame (or window (selected-window)))
		`((border-width . ,pos-tip-border-width)
		  (internal-border-width . ,pos-tip-internal-border-width)
		  (left . ,ax)
		  (top . ,ay)
		  ,@(and (stringp fg) `((foreground-color . ,fg)))
		  ,@(and (stringp bg) `((background-color . ,bg))))
		(and timeout (> timeout 0) timeout))
    (if (and timeout (<= timeout 0))
	(pos-tip-cancel-timer))
    (cons (- ax (car pos-tip-saved-frame-coordinates))
	  (- ay (cdr pos-tip-saved-frame-coordinates)))))

(defun pos-tip-show
  (string &optional pos window timeout pixel-width pixel-height frame-coordinates)
  "Show STRING in a tooltip at POS in WINDOW. The tooltip is a small
X window. Automatically hide the tooltip after TIMEOUT seconds.

Return pixel position of tooltip relative to top left corner of frame as
a cons cell like (X . Y).

Omitting POS and WINDOW means use current position and selected window,
respectively.

Omitting TIMEOUT means use the default timeout of 5 seconds.
Not-positive TIMEOUT means don't hide tooltip automatically.

If PIXEL-WIDTH and PIXEL-HEIGHT are given, they specify the size of tooltip,
which will be located around the point character, and are used to adjust
the position in order that the window doesn't disappear by sticking out of
the display. Note that this function does't calculate tooltip's size itself,
so user should calculate these values by using `pos-tip-tooltip-width' and
`pos-tip-tooltip-height'.

FRAME-COORDINATES specifies the pixel coordinates of top left corner of the
target frame as a cons cell like (LEFT . TOP). If omitted, it's automatically
obtained by `pos-tip-frame-top-left-coordinates', but slightly slower than
when explicitly specified. Users can get the latest frame coordinates for
next call by referring the variable `pos-tip-saved-frame-coordinates' just
after calling this function."
  (pos-tip-show-no-propertize (propertize string 'face 'pos-tip) 'pos-tip
			      pos window timeout pixel-width pixel-height
			      frame-coordinates))

(defalias 'pos-tip-hide 'x-hide-tip
  "Hide pos-tip's tooltip.")

(defun pos-tip-tooltip-width (width char-width)
  "Calculate tooltip pixel width."
  (+ (* width char-width)
     (ash (+ pos-tip-border-width
	     pos-tip-internal-border-width)
	  1)))

(defun pos-tip-tooltip-height (height char-height)
  "Calculate tooltip pixel height."
  (+ (* height (+ char-height
		  (or (default-value 'line-spacing) 0)))
     (ash (+ pos-tip-border-width
	     pos-tip-internal-border-width)
	  1)))


(provide 'pos-tip)

;;;
;;; pos-tip.el ends here

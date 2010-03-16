;;; pos-tip.el -- Show tooltip at point -*- coding: utf-8 -*-

;; Copyright (C) 2010 S. Irie

;; Author: S. Irie
;; Maintainer: S. Irie
;; Keywords: Tooltip

(defconst pos-tip-version "0.1.3")

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

;; *** Note that this program can work only under X window system. ***

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
;;   (pos-tip-show "foo bar" '(FG-COLOR . BG-COLOR) POS WINDOW)
;;
;; Here, '(FG-COLOR . BG-COLOR), POS and WINDOW can be omitted, means
;; use default colors, current position and selected window, respectively.
;;

;;; History:
;; 2010-03-16  S. Irie
;;         * Bug fix
;;         * Changed calculation for `x-max-tooltip-size'
;;         * Modified docstring
;;         * Version 0.1.3
;;
;; 2010-03-11  S. Irie
;;         * Modified commentary
;;         * Version 0.1.2
;;
;; 2010-03-11  S. Irie
;;         * Re-implemented `pos-tip-string-width-height'
;;         * Added indicator variable `pos-tip-upperside-p'
;;         * Version 0.1.1
;;
;; 2010-03-09  S. Irie
;;         * Re-implemented `pos-tip-show' (*incompatibly changed*)
;;             - Use frame default font
;;             - Automatically calculate tooltip pixel size
;;             - Added optional arguments: TIP-COLOR, MAX-WIDTH
;;         * Added utility functions:
;;             `pos-tip-split-string', `pos-tip-string-width-height'
;;         * Bug fixes
;;         * Version 0.1.0
;;
;; 2010-03-08  S. Irie
;;         * Added optional argument DX
;;         * Version 0.0.4
;;
;; 2010-03-08  S. Irie
;;         * Bug fix
;;         * Version 0.0.3
;;
;; 2010-03-08  S. Irie
;;         * Modified to move out mouse pointer
;;         * Version 0.0.2
;;
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

(defvar pos-tip-foreground-color "black"
  "Default foreground color of pos-tip's tooltip.")

(defvar pos-tip-background-color "lightyellow"
  "Default background color of pos-tip's tooltip.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pos-tip-saved-frame-coordinates '(0 . 0))

(defun pos-tip-frame-top-left-coordinates (&optional frame)
  "Return the pixel coordinates of FRAME as a cons cell (LEFT . TOP),
which are relative to top left corner of screen.

If FRAME is omitted, use selected-frame.

Users can also get the frame coordinates by referring the variable
`pos-tip-saved-frame-coordinates' just after calling this function."
  (with-current-buffer (get-buffer-create "*xwininfo*")
    (let ((case-fold-search nil))
      (buffer-disable-undo)
      (erase-buffer)
      (call-process shell-file-name nil t nil shell-command-switch
		    (concat "xwininfo -id " (frame-parameter frame 'window-id)))
      (goto-char (point-min))
      (search-forward "\n  Absolute")
      (setq pos-tip-saved-frame-coordinates
	    (cons (progn (string-to-number (buffer-substring-no-properties
					    (search-forward "X: ")
					    (line-end-position))))
		  (progn (string-to-number (buffer-substring-no-properties
					    (search-forward "Y: ")
					    (line-end-position)))))))))

(defvar pos-tip-upperside-p nil)

(defun pos-tip-compute-pixel-position
  (&optional pos window pixel-width pixel-height frame-coordinates dx)
  "Return the screen pixel position of POS in WINDOW as a cons cell (X . Y).
Its values show the coordinates of lower left corner of the character.

Omitting POS and WINDOW means use current position and selected window,
respectively.

If PIXEL-WIDTH and PIXEL-HEIGHT are given, this function assumes these
values as the size of small window like tooltip which is located around the
point character. These value are used to adjust the position in order that
the window doesn't disappear by sticking out of the display. By referring
the variable `pos-tip-upperside-p' after calling this function, user can
examine whether the window will be located above POS.

If FRAME-COORDINATES is omitted, automatically obtain the absolute
coordinates of the top left corner of frame which WINDOW is on. Here,
`top left corner of frame' represents the origin of `window-pixel-edges'
and its coordinates are essential for calculating the return value. If
non-nil, specifies the frame location as a cons cell like (LEFT . TOP).
This option makes the calculations slightly faster, but can be used only
when it's clear that frame is in the specified position. Users can get the
latest values of frame location for using in the next call by referring the
variable `scim-saved-frame-coordinates' just after calling this function.

DX specifies horizontal offset in pixel."
  (unless frame-coordinates
    (pos-tip-frame-top-left-coordinates
     (window-frame (or window (selected-window)))))
  (let* ((x-y (or (pos-visible-in-window-p (or pos (window-point window)) window t)
		  '(0 0)))
	 (ax (+ (car pos-tip-saved-frame-coordinates)
		(car (window-inside-pixel-edges))
		(car x-y)
		(or dx 0)))
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
    (cons (max 0 (min ax (- (x-display-pixel-width) (or pixel-width 0))))
	  (if (setq pos-tip-upperside-p
		    (> (+ ay char-height (or pixel-height 0))
		       (x-display-pixel-height)))
	      (max 0 (- ay (or pixel-height 0)))
	    (+ ay char-height)))))

(defun pos-tip-cancel-timer ()
  "Cancel timeout of tooltip."
  (mapc (lambda (timer)
	  (if (eq (aref timer 5) 'x-hide-tip)
	      (cancel-timer timer)))
	timer-list))

(defun pos-tip-avoid-mouse (left right top bottom &optional frame)
  "Move out mouse pointer if it is inside region (LEFT RIGHT TOP BOTTOM)
in FRAME."
  (let* ((mpos (mouse-pixel-position))
	 (mframe (pop mpos))
	 (mx (car mpos))
	 (my (cdr mpos)))
    (when (numberp mx)
      (let* ((large-number (+ (x-display-pixel-width) (x-display-pixel-height)))
	     (dl (if (> left 2)
		     (1+ (- mx left))
		   large-number))
	     (dr (if (< (1+ right) (x-display-pixel-width))
		     (- right mx)
		   large-number))
	     (dt (if (> top 2)
		     (1+ (- my top))
		   large-number))
	     (db (if (< (1+ bottom) (x-display-pixel-height))
		     (- bottom my)
		   large-number))
	     (d (min dl dr dt db)))
	(when (and mpos
		   (eq (or frame (selected-frame)) mframe)
		   (> d -2))
	  (cond
	   ((= d dl)
	    (set-mouse-pixel-position mframe (- left 2) my))
	   ((= d dr)
	    (set-mouse-pixel-position mframe (1+ right) my))
	   ((= d dt)
	    (set-mouse-pixel-position mframe mx (- top 2)))
	   (t
	    (set-mouse-pixel-position mframe mx (1+ bottom)))))))))

(defvar pos-tip-default-char-width-height
  (let ((f (x-create-frame '((visibility . nil)
			     (minibuffer . nil)
			     (menu-bar-lines . nil)
			     (tool-bar-lines . nil)
			     (vertical-scroll-bars . nil)))))
    (prog1
	(with-selected-frame f
	  (cons (frame-char-width) (frame-char-height)))
      (delete-frame f))))

(defun pos-tip-show-no-propertize
  (string &optional tip-color pos window timeout pixel-width pixel-height frame-coordinates dx)
  "Show STRING in a tooltip at POS in WINDOW.
Analogous to `pos-tip-show' except don't propertize STRING by `pos-tip' face.

PIXEL-WIDTH and PIXEL-HEIGHT specify the size of tooltip, if given. These
are used to adjust the tooltip position in order that it doesn't disappear by
sticking out of the display, and also used to prevent it from vanishing by
overlapping with mouse pointer.

Note that this function itself doesn't calculate tooltip size because the
character width and height specified by faces are unknown. So users should
calculate PIXEL-WIDTH and PIXEL-HEIGHT by using `pos-tip-tooltip-width' and
`pos-tip-tooltip-height', or use `pos-tip-show' instead, which can
automatically calculate tooltip size.

See `pos-tip-show' for details.

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
   (pos-tip-show-no-propertize str 'my-tooltip))"
  (let* ((x-y (pos-tip-compute-pixel-position pos window pixel-width pixel-height
					      frame-coordinates dx))
	 (ax (car x-y))
	 (ay (cdr x-y))
	 (rx (- ax (car pos-tip-saved-frame-coordinates)))
	 (ry (- ay (cdr pos-tip-saved-frame-coordinates)))
	 (fg (or (and (facep tip-color)
		      (face-attribute tip-color :foreground))
		 (car-safe tip-color)
		 pos-tip-foreground-color))
	 (bg (or (and (facep tip-color)
		      (face-attribute tip-color :background))
		 (cdr-safe tip-color)
		 pos-tip-background-color))
	 (frame (window-frame (or window (selected-window))))
	 (x-max-tooltip-size
	  (cons (1+ (/ (or pixel-width
			   (x-display-pixel-width))
		       (car pos-tip-default-char-width-height)))
		(1+ (/ (or pixel-height
			   (x-display-pixel-height))
		       (cdr pos-tip-default-char-width-height))))))
    (and pixel-width pixel-height
	 (pos-tip-avoid-mouse rx (+ rx pixel-width)
			      ry (+ ry pixel-height)
			      frame))
    (x-show-tip string frame
		`((border-width . ,pos-tip-border-width)
		  (internal-border-width . ,pos-tip-internal-border-width)
		  (left . ,ax)
		  (top . ,ay)
		  ,@(and (stringp fg) `((foreground-color . ,fg)))
		  ,@(and (stringp bg) `((background-color . ,bg))))
		(and timeout (> timeout 0) timeout))
    (if (and timeout (<= timeout 0))
	(pos-tip-cancel-timer))
    (cons rx ry)))

(defun pos-tip-split-string (string &optional max-width left-margin)
  "Split STRING into fixed width strings. Return a list of these strings.

MAX-WIDTH specifies maximum column number of each row. MAX-WIDTH nil means
use display-width. Note that this function doesn't add any padding characters
at the end of the result.

The optional 3rd arg LEFT-MARGIN, if non-nil, specifies number of spece
characters to add at the beginning of each row."
  (let* ((display-width (/ (x-display-pixel-width) (frame-char-width)))
	 (width (or (and max-width
			 (min max-width display-width))
		    display-width))
	 (margin (and left-margin
		      (make-string left-margin ?\s)))
	 line row rows)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (end-of-line)
      (while (prog2
		 (let ((string (buffer-substring
				(line-beginning-position) (point))))
		   (while (progn
			    (setq string (concat margin string)
				  row (truncate-string-to-width string width)
				  rows (cons row rows))
			    (if (not (= (length row) (length string)))
				(setq string (substring string (length row)))))))
		 (< (point) (point-max))
	       (end-of-line 2))))
    (nreverse rows)))


(defun pos-tip-string-width-height (string)
  "Count columns and rows of STRING. Return a cons cell like (WIDTH . HEIGHT).

Example:
 (pos-tip-string-width-height \"abc\\nあいう\\n123\")
 ;; => (6 . 3)"
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (end-of-line)
    (let ((width (current-column))
	  (height 1))
      (while (< (point) (point-max))
	(end-of-line 2)
	(setq width (max (current-column) width)
	      height (1+ height)))
      (cons width height))))

(defun pos-tip-tooltip-width (width char-width)
  "Calculate tooltip pixel width."
  (+ (* width char-width)
     (ash (+ pos-tip-border-width
	     pos-tip-internal-border-width)
	  1)))

(defun pos-tip-tooltip-height (height char-height)
  "Calculate tooltip pixel height."
  (let ((spacing (default-value 'line-spacing)))
    (+ (* height (+ char-height
		    (cond
		     ((integerp spacing)
		      spacing)
		     ((floatp spacing)
		      (truncate (* (cdr pos-tip-default-char-width-height)
				   spacing)))
		     (t 0))))
       (ash (+ pos-tip-border-width
	       pos-tip-internal-border-width)
	    1))))

(make-face 'pos-tip-temp)

(defun pos-tip-show
  (string &optional tip-color pos window timeout max-width frame-coordinates dx)
  "Show STRING in a tooltip, which is a small X window, at POS in WINDOW
using frame's default font with TIP-COLOR.

Return pixel position of tooltip relative to top left corner of frame as
a cons cell like (X . Y).

TIP-COLOR is a face or a cons cell like (FOREGROUND-COLOR . BACKGROUND-COLOR)
used to specify *only* foreground-color and background-color of tooltip.
If omitted, use `pos-tip-foreground-color' and `pos-tip-background-color'
instead.

Omitting POS and WINDOW means use current position and selected window,
respectively.

Automatically hide the tooltip after TIMEOUT seconds. Omitting TIMEOUT means
use the default timeout of 5 seconds. Non-positive TIMEOUT means don't hide
tooltip automatically.

MAX-WIDTH specifies the maximum number of columns, if non-nil.

If FRAME-COORDINATES is omitted, automatically obtain the absolute
coordinates of the top left corner of frame which WINDOW is on. Here,
`top left corner of frame' represents the origin of `window-pixel-edges'
and its coordinates are essential for calculating the return value. If
non-nil, specifies the frame location as a cons cell like (LEFT . TOP).
This option makes the calculations slightly faster, but can be used only
when it's clear that frame is in the specified position. Users can get the
latest values of frame location for using in the next call by referring the
variable `scim-saved-frame-coordinates' just after calling this function.

DX specifies horizontal offset in pixel.

See also `pos-tip-show-no-propertize'."
  (let ((frame (window-frame (or window (selected-window))))
	(w-h (if max-width
		 (let ((rows (pos-tip-split-string string max-width)))
		   (setq string (mapconcat 'identity rows "\n"))
		   (cons (apply 'max (mapcar 'length rows))
			 (length rows)))
	       (pos-tip-string-width-height string))))
    (face-spec-reset-face 'pos-tip-temp)
    (set-face-font 'pos-tip-temp (frame-parameter frame 'font))
    (pos-tip-show-no-propertize
     (propertize string 'face 'pos-tip-temp)
     tip-color pos window timeout
     (pos-tip-tooltip-width (car w-h) (frame-char-width frame))
     (pos-tip-tooltip-height (cdr w-h) (frame-char-height frame)))))

(defalias 'pos-tip-hide 'x-hide-tip
  "Hide pos-tip's tooltip.")

(provide 'pos-tip)

;;;
;;; pos-tip.el ends here

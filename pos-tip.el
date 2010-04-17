;;; pos-tip.el -- Show tooltip at point -*- coding: utf-8 -*-

;; Copyright (C) 2010 S. Irie

;; Author: S. Irie
;; Maintainer: S. Irie
;; Keywords: Tooltip

(defconst pos-tip-version "0.3.3")

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

;; This program is tested on GNU Emacs 22, 23 under X window system and
;; Emacs 23 for MS-Windows.

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
;; To use the full features of this program on MS-Windows,
;; put the additional setting in .emacs file:
;;
;;   (pos-tip-w32-max-width-height)   ; Maximize frame temporarily
;;
;; or
;;
;;   (pos-tip-w32-max-width-height t) ; Keep frame maximized

;;
;; Examples:
;;
;; We can display a tooltip at the current position by the following:
;;
;;   (pos-tip-show "foo bar")
;;
;; If you'd like to specify the tooltip color, use an expression as:
;;
;;   (pos-tip-show "foo bar" '("white" . "red"))
;;
;; Here, "white" and "red" are the foreground color and background
;; color, respectively.


;;; History:
;; 2010-04-16  S. Irie
;;         * Changed `pos-tip-show' not to fill paragraph unless exceeding WIDTH
;;         * Version 0.3.3
;;
;; 2010-04-08  S. Irie
;;         * Bug fix
;;         * Version 0.3.2
;;
;; 2010-03-31  S. Irie
;;         * Bug fix
;;         * Version 0.3.1
;;
;; 2010-03-30  S. Irie
;;         * Added support for MS-Windows
;;         * Added option `pos-tip-use-relative-coordinates'
;;         * Bug fixes
;;         * Version 0.3.0
;;
;; 2010-03-23  S. Irie
;;         * Changed argument WORD-WRAP to JUSTIFY
;;         * Added optional argument SQUEEZE
;;         * Added function `pos-tip-fill-string'
;;         * Added option `pos-tip-tab-width' used to expand tab characters
;;         * Bug fixes
;;         * Version 0.2.0
;;
;; 2010-03-22  S. Irie
;;         * Added optional argument WORD-WRAP to `pos-tip-split-string'
;;         * Changed `pos-tip-show' to perform word wrap or kinsoku shori
;;         * Version 0.1.8
;;
;; 2010-03-20  S. Irie
;;         * Added optional argument DY
;;         * Bug fix
;;         * Modified docstrings
;;         * Version 0.1.7
;;
;; 2010-03-18  S. Irie
;;         * Added/modifed docstrings
;;         * Changed working buffer name to " *xwininfo*"
;;         * Version 0.1.6
;;
;; 2010-03-17  S. Irie
;;         * Fixed typos in docstrings
;;         * Version 0.1.5
;;
;; 2010-03-16  S. Irie
;;         * Added support for multi-display environment
;;         * Bug fix
;;         * Version 0.1.4
;;
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

(defvar pos-tip-tab-width nil
  "Tab width used for `pos-tip-split-string' and `pos-tip-fill-string'
to expand tab characters. nil means use default value of `tab-width'.")

(defvar pos-tip-use-relative-coordinates nil
  "Non-nil means tooltip location is calculated as a coordinates
relative to the top left corner of frame. In this case the tooltip
will always be displayed within the frame.

Note that this variable is automatically set to non-nil if absolute
coordinates can't be obtained by `pos-tip-compute-pixel-position'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pos-tip-window-system (&optional frame)
  "The name of the window system that FRAME is displaying through.
The value is a symbol---for instance, 'x' for X windows.
The value is nil if Emacs is using a text-only terminal.

FRAME defaults to the currently selected frame."
  (cond
   ((fboundp 'window-system)
    ;; Emacs 23
    (window-system frame))
   (frame
    ;; Emacs 22
    (if (fboundp 'with-selected-frame)
	(with-selected-frame frame
	  window-system)
      (let ((orig-frame (selected-frame)))
	(select-frame frame)
	(prog1
	    window-system
	  (select-frame orig-frame)))))
   (t
    window-system)))

(defvar pos-tip-saved-frame-coordinates '(0 . 0)
  "The latest result of `pos-tip-frame-top-left-coordinates'.")

(defvar pos-tip-frame-offset nil
  "The latest result of `pos-tip-calibrate-frame-offset'. This value
is used for non-X graphical environment.")

(defun pos-tip-frame-top-left-coordinates (&optional frame)
  "Return the pixel coordinates of FRAME as a cons cell (LEFT . TOP),
which are relative to top left corner of screen.

Return nil if failing to acquire the coordinates.

If FRAME is omitted, use selected-frame.

Users can also get the frame coordinates by referring the variable
`pos-tip-saved-frame-coordinates' just after calling this function."
  (let ((winsys (pos-tip-window-system)))
    (cond
     ((null winsys)
      (error "text-only frame: %S" frame))
     ((eq winsys 'x)
      (ignore-errors
	(with-current-buffer (get-buffer-create " *xwininfo*")
	  (let ((case-fold-search nil))
	    (buffer-disable-undo)
	    (erase-buffer)
	    (call-process shell-file-name nil t nil shell-command-switch
			  (concat "xwininfo -id "
				  (frame-parameter frame 'window-id)))
	    (goto-char (point-min))
	    (search-forward "\n  Absolute")
	    (setq pos-tip-saved-frame-coordinates
		  (cons (string-to-number (buffer-substring-no-properties
					   (search-forward "X: ")
					   (line-end-position)))
			(string-to-number (buffer-substring-no-properties
					   (search-forward "Y: ")
					   (line-end-position)))))))))
     (t
      (and (or pos-tip-frame-offset
	       (pos-tip-calibrate-frame-offset frame))
	   (setq pos-tip-saved-frame-coordinates
		 (cons (+ (eval (frame-parameter frame 'left))
			  (car pos-tip-frame-offset))
		       (+ (eval (frame-parameter frame 'top))
			  (cdr pos-tip-frame-offset)))))))))

(defvar pos-tip-upperside-p nil
  "Non-nil indicates the latest result of `pos-tip-compute-pixel-position'
was upper than the location specified by the arguments.")

(defvar pos-tip-w32-saved-max-width-height nil
  "Display pixel size effective for showing tooltip in MS-Windows desktop.
This doesn't include the taskbar area, so isn't same as actual display size.")

(defun pos-tip-compute-pixel-position
  (&optional pos window pixel-width pixel-height frame-coordinates dx dy)
  "Return pixel position of POS in WINDOW like (X . Y), which indicates
the absolute or relative coordinates of bottom left corner of the object.

Omitting POS and WINDOW means use current position and selected window,
respectively.

If PIXEL-WIDTH and PIXEL-HEIGHT are given, this function assumes these
values as the size of small window like tooltip which is located around the
object at POS. These values are used to adjust the location in order that
the tooltip won't disappear by sticking out of the display. By referring
the variable `pos-tip-upperside-p' after calling this function, user can
examine whether the tooltip will be located above the specified position.

If FRAME-COORDINATES is omitted or nil, automatically obtain the absolute
coordinates of the top left corner of frame which WINDOW is on. Here,
`top left corner of frame' represents the origin of `window-pixel-edges'
and its coordinates are essential for calculating the return value as
absolute coordinates. If a cons cell like (LEFT . TOP), specifies the
frame absolute location and makes the calculation slightly faster, but can
be used only when it's clear that frame is in the specified position. Users
can get the latest values of frame coordinates for using in the next call
by referring the variable `pos-tip-saved-frame-coordinates' just after
calling this function. Otherwise, FRAME-COORDINATES `relative' means return
pixel coordinates of the object relative to the top left corner of the frame.
This is the same effect as `pos-tip-use-relative-coordinates' is non-nil.

DX specifies horizontal offset in pixel.

DY specifies vertical offset in pixel. This makes the calculations done
without considering the height of object at POS, so the object might be
hidden by the tooltip."
  (let* ((frame (window-frame (or window (selected-window))))
	 (w32-frame (eq (pos-tip-window-system frame) 'w32))
	 (relative (or pos-tip-use-relative-coordinates
		       (eq frame-coordinates 'relative)
		       (and w32-frame
			    (null pos-tip-w32-saved-max-width-height))))
	 (frame-coord (or (and relative '(0 . 0))
			  frame-coordinates
			  (pos-tip-frame-top-left-coordinates frame)
			  (progn
			    (setq relative t
				  pos-tip-use-relative-coordinates t)
			  '(0 . 0))))
	 (x-y (or (pos-visible-in-window-p (or pos (window-point window)) window t)
		  '(0 0)))
	 (x (+ (car frame-coord)
	       (car (window-inside-pixel-edges window))
	       (car x-y)
	       (or dx 0)))
	 (y0 (+ (cdr frame-coord)
		(cadr (window-pixel-edges window))
		(cadr x-y)))
	 (y (+ y0
	       (or dy
		   ;; `posn-object-width-height' returns an incorrect value
		   ;; when the header line is displayed (Emacs bug #4426).
		   ;; In this case, `frame-char-height' is used substitutively,
		   ;; but this function doesn't return actual object height.
		   (and header-line-format
			(frame-char-height frame))
		   (cdr (posn-object-width-height
			 (posn-at-x-y (max (car x-y) 0) (cadr x-y)))))))
	 xmax ymax)
    (cond
     (relative
      (setq xmax (frame-pixel-width frame)
	    ymax (frame-pixel-height frame)))
     (w32-frame
      (setq xmax (car pos-tip-w32-saved-max-width-height)
	    ymax (cdr pos-tip-w32-saved-max-width-height)))
     (t
      (setq xmax (x-display-pixel-width)
	    ymax (x-display-pixel-height))))
    (setq pos-tip-upperside-p (> (+ y (or pixel-height 0))
				 ymax))
    (cons (max 0 (min x (- xmax (or pixel-width 0))))
	  (max 0 (if pos-tip-upperside-p
		     (- (if dy ymax y0) (or pixel-height 0))
		   y)))))

(defun pos-tip-cancel-timer ()
  "Cancel timeout of tooltip."
  (mapc (lambda (timer)
	  (if (eq (aref timer 5) 'x-hide-tip)
	      (cancel-timer timer)))
	timer-list))

(defun pos-tip-avoid-mouse (left right top bottom &optional frame)
  "Move out mouse pointer if it is inside region (LEFT RIGHT TOP BOTTOM)
in FRAME. Return new mouse position like (FRAME . (X . Y))."
  (unless frame
    (setq frame (selected-frame)))
  (let* ((mpos (mouse-pixel-position))
	 (mframe (pop mpos))
	 (mx (car mpos))
	 (my (cdr mpos)))
    (when (and (eq mframe frame)
	       (numberp mx))
      (let* ((large-number (+ (frame-pixel-width frame) (frame-pixel-height frame)))
	     (dl (if (> left 2)
		     (1+ (- mx left))
		   large-number))
	     (dr (if (< (1+ right) (frame-pixel-width frame))
		     (- right mx)
		   large-number))
	     (dt (if (> top 2)
		     (1+ (- my top))
		   large-number))
	     (db (if (< (1+ bottom) (frame-pixel-height frame))
		     (- bottom my)
		   large-number))
	     (d (min dl dr dt db)))
	(when (> d -2)
	  (cond
	   ((= d dl)
	    (setq mx (- left 2)))
	   ((= d dr)
	    (setq mx (1+ right)))
	   ((= d dt)
	    (setq my (- top 2)))
	   (t
	    (setq my (1+ bottom))))
	  (set-mouse-pixel-position frame mx my))))
    (cons mframe (and mpos (cons mx my)))))

(defun pos-tip-show-no-propertize
  (string &optional tip-color pos window timeout pixel-width pixel-height frame-coordinates dx dy)
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

\(defface my-tooltip
  '((t
     :background \"gray85\"
     :foreground \"black\"
     :inherit variable-pitch))
  \"Face for my tooltip.\")

\(defface my-tooltip-highlight
  '((t
     :background \"blue\"
     :foreground \"white\"
     :inherit my-tooltip))
  \"Face for my tooltip highlighted.\")

\(let ((str (propertize \" foo \\n bar \\n baz \" 'face 'my-tooltip)))
  (put-text-property 6 11 'face 'my-tooltip-highlight str)
  (pos-tip-show-no-propertize str 'my-tooltip))"
  (let* ((frame (window-frame (or window (selected-window))))
	 (winsys (pos-tip-window-system frame))
	 (x-frame (eq winsys 'x))
	 (w32-frame (eq winsys 'w32))
	 (relative (or pos-tip-use-relative-coordinates
		       (eq frame-coordinates 'relative)
		       (and w32-frame
			    (null pos-tip-w32-saved-max-width-height))))
	 (x-y (prog1
		  (pos-tip-compute-pixel-position pos window
						  pixel-width pixel-height
						  frame-coordinates dx dy)
		(if pos-tip-use-relative-coordinates
		    (setq relative t))))
	 (ax (car x-y))
	 (ay (cdr x-y))
	 (rx (if relative ax (- ax (car pos-tip-saved-frame-coordinates))))
	 (ry (if relative ay (- ay (cdr pos-tip-saved-frame-coordinates))))
	 (fg (or (and (facep tip-color)
		      (face-attribute tip-color :foreground))
		 (car-safe tip-color)
		 pos-tip-foreground-color))
	 (bg (or (and (facep tip-color)
		      (face-attribute tip-color :background))
		 (cdr-safe tip-color)
		 pos-tip-background-color))
	 (use-dxdy (or relative
		       (not x-frame)))
	 (spacing (frame-parameter frame 'line-spacing))
	 (border (ash (+ pos-tip-border-width
			 pos-tip-internal-border-width)
		      1))
	 (x-max-tooltip-size
	  (cons (+ (if x-frame 1 0)
		   (/ (- (or pixel-width
			     (cond
			      (relative
			       (frame-pixel-width frame))
			      (w32-frame
			       (car pos-tip-w32-saved-max-width-height))
			      (t
			       (x-display-pixel-width))))
			 border)
		      (frame-char-width frame)))
		(/ (- (or pixel-height
			  (x-display-pixel-height))
		      border)
		   (frame-char-height frame))))
	 default-frame-alist
	 mpos)
    (unless (or (not use-dxdy)
		(and (setq mpos (mouse-pixel-position))
		     (eq frame (car mpos)) (cadr mpos) (cddr mpos)))
      (let* ((edges (window-inside-pixel-edges (cadr (window-list))))
	     (mx (ash (+ (pop edges) (cadr edges)) -1))
	     (my (ash (+ (pop edges) (cadr edges)) -1)))
	(set-mouse-pixel-position frame mx my)))
    (and pixel-width pixel-height
	 (setq mpos (pos-tip-avoid-mouse rx (+ rx pixel-width
					       (if w32-frame 3 0))
					 ry (+ ry pixel-height)
					 frame)))
    (x-show-tip string frame
		`((border-width . ,pos-tip-border-width)
		  (internal-border-width . ,pos-tip-internal-border-width)
		  ,@(and (not use-dxdy) `((left . ,ax)
					  (top . ,ay)))
		  (font . ,(frame-parameter frame 'font))
		  ,@(and spacing `((line-spacing . ,spacing)))
		  ,@(and (stringp fg) `((foreground-color . ,fg)))
		  ,@(and (stringp bg) `((background-color . ,bg))))
		(and timeout (> timeout 0) timeout)
		(and use-dxdy (- rx (cadr mpos)))
		(and use-dxdy (- ry (cddr mpos))))
    (if (and timeout (<= timeout 0))
	(pos-tip-cancel-timer))
    (cons rx ry)))

(defun pos-tip-split-string (string &optional width margin justify squeeze)
  "Split STRING into fixed width strings. Return a list of these strings.

WIDTH specifies the width of filling each paragraph. WIDTH nil means use
the width of currently selected frame. Note that this function doesn't add any
padding characters at the end of each row.

MARGIN, if non-nil, specifies left margin width which is the number of spece
characters to add at the beginning of each row.

The optional fourth argument JUSTIFY specifies which kind of justification
to do: `full', `left', `right', `center', or `none'. A value of t means handle
each paragraph as specified by its text properties. Omitting JUSTIFY means
don't perform justification, word wrap and kinsoku shori (禁則処理).

SQUEEZE nil means leave whitespaces other than line breaks untouched."
  (with-temp-buffer
    (let* ((tab-width (or pos-tip-tab-width tab-width))
	   (fill-column (or width (frame-width)))
	   (left-margin (or margin 0))
	   (kinsoku-limit 1)
	   indent-tabs-mode
	   row rows)
      (insert string)
      (untabify (point-min) (point-max))
      (if justify
	  (fill-region (point-min) (point-max) justify (not squeeze))
	(setq margin (make-string left-margin ?\s)))
      (goto-char (point-min))
      (while (prog2
		 (let ((line (buffer-substring
			      (point) (progn (end-of-line) (point)))))
		   (if justify
		       (push line rows)
		     (while (progn
			      (setq line (concat margin line)
				    row (truncate-string-to-width line fill-column))
			      (push row rows)
			      (if (not (= (length row) (length line)))
				  (setq line (substring line (length row))))))))
		 (< (point) (point-max))
	       (beginning-of-line 2)))
      (nreverse rows))))

(defun pos-tip-fill-string (string &optional width margin justify squeeze)
  "Fill each of the paragraphs in STRING.

WIDTH specifies the width of filling each paragraph. WIDTH nil means use
the width of currently selected frame. Note that this function doesn't add any
padding characters at the end of each row.

MARGIN, if non-nil, specifies left margin width which is the number of spece
characters to add at the beginning of each row.

The optional fourth argument JUSTIFY specifies which kind of justification
to do: `full', `left', `right', `center', or `none'. A value of t means handle
each paragraph as specified by its text properties. Omitting JUSTIFY means
don't perform justification, word wrap and kinsoku shori (禁則処理).

SQUEEZE nil means leave whitespaces other than line breaks untouched."
  (if justify
      (with-temp-buffer
	(let* ((tab-width (or pos-tip-tab-width tab-width))
	       (fill-column (or width (frame-width)))
	       (left-margin (or margin 0))
	       (kinsoku-limit 1)
	       indent-tabs-mode)
	  (insert string)
	  (untabify (point-min) (point-max))
	  (fill-region (point-min) (point-max) justify (not squeeze))
	  (buffer-string)))
    (mapconcat 'identity
	       (pos-tip-split-string string width margin)
	       "\n")))

(defun pos-tip-string-width-height (string)
  "Count columns and rows of STRING. Return a cons cell like (WIDTH . HEIGHT).

Example:

\(pos-tip-string-width-height \"abc\\nあいう\\n123\")
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

(defun pos-tip-tooltip-height (height char-height &optional frame)
  "Calculate tooltip pixel height."
  (let ((spacing (or (default-value 'line-spacing)
		     (frame-parameter frame 'line-spacing))))
    (+ (* height (+ char-height
		    (cond
		     ((integerp spacing)
		      spacing)
		     ((floatp spacing)
		      (truncate (* (frame-char-height frame)
				   spacing)))
		     (t 0))))
       (ash (+ pos-tip-border-width
	       pos-tip-internal-border-width)
	    1))))

(make-face 'pos-tip-temp)

(defun pos-tip-show
  (string &optional tip-color pos window timeout width frame-coordinates dx dy)
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

WIDTH, if non-nil, specifies the width of filling each paragraph.

If FRAME-COORDINATES is omitted or nil, automatically obtain the absolute
coordinates of the top left corner of frame which WINDOW is on. Here,
`top left corner of frame' represents the origin of `window-pixel-edges'
and its coordinates are essential for calculating the absolute coordinates
of the tooltip. If a cons cell like (LEFT . TOP), specifies the frame
absolute location and makes the calculation slightly faster, but can be
used only when it's clear that frame is in the specified position. Users
can get the latest values of frame coordinates for using in the next call
by referring the variable `pos-tip-saved-frame-coordinates' just after
calling this function. Otherwise, FRAME-COORDINATES `relative' means use
the pixel coordinates relative to the top left corner of the frame for
displaying the tooltip. This is the same effect as
`pos-tip-use-relative-coordinates' is non-nil.

DX specifies horizontal offset in pixel.

DY specifies vertical offset in pixel. This makes the calculations done
without considering the height of object at POS, so the object might be
hidden by the tooltip.

See also `pos-tip-show-no-propertize'."
  (let ((frame (window-frame (or window (selected-window))))
	(w-h (pos-tip-string-width-height string)))
    (if (and width
	     (> (car w-h) width))
	(setq string (pos-tip-fill-string string width nil 'none)
	      w-h (pos-tip-string-width-height string)))
    (face-spec-reset-face 'pos-tip-temp)
    (set-face-font 'pos-tip-temp (frame-parameter frame 'font))
    (pos-tip-show-no-propertize
     (propertize string 'face 'pos-tip-temp)
     tip-color pos window timeout
     (pos-tip-tooltip-width (car w-h) (frame-char-width frame))
     (pos-tip-tooltip-height (cdr w-h) (frame-char-height frame) frame)
     frame-coordinates dx dy)))

(defalias 'pos-tip-hide 'x-hide-tip
  "Hide pos-tip's tooltip.")

(defun pos-tip-calibrate-frame-offset (&optional frame)
  "Return coordinates of FRAME orign relative to the top left corner of
the FRAME extent, like (LEFT . TOP). The return value is recorded to
`pos-tip-frame-offset'.

Note that this function does't correctly work for X frame and Emacs 22."
  (setq pos-tip-frame-offset nil)
  (let* ((window (frame-first-window frame))
	 (delete-frame-functions
	  '((lambda (frame)
	      (if (equal (frame-parameter frame 'name) "tooltip")
		  (setq pos-tip-frame-offset
			(cons (eval (frame-parameter frame 'left))
			      (eval (frame-parameter frame 'top))))))))
	 (pos-tip-border-width 0)
	 (pos-tip-internal-border-width 1)
	 (rpos (pos-tip-show ""
			     '(nil . (frame-parameter frame 'background-color))
			     (window-start window) window
			     nil nil 'relative nil 0)))
    (sit-for 0)
    (pos-tip-hide)
    (and pos-tip-frame-offset
	 (setq pos-tip-frame-offset
	       (cons (- (car pos-tip-frame-offset)
			(car rpos)
			(eval (frame-parameter frame 'left)))
		     (- (cdr pos-tip-frame-offset)
			(cdr rpos)
			(eval (frame-parameter frame 'top))))))))

(defun pos-tip-w32-max-width-height (&optional keep-maximize)
  "Maximize the currently selected frame temporarily and set
`pos-tip-w32-saved-max-width-height' the effective display size in order
to become possible to calculate the absolute location of tooltip.

KEEP-MAXIMIZE non-nil means leave the frame maximized.

Note that this function is usable only in Emacs 23 for MS-Windows."
  (interactive)
  ;; Maximize frame
  (w32-send-sys-command 61488)
  (sit-for 0)
  (let ((offset (pos-tip-calibrate-frame-offset)))
    (prog1
	(setq pos-tip-w32-saved-max-width-height
	      (cons (frame-pixel-width)
		    (+ (frame-pixel-height)
		       (- (cdr offset) (car offset)))))
      (if (interactive-p)
	  (message "%S" pos-tip-w32-saved-max-width-height))
      (unless keep-maximize
	;; Restore frame
	(w32-send-sys-command 61728)))))


(provide 'pos-tip)

;;;
;;; pos-tip.el ends here

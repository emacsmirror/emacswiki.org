;;; mouse-drag-fix.el --- Bug fixed version of mouse-drag

;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.

;; Author: Kahlil Hodgson <dorge@tpg.com.au>
;; X-URL: http://www.emacswiki.org/elisp/mouse-drag-fix.el

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

;; Fixes to bugs/anomalies in the `mouse-drag-drag' function (see
;; commentary to mouse-drag.el) that occur when mouse leaves the
;; window or frame during a drag.
;;
;; Is not compatible with XEmacs!!!

;;; Installation:

;; To get mouse-drag to work with flyspell add the following to your
;; .emacs
;;
;; (when (symbolp 'mouse-drag-drag)
;;   ;; so mouse-yank-at-click works
;;   (define-key flyspell-mode-map [(mouse-2)] nil)
;;
;;   ;; so as to not interfere with mouse-drag
;;   (define-key flyspell-mouse-map [(mouse-2)] nil)
;;   (define-key flyspell-mouse-map [(down-mouse-2)] 'flyspell-correct-word)
;;   )

;;;; To Do

;; (1) should really fix drag throw  as well
;; (2) XEmacs compatibility

(require 'mouse-drag)

(defun mouse-drag-drag (start-event)
  "\"Drag\" the page according to a mouse drag.

Drag scrolling moves the page according to the movement of the mouse.
You \"grab\" the character under the mouse and move it around.

If the mouse is clicked and released in the same place of time we
assume that the user didn't want to scroll but wanted to whatever
mouse-2 used to do, so we pass it through.

Drag scrolling is identical to the \"hand\" option in MacPaint, or the
middle button in Tk text widgets.

To test this function, evaluate:
    (global-set-key [down-mouse-2] 'mouse-drag-drag)"

  (interactive "e")
  ;; we want to do save-selected-window, but that requires 19.29
  (let* ((start-posn (event-start start-event))
	 (start-window (posn-window start-posn))

	 ;; since we may not want to select he start-window ...
	 (old-selected-window (selected-window))

	 ;; be conservative about allowing horizontal scrolling
	 (col-scrolling-p (mouse-drag-should-do-col-scrolling))

	 ;; set this relative to top left corner of frame
	 ;; (this is faster than using `mouse-position')
	 (start-row (+ (cdr (posn-col-row start-posn))
		     (car (cdr (window-edges start-window)))))

	 (start-col (if col-scrolling-p
			(+ (car (posn-col-row start-posn))
			   (car (window-edges start-window)))))

	 event
	 (end nil) ;; initialize
	 row col
	 (scroll-delta 0)
	 (scroll-col-delta 0)
	 (have-scrolled  nil)
	 (point-event-p  nil)
	 old-binding)

    (select-window start-window)

    (setq end nil);; initialize
    (track-mouse
      (while (progn
	       (setq event (read-event)
		     end (event-end event))
	       (mouse-movement-p event))

	;; need to be sure event is a mouse movement
	(if (windowp (posn-window end))
	    (setq row (+ (cdr (posn-col-row end))
			 (car (cdr (window-edges (posn-window end)))))
		  col (+ (car (posn-col-row end))
			 (car (window-edges (posn-window end)))))
	  (setq row (cdr (posn-col-row end))
		col (car (posn-col-row end))))

	(if (not (eq start-window (posn-window end)))
	    (setq point-event-p nil)) ;; we've moved out of window so don't

	(setq scroll-delta (- row start-row)
	      start-row row)
	(if col-scrolling-p
	    (setq scroll-col-delta (- col start-col)
		  start-col col))
	(when (or (/= 0 scroll-delta)
		  (/= 0 scroll-col-delta))
	  (setq have-scrolled t)
	  (mouse-drag-safe-scroll scroll-delta scroll-col-delta))))
    ;; If it was a click and not a drag, prepare to pass the event on.
    ;; Note:  We must determine the pass-through event before restoring
    ;; the window, but invoke it after.  Sigh.
    (if (and (not have-scrolled) end
	     (mouse-drag-events-are-point-events-p start-posn end))
	(setq point-event-p t
	      old-binding (key-binding
			   (vector (event-basic-type start-event)))))
    ;; Now restore the old window.
    (select-window old-selected-window)
    ;; For clicks, call the old function.
    (if point-event-p
	(call-interactively old-binding))))

(provide 'mouse-drag-fix)

;;; mouse-drag-fix.el ends here

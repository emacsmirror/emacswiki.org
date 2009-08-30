;;; track-scroll.el --- Enable direct scrolling with the mouse
;; $Id: $
;; Copyright (C) 2002 by Stefan Kamphausen
;; Author:
;;           Stefan Kamphausen <mail@skamphausen.de>
;; Keywords: services, user

;; This file is not part of XEmacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.


;;; Commentary:
;; GNU Emacs comes with mouse-drag.el which implements a feature I've
;; been wanting for years but doesn't work with XEmacs. *grr*
;; Loading this file enables the dragging of the window using the
;; middle mouse button. Holding the control key at the same time calls
;; a different function which behaves more like the scrollbar does.
;; It's probably best to just try it yourself and see what happens.
;; This abandons drag and drop functionality which is usually called
;; when dragging the middle mouse button

;; Problems:
;; - horizontal scrolling
;; - customizable sensitivities for both directions?
;; - customizable keys (mouse button, modifier)

;;; Code:
(require 'mouse)
;; The control modifier is currently hard coded :-(
(global-set-key '[(control button2)] 'mouse-track)

;; Remove default handler and install our own version which handles
;; mouse button2 different
(remove-hook 'mouse-track-drag-hook
			 'default-mouse-track-drag-hook)
(add-hook 'mouse-track-drag-hook
			 'track-scroll-mouse-track-drag-hook)

;; The replacement for default-mouse-track-drag-hook
(defun track-scroll-mouse-track-drag-hook (event click-count was-timeout)
  "A mouse dragging handler that enables scrolling the window.
This is just a slightly modified copy of
`default-mouse-track-drag-hook'. which calls a drag and drop routine
when dragging with button2."
  (cond ((default-mouse-track-event-is-with-button event 1)
	 (default-mouse-track-deal-with-down-event click-count)
	 (default-mouse-track-set-point event default-mouse-track-window)
	 (default-mouse-track-cleanup-extent)
	 (default-mouse-track-next-move default-mouse-track-min-anchor
	   default-mouse-track-max-anchor
	   default-mouse-track-extent)
	 t)
	((default-mouse-track-event-is-with-button event 2)
	 (track-scroll event))))


;; This is needed to recognize whether the dragging has just started
;; (otherwise I missed something)
(add-hook 'mouse-track-up-hook
		  'track-scroll-finalize)
(defun track-scroll-finalize (event click-count)
  (setq track-scroll-new t)
  (setq track-scroll-initpos nil)
  (setq track-scroll-start-col nil)
  (setq track-scroll-event-window nil))

(defvar track-scroll-new t)
(defvar track-scroll-start-point nil)
(defvar track-scroll-start-col nil)
(defvar track-scroll-event-window nil)

(defun track-scroll (event)
  "Calls one of the different drag handlers.
Depending on the used modifier."
  (if (memq 'control (event-modifiers event))
	  (track-scroll-fullwin event)
	(track-scroll-drag event)))

(defun track-scroll-fullwin (event)
  "Scroll the window taking it's height as 100%.
This has the sideffect that when you start dragging the mouse
the window is scrolled to the approriate percentage of that starting
position before the fluent scrolling starts. This can be confusing but
is nice for quick movements."
  (if track-scroll-new
	  (progn
		(setq track-scroll-new nil)
		(setq track-scroll-event-window (event-window event))
		(select-window track-scroll-event-window)
		(setq track-scroll-start-col (current-column))))
  (let* ((pos (event-window-y-pixel event))
		 (perc (* 100 (/ (float pos)
						 (window-pixel-height))))
		 (newpoint (floor (+ (point-min)
							 (/ (* perc (- (point-max) (point-min)))
								100))))
		 (window (event-window event)))
	(goto-char newpoint (window-buffer window))
	(beginning-of-line)
	(move-to-column (if (> (point-at-eol
							track-scroll-start-col))
						track-scroll-start-col
						0))
	(recenter (/ (window-height) 2) window)))


(defun track-scroll-drag (event)
  "Drag the window's contents.
This means actually grabbing a character and moving it around a
little. That also means that you can only scroll a part according to
your window size.

It behaves very similar to GNU Emacs `mouse-drag-drag'."
  (if track-scroll-new
	  (progn
		(setq track-scroll-new nil)
		(setq track-scroll-event-window (event-window event))
		(select-window track-scroll-event-window)
		(setq track-scroll-start-point (event-closest-point event))))
  (if (eq (event-window event) track-scroll-event-window)
	  (let* ((currpoint (event-closest-point event)))
		(if currpoint
			(let* ((oldline (line-number track-scroll-start-point))
				   (newline (line-number currpoint))
				   (linediff (- newline oldline))
				   (window (event-window event)))
			  (condition-case nil
				  (progn
					(scroll-down linediff))
				(error nil)))))))

;; This version _tries_ to do horizontal scrolling, but it's not
;; really working well
;; If you intent to fix this you'll probably know how to enable it as
;; well... 
(defun track-scroll-drag-h (event)
  "Same as `track-scroll-drag' but tries to do some horizontal
  scrolling which isn't really working well."
  (if track-scroll-new
	  (progn
		(setq track-scroll-new nil)
		(setq track-scroll-event-window (event-window event))
		(select-window track-scroll-event-window)
		(setq track-scroll-start-point (event-closest-point event))
		(setq track-scroll-start-col
			  (save-excursion
				(goto-char track-scroll-start-point)
				(current-column)))
		))
  (if (eq (event-window event) track-scroll-event-window)
	  (let* ((currpoint (event-closest-point event)))
		(if currpoint
			(let* ((oldline (line-number track-scroll-start-point))
				   (newline (line-number currpoint))
				   (linediff (- newline oldline))
				   (oldcol track-scroll-start-col)
				   (newcol (save-excursion
							 (goto-char currpoint)
							 (current-column)))
				   (coldiff (- newcol oldcol))
				   (window (event-window event)))
			  (condition-case nil
				  (progn
					(scroll-down linediff)
					;; FIXME: sensitivity customizable
					(cond
					 ((< coldiff -1)
					  (scroll-right coldiff))
					 ((> coldiff 1)
					  (scroll-left (- 0 coldiff)))
					 t nil
					 ))
				(error nil)))))))


(provide 'track-scroll)
;;; track-scroll.el ends here

;;; acme-search.el --- right-click searching

;; Author: Dan McCarthy <daniel.c.mccarthy@gmail.com>
;; Copyright (C) 2006, Dan McCarthy, see section 'Terms'.

;; Description:

;; In the Acme editor, right-clicking on a word finds the next occurrence
;; of that word, and moves the mouse to the new occurrence. In this way,
;; you can search through all occurrences of a word by repeatedly
;; right-clicking. This file gives Emacs the same power.
;;
;; To use, add (require 'acme-search) to your .emacs. I bind
;; `acme-search-forward' to mouse-3, and `acme-search-backward' to
;; shift-mouse-3, like so:

;; (global-set-key [(mouse-3)] 'acme-search-forward)
;; (global-set-key [(shift mouse-3)] 'acme-search-backward)

;; Terms:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

(eval-when-compile (require 'cl)) ;;when, unless, case

(defun header-line-active-p ()
  (not (null header-line-format)))

(defun move-mouse-to-point ()
  "Move the mouse pointer to point in the current window."
  (let* ((coords (posn-col-row (posn-at-point)))
	 (window-coords (window-inside-edges))
	 (x (+ (car coords) (car window-coords) -1)) ;the fringe is 0
	 (y (+ (cdr coords) (cadr window-coords)
	       (if (header-line-active-p)
		   -1
		 0))))
    (set-mouse-position (selected-frame) x y)))

(defun blink-region (start end)
  "Make the text between START and END blink."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'region)
    (run-at-time 0.1 nil 'delete-overlay overlay)))

(defun acme-search (posn direction)
  "Search forward for the symbol under mouse, moving mouse and point forward.
This is inspired by Rob Pike's Acme."
  (mouse-set-point posn)
  (let ((sym (thing-at-point 'symbol))
	(search-function (case direction
			   ((forward) 'word-search-forward)
			   ((backward) 'word-search-backward)
			   (t (error "Direction must be forward or backward"))))
	(restart-point (case direction
			 ((forward) (point-min))
			 ((backward) (point-max)))))
    (when (eq direction 'backward)
      ;;We have to move point backwards to before the symbol under the mouse, or
      ;;else search-backward will only find this instance of the symbol.
      (backward-char (length sym)))
    (unless (funcall search-function sym nil t)
      (message "Search failed; restarting from %s..."
	       (if (eq direction 'forward)
		   "top"
		 "bottom"))
      (goto-char restart-point)
      (funcall search-function sym nil t))
    (when (eq direction 'backward)
      ;;search-backward leaves point at the beginning of the symbol
      (forward-char (length sym))))
  ;;Redisplay the screen if we search off the bottom of the window.
  (unless (posn-at-point)
    (universal-argument)
    (recenter))
  (move-mouse-to-point)
  (blink-region (match-beginning 0) (match-end 0)))

(defun acme-search-forward (posn)
  (interactive "e")
  (acme-search posn 'forward))

(defun acme-search-backward (posn)
  (interactive "e")
  (acme-search posn 'backward))

(provide 'acme-search)

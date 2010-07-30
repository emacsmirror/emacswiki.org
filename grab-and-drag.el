;;; grab-and-drag.el --- Scroll a window by mouse dragging

;; Copyright (C) 2010  S. Irie

;; Author: S. Irie
;; Maintainer: S. Irie
;; Keywords: mouse, scroll

(defconst grab-and-drag-version "0.2.0")

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a command and a minor mode which allow users
;; to scroll the window in the arbitrary direction by mouse dragging,
;; like "Grab and Drag" firefox extension. It's probably useful
;; for a touch panel based computer.
;;
;; By default settings, you can do "Grab and Drag" using a command
;; `grab-and-drag' by mouse dragging with the left button. That means
;; grab-and-drag-mode overrides the binding of `down-mouse-1' event,
;; which is usually bound to `mouse-drag-region' command. Even Though
;; grab-and-drag-mode overrides the original binding, you can still use
;; it by holding down the button for 1 second without moving the pointer.
;;
;; If you release the mouse button while moving the pointer, the
;; additional action is performed according to the dragging time and
;; direction. The mouse dragging longer than 500 milliseconds causes
;; an inertial scrolling vertically. Otherwise the quick dragging is
;; recognized as a flick gesture, which causes a scrolling by screenful
;; in that direction.
;;
;; The code is rather X-dependent and is tested on GNU Emacs 22 and 23.

;;
;; Installation:
;;
;; First, save this file as grab-and-drag.el and byte-compile in
;; a directory that is listed in load-path.
;;
;; Put the following in your .emacs file:
;;
;;   (require 'grab-and-drag)
;;   (grab-and-drag-mode 1)
;;
;; Then, start Emacs and grab-and-drag-mode is activated.

;; History:
;; 2010-07-29  S. Irie
;;         * Version 0.2.0
;;         * Add Horizontal scrolling
;;         * Add Inertial scrolling
;;         * Add animated scrolling by screenfuls:
;;         * Add timeout of dragging
;;         * Add flick gesture recognition
;;         * Add hook variables:
;;            `grab-and-drag-beginning-of-buffer-hook'
;;            `grab-and-drag-end-of-buffer-hook'
;;         * Fix a lot of bugs
;; 2010-07-21  S. Irie
;;         * Version 0.1.0
;;         * Initial version

;; ToDo:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup grab-and-drag nil
  "Scroll a window by mouse dragging."
  :prefix "grab-and-drag-"
  :group 'convenience :group 'mouse :group 'windows)

(defcustom grab-and-drag-button 1
  "Button number that `grab-and-drag-mode' uses for mouse dragging."
  :group 'grab-and-drag
  :type 'integer)

(defcustom grab-and-drag-mode-pointer-shape
  (and (boundp 'x-pointer-hand1) x-pointer-hand1)
  "Mouse pointer shape for `grab-and-drag-mode'.
The value nil means don't set the pointer shape."
  :group 'grab-and-drag
  :type '(choice (const :tag "Don't change pointer" :value nil)
		 (choice :tag "Pointer shape" :value 58
			 (const :tag "X-cursor" :value 0)
			 (const :tag "arrow" :value 2)
			 (const :tag "based-arrow-down" :value 4)
			 (const :tag "based-arrow-up" :value 6)
			 (const :tag "boat" :value 8)
			 (const :tag "bogosity" :value 10)
			 (const :tag "bottom-left-corner" :value 12)
			 (const :tag "bottom-right-corner" :value 14)
			 (const :tag "bottom-side" :value 16)
			 (const :tag "bottom-tee" :value 18)
			 (const :tag "box-spiral" :value 20)
			 (const :tag "center-ptr" :value 22)
			 (const :tag "circle" :value 24)
			 (const :tag "clock" :value 26)
			 (const :tag "coffee-mug" :value 28)
			 (const :tag "cross" :value 30)
			 (const :tag "cross-reverse" :value 32)
			 (const :tag "crosshair" :value 34)
			 (const :tag "diamond-cross" :value 36)
			 (const :tag "dot" :value 38)
			 (const :tag "dotbox" :value 40)
			 (const :tag "double-arrow" :value 42)
			 (const :tag "draft-large" :value 44)
			 (const :tag "draft-small" :value 46)
			 (const :tag "draped-box" :value 48)
			 (const :tag "exchange" :value 50)
			 (const :tag "fleur" :value 52)
			 (const :tag "gobbler" :value 54)
			 (const :tag "gumby" :value 56)
			 (const :tag "hand1" :value 58)
			 (const :tag "hand2" :value 60)
			 (const :tag "heart" :value 62)
			 (const :tag "icon" :value 64)
			 (const :tag "iron-cross" :value 66)
			 (const :tag "left-ptr" :value 68)
			 (const :tag "left-side" :value 70)
			 (const :tag "left-tee" :value 72)
			 (const :tag "leftbutton" :value 74)
			 (const :tag "ll-angle" :value 76)
			 (const :tag "lr-angle" :value 78)
			 (const :tag "man" :value 80)
			 (const :tag "middlebutton" :value 82)
			 (const :tag "mouse" :value 84)
			 (const :tag "pencil" :value 86)
			 (const :tag "pirate" :value 88)
			 (const :tag "plus" :value 90)
			 (const :tag "question-arrow" :value 92)
			 (const :tag "right-ptr" :value 94)
			 (const :tag "right-side" :value 96)
			 (const :tag "right-tee" :value 98)
			 (const :tag "rightbutton" :value 100)
			 (const :tag "rtl-logo" :value 102)
			 (const :tag "sailboat" :value 104)
			 (const :tag "sb-down-arrow" :value 106)
			 (const :tag "sb-h-double-arrow" :value 108)
			 (const :tag "sb-left-arrow" :value 110)
			 (const :tag "sb-right-arrow" :value 112)
			 (const :tag "sb-up-arrow" :value 114)
			 (const :tag "sb-v-double-arrow" :value 116)
			 (const :tag "shuttle" :value 118)
			 (const :tag "sizing" :value 120)
			 (const :tag "spider" :value 122)
			 (const :tag "spraycan" :value 124)
			 (const :tag "star" :value 126)
			 (const :tag "target" :value 128)
			 (const :tag "tcross" :value 130)
			 (const :tag "top-left-arrow" :value 132)
			 (const :tag "top-left-corner" :value 134)
			 (const :tag "top-right-corner" :value 136)
			 (const :tag "top-side" :value 138)
			 (const :tag "top-tee" :value 140)
			 (const :tag "trek" :value 142)
			 (const :tag "ul-angle" :value 144)
			 (const :tag "umbrella" :value 146)
			 (const :tag "ur-angle" :value 148)
			 (const :tag "watch" :value 150)
			 (const :tag "xterm" :value 152)
			 (const :tag "invisible" :value 255))
		 (integer :tag "Pointer shape number" :value 58)))

(defcustom grab-and-drag-pointer-shape
  (and (boundp 'x-pointer-fleur) x-pointer-fleur)
  "Mouse pointer shape that `grab-and-drag' sets during mouse dragging.
The value nil means don't set the pointer shape."
  :group 'grab-and-drag
  :type '(choice (const :tag "Don't change pointer" :value nil)
		 (choice :tag "Pointer shape" :value 52
			 (const :tag "X-cursor" :value 0)
			 (const :tag "arrow" :value 2)
			 (const :tag "based-arrow-down" :value 4)
			 (const :tag "based-arrow-up" :value 6)
			 (const :tag "boat" :value 8)
			 (const :tag "bogosity" :value 10)
			 (const :tag "bottom-left-corner" :value 12)
			 (const :tag "bottom-right-corner" :value 14)
			 (const :tag "bottom-side" :value 16)
			 (const :tag "bottom-tee" :value 18)
			 (const :tag "box-spiral" :value 20)
			 (const :tag "center-ptr" :value 22)
			 (const :tag "circle" :value 24)
			 (const :tag "clock" :value 26)
			 (const :tag "coffee-mug" :value 28)
			 (const :tag "cross" :value 30)
			 (const :tag "cross-reverse" :value 32)
			 (const :tag "crosshair" :value 34)
			 (const :tag "diamond-cross" :value 36)
			 (const :tag "dot" :value 38)
			 (const :tag "dotbox" :value 40)
			 (const :tag "double-arrow" :value 42)
			 (const :tag "draft-large" :value 44)
			 (const :tag "draft-small" :value 46)
			 (const :tag "draped-box" :value 48)
			 (const :tag "exchange" :value 50)
			 (const :tag "fleur" :value 52)
			 (const :tag "gobbler" :value 54)
			 (const :tag "gumby" :value 56)
			 (const :tag "hand1" :value 58)
			 (const :tag "hand2" :value 60)
			 (const :tag "heart" :value 62)
			 (const :tag "icon" :value 64)
			 (const :tag "iron-cross" :value 66)
			 (const :tag "left-ptr" :value 68)
			 (const :tag "left-side" :value 70)
			 (const :tag "left-tee" :value 72)
			 (const :tag "leftbutton" :value 74)
			 (const :tag "ll-angle" :value 76)
			 (const :tag "lr-angle" :value 78)
			 (const :tag "man" :value 80)
			 (const :tag "middlebutton" :value 82)
			 (const :tag "mouse" :value 84)
			 (const :tag "pencil" :value 86)
			 (const :tag "pirate" :value 88)
			 (const :tag "plus" :value 90)
			 (const :tag "question-arrow" :value 92)
			 (const :tag "right-ptr" :value 94)
			 (const :tag "right-side" :value 96)
			 (const :tag "right-tee" :value 98)
			 (const :tag "rightbutton" :value 100)
			 (const :tag "rtl-logo" :value 102)
			 (const :tag "sailboat" :value 104)
			 (const :tag "sb-down-arrow" :value 106)
			 (const :tag "sb-h-double-arrow" :value 108)
			 (const :tag "sb-left-arrow" :value 110)
			 (const :tag "sb-right-arrow" :value 112)
			 (const :tag "sb-up-arrow" :value 114)
			 (const :tag "sb-v-double-arrow" :value 116)
			 (const :tag "shuttle" :value 118)
			 (const :tag "sizing" :value 120)
			 (const :tag "spider" :value 122)
			 (const :tag "spraycan" :value 124)
			 (const :tag "star" :value 126)
			 (const :tag "target" :value 128)
			 (const :tag "tcross" :value 130)
			 (const :tag "top-left-arrow" :value 132)
			 (const :tag "top-left-corner" :value 134)
			 (const :tag "top-right-corner" :value 136)
			 (const :tag "top-side" :value 138)
			 (const :tag "top-tee" :value 140)
			 (const :tag "trek" :value 142)
			 (const :tag "ul-angle" :value 144)
			 (const :tag "umbrella" :value 146)
			 (const :tag "ur-angle" :value 148)
			 (const :tag "watch" :value 150)
			 (const :tag "xterm" :value 152)
			 (const :tag "invisible" :value 255))
		 (integer :tag "Pointer shape number" :value 52)))

(defcustom grab-and-drag-enable-inertia t
  "Non-nil means enable `grab-and-drag' to cause an inertial scrolling
after mouse dragging."
  :group 'grab-and-drag
  :type 'boolean)

(defcustom grab-and-drag-timeout 1.0
  "Time limit (in seconds) that `grab-and-drag' accepts mouse dragging.
If the button is kept pressing without movement of the pointer until
it becomes timeout, the original command (normally `mouse-drag-region')
is called instead."
  :group 'grab-and-drag
  :type '(choice (number :tag "Timeout (sec)" :value 1.0)
		 (const :tag "No timeout" nil)))

(defcustom grab-and-drag-interval 0.05
  "Time interval (in seconds) for updating a window by `grab-and-drag'.
The value also specifies a minimum time interval of inertial scrolling."
  :group 'grab-and-drag
  :type 'number)

(defcustom grab-and-drag-gesture-time 500
  "Maximum time length of flick gestures recognized by `grab-and-drag'.
Measured in milliseconds. The negative value means disable the gesture
recognition. Users can change the actions performed in responses to the
horizontal flick gestures, which normally cause scrolling by screenfuls,
by setting commands to the options `grab-and-drag-gesture-left-command'
and `grab-and-drag-gesture-right-command'."
  :group 'grab-and-drag
  :type 'integer)

(defcustom grab-and-drag-gesture-left-command nil
  "Command that `grab-and-drag' invokes for a flick gesture to the left
direction. The value nil means use `grab-and-drag-scroll-left'."
  :group 'grab-and-drag
  :type '(choice (function :tag "Command")
		 (const :tag "Scroll left" nil)))

(defcustom grab-and-drag-gesture-right-command nil
  "Command that `grab-and-drag' invokes for a flick gesture to the right
direction. The value nil means use `grab-and-drag-scroll-right'."
  :group 'grab-and-drag
  :type '(choice (function :tag "Command")
		 (const :tag "Scroll right" nil)))

(defcustom grab-and-drag-vscroll-anim-step 4
  "Number of lines to move text of window for animated scrolling.
The values of this option and `grab-and-drag-scroll-anim-interval'
govern the vertical scroll speeds of `grab-and-drag-scroll-down'
and `grab-and-drag-scroll-up'."
  :group 'grab-and-drag
  :type 'integer)

(defcustom grab-and-drag-hscroll-anim-step 8
  "Number of columns to move text of window for animated scrolling.
The values of this option and `grab-and-drag-scroll-anim-interval'
govern the horizontal scroll speeds of `grab-and-drag-scroll-right'
and `grab-and-drag-scroll-left'."
  :group 'grab-and-drag
  :type 'integer)

(defcustom grab-and-drag-scroll-anim-interval 0.02
  "Time interval of updating a window for animated scrolling.
Measured in seconds. The value governs the scroll speeds together with
`grab-and-drag-vscroll-anim-step' or `grab-and-drag-hscroll-anim-step'."
  :group 'grab-and-drag
  :type 'number)

(defcustom grab-and-drag-context-columns 10
  "Number of columns for continuity of horizontal scrolling by screenfuls."
  :group 'grab-and-drag
  :type 'integer)

(defcustom grab-and-drag-beginning-of-buffer-hook nil
  "Normal hook run when scrolling reaches the beginning of buffer."
  :group 'grab-and-drag
  :type 'hook)

(defcustom grab-and-drag-end-of-buffer-hook nil
  "Normal hook run when scrolling reaches the end of buffer."
  :group 'grab-and-drag
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grab-and-drag-row-height (&optional window)
  "Return pixel height of rows in WINDOW, including line spacing.
Omitting WINDOW means use selected window."
  (unless window
    (setq window (selected-window)))
  (with-current-buffer (window-buffer window)
    (let* ((frame (window-frame window))
	   (spacing (or line-spacing
			(frame-parameter frame 'line-spacing)
			0)))
      (+ (if (and (bound-and-true-p text-scale-mode)
		  (not (zerop (with-no-warnings
				text-scale-mode-amount))))
	     (round (* (frame-char-height frame)
		       (with-no-warnings
			 (expt text-scale-mode-step
			       text-scale-mode-amount))))
	   (frame-char-height frame))
	 (if (floatp spacing)
	     (truncate (* (frame-char-height frame) spacing))
	   spacing)))))

(defun grab-and-drag-truncated-p (&optional window)
  "Return non-nil if lines in WINDOW are truncated."
  (unless window
    (setq window (selected-window)))
  (with-current-buffer (window-buffer window)
    (or (not (zerop (window-hscroll window)))
	(and truncate-partial-width-windows
	     (not (window-full-width-p window))
	     (or (not (integerp truncate-partial-width-windows))
		 (< (window-width window)
		    truncate-partial-width-windows)))
	truncate-lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inertial scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar grab-and-drag-timer-alist nil
  "Alist of timers internally used for inertial scrolling.
car and cdr of each element are window and timer, respectively.")

(defun grab-and-drag-stop-scrolling (&optional window)
  "Stop inertial scrolling of WINDOW.
Omitting WINDOW means use selected window."
  (unless window
    (setq window (selected-window)))
  (let ((windows (list window))
	timer)
    ;; If follow-mode is active, try canceling the timer for all
    ;; windows displaying the same buffer.
    (if (and (window-live-p window)
	     (bound-and-true-p follow-mode))
	(let ((frame (window-frame window))
	      (buffer (window-buffer window)))
	  (dolist (w (cdr (window-list frame 0 window)))
	    (when (eq (window-buffer w) buffer)
	      (push w windows)))))
    (dolist (w windows)
      (setq timer (assq w grab-and-drag-timer-alist))
      (when timer
	(cancel-timer (cdr timer))
	(setq grab-and-drag-timer-alist (delq timer
					      grab-and-drag-timer-alist)))))
  (when (null grab-and-drag-timer-alist)
    (remove-hook 'post-command-hook 'grab-and-drag-stop-scrolling)))

(defun grab-and-drag-scroll-1 (displacement window)
  "Scroll WINDOW down DISPLACEMENT lines.
Negative DISPLACEMENT means scroll upward."
  (condition-case nil
      (with-selected-window window
	(scroll-down displacement)
	;; Synchronize window scrolling when follow-mode.
	(with-current-buffer (window-buffer)
	  (if (bound-and-true-p follow-mode)
	      (let ((inhibit-redisplay t))
		(funcall (with-no-warnings 'follow-post-command-hook))))))
    ;; Stop scrolling if reaching the beginning or end of buffer.
    (beginning-of-buffer
     (with-selected-window window
       (with-current-buffer (window-buffer)
	 (set-window-start window (point-min))
	 (unwind-protect
	     (run-hooks 'grab-and-drag-beginning-of-buffer-hook)
	   (if (eq (window-start) (point-min))
	       (grab-and-drag-stop-scrolling))))))
    (end-of-buffer
     (with-selected-window window
       (with-current-buffer (window-buffer)
	 (set-window-start window (point-max))
	 (unwind-protect
	     (run-hooks 'grab-and-drag-end-of-buffer-hook)
	   (if (eq (window-start) (point-max))
	       (grab-and-drag-stop-scrolling))))))
    ;; Cancel timer if WINDOW is not a window or already deleted.
    (error
     (grab-and-drag-stop-scrolling window))))

(defun grab-and-drag-start-scrolling (velocity &optional window)
  "Start inertial scrolling of WINDOW with vertical VELOCITY (pixcels/sec).
Omitting WINDOW means use selected window."
  (unless window
    (setq window (selected-window)))
  (grab-and-drag-stop-scrolling window)
  (when (and (window-live-p window)
	     (numberp velocity)
	     (not (zerop velocity)))
    ;; If follow-mode is active, select the first window for
    ;; positive velocity or the last window for negative velocity,
    ;; to avoid conflicts of multiple scrolling velocities of
    ;; windows displaying the same buffer.
    (if (bound-and-true-p follow-mode)
	(let ((frame (window-frame window))
	      (buffer (window-buffer window))
	      (pos (if (> velocity 0) (point-max) (point-min))))
	  (dolist (w (window-list frame 0))
	    (when (and (eq (window-buffer w) buffer)
		       (if (> velocity 0)
			   (< (window-start w) pos)
			 (> (window-start w) pos)))
	      (setq window w
		    pos (window-start w))))))
    ;; Register the scrolling timer.
    (let ((interval (/ (grab-and-drag-row-height window)
		       (float (abs velocity))))
	  (drows (if (> velocity 0) 1 -1)))
      (when (< interval grab-and-drag-interval)
	(setq drows (* drows (ceiling (/ grab-and-drag-interval interval)))
	      interval (* interval (float (abs drows)))))
      (push (cons window
		  (run-with-timer interval interval
				  'grab-and-drag-scroll-1 drows window))
	    grab-and-drag-timer-alist))
    ;; Register post-command-hook to stop scrolling.
    (remove-hook 'post-command-hook 'grab-and-drag-stop-scrolling)
    ;; Use timer to avoid immediate stop of the scrolling.
    (run-with-timer 0 nil
		    'add-hook 'post-command-hook 'grab-and-drag-stop-scrolling)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animated scrolling by screenfuls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grab-and-drag-scroll-down ()
  "Scroll backward by nearly a full window with an animation effect.
The scroll amount is `next-screen-context-lines' less than a window height.
The animation effect of scrolling can be conditioned by options
`grab-and-drag-vscroll-anim-step' and `grab-and-drag-scroll-anim-interval'."
  (interactive)
  (with-current-buffer (window-buffer)
    (let ((goal (condition-case nil
		    (save-excursion
		      (save-window-excursion
			(scroll-down)
			(window-start)))
		  (beginning-of-buffer (point-min)))))
      (condition-case nil
	  (while (> (window-start) goal)
	    (sit-for grab-and-drag-scroll-anim-interval)
	    (scroll-down grab-and-drag-vscroll-anim-step))
	(beginning-of-buffer nil))
      (unless (= (window-start) goal)
	(set-window-start (selected-window) goal))
      (if (eq (window-start) (point-min))
	  (unwind-protect
	      (run-hooks 'grab-and-drag-beginning-of-buffer-hook)
	    ;; Show message "Beginning of buffer"
	    (if (eq (window-start) (point-min))
		(scroll-down 1)))))))

(defun grab-and-drag-scroll-up ()
  "Scroll forward by nearly a full window with an animation effect.
The scroll amount is `next-screen-context-lines' less than a window height.
The animation effect of scrolling can be conditioned by options
`grab-and-drag-vscroll-anim-step' and `grab-and-drag-scroll-anim-interval'."
  (interactive)
  (with-current-buffer (window-buffer)
    (let ((goal (condition-case nil
		    (save-excursion
		      (save-window-excursion
			(scroll-up)
			(window-start)))
		  (end-of-buffer (point-max)))))
      (condition-case nil
	  (while (< (window-start) goal)
	    (sit-for grab-and-drag-scroll-anim-interval)
	    (scroll-up grab-and-drag-vscroll-anim-step))
	(end-of-buffer nil))
      (unless (= (window-start) goal)
	(set-window-start (selected-window) goal))
      (if (eq (window-start) (point-max))
	  (unwind-protect
	      (run-hooks 'grab-and-drag-end-of-buffer-hook)
	    ;; Show message "End of buffer"
	    (if (eq (window-start) (point-max))
		(scroll-up 1)))))))

(defun grab-and-drag-scroll-right ()
  "Scroll right by nearly a full window with an animation effect. The
scroll amount is `grab-and-drag-context-columns' less than a window width.
The animation effect of scrolling can be conditioned by options
`grab-and-drag-hscroll-anim-step' and `grab-and-drag-scroll-anim-interval'."
  (interactive)
  (let ((goal (max (- (window-hscroll)
		      (- (window-width) grab-and-drag-context-columns))
		   0))
	automatic-hscrolling)
    (while (> (window-hscroll) goal)
      (sit-for grab-and-drag-scroll-anim-interval)
      (scroll-right (min grab-and-drag-hscroll-anim-step
			 (- (window-hscroll) goal))
		    grab-and-drag-hscroll-anim-step))
    (if (grab-and-drag-truncated-p)
	(let ((x-y (pos-visible-in-window-p
		    ;; `pos-visible-in-window-p' returns incorrect coordinates
		    ;; when point is at the end of buffer and overflows into
		    ;; the right-hand side of a window.
		    (and (= (point) (point-max)) (line-beginning-position))
		    nil t))
	      (xmax (- (window-width) hscroll-margin 1)))
	  (if (or (> (/ (car x-y) (frame-char-width)) xmax)
		  (= (point) (point-max)))
	      (goto-char (posn-point (posn-at-x-y (* xmax (frame-char-width))
						  (cadr x-y)))))))))

(defun grab-and-drag-scroll-left ()
  "Scroll left by nearly a full window with an animation effect. The
scroll amount is `grab-and-drag-context-columns' less than a window width.
The animation effect of scrolling can be conditioned by options
`grab-and-drag-hscroll-anim-step' and `grab-and-drag-scroll-anim-interval'."
  (interactive)
  (let ((goal (+ (window-hscroll)
		 (- (window-width) grab-and-drag-context-columns))))
    (while (< (window-hscroll) goal)
      (sit-for grab-and-drag-scroll-anim-interval)
      (scroll-left (min grab-and-drag-hscroll-anim-step
			(- goal (window-hscroll)))
		   grab-and-drag-hscroll-anim-step))
    (let ((x-y (pos-visible-in-window-p nil nil t)))
      (if (< (car x-y) 0)
	  (goto-char (posn-point (posn-at-x-y 0 (cadr x-y))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grab and Drag command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grab-and-drag-call-default-binding (event)
  "Run command that EVENT is bound to, without consideration of any
bindings included in `grab-and-drag-mode-map'."
  (with-no-warnings
    (when grab-and-drag-mode
      (setcdr (assq 'grab-and-drag-mode minor-mode-map-alist) nil)
      (unwind-protect
	  (let* ((key (vector (event-convert-list
			       ;; Get rid of `double' modifier  because
			       ;; it prevents `moouse-drag-region' from
			       ;; recognizing a multiple click event.
			       (delq 'double
				     (append (event-modifiers event)
					     (list (event-basic-type event)))))))
		 (command (key-binding key nil nil (event-start event))))
	    (when (and (commandp command)
		       ;; Avoid infinite loop.
		       (not (eq command this-command)))
	      (setq this-command command)
	      (call-interactively command)))
	(setcdr (assq 'grab-and-drag-mode minor-mode-map-alist)
		grab-and-drag-mode-map)))))

(defvar grab-and-drag-orig-pointer-shape
  (and (boundp 'x-pointer-shape) x-pointer-shape)
  "Mouse pointer shape that `grab-and-drag' uses when mouse dragging
becomes timeout. This indicates the command originally bound to the
mouse event is running.

Note that the users must not change this variable directly when
`grab-and-drag-mode' is active.")

(defun grab-and-drag (click)
  "Scroll window by mouse dragging, and move point to that position.
CLICK must be a mouse down event which starts the mouse dragging.
The mouse dragging shorter than `grab-and-drag-gesture-time' is recognized
as a flick gesture, which causes a scrolling by a screenful. The long time
dragging can make an inertial scrolling if `grab-and-drag-enable-inertia'
is non-nil. `grab-and-drag-timeout' is the time limit that the mouse
dragging is accepted. `grab-and-drag-interval' is the time interval for
updating the window."
  (interactive "e")
  (let* ((button (if (memq 'down (event-modifiers click))
		     (event-basic-type click)
		   (error "not a mouse down event: %s" click)))
	 (posn (event-start click))
	 (window (posn-window posn))
	 (orig-window (selected-window))
	 (orig-buffer (current-buffer)))
    (grab-and-drag-stop-scrolling window)
    (if (window-minibuffer-p window)
	;; Minibuffer can't be scrolled.
	(grab-and-drag-call-default-binding click)
      ;; Give temporary modes a chance to turn off.
      (run-hooks 'mouse-leave-buffer-hook)
      ;; Go to the place that was clicked.
      (select-window window)
      (set-buffer (window-buffer))
      (goto-char (posn-point posn))
      (let* ((orig-pointer-shape (and (boundp 'x-pointer-shape) x-pointer-shape))
	     (orig-mouse-color (frame-parameter nil 'mouse-color))
	     (edges (window-pixel-edges))
	     (inside-edges (window-inside-pixel-edges))
	     (maxpos (and (eq (window-end) (point-max))
			  (pos-visible-in-window-p (point-max) nil t)))
	     ;; Calculate offset from cursor position to mouse pointer.
	     ;; It's necessary when mouse dragging starts from the place
	     ;; where cursor can't be put such as the end of buffer.
	     (offset (and (eq (point) (point-max))
			  (+ (- (cdr (posn-x-y posn))
				(cadr maxpos))
			     (- (cadr inside-edges)
				(cadr edges)))))
	     (row-height (grab-and-drag-row-height))
	     (truncated (grab-and-drag-truncated-p))
	     (winstart0 (window-start))
	     (winend0 (window-end))
	     (hscroll0 (window-hscroll))
	     (mpos0 (cdr (mouse-pixel-position)))
	     (mpos mpos0)
	     (time (float-time))
	     prev-mpos prev-time event event0
	     (slip 0) (prev-dy 0)
	     (hscroll-step 1))
	;; Change mouse pointer's shape.
	(when grab-and-drag-pointer-shape
	  (setq x-pointer-shape grab-and-drag-pointer-shape)
	  (set-mouse-color orig-mouse-color))
	(unwind-protect
	    (catch 'exit
	      ;; Wait for first movement.
	      (track-mouse
		(setq event0 (read-event nil nil grab-and-drag-timeout))
		(if event0
		    (push event0 unread-command-events)
		  (unless (eq (and (boundp 'x-pointer-shape) x-pointer-shape)
			      grab-and-drag-orig-pointer-shape)
		    (setq x-pointer-shape grab-and-drag-orig-pointer-shape)
		    (set-mouse-color orig-mouse-color))
		  (grab-and-drag-call-default-binding click)
		  (throw 'exit nil)))
	      ;; Start main loop.
	      (while t
		(setq prev-mpos mpos
		      prev-time time)
		;; It's necessary to get mouse position before `read-event'
		;; because `mouse-position' might return nil for coordinates
		;; after mouse button is released.
		(setq mpos (cdr (mouse-pixel-position))
		      time (float-time)
		      event (read-event nil nil grab-and-drag-interval))
		;; Exit loop if button up event is received.
		(when (eq (event-basic-type event) button)
		  (if (memq 'drag (event-modifiers event))
		      (when (and grab-and-drag-enable-inertia
				 (cdr mpos))
			(cond
			 ;; Do nothing if pointer stops before up-event.
			 ((equal mpos prev-mpos))
			 ;; Setup inertial scrolling.
			 ((> (- (posn-timestamp (event-end event))
				(posn-timestamp (event-start event0)))
			     grab-and-drag-gesture-time)
			  (grab-and-drag-start-scrolling (/ (- (cdr mpos)
							       (cdr prev-mpos))
							    (- time prev-time))
							 window))
			 ;; Vertically scroll a screenful.
			 ((< (abs (- (car mpos) (car mpos0)))
			     (abs (- (cdr mpos) (cdr mpos0))))
			  (set-window-hscroll (selected-window) hscroll0)
			  (let ((next-screen-context-lines
				 next-screen-context-lines))
			    (if (> (cdr mpos) (cdr mpos0))
				(progn
				  (save-excursion
				    (goto-char (window-start))
				    (while (< (point) winstart0)
				      (line-move 1 t)
				      (setq next-screen-context-lines
					    (1+ next-screen-context-lines))))
				  (grab-and-drag-scroll-down))
			      (save-excursion
				(goto-char (window-end))
				(let ((visible (pos-visible-in-window-p nil nil t)))
				  (when visible
				    (setq next-screen-context-lines
					  (+ next-screen-context-lines
					     (/ (- (if maxpos
						       (cadr maxpos)
						     (- (nth 3 inside-edges)
							(cadr edges)))
						   (cadr visible))
						row-height)))))
				(while (> (point) winend0)
				  (line-move -1 t)
				  (setq next-screen-context-lines
					(1+ next-screen-context-lines))))
			      (grab-and-drag-scroll-up))))
			 ;; Horizontally scroll a screenful.
			 ((> (car mpos) (car mpos0))
			  (set-window-start (selected-window) winstart0)
			  (if grab-and-drag-gesture-right-command
			      (progn
				(set-window-hscroll (selected-window) hscroll0)
				(call-interactively
				 grab-and-drag-gesture-right-command))
			    (if truncated
				(let ((grab-and-drag-context-columns
				       (+ grab-and-drag-context-columns
					  (- hscroll0 (window-hscroll)))))
				  (grab-and-drag-scroll-right)))))
			 (t
			  (set-window-start (selected-window) winstart0)
			  (if grab-and-drag-gesture-left-command
			      (progn
				(set-window-hscroll (selected-window) hscroll0)
				(call-interactively
				 grab-and-drag-gesture-left-command))
			    (if truncated
				(let ((grab-and-drag-context-columns
				       (+ grab-and-drag-context-columns
					  (- (window-hscroll) hscroll0))))
				  (grab-and-drag-scroll-left)))))))
		    ;; If pointer wasn't moved, put the up-event back and run
		    ;; a command bound to the click event with lower priority.
		    (push event unread-command-events)
		    (grab-and-drag-call-default-binding click))
		  (throw 'exit nil))
		;; Horizontal scrolling.
		(if truncated
		    (let* ((dx (- (/ (car mpos) (frame-char-width))
				  (/ (car prev-mpos) (frame-char-width))))
			   (vx (if (and (> slip 0) (< dx 0))
				   (min 0 (+ dx slip))
				 dx))
			   (hscroll (window-hscroll)))
		      (unless (zerop vx)
			(scroll-right vx vx)
			;; Give a chance of automatic scrolling.
			(sit-for 0))
		      (setq slip (+ slip dx (- (window-hscroll) hscroll)))))
		;; Vertical scrolling.
		(let* ((y (- (min (max (cadr inside-edges)
				       (- (cdr mpos) (or offset 0)))
				  (- (nth 3 inside-edges)
				     (or (nth 4 (pos-visible-in-window-p
						 (1- (window-end)) nil t))
					 0)
				     1))
			     (cadr edges)))
		       (vy (round (- y
				     (cadr (pos-visible-in-window-p nil nil t))
				     (ash row-height -1))
				  row-height))
		       (dy (- (cdr mpos) (cdr prev-mpos))))
		  ;; `dy' and `prev-dy' were introduced so that the window
		  ;; doesn't shake vertically when it includes images.
		  (if (zerop dy)
		      (setq dy prev-dy)
		    (setq prev-dy dy))
		  (unless (or (zerop vy)
			      (< (* dy vy) 0))
		    (save-excursion
		      (condition-case nil
			  (scroll-down vy)
			(beginning-of-buffer
			 (set-window-start window (point-min)))
			(end-of-buffer
			 (set-window-start window (point-max)))
			(error nil)))))
		;; Synchronize window scrolling when follow-mode.
		(if (bound-and-true-p follow-mode)
		    (funcall (with-no-warnings 'follow-post-command-hook)))))
	  ;; Restore mouse pointer's shape.
	  (unless (eq (and (boundp 'x-pointer-shape) x-pointer-shape)
		      orig-pointer-shape)
	    (setq x-pointer-shape orig-pointer-shape)
	    (set-mouse-color orig-mouse-color)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grab-and-drag-set-pointer-shape (shape)
  "Set the shape of the mouse pointer of all frames to SHAPE."
  (setq x-pointer-shape shape)
  (let ((orig-frame (selected-frame)))
    (unwind-protect
	(mapc (lambda (frame)
		(select-frame frame)
		(set-mouse-color (frame-parameter frame 'mouse-color)))
	      (frame-list))
      (select-frame orig-frame))))

(defun grab-and-drag-make-keymap ()
  "Return keymap used for `grab-and-drag-mode'."
  (let ((map (make-sparse-keymap)))
    (define-key map
      (vector (intern (format "down-mouse-%s" grab-and-drag-button)))
      'grab-and-drag)
    map))

(define-minor-mode grab-and-drag-mode
  "Toggle `Grab and Drag' minor mode (grab-and-drag-mode).
With optional argument ARG, turn grab-and-drag-mode on if ARG is
positive, otherwise turn it off.

When grab-and-drag-mode is enabled, the mouse dragging with the
button specified by `grab-and-drag-button' makes a window scroll
in the arbitrary direction. See the command `grab-and-drag'."
  nil
  " GaD"
  (make-sparse-keymap)
  :group 'grab-and-drag
  :global t
  (if grab-and-drag-mode
      (with-no-warnings
	(set-keymap-parent grab-and-drag-mode-map (grab-and-drag-make-keymap))
	(setq grab-and-drag-orig-pointer-shape (and (boundp 'x-pointer-shape)
						    x-pointer-shape))
	(when grab-and-drag-mode-pointer-shape
	  (grab-and-drag-set-pointer-shape grab-and-drag-mode-pointer-shape)))
    (unless (eq (and (boundp 'x-pointer-shape) x-pointer-shape)
		grab-and-drag-orig-pointer-shape)
      (grab-and-drag-set-pointer-shape grab-and-drag-orig-pointer-shape))))

;; Setup mode-line-mode-menu.
(define-key mode-line-mode-menu [grab-and-drag-mode]
  `(menu-item ,(purecopy "Grab and Drag") grab-and-drag-mode
	      :help "Scroll a window by mouse dragging"
	      :button (:toggle . (bound-and-true-p grab-and-drag-mode))))

(provide 'grab-and-drag)

;;
;;; grab-and-drag.el ends here

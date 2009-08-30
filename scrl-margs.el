;;; scrl-margs.el --- scroll margins minor mode
;;-------------------------------------------------------------------
;;
;; Copyright (C) 2002,2003,2008 David Andersson
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------
;;
;; Author: David Andersson <l.david.andersson(at)sverige.nu>
;; Created: 10 May 2002
;; Version: 0.4 (2008-09-19)
;; Keywords: scroll
;;
;;; Commentary:
;;
;;   A minor mode that maintain margins between the cursor and the top and
;;   bottom of a window. It scrolls the window if the cursor is moved to close
;;   to either edge.
;;
;;   If point is moved to a place not visible in the window, Emacs will
;;   recenter as usual.
;;
;; Usage:
;;
;;   Toggle this minor mode:
;;
;;	  M-x scroll-margs-mode
;;
;;   or set margin and enable this minor mode:
;;
;;	  M-x set-scroll-margs
;;
;; Install:
;;
;;   Place "scrl-margs.el" in a directory in your load-path and
;;   add to your ~/.emacs file:
;;
;;	  (require 'scrl-margs)
;;   or
;;	  (autoload 'scroll-margs-mode "scrl-margs" nil t)
;;	  (autoload 'set-scroll-margs "scrl-margs" nil t)
;;
;;   To enable it at start up time, also add to your ~/.emacs file:
;;
;;	  (scroll-margs-mode 1) 
;;   or
;;	  (set-scroll-margs 3 3)
;;
;;   If you have defined your own scrolling command, or otherwise don't want
;;   scrl-margs to interfere with a command, put a property value to that
;;   commands function name. The property is `scroll-margs' and the value
;;   is preferably `no-scroll' but can be anything except nil. Example:
;;
;;	  (put 'my-recenter-cursor-at-top 'scroll-margs 'no-scroll)
;;
;; Compatibility
;;
;;   + This module works with GNU Emacs-19.29, 19.31, 20.6, 20.7, 21.1 and 22.1
;;     and XEmacs-20.1, 20.4 and 21.1 (and probably all versions inbetween).
;;     It does not work with XEmacs-21.4.
;;
;;   + The scroll margin capabilitis of Emacs-20 and later (`scroll-margin',
;;     `scroll-conservatively' and `scroll-step') and this module lives quite
;;     happily together and complement each other. This module is more smooth
;;     than Emacs-20's `scroll-margin' in comint modes and during query replace,
;;     and seems to handle `mouse-set-point' better than Emacs-22.
;;     Setting `scroll-conservatively' while using this mode makes scrolling
;;     over really long lines smoother.
;;
;;   + It seems to work with `truncate-lines' and `truncate-partial-width-windows'.
;;
;;   + If works with hidden text in e.g. outline-mode.
;;
;;   + It seems to work with the "scroll-in-place" module. (Version 1.3 of
;;     that module redefines the functions `scroll-up' and `scroll-down' with
;;     new semantics. The scroll-in-place mode is temporarly disabled to
;;     get the standard behaviour of those functions.)
;;
;; Incompatibility
;;
;;   - This module does not cooperate well with the "follow-mode".
;;
;; Shortcomings
;;
;;   - Scroll margins cannot be set to zero.
;;
;;   - Performance. The function `scroll-margs-curr-line' is called after almost
;;     every motion command and do a lot of vertical motions internally.
;;     Notisable performance degration when window contains large amounts
;;     of hidden text, e.g. in outline mode.
;;
;;-------------------------------------------------------------------
;;
;; History:
;;
;; 0.4 (2008-09-19) David Andersson <l.david.andersson(at)sverige.nu>
;;   New e-mail address; Update compatibilivy comments.
;; 0.3 (2003-08-14) David Andersson
;;   XEmacs-21.1. Somewhat boost `scroll-margs-curr-line'.
;; 0.2 (2002-06-19) David Andersson
;;   Set `scroll-step'. Improve docs and comments.
;; 0.1 (2002-05-18) David Andersson
;;   Modularised
;;
;;-------------------------------------------------------------------
;;
;;todo: File name is cryptic but within recommended length. Find a better name?
;;todo: Change prefix "scroll-margs" to "scroll-margins" or something?
;;todo: Respect a commands 'no-scroll' property more than one event after.
;;todo: Better support for (optionally) making this a buffer local mode.
;;todo: Allow margins to be float, then meaning fraction of window height.
;;todo: Be follow-mode compatible.
;;todo: Declare vars for customization.
;;todo: Overstep the margin when it helps keeping active region visible.
;;todo: Maintain margins when window is resized?
;;todo: Use hook window-scroll-functions? Defadvice recenter?
;;
;;-------------------------------------------------------------------

;;; Code:

;; Register commands after which scroll margins should not be checked
(put 'mouse-drag-region		'scroll-margs 'no-scroll)
(put 'mouse-set-point		'scroll-margs 'no-scroll)
(put 'mouse-track		'scroll-margs 'no-scroll)
(put 'recenter			'scroll-margs 'no-scroll)
(put 'scroll-up			'scroll-margs 'no-scroll)
(put 'scroll-down		'scroll-margs 'no-scroll)
(put 'other-window		'scroll-margs 'no-scroll)
;(put 'scroll-bar-scroll-up	'scroll-margs 'point-follows-window)
;(put 'scroll-bar-drag		'scroll-margs 'point-follows-window)
;(put 'scroll-bar-scroll-down	'scroll-margs 'point-follows-window)

(defvar scroll-margs-top 3 
  "*Top scroll margin. 
Scroll down if cursor is placed closer than this to the top of the window.
See `scroll-margs-mode'.")
(defvar scroll-margs-bottom 3 
  "*Bottom scroll margin. 
Scroll up if cursor is placed closer than this to the bottom of the window.
See `scroll-margs-mode'.")

(defvar scroll-margs-mode nil 
  "Non-nil when scroll margins mode is enabled.
\nBy default scroll margins mode is global.
You can make the mode buffer local with (make-local-variable 'scroll-margs-mode).
You might then consider making `scroll-margs-top', `scroll-margs-bottom' 
and `scroll-step' local too.
\nAlso see command `scroll-margs-mode'.")

(defvar scroll-margs-last-point nil 
  "Save position of point to detect move direction. Internal.")
;; Make this variable buffer local. But it would better be "window local"
;; if there were such a thing.
(make-variable-buffer-local 'scroll-margs-last-point)

;; (defvar scroll-margs-inhibit nil
;;   "When non-nil: don't scroll even if within the scroll margins.
;; The flag is reset when a scroll is forced.")

;; Idle timer. Started the first time scroll margins mode is enabled.
(defvar scroll-margs-timer nil)


;;;###autoload
(defun set-scroll-margs (top &optional bottom)
  "If TOP and BOTTOM are positive numbers, set scroll margins
to those numbers and enable scroll margs mode,
If either is zero or both are nil, disable scroll margs mode.
If BOTTOM is omitted, it defaults to the same as TOP.
\nWhen called interactively it ask for just one value that becomes
both top and bottom margins.
\nSee also command `scroll-margs-mode'."
  (interactive "nScroll margins (0=off): ")
  (if (null bottom) (setq bottom top))
  (if (or (null top) (not (numberp top)) (zerop top)
	  (null bottom) (not (numberp bottom)) (zerop bottom))
      (scroll-margs-mode 0)
    ;; else
    (setq scroll-margs-top top)    
    (setq scroll-margs-bottom bottom)
;    (setq scroll-step (1+ (min top bottom)))
;    (setq scroll-conservatively 999)
    (scroll-margs-mode 1)))

;;;###autoload
(defun scroll-margs-mode (arg)
  "Toggle scroll margins mode.
If optional ARG is positive, enable mode. If zero or negative, disable mode.
\nWhen scroll margins mode is enabled the window scrolls when the cursor
is moved too close to the top or bottom of the window. Thus you can always
see the context of the current line.
\nBy default margins are 3 lines. You can change margins either by
setting variables `scroll-margs-top' and `scroll-margs-bottom'
or by calling the command `set-scroll-margs'.
\nThis minor mode currently affects all buffers (windows). 
See variable `scroll-margs-mode' how to change that."
  (interactive "P")
  (setq scroll-margs-mode (if arg
			      (> (prefix-numeric-value arg) 0)
			    (not scroll-margs-mode)))
  (cond (scroll-margs-mode
	 (scroll-margs-setup)
	 (message "Scroll margins on (%d & %d)" scroll-margs-top scroll-margs-bottom))
	(t
	 (scroll-margs-cleanup)
	 (message "Scroll margins off")))
  scroll-margs-mode)

;; Set up hooks for scroll margins
;; Uses idle timer if it exists. Otherwise resort to post-command-hook,
;; which is not run during e.g. query-replace.
;; (Emacs-19.28 and XEmacs-20.4 doesn't have run-with-idle-timer,
;; but XEmacs might have something else as good with a different name.)
;; (XEmacs-21 has run-with-idle-timer but doesn't allow zero delay,
;; thus the 0.000001 below.)
;; (In Emacs-20.6 and 21.1 idle timer is not called during query-replace.)
(defun scroll-margs-setup ()
  "Set up things when scroll-margs-mode is turned on. Internal."
  (if (fboundp 'run-with-idle-timer)
      (if (not scroll-margs-timer)
	  (setq scroll-margs-timer (run-with-idle-timer 0.000001 'repeat 'scroll-margs-handler)))
    ;; else
    (add-hook 'post-command-hook 'scroll-margs-handler)))

(defun scroll-margs-cleanup ()
  "Clean up when scroll-margs-mode is turned off. Internal."
  ;; Don't cancel timer or hook, it might be used in other buffers
  ;; if mode variables have been made buffer local.
)

;; This function is called after every command,
;; either from an idle timer or from the post-command-hook.
;; It tests if point has been moved past the top or bottom margin,
;; and scrolls (recenter) if it has.
;;todo: Don't trust (window-end) to be correct unless directly after redisplay
(defun scroll-margs-handler ()
  "Scroll window if point have moved past scroll margins.
See variables `scroll-margs-top' and `scroll-margs-bottom'.
\nBehaviour depends on which direction point has moved.
To be called from idle timer or post-command-hook."
  (if (and scroll-margs-mode
	   scroll-margs-last-point
	   (/= (point) scroll-margs-last-point)
	   (> (window-height) (+ scroll-margs-top scroll-margs-bottom 1))
	   (not (and (symbolp last-command) (get last-command 'scroll-margs)))
	   (not (and (symbolp this-command) (get this-command 'scroll-margs)))
	   )
	(cond ((and (< (point) scroll-margs-last-point) ; moved backward
		    (> (window-start) (point-min)) ; not top
		    (>= (point) (window-start))) ; not moved out of sight
	       (let ((l1 (scroll-margs-curr-line scroll-margs-top))
		     (scroll-in-place nil))
		 (if (< l1 scroll-margs-top)
		     (scroll-down (- scroll-margs-top l1)))))
	      ((and (> (point) scroll-margs-last-point) ; moved forward
		    (not (pos-visible-in-window-p (point-max))) ; not bottom
		    (< (point) (window-end))) ; not moved out of sight
	       (let ((l2 (- (window-height) (scroll-margs-curr-line) 2))
		     (scroll-in-place nil))
		 (if (< l2 scroll-margs-bottom)
		     (scroll-up (- scroll-margs-bottom l2)))))))
  (setq scroll-margs-last-point (point)))

;; (defun scroll-margs-curr-line (&optional maxline)
;;   "Return the window line number the point is on, counting from zero.
;; Option arg MAXLINE can be used to optimize performance if you don't need
;; results larger than that.
;; Negative MAXLINE counts from the bottom of the window
;; \(but the returned result is always non-negative)."
;;   (cond ((null maxline)
;; 	 (setq maxline (1- (window-height)))) ; don't count mode line
;; 	((< maxline 0)
;; 	 (setq maxline (+ (window-height) maxline))))
;;   ;; If maxline always is (window-height)-1 or greater
;;   ;; save-excursion would not be needed. Vertical-motion takes us to point.
;;   ;; (What is more expensive? A full vertical-motion or save-excursion?)
;;   (save-excursion (save-restriction 
;;     (narrow-to-region (point-min) (point))
;;     (goto-char (window-start))
;;     (vertical-motion maxline))))

(defun scroll-margs-curr-line (&optional maxline)
  "Return the window line number the point is on, counting from zero.
Option arg MAXLINE can be used to optimize performance if you don't need
results larger than that.
Negative MAXLINE counts from the bottom of the window
\(but the returned result is always non-negative)."
  (cond ((null maxline)
	 (setq maxline (1- (window-height)))) ; don't count mode line
	((< maxline 0)
	 (setq maxline (+ (window-height) maxline))))
  ;; This algorithm, avoiding narrowing, is at least twice as fast as the above.
  ;; It is still a bit slow if there is a lot of invisible text in the window.
  ;; Isn't there a simple way to get the screen line of the cursor in Emacs?
  (save-excursion
    (vertical-motion 0)			; goto beginning of *screen* line
    (let ((p (point))
	  (count 0))
      (goto-char (window-start))
      (while (and (< count maxline)
		  (< (point) p)
		  (< 0 (vertical-motion 1)))
	(setq count (1+ count)))
      count)))

;; Enable now if the mode variable was set prior to loading this module.
;; (The user should have called the mode function instead, but let's be helpful.)
(if scroll-margs-mode
    (scroll-margs-mode 1))

(provide 'scrl-margs)

;;; scrl-margs.el ends here

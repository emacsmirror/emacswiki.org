;;; planner-frame.el --- Devote a frame to the Planner

;; Copyright (C) 2005 Jesse Alama <alama@stanford.edu>

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;
;; I find it helpful to look at my planner within one frame, taking up
;; the only window of that frame.  I want to treat the Planner as an
;; application in the sense of giving it its own frame and dealing
;; with that frame intelligently.  I've been thus far dissatisfied
;; with other approaches (e.g., `C-x 5 2 RET M-x plan'), so I decided
;; to try my hand at making an extension of planner that does things
;; the way I want.
;;
;; What is the way I want?  Answer: gnus.  The code that follows is
;; based on the way that gnus regards itself as an application: it
;; provides a function `gnus-other-frame' for reading mail and news in
;; a distinguished frame, `gnus-exit-gnus' to save one's configuration
;; and stop reading mail and news and, and `gnus-suspend-gnus' for
;; making the interface go away temporarily while leaving most of its
;; state intact.  I also find it easy to deal with emacs-w3m, which I
;; regard as another emacs application.
;;
;; Currently planner doesn't seem to fit this way of thinking (or at
;; least: I wasn't smart enough or informed enough to see how Planner
;; could be viewed this way), so I created planner-frame.  The purpose
;; is to enable one to think of the Planner as an emacs application in
;; a way similar to how one can view gnus and emacs-w3m as emacs
;; applications.  (A good deal of the code here is directly adapted,
;; if not outright copied, from gnus.  We have here yet another case
;; where the ability to study and adapt code from other applications
;; was a source of enlightenment, satisfaction, and, I hope, value to
;; others.)
;;
;; The user functions are `plan-other-frame',
;; `planner-suspend-planner', and `planner-exit-planner'.  I've also
;; added a couple new keybindings for the latter two functions.  By
;; analogy with gnus, for these two "quit" functions I've created the
;; hooks `planner-exit-planner-hook' and
;; `planner-suspend-planner-hook'.  Customize
;; `planner-frame-parameters' to modify the frame produced by
;; `plan-other-frame'.
;;
;;
;;
;; To get started, simply add the form
;;
;;   (require 'planner-frame)
;;
;; to your emacs initialization file.  The function `plan-other-frame'
;; will then get your started planning in a distinguished planner
;; frame.
;;
;;; Todo
;;
;;; - Integrate with `planner.el'
;;
;; It could be valuable to somehow integrate the code here with
;; `planner.el', ala gnus, which provides `gnus-other-frame' and its
;; supporting code alongside `gnus'.
;;
;;; - Offer a way to make the planner frame permanently visible.
;;
;;; - Launch applications in other frames
;;
;; It would be nice to make things so that following links to data
;; outside the Planner project (either in the emacs-wiki or muse
;; sense) pops up a new frame.  The idea here is to make the Planner,
;; in its own frame, a kind of repellant force: the only things that
;; really go in that frame are Planner pages; trying to go outside the
;; planner lands one in a different frame.  Enforcing such
;; restrictions could be helpful; thus there would be little chance of
;; getting distracted, with one's planner pages buried deep uneath a
;; pile of erc, bbdb, message, and other buffers.  It would allow one
;; to better focus on planning and what needs to be done.  It would
;; make the Planner a kind of omnipresent application.
;;
;;; - Integration with other applications
;;
;; (Alert: vague rambling follows.)  My idea is to somehow collect PIM
;; applications together.  The Planner isn't the only one: there's the
;; diary and the calendar (already an example of two PIM applications
;; working together), the BBDB, timeclock, and so on.  Somehow it
;; would be nice to gather these together in a single package.  Of
;; course already there is quite a lot of glue between these provided
;; by Planner; that's the purpose of, for example, remember with its
;; annotation functions.  But it seems to me that the connections
;; could be even tighter.  One sense in which the connections could be
;; tighter is the frame and window code; it would be great if,
;; somehow, Planner were the uber-application in its own frame, and it
;; had some "servant" frames.
;; 
;;
;; I'd love to hear how you've used planner-frame.el, and how you
;; think it can be improved!  My email address is at the beginning of
;; the file.


(require 'planner)

;;; User Variables

(defcustom planner-frame-parameters nil
  "Frame parameters used by `plan-other-frame' to create a Planner frame.
This should be an alist for Emacs, or a plist for XEmacs."
  :group 'planner
  :type (if (featurep 'xemacs)
	    '(repeat (list :inline t :format "%v"
			   (symbol :tag "Property")
			   (sexp :tag "Value")))
	  '(repeat (cons :format "%v"
			 (symbol :tag "Parameter")
			 (sexp :tag "Value")))))

(defcustom planner-exit-planner-hook nil
  "Hook called when exiting Planner."
  :group 'planner
  :type 'hook)

(defcustom planner-suspend-planner-hook nil
  "Hook called when suspending Planner."
  :group 'planner
  :type 'hook)

;;; Default Keybindings

(define-key planner-mode-map "\C-cz" 'planner-suspend-planner)
(define-key planner-mode-map "\C-cx" 'planner-exit-planner)

;;; Internal Functions

(defun planner-select-frame-set-input-focus (frame)
  "Select `planner-frame', raise it, and set input focus, if possible."
  ;;; taken verbatim from `gnus-select-frame-set-input-focus'
  (cond ((featurep 'xemacs)
	 (raise-frame frame)
	 (select-frame frame)
	 (focus-frame frame))
	;; The function `select-frame-set-input-focus' won't set
	;; the input focus under Emacs 21.2 and X window system.
	;;((fboundp 'select-frame-set-input-focus)
	;; (defalias 'gnus-select-frame-set-input-focus
	;;   'select-frame-set-input-focus)
	;; (select-frame-set-input-focus frame))
	(t
	 (raise-frame frame)
	 (select-frame frame)
	 (cond ((and (eq window-system 'x)
		     (fboundp 'x-focus-frame))
		(x-focus-frame frame))
	       ((eq window-system 'w32)
		(w32-focus-frame frame)))
	 (when focus-follows-mouse
	   (set-mouse-position frame
			       (1- (frame-width frame)) 0)))))

(defun planner-frame-or-window-display-name (object)
  "Given a frame or window, return the associated display name.
Return nil otherwise."
  (if (featurep 'xemacs)
      (device-connection (dfw-device object))
    (if (or (framep object)
	    (and (windowp object)
		 (setq object (window-frame object))))
	(let ((display (frame-parameter object 'display)))
	  (if (and (stringp display)
		   ;; Exclude invalid display names.
		   (string-match "\\`[^:]*:[0-9]+\\(\\.[0-9]+\\)?\\'"
				 display))
	      display)))))

(defun planner-buffer-p (buf)
  (with-current-buffer buf
    (string-match mode-name "planner")))

(defun planner-alive-p ()
  "Determine whether Planner is running" 
  ;; planner is running iff there is a buffer anywhere whose mode is
  ;; `planner-mode'.
  (let ((buf-list (buffer-list))
	(found nil))
    (while (and buf-list (not found))
      (let ((buf (car buf-list)))
	(when (planner-buffer-p buf)
	  (setq found t)))
      (setq buf-list (cdr buf-list)))
    found))

;;; Internal Variables

(defvar planner-most-recent-buffer nil
  "The most recent planner buffer that has been visited.")

(defvar planner-frame nil
  "A frame object which will be created by `plan-other-frame'.")

;;; Public Functions

;;;###autoload
(defun plan-other-frame (&optional force-days display)
  "Like `plan', but does so in a new frame.

This will call `plan' and pop up a Planner frame; the value of
FORCE-DAYS is passed to `plan'.  The optional second argument
DISPLAY should be a standard display string such as \"unix:0\" to
specify where to pop up a frame.  If DISPLAY is omitted or the
function `make-frame-on-display' is not available, the current
display is used."
;; Docstring taken from docstring of `gnus-other-frame'.
  (interactive "P")
  (if (fboundp 'make-frame-on-display)
      (unless display
	(setq display (planner-frame-or-window-display-name (selected-frame))))
    (setq display nil))
  (let ((alive (planner-alive-p)))
    (unless (and alive
		 (catch 'found
		   (walk-windows
		    (lambda (window)
		      (when
			  (and
			   (or (not display)
			       (equal display
				      (planner-frame-or-window-display-name
				       window)))
			   (with-current-buffer (window-buffer window)
			     (string-match "\\`planner-"
					   (symbol-name major-mode))))
			(planner-select-frame-set-input-focus
			 (setq planner-frame (window-frame window)))
			(select-window window)
			(throw 'found t)))
		    'ignore t)))
      (planner-select-frame-set-input-focus
       (setq planner-frame
	     (if display
		 (make-frame-on-display display planner-frame-parameters)
	       (make-frame planner-frame-parameters))))
      (if alive
	  (switch-to-buffer planner-most-recent-buffer)
	(plan force-days)))))

;;;###autoload
(defun planner-exit-planner ()
  "Save and then close all Planner buffers, delete the frame that
Planner created, and run the hook `planner-exit-planner-hook'."
  (interactive)
  (planner-save-buffers)
  (when (and (frame-live-p planner-frame)
	     (cdr (frame-list)))
    (delete-frame planner-frame))
  (dolist (buf (buffer-list))
    (when (planner-buffer-p buf)
      (kill-buffer buf)))
  (setq planner-frame nil)
  (run-hooks 'planner-exit-planner-hook))

;;;###autoload
(defun planner-suspend-planner ()
  "Delete the frame that was created for the Planner, then run the
hook `planner-suspend-planner-hook'."
  (interactive)
  (when (and (frame-live-p planner-frame)
	     (cdr (frame-list)))
    (delete-frame planner-frame))
  (run-hooks 'planner-suspend-planner-hook))

;;; Export Statements
(provide 'planner-frame)

;;; planner-frame.el ends here

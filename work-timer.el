;;; work-timer.el --- A timer so you know when telax, and when to work
(defconst work-timer-version "0.3")
;; Copyright (c)2008 Jonathan Arkell. (by)(nc)(sa)  Some rights reserved.
;; Author: Jonathan Arkell <jonnay@jonnay.net>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:
(defgroup work-timer '()
  "A simple package to help deal with procrastination.

This package makes use of the (10+2)*5 procrastination hack by
Merlin Mann.  Essentially you work for 10 minutes on a task, then
you switch to 2 minutes of procrastination, with the idea that by
having a scheduled procrastination time, you will be lass likely
to actually procrastinate.

For a more detailed explination see:
http://www.43folders.com/2005/10/11/procrastination-hack-1025")
  
;;; Installation:
;; make sure you have the todochiku package installed.  You can get it
;; from the Emacs wiki:
;; http://www.emacswiki.org/emacs/download/todochiku.el
;; or M-x auto-install-from-emacswiki RET todochiku.el RET
;; Todochiku provides the alert mechanism to tell you when you should
;; get back to work, and when you should slack.  It should work
;; in any environment, any configuration.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `work-timer-start'
;;    Start the work timer.
;;  `work-timer-stop'
;;    Stop the work timer.
;;  `work-timer-reset'
;;    Reset the work timer in relax mode.
;;  `work-timer-reset-work'
;;    Reset the work timer in work mode.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `work-timer-working-time'
;;    Time fragment that you should spend working (in minutes).
;;    default = 10
;;  `work-timer-relax-time'
;;    Time fragment that you should spend relaxing (in minutes).
;;    default = 2
;;  `work-timer-work-hook'
;;    Hook that is run when it is time to work.
;;    default = nil
;;  `work-timer-relax-hook'
;;    Hook that is run when it is time to relax.
;;    default = nil

;; Put work-timer.el somewhere in your load-path.
;; (Use M-x show-variable RET load-path to see what your load path is.)
;; Add this to your emacs init file.
;(require 'work-timer)


;;; interactive commands:
;; work-timer-start - Start the work timer
;; work-timer-reset - reset the timer in relax mode
;; work-timer-reset-work - reset the work timer in work mode
;; work-timer-stop - stop the work timer

;;; elisp:
;; 

;;; CHANGELOG:
;; v 0.3 - Un-stupified the code.  (decoupled interactive functions, etc.)
;;       - Ran checkdoc.
;; v 0.2 - Added status variable.  (this makes it easy to hook into jiseki)
;;       - Fix bug in work-timer-stop (Thanks Scott Jaderholm)
;; v 0.1 - Initial release

;;; Code:
(defcustom work-timer-working-time 10
  "Time fragment that you should spend working (in minutes)."
  :type 'float
  :group 'work-timer)

(defcustom work-timer-relax-time 2
  "Time fragment that you should spend relaxing (in minutes)."
  :type 'float
  :group 'work-timer)

(defcustom work-timer-work-hook nil
   "Hook that is run when it is time to work."
   :type 'hook
   :group 'work-timer)

(defcustom work-timer-relax-hook nil
   "Hook that is run when it is time to relax."
   :type 'hook
   :group 'work-timer)

(defvar work-timer-timer nil
  "Actual timer for work-timer.")

(defvar work-timer-state 'not-started
  "Shows the actual state of the worktimer.
Can be one of the following symbols:
  not-started
  work-time
  relax-time")

;;* timer  relax
(defun work-timer--relax ()
  "Internal function to set the timer mode to relax."
  (when (timerp work-timer-timer)
		(cancel-timer work-timer-timer))
  (setq work-timer-state 'relax-time)

  (run-hooks 'work-timer-relax-hook)
  (todochiku-message "Work Timer" "Time to chill." (todochiku-icon 'star))

  (setq work-timer-timer (run-with-timer (* 60 work-timer-relax-time) nil 'work-timer--work)))

;;* timer work
(defun work-timer--work ()
  "Internal function to set the timer mode to work."
  (when (timerp work-timer-timer)
		(cancel-timer work-timer-timer))
  (setq work-timer-state 'work-time)
  
  (run-hooks 'work-timer-work-hook)
  (todochiku-message "Work Timer" "Time to work." (todochiku-icon 'alarm))
  
  (setq work-timer-work (run-with-timer (* 60 work-timer-working-time) nil 'work-timer--relax)))

;;* interactive work
(defun work-timer-start ()
   "Start the work timer.

This will call todochiku to tell  you to work, and then run `work-timer-work-hook'."
   (interactive)
   (work-timer--work)
   "Work Timer Started")

;;* interactive cancel
(defun work-timer-stop ()
  "Stop the work timer.

This will call todochiku to tell you to chill the heck out, and run
`work-timer-relax-hook'."
  (interactive)
  (when (not (null work-timer-timer))
		(cancel-timer work-timer-timer))
  (todochiku-message "Work Timer" "Work Timer Cancelled." (todochiku-icon 'alert))
  (setq work-timer-state 'not-started)
  "Work Timer Cancelled.")

;;* interactive cancel relax
(defun work-timer-reset ()
   "Reset the work timer in relax mode."
   (interactive)
   (work-timer--relax)
   "Reset to Relax Mode")

;;* interactive cancel work
(defun work-timer-reset-work ()
  "Reset the work timer in work mode."
  (interactive)
  (work-timer--work)
  "Reset to Work Mode")

;;* test
(when nil
	  "Testing"
	  (progn
	    "Setup for easy testing."
		(setq todochiku-message-too t)
		(setq work-timer-working-time 0.0625)
		(setq work-timer-relax-time 0.125))
	  (work-timer-start)
	  (progn
	   "Undo testing."
	   (setq todochiku-message-too nil)
	   (setq work-timer-working-time 10)
	   (setq work-timer-relax-time 2)
	   (work-timer-stop))
	  )

(provide 'work-timer)

;;; work-timer.el ends here

;;; keyboard-macro-timer.el --- Run last keyboard macro with a timer

;; Copyright (C) 2007 Mathias Dahl
;;
;; Version: 0.1
;; Keywords: convenience, keyboard macros, timers
;; Author: Mathias Dahl <[EMAIL PROTECTED]>

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;
;; Sometimes I record a small keyboard macro just to run some simple
;; command over and over again, each X number of seconds, often to
;; execute some SQL query or running some other command to monitor
;; some process.  It is easy enough to execute the macro with some
;; specially bound key, but why not letting Emacs do it for me
;; instead?  That's why I made this small hack.  Another example of
;; usage could be to have it act as a simple document reader; just
;; record a keyboard macro to step one line down, and then start the
;; timer.
;;
;; To install it, put the file in your load path and add these lines
;; to your .emacs:
;;
;;   (autoload 'keyboard-macro-timer-start "keyboard-macro-timer"
;;   "Execute last keyboard macro with a timer." t)
;;
;; To use it, call `keyboard-macro-timer-start' after you have
;; recorded a keyboard macro.  You will be prompted for a number of
;; seconds to wait before executing the macro the first time and for
;; the interval, in seconds, between invocations.  Both values may be
;; integers or floating point numbers so you can specify fractions of
;; a second and both have default values
;; (`keyboard-macro-timer-default-start-in' and
;; `keyboard-macro-timer-default-interval') so that you can just type
;; `RET' to accept the defaults.  The default values are configurable.
;;
;; To stop the execution, call `keyboard-macro-timer-cancel'.
;;
;; Make it really easy to stop the timer by binding the cancel command
;; to a key:
;;
;;  (global-set-key [f6] 'keyboard-macro-timer-cancel)
;;
;; Try it now: type `C-x (' to start a keyboard macro, then type `C-f'
;; to step one character forward.  Stop macro recording with `C-x
;; )'. Next, do M-x keyboard-macro-timer-start and type `RET' at both
;; prompts.  Stop it the timer by running
;; `keyboard-macro-timer-cancel'.
;;
;; Only tested on Emacs 22.1.
;;

;;; History:
;;
;; Version 0.1, 2007-07-19
;;
;; * First release.
;;

;;; Code:

(defvar keyboard-macro-timer nil
  "Timer for running keyboard macro.")

(defcustom keyboard-macro-timer-default-start-in 2
  "Default number of seconds for the START-IN argument."
  :type 'number
  :group 'keyboard-macro-timer)

(defcustom keyboard-macro-timer-default-interval 2
  "Default number of seconds for the INTERVAL argument."
  :type 'number
  :group 'keyboard-macro-timer)

(defun keyboard-macro-timer-start (start-in interval)
  "Execute last keyboard macro with a timer.
START-IN defines the number of seconds until the macro is first
run, and INTERVAL defines the number of seconds between each
call.  Both arguments may be integers or floating point numbers
so you can specify fractions of a second."
  (interactive (let ((start-in (read-number
                                "Start in: "
                                keyboard-macro-timer-default-start-in))
                     (interval (read-number
                                "Interval: "
                                keyboard-macro-timer-default-interval)))
                 (list start-in interval)))
  (setq keyboard-macro-timer
        (run-with-timer
         start-in interval
         (lambda ()
           (kmacro-end-or-call-macro 1)))))

(defun keyboard-macro-timer-cancel ()
  "Cancel execution of keyboard macro timer."
  (interactive)
  (cancel-timer keyboard-macro-timer))

(provide 'keyboard-macro-timer)

;;; keyboard-macro-timer.el ends here

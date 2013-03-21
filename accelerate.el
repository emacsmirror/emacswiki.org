;;; accelerate.el --- pump numeric arg for auto-repeated interactive commands
;; ---------------------------------------------------------------------------
;;
;; Copyright (C) 2006, David Andersson
;;
;; This file is NOT part of Emacs.
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
;; Created: 2006-06-30
;; Version: 0.2
;;
;;; Commentary:
;;
;; When moving the cursor (with e.g. the arrow keys or C-n, C-p, etc) and the
;; key is held down and auto-repeated, then start moving cursor in increasingly
;; larger steps.
;;
;; Any command that takes a numeric argument as its first arg (specified
;; by e.g. (interactive "p")) can be accelerated. But it is mostly only useful
;; to accelerate cursor movement commands that normally move cursor a short
;; distance.
;;
;; The feature is disabled when recording (and executing) keyboard macros,
;; because timing information is not stored in macros.
;;
;; Example
;;
;;  (require 'accelerate)
;;  (accelerate previous-line 2)
;;  (accelerate next-line 2)
;;  (accelerate backward-char 3)
;;  (accelerate forward-char 3)
;;  (accelerate dired-previous-line 2)
;;  (accelerate dired-next-line 2)
;;  (accelerate speedbar-prev 2)
;;  (accelerate speedbar-next 2)
;;
;; (Whether the command being accelerated is defined yet or not does not matter.
;;  E.g. when accelerating dired-next-line, (require 'dired) is not needed.)
;;
;; TODO: Problem accelerating backward-char and forward-char; defadvice does not
;;       take effect. (Is it because they are subs, and not functions?)

(require 'advice)

(defvar acc-auto-repeat-time 100000
  "Interval in microseconds to detect keyboard auto-repeat.
\nIf an interactive command is repeated within this time, and invoked with
the same keyboard key, it is considered to be auto-repeated.")

(defvar acc--last-time (current-time))
(defvar acc--next-multiplier '(1))
(defvar acc--last-command-event nil)

(defun acc-time-diff (time1 time2)
  "Difference between TIME1 and TIME2 in microseconds.
Assume TIME1 is before or equal to TIME2.
Return 1000000 if diff is larger than one second.
See `current-time' for time format."
  (let ((hi (- (car time2) (car time1)))
	(s (- (nth 1 time2) (nth 1 time1)))
	(us (- (nth 2 time2) (nth 2 time1))))
    (cond ((/= hi 0)    1000000)
	  ((=  s 1)     (+ 1000000 us))
	  ((/= s 0)     1000000)
	  (t            us))
    ))

;; `accelerate' is a macro. It could have most of the logic in
;; `acc-remove-advice', `acc-save-mult' and `acc-pump-arg', but
;; placing them in separate functions that aren't macros will allow
;; the expansion of `accelerate' to be smaller. It also enhances
;; readability, just slightly.

(defun acc-remove-advice (funct class name)
  ;; Remove a specific piece of advice.
  ;; Basically the reverse of `defadvice'.
  (if (ad-find-advice funct 'before 'accelerate)
      (progn
	(ad-remove-advice funct 'before 'accelerate)
	(ad-activate-on funct)
	nil)))

(defun acc-save-mult (multiplier symb)
  ;; Normalize MULTIPLIER, store it in a property of SYMB, and return it.
  ;; If MULTIPLIER is a number, normalize it by putting it in a list, otherwise
  ;; return it as it is. `nil' is returned as it is too.
  (if (numberp multiplier) (setq multiplier (list multiplier)))
  (put symb 'accelerate multiplier)
  multiplier)

(defun acc-pump-arg (arg0 symb)
  ;; Given an argument value ARG0 assumed to be the first arg of an advised
  ;; command, compute and return a replacement value for that arg.
  ;; If it is concluded that this is not an auto-repeated invocation of
  ;; the advised command, ARG0 is returned unchanged.
  ;; SYMB is the command symbol, which is used to get the multiplier list
  ;; stored in a property on that symbol.
  ;; Variables `acc--last-command-event', `acc--last-time' and/or
  ;; `acc--next-multiplier' are updated.
  (if (and (eq last-command-event acc--last-command-event)
	   (not defining-kbd-macro)
	   (not executing-kbd-macro)
	   (eq arg0 1))
      (progn
	(let ((curr (current-time)))
	  (if (< (acc-time-diff acc--last-time curr) acc-auto-repeat-time)
	      (setq arg0 (car acc--next-multiplier)
		    acc--next-multiplier (or (cdr acc--next-multiplier)
					     acc--next-multiplier))
	    ;; else  too long since last time
	    (setq acc--next-multiplier (get symb 'accelerate)))
	  (setq acc--last-time curr)))
    ;; else  temporary disabled
    (setq acc--last-command-event last-command-event))
  arg0)

;;;###autoload
(defmacro accelerate (command multiplier)
  "Advise COMMAND so its numeric argument is increased when repeated quickly.
\nCOMMAND should be a symbol name of an interactive command where the first arg
is 1 by default. Normally that is a function declared with \(interactive \"p\").
COMMAND should not be quoted since this is a macro.
\nMULTIPLIER is a number \(or a list of numbers\) to become the first arg of the
command when the command is repeated quickly, or `nil' to remove acceleration.
If MULTIPLIER is a list of numbers, each consecutive repeated invocation of the
command will use the next number in the list. If the end of the list is reached
the last number is used again in further repeated invocations.
\nAlso see variable `acc-auto-repeat-time'."
  `(if (acc-save-mult ,multiplier ',command)
       (defadvice ,command (before accelerate activate)
	 "Accelerated when auto-repeated. See `accelerate'"
	 (if (interactive-p)
	     (ad-set-arg 0 (acc-pump-arg (ad-get-arg 0) ',command))))
     ;; else
     (acc-remove-advice ',command 'before 'accelerate)))

(provide 'accelerate)

;;; task-timer.el --- Simple task timer package.

;; Author: Jeremy Cowgar <jeremy@cowgar.com>
;; Keywords: timer task-timer

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package allows you to time an activity. Start the timer then
;; when stoppig the timer it will report the amount of time elapsed

;;; Requirements:

;; Only tested on Emacs 23

;;; Installation:

;; Add below code in your .emacs
;;
;; (require 'task-timer)

;;; Usage:
;;
;; 1. Interactively call (task-timer-start) to start the timer
;; 2. Interactively call (task-timer-stop) to stop the timer.
;;    Upon stopping, the duration will be displayed in the status bar
;;
;; Suggested binding:
;; (define-key global-map "\C-ctb" 'task-timer-begin)
;; (define-key global-map "\C-cts" 'task-timer-status)
;;

(defun task-timer-begin ()
  (interactive)
  (setq task-timer-started (current-time)))

(defun task-timer-status ()
  (interactive)
  (let* ((remaining-time (decode-time (time-subtract (current-time) task-timer-started)))
	 (remaining-seconds (nth 0 remaining-time))
	 (remaining-minutes (nth 1 remaining-time)))
    (message "%d min %d sec" remaining-minutes remaining-seconds)))

(provide 'task-timer)
;;; task-timer ends here

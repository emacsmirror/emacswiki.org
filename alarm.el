;;; alarm.el --- Alarm

;; Author: Emacs guys <emacser@freedom.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Emacs guys, all rights reserved.
;; Created: 2008-06-17 09:04:54
;; Version: 1.0
;; Last-Updated: 2008-06-17 09:04:57
;; URL:  http://www.emacswiki.org/emacs/download/alarm.el
;; Keywords: Alarm
;; Compatibility: GNU Emacs 23.0.60.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Features that might be required by this library:
;;
;; Popup notify for alarm.
;;
;; This extension use `notify-send' for notify.
;; So make you have install `notify-send' in your system.
;;

;;; Installation:
;;
;; Copy alarm.el to your load-path and add to your ~/.emacs
;;
;;  (require 'alarm)
;;
;; No need more

;;; Commentary:
;;
;; Alarm.
;;

;;; Change log:
;;
;; 2008/06/17
;;         First release.
;;

;;; Acknowledgments:
;;
;;  Emacs guys.
;;

;;; TODO
;;
;; None
;;

;;; Require

;;; Code:

(defvar alarm-clock-timer nil
  "Keep timer so that the user can cancel the alarm.")

(defun alarm-clock-message (text)
  "The actual alarm action.
Argument TEXT alarm message."
  (shell-command (format "notify-send -t 10000 -- \"%s\" " text)))
(defun alarm-clock ()
  "Set an alarm.
The time format is the same accepted by `run-at-time'.  For
example \"11:30am\"."
  (interactive)
  (let ((time (read-string "Time(example, 11:30am): "))
        (text (read-string "Message: ")))
    (setq alarm-clock-timer (run-at-time time nil 'alarm-clock-message text))))
(defun alarm-clock-cancel ()
  "Cancel the alarm clock."
  (interactive)
  (cancel-timer alarm-clock-timer))

(provide 'alarm)

;;; alarm.el ends here

;;; LocalWords:  el

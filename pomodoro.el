;;; pomodoro.el --- Pomodoro Technique for emacs

;; Copyright (C) 2011 Ivan Kanis
;; Author: Ivan Kanis
;; URL: http://ivan.kanis.fr/pomodoro.el
;; Version: 0.1
;;
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; The technique is described in http://www.pomodorotechnique.com
;;
;; to start the pomodoro you issue the following command:
;;
;; M-x pomodoro
;;
;; in the modeline you will see the following indicator W1-25. This
;; means that you are working on set 1 and that you have 25 minutes
;; remaining. The counter will decrease each minutes. When it reaches
;; 0 you will get a message in a buffer that it's time to take a
;; break. The modeline will display B1-5, that is you have a break of
;; 5 minutes. When the count reaches 0 you will another message to get
;; back to work and the set number will increase. At the end of the
;; 4th set you will get a long break. The modeline will display LB
;; instead of B.
;;
;; When you don't need the pomodoro anymore you do:
;;
;; M-x pomodoro-stop
;;
;; I you got interrupted and you want to rewind the pomodoro on the
;; current set just do:
;;
;; M-x pomodoro-rewind
;;
;; Calling M-x pomodoro again will reset it to the first working set
;;
;; You can customize this mode with the following variables:
;;  - `pomodoro-work-time' number of minutes of working
;;  - `pomodoro-short-break' number of minutes of a short break
;;  - `pomodoro-long-break' number of minutes of a long break
;;  - `pomodoro-set-number' number of sets until a long break
;;  - `pomodoro-set-number' the minutes of a working set with

;;; THANKS:

;; Obviously Francesco Cirillo for creating the technique. I was
;; inspired by a pomodoro timer for Windows but I can't find out who
;; wrote it...
;; Richard Riley for pointing out I forgot provide
;; Manan Tuli for fixing the modeline and adding a hook

;;; BUGS:

;; Are you kidding me? This software is perfect ;)

;;; INSTALLATION:

;; To activate this package simply put this file in your load path.
;; For example if you put the file is in the directory ~/tmp you need
;; to do the following :
;;
;; (add-to-list 'load-path "~/tmp")
;; (require 'pomodoro)
;;

;;; Code:

(defvar pomodoro-work-time 25
  "Time in minutes of work")

(defvar pomodoro-short-break 5
  "Time in minute of short break")

(defvar pomodoro-long-break 15
  "Time in minute of long break")

(defvar pomodoro-set-number 4
  "Number of sets until a long break")

(defvar pomodoro-buffer-name "*pomodoro*"
  "Name of the pomodoro buffer")

(defvar pomodoro-raise-frame t
  "When t raise frame on pomodoro notification")

(defvar pomodoro-message-hook nil
  "Hook run on pomodoro notification.
The function take one argument that is the message to be
diplayed")

(defvar pomodoro-work-format " W%d-%d "
  "String displayed in the modeline when working.")

(defvar pomodoro-break-format " B%d-%d "
  "String displayed in the modeline for a break.")

(defvar pomodoro-long-break-format " LB-%d "
  "String displayed in the modeline for a long break.")

(defvar pomodoro-display-string "")
(defvar pomodoro-minute)
(defvar pomodoro-set)
(defvar pomodoro-timer nil)
(defvar pomodoro-state 'work)

;;;###autoload
(defun pomodoro ()
  "Start pomodoro, also rewind pomodoro to first set."
  (interactive)
  (when pomodoro-timer
    (cancel-timer pomodoro-timer))
  (or global-mode-string (setq global-mode-string '("")))
  (or (memq 'pomodoro-display-string global-mode-string)
      (setq global-mode-string
            (append global-mode-string '(pomodoro-display-string))))
  (setq pomodoro-minute pomodoro-work-time
        pomodoro-set 1
        pomodoro-state 'work
        pomodoro-timer (run-at-time t 60 'pomodoro-timer))
  (pomodoro-update-modeline))

;;;###autoload
(defun pomodoro-rewind ()
  "Rewind pomodoro, keep current set"
  (interactive)
  (setq pomodoro-minute pomodoro-work-time
        pomodoro-state 'work)
  (pomodoro-update-modeline))

;;;###autoload
(defun pomodoro-stop ()
  "Stop pomodoro."
  (interactive)
  (when pomodoro-timer
    (cancel-timer pomodoro-timer))
  (setq pomodoro-display-string "")
  (when (get-buffer pomodoro-buffer-name)
    (kill-buffer "*pomodoro*")))

(defun pomodoro-timer ()
  "Function called every minute.
It takes care of updating the modeline as well a message buffer"
  (setq pomodoro-minute (- pomodoro-minute 1))
  (when (<= pomodoro-minute 0)
    (cond ((eq pomodoro-state 'long-break)
           (setq pomodoro-state 'work
                 pomodoro-minute pomodoro-work-time)
           (pomodoro-message "Work"))
          ((eq pomodoro-state 'short-break)
           (setq pomodoro-state 'work
                 pomodoro-minute pomodoro-work-time)
           (setq pomodoro-set (+ pomodoro-set 1))
           (pomodoro-message "Work"))
          ((eq pomodoro-state 'work)
           (if (>= pomodoro-set pomodoro-set-number)
               (progn
                 (setq pomodoro-minute pomodoro-long-break
                       pomodoro-state 'long-break
                       pomodoro-set 1)
                 (pomodoro-message "Long break"))
             (setq pomodoro-minute pomodoro-short-break
                   pomodoro-state 'short-break)
             (pomodoro-message "Short break")))))
  (pomodoro-update-modeline))

(defun pomodoro-update-modeline ()
  "Update the modeline."
  (setq pomodoro-display-string
        (cond ((eq pomodoro-state 'work)
               (format pomodoro-work-format
                       pomodoro-set pomodoro-minute))
              ((eq pomodoro-state 'short-break)
               (format pomodoro-break-format
                       pomodoro-set pomodoro-minute))
              (t
               (format pomodoro-long-break-format
                       pomodoro-minute))))
  (force-mode-line-update))

(defun pomodoro-message (msg)
  "Display a message in a buffer and maybe raise emacs frame."
  (when pomodoro-raise-frame
    (raise-frame (selected-frame)))
  (let ((this-window (selected-window)))
    (with-current-buffer (get-buffer-create pomodoro-buffer-name)
      (erase-buffer)
      (insert msg))
    (pop-to-buffer pomodoro-buffer-name)
    (fit-window-to-buffer)
    (select-window this-window))
  (run-hook-with-args 'pomodoro-message-hook msg))

(provide 'pomodoro)

;; Local Variables:
;; compile-command: "make"
;; End:

;;; pomodoro.el ends here

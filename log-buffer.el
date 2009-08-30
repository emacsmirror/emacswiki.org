;;; log-buffer.el --- Log buffer utils
;;
;; Author:   Cedric Lallain <kandjar76@hotmail.com>
;; Version:  1.0
;; Keywords: log
;; Description: Log utils.
;; Tested with: GNU Emacs 21.x and GNU Emacs 22.x
;;
;; This file is *NOT* part of GNU Emacs.
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The purpose of this file is just to help the logging of information.
;; The first feature of a log-window is: read-only. 
;; The second is: if the cursor is at the bottom of the text, the windows
;; scrolls automatically.
;;
;; . make-new-log-buffer: create a read-only buffer to log the code
;; . log-printf:          do a c-style printf in the log buffer
;;                        also scroll the buffer to the bottom if the cursor
;;                        was at the end of it.
;;
;;
;; e.g:
;;
;;   (make-new-log-buffer "*test*")
;;   (log-printf "*test*" "this is a test!!!\n")
;;   (log-printf "*test*" "a text of %s and number like %i\n" "string" 12)
;;   (log-printf "*test*" "Test\nof\nmultiple\nlines\nof\ntext\ndisplayed\nat\nonce..\nto\nsee\nif\nthe\nscrolling\nis\nstill\nok\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-new-log-buffer (buffer-name)
  "Create a new log buffer, called BUFFER-NAME.
If BUFFER-NAME already exist, the function will just set the read-only flag.
The log buffer is returned as a result of this call."
  (let ((buffer (get-buffer-create buffer-name)))
    (save-excursion (set-buffer buffer)
		    (toggle-read-only 1)
		    buffer)))
    
(defun log-printf(buffer format-string &rest args)
  "Display the text in the log buffer at the very end of it."
  (let ((inhibit-read-only t)
	(auto-window-vscroll t)
	(user-buffer (current-buffer))
	(window (get-buffer-window buffer))
	(user-window (selected-window)))
    (set-buffer buffer)
    (let* ((current-point (point))
	   (current-bot (= current-point (point-max))))
      (goto-char (point-max))
      (insert-string (apply 'format (cons format-string args)))
      (if (eq user-window window)
	  (setq user-window nil))
      (if current-bot
	  (progn (if window 
		     (progn (select-window window)
			    (goto-char (point-max))
			    (recenter (- (window-body-height window) 3))) ;; mode line size is assumed to be 2
		     (goto-char (point-max)))
		 (if user-window (select-window user-window)))
	  (goto-char current-point)))))

(provide 'log-buffer)


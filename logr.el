;;; logr.el Simple logging tool

;;
;; logr.el
;; Author: Jason Meade <jemeade@gmail.com>
;; Last Edited: 5/27/2024
;; Version: 1.0.0
;;
;; Allow users to log tasks
;;
;; Installation:
;;    Save logr.el wherever you keep your elisp files.
;;    For example: ~/.emacs.d/elisp/logr.el
;;
;;    Optionally byte compile.
;;    For example: M-x byte-compile-file<ret> logr.el<ret>
;;
;;    You may need to update your .emacs file with the above location.
;;    For example: (add-to-list 'load-path "~/.emacs.d/elisp/")
;;
;;    Add the following to your .emacs file:
;;    (require 'logr)
;;
;; Usage:
;;    M-x logr<ret> Enter your one-line log message<ret>
;;
;; The logr feature is intended to for users to write simple
;; one-line log entries. For example, this could be useful
;; as a timesheet app or for jotting down quick notes while
;; working.
;;
;;
;;    Copyright (C) 2024  Jason Meade <jemeade@gmail.com>
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Code:

(defvar logr-file "~/.logfile"
  "Save log entries to file")

(defun logr ()
  "Usage: M-x logr<ret> Enter your one-line log message<ret>"
  (interactive
   (let* ((log-message (read-from-minibuffer "Log: "))
          (log-entry (format "%s :: %s\n" (logr-time) log-message))
          (log-buffer (find-file-noselect logr-file)))
     (progn
       (set-buffer log-buffer)
       (goto-char (point-max))
       (insert log-entry)
       (save-buffer 0)
       (kill-buffer)
       nil))))

(defun logr-time ()
  "Returns current time in MM/DD/YYYY MM:HH format"
  (let ((log-time (decode-time (current-time))))
    (format "%02d/%02d/%04d %02d:%02d"
            (car (cddddr log-time))
            (cadddr log-time)
            (cadr (cddddr log-time))
            (caddr log-time)
            (cadr log-time))))

(provide 'logr)

;;; logr.el ends here

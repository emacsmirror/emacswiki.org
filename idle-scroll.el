;;; idle-scroll.el --- scroll down a line when Emacs is idle

;; Copyright (C) 2003  Alex Schroeder <alex@gnu.org>
;; Author: Alex Schroeder <alex@gnu.org>

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

;; Run M-x idle-scroll-mode to automatically scroll down.

(defvar idle-scroll-timer nil
  "Timer for `idle-scroll-mode'.")

(defvar idle-scroll-interval 1000
  "*How many milliseconds to wait before scrolling down one line.
Should be a positive integer.")

(defvar idle-scroll-amount 50
  "*How many milliseconds to change each time")

(defvar idle-scroll-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "+" 'idle-scroll-faster)
    (define-key map "-" 'idle-scroll-slower)
    (define-key map "q" 'idle-scroll-stop)
    map)
  "Key map of `idle-scroll-mode'")

(make-variable-buffer-local 'idle-scroll-timer)
(make-variable-buffer-local 'idle-scroll-interval)

(define-minor-mode idle-scroll-mode
  "Scroll down line by line when idle.

\\{idle-scroll-mode-map}"
  :lighter " Scrl"
  :keymap idle-scroll-mode-map
  (and idle-scroll-timer
       (cancel-timer idle-scroll-timer))
  (when idle-scroll-mode
    (if (< idle-scroll-interval 0)
        (setq idle-scroll-interval
              (default-value 'idle-scroll-interval)))
    (setq idle-scroll-timer
          (run-at-time t (/ idle-scroll-interval 1000.0)
                       'idle-scroll-scroll (current-buffer)))))

(defun idle-scroll-faster (arg)
  (interactive "p")
  (setq idle-scroll-interval (- idle-scroll-interval (* arg idle-scroll-amount)))
  (if (< idle-scroll-interval 0)
      (message "Can't be faster!")
    (aset idle-scroll-timer 4 (/ idle-scroll-interval 1000.0))
    (message "Scroll at %.2f seconds." (/ idle-scroll-interval 1000.0))))

(defun idle-scroll-slower (arg)
  (interactive "p")
  (idle-scroll-faster (- arg)))

(defun idle-scroll-stop ()
  (interactive)
  (idle-scroll-mode -1))

(defun idle-scroll-scroll (buf)
  "Scroll if `idle-scroll-mode' is active."
  (when (eq (current-buffer) buf)
    (condition-case nil
        (scroll-up 1)
      (error (idle-scroll-mode -1)))))

(provide 'idle-scroll)
;;; idle-scroll.el ends here

;;; auto-scroll.el --- Automatically scroll down line.

;; Filename: auto-scroll.el
;; Description:  Scroll down line when Emacs is auto
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-26 17:15:45
;; Version: 0.1
;; Last-Updated: 2008-10-26 17:15:49
;;           By: Andy Stewart
;; URL:
;; Keywords: auto, scroll
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
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

;;; Commentary:
;;
;;  This package just copy from `idle-scroll.el' and with slightly modified
;;  for use `DocView' mode's remap keystroke.
;;  Thanks Alex Schroeder.
;;
;;  Automatically scroll down line.
;;  You can use function `auto-scroll-mode' to auto scroll current buffer.
;;  And use `auto-scroll-mode' again can stop scroll.
;;
;;  Use function `auto-scroll-faster' can make buffer scroll faster.
;;  Use function `auto-scroll-slower' can make buffer scroll slower.
;;

;;; Installation:
;;
;; Put auto-scroll.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'auto-scroll)
;;
;; No need more.

;;; Change log:
;;
;; 2008/10/26
;;      First released.
;;

;;; Acknowledgements:
;;
;;      Alex Schroeder  <alex@gnu.org>  for idle-scroll.el
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defvar auto-scroll-timer nil
  "Timer for `auto-scroll-mode'.")

(defvar auto-scroll-interval 1000
  "*How many milliseconds to wait before scrolling down one line.
Should be a positive integer.")

(defvar auto-scroll-interval-min 10
  "The minimum milliseconds that scrolling down on line.")

(defvar auto-scroll-amount 50
  "*How many milliseconds to change each time")

(make-variable-buffer-local 'auto-scroll-timer)
(make-variable-buffer-local 'auto-scroll-interval)

(define-minor-mode auto-scroll-mode
  "Scroll down line by line when auto.

\\{auto-scroll-mode-map}"
  :lighter " Scrl"
  (and auto-scroll-timer
       (cancel-timer auto-scroll-timer))
  (when auto-scroll-mode
    (if (< auto-scroll-interval 0)
        (setq auto-scroll-interval
              (default-value 'auto-scroll-interval)))
    (setq auto-scroll-timer
          (run-at-time t (/ auto-scroll-interval 1000.0)
                       'auto-scroll-scroll (current-buffer)))))

(defun auto-scroll-faster (arg)
  (interactive "p")
  (setq auto-scroll-interval (- auto-scroll-interval (* arg auto-scroll-amount)))
  (and (< auto-scroll-interval auto-scroll-interval-min)
       (setq auto-scroll-interval auto-scroll-interval-min))
  (aset auto-scroll-timer 4 (/ auto-scroll-interval 1000.0))
  (message "Scroll at %.2f seconds." (/ auto-scroll-interval 1000.0)))

(defun auto-scroll-slower (arg)
  (interactive "p")
  (auto-scroll-faster (- arg)))

(defun auto-scroll-scroll (buf)
  "Scroll if `auto-scroll-mode' is active."
  (when (eq (current-buffer) buf)
    (condition-case nil
        (funcall (auto-scroll-get-function 'scroll-up) 1)
      (error (auto-scroll-mode -1)))))

(defun auto-scroll-get-function (symbol)
  "Return SYMBOL if it's function is not remapped, else return
the remapping function."
  (or (command-remapping symbol)
      symbol))

(provide 'auto-scroll)

;;; auto-scroll.el ends here

;;; LocalWords:  el

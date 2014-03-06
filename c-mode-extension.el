;;; c-mode-extension.el --- Some extension for C mode

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-09-27 00:20:18
;; Version: 0.1
;; Last-Updated: 2008-09-27 00:20:21
;; URL: not distributed yet
;; Keywords: c
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
;;  None
;;

;;; Installation:
;;
;; Copy c-mode-extension.el to your load-path and add to your ~/.emacs
;;
;;  (require 'c-mode-extension)
;;
;; No need more

;;; Commentary:
;;
;; This package have some extensions for C Programming.
;;

;;; Change log:
;;
;; 2008/09/27
;;         First released.
;;

;;; Acknowledgements:
;;
;;      All emacsers...
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defun my-c-previous-line-comment ()
  "Return to previous line comment."
  (interactive)
  (c-hungry-backspace)
  (call-interactively 'comment-dwim))

(defun my-c-function-init-indent()
  "Format parentheses of function in C-mode."
  (interactive)
  (newline-and-indent)
  (call-interactively 'previous-line)
  (end-of-line)
  (newline-and-indent))

(defun my-c-mode-auto-newline-break()
  "Automatic breakout end line and jump right of next brace."
  (interactive)
  (my-to-next-brace-right)
  (backward-char 2)
  (c-hungry-backspace)
  (my-to-next-brace-right))

(defun my-c-mode-auto-newline-break-newline()
  "Automatic breakout end line and newline."
  (interactive)
  (my-c-mode-auto-newline-break)
  (open-newline-below 1))
(defun my-to-next-brace-right()
  "To right of next match parentheses."
  (while (not (looking-at "}"))
    (forward-char 1))
  (forward-char 1))

(provide 'c-mode-extension)

;;; c-mode-extension.el ends here

;;; LocalWords:  el dwim

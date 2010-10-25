;;; show-point-mode.el --- show point position in status bar


;; Copyright (C) 2007, 2010 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2
;; Keywords: point, mode line
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Commentary:
;;
;; A simple minor-mode to display the point in the status bar.


;;; Change log:
;;
;; Version 0.2
;; * rewritten without any need for an idle-timer
;;
;; Version 0.1
;; * initial release



;;; Code:

(provide 'show-point-mode)


;; add point display to mode-line construct
(let ((linenum-format (assq 'line-number-mode mode-line-position)))
  (setq mode-line-position
	(assq-delete-all 'line-number-mode mode-line-position))
  (setq mode-line-position
	(append mode-line-position
		`((show-point-mode
		   (line-number-mode
		    ((column-number-mode
		      (20 (" (%l,%c)" (:eval (format "(%d)" (point)))))
		      (15 (" L%l"     (:eval (format "(%d)" (point)))))))
		    ((column-number-mode
		      (15 (" C%c"     (:eval (format "(%d)" (point)))))
		      (10 (:eval (format "(%d") (point))))))
		   ,linenum-format)))))


(define-minor-mode show-point-mode
  "Toggle show-point mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

Note that simply setting the minor-mode variable
`show-point-mode' is not sufficient to enable show-point-mode
mode.

When enabled, the value of `point' is displayed in the
mode-line (after the line and column numbers, if those are being
displayed too).")


;;; show-point-mode.el ends here

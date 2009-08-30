;;; show-point-mode.el --- show point position in status bar


;; Copyright (C) 2007 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1
;; Keywords: point, mode line


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
;; version 0.1
;; * initial release



;;; Code:

(provide 'show-point-mode)


(defvar show-point-value nil
  "Variable used to store point value for display in mode-line.")
(make-variable-buffer-local 'show-point-value)


;; add point display to mode-line construct
(let ((linenum-format (assq 'line-number-mode mode-line-position)))
  (setq mode-line-position
	(assq-delete-all 'line-number-mode mode-line-position))
  (setq mode-line-position
	(append mode-line-position
		`((show-point-mode
		   (line-number-mode
		    ((column-number-mode
		      (15 " (%l,%c)(" show-point-value ")")
		      (10 " L%l(" show-point-value ")")))
		    ((column-number-mode
		      (10 " C%c(" show-point-value ")"))))
		   ,linenum-format))))
)


(define-minor-mode show-point-mode
  "Toggle show-point mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

Note that simply setting the minor-mode variable
`predictive-which-dict-mode' is not sufficient to enable
predictive mode."

  ;; initial value, mode-line indicator, and keymap
  nil nil nil
  
  ;; if show-point-mode had been turned on, add update function to
  ;; `post-command-hook'
  (if show-point-mode
      (add-hook 'post-command-hook 'show-point-modeline-update nil t)
    (remove-hook 'post-command-hook 'show-point-modeline-update t))
)


(defun show-point-modeline-update nil
  ;; Update variable storing point position for display in mode line.
  (setq show-point-value (format "%d" (point))))


;;; show-point-mode.el ends here

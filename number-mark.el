;;; number-mark.el --- a minor-mode for editing numbers
;;
;; Copyright 2023 Alex Schroeder <alex@gnu.org>
;;
;; Author: Alex Schroeder <alex@gnu.org>
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is for people who need to increase or decrease numbers in text
;; buffers.

;;; Code:

(require 'thingatpt)

(defvar number-digits "0123456789")

(defun number-unmark-all ()
  "Remove all marks."
  (interactive)
  (remove-overlays
   (if (region-active-p) (region-beginning) (point-min))
   (if (region-active-p) (region-end) (point-max))
   'number t))

(defun number-unmark ()
  "Remove the current mark."
  (interactive)
  (dolist (o (overlays-at (point)))
    (when (overlay-get o 'number)
      (delete-overlay o))))

;;;###autoload
(defun number-mark ()
  "Mark number at point for distribution."
  (interactive)
  (skip-chars-backward number-digits)
  (let ((start (point)))
    (skip-chars-forward number-digits)
    (when (= start (point))
      (error "No number at point"))
    (let ((o (make-overlay start (point) nil nil t)))
      (overlay-put o 'number t)
      (overlay-put o 'face 'query-replace))))

(defun number-mark-add (amount)
  "Add a number to all marked numbers."
  (interactive "nAdd how much? ")
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (overlay-get o 'number)
      (let ((num (string-to-number (buffer-substring (overlay-start o) (overlay-end o)))))
	(goto-char (overlay-start o))
	(delete-region (overlay-start o) (overlay-end o))
	(insert (number-to-string (+ num amount)))))))

(defun number-mark-distribute (amount)
  "Distribute a number to all marked numbers."
  (interactive "nDistribute how much? ")
  (let* ((os (overlays-in (point-min) (point-max)))
	 (n (/ amount (length os))))
    (dolist (o os)
      (when (overlay-get o 'number)
	(let ((num (string-to-number (buffer-substring (overlay-start o) (overlay-end o)))))
	  (goto-char (overlay-start o))
	  (delete-region (overlay-start o) (overlay-end o))
	  (insert (number-to-string (+ num n))))))))

(defun number-mark-sum ()
  "Sum all marked numbers."
  (interactive)
  (let ((n 0))
    (dolist (o (overlays-in (point-min) (point-max)))
      (when (overlay-get o 'number)
	(let ((num (string-to-number (buffer-substring (overlay-start o) (overlay-end o)))))
	  (setq n (+ num n)))))
    (message "Sum: %d" n)))

;;;###autoload
(defun number-mark-column (regexp)
  "Mark all the numbers at the current column
in this buffer, if the line matches REGEXP."
  (interactive "sRegexp: ")
  (save-excursion
    (let ((target-column (current-column)))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(when (and (eq (move-to-column target-column) target-column)
		   (not (memq t (mapcar (lambda (o)
					  (when (overlay-get o 'number) t))
					(overlays-at (point))))))
	  (skip-chars-backward number-digits)
	  (let ((start (point)))
	    (skip-chars-forward number-digits)
	    (when (< start (point))
	      (let ((o (make-overlay start (point) nil nil t)))
		(overlay-put o 'number t)
		(overlay-put o 'face 'query-replace)))))
	(forward-line 1)))))

;;;###autoload
(defun number-mark-group (regexp)
  "Mark all the numbers in this buffer matching group 1 in REGEXP"
  (interactive (list (read-string "Regexp with one group matching the number: " "\\([0-9]+\\)")))
  (save-excursion
    (goto-char (if (region-active-p) (region-beginning) (point-min)))
    (while (re-search-forward regexp (if (region-active-p) (region-end)) t)
      (when (not (memq t (mapcar (lambda (o)
				   (when (overlay-get o 'number) t))
				 (overlays-at (match-beginning 1)))))
	(let ((o (make-overlay (match-beginning 1) (match-end 1) nil nil t)))
	  (overlay-put o 'number t)
	  (overlay-put o 'face 'query-replace))))))

;;;###autoload
(define-minor-mode number-mark-mode
  "A mode to work with numbers in a text buffer
This mode allows you to mark numbers using \\[number-mark],
or mark them using a regular expression search using \\[number-mark-group],
or mark all the numbers in the current column on lines matching a regular
expression using \\[number-mark-column]. Use \\[narrow-to-region]] if necessary. 

Marked numbers are highlighted using the face `query-replace'.

Once you have marked all the numbers you want to work with, you
can increment them all by a certain amount using \\[number-mark-add],
you can distribute a certain amount using \\[number-mark-distribute],
and you can sum them using \\[number-mark-sum]."
  :init-value nil
  :lighter " #"
  :keymap (list (cons (kbd "C-c C-n m") 'number-mark)
                (cons (kbd "C-c C-n u") 'number-unmark-all)
                (cons (kbd "C-c C-n s") 'number-mark-sum)
                (cons (kbd "C-c C-n a") 'number-mark-add)
                (cons (kbd "C-c C-n g") 'number-mark-group)
	        (cons (kbd "C-c C-n d") 'number-mark-distribute)
	        (cons (kbd "C-c C-n c") 'number-mark-column)))

(provide 'number-mark)

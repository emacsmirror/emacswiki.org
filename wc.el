;;; wc.el --- count words

;; Version: 0.1
;; Copyright (C) 2007 Theron Tlax
;; Time-stamp: <2007-09-01 14:24:41 thorne>
;; Author: thorne <thorne@timbral.net>
;; Created: 2007.9.1
;; Keywords: wp
;; Favorite day: Monday

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License,
;; version 2, as published by the Free Software Foundation.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; I can't freaking believe this is still not part of Emacs.
;; Maybe it could be done better--this is pretty basic, really.
;; I mean, sure you can't trust the word count for things like
;; sending rocket ships to the moon, but frankly, is it that
;; important?  Anyway, do M-x wc RET and it should pretty much
;; work.  It uses the current syntax table's definition of a
;; word.

;;; Code.

;; This is the logic, and can be used in a program.  The other
;; functions use this interactively in various ways.
(defun wc-non-interactive (start end)
  "Count the number of words in the current region."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (count-matches "\\sw+"))))

;;;###autoload
(defun wc-buffer ()
  "Display the number of words in the current buffer."
  (interactive)
  (message (concat "The current buffer contains "
		   (number-to-string
		    (wc-non-interactive (point-min) (point-max)))
		   " words.")))

;;;###autoload
(defun wc-region (start end)
  "Display number of words in the region."
  (interactive "r")
  (message (concat "The current region contains "
		   (number-to-string
		    (wc-non-interactive start end))
		   " words.")))

;;;###autoload
(defun wc-dwim ()
  "Display a word count.
If there is a region defined, display the count for the region.
If not, display a word count for the whole buffer."
  (interactive)
  (if mark-active
      (wc-region (point) (mark))
    (wc-buffer)))

(defalias 'wc 'wc-dwim)

(provide 'wc)

;;; wc.el ends here

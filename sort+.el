;;; sort+.el --- Extensions to `sort.el'.
;;
;; Filename: sort+.el
;; Description: Extensions to `sort.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2012, Drew Adams, all rights reserved.
;; Created: Thu Apr 18 10:16:50 1996
;; Version: 20.0
;; Last-Updated: Sun Jan  1 14:05:11 2012 (-0800)
;;           By: dradams
;;     Update #: 86
;; URL: http://www.emacswiki.org/cgi-bin/wiki/sort+.el
;; Keywords: unix, tools, sorting
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `sort'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `sort.el'.
;;
;;
;;  ***** NOTE: The following function defined in `sort.el' has been
;;              REDEFINED HERE:
;;
;;  `sort-reorder-buffer' - This version preserves text properties,
;;                          even in Emacs versions prior to 20.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `sort.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "sort" '(require 'sort+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'sort)
(eval-when-compile (require 'cl)) ;; cddar (plus, for Emacs <21: pop)

;;;;;;;;;;;;;;;;;;;


;; REPLACES ORIGINAL in `sort.el':
;; In versions of Emacs prior to 20, the original `sort-reorder-buffer'
;; did not preserve text properties (e.g. face, mouse-face).
;; This version does preserve them.  The only difference here is that
;; (insert-buffer-substring (current-buffer)...) has been replaced by
;; (insert-string (buffer-substring...))
(defun sort-reorder-buffer (sort-lists old)
  "Helper function for `sort-subr'."
  (let ((inhibit-quit t)
        (last (point-min))
        (min (point-min)) (max (point-max)))
    ;; Make sure insertions done for reordering do not go after any markers
    ;; at the end of the sorted region, by inserting a space to separate them.
    (goto-char (point-max))
    (insert-before-markers " ")
    (narrow-to-region min (1- (point-max)))
    (while sort-lists
      (goto-char (point-max))
      (insert-string (buffer-substring last (nth 1 (car old))))
      (goto-char (point-max))
      (insert-string (buffer-substring (nth 1 (car sort-lists))
                               (cddar sort-lists)))
      (setq last (cddar old))
      (pop sort-lists)
      (pop old))
    (goto-char (point-max))
    (insert-string (buffer-substring last max))
    (delete-region min max)             ; Delete original copy of the text.
    (goto-char (point-max))             ; Get rid of the separator, " ".
    (narrow-to-region min (1+ (point)))
    (delete-region (point) (1+ (point)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'sort+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sort+.el ends here

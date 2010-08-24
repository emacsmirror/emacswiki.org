;;; toggle-backslash-line.el ---  toggle between slashes and backslashes in the current line
;; -*- Mode: Emacs-Lisp -*-

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Usage:
;;
;; Typically bound to to the comint-mode-map as follows:
;;
;; (add-hook 'comint-mode-hook
;;   (lambda()
;;    (define-key comint-mode-map [(control x) (control ?\\)] 'toggle-backslash-line)))
;;
;; History:
;;    2010-08-24: Initial version <dov.grobgeld@gmail.com>
;;

(defun toggle-backslash-line ()
  "Toggle between forward slashes and backslashes in the current line."
  (interactive)
  (save-excursion
    (setq myBoundaries (bounds-of-thing-at-point 'line))
    (beginning-of-line)
    (save-restriction
      (narrow-to-region (car myBoundaries) (cdr myBoundaries))
      (if (search-forward "/" nil t)
          (progn
            (beginning-of-line)
            (while (search-forward "/" nil t) (replace-match "\\\\" 'literal)))
        (progn
          (beginning-of-line)
          (while (search-forward "\\" nil t) (replace-match "/")))
        )
      (end-of-line))))


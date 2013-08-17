;;; aok.el --- various useful ways to do `multi-occur'

;; Copyright (C) 2013 lynnux <lynnux.cn@gmail.com>
;; Copyright (C) 2004 Joe Corneli <jcorneli@math.utexas.edu>

;; Time-stamp: <jac -- Tue May 10 11:01:12 CDT 2005>

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; search all buffers, all buffers whose filenames have a certain
;; type, or all buffers in a certain mode, with `all-occur',
;; `type-occur', or `mode-occur', respectively.

;;; Changelog:
;;; 2013.8.6 updated for emacs 24.x, and occur-select by lynnux

;;; Code:

(eval-when-compile (require 'cl))

;;;###autoload
(defun all-occur (rexp)
  "Search all buffers for REXP."
  (interactive "MRegexp: ")
  (multi-occur (buffer-list) rexp))

;; this one {c}/{sh}ould be a completing read that would read from a
;; predefined list of filetype extensions (without requiring a match).
;;;###autoload
(defun type-occur (extension rexp)
  "EXTENSION denotes a filetype extension to search.
Run occur in all buffers whose names match this type for REXP."
  (interactive "MExtension: \nMRegexp: ")
  (or (when (functionp 'multi-occur-by-filename-regexp)
	(multi-occur-by-filename-regexp (concat ".*\." extension) rexp))
      (when (functionp 'multi-occur-in-matching-buffers)
	(multi-occur-in-matching-buffers (concat ".*\." extension) rexp))))

;;;###autoload
(defun mode-occur (mode rexp)
  "Search all buffers with major mode MODE for REXP."
  (interactive (list (read-command "Mode: ")
                     (read-string "Regexp: ")))
  (multi-occur (remove-if (lambda (buf)
                            (set-buffer buf)
                            (not (eq major-mode mode)))
                          (buffer-list))
               rexp))

;;;###autoload
(defun occur-select (more regx &optional nothing)
  "select what you wan't to see occur"
  (interactive 
   (cons
    (let* ((choice (read-char "Occur in: [a]ll, [t]ype, [m]ode, or just this buffer(any other key)?"))
	   (more  (list (cond ((eq choice ?a) nil)
			      ((eq choice ?t) (read-string "Extension: "))
			      ((eq choice ?m) (read-command "Mode: "))
			      (t ?o)))))
      (add-to-list 'more choice)
      (nreverse more))
    (occur-read-primary-args)))
  (let* ((choice (cadr more))
	 (morearg (car more)))
    (cond ((eq choice ?a) (all-occur regx))
	  ((eq choice ?t) (type-occur morearg regx))
	  ((eq choice ?m) (mode-occur morearg regx))
	  (t (occur regx)))))

(provide 'aok)
;;; aok.el ends here

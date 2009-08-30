;;; wikify-ell.el --- Transform the ELL into a Wiki page.

;; Copyright (C) 2001  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Version: 0.1

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
;;
;; This code uses ell.el to get and parse the ELL from the ELL site.
;; It the produces a wikified representation of the ELL.

;;; Code:

(require 'ell)

;;;###autoload
(defun wikify-ell-packages ()
  "Display the Emacs Lisp list in a Emacs buffer."
  (interactive)
  (if (get-buffer "*wikified-ell-packages*")
      (kill-buffer "*wikified-ell-packages*"))
  (switch-to-buffer "*wikified-ell-packages*")
  (insert "This page was created " (format-time-string "%Y-%m-%d") " from the EmacsLispList.\n\n")
  (insert "== The Emacs Lisp List ==\n\n")
  (insert "by StephenEglen\n\n")
  (insert "----\n")
  (mapcar (lambda (x)
            (insert (format "[%s %s] - %s (by %s)\n\n"
			    (car x)
			    (cadr x)
                            (car (cdr (cdr x)))
                            (wikify-name (car (cdr (cdr (cdr x))))))))
	  ;; use (setq test (ell-packages-list)) for testing
	  (reverse (ell-packages-list)))
  (goto-char (point-min)))

(defun wikify-name (name)
  "Wikify a name"
  ;; (setq name (encode-coding-string name 'iso-latin-1))
  (let ((words (split-string name " "))
	result)
    (dolist (word words)
      (when (string-match "^\\w\\w+$" word)
	(setq result (cons word result))))
    (setq result (nreverse result))
    (if (> (length result) 1)
	(apply 'concat result)
      (message "not wikified: %s" name)
      name)))

;;; wikify-ell.el ends here

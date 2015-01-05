;;; options+.el --- Extensions to `options.el'.
;;
;; Filename: options+.el
;; Description: Extensions to `options.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2015, Drew Adams, all rights reserved.
;; Created: Tue Feb  6 16:50:23 1996
;; Version: 20.0
;; Last-Updated: Thu Jan  1 11:08:03 2015 (-0800)
;;           By: dradams
;;     Update #: 94
;; URL: http://www.emacswiki.org/options+.el
;; Keywords: docs, help, internal
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `options', `options+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `options.el'.
;;
;;
;;  ***** NOTE: The following function defined in `options.el' has
;;              been REDEFINED HERE:
;;
;;  `list-options' - Runs `list-options-hook' at end.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `options.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "options" '(progn (require 'options+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 1996/03/19 dadams
;;     list-options: Added in-progress msgs.
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

(require 'options)

;;;;;;;;;;;;;;;;;;;;;


;; REPLACES ORIGINAL in `options.el':
;; 1. Runs `list-options-hook' at end.
;; 2. Added messages.
;;;###autoload
(defun list-options ()
  "Display a list of Emacs user options, with values and documentation.
Runs hook `list-options-hook' at the end."
  (interactive)
  (message "Looking up options (user variables)...")
  (with-output-to-temp-buffer "*List Options*"
    (let (vars)
      (mapatoms (function (lambda (sym)
                            (if (user-variable-p sym)
                                (setq vars (cons sym vars))))))
      (setq vars (sort vars 'string-lessp))
      (while vars
        (let ((sym (car vars)))
          (when (boundp sym)
            (princ ";; ")
            (prin1 sym)
            (princ ":\n\t")
            (prin1 (symbol-value sym))
            (terpri)
            (princ (substitute-command-keys
                    (documentation-property sym 'variable-documentation)))
            (princ "\n;;\n"))
        (setq vars (cdr vars))))
      (message "Looking up options (user variables)...done")
      (with-current-buffer "*List Options*"
        (Edit-options-mode)
        (run-hooks 'list-options-hook)
        (setq buffer-read-only t)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'options+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; options+.el ends here

;;; diff+.el --- Extensions to `diff.el' for Emacs 21 and later.
;;
;; Filename: diff+.el
;; Description: Extensions to `diff.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2012, Drew Adams, all rights reserved.
;; Created: Fri Mar 15 09:33:29 1996
;; Version: 21.0
;; Last-Updated: Thu Aug 23 10:06:38 2012 (-0700)
;;           By: dradams
;;     Update #: 574
;; URL: http://www.emacswiki.org/cgi-bin/wiki/diff+.el
;; Doc URL: http://emacswiki.org/emacs/DiffEnhancements
;; Keywords: data, matching, tools, unix, local
;; Compatibility: GNU Emacs: 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `diff.el' for Emacs 21 and later.
;;    The frame is resized to fit buffer "*Diff*".
;;
;;  Library `diff.el' changed significantly from Emacs 20 to Emacs 21.
;;  For extensions to `diff.el' that work with Emacs 20, see library
;;  `diff+20.el'.
;;
;;  For extensions to `diff' highlighting in Emacs 21 and later, see
;;  library `diff-mode-.el'.
;;
;;
;;  ***** NOTE: The following function defined in `diff.el' has
;;              been REDEFINED HERE:
;;
;;    `diff-sentinel' - Works with multiple Emacs versions.
;;                      Fits frame to *Diff* buffer.
;;
;;
;;  This file should be loaded *after* loading the standard GNU file
;;  `diff.el'.  So, in your `~/.emacs' file, do this:
;;
;;    (eval-after-load "diff" '(require 'diff+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2010/12/19 dadams
;;     diff-sentinel: Updated for > Emacs 23.2.
;; 2004/11/18 dadams
;;     Wrapped diff-sentinel body in conditon-case.
;; 2004/11/09 dadams
;;     New for Emacs 21. Renamed previous library diff+.el to diff+20.el.
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

;; Quiet the byte-compiler.
(defvar diff-old-temp-file)
(defvar diff-new-temp-file)

;;;;;;;;;;;;;;;;;;;;;;;;



;; REPLACES version in `diff.el'.
;;
;; 1. Selects *Diff* buffer's window and calls `fit-frame-if-one-window' at end.
;; 2. Handles Emacs 22 and later.  (Emacs 23.3 added the optional args.)
;;
(when (>= emacs-major-version 21)
  (defun diff-sentinel (code &optional old-temp-file new-temp-file)
    "Code run when the diff process exits.
CODE is the exit code of the process.  It should be 0 iff no diffs were found."
    (condition-case nil
        (progn
          (cond ((or (> emacs-major-version 23)
                     (and (= emacs-major-version 23) (> emacs-minor-version 2)))
                 (when old-temp-file (delete-file old-temp-file))
                 (when new-temp-file (delete-file new-temp-file)))
                (t
                 (when diff-old-temp-file (delete-file diff-old-temp-file))
                 (when diff-new-temp-file (delete-file diff-new-temp-file))))
          (save-excursion
            (save-window-excursion
              (select-window (get-buffer-window (current-buffer) t))
              (goto-char (point-max))
              (insert (format "\nDiff finished%s.  %s\n"
                              (if (equal 0 code) " (no differences)" "")
                              (current-time-string)))
              (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window)))))
      (error nil))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'diff+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diff+.el ends here

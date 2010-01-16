;;; diff+.el --- Extensions to `diff.el' for Emacs 21.
;;
;; Filename: diff+.el
;; Description: Extensions to `diff.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2010, Drew Adams, all rights reserved.
;; Created: Fri Mar 15 09:33:29 1996
;; Version: 21.0
;; Last-Updated: Fri Jan 15 12:50:25 2010 (-0800)
;;           By: dradams
;;     Update #: 559
;; URL: http://www.emacswiki.org/cgi-bin/wiki/diff+.el
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
;;    Extensions to `diff.el' for Emacs 21.
;;    The frame is resized to fit buffer "*Diff*".
;;
;;  Library `diff.el' changed significantly from Emacs 20 to Emacs 21.
;;  For extensions to `diff.el' that work with Emacs versions prior to
;;  version 21, see library `diff+20.el'.
;;
;;  For extensions to `diff' highlighting in Emacs 21, see library
;;  `diff-mode-.el'.
;;
;;
;;  ***** NOTE: The following function defined in `diff.el' has
;;              been REDEFINED HERE:
;;
;;    `diff-sentinel' - Fits frame to *Diff* buffer.
;;
;;
;;  This file should be loaded *after* loading the standard GNU file
;;  `diff.el'.  So, in your `~/.emacs' file, do this:
;;
;;    (eval-after-load "diff" '(require 'diff+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;



;; REPLACES version in `diff.el'.
;; Selects *Diff* buffer's window and calls `fit-frame-if-one-window' at end.
;;
(when (>= emacs-major-version 21)
  (defun diff-sentinel (code)
    "Code run when the diff process exits.
CODE is the exit code of the process.  It should be 0 iff no diffs were found."
    (condition-case nil
        (progn
          (if diff-old-temp-file (delete-file diff-old-temp-file))
          (if diff-new-temp-file (delete-file diff-new-temp-file))
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

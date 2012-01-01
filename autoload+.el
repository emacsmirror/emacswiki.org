;;; autoload+.el --- Extensions to `autoload.el'.
;;
;; Filename: autoload+.el
;; Description: Extensions to `autoload.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2012, Drew Adams, all rights reserved.
;; Created: Wed Mar 19 15:45:38 1997
;; Version: 21.0
;; Last-Updated: Sun Jan  1 14:29:26 2012 (-0800)
;;           By: dradams
;;     Update #: 50
;; URL: http://www.emacswiki.org/cgi-bin/wiki/autoload+.el
;; Keywords: maint
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `autoload'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;  ***** NOTE: The following function defined in `autoload.el' has
;;              been REDEFINED HERE:
;;
;;  `update-file-autoloads' - Ignores hooks for `emacs-lisp-mode'.
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

(require 'autoload) ;; update-file-autoloads

;;;;;;;;;;;;;;;;;;;;;;


(or (fboundp 'old-update-file-autoloads)
    (fset 'old-update-file-autoloads (symbol-function 'update-file-autoloads)))

;; REPLACES ORIGINAL in `autoload.el' (dumped):
;; Temporarily removes any hooks for `emacs-lisp-mode'.
(defsubst update-file-autoloads (file)
  "Update the autoloads for FILE in `generated-autoload-file'
\(which FILE might bind in its local variables).
Return FILE if there was no autoload cookie in it.

Note: Temporarily removes any hooks for emacs-lisp-mode."
  (interactive "fUpdate autoloads for file: ")
  (let ((emacs-lisp-mode-hook nil)) (old-update-file-autoloads file)))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'autoload+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autoload+.el ends here

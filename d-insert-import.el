;;; d-insert-import.el --- insert import statement for D language
;; $Id: d-insert-import.el 1499 2007-07-15 07:57:52Z rubikitch $

;; Copyright (C) 2007  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: D, import, convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; `d-insert-import' prepends appropriate import statement for the
;; function at point.

;;; Installation:

;; (require 'd-insert-import)
;; (defun d-mode-hook0 ()
;;   (define-key d-mode-map "\C-c\C-i" 'd-insert-import-at-point))
;; (add-hook 'd-mode-hook 'd-mode-hook0)

;; 
;;; History:

;; ------------------------------------------------------------------------
;; r1499 | rubikitch | 2007-07-15 16:57:52 +0900 (Sun, 15 Jul 2007) | 2 lines

;; skip module declaration.

;; ------------------------------------------------------------------------
;; r1457 | rubikitch | 2007-01-24 04:33:44 +0900 (Wed, 24 Jan 2007) | 3 lines

;; updated Commentary.
;; rename function: d-insert-import -> d-insert-import-at-point

;; ------------------------------------------------------------------------
;; r1456 | rubikitch | 2007-01-24 01:11:27 +0900 (Wed, 24 Jan 2007) | 2 lines

;; import -> private import

;; ------------------------------------------------------------------------
;; r1453 | rubikitch | 2007-01-18 20:48:12 +0900 (Thu, 18 Jan 2007) | 1 line

;; *** empty log message ***
;; ------------------------------------------------------------------------
;; r1452 | rubikitch | 2007-01-18 20:46:04 +0900 (Thu, 18 Jan 2007) | 2 lines

;; changed message

;; ------------------------------------------------------------------------
;; r1451 | rubikitch | 2007-01-18 20:34:26 +0900 (Thu, 18 Jan 2007) | 2 lines

;; initial

;; ------------------------------------------------------------------------


;;; Code:

(require 'd-insert-import-data)
(defvar d-import-token-hash)

(defun d-insert-import-at-point (sym)
  (interactive (list (word-at-point)))
  (let ((importlib (gethash sym d-import-token-hash)))
    (if importlib
        (let ((import-stmt (concat "import " importlib ";\n")))
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^module.+\n" nil t)
            (cond ((search-forward import-stmt nil t)
                   (message "found:%s" import-stmt)
                   t)
                  (t
                   (insert "private " import-stmt)
                   (message "inserted:%s" import-stmt)
                   importlib))))
      (error "cannot find library for %s" sym))))

;; (equal "std.cstream" (with-temp-buffer (d-insert-import-at-point "dout")))
;; (eq t (with-temp-buffer (insert "private import std.cstream;\n") (d-insert-import-at-point "dout")))
;; (with-temp-buffer (d-insert-import-at-point "notfound"))


(provide 'd-insert-import)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "d-insert-import.el")
;; prepare (find-sh0 "ruby generate-el.rb > d-insert-import-data.el")
;;; d-insert-import.el ends here

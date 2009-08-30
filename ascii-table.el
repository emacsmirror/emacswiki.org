;;; ascii-table.el --- simple ASCII table

;; Copyright (C) 2002  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.0
;; Keywords: convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?AsciiTable

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; `ascii-table' shows you an ASCII table.

;;; Code:

(defun ascii-table (&optional limit base)
  "Print the ascii table (up to char 127).

Given the optional argument LIMIT, print the characters up to char
LIMIT.  Try 254 for example.

Optional argument BASE can be either 8 for octal, 10 for decimal, or
16 for hex."
  (interactive "P")
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (let ((fmt (cond ((eq base 16) "%4x %4s")
		   ((eq base 8) "%4o %4s")
		   (t "%4d %4s")))
	(i 0))
    (setq limit (or limit 127))
    (insert (format "ASCII characters up to number %d.\n" limit))
    (while (<= i limit)
      (insert (format fmt i (single-key-description i)))
      (setq i (+ i 1))
      (if (= 0 (mod i 6))
	  (newline)
	(insert "   "))))
  (beginning-of-buffer))

(defalias 'ascii-table-decimal 'ascii-table)

(defun ascii-table-octal (&optional limit)
  "Print the ascii table up to LIMIT (default is 0177)."
  (interactive "P")
  (ascii-table limit 8))

(defun ascii-table-hex (&optional limit)
  "Print the ascii table up to LIMIT (default is 0x7f)."
  (interactive "P")
  (ascii-table limit 16))

(provide 'ascii-table)

;;; ascii-table.el ends here.

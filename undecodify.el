;;; undecodify.el --- Translate UTF-8 encoded as CP1252 encoded as UTF-8
;; Copyright (C) 2019 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: charsets

;; Undecodify is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Undecodify is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; (undecodify "itâ€™s") => "it’s"

;;; Code:

(defvar undecodify-map
  '((#x80 . #x20AC)
    (#x82 . #x201A)
    (#x83 . #x0192)
    (#x84 . #x201E)
    (#x85 . #x2026)
    (#x86 . #x2020)
    (#x87 . #x2021)
    (#x88 . #x02C6)
    (#x89 . #x2030)
    (#x8A . #x0160)
    (#x8B . #x2039)
    (#x8C . #x0152)
    (#x8E . #x017D)
    (#x91 . #x2018)
    (#x92 . #x2019)
    (#x93 . #x201C)
    (#x94 . #x201D)
    (#x95 . #x2022)
    (#x96 . #x2013)
    (#x97 . #x2014)
    (#x98 . #x02DC)
    (#x99 . #x2122)
    (#x9A . #x0161)
    (#x9B . #x203A)
    (#x9C . #x0153)
    (#x9E . #x017E)
    (#x9F . #x0178)))

(defun undecodify (string)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (cl-loop for char across string
	     do (insert (or (car (rassq char undecodify-map))
			    char)))
    (decode-coding-string (buffer-string) 'utf-8)))

(defun undecodify-buffer ()
  "Undecodify the current buffer."
  (interactive)
  (let ((string (buffer-string)))
    (delete-region (point-min) (point-max))
    (insert (undecodify string))))

(provide 'undecodify)

;;; undecodify.el ends here

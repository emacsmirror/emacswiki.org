;;; linum-relative.el --- display relative line number in the left margin

;; Copyright 2012 Yen-Chin,Lee
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords: converience
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/linum-relative.el
(defconst linum-relative-version "0.1")

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; Display relative line numbers for the current buffer.
;;

;;; Usage:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'linum-relative)

;;; Code:

(eval-when-compile (require 'cl))
(require 'linum)

;;;; Faces
(defface linum-relative-current-face
  '((t :inherit linum :foreground "#CAE682" :background "#444444" :weight bold))
  "Face for displaying current line."
  :group 'linum)

;;;; Local vars
(defvar linum-relative-last-pos 0
  "Store last position.")

(defvar linum-relative-current-symbol "0"
  "The symbol you want to show on the current line, by default it is 0.
   You can use any string like \"->\". ")

;;;; Advices
(defadvice linum-update (around relative-linum-update activate)
  "This advice get the last position of linum."
  (setq linum-relative-last-pos (line-number-at-pos))
  ad-do-it)

;;;; Functions
(defun linum-relative (line-number)
  (let ((diff (abs (- line-number linum-relative-last-pos))))
    (propertize (format "%3s" (cond ((zerop diff) linum-relative-current-symbol)
				    (t (number-to-string diff))
				    ))
		'face (cond ((zerop diff) 'linum-relative-current-face)
			    (t 'linum)))))


(setq linum-format 'linum-relative)

(provide 'linum-relative)
;; linum-relative.el ends here.

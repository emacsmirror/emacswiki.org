;;; insert-time-string.el --- Insert the current time. -*- coding: utf-8; -*-

;; Copyright © 2010 Kevin Rodgers

;; Author: Kevin Rodgers <kevin.d.rodgers@gmail.com>
;; Created: 25 February 2010
;; Version: $Revision: 1.5 $
;; Keywords: time, date, convenience
;; RCS $Id: insert-time-string.el,v 1.5 2010/02/26 07:46:35 kevin Exp $

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;; M-x insert-time-string
;; C-u M-x insert-time-string

;; Customizations:

;; Key binding: (global-set-key (kbd "C-c t") 'insert-time-string)

;; Date/Time format:
;;  (setq insert-time-string-format-alist
;;       (cons '("pseudo-iso" . "%Y-%m-%d %T") insert-time-string-format-alist))

;; Voilà!
;; C-c t pseudo-iso RET

;; If that's too much typing:
;; (setq insert-time-string-default-format "pseudo-iso")
;; C-c t RET

;;; Code:

(defvar insert-time-string-format-alist
  '(("iso-8601-date" . "%Y-%m-%d")
    ("iso-8601-time" . "%T%z")
    ("iso-8601" . "%Y-%m-%dT%T%z")
    ("locale-date" . "%x")
    ("locale-time" . "%X")
    ("locale" . "%c")
    ("locale-alternative-date" . "%Ex")
    ("locale-alternative-time" . "%EX")
    ("locale-alternative" . "%Ec"))
  "Alist of (NAME . FORMAT-STRING) elements.
See `format-time-string' for FORMAT-STRING.")

(defvar insert-time-string-default-format "locale")

(defun insert-time-string (format-string &optional time universal)
  "Insert the current time at point, according to FORMAT-STRING.
By default, insert the local time; with a prefix arg, insert the Universal Time.
See `format-time-string' for FORMAT-STRING, TIME, and UNIVERSAL arguments."
  (interactive
   (list (cdr (assoc (completing-read (format "Format (%s): "
					      insert-time-string-default-format)
				      insert-time-string-format-alist
				      nil t nil nil
				      insert-time-string-default-format)
		     insert-time-string-format-alist))
	 (current-time)
	 current-prefix-arg))
  (insert (format-time-string format-string time universal)))

(provide 'insert-time-string)
;;; insert-time-string.el ends here

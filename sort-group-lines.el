;;; sort-group-lines.el --- Group lines in region matching regexps.

;; Filename: sort-group-lines.el
;; Description: Group lines in region matching regexps.
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2015, Joe Bloggs, all rites reversed.
;; Created: 2015-08-30 22:49:38
;; Version: 0.1
;; Last-Updated: 2015-08-30 22:49:38
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/sort-group-lines
;; Keywords: convenience
;; Compatibility: GNU Emacs 24.5.1
;; Package-Requires:  
;;
;; Features that might be required by this library:
;;
;; 
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 12k9zUo9Dgqk8Rary2cuzyvAQWD5EAuZ4q
;;
;; This file provides a single command `sort-group-lines' for sorting lines into
;; groups matching regular expressions.
;; 
;;;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `sort-group-lines'
;;    Group lines between positions BEG and END according to which regexp in REGEXPS they match.
;;

;;; Installation:
;;
;; Put sort-group-lines.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'sort-group-lines)


;;; Code:

;;;###autoload
(defun sort-group-lines (reverse beg end regexps)
  "Group lines between positions BEG and END according to which regexp in REGEXPS they match.
The groups are then placed in the same order as in REGEXPS; top first if REVERSE is nil, or bottom first if non-nil.
 When called interactively the regexp's are prompted for until a blank is entered, BEG and END are defined by the currently
 active region, and REVERSE is set to t if a prefix arg is passed but nil otherwise."
  (interactive (list current-prefix-arg (region-beginning) (region-end) nil))
  (let ((n 0) (regexp t) (intp (interactive-p)))
    (while (and intp (not (equal regexp "")))
      (setq regexps
	    (append regexps (list (read-string (concat "Enter regexps to match, in order (leave blank to end): ")))))
      (setq regexp (nth n regexps))
      (setq n (1+ n))))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t))
	(sort-subr reverse 'forward-line 'end-of-line nil nil
		   (lambda (str1 str2)
		     (let ((cur 0) (match nil))
		       (while (and (< cur (length regexps)) (not match))
			 (let* ((regexp (nth cur regexps))
				(m1 (string-match regexp (buffer-substring (car str1) (cdr str1))))
				(m2 (string-match regexp (buffer-substring (car str2) (cdr str2)))))
			   (setq cur (1+ cur))
			   (setq match
				 (cond ((and (not m1) (not m2)) nil)
				       ((and m1 (not m2)) 1)
				       ((and (not m1) m2) -1)
				       ((< m1 m2) 1)
				       (t -1)))))
		       (> match 0))))))))

(provide 'sort-group-lines)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "sort-group-lines.el" (buffer-name) (buffer-string) "update")

;;; sort-group-lines.el ends here

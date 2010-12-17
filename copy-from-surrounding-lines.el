;;; copy-from-surrounding-lines.el
;; Author: Paul Hobbs
;; Email:  paul dot mcdill dot hobbs atsign gmail
;; Usage: 
;; (global-set-key (kbd "M-n") 'ph/copy-thing-from-next-line)
;; (global-set-key (kbd "M-p") 'ph/copy-thing-from-prev-line)

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'misc)
(defalias 'copy-prev-line 'copy-from-above-command)
(defun copy-one-from-prev-line (&optional arg)
  (interactive "P")
  (copy-prev-line (or arg 1)))

(defun copy-one-from-next-line (&optional arg)
  (interactive "P")
  (copy-next-line (or arg 1)))

(defun copy-sexp-from-prev-line (&optional arg)
  (interactive "P")
  (unless (> (location-of (previous-line)
			  (forward-sexp))
	     (line-end-position 0))
    ;; If it's not a balanced expression, fill the whole word.
    (let ((w (save-excursion (previous-line)
			    (thing-at-point 'word))))
      (if w (insert w) 
	(insert (save-excursion 
		  (previous-line)
		  (buffer-substring (point) 
				    (location-of (forward-sexp)))))))))

(defun copy-sexp-from-next-line (&optional arg)
  (interactive "P")
  (unless (> (location-of (next-line)
			  (forward-sexp))
	     (line-end-position 2))
    ;; If it's not a balanced expression, fill the whole word.
    (let ((w (save-excursion (next-line)
			     (thing-at-point 'word))))
      (if w (insert w) 
	(insert (save-excursion 
		  (next-line)
		  (buffer-substring (point) 
				    (location-of (forward-sexp)))))))))


(defun ph/copy-thing-from-next-line (&optional arg)
  (interactive "P")
  (if (eq last-command 'ph/copy-thing-from-next-line)
      (progn 
	(undo)
	(copy-next-line arg))
    (copy-sexp-from-next-line arg)))

(defun ph/copy-thing-from-prev-line (&optional arg)
  (interactive "P")
  (if (eq last-command 'ph/copy-thing-from-prev-line)
      (progn 
	(undo)
	(copy-prev-line arg))
    (copy-sexp-from-prev-line arg)))


(defun copy-next-line (&optional arg)
  "Copy characters from next nonblank line, starting just above point.
Copy ARG characters, but not past the end of that line.
If no argument given, copy the entire rest of the line.
The characters copied are inserted in the buffer before point."
  (interactive "P")
  (let ((cc (current-column))
	n (string ""))
    (save-excursion
      (end-of-line)
      (forward-char 1)
      (skip-chars-forward "\ \t\n")
      (move-to-column cc)
      ;; Default is enough to copy the whole rest of the line.
      (setq n (if arg (prefix-numeric-value arg) (point-max)))
      ;; If current column winds up in middle of a tab,
      ;; copy appropriate number of "virtual" space chars.
      (if (< cc (current-column))
	  (if (= (preceding-char) ?\t)
	      (progn
		(setq string (make-string (min n (- (current-column) cc)) ?\s))
		(setq n (- n (min n (- (current-column) cc)))))
	    ;; In middle of ctl char => copy that whole char.
	    (backward-char 1)))
      (setq string (concat string
			   (buffer-substring
			    (point)
			    (min (save-excursion (end-of-line) (point))
				 (+ n (point)))))))
    (insert string)))

(provide 'copy-from-surrounding-lines)

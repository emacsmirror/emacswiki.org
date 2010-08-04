;;; autosmiley.el --- Convert smileys into their graphical representation

;; Author: Damyan Pepper (gmail account, username damyanp)
;; Created: 20060315

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Defines the minor mode autosmiley-mode that converts smileys like
;; :-) into their graphical representations on the fly.

;; Tested on:
;;
;; GNU Emacs 22.0.50.1 (i386-mingw-nt5.1.2600) of 2006-03-14 on W2ONE
;;
;; History:
;;
;; 20060315 - First Release



(require 'smiley)

(defun autosmiley-overlay-p (overlay)
  "Return whether OVERLAY is an overlay of autosmiley mode."
  (memq (overlay-get overlay 'category)
		'(autosmiley)))

(defun autosmiley-remove-smileys (beg end)
  (dolist (o (overlays-in beg end))
	(when (autosmiley-overlay-p o)
	  (delete-overlay o))))

(defvar *autosmiley-counter* 0
  "Each smiley needs to have a unique display string otherwise
  adjacent smileys will be merged into a single image.  So we put
  a counter on each one to make them unique")

(defun autosmiley-add-smiley (beg end image)  
  (let ((overlay (make-overlay beg end)))
	(overlay-put overlay 'category 'autosmiley)
	(overlay-put overlay 'display (append image (list :counter (incf *autosmiley-counter*))))))


(defun autosmiley-add-smileys (beg end)
  (save-excursion
	(dolist (entry smiley-cached-regexp-alist)
	  (let ((regexp (car entry))
			(group (nth 1 entry))
			(image (nth 2 entry)))
		(when image
		  (goto-char beg)
		  (while (re-search-forward regexp end t)
			(autosmiley-add-smiley (match-beginning group) (match-end group) image)))))))


(defun autosmiley-change (beg end &optional old-len)
  (let ((beg-line (save-excursion (goto-char beg) (line-beginning-position)))
		(end-line (save-excursion (goto-char end) (line-end-position))))
	(autosmiley-remove-smileys beg-line end-line)
	(autosmiley-add-smileys beg-line end-line)))
  

;;;###autoload
(define-minor-mode autosmiley-mode
  "Minor mode for automatically replacing smileys in text with
cute little graphical smileys."
  :group 'autosmiley :lighter " :)"
  (save-excursion
	(save-restriction
	  (widen)
	  (autosmiley-remove-smileys (point-min) (point-max))
	  (if autosmiley-mode
		  (progn
			(unless smiley-cached-regexp-alist
			  (smiley-update-cache))
			(jit-lock-register 'autosmiley-change))
		(jit-lock-unregister 'autosmiley-change)))))


(provide 'autosmiley)

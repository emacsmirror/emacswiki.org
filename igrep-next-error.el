;;; igrep-next-error.el --- Highlight text matched by `igrep'
;;; -*-unibyte: t;-*-

;; Copyright © 2003, 2004 Kevin Rodgers

;; Author: Kevin Rodgers <ihs_4664@yahoo.com>
;; Created:  14 Aug 2003
;; Version: $Revision: 1.2 $
;; Keywords: search, highlight
;; RCS: $Id: igrep-next-error.el,v 1.2 2004/05/12 19:45:28 kevinr Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; The `igrep-next-error' command is just like the `next-error' Emacs
;; command, but it also highlights the matched text in the source buffer
;; if the compilation buffer is an *igrep* buffer.  Similarly, the
;; `igrep-previous-error' and `igrep-first-error' commands are just like
;; the `previous-error' and `first-error' Emacs commands, plus
;; highlighting.
;; 
;; You can customize the `igrep-highlight' user option and the
;; `igrep-highlight' face, and you can make the
;; `igrep-highlight-overlay' variable buffer local.
;; 
;; You can also safely replace the Emacs commands' key bindings:
;; (substitute-key-definition 'next-error 'igrep-next-error
;; 			      (current-global-map))
;; (substitute-key-definition 'previous-error 'igrep-previous-error
;; 			      (current-global-map))
;; (substitute-key-definition 'first-error 'igrep-first-error
;; 			      (current-global-map))

;;; Code:

(require 'igrep)			; igrep-regex-history
(require 'custom)			; defgroup, defcustom

(defcustom igrep-highlight t
  "*If non-nil, highlight matched text in the `igrep-highlight' face."
  :group 'igrep
  :type '(boolean))

(defface igrep-highlight
  '((t (:inherit highlight)))
  "Face for highlighting `grep' matches."
  :group 'igrep)

(defvar igrep-highlight-overlay nil
  ;; Do we have to worry about interference from fontification (text
  ;; properties vs. overlays, priority property)?
  "The overlay used by \\[igrep-next-error] to highlight matched text.
To highlight matched text simultaneously in multiple buffers, use
\\[make-variable-buffer-local].")

;; This could be implemented as (defadvice next-error (after ...) ...):
(defun igrep-next-error (argp)
  "Like `next-error', but also highlight the matched text.
Highlighting only occurs if the compilation buffer is an *igrep* buffer.

See the `igrep-highlight' option and the `igrep-highlight' face.
See also `igrep-highlight-overlay'."
  (interactive "P")
  (next-error argp)
  (if (and igrep-highlight
	   (equal (save-excursion
		    (set-buffer compilation-last-buffer)
		    mode-name)
		  "igrep"))
      (save-excursion
	(beginning-of-line)
	;; Signal an error if the search fails:
	(if (re-search-forward (igrep-regex-to-emacs (car igrep-regex-history))
			       (if (fboundp 'line-end-position)
				   (line-end-position) ; Emacs 20
				 (save-excursion (end-of-line) (point))))
	    (if (and (overlayp igrep-highlight-overlay)
		     (not (local-variable-if-set-p 'igrep-highlight-overlay)))
		(move-overlay igrep-highlight-overlay
			      (match-beginning 0) (match-end 0)
			      (current-buffer))
	      (progn
		(setq igrep-highlight-overlay
		      (make-overlay (match-beginning 0) (match-end 0)))
		(overlay-put igrep-highlight-overlay 'category 'igrep)
		(overlay-put igrep-highlight-overlay 'face
			     'igrep-highlight)))))))

(defun igrep-regex-to-emacs (regex &optional program)
  "Convert an `igrep-program' REGEX to an Emacs regexp."
  (if (eq program "fgrep")
      (regexp-quote regex)
    regex))

(defun igrep-previous-error (&optional argp)
  "Like `previous-error', but also highlight the matched text.
Highlighting only occurs if the compilation buffer is an *igrep* buffer.

See `igrep-next-error'."
  (interactive "p")
  (igrep-next-error (- argp)))

(defun igrep-first-error ()
  "Like `first-error', but also highlight the matched text.
Highlighting only occurs if the compilation buffer is an *igrep* buffer.

See `igrep-next-error'."
  (interactive)
  (igrep-next-error '(4)))

(defun igrep-unhighlight ()
  "Delete `igrep-highlight-overlay'."
  (interactive)
  (let ((buffers (if (local-variable-if-set-p 'igrep-highlight-overlay)
		     (buffer-list)
		   (list (current-buffer)))))
    (while buffers
      (save-excursion
	(set-buffer (car buffers))
	(if (overlayp igrep-highlight-overlay)
	    (delete-overlay igrep-highlight-overlay)))
      (setq buffers (cdr buffers)))))

(defun igrep-kill-buffer-hook ()
  "Delete the `igrep-highlight-overlay' when an *igrep* buffer is killed."
  (if (or (equal mode-name "igrep")
	  (and (boundp 'name-of-mode)
	       (equal (symbol-value 'name-of-mode) "igrep"))) ; Emacs 19 hack
      ;; local hook function:
      (add-hook 'kill-buffer-hook 'igrep-unhighlight nil t)))

(or (face-differs-from-default-p 'igrep-highlight)
    (copy-face 'highlight 'igrep-highlight))

(add-hook 'compilation-mode-hook 'igrep-kill-buffer-hook)

(provide 'igrep-next-error)

;;; igrep-next-error.el ends here

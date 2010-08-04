;;;; redspace mode --- a minor mode for highlighting empty space at end of lines

;; Copyright (C) 2008  Kristian Rumberg

;; Author: Kristian Rumberg <kristianrumberg is my full gmail>
;; Created: 2008

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place, Suite 330, Boston, MA  02111-1307  USA

;; The overlay code in this program is stolen from the "flymake-mode" in GNU Emacs

;;; Code:

(defvar redspace-timer nil)

(defface redspace-errline
  ;;+   '((((class color)) (:foreground "OrangeRed" :bold t :underline t))
  ;;+   '((((class color)) (:underline "OrangeRed"))
  '((((class color)) (:background "OrangeRed")))
  "Face used for marking error lines."
  :group 'redspace)

(defun redspace-overlay-p (ov)
  "Determine whether overlay OV was created by redspace."
  (and (overlayp ov) (overlay-get ov 'redspace-overlay)))

(defun redspace-region-has-redspace-overlays (beg end)
  "Check if region specified by BEG and END has overlay.
Return t if it has at least one redspace overlay, nil if no overlay."
  (let ((ov                  (overlays-in beg end))
	(has-redspace-overlays  nil))
    (while (consp ov)
      (when (redspace-overlay-p (car ov))
	(setq has-redspace-overlays t))
      (setq ov (cdr ov)))
    has-redspace-overlays))

(defun redspace-make-overlay (beg end)
  (let ((face 'redspace-errline))
  "Allocate a redspace overlay in range BEG and END."
  (when (not (redspace-region-has-redspace-overlays beg end))
    (let ((ov (make-overlay beg end nil t t)))
      (overlay-put ov 'face           face)
      (overlay-put ov 'redspace-overlay  t)
      (overlay-put ov 'priority 100)
      ov))))

(defun redspace-delete-own-overlays ()
  "Delete all redspace overlays in BUFFER."
  (interactive)
  (dolist (ol (overlays-in (point-min) (point-max)))
    (when (redspace-overlay-p ol)
      (delete-overlay ol))))

(defun redspace-starttimer()
  (setq redspace-timer (run-with-timer 1 5 'redspace-check-buffer)))

(defun redspace-killtimer()
  (cancel-timer redspace-timer))

(defun redspace-check-buffer()
  (interactive)
  (redspace-delete-own-overlays)
(save-excursion
  (goto-char (point-min))
  (while (re-search-forward "[ +|\t+]$" (point-max) t)
      (let (e)
	(setq e (point))
	(while (looking-back "[ +|\t+]")
	  (backward-char)
	  )
	(redspace-make-overlay (point) e)
	(end-of-line)))))

;;;###autoload
(define-minor-mode redspace-mode
  "Toogle redspace-mode"
  :group 'redspace
  :global t
  :init-value nil
  :lighter redspace-string
  (if redspace-mode
      (progn
	(redspace-starttimer))
    (progn
      (redspace-killtimer)))
      (redspace-delete-own-overlays)
  redspace-mode)

(provide 'redspace-mode)

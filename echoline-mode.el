;;; echoline-mode.el --- A minor mode for octave which highlights "echoing" lines
;; that does not end with a ";" or that do not cause echo
;; such as "plot" or "figure"
;; Copyright (C) 2008,  Kristian Rumberg (kristianrumberg@gmail.com)

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

(defvar echoline-timer nil)
(defvar echoline-mode nil)
(make-variable-buffer-local 'echoline-mode)
(defvar echoline-string " echoline "
  "Variable containing the string to be inserted into the mode line.")
(make-variable-buffer-local 'echoline-string)

(defface echoline-face
  '((((class color)) (:background "LightGreen")))
  "Face used for marking error lines."
  :group 'echoline)

(defun echoline-overlay-p (ov)
  "Determine whether overlay OV was created by echoline."
  (and (overlayp ov) (overlay-get ov 'echoline-overlay)))

(defun echoline-region-has-echoline-overlays (beg end)
  "Check if region specified by BEG and END has overlay.
Return t if it has at least one echoline overlay, nil if no overlay."
  (let ((ov                  (overlays-in beg end))
	(has-echoline-overlays  nil))
    (while (consp ov)
      (when (echoline-overlay-p (car ov))
	(setq has-echoline-overlays t))
      (setq ov (cdr ov)))
    has-echoline-overlays))

(defun echoline-make-overlay (beg end)
  (let ((face 'echoline-face))
  "Allocate a echoline overlay in range BEG and END."
  (when (not (echoline-region-has-echoline-overlays beg end))
    (let ((ov (make-overlay beg end nil t t)))
      (overlay-put ov 'face           face)
      (overlay-put ov 'echoline-overlay  t)
      (overlay-put ov 'priority 100)
      ov))))

(defun echoline-delete-own-overlays ()
  "Delete all echoline overlays in BUFFER."
  (interactive)
  (dolist (ol (overlays-in (point-min) (point-max)))
    (when (echoline-overlay-p ol)
      (delete-overlay ol))))

(defun echoline-starttimer()
  (setq echoline-timer (run-with-timer 1 5 'echoline-check-buffer)))

(defun echoline-killtimer()
  (cancel-timer echoline-timer))

(defun echoline-looking-at-octave-keyword()
  (interactive)
   (let ((kwords '("#" "%" "if" "else" "while" "end" "function" "for"
		   "clear" "close" "hold" "figure" "imagesc" "pause"
		   "subplot" "plot" "disp" "break" "error" "usage" 
                   "switch" "case" "try" "catch")) found it)
     (setq found nil)
     (setq it kwords)
     (while it
       (when (looking-at (car it))
	 (setq found t))
       (setq it (cdr it)))
     found))

(defun echoline-check-buffer()
  (interactive)
  (when (and (equal major-mode 'octave-mode) echoline-mode)
  (echoline-delete-own-overlays)
  (let ((matches_list '("[^;]$"))) ;;'("\\w+$" "[\]|\)]$")))
    (mapc '(lambda(s)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward s (point-max) t)
	(let (e comps)
	  (setq e (point))
	    (setq compos (re-search-backward "[%|#]" (line-beginning-position) t))
	    (while (looking-back " ") (backward-char))
	    (when (or (and compos (not (looking-back ";"))) (not compos))
	      (back-to-indentation)
	      (unless (echoline-looking-at-octave-keyword)
		(echoline-make-overlay (point) e))))
	    (end-of-line))))    matches_list))))

;;;###autoload
(define-minor-mode echoline-mode
  "Toogle echoline-mode"
  :group 'echoline
  :global nil
  :init-value nil
  :lighter echoline-string
  (if echoline-mode
      (progn
	(echoline-starttimer))
    (progn
      (echoline-killtimer)))
      (echoline-delete-own-overlays)
  echoline-mode)

(provide 'echoline-mode)

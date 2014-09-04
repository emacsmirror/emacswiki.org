;;; pointhistory.el --- Remember point positions

;; Copyright (C) 2014  U-ITIHQ\Tobias.Zawada

;; Author: U-ITIHQ\Tobias.Zawada <Tobias.Zawada@smtp.1und1.de>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides two rings `pointhistory-ring' and `pointhistory-auto-ring'
;; for remembering positions of point.
;; If a command moves point over a long distance. The previous point position
;; is stored in `pointhistory-auto-ring'.
;; You can return to these point positions by the command `pointhistory-auto-backward' (bound to M-S-up).
;; This is most useful if you accidently moved point and want to return to the previous position.
;;
;; If you want to remember a point position for later use you can call pointhistory-point2ring (bound to M-+).
;; Press M-up to rotate through the remembered point positions.

;;; Code:


(require 'ring)

;; Run only once!
(defvar pointhistory-ringlength 100)
(defvar pointhistory-ring (make-ring pointhistory-ringlength)
  "Point position history (as a ring).")

(defvar pointhistory-pos 0
  "Current location in pointhistory-ring.")

(defvar pointhistory-auto-ring (make-ring pointhistory-ringlength)
  "Point position history (as a ring).")

(defvar pointhistory-auto-pos 0
  "Current location in pointhistory-ring.")

(defun pointhistory-point2ring (&optional point_ hist_ pos_)
  "Insert (\"name of current buffer\" . point position) into point history."
  (interactive)
  (unless point_ (setq point_ (point)))
  (unless hist_ (setq hist_ pointhistory-ring))
  (unless pos_ (setq pos_ 'pointhistory-pos))
  (let ((pos (cons (buffer-name) point_)))
    (ring-insert hist_ pos)
    ; (message "Push point: %S" pos)
    (set pos_ 0)
    ))

(defvar pointhistory-move-cmd nil
  "Last command that moves point.")
(make-variable-buffer-local 'pointhistory-cmd)

(defvar pointhistory-previous-move-cmd nil
  "Previous command that moves point.")
(make-variable-buffer-local 'pointhistory-cmd)

(defun pointhistory-move (bufpos)
  (let ((buf (get-buffer (car bufpos))))
    (when (bufferp buf)
      (setq pointhistory-move-cmd 'pointhistory-move)
      (switch-to-buffer buf 'noRecord)
      (goto-char (cdr bufpos)))))

(defun pointhistory-histmove (n &optional hist_ pos_)
  (unless hist_ (setq hist_ pointhistory-ring))
  (unless pos_ (setq pos_ 'pointhistory-pos))
  (unless (ring-empty-p hist_)
	(set pos_ (+ (eval pos_) n))
	(pointhistory-move (ring-ref hist_ (eval pos_)))))

(defun pointhistory-backward ()
  "Move forward one position in pointhistory."
  (interactive)
  (pointhistory-histmove 1))

(defun pointhistory-forward ()
  "Move forward one position in pointhistory."
  (interactive)
  (pointhistory-histmove -1))

(defun pointhistory-auto-backward ()
  "Move forward one position in pointhistory."
  (interactive)
  (pointhistory-histmove 1 pointhistory-auto-ring 'pointhistory-auto-pos))

(defun pointhistory-auto-forward ()
  "Move forward one position in pointhistory."
  (interactive)
  (pointhistory-histmove -1 pointhistory-auto-ring 'pointhistory-auto-pos))

(defvar pointhistory-save-at-jump-width 300
  "If point that far between two commands the original point goes into pointhistory-ring.")

(defvar pointhistory-previous-point 0
  "Previous point position in current buffer")
(make-variable-buffer-local 'pointhistory-previous-point)

(defvar pointhistory-in-move-sequence)
(make-variable-buffer-local 'pointhistory-in-move-sequence)

(defvar pointhistory-command-list
  '(scroll-up-command
    scroll-down-command
    mwheel-scroll
    beginning-of-buffer
    end-of-buffer
    mouse-drag-region
    ))

(defun pointhistory-advice-commands ()
  (loop for cmd in pointhistory-command-list do
	(eval `(defadvice ,cmd (after pointhistory activate)
		 (setq pointhistory-move-cmd (quote ,cmd))))
	))

(pointhistory-advice-commands)

(defun pointhistory-post-command-hook ()
  "Detects large jumps of points (see `pointhistory-save-at-jump-width') and saves old point to `pointhistory-auto-ring'."
  (let ((in-move-sequence (and pointhistory-move-cmd
			       (eq pointhistory-move-cmd pointhistory-previous-move-cmd))))
    (when (and (>= (abs (- pointhistory-previous-point (point))) pointhistory-save-at-jump-width)
	       (null pointhistory-in-move-sequence))
      (pointhistory-point2ring pointhistory-previous-point pointhistory-auto-ring 'pointhistory-auto-pos)
      (setq pointhistory-in-move-sequence in-move-sequence))
    (unless in-move-sequence
      (setq pointhistory-previous-point (point))
      (setq pointhistory-in-move-sequence nil))
    (setq pointhistory-previous-move-cmd pointhistory-move-cmd)
    (setq pointhistory-move-cmd nil)
    ))

(add-hook 'post-command-hook 'pointhistory-post-command-hook)

(global-set-key "\M-+" 'pointhistory-point2ring)

(global-set-key [M-up] 'pointhistory-backward)
(global-set-key [M-down] 'pointhistory-forward)

(global-set-key [M-S-up] 'pointhistory-auto-backward)
(global-set-key [M-S-down] 'pointhistory-auto-forward)

(provide 'pointhistory)
;;; pointhistory.el ends here

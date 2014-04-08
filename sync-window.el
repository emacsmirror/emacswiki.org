;;; sync-window.el --- Synchronize two side-by-side windows

;; Copyright (C) 2013  Zawada

;; Author: Tobias Zawada <i@tn-home.de>
;; Keywords: 

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

;; This is experimental code.
;; The main function is sync-window-mode (which see).

;;; Code:

(defface sync-window-face ;; originally copied from font-lock-function-name-face
  '((((class color) (min-colors 88) (background light)) (:foreground "Yellow" :background "Blue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Red" :background  "LightSkyBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Blue" :background "Yellow"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue" :background "Yellow"))
    (((class color) (min-colors 8)) (:foreground "blue" :bold t))
    (t (:bold t)))
  "Face used to highlight regions in `sync-window-mode' slaves."
  :group 'sync-window)

(defvar sync-window-overlay nil
  "Overlay for current master region in `sync-window-mode' slaves.")
(make-variable-buffer-local 'sync-window-overlay)

(defun sync-window-cleanup ()
  "Clean up after `sync-window-mode'."
  (interactive)
  (if (overlayp sync-window-overlay)
      (progn
    (delete-overlay sync-window-overlay)
    (setq sync-window-overlay nil))
    (remove-overlays (point-min) (point-max) 'sync-window-slave t)))

(defvar sync-window-master-hook nil
  "Hooks to be run by `sync-window' in the master window ")

(defun sync-window (&optional display-start)
  "Synchronize point position other window in current frame.
Only works if there are exactly two windows in the active wrame not counting the minibuffer."
  (interactive)
  (when (= (count-windows 'noMiniBuf) 2)
    (let ((p (line-number-at-pos))
      (start (line-number-at-pos (or display-start (window-start))))
      (vscroll (window-vscroll))
      breg ereg)
      (when (use-region-p)
    (setq breg (line-number-at-pos (region-beginning))
          ereg  (line-number-at-pos (if (looking-back "\n") (1- (region-end)) (region-end)))))
      (run-hooks 'sync-window-master-hook)
      (other-window 1)
      (goto-char (point-min))
      (when breg
    (sync-window-cleanup)
    (overlay-put (setq sync-window-overlay (make-overlay (line-beginning-position breg) (line-end-position ereg))) 'face 'sync-window-face)
    (overlay-put sync-window-overlay 'sync-window-slave t))
      (setq start (line-beginning-position start))
      (forward-line (1- p))
      (set-window-start (selected-window) start)
      (set-window-vscroll (selected-window) vscroll)
      (other-window 1)
      (unless display-start
    (redisplay t))
      )))

(defvar sync-window-mode-hook nil
  "Hooks to be run at start of `sync-window-mode'.")

(define-minor-mode sync-window-mode
  "Synchronized view of two buffers in two side-by-side windows."
  :group 'windows
  :lighter " ⇕"
  (if sync-window-mode
      (progn
    (add-hook 'post-command-hook 'sync-window-wrapper 'append t)
    (add-to-list 'window-scroll-functions 'sync-window-wrapper)
    (run-hooks 'sync-window-mode-hook)
    (sync-window))
    (remove-hook 'post-command-hook 'sync-window-wrapper t)
    (setq window-scroll-functions (remove 'sync-window-wrapper window-scroll-functions))
    ))

(defun sync-window-wrapper (&optional window display-start)
  "This wrapper makes sure that `sync-window' is fired from `post-command-hook'
only when the buffer of the active window is in `sync-window-mode'."
  (with-selected-window (or window (selected-window))
    (when sync-window-mode
      (sync-window display-start))))

(defvar mercury-blame-resize-min 5)

(defun mercury-blame-resize ()
  "Resize mercury blame window to blame string at point only."
  (interactive) ;; for debugging
  (save-excursion
    (beginning-of-line)
    (let ((n (skip-chars-forward "^:\n")))
      (when (looking-at ":")
    (condition-case err
        (enlarge-window (- (max mercury-blame-resize-min n)
                   (window-width) -1)
                'horizontal)
      (error))))))

(provide 'sync-window)
;;; sync-windows.el ends here

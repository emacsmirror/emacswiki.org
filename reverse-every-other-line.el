;;; reverse-every-other-line.el -- Reverse every other visible line.
;;
;; Copyright 2014 Marco Wahl
;;
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
;;
;;
;; Version: 0.3
;; Created: 2014-01-27
;; Contact: marcowahlsoft()gmail.com
;;
;;
;; Description:
;;
;; This program can help to save eye movements.
;;
;; By reversing every other line the reader often just can dip the
;; gaze at the end of a line to read on instead of doing the annoying
;; search for the next line at the other side of the text.
;;
;; The reversing of every other line is realized by making an
;; image-snapshot of every other line.  Each image gets reversed and
;; re-inserted.
;;
;; Results are quite good for a buffer that uses entirely the font
;; Courier.
;;
;;
;; Activation:
;;
;; Evaluate this buffer.  E.g. use M-x eval-buffer.
;;
;; Switch to the buffer that you want to read with every other line
;; reversed.  Activate the mode with M-x reol-mode.  Turn the mode off
;; with a further M-x reol-mode.
;;
;; See C-h f reol-mode for documentation.
;;
;; Note that the line wise scrolling can prefixed with a number.
;; E.g. C-3 v scrolls down three lines.
;;
;;
;; Precondition:
;;
;; - The program 'convert' of the ImageMagic-suite is needed for
;; creating snapshots and reversing images.
;;
;;
;; Bugs:
;;
;; - The reversing fails most of the time for info-buffers.
;;
;; - Activating reol-mode on a further buffer is not handled properly.
;;
;;
;; Potential:
;;
;; - Reuse the image-filenames instead of creating a new filename
;; every time.
;;
;; - Improve: Fix a scenario where this functionality fails.
;;
;; - Do something clever with the leading whitespaces.

 
;; Variables
(defvar reol-overlays nil
  "List of reol-overlays.")

(defvar reol-olimid-next-unused 0
  "Overlay-image-id that has not been used yet.

The program must reset this variable reasonably when an id gets used.")

(defvar reol-image-overlay-path "~/.emacs.d/reverse-every-other-line/")

(defvar reol-image-overlay-filename-format-string
  (concat (file-name-directory reol-image-overlay-path) "%d.png")
  "Template for the filenames to be written to disk.")

(defvar reol-mode nil)
(make-variable-buffer-local 'reol-mode)

(defvar reol-mode-hook nil)

(defvar reol-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'reol-next-page)
    (define-key map (kbd "<backspace>") 'reol-prev-page)
    (define-key map (kbd "<return>") 'reol-scroll-up-line) ; like in view-mode
    (define-key map "v" 'reol-scroll-up-line) 
    (define-key map "y" 'reol-scroll-down-line) ; like in view-mode
    (define-key map "V" 'reol-scroll-down-line)
    (define-key map "g" 'reol-refresh)
    (define-key map "q" 'reol-quit)
    (define-key map "?" 'describe-mode)
    map)
  "Keymap for reol-mode.")

 
;; Functions
(define-minor-mode reol-mode
  "Reverse every other line mode. 

In reol-mode every other line gets reversed.  reol-mode is a view
only mode.

\\{reol-mode-map}

This mode can help to save eye movements.

By reversing every other line the reader often just can dip the
gaze at the end of a line to read on instead of doing the annoying
search for the next line at the other side of the text.
"
  :lighter " reol-view" :keymap reol-mode-map
  (if reol-mode (reol-mode-enable) (reol-mode-disable)))

(defun reol-mode-enable ()
  (unless (file-exists-p reol-image-overlay-path)
    (make-directory reol-image-overlay-path))
  (setq reol-mode t
	reol-old-buffer-read-only buffer-read-only
	buffer-read-only t)
  (reol-reverse-every-other-in-visible-buffer-part-with-images)
;  (force-mode-line-update)
  (run-hooks 'reol-mode-hook))

(defun reol-mode-disable ()
  (reol-delete-overlays)
  (setq reol-mode nil
	buffer-read-only reol-old-buffer-read-only))

(defun reol-screen-line-at-pos (pos)
  "Return the window-line number that contains position POS."
  (cdr (nth 6 (posn-at-point pos))))

(defun reol-delete-overlays ()
  "Delete all overlays currently used with the reol-feature."
  (interactive)
  (mapc #'delete-overlay reol-overlays)
  (setq reol-overlays nil))

;; for testing: (local-set-key (kbd "<f8>") 'reol-snap-a-line-under-olimid-filename)
(defun reol-snap-a-line-under-olimid-filename (&optional n)
  "Snapshot line N and save under a certain name.

For details use the source."
  (interactive "P")
  (unless n (setq n 0))
  (let* ((beg (progn (move-to-window-line n) (point)))
	 (end (progn (end-of-line) (point)))
	 (x-win-left (nth 0 (window-inside-pixel-edges)))
	 (y-win-top (nth 1 (window-inside-pixel-edges)))
	 (y-pos-line (cdr (posn-x-y (posn-at-point end))))
	 (width (- (car (posn-x-y (posn-at-point end)))
		   (car (posn-x-y (posn-at-point beg)))))
	 (height (cdr (nth 9 (posn-at-point beg))))
	 (x-anchor (+ x-win-left))
	 (y-anchor (+ y-win-top y-pos-line)))
    (call-process
     "convert" nil nil nil
     (format "x:%s[%dx%d+%d+%d]" 
	     (frame-parameter nil 'window-id)
	     width height x-anchor y-anchor)
     "-flip"
     "-flop"
     (expand-file-name
      (format
       reol-image-overlay-filename-format-string
       ((lambda () (1- (setq reol-olimid-next-unused
			     (1+ reol-olimid-next-unused))))))))))

(defun reol-reverse-every-other-in-visible-buffer-part-with-images ()
  "Results are quite good for font Courier.

BUG: [2014-01-24 Fri 12:43] There is an additional one-pixel-line
for each image for several fonts, but not Courier.  It makes the
text longer when using the reol-feature.  No idea, how to get rid
of it.
"
  (interactive)
  (save-excursion 
    (let* ((first-line (progn (move-to-window-line 0) (point)))
	   (last-line (progn (move-to-window-line -1) (beginning-of-line) (point)))
	 (toggle t)
	 (olimid-start reol-olimid-next-unused)
	 (olimid-current olimid-start))
    (goto-char first-line)
    (while (and (= 0 (forward-line 1)) (<= (point) last-line))
      (if toggle
	  (let ((l-beg (progn (beginning-of-line) (point)))
		(l-end (progn (end-of-line) (point))))
	    (reol-snap-a-line-under-olimid-filename (reol-screen-line-at-pos l-beg))))
      (setq toggle (not toggle)))
    (goto-char first-line)
    (setq toggle t)
    (while (and (= 0 (forward-line 1)) (<= (point) last-line))
      (if toggle
	  (let ((l-beg (progn (beginning-of-line) (point)))
		(l-end (progn (end-of-line) (point))))
	    (setq reol-overlays (cons (make-overlay l-beg l-end) reol-overlays))
	    (overlay-put (car reol-overlays) 'display
	     (create-image 
	      (expand-file-name (format reol-image-overlay-filename-format-string
		      olimid-current))
	      nil nil
	      :ascent 'center
	      ; TODO: try to refine.  hint: try understand.  is this a
	      ; font-dependent thing?  e.g. :ascent 83 is possible.
	      ; there are further attributes...
	      ))
	    (setq olimid-current (1+ olimid-current))
	    (overlay-put (car reol-overlays) 'after-string " ⟵")))
      (setq toggle (not toggle))))))

(defun reol-next-page ()
  (interactive)
  (reol-delete-overlays)
  (scroll-up-command)
  (redisplay t)
  (reol-reverse-every-other-in-visible-buffer-part-with-images))

(defun reol-prev-page ()
  (interactive)
  (reol-delete-overlays)
  (scroll-down-command)
  (redisplay t)
  (reol-reverse-every-other-in-visible-buffer-part-with-images))

(defun reol-scroll-line (n)
  "Scroll the buffer N lines and reverse every other line."
  (reol-delete-overlays)
  (scroll-up-line n)
  (redisplay t)
  (reol-reverse-every-other-in-visible-buffer-part-with-images))

(defun reol-scroll-up-line (n)
  "Scroll the buffer up N lines and reverse every other line.

E.g.  for N = 1 the second-line becomes first."
  (interactive "p")
  (unless n (setq n 1))
  (reol-scroll-line n))

(defun reol-scroll-down-line (n)
  "Scroll the buffer down N lines and reverse every other line.

E.g.  for N = 1 the first-line becomes second."
  (interactive "p")
  (unless n (setq n 1))
  (reol-scroll-line (- n)))

(defun reol-refresh ()
  "Refresh the reol-representation for the given window."
  (interactive)
  (reol-delete-overlays)
  (redisplay t)
  (reol-reverse-every-other-in-visible-buffer-part-with-images) )

(defun reol-quit ()
  (interactive)
  (reol-mode 'toggle))

 
(provide 'reverse-every-other-line)

;; reverse-every-other-line.el ends here.

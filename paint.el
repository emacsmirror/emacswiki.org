;;; paint.el --- Makes a Emacs to the Paint Tool

;; Copyright (C) 2008  Taiki SUGAWARA <buzz.taiki@gmail.com>

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Requirements:
;; - emacs(>= 22) (maybe)

;;; Usage:
;; 1. put followings to your .emacs
;; (require 'paint)
;;
;; 2. if you want to create a new xbm image, type M-x paint.
;;    otherwise, if you want to edit a existing xbm image file, type
;;    M-x paint-load.
;;
;; 3. on a paint buffer, you can draw the line using mouse drags.
;;
;; 4. when your are satisfied, type M-x paint-save for save a image.
;; 
;; have fun!!

;;; Todo:
;; - support to not multiple of eight width bitmap.
;; - add following operations
;;   - filling operation
;;   - undo operation
;;   - pen shape selector
;;   - draw square operation
;; - add internal use overlay image.

;;; Reference URLs for plotting algrothms (Japanese)
;; - http://lee.phys.titech.ac.jp/~yasutake/PaintArea.html
;; - http://www2.starcat.ne.jp/~fussy/algo/index.htm


;;; Code:

(require 'cl)				; use cl-seq.

(defvar paint-buffer-name "*paint*")
(defvar paint-image nil)
(defvar paint-image-releaf 2)

(defvar paint-timer nil)
(defvar paint-timer-idle-delay 0.1)
(defvar paint-timer-pen-last-pos nil)

(defconst paint-motion-default-type
  (if (featurep 'meadow) 'timer 'track))

(defvar paint-motion-type paint-motion-default-type
  "*This variable specified motion type.
Following symbols are available:

- `timer': use a timer. this motion is a little slower, but can
  be used on meadow.

- `track': use a mouse tracking. recommended to be used.
")

(defvar paint-bit-fliped-system-p
  (and (eq system-type 'windows-nt) (not (featurep 'meadow))))

(defvar paint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'paint-save)
    (define-key map "l" 'paint-load)
    (define-key map " " 'paint-toggle-erase)
    (define-key map [down-mouse-2] 'paint-show-menu)
    map)
  "A keymap for `paint-mode'.")

(defvar paint-timer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map paint-mode-map)
    (define-key map [down-mouse-1] 'paint-timer-pen-down)
    (define-key map [drag-mouse-1] 'paint-timer-pen-up)
    (define-key map [mouse-1] 'paint-timer-pen-up)
    map)
  "A keymap used when `paint-motion-type' is a `timer'.")

(defvar paint-track-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map paint-mode-map)
    (define-key map [down-mouse-1] 'paint-track-pen-down)
    map)
  "A keymap used when `paint-motion-type' is a `track'.")

(easy-menu-define paint-menu paint-mode-map
  "A paint mode menu"
  '("Paint"
    ["Eraser" paint-toggle-erase :style toggle :selected paint-eraser-mode]
    ["Load" paint-load t]
    ["Save" paint-save t]))

(defvar paint-motion-type-alist
  '((timer . paint-timer-mode-map)
    (track . paint-track-mode-map)))

(defvar paint-foreground-color "black"
  "*Foreground color of xbm images.")
(defvar paint-background-color "white"
  "*Background color of xbm images.")

(defvar paint-eraser-mode nil)
(defvar paint-eraser-size 5)

(defun paint-mode ()
  "Paint Mode.

track motion map:
\\{paint-track-mode-map}

timer motion map:
\\{paint-timer-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map (symbol-value
		  (assoc-default
		   paint-motion-type paint-motion-type-alist
		   'eq paint-motion-default-type)))
  (setq buffer-read-only t)
  (setq cursor-type nil)
  (setq mode-name "Paint")
  (setq major-mode 'paint-mode))

(defun paint-show-menu (event)
  (interactive "@e")
  (let ((ret (paint-menu event)))
    (when ret
      (call-interactively (lookup-key paint-menu (apply 'vector ret))))))

(defun paint-window-edge (edges key)
  (let ((alist '((left		. 0)
		 (top		. 1)
		 (right		. 2)
		 (buttom	. 3))))
    (nth (cdr (assq key alist)) edges)))

(defun paint-default-size (&optional win)
  (let ((edges (window-inside-pixel-edges win)))
    (cons (- (paint-window-edge edges 'right)
	     (paint-window-edge edges 'left)
	     8)
	  (- (paint-window-edge edges 'buttom)
	     (paint-window-edge edges 'top)
	     8))))

(defun paint (width height &optional data)
  "Run a paint tool.
If DATA is non-nil, use that data for painting.
Otherwise, create a new bitmap image.

WIDTH and HEIGHT means bitmap image's width and height.
WIDTH should be multiple of eight number.

On interactive use, WIDTH is a round to multiple of eight number."
  (interactive
   (let* ((size (paint-default-size))
	  (width (read-number "width: " (car size)))
	  (height (read-number "height: " (cdr size))))
     (list (- width (% width 8))
	   height)))
  (unless (% width 8)
    (error "width should be multiple of eight"))
  (if data
      (unless (= (* width height) (length data))
	(error "data size is not width * height"))
    (setq data (make-bool-vector (* width height) nil)))
  (switch-to-buffer (get-buffer-create paint-buffer-name))
  (paint-mode)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (remove-images (point-min) (point-max))
    (setq paint-image
	  (apply 'create-image
		 data
		 'xbm t
		 :width width
		 :height height
		 :relief paint-image-releaf
		 :pointer 'arrow
		 (if paint-bit-fliped-system-p
		     (list :foreground paint-background-color
			   :background paint-foreground-color)
		   (list :foreground paint-foreground-color
			 :background paint-background-color))))
    (put-image paint-image (point-min)))
  (setq paint-eraser-mode nil)
  (paint-update))

(defsubst paint-image-property (prop)
  (plist-get (cdr paint-image) prop))

(defsubst paint-pos (x y)
  (cons x y))

(defsubst paint-pos-x (pos)
  (car pos))

(defsubst paint-pos-y (pos)
  (cdr pos))

(defun paint-mouse-in-window-p (mouse-pxl-pos window)
  (let ((edges (window-inside-pixel-edges window))
	(frame (car mouse-pxl-pos))
	(pos (cdr mouse-pxl-pos)))
    (and (eq (window-frame window) frame)
	 (<= (paint-window-edge edges 'left)	(paint-pos-x pos))
	 (>= (paint-window-edge edges 'right)	(paint-pos-x pos))
	 (<= (paint-window-edge edges 'top)	(paint-pos-y pos))
	 (>= (paint-window-edge edges 'buttom)	(paint-pos-y pos)))))
  
(defun paint-in-canvas-p (pos &optional mouse-position)
  (and 
   (< (paint-pos-x pos) (paint-image-property :width))
   (< (paint-pos-y pos) (paint-image-property :height))
   (or (null mouse-position)
       (find-if (lambda (win) (paint-mouse-in-window-p mouse-position win))
		(get-buffer-window-list (get-buffer paint-buffer-name))))))

;;; plot
(defsubst paint-plot (pos bit)
  (aset (paint-image-property :data)
	(+ (* (paint-image-property :width) (paint-pos-y pos))
	   (paint-pos-x pos))
	bit))

(defun paint-get (pos)
  (aref (paint-image-property :data)
	(+ (* (paint-image-property :width) (paint-pos-y pos))
	   (paint-pos-x pos))))

(defmacro paint-swap (m n)
  (let ((v (gensym)))
    `(let ((,v ,m))
       (setq ,m ,n
	     ,n ,v))))
  
(defun paint-plot-line (pos0 pos1 bit &optional size)
  ;; Bresenham's line algorithm
  ;; see http://en.wikipedia.org/wiki/Bresenham's_line_algorithm
  (let* ((x0 (paint-pos-x pos0))
	 (y0 (paint-pos-y pos0))
	 (x1 (paint-pos-x pos1))
	 (y1 (paint-pos-y pos1))
	 (steep (> (abs (- y1 y0)) (abs (- x1 x0)))))
    (when steep
      (paint-swap x0 y0)
      (paint-swap x1 y1))
    (when (> x0 x1)
      (paint-swap x0 x1)
      (paint-swap y0 y1))
    (let* ((dx (- x1 x0))
	   (dy (abs (- y1 y0)))
	   (err (/ (- (1+ dx)) 2))
	   (y y0)
	   (ystep (if (< y0 y1) 1 -1)))
      (loop for x from x0 to x1 do
	    (let ((xx x)
		  (yy y))
	      (when steep
		(paint-swap xx yy))
	      (if size
		  (paint-plot-square (paint-pos xx yy) (paint-pos (+ xx size) (+ yy size)) bit t)
		(paint-plot (paint-pos xx yy) bit)))
	    (setq err (+ err dy))
	    (if (>= err 0)
		(setq y (+ y ystep)
		      err (- err dx)))))))

(defun paint-plot-square (pos0 pos1 bit &optional fill-p)
  (if fill-p
      (loop for y from (paint-pos-y pos0) to (paint-pos-y pos1) do
	    (loop for x from (paint-pos-x pos0) to (paint-pos-x pos1) do
		  (paint-plot (paint-pos x y) bit)))
    (paint-plot-line pos0 (paint-pos (paint-pos-x pos0) (paint-pos-y pos1)) bit)
    (paint-plot-line pos0 (paint-pos (paint-pos-x pos1) (paint-pos-y pos0)) bit)
    (paint-plot-line pos1 (paint-pos (paint-pos-x pos1) (paint-pos-y pos0)) bit)
    (paint-plot-line pos1 (paint-pos (paint-pos-x pos0) (paint-pos-y pos1)) bit)))

(defun paint-erase-line (pos0 pos1)
  (let ((n (/ paint-eraser-size 2)))
    (setq pos0 (paint-pos (max (- (paint-pos-x pos0) n) 0) (max (- (paint-pos-y pos0) n) 0)))
    (setq pos1 (paint-pos (max (- (paint-pos-x pos1) n) 0) (max (- (paint-pos-y pos1) n) 0)))
    (paint-plot-line pos0 pos1 nil paint-eraser-size)))

(defun paint-update ()
  (set-buffer paint-buffer-name)
  (clear-image-cache))
;;   (erase-buffer)
;;   (insert-image paint-image))


(defun paint-toggle-erase ()
  "Toggle erasing mode."
  (interactive)
  (setq paint-eraser-mode (not paint-eraser-mode)))

;;; pen with track-mouse
;; mouse tracking is not performed on meadow when there is a cursor on the image. why?
(defun paint-track-pen-down (event)
  (interactive "e")
  (paint-track-pen event))

(defun paint-track-pen (start-event)
  (track-mouse
    (let (event)
      (while (progn
	       (setq event (read-event))
	       (mouse-movement-p event))
	(let ((pos0 (posn-x-y (event-end start-event)))
	      (pos1 (posn-x-y (event-end event))))
	  (when (and (paint-in-canvas-p pos0)
		     (paint-in-canvas-p pos1 (mouse-pixel-position)))
	    (if paint-eraser-mode
		(paint-erase-line pos0 pos1)
	      (paint-plot-line pos0 pos1 t))
	    (paint-update)))
	(setq start-event event)))))
    
;;; pen with timer
(defun paint-timer-pen-down (event)
  (interactive "e")
  (if paint-timer
      (paint-timer-pen-cancel)
    (setq paint-timer-pen-last-pos (posn-x-y (event-start event)))
    (setq paint-timer
	  (run-at-time paint-timer-idle-delay
		       paint-timer-idle-delay
		       'paint-timer-pen-drag
		       (window-inside-pixel-edges
			(get-buffer-window (get-buffer paint-buffer-name)))))))

(defun paint-timer-pen-up (event)
  (interactive "e")
  (timer-event-handler paint-timer)
  (paint-timer-pen-cancel))

(defun paint-timer-pen-drag (edges)
  (let* ((pxl-pos (mouse-pixel-position))
	 (pos (cdr pxl-pos)))
    (setq pos (paint-pos (- (paint-pos-x pos)
			    (paint-window-edge edges 'left))
			 (- (paint-pos-y pos)
			    (paint-window-edge edges 'top))))
    (when (and (paint-in-canvas-p paint-timer-pen-last-pos)
	       (paint-in-canvas-p pos pxl-pos))
      (if paint-eraser-mode
	  (paint-erase-line paint-timer-pen-last-pos pos)
	(paint-plot-line paint-timer-pen-last-pos pos t))
      (paint-update))
    (setq paint-timer-pen-last-pos pos)))

(defun paint-timer-pen-cancel ()
  (cancel-timer paint-timer)
  (setq paint-timer nil)
  (setq paint-timer-pen-last-pos nil))

;;; save & load
(defun paint-save (file)
  "Save drawd bitmap to a FILE."
  (interactive "FFile name: ")
  (let ((coding-system-for-write
	 (coding-system-change-eol-conversion
	  default-buffer-file-coding-system 'unix))
	(name (file-name-sans-extension (file-name-nondirectory file)))
	(data ))
    (with-temp-file file
      (insert (format "#define %s_width %d\n"
		      name (paint-image-property :width)))
      (insert (format "#define %s_height %d\n"
		      name (paint-image-property :height)))
      (insert (format "static unsigned char %s_bits[] = {\n"
		      name))
      (let ((x 0))
	(dotimes (n (length (paint-image-property :data)))
	  ;; byte of xbm bits ordered from low to heigh.
	  (setq x (logior x
			  (lsh (if (aref (paint-image-property :data) n) 1 0)
			       (% n 8))))
	  (when (= (% n 8) 7)
	    (insert (format "0x%02x, " x))
	    (setq x 0))))
      (delete-backward-char 2)
      (insert "\n};"))))

(defun paint-load (file)
  "Load bitmap from a FILE and run a `paint' with that data."
  (interactive "fFile name:")
  (let (width height data)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (setq width (and (re-search-forward "width \\([0-9]+\\)" nil t)
		       (string-to-number (match-string 1))))
      (setq height (and (re-search-forward "height \\([0-9]+\\)" nil t)
			(string-to-number (match-string 1))))
      (unless (and width height)
	(error "file is not xbm format"))
      (setq data (make-bool-vector (* width height) nil))
      (let ((n 0))
	(while (re-search-forward "0x\\([0-9a-f]+\\)" nil t)
	  (let ((x (string-to-number (match-string 1) 16)))
	    (dotimes (m 8)
	      ;; byte of xbm bits ordered from low to heigh.
	      (aset data n (not (zerop (logand x (lsh 1 m)))))
	      (setq n (1+ n)))))))
    (paint width height data)))

;;; debug
(defun paint-track-debug ()
  (interactive)
  (track-mouse
    (let (event)
      (while (progn
	       (setq event (read-event))
	       (mouse-movement-p event))
	(message "%s" (posn-x-y (event-start event)))))))

(provide 'paint)
;;; paint.el ends here

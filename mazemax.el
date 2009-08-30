;;; mazemax.el --- labyrinth generation and exploration

;; Copyright (c) 2001, 2002, 2003 Michele Bini

;; Author: Michele Bini <mibin@libero.it>
;; Created: Jul 11 2001
;; Version: 1.4
;; Keywords: games

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA


;;; Commentary:

;; mazemax.el lets you to play around with mazes in an Emacs buffer.

;; You can either use a preexisting buffer and turn on
;; mazemax-ra-minor-mode in it or run M-x mazemax to generate a random
;; maze and play with it immediately.

;; Here is a sample maze.

;; To test the minor mode you can move the cursor inside
;; this little maze and activate 'mazemax-ra-mode': further cursor
;; motion is limited to free (generally white space) cells.

;; 888888888888888888888888888888888888888888888888888888888888
;; 8                      8    8               8          8   8
;; 8 8888888888  8888888 88 8888 88 8 8 8 8  888 88 8  88 8 888
;; 8  8    8    88 8      8 8 8   8 8 888888  8   8888  8   8 8
;; 8 88888   8 88     8 8   8 88 88 8 8   8  88 8 8    8888 8 8
;; 8 8   88888  8 8 8 8 8 8   8  8  88888       8 888888      8
;; 8 8 8    8  88 8 88888888  8888888     88  8 8 8      8 88 8
;; 8 8 888     8  8 8        88  8  88888 8  888888888888888  8
;; 8 8 8   8 8 88888888 8888       88     88  8   8           8
;; 888888 888888     8  8    88 8   88888 8  8888 888888888 888
;; 8  8   8  8 88888 8888888  8 8 8 8 8 8 88 8 8  8         8 8
;; 8 8888888 8 8           88 8 8 8   8    888       8 8 8    8
;; 8   8 8   8 88888888888  8 888888888888   8 8 8 8 8 888888 8
;; 8  88 88              88 8   8     8 8   8888 88888 8      8
;; 8 88  8  8 8  8 88 88  8   888 88888 8 8    888   88888888 8
;; 8 8   8 88888 8  8 8  8888       8 8 888 8    8 8   8 8    8
;; 8 8 8    8  8 8888888  8 8 8888888 8     8 8  8 8 8 8 8888 8
;; 8 888888   88 8 8  8  88 8     8 8 8  88888888888 8    8 8 8
;; 8 8  8 8 8  8 8       8    8 8   8 88 8   8 8   8 8 8 88   8
;; 8 88 8 8 8 88 888 888888 8888888 8 8  8 888 88 8888 8  888 8
;; 8        8  8  8           8                   8    8      8
;; 888888888888888888888888888888888888888888888888888888888888

;; The generated mazes, like the one above, are guaranteed to
;; solvable.  This means that any free cell of the maze should be
;; reachable from any other.

;; If you find the generated mazes too easy, try the interactive
;; command mazemax-large.  This will create a maze larger than the
;; buffer window (generation may take some time on some systems, but
;; byte compilation helps).

;; Faces and characters used by the generator are customizable, via
;; M-x customize-group mazemax.

;;; Implementation notes:

;; The default maze generator algorithm employed, mazemax-parallel, is
;; designed to have roughly O(n*log(sqrt(n))) time complexity (where n
;; is the area of the maze to be generated).  During maze generation,
;; a list of candidate growing points is updated until all valid
;; growing points are exhausted (growing points are considered valid
;; as long as they do not violate the maze solubility rule).

;; For simplicity, none of the minor modes have an indicator in the
;; mode-line.

;;; History:
;; 2003-06-10  Michele Bini  <mibin@libero.it>
;; 
;;   * mazemax.el: XEmacs compatibility fix, initially suggested by
;;   Bulia Byak, on the Emacs Wiki.
;;   * mazemax-readjust-view-mode: New mode, for automatically
;;   readjusting the displayed portion of the buffer during
;;   exploration.
;; 
;; 2002-03-03  Michele Bini  <mibin@libero.it>
;;   * mazemax.el (mazemax): Use switch-to-buffer instead of
;;   pop-to-buffer.
;;   Renamed "mazemax-ra-minor-" and "mazemax-wm-minor-", into
;;   "mazemax-ra-" and "mazemax-wm-", respectively.
;;   (mazemax-ra-rules): Renamed from mazemax-ra-access-list.
;; 
;; 2001-12-03  Michele Bini <mibin@libero.it>
;;   * mazemax.el: A few documentation clean ups.
;; 
;; 2001-12-02  Michele Bini <mibin@libero.it>
;;   * mazemax.el: First upload on the Emacs Wiki.

;;; Code:

(defgroup mazemax nil
  "Labyrinth generation and exploration."
  :group 'games
  :prefix 'mazemax-)

(defvar mazemax-pitch nil)
(make-variable-buffer-local 'mazemax-pitch)
(defvar mazemax-height nil)
(make-variable-buffer-local 'mazemax-height)
(defvar mazemax-width nil)
(make-variable-buffer-local 'mazemax-width)

(defvar mazemax-ra-mode nil)
(make-variable-buffer-local `mazemax-ra-mode)

(defcustom mazemax-ra-rules (list "\\w")
  "Describes which portions of the buffer are accessible.
Odd elements of the list determine forbidden areas.  Even ones
describe accessible ones.  The last entry evaluated to true determines
accessibility.  Each entry can either a regexp, a function or nil.
Regexps are used in `looking-at' calls.  A function is allowed to
alter the match data but not to move the current point, so you should
use `save-excursion' if necessary.  nil in an entry means to evaluate it as
true and to skip to the next entry.  When the variable itself is nil
every portion is accessible.

This variable is used by `mazemax-ra-access-p'."
  :type '(repeat regexp)
  :group 'mazemax)

(defvar mazemax-ra-home-marker nil)

(defun mazemax-ra-access-p ()
  "Return true if the current position is accessible.

See also: `mazemax-ra-rules'.

For clarity the algorithm employed is the following:
  (let ((ok t)  (l mazemax-ra-rules))
    (save-match-data
      (while
	  (cond
	   ((not l) nil)
	   ((stringp (car l)) (looking-at (car l)))
	   ((functionp (car l)) (apply (car l)))
	   ((null (car l)) t))
	(setq ok (not ok))
	(setq l (cdr l))))
    ok))"
  (let ((ok t)  (l mazemax-ra-rules))
    (save-match-data
      (while
	  (cond
	   ((not l) nil)
	   ((stringp (car l)) (looking-at (car l)))
	   ((functionp (car l)) (apply (car l)))
	   ((null (car l)) t))
	(setq ok (not ok))
	(setq l (cdr l))))
    ok))

(defun mazemax-ra-after-command-patch ()
  (if (mazemax-ra-access-p)
      (set-marker mazemax-ra-home-marker (point))
    (goto-char (marker-position mazemax-ra-home-marker))))

(defun mazemax-ra-mode (&optional arg)
  "Toggle mazemax restricted access mode in the current buffer.

Portions of the buffer, as specified by `mazemax-ra-rules', are made
inaccessible via interactive commands.

With prefix ARG, turn the mode on if ARG is positive."
  (interactive "P")
  (cond
   ((null arg)
    (if mazemax-ra-mode (turn-off-mazemax-ra-mode)
      (turn-on-mazemax-ra-mode)))
   ((> (prefix-numeric-value arg) 0)
    (turn-on-mazemax-ra-mode))
   (t (turn-off-mazemax-ra-mode))))

(defun turn-on-mazemax-ra-mode ()
  (make-local-variable 'mazemax-ra-home-marker)
  (setq mazemax-ra-home-marker (set-marker (make-marker) (point)))
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'mazemax-ra-after-command-patch nil t)
  (setq mazemax-ra-mode t)
  (run-hooks 'mazemax-ra-mode-hook))

(defun turn-off-mazemax-ra-mode ()
  (remove-hook 'post-command-hook 'mazemax-ra-after-command-patch t)
  (setq mazemax-ra-mode nil))

(defvar mazemax-wm-mode nil)
(make-variable-buffer-local `mazemax-wm-mode)

(defvar mazemax-wm-mode-map nil)
(unless mazemax-wm-mode-map
  (let ((k (make-sparse-keymap)))
    (define-key k [up] 'mazemax-wm-previous-line)
    (define-key k [down] 'mazemax-wm-next-line)
    (define-key k [left] 'mazemax-wm-previous-char)
    (define-key k [right] 'mazemax-wm-next-char)
    (setq mazemax-wm-mode-map k)))

(defun mazemax-wm-redefine-keys ()
  "Redefine the wrapped motion keys.
The new keys are not saved and thus are active
only in the current Emacs session."
  (interactive)
  (let ((k (make-sparse-keymap)))
    (define-key k (read-key-sequence "Press [up] key: ")
      'mazemax-wm-previous-line)
    (define-key k (read-key-sequence "Press [down] key: ")
      'mazemax-wm-next-line)
    (define-key k (read-key-sequence "Press [left] key: ")
      'mazemax-wm-previous-char)
    (define-key k (read-key-sequence "Press [right] key: ")
      'mazemax-wm-next-char)
    (setq mazemax-wm-mode-map k)
    (setcdr (assq 'mazemax-wm-mode minor-mode-map-alist)
	    k)))

(unless (assq 'mazemax-wm-mode minor-mode-map-alist)
  (setq minor-mode-map-alist
	(cons (cons 'mazemax-wm-mode
		    mazemax-wm-mode-map)
	      minor-mode-map-alist)))

(defun mazemax-wm-previous-line ()
  (interactive)
  (let* ((p (mazemax-index-pair))
	 (x (car p))
	 (y (cdr p)))
    (setq y (- y 1))
    (if (< y 0) (setq y (+ mazemax-height y)))
    (goto-char (mazemax-index x y))))

(defun mazemax-wm-next-line ()
  (interactive)
  (let* ((p (mazemax-index-pair))
	 (x (car p))
	 (y (cdr p)))
    (setq y (+ y 1))
    (if (>= y mazemax-height)
	(setq y (- y mazemax-height)))
    (goto-char (mazemax-index x y))))

(defun mazemax-wm-previous-char ()
  (interactive)
  (let* ((p (mazemax-index-pair))
	 (x (car p))
	 (y (cdr p)))
    (setq x (- x 1))
    (if (< x 0) (setq x (+ x mazemax-width)))
    (goto-char (mazemax-index x y))))

(defun mazemax-wm-next-char ()
  (interactive)
  (let* ((p (mazemax-index-pair))
	 (x (car p))
	 (y (cdr p)))
    (setq x (+ x 1))
    (if (>= x mazemax-width)
	(setq x (- x mazemax-width)))
    (goto-char (mazemax-index x y))))

(defun mazemax-wm-mode (&optional arg)
  "Toggle the wrapped motion mode in the current buffer.

Defines keys for wrapped motion.  E.g. if you try to move the point
beyond the last character of a line, it will be warped to the
beginning of that line.

This minor mode is meant for buffers created via `mazemax' or alike
functions.

With prefix ARG, turn the mode on if ARG is positive."
  (interactive)
  (cond
   ((null arg)
    (if mazemax-wm-mode (turn-off-mazemax-wm-mode)
      (turn-on-mazemax-wm-mode)))
   ((> (prefix-numeric-value arg) 0)
    (turn-on-mazemax-wm-mode))
   (t (turn-off-mazemax-wm-mode))))

(defun turn-on-mazemax-wm-mode ()
  (setq mazemax-wm-mode t)
  (run-hooks 'mazemax-wm-mode-hook))
(defun turn-off-mazemax-wm-mode ()
  (setq mazemax-wm-mode nil))

;;;; readjust view mode

(defvar mazemax-readjust-view-mode nil)
(make-variable-buffer-local `mazemax-readjust-view-mode)

(defun mazemax-readjust-view-mode (&optional arg)
  "Toggle the readjust view mode.

With prefix ARG, turn it on if ARG is positive."
  (interactive)
  (cond
   ((null arg)
    (if mazemax-readjust-view-mode
	(turn-off-mazemax-readjust-view-mode)
      (turn-on-mazemax-readjust-view-mode)))
   ((> (prefix-numeric-value arg) 0)
    (turn-on-mazemax-readjust-view-mode))
   (t (turn-off-mazemax-readjust-view-mode))))

(defun turn-on-mazemax-readjust-view-mode ()
  (add-hook 'post-command-hook 'mazemax-readjust-view t t)
  (setq mazemax-readjust-view-mode t))

(defun turn-off-mazemax-readjust-view-mode ()
  (remove-hook 'post-command-hook 'mazemax-readjust-view t)
  (setq mazemax-readjust-view-mode nil))

(defun mazemax-readjust-view ()
  (set-window-start
   (selected-window)
   (save-excursion
     (let ((h (window-text-height (selected-window))))
       (forward-line (/ h 2))
       (forward-line (- 1 h))
       (beginning-of-line)
       (point))))
  (let ((w (window-width))
	(c (current-column))
	(e (save-excursion
	     (end-of-line)
	     (- (current-column) 1))))
    (set-window-hscroll
     (selected-window)
     (max (min (- c (/ w 2)) (- e w)) 0))))

;;;; random maze generation

(defun mazemax-index (x y)
  (+ (point-min) (* y mazemax-pitch) x))

(defun mazemax-index-pair (&optional where)
  (let ((a (- (or where (point)) (point-min))))
    (cons
     (% a mazemax-pitch)
     (/ a mazemax-pitch))))

(defun mazemax-index-wrap (x y)
  (if (< x 0)
      (setq x (+ x mazemax-width))
    (if (>= x mazemax-width)
	(setq x (- x mazemax-width))))
  (if (< y 0)
      (setq y (+ y mazemax-height))
    (if (>= y mazemax-height)
	(setq y (- y mazemax-height))))
  (mazemax-index x y))

(defun mazemax-index-up (x y)
  (setq y (- y 1))
  (if (< y 0) (setq y (+ y mazemax-height)))
  (mazemax-index x y))

(defun mazemax-index-down (x y)
  (setq y (+ y 1))
  (if (>= y mazemax-height) (setq y (- y mazemax-height)))
  (mazemax-index x y))

(defun mazemax-index-left (x y)
  (setq x (- x 1))
  (if (< x 0) (setq x (+ x mazemax-width)))
  (mazemax-index x y))

(defun mazemax-index-right (x y)
  (setq x (+ x 1))
  (if (>= x mazemax-width) (setq x (+ x mazemax-width)))
  (mazemax-index x y))

(defun mazemax-char (x y)
  (char-after (mazemax-index x y)))

(defun mazemax-at (x y)
  (let ((i (mazemax-index-wrap x y)))
    (buffer-substring i (+ i 1))))

(defun mazemax-surroundings (x y)
  (setq x (- x 1))
  (setq y (- y 1))
  (cond
   ((and (>= x 0) (>= y 0)
 	 (< (+ x 2) mazemax-width)
 	 (< (+ y 2) mazemax-height))
    (let* ((i (mazemax-index x y))
	   (i2 (+ i mazemax-pitch))
	   (i3 (+ i2 mazemax-pitch)))
      (concat
       (buffer-substring i (+ i 3))
       (buffer-substring i2 (+ i2 3))
       (buffer-substring i3 (+ i3 3)))))
   (t
    (concat
     (mazemax-at x y)
     (mazemax-at (+ x 1) y)
     (mazemax-at (+ x 2) y)
     (mazemax-at x (+ y 1))
     (mazemax-at (+ x 1) (+ y 1))
     (mazemax-at (+ x 2) (+ y 1))
     (mazemax-at x (+ y 2))
     (mazemax-at (+ x 1) (+ y 2))
     (mazemax-at (+ x 2) (+ y 2))))
   ;;(t "CCCCCCCCC")
   ((and (< x 0) (< y 0))
    "CCCC  C  ")
   ((and (< x 0) (< (+ y 2) mazemax-height))
    "C  C  C  ")
   ((and (< (+ x 2) mazemax-width) (< y 0))
    "CCC      ")
   ((and (>= (+ x 2) mazemax-width) (< y 0))
    "CCC  C  C")
   ((and (>= (+ y 2) mazemax-height) (< x 0))
    "  C  CCCC")
   ((and (>= (+ x 2) mazemax-width) (< (+ y 2) mazemax-height))
    "  C  C  C")
   ((and (>= (+ y 2) mazemax-height) (< (+ x 2) mazemax-width))
    "      CCC")
   ((and (>= (+ x 2) mazemax-width) (>= (+ y 2) mazemax-height))
    "  C  CCCC")
   ;;(t "         ")
   ))

(defconst mazemax-growth-pattern
  (eval-when-compile
    (regexp-opt
     (list
      "         "
      " C       "
      "   C     "
      "     C   "
      "       C "
      "CC       "
      " CC      "
      "C  C     "
      "   C  C  "
      "     C  C"
      "  C  C   "
      "      CC "
      "       CC"
      "CCC      "
      "C  C  C  "
      "  C  C  C"
      "      CCC"
      ;;"CC C     "
      ;;"   C  CC "
      ;;"     C CC"
      ;;" CC  C   "
      ;;"CC C  C  "
      ;;"C  C  CC "
      ;;"   C  CCC"
      ;;"     CCCC"
      ;;"  C  C CC"
      ;;" CC  C  C"
      ;;"CCC  C   "
      ;;"CCCC     "
      ))))

(defun mazemax-try-grow (x y)
  (when (eq ?\  (char-after (mazemax-index x y)))
    (let ((s (mazemax-surroundings x y)))
      (and
       (not
	(string-match ".*C.*C.*C.*C.*" s))
       (string-match mazemax-growth-pattern s)))))

(defun mazemax-grow (x y)
  (goto-char (mazemax-index x y))
  (delete-char 1)
  (insert-char (car (string-to-list "C")) 1))

(defun mazemax-make-sparse-one ()
  (let ((i (* mazemax-height mazemax-width 12)))
    (while (> i 0)
      (let ((x (random mazemax-width))
	    (y (random mazemax-height)))
	(when (mazemax-try-grow x y)
	  (mazemax-grow x y)))
      (setq i (- i 1)))))

(defun mazemax-monotonic-grow (x y &optional justfirst)
  (let* ((dirs
	  (eval-when-compile
	    (vector
	     (list 0 1 2 3) (list 0 1 3 2) (list 0 2 1 3)
	     (list 0 2 3 1) (list 0 3 1 2) (list 0 3 2 1)
	     (list 1 0 2 3) (list 1 0 3 2) (list 1 2 0 3)
	     (list 1 2 3 0) (list 1 3 0 2) (list 1 3 2 0)
	     (list 2 0 1 3) (list 2 0 3 1) (list 2 1 0 3)
	     (list 2 1 3 0) (list 2 3 0 1) (list 2 3 1 0)
	     (list 3 0 1 2) (list 3 0 2 1) (list 3 1 0 2)
	     (list 3 1 2 0) (list 3 2 0 1) (list 3 2 1 0))))
	 (ndirs (length dirs))
	 (dirs (aref dirs (random ndirs)))
	 (r nil)
	 (nx 0)
	 (ny 0)
	 (more nil))
    (mapcar
     (lambda (dir)
       (cond
	((= dir 0) (setq nx x) (setq ny (- y 1)))
	((= dir 1) (setq nx (+ x 1)) (setq ny y))
	((= dir 2) (setq nx x) (setq ny (+ y 1)))
	((= dir 3) (setq nx (- x 1)) (setq ny y)))
       (if (< nx 0)
	   (setq nx (+ nx mazemax-width))
	 (if (>= nx mazemax-width)
	     (setq nx (- nx mazemax-width))))
       (if (< ny 0)
	   (setq ny (+ ny mazemax-height))
	 (if (>= ny mazemax-height)
	     (setq ny (- ny mazemax-height))))
       (cond
	((mazemax-try-grow nx ny)
	 (if (and r justfirst)
	     (setq more t)
	   (mazemax-grow nx ny)
	   (setq r (cons (cons nx ny) r))))))
     dirs)
    (if more (cons (cons x y) r) r)))

(defun mazemax-split-n (l n)
  "Get the first N elements in the list L.
If the list is smaller than N, then return the whole list.

The elements in the resulting list will appear in reversed order."
  (let ((r nil))
    (while (and l (> n 0))
      (setq r (cons (car l) r))
      (setq l (cdr l))
      (setq n (- n 1)))
    (cons r l)))

(defun mazemax-parallel-one (seedcells &optional limit)
  (or limit
      (setq limit
	    (ceiling
	     (/
	      (sqrt (* mazemax-width mazemax-height))
	      3))))
  (message "Generating maze...")
  (while seedcells
    (let* ((s (mazemax-split-n seedcells limit))
	   (f (car s))
	   (r nil)
	   (l (cdr s)))
      (while f
	(let ((c (car f)))
	  (setq f (cdr f))
	  (setq r
		(append
		 (mazemax-monotonic-grow (car c) (cdr c) t)
		 r))))
      (setq seedcells (append (mazemax-scramble r) l))))
  (message "Generating maze...done."))

(defun mazemax-draw-borders ()
  (let ((n 0))
    (setq n mazemax-width)
    (while (> n 0)
      (setq n (- n 1))
      (mazemax-grow n 0)
      (mazemax-grow n (- mazemax-height 1)))
    (setq n (- mazemax-height 1))
    (while (> n 1)
      (setq n (- n 1))
      (mazemax-grow 0 n)
      (mazemax-grow (- mazemax-width 1) n))))

(defun mazemax-iterate (func)
  "Apply FUNC to each x y position of the current buffer."
  (let ((h mazemax-height))
    (while (> h 0)
      (setq h (- h 1))
      (let ((w mazemax-width))
	(while (> w 0)
	  (setq w (- w 1))
	  (apply func w h '()))))))

(defun mazemax-non-free ()
  "Return a list of all the non-free cells in the current buffer."
  (let ((mazemax-non-free (list)))
    (mazemax-iterate
     (lambda (x y)
       (unless (eq ?\  (char-after (mazemax-index x y)))
	 (setq mazemax-non-free
	       (cons (cons x y) mazemax-non-free)))))
    mazemax-non-free))

(defun mazemax-scramble (l)
  "Randomly permutate the elements in list L."
  (mapcar
   'car
   (sort
    (mapcar
     (lambda (e)
       (cons e (random)))
     l)
    (lambda (a b)
      (> (cdr a) (cdr b))))))

(defun mazemax-large (&optional wrap x y win-width win-height)
  "Call `mazemax' but with larger values for width and height.

The optional argument WRAP behaves like in `mazemax'.
The ratios (relative to the emacs frame size) for the larger values
are provided by X and Y (defaulting to 2.5).

The optional arguments WIN-WIDTH and WIN-HEIGHT behave like in the
function `mazemax'."
  (interactive)
  (or x (setq x 2.5))
  (or y (setq y 2.5))
  (mazemax
   wrap
   (ceiling (* (frame-width) x))
   (ceiling (* (frame-height) y))
   win-width win-height))

;; (mazemax-large nil nil nil 0 0)

(defface mazemax-face
  '((t :background "yellow" :foreground "red" :inverse-video t))
  "Face for maze cells."
  :group 'mazemax)

(defcustom mazemax-char "C"
  "Character for maze cells.

It should be compatible with `mazemax-ra-rules'."
  :group 'mazemax :type 'string)

(defface mazemax-free-face
  '((t :background "blue"))
  "Face for free cells."
  :group 'mazemax)

(defcustom mazemax-free-char " "
  "Character for maze cells that are \"free\" (accessible).

It should be compatible with `mazemax-ra-rules'."
  :group 'mazemax :type 'string)

(defun mazemax-goto-random-free-point (&optional return-value)
  "Goto a random but accessible point in the map.

If the optional argument return-value is not nil, the coordinates of
the choosen point are returned."
  (interactive)
  (let (x y)
    (while
	(progn
	  (setq x (random mazemax-width))
	  (setq y (random mazemax-height))
	  (goto-char (mazemax-index x y))
	  (not (eq ?\  (char-after)))))
    (when return-value (cons x y))))

(defun mazemax (&optional wrap width height win-width win-height)
  "Generate a random maze.
The maze will be created in a buffer named *maze*.

If WRAP is non-nil, a tileable maze will be created.
The generated field will have the size of the current window,
or that specified via the WIDTH and HEIGHT arguments, if specified.

The optional arguments WIN-WIDTH and WIN-HEIGHT specify the preferred
size of the window used to display the maze."
  (interactive)
  (let ((mazemax-buffer (get-buffer-create "*maze*")))
    (save-excursion
      (set-buffer mazemax-buffer)
      (kill-all-local-variables)
      (let ((buffer-read-only nil))
	(erase-buffer)
	(let ((h (max 5 (or height (window-text-height))))
	      (w (max 5 (or width (- (window-width) 1)))))
	  (let ((h h)
		(s (make-string w ?\ )))
	    (while (> h 0)
	      (insert s)
	      (if (> h 1) (insert "\n"))
	      (setq h (- h 1))))
	  (goto-char (point-min))
	  (setq mazemax-pitch (+ w 1))
	  (setq mazemax-width w)
	  (setq mazemax-height h)
	  (if (not wrap)
	      (progn
		(mazemax-draw-borders)
		(mazemax-parallel-one
		 (mazemax-scramble
		  (mazemax-non-free))))
	    (mazemax-parallel-one
	     (list (cons 0 0))))
	  (goto-char (point-min))
	  (put-text-property 0 1 'face 'mazemax-face mazemax-char)
	  (put-text-property
	   0 1 'face 'mazemax-free-face mazemax-free-char)
	  (while (not (eobp))
	    (cond
	     ((looking-at "C")
	      (replace-match
	       mazemax-char nil t))
	     ((looking-at " ")
	      (replace-match
	       mazemax-free-char nil t))
	     (t (forward-char))))
	  ))
      (turn-on-mazemax-ra-mode)
      (turn-on-mazemax-wm-mode)
      (turn-on-mazemax-readjust-view-mode)
      (setq buffer-read-only t
	    truncate-lines t)
      (when (boundp 'cursor-type)
	(make-local-variable 'cursor-type)
	(setq cursor-type 'block)))
    (when (and (functionp 'delete-other-windows)
	       (or (and win-width (> win-width (window-width)))
		   (and win-height (> win-height (window-text-height)))))
      (delete-other-windows))
    (when win-width
      (let ((width (window-width))
	    (win-width
	     (cond
	      ((boundp 'window-min-width)
	       (max window-min-width win-width))
	      (t win-width)))
	    ;;(window-min-width 0)
	    )
	(cond
	 ((not width))
	 (nil
	  (< width win-width)
	  (enlarge-window (- win-width width) t))
	 (nil
	  (< win-width width)
	  (shrink-window (- width win-width) t))
	 ((> width win-width)
	  (split-window nil win-width t)))))
    (when win-height
      (let ((height (window-text-height))
	    (win-height
	     (cond
	      ((boundp 'window-min-height)
	       (max window-min-height win-height))
	      (t win-height)))
	    ;;(window-min-height 0)
	    )
	(cond
	 ((not height))
	 ((> height win-height)
	  ;; add one line for the mode-line
	  (split-window nil (+ win-height 1)))
	 (nil
	  (< height win-height)
	  (enlarge-window (- win-height height)))
	 (nil
	  (< win-height height)
	  (shrink-window (- height win-height))))))
    (switch-to-buffer mazemax-buffer)
    (mazemax-goto-random-free-point)))

;;;; draft code

(defun mazemax-beautify-buffer ()
  (interactive)
  (let ((v (vector "A" "E" "I" "O" "U" "K" "P" "T" "S"))
	(buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\w" nil t)
	(replace-match (aref v (random (length v))))))))

(provide 'mazemax)
;;; mazemax.el ends here

;;; framemove.el --- directional frame selection routines
;;
;; Copyright (C) 2010
;;
;; Author: Trey Jackson (bigfaceworm@gmail.com)
;; Created: February 14, 2010
;; Keywords: frame, movement, convenience
;;
;; This file is not (yet) a part of GNU Emacs.
;;
;; Very much like the windmove package, only for frames.
;; Provide a simple set of keystrokes to move the input/focus
;; between windows.
;;
;; Version 0.9
;;
;; This software is licensed under the GPL version 3.
;;
;; To install:
;;   (require 'framemove)
;;   (framemove-default-keybindings)
;;
;; If you want to integrate framemove and windmove
;; You can omit the call to 'framemove-default-keybindings
;; And instead do:
;;    (require 'framemove)
;;    (windmove-default-keybindings)
;;    (setq framemove-hook-into-windmove t)
;; 
;; Compatibility: GNU Emacs 22.x, 23.x
;;

(defvar framemove-hook-into-windmove nil
  "When non-nil, try moving frames if moving windows fails.")

(defun fm-frame-bbox (frame)
  ;; eval b/c when things are beyond borders, you get
  ;; (+ -11) weirdness
  (let ((yl (eval (frame-parameter frame 'top)))
        (xl (eval (frame-parameter frame 'left))))
    (list xl
          yl
          (+ xl (frame-pixel-width frame))
          (+ yl (frame-pixel-height frame)))))

(defun fm-opposite (dir)
  (cdr (assq dir '((left . right) (right . left) (up . down) (down . up)))))

(defun fm-frame-coord (frame-or-box dir)
  (nth (cdr (assq dir '((left . 0) (up . 1) (right . 2) (down . 3))))
       (if (framep frame-or-box)
           (fm-frame-bbox frame-or-box)
         frame-or-box)))

(defun fm-frame-is-completly-to-dir-of (refframe dir otherframe)
  (cond
   ((eq refframe otherframe)
    nil)
   ((memq dir '(left up))
    (< (fm-frame-coord refframe (fm-opposite dir)) (fm-frame-coord otherframe dir)))
   ((memq dir '(right down))
    (> (fm-frame-coord refframe (fm-opposite dir)) (fm-frame-coord otherframe dir)))
   (t (error "Invalid direction of movement: %s" dir))))

(defun fm-frame-is-to-dir-of (refframe dir otherframe)
  (cond
   ((not (eq (frame-parameter refframe 'display) (frame-parameter otherframe 'display)))
      nil)
   ((eq refframe otherframe)
    nil)
   ((memq dir '(left up))
    (< (fm-frame-coord refframe dir) (fm-frame-coord otherframe dir)))
   ((memq dir '(right down))
    (> (fm-frame-coord refframe dir) (fm-frame-coord otherframe dir)))
   (t (error "Invalid direction of movement: %s" dir))))

(defun fm-absolute-coords-of-position (position)
  (let ((rel-x-y (fm-frame-relative-coordinates position))
        (frame-bbox (fm-frame-bbox (window-frame (posn-window position)))))
    (cons (+ (car frame-bbox) (car rel-x-y))
          (+ (cadr frame-bbox) (cdr rel-x-y)))))

(defun fm-frame-relative-coordinates (position)
  "Return frame-relative coordinates from POSITION."
  (let* ((x-y (posn-x-y position))
         (window (posn-window position))
         (edges (window-inside-pixel-edges window)))
    (cons (+ (car x-y) (car edges))
          (+ (cdr x-y) (cadr edges)))))

(defun fm-project (coord frame dir)
  "project COORD in direction DIR to edge of FRAME"
  (if (memq dir '(up down))
      (cons (car coord)
            (fm-frame-coord frame dir))
    (cons (fm-frame-coord frame dir)
          (cdr coord))))


(defun fm-next-frame (dir)
  "move focus to next frame in direction (from currently focused frame)"
  (interactive (list
                (intern (completing-read "Which direction: " '("up" "down" "left" "right") nil t))))
  (let* ((thisframe (selected-frame))
         (current-coords (fm-absolute-coords-of-position (posn-at-point)))
         (coords-projected-in-dir (fm-project current-coords thisframe dir))
         (possible-frames
          (sort
           (remove-if-not
            '(lambda (f) (fm-frame-is-to-dir-of f dir thisframe))
            (visible-frame-list))
           '(lambda (f1 f2) (fm-frame-is-to-dir-of f1 (fm-opposite dir) f2)))))
    (if possible-frames
        (let ((frames-in-line-of-cursor
               ;; try to find frame in line with cursor
               (remove-if-not
                '(lambda (f) (fm-coord-in-range current-coords dir f))
                possible-frames))
              (frames-in-line-of-frame
               ;; find frame that overlaps current frame
               ;; need to sort by distance from cursor
               (sort
                (remove-if-not
                 '(lambda (f) (fm-range-overlap thisframe f dir))
                 possible-frames)
                '(lambda (f1 f2)
                   (< (fm-dist-from-coords coords-projected-in-dir f1)
                      (fm-dist-from-coords coords-projected-in-dir f2))))))
          (select-frame-set-input-focus
           (or (car frames-in-line-of-cursor)
               (car frames-in-line-of-frame)
               (car possible-frames))))
      (error "No frame in that direction"))))

(defun fm-dist-from-coords (coord frame)
  "distance from coord to the bbox of the frame"
  (let* ((x (car coord))
         (y (cdr coord))
         (x-in-range (fm-v-in-range x (fm-bbox-range 'left frame)))
         (y-in-range (fm-v-in-range y (fm-bbox-range 'up frame)))
         (x-dist (min (abs (- x (fm-frame-coord frame 'left)))
                      (abs (- x (fm-frame-coord frame 'right)))))
         (y-dist (min (abs (- y (fm-frame-coord frame 'up)))
                      (abs (- y (fm-frame-coord frame 'down))))))
    (cond ((and x-in-range y-in-range)
           0)
          (x-in-range
           y-dist)
          (y-in-range
           x-dist)
          ((sqrt (+ (expt x-dist 2)
                    (expt y-dist 2)))))))
              
(defun fm-v-in-range (v range)
  (and (> v (car range))
       (< v (cdr range))))

(defun fm-bbox-range (dir box)
  (if (memq dir '(up down))
      (cons (fm-frame-coord box 'up)
            (fm-frame-coord box 'down))
    (cons (fm-frame-coord box 'left)
          (fm-frame-coord box 'right))))

(defun fm-range-overlap (f1 f2 dir)
  "return true if the bbox'es of the two frames overlap using coords perpendicular to dir"
  (let ((perp (if (memq dir '(up down)) 'left 'up))
        (f1box (fm-frame-bbox f1))
        (f2box (fm-frame-bbox f2)))
    (or (fm-v-in-range (fm-frame-coord f1 perp) (fm-bbox-range perp f2))
        (fm-v-in-range (fm-frame-coord f1 (fm-opposite perp)) (fm-bbox-range perp f2))
        (fm-v-in-range (fm-frame-coord f2 perp) (fm-bbox-range perp f1))
        (fm-v-in-range (fm-frame-coord f2 (fm-opposite perp)) (fm-bbox-range perp f1)))))

(defun fm-coord-in-range (coord dir frame)
  "return true if the coord can be projected in orientation of dir
onto the bbox of the frame, or more simply, is the part of the coord
perpendicular to DIR between the edges of frame perpendicular to DIR"
  (let ((n (if (memq dir '(up down)) (car coord) (cdr coord)))
        (perp (if (memq dir '(up down)) 'left 'up)))
    (and (< (fm-frame-coord frame perp) n)
         (> (fm-frame-coord frame (fm-opposite perp)) n))))

(defun fm-sort-frames-by-edge (framelist dir)
  (sort
   framelist
   (lambda (f1 f2)
     (apply (symbol-function
             (if (memq dir '(left up)) '> '<))
            (list (fm-frame-coord f1 dir) (fm-frame-coord f2 dir))))))

;;;###autoload
(defun fm-down-frame ()
  (interactive)
  (fm-next-frame 'down))
;;;###autoload
(defun fm-up-frame ()
  (interactive)
  (fm-next-frame 'up))
;;;###autoload
(defun fm-left-frame ()
  (interactive)
  (fm-next-frame 'left))
;;;###autoload
(defun fm-right-frame ()
  (interactive)
  (fm-next-frame 'right))

;;;###autoload
(defun framemove-default-keybindings (&optional modifier)
  "Set up keybindings for `framemove'.
Keybindings are of the form MODIFIER-{left,right,up,down}.
Default MODIFIER is 'meta."
  (interactive)
  (unless modifier (setq modifier 'meta))

  (global-set-key (vector (list modifier 'down))  'fm-down-frame)
  (global-set-key (vector (list modifier 'up))    'fm-up-frame)
  (global-set-key (vector (list modifier 'left))  'fm-left-frame)
  (global-set-key (vector (list modifier 'right)) 'fm-right-frame))

(defadvice windmove-do-window-select (around framemove-do-window-select-wrapper activate)
  "Let windmove do its own thing, if there is an error, try framemove in that direction."
  (condition-case err
      ad-do-it
    (error
     (if framemove-hook-into-windmove
         (fm-next-frame (ad-get-arg 0))
       (error (error-message-string err))))))

(provide 'framemove)
;;; framemove.el ends here

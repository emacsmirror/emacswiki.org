;;; eight-puzzle.el --- A simple little puzzle game

;; Copyright 2005 Wenbin Ye
;;
;; Author: wenbinye@163.com
;; Version: $Id: eight-puzzle.el,v 0.0 2005/11/25 03:26:08 Administrator Exp $
;; Keywords: game, eight puzzle
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; If you use images instead of letters, you should set the varible:
;;
;;   (setq eight-puzzle-use-image t)
;;   (setq eight-puzzle-pics-path "path/to/images/")
;;   (setq eight-puzzle-pics-name "name")
;;
;; The eight-puzzle-pics-name is the oringal image name, such as
;; "x.png". And this image split to nine part, which the first in the
;; top left corner is substitued by a blank image that has the same
;; width and height. These nine images should name in order of number,
;; such as "x0.png", "x1.png", ... "x8.png".

;; Put this file into your load-path and the following into your ~/.emacs:
;; (autoload 'eight-puzzle "eight-puzzle" nil t)

;;; Code:

(provide 'eight-puzzle)
(eval-when-compile
  (require 'cl))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(defvar eight-puzzle-goal "012345678" "The state to reach")

(defvar eight-puzzle-display " 12345678"
  "String to display. You can set like \" abcdefgh\"")

(defvar eight-puzzle-use-image nil
  "Use image or not")

(defvar eight-puzzle-pics-path
  "~/.8puzzle/")

(defvar eight-puzzle-pics-name "x")

(defvar eight-puzzle-image-sets '("a" "x" "l")
  "A set of pictures used to display")

(defvar eight-puzzle-offsets '[(0 . 1) (1 . 0) (0 . -1) (-1 . 0)]
  "Directions to move")

(defvar eight-puzzle-state "012345678" "Current state")

(defvar eight-puzzle-buffer-name "*8-puzzle*")

(defvar eight-puzzle-mode-hook nil "Hook run before enter eight-puzzle-mode")

(defvar eight-puzzle-mode-map nil
  "Local keymap for the eight puzzle game.")

(unless eight-puzzle-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [left] 'eight-puzzle-left)
    (define-key map [right] 'eight-puzzle-right)
    (define-key map [up] 'eight-puzzle-up)
    (define-key map [down] 'eight-puzzle-down)
    (define-key map "h" 'eight-puzzle-left)
    (define-key map "l" 'eight-puzzle-right)
    (define-key map "k" 'eight-puzzle-up)
    (define-key map "j" 'eight-puzzle-down)
    (define-key map "r" 'eight-puzzle-restart)
    (define-key map "q" 'eight-puzzle-quit)
    (define-key map "s" 'eight-puzzle-show-state)
    (define-key map "c" 'eight-puzzle-change-image)
    (define-key map "t" 'eight-puzzle-toggle-image)
    (setq eight-puzzle-mode-map map)))

(defun eight-puzzle-shuffle-string (str)
  (let ((len (length str)))
    (dotimes (pos len)
      (eight-puzzle-swap-char str pos (random len)))
    str))

(defun eight-puzzle-restart ()
  (interactive)
  (let ((even (eight-puzzle-evenp eight-puzzle-goal)))
    (while (progn
             (eight-puzzle-shuffle-string eight-puzzle-state)
             (not (equal (eight-puzzle-evenp eight-puzzle-state)
                         even))))
    (eight-puzzle-display-state)))

(defun eight-puzzle-evenp (str)
  (let* ((count 0)
         (pos (string-match "0" str))
         (str (concat (substring str 0 pos)
                      (substring str (1+ pos))))
         i j)
    (dotimes (i 7)
      (setq j (1+ i))
      (while (< j 8)
        (if (> (aref str i)
               (aref str j))
            (setq count (1+ count)))
        (setq j (1+ j))))
    (= (mod count 2) 0)))

(defun eight-puzzle-swap-char (str pos1 pos2)
  (let (tmp)
    (setq tmp (aref str pos1))
    (aset str pos1 (aref str pos2))
    (aset str pos2 tmp)
    str))

(defun eight-puzzle-display-state ()
  (let ((len (length eight-puzzle-state))
        (pics (concat eight-puzzle-pics-path eight-puzzle-pics-name))
        idx)
    (setq buffer-read-only nil)
    (erase-buffer)
    (dotimes (pos len)
      (setq idx (- (aref eight-puzzle-state pos) ?0))
      (insert
       (if eight-puzzle-use-image
           (propertize (format "[%c]" (aref eight-puzzle-display idx))
                       'display `(image
                                  :type png
                                  :file ,(format "%s%d.png" pics idx)
                                  :ascent center
                                  :relief 1
                                  :margin 1))
         (format "[%c]" (aref eight-puzzle-display idx)))
       (if (= (mod (+ pos 1) 3) 0)
           "\n" "")))
    (insert "\n")
    (if eight-puzzle-use-image
        (insert-image (create-image (concat pics ".png") 'png nil :relief 1)))
    (setq buffer-read-only t)
    (goto-char (point-min))
    (re-search-forward (format "[%c]" (aref eight-puzzle-display 0)))
    (goto-char
     (if eight-puzzle-use-image
         (1- (match-beginning 0))
       (match-beginning 0)))))
  
(defun eight-puzzle-gap-pos ()
  (let* ((pos (string-match "0" eight-puzzle-state))
         (col (mod pos 3))
         (row (/ pos 3)))
    (cons row col)))

(defun eight-puzzle-index (pos)
  (+ (* 3 (car pos)) (cdr pos)))

(defun eight-puzzle-check-pos (pos)
  (and (and (> (car pos) -1)
            (< (car pos) 3))
       (and (> (cdr pos) -1)
            (< (cdr pos) 3))))

(defun eight-puzzle-move (dir)
  (let* ((gap-pos (eight-puzzle-gap-pos))
        (new-pos (cons (+ (car gap-pos)
                          (car (aref eight-puzzle-offsets dir)))
                       (+ (cdr gap-pos)
                          (cdr (aref eight-puzzle-offsets dir))))))
    (if (eight-puzzle-check-pos new-pos)
        (progn
          (eight-puzzle-swap-char eight-puzzle-state
                                  (eight-puzzle-index gap-pos)
                                  (eight-puzzle-index new-pos))
          (eight-puzzle-display-state)
          (if (eight-puzzle-goalp)
              (if (y-or-n-p "You did it! Start again ")
                  (eight-puzzle-restart)
                (eight-puzzle-quit)))))))

(defun eight-puzzle-left ()
  (interactive)
  (eight-puzzle-move 0))
(defun eight-puzzle-up ()
  (interactive)
  (eight-puzzle-move 1))
(defun eight-puzzle-right ()
  (interactive)
  (eight-puzzle-move 2))
(defun eight-puzzle-down ()
  (interactive)
  (eight-puzzle-move 3))

(defun eight-puzzle-goalp ()
  (equal eight-puzzle-state eight-puzzle-goal))

(defun eight-puzzle-mode ()
  " A mode for playing `eight-puzzle'

The key bindings for eight-puzzle-mode are:

\\{eight-puzzle-mode-map}
"
  (kill-all-local-variables)
  (use-local-map eight-puzzle-mode-map)
  (setq major-mode 'eight-puzzle-mode
        mode-name "8-puzzle")
  (run-hooks 'eight-puzzle-mode-hook)
  (setq buffer-read-only t
        truncate-lines   t)
  (buffer-disable-undo (current-buffer)))

(defun eight-puzzle ()
  "Move the num to position like this:

   [ ][1][2]
   [3][4][5]
   [6][7][8]
"
  (interactive)
  (switch-to-buffer eight-puzzle-buffer-name)
  (eight-puzzle-mode)
  (eight-puzzle-restart))

(defun eight-puzzle-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun eight-puzzle-show-state ()
  (interactive)
  (message "%s\n%s" eight-puzzle-state
           eight-puzzle-goal))

(defun eight-puzzle-toggle-image (&optional arg)
  (interactive "P")
  (or arg (setq arg 0))
  (cond ((> arg 0)
         (setq eight-puzzle-use-image t))
        ((< arg 0)
         (setq eight-puzzle-use-image nil))
        ((= arg 0)
         (setq eight-puzzle-use-image
               (not eight-puzzle-use-image))))
  (clear-image-cache)
  (eight-puzzle-display-state))

(defun eight-puzzle-change-image (name)
  (interactive
     (list (completing-read
            "name: "
            eight-puzzle-image-sets)))
  (setq eight-puzzle-pics-name name)
  (clear-image-cache)
  (eight-puzzle-display-state))
  
;;; eight-puzzle.el ends here


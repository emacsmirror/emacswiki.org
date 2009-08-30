;;; poker.el --- A simple poker game for Emacs

;; Copyright (C) 2006  Marc Abramowitz

;; Author: Marc Abramowitz (http://marc.abramowitz.info)
;; Keywords: games

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

;;; Commentary:

;; 

;;; Code:

(defconst poker-ranks                '(2 3 4 5 6 7 8 9 10 j q k a))
(defconst poker-suits                '(h d s c))
(defconst poker-best-hands
  '(
    royal-flush
    straight-flush
    four-of-a-kind
    full-house
    flush
    straight
    three-of-a-kind
    two-pair
    one-pair
    ))

(defconst poker-debug                nil)

(defvar   poker-deck                 '())
(defvar   poker-hand                 '())
(defvar   poker-discard-list         '())
(defvar   poker-game-state           0)

(defun rank-of-card (card)
  (first card))

(defun rank-value (rank)
  (position rank poker-ranks))

(defun rank-value-of-card (card)
  (rank-value (rank-of-card card)))

(defun max-rank (rank1 rank2)
  (elt poker-ranks (max (rank-value rank1)
			(rank-value rank2))))

(defun card-< (card1 card2)
  (<= (rank-value-of-card card1) (rank-value-of-card card2)))

(defun max-card-2 (card1 card2)
  (cond
   ((>= (rank-value-of-card card1) (rank-value-of-card card2)) card1)
   (t card2)))

(defun max-card-in-hand (hand)
  (cond
   ((= 1 (length hand)) (car hand))
   (t (max-card-2 (car hand) (max-card-in-hand (cdr hand))))))

(defun is-straight (hand)
  (cond
   ((null hand) (error 'null-hand))
   ((= 1 (length hand)) t)
   (t
    (let* ((hi-card-1 (max-card-in-hand hand))
	   (hand-without-hi-card-1 (remove hi-card-1 hand))
	   (hi-card-2 (max-card-in-hand hand-without-hi-card-1)))
      (and (= 1 (- (rank-value-of-card hi-card-1) (rank-value-of-card hi-card-2)))
	   (is-straight hand-without-hi-card-1))))))
     
(defun suit (card) 
  (second card))

(defun eq-suit (card1 card2)
  (let ((ret (eq (suit card1) (suit card2))))
    (when poker-debug (format "(eq-suit %s %s) => %s\n" card1 card2 ret))
    ret))

(defun is-flush (hand)
  (cond
   ((null hand) (error 'null-hand))
   ((= 1 (length hand)) t)
   (t (and (eq-suit (first hand) (second hand))
	   (is-flush (cdr hand))))))

(defun is-straight-flush (hand)
  (and (is-straight hand) (is-flush hand)))

(defun is-royal-flush (hand)
  (and (is-straight-flush hand) (equal 'A (rank-of-card (max-card-in-hand hand)))))

(defun get-best-hand (hand)
  (dolist (x poker-best-hands)
    (let* ((pred-func    (intern-soft (concat "is-" (symbol-name x)))))
      (when (funcall pred-func hand) (return x)))))

(defun count-ranks (hand)
  (let* ((ranks (mapcar #'rank-of-card hand))
	 (ranks-no-dups (remove-duplicates ranks)))
    (mapcar
     (lambda (rank) (list rank (count rank ranks)))
     ranks-no-dups)))

(defun num-pairs (hand)
  (count 2 (count-ranks hand) :key #'second))

(defun is-two-pair (hand)
  (= 2 (num-pairs hand)))

(defun is-one-pair (hand)
  (= 1 (num-pairs hand)))

(defun is-three-of-a-kind (hand)
  (when (find 3 (count-ranks hand) :key #'second) t))
  
(defun is-four-of-a-kind (hand)
  (when (find 4 (count-ranks hand) :key #'second) t))

(defun is-full-house (hand)
  (and (is-three-of-a-kind hand) (is-one-pair hand)))

(defun card (rank suit)
  (list rank suit))

(defun random-rank ()
  (elt poker-ranks (random (length poker-ranks))))

(defun random-suit ()
  (elt poker-suits (random (length poker-suits))))

(defun random-card ()
  (card (random-rank) (random-suit)))

(defun full-deck ()
  (let ((ret '()))
    (dolist (suit poker-suits)
      (dolist (rank poker-ranks)
	(setf ret (cons (card rank suit) ret))))
    ret))

(defun random-deck ()
  (interactive)
  (let* ((ret             '())
 	 (random-card     '())
	 (deck            (full-deck))
	 (deck-len        (length deck)))
    (dotimes (i deck-len)
      (setf deck-len (length deck))
      (setf random-card-num (random deck-len))
      (setf random-card (elt deck (random deck-len)))
      (setf deck (remove random-card deck))
      (setf ret (cons random-card ret)))
    ret))

(defun new-deck! ()
  (setf poker-deck (random-deck)))

(defun num-cards-left-in-deck (deck)
  (length deck))

(defun take-from-deck! (num)
  (let ((ret '()))
    (dotimes (i num)
      (setf ret (push (pop poker-deck) ret)))
    (setf ret (nreverse ret))))

(defun deal-me! (num)
  (when (< (num-cards-left-in-deck poker-deck) 5)
    (new-deck!))
  (setf poker-hand (append poker-hand (take-from-deck! num))))

(defun discard! (list-of-nums)
  (when poker-debug (insert (format "list-of-nums = %s\n" list-of-nums)))
  (let* ((list-of-cards  (mapcar (lambda (x) (elt poker-hand x)) list-of-nums)))
    (when poker-debug (insert (format "list-of-cards = %s\n" list-of-cards)))
    (dolist (card list-of-cards)
      (when poker-debug (insert (format "Discarding: %s\n" card)))
      (setf poker-hand (remove card poker-hand))))
  poker-hand)

(defun goto-pos (x y)
  (goto-line y)
  (beginning-of-line)
  (forward-char x))

(defun display-card-old (card)
  (interactive)
    (insert "+---------+")
    (forward-line)
    (backward-char 11)
    (insert (format "| %-2s    %1s |" (rank-of-card card) (suit card)))
    (forward-line)
    (backward-char 11)
    (dotimes (i 3)
      (insert "|         |")
      (forward-line)
      (backward-char 11))
    (insert (format "| %1s    %-2s |" (suit card) (rank-of-card card)))
    (forward-line)
    (backward-char 11)
    (insert "+---------+"))

(defun display-card (i)       ; i is 0-based index
  (let ((card (elt poker-hand i)))
    (goto-pos (* i 13) 4) (delete-char 11) (insert "+---------+")
    (goto-pos (* i 13) 5) (delete-char 11) (insert (format "| %-2s    %1s |" (rank-of-card card) (suit card)))
    (dotimes (y 3)
      (goto-pos (* i 13) (+ y 6)) (delete-char 11) (insert "|         |"))
    (goto-pos (* i 13) 9) (delete-char 11)  (insert (format "| %1s    %2s |" (suit card) (rank-of-card card)))
    (goto-pos (* i 13) 10) (delete-char 11) (insert "+---------+")))

(defun mark-for-discard (i)   ; i is 0-based index
  (goto-pos (* i 13) 4) (delete-char 11) (insert "+---------+")
  (dotimes (y 5)
    (goto-pos (* i 13) (+ y 5)) (delete-char 11) (insert "|xxxxxxxxx|")))

(defun toggle-card-display (i)  ; i is 0-based index
  (interactive)
  (let ((discarded (find i poker-discard-list)))
    (cond
     (discarded     (display-card i))
     (t             (mark-for-discard i)))
    (setf poker-discard-list (if discarded (remove i poker-discard-list) (cons i poker-discard-list)))))

(defconst poker-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "1"           '(lambda () (interactive) (toggle-card-display 0)))
    (define-key map "2"           '(lambda () (interactive) (toggle-card-display 1)))
    (define-key map "3"           '(lambda () (interactive) (toggle-card-display 2)))
    (define-key map "4"           '(lambda () (interactive) (toggle-card-display 3)))
    (define-key map "5"           '(lambda () (interactive) (toggle-card-display 4)))
    (define-key map (kbd "RET")   '(lambda () (interactive) (discard-and-deal-new)))
    map))

(defun poker-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map poker-mode-map)
  (setq major-mode 'poker-mode)
  (setq mode-name "Poker"))

(defun display-hand (hand)
  (interactive)
  (dotimes (r 7)
    (dotimes (c 68)
      (insert " "))
    (insert "\n"))
  (dolist (card hand)
    (display-card (position card hand))))

(defun update-screen()
  (erase-buffer)
  (insert "\n\n\n")
  (display-hand poker-hand)
  (goto-char (point-max))
  (insert "\n\nYou have: " (symbol-name (get-best-hand poker-hand)) "\n\n")
  (when (= poker-game-state 1)
    (insert "Choose cards to discard with number keys and then press <Enter> to deal new ones."))
  (when (= poker-game-state 2)
    (insert "Press <Enter> to deal a new hand.")))

(defun poker-play ()
  (interactive)
  (switch-to-buffer "poker-poker")
  (poker-mode)
  (setf poker-deck (full-deck))
  (new-deck!)
  (setf poker-hand '())
  (deal-me! 5)
  (setf poker-game-state 1)
  (update-screen))

(defun discard-and-deal-new ()
  (when (= poker-game-state 2)
    (setf poker-discard-list '(0 1 2 3 4)))
  (when poker-debug (insert (format "Discard: %s\n" poker-discard-list)))
  (discard! poker-discard-list)
  (when poker-debug (insert (format "Hand: %s\n" poker-hand)))
  (deal-me! (length poker-discard-list))
  (when poker-debug (insert (format "Hand: %s\n" poker-hand)))
  (setf poker-discard-list '())
  (setf poker-game-state (if (= poker-game-state 1) 2 1))
  (update-screen))

(provide 'poker)
;;; poker.el ends here



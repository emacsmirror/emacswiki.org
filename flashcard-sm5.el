;;; flashcard-sm5.el --- SuperMemo algorithm for Emacs flashcard

;; Copyright (C) 2006  David Smith

;; Author: David Smith <davidsmith@acm.org>
;; Keywords: 

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

;; This is meant to be a replacement for the Leitner method
;; implementation already included with flashcard. It should
;; produce better results while supporting a much larger
;; database of cards.

;; The basic usage is still the same but after entering what
;; they think the answer is (or yes or no, whatever), the
;; important thing that the user must do is input a number from
;; 0 to 5 (inc) to indicate how well he or she personally
;; remembered the item. This number will be a primary factor in
;; determining the date to show the card again.

;; There are parts of this that may be wrong. I've tried to
;; label those parts clearly but someone else should really
;; read the design doc for SM5 and compare.

;; The optimal factors matrix is stored in the deck file but it
;; is kept in memory between deck files on purpose. The optimal
;; factors are optimal to your brain, not to the material you
;; are stuffing in it, so if you move to a different deck file
;; with different material your optimal factors matrix should
;; go with you. There is a problem with this, though; if you
;; drill a deck, then quit emacs, then load a different deck
;; before loading the deck you were drilling before, your
;; in-memory optimal factors matrix will not be the
;; most-current version. Suggestions for how to fix this are
;; very much appreciated.

;; From the SM5 design doc, propagation of the optimal factors
;; matrix is recommended but not implemented here yet. I would
;; like to do so but it seems like it would be quite slow and
;; the docs are slightly contradictory so I have left it out
;; for now.

;; The searching procedures used for the next card should be
;; optimized. One idea for how to make this much faster would
;; be to keep the cards in a balanced-tree (in a list). This
;; would allow keeping the cards sorted by date with fast
;; insertion and easy retrieval of the next card.

;; UPDATE: 08/14. Rereading the SM5 documentation, I've updated
;; the procedure for calculating the next interval. It seems to
;; make the interval-matrix irrelevant but it might be a
;; mistake. I'll test for a few days.

;; UPDATE: 09/01. I think I've fixed the problem with the
;; calcuation of the interval after the first time a card is
;; asked that was introduced in the previous update. Please
;; test and report. Also, I've incorporated a patch from Danien
;; Elmes for reading characters.


;;; Code:
(require 'flashcard)

(defvar flashcard-sm5-version "0.1"
  "The version string for flashcard-sm5.el")

(defgroup flashcard-method-sm5 nil
  "The SuperMemo 6 method."
  :prefix "flashcard-method-sm5-"
  :group 'flashcard-methods)

(defvar flashcard-sm5-initial-ease 10
  "The initial ease setting")
(defvar flashcard-sm5-matrix-size 20
  "The size of the matrices")
(defvar flashcard-sm5-optimal-factors nil 
  "The Optimal Factors matrix")
(defvar flashcard-sm5-interval-matrix nil
  "The Intervals matrix")
(defvar flashcard-sm5-rate 0.8
  "Optimal Factors rate of change setting")
(defvar flashcard-sm5-initial-interval 4.0
  "The initial interval in days. Note that this only applies to
the first card you ever see in the deck as this will be
constantly refined by the algorithm.")

(make-variable-buffer-local 'flashcard-sm5-optimal-factors)
(make-variable-buffer-local 'flashcard-sm5-rate)
			    
(defun flashcard-sm5-get-element (mat x y)
  (aref (aref mat x) y))

(defun flashcard-sm5-set-element (mat x y val)
  (aset (aref mat x) y val))

(defun flashcard-sm5-randomize (PI OF)
  (let* ((a 0.047)
	(b 0.092)
	(p (/ (sin (random t)) 2))
	(m (* (/ -1.0 b) (log (- 1.0 (* (/ b a) (abs p)))) (if (< 0 p) -1.0 1.0))))
    (* PI (+ 1.0 (* (- OF 1.0) (/ (+ 100.0 m) 100.0))))))

(defun flashcard-sm5-get-OF (count ease)
  (flashcard-sm5-get-element flashcard-sm5-optimal-factors count ease))

(defun flashcard-sm5-update-intervals (interval oldOF count ease quality)
  (let ((factor (flashcard-sm5-optimal-factor 
		 interval quality oldOF (flashcard-sm5-get-OF count ease))))
    (flashcard-sm5-set-element 
     flashcard-sm5-optimal-factors count ease factor)))

(defun flashcard-sm5-get-PI (count ease oldI &optional OF)
  (let ((OF (or OF (flashcard-sm5-get-OF count ease))))
    (if (eq count 0)
	(flashcard-sm5-randomize OF 1.0)
      (flashcard-sm5-randomize oldI OF))))

(defun flashcard-sm5-create-matrix ()
  (let ((matrix (make-vector flashcard-sm5-matrix-size nil)))
    (dotimes (i flashcard-sm5-matrix-size)
      (aset matrix i (make-vector flashcard-sm5-matrix-size nil)))
    matrix))

(defun flashcard-sm5-create-optimal-factors ()
  (let* ((first-row (make-vector flashcard-sm5-matrix-size flashcard-sm5-initial-interval))
	 (levels (mapcar (lambda (x) (+ 1.01 (* x 0.05))) 
			(number-sequence 0 flashcard-sm5-matrix-size)))
	 (matrix (flashcard-sm5-create-matrix)))
    (aset matrix 0 first-row)
    (dotimes (i (- flashcard-sm5-matrix-size 1))
      (aset matrix (+ 1 i) (vconcat levels)))
    matrix))

(defun flashcard-sm5-optimal-factor (oldI quality usedOF oldOF)
  (let* ((mod5 (max 1.05 (/ (1+ oldI) oldI)))
	 (mod2 (min 0.75 (/ (- oldI 1) oldI)))
	 (mod (max 0.05
		   (if (> quality 4)
		       ;; SM5 does (1+ (* (1- mod5) (- quality 4))) but
		       ;; if quality > 4, it has to be 5, which makes the above
		       ;; just equal to (1+ (1- mod5 1)) which is just mod5, sigh.
		       ;; so before anyone thinks I'm doing this wrong, there's
		       ;; five lines of comments to explain why I'm just writing mod5...
		       mod5
		     (- 1 (* (/ (- 1 mod2) 2.0) (- 4 quality))))))
	 (res (* usedOF mod)))
    (if (> quality 4)
	(if (< res oldOF)
	    (setq res oldOF))
      (if (> res oldOF)
	  (setq res oldOF)))
    (max 1.01 (+ (* res flashcard-sm5-rate) (* oldOF (- 1 flashcard-sm5-rate))))))

(defun flashcard-sm5-create-intervals (OF)
  (let* ((matrix (flashcard-sm5-create-matrix))
	 (idx 2))
    (dotimes (i flashcard-sm5-matrix-size)
      (progn
	(flashcard-sm5-set-element 
	 matrix 0 i 
	 (- 8.0 
	    (* (/ (- (flashcard-sm5-get-element OF 0 i) 1.1)
		  (- 2.5 1.1))
	       3.0)))
	(flashcard-sm5-set-element
	 matrix 1 i
	 (+ 13.0
	    (* (/ (- (flashcard-sm5-get-element OF 0 i) 1.1)
		  (- 2.5 1.1))
	       8.0)))))
    (while (< idx flashcard-sm5-matrix-size)
      (dotimes (i flashcard-sm5-matrix-size)
	(flashcard-sm5-set-element
	 matrix idx i
	 (* (flashcard-sm5-get-element matrix (- idx 1) i)
	    (- (flashcard-sm5-get-element OF 1 i) 0.1))))
      (setq idx (+ idx 1)))
    matrix))

(defun flashcard-method-sm5-get-card (deck)
  "Return a new card from DECK according to the SM5 algorithm."
  ;; first, try and load matrices from this deck.
  (unless flashcard-sm5-optimal-factors
    (setq flashcard-sm5-optimal-factors
	  (or (flashcard-deck-get-note deck 'sm5-optimal-factors)
	      (flashcard-sm5-create-optimal-factors))))
  (unless flashcard-sm5-interval-matrix
    (setq flashcard-sm5-interval-matrix
	  (flashcard-sm5-create-intervals (flashcard-sm5-create-optimal-factors))))
  ;; scan the deck for a card with an expiration date 
  (let* ((cards (flashcard-deck-cards deck))
	 (found nil))
    (while (and cards (not found))
      (if (time-less-p (flashcard-card-get-note (car cards) 'sm-next-time) (current-time))
	  (setq found (car cards))
	(setq cards (cdr cards))))
    (unless found
      ;; try again looking for cards with level of 0
      (setq cards (flashcard-deck-cards deck))
      (while (and cards (not found))
	(if (eq 0 (flashcard-card-get-note (car cards) 'sm-count))
	    (setq found (car cards))
	  (setq cards (cdr cards)))))
    ;; tell the user when the next card will be ready
    (or found
	(let* ((cards (flashcard-deck-cards deck))
	       (next nil)
	       (next-card
		(dolist (card cards next)
		  (if  (not next) (setq next card))
		  (if (time-less-p (flashcard-card-get-note card 'sm-next-time)
				   (flashcard-card-get-note next 'sm-next-time))
		      (setq next card)))))
	  (flashcard-insert (format "The next card will be ready at %s\n\n"
				    (format-time-string "%R %D"
							(flashcard-card-get-note next-card 'sm-next-time))))))))

(defun flashcard-method-sm5-initialize-card (card)
  "Initialize CARD to be used with the SM5 method. For this, we
have to remember the time to ask the card next, which initialy is
the current-time, the initial ease, and the number of times this
card has been asked."
  (progn
    (unless (flashcard-card-get-note card 'sm-next-time)
      (flashcard-card-set-note card 'sm-next-time (current-time)))
    (unless (flashcard-card-get-note card 'sm-interval)
      (flashcard-card-set-note card 'sm-interval flashcard-sm5-initial-interval))
    (unless (flashcard-card-get-note card 'sm-factor)
      (flashcard-card-set-note card 'sm-factor flashcard-sm5-initial-interval))
    (unless (flashcard-card-get-note card 'sm-ease)
      (flashcard-card-set-note card 'sm-ease flashcard-sm5-initial-ease))
    (unless (flashcard-card-get-note card 'sm-count)
      (flashcard-card-set-note card 'sm-count 0))))

(defun flashcard-method-sm5-answered (card quality)
  "Set the next time CARD will be asked based on the response from the user."
  (let* ((count (flashcard-card-get-note card 'sm-count))
	 (ease (flashcard-card-get-note card 'sm-ease))
	 (oldI (flashcard-card-get-note card 'sm-interval))
 	 (ease (cond
                ;; only modify ease for a score of 3 or better
                ((or (< quality 3) (>= 0 quality)) ease)
		((eq 3 quality)
		 (- ease 1))
		((eq 4 quality) ease)
		((eq 5 quality)
		 (+ ease 1))
		(t
		 (error "Invalid quality response."))))
	 (ease (if (< ease 0)
		   0
		 ease))
	 (ease (if (> ease flashcard-sm5-matrix-size)
		   flashcard-sm5-matrix-size
		 ease))
	 (interval (flashcard-sm5-get-PI count ease oldI))
	 (last-time (flashcard-card-get-note card 'sm-next-time)))
    ;; always update the intervals
    (flashcard-sm5-update-intervals 
     oldI (flashcard-card-get-note card 'sm-factor)
     count ease quality)
    ;; set the ease factor
    (flashcard-card-set-note card 'sm-ease ease)
    (flashcard-deck-set-note flashcard-deck 'sm5-optimal-factors
			     flashcard-sm5-optimal-factors)
    (flashcard-card-set-note card 'sm-factor (flashcard-sm5-get-OF count ease))
    (if (< quality 3)
        ;; score less than 3, restart repetitions but maintain ease
	(flashcard-card-set-note card 'sm-count 0))
    (if (< quality 4)
        ;; score less than 4, reschedule for inside of 10 minutes (review)
	(flashcard-card-set-note card 'sm-next-time
				 (time-add (current-time) (seconds-to-time 600)))
      ;; score 4 or higher, then increase count and schedule for next interval.
      (flashcard-card-set-note card 'sm-count
			       (1+ count))
      (flashcard-card-set-note card 'sm-next-time
			       (time-add (current-time)
					 (days-to-time interval)))
      (flashcard-card-set-note card 'sm-interval interval)
      (flashcard-insert (format "Scheduling this card at %0.2f days from now\n" interval)))))


(defun flashcard-method-sm5-check-answer (card answer)
  "Insert the answer, ask the user for a quality point."
  (flashcard-insert "The correct answer is:\n"
                    (propertize (flashcard-card-answer card)
                                'face 'flashcard-answer-face
                                'rear-nonsticky t) "\n")
  (let (char)
    (while (not char)
      (setq char (read-char "Quality of your answer (0-5): "))
      (if (and (>= char 48) (<= char 53))
          (setq char (- char 48))
        (setq char nil)))
    char))

(defun flashcard-method-sm5 ()
  "Use the SuperMemo 5 method in flashcard."
  (interactive)
  (message "Setting up the SM5 method...")
  (setq flashcard-method 'sm5
	flashcard-method-get-card-function 'flashcard-method-sm5-get-card
	flashcard-method-answered-function 'flashcard-method-sm5-answered
	flashcard-method-initialize-card-function 'flashcard-method-sm5-initialize-card
	flashcard-method-check-answer-function 'flashcard-method-sm5-check-answer
	flashcard-method-correct-p-function 'identity
	flashcard-sm5-optimal-factors 
	(or (flashcard-deck-get-note flashcard-deck 'sm5-optimal-factors)
	    (flashcard-sm5-create-optimal-factors))
	)
  (flashcard-deck-initialize flashcard-deck)
  (message "Setitng up the SM5 method...ok"))
			   
(provide 'flashcard-sm5)
;;; flashcard-sm5.el ends here

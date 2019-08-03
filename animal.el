;;; animals.el --- guess the animal

;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Keith Gabryelski <ag@yoda.omnicron.com>
;;	Denis Howe <denis.howe@gmail.com>
;; Maintainer: Denis Howe <denis.howe@gmail.com>
;; Created: 1992-12-23
;; Version: 1.02
;; Keywords: games

;; This file was part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Play the "animal" game ("20 questions" with no limit).  The user
;; thinks of an animal and the program uses a tree whose nodes are
;; yes-no questions and whose leaves are animals to try to guess which
;; animal the user is thinking of.  If it fails, the user is invited
;; to extend the program's tree by telling it a question to
;; distinguish his animal from the program's best guess.

;; Possibly inspired by/descended from the 1978 book "BASIC Computer
;; Games" (https://www.atariarchives.org/basicgames/showpage.php?page=4).

;; https://animalgame.com/ is an online version of the game, probably
;; only vaguely related to the original BASIC or this Lisp version.

;; In your ~/.emacs put:
;;
;;	(autoload 'play-animal "animal" "Play the animal game." t)

;;; Change log:

;; v1.00 1992-12-23 Keith Gabryelski <ag@yoda.omnicron.com>
;; Original version.

;; v1.01 1994-06-27 Denis Howe <dbh@doc.ic.ac.uk>
;; Extensive rewrite.

;; v1.02 1994-07-18 Denis Howe <dbh@doc.ic.ac.uk>
;; Clear case-fold-search in format-animal.

;; LCD Archive Entry:
;; animal|Denis Howe|dbh@doc.ic.ac.uk|
;; The ancient game of guess the animal.|
;; 18 Jul 1994|1.02|~/games/animal.el|

;;; Code:

(defvar animals-file "~/.animals"
  "Animal game data file.")

(defvar animals "a cat"
  "Default data for animal game.
A tree where each node is either of the form
(QUESTION YES-TREE . NO-TREE) or the name of an animal.")

(defun play-animal ()
  "Play the games of animal until you're sick of it."
  (interactive)
  (animal)
  (if (y-or-n-p "Would you like to play again? ")
      (play-animal)))

(defun animal ()
  "Play the game \"Guess The Animal\".
Data is read from the file named by the variable \"animals-file\" if
that file exists.  The data is saved there after each game."
  (interactive)
  (animal-read-file)
  (switch-to-buffer " Animals dialogue")
  (erase-buffer)
  (insert "Please think of an animal and I will try
to guess which one you are thinking of.\n\n")
  (message "Hit a key when ready")
  (read-event)
  (setq animals (animal-ask animals))
  (animal-write-file))

(defun animal-ask (animal-tree)
  "Ask about known animals until we reach a terminal.
Return a replacement animal tree."
    (if (listp animal-tree)
	;; Non-leaf node - a question to ask
	(let* ((question (car animal-tree))
	       (subtrees (cdr animal-tree))
	       (yes-tree (car subtrees))
	       (no-tree (cdr subtrees)))
	  (cons question
		(if (animal-query-bool question)
		    (cons (animal-ask yes-tree) no-tree)
		  (cons yes-tree (animal-ask no-tree)))))
      ;; Leaf-node - an animal name
      (if (animal-query-bool (concat "Was it " animal-tree))
	  animal-tree			; Got it - no change
	(animal-give-up animal-tree)))) ; No luck - give up

(defun animal-give-up (old-animal)
  "Ask the user for a new animal and question.
Return a new tree for the best guess so far (OLD-ANIMAL), the new
question and the new animal."
  (let* ((new-animal
	  (format-animal
	   (animal-query-string "What animal where you thinking of")))
	 (new-question
	  (animal-get-question new-animal old-animal)))
    (cons new-question
	  (if (animal-query-bool (animal-replace-it new-question new-animal))
	      (cons new-animal old-animal)
	    (cons old-animal new-animal)))))

(defun animal-get-question (new old)
  "Ask the user for a question to distingish NEW from OLD."
  (let ((question (animal-query-string
		   (format "What yes/no question distinguishes %s from %s"
			   new old))))
    (if (string-match "\\<it\\>" question)
	(format-question question)
      (insert "Please include the word \"it\" in your question.\n\n")
      (animal-get-question new old))))

(defun format-question (question)
  "Zap trailing \"?\" from QUESTION and ensure that it starts with
upper-case."
  (if (eq (substring question -1) "?")	; Zap trailing "?"
      (setq question (substring question 0 -1)))
  (concat (upcase (substring question 0 1))
	(substring question 1)))

(defun animal-read-file ()
  "Read the animals data.
Return the data read from \"animals-file\" if it exists."
  (setq animals-file (expand-file-name animals-file))
  (if (file-readable-p animals-file)
      (save-excursion
	(find-file animals-file)
	(goto-char (point-min))
	(setq animals (read (current-buffer))))))

(defun animal-write-file ()
  "Write the animals data to \"animals-file\"."
  (let* ((standard-output (get-buffer-create " Animals data")))
    (set-buffer standard-output)
    (print animals)
    (write-file animals-file)
    (kill-buffer standard-output)))

(defun animal-replace-it (question animal)
  "Replace the string \"it\" in QUESTION with ANIMAL."
  (concat (substring question 0 (string-match "it" question))
	  animal
	  (substring question (match-end 0))))

(defun format-animal (animal)
  "Make sure ANIMAL starts with \"a \", \"an \", \"the \" or upper case."
  (let ((lower (downcase animal))
	(case-fold-search nil))
    (cond ((string-match "^a " lower) animal)
	  ((string-match "^an " lower) animal)
	  ((string-match "^the " lower) animal)
	  ((string-match "^[A-Z]" animal) animal)
	  ((string-match "^[aeiou]" lower) (concat "an " animal))
	  (t (concat "a " animal)))))

(defun animal-query-bool (prompt)
  "Ask the user a yes/no PROMPT.
Insert it and the reply in the current buffer."
  (insert prompt "? ")
  (let ((reply (y-or-n-p (concat prompt "? "))))
    (insert (if reply "Yes." "No.") "\n\n")
    (sit-for 0)
    reply))

(defun animal-query-string (prompt)
  "Insert PROMPT in the current buffer, read a reply and insert that too
and return it."
  (insert prompt "?\n")
  (let ((reply (read-string (concat prompt "? "))))
    (insert reply "\n\n")
    (sit-for 0)
    reply))

;; animals.el ends here

;;; mp3player-scores.el --- Scoring system for mp3player

;; Author & Maintainer: Jean-Philippe Theberge (jphil21@sourceforge.net)
;; version :
(defconst mp3player-scores-version "1.9")
;; last update: 12/05/2003
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1998 - 1999 Free Software Foundation, Inc.
;;
;; This file is not part of GNU Emacs. :-(
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NOTE: This is experimental stuff - comments welcome!
;;
;; * How to use scoring in mp3player
;;
;; When you load mp3player, you are set to a default mood
;; 'mp3player-default-mood' A mood is a one word string describing how
;; you feel (like "funny", "tired", "aggresive"...)  Each mood have is
;; own set of scoring rules.
;;
;; You can change your mood with M-x mp3player-change-mood.
;;
;; Every music file start with a default score of 0 the command
;; mp3player-score-up-current and mp3player-score-down-current modify the
;; score of the file you are curently listening by 1 In addition,
;; skipping a file (with mp3player-skip) automaticaly score the file
;; down.
;;
;; With scoring on (this mean the variable mp3player-use-scoring is t),
;; mp3player will compare the score of the file with your tolerance to
;; decide if it is played or not.
;;
;; The default tolerance level is 0 (or the variable
;; mp3player-min-score).  This mean files with a score of 0 or more will
;; be played and files with a score of -1 or less will be skipped.
;;
;; You can change the tolerance (by 1) with M-x
;; mp3player-score-lower-tolerance and M-x
;; mp3player-score-be-more-tolerant
;;
;; History:
;;
;; Initial version of this file was 1.8.  
;; A experimental buggy version! 
;;
;; 1.9: Fix in the scores-file format.
;;
;;; Code:

(defvar mp3player-current-mood "happy")
(defvar mp3player-min-score 0)
(defcustom mp3player-scoresfiles-directory "~/.emacs-mpg123/scores"
  "*Directory to store scoresfiles."
  :type 'string
  :group 'mp3player)
(defvar mp3player-default-score 0)

(defun mp3player-save-scores ()
  (interactive)
  (find-file (concat mp3player-scoresfiles-directory "/" mp3player-current-mood ".sco"))
  (delete-region (point-min)(point-max))
  (prin1
   mp3player-scores-list (current-buffer))
  (save-buffer 0)
  (kill-buffer (current-buffer)))

(defun mp3player-load-scores ()
  (let ((scores-file (concat mp3player-scoresfiles-directory "/" mp3player-current-mood ".sco")))
    (setq mp3player-scores-list
	  (if (file-exists-p scores-file)
	    (read
	     (with-temp-buffer
	       (insert-file scores-file)
	       (buffer-string)))))))

(if (not mp3player-scores-list) (setq mp3player-scores-list (mp3player-load-scores)))

(defun mp3player-change-mood (mood)
  "Change the current MOOD.  if called interactively, confirm saving 
of the score file." 
  (interactive "sMood: ")
  (if (or (not (interactive-p))
	  (y-or-n-p (concat "Save " mp3player-current-mood " scores to file? ")))
      (if mp3player-scores-list (mp3player-save-scores)))
  (setq mp3player-current-mood (downcase mood))
  (mp3player-load-scores))

(defun mp3player-assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is KEY.
Return the modified alist."
  (let ((tail alist))
    (while tail
      (if (equal (car (car tail)) key)
	  (setq alist (delete (car tail) alist)))
      (setq tail (cdr tail)))
    alist))

(defun mp3player-score (score mp3)
  (let ((sc (or (cdr (assoc mp3 mp3player-scores-list)) 0))
	(mp3i (intern mp3)))
    (setq mp3player-scores-list (mp3player-assoc-delete-all mp3i mp3player-scores-list))
    (setq mp3player-scores-list (cons (cons mp3i (+ score sc))
				      mp3player-scores-list))
    (message "New score is %s" (+ score sc))))
  
(defun mp3player-score-up-current ()
  (interactive)
  (mp3player-score 1 mp3player-now-playing-filename))

(defun mp3player-score-down-current ()
  (interactive)
  (mp3player-score -1 mp3player-now-playing-filename))

;; automaticaly score down skipped files!
(defun mp3player-skip (noscoring)
  "Stop playing the current song and skip to the next in memory playlist."
  (interactive "P")
  (if (not noscoring)
      (if mp3player-scores-list (mp3player-score-down-current)))
  (if (equal mp3player-program-id 'winamp)
      (if mp3player-command-line-winamp-interface
	  (shell-command (concat mp3player-command-line-winamp-interface " -n")))
    (let ((lst mp3player-queued-files))
      (mp3player-stop)
      (if lst (mp3player-play-files-or-urls lst)))))

;; (defadvice mp3player-skip (before mp3player-score-down-skipped)
;;   (mp3player-score-down-current))
;; (ad-activate 'mp3player-skip)
(defadvice mp3player-quit (after mp3player-save-score-on-quit)
  (if (y-or-n-p (concat "Save " mp3player-current-mood " scores to file? "))
      (if mp3player-scores-list (mp3player-save-scores))))
(ad-activate 'mp3player-quit)

(defun mp3player-get-score (f)
  (or (cdr (assoc (intern f) mp3player-scores-list)) mp3player-default-score))

(defun mp3player-check-score (f)
  (if (>= (mp3player-get-score f) mp3player-min-score)
      t
    (progn (message "File %s score too low, skipping" (file-name-nondirectory f)) 
	   nil)))

(defun mp3player-score-lower-tolerance ()
  "Only play mp3 with a higher score"
  (interactive)
  (setq mp3player-min-score (+ mp3player-min-score 1)))

(defun mp3player-score-be-more-tolerant ()
  "Allow playing of mp3 with a lower score"
  (interactive)
  (setq mp3player-min-score (- mp3player-min-score 1)))

(global-set-key [C-M-kp-up] 'mp3player-score-up-current)
(global-set-key [C-M-kp-down] 'mp3player-score-down-current)

;; This is just to convert string-based scores-list to symbol-based
;; you are not supposed to use this!
(defun mp3player-convert-scores-list ()
  (setq mp3player-scores-list 
	(mapcar 
	 (lambda (x)
	   (let ((a (car x))
		 (d (cdr x)))
	     (cons (if (stringp a)
		       (intern a)
		     a)
		   d)))
	 mp3player-scores-list)))

(provide 'mp3player-scores)



;;; mp3player-scores.el ends here


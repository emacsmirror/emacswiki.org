;;; flyguess.el --- flyspell dictionary guesser (replaces flyspell-guess.el)

;; Copyright (C) 2007, 2008, 2014 Alessandro Di Marco

;; Author:          Alessandro Di Marco (dmr@ethzero.com)
;; Maintainer:      Alessandro Di Marco (dmr@ethzero.com)
;; Created:         Oct 27, 2007
;; Keywords:        convenience
;; Latest Version:  Jun 27, 2014

;; This file is not part of Emacs

;; COPYRIGHT NOTICE

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Brand new version of flyspell-guess. It uses a new sampling algorithm that
;;  boosts guess speed and accuracy.

;;; Installation:
;;
;;  Load it as usual, ie. put this file in the Emacs-Lisp load path and add the
;;  following to your dot-emacs startup file:
;;
;;     (require 'flyguess)
;;
;;  This way flyguess will load every time you start Emacs; the insinuation of
;;  the guess indicator is now automatic so that's really it.

;;; Usage:
;;
;;  (flyguess-buffer) tries to guess the language of the current buffer --
;;  let's call it the guessing buffer. Depending on the language(s) used in the
;;  guessing buffer, a certain amount of text is needed for a successful
;;  guess. If flyguess gets puzzled it enters the supplicant mode, otherwise it
;;  will plug the guessed language dictionary and turn on flyspell. Please note
;;  that flyguess won't mess with buffers other than the guessing one, so you
;;  can have as many guessing buffer as you like and flyguess will operate on
;;  all of them seamlessly.

;;  When in supplicant mode flyguess periodically tries to guess the language
;;  of the guessing buffer with a given time interval (see the flyguess
;;  customization page for the details on the poll-timeout.) Flyguess will only
;;  exit the polling mode when either it finds a suitable language or the
;;  guessing buffer gets killed.
;;
;;  When flyguess is idle (ie. it is awaiting the expiration of the polling
;;  timeout) the "[?]" indicator will be shown on the mode-line of the guessing
;;  buffer (again, see the flyguess customization page for details on the
;;  indicator-format.) Once the polling timeout expires, the indicator changes
;;  in "[G]" and the guessing procedure starts immediately after. If no
;;  suitable language is found, the indicator is reverted back to "[?]" and
;;  flyguess turns idle again. The indicator is removed from the mode-line as
;;  soon as a suitable language is found.

;;  Sometimes the guessing procedure could be very quick so the "[G]" indicator
;;  won't ever show up on the mode-line; clearly this is not a bug. Depending
;;  on your hardware setup, you could need to tweak the options of the guessing
;;  engine in the customization page; ideally the time spent for a guess
;;  shouldn't last longer than 500ms or so.

;;; Known Bugs:
;;
;;  So far so good ;-)

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Alessandro Di Marco (dmr@ethzero.com).
;;
;;  This version of flyguess was developed and tested with GNU Emacs 24.3.91.1
;;  under Linux. Please, let me know if it works with other OS and versions of
;;  Emacs.

;;; Code:

(require 'flyspell)

(defcustom flyguess-persistence 1000
  "The persistence of the guessing engine. This value depends on several factors,
such as: the amount of guessing regions, their geometry and the
set of languages involved with the guess. Large buffers written
in a common language usually require less persistence than small
buffers written in slang/mixed languages. Useful range > 0; if
unsure say no less than 800."
  :group 'flyguess
  :type 'number)

(defcustom flyguess-min-region-size 32
  "Minimum guessing region size in chars. Pushing this too high
will slow down the guessing process. Useful range > 0; if unsure
say no more than 128."
  :group 'flyguess
  :type 'number)

(defcustom flyguess-max-region-size 128
  "Maximum guessing region size in chars. Pushing this too low
will puzzle the guessing engine. Useful range > 0; if unsure say
no less than 64."
  :group 'flyguess
  :type 'number)

(defcustom flyguess-regions 5
  "Number of probing regions in the buffer. The more regions you
probe the more focused the guess will be, but don't push this too
high or the guessing engine will slow down noticeably. Useful
range > 0; if unsure say no more than 10"
  :group 'flyguess
  :type 'number)

(defcustom flyguess-dictionaries
  '("de" "en" "fr" "it" "nl" "pt")
  "List of dictionary names to be taken into account during the
guess. Keep it as slim as possible or the guessing engine will
slow down noticeably."
  :group 'flyguess
  :type '(repeat (string)))

(defcustom flyguess-tolerance 0.5
  "How much the guessing engine must be confident about a guess
in order to accept it as the buffer's language. Low values make
the engine more prone to take risks, ie. less words are needed to
decide a language. Useful range in [0,1]; if unsure say no less
than 0.4"
  :group 'flyguess
  :type 'number)

(defcustom flyguess-indicator-format "[%1s] "
  "The mode-line format of the guess indicator; use nil to leave
the mode-line unaffected during the guess."
  :group 'flyguess
  :type 'string)

(defcustom flyguess-poll-timeout 30
  "How many seconds the engine must wait between one guessing
attempt and another."
  :group 'flyguess
  :type 'number)

(defvar flyguess-timer nil)
(defvar flyguess-indicator nil)

(make-variable-buffer-local 'flyguess-timer)
(make-variable-buffer-local 'flyguess-indicator)

(declare-function regions-intersect "flyguess" (r1 r2))
(declare-function guess-slot "flyguess" (regions slot))
(declare-function around "flyguess" (min weight))
(declare-function indicate "flyguess" (what))
(declare-function inspector "flyguess" (buffer prepare))
(declare-function insinuate "flyguess" (&optional conceal))

(defun flyguess-survey (left right)
  (let ((err 0))
    (defun incorrect (left right poss)
      (setq err (1+ err)))
    (add-hook 'flyspell-incorrect-hook 'incorrect nil t)
    (flyspell-region left right)
    (remove-hook 'flyspell-incorrect-hook 'incorrect t)
    (list err (count-words-region left right))))

(defun flyguess-focus-region (left right)
  (let ((left (save-excursion
		(goto-char left)(forward-word 2)(backward-word)(point)))
	(right (save-excursion
		 (goto-char right)(backward-word 2)(forward-word)(point))))
    (if (< left right)
	(list left right)
      nil)))

(defun flyguess-guess-region (left right)
  (let ((region (flyguess-focus-region left right)))
    (if region (apply 'flyguess-survey region))))

(defun flyguess-toss-region ()
  (let ((span (- (point-max) (point-min))))
    (if (> span flyguess-min-region-size)
	(catch 'break
	  (dotimes (cycles flyguess-persistence nil)
	    (let ((begin (random span)))
	      (let ((width (- span begin)))
		(let ((range (min (- width flyguess-min-region-size)
				  flyguess-max-region-size)))
		  (if (> range 0)
		      (let ((end (+ begin
				    (+ flyguess-min-region-size
				       (random range)))))
			(throw 'break (list begin end))))))))))))

(defun flyguess-toss-regions (size)
  (defun regions-intersect (r1 r2)
    (let ((f1 (car r1))
	  (t1 (car (cdr r1)))
	  (f2 (car r2))
	  (t2 (car (cdr r2))))
      (and (>= t1 f2) (>= t2 f1))))
  (defun guess-slot (regions slot)
    (catch 'break
      (let ((cycles 0))
	(while (let ((region (flyguess-toss-region)))
		 (unless region
		   (throw 'break nil))
		 (aset regions slot region)
		 (catch 'break
		   (dotimes (sub slot regions)
		     (if (regions-intersect region (aref regions sub))
			 (throw 'break t)))
		   nil))
	  (if (> (setq cycles (1+ cycles)) flyguess-persistence)
	      (throw 'break nil)))
	t)))
  (catch 'break
    (let ((regions (make-vector size nil)))
      (dotimes (slot size regions)
	(unless (guess-slot regions slot)
	  (throw 'break nil))))))

(defun flyguess-guess-regions (regions)
  (catch 'break
    (let ((size (length regions)))
      (let ((guesses (make-vector size nil)))
	(dotimes (sub size guesses)
	  (let ((region (aref regions sub)))
	    (let ((hits (flyguess-guess-region
			 (car region) (car (cdr region)))))
	      (if hits
		  (aset guesses sub
			(append hits (list (/ (* (car hits) 100.0)
					      (car (cdr hits))))))
		(throw 'break nil)))))))))

(defun flyguess-gauge (guesses)
  (if guesses
      (let ((mean 0.0)
	    (size (length guesses)))
	(/ (dotimes (sub size mean)
	     (setq mean (+ (car (last (aref guesses sub))) mean))) size))))

(defun flyguess-guess (dicts)
  (defun around (min weight)
    (if (equal weight nil)
	1
      (if (= weight 0)
	  0
	(- 1 (/ (float min) weight)))))
  (let ((regions (flyguess-toss-regions flyguess-regions)))
    (catch 'break
      (if regions
	  (let ((fitness
		 (mapcar (lambda (lang)
			   (ispell-change-dictionary lang)
			   (let ((weight (flyguess-gauge
					  (flyguess-guess-regions regions))))
			     (list weight lang)))
			 dicts)))
	    (let ((min nil)
		  (lang nil)
		  (cnt 0))
	      (dolist (pair fitness)
		(let ((weight (car pair)))
		  (if (equal weight nil)
		      (throw 'break nil))
		  (unless (and min (<= min weight))
		    (setq min (car pair))
		    (setq lang (cadr pair)))))
	      (dolist (pair fitness)
		(let ((weight (car pair)))
		  (if (< (around min weight) flyguess-tolerance)
		      (setq cnt (1+ cnt)))))
	      (list lang cnt)))))))

(defun flyguess-guess-dictionary (buffer &optional enable prepare)
  (let ((dicts flyguess-dictionaries))
    (dolist (lang flyguess-dictionaries)
      (condition-case nil (ispell-change-dictionary lang)
	(error (setq dicts (delete lang dicts)))))

    (defun indicate (what)
      (if flyguess-indicator-format
	  (progn
	    (setq flyguess-indicator
		  (if what (format flyguess-indicator-format what) ""))
	    (force-mode-line-update))))
    
    (if (get-buffer buffer)
	(let ((old (current-buffer)))
	  (indicate "G")
	  (set-buffer buffer)
	  (let ((new (clone-indirect-buffer "flyguess" nil t)))
	    (set-buffer new)
	    (if (functionp prepare)
		(funcall prepare))
	    ;; prevents potential buffer switches in helper
	    (set-buffer new)
	    (let ((guess (flyguess-guess dicts)))
	      (if guess
		  (if (= (cadr guess) 1)
		      (let ((lang (car guess)))
			(set-buffer buffer)
			(kill-buffer new)
			(if enable
			    (progn
			      (ispell-change-dictionary lang)
			      (turn-on-flyspell)))
			(set-buffer old)
			(indicate nil)
			lang)
		    (set-buffer old)
		    (kill-buffer new)
		    (indicate "?")
		    (if enable
			nil
		      (message "Could not guess a suitable dictionary, try later")))
		(set-buffer old)
		(kill-buffer new)
		(indicate "?")
		(if enable
		    nil
		  (message "Not enough context to guess"))))))
      t)))

(defun flyguess-buffer (&optional buffer prepare seed)
  (interactive "P")

  (if seed
      (random t))
  
  (defun insinuate (&optional conceal)
    (delq 'flyguess-indicator global-mode-string)
    (unless conceal
      (if global-mode-string
	  (setcdr global-mode-string (cons 'flyguess-indicator (cdr global-mode-string)))
	(setq global-mode-string '("" flyguess-indicator))) t))

  (defun inspector (buffer prepare)
    (if (buffer-live-p buffer)
	(if (flyguess-guess-dictionary buffer t prepare)
	    (insinuate t)
	  (setq flyguess-timer
		(run-at-time flyguess-poll-timeout
			     nil #'inspector buffer prepare)))))
  
  (if (timerp flyguess-timer)
      (cancel-timer flyguess-timer))
  (insinuate)
  (setq flyguess-timer
	(run-at-time 0 nil #'inspector
		     (if (bufferp buffer) buffer (current-buffer)) prepare)))

(setq flyspell-issue-message-flag nil)

(provide 'flyguess)

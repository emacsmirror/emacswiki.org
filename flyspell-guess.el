;;; flyspell-guess.el --- flyspell dictionary guesser

;; Copyright (C) 2007, 2008 Alessandro Di Marco

;; Author:          Alessandro Di Marco (dmr@c0nc3pt.com)
;; Maintainer:      Alessandro Di Marco (dmr@c0nc3pt.com)
;; Created:         Oct 27, 2007
;; Keywords:        convenience
;; Latest Version:

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
;;  Still too lazy to write one ;-)

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp  load path, then add the following to your
;;  ~/.emacs startup  file:
;;
;;     (require 'flyspell-guess)
;;
;;  to load flyspell-guess every time you start Emacs.
;;
;;     (eval-after-load "flyspell-guess" '(flyspell-insinuate-guess-indicator))
;;
;;  to activate the guess indicator. This last step is optional and it does not
;;  affect the flyspell-guess  functioning in any way (except  for the presence
;;  of the indicator itself, of course).

;;; Usage:
;;
;;  Do you really need help for this?

;;; Known Bugs:
;;
;;  None at the moment ;-)

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Alessandro Di Marco (dmr@c0nc3pt.com).
;;
;;  This  version of  flyspell-guess was  developed and  tested with  GNU Emacs
;;  23.0.60.1 under  Linux. Please, let me know  if it works with  other OS and
;;  versions of Emacs.

;;; Code:

(require 'flyspell)

(defcustom flyspell-dictionaries
  '("english" "italian")
  "List of dictionaries considered in the guess."
  :group 'flyspell
  :type '(repeat (string)))

(defcustom flyspell-guess-size 76
  "The guessing region size."
  :group 'flyspell
  :type 'integer)

(defcustom flyspell-guess-slots 3
  "The number of guessing regions."
  :group 'flyspell
  :type 'integer)

(defcustom flyspell-focus 50
  "The percentage offset of the guessing area center."
  :group 'flyspell
  :type '(integer
	  :match (lambda (widget value)
		   (and (>= value 0)
			(<= value 100)))))

(defcustom flyspell-timeout 3
  "The  first  guess attempt  timeout  in  case  of almost  empty
buffer."
  :group 'flyspell
  :type 'number)

(defcustom flyspell-polling-timeout 10
  "The timeout between successive guessing attempts."
  :group 'flyspell
  :type 'number)

(defcustom flyspell-idle-timeout 1.5
  "The idle period needed to start guessing."
  :group 'flyspell
  :type 'number)

(defcustom flyspell-indicator-format "[%s]--"
  "The format of the guessing task indicator."
  :group 'flyspell
  :type 'string)

(defvar flyspell-timer nil)
(defvar flyspell-timer-aux nil)
(defvar flyspell-origin)
(defvar flyspell-guess-indicator nil)

(make-variable-buffer-local 'flyspell-timer)
(make-variable-buffer-local 'flyspell-timer-aux)
(make-variable-buffer-local 'flyspell-origin)
(make-variable-buffer-local 'flyspell-guess-indicator)

(defvar flyspell-min-buffer-size
  (* flyspell-guess-size flyspell-guess-slots))

(defun flyspell-count-errors-range (dict left right)
  (let ((errors (make-vector flyspell-guess-slots 0))
	(flyspell-issue-message-flag nil))
    (defun flyspell-error-counter (left right undef)
      (let ((slotl (/ (- left flyspell-origin) flyspell-guess-size))
	    (slotr (/ (- right flyspell-origin) flyspell-guess-size)))
	(if (and (/= slotl slotr) (< slotr flyspell-guess-slots))
	    (aset errors slotr
		  (1+ (aref errors slotr))))
	(if (< slotl flyspell-guess-slots)
	    (aset errors slotl
		  (1+ (aref errors slotl))))))
    (condition-case nil
    	(progn
    	  (ispell-change-dictionary dict)
    	  (add-hook 'flyspell-incorrect-hook
    		    'flyspell-error-counter nil t)
    	  (flyspell-region (max (point-min) left)
    			   (min (point-max) right))
    	  (remove-hook 'flyspell-incorrect-hook
    		       'flyspell-error-counter t)
    	  )
      (error ((lambda ()
    		(message "Skipping dictionary '%s'" dict)
    		(setq errors nil)))))
    errors))

(defun flyspell-guess-dictionary-range (dicts left right)
  (let ((dict)
	(min nil))
    (while (and dicts (not min))
      (setq dict (car dicts))
      (setq min (flyspell-count-errors-range dict left right))
      (setq dicts (cdr dicts)))
    (if min
	(let ((guess (make-vector flyspell-guess-slots dict))
	      (equal (make-vector flyspell-guess-slots nil)))
	  (dolist (dict dicts guess)
	    (let ((cur (flyspell-count-errors-range dict left right)))
	      (if cur
		  (dotimes (i flyspell-guess-slots)
		    (let ((c (aref cur i))
			  (m (aref min i)))
		      (if (= c m)
			  (aset equal i t)
			(if (< c m)
			    (progn
			      (aset min i c)
			      (aset guess i dict)))))))))
	  (dotimes (i flyspell-guess-slots)
	    (if (aref equal i)
		(aset guess i nil)))
	  guess)
      nil)))

(defun flyspell-do-guess-dictionary (buffer &optional assist)
  (let ((old (current-buffer)))
    (set-buffer buffer)
    (setq flyspell-guess-indicator nil)
    (let ((new (clone-indirect-buffer "guess" nil t)))
      (set-buffer new)
      (if assist
	  (eval assist))
      (setq flyspell-origin
	    (- (/ (* (- (point-max) (point-min)) 50) 100)
	       (/ flyspell-min-buffer-size 2)))
      (setq flyspell-origin
	    (+ (point-min)
	       (if (< flyspell-origin 0) 
		   0
		 flyspell-origin)))
      (let ((guess
	     (flyspell-guess-dictionary-range
	      flyspell-dictionaries
	      flyspell-origin
	      (+ flyspell-origin
		 (* flyspell-guess-size flyspell-guess-slots)))))
	(set-buffer buffer)
	(kill-buffer new)
	(if guess
	    (let ((outc (make-hash-table :test 'eq))
		  (dict))
	      (defun findhash (hash &optional min)
		(let ((refk nil)
		      (refv nil))
		  (defun findmin (key value)
		    (if (or (not refv) (< value refv))
			(progn
			  (setq refk key)
			  (setq refv value))))
		  (defun findmax (key value)
		    (if (or (not refv) (> value refv))
			(progn
			  (setq refk key)
			  (setq refv value))))
		  (maphash
		   (if min
		       'findmin
		     'findmax) hash)
		  refk))
	      (dotimes (i flyspell-guess-slots)
		(if (setq dict (aref guess i))
		    (puthash dict (1+ (gethash dict outc 0)) outc)))
	      (if (and (fboundp 'findhash)
                       ;; this ^ shuts-up the  lisp compiler (still I dunno why
                       ;; it complains anyway)
		       (setq dict (findhash outc)))
		  (progn
		    (message "Guessed language '%s'" dict)
		    (ispell-change-dictionary dict)
		    (turn-on-flyspell))
		(message "Could not guess a dictionary")))
	  (message "No valid dictionaries specified"))))
    (set-buffer old)))

(defun flyspell-indicate-guess-indicator (what)
  (setq flyspell-guess-indicator 
	(format flyspell-indicator-format what))
  (force-mode-line-update))
  
(defun flyspell-guess-dictionary (&optional mbs assist)
  "Guess the buffer language and installs the corresponding
flyspell dictionary. Optional parameters are: MBS, the minimum
buffer size (in chars) required for guessing, and ASSIST, a
callback invoked just before the guessing (for example, it can be
used to narrow down the buffer, in order to skip header and
footer out in mail messages)."
  (interactive "P")
  (if (not flyspell-timer)
      (let ((min-buffer-size 
	     (if (not (and mbs (integerp mbs))) 
		 flyspell-min-buffer-size 
	       mbs)))
	(if (< (- (point-max) (point-min))
	       min-buffer-size)
	    (progn
	      (flyspell-indicate-guess-indicator "G")
	      (add-hook 'kill-buffer-hook
			'(lambda ()
			   (if (timerp flyspell-timer)
			       (setq flyspell-timer
				     (cancel-timer flyspell-timer)))
			   (if (timerp flyspell-timer-aux)
			       (setq flyspell-timer-aux
				     (cancel-timer flyspell-timer-aux)))) nil t)
	      (setq flyspell-timer ; timer-list
		    (run-with-timer
		     flyspell-timeout flyspell-polling-timeout
		     '(lambda (buffer size &optional assist)
			(let ((old (current-buffer)))
			  (defun update-guess-indicator (buffer)
			    (flyspell-indicate-guess-indicator "?")
			    (setq flyspell-timer-aux 
				  (run-with-timer 
				   0.2 1 
				   (quote (lambda (buffer) 
					    (let ((old (current-buffer)))
					      (set-buffer buffer)
					      (setq flyspell-timer-aux
						    (cancel-timer flyspell-timer-aux))
					      (flyspell-indicate-guess-indicator "G")
					      (set-buffer old))))
				   buffer)))
			  (set-buffer buffer)
			  (if (>= (- (point-max) (point-min)) size)
			      (progn
				(setq flyspell-timer
				      (cancel-timer flyspell-timer))
				(run-with-idle-timer 
				 flyspell-idle-timeout nil
				 'flyspell-do-guess-dictionary
				 buffer assist))
			    (update-guess-indicator buffer))
			  (set-buffer old)))
		     (current-buffer) min-buffer-size assist)))
	  (flyspell-do-guess-dictionary (current-buffer) assist))
	nil)
    (message "Waiting a suitable context for guessing")))

(defun flyspell-insinuate-guess-indicator ()
  "Install the guess indicator in emacs mode string."
  (interactive)
  (delq 'flyspell-guess-indicator global-mode-string)
  (setcdr global-mode-string 
	  (cons 'flyspell-guess-indicator 
		(cdr global-mode-string))))

(provide 'flyspell-guess)

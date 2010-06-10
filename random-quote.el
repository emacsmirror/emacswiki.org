;;; @(#) random-quote.el -- chooses a random quote from a file
;;; @(#) $Id$

;; This file is not part of Emacs

;; Copyright (C) 2006 by Alessandro Di Marco
;; Author:          Alessandro Di Marco (dmr@gmx.it)
;; Maintainer:      Alessandro Di Marco (dmr@gmx.it)
;; Created:         April 26, 2006
;; Keywords:        quote random
;; Latest Version:  

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  random-quote provides a simple mechanism to pick a random quote in a
;;  file. It is sufficient to put the file name containing the quotes in the
;;  `random-quote-file' variable. Next, at each invocation of
;;  `pick-random-quote', a string containing a random quote will be
;;  returned. The `random-quote-file' format has been kept as simpler as
;;  possible; at moment it simply consists in a quote per line.

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path, then add one of the following
;;  to your ~/.emacs startup file.  You can load random-quote every time you
;;  start Emacs:
;;
;;     (require 'random-quote)

;;; Usage:
;;
;;  Do you really need help for this?

;;; Known Bugs:
;;
;;  Too simple to have one (hopefully ;-)

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Alessandro Di Marco (dmr@gmx.it).
;;
;;  This version of random-quote was developed and tested with GNU Emacs
;;  22.0.50.1 under Linux. Please, let me know if it works with other OS and
;;  versions of Emacs.

;;; Change Log:
;;
;;  

(defvar random-quote-file (expand-file-name "~/.quotes")
  "*Pathname of a file containing quotes. One quote per line.")

(defun pick-random-quote ()
  "Returns a string containing a randomly selected quote,
picked from random-quote-file."
  (interactive)
  (save-excursion
    (let ((quote-buf 
	   (find-file-noselect random-quote-file)))
      (set-buffer quote-buf)
      (bury-buffer quote-buf)
      (if (> (random 100) 90)
	  (random t))
      (let ((quote-buf-lines 
	     (count-lines 1 (buffer-size))))
	(let ((quote-random-line 
	       (+ (random quote-buf-lines) 1)))
	  (goto-line quote-random-line)
	  (beginning-of-line)
	  (let ((beg (point)))
	    (end-of-line)
	    (let ((end (point))
		  (temp-buf 
		   (generate-new-buffer "prqbuf")))
	      (set-buffer temp-buf)
	      (bury-buffer temp-buf)
	      (insert-buffer-substring quote-buf beg end)
	      (fill-region 1 (- end beg) 'left)
	      (let ((indented-string 
		     (buffer-substring 1 (+ (buffer-size) 1))))
		(kill-buffer temp-buf)
		(kill-buffer quote-buf)
		indented-string))))))))

(provide 'random-quote)

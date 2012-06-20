;;; loops.el --- detect loops in lists

;; Copyright (C) 2002  Alex Schroeder, ZwaX

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.6
;; Keywords: lisp
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?LoopFindPackage

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; See the test cases at the end of the file to get an idea of how to
;; use this.  I am sure this can be optimized a lot.

(require 'cl)

(defalias 'loop-find 'loop-find-longest)

(defvar loop-consecutive nil); to bind dynamically in the test

(defun loop-find-shortest (lst loop-min-repetitions loop-min-length loop-max-length &optional loop-consecutive)
  "Find the shortest loop in LST.
There must be at least LOOP-MIN-REPETITIONS repetitions,
and a loop must be at least LOOP-MIN-LENGTH elements long,
but no longer than LOOP-MAX-LENGTH.  If LOOP-CONSECUTIVE
is non-nil, the repetitions must be consecutive.

Examples:

  (loop-find-shortest '(1 2 3 1 2 3 1 2 3 4 5 6) 2 2 10 nil)
  => (1 2)

  (loop-find-shortest '(1 2 3 1 2 3 1 2 3 4 5 6) 2 2 10 t)
  => (1 2 3)

See also `loop-find-longest'."
  (let ((len loop-min-length) result)
    (while (and (<= len loop-max-length)
		(not result))
      (let ((candidate (subseq lst 0 len)))
	(if (>= (loop-count lst candidate loop-min-repetitions len) loop-min-repetitions)
	    (setq result candidate)
	  (setq len (1+ len)))))
    result))

(defun loop-find-longest (lst loop-min-repetitions loop-min-length loop-max-length &optional loop-consecutive)
  "Find longest loop in LST.
There must be at least LOOP-MIN-REPETITIONS repetitions,
and a loop must be at least LOOP-MIN-LENGTH elements long,
but no longer than LOOP-MAX-LENGTH.  If LOOP-CONSECUTIVE
is non-nil, the repetitions must be consecutive.

Examples:

  (loop-find-longest '(1 2 1 2 3 1 2 1 2) 2 2 10 nil)
  => (1 2 1 2)

  (loop-find-longest '(1 2 1 2 3 1 2 1 2) 2 2 10 t)
  => (1 2)

See also `loop-find-shortest'."
  ;; if they have to be consecutive, it's possible to find a pattern
  ;; of length 3 even if there is no pattern of length 2 so start by
  ;; looking at the longest possible pattern and reduce it in stages
  (if loop-consecutive
      (let ((len loop-max-length)
	    result longest)
	(while (and (>= len loop-min-length)
		    (not result))
	  (let ((candidate (subseq lst 0 len)))
	    (if (= (loop-count lst candidate loop-min-repetitions len) loop-min-repetitions)
		(setq result candidate)
	      (setq len (1- len)))))
	result)

    ;; but when they needn't be consecutive, as soon as one length
    ;; fails, all larger lengths will also fail, so don't bother
    ;; looking for them
    (let ((ret (loops-find-longest-loop lst loop-min-length loop-max-length loop-min-repetitions)))
      (if ret (subseq lst 0 (car ret))))))

(defun loop-count (lst candidate &optional max-count len)
  "Count how often LST contains CANDIDATE.
This counts the repetitions of CANDIDATE at the beginning
of LST, up to a maximum of MAX-COUNT."
  (unless len (setq len (length candidate)))
  (if (not loop-consecutive)
      (loop-count-non-consecutive lst candidate max-count len)
    (loop-count-consecutive lst candidate max-count len)))

(defun loop-count-consecutive (lst candidate max-count len)
;; don't look for more than the maximum number of repetitions possible
  (let ((count 0)
	max-possible)
    (setq max-possible (/ (length lst) len))
    (if max-count (setq max-possible (min max-count max-possible)))

    (while (and (< count max-possible) (not (mismatch candidate lst :end2 len)))
      (setq count (1+ count)
	    lst (nthcdr len lst)))
    count))

(defun loop-count-non-consecutive (lst candidate max-count len)
  (let ((count 0)
	max-possible
	(pos (loop-search candidate lst)))
    (while (and pos (or (not max-count) (< count max-count)))
      (setq count (1+ count)
	    lst (nthcdr (+ pos len) lst)
	    pos (loop-search candidate lst)))
    count))

(defun loop-search (seq lst)
  "Search for SEQ as a subsequence of LST.
Return the index of the leftmost element of the first match found;
return nil if there are no matches.

Examples:

  (loop-search '(1 2 3) '(0 1 2 3 4 5 6))
  => 1

  (loop-search '(1 2 3) '(0 1 2))
  => nil
"
  (let ((pos 0)
	(bad t)
	s l ok)
    (while (and lst bad)
      (setq ok t
	    s seq
	    l lst)
      (while (and s ok)
	(if (equal (car s) (car l))
	    (setq s (cdr s)
		  l (cdr l))
	  (setq ok nil)))
      (if ok
	  (setq bad nil)
	(setq lst (cdr lst)
	      pos (1+ pos))))
    (if bad nil pos)))

(defun loops-match (seq1 seq2 len)
  (let ((ok t))
    (while (and ok (> len 0))
      (if (and (equal (car seq1) (car seq2))
	       (car seq1))
	  (setq len (1- len)
		seq1 (cdr seq1)
		seq2 (cdr seq2))
	(setq ok nil)))
    ok))

;; try to find REPS matches for the first LEN elements of START
;; optional LIST gives elements to search in (otherwise elements immediately after prefix are used
;; optional POS tells the index in the START that LIST can be found
(defun loops-find-initial-occurances (start len reps &optional list pos)
  (let* ((rest (nthcdr len start))
	 occurances)
    (if (not list)
	(setq list (nthcdr len start)
	      pos len))
    (while (and list (> reps 0))
      (if (loops-match start list len)
	    (setq reps (1- reps)
		  list (nthcdr len list)
		  occurances (cons (cons pos list) occurances)
		  pos (+ pos len))
	(setq pos (1+ pos)
	      list (cdr list))))
    (setq ret (if (= reps 0) (cons (cons pos list) (cons rest occurances))))))

(defun loops-find-longest-loop (list min max reps)
  (let* ((ret (loops-find-initial-occurances list min (1- reps)))
	 (next-pos (caar ret))
	 (next-list (cdar ret))
	 (rest (cadr ret))
	 (best-occurances (cddr ret))
	 (occurances best-occurances)
	 (ok t)
	 (len min)
	 new-occurances occurance pos lst)
    (when ret
      (while (and ok (< len max))
	    (while (and occurances ok)
	      (setq occurance (car occurances)
		    pos (car occurance)
		    lst (cdr occurance)
		    occurances (cdr occurances))
	      (if (equal (car lst) (car rest))
		  (setq new-occurances (cons (cons pos (cdr lst)) new-occurances))
		(setq ret (loops-find-initial-occurances list (1+ len) 1 next-list next-pos))
		(if ret
		    (setq next-pos (caar ret)
			  next-list (cdar ret)
			  new-occurances (cons (caddr ret) new-occurances))
		  (setq ok nil))))
	    (if ok (setq rest (cdr rest)
			 best-occurances new-occurances
			 occurances best-occurances
			 len (1+ len)
			 new-occurances nil)))
      (cons len best-occurances))))

;;; Test
(defun string-to-list (str)
  (if (not (equal str ""))
      (cons (substring str 0 1) (string-to-list (substring str 1)))))

(eval-when-compile
  (let ((test '(1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 6 7 8 9 0))
	(loop-consecutive t))
    (assert (= 4 (loop-count test '(1 2 3 4 5))))
    (assert (= 1 (loop-count test '(1 2 3 4))))
    (assert (equal '(1 2 3 4 5) (loop-find-longest test 3 2 6 t))))

  (let ((test '(1 2 3 4 5 a a 1 2 3 4 5 b b b 1 2 3 4 5 c 1 2 3 4 5 d 6 7)))
    (assert (= 4 (loop-count test '(1 2 3 4 5))))
    (assert (= 4 (loop-count test '(1 2 3 4))))
    (assert (equal '(1 2) (loop-find-shortest test 3 2 200)))
    (assert (equal '(1 2 3 4 5) (loop-find-longest test 3 2 200))))

  (assert (loops-match '("a" "b" "c" "d") '("a" "b" "d" "e") 2))
  (assert (not (loops-match '("a" "b" "c" "d") '("a" "b" "d" "e") 3)))
  (assert (not (loops-match '("a") '("a" "b" "d" "e") 2)))
  (assert (not (loops-match '("a" "b" "c" "d") '("a") 2)))

  ;;                                     111111111122222222223333333
  ;;                           0123456789012345678901234567890123456
  (let ((test (string-to-list "abcdabcababcdabcdeabcdafabcaaabcdabde"))
	(min-len 2)
	(max-len 100)
	(min-rep 2))
    ;; what is the longest initial sequence (2 <= length <= 100) which occurs twice?
    (assert (equal (loops-find-longest-loop test min-len max-len min-rep)
		 
		   '(7			;it is length 7
		     ;; it starts at position 9, and is followed by these characters
		     (9 "d" "e" "a" "b" "c" "d" "a" "f" "a" "b" "c" "a" "a" "a" "b" "c" "d" "a" "b" "d" "e"))))

    ;; what is the longest initial sequence (2 <= length <= 100) which occurs 3 times?
    (assert (equal (loops-find-longest-loop test    2      100      3   )
		   '(6			;it is length 6
		     ;; and the 2 non-initial occurances start at positions 9 and 29:
		     (9 "c" "d" "e" "a" "b" "c" "d" "a" "f" "a" "b" "c" "a" "a" "a" "b" "c" "d" "a" "b" "d" "e")
		     (29 "d" "e"))))

    ;; what is the longest initial sequence (4 <= length <= 100) which occurs 3 times?
    (assert (equal (loops-find-longest-loop test    4      100      3   )
		   '(6			;it is length 6
		     ;; and the 2 non-initial occurances start at positions 9 and 29:
		     (29 "d" "e")
		     (9 "c" "d" "e" "a" "b" "c" "d" "a" "f" "a" "b" "c" "a" "a" "a" "b" "c" "d" "a" "b" "d" "e"))))

    ;; what is the longest initial sequence (4 <= length <= 5) which occurs 3 times?
    (assert (equal (loops-find-longest-loop test    4       5       3   )
		   '(5			;it is length 5
		     ;; and the 2 non-initial occurances start at positions 9 and 18:
		     (9 "b" "c" "d" "e" "a" "b" "c" "d" "a" "f" "a" "b" "c" "a" "a" "a" "b" "c" "d" "a" "b" "d" "e")
		     (18 "f" "a" "b" "c" "a" "a" "a" "b" "c" "d" "a" "b" "d" "e"))))

    ;; what is the longest initial sequence (4 <= length <= 4) which occurs 5 times?
    (assert (equal (loops-find-longest-loop test    4       4       5   )
		   '(4			;it is length 5
		     ;; and the 4 non-initial occurances start at positions 29, 18, 13 and 9
		     (29 "a" "b" "d" "e")
		     (18 "a" "f" "a" "b" "c" "a" "a" "a" "b" "c" "d" "a" "b" "d" "e")
		     (13 "e" "a" "b" "c" "d" "a" "f" "a" "b" "c" "a" "a" "a" "b" "c" "d" "a" "b" "d" "e")
		     (9 "a" "b" "c" "d" "e" "a" "b" "c" "d" "a" "f" "a" "b" "c" "a" "a" "a" "b" "c" "d" "a" "b" "d" "e"))))

    ))

(provide 'loops)
;;; loops.el ends here

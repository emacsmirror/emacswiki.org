;;; rational.el --- Rational number in factorial-base format.

;; Copyright (C) 2000, 2001, 2006, 2007 Vinicius Jose Latorre

;; Author:	Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer:	Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords:	data, curiosity, mathematics
;; Time-stamp:	<2007/04/05 01:44:43 vinicius>
;; Version:	1.1.1
;; X-URL:	http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

;; This file is *NOT* (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package was adapted from article:
;;
;;    Error-Free Fractions
;;    Peter Wayner
;;    BYTE, june 1988, pages 289-298
;;
;; This package provides a way to represent a rational number exactly with the
;; use of factorial-base format.
;;
;; To use rational, insert in your ~/.emacs:
;;
;;    (require 'rational)
;;
;; For good performance, be sure to byte-compile rational.el, e.g.
;;
;;    M-x byte-compile-file <give the path to rational.el when prompted>
;;
;; This will generate rational.elc, which will be loaded instead of
;; rational.el.
;;
;; rational was tested with GNU Emacs 20.4.1.
;;
;; Please, read the article for mathematical proof and references.
;;
;;
;; Some Experimental Results
;; -------------------------
;;
;; As the article cited above explains (page 292):
;;
;;    ......
;;    [Consider the Pascal-like code:]
;;
;;    x := 1 / n;
;;    for i := 1 to 30 do
;;	 x := (n + 1) * x - 1;
;;
;;    Mathematically, the function f(x) = (n + 1)x - 1 is invariant at the point
;;    x = 1/n; that is, f(x) = x for x = 1/n.  On paper, then, you would expect
;;    the variable x to remain unchanged after 30 interations of the loop.  This
;;    is the case when I used factorial-base numbers.  But the standard
;;    floating-point system failed badly and returned 286,331,161.6 instead of
;;    0.33333 when n was set to 3.
;;    From previous discussions, you might expect the floating-point software to
;;    find the correct answer at least for n = 10 because 1/10 = 0.1 exactly in
;;    base 10.  This is quite far from the truth: x should be equaled 0.1 but
;;    turned into 2.36378547759e21 after 30 loops.  All the calculations are, of
;;    course, done in binary.  The floating-point software finds the correct
;;    answer only when n is 2.
;;    The only negative aspect of the factorial-base system is the slowness of
;;    the calculations.
;;    ......
;;
;; Try to execute the test:
;;
;;    M-x rational-test RET
;;
;; It's displayed:
;;
;;    *** Rational Test ***
;;
;;    function:  f(x) = (n + 1)x - 1  with  x = 1/n  and  n = 3
;;		 (after 30 interactions of the loop)
;;
;;    Using rational package (factorial-base): 0 . 0 2 0 0 0 0 0 0 0 0 0
;;    Using rational package (decimal-base)  : 0.3333333333333333
;;    Using floating-point                   : -21.0
;;
;;    *********************
;;
;; The floating-point result may differ depending on which machine/environment
;; you run, but the result will not be 0.333333.
;;
;;
;; Using rational
;; --------------
;;
;; Use `rational-customize' to customize rational options by typing:
;;
;;    M-x rational-customize RET
;;
;; You can also bind `rational-customize' to some key, like:
;;
;;    (global-set-key "\C-c\C-c" 'rational-customize)
;;
;; There are the following predefined rational constants: `rational-zero' and
;; `rational-one'.
;;
;; To create a rational number you can use `make-rational',
;; `integer-to-rational' or `rational-convert'.
;;
;; To check if an object is a rational factorial-base number, use `rationalp'.
;;
;; To set a rational number to zero or one, use `rational-zero' or
;; `rational-one' functions, respectively.
;;
;; To set a rational number to any integer, use `rational-set'.
;;
;; To copy a rational, use `rational-copy'.
;;
;; To compare two rationals, use `rational-lessequal', `rational-less' or
;; `rational-equal'.
;;
;; There are the following rational operations: `rational-add',
;; `rational-subtract', `rational-absolute', `rational-negative',
;; `rational-multbyint', `rational-divbyint', `rational-multiply' and
;; `rational-divide'.
;;
;; To translate a rational to another representation, use `rational-to-string'
;; or `rational-to-float'.
;;
;;
;; Options
;; -------
;;
;; The variable `rational-max-size' specifies the maximum array storage for
;; factorial-base number.
;;
;;
;; Things To Change
;; ----------------
;;
;; . At moment, nothing.  Any idea?  Send it!
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code:

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Variables:


;;; Interface to the command system


(defgroup rational nil
  "Rational numbers in factorial-base format"
  :link '(emacs-library-link :tag "Source Lisp File" "rational.el")
  :prefix "rational-"
  :group 'data)


(defcustom rational-max-size 12
  "*Maximum array storage for factorial-base number."
  :type 'integer
  :group 'rational)

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization


;;;###autoload
(defun rational-customize ()
  "Customize rational options."
  (interactive)
  (customize-group 'rational))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User functions


;;;###autoload
(defun make-rational (&optional int)
  "Create a rational factorial-base vector.

The coefficients of a factorial-base number.
Slot i contains a(2 - i).
Slot 1 is used to carry the integer part of the number."
  (let ((rational (make-vector (1+ rational-max-size) 0)))
    (aset rational 0 'rational)
    (and int
	 (aset rational 1 int))
    rational))


;;;###autoload
(defsubst rationalp (x)
  "Return t if X is a rational factorial-base vector."
  (and (vectorp x) (= (length x) (1+ rational-max-size))
       (eq (aref x 0) 'rational)))


(defconst rational-zero (make-rational)
  "Constant zero.")


(defconst rational-one (make-rational 1)
  "Constant one.")


;;;###autoload
(defun rational-set (x int)
  "Set rational number X to integer INT."
  (let ((i rational-max-size))
    (while (> i 1)
      (aset x i 0)
      (setq i (1- i)))
    (aset x 1 int)
    x))


;;;###autoload
(defun rational-zero (x)
  "Set rational number X to 0."
  (rational-set x 0))


;;;###autoload
(defun rational-one (x)
  "Set rational number X to 1."
  (rational-set x 1))


;;;###autoload
(defun rational-copy (x &optional result)
  "Create a copy from rational number X."
  (let ((copy (if (and result (not (eq x result)))
		  result
		(make-rational)))
	(i rational-max-size))
    (while (> i 0)
      (aset copy i (aref x i))
      (setq i (1- i)))
    copy))


;;;###autoload
(defun integer-to-rational (p &optional result)
  "Convert integer P into a factorial-base number."
  (if result
      (rational-set result p)
    (make-rational p)))


;;;###autoload
(defun rational-convert (p q &optional result)
  "Convert (integers) P and Q into a factorial-base number."
  (let ((rational (or result (make-rational)))
	(i 2))
    (aset rational 1 (/ p q))
    (and (< p 0) (/= (% p q) 0)
	 (aset rational 1 (1- (aref rational 1))))
    (setq p (% p q))
    (while (<= i rational-max-size)
      (setq p (* p i))
      (aset rational i (/ p q))
      (setq p (% p q)
	    i (1+ i)))
    rational))


;;;###autoload
(defun rational-lessequal (x y)
  "Return t if X and Y are rational and X <= Y."
  (let ((i (rational-differ x y)))
    (<= (aref x i) (aref y i))))


;;;###autoload
(defun rational-less (x y)
  "Return t if X and Y are rational and X < Y."
  (let ((i (rational-differ x y)))
    (< (aref x i) (aref y i))))


;;;###autoload
(defun rational-equal (x y)
  "Return t if X and Y are rational and X = Y."
  (let ((i (rational-differ x y)))
    (= (aref x i) (aref y i))))


;;;###autoload
(defun rational-add (x y &optional result)
  "Add rational numbers X and Y."
  (let ((temp (or result (make-rational)))
	(i 1))
    (while (<= i rational-max-size)
      (aset temp i (+ (aref x i) (aref y i)))
      (setq i (1+ i)))
    (rational-smooth temp)))


;;;###autoload
(defun rational-subtract (x y &optional result)
  "Subtract rational number X from Y."
  (let ((temp (or result (make-rational)))
	(i 1))
    (while (<= i rational-max-size)
      (aset temp i (- (aref x i) (aref y i)))
      (setq i (1+ i)))
    (rational-smooth temp)))


;; used by `rational-absolute'
(defvar rational-abs (make-rational))


;;;###autoload
(defun rational-absolute (x &optional result)
  "Return the absolute value of rational number X.
This is trickier than flipping a bit because the sign bit is attached to x[1].
So if x[1]<0 then compute -x[1] subtract the rest of the terms x[2..n]."
  (cond ((< (aref x 1) 0)
	 (rational-zero rational-abs)
	 (aset rational-abs 1 (- (aref x 1)))
	 (aset x 1 0)
	 (rational-subtract rational-abs x result))
	((eq x result)
	 x)
	(t
	 (rational-copy x result))
	))


;;;###autoload
(defun rational-negative (x &optional result)
  "Convert a positive rational number X to negative form."
  (let ((temp (- -1 (aref x 1))))
    (aset x 1 0)
    (setq result (rational-subtract rational-one x result))
    (aset result 1 temp)
    result))


;;;###autoload
(defun rational-multbyint (x int &optional result)
  "Multiply rational number X by an integer INT."
  (let ((temp (or result (make-rational)))
	(i 1))
    (while (<= i rational-max-size)
      (aset temp i (* (aref x i) int))
      (setq i (1+ i)))
    (rational-smooth temp)))


;;;###autoload
(defun rational-divbyint (x int &optional result)
  "Divide rational number X by an integer INT."
  (let ((temp (or result (make-rational)))
	(negative (< (aref x 1) 0))
	(i 1)
	(carry 0))
    (and negative
	 (setq x (rational-absolute x)))
    (while (<= i rational-max-size)
      (let ((part (+ (aref x i) (* carry i))))
	(setq carry (% part int))
	(aset temp i (/ part int)))
      (setq i (1+ i)))
    (rational-smooth temp)
    (if negative
	(rational-negative temp temp)
      temp)))


;; used by `rational-multiply' and `rational-divide'
(defvar rational-partial (make-rational))
(defvar rational-temp    (make-rational))
(defvar rational-x       (make-rational))
(defvar rational-y       (make-rational))


;;;###autoload
(defun rational-multiply (x y &optional result)
  "Multiply ratinal numbers X and Y."
  (let ((i 1))
    (rational-copy y rational-y)
    (rational-zero rational-partial)
    (while (<= i rational-max-size)
      ;; shift y over one decimal place
      (rational-divbyint rational-y i rational-y)
      ;; now temp contains y*(x[i]/i!)
      (rational-multbyint rational-y (aref x i) rational-temp)
      ;; add it and continue
      (rational-add rational-partial rational-temp rational-partial)
      (setq i (1+ i)))
    (rational-copy rational-partial result)))


;;;###autoload
(defun rational-divide (x y &optional result)
  "Divide ratinal numbers X by Y.

Begins by scaling the numbers to find an easy, accurate way of computing the
first value.  After that it proceeds to use long division."
  (let ((negative (or (and (< (aref x 1) 0) (> (aref y 1) 0))
		      (and (> (aref x 1) 0) (< (aref y 1) 0))))
	(i 1)
	(posit 1)
	(denomfact 1)
	(denom (aref y 1)))
    (rational-absolute x rational-x)
    (rational-absolute y rational-y)
    (while (and (< denom 100) (< i 7))
      ;; get approximately 3 significant figures
      (setq i         (1+ i)
	    denom     (+ (* denom i) (aref rational-y i))
	    denomfact (* denomfact i)))
    ;; do loop until rational-x = 0 for best accuracy
    (rational-zero rational-partial)
    (while (and (not (rational-equal rational-x rational-zero))
		(<= posit rational-max-size))
      (let ((estimate (/ (* (aref rational-x 1) denomfact) denom)))
	(while (progn
		 (rational-multbyint rational-y estimate rational-temp)
		 (setq estimate (1- estimate))
		 (rational-lessequal rational-temp rational-x)))
	;; record the result
	(aset rational-partial posit (1+ estimate)))
      ;; calculate the remainder
      (rational-subtract rational-x rational-temp rational-x)
      ;; move over one notch
      (setq posit (1+ posit))
      ;; shift the numerator over one notch
      (rational-multbyint rational-x posit rational-x))
    (rational-smooth rational-partial)
    (if negative
	(rational-negative rational-partial result)
      (rational-copy rational-partial result))))


;;;###autoload
(defun rational-to-string (x)
  "Convert rational number X to string."
  (let ((i 2)
	str)
    (rational-copy x rational-x)
    (when (< (aref rational-x 1) 0)
      ;; the numbers are stored in the form  sum x[i]/i!  so convert to an
      ;; equivalent positive number by subtracting the x[2..max] terms from
      ;; -x[1], the integer part of the number.
      (rational-zero rational-y)
      (aset rational-y 1 (- (aref rational-x 1)))
      (aset rational-x 1 0)
      (rational-subtract rational-y rational-x rational-x)
      (setq str "-"))
    (setq str (concat str (number-to-string (aref rational-x 1)) " ."))
    (while (<= i rational-max-size)
      (setq str (concat str " " (number-to-string (aref rational-x i)))
	    i   (1+ i)))
    str))


;;;###autoload
(defun rational-to-float (x)
  "Convert rational number X to float."
  (let ((float (aref x 1))
	(fact  1.0)
	(i     2)
	(negative (< (aref x 1) 0)))
    (rational-copy x rational-x)
    (when negative
      ;; the numbers are stored in the form  sum x[i]/i!  so convert to an
      ;; equivalent positive number by subtracting the x[2..max] terms from
      ;; -x[1], the integer part of the number.
      (rational-zero rational-y)
      (aset rational-y 1 (- (aref rational-x 1)))
      (aset rational-x 1 0)
      (rational-subtract rational-y rational-x rational-x))
    (while (<= i rational-max-size)
      (setq fact  (* fact i)
	    float (+ float (/ (aref rational-x i) fact))
	    i     (1+ i)))
    (if negative
	(- float)
      float)))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions


(defun rational-smooth (x)
  "Take rational number X and does all the carrying and the borrowing."
  (let ((i rational-max-size))
    (while (>= i 2)
      (aset x (1- i) (+ (aref x (1- i)) (/ (aref x i) i)))
      (and (< (aref x i) 0) (/= (% (aref x i) i) 0)
	   (aset x (1- i) (1- (aref x (1- i)))))
      (aset x i (% (aref x i) i))
      (setq i (1- i)))
    x))


(defun rational-differ (x y)
  "Return index where X and Y differ."
  (let ((i 1))
    (while (and (< i rational-max-size) (= (aref x i) (aref y i)))
      (setq i (1+ i)))
    i))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test


(defun rational-test (&optional no-display)
  "Rational test.

Consider the Pascal-like code:

   x := 1 / n;
   for i := 1 to 30 do
      x := (n + 1) * x - 1;

Mathematically, the function f(x) = (n + 1)x - 1 is invariant at the point
x = 1/n; that is, f(x) = x for x = 1/n.  On paper, then, you would expect the
variable x to remain unchanged after 30 interations of the loop.

This routine does the above test using rational package and floating-point.
Returns a string with the results."
  (interactive)
  (let ((float (/ 1.0 3.0))
	(a     (rational-divbyint rational-one 3))
	(i     30))
    (while (> i 0)
      (rational-multbyint a 4 a)
      (rational-subtract a rational-one a)
      (setq float (- (* 4.0 float) 1.0)
	    i     (1- i)))
    (let ((str
	   (concat "\n*** Rational Test ***\n"
		   "\nfunction:  f(x) = (n + 1)x - 1  with  x = 1/n  and  n = 3"
		   "\n           (after 30 interactions of the loop)\n"
		   "\nUsing rational package (factorial-base): "
		   (rational-to-string a)
		   "\nUsing rational package (decimal-base)  : "
		   (number-to-string (rational-to-float a))
		   "\nUsing floating-point                   : "
		   (number-to-string float)
		   "\n\n*********************\n")))
      (or no-display
	  ;; Display the data
	  (with-output-to-temp-buffer " *Rational Test*"
	    (princ str)
	    (print-help-return-message)))
      str)))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'rational)


;;; rational.el ends here

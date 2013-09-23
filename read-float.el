;;; Copyright (C) 2012  Tobias.Naehring -- Read float values in binary format from literal buffer

;; Author: Tobias.Naehring <i@tn-home.de>
;; Keywords: Search for mouse selection.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Function `read-float' reads floating point numbers in binary format
;; from buffer. Leading most significant bit is assumed.

;; Just experimental.
;; 

(defvar IEEE754-types
  ;; number of bits:
  ;; overall, mantissa-length
  '(
    (single 32 23) ;; 1bit sign, 8bits exponent
    (double 64 52) ;; 1bit sign, 11bits exponent
    )
  "Number formats.
Each entry is a list of the form (fmt size mantissa).
fmt is a symbol denoting the floating point number format
size is the overall size in bits
mantissa is the length of the mantissa")
;; additionally there is one sign bit
;; the remainding number of bits are for the exponent

(require 'calc)

(defun ash-bignum (x n)
  "Multiply the math-bignum x by 2^n.
Works for positive and negative n.
The fractional part is truncated."
  (when (integerp x)
    (setq x (math-bignum-big x)))
  (if (< n 0)
      (loop for i from n to -1 do
	    (setq x (math-div2-bignum x)))
    (loop for i from 1 to n do
	  (setq x (math-mul-bignum x '(2)))))
  x)

(defun read-number-bits (v b n &optional big)
  "Read binary number from vector V of bytes.
\(most significant bit first)
B is the begin of the bit sequence counting from zero
N is the number of bits.
If big is non-nil result is a math-bignum."
  (let* ((b8 (/ b 8)) ;; first byte used (counting from zero)
	 (e8 (/ (+ b n 7) 8)) ;; end byte: not used byte (counting from zero)
	 (db8 (- (* (1+ b8) 8) b)) ;; significant bits in first byte
	 (s (- n db8))
	 (c (logand (aref v b8) (- (ash 1 db8) 1)))
	 (x (if big (ash-bignum c s) (ash c s))))
    (setq b8 (1+ b8))
    (while (< b8 e8)
      (setq s (- s 8))
      (setq c (aref v b8))
      (if big
	  (setq x (math-add-bignum x (ash-bignum c s)))
	(setq x (+ x (ash c s))))
      (setq b8 (1+ b8)))
    x))

(defun bignum-to-float (x)
  (let ((r 0.0))
    (mapc #'(lambda (d)
	     (setq r (+ (* r math-bignum-digit-size) d)))
	  (reverse x))
    r))

(defun pow2 (n)
  "Calculate 2^n as a floating point number."
  (let ((x 1.0))
  (if (> n 0)
      (loop for i from 1 to n do (setq x (* x 2.0)))
    (loop for i from n to -1 do (setq x (/ x 2.0))))
  x))

(defun vreverse (v)
  "Reverse entries of vector v."
  (apply 'vector (nreverse (append v nil))))

(defun read-float (fmt &optional pos buf littleEndian)
  "Read floating point number in binary format FMT from buffer
BUF at position POS (which defaults to point). After success
point is behind the read bytes. If you want to read a binary file
make sure that the buffer is unibyte. You can
use (set-buffer-multibyte nil) for that."
  (unless buf
    (setq buf (current-buffer)))
  (with-current-buffer buf
    (when pos (goto-char pos))
    ;; extract sign (highest bit)
    (if (symbolp fmt)
	(setq fmt (cdr (assoc fmt IEEE754-types))))
    (let* ((size (car fmt))
	   (sizeBytes (/ size 8))
	   (mantissa-size (cadr fmt))
	   (exp-size (- size mantissa-size 1))
	   (bytes (apply 'vector (let ((b (point))
				       (e (+ (point) sizeBytes -1)))
				   (if littleEndian
				       (loop for i from e downto b collect (get-byte i))
				     (loop for i from b to e collect (get-byte i))))))
	   (sign (logand #b10000000 (aref bytes 0)))
	   (exp (read-number-bits bytes 1 exp-size)) ;; it is assumed that the exponential fits into an int
	   (NAN-exp (1- (ash 1 exp-size)))
	   (digits (read-number-bits bytes (1+ exp-size) mantissa-size 'big))) ;; mantissa may exceed int-size
      (forward-char sizeBytes)
      (case exp
	(0 ;; denormalized numbers
	 (/ (bignum-to-float digits) (pow2 mantissa-size)))
	(NAN-exp ;; nan and inf
	 ;; todo
	 0.0)
	(t ;; normalized number
	 (* (1+ (/ (bignum-to-float digits) (bignum-to-float (ash-bignum 1 mantissa-size))))
	    (pow2 (- exp (ash 1 (1- exp-size)) -1))
	    (if (= sign 0) 1 -1)))
	))))

;; Test:
(when nil
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read 0.0F:
  (with-temp-buffer (set-buffer-multibyte nil)
    ;;........SEEEEEEE
    (insert #b00000000
	    ;;EMMMMMMM
	    #b00000000
	    ;;MMMMMMMM
	    #b00000000
	    ;;MMMMMMMM
	    #b00000000)
    (read-float 'single 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read 1.0F:
  (with-temp-buffer (set-buffer-multibyte nil)
    ;;........SEEEEEEE
    (insert #b00111111
	    ;;EMMMMMMM
	    #b10000000
	    ;;MMMMMMMM
	    #b00000000
	    ;;MMMMMMMM
	    #b00000000)
    (read-float 'single 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read -1.0F:
  (with-temp-buffer (set-buffer-multibyte nil)
    (insert #b10111111
	    ;;EMMMMMMM
	    #b10000000
	    ;;MMMMMMMM
	    #b00000000
	    ;;MMMMMMMM
	    #b00000000)
    ;;........SEEEEEEE
    (read-float 'single 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read 2.0F:
  (with-temp-buffer (set-buffer-multibyte nil)
    (insert #b01000000
	    ;;EMMMMMMM
	    #b00000000
	    ;;MMMMMMMM
	    #b00000000
	    ;;MMMMMMMM
	    #b00000000)
    ;;........SEEEEEEE
    (read-float 'single 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read 0.5F:
  (with-temp-buffer (set-buffer-multibyte nil)
    ;;........SEEEEEEE
    (insert #b00111111
	    ;;EMMMMMMM
	    #b00000000
	    ;;MMMMMMMM
	    #b00000000
	    ;;MMMMMMMM
	    #b00000000)
    (read-float 'single 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read 0.0:
  (with-temp-buffer (set-buffer-multibyte nil)
    ;;........S0123456
    ;;........SEEEEEEE
    (insert #b00000000
	    ;;789A0123
	    ;;EEEEMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    )
    (read-float 'double 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read 1.0:
  (with-temp-buffer (set-buffer-multibyte nil)
    ;;........S0123456
    ;;........SEEEEEEE
    (insert #b00111111
	    ;;789A0123
	    ;;EEEEMMMM
	    #b11110000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    )
    (read-float 'double 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read 2.0:
  (with-temp-buffer (set-buffer-multibyte nil)
    ;;........S0123456
    ;;........SEEEEEEE
    (insert #b01000000
	    ;;789A0123
	    ;;EEEEMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    )
    (read-float 'double 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read 0.5:
  (with-temp-buffer (set-buffer-multibyte nil)
    ;;........S0123456
    ;;........SEEEEEEE
    (insert #b00111111
	    ;;789A0123
	    ;;EEEEMMMM
	    #b11100000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    )
    (read-float 'double 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read -0.5:
  (with-temp-buffer (set-buffer-multibyte nil)
    ;;........S0123456
    ;;........SEEEEEEE
    (insert #b10111111
	    ;;789A0123
	    ;;EEEEMMMM
	    #b11100000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    )
    (read-float 'double 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read 1.5:
  (with-temp-buffer (set-buffer-multibyte nil)
    ;;........S0123456
    ;;........SEEEEEEE
    (insert #b00111111
	    ;;789A0123
	    ;;EEEEMMMM
	    #b11111000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    )
    (read-float 'double 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read 0.75d:
  (with-temp-buffer (set-buffer-multibyte nil)
    ;;........S0123456
    ;;........SEEEEEEE
    (insert #b00111111
	    ;;789A0123
	    ;;EEEEMMMM
	    #b11101000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    )
    (read-float 'double 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read 1+eps:
  (with-temp-buffer (set-buffer-multibyte nil)
    ;;........S0123456
    ;;........SEEEEEEE
    (insert #b00111111
	    ;;789A0123
	    ;;EEEEMMMM
	    #b11110000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000000
	    ;;456789AB
	    ;;MMMMMMMM
	    #b00000000
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b00000001
	    )
    (read-float 'double 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read 0.9:
  (with-temp-buffer (set-buffer-multibyte nil)
    ;;........S0123456
    ;;........SEEEEEEE
    (insert #b00111111
	    ;;789A0123
	    ;;EEEEMMMM
	    #b11101100
	    ;;456789AB
	    ;;MMMMMMMM
	    #b11001100
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b11001100
	    ;;456789AB
	    ;;MMMMMMMM
	    #b11001100
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b11001100
	    ;;456789AB
	    ;;MMMMMMMM
	    #b11001100
	    ;;CDEF0123
	    ;;MMMMMMMM
	    #b11001101
	    )
    (read-float 'double 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read a buffer "file1.bin" full of binary double numbers.
  (with-temp-buffer
    (set-buffer-file-coding-system 'binary)
    (insert-file-contents-literally "/c/temp/34/protocol/file3.bin")
    (set-buffer-multibyte nil)
    (let ((output (get-buffer-create "*test*")))
      (with-current-buffer output (erase-buffer))
      (goto-char (point-min))
      (let (num)
	(while (/= (point) (point-max))
	  (setq num (read-float 'double nil nil 'littleEndian))
	  (with-current-buffer output
	    (insert (format "%.16g\n" num)))))))
  )

(provide 'read-float)
;; read-float.el ends here

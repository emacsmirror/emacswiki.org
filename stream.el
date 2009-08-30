;;; stream.el -- a stream library in elisp

;;; Licence: 
;;
;; Copyright (C) 2002 Akimichi Tatsukawa <akimichi@mbox.co.jp>
;;
;; This file is NOT a part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;     
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;     
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.
;;
;; Please send suggestions and bug reports to <akimichi@mbox.co.jp>. 
;; The latest version of this package should be available at
;;
;;    <URL:http://akimichi.homeunix.net/~emile/aki/program/elisp/>

;;  Author: Akimichi Tatsukawa <akimichi@mbox.co.jp>
;;  Keywords: stream, delayed evaluation
;;  $Id:  $
;;  $Date:  $
;;  $Log:  $

;;; Commentary:
;;    This package provides a stream library.
;;    Please refer to the chapter 3.5 of the book "Structure and Interpretation of Computer Program"(SICP).
;;; Usage:
;;    This package needs enough max-specpdl-size.

;;    (setq max-specpdl-size 10000)
;;    (load-file "/home/emile/develop/elisp/stream.el")
;;    (display-stream (stream-map #'(lambda (i) (sqrt i)) (stream-enumerate-interval 100 120)))
;;    (display-stream  (stream-filter #'(lambda (i) (oddp i)) (stream-enumerate-interval 100 120)))
;;    (stream-ref (stream-filter #'(lambda (i) (oddp i)) integers) 10)

;; delayed evaluation
(defun force (a)
  (if (functionp a)
      (funcall a)
    a))

(defmacro delay (a)
  `#'(lambda () ,a))

;; stream functions
(defmacro cons-stream  (a b)
  `(cons ,a (delay ,b)))

(defun stream-car (stream)
  (car stream))

(defun stream-cdr (stream)
  (force (cdr stream)))


(defun stream-ref (s n)
  (if (= n 0)
      (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(defmacro stream-map (proc s)
  `(if (null ,s)
       nil
     (cons-stream (funcall ,proc (stream-car ,s))
		  (stream-map ,proc (stream-cdr ,s)))))

(defmacro stream-for-each (proc s)
  `(if (null ,s)
       'done
     (funcall ,proc (stream-car ,s))
     (stream-for-each ,proc (stream-cdr ,s))))
(defun display-stream (s)
  (stream-for-each #'(lambda (x) (message "%s" x)) s))

; (defmacro display-line ()
;   `#'(lambda (x) (message "%s" ,x)))

(defmacro stream-filter (pred stream)
  `(cond ((null ,stream)
	  nil)
	 ((funcall ,pred (stream-car ,stream))
	  (cons-stream (stream-car ,stream)
		       (stream-filter ,pred (stream-cdr ,stream))))
	 (t
	  (stream-filter ,pred (stream-cdr ,stream)))))
    
(defmacro stream-enumerate-interval (low high)
  `(if (> ,low ,high)
       nil
     (cons-stream
      ,low
      (stream-enumerate-interval (+ ,low 1) ,high))))
	      
;; infinite stream
(defmacro integers-starting-from (n)
  `(cons-stream ,n (integers-starting-from (+ ,n 1))))
(setq integers (integers-starting-from 1))



; (defmacro skimmer (n)
;   `(lambda () (expt ,n 2)))
; (defun thinker (proc)
;   (funcall proc))

; (defmacro encapsulate (form)
;   `(lambda () ,form))
; (defmacro expose (proc)
;   `(funcall ,proc))

; (defun skimmer (n)
;   (encapsulate (expt n 2)))

; (defun thinker (proc)
;   (expose proc))

(provide 'stream)


;;; Local Variables:
;;; End:


;;; stream.el ends here



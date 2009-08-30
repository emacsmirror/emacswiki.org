;;; constraint.el -- a constraint propagation library in elisp

;;; Licence: 
;;
;;  Copyright (C) 2002 Akimichi Tatsukawa <akimichi@mbox.co.jp>
;;
;;  This file is NOT a part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License as
;;  published by the Free Software Foundation; either version 2 of
;;  the License, or (at your option) any later version.
;;     
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;  GNU General Public License for more details.
;;     
;;  You should have received a copy of the GNU General Public
;;  License along with this program; if not, write to the Free
;;  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;  MA 02111-1307, USA.
;;
;;  Please send suggestions and bug reports to <akimichi@mbox.co.jp>. 
;;  The latest version of this package should be available at
;;
;;     <URL:http://akimichi.homeunix.net/~emile/program/elisp/>

;;  Author: Akimichi Tatsukawa <akimichi@mbox.co.jp>
;;  Keywords: constraint
;;  $Id: constraint.el,v 1.3 2002/04/18 03:04:38 emile Exp emile $
;;  $Date: 2002/04/18 03:04:38 $
;;  $Log: constraint.el,v $
;;  Revision 1.3  2002/04/18 03:04:38  emile
;;  assertion using obj-of-class-p is changed to same-class-p.
;;  function c/ is modified.
;;
;;  Revision 1.2  2002/04/04 02:29:05  emile
;;  celsius-fahrenheit-converter succeeded.
;;
;;  Revision 1.1  2002/04/04 02:04:29  emile
;;  Initial revision
;;

;;; Commentary:
;;    This package provides a constraint propagation library.
;;    Please refer to the chapter 3 of the book "Structure and Interpretation of Computer Program"(SICP).
;;; Usage:
;;    This package requires cl and eieio packages.
;;    Following examples descrived at SICP book will help.

; (load-file "/home/emile/develop/elisp/constraint.el")
; (setq c (make-connector))
; (setq f (make-connector))
; (celsius-fahrenheit-converter c f)
; (make-probe "C" c)
; (make-probe "F" f)
; (set-value c 25.0 'user)
; (forget-value c 'user)

; (load-file "/home/emile/develop/elisp/constraint.el")
; (setq c (make-connector))
; (setq f (celsius2fahrenheit c))
; (celsius-fahrenheit-converter c f)
; (make-probe "C" c)
; (make-probe "F" f)
; (set-value c 25.0 'user)
; (forget-value c 'user)


;; TODO:

;;; Code:

(require 'cl)
(require 'eieio)

(defvar constraint-version "0.1"
  "Current version of constraint.el.")


;; constraint classes
(defclass constraint ()
  ((output :initarg :output
	   :type connector
	   :accessor get-output
	   :documentation ""))
  "most basic constraint superclass")

(defclass binary-constraint (constraint)
  ()
  "binary constraint superclass")

;; ternary-constraint constraint class
(defclass ternary-constraint (constraint)
  ((input-a :initarg :input-a
	    :type connector
	    :accessor get-input-a
	    :documentation "")
   (input-b :initarg :input-b
	    :type connector
	    :accessor get-input-b
	    :documentation ""))
  "ternary constraint superclass")

(defmethod request ((this ternary-constraint) message)
  (cond ((eq message 'I-have-a-value)
	 (process-new-value this))
	((eq message 'I-lost-my-value)
	 (process-forget-value this))
	(t
	 (error "Unknown request %s -- ADDER" message))))

(defmethod process-forget-value ((this ternary-constraint))
  (forget-value (get-input-a this) this)
  (forget-value (get-input-b this) this)
  (forget-value (get-output this) this)
  (process-new-value this))

;; adder constraint class
(defclass adder (ternary-constraint)
  ((dummy))
  "adder class")

(defun make-adder (a1 a2 sum)
  (let ((adder-constraint (adder "adder" :input-a a1 :input-b a2 :output sum)))
    (connect a1 adder-constraint)
    (connect a2 adder-constraint)
    (connect sum adder-constraint)))

(defmethod process-new-value ((this adder))
  (let ((a1 (get-input-a this))
	(a2 (get-input-b this))
	(sum (get-output this)))
    (assert (obj-of-class-p a1 'connector))
    (assert (obj-of-class-p a2 'connector))
    (assert (obj-of-class-p sum 'connector))
;     (assert (same-class-p a1 'connector))
;     (assert (same-class-p a2 'connector))
;     (assert (same-class-p sum 'connector))
    (cond ((and (has-value a1) (has-value a2))
	   (set-value sum
		      (+ (get-value a1) (get-value a2))
		      this))
	  ((and (has-value a1) (has-value sum))
	   (set-value a2
		      (- (get-value sum) (get-value a1))
		      this))
	  ((and (has-value a2) (has-value sum))
	   (set-value a1
		      (- (get-value sum) (get-value a2))
		      this)))))
;; multiplier constraint class
(defclass multiplier (ternary-constraint)
  ()
  "multiplier class")

(defun make-multiplier (a1 a2 product)
  (let ((multiplier-constraint (multiplier "multiplier" :input-a a1 :input-b a2 :output product)))
    (connect a1 multiplier-constraint)
    (connect a2 multiplier-constraint)
    (connect product multiplier-constraint)))

(defmethod process-new-value ((this multiplier))
  (let ((a1 (get-input-a this))
	(a2 (get-input-b this))
	(product (get-output this)))
    (assert (obj-of-class-p a1 'connector))
    (assert (obj-of-class-p a2 'connector))
    (assert (obj-of-class-p product 'connector))
;     (assert (same-class-p a1 'connector))
;     (assert (same-class-p a2 'connector))
;     (assert (same-class-p product 'connector))
    (cond ((and (has-value a1) (has-value a2))
	   (set-value product
		      (* (get-value a1) (get-value a2))
		      this))
	  ((and (has-value a1) (has-value product))
	   (set-value a2
		      (/ (get-value product) (get-value a1))
		      this))
	  ((and (has-value a2) (has-value product))
	   (set-value a1
		      (/ (get-value product) (get-value a2))
		      this)))))


(setq inform-about-value
      (function (lambda (constraint)
		  (request constraint 'I-have-a-value))))
(setq inform-about-no-value
      (function (lambda (constraint)
		  (request constraint 'I-lost-my-value))))

;; constant class
(defclass constant (constraint)
  ()
  "constant class")

(defmethod request ((this constant) message)
  (error "Unknown request %s -- CONSTANT.request" message))


(defun make-constant (value connector)
  (let ((constant-constraint (constant "constant")))
    (connect connector constant-constraint)
    (set-value connector value constant-constraint)
    connector))


;; probe class
(defclass probe (constraint)
  ((name :initarg :name
	  :accessor get-name
	  :documentation "")
   (connector :initarg :connector
	      :accessor get-connector
	      :documentation ""))
  "probe class")

(defun make-probe (name connector)
  (let ((probe-constraint (probe "probe" :name name :connector connector)))
    (connect connector probe-constraint)))

(defmethod print-probe ((this probe) value)
  (message "Probe: %s = %s" (get-name this) value))

(defmethod process-new-value ((this probe))
  (print-probe this (get-value (get-connector this))))

(defmethod process-forget-value ((this probe))
  (print-probe this "?"))

(defmethod request ((this probe) message)
  (cond ((eq message 'I-have-a-value)
	 (process-new-value this))
	((eq message 'I-lost-my-value)
	 (process-forget-value this))
	(t
	 (error "Unknown request %s -- PROBE.request" message))))

;; connector class
(defclass connector ()
  ((value :initarg :value
	  :initform nil
	  :accessor get-value
	  :documentation "")
   (informant :initarg :informant
	      :initform false
	      :accessor get-informant
	      :documentation "")
   (constraints :initarg :constraints
		:initform '()
		:accessor get-constraints
		:documentation "Surrounding constraints around this connector"))
  "connector class")

(defun make-connector ()
  (connector "connector" ))

(defmethod has-value ((this connector))
  (if (eq (get-informant this) 'false)
      nil
    t))

(defmethod value ((this connector))
  (get-value this))

(defmethod set-value ((this connector) newval setter)
  (let ((oldval (get-value this)))
    (cond ((not (has-value this))
	   (oset this value newval)
	   (oset this informant setter)
	   (for-each-except setter
			    inform-about-value
			    (get-constraints this)))
	  ((not (= oldval newval))
;	   (error "Contradiction - %s" (list oldval newval)))
	   (message "Contradiction - %s" (list oldval newval)))
	  (t
	   'ignored))))
(defmethod forget-value ((this connector) retractor)
;  (message "retractor -> %s, (get-informant this) ->  %s\n" retractor (get-informant this))
  (cond ((eq retractor (get-informant this))
	 (oset this informant 'false)
	 (for-each-except retractor
			  inform-about-no-value
			  (get-constraints this)))
	(t
	 'ignored)))

(defmethod connect ((this connector) new-constraint)
  (if (not (memq new-constraint (get-constraints this)))
      (oset this constraints (cons new-constraint (get-constraints this))))
  (if (has-value this)
      (funcall inform-about-value new-constraint))
  'done)

(defun for-each-except (exception procedure list)
  (loop for i in list
	if (not (eq i exception))
	do (funcall procedure i)
	end))

;; example
(defun celsius-fahrenheit-converter (c f)
  (let ((u (make-connector))
	(v (make-connector))
	(w (make-connector))
	(x (make-connector))
	(y (make-connector)))
    (make-multiplier c w u)
    (make-multiplier v x u)
    (make-adder v y f)
    (make-constant 9.0 w)
    (make-constant 5.0 x)
    (make-constant 32.0 y)
    'ok))

(defun celsius2fahrenheit (c)
  (c+ (c* (c/ (cv 9.0) (cv 5.0))
  	  c)
      (cv 32.0)))
	   
   
(defun c+ (x y)
  (let ((z (make-connector)))
    (make-adder x y z)
    z))

(defun c* (x y)
  (let ((z (make-connector)))
    (make-multiplier x y z)
    z))
(defun c/ (z y)
  (let ((x (make-connector)))
    (make-multiplier x y z)
    x))
; (defun c/ (x z) not accurate
;   (let ((y (make-connector))
; 	(1/y (make-connector)))
;     (make-multiplier x y z)
;     (make-multiplier y 1/y (cv 1.0))
;     1/y))

(defun cv (val)
  (let ((x (make-connector)))
    (make-constant val x)))

(provide 'constraint)


;;; Local Variables:
;;; End:


;;; constraint.el ends here



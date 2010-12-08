;;; :NOTE cl-array.el is as of 2010-11-30 available at web.archive.org
;;; :SEE (URL `http://web.archive.org/web/20010617092325/http://mit.edu/cadet/www/cl-array.el')
;;; ==============================

;;; cl-array.el	-- Common Lisp style implementation of multi-dimensional arrays --
;;  Thursday Jun 2 1999

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: David Bakhash <cadet@alum.edu>
;; Maintainer: David Bakhash <cadet@alum.edu>
;; Version: 0.4
;; Created: Wed Jun 2 08:49:22 EST 1999
;; Keywords: extensions, lisp

;; This file is not part of emacs or XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; XEmacs program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This package is designed as an API for multi-dimensional arrays.  It
;; strives to conform to the Common Lisp specification for
;; multi-dimensional arrays.  The limitations of this package are a
;; direct result of the limitations of Emacs Lisp.

;; Some things to note.  If you're going to use this package, to port
;; real CL code to (X)Emacs, then you must replace the following function
;; calls:

;; aref          ==>     aref*
;; arrayp        ==>     arrayp*

;; This last one is the most crucial since, if you don't, the code
;; will not err when it should, since I'm using elisp `vectors' to
;; store the array structures.  This being done, the code should work
;; (with respect to arrays), provided that they're not using
;; fill-pointers, displacement, etc.

;;  Here are some examples of how it's used...

;; (setq 3D-array (make-array '(5 8 10) :initial-element 0))
;; (arrayp* 3D-array)
;; ==> t
;; (vectorp* 3D-array)
;; ==> nil
;; (array-in-bounds-p 3D-array 1 1 1)
;; ==> t
;; (array-in-bounds-p 3D-array 20 20 20)
;; ==> nil
;; (aref* 3D-array 2 4 6)
;; ==> 0
;; (aref* 3D-array 2 4 6)
;; ==> 10
;; (setf (aref* 3D-array 10 20 30) 5)
;; ==> ERROR
;; (setf (aref* 3D-array 2 4 6) 5)
;; ==> 5
;; (loop for i below (array-total-size 3D-array)
;;   do
;;   (setf (row-major-aref 3D-array i) (* i i)))
;; (row-major-aref 3D-array 12)
;; ==> 144
;; (array-rank 3D-array)
;; ==> 3
;; (array-dimension 3D-array 2)
;; ==> 8
;; (array-total-size 3D-array)
;; ==> 400

(require 'cl)

;;(defvar cl-array-do-bounds-checking-p t
;;  "*Non-nil causes bounds checking to occur for cl-arrays.
;;Default is t.")

(defstruct
  (cl-array
   (:type list) :named
   (:copier nil))		; disable copier (use copy-sequence instead???)
  (dimensions :read-only t)
  elements)

;; I was thinking of adding advice (defadvice) to the function
;; `arrayp'.  But on second thought, I figured that since I just stick
;; with the cl.el convention of appending a `*' to functions whose
;; names already exist in elisp, but which should be upgraded for CL,
;; I will, in this package, not use defadvice.

;;(defun length* (object)
;;  (cond ((cl-array-p object)
;;	 (if (= (array-rank object) 1)
;;	     (if (array-has-fill-pointer-p object)
;;		 (fill-pointer object)
;;	       (length (cl-array-elements object)))
;;	   (error "Function `length' does not work for multi-dimensional arrays")))
;;	(t				; else
;;	 (length object))))

;; I think I really have to think about this one...

;; (defalias 'arrayp* 'arrayp)

(defun arrayp* (object)
  "Returns t iff OBJECT is any type of array, either CL-style, or elisp-style."
  (or (cl-array-p object)
      (arrayp object)))

(defun* make-array (dimensions &key
			       (initial-element nil ie-p)
			       (initial-contents nil ic-p)
;;			       (element-type t et-p)
;;			       (fill-pointer nil fp-p)
			       &allow-other-keys)
  "Return a newly created multi-dimensional array with dimesions DIMENSIONS.
Optional keys that can be specified are:

:initial-element   INITIAL-ELEMENT sets each element of the array to
                   INITIAL-ELEMENT

:initial-contents  INITIAL-CONTENTS also sets the initial contents of
                   the array.  This should be a sequence of nested
                   sequences which describes the structure of the array.

Example: \(setq my-matrix \(make-array '\(2 4\)
                                     :initial-contents '\(\(1 2 3 4\)
                                                         \(5 6 7 8\)\)\)\)

Example: \(setq 3D-array \(make-array '\(5 8 10\) :initial-element 0\)\)
will make a rank-3 array with all elements initialized to 0.
Note: INITIAL-CONTENTS and INITIAL-ELEMENT cannot both be specified."
  (when (atom dimensions)
    (setq dimensions (list dimensions)))
  (let ((cl-array (cond ((rest dimensions)
			 ;; rank > 1
			 (make-cl-array
			  :dimensions dimensions
			  :elements (make-vector (apply '* dimensions)
			  initial-element)))
			(t ; rank <= 1
			 (make-vector (first dimensions) initial-element)))))
    (cond ((and ie-p ic-p)
	   (error "Function `make-array': cannot specifiy both :initial-element and :initial-contents"))
	  (ic-p
	   ;; initial-contents is composed of a nested structure of
	   ;; sequences.  The numbers of levels in the structure must
	   ;; equal the rank of array.  Each leaf of the nested
	   ;; structure must be of the type given by element-type.  If
	   ;; array is zero-dimensional, then initial-contents
	   ;; specifies the single element.  Otherwise,
	   ;; initial-contents must be a sequence whose length is
	   ;; equal to the first dimension; each element must be a
	   ;; nested structure for an array whose dimensions are the
	   ;; remaining dimensions, and so on.  Initial-contents
	   ;; cannot be supplied if either initial-element is supplied
	   ;; or displaced-to is non-nil.  If initial-contents is not
	   ;; supplied, the consequences of later reading an
	   ;; uninitialized element of new-array are undefined unless
	   ;; either initial-element is supplied or displaced-to is
	   ;; non-nil.
	   (let ((rank (length dimensions)))
	     (labels ((setr! (subscripts level subseqs)
			(cond ((= level rank)
			       (cl-aset cl-array subscripts subseqs))
			      (t ; else
			       (incf level)
			       ;; check dimensions...
			       (unless (= (nth (1- level) dimensions)
					  (length subseqs))
				 (error ":initial-contents sequence argument in `make-array' must have proper dimensions"))
			       (loop
				 for i below (nth (1- level) dimensions)
				 for subseq = (elt subseqs i)
				 do
				 (setr! (append subscripts (list i)) level subseq))))))
	       (setr! '() 0 initial-contents)))))
    cl-array))

(defun adjustable-array-p (cl-array)
  "Returns nil since adjustable arrays have not been implemented yet."
  nil)

(defun adjust-array (&rest args)
  "Not implemented yet, but will (eventually) look like CL's
`adjust-array'."
  (error "Function `adjust-array' not implemented yet."))

(defun array-has-fill-pointer-p (cl-array)
  "Will always return nil since fill-pointers have not been
implemented yet."
  nil)

(defun fill-pointer (cl-array)
  "Not implemented yet, but may eventually be a like CL's `fill-pointer'."
  (error "Function `fill-pointer' not implemented yet."))

(defun array-element-type (cl-array)
  "Since elisp doesn't have types, this function assumes that the
type of an array is `t'.
An alternative definition could be to return the type of the first
element of the array."
;;  (type-of (row-major-aref cl-array 0)))
  t)

(defun array-in-bounds-p (cl-array &rest subscripts)
  "Returns t iff SUBSCRIPTS are all legal subscripts for CL-ARRAY."
  (let ((dims (array-dimensions cl-array))
	(rank (array-rank cl-array)))
    (and (= rank (length subscripts))
;;	 (every #'identity (mapcar* #'< subscripts dims)))))
	 (loop
	   for s in subscripts
	   and d in dims
	   always (< s d)))))

(defun array-dimension (cl-array axis-number)
  "Returns the length of AXIS-NUMBER for CL-ARRAY."
  (nth axis-number (array-dimensions cl-array)))

(defun array-dimensions (cl-array)
  "Return the dimensions CL-ARRAY in a list."
  (cond ((cl-array-p cl-array)
	 (cl-array-dimensions cl-array))
	 ;; #### shoud I type-check this, or just assume it's an elisp array?
	(t ; (arrayp cl-array)
	 (list (length cl-array)))))

(defun aref* (cl-array &rest subscripts)
  "Return the element of CL-ARRAY at position specified by SUBSCRIPTS.
This function is settable.
Example: \(setq x \(aref* 3D-array 2 4 6\)\)
This function also accepts regular elisp arrays."
  (cond ((cl-array-p cl-array)
	 ;; #### start by doing bounds checking...
	 ;; #### If I decide to re-instantiate the bounds checking, then
	 ;; #### I'll un-comment the `when' clause below.  It's a speed hit.
;;	 (when cl-array-do-bounds-checking-p
;;	   (unless (apply 'array-in-bounds-p cl-array subscripts)
;;	     (error "aref*: out-of-bounds array reference")))
	 (let* ((linear-ref (apply #'array-row-major-index cl-array subscripts)))
	   (aref (cl-array-elements cl-array) linear-ref)))
	;; #### Should I do type checking ??? ####
	(t ; (arrayp cl-array)
	 ;; #### Note: I don't check to make sure that `subscripts' is 
	 ;; #### length = 1 !!! ####
	 (aref cl-array (first subscripts)))))

(defun cl-aset (cl-array subscripts newval)
  ;; #### start by doing bounds checking...
  ;; #### If I decide to re-instantiate the bounds checking, then
  ;; #### I'll un-comment the `when' clause below.  It's a speed hit.
  ;;	 (when cl-array-do-bounds-checking-p
  ;;	   (unless (apply 'cl-array-in-bounds-p cl-array subscripts)
  ;;	     (error "cl-aset: out-of-bounds array reference")))
  (cond ((cl-array-p cl-array)
	 (let* ((linear-ref (apply #'array-row-major-index cl-array subscripts)))
	   (aset (cl-array-elements cl-array) linear-ref newval)))
	;; #### shoud I type-check this, or just assume it's an elisp array?
	(t ; (arrayp cl-array)
	 (aset cl-array (first subscripts) newval))))

(defsetf aref* (cl-array &rest subscripts) (newval)
  `(cl-aset ,cl-array (list ,@subscripts) ,newval))

(defun simple-vector-p (cl-array)
  "Until fill-pointers are implemented, this will always return t if
CL-ARRAY is a rank one array."
  (= (array-rank cl-array) 1))

(defun svref (cl-vector i)
  "Returns the ith element of (simple) CL-VECTOR. Settable."
  (cond ((= (array-rank cl-vector) 1)
	 (aref cl-vector i))
	(t ; else
	 (error "First argument to `svref' must be a simple vector."))))

(defsetf svref (cl-vector i) (newval)
  `(cl-aset ,cl-vector (list ,i) ,newval))

(defun array-row-major-index (cl-array &rest subscripts)
  "Return the index that `row-major-aref' would take which is
equivalent to SUBSCRIPTS."
  (let ((dims (array-dimensions cl-array)))
    (loop for i in subscripts
      for j on dims
      sum (* i (apply '* (cdr j))))))

(defun row-major-aref (cl-array index)
  "Treat the CL-ARRAY as a single array, indexed by a single number.
Indexed starting at 0.
This function is settable."
  (aref (if (cl-array-p cl-array)
	    (cl-array-elements cl-array)
	  cl-array)
	index))

(defun cl-row-major-aset (cl-array index newval)
  (setf (aref (if (cl-array-p cl-array)
		  (cl-array-elements cl-array)
		cl-array)
	      index)
    newval))

(defsetf row-major-aref (cl-array index) (newval)
  `(cl-row-major-aset ,cl-array ,index ,newval))

(defun array-rank (cl-array)
  "Returns the rank, or number of dimensions \(axes\) of CL-ARRAY."
  (cond ((cl-array-p cl-array)
	 (length (cl-array-dimensions cl-array)))
	;; #### shoud I type-check this, or just assume it's an elisp array?
	(t ; (arrayp cl-array)
	 1)))

(defun array-total-size (cl-array)
  "Returns the total number of elements in CL-ARRAY.
This is the same as the product of all the dimensions."
  (cond ((cl-array-p cl-array)
	 ;; (apply '* (cl-array-dimensions cl-array)))
	 (length (cl-array-elements cl-array)))
	 ;; #### Should I do type checking ??? ####
	(t ; (arrayp cl-array)
	 (length cl-array))))

(provide 'cl-array)

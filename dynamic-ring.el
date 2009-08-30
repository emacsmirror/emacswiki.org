;;; dynamic-ring.el --- A dynamically sized ring structure.

;; Copyright (C) 2009 Mike Mattie
;; Author: Mike Mattie codermattie@gmail.com
;; Maintainer: Mike Mattie codermattie@gmail.com
;; Created: 2009-4-16
;; Version: 0.0.2

;; This file is NOT a part of Gnu Emacs.

;; License: GPL-v3

;; dynamic-ring.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defconst dynamic-ring-version "0.0.2" "dynamic-ring version")

(eval-when-compile
  (require 'cl))

;;
;; ring structure
;;

(defun make-dyn-ring ()
  "make-dyn-ring

   Return a new dynamic ring stucture. A ring structure is a cons
   cell where the car is linked to the current head element of
   the ring, and the cdr is the number of elements in the ring.
  "
  (cons nil 0))

(defun dyn-ring-empty-p ( ring-struct )
  "dyn-ring-empty-p RING

   return t if RING has no elements.
  "
  (not (car ring-struct)))

(defun dyn-ring-size ( ring-struct )
  "dyn-ring-size RING

   Return the number of elements in RING.
  "
  (cdr ring-struct))

(defun dyn-ring-value ( ring-struct )
  "dyn-ring-value RING

   Return the value of RING's head element.
  "
  (when (car ring-struct)
    (aref (car ring-struct) dyn-ring-value)))

;;
;; ring elements
;;

(defconst dyn-ring-linkage 0)
(defconst dyn-ring-value   1)

(defun dyn-ring-make-element ( value )
  "dyn-ring-make-element VALUE

   Create a new dynamic ring element with VALUE.

   An element stores a value within a ring with linkage to the
   other elements in the ring. It is an array.

   [linkage,value]

   linkage is a cons cell. The car points to the left element in
   the ring. The cdr points to the right element in the ring.
  "
  (let
    ((new-elm (make-vector 2 nil)))
    (aset new-elm dyn-ring-value value)
    (aset new-elm dyn-ring-linkage (cons nil nil))
    new-elm))

(defun dyn-ring-element-value ( element  )
  "dyn-ring-element-value ELEMENT

   Return the value of ELEMENT.
   "
  (aref element dyn-ring-value))

(defun dyn-ring-set-element-value ( element value )
  "dyn-ring-set-element-value ELEMENT VALUE

   Set the value of ELEMENT to VALUE.
  "
  (aset element dyn-ring-value value))

;;
;; ring traversal.
;;

(defun dyn-ring-traverse ( ring-struct fn )
  "dyn-ring-traverse RING FN

   walk all of the elements in RING passing each
   element to FN.
  "
  (let
    ((head (car ring-struct)))

    (when head
      (funcall fn head)

      (let
        ((current (cdr (aref head dyn-ring-linkage))))

        ;; loop until we return to the head
        (while (and current (not (eq current head)))
          (funcall fn current)
          (setq current (cdr (aref current dyn-ring-linkage))))
        t))))

(defun dyn-ring-map ( ring-struct map-fn )
  "dyn-ring-map RING FN

   Walk the elements of RING passing each element to FN.  The
   values of FN for each element is collected into a list and
   returned.
  "
  (lexical-let
    ((output nil))

    (dyn-ring-traverse ring-struct
      (lambda ( element )
        (push (funcall map-fn (dyn-ring-element-value element)) output)))

    output))

(defun dyn-ring-rotate-until ( ring-struct direction fn )
  "dyn-ring-rotate-until RING DIRECTION FN

   Rotate the head of RING in DIRECTION which is one of two
   functions: dyn-ring-rotate-right or dyn-ring-rotate-left.

   The rotation continues until FN predicate which evaluates the
   new head element of each rotation returns non-nil.

   If the predicate does not return non-nil the ring is reset to
   the head element it started with.
  "
  (let
    ((start (car ring-struct)))

    (catch 'stop

      (when start
        (when (< (dyn-ring-size ring-struct) 2) (throw 'stop nil))

        (funcall direction ring-struct)

        ;; when we have moved off the start loop until we return to it.
        (while (not (eq (car ring-struct) start))
          (when (funcall fn (dyn-ring-value ring-struct)) (throw 'stop t))
          (funcall direction ring-struct))

        ;; if nothing is found reset the head back to the original value
        (setcar ring-struct start)
        nil)) ))

(defun dyn-ring-find ( ring-struct predicate )
  "dyn-ring-find RING PREDICATE

   Search RING for elements matching PREDICATE, a function that
   evaluates non-nil for for the desired elements.

   The list of matching elements is returned.
  "
  (lexical-let
    ((found nil)
     (p     predicate))

    (dyn-ring-traverse ring-struct
      (lambda ( element )
        (when (funcall p element)
          (push element found))))

      found))

;;
;; ring modification functions.
;;

(defun dyn-ring-head-linkage ( ring-struct )
  "dyn-ring-head-linkage RING

   Return the linkage of the head element.
  "

  (let
    ((head (car ring-struct)))
    (when head (aref head dyn-ring-linkage))))

(defun dyn-ring-destroy ( ring-struct )
  "dyn-ring-destroy  RING

   - INTERNAL -

   Delete the RING. The circular linkage of a ring structure
   makes it doubtful that the garbage collector will be able to
   free a ring without calling dyn-ring-destroy.
  "
  (let
    ((linkage (dyn-ring-head-linkage ring-struct)))

    (when linkage

      (if (cdr linkage)
        (progn
          ;; There is more than one element. Break the ring by
          ;; terminating the left element
          (setcdr (cdr linkage) nil)

        (while (cdr linkage)
          (let
            ((right (cdr linkage)))

            ;; delete all the links in the current element
            (setcdr linkage nil)
            (setcar linkage nil)

            ;; move to the right
            (setq linkage (aref right dyn-ring-linkage)) )))
        ;; only one link, so delete the head pointer.
        (setcar ring-struct nil)) )))

(defun dyn-ring-link ( left element right )
  "dyn-ring-link LEFT ELEMENT RIGHT

   - INTERNAL -

   Insert ELEMENT between LEFT and RIGHT by relinking
   LEFT RIGHT and ELEMENT.
  "
  (let
    ((insert-linkage (aref element dyn-ring-linkage)))

    ;; double link the left side
    (setcdr (aref left dyn-ring-linkage) element)
    (setcar insert-linkage left)

    ;; double link the right side
    (setcar (aref right dyn-ring-linkage) element)
    (setcdr insert-linkage right)))

(defun dyn-ring-insert ( ring-struct insert )
  "dyn-ring-insert RING ELEMENT

   Insert ELEMENT into RING. The head of the ring
   will point to the new ELEMENT
  "
  (let
    ((ring-size (dyn-ring-size ring-struct))
     (head-linkage (dyn-ring-head-linkage ring-struct)))

    (when head-linkage
      (let
        ((insert-linkage (aref insert dyn-ring-linkage)))

        (cond
          ((equal 1 ring-size)
            (progn
              ;; a single element is a special case as both
              ;; the left and right linkage are nil.

              ;; link the existing element to the new element
              (setcar head-linkage insert)
              (setcdr head-linkage insert)

              ;; link the new element to the head
              (setcar insert-linkage (car ring-struct))
              (setcdr insert-linkage (car ring-struct))))

          ((equal 2 ring-size)
            ;; two elements is a special case because both
            ;; elements point to each other.
            (let
              ((other  (car head-linkage))
               (head   (car ring-struct)))

              (dyn-ring-link other insert head)))

          ((> ring-size 2)
            (let
              ((left  (car head-linkage))
               (right (cdr head-linkage)))

              (dyn-ring-link left insert right))) ) )))

  ;; point the head at the new element
  (setcar ring-struct insert)
  ;; update the element count.
  (setcdr ring-struct (+ (cdr ring-struct) 1))

  ;; return the newly inserted element.
  insert)

(defun dyn-ring-link-left-to-right ( left right )
  "dyn-ring-link-left-to-right.

   - INTERNAL -

   Link elements LEFT and RIGHT to each other.  This is used for
   deleting elements from a ring.
  "
  (setcdr (aref left  dyn-ring-linkage) right)
  (setcar (aref right dyn-ring-linkage) left))

(defun dyn-ring-unlink-element ( element )
  "dyn-ring-unlink-element

   - INTENRAL -

   Unlink ELEMENT. by relinking it's left and right elements to
   each other.
  "
  (let
    ((linkage (aref element dyn-ring-linkage)))

    (dyn-ring-link-left-to-right (car linkage) (cdr linkage))
    (cdr linkage) ))

(defun dyn-ring-delete ( ring-struct element )
  "dyn-ring-delete RING ELEMENT

   Delete ELEMENT from RING.
  "
  (let
    ((ring-size (dyn-ring-size ring-struct)))

    (cond
      ((equal 0 ring-size) nil)
      ((equal 1 ring-size)
        ;; when there is only one element we ignore element and simply
        ;; delete the head and reset the size.
        (progn
          (setcar ring-struct nil)
          (setcdr ring-struct 0)
          t))
      ((equal 2 ring-size)
        ;; if there are two elements nullify the other element's links
        ;; and reset both head and the size.
        (progn
          (let*
            ((other   (car  (aref element dyn-ring-linkage)))
             (linkage (aref other dyn-ring-linkage)))

            (setcar linkage nil)
            (setcdr linkage nil)

            (setcar ring-struct other)
            (setcdr ring-struct 1) )
          t))

      ;; for three or more elements perform a unlink.
      (t
        (let
          ((right (dyn-ring-unlink-element element)))

          ;; if we deleted the head element set the
          ;; head to the right element.
          (when (eq (car ring-struct) element)
            (setcar ring-struct right))

          (setcdr ring-struct (- (cdr ring-struct) 1))
          t)) )))

(defun dyn-ring-rotate ( ring-struct direction )
  "dyn-ring-rotate RING DIRECTION

   - INTERNAL -

   This is an internal function. To rotate the ring use
   dyn-ring-rotate-left or dyn-ring-rotate-right.

   Rotate the ring in DIRECTION: left = 'car , right = 'cdr.
   If the ring is empty nil is returned. If the ring
   has a single element the element is returned.

   Otherwise the head of the ring is set to the element
   of DIRECTION, and the element is returned.
  "
  (let
    ((linkage (dyn-ring-head-linkage ring-struct)))

    (when linkage
      (let
        ((link-to (funcall direction linkage)))

        (if link-to
          (setcar ring-struct link-to)) )) ))

(defun dyn-ring-rotate-left ( ring-struct )
  "dyn-ring-rotate-left RING

   Rotate the head of ring to the element left of the current
   head.
  "
  (dyn-ring-rotate ring-struct 'car))

(defun dyn-ring-rotate-right ( ring-struct )
  "dyn-ring-rotate-right RING

   Rotate the head of ring to the element right of the current
   head.
  "
  (dyn-ring-rotate ring-struct 'cdr))

(provide 'dynamic-ring)
;;; dynamic-ring.el ends here

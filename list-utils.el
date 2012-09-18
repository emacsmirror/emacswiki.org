;;; list-utils.el --- List-manipulation utility functions
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/list-utils
;; URL: http://raw.github.com/rolandwalker/list-utils/master/list-utils.el
;; Version: 0.1.0
;; Last-Updated: 17 Sep 2012
;; EmacsWiki: ListUtils
;; Keywords:
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'list-utils)
;;
;;     (list-utils-flatten '(1 2 (3 4 (5 6 7))))
;;
;;     (let ((cyclic-list '(1 2 3 4 5 6 7)))
;;       (nconc cyclic-list (cdr cyclic-list))
;;       (list-utils-flatten cyclic-list))
;;
;;     (list-utils-cyclic-p '(1 2 3))
;;
;;     (list-utils-plist-del '(:one 1 :two 2 :three 3) :two)
;;
;; Explanation
;;
;; List-utils is a collection of functions for list manipulation.
;; This library has no user-level interface; it is only useful
;; for programming in Emacs Lisp.
;;
;; Notable functionality includes
;;
;;     * `list-utils-flatten', a robust list-flattener which handles
;;       cyclic lists, non-nil-terminated lists, and preserves nils
;;       when they are found as list elements.
;;
;;     * `tconc', a simple data structure for efficiently appending
;;       to a list
;;
;; The following functions are provided:
;;
;;     `make-tconc'
;;     `tconc-p'
;;     `tconc-list'
;;     `tconc'
;;     `list-utils-cons-cell-p'
;;     `list-utils-cyclic-length'
;;     `list-utils-linear-p'
;;     `list-utils-linear-subseq'
;;     `list-utils-cyclic-p'
;;     `list-utils-cyclic-subseq'
;;     `list-utils-safe-length'
;;     `list-utils-flatten'
;;     `list-utils-alist-flatten'
;;     `list-utils-insert-before'
;;     `list-utils-insert-after'
;;     `list-utils-insert-before-pos'
;;     `list-utils-insert-after-pos'
;;     `list-utils-plist-reverse'
;;     `list-utils-plist-del'
;;
;; To use list-utils, place the list-utils.el library somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'list-utils)
;;
;; Notes
;;
;;     This library includes an implementation of the classic LISP
;;     `tconc' which is outside the "list-utils-" namespace.
;;
;; Compatibility and Requirements
;;
;;     Tested on GNU Emacs versions 23.3 and 24.1
;;
;;     No external dependencies
;;
;; Bugs
;;
;; TODO
;;
;;; License
;;
;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Roland Walker "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Roland Walker or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Roland Walker.
;;
;;; Code:
;;

;;; requires

;; for defstruct, assert, setf, callf
(require 'cl)

;;; tconc - this section of code is in the public domain

;;;###autoload
(defstruct tconc head tail)

;;;###autoload
(defun tconc-list (tc list)
  "Efficiently append LIST to TC.

TC is a data structure created by `make-tconc'."
  (assert (tconc-p tc) nil "TC must be created by `make-tconc'.")
  (when list
    (if (null (tconc-tail tc))
        (setf (tconc-head tc) list)
      ;; else
      (setcdr (tconc-tail tc) list))
    (setf (tconc-tail tc) (last list)))
  (tconc-head tc))

;;;###autoload
(defun tconc (tc &rest args)
  "Efficiently append ARGS to TC.

TC is a data structure created by `make-tconc'

Without ARGS, return the list held by TC."
  (tconc-list tc args))

;;; lists

;;;###autoload
(defun list-utils-cons-cell-p (cell)
  "Return non-nil if CELL holds a cons cell rather than a proper list.

A proper list is defined as a series of cons cells in which the
cdr slot of each cons holds a pointer to the next element of the
list, and the cdr slot in the final cons holds nil.

A plain cons cell, for the purpose of this function, is a single
cons in which the cdr holds data rather than a pointer to the
next cons cell, eg

    '(1 . 2)

In addition, a list which is not nil-terminated is not a proper
list and will be recognized by this function as a cons cell.
Such a list is using dot notation for the last two elements, eg

    '(1 2 3 4 . 5)"
  (let ((len (safe-length cell)))
    (when (and (consp cell)
               (> len 0)
               (not (listp (nthcdr len cell))))
      (nthcdr len cell))))

;;;###autoload
(defun list-utils-cyclic-length (list)
  "Return the number of cyclic elements in LIST.

If some portion of LIST is linear, only the cyclic
elements will be counted.

If LIST is completely linear, return 0."
  (if (list-utils-cons-cell-p list)
      0
    ;;else
    (let ((fast list)
          (slow list)
          (counter 0))
      (catch 'cycle
        (while slow
          (incf counter)
          (setq slow (cdr slow))
          (setq fast (cdr (cdr fast)))
          (when (eq slow list)
            (throw 'cycle t))
          (when (eq slow fast)
            (setq list slow)
            (setq counter 0)
            (setq fast nil))))
      counter)))

;;;###autoload
(defun list-utils-cyclic-p (list &optional perfect)
  "Return non-nil if LIST contains any cyclic structures.

If optional PERFECT is set, only return non-nil if LIST is a
perfect non-branching cycle in the last element points to the
first."
  (let ((cycle (list-utils-cyclic-subseq list)))
    (when (or (not perfect)
              (not (list-utils-linear-subseq list (list-utils-cyclic-length cycle))))
        cycle)))

;;;###autoload
(defun list-utils-linear-p (list)
  "Return non-nil if LIST is linear (no cyclic structure)."
  (not (list-utils-cyclic-subseq list)))

;;;###autoload
(defun list-utils-linear-subseq (list &optional cycle-length)
  "Return the linear elements from a partially cyclic LIST.

If there is no cycle in LIST, return LIST.  If all elements of
LIST are included in a cycle, return nil.

As an optimization, CYCLE-LENGTH may be specified if the length
of the cyclic portion is already known.  Otherwise it will be
calculated from LIST."
  (callf or cycle-length (list-utils-cyclic-length list))
  (if (= 0 cycle-length)
      list
    ;; else
    (let ((behind list)
          (ahead (nthcdr cycle-length list))
          (linear-subseq nil))
      (catch 'cycle
        (while behind
          (when (eq ahead behind)
            (throw 'cycle t))
          (push (car behind) linear-subseq)
          (setq ahead (cdr ahead))
          (setq behind (cdr behind))))
      (nreverse linear-subseq))))

;;;###autoload
(defun list-utils-cyclic-subseq (list &optional from-start)
  "Return any cyclic elements from LIST as a circular list.

The first element of the cyclic structure is not guaranteed to be
first element of the return value unless FROM-START is non-nil.

To linearize the return value, use `list-utils-flatten'.

If there is no cycle in LIST, return nil."
  (cond
    ((list-utils-cons-cell-p list)
     nil)
    (from-start
     (nthcdr (length (list-utils-linear-subseq list)) list))
    (t
     (let ((fast list)
           (slow list))
       (catch 'cycle
         (while (cdr fast)
           (setq fast (cdr (cdr fast)))
           (setq slow (cdr slow))
           (when (eq slow fast)
             (throw 'cycle slow))))))))

;;;###autoload
(defun list-utils-safe-length (list)
  "Return the number of elements in LIST.

LIST may be linear or cyclic.

If LIST is not really a list, returns 0."
  (if (not (listp list))
      0
    ;; else
    (let ((cycle-length (list-utils-cyclic-length list)))
      (+ cycle-length
         (length (list-utils-linear-subseq list cycle-length))))))

;;;###autoload
(defun list-utils-flatten (list)
  "Flatten LIST which may contain other lists.

This function flattens cons cells as lists, and
flattens circular list structures."

  (when (and (listp list)
             (list-utils-cyclic-subseq list))
    (setq list (subseq list 0 (list-utils-safe-length list))))

  (cond

    ((null list)
     nil)

    ((and (listp list)
          (consp (car list)))
     (append (list-utils-flatten (car list))
             (list-utils-flatten (cdr list))))

    ((listp list)
     (cons (car list)
           (list-utils-flatten (cdr list))))

    (t
     (list list))))

;;;###autoload
(defun list-utils-insert-before (list element new-element)
  "Look in LIST for ELEMENT and insert NEW-ELEMENT before it.

LIST is modified and the new value is returned."
  (let ((pos (position element list)))
    (assert pos nil "Element not found: %s" element)
    (push new-element (nthcdr pos list)))
  list)

;;;###autoload
(defun list-utils-insert-after (list element new-element)
  "Look in LIST for ELEMENT and insert NEW-ELEMENT after it.

LIST is modified and the new value is returned."
  (let ((pos (position element list)))
    (assert pos nil "Element not found: %s" element)
    (push new-element (cdr (nthcdr pos list))))
  list)

;;;###autoload
(defun list-utils-insert-before-pos (list pos new-element)
  "Look in LIST for position POS, and insert NEW-ELEMENT before.

POS is zero-indexed.

LIST is modified and the new value is returned."
  (assert (and (integerp pos)
               (>= pos 0)
               (< pos (length list))) nil "No such position %s" pos)
  (push new-element
        (nthcdr pos list))
  list)

;;;###autoload
(defun list-utils-insert-after-pos (list pos new-element)
  "Look in LIST for position POS, and insert NEW-ELEMENT after.

LIST is modified and the new value is returned."
  (assert (and (integerp pos)
               (>= pos 0)
               (< pos (length list))) nil "No such position %s" pos)
  (push new-element
        (cdr (nthcdr pos list)))
  list)

;;; alists

;;;###autoload
(defun list-utils-alist-flatten (list)
  "Flatten LIST which may contain other lists.  Do not flatten cons cells.

It is not guaranteed that the result contains *only* cons cells.
The result could contain other data types present in LIST.

This function simply avoids flattening single conses or improper
lists where the last two elements would be expressed as a dotted
pair."

  (when (and (listp list)
             (list-utils-cyclic-subseq list))
    (setq list (subseq list 0 (list-utils-safe-length list))))

  (cond

    ((null list)
     nil)

    ((list-utils-cons-cell-p list)
     list)

    ((and (listp list)
          (consp (car list))
          (not (list-utils-cons-cell-p (car list))))
     (append (list-utils-alist-flatten (car list))
             (list-utils-alist-flatten (cdr list))))

    ((listp list)
     (cons (car list)
           (list-utils-alist-flatten (cdr list))))

    (t
     (list list))))

;;; plists

;;;###autoload
(defun list-utils-plist-reverse (plist)
  "Return reversed copy of property-list PLIST, maintaining pair associations."
  (assert (= 0 (% (length plist) 2)) nil "Not a PLIST")
  (let ((alist nil))
    (while plist
      (push (cons (pop plist) (pop plist)) alist))
    (list-utils-flatten alist)))

;;;###autoload
(defun list-utils-plist-del (plist prop)
  "Delete from PLIST the property PROP and its associated value.

When PROP is not present in PLIST, there is no effect.

The new plist is returned; use `(setq x (list-utils-plist-del x prop))'
to be sure to use the new value.

This functionality overlaps with the undocumented `cl-do-remf'."
  (let ((prop-pos (position prop plist)))
    (when (and prop-pos
               (= 0 (% prop-pos 2)))
      (setf (nthcdr prop-pos plist) (nthcdr (+ 2 prop-pos) plist))))
    plist)

(provide 'list-utils)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;
;; LocalWords: ListUtils ARGS alist utils nconc tconc defstruct setf
;; LocalWords: plists PLIST setq autoloading plist callf
;;

;;; list-utils.el ends here

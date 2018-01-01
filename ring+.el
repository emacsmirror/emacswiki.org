;;; ring+.el --- Extensions to `ring.el'.
;;
;; Filename: ring+.el
;; Description: Extensions to `ring.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2018, Drew Adams, all rights reserved.
;; Created: Thu Apr 11 16:46:04 1996
;; Version: 0
;; Last-Updated: Mon Jan  1 15:33:44 2018 (-0800)
;;           By: dradams
;;     Update #: 233
;; URL: https://www.emacswiki.org/emacs/download/ring%2b.el
;; Doc URL: https://emacswiki.org/emacs/RingPlus
;; Keywords: extensions, lisp, emacs-lisp
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x
;;
;; Features that might be required by this library:
;;
;;   `ring'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `ring.el'.
;;
;;  The code in this library is part of GNU Emacs 23 and later, so
;;  this library is useful only for releases prior to Emacs 23.
;;
;;  Main new functions here:
;;
;;    `ring-convert-sequence-to-ring', `ring-insert+extend',
;;    `ring-remove+insert+extend', `ring-member', `ring-next',
;;    `ring-previous'.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `ring.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "ring" '(progn (require 'ring+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/01/04 dadams
;;     Removed autoload cookies (non-interactive commands).
;; 2004/09/26 dadams
;;     Renamed convert-sequence-to-ring to ring-convert-sequence-to-ring
;; 2004/09/08 dadams
;;     Reversed argument order: ring-member, ring-next, ring-previous.
;; 2004/09/04 dadams
;;     Added: convert-sequence-to-ring, ring-insert+extend.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'ring) ;; ring-length, ring-ref, ring-remove, ring-insert

;;;;;;;;;;;;;;;;;

(defun ring-member (ring item)
  "Return index of ITEM if on RING, else nil.
Comparison is done via `equal'.  The index is 0-based."
  (catch 'found
    (dotimes (ind (ring-length ring) nil)
      (when (equal item (ring-ref ring ind))
        (throw 'found ind)))))

(defun ring-next (ring item)
  "Return the next item in the RING, after ITEM.
Raise error if ITEM is not in the RING."
  (let ((curr-index (ring-member ring item)))
    (unless curr-index (error "Item is not in the ring: `%s'" item))
    (ring-ref ring (ring-plus1 curr-index (ring-length ring)))))

(defun ring-previous (ring item)
  "Return the previous item in the RING, before ITEM.
Raise error if ITEM is not in the RING."
  (let ((curr-index (ring-member ring item)))
    (unless curr-index (error "Item is not in the ring: `%s'" item))
    (ring-ref ring (ring-minus1 curr-index (ring-length ring)))))


(defun ring-insert+extend (ring item &optional grow-p)
  "Like ring-insert, but if GROW-P is non-nil, then enlarge ring.
Insert onto ring RING the item ITEM, as the newest (last) item.
If the ring is full, behavior depends on GROW-P:
  If GROW-P is non-nil, enlarge the ring to accommodate the new item.
  If GROW-P is nil, dump the oldest item to make room for the new."
  (let* ((vec (cdr (cdr ring)))
         (veclen (length vec))
         (hd (car ring))
         (ringlen (ring-length ring)))
    (prog1
        (cond ((and grow-p (= ringlen veclen)) ; Full ring.  Enlarge it.
               (setq veclen (1+ veclen))
               (setcdr ring (cons (setq ringlen (1+ ringlen))
                                  (setq vec (vconcat vec (vector item)))))
               (setcar ring hd))
              (t (aset vec (mod (+ hd ringlen) veclen) item)))
      (if (= ringlen veclen)
          (setcar ring (ring-plus1 hd veclen))
        (setcar (cdr ring) (1+ ringlen))))))

(defun ring-remove+insert+extend (ring item &optional grow-p)
  "`ring-remove' ITEM from RING, then `ring-insert+extend' it.
This ensures that there is only one ITEM on RING.

If the RING is full, behavior depends on GROW-P:
  If GROW-P is non-nil, enlarge the ring to accommodate the new ITEM.
  If GROW-P is nil, dump the oldest item to make room for the new."
  (let (ind)
    (while (setq ind (ring-member ring item)) (ring-remove ring ind)))
  (ring-insert+extend ring item grow-p))

(defun ring-convert-sequence-to-ring (seq)
  "Convert sequence SEQ to a ring.  Return the ring.
If SEQ is already a ring, return it."
  (if (ring-p seq)
      seq
    (let* ((size (length seq))
           (ring (make-ring size))
           (count 0))
      (while (< count size)
        (if (or (ring-empty-p ring)
                (not (equal (ring-ref ring 0) (elt seq count))))
            (ring-insert-at-beginning ring (elt seq count)))
        (setq count (1+ count)))
      ring)))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ring+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ring+.el ends here

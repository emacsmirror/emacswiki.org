;;; zones.el --- Zones of text - like multiple regions.
;; 
;; Filename: zones.el
;; Description: Zones of text - like multiple regions.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2015, Drew Adams, all rights reserved.
;; Created: Tue Aug  4 08:54:06 2015 (-0700)
;; Version: 2015.08.08
;; Package-Requires: ()
;; Last-Updated: Sun Aug  9 15:42:17 2015 (-0700)
;;           By: dradams
;;     Update #: 179
;; URL: http://www.emacswiki.org/zones.el
;; Doc URL: http://www.emacswiki.org/Zones
;; Keywords: region zone
;; Compatibility: GNU Emacs 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;    Zones of text - like multiple regions.
;;
;;  A zone is a list of two buffer positions followed by a possibly
;;  empty list of extra information: (POS1 POS2 . EXTRA).  A zone
;;  represents the text between its two positions, just as an Emacs
;;  region is the text between point and mark.
;;
;;  The positions of a zone can be natural numbers (1, 2, 3,...) or
;;  markers from the same buffer.  (Behavior is undefined if a zone
;;  has markers from different buffers.)
;;
;;  Zone union and intersection operations (`zzz-zone-union',
;;  `zzz-zone-intersection') each act on a list of zones, returning
;;  another such list, but which has POS1 <= POS2 in each of its
;;  zones, and which lists its zones in ascending order of their cars.
;;
;;  The extra info in the zones resulting from zone union or zone
;;  intersection is just the set union or set intersection of the
;;  extra info in the zones that are combined.
;;
;;  See also library `wide-n.el', which provides some commands that
;;  make use of `zones.el'.
;;
;;
;;  Non-interactive functions defined here:
;;
;;    `zzz-buffer-of-markers', `zzz-car-<', `zzz-zone-complement',
;;    `zzz-every', `zzz-max', `zzz-min', `zzz-ordered-zone',
;;    `zzz-set-union', `zzz-set-intersection', `zzz-some',
;;    `zzz-two-zone-intersection', `zzz-two-zone-union',
;;    `zzz-zone-intersection', `zzz-zone-intersection-1',
;;    `zzz-zones-overlap-p', `zzz-zones-same-buffer-p',
;;    `zzz-zone-union', `zzz-zone-union-1'.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;
;; 2015/08/09 dadams
;;     Added: zzz-zone-complement.
;; 2015/08/05 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;

(defgroup zones nil
  "Zones of text - like multiple regions."
  :prefix "zzz-"
  :group 'editing
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
zones.el bug: \
&body=Describe bug here, starting with `emacs -Q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "http://www.emacswiki.org/zones.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/Zones")
  :link '(emacs-commentary-link :tag "Commentary" "zones"))

(defun zzz-ordered-zone (zone)
  "Return ZONE or ZONE with its car and cadr reversed, so car <= cadr.
The cddr of ZONE remains as it was."
  (let ((beg    (car  zone))
        (end    (cadr zone))
        (extra  (cddr zone)))
    (if (<= beg end) zone `(,end ,beg ,@extra))))

(defun zzz-zones-overlap-p (zone1 zone2)
  "Return non-nil if ZONE1 and  ZONE2 overlap.
Assumes that each zone is ordered (its car <= its cadr).
The cddrs are ignored.

Zones that use markers do not overlap if the marker buffers differ."
  (and (zzz-zones-same-buffer-p zone1 zone2)
       (progn (when (< (car zone2) (car zone1))
                (setq zone1 (prog1 zone2 (setq zone2 zone1))))
              (<= (car zone2) (cadr zone1)))))

(defun zzz-zones-same-buffer-p (zone1 zone2)
  "Return non-nil if ZONE1 and ZONE2 apply to the same buffer.
This is the case if they do not contain markers or the markers are
from the same buffer."
  (let* ((car1   (car zone1))
         (cadr1  (cadr zone1))
         (mkr1   (or (and (markerp car1)   car1)
                     (and (markerp cadr1)  cadr1)))
         (car2   (car zone2))
         (cadr2  (cadr zone2))
         (mkr2   (or (and (markerp car2)   car2)
                     (and (markerp cadr2)  cadr2))))
    (or (not (and mkr1  mkr2))
        (eq (marker-buffer mkr1) (marker-buffer mkr2)))))

(defun zzz-zone-complement (zones &optional beg end)
  "Return a list of zones that is the complement of ZONES, from BEG to END.
ZONES is assumed to be a union, i.e., sorted by car, with no overlaps.
Any extra info in a zone of ZONES, i.e., after the cadr, is ignored."
  (setq beg  (or beg  (point-min))
        end  (or end  (point-max)))
  (let ((res  ()))
    (dolist (zone  zones)
      (push (list beg (car zone)) res)
      (setq beg  (cadr zone)))
    (setq res  (nreverse (push (list beg end) res)))))

(defun zzz-two-zone-union (zone1 zone2)
  "Return the union of ZONE1 and ZONE2, or nil if they do not overlap.
Assumes that each zone is ordered (its car <= its cadr).

The cddr of a non-nil result (its EXTRA information, which must be a
list) is the union of the EXTRA information of each zone:

 (zzz-set-union (cddr zone1) (cddr zone2))"
  (and (zzz-zones-overlap-p zone1 zone2)
       `(,(zzz-min (car zone1)  (car zone2))
         ,(zzz-max (cadr zone1) (cadr zone2))
         ,@(zzz-set-union (cddr zone1) (cddr zone2)))))

(defun zzz-zone-union (zones)
  "Return the union of the zones in list ZONES.
Each element of ZONES is a list of two zone limits, possibly followed
by entra info: (LIMIT1 LIMIT2 . EXTRA), where EXTRA is a list.

The limits do not need to be in numerical order.

The list value returned is sorted by the lower limit of each zone,
which is its car.

Each zone in ZONES is first ordered, so that its car <= its cadr.
The resulting zones are then sorted by their cars.

`zzz-two-zone-union' is then applied recursively to combine
overlapping zones.  This means also that any EXTRA info is combined
when zones are merged together."
  (let* ((flipped-zones  (mapcar #'zzz-ordered-zone zones))
         (sorted-zones   (sort flipped-zones #'zzz-car-<)))
    (zzz-zone-union-1 sorted-zones)))

(defun zzz-zone-union-1 (zones)
  "Helper for `zzz-zone-union'."
  (if (null (cdr zones))
      zones
    (let ((new  (zzz-two-zone-union (car zones) (cadr zones))))
      (if new
          (zzz-zone-union-1 (cons new (cddr zones)))
        (cons (car zones) (zzz-zone-union-1 (cdr zones)))))))

(defun zzz-car-< (zone1 zone2)
  "Return non-nil if car of ZONE1 < car of ZONE2.
Each car can be a number or a marker.

* Two numbers or two markers in the same buffer: Use `<'.
* Two markers in different buffers: Use `string<' of the buffer names.
* Only one is a marker:
  If its buffer is current then treat it as a number, using `<'.
  Else return false if the marker is for ZONE1 and return true if it
       is for ZONE2."
  (let* ((p1  (car zone1))
         (p2  (car zone2))
         (m1  (markerp p1))
         (m2  (markerp p2))
         (b1  (and m1  (marker-buffer p1)))
         (b2  (and m2  (marker-buffer p2))))
    (cond ((and (not m1)  (not m2)) (< p1 p2))
          ((and m1  m2) (if (eq b1 b2)
                            (< p1 p2)
                          (string< (buffer-name b1) (buffer-name b2))))
          (m1 (and (eq (current-buffer) b1)  (< p1 p2)))
          (m2 (or (not (eq (current-buffer) b2))  (< p1 p2))))))

(defun zzz-two-zone-intersection (zone1 zone2)
  "Return intersection of ZONE1 and ZONE2.
\(The result is nil if they do not overlap.)
Assumes that each zone is ordered (its car <= its cadr).

The cddr of a non-nil result (its EXTRA information) is
the intersection of the EXTRA information of each zone:

 (zzz-set-intersection (cddr zone1) (cddr zone2))"
  (and (zzz-zones-overlap-p zone1 zone2)
       `(,(zzz-max (car zone1)  (car zone2))
         ,(zzz-min (cadr zone1) (cadr zone2))
         ,@(zzz-set-intersection (cddr zone1) (cddr zone2)))))

(defun zzz-zone-intersection (zones)
  "Return the intersection of the zones in list ZONES.
Each element of ZONES is a list of two zone limits, possibly followed
by entra info: (LIMIT1 LIMIT2 . EXTRA), where EXTRA is a list.

The limits do not need to be in numerical order.

The list value returned is sorted by the lower limit of each zone,
which is its car.

Each zone in ZONES is first ordered, so that its car <= its cadr.
The resulting zones are then sorted by their cars.

`zzz-two-zone-intersection' is then applied recursively to combine
overlapping zones.  This means also that any EXTRA info is combined
when zones are merged together."
  (let* ((flipped-zones  (mapcar #'zzz-ordered-zone zones))
         (sorted-zones   (sort flipped-zones (lambda (z1 z2) (< (car z1) (car z2))))))
    (zzz-zone-intersection-1 sorted-zones)))

(defun zzz-zone-intersection-1 (zones)
  "Helper for `zzz-zone-intersection'."
  (if (null (cdr zones))
      zones
    (let ((new  (zzz-two-zone-intersection (car zones) (cadr zones))))
      (and new  (zzz-zone-intersection-1 (cons new (cddr zones)))))))

;; From `cl-seq.el', function `union', without keyword treatment.
(defun zzz-set-union (list1 list2)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or
LIST2.  This is a non-destructive function; it copies the data if
necessary."
  (cond ((null list1)         list2)
        ((null list2)         list1)
        ((equal list1 list2)  list1)
        (t
         (unless (>= (length list1) (length list2))
           (setq list1  (prog1 list2 (setq list2  list1)))) ; Swap them.
         (while list2
           (unless (member (car list2) list1)  (setq list1  (cons (car list2) list1)))
           (setq list2  (cdr list2)))
         list1)))

;; From `cl-seq.el', function `intersection', without keyword treatment.
(defun zzz-set-intersection (list1 list2)
  "Set intersection of lists LIST1 and LIST2.
This is a non-destructive operation: it copies the data if necessary."
  (and list1  list2
       (if (equal list1 list2)
           list1
         (let ((result  ()))
           (unless (>= (length list1) (length list2))
             (setq list1  (prog1 list2 (setq list2  list1)))) ; Swap them.
           (while list2
             (when (member (car list2) list1)
               (setq result  (cons (car list2) result)))
             (setq list2  (cdr list2)))
           result))))

(defun zzz-min (&rest ns)
  "Like `min', but if the args include a marker then return a marker.
Raise an error if the args include markers from different buffers."
  (let ((buf  (zzz-buffer-of-markers ns))
        (min  (apply #'min ns)))
    (if (not buf)
        min
      (with-current-buffer (get-buffer-create buf)
        (set-marker (copy-marker min) min buf)))))

(defun zzz-max (&rest ns)
  "Like `max', but if the args include a marker then return a marker.
Raise an error if the args include markers from different buffers."
  (let ((buf  (zzz-buffer-of-markers ns))
        (max  (apply #'max ns)))
    (if (not buf)
        max
      (with-current-buffer (get-buffer-create buf)
        (set-marker (copy-marker max) max buf)))))

(defun zzz-buffer-of-markers (ns)
  "Return the buffer of the markers in list NS, or nil if no markers.
Raise an error if NS contains markers from different buffers."
  (let ((mkr  (zzz-some #'markerp ns)))
    (and mkr
         (progn
           (unless (zzz-every (lambda (nn)
                                (or (not (markerp nn))
                                    (eq (marker-buffer nn) (marker-buffer mkr))))
                              ns)
             (error "List contains markers from different buffers"))
           t)
         (marker-buffer mkr))))

;; Similar to `every' in `cl-extra.el', without non-list sequences and multiple
;; sequences.
(defun zzz-every (predicate list)
  "Return t if PREDICATE is true for all elements of LIST; else nil."
  (while (and list  (funcall predicate (car list)))  (setq list  (cdr list)))
  (null list))

;; Same as `bmkp-some' in `bookmark+-1.el'.
;; Similar to `some' in `cl-extra.el', without non-list sequences and multiple
;; sequences.
(defun zzz-some (predicate list)
  "Return non-nil if PREDICATE is true for some element of LIST; else nil.
Return the first non-nil value returned by PREDICATE."
  (let (res)
    (catch 'zzz-some
      (while list
        (when (funcall predicate (setq res  (pop list))) (throw 'zzz-some res)))
      (setq res  nil))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'zones)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zones.el ends here

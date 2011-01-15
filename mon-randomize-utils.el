;;; mon-randomize-utils.el --- procedures for generating pseudo randomness
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-randomize-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-22T11:57:09-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, extensions, emacs, 

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-randomize-utils provides procedures for generating pseudo randomness
;;
;; FUNCTIONS:►►►
;; `mon-next-almost-prime', `mon-gensym-counter-randomizer',
;; `mon-make-random-state', `mon-generate-prand-id', `mon-generate-prand-seed',
;; `mon-mix-fields', `mon-string-wonkify', `mon-generate-WPA-key',
;;
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; FACES:
;;
;; VARIABLES:
;;
;; GROUPS:
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; <UNQUALIFIED-ALIAS>                  <PREFIX>-<NON-CORE-SYMBOL>
;; `next-almost-prime'               -> `mon-next-almost-prime'
;;
;;  <PREFIX>-<QUALIFIED>                <PREFIX>-<NON-CORE-SYMBOL>
;; `mon-get-next-almost-prime'       -> `mon-next-almost-prime'
;; `mon-generate-wonky'              -> `mon-string-wonkify'
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `mon-next-almost-prime'            <- mon-utils.el
;; `mon-gensym-counter-randomizer'    <- mon-utils.el
;; `mon-make-random-state'            <- mon-utils.el
;; `mon-generate-prand-id'            <- mon-utils.el
;; `mon-generate-prand-seed'          <- mon-utils.el
;; `mon-string-wonkify'               <- mon-utils.el
;; `mon-generate-WPA-key'             <- mon-utils.el
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-randomize-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-11-25T02:27:00-05:00Z}#{10476} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-randomize-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-22T11:57:09-05:00Z}#{10471} - by MON KEY>
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2010-2011 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

(declare-function mon-nshuffle-vector "mon-macs" (mixup-vector))

;; (declare-function mon-string-to-sequence    "mon-string-utils" (string-to-frob &rest more-strings))
;; (declare-function mon-string-from-sequence  "mon-string-utils" (stringify-seq &rest other-seqs))

 
;;; ==============================
;;; :NOTE :FILE calc-comb.el has `math-primes-table'
;;; :COURTESY :FILE fns.c `next_almost_prime'
;;; :CREATED <Timestamp: #{2010-08-05T16:09:48-04:00Z}#{10314} - by MON>
(defun mon-next-almost-prime (w-integer)
  "From W-INTEGER return either a prime or a number nearer the next prime.\n
Return a value I where (>= I w-integer) and (>= W-INTEGER 0).\n
Useful for generating a reasonable arg for `make-hash-table's :size keyword.\n
:EXAMPLE\n\n\(let* \(\(prms-lt-100 '\(2 3 5 7 11 13 17 19 23 29 31 37 41 
                     43 47 53 59 61 67 71 73 79 83 89 97\)\)
       \(n->100 \(number-sequence 1 99 1\)\)
       next-almost\)
  \(mapc #'\(lambda \(nx-ap\)
            \(let \(\(non-prime \(unless \(memq nx-ap prms-lt-100\)
                               nx-ap\)\)\)
              \(when non-prime 
                \(push `\(,nx-ap . ,\(mon-next-almost-prime non-prime\)\) 
                      next-almost\)\)\)\)
        n->100\)
  \(setq next-almost \(nreverse next-almost\)\)\)\n
\(let \(\(n->100 \(number-sequence 100 300 1\)\)
      next-almost\)
  \(mapc #'\(lambda \(nx-ap\)
            \(push `\(,nx-ap . ,\(mon-next-almost-prime nx-ap\)\) 
                      next-almost\)\) 
        n->100\)
  \(setq next-almost \(nreverse next-almost\)\)\)\n
:NOTE The original C definition:\n
  ,----
  |  int
  |  next_almost_prime (int n)
  |  {
  |    if (n % 2 == 0)
  |      n += 1;
  |    if (n % 3 == 0)
  |      n += 2;
  |    if (n % 7 == 0)
  |      n += 4;
  |    return n;
  |  }
  `----\n
:ALIASED-BY `mon-get-next-almost-prime'
:ALIASED-BY `next-almost-prime'\n
:SEE-ALSO `mon-gensym-counter-randomizer', `mon-make-random-state',
`mon-generate-prand-seed', `mon-generate-WPA-key', `mon-generate-prand-id',
`mon-string-wonkify', `random', `math-primes-table'.\n►►►"
  (let ((mnapN w-integer))
    (when (= (% mnapN 2) 0) (setq mnapN (1+ mnapN)))
    (when (= (% mnapN 3) 0) (setq mnapN (+ mnapN 2)))
    (when (= (% mnapN 7) 0) (setq mnapN (+ mnapN 4)))
    ;; (when (= (% mnapN 5) 0) (setq mnapN (+ (- mnapN 2) 3)))
    ;; (when (= (% mnapN 2) 0) (setq mnapN 
    ;;                              (mon-next-almost-prime mnapN)))
  mnapN))

;;; ==============================
;;; :PREFIX "mgcr-"
;;; :CREATED <Timestamp: #{2010-07-29T17:45:55-04:00Z}#{10304} - by MON>
(defun mon-gensym-counter-randomizer (randomize-sym/str)
  "Generate a semi-randomized integer from RANDOMIZE-SYM/STR.\n
Helper function for `mon-with-gensyms'.\n
:EXAMPLE\n\n\(mon-gensym-counter-randomizer-TEST \"bubba\" 10000\)\n
\(mon-gensym-counter-randomizer-TEST 'bu  10000\)\n
:NOTE On average this function will return ~45-60 duplicates per 10,000
invocations per seed symbol. IOW one might create an average of ~9948 unique
`bubba's if we batched inside a procedure capable of accounting for collisions.\n
:SEE-ALSO `mon-gensym', `mon-gensym-counter-randomizer-TEST'.\n►►►"
  (let ((mgcr-pr1
         [37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131
          137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227
          229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317
          331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431
          433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541])
        (mgcr-pr2 (make-vector 6 0)) ;; 6th elt of is length randomize-sym/str
        (mgcr-pr3 (make-vector 6 0)) ;; 6th elt of is string-bytes randomize-sym/str
        (mgcr-merp [3 5 7 13 17 19]) 
        (mgcr-rtn randomize-sym/str))
    ;; Fill vectors mgcr-pr2 and mgcr-pr3 with primes from mgcr-pr1 
    (dolist (mgcr-shfv '(mgcr-pr2 mgcr-pr3))
      (mon-nshuffle-vector mgcr-pr1)
      (dotimes (mgcr-mkv 5)
        (aset (symbol-value mgcr-shfv) mgcr-mkv (aref mgcr-pr1 mgcr-mkv))))
    ;; :DEBUGGING  `(,mgcr-pr2 ,mgcr-pr3) 
    ;;
    ;; Put randomize-sym/str on a sub section of the shuffled mgcr-pr1 vector.
    ;; This assures we get at least 5 non-null char values when 
    ;; (< (length mgcr-rtn) 5) Then we add at least one more char for variance.
    (let* ((mgcr-rndmz-sym/str (or (and (stringp mgcr-rtn) mgcr-rtn)
                                   (format "%s" mgcr-rtn)))
           (mgcr-rndmz-len  (length mgcr-rndmz-sym/str))
           (mgcr-rndmz-trunc-len (- mgcr-rndmz-len 6))
           (mgcr-subv1 (if (or (zerop mgcr-rndmz-trunc-len) (natnump mgcr-rndmz-trunc-len)) 
                           ;; Only get 1 extra value in range 0-88 e.g. (1- (length mgcr-pr1)).
                           (random 87) 
                         ;; Its negative, get the difference.
                         (+ 88 mgcr-rndmz-trunc-len))) 
           (mgcr-subv2  (if (or (zerop mgcr-rndmz-trunc-len) (natnump mgcr-rndmz-trunc-len))
                            (1+ mgcr-subv1) 
                          (- mgcr-subv1 mgcr-rndmz-trunc-len))))
      (aset mgcr-pr2 5 mgcr-rndmz-len)
      (aset mgcr-pr3 5 (string-bytes mgcr-rndmz-sym/str))
      (setq mgcr-pr1
            (vconcat
             (substring
              (concat 
               (substring (concat (mon-nshuffle-vector mgcr-pr1) "") mgcr-subv1 mgcr-subv2)
               (if (zerop mgcr-rndmz-trunc-len) 
                   (substring mgcr-rndmz-sym/str 0 5)
                 mgcr-rndmz-sym/str))
              -6))))
    (mon-nshuffle-vector mgcr-pr1)
    ;; :DEBUGGING (concat (mon-gensym-counter-randomizer "bubba") "")
    ;;
    ;; Shuffle the hell out of it then maximize the list vals.
    (setq mgcr-rtn 
          (mapcar #'(lambda (mgcr-stl-char)
                      (mon-nshuffle-vector mgcr-pr2) 
                      (mon-nshuffle-vector mgcr-pr3)
                      (mon-nshuffle-vector mgcr-merp)
                      (let (mgcr-gthr)
                        (dotimes (i 5 
                                    (setq mgcr-gthr (lsh (apply '+ mgcr-gthr) 
                                                         (- (aref mgcr-merp 0) (aref mgcr-merp 1)))))
                          (push (* (aref mgcr-pr2 i) (aref mgcr-pr3 i) mgcr-stl-char) mgcr-gthr))))
                  mgcr-pr1))
    (setq mgcr-rtn (apply '+ mgcr-rtn))
    ;; Shift it around but keep it signed.
    (setq mgcr-rtn (abs (ash mgcr-rtn (- (aref mgcr-merp 2) (aref mgcr-merp 3)))))
    ;; Make sure we have a value over 10000 else recurse
    (if (or (null mgcr-rtn)
            (< mgcr-rtn 10000)
            (= mgcr-rtn 0))
        (mon-gensym-counter-randomizer randomize-sym/str)
      mgcr-rtn)))
;;
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; |
;; | (mon-gensym-counter-randomizer-TEST "bubba" 10000)
;; | 
;; | (mon-gensym-counter-randomizer-TEST 'bu  10000)
;; | 
;; | (progn (garbage-collect) 
;; |        (benchmark 10000 '(mon-gensym-counter-randomizer "bubba")))
;; | 
;; | (progn (garbage-collect) 
;; |        (benchmark 10000 '(mon-gensym-counter-randomizer 'bu)))
;; |
;; `----

;;; ==============================
;;; :COURTESY Roman Marynchak 2010-11-24
;;; Subject: [PATCH] better MIX function for SXHASH
;;; ,----
;;; | Add more "randomness" to MIX function by shifting
;;; | the input values combination XY in both directions
;;; | and counting X and Y values with the different
;;; | coefficients during the mixing process.
;;; `----
;;; :SEE sbcl/src/code/target-sxhash.lisp
;;; :SEE (URL `https://bugs.launchpad.net/sbcl/+bug/309443')
;;; :CHANGESET 2331
;;; :CREATED <Timestamp: #{2010-12-02T19:16:16-05:00Z}#{10484} - by MON KEY>
(defun mon-mix-fields (mmf-x mmf-y)
  "
:EXAMPLE\n\n\(mon-mix-fields 37 41\)\n
:SEE-ALSO `mon-make-random-state', `mon-gensym-counter-randomizer'.\n►►►" 
  (let ((mmf-xy (+ (* mmf-x 3) mmf-y)))
    (logand most-positive-fixnum ;; 536870911
            (logxor 441516657
                    ;; - mmf-xy
                    (ash (- mmf-xy (* mmf-y 13) 8738747) 9)  ;; +
                    (ash (+ mmf-xy (* mmf-x 23) 7882763) -9) ;; +
                    (ash mmf-xy -5)))))

;;; 

;;; ==============================
;;; :CHANGESET 2043
;;; :CREATED <Timestamp: #{2010-08-04T22:08:00-04:00Z}#{10313} - by MON KEY>
(defun mon-make-random-state ()
  "Return an integer with some randomness.\n
:EXAMPLE\n\n\(mon-make-random-state\)\n
:SEE-ALSO `mon-next-almost-prime', `mon-gensym-counter-randomizer', `mon-make-random-state',
`mon-generate-prand-seed', `mon-generate-WPA-key', `mon-generate-prand-id',
`mon-string-wonkify', `random'.\n►►►"
  (let ((mmrst8 
         (lsh (+ cons-cells-consed 
                 floats-consed 
                 vector-cells-consed 
                 symbols-consed
                 string-chars-consed 
                 misc-objects-consed 
                 intervals-consed
                 strings-consed
                 pure-bytes-used
                 num-nonmacro-input-events
                 (floor (* gcs-done (sqrt gcs-done)) 1)
                 (floor gc-elapsed 1))
              (aref (mon-nshuffle-vector [17 13 5 19 11 7 3 23]) 0))))
    (logxor
     (if (not (zerop mmrst8)) mmrst8 (mon-make-random-state))
     (random))))

;;; ==============================
;;; :PREFIX "mgpi-"
;;; :CREATED <Timestamp: #{2009-10-13T17:40:20-04:00Z}#{09422} - by MON>
(defun mon-generate-prand-id (&optional cnt)
  "Return a pseudo-rand UID.\n
Return value is a 40 char hex string generated as sha1 sum from seed
`mon-generate-prand-seed'. When > CNT 1 return N UID's.\n
:EXAMPLE\n\n\(mon-generate-prand-id 6\)\n
Only the first sum has random qualities and subsequent sha1 sums are taken from
sum calculated in previous iteration.\n 
Thus, if CNT is 4 then the sha1 of sum1 -> sum2 -> sum3 -> sum4.\n
This means:\n
a) the return value of all elts after car are _not_ random at all;\n
b) where UID assignment occurs in parallel with time-stamping we can infer
   when the UID was generated relative the index of previous/subsequent elts.
Item b is a Featured-Bug®.\n
:SEE-ALSO `mon-string-to-hex-string', `mon-generate-WPA-key', `mon-string-wonkify',
`url-hexify-string', `url-unhex-string', `url-unhex'.\n►►►"
  (eval-when-compile (require 'sha1))
  (let ((mgpi-cnt (if cnt cnt 1))
        mgpi-gthr)
    (do* ((mgpi-i 1 (1+ mgpi-i))
          (mgpi-j (sha1 (mon-generate-prand-seed)) (sha1 (car mgpi-gthr)))
          (mgpi-k (push mgpi-j mgpi-gthr) (push mgpi-j mgpi-gthr)))
        ((>= mgpi-i mgpi-cnt) mgpi-k))
    (nreverse mgpi-gthr)))
;;;
;;; :TEST-ME 
;;; (save-excursion 
;;;   (newline) 
;;;   (dolist (i (mon-generate-prand-id 1000))
;;;     (newline) (prin1 i (current-buffer))))

;;; ==============================
;;; :PREFIX "mgrs-"
;;; :CREATED <Timestamp: #{2009-10-12T15:07:02-04:00Z}#{09421} - by MON KEY>
(defun mon-generate-prand-seed ()
  "Generate a seed for 'unique random identifier' a 32 character hex string.
Seed is only pseudo random/unique but it will suffice for our needs.
Don't call this function in a loop it won't work b/c TIME is slow as hell.
Instead, use as a seed for `mon-generate-prand-id'.
On MON system min. 0.85 seconds is needed between calls to produce unique id's.\n
:EXAMPLE\n(mon-generate-prand-id)\n
\(let \(\(i 11\) \(k\)\)
  \(while \(/= i 0\)
    \(sleep-for 0.85\)
    \(setq k \(cons `\(,\(mon-generate-prand-id\)\) k\)\)
    \(setq i \(1- i\)\)\)
\(prin1 k\)\)\n
:SEE-ALSO `mon-generate-WPA-key', `mon-string-wonkify', `mon-list-nshuffle',
`mon-nshuffle-vector', `url-cache-create-filename-using-md5'.\n►►►"
  ;(eval-when-compile (require 'cookie1))
  (let* ((mgrs-pseudo-r #'(lambda () 
                       (mon-string-to-sequence 
                        (number-to-string (abs (random t))))))
         (mgrs-seq->v #'(lambda (mgrs-L-1) (apply 'vector mgrs-L-1)))
         (mgrs-shufv #'(lambda (mgrs-L-2) 
                    (mon-nshuffle-vector mgrs-L-2))))
    (md5    
     (mon-string-from-sequence
      (funcall mgrs-shufv
               (funcall mgrs-seq->v    
                        (mon-string-to-sequence
                         (md5
                          (mon-string-from-sequence
                           (funcall mgrs-shufv
                                    (vconcat
                                     (funcall mgrs-seq->v
                                              (mon-string-to-sequence
                                               (md5 
                                                (mon-string-from-sequence
                                                 (funcall mgrs-shufv 
                                                          (funcall mgrs-seq->v  
                                                                   (nreverse  
                                                                    (funcall mgrs-pseudo-r))))))))
                                     (funcall mgrs-shufv 
                                              (funcall mgrs-seq->v 
                                                       (funcall mgrs-pseudo-r))))))))))))))
;;;
;;; :TEST-ME (mon-generate-prand-seed)
;;; :TEST-ME (length (mon-generate-prand-seed))
;;; :TEST-ME (let ((i 11) (k))
;;;               (while (/= i 0)
;;;                 (sleep-for 0.85)
;;;                 (setq k (cons `(,(mon-generate-prand-seed)) k))
;;;                 (setq i (1- i)))
;;;               (prin1 k))

;;; ==============================
;;; :PREFIX "mswnky-"
;;; :NOTE Used in :FILE mon-site-local-defaults.el 
;;; This function shadows that symbol so that it can be compiled.
;;; :CREATED <Timestamp: #{2010-02-10T19:47:57-05:00Z}#{10064} - by MON KEY>
(defun mon-string-wonkify (wonk-words wonkify-n-times)
  "Wonkify the string WONK-WORDS.\n
:EXAMPLE\n\n\(mon-string-wonkify \"These are some wonky words\" 10\)\n
\(mon-string-wonkify \"These are some wonky words\" 3\)\n
:ALIASED-BY `mon-generate-wonky'\n
:SEE-ALSO `mon-zippify-region', `mon-generate-prand-seed', `mon-generate-prand-id',
`mon-generate-WPA-key', `mon-nshuffle-vector', `mon-list-nshuffle'\n►►►" 
  (let* ((mswnky-wrds wonk-words)
         ;; :PREFIX "wnkusr-"
         (wonk-USR #'(lambda (wnkusr-L-1 wnkusr-L-2) 
              (let (wnkusr-lt-0)
                (dolist (wnkusr-D-1 wnkusr-L-1)
                  (let ((wnkusr-lt-1 wnkusr-D-1))
                    (dotimes (wnkusr-D-2 (random (length wnkusr-lt-1)))
                      (let ((wnkusr-lt-2 (random (length wnkusr-lt-1))))
                        (setf (nth wnkusr-lt-2 wnkusr-lt-1)
                              (if wnkusr-L-2 
                                  (upcase (nth wnkusr-lt-2 wnkusr-lt-1)) 
                                (downcase (nth wnkusr-lt-2 wnkusr-lt-1))))))
                    (push (apply 'string wnkusr-lt-1) wnkusr-lt-0)))
                (setq mswnky-wrds wnkusr-lt-0))))
         ;; :PREFIX "sqfy-"
         (mswnky-seqify #'(lambda (sqfy-L-1)
                     (let (sqfy-lt-1)
                       (mapc #'(lambda (sqfy-L-2) 
                                 (push (string-to-list sqfy-L-2) sqfy-lt-1)) 
                             (cond ((listp sqfy-L-1) sqfy-L-1)
                                   ((stringp sqfy-L-1) (list sqfy-L-1))))
                       sqfy-lt-1)))
         ;; :PREFIX "lcgc-"
         (mswnky-GCD #'(lambda (&rest lcgc-L-1)
                      (let ((lcgc-lt-1 (abs (or (pop lcgc-L-1) 0))))
                        (while lcgc-L-1
                          (let ((lcgc-lt-2 (abs (pop lcgc-L-1))))
                            (while (> lcgc-lt-2 0) 
                              (setq lcgc-lt-2 
                                    (% lcgc-lt-1 
                                       (setq lcgc-lt-1 lcgc-lt-2))))))
                        lcgc-lt-1))))
    (setq mswnky-wrds (make-list wonkify-n-times (car (funcall mswnky-seqify mswnky-wrds))))
    (do ((mswnky-D-1 wonkify-n-times))
         ((< mswnky-D-1 0)  mswnky-wrds)
      (setq mswnky-D-1 (1- mswnky-D-1))
      (setq mswnky-wrds (apply 'vector mswnky-wrds))
      (setq mswnky-wrds (mon-nshuffle-vector mswnky-wrds))
      (setq mswnky-wrds (append mswnky-wrds nil))
      (when (stringp (car mswnky-wrds))
        (setq mswnky-wrds (funcall mswnky-seqify mswnky-wrds)))
      (funcall wonk-USR mswnky-wrds 
               (if (eq (apply mswnky-GCD `(,mswnky-D-1 2)) 2) t)))))
;;
;;; :TEST-ME (mon-string-wonkify "These are some wonky words" 10)
;;; :TEST-ME (mon-string-wonkify "These are some wonky words" 3)

;;; ==============================
;;; :PREFIX "mgWk-"
;;; :CREATED <Timestamp: #{2009-11-06T17:49:43-05:00Z}#{09455} - by MON KEY>
(defun mon-generate-WPA-key (&optional insrtp intrp)
  "Return a 64 char pseudo-random hex-string.\n
When INSRTP is non-nil or called-interactively insert string at point.
Does not move point.\n
:EXAMPLE\n\n\(mon-generate-WPA-key\)\n
:SEE-ALSO `mon-generate-prand-id', `mon-generate-prand-seed',
`mon-string-wonkify', `mon-string-from-hex-list', `mon-string-to-hex-list',
`mon-string-to-hex-string', `mon-make-random-state'.\n►►►"
  (interactive "i\np")
  (let ((mgWk-hex-str (mon-string-to-hex-string :prand-hex-len 64)))
    (if (or insrtp intrp)
        (save-excursion (princ (format " \"%s\" " mgWk-hex-str) (current-buffer)))
      mgWk-hex-str)))
;;
;;; :TEST-ME (mon-generate-WPA-key)
;;; :TEST-ME (call-interactively 'mon-generate-WPA-key)

;;; ==============================
(provide 'mon-randomize-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-randomize-utils.el ends here
;;; EOF

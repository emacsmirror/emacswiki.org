;;; cyrillic-folding.el --- support for Cyrillic -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2004 Eugene V. Markov

;; Author: Eugene V. Markov
;; Keywords: multilingual, Cyrillic, folding

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;;; Code:

(require 'cyrillic)

;; KOI-8 staff

(define-ccl-program ccl-encode-koi8
  `(1
    (
     (loop
      (read-multibyte-character r0 r1)
      (if (r0 == ,(charset-id 'ascii))
          (if (r1 == ?\r)
              (r1 = ?\n)))
      (if (r0 == ,(charset-id 'cyrillic-iso8859-5))
	  (translate-character cyrillic-koi8-r-encode-table r0 r1))
      (if (r0 != ,(charset-id 'ascii))
	  (if (r0 != ,(charset-id 'eight-bit-graphic))
	      (if (r0 != ,(charset-id 'eight-bit-control))
		  (r1 = ??))))
      (write-repeat r1))
     ))
  "CCL program to encode KOI8.")


;; ;; for koi8-unix
;; (let ((coding-spec (copy-sequence (get 'cyrillic-koi8-unix 'coding-system))))
;;   (aset coding-spec 4 '(ccl-decode-koi8 . ccl-encode-koi8-unix))
;;   (put 'cyrillic-koi8-unix 'coding-system coding-spec))


;; replace ...\r... with ...\r\n... (for single character), but safe ...\r\n...
(define-ccl-program ccl-encode-koi8-dos
  `(1
    (
     (r6 = 0)
     (loop
      (read-multibyte-character r0 r1)
      (if (r0 == ,(charset-id 'ascii))
          (if (r1 == ?\n)
              (if (r6 == 1)
                  ((r6 = 0)      ;; found \r\n pair.
                   (write ?\r)
                   (write ?\n)))
            (if (r1 == ?\r)
                (if (r6 == 1)
                    ((write ?\r) ;; found ...\r\r...
                     (write ?\n));; replace the first \r with \r\n.
                  (r6 = 1))
              ((if (r6 == 1)
                   ((r6 = 0)     ;; found any ascii character after \r (not \n).
                    (write ?\r)  ;; replace the \r with \r\n.
                    (write ?\n)))
               (write r1))))
        ((if (r0 == ,(charset-id 'cyrillic-iso8859-5))
             (translate-character cyrillic-koi8-r-encode-table r0 r1)
           (if (r0 != ,(charset-id 'eight-bit-graphic))
               (if (r0 != ,(charset-id 'eight-bit-control))
                   (r1 = ??))))
         (write-repeat r1)))
      (repeat))
     (if (r6 == 1) ;; for last \r (if it exist).
         ((write ?\r)
          (write ?\n)))
     ))
  "CCL program to encode koi8-dos.")

;; for koi8-dos
(let ((coding-spec (copy-sequence (get 'cyrillic-koi8-dos 'coding-system))))
  (aset coding-spec 4 '(ccl-decode-koi8 . ccl-encode-koi8-dos))
  (put 'cyrillic-koi8-dos 'coding-system coding-spec))

;;; ALTERNATIVNYJ stuff

;; replace \r with \n.
(define-ccl-program ccl-encode-alternativnyj
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (if (r0 == ,(charset-id 'ascii))
          (if (r1 == ?\r)
              (r1 = ?\n)))
      (if (r0 == ,(charset-id 'cyrillic-iso8859-5))
	  (translate-character cyrillic-alternativnyj-encode-table r0 r1))
      (if (r0 != ,(charset-id 'ascii))
	  (if (r0 != ,(charset-id 'eight-bit-graphic))
	      (if (r0 != ,(charset-id 'eight-bit-control))
		  (r1 = ??))))
      (write-repeat r1))))
  "CCL program to encode Alternativnyj.")

;; replace ...\r... with ...\r\n... (for single character), but safe ...\r\n...
(define-ccl-program ccl-encode-alternativnyj-dos
  `(1
    (
     (r6 = 0)
     (loop
      (read-multibyte-character r0 r1)
      (if (r0 == ,(charset-id 'ascii))
          (if (r1 == ?\n)
              (if (r6 == 1)
                  ((r6 = 0)      ;; found \r\n pair.
                   (write ?\r)
                   (write ?\n)))
            (if (r1 == ?\r)
                (if (r6 == 1)
                    ((write ?\r) ;; found ...\r\r...
                     (write ?\n));; replace the first \r with \r\n.
                  (r6 = 1))
              ((if (r6 == 1)
                   ((r6 = 0)     ;; found any ascii character after \r (not \n).
                    (write ?\r)  ;; replace the \r with \r\n.
                    (write ?\n)))
               (write r1))))
        ((if (r0 == ,(charset-id 'cyrillic-iso8859-5))
             (translate-character cyrillic-alternativnyj-encode-table r0 r1)
           (if (r0 != ,(charset-id 'eight-bit-graphic))
               (if (r0 != ,(charset-id 'eight-bit-control))
                   (r1 = ??))))
         (write-repeat r1)))
      (repeat))
     (if (r6 == 1) ;; for last \r (if it exist).
         ((write ?\r)
          (write ?\n))))
    )
  "CCL program to encode alternativnyj-dos.")

;; for alternativnyj-dos
(let ((coding-spec (copy-sequence (get 'cyrillic-alternativnyj-dos 'coding-system))))
  (aset coding-spec 4 '(ccl-decode-alternativnyj . ccl-encode-alternativnyj-dos))
  (put 'cyrillic-alternativnyj-dos 'coding-system coding-spec))

(provide 'cyrillic-folding)

;;; cyrillic-folding.el ends here

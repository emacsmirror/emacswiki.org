;;; cyrillic-win.el --- support for Cyrillic-CP1251 -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2004 Eugene V. Markov

;; Author: Eugene V. Markov
;; Keywords: multilingual, Cyrillic

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

;; cp1251 staff

(defvar cp1251-decode-translation-table
  [
   0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15
   16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
   32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
   48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
   64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79
   80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
   96  97  98  99  100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
   128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
   144 145  39 147 148 149 150 151 152 153 154 155 156 157 158 159
    32 161 162 163 164 165 166 167 ?,L!(B  169 170 171 172 173 174 175
   176 177 178 179 180 181 182 183 ?,Lq(B  185 186 187 188 189 190 191
   ?,L0(B  ?,L1(B  ?,L2(B  ?,L3(B  ?,L4(B  ?,L5(B  ?,L6(B  ?,L7(B  ?,L8(B  ?,L9(B  ?,L:(B  ?,L;(B  ?,L<(B  ?,L=(B  ?,L>(B  ?,L?(B
   ?,L@(B  ?,LA(B  ?,LB(B  ?,LC(B  ?,LD(B  ?,LE(B  ?,LF(B  ?,LG(B  ?,LH(B  ?,LI(B  ?,LJ(B  ?,LK(B  ?,LL(B  ?,LM(B  ?,LN(B  ?,LO(B
   ?,LP(B  ?,LQ(B  ?,LR(B  ?,LS(B  ?,LT(B  ?,LU(B  ?,LV(B  ?,LW(B  ?,LX(B  ?,LY(B  ?,LZ(B  ?,L[(B  ?,L\(B  ?,L](B  ?,L^(B  ?,L_(B
   ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,Ld(B  ?,Le(B  ?,Lf(B  ?,Lg(B  ?,Lh(B  ?,Li(B  ?,Lj(B  ?,Lk(B  ?,Ll(B  ?,Lm(B  ?,Ln(B  ?,Lo(B
  ]
  "Cyrillic cp1251 decoding table.")

(let ((table (make-translation-table-from-vector
	      cp1251-decode-translation-table)))
  (define-translation-table 'cp1251-nonascii-translation-table table)
  (define-translation-table 'cp1251-encode-translation-table
    (char-table-extra-slot table 0)))

(define-ccl-program ccl-decode-cp1251
  `(3
    ((loop
      (r0 = 0)
      (read r1)
      (if (r1 < 128)
	  (write-repeat r1)
	((translate-character cp1251-nonascii-translation-table r0 r1)
	 (write-multibyte-character r0 r1)
	 (repeat))))))
  "CCL program to decode cp1251.")

;; replace ...\r... with ...\r\n... (for single character), but safe ...\r\n...
(define-ccl-program ccl-encode-cp1251-dos
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
             (translate-character cp1251-encode-translation-table r0 r1)
           (if (r0 != ,(charset-id 'eight-bit-graphic))
               (if (r0 != ,(charset-id 'eight-bit-control))
                   (r1 = ??))))
         (write-repeat r1)))
      (repeat))
     (if (r6 == 1) ;; for last \r (if it exist).
         ((write ?\r)
          (write ?\n))))
    )
  "CCL program to encode cp1251-dos.")

;; replace \r with \n.
(define-ccl-program ccl-encode-cp1251-unix
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (if (r0 == ,(charset-id 'ascii))
          (if (r1 == ?\r)
              (r1 = ?\n)))
      (if (r0 == ,(charset-id 'cyrillic-iso8859-5))
	  (translate-character cp1251-encode-translation-table r0 r1))
      (if (r0 != ,(charset-id 'ascii))
	  (if (r0 != ,(charset-id 'eight-bit-graphic))
	      (if (r0 != ,(charset-id 'eight-bit-control))
		  (r1 = ??))))
      (write-repeat r1))))
  "CCL program to encode cp1251-unix.")

(define-ccl-program ccl-encode-cp1251-mac
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (if (r0 == ,(charset-id 'cyrillic-iso8859-5))
	  (translate-character cp1251-encode-translation-table r0 r1))
      (if (r0 != ,(charset-id 'ascii))
	  (if (r0 != ,(charset-id 'eight-bit-graphic))
	      (if (r0 != ,(charset-id 'eight-bit-control))
		  (r1 = ??))))
      (write-repeat r1))))
  "CCL program to encode cp1251-mac.")

(make-coding-system
 'cp1251 4 ?W "cp1251 8-bit encoding for Cyrillic (MIME: windows-1251)"
 '(ccl-decode-cp1251 . ccl-encode-cp1251-unix)
 `((safe-chars . ,(let ((table (make-char-table 'safe-chars))
			(i 0))
		    (while (< i 256)
		      (aset table (aref cp1251-decode-translation-table i) t)
		      (setq i (1+ i)))
		    table))
   (mime-charset . windows-1251)
   (valid-codes (0 . 127) 146 160 168 184 (192 . 255))
   (charset-origin-alist (cyrillic-iso8859-5 "CP1251"
                                             cyrillic-encode-cp1251-char))))


;; for cp1251-dos
(let ((coding-spec (copy-sequence (get 'cp1251-dos 'coding-system))))
  (aset coding-spec 4 '(ccl-decode-cp1251 . ccl-encode-cp1251-dos))
  (put 'cp1251-dos 'coding-system coding-spec))

;; for cp1251-mac
(let ((coding-spec (copy-sequence (get 'cp1251-mac 'coding-system))))
  (aset coding-spec 4 '(ccl-decode-cp1251 . ccl-encode-cp1251-mac))
  (put 'cp1251-mac 'coding-system coding-spec))


(define-coding-system-alias 'windows-1251 'cp1251)

(define-ccl-program ccl-encode-cp1251-font
  `(0
    ((translate-character cp1251-encode-translation-table r0 r1)))
  "CCL program to encode Cyrillic chars to cp1251 font.")

(setq font-ccl-encoder-alist
      (cons '("cp1251" . ccl-encode-cp1251-font) font-ccl-encoder-alist))

(set-language-info-alist
 "Cyrillic-CP1251" `((charset cyrillic-iso8859-5)
		   (nonascii-translation
		    . ,(get 'cp1251-nonascii-translation-table
			    'translation-table))
		   (coding-system cp1251)
		   (coding-priority cp1251)
		   (input-method . "cyrillic-jcuken")
		   (features cyril-win-util)
		   (unibyte-display . cp1251)
		   (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		   (documentation . "Support for Cyrillic cp1251."))
 '("Cyrillic"))


(provide 'cyrillic-win)

;;; cyrillic-win.el ends here

;;; rijndael.el --- Rijndael (AES) block cipher implementation
;; Copyright (C) 2001 Simon Josefsson

;; Author: Simon Josefsson <jas@pdc.kth.se>
;; Keywords: rijndael aes block cipher symmetric encryption cryptography

;; This is free software
;; This software is provided as-is, without express or implied
;; warranty.  Permission to use, copy, modify, distribute or sell this
;; software, without fee, for any purpose and by any individual or
;; organization, is hereby granted, provided that the above copyright
;; notice and this paragraph appear in all copies.

;;; Commentary:

;; This is a implementation of the Rijndael block cipher algorithm in
;; Emacs Lisp.  Rijndael has been chosen as the Advanced Encryption
;; Standard (AES) by NIST.
;;
;; This package also have functionality to generate (some of) NIST's
;; Rijndael test vectors.  However, they will take a very long time to
;; generate in elisp.  You can customize `rijndael-monte-carlo-limit'
;; from the default of 10,000 into something more sensible as, say, 1
;; to make it faster.  It is also possible to customize the loop
;; length defined by `rijndael-monte-carlo-loop' from the default 400.
;; Of course, you need to modify the reference implementation as well
;; if you want to make useful comparisons of the test vectors.
;;
;; Rijndael home page is at:
;; http://www.esat.kuleuven.ac.be/~rijmen/rijndael/
;;
;; Any updated releases of this file will be located at:
;; http://josefsson.org/aes/

;;; Instructions:

;; Call functions in Emacs Lisp programs, or use this package
;; interactively in the *scratch* buffer in emacs.
;;
;; Here's how you could do a simple encryption interactively (press
;; C-j to evaluate each line):
;;
;; (setq key (rijndael-make-key 128 (rijndael-hexstring-to-bitstring
;;                                   "00000000000000000000000000000000")))
;;
;; (setq data (rijndael-hexstring-to-bitstring
;;	       "00000000000000000000000000000000"))
;;
;; (rijndael-bitstring-to-hexstring
;;  (rijndael-block-encrypt data key 'ecb 128))
;;
;; Or more compact as:
;;
;; (rijndael-bitstring-to-hexstring
;;  (rijndael-block-encrypt 
;;   (rijndael-hexstring-to-bitstring "00000000000000000000000000000000")
;;   (rijndael-make-key 128 (rijndael-hexstring-to-bitstring
;;                           "00000000000000000000000000000000"))
;;   'ecb 128))
;;
;; For reference, correct output is "66e94bd4ef8a2c3b884cfa59ca342b2e".
;;
;; NB! `data' will be destructively modified.

;;; Todo:

;; Other modes than ECB.

;;; Revision history:

;; 2001-03-31  Posted to gnu.emacs.sources.
;; 2001-04-04  Ported to (X)Emacs 19.
;; 2001-05-10  Supports 160 and 224 key and block sizes.
;; 2001-09-27  Posted to gnu.emacs.sources.


;;; Code:

;; User variables:

(eval-and-compile
  ;; Provide dummy customize functions for emacsen that doesn't have them.
  (if (not (and (or (featurep 'custom) (load "custom" t))
		(fboundp 'defcustom) (fboundp 'defgroup)))
      (progn
	(if (not (fboundp 'defcustom))
	    (defmacro defcustom (var value doc &rest args)
	      (list 'defvar var value doc)))
	(if (not (fboundp 'defgroup))
	    (defmacro defgroup (&rest args))))))

(defgroup rijndael nil
  "Rijndael cryptographic functions")

(defcustom rijndael-monte-carlo-limit 10000
  "How many iterations to do in the Monte-Carlo tests.
NIST uses 10000 (the default), but this will be very slow, so you may
change this to a lower value (like, say, 1).  Of course, you must
modify the Rijndael reference implementation to be able to compare the
output."
  :group 'rijndael
  :type 'integer)

(defcustom rijndael-monte-carlo-loop 400
  "How many iterations to do in the Monte-Carlo tests.
NIST uses 400 (the default), but this will be very slow, so you may
change this to a lower value (like, say, 1).  Of course, you must
modify the Rijndael reference implementation to be able to compare the
output."
  :group 'rijndael
  :type 'integer)


;; Internal constants:

(defconst rijndael-Logtable
  [  0   0  25   1  50   2  26 198  75 199  27 104  51 238 223   3
   100   4 224  14  52 141 129 239  76 113   8 200 248 105  28 193
   125 194  29 181 249 185  39 106  77 228 166 114 154 201   9 120
   101  47 138   5  33  15 225  36  18 240 130  69  53 147 218 142
   150 143 219 189  54 208 206 148  19  92 210 241  64  70 131  56
   102 221 253  48 191   6 139  98 179  37 226 152  34 136 145  16
   126 110  72 195 163 182  30  66  58 107  40  84 250 133  61 186
    43 121  10  21 155 159  94 202  78 212 172 229 243 115 167  87
   175  88 168  80 244 234 214 116  79 174 233 213 231 230 173 232
    44 215 117 122 235  22  11 245  89 203  95 176 156 169  81 160
   127  12 246 111  23 196  73 236 216  67  31  45 164 118 123 183
   204 187  62  90 251  96 177 134  59  82 161 108 170  85  41 157
   151 178 135 144  97 190 220 252 188 149 207 205  55  63  91 209
    83  57 132  60  65 162 109  71  20  42 158  93  86 242 211 171
    68  17 146 217  35  32  46 137 180 124 184  38 119 153 227 165
   103  74 237 222 197  49 254  24  13  99 140 128 192 247 112   7]
  "Multiplication in GF(2^8) lookup table.")

(defconst rijndael-Alogtable
  [  1   3   5  15  17  51  85 255  26  46 114 150 161 248  19  53
    95 225  56  72 216 115 149 164 247   2   6  10  30  34 102 170
   229  52  92 228  55  89 235  38 106 190 217 112 144 171 230  49
    83 245   4  12  20  60  68 204  79 209 104 184 211 110 178 205
    76 212 103 169 224  59  77 215  98 166 241   8  24  40 120 136
   131 158 185 208 107 189 220 127 129 152 179 206  73 219 118 154
   181 196  87 249  16  48  80 240  11  29  39 105 187 214  97 163
   254  25  43 125 135 146 173 236  47 113 147 174 233  32  96 160
   251  22  58  78 210 109 183 194  93 231  50  86 250  21  63  65
   195  94 226  61  71 201  64 192  91 237  44 116 156 191 218 117
   159 186 213 100 172 239  42 126 130 157 188 223 122 142 137 128
   155 182 193  88 232  35 101 175 234  37 111 177 200  67 197  84
   252  31  33  99 165 244   7   9  27  45 119 153 176 203  70 202
    69 207  74 222 121 139 134 145 168 227  62  66 198  81 243  14
    18  54  90 238  41 123 141 140 143 138 133 148 167 242  13  23
    57  75 221 124 132 151 162 253  28  36 108 180 199  82 246   1]
  "Multiplication in GF(2^8) lookup table.")

(defconst rijndael-S
  [ 99 124 119 123 242 107 111 197  48   1 103  43 254 215 171 118
   202 130 201 125 250  89  71 240 173 212 162 175 156 164 114 192
   183 253 147  38  54  63 247 204  52 165 229 241 113 216  49  21
     4 199  35 195  24 150   5 154   7  18 128 226 235  39 178 117
     9 131  44  26  27 110  90 160  82  59 214 179  41 227  47 132
    83 209   0 237  32 252 177  91 106 203 190  57  74  76  88 207
   208 239 170 251  67  77  51 133  69 249   2 127  80  60 159 168
    81 163  64 143 146 157  56 245 188 182 218  33  16 255 243 210
   205  12  19 236  95 151  68  23 196 167 126  61 100  93  25 115
    96 129  79 220  34  42 144 136  70 238 184  20 222  94  11 219
   224  50  58  10  73   6  36  92 194 211 172  98 145 149 228 121
   231 200  55 109 141 213  78 169 108  86 244 234 101 122 174   8
   186 120  37  46  28 166 180 198 232 221 116  31  75 189 139 138
   112  62 181 102  72   3 246  14  97  53  87 185 134 193  29 158
   225 248 152  17 105 217 142 148 155  30 135 233 206  85  40 223
   140 161 137  13 191 230  66 104  65 153  45  15 176  84 187  22]
  "Rijndael S-box.")

(defconst rijndael-Si
  [ 82   9 106 213  48  54 165  56 191  64 163 158 129 243 215 251
   124 227  57 130 155  47 255 135  52 142  67  68 196 222 233 203
    84 123 148  50 166 194  35  61 238  76 149  11  66 250 195  78
     8  46 161 102  40 217  36 178 118  91 162  73 109 139 209  37
   114 248 246 100 134 104 152  22 212 164  92 204  93 101 182 146
   108 112  72  80 253 237 185 218  94  21  70  87 167 141 157 132
   144 216 171   0 140 188 211  10 247 228  88   5 184 179  69   6
   208  44  30 143 202  63  15   2 193 175 189   3   1  19 138 107
    58 145  17  65  79 103 220 234 151 242 207 206 240 180 230 115
   150 172 116  34 231 173  53 133 226 249  55 232  28 117 223 110
    71 241  26 113  29  41 197 137 111 183  98  14 170  24 190  27
   252  86  62  75 198 210 121  32 154 219 192 254 120 205  90 244
    31 221 168  51 136   7 199  49 177  18  16  89  39 128 236  95
    96  81 127 169  25 181  74  13  45 229 122 159 147 201 156 239
   160 224  59  77 174  42 245 176 200 235 187  60 131  83 153  97
    23  43   4 126 186 119 214  38 225 105  20  99  85  33  12 125]
  "Rijndael inverted S-box.")

(defconst rijndael-rcon [ ?\x01 ?\x02 ?\x04 ?\x08 ?\x10 ?\x20 ?\x40 ?\x80
			  ?\x1b ?\x36 ?\x6c ?\xd8 ?\xab ?\x4d ?\x9a ?\x2f
			  ?\x5e ?\xbc ?\x63 ?\xc6 ?\x97 ?\x35 ?\x6a ?\xd4
			  ?\xb3 ?\x7d ?\xfa ?\xef ?\xc5 ?\x91]
  "Rijndael round constants used in key scheduling.")

(defconst rijndael-shifts [[[0 0]
			    [1 3]
			    [2 2]
			    [3 1]]

			   [[0 0]
			    [1 4]
			    [2 3]
			    [3 2]]
		     
			   [[0 0]
			    [1 5]
			    [2 4]
			    [3 3]]

			   [[0 0]
			    [1 6]
			    [2 5]
			    [4 3]]
			   
			   [[0 0]
			    [1 7]
			    [3 5]
			    [4 4]]]
  "Rijndael shift offsets.
Element M is used when blocksizes match M=Nb-4.
Element N inside a shift amount for each blocksize corresponds with
the row to shift.
The first element O1 of each row corresponds with encryption offset,
the second element O2 to decryption offset. (O2 inverse of O1 under mod Nb.)")

(defconst rijndael-maxbc (/ 256 32)
  "Rijndael maximum size of data blocks.")

(defconst rijndael-maxkc (/ 256 32)
  "Rijndael maximum size of key blocks.")

(defconst rijndael-maxrounds 14
  "Rijndael maximum number of rounds.")

(defconst rijndael-hex-alist 
  '((?0 . 0)	      (?a . 10)	      (?A . 10)
    (?1 . 1)	      (?b . 11)	      (?B . 11)
    (?2 . 2)	      (?c . 12)	      (?C . 12)
    (?3 . 3)	      (?d . 13)	      (?D . 13)
    (?4 . 4)	      (?e . 14)	      (?E . 14)
    (?5 . 5)	      (?f . 15)	      (?F . 15)
    (?6 . 6)
    (?7 . 7)
    (?8 . 8)
    (?9 . 9))
  "Hex table for use in base conversions.")


;; Basic low-level cryptograhic primitives:

(defsubst rijndael-mul (a b)
  "Rijndael MUL primitive.
Multiply two elements of GF(2^m).
needed for MixColumn and InvMixColumn."
  (if (and (not (= a 0))
	   (not (= b 0)))
      (aref rijndael-Alogtable (% (+ (aref rijndael-Logtable a)
				     (aref rijndael-Logtable b))
				  255))
    0))

(defsubst rijndael-key-addition (a rk bc)
  "Rijndael keyAddition primitive.
Exor corresponding text input and round key input bytes."
  (let ((i 0) j)
    (while (< i 4)
      (setq j 0)
      (while (< j bc)
	(aset (aref a i) j
	      (logxor (aref (aref a i) j)
		      (aref (aref rk i) j)))
	(setq j (1+ j)))
      (setq i (1+ i)))))

(defsubst rijndael-shift-row (a d bc)
  "Rijndael ShiftRow primitive.
Row 0 remains unchanged.
The other three rows are shifted a variable amount."
  (let (i j (tmp (make-vector rijndael-maxbc 0)))
    (setq i 1)
    (while (< i 4)
      (setq j 0)
      (while (< j bc)
	(aset tmp j
	      (aref (aref a i)
		    (% (+ j (aref (aref (aref rijndael-shifts
					      (- bc 4))
					i)
				  d))
		       bc)))
	(setq j (1+ j)))
      (setq j 0)
      (while (< j bc)
	(aset (aref a i) j (aref tmp j))
	(setq j (1+ j)))
      (setq i (1+ i)))))

(defsubst rijndael-substitution (a box bc)
  "Rijndael substitution primitive.
Replace every byte of the input by the byte at that place
in the nonlinear S-box."
  (let ((i 0) j)
    (while (< i 4)
      (setq j 0)
      (while (< j bc)
	(aset (aref a i) j (aref box (aref (aref a i) j)))
	(setq j (1+ j)))
      (setq i (1+ i)))))

(defsubst rijndael-mix-column (a bc)
  "Rijndael MixColumn primitive.
Mix the four bytes of every column in a linear way."
  (let ((b (make-vector 4 0)) i j)
    ;; init b
    (setq i 0)
    (while (< i 4)
      (aset b i (make-vector rijndael-maxbc 0))
      (setq i (1+ i)))
    ;; do it
    (setq j 0)
    (while (< j bc)
      (setq i 0)
      (while (< i 4)
	(aset (aref b i)
	      j
	      (logxor (rijndael-mul 2 (aref (aref a i) j))
		      (rijndael-mul 3 (aref (aref a (% (+ i 1) 4)) j))
		      (aref (aref a (% (+ i 2) 4)) j)
		      (aref (aref a (% (+ i 3) 4)) j)))
	(setq i (1+ i)))
      (setq j (1+ j)))
    ;; copy b back into a
    (setq i 0)
    (while (< i 4)
      (setq j 0)
      (while (< j bc)
	(aset (aref a i) j (aref (aref b i) j))
	(setq j (1+ j)))
      (setq i (1+ i)))))

(defsubst rijndael-inv-mix-column (a bc)
  "Rijndael inverted MixColumn primitive.
Mix the four bytes of every column in a linear way.
This is the opposite operation of Mixcolumn."
  (let ((b (make-vector 4 0)) i j)
    ;; init b
    (setq i 0)
    (while (< i 4)
      (aset b i (make-vector rijndael-maxbc 0))
      (setq i (1+ i)))
    ;; do it
    (setq j 0)
    (while (< j bc)
      (setq i 0)
      (while (< i 4)
	(aset (aref b i)
	      j
	      (logxor (rijndael-mul ?\x0E (aref (aref a i) j))
		      (rijndael-mul ?\x0B (aref (aref a (% (+ i 1) 4)) j))
		      (rijndael-mul ?\x0D (aref (aref a (% (+ i 2) 4)) j))
		      (rijndael-mul ?\x09 (aref (aref a (% (+ i 3) 4)) j))))
	(setq i (1+ i)))
      (setq j (1+ j)))
    ;; copy b back into a
    (setq i 0)
    (while (< i 4)
      (setq j 0)
      (while (< j bc)
	(aset (aref a i) j (aref (aref b i) j))
	(setq j (1+ j)))
      (setq i (1+ i)))))


;; Internal functions:

(defun rijndael-bc (blockBits)
  "Determine BC given block size."
  (if (or (< blockBits 128) (> blockBits 256))
      (error "Invalid block size %d bits" blockBits)
    (/ blockBits 32)))

(defun rijndael-rounds (keyBits blockBits)
  "Determine Rijndael rounds given key and block sizes."
  (if (or (< keyBits 128) (> keyBits 256) (< blockBits 128) (> blockBits 256))
      (error "Invalid key/block size combination (key size %d block size %d)"
	     keyBits blockBits)
    (+ (/ (max keyBits blockBits) 32) 6)))

(defun rijndael-print-key-schedule (key-schedule)
  (let (i j k str)
    (setq i 0)
    (while (< i (1+ rijndael-maxrounds))
      (setq j 0)
      (while (< j 4)
	(setq k 0)
	(while (< k rijndael-maxbc)
	  (setq str (concat str (format "%3d "
					(aref (aref (aref key-schedule i)
						    j)
					      k))))
	  (setq k (1+ k)))
	(setq str (concat str "\n"))
	(setq j (1+ j)))
      (setq str (concat str "\n"))
      (setq i (1+ i)))
    (setq str (concat str "\n"))))

(defun rijndael-key-schedule (k keyBits blockBits)
  "Rijndael KeySchedule function (internal).
Calculate the necessary round keys.
The number of calculations depends on keyBits and blockBits."
  (let ((rconpointer 0) i j T
	(kc (rijndael-bc keyBits))
	(bc (rijndael-bc blockBits))
	(rounds (rijndael-rounds keyBits blockBits))
	(tk (make-vector 4 0))
	(W (make-vector (1+ rijndael-maxrounds) 0))
	(mid 4))
    ;; init tk
    (setq i 0)
    (while (< i 4)
      (aset tk i (make-vector rijndael-maxkc 0))
      (setq i (1+ i)))
    ;; init W
    (setq i 0)
    (while (< i (1+ rijndael-maxrounds))
      (aset W i (make-vector 4 0))
      (setq j 0)
      (while (< j 4)
	(aset (aref W i) j (make-vector rijndael-maxbc 0))
	(setq j (1+ j)))
      (setq i (1+ i)))
    ;; start
    (setq j 0)
    (while (< j kc)
      (setq i 0)
      (while (< i 4)
	(aset (aref tk i) j
	      (aref (aref k i) j))
	(setq i (1+ i)))
      (setq j (1+ j)))
    ;; copy values into round key array
    (setq j 0
	  T 0)
    (while (and (< j kc) (< T (* (1+ rounds) bc)))
      (setq i 0)
      (while (< i 4)
	(aset (aref (aref W (/ T bc)) i) (% T bc)
	      (aref (aref tk i) j))
	(setq i (1+ i)))
      (setq j (1+ j))
      (setq T (1+ T)))
    ;; while not enough round key material calculated
    (while (< T (* (1+ rounds) bc))
      ;; calculate new values
      (setq i 0)
      (while (< i 4)
	(aset (aref tk i) 0
	      (logxor (aref (aref tk i) 0)
		      (aref rijndael-S
			    (aref (aref tk (% (1+ i) 4)) (1- kc)))))
	(setq i (1+ i)))
      (aset (aref tk 0) 0
	    (logxor (aref (aref tk 0) 0)
		    (aref rijndael-rcon rconpointer)))
      (setq rconpointer (1+ rconpointer))
      (setq j 1)
      (if (<= kc 6)
	  (while (< j kc)
	    (setq i 0)
	    (while (< i mid)
	      (aset (aref tk i) j
		    (logxor (aref (aref tk i) j)
			    (aref (aref tk i) (1- j))))
	      (setq i (1+ i)))
	    (setq j (1+ j)))
	(while (< j mid)
	  (setq i 0)
	  (while (< i 4)
	    (aset (aref tk i) j
		  (logxor (aref (aref tk i) j)
			  (aref (aref tk i) (1- j))))
	    (setq i (1+ i)))
	  (setq j (1+ j)))
	(setq i 0)
	(while (< i 4)
	  (aset (aref tk i) mid
		(logxor (aref (aref tk i) mid)
			(aref rijndael-S (aref (aref tk i) (1- mid)))))
	  (setq i (1+ i)))
	(setq j (1+ mid) i 0)
	(while (< j kc)
	  (setq i 0)
	  (while (< i mid)
	    (aset (aref tk i) j
		  (logxor (aref (aref tk i) j)
			  (aref (aref tk i) (1- j))))
	    (setq i (1+ i)))
	  (setq j (1+ j))))
      ;; copy values into round key array
      (setq j 0)
      (while (and (< j kc) (< T (* (1+ rounds) bc)))
	(setq i 0)
	(while (< i 4)
	  (aset (aref (aref W (/ T bc)) i) (% T bc)
		(aref (aref tk i) j))
	  (setq i (1+ i)))
	(setq j (1+ j))
	(setq T (1+ T))))
    W))

(defun rijndael-encrypt (a keyBits blockBits rk &optional rounds)
  "Rijndael block encryption (internal).
Encryption of one block.  If optional argument rounds is non-null,
encrypt only a certain number of rounds (for debugging only)."
  (let ((r 1)
	(bc (rijndael-bc blockBits))
	(ROUNDS (rijndael-rounds keyBits blockBits)))
    ;; make number of rounds sane
    (if (or (null rounds) (> rounds ROUNDS))
	(setq rounds ROUNDS))
    ;; begin with a key addition
    (rijndael-key-addition a (aref rk 0) bc)
    ;; ROUNDS-1 ordinary rounds
    (while (and (<= r rounds) (< r ROUNDS))
      (rijndael-substitution a rijndael-S bc)
      (rijndael-shift-row a 0 bc)
      (rijndael-mix-column a bc)
      (rijndael-key-addition a (aref rk r) bc)
      (setq r (1+ r)))
    ;; Last round is special: there is no MixColumn
    (if (= rounds ROUNDS)
	(progn
	  (rijndael-substitution a rijndael-S bc)
	  (rijndael-shift-row a 0 bc)
	  (rijndael-key-addition a (aref rk rounds) bc)))))

(defun rijndael-decrypt (a keyBits blockBits rk &optional rounds)
  "Rijndael block decryption (internal).
Decryption of one block.  If optional argument rounds is non-null,
decrypt only a certain number of rounds (for debugging only)."
  (let* ((bc (rijndael-bc blockBits))
	(ROUNDS (rijndael-rounds keyBits blockBits))
	(r (1- ROUNDS)))
    ;; make number of rounds sane
    (if (or (null rounds) (> rounds ROUNDS))
	(setq rounds 0))
    ;; First the special round:
    ;;   without InvMixColumn
    ;;   with extra KeyAddition
    (rijndael-key-addition a (aref rk ROUNDS) bc)
    (rijndael-substitution a rijndael-Si bc)
    (rijndael-shift-row a 1 bc)
    (while (> r rounds)
      (rijndael-key-addition a (aref rk r) bc)
      (rijndael-inv-mix-column a bc)
      (rijndael-substitution a rijndael-Si bc)
      (rijndael-shift-row a 1 bc)
      (setq r (1- r)))
    ;; End with the extra key addition
    (if (= rounds 0)
	(rijndael-key-addition a (aref rk 0) bc))))


;; Rijndael API functions:

(defun rijndael-make-key (blockbits keymaterial)
  "Generate Key Schedule given keying material KEYMATERIAL.
KEYMATERIAL must be of length multiple of 32 bits.
BLOCKBITS is size of blocks in bits (valid values 128, 160, 192, 224 and 256)."
  (let ((k (make-vector 4 0)) i
	(keybits (* (length keymaterial) 8)))
    ;; init k
    (setq i 0)
    (while (< i 4)
      (aset k i (make-vector rijndael-maxkc 0))
      (setq i (1+ i)))
    (setq i 0)
    (while (< i (/ keybits 8))
      (aset (aref k (% i 4)) (/ i 4) (aref (vconcat keymaterial) i))
      (setq i (1+ i)))
    (list keybits keymaterial (rijndael-key-schedule k keybits blockbits))))

(defun rijndael-block-encrypt (data key mode blockbits &optional iv)
  "Perform Rijndael encryption of DATA with key KEY.
DATA is a vector with data to encrypt (the block).
KEY is a Rijndael Key Schedule (see `rijndael-make-key').
MODE is a symbol, `ecb', `cbc' or `cfb1', for the encryption mode to use.
BLOCKBITS is size of blocks in bits (valid values 128, 160, 192, 224 and 256).
Optional variable IV is a string containing initialization vectors."
  (let ((numBlocks (/ (length data) (/ blockbits 8)))
	(blk (make-vector 4 0))
	i j l)
    ;; init blk
    (setq i 0)
    (while (< i 4)
      (aset blk i (make-vector rijndael-maxbc 0))
      (setq i (1+ i)))
    (cond ((eq mode 'ecb)
	   (setq i 0)
	   (while (< i numBlocks)
	     (setq j 0)
	     (while (< j (/ blockbits 32))
	       (setq l 0)
	       (while (< l 4)
		 (aset (aref blk l) j (aref data (+ (* (/ blockbits 8) i)
						    (* 4 j)
						    l)))
		 (setq l (1+ l)))
	       (setq j (1+ j)))
	     (rijndael-encrypt blk (nth 0 key) blockbits (nth 2 key))
	     (setq j 0)
	     (while (< j (/ blockbits 32))
	       (setq l 0)
	       (while (< l 4)
		 (aset data (+ (* (/ blockbits 8) i)
			       (* 4 j)
			       l)
		       (aref (aref blk l) j))
		 (setq l (1+ l)))
	       (setq j (1+ j)))
	     (setq i (1+ i))))
	  (t
	   (error "Unknown encryption mode: %s" mode)))
    data))

(defun rijndael-block-decrypt (data key mode blockbits &optional iv)
  "Perform Rijndael encryption of DATA with key KEY.
DATA is a vector with data to encrypt (the block).
KEY is a Rijndael Key Schedule (see `rijndael-make-key').
MODE is a symbol, `ecb', `cbc' or `cfb1', for the decryption mode to use.
BLOCKBITS is size of block in bits (valid values 128, 160, 192, 224 and 256).
Optional variable IV is a string containing initialization vectors."
  (let ((numBlocks (/ (length data) (/ blockbits 8)))
	(blk (make-vector 4 0))
	i j l)
    ;; init blk
    (setq i 0)
    (while (< i 4)
      (aset blk i (make-vector rijndael-maxbc 0))
      (setq i (1+ i)))
    (cond ((eq mode 'ecb)
	   (setq i 0)
	   (while (< i numBlocks)
	     (setq j 0)
	     (while (< j (/ blockbits 32))
	       (setq l 0)
	       (while (< l 4)
		 (aset (aref blk l) j (aref data (+ (* (/ blockbits 8) i)
						    (* 4 j)
						    l)))
		 (setq l (1+ l)))
	       (setq j (1+ j)))
	     (rijndael-decrypt blk (nth 0 key) blockbits (nth 2 key))
	     (setq j 0)
	     (while (< j (/ blockbits 32))
	       (setq l 0)
	       (while (< l 4)
		 (aset data (+ (* (/ blockbits 8) i)
			       (* 4 j)
			       l)
		       (aref (aref blk l) j))
		 (setq l (1+ l)))
	       (setq j (1+ j)))
	     (setq i (1+ i))))
	  (t
	   (error "Unknown decryption mode: %s" mode)))
    data))


;; Conversion functions:

(defun rijndael-hex-to-int (str)
  (if str
      (if (listp str)
	  (+ (* 16 (rijndael-hex-to-int (cdr str)))
	     (cdr (assoc (car str) rijndael-hex-alist)))
	(rijndael-hex-to-int (reverse (append str nil))))
    0))

(defun rijndael-hexstring-to-bitstring (str)
  (let (out)
    (while (< 0 (length str))
      (setq out (cons (rijndael-hex-to-int (substring str -2)) out))
      (setq str (substring str 0 -2)))
    (concat out)))

(defun rijndael-vector-to-hexstring (vec)
  (mapconcat (lambda (el)
	       (format "%02x" el))
	     vec
	     ""))

(defun rijndael-vector-to-bitstring (vec)
  (rijndael-hexstring-to-bitstring (rijndael-vector-to-hexstring vec)))

(defun rijndael-bitstring-to-vector (str)
  (let ((out (make-vector (length str) 0))
	(i (1- (length str))))
    (while (< 0 (length str))
      (aset out i (string-to-char (substring str -1)))
      (setq i (1- i))
      (setq str (substring str 0 -1)))
    out))

(defun rijndael-bitstring-to-hexstring (str)
  (let ((out ""))
    (while (< 0 (length str))
      (setq out (format "%02x%s" (string-to-char (substring str -1)) out))
      (setq str (substring str 0 -1)))
    out))


;; NIST test value generator

(defun rijndael-nist ()
  "Generate NIST test values.
The output is to be compared with the NIST tabulated values."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*NIST Rijndael test values"))
  (erase-buffer)
  (insert "\n\n\nElectronic Codebook (ECB) Mode - ENCRYPTION\n")
  ;; ECB encrypt keysize 128
  (rijndael-nist-ecb-mct
   "00000000000000000000000000000000" 128
   "00000000000000000000000000000000" 128 'encrypt)
  ;; ECB encrypt keysize 160
  (rijndael-nist-ecb-mct
   "0000000000000000000000000000000000000000" 160
   "00000000000000000000000000000000" 128 'encrypt)
  ;; ECB encrypt keysize 192
  (rijndael-nist-ecb-mct
   "000000000000000000000000000000000000000000000000" 192
   "00000000000000000000000000000000" 128 'encrypt)
  ;; ECB encrypt keysize 224
  (rijndael-nist-ecb-mct
   "00000000000000000000000000000000000000000000000000000000" 224
   "00000000000000000000000000000000" 128 'encrypt)
  ;; ECB encrypt keysize 256
  (rijndael-nist-ecb-mct
   "0000000000000000000000000000000000000000000000000000000000000000" 256
   "00000000000000000000000000000000" 128 'encrypt)
  (insert "\n\n\nElectronic Codebook (ECB) Mode - DECRYPTION\n")
  ;; ECB decrypt keysize 128
  (rijndael-nist-ecb-mct
   "00000000000000000000000000000000" 128
   "00000000000000000000000000000000" 128 'decrypt)
  ;; ECB decrypt keysize 160
  (rijndael-nist-ecb-mct
   "0000000000000000000000000000000000000000" 160
   "00000000000000000000000000000000" 128 'decrypt)
  ;; ECB decrypt keysize 192
  (rijndael-nist-ecb-mct
   "000000000000000000000000000000000000000000000000" 192
   "00000000000000000000000000000000" 128 'decrypt)
  ;; ECB decrypt keysize 224
  (rijndael-nist-ecb-mct
   "00000000000000000000000000000000000000000000000000000000" 224
   "00000000000000000000000000000000" 128 'decrypt)
  ;; ECB decrypt keysize 256
  (rijndael-nist-ecb-mct
   "0000000000000000000000000000000000000000000000000000000000000000" 256
   "00000000000000000000000000000000" 128 'decrypt)
  (message "NIST Rijndael tables generation...done"))

(defun rijndael-nist-ecb-mct (initKey keyLength initBlock blockLength dir)
  (let ((binKey (rijndael-bitstring-to-vector
		 (rijndael-hexstring-to-bitstring initKey)))
	(outBlock (rijndael-hexstring-to-bitstring initBlock))
	(i 0) j keyInst inBlock)
    (insert "\n=========================\n\n")
    (insert (format "KEYSIZE=%d\n" keyLength))
    (while (< i rijndael-monte-carlo-loop)
      (sit-for 0)
      (insert (format "\nI=%d\n" i))
      (insert "KEY=" (rijndael-vector-to-hexstring binKey) "\n")
      (setq keyInst (rijndael-make-key blockLength 
				       (rijndael-vector-to-bitstring binKey)))
      (insert (if (eq dir 'encrypt) "PT=" "CT=")
	      (rijndael-bitstring-to-hexstring outBlock) "\n")
      (setq j 0)
      (while (< j rijndael-monte-carlo-limit)
	(if (eq (% j 100) 0)
	    (message (concat "Rijndael NIST ECB MCT %sion (%db keys, "
			     "%db blocks) loop %d of %d%s")
		     dir keyLength blockLength i 
		     rijndael-monte-carlo-loop
		     (if (> rijndael-monte-carlo-limit 1)
			 (format " iteration %d of %d" j
				 rijndael-monte-carlo-limit)
		       "")))
	(setq inBlock (copy-sequence outBlock))
	(if (eq dir 'encrypt)
	    (rijndael-block-encrypt outBlock keyInst 'ecb blockLength)
	  (rijndael-block-decrypt outBlock keyInst 'ecb blockLength))
	(setq j (1+ j)))
      (insert (if (eq dir 'encrypt) "CT=" "PT=")
	      (rijndael-bitstring-to-hexstring outBlock) "\n")
      (cond ((eq keyLength 128)
	     (setq j 0)
	     (while (< j (/ 128 8))
	       (aset binKey j (logxor (aref binKey j)
				      (aref
				       (rijndael-bitstring-to-vector outBlock)
				       j)))
	       (setq j (1+ j))))
	    ((eq keyLength 192)
	     (setq j 0)
	     (while (< j (/ 64 8))
	       (aset binKey j (logxor (aref binKey j)
				      (aref
				       (rijndael-bitstring-to-vector inBlock)
				       (+ j (/ 64 8)))))
	       (setq j (1+ j)))
	     (setq j 0)
	     (while (< j (/ 128 8))
	       (aset binKey (+ j (/ 64 8))
		     (logxor (aref binKey (+ j (/ 64 8)))
			     (aref (rijndael-bitstring-to-vector outBlock) j)))
	       (setq j (1+ j))))
	    ((eq keyLength 256)
	     (setq j 0)
	     (while (< j (/ 128 8))
	       (aset binKey j (logxor (aref binKey j)
				      (aref
				       (rijndael-bitstring-to-vector inBlock)
				       j)))
	       (setq j (1+ j)))
	     (setq j 0)
	     (while (< j (/ 128 8))
	       (aset binKey (+ j (/ 128 8))
		     (logxor (aref binKey j)
			     (aref
			      (rijndael-bitstring-to-vector outBlock)
			      j)))
	       (setq j (1+ j)))))
      (setq i (1+ i)))))

(provide 'rijndael)

;;; rijndael.el ends here

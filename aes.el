DeletedPage
;;; aes.el --- Implementation of AES




;; From now on updates will be published on Github https://github.com/Sauermann/emacs-aes
;; This version is outdated!
















;; Copyright (C) 2008 Markus Sauermann

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA

;; Author: Markus Sauermann <mhoram@gmx.de>
;; Maintainer: Markus Sauermann <mhoram@gmx.de>
;; Created: 15 Feb 2008
;; Version: 0.4
;; Revision: $Id: aes.el 22 2008-10-02 20:12:00Z mhoram $
;; Keywords: data tools

;;; Change Log:

;; 0.1 initial working unpublished version
;; 0.2 performance enhancements:
;;     - keys are now represented as lists
;;     - general optimizations
;;     documentation written
;;     code cleanup
;; 0.3 Bugfix in password generation
;;     documentation cleanup
;; 0.4 Increased ocb performance and adjusted possible blocksizes
;;     Replaced aes-ocb-max-default-length by aes-default-method

;;; Commentary:

;; Config file
;; Insert "(require 'aes)" into your local .emacs file to load this library.
;; Insert "(aes-enable-auto-decryption)" into yout local .emacs file for
;; convenient automatic recognization of encrypted files during loading.

;; Whenever possible, this library should be used byte-compiled, as this
;; provides a great performance boost!

;; Main entry points:
;; `aes-encrypt-current-buffer' Ask for password and encrypt current buffer.
;; `aes-decrypt-current-buffer' Ask for password and decrypt current buffer.
;; `aes-insert-password' Generate a random password from user input.
;; For customizing this library, there is the customization group aes in the
;; applications group.

;; Emacs version 22 is recommended.  It should work with version 21, but there
;; were no tests recently.  Version 23 is not yet tested.

;; This library implements the Rijndael algorithm [1] natively in Emacs and
;; allows to encrypt and decrypt buffers or strings.
;; Rijndael is a superset of the AES algorithm [2].
;; Further this library contains implementations of Cipher-block chaining [4]
;; and Offset Codebook Mode [5].
;; For patent issues about OCB see [6], which allows this distribution.

;; This implementation allows additionally to the AES specification blocklengths
;; of 24 and 32 bytes.

;; Nb denotes the number of 32-bit words in the state.
;; Nk denotes the number of 32-bit words comprising the cipher key.
;; Nr denotes the number of rounds.
;; We allow Nb and Nk to be 4, 6, or 8. and Nr = max(Nb, Nk) + 6

;; Since Emacs implements integers as 29 bit numbers, it is not possible to
;; use the optimization, which requires 32 bit numbers.  For details see [3].
;; This leads to an 8-bit design for this implementation.
;; So the following fitting implementation is used here.
;; - Multiplication and inverting in GF(2^8) are implemented as a table lookups.
;; - The state is implemented as a string of length 4 * Nb.
;; - Plaintext and ciphertext are implemented as unibyte strings.
;; - The expanded key is implemented as a list of length 4 * Nb * (1 + Nr)
;;   with entries '((A . B) . (C . D)), where A, B, C and D are bytes.  It is
;;   precalculated before the en-/decryption algorithms.
;; - The S-boxes are implemented by lookup tables.
;; - The three operations ByteSub, ShiftRow and MixColumn together with
;;   round-key-addition are implemented in the functions `aes-SubShiftMixKeys'
;;   and `aes-InvSubShiftMixKeys' for encryption and decryption respectively.
;; - CBC mode is implemented straightforward, using a 0-padding to the full
;;   blocklength.  The IV is appended to and saved with the ciphertext.
;; - OCB mode made the implementation of a pmac, based on AES, necessary, but
;;   the further details were straightforward.  The IV is appended to the
;;   ciphertext.  During decryption the created hash-value is checked.
;; - the function `aes-key-from-passwd' generates an AES key from an user input
;;   string (password).
;; - Further a facility is provided to generate random passwords, based on
;;   random user input like mousemovement, time and keyinput.
;; - The ciphertext is usually converted to a base-64 encoded string.

;; The version of the internal storage format of encrypted data is 1.2.

;; Known Bugs:
;; - Encrypted buffers are Auto-Saved unencrypted.
;; - Exiting Emacs via C-x-c saves buffers unencrypted.
;; - This implementation is not resistant against DPA attacks.
;; - `aes-auto-decrypt' is not completely compliant to Emacs standards.

;; [1] http://csrc.nist.gov/archive/aes/rijndael/Rijndael-ammended.pdf
;; [2] http://csrc.nist.gov/publications/fips/fips197/fips-197.pdf
;; [3] http://www.openssl.org/
;; [4] http://en.wikipedia.org/wiki/Block_cipher_modes_of_operation
;; [5] http://tools.ietf.org/html/draft-krovetz-ocb-00
;; [6] http://www.cs.ucdavis.edu/~rogaway/ocb/grant.htm

;;; Code:

;;;; Helpers

(defun aes-xor (x y)
  "Return X and Y bytewise xored.
X and Y and the return values are unibyte strings.
Y must not be shorter than X."
  (let* ((l (length x))
         (res (make-string l 0))
         (i 0))
    (while (< i l)
      (aset res i (logxor (aref x i) (aref y i)))
      (setq i (1+ i)))
    res))

(defun aes-xor-de (x y)
  "Calculate X and Y bytewise xored destructively in X.
X and Y are unibyte strings.  Y must not be shorter than X.
The result is stored in X.
The return value is X."
  (let* ((l (length x))
         (i 0))
    (while (< i l)
      (aset x i (logxor (aref x i) (aref y i)))
      (setq i (1+ i))))
  x)

(defun aes-xor-4 (x y)
  "Return the 4 byte objects X and Y bytewise xored as new cons cell.
X and Y are objects of the form '((A . B) . (C . D))"
  (cons (cons (logxor (car (car x)) (car (car y)))
              (logxor (cdr (car x)) (cdr (car y))))
        (cons (logxor (car (cdr x)) (car (cdr y)))
              (logxor (cdr (cdr x)) (cdr (cdr y))))))

(defun aes-xor-4-de (x y)
  "X and Y are bytewise xored destructively in X.
X and Y are objects of the form '((A . B) . (C . D))"
  (setcar (car x) (logxor (car (car x)) (car (car y))))
  (setcdr (car x) (logxor (cdr (car x)) (cdr (car y))))
  (setcar (cdr x) (logxor (car (cdr x)) (car (cdr y))))
  (setcdr (cdr x) (logxor (cdr (cdr x)) (cdr (cdr y)))))

(defun aes-enlarge-to-multiple (v bs)
  "Enlarge unibyte string V to a multiple of number BS and pad with Zeros.
Return a new unibyte string containing the result.  V is not changed"
  (concat v (make-string (mod (- (string-bytes v)) bs) 0)))

(defun aes-enlarge-to-multiple-num (n bs)
  "Return the smallest multiple of BS, not smaller than N."
  (+ n (mod (- n) bs) 0))

(defun aes-str-to-b (str)
  "Convert the unibyte string STR to a list-representation.
The length of STR must be a multiple of 4.
The length of the resulting list has a quarter of the length of STR.
Elements 4*K to 4*K+3 of STR (named A, B, C and D in this order) are stored in
position K of the result as '((A . B) . (C . D))."
  (let (res
        (l (length str))
        (i 0))
    (while (< i l)
      (setq res (cons
                 (cons (cons (aref str i) (aref str (1+ i)))
                       (cons (aref str (+ i 2)) (aref str (+ i 3))))
                 res))
      (setq i (+ i 4)))
    (nreverse res)))

;;;; Multiplication

(defun aes-mul-pre (a b)
  "Multiply the bytes A and B in GF(2^8) and return their product."
  ;; For a description, see [1, Ch 2.1.2] or [2. Ch 4.2.1]
  (let ((p 0)
        (c 0))
    (while (< c 8)
      (if (= 1 (logand b 1))
          (setq p (logxor a p)))
      (if (prog1 (= #x80 (logand a #x80))
            (setq a (logand #xff (lsh a 1))))
          (setq a (logxor a #x1b)))
      (setq b (lsh b -1))
      (setq c (1+ c)))
    p))

(let ((l (make-string 256 0))
      (mt (make-vector #xf 0))
      (x 0)
      i res)
  (while (< x #xf)
    (aset mt x (make-string 256 0))
    (setq x (1+ x)))
  (setq x 1)
  (while (< x 256)
    (setq i x)
    (while (< i 256)
      (setq res (aes-mul-pre i x))
      (if (= #x01 res) (progn (aset l x i) (aset l i x)))
      (and (< x #xf) (aset (aref mt x) i res)
           (and (< i #xf) (aset (aref mt i) x res)))
      (setq i (1+ i)))
    (setq x (1+ x)))
  (defconst aes-inv-table l
    "This variable contains the GF(2^8) inverting lookup table.")
  ;; The following 6 tables are used during the time critical
  ;; functions `aes-SubShiftMixKeys' and `aes-InvSubShiftMixKeys'
  (defconst aes-l2 (aref mt #x02))
  (defconst aes-l3 (aref mt #x03))
  (defconst aes-l9 (aref mt #x09))
  (defconst aes-lb (aref mt #x0b))
  (defconst aes-ld (aref mt #x0d))
  (defconst aes-le (aref mt #x0e)))

;;;; SubBytes Transformation

(let ((l1 (make-string 256 0))
      (l2 (make-string 256 0))
      (x 0)
      b g i)
  (while (< x 256)
    (setq b (aref aes-inv-table x))
    (setq g 0)
    (setq i 0)
    (while (< i 8)
      (setq g (logxor g (lsh (logand 1 (logxor (lsh (logxor b #x63) (- i))
                                               (lsh b (- (% (+ i 4) 8)))
                                               (lsh b (- (% (+ i 5) 8)))
                                               (lsh b (- (% (+ i 6) 8)))
                                               (lsh b (- (% (+ i 7) 8)))))
                             i)))
      (setq i (1+ i)))
    (aset l1 x g)
    (aset l2 g x)
    (setq x (1+ x)))
  (defconst aes-s-boxes-enc l1
    "This variable contains the encryption S-Boxes.
The S-boxes are stored as strings of length 256.")
  (defconst aes-s-boxes-dec l2
    "This variable contains the decryption S-Boxes.
The S-boxes are stored as strings of length 256."))
;; For a description see [1, Ch 4.2.1] or [2, Ch 5.1.1]

(defun aes-SubBytes (state)
  "Apply the SubBytes transformation to each byte of the unibyte string STATE.
STATE may be of arbitrary length."
  ;; For a description of SubBytes see [1, Ch 4.2.1] or [2, Ch 5.1.1]
  (let ((l (length state))
        (i 0))
    (while (< i l)
      (aset state i (aref aes-s-boxes-enc (aref state i)))
      (setq i (1+ i)))))

(defun aes-InvSubBytes (state)
  "Apply the InvSubBytes transformation to each byte of the string STATE.
The unibyte string STATE may be of arbitrary length."
  ;; For a description of InvSubBytes see [1, Ch 4.2.1] or [2, Ch 5.3.2]
  (let ((l (length state))
        (i 0))
    (while (< i l)
      (aset state i (aref aes-s-boxes-dec (aref state i)))
      (setq i (1+ i)))))

(defun aes-SubWord (x)
  "Apply the SubBytes transformation to all 4 bytes of X.
X is of the form '((A . B) . (C . D))."
  (setcar (car x) (aref aes-s-boxes-enc (car (car x))))
  (setcdr (car x) (aref aes-s-boxes-enc (cdr (car x))))
  (setcar (cdr x) (aref aes-s-boxes-enc (car (cdr x))))
  (setcdr (cdr x) (aref aes-s-boxes-enc (cdr (cdr x)))))

;;;; ShiftRows Transformation

(defun aes-ShiftRows (state)
  "Apply the shift rows transformation destructively in STATE.
The length of the unibyte string STATE must be a multiple of 4 and larger
than 12."
  ;; For a description of ShiftRows see [1, Ch 4.2.2] or [2, Ch 5.1.2]
  (let* ((border (- (length state) 4))
         (x (aref state 1))
         (c 1)
         (y (aref state 6))
         (z (aref state 11)))
    (while (< c border)
      (aset state c (aref state (+ c 4)))
      (setq c (+ c 4)))
    (aset state c x)
    (setq x (aref state 2))
    (setq c 2)
    (setq border (- border 4))
    (while (< c border)
      (aset state c (aref state (+ c 8)))
      (setq c (+ c 4)))
    (aset state c x)
    (aset state (+ c 4) y)
    (setq x (aref state 3))
    (setq y (aref state 7))
    (setq c 3)
    (setq border (- border 4))
    (while (< c border)
      (aset state c (aref state (+ c 12)))
      (setq c (+ c 4)))
    (aset state c x)
    (aset state (+ c 4) y)
    (aset state (+ c 8) z)))

(defun aes-InvShiftRows (state)
  "Apply the inverted shift rows transformation destructively in STATE.
The length of the unibyte string STATE must be a multiple of 4 and larger
than 12."
  ;; For a description of InvShiftRows see [1, Ch 4.2.2] or [2, Ch 5.3.1]
  (let* ((Nb4 (length state))
         (c (- Nb4 3))
         (x (aref state c))
         (y (aref state (- Nb4 6)))
         (z (aref state (- Nb4 9))))
    (while (< 4 c)
      (aset state c (aref state (setq c (- c 4)))))
    (aset state 1 x)
    (setq x (aref state (setq c (- Nb4 2))))
    (while (< 8 c)
      (aset state c (aref state (- c 8)))
      (setq c (- c 4)))
    (aset state 6 x)
    (aset state 2 y)
    (setq x (aref state (setq c (- Nb4 1))))
    (setq y (aref state (- c 4)))
    (while (< 12 c)
      (aset state c (aref state (- c 12)))
      (setq c (- c 4)))
    (aset state 11 x)
    (aset state 7 y)
    (aset state 3 z)))

;;;; Combined Single Round Transformation

(defsubst aes-SubShiftMixKeys (state keys)
  "Apply one round of the aes encryption destructively to the string STATE.
KEYS is a list containing a part of the expanded key schedule.  See
`aes-KeyExpansion' for how KEYS looks like.
The relevant keys for this round are stored in the first Nb elements of KEYS,
which means that the length of KEYS is at least Nb.
In this function the 4 transformations SubBytes, ShiftRows, MixColumns and
AddRoundKey of one aes round are applied to STATE.
The length of the unibyte string STATE is a multiple of 4 and larger than 12."
  ;; For a description of MixColumns see [1, Ch 4.2.3] or [2, Ch 5.1.3]
  ;; For a description of AddRoundKey see [1, Ch 4.2.4] or [2, Ch 5.1.4]
  (let* ((copy (copy-sequence state))
         (x4 0)
         (Nb4 (length state))
         s0 s1 s2 s3 keyA)
    (while (< x4 Nb4)
      (setq s0 (aref aes-s-boxes-enc (aref copy x4)))
      (setq s1 (aref aes-s-boxes-enc (aref copy (% (+ x4 1 4) Nb4))))
      (setq s2 (aref aes-s-boxes-enc (aref copy (% (+ x4 2 8) Nb4))))
      (setq s3 (aref aes-s-boxes-enc (aref copy (% (+ x4 3 12) Nb4))))
      (setq keyA (car keys))
      (aset state x4 (logxor (aref aes-l2 s0) (aref aes-l3 s1) s2 s3
                             (car (car keyA))))
      (aset state (1+ x4) (logxor s0 (aref aes-l2 s1) (aref aes-l3 s2) s3
                                  (cdr (car keyA))))
      (aset state (+ 2 x4) (logxor s0 s1 (aref aes-l2 s2) (aref aes-l3 s3)
                                   (car (cdr keyA))))
      (aset state (+ 3 x4) (logxor (aref aes-l3 s0) s1 s2 (aref aes-l2 s3)
                                   (cdr (cdr keyA))))
      (setq keys (cdr keys))
      (setq x4 (+ x4 4)))))

(defsubst aes-InvSubShiftMixKeys (state keys)
  "Apply the 4 inverted transformations destructively to STATE.
See `aes-SubShiftMixKeys' for additional information.
Note that the part of the key espansion KEYS is in the reverse order than it was
in `aes-SubShiftMixKeys'."
  ;; For a description of InvMixColumns see [1, Ch 4.2.3] or [2, Ch 5.3.3]
  ;; For a description of InvAddRoundKey see [1, Ch 4.2.4] or [2, Ch 5.3.4]
  (let* ((copy (copy-sequence state))
         (Nb4 (length state))
         (x4 (- Nb4 4))
         s0 s1 s2 s3 keyA)
    (while (<= 0 x4)
      (setq keyA (car keys))
      (setq s0 (logxor (aref copy x4) (car (car keyA))))
      (setq s1 (logxor (aref copy (1+ x4)) (cdr (car keyA))))
      (setq s2 (logxor (aref copy (+ 2 x4)) (car (cdr keyA))))
      (setq s3 (logxor (aref copy (+ 3 x4)) (cdr (cdr keyA))))
      (aset state x4
            (aref aes-s-boxes-dec (logxor (aref aes-le s0) (aref aes-lb s1)
                                          (aref aes-ld s2) (aref aes-l9 s3))))
      (aset state (% (+ 1 4 x4) Nb4)
            (aref aes-s-boxes-dec (logxor (aref aes-l9 s0) (aref aes-le s1)
                                          (aref aes-lb s2) (aref aes-ld s3))))
      (aset state (% (+ 2 8 x4) Nb4)
            (aref aes-s-boxes-dec (logxor (aref aes-ld s0) (aref aes-l9 s1)
                                          (aref aes-le s2) (aref aes-lb s3))))
      (aset state (% (+ 3 12 x4) Nb4)
            (aref aes-s-boxes-dec (logxor (aref aes-lb s0) (aref aes-ld s1)
                                          (aref aes-l9 s2) (aref aes-le s3))))
      (setq x4 (- x4 4))
      (setq keys (cdr keys)))))

;;;; Key Expansion

(defun aes-RotWord (x)
  "Rotate X by one byte.
Append the first byte to the end."
  (let ((te (car (car x))))
    (setcar (car x) (cdr (car x)))
    (setcdr (car x) (car (cdr x)))
    (setcar (cdr x) (cdr (cdr x)))
    (setcdr (cdr x) te)))

(defun aes-KeyExpansion (key Nb &optional Nr)
  "Return a list, containing the key expansion of KEY.
KEY is a list of NK elements with entries '((A . B) . (C . D)), where A, B, C
and D are bytes.
NB, NK and NR are defined in the Commentary section of the sourcecode.
The expanded key is a list of length 4 * Nb * (1 + Nr) with
entries of the same form as in KEY."
  ;; For a description of the key expansion see [1, Ch 4.3.1] or [2, Ch 5.2]
  (let* ((Nk (length key))
         (w (reverse key))
         (i Nk)
         (rcon (cons (cons 1 0) (cons 0 0)))
         (Nk2 (lsh Nk 2))
         (border (* Nb (1+ (or Nr (+ (max Nb Nk) 6)))))
         (temp (cons (cons nil nil) (cons nil nil)))
         f)
    (while (< i border)
      (setq f (car w))
      (setcar (car temp) (car (car f)))
      (setcdr (car temp) (cdr (car f)))
      (setcar (cdr temp) (car (cdr f)))
      (setcdr (cdr temp) (cdr (cdr f)))
      (if (= 0 (% i Nk))
          (progn (aes-RotWord temp)
                 (aes-SubWord temp)
                 (aes-xor-4-de temp rcon)
                 (setcar (car rcon) (aref aes-l2 (car (car rcon)))))
        (if (and (< 6 Nk) (= (% i Nk) 4))
            (aes-SubWord temp)))
      (setq w (cons (aes-xor-4 (nth 3 w) temp) w))
      (setq i (1+ i)))
    (nreverse w)))

;;;; Add Round Key

(defun aes-AddRoundKey (state keys)
  "Apply one AddRoundKey transformation to the unibyte string STATE.
Use the first NB elements of the list KEYS as keys.
NB denotes the number of 32-bit words in the state.
KEYS is a part of the key expansion as defined in `aes-SubShiftMixKeys'.
The length of STATE is a multiple of 8 and larger than 12."
  ;; For a description of AddRoundKey see [1, Ch 4.2.4] and [1, Ch 4.3.2]
  ;; or [2, Ch 5.1.4]
  (let ((Nb4 (length state))
        (i 0)
        keysA)
    (while (< i Nb4)
      (setq keysA (car keys))
      (aset state i (logxor (aref state i) (car (car keysA))))
      (aset state (1+ i) (logxor (aref state (1+ i)) (cdr (car keysA))))
      (aset state (+ 2 i) (logxor (aref state (+ 2 i)) (car (cdr keysA))))
      (aset state (+ 3 i) (logxor (aref state (+ 3 i)) (cdr (cdr keysA))))
      (setq keysA (car (setq keys (cdr keys))))
      (aset state (+ 4 i) (logxor (aref state (+ 4 i)) (car (car keysA))))
      (aset state (+ 5 i) (logxor (aref state (+ 5 i)) (cdr (car keysA))))
      (aset state (+ 6 i) (logxor (aref state (+ 6 i)) (car (cdr keysA))))
      (aset state (+ 7 i) (logxor (aref state (+ 7 i)) (cdr (cdr keysA))))
      (setq keys (cdr keys))
      (setq i (+ 8 i)))))

(defun aes-InvAddRoundKey (state keys)
  "Apply one AddRoundKey transformation to the unibyte string STATE.
Use the first NB elements of the list KEYS as keys.
NB denotes the number of 32-bit words in the state.
KEYS is a part of the key expansion as defined in `aes-InvSubShiftMixKeys'."
  ;; For a description of the inverse of AddRoundKey see [1, Ch 4.2.4] and
  ;; [1, Ch 4.3.2] or [2, Ch 5.3.4]
  (let* ((Nb4 (length state))
         (i (- Nb4 4))
         keysA)
    (while (<= 0 i)
      (setq keysA (car keys))
      (aset state i (logxor (aref state i) (car (car keysA))))
      (aset state (1+ i) (logxor (aref state (1+ i)) (cdr (car keysA))))
      (aset state (+ 2 i) (logxor (aref state (+ 2 i)) (car (cdr keysA))))
      (aset state (+ 3 i) (logxor (aref state (+ 3 i)) (cdr (cdr keysA))))
      (setq keys (cdr keys))
      (setq i (- i 4)))))

;;;; AES Cipher

(defun aes-Cipher (plain keys Nb &optional Nr)
  "Perform a complete aes encryption of the unibyte string PLAIN.
Return a new string containing the encrypted string PLAIN.
Use KEYS as the expanded key as defined in `aes-SubShiftMixKeys'.
NB is the number of 32-bit words in PLAIN.  NR is the number of rounds.
The length of KEYS is (1 + NR) * NB."
  ;; For a description of the AES cipher see [1, Ch 4.4] or [2, Ch 5.1]
  (let* ((state (make-string (lsh Nb 2) 0))
         (r 1))
    (unless Nr (setq Nr (+ (max Nb (- (/ (length keys) Nb) 7)) 6)))
    (store-substring state 0 plain)
    (aes-AddRoundKey state keys)
    (while (< r Nr)
      (aes-SubShiftMixKeys state (setq keys (nthcdr Nb keys)))
      (setq r (1+ r)))
    (aes-SubBytes state)
    (aes-ShiftRows state)
    (aes-AddRoundKey state (nthcdr Nb keys))
    state))

(defun aes-InvCipher (cipher keys Nb &optional Nr)
  "Perform a complete aes decryption of the unibyte string CIPHER.
Return a new string containing the decrypted string CIPHER.
Use KEYS as the expanded key as defined in `aes-InvSubShiftMixKeys'.
NB is the number of 32-bit words in CIPHER.  NR is the number of rounds.
The length of KEYS is (1 + NR) * NB."
  ;; For a description of the inverted AES cipher see [1, Ch 5.3] or [2, Ch 5.3]
  (let* ((state (make-string (lsh Nb 2) 0))
         (r (or Nr (+ (max Nb (- (/ (length keys) Nb) 7)) 6))))
    (store-substring state 0 cipher)
    (aes-InvAddRoundKey state keys)
    (aes-InvShiftRows state)
    (aes-InvSubBytes state)
    (while (< 1 r)
      (aes-InvSubShiftMixKeys state (setq keys (nthcdr Nb keys)))
      (setq r (1- r)))
    (aes-InvAddRoundKey state (nthcdr Nb keys))
    state))

;;;; Cipher-Block Chaining

(defun aes-cbc-encrypt (plain iv keys Nb)
  "Encrypt the string PLAIN by the cbc method using aes for encryption.
Return a new unibyte string containing the result and dont change PLAIN.
Use the unibyte string IV as initialization vector and KEYS as the complete key
expansion as defined in `aes-SubShiftMixKeys'.
The length of IV must be the blocksize NB * 4.
If the length of the unibyte string PLAIN is not a multiple of the
blocksize, then append to PLAIN as many zeros as are needed to get a
complete blocksize."
  ;; For a description of the CBC mode see [4]
  (let* ((Nb4 (lsh Nb 2))
         (res (aes-enlarge-to-multiple plain Nb4))
         (len (length res))
         (p 0))
    (while (< p len)
      (store-substring
       res p
       (setq iv (aes-Cipher (aes-xor iv (substring res p (setq p (+ p Nb4))))
                            keys Nb))))
    res))

(defun aes-cbc-decrypt (c iv keys Nb)
  "Decrypt the string C by the cbc method using aes for decryption.
Return a new unibyte string containing the result and dont change C.
Use the unibyte string IV as initialization vector and KEYS as the complete key
expansion as defined in `aes-InvSubShiftMixKeys'.
The length of IV must be the blocksize NB * 4.
The length of the unibyte strings C and the result are identical and a
multiple of the blocksize."
  ;; For a description of the CBC mode see [4]
  (let* ((Nb4 (lsh Nb 2))
         (len (length c))
         (res (make-string len 0))
         (p 0))
    (while (< p len)
      (store-substring
       res p
       (aes-xor iv (aes-InvCipher (setq iv (substring c p (setq p (+ p Nb4))))
                                  keys Nb))))
    res))

;;;; Offset Codebook Mode 2.0

(defun aes-ocb-double-de (x)
  "Calculate X multiplicated by 2.
The calculation is done in a bit field according to the length of X.
This is done destructively in the unibyte string X.
The length of X is 16, 24, 32, 40, 48, 56 or 64 bytes.
The return value is the result."
  ;; For a description of the multiplication see [4, Ch 2]
  (let* ((len (length x))
         (len1 (and (or (member len '(16 24 32 40 48 56 64))
                        (error "%s \"%s\" is not allowed."
                               "The specified blocksize of string" x))
                    (- len 2)))
         (c (* (aref [135  135 1061 27 4107 2115 293] (- (lsh len -3) 2))
               (lsh (aref x 0) -7)))
         (c1 (logand (lsh c -8) #xff))
         (i -1))
    (setq c (logand c #xff))
    (while (< i len1)
      (aset x (setq i (1+ i)) (logand #xff (logxor (lsh (aref x i) 1)
                                                   (lsh (aref x (+ i 1)) -7)))))
    (aset x i (logxor (aref x i) c1))
    (aset x (1- len) (logxor (logand #xff (lsh (aref x (1- len)) 1)) c)))
  x)

(defun aes-ocb-triple-de (x)
  "Return X multiplicated by 3.
The calculation is done in a bit field according to the length of X.
X and the return value area unibyte strings of arbitrary length.
This is done destructively in X.
Return X."
  ;; For a description of the multiplication see [4, Ch 2]
  (aes-xor-de x (aes-ocb-double-de (copy-sequence x))))

(defun aes-num2str (x n)
  "Calculate the N-byte representation of the number X.
Return a unibyte string of length N containing the representation of X,
where the most significant byte is at position 0."
  (let ((res (make-string n 0))
        (offset n))
    (while (< 0 x)
      (aset res (setq offset (1- offset)) (logand x #xff))
      (setq x (lsh x -8)))
    res))

(defun aes-pmac (header keys Nb)
  "Calculate the pmac of HEADER using aes encryption.
NB * 4 denotes the blocksize.
Return the pmac of the unibyte string HEADER of arbitrary length as unibyte
string of blocksize length.
KEYS is the expanded key as defined in `aes-KeyExpansion'."
  ;; For a description of the pmac see [4, Ch 4]
  (let* ((l (length header))
         (bs (lsh Nb 2))
         (whole-blocks (/ l bs))
         (total-blocks (max 1 (+ whole-blocks (if (= 0 (% l bs)) 0 1))))
         (border (* whole-blocks total-blocks))
         (b (if (= whole-blocks total-blocks) bs (% l bs)))
         (D (aes-ocb-triple-de
             (aes-ocb-triple-de (aes-Cipher (make-string bs 0) keys Nb))))
         (checksum (make-string bs 0))
         (p 0))
    (while (< p border)
      (aes-ocb-double-de D)
      (aes-xor-de
       checksum
       (aes-Cipher (aes-xor D (substring header p (setq p (+ p bs)))) keys Nb)))
    (aes-ocb-triple-de (aes-ocb-double-de D))
    (if (= b bs)
        (aes-xor-de checksum (substring header (* bs (1- total-blocks))))
      (aes-ocb-triple-de D)
      (aes-xor-de checksum (concat (substring header (* bs (1- total-blocks)))
                                   (eval-when-compile (char-to-string #x80))
                                   (make-string (- bs b 1) 0))))
    (aes-Cipher (aes-xor D checksum) keys Nb)))

(defun aes-ocb-encrypt (header input iv keys Nb)
  "Encrypt the string INPUT using OCB.
Additionally generate a pmac of HEADER and INPUT.
HEADER and INPUT are unibyte strings of arbitrary length.
NB describes the blocksize which is NB * 4 bytes.
KEYS contains the expanded key as described in `aes-KeyExpansion'.
IV is a unibyte string of length blocksize containing the initialization vector.
Return a cons cell (C . P), where C is a unibyte string containing the
ciphertext and the unibyte string P of blocksize length is the hash value."
  ;; For a description of the ocb encryption see [4, Ch 5]
  (let* ((D (aes-Cipher iv keys Nb))
         (C (make-string (length input) 0))
         P
         (checksum (make-string (lsh Nb 2) 0))
         (l (length input))
         (blocksize (lsh Nb 2))
         (whole-blocks (/ l blocksize))
         (total-blocks (max 1 (+ whole-blocks (if (= 0 (% l blocksize)) 0 1))))
         (border (* blocksize (1- total-blocks)))
         (b (if (= whole-blocks total-blocks) blocksize (% l blocksize)))
         (pointer 0))
    (while (< pointer border)
      (aes-ocb-double-de D)
      (setq checksum (aes-xor checksum (substring input pointer
                                                  (+ pointer blocksize))))
      (store-substring C pointer
                       (aes-xor D (aes-Cipher
                                   (aes-xor D (substring
                                               input pointer
                                               (setq pointer
                                                     (+ pointer blocksize))))
                                   keys Nb))))
    (aes-ocb-double-de D)
    (let ((pad (aes-Cipher (aes-xor D (aes-num2str (lsh b 3) blocksize))
                           keys
                           Nb))
          (Mm (substring input pointer)))
      (store-substring C pointer (aes-xor Mm (substring pad 0 b)))
      (aes-xor-de checksum (concat Mm (substring pad b))))
    (setq P (aes-Cipher (aes-xor checksum (aes-ocb-triple-de D)) keys Nb))
    (if (< 0 (length header)) (aes-xor-de P (aes-pmac header keys Nb)))
    (cons C P)))

(defun aes-ocb-decrypt (header input tag iv keys &optional Nb)
  "Decrypt the string INPUT using OCB.
Additionally verify the pmac hash value of HEADER and INPUT with TAG.
HEADER and INPUT are unibyte strings of arbitrary length.
NB describes the blocksize which is NB * 4 bytes.
TAG is a unibyte string of blocksize length containing the hash value generated
during encryption.
KEYS contains the expanded key as described in `aes-KeyExpansion'.
IV is a unibyte string of length blocksize containing the initialization vector.
Return the plaintext as unibte string, if the hashvalue fits.
Otherwise return nil."
  ;; For a description of the ocb decryption see [4, Ch 6]
  (unless Nb (setq Nb 4))
  (let* ((D (aes-Cipher iv keys Nb))
         (M (make-string (length input) 0))
         (l (length input))
         (blocksize (lsh Nb 2))
         (checksum (make-string blocksize 0))
         (whole-blocks (/ l blocksize))
         (total-blocks (max 1 (+ whole-blocks (if (= 0 (% l blocksize)) 0 1))))
         (border (* blocksize (1- total-blocks)))
         (b (if (= whole-blocks total-blocks) blocksize (% l blocksize)))
         (pointer 0)
         Mi)
    (setq keys (nreverse keys))
    (while (< pointer border)
      (aes-ocb-double-de D)
      (store-substring
       M pointer
       (setq Mi (aes-xor D (aes-InvCipher
                            (aes-xor D (substring input pointer
                                                  (setq pointer
                                                        (+ pointer blocksize))))
                            keys Nb))))
      (aes-xor-de checksum Mi))
    (setq keys (nreverse keys))
    (aes-ocb-double-de D)
    (let* ((pad (aes-Cipher (aes-xor D (aes-num2str (* 8 b) blocksize))
                            keys Nb))
           (Mm (aes-xor (substring
                         input (* blocksize (- total-blocks 1)))
                        (substring pad 0 b))))
      (store-substring M pointer Mm)
      (aes-xor-de checksum
                  (concat Mm (substring pad b))))
    (aes-ocb-triple-de D)
    (let ((T (aes-Cipher (aes-xor D checksum) keys Nb)))
      (if (< 0 (length header))
          (setq T (aes-xor T (aes-pmac header keys Nb))))
      (if (equal tag
                 (substring T 0 (length tag)))
          M
        nil))))

;;;; Password handling and key generation from passwords

(defgroup aes nil
  "Advanced Encryption Standard implementation"
  :group 'applications)

(defcustom aes-always-ask-for-passwords t
  "Always ask for passwords, if non-nil.
If this variable is set to a non-nil value, then everytime a buffer or string is
encrypted/decrypted, the according password is asked from the user and no
passwords are stored in `aes-plaintext-passwords'.
Set this to nil, if you are risky."
  :type 'boolean
  :group 'aes)

(defcustom aes-enable-plaintext-password-storage nil
  "Store passwords in emacs-memory in plaintext, if non-nil.
Enabling this feature allows everyone to read the passwords in plaintext by
accessing the variable `aes-plaintext-passwords'.
If changing the value from non-nil to nil, then the passwords stored in
`aes-plaintext-passwords' are not deleted automatically.
Set this to a non-nil value, if you are risky."
  :type 'boolean
  :group 'aes)

(defvar aes-plaintext-passwords ()
  "Association list of plaintext passwords.
Warning: passwords are stored in plaintext and can be read by anyone with
access to the current Emacs session.
Every entry of this list consists of (A . B), where A and B are strings.
With A the password B can be refered to.")

(defun aes-clear-plaintext-keys ()
  "Remove all stored plaintext passwords."
  (interactive)
  (setq aes-plaintext-passwords))

(defvar aes-idle-timer-value nil
  "Reference to idle timer.
If this is non-nil, then it referes to an idle-timer function, which removes
all stored plaintext passwords.")

(defun aes-idle-clear-plaintext-keys ()
  "Remove all stored plaintext passwords.
This function is called, when idle-password-clearing is activated.
This function also clears the message buffer, as it might contain confidential
content."
  (setq aes-plaintext-passwords)
  (setq aes-idle-timer-value nil)
  (with-current-buffer "*Messages*"
    (erase-buffer))
  (message "AES Passwords cleared."))

(defcustom aes-delete-passwords-after-idle 1
  "Delete the stored plaintext passwords after the given time.
This is disabled, if the value is 0. Otherwise the number is
interpreted as seconds for Emacs to be idle before the deletion
happens."
  :type 'integer
  :group 'aes)

(defvar aes-path-passwd-hook ()
  "Hook for grouping filenames.
Functions, appended to this hook, get one argument: a path of a file to be
en- or decrypted.
According to the path the function should return a string, providing
information about the group of files, or nil otherwise.
Using this method it is possible to store the same password, used for multiple
files.")

(defun aes-exec-passws-hooks (path)
  "Run the functions in the hook `aes-path-passwd-hook'.
PATH is a file system path, that is passed as argument to each function.
Return a string resulting from the first hook that returns a non-nil value.
Return nil, if every function in the hook returns nil."
  (run-hook-with-args-until-success 'aes-path-passwd-hook path))

(defun aes-key-from-passwd (Nk usage type-or-file)
  "Return a key, generated from a password.
This is done by encrypting the password by a key generated from the password
using a constant initialization vector.
USAGE must be a string either \"encryption\" or \"decryption\" denoting the
usage of the password.
TYPE-OR-FILE is a string describing what the password is used for. If the key is
used for a string, TYPE-OR-FILE should be \"string\". If the key is used for a
group of files (by using `aes-path-passwd-hook'), it should be a string denoting
the group. Otherwise the key is used for a file not in a group and in this case
it should be the filename.
If `aes-use-plaintext-keys' is nil and `aes-disable-global-plaintext-keys' is
non-nil, then use `aes-plaintext-passwords' for storing and reading passwords.
Passwords are only stored there, if TYPE-OR-FILE denotes a group of files.
Query the password from the user if it is not available via
`aes-plaintext-passwords'. This implementation does not test the quality of the
password.
Return the key generated from the password. The key is a string of
length NK * 4."
  (if (not (member usage '("encryption" "decryption")))
      (error "Wrong argument in aes-key-from-passwd: \"%S\"" usage))
  (let* (passwd passwdkeys (p ""))
    (if (and (not aes-always-ask-for-passwords)
             aes-enable-plaintext-password-storage
             (assoc type-or-file aes-plaintext-passwords))
        (setq passwd (cdr (assoc type-or-file aes-plaintext-passwords)))
      (while (equal p "")
        (setq p (read-passwd
                 (concat usage " Password for " type-or-file ": ")
                 (equal "encryption" usage))))
      (if (and (not aes-always-ask-for-passwords)
               aes-enable-plaintext-password-storage
               (not (get-buffer type-or-file))
               (not (equal "string" type-or-file)))
          (progn
            ;; store the new password
            (setq aes-plaintext-passwords
                  (cons (cons type-or-file p) aes-plaintext-passwords))
            ;; reset idle timer
            (if aes-idle-timer-value
                (progn (cancel-timer aes-idle-timer-value)
                       (setq aes-idle-timer-value nil)))
            ;; set new idle timer
            (if (< 0 aes-delete-passwords-after-idle)
                (setq aes-idle-timer-value
                      (run-with-idle-timer
                       aes-delete-passwords-after-idle
                       nil
                       'aes-idle-clear-plaintext-keys)))))
      (setq passwd p))
    (setq passwd (aes-enlarge-to-multiple passwd (lsh Nk 2)))
    (setq passwdkeys
          (aes-KeyExpansion
           (aes-str-to-b (substring passwd 0 (lsh Nk 2))) Nk))
    (substring (aes-cbc-encrypt passwd (make-string (lsh Nk 2) 0) passwdkeys Nk)
               (- (lsh Nk 2)))))

(defcustom aes-password-char-groups
  '((?a t "abcdefghjkmnopqrstuvwxyz") ; downcase letters, i and l excluded
    (?A t "ABCDEFGHJKLMNPQRSTUVWXYZ") ; upcase letters, I and O excluded
    (?5 t "23456789")                 ; numbers, 0 and 1 excluded
    (?0 t "0OilI1")                   ; characters difficult to distinguish
    (?. nil ",.!?;:_()[]{}<>")        ; punctuation and brackets
    (?+ nil "-+*/=")                  ; calculation
    (?% nil "|^~#$%&'"))              ; others
  "Groups of characters for password generation.
The first entry in each list is a character, which can be used in the
argument TYP of `aes-generate-password' to refer to this password
group.  The second entry denotes the default value of the application
of this character group; if it is non-nil, the this group is activated and used.
The third entry denotes the characters in this group used in password
generation."
  :group 'aes
  :type '(repeat (list character (choice (const :tag "active" t)
                                         (const :tag "inactive" nil))
                       string)))

(defun aes-fisher-yates-shuffle-array (s)
  "Shuffle array S randomly.
This is done destructively in S.  The result is returned."
  (let ((i (length s))
        j temp)
    (while (< 1 i)
      (aset s (setq j (random i))
            (prog1 (aref s (setq i (1- i))) (aset s i (aref s j))))))
  s)

(defcustom aes-user-interaction-entropy t
  "Query User for Entropy if non-nil.
If the value is non-nil, then the user must use mouse or key input to feed the
random number generator.
Otherwise use Emacs internal pseudo random number generator."
  :type 'boolean
  :group 'aes)

(defcustom aes-entropy-of-mousemovement 4
  "The bit-entropy of a mouse movement event."
  :type 'integer
  :group 'aes)

(defcustom aes-entropy-of-keyinput 2
  "The bit-entropy of a keyinput event."
  :type 'integer
  :group 'aes)

(defun aes-user-entropy (len &optional localmax)
  "Return a list of random numbers.
The length of the list is LEN and each integer in the list is in the range from
0 inclusive to LOCALMAX exclusive.
Read user entropy from keyboard and mouse to generate the random number
sequence, if `aes-user-interaction-entropy' is non-nil; otherwise use the
elisp function `random'.
Display an approximation of how much entropy is already generated.
Changing the window-size during the process will cause problems."
  (unless localmax (setq localmax 256))
  (if (not aes-user-interaction-entropy)
      (let ((res ()))
        (dotimes (i len) (setq res (cons (random localmax) res)))
        res)
    (let* ((chars
            "acbdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@+")
           (ch (format "%s%s%s"
                       (or (and (boundp 'recent-keys) recent-keys)
                           (recent-keys))
                       command-history
                       (current-time)))
           (chmd5b (md5 ch))
           (chmd5 (md5 (aes-fisher-yates-shuffle-array ch)))
           (tempentropy "")
           (tempentropybits 0)
           (preres "")
           (res ())
           curwin
           (iv (make-string 16 0))
           (key (make-string 16 0))
           (extract
            (lsh (aes-enlarge-to-multiple-num
                  (1+ (logb (or (and (= localmax 1) 1) (1- localmax)))) 8) -3))
           (maxfac (/ (expt 256 extract) localmax))
           (maxborder (* localmax maxfac))
           (needed-entropy-bits (aes-enlarge-to-multiple-num
                                 (* len extract 8 (/ (expt 256 extract)
                                                     (+ 0.0 maxborder)))
                                 128))
           (current-entropy-bits 0)
           (percentage-ready 0)
           keys)
      ;; generate aes key and a random iv from previous input
      (dotimes (i 16)
        (aset key i (string-to-number
                     (format "%s" (substring chmd5b (* 2 i) (+ 2 (* 2 i)))) 16))
        (aset iv i (string-to-number
                    (format "%s" (substring chmd5 (* 2 i) (+ 2 (* 2 i)))) 16)))
      (setq key (aes-str-to-b key))
      (setq keys (aes-KeyExpansion key 4))
      ;; start the user input
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let* ((h (window-body-height))
               (w (window-width))
               (p (1+ (* h w)))
               (i 1))
          ;; fill window with random characters
          (while (< i p)
            (insert-char (aref chars (random 64)) 1)
            (if (and (< i (1- p)) (< 0 i) (= (% i w) 0)) (insert "\n"))
            (setq i (1+ i)))
          ;; display buffer
          (switch-to-buffer (current-buffer))
          (setq curwin (selected-window))
          ;; get entropy and do the calculations
          (while (< (length res) len)
            (setq percentage-ready (/ (* current-entropy-bits 100.0)
                                      needed-entropy-bits))
            (let ((eve (track-mouse
                         (read-event
                          (format (concat "Move mouse or "
                                          "press keys as random input "
                                          "(C-g to abort) (about %2.2f%%):" )
                                  percentage-ready)))))
              (cond ((numberp eve)
                     (setq tempentropybits (+ aes-entropy-of-keyinput
                                              tempentropybits))
                     (setq current-entropy-bits
                           (+ aes-entropy-of-keyinput current-entropy-bits))
                     (setq tempentropy (format "%s%d" tempentropy eve)))
                    ((and (consp eve)
                          (eq 'mouse-movement (car eve))
                          (eq curwin (car (car (cdr eve))))
                          (numberp (nth 1 (car (cdr eve)))))
                     (setq tempentropy
                           (format "%s(%x,%x,%s)"
                                   tempentropy
                                   (car (nth 2 (car (cdr eve))))
                                   (cdr (nth 2 (car (cdr eve))))
                                   (buffer-substring-no-properties
                                    (nth 1 (car (cdr eve)))
                                    (1+ (nth 1 (car (cdr eve)))))))
                     (setq tempentropybits (+ aes-entropy-of-mousemovement
                                              tempentropybits))
                     (setq current-entropy-bits
                           (+ aes-entropy-of-mousemovement
                              current-entropy-bits))))
              (if (<= 128 tempentropybits)
                  ;; now there is enough entropy to gen a random 16 byte string
                  (progn
                    (setq preres
                          (concat preres
                                  (substring
                                   (aes-cbc-encrypt tempentropy iv keys 4)
                                   -16)))
                    (while (and (<= extract (length preres))
                                (< (length res) len))
                      (let ((nu 0))
                        (dotimes (i extract)
                          (setq nu (logxor (lsh nu 8) (aref preres i))))
                        (if (< nu maxborder)
                            (setq res (cons (/ nu maxfac) res))))
                      (setq preres (substring preres extract)))
                    (if (<= needed-entropy-bits current-entropy-bits)
                        (setq needed-entropy-bits (+ 128 needed-entropy-bits)))
                    (setq tempentropybits 0)
                    (setq tempentropy ""))))))
        res))))

(defun aes-generate-password (length &optional typ)
  "Return a password of length LENGTH.
TYP is a string consisting only of a subset of the characters defined in
the car values of `aes-password-char-groups'."
  (let* ((cs (mapcar 'car aes-password-char-groups))
         (case-fold-search nil)
         (chars
          (let ((res ""))
            (dolist (c cs)
              (setq
               res
               (concat
                res
                (if typ
                    (and (string-match (regexp-quote (char-to-string c)) typ)
                         (elt (assoc c aes-password-char-groups) 2))
                  (or (and (cadr (assoc c aes-password-char-groups))
                           (elt (assoc c aes-password-char-groups) 2))
                      "")))))
            res))
         (clen (length chars))
         (random (aes-user-entropy length clen))
         (res (make-string length 0)))
    (dotimes (i (length res))
      (aset res i (aref chars (car random)))
      (setq random (cdr random)))
    res))

(defun aes-insert-password (length)
  "Insert a newly generated password at point.
LENGTH denotes the length of the password.  The used characters are defined
in the variable `aes-password-char-groups'.  Use mouse movement and user input
as input for the pseudo randon number generator, if
`aes-user-interaction-entropy' is non-nil."
  (interactive "NLength of password: ")
  (insert (aes-generate-password length)))

;;;; buffer and string en-/decryption

(defun aes-toggle-representation (s)
  "Toggle string S between unibyte and multibyte.
Return a new string containing the other representation."
  (let ((mb (multibyte-string-p s)))
    (with-temp-buffer
      (if (not mb) (set-buffer-multibyte nil))
      (insert s)
      (set-buffer-multibyte (not mb))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defcustom aes-discard-undo-after-encryption t
  "Delete undo information after encryption, if non-nil.
If this is nil, then one can decrypt the buffer using the Emacs undo facility."
  :type 'boolean
  :group 'aes)

(defcustom aes-default-method "OCB"
  "Default encryption method.
Valid are: OCB and CBC.
OCB is Offset Codebook Mode (encryption with hashing).
CBC is Cipher-block chaining (encryption)."
  :type '(choice (const "OCB")
                 (const "CBC"))
  :group 'aes)

(defcustom aes-Nb 4
  "Default Nb value used.
4, 6 and 8 are valid values.
For OCB only 4 is supported."
  :type 'integer
  :group 'aes)

(defcustom aes-Nk 4
  "Default Nk value used.
4, 6 and 8 are valid values."
  :type 'integer
  :group 'aes)

(defun aes-encrypt-buffer-or-string (bos &optional type Nk Nb nonb64)
  "Encrypt buffer or string BOS (V 1.2).
If BOS is a string matching the name of a buffer, then this buffer is used.
Use method TYPE.  (\"OCB\" or \"CBC\"), If it is not specified, then decide
according to `aes-default-method'.
Use NK as keysize. If it is nil, then use the value of `aes-Nk'.
Use NB as blocksite If it is nil, then use the value of `aes-Nb'.
Use base64-encoding if NONB64 is nil, and binary representation otherwise. It
has a default value of nil.
Generate a weak random initialization vector.
Get the key for encryption from the function `aes-key-from-passwd'.
Return t, if a buffer was encrypted and otherwise the encrypted string."
  (unless Nb (setq Nb aes-Nb))
  (unless Nk (setq Nk aes-Nk))
  (let* ((buffer (or (get-buffer bos) (and (bufferp bos) bos)))
         (length (if buffer (with-current-buffer buffer (point-max))
                   (length bos))))
    (if (not (or (and (not type) (setq type aes-default-method))
                 (member type '("OCB" "CBC"))))
        (message "Wrong type.")
      (let* ((group (or (and buffer (or (aes-exec-passws-hooks
                                         (buffer-file-name buffer))
                                        (buffer-name buffer)))
                        "string"))
             (key (aes-str-to-b (aes-key-from-passwd Nk "encryption" group)))
             (keys (aes-KeyExpansion key Nb))
             (iv (let* ((x (make-string (lsh Nb 2) 0))
                        (aes-user-interaction-entropy nil)
                        (y (aes-user-entropy (lsh Nb 2) 256)))
                   (dotimes (i (lsh Nb 2)) (aset x i (car y)) (setq y (cdr y)))
                   x))
             (multibyte
              (if buffer (if (with-current-buffer buffer
                               enable-multibyte-characters)
                             "M" "U")
                (if (multibyte-string-p bos) "M" "U")))
             (unibyte-string
              (if buffer
                  (with-current-buffer buffer
                    (if (equal multibyte "M") (set-buffer-multibyte nil))
                    (buffer-substring-no-properties (point-min) (point-max)))
                (if (equal multibyte "M") (aes-toggle-representation bos) bos)))
             (header (format "aes-encrypted V 1.2-%s-%s-%d-%d-%s\n"
                             type (if nonb64 "N" "B") Nb Nk multibyte))
             (plain (if (equal type "OCB") unibyte-string
                      (concat (number-to-string (length unibyte-string))
                              "\n" unibyte-string)))
             (enc (if (equal type "OCB")
                      (let* ((res (aes-ocb-encrypt header plain iv keys Nb)))
                        (concat iv (cdr res) (car res)))
                    (concat iv (aes-cbc-encrypt plain iv keys Nb)))))
        (if nonb64 nil
          (setq enc (base64-encode-string enc)))
        (setq enc (concat header enc))
        (if buffer (with-current-buffer buffer
                     (erase-buffer)
                     (insert enc)
                     (if aes-discard-undo-after-encryption
                         (setq buffer-undo-list))
                     t)
          enc)))))

(defun aes-decrypt-buffer-or-string (bos)
  "Decrypt buffer or string BOS (V 1.2).
BOS is a buffer, a buffer name or a string.
If BOS is a string matching the name of a buffer, then this buffer is used.
Get the key for encryption by the function `aes-key-from-passwd'.
Return t, if a buffer was decrypted and otherwise the decrypted string."
  (let* ((buffer (or (and (bufferp bos) bos) (get-buffer bos)))
         (sp (if buffer (with-current-buffer bos
                          (buffer-substring-no-properties
                           (point-min) (point-max)))
               bos)))
    (and
     (or (string-match
          (concat
           "aes-encrypted V 1.2-\\(CBC\\|OCB\\)-\\([BN]\\)-"
           "\\([0-9]+\\)-\\([0-9]+\\)-\\([MU]\\)\n") sp)
         (and (message "buffer or string '%s' is not properly encrypted." bos)
              nil))
     (let* ((type (match-string 1 sp))
            (b64 (equal "B" (match-string 2 sp)))
            (Nb (string-to-number (match-string 3 sp)))
            (blocksize (lsh Nb 2))
            (Nk (string-to-number (match-string 4 sp)))
            (Nr (+ (max Nk Nb) 6))
            (um (match-string 5 sp))
            (multibyte (equal "M" (match-string 5 sp)))
            (header (match-string 0 sp))
            (res1 (substring sp (match-end 0)))
            (res2 (if b64 (base64-decode-string res1) res1))
            (iv (substring res2 0 blocksize))
            (enc-offset (cond ((equal type "CBC") blocksize)
                              ((equal type "OCB") (lsh blocksize 1))))
            (tag (substring res2 blocksize enc-offset))
            (enc (substring res2 enc-offset))
            (group (or (and buffer (or (aes-exec-passws-hooks
                                        (buffer-file-name buffer))
                                       (buffer-name buffer)))
                       "string"))
            (key (aes-str-to-b (aes-key-from-passwd Nk "decryption" group)))
            (keys (aes-KeyExpansion key Nb))
            (res (if (equal type "CBC")
                     (aes-cbc-decrypt enc iv (nreverse keys) Nb)
                   (aes-ocb-decrypt header enc tag iv keys Nb)))
            len)
       (if (or (and (equal type "CBC")
                    (not (string-match "\\`\\([0-9]+\\)\n" res)))
               (and (equal type "OCB") (not res)))
           (message (concat "buffer or string '"
                            (if (bufferp bos) (buffer-name bos) bos)
                            "' could not be decrypted."))
         (setq len (and (equal type "CBC")
                        (string-to-number (match-string 1 res))))
         (setq res (if (equal type "OCB") res
                     (substring res (match-end 0) (+ (match-end 0) len))))
         (if buffer (with-current-buffer bos
                      (erase-buffer) (set-buffer-multibyte nil)
                      (insert res)
                      (if multibyte (set-buffer-multibyte t))
                      (setq buffer-file-coding-system
                            (car (find-coding-systems-region
                                  (point-min) (point-max))))
                      t)
           (if multibyte (aes-toggle-representation res) res)))))))

(defun aes-is-encrypted ()
  "Return t, if the current buffer is aes-encrypted.
The test is done by looking at the first line of the buffer."
  (save-excursion
    (goto-char (point-min))
    (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n")))

(defun aes-encrypt-current-buffer-check ()
  "Encrypt current buffer, if it is not encrypted.
Return nil."
  (if (not (aes-is-encrypted))
      (progn
        (aes-encrypt-buffer-or-string (current-buffer))
        nil)))

(defun aes-encrypt-current-buffer ()
  "Encrypt current buffer."
  (interactive)
  (aes-encrypt-buffer-or-string (current-buffer)))

(defun aes-decrypt-current-buffer ()
  "Decrypt current buffer."
  (interactive)
  (aes-decrypt-buffer-or-string (current-buffer)))

(defun aes-toggle-encryption ()
  "Encrypt or decrypt current buffer.  Set according saving hook.
Based on the function `aes-is-encrypted' it is decided if the buffer should be
encrypted or decrypted.
Preserve modification status of buffer during decryption."
  (interactive)
  (let ((p (point)))
    (if (aes-is-encrypted)
        (let ((mod-flag (buffer-modified-p)))
          (aes-decrypt-buffer-or-string (current-buffer))
          (set-buffer-modified-p mod-flag)
          (add-hook (if (<= emacs-major-version 21)
                        'local-write-file-hooks
                      'write-file-functions)
                    'aes-encrypt-current-buffer-check nil t))
      (aes-encrypt-buffer-or-string (current-buffer)))
    (goto-char p)))

(defun aes-remove-encryption-hook ()
  "Remove saving-hook from current buffer.
This allows saving a previously encrypted buffer in plaintext."
  (interactive)
  (remove-hook (if (<= emacs-major-version 21)
                   'local-write-file-hooks
                 'write-file-functions)
               'aes-encrypt-current-buffer-check t)
  (message "Encryption Hook removed."))

(defun aes-auto-decrypt (&rest x)
  "Function for auto decryption used in `format-alist'.
WARNING: not compliant to `format-alist' in the sense that the function
decrypts the whole file and not just the region indicated in X."
  (if (aes-is-encrypted)
      (let ((mod-flag (buffer-modified-p)))
        (aes-decrypt-buffer-or-string (current-buffer))
        (set-buffer-modified-p mod-flag)
        (if (<= emacs-major-version 21)
            (add-hook 'local-write-file-hooks
                      'aes-encrypt-current-buffer-check nil t)
          (add-hook 'write-file-functions
                    'aes-encrypt-current-buffer-check nil t))))
  (goto-char (point-min))
  (point-max))

(defun aes-enable-auto-decryption ()
  "Enable auto decryption via `format-alist'."
  (if (assoc 'aes format-alist)
      (setq format-alist (assq-delete-all 'aes format-alist)))
  (setq format-alist
        (cons (list 'aes
                    "AES-encrypted format"
                    "aes-encrypted V [0-9]+.[0-9]+-.+\n"
                    'aes-auto-decrypt
                    nil
                    t
                    nil)
              format-alist)))

;;;; Provide

(provide 'aes)

;;;; Footer

;; Local Variables:
;; mode: outline-minor
;; comment-column:0
;; End:

;;; aes.el ends here

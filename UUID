;;; uuid.el

;; This file is free software

;; It is Copyright 2005 James Mastros, and may be used under your
;; choice of the GNU General Public License (version 2 or later), 
;; or the Artistic License, as promolgated by Perl.

(provide 'uuid)
(require 'cl-macs)

; Create, parse, and manipulate UUIDs/GUIDs, as specified in
; http://www.ietf.org/internet-drafts/draft-mealling-uuid-urn-05.txt

; Create a new UUID object, using version 4 (random).  (It's likely
; easiest to implement, plus it satisifies the privicy concerns of
; wackos.)

(defun uuid-create ()
  (let (
		; Note: Could use bit-vector under xemacs, but gnuemacs doesn't have it.
		(bits (apply 'vector (loop for i upto 127 collect (uuid-random-bit))))
		)
	; Version field: byte 7, bits 7-4
	(aset bits 60 0)
	(aset bits 61 1)
	(aset bits 62 0)
	(aset bits 63 0)
	; byte 9, bits 7-6
	(aset bits 69 0)
	(aset bits 70 1)
	; byte 10, bit 0
	(aset bits 71 1)
	bits
	)
)

(defun uuid-random-bit ()
  (logand (math-random-digits 1) 1)
)

(defun uuid-get-byte (uuid bytenum)
  (let* ((lsb (* bytenum 8))
		 (msb (+ lsb 7))
		 (val 0))
	(loop for bitnum from lsb to msb do
		 (let ((bit (aref uuid bitnum)))
			  (setq val (+ bit (lsh val 1)))
	     )
    )
	val
  )
)

(defun uuid-to-stringy (uuid)
  (format "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
		  ; time-low
		  (uuid-get-byte uuid 0)
		  (uuid-get-byte uuid 1)
		  (uuid-get-byte uuid 2)
		  (uuid-get-byte uuid 3)
		  ; time-mid
		  (uuid-get-byte uuid 4)
		  (uuid-get-byte uuid 5)
		  ; time-high-and-version
		  (uuid-get-byte uuid 6)
		  (uuid-get-byte uuid 7)
		  ; clock-seq-and-reserved
		  (uuid-get-byte uuid 9)
		  ; clock-seq-low
		  (uuid-get-byte uuid 8)
		  ; node
		  (uuid-get-byte uuid 10)
		  (uuid-get-byte uuid 11)
		  (uuid-get-byte uuid 12)
		  (uuid-get-byte uuid 13)
		  (uuid-get-byte uuid 14)
		  (uuid-get-byte uuid 15)
  )
)

(defun uuid-to-win32 (uuid)
			 (format "{%s}" (uuid-to-stringy uuid))
)

(defalias 'uuid-to-w32       'uuid-to-win32)
(defalias 'uuid-to-win-nt    'uuid-to-win32)
(defalias 'uuid-to-mswindows 'uuid-to-win32)


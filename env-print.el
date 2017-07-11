;;; env-print.el -- print an envelope

;; Author: Hans Halvorson (www.princeton.edu/~hhalvors)
;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation.

;; Some parts are based on Gregory Spath's "envelope" Perl script
;; (http://freefall.homeip.net/code/).

;;; Commentary:
;;
;; The function `env-print' takes a list of strings and prints a
;; postscript envelope through lpr-buffer.  It is likely that you will
;; have to tune the custom variables for your printer.
;;
;; The interactive function `env-from-bbdb' prints an envelope from
;; the bbdb record at the point.  It currently defaults to the first
;; address in the record, and it has only been tested for US
;; addresses.

;;; Code:

(defgroup env-print nil
  "Print envelopes."
  )

(defcustom env-lpr-buffer nil
  "Set to nil if your `lpr-buffer' function gives odd results,
  and you can call lpr directly from a shell.")

(defcustom env-normalpaper 11
 "Height of normal paper."
 :group 'env-print)

(defcustom env-ewidth 9.5
  "Envelope width"
  :group 'env-print)

(defcustom env-eheight 4.125
  "Envelope height"
  :group 'env-print)

(defcustom env-normalwidth 8.5
  "Normal paper width."
  :group 'env-print)

; (defcustom env-topmargin 0.30
;  "Top margin")

(setq env-topmargin (+ 0.30 (/ (- env-normalwidth env-eheight) 2)))

(defcustom env-leftmargin 0.25
  "Left margin"
  :group 'env-print)
  
(defcustom env-line1 '("Times-Roman" 12)
  "Default font for first line of mailing address. You must use valid PostScript font names and sizes."
  :group 'env-print)

(defcustom env-linex '("Times-Roman" 12)
  "Default font for mailing address, lines two and greater.  You must use valid PostScript font names and sizes."
  :group 'env-print)


(defun env-print (address)
 ; usage:  (env-print '("Grandy Halvorson" "2817 High Ridge Rd" "Charlotte NC 28226"))
 ; the ADDRESS should be a list of strings, where each string
 ; corresponds to a line in the printed representation
  "Print an envelope."
  (with-temp-buffer
  ;; create the PostScript buffer  
  (insert "%!\n% PS file from env-print.el\nnewpath\n")
  (let* ((x-orig 0)
	 (y-orig (* 72 (- env-normalpaper env-ewidth)))
	 (var-i (floor (+ x-orig (* env-topmargin 72))))
	 (var-j (floor (+ y-orig (* env-leftmargin 72))))
	 (line1pointsize (car (cdr env-line1)))
	 (linexpointsize (car (cdr env-linex)))
	 (line1font (car env-line1))
	 (linexfont (car env-linex))
	 (x-pos (floor (* 0.40 72 env-ewidth)))
	 (y-pos (floor (* -0.40 72 env-eheight))))
    ; set coordinate system
    (insert (format "%d %d translate\n90 rotate\n" var-i var-j))
    ; PS code for the first line of the address
    (insert (format "/%s findfont %d scalefont setfont\n" line1font line1pointsize))
    (insert (format "%d %d moveto\n" x-pos y-pos))
    (insert (format "(%s) show\n" (car address)))
    ; move y-pos down the dimension of line1pointsize
    (setq y-pos (- y-pos line1pointsize))
    ; PS code for subsequent lines of the address
    (let ((mod-address (cdr address)))
      (dolist (z mod-address)
	(insert (format "/%s findfont %d scalefont setfont\n" linexfont linexpointsize))
	(insert (format "%d %d moveto\n" x-pos y-pos))
	(insert (format "(%s) show\n" z))
	(setq y-pos (- y-pos linexpointsize)))))
  ; PS postamble
  (insert "showpage\n")
  ;; send the buffer to the printer
  ;; most natural to use `lpr-buffer', but that gives strange results on my machine
  (if env-lpr-buffer
      (lpr-buffer)
    (shell-command-on-region (point-min) (point-max) "lpr"))))


;;+ Print from bbdb record

;; right now this function picks off the first address element -- see `car' in defn of addr2
;; TO DO: give choice of which address to print

(defun env-from-bbdb ()
  (interactive)
  (let* ((addr1 (bbdb-record-get-field-internal (bbdb-current-record) 'name)) ; string
	 (addr0 (bbdb-record-get-field-internal (bbdb-current-record) 'address)) ; list of vectors
	 (addr2 (car addr0)) ; vector
         (addr3 (aref addr2 1)) ; list of address strings	 
	 (city  (aref addr2 2)) ; string
	 (state (aref addr2 3)) ; string
	 (zip   (aref addr2 4)) ; string
	 (addr-last (list (concat city " " state "  " zip)))) ;; list
    (env-print (cons addr1 (append addr3 addr-last)))))


;; TO DO:
;; 1. pipe in from a text buffer
;; 2. pipe in from region
;; 3. return address
;; 4. defcustom orientation

(provide 'env-print)

;;; end of file env-print.el

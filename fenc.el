;;; fenc.el --- Detect CJK coding system by counting most frequence charaters

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Time-stamp: <Ye Wenbin 2006-11-27 22:48:01>
;; Version: $Id: fenc.el,v 1.2 2006/11/27 13:05:31 ywb Exp ywb $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'fenc)

;;; Code:

(provide 'fenc)
(eval-when-compile
  (require 'cl))

;;{{{  top chars
(defvar fenc-cp936-top-chars
  '((#x9c #xca)
    (#xb0 #xd1 #xd9 #xd7 #xb2 #xcb #xeb #xec #xe3 #xb4 #xfc)
    (#xb1 #xbe #xed #xc8 #xe4 #xdf #xf0 #xbb #xd8 #xa3 #xb1 #xa8 #xb8 #xe3 #xea)
    (#xb2 #xbb #xfa #xbf #xc5 #xc9 #xbd #xe9 #xe3 #xce)
    (#xb3 #xf6 #xc9 #xa4 #xcc #xa3 #xa1 #xb5 #xd6 #xc6 #xfd #xa7)
    (#xb4 #xf3 #xd3 #xcb #xfa #xce #xa6 #xf2 #xf8 #xef #xab #xe6)
    (#xb5 #xc4 #xd8 #xbd #xc3 #xc8 #xe7 #xb1 #xe3 #xab #xda #xc0 #xb3 #xf7 #xbc #xa5 #xcd #xd7)
    (#xb6 #xd4 #xaf #xf8 #xe0 #xa8 #xc8 #xbc #xfe #xd3 #xab #xce #xf9 #xcf)
    (#xb7 #xd6 #xa2 #xbd #xa8 #xb4 #xc5 #xe7 #xc7)
    (#xb8 #xf6 #xdf #xf7 #xef #xf9 #xf8 #xc9 #xc4 #xfc #xd0 #xae #xf1 #xc3)
    (#xb9 #xfa #xa4 #xfd #xd8 #xab #xfb #xdc #xe2 #xe6 #xb2 #xe3 #xdb #xb9)
    (#xba #xcd #xf3 #xc3 #xcf #xdc #xa3 #xce #xc5 #xf2 #xec)
    (#xbb #xe1 #xaf #xfa #xb9 #xf2 #xee #xf9 #xd8 #xa8 #xfd #xaa #xb0 #xae #xf0)
    (#xbc #xd2 #xd3 #xe4 #xb0 #xfe #xb6 #xfb #xc6 #xb8 #xb4 #xc3 #xab #xba #xaf #xc7 #xca #xbc #xdb)
    (#xbd #xf8 #xe1 #xe2 #xa8 #xd3 #xcf #xab #xc7 #xf0 #xd7 #xbb #xcc #xda #xf1 #xe7 #xd0 #xfc #xad)
    (#xbe #xcd #xad #xfc #xc5 #xf6 #xdd #xdf #xbf #xab #xa9 #xd6)
    (#xbf #xc9 #xaa #xb4 #xda #xc6 #xd5 #xf6 #xf3 #xcb #xec)
    (#xc0 #xb4 #xed #xef #xfb #xcf #xfd #xe0 #xeb #xad #xcd #xfa)
    (#xc1 #xcb #xa6 #xbf #xbd #xa2 #xcf #xf7 #xec #xaa #xf9 #xd6 #xac #xd0)
    (#xc2 #xb7 #xdb #xed #xc9 #xca #xfa)
    (#xc3 #xc7 #xe6 #xf1 #xf7 #xb4 #xfc #xbb #xc5 #xc0 #xbf #xc5 #xfb #xd7 #xab)
    (#xc4 #xea #xdc #xc7 #xda #xe3 #xcf #xbf #xd1)
    (#xc5 #xa9 #xc9)
    (#xc6 #xf0 #xe4 #xbd #xf8 #xb7 #xda #xdf #xf7 #xac)
    (#xc7 #xb0 #xe9 #xf3 #xf8 #xbf #xe5 #xd0 #xd2 #xa7 #xe0 #xd7 #xe1)
    (#xc8 #xcb #xfd #xe7 #xa5 #xbb #xd5 #xab #xeb #xce #xa1 #xc8 #xcf #xa8 #xb7 #xba #xdd #xb4)
    (#xc9 #xcf #xfa #xe7 #xe8 #xd9 #xbd #xab #xed #xf9 #xcc #xee #xf5)
    (#xca #xc7 #xb1 #xae #xb5 #xb9 #xc2 #xfd #xbd #xd6 #xc0 #xf5 #xdc #xd5 #xbe #xaf #xe9 #xa1 #xb7 #xd0 #xb2 #xbc #xb6 #xca #xf4 #xd7)
    (#xcb #xfb #xb5 #xf9 #xae #xfc #xc4 #xfd #xbc #xd9 #xe3 #xd8 #xe1 #xef #xb9)
    (#xcc #xe5 #xec #xf5 #xe1 #xe2 #xd8 #xab #xfd #xfa)
    (#xcd #xac #xe2 #xa8 #xb7 #xbc #xb3 #xf2 #xea #xc1 #xc5 #xf9 #xf5)
    (#xce #xaa #xd2 #xef #xca #xde #xe5 #xbb #xc4 #xf7 #xf1 #xc2 #xaf #xac)
    (#xcf #xc2 #xd6 #xe0 #xdf #xf2 #xeb #xf3 #xc8 #xb5 #xb0 #xec #xfb #xd8 #xb8)
    (#xd0 #xd0 #xa1 #xd4 #xa9 #xce #xc2 #xc4 #xc5 #xed #xe8 #xeb #xb4 #xa7 #xcd)
    (#xd1 #xa7 #xf9 #xb9 #xd0 #xe9 #xa1 #xdb #xf8 #xcf)
    (#xd2 #xbb #xd4 #xaa #xb2 #xb5 #xf2 #xe5 #xe2 #xd1 #xe9 #xf4 #xd7 #xfd #xba)
    (#xd3 #xd0 #xc3 #xda #xa6 #xc9 #xeb #xd6 #xcd #xb0 #xfd)
    (#xd4 #xda #xad #xc2 #xb1 #xcb #xf2 #xec #xf6 #xd9 #xaa #xbd #xbc #xba #xb2)
    (#xd5 #xe2 #xfe #xfd #xdf #xb9 #xbd #xf9 #xe6 #xc5 #xfb #xd5)
    (#xd6 #xd0 #xf7 #xd6 #xae #xd8 #xc6 #xca #xbb #xb1 #xaa #xb8 #xce #xa4 #xbe #xc1 #xaf #xda #xa7 #xb5 #xdc #xc3)
    (#xd7 #xf7 #xd3 #xd4 #xee #xdc #xca #xe9 #xf6 #xaa #xdf #xb0 #xe5 #xa8 #xb4 #xa1)))

(defvar fenc-cp950-top-chars
  '((#xa4 #x40 #xa3 #x46 #x48 #xa4 #x6a #x57 #xc0 #x75 #x5d #x55 #x6c #xe8 #x51 #x54 #xa7 #x4f #xf4 #xc6 #x47 #x70 #xd1 #xe9 #xba #xdf #xcf #x53 #xf1 #xeb #xbd #x77 #xad #x4a #xe5 #xce #xd6 #x73 #xe2 #x45 #x43 #x66 #xbb #x76 #xc1 #x4b #x7e #xb8 #xb5 #x67 #x64 #xe4 #xd3 #xb0 #xf2 #xfd #xf9 #xf5 #xde)
    (#xa5 #x48 #x4c #xce #xcd #x58 #x69 #x44 #xc1 #x5b #xbb #x68 #xa6 #xd1 #x7e #x7c #xad #xfe #xbf #x75 #xdf #x4e #xf3 #xb2 #xfd #xf4 #xfa #x5f #x40 #xe6 #xb4 #xd5 #xd8 #xdc #xdb #x42 #x73 #x62 #x76 #xab #x5d)
    (#xa6 #x62 #xb3 #x61 #xa8 #x7e #x50 #xe6 #xd3 #x68 #x70 #xdb #x6e #x58 #x5d #x55 #xfd #x56 #xb9 #xec #xb8 #xa1 #xd1 #x6f #xe8 #x5e #xe2 #xca #x40 #xac #x41 #x77 #xdc #x57 #xcc #x43 #xf3 #x73 #xbf #xed)
    (#xa7 #xda #x40 #xe2 #xce #x41 #x51 #x4f #x59 #xef #xd3 #xf3 #xb9 #x43 #xde #x4a #xd6 #xbd)
    (#xa8 #xd3 #xec #xe2 #xcf #xee #xe4 #xc7 #xba #xc6 #x53 #x44 #xa3 #xa4 #x4d #xfa #xfc #xad #xae #xd2 #xe3 #x43 #xab #x42 #x73 #x74 #xe0 #x7c #xca)
    (#xa9 #x4d #xf3 #x77 #xd2 #xca #xfa #xce #x52 #xf1 #xd4 #x65 #xb9 #xb2 #x50 #xf6 #x6c #xb3)
    (#xaa #xba #x6b #xf8 #xab #xc0 #xed #xcc #xbd #x47 #xbe #xf9 #x76 #x46 #xf7 #xa7 #x6f #xe1 #xc5 #x70 #x4c #xf1 #xac)
    (#xab #xe1 #xd7 #x65 #xd8 #xdc #x7e #xfc #x68 #x6e #x4f #xe4 #x48 #xdf #xf9 #x44 #x43 #x4b #x59 #xac #xf6 #x6f)
    (#xac #x4f #xb0 #x46 #xdb #xdd #xa1 #x79 #xfc #xec #x71 #xe3 #xc9 #xd9 #xf9 #xc6 #x64 #xa3 #xf5)
    (#xad #xd3 #x6e #xcc #xb1 #xab #xec #x78 #xfb #xb2 #x70 #xb7 #xb5 #xc8 #xd4 #xba)
    (#xae #xc9 #x61 #xf0 #x69 #xc6 #xda #xfc #xd1 #x65 #xf8 #xc4 #xe6)
    (#xaf #xe0 #x53 #xc5 #x75 #xc0 #xeb)
    (#xb0 #xea #xca #xaa #x5f #xdd #xf2 #xcf #xb5 #xc8 #xab #xa8 #x4f #xd3 #xa3 #x7c #xd1)
    (#xb1 #x6f #x71 #xf8 #xa1 #x60 #xb5 #x4e #x6a #xd0 #x61 #x69 #xc4 #xda #x4d)
    (#xb2 #xa3 #x7a #x7b #xc4 #xce #xd5 #x4d #xb3 #x76 #x60 #xdf #xb4 #xd3 #x47)
    (#xb3 #x6f #xa1 #xa3 #xcc #x71 #x5d #x51 #x42 #x79 #x57 #x4e #xe6 #x74 #xf5 #xf8 #x5c #x73 #xc6 #xd2)
    (#xb4 #x4e #xa3 #xc1 #x58 #xb5)
    (#xb5 #x6f #xa5 #x4d #xb2 #x4c #x7b #xb9 #xd8)
    (#xb6 #x69 #x71 #x7d #xa1 #x48 #xa4 #xa5 #xc7 #xb0 #x56 #xb7 #xea)
    (#xb7 #x7c #xed #x7e #x73 #x4e #x51 #xa5 #x46 #xc5 #x50 #xd3 #xc7)
    (#xb8 #x67 #x71 #xd1 #xea #xf4 #xfb #x55 #x60 #xdc #x73 #xcb #xb9 #x6d #xd3)
    (#xb9 #xef #x4c #x71 #xea #x44 #xcf #x42 #x41 #x46 #xce #xba)
    (#xba #xd8 #xde #xe2 #xd9 #x63 #xeb #xfb #xa1)
    (#xbb #xa1 #x50 #xf2 #xe2 #x7b #xdd #xda #xc4 #xb4 #xf9)
    (#xbc #xcb #xc6 #xf6 #x57 #x73 #x76 #xd0 #x67 #x68 #x74)
    (#xbd #x75 #xe8 #xd7 #xd5 #x54)
    (#xbe #xc7 #xf7 #xd4 #xda #xc9 #xb9 #xe3 #xfa #x69 #x41)
    (#xbf #x6e #xec #xef #xcb #xa4)
    (#xc0 #xb3 #x59 #xa3 #xd9)
    (#xc1 #xd9 #x60 #x70 #x6e)
    (#xc2 #x49 #xe0 #xf7 #xb4 #x5f)
    (#xc3 #xf6 #x44 #xe4 #xd2 #xfe #xf8 #xd1)
    (#xc4 #xd2 #xb3 #x71 #xdd #x59)
    (#xc5 #xe9 #xdc #x76 #xe7 #x54 #xa5 #x4b)
    (#xc6 #x5b)
    (#xf9 #xd8)))

(defvar fenc-cp932-top-chars
  '((#x81 #x5b #x58)
    (#x82 #xb5 #xf0 #xcc #xb7 #xdc #xc9 #xe9 #xc6 #xcd #xc5 #xa4 #xc4 #xa2 #xe5 #xb1 #xbd #xea #xaa #xc8 #xe8 #xb3 #xe7 #xa9 #xe0 #xab #xe6 #xc1 #xad #xc2 #xdd #xbb #xaf #xa6 #xd6 #xa0 #xdf #xce #xed #xf1 #xbe #xb9 #xe2 #xb6 #xc7 #xb8 #xa8 #xde #x9f #xe4 #xcb #xbf #xeb #xd9 #xd7 #xd1 #xc3 #xd4 #xbc #xb0 #xac #xda #xb2)
    (#x83 #x93 #x8b #x43 #x76 #x5e #x58 #x68 #x62 #x4a #x8c #x7d #x5c #x52 #x67 #x74 #x65 #x40 #x4c #x82 #x56 #x49 #x87 #x8a #x79 #x45 #x42 #x4e #x41 #x66 #x85 #x60 #x77 #x6d #x57 #x8d #x89 #x73 #x47 #x46 #x6f #x80 #x72 #x69 #x83 #x7e #x75 #x5b #x59 #x55 #x4f #x86 #x84 #x71 #x70 #x6a #x54)
    (#x88 #xda #xc8 #xca #xe1 #xea #xd7 #xd3 #xcd #xd9 #xf8 #xf3 #xcb)
    (#x89 #xba #x9f #xc1 #xbd #xf1 #xe6 #xf0 #x9e #x45 #xc2 #xd3 #xf5 #xaa)
    (#x8a #xd4 #xb7 #x6d #xae #x4a #x6f #xf9 #x77 #x4f #x87 #xdc #xb5 #xf4 #xc8 #xfc #x69 #xc4 #xc3 #x54 #x79 #xfa #xf6 #xb4 #xee)
    (#x8b #xe5 #x4e #xad #x74 #xf3 #xc6 #xe6 #x43 #xf7 #x4d #x81 #x40 #xb3 #x93 #x79 #xa4)
    (#x8c #xea #x9f #xe3 #x4a #xa9 #xfc #xbb #xca #x60 #x88 #xf8 #xb3 #xaf #x79 #xeb #x76 #xbe #x66 #x62 #xc4 #xc3)
    (#x8d #x73 #xed #x58 #xf5 #xc5 #x9e #xec #x87 #xdb #xc4 #xdd #x86 #x7e #xb6 #x44 #x80 #xda #xd7 #xbb #x5c #x4f #x4c #x90 #x8f #xa1)
    (#x8e #x9a #xa6 #x67 #x9f #xc0 #xe6 #x6e #x9e #x8e #xa9 #x63 #x77 #xae #xd0 #x9d #x51 #xd4 #x8b #xd2 #x86 #x84 #xe8 #x76 #x97 #x64)
    (#x8f #x9c #x49 #x89 #xc1 #xe3 #xea #x91 #xac #x6f #x57 #x43 #x8a #x4b #xf3 #xee #xcd #xc6 #x87 #x71 #xda #x83 #xbc #xed #xa7 #x5c #x5b)
    (#x90 #xb3 #x94 #xe6 #x69 #xdd #xac #xe0 #x6c #xc2 #xd4 #x84 #x53 #x4d #x67 #x65 #xbb #x46 #x7d)
    (#x91 #x53 #x7d #x4f #xb6 #xe5 #xbd #xcc #xce #x49 #xf0 #xb1 #xbc #xd4 #x80 #xe8 #xab #x45 #x67 #xe6 #xac #xd7 #xba #xc5 #xbe #xe3)
    (#x92 #x75 #x50 #xc7 #xe8 #x6c #xb2 #x5b #xbc #x86 #x6d #x8d #xa5 #xca #xb7 #x42 #x4e #x76 #xf6 #x5a #x69 #x51)
    (#x93 #xae #xfc #xaa #x78 #xc7 #xaf #xc1 #xe0 #x5c #xbe #xad #x4b #x72 #xa5 #x9c #x99 #x49 #x5f #xfa #x60)
    (#x94 #x46 #xd4 #xf6 #x5c #xcd #xc5 #x92 #xb2 #x5b #x6a #xf1 #x7a #xad #x4f #xbb #xf5)
    (#x95 #xb6 #xcf #xfb #xd4 #x94 #x5c #xdb #xe2 #xd2 #xaa #x4b #x74 #xca #xa1 #xf1 #x73 #xc2 #x70 #x84 #xd7 #xd6 #xb9)
    (#x96 #xbc #x96 #xda #xf1 #x40 #xdf #x7b #xca #xb3 #xbe #xee #x59 #xf0 #xf3 #xe2 #xa1 #x60 #x9c)
    (#x97 #x70 #x76 #x6c #xcd #xb9 #xe1 #x97 #x88 #x5e #x9d #xde #xfb #xa7 #xed #x8e #xc7 #xaa #x65 #x98 #xbc)
    (#x98 #x41 #x59)
    (#xe3 #x59)))
;;}}}

(defvar fenc-max-count 100
  "If the counter of certain coding system arrive this number, then
use this coding system.")

(defvar fenc-max-char/error 50
  "How many error may accept for a certain number of characters")

(defvar fenc-min-char/count 2
  "The minimum top chars in this number of characters")

(defvar fenc-coding-list
  '([cp936 fenc-cp936-top-chars 0]
    [cp950 fenc-cp950-top-chars 0]
    [cp932 fenc-cp932-top-chars 0]))

(defsubst fenc-coding-name (coding)
  (aref coding 0))

(defsubst fenc-table (coding)
  (aref coding 1))

(defsubst fenc-counter (coding) 
  (aref coding 2))

(defsubst fenc-set-counter (coding value)
  (aset coding 2 value))

(defvar fenc-coding-system nil)
(defvar fenc-error-counter nil)
(defvar fenc-char-counter nil)

(defsubst fenc-reset-vars ()
  (dolist (coding fenc-coding-list)
    (fenc-set-counter coding 0))
  (setq fenc-coding-system nil
        fenc-error-counter 0
        fenc-char-counter 0))

(defun fenc-lookup-char (table code1 code2)
  (let ((sec (assoc code1 table)))
    (and sec (member code2 (cdr sec)))))

(defun fenc-count-char (coding code1 code2)
  (let ((counter (fenc-counter coding)))
    (when (fenc-lookup-char (symbol-value (fenc-table coding)) code1 code2)
      (fenc-set-counter coding (1+ counter))
      (if (> counter fenc-max-count)
          (setq fenc-coding-system (fenc-coding-name coding))))))

(defun fenc-auto-coding-function (size)
  "Find a CJK coding system for a file in the first 1k and last 3k of
file."
  (let* ((head-start (point))
         (head-end (+ head-start (min size 1024)))
         (tail-start (+ head-start (max (- size 3072) 0)))
         (tail-end size)
         end code1 code2)
    (fenc-reset-vars)
    (dolist (region (list (cons head-start head-end)
                          (cons tail-start tail-end)))
      (goto-char (car region))
      (setq end (cdr region))
      (while (and (null fenc-coding-system)
                  (< (point) end))
        (setq code1 (char-after))
        (forward-char 1)
        (when (>= code1 #x80)
          (setq code2 (char-after))
          (forward-char 1)
          (if (< code2 #x40)
              (incf fenc-error-counter)
            (incf fenc-char-counter)
            (dolist (coding fenc-coding-list)
              (fenc-count-char coding code1 code2))))))
    ;; the number of right characters should
    ;; larger than (fenc-error-counter * fenc-max-char/error) and
    ;; less than (counter * fenc-min-char/count)
    (setq fenc-coding-system (car
                              (sort (copy-sequence fenc-coding-list)
                                    (lambda (c1 c2) (> (fenc-counter c1)
                                                       (fenc-counter c2))))))
    (when (and (> fenc-char-counter (* fenc-error-counter fenc-max-char/error))
               (< fenc-char-counter (* (fenc-counter fenc-coding-system)
                                       fenc-min-char/count)))
      (fenc-coding-name fenc-coding-system))))
(add-to-list 'auto-coding-functions 'fenc-auto-coding-function)

;;;###autoload
(defun fenc-dump-top-chars (coding table)
  (with-current-buffer (get-buffer-create "*chars*")
    (erase-buffer)
    (dolist (code table)
      (let ((first (car code)))
        (insert (decode-coding-string
                 (concat (apply 'append
                                (mapcar (lambda (sec)
                                          (list first sec))
                                        (cdr code))))
                 coding) "\n")))
    (display-buffer (current-buffer))))

;;; fenc.el ends here


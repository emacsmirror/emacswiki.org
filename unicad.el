;;; unicad.el --- an elisp port of Mozilla Universal Charset Auto Detector

;;;{{{  Copyright and License
;; Copyright (C) 2006, 2007, 2008, 2010 Qichen Huang
;; $Id$
;; Author: Qichen Huang <unicad.el@gmail.com>
;; Time-stamp: <2010-04-09 12:53:15>
;; Version: v1.1.5
;; Keywords: coding-system, auto-coding-functions
;; URL: http://code.google.com/p/unicad/

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
;;;}}}

;;;{{{ Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'unicad)
;;
;; You can disable unicad by M-x `unicad-disable' and enable it by M-x
;; `unicad-enable' or `unicad'.
;;
;; It may take a few seconds to detect some large latin-1 or latin-2 files,
;; you can byte-compile this file to speed up detecting process.

;; Following coding systems can be auto detected:
;;  * (BOM detect)
;;  * (ascii)
;;  * multibyte coding systems:
;;    - gb18030 (gb2312)
;;    - big5
;;    - sjis
;;    - euc-jp
;;    - euc-kr
;;    - euc-tw
;;    - utf-8
;;    - utf-16le (also without signature)
;;    - utf-16be (also without signature)
;;  * singlebyte coding systems:
;;    - greek
;;      > iso-8859-7
;;      > windows-1253
;;    - russian
;;      >koi8-r
;;      >windows-1251
;;      >iso-8859-5
;;      >ibm855
;;  - bulgarian
;;      >iso-8859-5
;;      >windows-1251
;;  - sjis (singlebyte only)
;;  * latin-1
;;  * latin-2

;;; TODO:
;; Endline pattern
;; Follows are planned to be involved in next version:
;;  - iso-8859-9 (turkish, latin-5)
;;  - hebrew
;;  - thai
;;  - x-mac-cyrillic
;;  - ibm866
;;  - iso-2022 (Emacs itself can detect iso-2022-cn -jp and -kr corretly)
;;  - hz
;; Local Variables

;;; KNOWN BUGS:
;; - Conflict with ido when exit Emacs. Add `unicad-disable' to `kill-emacs-hook' to avoid this problem.
;; - Some files are too short to be detected.
;; - Some don't contain the most frequently characters, so that they can't be detected.
;; - If a japanese text file is encoded with gb18030, it's very hard to be detected.
;; - GBK characters like "毛(#xC3AB), 狮(#xCAA8)" are similar to utf-8, could be incorrectly detected.
;;   So I made the priority of gbk and other charsets higher than utf-8.
;; - Detecting traditional chinese encoded in gbk is not supported in Emacs 22.1 and below,
;;   because the function `decode-char' only support `ucs'.
;;
;; *** If you find undetected files, please send me a bug report and attach the files. Many thanks. ***

;;; NEED HELP:
;; If some one can provide a language model of japanese katakana (for
;; detecting sjis halfwidth katakana only files), please contact me.
;; Thanks!

;; This is an elisp port of Universal Charset Detector. However, some
;; small changes have been made to make the dectection more
;; comfortable and quickly in Emacs. For more information about
;; Mozilla Universal Charset Detector, please refer to:
;; http://www.mozilla.org/projects/intl/ChardetInterface.htm
;;;}}}

;;;{{{ Changelog
;; v1.1.5 fixed bug in `unicad-char-after' and `unicad-universal-charset-detect' (size calc was wrong)
;; v1.1.4 Add function and variable `unicad-version'. small bug fix, correct unicad-eol from unicad-eof.
;; v1.1.3 detect dos eol-type
;; v1.1.2 (add-to-hook 'kill-emacs-hook 'unicad-disable) to avoid conflict with ido, session, etc.
;; v1.1.1 Fixed a bug in `unicad-sjis-sb-prober', added a char2order 256 for impossible char.
;; v1.1.0 Added interactive functions `unicad-enable', `unicad-disable' and `unicad'.
;;        Add support for compressed files.
;; v1.0.6 Fixed a bug in `unicad-gbkcht-analyser' for some incompatitable reason.
;; v1.0.5 Changed define order to eliminate byte-compile warnings.
;; v1.0.4 remove simplified chinese in big5, it's nonsense.
;; v1.0.3 fixed charset names of utf-16le and utf-16be (without signature),
;;        minor changes for `unicad-ucs2le-prober' and `unicad-ucs2be-prober'
;; v1.0.2 changed the sequence of `unicad-multibyte-group-list',
;;        fixed a bug in `unicad-dist-table-get-confidence'
;; v1.0.1 add support for simplified chinese encoded in big5
;; v1.0.0 minor changes, just some tidy works
;; v0.65 fixed a bug in `unicad-gbkcht-analyser'
;; v0.64 add support for traditional chinese encoded in gbk
;; v0.63 changed the threhold in `unicad-dist-table-get-confidence'
;; v0.62 allow esc(0x1B) as legal value in utf-8, gb18030, sjis, big5, euckr
;; v0.61 add a `unicad-quick-multibyte-words', increase `unicad-quick-size' to 500
;;       add support for sjis single byte only katakana files
;; v0.60 delete singlebyte-model, replace it with more reasonable variables
;; v0.52 change singlebyte-dist-table to vectors, and some other changes
;; v0.50 add support for bulgarian and latin2, change prefix to unicad-*
;; v0.40 add support for single byte coding systems
;; v0.30 change variable and function names to ucad-* prefix
;; v0.22 add BOM detector
;; v0.21 use vectors instead of class bits as it was in original Mozilla cpp code
;;}}}

;;; Code:

;;{{{  define variable

;;(provide 'unicad)
(eval-when-compile
  (require 'cl))

(defvar unicad-version "Unicad v1.1.5")
(defvar unicad-global-enable t)
(defvar unicad-eol nil)
(defvar unicad-quick-size 500)
(defvar unicad-quick-multibyte-words  50)
(defvar unicad-quick-singlebyte-words 50)
(defvar unicad-max-size 10000)
(defvar unicad-default-coding-system 'nil)

(defvar unicad-threshold 0.95)
(defvar unicad-data-threshold 1024)
(defvar unicad-minimum-data-threshold 1)
(defvar unicad-minimum-size-threshold 10)

(defvar unicad-cjk-prefer nil
  "set preference encoding system (only for chinese, japanese, korean coding systems.)")

(defconst unicad--sure-yes 0.99)
(defconst unicad--sure-no 0.01)

(defvar unicad-best-guess nil
  "(MACHINE-BEST-GUESS BEST-CONFIDENCE)")

;; (make-variable-buffer-local 'unicad-best-guess)

(defvar unicad-singlebyte-best-guess nil)

(defvar unicad-singlebyte-group-guess nil)

(defvar unicad-latin-best-guess '(latin-1 0.0))

(defvar unicad-latin1-guess
  '(latin-1 0.0))

(defvar unicad-latin2-guess
  '(latin-2 0.0))

(defvar unicad-chosen-gb-coding-system
  (cond
   ((coding-system-p 'gb18030)
    'gb18030)
   ((coding-system-p 'gbk)
    'gbk)
   (t 'gb2312)))

(defvar unicad-multibyte-group-list
  `([sjis 0 unicad-sjis-prober]
    [euc-jp 0 unicad-eucjp-prober]
    [,unicad-chosen-gb-coding-system 0 unicad-gb2312-prober]
    [euc-kr 0 unicad-euckr-prober]
    [big5 0 unicad-big5-prober]
    [euc-tw 0 unicad-euctw-prober]
    [,unicad-chosen-gb-coding-system 0 unicad-gbkcht-prober]
    [utf-8 0 unicad-utf8-prober]
    [utf-16le 0 unicad-ucs2le-prober]
    [utf-16be 0 unicad-ucs2be-prober])
  "([CODING-SYSTEM BEST-CONFIDENCE PROBER-FUNCTION] ...)")

;;}}}

;;{{{  unicad-enable, unicad-disable
(defun unicad-version (&optional here)
  "Return unicad version.
If optional argument HERE is non-nil, insert string at point."
  (interactive "P")
  (if here
      (insert unicad-version)
    (if (interactive-p)
        (message "%s" unicad-version)
      unicad-version)))

(defun unicad-enable ()
  "Enable Unicad function."
  (interactive)
  (setq unicad-global-enable t)
  (message "Unicad enabled."))

(defun unicad-disable ()
  "Disable Unicad function."
  (interactive)
  (setq unicad-global-enable -1)
  (message "Unicad DISABLED."))

(defalias 'unicad 'unicad-enable)
;;}}}

;;{{{  Auto Coding Function

(defun unicad-char-after (&optional pos)
  (let (char)
    (if pos
        (setq char (char-after pos))
      (setq char (char-after)))
    (if (numberp char)
        (progn
          (if (> char #xff)
              (setq char (logand char #xff)))
          char)
      nil)))

;; ePureAscii, eEscAscii -> unicad-default-coding-system
;; eHighbyte -> multibyte-group-prober -> latin1->prober
(defun unicad-universal-charset-detect (size)
  "detect charset"
  ;;(goto-char (point-min))
  (when (and  (or (and (numberp unicad-global-enable) (> unicad-global-enable 0))
                  (eq unicad-global-enable t))
              (not (local-variable-p 'buffer-file-coding-system)))
    (let ((buf (current-buffer)))
;;       (make-local-variable 'unicad-best-guess)
;;       (make-local-variable 'unicad-singlebyte-best-guess)
;;       (make-local-variable 'unicad-singlebyte-group-guess)
;;       (make-local-variable 'unicad-latin-best-guess)
;;       (make-local-variable 'unicad-latin1-guess)
;;       (make-local-variable 'unicad-latin2-guess)
;;       (make-local-variable 'unicad-multibyte-group-list)
      (save-excursion
        (goto-char (point-min))
        (let (;;(end (+ (point) (min size unicad-max-size)))
              (end (min size (+ (point) unicad-max-size)))
              (input-state 'ePureAscii)
              (code0 0)
              prober-result
              code1 code2 state
              start quick-start quick-end
              )
          (setq unicad-eol nil)
          (unless (setq prober-result (unicad-bom-detect))
            (goto-char (point-min))
            (while (and (not (eq state 'mDone))
                        (eq input-state 'ePureAscii)
                        (< (point) end))
              (setq code1 (unicad-char-after))
;; 	      (if (not (numberp code1))
;; 		  (message "code1 nil %d, end point %d" (point) end))
              (forward-char 1)
              (if (and (= code0 #x0D) (= code1 #x0A))
                  (setq unicad-eol 1))
              (if (and (>= code1 #x80)
                       (/= code1 #xA0)) ;; since many Ascii only page contains NBSP
                  ;; we got a non-ascii byte (high-byte)
                  (setq input-state 'eHighbyte
                        start (1- (point)))
                ;; OK, just pure ascii so far
                (if (and (eq input-state 'ePureAscii)
                         (or (= code1 #x1B) ;; ESC char
                             (and (= code1 ?{ ) (= code0 ?~ )))) ;; HZ "~{"
                    ;; found escape character or HZ "~{"
                    (setq input-state 'eEscAscii
                          start (1- (point))))
                (setq code0 code1)))
            (cond
             ((eq input-state 'eEscAscii)
              ;; actually emacs itself can tell the esc charset very well
              ;; so we don't need to do ourselves
              ;; (unicad-esc-group-prober (point-min) end)
              )
             ((eq input-state 'eHighbyte)
              (if (> (- end start) unicad-quick-size)
                  (setq quick-start start
                        quick-end (+ unicad-quick-size start))
                (setq quick-start start
                      quick-end end))
              (let ((maxConfidence 0.0)
                    quick-status)
                (cond
                 ((eq (setq quick-status
                            (unicad-multibyte-group-prober quick-start quick-end)) 'eFoundIt)
                  (setq prober-result (car unicad-best-guess))
                  (setq maxConfidence unicad--sure-yes))
                 ((and (not (eq quick-status 'eNotMe))
                       (/= quick-end end)
                       (eq (unicad-multibyte-group-prober start end) 'eFoundIt))
                  (setq prober-result (car unicad-best-guess))
                  (setq maxConfidence unicad--sure-yes))
                 ((eq (unicad-singlebyte-group-prober start end) 'eFoundIt)
                  (setq prober-result (car unicad-singlebyte-best-guess)))
                 (t
                  (setq maxConfidence (unicad-latin-group-prober start end))
                  (if (> (cadr unicad-singlebyte-best-guess) (cadr unicad-best-guess))
;;                       (set (make-local-variable 'unicad-best-guess) unicad-singlebyte-best-guess)
                      (setq unicad-best-guess unicad-singlebyte-best-guess)
                    )
                  (if (> maxConfidence (cadr unicad-best-guess))
                      (setq prober-result (car unicad-latin-best-guess)
                            unicad-best-guess unicad-latin-best-guess)
                    (setq prober-result (car unicad-best-guess)
                          maxConfidence (cadr unicad-best-guess)))))))))
;;           (test-foo)
;;           (message "%S" (current-buffer))
          (unless (coding-system-p prober-result)
            (setq prober-result unicad-default-coding-system))
          (if (null prober-result) (setq prober-result 'undecided))
          (when (numberp unicad-eol)
            (if (null (numberp (coding-system-eol-type prober-result)))
                (if (and unicad-eol (= unicad-eol 1))
                    ;; i want to skip unix eol
                    (setq prober-result (aref (coding-system-eol-type prober-result) unicad-eol)))))
;;           (if (and unicad-eol (= unicad-eol 1))
;;               (message "unicad dos.")
;;             (message "unicad unix/mac."))
          (or prober-result unicad-default-coding-system))))))

(if (>= emacs-major-version 22)
    (add-to-list 'auto-coding-functions 'unicad-universal-charset-detect)
  (setq set-auto-coding-function 'emacs21-check-coding-system))

(defun emacs21-check-coding-system (filename size)
  (let (coding-system)
    (setq coding-system (set-auto-coding filename size))
    (unless coding-system
      (setq coding-system (unicad-universal-charset-detect size)))
    coding-system))

(add-hook 'kill-emacs-hook 'unicad-disable)

;;}}}

;;{{{  BOM detector
(defun unicad-bom-detect ()
  "BOM detector. For detecting the signature of utf-8, utf-16le,
utf-16be ..."
  (save-excursion
    (goto-char (point-min))
    (when (> (point-max) 3)
      (let (code0 code1 code2 code3)
        (setq code0 (unicad-char-after))
        (forward-char)
        (setq code1 (unicad-char-after))
        (forward-char)
        (setq code2 (unicad-char-after))
        (forward-char)
        (setq code3 (unicad-char-after))
        (forward-char)
        (cond
         ((and (= code0 #xEF)
               (= code1 #xBB)
               (= code2 #xBF))
          'utf-8)
         ((and (= code0 #xFE)
               (= code1 #xFF)
               (= code2 #x00)
               (= code3 #x00))
          ;; unusual octet order BOM (3412)
          nil)
         ((and (= code0 #xFE)
               (= code1 #xFF))
          'utf-16be-with-signature)
         ((and (= code0 0)
               (= code1 0)
               (= code2 #xFE)
               (= code3 #xFF))
          ;;utf-32be is not supported by emacs23
          nil)
         ((and (= code0 0)
               (= code1 0)
               (= code2 #xFF)
               (= code3 #xFE))
          ;; ucs-4 unusual octet order BOM (2143)
          nil)
         ((and (= code0 #xFF)
               (= code1 #xFE)
               (= code2 #x00)
               (= code3 #x00))
          ;; utf-32 little-endian not supported by emacs23
          nil)
         ((and (= code0 #xFF)
               (= code1 #xFE))
          'utf-16le-with-signature))))))
;;}}}

;;{{{  some chardet functions

(defsubst unicad-chardet-prober (chardet)
  "fetch the multibyte chardet prober function"
  (aref chardet 2))

(defsubst unicad-chardet-name (chardet)
  "get the coding system name of a multibyte chardet"
  (aref chardet 0))

(defsubst unicad-chardet-set-confidence (chardet conf)
  "set the confidence (probability) of a multibyte chardet"
  (aset chardet 1 conf))

(defsubst unicad-chardet-confidence (chardet)
  "get the confidence"
  (aref chardet 1))

(defun unicad-chardet (group prober)
  "Search for detector by CODING"
  (let ((lists group)
        chardet)
    (while lists
      (setq chardet (pop lists))
      (if (eq (unicad-chardet-prober chardet) prober)
          (setq lists nil)
        (setq chardet nil)))
    chardet))
;;}}}

;;{{{  Greek Model
;; ****************************************************************
;; 255: Control characters that usually does not exist in any text
;; 254: Carriage/Return
;; 253: symbol (punctuation) that does not belong to word
;; 252: 0 - 9

;; *****************************************************************/

;;Character Mapping Table:
(defvar unicad-latin7-char2order-map
  [
   255 255 255 255 255 255 255 255 255 255 254 255 255 254 255 255 ;;00
   255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;10
   253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 ;;20
   252 252 252 252 252 252 252 252 252 252 253 253 253 253 253 253 ;;30
   253  82 100 104  94  98 101 116 102 111 187 117  92  88 113  85 ;;40
   79 118 105  83  67 114 119  95  99 109 188 253 253 253 253 253 ;;50
   253  72  70  80  81  60  96  93  89  68 120  97  77  86  69  55 ;;60
   78 115  65  66  58  76 106 103  87 107 112 253 253 253 253 253 ;;70
   255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;80
   255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;90
   253 233  90 253 253 253 253 253 253 253 253 253 253  74 253 253 ;;a0
   253 253 253 253 247 248  61  36  46  71  73 253  54 253 108 123 ;;b0
   110  31  51  43  41  34  91  40  52  47  44  53  38  49  59  39 ;;c0
   35  48 250  37  33  45  56  50  84  57 120 121  17  18  22  15 ;;d0
   124   1  29  20  21   3  32  13  25   5  11  16  10   6  30   4 ;;e0
   9   8  14   7   2  12  28  23  42  24  64  75  19  26  27 253 ;;f0
   ])



(defvar unicad-win1253-char2order-map
  [
   255 255 255 255 255 255 255 255 255 255 254 255 255 254 255 255 ;;00
   255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;10
   253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 ;;20
   252 252 252 252 252 252 252 252 252 252 253 253 253 253 253 253 ;;30
   253  82 100 104  94  98 101 116 102 111 187 117  92  88 113  85 ;;40
   79 118 105  83  67 114 119  95  99 109 188 253 253 253 253 253 ;;50
   253  72  70  80  81  60  96  93  89  68 120  97  77  86  69  55 ;;60
   78 115  65  66  58  76 106 103  87 107 112 253 253 253 253 253 ;;70
   255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;80
   255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;90
   253 233  61 253 253 253 253 253 253 253 253 253 253  74 253 253 ;;a0
   253 253 253 253 247 253 253  36  46  71  73 253  54 253 108 123 ;;b0
   110  31  51  43  41  34  91  40  52  47  44  53  38  49  59  39 ;;c0
   35  48 250  37  33  45  56  50  84  57 120 121  17  18  22  15 ;;d0
   124   1  29  20  21   3  32  13  25   5  11  16  10   6  30   4 ;;e0
   9   8  14   7   2  12  28  23  42  24  64  75  19  26  27 253 ;;f0
   ])

;;Model Table:
;;total sequences: 100%
;;first 512 sequences: 98.2851%
;;first 1024 sequences:1.7001%
;;rest  sequences:     0.0359%
;;negative sequences:  0.0148%
(defvar unicad-greek-lang-model
  [
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 3 2 2 3 3 3 3 3 3 3 3 1 3 3 3 0 2 2 3 3 0 3 0 3 2 0 3 3 3 0
   3 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 3 3 3 3 0 3 3 0 3 2 3 3 0 3 2 3 3 3 0 0 3 0 3 0 3 3 2 0 0 0
   2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0
   0 2 3 2 2 3 3 3 3 3 3 3 3 0 3 3 3 3 0 2 3 3 0 3 3 3 3 2 3 3 3 0
   2 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 2 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 0 2 1 3 3 3 3 2 3 3 2 3 3 2 0
   0 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 3 3 3 0 3 3 3 3 3 3 0 3 3 0 3 3 3 3 3 3 3 3 3 3 0 3 2 3 3 0
   2 0 1 0 2 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
   0 3 3 3 3 3 2 3 0 0 0 0 3 3 0 3 1 3 3 3 0 3 3 0 3 3 3 3 0 0 0 0
   2 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 3 3 3 3 0 3 0 3 3 3 3 3 0 3 2 2 2 3 0 2 3 3 3 3 3 2 3 3 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 3 3 3 3 3 2 2 2 3 3 3 3 0 3 1 3 3 3 3 2 3 3 3 3 3 3 3 2 2 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 3 3 3 3 2 0 3 0 0 0 3 3 2 3 3 3 3 3 0 0 3 2 3 0 2 3 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 0 3 3 3 3 0 0 3 3 0 2 3 0 3 0 3 3 3 0 0 3 0 3 0 2 2 3 3 0 0
   0 0 1 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 3 3 3 3 2 0 3 2 3 3 3 3 0 3 3 3 3 3 0 3 3 2 3 2 3 3 2 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 3 2 3 2 3 3 3 3 3 3 0 2 3 2 3 2 2 2 3 2 3 3 2 3 0 2 2 2 3 0
   2 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 3 0 0 0 3 3 3 2 3 3 0 0 3 0 3 0 0 0 3 2 0 3 0 3 0 0 2 0 2 0
   0 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 3 3 3 0 3 3 3 3 3 3 0 3 3 0 3 0 0 0 3 3 0 3 3 3 0 0 1 2 3 0
   3 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 3 3 3 3 2 0 0 3 2 2 3 3 0 3 3 3 3 3 2 1 3 0 3 2 3 3 2 1 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 3 3 0 2 3 3 3 3 3 3 0 0 3 0 3 0 0 0 3 3 0 3 2 3 0 0 3 3 3 0
   3 0 0 0 2 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 3 3 3 0 3 3 3 3 3 3 0 0 3 0 3 0 0 0 3 2 0 3 2 3 0 0 3 2 3 0
   2 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 3 1 2 2 3 3 3 3 3 3 0 2 3 0 3 0 0 0 3 3 0 3 0 2 0 0 2 3 1 0
   2 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 0 3 3 3 3 0 3 0 3 3 2 3 0 3 3 3 3 3 3 0 3 3 3 0 2 3 0 0 3 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 0 3 3 3 0 0 3 0 0 0 3 3 0 3 0 2 3 3 0 0 3 0 3 0 3 3 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 3 0 0 0 3 3 3 3 3 3 0 0 3 0 2 0 0 0 3 3 0 3 0 3 0 0 2 0 2 0
   0 0 0 0 1 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 3 3 3 3 3 0 3 0 2 0 3 2 0 3 2 3 2 3 0 0 3 2 3 2 3 3 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 3 0 0 2 3 3 3 3 3 0 0 0 3 0 2 1 0 0 3 2 2 2 0 3 0 0 2 2 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 0 3 3 3 2 0 3 0 3 0 3 3 0 2 1 2 3 3 0 0 3 0 3 0 3 3 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 2 3 3 3 0 3 3 3 3 3 3 0 2 3 0 3 0 0 0 2 1 0 2 2 3 0 0 2 2 2 0
   0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 3 0 0 2 3 3 3 2 3 0 0 1 3 0 2 0 0 0 0 3 0 1 0 2 0 0 1 1 1 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 3 3 3 3 1 0 3 0 0 0 3 2 0 3 2 3 3 3 0 0 3 0 3 2 2 2 1 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 0 3 3 3 0 0 3 0 0 0 0 2 0 2 3 3 2 2 2 2 3 0 2 0 2 2 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 3 3 3 2 0 0 0 0 0 0 2 3 0 2 0 2 3 2 0 0 3 0 3 0 3 1 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 3 2 3 3 2 2 3 0 2 0 3 0 0 0 2 0 0 0 0 1 2 0 2 0 2 0
   0 2 0 2 0 2 2 0 0 1 0 2 2 2 0 2 2 2 0 2 2 2 0 0 2 0 0 1 0 0 0 0
   0 2 0 3 3 2 0 0 0 0 0 0 1 3 0 2 0 2 2 2 0 0 2 0 3 0 0 2 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 3 0 2 3 2 0 2 2 0 2 0 2 2 0 2 0 2 2 2 0 0 0 0 0 0 2 3 0 0 0 2
   0 1 2 0 0 0 0 2 2 0 0 0 2 1 0 2 2 0 0 0 0 0 0 1 0 2 0 0 0 0 0 0
   0 0 2 1 0 2 3 2 2 3 2 3 2 0 0 3 3 3 0 0 3 2 0 0 0 1 1 0 2 0 2 2
   0 2 0 2 0 2 2 0 0 2 0 2 2 2 0 2 2 2 2 0 0 2 0 0 0 2 0 1 0 0 0 0
   0 3 0 3 3 2 2 0 3 0 0 0 2 2 0 2 2 2 1 2 0 0 1 2 2 0 0 3 0 0 0 2
   0 1 2 0 0 0 1 2 0 0 0 0 0 0 0 2 2 0 1 0 0 2 0 0 0 2 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 2 3 3 2 2 0 0 0 2 0 2 3 3 0 2 0 0 0 0 0 0 2 2 2 0 2 2 0 2 0 2
   0 2 2 0 0 2 2 2 2 1 0 0 2 2 0 2 0 0 2 0 0 0 0 0 0 2 0 0 0 0 0 0
   0 2 0 3 2 3 0 0 0 3 0 0 2 2 0 2 0 2 2 2 0 0 2 0 0 0 0 0 0 0 0 2
   0 0 2 2 0 0 2 2 2 0 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 2 0 0 3 2 0 2 2 2 2 2 0 0 0 2 0 0 0 0 2 0 1 0 0 2 0 1 0 0 0
   0 2 2 2 0 2 2 0 1 2 0 2 2 2 0 2 2 2 2 1 2 2 0 0 2 0 0 0 0 0 0 0
   0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
   0 2 0 2 0 2 2 0 0 0 0 1 2 1 0 0 2 2 0 0 2 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 3 2 3 0 0 2 0 0 0 2 2 0 2 0 0 0 1 0 0 2 0 2 0 2 2 0 0 0 0
   0 0 2 0 0 0 0 2 2 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0
   0 2 2 3 2 2 0 0 0 0 0 0 1 3 0 2 0 2 2 0 0 0 1 0 2 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 2 0 2 0 3 2 0 2 0 0 0 0 0 0 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
   0 0 2 0 0 0 0 1 1 0 0 2 1 2 0 2 2 0 1 0 0 1 0 0 0 2 0 0 0 0 0 0
   0 3 0 2 2 2 0 0 2 0 0 0 2 0 0 0 2 3 0 2 0 0 0 0 0 0 2 2 0 0 0 2
   0 1 2 0 0 0 1 2 2 1 0 0 0 2 0 0 2 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 2 1 2 0 2 2 0 2 0 0 2 0 0 0 0 1 2 1 0 2 1 0 0 0 0 0 0 0 0 0 0
   0 0 2 0 0 0 3 1 2 2 0 2 0 0 0 0 2 0 0 0 2 0 0 3 0 0 0 0 2 2 2 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 2 1 0 2 0 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 1 0 0 0 0 0 0 2
   0 2 2 0 0 2 2 2 2 2 0 1 2 0 0 0 2 2 0 1 0 2 0 0 2 2 0 0 0 0 0 0
   0 0 0 0 1 0 0 0 0 0 0 0 3 0 0 2 0 0 0 0 0 0 0 0 2 0 2 0 0 0 0 2
   0 1 2 0 0 0 0 2 2 1 0 1 0 1 0 2 2 2 1 0 0 0 0 0 0 1 0 0 0 0 0 0
   0 2 0 1 2 0 0 0 0 0 0 0 0 0 0 2 0 0 2 2 0 0 0 0 1 0 0 0 0 0 0 2
   0 2 2 0 0 0 0 2 2 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 2 0 0 2 0 0 0
   0 2 2 2 2 0 0 0 3 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 2 0 0 0 0 0 0 1
   0 0 2 0 0 0 0 1 2 0 0 0 0 0 0 2 2 1 1 0 0 0 0 0 0 1 0 0 0 0 0 0
   0 2 0 2 2 2 0 0 2 0 0 0 0 0 0 0 2 2 2 0 0 0 2 0 0 0 0 0 0 0 0 2
   0 0 1 0 0 0 0 2 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0
   0 3 0 2 0 0 0 0 0 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 0 0 2 0 0 0 0 2
   0 0 2 0 0 0 0 2 2 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 2 0 2 2 1 0 0 0 0 0 0 2 0 0 2 0 2 2 2 0 0 0 0 0 0 2 0 0 0 0 2
   0 0 2 0 0 2 0 2 2 0 0 0 0 2 0 2 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0
   0 0 3 0 0 0 2 2 0 2 2 0 0 0 0 0 2 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 2 0 0 0 0 0
   0 2 2 2 2 2 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1
   0 0 0 0 0 0 0 2 1 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 2 2 0 0 0 0 0 2 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
   0 2 0 0 0 2 0 0 0 0 0 1 0 0 0 0 2 2 0 0 0 1 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 2 0 0 0
   0 2 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 2 0 2 0 0 0
   0 0 0 0 0 0 0 0 2 1 0 0 0 0 0 0 2 0 0 0 1 2 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   ])

;;}}}
;;{{{  Russian Model

;;KOI8-R language model
;;Character Mapping Table:
(defvar unicad-koi8r-char2order-map
  `[
    255 255 255 255 255 255 255 255 255 255 254 255 255 254 255 255 ;;00
    255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;10
    +253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 ;;20
    252 252 252 252 252 252 252 252 252 252 253 253 253 253 253 253 ;;30
    253 142 143 144 145 146 147 148 149 150 151 152  74 153  75 154 ;;40
    155 156 157 158 159 160 161 162 163 164 165 253 253 253 253 253 ;;50
    253  71 172  66 173  65 174  76 175  64 176 177  77  72 178  69 ;;60
    67 179  78  73 180 181  79 182 183 184 185 253 253 253 253 253 ;;70
    191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 ;;80
    207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 ;;90
    223 224 225  68 226 227 228 229 230 231 232 233 234 235 236 237 ;;a0
    238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 ;;b0
    27   3  21  28  13   2  39  19  26   4  23  11   8  12   5   1 ;;c0
    15  16   9   7   6  14  24  10  17  18  20  25  30  29  22  54 ;;d0
    59  37  44  58  41  48  53  46  55  42  60  36  49  38  31  34 ;;e0
    35  43  45  32  40  52  56  33  61  62  51  57  47  63  50  70 ;;f0
    ])

(defvar unicad-win1251-char2order-map
  `[
    255 255 255 255 255 255 255 255 255 255 254 255 255 254 255 255 ;;00
    255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;10
    +253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 ;;20
    252 252 252 252 252 252 252 252 252 252 253 253 253 253 253 253 ;;30
    253 142 143 144 145 146 147 148 149 150 151 152  74 153  75 154 ;;40
    155 156 157 158 159 160 161 162 163 164 165 253 253 253 253 253 ;;50
    253  71 172  66 173  65 174  76 175  64 176 177  77  72 178  69 ;;60
    67 179  78  73 180 181  79 182 183 184 185 253 253 253 253 253 ;;70
    191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206
    207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222
    223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238
    239 240 241 242 243 244 245 246  68 247 248 249 250 251 252 253
    37  44  33  46  41  48  56  51  42  60  36  49  38  31  34  35
    45  32  40  52  53  55  58  50  57  63  70  62  61  47  59  43
    3  21  10  19  13   2  24  20   4  23  11   8  12   5   1  15
    9   7   6  14  39  26  28  22  25  29  54  18  17  30  27  16
    ])

(defvar unicad-latin5-char2order-map
  `[
    255 255 255 255 255 255 255 255 255 255 254 255 255 254 255 255 ;;00
    255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;10
    +253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 ;;20
    252 252 252 252 252 252 252 252 252 252 253 253 253 253 253 253 ;;30
    253 142 143 144 145 146 147 148 149 150 151 152  74 153  75 154 ;;40
    155 156 157 158 159 160 161 162 163 164 165 253 253 253 253 253 ;;50
    253  71 172  66 173  65 174  76 175  64 176 177  77  72 178  69 ;;60
    67 179  78  73 180 181  79 182 183 184 185 253 253 253 253 253 ;;70
    191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206
    207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222
    223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238
    37  44  33  46  41  48  56  51  42  60  36  49  38  31  34  35
    45  32  40  52  53  55  58  50  57  63  70  62  61  47  59  43
    3  21  10  19  13   2  24  20   4  23  11   8  12   5   1  15
    9   7   6  14  39  26  28  22  25  29  54  18  17  30  27  16
    239  68 240 241 242 243 244 245 246 247 248 249 250 251 252 255
    ])

(defvar unicad-maccyrillic-char2order-map
  `[
    255 255 255 255 255 255 255 255 255 255 254 255 255 254 255 255 ;;00
    255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;10
    +253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 ;;20
    252 252 252 252 252 252 252 252 252 252 253 253 253 253 253 253 ;;30
    253 142 143 144 145 146 147 148 149 150 151 152  74 153  75 154 ;;40
    155 156 157 158 159 160 161 162 163 164 165 253 253 253 253 253 ;;50
    253  71 172  66 173  65 174  76 175  64 176 177  77  72 178  69 ;;60
    67 179  78  73 180 181  79 182 183 184 185 253 253 253 253 253 ;;70
    37  44  33  46  41  48  56  51  42  60  36  49  38  31  34  35
    45  32  40  52  53  55  58  50  57  63  70  62  61  47  59  43
    191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206
    207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222
    223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238
    239 240 241 242 243 244 245 246 247 248 249 250 251 252  68  16
    3  21  10  19  13   2  24  20   4  23  11   8  12   5   1  15
    9   7   6  14  39  26  28  22  25  29  54  18  17  30  27 255
    ])

(defvar unicad-ibm855-char2order-map
  `[
    255 255 255 255 255 255 255 255 255 255 254 255 255 254 255 255 ;;00
    255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;10
    +253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 ;;20
    252 252 252 252 252 252 252 252 252 252 253 253 253 253 253 253 ;;30
    253 142 143 144 145 146 147 148 149 150 151 152  74 153  75 154 ;;40
    155 156 157 158 159 160 161 162 163 164 165 253 253 253 253 253 ;;50
    253  71 172  66 173  65 174  76 175  64 176 177  77  72 178  69 ;;60
    67 179  78  73 180 181  79 182 183 184 185 253 253 253 253 253 ;;70
    191 192 193 194  68 195 196 197 198 199 200 201 202 203 204 205
    206 207 208 209 210 211 212 213 214 215 216 217  27  59  54  70
    3  37  21  44  28  58  13  41   2  48  39  53  19  46 218 219
    220 221 222 223 224  26  55   4  42 225 226 227 228  23  60 229
    230 231 232 233 234 235  11  36 236 237 238 239 240 241 242 243
    8  49  12  38   5  31   1  34  15 244 245 246 247  35  16 248
    43   9  45   7  32   6  40  14  52  24  56  10  33  17  61 249
    250  18  62  20  51  25  57  30  47  29  63  22  50 251 252 255
    ])

(defvar unicad-ibm866-char2order-map
  `[
    255 255 255 255 255 255 255 255 255 255 254 255 255 254 255 255 ;;00
    255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;10
    +253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 ;;20
    252 252 252 252 252 252 252 252 252 252 253 253 253 253 253 253 ;;30
    253 142 143 144 145 146 147 148 149 150 151 152  74 153  75 154 ;;40
    155 156 157 158 159 160 161 162 163 164 165 253 253 253 253 253 ;;50
    253  71 172  66 173  65 174  76 175  64 176 177  77  72 178  69 ;;60
    67 179  78  73 180 181  79 182 183 184 185 253 253 253 253 253 ;;70
    37  44  33  46  41  48  56  51  42  60  36  49  38  31  34  35
    45  32  40  52  53  55  58  50  57  63  70  62  61  47  59  43
    3  21  10  19  13   2  24  20   4  23  11   8  12   5   1  15
    191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206
    207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222
    223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238
    9   7   6  14  39  26  28  22  25  29  54  18  17  30  27  16
    239  68 240 241 242 243 244 245 246 247 248 249 250 251 252 255
    ])

;;Model Table:
;;total sequences: 100%
;;first 512 sequences: 97.6601%
;;first 1024 sequences: 2.3389%
;;rest  sequences:      0.1237%
;;negative sequences:   0.0009%
(defvar unicad-russian-lang-model
  `[
    0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 3 3 3 3 1 3 3 3 2 3 2 3 3
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0 3 2 2 2 2 2 0 0 2
    3 3 3 2 3 3 3 3 3 3 3 3 3 3 2 3 3 0 0 3 3 3 3 3 3 3 3 3 2 3 2 0
    0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 2 2 3 3 3 3 3 3 3 3 3 2 3 3 0 0 3 3 3 3 3 3 3 3 2 3 3 1 0
    0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 2 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 0 0 3 3 3 3 3 3 3 3 3 3 3 2 1
    0 0 0 0 0 0 0 2 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 0 0 3 3 3 3 3 3 3 3 3 3 3 2 1
    0 0 0 0 0 1 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 3 3 2 2 2 3 1 3 3 1 3 3 3 3 2 2 3 0 2 2 2 3 3 2 1 0
    0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 2 3 3 3 3 3 2 2 3 2 3 3 3 2 1 2 2 0 1 2 2 2 2 2 2 0
    0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 3 0 2 2 3 3 2 1 2 0
    0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 1 0 0 2 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 2 3 3 1 2 3 2 2 3 2 3 3 3 3 2 2 3 0 3 2 2 3 1 1 1 0
    0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 3 3 2 2 3 3 3 3 3 2 3 3 3 3 2 2 2 0 3 3 3 2 2 2 2 0
    0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 3 3 3 3 2 3 2 3 3 3 3 3 3 2 3 2 2 0 1 3 2 1 2 2 1 0
    0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 3 3 3 3 3 2 1 1 3 0 1 1 1 1 2 1 1 0 2 2 2 1 2 0 1 0
    0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 2 3 3 2 2 2 2 1 3 2 3 2 3 2 1 2 2 0 1 1 2 1 2 1 2 0
    0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 3 3 3 3 3 3 2 2 3 2 3 3 3 2 2 2 2 0 2 2 2 2 3 1 1 0
    0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0
    3 2 3 2 2 3 3 3 3 3 3 3 3 3 1 3 2 0 0 3 3 3 3 2 3 3 3 3 2 3 2 0
    0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 3 3 3 3 3 2 2 3 3 0 2 1 0 3 2 3 2 3 0 0 1 2 0 0 1 0 1 2 1 1 0
    0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 0 3 0 2 3 3 3 3 2 3 3 3 3 1 2 2 0 0 2 3 2 2 2 3 2 3 2 2 3 0 0
    0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 2 3 0 2 3 2 3 0 1 2 3 3 2 0 2 3 0 0 2 3 2 2 0 1 3 1 3 2 2 1 0
    0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 1 3 0 2 3 3 3 3 3 3 3 3 2 1 3 2 0 0 2 2 3 3 3 2 3 3 0 2 2 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 2 2 3 3 2 2 2 3 3 0 0 1 1 1 1 1 2 0 0 1 1 1 1 0 1 0
    0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 2 2 3 3 3 3 3 3 3 0 3 2 3 3 2 3 2 0 2 1 0 1 1 0 1 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 2 3 3 3 2 2 2 2 3 1 3 2 3 1 1 2 1 0 2 2 2 2 1 3 1 0
    0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0
    2 2 3 3 3 3 3 1 2 2 1 3 1 0 3 0 0 3 0 0 0 1 1 0 1 2 1 0 0 0 0 0
    0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 2 2 1 1 3 3 3 2 2 1 2 2 3 1 1 2 0 0 2 2 1 3 0 0 2 1 1 2 1 1 0
    0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 2 3 3 3 3 1 2 2 2 1 2 1 3 3 1 1 2 1 2 1 2 2 0 2 0 0 1 1 0 1 0
    0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 3 3 3 3 3 2 1 3 2 2 3 2 0 3 2 0 3 0 1 0 1 1 0 0 1 1 1 1 0 1 0
    0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 2 3 3 3 2 2 2 3 3 1 2 1 2 1 0 1 0 1 1 0 1 0 0 2 1 1 1 0 1 0
    0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0
    3 1 1 2 1 2 3 3 2 2 1 2 2 3 0 2 1 0 0 2 2 3 2 1 2 2 2 2 2 3 1 0
    0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 1 1 0 1 1 2 2 1 1 3 0 0 1 3 1 1 1 0 0 0 1 0 1 1 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 1 3 3 3 2 0 0 0 2 1 0 1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 0 1 0 0 2 3 2 2 2 1 2 2 2 1 2 1 0 0 1 1 1 0 2 0 1 1 1 0 0 1 1
    1 0 0 0 0 0 1 2 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0
    2 3 3 3 3 0 0 0 0 1 0 0 0 0 3 0 1 2 1 0 0 0 0 0 0 0 1 1 0 0 1 1
    1 0 1 0 1 2 0 0 1 1 2 1 0 1 1 1 1 0 1 1 1 1 0 1 0 0 1 0 0 1 1 0
    2 2 3 2 2 2 3 1 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 0 1 0 1 1 1 0 2 1
    1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 0 1 0 1 1 0 1 1 1 0 1 1 0
    3 3 3 2 2 2 2 3 2 2 1 1 2 2 2 2 1 1 3 1 2 1 2 0 0 1 1 0 1 0 2 1
    1 1 1 1 1 2 1 0 1 1 1 1 0 1 0 0 1 1 0 0 1 0 1 0 0 1 0 0 0 1 1 0
    2 0 0 1 0 3 2 2 2 2 1 2 1 2 1 2 0 0 0 2 1 2 2 1 1 2 2 0 1 1 0 2
    1 1 1 1 1 0 1 1 1 2 1 1 1 2 1 0 1 2 1 1 1 1 0 1 1 1 0 0 1 0 0 1
    1 3 2 2 2 1 1 1 2 3 0 0 0 0 2 0 2 2 1 0 0 0 0 0 0 1 0 0 0 0 1 1
    1 0 1 1 0 1 0 1 1 0 1 1 0 2 0 0 1 1 0 0 1 0 0 0 0 0 0 0 0 1 1 0
    2 3 2 3 2 1 2 2 2 2 1 0 0 0 2 0 0 1 1 0 0 0 0 0 0 0 1 1 0 0 2 1
    1 1 2 1 0 2 0 0 1 0 1 0 0 1 0 0 1 1 0 1 1 0 0 0 0 0 1 0 0 0 0 0
    3 0 0 1 0 2 2 2 3 2 2 2 2 2 2 2 0 0 0 2 1 2 1 1 1 2 2 0 0 0 1 2
    1 1 1 1 1 0 1 2 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 0 0 1
    2 3 2 3 3 2 0 1 1 1 0 0 1 0 2 0 1 1 3 1 0 0 0 0 0 0 0 1 0 0 2 1
    1 1 1 1 1 1 1 0 1 0 1 1 1 1 0 1 1 1 0 0 1 1 0 1 0 0 0 0 0 0 1 0
    2 3 3 3 3 1 2 2 2 2 0 1 1 0 2 1 1 1 2 1 0 1 1 0 0 1 0 1 0 0 2 0
    0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 3 3 3 2 0 0 1 1 2 2 1 0 0 2 0 1 1 3 0 0 1 0 0 0 0 0 1 0 1 2 1
    1 1 2 0 1 1 1 0 1 0 1 1 0 1 0 1 1 1 1 0 1 0 0 0 0 0 0 1 0 1 1 0
    1 3 2 3 2 1 0 0 2 2 2 0 1 0 2 0 1 1 1 0 1 0 0 0 3 0 1 1 0 0 2 1
    1 1 1 0 1 1 0 0 0 0 1 1 0 1 0 0 2 1 1 0 1 0 0 0 1 0 1 0 0 1 1 0
    3 1 2 1 1 2 2 2 2 2 2 1 2 2 1 1 0 0 0 2 2 2 0 0 0 1 2 1 0 1 0 1
    2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 2 1 1 1 0 1 0 1 1 0 1 1 1 0 0 1
    3 0 0 0 0 2 0 1 1 1 1 1 1 1 0 1 0 0 0 1 1 1 0 1 0 1 1 0 0 1 0 1
    1 1 0 0 1 0 0 0 1 0 1 1 0 0 1 0 1 0 1 0 0 0 0 1 0 0 0 1 0 0 0 1
    1 3 3 2 2 0 0 0 2 2 0 0 0 1 2 0 1 1 2 0 0 0 0 0 0 0 0 1 0 0 2 1
    0 1 1 0 0 1 1 0 0 0 1 1 0 1 1 0 1 1 0 0 1 0 0 0 0 0 0 0 0 0 1 0
    2 3 2 3 2 0 0 0 0 1 1 0 0 0 2 0 2 0 2 0 0 0 0 0 1 0 0 1 0 0 1 1
    1 1 2 0 1 2 1 0 1 1 2 1 1 1 1 1 2 1 1 0 1 0 0 1 1 1 1 1 0 1 1 0
    1 3 2 2 2 1 0 0 2 2 1 0 1 2 2 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1 1
    0 0 1 1 0 1 1 0 0 1 1 0 1 1 0 0 1 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0
    1 0 0 1 0 2 3 1 2 2 2 2 2 2 1 1 0 0 0 1 0 1 0 2 1 1 1 0 0 0 0 1
    1 1 0 1 1 0 1 1 1 1 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0
    2 0 2 0 0 1 0 3 2 1 2 1 2 2 0 1 0 0 0 2 1 0 0 2 1 1 1 1 0 2 0 2
    2 1 1 1 1 1 1 1 1 1 1 1 1 2 1 0 1 1 1 1 0 0 0 1 1 1 1 0 1 0 0 1
    1 2 2 2 2 1 0 0 1 0 0 0 0 0 2 0 1 1 1 1 0 0 0 0 1 0 1 2 0 0 2 0
    1 0 1 1 1 2 1 0 1 0 1 1 0 0 1 0 1 1 1 0 1 0 0 0 1 0 0 1 0 1 1 0
    2 1 2 2 2 0 3 0 1 1 0 0 0 0 2 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1
    0 0 0 1 1 1 0 0 1 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0
    1 2 2 3 2 2 0 0 1 1 2 0 1 2 1 0 1 0 1 0 0 1 0 0 0 0 0 0 0 0 0 1
    0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 1 1 0 0 1 0 0 0 0 0 0 0 0 1 1 0
    2 2 1 1 2 1 2 2 2 2 2 1 2 2 0 1 0 0 0 1 2 2 2 1 2 1 1 1 1 1 2 1
    1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 0 1 1 1 0 0 0 0 1 1 1 0 1 1 0 0 1
    1 2 2 2 2 0 1 0 2 2 0 0 0 0 2 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 2 0
    0 0 1 0 0 1 0 0 0 0 1 0 1 1 0 0 1 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0
    0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 2 2 2 2 0 0 0 2 2 2 0 1 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1
    0 1 1 0 0 1 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 2 2 2 2 0 0 0 0 1 0 0 1 1 2 0 0 0 0 1 0 1 0 0 1 0 0 2 0 0 0 1
    0 0 1 0 0 1 0 0 0 1 1 0 0 0 0 0 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0
    1 2 2 2 1 1 2 0 2 1 1 1 1 0 2 2 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1 1
    0 0 1 0 1 1 0 0 0 0 1 0 0 0 0 0 1 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0
    1 0 2 1 2 0 0 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0
    0 0 1 0 1 1 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0
    1 0 0 0 0 2 0 1 2 1 0 1 1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 0 0 1
    0 0 0 0 0 1 0 0 1 1 0 0 1 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1
    2 2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
    1 0 0 0 1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0
    2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
    1 1 1 0 1 0 1 0 0 1 1 1 1 0 0 0 1 0 0 0 0 1 0 0 0 1 0 1 0 0 0 0
    1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
    1 1 0 1 1 0 1 0 1 0 0 0 0 1 1 0 1 1 0 0 0 0 0 1 0 1 1 0 1 0 0 0
    0 1 1 1 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0
    ])

;;}}}
;;{{{  Bulgarian Model

;; ****************************************************************
;; 255: Control characters that usually does not exist in any text
;; 254: Carriage/Return
;; 253: symbol (punctuation) that does not belong to word
;; 252: 0 - 9
;; *****************************************************************

;;Character Mapping Table:
;;this talbe is modified base on win1251BulgarianCharToOrderMap, so
;;only number <64 is sure valid

(defconst unicad-latin5-bulgarian-char2order-map
  `[
    255 255 255 255 255 255 255 255 255 255 254 255 255 254 255 255 ;;00
    255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;10
    +253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 ;;20
    252 252 252 252 252 252 252 252 252 252 253 253 253 253 253 253 ;;30
    253  77  90  99 100  72 109 107 101  79 185  81 102  76  94  82 ;;40
    110 186 108  91  74 119  84  96 111 187 115 253 253 253 253 253 ;;50
    253  65  69  70  66  63  68 112 103  92 194 104  95  86  87  71 ;;60
    116 195  85  93  97 113 196 197 198 199 200 253 253 253 253 253 ;;70
    194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 ;;80
    210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 ;;90
    81 226 227 228 229 230 105 231 232 233 234 235 236  45 237 238 ;;a0
    31  32  35  43  37  44  55  47  40  59  33  46  38  36  41  30 ;;b0
    39  28  34  51  48  49  53  50  54  57  61 239  67 240  60  56 ;;c0
    1  18   9  20  11   3  23  15   2  26  12  10  14   6   4  13 ;;d0
    7   8   5  19  29  25  22  21  27  24  17  75  52 241  42  16 ;;e0
    62 242 243 244  58 245  98 246 247 248 249 250 251  91 252 253 ;;f0
    ])

(defconst unicad-win1251-bulgarian-char2order-map
  `[
    255 255 255 255 255 255 255 255 255 255 254 255 255 254 255 255 ;;00
    255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;10
    +253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 ;;20
    252 252 252 252 252 252 252 252 252 252 253 253 253 253 253 253 ;;30
    253  77  90  99 100  72 109 107 101  79 185  81 102  76  94  82 ;;40
    110 186 108  91  74 119  84  96 111 187 115 253 253 253 253 253 ;;50
    253  65  69  70  66  63  68 112 103  92 194 104  95  86  87  71 ;;60
    116 195  85  93  97 113 196 197 198 199 200 253 253 253 253 253 ;;70
    206 207 208 209 210 211 212 213 120 214 215 216 217 218 219 220 ;;80
    221  78  64  83 121  98 117 105 222 223 224 225 226 227 228 229 ;;90
    88 230 231 232 233 122  89 106 234 235 236 237 238  45 239 240 ;;a0
    73  80 118 114 241 242 243 244 245  62  58 246 247 248 249 250 ;;b0
    31  32  35  43  37  44  55  47  40  59  33  46  38  36  41  30 ;;c0
    39  28  34  51  48  49  53  50  54  57  61 251  67 252  60  56 ;;d0
    1  18   9  20  11   3  23  15   2  26  12  10  14   6   4  13 ;;e0
    7   8   5  19  29  25  22  21  27  24  17  75  52 253  42  16 ;;f0
    ])

;;Model Table:
;;total sequences: 100%
;;first 512 sequences: 96.9392%
;;first 1024 sequences:3.0618%
;;rest  sequences:     0.2992%
;;negative sequences:  0.0020%
(defconst unicad-bulgarian-lang-model
  `[
    0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 2 3 3 3 3 3
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0 3 3 3 2 2 3 2 2 1 2 2
    3 1 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 0 3 3 3 3 3 3 3 3 3 3 0 3 0 1
    0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 2 3 3 3 3 3 3 3 3 0 3 1 0
    0 1 0 0 0 0 0 0 0 0 1 1 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
    3 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 1 3 2 3 3 3 3 3 3 3 3 0 3 0 0
    0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 2 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 1 3 2 3 3 3 3 3 3 3 3 0 3 0 0
    0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 3 3 3 3 3 2 3 2 2 1 3 3 3 3 2 2 2 1 1 2 0 1 0 1 0 0
    0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 3 3 3 3 2 3 2 2 3 3 1 1 2 3 3 2 3 3 3 3 2 1 2 0 2 0 3 0 0
    0 0 0 0 0 0 0 1 0 0 2 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 3 3 3 3 1 3 3 3 3 3 2 3 2 3 3 3 3 3 2 3 3 1 3 0 3 0 2 0 0
    0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 3 3 3 3 3 1 3 3 2 3 3 3 1 3 3 2 3 2 2 2 0 0 2 0 2 0 2 0 0
    0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 3 3 3 3 3 3 0 3 3 3 2 2 3 3 3 1 2 2 3 2 1 1 2 0 2 0 0 0 0
    1 0 0 0 0 0 0 0 0 0 2 0 0 1 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 3 3 3 3 2 3 3 1 2 3 2 2 2 3 3 3 3 3 2 2 3 1 2 0 2 1 2 0 0
    0 0 0 0 0 0 0 0 0 0 3 0 0 1 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 3 3 1 3 3 3 3 3 2 3 3 3 2 3 3 2 3 2 2 2 3 1 2 0 1 0 1 0 0
    0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 3 3 3 3 3 3 3 3 1 1 1 2 2 1 3 1 3 2 2 3 0 0 1 0 1 0 1 0 0
    0 0 0 1 0 0 0 0 1 0 2 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 3 3 2 2 3 2 2 3 1 2 1 1 1 2 3 1 3 1 2 2 0 1 1 1 1 0 1 0 0
    0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 3 3 1 3 2 2 3 3 1 2 3 1 1 3 3 3 3 1 2 2 1 1 1 0 2 0 2 0 1
    0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 2 2 3 3 3 2 2 1 1 2 0 2 0 1 0 0
    0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1
    3 0 1 2 1 3 3 2 3 3 3 3 3 2 3 2 1 0 3 1 2 1 2 1 2 3 2 1 0 1 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 1 1 2 3 3 3 3 3 3 3 3 3 3 3 3 0 0 3 1 3 3 2 3 3 2 2 2 0 1 0 0
    0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 3 3 3 3 0 3 3 3 3 3 2 1 1 2 1 3 3 0 3 1 1 1 1 3 2 0 1 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1
    3 3 2 2 2 3 3 3 3 3 3 3 3 3 3 3 1 1 3 1 3 3 2 3 2 2 2 3 0 2 0 0
    0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 2 3 3 2 2 3 2 1 1 1 1 1 3 1 3 1 1 0 0 0 1 0 0 0 1 0 0
    0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 2 3 2 0 3 2 0 3 0 2 0 0 2 1 3 1 0 0 1 0 0 0 1 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 3 2 1 1 1 1 2 1 1 2 1 1 1 2 2 1 2 1 1 1 0 1 1 0 1 0 1 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 3 2 1 3 1 1 2 1 3 2 1 1 0 1 2 3 2 1 1 1 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 3 3 3 3 2 2 1 0 1 0 0 1 0 0 0 2 1 0 3 0 0 1 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 2 3 2 3 3 1 3 2 1 1 1 2 1 1 2 1 3 0 1 0 0 0 1 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 1 1 2 2 3 3 2 3 2 2 2 3 1 2 2 1 1 2 1 1 2 2 0 1 1 0 1 0 2 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 2 1 3 1 0 2 2 1 3 2 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 1 0 0
    0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1
    3 3 3 3 3 3 1 2 0 2 3 1 2 3 2 0 1 3 1 2 1 1 1 0 0 1 0 0 2 2 2 3
    2 2 2 2 1 2 1 1 2 2 1 1 2 0 1 1 1 0 0 1 1 0 0 1 1 0 0 0 1 1 0 1
    3 3 3 3 3 2 1 2 2 1 2 0 2 0 1 0 1 2 1 2 1 1 0 0 0 1 0 1 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 1
    3 3 2 3 3 1 1 3 1 0 3 2 1 0 0 0 1 2 0 2 0 1 0 0 0 1 0 1 2 1 2 2
    1 1 1 1 1 1 1 2 2 2 1 1 1 1 1 1 1 0 1 2 1 1 1 0 0 0 0 0 1 1 0 0
    3 1 0 1 0 2 3 2 2 2 3 2 2 2 2 2 1 0 2 1 2 1 1 1 0 1 2 1 2 2 2 1
    1 1 2 2 2 2 1 2 1 1 0 1 2 1 2 2 2 1 1 1 0 1 1 1 1 2 0 1 0 0 0 0
    2 3 2 3 3 0 0 2 1 0 2 1 0 0 0 0 2 3 0 2 0 0 0 0 0 1 0 0 2 0 1 2
    2 1 2 1 2 2 1 1 1 2 1 1 1 0 1 2 2 1 1 1 1 1 0 1 1 1 0 0 1 2 0 0
    3 3 2 2 3 0 2 3 1 1 2 0 0 0 1 0 0 2 0 2 0 0 0 1 0 1 0 1 2 0 2 2
    1 1 1 1 2 1 0 1 2 2 2 1 1 1 1 1 1 1 0 1 1 1 0 0 0 0 0 0 1 1 0 0
    2 3 2 3 3 0 0 3 0 1 1 0 1 0 0 0 2 2 1 2 0 0 0 0 0 0 0 0 2 0 1 2
    2 2 1 1 1 1 1 2 2 2 1 0 2 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 0 1 0 0
    3 3 3 3 2 2 2 2 2 0 2 1 1 1 1 2 1 2 1 1 0 2 0 1 0 1 0 0 2 0 1 2
    1 1 1 1 1 1 1 2 2 1 1 0 2 0 1 0 2 0 0 1 1 1 0 0 2 0 0 0 1 1 0 0
    2 3 3 3 3 1 0 0 0 0 0 0 0 0 0 0 2 0 0 1 1 0 0 0 0 0 0 1 2 0 1 2
    2 2 2 1 1 2 1 1 2 2 2 1 2 0 1 1 1 1 1 1 0 1 1 1 1 0 0 1 1 1 0 0
    2 3 3 3 3 0 2 2 0 2 1 0 0 0 1 1 1 2 0 2 0 0 0 3 0 0 0 0 2 0 2 2
    1 1 1 2 1 2 1 1 2 2 2 1 2 0 1 1 1 0 1 1 1 1 0 2 1 0 0 0 1 1 0 0
    2 3 3 3 3 0 2 1 0 0 2 0 0 0 0 0 1 2 0 2 0 0 0 0 0 0 0 0 2 0 1 2
    1 1 1 2 1 1 1 1 2 2 2 0 1 0 1 1 1 0 0 1 1 1 0 0 1 0 0 0 0 1 0 0
    3 3 2 2 3 0 1 0 1 0 0 0 0 0 0 0 1 1 0 3 0 0 0 0 0 0 0 0 1 0 2 2
    1 1 1 1 1 2 1 1 2 2 1 2 2 1 0 1 1 1 1 1 0 1 0 0 1 0 0 0 1 1 0 0
    3 1 0 1 0 2 2 2 2 3 2 1 1 1 2 3 0 0 1 0 2 1 1 0 1 1 1 1 2 1 1 1
    1 2 2 1 2 1 2 2 1 1 0 1 2 1 2 2 1 1 1 0 0 1 1 1 2 1 0 1 0 0 0 0
    2 1 0 1 0 3 1 2 2 2 2 1 2 2 1 1 1 0 2 1 2 2 1 1 2 1 1 0 2 1 1 1
    1 2 2 2 2 2 2 2 1 2 0 1 1 0 2 1 1 1 1 1 0 0 1 1 1 1 0 1 0 0 0 0
    2 1 1 1 1 2 2 2 2 1 2 2 2 1 2 2 1 1 2 1 2 3 2 2 1 1 1 1 0 1 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 2 2 3 2 0 1 2 0 1 2 1 1 0 1 0 1 2 1 2 0 0 0 1 1 0 0 0 1 0 0 2
    1 1 0 0 1 1 0 1 1 1 1 0 2 0 1 1 1 0 0 1 1 0 0 0 0 1 0 0 0 1 0 0
    2 0 0 0 0 1 2 2 2 2 2 2 2 1 2 1 1 1 1 1 1 1 0 1 1 1 1 1 2 1 1 1
    1 2 2 2 2 1 1 2 1 2 1 1 1 0 2 1 2 1 1 1 0 2 1 1 1 1 0 1 0 0 0 0
    3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0
    1 1 0 1 0 1 1 1 1 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 2 2 3 2 0 0 0 0 1 0 0 0 0 0 0 1 1 0 2 0 0 0 0 0 0 0 0 1 0 1 2
    1 1 1 1 1 1 0 0 2 2 2 2 2 0 1 1 0 1 1 1 1 1 0 0 1 0 0 0 1 1 0 1
    2 3 1 2 1 0 1 1 0 2 2 2 0 0 1 0 0 1 1 1 1 0 0 0 0 0 0 0 1 0 1 2
    1 1 1 1 2 1 1 1 1 1 1 1 1 0 1 1 0 1 0 1 0 1 0 0 1 0 0 0 0 1 0 0
    2 2 2 2 2 0 0 2 0 0 2 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 2 0 2 2
    1 1 1 1 1 0 0 1 2 1 1 0 1 0 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0
    1 2 2 2 2 0 0 2 0 1 1 0 0 0 1 0 0 2 0 2 0 0 0 0 0 0 0 0 0 0 1 1
    0 0 0 1 1 1 1 1 1 1 1 1 1 0 1 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0
    1 2 2 3 2 0 0 1 0 0 1 0 0 0 0 0 0 1 0 2 0 0 0 1 0 0 0 0 0 0 0 2
    1 1 0 0 1 0 0 0 1 1 0 0 1 0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0
    2 1 2 2 2 1 2 1 2 2 1 1 2 1 1 1 0 1 1 1 1 2 0 1 0 1 1 1 1 0 1 1
    1 1 2 1 1 1 1 1 1 0 0 1 2 1 1 1 1 1 1 0 0 1 1 1 0 0 0 0 0 0 0 0
    1 0 0 1 3 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 2 2 2 1 0 0 1 0 2 0 0 0 0 0 1 1 1 0 1 0 0 0 0 0 0 0 0 2 0 0 1
    0 2 0 1 0 0 1 1 2 0 1 0 1 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0
    1 2 2 2 2 0 1 1 0 2 1 0 1 1 1 0 0 1 0 2 0 1 0 0 0 0 0 0 0 0 0 1
    0 1 0 0 1 0 0 0 1 1 0 0 1 0 0 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0
    2 2 2 2 2 0 0 1 0 0 0 1 0 1 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 1
    0 1 0 1 1 1 0 0 1 1 1 0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0
    2 0 1 0 0 1 2 1 1 1 1 1 1 2 2 1 0 0 1 0 1 0 0 0 0 1 1 1 1 0 0 0
    1 1 2 1 1 1 1 0 0 0 1 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 2 1 2 1 0 0 1 0 0 0 0 0 0 0 0 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 1
    0 0 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 0 0 1 2 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0
    0 1 1 0 1 1 1 0 0 1 0 0 1 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0
    1 0 1 0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 2 0 0 2 0 1 0 0 1 0 0 1
    1 1 0 0 1 1 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0
    1 1 1 1 1 1 1 2 0 0 0 0 0 0 2 1 0 1 1 0 0 1 1 1 0 1 0 0 0 0 0 0
    2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 1 0 1 1 1 1 1 0 1 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
    ])

;;;}}}
;;{{{  Hebrew Model
;; ****************************************************************
;; 255: Control characters that usually does not exist in any text
;; 254: Carriage/Return
;; 253: symbol (punctuation) that does not belong to word
;; 252: 0 - 9

;; *****************************************************************

;; Windows-1255 language model
;; Character Mapping Table:
(defvar unicad-win1255-char2order-map
  `[
    255 255 255 255 255 255 255 255 255 255 254 255 255 254 255 255 ;;00
    255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;10
    +253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 ;;20
    252 252 252 252 252 252 252 252 252 252 253 253 253 253 253 253 ;;30
    253  69  91  79  80  92  89  97  90  68 111 112  82  73  95  85 ;;40
    78 121  86  71  67 102 107  84 114 103 115 253 253 253 253 253 ;;50
    253  50  74  60  61  42  76  70  64  53 105  93  56  65  54  49 ;;60
    66 110  51  43  44  63  81  77  98  75 108 253 253 253 253 253 ;;70
    124 202 203 204 205  40  58 206 207 208 209 210 211 212 213 214
    215  83  52  47  46  72  32  94 216 113 217 109 218 219 220 221
    34 116 222 118 100 223 224 117 119 104 125 225 226  87  99 227
    106 122 123 228  55 229 230 101 231 232 120 233  48  39  57 234
    30  59  41  88  33  37  36  31  29  35 235  62  28 236 126 237
    238  38  45 239 240 241 242 243 127 244 245 246 247 248 249 250
    9   8  20  16   3   2  24  14  22   1  25  15   4  11   6  23
    12  19  13  26  18  27  21  17   7  10   5 251 252 128  96 253
    ])

;;Model Table:
;;total sequences: 100%
;;first 512 sequences: 98.4004%
;;first 1024 sequences: 1.5981%
;;rest  sequences:      0.087%
;;negative sequences:   0.0015%
(defvar unicad-hebrew-lang-model
  `[
    0 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 2 3 2 1 2 0 1 0 0
    3 0 3 1 0 0 1 3 2 0 1 1 2 0 2 2 2 1 1 1 1 2 1 1 1 2 0 0 2 2 0 1
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2
    1 2 1 2 1 2 0 0 2 0 0 0 0 0 1 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2
    1 2 1 3 1 1 0 0 2 0 0 0 1 0 1 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 0 1 2 2 1 3
    1 2 1 1 2 2 0 0 2 2 0 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 0 1 0 1 1 0
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 2 2 2 2 3 2
    1 2 1 2 2 2 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 2 3 2 2 3 2 2 2 1 2 2 2 2
    1 2 1 1 2 2 0 1 2 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 0 2 2 2 2 2
    0 2 0 2 2 2 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 0 2 2 2
    0 2 1 2 2 2 0 0 2 1 0 0 0 0 1 0 1 0 0 0 0 0 0 2 0 0 0 0 0 0 1 0
    3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 2 1 2 3 2 2 2
    1 2 1 2 2 2 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 1 0
    3 3 3 3 3 3 3 3 3 2 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 1 0 2 0 2
    0 2 1 2 2 2 0 0 1 2 0 0 0 0 1 0 1 0 0 0 0 0 0 1 0 0 0 2 0 0 1 0
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 2 3 2 2 3 2 1 2 1 1 1
    0 1 1 1 1 1 3 0 1 0 0 0 0 2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0
    3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 0 0 1 0 0 1 0 0 0 0
    0 0 1 0 0 0 0 0 2 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2
    0 2 0 1 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0
    3 3 3 3 3 3 3 3 3 2 3 3 3 2 1 2 3 3 2 3 3 3 3 2 3 2 1 2 0 2 1 2
    0 2 0 2 2 2 0 0 1 2 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0
    3 3 3 3 3 3 3 3 3 2 3 3 3 1 2 2 3 3 2 3 2 3 2 2 3 1 2 2 0 2 2 2
    0 2 1 2 2 2 0 0 1 2 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 1 0 0 1 0
    3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 2 3 3 2 2 2 3 3 3 3 1 3 2 2 2
    0 2 0 1 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0
    3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 3 3 3 2 3 2 2 2 1 2 2 0 2 2 2 2
    0 2 0 2 2 2 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0
    3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 1 3 2 3 3 2 3 3 2 2 1 2 2 2 2 2 2
    0 2 1 2 1 2 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 1 0
    3 3 3 3 3 3 2 3 2 3 3 2 3 3 3 3 2 3 2 3 3 3 3 3 2 2 2 2 2 2 2 1
    0 2 0 1 2 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0
    3 3 3 3 3 3 3 3 3 2 1 2 3 3 3 3 3 3 3 2 3 2 3 2 1 2 3 0 2 1 2 2
    0 2 1 1 2 1 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 2 0
    3 3 3 3 3 3 3 3 3 2 3 3 3 3 2 1 3 1 2 2 2 1 2 3 3 1 2 1 2 2 2 2
    0 1 1 1 1 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 2 0 0 0 0 0 0 0 0
    3 3 3 3 3 3 3 3 3 3 0 2 3 3 3 1 3 3 3 1 2 2 2 2 1 1 2 2 2 2 2 2
    0 2 0 1 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0
    3 3 3 3 3 3 2 3 3 3 2 2 3 3 3 2 1 2 3 2 3 2 2 2 2 1 2 1 1 1 2 2
    0 2 1 1 1 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0
    3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 0 1 0 0 0 0 0
    1 0 1 0 0 0 0 0 2 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 3 3 3 3 2 3 3 2 3 1 2 2 2 2 3 2 3 1 1 2 2 1 2 2 1 1 0 2 2 2 2
    0 1 0 1 2 2 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0
    3 0 0 1 1 0 1 0 0 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 2 0
    0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 0 1 0 1 0 1 1 0 1 1 0 0 0 1 1 0 1 1 1 0 0 0 0 0 0 1 0 0 0 0 0
    0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 0 0 0 1 1 0 1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
    3 2 2 1 2 2 2 2 2 2 2 1 2 2 1 2 2 1 1 1 1 1 1 1 1 2 1 1 0 3 3 3
    0 3 0 2 2 2 2 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0
    2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 1 2 2 2 1 1 1 2 0 1
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 0 2 2 0 0 0 0 0 0
    0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 3 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 1 0 2 1 0
    0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0
    0 3 1 1 2 2 2 2 2 1 2 2 2 1 1 2 2 2 2 2 2 2 1 2 2 1 0 1 1 1 1 0
    0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    3 2 1 1 1 1 2 1 1 2 1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 0 0 0 0 0
    0 0 2 0 0 0 0 0 0 0 0 1 1 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 0 1 0 0
    2 1 1 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 1 2 1 2 1 1 1 1 0 0 0 0
    0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 2 1 2 2 2 2 2 2 2 2 2 2 1 2 1 2 1 1 2 1 1 1 2 1 2 1 2 0 1 0 1
    0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 1 2 2 2 1 2 2 2 2 2 2 2 2 1 2 1 1 1 1 1 1 2 1 2 1 1 0 1 0 1
    0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 1 2 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 2
    0 2 0 1 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0
    3 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 1 1 1 1 1 1 1 0 1 1 0 1 0 0 1 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 2 0 1 1 1 0 1 0 0 0 1 1 0 1 1 0 0 0 0 0 1 1 0 0
    0 1 1 1 2 1 2 2 2 0 2 0 2 0 1 1 2 1 1 1 1 2 1 0 1 1 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 0 1 0 0 0 0 0 1 0 1 2 2 0 1 0 0 1 1 2 2 1 2 0 2 0 0 0 1 2 0 1
    2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 2 0 2 1 2 0 2 0 0 1 1 1 1 1 1 0 1 0 0 0 1 0 0 1
    2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 1 0 0 0 0 0 1 0 2 1 1 0 1 0 0 1 1 1 2 2 0 0 1 0 0 0 1 0 0 1
    1 1 2 1 0 1 1 1 0 1 0 1 1 1 1 0 0 0 1 0 1 0 0 0 0 0 0 0 0 2 2 1
    0 2 0 1 2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 1 0 0 1 0 1 1 1 1 0 0 0 0 0 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 1 1 1 1 1 1 1 1 2 1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 1 1 1 0 1 1 0 1 0 0 0 1 1 0 1
    2 0 1 0 1 0 1 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 1 0 1 1 1 0 1 0 0 1 1 2 1 1 2 0 1 0 0 0 1 1 0 1
    1 0 0 1 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 1 0 1 1 2 0 1 0 0 0 0 2 1 1 2 0 2 0 0 0 1 1 0 1
    1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 1 0 2 1 1 0 1 0 0 2 2 1 2 1 1 0 1 0 0 0 1 1 0 1
    2 0 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 1 2 2 0 0 0 0 0 1 1 0 1 0 0 1 0 0 0 0 1 0 1
    1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 1 2 2 0 0 0 0 2 1 1 1 0 2 1 1 0 0 0 2 1 0 1
    1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 1 0 1 1 2 0 1 0 0 1 1 0 2 1 1 0 1 0 0 0 1 1 0 1
    2 2 1 1 1 0 1 1 0 1 1 0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 1 0 2 1 1 0 1 0 0 1 1 0 1 2 1 0 2 0 0 0 1 1 0 1
    2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0
    0 1 0 0 2 0 2 1 1 0 1 0 1 0 0 1 0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 1 0 1 1 2 0 1 0 0 1 1 1 0 1 0 0 1 0 0 0 1 0 0 1
    1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 0 0 0 0 0 0 0 1 0 1 1 0 0 1 0 0 2 1 1 1 1 1 0 1 0 0 0 0 1 0 1
    0 1 1 1 2 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 1 2 1 0 0 0 0 0 1 1 1 1 1 0 1 0 0 0 1 1 0 0
    ])

(defvar unicad-win1255-model
  (list
   (cons 'char2order-map unicad-win1255-char2order-map)
   (cons 'precedence-matrix unicad-hebrew-lang-model)
   (cons 'typ-positive-ratio 0.984004)
   (cons 'keep-english-letter nil)
   (cons 'charset-name 'windows-1255)
   ))

;;;}}}
;;{{{  sjis single byte Model

(defvar unicad-sjis-sb-char2order-map
  `[
    255 255 255 255 255 255 255 255 255 255 254 255 255 254 255 255 ;;00
    255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 ;;10
    +253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 253 ;;20
    252 252 252 252 252 252 252 252 252 252 253 253 253 253 253 253 ;;30
    253 142 143 144 145 146 147 148 149 150 151 152  74 153  75 154 ;;40
    155 156 157 158 159 160 161 162 163 164 165 253 253 253 253 253 ;;50
    253  71 172  66 173  65 174  76 175  64 176 177  77  72 178  69 ;;60
    67 179  78  73 180 181  79 182 183 184 185 253 253 253 253 253 ;;70
    256 256 256 256 256 256 256 256 256 256 256 256 256 256 256 256 ;;80
    256 256 256 256 256 256 256 256 256 256 256 256 256 256 256 256 ;;90
    256 252 254 254 254  16  59  37  17  58  31  53  49  33  40  24;;a0
      2  13   8  22  34  43  27  19   7  38  23  35  14   6  44  29;;b0
     10  48  50  12   3  46  39  57  47  54  25  28   5  32  41  21;;c0
     45  30  26  42  55  52  56  20  15  11  36  18  51   4   1   9;;d0
    256 256 256 256 256 256 256 256 256 256 256 256 256 256 256 256;;e0
    256 256 256 256 256 256 256 256 256 256 256 256 256 256 256 256;;f0
    ])

(defvar unicad-sjis-sb-lang-model
  `[
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 3 2 3 3 3 3 3 0 3 3 3 3 3 3 3 3 2 3 3 0 3 0 3 2 2 2 2 2 3 2
    2 3 0 3 3 3 3 2 3 2 2 3 2 2 3 3 0 3 2 2 0 0 0 2 0 2 2 0 0 0 0 0
    0 3 0 3 0 0 3 0 0 0 3 3 0 0 2 0 0 0 0 0 0 0 2 0 0 0 0 2 0 0 0 2
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 2 2 0 0 0 0 2 0 0 0 0 0 0 0
    0 3 0 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 0 3 0 0 3 3 0 0 3 3 0 0 3 0 0 0 0 0 3 0 0 0 0 0 0 0 0 3 0 0
    0 0 0 3 0 0 0 0 0 0 0 0 0 0 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 3 3 0 0 0 0 0 3 0 0 2 0 0 3 2 3 2 0 3 0 0 0 0 0 0 0 0 0 3 0
    0 0 0 0 2 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0
    0 3 0 3 0 0 2 0 0 0 3 3 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 0 3 0 0 3 0 0 0 2 0 3 0 3 3 2 0 3 0 3 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 2 0 0 0 0 0 0 0 2 0 0 0 0 0 2 2 0 2 2 0 0 0 0 0 0 0 0 0
    0 3 3 3 3 0 3 3 0 0 3 2 3 0 2 3 3 0 2 0 0 0 0 2 0 0 3 0 0 2 0 2
    0 0 0 0 0 0 0 0 0 0 0 2 3 0 2 2 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 3 0 3 0 2 2 3 0 2 0 3 0 2 3 0 0 3 0 3 0 0 0 3 0 0 0 0 3 0 0
    0 3 0 2 3 0 0 0 0 0 0 0 0 0 0 0 2 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 2 0 0 2 0 0 0 0 0 2 2 0 0 2 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 3 3 0 2 3 0 0 2 3 2 0 0 2 0 0 0 0 0 0 0 2 0 2 0 0 0 0 0 0 0
    0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 2 0 2 0 2 0 0 0 0 0 0 0 0 0 0 0
    0 3 3 3 3 3 3 2 3 3 2 0 3 0 2 0 3 0 2 0 2 0 2 0 3 0 3 2 2 0 0 0
    0 0 0 0 2 0 0 3 0 0 2 2 0 0 2 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 3 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 2 0
    0 2 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 2 3 2 0 0 3 0 0 0 3 2 0 0 3 3 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0
    0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0
    0 2 0 2 0 0 3 0 0 0 0 2 2 0 3 2 0 0 0 0 2 0 2 0 0 0 0 0 0 2 0 2
    0 0 0 2 3 0 0 2 0 0 2 2 2 0 0 0 0 0 0 2 2 0 0 0 0 0 0 0 0 0 0 0
    0 0 3 0 0 0 3 0 0 0 3 0 0 3 0 0 0 0 0 2 0 0 0 0 3 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 3 0 0 0 3 0 0 0 2 0 0 0 3 0 0 0 0 0 0 0 2 0 3 0 0 0 0 3 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 2 3 0 2 0 3 2 0 0 0 0 0 0 3 2 0 0 0 0 0 0 0 0 2 0 3 0 0 2 0 0
    0 3 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 2 2 0 0 2 0 0 0 0 2 0 0 3 3 0 0 0 0 0 0 3 0 3 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 2 3 0 3 0 2 0 0 0 2 0 2 0 2 0 0 0 0 0 0 0 2 0 3 0 0 0 0 0 0 0
    0 0 0 2 2 0 0 3 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 3 0 3 0 0 0 0 0 0 0 2 0 0 2 0 0 0 0 0 3 0 0 0 0 3 0 0 0 0 0
    0 0 0 0 2 0 0 0 0 0 2 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 0 3 0 2 2 2 0 2 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 3 0 2 0 0 0 3 3 0 0 0 0 0 0 0 0 2 0 0 0 2 0 2 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 3 2 3 0 0 0 0 0 2 3 0 0 2 3 0 0 0 0 2 0 0 0 2 0 0 0 0 2 0 0
    0 0 0 0 0 0 0 2 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 3 2 2 0 3 0 0 0 2 3 3 0 2 0 0 0 0 0 0 0 0 0 2 0 2 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 2 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 0 0 0 0 2 0 0 3 0 0 2 0 0 0 0 0 2 0 2 0 0 0 2 0 0 0 0 0 0 0
    0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 3 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 2 2 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 0 0 0 2 0 0 0 3 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 2 2 2 2 2 0 2 2 2 3 2 0 0 3 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0
    0 0 0 0 2 0 0 0 0 2 0 0 2 3 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0
    0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 3 2 2 0 2 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 3 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 3 0 2 0 0 0 0 0 0 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 3 0 0 0 0 0 0 0 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 2 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 3 0 0 0 0 0 2 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 2 3 0 3 0 2 0 0 0 2 2 2 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 2 3 0 0 0 0 0 0 0 0 2 2 0 0 2 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 3 2 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 2 2 2 3 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0
    0 3 0 2 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 2 2 0 0 0 2 0 0 0 2 0 3 0 2 2 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 2 2 0 0 0 3 0 0 0 2 0 3 0 2 0 0 0 0 0 0 0 2 0 3 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 3 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 2 0 0 0 0 0 0 0 0 0 3 0 2 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ])
;;;}}}

;;{{{  SingleByte Prober

(defconst unicad-sample-size 64)
(defconst unicad-sb-enough-rel-threshold 1024)
(defconst unicad-positive-shortcut-threshold 0.95)
(defconst unicad-negative-shortcut-threshold 0.05)
(defconst unicad-symbol-cat-order 250)
(defconst unicad-number-of-seq-cat 4)
(defconst unicad-positive-cat (1- unicad-number-of-seq-cat))
(defconst unicad-negative-cat 0)

(defvar unicad-singlebyte-group-list
  '(unicad-latin7-prober
    unicad-win1253-prober
    unicad-koi8r-prober
    unicad-win1251-prober
    unicad-latin5-prober
    unicad-ibm855-prober
    unicad-latin5-bulgarian-prober
    unicad-win1251-bulgarian-prober
    unicad-sjis-sb-prober)
  "a list of singlebyte prober functions")

(defsubst unicad-sb-dist-table-reset (dist-table)
  (fillarray dist-table 0))

(defsubst unicad-sb-seq-counters++ (seq-count-table count)
  (aset seq-count-table count (1+ (aref seq-count-table count))))

(defsubst unicad-sb-get-name (model)
  (cdr (nth 0 model)))

(defun unicad-singlebyte-group-prober (start end)
  "extract the singlebyte chardet prober functions from
`unicad-singlebyte-group-list', compare the confidence of each
chardet and return the best guess."
  (let ((lists unicad-singlebyte-group-list)
        (mState 'eDetecting)
        (bestConf 0.0)
        state prober mBestGuess cf)
    (setq unicad-singlebyte-best-guess '(nil 0.0)
          unicad-singlebyte-group-guess nil)
    (while (and lists (eq mState 'eDetecting))
      (setq state (funcall (pop lists)
                           start end))
      (cond
       ((eq state 'eFoundIt)
        (setq mBestGuess (car (nth 0 unicad-singlebyte-group-guess)))
        (setq bestConf unicad--sure-yes)
        (setq mState 'eFoundIt))
       ((eq state 'eNotMe) nil)
       (t
        (setq cf (cdr (nth 0 unicad-singlebyte-group-guess)))
        (if (> cf bestConf)
            (progn
              (setq mBestGuess (car (nth 0 unicad-singlebyte-group-guess)))
              (setq bestConf cf)))))
      )
    (setq unicad-singlebyte-group-guess (reverse unicad-singlebyte-group-guess))
    (if (or (<= bestConf unicad--sure-no) (null mBestGuess))
        (setq mState 'eNotMe)
      (setq unicad-singlebyte-best-guess (list mBestGuess bestConf)))
    mState))

(defun unicad-singlebyte-prober (start end
                                       charset-name
                                       positive-ratio
                                       char2order-map
                                       lang-model)
  "detect singlebyte charset"
  (goto-char start)
  (let ((mReversed nil)
        (mState 'eDetecting)
        (meetMSB t)
        (word-mark (point))
        (words 0)
        (order0 255)
        (total-char 0)
        (freq-char 0)
        (total-seqs 0)
        (seq-counters [0 0 0 0])
        (code0 0)
        code1 cf order1)
    (fillarray seq-counters 0)
    (save-excursion
      (while (and (< (point) end)
                  (eq mState 'eDetecting)
                  (< words unicad-quick-singlebyte-words)
                  )
        (setq code1 (unicad-char-after))
        (forward-char)
        (if (and (= code0 #x0D) (= code1 #x0A))
            (setq unicad-eol 1))
        (when (and (>= code1 #x80) (not meetMSB))
          (setq meetMSB t)
          (goto-char word-mark)
          (setq code1 (unicad-char-after))
          (forward-char)
          )
        (if (and
             (< code1 #x80)
             (not (or (and (> code1 ?a) (< code1 ?z))
                      (and (> code1 ?A) (< code1 ?Z)))))
            (setq meetMSB nil
                  word-mark (point)
                  words (1+ words)))
        (when meetMSB
          (setq order1 (aref char2order-map code1))
          (if (< order1 unicad-symbol-cat-order)
              (setq total-char (1+ total-char)))
          (when (< order1 unicad-sample-size)
            (setq freq-char (1+ freq-char))
            (when (< order0 unicad-sample-size)
              (setq total-seqs (1+ total-seqs))
              (if (not mReversed)
                  (unicad-sb-seq-counters++
                   seq-counters
                   (aref lang-model (+ order1 (* order0 unicad-sample-size))))
                (unicad-sb-seq-counters++
                 seq-counters
                 (aref lang-model (+ order0 (* order1 unicad-sample-size)))))
              ))
          (if (> order1 255)
              (setq mState 'eNotMe))
          (setq order0 order1))
        (setq code0 code1)))
    (setq cf (unicad-singlebyte-get-confidence
              positive-ratio total-char freq-char total-seqs seq-counters))
    (cond
     ((and (> total-seqs unicad-sb-enough-rel-threshold)
           (> cf unicad-positive-shortcut-threshold))
      (setq mState 'eFoundIt)
      (push (cons charset-name unicad--sure-yes) unicad-singlebyte-group-guess))
     ((< cf unicad-negative-shortcut-threshold)
      (setq mState 'eNotMe)
      (push (cons charset-name unicad--sure-no) unicad-singlebyte-group-guess))
     (t
      (push (cons charset-name cf) unicad-singlebyte-group-guess)))
    mState))

(defun unicad-singlebyte-get-confidence (positive-ratio total-char freq-char total-seqs seq-counters)
  "calculate the confidence for a singlebyte chardet"
  ;;positive approach
  (let ((confidence 0.01)
        r)
    (when (> total-seqs 0)
      (setq r (/ (/ (float (aref seq-counters unicad-positive-cat)) (float total-seqs))
                 (float positive-ratio)))
      (setq r (* r (/ (float freq-char)
                      (float total-char))))
      (setq confidence (min 0.99 r)))
    confidence))

(defconst unicad-latin7-name 'iso-8859-7
  "The charset name for latin7-prober")

(defconst unicad-latin7-positive-ratio 0.982851)

(defsubst unicad-latin7-prober (start end)
  (unicad-singlebyte-prober start end
                            unicad-latin7-name
                            unicad-latin7-positive-ratio
                            unicad-latin7-char2order-map
                            unicad-greek-lang-model))

(defconst unicad-win1253-name 'windows-1253
  "The charset name for win1253-prober")

(defconst unicad-win1253-positive-ratio 0.982851)

(defsubst unicad-win1253-prober (start end)
  (unicad-singlebyte-prober start end
                            unicad-win1253-name
                            unicad-win1253-positive-ratio
                            unicad-win1253-char2order-map
                            unicad-greek-lang-model))

(defconst unicad-koi8r-name 'koi8-r
  "The charset name for koi8r-prober")

(defconst unicad-koi8r-positive-ratio 0.976601)

(defsubst unicad-koi8r-prober (start end)
  (unicad-singlebyte-prober start end
                            unicad-koi8r-name
                            unicad-koi8r-positive-ratio
                            unicad-koi8r-char2order-map
                            unicad-russian-lang-model))

(defconst unicad-win1251-name 'windows-1251
  "The charset name for win1251-prober")

(defconst unicad-win1251-positive-ratio 0.976601)

(defsubst unicad-win1251-prober (start end)
  (unicad-singlebyte-prober start end
                            unicad-win1251-name
                            unicad-win1251-positive-ratio
                            unicad-win1251-char2order-map
                            unicad-russian-lang-model))

(defconst unicad-latin5-name 'iso-8859-5
  "The charset name for latin5-prober")

(defconst unicad-latin5-positive-ratio 0.976601)

(defsubst unicad-latin5-prober (start end)
  (unicad-singlebyte-prober start end
                            unicad-latin5-name
                            unicad-latin5-positive-ratio
                            unicad-latin5-char2order-map
                            unicad-russian-lang-model))

(defconst unicad-ibm855-name 'ibm855
  "The charset name for ibm855-prober")

(defconst unicad-ibm855-positive-ratio 0.976601)

(defsubst unicad-ibm855-prober (start end)
  (unicad-singlebyte-prober start end
                            unicad-ibm855-name
                            unicad-ibm855-positive-ratio
                            unicad-ibm855-char2order-map
                            unicad-russian-lang-model))

(defconst unicad-latin5-bulgarian-name 'iso-8859-5
  "The charset name for latin5-bulgarian-prober")

(defconst unicad-latin5-bulgarian-positive-ratio 0.976601)

(defsubst unicad-latin5-bulgarian-prober (start end)
  (unicad-singlebyte-prober start end
                            unicad-latin5-bulgarian-name
                            unicad-latin5-bulgarian-positive-ratio
                            unicad-latin5-bulgarian-char2order-map
                            unicad-russian-lang-model))

(defconst unicad-win1251-bulgarian-name 'windows-1251
  "The charset name for win1251-bulgarian-prober")

(defconst unicad-win1251-bulgarian-positive-ratio 0.969392)

(defsubst unicad-win1251-bulgarian-prober (start end)
  (unicad-singlebyte-prober start end
                            unicad-win1251-bulgarian-name
                            unicad-win1251-bulgarian-positive-ratio
                            unicad-win1251-bulgarian-char2order-map
                            unicad-russian-lang-model))

(defconst unicad-sjis-sb-name 'sjis
  "The charset name for sjis-sb-prober")

(defconst unicad-sjis-sb-positive-ratio 0.95)

(defsubst unicad-sjis-sb-prober (start end)
  (unicad-singlebyte-prober start end
                            unicad-sjis-sb-name
                            unicad-sjis-sb-positive-ratio
                            unicad-sjis-sb-char2order-map
                            unicad-sjis-sb-lang-model))

;;}}}

;;{{{  gb2312 freq order table
(defvar unicad-gb2312-table-size 3760)
(defvar unicad-gb2312-dist-ratio 0.9)
(defvar unicad-gb2312-char-freq-order
  [
   1671  749 1443 2364 3924 3807 2330 3921 1704 3463 2691 1511 1515  572 3191 2205
   2361  224 2558  479 1711  963 3162  440 4060 1905 2966 2947 3580 2647 3961 3842
   2204  869 4207  970 2678 5626 2944 2956 1479 4048  514 3595  588 1346 2820 3409
   249 4088 1746 1873 2047 1774  581 1813  358 1174 3590 1014 1561 4844 2245  670
   1636 3112  889 1286  953  556 2327 3060 1290 3141  613  185 3477 1367  850 3820
   1715 2428 2642 2303 2732 3041 2562 2648 3566 3946 1349  388 3098 2091 1360 3585
   152 1687 1539  738 1559   59 1232 2925 2267 1388 1249 1741 1679 2960  151 1566
   1125 1352 4271  924 4296  385 3166 4459  310 1245 2850   70 3285 2729 3534 3575
   2398 3298 3466 1960 2265  217 3647  864 1909 2084 4401 2773 1010 3269 5152  853
   3051 3121 1244 4251 1895  364 1499 1540 2313 1180 3655 2268  562  715 2417 3061
   544  336 3768 2380 1752 4075  950  280 2425 4382  183 2759 3272  333 4297 2155
   1688 2356 1444 1039 4540  736 1177 3349 2443 2368 2144 2225  565  196 1482 3406
   927 1335 4147  692  878 1311 1653 3911 3622 1378 4200 1840 2969 3149 2126 1816
   2534 1546 2393 2760  737 2494   13  447  245 2747   38 2765 2129 2589 1079  606
   360  471 3755 2890  404  848  699 1785 1236  370 2221 1023 3746 2074 2026 2023
   2388 1581 2119  812 1141 3091 2536 1519  804 2053  406 1596 1090  784  548 4414
   1806 2264 2936 1100  343 4114 5096  622 3358  743 3668 1510 1626 5020 3567 2513
   3195 4115 5627 2489 2991   24 2065 2697 1087 2719   48 1634  315   68  985 2052
   198 2239 1347 1107 1439  597 2366 2172  871 3307  919 2487 2790 1867  236 2570
   1413 3794  906 3365 3381 1701 1982 1818 1524 2924 1205  616 2586 2072 2004  575
   253 3099   32 1365 1182  197 1714 2454 1201  554 3388 3224 2748  756 2587  250
   2567 1507 1517 3529 1922 2761 2337 3416 1961 1677 2452 2238 3153  615  911 1506
   1474 2495 1265 1906 2749 3756 3280 2161  898 2714 1759 3450 2243 2444  563   26
   3286 2266 3769 3344 2707 3677  611 1402  531 1028 2871 4548 1375  261 2948  835
   1190 4134  353  840 2684 1900 3082 1435 2109 1207 1674  329 1872 2781 4055 2686
   2104  608 3318 2423 2957 2768 1108 3739 3512 3271 3985 2203 1771 3520 1418 2054
   1681 1153  225 1627 2929  162 2050 2511 3687 1954  124 1859 2431 1684 3032 2894
   585 4805 3969 2869 2704 2088 2032 2095 3656 2635 4362 2209  256  518 2042 2105
   3777 3657  643 2298 1148 1779  190  989 3544  414   11 2135 2063 2979 1471  403
   3678  126  770 1563  671 2499 3216 2877  600 1179  307 2805 4937 1268 1297 2694
   252 4032 1448 1494 1331 1394  127 2256  222 1647 1035 1481 3056 1915 1048  873
   3651  210   33 1608 2516  200 1520  415  102    0 3389 1287  817   91 3299 2940
   836 1814  549 2197 1396 1669 2987 3582 2297 2848 4528 1070  687   20 1819  121
   1552 1364 1461 1968 2617 3540 2824 2083  177  948 4938 2291  110 4549 2066  648
   3359 1755 2110 2114 4642 4845 1693 3937 3308 1257 1869 2123  208 1804 3159 2992
   2531 2549 3361 2418 1350 2347 2800 2568 1291 2036 2680   72  842 1990  212 1233
   1154 1586   75 2027 3410 4900 1823 1337 2710 2676  728 2810 1522 3026 4995  157
   755 1050 4022  710  785 1936 2194 2085 1406 2777 2400  150 1250 4049 1206  807
   1910  534  529 3309 1721 1660  274   39 2827  661 2670 1578  925 3248 3815 1094
   4278 4901 4252   41 1150 3747 2572 2227 4501 3658 4902 3813 3357 3617 2884 2258
   887  538 4187 3199 1294 2439 3042 2329 2343 2497 1255  107  543 1527  521 3478
   3568  194 5062   15  961 3870 1241 1192 2664   66 5215 3260 2111 1295 1127 2152
   3805 4135  901 1164 1976  398 1278  530 1460  748  904 1054 1966 1426   53 2909
   509  523 2279 1534  536 1019  239 1685  460 2353  673 1065 2401 3600 4298 2272
   1272 2363  284 1753 3679 4064 1695   81  815 2677 2757 2731 1386  859  500 4221
   2190 2566  757 1006 2519 2068 1166 1455  337 2654 3203 1863 1682 1914 3025 1252
   1409 1366  847  714 2834 2038 3209  964 2970 1901  885 2553 1078 1756 3049  301
   1572 3326  688 2130 1996 2429 1805 1648 2930 3421 2750 3652 3088  262 1158 1254
   389 1641 1812  526 1719  923 2073 1073 1902  468  489 4625 1140  857 2375 3070
   3319 2863  380  116 1328 2693 1161 2244  273 1212 1884 2769 3011 1775 1142  461
   3066 1200 2147 2212  790  702 2695 4222 1601 1058  434 2338 5153 3640   67 2360
   4099 2502  618 3472 1329  416 1132  830 2782 1807 2653 3211 3510 1662  192 2124
   296 3979 1739 1611 3684   23  118  324  446 1239 1225  293 2520 3814 3795 2535
   3116   17 1074  467 2692 2201  387 2922   45 1326 3055 1645 3659 2817  958  243
   1903 2320 1339 2825 1784 3289  356  576  865 2315 2381 3377 3916 1088 3122 1713
   1655  935  628 4689 1034 1327  441  800  720  894 1979 2183 1528 5289 2702 1071
   4046 3572 2399 1571 3281   79  761 1103  327  134  758 1899 1371 1615  879  442
   215 2605 2579  173 2048 2485 1057 2975 3317 1097 2253 3801 4263 1403 1650 2946
   814 4968 3487 1548 2644 1567 1285    2  295 2636   97  946 3576  832  141 4257
   3273  760 3821 3521 3156 2607  949 1024 1733 1516 1803 1920 2125 2283 2665 3180
   1501 2064 3560 2171 1592  803 3518 1416  732 3897 4258 1363 1362 2458  119 1427
   602 1525 2608 1605 1639 3175  694 3064   10  465   76 2000 4846 4208  444 3781
   1619 3353 2206 1273 3796  740 2483  320 1723 2377 3660 2619 1359 1137 1762 1724
   2345 2842 1850 1862  912  821 1866  612 2625 1735 2573 3369 1093  844   89  937
   930 1424 3564 2413 2972 1004 3046 3019 2011  711 3171 1452 4178  428  801 1943
   432  445 2811  206 4136 1472  730  349   73  397 2802 2547  998 1637 1167  789
   396 3217  154 1218  716 1120 1780 2819 4826 1931 3334 3762 2139 1215 2627  552
   3664 3628 3232 1405 2383 3111 1356 2652 3577 3320 3101 1703  640 1045 1370 1246
   4996  371 1575 2436 1621 2210  984 4033 1734 2638   16 4529  663 2755 3255 1451
   3917 2257 1253 1955 2234 1263 2951  214 1229  617  485  359 1831 1969  473 2310
   750 2058  165   80 2864 2419  361 4344 2416 2479 1134  796 3726 1266 2943  860
   2715  938  390 2734 1313 1384  248  202  877 1064 2854  522 3907  279 1602  297
   2357  395 3740  137 2075  944 4089 2584 1267 3802   62 1533 2285  178  176  780
   2440  201 3707  590  478 1560 4354 2117 1075   30   74 4643 4004 1635 1441 2745
   776 2596  238 1077 1692 1912 2844  605  499 1742 3947  241 3053  980 1749  936
   2640 4511 2582  515 1543 2162 5322 2892 2993  890 2148 1924  665 1827 3581 1032
   968 3163  339 1044 1896  270  583 1791 1720 4367 1194 3488 3669   43 2523 1657
   163 2167  290 1209 1622 3378  550  634 2508 2510  695 2634 2384 2512 1476 1414
   220 1469 2341 2138 2852 3183 2900 4939 2865 3502 1211 3680  854 3227 1299 2976
   3172  186 2998 1459  443 1067 3251 1495  321 1932 3054  909  753 1410 1828  436
   2441 1119 1587 3164 2186 1258  227  231 1425 1890 3200 3942  247  959  725 5254
   2741  577 2158 2079  929  120  174  838 2813  591 1115  417 2024   40 3240 1536
   1037  291 4151 2354  632 1298 2406 2500 3535 1825 1846 3451  205 1171  345 4238
   18 1163  811  685 2208 1217  425 1312 1508 1175 4308 2552 1033  587 1381 3059
   2984 3482  340 1316 4023 3972  792 3176  519  777 4690  918  933 4130 2981 3741
   90 3360 2911 2200 5184 4550  609 3079 2030  272 3379 2736  363 3881 1130 1447
   286  779  357 1169 3350 3137 1630 1220 2687 2391  747 1277 3688 2618 2682 2601
   1156 3196 5290 4034 3102 1689 3596 3128  874  219 2783  798  508 1843 2461  269
   1658 1776 1392 1913 2983 3287 2866 2159 2372  829 4076   46 4253 2873 1889 1894
   915 1834 1631 2181 2318  298  664 2818 3555 2735  954 3228 3117  527 3511 2173
   681 2712 3033 2247 2346 3467 1652  155 2164 3382  113 1994  450  899  494  994
   1237 2958 1875 2336 1926 3727  545 1577 1550  633 3473  204 1305 3072 2410 1956
   2471  707 2134  841 2195 2196 2663 3843 1026 4940  990 3252 4997  368 1092  437
   3212 3258 1933 1829  675 2977 2893  412  943 3723 4644 3294 3283 2230 2373 5154
   2389 2241 2661 2323 1404 2524  593  787  677 3008 1275 2059  438 2709 2609 2240
   2269 2246 1446   36 1568 1373 3892 1574 2301 1456 3962  693 2276 5216 2035 1143
   2720 1919 1797 1811 2763 4137 2597 1830 1699 1488 1198 2090  424 1694  312 3634
   3390 4179 3335 2252 1214  561 1059 3243 2295 2561  975 5155 2321 2751 3772  472
   1537 3282 3398 1047 2077 2348 2878 1323 3340 3076  690 2906   51  369  170 3541
   1060 2187 2688 3670 2541 1083 1683  928 3918  459  109 4427  599 3744 4286  143
   2101 2730 2490   82 1588 3036 2121  281 1860  477 4035 1238 2812 3020 2716 3312
   1530 2188 2055 1317  843  636 1808 1173 3495  649  181 1002  147 3641 1159 2414
   3750 2289 2795  813 3123 2610 1136 4368    5 3391 4541 2174  420  429 1728  754
   1228 2115 2219  347 2223 2733  735 1518 3003 2355 3134 1764 3948 3329 1888 2424
   1001 1234 1972 3321 3363 1672 1021 1450 1584  226  765  655 2526 3404 3244 2302
   3665  731  594 2184  319 1576  621  658 2656 4299 2099 3864 1279 2071 2598 2739
   795 3086 3699 3908 1707 2352 2402 1382 3136 2475 1465 4847 3496 3865 1085 3004
   2591 1084  213 2287 1963 3565 2250  822  793 4574 3187 1772 1789 3050  595 1484
   1959 2770 1080 2650  456  422 2996  940 3322 4328 4345 3092 2742  965 2784  739
   4124  952 1358 2498 2949 2565  332 2698 2378  660 2260 2473 4194 3856 2919  535
   1260 2651 1208 1428 1300 1949 1303 2942  433 2455 2450 1251 1946  614 1269  641
   1306 1810 2737 3078 2912  564 2365 1419 1415 1497 4460 2367 2185 1379 3005 1307
   3218 2175 1897 3063  682 1157 4040 4005 1712 1160 1941 1399  394  402 2952 1573
   1151 2986 2404  862  299 2033 1489 3006  346  171 2886 3401 1726 2932  168 2533
   47 2507 1030 3735 1145 3370 1395 1318 1579 3609 4560 2857 4116 1457 2529 1965
   504 1036 2690 2988 2405  745 5871  849 2397 2056 3081  863 2359 3857 2096   99
   1397 1769 2300 4428 1643 3455 1978 1757 3718 1440   35 4879 3742 1296 4228 2280
   160 5063 1599 2013  166  520 3479 1646 3345 3012  490 1937 1545 1264 2182 2505
   1096 1188 1369 1436 2421 1667 2792 2460 1270 2122  727 3167 2143  806 1706 1012
   1800 3037  960 2218 1882  805  139 2456 1139 1521  851 1052 3093 3089  342 2039
   744 5097 1468 1502 1585 2087  223  939  326 2140 2577  892 2481 1623 4077  982
   3708  135 2131   87 2503 3114 2326 1106  876 1616  547 2997 2831 2093 3441 4530
   4314    9 3256 4229 4148  659 1462 1986 1710 2046 2913 2231 4090 4880 5255 3392
   3274 1368 3689 4645 1477  705 3384 3635 1068 1529 2941 1458 3782 1509  100 1656
   2548  718 2339  408 1590 2780 3548 1838 4117 3719 1345 3530  717 3442 2778 3220
   2898 1892 4590 3614 3371 2043 1998 1224 3483  891  635  584 2559 3355  733 1766
   1729 1172 3789 1891 2307  781 2982 2271 1957 1580 5773 2633 2005 4195 3097 1535
   3213 1189 1934 5693 3262  586 3118 1324 1598  517 1564 2217 1868 1893 4445 3728
   2703 3139 1526 1787 1992 3882 2875 1549 1199 1056 2224 1904 2711 5098 4287  338
   1993 3129 3489 2689 1809 2815 1997  957 1855 3898 2550 3275 3057 1105 1319  627
   1505 1911 1883 3526  698 3629 3456 1833 1431  746   77 1261 2017 2296 1977 1885
   125 1334 1600  525 1798 1109 2222 1470 1945  559 2236 1186 3443 2476 1929 1411
   2411 3135 1777 3372 2621 1841 1613 3229  668 1430 1839 2643 2916  195 1989 2671
   2358 1387  629 3205 2293 5256 4439  123 1310  888 1879 4300 3021 3605 1003 1162
   3192 2910 2010  140 2395 2859   55 1082 2012 2901  662  419 2081 1438  680 2774
   4654 3912 1620 1731 1625 5035 4065 2328  512 1344  802 5443 2163 2311 2537  524
   3399   98 1155 2103 1918 2606 3925 2816 1393 2465 1504 3773 2177 3963 1478 4346
   180 1113 4655 3461 2028 1698  833 2696 1235 1322 1594 4408 3623 3013 3225 2040
   3022  541 2881  607 3632 2029 1665 1219  639 1385 1686 1099 2803 3231 1938 3188
   2858  427  676 2772 1168 2025  454 3253 2486 3556  230 1950  580  791 1991 1280
   1086 1974 2034  630  257 3338 2788 4903 1017   86 4790  966 2789 1995 1696 1131
   259 3095 4188 1308  179 1463 5257  289 4107 1248   42 3413 1725 2288  896 1947
   774 4474 4254  604 3430 4264  392 2514 2588  452  237 1408 3018  988 4531 1970
   3034 3310  540 2370 1562 1288 2990  502 4765 1147    4 1853 2708  207  294 2814
   4078 2902 2509  684   34 3105 3532 2551  644  709 2801 2344  573 1727 3573 3557
   2021 1081 3100 4315 2100 3681  199 2263 1837 2385  146 3484 1195 2776 3949  997
   1939 3973 1008 1091 1202 1962 1847 1149 4209 5444 1076  493  117 5400 2521  972
   1490 2934 1796 4542 2374 1512 2933 2657  413 2888 1135 2762 2314 2156 1355 2369
   766 2007 2527 2170 3124 2491 2593 2632 4757 2437  234 3125 3591 1898 1750 1376
   1942 3468 3138  570 2127 2145 3276 4131  962  132 1445 4196   19  941 3624 3480
   3366 1973 1374 4461 3431 2629  283 2415 2275  808 2887 3620 2112 2563 1353 3610
   955 1089 3103 1053   96   88 4097  823 3808 1583  399  292 4091 3313  421 1128
   642 4006  903 2539 1877 2082  596   29 4066 1790  722 2157  130  995 1569  769
   1485  464  513 2213  288 1923 1101 2453 4316  133  486 2445   50  625  487 2207
   57  423  481 2962  159 3729 1558  491  303  482  501  240 2837  112 3648 2392
   1783  362    8 3433 3422  610 2793 3277 1390 1284 1654   21 3823  734  367  623
   193  287  374 1009 1483  816  476  313 2255 2340 1262 2150 2899 1146 2581  782
   2116 1659 2018 1880  255 3586 3314 1110 2867 2137 2564  986 2767 5185 2006  650
   158  926  762  881 3157 2717 2362 3587  306 3690 3245 1542 3077 2427 1691 2478
   2118 2985 3490 2438  539 2305  983  129 1754  355 4201 2386  827 2923  104 1773
   2838 2771  411 2905 3919  376  767  122 1114  828 2422 1817 3506  266 3460 1007
   1609 4998  945 2612 4429 2274  726 1247 1964 2914 2199 2070 4002 4108  657 3323
   1422  579  455 2764 4737 1222 2895 1670  824 1223 1487 2525  558  861 3080  598
   2659 2515 1967  752 2583 2376 2214 4180  977  704 2464 4999 2622 4109 1210 2961
   819 1541  142 2284   44  418  457 1126 3730 4347 4626 1644 1876 3671 1864  302
   1063 5694  624  723 1984 3745 1314 1676 2488 1610 1449 3558 3569 2166 2098  409
   1011 2325 3704 2306  818 1732 1383 1824 1844 3757  999 2705 3497 1216 1423 2683
   2426 2954 2501 2726 2229 1475 2554 5064 1971 1794 1666 2014 1343  783  724  191
   2434 1354 2220 5065 1763 2752 2472 4152  131  175 2885 3434   92 1466 4920 2616
   3871 3872 3866  128 1551 1632  669 1854 3682 4691 4125 1230  188 2973 3290 1302
   1213  560 3266  917  763 3909 3249 1760  868 1958  764 1782 2097  145 2277 3774
   4462   64 1491 3062  971 2132 3606 2442  221 1226 1617  218  323 1185 3207 3147
   571  619 1473 1005 1744 2281  449 1887 2396 3685  275  375 3816 1743 3844 3731
   845 1983 2350 4210 1377  773  967 3499 3052 3743 2725 4007 1697 1022 3943 1464
   3264 2855 2722 1952 1029 2839 2467   84 4383 2215  820 1391 2015 2448 3672  377
   1948 2168  797 2545 3536 2578 2645   94 2874 1678  405 1259 3071  771  546 1315
   470 1243 3083  895 2468  981  969 2037  846 4181  653 1276 2928   14 2594  557
   3007 2474  156  902 1338 1740 2574  537 2518  973 2282 2216 2433 1928  138 2903
   1293 2631 1612  646 3457  839 2935  111  496 2191 2847  589 3186  149 3994 2060
   4031 2641 4067 3145 1870   37 3597 2136 1025 2051 3009 3383 3549 1121 1016 3261
   1301  251 2446 2599 2153  872 3246  637  334 3705  831  884  921 3065 3140 4092
   2198 1944  246 2964  108 2045 1152 1921 2308 1031  203 3173 4170 1907 3890  810
   1401 2003 1690  506  647 1242 2828 1761 1649 3208 2249 1589 3709 2931 5156 1708
   498  666 2613  834 3817 1231  184 2851 1124  883 3197 2261 3710 1765 1553 2658
   1178 2639 2351   93 1193  942 2538 2141 4402  235 1821  870 1591 2192 1709 1871
   3341 1618 4126 2595 2334  603  651   69  701  268 2662 3411 2555 1380 1606  503
   448  254 2371 2646  574 1187 2309 1770  322 2235 1292 1801  305  566 1133  229
   2067 2057  706  167  483 2002 2672 3295 1820 3561 3067  316  378 2746 3452 1112
   136 1981  507 1651 2917 1117  285 4591  182 2580 3522 1304  335 3303 1835 2504
   1795 1792 2248  674 1018 2106 2449 1857 2292 2845  976 3047 1781 2600 2727 1389
   1281   52 3152  153  265 3950  672 3485 3951 4463  430 1183  365  278 2169   27
   1407 1336 2304  209 1340 1730 2202 1852 2403 2883  979 1737 1062  631 2829 2542
   3876 2592  825 2086 2226 3048 3625  352 1417 3724  542  991  431 1351 3938 1861
   2294  826 1361 2927 3142 3503 1738  463 2462 2723  582 1916 1595 2808  400 3845
   3891 2868 3621 2254   58 2492 1123  910 2160 2614 1372 1603 1196 1072 3385 1700
   3267 1980  696  480 2430  920  799 1570 2920 1951 2041 4047 2540 1321 4223 2469
   3562 2228 1271 2602  401 2833 3351 2575 5157  907 2312 1256  410  263 3507 1582
   996  678 1849 2316 1480  908 3545 2237  703 2322  667 1826 2849 1531 2604 2999
   2407 3146 2151 2630 1786 3711  469 3542  497 3899 2409  858  837 4446 3393 1274
   786  620 1845 2001 3311  484  308 3367 1204 1815 3691 2332 1532 2557 1842 2020
   2724 1927 2333 4440  567   22 1673 2728 4475 1987 1858 1144 1597  101 1832 3601
   12  974 3783 4391  951 1412    1 3720  453 4608 4041  528 1041 1027 3230 2628
   1129  875 1051 3291 1203 2262 1069 2860 2799 2149 2615 3278  144 1758 3040   31
   475 1680  366 2685 3184  311 1642 4008 2466 5036 1593 1493 2809  216 1420 1668
   233  304 2128 3284  232 1429 1768 1040 2008 3407 2740 2967 2543  242 2133  778
   1565 2022 2620  505 2189 2756 1098 2273  372 1614  708  553 2846 2094 2278  169
   3626 2835 4161  228 2674 3165  809 1454 1309  466 1705 1095  900 3423  880 2667
   3751 5258 2317 3109 2571 4317 2766 1503 1342  866 4447 1118   63 2076  314 1881
   1348 1061  172  978 3515 1747  532  511 3970    6  601  905 2699 3300 1751  276
   1467 3725 2668   65 4239 2544 2779 2556 1604  578 2451 1802  992 2331 2624 1320
   3446  713 1513 1013  103 2786 2447 1661  886 1702  916  654 3574 2031 1556  751
   2178 2821 2179 1498 1538 2176  271  914 2251 2080 1325  638 1953 2937 3877 2432
   2754   95 3265 1716  260 1227 4083  775  106 1357 3254  426 1607  555 2480  772
   1985  244 2546  474  495 1046 2611 1851 2061   71 2089 1675 2590  742 3758 2843
   3222 1433  267 2180 2576 2826 2233 2092 3913 2435  956 1745 3075  856 2113 1116
   451    3 1988 2896 1398  993 2463 1878 2049 1341 2718 2721 2870 2108  712 2904
   4363 2753 2324  277 2872 2349 2649  384  987  435  691 3000  922  164 3939  652
   1500 1184 4153 2482 3373 2165 4848 2335 3775 3508 3154 2806 2830 1554 2102 1664
   2530 1434 2408  893 1547 2623 3447 2832 2242 2532 3169 2856 3223 2078   49 3770
   3469  462  318  656 2259 3250 3069  679 1629 2758  344 1138 1104 3120 1836 1283
   3115 2154 1437 4448  934  759 1999  794 2862 1038  533 2560 1722 2342  855 2626
   1197 1663 4476 3127   85 4240 2528   25 1111 1181 3673  407 3470 4561 2679 2713
   768 1925 2841 3986 1544 1165  932  373 1240 2146 1930 2673  721 4766  354 4333
   391 2963  187   61 3364 1442 1102  330 1940 1767  341 3809 4118  393 2496 2062
   2211  105  331  300  439  913 1332  626  379 3304 1557  328  689 3952  309 1555
   931  317 2517 3027  325  569  686 2107 3084   60 1042 1333 2794  264 3177 4014
   1628  258 3712    7 4464 1176 1043 1778  683  114 1975   78 1492  383 1886  510
   386  645 5291 2891 2069 3305 4138 3867 2939 2603 2493 1935 1066 1848 3588 1015
   1282 1289 4609  697 1453 3044 2666 3611 1856 2412   54  719 1330  568 3778 2459
   1748  788  492  551 1191 1000  488 3394 3763  282 1799  348 2016 1523 3155 2390
   1049  382 2019 1788 1170  729 2968 3523  897 3926 2785 2938 3292  350 2319 3238
   1718 1717 2655 3453 3143 4465  161 2889 2980 2009 1421   56 1908 1640 2387 2232
   1917 1874 2477 4921  148   83 3438  592 4245 2882 1822 1055  741  115 1496 1624
   381 1638 4592 1020  516 3214  458  947 4575 1432  211 1514 2926 1865 2142  189
   852 1221 1400 1486  882 2299 4036  351   28 1122  700 6479 6480 6481 6482 6483
   ])
;;}}}
;;{{{  big5 freq order
(defvar unicad-big5-table-size 5376)
(defvar unicad-big5-dist-ratio 0.75)
(defvar unicad-big5-char-freq-order
[
   1 1801 1506  255 1431  198    9   82    6 5008  177  202 3681 1256 2821  110
3814   33 3274  261   76   44 2114   16 2946 2187 1176  659 3971   26 3451 2653
1198 3972 3350 4202  410 2215  302  590  361 1964    8  204   58 4510 5009 1932
  63 5010 5011  317 1614   75  222  159 4203 2417 1480 5012 3555 3091  224 2822
3682    3   10 3973 1471   29 2787 1135 2866 1940  873  130 3275 1123  312 5013
4511 2052  507  252  682 5014  142 1915  124  206 2947   34 3556 3204   64  604
5015 2501 1977 1978  155 1991  645  641 1606 5016 3452  337   72  406 5017   80
 630  238 3205 1509  263  939 1092 2654  756 1440 1094 3453  449   69 2987  591
 179 2096  471  115 2035 1844   60   50 2988  134  806 1869  734 2036 3454  180
 995 1607  156  537 2907  688 5018  319 1305  779 2145  514 2379  298 4512  359
2502   90 2716 1338  663   11  906 1099 2553   20 2441  182  532 1716 5019  732
1376 4204 1311 1420 3206   25 2317 1056  113  399  382 1950  242 3455 2474  529
3276  475 1447 3683 5020  117   21  656  810 1297 2300 2334 3557 5021  126 4205
 706  456  150  613 4513   71 1118 2037 4206  145 3092   85  835  486 2115 1246
1426  428  727 1285 1015  800  106  623  303 1281 5022 2128 2359  347 3815  221
3558 3135 5023 1956 1153 4207   83  296 1199 3093  192  624   93 5024  822 1898
2823 3136  795 2065  991 1554 1542 1592   27   43 2867  859  139 1456  860 4514
 437  712 3974  164 2397 3137  695  211 3037 2097  195 3975 1608 3559 3560 3684
3976  234  811 2989 2098 3977 2233 1441 3561 1615 2380  668 2077 1638  305  228
1664 4515  467  415 5025  262 2099 1593  239  108  300  200 1033  512 1247 2078
5026 5027 2176 3207 3685 2682  593  845 1062 3277   88 1723 2038 3978 1951  212
 266  152  149  468 1899 4208 4516   77  187 5028 3038   37    5 2990 5029 3979
5030 5031   39 2524 4517 2908 3208 2079   55  148   74 4518  545  483 1474 1029
1665  217 1870 1531 3138 1104 2655 4209   24  172 3562  900 3980 3563 3564 4519
  32 1408 2824 1312  329  487 2360 2251 2717  784 2683    4 3039 3351 1427 1789
 188  109  499 5032 3686 1717 1790  888 1217 3040 4520 5033 3565 5034 3352 1520
3687 3981  196 1034  775 5035 5036  929 1816  249  439   38 5037 1063 5038  794
3982 1435 2301   46  178 3278 2066 5039 2381 5040  214 1709 4521  804   35  707
 324 3688 1601 2554  140  459 4210 5041 5042 1365  839  272  978 2262 2580 3456
2129 1363 3689 1423  697  100 3094   48   70 1231  495 3139 2196 5043 1294 5044
2080  462  586 1042 3279  853  256  988  185 2382 3457 1698  434 1084 5045 3458
 314 2625 2788 4522 2335 2336  569 2285  637 1817 2525  757 1162 1879 1616 3459
 287 1577 2116  768 4523 1671 2868 3566 2526 1321 3816  909 2418 5046 4211  933
3817 4212 2053 2361 1222 4524  765 2419 1322  786 4525 5047 1920 1462 1677 2909
1699 5048 4526 1424 2442 3140 3690 2600 3353 1775 1941 3460 3983 4213  309 1369
1130 2825  364 2234 1653 1299 3984 3567 3985 3986 2656  525 1085 3041  902 2001
1475  964 4527  421 1845 1415 1057 2286  940 1364 3141  376 4528 4529 1381    7
2527  983 2383  336 1710 2684 1846  321 3461  559 1131 3042 2752 1809 1132 1313
 265 1481 1858 5049  352 1203 2826 3280  167 1089  420 2827  776  792 1724 3568
4214 2443 3281 5050 4215 5051  446  229  333 2753  901 3818 1200 1557 4530 2657
1921  395 2754 2685 3819 4216 1836  125  916 3209 2626 4531 5052 5053 3820 5054
5055 5056 4532 3142 3691 1133 2555 1757 3462 1510 2318 1409 3569 5057 2146  438
2601 2910 2384 3354 1068  958 3043  461  311 2869 2686 4217 1916 3210 4218 1979
 383  750 2755 2627 4219  274  539  385 1278 1442 5058 1154 1965  384  561  210
  98 1295 2556 3570 5059 1711 2420 1482 3463 3987 2911 1257  129 5060 3821  642
 523 2789 2790 2658 5061  141 2235 1333   68  176  441  876  907 4220  603 2602
 710  171 3464  404  549   18 3143 2398 1410 3692 1666 5062 3571 4533 2912 4534
5063 2991  368 5064  146  366   99  871 3693 1543  748  807 1586 1185   22 2263
 379 3822 3211 5065 3212  505 1942 2628 1992 1382 2319 5066  380 2362  218  702
1818 1248 3465 3044 3572 3355 3282 5067 2992 3694  930 3283 3823 5068   59 5069
 585  601 4221  497 3466 1112 1314 4535 1802 5070 1223 1472 2177 5071  749 1837
 690 1900 3824 1773 3988 1476  429 1043 1791 2236 2117  917 4222  447 1086 1629
5072  556 5073 5074 2021 1654  844 1090  105  550  966 1758 2828 1008 1783  686
1095 5075 2287  793 1602 5076 3573 2603 4536 4223 2948 2302 4537 3825  980 2503
 544  353  527 4538  908 2687 2913 5077  381 2629 1943 1348 5078 1341 1252  560
3095 5079 3467 2870 5080 2054  973  886 2081  143 4539 5081 5082  157 3989  496
4224   57  840  540 2039 4540 4541 3468 2118 1445  970 2264 1748 1966 2082 4225
3144 1234 1776 3284 2829 3695  773 1206 2130 1066 2040 1326 3990 1738 1725 4226
 279 3145   51 1544 2604  423 1578 2131 2067  173 4542 1880 5083 5084 1583  264
 610 3696 4543 2444  280  154 5085 5086 5087 1739  338 1282 3096  693 2871 1411
1074 3826 2445 5088 4544 5089 5090 1240  952 2399 5091 2914 1538 2688  685 1483
4227 2475 1436  953 4228 2055 4545  671 2400   79 4229 2446 3285  608  567 2689
3469 4230 4231 1691  393 1261 1792 2401 5092 4546 5093 5094 5095 5096 1383 1672
3827 3213 1464  522 1119  661 1150  216  675 4547 3991 1432 3574  609 4548 2690
2402 5097 5098 5099 4232 3045    0 5100 2476  315  231 2447  301 3356 4549 2385
5101  233 4233 3697 1819 4550 4551 5102   96 1777 1315 2083 5103  257 5104 1810
3698 2718 1139 1820 4234 2022 1124 2164 2791 1778 2659 5105 3097  363 1655 3214
5106 2993 5107 5108 5109 3992 1567 3993  718  103 3215  849 1443  341 3357 2949
1484 5110 1712  127   67  339 4235 2403  679 1412  821 5111 5112  834  738  351
2994 2147  846  235 1497 1881  418 1993 3828 2719  186 1100 2148 2756 3575 1545
1355 2950 2872 1377  583 3994 4236 2581 2995 5113 1298 3699 1078 2557 3700 2363
  78 3829 3830  267 1289 2100 2002 1594 4237  348  369 1274 2197 2178 1838 4552
1821 2830 3701 2757 2288 2003 4553 2951 2758  144 3358  882 4554 3995 2759 3470
4555 2915 5114 4238 1726  320 5115 3996 3046  788 2996 5116 2831 1774 1327 2873
3997 2832 5117 1306 4556 2004 1700 3831 3576 2364 2660  787 2023  506  824 3702
 534  323 4557 1044 3359 2024 1901  946 3471 5118 1779 1500 1678 5119 1882 4558
 165  243 4559 3703 2528  123  683 4239  764 4560   36 3998 1793  589 2916  816
 626 1667 3047 2237 1639 1555 1622 3832 3999 5120 4000 2874 1370 1228 1933  891
2084 2917  304 4240 5121  292 2997 2720 3577  691 2101 4241 1115 4561  118  662
5122  611 1156  854 2386 1316 2875    2  386  515 2918 5123 5124 3286  868 2238
1486  855 2661  785 2216 3048 5125 1040 3216 3578 5126 3146  448 5127 1525 5128
2165 4562 5129 3833 5130 4242 2833 3579 3147  503  818 4001 3148 1568  814  676
1444  306 1749 5131 3834 1416 1030  197 1428  805 2834 1501 4563 5132 5133 5134
1994 5135 4564 5136 5137 2198   13 2792 3704 2998 3149 1229 1917 5138 3835 2132
5139 4243 4565 2404 3580 5140 2217 1511 1727 1120 5141 5142  646 3836 2448  307
5143 5144 1595 3217 5145 5146 5147 3705 1113 1356 4002 1465 2529 2530 5148  519
5149  128 2133   92 2289 1980 5150 4003 1512  342 3150 2199 5151 2793 2218 1981
3360 4244  290 1656 1317  789  827 2365 5152 3837 4566  562  581 4004 5153  401
4567 2252   94 4568 5154 1399 2794 5155 1463 2025 4569 3218 1944 5156  828 1105
4245 1262 1394 5157 4246  605 4570 5158 1784 2876 5159 2835  819 2102  578 2200
2952 5160 1502  436 3287 4247 3288 2836 4005 2919 3472 3473 5161 2721 2320 5162
5163 2337 2068   23 4571  193  826 3838 2103  699 1630 4248 3098  390 1794 1064
3581 5164 1579 3099 3100 1400 5165 4249 1839 1640 2877 5166 4572 4573  137 4250
 598 3101 1967  780  104  974 2953 5167  278  899  253  402  572  504  493 1339
5168 4006 1275 4574 2582 2558 5169 3706 3049 3102 2253  565 1334 2722  863   41
5170 5171 4575 5172 1657 2338   19  463 2760 4251  606 5173 2999 3289 1087 2085
1323 2662 3000 5174 1631 1623 1750 4252 2691 5175 2878  791 2723 2663 2339  232
2421 5176 3001 1498 5177 2664 2630  755 1366 3707 3290 3151 2026 1609  119 1918
3474  862 1026 4253 5178 4007 3839 4576 4008 4577 2265 1952 2477 5179 1125  817
4254 4255 4009 1513 1766 2041 1487 4256 3050 3291 2837 3840 3152 5180 5181 1507
5182 2692  733   40 1632 1106 2879  345 4257  841 2531  230 4578 3002 1847 3292
3475 5183 1263  986 3476 5184  735  879  254 1137  857  622 1300 1180 1388 1562
4010 4011 2954  967 2761 2665 1349  592 2134 1692 3361 3003 1995 4258 1679 4012
1902 2188 5185  739 3708 2724 1296 1290 5186 4259 2201 2202 1922 1563 2605 2559
1871 2762 3004 5187  435 5188  343 1108  596   17 1751 4579 2239 3477 3709 5189
4580  294 3582 2955 1693  477  979  281 2042 3583  643 2043 3710 2631 2795 2266
1031 2340 2135 2303 3584 4581  367 1249 2560 5190 3585 5191 4582 1283 3362 2005
 240 1762 3363 4583 4584  836 1069 3153  474 5192 2149 2532  268 3586 5193 3219
1521 1284 5194 1658 1546 4260 5195 3587 3588 5196 4261 3364 2693 1685 4262  961
1673 2632  190 2006 2203 3841 4585 4586 5197  570 2504 3711 1490 5198 4587 2633
3293 1957 4588  584 1514  396 1045 1945 5199 4589 1968 2449 5200 5201 4590 4013
 619 5202 3154 3294  215 2007 2796 2561 3220 4591 3221 4592  763 4263 3842 4593
5203 5204 1958 1767 2956 3365 3712 1174  452 1477 4594 3366 3155 5205 2838 1253
2387 2189 1091 2290 4264  492 5206  638 1169 1825 2136 1752 4014  648  926 1021
1324 4595  520 4596  997  847 1007  892 4597 3843 2267 1872 3713 2405 1785 4598
1953 2957 3103 3222 1728 4265 2044 3714 4599 2008 1701 3156 1551   30 2268 4266
5207 2027 4600 3589 5208  501 5209 4267  594 3478 2166 1822 3590 3479 3591 3223
 829 2839 4268 5210 1680 3157 1225 4269 5211 3295 4601 4270 3158 2341 5212 4602
4271 5213 4015 4016 5214 1848 2388 2606 3367 5215 4603  374 4017  652 4272 4273
 375 1140  798 5216 5217 5218 2366 4604 2269  546 1659  138 3051 2450 4605 5219
2254  612 1849  910  796 3844 1740 1371  825 3845 3846 5220 2920 2562 5221  692
 444 3052 2634  801 4606 4274 5222 1491  244 1053 3053 4275 4276  340 5223 4018
1041 3005  293 1168   87 1357 5224 1539  959 5225 2240  721  694 4277 3847  219
1478  644 1417 3368 2666 1413 1401 1335 1389 4019 5226 5227 3006 2367 3159 1826
 730 1515  184 2840   66 4607 5228 1660 2958  246 3369  378 1457  226 3480  975
4020 2959 1264 3592  674  696 5229  163 5230 1141 2422 2167  713 3593 3370 4608
4021 5231 5232 1186   15 5233 1079 1070 5234 1522 3224 3594  276 1050 2725  758
1126  653 2960 3296 5235 2342  889 3595 4022 3104 3007  903 1250 4609 4023 3481
3596 1342 1681 1718  766 3297  286   89 2961 3715 5236 1713 5237 2607 3371 3008
5238 2962 2219 3225 2880 5239 4610 2505 2533  181  387 1075 4024  731 2190 3372
5240 3298  310  313 3482 2304  770 4278   54 3054  189 4611 3105 3848 4025 5241
1230 1617 1850  355 3597 4279 4612 3373  111 4280 3716 1350 3160 3483 3055 4281
2150 3299 3598 5242 2797 4026 4027 3009  722 2009 5243 1071  247 1207 2343 2478
1378 4613 2010  864 1437 1214 4614  373 3849 1142 2220  667 4615  442 2763 2563
3850 4028 1969 4282 3300 1840  837  170 1107  934 1336 1883 5244 5245 2119 4283
2841  743 1569 5246 4616 4284  582 2389 1418 3484 5247 1803 5248  357 1395 1729
3717 3301 2423 1564 2241 5249 3106 3851 1633 4617 1114 2086 4285 1532 5250  482
2451 4618 5251 5252 1492  833 1466 5253 2726 3599 1641 2842 5254 1526 1272 3718
4286 1686 1795  416 2564 1903 1954 1804 5255 3852 2798 3853 1159 2321 5256 2881
4619 1610 1584 3056 2424 2764  443 3302 1163 3161 5257 5258 4029 5259 4287 2506
3057 4620 4030 3162 2104 1647 3600 2011 1873 4288 5260 4289  431 3485 5261  250
  97   81 4290 5262 1648 1851 1558  160  848 5263  866  740 1694 5264 2204 2843
3226 4291 4621 3719 1687  950 2479  426  469 3227 3720 3721 4031 5265 5266 1188
 424 1996  861 3601 4292 3854 2205 2694  168 1235 3602 4293 5267 2087 1674 4622
3374 3303  220 2565 1009 5268 3855  670 3010  332 1208  717 5269 5270 3603 2452
4032 3375 5271  513 5272 1209 2882 3376 3163 4623 1080 5273 5274 5275 5276 2534
3722 3604  815 1587 4033 4034 5277 3605 3486 3856 1254 4624 1328 3058 1390 4035
1741 4036 3857 4037 5278  236 3858 2453 3304 5279 5280 3723 3859 1273 3860 4625
5281  308 5282 4626  245 4627 1852 2480 1307 2583  430  715 2137 2454 5283  270
 199 2883 4038 5284 3606 2727 1753  761 1754  725 1661 1841 4628 3487 3724 5285
5286  587   14 3305  227 2608  326  480 2270  943 2765 3607  291  650 1884 5287
1702 1226  102 1547   62 3488  904 4629 3489 1164 4294 5288 5289 1224 1548 2766
 391  498 1493 5290 1386 1419 5291 2056 1177 4630  813  880 1081 2368  566 1145
4631 2291 1001 1035 2566 2609 2242  394 1286 5292 5293 2069 5294   86 1494 1730
4039  491 1588  745  897 2963  843 3377 4040 2767 2884 3306 1768  998 2221 2070
 397 1827 1195 1970 3725 3011 3378  284 5295 3861 2507 2138 2120 1904 5296 4041
2151 4042 4295 1036 3490 1905  114 2567 4296  209 1527 5297 5298 2964 2844 2635
2390 2728 3164  812 2568 5299 3307 5300 1559  737 1885 3726 1210  885   28 2695
3608 3862 5301 4297 1004 1780 4632 5302  346 1982 2222 2696 4633 3863 1742  797
1642 4043 1934 1072 1384 2152  896 4044 3308 3727 3228 2885 3609 5303 2569 1959
4634 2455 1786 5304 5305 5306 4045 4298 1005 1308 3728 4299 2729 4635 4636 1528
2610  161 1178 4300 1983  987 4637 1101 4301  631 4046 1157 3229 2425 1343 1241
1016 2243 2570  372  877 2344 2508 1160  555 1935  911 4047 5307  466 1170  169
1051 2921 2697 3729 2481 3012 1182 2012 2571 1251 2636 5308  992 2345 3491 1540
2730 1201 2071 2406 1997 2482 5309 4638  528 1923 2191 1503 1874 1570 2369 3379
3309 5310  557 1073 5311 1828 3492 2088 2271 3165 3059 3107  767 3108 2799 4639
1006 4302 4640 2346 1267 2179 3730 3230  778 4048 3231 2731 1597 2667 5312 4641
5313 3493 5314 5315 5316 3310 2698 1433 3311  131   95 1504 4049  723 4303 3166
1842 3610 2768 2192 4050 2028 2105 3731 5317 3013 4051 1218 5318 3380 3232 4052
4304 2584  248 1634 3864  912 5319 2845 3732 3060 3865  654   53 5320 3014 5321
1688 4642  777 3494 1032 4053 1425 5322  191  820 2121 2846  971 4643  931 3233
 135  664  783 3866 1998  772 2922 1936 4054 3867 4644 2923 3234  282 2732  640
1372 3495 1127  922  325 3381 5323 5324  711 2045 5325 5326 4055 2223 2800 1937
4056 3382 2224 2255 3868 2305 5327 4645 3869 1258 3312 4057 3235 2139 2965 4058
4059 5328 2225  258 3236 4646  101 1227 5329 3313 1755 5330 1391 3314 5331 2924
2057  893 5332 5333 5334 1402 4305 2347 5335 5336 3237 3611 5337 5338  878 1325
1781 2801 4647  259 1385 2585  744 1183 2272 4648 5339 4060 2509 5340  684 1024
4306 5341  472 3612 3496 1165 3315 4061 4062  322 2153  881  455 1695 1152 1340
 660  554 2154 4649 1058 4650 4307  830 1065 3383 4063 4651 1924 5342 1703 1919
5343  932 2273  122 5344 4652  947  677 5345 3870 2637  297 1906 1925 2274 4653
2322 3316 5346 5347 4308 5348 4309   84 4310  112  989 5349  547 1059 4064  701
3613 1019 5350 4311 5351 3497  942  639  457 2306 2456  993 2966  407  851  494
4654 3384  927 5352 1237 5353 2426 3385  573 4312  680  921 2925 1279 1875  285
 790 1448 1984  719 2168 5354 5355 4655 4065 4066 1649 5356 1541  563 5357 1077
5358 3386 3061 3498  511 3015 4067 4068 3733 4069 1268 2572 3387 3238 4656 4657
5359  535 1048 1276 1189 2926 2029 3167 1438 1373 2847 2967 1134 2013 5360 4313
1238 2586 3109 1259 5361  700 5362 2968 3168 3734 4314 5363 4315 1146 1876 1907
4658 2611 4070  781 2427  132 1589  203  147  273 2802 2407  898 1787 2155 4071
4072 5364 3871 2803 5365 5366 4659 4660 5367 3239 5368 1635 3872  965 5369 1805
2699 1516 3614 1121 1082 1329 3317 4073 1449 3873   65 1128 2848 2927 2769 1590
3874 5370 5371   12 2668   45  976 2587 3169 4661  517 2535 1013 1037 3240 5372
3875 2849 5373 3876 5374 3499 5375 2612  614 1999 2323 3877 3110 2733 2638 5376
2588 4316  599 1269 5377 1811 3735 5378 2700 3111  759 1060  489 1806 3388 3318
1358 5379 5380 2391 1387 1215 2639 2256  490 5381 5382 4317 1759 2392 2348 5383
4662 3878 1908 4074 2640 1807 3241 4663 3500 3319 2770 2349  874 5384 5385 3501
3736 1859   91 2928 3737 3062 3879 4664 5386 3170 4075 2669 5387 3502 1202 1403
3880 2969 2536 1517 2510 4665 3503 2511 5388 4666 5389 2701 1886 1495 1731 4076
2370 4667 5390 2030 5391 5392 4077 2702 1216  237 2589 4318 2324 4078 3881 4668
4669 2703 3615 3504  445 4670 5393 5394 5395 5396 2771   61 4079 3738 1823 4080
5397  687 2046  935  925  405 2670  703 1096 1860 2734 4671 4081 1877 1367 2704
3389  918 2106 1782 2483  334 3320 1611 1093 4672  564 3171 3505 3739 3390  945
2641 2058 4673 5398 1926  872 4319 5399 3506 2705 3112  349 4320 3740 4082 4674
3882 4321 3741 2156 4083 4675 4676 4322 4677 2408 2047  782 4084  400  251 4323
1624 5400 5401  277 3742  299 1265  476 1191 3883 2122 4324 4325 1109  205 5402
2590 1000 2157 3616 1861 5403 5404 5405 4678 5406 4679 2573  107 2484 2158 4085
3507 3172 5407 1533  541 1301  158  753 4326 2886 3617 5408 1696  370 1088 4327
4680 3618  579  327  440  162 2244  269 1938 1374 3508  968 3063   56 1396 3113
2107 3321 3391 5409 1927 2159 4681 3016 5410 3619 5411 5412 3743 4682 2485 5413
2804 5414 1650 4683 5415 2613 5416 5417 4086 2671 3392 1149 3393 4087 3884 4088
5418 1076   49 5419  951 3242 3322 3323  450 2850  920 5420 1812 2805 2371 4328
1909 1138 2372 3885 3509 5421 3243 4684 1910 1147 1518 2428 4685 3886 5422 4686
2393 2614  260 1796 3244 5423 5424 3887 3324  708 5425 3620 1704 5426 3621 1351
1618 3394 3017 1887  944 4329 3395 4330 3064 3396 4331 5427 3744  422  413 1714
3325  500 2059 2350 4332 2486 5428 1344 1911  954 5429 1668 5430 5431 4089 2409
4333 3622 3888 4334 5432 2307 1318 2512 3114  133 3115 2887 4687  629   31 2851
2706 3889 4688  850  949 4689 4090 2970 1732 2089 4335 1496 1853 5433 4091  620
3245  981 1242 3745 3397 1619 3746 1643 3326 2140 2457 1971 1719 3510 2169 5434
3246 5435 5436 3398 1829 5437 1277 4690 1565 2048 5438 1636 3623 3116 5439  869
2852  655 3890 3891 3117 4092 3018 3892 1310 3624 4691 5440 5441 5442 1733  558
4692 3747  335 1549 3065 1756 4336 3748 1946 3511 1830 1291 1192  470 2735 2108
2806  913 1054 4093 5443 1027 5444 3066 4094 4693  982 2672 3399 3173 3512 3247
3248 1947 2807 5445  571 4694 5446 1831 5447 3625 2591 1523 2429 5448 2090  984
4695 3749 1960 5449 3750  852  923 2808 3513 3751  969 1519  999 2049 2325 1705
5450 3118  615 1662  151  597 4095 2410 2326 1049  275 4696 3752 4337  568 3753
3626 2487 4338 3754 5451 2430 2275  409 3249 5452 1566 2888 3514 1002  769 2853
 194 2091 3174 3755 2226 3327 4339  628 1505 5453 5454 1763 2180 3019 4096  521
1161 2592 1788 2206 2411 4697 4097 1625 4340 4341  412   42 3119  464 5455 2642
4698 3400 1760 1571 2889 3515 2537 1219 2207 3893 2643 2141 2373 4699 4700 3328
1651 3401 3627 5456 5457 3628 2488 3516 5458 3756 5459 5460 2276 2092  460 5461
4701 5462 3020  962  588 3629  289 3250 2644 1116   52 5463 3067 1797 5464 5465
5466 1467 5467 1598 1143 3757 4342 1985 1734 1067 4702 1280 3402  465 4703 1572
 510 5468 1928 2245 1813 1644 3630 5469 4704 3758 5470 5471 2673 1573 1534 5472
5473  536 1808 1761 3517 3894 3175 2645 5474 5475 5476 4705 3518 2929 1912 2809
5477 3329 1122  377 3251 5478  360 5479 5480 4343 1529  551 5481 2060 3759 1769
2431 5482 2930 4344 3330 3120 2327 2109 2031 4706 1404  136 1468 1479  672 1171
3252 2308  271 3176 5483 2772 5484 2050  678 2736  865 1948 4707 5485 2014 4098
2971 5486 2737 2227 1397 3068 3760 4708 4709 1735 2931 3403 3631 5487 3895  509
2854 2458 2890 3896 5488 5489 3177 3178 4710 4345 2538 4711 2309 1166 1010  552
 681 1888 5490 5491 2972 2973 4099 1287 1596 1862 3179  358  453  736  175  478
1117  905 1167 1097 5492 1854 1530 5493 1706 5494 2181 3519 2292 3761 3520 3632
4346 2093 4347 5495 3404 1193 2489 4348 1458 2193 2208 1863 1889 1421 3331 2932
3069 2182 3521  595 2123 5496 4100 5497 5498 4349 1707 2646  223 3762 1359  751
3121  183 3522 5499 2810 3021  419 2374  633  704 3897 2394  241 5500 5501 5502
 838 3022 3763 2277 2773 2459 3898 1939 2051 4101 1309 3122 2246 1181 5503 1136
2209 3899 2375 1446 4350 2310 4712 5504 5505 4351 1055 2615  484 3764 5506 4102
 625 4352 2278 3405 1499 4353 4103 5507 4104 4354 3253 2279 2280 3523 5508 5509
2774  808 2616 3765 3406 4105 4355 3123 2539  526 3407 3900 4356  955 5510 1620
4357 2647 2432 5511 1429 3766 1669 1832  994  928 5512 3633 1260 5513 5514 5515
1949 2293  741 2933 1626 4358 2738 2460  867 1184  362 3408 1392 5516 5517 4106
4359 1770 1736 3254 2934 4713 4714 1929 2707 1459 1158 5518 3070 3409 2891 1292
1930 2513 2855 3767 1986 1187 2072 2015 2617 4360 5519 2574 2514 2170 3768 2490
3332 5520 3769 4715 5521 5522  666 1003 3023 1022 3634 4361 5523 4716 1814 2257
 574 3901 1603  295 1535  705 3902 4362  283  858  417 5524 5525 3255 4717 4718
3071 1220 1890 1046 2281 2461 4107 1393 1599  689 2575  388 4363 5526 2491  802
5527 2811 3903 2061 1405 2258 5528 4719 3904 2110 1052 1345 3256 1585 5529  809
5530 5531 5532  575 2739 3524  956 1552 1469 1144 2328 5533 2329 1560 2462 3635
3257 4108  616 2210 4364 3180 2183 2294 5534 1833 5535 3525 4720 5536 1319 3770
3771 1211 3636 1023 3258 1293 2812 5537 5538 5539 3905  607 2311 3906  762 2892
1439 4365 1360 4721 1485 3072 5540 4722 1038 4366 1450 2062 2648 4367 1379 4723
2593 5541 5542 4368 1352 1414 2330 2935 1172 5543 5544 3907 3908 4724 1798 1451
5545 5546 5547 5548 2936 4109 4110 2492 2351  411 4111 4112 3637 3333 3124 4725
1561 2674 1452 4113 1375 5549 5550   47 2974  316 5551 1406 1591 2937 3181 5552
1025 2142 3125 3182  354 2740  884 2228 4369 2412  508 3772  726 3638  996 2433
3639  729 5553  392 2194 1453 4114 4726 3773 5554 5555 2463 3640 2618 1675 2813
 919 2352 2975 2353 1270 4727 4115   73 5556 5557  647 5558 3259 2856 2259 1550
1346 3024 5559 1332  883 3526 5560 5561 5562 5563 3334 2775 5564 1212  831 1347
4370 4728 2331 3909 1864 3073  720 3910 4729 4730 3911 5565 4371 5566 5567 4731
5568 5569 1799 4732 3774 2619 4733 3641 1645 2376 4734 5570 2938  669 2211 2675
2434 5571 2893 5572 5573 1028 3260 5574 4372 2413 5575 2260 1353 5576 5577 4735
3183  518 5578 4116 5579 4373 1961 5580 2143 4374 5581 5582 3025 2354 2355 3912
 516 1834 1454 4117 2708 4375 4736 2229 2620 1972 1129 3642 5583 2776 5584 2976
1422  577 1470 3026 1524 3410 5585 5586  432 4376 3074 3527 5587 2594 1455 2515
2230 1973 1175 5588 1020 2741 4118 3528 4737 5589 2742 5590 1743 1361 3075 3529
2649 4119 4377 4738 2295  895  924 4378 2171  331 2247 3076  166 1627 3077 1098
5591 1232 2894 2231 3411 4739  657  403 1196 2377  542 3775 3412 1600 4379 3530
5592 4740 2777 3261  576  530 1362 4741 4742 2540 2676 3776 4120 5593  842 3913
5594 2814 2032 1014 4121  213 2709 3413  665  621 4380 5595 3777 2939 2435 5596
2436 3335 3643 3414 4743 4381 2541 4382 4744 3644 1682 4383 3531 1380 5597  724
2282  600 1670 5598 1337 1233 4745 3126 2248 5599 1621 4746 5600  651 4384 5601
1612 4385 2621 5602 2857 5603 2743 2312 3078 5604  716 2464 3079  174 1255 2710
4122 3645  548 1320 1398  728 4123 1574 5605 1891 1197 3080 4124 5606 3081 3082
3778 3646 3779  747 5607  635 4386 4747 5608 5609 5610 4387 5611 5612 4748 5613
3415 4749 2437  451 5614 3780 2542 2073 4388 2744 4389 4125 5615 1764 4750 5616
4390  350 4751 2283 2395 2493 5617 4391 4126 2249 1434 4127  488 4752  458 4392
4128 3781  771 1330 2396 3914 2576 3184 2160 2414 1553 2677 3185 4393 5618 2494
2895 2622 1720 2711 4394 3416 4753 5619 2543 4395 5620 3262 4396 2778 5621 2016
2745 5622 1155 1017 3782 3915 5623 3336 2313  201 1865 4397 1430 5624 4129 5625
5626 5627 5628 5629 4398 1604 5630  414 1866  371 2595 4754 4755 3532 2017 3127
4756 1708  960 4399  887  389 2172 1536 1663 1721 5631 2232 4130 2356 2940 1580
5632 5633 1744 4757 2544 4758 4759 5634 4760 5635 2074 5636 4761 3647 3417 2896
4400 5637 4401 2650 3418 2815  673 2712 2465  709 3533 4131 3648 4402 5638 1148
 502  634 5639 5640 1204 4762 3649 1575 4763 2623 3783 5641 3784 3128  948 3263
 121 1745 3916 1110 5642 4403 3083 2516 3027 4132 3785 1151 1771 3917 1488 4133
1987 5643 2438 3534 5644 5645 2094 5646 4404 3918 1213 1407 2816  531 2746 2545
3264 1011 1537 4764 2779 4405 3129 1061 5647 3786 3787 1867 2897 5648 2018  120
4406 4407 2063 3650 3265 2314 3919 2678 3419 1955 4765 4134 5649 3535 1047 2713
1266 5650 1368 4766 2858  649 3420 3920 2546 2747 1102 2859 2679 5651 5652 2000
5653 1111 3651 2977 5654 2495 3921 3652 2817 1855 3421 3788 5655 5656 3422 2415
2898 3337 3266 3653 5657 2577 5658 3654 2818 4135 1460  856 5659 3655 5660 2899
2978 5661 2900 3922 5662 4408  632 2517  875 3923 1697 3924 2296 5663 5664 4767
3028 1239  580 4768 4409 5665  914  936 2075 1190 4136 1039 2124 5666 5667 5668
5669 3423 1473 5670 1354 4410 3925 4769 2173 3084 4137  915 3338 4411 4412 3339
1605 1835 5671 2748  398 3656 4413 3926 4138  328 1913 2860 4139 3927 1331 4414
3029  937 4415 5672 3657 4140 4141 3424 2161 4770 3425  524  742  538 3085 1012
5673 5674 3928 2466 5675  658 1103  225 3929 5676 5677 4771 5678 4772 5679 3267
1243 5680 4142  963 2250 4773 5681 2714 3658 3186 5682 5683 2596 2332 5684 4774
5685 5686 5687 3536  957 3426 2547 2033 1931 2941 2467  870 2019 3659 1746 2780
2781 2439 2468 5688 3930 5689 3789 3130 3790 3537 3427 3791 5690 1179 3086 5691
3187 2378 4416 3792 2548 3188 3131 2749 4143 5692 3428 1556 2549 2297  977 2901
2034 4144 1205 3429 5693 1765 3430 3189 2125 1271  714 1689 4775 3538 5694 2333
3931  533 4417 3660 2184  617 5695 2469 3340 3539 2315 5696 5697 3190 5698 5699
3932 1988  618  427 2651 3540 3431 5700 5701 1244 1690 5702 2819 4418 4776 5703
3541 4777 5704 2284 1576  473 3661 4419 3432  972 5705 3662 5706 3087 5707 5708
4778 4779 5709 3793 4145 4146 5710  153 4780  356 5711 1892 2902 4420 2144  408
 803 2357 5712 3933 5713 4421 1646 2578 2518 4781 4782 3934 5714 3935 4422 5715
2416 3433  752 5716 5717 1962 3341 2979 5718  746 3030 2470 4783 4423 3794  698
4784 1893 4424 3663 2550 4785 3664 3936 5719 3191 3434 5720 1824 1302 4147 2715
3937 1974 4425 5721 4426 3192  823 1303 1288 1236 2861 3542 4148 3435  774 3938
5722 1581 4786 1304 2862 3939 4787 5723 2440 2162 1083 3268 4427 4149 4428  344
1173  288 2316  454 1683 5724 5725 1461 4788 4150 2597 5726 5727 4789  985  894
5728 3436 3193 5729 1914 2942 3795 1989 5730 2111 1975 5731 4151 5732 2579 1194
 425 5733 4790 3194 1245 3796 4429 5734 5735 2863 5736  636 4791 1856 3940  760
1800 5737 4430 2212 1508 4792 4152 1894 1684 2298 5738 5739 4793 4431 4432 2213
 479 5740 5741  832 5742 4153 2496 5743 2980 2497 3797  990 3132  627 1815 2652
4433 1582 4434 2126 2112 3543 4794 5744  799 4435 3195 5745 4795 2113 1737 3031
1018  543  754 4436 3342 1676 4796 4797 4154 4798 1489 5746 3544 5747 2624 2903
4155 5748 5749 2981 5750 5751 5752 5753 3196 4799 4800 2185 1722 5754 3269 3270
1843 3665 1715  481  365 1976 1857 5755 5756 1963 2498 4801 5757 2127 3666 3271
 433 1895 2064 2076 5758  602 2750 5759 5760 5761 5762 5763 3032 1628 3437 5764
3197 4802 4156 2904 4803 2519 5765 2551 2782 5766 5767 5768 3343 4804 2905 5769
4805 5770 2864 4806 4807 1221 2982 4157 2520 5771 5772 5773 1868 1990 5774 5775
5776 1896 5777 5778 4808 1897 4158  318 5779 2095 4159 4437 5780 5781  485 5782
 938 3941  553 2680  116 5783 3942 3667 5784 3545 2681 2783 3438 3344 2820 5785
3668 2943 4160 1747 2944 2983 5786 5787  207 5788 4809 5789 4810 2521 5790 3033
 890 3669 3943 5791 1878 3798 3439 5792 2186 2358 3440 1652 5793 5794 5795  941
2299  208 3546 4161 2020  330 4438 3944 2906 2499 3799 4439 4811 5796 5797 5798
])

;;}}}
;;{{{  jis freq table
;; JIS Freq table

;;Sampling from about 20M text materials include literature and computer technology

;; Japanese frequency table, applied to both S-JIS and EUC-JP
;;They are sorted in order.

;;******************************************************************************
;; * 128  --> 0.77094
;; * 256  --> 0.85710
;; * 512  --> 0.92635
;; * 1024 --> 0.97130
;; * 2048 --> 0.99431
;; *
;; * Idea Distribution Ratio = 0.92635 / (1-0.92635) = 12.58
;; * Random Distribution Ration = 512 / (2965+62+83+86-512) = 0.191
;; *
;; * Typical Distribution Ratio, 25% of IDR
;; *****************************************************************************/

(defvar unicad-jis-dist-ratio 3.0)
(defvar unicad-jis-table-size  4368)
(defvar unicad-jis-char-freq-order
[
  40    1    6  182  152  180  295 2127  285  381 3295 4304 3068 4606 3165 3510
3511 1822 2785 4607 1193 2226 5070 4608  171 2996 1247   18  179 5071  856 1661
1262 5072  619  127 3431 3512 3230 1899 1700  232  228 1294 1298  284  283 2041
2042 1061 1062   48   49   44   45  433  434 1040 1041  996  787 2997 1255 4305
2108 4609 1684 1648 5073 5074 5075 5076 5077 5078 3687 5079 4610 5080 3927 3928
5081 3296 3432  290 2285 1471 2187 5082 2580 2825 1303 2140 1739 1445 2691 3375
1691 3297 4306 4307 4611  452 3376 1182 2713 3688 3069 4308 5083 5084 5085 5086
5087 5088 5089 5090 5091 5092 5093 5094 5095 5096 5097 5098 5099 5100 5101 5102
5103 5104 5105 5106 5107 5108 5109 5110 5111 5112 4097 5113 5114 5115 5116 5117
5118 5119 5120 5121 5122 5123 5124 5125 5126 5127 5128 5129 5130 5131 5132 5133
5134 5135 5136 5137 5138 5139 5140 5141 5142 5143 5144 5145 5146 5147 5148 5149
5150 5151 5152 4612 5153 5154 5155 5156 5157 5158 5159 5160 5161 5162 5163 5164
5165 5166 5167 5168 5169 5170 5171 5172 5173 5174 5175 1472  598  618  820 1205
1309 1412 1858 1307 1692 5176 5177 5178 5179 5180 5181 5182 1142 1452 1234 1172
1875 2043 2149 1793 1382 2973  925 2404 1067 1241  960 1377 2935 1491  919 1217
1865 2030 1406 1499 2749 4098 5183 5184 5185 5186 5187 5188 2561 4099 3117 1804
2049 3689 4309 3513 1663 5189 3166 3118 3298 1587 1561 3433 5190 3119 1625 2998
3299 4613 1766 3690 2786 4614 5191 5192 5193 5194 2161   26 3377    2 3929   20
3691   47 4100   50   17   16   35  268   27  243   42  155   24  154   29  184
   4   91   14   92   53  396   33  289    9   37   64  620   21   39  321    5
  12   11   52   13    3  208  138    0    7   60  526  141  151 1069  181  275
1591   83  132 1475  126  331  829   15   69  160   59   22  157   55 1079  312
 109   38   23   25   10   19   79 5195   61  382 1124    8   30 5196 5197 5198
5199 5200 5201 5202 5203 5204 5205 5206   89   62   74   34 2416  112  139  196
 271  149   84  607  131  765   46   88  153  683   76  874  101  258   57   80
  32  364  121 1508  169 1547   68  235  145 2999   41  360 3027   70   63   31
  43  259  262 1383   99  533  194   66   93  846  217  192   56  106   58  565
 280  272  311  256  146   82  308   71  100  128  214  655  110  261  104 1140
  54   51   36   87   67 3070  185 2618 2936 2020   28 1066 2390 2059 5207 5208
5209 5210 5211 5212 5213 5214 5215 5216 4615 5217 5218 5219 5220 5221 5222 5223
5224 5225 5226 5227 5228 5229 5230 5231 5232 5233 5234 5235 5236 3514 5237 5238
5239 5240 5241 5242 5243 5244 2297 2031 4616 4310 3692 5245 3071 5246 3598 5247
4617 3231 3515 5248 4101 4311 4618 3808 4312 4102 5249 4103 4104 3599 5250 5251
5252 5253 5254 5255 5256 5257 5258 5259 5260 5261 5262 5263 5264 5265 5266 5267
5268 5269 5270 5271 5272 5273 5274 5275 5276 5277 5278 5279 5280 5281 5282 5283
5284 5285 5286 5287 5288 5289 5290 5291 5292 5293 5294 5295 5296 5297 5298 5299
5300 5301 5302 5303 5304 5305 5306 5307 5308 5309 5310 5311 5312 5313 5314 5315
5316 5317 5318 5319 5320 5321 5322 5323 5324 5325 5326 5327 5328 5329 5330 5331
5332 5333 5334 5335 5336 5337 5338 5339 5340 5341 5342 5343 5344 5345 5346 5347
5348 5349 5350 5351 5352 5353 5354 5355 5356 5357 5358 5359 5360 5361 5362 5363
5364 5365 5366 5367 5368 5369 5370 5371 5372 5373 5374 5375 5376 5377 5378 5379
5380 5381  363  642 2787 2878 2788 2789 2316 3232 2317 3434 2011  165 1942 3930
3931 3932 3933 5382 4619 5383 4620 5384 5385 5386 5387 5388 5389 5390 5391 5392
5393 5394 5395 5396 5397 5398 5399 5400 5401 5402 5403 5404 5405 5406 5407 5408
5409 5410 5411 5412 5413 5414 5415 5416 5417 5418 5419 5420 5421 5422 5423 5424
5425 5426 5427 5428 5429 5430 5431 5432 5433 5434 5435 5436 5437 5438 5439 5440
5441 5442 5443 5444 5445 5446 5447 5448 5449 5450 5451 5452 5453 5454 5455 5456
5457 5458 5459 5460 5461 5462 5463 5464 5465 5466 5467 5468 5469 5470 5471 5472
5473 5474 5475 5476 5477 5478 5479 5480 5481 5482 5483 5484 5485 5486 5487 5488
5489 5490 5491 5492 5493 5494 5495 5496 5497 5498 5499 5500 5501 5502 5503 5504
5505 5506 5507 5508 5509 5510 5511 5512 5513 5514 5515 5516 5517 5518 5519 5520
5521 5522 5523 5524 5525 5526 5527 5528 5529 5530 5531 5532 5533 5534 5535 5536
5537 5538 5539 5540 5541 5542 5543 5544 5545 5546 5547 5548 5549 5550 5551 5552
5553 5554 5555 5556 5557 5558 5559 5560 5561 5562 5563 5564 5565 5566 5567 5568
5569 5570 5571 5572 5573 5574 5575 5576 5577 5578 5579 5580 5581 5582 5583 5584
5585 5586 5587 5588 5589 5590 5591 5592 5593 5594 5595 5596 5597 5598 5599 5600
5601 5602 5603 5604 5605 5606 5607 5608 5609 5610 5611 5612 5613 5614 5615 5616
5617 5618 5619 5620 5621 5622 5623 5624 5625 5626 5627 5628 5629 5630 5631 5632
5633 5634 5635 5636 5637 5638 5639 5640 5641 5642 5643 5644 5645 5646 5647 5648
5649 5650 5651 5652 5653 5654 5655 5656 5657 5658 5659 5660 5661 5662 5663 5664
5665 5666 5667 5668 5669 5670 5671 5672 5673 5674 5675 5676 5677 5678 5679 5680
5681 5682 5683 5684 5685 5686 5687 5688 5689 5690 5691 5692 5693 5694 5695 5696
5697 5698 5699 5700 5701 5702 5703 5704 5705 5706 5707 5708 5709 5710 5711 5712
5713 5714 5715 5716 5717 5718 5719 5720 5721 5722 5723 5724 5725 5726 5727 5728
5729 5730 5731 5732 5733 5734 5735 5736 5737 5738 5739 5740 5741 5742 5743 5744
5745 5746 5747 5748 5749 5750 5751 5752 5753 5754 5755 5756 5757 5758 5759 5760
5761 5762 5763 5764 5765 5766 5767 5768 5769 5770 5771 5772 5773 5774 5775 5776
5777 5778 5779 5780 5781 5782 5783 5784 5785 5786 5787 5788 5789 5790 5791 5792
5793 5794 5795 5796 5797 5798 5799 5800 5801 5802 5803 5804 5805 5806 5807 5808
5809 5810 5811 5812 5813 5814 5815 5816 5817 5818 5819 5820 5821 5822 5823 5824
5825 5826 5827 5828 5829 5830 5831 5832 5833 5834 5835 5836 5837 5838 5839 5840
5841 5842 5843 5844 5845 5846 5847 5848 5849 5850 5851 5852 5853 5854 5855 5856
5857 5858 5859 5860 5861 5862 5863 5864 5865 5866 5867 5868 5869 5870 5871 5872
5873 5874 5875 5876 5877 5878 5879 5880 5881 5882 5883 5884 5885 5886 5887 5888
5889 5890 5891 5892 5893 5894 5895 5896 5897 5898 5899 5900 5901 5902 5903 5904
5905 5906 5907 5908 5909 5910 5911 5912 5913 5914 5915 5916 5917 5918 5919 5920
5921 5922 5923 5924 5925 5926 5927 5928 5929 5930 5931 5932 5933 5934 5935 5936
5937 5938 5939 5940 5941 5942 5943 5944 5945 5946 5947 5948 5949 5950 5951 5952
5953 5954 5955 5956 5957 5958 5959 5960 5961 5962 5963 5964 5965 5966 5967 5968
5969 5970 5971 5972 5973 5974 5975 5976 5977 5978 5979 5980 5981 5982 5983 5984
5985 5986 5987 5988 5989 5990 5991 5992 5993 5994 5995 5996 5997 5998 5999 6000
6001 6002 6003 6004 6005 6006 6007 6008 6009 6010 6011 6012 6013 6014 6015 6016
6017 6018 6019 6020 6021 6022 6023 6024 6025 6026 6027 6028 6029 6030 6031 6032
6033 6034 6035 6036 6037 6038 6039 6040 6041 6042 6043 6044 6045 6046 6047 6048
6049 6050 6051 6052 6053 6054 6055 6056 6057 6058 6059 6060 6061 6062 6063 6064
6065 6066 6067 6068 6069 6070 6071 6072 6073 6074 6075 6076 6077 6078 6079 6080
6081 6082 6083 6084 6085 6086 6087 6088 6089 6090 6091 6092 6093 6094 6095 6096
6097 6098 6099 6100 6101 6102 6103 6104 6105 6106 6107 6108 6109 6110 6111 6112
6113 6114 2044 2060 4621  997 1235  473 1186 4622  920 3378 6115 6116  379 1108
4313 2657 2735 3934 6117 3809  636 3233  573 1026 3693 3435 2974 3300 2298 4105
 854 2937 2463  393 2581 2417  539  752 1280 2750 2480  140 1161  440  708 1569
 665 2497 1746 1291 1523 3000  164 1603  847 1331  537 1997  486  508 1693 2418
1970 2227  878 1220  299 1030  969  652 2751  624 1137 3301 2619   65 3302 2045
1761 1859 3120 1930 3694 3516  663 1767  852  835 3695  269  767 2826 2339 1305
 896 1150  770 1616 6118  506 1502 2075 1012 2519  775 2520 2975 2340 2938 4314
3028 2086 1224 1943 2286 6119 3072 4315 2240 1273 1987 3935 1557  175  597  985
3517 2419 2521 1416 3029  585  938 1931 1007 1052 1932 1685 6120 3379 4316 4623
 804  599 3121 1333 2128 2539 1159 1554 2032 3810  687 2033 2904  952  675 1467
3436 6121 2241 1096 1786 2440 1543 1924  980 1813 2228  781 2692 1879  728 1918
3696 4624  548 1950 4625 1809 1088 1356 3303 2522 1944  502  972  373  513 2827
 586 2377 2391 1003 1976 1631 6122 2464 1084  648 1776 4626 2141  324  962 2012
2177 2076 1384  742 2178 1448 1173 1810  222  102  301  445  125 2420  662 2498
 277  200 1476 1165 1068  224 2562 1378 1446  450 1880  659  791  582 4627 2939
3936 1516 1274  555 2099 3697 1020 1389 1526 3380 1762 1723 1787 2229  412 2114
1900 2392 3518  512 2597  427 1925 2341 3122 1653 1686 2465 2499  697  330  273
 380 2162  951  832  780  991 1301 3073  965 2270 3519  668 2523 2636 1286  535
1407  518  671  957 2658 2378  267  611 2197 3030 6123  248 2299  967 1799 2356
 850 1418 3437 1876 1256 1480 2828 1718 6124 6125 1755 1664 2405 6126 4628 2879
2829  499 2179  676 4629  557 2329 2214 2090  325 3234  464  811 3001  992 2342
2481 1232 1469  303 2242  466 1070 2163  603 1777 2091 4630 2752 4631 2714  322
2659 1964 1768  481 2188 1463 2330 2857 3600 2092 3031 2421 4632 2318 2070 1849
2598 4633 1302 2254 1668 1701 2422 3811 2905 3032 3123 2046 4106 1763 1694 4634
1604  943 1724 1454  917  868 2215 1169 2940  552 1145 1800 1228 1823 1955  316
1080 2510  361 1807 2830 4107 2660 3381 1346 1423 1134 4108 6127  541 1263 1229
1148 2540  545  465 1833 2880 3438 1901 3074 2482  816 3937  713 1788 2500  122
1575  195 1451 2501 1111 6128  859  374 1225 2243 2483 4317  390 1033 3439 3075
2524 1687  266  793 1440 2599  946  779  802  507  897 1081  528 2189 1292  711
1866 1725 1167 1640  753  398 2661 1053  246  348 4318  137 1024 3440 1600 2077
2129  825 4319  698  238  521  187 2300 1157 2423 1641 1605 1464 1610 1097 2541
1260 1436  759 2255 1814 2150  705 3235  409 2563 3304  561 3033 2005 2564  726
1956 2343 3698 4109  949 3812 3813 3520 1669  653 1379 2525  881 2198  632 2256
1027  778 1074  733 1957  514 1481 2466  554 2180  702 3938 1606 1017 1398 6129
1380 3521  921  993 1313  594  449 1489 1617 1166  768 1426 1360  495 1794 3601
1177 3602 1170 4320 2344  476  425 3167 4635 3168 1424  401 2662 1171 3382 1998
1089 4110  477 3169  474 6130 1909  596 2831 1842  494  693 1051 1028 1207 3076
 606 2115  727 2790 1473 1115  743 3522  630  805 1532 4321 2021  366 1057  838
 684 1114 2142 4322 2050 1492 1892 1808 2271 3814 2424 1971 1447 1373 3305 1090
1536 3939 3523 3306 1455 2199  336  369 2331 1035  584 2393  902  718 2600 6131
2753  463 2151 1149 1611 2467  715 1308 3124 1268  343 1413 3236 1517 1347 2663
2093 3940 2022 1131 1553 2100 2941 1427 3441 2942 1323 2484 6132 1980  872 2368
2441 2943  320 2369 2116 1082  679 1933 3941 2791 3815  625 1143 2023  422 2200
3816 6133  730 1695  356 2257 1626 2301 2858 2637 1627 1778  937  883 2906 2693
3002 1769 1086  400 1063 1325 3307 2792 4111 3077  456 2345 1046  747 6134 1524
 884 1094 3383 1474 2164 1059  974 1688 2181 2258 1047  345 1665 1187  358  875
3170  305  660 3524 2190 1334 1135 3171 1540 1649 2542 1527  927  968 2793  885
1972 1850  482  500 2638 1218 1109 1085 2543 1654 2034  876   78 2287 1482 1277
 861 1675 1083 1779  724 2754  454  397 1132 1612 2332  893  672 1237  257 2259
2370  135 3384  337 2244  547  352  340  709 2485 1400  788 1138 2511  540  772
1682 2260 2272 2544 2013 1843 1902 4636 1999 1562 2288 4637 2201 1403 1533  407
 576 3308 1254 2071  978 3385  170  136 1201 3125 2664 3172 2394  213  912  873
3603 1713 2202  699 3604 3699  813 3442  493  531 1054  468 2907 1483  304  281
4112 1726 1252 2094  339 2319 2130 2639  756 1563 2944  748  571 2976 1588 2425
2715 1851 1460 2426 1528 1392 1973 3237  288 3309  685 3386  296  892 2716 2216
1570 2245  722 1747 2217  905 3238 1103 6135 1893 1441 1965  251 1805 2371 3700
2601 1919 1078   75 2182 1509 1592 1270 2640 4638 2152 6136 3310 3817  524  706
1075  292 3818 1756 2602  317   98 3173 3605 3525 1844 2218 3819 2502  814  567
 385 2908 1534 6137  534 1642 3239  797 6138 1670 1529  953 4323  188 1071  538
 178  729 3240 2109 1226 1374 2000 2357 2977  731 2468 1116 2014 2051 6139 1261
1593  803 2859 2736 3443  556  682  823 1541 6140 1369 2289 1706 2794  845  462
2603 2665 1361  387  162 2358 1740  739 1770 1720 1304 1401 3241 1049  627 1571
2427 3526 1877 3942 1852 1500  431 1910 1503  677  297 2795  286 1433 1038 1198
2290 1133 1596 4113 4639 2469 1510 1484 3943 6141 2442  108  712 4640 2372  866
3701 2755 3242 1348  834 1945 1408 3527 2395 3243 1811  824  994 1179 2110 1548
1453  790 3003  690 4324 4325 2832 2909 3820 1860 3821  225 1748  310  346 1780
2470  821 1993 2717 2796  828  877 3528 2860 2471 1702 2165 2910 2486 1789  453
 359 2291 1676   73 1164 1461 1127 3311  421  604  314 1037  589  116 2487  737
 837 1180  111  244  735 6142 2261 1861 1362  986  523  418  581 2666 3822  103
 855  503 1414 1867 2488 1091  657 1597  979  605 1316 4641 1021 2443 2078 2001
1209   96  587 2166 1032  260 1072 2153  173   94  226 3244  819 2006 4642 4114
2203  231 1744  782   97 2667  786 3387  887  391  442 2219 4326 1425 6143 2694
 633 1544 1202  483 2015  592 2052 1958 2472 1655  419  129 4327 3444 3312 1714
1257 3078 4328 1518 1098  865 1310 1019 1885 1512 1734  469 2444  148  773  436
1815 1868 1128 1055 4329 1245 2756 3445 2154 1934 1039 4643  579 1238  932 2320
 353  205  801  115 2428  944 2321 1881  399 2565 1211  678  766 3944  335 2101
1459 1781 1402 3945 2737 2131 1010  844  981 1326 1013  550 1816 1545 2620 1335
1008  371 2881  936 1419 1613 3529 1456 1395 2273 1834 2604 1317 2738 2503  416
1643 4330  806 1126  229  591 3946 1314 1981 1576 1837 1666  347 1790  977 3313
 764 2861 1853  688 2429 1920 1462   77  595  415 2002 3034  798 1192 4115 6144
2978 4331 3035 2695 2582 2072 2566  430 2430 1727  842 1396 3947 3702  613  377
 278  236 1417 3388 3314 3174  757 1869  107 3530 6145 1194  623 2262  207 1253
2167 3446 3948  492 1117 1935  536 1838 2757 1246 4332  696 2095 2406 1393 1572
3175 1782  583  190  253 1390 2230  830 3126 3389  934 3245 1703 1749 2979 1870
2545 1656 2204  869 2346 4116 3176 1817  496 1764 4644  942 1504  404 1903 1122
1580 3606 2945 1022  515  372 1735  955 2431 3036 6146 2797 1110 2302 2798  617
6147  441  762 1771 3447 3607 3608 1904  840 3037   86  939 1385  572 1370 2445
1336  114 3703  898  294  203 3315  703 1583 2274  429  961 4333 1854 1951 3390
2373 3704 4334 1318 1381  966 1911 2322 1006 1155  309  989  458 2718 1795 1372
1203  252 1689 1363 3177  517 1936  168 1490  562  193 3823 1042 4117 1835  551
 470 4645  395  489 3448 1871 1465 2583 2641  417 1493  279 1295  511 1236 1119
  72 1231 1982 1812 3004  871 1564  984 3449 1667 2696 2096 4646 2347 2833 1673
3609  695 3246 2668  807 1183 4647  890  388 2333 1801 1457 2911 1765 1477 1031
3316 3317 1278 3391 2799 2292 2526  163 3450 4335 2669 1404 1802 6148 2323 2407
1584 1728 1494 1824 1269  298  909 3318 1034 1632  375  776 1683 2061  291  210
1123  809 1249 1002 2642 3038  206 1011 2132  144  975  882 1565  342  667  754
1442 2143 1299 2303 2062  447  626 2205 1221 2739 2912 1144 1214 2206 2584  760
1715  614  950 1281 2670 2621  810  577 1287 2546 4648  242 2168  250 2643  691
 123 2644  647  313 1029  689 1357 2946 1650  216  771 1339 1306  808 2063  549
 913 1371 2913 2914 6149 1466 1092 1174 1196 1311 2605 2396 1783 1796 3079  406
2671 2117 3949 4649  487 1825 2220 6150 2915  448 2348 1073 6151 2397 1707  130
 900 1598  329  176 1959 2527 1620 6152 2275 4336 3319 1983 2191 3705 3610 2155
3706 1912 1513 1614 6153 1988  646  392 2304 1589 3320 3039 1826 1239 1352 1340
2916  505 2567 1709 1437 2408 2547  906 6154 2672  384 1458 1594 1100 1329  710
 423 3531 2064 2231 2622 1989 2673 1087 1882  333  841 3005 1296 2882 2379  580
1937 1827 1293 2585  601  574  249 1772 4118 2079 1120  645  901 1176 1690  795
2207  478 1434  516 1190 1530  761 2080  930 1264  355  435 1552  644 1791  987
 220 1364 1163 1121 1538  306 2169 1327 1222  546 2645  218  241  610 1704 3321
1984 1839 1966 2528  451 6155 2586 3707 2568  907 3178  254 2947  186 1845 4650
 745  432 1757  428 1633  888 2246 2221 2489 3611 2118 1258 1265  956 3127 1784
4337 2490  319  510  119  457 3612  274 2035 2007 4651 1409 3128  970 2758  590
2800  661 2247 4652 2008 3950 1420 1549 3080 3322 3951 1651 1375 2111  485 2491
1429 1156 6156 2548 2183 1495  831 1840 2529 2446  501 1657  307 1894 3247 1341
 666  899 2156 1539 2549 1559  886  349 2208 3081 2305 1736 3824 2170 2759 1014
1913 1386  542 1397 2948  490  368  716  362  159  282 2569 1129 1658 1288 1750
2674  276  649 2016  751 1496  658 1818 1284 1862 2209 2087 2512 3451  622 2834
 376  117 1060 2053 1208 1721 1101 1443  247 1250 3179 1792 3952 2760 2398 3953
6157 2144 3708  446 2432 1151 2570 3452 2447 2761 2835 1210 2448 3082  424 2222
1251 2449 2119 2836  504 1581 4338  602  817  857 3825 2349 2306  357 3826 1470
1883 2883  255  958  929 2917 3248  302 4653 1050 1271 1751 2307 1952 1430 2697
2719 2359  354 3180  777  158 2036 4339 1659 4340 4654 2308 2949 2248 1146 2232
3532 2720 1696 2623 3827 6158 3129 1550 2698 1485 1297 1428  637  931 2721 2145
 914 2550 2587   81 2450  612  827 2646 1242 4655 1118 2884  472 1855 3181 3533
3534  569 1353 2699 1244 1758 2588 4119 2009 2762 2171 3709 1312 1531 6159 1152
1938  134 1830  471 3710 2276 1112 1535 3323 3453 3535  982 1337 2950  488  826
 674 1058 1628 4120 2017  522 2399  211  568 1367 3454  350  293 1872 1139 3249
1399 1946 3006 1300 2360 3324  588  736 6160 2606  744  669 3536 3828 6161 1358
 199  723  848  933  851 1939 1505 1514 1338 1618 1831 4656 1634 3613  443 2740
3829  717 1947  491 1914 6162 2551 1542 4121 1025 6163 1099 1223  198 3040 2722
 370  410 1905 2589  998 1248 3182 2380  519 1449 4122 1710  947  928 1153 4341
2277  344 2624 1511  615  105  161 1212 1076 1960 3130 2054 1926 1175 1906 2473
 414 1873 2801 6164 2309  315 1319 3325  318 2018 2146 2157  963  631  223 4342
4343 2675  479 3711 1197 2625 3712 2676 2361 6165 4344 4123 6166 2451 3183 1886
2184 1674 1330 1711 1635 1506  799  219 3250 3083 3954 1677 3713 3326 2081 3614
1652 2073 4657 1147 3041 1752  643 1961  147 1974 3955 6167 1716 2037  918 3007
1994  120 1537  118  609 3184 4345  740 3455 1219  332 1615 3830 6168 1621 2980
1582  783  212  553 2350 3714 1349 2433 2082 4124  889 6169 2310 1275 1410  973
 166 1320 3456 1797 1215 3185 2885 1846 2590 2763 4658  629  822 3008  763  940
1990 2862  439 2409 1566 1240 1622  926 1282 1907 2764  654 2210 1607  327 1130
3956 1678 1623 6170 2434 2192  686  608 3831 3715  903 3957 3042 6171 2741 1522
1915 1105 1555 2552 1359  323 3251 4346 3457  738 1354 2553 2311 2334 1828 2003
3832 1753 2351 1227 6172 1887 4125 1478 6173 2410 1874 1712 1847  520 1204 2607
 264 4659  836 2677 2102  600 4660 3833 2278 3084 6174 4347 3615 1342  640  532
 543 2608 1888 2400 2591 1009 4348 1497  341 1737 3616 2723 1394  529 3252 1321
 983 4661 1515 2120  971 2592  924  287 1662 3186 4349 2700 4350 1519  908 1948
2452  156  796 1629 1486 2223 2055  694 4126 1259 1036 3392 1213 2249 2742 1889
1230 3958 1015  910  408  559 3617 4662  746  725  935 4663 3959 3009 1289  563
 867 4664 3960 1567 2981 2038 2626  988 2263 2381 4351  143 2374  704 1895 6175
1188 3716 2088  673 3085 2362 4352  484 1608 1921 2765 2918  215  904 3618 3537
 894  509  976 3043 2701 3961 4353 2837 2982  498 6176 6177 1102 3538 1332 3393
1487 1636 1637  233  245 3962  383  650  995 3044  460 1520 1206 2352  749 3327
 530  700  389 1438 1560 1773 3963 2264  719 2951 2724 3834  870 1832 1644 1000
 839 2474 3717  197 1630 3394  365 2886 3964 1285 2133  734  922  818 1106  732
 480 2083 1774 3458  923 2279 1350  221 3086   85 2233 2234 3835 1585 3010 2147
1387 1705 2382 1619 2475  133  239 2802 1991 1016 2084 2383  411 2838 1113  651
1985 1160 3328  990 1863 3087 1048 1276 2647  265 2627 1599 3253 2056  150  638
2019  656  853  326 1479  680 1439 4354 1001 1759  413 3459 3395 2492 1431  459
4355 1125 3329 2265 1953 1450 2065 2863  849  351 2678 3131 3254 3255 1104 1577
 227 1351 1645 2453 2193 1421 2887  812 2121  634   95 2435  201 2312 4665 1646
1671 2743 1601 2554 2702 2648 2280 1315 1366 2089 3132 1573 3718 3965 1729 1189
 328 2679 1077 1940 1136  558 1283  964 1195  621 2074 1199 1743 3460 3619 1896
1916 1890 3836 2952 1154 2112 1064  862  378 3011 2066 2113 2803 1568 2839 6178
3088 2919 1941 1660 2004 1992 2194  142  707 1590 1708 1624 1922 1023 1836 1233
1004 2313  789  741 3620 6179 1609 2411 1200 4127 3719 3720 4666 2057 3721  593
2840  367 2920 1878 6180 3461 1521  628 1168  692 2211 2649  300  720 2067 2571
2953 3396  959 2504 3966 3539 3462 1977  701 6181  954 1043  800  681  183 3722
1803 1730 3540 4128 2103  815 2314  174  467  230 2454 1093 2134  755 3541 3397
1141 1162 6182 1738 2039  270 3256 2513 1005 1647 2185 3837  858 1679 1897 1719
2954 2324 1806  402  670  167 4129 1498 2158 2104  750 6183  915  189 1680 1551
 455 4356 1501 2455  405 1095 2955  338 1586 1266 1819  570  641 1324  237 1556
2650 1388 3723 6184 1368 2384 1343 1978 3089 2436  879 3724  792 1191  758 3012
1411 2135 1322 4357  240 4667 1848 3725 1574 6185  420 3045 1546 1391  714 4358
1967  941 1864  863  664  426  560 1731 2680 1785 2864 1949 2363  403 3330 1415
1279 2136 1697 2335  204  721 2097 3838   90 6186 2085 2505  191 3967  124 2148
1376 1798 1178 1107 1898 1405  860 4359 1243 1272 2375 2983 1558 2456 1638  113
3621  578 1923 2609  880  386 4130  784 2186 2266 1422 2956 2172 1722  497  263
2514 1267 2412 2610  177 2703 3542  774 1927 1344  616 1432 1595 1018  172 4360
2325  911 4361  438 1468 3622  794 3968 2024 2173 1681 1829 2957  945  895 3090
 575 2212 2476  475 2401 2681  785 2744 1745 2293 2555 1975 3133 2865  394 4668
3839  635 4131  639  202 1507 2195 2766 1345 1435 2572 3726 1908 1184 1181 2457
3727 3134 4362  843 2611  437  916 4669  234  769 1884 3046 3047 3623  833 6187
1639 2250 2402 1355 1185 2010 2047  999  525 1732 1290 1488 2612  948 1578 3728
2413 2477 1216 2725 2159  334 3840 1328 3624 2921 1525 4132  564 1056  891 4363
1444 1698 2385 2251 3729 1365 2281 2235 1717 6188  864 3841 2515  444  527 2767
2922 3625  544  461 6189  566  209 2437 3398 2098 1065 2068 3331 3626 3257 2137
])
;;}}}
;;{{{  euckr freq table
;; euc-kr freq table

;;Sampling from about 20M text materials include literature and computer technology

;;******************************************************************************
;; * 128  --> 0.79
;; * 256  --> 0.92
;; * 512  --> 0.986
;; * 1024 --> 0.99944
;; * 2048 --> 0.99999
;; *
;; * Idea Distribution Ratio = 0.98653 / (1-0.98653) = 73.24
;; * Random Distribution Ration = 512 / (2350-512) = 0.279.
;; *
;; * Typical Distribution Ratio
;; *****************************************************************************/

(defvar unicad-euckr-dist-ratio  6.0)
(defvar unicad-euckr-table-size  2352)
(defvar unicad-euckr-char-freq-order
[
  13  130  120 1396  481 1719 1720  328  609  212 1721  707  400  299 1722   87
1397 1723  104  536 1117 1203 1724 1267  685 1268  508 1725 1726 1727 1728 1398
1399 1729 1730 1731  141  621  326 1057  368 1732  267  488   20 1733 1269 1734
 945 1400 1735   47  904 1270 1736 1737  773  248 1738  409  313  786  429 1739
 116  987  813 1401  683   75 1204  145 1740 1741 1742 1743   16  847  667  622
 708 1744 1745 1746  966  787  304  129 1747   60  820  123  676 1748 1749 1750
1751  617 1752  626 1753 1754 1755 1756  653 1757 1758 1759 1760 1761 1762  856
 344 1763 1764 1765 1766   89  401  418  806  905  848 1767 1768 1769  946 1205
 709 1770 1118 1771  241 1772 1773 1774 1271 1775  569 1776  999 1777 1778 1779
1780  337  751 1058   28  628  254 1781  177  906  270  349  891 1079 1782   19
1783  379 1784  315 1785  629  754 1402  559 1786  636  203 1206 1787  710  567
1788  935  814 1789 1790 1207  766  528 1791 1792 1208 1793 1794 1795 1796 1797
1403 1798 1799  533 1059 1404 1405 1156 1406  936  884 1080 1800  351 1801 1802
1803 1804 1805  801 1806 1807 1808 1119 1809 1157  714  474 1407 1810  298  899
 885 1811 1120  802 1158 1812  892 1813 1814 1408  659 1815 1816 1121 1817 1818
1819 1820 1821 1822  319 1823  594  545 1824  815  937 1209 1825 1826  573 1409
1022 1827 1210 1828 1829 1830 1831 1832 1833  556  722  807 1122 1060 1834  697
1835  900  557  715 1836 1410  540 1411  752 1159  294  597 1211  976  803  770
1412 1837 1838   39  794 1413  358 1839  371  925 1840  453  661  788  531  723
 544 1023 1081  869   91 1841  392  430  790  602 1414  677 1082  457 1415 1416
1842 1843  475  327 1024 1417  795  121 1844  733  403 1418 1845 1846 1847  300
 119  711 1212  627 1848 1272  207 1849 1850  796 1213  382 1851  519 1852 1083
 893 1853 1854 1855  367  809  487  671 1856  663 1857 1858  956  471  306  857
1859 1860 1160 1084 1861 1862 1863 1864 1865 1061 1866 1867 1868 1869 1870 1871
 282   96  574 1872  502 1085 1873 1214 1874  907 1875 1876  827  977 1419 1420
1421  268 1877 1422 1878 1879 1880  308 1881    2  537 1882 1883 1215 1884 1885
 127  791 1886 1273 1423 1887   34  336  404  643 1888  571  654  894  840 1889
   0  886 1274  122  575  260  908  938 1890 1275  410  316 1891 1892  100 1893
1894 1123   48 1161 1124 1025 1895  633  901 1276 1896 1897  115  816 1898  317
1899  694 1900  909  734 1424  572  866 1425  691   85  524 1010  543  394  841
1901 1902 1903 1026 1904 1905 1906 1907 1908 1909   30  451  651  988  310 1910
1911 1426  810 1216   93 1912 1913 1277 1217 1914  858  759   45   58  181  610
 269 1915 1916  131 1062  551  443 1000  821 1427  957  895 1086 1917 1918  375
1919  359 1920  687 1921  822 1922  293 1923 1924   40  662  118  692   29  939
 887  640  482  174 1925   69 1162  728 1428  910 1926 1278 1218 1279  386  870
 217  854 1163  823 1927 1928 1929 1930  834 1931   78 1932  859 1933 1063 1934
1935 1936 1937  438 1164  208  595 1938 1939 1940 1941 1219 1125 1942  280  888
1429 1430 1220 1431 1943 1944 1945 1946 1947 1280  150  510 1432 1948 1949 1950
1951 1952 1953 1954 1011 1087 1955 1433 1043 1956  881 1957  614  958 1064 1065
1221 1958  638 1001  860  967  896 1434  989  492  553 1281 1165 1959 1282 1002
1283 1222 1960 1961 1962 1963   36  383  228  753  247  454 1964  876  678 1965
1966 1284  126  464  490  835  136  672  529  940 1088 1435  473 1967 1968  467
  50  390  227  587  279  378  598  792  968  240  151  160  849  882 1126 1285
 639 1044  133  140  288  360  811  563 1027  561  142  523 1969 1970 1971    7
 103  296  439  407  506  634  990 1972 1973 1974 1975  645 1976 1977 1978 1979
1980 1981  236 1982 1436 1983 1984 1089  192  828  618  518 1166  333 1127 1985
 818 1223 1986 1987 1988 1989 1990 1991 1992 1993  342 1128 1286  746  842 1994
1995  560  223 1287   98    8  189  650  978 1288 1996 1437 1997   17  345  250
 423  277  234  512  226   97  289   42  167 1998  201 1999 2000  843  836  824
 532  338  783 1090  182  576  436 1438 1439  527  500 2001  947  889 2002 2003
2004 2005  262  600  314  447 2006  547 2007  693  738 1129 2008   71 1440  745
 619  688 2009  829 2010 2011  147 2012   33  948 2013 2014   74  224 2015   61
 191  918  399  637 2016 1028 1130  257  902 2017 2018 2019 2020 2021 2022 2023
2024 2025 2026  837 2027 2028 2029 2030  179  874  591   52  724  246 2031 2032
2033 2034 1167  969 2035 1289  630  605  911 1091 1168 2036 2037 2038 1441  912
2039  623 2040 2041  253 1169 1290 2042 1442  146  620  611  577  433 2043 1224
 719 1170  959  440  437  534   84  388  480 1131  159  220  198  679 2044 1012
 819 1066 1443  113 1225  194  318 1003 1029 2045 2046 2047 2048 1067 2049 2050
2051 2052 2053   59  913  112 2054  632 2055  455  144  739 1291 2056  273  681
 499 2057  448 2058 2059  760 2060 2061  970  384  169  245 1132 2062 2063  414
1444 2064 2065   41  235 2066  157  252  877  568  919  789  580 2067  725 2068
2069 1292 2070 2071 1445 2072 1446 2073 2074   55  588   66 1447  271 1092 2075
1226 2076  960 1013  372 2077 2078 2079 2080 2081 1293 2082 2083 2084 2085  850
2086 2087 2088 2089 2090  186 2091 1068  180 2092 2093 2094  109 1227  522  606
2095  867 1448 1093  991 1171  926  353 1133 2096  581 2097 2098 2099 1294 1449
1450 2100  596 1172 1014 1228 2101 1451 1295 1173 1229 2102 2103 1296 1134 1452
 949 1135 2104 2105 1094 1453 1454 1455 2106 1095 2107 2108 2109 2110 2111 2112
2113 2114 2115 2116 2117  804 2118 2119 1230 1231  805 1456  405 1136 2120 2121
2122 2123 2124  720  701 1297  992 1457  927 1004 2125 2126 2127 2128 2129 2130
  22  417 2131  303 2132  385 2133  971  520  513 2134 1174   73 1096  231  274
 962 1458  673 2135 1459 2136  152 1137 2137 2138 2139 2140 1005 1138 1460 1139
2141 2142 2143 2144   11  374  844 2145  154 1232   46 1461 2146  838  830  721
1233  106 2147   90  428  462  578  566 1175  352 2148 2149  538 1234  124 1298
2150 1462  761  565 2151  686 2152  649 2153   72  173 2154  460  415 2155 1463
2156 1235  305 2157 2158 2159 2160 2161 2162  579 2163 2164 2165 2166 2167  747
2168 2169 2170 2171 1464  669 2172 2173 2174 2175 2176 1465 2177   23  530  285
2178  335  729 2179  397 2180 2181 2182 1030 2183 2184  698 2185 2186  325 2187
2188  369 2189  799 1097 1015  348 2190 1069  680 2191  851 1466 2192 2193   10
2194  613  424 2195  979  108  449  589   27  172   81 1031   80  774  281  350
1032  525  301  582 1176 2196  674 1045 2197 2198 1467  730  762 2199 2200 2201
2202 1468 2203  993 2204 2205  266 1070  963 1140 2206 2207 2208  664 1098  972
2209 2210 2211 1177 1469 1470  871 2212 2213 2214 2215 2216 1471 2217 2218 2219
2220 2221 2222 2223 2224 2225 2226 2227 1472 1236 2228 2229 2230 2231 2232 2233
2234 2235 1299 2236 2237  200 2238  477  373 2239 2240  731  825  777 2241 2242
2243  521  486  548 2244 2245 2246 1473 1300   53  549  137  875   76  158 2247
1301 1474  469  396 1016  278  712 2248  321  442  503  767  744  941 1237 1178
1475 2249   82  178 1141 1179  973 2250 1302 2251  297 2252 2253  570 2254 2255
2256   18  450  206 2257  290  292 1142 2258  511  162   99  346  164  735 2259
1476 1477    4  554  343  798 1099 2260 1100 2261   43  171 1303  139  215 2262
2263  717  775 2264 1033  322  216 2265  831 2266  149 2267 1304 2268 2269  702
1238  135  845  347  309 2270  484 2271  878  655  238 1006 1478 2272   67 2273
 295 2274 2275  461 2276  478  942  412 2277 1034 2278 2279 2280  265 2281  541
2282 2283 2284 2285 2286   70  852 1071 2287 2288 2289 2290   21   56  509  117
 432 2291 2292  331  980  552 1101  148  284  105  393 1180 1239  755 2293  187
2294 1046 1479 2295  340 2296   63 1047  230 2297 2298 1305  763 1306  101  800
 808  494 2299 2300 2301  903 2302   37 1072   14    5 2303   79  675 2304  312
2305 2306 2307 2308 2309 1480    6 1307 2310 2311 2312    1  470   35   24  229
2313  695  210   86  778   15  784  592  779   32   77  855  964 2314  259 2315
 501  380 2316 2317   83  981  153  689 1308 1481 1482 1483 2318 2319  716 1484
2320 2321 2322 2323 2324 2325 1485 2326 2327  128   57   68  261 1048  211  170
1240   31 2328   51  435  742 2329 2330 2331  635 2332  264  456 2333 2334 2335
 425 2336 1486  143  507  263  943 2337  363  920 1487  256 1488 1102  243  601
1489 2338 2339 2340 2341 2342 2343 2344  861 2345 2346 2347 2348 2349 2350  395
2351 1490 1491   62  535  166  225 2352 2353  668  419 1241  138  604  928 2354
1181 2355 1492 1493 2356 2357 2358 1143 2359  696 2360  387  307 1309  682  476
2361 2362  332   12  222  156 2363  232 2364  641  276  656  517 1494 1495 1035
 416  736 1496 2365 1017  586 2366 2367 2368 1497 2369  242 2370 2371 2372 1498
2373  965  713 2374 2375 2376 2377  740  982 1499  944 1500 1007 2378 2379 1310
1501 2380 2381 2382  785  329 2383 2384 1502 2385 2386 2387  932 2388 1503 2389
2390 2391 2392 1242 2393 2394 2395 2396 2397  994  950 2398 2399 2400 2401 1504
1311 2402 2403 2404 2405 1049  749 2406 2407  853  718 1144 1312 2408 1182 1505
2409 2410  255  516  479  564  550  214 1506 1507 1313  413  239  444  339 1145
1036 1508 1509 1314 1037 1510 1315 2411 1511 2412 2413 2414  176  703  497  624
 593  921  302 2415  341  165 1103 1512 2416 1513 2417 2418 2419  376 2420  700
2421 2422 2423  258  768 1316 2424 1183 2425  995  608 2426 2427 2428 2429  221
2430 2431 2432 2433 2434 2435 2436 2437  195  323  726  188  897  983 1317  377
 644 1050  879 2438  452 2439 2440 2441 2442 2443 2444  914 2445 2446 2447 2448
 915  489 2449 1514 1184 2450 2451  515   64  427  495 2452  583 2453  483  485
1038  562  213 1515  748  666 2454 2455 2456 2457  334 2458  780  996 1008  705
1243 2459 2460 2461 2462 2463  114 2464  493 1146  366  163 1516  961 1104 2465
 291 2466 1318 1105 2467 1517  365 2468  355  951 1244 2469 1319 2470  631 2471
2472  218 1320  364  320  756 1518 1519 1321 1520 1322 2473 2474 2475 2476  997
2477 2478 2479 2480  665 1185 2481  916 1521 2482 2483 2484  584  684 2485 2486
 797 2487 1051 1186 2488 2489 2490 1522 2491 2492  370 2493 1039 1187   65 2494
 434  205  463 1188 2495  125  812  391  402  826  699  286  398  155  781  771
 585 2496  590  505 1073 2497  599  244  219  917 1018  952  646 1523 2498 1323
2499 2500   49  984  354  741 2501  625 2502 1324 2503 1019  190  357  757  491
  95  782  868 2504 2505 2506 2507 2508 2509  134 1524 1074  422 1525  898 2510
 161 2511 2512 2513 2514  769 2515 1526 2516 2517  411 1325 2518  472 1527 2519
2520 2521 2522 2523 2524  985 2525 2526 2527 2528 2529 2530  764 2531 1245 2532
2533   25  204  311 2534  496 2535 1052 2536 2537 2538 2539 2540 2541 2542  199
 704  504  468  758  657 1528  196   44  839 1246  272  750 2543  765  862 2544
2545 1326 2546  132  615  933 2547  732 2548 2549 2550 1189 1529 2551  283 1247
1053  607  929 2552 2553 2554  930  183  872  616 1040 1147 2555 1148 1020  441
 249 1075 2556 2557 2558  466  743 2559 2560 2561   92  514  426  420  526 2562
2563 2564 2565 2566 2567 2568  185 2569 2570 2571 2572  776 1530  658 2573  362
2574  361  922 1076  793 2575 2576 2577 2578 2579 2580 1531  251 2581 2582 2583
2584 1532   54  612  237 1327 2585 2586  275  408  647  111 2587 1533 1106  465
   3  458    9   38 2588  107  110  890  209   26  737  498 2589 1534 2590  431
 202   88 1535  356  287 1107  660 1149 2591  381 1536  986 1150  445 1248 1151
 974 2592 2593  846 2594  446  953  184 1249 1250  727 2595  923  193  883 2596
2597 2598  102  324  539  817 2599  421 1041 2600  832 2601   94  175  197  406
2602  459 2603 2604 2605 2606 2607  330  555 2608 2609 2610  706 1108  389 2611
2612 2613 2614  233 2615  833  558  931  954 1251 2616 2617 1537  546 2618 2619
1009 2620 2621 2622 1538  690 1328 2623  955 2624 1539 2625 2626  772 2627 2628
2629 2630 2631  924  648  863  603 2632 2633  934 1540  864  865 2634  642 1042
 670 1190 2635 2636 2637 2638  168 2639  652  873  542 1054 1541 2640 2641 2642
])
;;}}}
;;{{{  euctw freq table
;; EUCTW frequency table
;; Converted from big5 work
;; by Taiwan's Mandarin Promotion Council
;; <http://www.edu.tw:81/mandr/>


;;******************************************************************************
;; * 128  --> 0.42261
;; * 256  --> 0.57851
;; * 512  --> 0.74851
;; * 1024 --> 0.89384
;; * 2048 --> 0.97583
;; *
;; * Idea Distribution Ratio = 0.74851/(1-0.74851) =2.98
;; * Random Distribution Ration = 512/(5401-512)=0.105
;; *
;; * Typical Distribution Ratio about 25% of Ideal one, still much higher than RDR
;; *****************************************************************************/

(defvar unicad-euctw-dist-ratio 0.75)
(defvar unicad-euctw-table-size  8102)
(defvar unicad-euctw-char-freq-order
[
   1 1800 1506  255 1431  198    9   82    6 7310  177  202 3615 1256 2808  110
3735   33 3241  261   76   44 2113   16 2931 2184 1176  659 3868   26 3404 2643
1198 3869 3313 4060  410 2211  302  590  361 1963    8  204   58 4296 7311 1931
  63 7312 7313  317 1614   75  222  159 4061 2412 1480 7314 3500 3068  224 2809
3616    3   10 3870 1471   29 2774 1135 2852 1939  873  130 3242 1123  312 7315
4297 2051  507  252  682 7316  142 1914  124  206 2932   34 3501 3173   64  604
7317 2494 1976 1977  155 1990  645  641 1606 7318 3405  337   72  406 7319   80
 630  238 3174 1509  263  939 1092 2644  756 1440 1094 3406  449   69 2969  591
 179 2095  471  115 2034 1843   60   50 2970  134  806 1868  734 2035 3407  180
 995 1607  156  537 2893  688 7320  319 1305  779 2144  514 2374  298 4298  359
2495   90 2707 1338  663   11  906 1099 2545   20 2436  182  532 1716 7321  732
1376 4062 1311 1420 3175   25 2312 1056  113  399  382 1949  242 3408 2467  529
3243  475 1447 3617 7322  117   21  656  810 1297 2295 2329 3502 7323  126 4063
 706  456  150  613 4299   71 1118 2036 4064  145 3069   85  835  486 2114 1246
1426  428  727 1285 1015  800  106  623  303 1281 7324 2127 2354  347 3736  221
3503 3110 7325 1955 1153 4065   83  296 1199 3070  192  624   93 7326  822 1897
2810 3111  795 2064  991 1554 1542 1592   27   43 2853  859  139 1456  860 4300
 437  712 3871  164 2392 3112  695  211 3017 2096  195 3872 1608 3504 3505 3618
3873  234  811 2971 2097 3874 2229 1441 3506 1615 2375  668 2076 1638  305  228
1664 4301  467  415 7327  262 2098 1593  239  108  300  200 1033  512 1247 2077
7328 7329 2173 3176 3619 2673  593  845 1062 3244   88 1723 2037 3875 1950  212
 266  152  149  468 1898 4066 4302   77  187 7330 3018   37    5 2972 7331 3876
7332 7333   39 2517 4303 2894 3177 2078   55  148   74 4304  545  483 1474 1029
1665  217 1869 1531 3113 1104 2645 4067   24  172 3507  900 3877 3508 3509 4305
  32 1408 2811 1312  329  487 2355 2247 2708  784 2674    4 3019 3314 1427 1788
 188  109  499 7334 3620 1717 1789  888 1217 3020 4306 7335 3510 7336 3315 1520
3621 3878  196 1034  775 7337 7338  929 1815  249  439   38 7339 1063 7340  794
3879 1435 2296   46  178 3245 2065 7341 2376 7342  214 1709 4307  804   35  707
 324 3622 1601 2546  140  459 4068 7343 7344 1365  839  272  978 2257 2572 3409
2128 1363 3623 1423  697  100 3071   48   70 1231  495 3114 2193 7345 1294 7346
2079  462  586 1042 3246  853  256  988  185 2377 3410 1698  434 1084 7347 3411
 314 2615 2775 4308 2330 2331  569 2280  637 1816 2518  757 1162 1878 1616 3412
 287 1577 2115  768 4309 1671 2854 3511 2519 1321 3737  909 2413 7348 4069  933
3738 7349 2052 2356 1222 4310  765 2414 1322  786 4311 7350 1919 1462 1677 2895
1699 7351 4312 1424 2437 3115 3624 2590 3316 1774 1940 3413 3880 4070  309 1369
1130 2812  364 2230 1653 1299 3881 3512 3882 3883 2646  525 1085 3021  902 2000
1475  964 4313  421 1844 1415 1057 2281  940 1364 3116  376 4314 4315 1381    7
2520  983 2378  336 1710 2675 1845  321 3414  559 1131 3022 2742 1808 1132 1313
 265 1481 1857 7352  352 1203 2813 3247  167 1089  420 2814  776  792 1724 3513
4071 2438 3248 7353 4072 7354  446  229  333 2743  901 3739 1200 1557 4316 2647
1920  395 2744 2676 3740 4073 1835  125  916 3178 2616 4317 7355 7356 3741 7357
7358 7359 4318 3117 3625 1133 2547 1757 3415 1510 2313 1409 3514 7360 2145  438
2591 2896 2379 3317 1068  958 3023  461  311 2855 2677 4074 1915 3179 4075 1978
 383  750 2745 2617 4076  274  539  385 1278 1442 7361 1154 1964  384  561  210
  98 1295 2548 3515 7362 1711 2415 1482 3416 3884 2897 1257  129 7363 3742  642
 523 2776 2777 2648 7364  141 2231 1333   68  176  441  876  907 4077  603 2592
 710  171 3417  404  549   18 3118 2393 1410 3626 1666 7365 3516 4319 2898 4320
7366 2973  368 7367  146  366   99  871 3627 1543  748  807 1586 1185   22 2258
 379 3743 3180 7368 3181  505 1941 2618 1991 1382 2314 7369  380 2357  218  702
1817 1248 3418 3024 3517 3318 3249 7370 2974 3628  930 3250 3744 7371   59 7372
 585  601 4078  497 3419 1112 1314 4321 1801 7373 1223 1472 2174 7374  749 1836
 690 1899 3745 1772 3885 1476  429 1043 1790 2232 2116  917 4079  447 1086 1629
7375  556 7376 7377 2020 1654  844 1090  105  550  966 1758 2815 1008 1782  686
1095 7378 2282  793 1602 7379 3518 2593 4322 4080 2933 2297 4323 3746  980 2496
 544  353  527 4324  908 2678 2899 7380  381 2619 1942 1348 7381 1341 1252  560
3072 7382 3420 2856 7383 2053  973  886 2080  143 4325 7384 7385  157 3886  496
4081   57  840  540 2038 4326 4327 3421 2117 1445  970 2259 1748 1965 2081 4082
3119 1234 1775 3251 2816 3629  773 1206 2129 1066 2039 1326 3887 1738 1725 4083
 279 3120   51 1544 2594  423 1578 2130 2066  173 4328 1879 7386 7387 1583  264
 610 3630 4329 2439  280  154 7388 7389 7390 1739  338 1282 3073  693 2857 1411
1074 3747 2440 7391 4330 7392 7393 1240  952 2394 7394 2900 1538 2679  685 1483
4084 2468 1436  953 4085 2054 4331  671 2395   79 4086 2441 3252  608  567 2680
3422 4087 4088 1691  393 1261 1791 2396 7395 4332 7396 7397 7398 7399 1383 1672
3748 3182 1464  522 1119  661 1150  216  675 4333 3888 1432 3519  609 4334 2681
2397 7400 7401 7402 4089 3025    0 7403 2469  315  231 2442  301 3319 4335 2380
7404  233 4090 3631 1818 4336 4337 7405   96 1776 1315 2082 7406  257 7407 1809
3632 2709 1139 1819 4091 2021 1124 2163 2778 1777 2649 7408 3074  363 1655 3183
7409 2975 7410 7411 7412 3889 1567 3890  718  103 3184  849 1443  341 3320 2934
1484 7413 1712  127   67  339 4092 2398  679 1412  821 7414 7415  834  738  351
2976 2146  846  235 1497 1880  418 1992 3749 2710  186 1100 2147 2746 3520 1545
1355 2935 2858 1377  583 3891 4093 2573 2977 7416 1298 3633 1078 2549 3634 2358
  78 3750 3751  267 1289 2099 2001 1594 4094  348  369 1274 2194 2175 1837 4338
1820 2817 3635 2747 2283 2002 4339 2936 2748  144 3321  882 4340 3892 2749 3423
4341 2901 7417 4095 1726  320 7418 3893 3026  788 2978 7419 2818 1773 1327 2859
3894 2819 7420 1306 4342 2003 1700 3752 3521 2359 2650  787 2022  506  824 3636
 534  323 4343 1044 3322 2023 1900  946 3424 7421 1778 1500 1678 7422 1881 4344
 165  243 4345 3637 2521  123  683 4096  764 4346   36 3895 1792  589 2902  816
 626 1667 3027 2233 1639 1555 1622 3753 3896 7423 3897 2860 1370 1228 1932  891
2083 2903  304 4097 7424  292 2979 2711 3522  691 2100 4098 1115 4347  118  662
7425  611 1156  854 2381 1316 2861    2  386  515 2904 7426 7427 3253  868 2234
1486  855 2651  785 2212 3028 7428 1040 3185 3523 7429 3121  448 7430 1525 7431
2164 4348 7432 3754 7433 4099 2820 3524 3122  503  818 3898 3123 1568  814  676
1444  306 1749 7434 3755 1416 1030  197 1428  805 2821 1501 4349 7435 7436 7437
1993 7438 4350 7439 7440 2195   13 2779 3638 2980 3124 1229 1916 7441 3756 2131
7442 4100 4351 2399 3525 7443 2213 1511 1727 1120 7444 7445  646 3757 2443  307
7446 7447 1595 3186 7448 7449 7450 3639 1113 1356 3899 1465 2522 2523 7451  519
7452  128 2132   92 2284 1979 7453 3900 1512  342 3125 2196 7454 2780 2214 1980
3323 7455  290 1656 1317  789  827 2360 7456 3758 4352  562  581 3901 7457  401
4353 2248   94 4354 1399 2781 7458 1463 2024 4355 3187 1943 7459  828 1105 4101
1262 1394 7460 4102  605 4356 7461 1783 2862 7462 2822  819 2101  578 2197 2937
7463 1502  436 3254 4103 3255 2823 3902 2905 3425 3426 7464 2712 2315 7465 7466
2332 2067   23 4357  193  826 3759 2102  699 1630 4104 3075  390 1793 1064 3526
7467 1579 3076 3077 1400 7468 4105 1838 1640 2863 7469 4358 4359  137 4106  598
3078 1966  780  104  974 2938 7470  278  899  253  402  572  504  493 1339 7471
3903 1275 4360 2574 2550 7472 3640 3029 3079 2249  565 1334 2713  863   41 7473
7474 4361 7475 1657 2333   19  463 2750 4107  606 7476 2981 3256 1087 2084 1323
2652 2982 7477 1631 1623 1750 4108 2682 7478 2864  791 2714 2653 2334  232 2416
7479 2983 1498 7480 2654 2620  755 1366 3641 3257 3126 2025 1609  119 1917 3427
 862 1026 4109 7481 3904 3760 4362 3905 4363 2260 1951 2470 7482 1125  817 4110
4111 3906 1513 1766 2040 1487 4112 3030 3258 2824 3761 3127 7483 7484 1507 7485
2683  733   40 1632 1106 2865  345 4113  841 2524  230 4364 2984 1846 3259 3428
7486 1263  986 3429 7487  735  879  254 1137  857  622 1300 1180 1388 1562 3907
3908 2939  967 2751 2655 1349  592 2133 1692 3324 2985 1994 4114 1679 3909 1901
2185 7488  739 3642 2715 1296 1290 7489 4115 2198 2199 1921 1563 2595 2551 1870
2752 2986 7490  435 7491  343 1108  596   17 1751 4365 2235 3430 3643 7492 4366
 294 3527 2940 1693  477  979  281 2041 3528  643 2042 3644 2621 2782 2261 1031
2335 2134 2298 3529 4367  367 1249 2552 7493 3530 7494 4368 1283 3325 2004  240
1762 3326 4369 4370  836 1069 3128  474 7495 2148 2525  268 3531 7496 3188 1521
1284 7497 1658 1546 4116 7498 3532 3533 7499 4117 3327 2684 1685 4118  961 1673
2622  190 2005 2200 3762 4371 4372 7500  570 2497 3645 1490 7501 4373 2623 3260
1956 4374  584 1514  396 1045 1944 7502 4375 1967 2444 7503 7504 4376 3910  619
7505 3129 3261  215 2006 2783 2553 3189 4377 3190 4378  763 4119 3763 4379 7506
7507 1957 1767 2941 3328 3646 1174  452 1477 4380 3329 3130 7508 2825 1253 2382
2186 1091 2285 4120  492 7509  638 1169 1824 2135 1752 3911  648  926 1021 1324
4381  520 4382  997  847 1007  892 4383 3764 2262 1871 3647 7510 2400 1784 4384
1952 2942 3080 3191 1728 4121 2043 3648 4385 2007 1701 3131 1551   30 2263 4122
7511 2026 4386 3534 7512  501 7513 4123  594 3431 2165 1821 3535 3432 3536 3192
 829 2826 4124 7514 1680 3132 1225 4125 7515 3262 4387 4126 3133 2336 7516 4388
4127 7517 3912 3913 7518 1847 2383 2596 3330 7519 4389  374 3914  652 4128 4129
 375 1140  798 7520 7521 7522 2361 4390 2264  546 1659  138 3031 2445 4391 7523
2250  612 1848  910  796 3765 1740 1371  825 3766 3767 7524 2906 2554 7525  692
 444 3032 2624  801 4392 4130 7526 1491  244 1053 3033 4131 4132  340 7527 3915
1041 2987  293 1168   87 1357 7528 1539  959 7529 2236  721  694 4133 3768  219
1478  644 1417 3331 2656 1413 1401 1335 1389 3916 7530 7531 2988 2362 3134 1825
 730 1515  184 2827   66 4393 7532 1660 2943  246 3332  378 1457  226 3433  975
3917 2944 1264 3537  674  696 7533  163 7534 1141 2417 2166  713 3538 3333 4394
3918 7535 7536 1186   15 7537 1079 1070 7538 1522 3193 3539  276 1050 2716  758
1126  653 2945 3263 7539 2337  889 3540 3919 3081 2989  903 1250 4395 3920 3434
3541 1342 1681 1718  766 3264  286   89 2946 3649 7540 1713 7541 2597 3334 2990
7542 2947 2215 3194 2866 7543 4396 2498 2526  181  387 1075 3921  731 2187 3335
7544 3265  310  313 3435 2299  770 4134   54 3034  189 4397 3082 3769 3922 7545
1230 1617 1849  355 3542 4135 4398 3336  111 4136 3650 1350 3135 3436 3035 4137
2149 3266 3543 7546 2784 3923 3924 2991  722 2008 7547 1071  247 1207 2338 2471
1378 4399 2009  864 1437 1214 4400  373 3770 1142 2216  667 4401  442 2753 2555
3771 3925 1968 4138 3267 1839  837  170 1107  934 1336 1882 7548 7549 2118 4139
2828  743 1569 7550 4402 4140  582 2384 1418 3437 7551 1802 7552  357 1395 1729
3651 3268 2418 1564 2237 7553 3083 3772 1633 4403 1114 2085 4141 1532 7554  482
2446 4404 7555 7556 1492  833 1466 7557 2717 3544 1641 2829 7558 1526 1272 3652
4142 1686 1794  416 2556 1902 1953 1803 7559 3773 2785 3774 1159 2316 7560 2867
4405 1610 1584 3036 2419 2754  443 3269 1163 3136 7561 7562 3926 7563 4143 2499
3037 4406 3927 3137 2103 1647 3545 2010 1872 4144 7564 4145  431 3438 7565  250
  97   81 4146 7566 1648 1850 1558  160  848 7567  866  740 1694 7568 2201 2830
3195 4147 4407 3653 1687  950 2472  426  469 3196 3654 3655 3928 7569 7570 1188
 424 1995  861 3546 4148 3775 2202 2685  168 1235 3547 4149 7571 2086 1674 4408
3337 3270  220 2557 1009 7572 3776  670 2992  332 1208  717 7573 7574 3548 2447
3929 3338 7575  513 7576 1209 2868 3339 3138 4409 1080 7577 7578 7579 7580 2527
3656 3549  815 1587 3930 3931 7581 3550 3439 3777 1254 4410 1328 3038 1390 3932
1741 3933 3778 3934 7582  236 3779 2448 3271 7583 7584 3657 3780 1273 3781 4411
7585  308 7586 4412  245 4413 1851 2473 1307 2575  430  715 2136 2449 7587  270
 199 2869 3935 7588 3551 2718 1753  761 1754  725 1661 1840 4414 3440 3658 7589
7590  587   14 3272  227 2598  326  480 2265  943 2755 3552  291  650 1883 7591
1702 1226  102 1547   62 3441  904 4415 3442 1164 4150 7592 7593 1224 1548 2756
 391  498 1493 7594 1386 1419 7595 2055 1177 4416  813  880 1081 2363  566 1145
4417 2286 1001 1035 2558 2599 2238  394 1286 7596 7597 2068 7598   86 1494 1730
3936  491 1588  745  897 2948  843 3340 3937 2757 2870 3273 1768  998 2217 2069
 397 1826 1195 1969 3659 2993 3341  284 7599 3782 2500 2137 2119 1903 7600 3938
2150 3939 4151 1036 3443 1904  114 2559 4152  209 1527 7601 7602 2949 2831 2625
2385 2719 3139  812 2560 7603 3274 7604 1559  737 1884 3660 1210  885   28 2686
3553 3783 7605 4153 1004 1779 4418 7606  346 1981 2218 2687 4419 3784 1742  797
1642 3940 1933 1072 1384 2151  896 3941 3275 3661 3197 2871 3554 7607 2561 1958
4420 2450 1785 7608 7609 7610 3942 4154 1005 1308 3662 4155 2720 4421 4422 1528
2600  161 1178 4156 1982  987 4423 1101 4157  631 3943 1157 3198 2420 1343 1241
1016 2239 2562  372  877 2339 2501 1160  555 1934  911 3944 7611  466 1170  169
1051 2907 2688 3663 2474 2994 1182 2011 2563 1251 2626 7612  992 2340 3444 1540
2721 1201 2070 2401 1996 2475 7613 4424  528 1922 2188 1503 1873 1570 2364 3342
3276 7614  557 1073 7615 1827 3445 2087 2266 3140 3039 3084  767 3085 2786 4425
1006 4158 4426 2341 1267 2176 3664 3199  778 3945 3200 2722 1597 2657 7616 4427
7617 3446 7618 7619 7620 3277 2689 1433 3278  131   95 1504 3946  723 4159 3141
1841 3555 2758 2189 3947 2027 2104 3665 7621 2995 3948 1218 7622 3343 3201 3949
4160 2576  248 1634 3785  912 7623 2832 3666 3040 3786  654   53 7624 2996 7625
1688 4428  777 3447 1032 3950 1425 7626  191  820 2120 2833  971 4429  931 3202
 135  664  783 3787 1997  772 2908 1935 3951 3788 4430 2909 3203  282 2723  640
1372 3448 1127  922  325 3344 7627 7628  711 2044 7629 7630 3952 2219 2787 1936
3953 3345 2220 2251 3789 2300 7631 4431 3790 1258 3279 3954 3204 2138 2950 3955
3956 7632 2221  258 3205 4432  101 1227 7633 3280 1755 7634 1391 3281 7635 2910
2056  893 7636 7637 7638 1402 4161 2342 7639 7640 3206 3556 7641 7642  878 1325
1780 2788 4433  259 1385 2577  744 1183 2267 4434 7643 3957 2502 7644  684 1024
4162 7645  472 3557 3449 1165 3282 3958 3959  322 2152  881  455 1695 1152 1340
 660  554 2153 4435 1058 4436 4163  830 1065 3346 3960 4437 1923 7646 1703 1918
7647  932 2268  122 7648 4438  947  677 7649 3791 2627  297 1905 1924 2269 4439
2317 3283 7650 7651 4164 7652 4165   84 4166  112  989 7653  547 1059 3961  701
3558 1019 7654 4167 7655 3450  942  639  457 2301 2451  993 2951  407  851  494
4440 3347  927 7656 1237 7657 2421 3348  573 4168  680  921 2911 1279 1874  285
 790 1448 1983  719 2167 7658 7659 4441 3962 3963 1649 7660 1541  563 7661 1077
7662 3349 3041 3451  511 2997 3964 3965 3667 3966 1268 2564 3350 3207 4442 4443
7663  535 1048 1276 1189 2912 2028 3142 1438 1373 2834 2952 1134 2012 7664 4169
1238 2578 3086 1259 7665  700 7666 2953 3143 3668 4170 7667 4171 1146 1875 1906
4444 2601 3967  781 2422  132 1589  203  147  273 2789 2402  898 1786 2154 3968
3969 7668 3792 2790 7669 7670 4445 4446 7671 3208 7672 1635 3793  965 7673 1804
2690 1516 3559 1121 1082 1329 3284 3970 1449 3794   65 1128 2835 2913 2759 1590
3795 7674 7675   12 2658   45  976 2579 3144 4447  517 2528 1013 1037 3209 7676
3796 2836 7677 3797 7678 3452 7679 2602  614 1998 2318 3798 3087 2724 2628 7680
2580 4172  599 1269 7681 1810 3669 7682 2691 3088  759 1060  489 1805 3351 3285
1358 7683 7684 2386 1387 1215 2629 2252  490 7685 7686 4173 1759 2387 2343 7687
4448 3799 1907 3971 2630 1806 3210 4449 3453 3286 2760 2344  874 7688 7689 3454
3670 1858   91 2914 3671 3042 3800 4450 7690 3145 3972 2659 7691 3455 1202 1403
3801 2954 2529 1517 2503 4451 3456 2504 7692 4452 7693 2692 1885 1495 1731 3973
2365 4453 7694 2029 7695 7696 3974 2693 1216  237 2581 4174 2319 3975 3802 4454
4455 2694 3560 3457  445 4456 7697 7698 7699 7700 2761   61 3976 3672 1822 3977
7701  687 2045  935  925  405 2660  703 1096 1859 2725 4457 3978 1876 1367 2695
3352  918 2105 1781 2476  334 3287 1611 1093 4458  564 3146 3458 3673 3353  945
2631 2057 4459 7702 1925  872 4175 7703 3459 2696 3089  349 4176 3674 3979 4460
3803 4177 3675 2155 3980 4461 4462 4178 4463 2403 2046  782 3981  400  251 4179
1624 7704 7705  277 3676  299 1265  476 1191 3804 2121 4180 4181 1109  205 7706
2582 1000 2156 3561 1860 7707 7708 7709 4464 7710 4465 2565  107 2477 2157 3982
3460 3147 7711 1533  541 1301  158  753 4182 2872 3562 7712 1696  370 1088 4183
4466 3563  579  327  440  162 2240  269 1937 1374 3461  968 3043   56 1396 3090
2106 3288 3354 7713 1926 2158 4467 2998 7714 3564 7715 7716 3677 4468 2478 7717
2791 7718 1650 4469 7719 2603 7720 7721 3983 2661 3355 1149 3356 3984 3805 3985
7722 1076   49 7723  951 3211 3289 3290  450 2837  920 7724 1811 2792 2366 4184
1908 1138 2367 3806 3462 7725 3212 4470 1909 1147 1518 2423 4471 3807 7726 4472
2388 2604  260 1795 3213 7727 7728 3808 3291  708 7729 3565 1704 7730 3566 1351
1618 3357 2999 1886  944 4185 3358 4186 3044 3359 4187 7731 3678  422  413 1714
3292  500 2058 2345 4188 2479 7732 1344 1910  954 7733 1668 7734 7735 3986 2404
4189 3567 3809 4190 7736 2302 1318 2505 3091  133 3092 2873 4473  629   31 2838
2697 3810 4474  850  949 4475 3987 2955 1732 2088 4191 1496 1852 7737 3988  620
3214  981 1242 3679 3360 1619 3680 1643 3293 2139 2452 1970 1719 3463 2168 7738
3215 7739 7740 3361 1828 7741 1277 4476 1565 2047 7742 1636 3568 3093 7743  869
2839  655 3811 3812 3094 3989 3000 3813 1310 3569 4477 7744 7745 7746 1733  558
4478 3681  335 1549 3045 1756 4192 3682 1945 3464 1829 1291 1192  470 2726 2107
2793  913 1054 3990 7747 1027 7748 3046 3991 4479  982 2662 3362 3148 3465 3216
3217 1946 2794 7749  571 4480 7750 1830 7751 3570 2583 1523 2424 7752 2089  984
4481 3683 1959 7753 3684  852  923 2795 3466 3685  969 1519  999 2048 2320 1705
7754 3095  615 1662  151  597 3992 2405 2321 1049  275 4482 3686 4193  568 3687
3571 2480 4194 3688 7755 2425 2270  409 3218 7756 1566 2874 3467 1002  769 2840
 194 2090 3149 3689 2222 3294 4195  628 1505 7757 7758 1763 2177 3001 3993  521
1161 2584 1787 2203 2406 4483 3994 1625 4196 4197  412   42 3096  464 7759 2632
4484 3363 1760 1571 2875 3468 2530 1219 2204 3814 2633 2140 2368 4485 4486 3295
1651 3364 3572 7760 7761 3573 2481 3469 7762 3690 7763 7764 2271 2091  460 7765
4487 7766 3002  962  588 3574  289 3219 2634 1116   52 7767 3047 1796 7768 7769
7770 1467 7771 1598 1143 3691 4198 1984 1734 1067 4488 1280 3365  465 4489 1572
 510 7772 1927 2241 1812 1644 3575 7773 4490 3692 7774 7775 2663 1573 1534 7776
7777 4199  536 1807 1761 3470 3815 3150 2635 7778 7779 7780 4491 3471 2915 1911
2796 7781 3296 1122  377 3220 7782  360 7783 7784 4200 1529  551 7785 2059 3693
1769 2426 7786 2916 4201 3297 3097 2322 2108 2030 4492 1404  136 1468 1479  672
1171 3221 2303  271 3151 7787 2762 7788 2049  678 2727  865 1947 4493 7789 2013
3995 2956 7790 2728 2223 1397 3048 3694 4494 4495 1735 2917 3366 3576 7791 3816
 509 2841 2453 2876 3817 7792 7793 3152 3153 4496 4202 2531 4497 2304 1166 1010
 552  681 1887 7794 7795 2957 2958 3996 1287 1596 1861 3154  358  453  736  175
 478 1117  905 1167 1097 7796 1853 1530 7797 1706 7798 2178 3472 2287 3695 3473
3577 4203 2092 4204 7799 3367 1193 2482 4205 1458 2190 2205 1862 1888 1421 3298
2918 3049 2179 3474  595 2122 7800 3997 7801 7802 4206 1707 2636  223 3696 1359
 751 3098  183 3475 7803 2797 3003  419 2369  633  704 3818 2389  241 7804 7805
7806  838 3004 3697 2272 2763 2454 3819 1938 2050 3998 1309 3099 2242 1181 7807
1136 2206 3820 2370 1446 4207 2305 4498 7808 7809 4208 1055 2605  484 3698 7810
3999  625 4209 2273 3368 1499 4210 4000 7811 4001 4211 3222 2274 2275 3476 7812
7813 2764  808 2606 3699 3369 4002 4212 3100 2532  526 3370 3821 4213  955 7814
1620 4214 2637 2427 7815 1429 3700 1669 1831  994  928 7816 3578 1260 7817 7818
7819 1948 2288  741 2919 1626 4215 2729 2455  867 1184  362 3371 1392 7820 7821
4003 4216 1770 1736 3223 2920 4499 4500 1928 2698 1459 1158 7822 3050 3372 2877
1292 1929 2506 2842 3701 1985 1187 2071 2014 2607 4217 7823 2566 2507 2169 3702
2483 3299 7824 3703 4501 7825 7826  666 1003 3005 1022 3579 4218 7827 4502 1813
2253  574 3822 1603  295 1535  705 3823 4219  283  858  417 7828 7829 3224 4503
4504 3051 1220 1889 1046 2276 2456 4004 1393 1599  689 2567  388 4220 7830 2484
 802 7831 2798 3824 2060 1405 2254 7832 4505 3825 2109 1052 1345 3225 1585 7833
 809 7834 7835 7836  575 2730 3477  956 1552 1469 1144 2323 7837 2324 1560 2457
3580 3226 4005  616 2207 3155 2180 2289 7838 1832 7839 3478 4506 7840 1319 3704
3705 1211 3581 1023 3227 1293 2799 7841 7842 7843 3826  607 2306 3827  762 2878
1439 4221 1360 7844 1485 3052 7845 4507 1038 4222 1450 2061 2638 4223 1379 4508
2585 7846 7847 4224 1352 1414 2325 2921 1172 7848 7849 3828 3829 7850 1797 1451
7851 7852 7853 7854 2922 4006 4007 2485 2346  411 4008 4009 3582 3300 3101 4509
1561 2664 1452 4010 1375 7855 7856   47 2959  316 7857 1406 1591 2923 3156 7858
1025 2141 3102 3157  354 2731  884 2224 4225 2407  508 3706  726 3583  996 2428
3584  729 7859  392 2191 1453 4011 4510 3707 7860 7861 2458 3585 2608 1675 2800
 919 2347 2960 2348 1270 4511 4012   73 7862 7863  647 7864 3228 2843 2255 1550
1346 3006 7865 1332  883 3479 7866 7867 7868 7869 3301 2765 7870 1212  831 1347
4226 4512 2326 3830 1863 3053  720 3831 4513 4514 3832 7871 4227 7872 7873 4515
7874 7875 1798 4516 3708 2609 4517 3586 1645 2371 7876 7877 2924  669 2208 2665
2429 7878 2879 7879 7880 1028 3229 7881 4228 2408 7882 2256 1353 7883 7884 4518
3158  518 7885 4013 7886 4229 1960 7887 2142 4230 7888 7889 3007 2349 2350 3833
 516 1833 1454 4014 2699 4231 4519 2225 2610 1971 1129 3587 7890 2766 7891 2961
1422  577 1470 3008 1524 3373 7892 7893  432 4232 3054 3480 7894 2586 1455 2508
2226 1972 1175 7895 1020 2732 4015 3481 4520 7896 2733 7897 1743 1361 3055 3482
2639 4016 4233 4521 2290  895  924 4234 2170  331 2243 3056  166 1627 3057 1098
7898 1232 2880 2227 3374 4522  657  403 1196 2372  542 3709 3375 1600 4235 3483
7899 4523 2767 3230  576  530 1362 7900 4524 2533 2666 3710 4017 7901  842 3834
7902 2801 2031 1014 4018  213 2700 3376  665  621 4236 7903 3711 2925 2430 7904
2431 3302 3588 3377 7905 4237 2534 4238 4525 3589 1682 4239 3484 1380 7906  724
2277  600 1670 7907 1337 1233 4526 3103 2244 7908 1621 4527 7909  651 4240 7910
1612 4241 2611 7911 2844 7912 2734 2307 3058 7913  716 2459 3059  174 1255 2701
4019 3590  548 1320 1398  728 4020 1574 7914 1890 1197 3060 4021 7915 3061 3062
3712 3591 3713  747 7916  635 4242 4528 7917 7918 7919 4243 7920 7921 4529 7922
3378 4530 2432  451 7923 3714 2535 2072 4244 2735 4245 4022 7924 1764 4531 7925
4246  350 7926 2278 2390 2486 7927 4247 4023 2245 1434 4024  488 4532  458 4248
4025 3715  771 1330 2391 3835 2568 3159 2159 2409 1553 2667 3160 4249 7928 2487
2881 2612 1720 2702 4250 3379 4533 7929 2536 4251 7930 3231 4252 2768 7931 2015
2736 7932 1155 1017 3716 3836 7933 3303 2308  201 1864 4253 1430 7934 4026 7935
7936 7937 7938 7939 4254 1604 7940  414 1865  371 2587 4534 4535 3485 2016 3104
4536 1708  960 4255  887  389 2171 1536 1663 1721 7941 2228 4027 2351 2926 1580
7942 7943 7944 1744 7945 2537 4537 4538 7946 4539 7947 2073 7948 7949 3592 3380
2882 4256 7950 4257 2640 3381 2802  673 2703 2460  709 3486 4028 3593 4258 7951
1148  502  634 7952 7953 1204 4540 3594 1575 4541 2613 3717 7954 3718 3105  948
3232  121 1745 3837 1110 7955 4259 3063 2509 3009 4029 3719 1151 1771 3838 1488
4030 1986 7956 2433 3487 7957 7958 2093 7959 4260 3839 1213 1407 2803  531 2737
2538 3233 1011 1537 7960 2769 4261 3106 1061 7961 3720 3721 1866 2883 7962 2017
 120 4262 4263 2062 3595 3234 2309 3840 2668 3382 1954 4542 7963 7964 3488 1047
2704 1266 7965 1368 4543 2845  649 3383 3841 2539 2738 1102 2846 2669 7966 7967
1999 7968 1111 3596 2962 7969 2488 3842 3597 2804 1854 3384 3722 7970 7971 3385
2410 2884 3304 3235 3598 7972 2569 7973 3599 2805 4031 1460  856 7974 3600 7975
2885 2963 7976 2886 3843 7977 4264  632 2510  875 3844 1697 3845 2291 7978 7979
4544 3010 1239  580 4545 4265 7980  914  936 2074 1190 4032 1039 2123 7981 7982
7983 3386 1473 7984 1354 4266 3846 7985 2172 3064 4033  915 3305 4267 4268 3306
1605 1834 7986 2739  398 3601 4269 3847 4034  328 1912 2847 4035 3848 1331 4270
3011  937 4271 7987 3602 4036 4037 3387 2160 4546 3388  524  742  538 3065 1012
7988 7989 3849 2461 7990  658 1103  225 3850 7991 7992 4547 7993 4548 7994 3236
1243 7995 4038  963 2246 4549 7996 2705 3603 3161 7997 7998 2588 2327 7999 4550
8000 8001 8002 3489 3307  957 3389 2540 2032 1930 2927 2462  870 2018 3604 1746
2770 2771 2434 2463 8003 3851 8004 3723 3107 3724 3490 3390 3725 8005 1179 3066
8006 3162 2373 4272 3726 2541 3163 3108 2740 4039 8007 3391 1556 2542 2292  977
2887 2033 4040 1205 3392 8008 1765 3393 3164 2124 1271 1689  714 4551 3491 8009
2328 3852  533 4273 3605 2181  617 8010 2464 3308 3492 2310 8011 8012 3165 8013
8014 3853 1987  618  427 2641 3493 3394 8015 8016 1244 1690 8017 2806 4274 4552
8018 3494 8019 8020 2279 1576  473 3606 4275 3395  972 8021 3607 8022 3067 8023
8024 4553 4554 8025 3727 4041 4042 8026  153 4555  356 8027 1891 2888 4276 2143
 408  803 2352 8028 3854 8029 4277 1646 2570 2511 4556 4557 3855 8030 3856 4278
8031 2411 3396  752 8032 8033 1961 2964 8034  746 3012 2465 8035 4279 3728  698
4558 1892 4280 3608 2543 4559 3609 3857 8036 3166 3397 8037 1823 1302 4043 2706
3858 1973 4281 8038 4282 3167  823 1303 1288 1236 2848 3495 4044 3398  774 3859
8039 1581 4560 1304 2849 3860 4561 8040 2435 2161 1083 3237 4283 4045 4284  344
1173  288 2311  454 1683 8041 8042 1461 4562 4046 2589 8043 8044 4563  985  894
8045 3399 3168 8046 1913 2928 3729 1988 8047 2110 1974 8048 4047 8049 2571 1194
 425 8050 4564 3169 1245 3730 4285 8051 8052 2850 8053  636 4565 1855 3861  760
1799 8054 4286 2209 1508 4566 4048 1893 1684 2293 8055 8056 8057 4287 4288 2210
 479 8058 8059  832 8060 4049 2489 8061 2965 2490 3731  990 3109  627 1814 2642
4289 1582 4290 2125 2111 3496 4567 8062  799 4291 3170 8063 4568 2112 1737 3013
1018  543  754 4292 3309 1676 4569 4570 4050 8064 1489 8065 3497 8066 2614 2889
4051 8067 8068 2966 8069 8070 8071 8072 3171 4571 4572 2182 1722 8073 3238 3239
1842 3610 1715  481  365 1975 1856 8074 8075 1962 2491 4573 8076 2126 3611 3240
 433 1894 2063 2075 8077  602 2741 8078 8079 8080 8081 8082 3014 1628 3400 8083
3172 4574 4052 2890 4575 2512 8084 2544 2772 8085 8086 8087 3310 4576 2891 8088
4577 8089 2851 4578 4579 1221 2967 4053 2513 8090 8091 8092 1867 1989 8093 8094
8095 1895 8096 8097 4580 1896 4054  318 8098 2094 4055 4293 8099 8100  485 8101
 938 3862  553 2670  116 8102 3863 3612 8103 3498 2671 2773 3401 3311 2807 8104
3613 2929 4056 1747 2930 2968 8105 8106  207 8107 8108 2672 4581 2514 8109 3015
 890 3614 3864 8110 1877 3732 3402 8111 2183 2353 3403 1652 8112 8113 8114  941
2294  208 3499 4057 2019  330 4294 3865 2892 2492 3733 4295 8115 8116 8117 8118
])
;;}}}

;;{{{  Dist Table functions
(defsubst unicad-dist-table-reset (dist-table)
  (setcar dist-table 0)
  (setcdr dist-table 0))

(defalias 'unicad-dist-table-total-chars 'car)
(defalias 'unicad-dist-table-freq-chars 'cdr)

(defsubst unicad-dist-table-total-chars++ (dist-table)
  (setcar dist-table (1+ (car dist-table))))
(defsubst unicad-dist-table-freq-chars++ (dist-table)
  (setcdr dist-table (1+ (cdr dist-table))))

(defun unicad-dist-table-get-confidence (dist-table dist-ratio size &optional prefer)
  (let ((Confidence 0.0)
        (total-chars (unicad-dist-table-total-chars dist-table))
        (freq-chars (unicad-dist-table-freq-chars dist-table)))
    (cond
     ((and (> size unicad-minimum-size-threshold)
           (or (<= total-chars (- (/ unicad-minimum-size-threshold 2) 4))    ;; this was `0'
               (< freq-chars unicad-minimum-data-threshold)))
      (setq Confidence unicad--sure-no))
     ((<= total-chars 0)
      (setq Confidence unicad--sure-no))
     ((/= total-chars freq-chars)
      (setq Confidence (min (/ freq-chars (* (- total-chars freq-chars) dist-ratio))
                            unicad--sure-yes)))
     (t (setq Confidence unicad--sure-yes)))))

;;}}}

;;{{{  State Machine functions
(defvar unicad-sm-coding-state nil
  "Init state:
   ((mState . eStart)
    (mCharLen . 0)
    (mBytePos . 0))")

(defconst unicad--eStart 0)
(defconst unicad--eError 1)
(defconst unicad--eItsMe 2)

(defsubst unicad-sm-reset ()
  (setq unicad-sm-coding-state `((mState . ,unicad--eStart)
                               (mCharLen . 0)
                               (mBytePos . 0))))

(defsubst unicad-sm-set (name value)
  (setcdr (assoc name unicad-sm-coding-state) value))

(defsubst unicad-sm-get (name)
  "To get mState mCharlen or mByte"
  (cdr (assoc name unicad-sm-coding-state)))

(defun unicad-next-state (ch model)
  "Verificate multibyte codings by class-table and state-table"
  (let ((current-state (unicad-sm-get 'mState))
        (current-bytepos (unicad-sm-get 'mBytePos))
        (byteCls (aref (cdr (assoc 'classTable model))  ch))
        (next-charlen (unicad-sm-get 'mCharLen))
        next-state next-bytepos)
    (when (= current-state unicad--eStart)
      (setq next-bytepos 0)
      (setq next-charlen (aref (cdr (assoc 'charLenTable model)) byteCls)))
    (setq next-state
          (aref (cdr (assoc 'stateTable model))
                (+ (* current-state (cdr (assoc 'classFactor model)))
                   byteCls))
          next-bytepos (1+ current-bytepos)
          unicad-sm-coding-state `((mState . ,next-state)
                                 (mCharLen . ,next-charlen)
                                 (mBytePos . ,next-bytepos)))
    next-state))

;;}}}
;;{{{  utf8 state machine

(defvar unicad-utf8-class-table
  `[
    1 1 1 1 1 1 1 1         ;; 00 - 07 0
    1 1 1 1 1 1 0 0         ;; 08 - 0f 1
    1 1 1 1 1 1 1 1         ;; 10 - 17 2
    1 1 1 1 1 1 1 1        ;; 1 1 1 0 1 1 1 1        ;; 18 - 1f
    1 1 1 1 1 1 1 1         ;; 20 - 27 4
    1 1 1 1 1 1 1 1         ;; 28 - 2f 5
    1 1 1 1 1 1 1 1         ;; 30 - 37 6
    1 1 1 1 1 1 1 1         ;; 38 - 3f 7
    1 1 1 1 1 1 1 1         ;; 40 - 47 8
    1 1 1 1 1 1 1 1         ;; 48 - 4f 9
    1 1 1 1 1 1 1 1         ;; 50 - 57 10
    1 1 1 1 1 1 1 1         ;; 58 - 5f 11
    1 1 1 1 1 1 1 1         ;; 60 - 67 12
    1 1 1 1 1 1 1 1         ;; 68 - 6f 13
    1 1 1 1 1 1 1 1         ;; 70 - 77 14
    1 1 1 1 1 1 1 1         ;; 78 - 7f 15
    2 2 2 2 3 3 3 3         ;; 80 - 87 16
    4 4 4 4 4 4 4 4         ;; 88 - 8f 17
    4 4 4 4 4 4 4 4         ;; 90 - 97 18
    4 4 4 4 4 4 4 4         ;; 98 - 9f 19
    5 5 5 5 5 5 5 5         ;; a0 - a7 20
    5 5 5 5 5 5 5 5         ;; a8 - af 21
    5 5 5 5 5 5 5 5         ;; b0 - b7 22
    5 5 5 5 5 5 5 5         ;; b8 - bf 23
    0 0 6 6 6 6 6 6         ;; c0 - c7 24
    6 6 6 6 6 6 6 6         ;; c8 - cf 25
    6 6 6 6 6 6 6 6         ;; d0 - d7 26
    6 6 6 6 6 6 6 6         ;; d8 - df 27
    7 8 8 8 8 8 8 8         ;; e0 - e7 28
    8 8 8 8 8 9 8 8         ;; e8 - ef 29
    10 11 11 11 11 11 11 11 ;; f0 - f7 30
    12 13 13 13 14 15 0 0   ;; f8 - ff 31
    ])


(defvar unicad-utf8-state-table
  (let ((eStart 0)
        (eError 1)
        (eItsMe 2))
    (vector
     eError eStart eError eError eError eError 12     10  ;; 00-07  0
     9      11     8      7      6      5      4      3   ;; 08-0f  1
     eError eError eError eError eError eError eError eError ;; 10-17  2
     eError eError eError eError eError eError eError eError ;; 18-1f  3
     eItsMe eItsMe eItsMe eItsMe eItsMe eItsMe eItsMe eItsMe ;; 20-27  4
     eItsMe eItsMe eItsMe eItsMe eItsMe eItsMe eItsMe eItsMe ;; 28-2f  5
     eError eError      5      5      5      5 eError eError ;; 30-37  6
     eError eError eError eError eError eError eError eError ;; 38-3f  7
     eError eError eError      5      5      5 eError eError ;; 40-47  8
     eError eError eError eError eError eError eError eError ;; 48-4f  9
     eError eError      7      7      7      7 eError eError ;; 50-57  10
     eError eError eError eError eError eError eError eError ;; 58-5f  11
     eError eError eError eError      7      7 eError eError ;; 60-67  12
     eError eError eError eError eError eError eError eError ;; 68-6f  13
     eError eError      9      9      9      9 eError eError ;; 70-77  14
     eError eError eError eError eError eError eError eError ;; 78-7f  15
     eError eError eError eError eError      9 eError eError ;; 80-87  16
     eError eError eError eError eError eError eError eError ;; 88-8f  17
     eError eError     12     12     12     12 eError eError ;; 90-97  18
     eError eError eError eError eError eError eError eError ;; 98-9f  19
     eError eError eError eError eError     12 eError eError ;; a0-a7  20
     eError eError eError eError eError eError eError eError ;; a8-af  21
     eError eError     12     12     12 eError eError eError ;; b0-b7  22
     eError eError eError eError eError eError eError eError ;; b8-bf  23
     eError eError eStart eStart eStart eStart eError eError ;; c0-c7  24
     eError eError eError eError eError eError eError eError ;; c8-cf  25
     )))

(defvar unicad-utf8-charlen-table
;; 0  1  2  3  4  5  6  7
  [0  1  0  0  0  0  2  3
   3  3  4  4  5  5  6  6 ])

(defvar unicad-utf8-sm-model
  (list
   (cons 'classTable unicad-utf8-class-table)
   (cons 'classFactor 16)
   (cons 'stateTable unicad-utf8-state-table)
   (cons 'charLenTable unicad-utf8-charlen-table)
   (cons 'name 'utf-8)))

;;}}}
;;{{{  gb18030 state machine

(defvar unicad-gb18030-class-table
  [
   1 1 1 1 1 1 1 1        ;; 00 - 07
   1 1 1 1 1 1 0 0        ;; 08 - 0f
   1 1 1 1 1 1 1 1        ;; 10 - 17
   1 1 1 1 1 1 1 1        ;; 18 - 2f allow esc as legal value
   1 1 1 1 1 1 1 1        ;; 20 - 27
   1 1 1 1 1 1 1 1        ;; 28 - 2f
   3 3 3 3 3 3 3 3        ;; 30 - 37
   3 3 1 1 1 1 1 1        ;; 38 - 3f
   2 2 2 2 2 2 2 2        ;; 40 - 47
   2 2 2 2 2 2 2 2        ;; 48 - 4f
   2 2 2 2 2 2 2 2        ;; 50 - 57
   2 2 2 2 2 2 2 2        ;; 58 - 5f
   2 2 2 2 2 2 2 2        ;; 60 - 67
   2 2 2 2 2 2 2 2        ;; 68 - 6f
   2 2 2 2 2 2 2 2        ;; 70 - 77
   2 2 2 2 2 2 2 4        ;; 78 - 7f
   5 6 6 6 6 6 6 6        ;; 80 - 87
   6 6 6 6 6 6 6 6        ;; 88 - 8f
   6 6 6 6 6 6 6 6        ;; 90 - 97
   6 6 6 6 6 6 6 6        ;; 98 - 9f
   6 6 6 6 6 6 6 6        ;; a0 - a7
   6 6 6 6 6 6 6 6        ;; a8 - af
   6 6 6 6 6 6 6 6        ;; b0 - b7
   6 6 6 6 6 6 6 6        ;; b8 - bf
   6 6 6 6 6 6 6 6        ;; c0 - c7
   6 6 6 6 6 6 6 6        ;; c8 - cf
   6 6 6 6 6 6 6 6        ;; d0 - d7
   6 6 6 6 6 6 6 6        ;; d8 - df
   6 6 6 6 6 6 6 6        ;; e0 - e7
   6 6 6 6 6 6 6 6        ;; e8 - ef
   6 6 6 6 6 6 6 6        ;; f0 - f7
   6 6 6 6 6 6 6 0        ;; f8 - ff
   ])

(defvar unicad-gb18030-state-table
  (let ((eStart 0)
        (eError 1)
        (eItsMe 2))
    (vector
     ;;   0      1      2      3      4      5      6
     eError eStart eStart eStart eStart eStart      3
     eError eError eError eError eError eError eError
     eItsMe eItsMe eItsMe eItsMe eItsMe eItsMe eItsMe
     eError eError eStart      4 eError eStart eStart
     eError eError eError eError eError eError      5
     eError eError eError eItsMe eError eError eError)))

  ;; To be accurate  the length of class 6 can be either 2 or 4.
  ;; But it is not necessary to discriminate between the two since
  ;; it is used for frequency analysis only  and we are validing
  ;; each code range there as well. So it is safe to set it to be
  ;; 2 here.
(defvar unicad-gb18030-charlen-table
      [0  1  1  1  1  1  2])

(defvar unicad-gb18030-sm-model
  (list
   (cons 'classTable  unicad-gb18030-class-table)
   (cons 'classFactor  7 )
   (cons 'stateTable  unicad-gb18030-state-table)
   (cons 'charLenTable  unicad-gb18030-charlen-table)
   (cons 'name 'gb18030)))

;;}}}
;;{{{  big5 state machine
(defvar unicad-big5-class-table
      `[
        1 1 1 1 1 1 1 1 ;; 00 - 07    ;;allow #x00 as legal value
        1 1 1 1 1 1 0 0 ;; 08 - 0f
        1 1 1 1 1 1 1 1 ;; 10 - 17
        1 1 1 1 1 1 1 1 ;; 18 - 1f    ;;allow esc as legal value
        1 1 1 1 1 1 1 1 ;; 20 - 27
        1 1 1 1 1 1 1 1 ;; 28 - 2f
        1 1 1 1 1 1 1 1 ;; 30 - 37
        1 1 1 1 1 1 1 1 ;; 38 - 3f
        2 2 2 2 2 2 2 2 ;; 40 - 47
        2 2 2 2 2 2 2 2 ;; 48 - 4f
        2 2 2 2 2 2 2 2 ;; 50 - 57
        2 2 2 2 2 2 2 2 ;; 58 - 5f
        2 2 2 2 2 2 2 2 ;; 60 - 67
        2 2 2 2 2 2 2 2 ;; 68 - 6f
        2 2 2 2 2 2 2 2 ;; 70 - 77
        2 2 2 2 2 2 2 1 ;; 78 - 7f
        4 4 4 4 4 4 4 4 ;; 80 - 87
        4 4 4 4 4 4 4 4 ;; 88 - 8f
        4 4 4 4 4 4 4 4 ;; 90 - 97
        4 4 4 4 4 4 4 4 ;; 98 - 9f
        4 3 3 3 3 3 3 3 ;; a0 - a7
        3 3 3 3 3 3 3 3 ;; a8 - af
        3 3 3 3 3 3 3 3 ;; b0 - b7
        3 3 3 3 3 3 3 3 ;; b8 - bf
        3 3 3 3 3 3 3 3 ;; c0 - c7
        3 3 3 3 3 3 3 3 ;; c8 - cf
        3 3 3 3 3 3 3 3 ;; d0 - d7
        3 3 3 3 3 3 3 3 ;; d8 - df
        3 3 3 3 3 3 3 3 ;; e0 - e7
        3 3 3 3 3 3 3 3 ;; e8 - ef
        3 3 3 3 3 3 3 3 ;; f0 - f7
        3 3 3 3 3 3 3 0 ;; f8 - ff
        ])

(defvar unicad-big5-state-table
  (let ((eStart 0)
        (eError 1)
        (eItsMe 2))
    (vector
     ;;   0      1      2      3      4
     eError eStart eStart      3 eError
     eError eError eError eError eError
     eItsMe eItsMe eItsMe eItsMe eItsMe
     eError eError eStart eStart eStart)))

(defvar unicad-big5-charlen-table
  [0  1  1  2  0])

(defvar unicad-big5-sm-model
  (list
   (cons 'classTable  unicad-big5-class-table)
   (cons 'classFactor  5)
   (cons 'stateTable  unicad-big5-state-table)
   (cons 'charLenTable  unicad-big5-charlen-table)
   (cons 'name  'big5)))
;;}}}
;;{{{  sjis state machine
(defvar unicad-sjis-class-table
  `[
    ;;,(PCK4BITS 0 1 1 1 1 1 1 1)   ;; 00 - 07
    1 1 1 1 1 1 1 1       ;; 00 - 07
    1 1 1 1 1 1 0 0       ;; 08 - 0f
    1 1 1 1 1 1 1 1       ;; 10 - 17
    1 1 1 1 1 1 1 1       ;; 18 - 1f  allow esc as legal value
    1 1 1 1 1 1 1 1       ;; 20 - 27
    1 1 1 1 1 1 1 1       ;; 28 - 2f
    1 1 1 1 1 1 1 1       ;; 30 - 37
    1 1 1 1 1 1 1 1       ;; 38 - 3f
    2 2 2 2 2 2 2 2       ;; 40 - 47
    2 2 2 2 2 2 2 2       ;; 48 - 4f
    2 2 2 2 2 2 2 2       ;; 50 - 57
    2 2 2 2 2 2 2 2       ;; 58 - 5f
    2 2 2 2 2 2 2 2       ;; 60 - 67
    2 2 2 2 2 2 2 2       ;; 68 - 6f
    2 2 2 2 2 2 2 2       ;; 70 - 77
    2 2 2 2 2 2 2 1       ;; 78 - 7f
    3 3 3 3 3 3 3 3       ;; 80 - 87
    3 3 3 3 3 3 3 3       ;; 88 - 8f
    3 3 3 3 3 3 3 3       ;; 90 - 97
    3 3 3 3 3 3 3 3       ;; 98 - 9f
    ;;#xa0 is illegal in sjis encoding  but some pages does
    ;;contain such byte. We need to be more error forgiven.
    2 2 2 2 2 2 2 2       ;; a0 - a7
    2 2 2 2 2 2 2 2       ;; a8 - af
    2 2 2 2 2 2 2 2       ;; b0 - b7
    2 2 2 2 2 2 2 2       ;; b8 - bf
    2 2 2 2 2 2 2 2       ;; c0 - c7
    2 2 2 2 2 2 2 2       ;; c8 - cf
    2 2 2 2 2 2 2 2       ;; d0 - d7
    2 2 2 2 2 2 2 2       ;; d8 - df
    3 3 3 3 3 3 3 3       ;; e0 - e7
    3 3 3 3 3 4 4 4       ;; e8 - ef
    4 4 4 4 4 4 4 4       ;; f0 - f7
    4 4 4 4 4 0 0 0       ;; f8 - ff
    ])


(defvar unicad-sjis-state-table
  (let ((eStart 0)
        (eError 1)
        (eItsMe 2))
    (vector
     eError eStart eStart      3 eError eError eError eError ;;00-07
     eError eError eError eError eItsMe eItsMe eItsMe eItsMe ;;08-0f
     eItsMe eItsMe eError eError eStart eStart eStart eStart ;;10-17
     )))

(defvar unicad-sjis-charlen-table
  [0  1  1  2  0  0])

(defvar unicad-sjis-sm-model
  (list
   (cons 'classTable  unicad-sjis-class-table)
   (cons 'classFactor  6 )
   (cons 'stateTable  unicad-sjis-state-table)
   (cons 'charLenTable  unicad-sjis-charlen-table)
   (cons 'name 'sjis)))

;;}}}
;;{{{  eucjp state machine

(defvar unicad-eucjp-class-table
      `[
        ;;,(PCK4BITS 5 4 4 4 4 4 4 4)   ;; 00 - 07
        4 4 4 4 4 4 4 4   ;; 00 - 07
        4 4 4 4 4 4 5 5   ;; 08 - 0f
        4 4 4 4 4 4 4 4   ;; 10 - 17
        4 4 4 5 4 4 4 4   ;; 18 - 1f
        4 4 4 4 4 4 4 4   ;; 20 - 27
        4 4 4 4 4 4 4 4   ;; 28 - 2f
        4 4 4 4 4 4 4 4   ;; 30 - 37
        4 4 4 4 4 4 4 4   ;; 38 - 3f
        4 4 4 4 4 4 4 4   ;; 40 - 47
        4 4 4 4 4 4 4 4   ;; 48 - 4f
        4 4 4 4 4 4 4 4   ;; 50 - 57
        4 4 4 4 4 4 4 4   ;; 58 - 5f
        4 4 4 4 4 4 4 4   ;; 60 - 67
        4 4 4 4 4 4 4 4   ;; 68 - 6f
        4 4 4 4 4 4 4 4   ;; 70 - 77
        4 4 4 4 4 4 4 4   ;; 78 - 7f
        5 5 5 5 5 5 5 5   ;; 80 - 87
        5 5 5 5 5 5 1 3   ;; 88 - 8f
        5 5 5 5 5 5 5 5   ;; 90 - 97
        5 5 5 5 5 5 5 5   ;; 98 - 9f
        5 2 2 2 2 2 2 2   ;; a0 - a7
        2 2 2 2 2 2 2 2   ;; a8 - af
        2 2 2 2 2 2 2 2   ;; b0 - b7
        2 2 2 2 2 2 2 2   ;; b8 - bf
        2 2 2 2 2 2 2 2   ;; c0 - c7
        2 2 2 2 2 2 2 2   ;; c8 - cf
        2 2 2 2 2 2 2 2   ;; d0 - d7
        2 2 2 2 2 2 2 2   ;; d8 - df
        0 0 0 0 0 0 0 0   ;; e0 - e7
        0 0 0 0 0 0 0 0   ;; e8 - ef
        0 0 0 0 0 0 0 0   ;; f0 - f7
        0 0 0 0 0 0 0 5   ;; f8 - ff
        ])

(defvar unicad-eucjp-state-table
  (let ((eStart 0)
        (eError 1)
        (eItsMe 2))
    (vector
     ;; 0   1      2      3      4      5
     3      4      3      5      eStart eError
     eError eError eError eError eError eError
     eItsMe eItsMe eItsMe eItsMe eItsMe eItsMe
     eStart eError eStart eError eError eError
     eError eError eStart eError eError eError
     3      eError 3      eError eError eError)))

(defvar unicad-eucjp-charlen-table
  [2  2  2  3  1  0])

(defvar unicad-eucjp-sm-model
  (list
   (cons 'classTable  unicad-eucjp-class-table)
   (cons 'classFactor  6 )
   (cons 'stateTable  unicad-eucjp-state-table)
   (cons 'charLenTable  unicad-eucjp-charlen-table)
   (cons 'name 'euc-jp)))

;;}}}
;;{{{  euckr state machine
(defvar unicad-euckr-class-table
  `[
    ;;,(PCK4BITS 0 1 1 1 1 1 1 1)   ;; 00 - 07
    1 1 1 1 1 1 1 1       ;; 00 - 07
    1 1 1 1 1 1 0 0       ;; 08 - 0f
    1 1 1 1 1 1 1 1       ;; 10 - 17
    1 1 1 1 1 1 1 1       ;; 18 - 1f ;; allow esc as legal char
    1 1 1 1 1 1 1 1       ;; 20 - 27
    1 1 1 1 1 1 1 1       ;; 28 - 2f
    1 1 1 1 1 1 1 1       ;; 30 - 37
    1 1 1 1 1 1 1 1       ;; 38 - 3f
    1 1 1 1 1 1 1 1       ;; 40 - 47
    1 1 1 1 1 1 1 1       ;; 48 - 4f
    1 1 1 1 1 1 1 1       ;; 50 - 57
    1 1 1 1 1 1 1 1       ;; 58 - 5f
    1 1 1 1 1 1 1 1       ;; 60 - 67
    1 1 1 1 1 1 1 1       ;; 68 - 6f
    1 1 1 1 1 1 1 1       ;; 70 - 77
    1 1 1 1 1 1 1 1       ;; 78 - 7f
    0 0 0 0 0 0 0 0       ;; 80 - 87
    0 0 0 0 0 0 0 0       ;; 88 - 8f
    0 0 0 0 0 0 0 0       ;; 90 - 97
    0 0 0 0 0 0 0 0       ;; 98 - 9f
    0 2 2 2 2 2 2 2       ;; a0 - a7
    2 2 2 2 2 3 3 3       ;; a8 - af
    2 2 2 2 2 2 2 2       ;; b0 - b7
    2 2 2 2 2 2 2 2       ;; b8 - bf
    2 2 2 2 2 2 2 2       ;; c0 - c7
    2 3 2 2 2 2 2 2       ;; c8 - cf
    2 2 2 2 2 2 2 2       ;; d0 - d7
    2 2 2 2 2 2 2 2       ;; d8 - df
    2 2 2 2 2 2 2 2       ;; e0 - e7
    2 2 2 2 2 2 2 2       ;; e8 - ef
    2 2 2 2 2 2 2 2       ;; f0 - f7
    2 2 2 2 2 2 2 0       ;; f8 - ff
    ])

(defvar unicad-euckr-state-table
  (let ((eStart 0)
        (eError 1)
        (eItsMe 2))
    (vector
     ;; 0   1      2      3
     eError eStart 3      eError
     eError eError eError eError
     eItsMe eItsMe eItsMe eItsMe
     eError eError eStart eStart)))

(defvar unicad-euckr-charlen-table
  [0  1  2  0])

(defvar unicad-euckr-sm-model
  (list
   (cons 'classTable  unicad-euckr-class-table)
   (cons 'classFactor  4 )
   (cons 'stateTable  unicad-euckr-state-table)
   (cons 'charLenTable  unicad-euckr-charlen-table)
   (cons 'name 'euc-kr)))

;;}}}
;;{{{  euctw state machine
(defvar unicad-euctw-class-table
  `[
    ;;,(PCK4BITS 0 2 2 2 2 2 2 2)   ;; 00 - 07
    2 2 2 2 2 2 2 2       ;; 00 - 07
    2 2 2 2 2 2 0 0       ;; 08 - 0f
    2 2 2 2 2 2 2 2       ;; 10 - 17
    2 2 2 2 2 2 2 2       ;; 2 2 2 0 2 2 2 2       ;; 18 - 1f
    2 2 2 2 2 2 2 2       ;; 20 - 27
    2 2 2 2 2 2 2 2       ;; 28 - 2f
    2 2 2 2 2 2 2 2       ;; 30 - 37
    2 2 2 2 2 2 2 2       ;; 38 - 3f
    2 2 2 2 2 2 2 2       ;; 40 - 47
    2 2 2 2 2 2 2 2       ;; 48 - 4f
    2 2 2 2 2 2 2 2       ;; 50 - 57
    2 2 2 2 2 2 2 2       ;; 58 - 5f
    2 2 2 2 2 2 2 2       ;; 60 - 67
    2 2 2 2 2 2 2 2       ;; 68 - 6f
    2 2 2 2 2 2 2 2       ;; 70 - 77
    2 2 2 2 2 2 2 2       ;; 78 - 7f
    0 0 0 0 0 0 0 0       ;; 80 - 87
    0 0 0 0 0 0 6 0       ;; 88 - 8f
    0 0 0 0 0 0 0 0       ;; 90 - 97
    0 0 0 0 0 0 0 0       ;; 98 - 9f
    0 3 4 4 4 4 4 4       ;; a0 - a7
    5 5 1 1 1 1 1 1       ;; a8 - af
    1 1 1 1 1 1 1 1       ;; b0 - b7
    1 1 1 1 1 1 1 1       ;; b8 - bf
    1 1 3 1 3 3 3 3       ;; c0 - c7
    3 3 3 3 3 3 3 3       ;; c8 - cf
    3 3 3 3 3 3 3 3       ;; d0 - d7
    3 3 3 3 3 3 3 3       ;; d8 - df
    3 3 3 3 3 3 3 3       ;; e0 - e7
    3 3 3 3 3 3 3 3       ;; e8 - ef
    3 3 3 3 3 3 3 3       ;; f0 - f7
    3 3 3 3 3 3 3 0       ;; f8 - ff
    ])

(defvar unicad-euctw-state-table
  (let ((eStart 0)
        (eError 1)
        (eItsMe 2))
    (vector
     ;;  0   1     2      3      4      5      6
     eError eError eStart 3      3      3      4
     eError eError eError eError eError eError eError
     eItsMe eItsMe eItsMe eItsMe eItsMe eItsMe eItsMe
     eError eStart eError eStart eStart eStart eError
     eError eError eError eError 5      eError eError
     eError eStart eError eStart eStart eStart eError)))

(defvar unicad-euctw-charlen-table
      [0  0  1  2  2  2  3])

(defvar unicad-euctw-sm-model
  (list
   (cons 'classTable  unicad-euctw-class-table)
   (cons 'classFactor  7 )
   (cons 'stateTable  unicad-euctw-state-table)
   (cons 'charLenTable  unicad-euctw-charlen-table)
   (cons 'name  'euc-tw)))

;;}}}

;;{{{  Multibyte Prober

(defun unicad-multibyte-group-prober (start end)
  "extract the multibyte chardet prober functions from
`unicad-multibyte-group-list', compare the confidence of each
chardet and return the best guess."
  (let ((lists unicad-multibyte-group-list)
        (mState 'eDetecting)
        (bestConf 0.0)
        state chardet mBestGuess cf)
    (setq unicad-best-guess '(nil 0.0))
    (while (and lists (eq mState 'eDetecting))
      (setq chardet (pop lists))
      (setq state (funcall (unicad-chardet-prober chardet)
                           start end))
      (cond
       ((eq state 'eFoundIt)
        (setq mBestGuess (unicad-chardet-name chardet))
        (setq bestConf unicad--sure-yes)
        (setq mState 'eFoundIt))
       ((eq state 'eNotMe) nil)
       (t
        (setq cf (unicad-chardet-confidence chardet))
        (if (> cf bestConf)
            (progn
              (setq mBestGuess (unicad-chardet-name chardet))
              (setq bestConf cf))))))
    (if (or (<= bestConf unicad--sure-no) (null mBestGuess))
        (setq mState 'eNotMe)
      (setq unicad-best-guess (list mBestGuess bestConf)))
    mState))

(defun unicad-cjk-prober (start end chardet model dist-table dist-ratio analyser)
  "A generic prober for two byte coding system. e.g. chinese,
japanese, korean"
  (let ((mState 'eDetecting)
        (mConfidence 0.0)
        (code0 0) (code1 0)
        (size (- end start))
        (mb-num 0)                      ;number of multi-byte words
        (mCodingSystem (cdr (assoc 'name model)))
        codingState charlen)
    (unicad-sm-reset)
    (unicad-dist-table-reset dist-table)
    (save-excursion
      (goto-char start)
      (setq code1 (unicad-char-after))
      (while (and (< (point) end)
                  (eq mState 'eDetecting)
                  (< mb-num unicad-quick-multibyte-words))
        (setq code1 (unicad-char-after))
        (setq codingState (unicad-next-state code1 model))
        (forward-char 1)
        (if (and (= code0 #x0D) (= code1 #x0A))
            (setq unicad-eol 1))
        (cond
         ;; we are interested only in 2-byte
         ((= codingState unicad--eStart)
          (setq charlen (unicad-sm-get 'mCharLen))
          (if (> charlen 1)
                (setq mb-num  (1+ mb-num)))
          ;; we need the analyser, only when charlen > 1
          (and (> charlen 1) (funcall analyser code0 code1)))
         ((= codingState unicad--eError)
          (setq mState 'eNotMe)
          (unicad-chardet-set-confidence chardet unicad--sure-no))
         ((= codingState unicad--eItsMe)
          (setq mState 'eFoundIt)
          (unicad-chardet-set-confidence chardet unicad--sure-yes))
         (t nil))
        (setq code0 code1))
      (if (eq mState 'eDetecting)
          (if (and (> (setq mConfidence (unicad-dist-table-get-confidence dist-table dist-ratio size
                                                                          (eq unicad-cjk-prefer mCodingSystem)))
                      unicad-threshold)
                   (> (unicad-dist-table-total-chars dist-table)
                      unicad-data-threshold))
              (progn
                (setq mState 'eFoundIt)
                (unicad-chardet-set-confidence chardet unicad--sure-yes))
            (if (= (unicad-chardet-set-confidence chardet mConfidence) unicad--sure-yes)
                (setq mState 'eFoundIt))))
      mState)))
;;}}}

;;{{{  utf8 prober
(defvar unicad-utf8-list (unicad-chardet unicad-multibyte-group-list 'unicad-utf8-prober))

(defun unicad-utf8-prober (start end)
  "Detect for utf-8 coding-system by state
machine (`unicad-next-state') and get the confidence"
  (let ((mState 'eDetecting)
        (mNumOfMBChar 0)
        codingState charlen (mConfidence 0.0))
    (unicad-sm-reset)
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (eq mState 'eDetecting))
        (setq codingState (unicad-next-state (unicad-char-after)
                                           unicad-utf8-sm-model))
        (setq charlen (unicad-sm-get 'mCharLen))
        (forward-char 1)
        (cond
         ((= codingState unicad--eStart)
          (if (>= charlen 2)
              (setq mNumOfMBChar (1+ mNumOfMBChar))))
         ((= codingState unicad--eError)
          (setq mState 'eNotMe)
          (unicad-chardet-set-confidence unicad-utf8-list unicad--sure-no))
         ((= codingState unicad--eItsMe)
          (setq mState 'eFoundIt)
          (unicad-chardet-set-confidence unicad-utf8-list unicad--sure-yes))
         (t nil)))
      (if (eq mState 'eDetecting)
          (if (> (setq mConfidence (unicad-utf8-get-confidence mNumOfMBChar)) unicad-threshold)
              (progn
                (setq mState 'eFoundIt)
                (unicad-chardet-set-confidence unicad-utf8-list unicad--sure-yes))
            (unicad-chardet-set-confidence unicad-utf8-list mConfidence)))
      mState)))

(defun unicad-utf8-get-confidence (mNumOfMBChar)
  "calculate the confidence for utf-8"
  (let ((unlike unicad--sure-yes)
        (one-char-prob 0.5))
    (if (< mNumOfMBChar 6)
        (setq unlike (* (expt one-char-prob mNumOfMBChar) unlike))
      (setq unlike unicad--sure-no))
    (- 1 unlike)))

;;}}}
;;{{{  ucs2be and ucs2le prober

(defvar unicad-ucs2be-list (unicad-chardet unicad-multibyte-group-list 'unicad-ucs2be-prober))

(defun unicad-ucs2be-prober (start end)
  "A simple prober function for utf-16-be. It only counts the
eol, space, numbers and english letters."
  (let ((mState 'eNotMe)
        (count 0)
        code0 code1)
    (setq start (point-min)
          end (min (point-max) 1000))
    (if (% end 2)
        (setq end (1- end)))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (eq mState 'eNotMe))
        (setq code0 (unicad-char-after))
        (forward-char 1)
        (setq code1 (unicad-char-after))
        (forward-char 1)
        (when (= code0 0)
          (if (or (= code1 #x0d)
                  (= code1 #x0a)
                  (= code1 #x20)
                  (and (> code1 ?0) (< code1 ?9))
                  (and (> code1 ?a) (< code1 ?z))
                  (and (> code1 ?A) (< code1 ?Z)))
              (setq count (1+ count)))
          (if (> count 10)
              (progn
                (setq mState 'eFoundIt)
                (unicad-chardet-set-confidence unicad-ucs2be-list unicad--sure-yes))
            (unicad-chardet-set-confidence unicad-ucs2be-list unicad--sure-no)))))
    mState))

(defvar unicad-ucs2le-list (unicad-chardet unicad-multibyte-group-list 'unicad-ucs2le-prober))

(defun unicad-ucs2le-prober (start end)
  "A simple prober function for utf-16-le. It only counts the
eol, space, numbers and english letters."
  (let ((mState 'eNotMe)
        (count 0)
        code0 code1)
    (setq start (point-min)
          end (min (point-max) 1000))
    (if (% end 2)
        (setq end (1- end)))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (eq mState 'eNotMe))
        (setq code0 (unicad-char-after))
        (forward-char 1)
        (setq code1 (unicad-char-after))
        (forward-char 1)
        (when (= code1 0)
          (if (or (= code0 #x0d)
                  (= code0 #x0a)
                  (= code0 #x20)
                  (and (> code0 ?0) (< code0 ?9))
                  (and (> code0 ?a) (< code0 ?z))
                  (and (> code0 ?A) (< code0 ?Z)))
              (setq count (1+ count)))
          (if (> count 10)
              (progn
                (setq mState 'eFoundIt)
                (unicad-chardet-set-confidence unicad-ucs2le-list unicad--sure-yes))
            (unicad-chardet-set-confidence unicad-ucs2le-list unicad--sure-no)))))
    mState))

;;}}}
;;{{{  ucs2 state machine
;; the state machine for ucs2 seems doesn't work.
;; so I use the simple prober above to detect ucs2

(defconst unicad-ucs2be-class-table
  [
   0 0 0 0 0 0 0 0 ;; 00 - 07
   0 0 1 0 0 2 0 0 ;; 08 - 0f
   0 0 0 0 0 0 0 0 ;; 10 - 17
   0 0 0 3 0 0 0 0 ;; 18 - 1f
   0 0 0 0 0 0 0 0 ;; 20 - 27
   0 3 3 3 3 3 0 0 ;; 28 - 2f
   0 0 0 0 0 0 0 0 ;; 30 - 37
   0 0 0 0 0 0 0 0 ;; 38 - 3f
   0 0 0 0 0 0 0 0 ;; 40 - 47
   0 0 0 0 0 0 0 0 ;; 48 - 4f
   0 0 0 0 0 0 0 0 ;; 50 - 57
   0 0 0 0 0 0 0 0 ;; 58 - 5f
   0 0 0 0 0 0 0 0 ;; 60 - 67
   0 0 0 0 0 0 0 0 ;; 68 - 6f
   0 0 0 0 0 0 0 0 ;; 70 - 77
   0 0 0 0 0 0 0 0 ;; 78 - 7f
   0 0 0 0 0 0 0 0 ;; 80 - 87
   0 0 0 0 0 0 0 0 ;; 88 - 8f
   0 0 0 0 0 0 0 0 ;; 90 - 97
   0 0 0 0 0 0 0 0 ;; 98 - 9f
   0 0 0 0 0 0 0 0 ;; a0 - a7
   0 0 0 0 0 0 0 0 ;; a8 - af
   0 0 0 0 0 0 0 0 ;; b0 - b7
   0 0 0 0 0 0 0 0 ;; b8 - bf
   0 0 0 0 0 0 0 0 ;; c0 - c7
   0 0 0 0 0 0 0 0 ;; c8 - cf
   0 0 0 0 0 0 0 0 ;; d0 - d7
   0 0 0 0 0 0 0 0 ;; d8 - df
   0 0 0 0 0 0 0 0 ;; e0 - e7
   0 0 0 0 0 0 0 0 ;; e8 - ef
   0 0 0 0 0 0 0 0 ;; f0 - f7
   0 0 0 0 0 0 4 5 ;; f8 - ff
   ])


(defconst unicad-ucs2be-state-table
  (let ((eStart 0)
        (eError 1)
        (eItsMe 2))
    (vector
     5      7      7 eError      4      3 eError eError ;;00-07
     eError eError eError eError eItsMe eItsMe eItsMe eItsMe ;;08-0f
     eItsMe eItsMe      6      6      6      6 eError eError ;;10-17
     6      6      6      6      6 eItsMe      6      6      ;;18-1f
     6      6      6      6      5      7      7 eError      ;;20-27
     5      8      6      6 eError      6      6      6      ;;28-2f
     6      6      6      6 eError eError eStart eStart      ;;30-37
     )))

(defconst unicad-ucs2be-charlen-table
  [2  2  2  0  2  2])

(defvar unicad-ucs2be-sm-model
  (list
   (cons 'classTable unicad-ucs2be-class-table)
   (cons 'classFactor 6)
   (cons 'stateTable unicad-ucs2be-state-table)
   (cons 'charLenTable unicad-ucs2be-charlen-table)
   (cons 'name "UTF-16-BE")))

(defconst unicad-ucs2le-class-table
  [
   0 0 0 0 0 0 0 0 ;; 00 - 07
   0 0 1 0 0 2 0 0 ;; 08 - 0f
   0 0 0 0 0 0 0 0 ;; 10 - 17
   0 0 0 3 0 0 0 0 ;; 18 - 1f
   0 0 0 0 0 0 0 0 ;; 20 - 27
   0 3 3 3 3 3 0 0 ;; 28 - 2f
   0 0 0 0 0 0 0 0 ;; 30 - 37
   0 0 0 0 0 0 0 0 ;; 38 - 3f
   0 0 0 0 0 0 0 0 ;; 40 - 47
   0 0 0 0 0 0 0 0 ;; 48 - 4f
   0 0 0 0 0 0 0 0 ;; 50 - 57
   0 0 0 0 0 0 0 0 ;; 58 - 5f
   0 0 0 0 0 0 0 0 ;; 60 - 67
   0 0 0 0 0 0 0 0 ;; 68 - 6f
   0 0 0 0 0 0 0 0 ;; 70 - 77
   0 0 0 0 0 0 0 0 ;; 78 - 7f
   0 0 0 0 0 0 0 0 ;; 80 - 87
   0 0 0 0 0 0 0 0 ;; 88 - 8f
   0 0 0 0 0 0 0 0 ;; 90 - 97
   0 0 0 0 0 0 0 0 ;; 98 - 9f
   0 0 0 0 0 0 0 0 ;; a0 - a7
   0 0 0 0 0 0 0 0 ;; a8 - af
   0 0 0 0 0 0 0 0 ;; b0 - b7
   0 0 0 0 0 0 0 0 ;; b8 - bf
   0 0 0 0 0 0 0 0 ;; c0 - c7
   0 0 0 0 0 0 0 0 ;; c8 - cf
   0 0 0 0 0 0 0 0 ;; d0 - d7
   0 0 0 0 0 0 0 0 ;; d8 - df
   0 0 0 0 0 0 0 0 ;; e0 - e7
   0 0 0 0 0 0 0 0 ;; e8 - ef
   0 0 0 0 0 0 0 0 ;; f0 - f7
   0 0 0 0 0 0 4 5 ;; f8 - ff
   ])


(defconst unicad-ucs2le-state-table
  (let ((eStart 0)
        (eError 1)
        (eItsMe 2))
    (vector
     6      6      7      6      4      3 eError eError ;;00-07
     eError eError eError eError eItsMe eItsMe eItsMe eItsMe ;;08-0f
     eItsMe eItsMe      5      5      5 eError eItsMe eError ;;10-17
     5      5      5 eError      5 eError      6      6      ;;18-1f
     7      6      8      8      5      5      5 eError      ;;20-27
     5      5      5 eError eError eError      5      5      ;;28-2f
     5      5      5 eError      5 eError eStart eStart      ;;30-37
     )))

(defconst unicad-ucs2le-charlen-table
  [2  2  2  2  2  2])

(defvar unicad-ucs2le-sm-model
  (list
   (cons 'classTable unicad-ucs2le-class-table)
   (cons 'classFactor 6)
   (cons 'stateTable unicad-ucs2le-state-table)
   (cons 'charLenTable unicad-ucs2le-charlen-table)
   (cons 'name "UTF-16-LE")))

;;}}}

;;{{{  gb2312 prober
(defvar unicad-gb2312-list (unicad-chardet unicad-multibyte-group-list 'unicad-gb2312-prober))
(defvar unicad-gb2312-dist-table '(0 . 0))

(defsubst unicad-gb2312-prober (start end)
  (unicad-cjk-prober start end unicad-gb2312-list
                   unicad-gb18030-sm-model unicad-gb2312-dist-table
                   unicad-gb2312-dist-ratio 'unicad-gb2312-analyser))

(defun unicad-gb2312-analyser (ch0 ch1)
  "for GB2312 encoding, we are interested
first  byte range: 0xb0 -- 0xfe
second byte range: 0xa1 -- 0xfe
no validation needed here.  State machine has done that"
  (when (and (>= ch0 #xb0) (>= ch1 #xa1))
    (let (order)
      (setq order (- (+ (* 94 (- ch0 #xb0)) ch1) #xa1))
      (when (>= order 0)
        (unicad-dist-table-total-chars++ unicad-gb2312-dist-table)
        (if (and (< order unicad-gb2312-table-size)
                 (<= (aref unicad-gb2312-char-freq-order order) 512))
            (unicad-dist-table-freq-chars++ unicad-gb2312-dist-table))))))

;;}}}
;;{{{  gbkcht prober
;; use gbk state machine but use big5 analyser

(defvar unicad-gbkcht-list (unicad-chardet unicad-multibyte-group-list 'unicad-gbkcht-prober))
(defvar unicad-big5-dist-table '(0 . 0))
(defsubst unicad-gbkcht-prober (start end)
  (unicad-cjk-prober start end unicad-gbkcht-list
                   unicad-gb18030-sm-model unicad-big5-dist-table
                   unicad-big5-dist-ratio 'unicad-gbkcht-analyser))


(defun unicad-gbkcht-analyser (ch0 ch1)
  "we convert the gbk code into big5, than use `unicad-big5-analyser' to get the order"
  (let ((chargbk (decode-char 'chinese-gbk (+ (* 256 ch0) ch1))))
    (when chargbk
      (let ((bar (encode-coding-char chargbk 'big5)))
        (if bar
            (let ((chr0 (string-to-char (substring bar 0)))
                  (chr1 (string-to-char (substring bar 1))))
              (unicad-big5-analyser chr0 chr1)))))))

;;;}}}
;;{{{  big5 prober

(defvar unicad-big5-list (unicad-chardet unicad-multibyte-group-list 'unicad-big5-prober))
(defvar unicad-big5-dist-table '(0 . 0))
(defsubst unicad-big5-prober (start end)
  (unicad-cjk-prober start end unicad-big5-list
                   unicad-big5-sm-model unicad-big5-dist-table
                   unicad-big5-dist-ratio 'unicad-big5-analyser))

(defun unicad-big5-analyser (ch0 ch1)
  "for big5 encoding, we are interested
  first  byte range: 0xa4 -- 0xfe
  second byte range: 0x40 -- 0x7e , 0xa1 -- 0xfe
no validation needed here. State machine has done that"
  (when (>= ch0 #xa4)
    (let ((order -1))
      (if (>= ch1 #xa1)
          (setq order (- (+ (* 157 (- ch0 #xa4)) ch1 63) #xa1))
        (setq order (- (+ (* 157 (- ch0 #xa4)) ch1) #x40)))
      (when (>= order 0)
        (unicad-dist-table-total-chars++ unicad-big5-dist-table)
        (if (and (< order unicad-big5-table-size)
                 (<= (aref unicad-big5-char-freq-order order) 512))
            (unicad-dist-table-freq-chars++ unicad-big5-dist-table))))))

;;}}}
;;{{{  sjis prober

(defvar unicad-sjis-list (unicad-chardet unicad-multibyte-group-list 'unicad-sjis-prober))
(defvar unicad-sjis-dist-table '(0 . 0))
(defsubst unicad-sjis-prober (start end)
  (unicad-cjk-prober start end unicad-sjis-list
                   unicad-sjis-sm-model unicad-sjis-dist-table
                   unicad-jis-dist-ratio 'unicad-sjis-analyser))

(defun unicad-sjis-analyser (ch0 ch1)
  "for sjis encoding, we are interested
  first  byte range: 0x81 -- 0x9f , 0xe0 -- 0xfe
  second byte range: 0x40 -- 0x7e,  0x80 -- 0xfc
no validation needed here. State machine has done that
 !NOTE! 0xA1 -- 0xDF are valid halfwidth katakana!!!"
  (let ((order -1))
    (cond
     ((and (>= ch0 #x81) (<= ch0 #x9f))
      (setq order (* 188 (- ch0 #x81))))
     ((and (>= ch0 #xe0) (<= ch0 #xef))
      (setq order (* 188 (+ (- ch0 #xe0) 31)))))
    (when (>= order 0)
      (setq order (+ order (- ch1 #x40)))
      (if (> ch1 #x7f)
          (setq order (1- order))))
    (when (>= order 0)
      (unicad-dist-table-total-chars++ unicad-sjis-dist-table)
      (if (and (< order unicad-jis-table-size)
               (<= (aref unicad-jis-char-freq-order order) 512))
          (unicad-dist-table-freq-chars++ unicad-sjis-dist-table)))))

;;}}}
;;{{{  eucjp prober

(defvar unicad-eucjp-list (unicad-chardet unicad-multibyte-group-list 'unicad-eucjp-prober))
(defvar unicad-eucjp-dist-table '(0 . 0))
(defsubst unicad-eucjp-prober (start end)
  (unicad-cjk-prober start end unicad-eucjp-list
                   unicad-eucjp-sm-model unicad-eucjp-dist-table
                   unicad-jis-dist-ratio 'unicad-eucjp-analyser))

(defun unicad-eucjp-analyser (ch0 ch1)
  "for EUCJP encoding, we are interested
  first  byte range: 0xa0 -- 0xfe
  second byte range: 0xa1 -- 0xfe
no validation needed here. State machine has done that"
  (when (>= ch0 #xa0)
    (let (order)
      (setq order (- (+ (* 94 (- ch0 #xa1)) ch1) #xa1))
      (when (>= order 0)
        (unicad-dist-table-total-chars++ unicad-eucjp-dist-table)
        (if (and (< order unicad-jis-table-size)
                 (<= (aref unicad-jis-char-freq-order order) 512))
            (unicad-dist-table-freq-chars++ unicad-eucjp-dist-table))))))

;;}}}
;;{{{  euckr prober

(defvar unicad-euckr-list (unicad-chardet unicad-multibyte-group-list 'unicad-euckr-prober))
(defvar unicad-euckr-dist-table '(0 . 0))
(defsubst unicad-euckr-prober (start end)
  (unicad-cjk-prober start end unicad-euckr-list
                   unicad-euckr-sm-model unicad-euckr-dist-table
                   unicad-jis-dist-ratio 'unicad-euckr-analyser))

(defun unicad-euckr-analyser (ch0 ch1)
  "for euc-KR encoding, we are interested
  first  byte range: 0xb0 -- 0xfe
  second byte range: 0xa1 -- 0xfe
no validation needed here. State machine has done that"
  (when (>= ch0 #xb0)
    (let (order)
      (setq order (- (+ (* 94 (- ch0 #xb0)) ch1) #xa1))
      (when (>= order 0)
        (unicad-dist-table-total-chars++ unicad-euckr-dist-table)
        (if (and (< order unicad-euckr-table-size)
                 (<= (aref unicad-euckr-char-freq-order order) 512))
            (unicad-dist-table-freq-chars++ unicad-euckr-dist-table))))))

;;}}}
;;{{{  euctw prober

(defvar unicad-euctw-list (unicad-chardet unicad-multibyte-group-list 'unicad-euctw-prober))
(defvar unicad-euctw-dist-table '(0 . 0))
(defsubst unicad-euctw-prober (start end)
  (unicad-cjk-prober start end unicad-euctw-list
                   unicad-euctw-sm-model unicad-euctw-dist-table
                   unicad-euctw-dist-ratio 'unicad-euctw-analyser))

(defun unicad-euctw-analyser (ch0 ch1)
  "for euc-TW encoding, we are interested
  first  byte range: 0xc4 -- 0xfe
  second byte range: 0xa1 -- 0xfe
no validation needed here. State machine has done that"
  (when (>= ch0 #xc4)
    (let (order)
      (setq order (- (+ (* 94 (- ch0 #xc4)) ch1) #xa1))
      (when (>= order 0)
        (unicad-dist-table-total-chars++ unicad-euctw-dist-table)
        (if (and (< order unicad-euctw-table-size)
                 (<= (aref unicad-euctw-char-freq-order order) 512))
            (unicad-dist-table-freq-chars++ unicad-euctw-dist-table))))))
;;}}}

;;{{{  Latin2

(defvar unicad-latin2-class-num 9)   ;; total classes

(defvar unicad-latin2-class-table
  (let ((UDF 0) ;; undefined
        (OTH 1) ;; other
        (ASC 2) ;; ascii capital letter
        (ASS 3) ;; ascii small letter
        (ACV 4) ;; accent capital vowel
        (ACO 5) ;; accent capital other
        (ASV 6) ;; accent small vowel
        (ASO 7) ;; accent small other
        (OTS 8) ;; Other Symbol (> #xA0)
        )
    (vector
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 00 - 07
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 08 - 0F
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 10 - 17
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 18 - 1F
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 20 - 27
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 28 - 2F
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 30 - 37
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 38 - 3F
     OTH  ASC  ASC  ASC  ASC  ASC  ASC  ASC ;; 40 - 47
     ASC  ASC  ASC  ASC  ASC  ASC  ASC  ASC ;; 48 - 4F
     ASC  ASC  ASC  ASC  ASC  ASC  ASC  ASC ;; 50 - 57
     ASC  ASC  ASC  OTH  OTH  OTH  OTH  OTH ;; 58 - 5F
     OTH  ASS  ASS  ASS  ASS  ASS  ASS  ASS ;; 60 - 67
     ASS  ASS  ASS  ASS  ASS  ASS  ASS  ASS ;; 68 - 6F
     ASS  ASS  ASS  ASS  ASS  ASS  ASS  ASS ;; 70 - 77
     ASS  ASS  ASS  OTH  OTH  OTH  OTH  OTH ;; 78 - 7F
     OTH  UDF  OTH  ASO  OTH  OTH  OTH  OTH ;; 80 - 87
     OTH  OTH  ACO  OTH  ACO  UDF  ACO  UDF ;; 88 - 8F
     UDF  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 90 - 97
     OTH  OTH  ASO  OTH  ASO  UDF  ASO  ACO ;; 98 - 9F
     OTS  ACV  OTS  ACO  OTS  ACO  ACO  OTS ;; A0 - A7
     OTS  ACO  ACO  ACO  ACO  OTS  ACO  ACO ;; A8 - AF
     OTS  ASV  OTS  ASO  OTS  ASO  ASO  OTS ;; B0 - B7
     OTS  ASO  ASO  ASO  ASO  OTS  ASO  ASO ;; B8 - BF
     ACO  ACV  ACV  ACV  ACV  ACO  ACO  ACO ;; C0 - C7
     ACO  ACV  ACV  ACV  ACV  ACV  ACV  ACO ;; C8 - CF
     ACO  ACO  ACO  ACV  ACV  ACV  ACV  OTS ;; D0 - D7
     ACO  ACV  ACV  ACV  ACV  ACO  ACO  ASO ;; D8 - DF
     ASO  ASV  ASV  ASV  ASV  ASO  ASO  ASO ;; E0 - E7
     ASO  ASV  ASV  ASV  ASV  ASV  ASV  ASO ;; E8 - EF
     ASO  ASO  ASO  ASV  ASV  ASV  ASV  OTS ;; F0 - F7
     ASO  ASV  ASV  ASV  ASV  ASO  ASO  OTS ;; F8 - FF
     )))

(defvar unicad-latin2-model
  [
;; UDF OTH ASC ASS ACV ACO ASV ASO OTS
   0   0   0   0   0   0   0   0   0 ;; UDF last char
   0   3   3   3   3   3   3   3   3 ;; OTH
   0   3   3   3   3   3   3   3   2 ;; ASC
   0   3   3   3   1   1   3   3   2 ;; ASS
   0   3   3   3   1   2   1   2   1 ;; ACV
   0   3   3   3   3   3   3   3   1 ;; ACO
   0   3   1   3   1   1   1   3   1 ;; ASV
   0   3   1   3   1   1   3   3   1 ;; ASO
   0   3   2   2   2   2   1   1   1 ;; OTS
   ])

;;;}}}

;;{{{  Latin5 iso-8859-9 Turkish

(defvar unicad-latin5-class-num  9)   ;; total classes
(defvar unicad-latin5-class-table
  (let ((UDF 0) ;; undefined
        (OTH 1) ;; other
        (ASC 2) ;; ascii capital letter
        (ASS 3) ;; ascii small letter
        (ACV 4) ;; accent capital vowel
        (ACO 5) ;; accent capital other
        (ASV 6) ;; accent small vowel
        (ASO 7) ;; accent small other
        (OTS 8) ;; Other Symbol (>= #xA0)
        )
    (vector
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 00 - 07
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 08 - 0F
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 10 - 17
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 18 - 1F
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 20 - 27
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 28 - 2F
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 30 - 37
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 38 - 3F
     OTH  ASC  ASC  ASC  ASC  ASC  ASC  ASC ;; 40 - 47
     ASC  ASC  ASC  ASC  ASC  ASC  ASC  ASC ;; 48 - 4F
     ASC  ASC  ASC  ASC  ASC  ASC  ASC  ASC ;; 50 - 57
     ASC  ASC  ASC  OTH  OTH  OTH  OTH  OTH ;; 58 - 5F
     OTH  ASS  ASS  ASS  ASS  ASS  ASS  ASS ;; 60 - 67
     ASS  ASS  ASS  ASS  ASS  ASS  ASS  ASS ;; 68 - 6F
     ASS  ASS  ASS  ASS  ASS  ASS  ASS  ASS ;; 70 - 77
     ASS  ASS  ASS  OTH  OTH  OTH  OTH  OTH ;; 78 - 7F
     OTH  UDF  OTH  ASO  OTH  OTH  OTH  OTH ;; 80 - 87
     OTH  OTH  ACO  OTH  ACO  UDF  ACO  UDF ;; 88 - 8F
     UDF  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 90 - 97
     OTH  OTH  ASO  OTH  ASO  UDF  ASO  ACO ;; 98 - 9F
     OTS  OTS  OTS  OTS  OTS  OTS  OTS  OTS ;; A0 - A7
     OTS  OTS  OTS  OTS  OTS  OTS  OTS  OTS ;; A8 - AF
     OTS  OTS  OTS  OTS  OTS  OTS  OTS  OTS ;; B0 - B7
     OTS  OTS  OTS  OTS  OTS  OTS  OTS  OTS ;; B8 - BF
     ACV  ACV  ACV  ACV  ACV  ACV  ACO  ACO ;; C0 - C7
     ACV  ACV  ACV  ACV  ACV  ACV  ACV  ACV ;; C8 - CF
     ACO  ACO  ACV  ACV  ACV  ACV  ACV  OTS ;; D0 - D7
     ACV  ACV  ACV  ACV  ACV  ACO  ACO  ASO ;; D8 - DF
     ASV  ASV  ASV  ASV  ASV  ASV  ASO  ASO ;; E0 - E7
     ASV  ASV  ASV  ASV  ASV  ASV  ASV  ASV ;; E8 - EF
     ASO  ASO  ASV  ASV  ASV  ASV  ASV  OTS ;; F0 - F7
     ASV  ASV  ASV  ASV  ASV  ASO  ASO  ASO ;; F8 - FF
     )))

(defvar unicad-latin5-model
  [
;; UDF OTH ASC ASS ACV ACO ASV ASO OTS
   0   0   0   0   0   0   0   0   0 ;; UDF last char
   0   3   3   3   3   3   3   3   3 ;; OTH
   0   3   3   3   3   3   3   3   2 ;; ASC
   0   3   3   3   1   1   3   3   2 ;; ASS
   0   3   3   3   1   2   1   2   1 ;; ACV
   0   3   3   3   3   3   3   3   1 ;; ACO
   0   3   1   3   1   1   1   3   1 ;; ASV
   0   3   1   3   1   1   3   3   1 ;; ASO
   0   3   2   2   2   2   1   1   1 ;; OTS
   ])
;;;}}}

;;{{{  latin1 state machine
(defvar unicad-latin1-class-num  9)   ;; total classes
(defvar unicad-latin1-class-table
  (let ((UDF 0) ;; undefined
        (OTH 1) ;; other
        (ASC 2) ;; ascii capital letter
        (ASS 3) ;; ascii small letter
        (ACV 4) ;; accent capital vowel
        (ACO 5) ;; accent capital other
        (ASV 6) ;; accent small vowel
        (ASO 7) ;; accent small other
        (OTS 8) ;; Other Symbol (>= #xA0)
        )
    (vector
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 00 - 07
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 08 - 0F
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 10 - 17
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 18 - 1F
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 20 - 27
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 28 - 2F
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 30 - 37
     OTH  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 38 - 3F
     OTH  ASC  ASC  ASC  ASC  ASC  ASC  ASC ;; 40 - 47
     ASC  ASC  ASC  ASC  ASC  ASC  ASC  ASC ;; 48 - 4F
     ASC  ASC  ASC  ASC  ASC  ASC  ASC  ASC ;; 50 - 57
     ASC  ASC  ASC  OTH  OTH  OTH  OTH  OTH ;; 58 - 5F
     OTH  ASS  ASS  ASS  ASS  ASS  ASS  ASS ;; 60 - 67
     ASS  ASS  ASS  ASS  ASS  ASS  ASS  ASS ;; 68 - 6F
     ASS  ASS  ASS  ASS  ASS  ASS  ASS  ASS ;; 70 - 77
     ASS  ASS  ASS  OTH  OTH  OTH  OTH  OTH ;; 78 - 7F
     OTH  UDF  OTH  ASO  OTH  OTH  OTH  OTH ;; 80 - 87
     OTH  OTH  ACO  OTH  ACO  UDF  ACO  UDF ;; 88 - 8F
     UDF  OTH  OTH  OTH  OTH  OTH  OTH  OTH ;; 90 - 97
     OTH  OTH  ASO  OTH  ASO  UDF  ASO  ACO ;; 98 - 9F
     OTS  OTS  OTS  OTS  OTS  OTS  OTS  OTS ;; A0 - A7
     OTS  OTS  OTS  OTS  OTS  OTS  OTS  OTS ;; A8 - AF
     OTS  OTS  OTS  OTS  OTS  OTS  OTS  OTS ;; B0 - B7
     OTS  OTS  OTS  OTS  OTS  OTS  OTS  OTS ;; B8 - BF
     ACV  ACV  ACV  ACV  ACV  ACV  ACO  ACO ;; C0 - C7
     ACV  ACV  ACV  ACV  ACV  ACV  ACV  ACV ;; C8 - CF
     ACO  ACO  ACV  ACV  ACV  ACV  ACV  OTS ;; D0 - D7
     ACV  ACV  ACV  ACV  ACV  ACO  ACO  ASO ;; D8 - DF
     ASV  ASV  ASV  ASV  ASV  ASV  ASO  ASO ;; E0 - E7
     ASV  ASV  ASV  ASV  ASV  ASV  ASV  ASV ;; E8 - EF
     ASO  ASO  ASV  ASV  ASV  ASV  ASV  OTS ;; F0 - F7
     ASV  ASV  ASV  ASV  ASV  ASO  ASO  ASO ;; F8 - FF
     )))

(defvar unicad-latin1-model
  [
;; UDF OTH ASC ASS ACV ACO ASV ASO OTS
   0   0   0   0   0   0   0   0   0 ;; UDF last char
   0   3   3   3   3   3   3   3   3 ;; OTH
   0   3   3   3   3   3   3   3   2 ;; ASC
   0   3   3   3   1   1   3   3   2 ;; ASS
   0   3   3   3   1   2   1   2   1 ;; ACV
   0   3   3   3   3   3   3   3   1 ;; ACO
   0   3   1   3   1   1   1   3   1 ;; ASV
   0   3   1   3   1   1   3   3   1 ;; ASO
   0   3   2   2   2   2   1   1   1 ;; OTS
   ])
;;}}}

;;{{{  latin prober

(defun unicad-latin-group-prober (start end)
  "for latin-1 and latin-2"
  (let (latin1-conf latin2-conf)
    (save-excursion
      (setq latin1-conf
            (unicad-latin-prober start end
                                 unicad-latin1-class-table unicad-latin1-class-num unicad-latin1-model))
      (if (>= latin1-conf 0.5)
          (setq unicad-latin-best-guess (list 'latin-1 latin1-conf))
        (setq latin2-conf
              (unicad-latin-prober start end
                                   unicad-latin2-class-table unicad-latin2-class-num unicad-latin2-model))
        (if (> latin1-conf latin2-conf)
            (setq unicad-latin-best-guess (list 'latin-1 latin1-conf))
          (setq unicad-latin-best-guess (list 'latin-2 latin2-conf)))))
    (cadr unicad-latin-best-guess)))

(defun unicad-latin-prober (start end class-table class-num latin-model)
  (let ((mState 'eDetecting)
        (code0-class 1)                 ; OTH
        (mFreqCounter [0 0 0 0])
        (code0 0)
        code1-class code1 freq)
    (fillarray mFreqCounter 0)
    (save-excursion
      (goto-char start)
      (setq unicad-latin-best-guess '(latin-1 0.0))
      (while (and (< (point) end)
                  (not (eq mState 'eNotMe))
                  (< (aref mFreqCounter 3) 2000))
        (setq code1 (unicad-char-after))
        (forward-char 1)
        (if (and (= code0 #x0D) (= code1 #x0A))
            (setq unicad-eol 1))
        (setq code1-class (aref class-table code1))
        (setq freq (aref latin-model
                         (+ (* code0-class class-num)
                            code1-class)))
        (if (= freq 0)
            (setq mState 'eNotMe)
          (aset mFreqCounter freq (1+ (aref mFreqCounter freq)))
          )
        (setq code0-class code1-class
                code0 code1)))
    (unicad-latin-get-confidence mState mFreqCounter)))

(defun unicad-latin-get-confidence (mState mFreqCounter)
  (let ((confidence 0.0)
        (total 0))
    (if (eq mState 'eNotMe)
        unicad--sure-no
      (setq total (apply '+ (append mFreqCounter nil)))
      (when (> total 0)
        (setq confidence (- (/ (* (aref mFreqCounter 3) 1.0) total)
                            (/ (* (aref mFreqCounter 1) 20.0) total))))
      (max unicad--sure-no (* confidence 0.5)))))


;;}}}

;;{{{  HZ state machine

;; Esc Charset State Machine

;; static PRUint32 HZ-cls
(defconst unicad-hz-class-table
  [
   1 0 0 0 0 0 0 0 ;; 00 - 07
   0 0 0 0 0 0 0 0 ;; 08 - 0f
   0 0 0 0 0 0 0 0 ;; 10 - 17
   0 0 0 1 0 0 0 0 ;; 18 - 1f
   0 0 0 0 0 0 0 0 ;; 20 - 27
   0 0 0 0 0 0 0 0 ;; 28 - 2f
   0 0 0 0 0 0 0 0 ;; 30 - 37
   0 0 0 0 0 0 0 0 ;; 38 - 3f
   0 0 0 0 0 0 0 0 ;; 40 - 47
   0 0 0 0 0 0 0 0 ;; 48 - 4f
   0 0 0 0 0 0 0 0 ;; 50 - 57
   0 0 0 0 0 0 0 0 ;; 58 - 5f
   0 0 0 0 0 0 0 0 ;; 60 - 67
   0 0 0 0 0 0 0 0 ;; 68 - 6f
   0 0 0 0 0 0 0 0 ;; 70 - 77
   0 0 0 4 0 5 2 0 ;; 78 - 7f
   1 1 1 1 1 1 1 1 ;; 80 - 87
   1 1 1 1 1 1 1 1 ;; 88 - 8f
   1 1 1 1 1 1 1 1 ;; 90 - 97
   1 1 1 1 1 1 1 1 ;; 98 - 9f
   1 1 1 1 1 1 1 1 ;; a0 - a7
   1 1 1 1 1 1 1 1 ;; a8 - af
   1 1 1 1 1 1 1 1 ;; b0 - b7
   1 1 1 1 1 1 1 1 ;; b8 - bf
   1 1 1 1 1 1 1 1 ;; c0 - c7
   1 1 1 1 1 1 1 1 ;; c8 - cf
   1 1 1 1 1 1 1 1 ;; d0 - d7
   1 1 1 1 1 1 1 1 ;; d8 - df
   1 1 1 1 1 1 1 1 ;; e0 - e7
   1 1 1 1 1 1 1 1 ;; e8 - ef
   1 1 1 1 1 1 1 1 ;; f0 - f7
   1 1 1 1 1 1 1 1 ;; f8 - ff
   ])


;; static PRUint32 HZ_st

(defconst unicad-hz-state-table
  (let ((eStart 0)
        (eError 1)
        (eItsMe 2))
    (vector
     eStart eError      3 eStart eStart eStart eError eError ;;00-07
     eError eError eError eError eItsMe eItsMe eItsMe eItsMe ;;08-0f
     eItsMe eItsMe eError eError eStart eStart      4 eError ;;10-17
          5 eError      6 eError      5      5      4 eError ;;18-1f
          4 eError      4      4      4 eError      4 eError ;;20-27
          4 eItsMe eStart eStart eStart eStart eStart eStart ;;28-2f
     )))

;; static const PRUint32 HZCharLenTable[] = {0  0  0  0  0  0};

(defconst unicad-hz-charlen-table
  [0  0  0  0  0  0])

(defvar unicad-hz-sm-model
  (list
   (cons 'classTable unicad-hz-class-table)
   (cons 'classFactor 6)
   (cons 'stateTable unicad-hz-state-table)
   (cons 'charLenTable unicad-hz-charlen-table)
   (cons 'name "HZ-GB-2312")))

;;}}}
;;{{{  iso-2022-cn state machine

(defconst unicad-iso2022cn-class-table
  [
   2 0 0 0 0 0 0 0 ;; 00 - 07
   0 0 0 0 0 0 0 0 ;; 08 - 0f
   0 0 0 0 0 0 0 0 ;; 10 - 17
   0 0 0 1 0 0 0 0 ;; 18 - 1f
   0 0 0 0 0 0 0 0 ;; 20 - 27
   0 3 0 0 0 0 0 0 ;; 28 - 2f
   0 0 0 0 0 0 0 0 ;; 30 - 37
   0 0 0 0 0 0 0 0 ;; 38 - 3f
   0 0 0 4 0 0 0 0 ;; 40 - 47
   0 0 0 0 0 0 0 0 ;; 48 - 4f
   0 0 0 0 0 0 0 0 ;; 50 - 57
   0 0 0 0 0 0 0 0 ;; 58 - 5f
   0 0 0 0 0 0 0 0 ;; 60 - 67
   0 0 0 0 0 0 0 0 ;; 68 - 6f
   0 0 0 0 0 0 0 0 ;; 70 - 77
   0 0 0 0 0 0 0 0 ;; 78 - 7f
   2 2 2 2 2 2 2 2 ;; 80 - 87
   2 2 2 2 2 2 2 2 ;; 88 - 8f
   2 2 2 2 2 2 2 2 ;; 90 - 97
   2 2 2 2 2 2 2 2 ;; 98 - 9f
   2 2 2 2 2 2 2 2 ;; a0 - a7
   2 2 2 2 2 2 2 2 ;; a8 - af
   2 2 2 2 2 2 2 2 ;; b0 - b7
   2 2 2 2 2 2 2 2 ;; b8 - bf
   2 2 2 2 2 2 2 2 ;; c0 - c7
   2 2 2 2 2 2 2 2 ;; c8 - cf
   2 2 2 2 2 2 2 2 ;; d0 - d7
   2 2 2 2 2 2 2 2 ;; d8 - df
   2 2 2 2 2 2 2 2 ;; e0 - e7
   2 2 2 2 2 2 2 2 ;; e8 - ef
   2 2 2 2 2 2 2 2 ;; f0 - f7
   2 2 2 2 2 2 2 2 ;; f8 - ff
   ])


(defconst unicad-iso2022cn-state-table
  (let ((eStart 0)
        (eError 1)
        (eItsMe 2))
    (vector
     eStart      3 eError eStart eStart eStart eStart eStart ;;00-07
     eStart eError eError eError eError eError eError eError ;;08-0f
     eError eError eItsMe eItsMe eItsMe eItsMe eItsMe eItsMe ;;10-17
     eItsMe eItsMe eItsMe eError eError eError      4 eError ;;18-1f
     eError eError eError eItsMe eError eError eError eError ;;20-27
     5      6 eError eError eError eError eError eError      ;;28-2f
     eError eError eError eItsMe eError eError eError eError ;;30-37
     eError eError eError eError eError eItsMe eError eStart ;;38-3f
     )))

(defconst unicad-iso2022cn-charlen-table
  [0  0  0  0  0  0])

(defvar unicad-iso2022cn-sm-model
  (list
   (cons 'classTable unicad-iso2022cn-class-table)
   (cons 'classFactor 9)
   (cons 'stateTable unicad-iso2022cn-state-table)
   (cons 'charLenTable unicad-iso2022cn-charlen-table)
   (cons 'name "ISO-2022-CN")))

;;}}}
;;{{{  iso-2022-jp state machine

(defconst unicad-iso2022jp-class-table
  [
   2 0 0 0 0 0 0 0 ;; 00 - 07
   0 0 0 0 0 0 2 2 ;; 08 - 0f
   0 0 0 0 0 0 0 0 ;; 10 - 17
   0 0 0 1 0 0 0 0 ;; 18 - 1f
   0 0 0 0 7 0 0 0 ;; 20 - 27
   3 0 0 0 0 0 0 0 ;; 28 - 2f
   0 0 0 0 0 0 0 0 ;; 30 - 37
   0 0 0 0 0 0 0 0 ;; 38 - 3f
   6 0 4 0 8 0 0 0 ;; 40 - 47
   0 9 5 0 0 0 0 0 ;; 48 - 4f
   0 0 0 0 0 0 0 0 ;; 50 - 57
   0 0 0 0 0 0 0 0 ;; 58 - 5f
   0 0 0 0 0 0 0 0 ;; 60 - 67
   0 0 0 0 0 0 0 0 ;; 68 - 6f
   0 0 0 0 0 0 0 0 ;; 70 - 77
   0 0 0 0 0 0 0 0 ;; 78 - 7f
   2 2 2 2 2 2 2 2 ;; 80 - 87
   2 2 2 2 2 2 2 2 ;; 88 - 8f
   2 2 2 2 2 2 2 2 ;; 90 - 97
   2 2 2 2 2 2 2 2 ;; 98 - 9f
   2 2 2 2 2 2 2 2 ;; a0 - a7
   2 2 2 2 2 2 2 2 ;; a8 - af
   2 2 2 2 2 2 2 2 ;; b0 - b7
   2 2 2 2 2 2 2 2 ;; b8 - bf
   2 2 2 2 2 2 2 2 ;; c0 - c7
   2 2 2 2 2 2 2 2 ;; c8 - cf
   2 2 2 2 2 2 2 2 ;; d0 - d7
   2 2 2 2 2 2 2 2 ;; d8 - df
   2 2 2 2 2 2 2 2 ;; e0 - e7
   2 2 2 2 2 2 2 2 ;; e8 - ef
   2 2 2 2 2 2 2 2 ;; f0 - f7
   2 2 2 2 2 2 2 2 ;; f8 - ff
   ])


(defconst unicad-iso2022jp-state-table
  (let ((eStart 0)
        (eError 1)
        (eItsMe 2))
    (vector
     eStart      3 eError eStart eStart eStart eStart eStart ;;00-07
     eStart eStart eError eError eError eError eError eError ;;08-0f
     eError eError eError eError eItsMe eItsMe eItsMe eItsMe ;;10-17
     eItsMe eItsMe eItsMe eItsMe eItsMe eItsMe eError eError ;;18-1f
     eError      5 eError eError eError      4 eError eError ;;20-27
     eError eError eError      6 eItsMe eError eItsMe eError ;;28-2f
     eError eError eError eError eError eError eItsMe eItsMe ;;30-37
     eError eError eError eItsMe eError eError eError eError ;;38-3f
     eError eError eError eError eItsMe eError eStart eStart ;;40-47
     )))

(defconst unicad-iso2022jp-charlen-table
  [0  0  0  0  0  0])

(defvar unicad-iso2022jp-sm-model
  (list
   (cons 'classTable unicad-iso2022jp-class-table)
   (cons 'classFactor 10)
   (cons 'stateTable unicad-iso2022jp-state-table)
   (cons 'charLenTable unicad-iso2022jp-charlen-table)
   (cons 'name "ISO-2022-JP")))

;;}}}
;;{{{  iso-2022-kr state machine

(defconst unicad-iso2022kr-class-table
  [
   2 0 0 0 0 0 0 0 ;; 00 - 07
   0 0 0 0 0 0 0 0 ;; 08 - 0f
   0 0 0 0 0 0 0 0 ;; 10 - 17
   0 0 0 1 0 0 0 0 ;; 18 - 1f
   0 0 0 0 3 0 0 0 ;; 20 - 27
   0 4 0 0 0 0 0 0 ;; 28 - 2f
   0 0 0 0 0 0 0 0 ;; 30 - 37
   0 0 0 0 0 0 0 0 ;; 38 - 3f
   0 0 0 5 0 0 0 0 ;; 40 - 47
   0 0 0 0 0 0 0 0 ;; 48 - 4f
   0 0 0 0 0 0 0 0 ;; 50 - 57
   0 0 0 0 0 0 0 0 ;; 58 - 5f
   0 0 0 0 0 0 0 0 ;; 60 - 67
   0 0 0 0 0 0 0 0 ;; 68 - 6f
   0 0 0 0 0 0 0 0 ;; 70 - 77
   0 0 0 0 0 0 0 0 ;; 78 - 7f
   2 2 2 2 2 2 2 2 ;; 80 - 87
   2 2 2 2 2 2 2 2 ;; 88 - 8f
   2 2 2 2 2 2 2 2 ;; 90 - 97
   2 2 2 2 2 2 2 2 ;; 98 - 9f
   2 2 2 2 2 2 2 2 ;; a0 - a7
   2 2 2 2 2 2 2 2 ;; a8 - af
   2 2 2 2 2 2 2 2 ;; b0 - b7
   2 2 2 2 2 2 2 2 ;; b8 - bf
   2 2 2 2 2 2 2 2 ;; c0 - c7
   2 2 2 2 2 2 2 2 ;; c8 - cf
   2 2 2 2 2 2 2 2 ;; d0 - d7
   2 2 2 2 2 2 2 2 ;; d8 - df
   2 2 2 2 2 2 2 2 ;; e0 - e7
   2 2 2 2 2 2 2 2 ;; e8 - ef
   2 2 2 2 2 2 2 2 ;; f0 - f7
   2 2 2 2 2 2 2 2 ;; f8 - ff
   ])


(defconst unicad-iso2022kr-state-table
  (let ((eStart 0)
        (eError 1)
        (eItsMe 2))
    (vector
     eStart      3 eError eStart eStart eStart eError eError ;;00-07
     eError eError eError eError eItsMe eItsMe eItsMe eItsMe ;;08-0f
     eItsMe eItsMe eError eError eError      4 eError eError ;;10-17
     eError eError eError eError      5 eError eError eError ;;18-1f
     eError eError eError eItsMe eStart eStart eStart eStart ;;20-27
     )))

(defconst unicad-iso2022kr-charlen-table
  [0  0  0  0  0  0])

(defvar unicad-iso2022kr-sm-model
  (list
   (cons 'classTable unicad-iso2022kr-class-table)
   (cons 'classFactor 6)
   (cons 'stateTable unicad-iso2022kr-state-table)
   (cons 'charLenTable unicad-iso2022kr-charlen-table)
   (cons 'name "ISO-2022-KR")))

;;}}}

;;{{{  Esc CharSet Prober

(defvar unicad-esc-group-list
  (list
   'unicad-hz-prober
   'unicad-iso2022cn-prober
   'unicad-iso2022jp-prober
   'unicad-iso2022kr-prober))

(defvar unicad-esc-group-guess nil)

(defun unicad-esc-group-prober (start end)
  (let ((lists unicad-esc-group-list)
        (mState 'eDetecting)
        (mBestGuess nil)
        (bestConf 0.0)
        state)
    (setq unicad-esc-group-guess nil)
    (while (and lists (eq mState 'eDetecting))
      (setq state (funcall (pop lists)
                           start end))
      (cond
       ((eq state 'eItsMe)
        (setq mState 'eFoundIt)
        (setq mBestGuess (car (nth 0 unicad-esc-group-guess)))
        (setq bestConf   (cdr (nth 0 unicad-esc-group-guess)))
        )
       ((eq state 'eNotMe) nil)
       ))
    ))

(defvar unicad-hz-name 'hz-gb-2312)
(defvar unicad-iso2022cn-name 'iso-2022-cn)
(defvar unicad-iso2022jp-name 'iso-2022-jp)
(defvar unicad-iso2022kr-name 'iso-2022-kr)

(defsubst unicad-hz-prober (start end)
  (unicad-esc-charset-prober
   start end unicad-hz-name unicad-hz-sm-model))

(defsubst unicad-iso2022cn-prober (start end)
  (unicad-esc-charset-prober
   start end unicad-iso2022cn-name unicad-iso2022cn-sm-model))

(defsubst unicad-iso2022jp-prober (start end)
  (unicad-esc-charset-prober
   start end unicad-iso2022jp-name unicad-iso2022jp-sm-model))

(defsubst unicad-iso2022kr-prober (start end)
  (unicad-esc-charset-prober
   start end unicad-iso2022kr-name unicad-iso2022kr-sm-model))

(defun unicad-esc-charset-prober (start end charset-name model)
  (let ((mState 'eDetecting)
        (code0 0)
        (code1 0)
        codingState)
    (unicad-sm-reset)
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (eq mState 'eDetecting))
        (setq code1 (unicad-char-after))
        (forward-char 1)
        (if (and (= code0 #x0D) (= code1 #x0A))
            (setq unicad-eol 1))
        (setq codingState (unicad-next-state code1 model))
        (cond
         ((= codingState unicad--eError)
          (setq mState 'eNotMe)
          (push (cons charset-name unicad--sure-no) unicad-esc-group-guess))
         ((= codingState unicad--eItsMe)
          (setq mState 'eItsMe)
          (push (cons charset-name unicad--sure-yes) unicad-esc-group-guess))))
      )))

;;}}}

(provide 'unicad)

;;; unicad.el ends here
;;; Local Variables:
;;; coding: utf-8-unix
;;; End:

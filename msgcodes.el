;;; msgcodes.el --- common and rare Usenet message "encodings"
 
;; Copyright (C) 2003, 2004 Michael Schierl
 
;; Author: Michael Schierl <schierlm-public@gmx.de>
;; Keywords: usenet codes kenny morse base64 hex reverse
;; Version: 0.4
 
(defconst msgcode-version "0.4"
  "Version of msgcodes.el.")
 
;; This file is not part of GNU Emacs.
 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;;; Commentary:
 
;; provides convenience functions for different "encodings" used in
;; e-mails and usenet articles.
 
;; for GNU emacs 21
 
;;; Installation:
 
;; load (or autoload) this file, and run (msgcode-init-keys)
;; to bind it to the default keys (C-c m ...). Alternatively, run
;; (msgcode-init-keymap) and bind any key to `msgcode-map'.
 
;; This will provide the following keys:
 
;; C-c m m  `msgcode-morse-autocode'
;; C-c m <  `msgcode-backwards-region'
;; C-c m n  `msgcode-n-ary-autocode'
;; C-c m k  `msgcode-kenny-autocode'
;; C-c m r  `msgcode-rot13-region'
;; C-c m b  `msgcode-base64-autocode'
;; C-c m h  `msgcode-hex-autocode'
 
;; Note that hex, backwards and n-ary encoding use the prefix
;; argument.  See the docstrings for details.
 
;;; Known bugs:
 
;; Each of this functions SHOULD leave the point at beginning/end of
;; encoded region when it was at beginning/end of unencoded region
;; when calling it. But some sometimes do not. (that's because I
;; collected them from several places and it does not work as it
;; should...). Rationale: You should be able to "cascade" multiple
;; functions without selecting everything again.
 
;;; History:
 
;; i hate history...
 
;;; Code:
 
(require 'cl)
 
(defvar msgcode-rot13-table
  (eval-when-compile
    (let ((i -1)
   (table (make-string 256 0)))
      (while (< (incf i) 256)
 (aset table i i))
      (concat
       (substring table 0 ?A)
       (substring table ?N (1+ ?Z))
       (substring table ?A ?N)
       (substring table (1+ ?Z) ?a)
       (substring table ?n (1+ ?z))
       (substring table ?a ?n)
       (substring table (1+ ?z) 255)))))
 
(defvar msgcode-map (make-sparse-keymap))
(define-key msgcode-map (kbd "h") 'msgcode-hex-autocode)
(define-key msgcode-map (kbd "b") 'msgcode-base64-autocode)
(define-key msgcode-map (kbd "r") 'msgcode-rot13-region)
(define-key msgcode-map (kbd "k") 'msgcode-kenny-autocode)
(define-key msgcode-map (kbd "n") 'msgcode-n-ary-autocode)
(define-key msgcode-map (kbd "<") 'msgcode-backwards-region)
(define-key msgcode-map (kbd "m") 'msgcode-morse-autocode)
 
(defconst msgcode-hex-digits "0123456789ABCDEF")
(defconst msgcode-digits "0123456789abcdefghijklmnopqrstuvwxyz")
 
;; generic helper functions/macros
 
(defun msgcode-match-select (beg end args string fun1 fun2)
  "Run one of two functions depending on contents of a region.
Region goes from BEG to END.  ARGS are passed to the functions
additionally to BEG and END.  When the region contains a match for
STRING, call FUN1, else call FUN2."
  (if (string-match string (buffer-substring-no-properties beg end))
      (apply fun1 beg end args)
    (apply fun2 beg end args)))
 
(defun msgcode-apply-on-region (beg end args func)
  "Apply a function requireing a string to a region's substring.
Region goes from BEG to END.  ARGS are passe to the function,
additionally to the region contents.  FUNC is the function to be
called."
  (let ((text (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (push-mark)
    (insert (apply func text args))))
 
;; rot13
 
;;;###autoload
(defun msgcode-rot13-region (beg end)
  "ROT13 encode the region from BEG to END."
  (interactive "*r")
  (translate-region beg end msgcode-rot13-table))
 
;; kenny
 
;;;###autoload
(defun msgcode-kenny-autocode (beg end)
  "Automatically encode or decode kenny in region from BEG to END."
  (interactive "*r")
  (msgcode-match-select beg end nil "[A-EG-LN-OQ-Za-eg-ln-oq-z]"
   'msgcode-kenny-encode-region
   'msgcode-kenny-decode-region))
 
;;;###autoload
(defun msgcode-kenny-encode-region (beg end)
  "Kennify the region from BEG to END."
  (interactive "*r")
  (msgcode-apply-on-region beg end nil 'msgcode-kenny-encode-string))
 
;;;###autoload
(defun msgcode-kenny-encode-string (str)
  "Kennify a string STR."
  (mapconcat 'msgcode-internal-kenny-encode-char str ""))
 
(defun msgcode-internal-kenny-encode-char (val)
  "Encode one character, VAL."
  (cond
   ((and (>= val ?a) (<= val ?z))
    (msgcode-internal-kenny-encode-char-0 (- val ?a) 3))
   ((and (>= val ?A) (<= val ?Z))
    (upcase-initials (msgcode-internal-kenny-encode-char-0 (- val ?A) 3)))
   (t (char-to-string val))))
 
(defun msgcode-internal-kenny-encode-char-0 (val num)
  "Internal function.
Encode one character into a string.  This function sd recursive.  VAL
specifies which character to encode and NUM how many characters are
left."
  (if (= num 0)
      ""
    (concat (msgcode-internal-kenny-encode-char-0 (floor val 3) (1- num))
     (substring "mpf" (% val 3) (1+ (% val 3))))))
 
;;;###autoload
(defun msgcode-kenny-decode-region (beg end)
  "Decode a kenny region from BEG to END."
  (interactive "*r")
  (msgcode-apply-on-region beg end nil 'msgcode-kenny-decode-string))
 
;;;###autoload
(defun msgcode-kenny-decode-string (str)
  "Decode a kenny string STR."
(let ((rest str)
 match
 (result "")
 (case-fold-search nil))
    (while (not (string= rest ""))
      (cond
       ((string-match "\\`[mpf][mpf][mpf]" rest)
 (setq match (match-string-no-properties 0 rest)
       rest (substring rest 3)
       result (concat result
          (msgcode-internal-kenny-decode-chars match 0))))
       ((string-match "\\`[MPF][mpf][mpf]" rest)
 (setq match (match-string-no-properties 0 rest)
       rest (substring rest 3)
       result (concat result
          (upcase (msgcode-internal-kenny-decode-chars
     (downcase match) 0)))))
       ((string-match "\\`[MPFmpf]" rest)
 (setq result (concat result "?")
       rest (substring rest 1)))
       (t (setq result (concat result (substring rest 0 1))
  rest (substring rest 1)))))
    result))
  
(defun msgcode-internal-kenny-decode-chars (str val)
  "Internal function.
Decode string STR.
Argument VAL is stupid."
  (if (string= "" str) (char-to-string (+ ?a val))
  (let* ((char1 (substring str 0 1))
 (rest (substring str 1))
 (pos (string-match  char1 "mpf")))
 (if pos
     (msgcode-internal-kenny-decode-chars rest (+ (* 3 val) pos))
   "?"))))
 
;; morse
    
;;;###autoload
(defun msgcode-morse-autocode (beg end)
  "Automatically encode or decode morse in region from BEG to END."
(interactive "*r")
  (msgcode-match-select beg end nil "[a-zA-Z]"
   'morse-region
   'unmorse-region))
;; base64
 
;;;###autoload
(defun msgcode-base64-autocode (beg end)
  "Automatically encode or decode a base64 region from BEG to END."
  (interactive "*r")
  (condition-case nil
      (base64-decode-region beg end)
      (error (base64-encode-region beg end))))
 
;; hex
 
;;;###autoload
(defun msgcode-hex-autocode (beg end &optional arg)
  "Automatically encode or decode morse in region from BEG to END.
Prefix argument ARG forces encoding -- the arg is interpreted as
described for `msgcode-hex-encode-region'."
  (interactive "*r\nP")
  (cond
   (arg (msgcode-hex-encode-region beg end arg))
   ((string-match "^[0-9A-Fa-f ]\\{7\\}"
    (buffer-substring-no-properties beg end))
    (msgcode-hex-decode-region beg end))
   (t (msgcode-hex-encode-region beg end arg))))
  
;;;###autoload
(defun msgcode-hex-encode-region (beg end &optional arg)
  "Hex encode region from BEG to END.
If prefix ARG is one or more `\\[universal-argument]'s, add a
cleartext section as well.  With a numeric argument, show that many
columns instead.  With a `\\[universal-argument] -', just encode as
with no arg."
  (interactive "*r\nP")
  (let ((result "")
 (width 16) (extra nil)
 (text (buffer-substring-no-properties (point) (mark))))
  (cond
   ((not arg) nil)
   ((listp arg) (setq extra t))
   ((numberp arg) (setq width arg))
   (t nil))
  (setq result (msgcode-hex-encode-string text width extra))
  (delete-region (point) (mark))
  (push-mark)
  (insert result)))
 
(defun msgcode-hex-encode-string (str width extra)
  "Encode a hex string STR.
WIDTH specifies how many hex columns should be used.  If EXTRA is
non-nil, add a cleartext section."
  (cond
   ((> (length str) width)
    (concat (msgcode-hex-encode-string (substring str 0 width) width extra)
     "\n"
     (msgcode-hex-encode-string (substring str width) width extra)))
   (extra
    (concat (msgcode-hex-encode-string str width nil)
     (make-string (* 3 (- width (length str))) ? )
     "   " (mapconcat 'msgcode-mask-specials str "")))
   (t
    (mapconcat 'msgcode-hex-encode-char str ""))))
         
(defun msgcode-hex-encode-char (char)
  "Encode one hex character CHAR."
  (concat  (substring msgcode-hex-digits (floor char 16) (1+ (floor char 16)))
    (substring msgcode-hex-digits (% char 16) (1+ (% char 16)))
    " "))
 
(defun msgcode-mask-specials (char)
  "Mask control characters for cleartext section of hex encoding.
Each CHAR is processed isolated."
  (if (< char 33) "." (char-to-string char)))
 
;;;###autoload
(defun msgcode-hex-decode-region (beg end)
  "Decode a hex-encoded region from BEG to END."
  (interactive "*r")
  (msgcode-apply-on-region beg end nil 'msgcode-hex-decode-string))
 
;;;###autoload
(defun msgcode-hex-decode-string (str)
  "Decode a hex encoded string STR."
  (let ((rest str)
 match
 (result "")
 (case-fold-search t))
    (while (not (string= rest ""))
      (cond
       ((string-match "^  +[^ \n]+\n?" rest) ;; eat clear text parts
 (setq rest (substring rest (match-end 0))))
       ((string-match "^ [^ ]\\|^ $" rest) ;; eat single spaces
 (setq rest (substring rest 1)))
       ((string-match "^[0-9A-Fa-f]\\{1,2\\}" rest) ;; hex char
 (setq match (match-string-no-properties 0 rest)
       rest (substring rest (match-end 0))
       result (concat result
        (msgcode-internal-unhex match))))
       (t (setq result (concat result (substring rest 0 1))
  rest (substring rest 1)))))
    result))
 
(defun msgcode-internal-unhex (str)
  "Internal function.
Unhexify string STR."
  (char-to-string
   (cond
    ((string= "" str)
       0)
    ((= (length str) 1)
     (string-match (upcase (substring str 0 1)) msgcode-hex-digits))
    ((= (length str) 2)
     (+ (* 16  (string-match (upcase (substring str 0 1)) msgcode-hex-digits))
 (string-match (upcase (substring str 1)) msgcode-hex-digits)))
    (t (error "String too long")))))
   
;;;###autoload
(defun msgcode-n-ary-encode-region (beg end &optional base)
  "N-ary encode region from BEG to END.
Optional argument BASE specifies which base to use (Default 2)."
  (interactive "*r\nP")
  (flet ((num-to-string (num)
    (substring msgcode-digits num (1+ num)))
  (number-to-base (num base)
    (if (> num 0)
      (let ((n ""))
        (while (> num 0)
   (setq n (concat (num-to-string (mod num base)) n)
         num (floor num base)))
        n)
      "0"))
  (char_to_int (char) char)
  (string-to-num (str base)
    (number-to-base (char_to_int (string-to-char str)) base)))
    (save-excursion
      (save-restriction
 (narrow-to-region beg end)
 (goto-char (point-min))
 (while (re-search-forward "[^ \r\n\t\f.!?]" nil t)
   (let* ((base (or base 2))
   (base (if (consp base) (car base) base))
   (num (string-to-num (match-string 0) base)))
     (delete-char -1)
     (insert " " num)))
 (goto-char (point-max))))))
 
;;;###autoload
(defun msgcode-n-ary-decode-region (beg end &optional base)
  "N-ary decode region from BEG to END.
Optional argument BASE specifies which base to use (Default 2)."
  (interactive "*r\nP")
  (let* ((base (or base 2))
  (base (if (consp base) (car base) base))
  (reg (concat " ?[" (substring msgcode-digits 0 base) "]+")))
    (flet ((string-to-num (str base place)
      (reduce #'(lambda (char sum)
    (save-match-data
      (+ sum
         (* (expt base (incf place))
     (string-match (char-to-string char)
            msgcode-digits)))))
    (remove ?\ str)
    :initial-value 0
    :from-end t)))
      (save-excursion
 (save-restriction
   (narrow-to-region beg end)
   (goto-char (point-min))
   (while (re-search-forward reg nil t)
     (let ((num (string-to-num (match-string 0) base -1)))
       (when (< 31 num)
  (delete-region (match-beginning 0) (match-end 0))
  (insert (format "%c" num))))))))))
 
;;;###autoload
(defun msgcode-n-ary-autocode (beg end base)
  "Automatically N-ary encode or decode region from BEG to END.
Optional argument BASE specifies which base to use (Default 2)."
  (interactive "*r\nP")
 
  (let* ((b (or base 2))
  (b (if (consp b) (car b) b))
  (undecodable-regex
   (concat "[^" (substring msgcode-digits 0 b) " \n\r\t\f.!?]")))
    (msgcode-match-select beg end (list base) undecodable-regex
     'msgcode-n-ary-encode-region
     'msgcode-n-ary-decode-region)))
 
;; backwards
 
(defun msgcode-backwards-region (beg end fill)
  "Print a region from BEG to END backwards (line for line).
Optional argument FILL specifies that the lines should be filled to
equal length with spaces first."
  (interactive "*r\nP")
  (msgcode-apply-on-region beg end (list fill) 'msgcode-backwards-string))
   
(defun msgcode-backwards-string (str &optional fill)
  "Print a string STR backwards (line for line).
Optional argument FILL specifies that the lines should be filled to
equal length with spaces first."
  (let ((beg "") (end "") linelist (s str))
    (while (string= (substring s 0 1) "\n")
      (setq beg (concat beg "\n")
     s (substring s 1)))
    (while (string= (substring s -1) "\n")
      (setq end (concat end "\n")
     s (substring s 0 -1)))
    (setq linelist (split-string s "\n"))
    (if fill
 (let ((maxlen 0))
   (mapc (lambda (line)
    (if (> (length line) maxlen)
        (setq maxlen (length line))))
  linelist)
   (setq linelist (mapcar (lambda (line)
       (concat line (make-string (- maxlen
        (length line))
            ? )))
     linelist))))
    (concat beg
     (mapconcat (lambda (line)
    (let ((revline ""))
      (mapc
       (lambda (letter)
         (setq revline (concat (char-to-string letter)
          revline)))
       line)
      revline))
         linelist "\n")
     end)))
 
;; keybindings
 
;;;###autoload
(defun msgcode-init-keymap ()
  "Initialize the key map by autoload side effect."
  nil)
 
;;;###autoload
(defun msgcode-init-keys ()
  "Bind `C-c m' to the msgcode key map."
  (interactive)
  (unless msgcode-map (msgcode-init-keymap))
  (global-set-key (kbd "C-c m") msgcode-map))
 
(provide 'msgcodes)
 
;;; msgcodes.el ends here

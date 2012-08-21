;;; string-utils.el --- String-manipulation utilities
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker walker@pobox.com
;; URL: https://github.com/rolandwalker/string-utils.el
;; Version: 0.0.1
;; Last-Updated: 21 Aug 2012
;; EmacsWiki: StringUtils
;; Keywords:
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; String-utils is a collection of functions for string manipulation.
;; This library has no user-level interface; it is only useful
;; for programming in Emacs Lisp.
;;
;; The following functions are provided:
;;
;;    string-utils-stringify-anything
;;    string-utils-has-darkspace-p
;;    string-utils-has-whitespace-p
;;    string-utils-trim-whitespace
;;    string-utils-compress-whitespace
;;    string-utils-string-repeat
;;    string-utils-escape-double-quotes
;;    string-utils-quotemeta
;;    string-utils-pad
;;    string-utils-pad-list
;;
;; To use string-utils, place the string-utils.el library somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;    (require 'string-utils)
;;
;; Notes
;;
;; Compatibility
;;
;;    Tested only on GNU Emacs version 24.1
;;
;; Bugs
;;
;; TODO
;;
;;; License
;;
;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Roland Walker "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Roland Walker or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Roland Walker.
;;
;;; Code:
;;

;;; requires

;; for callf, callf2
(eval-when-compile
  (require 'cl))

;; variables

(defvar string-utils-whitespace (concat [#x0000d  ; "Carriage Return (CR)"
                                         #x00088  ; "Character Tabulation Set"
                                         #x00089  ; "Character Tabulation With Justification"
                                         #x00009  ; "Character Tabulation" (ordinary ASCII tab)
                                         #x0034f  ; "Combining Grapheme Joiner"
                                         #x02001  ; "Em Quad"
                                         #x02003  ; "Em Space"
                                         #x02000  ; "En Quad"
                                         #x02002  ; "En Space"
                                         #x02007  ; "Figure Space"
                                         #x0000c  ; "Form Feed (FF)"
                                         #x02005  ; "Four-Per-Em Space"
                                         #x0200a  ; "Hair Space"
                                         #x03000  ; "Ideographic Space"
                                         #x0000a  ; "Line Feed (LF)"
                                         #x02028  ; "Line Separator"
                                         #x0008a  ; "Line Tabulation Set"
                                         #x0000b  ; "Line Tabulation"
                                         #x0205f  ; "Medium Mathematical Space"
                                         #x0180e  ; "Mongolian Vowel Separator"
                                         #x0202f  ; "Narrow No-Break Space"
                                         #x00085  ; "Next Line (NEL)"
                                         #x000a0  ; "No-Break Space"
                                         #x01680  ; "Ogham Space Mark"
                                         #x02029  ; "Paragraph Separator"
                                         #x02008  ; "Punctuation Space"
                                         #x02006  ; "Six-Per-Em Space"
                                         #x00020  ; "Space" (ordinary ASCII space)
                                         #xe0020  ; "Tag Space"
                                         #x02009  ; "Thin Space"
                                         #x02004  ; "Three-Per-Em Space"
                                         #x02d7f  ; "Tifinagh Consonant Joiner"
                                         #x02060  ; "Word Joiner"
                                         #x0200d  ; "Zero Width Joiner"
                                         #x0feff  ; "Zero Width No-Break Space"
                                         #x0200c  ; "Zero Width Non-Joiner"
                                         #x0200b  ; "Zero Width Space"
                                         ])
  "Definition of whitespace characters used by string-utils.

Includes Unicode whitespace characters.")

(defvar string-utils-whitespace-ascii " \n\t\r\f" "ASCII-only whitespace characters used by string-utils.")

;;; utility functions

;;;###autoload
(defun string-utils-stringify-anything (obj)
  "Coerce any object OBJ into a string.

Contrary to usual conventions, return the empty string for nil."
  (if (null obj)
    ""
    ;; else
    (typecase obj
      (string   obj)
      (symbol   (symbol-name obj))
      (integer  (number-to-string obj))
      (float    (number-to-string obj))
      (t        (format "%s" obj)))))

;;;###autoload
(defun string-utils-has-darkspace-p (obj &optional ascii-only)
  "Test whether OBJ, when coerced to a string, has any non-whitespace characters.

Returns the position of the first non-whitespace character
on success.

If optional ASCII-ONLY is set, use an ASCII-only definition
of whitespace characters."
  (let ((str-val (if (stringp obj) obj (string-utils-stringify-anything obj))))
    (string-match-p (concat "[^" string-utils-whitespace "]+") str-val)))

;;;###autoload
(defun string-utils-has-whitespace-p (obj &optional ascii-only)
  "Test whether OBJ, when coerced to a string, has any whitespace characters.

Returns the position of the first whitespace character on
success.

If optional ASCII-ONLY is set, use an ASCII-only definition
of whitespace characters."
  (let ((str-val (if (stringp obj) obj (string-utils-stringify-anything obj)))
        (string-utils-whitespace (if ascii-only string-utils-whitespace-ascii string-utils-whitespace)))
    (string-match-p (concat "[" string-utils-whitespace "]+") str-val)))

;;;###autoload
(defun string-utils-trim-whitespace (str-val &optional ascii-only multi-line)
  "Return STR-VAL with leading and trailing whitespace removed.

If optional ASCII-ONLY is set, use an ASCII-only definition
of whitespace characters.

If optional MULTI-LINE is set, trim spaces at starts and
ends of all lines throughout STR-VAL."
  (let ((string-utils-whitespace (if ascii-only string-utils-whitespace-ascii string-utils-whitespace))
        (start-pat (if multi-line "^" "\\`"))
        (end-pat   (if multi-line "$" "\\'")))
    (save-match-data
      (replace-regexp-in-string (concat start-pat "[" string-utils-whitespace "]+") ""
         (replace-regexp-in-string (concat "[" string-utils-whitespace "]+" end-pat) ""
            str-val)))))

;;;###autoload
(defun string-utils-compress-whitespace (str-val &optional ascii-only)
  "Return STR-VAL with all contiguous whitespace compressed to one space.

If optional ASCII-ONLY is set, use an ASCII-only definition
of whitespace characters."
  (let ((string-utils-whitespace (if ascii-only string-utils-whitespace-ascii string-utils-whitespace)))
    (save-match-data
      (replace-regexp-in-string (concat "[" string-utils-whitespace "]+") " "
         str-val))))

;;;###autoload
(defun string-utils-string-repeat (str-val n)
  "Return a new string formed by repeating STR-VAL, N times.

STR-VAL may be of any length."
  (apply 'concat (make-list n str-val)))

;;;###autoload
(defun string-utils-escape-double-quotes (str-val)
  "Return STR-VAL with every double-quote escaped with backslash."
  (save-match-data
    (replace-regexp-in-string "\"" "\\\\\"" str-val)))

;;;###autoload
(defun string-utils-quotemeta (str-val)
  "Return STR-VAL with all non-word characters escaped with backslash.

This is more vigorous than `shell-quote-argument'."
  (save-match-data
    (replace-regexp-in-string "\\([^A-Za-z_0-9]\\)" "\\\\\\1" str-val)))

;;;###autoload
(defun string-utils-pad (str-val width &optional mode char throw-error)
  "Pad STR-VAL to WIDTH.

Optional MODE defaults to 'right, but may be 'left, 'center, or
an integer.

When MODE is 'left, padding characters are prepended.  When MODE
is 'center, padding characters are both appended and prepended so
that STR-VAL is centered within WIDTH.

When MODE is a positive integer, the behavior is fixed-position
padding.  Similar to 'center, padding may be added on the right
and on the left.  Exactly MODE-many padding characters are
added on the left before padding to the full WIDTH on the right.
When MODE is a negative integer, the behavior is the same, except
that MODE fixes the right-side padding.

Optional CHAR sets the padding character (defaults to space).

Optional THROW-ERROR throws an error if the length of STR-VAL
already exceeds WIDTH, or if the fixed-position padding requested
would cause the result to exceed WIDTH.  When THROW-ERROR is not
set (the default), a best-attempt result is always returned.

Tabs are expanded to spaces according to the value of
`tab-width'.

Returns a padded copy of string STR-VAL."
  (save-match-data
    (setq str-val (replace-regexp-in-string "\t" (make-string tab-width ?\s) str-val))
    (when (and throw-error
               (> (length str-val) width))
      (error "STR-VAL too wide"))
    (callf or char ?\s)
    (callf or mode 'right)
    (let ((total-pad 0)
          (left-pad 0)
          (right-pad 0))
      (when (> width (length str-val))
        (setq total-pad (- width (length str-val)))
        (when (and (numberp mode)
                   (> (abs mode) total-pad))
          (when throw-error
            (error "Fixed-position padding is too wide"))
          (setq mode (truncate (* total-pad (/ mode (abs mode))))))
        (cond
          ((eq mode 'left)
           (setq left-pad total-pad))
          ((eq mode 'right)
           (setq right-pad total-pad))
          ((eq mode 'center)
           (setq left-pad (truncate (* .5 total-pad)))
           (setq right-pad (- total-pad left-pad)))
          ((and (integerp mode)
                (< mode 0))
           (setq right-pad (abs mode))
           (setq left-pad (- total-pad (abs right-pad))))
          ((integerp mode)
           (setq left-pad mode)
           (setq right-pad (- total-pad left-pad)))
          (t
           (error "Bad padding MODE %s" mode))))
      (concat
       (make-string left-pad char)
       str-val
       (make-string right-pad char)))))

;;;###autoload
(defun string-utils-pad-list (str-list &optional additional-width target-width mode char throw-error)
  "Pad each member of STR-LIST to match the longest width.

ADDITIONAL-WIDTH sets a relative amount to pad beyond the longest
length.

TARGET-WIDTH sets an absolute target width, causing maximum
string length and ADDITIONAL-WIDTH to be ignored.

Optional MODE, CHAR, and THROW-ERROR are as for `string-utils-pad'.
Fixed-position MODE will attempt to pad all entries consistently,
based on any adjustments made to the longest member of STR-LIST.

Tabs are expanded to spaces according to the value of
`tab-width'.

Returns padded STR-LIST."
  (save-match-data
    (let ((width target-width)
          (max-width nil)
          (orig-mode mode))
      (callf2 mapcar #'(lambda (str)
                         (replace-regexp-in-string "\t" (make-string tab-width ?\s) str)) str-list)
      (setq max-width (apply 'max (mapcar #'length str-list)))
      (unless width
        (callf or additional-width 0)
        (setq width (+ additional-width max-width)))
      (when (and (numberp mode)
                 (> (+ (abs mode) max-width) width))
        (when throw-error
          (error "Fixed-position padding is too wide"))
        (setq mode (abs mode))
        (decf mode (- (+ (abs mode) max-width) width))
        (when (< mode 0)
          (setq mode 0))
        (when (< orig-mode 0)
          (setq mode (* -1 mode))))
      (mapcar #'(lambda (str)
                  (string-utils-pad str width mode char throw-error)) str-list))))

(provide 'string-utils)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; End:
;;
;; LocalWords:  StringUtils ARGS alist utils darkspace quotemeta
;;

;;; string-utils.el ends here

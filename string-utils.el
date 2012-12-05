;;; string-utils.el --- String-manipulation utilities
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/string-utils
;; URL: http://raw.github.com/rolandwalker/string-utils/master/string-utils.el
;; Version: 0.2.8
;; Last-Updated: 8 Nov 2012
;; Package-Requires: ((list-utils "0.1.2"))
;; EmacsWiki: StringUtils
;; Keywords: extensions
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'string-utils)
;;
;;     (string-utils-squeeze-filename (buffer-file-name (current-buffer)) 20)
;;
;;     (string-utils-stringify-anything (selected-frame))
;;
;;     (progn
;;       (message (string-utils-pad (buffer-name (current-buffer)) (window-width) 'right))
;;       (sit-for 1)
;;       (message (string-utils-pad (buffer-name (current-buffer)) (window-width) 'center))
;;       (sit-for 1)
;;       (message (string-utils-pad (buffer-name (current-buffer)) (window-width) 'left))
;;       (sit-for 1))
;;
;; Explanation
;;
;; String-utils is a collection of functions for string manipulation.
;; This library has no user-level interface; it is only useful
;; for programming in Emacs Lisp.
;;
;; The following functions are provided:
;;
;;     `string-utils-stringify-anything'
;;     `string-utils-has-darkspace-p'
;;     `string-utils-has-whitespace-p'
;;     `string-utils-trim-whitespace'
;;     `string-utils-compress-whitespace'
;;     `string-utils-string-repeat'
;;     `string-utils-escape-double-quotes'
;;     `string-utils-quotemeta'
;;     `string-utils-pad'
;;     `string-utils-pad-list'
;;     `string-utils-propertize-fillin'
;;     `string-utils-plural-ending'
;;     `string-utils-squeeze-filename'
;;     `string-utils-squeeze-url'
;;     `string-utils-split'
;;     `string-utils-truncate-to'
;;
;; To use string-utils, place the string-utils.el library somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'string-utils)
;;
;; Notes
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.3-devel     : yes, at the time of writing
;;     GNU Emacs version 24.1 & 24.2    : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.3 and lower : no
;;
;;     Uses if present: list-utils.el, obarray-fns.el
;;
;; Bugs
;;
;;     Some objects such as window-configuration are completely
;;     opaque and won't be stringified usefully.
;;
;; TODO
;;
;;     Stringification of autoload data type
;;         http://www.gnu.org/software/emacs/manual/html_node/elisp/Autoload-Type.html
;;
;;     Maybe args should not be included when stringifying lambdas and
;;     macros.
;;
;;     In string-utils-propertize-fillin, strip properties which are
;;     set to nil at start, which will create more contiguity in the
;;     result.  See this example, where the first two characters have
;;     the same properties
;;
;;         (let ((text "text"))
;;           (add-text-properties 0 1 '(face nil) text)
;;           (add-text-properties 2 3 '(face error) text)
;;           (string-utils-propertize-fillin text 'face 'highlight)
;;           text)
;;
;;      String-utils-squeeze-url needs improvement, sometimes using
;;      two elisions where one would do.
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

;;; requirements

;; for callf, callf2, assert, loop
(require 'cl)

(require 'eieio nil t)
(require 'list-utils nil t)
(require 'obarray-fns nil t)

(autoload 'font-lock-fillin-text-property "font-lock"
  "Fill in one property of the text from START to END.")

;;; declarations

(declare-function object-name-string "eieio.el")
(declare-function ring-elements      "ring.el")

;; variables

(defvar string-utils-whitespace (concat
                                 (apply 'vector
                                        (mapcar #'(lambda (x)
                                                    (decode-char 'ucs x))
                                                '(#x0000d  ; "Carriage Return (CR)"
                                                  #x00088  ; "Character Tabulation Set"
                                                  #x00089  ; "Character Tabulation With Justification"
                                                  #x00009  ; "Character Tabulation" (ordinary ASCII tab)
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
                                                  #x02060  ; "Word Joiner"
                                                  #x0feff  ; "Zero Width No-Break Space"
                                                  #x0200b  ; "Zero Width Space"
                                                  ))))
  "Definition of whitespace characters used by string-utils.

Includes Unicode whitespace characters.")

(defvar string-utils-whitespace-ascii " \n\t\r\f" "ASCII-only whitespace characters used by string-utils.")
(defvar string-utils-whitespace-syntax "\\s-"     "Whitespace regular expression according to `syntax-table'.")

;;; utility functions

;;;###autoload
(defun string-utils-stringify-anything (obj &optional separator ints-are-chars record-separator)
  "Coerce any object OBJ into a string.

Contrary to usual conventions, return the empty string for nil.

Sequences are flattened down to atoms and joined with string
SEPARATOR, which defaults to a single space.  Cyclic lists
may give unpredictable results (similar to `format') unless
list-utils.el is installed.

When INTS-ARE-CHARS is non-nil, interpret positive integers in
OBJ as characters.

Optional RECORD-SEPARATOR is a string (defaulting to the value of
SEPARATOR) which delimits end-of-record for paired data types
such as hash tables.

This is not a pretty-printer for OBJ, but a way to look at
the *contents* of OBJ (so much as is possible) as if it was
an ordinary string."
  (callf or separator " ")
  (callf or record-separator separator)
  (cond

    ;; nil
    ((null obj)
     "")

    ;; string
    ((stringp obj)
     obj)

    ;; symbol
    ((symbolp obj)
     (symbol-name obj))

    ;; character
    ((and
      ints-are-chars
      (characterp obj))
     (string obj))

    ;; number
    ((numberp obj)
     (number-to-string obj))

    ;; frame
    ((framep obj)
     (or (ignore-errors (frame-parameter obj 'name)) ""))

    ;; window
    ((windowp obj)
     (buffer-name (window-buffer obj)))

    ;; buffer
    ((bufferp obj)
     (buffer-name obj))

    ;; marker
    ((markerp obj)
     (string-utils-stringify-anything (list (marker-position obj)
                                            (marker-buffer obj)) separator ints-are-chars record-separator))
    ;; overlay
    ((overlayp obj)
     (string-utils-stringify-anything (list (overlay-start obj)
                                            (overlay-end obj)
                                            (overlay-buffer obj)) separator ints-are-chars record-separator))

    ;; network process
    ((and (processp obj)
          (eq 'network (process-type obj)))
     (let ((contact (process-contact obj t)))
       (cond
         ((and (plist-get contact :server)
               (or (plist-get contact :family)
                   (plist-get contact :service)))
          (format "%s:%s"
                  (or (plist-get contact :family) "")
                  (or (plist-get contact :service) "")))
         ((plist-get contact :host)
          (format "%s" (plist-get contact :host)))
         (t
          "network_process"))))

    ;; serial process
    ((and (processp obj)
          (eq 'serial (process-type obj)))
     (let ((contact (process-contact obj t)))
       (format "%s" (or (plist-get contact :name)
                        (plist-get contact :port)
                        "serial_process"))))

    ;; real process
    ((processp obj)
     (string-utils-stringify-anything (process-command obj) separator ints-are-chars record-separator))

    ;; ring
    ((ring-p obj)
     (string-utils-stringify-anything (ring-elements obj) separator ints-are-chars record-separator))

    ;; EIEIO object
    ((and (fboundp 'object-p)
          (object-p obj))
     (object-name-string obj))

    ;; font object
    ((fontp obj)
     (string-utils-stringify-anything (or (font-get obj :name)
                                          (font-get obj :family)
                                          "") separator ints-are-chars record-separator))

    ;; font vector as returned by `font-info'
    ((and (vectorp obj)
          (= 7 (length obj))
          (stringp (aref obj 0))
          (stringp (aref obj 1))
          (numberp (aref obj 2))
          (numberp (aref obj 3))
          (numberp (aref obj 4))
          (numberp (aref obj 5))
          (numberp (aref obj 6))
          (> (length (aref obj 1)) 0)
          (string-match-p "\\`\\(?:-[^-]+\\)\\{14,20\\}\\'" (aref obj 0)))
     (aref obj 1))

    ;; hash-table
    ((hash-table-p obj)
     (let ((output nil))
       (maphash #'(lambda (k v)
                    (push (string-utils-stringify-anything k separator ints-are-chars record-separator) output)
                    (push (string-utils-stringify-anything v separator ints-are-chars record-separator) output)) obj)
       (mapconcat 'identity
                  (nbutlast
                   (loop for (k v) on (nreverse output) by 'cddr
                         collect k
                         collect separator
                         collect v
                         collect record-separator)
                   (if (equal record-separator separator) 1 0))
                  "")))

    ;; char-table
    ((char-table-p obj)
     (let ((output nil))
       (map-char-table #'(lambda (k v)
                           (push (string-utils-stringify-anything k separator t record-separator) output)
                           (push (string-utils-stringify-anything v separator ints-are-chars record-separator) output)) obj)
       (mapconcat 'identity
                  (nbutlast
                   (loop for (k v) on (nreverse output) by 'cddr
                         collect k
                         collect separator
                         collect v
                         collect record-separator)
                   (if (equal record-separator separator) 1 0))
                  "")))

    ;; subr
    ((subrp obj)
     (subr-name obj))

    ;; compiled byte-code
    ((byte-code-function-p obj)
     (mapconcat #'(lambda (x)
                    (string-utils-stringify-anything x separator ints-are-chars record-separator)) (append obj nil) separator))

    ;; keymap, function, frame-configuration
    ((or (keymapp obj)
         (functionp obj)
         (frame-configuration-p obj))
     (string-utils-stringify-anything (cdr obj) separator ints-are-chars record-separator))

    ;; macro
    ((and (listp obj)
          (eq 'macro (car obj))
          (functionp (cdr obj)))
     (string-utils-stringify-anything (cddr obj) separator ints-are-chars record-separator))

    ;; list
    ((listp obj)
     (let* ((measurer (if (fboundp 'list-utils-safe-length) 'list-utils-safe-length 'safe-length))
            (len (funcall measurer obj)))
       (if (and (consp obj)
                (> len 0)
                (not (listp (nthcdr len obj))))
           ;; cons or improper list would choke mapconcat
           (string-utils-stringify-anything (append (subseq obj 0 len) (list (nthcdr len obj))) separator ints-are-chars record-separator)
         ;; else
         ;; accumulate output
         (let ((output nil))
           (push (string-utils-stringify-anything (car obj) separator ints-are-chars record-separator) output)
           (when (> len 1)
             ;; the subseq is to break cyclic lists
             (push (string-utils-stringify-anything (subseq obj 1 len) separator ints-are-chars record-separator) output))
           (mapconcat 'identity (nreverse output) separator)))))

    ;; defstruct
    ((and (vectorp obj)
          (symbolp (aref obj 0))
          (string-match-p "\\`cl-" (symbol-name (aref obj 0))))
     (mapconcat #'(lambda (x)
                    (string-utils-stringify-anything x separator ints-are-chars record-separator)) (cdr (append obj nil)) separator))

    ;; bool-vector
    ((bool-vector-p obj)
     (mapconcat #'(lambda (x)
                    (string-utils-stringify-anything x separator ints-are-chars record-separator)) (append obj nil) separator))

    ;; abbrev-table
    ((ignore-errors (abbrev-table-p obj))
     (let ((output nil))
       (mapatoms #'(lambda (sym)
                     (when (> (length (symbol-name sym)) 0)
                       (if (stringp (symbol-value sym))
                           (push (string-utils-stringify-anything (symbol-value sym) separator ints-are-chars record-separator) output)
                         (push (string-utils-stringify-anything (symbol-function sym) separator ints-are-chars record-separator) output))
                       (push (string-utils-stringify-anything sym separator ints-are-chars record-separator) output))) obj)
       (mapconcat 'identity
                  (nbutlast
                   (loop for (k v) on output by 'cddr
                         collect k
                         collect separator
                         collect v
                         collect record-separator)
                   (if (equal record-separator separator) 1 0))
                  "")))

    ;; obarray
    ((and (fboundp 'obarrayp)
          (obarrayp obj))
     (let ((output nil))
       (mapatoms #'(lambda (sym)
                     (when (boundp sym)
                       (push (string-utils-stringify-anything (symbol-value sym) separator ints-are-chars record-separator) output)
                       (push (string-utils-stringify-anything sym separator ints-are-chars record-separator) output))) obj)
       (mapconcat 'identity
                  (nbutlast
                   (loop for (k v) on output by 'cddr
                         collect k
                         collect separator
                         collect v
                         collect record-separator)
                   (if (equal record-separator separator) 1 0))
                  "")))

    ;; ordinary vector
    ((vectorp obj)
     (mapconcat #'(lambda (x)
                    (string-utils-stringify-anything x separator ints-are-chars record-separator)) obj separator))

    ;; fallback
    (t
     (format "%s" obj))))

;;;###autoload
(defun string-utils-has-darkspace-p (obj &optional whitespace-type)
  "Test whether OBJ, when coerced to a string, has any non-whitespace characters.

Returns the position of the first non-whitespace character
on success.

If optional WHITESPACE-TYPE is 'ascii or t, use an ASCII-only
definition of whitespace characters.  If WHITESPACE-TYPE is
'syntax, is the definition of whitespace from the current
`syntax-table'.  Otherwise, use a broad, Unicode-aware
definition of whitespace from `string-utils-whitespace'."
  (assert (memq whitespace-type '(ascii ascii-only t syntax unicode nil)) nil "Bad WHITESPACE-TYPE")
  (let* ((str-val (if (stringp obj) obj (string-utils-stringify-anything obj "")))
         (string-utils-whitespace (if (memq whitespace-type '(ascii ascii-only t))
                                      string-utils-whitespace-ascii
                                    string-utils-whitespace))
         (darkspace-regexp (if (eq whitespace-type 'syntax)
                               (upcase string-utils-whitespace-syntax)
                             (concat "[^" string-utils-whitespace "]"))))
    (string-match-p darkspace-regexp str-val)))

;;;###autoload
(defun string-utils-has-whitespace-p (obj &optional whitespace-type)
  "Test whether OBJ, when coerced to a string, has any whitespace characters.

Returns the position of the first whitespace character on
success.

If optional WHITESPACE-TYPE is 'ascii or t, use an ASCII-only
definition of whitespace characters.  If WHITESPACE-TYPE is
'syntax, is the definition of whitespace from the current
`syntax-table'.  Otherwise, use a broad, Unicode-aware
definition of whitespace from `string-utils-whitespace'."
  (assert (memq whitespace-type '(ascii ascii-only t syntax unicode nil)) nil "Bad WHITESPACE-TYPE")
  (let* ((str-val (if (stringp obj) obj (string-utils-stringify-anything obj "")))
         (string-utils-whitespace (if (memq whitespace-type '(ascii ascii-only t))
                                      string-utils-whitespace-ascii
                                    string-utils-whitespace))
         (whitespace-regexp (if (eq whitespace-type 'syntax)
                                string-utils-whitespace-syntax
                              (concat "[" string-utils-whitespace "]"))))
    (string-match-p whitespace-regexp str-val)))

;;;###autoload
(defun string-utils-trim-whitespace (str-val &optional whitespace-type multi-line)
  "Return STR-VAL with leading and trailing whitespace removed.

If optional WHITESPACE-TYPE is 'ascii or t, use an ASCII-only
definition of whitespace characters.  If WHITESPACE-TYPE is
'syntax, is the definition of whitespace from the current
`syntax-table'.  Otherwise, use a broad, Unicode-aware
definition of whitespace from `string-utils-whitespace'.

If optional MULTI-LINE is set, trim spaces at starts and
ends of all lines throughout STR-VAL."
  (assert (memq whitespace-type '(ascii ascii-only t syntax unicode nil)) nil "Bad WHITESPACE-TYPE")
  (let* ((string-utils-whitespace (if (memq whitespace-type '(ascii ascii-only t))
                                      string-utils-whitespace-ascii
                                    string-utils-whitespace))
         (whitespace-regexp (if (eq whitespace-type 'syntax)
                                string-utils-whitespace-syntax
                              (concat "[" string-utils-whitespace "]")))
         (start-pat (if multi-line "^" "\\`"))
         (end-pat   (if multi-line "$" "\\'")))
    (save-match-data
      (replace-regexp-in-string (concat start-pat whitespace-regexp "+") ""
         (replace-regexp-in-string (concat whitespace-regexp "+" end-pat) ""
            str-val)))))

;;;###autoload
(defun string-utils-compress-whitespace (str-val &optional whitespace-type)
  "Return STR-VAL with all contiguous whitespace compressed to one space.

If optional WHITESPACE-TYPE is 'ascii or t, use an ASCII-only
definition of whitespace characters.  If WHITESPACE-TYPE is
'syntax, is the definition of whitespace from the current
`syntax-table'.  Otherwise, use a broad, Unicode-aware
definition of whitespace from `string-utils-whitespace'."
  (assert (memq whitespace-type '(ascii ascii-only t syntax unicode nil)) nil "Bad WHITESPACE-TYPE")
  (let* ((string-utils-whitespace (if (memq whitespace-type '(ascii ascii-only t))
                                      string-utils-whitespace-ascii
                                    string-utils-whitespace))
         (whitespace-regexp (if (eq whitespace-type 'syntax)
                                string-utils-whitespace-syntax
                              (concat "[" string-utils-whitespace "]"))))
    (save-match-data
      (replace-regexp-in-string (concat whitespace-regexp "+") " "
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

;;;###autoload
(defun string-utils-propertize-fillin (str-val &rest properties)
  "Return a copy of STR-VAL with text properties added, without overriding.

Works exactly like `propertize', except that (character-by-character)
already existing properties are respected.

STR-VAL and PROPERTIES are treated as documented for the STRING
and PROPERTIES arguments to `propertize'."
  (unless (= 0 (% (length properties) 2))
    (error "Wrong number of arguments"))
  (while properties
    (let ((prop (pop properties))
          (val  (pop properties)))
      (font-lock-fillin-text-property 0 (length str-val) prop val str-val)))
   str-val)

;;;###autoload
(defun string-utils-plural-ending (num)
  "Return \"s\" or \"\", depending on whether NUM requires a plural in English.

Intended to be used in a format string as follows:

    (message \"%s item%s deleted\" del-counter (string-utils-plural-ending del-counter))"
  (if (and (numberp num)
           (= num 1))
      "" "s"))

;;;###autoload
(defun string-utils-squeeze-filename (name maxlen &optional path-removal ellipsis no-tail)
  "Intelligibly squeeze file-name or buffer-name NAME to fit within MAXLEN.

When shortening file or buffer names for presentation to human
readers, it is often preferable not to truncate the ends, but to
remove leading or middle portions of the string.

This function keeps basename intact, and (failing that) the
beginning and end of the basename, so that a shortened file or
buffer name is more identifiable to a human reader.

The heuristic

   1.  Works equally for file names or buffer names.

   2.  Applies abbreviations to file names such as \"~\" for home
       directory.

   3.  Selectively removes the longest leading directory
       components from a path, preferring to keep the rightmost
       components, leaving a single ellipsis where any number of
       path elements were removed.

   4.  Shortens the basename of NAME if needed, preserving the
       meaningful file extension.

The string returned is as long as MAXLEN or shorter.

When PATH-REMOVAL is non nil, it is permitted to shorten a
pathname by removing the directory components completely,
substituting no ellipsis.

ELLIPSIS is a string inserted wherever characters were removed.
It defaults to the UCS character \"Horizontal Ellipsis\", or
\"...\" if extended characters are not displayable.

If NO-TAIL is set, do not preserve the trailing letters of
a filename unless there is a dotted extension."
  ;; character x2026 = Horizontal Ellipsis
  (callf or ellipsis (if (char-displayable-p (decode-char 'ucs #x2026)) (string (decode-char 'ucs #x2026)) "..."))
  (cond
    ;; corner cases for tiny MAXLEN
    ((< maxlen 0)
     (error "Length must be greater than or equal to 0"))
    ((= maxlen 0)
     "")
    ((and (<= maxlen (length ellipsis))
          (> (length ellipsis) 0))
     (substring ellipsis 0 maxlen))
    (t
     ;; most cases
     (save-match-data
       (let ((dir-sep "/")
             (path nil)
             (used-last-elt 'first)
             (orig-name nil)
             (added-path ""))
         (when (bufferp name)
           (setq name (buffer-name name)))
         (setq path (nreverse (split-string (directory-file-name (abbreviate-file-name name)) dir-sep)))
         (setq name (pop path))
         (setq orig-name name)

         ;; prepend path components, so long as within MAXLEN
         (while path
           (if (and (<= (+ (length (car path))
                           (length name)
                           (length dir-sep)
                           (if (> (length path) 1) (+ (length dir-sep) (length ellipsis)) 0))
                        maxlen)
                    ;; leading "/" followed by ellipsis is not meaningful
                    (not (and (not used-last-elt)
                              (= (length (car path)) 0))))
               (progn
                 (setq added-path (concat (car path) dir-sep added-path))
                 (setq name (concat (car path) dir-sep name))
                 (setq used-last-elt t))
             ;; else
             (when used-last-elt
               (setq name (concat ellipsis dir-sep name))
               (setq added-path (concat ellipsis dir-sep added-path)))
             (setq used-last-elt nil))
           (pop path))

         ;; PATH-REMOVAL logic
         (when (and (> (length name) maxlen)
                    path-removal)
           (setq added-path "")
           (setq name orig-name))

         ;; squeeze basename
         (when (> (length name) maxlen)

           ;; find extension or tail
           (let ((extension ""))
             (when (string-match "\\(\\.[^.]\\{1,6\\}\\)\\'" name)
               (setq extension (match-string 1 name))
               (setq name (replace-match "" t t name 0)))
             (when (and (equal extension "")
                        (not no-tail)
                        (string-match ".\\(.\\{4\\}\\)\\'" name))
               (setq extension (match-string 1 name))
               (setq name (replace-match "" t t name 1)))

             ;; these conditionals are just corner cases for small MAXLEN
             (when (>= (+ (length extension) (length ellipsis)) maxlen)
               (setq extension ""))
             (when (and (not (string-match-p "\\`\\." extension))
                        (>= (+ (* 2 (length extension)) (length ellipsis)) maxlen))
               (setq extension ""))
             (when (<= (- maxlen (length ellipsis) (length extension))
                       (length added-path))
               (setq extension ""))
             (when (and (>= (+ (length extension) (length ellipsis)) maxlen)
                        (> (length ellipsis) 1))
               (callf substring ellipsis 0 (1- (length ellipsis))))
             (when (and (<= (- maxlen (length ellipsis) (length extension))
                            (length added-path))
                        (> (length ellipsis) 1))
               (callf substring ellipsis 0 (1- (length ellipsis))))

             ;; truncate and add back ellipsis and extension
             (callf substring name 0 (- maxlen (length ellipsis) (length extension)))
             (callf concat name ellipsis extension)))))

     ;; hardcode one last corner case
     (when (equal name ".../.")
       (setq name "....."))

     ;; defensive driving - name should already be <= than MAXLEN
     (substring name 0 (min maxlen (length name))))))

;;;###autoload
(defun string-utils-squeeze-url (url maxlen &optional ellipsis)
  "Intelligibly squeeze string URL to fit within MAXLEN.

Fit URL within MAXLEN for presentation to a human reader.
Follows rules similar to `string-utils-squeeze-filename'.

ELLIPSIS is a string inserted wherever characters were removed.
It defaults to the UCS character \"Horizontal Ellipsis\", or
\"...\" if extended characters are not displayable."
  (callf or ellipsis (if (char-displayable-p (decode-char 'ucs #x2026)) (string (decode-char 'ucs #x2026)) "..."))
  (save-match-data
    (let* ((parsed (url-generic-parse-url url))
           (host (aref parsed 4))
           (scheme (aref parsed 1))
           (prefix "")
           (rest-of-string url))
      (cond
        ((> (length host) 0)
          (string-match (concat "\\`\\(.*?" (regexp-quote host) "[/?]*\\)") rest-of-string)
          (setq prefix (match-string 1 rest-of-string))
          (setq rest-of-string (replace-match "" t t rest-of-string 1)))
        ((> (length scheme) 0)
         (string-match (concat "\\`\\(" (regexp-quote scheme) "[/:]*\\)") rest-of-string)
         (setq prefix (match-string 1 rest-of-string))
         (setq rest-of-string (replace-match "" t t rest-of-string 1))))
      (cond
        ((>= (length ellipsis) maxlen)
         (substring ellipsis 0 maxlen))
        ((or (> (length prefix) maxlen)
             (and (= (length prefix) maxlen)
                  (> (length rest-of-string) 0)))
         ;; todo could drop leading "www" and attempt to preserve domain name
         (callf substring url 0 (- maxlen (length ellipsis)))
         (callf concat url ellipsis)
         url)
        (t
         (concat prefix
                 (string-utils-squeeze-filename rest-of-string (- maxlen (length prefix)) nil ellipsis)))))))

(defun string-utils--repair-split-list (list-val separator)
  "Repair list LIST-VAL, split at string SEPARATOR, if SEPARATOR was escaped.

The escape character is backslash \(\\\)."
  (let ((ret-val nil))
    (while list-val
      (let ((top (pop list-val)))
        (while (string-match-p "\\\\\\'" top)
          (callf concat top separator)
          (when list-val
            (callf concat top (pop list-val))))
        (push top ret-val)))
    (setq ret-val (nreverse ret-val))))

;; todo
;;
;;  - fully re-implement split-string, so that SEPARATORS may be a regexp when
;;    respect-escapes is set.
;;
;;  - implement 'include-separators, allowing separators to be returned
;;    in the list as with perl split
;;
;;  - remove string-utils--repair-split-list
;;
;;;###autoload
(defun string-utils-split (string &optional separators omit-nulls include-separators respect-escapes)
  "Like `split-string', with additional options.

STRING, SEPARATORS, and OMIT-NULLS are as documented at `split-string'.

INCLUDE-SEPARATORS is currently unimplemented.

When RESPECT-ESCAPES is set, STRING is not split where the
separator is escaped with backslash.  This currently has the
limitation that SEPARATORS must be an explicit string rather than
a regular expression."
  (cond
    (respect-escapes
     (assert separators nil "SEPARATORS must be a string")
     (string-utils--repair-split-list (split-string string separators omit-nulls) separators))
    (t
     (split-string string separators omit-nulls))))

;;;###autoload
(defun string-utils-truncate-to (str-val maxlen &optional ellipsis)
  "Truncate STRING to MAXLEN.

The returned value is of length MAXLEN or less, including
ELLIPSIS.

ELLIPSIS is a string inserted wherever characters were removed.
It defaults to the UCS character \"Horizontal Ellipsis\", or
\"...\" if extended characters are not displayable."
  ;; character x2026 = Horizontal Ellipsis
  (callf or ellipsis (if (char-displayable-p (decode-char 'ucs #x2026)) (string (decode-char 'ucs #x2026)) "..."))
  (when (> (length str-val) maxlen)
    (if (>= (length ellipsis) maxlen)
        (setq str-val ellipsis)
      (callf substring str-val 0 (- maxlen (length ellipsis)))
      (callf concat str-val ellipsis))
    (callf substring str-val 0 maxlen))
  str-val)

(provide 'string-utils)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: StringUtils ARGS alist utils darkspace quotemeta bool
;; LocalWords: propertize fillin callf MULTI MAXLEN mapconcat progn
;; LocalWords: defstruct stringified Stringification INTS ascii subr
;; LocalWords: devel eieio EIEIO subseq obarray
;;

;;; string-utils.el ends here

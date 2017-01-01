;;; subr+.el --- Extensions to standard library `subr.el'.
;;
;; Filename: subr+.el
;; Description: Extensions to standard library `subr.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2014-2017, Drew Adams, all rights reserved.
;; Created: Sat May 24 19:24:18 2014 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Jan  1 11:40:44 2017 (-0800)
;;           By: dradams
;;     Update #: 162
;; URL: http://www.emacswiki.org/simple%2b.el
;; Doc URL: http://www.emacswiki.org/SplittingStrings
;; Keywords: strings, text
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to standard library `subr.el'.
;;
;;  This library extends `split-string' so that you can split a string
;;  based on text properties or a character predicate, not just regexp
;;  matching.
;;
;;  To take advantage of this, your code can conditionally test
;;  whether this library is loaded, or just test whether (fboundp
;;  'subr+-split-string).  That function is an alias for `split-string'.
;;
;;  Buffer substring functions are also defined here, which return a
;;  buffer substring that includes or excludes characters that have a
;;  given text property.  In particular, `buffer-substring-of-visible'
;;  include only visible chars, and `buffer-substring-of-invisible'
;;  includes only invisible chars.
;;
;;
;;  Functions defined here:
;;
;;    `buffer-substring-of-faced', `buffer-substring-of-invisible',
;;    `buffer-substring-of-propertied', `buffer-substring-of-unfaced',
;;    `buffer-substring-of-unpropertied',
;;    `buffer-substring-of-un/propertied-1',
;;    `buffer-substring-of-visible', `next-char-predicate-change',
;;    `split-string-by-predicate', `split-string-by-property',
;;    `split-string-by-regexp', `split-string-trim-omit-push',
;;    `subr+-split-string'.
;;
;;
;;  ***** NOTE: The following functions defined in `simple.el' have
;;              been REDEFINED HERE:
;;
;;    `split-string' - Can also split by char property or predicate.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014/05/31 dadams
;;     Added: buffer-substring-of-*.
;;     split-string-by-property: Corrected second cond clause for FLIP case.
;; 2014/05/28 dadams
;;     Removed: next-single-char-prop-val-change.
;;     split-string-by-property: Rewrote.
;;     split-string: Added optional arg TEST, for property splitting.
;; 2014/05/27 dadams
;;     Added subr+-split-string as alias for new split-string.
;; 2014/05/26 dadams
;;     Added: next-single-char-prop-val-change.
;;     split-string-by-property: Handle change in VAL, not just PROP.
;; 2014/05/24 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(load-library "subr")                   ; Emacs 20 does not `provide' it.

;;;;;;;;;;;;;;;;;;;

(unless (boundp 'split-string-default-separators) ; Emacs 20
  (setq split-string-default-separators  "[ \f\t\n\r\v]+"))


;; REPLACES ORIGINAL in `simple.el':
;;
;; 1. Second arg HOW can be a character property list or a predicate, in addition to a regexp.
;; 2. Additional optional arg FLIP: to complement set of returned substrings.
;; 3. Additional optional arg TEST: passed to `split-string-by-property' if HOW is
;;    (PROPERTY VALE).
;;
(defun split-string (string &optional how omit-nulls trim flip test)
  "Split STRING into substrings.
Arg HOW determines how splitting is done.  it is one of the following:
* a regexp (a string) - see function `split-string-by-regexp'
* a list whose first element is a text property (a symbol) and whose
  second element is the property value - see function
  `split-string-by-property'
* a predicate that accepts a character as its first argument - see
  function `split-string-by-predicate'

If optional arg OMIT-NULLS is t, then empty substrings are omitted
from the returned list.  If nil, zero-length substrings are retained,
which correctly parses CSV format, for example.

If TRIM is non-nil, it should be a regular expression to match text to
trim from the beginning and end of each substring.  If trimming makes
a substring empty, it is treated according to OMIT-NULLS.

Optional arg TEST is used only if HOW is a (PROPERTY VALUE) list, in
which case it is passed to function `split-string-by-property' (which
see).  Otherwise, it is ignored.

Modifies the match data; use `save-match-data' if necessary."
  (unless how (setq how  split-string-default-separators))
  (cond ((stringp how)
         (split-string-by-regexp string how omit-nulls trim flip))
        ((functionp how)
         (split-string-by-predicate string how omit-nulls trim flip))
        ((and (consp how)  (car how)  (symbolp (car how)))
         (split-string-by-property string how omit-nulls trim flip test))
        (t (error "`split-string', bad HOW arg: `%S'" how))))

;; Do this so code can test (fboundp 'subr+-split-string) to see if this version is
;; available, e.g., to use property or predicate splitting.
(defalias 'subr+-split-string 'split-string)

(defun split-string-trim-omit-push (string how omit-nulls trim start end parts)
  "Push the substring of STRING from START to END to list PARTS.
Return updated list PARTS.

Do not add substring if it is empty (\"\") and OMIT-NULLS is non-nil.
Before adding it, trim its ends if they match regexp TRIM.

Argumensts HOW and PARTS are as for function `split-string'.

Modifies the match data; use `save-match-data' if necessary."
  (when trim                            ; Trim beginning of substring.
    (let ((trim-beg  (string-match trim string start)))
      (when (eq trim-beg start) (setq start  (match-end 0)))))
  (let ((keep-nulls  (not (if how omit-nulls t))))
    (when (or keep-nulls  (< start end)) ; Do not add empty substring.
      (let ((this  (substring string start end)))
        (when trim                      ; Trim end of substring.
          (let ((trim-beg  (string-match (concat trim "\\'") this 0)))
            (when (and trim-beg  (< trim-beg (length this)))
              (setq this  (substring this 0 trim-beg)))))
        (when (or keep-nulls  (> (length this) 0)) (push this parts)))))
  parts)

(defun split-string-by-regexp (string separators &optional omit-nulls trim flip)
  "Split STRING into substrings bounded by matches for SEPARATORS.
Return the list of substrings.  The beginning and end of STRING, and
each match for SEPARATORS, are splitting points.

By default, the substrings matching SEPARATORS are removed, and the
substrings between the splitting points are collected as a list, which
is returned.  With non-nil optional argument FLIP this is reversed:
the list of matches to SEPARATORS is returned.

If SEPARATORS is non-nil, it should be a regular expression matching
text which separates, but is not part of, the substrings.  If nil it
defaults to `split-string-default-separators', normally
\"[ \\f\\t\\n\\r\\v]+\", and OMIT-NULLS is forced to t.

See function `split-string' for information about args OMIT-NULLS and
TRIM.

Note that for the default value of SEPARATORS, non-nil OMIT-NULLS
trims leading and trailing whitespace.  However, if you want to trim
whitespace from the substrings, the reliably correct way is using
TRIM.  Making SEPARATORS match that whitespace gives incorrect results
when there is whitespace at the start or end of STRING.  If you see
such calls to `split-string', please fix them.

Note that the effect of `(split-string STRING)' is the same as that of
`(split-string STRING split-string-default-separators t)'.  In the
rare case that you wish to retain empty substrings when splitting on
whitespace, use `(split-string STRING split-string-default-separators)'.

Modifies the match data; use `save-match-data' if necessary."
  (let* ((regexp    (or separators  split-string-default-separators))
         (s-len     (length string))
         (start     0)
         (notfirst  nil)
         (this-beg  nil)
         (this-end  nil)
         (s-parts   ()))
    (while (and (< start s-len)
                (string-match regexp string
                              (if (and notfirst
                                       (= start (match-beginning 0))
                                       (< start s-len))
                                  (1+ start)
                                start)))
      (setq notfirst  t
            this-beg  start
            this-end  (match-beginning 0)
            start     (match-end 0))
      (setq s-parts  (split-string-trim-omit-push string separators omit-nulls trim
                                                  (if flip this-end this-beg)
                                                  (if flip start this-end)
                                                  s-parts)))
    (setq this-beg  start ; Handle the substring at the end of STRING.
          this-end  s-len
          s-parts   (split-string-trim-omit-push string separators omit-nulls trim
                                                 this-beg this-end s-parts))
    (nreverse s-parts)))

(defun split-string-by-property (string prop+val &optional omit-nulls trim flip test)
  "Split STRING into substrings determined by a text property.
Return the list of substrings.

By default, the substrings that have the given property with the given
value are removed, and the substrings between these are collected as a
list, which is returned.

With non-nil optional argument FLIP this behavior is reversed: The
list of substrings that have the given property and value is returned,
and the substrings that do not are removed.

PROP+VAL is a list (PROPERTY VALUE), where PROPERTY is the text
property (a symbol) and VALUE is the property value to match.

Arguments OMIT-NULLS and TRIM are as for function
`split-string-by-regexp'.

Optional arg TEST is a binary predicate that accepts the actual value
of PROPERTY for a given string position as its first arg and VALUE as
its second.  It returns non-nil if the char at that position is part
of an excluded substring (or an included one, if FLIP is non-nil).

If TEST is omitted or nil (the default) then:

* If VALUE is not `nil' then TEST is `eq'.  That is, if you provide
  VALUE and no TEST then the actual value must match exactly.

* If VALUE is `nil' then TEST checks only for a non-null value, that
  is, for the presence of PROPERTY.

By providing a TEST argument you can get fairly flexible behavior.
For example:

* You might want to test for an actual property value that belongs to
  a given list of values.  E.g., test whether the actual value of
  PROPERTY `invisible' belongs to the current
  `buffer-invisibility-spec'.

* You might want to test for an actual value that is a list that has
  VALUE as a member.  E.g., test membership of a particular face
  (VALUE) in a list of faces that is the value of PROPERTY `face').

Modifies the match data; use `save-match-data' if necessary.

\(This function requires Emacs 22 or later.)"
  (unless (fboundp 'next-single-char-property-change)
    (error "`split-string-by-property' requires Emacs 22 or later"))
  (let* ((prop      (car prop+val))
         (val       (cadr prop+val))
         (s-len     (length string))
         (start     0)
         (ostart    0)
         (this-beg  nil)
         (this-end  nil)
         (s-parts   ())
         has-val-b  has-val-e)
    (unless test
      (setq test  (if val #'eq (lambda (actual-prop-val _ignore) actual-prop-val))))
    (while (and (< start s-len)
                (setq this-end  (next-single-char-property-change start prop string)))
      (setq this-beg   start)
      (setq start      this-end)
      (setq has-val-b  (funcall test (get-char-property this-beg prop string) val))
      (setq has-val-e  (funcall test (get-char-property this-end prop string) val))

      (cond ((and has-val-b  (not has-val-e))
             (when flip
               (setq s-parts  (split-string-trim-omit-push string prop+val omit-nulls trim
                                                           this-beg this-end s-parts)))
             (setq ostart  start))
            ((and has-val-e  (if flip has-val-b (not has-val-b)))
             (let ((has-val-o  (funcall test (get-char-property ostart prop string) val)))
               (when (if flip (not has-val-o) has-val-o)
                 (setq ostart  this-beg)))
             (setq s-parts  (split-string-trim-omit-push string prop+val omit-nulls trim
                                                         ostart this-end s-parts))
             (setq ostart  start))))
    (setq this-beg  start) ; Handle the substring at the end of STRING.
    (setq this-end  s-len)
    (unless (or flip  has-val-b)
      (setq s-parts  (split-string-trim-omit-push string prop+val omit-nulls trim
                                                  ostart this-end s-parts)))
    (nreverse s-parts)))

(defun split-string-by-predicate (string predicate &optional omit-nulls trim flip)
  "Split STRING into substrings determined by a character predicate.
Return the list of substrings.

By default, the substrings for which PREDICATE is true are removed,
and the substrings between these are collected as a list, which is
returned.  With non-nil optional argument FLIP this is reversed: the
list of substrings for which the PREDICATE is true is returned.

PREDICATE is a Boolean function that accepts a character as its first
argument.  Arguments OMIT-NULLS and TRIM are as for function
`split-string-by-regexp'.

Modifies the match data; use `save-match-data' if necessary."
  (let ((s-len     (length string))
        (start     0)
        (this-beg  nil)
        (this-end  nil)
        (is-true   nil)
        (s-parts   ()))
    (while (and (< start s-len)
                (setq this-end  (next-char-predicate-change start predicate string)))
      (setq this-beg  start
            start     (next-char-predicate-change start predicate string)
            is-true   (funcall predicate (aref string this-beg)))
      (when (if flip is-true (not is-true))
        (setq s-parts  (split-string-trim-omit-push string predicate omit-nulls trim
                                                    this-beg this-end s-parts))))
    (setq this-beg  start               ; Handle the substring at the end of STRING.
          this-end  s-len
          is-true   (funcall predicate this-beg))
    (when (if flip is-true (not is-true))
      (setq s-parts  (split-string-trim-omit-push string predicate omit-nulls trim
                                                  this-beg this-end s-parts)))
    (nreverse s-parts)))

(defun next-char-predicate-change (position predicate string)
  "Return next position in STRING after POSITION where PREDICATE is true.
PREDICATE is a function accepting a character as its first argument."
  (let ((s-len   (length string))
        (otruth  (and (funcall predicate (aref string position))  t)))
    (while (and (< (setq position  (1+ position)) s-len)
                (eq otruth (and (funcall predicate (aref string position))  t))))
    position))


;; Buffer substring functions

(defun buffer-substring-of-un/propertied-1 (start end property &optional flip)
  "Helper for `buffer-substring-of-(un)propertied'.
Optional arg FLIP is passed to `split-string-by-property', so non-nil
FLIP gives `*-propertied', and nil gives `*-unpropertied'."
  (unless (require 'subr+ nil t)        ; `split-string-by-property'
    (error "This function requires library `subr+.el'"))
  (when (> start end) (setq start (prog1 end (setq end start))))
  (let* ((filter-buffer-substring-function   (lambda (beg end _delete)
                                               (let* ((strg   (buffer-substring beg end))
                                                      (parts  (split-string-by-property
                                                               strg `(,property nil) 'OMIT-NULLS
                                                               split-string-default-separators
                                                               flip))
                                                      (strg   (apply #'concat parts)))
                                                 (set-text-properties 0 (length strg) () strg)
                                                 strg)))
         ;; Older Emacs versions use `filter-buffer-substring-functions'.
         (filter-buffer-substring-functions  (list (lambda (fun beg end del)
                                                     (funcall filter-buffer-substring-function
                                                              beg end del)))))
    (filter-buffer-substring start end)))

(defun buffer-substring-of-unpropertied (start end property)
  "Return unpropertied contents of buffer from START to END, as a string.
Text from START to END that has PROPERTY is excluded from the string.
START and END can be in either order."
  (buffer-substring-of-un/propertied-1 start end property))

(defun buffer-substring-of-propertied (start end property)
  "Return PROPERTY'ed contents of buffer from START to END, as a string.
Only text from START to END that has PROPERTY is included.
START and END can be in either order."
  (buffer-substring-of-un/propertied-1 start end property 'FLIP))
  
(defun buffer-substring-of-visible (start end)
  "Return contents of visible part of buffer from START to END, as a string.
START and END can be in either order."
  (buffer-substring-of-unpropertied start end 'invisible))

(defun buffer-substring-of-invisible (start end)
  "Return contents of invisible part of buffer from START to END, as a string.
START and END can be in either order."
  (buffer-substring-of-propertied start end 'invisible))

(defun buffer-substring-of-unfaced (start end)
  "Return unfaced contents of buffer from START to END, as a string.
That is, include only text that has no `face' property.
START and END can be in either order."
  (buffer-substring-of-unpropertied start end 'face))

(defun buffer-substring-of-faced (start end)
  "Return faced contents of buffer from START to END, as a string.
That is, include only text that has property `face'.
START and END can be in either order."
  (buffer-substring-of-propertied start end 'face))

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'subr+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; subr+.el ends here

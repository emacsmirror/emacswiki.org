;;; subr+.el --- Extensions to standard library `subr.el'.
;;
;; Filename: subr+.el
;; Description: Extensions to standard library `subr.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2014, Drew Adams, all rights reserved.
;; Created: Sat May 24 19:24:18 2014 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Mon May 26 14:01:55 2014 (-0700)
;;           By: dradams
;;     Update #: 61
;; URL: http://www.emacswiki.org/simple%2b.el
;; Doc URL: http://www.emacswiki.org/SplittingStrings
;; Keywords: strings, text
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x
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
;;  Functions defined here:
;;
;;    `next-char-predicate-change',
;;    `next-single-char-prop-val-change', `split-string-by-predicate',
;;    `split-string-by-property', `split-string-by-regexp',
;;    `split-string-trim-omit-push',
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
;; 1. Second arg can be a character property list or a predicate, in
;;    addition to a regexp.
;; 2. Addtional optional arg FLIP: to complement set of returned substrings.
;;
(defun split-string (string &optional how omit-nulls trim flip)
  "Split STRING into substrings.
Arg HOW determines how splitting is done.  it is one of the following:
* a regexp (a string) - see function `split-string-by-regexp'
* a list whose first element is a text property (a symbol) and whose
  second element is the property value - see function
  `split-string-by-property'
* a predicate that accepts a character as its first argument - see
  function `split-string-by-predicate'

If optional arg OMIT-NULLS is t, then empty substrings are omitted
from the returned list.  If nil, all zero-length substrings are
retained, which correctly parses CSV format, for example.

If TRIM is non-nil, it should be a regular expression to match text to
trim from the beginning and end of each substring.  If trimming makes
a substring empty, it is treated according to OMIT-NULLS.

Modifies the match data; use `save-match-data' if necessary."
  (unless how (setq how  split-string-default-separators))
  (cond ((stringp how)
         (split-string-by-regexp string how omit-nulls trim flip))
        ((functionp how)
         (split-string-by-predicate string how omit-nulls trim flip))
        ((and (consp how)  (car how)  (symbolp (car how)))
         (split-string-by-property string how omit-nulls trim flip))
        (t (error "`split-string', bad HOW arg: `%S'" how))))

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

(defun split-string-by-property (string prop+val &optional omit-nulls trim flip)
  "Split STRING into substrings determined by a text property.
Return the list of substrings.

By default, the substrings that have the given property with the given
value are removed, and the substrings between these are collected as a
list, which is returned.  With non-nil optional argument FLIP this is
reversed: The list of substrings that have the property and value is
returned, and the substrings that do not are removed.

PROP+VAL is a property list whose first element is the property (a
symbol) and whose second is the property value.  Arguments OMIT-NULLS
and TRIM are as for function `split-string-by-regexp'.

Modifies the match data; use `save-match-data' if necessary.

\(This function requires Emacs 22 or later.)"
  (unless (fboundp 'next-single-char-property-change)
    (error "`split-string-by-property' requires Emacs 22 or later"))
  (let* ((prop      (car prop+val))
         (val       (cadr prop+val))
         (s-len     (length string))
         (start     0)
         (this-beg  nil)
         (this-end  nil)
         (has-prop  (eq (get-char-property start prop string) val))
         (s-parts   ()))
    (while (and (< start s-len)
                (setq this-end  (next-single-char-prop-val-change
                                 start prop val (if flip has-prop (not has-prop)) string)))
      (setq this-beg  start)
      (setq start     (next-single-char-prop-val-change
                       start prop val (if flip (not has-prop) has-prop) string))
      (setq has-prop  (eq (get-char-property this-beg prop string) val))
      (when (if flip has-prop (not has-prop))
        (setq s-parts  (split-string-trim-omit-push string prop+val omit-nulls trim
                                                    this-beg this-end s-parts))))
    (setq this-beg  start) ; Handle the substring at the end of STRING.
    (setq this-end  s-len)
    (setq has-prop  (eq (get-char-property this-beg prop string) val))
    (when (if flip has-prop (not has-prop))
      (setq s-parts  (split-string-trim-omit-push string prop+val omit-nulls trim
                                                  this-beg this-end s-parts)))
    (nreverse s-parts)))

(defun next-single-char-prop-val-change (position property value notp &optional object limit)
  "Return next position after POSITION where PROPERTY VALUE changes.
By default, return the next position where text or overlay PROPERTY
has VALUE.  Non-nil optional arg NOTP means return the next position
where PROPERTY does not have VALUE.

Scans chars forward from POSITION until it finds a position where
PROPERTY having VALUE becomes true (false if NOTP).  Returns the
position of this change.  Property values are compared using `eq'.

Optional argument OBJECT is a string or buffer.  A nil value means the
current buffer.  POSITION is a buffer position (integer or marker) or
a 0-based index into the string.

By default, the end of the buffer or string limits scanning.  Optional
arg LIMIT non-nil and less than the end position means stop scanning
at LIMIT.  If PROPERTY does not change from POSITION to the effective
limit then return that limit."
  (let* ((lim  (if object (length object) (point-max)))
         (lim  (if limit (min limit lim) lim))
         (pos  (next-single-char-property-change position property object lim))
         (val  (get-char-property pos property object))
         samep)
    (while (and (< pos lim)
                (progn (setq samep  (eq value (get-char-property pos property object)))
                       (if notp samep (not samep))))
      (setq pos  (next-single-char-property-change pos property object)))
    (setq samep (eq value (get-char-property pos property object)))
    (if (if notp (not samep) samep) pos lim)))

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
    (setq this-beg  start ; Handle the substring at the end of STRING.
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

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'subr+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; subr+.el ends here

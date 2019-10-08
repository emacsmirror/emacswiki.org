;;; find-where.el --- Find where something is true.  -*- lexical-binding:t -*-
;;
;; Filename: find-where.el
;; Description: Find where something is true.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2018, Drew Adams, all rights reserved.
;; Created: Sat Mar 17 10:13:09 2018 (-0700)
;; Version: 2018-10-08
;; Package-Requires: (thingatpt+ "0")
;; Last-Updated: Tue Oct  8 13:02:17 2019 (-0700)
;;           By: dradams
;;     Update #: 854
;; URL: https://www.emacswiki.org/emacs/download/find-where.el
;; Doc URL: https://www.emacswiki.org/emacs/FindWhere
;; Keywords: motion thing search
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Find where something is true.
;;
;;  Get or go to the next place where some predicate is satisfied.
;;
;;  But first, you don't really need this library! ;-)
;;
;;  In Emacs and Emacs Lisp there are multiple ways to find things.
;;  And in many cases it will be simpler or more efficient to use
;;  another way than to take advantage of this library.
;;
;;  What this library offers is some convenience sometimes, and a
;;  certain kind of generality: Specify what you want to find by a
;;  predicate.  The predicate is tested at successive places, forward
;;  or backward, until it is satisfied.
;;
;;  By default, the forward and backward movement is among buffer
;;  positions, in the usual sense.  And by default, each movement
;;  before testing is just one character.  This is the minimum
;;  movement needed to get past the current position (which is often
;;  the last place found).
;;
;;  Moving only one character and testing is obviously not very
;;  efficient, but it is all that can be done in the general case.
;;
;;  When you know a way to move farther before testing and to be sure
;;  there is no need to test closer, you can take advantage of that by
;;  providing for the appropriate movement in the predicate you
;;  provide or in optional forward and backward movement functions.
;;
;;  Clearly, this move-one-char-and-test approach is not the way to go
;;  for ordinary string searching.  Emacs uses an efficient,
;;  Boyer-Moore string-search algorithm (see
;;  https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string-search_algorithm),
;;  which essentially moves forward in chunks that are as long as what
;;  your search string matches, rather than moving just a character at
;;  a time before each match attempt.
;;
;;  So if you want to search for a sequence of characters, just use
;;  `(re-)search-forward' or similar.  And if you need an additional
;;  test at a match position (e.g., check a text or overlay property)
;;  you can easily add that.  So forget about this library for
;;  ordinary buffer search.
;;
;;  Still, you might find this library convenient for some things,
;;  even in cases where there is an easy alternative.  The abstraction
;;  of defining a destination by a predicate that holds there can be
;;  helpful.
;;
;;  So just what does it mean to "find where something is true"?  Find
;;  what?  Well, the what is defined by your predicate.  What is
;;  found, that is, returned, is both the position (where) and
;;  whatever the predicate returns as a non-nil value.  For example,
;;  you can find a text THING, such as the next vector with 13
;;  elements, and have it returned along with its bounds (start and
;;  end positions).
;;  
;;  The library defines some general functions to find and return the
;;  next or previous where-plus-what, which you provide with a
;;  defining predicate.  And it defines corresponding commands to go
;;  to the next or previous such location.
;;
;;  The commands have names that start (after prefix `fw-') with
;;  `to-'.  The corresponding non-interactive functions generally have
;;  the same names without the `to-'.  For example, function
;;  `fw-next-where' returns the next position and data that satisfy a
;;  predicate, and command `fw-to-next-where' goes there.
;;
;;  By default, movement is to the start position of something, but
;;  really the where, in relation to its what, is up to you.  The
;;  default behavior is thus different from the standard Emacs
;;  `forward-THING' and `backward-THING' behavior, which moves just
;;  past the THING rather than just to it.
;;
;;  This default behavior applies to all functions provided here, not
;;  just to those (`fw-next-thing' etc.) that find buffer THINGs.  For
;;  THINGs, `find-where.el' requires library `thingatpt+.el', which
;;  uses and enhances standard library `thingatpt.el'.
;;
;;  When repeated, the commands reuse the same predicate as the last
;;  time (it is the value of variable `fw-last-pred'), but a plain
;;  prefix argument (`C-u') makes them prompt you for the predicate to
;;  use.  The predicate you enter must accept a buffer position (the
;;  position to test) as its first argument.
;;
;;  A typical use might check something about the character at (i.e.,
;;  after) that position.
;;
;;  The THING-related function `fw-next-thing' just uses
;;  `fw-next-where' with a predicate that tests whether the position
;;  is at the start of a THING, where being at the start also means
;;  that the previous buffer position is not on the same thing (not
;;  just the same type of thing).
;;
;;  For instance, if the THING type passed to `fw-next-thing' is
;;  `list' then it returns the start position of the next list (as
;;  well as the list text as a string and its end position).
;;
;;  When repeated, the THING commands reuse the same THING type as the
;;  last time (it is the value of variable `fw-last-thing'), but a
;;  plain prefix argument (`C-u') makes them prompt you for the THING
;;  type to use.
;;
;;  You can bind any of the commands defined here to keys, of course.
;;  But you can also easily define other commands that make use of
;;  them, and bind those commands to keys.
;;
;;  For example, here's a simple command that moves to the start of
;;  the next use of face `font-lock-doc-face':
;;
;;  (defun to-next-doc-face (n)
;;    "Move to next occurrence of `font-lock-doc-face'.
;;  With numeric prefix arg N, move to the Nth next occurrence."
;;    (interactive "p")
;;    (fw-to-next-where #'doc-face-start-p nil nil n))
;;
;;  where the predicate is defined like so:
;;
;;  (defun doc-face-start-p (position)
;;    "Return non-nil if char at POSITION starts `font-lock-doc-face'.
;;  That is, it has that face, and any char just before it does not."
;;    (and (eq (get-text-property position 'face) 'font-lock-doc-face)
;;         (or (= position (point-min))
;;             (not (eq (get-text-property (1- position) 'face)
;;                      'font-lock-doc-face)))))
;;
;;  Note the use here of two complementary tests within the predicate.
;;  The character at the tested position must pass the test (having
;;  property `font-lock-doc-face'), and the preceding char, if there
;;  is one, must NOT pass the test.  This means that
;;  `to-next-doc-face' finds the _first_ character that passes the
;;  test.  This is typical of a predicate used with `find-where.el'
;;  functions.
;;
;;  For this reason, you can use helper function `fw-test-start-p' to
;;  take care of that true-here-but-not-just-before-here logic.  It
;;  takes the position to test and a predicate as arguments.  The
;;  predicate must be true at the position and false just before the
;;  position, for `fw-test-start-p' to be true (return non-nil).
;;
;;  Using `fw-test-start-p', `doc-face-start-p' becomes just this:
;;
;;  (defun doc-face-start-p (position)
;;    "Return non-nil if char at POSITION starts `font-lock-doc-face'.
;;  That is, it has that face, and any char just before it does not."
;;    (fw-test-start-p position
;;                     (lambda ()
;;                       (eq (get-text-property (point) 'face)
;;                           'font-lock-doc-face))))
;;
;;  The predicate arg to `fw-to-next-where' can accept additional
;;  arguments, besides the position.  So you can use a predicate that
;;  accepts, as argument, the face to look for, as well as the
;;  position to test.  For example:
;;
;;  (defun face-start-p (position face)
;;    "Return non-nil if the character at POSITION starts FACE.
;;  That is, it has FACE, and any character just before it does not."
;;    (and (eq (get-text-property position 'face) face)
;;         (or (= position (point-min))
;;             (not (eq (get-text-property (1- position) 'face)
;;                      face)))))
;;
;;  Or simplified using `fw-test-start-p':
;;
;;  (defun face-start-p (position face)
;;    "Return non-nil if the character at POSITION starts FACE.
;;  That is, it has FACE, and any character just before it does not."
;;    (fw-test-start-p position
;;                     `(lambda ()
;;                        (eq (get-text-property (point) 'face)
;;                            ',face))))
;;
;;  (defvar last-face nil "Last face used by `to-next-face'.")
;;
;;  (defun to-next-face (arg)
;;    "Move to next text-property occurrence of face `last-face'.
;;  With plain `C-u', prompt for the face to assign to `last-face'.
;;  With numeric prefix arg N, move to the Nth next occurrence."
;;    (interactive "P")
;;    (if (or (consp arg)  (not last-face))
;;        (setq last-face  (read-face-name "Face: ")
;;              arg        1)
;;      (setq arg  (prefix-numeric-value arg)))
;;    (fw-to-next-where #'face-start-p nil (list last-face) arg))
;;
;;
;;  [Note: Text property `face' can actually have a list of faces as
;;   its value, so instead of using an `eq' text in those `*-start-p'
;;   functions a more realistic example would test for the particular
;;   face using both `eq' and `memq' (return true if either is true).]
;;
;;  Now recall that the way `fw-to-next-where' works by default is to
;;  move forward only one char and then test with the predicate.  This
;;  is not very efficient, but it is all that can be done, unless
;;  there is some way to move farther forward before testing and be
;;  sure there is no need to test closer.
;;
;;  In the case of testing for a given text property (or overlay
;;  property) at a buffer position there is such a better way: use
;;  function `next-single-property-change'.  Using that, we can define
;;  the forward-movement function `to-next-face-prop', which we can
;;  pass to `to-next-face' to override the default one-char movement
;;  (`forward-char').
;;
;;  (defun to-next-face-prop ()
;;    "Go to next change in `face' text property."
;;    (let ((pos  (next-single-property-change (point) 'face)))
;;      (when pos (goto-char pos))))
;;
;;  (defun to-next-face (arg)
;;    (interactive "P")
;;    (if (or (consp arg)  (not last-face))
;;        (setq last-face  (read-face-name "Face: ")
;;              arg        1)
;;      (setq arg  (prefix-numeric-value arg)))
;;    (fw-to-next-where #'face-start-p nil (list last-face) arg
;;                      nil nil nil #'to-next-face-prop))
;;
;;
;;  As an example of defining a next-THING command, this defines a
;;  command to move forward among sexps:
;;
;;  (defun to-next-sexp (n)
;;    "Go to next start of a sexp."
;;    (interactive "p")
;;    (fw-to-next-thing 'sexp nil n))
;;
;;  And this moves among strings:
;;
;;  (defun to-next-string (n)
;;    "Go to next start of a string."
;;    (interactive "p")
;;    (fw-to-next-thing 'string nil n))
;;
;;  Note that the various `fw-to-next-*' and `fw-to-previous-*'
;;  commands move to the _beginning_ of the next or previous place
;;  where something is true.  For example, if you use `fw-next-thing'
;;  with THING `word' then the cursor moves to the beginning of each
;;  word.  This is different from typical Emacs `forward-*' and
;;  `backward-*' commands, which move _past_ the end or the beginning
;;  of something.
;;
;;  Typical Emacs `forward-*' commands essentially perform the
;;  following sequence of actions, expressed in terms of `fw-next-*':
;;
;;    1. While some predicate PRED-X is NOT true, do `fw-to-next-X'.
;;    2. Do `fw-to-next-NOT-X'.
;;
;;  Step 1 moves to the next place X is true (e.g., a word beginning).
;;  Step 2 moves just past where X continues to be true (e.g., just
;;  after the end of the word).
;;
;;  Partly as a way of illustrating this, commands `fw-downward-word'
;;  and `fw-upward-word' act like Emacs `forward-word' and
;;  `backward-word', but they move through text vertically, not
;;  horizontally.  They are defined using steps 1 and 2.
;;
;;
;;  Commands defined here:
;;
;;    `fw-downward-word', `fw-to-column-down', `fw-to-column-up',
;;    `fw-to-next-thing', `fw-to-next-where',
;;    `fw-to-next-where-vertical', `fw-to-previous-thing',
;;    `fw-to-previous-where', `fw-to-previous-where-vertical',
;;    `fw-upward-word'.
;;
;;  Non-interactive functions defined here:
;;
;;    `fw--next/prev-thing', `fw--next/prev-where',
;;    `fw--read-predicate', `fw--to-next/prev-thing',
;;    `fw--to-next/prev-where', `fw-next-thing', `fw-next-where',
;;    `fw-next-where-vertical', `fw-not-word-char-after-p',
;;    `fw-not-word-char-before-p', `fw-previous-thing',
;;    `fw-previous-where', `fw-previous-where-vertical',
;;    `fw-test-start-p', `fw-thing-start-p', `fw-word-char-after-p',
;;    `fw-word-char-before-p'.
;;
;;  Internal variables defined here:
;;
;;    `fw-last-pred', `fw-last-thing'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2018/07/09 dadams
;;     Renamed find-where.el from gowhere.el.
;; 2018/07/04 dadams
;;     Added: fw--read-predicate, fw-test-start-p.
;;     Renamed: fw-to-where-last to fw-last-pred and fw-to-where-last to fw-last-thing.
;;     Commands: Added progress messages.
;;     fw-thing-start-p: Use fw-test-start-p.
;;     fw-to-(next|previous-)(where|thing): Corrected use of prefix arg.
;;     fw-to-(next|previous-)thing, fw--to-next/prev-thing: Added optional arg INTERACTIVEP.
;;       Corrected for args N and READP.  If fw-last-thing then do not read.
;;     fw--to-next/prev-(where|thing): Corrected use of NOERROR.  Use fw--read-predicate.  Made
;;       args except NEXT/PREV optional.  Added arg INTERACTIVEP.
;;     fw--to-next/prev-where: Removed spurious occurrence of var PREDICATE.
;;     fw-thing-start-p: Provide lexical-binding version for Emacs 24+.
;;     fw-(to-)(next|previous-)where:
;;       Handle negative N.  Added args FORWARD-FN, BACKWARD-FN.  Define previous using next.
;;     fw-(to-)(next|previous-)where-vertical: Use lexical-binding (removed backquote construct).
;;       Handle PREDICATE and READP correctly.  Use 1, not N, for movement functions.  Define
;;       previous using next.
;;     fw-(next|previous)-where-vertical:
;;       Handle null PREDICATE.  Handle negative N.  Made PREDICATE optional too.  Define previous
;;       using next.
;;     fw-to-column-up: Define using down.
;;     fw-(up|down)ward-word: Removed _POS arg.  Added INTERACTIVEP arg.  Handle negative N.
;;       Define up using down.
;; 2018/07/01 dadams
;;     fw-(up|down)ward-word: Corrected use of bobp|eobp.
;; 2018/06/30 dadams
;;     Return more than just the position:
;;       fw--next/prev-where, fw--to-next/prev-where:
;;         Return cons of position found and result of applying predicate.
;;       fw-thing-start-p:
;;         Return cons of thing and end position.  Do not try to go back a char if at bob.
;;       fw--to-next/prev-thing: Adapt to change in fw--next/prev-thing.
;; 2018/06/29 dadams
;;     fw--next/prev-where: Use eobp for next, bobp for previous.
;;     fw-upward-word: Use bobp, not eobp.
;; 2018/03/27 dadams
;;     Renamed fw-line-move-down to fw-to-column-down, fw-line-move-up to fw-to-column-up.
;;     fw-to-column-(down|up): Move down only if point already at COLUMN or N > 1.
;; 2018/03/25 dadams
;;     fw-line-move-(down|up): Default COLUMN to current.  Prefix arg is for N, not COLUMN.
;; 2018/03/24 dadams
;;     Renamed library from next/prev.el to gowhere.el (prefix np- to fw-).
;;     Added: fw-line-move-down, fw-line-move-up, fw-word-char-after-p, fw-word-char-before-p,
;;            fw-not-word-char-after-p, fw-not-word-char-before-p.
;;     Renamed:  fw-to-next/prev-thing to fw--to-next/prev-thing,
;;               fw-to-(next|previous)-word-vertical to fw-(down|up)ward-word.
;;     Removed: np--line-move-visual-down-1.
;;     fw--next/prev-where: Call FORWARD-FN and BACKWARD-FN with no args now.
;;     fw-line-move-(down|up):
;;       Added args COLUMN and FORCE.  All args optional now.  Move to COLUMN first.
;;     fw-(to-)(next|previous)-where-vertical:
;;       Added args NOERROR and FORCE.  Effective predicate checks that column remains the same.
;;       Use constructed nullary movement function.
;;     fw-(down|up)ward-word: Transactional for double movement, and restore point if error.
;; 2018/03/18 dadams
;;     Added: np--line-move-visual-down-1, np-next-where-vertical, np-previous-where-vertical,
;;            np-to-next-where-vertical, np-to-previous-where-vertical.
;;     Renamed: np-to-next/prev-where to np--to-next/prev-where.
;;     np--(to-)next/prev-where: Added args FORWARD-FN and BACKWARD-FN.  Made most args optional.
;; 2018/03/17 dadams
;;     Created as next/prev.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'thingatpt+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup find-where nil
  "Next and previous functions to find where something is true."
  :prefix "fw-"
  :group 'editing :group 'convenience
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
find-where.el bug: \
&body=Describe bug here, starting with `emacs -Q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "https://www.emacswiki.org/emacs/download/find-where.el")
  :link '(url-link :tag "Description" "https://www.emacswiki.org/emacs/FindWhere")
  :link '(emacs-commentary-link :tag "Commentary" "find-where"))

(defvar fw-last-pred nil
  "Last predicate used by `to-next-where' or `to-previous-where'.")

(defun fw-next-where (predicate &optional start args n forward-fn backward-fn)
  "Find the Nth buffer position after START where PREDICATE is true.
If N is negative, find the Nth position before START.
Return nil if there is no such position.
Otherwise, return the found position in a cons (POSITION . VALUE),
 where VALUE is what PREDICATE returns.

PREDICATE must accept a buffer position as its first arg.
START defaults to point.
Non-nil ARGS are passed to PREDICATE as additional args.
N defaults to 1.

Optional args FORWARD-FN and BACKWARD-FN are functions for moving
forward and backward, respectively, to a position to test.  Only one
of them is used, depending on the sign of N, and it is called with no
arguments.  By default, they each move one character."
  (fw--next/prev-where
   (if (< n 0) 'previous 'next) predicate start args (if (< n 0) (- n) n) forward-fn backward-fn))

(defun fw-previous-where (predicate &optional start args n forward-fn backward-fn)
  "Find the Nth buffer position before START where PREDICATE is true.
Like as `fw-next-where', but this searches backward."
  (fw-next-where predicate start args (- n) forward-fn backward-fn))


(defun fw--next/prev-where (next/prev predicate &optional start args n forward-fn backward-fn)
  "Helper for `fw-next-where*' and `fw-previous-where*'.
Optional args FORWARD-FN and BACKWARD-FN are functions for moving
forward and backward, respectively.  Only one of them is invoked,
depending on NEXT/PREV, and it is called with no arguments.
By default, they move forward or backward one character."
  (setq n            (or n  1)
        start        (or start  (point))
        forward-fn   (or forward-fn   (lambda () (forward-char 1)))
        backward-fn  (or backward-fn  (lambda () (backward-char 1))))
  (let ((pos    nil)
        (count  0)
        res)
    (save-excursion
      (goto-char start)
      (while (and (< count n)  (not (if (eq 'next next/prev) (eobp) (bobp))))
        (funcall (if (eq 'next next/prev) forward-fn backward-fn))
        (when (setq res  (apply predicate (point) args))
          (setq pos    (point)
                count  (1+ count)))))
    (and res  (cons pos res))))

;;;###autoload
(defun fw-to-next-where (&optional predicate start args n noerror readp interactivep
                           forward-fn backward-fn)
  "Go to first buffer position after point where PREDICATE is true.
PREDICATE must accept a buffer position as its first arg.

You are prompted for PREDICATE if you use a plain prefix arg or it is
the first time you use the command.  Otherwise, PREDICATE is the value
of `fw-last-pred', which is the last predicate used by the command.

Return nil if there is no such position.
Otherwise, return the found position in a cons (POSITION . VALUE),
 where VALUE is what PREDICATE returns.

Non-interactively:

Go to Nth buffer position after START where PREDICATE is true.

Non-nil NOERROR means do not raise an error when there is no such
next position.  See `fw-next-where' for the other arguments.

Non-nil READP means read a new predicate.

Non-nil INTERACTIVEP means treat the call as if interactive: read a
predicate if none has been defined, and provide status messages.

Optional args FORWARD-FN and BACKWARD-FN are functions for moving
forward and backward, respectively, to a position to test.  Only one
of them is used, depending on the sign of N, and it is called with no
arguments.  By default, they each move one character."
  (interactive
   (let ((parg  current-prefix-arg))
     (list nil nil nil (if (atom parg) (prefix-numeric-value parg) 1) nil (consp parg) t)))
  (setq n  (or n  1))
  (fw--to-next/prev-where
   (if (< n 0) 'previous 'next) predicate start args (if (< n 0) (- n) n)
   noerror readp interactivep forward-fn backward-fn))

;;;###autoload
(defun fw-to-previous-where (&optional predicate start args n noerror readp interactivep
                               forward-fn backward-fn)
  "Go to first buffer position before point where PREDICATE is true.
Like `fw-to-next-where', but this moves backward."
  (interactive
   (let ((parg  current-prefix-arg))
     (list nil nil nil (if (atom parg) (prefix-numeric-value parg) 1) nil (consp parg) t)))
  (setq n  (or n  1))
  (fw-to-next-where predicate start args (- n) noerror readp interactivep forward-fn backward-fn))

(defun fw--to-next/prev-where (&optional next/prev predicate start args n noerror
                                 readp interactivep forward-fn backward-fn)
  "Helper for `fw-to-next-where' and `fw-to-previous-where'.
FORWARD-FN and BACKWARD-FN are functions for moving forward and
backward, respectively, by one unit (defaults: `forward-char',
`backward-char').  (Only one of them is used, depending on NEXT/PREV.)"
  (when readp     (setq fw-last-pred  nil))
  (when predicate (setq fw-last-pred  predicate))
  (when interactivep
    (unless fw-last-pred (setq fw-last-pred  (fw--read-predicate)))
    (message "Finding %s%s..." next/prev (if (= n 1) "" (format " (%d)" n))))
  (let ((res  (fw--next/prev-where next/prev fw-last-pred start args n forward-fn backward-fn)))
    (if (not res)
        (unless noerror (error "Not found"))
      (goto-char (car res)))
    (when interactivep
      (message "Finding %s%s...done" next/prev (if (= n 1) "" (format " (%d)" n))))
    res))

(defun fw--read-predicate ()
  "Read a predicate.
You are prompted for a function name or lambda expression."
  ;; The function needs to accept a buffer position as its first arg, to be usable by find-where.
  (let (pred)
    (while (not (functionp pred))
      (setq pred  (read (let (this-command) (read-string "Predicate: ")))))
    (when (fboundp 'func-arity)         ; Emacs 26+
      (let ((arity  (if (subrp pred) (subr-arity pred) (func-arity pred))))
        (while (or (not (>= (car arity) 1))
                   (not (= (car arity) 1))) ; Cannot know how to read ARGS.
          (setq pred   (read (let (this-command) (read-string "Predicate (at least 1 arg): ")))
                arity  (if (subrp pred) (subr-arity pred) (func-arity pred))))))
    pred))

(defun fw-word-char-after-p (pos)
  "Return non-nil if next char is a word constituent."
  (equal '(2) (syntax-after pos)))

(defun fw-not-word-char-after-p (pos)
  "Return non-nil if next char is not a word constituent."
  (not (equal '(2) (syntax-after pos))))

(defun fw-word-char-before-p (pos)
  "Return non-nil if previous char is a word constituent."
  (equal '(2) (syntax-after (1- pos))))

(defun fw-not-word-char-before-p (pos)
  "Return non-nil if previous char is not a word constituent."
  (not (equal '(2) (syntax-after (1- pos)))))

;; (defun fw-go-past-next-word (pos &optional n)
;;   "Same behavior as `forward-word'."
;;   (interactive "i\np")
;;   (dotimes (ii  n)
;;     (if (and (not (eobp))  (fw-word-char-after-p (point)))
;;         (fw-to-next-where #'fw-not-word-char-after-p)
;;       (fw-to-next-where #'fw-word-char-after-p)
;;       (fw-to-next-where #'fw-not-word-char-after-p))))

;; (defun fw-go-before-previous-word (pos &optional n)
;;   "Same behavior as `backward-word'."
;;   (interactive "i\np")
;;   (dotimes (ii  n)
;;     (if (and (not (bobp))  (fw-word-char-before-p (point)))
;;         (fw-to-previous-where #'fw-not-word-char-before-p)
;;       (fw-to-previous-where #'fw-word-char-before-p)
;;       (fw-to-previous-where #'fw-not-word-char-before-p))))

(defun fw-test-start-p (position predicate &rest args)
  "Return non-nil if PREDICATE is true at POSITION and not just before it.
Otherwise return nil.  The non-nil return value is whatever PREDICATE
returns at POSITION.

PREDICATE is applied to (only) the arguments in list ARGS (empty by
default)."
  (let ((here  (save-excursion (goto-char position) (apply predicate args))))
    (and here
         (or (bobp)
             (not (equal here (save-excursion (goto-char (1- position)) (apply predicate args)))))
         here)))


;;; Vertical movement ------------------------------------------------

(when (fboundp 'line-move-visual)       ; Emacs 25+

  (defun fw-to-column-down (&optional column n noerror force)
    "Move to COLUMN.  If already there or N > 1 then move down N lines.
The line movement uses `line-move-visual'.
COLUMN defaults to the current column.
N is the numeric prefix argument.  It defaults to 1.
Non-nil NOERROR means do not raise an error.  Otherwise, raise an
 error if `line-move-visual' would raise an error.
Non-nil FORCE means force moving to COLUMN, inserting SPC chars as
needed."
    (interactive "i\np")
    (let ((opoint  (point)))
      (setq column  (or column  (current-column))
            n       (or n  1))
      (move-to-column column force)
      (when (or (= opoint (point))  (> n 1)) (line-move-visual n noerror))))

  (defun fw-to-column-up (&optional column n noerror force)
    "Same as `fw-to-column-down', except move up, not down."
    (interactive "i\np")
    (fw-to-column-down column (- n) noerror force))

  (defun fw-next-where-vertical (&optional predicate start args n noerror force)
    "Like `fw-next-where', but look down instead of forward (right).
Optional args NOERROR and FORCE are as for `fw-to-column-down'."
    (setq n  (or n  1))
    (let* ((col   (current-column))
           (pred  (lambda (pos &rest args)
                    (and (= (current-column) col)
                         (or (not (functionp predicate))  (apply predicate pos args))))))
      (fw--next/prev-where (if (< n 0) 'previous 'next) pred start args (if (< n 0) (- n) n)
                           (lambda () (fw-to-column-down col n noerror force)))))

  (defun fw-previous-where-vertical (&optional predicate start args n noerror force)
    "Like `fw-next-where-vertical', but look up instead of down."
    (setq n  (or n  1))
    (fw-next-where-vertical predicate start args (- n) noerror force))

  (defun fw-to-next-where-vertical (&optional predicate start args n noerror
                                      force readp interactivep)
    "Like `fw-to-next-where', but move down instead of forward (across).
Find the Nth position directly below START where PREDICATE is true.
If N is negative, find the Nth position directly above START.

Optional args NOERROR and FORCE are as for `fw-to-column-down'.
Other args are as for `fw-to-next-where'."
    (interactive
     (let ((parg  current-prefix-arg))
       (list nil nil nil (if (atom parg) (prefix-numeric-value parg) 1) nil nil (consp parg) t)))
    (setq n  (or n  1))
    ;; If PREDICATE arg, use it.  ELSE if READP arg, read PREDICATE and use it.
    ;; In each of those cases, use PREDICATE to define a column-specific predicate, and use that.
    ;; ELSE (no PREDICATE and no READP arg provided), use `fw-last-pred' (which could be nil).
    (setq predicate  (or predicate  (and readp  (fw--read-predicate))))
    (let* ((col   (current-column))
           (pred  (if (not predicate)
                      fw-last-pred
                    (lambda (pos &rest args)
                      (and (= (current-column) col)
                           (or (not (functionp predicate))  (apply predicate pos args)))))))
      (fw--to-next/prev-where (if (< n 0) 'previous 'next) pred start args (if (< n 0) (- n) n)
                              noerror nil interactivep
                              ;; Use 1, not N, so can test after each move.
                              (lambda () (fw-to-column-down col (if (< n 0) -1 1) noerror force))
                              (lambda () (fw-to-column-down col (if (< n 0) -1 1) noerror force)))))

  (defun fw-to-previous-where-vertical (&optional predicate start args n noerror
                                          force readp interactivep)
    "Like `fw-to-previous-where', but move up instead of backward (across)."
    (interactive
     (let ((parg  current-prefix-arg))
       (list nil nil nil (if (atom parg) (prefix-numeric-value parg) 1) nil nil (consp parg) t)))
    (setq n  (or n  1))
    (fw-to-next-where-vertical
     predicate start args (if (< n 0) n (- n)) noerror force readp interactivep))

  ;; Could be called `fw-next-word-vertical', but it moves _after_ the word, like `forward-word'.
  (defun fw-downward-word (&optional n interactivep)
    "Like `forward-word', but move down, not across."
    (interactive "p\np")
    (setq n  (or n  1))
    (let ((next/prev  (if (< n 0) 'previous 'next)))
      (when interactivep (message "Finding %s%s word%s vertically..."
                                  next/prev
                                  (if (memq n '(1 -1)) "" (format " (%d)" (abs n)))
                                  (if (memq n '(1 -1)) "" "s")))
      (dotimes (_i  (abs n))
        (if (or (eobp)  (fw-word-char-after-p (point)))
            (fw-to-next-where-vertical #'fw-not-word-char-after-p nil nil (if (< n 0) -1 1))
          (let ((pos  (point)))
            (condition-case err
                (progn
                  (fw-to-next-where-vertical #'fw-word-char-after-p nil nil (if (< n 0) -1 1))
                  (fw-to-next-where-vertical #'fw-not-word-char-after-p nil nil (if (< n 0) -1 1))
                  (when interactivep (message "Finding %s%s word%s vertically...done"
                                              next/prev
                                              (if (memq n '(1 -1)) "" (format " (%d)" (abs n)))
                                              (if (memq n '(1 -1)) "" "s"))))
              (error (progn (goto-char pos) (error "%s" (error-message-string err))))))))))

  ;; Could be called `fw-previous-word-vertical', but moves _before_  word, like `backward-word'.
  (defun fw-upward-word (&optional n interactivep)
    "Like `backward-word', but move up, not across."
    (interactive "p\np")
    (fw-downward-word (- n) interactivep))

  )

;;; THING movement ---------------------------------------------------

(defvar fw-last-thing nil
  "Last thing used by `fw-to-next-thing' or `fw-to-previous-thing'.")

(defun fw-next-thing (thing &optional start n)
  "Find the Nth buffer position after START that is the start of a THING.
Return nil if there is no such position.
Otherwise, return the found position in a cons (POSITION . VALUE),
 where VALUE is what `fw-thing-start-p' returns.

START defaults to point.
N defaults to 1."
  (fw--next/prev-thing 'next thing start n))

(defun fw-previous-thing (thing &optional start n)
  "Find the Nth buffer position before START that is the start of a THING.
Same as `fw-next-thing' except this searches backward."
  (fw--next/prev-thing 'previous thing start n))

(defun fw--next/prev-thing (next/prev thing start n)
  "Helper for `fw-next-thing' and `fw-previous-thing'."
  (fw--next/prev-where next/prev #'fw-thing-start-p start (list thing) n))

(defun fw-thing-start-p (position thing)
  "Return true if POSITION is at the start of a THING, otherwise nil.
A true value means also that (1- POSITION) is not on the same THING,
or else point is at the beginning of the buffer.

The true value returned is a cons (THE-THING . END), where THE-THING is
the THING that starts at POSITION, and END is the buffer position of its end.
THE-THING."
  (fw-test-start-p position
                   (if (> emacs-major-version 23) ; Emacs 24+
                       (lambda ()
                         (let ((bounds  (tap-bounds-of-thing-at-point thing)))
                           (and bounds
                                (= position (car bounds))
                                (cons (buffer-substring (car bounds) (cdr bounds))
                                      (cdr bounds)))))
                     `(lambda ()        ; No `lexical-binding' before Emacs 24
                       (let ((bounds  (tap-bounds-of-thing-at-point ',thing)))
                         (and bounds
                              (= ,position (car bounds))
                              (cons (buffer-substring (car bounds) (cdr bounds))
                                    (cdr bounds))))))))

;;;###autoload
(defun fw-to-next-thing (&optional thing start n noerror readp interactivep)
  "Go to first buffer position after point that is the start of a THING.
You are prompted for THING if you use a plain prefix arg or if this is
the first time you use the command.  Otherwise, THING is the value of
`fw-last-thing', which is the last THING used by the command.

N is the numeric prefix argument.  It defaults to 1.

Non-interactively:
Go to Nth buffer position after START that is the start of a THING.
Non-nil NOERROR means do not raise an error when there is no such
next position.

Return what `fw-thing-start-p' returns:
* nil if there is no such position.
* a cons (START THE-THING . END), where THE-THING is the THING, and
  START and END are its buffer-position bounds."
  (interactive (let ((parg  current-prefix-arg))
                 (list nil nil (if (atom parg) (prefix-numeric-value parg) 1) nil (consp parg) t)))
  (fw--to-next/prev-thing 'next thing start n noerror readp interactivep))

;;;###autoload
(defun fw-to-previous-thing (&optional thing start n noerror readp interactivep)
  "Go to first buffer position before point that is the start of a THING.
Same as `fw-to-next-thing', except this moves backward."
  (interactive (let ((parg  current-prefix-arg))
                 (list nil nil (if (atom parg) (prefix-numeric-value parg) 1) nil (consp parg) t)))
  (fw--to-next/prev-thing 'previous thing start n noerror readp interactivep))

(defun fw--to-next/prev-thing (next/prev &optional thing start n noerror readp interactivep)
  "Helper for `fw-to-next-thing' and `fw-to-previous-thing'."
  (when readp (setq fw-last-thing  nil))
  (when thing (setq fw-last-thing  thing))
  (unless fw-last-thing
    (while (not fw-last-thing)
      (setq fw-last-thing  (read (let (this-command) (read-string "Thing: "))))))
  (when interactivep (message "Finding %s%s %s..."
                              next/prev
                              (if (= n 1) "" (format " (%d)" n))
                              fw-last-thing))
  (let ((res  (fw--next/prev-thing next/prev fw-last-thing start n)))
    (if (not res)
        (unless noerror (error "Not found"))
      (goto-char (car res))
      (when interactivep (message "Finding %s%s %s...done"
                                  next/prev
                                  (if (= n 1) "" (format " (%d)" n))
                                  fw-last-thing)))
    res))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'find-where)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find-where.el ends here

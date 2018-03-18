;;; next-prev.el --- Next and previous functions and motion commands. -*- lexical-binding:t -*-
;;
;; Filename: next-prev.el
;; Description:  Next and previous functions and motion commands.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2018, Drew Adams, all rights reserved.
;; Created: Sat Mar 17 10:13:09 2018 (-0700)
;; Version: 2018-03-17
;; Package-Requires: (thingatpt+ "0")
;; Last-Updated: Sun Mar 18 14:22:15 2018 (-0700)
;;           By: dradams
;;     Update #: 153
;; URL: https://www.emacswiki.org/emacs/download/next-prev.el
;; Doc URL: https://www.emacswiki.org/emacs/NextPrevious
;; Keywords: motion thing
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Next and previous functions and motion commands.
;;
;;  This library is about getting, or moving to, the next or previous
;;  buffer position that satisfies some predicate.
;;
;;  It includes functions that get or move to the next or previous
;;  THING, as defined by library `thingatpt+.el', which is required.
;;  Library `thingatpt+.el' uses and enhances standard library
;;  `thingatpt.el'.
;;
;;  The workhorse functions here are `np-next-where' and
;;  `np-previous-where'.  `np-next-where' returns the next (or the nth
;;  next) buffer position after point (or after a given position)
;;  where a given predicate is true - or nil if there is no such
;;  position.  `np-previous-where' does the same thing in the reverse
;;  direction.
;;
;;  Functions `np-next-where-vertical' and
;;  `np-previous-where-vertical' are similar, but they find a position
;;  that is directly below or above point, instead of after or before
;;  point.
;;
;;  The main commands here are `np-to-next-where' and
;;  `np-to-previous-where'.  They move the cursor to positions
;;  `np-next-where' and `np-previous-where', respectively.
;;
;;  Commands `np-to-next-where-vertical' and
;;  `np-to-previous-where-vertical' are similar, but they move to a
;;  position directly below or above point, instead of after or before
;;  point.
;;
;;  When repeated, all of these conditional-motion commands reuse the
;;  same predicate as the last time (it is the value of variable
;;  `np-to-where-last'), but a prefix arg makes them prompt you for
;;  the predicate to use.  The predicate you enter must accept at
;;  least one argument, and its first argument must be a buffer
;;  position (the position to test).
;;
;;  A typical use might check something about the character at (i.e.,
;;  after) that position.
;;
;;  The thing-related functions are `np-next-thing' and
;;  `np-previous-thing'.  They just use `np-next-where' and
;;  `np-previous-where' with a predicate that tests whether the
;;  position is at the start of a given type of thing, where being at
;;  the start also means that the previous buffer position is not on
;;  the same thing (not just the same type of thing).  For instance,
;;  if the thing type passed to `np-next-thing' is `list' then it
;;  returns the position of the start of the next list (or nil, if
;;  none).
;;
;;  The thing-related commands are `np-to-next-thing' and
;;  `np-to-previous-thing'.  They move the cursor to positions
;;  `np-next-thing' and `np-previous-thing', respectively.
;;
;;  When repeated, these thing-related commands reuse the same thing
;;  type as the last time (it is the value of variable
;;  `np-to-thing-last'), but a prefix arg makes them prompt you for
;;  the thing type to use.
;;
;;  You can bind any of the commands defined here to keys, of course.
;;  But you can also easily define other commands that make use of
;;  them, and bind those commands to keys.
;;
;;  For example:
;;
;;  (defun doc-face-p (position)
;;    "Return non-nil if char at POSITION has face `font-lock-doc-face'."
;;    (and (eq (get-text-property position 'face) 'font-lock-doc-face)
;;         (or (eq position (point-min))
;;             (not (eq (get-text-property (1- position) 'face)
;;                      'font-lock-doc-face)))))
;;
;;  (defun to-next-doc-face (n)
;;    "Move to next doc face.
;;  With numeric prefix arg N, move to Nth next doc face."
;;    (interactive "p")
;;    (np-to-next-where #'doc-face-p nil '(font-lock-doc-face) n))
;;
;;  And because the predicate can accept additional args, besides the
;;  position, you can use a predicate that accepts the face to look
;;  for.
;;
;;  (defun face-p (position face)
;;    "Return non-nil if char at POSITION has FACE."
;;    (and (eq (get-text-property position 'face) face)
;;         (or (eq position (point-min))
;;             (not (eq (get-text-property (1- position) 'face)
;;                      face)))))
;;
;;  (defvar last-face nil "Last face used by `to-next-face'.")
;;
;;  (defun to-next-face (arg)
;;    "Move to next use of FACE.
;;  With a plain prefix arg, prompt for FACE.
;;  With numeric prefix arg N, move to Nth next FACE."
;;    (interactive "P")
;;    (if (or (consp arg)  (not last-face))
;;        (setq last-face  (read-face-name "Face: ")
;;              arg        1)
;;      (setq arg  (prefix-numeric-value arg)))
;;    (np-to-next-where #'face-p nil (list last-face) arg))
;;
;;  As an example of defining a next-thing command, this is how you
;;  might define a command to move among sexps:
;;
;;  (defun to-next-sexp (n)
;;    "Go to next start of a sexp."
;;    (interactive "p")
;;    (np-to-next-thing 'sexp nil n))
;;
;;  Or among strings:
;;
;;  (defun to-next-string (n)
;;    "Go to next start of a string."
;;    (interactive "p")
;;    (np-to-next-thing 'string nil n))
;;
;;
;;
;;  Commands defined here:
;;
;;    `np-to-next-thing', `np-to-next-where',
;;    `np-to-next-where-vertical', `np-to-previous-thing',
;;    `np-to-previous-where', `np-to-previous-where-vertical'.
;;
;;  Non-interactive functions defined here:
;;
;;    `np--line-move-visual-down-1', `np--next/prev-thing',
;;    `np--next/prev-where', `np--to-next/prev-where',
;;    `np-next-thing', `np-next-where', `np-next-where-vertical',
;;    `np-previous-thing', `np-previous-where',
;;    `np-previous-where-vertical', `np-thing-start-p',
;;    `np-to-next/prev-thing'.
;;
;;  Internal variables defined here:
;;
;;    `np-to-thing-last', `np-to-where-last'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2018/03/18 dadams
;;     Added: np--line-move-visual-down-1, np-next-where-vertical, np-previous-where-vertical,
;;            np-to-next-where-vertical, np-to-previous-where-vertical.
;;     Renamed: np-to-next/prev-where to np--to-next/prev-where.
;;     np--(to-)next/prev-where: Added args FORWARD-FN and BACKWARD-FN.  Made most args optional.
;; 2018/03/17 dadams
;;     Created.
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

(defgroup next-prev nil
  "Next and previous functions."
  :prefix "np-"
  :group 'editing :group 'convenience
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
next-prev.el bug: \
&body=Describe bug here, starting with `emacs -Q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "https://www.emacswiki.org/emacs/download/next-prev.el")
  :link '(url-link :tag "Description" "https://www.emacswiki.org/emacs/NextPrevious")
  :link '(emacs-commentary-link :tag "Commentary" "next-prev"))

(defvar np-to-where-last nil
  "Last predicate used by `to-next-where' or `to-previous-where'.")

(defun np-next-where (predicate &optional start args n)
  "Return Nth buffer position after START where PREDICATE is true.
Return nil if there is no such position.

PREDICATE must accept a buffer position as its first arg.
Non-nil ARGS are passed to PREDICATE as additional args.
START defaults to point.
N defaults to 1."
  (np--next/prev-where 'next predicate start args n))

(defun np-previous-where (predicate &optional start args n)
  "Return Nth buffer position before START where PREDICATE is true.
Return nil if there is no such position.

PREDICATE must accept a buffer position as its first arg.
Non-nil ARGS are passed to PREDICATE as additional args.
START defaults to point.
N defaults to 1."
  (np--next/prev-where 'previous predicate start args n))

(when (fboundp 'line-move-visual)       ; Emacs 25+

  (defun np-next-where-vertical (predicate &optional start args n)
    "Like `np-next-where', but look down instead of forward (right)."
    (np--next/prev-where 'next predicate start args n #'line-move-visual))

  (defun np-previous-where-vertical (predicate &optional start args n)
    "Like `np-previous-where', but look up instead of backward (left)."
    (np--next/prev-where 'previous predicate start args n nil #'np--line-move-visual-down-1))

  (defun np--line-move-visual-down-1 ()
    "Move down one visual line."
    (line-move-visual -1))

  )

(defun np--next/prev-where (next/prev predicate &optional start args n forward-fn backward-fn)
  "Helper for `np-next-where*' and `np-previous-where*'.
Optional args FORWARD-FN and BACKWARD-FN are functions for moving
forward and backward, respectively, by one unit (defaults:
`forward-char', `backward-char').  (Only one of them is used,
depending on NEXT/PREV.)"
  (setq n            (or n  1)
        start        (or start  (point))
        forward-fn   (or forward-fn   #'forward-char)
        backward-fn  (or backward-fn  #'backward-char))
  (let ((pos    nil)
        (count  0))
    (save-excursion
      (goto-char start)
      (while (and (< count n)  (not (eobp)))
        (funcall (if (eq 'next next/prev) forward-fn backward-fn) 1)
        (when (apply predicate (point) args)
          (setq pos    (point)
                count  (1+ count)))))
    pos))

;;;###autoload
(defun np-to-next-where (&optional predicate start args n noerror readp interactivep)
  "Go to first buffer position after point where PREDICATE is true.
PREDICATE must accept a buffer position as its first arg.  You are
prompted for PREDICATE if you use a prefix arg.  Otherwise, PREDICATE
is the value of `np-to-where-last', which is the last predicate used
by the command.

Non-interactively:
Go to Nth buffer position after START where PREDICATE is true.
Non-nil NOERROR means do not raise an error when there is no such
next position.  See `np-next-where' for the other arguments."
  (interactive "i\ni\ni\ni\ni\nP\np")
  (np--to-next/prev-where 'next predicate start args n noerror readp interactivep))

;;;###autoload
(defun np-to-previous-where (&optional predicate start args n noerror readp interactivep)
  "Go to first buffer position before point where PREDICATE is true.
Same as `np-to-next-where' except this moves backward."
  (interactive "i\ni\ni\ni\ni\nP")
  (np--to-next/prev-where 'previous predicate start args n noerror readp interactivep))

(when (fboundp 'line-move-visual)       ; Emacs 25+

  (defun np-to-next-where-vertical (&optional predicate start args n noerror readp interactivep)
    "Like `np-to-next-where', but move down instead of forward (right)."
    (interactive "i\ni\ni\ni\ni\nP\np")
    (np--to-next/prev-where 'next predicate start args n noerror readp interactivep
                            #'line-move-visual))

  (defun np-to-previous-where-vertical (&optional predicate start args n noerror readp interactivep)
    "Like `np-previous-where', but move up instead of backward (left)."
    (interactive "i\ni\ni\ni\ni\nP")
    (np--to-next/prev-where 'previous predicate start args n noerror readp interactivep
                            nil #'np--line-move-visual-down-1))

  )

(defun np--to-next/prev-where (&optional next/prev predicate start args n noerror readp interactivep
                               forward-fn backward-fn)
  "Helper for `np-to-next-where' and `np-to-previous-where'.
FORWARD-FN and BACKWARD-FN are functions for moving forward and
backward, respectively, by one unit (defaults: `forward-char',
`backward-char').  (Only one of them is used, depending on NEXT/PREV.)"
  (when readp     (setq np-to-where-last nil))
  (when predicate (setq np-to-where-last predicate))
  ;; $$ An alternative - using this means that it reads whenever not repeated (or C-u).
  ;;   (when (or (not (eq this-command last-command))  readp)
  ;;     (setq np-to-where-last nil))
  (unless (or predicate  (and np-to-where-last  (eq this-command last-command)))
    (while (not (functionp np-to-where-last))
      (setq np-to-where-last  (read (let (this-command) (read-string "Predicate: ")))))
    (when (fboundp 'func-arity)         ; Emacs 26+
      (let ((arity  (if (subrp np-to-where-last)
                        (subr-arity np-to-where-last)
                      (func-arity np-to-where-last))))
        (while (or (not (>= (car arity) 1))
                   (and interactivep  (not (= (car arity) 1)))) ; Cannot know how to read ARGS.
          (setq np-to-where-last  (read (let (this-command)
                                          (read-string "Predicate (at least 1 arg): ")))
                arity                            (if (subrp np-to-where-last)
                                                     (subr-arity np-to-where-last)
                                                   (func-arity np-to-where-last)))))))
  (let ((pos  (np--next/prev-where next/prev np-to-where-last start args n forward-fn backward-fn)))
    (if pos (goto-char pos) (unless noerror (error "No such position")))))


(defvar np-to-thing-last nil
  "Last thing used by `np-to-next-thing' or `np-to-previous-thing'.")

(defun np-next-thing (thing &optional start n)
  "Return Nth buffer position after START that is the start of a THING.
Return nil if there is no such position.
START defaults to point.
N defaults to 1."
  (np--next/prev-thing 'next thing start n))

(defun np-previous-thing (thing &optional start n)
  "Return Nth buffer position before START that is the start of a THING.
Return nil if there is no such position.
START defaults to point.
N defaults to 1."
  (np--next/prev-thing 'previous thing start n))

(defun np--next/prev-thing (next/prev thing start n)
  "Helper for `np-next-thing' and `np-previous-thing'."
  (np--next/prev-where next/prev #'np-thing-start-p start (list thing) n))

(defun np-thing-start-p (position thing)
  "Return non-nil if POSITION is at the start of a THING.
This also means that (1- POSITION) is not on the same THING.
Else return nil."
  (let ((bnds  (save-excursion (goto-char position) (tap-bounds-of-thing-at-point thing))))
    (and bnds
         (= position (car bnds))
         (not (equal bnds (save-excursion (goto-char (1- position))
                                          (tap-bounds-of-thing-at-point thing)))))))

;;;###autoload
(defun np-to-next-thing (&optional thing start n noerror readp)
  "Go to first buffer position after point that is the start of a THING.
Non-interactively:
Go to Nth buffer position after START that is the start of a THING.
Non-nil NOERROR means do not raise an error when there is no such
next position."
  (interactive "i\ni\ni\ni\ni\nP\np")
  (np-to-next/prev-thing 'next thing start n noerror readp))

;;;###autoload
(defun np-to-previous-thing (&optional thing start n noerror readp)
  "Go to first buffer position before point that is the start of a THING.
Same as `np-to-next-thing', except this moves backward."
  (interactive "i\ni\ni\ni\ni\nP\np")
  (np-to-next/prev-thing 'previous thing start n noerror readp))

(defun np-to-next/prev-thing (next/prev thing start n noerror readp)
  "Helper for `np-to-next-thing' and `np-to-previous-thing'."
  (when readp (setq np-to-thing-last nil))
  (when thing (setq np-to-thing-last thing))
  (unless (or thing  (and np-to-thing-last  (eq this-command last-command)))
    (while (not np-to-thing-last)
      (setq np-to-thing-last  (read (let (this-command) (read-string "Thing: "))))))
  (let ((pos  (np--next/prev-thing next/prev np-to-thing-last start n)))
    (if pos (goto-char pos) (unless noerror (error "No such position")))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'next-prev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; next-prev.el ends here

;;; gowhere.el --- Get or go to next position that satisfies a predicate.  -*- lexical-binding:t -*-
;;
;; Filename: gowhere.el
;; Description:  Get or go to next position that satisfies a predicate.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2018, Drew Adams, all rights reserved.
;; Created: Sat Mar 17 10:13:09 2018 (-0700)
;; Version: 2018-03-17
;; Package-Requires: (thingatpt+ "0")
;; Last-Updated: Sun Jul  1 15:11:22 2018 (-0700)
;;           By: dradams
;;     Update #: 342
;; URL: https://www.emacswiki.org/emacs/download/gowhere.el
;; Doc URL: https://www.emacswiki.org/emacs/GoWhere
;; Keywords: motion thing
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
;;    Get or go to the next position that satisfies some predicate.
;;
;;  The library also includes functions that get or move to the next
;;  or previous THING, as defined by library `thingatpt+.el', which is
;;  required.  Library `thingatpt+.el' uses and enhances standard
;;  library `thingatpt.el'.
;;
;;  The workhorse functions here are `gw-next-where' and
;;  `gw-previous-where'.  `gw-next-where' returns the next (or the nth
;;  next) buffer position after point (or after a given position)
;;  where a given predicate is true - or nil if there is no such
;;  position.  `gw-previous-where' does the same thing in the reverse
;;  direction.
;;
;;  Functions `gw-next-where-vertical' and
;;  `gw-previous-where-vertical' are similar, but they find a position
;;  that is directly below or above point, instead of after or before
;;  point.
;;
;;  The main commands here are `gw-to-next-where' and
;;  `gw-to-previous-where'.  They move the cursor to positions
;;  `gw-next-where' and `gw-previous-where', respectively.
;;
;;  Commands `gw-to-next-where-vertical' and
;;  `gw-to-previous-where-vertical' are similar, but they move to a
;;  position directly below or above point, instead of after or before
;;  point.
;;
;;  When repeated, all such conditional-motion commands reuse the same
;;  predicate as the last time (it is the value of variable
;;  `gw-to-where-last'), but a prefix argument makes them prompt you
;;  for the predicate to use.  The predicate you enter must accept at
;;  least one argument, and its first argument must be a buffer
;;  position (the position to test).
;;
;;  A typical use might check something about the character at (i.e.,
;;  after) that position.
;;
;;  The thing-related functions are `gw-next-thing' and
;;  `gw-previous-thing'.  They just use `gw-next-where' and
;;  `gw-previous-where' with a predicate that tests whether the
;;  position is at the start of a given type of thing, where being at
;;  the start also means that the previous buffer position is not on
;;  the same thing (not just the same type of thing).  For instance,
;;  if the thing type passed to `gw-next-thing' is `list' then it
;;  returns the position of the start of the next list (or nil, if
;;  none).
;;
;;  The thing-related commands are `gw-to-next-thing' and
;;  `gw-to-previous-thing'.  They move the cursor to positions
;;  `gw-next-thing' and `gw-previous-thing', respectively.
;;
;;  When repeated, these commands reuse the same thing type as the
;;  last time (it is the value of variable `gw-to-thing-last'), but a
;;  prefix argument makes them prompt you for the thing type to use.
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
;;    (gw-to-next-where #'doc-face-p nil nil n))
;;
;;  Note the use here of two complementary tests within the predicate,
;;  `doc-face-p'.  The char at the tested position must pass the test
;;  (having property `font-lock-doc-face'), and the preceding char
;;  must not pss the test.  That is, the test finds the next char that
;;  is the first one to pass the test.  This is typical of a predicate
;;  used with `gowhere.el' functions.
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
;;    (gw-to-next-where #'face-p nil (list last-face) arg))
;;
;;  As an example of defining a next-thing command, this is how you
;;  might define a command to move among sexps:
;;
;;  (defun to-next-sexp (n)
;;    "Go to next start of a sexp."
;;    (interactive "p")
;;    (gw-to-next-thing 'sexp nil n))
;;
;;  Or among strings:
;;
;;  (defun to-next-string (n)
;;    "Go to next start of a string."
;;    (interactive "p")
;;    (gw-to-next-thing 'string nil n))
;;
;;  Note that the various `gw-next-*' and `gw-previous-*' commands
;;  move to the beginning of the next or previous place where
;;  something is true.  For example, if you use `gw-next-thing' with
;;  THING `word' then the cursor moves to the beginning of each word.
;;  This is different from typical Emacs `forward-*' and `backward-*'
;;  commands, which move _past_ the end or the beginning of something.
;;
;;  Typical `forward-*' commands essentially perform the following
;;  sequence of actions, expressed in terms of `gw-next-*':
;;
;;    1. While some predicate PRED-X is NOT true, do `gw-to-next-X'.
;;    2. Do `gw-to-next-NOT-X'.
;;
;;  Step 1 moves to the next place X is true (e.g., a word beginning).
;;  Step 2 moves just past where X continues to be true (e.g., just
;;  after the end of the word).
;;
;;  Partly as a way of illustrating this, commands `gw-downward-word'
;;  and `gw-upward-word' act like Emacs `forward-word' and
;;  `backward-word', but they move through text vertically, not
;;  horizontally.  They are defined using steps 1 and 2.
;;
;;
;;  Commands defined here:
;;
;;    `gw-downward-word', `gw-to-column-down', `gw-to-column-up',
;;    `gw-to-next-thing', `gw-to-next-where',
;;    `gw-to-next-where-vertical', `gw-to-previous-thing',
;;    `gw-to-previous-where', `gw-to-previous-where-vertical',
;;    `gw-upward-word'.
;;
;;  Non-interactive functions defined here:
;;
;;    `gw--next/prev-thing', `gw--next/prev-where',
;;    `gw--to-next/prev-thing', `gw--to-next/prev-where',
;;    `gw-next-thing', `gw-next-where', `gw-next-where-vertical',
;;    `gw-not-word-char-after-p', `gw-not-word-char-before-p',
;;    `gw-previous-thing', `gw-previous-where',
;;    `gw-previous-where-vertical', `gw-thing-start-p',
;;    `gw-word-char-after-p', `gw-word-char-before-p'.
;;
;;  Internal variables defined here:
;;
;;    `gw-to-thing-last', `gw-to-where-last'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 2018/07/01 dadams
;;     gw-(up|down)ward-word: Corrected use of bobp|eobp.
;; 2018/06/30 dadams
;;     Return more than just the position:
;;       gw--next/prev-where, gw--to-next/prev-where:
;;         Return cons of position found and result of applying predicate.
;;       gw-thing-start-p: Return cons of thing and end position.  Do not try to go back a char if at bob.
;;       gw--to-next/prev-thing: Adapt to change in gw--next/prev-thing.
;; 2018/06/29 dadams
;;     gw--next/prev-where: Use eobp for next, bobp for previous.
;;     gw-upward-word: Use bobp, not eobp.
;;; Change Log:
;; 2018/03/27 dadams
;;     Renamed gw-line-move-down to gw-to-column-down, gw-line-move-up to gw-to-column-up.
;;     gw-to-column-(down|up): Move down only if point already at COLUMN or N > 1.
;; 2018/03/25 dadams
;;     gw-line-move-(down|up): Default COLUMN to current.  Prefix arg is for N, not COLUMN.
;; 2018/03/24 dadams
;;     Renamed library from next/prev.el to gowhere.el (prefix np- to gw-).
;;     Added: gw-line-move-down, gw-line-move-up, gw-word-char-after-p, gw-word-char-before-p,
;;            gw-not-word-char-after-p, gw-not-word-char-before-p.
;;     Renamed:  gw-to-next/prev-thing to gw--to-next/prev-thing,
;;               gw-to-(next|previous)-word-vertical to gw-(down|up)ward-word.
;;     Removed: np--line-move-visual-down-1.
;;     gw--next/prev-where: Call FORWARD-FN and BACKWARD-FN with no args now.
;;     gw-line-move-(down|up): Added args COLUMN and FORCE.  All args optional now.  Move to COLUMN first.
;;     gw-(to-)(next|previous)-where-vertical:
;;       Added args NOERROR and FORCE.  Effective predicate also checks that column remains the same.
;;       Use constructed nullary movement function.
;;     gw-(down|up)ward-word: Transactional for double movement, and restore point if error.
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

(defgroup gowhere nil
  "Next and previous functions."
  :prefix "gw-"
  :group 'editing :group 'convenience
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
gowhere.el bug: \
&body=Describe bug here, starting with `emacs -Q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "https://www.emacswiki.org/emacs/download/gowhere.el")
  :link '(url-link :tag "Description" "https://www.emacswiki.org/emacs/GoWhere")
  :link '(emacs-commentary-link :tag "Commentary" "gowhere"))

(defvar gw-to-where-last nil
  "Last predicate used by `to-next-where' or `to-previous-where'.")

(defun gw-next-where (predicate &optional start args n)
  "Find the Nth buffer position after START where PREDICATE is true.
Return nil if there is no such position.
Otherwise, return the found position in a cons (POSITION . VALUE),
 where VALUE is what PREDICATE returns.

PREDICATE must accept a buffer position as its first arg.
Non-nil ARGS are passed to PREDICATE as additional args.
START defaults to point.
N defaults to 1."
  (gw--next/prev-where 'next predicate start args n))

(defun gw-previous-where (predicate &optional start args n)
  "Find the Nth buffer position before START where PREDICATE is true.
Same as `gw-next-where' except this searches backward."
  (gw--next/prev-where 'previous predicate start args n))

(defun gw--next/prev-where (next/prev predicate &optional start args n forward-fn backward-fn)
  "Helper for `gw-next-where*' and `gw-previous-where*'.
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
(defun gw-to-next-where (&optional predicate start args n noerror readp interactivep)
  "Go to first buffer position after point where PREDICATE is true.
PREDICATE must accept a buffer position as its first arg.  You are
prompted for PREDICATE if you use a prefix arg.  Otherwise, PREDICATE
is the value of `gw-to-where-last', which is the last predicate used
by the command.

Return nil if there is no such position.
Otherwise, return the found position in a cons (POSITION . VALUE),
 where VALUE is what PREDICATE returns.

Non-interactively:
Go to Nth buffer position after START where PREDICATE is true.
Non-nil NOERROR means do not raise an error when there is no such
next position.  See `gw-next-where' for the other arguments."
  (interactive "i\ni\ni\ni\ni\nP\np")
  (gw--to-next/prev-where 'next predicate start args n noerror readp interactivep))

;;;###autoload
(defun gw-to-previous-where (&optional predicate start args n noerror readp interactivep)
  "Go to first buffer position before point where PREDICATE is true.
Same as `gw-to-next-where' except this moves backward."
  (interactive "i\ni\ni\ni\ni\nP")
  (gw--to-next/prev-where 'previous predicate start args n noerror readp interactivep))

(defun gw--to-next/prev-where (&optional next/prev predicate start args n noerror readp interactivep
                                 forward-fn backward-fn)
  "Helper for `gw-to-next-where' and `gw-to-previous-where'.
FORWARD-FN and BACKWARD-FN are functions for moving forward and
backward, respectively, by one unit (defaults: `forward-char',
`backward-char').  (Only one of them is used, depending on NEXT/PREV.)"
  (when readp     (setq gw-to-where-last  nil))
  (when predicate (setq gw-to-where-last  predicate))
  ;; $$ An alternative - using this means that it reads whenever not repeated (or C-u).
  ;;   (when (or (not (eq this-command last-command))  readp)
  ;;     (setq gw-to-where-last nil))
  (unless (or predicate  (and gw-to-where-last  (eq this-command last-command)))
    (while (not (functionp gw-to-where-last))
      (setq gw-to-where-last  (read (let (this-command) (read-string "Predicate: ")))))
    (when (fboundp 'func-arity)         ; Emacs 26+
      (let ((arity  (if (subrp gw-to-where-last)
                        (subr-arity gw-to-where-last)
                      (func-arity gw-to-where-last))))
        (while (or (not (>= (car arity) 1))
                   (and interactivep  (not (= (car arity) 1)))) ; Cannot know how to read ARGS.
          (setq gw-to-where-last  (read (let (this-command)
                                          (read-string "Predicate (at least 1 arg): ")))
                arity                            (if (subrp gw-to-where-last)
                                                     (subr-arity gw-to-where-last)
                                                   (func-arity gw-to-where-last)))))))
  (let ((res  (gw--next/prev-where next/prev gw-to-where-last start args n forward-fn backward-fn)))
    (if (not res)
        (and noerror  (error "No such position"))
      (goto-char (car res))
      res)))

(defun gw-word-char-after-p (pos)
  "Return non-nil if next char is a word constituent."
  (equal '(2) (syntax-after pos)))

(defun gw-not-word-char-after-p (pos)
  "Return non-nil if next char is not a word constituent."
  (not (equal '(2) (syntax-after pos))))

(defun gw-word-char-before-p (pos)
  "Return non-nil if previous char is a word constituent."
  (equal '(2) (syntax-after (1- pos))))

(defun gw-not-word-char-before-p (pos)
  "Return non-nil if previous char is not a word constituent."
  (not (equal '(2) (syntax-after (1- pos)))))

;; (defun gw-to-next-word (pos &optional n)
;;   "Same behavior as `forward-word'."
;;   (interactive "i\np")
;;   (dotimes (ii  n)
;;     (if (and (not (eobp))  (gw-word-char-after-p (point)))
;;         (gw-to-next-where #'gw-not-word-char-after-p)
;;       (gw-to-next-where #'gw-word-char-after-p)
;;       (gw-to-next-where #'gw-not-word-char-after-p))))

;; (defun gw-to-previous-word (pos &optional n)
;;   "Same behavior as `backward-word'."
;;   (interactive "i\np")
;;   (dotimes (ii  n)
;;     (if (and (not (bobp))  (gw-word-char-before-p (point)))
;;         (gw-to-previous-where #'gw-not-word-char-before-p)
;;       (gw-to-previous-where #'gw-word-char-before-p)
;;       (gw-to-previous-where #'gw-not-word-char-before-p))))


;;; Vertical movement ------------------------------------------------

(when (fboundp 'line-move-visual)       ; Emacs 25+

  (defun gw-to-column-down (&optional column n noerror force)
    "Move to COLUMN.  If already there or N > 1 then move down N lines.
The line movement uses `line-move-visual'.
COLUMN defaults to the current column.
N defaults to 1.

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

  (defun gw-to-column-up (&optional column n noerror force)
    "Same as `gw-to-column-down', except move up, not down."
    (interactive "i\np")
    (let ((opoint  (point)))
      (setq column  (or column  (current-column))
            n       (or n  1))
      (move-to-column column force)
      (when (or (= opoint (point))  (> n 1)) (line-move-visual (- n) noerror))))

  (defun gw-next-where-vertical (predicate &optional start args n noerror force)
    "Like `gw-next-where', but look down instead of forward (right).
Optional args NOERROR and FORCE are as for `gw-to-column-down'."
    (setq n  (or n  1))
    (let* ((col   (current-column))
           (pred  (lambda (pos &rest args) (and (= (current-column) col)  (apply predicate pos args)))))
      (gw--next/prev-where 'next pred start args n
                           `(lambda () (gw-to-column-down ,col ,n ,noerror ,force)))))

  (defun gw-previous-where-vertical (predicate &optional start args n noerror force)
    "Like `gw-next-where-vertical', but look up instead of down."
    (setq n  (or n  1))
    (let* ((col   (current-column))
           (pred  (lambda (pos &rest args) (and (= (current-column) col)  (apply predicate pos args)))))
      (gw--next/prev-where 'previous pred start args n
                           nil `(lambda () (gw-to-column-up ,col ,n ,noerror ,force)))))

  (defun gw-to-next-where-vertical (&optional predicate start args n noerror force readp interactivep)
    "Like `gw-to-next-where', but move down instead of forward (across).
Optional args NOERROR and FORCE are as for `gw-to-column-down'."
    (interactive "i\ni\ni\ni\ni\nP\np")
    (setq n  (or n  1))
    (let* ((col   (current-column))
           (pred  (lambda (pos &rest args) (and (= (current-column) col)  (apply predicate pos args)))))
      (gw--to-next/prev-where 'next pred start args n noerror readp interactivep
                              `(lambda () (gw-to-column-down ,col ,n ,noerror ,force)))))

  (defun gw-to-previous-where-vertical (&optional predicate start args n noerror force readp interactivep)
    "Like `gw-to-previous-where', but move up instead of backward (across)."
    (interactive "i\ni\ni\ni\ni\nP\np")
    (setq n  (or n  1))
    (let* ((col   (current-column))
           (pred  (lambda (pos &rest args) (and (= (current-column) col)  (apply predicate pos args)))))
      (gw--to-next/prev-where 'previous pred start args n noerror readp interactivep
                              nil `(lambda () (gw-to-column-up ,col ,n ,noerror ,force)))))

  ;; Could be called `gw-next-word-vertical', but it moves _after_ the word, like `forward-word'.
  (defun gw-downward-word (_pos &optional n)
    "Like `forward-word', but move down, not across."
    (interactive "i\np")
    (setq n  (or n  1))
    (dotimes (_i  n)
      (if (or (eobp)  (gw-word-char-after-p (point)))
          (gw-to-next-where-vertical #'gw-not-word-char-after-p)
        (let ((pos  (point)))
          (condition-case err
              (progn (gw-to-next-where-vertical #'gw-word-char-after-p)
                     (gw-to-next-where-vertical #'gw-not-word-char-after-p))
            (error (progn (goto-char pos) (error "%s" (error-message-string err)))))))))

  ;; Could be called `gw-previous-word-vertical', but it moves _before_ the word, like `backward-word'.
  (defun gw-upward-word (_pos &optional n)
    "Like `backward-word', but move up, not across."
    (interactive "i\np")
    (setq n  (or n  1))
    (dotimes (_i  n)
      (if (or (bobp)  (gw-word-char-after-p (point)))
          (gw-to-previous-where-vertical #'gw-not-word-char-after-p)
        (let ((pos  (point)))
          (condition-case err
              (progn (gw-to-previous-where-vertical #'gw-word-char-after-p)
                     (gw-to-previous-where-vertical #'gw-not-word-char-after-p))
            (error (progn (goto-char pos) (error "%s" (error-message-string err)))))))))

  )

;;; THING movement ---------------------------------------------------

(defvar gw-to-thing-last nil
  "Last thing used by `gw-to-next-thing' or `gw-to-previous-thing'.")

(defun gw-next-thing (thing &optional start n)
  "Find the Nth buffer position after START that is the start of a THING.
Return nil if there is no such position.
Otherwise, return the found position in a cons (POSITION . VALUE),
 where VALUE is what `gw-thing-start-p' returns.
xc
START defaults to point.
N defaults to 1."
  (gw--next/prev-thing 'next thing start n))

(defun gw-previous-thing (thing &optional start n)
  "Find the Nth buffer position before START that is the start of a THING.
Same as `gw-next-thing' except this searches backward."
  (gw--next/prev-thing 'previous thing start n))

(defun gw--next/prev-thing (next/prev thing start n)
  "Helper for `gw-next-thing' and `gw-previous-thing'."
  (gw--next/prev-where next/prev #'gw-thing-start-p start (list thing) n))

(defun gw-thing-start-p (position thing)
  "Return true if POSITION is at the start of a THING, otherwise nil.
A true value means also that (1- POSITION) is not on the same THING,
or else point is at the beginning of the buffer.

The true value returned is a cons (THE-THING . END), where THE-THING is
the THING that starts at POSITION, and END is the buffer position of its end.
THE-THING."
  (let ((bounds  (save-excursion (goto-char position) (tap-bounds-of-thing-at-point thing))))
    (and bounds
         (= position (car bounds))
         (or (bobp)  (not (equal bounds (save-excursion (goto-char (1- position))
                                                        (tap-bounds-of-thing-at-point thing)))))
         (cons (buffer-substring (car bounds) (cdr bounds)) (cdr bounds)))))

;;;###autoload
(defun gw-to-next-thing (&optional thing start n noerror readp)
  "Go to first buffer position after point that is the start of a THING.
Non-interactively:
Go to Nth buffer position after START that is the start of a THING.
Non-nil NOERROR means do not raise an error when there is no such
next position.

Return what `gw-thing-start-p' returns:
* nil if there is no such position.
* a cons (START THE-THING . END), where THE-THING is the THING, and
  START and END are its buffer-position bounds."
  (interactive "i\ni\ni\ni\nP")
  (gw--to-next/prev-thing 'next thing start n noerror readp))

;;;###autoload
(defun gw-to-previous-thing (&optional thing start n noerror readp)
  "Go to first buffer position before point that is the start of a THING.
Same as `gw-to-next-thing', except this moves backward."
  (interactive "i\ni\ni\ni\nP")
  (gw--to-next/prev-thing 'previous thing start n noerror readp))

(defun gw--to-next/prev-thing (next/prev thing start n noerror readp)
  "Helper for `gw-to-next-thing' and `gw-to-previous-thing'."
  (when readp (setq gw-to-thing-last  nil))
  (when thing (setq gw-to-thing-last  thing))
  (unless (or thing  (and gw-to-thing-last  (eq this-command last-command)))
    (while (not gw-to-thing-last)
      (setq gw-to-thing-last  (read (let (this-command) (read-string "Thing: "))))))
  (let ((res  (gw--next/prev-thing next/prev gw-to-thing-last start n)))
    (if (not res)
        (and noerror  (error "No such position"))
      (goto-char (car res))
      res)))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'gowhere)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gowhere.el ends here

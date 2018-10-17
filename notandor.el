;;; notandor.el --- Transform Boolean sexps using De Morgan's laws. -*- lexical-binding:t -*-
;;
;; Filename: notandor.el
;; Description: Transform Boolean Lisp sexps using De Morgan's laws.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2018, Drew Adams, all rights reserved.
;; Created: Mon Apr 30 12:26:00 2018 (-0700)
;; Version: 0
;; Package-Requires: ((thingatpt+ "0"))
;; Last-Updated: Tue Oct 16 18:56:04 2018 (-0700)
;;           By: dradams
;;     Update #: 265
;; URL: https://www.emacswiki.org/emacs/download/notandor.el
;; Doc URL: https://emacswiki.org/emacs/NotAndOr
;; Keywords: lisp logic conditional
;; Compatibility: GNU Emacs: 24.x, 25.x, 26.x (`pcase')
;;
;; Features that might be required by this library:
;;
;;   `cl-lib', `cl-seq', `macroexp', `pp', `pp+', `thingatpt',
;;   `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Transform Boolean Lisp sexps using De Morgan's laws.
;;
;;  The logic of Emacs-Lisp code that involves special forms `not',
;;  `and', and `or' can sometimes be a bit complex.  You might find
;;  yourself manually simplifying such code to make it more readable,
;;  while maintaining the meaning and behavior, i.e., expressing the
;;  same control flow in a way that you find easier to understand.
;;  Subsequent changes to the logic might lead you to resimplify.
;;
;;  Such manual simplification can be error prone.  The commands
;;  defined here can help by performing such equivalence-preserving
;;  transformations.
;;
;;  FWIW, in my own code I generally follow these conventions, to help
;;  (human) readers:
;;
;;  * Use `when' or `unless' only when the return value is not
;;    important, i.e., when the code is for side-effect only.
;;
;;  * When the return value is important (used), use a combination of
;;    `and', `or', and `not' instead of an `if' with a branch of just
;;    `t' or `nil' or an `if' with no second branch.
;;
;;    That is, use `if' only with two branches, neither of which is
;;    just `t' or `nil'.
;;
;;    For example, I use `(and X Y)' instead of `(if X Y)', and `(or
;;    (not X) Y)' instead of `(if X Y t)'.
;;
;;    (Sometimes an explicit `t' or `nil' in an `if' can be helpful
;;    (e.g., to indicate the return value of a function where the `if'
;;    is invoked), but usually it is noisier and less clear, I think.)
;;
;;  * Avoid using `if' with a first branch that uses `progn'.  Either
;;    negate the test and swap the branches or use `cond' instead.
;;
;;    For example, I tend to use `(if (not X) Y A B C)' instead of
;;    `(if X (progn A B C) Y)', and `(cond (X A B C) (t D E F G))'
;;    instead of `(if X (progn A B C) D E F G)'.
;;
;;  Adhering to such convention entails changing the form of
;;  conditional code when program logic changes, even though the edits
;;  might be mainly for reasons of readability.  That means some extra
;;  maintenance work for me, but hopefully for the benefit of readers
;;  of the code (including me).
;;
;;  The commands here can make some such Boolean transformations a bit
;;  easier.  They provide the following transformation rules:
;;
;;  1. (not (and A B)) transforms to      (or  (not A) (not B))
;;  2.      (and A B)  transforms to (not (or  (not A) (not B)))
;;
;;  3. (not (or  A B)) transforms to      (and (not A) (not B))
;;  4.      (or  A B)  transforms to (not (and (not A) (not B)))
;;
;;  5. (not (not A))   transforms to           A
;;  6.           A     transforms to (not (not A))
;;
;;  Rule #6 is used only at the top level, so as not to inject `(not
;;  (not...))' into subexpressions.  If you want that rule then you
;;  must ask for it explicitly for a given occurrence of the code,
;;  `A', to wrap as `(not (not A))'.
;;
;;  Which of the rules gets used depends on where you place the
;;  cursor.  For example, if it is at the beginning of an `or' sexp,
;;  `(or A B)', then rule #4 is used, and the result is `(not (and
;;  (not A) (not B)))'.  If on a `not' sexp of the form `(not (and
;;  (not A) (not B)))' then it is transformed to `(or A B)' - the
;;  opposite direction.
;;
;;  So if you transform twice at the same location you get back what
;;  you started with: the tranformation operation is its own inverse.
;;  (You can also use undo, of course.)
;;
;;  If you want to use a transformation that expects outermost `not'
;;  where there is none then you can first use a transformation that
;;  produces an outermost `not'.  It's up to you where to put the
;;  cursor before invoking a transformation command.
;;
;;  Two commands that transform a Boolean sexp are defined here:
;;
;;  * Command `notandor-at-point' replaces the sexp at point by its
;;    transformation.
;;
;;  * Command `notandor-show' just shows you (in a tooltip, a message,
;;    or another buffer) what the result of transforming the sexp at
;;    point would be - it does not alter your code.  It prompts you
;;    for the sexp to transform, the default being the sexp at point.
;;
;;  Both commands return the new sexp, the result of transformation.
;;
;;  If you also use library `pp+.el' then these commands give you
;;  additional behavior with a prefix argument.
;;
;;  This library requires library `thingatpt+.el', which uses and
;;  enhances standard library `thingatpt.el'.
;;
;;
;;  Commands defined here:
;;
;;    `notandor-at-point',  `notandor-show'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2018/05/03 dadams
;;     notandor-(at-pointHandle|show): Handle single-quoted sexps and strings.
;; 2018/05/01 dadams
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

(require 'thingatpt+) ; tap-sexp-at-point-with-bounds
(require 'cl-seq)     ; cl-remove-duplicates

(require 'pp+ nil t) ;; (no error if not found): pp-eval-expression

;;;;;;;;;;;;;;;;;;;;;;;

(defun notandor-trans (sexp &optional recursivep)
  "Transform Boolean SEXP to an equivalent one.
Optional arg RECURSIVEP is non-nil for recursive calls."
  (pcase sexp
    (`(not (and ,a ,b))                 `(or  ,(notandor-trans `(not ,a) 'RECURSE)
                                              ,(notandor-trans `(not ,b) 'RECURSE)))
    (`(not (or  ,a ,b))                 `(and ,(notandor-trans `(not ,a) 'RECURSE)
                                              ,(notandor-trans `(not ,b) 'RECURSE)))
    (`(or  ,a ,b)                       `(not (and ,(notandor-trans `(not ,a) 'RECURSE)
                                                   ,(notandor-trans `(not ,b) 'RECURSE))))
    (`(and ,a ,b)                       `(not (or ,(notandor-trans `(not ,a) 'RECURSE)
                                                  ,(notandor-trans `(not ,b) 'RECURSE))))
    (`(not (not ,a))                    a)
    (`(not ,a)                          `(not ,a))
    ((and a  (guard (not recursivep)))  `(not (not ,a)))))

;;;###autoload
(defun notandor-at-point (&optional show-only-p swap-tooltip)
  "Replace the Boolean sexp at point with an equivalent one.
There are two special cases when determining the sexp at point:

* If point is on a sexp that is single-quoted then the single-quote
  mark is ignored.  If you want to transform the quoted sexp then put
  point on the single-quote mark itself.

* If point is on the double-quote char at the start of a string then
  that string is the sexp to be transformed.  Otherwise, the
  double-quoting (string) is ignored when obtaining the sexp to be
  transformed.

  Put differently, if point is on the start of a string then the whole
  string is the object of transformation.  If point is inside a string
  then the transformation takes place inside the string (on the sexp
  at point).

Also show the new sexp separately, using `pp-eval-expression'.

If you use library `pp+.el' then option `pp-max-tooltip-size', and
optionally a prefix arg, control the display by `pp-eval-expression'.
The option controls whether to use a tooltip and its size.

With a prefix arg, do not replace the sexp, just show the new one.

With a zero prefix arg, swap the use of a tooltip according to
`pp-max-tooltip-size': use a tooltip only if the option is nil."
  (interactive (list current-prefix-arg (zerop (prefix-numeric-value current-prefix-arg))))
  (let* ((sexp+bnds  (tap-sexp-at-point-with-bounds))
         (on-dbl-q   (looking-at-p "\"")) ; Use whole string as sexp.
         ;; Next 3 bindings remove single-quote, i.e., `(quote...)' wrapper, unless point
         ;; is directly on the quote mark.  A simpler version would just use the sexp from
         ;; `tap-sexp-at-point-with-bounds' directly.
         (on-quote   (or (looking-at-p "'")  (looking-at-p "([ \t\n]*quote\\>")))
         (sexp       (and sexp+bnds  (car sexp+bnds)))
         (sexp       (and sexp  (pcase sexp
                                  ((and `(quote ,a)  (guard (not on-quote)))  a)
                                  (_ sexp))))
         (beg        (and sexp+bnds  (cadr sexp+bnds)))
         (end        (and sexp+bnds  (cddr sexp+bnds)))
         new-sexp)
    (unless sexp (error "No Boolean sexp at point"))
    (setq new-sexp  (notandor-trans sexp))
    (if (equal new-sexp sexp)
        (message "No change")
      (unless show-only-p
        (save-excursion (goto-char beg)
                        (delete-region beg end)
                        (let ((print-quoted  t))
                          (if on-dbl-q
                              (prin1 new-sexp (current-buffer))
                            (princ new-sexp (current-buffer))))))
      (pp-eval-expression `',new-sexp nil swap-tooltip))
    new-sexp))  ; Return it.

;;;###autoload
(defun notandor-show (sexp &optional insert-value swap-tooltip)
  "Show an equivalent Boolean expression to SEXP.
You are prompted for SEXP, which defaults to the sexp at point.

There are two special cases when determining the sexp at point:

* If point is on a sexp that is single-quoted then the single-quote
  mark is ignored.  If you want the quoted sexp as the default then
  put point on the single-quote mark itself.

* If point is on the double-quote char at the start of a string then
  that string is the sexp.  Otherwise, the double-quoting (string) is
  ignored when obtaining the default sexp.

The new, equivalent sexp is returned, and it is displayed using
`pp-eval-expression'.

If you use library `pp+.el' then option `pp-max-tooltip-size', and
optionally a prefix arg, control the display by `pp-eval-expression'.
The option controls whether to use a tooltip and its size.

* With no prefix arg, respect `pp-max-tooltip-size'.  If a tooltip is
  not used then if the value fits on one line (frame width) show it in
  the echo area.  Otherwise, show it in buffer `*Pp Eval Output*'.

* With a zero prefix arg, swap the use of a tooltip according to
  `pp-max-tooltip-size': use a tooltip only if the option is nil.

* With a non-zero prefix arg, insert the value into the current buffer
  at point.  If the prefix arg is negative and the value is a string
  then insert it into the buffer without double-quotes (`\"')."
  (interactive
   (let* ((sexp+bnds  (tap-sexp-at-point-with-bounds))
          (on-dbl-q   (looking-at-p "\"")) ; Use string as sexp.
          ;; Next 3 bindings remove single-quote, i.e., `(quote...)' wrapper, unless point
          ;; is directly on the quote mark.  A simpler version would just use the sexp from
          ;; `tap-sexp-at-point-with-bounds' directly.
          (on-quote   (or (looking-at-p "'")  (looking-at-p "([ \t\n]*quote\\>")))
          (sexp       (and sexp+bnds  (car sexp+bnds)))
          (sexp       (and sexp  (pcase sexp
                                   ((and `(quote ,a)  (guard (not on-quote)))  a)
                                   (_ sexp))))
          (default    (and sexp    (format (if on-dbl-q "%S" "%s") sexp)))
          (cands      (mapcar #'list (cl-remove-duplicates read-expression-history)))
          (sxp        (car (read-from-string
                            (completing-read "Sexp: " cands nil nil nil
                                             'read-expression-history default)))))
     (list sxp  current-prefix-arg  (zerop (prefix-numeric-value current-prefix-arg)))))
  (let ((new-sexp  (notandor-trans sexp)))
    (if (equal new-sexp sexp)
        (message "No change")
      (if (boundp 'pp-max-tooltip-size) ; `pp+.el' is loaded.
          (pp-eval-expression `',new-sexp insert-value swap-tooltip)
        (pp-eval-expression `',new-sexp)))
    new-sexp))                          ; Return it.


;; Some tests:
;;
;; (notandor-show '(or x y))      ; (notandor-show '(not (and (not x) (not y))))
;; (notandor-show '(and x y))     ; (notandor-show '(not (or (not x) (not y))))
;; (notandor-show '(not (not x))) ; (notandor-show 'x)
;; (notandor-show '(foo bar))     ; (notandor-show '(not (not (foo bar))))
;; (notandor-show 'x)             ; (notandor-show '(not (not x)))
;;
;; (notandor-show '(not (and b c)))
;; (notandor-show '(or (not b) (not c)))
;; (notandor-show '(not (and (not (not b)) (not (not c)))))
;;
;; (notandor-show '(and a (or (not b) (not c))))
;; (notandor-show '(not (or (not a) (and b c))))
;; (notandor-show '(not (or (not a) (not (or (not b) (not c))))))
;; 
;; (notandor-show '(not a))
;; (notandor-show '(not (not (not a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'notandor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; notandor.el ends here

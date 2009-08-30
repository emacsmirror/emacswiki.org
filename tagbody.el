;;; tagbody.el --- Common Lisp `tagbody' for Emacs Lisp

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Emacs Lisp Archive Entry
;; Filename: tagbody.el
;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>
;; Version: 1.0
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file defines the equivalent of the Common Lisp `tagbody'
;; special form for Emacs Lisp.

;;; Code:

(require 'cl)

(defmacro tagbody (&rest expressions)
  "Evaluate EXPRESSIONS and return nil, allowing tags and `go's.
Any bare \(unquoted) symbols and integers in EXPRESSIONS are not
evaluated and instead define tags which can be the target of `go'
expressions, which can appear anywhere in the `tagbody' and cause
control to be transfered to the statement immediately following the
corresponding tag.

It is very rare that you would want to use `tagbody', since it is
essentially a goto statement, something conspicuously absent from
Lisp.  In Common Lisp, `tagbody' is mainly used to define other
control expressions, most of which are already provided by Emacs Lisp
or the CL package."
  ;; We implement `tagbody' by splitting EXPRESSIONS at each tag,
  ;; making the resulting tagless blocks of code into a list of lambda
  ;; functions which are normally executed sequentially.  To `go' to a
  ;; tag, we look up the tag in `taglist' to find the associated block
  ;; and restart execution at that point.  In order to stop execution
  ;; when a `go' is encountered, we use `catch' and `throw'.
  (let ((taglist (gensym))      ; alist of (tag . block-number)
        (blocks (gensym))       ; list of lambda functions
        (current (gensym))
        the-taglist the-blocks) ; future values of the gensyms
    (loop for counter from 1
          for rest = (member-if
                      #'(lambda (x) (or (symbolp x) (integerp x)))
                      expressions)
          while rest
          do (setq the-taglist (cons (cons (car rest) counter)
                                     the-taglist)
                   ;; Make a block from all the expressions between
                   ;; the last tag and this one.  Order matters.
                   the-blocks (nconc the-blocks
                                     `((lambda ()
                                         ,@(ldiff expressions rest))))
                   ;; Skip the tag itself
                   expressions (cdr rest))
          ;; Include the block following the last tag
          finally (setq the-blocks (nconc the-blocks
                                          `((lambda ()
                                              ,@expressions)))))
    `(let ((,taglist ',the-taglist)
           ;; Can't quote the list of lambdas because then they can't
           ;; become closures.  `lexical-let' skips quoted data.
           (,blocks (list ,@the-blocks))
           ;; The number of the block at which execution is going to
           ;; start next time.
           (,current 0))
       ;; Repeat until execution falls off the end.
       (loop
        (setq ,current
              ;; Catch the tag to which to transfer
              ;; execution and find its associated block.
              (catch 'tagbody-go
                (mapc #'funcall
                      (nthcdr ,current ,blocks))
                ;; If we completed execution without a throw, return
                ;; nil from `tagbody'.
                (return)))
        ;; At this point, `go' has happened and `current' is the tag.
        (setq ,current
              ;; Get the block number associated with this tag.
              (cdr (or (assoc ,current ,taglist)
                       ;; If tag not found, defer to a possible outer
                       ;; containing `tagbody', or signal an error if
                       ;; none found.
                       (throw 'tagbody-go ,current))))))))

(defmacro go (tag)
  "Jump to a tag inside a `tagbody'.
Causes execution to jump to the statement immediately following TAG in
a current `tagbody' form, searching outwards and signalling an error
if no such tag is found.  TAG is not evaluated and should be a symbol
or an integer.  See `tagbody'."
  (assert (or (symbolp tag) (integerp tag)) nil
          "Invalid tag in `go': symbols and integers only")
  `(throw 'tagbody-go ',tag))

(defun go* (tag)
  "Jump to a tag inside a `tagbody'.
Causes execution to jump to the statement immediately following TAG in
a current `tagbody' form, searching outwards and signalling an error
if no such tag is found.  TAG should be a symbol or an integer."
  (assert (or (symbolp tag) (numberp tag)) nil
          "Invalid tag in `go*': symbols and integers only")
  (throw 'tagbody-go tag))

(provide 'tagbody)

;;; tagbody.el ends here

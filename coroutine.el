;;; coroutine.el --- Coroutines for Emacs Lisp

;;;; used to be at http://www.ugcs.caltech.edu/~shulman/pub/Main/Software/coroutine.el

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Emacs Lisp Archive Entry
;; Filename: coroutine.el
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

;; This file allows the definition of coroutines in Emacs Lisp.  A
;; coroutine is a function that can return ("yield") a value and still
;; retain its execution state so that at the next call it can pick up
;; where it left off.

;; Coroutines are very difficult to implement without the aid of
;; first-class continuations or a similar control feature, and this
;; imposes certain limits: for example, `yield' is only valid at
;; top-level in the coroutine, or inside a specially tweaked macro.

;; This file requires `tagbody.el' which defines the Common Lisp
;; `tagbody' form for Emacs Lisp.

;;; Code:

(require 'cl)
(require 'tagbody)

;; Copied from the code for `destructuring-bind' in `cl-macs.el' and
;; modified to set rather than bind.
(defmacro destructuring-setq (args expr)
  "Same as `destructuring-bind', but sets instead of binding."
  (let ((bind-lets nil) (bind-forms nil) (bind-inits nil)
        (bind-defs nil) (bind-block 'cl-none))
    (cl-do-arglist (or args '(&aux)) expr)
    `(progn
       ,@bind-inits
       ,@(mapcar #'(lambda (pair) (cons 'setq pair))
                 (nreverse bind-lets))
       ,@(nreverse bind-forms))))

(defmacro define-coroutine (name arglist varlist &rest body)
  "Define NAME as a coroutine.
The coroutine takes arguments ARGLIST and has local variables VARLIST,
and its BODY is contained in an implicit `tagbody'.  Arguments of an
inital invocation are passed to the BODY and parsed according to
ARGLIST, while the argument list of a later invocation are placed in
the variable `return-value' after execution is resumed.  The values of
variables in VARLIST are saved between invocations, and those not also
appearing in ARGLIST are automatically bound with `let' in the entire
coroutine.  All variables in ARGLIST are added to VARLIST if not
already present.  All local variables used by the coroutine should be
in one of VARLIST or ARGLIST.  ARGLIST allows full Common Lisp
conventions as provided by the CL package.

Intermediate values should be returned with the `yield' form, which
normally must appear at top-level in BODY.  In order to control
execution around it, the fact that BODY is enclosed in an implicit
`tagbody' may be used.  For convenience, `if', `while', and `progn'
forms at top-level in BODY are expanded into equivalent constructions
using tags, so that `yield' may appear \(at top-level) in their
bodies.  This expansion is done recursively, so such calls may be
nested.  No other macros are expanded in this way, so forms such as
\(when test \(yield value)) are invalid.

Note that because of the implicit `tagbody' \(which is necessary to
implement coroutines), bare symbols and integers are interpreted as
tags rather than evaluted.  This only matters when a symbol or integer
is the last form in BODY: in this case, the coroutine returns nil.  To
return a bare integer or the value of a variable from a coroutine,
surround it with a call to `identity'.

Coroutines can call themselves recursively, with a few limitations.
The call must be made at top-level, or inside a specially expanded
macro as described above.  The return value is placed in the variable
`return-value', or returned from the caller if the recursive call is
the last element in BODY.  Variables in VARLIST \(only) are
initialized to nil, and variables in ARGLIST are initialized from the
arguments to the recursive call.  Both are saved separately for each
recursive call."
  ;; Put all variables from ARGLIST into VARLIST as well
  (labels ((flatten (obj) (if (listp obj) (mapcar #'flatten obj) obj))
           (is&symbol (sym) (eq (aref (symbol-name sym) 0) ?&)))
    (setq varlist
          (remove-duplicates
           (nconc (remove-if #'is&symbol (flatten arglist))
                  varlist))))
  ;; Not all identifiers need to be hygienicized, only those whose
  ;; scope overlaps that of BODY and for which capture is undesired.
  (let ((entry-point (gensym))
        (start (gensym))
        (all-args (gensym))
        (recursion-stack (gensym)))
    ;; Simply defining `yield' as a macro is no good, since it won't
    ;; be able to put tags at top-level in the `tagbody'.  We have to
    ;; macroexpand each call manually and splice the values together.
    ;; Macros in this environment should return a pure list of forms,
    ;; without `progn' or other block construct.
    (let ((env (list
                (cons 'yield
                      #'(lambda (value)
                          (let ((tag (gensym)))
                            `((setq ,entry-point
                                    (list ',tag ,@varlist))
                              (return-from ,name ,value)
                              ,tag
                              ;; In case this is the last form
                              (identity return-value)))))
                (cons 'if
                      #'(lambda (condition then &rest elses)
                          (let ((else-tag (gensym))
                                (done-tag (gensym)))
                            `((unless ,condition (go ,else-tag))
                              ,then
                              (go ,done-tag)
                              ,else-tag
                              ,@elses
                              ,done-tag))))
                (cons 'while
                      #'(lambda (condition &rest body)
                          (let ((start-tag (gensym))
                                (done-tag (gensym)))
                            `(,start-tag
                              (unless ,condition (go ,done-tag))
                              ,@body
                              (go ,start-tag)
                              ,done-tag))))
                (cons 'progn
                      #'(lambda (&rest body)
                          (copy-list body)))
                (cons name
                      #'(lambda (&rest args)
                          (let ((return-tag (gensym)))
                            `((push (list ',return-tag ,@varlist)
                                    ,recursion-stack)
                              (let ((args (list ,@args)))
                                (multiple-value-setq ,varlist '())
                                (destructuring-setq ,arglist args))
                              (go ,start)
                              ,return-tag
                              ;; In case this is the last form
                              (identity return-value)))))
                )))
      (loop for formlist on body
            with newforms
            do (when (and (consp (car formlist))
                          (memq (caar formlist) (mapcar #'car env)))
                 ;; Expand the macro
                 (setq newforms (macroexpand (car formlist) env))
                 ;; Splice in the returned values
                 (setcar formlist (car newforms))
                 (setcdr formlist
                         (nconc (cdr newforms) (cdr formlist)))
                 ;; Fool the list into parsing the new forms as well.
                 (setq formlist (cons nil formlist)))))
    ;; Hack to return the value of the last form correctly.  See the
    ;; docstring for the intended behavior.
    (when (or (symbolp (car (last body)))
              (integerp (car (last body))))
      (setq body (append body '(nil))))
    ;; Now actually make the definition.
    `(lexical-let ((,entry-point nil)
                   (,recursion-stack '()))
       ;; `defun*' surrounds its body in a block named NAME.
       (defun* ,name (&rest ,all-args)
         (let (return-value ,@varlist)
           (destructuring-setq ,arglist ,all-args)
           (tagbody
            (when ,entry-point
              ;; Continue a previous invocation
              (multiple-value-setq ,varlist (cdr ,entry-point))
              (setq return-value ,all-args)
              (go* (prog1 (car ,entry-point)
                     (setq ,entry-point nil))))
            ,start
            ,@(butlast body)
            ;; Now return the value of the last form.
            (if ,recursion-stack
                ;; "Return" to a recursive "call"
                (let ((return-point (pop ,recursion-stack)))
                  (multiple-value-setq ,varlist (cdr return-point))
                  (setq return-value ,(car (last body)))
                  (go* (car return-point)))
              ;; Final return to top-level
              (return-from ,name ,(car (last body))))))))))

(defmacro yield (value)
  "Inside a coroutine, yield VALUE and maintain state.
The next call to the coroutine will resume execution from after the
`yield' form.  A `yield' form must appear at top-level inside the
coroutine, but execution can be controlled via `tagbody' constructs.
See `define-coroutine' for more information on coroutines."
  ;; We sneakily define the real `yield' by manually expanding macros
  ;; when a coroutine is defined.
  (error "Yield is only valid at top-level in a coroutine"))

(provide 'coroutine)

;;; coroutine.el ends here

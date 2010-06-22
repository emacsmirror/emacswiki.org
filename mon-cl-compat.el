;;; mon-cl-compat.el --- cl functions prefixed as cl::some-cl-function
;; -*- mode: EMACS-LISP; byte-compile-dynamic: t; -*-

;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-cl-compat.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-01-17T02:30:54-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, development, extensions, 

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-cl-compat.el provides Common Lisp functions from cl-*.el files.
;; Emacs signals compile-time warnings if these functions are called at runtime
;; from the CL package (as distinguished from macros and aliases).  We
;; reproduce the entirety of that package here with all relevant `cl-symbol's
;; prefixed with `cl::symbol's. This is intended to be used as a drop-in
;; replacement for the cl-seq.el when compiling your package and therefor
;; doesn't necessitate declaration of file local variable constucts using
;; `byte-compile-warnings' e.g.: `byte-compile-warnings': (not cl-functions)
;; Obv. we could silence the byte-compiler with: 
;; (custom-set-default 'byte-compile-warnings (not cl-functions))
;; Or, do like slime.el (let ((byte-compile-warnings '())) ... ,@body)
;;
;; FUNCTIONS:►►►
;; `cl::reduce', `cl::fill', `cl::replace', `cl::remove-duplicates',
;; `cl::remove-if-not', `cl::remove-if', `cl::remove',
;; `cl::cl-delete-duplicates', `cl::delete-duplicates', `cl::delete-if-not',
;; `cl::delete-if', `cl::delete*', `cl::nsubstitute-if-not',
;; `cl::nsubstitute-if', `cl::nsubstitute', `cl::substitute-if-not',
;; `cl::substitute-if', `cl::substitute', `cl::find-if-not', `cl::find-if',
;; `cl::find', `cl::cl-position', `cl::position-if-not', `cl::position-if',
;; `cl::position', `cl::count-if-not', `cl::count-if', `cl::count',
;; `cl::mismatch', `cl::search', `cl::stable-sort', `cl::sort*', `cl::merge',
;; `cl::member-if-not', `cl::member-if', `cl::member*', `cl::cl-member',
;; `cl::cl-adjoin', `cl::rassoc-if-not', `cl::rassoc-if', `cl::rassoc*',
;; `cl::assoc-if-not', `cl::assoc-if', `cl::assoc*', `cl::nunion', `cl::union',
;; `cl::nintersection', `cl::intersection', `cl::nset-difference',
;; `cl::set-difference', `cl::nset-exclusive-or', `cl::set-exclusive-or',
;; `cl::subsetp', `cl::nsubst-if-not', `cl::nsubst-if', `cl::nsubst',
;; `cl::subst-if-not', `cl::subst-if', `cl::cl-nsublis-rec', `cl::nsublis',
;; `cl::cl-sublis-rec', `cl::sublis', `cl::cl-tree-equal-rec',
;; `cl::tree-equal', `cl::subseq', `cl::ldiff', `cl::coerce', `cl::typep',
;; `cl::cl-make-type-test', `cl::floatp-safe', `cl::plusp', `cl::minusp',
;; `cl::oddp', `cl::evenp', `cl::cl-remprop', `cl::cl-do-remf', 
;; `cl::gensym', `cl::gentemp', 
;; `cl::compiler-macroexpand', `cl::cl-byte-compile-compiler-macro'
;; `cl::signum', `cl::rem*', `cl::mod*', `cl::round*', `cl::truncate*',
;; `cl::ceiling*', `cl::floor*', `cl::isqrt', `cl::lcm', `cl::gcd',
;; `cl::maplist'  `cl::cl-arglist-args', `cl::cl-do-arglist'
;; `cl::cl-make-type-test' , `cl::typep'
;;
;; `cl::compiler-macroexpand', `cl::cl-byte-compile-compiler-macro' 
;; `cl::cl-byte-compile-throw', `cl::cl-byte-compile-block',
;; `cl::cl-transform-lambda', `cl::cl-transform-function-property'
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;; `cl-parsing-keywords', `cl-check-key',
;; `cl-check-test-nokey', `cl-check-test',
;; `cl-check-match', `cl::cl-defun-expander', `cl::cl-do-proclaim',
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; FACES:
;;
;; VARIABLES:
;; `cl::alist'
;;
;; :NOTE Following defvar'd inside an `eval-when-compile' cl-seq.el will cover
;;  these at runtime:
;; `cl-test-not', `cl-test', `cl-if-not', `cl-if', `cl-key', 
;;
;; :NOTE Following are not prefixed but are present: 
;; `bind-block', `bind-defs', `bind-enquote',
;;  `bind-inits', `bind-lets', `bind-forms'
;; They are some sort of byte-compiler thunks for `cl-transform-lambda'.
;; They are nulled post compile-time and won't appear in your environment... 
;; Or, at least they shouldn't! :).
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;;
;; TODO:
;; c[ad]r+s
;; Core functions from cl-macs.el we'll have to be be careful with:
;; `cl-loop-let',  `cl-parse-loop-clause', `cl-loop-handle-accum',
;; `cl-loop-build-ands', `cl-expand-do-loop'
;;
;; `cl-setf-make-apply', `get-setf-method', `cl-setf-do-modify',
;; `cl-setf-do-store', `cl-setf-simple-store-p'
;; `cl-do-pop'
;; `cl-struct-setf-expander'
;;
;; :COMPILER-MACROS in cl-macs.el with `define-compiler-macro':
;;  `member*' `assoc*' `adjoin' `list*' `get*' `typep'.
;; :NOTE Also, `eql' but doesn't get byte-compiled...???
;;
;; :INLINED in cl-macs.el 
;; `floatp-safe' `acons' `map' `concatenate' `notany' `notevery' `cl-set-elt'
;; `revappend' `nreconc' `gethash'
;;
;; :SIDE-EFFECT-FREE as proclaimed in cl-macs.el
;; `oddp' `evenp' `signum' `last' `butlast' `ldiff' `pairlis' `gcd' `lcm'
;; `isqrt' `floor*' `ceiling*' `truncate*' `round*' `mod*' `rem*' `subseq'
;; `list-length' `get*' `getf' `first' `second' `third' `fourth' `fifth'
;; `sixth' `seventh' `eighth' `ninth' `tenth' `rest' `endp' `plusp' `minusp'
;; `caaar' `caadr' `cadar' `caddr' `cdaar' `cdadr' `cddar' `cdddr' `caaaar'
;; `caaadr' `caadar' `caaddr' `cadaar' `cadadr' `caddar' `cadddr' `cdaaar'
;; `cdaadr' `cdadar' `cdaddr' `cddaar' `cddadr' `cdddar' `cddddr'
;;
;; :SIDE-EFFECT-AND-ERROR-FREE in cl-macs.el
;; `eql' `floatp-safe' `list*' `subst' `acons' `equalp' `random-state-p'
;; `copy-tree' `sublis'
;;
;; NOTES:
;; The variable `byte-compile-cl-functions' returns the list of functions
;; defined in CL. The goal is to eventually encorporate all of its elts.
;;
;; SNIPPETS:
;;
;; REQUIRES:
;; `mon-cl-compat-regexps.el' (only if present in load-path)
;; `mon-CL-namespace-colonic', `mon-CL-cln-colon-swap'
;;
;; THIRD-PARTY-CODE:
;; This file is a duplicate of cl-*.el symbols renamed with suffix `cl::'. 
;; They were sourced from the following files:
;; :FILE cl-extra.el
;; :FILE cl-seqs.el
;; :FILE cl-macs.el
;; :FILE cl.el
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-cl-compat.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-01-17T23:06:12-05:00Z}#{10027} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-cl-compat. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-01-17T02:30:54-05:00Z}#{10027} - by MON KEY>
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;;; cl-seq.el --- Common Lisp features, part 3
;;; Copyright (C) 1993, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
;;; Free Software Foundation, Inc.
;;; arch-tag: ec1cc072-9006-4225-b6ba-d6b07ed1710c
;;; Author: Dave Gillespie <daveg@synaptics.com>
;;; Version: 2.02
;;; Keywords: extensions
;;; The cl-seq.el file is part of GNU Emacs.
;;; GNU Emacs is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:
;;; These are extensions to Emacs Lisp that provide a degree of
;;; Common Lisp compatibility, beyond what is already built-in
;;; in Emacs Lisp.
;;; This package was written by Dave Gillespie; it is a complete
;;; rewrite of Cesar Quiroz's original cl.el package of December 1986.
;; =================================================================

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2009, 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

(when (locate-library "mon-cl-compat-regexps")
  (require 'mon-cl-compat-regexps))

;; silence byte-compiler warnings about free variables. 
;; cl-seq.el  should take care of these for us.
(eval-when-compile 
  (defvar cl-test)
  (defvar cl-test-not)
  (defvar cl-if)
  (defvar cl-if-not)
  (defvar cl-key))

;;; Assuming that any macros should come first in this file.

;;; ==============================
;;; Following functions from cl-macs.el
;;; Assume putting at Top of file but after the above macros is best...

;;; ==============================
;;; `cl-simple-expr-p' -> cl-macs.el
;;; Check if no side effects, and executes quickly.
(defun cl::cl-simple-expr-p (x &optional size)
  (or size (setq size 10))
  (if (and (consp x) (not (memq (car x) '(quote function function*)))) ;; `function*' <CL-MACRO>
      (and (symbolp (car x))
	   (or (memq (car x) cl-simple-funcs) ;; `cl-simple-funcs' <CL-VARIABLE>
	       (get (car x) 'side-effect-free))
	   (progn
	     (setq size (1- size))
	     (while (and (setq x (cdr x))
			 (setq size (cl::cl-simple-expr-p (car x) size))))
	     (and (null x) (>= size 0) size)))
    (and (> size 0) (1- size))))

;;; ==============================
;;; `cl-simple-exprs-p' -> cl-macs.el
(defun cl::cl-simple-exprs-p (xs)
  (while (and xs (cl::cl-simple-expr-p (car xs)))
    (setq xs (cdr xs)))
  (not xs))

;;; ==============================
;;; `cl-safe-expr-p' -> cl-macs.el
;;; Check if no side effects.
(defun cl::cl-safe-expr-p (x)
  (or (not (and (consp x) (not (memq (car x) '(quote function function*))))) ;; <CL-MACRO>
      (and (symbolp (car x))
	   (or (memq (car x) cl-simple-funcs) ;; <CL-VARIABLE>
	       (memq (car x) cl-safe-funcs)   ;; <CL-VARIABLE>
	       (get (car x) 'side-effect-free))
	   (progn
	     (while (and (setq x (cdr x)) (cl::cl-safe-expr-p (car x))))
	     (null x)))))

;;; ==============================
;;; `cl-const-expr-p' -> cl-macs.el
;;; Check if constant (i.e., no side effects or dependencies).
(defun cl::cl-const-expr-p (x)
  (cond ((consp x)
	 (or (eq (car x) 'quote)
	     (and (memq (car x) '(function function*)) ;; `function*' <MACRO> cl-macs.el
		  (or (symbolp (nth 1 x))
		      (and (eq (car-safe (nth 1 x)) 'lambda) 'func)))))
	((symbolp x) (and (memq x '(nil t)) t))
	(t t)))

;;; ==============================
;;; `cl-const-exprs-p' -> cl-macs.el
(defun cl::cl-const-exprs-p (xs)
  (while (and xs (cl::cl-const-expr-p (car xs)))
    (setq xs (cdr xs)))
  (not xs))

;;; ==============================
;;; `cl-const-expr-val' -> cl-macs.el
(defun cl::cl-const-expr-val (x)
  (and (eq (cl::cl-const-expr-p x) t) (if (consp x) (nth 1 x) x)))

;;; ==============================
;;; `cl-expr-access-order' -> cl-macs.el
(defun cl::cl-expr-access-order (x v)
  (if (cl::cl-const-expr-p x) v
    (if (consp x)
	(progn
	  (while (setq x (cdr x)) (setq v (cl::cl-expr-access-order (car x) v)))
	  v)
      (if (eq x (car v)) (cdr v) '(t)))))
 
;;; ==============================
;;; `cl-expr-contains' -> cl-macs.el
;;; Count number of times X refers to Y.  Return nil for 0 times.
(defun cl::cl-expr-contains (x y)
  (cond ((equal y x) 1)
	((and (consp x) (not (memq (car-safe x) '(quote function function*))));; <CL-MACRO>
	 (let ((sum 0))
	   (while x
	     (setq sum (+ sum (or (cl::cl-expr-contains (pop x) y) 0))))
	   (and (> sum 0) sum)))
	(t nil)))

;;; ==============================
;;; `cl-expr-contains-any' -> cl-macs.el
(defun cl::cl-expr-contains-any (x y)
  (while (and y (not (cl::cl-expr-contains x (car y)))) (pop y))
  y)

;;; ==============================
;;; `cl-expr-depends-p' -> cl-macs.el
;;; Check whether X may depend on any of the symbols in Y.
(defun cl::cl-expr-depends-p (x y)
  (and (not (cl::cl-const-expr-p x))
       (or (not (cl::cl-safe-expr-p x)) (cl::cl-expr-contains-any x y))))


;;; ==============================
;;; :NOTE Doubt we need to duplicate *gensym-counter*.
;; `gensym' -> cl-macs.el
;;;###autoload
(defun cl::gensym (&optional prefix)
  "Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"G\"."
  (let ((pfix (if (stringp prefix) prefix "G"))
	(num (if (integerp prefix) prefix
	       (prog1 *gensym-counter* ;; <CL-VARIABLE>
		 (setq *gensym-counter* (1+ *gensym-counter*)))))) 
    (make-symbol (format "%s%d" pfix num))))

;;; ==============================
;; `gentemp' -> cl-macs.el
;;;###autoload
(defun cl::gentemp (&optional prefix)
  "Generate a new interned symbol with a unique name.
The name is made by appending a number to PREFIX, default \"G\"."
  (let ((pfix (if (stringp prefix) prefix "G"))
	name)
    (while (intern-soft (setq name (format "%s%d" pfix *gensym-counter*))) ;; <CL-VARIABLE>
      (setq *gensym-counter* (1+ *gensym-counter*))) 
    (intern name)))

;;; ==============================
;; `cl-defun-expander' -> cl-macs.el
(defun cl::cl-defun-expander (func &rest rest)
  (list 'progn
	(list 'defalias (list 'quote func)
	      (list 'function (cons 'lambda rest)))
	(list 'quote func)))

;;; ==============================
;; `cl-do-proclaim' cl-macs.el
(defun cl::cl-do-proclaim (spec hist)
  (and hist (listp cl-proclaim-history) (push spec cl-proclaim-history))
  (cond ((eq (car-safe spec) 'special)
	 (if (boundp 'byte-compile-bound-variables)
	     (setq byte-compile-bound-variables
		   (append (cdr spec) byte-compile-bound-variables))))
	((eq (car-safe spec) 'inline)
	 (while (setq spec (cdr spec))
	   (or (memq (get (car spec) 'byte-optimizer)
		     '(nil byte-compile-inline-expand))
	       (error "%s already has a byte-optimizer, can't make it inline"
		      (car spec)))
	   (put (car spec) 'byte-optimizer 'byte-compile-inline-expand)))
	((eq (car-safe spec) 'notinline)
	 (while (setq spec (cdr spec))
	   (if (eq (get (car spec) 'byte-optimizer)
		   'byte-compile-inline-expand)
	       (put (car spec) 'byte-optimizer nil))))
	((eq (car-safe spec) 'optimize)
	 (let ((speed (assq (nth 1 (assq 'speed (cdr spec)))
			    '((0 nil) (1 t) (2 t) (3 t))))
	       (safety (assq (nth 1 (assq 'safety (cdr spec)))
			     '((0 t) (1 t) (2 t) (3 nil)))))
	   (if speed (setq cl-optimize-speed (car speed)
			   byte-optimize (nth 1 speed)))
	   (if safety (setq cl-optimize-safety (car safety)
			    byte-compile-delete-errors (nth 1 safety)))))
	((and (eq (car-safe spec) 'warn) (boundp 'byte-compile-warnings))
	 (while (setq spec (cdr spec))
	   (if (consp (car spec))
	       (if (eq (cadar spec) 0)
                   (byte-compile-disable-warning (caar spec))
                 (byte-compile-enable-warning (caar spec)))))))
  nil)

;;; ==============================
;;; `cl-defsubst-expand' -> cl-macs.el
(defun cl::cl-defsubst-expand (argns body simple whole unsafe &rest argvs)
  (if (and whole (not (cl::cl-safe-expr-p (cons 'progn argvs)))) whole
    (if (cl::cl-simple-exprs-p argvs) (setq simple t))
    ;; :SEE :CHANGESET 99853 2010-04-08 15:59:47 
    ;;      "Do the substitutions simultaneously"
    ;;
    ;; :WAS (let ((lets (delq nil
    ;;                   (cl::mapcar* (function
    ;;                                 (lambda (argn argv)
    ;;                                   (if (or simple (cl::cl-const-expr-p argv))
    ;;                                       (progn (setq body (cl::subst argv argn body))
    ;;                                              (and unsafe (list argn argv)))
    ;;                                     (list argn argv))))
    ;;                                argns argvs))))
    ;;   (if lets (list 'let lets body) body))))
    (let* ((substs ())
           (lets (delq nil
                       (cl::mapcar* (function
                                     (lambda (argn argv)
                                   (if (or simple (cl::cl-const-expr-p argv))
                                       (progn (push (cons argn argv) substs)
                                              (and unsafe (list argn argv)))
                                     (list argn argv))))
                                argns argvs))))
      ;; FIXME: `sublis/subst' will happily substitute the symbol
      ;; `argn' in places where it's not used as a reference
      ;; to a variable.
      ;; FIXME: `sublis/subst' will happily copy `argv' to a different
      ;; scope, leading to name capture.
      (setq body (cond ((null substs) body)
                       ((null (cdr substs))
                        (cl::subst (cdar substs) (caar substs) body))
                       (t (cl::sublis substs body))))
      (if lets (list 'let lets body) body))))


;;; ==============================
;;; :NOTE Top of cl-macs.el puts the 'cl-kludge on features for byte-compiler.
;;; Do we need to do this also??? e.g. the lambda ?n ?p ?f
;;; `cl-transform-function-property' -> cl-macs.el
(defun cl::cl-transform-function-property (func prop form)
  (let ((res (cl::cl-transform-lambda form func)))
    (append '(progn) (cdr (cdr (car res)))
	    (list (list 'put (list 'quote func) (list 'quote prop)
			(list 'function (cons 'lambda (cdr res))))))))

;;; ==============================
;;; Not quite sure what to do with these, or why their defvar'd. 
;;; Best i can tell we're interning the symbol... 
;;; I guess the defvar is so the bc can see 'em.
;;; They don't appear in the evironment post compile time.
(defvar bind-block) (defvar bind-defs) (defvar bind-enquote)
(defvar bind-inits) (defvar bind-lets) (defvar bind-forms)

;;; ==============================
;;; `cl-transform-lambda' -> cl-macs.el
(defun cl::cl-transform-lambda (form bind-block)
  (let* ((args (car form)) (body (cdr form)) (orig-args args)
	 (bind-defs nil) (bind-enquote nil)
	 (bind-inits nil) (bind-lets nil) (bind-forms nil)
	 (header nil) (simple-args nil))
    (while (or (stringp (car body))
	       (memq (car-safe (car body)) '(interactive declare)))
      (push (pop body) header))
    (setq args (if (listp args) (cl::copy-list args) (list '&rest args)))
    (let ((p (last args))) (if (cdr p) (setcdr p (list '&rest (cdr p)))))
    (if (setq bind-defs (cadr (memq '&cl-defs args)))
	(setq args (delq '&cl-defs (delq bind-defs args))
	      bind-defs (cadr bind-defs)))
    (if (setq bind-enquote (memq '&cl-quote args))
	(setq args (delq '&cl-quote args)))
    (if (memq '&whole args) (error "&whole not currently implemented"))
    (let* ((p (memq '&environment args)) (v (cadr p)))
      (if p (setq args (nconc (delq (car p) (delq v args))
			      (list '&aux (list v 'cl-macro-environment))))))
    (while (and args (symbolp (car args))
		(not (memq (car args) '(nil &rest &body &key &aux)))
		(not (and (eq (car args) '&optional)
			  (or bind-defs (consp (cadr args))))))
      (push (pop args) simple-args))
    (or (eq bind-block 'cl-none)
	(setq body (list (list* 'block bind-block body))))
    (if (null args)
	(list* nil (nreverse simple-args) (nconc (nreverse header) body))
      (if (memq '&optional simple-args) (push '&optional args))
      (cl::cl-do-arglist args nil (- (length simple-args)
				 (if (memq '&optional simple-args) 1 0)))
      (setq bind-lets (nreverse bind-lets))
      (list* (and bind-inits (list* 'eval-when '(compile load eval)
				    (nreverse bind-inits)))
	     (nconc (nreverse simple-args)
		    (list '&rest (car (pop bind-lets))))
	     (nconc (let ((hdr (nreverse header)))
                      ;; Macro expansion can take place in the middle of
                      ;; apparently harmless computation, so it should not
                      ;; touch the match-data.
                      (save-match-data
                        (require 'help-fns)
                        (cons (help-add-fundoc-usage
                               (if (stringp (car hdr)) (pop hdr))
                               ;; orig-args can contain &cl-defs (an internal
                               ;; CL thingy I don't understand), so remove it.
                               (let ((x (memq '&cl-defs orig-args)))
                                 (if (null x) orig-args
                                   (delq (car x) (remq (cadr x) orig-args)))))
                              hdr)))
		    (list (nconc (list 'let* bind-lets)
				 (nreverse bind-forms) body)))))))

;;; ==============================
;;; :NOTE `lambda-list-keywords' is a variable in cl-macs.el 
;;; `cl-do-arglist' -> cl-macs.el
(defun cl::cl-do-arglist (args expr &optional num)   ; uses bind-*
  (if (nlistp args)
      (if (or (memq args lambda-list-keywords) (not (symbolp args)))
	  (error "Invalid argument name: %s" args)
	(push (list args expr) bind-lets))
    (setq args (cl::copy-list args))
    (let ((p (last args))) (if (cdr p) (setcdr p (list '&rest (cdr p)))))
    (let ((p (memq '&body args))) (if p (setcar p '&rest)))
    (if (memq '&environment args) (error "&environment used incorrectly"))
    (let ((save-args args)
	  (restarg (memq '&rest args))
	  (safety (if (cl::cl-compiling-file) cl-optimize-safety 3)) ;; !!!
	  (keys nil)
	  (laterarg nil) (exactarg nil) minarg)
      (or num (setq num 0))
      (if (listp (cadr restarg))
	  (setq restarg (make-symbol "--cl-rest--"))
	(setq restarg (cadr restarg)))
      (push (list restarg expr) bind-lets)
      (if (eq (car args) '&whole)
	  (push (list (cl-pop2 args) restarg) bind-lets)) ;`cl-pop2' <MACRO>
      (let ((p args))
	(setq minarg restarg)
	(while (and p (not (memq (car p) lambda-list-keywords)))
	  (or (eq p args) (setq minarg (list 'cdr minarg)))
	  (setq p (cdr p)))
	(if (memq (car p) '(nil &aux))
	    (setq minarg (list '= (list 'length restarg)
			       (length (cl::ldiff args p)))
		  exactarg (not (eq args p)))))
      (while (and args (not (memq (car args) lambda-list-keywords)))
	(let ((poparg (list (if (or (cdr args) (not exactarg)) 'pop 'car)
			    restarg)))
	  (cl::cl-do-arglist
	   (pop args)
	   (if (or laterarg (= safety 0)) poparg
	     (list 'if minarg poparg
		   (list 'signal '(quote wrong-number-of-arguments)
			 (list 'list (and (not (eq bind-block 'cl-none))
					  (list 'quote bind-block))
			       (list 'length restarg)))))))
	(setq num (1+ num) laterarg t))
      (while (and (eq (car args) '&optional) (pop args))
	(while (and args (not (memq (car args) lambda-list-keywords)))
	  (let ((arg (pop args)))
	    (or (consp arg) (setq arg (list arg)))
	    (if (cddr arg) (cl::cl-do-arglist (nth 2 arg) (list 'and restarg t)))
	    (let ((def (if (cdr arg) (nth 1 arg)
			 (or (car bind-defs)
			     (nth 1 (assq (car arg) bind-defs)))))
		  (poparg (list 'pop restarg)))
	      (and def bind-enquote (setq def (list 'quote def)))
	      (cl::cl-do-arglist (car arg)
			     (if def (list 'if restarg poparg def) poparg))
	      (setq num (1+ num))))))
      (if (eq (car args) '&rest)
	  (let ((arg (cl-pop2 args)))
	    (if (consp arg) (cl::cl-do-arglist arg restarg)))
	(or (eq (car args) '&key) (= safety 0) exactarg
	    (push (list 'if restarg
			   (list 'signal '(quote wrong-number-of-arguments)
				 (list 'list
				       (and (not (eq bind-block 'cl-none))
					    (list 'quote bind-block))
				       (list '+ num (list 'length restarg)))))
		     bind-forms)))
      (while (and (eq (car args) '&key) (pop args))
	(while (and args (not (memq (car args) lambda-list-keywords)))
	  (let ((arg (pop args)))
	    (or (consp arg) (setq arg (list arg)))
	    (let* ((karg (if (consp (car arg)) (caar arg)
			   (intern (format ":%s" (car arg)))))
		   (varg (if (consp (car arg)) (cadar arg) (car arg)))
		   (def (if (cdr arg) (cadr arg)
			  (or (car bind-defs) (cadr (assq varg bind-defs)))))
		   (look (list 'memq (list 'quote karg) restarg)))
	      (and def bind-enquote (setq def (list 'quote def)))
	      (if (cddr arg)
		  (let* ((temp (or (nth 2 arg) (make-symbol "--cl-var--")))
			 (val (list 'car (list 'cdr temp))))
		    (cl::cl-do-arglist temp look)
		    (cl::cl-do-arglist varg
				   (list 'if temp
					 (list 'prog1 val (list 'setq temp t))
					 def)))
		(cl::cl-do-arglist
		 varg
		 (list 'car
		       (list 'cdr
			     (if (null def)
				 look
			       (list 'or look
				     (if (eq (cl::cl-const-expr-p def) t)
					 (list
					  'quote
					  (list nil (cl::cl-const-expr-val def)))
				       (list 'list nil def))))))))
	      (push karg keys)))))
      (setq keys (nreverse keys))
      (or (and (eq (car args) '&allow-other-keys) (pop args))
	  (null keys) (= safety 0)
	  (let* ((var (make-symbol "--cl-keys--"))
		 (allow '(:allow-other-keys))
		 (check (list
			 'while var
			 (list
			  'cond
			  (list (list 'memq (list 'car var)
				      (list 'quote (append keys allow)))
				(list 'setq var (list 'cdr (list 'cdr var))))
			  (list (list 'car
				      (list 'cdr
					    (list 'memq (cons 'quote allow)
						  restarg)))
				(list 'setq var nil))
			  (list t
				(list
				 'error
				 (format "Keyword argument %%s not one of %s"
					 keys)
				 (list 'car var)))))))
	    (push (list 'let (list (list var restarg)) check) bind-forms)))
      (while (and (eq (car args) '&aux) (pop args))
	(while (and args (not (memq (car args) lambda-list-keywords)))
	  (if (consp (car args))
	      (if (and bind-enquote (cadar args))
		  (cl::cl-do-arglist (caar args)
				 (list 'quote (cadr (pop args))))
		(cl::cl-do-arglist (caar args) (cadr (pop args))))
	    (cl::cl-do-arglist (pop args) nil))))
      (if args (error "Malformed argument list %s" save-args)))))

;;; ==============================
;;; `cl-arglist-args' -> cl-macs.el
(defun cl::cl-arglist-args (args)
  (if (nlistp args) (list args)
    (let ((res nil) (kind nil) arg)
      (while (consp args)
	(setq arg (pop args))
	(if (memq arg lambda-list-keywords) (setq kind arg)
	  (if (eq arg '&cl-defs) (pop args)
	    (and (consp arg) kind (setq arg (car arg)))
	    (and (consp arg) (cdr arg) (eq kind '&key) (setq arg (cadr arg)))
	    (setq res (nconc res (cl::cl-arglist-args arg))))))
      (nconc res (and args (list args))))))

;;; ==============================
;;; `cl-compile-time-too' -> cl-macs.el
(defun cl::cl-compile-time-too (form)
  (or (and (symbolp (car-safe form)) (get (car-safe form) 'byte-hunk-handler))
      (setq form (macroexpand
		  form (cons '(eval-when) byte-compile-macro-environment))))
  (cond ((eq (car-safe form) 'progn)
	 (cons 'progn (mapcar 'cl::cl-compile-time-too (cdr form))))
	((eq (car-safe form) 'eval-when)
	 (let ((when (nth 1 form)))
	   (if (or (memq 'eval when) (memq :execute when))
	       (list* 'eval-when (cons 'compile when) (cddr form))
	     form)))
	(t (eval form) form)))

;;; ==============================
;;; `cl-byte-compile-block' -> cl-macs.el
(put 'cl-block-wrapper 'byte-compile 'cl-byte-compile-block)
(defun cl::cl-byte-compile-block (cl-form)
  (if (fboundp 'byte-compile-form-do-effect)  ; Check for optimizing compiler
      (progn
	(let* ((cl-entry (cons (nth 1 (nth 1 (nth 1 cl-form))) nil))
	       (cl-active-block-names (cons cl-entry cl-active-block-names))
	       (cl-body (byte-compile-top-level
			 (cons 'progn (cddr (nth 1 cl-form))))))
	  (if (cdr cl-entry)
	      (byte-compile-form (list 'catch (nth 1 (nth 1 cl-form)) cl-body))
	    (byte-compile-form cl-body))))
    (byte-compile-form (nth 1 cl-form))))

;;; ==============================
;;; `cl-byte-compile-throw' -> cl-macs.el
(put 'cl-block-throw 'byte-compile 'cl::cl-byte-compile-throw)
(defun cl::cl-byte-compile-throw (cl-form)
  (let ((cl-found (assq (nth 1 (nth 1 cl-form)) cl-active-block-names)))
    (if cl-found (setcdr cl-found t)))
  (byte-compile-normal-call (cons 'throw (cdr cl-form))))

;;; ==============================
;;; {... SKIPPING LOOP for now `cl-parse-loop-clause', 
;;; `cl-loop-let', `cl-loop-handle-accum', `cl-loop-build-ands' ...} 
;;; `cl-expand-do-loop', 
;;; ==============================

;;; ==============================
;;; `compiler-macroexpand' -> cl-macs.el
;;;###autoload
(defun cl::compiler-macroexpand (form)
  (while
      (let ((func (car-safe form)) (handler nil))
	(while (and (symbolp func)
		    (not (setq handler (get func 'cl-compiler-macro)))
		    (fboundp func)
		    (or (not (eq (car-safe (symbol-function func)) 'autoload))
			(load (nth 1 (symbol-function func)))))
	  (setq func (symbol-function func)))
	(and handler
	     (not (eq form (setq form (apply handler form (cdr form))))))))
  form)

;;; ==============================
;;; :NOTE Evaluated at bottom of cl-macs.el to put variaous flags for byte-compiler.
;;; `cl-byte-compile-compiler-macro' -> cl-macs.el
(defun cl::cl-byte-compile-compiler-macro (form)
  (if (eq form (setq form (cl::compiler-macroexpand form)))
      (byte-compile-normal-call form)
    (byte-compile-form form)))

;;; ==============================
;;; `cl-make-type-test' -> :FILE cl-macs.el
(defun cl::cl-make-type-test (val type)
  (if (symbolp type)
      (cond ((get type 'cl-deftype-handler)
	     (cl::cl-make-type-test val (funcall (get type 'cl-deftype-handler))))
	    ((memq type '(nil t)) type)
	    ((eq type 'null) `(null ,val))
	    ((eq type 'atom) `(atom ,val))
	    ((eq type 'float) `(cl::floatp-safe ,val))
	    ((eq type 'real) `(numberp ,val))
	    ((eq type 'fixnum) `(integerp ,val))
	    ;; FIXME: Should `character' accept things like ?\C-\M-a ?  -stef
	    ((memq type '(character string-char)) `(characterp ,val))
	    (t
	     (let* ((name (symbol-name type))
		    (namep (intern (concat name "p"))))
	       (if (fboundp namep) (list namep val)          ;; What is `namep'???
		 (list (intern (concat name "-p")) val)))))
    (cond ((get (car type) 'cl-deftype-handler)
	   (cl::cl-make-type-test val (apply (get (car type) 'cl-deftype-handler)
					 (cdr type))))
	  ((memq (car type) '(integer float real number))
	   (delq t (list 'and (cl::cl-make-type-test val (car type))
			 (if (memq (cadr type) '(* nil)) t
			   (if (consp (cadr type)) (list '> val (caadr type)) ;<-`caadr' cl.el
			     (list '>= val (cadr type))))
			 (if (memq (caddr type) '(* nil)) t
			   (if (consp (caddr type)) (list '< val (caaddr type)) ;<-`caaddr' cl.el
			     (list '<= val (caddr type))))))) ;<-`caddr' cl.el
	  ((memq (car type) '(and or not))
	   (cons (car type)
		 (mapcar (function (lambda (x) (cl::cl-make-type-test val x)))
			 (cdr type))))
	  ((memq (car type) '(member cl::member*))
	   (list 'and (list 'cl::member* val (list 'quote (cdr type))) t))
	  ((eq (car type) 'satisfies) (list (cadr type) val))
	  (t (error "Bad type spec: %s" type)))))

;;;###autoload
(defun cl::reduce (cl-func cl-seq &rest cl-keys)
  "Reduce two-argument FUNCTION across SEQ.
\nKeywords supported:  :start :end :from-end :initial-value :key
\n(fn FUNCTION SEQ [KEYWORD VALUE]...)"
  (cl-parsing-keywords (:from-end (:start 0) :end :initial-value :key) ()
    (or (listp cl-seq) (setq cl-seq (append cl-seq nil)))
    (setq cl-seq (cl::subseq cl-seq cl-start cl-end))
    (if cl-from-end (setq cl-seq (nreverse cl-seq)))
    (let ((cl-accum (cond ((memq :initial-value cl-keys) cl-initial-value)
			  (cl-seq (cl-check-key (pop cl-seq)))
			  (t (funcall cl-func)))))
      (if cl-from-end
	  (while cl-seq
	    (setq cl-accum (funcall cl-func (cl-check-key (pop cl-seq))
				    cl-accum)))
	(while cl-seq
	  (setq cl-accum (funcall cl-func cl-accum
				  (cl-check-key (pop cl-seq))))))
      cl-accum)))

;;;###autoload
(defun cl::fill (seq item &rest cl-keys)
  "Fill the elements of SEQ with ITEM.
\nKeywords supported:  :start :end
\n(fn SEQ ITEM [KEYWORD VALUE]...)"
  (cl-parsing-keywords ((:start 0) :end) ()
    (if (listp seq)
	(let ((p (nthcdr cl-start seq))
	      (n (if cl-end (- cl-end cl-start) 8000000)))
	  (while (and p (>= (setq n (1- n)) 0))
	    (setcar p item)
	    (setq p (cdr p))))
      (or cl-end (setq cl-end (length seq)))
      (if (and (= cl-start 0) (= cl-end (length seq)))
	  (fillarray seq item)
	(while (< cl-start cl-end)
	  (aset seq cl-start item)
	  (setq cl-start (1+ cl-start)))))
    seq))

;;;###autoload
(defun cl::replace (cl-seq1 cl-seq2 &rest cl-keys)
  "Replace the elements of SEQ1 with the elements of SEQ2.
SEQ1 is destructively modified, then returned.
\nKeywords supported:  :start1 :end1 :start2 :end2
\n(fn SEQ1 SEQ2 [KEYWORD VALUE]...)"
  (cl-parsing-keywords ((:start1 0) :end1 (:start2 0) :end2) ()
    (if (and (eq cl-seq1 cl-seq2) (<= cl-start2 cl-start1))
	(or (= cl-start1 cl-start2)
	    (let* ((cl-len (length cl-seq1))
		   (cl-n (min (- (or cl-end1 cl-len) cl-start1)
			      (- (or cl-end2 cl-len) cl-start2))))
	      (while (>= (setq cl-n (1- cl-n)) 0)
		(cl-set-elt cl-seq1 (+ cl-start1 cl-n)
			    (elt cl-seq2 (+ cl-start2 cl-n))))))
      (if (listp cl-seq1)
	  (let ((cl-p1 (nthcdr cl-start1 cl-seq1))
		(cl-n1 (if cl-end1 (- cl-end1 cl-start1) 4000000)))
	    (if (listp cl-seq2)
		(let ((cl-p2 (nthcdr cl-start2 cl-seq2))
		      (cl-n (min cl-n1
				 (if cl-end2 (- cl-end2 cl-start2) 4000000))))
		  (while (and cl-p1 cl-p2 (>= (setq cl-n (1- cl-n)) 0))
		    (setcar cl-p1 (car cl-p2))
		    (setq cl-p1 (cdr cl-p1) cl-p2 (cdr cl-p2))))
	      (setq cl-end2 (min (or cl-end2 (length cl-seq2))
				 (+ cl-start2 cl-n1)))
	      (while (and cl-p1 (< cl-start2 cl-end2))
		(setcar cl-p1 (aref cl-seq2 cl-start2))
		(setq cl-p1 (cdr cl-p1) cl-start2 (1+ cl-start2)))))
	(setq cl-end1 (min (or cl-end1 (length cl-seq1))
			   (+ cl-start1 (- (or cl-end2 (length cl-seq2))
					   cl-start2))))
	(if (listp cl-seq2)
	    (let ((cl-p2 (nthcdr cl-start2 cl-seq2)))
	      (while (< cl-start1 cl-end1)
		(aset cl-seq1 cl-start1 (car cl-p2))
		(setq cl-p2 (cdr cl-p2) cl-start1 (1+ cl-start1))))
	  (while (< cl-start1 cl-end1)
	    (aset cl-seq1 cl-start1 (aref cl-seq2 cl-start2))
	    (setq cl-start2 (1+ cl-start2) cl-start1 (1+ cl-start1))))))
    cl-seq1))

;;;###autoload
(defun cl::remove* (cl-item cl-seq &rest cl-keys)
  "Remove all occurrences of ITEM in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :test :test-not :key :count :start :end :from-end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (cl-parsing-keywords (:test :test-not :key :if :if-not :count :from-end
			(:start 0) :end) ()
    (if (<= (or cl-count (setq cl-count 8000000)) 0)
	cl-seq
      (if (or (nlistp cl-seq) (and cl-from-end (< cl-count 4000000)))
	  (let ((cl-i (cl::cl-position cl-item cl-seq cl-start cl-end
				   cl-from-end)))
	    (if cl-i
		(let ((cl-res (apply 'delete* cl-item (append cl-seq nil)
				     (append (if cl-from-end
						 (list :end (1+ cl-i))
					       (list :start cl-i))
					     cl-keys))))
		  (if (listp cl-seq) cl-res
		    (if (stringp cl-seq) (concat cl-res) (vconcat cl-res))))
	      cl-seq))
	(setq cl-end (- (or cl-end 8000000) cl-start))
	(if (= cl-start 0)
	    (while (and cl-seq (> cl-end 0)
			(cl-check-test cl-item (car cl-seq))
			(setq cl-end (1- cl-end) cl-seq (cdr cl-seq))
			(> (setq cl-count (1- cl-count)) 0))))
	(if (and (> cl-count 0) (> cl-end 0))
	    (let ((cl-p (if (> cl-start 0) (nthcdr cl-start cl-seq)
			  (setq cl-end (1- cl-end)) (cdr cl-seq))))
	      (while (and cl-p (> cl-end 0)
			  (not (cl-check-test cl-item (car cl-p))))
		(setq cl-p (cdr cl-p) cl-end (1- cl-end)))
	      (if (and cl-p (> cl-end 0))
		  (nconc (cl::ldiff cl-seq cl-p)
			 (if (= cl-count 1) (cdr cl-p)
			   (and (cdr cl-p)
				(apply 'delete* cl-item
				       (copy-sequence (cdr cl-p))
				       :start 0 :end (1- cl-end)
				       :count (1- cl-count) cl-keys))))
		cl-seq))
	  cl-seq)))))

;;;###autoload
(defun cl::remove-if (cl-pred cl-list &rest cl-keys)
  "Remove all items satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl::remove* nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl::remove-if-not (cl-pred cl-list &rest cl-keys)
  "Remove all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl::remove* nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl::delete* (cl-item cl-seq &rest cl-keys)
  "Remove all occurrences of ITEM in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :test :test-not :key :count :start :end :from-end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (cl-parsing-keywords (:test :test-not :key :if :if-not :count :from-end
			(:start 0) :end) ()
    (if (<= (or cl-count (setq cl-count 8000000)) 0)
	cl-seq
      (if (listp cl-seq)
	  (if (and cl-from-end (< cl-count 4000000))
	      (let (cl-i)
		(while (and (>= (setq cl-count (1- cl-count)) 0)
			    (setq cl-i (cl::cl-position cl-item cl-seq cl-start
						    cl-end cl-from-end)))
		  (if (= cl-i 0) (setq cl-seq (cdr cl-seq))
		    (let ((cl-tail (nthcdr (1- cl-i) cl-seq)))
		      (setcdr cl-tail (cdr (cdr cl-tail)))))
		  (setq cl-end cl-i))
		cl-seq)
	    (setq cl-end (- (or cl-end 8000000) cl-start))
	    (if (= cl-start 0)
		(progn
		  (while (and cl-seq
			      (> cl-end 0)
			      (cl-check-test cl-item (car cl-seq))
			      (setq cl-end (1- cl-end) cl-seq (cdr cl-seq))
			      (> (setq cl-count (1- cl-count)) 0)))
		  (setq cl-end (1- cl-end)))
	      (setq cl-start (1- cl-start)))
	    (if (and (> cl-count 0) (> cl-end 0))
		(let ((cl-p (nthcdr cl-start cl-seq)))
		  (while (and (cdr cl-p) (> cl-end 0))
		    (if (cl-check-test cl-item (car (cdr cl-p)))
			(progn
			  (setcdr cl-p (cdr (cdr cl-p)))
			  (if (= (setq cl-count (1- cl-count)) 0)
			      (setq cl-end 1)))
		      (setq cl-p (cdr cl-p)))
		    (setq cl-end (1- cl-end)))))
	    cl-seq)
	(apply 'cl::remove* cl-item cl-seq cl-keys)))))

;;;###autoload
(defun cl::delete-if (cl-pred cl-list &rest cl-keys)
  "Remove all items satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl::delete* nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl::delete-if-not (cl-pred cl-list &rest cl-keys)
  "Remove all items not satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl::delete* nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl::remove-duplicates (cl-seq &rest cl-keys)
  "Return a copy of SEQ with all duplicate elements removed.
\nKeywords supported:  :test :test-not :key :start :end :from-end
\n(fn SEQ [KEYWORD VALUE]...)"
  (cl::cl-delete-duplicates cl-seq cl-keys t))

;;;###autoload
(defun cl::delete-duplicates (cl-seq &rest cl-keys)
  "Remove all duplicate elements from SEQ (destructively).
\nKeywords supported:  :test :test-not :key :start :end :from-end
\n(fn SEQ [KEYWORD VALUE]...)"
  (cl::cl-delete-duplicates cl-seq cl-keys nil))

(defun cl::cl-delete-duplicates (cl-seq cl-keys cl-copy)
  (if (listp cl-seq)
      (cl-parsing-keywords (:test :test-not :key (:start 0) :end :from-end :if)
	  ()
	(if cl-from-end
	    (let ((cl-p (nthcdr cl-start cl-seq)) cl-i)
	      (setq cl-end (- (or cl-end (length cl-seq)) cl-start))
	      (while (> cl-end 1)
		(setq cl-i 0)
		(while (setq cl-i (cl::cl-position (cl-check-key (car cl-p))
					       (cdr cl-p) cl-i (1- cl-end)))
		  (if cl-copy (setq cl-seq (copy-sequence cl-seq)
				    cl-p (nthcdr cl-start cl-seq) cl-copy nil))
		  (let ((cl-tail (nthcdr cl-i cl-p)))
		    (setcdr cl-tail (cdr (cdr cl-tail))))
		  (setq cl-end (1- cl-end)))
		(setq cl-p (cdr cl-p) cl-end (1- cl-end)
		      cl-start (1+ cl-start)))
	      cl-seq)
	  (setq cl-end (- (or cl-end (length cl-seq)) cl-start))
	  (while (and (cdr cl-seq) (= cl-start 0) (> cl-end 1)
		      (cl::cl-position (cl-check-key (car cl-seq))
				   (cdr cl-seq) 0 (1- cl-end)))
	    (setq cl-seq (cdr cl-seq) cl-end (1- cl-end)))
	  (let ((cl-p (if (> cl-start 0) (nthcdr (1- cl-start) cl-seq)
			(setq cl-end (1- cl-end) cl-start 1) cl-seq)))
	    (while (and (cdr (cdr cl-p)) (> cl-end 1))
	      (if (cl::cl-position (cl-check-key (car (cdr cl-p)))
			       (cdr (cdr cl-p)) 0 (1- cl-end))
		  (progn
		    (if cl-copy (setq cl-seq (copy-sequence cl-seq)
				      cl-p (nthcdr (1- cl-start) cl-seq)
				      cl-copy nil))
		    (setcdr cl-p (cdr (cdr cl-p))))
		(setq cl-p (cdr cl-p)))
	      (setq cl-end (1- cl-end) cl-start (1+ cl-start)))
	    cl-seq)))
    (let ((cl-res (cl::cl-delete-duplicates (append cl-seq nil) cl-keys nil)))
      (if (stringp cl-seq) (concat cl-res) (vconcat cl-res)))))

;;;###autoload
(defun cl::substitute (cl-new cl-old cl-seq &rest cl-keys)
  "Substitute NEW for OLD in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :test :test-not :key :count :start :end :from-end
\n(fn NEW OLD SEQ [KEYWORD VALUE]...)"
  (cl-parsing-keywords (:test :test-not :key :if :if-not :count
			(:start 0) :end :from-end) ()
    (if (or (eq cl-old cl-new)
	    (<= (or cl-count (setq cl-from-end nil cl-count 8000000)) 0))
	cl-seq
      (let ((cl-i (cl::cl-position cl-old cl-seq cl-start cl-end)))
	(if (not cl-i)
	    cl-seq
	  (setq cl-seq (copy-sequence cl-seq))
	  (or cl-from-end
	      (progn (cl-set-elt cl-seq cl-i cl-new)
		     (setq cl-i (1+ cl-i) cl-count (1- cl-count))))
	  (apply 'cl::nsubstitute cl-new cl-old cl-seq :count cl-count
		 :start cl-i cl-keys))))))

;;;###autoload
(defun cl::substitute-if (cl-new cl-pred cl-list &rest cl-keys)
  "Substitute NEW for all items satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl::substitute cl-new nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl::substitute-if-not (cl-new cl-pred cl-list &rest cl-keys)
  "Substitute NEW for all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl::substitute cl-new nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl::nsubstitute (cl-new cl-old cl-seq &rest cl-keys)
  "Substitute NEW for OLD in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :test :test-not :key :count :start :end :from-end
\n(fn NEW OLD SEQ [KEYWORD VALUE]...)"
  (cl-parsing-keywords (:test :test-not :key :if :if-not :count
			(:start 0) :end :from-end) ()
    (or (eq cl-old cl-new) (<= (or cl-count (setq cl-count 8000000)) 0)
	(if (and (listp cl-seq) (or (not cl-from-end) (> cl-count 4000000)))
	    (let ((cl-p (nthcdr cl-start cl-seq)))
	      (setq cl-end (- (or cl-end 8000000) cl-start))
	      (while (and cl-p (> cl-end 0) (> cl-count 0))
		(if (cl-check-test cl-old (car cl-p))
		    (progn
		      (setcar cl-p cl-new)
		      (setq cl-count (1- cl-count))))
		(setq cl-p (cdr cl-p) cl-end (1- cl-end))))
	  (or cl-end (setq cl-end (length cl-seq)))
	  (if cl-from-end
	      (while (and (< cl-start cl-end) (> cl-count 0))
		(setq cl-end (1- cl-end))
		(if (cl-check-test cl-old (elt cl-seq cl-end))
		    (progn
		      (cl-set-elt cl-seq cl-end cl-new)
		      (setq cl-count (1- cl-count)))))
	    (while (and (< cl-start cl-end) (> cl-count 0))
	      (if (cl-check-test cl-old (aref cl-seq cl-start))
		  (progn
		    (aset cl-seq cl-start cl-new)
		    (setq cl-count (1- cl-count))))
	      (setq cl-start (1+ cl-start))))))
    cl-seq))

;;;###autoload
(defun cl::nsubstitute-if (cl-new cl-pred cl-list &rest cl-keys)
  "Substitute NEW for all items satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl::nsubstitute cl-new nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl::nsubstitute-if-not (cl-new cl-pred cl-list &rest cl-keys)
  "Substitute NEW for all items not satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl::nsubstitute cl-new nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl::find (cl-item cl-seq &rest cl-keys)
  "Find the first occurrence of ITEM in SEQ.
Return the matching ITEM, or nil if not found.
\nKeywords supported:  :test :test-not :key :start :end :from-end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (let ((cl-pos (apply 'position cl-item cl-seq cl-keys)))
    (and cl-pos (elt cl-seq cl-pos))))

;;;###autoload
(defun cl::find-if (cl-pred cl-list &rest cl-keys)
  "Find the first item satisfying PREDICATE in SEQ.
Return the matching item, or nil if not found.
\nKeywords supported:  :key :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl::find nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl::find-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item not satisfying PREDICATE in SEQ.
Return the matching item, or nil if not found.
\nKeywords supported:  :key :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl::find nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl::position (cl-item cl-seq &rest cl-keys)
  "Find the first occurrence of ITEM in SEQ.
Return the index of the matching item, or nil if not found.
\nKeywords supported:  :test :test-not :key :start :end :from-end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (cl-parsing-keywords (:test :test-not :key :if :if-not
			(:start 0) :end :from-end) ()
    (cl::cl-position cl-item cl-seq cl-start cl-end cl-from-end)))

(defun cl::cl-position (cl-item cl-seq cl-start &optional cl-end cl-from-end)
  (if (listp cl-seq)
      (let ((cl-p (nthcdr cl-start cl-seq)))
	(or cl-end (setq cl-end 8000000))
	(let ((cl-res nil))
	  (while (and cl-p (< cl-start cl-end) (or (not cl-res) cl-from-end))
	    (if (cl-check-test cl-item (car cl-p))
		(setq cl-res cl-start))
	    (setq cl-p (cdr cl-p) cl-start (1+ cl-start)))
	  cl-res))
    (or cl-end (setq cl-end (length cl-seq)))
    (if cl-from-end
	(progn
	  (while (and (>= (setq cl-end (1- cl-end)) cl-start)
		      (not (cl-check-test cl-item (aref cl-seq cl-end)))))
	  (and (>= cl-end cl-start) cl-end))
      (while (and (< cl-start cl-end)
		  (not (cl-check-test cl-item (aref cl-seq cl-start))))
	(setq cl-start (1+ cl-start)))
      (and (< cl-start cl-end) cl-start))))

;;;###autoload
(defun cl::position-if (cl-pred cl-list &rest cl-keys)
  "Find the first item satisfying PREDICATE in SEQ.
Return the index of the matching item, or nil if not found.
\nKeywords supported:  :key :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl::position nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl::position-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item not satisfying PREDICATE in SEQ.
Return the index of the matching item, or nil if not found.
\nKeywords supported:  :key :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl::position nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl::count (cl-item cl-seq &rest cl-keys)
  "Count the number of occurrences of ITEM in SEQ.
\nKeywords supported:  :test :test-not :key :start :end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (cl-parsing-keywords (:test :test-not :key :if :if-not (:start 0) :end) ()
    (let ((cl-count 0) cl-x)
      (or cl-end (setq cl-end (length cl-seq)))
      (if (consp cl-seq) (setq cl-seq (nthcdr cl-start cl-seq)))
      (while (< cl-start cl-end)
	(setq cl-x (if (consp cl-seq) (pop cl-seq) (aref cl-seq cl-start)))
	(if (cl-check-test cl-item cl-x) (setq cl-count (1+ cl-count)))
	(setq cl-start (1+ cl-start)))
      cl-count)))

;;;###autoload
(defun cl::count-if (cl-pred cl-list &rest cl-keys)
  "Count the number of items satisfying PREDICATE in SEQ.
\nKeywords supported:  :key :start :end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl::count nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl::count-if-not (cl-pred cl-list &rest cl-keys)
  "Count the number of items not satisfying PREDICATE in SEQ.
\nKeywords supported:  :key :start :end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl::count nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl::mismatch (cl-seq1 cl-seq2 &rest cl-keys)
  "Compare SEQ1 with SEQ2, return index of first mismatching element.
Return nil if the sequences match.  If one sequence is a prefix of the
other, the return value indicates the end of the shorter sequence.
\nKeywords supported:  :test :test-not :key :start1 :end1 :start2 :end2 :from-end
\n(fn SEQ1 SEQ2 [KEYWORD VALUE]...)"
  (cl-parsing-keywords (:test :test-not :key :from-end
			(:start1 0) :end1 (:start2 0) :end2) ()
    (or cl-end1 (setq cl-end1 (length cl-seq1)))
    (or cl-end2 (setq cl-end2 (length cl-seq2)))
    (if cl-from-end
	(progn
	  (while (and (< cl-start1 cl-end1) (< cl-start2 cl-end2)
		      (cl-check-match (elt cl-seq1 (1- cl-end1))
				      (elt cl-seq2 (1- cl-end2))))
	    (setq cl-end1 (1- cl-end1) cl-end2 (1- cl-end2)))
	  (and (or (< cl-start1 cl-end1) (< cl-start2 cl-end2))
	       (1- cl-end1)))
      (let ((cl-p1 (and (listp cl-seq1) (nthcdr cl-start1 cl-seq1)))
	    (cl-p2 (and (listp cl-seq2) (nthcdr cl-start2 cl-seq2))))
	(while (and (< cl-start1 cl-end1) (< cl-start2 cl-end2)
		    (cl-check-match (if cl-p1 (car cl-p1)
				      (aref cl-seq1 cl-start1))
				    (if cl-p2 (car cl-p2)
				      (aref cl-seq2 cl-start2))))
	  (setq cl-p1 (cdr cl-p1) cl-p2 (cdr cl-p2)
		cl-start1 (1+ cl-start1) cl-start2 (1+ cl-start2)))
	(and (or (< cl-start1 cl-end1) (< cl-start2 cl-end2))
	     cl-start1)))))

;;;###autoload
(defun cl::search (cl-seq1 cl-seq2 &rest cl-keys)
  "Search for SEQ1 as a subsequence of SEQ2.
Return the index of the leftmost element of the first match found;
return nil if there are no matches.
\nKeywords supported:  :test :test-not :key :start1 :end1 :start2 :end2 :from-end
\n(fn SEQ1 SEQ2 [KEYWORD VALUE]...)"
  (cl-parsing-keywords (:test :test-not :key :from-end
			(:start1 0) :end1 (:start2 0) :end2) ()
    (or cl-end1 (setq cl-end1 (length cl-seq1)))
    (or cl-end2 (setq cl-end2 (length cl-seq2)))
    (if (>= cl-start1 cl-end1)
	(if cl-from-end cl-end2 cl-start2)
      (let* ((cl-len (- cl-end1 cl-start1))
	     (cl-first (cl-check-key (elt cl-seq1 cl-start1)))
	     (cl-if nil) cl-pos)
	(setq cl-end2 (- cl-end2 (1- cl-len)))
	(while (and (< cl-start2 cl-end2)
		    (setq cl-pos (cl::cl-position cl-first cl-seq2
					      cl-start2 cl-end2 cl-from-end))
		    (apply 'cl::mismatch cl-seq1 cl-seq2
			   :start1 (1+ cl-start1) :end1 cl-end1
			   :start2 (1+ cl-pos) :end2 (+ cl-pos cl-len)
			   :from-end nil cl-keys))
	  (if cl-from-end (setq cl-end2 cl-pos) (setq cl-start2 (1+ cl-pos))))
	(and (< cl-start2 cl-end2) cl-pos)))))

;;;###autoload
(defun cl::sort* (cl-seq cl-pred &rest cl-keys)
  "Sort the argument SEQ according to PREDICATE.
This is a destructive function; it reuses the storage of SEQ if possible.
\nKeywords supported:  :key
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  (if (nlistp cl-seq)
      (cl::replace cl-seq (apply 'cl::sort* (append cl-seq nil) cl-pred cl-keys))
    (cl-parsing-keywords (:key) ()
      (if (memq cl-key '(nil identity))
	  (sort cl-seq cl-pred)
	(sort cl-seq (function (lambda (cl-x cl-y)
				 (funcall cl-pred (funcall cl-key cl-x)
					  (funcall cl-key cl-y)))))))))

;;;###autoload
(defun cl::stable-sort (cl-seq cl-pred &rest cl-keys)
  "Sort the argument SEQ stably according to PREDICATE.
This is a destructive function; it reuses the storage of SEQ if possible.
\nKeywords supported:  :key
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  (apply 'cl::sort* cl-seq cl-pred cl-keys))

;;;###autoload
(defun cl::merge (cl-type cl-seq1 cl-seq2 cl-pred &rest cl-keys)
  "Destructively merge the two sequences to produce a new sequence.
TYPE is the sequence type to return, SEQ1 and SEQ2 are the two argument
sequences, and PREDICATE is a `less-than' predicate on the elements.
\nKeywords supported:  :key
\n(fn TYPE SEQ1 SEQ2 PREDICATE [KEYWORD VALUE]...)"
  (or (listp cl-seq1) (setq cl-seq1 (append cl-seq1 nil)))
  (or (listp cl-seq2) (setq cl-seq2 (append cl-seq2 nil)))
  (cl-parsing-keywords (:key) ()
    (let ((cl-res nil))
      (while (and cl-seq1 cl-seq2)
	(if (funcall cl-pred (cl-check-key (car cl-seq2))
		     (cl-check-key (car cl-seq1)))
	    (push (pop cl-seq2) cl-res)
	  (push (pop cl-seq1) cl-res)))
      (cl::coerce (nconc (nreverse cl-res) cl-seq1 cl-seq2) cl-type))))

;;;###autoload
(defun cl::member* (cl-item cl-list &rest cl-keys)
  "Find the first occurrence of ITEM in LIST.
Return the sublist of LIST whose car is ITEM.
\nKeywords supported:  :test :test-not :key
\n(fn ITEM LIST [KEYWORD VALUE]...)"
  (if cl-keys
      (cl-parsing-keywords (:test :test-not :key :if :if-not) ()
	(while (and cl-list (not (cl-check-test cl-item (car cl-list))))
	  (setq cl-list (cdr cl-list)))
	cl-list)
    (if (and (numberp cl-item) (not (integerp cl-item)))
	(member cl-item cl-list)
      (memq cl-item cl-list))))

;;;###autoload
(defun cl::member-if (cl-pred cl-list &rest cl-keys)
  "Find the first item satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (apply 'cl::member* nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl::member-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item not satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (apply 'cl::member* nil cl-list :if-not cl-pred cl-keys))

;;; :NOTE `adjoin' is a `define-compiler-macro' in cl-macs.el
;;;###autoload
(defun cl::cl-adjoin (cl-item cl-list &rest cl-keys)
  (if (cl-parsing-keywords (:key) t
	(apply 'cl::member* (cl-check-key cl-item) cl-list cl-keys))
      cl-list
    (cons cl-item cl-list)))

;;; :NOTE `assoc*' is a `define-compiler-macro' in cl-macs.el
;;;###autoload
(defun cl::assoc* (cl-item cl-alist &rest cl-keys)
  "Find the first item whose car matches ITEM in LIST.
\nKeywords supported:  :test :test-not :key
\n(fn ITEM LIST [KEYWORD VALUE]...)"
  (if cl-keys
      (cl-parsing-keywords (:test :test-not :key :if :if-not) ()
	(while (and cl-alist
		    (or (not (consp (car cl-alist)))
			(not (cl-check-test cl-item (car (car cl-alist))))))
	  (setq cl-alist (cdr cl-alist)))
	(and cl-alist (car cl-alist)))
    (if (and (numberp cl-item) (not (integerp cl-item)))
	(cl::assoc* cl-item cl-alist)
      (assq cl-item cl-alist))))

;;;###autoload
(defun cl::assoc-if (cl-pred cl-list &rest cl-keys)
  "Find the first item whose car satisfies PREDICATE in LIST.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (apply 'cl::assoc* nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl::assoc-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item whose car does not satisfy PREDICATE in LIST.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (apply 'cl::assoc* nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl::rassoc* (cl-item cl-alist &rest cl-keys)
  "Find the first item whose cdr matches ITEM in LIST.
\nKeywords supported:  :test :test-not :key
\n(fn ITEM LIST [KEYWORD VALUE]...)"
  (if (or cl-keys (numberp cl-item))
      (cl-parsing-keywords (:test :test-not :key :if :if-not) ()
	(while (and cl-alist
		    (or (not (consp (car cl-alist)))
			(not (cl-check-test cl-item (cdr (car cl-alist))))))
	  (setq cl-alist (cdr cl-alist)))
	(and cl-alist (car cl-alist)))
    (rassq cl-item cl-alist)))

;;;###autoload
(defun cl::rassoc-if (cl-pred cl-list &rest cl-keys)
  "Find the first item whose cdr satisfies PREDICATE in LIST.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (apply 'cl::rassoc* nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl::rassoc-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item whose cdr does not satisfy PREDICATE in LIST.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (apply 'cl::rassoc* nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl::union (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (cond ((null cl-list1) cl-list2) ((null cl-list2) cl-list1)
	((equal cl-list1 cl-list2) cl-list1)
	(t
	 (or (>= (length cl-list1) (length cl-list2))
	     (setq cl-list1 (prog1 cl-list2 (setq cl-list2 cl-list1))))
	 (while cl-list2
	   (if (or cl-keys (numberp (car cl-list2)))
	       (setq cl-list1 (apply 'adjoin (car cl-list2) cl-list1 cl-keys))
	     (or (memq (car cl-list2) cl-list1)
		 (push (car cl-list2) cl-list1)))
	   (pop cl-list2))
	 cl-list1)))

;;;###autoload
(defun cl::nunion (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (cond ((null cl-list1) cl-list2) ((null cl-list2) cl-list1)
	(t (apply 'cl::union cl-list1 cl-list2 cl-keys))))

;;;###autoload
(defun cl::intersection (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-intersection operation.
The result list contains all items that appear in both LIST1 and LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (and cl-list1 cl-list2
       (if (equal cl-list1 cl-list2) cl-list1
           (cl-parsing-keywords (:key) (:test :test-not)
                                    (let ((cl-res nil))
                                      (or (>= (length cl-list1) (length cl-list2))
                                          (setq cl-list1 (prog1 cl-list2 (setq cl-list2 cl-list1))))
                                      (while cl-list2
                                        (if (if (or cl-keys (numberp (car cl-list2)))
                                                (apply 'cl::member* (cl-check-key (car cl-list2))
                                                       cl-list1 cl-keys)
                                                (memq (car cl-list2) cl-list1))
                                            (push (car cl-list2) cl-res))
                                        (pop cl-list2))
                                      cl-res)))))

;;;###autoload
(defun cl::nintersection (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-intersection operation.
The result list contains all items that appear in both LIST1 and LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (and cl-list1 cl-list2 (apply 'cl::intersection cl-list1 cl-list2 cl-keys)))

;;;###autoload
(defun cl::set-difference (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-difference operation.
The result list contains all items that appear in LIST1 but not LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (if (or (null cl-list1) (null cl-list2)) cl-list1
      (cl-parsing-keywords (:key) (:test :test-not)
      (let ((cl-res nil))
	(while cl-list1
	  (or (if (or cl-keys (numberp (car cl-list1)))
		  (apply 'cl::member* (cl-check-key (car cl-list1))
			 cl-list2 cl-keys)
		(memq (car cl-list1) cl-list2))
	      (push (car cl-list1) cl-res))
	  (pop cl-list1))
	cl-res))))

;;;###autoload
(defun cl::nset-difference (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-difference operation.
The result list contains all items that appear in LIST1 but not LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (if (or (null cl-list1) (null cl-list2)) cl-list1
    (apply 'cl::set-difference cl-list1 cl-list2 cl-keys)))

;;;###autoload
(defun cl::set-exclusive-or (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-exclusive-or operation.
The result list contains all items that appear in exactly one of LIST1, LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (cond ((null cl-list1) cl-list2) ((null cl-list2) cl-list1)
	((equal cl-list1 cl-list2) nil)
	(t (append (apply 'cl::set-difference cl-list1 cl-list2 cl-keys)
		   (apply 'cl::set-difference cl-list2 cl-list1 cl-keys)))))

;;;###autoload
(defun cl::nset-exclusive-or (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-exclusive-or operation.
The result list contains all items that appear in exactly one of LIST1, LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (cond ((null cl-list1) cl-list2) ((null cl-list2) cl-list1)
	((equal cl-list1 cl-list2) nil)
	(t (nconc (apply 'cl::nset-difference cl-list1 cl-list2 cl-keys)
		  (apply 'cl::nset-difference cl-list2 cl-list1 cl-keys)))))

;;;###autoload
(defun cl::subsetp (cl-list1 cl-list2 &rest cl-keys)
  "Return true if LIST1 is a subset of LIST2.
I.e., if every element of LIST1 also appears in LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (cond ((null cl-list1) t) ((null cl-list2) nil)
	((equal cl-list1 cl-list2) t)
	(t (cl-parsing-keywords (:key) (:test :test-not)
	     (while (and cl-list1
			 (apply 'cl::member* (cl-check-key (car cl-list1))
				cl-list2 cl-keys))
	       (pop cl-list1))
	     (null cl-list1)))))

;;;###autoload
(defun cl::subst-if (cl-new cl-pred cl-tree &rest cl-keys)
  "Substitute NEW for elements matching PREDICATE in TREE (non-destructively).
Return a copy of TREE with all matching elements replaced by NEW.
\nKeywords supported:  :key
\n(fn NEW PREDICATE TREE [KEYWORD VALUE]...)"
  (apply 'cl::sublis (list (cons nil cl-new)) cl-tree :if cl-pred cl-keys))

;;;###autoload
(defun cl::subst-if-not (cl-new cl-pred cl-tree &rest cl-keys)
  "Substitute NEW for elts not matching PREDICATE in TREE (non-destructively).
Return a copy of TREE with all non-matching elements replaced by NEW.
\nKeywords supported:  :key
\n(fn NEW PREDICATE TREE [KEYWORD VALUE]...)"
  (apply 'cl::sublis (list (cons nil cl-new)) cl-tree :if-not cl-pred cl-keys))

;;;###autoload
(defun cl::nsubst (cl-new cl-old cl-tree &rest cl-keys)
  "Substitute NEW for OLD everywhere in TREE (destructively).
Any element of TREE which is `eql' to OLD is changed to NEW (via a call
to `setcar').
\nKeywords supported:  :test :test-not :key
\n(fn NEW OLD TREE [KEYWORD VALUE]...)"
  (apply 'cl::nsublis (list (cons cl-old cl-new)) cl-tree cl-keys))

;;;###autoload
(defun cl::nsubst-if (cl-new cl-pred cl-tree &rest cl-keys)
  "Substitute NEW for elements matching PREDICATE in TREE (destructively).
Any element of TREE which matches is changed to NEW (via a call to `setcar').
\nKeywords supported:  :key
\n(fn NEW PREDICATE TREE [KEYWORD VALUE]...)"
  (apply 'cl::nsublis (list (cons nil cl-new)) cl-tree :if cl-pred cl-keys))

;;;###autoload
(defun cl::nsubst-if-not (cl-new cl-pred cl-tree &rest cl-keys)
  "Substitute NEW for elements not matching PREDICATE in TREE (destructively).
Any element of TREE which matches is changed to NEW (via a call to `setcar').
\nKeywords supported:  :key
\n(fn NEW PREDICATE TREE [KEYWORD VALUE]...)"
  (apply 'cl::nsublis (list (cons nil cl-new)) cl-tree :if-not cl-pred cl-keys))

;;; ==============================
;;; :NOTE side-effect-and-error-free in cl-macs.el
;;;###autoload
(defun cl::sublis (cl-alist cl-tree &rest cl-keys)
  "Perform substitutions indicated by ALIST in TREE (non-destructively).
Return a copy of TREE with all matching elements replaced.
\nKeywords supported:  :test :test-not :key
\n(fn ALIST TREE [KEYWORD VALUE]...)"
  (cl-parsing-keywords (:test :test-not :key :if :if-not) ()
    (cl::cl-sublis-rec cl-tree)))

(defvar cl::alist)
(defun cl::cl-sublis-rec (cl-tree)   ; uses cl-alist/key/test*/if*
  (let ((cl-temp (cl-check-key cl-tree)) (cl-p cl::alist))
    (while (and cl-p (not (cl-check-test-nokey (car (car cl-p)) cl-temp)))
      (setq cl-p (cdr cl-p)))
    (if cl-p (cdr (car cl-p))
      (if (consp cl-tree)
	  (let ((cl-a (cl::cl-sublis-rec (car cl-tree)))
		(cl-d (cl::cl-sublis-rec (cdr cl-tree))))
	    (if (and (eq cl-a (car cl-tree)) (eq cl-d (cdr cl-tree)))
		cl-tree
	      (cons cl-a cl-d)))
	cl-tree))))

;;;###autoload
(defun cl::nsublis (cl::alist cl-tree &rest cl-keys)
  "Perform substitutions indicated by ALIST in TREE (destructively).
Any matching element of TREE is changed via a call to `setcar'.
\nKeywords supported:  :test :test-not :key
\n(fn ALIST TREE [KEYWORD VALUE]...)"
  (cl-parsing-keywords (:test :test-not :key :if :if-not) ()
    (let ((cl-hold (list cl-tree)))
      (cl::cl-nsublis-rec cl-hold)
      (car cl-hold))))

(defun cl::cl-nsublis-rec (cl-tree)   ; uses cl-alist/temp/p/key/test*/if*
  (while (consp cl-tree)
    (let ((cl-temp (cl-check-key (car cl-tree))) (cl-p cl::alist))
      (while (and cl-p (not (cl-check-test-nokey (car (car cl-p)) cl-temp)))
	(setq cl-p (cdr cl-p)))
      (if cl-p (setcar cl-tree (cdr (car cl-p)))
	(if (consp (car cl-tree)) (cl::cl-nsublis-rec (car cl-tree))))
      (setq cl-temp (cl-check-key (cdr cl-tree)) cl-p cl::alist)
      (while (and cl-p (not (cl-check-test-nokey (car (car cl-p)) cl-temp)))
	(setq cl-p (cdr cl-p)))
      (if cl-p
	  (progn (setcdr cl-tree (cdr (car cl-p))) (setq cl-tree nil))
	(setq cl-tree (cdr cl-tree))))))

;;;###autoload
(defun cl::tree-equal (cl-x cl-y &rest cl-keys)
  "Return t if trees TREE1 and TREE2 have `eql' leaves.
Atoms are compared by `eql'; cons cells are compared recursively.
\nKeywords supported:  :test :test-not :key
\n(fn TREE1 TREE2 [KEYWORD VALUE]...)"
  (cl-parsing-keywords (:test :test-not :key) ()
    (cl::cl-tree-equal-rec cl-x cl-y)))

(defun cl::cl-tree-equal-rec (cl-x cl-y)
  (while (and (consp cl-x) (consp cl-y)
	      (cl::cl-tree-equal-rec (car cl-x) (car cl-y)))
    (setq cl-x (cdr cl-x) cl-y (cdr cl-y)))
  (and (not (consp cl-x)) (not (consp cl-y)) (cl-check-match cl-x cl-y)))

;;; ==============================
;;; CL.EL
;;; ==============================


;;; ==============================
;; `cl-set-substring' -> cl.el
(defun cl::cl-set-substring (str start end val)
  (if end (if (< end 0) (incf end (length str)))
    (setq end (length str)))
  (if (< start 0) (incf start (length str)))
  (concat (and (> start 0) (substring str 0 start))
	  val
	  (and (< end (length str)) (substring str end))))

;; :NOTE Looks like `cl-old-macroexpand' is used once as a temp place holder.
;;; (defvar cl::cl-old-macroexpand 
;;;   (prog1 (symbol-function 'macroexpand)
;;;     (defalias 'cl::macroexpand 'cl::cl-macroexpand)))

;;; ==============================
;; `cl-macroexpand' -> cl.el
(defun cl::cl-macroexpand (cl-macro &optional cl-env)
  "Return result of expanding macros at top level of FORM.
If FORM is not a macro call, it is returned unchanged.
Otherwise, the macro is expanded and the expansion is considered
in place of FORM.  When a non-macro-call results, it is returned.

The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation.
\n(fn FORM &optional ENVIRONMENT)"
  (let ((cl-macro-environment cl-env))
    (while (progn (setq cl-macro (funcall cl-old-macroexpand cl-macro cl-env))
		  (and (symbolp cl-macro)
		       (cdr (assq (symbol-name cl-macro) cl-env))))
      (setq cl-macro (cadr (assq (symbol-name cl-macro) cl-env))))
    cl-macro))

;;; ==============================
;;; `cl-compiling-file' -> cl.el
(defun cl::cl-compiling-file ()
  ;; :NOTE `cl-compiling-file' is a variable in cl.el before it is a function.
  ;; IOW, don't mak it cl::compiling-file
  (or cl-compiling-file 
      (and (boundp 'outbuffer) (bufferp (symbol-value 'outbuffer))
	   (equal (buffer-name (symbol-value 'outbuffer))
		  " *Compiler Output*"))))

;;; ==============================
;;; `proclaim' -> cl.el
(defun cl::proclaim (spec)
  (if (fboundp 'cl::cl-do-proclaim) (cl::cl-do-proclaim spec t)
      (push spec cl-proclaims-deferred)) ;<- `cl-proclaims-deferred' <VARIABLE>
  nil)

;;; ==============================
;;; `cl-random-time' -> cl.el
(defun cl::cl-random-time ()
  (let* ((time (copy-sequence (current-time-string))) (i (length time)) (v 0))
    (while (>= (decf i) 0) (setq v (+ (* v 3) (aref time i))))
    v))

;;; ==============================
;;; `list*' -> cl.el
(defun cl::list* (arg &rest rest)   ; See compiler macro in cl-macs.el
  "Return a new list with specified ARGs as elements, consed to last ARG.
Thus, `(list* A B C D)' is equivalent to `(nconc (list A B C) D)', or to
`(cons A (cons B (cons C D)))'.
\n(fn ARG...)"
  (cond ((not rest) arg)
	((not (cdr rest)) (cons arg (car rest)))
	(t (let* ((n (length rest))
		  (copy (copy-sequence rest))
		  (last (nthcdr (- n 2) copy)))
	     (setcdr last (car (cdr last)))
	     (cons arg copy)))))

;;; `ldiff' -> cl.el
;;;###autoload
(defun cl::ldiff (list sublist)
  "Return a copy of LIST with the tail SUBLIST removed."
  (let ((res nil))
    (while (and (consp list) (not (eq list sublist)))
      (push (pop list) res))
    (nreverse res)))

;;; ==============================
;;; :NOTE inline in cl-macs.el
;;; :NOTE side-effect-and-error-free in cl-macs.el
;;; `floatp-safe' -> :FILE cl.el
(defun cl::floatp-safe (object)
  "Return t if OBJECT is a floating point number.
On Emacs versions that lack floating-point support, this function
always returns nil."
  (and (numberp object) (not (integerp object))))

;;; ==============================
;;; `plusp' -> :FILE cl.el
(defun cl::plusp (number)
  "Return t if NUMBER is positive."
  (> number 0))

;;; ==============================
;;; `minusp' -> :FILE cl.el
(defun cl::minusp (number)
  "Return t if NUMBER is negative."
  (< number 0))

;;; ==============================
;;; :NOTE side-effect-free in cl-macs.el
;;; `oddp' -> :FILE cl.el
(defun cl::oddp (integer)
  "Return t if INTEGER is odd."
  (eq (logand integer 1) 1))

;;; ==============================
;;; :NOTE side-effect-free in cl-macs.el
;;; `evenp' -> :FILE cl.el
(defun cl::evenp (integer)
  "Return t if INTEGER is even."
  (eq (logand integer 1) 0))

;;; ==============================
;;; `subst' -> :FILE cl.el 
(defun cl::subst (cl-new cl-old cl-tree &rest cl-keys)
  "Substitute NEW for OLD everywhere in TREE (non-destructively).
Return a copy of TREE with all elements `eql' to OLD replaced by NEW.
\nKeywords supported:  :test :test-not :key
\n(fn NEW OLD TREE [KEYWORD VALUE]...)"
  (if (or cl-keys (and (numberp cl-old) (not (integerp cl-old))))
      (apply 'cl::sublis (list (cons cl-old cl-new)) cl-tree cl-keys)
    (cl::cl-do-subst cl-new cl-old cl-tree)))

;;; ==============================
;;; `cl::cl-do-subst'-> :FILE cl.el 
(defun cl::cl-do-subst (cl-new cl-old cl-tree)
  (cond ((eq cl-tree cl-old) cl-new)
	((consp cl-tree)
	 (let ((a (cl::cl-do-subst cl-new cl-old (car cl-tree)))
	       (d (cl::cl-do-subst cl-new cl-old (cdr cl-tree))))
	   (if (and (eq a (car cl-tree)) (eq d (cdr cl-tree)))
	       cl-tree (cons a d))))
	(t cl-tree)))

;;; ==============================
;;; `copy-list'  -> :FILE cl.el 
(defun cl::copy-list (list)
  "Return a copy of LIST, which may be a dotted list.
The elements of LIST are not copied, just the list structure itself."
  (if (consp list)
      (let ((res nil))
	(while (consp list) (push (pop list) res))
	(prog1 (nreverse res) (setcdr res list)))
    (car list)))

;;; ==============================
;;; :NOTE inline in cl-macs.el
;;; :NOTE side-effect-and-error-free in cl-macs.el
;;; `acons' -> :FILE cl.el 
(defun cl::acons (key value alist)
  "Add KEY and VALUE to ALIST.
Return a new list with (cons KEY VALUE) as car and ALIST as cdr."
  (cons (cons key value) alist))

;;; ==============================
;;; :NOTE side-effect-and-error-free in cl-macs.el
;; `pairlis' -> :FILE cl.el
(defun cl::pairlis (keys values &optional alist)
  "Make an alist from KEYS and VALUES.
Return a new alist composed by associating KEYS to corresponding VALUES;
the process stops as soon as KEYS or VALUES run out.
If ALIST is non-nil, the new pairs are prepended to it."
  (nconc (cl::mapcar* 'cons keys values) alist))

;;; ==============================
;; `mapcar*' -> :FILE cl.el 
(defun cl::mapcar* (cl-func cl-x &rest cl-rest)
  "Apply FUNCTION to each element of SEQ, and make a list of the results.
If there are several SEQs, FUNCTION is called with that many arguments,
and mapping stops as soon as the shortest list runs out.  With just one
SEQ, this is like `mapcar'.  With several, it is like the Common Lisp
`mapcar' function extended to arbitrary sequence types.
\n(fn FUNCTION SEQ...)"
  (if cl-rest
      (if (or (cdr cl-rest) (nlistp cl-x) (nlistp (car cl-rest)))
	  (cl::cl-mapcar-many cl-func (cons cl-x cl-rest))
	(let ((cl-res nil) (cl-y (car cl-rest)))
	  (while (and cl-x cl-y)
	    (push (funcall cl-func (pop cl-x) (pop cl-y)) cl-res))
	  (nreverse cl-res)))
    (mapcar cl-func cl-x)))

;;; ==============================
;;; CL-EXTRA.EL
;;; ==============================

;;; ==============================
;;; :NOTE side-effect-free in cl-macs.el
;;; `subseq' -> :FILE cl-extra.el
;;;###autoload
(defun cl::subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (if (stringp seq) (substring seq start end)
    (let (len)
      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
      (if (< start 0) (setq start (+ start (or len (setq len (length seq))))))
      (cond ((listp seq)
	     (if (> start 0) (setq seq (nthcdr start seq)))
	     (if end
		 (let ((res nil))
		   (while (>= (setq end (1- end)) start)
		     (push (pop seq) res))
		   (nreverse res))
	       (copy-sequence seq)))
	    (t
	     (or end (setq end (or len (length seq))))
	     (let ((res (make-vector (max (- end start) 0) nil))
		   (i 0))
	       (while (< start end)
		 (aset res i (aref seq start))
		 (setq i (1+ i) start (1+ start)))
	       res))))))

;;; ==============================
;;; `coerce' -> :FILE cl-extra.el
;;;###autoload
(defun cl::coerce (x type)
  "Coerce OBJECT to type TYPE.
TYPE is a Common Lisp type specifier.
\n(fn OBJECT TYPE)"
  (cond ((eq type 'list) (if (listp x) x (append x nil)))
	((eq type 'vector) (if (vectorp x) x (vconcat x)))
	((eq type 'string) (if (stringp x) x (concat x)))
	((eq type 'array) (if (arrayp x) x (vconcat x)))
	((and (eq type 'character) (stringp x) (= (length x) 1)) (aref x 0))
	((and (eq type 'character) (symbolp x)) (cl::coerce (symbol-name x) type))
	((eq type 'float) (float x))
	((cl::typep x type) x)
	(t (error "Can't coerce %s to type %s" x type))))

;;; ==============================
;;; :NOTE `typep' is a `define-compiler-macro' in cl-macs.el
;;; `typep' -> :FILE cl-macs.el
;;;###autoload
(defun cl::typep (object type)   ; See compiler macro below.
  "Check that OBJECT is of type TYPE.
TYPE is a Common Lisp-style type specifier."
  (eval (cl::cl-make-type-test 'object type)))

;;; ==============================
;;; Numbers.
;; `gcd' -> :FILE cl-extra.el
;;;###autoload
(defun cl::gcd (&rest args)
  "Return the greatest common divisor of the arguments."
  (let ((a (abs (or (pop args) 0))))
    (while args
      (let ((b (abs (pop args))))
	(while (> b 0) (setq b (% a (setq a b))))))
    a))

;;; ==============================
;;;###autoload
;; `lcm' -> :FILE cl-extra.el
(defun cl::lcm (&rest args)
  "Return the least common multiple of the arguments."
  (if (memq 0 args)
      0
    (let ((a (abs (or (pop args) 1))))
      (while args
	(let ((b (abs (pop args))))
	  (setq a (* (/ a (cl::gcd a b)) b))))
      a)))

;;; ==============================
;;;###autoload
;; `isqrt' -> :FILE cl-extra.el
(defun cl::isqrt (x)
  "Return the integer square root of the argument."
  (if (and (integerp x) (> x 0))
      (let ((g (cond ((<= x 100) 10) ((<= x 10000) 100)
		     ((<= x 1000000) 1000) (t x)))
	    g2)
	(while (< (setq g2 (/ (+ g (/ x g)) 2)) g)
	  (setq g g2))
	g)
    (if (eq x 0) 0 (signal 'arith-error nil))))

;;; ==============================
;;;###autoload
;; `floor*' -> :FILE cl-extra.el
(defun cl::floor* (x &optional y)
  "Return a list of the floor of X and the fractional part of X.
With two arguments, return floor and remainder of their quotient."
  (let ((q (floor x y)))
    (list q (- x (if y (* y q) q)))))

;;; ==============================
;;;###autoload
;; `ceiling*' -> :FILE cl-extra.el
(defun cl::ceiling* (x &optional y)
  "Return a list of the ceiling of X and the fractional part of X.
With two arguments, return ceiling and remainder of their quotient."
  (let ((res (cl::floor* x y)))
    (if (= (car (cdr res)) 0) res
      (list (1+ (car res)) (- (car (cdr res)) (or y 1))))))

;;; ==============================
;;;###autoload
;; `truncate*' -> :FILE cl-extra.el
(defun cl::truncate* (x &optional y)
  "Return a list of the integer part of X and the fractional part of X.
With two arguments, return truncation and remainder of their quotient."
  (if (eq (>= x 0) (or (null y) (>= y 0)))
      (cl::floor* x y) (cl::ceiling* x y)))

;;; ==============================
;;;###autoload
;; `round*' -> :FILE cl-extra.el
(defun cl::round* (x &optional y)
  "Return a list of X rounded to the nearest integer and the remainder.
With two arguments, return rounding and remainder of their quotient."
  (if y
      (if (and (integerp x) (integerp y))
	  (let* ((hy (/ y 2))
		 (res (cl::floor* (+ x hy) y)))
	    (if (and (= (car (cdr res)) 0)
		     (= (+ hy hy) y)
		     (/= (% (car res) 2) 0))
		(list (1- (car res)) hy)
	      (list (car res) (- (car (cdr res)) hy))))
	(let ((q (round (/ x y))))
	  (list q (- x (* q y)))))
    (if (integerp x) (list x 0)
      (let ((q (round x)))
	(list q (- x q))))))

;;; ==============================
;;;###autoload
;; `mod*' -> :FILE cl-extra.el
(defun cl::mod* (x y)
  "The remainder of X divided by Y, with the same sign as Y."
  (nth 1 (cl::floor* x y)))

;;; ==============================
;;;###autoload
;; `rem*' -> :FILE cl-extra.el
(defun cl::rem* (x y)
  "The remainder of X divided by Y, with the same sign as X."
  (nth 1 (cl::truncate* x y)))

;;; ==============================
;;;###autoload
;; `signum' -> :FILE cl-extra.el
(defun cl::signum (x)
  "Return 1 if X is positive, -1 if negative, 0 if zero."
  (cond ((> x 0) 1) ((< x 0) -1) (t 0)))

;;; ==============================
;;; (declare-function cl-mapcar-many "cl-extra" (cl-func cl-seqs))
;; `cl-mapcar-many' -> :FILE cl-extra.el
;;;###autoload
(defun cl::cl-mapcar-many (cl-func cl-seqs)
  (if (cdr (cdr cl-seqs))
      (let* ((cl-res nil)
	     (cl-n (apply 'min (mapcar 'length cl-seqs)))
	     (cl-i 0)
	     (cl-args (copy-sequence cl-seqs))
	     cl-p1 cl-p2)
	(setq cl-seqs (copy-sequence cl-seqs))
	(while (< cl-i cl-n)
	  (setq cl-p1 cl-seqs cl-p2 cl-args)
	  (while cl-p1
	    (setcar cl-p2
		    (if (consp (car cl-p1))
			(prog1 (car (car cl-p1))
			  (setcar cl-p1 (cdr (car cl-p1))))
		      (aref (car cl-p1) cl-i)))
	    (setq cl-p1 (cdr cl-p1) cl-p2 (cdr cl-p2)))
	  (push (apply cl-func cl-args) cl-res)
	  (setq cl-i (1+ cl-i)))
	(nreverse cl-res))
    (let ((cl-res nil)
	  (cl-x (car cl-seqs))
	  (cl-y (nth 1 cl-seqs)))
      (let ((cl-n (min (length cl-x) (length cl-y)))
	    (cl-i -1))
	(while (< (setq cl-i (1+ cl-i)) cl-n)
	  (push (funcall cl-func
			    (if (consp cl-x) (pop cl-x) (aref cl-x cl-i))
			    (if (consp cl-y) (pop cl-y) (aref cl-y cl-i)))
		   cl-res)))
      (nreverse cl-res))))

;;; ==============================
;; `map' -> cl-extra.el
;;;###autoload
(defun cl::map (cl-type cl-func cl-seq &rest cl-rest)
  "Map a FUNCTION across one or more SEQUENCEs, returning a sequence.
TYPE is the sequence type to return.
\n(fn TYPE FUNCTION SEQUENCE...)"
  (let ((cl-res (apply 'cl::mapcar* cl-func cl-seq cl-rest)))
    (and cl-type (cl::coerce cl-res cl-type))))

;;; ==============================
;;; `cl::maplist' -> cl-extra.el
;;;###autoload
(defun cl::maplist (cl-func cl-list &rest cl-rest)
  "Map FUNCTION to each sublist of LIST or LISTs.
Like `mapcar', except applies to lists and their cdr's rather than to
the elements themselves.
\n(fn FUNCTION LIST...)"
  (if cl-rest
      (let ((cl-res nil)
	    (cl-args (cons cl-list (copy-sequence cl-rest)))
	    cl-p)
	(while (not (memq nil cl-args))
	  (push (apply cl-func cl-args) cl-res)
	  (setq cl-p cl-args)
	  (while cl-p (setcar cl-p (cdr (pop cl-p)) )))
	(nreverse cl-res))
    (let ((cl-res nil))
      (while cl-list
	(push (funcall cl-func cl-list) cl-res)
	(setq cl-list (cdr cl-list)))
      (nreverse cl-res))))

;;; ==============================
;;; `cl-mapc' -> cl-extra.el
(defun cl::cl-mapc (cl-func cl-seq &rest cl-rest)
  "Like `mapcar', but does not accumulate values returned by the function.
\n(fn FUNCTION SEQUENCE...)"
  (if cl-rest
      (progn (apply 'cl::map nil cl-func cl-seq cl-rest)
	     cl-seq)
    (mapc cl-func cl-seq)))

;;; ==============================
;; `mapl' -> cl-extra.el
;;;###autoload
(defun cl::mapl (cl-func cl-list &rest cl-rest)
  "Like `maplist', but does not accumulate values returned by the function.
\n(fn FUNCTION LIST...)"
  (if cl-rest
      (apply 'cl::maplist cl-func cl-list cl-rest)
    (let ((cl-p cl-list))
      (while cl-p (funcall cl-func cl-p) (setq cl-p (cdr cl-p)))))
  cl-list)

;;; ==============================
;; `mapcan' -> cl-extra.el
;;;###autoload
(defun cl::mapcan (cl-func cl-seq &rest cl-rest)
  "Like `mapcar', but nconc's together the values returned by the function.
\n(fn FUNCTION SEQUENCE...)"
  (apply 'nconc (apply 'cl::mapcar* cl-func cl-seq cl-rest)))

;;; ==============================
;; `mapcon' -> cl-extra.el
;;;###autoload
(defun cl::mapcon (cl-func cl-list &rest cl-rest)
  "Like `maplist', but nconc's together the values returned by the function.
\n(fn FUNCTION LIST...)"
  (apply 'nconc (apply 'cl::maplist cl-func cl-list cl-rest)))

;;; ==============================
;; `some' -> cl-extra.el
;;;###autoload
(defun cl::some (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is true of any element of SEQ or SEQs.
If so, return the true (non-nil) value returned by PREDICATE.
\n(fn PREDICATE SEQ...)"
  (if (or cl-rest (nlistp cl-seq))
      (catch 'cl-some-mon
	(apply 'cl::map nil
	       (function (lambda (&rest cl-x)
			   (let ((cl-res (apply cl-pred cl-x)))
			     (if cl-res (throw 'cl-some-mon cl-res)))))
	       cl-seq cl-rest) nil)
    (let ((cl-x nil))
      (while (and cl-seq (not (setq cl-x (funcall cl-pred (pop cl-seq))))))
      cl-x)))

;;; ==============================
;; `every' -> cl-extra.el
;;;###autoload
(defun cl::every (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is true of every element of SEQ or SEQs.
\n(fn PREDICATE SEQ...)"
  (if (or cl-rest (nlistp cl-seq))
      (catch 'cl-every-mon
	(apply 'cl::map nil
	       (function (lambda (&rest cl-x)
			   (or (apply cl-pred cl-x) (throw 'cl-every-mon nil))))
	       cl-seq cl-rest) t)
    (while (and cl-seq (funcall cl-pred (car cl-seq)))
      (setq cl-seq (cdr cl-seq)))
    (null cl-seq)))

;;; ==============================
;; `notany'  -> cl-extra.el
;;;###autoload
(defun cl::notany (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is false of every element of SEQ or SEQs.
\n(fn PREDICATE SEQ...)"
  (not (apply 'cl::some cl-pred cl-seq cl-rest)))

;;; ==============================
;; `notevery' -> cl-extra.el
;;;###autoload
(defun cl::notevery (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is false of some element of SEQ or SEQs.
\n(fn PREDICATE SEQ...)"
  (not (apply 'cl::every cl-pred cl-seq cl-rest)))

;;; ==============================
;; `concatenate' -> cl-extra.el
;;;###autoload
(defun cl::concatenate (type &rest seqs)
  "Concatenate, into a sequence of type TYPE, the argument SEQUENCEs.
\n(fn TYPE SEQUENCE...)"
  (cond ((eq type 'vector) (apply 'vconcat seqs))
	((eq type 'string) (apply 'concat seqs))
	((eq type 'list) (apply 'append (append seqs '(nil))))
	(t (error "Not a sequence type name: %s" type))))

;;; ==============================
;; `revappend' -> cl-extra.el
;;;###autoload
(defun cl::revappend (x y)
  "Equivalent to (append (reverse X) Y)."
  (nconc (reverse x) y))

;;; ==============================
;; `nreconc' -> cl-extra.el
;;;###autoload
(defun cl::nreconc (x y)
  "Equivalent to (nconc (nreverse X) Y)."
  (nconc (nreverse x) y))

;;; ==============================
;; `list-length' -> cl-extra.el
;;;###autoload
(defun cl::list-length (x)
  "Return the length of list X.  Return nil if list is circular."
  (let ((n 0) (fast x) (slow x))
    (while (and (cdr fast) (not (and (eq fast slow) (> n 0))))
      (setq n (+ n 2) fast (cdr (cdr fast)) slow (cdr slow)))
    (if fast (if (cdr fast) nil (1+ n)) n)))

;;; ==============================
;; `tailp' -> cl-extra.el
;;;###autoload
(defun cl::tailp (sublist list)
  "Return true if SUBLIST is a tail of LIST."
  (while (and (consp list) (not (eq sublist list)))
    (setq list (cdr list)))
  (if (numberp sublist) (equal sublist list) (eq sublist list)))

;;; ==============================
;;; :NOTE `get*' has a compiler macro definition that uses `getf'
;; `get*' -> cl-extra.el
;;;###autoload
(defun cl::get* (sym tag &optional def)    ; See compiler macro in cl-macs.el
  "Return the value of SYMBOL's PROPNAME property, or DEFAULT if none.
\n(fn SYMBOL PROPNAME &optional DEFAULT)"
  (or (get sym tag)
      (and def
	   (let ((plist (symbol-plist sym)))
	     (while (and plist (not (eq (car plist) tag)))
	       (setq plist (cdr (cdr plist))))
	     (if plist (car (cdr plist)) def)))))
;;
(define-compiler-macro cl::get* (sym prop &optional def)
  (if def
      (list 'getf (list 'symbol-plist sym) prop def)
    (list 'get sym prop)))

;;--cl-getf-symbol--

;;; ==============================
;; `cl-set-getf' -> cl-extra.el
;;;###autoload
(defun cl::cl-set-getf (plist tag val)
  (let ((p plist))
    (while (and p (not (eq (car p) tag))) (setq p (cdr (cdr p))))
    (if p (progn (setcar (cdr p) val) plist) (cl::list* tag val plist))))

;;; ==============================
;; `cl-do-remf' ->  cl-extra.el
;;;###autoload
(defun cl::cl-do-remf (plist tag)
  (let ((p (cdr plist)))
    (while (and (cdr p) (not (eq (car (cdr p)) tag))) (setq p (cdr (cdr p))))
    (and (cdr p) (progn (setcdr p (cdr (cdr (cdr p)))) t))))

;;; ==============================
;; `cl-remprop' -> cl-extra.el
;;;###autoload
(defun cl::cl-remprop (sym tag)
  "Remove from SYMBOL's plist the property PROPNAME and its value.
\n(fn SYMBOL PROPNAME)"
  (let ((plist (symbol-plist sym)))
    (if (and plist (eq tag (car plist)))
	(progn (setplist sym (cdr (cdr plist))) t)
      (cl::cl-do-remf plist tag))))
;;
;;; (defalias 'cl::remprop 'cl::cl-remprop)

;;; ==============================
;;; :NOTE There prob. isn't any reason to prefix this with `cl::'
;;; Could prob. do `cl::prettyprint'
;; `cl-prettyprint' -> cl-extra.el
(defun cl::cl-prettyprint (form)
  "Insert a pretty-printed rendition of a Lisp FORM in current buffer."
  (let ((pt (point)) last)
    (insert "\n" (prin1-to-string form) "\n")
    (setq last (point))
    (goto-char (1+ pt))
    (while (search-forward "(quote " last t)
      (delete-char -7)
      (insert "'")
      (forward-sexp)
      (delete-char 1))
    (goto-char (1+ pt))
    (cl::cl-do-prettyprint)))

;;; ==============================
;;; :NOTE There prob. isn't any reason to prefix this with `cl::'
;;; Could prob. do `cl::prettyprint'
;;; `cl-do-prettyprint' -> cl-extra.el
(defun cl::cl-do-prettyprint ()
  (skip-chars-forward " ")
  (if (looking-at "(")
      (let ((skip (or (looking-at "((") (looking-at "(prog")
		      (looking-at "(unwind-protect ")
		      (looking-at "(function (")
		      (looking-at "(cl-block-wrapper ")))
	    (two (or (looking-at "(defun ") (looking-at "(defmacro ")))
	    (let (or (looking-at "(let\\*? ") (looking-at "(while ")))
	    (set (looking-at "(p?set[qf] ")))
	(if (or skip let
		(progn
		  (forward-sexp)
		  (and (>= (current-column) 78) (progn (backward-sexp) t))))
	    (let ((nl t))
	      (forward-char 1)
	      (cl::cl-do-prettyprint)
	      (or skip (looking-at ")") (cl::cl-do-prettyprint))
	      (or (not two) (looking-at ")") (cl::cl-do-prettyprint))
	      (while (not (looking-at ")"))
		(if set (setq nl (not nl)))
		(if nl (insert "\n"))
		(lisp-indent-line)
		(cl::cl-do-prettyprint))
	      (forward-char 1))))
    (forward-sexp)))

;;; ==============================
;;; :NOTE we may not need the vars `cl-macroexpand-cmacs', `cl-closure-vars'
;; `cl-macroexpand-all'
(defvar cl::cl-macroexpand-cmacs nil)
(defvar cl::cl-closure-vars nil)
;;;###autoload
(defun cl::cl-macroexpand-all (form &optional env)
  "Expand all macro calls through a Lisp FORM.
This also does some trivial optimizations to make the form prettier."
  ;;
  ;; :NOTE !CAREFUL! 
  ;; `macroexpand' is prog1 aliased via `cl-old-macroexpand' -> `cl-macroexpand' in cl.el.
  ;; Not sure if `cl-macroexpand' is in the environment yet or not (or how)...
  ;; 
  (while (or (not (eq form (setq form (macroexpand form env)))) 
	     (and cl::cl-macroexpand-cmacs
		  (not (eq form (setq form (cl::compiler-macroexpand form)))))))
  (cond ((not (consp form)) form)
	((memq (car form) '(let let*))
	 (if (null (nth 1 form))
	     (cl::cl-macroexpand-all (cons 'progn (cddr form)) env)
	   (let ((letf nil) (res nil) (lets (cadr form))) ;; `letf' <MACRO>
	     (while lets
	       (push (if (consp (car lets))
			    (let ((exp (cl::cl-macroexpand-all (caar lets) env)))
			      (or (symbolp exp) (setq letf t))
			      (cons exp (cl::cl-macroexpand-body (cdar lets) env)))
			  (let ((exp (cl::cl-macroexpand-all (car lets) env)))
			    (if (symbolp exp) exp
			      (setq letf t) (list exp nil)))) res)
	       (setq lets (cdr lets)))
	     (list* (if letf (if (eq (car form) 'let) 'letf 'letf*) (car form))
		    (nreverse res) (cl::cl-macroexpand-body (cddr form) env)))))
	((eq (car form) 'cond)
	 (cons (car form)
	       (mapcar (function (lambda (x) (cl::cl-macroexpand-body x env)))
		       (cdr form))))
	((eq (car form) 'condition-case)
	 (list* (car form) (nth 1 form) (cl::cl-macroexpand-all (nth 2 form) env)
		(mapcar (function
			 (lambda (x)
			   (cons (car x) (cl::cl-macroexpand-body (cdr x) env))))
			(cdddr form)))) ;;  `cdddr' cl.el
	((memq (car form) '(quote function))
	 (if (eq (car-safe (nth 1 form)) 'lambda)
	     (let ((body (cl::cl-macroexpand-body (cddadr form) env))) ;; `cddadr' cl.el
	       (if (and cl-closure-vars (eq (car form) 'function) ;; `cl-closure-vars' tricky -> `lexical-let'
			(cl::cl-expr-contains-any body cl-closure-vars))
		   (let* ((new (mapcar 'cl::gensym cl-closure-vars))
			  (sub (cl::pairlis cl-closure-vars new)) (decls nil))
		     (while (or (stringp (car body))
				(eq (car-safe (car body)) 'interactive))
		       (push (list 'quote (pop body)) decls))
		     (put (car (last cl-closure-vars)) 'used t)
		     (append
		      (list 'list '(quote lambda) '(quote (&rest --cl-rest--)))
		      (cl::sublis sub (nreverse decls))
		      (list
		       (list* 'list '(quote apply)
			      (list 'function
				    (cl::list* 'lambda
					   (append new (cadadr form)) ;; `cadadr' cl.el
					   (cl::sublis sub body)))
			      (nconc (mapcar (function
					      (lambda (x)
						(list 'list '(quote quote) x)))
					     cl-closure-vars)
				     '((quote --cl-rest--)))))))
		 (list (car form) (cl::list* 'lambda (cadadr form) body))))
	   (let ((found (assq (cadr form) env)))
	     (if (and found (ignore-errors
			      (eq (cadr (caddr found)) 'cl-labels-args)))
		 (cl::cl-macroexpand-all (cadr (caddr (cadddr found))) env) ;;`cadddr' cl.el
	       form))))
	((memq (car form) '(defun defmacro))
	 (list* (car form) (nth 1 form) (cl::cl-macroexpand-body (cddr form) env)))
	((and (eq (car form) 'progn) (not (cddr form)))
	 (cl::cl-macroexpand-all (nth 1 form) env))
	((eq (car form) 'setq)
	 (let* ((args (cl::cl-macroexpand-body (cdr form) env)) (p args))
	   (while (and p (symbolp (car p))) (setq p (cddr p)))
	   (if p (cl::cl-macroexpand-all (cons 'setf args)) (cons 'setq args))))
        ((consp (car form))
         (cl::cl-macroexpand-all (list* 'funcall
                                    (list 'function (car form))
                                    (cdr form))
                             env))
	(t (cons (car form) (cl::cl-macroexpand-body (cdr form) env)))))

;;; ==============================
;; `cl-macroexpand-body' -> cl-extra.el
(defun cl::cl-macroexpand-body (body &optional env)
  (mapcar (function (lambda (x) (cl::cl-macroexpand-all x env))) body))

;;; ==============================
;; `cl-prettyexpand' -> cl.extra.el
;;;###autoload
(defun cl-prettyexpand (form &optional full)
  (message "Expanding...")
  (let ((cl::cl-macroexpand-cmacs full) (cl::cl-compiling-file full) ;; `cl-compiling-file' -> cl.el
	(byte-compile-macro-environment nil))
    (setq form (cl::cl-macroexpand-all form
                                       (and (not full) '((block) (eval-when)))))
    (message "Formatting...")
    (prog1 (cl::cl-prettyprint form)
      (message ""))))


;;; ==============================
;;; :CHANGESET 1906
;;; :CREATED <Timestamp: #{2010-06-21T20:31:35-04:00Z}#{10251} - by MON KEY>
(defun mon-cl-compat-loadtime ()
  "Purge `byte-compile-noruntime-functions' of mon-cl-compat symbol-names.\n
Symbols with names having these prefixes are removed.\n
 `cl::.*', `*clean-.*' `*regexp-'\n
Evaluated at loadtime on an eval-after-load form.\n
:SEE-ALSO `mon-help-utils-loadtime', `mon-help-utils-CL-loadtime',
`mon-CL-cln-colon-swap', `mon-check-feature-for-loadtime',
`mon-after-mon-utils-loadtime'.\n►►►"
  (dolist (cl:: byte-compile-noruntime-functions)
    (let ((cl::chk (symbol-name cl::)))
      (when (and (not (null cl::chk))
                 (or (string-match-p "cl::.*"  cl::chk)
                     (string-match-p "\\*clean-" cl::chk)
                     (string-match-p "\\*regexp-" cl::chk)))
        (setq byte-compile-noruntime-functions 
              (delq cl:: byte-compile-noruntime-functions))))))

;;; ==============================
;;; :NOTE Not sure whether to evaluate this or not. 
;;;       Yes, I should be more sure... less cavalier :[
;; (run-hooks 'cl-seq-load-hook)
      
;;; ==============================
(provide 'mon-cl-compat)
;;; ==============================

;; Remove anything that got added to `byte-compile-noruntime-functions'.
(eval-after-load "mon-cl-compat" '(mon-cl-compat-loadtime))

;;; ================================================================
;;; mon-cl-compat.el ends here
;;; EOF
;;;
;;; :NOTE Following is the output from `byte-compile-cl-functions':
;;;
;;; (tree-equal nsublis sublis nsubst-if-not nsubst-if nsubst subst-if-not
;;;  subst-if subsetp nset-exclusive-or set-exclusive-or nset-difference
;;;  set-difference nintersection intersection nunion union rassoc-if-not
;;;  rassoc-if rassoc* assoc-if-not assoc-if assoc* cl-adjoin member-if-not
;;;  member-if member* merge stable-sort sort* search mismatch count-if-not
;;;  count-if count position-if-not position-if position find-if-not find-if
;;;  find nsubstitute-if-not nsubstitute-if nsubstitute substitute-if-not
;;;  substitute-if substitute delete-duplicates remove-duplicates delete-if-not
;;;  delete-if delete* remove-if-not remove-if remove* replace fill reduce
;;;  compiler-macroexpand define-compiler-macro assert check-type typep
;;;  cl-struct-setf-expander defstruct define-modify-macro callf2 callf letf*
;;;  letf rotatef shiftf remf cl-do-pop psetf setf get-setf-method defsetf
;;;  define-setf-method declare the locally multiple-value-setq
;;;  multiple-value-bind lexical-let* lexical-let symbol-macrolet macrolet
;;;  labels flet progv psetq do-all-symbols do-symbols dotimes dolist do* do
;;;  loop return-from return block etypecase typecase ecase case load-time-value
;;;  eval-when destructuring-bind function* defmacro* defun* gentemp gensym
;;;  cl-prettyexpand cl-macroexpand-all cl-hash-table-count cl-hash-table-p
;;;  cl-make-hash-table cl-maphash cl-clrhash cl-remhash cl-puthash cl-gethash
;;;  remprop cl-remprop cl-do-remf cl-set-getf getf get* tailp list-length
;;;  nreconc revappend concatenate subseq cl-float-limits random-state-p
;;;  make-random-state random* signum rem* mod* round* truncate* ceiling* floor*
;;;  isqrt lcm gcd cl-progv-before cl-set-frame-visible-p cl-map-overlays
;;;  cl-map-intervals cl-map-keymap-recursively cl-map-keymap notevery notany
;;;  every some mapcon mapcan mapl maplist map cl-mapcar-many equalp coerce
;;;  cl-hack-byte-compiler pairlis acons cl-do-subst subst adjoin cl-mod
;;;  cl-round cl-truncate cl-ceiling cl-floor cl-member cl-maclisp-member
;;;  copy-list ldiff list* cddddr cdddar cddadr cddaar cdaddr cdadar cdaadr
;;;  cdaaar cadddr caddar cadadr cadaar caaddr caadar caaadr caaaar cdddr cddar
;;;  cdadr cdaar caddr cadar caadr caaar tenth ninth eighth seventh sixth fifth
;;;  fourth third endp rest second first svref mapcar* copy-seq evenp oddp
;;;  minusp plusp floatp-safe cl-random-time declaim proclaim cl-compiling-file
;;;  cl-macroexpand macroexpand nth-value multiple-value-call
;;;  multiple-value-apply multiple-value-list values-list values cl-block-throw
;;;  cl-block-wrapper cl-map-extents cl-set-substring cl-set-buffer-substring
;;;  cl-set-nthcdr cl-set-elt pushnew push pop decf incf cl-unload-function
;;;  cl-defsubst-expand defsubst* cl-byte-compile-compiler-macro
;;;  compiler-macroexpand define-compiler-macro assert check-type typep
;;;  cl-make-type-test deftype cl-struct-setf-expander defstruct
;;;  define-modify-macro callf2 callf letf* letf rotatef shiftf remf cl-do-pop
;;;  psetf setf cl-setf-simple-store-p cl-setf-do-store cl-setf-do-modify
;;;  get-setf-method cl-setf-make-apply defsetf define-setf-expander
;;;  define-setf-method declare cl-do-proclaim the locally multiple-value-setq
;;;  multiple-value-bind cl-defun-expander lexical-let* lexical-let
;;;  symbol-macrolet macrolet labels flet progv psetq do-all-symbols do-symbols
;;;  dotimes dolist cl-expand-do-loop do* do cl-loop-build-ands
;;;  cl-loop-handle-accum cl-loop-let cl-parse-loop-clause loop return-from
;;;  return cl-byte-compile-throw cl-byte-compile-block block etypecase typecase
;;;  ecase case load-time-value cl-compile-time-too eval-when destructuring-bind
;;;  cl-arglist-args cl-do-arglist cl-transform-lambda
;;;  cl-transform-function-property function* defmacro* defun* gentemp gensym
;;;  cl-expr-depends-p cl-expr-contains-any cl-expr-contains
;;;  cl-expr-access-order cl-const-expr-val cl-const-exprs-p cl-const-expr-p
;;;  cl-safe-expr-p cl-simple-exprs-p cl-simple-expr-p cl-pop2)

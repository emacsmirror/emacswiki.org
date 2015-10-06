;;; keyword-arg-macros.el --- Some macros for extracting keyword args from lists

;; Filename: keyword-arg-macros.el
;; Description: Some macros for extracting keyword args from lists
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2015, Joe Bloggs, all rites reversed.
;; Created: 2015-10-06 12:30:53
;; Version: 0.1
;; Last-Updated: 2015-10-06 12:30:53
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/keyword-arg-macros
;; Keywords: extensions
;; Compatibility: GNU Emacs 24.5.1
;; Package-Requires: ((dash "20150829.433") (macro-utils "1.0"))
;;
;; Features that might be required by this library:
;;
;; cl, dash, macro-utils
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1FgnGFwRES9MGzoieRDWLmLkjqMT3AsjQF
;;
;; This library provides some macros for extracting keyword args from lists.
;; It can be helpful in situations where normal cl-defun arglist handling is not enough.
;;
;;;;

;;; Installation:
;;
;; Put keyword-arg-macros.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'keyword-arg-macros)

;;; Require
(eval-when-compile (require 'cl))
(require 'dash)

;;; Code:

;;;###autoload
(defmacro extract-keyword-arg (key lstsym &optional pred)
  "Remove KEY & following item from list referenced by LSTSYM, and return item.
A key here means a symbol whose first character is :
LSTSYM should be a symbol whose value is a list.
If KEY is not in the list then return nil.
If predicate function PRED is supplied then an error will be thrown if
PRED returns nil when supplied with the key value as argument."
  (let ((lst (gensym)))
    `(let* ((,lst (eval ,lstsym))
	    (ind (-elem-index ,key ,lst)))
       (unless (not ind)
	 (let ((val (nth (1+ ind) ,lst)))
	   (set ,lstsym (append (-take ind ,lst) (-drop (+ 2 ind) ,lst)))
	   (if (and ,pred (not (funcall ,pred val)))
	       (error "Invalid value for %S" ,key)
	     val))))))

;;;###autoload
(defmacro extract-keyword-bindings (args &optional check &rest keys)
  "Extract KEYS and corresponding values from ARGS, and return in let-style bindings list.
If ARGS is a symbol referring to a list, then KEYS and corresponding values will be removed from ARGS.
Keys can be given default values by using (:key value) instead of just :key
If CHECK is non-nil then if there are any keys (beginning with :) in ARGS other than those in KEYS 
an error will be thrown."
  (let ((args2 (gensym))
	(args3 (gensym)))
    `(let ((,args2 (if (symbolp ,args) (eval ,args) ,args))
	   (,args3 (if (symbolp ,args) ,args ',args2)))
       (if ,check
	   (let* ((argskeys (-filter (lambda (x) (keywordp x))
				     ,args2))
		  (requiredkeys (mapcar (lambda (x) (if (consp x) (car x) x)) ',keys))
		  (unusedkeys (-difference argskeys requiredkeys)))
	     (if unusedkeys
		 (error "Keyword argument %s not one of %s" (car unusedkeys) ',keys))))
       (cl-loop for pair in ',keys
		for key = (if (consp pair) (car pair) pair)
		for defval = (if (consp pair) (cadr pair))
		collect (list (if (keywordp key)
				  (intern (substring (symbol-name key) 1))
				key)
			      (if (memq key ,args2)
				  (extract-keyword-arg key ,args3)
				defval))))))

;;;###autoload
(defmacro extract-first-keyword-arg (lstsym &optional pred)
  "Remove & return first key & following item from list referenced by LSTSYM.
A key here means a symbol whose first character is :
LSTSYM should be a symbol whose value is a list.
If there are no keys in the list then return nil, otherwise return a cons cell
whose car is the key and whose cdr is the corresponding value.
If predicate function PRED is supplied then an error will be thrown if
PRED returns nil when supplied with the key value as argument."
  (let ((lst (gensym)))
    `(let* ((,lst (eval ,lstsym))
	    (ind (-find-index (lambda (x) (keywordp x)) ,lst)))
       (unless (not ind)
	 (let ((key (nth ind ,lst))
	       (val (nth (1+ ind) ,lst)))
	   (set ,lstsym (append (-take ind ,lst) (-drop (+ 2 ind) ,lst)))
	   (if (and ,pred (not (funcall ,pred val)))
	       (error "Invalid value for %S" key)
	     (cons key val)))))))

;;;###autoload
(defmacro loop-over-keyword-args (lst &rest body)
  "Loop over the keyword args in list LST, evaluating BODY forms each time.
For each iteration of the loop, `key' will be bound to the current keyword,
`value' will be bound to the corresponding value, and `keyvaluepair' will
be bound to a cons cell containing these elements (key & value)."
  `(let ((lstsym ,lst))
     (cl-loop for keyvaluepair = (extract-first-keyword-arg 'lstsym)
	      for key = (car keyvaluepair)
	      for value = (cdr keyvaluepair)
	      while keyvaluepair do (eval '(progn ,@body)))))


(provide 'keyword-arg-macros)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "keyword-arg-macros.el" (buffer-name) (buffer-string) "update")

;;; keyword-arg-macros.el ends here

;;; wlcompose.el --- some helper function that is not defined by default

;; Copyright (C) 2009  

;; Author:  <chunywan@3CNL03982>
;; Keywords: 

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:
(defvar wlfunction-var nil
  "temp variable to avoid warning message")
(defun wlfunction (f)
  (cond
   ((symbolp f) (list 'quote f))
   ((and (symbolp f)
	 (fboundp f)
	 (subrp (symbol-function f))) (list 'quote f))
   ((and (symbolp f)
	 (fboundp f)) (list 'quote f))
   ((functionp f) f)
   (t f)))
(defmacro wlcompose (&rest fsv)
  (let* ((fs (reverse fsv))
	 (args (make-symbol "args"))
	 (form args))
    (if (null fs)
	(error "at least one function needed"))
    (setq form `(apply ,(wlfunction (car fs)) ,args)
	  fs (cdr fs))
    (while fs
      (setq form `(funcall ,(wlfunction (car fs)) ,form)
	    fs (cdr fs)))
    `(lambda (&rest ,args)
       (,@form))))
(defmacro wlpartial-apply (f &rest args)
  "partialy apply the function and return a new function with
fewer arguments"
  (let ((x (make-symbol "x")))
    `(lambda (&rest ,x)
       (apply ,(wlfunction f) ,@args ,x))))
(defmacro wlflip (f)
  (let ((x (make-symbol "x"))
	(y (make-symbol "y")))
    (list 'lambda (list x y)
	  (list 'funcall (wlfunction f) y x))))
(defun wlmap-r (funcs x)
  "FUNCS is a list of functions. apply the functions to X and return a list of
return value"
  (mapcar ($ (wlflip funcall) x) funcs))
(defun wlfind-if (pred a-list)
  "return a list whose car satisfy PRED"
  (while (and a-list (not (funcall pred (car a-list))))
    (setq a-list (cdr a-list)))
  a-list)
(defalias '$ 'wlpartial-apply)
(defalias '\. 'wlcompose)
(provide 'wlcompose)
;;; wlcompose.el ends here

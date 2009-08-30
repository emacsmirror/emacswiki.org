;;; 123-menu.el --- Simple menuing system, reminiscent of Lotus 123 in DOS

;; Copyright (C) 2006  Free Software Foundation, Inc.

;; Author: Marc Abramowitz <http://marc-abramowitz.com>
;; Keywords: convenience

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

;; Even though I've been using Emacs for years, I still have trouble
;; remembering a lot of the key combinations for commands that I don't
;; use all the time (e.g.: killing and yanking rectangles, bookmarks,
;; etc.). I think Lotus 123 for DOS had a remarkably cool user
;; interface in that the menus let you explore around and find things
;; if you were a newbie, but once you knew the key sequences, you
;; could become very fast with it. This is my attempt at doing
;; something like the Lotus 123 menu system for Emacs.

;;; Code:

;; Common Lisp functions
(eval-when-compile (require 'cl))

(defmacro ilam (&rest body)
  `(lambda () (interactive) ,@body))

(defmacro ilamq (&rest body)
  `'(lambda () (interactive) ,@body))

(defvar 123-menu-stack '())

(defmacro 123-menu-make-menu-item (menu key caption &rest rest)
  `(setf ,menu 
	 (append ,menu 
		 (list (list 
			:key       ,key 
			:caption   ,caption 
			:func      ,@rest)))))

(defmacro 123-menu-defmenu (name &rest rest)
  `(progn
     (defconst ,name
       (let ((menu))
;; 	 (message (format "%s has %d entries" ',name (length ',rest)))
	 (dolist (item ',rest)
	   (123-menu-make-menu-item menu (first item) (second item) (eval (third item))))
;; 	 (message (format "menu = %s" menu))
	 menu))
     (fset (intern (concat "123-menu-display-menu-" (symbol-name ',name)))
	   (ilamq (123-menu-display-menu ,name)))
     )
  )

(defun 123-menu-display-menu (menu)
  (interactive)
  (let ((map        (make-sparse-keymap))
	(continue   t)
	(msg        "")
	(c          ""))
    (dolist (item menu) 
      (define-key map (getf item :key) (getf item :func)))
;;       (message (format "caption = %s ; func = %s" (getf item :caption) (getf item :func))))
    (define-key map "~" (ilamq (123-menu-display-menu (elt 123-menu-stack 1))))
    (setf msg (mapconcat '(lambda (x) (getf x :caption)) menu " "))
    (setf 123-menu-stack (push menu 123-menu-stack))
    (while continue
      (message msg)
      (setq c (read-key-sequence nil))
      (setq c (lookup-key map c))
      (cond ((eq c 'Helper-help-options)
	     (Helper-help-options))
	    ((commandp c)
	     (call-interactively c)
	     (setq continue nil))
	    (t
	     (ding)
	     (setq continue nil)))))
  (pop 123-menu-stack))

(provide '123-menu)
;;; 123-menu.el ends here

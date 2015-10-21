;;; ido-choose-function.el --- Use ido to select functions and (partially) apply arguments

;; Filename: ido-choose-function.el
;; Description: Use ido to select functions and (partially) apply arguments
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2015, Joe Bloggs, all rites reversed.
;; Created: 2015-10-21 14:23:27
;; Version: 0.1
;; Last-Updated: 2015-10-21 14:23:27
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/ido-choose-function
;; Keywords: convenience
;; Compatibility: GNU Emacs 24.5.1
;; Package-Requires:  
;;
;; Features that might be required by this library:
;;
;; 
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
;; Bitcoin donations gratefully accepted: 12k9zUo9Dgqk8Rary2cuzyvAQWD5EAuZ4q
;;
;; This files provides a couple of functions `apply-interactive' and `ido-choose-function'
;; which you might find useful in your code.
;; 
;; `apply-interactive' can be used to partially apply arguments to functions, like `apply-partially',
;;                     except that the curried arguments are obtained interactively using the `interactive'
;;                     form of the function. 
;; `ido-choose-function' can be used to prompt the user for a function (e.g. for filtering items),
;;                       and some of it's arguments.
;;;;


;;; Installation:
;;
;; Put ido-choose-function.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'ido-choose-function)

;;; Code:

;;;###autoload
(defun apply-interactive (func)
  "Call the interactive form of FUNC to (partially) apply arguments.
Return value is a function that does the same as FUNC, except that
it's arguments are fixed to the values obtained interactively during
this function call. If FUNC has no `interactive' form then it is
returned unchanged.
Any '<> symbols returned by the `interactive' form in FUNC will be
used as place holders for arguments of the returned function.
Also, if the `interactive' form returns a '&rest symbol this will
be used in the arglist of the returned function.
For example, the following call:

 (apply-interactive
  (lambda (x y &rest z)
    (interactive (list (read-number \"Factor: \") 
		       '<> '&rest '<>))
    (* x (apply '+ y z))))

will prompt for a number, x, and return a function that takes any
number of arguments, adds them together and multiplies the result
by x."
  (let ((interact (interactive-form func)))
    (if interact
	(let* ((args (eval `(call-interactively
			     (lambda (&rest args) ,interact args))))
	       (args2 (mapcar (lambda (x) (if (eq x '<>) (gensym) x))
			      (remove-if-not (lambda (y) (memq y '(<> &rest)))
					     args)))
	       (args3 (remove '&rest args))
	       (args4 (remove '&rest args2))
	       (restp (memq '&rest args2)))
	  ;; Use `eval' rather than `macroexpand' so that function can be called with `funcall'
	  (eval `(lambda ,args2
		   (,@(if restp `(apply ,func) `(,func))
		    ,@(mapcar
		       (lambda (x) (if (eq x '<>) (pop args4) x))
		       args3)))))
      func)))

;;;###autoload
(defvar ido-read-function-history nil
  "A history list of Lisp expressions forf `ido-choose-function'.
Keeps track of Lisp expressions entered by the user, (but not functions
selected from the list).")

;;;###autoload
(defun ido-choose-function (funcs &optional prompt other partial)
  "Prompt the user for one of the functions in FUNCS.
FUNCS should a list of cons cells whose cars are the function names,
 (either strings or symbols), and whose cdrs are the functions themselves.
If PROMPT is non-nil use that as the prompt.
If OTHER is non-nil allow the user to enter a function of their own.
If OTHER is a string, use that as the prompt when asking the user to
enter a function of their own.
If PARTIAL is non-nil then after the function is selected, if it contains
an `interactive' form, it will be called to obtain values for the arguments
which will be partially applied to the function before returning it.
For more details see `apply-interactive'."
  (cl-flet ((asstring (x) (if (symbolp x) (symbol-name x)
			    (if (stringp x) x
			      (error "Invalid element: %S" x)))))
    (let* ((names (mapcar (lambda (x) (asstring (car x))) funcs))
	   (otherstr (if other
			 (if (not (member "other" names))
			     "other"
			   "user function")))
	   (otherprompt (if other
			    (if (stringp other)
				other
			      "User function: ")))
	   (choice (ido-completing-read
		    (or prompt "Function: ")
		    (append (if otherstr (list otherstr)) names)
		    nil nil nil 'ido-functions-history))
	   (func (if (equal choice otherstr)
		     (read-from-minibuffer
		      otherprompt nil nil t 'ido-read-function-history)
		   (cdr (cl-assoc choice funcs
				  :test (lambda (a b) (equal (asstring a)
							     (asstring b))))))))
      (if (not (functionp func))
	  (error "Invalid function: %S" func)
	(apply-interactive func)))))



(provide 'ido-choose-function)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "ido-choose-function.el" (buffer-name) (buffer-string) "update")

;;; ido-choose-function.el ends here

;;; superego.el --- highlight your name

;; Copyright (c) 2002 Michele Bini

;; Author: Michele Bini <mibin@libero.it>
;; Created: 2002-02-15
;; Version: 0.3
;; Keywords: convenience

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; * What is it?

;; This package implements a simple minor mode that enables
;; occurrences of your name (or other arbitrary text) to be
;; highlighted in the current buffer.

;; By default your full name, your login name, your e-mail address and
;; your last and first name individually are highlighted.  You can
;; alter this behavior by setting the superego-regexp variable
;; (either in your .emacs or via M-x customize-variable).

;; * Usage

;; To use it, make sure to load superego in your .emacs or have it
;; auto-loaded on request:

;;     (autoload 'superego-mode "/path/to/superego.el" nil t)

;; Then for each mode you want, add a line like the following:

;;     (add-hook 'text-mode-hook 'superego-mode)
;;     (add-hook 'rmail-mode-hook 'superego-mode)

;; You can also enable the mode interactively in a specific buffer,
;; via M-x superego-mode.

;; * Thanks

;; Benjamin Drieu, author of egocentric.el, which inspired me to write
;; this package.

;;; History:
;; 2002-07-16  Michele Bini  <mibin@libero.it>
;;
;;   * superego.el: added commentary.
;;   (superego-regexp): Highlight last and first name individually.
;;
;; 2002-02-15  Michele Bini  <mibin@libero.it>
;;
;;   * superego.el: Created, inspired by egocentric.el.

;;; Code:

(require 'font-lock)

(defgroup superego nil
  "Highlight your name."
  :group 'convenience :prefix 'superego-)

(defvar superego-face 'superego-face) ;; make font-lock happy
(defface superego-face
  '((t (:underline t :foreground "red" :background "yellow")))
  "Face to use for highlighting your full name."
  :group 'superego)
(defcustom superego-regexp nil
  "Regexp to match your names.
If nil, superego will try to guess it."
  :group 'superego :type '(choice (const :tag "Automatic guess." nil) regexp))
(defcustom superego-mode-hook nil
  "Hook run when superego is activated."
  :group 'superego :type 'hook)
(defcustom superego-mode-line-string " sEgo"
  "String displayed on the mode line when super ego is active."
  :group 'superego :type '(choice (const :tag "No indicator." nil)
				  string))
(or (assq 'superego-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(superego-mode superego-mode-line-string)
		minor-mode-alist)))

(defun superego-regexp ()
  (or superego-regexp
      (setq superego-regexp
	    (regexp-opt
	     (append
	      (list (user-full-name)
		    (user-login-name))
	      (let ((a (split-string (user-full-name))))
		(if (> (length a) 1)
		    (list (car a) (car (reverse a)))))
	      (if (boundp 'user-mail-address)
		  (list user-mail-address) (list)))))))

(defvar superego-mode nil)
(defvar superego-font-lock-keywords nil)
(defvar superego-font-lock nil)

;;;###autoload
(defun superego-mode (&optional arg global)
  "Highlight the occurrences of your name in the current buffer.

With prefix ARG, turn the mode on iff ARG is positive.  When optional
argument GLOBAL is not nil, highlight your name in every buffer.  When
the mode is activated, `superego-mode-hook' is run."
  (interactive "P")
  (let ((enable nil)
	(disable nil))
    (cond
     ((null arg)
      (if superego-mode (setq disable t) (setq enable t)))
     ((> (setq arg (prefix-numeric-value arg)) 0)
      (setq enable t))
     (t (setq disable t)))
    (make-local-variable 'superego-mode)
    (if disable
	(progn
	  (when superego-font-lock-keywords
	    (font-lock-remove-keywords
	     nil superego-font-lock-keywords)
	    (setq superego-font-lock-keywords nil)
	    (if font-lock-mode
		(if superego-font-lock
		    (font-lock-fontify-buffer)
		  (font-lock-mode -1))))
	  (setq superego-mode nil))
      (if enable
	  (progn
	    (let ((a (list (superego-regexp) 0 'superego-face
			   'prepend)))
	      (font-lock-add-keywords nil (list a))
	      (make-local-variable 'superego-font-lock)
	      ;; remember whether font-lock is active
	      (setq superego-font-lock font-lock-mode)
	      (if font-lock-mode
		  (font-lock-fontify-buffer)
		  (font-lock-mode 1))
	      (make-local-variable 'superego-font-lock-keywords)
	      (setq superego-font-lock-keywords
		    (cons a
		     superego-font-lock-keywords)))
	    (setq superego-mode t)
	    (run-hooks 'superego-mode-hook))))))

(provide 'superego)
;;; superego.el ends here

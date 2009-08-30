;;; x-migrant.el --- Support for subprocesses handling remote X displays

;; Copyright (C) 2003 Bryan O'Sullivan

;; Author: Bryan O'Sullivan <bos@serpentine.com>

;; $Id: s.x-migrant.el 1.2 03/04/08 21:14:50-07:00 bos@camp4.serpentine.com $

;; x-migrant.el ("this file") is free software; you can redistribute
;; it and/or modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 2, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file, GNU Emacs, or XEmacs; see the file COPYING
;; (`C-h C-l').  If not, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package advises the Emacs process startup functions, so that
;; they set up the DISPLAY environment variable correctly.  This is
;; very useful if you run the same Emacs session on multiple X
;; displays at once, and sometimes need to pop up an X application
;; from within Emacs.

(require 'advice)

(defconst xm-running-xemacs (string-match "XEmacs" emacs-version)
  "Is x-migrant.el running under XEmacs?")

(defmacro current-x-display-name ()
  "Return the name of the currently active X display, or nil for none."
  (if xm-running-xemacs
      '(if (eq (frame-type) 'x)
	  (device-connection))
    '(frame-parameter nil 'display)))

(defvar default-x-display (or (current-x-display-name) (getenv "DISPLAY"))
  "*The default X display on which to pop up random X windows, or nil for none.
This is for use with the 'with-current-x-display' macro.")

(defmacro with-current-x-display (&rest body)
  "Evaluate BODY with the DISPLAY environment variable set correctly.
In this case, we take 'correct' as being the display on which the
frame that currently has the focus is showing."
  (let ((dpy (gensym "dpy-")))
    `(let* ((,dpy (or ,(current-x-display-name) default-x-display))
	    (process-environment
	     (if ,dpy
		 (mapcar (function (lambda (env)
				     (if (string-match "^DISPLAY=" env)
					 (concat "DISPLAY=" ,dpy)
				       env)))
			 process-environment)
	       process-environment)))
       (progn ,@body))))

(put 'with-current-x-display 'lisp-indent-function 0)

(if xm-running-xemacs
    (progn
      (defadvice start-process-internal (around display-remotely activate)
	"X programs will be run with the correct DISPLAY environment variable."
	(with-current-x-display
	  ad-do-it))
      (defadvice call-process-internal (around display-remotely activate)
	"X programs will be run with the correct DISPLAY environment variable."
	(with-current-x-display
	  ad-do-it)))
  (defadvice start-process (around display-remotely activate)
    "X programs will be run with the correct DISPLAY environment variable."
    (with-current-x-display
      ad-do-it))
  (defadvice call-process (around display-remotely activate)
    "X programs will be run with the correct DISPLAY environment variable."
    (with-current-x-display
      ad-do-it)))

(provide 'x-migrant)


;;; Local Variables:
;;; mode: emacs-lisp
;;; prompt-to-byte-compile: nil
;;; end:
;;; x-migrant.el is free software
;;; x-migrant.el ends here

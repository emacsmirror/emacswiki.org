;;; @(#) w3xt.el -- extended MIME control in w3m
;;; @(#) $Id: w3xt.el,v 1.1.1.1 2006/09/18 11:12:28 cvs Exp $

;; This file is not part of Emacs

;; Copyright (C) 2006 by Alessandro Di Marco
;; Author:          Alessandro Di Marco (dmr@c0nc3pt.com)
;; Maintainer:      Alessandro Di Marco (dmr@c0nc3pt.com)
;; Created:         April 26, 2006
;; Keywords:        w3m mime
;; Latest Version:

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  w3XT lets generic lisp functions to be called when certain MIME types are
;;  encountered. Once `w3m-content-type-alist' is properly configured with a
;;  given file type, you will be able to execute any lisp function on a file of
;;  that type simply clicking on its link.
;;
;;  In the example below we configure w3m to play any remote m3u playlist in emms:
;;
;;  (w3m-content-type-alist
;;   (quote (("audio/x-mpegurl" "\\.m3u\\'"
;;  	  ("'emms-play-m3u-playlist" file)
;;  	  nil))))
;;
;;  Feel free to submit EM3u as a patch to the w3m mainstream if you
;;  think it's worth your while.

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path, then add one of the following
;;  to your ~/.emacs startup file.  You can load w3XT every time you start
;;  Emacs:
;;
;;     (autoload 'w3m "w3xt" nil t)

;;; Usage:
;;
;;  Do you really need help for this?

;;; Known Bugs:
;;
;;  Too simple to have one (hopefully ;-)

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Alessandro Di Marco (dmr@c0nc3pt.com).
;;
;;  This version of EM3u was developed and tested with GNU Emacs 22.0.50.1
;;  under Linux. Please, let me know if it works with other OS and versions of
;;  Emacs.

(require 'w3m)

(eval-when-compile
  (require 'cl))

(eval-when-compile
  (unless (dolist (var nil t))
    ;; Override the `dolist' macro which may be faultily provided by
    ;; old egg.el.
    (load "cl-macs" nil t)))

(defun eval-string (str)
  "Wrap STR in a progn then read it and eval it.
Examples: (eval-string \"(+ 1 2) (* 3 4) ;; this returns 12=3*4\")
          (eval-string \";; this returns nil\")"
  (eval (read (concat "(progn\n" str "\n)"))))

(defsubst w3m-which-command-xt (command)
  (when (stringp command)
    (if (or (string-match "^'" command)
	    (and (file-name-absolute-p command)
		 (file-executable-p command)))
	command
      (setq command (file-name-nondirectory command))
      (catch 'found-command
	(let (bin)
	  (dolist (dir exec-path)
	    (when (or (file-executable-p
		       (setq bin (expand-file-name command dir)))
		      (file-executable-p
		       (setq bin (expand-file-name (concat command ".exe") dir))))
	      (throw 'found-command bin))))))))

(defun w3m-external-view (url &optional no-cache handler)
  (when (w3m-url-valid url)
    (lexical-let ((url url)
		  (no-cache no-cache))
      (w3m-process-do
	  (type (w3m-content-type url no-cache handler))
	(when type
	  (lexical-let ((method (nth 2 (assoc type w3m-content-type-alist))))
	    (cond
	     ((not method)
	      (if (w3m-url-local-p url)
		  (error "No method to view `%s' is
		  registered. Use `w3m-edit-this-url'"
			 (file-name-nondirectory (w3m-url-to-file-name url)))
		(w3m-download url nil no-cache handler)))
	     ((functionp method)
	      (funcall method url))
	     ((consp method)
	      (lexical-let
		  ((command (w3m-which-command-xt (car method)))
		   (arguments (cdr method))
		   (file (make-temp-name
			  (expand-file-name "w3mel" w3m-profile-directory)))
		   suffix)
		(setq suffix (file-name-nondirectory url))
		(when (string-match "\\.[a-zA-Z0-9]+$" suffix)
		  (setq suffix (match-string 0 suffix))
		  (when (< (length suffix) 5)
		    (setq file (concat file suffix))))
		(cond
		 ((and command (memq 'file arguments))
		  (let ((w3m-current-buffer (current-buffer)))
		    (w3m-process-do
			(success (w3m-download url file no-cache handler))
		      (when success
			(if (string-match "^'" command)
			    (eval-string (concat
					  (concat "("
						  (concat (concat (substring command 1) " \"") file))
					  "\")"))
			  (w3m-external-view-file command file url arguments))))))
		 (command
		  (w3m-external-view-file command nil url arguments))
		 (t
		  (w3m-download url nil no-cache handler))))))))))))

(provide 'w3xt)

;;; color-theme-maker.el --- install color themes

;; Copyright (C) 2002  Alex Schroeder <alex@gnu.org>

;; Version: 1.0.1
;; Keywords: faces
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ColorThemeMaker

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This package makes it easier to make subthemes.  If you like the
;; jde faces of theme A, the erc faces from theme B, and generally
;; prefer theme color-theme-gray30, then proceed as follows:

;; Install theme A, call M-x color-theme-maker, provide "jde" as
;; regexp and "my-jde-subtheme" as name, then install theme B, call
;; M-x color-theme-maker, provide "erc" as regexp and
;; "my-erc-subtheme" as name.  Save the two buffers as
;; my-jde-subtheme.el and my-erc-subtheme.el.  In your .emacs, add:
;;
;;   (require 'color-theme)
;;   (color-theme-gray30)
;;   (setq color-theme-is-cumulative t)
;;   (require 'my-jde-subtheme)
;;   (my-jde-subtheme)
;;   (require 'my-erc-subtheme)
;;   (my-erc-subtheme)

;;; Thanks

;; Girish Bharadwaj <girishb@gbvsoft.com> for the idea.

;;; Code:

(require 'widget)
(eval-when-compile
  (require 'wid-edit))
(require 'color-theme)

(defvar color-theme-maker-regexp nil)
(defvar color-theme-maker-name nil)

(defun color-theme-maker ()
  "Interactively create color subthemes.
Based on the faces currently defined, produce a color theme function
that sets only the faces matching a regexp."
  (interactive)
  (switch-to-buffer "*Color Theme Maker*")
  (kill-all-local-variables)
  (make-local-variable 'color-theme-maker-regexp)
  (make-local-variable 'color-theme-maker-name)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (widget-insert
     "Welcome to the Color Theme Maker\n"
     "\n"
     "The Regexp will be used to filter all faces currently defined.\n"
     "\n"
     "Regexp: ")
    (setq color-theme-maker-regexp (widget-create 'editable-field))
    (widget-insert
     "\n\n"
     "The name determines what the resulting color theme function will be called.\n"
     "\n"
     "Name:   ")
    (setq color-theme-maker-name (widget-create 'editable-field))
    (widget-insert "\n")
    (widget-create
     'push-button
     :notify (lambda (&rest ignore)
	       (color-theme-maker-print (widget-value color-theme-maker-regexp)
					(widget-value color-theme-maker-name)))
     "Done"))
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min))
  (widget-forward 1))

(defun color-theme-maker-print (regexp func-name)
  "Create a color theme function FUNC-NAME with faces matching REGEXP."
  (message "Making subtheme...")
  (let ((faces (color-theme-filter (color-theme-get-faces) regexp)))
    (unless faces
      (error "No faces match %s" regexp))
    (let ((buf (get-buffer-create (concat func-name ".el"))))
      (switch-to-buffer buf)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert "(defun " func-name " ()\n"
		"  \"Color subtheme, created " (format-time-string "%Y-%m-%d") ".\"\n"
		"  (interactive)\n"
		"  (color-theme-install\n"
		"   '(" func-name " nil nil")
	(when faces (insert "\n     "))
	(dolist (face faces)
	  (when (= (preceding-char) ?\))
	    (insert "\n     "))
	  (prin1 (color-theme-spec face) (current-buffer))))))
  (insert ")))\n"
	  "(provide '" func-name ")")
  (emacs-lisp-mode)
  (goto-char (point-min))
  (message "Making subtheme... done"))

(provide 'color-theme-maker)

;;; color-theme-maker.el ends here

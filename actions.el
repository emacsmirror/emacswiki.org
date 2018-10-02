;;; actions.el --- actions utilities

;; Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018 Vinicius Jose Latorre

;; Author: Vinicius Jose Latorre <viniciusjl.gnu@gmail.com>
;; Maintainer: Vinicius Jose Latorre <viniciusjl.gnu@gmail.com>
;; Keywords: convenience
;; Version: 0.10
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

(defconst actions-version "0.10"
  "actions.el, v 0.10 <2018/08/08 vinicius>

Please send all bug fixes and enhancements to
	Vinicius Jose Latorre <viniciusjl.gnu@gmail.com>
")

;; This file is *NOT* (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package provides an action menu which items are visible depending on
;; the point context.
;;
;; As an usage example, it can be used with `dired-mode' to expand .tar.gz
;; files, or view/edit an image file by just clicking `M-F6' on file, then an
;; menu with defined actions is shown.
;;
;; Or grep a word by clicking `M-F6' on the word.
;;
;;
;; Using `actions'
;; ---------------
;;
;; To use `actions' insert in your ~/.emacs file (or c:/_emacs, if you're
;; using Windows 9x/NT or MS-DOS):
;;
;;    (require 'actions)
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of `actions' options, please, see the
;; options declaration in the code for a long documentation.
;;
;; `actions-menu'	Define a submenu the same way as
;;			`easy-menu-define' does it.
;;
;; `actions-options'	Alist for tool options (see tool manual).
;;
;; To set the above options you may:
;;
;; a) insert the code in your ~/.emacs, like:
;;
;;	 (setq actions-menu '("Action" ["Option" (action) :visible t]))
;;
;;    This way always keep your default settings when you enter a new Emacs
;;    session.
;;
;; b) or use `set-variable' in your Emacs session, like:
;;
;;	 M-x set-variable RET actions-menu RET
;;       '("Action" ["Option" (action) :visible t]) RET
;;
;;    This way keep your settings only during the current Emacs session.
;;
;; c) or use customization, for example:
;;	 click on menu-bar *Help* option,
;;	 then click on *Customize*,
;;	 then click on *Browse Customization Groups*,
;;	 expand *Tools* group,
;;	 expand *Actions* group
;;	 and then customize `actions' options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; d) or see the option value:
;;
;;	 C-h v actions-menu RET
;;
;;    and click the *customize* hypertext button.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(require 'easymenu)


(unless (fboundp 'region-active-p)
  (defun region-active-p ()
    (and transient-mark-mode mark-active)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User defined

(defcustom actions-menu
  '("Action" :active t
    ["Expand .tar"     (actions-untar)
     :visible (actions-file-p "\\.tar$")]
    ["Expand .tar.***" (actions-untar)
     :visible (actions-file-p "\\.\\(tar\\.\\(gz\\|Z\\|bz2\\|lz\\|lzma\\|lzo\\|xz\\)\\|tgz\\|taz\\|taZ\\|tz2\\|tbz2\\|tbz\\|tlz\\)$")]
    ["Expand .gz"      (actions-call "gunzip")
     :visible (actions-file-p "\\.gz$")]
    ["Expand .zip"     (actions-call "unzip")
     :visible (actions-file-p "\\.zip$")]
    ["Expand .rar"     (actions-call "unrar")
     :visible (actions-file-p "\\.rar$")]
    ("Read PDF" :visible (actions-file-p "\\.\\(pdf\\|PDF\\)$")
     ["via 'xpdf'"     (actions-call "xpdf") :visible t]
     ["via 'okular'"   (actions-call "okular") :visible t])
    ["Read PS"         (actions-call "gv")
     :visible (actions-file-p "\\.\\(ps\\|PS\\)$")]
    ["Read PS.GZ"      (actions-call "gv")
     :visible (actions-file-p "\\.\\(ps\\|PS\\)\\.\\(gz\\|GZ\\)$")]
    ["Qt Designer"     (actions-call "designer")
     :visible (actions-file-p "\\.ui$")]
    ["VLC Video/Audio" (actions-call "vlc")
     :visible (actions-file-p "\\.\\(MP[34]\\|M4A\\|AAC\\|OPUS\\|mp[34]\\|m4a\\|aac\\|opus\\)$")]
    ["LibreOffice Doc" (actions-call "libreoffice")
     :visible (actions-file-p "\\.\\(OD[TF]\\|DOCX?\\|DOCX?\\.ENC\\|PPTX?\\|od[tf]\\|docx?\\|docx?\\.enc\\|pptx?\\)$")]
    ("Image" :visible (actions-file-p "\\.\\(JPG\\|JPEG\\|PNG\\|GIF\\|TIFF\\|SVG\\|XCF\\|jpg\\|jpeg\\|png\\|gif\\|tiff\\|svg\\|xcf\\)$")
     ["GIMP Image"     (actions-call "gimp") :visible t]
     ["GEEQIE Image"   (actions-call "geeqie")
      :visible (actions-file-p "\\.\\(JPG\\|JPEG\\|PNG\\|GIF\\|TIFF\\|SVG\\|jpg\\|jpeg\\|png\\|gif\\|tiff\\|svg\\)$")])
    ("grep recursively" :visible (region-active-p)
     ["case sensitive"   (actions-grep-r) :visible t]
     ["case insensitive" (actions-grep-r "-i") :visible t])
    ("grep prompt" :visible t
     ["case sensitive"   (actions-grep-prompt-r) :visible t]
     ["case insensitive" (actions-grep-prompt-r "-i") :visible t])
    )
  "Define a submenu the same way as `easy-menu-define' does it.

It is used by `actions-action'."
  :type '(list :tag "Action Menu"
	       (const :tag "Action Menu" "Action")
	       (const :tag "active" :active)
	       (const :tag "always" t)
	       (repeat :tag "Menu Items" :inline t
		       (choice :tag "Item"
			(vector :tag "Menu Item"
				(string :tag "Option")
				(sexp :tag "Action")
				(const :tag "visible if" :visible)
				(sexp :tag "Visibility Criteria"))
			(list :tag "SubMenu"
			      (string :tag "SubMenu")
			      (const :tag "visible if" :visible)
			      (sexp :tag "Visibility Criteria")
			      (repeat :tag "SubMenu Items" :inline t
				      (vector
				       :tag "SubMenu Item"
				       (string :tag "Option")
				       (sexp :tag "Action")
				       (const :tag "visible if" :visible)
				       (sexp :tag "Visibility Criteria")))))))
  :group 'convenience)


(defcustom actions-options
  '(("grep"
     ("--exclude-dir="
      ".svn" "tmp" ".cccc")
     ("--exclude="
      "lib*" "*.ui" "*.txt" "*.log" "*.html" "Makefile*" "makefile*")
     )
     ("xpdf"
      ("-z height"))
     ("unrar"
      ("x -r -v")))
  "Alist for action tool options (see tool manual).

The alist element has the following form:

   (TOOL (OPTION ARG...)...)

Where:

   TOOL		is a string. It is the tool name.

   OPTION	is a string. It is the tool option.

   ARG		is a string for the option argument.
		It is used if the option should have an argument."
  :type '(repeat :tag "Action Tool Options"
		 (list
		  (string :tag "Tool")
		  (repeat :tag "Tool Options" :inline t
			  (list
			   (string :tag "Option")
			   (repeat :inline t
				   (string :tag "Option Arg"))))))
  :group 'convenience)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

;;;(global-set-key [f6]            'xtags-find-tag)	; F6
;;;(global-set-key [C-f6]          'xtags-create)		; C-F6
(global-set-key [M-f6]          'actions-action)	; M-F6
;;;(global-set-key [S-f6]          'switch-cc-to-h)	; S-F6
;;;(global-set-key [C-S-f6]        'unbound-key)	; C-S-F6
;;;(global-set-key [M-S-f6]        'unbound-key)	; M-S-F6


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions

(defun actions-call (command)
  "Call COMMAND via `shell-command' with current file in `dired-mode' as arg."
  (shell-command
   (format "%s %s '%s' &"
	   command (actions-options command) (dired-get-file-for-visit))))


(defun actions-untar (&optional compress)
  "Untar current file in `dired-mode'.

Optional arg COMPRESS indicates which uncompress method to use (see tar
manual).  For example, \"z\" for gzip; \"j\" for bz2."
  (actions-call (format "tar x%svf" (or compress ""))))


(defun actions-file-p (regexp)
  "Returns t if current line in `dired-mode' matches REGEXP and it is a file."
  (and (eq major-mode 'dired-mode)
       (not (car (file-attributes (dired-get-file-for-visit))))
       (string-match regexp (dired-get-file-for-visit))))


(defvar actions-grep-history nil)

(defun actions-grep-prompt-r (&optional extra-arg)
  "Prompt for string to grep before grepping recursively.

Optional arg EXTRA-ARG has an extra arg for grep."
  (let ((prompt (format "Grep%s for: "
			(if extra-arg
			    (format " (%s)" extra-arg)
			  ""))))
    (actions-do-grep-r
     (read-string prompt nil 'actions-grep-history)
     extra-arg)))


(defun actions-grep-r (&optional extra-arg)
  "Grep recursively for current text region.

Optional arg EXTRA-ARG has an extra arg for grep."
  (actions-do-grep-r (buffer-substring (region-beginning) (region-end))
		   extra-arg))


(defun actions-options (tool)
  "Return TOOL default options (see `actions-options' var)."
  (mapconcat (lambda (option)
	       (let ((name (car option))
		     (args (cdr option)))
		 (concat " "
			 (if args
			     (mapconcat (lambda (arg)
					  (concat name arg))
					args
					" ")
			   name))))
	     (cdr (assoc tool actions-options))
	     ""))


(defun actions-do-grep-r (regexp &optional extra-arg)
  "Grep for REGEXP with optional EXTRA-ARG.

Option `actions--options' has default arguments for grep."
  (grep (concat "grep -nH -r "		 ; line number, file name, recursive
		extra-arg		 ; extra arg (case insentive, etc.)
		(actions-options "grep") ; default options: exclude dir/file
		" -e '"			 ; regular expression for finding
		regexp
		"' *")))


(defvar actions-menu-popup nil)

(defun actions-action (&optional force)
  "Activate the `actions-menu' menu and execute the choosen action.

The `actions-menu' menu is evaluated if it was not evaluated or
if FORCE is non-nil."
  (interactive)
  (if (or force (not actions-menu-popup))
      (easy-menu-define
	actions-menu-popup global-map "actions menu action"
	actions-menu))
  (let ((command
	 (lookup-key actions-menu-popup
		     (vconcat (x-popup-menu (actions-position-pixel)
					    actions-menu-popup)))))
    (if (fboundp command)
	(funcall command)
      (eval command))))


(defun actions-position-pixel ()
  "Return position in pixel just below the current point position."
  (let ((x-y   (posn-x-y (posn-at-point)))
	(edges (window-inside-pixel-edges)))
    (list
     (list (+ (car x-y) (nth 0 edges))	; X
	   (+ (cdr x-y) (nth 1 edges)	; Y
	      (frame-char-height)))
     (selected-frame))))		; frame


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'actions)


;;; actions.el ends here

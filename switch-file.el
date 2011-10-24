;;; switch-file.el --- switch from one file to another.

;; Copyright (C) 2008, 2009, 2010, 2011 Vinicius Jose Latorre

;; Author:	Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer:	Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords:	convenience
;; Time-stamp:	<2011/10/23 12:11:46 vinicius>
;; Version:	0.3
;; X-URL:	http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

;; This file is *NOT* (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package facilitates when you need to switch from a .cpp file,
;; for example, to a corresponding .hpp file.  That is, you visit a
;; .cpp file, type M-x switch-cc-to-h RET, and the corresponding .hpp
;; file is visited automagically.
;;
;; It also helps when you have three (or more) file extensions to
;; switch, for example, you could have .cpp, .hpp and .inc extensions.
;; So, you could switch from a .cpp file to a .hpp file, from .hpp to
;; .inc, and from .inc to .cpp again.
;;
;; For good performance, be sure to byte-compile switch-file.el, e.g.
;;
;;    M-x byte-compile-file <give the path to switch-file.el when prompted>
;;
;; This will generate switch-file.elc, which will be loaded instead of
;; switch-file.el.
;;
;; switch-file was tested with GNU Emacs 21, 22 and 23.
;;
;; I don't know if it is compatible with XEmacs.
;;
;;
;; Usage
;; -----
;;
;; To use switch-file, insert in your ~/.emacs:
;;
;;    (require 'switch-file)
;;    (setq switch-path (list <PATH-LIST>))
;;
;; And, for example, to switch from a current .cc file to a .hh
;; corresponding file (or vice-versa) type:
;;
;;    M-x switch-cc-to-h RET
;;
;; As a suggestion for key bindings:
;;
;;    (global-set-key [f3] 'switch-cc-to-h)
;;
;; You can also include interactively a new path into `switch-path' option via
;;`switch-path' command (which see).
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of switch-file options,
;; please, see the options declaration in the code for a long
;; documentation.
;;
;; `switch-path'                Specify a path list for locating files
;;                              to switch.
;;
;; `switch-major-mode-alist'    Specify a major mode alist.
;;
;; To set the above options you may:
;;
;; a) insert code in your ~/.emacs, like:
;;
;;	 (setq switch-path '("some-dir/" "other-dir/"))
;;
;;    This method preserves your default settings when you enter a new
;;    Emacs session.
;;
;; b) or use `set-variable' in your Emacs session, like:
;;
;;	 M-x set-variable RET switch-path RET
;;	 '("some-dir/" "other-dir/") RET
;;
;;    This method preserves your settings only during the current
;;    Emacs session.
;;
;; c) or use customization, for example:
;;	 click on menu-bar *Help* option,
;;	 then click on *Customize*,
;;	 then click on *Browse Customization Groups*,
;;	 expand *Convenience* group,
;;	 and then customize switch-file options.
;;    This way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; d) or see the option value:
;;
;;	 C-h v switch-path RET
;;
;;    and click the *customize* hypertext button.
;;    This way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;;
;; Acknowledgments
;; ---------------
;;
;; Thanks to SeungcheolJung (EmacsWiki) for code correction.
;;
;; Thanks to Arndt Gulbrandsen (QtMode EmacsWiki) for very first
;; version.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code:


;;;###autoload
(defcustom switch-path
  '("./")
  "*Specify a path list for locating files to switch.

Each path must end with '/'."
  :type '(repeat :tag "Path List"
		 directory)
  :group 'convenience)


;;;###autoload
(defcustom switch-major-mode-alist
  '((c-mode   ("c" ".c") ("h" ".h"))
    (c++-mode ("c\\|cc\\|C\\|cpp" ".cc" ".C" ".cpp" ".c")
	      ("h\\|hh\\|H\\|hpp" ".hh" ".H" ".hpp" ".h")))
  "*Specify a major mode alist.

The alist element has the following form:

   (MAJOR-MODE (EXTENSION-REGEXP EXTENSION... )... )

Where:

MAJOR-MODE is a major mode symbol.

EXTENSION-REGEXP is a regexp used for matching the current buffer
file name extension.

EXTENSION is a string used for changing the file name extension."
  :type '(repeat :tag "Switch List"
		 (list
		  (symbol :tag "Id")
		  (repeat :inline t
			  (list
			   (regexp :tag "File Extension Regexp")
			   (repeat :inline t
				   (string :tag "File Extension"))))))
  :group 'convenience)


(defvar switch-path-history nil)

;;;###autoload
(defun switch-path ()
  "Read from minibuffer a new path to include into `switch-path'."
  (interactive)
  (let ((new-path (read-string "Path to switch a file: " nil
			       'switch-path-history default-directory))
	(paths    switch-path)
	found)
    (while (and (not found) paths)
      (setq found (string-match
		   (format "^%s" (regexp-quote (expand-file-name new-path)))
		   (expand-file-name (car paths)))
	    paths (cdr paths)))
    (unless found
      (setq switch-path (cons new-path switch-path)))))


;;;###autoload
(defun switch-c-to-h ()
  "Switch from C file to a H file or vice-versa."
  (interactive)
  (switch-file-major-mode 'c-mode))


;;;###autoload
(defun switch-cc-to-h ()
  "Switch from CC file to a H file or vice-versa."
  (interactive)
  (switch-file-major-mode 'c++-mode))


;;;###autoload
(defun switch-file-major-mode (&optional mode)
  "Switch from current file to another one depending on MODE or major mode."
  (interactive)
  (switch-to-other-file (cdr (assq (or mode major-mode)
                                   switch-major-mode-alist))))


(defun switch-to-other-file (extension-list)
  "Switch from one file to another file or vice-versa.

EXTENSION-LIST is a list which elements have the following form:

   ( (EXTENSION-REGEXP EXTENSION... )... )

Where:

EXTENSION-REGEXP is a regexp used for matching the current buffer
file name extension.

EXTENSION is a string used for changing the file name extension."
  (save-match-data
    (when (and buffer-file-name extension-list)
      (let* ((file-name (file-name-nondirectory buffer-file-name))
	     (name      (progn
			  (string-match "^\\(.*\\)\\.\\([^.]*\\)$"
                                        file-name)
			  (match-string 1 file-name)))
	     (elist     (let ((suffix
                               (file-name-extension buffer-file-name))
                              tmp)
                          (dolist (ext extension-list)
                            (unless (string-match (car ext) suffix)
                              (setq tmp (cons (cdr ext) tmp))))
                          (nreverse tmp)))
	     slist dlist full-file-name found)
        (while elist                    ; run list of extensions
          (setq slist (car elist)
                elist (cdr elist))
          (while slist                  ; run extensions
            (setq file-name (concat name (car slist))
                  slist     (cdr slist)
                  dlist     switch-path)
            (while dlist                ; run directories
              (setq full-file-name (concat (car dlist) file-name)
                    dlist          (cdr dlist))
              ;; found a file to switch to
              (when (file-exists-p full-file-name)
                (find-file full-file-name)
                (setq elist nil
                      slist nil
                      dlist nil
		      found t)))))
	(unless found
	  (message "Not found a file to switch to."))))))


(provide 'switch-file)


;;; switch-file.el ends here

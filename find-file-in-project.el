;;; find-file-in-project.el --- Find files in a project quickly.

;; Copyright (C) 2006, 2007, 2008 Phil Hagelberg and Doug Alcorn

;; Author: Phil Hagelberg and Doug Alcorn
;; URL: http://www.emacswiki.org/cgi-bin/wiki/FindFileInProject
;; Version: 2.0
;; Created: 2008-03-18
;; Keywords: project, convenience
;; EmacsWiki: FindFileInProject

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides a method for quickly finding any file in a given
;; project. Projects are defined as per the `project-local-variables'
;; library, by the presence of a `.emacs-project' file in a directory.

;; By default, it looks only for files whose names match
;; `ffip-regexp', but it's understood that that variable will be
;; overridden locally. This can be done either with a mode hook:

;; (add-hook 'emacs-lisp-mode-hook (lambda (setl ffip-regexp ".*\\.el")))

;; or by setting it in your .emacs-project file, in which case it will
;; get set locally by the project-local-variables library.

;; You can also be a bit more specific about what files you want to
;; find. For instance, in a Ruby on Rails project, you may be
;; interested in all .rb files that don't exist in the "vendor"
;; directory. In that case you could locally set `ffip-find-options'
;; to "" from within a hook or your .emacs-project file. The options
;; accepted in that variable are passed directly to the Unix `find'
;; command, so any valid arguments for that program are acceptable.

;; If `ido-mode' is enabled, the menu will use `ido-completing-read'
;; instead of `completing-read'.

;; Recommended binding:
;; (global-set-key (kbd "C-x C-M-f") 'find-file-in-project)

;;; TODO:

;; Performance testing with large projects
;; Switch to using a hash table if it's too slow

;;; Code:

(require 'project-local-variables)

(defvar ffip-regexp
  (concat ".*\\.\\(" (mapconcat (lambda (x) x) '("rb" "rhtml" "el") "\\|") "\\)")
  "Regexp of things to look for when using find-file-in-project.")

(defvar ffip-find-options
  ""
  "Extra options to pass to `find' when using find-file-in-project.

Use this to exclude portions of your project: \"-not -regex \\\".*vendor.*\\\"\"")

(defvar ffip-project-root nil
  "If non-nil, overrides the project root directory location.")

(defun ffip-project-files ()
  "Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique."
  (let ((file-alist nil))
    (mapcar (lambda (file)
	      (let ((file-cons (cons (file-name-nondirectory file)
				     (expand-file-name file))))
		(when (assoc (car file-cons) file-alist)
		  (ffip-uniqueify (assoc (car file-cons) file-alist))
		  (ffip-uniqueify file-cons))
		(add-to-list 'file-alist file-cons)
		file-cons))
	    (split-string (shell-command-to-string (concat "find " (or ffip-project-root
								       (ffip-project-root))
							   " -type f -regex \""
							   ffip-regexp
							   "\" " ffip-find-options))))))

(defun ffip-uniqueify (file-cons)
  "Set the car of the argument to include the directory name plus the file name."
  (setcar file-cons
	  (concat (car file-cons) ": "
		  (cadr (reverse (split-string (cdr file-cons) "/"))))))

(defun find-file-in-project ()
  "Prompt with a completing list of all files in the project to find one.

The project's scope is defined as the first directory containing
an `.emacs-project' file. You can override this by locally
setting the `ffip-project-root' variable."
  (interactive)
  (let* ((project-files (ffip-project-files))
	 (file (if (functionp 'ido-completing-read)
		   (ido-completing-read "Find file in project: "
					(mapcar 'car project-files))
		 (completing-read "Find file in project: "
				  (mapcar 'car project-files)))))
    (find-file (cdr (assoc file project-files)))))

(defun ffip-project-root (&optional dir)
  "Find the root of the project defined by presence of `.emacs-project'."
  (file-name-directory (plv-find-project-file default-directory "")))

(provide 'find-file-in-project)
;;; find-file-in-project.el ends here

;;; joseph-file-util.el --- Function library about file and directory.

;; Copyright (C) 2011~, Joseph, all rights reserved.
;; Created: 2011-03-31
;; Last Updated: Joseph 2011-10-31 16:39:08 星期一
;; Version: 0.1.2
;; Description: Function library about file and directory.
;; Author: Joseph <jixiuf@gmail.com>
;; Maintainer: Joseph <jixiuf@gmail.com>
;; URL:  http://www.emacswiki.org/emacs/download/joseph-file-util.el
;; Main Page: https://github.com/jixiuf/joseph-file-util
;; Keywords:  file directory
;; Compatibility: (Test on GNU Emacs 24.0.50.1)
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;  some functions handle file and directory.
;;  look examples above each function for detail.
;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; (print  (all-files-under-dir-recursively "~/.emacs.d/site-lisp/ahk-mode/" ))
;; (print  (all-files-under-dir-recursively "~/.emacs.d/site-lisp/ahk-mode/" "\\.el$"))

;; (all-files-under-dir-recursively "~/.emacs.d/site-lisp/ahk-mode/" "txt" nil )
;; return all files under dir  "~/.emacs.d/site-lisp/ahk-mode/" which filename match "txt"

;; (all-files-under-dir-recursively   "~/.emacs.d/site-lisp/ahk-mode/" "syntax" t)
;; return all files under dir  "~/.emacs.d/site-lisp/ahk-mode/" which full file path match "syntax"

;; (all-files-under-dir-recursively   "~/.emacs.d/site-lisp/ahk-mode/" "Key" nil "word" nil)
;; (all-files-under-dir-recursively   "~/.emacs.d/site-lisp/ahk-mode/" "Key" nil "word" )
;; return all files under dir  "~/.emacs.d/site-lisp/ahk-mode/" which file name match "Key" and filename doesn't match "word"

;; (all-files-under-dir-recursively   "~/.emacs.d/site-lisp/ahk-mode/" "Key" nil "word" t)
;; return all files under dir  "~/.emacs.d/site-lisp/ahk-mode/" which file name match "Key" and full file path doesn't match "word"

;; (all-files-under-dir-recursively   "~/.emacs.d/site-lisp/ahk-mode/" nil nil "syntax" t)
;; return all files under dir   "~/.emacs.d/site-lisp/ahk-mode/" which full  file path doesn't match "syntax"

;; (all-files-under-dir-recursively   "~/.emacs.d/site-lisp/ahk-mode/" nil nil "syntax" nil)
;; return all files under dir   "~/.emacs.d/site-lisp/ahk-mode/" which file name doesn't match "syntax"

;;;###autoload
(defun all-files-under-dir-recursively
  (dir &optional include-regexp  include-regexp-absolute-path-p exclude-regex exclude-regex-absolute-p)
  "return all files matched `include-regexp' under directory `dir' recursively.
if `include-regexp' is nil ,return all.
when `include-regexp-absolute-path-p' is nil or omited ,filename is used to match `include-regexp'
when `include-regexp-absolute-path-p' is t then full file path is used to match `include-regexp'
when `exclude-regexp-absolute-path-p' is nil or omited ,filename is used to match `exclude-regexp'
when `exclude-regexp-absolute-path-p' is t then full file path is used to match `exclude-regexp'
"
  (let((files (list dir))  matched-files head)
    (while (> (length files) 0)
      (setq head (pop files))
      (when (file-readable-p head)
        (if (file-directory-p head)
            (dolist (sub (directory-files head))
              (when  (not (string-match "^\\.$\\|^\\.\\.$" sub))
                (when (or (not exclude-regex)
                          (and exclude-regex (not exclude-regex-absolute-p))
                          (and exclude-regex exclude-regex-absolute-p  (not (string-match  exclude-regex  (expand-file-name sub head)))))
                  (setq files (append (list (expand-file-name sub head)) files)))))
          (if include-regexp
              (if (string-match include-regexp (if include-regexp-absolute-path-p head (file-name-nondirectory head)))
                  (if exclude-regex
                      (if (not (string-match exclude-regex (if exclude-regex-absolute-p head (file-name-nondirectory head))))
                          (add-to-list 'matched-files head))
                    (add-to-list 'matched-files head)))
            (if exclude-regex
                (if (not (string-match exclude-regex (if exclude-regex-absolute-p head (file-name-nondirectory head))))
                    (add-to-list 'matched-files head))
              (add-to-list 'matched-files head))))))
    matched-files))

;; ( all-subdir-under-dir-recursively "~/.emacs.d") will list all sub directories
;; under"~/.emacs.d" recursively (include "~/.emacs.d" directory),
;; ( all-subdir-under-dir-recursively "~/.emacs.d/site-lisp/" nil nil "\\.git\\|\\.svn" t)
;; will list all sub directories under home recursively ,exclude `.git' and `.svn'
;; directories.
(defun all-subdir-under-dir-recursively
  (dir &optional include-regexp  include-regexp-absolute-path-p exclude-regex exclude-regex-absolute-p)
  "return all files matched `include-regexp' under directory `dir' recursively.
if `include-regexp' is nil ,return all.
when `include-regexp-absolute-path-p' is nil or omited ,filename is used to match `include-regexp'
when `include-regexp-absolute-path-p' is t then full file path is used to match `include-regexp'
when `exclude-regexp-absolute-path-p' is nil or omited ,filename is used to match `exclude-regexp'
when `exclude-regexp-absolute-path-p' is t then full file path is used to match `exclude-regexp'"
  (let((files (list dir))  matched-dirs head)
    (while (> (length files) 0)
      (setq head (pop files))
      (when (file-readable-p head)
        (when (file-directory-p head)
          (dolist (sub (directory-files head))
            (when  (not (string-match "^\\.$\\|^\\.\\.$" sub))
              (when (or (not exclude-regex)
                        (and exclude-regex (not exclude-regex-absolute-p))
                        (and exclude-regex exclude-regex-absolute-p  (not (string-match  exclude-regex  (expand-file-name sub head)))))
                (setq files (append (list (expand-file-name sub head)) files)))))
          (if include-regexp
              (if (string-match include-regexp (if include-regexp-absolute-path-p head (file-name-nondirectory head)))
                  (if exclude-regex
                      (if (not (string-match exclude-regex (if exclude-regex-absolute-p head (file-name-nondirectory head))))
                          (add-to-list 'matched-dirs head))
                    (add-to-list 'matched-dirs head)))
            (if exclude-regex
                (if (not (string-match exclude-regex (if exclude-regex-absolute-p head (file-name-nondirectory head))))
                    (add-to-list 'matched-dirs head))
              (add-to-list 'matched-dirs head))))))
    matched-dirs))


;;;###autoload
(defun joseph-all-subdirs-under-dir-without-borring-dirs(dir)
  "return all sub directories under `dir' exclude those borring directory."
  (all-subdir-under-dir-recursively dir nil nil  "\\.git\\|\\.svn\\|RCS\\|rcs\\|CVS\\|cvs" t))


;; for example :
;;
;; (joseph-delete-matched-files '("/etc/hosts"  "/etc/host.conf" "/etc/bash/bashrc") "host")
;; return :("/etc/bash/bashrc")
;; (joseph-delete-matched-files '("/etc/hosts"  "/etc/host.conf" "/etc/bash/bashrc") "etc" t)
;; return nil
;; (joseph-delete-matched-files '("/etc/hosts"  "/etc/host.conf" "/etc/bash/bashrc") "etc" )
;; return all

;;;###autoload
(defun joseph-delete-matched-files
  (files pattern &optional absolute-path-p)
  "delete matched files from `files' the new list of files
will be returned ,`files' is a list of file or directory.
when `absolute-path-p' is nil,
the name of file is used to match the `pattern',
 if not , only the absolute path of file is used."
  (let ((tmp-files))
    (dolist (file files)
      (unless (string-match pattern  (if absolute-path-p file (file-name-nondirectory file)))
        (add-to-list 'tmp-files file)))
    tmp-files))

;; (get-system-file-path "d:/.emacs.d") on windows = d:\\.emacs.d
;;;###autoload
(defun get-system-file-path(file-path)
  "when on windows `expand-file-name' will translate from \\ to /
some times it is not needed . then this function is used to translate /
to \\ when on windows"
  (if (equal system-type 'windows-nt)
      (convert-standard-filename (expand-file-name file-path))
    (expand-file-name file-path)))

(provide 'joseph-file-util)


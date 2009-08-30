;;; -*- local-enable-local-variables: nil -*-
;;; dirvars.el --- Local variables that apply to an entire directory

;; Copyright (C) 2002  Matt Armstrong

;; Author: Matt Armstrong <matt@xxxxxxxxxx>
;; Location: http://www.lickey.com/env/elisp/dirvars.el
;; Keywords: files
;; Version: 1.3
;; Obscure: matt@xxxxxxxxxxxxxxxxxxx|elisp/dirvars.el|20021213043855|48166

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

;; Emacs allows you to specify local variable values for use when
;; editing a file either in the first line or in a local variables
;; list.
;;
;; This file provides similar functionality, but for an entire
;; directory tree.
;;
;; You simply place an .emacs-dirvars file in the root of your
;; project's tree, and you can then set emacs variables like you would
;; in a Local Variables: section at the end of a file.  E.g. the
;; contents of a typical dirvars file might look like this:
;;
;;    ;; -*- emacs-lisp -*-
;;    ;;
;;    ;; This file is processed by the dirvars emacs package.  Each variable
;;    ;; setting below is performed when this dirvars file is loaded.
;;    ;;
;;    indent-tabs-mode: nil
;;    tab-width: 8
;;    show-trailing-whitespace: t
;;    indicate-empty-lines: t
;;
;; Much of this code is stolen and modified from the standard Emacs
;; files.el
;;
;; This code refuses to set any symbol that meets any of these
;; criteria (this criteria is stolen from files.el):
;;
;;   - the symbol is in the ignored-local-variables list
;;   - the symbol has the risky-local-variable property.
;;   - the symbol name ends in -hook(s), -function(s), -form(s),
;;     -program, -command, or -predicate.
;;
;; change by Benjamin Rutt:  don't look upwards by default in a
;; remote filesystem

;;; Todo:

;; Implement the following changes to keep in line with elisp coding
;; conventions: When a package provides a modification of ordinary
;; Emacs behavior, it is good to include a command to enable and
;; disable the feature, Provide a command named `WHATEVER-mode' which
;; turns the feature on or off, and make it autoload (*note
;; Autoload::).  Design the package so that simply loading it has no
;; visible effect--that should not enable the feature.(2) Users will
;; request the feature by invoking the command.
;;
;; Support customize?

;;; Code:

(defcustom dirvars-file-name ".emacs-dirvars"
  "File base name that is loaded by dirvars."
  :type 'string) 

(defcustom dirvars-chase-remote nil
  "Whether dirvars looks upward if in a remote filesystem."
  :type 'boolean)

(defvar dirvars-enable-flag t
  "*Control use of directory variables in files you visit.
The meaningful values are nil and non-nil.")

(defun dirvars-find-upwards (file-name)
  "Find a file in the current directory or one of its parents.

Returns the fully qualified file name, or nil if it isn't found.

The FILE-NAME specifies the file name to search for."
  (if (and (not dirvars-chase-remote) (file-remote-p default-directory))
      nil
    ;; Chase links in the source file and search in the dir where it
    ;; points.
    (setq dir-name (or (and buffer-file-name
                            (file-name-directory (file-chase-links
                                                  buffer-file-name)))
                       default-directory))
    ;; Chase links before visiting the file.  This makes it easier to
    ;; use a single file for several related directories.
    (setq dir-name (file-chase-links dir-name))
    (setq dir-name (expand-file-name dir-name))
    ;; Move up in the dir hierarchy till we find a change log file.
    (let ((file1 (concat dir-name file-name))
          parent-dir)
      (while (and (not (file-exists-p file1))
                  (progn (setq parent-dir
                               (file-name-directory
                                (directory-file-name
                                 (file-name-directory file1))))
                         ;; Give up if we are already at the root dir.
                         (not (string= (file-name-directory file1)
                                       parent-dir))))
        ;; Move up to the parent dir and try again.
        (setq file1 (expand-file-name file-name parent-dir)))
      ;; If we found the file in a parent dir, use that.  Otherwise,
      ;; return nil
      (if (or (get-file-buffer file1) (file-exists-p file1))
          file1
        nil))))

(defun dirvars-eat-comment ()
  (while (looking-at "[ \t\n]*;")
    (let ((begin (point)))
      (skip-chars-forward " \t\n")
      (if (looking-at ";")
          (progn
            (end-of-line)
            (delete-region begin (point)))))))

(defun dirvars-hack-local-variables (dirvars-file)
  (save-excursion
    (let ((original-buffer (current-buffer))
          (temp-buffer (get-buffer-create "*dirvars-temp*"))
          (enable-local-variables (and local-enable-local-variables
                                       enable-local-variables
                                       dirvars-enable-flag))
          (continue t)
          (parse-sexp-ignore-comments t)
          (lisp-mode-hook nil)
          beg)
      (set-buffer temp-buffer)
      (erase-buffer)
      (lisp-mode)
      (insert-file dirvars-file)
      (goto-char (point-min))
      (catch 'done
        (while continue
          (if (null (scan-sexps (point) 1))
              (throw 'done nil))
          (goto-char (scan-sexps (point) 1))
          (goto-char (scan-sexps (point) -1))
          (if (eobp)
              (throw 'done nil))
          (setq beg (point))
          (skip-chars-forward "^:\n")
          (if (not (looking-at ":"))
              (error (format "Missing colon in directory variables entry at %d"
                             (point))))
          (skip-chars-backward " \t")
          (let* ((str (buffer-substring beg (point)))
                 (var (read str))
                 val)
            ;; Read the variable value.
            (skip-chars-forward "^:")
            (forward-char 1)
            (setq val (read (current-buffer)))
            (save-excursion
              (set-buffer original-buffer)
              (dirvars-hack-one-local-variable dirvars-file
                                               var val))))))))

(defun dirvars-hack-one-local-variable (dirvars-file var val)
  "\"Set\" one variable in a local variables spec.
A few variable names are treated specially."
  (cond ((memq var ignored-local-variables)
         nil)
        ;; Trap risky variables and such.  This is the same logic
        ;; that files.el uses.
        ((or (get var 'risky-local-variable)
             (and
              (string-match 
"-hooks?$\\|-functions?$\\|-forms?$\\|-program$\\|-command$\\|-predicate$"
                            (symbol-name var))
              (not (get var 'safe-local-variable))))
         (message (format "Ignoring %s in %s"
                          (symbol-name var) dirvars-file)))
        ;; Ordinary variable, really set it.
        (t (make-local-variable var)
           (set var val))))

(defun dirvars-hack-local-variables-before ()
  (let ((dirvars-file (dirvars-find-upwards dirvars-file-name)))
    (if dirvars-file
        (dirvars-hack-local-variables dirvars-file))))

(defadvice hack-local-variables
  (before dirvars-hack-local-variables-before activate)
  "Process dirvars before a file's local variables are processed."
  (dirvars-hack-local-variables-before))

(provide 'dirvars)
;;; dirvars.el ends here

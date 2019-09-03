;;; wsl-path.el -- Teach EMACS about windows subsystem for linux styles paths

;; Copyright (C) 2009 Victor Ren
;; Copyright (C) 2019-08-29 Zach Kost-Smith

;; Author: Victor Ren <victorhge@gmail.com>
;; Modified for WSL by: Zach Kost-Smith <zachkostsmith@gmail.com>
;; Keywords: windows, WSL, mount, path

;; This file is *NOT* (yet?) part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package lets you use windows-style filenames like "c:/path/file" or
;; "c:\path\file" in Emacs when running in the Windows Subsystem for Linux.

;;; Installation:

;; Put in your .emacs or site-start.el file the following lines:
;;   (require 'wsl-path)
;;   (wsl-path-activate)

;;; Compatibility

;; How it works: Push some functions onto file-name-handler-alist.  which detect
;; filenames expressed in Windows style, and translate those names into the WSL
;; equivalent.

;;; Code:

(require 'cl-lib)
(defconst wsl-path-version "0.2")

(defgroup wsl-path nil
  "Proper handling of windows filenames."
  :prefix "wsl-path-"
  :group 'files)


(defvar wsl-path-prefix "/mnt/"
  "Prefix for the WSL mount points.")

(defun resolve-file-ignoring-case (file dir)
  "Find a entry in DIR whose name matches FILE to within case."
  (cl-find (downcase file) (directory-files dir)
           :key 'downcase
           :test 'string-equal))

(defun resolve-path-ignoring-case (path)
  "Attempt to find a file ignoring case.  PATH must be an simple
absolute path like one returned from SUBSTITUTE-IN-FILE-NAME.
The file must exist for this to be meaningful, otherwise it will
simply return whatever you input."
  (if (file-exists-p path)
      path
    (let* ((filename "/"))
      (cl-loop for file in (split-string path "/" t)
               for guess = (concat filename file)
               do
               (if (file-exists-p guess)
                   (setf filename (concat guess "/"))
                 (let ((real-file (resolve-file-ignoring-case file filename)))
                   (if real-file
                       (setf filename (concat filename real-file "/"))
                     ;; If all else fails, leave it unchanged
                     (setf filename (concat filename file "/")))))
               finally (return
                        ;; Remove trailing slash if not on input
                        (if (equal ?/ (aref path (- (length path) 1)))
                            filename
                          (substring filename 0 (- (length filename) 1))))))))

(defun wsl-path-run-real-handler (operation args)
  "Run OPERATION with ARGS."
  (let ((inhibit-file-name-handlers
         (append '(wsl-path-map-drive-hook-function)
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defconst wsl-path-style1-regexp "\\`\\(.*/\\)?\\([a-zA-Z]:\\)\\\\")
(defconst wsl-path-style2-regexp "\\`\\(.*/\\)?\\([a-zA-Z]:\\)/")

;; We cannot assume that NAME matched wsl-path-style1-regexp nor
;; wsl-path-style2-regexp because this function could be called with
;; either argument to `expand-file-name', but only one argument to
;; `expand-file-name' may have matched a regexp.  For example,
;; `(expand-file-name ".." "c:/")' will trigger `(wsl-path-convert-file-name
;; "..")' and `(wsl-path-convert-file-name "c:/")' to be called.
(defun wsl-path-convert-file-name (name)
  "Convert file NAME to WSL style.
`x:/' to `/mnt/x/'."
  (let ((inhibit-file-name-handlers
         (append '(wsl-path-map-drive-hook-function)
                 (and (eq inhibit-file-name-operation
                          'substitute-in-file-name)
                      inhibit-file-name-handlers)))
        (inhibit-file-name-operation 'substitute-in-file-name))
    (cond ((string-match wsl-path-style1-regexp name)
           (let ((filename
                  (replace-match (concat wsl-path-prefix
                                         (downcase (substring (match-string 2 name) 0 1)))
                                 t nil name 2)))
             (while (string-match "\\\\" filename)
               (setq filename
                     (replace-match "/" t nil filename)))
             (resolve-path-ignoring-case
              (substitute-in-file-name filename))))
          ((string-match wsl-path-style2-regexp name)
           (resolve-path-ignoring-case
            (substitute-in-file-name
             (replace-match (concat wsl-path-prefix
                                    (downcase (substring (match-string 2 name) 0 1)))
                            t nil name 2))))

          (t name))))

;; (string-match wsl-path-style2-regexp "/sd/c:/xpd/file.txt")
;; (wsl-path-convert-file-name "sd/c:/xpd/file.txt")
;; (wsl-path-convert-file-name "~/path/c:/sds/")
;; (wsl-path-convert-file-name "/c:/sds/")
;; (wsl-path-convert-file-name "/sd/c:\\sds\\")

(defun wsl-path-map-drive-hook-function (operation name &rest args)
  "Run OPERATION on WSL NAME with ARGS.
Map Windows sytle name to the WSL-style \"/[A-Za-z]/\" and call
OPERATION with the mapped filename\(s). NAME must have the format looks like
\"^/[A-Za-z]:/\" or \"^[A-Za-z]:\\\"  here. Note that at least the first
element of ARGS could be a filename too \(then it must have the same syntax
like NAME!) which must be converted \(e.g. `expand-file-name' can be called
with two filenames)."
  (wsl-path-run-real-handler
   operation
   (cons (wsl-path-convert-file-name name)
		 (if (stringp (car args))
			 (cons (wsl-path-convert-file-name (car args))
				   (cdr args))
		   args))))

(defvar wsl-path-activated nil)

(defun wsl-path-activate ()
  "Activate wsl-path-style-handling."
  (interactive)

  (unless wsl-path-activated

    (add-to-list 'file-name-handler-alist
                 (cons wsl-path-style1-regexp
                       'wsl-path-map-drive-hook-function))
    (add-to-list 'file-name-handler-alist
                 (cons wsl-path-style2-regexp
                       'wsl-path-map-drive-hook-function))
    (setq wsl-path-activated t)))

(defun wsl-path-deactivate ()
  "Deactivate windows-style-path handling."
  (interactive)
  (unless (not wsl-path-activated)

    (setq file-name-handler-alist
          (delete (assoc wsl-path-style1-regexp file-name-handler-alist)
                  file-name-handler-alist))
    (setq file-name-handler-alist
          (delete (assoc wsl-path-style2-regexp file-name-handler-alist)
                  file-name-handler-alist))

    (setq wsl-path-activated nil)))

(provide 'wsl-path)

;;; wsl-path.el ends here

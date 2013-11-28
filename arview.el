;;; arview.el --- extract and view archives in the temporary directory

;; Copyright (C) 2013  Andrey Fainer

;; Author: Andrey Fainer <fandrey@gmx.com>
;; Version: 1.0
;; Keywords: files
;; URL: http://www.emacswiki.org/emacs/arview.el
;; Compatibility: Emacs24, Emacs23

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Arview extracts an archive file in the temporary directory and
;; opens a dired buffer for the directory.  Each archive has a unique
;; temporary directory:
;;
;; `temporary-file-directory'/arview-<archive-filename>.<random-string>
;;
;; Arview deletes the directory of the extracted archive when its
;; dired buffer is killed.

;; For remote archives arview copies them to
;; `temporary-file-directory'.  When the dired buffer of the copied
;; archive is killed and deleted its directory, the archive is
;; deleted too.

;; Arview is useful when archives have big directory trees and it is
;; inconvenient to view their content with `archive-mode'.  Or if you
;; want to use external programs on archived files, or copy them to
;; some other place, or do some sophisticated processing.

;; Use the command `arview' which asks for an archive file name.  In
;; `dired-mode' use `arview-dired' (bound to C-return by default).

;; To use arview, make sure that this file is in load-path and insert
;; in your .emacs:
;;
;;   (require 'arview)

;;; Code:

;; FIXME Temp directories are not deleted if Emacs is killed.

(require 'dired-aux)

(eval-when-compile
  (require 'cl))

(defgroup arview nil
  "The archive viewer customization group."
  :group 'data)

(defcustom arview-archive-type-functions
  '(arview-file-archive arview-file-extension)
  "The list of functions used to determine the archive file type.
The archive type is the value of the first function which returns
non-nil.  The functions must take one argument: the archive file
name."
  :type 'list
  :group 'arview)

(defcustom arview-types
  (let (types)
    (mapc #'(lambda (type)
              (when (executable-find (cadr type))
                (push type types)))
          '((zip "unzip")
            (7z "7z" "x")
            (rar "unrar" "x")))
    types)
  "Archive types known to arview.
Each element of the alist is

  (ARCHIVE-TYPE EXECUTABLE ARGUMENTS)

ARCHIVE-TYPE - a symbol which designates the archive type.
EXECUTABLE - a program to extract archives of this type.
ARGUMENTS - command-line arguments to the program."
  :type 'alist
  :group 'arview)

(defcustom arview-file-alist
  '((zip . ".*: Zip archive data")
    (7z  . ".*: 7-zip archive data")
    (rar . ".*: RAR archive data"))
  "Alist of archive type for the function `arview-file-archive'.
The element of the alist is a cons (ARCHIVE-TYPE . REGEXP), where
ARCHIVE-TYPE is a symbol which designates the archive type and
REGEXP used match against the output from file utility."
  :type 'alist
  :group 'arview)

(defconst arview-log-buffer-name "*arview-log*"
  "The name of buffer that contains output from the archive program.")

(defvar arview-buffer-p nil
  "Buffers with non-nil value are temporary archive directories.
See `arview-view'.")
(make-variable-buffer-local 'arview-buffer-p)

(defun arview-file-archive (filename)
  "Use the `file' utility to determine the type of FILENAME.
See `arview-file-alist'."
  (let (process-file-side-effects)
    (with-temp-buffer
      (cd (file-name-directory filename))
      (process-file "file" nil t t "--" (file-name-nondirectory filename))
      (goto-char (point-min))
      (let ((al arview-file-alist)
            (case-fold-search))
        (while (and al (not (looking-at-p (cdar al))))
          (setq al (cdr al)))
        (when al (caar al))))))

(defun arview-file-extension (filename)
  "Determine the type of FILENAME by its extension."
  (intern (downcase (file-name-extension filename))))

(defun arview-copy-remote-file (filename)
  "Copy FILENAME from a remote host to the temp directory.
If FILENAME is on the local host return it."
  (if (eq (find-file-name-handler filename 'copy-file)
          'tramp-file-name-handler)
      (progn
        (copy-file filename temporary-file-directory)
        (concat temporary-file-directory
                (file-name-nondirectory filename)))
    filename))

(defun arview-archive-type (filename)
  "Determine FILENAME type using `arview-archive-type-functions'."
  (let (type)
    (dolist (fn arview-archive-type-functions)
      (setq type (funcall fn filename))
      (when type (return type)))))

(defun arview-view (filename)
  "Extract the archive FILENAME and open its dired buffer.
The type of the archive determined with the function
`arview-archive-type'.  The archive extracted using the archive
program associated with the archive type (see `arview-types').

The temporary directory where the archive is extracted to is

`temporary-file-directory'/arview-FILENAME.<random-string>

Copy remote archives to the local temporary directory.

Remove the extracted archive directory when its dired buffer is
killed (see `arview-kill-buffer-hook').  Also if archive is a
remote file remove its local copy."
  (let ((ar (cdr (assoc (arview-archive-type filename) arview-types))))
    (if (not ar)
        (error "Unknown type of archive file: %s" filename)
      (let ((log (get-buffer-create arview-log-buffer-name))
            (file (arview-copy-remote-file filename)))
        (find-file
         (make-temp-file (concat "arview-"
                                 (file-name-nondirectory filename)
                                 ".")
                         t))
        (setq arview-buffer-p
              (if (string-equal file filename) t file))
        (with-current-buffer log
          (delete-region (point-min) (point-max)))
        (if (/= 0
                (apply 'call-process
                       (append (list* (car ar) nil log nil (cdr ar))
                               (list (expand-file-name file)))))
            (display-buffer log t))
        (revert-buffer)))))

(defun arview-kill-buffer-hook ()
  "Remove the archive directory when its dired buffer is killed.
Also if archive is a remote file remove its local copy.  See
`arview-view'."
  (when (and arview-buffer-p
             (eq major-mode 'dired-mode))
    (delete-directory default-directory t)
    (if (stringp arview-buffer-p)
        (delete-file arview-buffer-p))))

(add-hook 'kill-buffer-hook 'arview-kill-buffer-hook)

(defun arview-dired ()
  "View the arview under point in the current dired buffer.
See `arview-view'."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((file (dired-get-filename)))
        (arview-view file))))

(unless (lookup-key dired-mode-map [C-return])
  (define-key dired-mode-map [C-return] 'arview-dired))

(defun arview (filename)
  "Ask for the archive FILENAME and view it.
See `arview-view'."
  (interactive "fArchive file name: ")
  (arview-view filename))

(provide 'arview)
;;; arview.el ends here

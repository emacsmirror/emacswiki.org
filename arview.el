;;; arview.el --- extract and view archives in the temporary directory

;; Copyright (C) 2013-2016  Andrey Fainer

;; Author: Andrey Fainer <fandrey@gmx.com>
;; Version: 1.2
;; Keywords: files
;; URL: https://github.com/afainer/arview
;; Compatibility: Emacs24, Emacs23

;; This file is not part of GNU Emacs.

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
;; When the commands called with one prefix argument, arview prompts
;; you for another temporary directory instead of the default one.
;; With two prefix arguments you can also specify additional arguments
;; for the archive program.

;; To use arview, make sure that this file is in load-path and insert
;; in your .emacs:
;;
;;   (require 'arview)

;;; Code:

(require 'dired-aux)
(require 'tramp)

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
  '((tar "tar" "-xf")
    (zip "unzip")
    (7z "7z" "x")
    (rar "unrar" "x"))
  "Archive types known to arview.
Each element of the alist is

  (ARCHIVE-TYPE EXECUTABLE ARGUMENTS)

ARCHIVE-TYPE - a symbol which designates the archive type.
EXECUTABLE - a program to extract archives of this type.
ARGUMENTS - command-line arguments to the program."
  :type 'alist
  :group 'arview)

(defcustom arview-file-alist
  '((tar . ".*: .* tar archive")
    (zip . ".*: Zip archive data")
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
  (let ((ext (downcase (file-name-extension filename))))
    (if (or (string= ext "tar")
            (string= ext "tgz")
            (string-match-p "\.tar\.[bgx]z2?$" filename))
        'tar
      (intern ext))))

(defun arview-copy-remote-file (filename tempdir)
  "Copy FILENAME to TEMPDIR.
Copy only if FILENAME and TEMPDIR on different hosts.  Otherwise
return FILENAME."
  (if (tramp-tramp-file-p filename)
      (if (and (tramp-tramp-file-p tempdir)
               (string= (tramp-file-name-host
                         (tramp-dissect-file-name filename))
                        (tramp-file-name-host
                         (tramp-dissect-file-name tempdir))))
          filename
        (progn
          (copy-file filename tempdir)
          (concat tempdir (file-name-nondirectory filename))))
    (if (tramp-tramp-file-p tempdir)
        (progn
          (copy-file filename tempdir)
          (concat tempdir (file-name-nondirectory filename)))
      filename)))

(defun arview-archive-type (filename)
  "Determine FILENAME type using `arview-archive-type-functions'."
  (let (type)
    (dolist (fn arview-archive-type-functions)
      (setq type (funcall fn filename))
      (when type (return type)))))

(defun arview-process-file (arcmd arargs file log)
  "Run a shell process with ARCMD and ARARGS.
The filename FILE is a file for the archive command ARCMD.
Insert output in the buffer LOG."
  (let* ((comint-file-name-quote-list shell-file-name-quote-list)
         (args (if (not (tramp-tramp-file-p file))
                   ;; For consistency, run a local process with shell
                   ;; too.
                   (list shell-file-name
                         shell-command-switch
                         (concat arcmd " " arargs " "
                                 (comint-quote-filename
                                  (expand-file-name file))))
                 ;; Shamelessly stolen from
                 ;; `tramp-handle-shell-command'
                 (append
                  (cons (tramp-get-method-parameter
                         (tramp-file-name-method
                          (tramp-dissect-file-name file))
                         'tramp-remote-shell)
                        (tramp-get-method-parameter
                         (tramp-file-name-method
                          (tramp-dissect-file-name file))
                         'tramp-remote-shell-args))
                  (list (concat arcmd " " arargs " "
                                (comint-quote-filename
                                 (tramp-file-name-localname
                                  (tramp-dissect-file-name
                                   (expand-file-name file))))))))))
    (apply #'process-file (car args) nil log nil (cdr args))))

(defun arview-view (filename &optional tempdir args)
  "Extract the archive FILENAME and open its dired buffer.
The type of the archive determined with the function
`arview-archive-type'.  The archive extracted using the archive
program associated with the archive type (see `arview-types').
ARGS is additional arguments fo the archive program.

The temporary directory where the archive is extracted to is

TEMPDIR/arview-FILENAME.<random-string>

Set `arview-buffer-p' to t or FILENAME if FILENAME is a remote
file.  The variable is local to the temporary directory buffer."
  (let ((ar (cdr (assoc (arview-archive-type filename) arview-types)))
        (tempdir (if tempdir tempdir temporary-file-directory)))
    (if (not ar)
        (error "Unknown type of archive file: %s" filename)
      (let ((log (get-buffer-create arview-log-buffer-name))
            (file (arview-copy-remote-file filename tempdir)))
        (find-file
         (let ((temporary-file-directory tempdir))
           (make-temp-file (concat "arview-"
                                   (file-name-nondirectory filename)
                                   ".")
                           t)))
        (setq arview-buffer-p
              (if (string-equal file filename) t file))
        (with-current-buffer log
          (delete-region (point-min) (point-max)))
        (unless (zerop (arview-process-file (car ar)
                                            (concat (cadr ar) args)
                                            file
                                            log))
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

(defun arview-process-prefix-arg (arg)
  "Read from the minibuffer a temp dir and additional args.
When `arview' or `arview-dired' commands called with one prefix
argument, prompt for another temporary directory, not the default
one.  With two prefix arguments also promt for additional
arguments for the archive command.

ARG is the value of the prefix argument `arview' and
`arview-dired' called with."
  (if (equal arg '(4))
      (list (expand-file-name
             (read-directory-name "Temporary directory: "
                                  temporary-file-directory
                                  nil
                                  t)))
    (if (equal arg '(16))
        (list (expand-file-name
               (read-directory-name "Temporary directory: "
                                    temporary-file-directory
                                    nil
                                    t))
              (concat " " (read-string "Additional arguments: "))))))

;;;###autoload
(defun arview-dired (arg)
  "View the arview under point in the current dired buffer.
Process ARG using `arview-process-prefix-arg'.  See
`arview-view'."
  (interactive "P")
  (if (eq major-mode 'dired-mode)
      (let ((file (dired-get-filename)))
        (apply #'arview-view file (arview-process-prefix-arg arg)))))

(unless (lookup-key dired-mode-map [C-return])
  (define-key dired-mode-map [C-return] 'arview-dired))

;;;###autoload
(defun arview (arg filename)
  "Ask for the archive FILENAME and view it.
Process ARG using `arview-process-prefix-arg'.  See
`arview-view'."
  (interactive "P\nfArchive file name: ")
  (apply #'arview-view filename (arview-process-prefix-arg arg)))

(provide 'arview)
;;; arview.el ends here

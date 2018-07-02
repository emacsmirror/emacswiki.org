;;; dired-extension.el --- Some extension for dired

;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008 ~ 2016, Andy Stewart, all rights reserved.
;; Created: 2008-10-11 22:57:07
;; Version: 0.3
;; Last-Updated: 2018-07-02 17:30:09
;; URL:
;; Keywords: dired
;; Compatibility: GNU Emacs 27.0.50

;; This file is not part of GNU Emacs

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

;;; Features that might be required by this library:
;;
;;

;;; Overview:
;;
;; This package is some extension for `dired'.
;;

;;; Commentary:
;;
;; This package is some extension for `dired'.
;;

;;; Installation:
;;
;; Put dired-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'dired-extension)
;;
;; No need more

;;; Configuration:
;;
;;
;;

;;; Change log:
;;
;; 2016/07/02
;;      * Move function `file-binary-p' to `moccur-extension.el'
;;
;; 2016/06/29
;;      * Move moccur function to `moccur-extension.el'.
;;
;; 2016/6/6
;;      * Build new function `moccur-grep-find-without-binary-files' that make `moccur-grep-find-pwd' remove binary files from search result.
;;        I hate cross binary files in moccur search result.
;;
;; 2008/10/11
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require

;;; Code:
(defvar my-dired-omit-status t
  "The status of dired omit file.")
(defvar my-dired-omit-regexp "^\\.?#\\|^\\..*"
  "The regexp string that matching omit files.")
(defvar my-dired-omit-extensions '(".cache")
  "The list that matching omit file's extension.")

;; Advice `dired-run-shell-command' with asynchronously.
(defadvice dired-run-shell-command (around dired-run-shell-command-async activate)
  "Postfix COMMAND argument of `dired-run-shell-command' with an ampersand.
If there is none yet, so that it is run asynchronously."
  (let* ((cmd (ad-get-arg 0))
         (cmd-length (length cmd))
         (last-cmd-char (substring cmd
                                   (max 0 (- cmd-length 1))
                                   cmd-length)))
    (unless (string= last-cmd-char "&")
      (ad-set-arg 0 (concat cmd "&")))
    (save-window-excursion ad-do-it)))

(defun dired-sort-method ()
  "The sort method of `dired'."
  (let (buffer-read-only)
    (forward-line 2) ;; beyond dir. header
    (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))

(defun dired-omit-method ()
  "The omit method of dired."
  (when my-dired-omit-status
    (setq dired-omit-mode t)
    (setq dired-omit-files my-dired-omit-regexp)
    (setq dired-omit-extensions my-dired-omit-extensions)))

(defun dired-toggle-omit ()
  "Toggle omit status of dired files."
  (interactive)
  (if my-dired-omit-status
      (let ((dired-omit-size-limit nil))
        (setq dired-omit-mode nil)
        (dired-omit-expunge)
        (setq my-dired-omit-status nil))
    (setq dired-omit-mode t)
    (setq my-dired-omit-status t))
  (revert-buffer))

(defun dired-get-size ()
  "Get total size of marked files with `du' command.
If not marked any files, default is current file or directory."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*\\(total\\|总用量\\)$")
                 (match-string 1))))))

(defun dired-rename-with-copy ()
  "Rename name in Dired, and copy current name in yank."
  (interactive)
  (dired-copy-filename-as-kill)
  (dired-do-rename))

(defun dired-up-directory-single ()
  "Return up directory in single window.
When others visible window haven't current buffer, kill old buffer after `dired-up-directory'.
Otherwise, just `dired-up-directory'."
  (interactive)
  (let ((old-buffer (current-buffer))
        (current-window (selected-window)))
    (dired-up-directory)
    (catch 'found
      (walk-windows
       (lambda (w)
         (with-selected-window w
           (when (and (not (eq current-window (selected-window)))
                      (equal old-buffer (current-buffer)))
             (throw 'found "Found current dired buffer in others visible window.")))))
      (kill-buffer old-buffer))))

(defun dired-find-file+ ()
  "Like `dired-find-file'.
When open directory, if others visible window have this directory, do `find-file'.
Otherwise do `find-alternate-file'.
When open file, always use `find-file'."
  (interactive)
  (set-buffer-modified-p nil)
  (let ((file (dired-get-file-for-visit))
        (old-buffer (current-buffer))
        (current-window (selected-window)))
    (if (file-directory-p file)
        (catch 'found
          (walk-windows
           (lambda (w)
             (with-selected-window w
               (when (and (not (eq current-window (selected-window)))
                          (equal old-buffer (current-buffer)))
                 (find-file file)
                 (throw 'found "Found current dired buffer in others visible window.")))))
          (find-alternate-file file))
      (find-file file))))

(defun dired-serial-rename (dir ext name start)
  "Rename sequentially a set of file with the extension EXT.
In a repertory DIR with the name name + the start number start."
  (interactive "fDir: \nsExt(no dot): \nsName: \nnStart: ")
  (find-file dir)
  (let (ls-dir
        new-ls-dir
        n
        c)
    (setq ls-dir (file-expand-wildcards (format "*.%s" ext) t))
    (setq new-ls-dir nil)
    (setq n 0)
    (while (< n (length ls-dir))
      (if (< start 10)
          (push (concat dir name (format "0%s" start) "." ext) new-ls-dir)
        (push (concat dir name (format "%s" start) "." ext) new-ls-dir))
      (incf start)
      (incf n))
    (setq ls-dir (reverse ls-dir))
    (setq c 0)
    (dolist (i ls-dir)
      (rename-file i (nth c new-ls-dir))
      (incf c))))

(defun dired-next-file-line ()
  "Move to the next dired line that have a file or directory name on it."
  (interactive)
  (call-interactively 'dired-next-line)
  (if (eobp)
      (dired-previous-line 1)))

(defun dired-move-to-first-file ()
  "Move cursor to first file of dired."
  (interactive)
  (goto-char (point-min))
  (while (not (dired-move-to-filename))
    (call-interactively 'dired-next-line)))

(defun dired-move-to-last-file ()
  "Move cursor to last file of dired."
  (interactive)
  (goto-char (point-max))
  (while (not (dired-move-to-filename))
    (call-interactively 'dired-previous-line)))

(defun dired-previous-file-line ()
  "Move to the previous dired line that have a file or directory name on it."
  (interactive)
  (call-interactively 'dired-previous-line)
  (if (not (dired-move-to-filename))
      (dired-next-line 1)))

(defun dired-nautilus ()
  "Load current directory with nautilus."
  (interactive)
  (shell-command
   (concat "nautilus " (dired-current-directory))))

(defun dired-touch-now (touch-file)
  "Do `touch' command with TOUCH-FILE."
  (interactive "sTouch file: ")
  (cd (dired-current-directory))
  (shell-command
   (concat "touch \""
           ;; if filename is begin with `-', add '-- ' before file-name
           (if (string-match "^-.*" touch-file) "-- ")
           touch-file "\""))
  (sit-for 0.1)
  (revert-buffer)
  (dired-goto-file (concat (dired-current-directory) touch-file)))

(defun dired-gnome-open-file ()
  "Opens the current file in a Dired buffer."
  (interactive)
  (gnome-open-file (dired-get-file-for-visit)))

(defun gnome-open-file (filename)
  "gnome-opens the specified file."
  (interactive "fFile to open: ")
  (let ((process-connection-type nil))
    (start-process "" nil "/usr/bin/xdg-open" filename)))

(defun gnome-open-buffer ()
  "Open current buffer file with gnome."
  (interactive)
  (gnome-open-file buffer-file-name))

(defun dir-file-ext-my (file)
  "Given a full file's path name, returns a list of directory, filename
and extension.  The extension contains the ., and the directory
contains the /
See also file-name-directory and file-name-nondirectory.."
  (interactive "s String: ")
  (with-temp-buffer
    (insert file)
    (goto-char (point-max))
    (let ((aa (progn
                (goto-char (point-max))
                (search-backward "/" nil t)))
          (bb (progn
                (goto-char (point-max))
                (search-backward "." nil t))))
      (setq aa (if (null aa) (point-min) (+ aa 1)))
      (if (null bb) (setq bb (point-max)))
      (if (> aa bb) (setq bb (point-max))) ;that means that the . occurs in
                                        ;the path name rather than filename.
      (let ((cc
             (list
              (buffer-substring (point-min) aa)
              (buffer-substring aa bb)
              (buffer-substring bb (point-max)))))
        (if (interactive-p) (message "%S" cc))
        cc))))

(defun find-lisp-find-dired-pwd (regexp)
  "Find files in DIR, matching REGEXP."
  (interactive "sMatching regexp: ")
  (find-lisp-find-dired default-directory regexp))

(provide 'dired-extension)
;;; dired-extension.el ends here

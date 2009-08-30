;;; disk.el --- simplified find-file, revert-file, save-buffer interface

;; Copyright (C) 2002  Alex Schroeder
;; Copyright (C) 2008  Peter Barabas

;; Author: Alex Schroeder <alex@gnu.org> (2002)
;;         Peter Barabas <peter.barabas+disk@gmail.com> (2008)
;; Maintainer: Alex Schroeder <alex@gnu.org> (2002)
;;             Peter Barabas <peter.barabas+disk@gmail.com> (2008)
;; Version: 1.0.1
;; Keywords: convenience
;; URL: http://www.emacswiki.org/emacs/DiskKey

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This unifies disk operations on a file.  Use it by binding it to a
;; key.  Example setup in your ~/.emacs file:
;;
;; (global-set-key (kbd "<f9>") 'disk)
;; (autoload 'disk "disk" "Save, revert, or find file." t)

;;; Code:

(defgroup disk nil
  "Simplified find-file, revert-file, save-buffer interface."
  :group 'convenience)

(defcustom disk-find-file-function 'find-file
  "Function to use for visiting files."
  :type 'function
  :group 'disk)

(defvar disk-access nil)
(make-variable-buffer-local 'disk-access)

(add-hook 'find-file-hooks 'disk-notice)
(add-hook 'after-save-hook 'disk-notice)

(defun disk-file-modification-time ()
  "Return modification time of the visited file."
  (nth 5 (file-attributes (buffer-file-name))))

(defun disk-notice ()
  "Store access time in `disk-acess'."
  (setq disk-access (disk-file-modification-time)))

(defun disk-file-modified-p ()
  "Return non-nil if the visited file has been modified."
  (not (equal disk-access
	      (disk-file-modification-time))))

(defun disk ()
  "Do the right thing with files.

If the buffer has no file, find a file.  If the buffer needs saving, and
the file is unchanged, save the buffer.  If the buffer needs saving, and
the file has changed, warn the user.  If the buffer is unchanged, and
the file has changed, revert the buffer.  Else do nothing."
  (interactive)
  (cond ((not (buffer-file-name))
	 (call-interactively disk-find-file-function))
	((and (buffer-modified-p)
	      (not (disk-file-modified-p)))
	 (save-buffer))
	((and (buffer-modified-p)
	      (disk-file-modified-p))
	 (error "Buffer must be saved, but the file has also changed."))
	((and (not (buffer-modified-p))
	      (disk-file-modified-p))
	 (revert-buffer t t))
	(t
	 (message "Nothing to do"))))

;; Initialize all buffers
(save-excursion
  (dolist (buf (buffer-list))
    (set-buffer buf)
    (when (buffer-file-name)
      (disk-notice))))

(provide 'disk)

;;; disk.el ends here.

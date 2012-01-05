;;; mailcap-view.el --- View files based on the mime-type of the file extension

;; Copyright (C) 2003 Doug Alcorn <doug@lathi.net>
;; Author: Doug Alcorn <doug@lathi.net>
;; Version: 1.2

;; This file is not yet part of GNU Emacs.

;; mailcap-view.el free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; mailcap-view.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; mailcap-view.el is free software

;;; Commentary:

;; mailcap-view.el is a set of functions that allows you to do general
;; file browsing and viewing from within emacs.  The pimary function
;; is mailcap-view-file which launches a shell-command using the
;; default mime-handler.  The find-file-hooks function supplied allows
;; you to open files who's mime-type is text/* or nil in the normal
;; way.  However, files whos mime-type is defined and not text/* are
;; launched with an external viewer.  In short, hitting return in a
;; dired buffer on a .jpg will display the jpeg if you have your
;; mailcap setup properly.

;; I'm sure the style of this code isn't optimal.  Feel free to hack
;; on this and send me patches.

(require 'cl)
(require 'mailcap)

(defvar mailcap-view-file-async t
  "Non-nil means `mailcap-view-file' will execute the view in the background.")

(defun* mailcap-view-file (filename &optional (async t))
  "Using the file extension, view the FILENAME with the appropriate
handler as determined by `mailcap-mime-info'.  If ASYNC is non-nil,
run the viewer in the background and store the output in the `*Async
Shell Command*' buffer.  Otherwise the viewer is run in the foreground
and blocks emacs.  The default for ASYNC is t."
  (interactive "fFile to view: ")
  (let* ((view-async (if (interactive-p) mailcap-view-file-async async))
	 (ext (file-name-extension filename))
	 (mime-handler (if (stringp ext) 
			   (mailcap-mime-info (mailcap-extension-to-mime ext)) nil))
	 (command-format (if (stringp mime-handler)
			     (concat mime-handler (if view-async " &" ""))))
	 (output-buffer (if view-async (format "*Async Shell Command: %s*" filename)
			  (format "*Shell Command Output: %s*" filename))))
    (and command-format
	 (save-window-excursion
	   (shell-command (format command-format (expand-file-name filename)) output-buffer)
	   (bury-buffer output-buffer)))))

(defun mailcap-view-find-file-hook ()
  "Hook function to view the file with `mailcap-view-file' if the file isn't a text file."
  (let* ((back (other-buffer nil t))
	 (filename (buffer-file-name))
	 (ext (file-name-extension filename))
	 (mime-type (if (stringp ext) (mailcap-extension-to-mime ext)))
	 (major (if (stringp mime-type) (car (split-string mime-type "/")))))
    (if (and (stringp major) (not (string= "text" major))
	     (stringp mime-type) (not (string-match "emacs" mime-type)))
      (let ((buffer (find-buffer-visiting filename)))
	(mailcap-view-file filename t)
	(if buffer
	    (progn (kill-buffer buffer)
		   (switch-to-buffer back)))))))

(provide 'mailcap-view)

;;; Changes

;; 1.2 - same changes for mailcap-view-find-file-hook
;; 1.1 - fix for mailcap-view-file to handle files with no extensions
;; 1.0 - initial public release

;;; mailcap-view.el ends here.

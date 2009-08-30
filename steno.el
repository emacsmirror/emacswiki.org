;;; steno.el --- edit time-stamped entries in text notepads under Emacs.
;;; $Revision: 1.1 $

;; Copyright (C) 1996 David Megginson

;; Author: David Megginson (dmeggins@uottawa.ca)

;; steno.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; steno.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs or XEmacs; see the file COPYING.  If not,
;; write to the Free Software Foundation, Inc., 59 Temple Place -
;; Suite 330, Boston, MA 02111-1307, USA.

 
;;; Commentary:

;; This package has some similarities to Emacs' Changelog support,
;; but it is designed for a different purpose.  It allows users
;; to keep a collection of simple logs or diaries ("steno pads")
;; in a single directory, and to add new entries to them quickly.
;;
;; To install this package, copy steno.el to a directory on your
;; load-path and optionally compile it (it is not speed-critical),
;; then add the following to your .emacs file, site-start.el, or any
;; other relevant place:
;;
;; (autoload 'steno "steno" nil t)
;; (autoload 'steno-view "steno" nil t)
;;
;; You can bind either or both of these to menus or to key strokes.
;; If you wanted to bind 'steno to "\C-cs", for example, you could
;; use the command
;;
;; (global-set-key "\C-cs" 'steno)
;;
;; You may also want to change the value of the 'steno-pad-directory
;; variable.  By default it is set to $HOME/.steno, which is fine for
;; Unix users, but DOS/Windows users might prefer a different default.
;; The 'steno or 'steno-view functions will offer to create the
;; directory if it doesn't already exist.
;;
;; Use "M-x steno-view" to open a steno pad without starting a new
;; entry (though you can always do so later), or "M-x steno" to start
;; a new entry right away.  Inside a steno pad, the most important key
;; strokes are "\C-c\C-a" to add a new time-stamped entry, "\C-c\C-n"
;; to move to the next entry, "\C-c\C-p" to move to the previous
;; entry, and "\C-c\C-c" to save the steno pad and remove its window
;; from the display.  'steno-mode is derived from 'text-mode, so any key
;; sequences defined in 'text-mode-map will be available in
;; 'steno-mode as well.

;; I may add menu support and multi-file searching in a future release.

 
;;; Code:

(require 'derived)

;;
;; Variable for specifying the location of the steno directory.
;;
(defvar steno-pad-directory (expand-file-name "~/.steno")
  "Variable to specify the location of the steno-pad directory.
The default is good for Unix, but should be changed for Windows/DOS.")

;;
;; Regular expression for recognising entries.
;;
(defvar steno-entry-regexp "^\\*\\* [SMTWF][a-z]+.*[0-9][0-9][0-9][0-9]$"
  "Regular expression for recognising entries.")

;;
;; Regular expression for hilighting.
;;
(defvar steno-font-lock-keywords
  (list
   (list steno-entry-regexp 0 'font-lock-keyword-face)
   (list "^-\\*-Steno-\\*-.*$" 0 'font-lock-comment-face)))

;;
;; Create a new major mode for Steno pads.
;;
(define-derived-mode steno-mode text-mode "Steno"
  "Major mode for editing or viewing steno pads.
Steno pads are simple text files, kept in a common directory, with
dated entries (similar to a change log).

\\{steno-mode-map}"

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(steno-font-lock-keywords t))
  (define-key steno-mode-map "\C-c\C-n" 'steno-next-entry)
  (define-key steno-mode-map "\C-c\C-p" 'steno-previous-entry)
  (define-key steno-mode-map "\C-c\C-a" 'steno-add-entry)
  (define-key steno-mode-map "\C-c\C-c" 'steno-close)
  (run-hooks 'steno-mode-hook))

;;
;; Show a steno pad and start a new entry.
;;
(defun steno (pad)
  "Show a steno pad and start a new entry.
If called interactively, the user will be prompted for the pad name.
For more information, see steno-mode."
  (interactive (steno-choose-pad))
  (steno-view pad)
  (steno-add-entry))

;;
;; Show a steno pad, but don't start a new entry.
;;
(defun steno-view (pad)
  "Show a steno pad and start a new entry.
If called interactively, the user will be prompted for the pad name.
For more information, see steno-mode."
  (interactive (steno-choose-pad))
  (let ((buffer (find-file-noselect (concat steno-pad-directory
					    "/"
					    pad)
				    t)))
    (set-buffer buffer)
    (steno-mode)
    (switch-to-buffer-other-window buffer)
    (if (not (file-exists-p (buffer-file-name)))
	(insert (format "-*-Steno-*- (%s)\n\n" pad)))))

;;
;; Prompt the user for a steno pad.
;;
(defun steno-choose-pad ()
					; Does the directory exist?
  (if (file-exists-p steno-pad-directory)

					; Is it readable?
      (cond ((not (file-readable-p steno-pad-directory))
	     (error "steno-pad-directory \"%s\" is not readable."
		    steno-pad-directory))
					; Is it a directory?
	    ((not (car (file-attributes steno-pad-directory)))
	     (error "steno-pad-directory \"%s\" is not a directory.")))

					; If it doesn't exist, can
					; we create it?
    (if (y-or-n-p (format "Create directory \"%s\"? " steno-pad-directory))
	(make-directory steno-pad-directory t)
      (error "steno-pad-directory \"%s\" does not exist.")))

					; Read the pad from the user.
  (let ((pads (directory-files steno-pad-directory))
	(pad nil))

    (setq pad (completing-read "Select a steno pad: "
			       (mapcar 'list pads)))

					; If it is new, can we create it?
    (if (not (file-exists-p (concat steno-pad-directory
				    "/"
				    pad)))
	(if (not (y-or-n-p (format "Create new pad \"%s\"? " pad)))
	    (error "Pad \"%s\" does not exist.")))

					; Return the full path.
    (list pad)))

;;
;; Display a list of steno files.  Allow user to select a file
;; and edit it.  Attention: very simple minded code!
;;
(defun steno-dired ()
  "Invoke dired on the steno directory.
Allow user to select steno pads and edit them.
Very simple minded code!"
  (interactive)
  (dired (concat steno-pad-directory "/*[!~]"))
  (rename-buffer "*Steno-List*"))

;;
;; Add an entry to a steno pad.
;;    
(defun steno-add-entry ()
  "Add a time-stamped entry to the end of a steno pad.
See steno-mode for more information."
  (interactive)
  (goto-char (point-max))
  (cond ((search-backward-regexp "[^ \t\n\r\f]" nil t)
	 (end-of-line)
	 (delete-region (point) (point-max))
	 (insert (format "\n\n\n** %s\n\n" (current-time-string))))
	(t
	 (goto-char (point-min))
	 (delete-region (point) (point-max))
	 (insert (format "** %s\n\n" (current-time-string))))))

;;
;; Move forward one entry in a steno pad.
;;
(defun steno-next-entry (n)
  "Move forward to the n entries in a steno pad.
If called interactively, will move the number of entries
specified by the prefix argument."
  (interactive "p")
  (while (> n 0)
    (let ((old (point)))
      (beginning-of-line)
      (forward-char 1)
      (if (search-forward-regexp steno-entry-regexp
				 nil t)
	  (beginning-of-line)
	(progn
	  (goto-char old)
	  (error "No more entries in steno pad.")))
      (setq n (1- n)))))

;;
;; Move backwards one entry in a steno pad.
;;
(defun steno-previous-entry (n)
  "Move backwards n entries in a steno pad.
If called interactively, will move the number of entries
specified by the prefix argument."
  (interactive "p")
  (while (> n 0)
    (let ((old (point)))
      (if (search-backward-regexp "^\*\* [SMTWF][a-z]+.*[0-9][0-9][0-9][0-9]$"
				 nil t)
	  (beginning-of-line)
	(progn
	  (goto-char old)
	  (error "No more entries in steno pad.")))
      (setq n (1- n)))))

;;
;; Finish with a steno pad -- save to disk and hide it.
;;
(defun steno-close ()
  "Finish with a steno pad -- save it to disk and kill the buffer."
  (interactive)
  (save-buffer)
  (let ((buf (current-buffer)))
    (delete-windows-on buf)
    (kill-buffer buf)))
  

;;
;; For (require 'steno)
;;
(provide 'steno)

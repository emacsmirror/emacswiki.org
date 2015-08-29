;;; ido-jb-misc-extras.el --- Miscellaneous extra ido related commands

;; Filename: ido-jb-misc-extras.el
;; Description: miscellaneous functions for `dired' and `image-dired'
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2015, Joe Bloggs, all rites reversed.
;; Created: 2015-08-28 15:30:22
;; Version: 0.1
;; Last-Updated: 2015-08-28 15:30:22
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/ido-jb-misc-extras
;; Keywords: unix
;; Compatibility: GNU Emacs 24.5.1
;; Package-Requires: 
;;
;; Features that might be required by this library:
;;
;; run-assoc
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
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 12k9zUo9Dgqk8Rary2cuzyvAQWD5EAuZ4q
;;
;; This library provides various miscellaneous `dired' related commands & functions
;; that I use occasionally. 
;;

;;; Commands:
;;
;; Below is a complete command list:
;;
;; `ido-execute-extended-command'
;;  Use `ido' to select a command to execute.
;; `ido-bookmark-jump'
;;  Switch to bookmark interactively using `ido'.
;; `ido-run-associated-program'
;;  Open the current candidate file with `run-associated-program'.
;;  `ido-goto-favourite'
;;  Choose commonly used file/dired buffer with ido, and jump to it.
;;  `ido-goto-recent-file'
;;  Choose recently used file with ido, and jump to it.
;;  `ido-goto-recent-dir'
;;  Choose recently used dired buffer with ido, and jump to it.
;;
;;
;;; Functions:
;;
;; The following functions are defined:
;;
;; `ido-sort-mtime'
;;  Sort ido filelist by modification time instead of alphabetically.
;; `ido-completing-read-multiple'
;;  Read multiple items with `ido-completing-read'.
;;
;;; Customize:
;;
;; `ido-favourites-list'
;; List of choice-action pairs for use with the `ido-goto-favourite' command.
;;

;;; Installation:
;;
;; Put ido-jb-misc-extras.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'ido-jb-misc-extras)

;; To ensure files are sorted by modification time when using `ido-find-file',
;; put the following line somewhere in your startup file (~/.emacs):

;; (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)



;;; Require
(eval-when-compile (require 'cl))

;;;###autoload
(defun ido-run-associated-program nil
  "Open the current candidate file with `run-associated-program'."
  (interactive)
  (run-associated-program
   (concat ido-current-directory (car ido-matches))))

;;;###autoload
(defun ido-bookmark-jump (bname)
  "Switch to bookmark interactively using `ido'."
  (interactive (list (ido-completing-read "Bookmark: " (bookmark-all-names) nil t)))
  (bookmark-jump bname))

(defvar ido-execute-command-cache nil)
;;;###autoload
(defun ido-execute-extended-command ()
  "Use `ido' to select a command to execute."
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (progn
       (unless ido-execute-command-cache
	 (mapatoms (lambda (s)
		     (when (commandp s)
		       (setq ido-execute-command-cache
			     (cons (format "%S" s) ido-execute-command-cache))))))
       ido-execute-command-cache)))))

(defun ido-sort-mtime nil
  "Sort ido filelist by modification time instead of alphabetically."
  (if (not (or (equal "/" ido-current-directory)
	       (equal "/sudo:" ido-current-directory)
	       (equal "/su:" ido-current-directory)))
      (progn (setq ido-temp-list
		   (sort ido-temp-list
			 (lambda (a b)
			   (let* ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
				  (tb (nth 5 (file-attributes (concat ido-current-directory b))))
				  (ta0 (nth 0 ta))
				  (tb0 (nth 0 tb))
				  (ta1 (nth 1 ta))
				  (tb1 (nth 1 tb)))
			     (if (not ta) nil
			       (if (not tb) t
				 (if (= ta0 tb0)
				     (> ta1 tb1)
				   (> ta0 tb0))))))))
	     (ido-to-end ;; move . files to end (again)
	      (delq nil (mapcar
			 (lambda (x) (if (string-equal (substring x 0 1) ".") x))
			 ido-temp-list))))))

;; need this variable for the next function
(defcustom ido-favourites-list nil
  "List of choice-action pairs for use with the `ido-goto-favourite' command.
     Each element should be a cons cell (NAME . COMMAND) where NAME is the name
     displayed in the ido prompt, and COMMAND is the command to be executed when
     NAME is selected."
  :type 'alist
  :group 'ido)

;;;###autoload
(defun ido-goto-favourite (place)
  "Choose commonly used file/dired buffer with ido, and jump to it."
  (interactive
   (list (ido-completing-read "Favourite: "
			      (let ((items))
				(dolist (item ido-favourites-list)
				  (setq items (append (list (car item)) items)))
				items) nil t)))
  (funcall (cdr (assoc place ido-favourites-list))))

;;;###autoload
(defun ido-goto-recent-file (file)
  "Choose recently used file with ido, and jump to it."
  (interactive
   (list (let* ((filepaths (let ((items))
			     (dolist (item file-name-history)
			       (if (and (stringp item)
					(not (string-match ":" item))
					(file-regular-p item)
					(not (member item items)))
				   (add-to-list 'items item t)))
			     items))
		(filenames (mapcar 'file-name-nondirectory filepaths))
		(numfilenames (length filenames))
		;; get filename from user with ido
		(chosenfilename (ido-completing-read "Recent file: " filenames))
		(afterfilenameslist (member chosenfilename filenames))
		(posinlist (- numfilenames (length afterfilenameslist))))
	   (nth posinlist filepaths))))
  (find-file file))

;;;###autoload
(defun ido-goto-recent-dir (place)
  "Choose recently used dired buffer with ido, and jump to it."
  (interactive
   (list (ido-completing-read "Recent dir: "
			      (let ((items))
				(dolist (item file-name-history)
				  (if (and (stringp item)
					   (not (string-match ":" item))
					   (> (length item) 0))
				      (let ((itemd (file-name-directory item)))
					(if (and (stringp itemd) (file-directory-p itemd) (not (member itemd items)))
					    (add-to-list 'items itemd t)))))
				items))))
  (dired place))

(defun ido-completing-read-multiple (prompt choices &optional predicate require-match initial-input hist def sentinel)
  "Read multiple items with ido-completing-read. 
   Reading stops when the user enters SENTINEL. By default, SENTINEL is
   \"*done*\". SENTINEL is disambiguated with clashing completions
   by appending _ to SENTINEL until it becomes unique. So if there
   are multiple values that look like SENTINEL, the one with the
   most _ at the end is the actual sentinel value. See
   documentation for `ido-completing-read' for details on the
   other parameters."
  (let ((sentinel (if sentinel sentinel "*done*"))
	(done-reading nil)
	(res ()))
    ;; uniquify the SENTINEL value
    (while (find sentinel choices)
      (setq sentinel (concat sentinel "_")))
    (setq choices (cons sentinel choices))
    ;; read some choices
    (while (not done-reading)
      (setq this-choice (ido-completing-read prompt choices predicate require-match initial-input hist def))
      (if (equal this-choice sentinel)
	  (setq done-reading t)
	(setq res (cons this-choice res))))
    ;; return the result
    res))

(provide 'ido-jb-misc-extras)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "ido-jb-misc-extras.el" (buffer-name) (buffer-string) "update")

;;; ido-jb-misc-extras.el ends here

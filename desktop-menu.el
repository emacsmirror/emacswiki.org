;;; desktop-menu.el --- menu for managing Emacs desktops

;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Olaf Sylvester

;; Author: Olaf Sylvester <ole_i_dont_like_spam at geekware . de>
;; Maintainer: Štěpán Němec <stepnem@gmail.com>
;; Keywords: convenience
;; Time-stamp: "2009-11-14 00:04:01 CET stepnem"
;; Version: 1.0
;; URL: http://www.emacswiki.org/emacs/desktop-menu.el


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

;; This package provides a *Desktop Menu* for managing multiple Emacs
;; desktops, possibly in a single directory.
;;
;; Start the menu with `M-x desktop-menu RET'.
;;
;; Press `?' for help in the Desktop Menu.

;; Olaf speaketh:
;; Why a Desktop Menu?
;; The Emacs package `desktop' is a really nice menu for saving and reading
;; various Emacs desktops in different directories.
;; But often I have the problems:
;; - I want to save various Emacs desktops in the same directory and
;; - I lost the overview for directories with a Emacs desktop.
;; So I've developed a menu which handles various Emacs desktops
;; in one directory and I can give each desktop a nice name.

;;; Customization:

;; There is a customization group called `desktop-menu' in group `desktop'.
;; Start customization by `M-x customize-group RET desktop-menu RET'.

;;; History:
;;

;;; Code:

(require 'desktop)
(eval-when-compile (require 'cl))

;; ----------------------------------------------------------------------------
;; Customization variables
;; ----------------------------------------------------------------------------

(defgroup desktop-menu nil
  "Managing multiple desktops."
  :group 'desktop)

(defcustom desktop-menu-directory "~"
  "Directory storing the desktop files."
  :type  'directory
  :group 'desktop-menu)

(defcustom desktop-menu-base-filename
  (convert-standard-filename ".emacs.desktop")
  "Base filename for different desktop files."
  :type 'file
  :group 'desktop-menu)

(defcustom desktop-menu-list-file
  (convert-standard-filename ".emacs.desktops")
  "File listing all the desktop files in a single directory."
  :type 'file
  :group 'desktop-menu)

(defcustom desktop-menu-mode-hook nil
  "Hook run upon entering the *Desktop Menu*."
  :type 'hook
  :group 'desktop-menu)

(defcustom desktop-menu-clear 'ask
  "Specifies the strategy for clearing the current desktop.
Desktop will be cleared by `desktop-clear'.
Possible values:
 `ask' -- Ask user what to do.
 `no'  -- Don't clear the current desktop.
 `yes' -- Clear the current desktop."
  :type '(choice (const :tag "Ask user" ask)
		 (const :tag "Don't delete desktop" no)
		 (const :tag "Always delete desktop" yes))
  :group 'desktop-menu)

(defcustom desktop-menu-ask-user-on-delete t
  "If non-nil, ask user before deleting a desktop."
  :group 'desktop-menu
  :type 'boolean)

(defcustom desktop-menu-sort-p t
  "If non-nil, sort desktops by names."
  :group 'desktop-menu
  :type 'boolean)

(defcustom desktop-menu-mode-font-lock-keywords
  (list
        (list "\\(^ Desktops in directory\\) \\(.+\\)"
	      '(1 font-lock-type-face append)
	      '(1 'bold append)
	      '(2 font-lock-function-name-face append))
        (list "^.  \\(.+?\\)\\([0-9]+ Buffer\\(s\\)?\\)"
	      '(1 font-lock-function-name-face)
	      '(2 font-lock-constant-face append)))
  "Fontlock settings for Desktop Menu."
  :type 'sexp
  :group 'desktop-menu)

;; ----------------------------------------------------------------------------
;; Variables for internal use only
;; ----------------------------------------------------------------------------

(defvar desktop-menu--desktops nil
  "List of all known desktops.")

(defvar desktop-menu--current-desktop-name nil
  "Name of the current desktop.")

(defvar desktop-menu--orig-window nil
  "Window we started Desktop Menu from.")

(defvar desktop-menu--orig-layout nil
  "Window configuration before starting Desktop Menu.")

(defvar desktop-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (key) (define-key map (number-to-string key) 'digit-argument))
	  '(1 2 3 4 5 6 7 8 9))
    (define-key map " "       'desktop-menu-down)
    (define-key map "-"       'negative-argument)
    (define-key map "?"       'desktop-menu-help)
    (define-key map "\C-g"    'desktop-menu-abort)
    (define-key map "\C-m"    'desktop-menu-load)
    (define-key map "\e-"     'negative-argument)
    (define-key map "^"       'desktop-menu-up-directory)
    (define-key map "c"       'desktop-menu-change-directory)
    (define-key map "d"       'desktop-menu-delete)
    (define-key map "g"       'desktop-menu-refresh)
    (define-key map "m"       'desktop-menu-merge)
    (define-key map "n"       'desktop-menu-new)
    (define-key map "p"       'desktop-menu-up)
    (define-key map "q"       'desktop-menu-quit)
    (define-key map "r"       'desktop-menu-rename)
    (define-key map "s"       'desktop-menu-save)
    (define-key map "x"       'desktop-menu-clear)
    (define-key map [down]    'desktop-menu-down)
    (define-key map [up]      'desktop-menu-up)
    map)
  "Keymap for `desktop-menu-mode'.")

;; ----------------------------------------------------------------------------
;; Desktop Menu functions
;; ----------------------------------------------------------------------------

(defun desktop-menu-initialise ()
  "Create the default directory for different desktop files."
  (if (not (file-exists-p desktop-menu-directory))
      (make-directory desktop-menu-directory)))

(defun desktop-menu-save-into (filename)
  "Save the current desktop into FILENAME."
  (interactive "F")
  (let ((desktop-base-file-name (file-name-nondirectory filename)))
    (desktop-save (file-name-directory filename))))

(defun desktop-menu-read (filename)
  "Load desktop of FILENAME."
  (interactive "F")
  (let ((desktop-dirname (file-name-directory filename))
	(desktop-base-file-name (file-name-nondirectory filename)))
    (message (format "File: %S Desktop dir: %S"
		     desktop-base-file-name
		     desktop-dirname))
    (desktop-read desktop-dirname)))

(defun desktop-menu-mode ()
  "Major mode for editing Emacs desktops.
\\<desktop-menu-mode-map>
Aside from the header line, each line describes one Emacs desktop
stored in `desktop-menu-directory'.

\\[desktop-menu-load] with the cursor on a desktop loads that desktop.
\\[desktop-menu-save] saves current desktop into the desktop under cursor.
\\[desktop-menu-quit] saves the desktop list and leaves *Desktop Menu*.
\\[desktop-menu-abort] aborts the menu without saving.

For faster navigation, use digit keys to supply a numeric argument directly.

Other keybindings:
\\[desktop-menu-load] -- Select the current line's desktop.
\\[desktop-menu-help]   -- Display this help text.
\\[desktop-menu-up-directory]   -- Go up one directory.
\\[desktop-menu-change-directory]   -- Change to another directory.
\\[desktop-menu-delete]   -- Delete the current line's desktop.
\\[desktop-menu-refresh]   -- Refresh the listing.
\\[desktop-menu-merge]   -- Merge the current line's desktop into the current desktop.
\\[desktop-menu-new]   -- Create a new desktop.
\\[desktop-menu-rename]   -- Rename the current line's desktop.
\\[desktop-menu-clear]   -- Clear the current Emacs desktop."

  (interactive)
  (kill-all-local-variables)
  (use-local-map desktop-menu-mode-map)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-verbose)
  (setq major-mode 'desktop-menu-mode
	mode-name "Desktop Menu"
	buffer-read-only t
	truncate-lines t
	font-lock-defaults '(desktop-menu-mode-font-lock-keywords t)
	font-lock-verbose nil)
  (run-hooks 'desktop-menu-mode-hook))

(defun desktop-menu-save-main-list (desktops directory)
  "Save DESKTOPS in DIRECTORY into `desktop-menu-list-file'."
  (desktop-menu-initialise)
  (let ((mainfile (expand-file-name desktop-menu-list-file directory)))
    (find-file mainfile)
    (erase-buffer)
    (insert ";; ------------------------------------------------------------\n"
            ";; Desktop Files for Emacs in directory "
	    (expand-file-name "." directory) "\n"
	    ";; ------------------------------------------------------------\n"
	    ";; Created " (current-time-string) "\n"
	    ";; Emacs version " emacs-version "\n\n"
	    "(setq desktop-menu--desktops\n"
	    "      (list\n")
    (while desktops
      (insert (format "       '(%S %S)\n"
		      (caar desktops)
		      (cadar desktops)))
      (setq desktops (cdr desktops)))
    (insert "       ))\n")
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun desktop-menu-read-main-list (directory)
  "Read list of desktops from `desktop-menu-list-file' in DIRECTORY.
Return a list of desktops."
  (let (desktop-menu--desktops
	desktops
	(mainfile (expand-file-name desktop-menu-list-file directory)))
    (if (file-exists-p mainfile)
	(save-window-excursion
	  (save-excursion
	    (load-file mainfile))))
    ;; remove non-existent desktop files.
    (setq desktops desktop-menu--desktops)
    (let ((list (copy-list desktop-menu--desktops)))
      (while list
	(if (not (file-exists-p (expand-file-name (cadar list) directory)))
	    (setq desktops
		  (remove (car list) desktops)))
	(setq list (cdr list))))
    ;; find more desktop files
    (let* ((base desktop-menu-base-filename)
	   (existing-in-main (mapcar #'cadr desktops))
	   (all (directory-files directory
				 nil
				 (concat (regexp-quote base)
					 "[0-9]*$"))))
      (while all
	(if (not (member (car all) existing-in-main))
	    (setq desktops
		  (append desktops
			  (list (list (if (string= (car all) base)
					  "Default"
					"No Name")
				      (car all))))))
	(setq all (cdr all))))
    (if desktop-menu-sort-p
	(sort desktops
	      (function (lambda (e1 e2)
			  (string< (car e1) (car e2)))))
      desktops)))

(defun desktop-menu-list (directory &optional read-main-p)
  "List all desktops in DIRECTORY in a *Desktop Menu* buffer.
With optional argument READ-MAIN-P non-nil, read the file
`desktop-menu-list-file' for the desktop list."
  (unless (file-readable-p directory)
    (error "%S is not a valid directory" directory))
  (switch-to-buffer (get-buffer-create "*Desktop Menu*"))
  (cd directory)
  (setq desktop-menu-directory directory)
  (when read-main-p
    (setq desktop-menu--desktops
          (desktop-menu-read-main-list directory)))
  (desktop-menu-mode)
  (let ((inhibit-read-only t)
	(desktops desktop-menu--desktops))
    (erase-buffer)
    (insert-string " Desktops in directory "
		   (expand-file-name "." desktop-menu-directory)
		   "\n")
    (while desktops
      (let* ((desktop (car desktops))
             (name (car desktop))
             (fname (cadr desktop)))
        (insert (if (string= name
			     desktop-menu--current-desktop-name)
		    "."
		  " ")
		(format "  %-30s %20s  %s"
			name
			(desktop-menu-extra-desktop-description
			 fname)
			fname))
	(newline))
      (setq desktops (cdr desktops)))
    (backward-delete-char 1)
    (beginning-of-line)
    (set-buffer-modified-p nil)
    (font-lock-fontify-buffer)
    (desktop-menu--set-window-height)
    (beginning-of-buffer)
    (when (and (not (search-forward-regexp "^\\." nil t))
               (not (eq (line-end-position) (point-max))))
      (next-line 1))
    (beginning-of-line)))

(defun desktop-menu-up (arg)
  "Move cursor up ARG lines in Desktop Menu."
  (interactive "p")
  (if (> 0 arg)
      (desktop-menu-down (- arg))
    (let ((arg (mod arg (length desktop-menu--desktops)))
	  (lines (count-lines (point-min) (point))))
      (if (>= arg lines)
	  (desktop-menu-down (- (count-lines (point-min) (point-max))
				(1+ arg)))
	(previous-line arg)))))

(defun desktop-menu-down (arg)
  "Move cursor down ARG lines in Desktop Menu."
  (interactive "p")
  (when (eq 0 (count-lines (point-min) (point)))
    (progn (next-line 1)
           (setq arg (1- arg))))
  (if (> 0 arg)
      (desktop-menu-up (- arg))
    (let ((arg (mod arg (length desktop-menu--desktops)))
	  (lines (count-lines (point) (point-max))))
      (if (>= arg lines)
	  (desktop-menu-up (- (count-lines (point-min) (point-max)) arg 1))
	(next-line arg)))))

(defun desktop-menu-new (name)
  "Create a new desktop with name NAME."
  (interactive "sName of the new desktop: ")
  (setq desktop-menu--desktops
	(append desktop-menu--desktops
		(list (list name
			    (desktop-menu-new-file desktop-menu-directory)))))
  (desktop-menu-list desktop-menu-directory)
  (message "Now you can save the newly created desktop."))

(defun desktop-menu-refresh ()
  "Refresh Desktop Menu."
  (interactive)
  (desktop-menu-list desktop-menu-directory))

(defun desktop-menu-change-directory (directory)
  "Change to the desktop list in directory DIRECTORY."
  (interactive "DChange to directory: ")
  (unless (file-readable-p directory)
    (error "%S is not a valid directory" directory))
  (desktop-menu-save-main-list desktop-menu--desktops
			       desktop-menu-directory)
  (setq desktop-menu-directory directory
	default-directory directory)
  (desktop-menu-list desktop-menu-directory t))

(defun desktop-menu-up-directory ()
  "Switch to the parent directory of `desktop-menu-directory'."
  (interactive)
  (desktop-menu-change-directory
   (file-name-directory (directory-file-name desktop-menu-directory))))

(defun desktop-menu-rename (name)
  "Rename the current line's desktop to NAME."
  (interactive "sNew name: ")
  (setcar (desktop-menu-line-desktop)
	  name)
  (desktop-menu-list desktop-menu-directory))

(defun desktop-menu-new-file (directory)
  "Create a name for a new file in DIRECTORY."
  (let ((n -1)
	(filenames (mapcar #'cadr desktop-menu--desktops))
	filename)
    (while (not filename)
      (setq n (1+ n))
      (let ((relative-filename (concat desktop-menu-base-filename
				       (if (eq 0 n) "" (int-to-string n)))))
	(setq filename
	      (and (not (file-exists-p (expand-file-name relative-filename
							 directory)))
		   (not (member relative-filename filenames))
		   relative-filename))))
    filename))

(defun desktop-menu-quit ()
  "Leave Desktop Menu and save current desktop list."
  (interactive)
  (bury-buffer (current-buffer))
  (desktop-menu-save-main-list desktop-menu--desktops
			       desktop-menu-directory)
  (set-window-configuration desktop-menu--orig-layout))

(defun desktop-menu-abort ()
  "Ding and leave Desktop Menu without saving current desktop list."
  (interactive)
  (ding)
  (bury-buffer (current-buffer))
  (set-window-configuration desktop-menu--orig-layout))

(defun desktop-menu-clear ()
  "Clear the current desktop with `desktop-clear'."
  (interactive)
  (let ((desktop-clear-preserve-buffers (cons "*Desktop Menu*"
					      desktop-clear-preserve-buffers)))
    (desktop-clear)
    (set-window-configuration desktop-menu--orig-layout)))

(defun desktop-menu-load (&optional clearp)
  "Load the current line's desktop.
Optional argument CLEARP `ask', t, `yes' or `no'.
See the function and variable `desktop-menu-clear' for more explanation."
  (interactive)
  (let* ((desktop-clear-preserve-buffers (cons "*Desktop Menu*"
                                               desktop-clear-preserve-buffers))
         (desktop (desktop-menu-line-desktop))
         (name (car desktop))
         (fname (cadr desktop)))
    (setq clearp (or clearp desktop-menu-clear))
    (cond ((or (eq clearp t) (eq clearp 'yes))
	   (desktop-clear))
	  ((eq clearp 'ask)
	   (if (y-or-n-p "Clear desktop? ")
	       (desktop-clear))))
    (set-window-configuration desktop-menu--orig-layout)
    (desktop-menu-read (expand-file-name fname
					 desktop-menu-directory))
    (setq desktop-menu--current-desktop-name name)))

(defun desktop-menu-delete ()
  "Delete the current line's desktop.
Honours the `desktop-menu-ask-user-on-delete' variable setting."
  (interactive)
  (let* ((desktop (desktop-menu-line-desktop))
         (name (car desktop))
         (fname (cadr desktop)))
    (when (or (not desktop-menu-ask-user-on-delete)
	      (y-or-n-p (format "Delete Desktop %S? " name)))
      (when (file-exists-p fname)
        (delete-file fname))
      (setq desktop-menu--desktops
	    (remove desktop
		    desktop-menu--desktops))
      (desktop-menu-list desktop-menu-directory))))

(defun desktop-menu-merge ()
  "Load the current line's desktop; do not clear the current desktop."
  (interactive)
  (desktop-menu-load 'no))

(defun desktop-menu-save ()
  "Save the current desktop into the current line's desktop file."
  (interactive)
  (let* ((desktop (desktop-menu-line-desktop))
         (name (car desktop))
         (fname (cadr desktop)))
    (desktop-menu-save-into (expand-file-name fname
					      desktop-menu-directory))
    (setq desktop-menu--current-desktop-name name)
    (desktop-menu-list desktop-menu-directory)
    (message "Saved into desktop %s." name)))

(defun desktop-menu-line-desktop ()
  "Return the current line's desktop."
  (let ((line (1- (count-lines (point-min) (if (eobp) (point) (1+ (point)))))))
    (if (and (>= line 1)
	     (<= line (length desktop-menu--desktops)))
	(nth (1- line) desktop-menu--desktops)
      (error "You are not on a desktop line"))))

(defun desktop-menu--set-window-height ()
  "Change height of the selected window to suit the desktop list."
  (unless (one-window-p t)
    (shrink-window (- (window-height (selected-window))
		      ;; window-height in xemacs includes mode-line
		      (+ (if (featurep 'xemacs) 4 2)
			 (max 4 (length desktop-menu--desktops)))))))

(defun desktop-menu-help ()
  "Help for `desktop-menu-mode'."
  (interactive)
  (describe-function 'desktop-menu-mode))

(defun desktop-menu-extra-desktop-description (filename)
  "Return a desktop description string.
The string contains some information about the desktop saved in FILENAME."
  (let ((file (expand-file-name filename desktop-menu-directory)))
    (if (file-exists-p file)
	(with-temp-buffer
          (insert-file file)
	  (goto-char (point-min))
	  (let ((time (if (search-forward-regexp "^;; Created \\(.*\\)" nil t)
			  (match-string 1)
			""))
		(count 0))
	    (while (search-forward-regexp "^(desktop-create-buffer" nil t)
	      (setq count (1+ count)))
	    (format "%2d %s %s" count (if (= count 1) "Buffer" "Buffers") time)))
      "empty")))

;; ----------------------------------------------------------------------------
;; Desktop Menu main functions
;; ----------------------------------------------------------------------------

;;;###autoload
(defun desktop-menu-in (directory)
  "Make a menu of available desktops in directory DIRECTORY."
  (interactive "D")
  (setq desktop-menu--orig-window (selected-window)
	desktop-menu--orig-layout (current-window-configuration))
  (let ((active-desktop-window nil))
    (walk-windows (function (lambda (window)
			      (if (string= (buffer-name (window-buffer window))
					   "*Desktop Menu*")
				  (setq active-desktop-window window)))))
    (if active-desktop-window
	(select-window active-desktop-window)
      (when (> (window-height (selected-window)) 7) ; can split
        (split-window (selected-window)))
      (other-window 1)))
  (desktop-menu-list directory t))

;;;###autoload
(defun desktop-menu ()
  "Make a menu of available Emacs desktops in `desktop-menu-directory'."
  (interactive)
  (desktop-menu-in desktop-menu-directory))

(provide 'desktop-menu)

;;; desktop-menu.el ends here

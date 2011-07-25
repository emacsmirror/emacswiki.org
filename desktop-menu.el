;;; desktop-menu.el --- menu for managing Emacs desktops

;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Olaf Sylvester

;; Author: Olaf Sylvester <ole_i_dont_like_spam at geekware . de>
;;         Štěpán Němec <stepnem@gmail.com>
;; Maintainer: Štěpán Němec <stepnem@gmail.com>
;; Keywords: convenience, desktop
;; Time-stamp: "2011-07-24 15:32:08 CEST stepnem"
;; Version: 1.1
;; URL: http://www.emacswiki.org/emacs/download/desktop-menu.el
;; Dev-URL: http://github.com/stepnem/emacs-libraries/raw/master/desktop-menu.el
;; Compatibility: GNU Emacs (see Commentary for details)

;; This file is NOT part of GNU Emacs.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
 
;;; Commentary:

;; This package provides a *Desktop Menu* for managing multiple Emacs
;; desktops, possibly in a single directory.
;;
;; Start the menu with `M-x desktop-menu RET'.
;;
;; Press `?' for help in the Desktop Menu.

;; Olaf speaketh:
;;
;;   Why a Desktop Menu?
;;   The Emacs package `desktop' is a really nice menu for saving and
;;   reading various Emacs desktops in different directories.
;;   But often I have the problems:
;;   - I want to save various Emacs desktops in the same directory and
;;   - I lost the overview for directories with a Emacs desktop.
;;   So I've developed a menu which handles various Emacs desktops
;;   in one directory and I can give each desktop a nice name.

;; Compatibility note:
;;
;; Unfortunately, while testing the autosave feature, I found out that XEmacs'
;; `desktop-read' doesn't take an argument, which makes this package pretty
;; much useless. Thus, no effort is currently made to make it compatible with
;; XEmacs.

;;; Customization:

;; There is a customization group called `desktop-menu' in group `desktop'.
;; Start customization by `M-x customize-group RET desktop-menu RET'.
;; You might also want to customize some standard `desktop' variables,
;; such as `desktop-load-locked-desktop'.
 
;;; History:

;; 0.2
;; - Olaf's last version <http://www.geekware.de/software/emacs/desktop-menu.el>
;;
;; 1.0
;; - fix a number of language issues, function/variable naming and code cleanup,
;;   sync with current GNU Emacs API, fix some docstrings
;; - use `with-temp-buffer' to not leave behind a *Desktop Menu ttt* buffer
;; - fix keyword highlighting regexp
;; - display "Buffer" or "Buffers" according to the actual number of buffers in
;;   a desktop
;;
;; 1.1
;; - `desktop-menu-autosave'
;; - `desktop-menu-list-desktop-buffers' (bound to `l' by default)
;; - `desktop-menu-visit' (bound to `v' by default)
;; - make the key bindings more in line with similar Emacs interfaces;
;;   unfortunately that means `desktop-menu-new' no longer has a nice mnemonic;
;;   it's bound to `o' by default (think Gnus ;-))
;; - mention other customization variables
;; - add History section
;; - other minor fixes

;;; Todo:

;; - improve and extend `desktop-menu-list-desktop-buffers' ("cherry-picking"
;;   a buffer into current desktop, desktop contents editing functionality?)

;;; Code:
 
(require 'desktop)
(eval-when-compile (require 'cl))       ; caadr, cadadr, case
 
(defgroup desktop-menu nil
  "Managing multiple desktops."
  :group 'desktop)

(defvar desktop-menu--kill-emacs-hooked-p nil
  "Non-nil means we hooked into `kill-emacs-hook' to autosave current desktop.")

(defvar desktop-menu--autosave-timer nil
  "Timer running the current-desktop autosave function.")

(defcustom desktop-menu-autosave nil
  "Save the current desktop every this many seconds and on Emacs exit.
When set to a non-numeric true value, only save the desktop on Emacs exit.
If nil, don't autosave the current desktop."
  :type '(choice (const :tag "Don't autosave" nil)
                 (number)
                 (other :tag "Only save on Emacs exit" t))
  :set (lambda (sym val)
         (and (boundp 'desktop-menu--autosave-timer)
              desktop-menu--autosave-timer
              (cancel-timer desktop-menu--autosave-timer))
         (setq desktop-menu--autosave-timer
               (and val
                    (numberp val)
                    (run-at-time val val 'desktop-menu--do-autosave)))
         (if val (progn (add-hook 'kill-emacs-hook 'desktop-menu--do-autosave)
                        (setq desktop-menu--kill-emacs-hooked-p t))
           (when desktop-menu--kill-emacs-hooked-p
             (remove-hook 'kill-emacs-hook 'desktop-menu--do-autosave)
             (setq desktop-menu--kill-emacs-hooked-p nil)))
         (set-default sym val))
  :group 'desktop-menu)

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
If nil, never clear the current desktop when loading another one.
If `ask', ask the user.
Any other non-nil value means clear the desktop unconditionally.
Desktop will be cleared by `desktop-clear'."
  :type '(choice (const :tag "Ask user" ask)
                 (const :tag "Don't clear desktop" nil)
                 (other :tag "Always clear desktop" t))
  :group 'desktop-menu)

(defcustom desktop-menu-ask-user-on-delete t
  "If non-nil, ask user before deleting a desktop."
  :group 'desktop-menu
  :type 'boolean)

(defcustom desktop-menu-sort-p t
  "If non-nil, sort desktops by name."
  :group 'desktop-menu
  :type 'boolean)

(defcustom desktop-menu-mode-font-lock-keywords
  '(("\\(^ Desktops in directory\\) \\(.+\\)"
     (1 font-lock-type-face append)
     (1 'bold append)
     (2 font-lock-function-name-face append))
    ("^.  \\(.+?\\)\\([0-9]+ Buffer\\(s\\)?\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-constant-face append)))
  "Fontlock settings for Desktop Menu."
  :type 'sexp
  :group 'desktop-menu)

(defcustom desktop-menu-blist-mode-font-lock-keywords
  '(("^\\(.+\\)|\\(.+\\)$"
     (1 font-lock-type-face append)
     (1 'bold append)
     (2 font-lock-function-name-face append)))
  "Fontlock settings for Desktop Buffer List."
  :type 'sexp
  :group 'desktop-menu)
 
(defvar desktop-menu--desktops nil
  "List of all known desktops.")

(defvar desktop-menu--current-desktop nil
  "Pair (cons cell) holding the name and filename of the current desktop.")

(defvar desktop-menu--blist-orig-layout nil
  "Window configuration before opening Desktop Buffer List.")

(defvar desktop-menu--orig-layout nil
  "Window configuration before starting Desktop Menu.")

(defvar desktop-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 10)
      (define-key map (vector (+ ?0 i)) 'digit-argument))
    (define-key map "\C-g"    'desktop-menu-abort)
    (define-key map "\C-m"    'desktop-menu-load)
    (define-key map "-"       'negative-argument)
    (define-key map "?"       'desktop-menu-help)
    (define-key map "^"       'desktop-menu-up-directory)
    (define-key map "c"       'desktop-menu-change-directory)
    (define-key map "d"       'desktop-menu-delete)
    (define-key map "g"       'desktop-menu-refresh)
    (define-key map "l"       'desktop-menu-list-desktop-buffers)
    (define-key map "m"       'desktop-menu-merge)
    (define-key map "n"       'desktop-menu-down)
    (define-key map "o"       'desktop-menu-new)
    (define-key map "p"       'desktop-menu-up)
    (define-key map "q"       'desktop-menu-quit)
    (define-key map "r"       'desktop-menu-rename)
    (define-key map "s"       'desktop-menu-save)
    (define-key map "v"       'desktop-menu-visit)
    (define-key map "x"       'desktop-menu-clear)
    (define-key map [down]    'desktop-menu-down)
    (define-key map [up]      'desktop-menu-up)
    map)
  "Keymap for `desktop-menu-mode'.")
 
(defun desktop-menu--do-autosave ()
  (when desktop-menu--current-desktop
    (desktop-menu-save-into (cdr desktop-menu--current-desktop))))

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
\\[desktop-menu-help] - Display this help text.
\\[desktop-menu-up-directory] - Go up one directory.
\\[desktop-menu-change-directory] - Change to another directory.
\\[desktop-menu-delete] - Delete the current line's desktop.
\\[desktop-menu-refresh] - Refresh the listing.
\\[desktop-menu-list-desktop-buffers] - List the buffers saved in the current line's desktop.
\\[desktop-menu-merge] - Merge the current line's desktop into the current desktop.
\\[desktop-menu-new] - Create a new desktop.
\\[desktop-menu-rename] - Rename the current line's desktop.
\\[desktop-menu-visit] - View file storing the current line's desktop.
\\[desktop-menu-clear] - Clear the current Emacs desktop."
  (kill-all-local-variables)
  (use-local-map desktop-menu-mode-map)
  (setq buffer-read-only t
        font-lock-defaults '(desktop-menu-mode-font-lock-keywords t)
        font-lock-verbose nil
        major-mode 'desktop-menu-mode
        mode-name "Desktop Menu"
        truncate-lines t)
  (run-mode-hooks 'desktop-menu-mode-hook))

(defun desktop-menu-save-main-list (desktops directory)
  "Save DESKTOPS in DIRECTORY into `desktop-menu-list-file'."
  (unless (file-exists-p desktop-menu-directory)
    (make-directory desktop-menu-directory))
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
    ;; remove non-existent desktop files
    (setq desktops desktop-menu--desktops)
    (let ((list (copy-sequence desktop-menu--desktops)))
      (while list
        (if (not (file-exists-p (expand-file-name (cadar list) directory)))
            (setq desktops
                  (remove (car list) desktops)))
        (setq list (cdr list))))
    ;; find more desktop files
    (let* ((base desktop-menu-base-filename)
           (existing-in-main (mapcar 'cadr desktops))
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
    (insert " Desktops in directory "
            (expand-file-name "." desktop-menu-directory)
            "\n")
    (while desktops
      (let* ((desktop (car desktops))
             (name (car desktop))
             (fname (cadr desktop)))
        (insert (if (string= name (car desktop-menu--current-desktop))
                    "."
                  " ")
                (format "  %-30s %20s  %s"
                        name
                        (desktop-menu-extra-desktop-description fname)
                        fname))
        (newline))
      (setq desktops (cdr desktops)))
    (backward-delete-char 1)
    (beginning-of-line)
    (set-buffer-modified-p nil)
    (font-lock-fontify-buffer)
    (desktop-menu--set-window-height)
    (goto-char (point-min))
    (when (and (not (search-forward-regexp "^\\." nil t))
               (not (eq (line-end-position) (point-max))))
      (forward-line 1))
    (beginning-of-line)))

(defun desktop-menu-new-file (directory)
  "Create a name for a new file in DIRECTORY."
  (let ((n -1)
        (filenames (mapcar 'cadr desktop-menu--desktops))
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
                      (+ 2 (max 4 (length desktop-menu--desktops)))))))

(defun desktop-menu-extra-desktop-description (filename)
  "Return a desktop description string.
The string contains some information about the desktop saved in FILENAME."
  (let ((file (expand-file-name filename desktop-menu-directory)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((time (if (search-forward-regexp "^;; Created \\(.*\\)" nil t)
                          (match-string 1)
                        ""))
                (count 0))
            (while (search-forward-regexp "^(desktop-create-buffer" nil t)
              (setq count (1+ count)))
            (format "%2d %s %s"
                    count (if (= count 1) "Buffer" "Buffers") time)))
      "empty")))

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
  (message "Now you can save the newly created desktop"))

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
  (setcar (desktop-menu-line-desktop) name)
  (desktop-menu-list desktop-menu-directory))

(defun desktop-menu-quit ()
  "Leave Desktop Menu and save current desktop list."
  (interactive)
  (bury-buffer)
  (desktop-menu-save-main-list desktop-menu--desktops
                               desktop-menu-directory)
  (set-window-configuration desktop-menu--orig-layout))

(defun desktop-menu-abort ()
  "Ding and leave Desktop Menu without saving current desktop list."
  (interactive)
  (ding)
  (bury-buffer)
  (set-window-configuration desktop-menu--orig-layout))

(defun desktop-menu-clear ()
  "Clear the current desktop with `desktop-clear'."
  (interactive)
  (let ((desktop-clear-preserve-buffers
         (cons "*Desktop Menu*" desktop-clear-preserve-buffers)))
    (desktop-clear)
    (set-window-configuration desktop-menu--orig-layout)))

(defun desktop-menu-visit ()
  "Exit Desktop Menu and visit file storing the desktop under cursor."
  (interactive)
  (let ((desktop (desktop-menu-line-desktop)))
    (desktop-menu-quit)
    (find-file-read-only (expand-file-name (cadr desktop)
                                           desktop-menu-directory))))

(defun desktop-menu-load (&optional clearp)
  "Load the current line's desktop.
See the variable `desktop-menu-clear' for the meaning and
possible values of the CLEARP parameter."
  (interactive)
  (let* ((desktop-clear-preserve-buffers
          (cons "*Desktop Menu*" desktop-clear-preserve-buffers))
         (desktop (desktop-menu-line-desktop))
         (name (car desktop))
         (fname (expand-file-name (cadr desktop) desktop-menu-directory)))
    (setq clearp (or clearp desktop-menu-clear))
    (when (or (and (eq clearp 'ask)
                   (y-or-n-p "Clear desktop? "))
              clearp)
      (desktop-clear))
    (set-window-configuration desktop-menu--orig-layout)
    (desktop-menu-read fname)
    (setq desktop-menu--current-desktop (cons name fname))))

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
  (desktop-menu-load))

(defun desktop-menu-save ()
  "Save the current desktop into the current line's desktop file."
  (interactive)
  (let* ((desktop (desktop-menu-line-desktop))
         (name (car desktop))
         (fname (expand-file-name (cadr desktop) desktop-menu-directory)))
    (desktop-menu-save-into fname)
    (setq desktop-menu--current-desktop (cons name fname))
    (desktop-menu-list desktop-menu-directory)
    (message "Saved into desktop %s" name)))

(defun desktop-menu-help ()
  "Help for `desktop-menu-mode'."
  (interactive)
  (describe-function 'desktop-menu-mode))
 
;;; Desktop Buffer List
(defvar desktop-menu-blist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "l" 'desktop-menu-blist-lift)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "q" 'desktop-menu-blist-quit)
    map)
  "Keymap for *Desktop Buffer List*.")

'(defun desktop-menu-blist-lift (&optional arg)
  "Recreate the buffer at point in the current session.
With a prefix argument, also remove it from its original desktop."
  (interactive "P")
  (let ((bname (progn (beginning-of-line)
                      (looking-at ))))
    (eval (catch 'found
            (with-temp-buffer
              (insert-file-contents desktop-menu-blist-desktop-file)
              (while (search-forward "(desktop-create-buffer" nil t)
                (beginning-of-line)
                (let* ((buf (read (current-buffer))))
                  (and (string= (nth 3 buf) bname)
                       (throw 'found buf)))))))))

(defun desktop-menu-blist-quit ()
  (interactive)
  (kill-buffer nil)
  (set-window-configuration desktop-menu--blist-orig-layout))

(defvar desktop-menu-blist-desktop-file nil
  "Name of the file containing definition of the currently listed desktop.")
(put 'desktop-menu-blist-desktop-file 'permanent-local t)

(define-derived-mode desktop-menu-blist-mode special-mode "Desktop Buffer List"
  (setq font-lock-defaults '(desktop-menu-blist-mode-font-lock-keywords t)))

;; dynamically bound in `desktop-read'
(defvar desktop-first-buffer)

;; bound locally in `desktop-read'
(defvar desktop-buffer-ok-count)
(defvar desktop-buffer-fail-count)

(defun desktop-menu-list-desktop-buffers ()
  "List buffers saved in the current line's desktop."
  (interactive)
  (let* (blist
         (desktop (desktop-menu-line-desktop))
         (desktop-file (cadr desktop)))
    (with-temp-buffer
      (insert-file-contents desktop-file)
      (while (search-forward "(desktop-create-buffer" nil t)
        (beginning-of-line)
        (let* ((buf (read (current-buffer)))
               (entry
                (if (nth 2 buf)
                    (cons (nth 3 buf) (abbreviate-file-name (nth 2 buf)))
                  (case (cadr (nth 4 buf))
                    (dired-mode
                     (cons (nth 3 buf)
                           (abbreviate-file-name (caadr (nth 9 buf)))))
                    (Info-mode
                     (cons
                      (nth 3 buf)
                      (concat "("
                              (file-name-nondirectory
                               (or (caadr (nth 9 buf)) "No file"))
                              ") "
                              (cadadr (nth 9 buf)))))
                    (w3m-mode (cons (nth 3 buf) (nth 9 buf)))))))
          (and entry (push entry blist)))))
    (setq desktop-menu--blist-orig-layout (current-window-configuration))
    (switch-to-buffer-other-window (get-buffer-create "*Desktop Buffer List*"))
    (let ((inhibit-read-only t))
      (mapc (lambda (c)
              ;; let's assume buffer names containing " |" don't exist :-))
              (insert (car c) " |" (cdr c) "\n"))
            (sort blist (lambda (x y) (string< (car x) (car y)))))
      (align-regexp (point-min) (point-max) "\\( \\)|" 1 0))
  (desktop-menu-blist-mode)))
 
(defun desktop-menu-save-into (filename)
  "Save the current desktop into FILENAME."
  (interactive "F")
  (let ((desktop-base-file-name (file-name-nondirectory filename)))
    (desktop-save (file-name-directory filename))))

;;;###autoload
(defun desktop-menu-read (filename)
  "Load desktop of FILENAME."
  (interactive "F")
  (let ((desktop-dirname (file-name-directory filename))
        (desktop-base-file-name (file-name-nondirectory filename)))
    (message (format "File: %S Desktop dir: %S"
                     desktop-base-file-name
                     desktop-dirname))
    (desktop-read desktop-dirname)))

;;;###autoload
(defun desktop-menu-in (directory)
  "Make a menu of available desktops in directory DIRECTORY."
  (interactive "D")
  (setq desktop-menu--orig-layout (current-window-configuration))
  (let ((active-desktop-window nil))
    (walk-windows (function (lambda (window)
                              (if (string= (buffer-name (window-buffer window))
                                           "*Desktop Menu*")
                                  (setq active-desktop-window window)))))
    (if active-desktop-window
        (select-window active-desktop-window)
      (when (> (window-height (selected-window)) 7)
        (split-window (selected-window)))
      (other-window 1)))
  (desktop-menu-list directory t))

;;;###autoload
(defun desktop-menu ()
  "Make a menu of available Emacs desktops in `desktop-menu-directory'."
  (interactive)
  (desktop-menu-in desktop-menu-directory))
 
(provide 'desktop-menu)

;; Local Variables:
;; time-stamp-line-limit: 10
;; End:

;;; desktop-menu.el ends here

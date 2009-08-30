;;; find-dired+.el --- Extensions to `find-dired.el'.
;;
;; Filename: find-dired+.el
;; Description: Extensions to `find-dired.el'.
;; Author: Roland McGrath <roland@gnu.ai.mit.edu>,
;;      Sebastian Kremer <sk@thp.uni-koeln.de>,
;;      Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2009, Drew Adams, all rights reserved.
;; Copyright (C) 1992, 1994 Free Software Foundation, Inc.
;; Created: Wed Jan 10 14:31:50 1996
;; Version: 20.0
;; Last-Updated: Sat Dec 27 10:09:31 2008 (-0800)
;;           By: dradams
;;     Update #: 555
;; URL: http://www.emacswiki.org/cgi-bin/wiki/find-dired+.el
;; Keywords: internal, unix, tools, matching, local
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `cl', `custom', `dired', `dired+', `dired-aux', `dired-x',
;;   `easymenu', `ediff-diff', `ediff-help', `ediff-init',
;;   `ediff-merg', `ediff-mult', `ediff-util', `ediff-wind',
;;   `find-dired', `find-dired-', `fit-frame', `info', `info+',
;;   `misc-fns', `mkhtml', `mkhtml-htmlize', `strings', `thingatpt',
;;   `thingatpt+', `w32-browser'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `find-dired.el'.
;;
;;  See also the companion file `find-dired-.el'.
;;        `find-dired-.el' should be loaded before `find-dired.el'.
;;        `find-dired+.el' should be loaded after `find-dired.el'.
;;
;;  A `find' submenu has been added to Dired's menu bar, and most of
;;  the Emacs `find-*' commands have undergone slight improvements.
;;
;;
;;  New user options (variables) defined here:
;;
;;    `find-dired-default-fn', `find-dired-hook'.
;;
;;  Other new variable defined here: `menu-bar-run-find-menu'.
;;
;;
;;  ***** NOTE: The following functions defined in `find-dired.el'
;;              have been REDEFINED HERE:
;;
;;  `find-dired' - 1. Interactive spec uses `read-from-minibuffer',
;;                    `read-file-name', `dired-regexp-history' and
;;                    `find-dired-default-fn'.
;;                 2. Runs `find-dired-hook' at end.
;;                 3. Uses `find-dired-default-fn' for default input.
;;                 4. Buffer named after dir (not named "*Find*").
;;  `find-dired-filter' - Removes lines that just list a file.
;;  `find-dired-sentinel' - 1. Highlights file lines.
;;                          2. Puts `find' in mode-line.
;;  `find-grep-dired' - Interactive spec uses `read-from-minibuffer',
;;                      `read-file-name', `dired-regexp-history' and
;;                      `find-dired-default-fn'.
;;  `find-name-dired' - Interactive spec uses `read-from-minibuffer',
;;                      `read-file-name', `dired-regexp-history' and
;;                      `find-dired-default-fn'.
;;
;;
;;  ***** NOTE: The following variable defined in `find-dired.el'
;;              has been REDEFINED HERE:
;;
;;  `find-ls-options'   - Uses `dired-listing-switches' for Windows.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/03/20 dadams
;;     menu-bar-dired-subdir-menu -> diredp-menu-bar-subdir-menu.
;; 2000/09/27 dadams
;;     Updated for Emacs 20.7.
;; 2000/09/18 dadams
;;     1. find-dired: a) Use dired-simple-subdir-alist & find-ls-option anew
;;                       (instead of dired's default switches).
;;                    b) Updated to Emacs20 version: define-key added.
;;     2. Added: find-ls-options - redefined to treat Windows too.
;; 1999/04/06 dadams
;;     1. Protected symbol-name-nearest-point with fboundp.
;;     2. find-dired, find-name-dired, find-grep-dired: No default regexp
;;        if grep-default-regexp-fn is nil.
;; 1999/03/31 dadams
;;     Updated for Emacs version 19.34.
;;     1. Updated using version 19.34 find-dired.el. (find-dired, find-name-dired,
;;        find-grep-dired, find-dired-filter, find-dired-sentinel)
;;     2. Added: find-grep-options (added irix).
;;     3. Renamed: find-dired-history -> find-args-history.
;;     4. find-dired: pop-to-buffer -> switch-to-buffer.
;; 1996/04/18 dadams
;;     Menu is added to Dir (Subdir) menu, not This (Immediate) menu.
;; 1996/03/20 dadams
;;     1. Added find-dired-default-fn.
;;     2. find-dired, find-name-dired, find-grep-dired:
;;           symbol-name-nearest-point -> find-dired-default-fn.
;; 1996/01/25 dadams
;;     find-dired-sentinel:
;;       1. Use (my) dired-insert-set-properties.
;;       2. Highlight whole file lines.
;; 1996/01/11 dadams
;;     find-dired-filter: Corrected removal of extra lines just listing a file,
;;       and deletion of "./" prefix.
;; 1996/01/11 dadams
;;     1. Added redefinition of dired-revert.
;;     2. Buffer used has same root name as the dir (no longer "*Find*").
;;     3. Added " `find'" to mode-line-process.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, unless
(require 'find-dired-) ;; for new defvars from `find-dired.el'
(require 'find-dired)

(require 'dired+ nil t) ;; (no error if not found):
                        ;; dired-insert-set-properties,
                        ;; diredp-menu-bar-subdir-menu
 ;; Note: `dired+.el' does a (require 'dired): dired-mode-map
(require 'thingatpt+ nil t) ;; (no error if not found):
                            ;; symbol-name-nearest-point

;;; You will get this message:
;;;
;;; Compiling file find-dired+.el
;;;   ** the function dired-simple-subdir-alist is not known to be defined.

;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defvar find-dired-hook nil
  "*Hook to be run at the end of each `find-dired' execution.")

;;;###autoload
(defvar find-dired-default-fn (and (fboundp 'symbol-name-nearest-point)
                                    'symbol-name-nearest-point)
  "*Function of 0 args called to provide default input for \\[find-dired],
\\[find-name-dired], and  \\[find-grep-dired].

Some reasonable choices:
`word-nearest-point', `symbol-name-nearest-point', `sexp-nearest-point'.

If this is nil, then no default input is provided.")


;; REPLACES ORIGINAL in `find-dired.el':
;; Uses `dired-listing-switches' for Windows.
;; Note: `defconst' is necessary here because this is preloaded by basic emacs:
;; it is not sufficient to do a defvar before loading `find-dired.el'.  Too bad.
;; Otherwise, this could be just a `defvar' in `find-dired-.el'.
;;;###autoload
(defconst find-ls-option
  (cond ((eq system-type 'berkeley-unix)
         '("-ls" . "-gilsb"))
        ((eq system-type 'windows-nt)
         (cons "-ls" dired-listing-switches))
        (t
         '("-exec ls -ld {} \\;" . "-ld")))
  "*Description of the option to `find' to produce an `ls -l'-type listing.
This is a cons of two strings (FIND-OPTION . LS-SWITCHES).  FIND-OPTION
gives the option (or options) to `find' that produce the desired output.
LS-SWITCHES is a list of `ls' switches to tell dired how to parse the output.")


;; REPLACES ORIGINAL in `find-dired.el':
;; 1. Interactive spec uses `read-from-minibuffer', `read-file-name',
;;    `dired-regexp-history' and `find-dired-default-fn'.
;; 2. Runs `find-dired-hook' at end.
;; 3. Uses `find-dired-default-fn' to get default input text.
;; 4. Buffer used has same root name as the dir (not "*Find*").
;;;###autoload
(defun find-dired (dir args)
  "Run `find' and put its output in a buffer in Dired Mode.
Then run `find-dired-hook' and `dired-after-readin-hook'.
The `find' command run (after changing into DIR) is:

    find . \\( ARGS \\) -ls"
  (interactive
   (let ((default (and find-dired-default-fn
                       (funcall find-dired-default-fn))))
     (list (read-file-name "Run `find' in directory: " nil "" t)
           (read-from-minibuffer "Run `find' (with args): " default
                                 nil nil 'find-args-history default t))))
  (let ((dired-buffers dired-buffers))
    ;; Expand DIR ("" means default-directory), and make sure it has a
    ;; trailing slash.
    (setq dir (abbreviate-file-name
               (file-name-as-directory (expand-file-name dir))))
    (unless (file-directory-p dir)      ; Ensure that it's really a directory.
      (error "Command `find-dired' needs a directory: `%s'" dir))
    (switch-to-buffer (create-file-buffer (directory-file-name dir)))
    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory dir)
    (setq args (concat
                "find . " (if (string= "" args) "" (concat "\\( " args " \\) "))
                (car find-ls-option)))
    ;; The next statement will bomb in classic dired (no optional arg allowed)
    (dired-mode dir (cdr find-ls-option))
    ;; This really should rerun the find command, but I don't
    ;; have time for that.
    (use-local-map (append (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "g" 'undefined)
    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
        ;; will work even with nested dired format (dired-nstd.el,v 1.15
        ;; and later)
        (dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm)
      (set (make-local-variable 'dired-subdir-alist)
           (list (cons default-directory (point-min-marker)))))
    (setq buffer-read-only nil)
    ;; Subdir headerline must come first because the first marker in
    ;; `subdir-alist' points there.
    (insert "  " dir ":\n")
    ;; Make second line a "find" line in analogy to the "total" or
    ;; "wildcard" line.
    (insert "  " args "\n")
    ;; Start the `find' process.
    (let ((proc (start-process-shell-command "find" (current-buffer) args)))
      (set-process-filter proc (function find-dired-filter))
      (set-process-sentinel proc (function find-dired-sentinel))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) 1 (current-buffer))
      (setq mode-line-process '(": %s `find'"))
      (run-hooks 'find-dired-hook 'dired-after-readin-hook))))


;; REPLACES ORIGINAL in `find-dired.el':
;; Interactive spec uses `read-from-minibuffer', `read-file-name',
;; `dired-regexp-history' and `find-dired-default-fn'.
;;;###autoload
(defun find-name-dired (dir pattern)
  "Search directory DIR recursively for files matching globbing PATTERN,
and run `dired' on those files.  PATTERN may use shell wildcards, and
it need not be quoted.  It is not an Emacs regexp.
The command run (after changing into DIR) is: find . -name 'PATTERN' -ls"
  (interactive
   (let ((default (and find-dired-default-fn
                       (funcall find-dired-default-fn))))
   (list (read-file-name "Find-name (directory): " nil "" t)
         (read-from-minibuffer "Find-name (filename wildcard): " default
                               nil nil 'dired-regexp-history default t))))
  (find-dired dir (concat "-name '" pattern "'")))


;; REPLACES ORIGINAL in `find-dired.el':
;; Interactive spec uses `read-from-minibuffer', `read-file-name',
;; `dired-regexp-history' and `find-dired-default-fn'.
;;;###autoload
(defun find-grep-dired (dir regexp)
  "Find files in DIR containing a regexp REGEXP.
The output is in a Dired buffer.
The `find' command run (after changing into DIR) is:

    find . -exec grep -s REGEXP {} \\\; -ls

Thus REGEXP can also contain additional grep options."
  (interactive
   (let ((default (and find-dired-default-fn
                       (funcall find-dired-default-fn))))
     (list (read-file-name "Find-grep (directory): " nil "" t)
           (read-from-minibuffer "Find-grep (grep regexp): "
                                 nil nil 'dired-regexp-history default t))))
  ;; find -exec doesn't allow shell i/o redirections in the command,
  ;; or we could use `grep -l >/dev/null'
  ;; We use -type f, not ! -type d, to avoid getting screwed
  ;; by FIFOs and devices.  I'm not sure what's best to do
  ;; about symlinks, so as far as I know this is not wrong.
  (find-dired dir
              (concat "-type f -exec grep " find-grep-options " "
                      regexp " {} \\\; ")))


;; REPLACES ORIGINAL in `find-dired.el':
;; Removes lines that just list a file.
;;;###autoload
(defun find-dired-filter (proc string)
  "Filter for \\[find-dired] processes.
PROC is the process.
STRING is the string to insert."
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)               ; not killed?
        (save-excursion
          (set-buffer buf)
          (save-restriction
            (widen)
            (save-excursion
              (let ((buffer-read-only nil)
                    (end (point-max)))
                (goto-char end)
                (insert string)
                (goto-char end)
                (unless (looking-at "^") (forward-line 1))
                (while (looking-at "^")
                  (insert "  ")
                  (forward-line 1))
                (goto-char (- end 3))   ; no error if < 0
                (save-excursion         ; Remove lines just listing the file.
                  (let ((kill-whole-line t))
                    (while (re-search-forward "^  ./" nil t)
                      (beginning-of-line) (kill-line))))
                ;; Convert ` ./FILE' to ` FILE'
                ;; This would lose if the current chunk of output
                ;; starts or ends within the ` ./', so back up a bit:
                (while (search-forward " ./" nil t)
                  (delete-region (point) (- (point) 2)))
                ;; Find all the complete lines in the unprocessed
                ;; output and process it to add text properties.
                (goto-char end)
                (if (search-backward "\n" (process-mark proc) t)
                    (progn
                      (dired-insert-set-properties (process-mark proc)
                                                   (1+ (point)))
                      (move-marker (process-mark proc) (1+ (point)))))
                ))))
      (delete-process proc))))          ; The buffer was killed.


;; REPLACES ORIGINAL in `find-dired.el':
;; 1. Highlights file lines.
;; 2. Puts `find' in mode-line.
;;;###autoload
(defun find-dired-sentinel (proc state)
  "Sentinel for \\[find-dired] processes.
PROC is the process.
STATE is the state of process PROC."
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)
        (save-excursion
          (set-buffer buf)
          (let ((buffer-read-only nil))
            (save-excursion
              (goto-char (point-max))
              (insert "\nfind " state)  ; STATE is, e.g., "finished".
              (forward-char -1)         ; Back up before \n at end of STATE.
              (insert " at " (substring (current-time-string) 0 19))
              (forward-char 1)
              (setq mode-line-process
                    (concat ": " (symbol-name (process-status proc))
                            " `find'"))
              ;; Since the buffer and mode line will show that the
              ;; process is dead, we can delete it now.  Otherwise it
              ;; will stay around until M-x list-processes.
              (delete-process proc)
              ;; Highlight lines of file names for mouse selection.
              (dired-insert-set-properties (point-min) (point-max))
              (force-mode-line-update)))
          (message "find-dired `%s' done." (buffer-name))))))


;; Menu bar, `find' menu.
(defvar menu-bar-run-find-menu (make-sparse-keymap "Unix `find'"))
(defalias 'menu-bar-run-find-menu (symbol-value 'menu-bar-run-find-menu))
(define-key menu-bar-run-find-menu [find-dired]
  '("`find' <anything>..." . find-dired))
(define-key menu-bar-run-find-menu [find-name-dired]
  '("Find Files Named..." . find-name-dired))
(define-key menu-bar-run-find-menu [find-grep-dired]
  '("Find Files Containing..." . find-grep-dired))
;; Add it to Dired's "Search" menu.
(when (boundp 'menu-bar-search-menu)
  (define-key dired-mode-map [menu-bar search separator-find]
    '("--"))
  (define-key dired-mode-map [menu-bar search find]
    '("Run `find' Command" . menu-bar-run-find-menu)))
;; Add it to Dired's "Dir" menu (called "Subdir" in `dired.el').
(when (boundp 'diredp-menu-bar-subdir-menu) ; Defined in `dired+.el'.
  (define-key-after diredp-menu-bar-subdir-menu [find]
    '("Run `find' Command" . menu-bar-run-find-menu) 'up))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'find-dired+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find-dired+.el ends here

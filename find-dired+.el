;;; find-dired+.el --- Extensions to `find-dired.el'.
;;
;; Filename: find-dired+.el
;; Description: Extensions to `find-dired.el'.
;; Author: Roland McGrath <roland@gnu.ai.mit.edu>,
;;      Sebastian Kremer <sk@thp.uni-koeln.de>,
;;      Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2017, Drew Adams, all rights reserved.
;; Created: Wed Jan 10 14:31:50 1996
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Wed Feb 22 17:50:18 2017 (-0800)
;;           By: dradams
;;     Update #: 1214
;; URL: https://www.emacswiki.org/emacs/download/find-dired%2b.el
;; Doc URL: http://emacswiki.org/LocateFilesAnywhere
;; Keywords: internal, unix, tools, matching, local
;; Compatibility: GNU Emacs 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `autofit-frame', `avoid', `bookmark',
;;   `bookmark+', `bookmark+-1', `bookmark+-bmu', `bookmark+-key',
;;   `bookmark+-lit', `dired', `dired+', `dired-aux', `dired-x',
;;   `easymenu', `ffap', `find-dired', `fit-frame', `frame-fns',
;;   `help+20', `highlight', `image-dired', `image-file', `info',
;;   `info+20', `menu-bar', `menu-bar+', `misc-cmds', `misc-fns',
;;   `naked', `pp', `pp+', `second-sel', `strings', `subr-21',
;;   `thingatpt', `thingatpt+', `time-date', `unaccent',
;;   `w32-browser', `w32browser-dlgopen', `wid-edit', `wid-edit+',
;;   `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `find-dired.el'.
;;
;;  A `find' submenu is added to Dired's menu bar, and most of the
;;  Emacs `find-*' commands have undergone slight improvements.
;;
;;
;;  User options defined here:
;;
;;    `find-diredp-default-fn', `find-diredp-max-cmd-line',
;;    `find-diredp-repeat-reuses-buffer-flag',
;;    `find-diredp-time-prefix'.
;;
;;  Commands defined here:
;;
;;    `find-time-dired'.
;;
;;  Non-interactive functions defined here:
;;
;;    `find-diredp--parse-time'.
;;
;;  Internal variables defined here:
;;
;;    `find-dired-hook', `find-diredp-repeating-search',
;;    `menu-bar-run-find-menu'.
;;
;;
;;  ***** NOTE: The following functions defined in `find-dired.el'
;;              have been REDEFINED HERE:
;;
;;  `find-dired' - 1. Interactive spec uses `read-from-minibuffer',
;;                    `read-file-name', `dired-regexp-history' and
;;                    `find-diredp-default-fn'.
;;                 2. Runs `find-dired-hook' at end.
;;                 3. Uses `find-diredp-default-fn' for default input.
;;                 4. Buffer named after dir (not named "*Find*").
;;  `find-dired-filter' - Removes lines that just list a file.
;;  `find-dired-sentinel' - 1. Highlights file lines.
;;                          2. Puts `find' in mode-line.
;;  `find-grep-dired' - Interactive spec uses `read-from-minibuffer',
;;                      `read-file-name', `dired-regexp-history' and
;;                      `find-diredp-default-fn'.
;;  `find-name-dired' - Interactive spec uses `read-from-minibuffer',
;;                      `read-file-name', `dired-regexp-history' and
;;                      `find-diredp-default-fn'.
;;
;;
;;  ***** NOTE: The following variable defined in `find-dired.el'
;;              has been REDEFINED HERE:
;;
;;  `find-ls-option'   - Uses `dired-listing-switches' for Windows.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2017/01/27 dadams
;;     find-dired-hook: Added tip to doc string about removing hook function.
;;     find-dired-sentinel: Use with-current-buffer instead of save-excursion + set-buffer.
;; 2016/05/15 dadams
;;     Moved find after wdired-mode, in diredp-menu-bar-subdir-menu.
;; 2016/03/25 dadams
;;     find-dired-sentinel: Put 2 SPC chars at bol so msg not taken as a mark.  Thx to Tino Calancha.
;; 2015/08/27 dadams
;;     Removed require of find-dired-.el.
;; 2015/08/19 dadams
;;     find-grep-dired: Use shell-quote-argument.  Use grep-program.  Thx to Tino Calancha.
;; 2015/08/18 dadams
;;     find-dired: Move abbreviate-file-name to where we insert subdir heading.  Thx to Tino Calancha.
;; 2015/08/17 dadams
;;     Added: find-diredp-time-format.
;; 2015/08/10 dadams
;;     Added: find-diredp--parse-time.
;;     find-time-dired: Made MAX-TIME arg mandatory.  Use find-diredp--parse-time.
;;     Thx to Tino Calancha.
;;     Removed compile-time require of cl.el.
;; 2015/08/03 dadams
;;     Added: find-diredp-repeating-search, find-diredp-time-history, find-diredp-time-prefix, find-diredp-max-cmd-line,
;;            find-diredp-repeat-reuses-buffer-flag, find-time-dired.
;;     Renamed find-dired-default-fn to find-diredp-default-fn.
;;     Use custom-reevaluate-setting on find-ls-option.
;;     find(-name|-grep)-dired: Added optional args.  Pass them to find-dired.
;;     find-dired:
;;       Added missing " " after ARGS.
;;       If find-diredp-repeat-reuses-buffer-flag is non-nil then do not call switch-to-buffer.
;;       Handle optional args.
;;     Moved running of find-dired-hook from find-dired to find-dired-sentinel, to ensure all output was received
;;       and Dired settings (e.g. dired-omit-mode) are respected.
;;     find-ls-option: Use custom-reevaluate-setting to make defconst override custom value.
;;     Thx to Tino Calancha.
;; 2015/07/26 dadams
;;     find-dired: Update to more recent vanilla code.
;;      Added: find-args (for older Emacs versions).  Use read-string with INITIAL-INPUT.
;;      Offer to kill running old proc.  Save find-args.  Use shell-quote-argument.
;;      Use find-program, find-exec-terminator, if defined.  Use shell-command and
;;      dired-insert-set-properties instead of start-process-shell-command.
;; 2015/07/25 dadams
;;     find-dired-filter:
;;       Update wrt recent Emacs:
;;         Use with-current-buffer, not set-buffer. Removed extra save-excursion after widen.
;;         Added the code to pad number of links and file size.
;;       Use delete-region, not kill-line, to protect kill-ring.
;;       Escape the . in the regexp.
;; 2015/07/24 dadams
;;     find-ls-option: Updated wrt vanilla Emacs.
;;     find-dired: Set buffer read-only.
;;     find-dired-filter, find-dired-sentinel: Bind inhibit-read-only to t.
;;     Thx to Tino Calancha.
;; 2015/07/17 dadams
;;     find-name-dired: Use find-name-arg and read-directory-name, if available.
;;                      Use shell-quote-argument.
;; 2012/08/21 dadams
;;     Call tap-put-thing-at-point-props after load thingatpt+.el.
;; 2012/08/18 dadams
;;     Invoke tap-define-aliases-wo-prefix if thingatpt+.el is loaded.
;; 2011/08/30 dadams
;;     find-dired-default-fn:
;;       devar -> defcustom.
;;       symbol-name-nearest-point -> region-or-non-nil-symbol-name-nearest-point.
;;     find(-name|-grep)-dired: Use functionp as test, not just non-nil.
;; 2011/01/04 dadams
;;     Removed autoloads for defvar, defconst, and non-interactive functions.
;; 2010/03/24 dadams
;;     find-grep-dired:
;;       Added missing DEFAULT arg for read-from-minibuffer.
;;       Thx to Sascha Friedmann.
;;     find(-name)-dired: Use nil as INITIAL-CONTENTS arg to read-from-minibuffer.
;; 2006/03/20 dadams
;;     menu-bar-dired-subdir-menu -> diredp-menu-bar-subdir-menu.
;; 2000/09/27 dadams
;;     Updated for Emacs 20.7.
;; 2000/09/18 dadams
;;     1. find-dired: a) Use dired-simple-subdir-alist & find-ls-option anew
;;                       (instead of dired's default switches).
;;                    b) Updated to Emacs20 version: define-key added.
;;     2. Added: find-ls-option - redefined to treat Windows too.
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

;; (require 'find-dired-) ;; for new defvars from `find-dired.el'
(require 'find-dired)

(require 'dired+ nil t) ;; (no error if not found):
                        ;; dired-insert-set-properties,
                        ;; diredp-menu-bar-subdir-menu
 ;; Note: `dired+.el' does a (require 'dired): dired-mode-map

(when (and (require 'thingatpt+ nil t);; (no error if not found)
           (fboundp 'tap-put-thing-at-point-props)) ; >= 2012-08-21
  (tap-define-aliases-wo-prefix)
  (tap-put-thing-at-point-props))
 ;; region-or-non-nil-symbol-name-nearest-point

;; Quiet the byte-compiler.
(defvar find-ls-subdir-switches)        ; Emacs 22+
(defvar find-name-arg)                  ; Emacs 22+
(defvar find-program)                   ; Emacs 22+
(defvar find-exec-terminator)           ; Emacs 22+
(defvar grep-program)                   ; Emacs 22+

;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar find-args nil                   ; Define it for older Emacs versions.
  "Last arguments given to `find' by `\\[find-dired]'.")

(defvar find-dired-hook nil
  "*Hook to be run at the end of each `find-dired' execution.
After this hook is run, hook `dired-after-readin-hook' is run.

Note that if you add a function to either of those hooks, because you
want it to act on the complete directory listing returned by the find
operation, you will likely also want the function to remove itself
from the hook when it is done.

For example, you might add a function like this to the hook, to toggle
marks after listing the files:

 (defun my-toggle-marks ()
    (dired-toggle-marks)
    (remove-hook 'find-dired-hook 'my-toggle-marks))")

(defvar find-diredp-repeating-search nil
  "Non-nil means `find-dired' was called using `revert-buffer-function'.")

(defvar find-diredp-time-history nil
  "History of formatted time strings used by `find-time-dired'.")


(defcustom find-diredp-default-fn (if (fboundp 'region-or-non-nil-symbol-name-nearest-point)
                                      (lambda () (region-or-non-nil-symbol-name-nearest-point t))
                                    'word-at-point)
  "*Function of 0 args called to provide default input for \\[find-dired],
\\[find-name-dired], and  \\[find-grep-dired].

Some reasonable choices: `word-nearest-point',
`region-or-non-nil-symbol-name-nearest-point',
`non-nil-symbol-name-nearest-point', `sexp-nearest-point'.

If this is nil, then no default input is provided."
  :type '(choice
          (const :tag "No default for `find-dired'" nil)
          (function :tag "Function of zero args to provide default for `find-dired'"))
  :group 'find-dired)

(defcustom find-diredp-time-prefix "m"
  "String prepended to \"min\" and \"max\" for time switches.
Used by `find-time-dired'."
  :type '(choice
          (const :tag "Use last file modification time"  "m")
          (const :tag "Use last file-status change"      "c"))
  :group 'find-dired)

(defcustom find-diredp-time-format (or (and (boundp 'display-time-format)  display-time-format)
                                       "%F %T")
  "String specifying format for displaying the time in `find-time-dired'.
See function `format-time-string'."
  :type 'string :group 'find-dired)

(defcustom find-diredp-max-cmd-line 512
  "Maximum number of chars in a command-line command.
If a command uses more chars than this then it is shown only in buffer
`*Messages*', not in the Dired buffer."
  :type 'integer :group 'find-dired)

(defcustom find-diredp-repeat-reuses-buffer-flag t
  "Non-nil means repeating `find' via `revert-buffer-function' reuses buffer."
  :type 'boolean :group 'find-dired)


;; REPLACES ORIGINAL in `find-dired.el':
;;
;; Use `dired-listing-switches' for Windows.
;;
;; Note: `defconst' is necessary here because this is preloaded by Emacs.
;;       It is not sufficient to do a defvar before loading `find-dired.el'.
;;       Otherwise, this could be just a `defvar' in `find-dired-.el'.
(defconst find-ls-option
    (cond ((eq system-type 'windows-nt) (cons "-ls" dired-listing-switches))
          ((and (fboundp 'process-file)  (boundp 'find-program)  (boundp 'null-device) ;Emacs22+
                (eq 0 (condition-case nil
                          (process-file find-program nil nil nil null-device "-ls")
                        (error nil))))
           (cons "-ls" (if (eq system-type 'berkeley-unix) "-gilsb" "-dilsb")))
          (t (cons (format "-exec ls -ld {} %s" (if (boundp 'find-exec-terminator) find-exec-terminator "\\;"))
                   "-ld")))
  "*Description of the option to `find' to produce an `ls -l'-type listing.
This is a cons of two strings (FIND-OPTION . LS-SWITCHES).
FIND-OPTION is the option (or options) for `find' needed to produce
 the desired output.
LS-SWITCHES is a list of `ls' switches that tell Dired how to parse
 the output.")

;; Make `defconst' override custom value.
(when (fboundp 'custom-reevaluate-setting) (custom-reevaluate-setting 'find-ls-option))


;; REPLACES ORIGINAL in `find-dired.el':
;;
;; 1. Added optional args.
;; 2. Interactive spec uses `find-diredp-default-fn'.
;; 3. Runs `find-dired-hook' at end.
;; 4. Buffer used has same root name as the dir (not "*Find*").
;;;###autoload
(defun find-dired (dir args &optional depth-limits excluded-paths)
  "Run `find' and put its output in a buffer in Dired Mode.
Then run `find-dired-hook' and `dired-after-readin-hook'.
The `find' command run (after changing into DIR) is essentially this,
where LS-SWITCHES is `(car find-ls-option)':

  find . \\( ARGS \\) LS-SWITCHES

Optional args:

* DEPTH-LIMITS:   Minimum and maximum depths: (MIN-DEPTH MAX-DEPTH).
* EXCLUDED-PATHS: Strings matching paths to be excluded.
                  Uses `find' switch `-path'.

When both optional args are non-nil, the `find' command run is this:

  find . -mindepth MIN-DEPTH -maxdepth MAX-DEPTH
         \\( -path \*edir1\* -o -path \*edir2\* ... \\)
         -prune -o \\( ARGS \\) LS-SWITCHES"
  (interactive
   (let ((default  (and (functionp find-diredp-default-fn) (funcall find-diredp-default-fn))))
     (list (funcall (if (fboundp 'read-directory-name) 'read-directory-name 'read-file-name)
                    "Run `find' in directory: " nil "" t)
           (read-string "Run `find' (with args): " find-args '(find-args-history . 1) default))))
  (let ((dired-buffers  dired-buffers))
    ;; Expand DIR ("" means default-directory), and make sure it has a trailing slash.
    (setq dir  (file-name-as-directory (expand-file-name dir)))
    (unless (file-directory-p dir) (error "Command `find-dired' needs a directory: `%s'" dir))
    (if (not find-diredp-repeating-search)
        (switch-to-buffer (create-file-buffer (directory-file-name dir)))
      (setq find-diredp-repeating-search  nil)
      (unless find-diredp-repeat-reuses-buffer-flag
        (switch-to-buffer (create-file-buffer (directory-file-name dir)))))
    ;; See if there is still a `find' running, and offer to kill it first, if so.
    (let ((find-proc  (get-buffer-process (current-buffer))))
      (when find-proc
	(if (or (not (eq (process-status find-proc) 'run))
		(yes-or-no-p "A `find' process is running; kill it? "))
	    (condition-case nil
		(progn (interrupt-process find-proc) (sit-for 1)
                       (delete-process find-proc))
	      (error nil))
	  (error "Cannot have two processes in `%s' at once" (buffer-name)))))
    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only  nil)
    (erase-buffer)
    (let ((dlim0  (elt depth-limits 0))
          (dlim1  (elt depth-limits 1))
          (quot*  (shell-quote-argument "*")))
      (setq default-directory  dir
            find-args          args     ; Save for next interactive call.
            args               (concat (if (boundp 'find-program) find-program "find") " . "
                                       (if (and (>= (length depth-limits) 2)
                                                (integerp dlim0)  (>= dlim0 0)
                                                (integerp dlim1)  (>= dlim1 0))
                                           (format "-mindepth %d -maxdepth %d " (min dlim0 dlim1) (max dlim0 dlim1)))
                                       (if excluded-paths
                                           (let* ((ex-paths  excluded-paths)
                                                  (excluded  (concat "\\( -path " quot* (pop ex-paths) quot* " ")))
                                             (while ex-paths
                                               (setq excluded  (concat excluded " -o -path " quot* (pop ex-paths) quot*)))
                                             (setq excluded  (concat excluded " \\) -prune -o "))))
                                       (if (string= args "")
                                           ""
                                         (format "%s %s %s " (shell-quote-argument "(") args (shell-quote-argument ")")))
                                       (if (string-match "\\`\\(.*\\) {} \\(\\\\;\\|+\\)\\'" (car find-ls-option))
                                           (format "%s %s %s"
                                                   (match-string 1 (car find-ls-option))
                                                   (shell-quote-argument "{}")
                                                   (if (boundp 'find-exec-terminator) find-exec-terminator "\\;"))
                                         (car find-ls-option)))))
    (shell-command (concat args "&") (current-buffer)) ; Start `find' process.
    ;; The next statement will bomb in classic Dired (no optional arg allowed)
    (dired-mode dir (cdr find-ls-option))
    (let ((map  (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map "\C-c\C-k" 'kill-find)
      (use-local-map map))
    (when (boundp 'dired-sort-inhibit) (set (make-local-variable 'dired-sort-inhibit) t))
    (set (make-local-variable 'revert-buffer-function)
	 `(lambda (ignore-auto noconfirm)
           (setq find-diredp-repeating-search  t)
           (find-dired ,dir ,find-args ',depth-limits ',excluded-paths)))
    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
        ;; Works even with nested Dired format (dired-nstd.el,v 1.15 and later)
        (dired-simple-subdir-alist)
      ;; We have an ancient tree Dired (or classic Dired, where this does no harm)
      (set (make-local-variable 'dired-subdir-alist)
           (list (cons default-directory (point-min-marker)))))
    (when (boundp 'find-ls-subdir-switches) (set (make-local-variable 'dired-subdir-switches) find-ls-subdir-switches))
    (setq buffer-read-only  nil)
    ;; Subdir headerline must come first because first marker in `subdir-alist' points there.
    (insert "  " (abbreviate-file-name dir) ":\n")
    ;; Make second line a "find" line by analogy to the "total" or "wildcard" line.
    (let ((opoint  (point)))
      (if (<= (length args) find-diredp-max-cmd-line)
          (insert "  " args "\n")
        (insert "  Command longer than `find-diredp-max-cmd-line'.  See buffer `*Messages*'\n")
        (message args))
      (dired-insert-set-properties opoint (point)))
    (setq buffer-read-only t)
    (let ((proc  (get-buffer-process (current-buffer))))
      (set-process-filter proc (function find-dired-filter))
      (set-process-sentinel proc (function find-dired-sentinel))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) (point) (current-buffer)))
    (setq mode-line-process  '(": %s `find'"))))


;; REPLACES ORIGINAL in `find-dired.el':
;;
;; 1. Added optional args.
;; 2. Interactive spec uses `read-from-minibuffer', `read-directory-name',
;;    `dired-regexp-history' and `find-diredp-default-fn'.
;;
;;;###autoload
(defun find-name-dired (directory pattern &optional depth-limits excluded-paths)
  "Run `dired' on all files under DIRECTORY that match globbing PATTERN.
PATTERN can use shell wildcards, and it need not be quoted.  It is not
an Emacs regexp.

By default, the command run (after changing into DIR) is essentially
this, where LS-SWITCHES is `(car find-ls-option)':

  find . -name 'PATTERN' LS-SWITCHES

See `find-name-arg' to customize the `find' file-name pattern arg.

Optional arg DEPTH-LIMITS is a list (MIN-DEPTH MAX-DEPTH) of the
 minimum and maximum depths.  If nil, search directory tree under DIR.

Optional arg EXCLUDED-PATHS is a list of strings that match paths to
 exclude from the search.  If nil, search all directories.

When both optional args are non-nil, the `find' command run is this:

  find . -mindepth MIN-DEPTH -maxdepth MAX-DEPTH
         \\( -path \*edir1\* -o -path \*edir2\* ... \\)
         -prune -o name 'PATTERN' \\( ARGS \\) LS-SWITCHES"
  (interactive
   (let ((default  (and (functionp find-diredp-default-fn) (funcall find-diredp-default-fn))))
     (list (if (fboundp 'read-directory-name) ; Emacs 22+
               (read-directory-name "Find-name (directory): " nil nil t)
             (read-file-name "Find-name (directory): " nil "" t))
           (read-from-minibuffer "Find-name (filename wildcard): " nil
                                 nil nil 'dired-regexp-history default t))))
  (find-dired directory (concat (if (boundp 'find-name-arg) find-name-arg "-name") " " (shell-quote-argument pattern))
              depth-limits excluded-paths))


;; REPLACES ORIGINAL in `find-dired.el':
;;
;; 1. Added optional args.
;; 2. Interactive spec uses `read-from-minibuffer', `read-file-name',
;;    `dired-regexp-history' and `find-diredp-default-fn'.
;;
;;;###autoload
(defun find-grep-dired (dir regexp &optional depth-limits excluded-paths)
  "Find files in DIR containing a regexp REGEXP.
The output is in a Dired buffer.
The `find' command run (after changing into DIR) is essentially this,
where LS-SWITCHES is `(car find-ls-option)':

  find . -exec grep find-grep-options REGEXP {} \\\; LS-SWITCHES

Thus REGEXP can also contain additional grep options.

Optional arg DEPTH-LIMITS is a list (MIN-DEPTH MAX-DEPTH) of the
 minimum and maximum depths.  If nil, search directory tree under DIR.

Optional arg EXCLUDED-PATHS is a list of strings that match paths to
 exclude from the search.  If nil, search all directories.

When both optional args are non-nil, the `find' command run is this:

  find . -mindepth MIN-DEPTH -maxdepth MAX-DEPTH
         \\( -path \*edir1\* -o -path \*edir2\* ... \\)
         -prune -o -exec grep find-grep-options REGEXP {} \\\;
         LS-SWITCHES"
  (interactive
   (let ((default  (and (functionp find-diredp-default-fn) (funcall find-diredp-default-fn))))
     (list (read-file-name "Find-grep (directory): " nil "" t)
           (read-from-minibuffer "Find-grep (grep regexp): " nil nil nil 'dired-regexp-history default t))))
  ;; find -exec does not allow shell i/o redirections in the command.  If it did, we could use `grep -l >/dev/null'.
  ;; Use -type f, not ! -type d, to avoid getting screwed by FIFOs & devices.  Not sure what is best to do about symlinks.
  (find-dired dir (format "-type f -exec %s %s -e %s %s %s"
                          (if (boundp 'grep-program) grep-program "grep")
                          find-grep-options
                          (shell-quote-argument regexp)
                          (shell-quote-argument "{}")
                          (shell-quote-argument ";"))
              depth-limits excluded-paths))


;; REPLACES ORIGINAL in `find-dired.el':
;; Removes lines that just list a file.
(defun find-dired-filter (proc string)
  "Filter for \\[find-dired] processes.
PROC is the process.
STRING is the string to insert."
  (let ((buf                (process-buffer proc))
        (inhibit-read-only  t))
    (if (buffer-name buf)
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (widen)
              (let ((buffer-read-only  nil)
                    (beg               (point-max))
                    (l-opt             (and (consp find-ls-option)  (string-match "l" (cdr find-ls-option))))
                    (ls-regexp         "^ +[^ \t\r\n]+\\( +[^ \t\r\n]+\\) +[^ \t\r\n]+ +[^ \t\r\n]+\\( +[0-9]+\\)"))
                (goto-char beg)
                (insert string)
                (goto-char beg)
                (unless (looking-at "^") (forward-line 1))
                (while (looking-at "^")
                  (insert "  ")
                  (forward-line 1))
                (goto-char (- beg 3))   ; No error if < 0.
                (save-excursion         ; Remove lines just listing the file.
                  (while (re-search-forward "^  \\./" nil t)
                    (delete-region (line-beginning-position) (line-end-position))
                    (when (eq (following-char) ?\n) (delete-char 1))))
                ;; Convert ` ./FILE' to ` FILE'.
                ;; This will lose if current chunk of output starts or ends within the ` ./', so back up a bit.
                (while (search-forward " ./" nil t) (delete-region (point) (- (point) 2)))
		;; Pad number of links and file size.  Quick & dirty way to get columns to line up, but's not foolproof.
		(when l-opt
		  (goto-char beg)
		  (goto-char (line-beginning-position))
		  (while (re-search-forward ls-regexp nil t)
		    (replace-match (format "%4s" (match-string 1)) nil nil nil 1)
		    (replace-match (format "%9s" (match-string 2)) nil nil nil 2)
		    (forward-line 1)))
                ;; Find the complete lines in the unprocessed output, and add text props to them.
                (goto-char (point-max))
                (when (search-backward "\n" (process-mark proc) t)
                  (dired-insert-set-properties (process-mark proc) (1+ (point)))
                  (move-marker (process-mark proc) (1+ (point))))))))
      (delete-process proc))))          ; The buffer was killed.


;; REPLACES ORIGINAL in `find-dired.el':
;;
;; 1. Highlights file lines.
;; 2. Puts `find' in mode-line.
;; 3. Runs hooks `find-dired-hook' and `dired-after-readin-hook'.
;;
(defun find-dired-sentinel (proc state)
  "Sentinel for \\[find-dired] processes.
PROC is the process.
STATE is the state of process PROC."
  (let ((buf                (process-buffer proc))
        (inhibit-read-only  t))
    (when (buffer-name buf)
      (with-current-buffer buf
        (let ((buffer-read-only  nil))
          (save-excursion
            (goto-char (point-max))
            (insert "\n  find " state)  ; STATE is, e.g., "finished".  Insert 2 SPC so f in `find' is not taken as a mark.
            (forward-char -1)           ; Back up before \n at end of STATE.
            (insert " at " (substring (current-time-string) 0 19))
            (forward-char 1)
            (setq mode-line-process  (concat ": " (symbol-name (process-status proc)) " `find'"))
            ;; Since the buffer and mode line will show that the process is dead, we can
            ;; delete it now.  Otherwise it will stay around until M-x list-processes.
            (delete-process proc)
            ;; Highlight lines of file names for mouse selection.
            (dired-insert-set-properties (point-min) (point-max))
            (force-mode-line-update)))
        (run-hooks 'find-dired-hook 'dired-after-readin-hook)
        (message "find-dired `%s' done." (buffer-name))))))

(when (require 'time-date nil t)        ; Emacs 22+
  (require 'parse-time)                 ; parse-time-string

  (defun find-time-dired (dir min-time max-time &optional depth-limits excluded-paths)
    "Find files in directory DIR newer or older than a timestamp.
The output is shown in a Dired buffer.

MIN-TIME is a format-time string parsable by `parse-time-string', such
 as \"2014-12-25 23:59:00\".  Only files newer than this are shown.
 If MIN-TIME is nil or a string matching regexp \"^\\s-*$\", there is
 no lower time limit.

MAX-TIME is also a format-time string parsable by `parse-time-string'.
 Only files older than this time are shown.
 If MAX-TIME is nil or a string matching regexp \"^\\s-*$\", the upper
 time limit is the current system time.

Optional arg DEPTH-LIMITS is a list (MIN-DEPTH MAX-DEPTH) of the
 minimum and maximum depths.  If nil, search directory tree under DIR.

Optional arg EXCLUDED-PATHS is a list of strings that match paths to
 exclude from the search.  If nil, search all directories.

If args DEPTH-LIMITS and EXCLUDED-PATHS are both non-nil then the
command run is essentially the following:

    find . -mindepth MIN-DEPTH -maxdepth MAX-DEPTH
           \\( -path \*edir1\* -o -path \*edir2\* ... \\)
           -prune -o \\( -TIME-SWITCH -SINCE-MIN -TIME-SWITCH +SINCE-MAX \\)
           LS-SWITCHES

where:

* TIME-SWITCH is `find-diredp-time-prefix' concatenated with \"min\".
* SINCE-MIN is the elapsed time since MIN-TIME in minutes.
* SINCE-MAX is the elapsed time since MAX-TIME in minutes.
* LS-SWITCHES is `(car find-ls-option)'."
    ;; `parse-time-string' ignores AM/PM:
    ;; (let ((time  "Mon 10 Aug 2015 01:53:58 PM JST"))
    ;;   (format-time-string "%c" (apply 'encode-time (parse-time-string time))))
    ;; => "Mon 10 Aug 2015 01:53:58 AM JST"
    ;;
    ;; Avoid including %p (AM/PM) on find-diredp-time-format.
    (interactive
     (let* ((mod/status-time  (if (string= "m" find-diredp-time-prefix) 5 6))
            (default-min      (or (and (eq major-mode 'dired-mode)  (dired-get-filename t t)
                                       (format-time-string find-diredp-time-format
                                                           (elt (file-attributes (dired-get-filename)) mod/status-time)))
                                  ""))
            (default-max      (format-time-string find-diredp-time-format (current-time))))
       (list (read-file-name "Find-time (directory): " nil "" t)
             (read-from-minibuffer "Find-time AFTER (format string): "
                                   default-min nil nil '(find-diredp-time-history . 1) default-min t)
             (read-from-minibuffer "Find-time BEFORE (format string): " default-max
                                   nil nil nil default-max t))))
    (when (and (stringp min-time)  (string-match "^\\s-*$" min-time)) (setq min-time  nil))
    (when (and (stringp max-time)  (string-match "^\\s-*$" max-time))
      (setq max-time  (format-time-string find-diredp-time-format (current-time))))
    (let* ((now       (current-time))
           (now-strg  (format-time-string find-diredp-time-format now))
           (t1        (and min-time  (find-diredp--parse-time min-time)))
           (t2        (find-diredp--parse-time (or max-time
                                                   (format-time-string find-diredp-time-format (current-time))))))
      (setq min-time  (and t1  (format-time-string find-diredp-time-format t1))
            max-time  (format-time-string find-diredp-time-format t2))
      (when (and t1  (not (time-less-p t1 now)))
        (error "Min time/date (%s) is in the future (now= %s)" min-time now-strg))
      (unless (time-less-p t2 now)
        (error "Max time/date (%s) is in the future (now= %s)" max-time now-strg))
      (when (and t1  (not (time-less-p t1 t2)))
        (error "Max time/date (%s) is earlier than min time/date (%s)" max-time min-time))
      (let* ((t1-period  (or (and t1  (format "%.0f" (/ (time-to-seconds (time-since t1)) 60)))
                             "-inf"))
             (t2-period  (or (and t2  (format "%.0f" (/ (time-to-seconds (time-since t2)) 60)))
                             "0"))
             (arg-time   (concat "-type f")))
        (unless (string= t1-period "-inf")
          (setq arg-time  (concat arg-time " -" find-diredp-time-prefix "min -" t1-period)))
        (unless (string= t2-period "0")
          (setq arg-time  (concat arg-time " -" find-diredp-time-prefix "min +" t2-period)))
        (find-dired dir arg-time depth-limits excluded-paths))))


  ;; Try to support as many sensible date string formats as possible.  Example supported strings:
  ;; 2000
  ;; feb
  ;; feb 15
  ;; feb 15 2000
  ;; 19:20:56
  ;; 19:20:56 15
  ;; 19:20:56 feb 15
  ;; 19:20:56 feb 15 2000
  ;; 19:20:56 feb 15 2000
  ;;
  ;; Formats such as these are likely mistakes, and are not supported:
  ;; 19:20:56 2000
  ;; 19:20:56 Jul
  ;; 28 2000
  (defun find-diredp--parse-time (time)
    "Parse date/time string TIME, returning the corresponding internal time.
Any parts of TIME that are incomplete are taken from `current-time',
and `encode-time' is applied to the result and returned.
If `encode-time' returns nil then the current time is returned."
    (let* ((parsed-time  (and time  (stringp time)  (parse-time-string time)))
           (has-pm       (member "pm" (parse-time-tokenize time)))
           (basic        (butlast parsed-time 3))
           (rest         (nthcdr 6 parsed-time))
           (components   (mapcar #'integerp basic))
           (curtime      (current-time)))
      (let ((res  (vconcat
                   (cond ((equal components '(nil nil nil nil nil t)) ; YEAR
                          (vector 0 0 0 1 1 (elt basic 5)))
                         ((equal components '(nil nil nil nil t nil)) ; MONTH
                          (vector 0 0 0 1 (elt basic 4)
                                  (string-to-number (format-time-string "%Y" curtime))))
                         ((equal components '(nil nil nil t t nil)) ; DAY, MONTH
                          (vector 0 0 0 (elt basic 3) (elt basic 4)
                                  (string-to-number (format-time-string "%Y" curtime))))
                         ((equal components '(nil nil nil nil t t)) ; MONTH, YEAR
                          (vector 0 0 0 1 (elt basic 4) (elt basic 5)))
                         ((equal components '(nil nil nil t t t)) ; DAY, MONTH, YEAR
                          (vector 0 0 0 (elt basic 3) (elt basic 4) (elt basic 5)))
                         ((equal components '(t t t nil nil nil)) ; SEC, MIN, HOUR
                          (vector (elt basic 0) (elt basic 1) (elt basic 2)
                                  (string-to-number (format-time-string "%d" curtime))
                                  (string-to-number (format-time-string "%m" curtime))
                                  (string-to-number (format-time-string "%Y" curtime))))
                         ((equal components '(t t t t nil nil)) ; SEC, MIN, HOUR, DAY
                          (vector (elt basic 0) (elt basic 1) (elt basic 2) (elt basic 3) 
                                  (string-to-number (format-time-string "%m" curtime))
                                  (string-to-number (format-time-string "%Y" curtime))))
                         ((equal components '(t t t t t nil)) ; SEC, MIN, HOUR, DAY, MONTH
                          (vector
                           (elt basic 0) (elt basic 1) (elt basic 2) (elt basic 3) (elt basic 4)
                           (string-to-number (format-time-string "%Y" curtime))))
                         ((equal components '(t t t t t t)) ; SEC, MIN, HOUR, DAY, MONTH, YEAR
                          basic) 
                         (t (error "Cannot parse date/time string: `%S'" time)))
                   rest)))
        (let ((hour  (elt res 2))) (when (and has-pm  (< hour 12))  (aset res 2 (+ hour 12))))
        (setq res  (apply 'encode-time (append res ()))))))

  )


;; Menu bar, `find' menu.
(defvar menu-bar-run-find-menu (make-sparse-keymap "Unix `find'"))
(defalias 'menu-bar-run-find-menu (symbol-value 'menu-bar-run-find-menu))
(define-key menu-bar-run-find-menu [find-dired]
  '("`find' <anything>..." . find-dired))
(define-key menu-bar-run-find-menu [find-time-dired] '("Find Files By Time..." . find-time-dired))
(define-key menu-bar-run-find-menu [find-grep-dired] '("Find Files Containing..." . find-grep-dired))
(define-key menu-bar-run-find-menu [find-name-dired] '("Find Files Named..." . find-name-dired))
;; Add it to Dired's `Search' menu.
(when (boundp 'menu-bar-search-menu)
  (define-key dired-mode-map [menu-bar search separator-find] '("--"))
  (define-key dired-mode-map [menu-bar search find] '("Run `find' Command" . menu-bar-run-find-menu)))
;; Add it to Dired's `Dir' menu (called `Subdir' in `dired.el').
(when (boundp 'diredp-menu-bar-subdir-menu) ; Defined in `dired+.el'.
  (if (or (> emacs-major-version 21)  (fboundp 'wdired-change-to-wdired-mode))
      (define-key-after diredp-menu-bar-subdir-menu [find]
        '("Run `find' Command" . menu-bar-run-find-menu)
        'wdired-mode)
    (define-key diredp-menu-bar-subdir-menu [find] '("Run `find' Command" . menu-bar-run-find-menu))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'find-dired+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find-dired+.el ends here

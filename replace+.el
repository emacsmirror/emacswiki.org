;;; replace+.el --- Extensions to `replace.el'.
;;
;; Filename: replace+.el
;; Description: Extensions to `replace.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2012, Drew Adams, all rights reserved.
;; Created: Tue Jan 30 15:01:06 1996
;; Version: 21.0
;; Last-Updated: Sun Jan  1 14:05:12 2012 (-0800)
;;           By: dradams
;;     Update #: 1175
;; URL: http://www.emacswiki.org/cgi-bin/wiki/replace+.el
;; Keywords: matching, help, internal, tools, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `avoid', `faces', `faces+', `fit-frame',
;;   `frame-cmds', `frame-fns', `help+20', `highlight', `info',
;;   `info+', `isearch+', `menu-bar', `menu-bar+', `misc-cmds',
;;   `misc-fns', `naked', `second-sel', `strings', `thingatpt',
;;   `thingatpt+', `unaccent', `w32browser-dlgopen', `wid-edit',
;;   `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `replace.el'.
;;
;;  Functions defined here:
;;
;;    `query-replace-w-options', `toggle-replace-w-completion'.
;;
;;  Faces defined here:
;;
;;    `occur-highlight-linenum'.
;;
;;  User options defined here:
;;
;;    `replace-w-completion-flag', `search/replace-default-fn'.
;;
;;  Internal variable defined here: `occur-regexp'.
;;
;;
;;  ***** NOTE: The following functions defined in `replace.el' have
;;              been REDEFINED HERE:
;;
;;    `flush-lines' - (Not needed for Emacs 21)
;;                    1. The prompt has been changed, to mention that
;;                       only lines after point are affected.
;;                    2. The default regexp is provided by
;;                       `search/replace-default-fn'.
;;                    3. An in-progress message has been added.
;;    `how-many' - (Not needed for Emacs 21)
;;                 1. Prompt changed: lines after point are affected.
;;                 2. Default regexp: `search/replace-default-fn'.
;;                 3. An in-progress message has been added.
;;    `keep-lines' - Same as `flush-lines'. (Not needed for Emacs 21)
;;    `occur' - 1. Default regexp is from `search/replace-default-fn'.
;;              2. Regexp is saved as `occur-regexp' for use by
;;                 `occur-mode-mouse-goto'
;;    `occur-mode-goto-occurrence', `occur-mode-display-occurrence', 
;;    `occur-mode-goto-occurrence-other-window',
;;    `occur-mode-mouse-goto' - Highlights regexp in source buffer
;;                              and visited linenum in occur buffer.
;;    `occur-read-primary-args' - (Emacs 21 only) Default regexps via
;;                                `search/replace-default-fn'.
;;    `query-replace-read-args',  - (Not needed for Emacs 21+)
;;                                1. Uses `completing-read' if
;;                                   `replace-w-completion-flag' is
;;                                   non-nil.
;;                                2. Default regexps are obtained via
;;                                   `search/replace-default-fn'.
;;    `query-replace-read-(from|to)' - Same as `query-replace-read-args',
;;                                     but for Emacs 21+.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `replace.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "replace" '(progn (require 'replace+)))
;;
;;  For Emacs releases prior to Emacs 22, these Emacs 22 key bindings
;;  are made here:
;;
;;   (define-key occur-mode-map "o" 'occur-mode-goto-occurrence-other-window)
;;   (define-key occur-mode-map "\C-o" 'occur-mode-display-occurrence))
;;
;;  Suggested additional key binding:
;;
;;   (substitute-key-definition 'query-replace 'query-replace-w-options
;;                              global-map)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/12/19 dadams
;;     (keep|flush)-lines, occur(-mode-(mouse-goto|goto-occurrence(-other-window)|display-occurrence)):
;;       Use line-(beginning|end)-position, not (beginning|end)-of-line + point.
;; 2011/09/22 dadams
;;     Applied renaming of set-region-around-search-target to isearchp-set-region-around-search-target.
;; 2011/08/30 dadams
;;     search/replace-default-fn:
;;       defvar -> defcustom.
;;       symbol-name-nearest-point -> non-nil-symbol-name-nearest-point.
;;     query-replace-read-(to|from|args), (keep|flush)-lines, how-many, occur, occur-read-primary-args:
;;       Use functionp, not fboundp.
;; 2011/08/24 dadams
;;     Added macro menu-bar-make-toggle-any-version.  Use for menu-bar-toggle-replace-w-completion.
;; 2011/08/22 dadams
;;     menu-bar-toggle-replace-w-completion:
;;       Just use menu-bar-make-toggle, and adjust for diff releases.  Thx to PasJa (EmacsWiki).
;;     Removed eval-when-compile soft require of menu-bar+.el.
;; 2011/04/16 dadams
;;     occur, occur-mode-mouse-goto:
;;       Fix for lexbind Emacs 24: replace named arg REGEXP, EVENT by (ad-get-arg 0).
;; 2011/01/04 dadams
;;     Removed autoload cookies for non def* sexps, defadvice, and non-interactive fns.  Added for cmds.
;; 2010/01/12 dadams
;;     occur, occur-mode-mouse-goto: save-excursion + set-buffer -> with-current-buffer.
;; 2009/04/26 dadams
;;     occur-mode-mouse-goto, occur-mode-goto-occurrence(-other-window), occur-mode-display-occurrence:
;;       Bind inhibit-field-text-motion to t, for end-of-line.
;; 2008/03/31 dadams
;;     query-replace-w-options: current-prefix-arg -> prefix, so C-x ESC ESC will work.
;; 2007/08/11 dadams
;;     Added soft require of menu-bar+.el.
;;     Moved here from menu-bar+.el: Bind query-replace-w-options in menu-bar-search-replace-menu.
;;                                   Bind replace-w-completion-flag in menu-bar-options-menu.
;; 2007/06/02 dadams
;;     Renamed highlight-regexp-region to hlt-highlight-regexp-region.
;; 2007/03/15 dadams
;;     Added: occur-mode-goto-occurrence-other-window, occur-mode-display-occurrence.
;; 2006/08/01 dadams
;;     query-replace-w-options: Select last occurrence, if isearchp-set-region-flag is non-nil.
;;     Added soft require of isearch+.el.
;; 2006/03/31 dadams
;;     No longer use display-in-minibuffer.
;;     query-replace-w-options: Simplified code.
;; 2006/02/03 dadams
;;     All calls to read-from-minibuffer: Use default arg, not initial-value arg.
;; 2005/12/30 dadams
;;     replace-w-completion-flag: Use defcustom.
;;     Use defface instead of define-face-const.  Renamed face without "-face".
;;     Removed redefinition of list-matching-lines-face - do that in start-opt.el now.
;;     Removed require of def-face-const.
;; 2005/10/31 dadams
;;     Use nil as init-value arg in calls to completing-read, everywhere.
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2005/01/25 dadams
;;     Renamed: replace-w-completion -> replace-w-completion-flag.
;; 2004/12/09 dadams
;;     Added occur-highlight-linenum-face.
;; 2004/11/20 dadams
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/10/12 dadams
;;     Updated for Emacs 21 also:
;;       query-replace-w-options:
;;         Added args start & end.
;;         Removed arg display-msgs, so can no longer simulate interactive-p.
;;         Uses query-replace-read-args.
;;       Added query-replace-read-(from|to) and occur-read-primary-args.
;;       Made some fns Emacs-20 only.
;;       Removed defaliases for keep-lines, flush-lines, and how-many.
;;       occur: New version for Emacs 21 via defadvice.
;;     Only require cl.el for compiling.
;;     occur-mode-mouse-goto, occur-mode-goto-occurrence:
;;       Redefined, using defadvice.
;; 2004/10/07 dadams
;;     Renamed resize-frame to fit-frame.
;; 2004/06/01 dadams
;;     Renamed shrink-frame-to-fit to resize-frame.
;; 1996/06/20 dadams
;;     flush-lines, keep-lines: Default regexp from search/replace-default-fn.
;; 1996/06/14 dadams
;;     1. Added: replace-w-completion, toggle-replace-w-completion.
;;     2. query-replace-read-args, query-replace-w-options: Now sensitive to
;;        replace-w-completion.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/04/22 dadams
;;     Added: flush-lines, keep-lines.
;; 1996/04/15 dadams
;;     occur: Explicitly call shrink-frame-to-fit each time, after displaying.
;; 1996/03/26 dadams
;;     1. Added redefinition of query-replace-read-args.
;;     2. perform-replace: cond -> case.
;;     3. query-replace-w-options: message -> display-in-minibuffer (STRING).
;; 1996/03/20 dadams
;;     query-replace-w-options: Defaults for new and old are the same.
;; 1996/03/20 dadams
;;     1. Added search/replace-default-fn.
;;     2. query-replace-w-options, occur:
;;        symbol-name-nearest-point -> search/replace-default-fn.
;; 1996/02/15 dadams
;;     occur: Don't raise Occur frame if no occurrences.
;; 1996/02/05 dadams
;;     occur-mode-goto-occurrence, occur-mode-mouse-goto: Highlight last goto lineno.
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

;; Cannot do (require 'replace), because `replace.el' does no `provide'.
;; Don't want to do a (load-library "replace") either, because it wouldn't
;; allow doing (eval-after-load "replace" '(progn (require 'replace+)))

(eval-when-compile (require 'cl)) ;; incf (plus, for Emacs 20: push, pop,
                                  ;;       and, for Emacs <20: cadr, when, unless)

(require 'thingatpt nil t) ;; (no error if not found): word-at-point
(require 'thingatpt+ nil t) ;; (no error if not found): non-nil-symbol-name-nearest-point
(require 'frame-cmds nil t) ;; (no error if not found): show-a-frame-on
(require 'frame-fns nil t) ;; (no error if not found): get-a-frame
(require 'fit-frame nil t) ;; (no error if not found): fit-frame
(require 'highlight nil t) ;; (no error if not found): hlt-highlight-regexp-region
(require 'isearch+ nil t) ;; (no error if not found):
                          ;; isearchp-set-region-around-search-target, isearchp-set-region-flag
(require 'menu-bar+ nil t) ;; menu-bar-options-menu, menu-bar-search-replace-menu

;;;;;;;;;;;;;;;;;;;;;

;; Same as the version in `menu-bar+.el'.
(defmacro menu-bar-make-toggle-any-version (name variable doc message help &rest body)
  "Return a valid `menu-bar-make-toggle' call in Emacs 20 or later.
NAME is the name of the toggle command to define.
VARIABLE is the variable to set.
DOC is the menu-item name.
MESSAGE is the toggle message, minus status.
HELP is :help string.
BODY is the function body to use.  If present, it is responsible for
setting the variable and displaying a status message (not MESSAGE)."
  (if (< emacs-major-version 21)
      `(menu-bar-make-toggle ,name ,variable ,doc ,message ,@body)
    `(menu-bar-make-toggle ,name ,variable ,doc ,message ,help ,@body)))

(defface occur-highlight-linenum '((t (:foreground "Red")))
  "*Face to use to highlight line number of visited hit lines."
  :group 'matching :group 'faces)

;; This is defined in `faces.el', Emacs 22.  This definition is adapted to Emacs 20.
(unless (facep 'minibuffer-prompt)
  (defface minibuffer-prompt '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
    "*Face for minibuffer prompts."
    :group 'basic-faces))

(defvar occur-regexp nil "Search pattern used by `occur' command.") ; Internal variable.

(defcustom replace-w-completion-flag nil
  "*Non-nil means use minibuffer completion for replacement commands
such as `query-replace'.  With completion, to insert a SPC or TAB
char, you will need to preceed it by `\\[quoted-insert]'.  If this is
inconvenient, set this variable to nil."
  :type 'boolean :group 'matching)

;;;###autoload
(defun toggle-replace-w-completion (force-p)
  "Toggle whether to use minibuffer completion for replacement commands
such as `query-replace'.
Non-nil prefix arg FORCE-P => Use completion iff FORCE-P >= 0.

Note that with completion, to insert a SPC or TAB character you will
need to preceed it by `\\[quoted-insert]'.

This toggles the value of option `replace-w-completion-flag'."
  (interactive "P")
  (if force-p                           ; Force.
      (if (natnump (prefix-numeric-value force-p))
          (setq replace-w-completion-flag  t)
        (setq replace-w-completion-flag  nil))
    (setq replace-w-completion-flag  (not replace-w-completion-flag)))) ; Toggle.

(defcustom search/replace-default-fn (if (fboundp 'non-nil-symbol-name-nearest-point)
                                         'non-nil-symbol-name-nearest-point
                                       'word-at-point)
  "*Function to provide default input for search/replacement functions.
This includes commands such as \\[query-replace-w-options] and \\[occur].
The function is called with no arguments.

Some reasonable choices are defined in `thingatpt+.el':
`word-nearest-point', `non-nil-symbol-name-nearest-point',
`region-or-non-nil-symbol-name-nearest-point', `sexp-nearest-point'.

Note that if you use `region-or-non-nil-symbol-name-nearest-point' for
this then you cannot also take advantage of the use of the region to
bound the scope of query-replace.  Remember too that you can always
yank the region using `C-y'.

FWIW - Personally I use `region-or-non-nil-symbol-name-nearest-point'
as the value, and I also automatically insert the default value in the
minibuffer (`icicle-default-value' = `insert-end'.  Whenever I want to
restrict query-replace scope to the region I just narrow the buffer."
  :type '(choice
          (const :tag "No default input search/replacement functions" nil)
          (function :tag "Function of 0 args to provide default for search/replace"))
  :group 'matching)



;; REPLACES ORIGINAL in `replace.el'.
;; 1. Uses `completing-read' if `replace-w-completion-flag' is non-nil.
;; 2. Default values are provided by `search/replace-default-fn'.
;;
;; You can still use the history lists, and you can still enter
;; nothing to repeat the previous query replacement operation.
;;
;; However, in addition, this provides an initial value by
;; `search/replace-default-fn'.
;;
(when (> emacs-major-version 21)
  (defun query-replace-read-from (string regexp-flag)
    "Query and return the `from' argument of a query-replace operation.
The return value can also be a pair (FROM . TO) indicating that the user
wants to replace FROM with TO.
Non-nil `replace-w-completion-flag' means you can use completion."
    (if query-replace-interactive
        (car (if regexp-flag regexp-search-ring search-ring))
      (let* ((default   (if (functionp search/replace-default-fn)
                            (funcall search/replace-default-fn)
                          (car (symbol-value query-replace-from-history-variable))))
             (lastto    (car (symbol-value query-replace-to-history-variable)))
             (lastfrom  (car (symbol-value query-replace-from-history-variable)))
             (from-prompt
              (progn
                ;; Use second, not first, if the two history items are the same (e.g. shared lists).
                (when (equal lastfrom lastto)
                  (setq lastfrom  (cadr (symbol-value query-replace-from-history-variable))))
                (if (and lastto lastfrom)
                    (format "%s.  OLD (empty means %s -> %s): " string (query-replace-descr lastfrom)
                            (query-replace-descr lastto))
                  (concat string ".  OLD: "))))
             ;; The save-excursion here is in case the user marks and copies
             ;; a region in order to specify the minibuffer input.
             ;; That should not clobber the region for the query-replace itself.
             (from      (save-excursion
                          (if replace-w-completion-flag
                              (completing-read from-prompt obarray nil nil nil
                                               query-replace-from-history-variable default t)
                            (if query-replace-interactive
                                (car (if regexp-flag regexp-search-ring search-ring))
                              (read-from-minibuffer from-prompt nil nil nil
                                                    query-replace-from-history-variable default t))))))
        (if (and (zerop (length from)) lastto lastfrom)
            (cons lastfrom lastto)
          ;; Warn if user types \n or \t, but don't reject the input.
          (and regexp-flag
               (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\[nt]\\)" from)
               (let ((match  (match-string 3 from)))
                 (cond
                  ((string= match "\\n")
                   (message "Note: `\\n' here doesn't match a newline; to do that, type C-q C-j instead"))
                  ((string= match "\\t")
                   (message "Note: `\\t' here doesn't match a tab; to do that, just type TAB")))
                 (sit-for 2)))
          from)))))



;; REPLACES ORIGINAL in `replace.el'.
;; 1. Uses `completing-read' if `replace-w-completion-flag' is non-nil.
;; 2. Default values are provided by `search/replace-default-fn'.
;;
;; You can still use the history lists, and you can still enter
;; nothing to repeat the previous query replacement operation.
;;
;; However, in addition, this provides an initial value by
;; `search/replace-default-fn'.
;;
(when (> emacs-major-version 21)
  (defun query-replace-read-to (from string regexp-flag)
    "Query and return the `to' argument of a query-replace operation."
    (let* ((default    (if (functionp search/replace-default-fn)
                           (funcall search/replace-default-fn)
                         (car (symbol-value query-replace-to-history-variable))))
           (to-prompt  (format "%s.  NEW (replacing %s): " string (query-replace-descr from)))
           ;; The save-excursion here is in case the user marks and copies
           ;; a region in order to specify the minibuffer input.
           ;; That should not clobber the region for the query-replace itself.
           (to         (save-excursion
                         (if replace-w-completion-flag
                             (completing-read to-prompt obarray nil nil nil
                                              query-replace-to-history-variable default t)
                           (read-from-minibuffer to-prompt nil nil nil
                                                 query-replace-to-history-variable default t)))))
      (when (and regexp-flag (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\[,#]" to))
        (let (pos list char)
          (while (progn (setq pos  (match-end 0))
                        (push (substring to 0 (- pos 2)) list)
                        (setq char  (aref to (1- pos))
                              to    (substring to pos))
                        (cond ((eq char ?\#) (push '(number-to-string replace-count) list))
                              ((eq char ?\,)
                               (setq pos  (read-from-string to))
                               (push `(replace-quote ,(car pos)) list)
                               ;; Swallow a space after a symbol if there is a space.
                               (let ((end  (if (and (or (symbolp (car pos))
                                                        ;; Swallow a space after 'foo
                                                        ;; but not after (quote foo).
                                                        (and (eq (car-safe (car pos)) 'quote)
                                                             (not (= ?\( (aref to 0)))))
                                                    (eq (string-match " " to (cdr pos))
                                                        (cdr pos)))
                                               (1+ (cdr pos))
                                             (cdr pos))))
                                 (setq to  (substring to end)))))
                        (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\[,#]" to)))
          (setq to  (nreverse (delete "" (cons to list)))))
        (replace-match-string-symbols to)
        (setq to  (cons 'replace-eval-replacement (if (> (length to) 1) (cons 'concat to) (car to)))))
      to)))

(when (boundp 'menu-bar-search-replace-menu) ; In `menu-bar+.el'.
  (define-key menu-bar-search-replace-menu [query-replace]
    '(menu-item "Query String" query-replace-w-options
      :help "Replace string interactively asking about each occurrence"
      :enable (not buffer-read-only))))

(define-key-after menu-bar-options-menu [replace-w-completion-flag]
  (menu-bar-make-toggle-any-version menu-bar-toggle-replace-w-completion replace-w-completion-flag
                                    "Completion for Query Replace"
                                    "Using completion with query replace is %s"
                                    "Using completion with query replace")
  'case-fold-search)

;; The main difference between this and `query-replace' is in the treatment of the PREFIX
;; arg.  Only a positive (or nil) PREFIX value gives the same behavior.  A negative PREFIX
;; value does a regexp query replace.  Another difference is that non-nil
;; `isearchp-set-region-flag' means set the region around the last target occurrence.
;;
;; In Emacs 21+, this has the same behavior as the versions of `query-replace-read-to' and
;; `query-replace-read-from' defined here:
;;
;;    1. Uses `completing-read' if `replace-w-completion-flag' is non-nil.
;;    2. Default values are provided by `search/replace-default-fn'.
;;
;;    You can still use the history lists, and you can still enter
;;    nothing to repeat the previous query replacement operation.
;;    However, in addition, this provides an initial value by
;;    `search/replace-default-fn'.
;;
;; In Emacs 20, this has the same behavior as the version of `query-replace-read-args'
;; defined here:
;;
;;    1. It uses `completing-read' if `replace-w-completion-flag' is non-nil.
;;    2. The default regexps are provided by `search/replace-default-fn'.
;;
;;;###autoload
(defun query-replace-w-options (old new &optional prefix start end)
  "Replace some occurrences of OLD text with NEW one.
Fourth and fifth arg START and END specify the region to operate on.

This is the same as command `query-replace', except for the treatment
of a prefix argument.

No PREFIX argument (nil) means replace literal string matches.
Non-negative PREFIX argument means replace word matches.
Negative PREFIX argument means replace regexp matches.

Option `replace-w-completion-flag', if non-nil, provides for
minibuffer completion while you type OLD and NEW.  In that case, to
insert a SPC or TAB character, you will need to preceed it by \
`\\[quoted-insert]'.

If option `isearchp-set-region-flag' is non-nil, then select the last
replacement."
  (interactive
   (let* ((kind    (cond ((and current-prefix-arg (natnump (prefix-numeric-value current-prefix-arg)))
                          " WORD")
                         (current-prefix-arg " REGEXP")
                         (t " STRING")))
          (common  (query-replace-read-args (concat "Query replace" kind) (string= " REGEXP " kind))))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           ;; These are done separately here, so that command-history will record these expressions
           ;; rather than the values they had this time.
           (and transient-mark-mode mark-active (region-beginning))
           (and transient-mark-mode mark-active (region-end)))))
  (let ((kind  (cond ((and prefix (natnump (prefix-numeric-value prefix))) 'WORD)
                     (prefix 'REGEXP)
                     (t 'STRING))))
    (case kind
      (WORD
       (if (< emacs-major-version 21) (query-replace old new t) (query-replace old new t start end)))
      (REGEXP
       (if (< emacs-major-version 21)
           (query-replace-regexp old new)
         (query-replace-regexp old new nil start end)))
      (STRING
       (if (< emacs-major-version 21) (query-replace old new) (query-replace old new nil start end))))
    (when (interactive-p) (message "query-replace %s `%s' by `%s'...done" kind old new)))
  (when (and (boundp 'isearchp-set-region-flag) isearchp-set-region-flag)
    (isearchp-set-region-around-search-target))) ; Defined in `isearch+.el'.


;; REPLACES ORIGINAL in `replace.el'.
;; 1. Uses `completing-read' if `replace-w-completion-flag' is non-nil.
;; 2. The default regexps are provided by `search/replace-default-fn'.
;;
(unless (> emacs-major-version 21)
  (defun query-replace-read-args (string regexp-flag &optional noerror)
    "Read arguments for replacement functions such as `\\[query-replace]'.
Option `replace-w-completion-flag', if non-nil, provides for
minibuffer completion while you type the arguments.  In that case, to
insert a `SPC' or `TAB' character, you will need to preceed it by \
`\\[quoted-insert]'."
    (unless noerror (barf-if-buffer-read-only))
    (let* ((default     (if (functionp search/replace-default-fn)
                            (funcall search/replace-default-fn)
                          (car regexp-history)))
           (old-prompt  (concat string ".  OLD (to be replaced): "))
           (oldx        (if replace-w-completion-flag
                            (completing-read old-prompt obarray nil nil nil
                                             query-replace-from-history-variable default t)
                          (if query-replace-interactive
                              (car (if regexp-flag regexp-search-ring search-ring))
                            (read-from-minibuffer old-prompt nil nil nil
                                                  query-replace-from-history-variable default t))))
           (new-prompt  (format "NEW (replacing %s): " oldx))
           (newx        (if replace-w-completion-flag
                            (completing-read new-prompt obarray nil nil nil
                                             query-replace-to-history-variable default t)
                          (read-from-minibuffer new-prompt nil nil nil
                                                query-replace-to-history-variable default t))))
      (list oldx newx current-prefix-arg))))




;; REPLACES ORIGINAL in `replace.el':
;; 1. Prompt changed, to mention that lines after point are affected.
;; 2. The default regexp is provided by `search/replace-default-fn'.
;; 3. An in-progress message has been added.
(when (< emacs-major-version 21)
  (defun keep-lines (regexp)
    "Delete all lines after point except those with a match for REGEXP.
A match split across lines preserves all the lines it lies in.
Note that the lines are deleted, not killed to the kill-ring.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive."
    (interactive
     (list (read-from-minibuffer
            "Keep lines after cursor that contain a match for REGEXP: "
            (if (functionp search/replace-default-fn)
                (funcall search/replace-default-fn)
              (car regexp-history))
            nil nil 'regexp-history nil t)))
    (when (interactive-p) (message "Deleting non-matching lines..."))
    (save-excursion
      (unless (bolp) (forward-line 1))
      (let ((start             (point))
            (case-fold-search  (and case-fold-search (isearch-no-upper-case-p regexp t))))
        (while (not (eobp))
          ;; Start is first char not preserved by previous match.
          (if (not (re-search-forward regexp nil 'move))
              (delete-region start (point-max))
            (let ((end  (save-excursion (goto-char (match-beginning 0)) (line-beginning-position))))
              ;; Now end is first char preserved by the new match.
              (when (< start end) (delete-region start end))))
          (setq start  (save-excursion (forward-line 1) (point)))
          ;; If the match was empty, avoid matching again at same place.
          (and (not (eobp)) (= (match-beginning 0) (match-end 0))
               (forward-char 1)))))
    (when (interactive-p) (message "Deleting non-matching lines...done"))))



;; REPLACES ORIGINAL in `replace.el':
;; 1. Prompt changed, to mention that lines after point are affected.
;; 2. The default regexp is provided by `search/replace-default-fn'.
;; 3. An in-progress message has been added.
(when (< emacs-major-version 21)
  (defun flush-lines (regexp)
    "Delete lines after point that contain a match for REGEXP.
If a match is split across lines, all the lines it lies in are deleted.
Note that the lines are deleted, not killed to the kill-ring.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive."
    (interactive
     (list (read-from-minibuffer
            "Delete lines after cursor that contain a match for REGEXP: "
            (if (functionp search/replace-default-fn)
                (funcall search/replace-default-fn)
              (car regexp-history))
            nil nil 'regexp-history nil t)))
    (when (interactive-p) (message "Deleting matching lines..."))
    (let ((case-fold-search  (and case-fold-search (isearch-no-upper-case-p regexp t))))
      (save-excursion
        (while (and (not (eobp)) (re-search-forward regexp nil t))
          (delete-region (save-excursion (goto-char (match-beginning 0)) (line-beginning-position))
                         (progn (forward-line 1) (point))))))
    (when (interactive-p) (message "Deleting matching lines...done"))))



;; REPLACES ORIGINAL in `replace.el':
;; 1. Prompt changed, to mention that lines after point are affected.
;; 2. The default regexp is provided by `search/replace-default-fn'.
;; 3. An in-progress message has been added.
(when (< emacs-major-version 21)
  (defun how-many (regexp)
    "Print number of matches for REGEXP following point.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive."
    (interactive (list (read-from-minibuffer
                        "Count matches after point for REGEXP: "
                        (if (functonp search/replace-default-fn)
                            (funcall search/replace-default-fn)
                          (car regexp-history)) nil nil 'regexp-history nil t)))
    (when (interactive-p) (message "Counting matches after point..."))
    (let ((count             0)
          (case-fold-search  (and case-fold-search (isearch-no-upper-case-p regexp t)))
          opoint)
      (save-excursion
        (while (and (not (eobp))
                    (progn (setq opoint  (point))
                           (re-search-forward regexp nil t)))
          (if (= opoint (point))
              (forward-char 1)
            (setq count  (1+ count))))
        (message "%d matches after point." count)))))


;;;###autoload
(defalias 'list-matching-lines 'occur)



;; REPLACES ORIGINAL in `replace.el':
;; 1. The default regexp is provided by `search/replace-default-fn'.
;; 2. Regexp is saved as `occur-regexp' for use by `occur-mode-mouse-goto'
;;    and `occur-mode-goto-occurrence'.
(when (< emacs-major-version 21)
  (defun occur (regexp &optional nlines)
    "Show all lines in the current buffer containing a match for REGEXP.

If a match spreads across multiple lines, all those lines are shown.

Each line is displayed with NLINES lines before and after,
or -NLINES before if NLINES is negative.  NLINES defaults to
`list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*Occur*'.  This serves as a
menu to find any of the occurrences in the current buffer.
\\<occur-mode-map>\\[describe-mode] in the `*Occur*' buffer will explain how.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive."
    (interactive
     (list (let ((default  (if (functionp search/replace-default-fn)
                               (funcall search/replace-default-fn)
                             (car regexp-history))))
             (read-from-minibuffer "List lines matching regexp: "  nil nil nil 'regexp-history default t))
           current-prefix-arg))
    (setq occur-regexp  regexp)         ; Save for highlighting.
    (let ((nlines             (if nlines
                                  (prefix-numeric-value nlines)
                                list-matching-lines-default-context-lines))
          (first              t)
          ;;flag to prevent printing separator for first match
          (occur-num-matches  0)
          (buffer             (current-buffer))
          (dir                default-directory)
          (linenum            1)
          (prevpos            (point-min)) ; position of most recent match
          (case-fold-search   (and case-fold-search (isearch-no-upper-case-p regexp t)))
          (final-context-start
           ;; Marker to the start of context immediately following
           ;; the matched text in *Occur*.
           (make-marker)))
;;;     (save-excursion (beginning-of-line)
;;;                     (setq linenum  (1+ (count-lines (point-min) (point)))
;;;                           prevpos  (point)))
      (save-excursion
        (goto-char (point-min))
        ;; Check first whether there are any matches at all.
        (if (not (re-search-forward regexp nil t))
            (message "No matches for `%s'" regexp)
          ;; Back up, so the search loop below will find the first match.
          (goto-char (match-beginning 0))
          (with-output-to-temp-buffer "*Occur*"
            (with-current-buffer standard-output
              (save-excursion
                (setq default-directory  dir)
                ;; We will insert the number of lines, and "lines", later.
                (insert " matching ")
                (let ((print-escape-newlines  t)) (prin1 regexp))
                (insert " in buffer `" (buffer-name buffer) "'." ?\n)
                (occur-mode)
                ;; `occur-buffer', `occur-nlines', and `occur-command-arguments' are free here.
                (setq occur-buffer             buffer
                      occur-nlines             nlines
                      occur-command-arguments  (list regexp nlines))))
            (when (eq buffer standard-output) (goto-char (point-max)))
            (save-excursion
              ;; Find next match, but give up if prev match was at end of buffer.
              (while (and (not (= prevpos (point-max)))
                          (re-search-forward regexp nil t))
                (goto-char (match-beginning 0))
                (beginning-of-line)
                (save-match-data (setq linenum  (+ linenum (count-lines prevpos (point)))))
                (setq prevpos  (point))
                (goto-char (match-end 0))
                (let* ((start
                        ;; start point of text in source buffer to be put into *Occur*
                        (save-excursion (goto-char (match-beginning 0))
                                        (forward-line (if (< nlines 0) nlines (- nlines)))
                                        (point)))
                       (end
                        ;; end point of text in source buffer to be put into *Occur*
                        (save-excursion (goto-char (match-end 0))
                                        (if (> nlines 0)
                                            (forward-line (1+ nlines))
                                          (forward-line 1))
                                        (point)))
                       (match-beg
                        ;; Amount of context before matching text
                        (- (match-beginning 0) start))
                       (match-len
                        ;; Length of matching text
                        (- (match-end 0) (match-beginning 0)))
                       (tag    (format "%5d" linenum))
                       (empty  (make-string (length tag) ?\ ))
                       tem
                       insertion-start
                       ;; Number of lines of context to show for current match.
                       occur-marker
                       ;; Marker pointing to end of match in source buffer.
                       (text-beg
                        ;; Marker pointing to start of text for one
                        ;; match in *Occur*.
                        (make-marker))
                       (text-end
                        ;; Marker pointing to end of text for one match
                        ;; in *Occur*.
                        (make-marker)))
                  (save-excursion
                    (setq occur-marker  (make-marker))
                    (set-marker occur-marker (point))
                    (set-buffer standard-output)
                    (setq occur-num-matches  (1+ occur-num-matches))
                    (or first (zerop nlines) (insert "--------\n"))
                    (setq first  nil)

                    ;; Insert matching text including context lines from
                    ;; source buffer into *Occur*
                    (set-marker text-beg (point))
                    (setq insertion-start  (point))
                    (insert-buffer-substring buffer start end)
                    (or (and (/= (+ start match-beg) end)
                             (with-current-buffer buffer (eq (char-before end) ?\n)))
                        (insert "\n"))
                    (set-marker final-context-start (+ (- (point) (- end (match-end 0)))
                                                       (if (with-current-buffer buffer
                                                             (save-excursion (goto-char (match-end 0))
                                                                             (end-of-line)
                                                                             (bolp)))
                                                           1 0)))
                    (set-marker text-end (point))

                    ;; Highlight text that was matched.
                    (when list-matching-lines-face
                      (put-text-property
                       (+ (marker-position text-beg) match-beg)
                       (+ (marker-position text-beg) match-beg match-len)
                       'face list-matching-lines-face))

                    ;; `occur-point' property is used by occur-next and
                    ;; occur-prev to move between matching lines.
                    (put-text-property
                     (+ (marker-position text-beg) match-beg match-len)
                     (+ (marker-position text-beg) match-beg match-len 1)
                     'occur-point t)

                    ;; Now go back to the start of the matching text
                    ;; adding the space and colon to the start of each line.
                    (goto-char insertion-start)
                    ;; Insert space and colon for lines of context before match.
                    (setq tem  (if (< linenum nlines) (- nlines linenum) nlines))
                    (while (> tem 0)
                      (insert empty ?:)
                      (forward-line 1)
                      (setq tem  (1- tem)))

                    ;; Insert line number and colon for the lines of
                    ;; matching text.
                    (let ((this-linenum  linenum))
                      (while (< (point) final-context-start)
                        (when (null tag)
                          (setq tag  (format "%5d" this-linenum)))
                        (insert tag ?:)
;;;                    ;; DDA: Add mouse-face to line
;;;                    (put-text-property (line-beginning-position) (line-end-position)
;;;                                       'mouse-face 'underline)
;;;                    ;; DDA: Highlight `grep-pattern' in compilation buffer, if possible.
;;;                    (when (fboundp 'hlt-highlight-regexp-region)
;;;                      (hlt-highlight-regexp-region (line-beginning-position) (line-end-position)
;;;                                                   occur-regexp list-matching-lines-face))
                        (forward-line 1)
                        (setq tag  nil)
                        (incf this-linenum))
                      (while (and (not (eobp)) (<= (point) final-context-start))
                        (insert empty ?:)
                        (forward-line 1)
                        (setq this-linenum  (1+ this-linenum))))

                    ;; Insert space and colon for lines of context after match.
                    (while (and (< (point) (point-max)) (< tem nlines))
                      (insert empty ?:)
                      (forward-line 1)
                      (setq tem  (1+ tem)))

                    ;; Add text properties.  The `occur' prop is used to
                    ;; store the marker of the matching text in the
                    ;; source buffer.
                    (put-text-property (marker-position text-beg)
                                       (- (marker-position text-end) 1)
                                       'mouse-face 'underline)
                    (put-text-property (marker-position text-beg)
                                       (marker-position text-end)
                                       'occur occur-marker)
                    (goto-char (point-max)))
                  (forward-line 1)))
              (set-buffer standard-output)
              ;; Go back to top of *Occur* and finish off by printing the
              ;; number of matching lines.
              (goto-char (point-min))
              (let ((message-string  (if (= occur-num-matches 1)
                                         "1 line"
                                       (format "%d lines" occur-num-matches))))
                (insert message-string)
                (when (interactive-p)
                  (message "%s matched" message-string)))
              (setq buffer-read-only  t)))
          (when (fboundp 'show-a-frame-on) ; Defined in `frame-cmds.el'.
            (show-a-frame-on "*Occur*"))
          (let ((fr  (and (fboundp 'get-a-frame) ; Defined in `frame-fns.el'.
                          (get-a-frame "*Occur*"))))
            (when (and fr (fboundp 'fit-frame)) ; Defined in `fit-frame.el'.
              (fit-frame fr))))))))



;; REPLACES ORIGINAL in `replace.el':
;; Regexp is saved as `occur-regexp' for use by `occur-mode-mouse-goto' and `occur-mode-goto-occurrence'.
(when (>= emacs-major-version 21)
  (defadvice occur (before occur-save-regexp activate compile)
    (setq occur-regexp  (ad-get-arg 0)))) ; Save for highlighting.



;; REPLACES ORIGINAL in `replace.el':
;; The default regexp is provided by `search/replace-default-fn'.
(when (>= emacs-major-version 21)
  (defun occur-read-primary-args ()
    (list (let* ((default  (if (functionp search/replace-default-fn)
                               (funcall search/replace-default-fn)
                             (car regexp-history)))
                 (input    (read-from-minibuffer
                            (if default
                                (format "List lines matching regexp (default `%s'): "
                                        (query-replace-descr default))
                              "List lines matching regexp: ")
                            nil nil nil 'regexp-history default)))
            (if (equal input "") default input))
          (and current-prefix-arg (prefix-numeric-value current-prefix-arg)))))



;; REPLACES ORIGINAL in `replace.el':
;; Highlights visited linenum in occur buffer.
;; Highlights regexp in source buffer.
(defadvice occur-mode-mouse-goto (around occur-mode-mouse-goto-highlight activate compile)
  "Highlight visited line number in occur buffer.
Alo highlight occur regexp in source buffer."
  (with-current-buffer (window-buffer (posn-window (event-end (ad-get-arg 0))))
    (save-excursion
      (goto-char (posn-point (event-end (ad-get-arg 0))))
      (when (fboundp 'hlt-highlight-regexp-region) ; Highlight goto lineno.
        (let ((bol  (line-beginning-position)))
          (hlt-highlight-regexp-region bol (save-excursion (beginning-of-line)
                                                           (search-forward ":" (+ bol 20) t)
                                                           (point))
                                       "[0-9]+:" 'occur-highlight-linenum)))))
  ad-do-it
  (when (fboundp 'hlt-highlight-regexp-region)
    (let ((inhibit-field-text-motion  t)) ; Just to be sure, for `line-end-position'.
      (hlt-highlight-regexp-region (line-beginning-position) (line-end-position)
                                   occur-regexp list-matching-lines-face))))



;; REPLACES ORIGINAL in `replace.el':
;; Highlights visited linenum in occur buffer.
;; Highlights regexp in source buffer.
(defadvice occur-mode-goto-occurrence (around occur-mode-goto-occurrence-highlight activate compile)
  "Highlight visited line number in occur buffer.
Also highlight occur regexp in source buffer."
  (when (fboundp 'hlt-highlight-regexp-region) ; Highlight goto lineno.
    (let ((bol  (line-beginning-position)))
      (hlt-highlight-regexp-region bol (save-excursion (beginning-of-line)
                                                       (search-forward ":" (+ bol 20) t)
                                                       (point))
                                   "[0-9]+:" 'occur-highlight-linenum)))
  ad-do-it
  (when (fboundp 'hlt-highlight-regexp-region)
    (let ((inhibit-field-text-motion  t)) ; Just to be sure, for `line-end-position'.
      (hlt-highlight-regexp-region (line-beginning-position) (line-end-position)
                                   occur-regexp list-matching-lines-face))))

;; Bindings for Emacs prior to version 22.
(unless (> emacs-major-version 21)
  (define-key occur-mode-map "o" 'occur-mode-goto-occurrence-other-window)
  (define-key occur-mode-map "\C-o" 'occur-mode-display-occurrence))



;; REPLACES ORIGINAL in `replace.el' (Emacs 22):
;; Highlights visited linenum in occur buffer.
;; Highlights regexp in source buffer.
;;;###autoload
(defun occur-mode-goto-occurrence-other-window ()
  "Go to the occurrence the current line describes, in another window."
  (interactive)
  (when (fboundp 'hlt-highlight-regexp-region) ; Highlight goto lineno.
    (let ((bol  (line-beginning-position)))
      (hlt-highlight-regexp-region bol (save-excursion (beginning-of-line)
                                                       (search-forward ":" (+ bol 20) t)
                                                       (point))
                                   "[0-9]+:" 'occur-highlight-linenum)))
  (let ((pos  (occur-mode-find-occurrence)))
    (switch-to-buffer-other-window (marker-buffer pos))
    (goto-char pos)
    (when (fboundp 'hlt-highlight-regexp-region)
      (let ((inhibit-field-text-motion  t)) ; Just to be sure, for `line-end-position'.
        (hlt-highlight-regexp-region (line-beginning-position) (line-end-position)
                                     occur-regexp list-matching-lines-face)))))



;; REPLACES ORIGINAL in `replace.el' (Emacs 22):
;; Highlights visited linenum in occur buffer.
;; Highlights regexp in source buffer.
;;;###autoload
(defun occur-mode-display-occurrence ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (when (fboundp 'hlt-highlight-regexp-region) ; Highlight goto lineno.
    (let ((bol  (line-beginning-position)))
      (hlt-highlight-regexp-region bol (save-excursion (beginning-of-line)
                                                       (search-forward ":" (+ bol 20) t)
                                                       (point))
                                   "[0-9]+:" 'occur-highlight-linenum)))
  (let ((pos  (occur-mode-find-occurrence))
        window
        ;; Bind these to ensure `display-buffer' puts it in another window.
        same-window-buffer-names
        same-window-regexps)
    (setq window  (display-buffer (marker-buffer pos)))
    ;; This is the way to set point in the proper window.
    (save-selected-window
      (select-window window)
      (goto-char pos)
      (when (fboundp 'hlt-highlight-regexp-region)
        (let ((inhibit-field-text-motion  t)) ; Just to be sure, for `line-end-position'.
          (hlt-highlight-regexp-region (line-beginning-position) (line-end-position)
                                       occur-regexp list-matching-lines-face))))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'replace+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; replace+.el ends here

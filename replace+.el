;;; replace+.el --- Extensions to `replace.el'.
;;
;; Filename: replace+.el
;; Description: Extensions to `replace.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2011, Drew Adams, all rights reserved.
;; Created: Tue Jan 30 15:01:06 1996
;; Version: 21.0
;; Last-Updated: Sat Apr 16 09:50:06 2011 (-0700)
;;           By: dradams
;;     Update #: 1068
;; URL: http://www.emacswiki.org/cgi-bin/wiki/replace+.el
;; Keywords: matching, help, internal, tools, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `avoid', `faces', `faces+', `fit-frame',
;;   `frame-cmds', `frame-fns', `help+20', `highlight', `info',
;;   `info+', `isearch+', `menu-bar', `menu-bar+', `misc-cmds',
;;   `misc-fns', `second-sel', `strings', `thingatpt', `thingatpt+',
;;   `unaccent', `w32browser-dlgopen', `wid-edit', `wid-edit+',
;;   `widget'.
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
;;
;;  ***** NOTE: The following user option defined in `replace.el' has
;;              been REDEFINED HERE:
;;
;;    `list-matching-lines-face'.
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
;;    `query-replace-read-args',  - (Not needed for Emacs 21)
;;                                1. Uses `completing-read' if
;;                                   `replace-w-completion-flag' is
;;                                   non-nil.
;;                                2. Default regexps are obtained via
;;                                   `search/replace-default-fn'.
;;    `query-replace-read-(from|to)' - Same as `query-replace-read-args',
;;                                     but for Emacs 21.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `replace.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "replace" '(progn (require 'replace+)))
;;
;;  Because standard variables such as `list-matching-lines-face' are
;;  predefined, this file overrides the standard definition.  If you
;;  want a different value, you must set it after loading this file.
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
(require 'thingatpt+ nil t) ;; (no error if not found): symbol-name-nearest-point
(require 'frame-cmds nil t) ;; (no error if not found): show-a-frame-on
(require 'frame-fns nil t) ;; (no error if not found): get-a-frame
(require 'fit-frame nil t) ;; (no error if not found): fit-frame
(require 'highlight nil t) ;; (no error if not found): hlt-highlight-regexp-region
(require 'isearch+ nil t) ;; (no error if not found):
                          ;; isearchp-set-region-flag, set-region-around-search-target
(eval-when-compile (require 'menu-bar+ nil t)) ;; menu-bar-make-toggle-any-version
(require 'menu-bar+ nil t) ;; menu-bar-options-menu, menu-bar-search-replace-menu

;;;;;;;;;;;;;;;;;;;;;

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

(defvar search/replace-default-fn
  (if (fboundp 'symbol-name-nearest-point)
      'symbol-name-nearest-point
    'word-at-point)
  "*Fn of 0 args called to provide default input for search/replacement
functions such as \\[query-replace-w-options] and \\[occur].

Some reasonable choices are defined in `thingatpt+.el':
`word-nearest-point', `symbol-name-nearest-point', `sexp-nearest-point'")



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
      (let* ((default   (if (fboundp search/replace-default-fn)
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
    (let* ((default    (if (fboundp search/replace-default-fn)
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

(when (boundp 'menu-bar-options-menu)   ; In `menu-bar+.el'.
  (define-key-after menu-bar-options-menu [replace-w-completion-flag]
    (menu-bar-make-toggle-any-version menu-bar-toggle-replace-w-completion
                                      replace-w-completion-flag
                                      "Completion for Query Replace"
                                      "Using completion with query replace is %s"
                                      "Using completion with query replace")
    'case-fold-search))


;; The main difference between this and `query-replace' is in the treatment of the PREFIX
;; arg.  Only a positive (or nil) PREFIX value gives the same behavior.  A negative PREFIX
;; value does a regexp query replace.  Another difference is that non-nil
;; `isearchp-set-region-flag' means set the region around the last target occurrence.
;;
;; In Emacs 21, this has the same behavior as the versions of `query-replace-read-to' and
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
    (set-region-around-search-target))) ; Defined in `isearch+.el'.


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
    (let* ((default     (if (fboundp search/replace-default-fn)
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
            (if (fboundp search/replace-default-fn)
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
            (let ((end  (save-excursion (goto-char (match-beginning 0)) (beginning-of-line) (point))))
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
            (if (fboundp search/replace-default-fn)
                (funcall search/replace-default-fn)
              (car regexp-history))
            nil nil 'regexp-history nil t)))
    (when (interactive-p) (message "Deleting matching lines..."))
    (let ((case-fold-search  (and case-fold-search (isearch-no-upper-case-p regexp t))))
      (save-excursion
        (while (and (not (eobp)) (re-search-forward regexp nil t))
          (delete-region (save-excursion (goto-char (match-beginning 0)) (beginning-of-line) (point))
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
                        (if (fboundp search/replace-default-fn)
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
     (list (let ((default  (if (fboundp search/replace-default-fn)
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
;;;     (save-excursion
;;;       (beginning-of-line)
;;;       (setq linenum  (1+ (count-lines (point-min) (point))))
;;;       (setq prevpos  (point)))
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
                                                             (save-excursion
                                                               (goto-char (match-end 0))
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
;;;                    (put-text-property (save-excursion
;;;                                         (beginning-of-line) (point))
;;;                                       (save-excursion (end-of-line) (point))
;;;                                       'mouse-face 'underline)
;;;                    ;; DDA: Highlight `grep-pattern' in compilation buffer, if possible.
;;;                    (when (fboundp 'hlt-highlight-regexp-region)
;;;                      (hlt-highlight-regexp-region
;;;                       (save-excursion (beginning-of-line) (point))
;;;                       (save-excursion (end-of-line) (point))
;;;                       occur-regexp list-matching-lines-face))
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
    (list (let* ((default  (if (fboundp search/replace-default-fn)
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
        (let ((bol  (save-excursion (beginning-of-line) (point))))
          (hlt-highlight-regexp-region bol
                                       (save-excursion
                                         (beginning-of-line) (search-forward ":" (+ bol 20) t) (point))
                                       "[0-9]+:"
                                       'occur-highlight-linenum)))))
  ad-do-it
  (when (fboundp 'hlt-highlight-regexp-region)
    (let ((inhibit-field-text-motion  t)) ; Just to be sure, for end-of-line.
      (hlt-highlight-regexp-region (save-excursion (beginning-of-line) (point))
                                   (save-excursion (end-of-line) (point))
                                   occur-regexp list-matching-lines-face))))



;; REPLACES ORIGINAL in `replace.el':
;; Highlights visited linenum in occur buffer.
;; Highlights regexp in source buffer.
(defadvice occur-mode-goto-occurrence (around occur-mode-goto-occurrence-highlight activate compile)
  "Highlight visited line number in occur buffer.
Also highlight occur regexp in source buffer."
  (when (fboundp 'hlt-highlight-regexp-region) ; Highlight goto lineno.
    (let ((bol  (save-excursion (beginning-of-line) (point))))
      (hlt-highlight-regexp-region bol
                                   (save-excursion
                                     (beginning-of-line) (search-forward ":" (+ bol 20) t) (point))
                                   "[0-9]+:"
                                   'occur-highlight-linenum)))
  ad-do-it
  (when (fboundp 'hlt-highlight-regexp-region)
    (let ((inhibit-field-text-motion  t)) ; Just to be sure, for end-of-line.
      (hlt-highlight-regexp-region (save-excursion (beginning-of-line) (point))
                                   (save-excursion (end-of-line) (point))
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
    (let ((bol  (save-excursion (beginning-of-line) (point))))
      (hlt-highlight-regexp-region bol
                                   (save-excursion
                                     (beginning-of-line) (search-forward ":" (+ bol 20) t) (point))
                                   "[0-9]+:"
                                   'occur-highlight-linenum)))
  (let ((pos  (occur-mode-find-occurrence)))
    (switch-to-buffer-other-window (marker-buffer pos))
    (goto-char pos)
    (when (fboundp 'hlt-highlight-regexp-region)
      (let ((inhibit-field-text-motion  t)) ; Just to be sure, for end-of-line.
        (hlt-highlight-regexp-region (save-excursion (beginning-of-line) (point))
                                     (save-excursion (end-of-line) (point))
                                     occur-regexp list-matching-lines-face)))))



;; REPLACES ORIGINAL in `replace.el' (Emacs 22):
;; Highlights visited linenum in occur buffer.
;; Highlights regexp in source buffer.
;;;###autoload
(defun occur-mode-display-occurrence ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (when (fboundp 'hlt-highlight-regexp-region) ; Highlight goto lineno.
    (let ((bol  (save-excursion (beginning-of-line) (point))))
      (hlt-highlight-regexp-region bol
                                   (save-excursion
                                     (beginning-of-line) (search-forward ":" (+ bol 20) t) (point))
                                   "[0-9]+:"
                                   'occur-highlight-linenum)))
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
        (let ((inhibit-field-text-motion  t)) ; Just to be sure, for end-of-line.
          (hlt-highlight-regexp-region (save-excursion (beginning-of-line) (point))
                                       (save-excursion (end-of-line) (point))
                                       occur-regexp list-matching-lines-face))))))




;;;Emacs19 ;; REPLACES ORIGINAL in `replace.el':
;;;Emacs19 ;; When change markers to numbers (after query loop), ensure they are markers.
;;;Emacs19 (defun perform-replace (from-string replacements query-flag regexp-flag
;;;Emacs19                                     delimited-flag &optional repeat-count map)
;;;Emacs19   "Subroutine of `query-replace'.  Its complexity handles interactive queries.
;;;Emacs19 Don't use this in your own program unless you want to query and set the mark
;;;Emacs19 just as `query-replace' does.  Instead, write a simple loop like this:
;;;Emacs19   (while (re-search-forward \"foo[ \t]+bar\" nil t)
;;;Emacs19     (replace-match \"foobar\" nil nil))
;;;Emacs19 which will run faster and probably do what you want."
;;;Emacs19   (unless map (setq map query-replace-map))
;;;Emacs19   (let ((nocasify (not (and case-fold-search case-replace
;;;Emacs19                             (string-equal from-string
;;;Emacs19                                           (downcase from-string)))))
;;;Emacs19         (literal (not regexp-flag))
;;;Emacs19         (search-function (if regexp-flag 're-search-forward 'search-forward))
;;;Emacs19         (search-string from-string)
;;;Emacs19         (real-match-data nil)           ; The match data for the current match.
;;;Emacs19         (next-replacement nil)
;;;Emacs19         (replacement-index 0)
;;;Emacs19         (keep-going t)
;;;Emacs19         (stack nil)
;;;Emacs19         (next-rotate-count 0)
;;;Emacs19         (replace-count 0)
;;;Emacs19         (lastrepl nil)                  ; Position after last match considered.
;;;Emacs19         (match-again t)
;;;Emacs19         (message (and query-flag (substitute-command-keys "Query replacing %s \
;;;Emacs19 with %s: (\\<query-replace-map>\\[help] for help) "))))
;;;Emacs19     (if (stringp replacements)
;;;Emacs19         (setq next-replacement replacements)
;;;Emacs19       (unless repeat-count (setq repeat-count 1)))
;;;Emacs19     (when delimited-flag
;;;Emacs19       (setq search-function 're-search-forward)
;;;Emacs19       (setq search-string (concat "\\b" (if regexp-flag
;;;Emacs19                                             from-string
;;;Emacs19                                           (regexp-quote from-string))
;;;Emacs19                                   "\\b")))
;;;Emacs19     (push-mark)
;;;Emacs19     (undo-boundary)
;;;Emacs19     (unwind-protect
;;;Emacs19         ;; Loop finding occurrences that perhaps should be replaced.
;;;Emacs19         (while (and keep-going
;;;Emacs19                     (not (eobp))
;;;Emacs19                     (funcall search-function search-string nil t)
;;;Emacs19                     ;; If the search string matches immediately after
;;;Emacs19                     ;; the previous match, but it did not match there
;;;Emacs19                     ;; before the replacement was done, ignore the match.
;;;Emacs19                     (or (not (or (eq lastrepl (point))
;;;Emacs19                                  (and regexp-flag
;;;Emacs19                                       (eq lastrepl (match-beginning 0))
;;;Emacs19                                       (not match-again))))
;;;Emacs19                         (and (not (eobp))
;;;Emacs19                              ;; Don't replace the null string
;;;Emacs19                              ;; right after end of previous replacement.
;;;Emacs19                              (progn (forward-char 1)
;;;Emacs19                                     (funcall search-function search-string
;;;Emacs19                                              nil t)))))
;;;Emacs19           ;; Save the data associated with the real match.
;;;Emacs19           (setq real-match-data (match-data))
;;;Emacs19           ;; Before we make the replacement, decide whether the search string
;;;Emacs19           ;; can match again just after this match.
;;;Emacs19           (when regexp-flag (setq match-again (looking-at search-string)))
;;;Emacs19           ;; If time for a change, advance to next replacement string.
;;;Emacs19           (when (and (listp replacements) (= next-rotate-count replace-count))
;;;Emacs19             (incf next-rotate-count repeat-count)
;;;Emacs19             (setq next-replacement (nth replacement-index replacements))
;;;Emacs19             (setq replacement-index (% (1+ replacement-index)
;;;Emacs19                                        (length replacements))))
;;;Emacs19           (if (not query-flag)
;;;Emacs19               (progn (store-match-data real-match-data)
;;;Emacs19                      (replace-match next-replacement nocasify literal)
;;;Emacs19                      (incf replace-count))
;;;Emacs19             (undo-boundary)
;;;Emacs19             (let (done replaced key def)
;;;Emacs19               ;; Loop reading commands until one of them sets DONE,
;;;Emacs19               ;; which means it has finished handling this occurrence.
;;;Emacs19               (while (not done)
;;;Emacs19                 (store-match-data real-match-data)
;;;Emacs19                 (replace-highlight (match-beginning 0) (match-end 0))
;;;Emacs19              ;; Bind message-log-max so we don't fill up the message log
;;;Emacs19              ;; with a bunch of identical messages.
;;;Emacs19              (let ((message-log-max nil))
;;;Emacs19                (message message from-string next-replacement))
;;;Emacs19              (setq key (read-event))
;;;Emacs19              (setq key (vector key))
;;;Emacs19              (setq def (lookup-key map key))
;;;Emacs19              ;; Restore the match data while we process the command.
;;;Emacs19              (cond ((eq def 'help)
;;;Emacs19                     (with-output-to-temp-buffer "*Help*"
;;;Emacs19                       (princ
;;;Emacs19                        (concat "Query replacing "
;;;Emacs19                                (if regexp-flag "regexp " "")
;;;Emacs19                                from-string " by "
;;;Emacs19                                next-replacement ".\n\n"
;;;Emacs19                                (substitute-command-keys
;;;Emacs19                                 query-replace-help)))
;;;Emacs19                       (save-excursion
;;;Emacs19                         (set-buffer standard-output)
;;;Emacs19                         (help-mode))))
;;;Emacs19                    ((eq def 'exit)
;;;Emacs19                     (setq keep-going nil)
;;;Emacs19                     (setq done t))
;;;Emacs19                    ((eq def 'backup)
;;;Emacs19                     (if stack
;;;Emacs19                         (let ((elt (car stack)))
;;;Emacs19                           (goto-char (car elt))
;;;Emacs19                           (setq replaced (eq t (cdr elt)))
;;;Emacs19                           (unless replaced
;;;Emacs19                                (store-match-data (cdr elt)))
;;;Emacs19                           (pop stack))
;;;Emacs19                       (message "No previous match")
;;;Emacs19                       (ding 'no-terminate)
;;;Emacs19                       (sit-for 1)))
;;;Emacs19                    ((eq def 'act)
;;;Emacs19                     (unless replaced
;;;Emacs19                          (replace-match next-replacement nocasify literal))
;;;Emacs19                     (setq done t) (setq replaced t))
;;;Emacs19                    ((eq def 'act-and-exit)
;;;Emacs19                     (unless replaced
;;;Emacs19                         (replace-match next-replacement nocasify literal))
;;;Emacs19                     (setq keep-going nil)
;;;Emacs19                     (setq done t) (setq replaced t))
;;;Emacs19                    ((eq def 'act-and-show)
;;;Emacs19                     (unless replaced
;;;Emacs19                          (replace-match next-replacement nocasify literal)
;;;Emacs19                          (setq replaced t)))
;;;Emacs19                    ((eq def 'automatic)
;;;Emacs19                     (unless replaced
;;;Emacs19                          (replace-match next-replacement nocasify literal))
;;;Emacs19                     (setq done t)
;;;Emacs19                        (setq query-flag nil)
;;;Emacs19                        (setq replaced t))
;;;Emacs19                    ((eq def 'skip)
;;;Emacs19                     (setq done t))
;;;Emacs19                    ((eq def 'recenter)
;;;Emacs19                     (recenter nil))
;;;Emacs19                    ((eq def 'edit)
;;;Emacs19                        (message (substitute-command-keys
;;;Emacs19                                  "Recursive edit.  Type \\[exit-recursive-edit] \
;;;Emacs19 to return to top level."))
;;;Emacs19                     (store-match-data
;;;Emacs19                      (prog1 (match-data)
;;;Emacs19                        (save-excursion (recursive-edit))))
;;;Emacs19                     ;; Before we make the replacement,
;;;Emacs19                     ;; decide whether the search string
;;;Emacs19                     ;; can match again just after this match.
;;;Emacs19                     (when regexp-flag
;;;Emacs19                          (setq match-again (looking-at search-string))))
;;;Emacs19                    ((eq def 'delete-and-edit)
;;;Emacs19                        (message (substitute-command-keys
;;;Emacs19                                  "Recursive edit.  Type \\[exit-recursive-edit] \
;;;Emacs19 to return to top level."))
;;;Emacs19                     (delete-region (match-beginning 0) (match-end 0))
;;;Emacs19                     (store-match-data
;;;Emacs19                      (prog1 (match-data)
;;;Emacs19                        (save-excursion (recursive-edit))))
;;;Emacs19                     (setq replaced t))
;;;Emacs19                    ;; Note: we do not need to treat `exit-prefix'
;;;Emacs19                    ;; specially here, since we reread
;;;Emacs19                    ;; any unrecognized character.
;;;Emacs19                    (t
;;;Emacs19                     (setq this-command 'mode-exited)
;;;Emacs19                     (setq keep-going nil)
;;;Emacs19                     (setq unread-command-events
;;;Emacs19                           (append (listify-key-sequence key)
;;;Emacs19                                   unread-command-events))
;;;Emacs19                     (setq done t))))
;;;Emacs19            ;; Record previous position for ^ when we move on.
;;;Emacs19            ;; Change markers to numbers in the match data
;;;Emacs19            ;; since lots of markers slow down editing.
;;;Emacs19            (push (cons (point)
;;;Emacs19                           (or replaced
;;;Emacs19                               (mapcar (lambda (elt)
;;;Emacs19                                         (and (markerp elt)
;;;Emacs19                                              (prog1 (marker-position elt)
;;;Emacs19                                                (set-marker elt nil))))
;;;Emacs19                                       (match-data))))
;;;Emacs19                     stack)
;;;Emacs19            (when replaced (incf replace-count))))
;;;Emacs19        (setq lastrepl (point)))
;;;Emacs19       (replace-dehighlight))
;;;Emacs19     (or unread-command-events
;;;Emacs19      (message "Replaced %d occurrence%s"
;;;Emacs19               replace-count
;;;Emacs19               (if (= replace-count 1) "" "s")))
;;;Emacs19     (and keep-going stack)))


;;;;;;;;;;;;;;;;;;;;;;;

(provide 'replace+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; replace+.el ends here

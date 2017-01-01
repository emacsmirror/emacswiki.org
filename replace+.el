;;; replace+.el --- Extensions to `replace.el'.
;;
;; Filename: replace+.el
;; Description: Extensions to `replace.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2017, Drew Adams, all rights reserved.
;; Created: Tue Jan 30 15:01:06 1996
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Jan  1 11:21:59 2017 (-0800)
;;           By: dradams
;;     Update #: 1855
;; URL: http://www.emacswiki.org/replace%2b.el
;; Doc URL: http://www.emacswiki.org/ReplacePlus
;; Keywords: matching, help, internal, tools, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `avoid', `cl', `easymenu', `fit-frame',
;;   `frame-cmds', `frame-fns', `help+20', `highlight', `info',
;;   `info+20', `isearch+', `menu-bar', `menu-bar+', `misc-cmds',
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
;;  Commands defined here:
;;
;;    `occur-unhighlight-visited-hits', `query-replace-w-options',
;;    `toggle-replace-w-completion',
;;    `toggle-search/replace-region-as-default'.
;;
;;  Faces defined here:
;;
;;    `occur-highlight-linenum', `replacep-msg-emphasis',
;;    `replacep-msg-emphasis2'.
;;
;;  User options defined here:
;;
;;    `replace-w-completion-flag',
;;    `search/replace-region-as-default-flag',
;;    `search/replace-2nd-sel-as-default-flag',
;;    `search/replace-default-fn'.
;;
;;  Non-interactive functions defined here:
;;
;     `replacep-propertize', `replacep-remove-property',
;     `replacep-string-match-p.', `search/replace-default',
;     `usable-region'.
;;
;;  Internal variable defined here:
;;
;;    `occur-regexp', `occur-searched-buffers'.
;;
;;
;;  ***** NOTE: The following functions defined in `replace.el' have
;;              been REDEFINED or ADVISED HERE:
;;
;;    `flush-lines' - (Not needed for Emacs 21)
;;                    1. The prompt mentions that only lines after
;;                       point are affected.
;;                    2. The default input is provided by
;;                       `search/replace-region-as-default-flag' or
;;                       `search/replace-2nd-sel-as-default-flag' or
;;                       `search/replace-default-fn', in that order.
;;                    3. An in-progress message has been added.
;;    `how-many' - (Not needed for Emacs 21)
;;                 1. Prompt mentions tlines after point are affected.
;;                 2. The default input is provided by
;;                    `search/replace-region-as-default-flag' or
;;                    `search/replace-2nd-sel-as-default-flag' or
;;                    `search/replace-default-fn', in that order.
;;                 3. An in-progress message has been added.
;;    `keep-lines' - Same as `flush-lines'. (Not needed for Emacs 21)
;;    `occur' - Default from `search/replace-region-as-default-flag'
;;              or `search/replace-2nd-sel-as-default-flag'
;;              or `search/replace-default-fn' (Emacs 20 only)
;;    `occur', `multi-occur', `multi-occur-in-matching-buffers' -
;;              Regexp is saved as `occur-regexp' for use by
;;              `occur-mode-mouse-goto'
;;    `occur-engine' - Save list of searched buffers in
;;                     `occur-searched-buffers' (Emacs 22+)
;;    `occur-mode-goto-occurrence', `occur-mode-display-occurrence',
;;    `occur-mode-goto-occurrence-other-window',
;;    `occur-mode-mouse-goto' - Highlight regexp in source buffer
;;                              and visited linenum in occur buffer.
;;    `occur-read-primary-args' - (Emacs 21 only) Default regexps via
;;                                `search/replace-default'.
;;    `query-replace', `query-replace-regexp', `replace-string',
;;      `replace-regexp'        - No " in region" in prompt if
;;                                `*-region-as-default-flag'.
;;    `query-replace-read-args' - 1. Uses `completing-read' if
;;                                   `replace-w-completion-flag' is
;;                                   non-nil.
;;                                2. Default regexps are obtained via
;;                                   `search/replace-default'.
;;                                3. Deactivates region if
;;                                   `*-region-as-default-flag'.
;;    `query-replace-read-(from|to)' - Like `query-replace-read-args',
;;                                     but for Emacs 21+.
;;    `read-regexp' (Emacs 23-24.2) -
;;                        1. Allow DEFAULTS to be a list of strings.
;;                        2. Prepend DEFAULTS to the vanilla defaults.
;;
;;    `replace-highlight' (Emacs 24.4+) - Highlight regexp groups, per
;;                      `isearchp-highlight-regexp-group-levels-flag'.
;;    `replace-dehighlight' (Emacs 24.4+) - Dehighlight regexp groups.
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
;;  If you want the highlighting of regexp matches in the searched
;;  buffers to be removed when you quit occur or multi-occur, then add
;;  function `occur-unhighlight-visited-hits' to an appropripate hook.
;;  For example, to have this happen when you kill the occur buffer,
;;  add it to `kill-buffer-hook':
;;
;;    (add-hook 'kill-buffer-hook 'occur-unhighlight-visited-hits)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2016/12/24 dadams
;;    Support highlighting of regexp groups (option isearchp-highlight-regexp-group-levels-flag):
;;      Added redefinitions of replace-highlight and replace-dehighlight.
;; 2016/12/09 dadams
;;     x-get-selection -> gui-get-selection for Emacs 25+.
;; 2016/05/08 dadams
;;     query-replace-read-from: Use query-replace-compile-replacement, like vanilla.  Thx to Tino Calancha.
;; 2015/07/23 dadams
;;     replace-regexp: Typo: FROM -> REGEXP.  Thx to Tino Calancha. 
;; 2014/04/16 dadams
;;     query-replace-regexp, replace-string, replace-regexp: Got the emacs24.4+ version test backwards.
;; 2014/04/15 dadams
;;     query-replace(-read-args|-w-options|-regexp), replace-(string|regexp):
;;       Update version test for Emacs 24.4 pretest - use version<.
;; 2014/01/30 dadams
;;     query-replace, interactive spec: Swapped return lists - it is Emacs 24.4+ that has 6 args. 
;;     defadvices: Removed extra nil before interactive spec.
;; 2014/01/13 dadams
;;     query-replace-read-(from|to): Define for Emacs 20-21 also.
;; 2014/01/11 dadams
;;     Added: replacep-string-match-p.
;;     query-replace-w-options: Added MSGP arg.  New prefix arg behavior, to allow backward search.
;;     query-replace-read-args, query-replace(-regexp), replace-(string|regexp):
;;       Use replacep-string-match-p, not =.
;; 2014/01/10 dadams
;;     query-replace-read-args, query-replace(-regexp), replace-(string|regexp):
;;       Update for Emacs 24.4 - add new arg for BACKWARD.
;; 2013/12/13 dadams
;;     Added: replacep-msg-emphasis, replacep-msg-emphasis2, replacep-propertize, replacep-remove-property.
;;     toggle-search/replace-region-as-default, query-replace-read-from, query-replace-read-to,
;;       query-replace-w-options, read-regexp, occur-read-primary-args:
;;         Use replacep-propertize on prompt.
;;     query-replace-read-from, query-replace-read-to, read-regexp, occur-read-primary-args:
;;       Use replacep-remove-property on minibuffer-prompt-properties.
;; 2013/10/13 dadams
;;     Corrected handling of evaluable sexp for regexp replacement:
;;      query-replace-read-to: Rewrote, based closer on vanilla.
;;      query-replace-w-options: Use perform-replace for REGEXP handling, like vanilla.
;; 2013/07/24 dadams
;;     query-replace(-w-options|regexp), replace-(string|regexp): Region test includes being nonempty.
;; 2013/04/08 dadams
;;     read-regexp: Not needed for Emacs 24.3 or later.
;; 2013/03/28 dadams
;;     Added redefinition of query-replace-read-args for Emacs 22+.
;;     Added defadvice for query-replace(-regexp), replace-(string|regexp).
;;     query-replace-read-args: deactivate mark when search/replace-region-as-default-flag is non-nil.
;; 2013/02/07 dadams
;;     Added: search/replace-region-as-default-flag, toggle-search/replace-region-as-default'
;;            usable-region.
;;     search/replace-default: Respect search/replace-region-as-default-flag.
;;     Updated doc strings throughout to mention search/replace-region-as-default-flag..
;; 2013/01/29 dadams
;;     search/replace-2nd-sel-as-default-flag: Changed default value to nil.
;; 2012/08/31 dadams
;;     Added: search/replace-2nd-sel-as-default-flag, search/replace-default, redefinition of read-regexp.
;;     query-replace-read-(from|to|read-args), (keep|flush)-lines, how-many, occur(-read-primary-args):
;;       Respect search/replace-2nd-sel-as-default-flag.
;; 2012/08/21 dadams
;;     Call tap-put-thing-at-point-props after load thingatpt+.el.
;; 2012/08/18 dadams
;;     Invoke tap-define-aliases-wo-prefix if thingatpt+.el is loaded.
;; 2012/08/04 dadams
;;     occur-unhighlight-visited-hits: Removed requirement that it be called from occur mode.
;; 2012/08/03 dadams
;;     Added: occur-searched-buffers, occur-unhighlight-visited-hits.
;;     Advised multi-occur(--in-matching-buffers), to highlight source match.
;; 2012/08/02 dadams
;;     occur-read-primary-args, occur-mode-goto-occurrence-other-window, occur-mode-display-occurrence:
;;       Updated for Emacs 24.
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

(eval-when-compile (require 'cl)) ;; incf (plus, for Emacs 20: push)

(require 'thingatpt nil t) ;; (no error if not found): word-at-point

(when (and (require 'thingatpt+ nil t);; (no error if not found)
           (fboundp 'tap-put-thing-at-point-props)) ; >= 2012-08-21
  (tap-define-aliases-wo-prefix)
  (tap-put-thing-at-point-props))
 ;; non-nil-symbol-name-nearest-point

(require 'frame-cmds nil t) ;; (no error if not found): show-a-frame-on
(require 'frame-fns nil t) ;; (no error if not found): get-a-frame
(require 'fit-frame nil t) ;; (no error if not found): fit-frame
(require 'highlight nil t) ;; (no error if not found): hlt-highlight-regexp-region
(require 'isearch+ nil t) ;; (no error if not found):
                          ;; isearchp-highlight-regexp-group-levels-flag, isearchp-regexp-level-overlays,
                          ;; isearchp-set-region-around-search-target, isearchp-set-region-flag
(require 'menu-bar+ nil t) ;; menu-bar-options-menu, menu-bar-search-replace-menu

;; Quiet the byte compiler.
(defvar isearchp-highlight-regexp-group-levels-flag) ; In `isearch+.el' (Emacs 24.4+).
(defvar isearch-lazy-highlight-last-string) ; Emacs 22+.
(defvar isearchp-regexp-level-overlays) ; In `isearch+.el' (Emacs 24.4+).
(defvar lazy-highlight-cleanup)         ; Emacs 22+.
(defvar minibuffer-prompt-properties)   ; Emacs 22+.
(defvar occur-collect-regexp-history)   ; In `replace.el' (Emacs 24+).
(defvar query-replace-defaults)         ; In `replace.el' (Emacs 22+).
(defvar query-replace-lazy-highlight)   ; In `replace.el' (Emacs 22+).
(defvar replace-lax-whitespace)         ; In `replace.el' (Emacs 24.3+).
(defvar replace-regexp-lax-whitespace)  ; In `replace.el' (Emacs 24.3+).

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

(defface replacep-msg-emphasis
    '((((background dark)) (:foreground "#582725633E74")) ; a dark pink
      (t (:foreground "Brown")))
  "*Face used to emphasize (part of) a message."
   :group 'matching :group 'faces)

(defface replacep-msg-emphasis2
    '((((background dark)) (:foreground "DarkGreen"))
      (t (:foreground "Magenta")))
  "*Face used to emphasize (part of) a message."
  :group 'matching :group 'faces)

(defvar occur-regexp nil "Search pattern used by `occur' command.") ; Internal variable.

(defvar occur-searched-buffers ()
  "Source buffers searched by `occur' and `multi-occur'.")

(defcustom replace-w-completion-flag nil
  "*Non-nil means use completion for `query-replace'.
You can complete to any symbol name.  During completion, you can
insert a SPC or TAB char by preceding it with `\\[quoted-insert]'.  If this is
inconvenient, set this option to nil."
  :type 'boolean :group 'matching)

;; Same as `tap-string-match-p' in `thingatpt+.el' and `icicle-string-match-p' in `icicles-fn.el'.
(if (fboundp 'string-match-p)
    (defalias 'replacep-string-match-p 'string-match-p) ; Emacs 23+
  (defun replacep-string-match-p (regexp string &optional start)
    "Like `string-match', but this saves and restores the match data."
    (save-match-data (string-match regexp string start))))

;;;###autoload
(defun toggle-replace-w-completion (force-p)
  "Toggle whether to use minibuffer completion for `query-replace'.
This toggles the value of option `replace-w-completion-flag'.
During completion, you can insert a SPC or TAB char by preceding it
with `\\[quoted-insert]'.

A non-negative prefix arg means set `replace-w-completion-flag' to t.
A negative prefix arg means set it to nil."
  (interactive "P")
  (if force-p                           ; Force.
      (if (natnump (prefix-numeric-value force-p))
          (setq replace-w-completion-flag  t)
        (setq replace-w-completion-flag  nil))
    (setq replace-w-completion-flag  (not replace-w-completion-flag)))) ; Toggle.

(defcustom search/replace-2nd-sel-as-default-flag nil
  "*Non-nil means use secondary selection as default for search/replace.
That is, if there is currently a nonempty secondary selection, use it
as the default input.  All text properties are removed from the text.

This is used only if `search/replace-region-as-default-flag' is nil or
the region is not active."
  :type 'boolean :group 'matching)

(defcustom search/replace-region-as-default-flag nil
  "*Non-nil means use the active region text as default for search/replace.
That is, if the region is currently active then use its text as the
default input.  All text properties are removed from the text.

Note that in this case the active region is not used to limit the
search/replacement scope.  But in that case you can of course just
narrow the buffer temporarily to restrict the operation scope.

A non-nil value of this option takes precedence over the use of option
`search/replace-2nd-sel-as-default-flag'.  To give that option
precedence over using the active region, you can set this option to
nil and use `region-or-non-nil-symbol-name-nearest-point' as the value
of option `search/replace-default-fn'."
  :type 'boolean :group 'matching)

;;;###autoload
(defun toggle-search/replace-region-as-default (msgp)
  "Toggle whether to use the active region text as default.
This toggles the value of option
`search/replace-region-as-default-flag', which affects search and
replace commands."
  (interactive "p")
  (setq search/replace-region-as-default-flag  (not search/replace-region-as-default-flag))
  (when msgp (message "Using active region text as default is now %s"
                      (replacep-propertize (if search/replace-region-as-default-flag "ON" "OFF")
                                           'face 'replacep-msg-emphasis))))



(defcustom search/replace-default-fn (if (fboundp 'non-nil-symbol-name-nearest-point)
                                         'non-nil-symbol-name-nearest-point
                                       'word-at-point)
  "*Function to provide default input for search/replacement functions.
The function is called with no arguments.
This is used by `query-replace' and related commands: `occur',
`how-many', `flush-lines', `keep-lines', etc.

Reminder: Emacs 23 and later can use multiple default values.  The
function you use here can return a single string default value or a
list of such strings.

Some reasonable choices are defined in library `thingatpt+.el':
`word-nearest-point', `non-nil-symbol-name-nearest-point',
`region-or-non-nil-symbol-name-nearest-point', `sexp-nearest-point'.

If you use `region-or-non-nil-symbol-name-nearest-point' for this then
you cannot also take advantage of the use of the active region to
bound the scope of query-replace operations.  But in that case you can
of course just narrow the buffer temporarily to restrict the operation
scope.

See also options `search/replace-region-as-default-flag' and
`search/replace-2nd-sel-as-default-flag'."
  :type '(choice
          (const :tag "No default input search/replacement functions" nil)
          (function :tag "Function of 0 args to provide default for search/replace"))
  :group 'matching)

;; Same as `icicle-propertize', in `icicles-fn.el'.
(defun replacep-propertize (object &rest properties)
  "Like `propertize', but for all Emacs versions.
If OBJECT is not a string, then use `prin1-to-string' to get a string."
  (let ((new  (if (stringp object) (copy-sequence object) (prin1-to-string object))))
    (add-text-properties 0 (length new) properties new)
    new))

;; Same as `icicle-remove-property', in `icicles-fn.el'.
(defun replacep-remove-property (prop plist)
  "Remove property PROP from property-list PLIST, non-destructively.
Returns the modified copy of PLIST."
  (let ((cpy     plist)
        (result  ()))
    (while cpy
      (unless (eq prop (car cpy)) (setq result  `(,(cadr cpy) ,(car cpy) ,@result)))
      (setq cpy  (cddr cpy)))
    (nreverse result)))

(defun usable-region (&optional no-props)
  "Return the region text if the region is active and nonempty.
Non-nil optional arg NO-PROPS means return the text without any
properties."
  (and transient-mark-mode  mark-active  (/= (region-end) (region-beginning))
       (if no-props
           (buffer-substring-no-properties (region-beginning) (region-end))
         (buffer-substring (region-beginning) (region-end)))))

(defun search/replace-default (history)
  "Return a default value or list of such for search & replace functions.
A list of default input strings is computed and returned for Emacs 23
or later.  For Emacs 20-22, only the first such string is returned.
The possible strings are, in order:

* The active region, if option
  `search/replace-region-as-default-flag' is non-nil.

* The secondary selection, if option
  `search/replace-2nd-sel-as-default-flag' is non-nil.

* The result of calling the value of option
  `search/replace-default-fn', if non-nil.

* The first entry in the history list HISTORY."
  (let ((selection   (usable-region t))
        (second-sel  (and  (or (not search/replace-region-as-default-flag)  (not (usable-region t)))
                           search/replace-2nd-sel-as-default-flag
                           (if (fboundp 'gui-get-selection)
                               (gui-get-selection 'SECONDARY) ; Emacs 25.1+.
                             (x-get-selection 'SECONDARY)))))
    (when second-sel (set-text-properties 0 (length second-sel) () second-sel))
    (if (> emacs-major-version 22)
        (delq nil (list selection
                        second-sel
                        (and (functionp search/replace-default-fn) (funcall search/replace-default-fn))
                        (car (symbol-value query-replace-from-history-variable))))
      (or selection
          second-sel
          (and (functionp search/replace-default-fn) (funcall search/replace-default-fn))
          (car history)))))



;; REPLACE ORIGINAL in `replace.el'.
;;
;; 1. Use nonempty active region as default if `search/replace-region-as-default-flag' is non-nil.
;; 2. Else use secondary selection as default input if `search/replace-2nd-sel-as-default-flag' is non-nil.
;; 3. Else provide default input using `search/replace-default-fn'.
;; 4. Use `completing-read' if `replace-w-completion-flag' is non-nil.
;;
;; As with vanilla query-replace, you can also use the history lists,
;; and you can enter nothing to repeat the previous query replacement
;; operation.
;;
(defun query-replace-read-from (string regexp-flag)
  "Query and return the `from' argument of a query-replace operation.
The return value can also be a pair (FROM . TO) indicating that the user
wants to replace FROM with TO.

See options `search/replace-region-as-default-flag',
`search/replace-2nd-sel-as-default-flag', and
`search/replace-default-fn' regarding the default value.

If option `replace-w-completion-flag' is non-nil then you can complete
to a symbol name."
  (if query-replace-interactive
      (car (if regexp-flag regexp-search-ring search-ring))
    (let* ((default                       (search/replace-default
                                           (symbol-value query-replace-from-history-variable)))
           (lastto                        (car (symbol-value query-replace-to-history-variable)))
           (lastfrom                      (car (symbol-value query-replace-from-history-variable)))
           (minibuffer-prompt-properties  (and (boundp 'minibuffer-prompt-properties) ; Emacs 22+.
                                               (replacep-remove-property 'face
                                                                         minibuffer-prompt-properties)))
           (from-prompt
            (if (not (fboundp 'query-replace-descr)) ; Emacs 20-21.
                (concat string ".  OLD (to be replaced): ")
              ;; Use second, not first, if the two history items are the same (e.g. shared lists).
              (when (equal lastfrom lastto)
                (setq lastfrom  (cadr (symbol-value query-replace-from-history-variable))))
              (if (and lastto lastfrom)
                  (format "%s.  OLD (empty means %s -> %s): " string
                          (replacep-propertize (query-replace-descr lastfrom)
                                               'face 'replacep-msg-emphasis)
                          (replacep-propertize (query-replace-descr lastto)
                                               'face 'replacep-msg-emphasis2))
                (concat string ".  OLD: "))))
           ;; The save-excursion here is in case the user marks and copies
           ;; a region in order to specify the minibuffer input.
           ;; That should not clobber the region for the query-replace itself.
           (from                          (save-excursion
                                            (if replace-w-completion-flag
                                                (completing-read
                                                 from-prompt obarray nil nil nil
                                                 query-replace-from-history-variable default t)
                                              (if query-replace-interactive
                                                  (car (if regexp-flag regexp-search-ring search-ring))
                                                (read-from-minibuffer
                                                 from-prompt nil nil nil
                                                 query-replace-from-history-variable default t))))))
      (if (and (zerop (length from)) lastto lastfrom)
          (cons lastfrom
                (if (fboundp 'query-replace-compile-replacement)
                    (query-replace-compile-replacement lastto regexp-flag)
                  lastto))
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
        from))))



;; REPLACE ORIGINAL in `replace.el'.
;;
;; 1. Use nonempty active region as default if `search/replace-region-as-default-flag' is non-nil.
;; 2. Else use secondary selection as default input if `search/replace-2nd-sel-as-default-flag' is non-nil.
;; 3. Else provide default input using `search/replace-default-fn'.
;; 3. Use `completing-read' if `replace-w-completion-flag' is non-nil.
;;
;; As with vanilla query-replace, you can also use the history lists, and you can enter nothing to repeat
;; the previous query replacement operation.
;;
(defun query-replace-read-to (from string regexp-flag)
  "Query and return the `to' argument of a query-replace operation.
See also `query-replace-read-from'."
  (let ((to-replace
         (save-excursion
           (let* ((history-add-new-input         nil)
                  (default                       (search/replace-default
                                                  (symbol-value query-replace-to-history-variable)))
                  (minibuffer-prompt-properties  (and (boundp 'minibuffer-prompt-properties) ; Emacs 22+.
                                                      (replacep-remove-property
                                                       'face minibuffer-prompt-properties)))
                  (to-prompt                     (if (not (fboundp 'query-replace-descr)) ; Emacs 20-21.
                                                     (format "NEW (replacing %s): " from)
                                                   (format "%s.  NEW (replacing %s): "
                                                           string
                                                           (replacep-propertize
                                                            (query-replace-descr from)
                                                            'face 'replacep-msg-emphasis))))
                  ;; The save-excursion here is in case the user marks and copies
                  ;; a region in order to specify the minibuffer input.
                  ;; That should not clobber the region for the query-replace itself.
                  (to                            (save-excursion
                                                   (if replace-w-completion-flag
                                                       (completing-read
                                                        to-prompt obarray nil nil nil
                                                        query-replace-to-history-variable default t)
                                                     (read-from-minibuffer
                                                      to-prompt nil nil nil
                                                      query-replace-to-history-variable default t)))))
             (when (fboundp 'add-to-history) ; Emacs 22+
               (add-to-history query-replace-to-history-variable to nil t))
             (setq query-replace-defaults  (cons from to))
             to))))
    (if (fboundp 'query-replace-compile-replacement)
        (query-replace-compile-replacement to-replace regexp-flag)
      to-replace)))



;; REPLACE ORIGINAL in `replace.el'.
;;
;; 1. Use `completing-read' if `replace-w-completion-flag' is non-nil.
;; 2. Provide default regexps using `search/replace-default'.
;; 3. Deactivate mark if using region text for default.
;;
(unless (> emacs-major-version 21)
  (defun query-replace-read-args (string regexp-flag &optional noerror)
    "Read arguments for replacement functions such as `\\[query-replace]'.
See options `search/replace-region-as-default-flag',
`search/replace-2nd-sel-as-default-flag', and
`search/replace-default-fn' regarding the default value.

Option `replace-w-completion-flag', if non-nil, provides for
minibuffer completion while you type the arguments.  In that case, to
insert a `SPC' or `TAB' character, you will need to precede it by \
`\\[quoted-insert]'."
    (unless noerror (barf-if-buffer-read-only))
    (let* ((default     (search/replace-default regexp-history))
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
      (when (and search/replace-region-as-default-flag  (usable-region t)) (deactivate-mark))
      (list oldx newx current-prefix-arg))))



;; REPLACE ORIGINAL in `replace.el'.
;;
;; Deactivate mark if using region text for default.
;;
(when (> emacs-major-version 21)
  (defun query-replace-read-args (prompt regexp-flag &optional noerror)
    "Read arguments for replacement functions such as `\\[query-replace]'.
See options `search/replace-region-as-default-flag',
`search/replace-2nd-sel-as-default-flag', and
`search/replace-default-fn' regarding the default values.

Option `replace-w-completion-flag', if non-nil, provides for
minibuffer completion while you type the arguments.  In that case, to
insert a `SPC' or `TAB' character, you will need to precede it by \
`\\[quoted-insert]'."
    (unless noerror (barf-if-buffer-read-only))
    (let* ((from  (query-replace-read-from prompt regexp-flag))
           (to    (if (consp from)
                      (prog1 (cdr from) (setq from  (car from)))
                    (query-replace-read-to from prompt regexp-flag))))
      (when (and search/replace-region-as-default-flag  (usable-region t)) (deactivate-mark))
      (if (or (> emacs-major-version 24)  (and (= emacs-major-version 24)
                                               (not (version< emacs-version "24.3.50"))))
          (list from
                to
                (and current-prefix-arg  (not (eq current-prefix-arg '-)))
                (and current-prefix-arg  (eq current-prefix-arg '-)))
        (list from to current-prefix-arg)))))




(when (boundp 'menu-bar-search-replace-menu) ; In `menu-bar+.el'.
  (define-key menu-bar-search-replace-menu [query-replace]
    '(menu-item "Query" query-replace-w-options
      :help "Replace text interactively asking about each occurrence"
      :enable (not buffer-read-only))))

(define-key-after menu-bar-options-menu [replace-w-completion-flag]
  (menu-bar-make-toggle-any-version menu-bar-toggle-replace-w-completion replace-w-completion-flag
                                    "Completion for Query Replace"
                                    "Using completion with query replace is %s"
                                    "Using completion with query replace")
  'case-fold-search)




;; The main difference between this and `query-replace' is in the treatment of the PREFIX
;; arg.  See the doc string for the behavior.  Another difference is that non-nil
;; `isearchp-set-region-flag' means set the region around the last target occurrence.
;;
;; In Emacs 21+, this has the same behavior as the versions of `query-replace-read-to' and
;; `query-replace-read-from' defined here:
;;
;;    1. Uses `completing-read' if `replace-w-completion-flag' is non-nil.
;;    2. Default values are provided by `search/replace-default', which respects options
;;       `search/replace-region-as-default-flag', `search/replace-2nd-sel-as-default-flag', and
;;       `search/replace-default-fn'.
;;
;;    You can still use the history lists, and you can still enter
;;    nothing to repeat the previous query replacement operation.
;;    However, in addition, this provides an initial value by
;;    `search/replace-default'.
;;
;; In Emacs 20, this has the same behavior as the version of `query-replace-read-args'
;; defined here:
;;
;;    1. It uses `completing-read' if `replace-w-completion-flag' is non-nil.
;;    2. The default regexps are provided by `search/replace-default'.
;;
;;;###autoload
(defun query-replace-w-options (old new &optional kind start end msgp)
  "Replace some occurrences of OLD text with NEW text.
This is like `query-replace' or `query-replace-regexp'.  A prefix arg
determines what kind of matches to replace, as follows:

* None:                                literal string, forward
* Plain (`C-u'):                       word,           forward
* `-' (e.g. `C- -'):                   literal string, backward
* Non-negative numeric (e.g. `C- 2'):  regexp,         forward
* Negative numeric (e.g. `C- -2'):     regexp,         backward

See options `search/replace-region-as-default-flag',
`search/replace-2nd-sel-as-default-flag', and
`search/replace-default-fn' regarding default values of OLD and NEW.

Option `replace-w-completion-flag', if non-nil, provides for
minibuffer completion while you type OLD and NEW.  In that case, to
insert a SPC or TAB character, you will need to precede it by \
`\\[quoted-insert]'.

If option `isearchp-set-region-flag' is non-nil, then select the last
replacement."
  (interactive
   (let* ((emacs24.4+   (or (> emacs-major-version 24)
                            (and (= emacs-major-version 24)
                                 (not (version< emacs-version "24.3.50")))))
          (qr-kind      (cond ((consp current-prefix-arg) " WORD") ; `C-u'
                              ((and emacs24.4+  (eq `- current-prefix-arg)) " STRING BACKWARD") ; `-'
                              ((and emacs24.4+
                                    current-prefix-arg
                                    (< (prefix-numeric-value current-prefix-arg) 0))
                               " REGEXP BACKWARD") ; `C-- 2', `C-u -2'
                              (current-prefix-arg " REGEXP") ; `C-2,    `C-u 2'
                              (t " STRING"))) ; no prefix arg
          (prompt       (concat "Query replace" qr-kind))
          (regexp-flag  (replacep-string-match-p "^ REGEXP" qr-kind))
          (from         (query-replace-read-from prompt regexp-flag))
          (to           (if (consp from)
                            (prog1 (cdr from) (setq from  (car from)))
                          (query-replace-read-to from prompt regexp-flag)))
          (backward     (and emacs24.4+  (replacep-string-match-p "BACKWARD$" qr-kind))))
     (when (and search/replace-region-as-default-flag  (usable-region t))
       (deactivate-mark))
     (list from
           to
           qr-kind
           (and transient-mark-mode  mark-active  (> (region-end) (region-beginning))  (region-beginning))
           (and transient-mark-mode  mark-active  (> (region-end) (region-beginning))  (region-end))
           'MSGP)))
  (case (intern kind)
    (\ WORD
     (if (< emacs-major-version 21)
         (query-replace old new t)
       (query-replace old new t start end)))
    (\ REGEXP
     (if (< emacs-major-version 21)
         (query-replace-regexp old new)
       (perform-replace old new t t nil nil nil start end)))
    (\ STRING
     (if (< emacs-major-version 21)
         (query-replace old new)
       (query-replace old new nil start end)))
    (\ REGEXP\ BACKWARD
     (perform-replace old new t t nil nil nil start end 'BACKWARD))
    (\ STRING\ BACKWARD
     (query-replace old new nil start end 'BACKWARD)))
  (when msgp (message "query-replace %s `%s' by `%s'...done"
                      kind
                      (replacep-propertize old 'face 'replacep-msg-emphasis)
                      (replacep-propertize new 'face 'replacep-msg-emphasis)))
  (when (and (boundp 'isearchp-set-region-flag)  isearchp-set-region-flag)
    (isearchp-set-region-around-search-target))) ; Defined in `isearch+.el'.



;; ADVISE ORIGINAL in `replace.el':
;;
;; Do not add " in region" to prompt if `search/replace-region-as-default-flag'.
;;
(when (> emacs-major-version 21)
  (defadvice query-replace (before respect-search/replace-region-as-default-flag activate)
    (interactive
     (let* ((emacs24.4+  (or (> emacs-major-version 24)
                             (and (= emacs-major-version 24)
                                  (not (version< emacs-version "24.3.50")))))

            (common      (query-replace-read-args (concat "Query replace"
                                                          (and current-prefix-arg
                                                               (if (and emacs24.4+
                                                                        (eq current-prefix-arg '-))
                                                                   " backward"
                                                                 " word"))
                                                          (and transient-mark-mode  mark-active
                                                               (> (region-end) (region-beginning))
                                                               (not search/replace-region-as-default-flag)
                                                               " in region"))
                                                  nil))
            (from        (nth 0 common))
            (to          (nth 1 common))
            (delimited   (nth 2 common))
            ;; These are done separately here, so that `command-history' will record these expressions
            ;; rather than the values they had this time.
            (start       (and transient-mark-mode  mark-active  (> (region-end) (region-beginning))
                              (region-beginning)))
            (end         (and transient-mark-mode  mark-active  (> (region-end) (region-beginning))
                              (region-end))))
       (if emacs24.4+
           (list from to delimited start end (nth 3 common))
         (list from to delimited start end))))))



;; ADVISE ORIGINAL in `replace.el':
;;
;; Do not add " in region" to prompt if `search/replace-region-as-default-flag'.
;;
(when (> emacs-major-version 21)
  (defadvice query-replace-regexp (before respect-search/replace-region-as-default-flag activate)
    (interactive
     (let* ((emacs24.4+  (or (> emacs-major-version 24)
                             (and (= emacs-major-version 24)
                                  (not (version< emacs-version "24.3.50")))))
            (common      (query-replace-read-args (concat "Query replace"
                                                          (and current-prefix-arg
                                                               (if (and emacs24.4+
                                                                        (eq current-prefix-arg '-))
                                                                   " backward"
                                                                 " word"))
                                                          " regexp"
                                                          (and transient-mark-mode  mark-active
                                                               (> (region-end) (region-beginning))
                                                               (not search/replace-region-as-default-flag)
                                                               " in region"))
                                                  t))
            (regexp      (nth 0 common))
            (to          (nth 1 common))
            (delimited   (nth 2 common))
            ;; These are done separately here, so that `command-history' will record these expressions
            ;; rather than the values they had this time.
            (start       (and transient-mark-mode  mark-active  (> (region-end) (region-beginning))
                              (region-beginning)))
            (end         (and transient-mark-mode  mark-active  (> (region-end) (region-beginning))
                              (region-end))))
       (if emacs24.4+
           (list regexp to delimited start end (nth 3 common))
         (list regexp to delimited start end))))))



;; ADVISE ORIGINAL in `replace.el':
;;
;; Do not add " in region" to prompt if `search/replace-region-as-default-flag'.
;;
(when (> emacs-major-version 21)
  (defadvice replace-string (before respect-search/replace-region-as-default-flag activate)
    (interactive
     (let* ((emacs24.4+  (or (> emacs-major-version 24)
                             (and (= emacs-major-version 24)
                                  (not (version< emacs-version "24.3.50")))))
            (common      (query-replace-read-args (concat "Replace"
                                                          (and current-prefix-arg
                                                               (if (and emacs24.4+
                                                                        (eq current-prefix-arg '-))
                                                                   " backward"
                                                                 " word"))
                                                          " string"
                                                          (and transient-mark-mode  mark-active
                                                               (> (region-end) (region-beginning))
                                                               (not search/replace-region-as-default-flag)
                                                               " in region"))
                                                  nil))
            (from        (nth 0 common))
            (to          (nth 1 common))
            (delimited   (nth 2 common))
            ;; These are done separately here, so that `command-history' will record these expressions
            ;; rather than the values they had this time.
            (start       (and transient-mark-mode  mark-active  (> (region-end) (region-beginning))
                              (region-beginning)))
            (end         (and transient-mark-mode  mark-active  (> (region-end) (region-beginning))
                              (region-end))))
       (if emacs24.4+
           (list from to delimited start end (nth 3 common))
         (list from to delimited start end))))))



;; ADVISE ORIGINAL in `replace.el':
;;
;; Do not add " in region" to prompt if `search/replace-region-as-default-flag'.
;;
(when (> emacs-major-version 21)
  (defadvice replace-regexp (before respect-search/replace-region-as-default-flag activate)
    (interactive
     (let* ((emacs24.4+  (or (> emacs-major-version 24)
                             (and (= emacs-major-version 24)
                                  (not (version< emacs-version "24.3.50")))))
            (common      (query-replace-read-args (concat "Replace"
                                                          (and current-prefix-arg
                                                               (if (and emacs24.4+
                                                                        (eq current-prefix-arg '-))
                                                                   " backward"
                                                                 " word"))
                                                          " regexp"
                                                          (and transient-mark-mode  mark-active
                                                               (> (region-end) (region-beginning))
                                                               (not search/replace-region-as-default-flag)
                                                               " in region"))
                                                  t))
            (regexp      (nth 0 common))
            (to          (nth 1 common))
            (delimited   (nth 2 common))
            ;; These are done separately here, so that `command-history' will record these expressions
            ;; rather than the values they had this time.
            (start       (and transient-mark-mode  mark-active  (> (region-end) (region-beginning))
                              (region-beginning)))
            (end         (and transient-mark-mode  mark-active  (> (region-end) (region-beginning))
                              (region-end))))
       (if emacs24.4+
           (list regexp to delimited start end (nth 3 common))
         (list regexp to delimited start end))))))



;; REPLACE ORIGINAL in `replace.el':
;;
;; 1. Allow DEFAULTS to be a list of strings.
;; 2. Prepend DEFAULTS to the vanilla defaults.
;;
;; $$$$$$ Not needed for Emacs 24.3+, although perhaps I should provide it, to show a propertized default.
;;
;; $$$$$$ Should we let this return empty input ("") under some conditions?  E.g., if DEFAULTS contains ""?
;;
(when (or (= emacs-major-version 23)  (and (= emacs-major-version 24)  (< emacs-minor-version 3)))
  (defun read-regexp (prompt &optional defaults)
    "Read and return a regular expression as a string.
Prompt with PROMPT, which should not include a final `: '.

Non-nil optional arg DEFAULTS is a string or a list of strings that
are prepended to a list of standard default values, which include the
string at point, the last isearch regexp, the last isearch string, and
the last replacement regexp."
    (when (and defaults  (atom defaults)) (setq defaults  (list defaults)))
    (let* ((deflts                        (append
                                           defaults
                                           (list
                                            (regexp-quote
                                             (or (funcall
                                                  (or find-tag-default-function
                                                      (get major-mode 'find-tag-default-function)
                                                      'find-tag-default))
                                                 ""))
                                            (car regexp-search-ring)
                                            (regexp-quote (or (car search-ring)  ""))
                                            (car (symbol-value query-replace-from-history-variable)))))
           (deflts                        (delete-dups (delq nil (delete "" deflts))))
           (history-add-new-input         nil) ; Do not automatically add INPUT to hist, in case it is "".
           (minibuffer-prompt-properties  (replacep-remove-property
                                           'face minibuffer-prompt-properties))
           (input                         (read-from-minibuffer
                                           (if defaults
                                               (format
                                                "%s (default `%s'): "
                                                prompt
                                                (replacep-propertize
                                                 (mapconcat 'isearch-text-char-description (car deflts) "")
                                                 'face 'replacep-msg-emphasis))
                                             (format "%s: " prompt))
                                           nil nil nil 'regexp-history deflts t)))
      (if (equal input "")
          (or (car defaults)  input)
        (prog1 input (add-to-history 'regexp-history input))))))


(when (boundp 'isearchp-highlight-regexp-group-levels-flag) ; In `isearch+.el', Emacs 24.4+.


  ;; REPLACE ORIGINAL in `replace.el'
  ;;
  ;; Highlight regexp groups also, if `isearchp-highlight-regexp-group-levels-flag' is non-nil.
  ;;
  (defun replace-highlight (match-beg match-end range-beg range-end
                            search-string regexp-flag delimited-flag
                            case-fold-search backward)
    "Highlight current search hit.   Lazy-highlight other search hits.
If option `isearchp-highlight-regexp-group-levels-flag' is non-nil,
then highlight each regexp group differently."
    (when query-replace-highlight
      (if replace-overlay
          (move-overlay replace-overlay match-beg match-end (current-buffer))
        (setq replace-overlay (make-overlay match-beg match-end))
        (overlay-put replace-overlay 'priority 1001) ;higher than lazy overlays
        (overlay-put replace-overlay 'face 'query-replace)))
    (when isearchp-highlight-regexp-group-levels-flag
      (while isearchp-regexp-level-overlays
        (delete-overlay (car isearchp-regexp-level-overlays))
        (setq isearchp-regexp-level-overlays  (cdr isearchp-regexp-level-overlays)))
      ;; Highlight each regexp group differently.
      (save-match-data
        (let ((level         1)
              (max-levels    (min (regexp-opt-depth search-string) 8))
              (rep-priority  (or (overlay-get replace-overlay 'priority) ; `replace-overlay' is 1001.
                                 1001)))
          (save-excursion
            (goto-char match-beg)
            (when (looking-at search-string)
              (condition-case nil
                  (while (<= level max-levels)
                    (unless (equal (match-beginning level) (match-end level))
                      (let ((ov  (make-overlay (match-beginning level) (match-end level))))
                        (push ov isearchp-regexp-level-overlays)
                        (overlay-put ov 'priority (+ rep-priority 200 level))
                        (overlay-put ov 'face (intern (concat "isearchp-regexp-level-"
                                                              (number-to-string level))))))
                    (setq level  (1+ level)))
                (error nil)))))))
    (when query-replace-lazy-highlight
      (let ((isearch-string                 search-string)
            (isearch-regexp                 regexp-flag)
            (isearch-word                   delimited-flag)
            (isearch-lax-whitespace         replace-lax-whitespace)
            (isearch-regexp-lax-whitespace  replace-regexp-lax-whitespace)
            (isearch-case-fold-search       case-fold-search)
            (isearch-forward                (not backward))
            (isearch-other-end              match-beg)
            (isearch-error                  nil))
        (isearch-lazy-highlight-new-loop range-beg range-end))))


  ;; REPLACE ORIGINAL in `replace.el'
  ;;
  ;; Dehighlight regexp groups also, if any.
  ;;
  (defun replace-dehighlight ()
    "Remove search-hit highlighting."
    (when replace-overlay (delete-overlay replace-overlay))
    (when (boundp 'isearchp-regexp-level-overlays)
      (while isearchp-regexp-level-overlays
        (delete-overlay (car isearchp-regexp-level-overlays))
        (setq isearchp-regexp-level-overlays  (cdr isearchp-regexp-level-overlays))))
    (when query-replace-lazy-highlight
      (lazy-highlight-cleanup lazy-highlight-cleanup)
      (setq isearch-lazy-highlight-last-string  nil))
    (isearch-clean-overlays));; Close overlays opened by `isearch-range-invisible' in `perform-replace'.

  )



;; REPLACE ORIGINAL in `replace.el':
;;
;; 1. Prompt mentions that lines after point are affected.
;; 2. Provide default regexp using `search/replace-default'.
;; 3. Add an in-progress message.
;;
(when (< emacs-major-version 21)
  (defun keep-lines (regexp)
    "Delete all lines after point except those with a match for REGEXP.
A match split across lines preserves all the lines it lies in.
Note that the lines are deleted, not killed to the kill-ring.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

See options `search/replace-region-as-default-flag',
`search/replace-2nd-sel-as-default-flag', and
`search/replace-default-fn' regarding the default REGEXP value."
    (interactive
     (list (read-from-minibuffer "Keep lines after cursor that contain a match for REGEXP: "
                                 (search/replace-default regexp-history) nil nil 'regexp-history nil t)))
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



;; REPLACE ORIGINAL in `replace.el':
;;
;; 1. Prompt mentions that lines after point are affected.
;; 2. Provide default regexp using `search/replace-default'.
;; 3. Add an in-progress message.
;;
(when (< emacs-major-version 21)
  (defun flush-lines (regexp)
    "Delete lines after point that contain a match for REGEXP.
If a match is split across lines, all the lines it lies in are deleted.
Note that the lines are deleted, not killed to the kill-ring.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

See options `search/replace-region-as-default-flag',
`search/replace-2nd-sel-as-default-flag', and
`search/replace-default-fn' regarding the default REGEXP value."
    (interactive
     (list (read-from-minibuffer "Delete lines after cursor that contain a match for REGEXP: "
                                 (search/replace-default regexp-history) nil nil 'regexp-history nil t)))
    (when (interactive-p) (message "Deleting matching lines..."))
    (let ((case-fold-search  (and case-fold-search (isearch-no-upper-case-p regexp t))))
      (save-excursion
        (while (and (not (eobp)) (re-search-forward regexp nil t))
          (delete-region (save-excursion (goto-char (match-beginning 0)) (line-beginning-position))
                         (progn (forward-line 1) (point))))))
    (when (interactive-p) (message "Deleting matching lines...done"))))



;; REPLACE ORIGINAL in `replace.el':
;;
;; 1. Prompt mentions that lines after point are affected.
;; 2. Provide default regexp using `search/replace-default'.
;; 3. Add an in-progress message.
;;
(when (< emacs-major-version 21)
  (defun how-many (regexp)
    "Print number of matches for REGEXP following point.
If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

See options `search/replace-region-as-default-flag',
`search/replace-2nd-sel-as-default-flag', and
`search/replace-default-fn' regarding the default REGEXP value."
    (interactive (list (read-from-minibuffer "Count matches after point for REGEXP: "
                                             (search/replace-default regexp-history)
                                             nil nil 'regexp-history nil t)))
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


;; REPLACE ORIGINAL in `replace.el':
;;
;; 1. Provide default regexp using `search/replace-default'.
;; 2. Save regexp as `occur-regexp' for use by `occur-mode-mouse-goto' and `occur-mode-goto-occurrence'.
;;
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
the matching is case-sensitive.

See options `search/replace-region-as-default-flag',
`search/replace-2nd-sel-as-default-flag', and
`search/replace-default-fn' regarding the default REGEXP value."
    (interactive
     (list (let ((default  (search/replace-default regexp-history)))
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



;; REPLACE ORIGINAL in `replace.el':
;;
;; Save regexp as `occur-regexp' for use by `occur-mode-mouse-goto' and `occur-mode-goto-occurrence'.
;;
(when (> emacs-major-version 20)
  (defadvice occur (before occur-save-regexp activate compile)
    (setq occur-regexp  (ad-get-arg 0)))) ; Save for highlighting.



;; REPLACE ORIGINAL in `replace.el':
;;
;; Save regexp as `occur-regexp' for use by `occur-mode-mouse-goto' and `occur-mode-goto-occurrence'.
;;
(when (> emacs-major-version 20)
  (defadvice multi-occur (before multi-occur-save-regexp activate compile)
    (setq occur-regexp  (ad-get-arg 1)))) ; Save for highlighting.



;; REPLACE ORIGINAL in `replace.el':
;;
;; Save regexp as `occur-regexp' for use by `occur-mode-mouse-goto' and `occur-mode-goto-occurrence'.
;;
(when (> emacs-major-version 20)
  (defadvice multi-occur-in-matching-buffers (before multi-occur-in-matching-buffers-save-regexp
                                                     activate compile)
    (setq occur-regexp  (ad-get-arg 1)))) ; Save for highlighting.



;; REPLACE ORIGINAL in `replace.el':
;;
;; Provide default input using `search/replace-default'.
;;
(when (> emacs-major-version 20)
  (defun occur-read-primary-args ()
    "Read arguments for `occur'.
See options `search/replace-region-as-default-flag',
`search/replace-2nd-sel-as-default-flag', and
`search/replace-default-fn' regarding the default regexp value."
    (let* ((perform-collect  (and (> emacs-major-version 23)  (consp current-prefix-arg)))
           (default          (search/replace-default regexp-history))
           (regexp           (if (fboundp 'read-regexp) ; Emacs 23+
                                 (read-regexp (if perform-collect
                                                  "Collect strings matching regexp"
                                                "List lines matching regexp")
                                              default)
                               (when (consp default) (setq default  (car default)))
                               (read-from-minibuffer ; Emacs 20-22
                                (if default
                                    (format "List lines matching regexp (default `%s'): "
                                            (query-replace-descr default))
                                  "List lines matching regexp: ")
                                nil nil nil 'regexp-history default))))
      (when (equal regexp "") (setq regexp  default))
      (list regexp
            (if perform-collect
                (if (zerop (regexp-opt-depth regexp)) ; Perform collect operation
                    "\\&"               ; No subexpression, so collect entire match.
                  (let ((coll-def  (car occur-collect-regexp-history)) ; Regexp for collection pattern.
                        (minibuffer-prompt-properties  (replacep-remove-property
                                                        'face minibuffer-prompt-properties)))
                    (read-string (format "Regexp to collect (default %s): "
                                         (replacep-propertize coll-def 'face 'replacep-msg-emphasis))
                                 nil 'occur-collect-regexp-history coll-def)))
              ;; Otherwise normal occur takes numerical prefix argument.
              (and current-prefix-arg  (prefix-numeric-value current-prefix-arg)))))))



;; REPLACE ORIGINAL in `replace.el':
;;
;; Highlight visited linenum in occur buffer.
;; Highlight regexp in source buffer.
;;
(defadvice occur-mode-mouse-goto (around occur-mode-mouse-goto-highlight activate compile)
  "Go to the occurrence for the clicked line.
Highlight visited line number in the occur buffer.
Highlight the occur regexp in the source buffer."
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



;; REPLACE ORIGINAL in `replace.el':
;;
;; Highlight visited linenum in occur buffer.
;; Highlight regexp in source buffer.
;;
(defadvice occur-mode-goto-occurrence (around occur-mode-goto-occurrence-highlight activate compile)
  "Go to the occurrence for the current line.
Highlight visited line number in the occur buffer.
Highlight the occur regexp in the source buffer."
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



;; REPLACE ORIGINAL in `replace.el' (Emacs 22):
;;
;; Highlight visited linenum in occur buffer.
;; Highlight regexp in source buffer.
;;
;;;###autoload
(defun occur-mode-goto-occurrence-other-window ()
  "Go to the occurrence for the current line, in another window.
Highlight the visited line number in the occur buffer.
Highlight the occur regexp in the source buffer."
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
    (when (boundp 'occur-mode-find-occurrence-hook) (run-hooks 'occur-mode-find-occurrence-hook))
    (when (fboundp 'hlt-highlight-regexp-region)
      (let ((inhibit-field-text-motion  t)) ; Just to be sure, for `line-end-position'.
        (hlt-highlight-regexp-region (line-beginning-position) (line-end-position)
                                     occur-regexp list-matching-lines-face)))))



;; REPLACE ORIGINAL in `replace.el' (Emacs 22):
;;
;; Highlight visited linenum in occur buffer.
;; Highlight regexp in source buffer.
;;
;;;###autoload
(defun occur-mode-display-occurrence ()
  "Display in another window the occurrence for the current line.
Highlight the visited line number in the occur buffer.
Highlight the occur regexp in the source buffer."
  (interactive)
  (when (fboundp 'hlt-highlight-regexp-region) ; Highlight goto lineno.
    (let ((bol  (line-beginning-position)))
      (hlt-highlight-regexp-region bol (save-excursion (beginning-of-line)
                                                       (search-forward ":" (+ bol 20) t)
                                                       (point))
                                   "[0-9]+:" 'occur-highlight-linenum)))
  (let* ((pos     (occur-mode-find-occurrence))
         (window  (display-buffer (marker-buffer pos) t)))
    (save-selected-window            ; Set point in the proper window.
      (select-window window)
      (goto-char pos)
      (when (fboundp 'hlt-highlight-regexp-region)
        (let ((inhibit-field-text-motion  t)) ; Just to be sure, for `line-end-position'.
          (hlt-highlight-regexp-region (line-beginning-position) (line-end-position)
                                       occur-regexp list-matching-lines-face))))))



;; REPLACE ORIGINAL in `replace.el' (Emacs 22):
;;
;; Save list of searched buffers in `occur-searched-buffers'.
;;
(defadvice occur-engine (after record-buffers activate)
  (setq occur-searched-buffers  (ad-get-arg 1)))

(defun occur-unhighlight-visited-hits (&optional msgp)
  "Unhighlight visited search hits for the latest occur-mode buffer.
Non-interactively, this is a no-op without library `highlight.el'."
  (interactive "p")
  (when (and msgp  (not (fboundp 'hlt-unhighlight-region)))
    (error "`occur-unhighlight-visited-hits' requires library `highlight.el'"))
  (when (and (fboundp 'hlt-unhighlight-region)  (listp occur-searched-buffers))
    (dolist (buf  occur-searched-buffers)  (with-current-buffer buf (hlt-unhighlight-region)))
    (setq occur-searched-buffers  ())))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'replace+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; replace+.el ends here

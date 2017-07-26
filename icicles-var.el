;;; icicles-var.el --- Internal variables for Icicles
;;
;; Filename: icicles-var.el
;; Description: Internal variables for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2017, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:23:26 2006
;; Last-Updated: Wed Jul 26 08:21:37 2017 (-0700)
;;           By: dradams
;;     Update #: 1881
;; URL: https://www.emacswiki.org/emacs/download/icicles-var.el
;; Doc URL: https://www.emacswiki.org/emacs/Icicles
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `apropos-fn+var', `avoid', `bookmark',
;;   `bookmark+', `bookmark+-1', `bookmark+-bmu', `bookmark+-key',
;;   `bookmark+-lit', `cl', `cus-theme', `el-swank-fuzzy', `ffap',
;;   `ffap-', `fit-frame', `frame-fns', `fuzzy', `fuzzy-match',
;;   `help+20', `hexrgb', `icicles-opt', `info', `info+20', `kmacro',
;;   `levenshtein', `menu-bar', `menu-bar+', `misc-cmds', `misc-fns',
;;   `naked', `package', `pp', `pp+', `regexp-opt', `second-sel',
;;   `strings', `thingatpt', `thingatpt+', `unaccent',
;;   `w32browser-dlgopen', `wid-edit', `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  internal variables (not to be modified by users.  For Icicles
;;  documentation, see `icicles-doc1.el' and `icicles-doc2.el'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-abs-file-candidates', `icicle-acting-on-next/prev',
;;    `icicle-advice-info-list', `icicle-all-candidates-action',
;;    `icicle-all-candidates-list-action-fn',
;;    `icicle-all-candidates-list-alt-action-fn',
;;    `icicle-allowed-sort-predicate', `icicle-apply-nomsg',
;;    `icicle-apropos-complete-match-fn',
;;    `icicle-apropos-value-last-initial-cand-set',
;;    `icicle-auto-complete-key-idle-timer' (Emacs 22+),
;;    `icicle-auto-no-icomplete-mode-p', `icicle-auto-no-sort-p',
;;    `icicle-bookmark-history', `icicle-bookmark-list-names-only-p',
;;    `icicle-bookmark-types', `icicle-buffer-complete-fn',
;;    `icicle-buffer-config-history', `icicle-buffer-name-input-p',
;;    `icicle-buffer-sort-first-time-p', `icicle-bufflist',
;;    `icicle-candidate-action-fn', `icicle-candidate-alt-action-fn',
;;    `icicle-candidate-entry-fn', `icicle-candidate-help-fn',
;;    `icicle-candidate-nb', `icicle-candidate-properties-alist',
;;    `icicle-candidates-alist', `icicle-cands-to-narrow',
;;    `icicle-char-property-value-history',
;;    `icicle-cmd-calling-for-completion', `icicle-cmd-reading-input',
;;    `icicle-color-history', `icicle-color-theme-history',
;;    `icicle-command-abbrev-history', `icicle-commands-for-abbrev',
;;    `icicle-common-match-string',
;;    `icicle-comp-base-is-default-dir-p',
;;    `icicle-complete-input-overlay', `icicle-complete-keys-alist',
;;    `icicle-completing-keys-p', `icicle-completing-p',
;;    `icicle-completing-read+insert-candidates',
;;    `icicle-completion-candidates', `icicle-completion-map-vars',
;;    `icicle-completion-prompt-overlay',
;;    `icicle-completion-set-history', `icicle-completion-style-set',
;;    `icicle-compute-narrowing-regexp-p',
;;    `icicle-confirm-exit-commands',
;;    `icicle-current-completion-candidate-overlay',
;;    `icicle-current-completion-mode',
;;    `icicle-current-font-lock-part', `icicle-current-input',
;;    `icicle-current-raw-input', `icicle-cycling-p',
;;    `icicle-default-thing-insertion-flipped-p',
;;    `icicle-dictionary-history', `icicle-dir-candidate-can-exit-p',
;;    `icicle-doc-last-initial-cand-set',
;;    `icicle-dot-string-internal', `icicle-edit-update-p',
;;    `icicle-ess-use-ido', `icicle-exclude-default-proxies',
;;    `icicle-explore-final-choice',
;;    `icicle-explore-final-choice-full', `icicle-extra-candidates',
;;    `icicle-extra-candidates-dir-insert-p',
;;    `icicle-face-name-history', `icicle-face-remapping-Completions',
;;    `icicle-face-remapping-region', `icicle-fancy-candidates-p',
;;    `icicle-fancy-cands-internal-p',
;;    `icicle-file-name-completion-table' (Emacs 24+),
;;    `icicle-file-sort-first-time-p',
;;    `icicle-filtered-default-value', `icicle-font-name-history',
;;    `icicle-frame-alist', `icicle-frame-name-history',
;;    `icicle-full-cand-fn', `icicle-function-name-history',
;;    `icicle-fundoc-last-initial-cand-set',
;;    `icicle-general-help-string',
;;    `icicle-get-alist-candidate-function',
;;    `icicle-hist-cands-no-highlight', `icicle-hist-var',
;;    `icicle-ignored-extensions', `icicle-ignored-extensions-regexp',
;;    `icicle-incremental-completion-p', `icicle-Info-index-cache',
;;    `icicle-Info-only-rest-of-book-p', `icicle-Info-tag-table-posn',
;;    `icicle-inhibit-sort-p', `icicle-inhibit-try-switch-buffer',
;;    `icicle-initial-value', `icicle-input-completion-fail-overlay',
;;    `icicle-input-fail-pos', `icicle-insert-string-at-pt-end',
;;    `icicle-insert-string-at-pt-start',
;;    `icicle-interactive-history', `icicle-izones-var',
;;    `icicle-key-prefix-description', `icicle-kill-history',
;;    `icicle-kmacro-alist' (Emacs 22+), `icicle-kmacro-history',
;;    `icicle-last-apropos-complete-match-fn',
;;    `icicle-last-completion-candidate',
;;    `icicle-last-completion-command',
;;    `icicle-last-icomplete-mode-value', `icicle-last-input',
;;    `icicle-last-sort-comparer', `icicle-last-top-level-command',
;;    `icicle-last-transform-function', `icicle-lighter-truncation',
;;    `icicle-list-use-nth-parts', `icicle-menu-map',
;;    `icicle-minibuffer-message-ok-p', `icicle-minor-mode-map-entry',
;;    `icicle-mode-line-help', `icicle-ms-windows-drive-hash',
;;    `icicle-multi-completing-p', `icicle-multi-inputs-action-fn',
;;    `icicle-must-match-regexp', `icicle-must-not-match-regexp',
;;    `icicle-must-pass-after-match-predicate',
;;    `icicle-must-pass-predicate', `icicle-narrow-regexp',
;;    `icicle-nb-candidates-before-truncation',
;;    `icicle-nb-of-other-cycle-candidates', `icicle-new-last-cmd',
;;    `icicle-next-apropos-complete-cycles-p',
;;    `icicle-next-prefix-complete-cycles-p',
;;    `icicle-next-window-for-display-buffer', `icicle-orig-buff',
;;    `icicle-orig-font-lock-keywords',
;;    `icicle-orig-minibuffer-completion-pred',
;;    `icicle-orig-minibuffer-completion-table',
;;    `icicle-orig-must-match-regexp',
;;    `icicle-orig-must-not-match-regexp',
;;    `icicle-orig-must-pass-after-match-pred',
;;    `icicle-orig-must-pass-predicate', `icicle-orig-pt-explore',
;;    `icicle-orig-read-file-name-fn', `icicle-orig-window',
;;    `icicle-orig-win-explore', `icicle-other-window',
;;    `icicle-path-variables', `icicle-plist-last-initial-cand-set',
;;    `icicle-predicate-types-alist', `icicle-pref-arg',
;;    `icicle-pre-minibuffer-buffer', `icicle-post-command-hook',
;;    `icicle-pre-command-hook',
;;    `icicle-previous-raw-file-name-inputs',
;;    `icicle-previous-raw-non-file-name-inputs',
;;    `icicle-progressive-completing-p', `icicle-prompt',
;;    `icicle-proxy-candidate-regexp', `icicle-proxy-candidates',
;;    `icicle-read-char-history' (Emacs 23+),
;;    `icicle-read-expression-map', `icicle-remove-icicles-props-p',
;;    `icicle-re-no-dot', `icicle-require-match-p',
;;    `icicle-reverse-multi-sort-p', `icicle-reverse-sort-p',
;;    `icicle-saved-candidate-overlays',
;;    `icicle-saved-candidates-variables-obarray',
;;    `icicle-saved-completion-candidate',
;;    `icicle-saved-completion-candidates',
;;    `icicle-saved-completion-candidates-internal',
;;    `icicle-saved-ignored-extensions',
;;    `icicle-saved-kmacro-ring-max' (Emacs 22+),
;;    `icicle-saved-proxy-candidates',
;;    `icicle-saved-regexp-search-ring-max',
;;    `icicle-saved-region-background',
;;    `icicle-saved-search-ring-max', `icicle-scan-fn-or-regexp',
;;    `icicle-scroll-Completions-reverse-p', `icicle-search-command',
;;    `icicle-search-complement-domain-p',
;;    `icicle-search-context-level', `icicle-search-context-regexp',
;;    `icicle-search-current-overlay', `icicle-search-final-choice',
;;    `icicle-search-history', `icicle-search-in-context-fn',
;;    `icicle-searching-p', `icicle-search-level-overlays',
;;    `icicle-search-map', `icicle-search-modes',
;;    `icicle-search-overlays', `icicle-search-refined-overlays',
;;    `icicle-search-replacement',
;;    `icicle-search-replacement-history',
;;    `icicle-successive-grab-count',
;;    `icicle-text-property-value-history',
;;    `icicle-thing-at-pt-fns-pointer', `icicle-toggle-map',
;;    `icicle-toggle-transforming-message',
;;    `icicle-transform-before-sort-p', `icicle-transform-function',
;;    `icicle-universal-argument-map',
;;    `icicle-use-candidates-only-once-alt-p',
;;    `icicle-vardoc-last-initial-cand-set',
;;    `icicle-variable-name-history',
;;    `icicle-whole-candidate-as-text-prop-p',
;;    `lacarte-menu-items-alist'.
;;
;;  For descriptions of changes to this file, see `icicles-chg.el'.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  https://www.emacswiki.org/emacs/download/linkd.el.
;;
;;  (@> "Internal variables (alphabetical)")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'apropos-fn+var nil t) ;; (no error if not found): apropos-command,
                                ;; apropos-function, apropos-option, apropos-variable

(require 'icicles-opt) ;; icicle-kbd, icicle-sort-comparer

;;; Defvars to quiet byte-compiler:
(defvar kmacro-ring-max)                ; Defined in `kmacro.el' in Emacs 22+.
(defvar minibuffer-confirm-exit-commands) ; Defined in `minibuffer.el' in Emacs 23+.
(defvar minibuffer-local-filename-completion-map)
(defvar minibuffer-local-filename-must-match-map)
(defvar minibuffer-local-must-match-filename-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Internal variables (alphabetical)")

;;; Internal variables (alphabetical) --------------------------------

(defvar lacarte-menu-items-alist nil)    ; Defined in `lacarte.el'.

;; These two are defined here so they won't raise an error in `font-lock-add-keywords'.
(defvar font-lock-function-name-face 'font-lock-function-name-face ; In `font-lock.el'.
  "Face name to use for function names.")

(defvar font-lock-keyword-face 'font-lock-keyword-face ; Defined in `font-lock.el'.
  "Face name to use for keywords.")

(defvar icicle-abs-file-candidates ()
  "Current alist of absolute file-name candidates.
An alist appropriate as the COLLECTION argument for `completing-read'.
Each item is a cons whose car is an absolute file name (a string).")

(defvar icicle-acting-on-next/prev nil
  "Non-nil means this command acts on the previous or next candidate.
The particular non-nil value indicates the navigation direction:
`forward' or `backward'.")

(defvar icicle-advice-info-list ()
  "List of advice information for functions that Icicles redefines.
If such redefined functions are advised, then Icicles deactivates the
advice when you turn on Icicle mode.  It restores the recorded advice
activation state when you turn off Icicle mode.")

(defvar icicle-all-candidates-action nil
  "Non-nil means that we are acting on all candidates.
That is, `icicle-all-candidates-action-1' is in progress.
If neither nil nor t, then the value is the action function to use.")

(defvar icicle-all-candidates-list-action-fn nil
  "Action function to apply to a list of all matching completions.
It is applied using `M-!' during completion.")

(defvar icicle-all-candidates-list-alt-action-fn nil
  "Alternative action function to apply to a list of matching completions.
It is applied using `M-|' during completion.")

(defvar icicle-allowed-sort-predicate nil
  "Predicate that `icicle-current-sort-functions' allows in a sort order.
That is, if this predicate is used in an entry of
`icicle-sort-orders-alist' then that entry is retained by
`icicle-current-sort-functions'.")

(defvar icicle-apply-nomsg nil
  "Non-nil means `icicle-apply' does not show status of applying function.")

(defvar icicle-apropos-complete-match-fn 'string-match
  "Function to filter apropos completion candidates.
Takes as arguments an input string and a completion candidate, and
returns non-nil if the string is considered to match the candidate.

A value of nil instead of a function means completion candidates are
not filtered by the input, except possibly by a function-valued
`minibuffer-completion-table'.")

(defvar icicle-apropos-value-last-initial-cand-set ()
  "Cache for initial set of completion cands for `icicle-apropos-value'.")

(when (> emacs-major-version 21)
  (defvar icicle-auto-complete-key-idle-timer nil
    "Timer used to automatically complete a key sequence when Emacs is idle."))

(defvar icicle-auto-no-icomplete-mode-p nil
  "Non-nil means Icomplete mode has been turned off for this minibuffer activation.
This automatic turning off happens when there are at least
`icicle-icomplete-mode-max-candidates' completion candidates.")

(defvar icicle-auto-no-sort-p nil
  "Non-nil means sorting has been turned off for this minibuffer activation.
This automatic turning off happens when there are at least
`icicle-sorting-max-candidates' completion candidates.")

(defvar icicle-bookmark-history nil "History for bookmark names.")

(defvar icicle-bookmark-list-names-only-p nil
  "Non-nil means `icicle-bookmark-list' returns names, not bookmarks.")

(defvar icicle-bookmark-types ()
  "List of strings naming bookmark types.
The list represents the set of all bookmarks of the given types.
An empty list and the singleton list `(all)', where `all' is a symbol,
are equivalent and stand for the set of all bookmarks (of any type).")

(defvar icicle-buffer-complete-fn nil
  "If the value is non-nil then it is a buffer-name completion function.
The function is used as the COLLECTION argument to `completing-read'.

However, if the value is `internal-complete-buffer' then it is used
only if `icicle-buffer-ignore-space-prefix-flag' is non-nil.

Otherwise, all buffer names are used as candidates.")

(defvar icicle-buffer-config-history nil "History for buffer configuration names.")

(defvar icicle-buffer-name-input-p nil
  "Non-nil means we are reading a buffer name.")

(defvar icicle-buffer-sort-first-time-p t
  "Non-nil means buffer-name completion has not yet been used.")

(defvar icicle-bufflist nil
  "List of buffers defined by macro `icicle-buffer-bindings'.")

(defvar icicle-candidate-action-fn nil
  "Action function to apply to current completion candidate.
It is applied by `C-RET', `C-mouse-2', and similar keys during
completion.
For `icicle-all-candidates-action' to be able to report successes,
this should return nil for \"success\" and non-nil for \"failure\".")

(defvar icicle-candidate-alt-action-fn nil
  "Alternative action function to apply to current completion candidate.
It is applied by `C-S-RET', `C-S-mouse-2', and similar keys during
completion.
For `icicle-all-candidates-alt-action' to be able to report successes,
this should return nil for \"success\" and non-nil for \"failure\".")

(defvar icicle-candidate-entry-fn nil
  "Function to apply to selected entries in `icicle-candidates-alist'.")

(defvar icicle-candidate-help-fn nil
  "Help function to be applied to current completion candidate.
It is applied by `C-M-RET', `C-M-mouse-2', and similar keys during
completion.
If nil then default help function `icicle-help-on-candidate' is used.
If non-nil, it must be a function that accepts a completion candidate
in its display form.  If the candidate is a multi-completion then the
help function can invoke `icicle-transform-multi-completion' on it,
binding `icicle-list-use-nth-parts' as appropriate.")

(defvar icicle-candidate-nb nil
  "Current completion candidate number, or nil if not cycling candidates.
Numbering starts at zero.")

(defvar icicle-candidate-properties-alist nil
  "Alist of multi-completion indexes and associated text properties.
The text properties apply to candidates in `*Completions*'.
Each alist entry has the form (NTH PROPERTIES) or (NTH PROPERTIES
JOIN-TOO).

NTH is a whole-number index identifying the multi-completion part.

PROPERTIES is a list of text properties to apply to the part.

JOIN-TOO non-nil means to also apply PROPERTIES to the join string
that follows the part.

Example alist:

 ((3 (face 'underline))
  (2 (invisible t) t))

The first entry underlines the third multi-completion part.
The second entry makes both the second part and the join string that
follows it invisible.")

(defvar icicle-candidates-alist nil
  "Alist of candidate entries.
The car (key) of each entry is treated as a completion candidate.
The cdr is some other data to be used when the candidate is chosen.
This is reset to nil at the beginning of each top-level command.

This is used typically by commands that allow different cdrs for the
same car.  Icicles search is one such example.")

(defvar icicle-cands-to-narrow ()
  "Saved `icicle-completion-candidates' for reference during narrowing.")

(defvar icicle-char-property-value-history nil "History for text and overlay property values.")

(defvar icicle-cmd-calling-for-completion 'ignore
  "Last command causing display of list of possible completions.")

(defvar icicle-cmd-reading-input 'ignore
  "Last command reading input in the minibuffer.")

(defvar icicle-color-history nil "History for color names.")

(defvar icicle-color-theme-history nil "History for color-theme names.")

(defvar icicle-command-abbrev-history nil "History of command and abbrev entries.")

(defvar icicle-commands-for-abbrev nil
  "List of commands that match the current abbreviation.")

(defvar icicle-common-match-string nil
  "Expanded common match among all completion candidates.
nil means no such common match is available.")

(defvar icicle-comp-base-is-default-dir-p nil
  "Non-nil means to use `default-directory' as the completion base.
This means use its length as `completion-base-size'.")

(defvar icicle-complete-input-overlay nil
  "Overlay used to highlight minibuffer input when it is complete.")

(defvar icicle-complete-keys-alist () "Alist of keys and their bindings.
Each alist element is of the form (NAME KEY . BINDING), where:
 NAME is a symbol naming the key and its binding, whose name has form:
   KEYNAME  =  BINDING-NAME
 KEY is the actual key sequence
 BINDING is the actual binding of KEY.

\(The separator between KEY and BINDING-NAME is the value of option
`icicle-complete-keys-separator'.  Its default value is \"  =  \".)

Used only for Emacs 22 and later.")

(defvar icicle-completing-keys-p nil
  "Non-nil means completion is currently for a key sequence.
Used only for Emacs 22 and later.")

(defvar icicle-completing-p nil "Cached value of function `icicle-completing-p'.")

(defvar icicle-completion-candidates nil "Current list of completion candidates.")

(defvar icicle-completion-map-vars
  (delq nil (list 'minibuffer-local-completion-map
                  'minibuffer-local-must-match-map
                  (and (< emacs-major-version 24)
                       (boundp 'minibuffer-local-filename-completion-map)
                       (not (eq minibuffer-local-completion-map
                                (keymap-parent minibuffer-local-filename-completion-map)))
                       'minibuffer-local-filename-completion-map)
                  (and (< emacs-major-version 24)
                       (boundp 'minibuffer-local-filename-must-match-map)
                       (not (eq minibuffer-local-must-match-map
                                (keymap-parent minibuffer-local-filename-must-match-map)))
                       'minibuffer-local-filename-must-match-map)
                  (and (< emacs-major-version 24)
                       (boundp 'minibuffer-local-must-match-filename-map)
                       (not (eq minibuffer-local-must-match-map
                                (keymap-parent minibuffer-local-must-match-filename-map)))
                       'minibuffer-local-must-match-filename-map)))
  "Minibuffer completion keymap variables.")

;; Might as well do this here.
(dolist (map  icicle-completion-map-vars)
  ;; Non-nil value, to indicate completion.
  (define-key (symbol-value map) [icicle-is-completion-map] 'ignore))

(defvar icicle-completion-prompt-overlay nil
  "Overlay used to highlight saved completion candidates.")

(defvar icicle-completing-read+insert-candidates ()
  "`completing-read' COLLECTION arg to use for `icicle-completing-read+insert'.")

(defvar icicle-completion-set-history nil "History for completion-set names.")

(defvar icicle-completion-style-set nil ; Used only for Emacs 23+
  "Current set of `vanilla' completion styles for `\\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete]'.
The available completion style sets are defined by option
`icicle-completion-style-sets'.
Not used for Emacs prior to Emacs 23 (no `completion-styles').")

(defvar icicle-compute-narrowing-regexp-p nil
  "Non-nil means that narrowing computes `icicle-narrow-regexp'.")

(defvar icicle-confirm-exit-commands
  (and (boundp 'minibuffer-confirm-exit-commands)
       (append '(icicle-prefix-complete icicle-prefix-complete-no-display
                 icicle-prefix-word-complete
                 icicle-apropos-complete icicle-apropos-complete-no-display
                 icicle-apropos-complete-and-narrow ; ????
                 ;; icicle-apropos-complete-and-exit ; ????
                 )
               minibuffer-confirm-exit-commands))
  "Version of `minibuffer-confirm-exit-commands' for Icicle mode.
Effective starting with Emacs 23.")

(defvar icicle-current-completion-candidate-overlay nil
  "Overlay used to highlight current completion candidate.")

(defvar icicle-current-completion-mode nil
  "Symbol `prefix' or `apropos', specifying the current completion mode.")

(defvar icicle-current-font-lock-part nil
  "The part of `font-lock-keywords' currently being used, during cycling.
Updated by `icicle-next-font-lock-keywords(-repeat)'.")

(defvar icicle-current-input "" "Current minibuffer input.")

(defvar icicle-current-raw-input "" "Current minibuffer raw (unexpanded) input.
This can be different from `icicle-current-input' only when
`icicle-expand-input-to-common-match' causes your input to expand.")

(defvar icicle-cycling-p nil
  "Non-nil means the user is currently cycling completion candidates.")

(defvar icicle-default-thing-insertion-flipped-p nil
  "Non-nil means a previous `M-.' in this succession was used with `C-u'.
This means that the meaning of `icicle-default-thing-insertion' has
been reversed.")

(defvar icicle-dictionary-history nil "History for dictionary entries.")

(defvar icicle-dir-candidate-can-exit-p nil
  "Non-nil means you can exit the minibuffer when you choose a directory.")

(defvar icicle-doc-history () "History for documentation entries.")

(defvar icicle-doc-last-initial-cand-set ()
  "Cache for initial set of completion candidates for `icicle-doc'.")

(defvar icicle-dot-string-internal icicle-dot-string
  "Internal version of `icicle-dot-string' (same values).
This is changed automatically by Icicles when you switch completion
mode, whereas `icicle-dot-string' is changed only via user commands.")

(defvar icicle-edit-update-p nil
  "Internal flag: non-nil when editing text in minibuffer.
More precisely, non-nil when updating the completions list inside
simple character-editing commands such as `icicle-self-insert' and
`icicle-delete-backward-char'.")

(defvar icicle-ess-use-ido nil
  "Non-nil means that `ess-completing-read' respects `ess-use-ido'.
Otherwise, in Icicle mode `ess-completing-read' always uses Icicles
completion, never Ido completion.")

(defvar icicle-exclude-default-proxies nil
  "Non-nil means exclude default proxy candidates.")

(defvar icicle-explore-final-choice ""
  "Final `icicle-explore' completion choice (a string).")

(defvar icicle-explore-final-choice-full nil
  "Full alist element that corresponds to `icicle-explore-final-choice'.
This is an element of `icicle-candidates-alist'.
The element's car is a completion-candidate string.")

(defvar icicle-extra-candidates nil "A list of extra completion candidates (strings).")

(defvar icicle-extra-candidates-dir-insert-p t
  "Non-nil means, for an extra candidate, insert a directory component.
Can be bound to nil to prevent adding a directory to non file-name
extra candidates during file-name completion.  An extra candidate is
one that is a member of `icicle-extra-candidates'.")

(defvar icicle-face-name-history nil "History for face names.")

(defvar icicle-face-remapping-Completions nil ; Used for Emacs 23+ only.
  "Face remapping cookie for `*Completions*' buffer face for font family.")

(defvar icicle-face-remapping-region nil ; Used for Emacs 23+ only.
  "Face remapping cookie for `region' face with `icicle-region-background'.")

(defvar icicle-fancy-candidates-p nil
  "Non-nil means we are completing using possibly fancy candidates.
That is, some candidates might have attached properties.

You can bind this in your code if you need to treat fancy candidates
and your code has no direct access to the `completing-read' PROMPT
string.  See section `Candidates with Text Properties' of the Icicles
doc.

If you set this to non-nil, instead of binding it, then Icicles will
always check whether each completion candidate might be fancy.  That
can be costly.")

(defvar icicle-fancy-cands-internal-p nil
  "Same as `icicle-fancy-candidates-p', but for internal use only.
Do not set or bind this.  This is bound only by `completing-read'.")

(when (> emacs-major-version 23)
  (defvar icicle-file-name-completion-table
    (completion-table-in-turn #'icicle-completion--embedded-envvar-table
                              #'completion-file-name-table)
    "Completion table used for file-name completion."))

(defvar icicle-file-sort-first-time-p t
  "Non-nil means file-name completion has not yet been used.")

(defvar icicle-filtered-default-value nil
  "Minibuffer default value, after filtering with `icicle-filter-wo-input'.")

(defvar icicle-font-name-history nil "History for font names.")

(defvar icicle-frame-alist nil "Alist of frames, returned by `icicle-make-frame-alist'.")

(defvar icicle-frame-name-history nil "History for frame names.")

(defvar icicle-full-cand-fn nil
  "nil or a function to create a full candidate from a display candidate.
If candidates are currently multi-completions then the display
candidate is assumed to have been transformed first (using
`icicle-transform-multi-completion').")

(defvar icicle-function-name-history nil "History for function names.
Each name is a symbol name or a lambda form, as a string.")

(defvar icicle-fundoc-last-initial-cand-set ()
  "Cache for initial set of completion candidates for `icicle-fundoc'.")

(defvar icicle-general-help-string "
 

Customize Icicles: `M-x icicle-customize-icicles-group'.
Summary of customizable options and faces (alphabetical order).

Some of the options can be toggled or cycled - the keys for this are
noted in parentheses.

* `case-fold-search', `completion-ignore-case',
  (`C-u') `read-file-name-completion-ignore-case'
                                         - Case sensitivity? (`C-A')
* `completion-ignored-extensions'        - Ignored filenames (`C-.')
* `icicle-act-before-cycle-flag'         - Act then cycle or reverse?
* `icicle-add-proxy-candidates-flag'     - Include proxies? (`C-M-_')
* `icicle-alternative-actions-alist'     - Overriding alt actions
* `icicle-alternative-sort-comparer'     - Other sort (`M-,', `C-M-,')
* `icicle-apropos-complete-keys*'        - Keys to apropos-complete
* `icicle-apropos-cycle-*-keys'          - Keys to apropos-cycle
* `icicle-bookmark-name-length-max'      - Max length of bookmark name
* `icicle-bookmark-refresh-cache-flag'   - Refresh bookmarks cache?
* `icicle-top-level-key-bindings'        - Bind top-level commands
* `icicle-buffer-*'                      - `icicle-buffer' options
* `icicle-candidate-width-factor'        - Width %%, candidate columns
* `icicle-change-region-background-flag' - Change region color?
* `icicle-change-sort-order-completion'  - Control `C-,' behavior
* `icicle-C-l-uses-completion-flag'      - `C-l' uses completion?
* `icicle-color-themes'                  - For `icicle-color-theme'
* `icicle-comint-dynamic-complete-replacements' - Comint complete fns
* `icicle-command-abbrev*'               - Command abbrev behavior
* `icicle-complete-key-anyway-flag'      - `S-TAB' must complete keys
* `icicle-complete-keys-self-insert-ranges'- `S-TAB' for self-insert?
* `icicle-completing-read+insert-keys'   - Keys for complete-on-demand
* `icicle-completion-history-max-length' - Completion history length
* `icicle-completion-key-bindings'       - minibuffer completion keys
* `icicle-completion-list-key-bindings'  - `*Completions*' bindings
* `icicle-Completions-display-min-input-chars'- Remove `*Completions*'
                                           if fewer chars input
* `icicle-completions-format'            - `*Completions*' layout
* `icicle-move-Completions-frame'        - `*Completions*' at edge?
* `icicle-Completions-text-scale-decrease'- `*Completions*' shrink
* `icicle-Completions-window-max-height' - Max lines, `*Completions*'
* `icicle-customize-save-flag'           - Save some options on quit?
* `icicle-default-cycling-mode'          - Default completion mode for
                                           per-mode cycling
* `icicle-default-thing-insertion'       - Control behavior of \
\\<minibuffer-local-completion-map>\\[icicle-insert-string-at-point]
* `icicle-default-value'                 - How to treat default value
* `icicle-define-alias-commands-flag'    - Define top-level aliases?
* `icicle-deletion-action-flag'          - `S-delete' deletes?
* `icicle-dot-show-regexp-flag'          - Show regexp for `.'?
* `icicle-dot-string'                    - String that `.' inserts
* `icicle-expand-input-to-common-match'  - Expand your input? (`C-M-\"')
* `icicle-expand-input-to-common-match-alt' - Expand your input? (`C-\"')
* `icicle-file-*'                        - `icicle-file' options
* `icicle-filesets-as-saved-completion-sets-flag'- Use filesets?
* `icicle-guess-commands-in-path'        - Shell commands to complete
* `icicle-help-in-mode-line-delay'       - Secs to show candidate help
* `icicle-hide-common-match-in-Completions-flag'- Show common match?
* `icicle-hide-non-matching-lines-flag'  - Hide non-match lines?
* `icicle-highlight-historical-candidates-flag'
                                         - Highlight past input?
* `icicle-highlight-input-completion-failure*'- Input non-match sign
* `icicle-highlight-input-initial-whitespace-flag'
                                         - Highlight input whitespace?
* `icicle-highlight-lighter-flag'        - Highlight mode-line `Icy'
* `icicle-incremental-completion'        - Icompletion? (`C-#')
* `icicle-incremental-completion-delay'  - Delay before update cands
* `icicle-incremental-completion-threshold'- # of candidates for delay
* `icicle-inhibit-advice-functions'      - Advice-inhibited functions
* `icicle-inhibit-ding-flag'             - Suppress audible bell
* `icicle-input-string'                  - String inserted by `C-='
* `icicle-inter-candidates-min-spaces'   - Min spaces among candidates
* `icicle-isearch-complete-keys'         - Keys to complete search
* `icicle-key-complete-keys'             - Keys to complete keys
* `icicle-key-descriptions-use-<>-flag'  - Show key names with \"<>\"?
* `icicle-keymaps-for-key-completion'    - `S-TAB' = key-complete maps
* `icicle-kmacro-ring-max'               - Icicles `kmacro-ring-max'
* `icicle-levenshtein-distance'          - Levenshtein match distance
* `icicle-list-join-string'              - Multi-completion join
* `icicle-list-nth-parts-join-string'    - Join split-candidate parts
* `icicle-mark-position-in-candidate'    - Mark position in cycling
* `icicle-menu-items-to-history-flag'    - Add menus to history?
* `icicle-minibuffer-key-bindings'       - general minibuffer keys
* `icicle-minibuffer-setup-hook'         - Functions run after setup
* `icicle-modal-cycle-*-keys'            - Keys for modal cycling
* `icicle-option-type-prefix-arg-list'   - Prefix-args for `C-h C-o'
* `icicle-point-position-in-candidate'   - Cursor position in cycling
* `icicle-populate-interactive-history-flag'- Track interactive use?
* `icicle-pp-eval-expression-print-*'    - Print control for `pp-*'
* `icicle-prefix-complete-keys*'         - Keys to prefix-complete
* `icicle-prefix-cycle-*-keys'           - Keys to prefix-cycle
* `icicle-quote-shell-file-name-flag'    - Quote file name in shell?
* `icicle-read+insert-file-name-keys'    - Keys for on-demand file
* `icicle-regexp-quote-flag'             - Escape chars? (`C-`')
* `icicle-regexp-search-ring-max'        - `regexp-search-ring-max'
* `icicle-region-background'             - Background for region
* `icicle-require-match-flag'            - Override REQUIRE-MATCH?
* `icicle-saved-completion-sets'         - Completion sets for \
`\\[icicle-candidate-set-retrieve]'
* `icicle-search-cleanup-flag'           - Remove search highlighting?
                                           (`C-.')
* `icicle-search-from-isearch-keys'      - Isearch-to-Icicles keys
* `icicle-search-highlight-all-current-flag'- In each hit (`C-^')
* `icicle-search-highlight-context-levels-flag' -
                                           Highlight match subgroups?
* `icicle-search-highlight-threshold'    - # hits to highlight at once
* `icicle-search-hook'                   - Functions run by `C-c `'
* `icicle-search-replace-common-match-flag' - Replace ECM? (`M-;')
* `icicle-search-replace-literally-flag' - Replace text literally?
* `icicle-search-replace-whole-candidate-flag' - Replace input match
                                           or whole search hit?(`M-_')
* `icicle-search-ring-max'               - Icicles `search-ring-max'
* `icicle-search-whole-word-flag'        - Find whole words? (`M-q')
* `icicle-show-Completions-help-flag'    - Show `*Completions*' help?
* `icicle-show-Completions-initially-flag'- Show `*Completions*' 1st?
* `icicle-show-multi-completion-flag'    - Show extra candidate info?
* `icicle-sort-comparer'                 - Sort candidates (`C-,')
* `icicle-sort-orders-alist'             - Predicates for sorting
* `icicle-special-candidate-regexp'      - To highlight special cands
* `icicle-S-TAB-completion-methods-alist'- `S-TAB' methods (`M-(')
* `icicle-swank-*'                       - Swank completion control
* `icicle-TAB-completion-methods'        - `TAB' methods (`C-(')
* `icicle-TAB-shows-candidates-flag'     - 1st `TAB' shows candidates?
* `icicle-test-for-remote-files-flag'    - Check remote files? (`C-^')
* `icicle-thing-at-point-functions'      - Functions to yank things
* `icicle-top-level-key-bindings'        - Top-level key bindings
* `icicle-top-level-when-sole-completion-*'- Exiting if one completion
* `icicle-touche-pas-aux-menus-flag'     - Add to standard menus?
* `icicle-transform-function'            - Remove duplicates (`C-$')
* `icicle-type-actions-alist'            - Objects and their types
* `icicle-unpropertize-completion-result-flag'- Properties in result?
* `icicle-update-input-hook'             - Fns run when input changes
* `icicle-use-~-for-home-dir-flag'       - Use `~' for $HOME? (`M-~')
* `icicle-use-C-for-actions-flag'        - `C-' for actions? (`M-g')
* `icicle-use-candidates-only-once-flag' - Remove used candidate?
* `icicle-word-completion-keys'          - Keys for word completion
* `icicle-WYSIWYG-Completions-flag'      - WYSIWYG `*Completions*'?
* `icicle-yank-function'                 - Yank function to use

Faces that highlight input in minibuffer.

* `icicle-complete-input'               - Input when it is complete
* `icicle-completion'                   - Completing?
* `icicle-input-completion-fail*'       - Non-match part of input
* `icicle-match-highlight-minibuffer'   - Matched part of input
* `icicle-multi-command-completion'     - Multi-command completion?
* `icicle-mustmatch-completion'         - Strict completion?
* `icicle-whitespace-highlight'         - Initial whitespace in input

Faces that highlight candidates in buffer `*Completions*'.

* `icicle-candidate-part'               - Part of candidate
* `icicle-common-match-highlight-Completions' - Max common substring
* `icicle-current-candidate-highlight'  - Current candidate (cycling)
* `icicle-extra-candidate'              - Extra candidate
* `icicle-historical-candidate'         - Highlight candidates used
* `icicle-match-highlight-Completions'  - Matched part of input
* `icicle-proxy-candidate'              - Proxy candidate
* `icicle-saved-candidate'              - Saved candidate
* `icicle-special-candidate'            - Special candidate

Faces that highlight information in the mode line.

* `icicle-completion'                   - Completing?
* `icicle-mode-line-help'               - Candidate help
* `icicle-multi-command-completion'     - Multi-command completion?
* `icicle-mustmatch-completion'         - Strict completion?

Faces that highlight for command `icicle-search'.

* `icicle-search-context-level-*'       - Regexp subgroup highlighting
* `icicle-search-current-input'         - What input matches
* `icicle-search-main-regexp-current'   - Current match of 1st regexp
* `icicle-search-main-regexp-others'    - Other matches of 1st regexp

Icicle mode defines many top-level commands.  For a list, see the
Commentary headers of files `icicles-cmd1.el' and `icicles-cmd2.el'.
 

These are all of the top-level bindings in Icicle mode:

\\{icicle-mode-map}"
  "General help string included in `icicle-minibuffer-help'.")

(defvar icicle-get-alist-candidate-function 'icicle-get-alist-candidate
  "Function used to retrieve a full completion candidate.
The signature must match that of the default value,
`icicle-get-alist-candidate'.")

(defvar icicle-hist-cands-no-highlight ()
  "List of candidates not highlighted using `icicle-historical-candidate'.
Bind, don't assign this, since the same string can have different
meanings in different contexts.")

(defvar icicle-hist-var nil
  "A history variable.
`let'-bind this to a history variable.
Leave the global value as `nil', to use it conditionally: (or ...).")

(defvar icicle-ignored-extensions completion-ignored-extensions
  "Copy of `completion-ignored-extensions', serving as a control flag.
When `completion-ignored-extensions' changes, we remake
`icicle-ignored-extensions-regexp'.")

(defvar icicle-ignored-extensions-regexp
  (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "$\\|")
          "$\\)\\'")
  "Regular expression matching ignored file extensions.
If this is nil, then no file extensions are ignored.
The ignored file extensions come from `completion-ignored-extensions'.")

(defvar icicle-incremental-completion-p nil
  "Takes the place of `icicle-incremental-completion' during input.
The program updates this to `always' from `t' after `*Completions*' has
been displayed.")

(defvar icicle-Info-index-cache ()
  "Cache list of index entries and their nodes and files.
Each cache entry has the form (TOPIC NODE INFO-FILE).")

(defvar icicle-Info-only-rest-of-book-p nil
  "Non-nil means complete only Info nodes from the rest of the book.")

(defvar icicle-Info-tag-table-posn nil
  "Tag table position in last Info file used by `icicle-Info-goto-node'.")

(defvar icicle-inhibit-sort-p nil
  "Non-nil means that users cannot sort completion candidates.
They also cannot remove duplicates.")

(defvar icicle-inhibit-try-switch-buffer nil
  "Non-nil means do not switch back to `icicle-orig-buff'.
\(The potential switching is in `icicle-try-switch-buffer'.)")

(defvar icicle-initial-value ""
  "Initial value used in minibuffer completion.
Any function that reads from the minibuffer and accepts a default
value or initial value should, before reading, put that value in
`icicle-initial-value'.  For example, `completing-read' does that.

In addition, `completing-read' and `read-file-name' will respect this
value, using it as the initial value if none is provided explicitly.
This means that you can bind `icicle-initial-value' around an
expression that calls `completing-read' or `read-file-name', and the
bound value will be used as the initial value.")

(defvar icicle-input-completion-fail-overlay nil
  "Overlay used to highlight the input portion that does not complete.")

(defvar icicle-input-fail-pos nil
  "Position in minibuffer of start of completion match failure.
Nil means no match failure is known.")

(defvar icicle-insert-string-at-pt-end nil
  "Position of end of text `icicle-insert-string-at-point' inserted.")

(defvar icicle-insert-string-at-pt-start nil
  "Position of start of text `icicle-insert-string-at-point' inserted.")

(defvar icicle-interactive-history ()
  "History of commands called using `call-interactively'.")

(defvar icicle-izones-var 'zz-izones
  "Current izones variable used by Icicles zone commands.
\(See library`zones.el'.)")

(defvar icicle-key-prefix-description ""
  "Description of a prefix key at some point during key completion.")

(defvar icicle-kill-history nil "History of kill-ring entries.")

(when (require 'kmacro nil t)           ; Emacs 22+
  (defvar icicle-kmacro-alist nil
    "Alist with elements (CANDIDATE-NAME . RING-ITEM).
CANDIDATE-NAME is 1, 2, 3....

RING-ITEM is an item in `kmacro-ring' or `(kmacro-ring-head)'.")
  (defvar icicle-kmacro-history nil "History for keyboard-macro names."))

(defvar icicle-last-apropos-complete-match-fn 'string-match
  "Last value of `icicle-apropos-complete-match-fn'.")

(defvar icicle-last-completion-candidate ""
  "Last completion candidate used in minibuffer completion.")

(defvar icicle-last-completion-command nil "Last completion command used.")

(defvar icicle-last-icomplete-mode-value (and (featurep 'icomplete)  icomplete-mode)
  "Value of `icomplete-mode' when completion started.")

(defvar icicle-last-input "" "Last minibuffer input typed (not from cycling).")

(defvar icicle-last-sort-comparer (or icicle-sort-comparer 'icicle-case-string-less-p)
  "Local copy of `icicle-sort-comparer', so we can restore it.")

(defvar icicle-last-top-level-command nil "Last top-level command used.")

(defvar icicle-lighter-truncation "..."
  "String appended to Icy lighter to show candidates-list truncation.")

(defvar icicle-list-use-nth-parts nil
  "List of indexes of multi-completion pieces to use.
This is not an internal variable.  You can bind this in your own Lisp
code to affect completion behavior.

An empty list means use the entire multi-completion.  Otherwise,
concatenate, in order, the Nth parts of the multi-completion, where N
is each of the (one-based) indexes, in turn.  Any index larger than
the actual number of parts in the multi-completion means use the last
part.

For example: If the value is (1), then use only the first part of the
multi-completion as the completion candidate. If the value is (2 1),
then use as candidate the second part followed by the first part, the
two parts being joined by option `icicle-list-nth-parts-join-string'.
If the value is (1 99) and the multi-completion has fewer than 99
parts, then use the first and last parts, joined by
`icicle-list-nth-parts-join-string'.  If the value is (2 1 2), then
use the second part, first part, and second part again - you can use a
given part any number of times.")

(defvar icicle-menu-map nil "Icicles menu-bar menu keymap.")

(defvar icicle-minibuffer-message-ok-p t
  "Non-nil means we can show messages in minibuffer.
This affects only `icicle-msg-maybe-in-minibuffer'.")

(defvar icicle-minor-mode-map-entry nil "Icicles mode entry in `minor-mode-map-alist'.")

(defvar icicle-mode-line-help nil "Current mode line help (a string), or nil if none.")

(defvar icicle-ms-windows-drive-hash (and (fboundp 'make-hash-table)
                                          (make-hash-table :test 'equal))
  "Hash table for caching result of MS Windows `NET USE' system calls.
For Emacs 20 and 21, this is not used unless you load library `cl.el'
at runtime.")

(defvar icicle-multi-completing-p nil
  "Non-nil means we are currently completing with multi-completions.")

(defvar icicle-multi-inputs-action-fn nil
  "Function to apply to candidates that result from splitting input.
The current minibuffer input is interpreted as a list of candidates.
This function is applied to each candidate in turn, in list order.
If nil then act using function `icicle-candidate-action-fn'.")

(defvar icicle-must-match-regexp nil
  "A regexp that completion candidates must match, or nil.
If nil, then this does nothing.  If a regexp (string), then show only
candidates whose display form matches it (and matches the user input).
The display form is the string shown in `*Completions*'.

Note: This is similar to the standard variable
`completion-regexp-list', except:
* `completion-regexp-list' is a list of regexps, not just one.
* `icicle-must-match-regexp' is used after filtering using option
  `icicle-transform-function'.

See also `icicle-must-not-match-regexp'.")

(defvar icicle-must-not-match-regexp nil
  "A regexp that completion candidates must not match, or nil.
If nil, then this does nothing.  If a regexp (string), then show only
candidates whose display form does not match it.
The display form is the string shown in `*Completions*'.
See also `icicle-must-match-regexp'.")

(defvar icicle-must-pass-after-match-predicate nil
  "Predicate that completions must satisfy after matching input, or nil.
This is just like `icicle-must-pass-predicate', except that it is
applied only to display candidates that match your current input.")

(defvar icicle-must-pass-predicate nil
  "Predicate that completion display candidates must satisfy, or nil.
If nil, then this does nothing.  Otherwise, this is a function of one
argument, a display candidate (a string), and only the display
candidates that satisfy the predicate are displayed.  A display
candidate is a string of text such as you see in buffer
`*Completions*'.

Note that this predicate is different from the PREDICATE argument for
function `completing-read' or `read-file-name'.  The latter applies to
the elements of the COLLECTION argument, which are typically alist
entries or obarray symbols.  `icicle-must-pass-predicate' applies
instead to a string, the display form of a completion candidate.

See also `icicle-must-pass-after-match-predicate'.")

(defvar icicle-narrow-regexp nil
  "Regexp matching each member of `icicle-completion-candidates'.
This is nil except during narrowing, and then only if
`icicle-compute-narrowing-regexp-p' is non-nil.")

(defvar icicle-nb-candidates-before-truncation 0
  "Number of candidates, before truncation per `icicle-max-candidates'.")

(defvar icicle-nb-of-other-cycle-candidates 0
  "Number of other candidates available for cycling.
This is for use by other libraries, in particular, `icomplete+.el'.")

(defvar icicle-new-last-cmd nil
  "Copy of current command being executed.
Used by, e.g., `icicle-execute-extended-command'.")

(defvar icicle-next-apropos-complete-cycles-p nil
  "Whether the next apropos-completion command should cycle.")

(defvar icicle-next-prefix-complete-cycles-p nil
  "Whether the next prefix-completion command should cycle.")

(defvar icicle-next-window-for-display-buffer nil
  "The window to use for the next buffer-displaying call.
Affects `display-buffer', `switch-to-buffer',
`switch-to-buffer-other-window', and functions that call these.
\(Not used for Emacs versions prior to Emacs 24.)")

(defvar icicle-orig-buff nil
  "Current buffer when you invoked an Icicles multi-command.")

(defvar icicle-orig-font-lock-keywords nil
  "Saved value of `font-lock-keywords'.")
(make-variable-buffer-local 'icicle-orig-font-lock-keywords)

(defvar icicle-orig-minibuffer-completion-pred nil
  "Saved value of `minibufer-completion-predicate'.")

(defvar icicle-orig-minibuffer-completion-table nil
  "Saved value of `minibufer-completion-table'.")

(defvar icicle-orig-must-match-regexp nil
  "Saved value of `icicle-must-match-regexp'.")

(defvar icicle-orig-must-not-match-regexp nil
  "Saved value of `icicle-must-not-match-regexp'.")

(defvar icicle-orig-must-pass-after-match-pred nil
  "Saved value of `icicle-must-pass-after-match-predicate'.")

(defvar icicle-orig-must-pass-predicate nil
  "Saved value of `icicle-must-pass-predicate'.")

(defvar icicle-orig-pt-explore nil
  "Point when you invoked `icicle-explore'.")

(defvar icicle-orig-read-file-name-fn (and (not (boundp 'read-file-name-function)) ; Em 22+
                                           'icicle-ORIG-read-file-name) ; Emacs 20, 21
  "Value of `read-file-name-function' outside of Icicle mode.
For versions of Emacs < 22, this is the original `read-file-name'.")

(defvar icicle-orig-window nil
  "Selected window when you invoked an Icicles multi-command.")

(defvar icicle-orig-win-explore nil
  "Selected window when you invoked `icicle-explore'.")

(defvar icicle-other-window nil
  "Window scrolled by `icicle-scroll-forward'/`icicle-scroll-backward'")

(defvar icicle-path-variables '(cd-path  charset-map-path  compilation-search-path
                                custom-theme-load-path  exec-path  ffap-bib-path  ffap-c-path
                                ffap-fortran-path  ffap-tex-path  find-function-source-path
                                image-load-path  load-path  x-bitmap-file-path)
  "List of variables whose value can be a list containing directories.
The variables are not checked until they are used.  At that time:
* Any of them that are not bound are ignored.
* If the value of any of them is not a list it is ignored.
* If it is a list, any non-string elements in the list are ignored.")

(defvar icicle-plist-last-initial-cand-set ()
  "Cache for initial set of completion candidates for `icicle-plist'.")

(defvar icicle-post-command-hook nil
  "Functions added to `post-command-hook' when in Icicle mode.
Use command `icy-mode' (aka `icicle-mode') to set this up properly.")

(defvar icicle-pre-command-hook nil
  "Functions added to `pre-command-hook' when in Icicle mode.
Use command `icy-mode' (aka `icicle-mode') to set this up properly.")

(defvar icicle-predicate-types-alist
  '(("arrayp") ("atom") ("auto-save-file-name-p" . "file") ("backup-file-name-p" . "file")
    ("booleanp") ("bool-vector-p") ("bufferp" . "buffer")
    ("byte-code-function-p" . "function") ("byte-compile-const-symbol-p" . "symbol")
    ("case-table-p") ("char-or-string-p") ("char-table-p") ("color-defined-p" . "color")
    ("commandp" . "command") ("consp") ("custom-variable-p" . "option")
    ("display-table-p") ("facep" . "face") ("fboundp" . "function")
    ("ffap-file-remote-p" . "file") ("file-accessible-directory-p" . "file")
    ("file-directory-p" . "file") ("file-executable-p" . "file")
    ("file-exists-p" . "file") ("file-name-absolute-p" . "file")
    ("file-readable-p" . "file") ("file-regular-p" . "file") ("file-remote-p" . "file")
    ("file-symlink-p" . "file") ("file-writable-p" . "file") ("floatp")
    ("frame-configuration-p") ("frame-iconified-p" . "frame") ("frame-live-p" . "frame")
    ("frame-visible-p" . "frame") ("framep" . "frame") ("functionp" . "function")
    ("hash-table-p") ("icicle-binary-option-p" . "option") ("info-file-exists-p" . "file")
    ("integer-or-marker-p") ("integerp") ("keymapp") ("keywordp") ("listp")
    ("local-variable-p" . "variable") ("markerp") ("wholenump") ("nlistp") ("numberp")
    ("number-or-marker-p") ("overlayp") ("processp" . "process")
    ("process-running-child-p" . "process") ("risky-local-variable-p" . "variable")
    ("safe-local-variable-p" . "variable") ("sequencep") ("string-or-null-p") ("stringp")
    ("subrp") ("symbolp" . "symbol") ("syntax-table-p")
    ("thumfr-thumbnail-frame-p" . "frame") ("truncated-partial-width-window-p" . "window")
    ("user-variable-p" . "option") ("vectorp") ("window-configuration-p")
    ("window-fixed-size-p" . "window") ("window-full-width-p" . "window")
    ("window-live-p" . "window") ("window-minibuffer-p" . "window") ("windowp" . "window")
    ("window-safely-shrinkable-p" . "window") ("x-color-defined-p" . "color"))
  "Alist of type names that are predicate names.
Each element is cons of a predicate name and the associated type from
`icicle-type-actions-alist' (or nil if there is no associated type).")

(defvar icicle-pref-arg nil
  "Prefix arg value when you invoked an Icicles multi-command.")

(defvar icicle-pre-minibuffer-buffer nil
  "Buffer that was current before the minibuffer became active.")

(defvar icicle-previous-raw-file-name-inputs nil
  "Previous inputs user has typed during file-name completion.
These are inputs typed but not necessarily entered with `RET'.")

(defvar icicle-previous-raw-non-file-name-inputs nil
  "Previous inputs user has typed during non-file-name completion.
These are inputs typed but not necessarily entered with `RET'.")

(defvar icicle-progressive-completing-p nil
  "Non-nil means this completion is a narrowing completion.")

(defvar icicle-prompt nil
  "A minibuffer prompt.
`let'-bind this to a string.
Leave the global value as `nil', to use it conditionally: (or ...).")

(defvar icicle-proxy-candidate-regexp nil
  "Regexp to match proxy candidates, or nil to do nothing.
The candidates are highlighted in buffer `*Completions*' using face
`icicle-proxy-candidate'.")

(defvar icicle-proxy-candidates nil "List of proxy completion candidates (strings).")

(when (fboundp 'read-char-by-name)      ; Emacs 23+
  (defvar icicle-read-char-history ()
    "History list for reading characters by name.
Augmented by `icicle-read-char-maybe-completing' and
`icicle-read-char-by-name'."))

(defvar icicle-read-expression-map nil
  "Icicle mode version of `read-expression-map'.
Several standard Emacs-Lisp mode key bindings are available.
In addition, `TAB' completes a symbol and `C-M-i' (or `ESC TAB')
indents the current line.")
(unless icicle-read-expression-map
  (let ((map  (make-sparse-keymap)))
    (define-key map (icicle-kbd "C-M-i")   'lisp-indent-line) ; `ESC TAB', `C-M-i'
    (define-key map (icicle-kbd "C-i")     'icicle-lisp-complete-symbol) ; `C-i', `TAB'
    (define-key map (icicle-kbd "ESC tab") 'lisp-indent-line) ; `ESC tab'
    (define-key map (icicle-kbd "C-M-x")   'eval-defun) ; `ESC C-x', `C-M-x'
    (define-key map (icicle-kbd "C-M-q")   (if (fboundp 'indent-pp-sexp) ; `ESC C-q', `C-M-q'
                                               'indent-pp-sexp ; Emacs 22+
                                             'indent-sexp))
    ;;(define-key map (icicle-kbd "DEL") 'backward-delete-char-untabify)
    (set-keymap-parent map minibuffer-local-map)
    (setq icicle-read-expression-map  map)))

(defvar icicle-remove-icicles-props-p t
  "Non-nil means to remove Icicles text properties from completion result.
Icicles binds this internal variable to nil in contexts where it needs
the completion result string to retain its Icicles text properties.

Otherwise, function `icicle-unpropertize-completion' removes at least
the Icicles internal text properties from the final completion result.
Depending on the value of option
`icicle-unpropertize-completion-result-flag', it may also remove all
text properties.")

;; Same as `directory-files-no-dot-files-regexp' in `files.el', available for Emacs 23+.
(defconst icicle-re-no-dot "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"
  "Regexp that matches anything except `.' and `..'.")

(defvar icicle-require-match-p nil
  "Current REQUIRE-MATCH arg to `completing-read' or `read-file-name'.
Starting with Emacs 23, this is no longer enough to tell whether a
match is required - use function `icicle-require-match-p' instead.")

(defvar icicle-reverse-multi-sort-p nil
  "Non-nil means the truth values returned by predicates are complemented.
This changes the order of the sorting groups, but it does not in
general reverse that order.  The order within each group is unchanged
\(not reversed).")

(defvar icicle-reverse-sort-p nil
  "Non-nil means that candidates are being sorted in the reverse order.")

(defvar icicle-saved-candidate-overlays nil
  "Overlays used to highlight saved completion candidates.")

(defvar icicle-saved-candidates-variables-obarray (make-vector 100 0)
  "Obarray of variables you have saved sets of completion candidates in.
Used for completion in `icicle-candidate-set-retrieve-from-variable'.")

(defvar icicle-saved-completion-candidate nil
  "Completion candidate to be restored after recursive `completing-read'.")

(defvar icicle-saved-completion-candidates nil
  "Completion candidates saved using `icicle-candidate-set-save'.")

(defvar icicle-saved-completion-candidates-internal nil
  "Completion candidates saved temporarily by program.")

(defvar icicle-saved-ignored-extensions nil
  "Local copy of `icicle-ignored-extensions', so we can restore it.")

(when (require 'kmacro nil t)           ; Emacs 22+
  (defvar icicle-saved-kmacro-ring-max kmacro-ring-max
    "Saved value of `kmacro-ring-max', so it can be restored."))

(defvar icicle-saved-regexp-search-ring-max regexp-search-ring-max
  "Saved value of `search-ring-max', so it can be restored.")

(defvar icicle-saved-proxy-candidates nil "Saved value of `icicle-proxy-candidates'.")

(defvar icicle-saved-region-background nil
  "Background of `region' face.  Saved so it can be restored.")

(defvar icicle-saved-search-ring-max search-ring-max
  "Saved value of `search-ring-max', so it can be restored.")

(defvar icicle-scan-fn-or-regexp nil
  "`icicle-search' parameter SCAN-FN-OR-REGEXP.  Used by `M-,'.")

(defvar icicle-scroll-Completions-reverse-p nil
  "Non-nil means `icicle-scroll-Completions-*' scrolls in opposite direction.")

(defvar icicle-search-command 'icicle-search
  "Command to use for Icicles searches.
You can set a buffer-local value of this variable, to use a specific
search command in a particular mode.")

(defvar icicle-search-complement-domain-p nil
  "Non-nil means complement the initial search candidates wrt the buffer.
This has an effect only on (some) Icicles search commands.
The scan function or regexp for the search command defines a set of
matches in the buffer.  If this option is non-nil then the actual
candidates used are the sections of buffer text that are separated by
the initial candidates, that is, the non-candidates as defined by the
scan or regexp.")

(defvar icicle-search-context-level 0
  "Match level for `icicle-search' context regexp.
0 means use whatever matches the whole context regexp as the search
context.  1 means use whatever matches the first subgroup of the
regexp as the search context, and so on.")

(defvar icicle-search-context-regexp ""
  "Current search-context regexp used in `icicle-search'.")

(defvar icicle-search-current-overlay nil
  "Overlay used to highlight current match of `icicle-search' regexp arg.")

(defvar icicle-search-final-choice nil
  "Final user input from `icicle-search'.
This might or might not be one of the possible search candidates.")

(defvar icicle-search-history nil "History for `icicle-search' final choices.")

(defvar icicle-search-in-context-fn 'icicle-search-in-context-default-fn
  "Function used by `icicle-search-action' to act on search context.
The default value is `icicle-search-in-context-default-fn'.
The function must take two arguments:
 - A full search candidate object, which is a cons of the candidate
   name and its source-file marker.
 - A replacement string or function, or nil if no replacement is to be
   made.  If a function then it must accept a string argument (the
   match to replace) and return a string (the replacement text).

When the function is called, the region has been narrowed to the
current search context.")

(defvar icicle-searching-p nil "Non-nil means an Icicles search command is in progress.")

(defvar icicle-search-level-overlays nil
  "Overlays used to highlight context levels other than the top level.")

(defvar icicle-search-map
  (let ((map  (make-sparse-keymap)))
    (define-key map (icicle-kbd "b") 'icicle-search-buffer) ; `b'uffer
    (define-key map (icicle-kbd "c") 'icicle-search-char-property) ; `c'har property
    (define-key map (icicle-kbd "d") 'icicle-search-defs) ; `d'efinitions
    (define-key map (icicle-kbd "D") 'icicle-search-defs-full) ; `D'efinitions
    (define-key map (icicle-kbd ",") 'icicle-tags-search) ; Like `M-,' for `tags-loop-continue'
    (define-key map (icicle-kbd "f") 'icicle-search-file) ; `f'ile
    (define-key map (icicle-kbd "g") 'icicle-grep-saved-file-candidates) ; `g'rep
    (define-key map (icicle-kbd "i") 'icicle-imenu)  ; `i'menu
    (define-key map (icicle-kbd "I") 'icicle-imenu-full) ; `I'menu
    (define-key map (icicle-kbd "j") 'icicle-search-bookmark) ; `j'ump to bookmark first
    (define-key map (icicle-kbd "J") 'icicle-search-bookmarks-together); `J'ump to bookmark 1st
    (define-key map (icicle-kbd "k") 'icicle-search-keywords) ; `k'eywords
    (define-key map (icicle-kbd "l") 'icicle-search-lines) ; `l'ines
    (define-key map (icicle-kbd "C-l") 'icicle-search-pages) ; `C-l' is the page separator
    ;; Save `m' for `marked'/`mode-specific'.
    (define-key map (icicle-kbd "o") 'icicle-occur)  ; `o'ccur
    (define-key map (icicle-kbd "p") 'icicle-search-paragraphs) ; `p'aragraphs
    (define-key map (icicle-kbd "O") 'icicle-search-overlay-property) ; `O'verlay
    (define-key map (icicle-kbd "s") 'icicle-search-sentences) ; `s'entence
    (define-key map (icicle-kbd "M-s") 'icicle-search-generic)
    (define-key map (icicle-kbd "t") 'icicle-search-thing) ; `t'hing
    (define-key map (icicle-kbd "T") 'icicle-search-text-property) ; `T'ext
    (define-key map (icicle-kbd "w") 'icicle-search-word) ; `w'ord
    (define-key map (icicle-kbd "x") 'icicle-search-xml-element) ; `x'ml
    (define-key map (icicle-kbd "X") 'icicle-search-xml-element-text-node) ; `X'ml
    map)
  "Keymap for Icicles search commands.
It is bound to the key prefix `icicle-search-key-prefix'.")

(defvar icicle-search-modes
  '((dired-mode           (progn (unless (fboundp 'diredp-get-files)
                                   (icicle-user-error "You need library `Dired+' for this"))
                                 (diredp-get-files)))
    (ibuffer-mode         (nreverse (ibuffer-get-marked-buffers)))
    (Buffer-menu-mode     (Buffer-menu-marked-buffers))
    (bookmark-bmenu-mode  (progn (unless (fboundp 'bmkp-bmenu-get-marked-files)
                                   (icicle-user-error "You need library `Bookmark+' for this"))
                                 (bmkp-bmenu-get-marked-files))))
  "Alist that maps `major-mode' values to sexps that return WHERE.
Each entry is a two-element list (MODE SEXP).
SEXP returns the WHERE argument for `icicle-search', for MODE.")

(defvar icicle-search-overlays nil
  "Overlays used to highlight match of `icicle-search' regexp argument.")

(defvar icicle-search-refined-overlays nil
  "Overlay(s) used to highlight match of current input for `icicle-search'.
If `icicle-search-highlight-threshold' is less than one, then this is
a single overlay (or nil).  Otherwise, this is a list of overlays.")

(defvar icicle-search-replacement nil
  "Replacement string for use during `icicle-search'.")

(defvar icicle-search-replacement-history nil
  "History variable for reading replacement string for `icicle-search'.")

(defvar icicle-successive-grab-count 0
  "Number of text things to be grabbed by next `\\<minibuffer-local-map>\
\\[icicle-insert-string-at-point]'.")

(defvar icicle-text-property-value-history nil
  "History variable for reading text properties.")

;; (defvar icicle-text-properties-alist
;;   '(;; Properties listed in Elisp manual node `Special Properties':
;;     ("category") ("face") ("font-lock-face") ("mouse-face") ("fontified") ("display")
;;     ("help-echo") ("keymap") ("local-map") ("syntax-table") ("read-only") ("invisible")
;;     ("intangible") ("field") ("cursor") ("pointer") ("line-spacing") ("line-height")
;;     ("modification-hooks") ("insert-in-front-hooks") ("insert-behind-hooks")
;;     ("point-entered") ("point-left")
;;     ;; Properties listed in Elisp manual node `Format Properties':
;;     ("hard") ("right-margin") ("left-margin") ("justification")
;;     ;; Properties listed in Elisp manual node `Links and Mouse-1':
;;     ("follow-link")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp':
;;     ("allout-was-hidden") ("ansi-color") ("buffer") ("buffer-name") ("column")
;;     ("button") ("skip") ("literal") ("front-sticky") ("rear-nonsticky") ("composition")
;;     ("untranslated-utf-8") ("yank-handler") ("dired-filename") ("read-face-name")
;;     ("directory") ("message") ("debug") ("font-lock-multiline") ("unknown")
;;     ("insert-in-front-hooks") ("kbd-help") ("hilit-chg") ("ibuffer-filter-group-name")
;;     ("ibuffer-properties") ("ibuffer-title") ("ibuffer-summary")
;;     ("ibuffer-title-header") ("inhibit-line-move-field-capture") ("image-counter")
;;     ("header-line") ("cvs-goal-column") ("occur-target") ("occur-match")
;;     ("foreign-selection") ("before-string") ("after-string") ("ses")
;;     ("smerge-force-highlighting") ("speedbar-function") ("speedbar-token")
;;     ("speedbar-text") ("type") ("stroke-glyph") ("data") ("thumb-image-file")
;;     ("original-file-name") ("associated-dired-buffer") ("tags") ("comment")
;;     ("tumme-thumbnail") ("tutorial-remark") ("vc-cvs-annotate-time") ("end-name")
;;     ("old-name") ("end-link") ("old-link") ("end-perm") ("old-perm") ("perm-changed")
;;     ("widget-doc") ("secret") ("real-field")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/emacs-lisp':
;;     ("elp-symname") ("printed-value") ("duplicable")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/emulation':
;;     ("cursor")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/erc':
;;     ("erc-callback") ("erc-data") ("erc-identified") ("erc-parsed") ("erc-parsed")
;;     ("timestamp") ("erc-prompt")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/eshell':
;;     ("comment") ("arg-begin") ("arg-end") ("escaped") ("history") ("number")
;;     ("test-func")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/gnus':
;;     ("earcon-data") ("earcon-callback") ("gnus-category") ("gnus-part")
;;     ("article-type") ("gnus-decoration") ("dummy-invisible") ("original-date")
;;     ("gnus-data") ("gnus-callback") ("gnus-prev") ("gnus-next") ("gnus-mime-details")
;;     ("gnus-line-format") ("gnus-backlog") ("gnus-image-category")
;;     ("gnus-image-text-deletable") ("gnus-group") ("gnus-level") ("gnus-indentation")
;;     ("gnus-unread") ("gnus-number") ("articles") ("gnus-server") ("gnus-named-server")
;;     ("gnus-intangible") ("gnus-topic") ("gnus-topic-level") ("gnus-topic-unread")
;;     ("gnus-topic-visible") ("gnus-active") ("gnus-position") ("gnus-time")
;;     ("gnus-face") ("gnus-undeletable") ("message-rank") ("egg-end") ("egg-lang")
;;     ("egg-start") ("message-hidden") ("message-deletable") ("buffer") ("from") ("mm")
;;     ("script-name")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/international':
;;     ("kkc-conversion-index") ("advice") ("untranslated-utf-8") ("composition")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/mail':
;;     ("footnote-number") ("rmail-fontified")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/mh-e':
;;     ("mh-data") ("mh-mime-inserted") ("mh-part") ("mh-region") ("mh-callback")
;;     ("mh-button-pressed") ("mh-line-format") ("mh-folder") ("mh-children-p")
;;     ("mh-expanded") ("mh-level") ("mh-count")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/net':
;;     ("feed") ("w3m-image") ("nt-age") ("nt-title") ("nt-guid") ("nt-desc")
;;     ("org-invisible") ("nt-link") ("nt-type") ("nt-face")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/progmodes':
;;     ("c-type") ("c-awk-NL-prop") ("c-is-sws") ("c-decl-arg-start") ("c-decl-end")
;;     ("c-decl-id-start") ("c-decl-type-start") ("message") ("REx-interpolated")
;;     ("in-pod") ("here-doc-group") ("syntax-type") ("indentable") ("REx-part2")
;;     ("first-format-line") ("attrib-group") ("cperl-postpone") ("cpp-data")
;;     ("cpp-callback") ("token") ("ebrowse-tree") ("ebrowse-member") ("ebrowse-what")
;;     ("gdb-enabled") ("gdb-bptno") ("gdb-max-frames") ("link") ("fetch") ("begin-glyph")
;;     ("begin-glyph-layout") ("idlwave-class") ("data") ("source") ("keyword")
;;     ("find-args")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/term':
;;     ("mac-ts-active-input-string")
;;     ;; Others in Emacs-Lisp libraries in directory `lisp/textmodes':
;;     ("fill-space") ("priority") ("test") ("end-glyph") ("begin-glyph") ("org-cwidth")
;;     ("org-dwidth") ("org-dwidth-n") ("org-linked-text") (":org-clock-minutes")
;;     ("org-protected") ("org-date-line") ("org-today") ("day") ("org-agenda-type")
;;     ("time-of-day") ("org-not-done-regexp") ("prefix-length") ("tags") ("org-marker")
;;     ("org-agenda-diary-link") ("org-hd-marker") ("dotime") ("org-category")
;;     ("undone-face") ("done-face") ("xr-alist") ("table-cell") ("text-clones")
;;     ;; Others in my own libraries:
;;     ("font-lock-ignore") ("highlight") ("back-link") ("forward-link"))
;;   "Alist of text properties known to Emacs.
;; Each element is of form (PROP), where PROP is the name of a text
;; property (a string).")

(defvar icicle-thing-at-pt-fns-pointer 0
  "Current index into the car of `icicle-thing-at-point-functions'.
This points to the current function in the list.")

(defvar icicle-toggle-map nil
  "Icicles keymap for toggle and cycle commands on prefix key `M-i'.
Active only during completion in Icicle mode.")

(define-prefix-command 'icicle-toggle-map)
(define-key icicle-toggle-map (icicle-kbd "M-i") 'icicle-toggle-option) ; not bound, M-i
(define-key icicle-toggle-map (icicle-kbd "a") 'icicle-toggle-annotation) ; C-x C-a, `a'
(define-key icicle-toggle-map (icicle-kbd "A") 'icicle-toggle-case-sensitivity) ; C-A, `A'
(define-key icicle-toggle-map "F" 'icicle-toggle-include-cached-files) ; C-x F, `F'
(define-key icicle-toggle-map "g" 'icicle-toggle-C-for-actions) ; M-g, `g'
(define-key icicle-toggle-map "h" 'icicle-dispatch-C-x.) ; C-x ., `h'
(define-key icicle-toggle-map "m" 'icicle-toggle-show-multi-completion) ; M-m, `m'
(define-key icicle-toggle-map "p" 'icicle-toggle-proxy-candidates) ; C-M-_, `p'
(define-key icicle-toggle-map "q" 'icicle-dispatch-M-q) ; M-q, `q'
(define-key icicle-toggle-map "r" 'icicle-toggle-include-recent-files) ; C-x R, `r'
(define-key icicle-toggle-map "s" 'icicle-toggle-highlight-saved-candidates) ; S-pause, `s'
(define-key icicle-toggle-map "t" 'icicle-cycle-image-file-thumbnail) ; C-x t, `t'
(define-key icicle-toggle-map "w" 'icicle-toggle-WYSIWYG-Completions) ; C-S-pause, `w'
(define-key icicle-toggle-map "," 'icicle-toggle-sorting) ; C-, is `*-change-sort-order', `,'
(define-key icicle-toggle-map "\M-," 'icicle-toggle-alternative-sorting) ; C-M-,, `M-,'
(define-key icicle-toggle-map "<" 'icicle-toggle-angle-brackets) ; not bound, `<'
(define-key icicle-toggle-map "/" 'icicle-toggle-expand-directory) ; C-x /, `/'
(define-key icicle-toggle-map "\"" 'icicle-toggle-expand-to-common-match) ; C-", `"'
(define-key icicle-toggle-map "\M-\"" 'icicle-cycle-expand-to-common-match) ; C-", `"'
(define-key icicle-toggle-map "#" 'icicle-cycle-incremental-completion) ; C-#,
(define-key icicle-toggle-map (icicle-kbd "M-#") 'icicle-toggle-icomplete-mode) ; C-M-#, `#'
(define-key icicle-toggle-map ":" 'icicle-toggle-network-drives-as-remote) ; C-x :, `:'
(define-key icicle-toggle-map "$" 'icicle-toggle-transforming) ; C-$, `$'
(define-key icicle-toggle-map "^" 'icicle-dispatch-C-^) ; C-^ via, `^'
(define-key icicle-toggle-map "\M-^" 'icicle-toggle-completions-format) ; C-M-^, `M-^
(define-key icicle-toggle-map "." 'icicle-dispatch-C-.) ; C-., `.'
(define-key icicle-toggle-map (icicle-kbd "M-.") 'icicle-toggle-dot) ; C-M-., M-.'
(define-key icicle-toggle-map "_" 'icicle-dispatch-M-_) ; M-_, `_'
(define-key icicle-toggle-map "`" 'icicle-toggle-literal-replacement) ; C-M-`, ``'
(define-key icicle-toggle-map (icicle-kbd "C-`") 'icicle-toggle-regexp-quote) ; C-`, `C-`'
(define-key icicle-toggle-map ";" 'icicle-toggle-ignoring-comments) ; C-M-; `;'
(define-key icicle-toggle-map "\M-;" 'icicle-toggle-search-replace-common-match) ; M-;, `M-;'
(define-key icicle-toggle-map "~" 'icicle-toggle-search-complementing-domain) ; C-M-~, `~'
(define-key icicle-toggle-map "\M-~" 'icicle-toggle-~-for-home-dir) ; M-~
(define-key icicle-toggle-map (icicle-kbd "pause")
  'icicle-toggle-highlight-historical-candidates) ; C-pause, `pause'
(define-key icicle-toggle-map "\t" 'icicle-toggle-completion-mode-keys) ; TAB

(defvar icicle-toggle-transforming-message "Completion-candidate transformation is now %s"
  "Message used by `icicle-toggle-transforming'.
A format string that includes one `%s', to be replaced by `ON'/`OFF'.")

(defvar icicle-transform-before-sort-p nil
  "Non-nil means transform each multi-completion candidate before sorting.
Bind this to non-nil if you do not want sorting to use the whole
multi-completion.")

(defvar icicle-transform-function nil ; Toggle with `C-$,'.
  "Function used to transform the list of completion candidates.
This is applied to the list of initial candidates.
If this is nil, then no transformation takes place.

You can toggle this at any time from the minibuffer using `C-$,'.

The value is changed by program locally, for use in particular
contexts.  E.g., when you use `C-c C-`' (`icicle-search-generic') in a
`*shell*' buffer, Icicles uses this variable with a value of
`icicle-remove-duplicates', to remove duplicate shell commands from
your input history list.

You can use this variable in your Lisp code to transform the list of
candidates any way you like.  A typical use is to remove duplicates,
by binding it to `icicle-remove-duplicates' or
`icicle-remove-dups-if-extras'.")

(defvar icicle-last-transform-function (or icicle-transform-function
                                           'icicle-remove-duplicates)
  "Local copy of `icicle-transform-function', so we can restore it.")

(defvar icicle-universal-argument-map
  (let ((map                       (make-sparse-keymap))
        (universal-argument-minus
         (and (fboundp 'universal-argument--mode) ; Emacs 24.4+
              `(menu-item "" icicle-negative-argument
                :filter ,(lambda (cmd) (if (integerp prefix-arg) nil cmd))))))
    (cond ((fboundp 'universal-argument-other-key) ; Emacs < 24.4
           (define-key map [t]                         'icicle-universal-argument-other-key)
           (define-key map (vector meta-prefix-char t) 'icicle-universal-argument-other-key)
           (define-key map [switch-frame]              nil))
          (t                            ; Emacs 24.4+
           (define-key map [switch-frame]       (lambda (evt)
                                                  (interactive "e")
                                                  (handle-switch-frame evt)
                                                  (universal-argument--mode)))))
    (define-key map (icicle-kbd "C-u")          'icicle-universal-argument-more)
    (define-key map (icicle-kbd "-")            (if (fboundp 'universal-argument--mode)
                                                    universal-argument-minus ; Emacs 24.4+
                                                  'icicle-universal-argument-minus))
    (define-key map (icicle-kbd "0")            'icicle-digit-argument)
    (define-key map (icicle-kbd "1")            'icicle-digit-argument)
    (define-key map (icicle-kbd "2")            'icicle-digit-argument)
    (define-key map (icicle-kbd "3")            'icicle-digit-argument)
    (define-key map (icicle-kbd "4")            'icicle-digit-argument)
    (define-key map (icicle-kbd "5")            'icicle-digit-argument)
    (define-key map (icicle-kbd "6")            'icicle-digit-argument)
    (define-key map (icicle-kbd "7")            'icicle-digit-argument)
    (define-key map (icicle-kbd "8")            'icicle-digit-argument)
    (define-key map (icicle-kbd "9")            'icicle-digit-argument)
    (define-key map (icicle-kbd "kp-0")         'icicle-digit-argument)
    (define-key map (icicle-kbd "kp-1")         'icicle-digit-argument)
    (define-key map (icicle-kbd "kp-2")         'icicle-digit-argument)
    (define-key map (icicle-kbd "kp-3")         'icicle-digit-argument)
    (define-key map (icicle-kbd "kp-4")         'icicle-digit-argument)
    (define-key map (icicle-kbd "kp-5")         'icicle-digit-argument)
    (define-key map (icicle-kbd "kp-6")         'icicle-digit-argument)
    (define-key map (icicle-kbd "kp-7")         'icicle-digit-argument)
    (define-key map (icicle-kbd "kp-8")         'icicle-digit-argument)
    (define-key map (icicle-kbd "kp-9")         'icicle-digit-argument)
    (define-key map (icicle-kbd "kp-subtract")  (if (fboundp 'universal-argument--mode)
                                                    universal-argument-minus ; Emacs 24.4+
                                                  'icicle-universal-argument-minus))
    map)
  "Keymap used while processing `C-u' during Icicles completion.")

(defvar icicle-use-candidates-only-once-alt-p nil
  "*Non-nil means remove each candidate from the set after using it.
This is similar to `icicle-use-candidates-only-once-flag', but it is
used only for alternative actions (e.g. `C-S-RET').")

(defvar icicle-vardoc-last-initial-cand-set ()
  "Cache for initial set of completion candidates for `icicle-vardoc'.")

(defvar icicle-variable-name-history nil "History for variable names.")

(defvar icicle-whole-candidate-as-text-prop-p nil
  "Non-nil means string candidate has candidate data as text property.
If non-nil, then the value of text property `icicle-whole-candidate'
for a string completion candidate (e.g. what is displayed) is the cdr
of the full completion-candidate alist element.  The car of that
element is the string.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-var)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-var.el ends here

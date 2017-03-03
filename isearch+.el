;;; isearch+.el --- Extensions to `isearch.el' (incremental search).
;;
;; Filename: isearch+.el
;; Description: Extensions to `isearch.el' (incremental search).
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2017, Drew Adams, all rights reserved.
;; Created: Fri Dec 15 10:44:14 1995
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Fri Mar  3 15:03:38 2017 (-0800)
;;           By: dradams
;;     Update #: 5731
;; URL: https://www.emacswiki.org/emacs/download/isearch%2b.el
;; Doc URL: http://www.emacswiki.org/IsearchPlus
;; Doc URL: http://www.emacswiki.org/DynamicIsearchFiltering
;; Keywords: help, matching, internal, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `cl', `cl-lib', `color', `frame-fns', `gv', `help-fns',
;;   `hexrgb', `isearch-prop', `macroexp', `misc-cmds', `misc-fns',
;;   `strings', `thingatpt', `thingatpt+', `zones'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `isearch.el' (incremental search).
;;
;;  The Isearch+ libraries are these:
;;
;;  `isearch+.el' (this file)    - Various extensions to `isearch.el'.
;;                                 Can be used with Emacs 20 or later.
;;  `isearch-prop.el' (optional) - Commands to search within contexts,
;;                                 which are character-property zones:
;;                                 spans of text that have certain
;;                                 text or overlay properties.  Can be
;;                                 Used with Emacs 23 or later.
;;
;;  You can use either of the Isearch+ files without the other, but I
;;  recommend that you use them together.
;;
;;
;;  This file should be loaded *AFTER* loading the standard GNU file
;;  `isearch.el'.  So in your `~/.emacs' file, do this:
;;
;;  (eval-after-load "isearch" '(require 'isearch+))
;;
;;  Library `isearch-prop.el' is optional.  If you do not want to use
;;  it then do not put it in your `load-path'.  If it is in your
;;  `load-path' then it will automatically be loaded when you load
;;  library `isearch+.el'.
;;
;;  More description below - see Overview of Features.
;;
;;
;;  Index
;;  -----
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://www.emacswiki.org/emacs/download/linkd.el.
;;
;;  (@> "Overview of Features")
;;  (@> "Change log")
;;  (@> "Faces and Variables")
;;  (@> "Keys and Hooks")
;;  (@> "Macros")
;;  (@> "Commands")
;;  (@> "Non-Interactive Functions")
;;
;;
;;  Commands defined here:
;;
;;    `isearchp-act-on-demand' (Emacs 22+),
;;    `isearchp-add-filter-predicate' (Emacs 24.4+),
;;    `isearchp-add-inline-regexp-filter-predicate' (Emacs 24.4+),
;;    `isearchp-add-regexp-filter-predicate' (Emacs 24.4+),
;;    `isearchp-append-register',
;;    `isearchp-bookmark-current-filter-predicate' (Emacs 24.4+),
;;    `isearch-char-by-name' (Emacs 23-24.3), `isearchp-columns'
;;    (Emacs 24.4+), `isearchp-complement-filter' (Emacs 24.4+),
;;    `isearchp-complete', `isearchp-cycle-mismatch-removal',
;;    `isearchp-defun-filter-predicate' (Emacs 24.4+),
;;    `isearchp-describe-prefix-bindings',
;;    `isearchp-eval-sexp-and-insert' (Emacs 22+),
;;    `isearchp-fontify-buffer-now', `isearchp-init-edit',
;;    `isearchp-keep-filter-predicate' (Emacs 24.4+), `isearchp-near'
;;    (Emacs 24.4+), `isearchp-near-after' (Emacs 24.4+),
;;    `isearchp-near-before' (Emacs 24.4+),
;;    `isearchp-negate-last-filter' (Emacs 24.4+),
;;    `isearchp-open-recursive-edit' (Emacs 22+),
;;    `isearchp-or-filter-predicate' (Emacs 24.4+),
;;    `isearchp-or-last-filter' (Emacs 24.4+),
;;    `isearchp-remove-failed-part' (Emacs 22+),
;;    `isearchp-remove-failed-part-or-last-char' (Emacs 22+),
;;    `isearchp-remove-filter-predicate' (Emacs 24.4+),
;;    `isearchp-reset-filter-predicate' (Emacs 24.4+),
;;    `isearchp-reset-filter-preds-alist' (Emacs 24.4+),
;;    `isearchp-retrieve-last-quit-search',
;;    `isearchp-set-filter-predicate' (Emacs 24.4+),
;;    `isearchp-set-region-around-search-target',
;;    `isearchp-show-filters' (Emacs 24.4+),
;;    `isearchp-toggle-auto-keep-filter-predicate' (Emacs 24.4+),
;;    `isearchp-toggle-dimming-filter-failures' (Emacs 24.4+),
;;    `isearchp-toggle-highlighting-regexp-groups',
;;    `isearchp-toggle-lazy-highlight-cleanup' (Emacs 22+),
;;    `isearchp-toggle-lazy-highlighting' (Emacs 22+),
;;    `isearchp-toggle-literal-replacement' (Emacs 22+),
;;    `isearchp-toggle-option-toggle',
;;    `isearchp-toggle-regexp-quote-yank',
;;    `isearchp-toggle-repeat-search-if-fail' (Emacs 22+),
;;    `isearchp-toggle-search-invisible',
;;    `isearchp-toggle-set-region',
;;    `isearchp-toggle-symmetric-char-fold' (Emacs 25+),
;;    `isearchp-yank-char' (Emacs 22+), `isearchp-yank-line' (Emacs
;;    22+), `isearchp-yank-sexp-symbol-or-char' (Emacs 22+),
;;    `isearchp-yank-sexp-symbol-or-char-1' (Emacs 22+),
;;    `isearchp-yank-symbol-or-char' (Emacs 22+),
;;    `isearchp-yank-symbol-or-char-1' (Emacs 22+),
;;    `isearchp-yank-word-or-char' (Emacs 22+).
;;
;;  User options defined here:
;;
;;    `isearchp-auto-keep-filter-predicate-flag' (Emacs 22+),
;;    `isearchp-case-fold', `isearchp-deactivate-region-flag' (Emacs
;;    24.3+), `isearchp-drop-mismatch',
;;    `isearchp-drop-mismatch-regexp-flag',
;;    `isearchp-filter-predicates-alist' (Emacs 24.4+),
;;    `isearchp-highlight-regexp-group-levels-flag' (Emacs 24.4+),
;;    `isearchp-initiate-edit-commands' (Emacs 22+),
;;    `isearchp-lazy-dim-filter-failures-flag' (Emacs 24.4+),
;;    `isearchp-mouse-2-flag', `isearchp-movement-unit-alist' (Emacs
;;    24.4+), `isearchp-on-demand-action-function' (Emacs 22+),
;;    `isearchp-prompt-for-filter-name' (Emacs 24.4+),
;;    `isearchp-regexp-quote-yank-flag',
;;    `isearchp-repeat-search-if-fail-flag' (Emacs 22+),
;;    `isearchp-restrict-to-region-flag' (Emacs 24.3+),
;;    `isearchp-resume-with-last-when-empty-flag' (Emacs 22+),
;;    `isearchp-ring-bell-function', `isearchp-set-region-flag',
;;    `isearchp-toggle-option-flag',
;;    `isearchp-update-filter-predicates-alist-flag' (Emacs 24.4+).
;;
;;  Faces defined here:
;;
;;    `isearch-fail', `isearchp-lazy-odd-regexp-groups' (Emacs 24.4+),
;;    `isearchp-multi', `isearchp-overwrapped', `isearchp-regexp',
;;    `isearchp-regexp-level-1' (Emacs 24.4+),
;;    `isearchp-regexp-level-2' (Emacs 24.4+),
;;    `isearchp-regexp-level-3' (Emacs 24.4+),
;;    `isearchp-regexp-level-4' (Emacs 24.4+),
;;    `isearchp-regexp-level-5' (Emacs 24.4+),
;;    `isearchp-regexp-level-6' (Emacs 24.4+),
;;    `isearchp-regexp-level-7' (Emacs 24.4+),
;;    `isearchp-regexp-level-8' (Emacs 24.4+), `isearchp-word',
;;    `isearchp-wrapped'.
;;
;;  Macros defined here:
;;
;;    `isearchp-user-error'.
;;
;;  Non-interactive functions defined here:
;;
;;    `isearchp-add-filter-predicate-1' (Emacs 24.4+),
;;    `isearchp-assoc-delete-all', `isearchp-barf-if-use-minibuffer',
;;    `isearchp-columns-p' (Emacs 24.4+),
;;    `isearchp-complete-past-string',
;;    `isearchp-current-filter-predicates' (Emacs 24.4+),
;;    `isearchp-fail-pos', `isearchp-ffap-guesser' (Emacs 24.4+),
;;    `isearchp-filter-bookmark-alist-only' (Emacs 24.4+),
;;    `isearchp-filter-bookmark-p' (Emacs 24.4+),
;;    `isearchp-filters-description' (Emacs 24.4+),
;;    `isearchp-first-isearch-advice' (Emacs 24.4+),
;;    `isearchp-highlight-lighter', `isearchp-in-color-p' (Emacs
;;    24.4+), `isearchp-in-comment-p' (Emacs 24.4+),
;;    `isearchp-in-comment-or-delim-p' (Emacs 24.4+),
;;    `isearchp-in-decimal-number-p' (Emacs 24.4+),
;;    `isearchp-in-defun-p' (Emacs 24.4+),
;;    `isearchp-in-email-address-p' (Emacs 24.4+),
;;    `isearchp-in-file-name-p' (Emacs 24.4+),
;;    `isearchp-in-file-or-url-p' (Emacs 24.4+),
;;    `isearchp-in-hex-number-p' (Emacs 24.4+), `isearchp-in-line-p'
;;    (Emacs 24.4+), `isearchp-in-lisp-variable-p' (Emacs 24.4+),
;;    `isearchp-in-list-p' (Emacs 24.4+), `isearchp-in-number-p'
;;    (Emacs 24.4+), `isearchp-in-page-p' (Emacs 24.4+),
;;    `isearchp-in-paragraph-p' (Emacs 24.4+),
;;    `isearchp-in-sentence-p' (Emacs 24.4+), `isearchp-in-sexp-p'
;;    (Emacs 24.4+), `isearchp-in-string-or-comment-p' (Emacs 24.4+),
;;    `isearchp-in-string-p' (Emacs 24.4+), `isearchp-in-symbol-p'
;;    (Emacs 24.4+), `isearchp-in-url-p' (Emacs 24.4+),
;;    `isearchp-in-word-p' (Emacs 24.4+),
;;    `isearchp-last-isearch-advice' (Emacs 24.4+),
;;    `isearchp-match-regexp-filter-predicate' (Emacs 24.4+),
;;    `isearchp-message-prefix', `isearchp-message-suffix',
;;    `isearchp-near-after-predicate' (Emacs 24.4+),
;;    `isearchp-near-before-predicate' (Emacs 24.4+),
;;    `isearchp-near-predicate' (Emacs 24.4+), `isearchp-not-pred'
;;    (Emacs 24.4+), `isearchp-not-predicate' (Emacs 24.4+),
;;    `isearchp-oddp', `isearchp-or-predicates' (Emacs 24.4+),
;;    `isearchp-or-preds' (Emacs 24.4+), `isearchp-read-face-names',
;;    `isearchp-read-face-names--read', `isearchp-read-filter-name'
;;    (Emacs 24.4+), `isearchp-read-measure' (Emacs 24.4+),
;;    `isearchp-read-near-args' (Emacs 24.4+),
;;    `isearchp-read-predicate' (Emacs 24.4+),
;;    `isearchp-read-prompt-prefix' (Emacs 24.4+),
;;    `isearchp-read-regexp-during-search' (Emacs 24.4+),
;;    `isearchp-read-sexps', `isearchp-redo-lazy-highlighting' (Emacs
;;    24.4+), `isearchp-remove-duplicates',
;;    `isearchp-remove-mismatch', `isearchp-repeat-command',
;;    `isearchp-repeat-search-if-fail' (Emacs 22+),
;;    `isearchp-replace-fixed-case-p' (Emacs 22+),
;;    `isearchp-replace-match' (Emacs 22+),
;;    `isearchp-replace-multiple' (Emacs 22+),
;;    `isearchp-replace-on-demand' (Emacs 22+),
;;    `isearchp-reset-noprompt-action-fn', `isearchp-set-region',
;;    `isearchp-set-sel-and-yank', `isearchp-show-hit-w-crosshairs'
;;    (Emacs 24.4+), `isearchp-update-edit-init-commands' (Emacs 22+).
;;
;;  Internal variables defined here:
;;
;;    `isearchp-current-filter-preds-alist' (Emacs 24.4+),
;;    `isearchp-ffap-max-region-size' (Emacs 24.4+),
;;    `isearchp-filter-map' (Emacs 24.4+),
;;    `isearchp-in-lazy-highlight-update-p' (Emacs 24.3+),
;;    `isearchp-kept-filter-predicate' (Emacs 24.4+),
;;    `isearchp-last-non-nil-invisible',
;;    `isearchp-last-quit-regexp-search', `isearchp-last-quit-search',
;;    `isearchp-lazy-highlight-face' (Emacs 22+),
;;    `isearchp-lazy-regexp-level-overlays' (Emacs 24.4+),
;;    `isearchp-nomodify-action-hook' (Emacs 22+),
;;    `isearchp-noprompt-action-function',
;;    `isearchp-orig-ring-bell-fn', `isearchp-pref-arg',
;;    `isearchp-reg-beg', `isearchp-reg-end',
;;    `isearchp-regexp-level-overlays' (Emacs 24.4+),
;;    `isearchp-replace-literally' (Emacs 22+), `isearchp-replacement'
;;    (Emacs 22+), `isearchp--replacing-on-demand' (Emacs 22+),
;;    `isearch-update-post-hook' (Emacs 20-21),
;;    `isearchp-user-entered-new-filter-p' (Emacs 24.4+),
;;    `isearchp-win-pt-line'.
;;
;;
;;  ***** NOTE: The following macros and functions defined in
;;              `isearch.el' have been REDEFINED OR ADVISED HERE:
;;
;;  `isearch-abort'       - Save search string when `C-g'.
;;  `isearch-backward', `isearch-backward-regexp' -
;;                          Prefix arg can  `multi-isearch-buffers'.
;;  `isearch-cancel'      - Restore cursor position relative to window.
;;  `isearch-dehighlight' - Delete regexp-group level overlays too.
;;                          Added unused arg, for Emacs 20.
;;  `isearch--describe-word-mode' - Face `isearchp-word' on string.
;;  `isearch-done'        - Restore/update `isearch-filter-predicate'.
;;                          Reset `ring-bell-function'.
;;  `isearch-edit-string' - Put point at mismatch position.
;;  `isearch-forward', `isearch-forward-regexp' -
;;                          Prefix arg can  `multi-isearch-buffers'.
;;  `isearch-highlight'   - Highlight also regexp-group levels.
;;  `lazy-highlight-cleanup' - Delete lazy regexp overlays. (24.4+)
;;  `isearch-lazy-highlight-search' - Can limit to region (24.3+)
;;  `isearch-lazy-highlight-update' - Can limit to region (24.3+)
;;  `isearch-mode'        - Save cursor position relative to window.
;;  `isearch-mode-help'   - End isearch.  List bindings.
;;  `isearch-message'     - Highlight failed part of search string in
;;                          echo area, in face `isearch-fail'.
;;  `isearch-message-prefix' - Highlight prompt keywords: wrapped,
;;                          regexp, word, multi.  Highlight filter
;;                          prefixes, and reverse their order.
;;  `isearch-mouse-2'     - Respect `isearchp-mouse-2-flag'(Emacs 21+)
;;  `isearch-search'      - Can limit to active region (Emacs 24.3+)
;;  `isearch-repeat'      - Can limit to active region (Emacs 24.3+)
;;  `isearch-printing-char' - Respect option `isearchp-drop-mismatch'
;;  `isearch-toggle-case-fold' - Respect `isearchp-toggle-option-flag'
;;                               Show case sensitivity in mode-line.
;;                               Message.
;;  `isearch-toggle-invisible' - Respect `isearchp-toggle-option-flag'
;;                               Message.
;;  `isearch-toggle-word' - Message, and turn off regexp search.
;;  `isearch-update' - Run `isearch-update-post-hook' (Emacs 20-21).
;;                   - Run `isearchp-noprompt-action-function' and
;;                     `isearchp-nomodify-action-hook' (Emacs 22+).
;;  `isearch-yank-string' - Respect `isearchp-regexp-quote-yank-flag'.
;;  `with-isearch-suspended' - Add `catch': update `isearch-success'.
;;
;;
;;  ***** NOTE: The following internal variables defined in
;;              `isearch.el' have been REDEFINED HERE:
;;
;;  `isearch-invisible'   - defined for Emacs<24.4 & added doc string.
;;
;;
;;  ***** NOTE: The following function defined in `misearch.el' has
;;              been ADVISED HERE:
;;
;;  `multi-isearch-end'    - Fix for bug #20234: reset buffer list.
;;
;;
;;  Keys bound in `isearch-mode-map' here, by default.  Some are from
;;  `isearch-prop.el'.  Keys bound to `isearchp-init-edit' by default
;;  are controlled by option `isearchp-initiate-edit-commands'.
;;
;;    `C-`'        `isearchp-toggle-regexp-quote-yank'
;;    `C-+'        `isearchp-toggle-search-invisible'
;;    `C-_'        `isearchp-yank-symbol-or-char' (Emacs 22+)
;;    `C-('        `isearchp-yank-sexp-symbol-or-char' (Emacs 22+)
;;    `C-backspace' `isearchp-remove-failed-part-or-last-char'
;;                  (Emacs 22+)
;;    `C-end'      `goto-longest-line' (requires `misc-cmds.el')
;;    `C-left'     `isearchp-init-edit' (Emacs 22+)
;;    `C-b'        `isearchp-init-edit' (Emacs 22+)
;;    `C-h'        `isearch-mode-help'
;;    `C-t'        `isearchp-property-forward' (Emacs 23+)
;;                 (`isearch-prop.el')
;;    `C-x n'      `isearchp-toggle-region-restriction' (Emacs 24.3+)
;;    `C-x o'      `isearchp-open-recursive-edit' (Emacs 22+)
;;    `C-x r g'    `isearchp-append-register'
;;    `C-x 8 RET'  `isearch-char-by-name' (Emacs 23-24.3)
;;    `C-y C-_'    `isearchp-yank-symbol-or-char' (Emacs 22+)
;;    `C-y C-('    `isearchp-yank-sexp-symbol-or-char' (Emacs 22+)
;;    `C-y C-2'    `isearch-yank-secondary' (requires `second-sel.el')
;;    `C-y C-c'    `isearchp-yank-char' (Emacs 22+)
;;    `C-y C-e'    `isearchp-yank-line'
;;    `C-y C-w'    `isearchp-yank-word-or-char' (Emacs 22+)
;;    `C-y C-y'    `isearch-yank-kill'
;;    `C-y M-g'    `isearchp-retrieve-last-quit-search'
;;    `C-y M-y'    `isearch-yank-pop' (Emacs 24+)
;;    `C-z !'      `isearchp-set-filter-predicate' (Emacs 24.4+)
;;    `C-z %'      `isearchp-add-regexp-filter-predicate'
;;                 (Emacs 24.4+)
;;    `C-z .'      `isearchp-add-inline-regexp-filter-predicate'
;;                 (Emacs 24.4+)
;;    `C-z &'      `isearchp-add-filter-predicate' (Emacs 24.4+)
;;    `C-z -'      `isearchp-remove-filter-predicate' (Emacs 24.4+)
;;    `C-z 0'      `isearchp-reset-filter-predicate' (Emacs 24.4+)
;;    `C-z <'      `isearchp-near-before' (Emacs 24.4+)
;;    `C-z >'      `isearchp-near-after' (Emacs 24.4+)
;;    `C-z ?'      `isearchp-show-filters' (Emacs 24.4+)
;;    `C-z @'      `isearchp-near' (Emacs 24.4+)
;;    `C-z b'      `isearchp-bookmark-current-filter-predicate' (Emacs
;;                 24.4+ and requires library Bookmark+)
;;    `C-z c'      `isearchp-columns' (Emacs 24.4+)
;;    `C-z n'      `isearchp-defun-filter-predicate' (Emacs 24.4+)
;;    `C-z p'      `isearchp-toggle-showing-filter-prompt-prefixes'
;;                 (Emacs 24.4+)
;;    `C-z S'      `isearchp-toggle-auto-keep-filter-predicate'
;;                 (Emacs 24.4+)
;;    `C-z s'      `isearchp-keep-filter-predicate' (Emacs 24.4+)
;;    `C-z ||'     `isearchp-or-filter-predicate' (Emacs 24.4+)
;;    `C-z |1'     `isearchp-or-last-filter' (Emacs 24.4+)
;;    `C-z ~~'     `isearchp-complement-filter' (Emacs 24.4+)
;;    `C-z ~1'     `isearchp-negate-last-filter' (Emacs 24.4+)
;;    `C-M-;'      `isearchp-toggle-ignoring-comments' (Emacs 23+)
;;                 (`isearch-prop.el')
;;    `C-M-`'      `isearchp-toggle-literal-replacement' (Emacs 22+)
;;    `C-M-~'      `isearchp-toggle-complementing-domain' (Emacs 23+)
;;                 (`isearch-prop.el')
;;    `C-M-RET'    `isearchp-act-on-demand' (Emacs 22+)
;;    `C-M-tab'    `isearchp-complete' (on MS Windows)
;;    `C-M-b'      `isearchp-init-edit' (Emacs 22+)
;;    `C-M-D'      `isearchp-toggle-dimming-outside-search-area'
;;                 (Emacs 23+) (`isearch-prop.el')
;;    `C-M-left'   `isearchp-init-edit' (Emacs 22+)
;;    `C-M-i'      `isearchp-complete'
;;    `C-M-l'      `isearchp-remove-failed-part' (Emacs 22+)
;;    `C-M-t'      `isearchp-property-forward-regexp' (Emacs 23+)
;;    `C-M-y'      `isearch-yank-secondary' (requires `second-sel.el')
;;    `C-S-SPC'    `isearchp-narrow-to-lazy-highlights' (Emacs 23+)
;;                 (`isearch-prop.el')
;;    `M-:'        `isearchp-eval-sexp-and-insert' (Emacs 22+)
;;    `M-;'        `isearchp-toggle-hiding-comments' (Emacs 23+)
;;                 (`isearch-prop.el')
;;    `M-left'   `isearchp-init-edit' (Emacs 22+)
;;    `M-b'        `isearchp-init-edit' (Emacs 22+)
;;    `M-c'        `isearch-toggle-case-fold'
;;    `M-e'        `isearch-edit-string'
;;    `M-g'        `isearchp-retrieve-last-quit-search'
;;    `M-k'        `isearchp-cycle-mismatch-removal'
;;    `M-r'        `isearch-toggle-regexp'
;;    `M-w'        `isearchp-kill-ring-save'
;;    `M-s C-e'    `isearchp-yank-line'
;;    `M-s ='      `isearchp-toggle-symmetric-char-fold' (Emacs 25+)
;;    `M-s h d'    `isearchp-toggle-dimming-filter-failures'
;;                 (Emacs 24.4+)
;;    `M-s h l'    `isearchp-toggle-lazy-highlight-cleanup' (Emacs 22+)
;;    `M-s h L'    `isearchp-toggle-lazy-highlighting' (Emacs 22+)
;;    `M-s h R'    `isearchp-toggle-highlighting-regexp-groups'
;;    `M-s i'      `isearch-toggle-invisible'
;;    `M-s v'      `isearchp-toggle-option-toggle'
;;    `M-TAB'      `isearchp-complete'
;;    `M-s M-SPC'  `isearchp-toggle-set-region'
;;    `M-s M-k'    `isearchp-toggle-repeat-search-if-fail' (Emacs 22+)
;;    `M-s h '     `isearchp-toggle-lazy-highlighting'
;;    `M-S-delete' `isearchp-cleanup'  (Emacs 23+) (`isearch-prop.el')
;;    `left'       `isearchp-init-edit' (Emacs 22+)
;;    `mouse-2'    `isearch-mouse-2'
;;    `next'       `isearch-repeat-forward'
;;    `prior'      `isearch-repeat-backward'
;;
;;
;;  User option `isearchp-initiate-edit-commands' causes certain keys
;;  not to exit Isearch but rather to edit the search string.
;;  Customize it to `nil' if you do not want this behavior at all.
;;
;;
;;  The following bindings are made here for incremental search edit
;;  mode:
;;
;;    `C-x 8 RET'  `insert-char' (Emacs 23+)
;;    `C-M-tab'    `isearch-complete-edit' (MS Windows only)
 
;;(@* "Overview of Features")
;;
;;; Overview of Features ---------------------------------------------
;;
;;  * Dynamic search filtering (starting with Emacs 24.4).
;;
;;    You can add and remove any number of search filters while
;;    searching incrementally.  See
;;    https://www.emacswiki.org/emacs/DynamicIsearchFiltering.
;;
;;    The predicate that is the value of `isearch-filter-predicate' is
;;    advised by additional predicates that you add, creating a
;;    complex suite of predicates that act together.
;;
;;    Reminder: An Isearch filter predicate is a function that accepts
;;    two buffer positions, BEG and END, as its first two arguments.
;;    These values are the beginning and ending positions of a search
;;    hit.  If the return value of the function is `nil' then the
;;    search hit is excluded from searching; otherwise it is included.
;;
;;    The value of standard variable (but not a user option)
;;    `isearch-filter-predicate' is the filter predicate used by
;;    Isearch.  By default, the value is predicate
;;    `isearch-filter-visible', which returns non-`nil' for any search
;;    hit that is visible (not rendered invisible by a text property,
;;    overlay property, etc.)
;;
;;    If you search the Emacs Lisp source code, you will find only two
;;    uses, so far, of variable `isearch-filter-predicate', even
;;    though such filtering has been around since Emacs 23.  It’s
;;    hardly ever used.  Why?
;;
;;    Because it’s not so easy to use, out of the box.  And it’s not
;;    thought of as a way to *refine* searches, but rather as a way to
;;    *wall off* certain areas from searching.
;;
;;    Yes, those are in fact the same thing, but I don’t think people
;;    think this way ... because Isearch does not make it particularly
;;    easy to use filters.  Isearch+ tries to do that, to let you
;;    refine searches by adding filters incrementally.
;;
;;    The idea is simple: Isearch+ defines some keys that prompt you
;;    for a filter.  You can enter any filter predicates at the
;;    prompts.  There are also some predefined predicates that you can
;;    choose from, using completion.  You can combine predicates using
;;    AND, OR, and NOT.
;;
;;    A filter predicate does essentially the same thing as the search
;;    pattern that you type at the Isearch prompt.  Each restricts the
;;    search space (the buffer text) to certain zones: those that
;;    satisfy the predicate and those that match the search pattern.
;;
;;    But a predicate can be much more general than is the predefined
;;    pattern-matching provided by Emacs Isearch.  Suppose that you
;;    want to find lines of text that contain `cat', `dog', and
;;    `turtle'.  There is no simple search pattern that lets you do
;;    this.  A regexp would need to explicitly express each possible
;;    order, and there are 6 of them - not so simple.
;;
;;    But a predicate can just check each line for `cat' AND check for
;;    `dog' AND check for `turtle'.  It is usually much easier to
;;    combine simple patterns than it is to come up with a complex
;;    pattern that does the same thing.  And the way to combine
;;    patterns in Emacs Isearch is to use one or more filter
;;    predicates.
;;
;;    A filter predicate can even perform side effects, if you like.
;;    Only the return value is used by Isearch.  For example, if you
;;    wanted to more easily see the cursor position each time search
;;    stops at a search hit, you could use something like this as a
;;    filter predicate.  (This requires library `crosshairs.el', which
;;    highlights the current column and line using crosshairs.)
;;
;;      (lambda (beg end)
;;        (save-excursion (goto-char end)) ; Go to end of search hit.
;;        ;; Avoid calling `crosshairs' when inside
;;        ;; `isearch-lazy-highlight-search'.
;;        (unless isearchp-in-lazy-highlight-update-p (crosshairs))
;;        t)  ; Return non-nil always - no real filtering.
;;
;;    The side-effect-producing call to function `crosshairs' is
;;    guarded by variable `isearchp-in-lazy-highlight-update-p' here,
;;    so that it is invoked only when the cursor is moved to a search
;;    hit, not also when lazy highlighting is performed.  (Filtering
;;    applies also to lazy highlighting: it filters out search hits
;;    that are not being used.  But in this case no real filtering is
;;    done, and there is no need to show crosshairs moving across the
;;    buffer during lazy highlighting.)
;;
;;    (You can choose that crosshairs-showing filter predicate by the
;;    name `crosshairs' when prompted for a predicate.  It corresponds
;;    to predicate `isearchp-show-hit-w-crosshairs'.)
;;
;;    The following filtering commands are available during Isearch.
;;    They are all on prefix key `C-z', by default.  They are on
;;    prefix keymap `isearchp-filter-map', which you can bind to any
;;    key in `isearch-mode-map'.  If you forget a `C-z' key, you can
;;    use `C-z C-h' while searching to show them all.
;;
;;    - `C-z &' (`isearchp-add-filter-predicate') adds a filter
;;      predicate, AND-ing it as an additional `:after-while' filter.
;;
;;    - `C-z %' (`isearchp-add-regexp-filter-predicate') adds a filter
;;      predicate that requires search hits to match a given regexp.
;;
;;    - `C-z .' (`isearchp-add-inline-regexp-filter-predicate') is
;;      really just `C-z %', but `.*' is added to each side of the
;;      regexp you enter.  You can use this multiple times when regexp
;;      searching for full lines with `.+', to find the lines that
;;      contain multiple regexp matches in any order.
;;
;;    - `C-z ||' (`isearchp-or-filter-predicate') adds a filter
;;      predicate, OR-ing it as an additional `:before-until' filter.
;;
;;    - `C-z |1' (`isearchp-or-last-filter') replaces the last-added
;;      filter by its disjunction with another predicate, which you
;;      specify.
;;
;;    - `C-z ~~' (`isearchp-complement-filter') complements the current
;;      filter.  It either adds an `:around' filter that complements
;;      or it removes an existing top-level complementing filter.
;;
;;    - `C-z ~1' (`isearchp-negate-last-filter') replaces the
;;      last-added filter by its complement.
;;
;;    - `C-z -' (`isearchp-remove-filter-predicate') removes a filter
;;      predicate that you specify, using completion.  The last-added
;;      is the default - retrieve it using `M-n'.
;;
;;    - `C-z !' (`isearchp-set-filter-predicate') sets the overall
;;      filter predicate (advised `isearch-filter-predicate') to a
;;      single predicate.
;;
;;    - `C-z 0' (`isearchp-reset-filter-predicate') resets
;;      `isearch-filter-predicate' to its original (default) value.
;;
;;    - `C-z b' (`isearchp-bookmark-current-filter-predicate')
;;      bookmarks the current value of `isearch-filter-predicate',
;;      persisting it for reuse in future Emacs sessions.  You need
;;      library Bookmark+ to be able to use this.
;;
;;    - `C-z c' (`isearchp-columns') adds a filter predicate that
;;      limits search between two columns (or before/after a column).
;;
;;    - `C-z n' (`isearchp-defun-filter-predicate') names the current
;;      suite of filter predicates, creating a named predicate that
;;      does the same thing.  With a prefix arg it can also set or
;;      keep it (for this Emacs session) - that is, do what `C-z !' or
;;      `C-z s' does.
;;
;;      You can use that name with `C-z -' to remove that predicate.
;;      You can also use it to create a custom Isearch command that
;;      uses it for filtering.  For example:
;;
;;        (defun foo ()
;;          "Isearch with filter predicate `my-filter-pred'."
;;          (interactive)
;;          (let ((isearch-filter-predicate  'my-filter-pred))
;;            (isearch-forward)))
;;
;;    - `C-z p' (`isearchp-toggle-showing-filter-prompt-prefixes')
;;      toggles option `isearchp-show-filter-prompt-prefixes-flag',
;;      which controls whether to show filter prefixes in the Isearch
;;      prompt.
;;
;;    - `C-z s' (`isearchp-keep-filter-predicate') keeps the current
;;      filter-predicate suite for subsequent searches (in this Emacs
;;      session only).  Unless you do this (and unless auto-keeping is
;;      turned on), the next Isearch starts out from scratch, using
;;      the default value of `isearch-filter-predicate'.  (To remove
;;      the kept predicate suite, use `C-z 0'.)
;;
;;    - `C-z S' (uppercase `s')
;;      (`isearchp-toggle-auto-keep-filter-predicate') toggles option
;;      `isearchp-auto-keep-filter-predicate-flag', which
;;      automatically keeps the current filter-predicate suite, so
;;      that it is used for subsequent searches (so no need to use
;;      `C-z s').  (To remove a kept predicate suite, use `C-z 0'.)
;;
;;    - `C-z ?' (`isearchp-show-filters') echoes the current suite of
;;      filter predicates (advice and original, unadvised predicate).
;;
;;    - `C-z @', `C-z <', and `C-z >' (`isearchp-near',
;;      `isearchp-near-before', and `isearchp-near-after') constrain
;;      searching to be within a given distance of (near) another
;;      search pattern.  For example, you can limit search hits to
;;      those whose end (or beginning, if searching backward) is
;;      within, say, 4 words of another search pattern.  You are
;;      prompted for the search pattern for the nearby text, the
;;      "near" distance, and the unit of distance measurement
;;      (default: characters).  You can define the list of acceptable
;;      units by customizing option `isearchp-movement-unit-alist'.
;;      The default option value includes units character, word, sexp,
;;      list, and sentence.
;;
;;      You can also use functions `isearch-near-predicate',
;;      `isearchp-near-before-predicate', and
;;      `isearchp-near-before-predicate' to define your own nearness
;;      predicates, which incorporate particular patterns and
;;      distances. You can then simply add such a predicate using `C-z
;;      &' (no prompting for pattern or distance).
;;
;;    Typically you add (`C-z &', `C-z %', etc.) a filter predicate to
;;    those already active, or you remove one (`C-z -').  Adding is
;;    implicitly an AND operation: the list of current predicates must
;;    all be satisfied.  You can also OR a predicate against either
;;    the entire ANDed list of predicates (`C-z ||') or against only
;;    the last-added one (`C-z |1').  And you can complement either
;;    the entire ANDed list (`C-z ~~') or just the last-added
;;    predicate (`C-z ~1').
;;
;;    This ORing and NOTing, together with adding and removing
;;    predicates in a given order (implicitly ANDing them), gives you
;;    complete Boolean combination flexibility.
;;
;;    The list of filter predicates is always a conjunction.  But you
;;    can use, as any of the conjuncts, a predicate that implements a
;;    disjunction or a negation.  Or you can replace the entire list
;;    by a single predicate that implements a disjunction or a
;;    negation.
;;
;;    When you use one of the commands that adds a filter predicate as
;;    advice to `isearch-filter-predicate' you can be prompted for two
;;    things: (1) a short name for the predicate and (2) text to add
;;    to the Isearch prompt as a reminder of filtering.  The optional
;;    short name is a convenience for referring to the predicate - for
;;    adding it again or removing it, for example.
;;
;;    Two user options control this prompting:
;;
;;    - `isearchp-prompt-for-filter-name' says whether to prompt you
;;      always, never, or only when the predicate that you provide is
;;      not a symbol (it is a lambda form).  The last of these is the
;;      default behavior.  If you are prompted and provide a name, you
;;      can use that name with `C-z -' to remove that predicate.
;;
;;    - `isearchp-prompt-for-prompt-prefix-flag' says whether to
;;      prompt you for a prefix to add to the Isearch prompt.  You are
;;      prompted by default, but if you don't care to see such a
;;      prompt prefix and you don't want to be bothered by it, you can
;;      customize this to skip prompting.
;;
;;    In addition, whatever the value of these options, when you add a
;;    filter predicate you can override the option values by using a
;;    prefix argument.  A non-positive prefix arg overrides the option
;;    for name prompting, and a non-negative prefix arg overrides the
;;    option for prompt-prefix prompting.  (So zero, e.g., `M-0',
;;    overrides both.)
;;
;;    Option `isearchp-show-filter-prompt-prefixes-flag' controls
;;    whether prefixes for filters are added to the Isearch prompt.
;;    You can toggle this option during search using `C-z p'.
;;
;;    User option `isearchp-filter-predicates-alist' contains filter
;;    predicates that are available as completion candidates whenever
;;    you are prompted for one.  This is an important option.  The
;;    alist entries can be of several forms, which affect the behavior
;;    differently.
;;
;;    In particular, instead of choosing a filter predicate as a
;;    completion candidate, you can choose a function that creates and
;;    returns a filter predicate, after prompting you for some more
;;    information.
;;
;;    This is the case, for example, for function
;;    `isearchp-near-before-predicate'.  It is used in the predefined
;;    alist entry `("near<..."  isearchp-near-before-predicate)',
;;    which associates the short name `near<...', as a completion
;;    candidate, with the function.
;;
;;    When you choose this candidate, function
;;    `isearchp-near-before-predicate' prompts you for another pattern
;;    for Isearch to match, a max number of units of nearness, and
;;    which units to measure with.  It constructs and returns a
;;    predicate that checks those match parameters.  As usual, you can
;;    be prompted for a short name and an Isearch prompt prefix to
;;    associate with the newly defined predicate, so that you can
;;    easily choose it again (no prompting).
;;
;;    Similarly, candidate `not...' prompts you for a predicate to
;;    negate, and candidate `or...' prompts you for two predicates to
;;    combine using `or'.
;;
;;    For the completion candidates that are predefined, this
;;    naming convention is used:
;;
;;    * Bracketed names (`[...]') stand for predicates that check that
;;      the search hit is within something.  For example, name `[;]'
;;      tests whether it is inside a comment (`;' is the Emacs-Lisp
;;      comment-start character), and name `[defun]' tests whether it
;;      is inside a defun.
;;
;;    * Names that end in `...' indicate candidates that prompt you
;;      for more information.  These names represent, not filter
;;      predicates, but functions that return filter predicates.  For
;;      example, `near<...' stands for function
;;      `isearchp-near-before-predicate' (see above).
;;
;;    Filter predicates that you add dynamically are added as
;;    completion candidates for the current Emacs session.  If option
;;    `isearchp-update-filter-predicates-alist-flag' is non-`nil' then
;;    they are also added to `isearchp-filter-predicates-alist'.  That
;;    updated option value is NOT SAVED, however.  If you want to save
;;    your additions to it for future Emacs sessions then use
;;    `M-x customize-option isearchp-filter-predicates-alist'.
;;
;;    You can use command `isearchp-reset-filter-preds-alist' (not
;;    bound) to reset the filter predicates available for completion
;;    to those in option `isearchp-filter-predicates-alist'.  A prefix
;;    arg with `C-z 0' also resets this, along with resetting to the
;;    unadvised value of `isearch-filter-predicate'.
;;
;;    If option `isearchp-lazy-dim-filter-failures-flag' is non-`nil'
;;    then search hits that are skipped because they are removed by
;;    filtering are nevertheless lazy-highlighted, but using a face
;;    that dims the background.  You can toggle this highlighting of
;;    filter-failure search hits using `M-s h d' (command
;;    `isearchp-toggle-dimming-filter-failures').
;;
;;    The dimming face for this is hard-coded as having background
;;    color #9abfca, unless you also use library `isearch-prop.el'
;;    (recommended).  If you use `isearch-prop.el' then you can
;;    control the dimming color using option `isearchp-dimming-color'.
;;    It specifies a given background color to use always, or it
;;    specifies that the current background color is to be dimmed a
;;    given amount.
;;
;;  * Case-sensitivity is indicated in the mode line minor-mode
;;    lighter: `ISEARCH' for case-insensitive; `Isearch' for
;;    case-sensitive.
;;
;;  * Optional highlighting of the first eight regexp-group levels in
;;    the current search hit, controlled by option
;;    `isearchp-highlight-regexp-group-levels-flag'.  For
;;    lazy-highlighting of other search hits, the odd groups are
;;    highlighted differently from the even groups.  You can toggle
;;    the value using `M-s h R' (command
;;    `isearchp-toggle-highlighting-regexp-groups.')  during Isearch.
;;
;;  * Whether search is literal or regexp is indicated in the mode
;;    line minor-mode lighter: `R*SEARCH' or `R*search', for regexp.
;;
;;  * Highlighting of the mode-line minor-mode lighter when search has
;;    wrapped around or overwrapped.
;;
;;  * Highlighting of parts of the prompt, to indicate the type of
;;    search: regexp, word, multiple-buffer, and whether searching has
;;    wrapped around the buffer (Emacs 22+ only).
;;
;;  * Optional limiting of search to the active region, controlled by
;;    option `isearchp-restrict-to-region-flag'.  Deactivation of the
;;    active region is controlled by option
;;    `isearchp-deactivate-region-flag'.  Both of these are available
;;    for Emacs 24.3 and later.  You can use `C-x n' (command
;;    `isearchp-toggle-region-restriction') during search to toggle
;;    `isearchp-restrict-to-region-flag'.
;;
;;    NOTE: For search to be limited to the active region in Info, you
;;    must also use library `info+.el'.
;;
;;  * Option and commands to let you select the last target occurrence
;;    (set the region around it):
;;
;;    - Option `isearchp-set-region-flag' - Non-`nil' means
;;      automatically set the region around the last search target.
;;    - Command `isearchp-toggle-set-region', bound to `M-s M-SPC'
;;      during isearch - toggle `isearchp-set-region-flag'.
;;    - Command `isearchp-set-region-around-search-target' - manually
;;      set the region around the last search target.
;;
;;  * When you visit a search hit, you can perform an action on it.
;;    Use `C-M-RET' (command `isearchp-act-on-demand' - Emacs 22+
;;    only) to invoke the action function that is the value of option
;;    `isearchp-on-demand-action-function'.  That function is passed
;;    the current search-hit string and its start and end positions in
;;    the buffer.  Search moves to the next hit in the same search
;;    direction, so just repeating `C-M-RET' carries out the action on
;;    subsequent hits.
;;
;;  * The default value of `isearchp-on-demand-action-function' is
;;    function `isearchp-replace-on-demand', which replaces the search
;;    hit.  This means that you can replace (or delete) chosen search
;;    hits on demand.
;;
;;    By default, the replacement string is empty, so with no prefix
;;    argument the action is to delete the search hit (replace it with
;;    nothing).
;;
;;    With a prefix arg, `isearchp-replace-on-demand' prompts for the
;;    replacement, which is used thereafter until you again use a
;;    prefix arg.  Since you can use a prefix arg at any time, you can
;;    provide different replacements for different search hits.  When
;;    prompted, if you clear the minibuffer and hit `RET', hit
;;    replacement just becomes search-hit deletion.
;;
;;    . With a plain prefix arg (`C-u') or a numeric prefix arg of
;;      value 1 (e.g. `C-1'), `isearchp-replace-on-demand' replaces
;;      only the current search hit.
;;
;;    . With a negative prefix arg (e.g. `M--' or `C--'),
;;      `isearchp-replace-on-demand' toggles automatic replacement by
;;      just searching.  Automatic replacement means that each time
;;      you use a search key (e.g. `C-s') to visit a search hit, the
;;      hit is automatically replaced, without your needing to hit
;;      `C-M-RET'.  Using a prefix arg again with `C-M-RET' cancels
;;      this (as does quitting and restarting Isearch).
;;
;;    . With a positive prefix arg N (e.g. `C-8' or `C-u 200'),
;;      `isearchp-replace-on-demand' replaces N search hits (but it
;;      stops at the search limit, if reached).
;;
;;    . With a zero prefix arg (e.g. `C-0),
;;      `isearchp-replace-on-demand' replaces *all* remaining search
;;      hits (up to the search limit).
;;
;;    (NOTE: To use a prefix arg within Isearch, you must set
;;    `isearch-allow-prefix' (if available) or `isearch-allow-scroll'
;;    to non-`nil'.)
;;
;;  * When you use on-demand replacement (with `C-M-RET') the
;;    replacement text can be either inserted literally, as is, or
;;    interpreted as in `query-replace-regexp'.  In the latter case,
;;    you can use `\&', `\=\N', `\#', `\,' and `\?'.
;;
;;    For example, suppose you use a regexp-search pattern of
;;    `\(e\)\|a' and a replacement pattern of `\,(if \1 "a" "e")'.
;;    Each `C-M-RET' will then swap `e' for `a' and vice versa.
;;
;;    See the doc for `query-replace-regexp' and node `Regexp Replace'
;;    of the Emacs manual for more information.
;;
;;    (Note that `\?' is supported, but it is not very useful in this
;;    context, because it prompts you to edit the result each time you
;;    hit `C-M-RET'.  Instead, use `C-u C-M-RET' whenever you want to
;;    change (edit) the replacement pattern.)
;;
;;  * You can use `C-M-`' (`isearchp-toggle-literal-replacement')
;;    anytime during Isearch to toggle whether replacement text is
;;    used literally or interpreted per the special regexp-replacement
;;    constructs.
;;
;;    Note that the use of the special regexp replacement patterns is
;;    unrelated to the kind of incremental search: literal string
;;    search or regexp search.  Just remember that the way to switch
;;    on/off the special behavior of `\&' and so on is to use `C-M-`'.
;;
;;  * The value of variable `isearchp-noprompt-action-function' is a
;;    function that is invoked automatically, after you visit each
;;    search hit.  The function is called with no arguments.  It
;;    cannot use the minibuffer, but it can modify buffer contents.
;;    The variable is reset to `nil' when you quit Isearch.  As an
;;    example of use, command `isearchp-replace-on-demand' with a
;;    negative prefix arg sets this to `isearchp-replace-match', which
;;    causes automatic replacement each time you visit a search hit.
;;
;;  * Hook `isearchp-nomodify-action-hook' (Emacs 22+ only) is also
;;    run after each search visit.  Its functions also must accept the
;;    same arguments as `isearchp-act-on-demand'.  The functions can
;;    use the minibuffer, but they must not update the buffer text (in
;;    a way noticeable by Isearch), or else that will likely lead to a
;;    call-stack overflow.  This is because they are called with
;;    Isearch suspended during `isearch-update' (which can itself be
;;    invoked by the action...).
;;
;;  * Option (`isearchp-regexp-quote-yank-flag') and command
;;    (`isearchp-toggle-regexp-quote-yank', bound to `C-`') to toggle
;;    quoting (escaping) of regexp special characters.  With escaping
;;    turned off, you can yank text such as `^\*.*' without it being
;;    transformed to `\^\\\*\.\*'.

;;  * `M-:' (`isearchp-eval-sexp-and-insert') prompts you for a Lisp
;;    sexp, evaluates it, and appends the value to the search string.
;;    This is useful, for example, to use `rx' or another
;;    regexp-creation helper to create a regexp search pattern.
;;
;;    For example: `C-M-s M-: (rx (and line-start (1+ (in "("))))'
;;    searches using the result of that `rx' sexp, which is "^(+".
;;    (The double-quote chars are removed.)
;;
;;    Remember too that you can use `C-u M-:' after `M-e'.  That
;;    inserts the sexp value into the minibuffer, where you are
;;    editing the search string.  Use this when you do not want to
;;    simply append the sexp value to the search string, but instead
;;    you want to do some editing of it or the rest of the search
;;    string.
;;
;;  * `M-g' (`isearchp-retrieve-last-quit-search') yanks the last
;;    successful search string (regexp or plain) from when you last
;;    hit `C-g' in Isearch.  Sometimes you search for something but
;;    abandon the search - you just want to check the locations of
;;    something, without staying at any of them.  Afterward, if you
;;    want to find them again, use `M-g'.  This yanks that search
;;    string, so you can append it to whatever you are already
;;    searching for.
;;
;;  * `C-x r g' (`isearchp-append-register') appends the contents of a
;;    register to the search string.  You are prompted for the
;;    register to use.  This is the same key that is bound globally to
;;    `insert-register'.  If you want this key to instead exit Isearch
;;    and insert the register in the buffer, then define this key in
;;    `isearch-mode-map' as `nil' (i.e., unbind it), and optionally
;;    bind `isearchp-append-register' to a different key in
;;    `isearch-mode-map'.
;;
;;  * `C-M-y' (`isearch-yank-secondary') yanks the secondary selection
;;    into the search string, if you also use library `second-sel.el'.
;;
;;  * `C-y C-c' (`isearchp-yank-char') yanks successive characters
;;    onto the search string.
;;
;;  * `C-_' (`isearchp-yank-symbol-or-char') yanks successive symbols
;;    (or words or subwords or chars) into the search string.
;;
;;  * `C-(' (`isearchp-yank-sexp-symbol-or-char') yanks successive
;;    sexps (or symbols or words or subwords or chars) into the search
;;    string.
;;
;;  * `M-w' (`isearchp-kill-ring-save') copies the current search
;;    string to the kill ring.  You can then, for example, use `C-s
;;    M-y' to search for the same thing in another Emacs session.
;;
;;    (I use this all the time, but you might not use multiple Emacs
;;    sessions.)  Note that if you did not have this feature then you
;;    would need to select the search-string text (in the text buffer
;;    or in the `M-e' Isearch edit buffer) and copy it to the kill
;;    ring. (Note: `M-w' used to toggle word search, but
;;    `isearch-toggle-word' is now `M-s w'.)
;;
;;  * All commands that yank text onto the search string are bound to
;;    keys with prefix `C-y' (in addition to any other Isearch
;;    bindings):
;;
;;      `C-y C-_'   isearchp-yank-symbol-or-char
;;      `C-y C-('   isearchp-yank-sexp-symbol-or-char
;;      `C-y C-2'   isearch-yank-secondary
;;      `C-y C-c'   isearchp-yank-char
;;      `C-y C-e'   isearchp-yank-line
;;      `C-y C-w'   isearchp-yank-word-or-char
;;      `C-y C-y'   isearch-yank-kill
;;      `C-y M-y'   isearch-yank-pop
;;
;;    You can repeat any of these for which it makes sense (i.e., all
;;    except `isearch-yank-secondary', `isearch-yank-kill', and
;;    `isearch-yank-pop') by just repeating the last key.  For
;;    example: `C-y C-e C-e C-e' adds the text up to the end of three
;;    lines.
;;
;;  * `C-x 8 RET' (`isearch-char-by-name') reads the name of a Unicode
;;    character with completion and appends it to the search string.
;;    Same thing when editing the search string (i.e., after `M-e').
;;    This is part of GNU Emacs starting with Emacs 24.4.
;;
;;  * `C-x o' (`isearchp-open-recursive-edit') opens a recursive
;;    editing session, where you can do anything you like (including
;;    search for something different).  Using `C-M-c' closes the
;;    recursive editing session and resumes the search (from the
;;    current position where you hit `C-M-c').
;;
;;  * Option `isearchp-resume-with-last-when-empty-flag' non-`nil'
;;    (the default) means that if Isearch is resumed with an empty
;;    search string, after being suspended, the previous search string
;;    is used.  If `nil', it is resumed with an empty search string,
;;    as if starting over from the resumed location.
;;
;;  * `C-g' after successfully finding matches restores not only the
;;    original position but also its relative position in the window.
;;    IOW, you get back to what you saw before searching.  Fixes Emacs
;;    bug #12253 for Isearch.
;;
;;  * Highlighting of the mismatched portion of your search string in
;;    the minibuffer.  This is the portion that is removed if you do
;;    `C-g', or removed/replaced manually if you use `C-M-l' (see
;;    next) or automatically if you use `M-k' (see below).  I added
;;    this feature to GNU Emacs 23.1.
;;
;;  * `C-M-l' (`isearchp-remove-failed-part') removes the failed part
;;     of the search string, if any.  `C-g' does this as well, but
;;     `C-g' also has an effect when search is successful.
;;
;;  * `C-<backspace>' (`isearchp-remove-failed-part-or-last-char')
;;    also removes the failed part, if any.  If there is none then it
;;    removes the last character.  You might prefer to bind this to
;;    `DEL' (Backspace), in place of `isearch-delete-char'.
;;
;;  * `M-k' (`isearchp-cycle-mismatch-removal') cycles automatic
;;    removal or replacement of the input portion that does not match.
;;    The behavior is controlled by the value of option
;;    `isearchp-drop-mismatch':
;;
;;    `replace-last' - Your current input replaces the last mismatched
;;                     text.  You can always see your last input, even
;;                     if it is a mismatch.  And it is available for
;;                     editing using `M-e'.
;;    `nil'          - Your current input is appended, even if the
;;                     previous input has a mismatched portion.
;;    anything else  - Your current input is ignored (removed) if it
;;                     causes a mismatch.  The search string always
;;                     has successful matches.
;;
;;  * Option `isearchp-drop-mismatch-regexp-flag' controls whether
;;    regexp search respects option `isearchp-drop-mismatch'.  If
;;    `nil' (the default value) then regexp search acts as if
;;    `isearchp-drop-mismatch' were `nil'.  This is because typing a
;;    regexp such as `[a-w]' can be problematic when mismatches are
;;    automatically replaced.  There is no problem for many regexp
;;    patterns however, so you might prefer customizing this to
;;    non-`nil' and using `M-k' to turn `isearchp-drop-mismatch' off
;;    only temporarily, when needed.
;;
;;  * Non-nil option `isearchp-repeat-search-if-fail-flag' means that
;;    Isearch fails only when there are no search hits within the
;;    search limits.  If there are search hits, and if there are no
;;    more hits in the current search direction, then search restarts
;;    automatically at the limit.  You can toggle this behavior using
;;    `M-s M-k' anytime during Isearch.
;;
;;  * You can use option `isearchp-ring-bell-function' to suppress or
;;    replace bell ringing (`ding') during Isearch (but not for
;;    quitting with `C-g').  Set it to `ignore', for example, to
;;    suppress any indication of an error.  Set it to a function such
;;    as `echo-bell' (from library `echo-bell.el'), to indicate errors
;;    only visually.
;;
;;  * Non-`nil' option `isearchp-toggle-option-flag', which you can
;;    toggle using `M-s v' (`isearchp-toggle-option-toggle'),
;;    determines whether commands that toggle behavior also toggle an
;;    associated user option.  For such commands, a prefix argument
;;    flips the behavior, as if `isearchp-toggle-option-flag' were
;;    toggled temporarily.  Currently this feature applies to toggles
;;    `M-c' (case-sensitivity) and `M-s i' (matching hidden text).
;;
;;  * `M-c' (`isearch-toggle-case-fold') toggles case sensitivity.  If
;;    option `isearchp-toggle-option-flag' is non-`nil' then it
;;    toggles option `isearchp-case-fold' to change the sensitivity
;;    from now on.  Otherwise, the option value is not changed, so the
;;    effect is for the current search only.
;;
;;  * `M-s i' (`isearch-toggle-invisible') toggles invisible-text
;;    sensitivity.  If option `isearchp-toggle-option-flag' is
;;    non-`nil' then it toggles option `search-invisible' to change
;;    the sensitivity from now on.  Otherwise, the option value is not
;;    changed, so the effect is for the current search only.
;;
;;  * `C-+' (`isearchp-toggle-search-invisible') toggles the value of
;;    option `search-invisible'.  The effect is like that of `M-s i'
;;    with no prefix argument and with non-`nil'
;;    `isearchp-toggle-option-flag'.
;;
;;  * `M-s h l' (`isearchp-toggle-lazy-highlight-cleanup') toggles the
;;     value of option `lazy-highlight-cleanup'.  When the option
;;     value is `nil' you can continue to see the search hits
;;     highlighted from the last search.  Toggle the option off, or
;;     use command `isearch-lazy-highlight-cleanup', to remove the
;;     highlighting.  See also option `lazy-highlight-max-at-a-time'.
;;
;;  * `M-s h L' (`isearchp-toggle-lazy-highlighting') toggles the
;;     value of option `isearch-lazy-highlight'.  Turning this
;;     highlighting off can sometimes speed up searching considerably,
;;     in particular for symmetric character folding.
;;
;;  * Other bindings during Isearch:
;;
;;    - `next', `prior' repeat the last Isearch forward and backward
;;      (easier than using the chords `C-s', `C-r').
;;    - `C-end' - go to the longest line.  Repeat to go to the longest
;;      line following that one in the buffer.  As usual, `C-g' puts
;;      you back where you started.  This binding is made only if you
;;      also use `misc-cmds.el'.
;;    - `C-h' provides help on Isearch while searching.  This library
;;      also redefines `isearch-mode-help' so that it lists all
;;      Isearch bindings and ends Isearch properly.
;;
;;  * `M-e' (`isearch-edit-string') automatically puts the cursor at
;;    the first mismatch position in the search string, for easy
;;    editing.  Whereas `C-g' (see also `M-k') removes all of the
;;    mismatch, this feature lets you change or insert a character or
;;    two, without losing the rest of the search string.
;;
;;  * A user option, `isearchp-initiate-edit-commands', that specifies
;;    commands whose keys will not exit Isearch but will instead
;;    initiate editing of the search string.  For example, if
;;    `backward-char' is included in the list then `C-b' and `left'
;;    will just move the cursor backward over the search string so you
;;    can change, delete, or insert chars in the middle somewhere.
;;    This makes the search string more minibuffer-like.
;;
;;  * You can, by default, select text with the mouse, then hit `C-s'
;;    etc. to search for it.  This is controlled by user option
;;    `isearchp-mouse-2-flag'.
;;
;;  * If you also use library `character-fold+.el' then you can use
;;    `M-s =' (command `isearchp-toggle-symmetric-char-fold') to
;;    toggle whether character folding is symmetric.  Note that lazy
;;    highlighting can slow down symmetric char folding considerably,
;;    so you might also want to use `M-s h L' to turn off such
;;    highlighting.
;;
;;    This feature is not available now, since vanilla Emacs changed
;;    the way vanilla file `character-fold.el' works.
;;
;;  If you have Emacs 23 or later then I recommend that you also use
;;  the companion library, `isearch-prop.el'.  If it is in your
;;  `load-path' then it will be loaded by `isearch+.el'.  It lets you
;;  limit incremental searching to contexts that you define.
;;
;;  Example: search within zones having a `face' text property with a
;;  value of `font-lock-comment-face' or `font-lock-string-face'.
;;  Search overlays or text properties.
;;
;;  Besides relying on existing text properties such as `face' for
;;  contexts to search, you can use command
;;  `isearchp-put-prop-on-region' to add any text property to the
;;  region.  This gives you an easy way to set up contexts for
;;  text-property search.  For property `face', empty input to
;;  `isearchp-put-prop-on-region' removes all faces from the region.
;;
;;  If you use library `highlight.el' then you can highlight and
;;  unhighlight Isearch matches in different faces, including for
;;  multiple-buffer searches.  That library binds keys `M-s h h' and
;;  `M-s h u' for this highlighting and unhighlighting.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;(@* "Change log")
;;
;; 2017/02/07 dadams
;;     isearch-lazy-highlight-search:
;;       Use symbol lazy-highlight, not obsolete variable lazy-highlight-face.  Thx to Tino Calancha.
;; 2017/01/01 dadams
;;     Added: isearchp-describe-prefix-bindings, bound to C-z C-h.
;; 2016/12/31 dadams
;;     Added: isearchp-reset-filter-preds-alist (not bound).
;;     isearchp-bookmark-current-filter-predicate: Added filter-description field.
;;     isearchp-reset-filter-predicate: Prefix arg also resets isearchp-current-filter-preds-alist.
;; 2016/12/30 dadams
;;     Added: isearchp-filters-description.
;;     Renamed: isearchp-save-filter-predicate to isearchp-keep-filter-predicate,
;;              isearchp-auto-save-filter-predicate-flag to isearchp-auto-keep-filter-predicate-flag,
;;              isearchp-toggle-auto-save-filter-predicate to isearchp-toggle-auto-keep-filter-predicate,
;;              isearchp-prompt-filter-prefix-auto-save to isearchp-prompt-filter-prefix-auto-keep,
;;              isearchp-saved-filter-predicate to isearchp-kept-filter-predicate.
;;     Added redefinition of isearch-pre-command-hook and isearch-scroll property (fix for Emacs bug #25302).
;;     isearchp-defun-filter-predicate:
;;       Now C-u persists; C-u C-u persists and keeps current.  Combined args SET-P and KEEP-P as arg ACTION.
;;     isearchp-read-predicate: Fix bug introduced yesterday, for (NAME FUNCTION) case.
;;     isearchp-show-filters: Use isearchp-filters-description.
;; 2016/12/29 dadams
;;     Added: isearchp-current-filter-predicates, isearchp-first-isearch-advice, isearchp-last-isearch-advice.
;;       Use them in isearchp-or-last-filter, isearchp-remove-filter-predicate, isearchp-complement-filter,
;;       isearchp-negate-last-filter, isearchp-show-filters.
;;     Added: isearchp-filter-bookmark-alist-only, isearchp-filter-bookmark-p,
;;            isearchp-bookmark-current-filter-predicate (bound to C-z b).
;;     isearchp-read-predicate: Handle also filter bookmarks.
;;     isearchp-remove-filter-predicate: Make the default for completion be the last-added predicate.
;; 2016/12/27 dadams
;;     Added: isearchp-or-last-filter, isearchp-or-predicates, isearchp-or-preds,
;;            isearchp-negate-last-filter, isearchp-not-predicate, isearchp-add-inline-regexp-filter-predicate.
;;     isearchp-read-predicate:
;;       Handle a function symbol that has prop isearchp-part-pred.  Forgot to update this on 12/10.
;;     isearchp-defun-filter-predicate: Test existing name and fboundp before interning.  Add to preds alist.
;;     isearchp-regexp-level-1: Typo - missing quote.
;; 2016/12/24 dadams
;;     isearch-lazy-highlight-update (needed for query-replace-regexp, in replace+.el):
;;       Use isearch-lazy-highlight-regexp,      not isearch-regexp.
;;       Use isearch-lazy-highlight-last-string, not isearch-string.
;; 2016/12/23 dadams
;;     Soft-require hexrgb.el.
;;     isearchp-regexp-level-*: If hexrgb.el is available, inherit from hlt-regexp-level-*.
;; 2016/12/21 dadams
;;     Added: isearchp-ffap-max-region-size, isearchp-ffap-guesser.
;;     isearchp-in-file-or-url-p: Use isearchp-ffap-guesser, not ffap-guesser.
;; 2016/12/18 dadams
;;     isearch-lazy-highlight-update: Do not try to highlight regexp groups if there are none.
;; 2016/12/17 dadams
;;     Added: isearchp-lazy-odd-regexp-groups, isearchp-oddp, isearchp-lazy-regexp-level-overlays.
;;     Added redefinition of lazy-highlight-cleanup.
;;     isearchp-highlight-regexp-group-levels-flag, isearchp-toggle-highlighting-regexp-groups:
;;       For Emacs 24.4+ only.
;;     isearchp-lazy-highlight-face, isearchp-redo-lazy-highlighting: For Emacs 22+ now.
;;     isearch(-de)-highlight: Regexp highlighting is only for Emacs 24.4+.
;;     isearch-lazy-highlight-search: Protect isearchp-lazy-dim-filter-failures-flag, for Emacs 24.4+.
;;     isearch-lazy-highlight-update: Protect isearchp-highlight-regexp-group-levels-flag, for Emacs 24.4+.
;;     isearchp-toggle-highlighting-regexp-groups:
;;       Delete isearchp-regexp-level-overlays too, and call isearchp-redo-lazy-highlighting.
;;     isearch-lazy-highlight-update: Highlight odd regexp groups with face isearchp-lazy-odd-regexp-groups.
;; 2016/12/13 dadams
;;     Added: isearchp-redo-lazy-highlighting.
;;     isearchp-toggle-dimming-filter-failures, isearchp-add-filter-predicate-1,
;;       isearchp-remove-filter-predicate, isearchp-complement-filter, isearchp-(re)set-filter-predicate:
;;         Use isearchp-redo-lazy-highlighting.
;; 2016/12/12 dadams
;;     Added: isearchp-toggle-highlighting-regexp-groups.  Bound to M-s h R.
;;     Bound isearchp-yank-line to M-s C-e (replacing isearch-yank-line).
;; 2016/12/11 dadams
;;     Predefine isearch-face for all Emacs versions < 24.
;; 2016/12/10 dadams
;;     Added: isearchp-regexp-level-[1-8], redefinition of isearch-highlight (handles regexp-group matching),
;;            isearchp-regexp-level-overlays, isearchp-highlight-regexp-group-levels-flag.
;;     Added for Emacs < 22: isearch-face, isearch-error.
;;     isearchp-remove-mismatch, isearch-abort: Use (< emacs-major-version 22), not (boundp 'isearch-error).
;;     isearch-abort: For Emacs < 22, test isearch-invalid-regexp at the outset too.
;;     isearch-dehighlight: Delete overlays in isearchp-regexp-level-overlays.
;;     isearchp-filter-predicates-alist: Added near* entries.  Updated doc for pred-creating entries.
;;     isearchp-add-filter-predicate-1: Corrected isearchp-prompt-for-filter-name handling: added nil case.
;;     isearchp-near-(before|after)-predicate:
;;       Made args optional - initialized with isearchp-read-near-args.  Put prop isearchp-part-pred on them.
;;     isearchp-set-sel-and-yank: x-set-selection -> gui-set-selection for Emacs 25+.
;; 2016/12/09 dadams
;;     Added: isearchp-show-hit-w-crosshairs.
;;     isearchp-filter-predicates-alist: Added crosshairs.  Allow value to be just (NAME PREDICATE).
;;     isearchp-read-predicate: Allow value to be just (NAME PREDICATE).
;;     isearchp-add-filter-predicate-1: Unless isearchp-user-entered-new-filter-p, set prefix to empty string.
;; 2016/12/04 dadams
;;     Added: isearchp-lazy-dim-filter-failures-flag, isearchp-lazy-highlight-face,
;;            isearchp-toggle-dimming-filter-failures (bound to M-s h d).
;;     isearch-lazy-highlight-search: Clear RETRY if isearchp-lazy-dim-filter-failures-flag.
;;                                    Set isearchp-lazy-highlight-face according to filter success.
;;     isearch-lazy-highlight-update: Use isearchp-lazy-highlight-face, not just face lazy-highlight.
;; 2016/11/30 dadams
;;     Added: isearchp-in-lazy-highlight-update-p.
;;     isearch-query-replace is for Emacs 24.4+, not 24.3+.
;; 2016/11/29 dadams
;;     Added: isearchp-assoc-delete-all, isearchp-current-filter-preds-alist,
;;            isearchp-update-filter-predicates-alist-flag, isearchp-user-entered-new-filter-p.
;;     isearchp-filter-predicates-alist: Added set:, to synchronize isearchp-current-filter-preds-alist.
;;     isearchp-add-filter-predicate-1:
;;       Typo: non-symbol if PRED, not PREDICATE, is not symbolp.
;;       Update isearchp-current-filter-preds-alist and possibly isearchp-filter-predicates-alist.
;;     isearchp-read-predicate:
;;       Complete against isearchp-current-filter-preds-alist, not isearchp-filter-predicates-alist.
;;       Set isearchp-user-entered-new-filter-p if user entered a new predicate.
;;     isearchp-remove-filter-predicate:
;;       Correctly handle case where PREDICATE is not a name (string).
;;       Update isearchp-current-filter-preds-alist and possibly isearchp-filter-predicates-alist.
;;     Dynamic filter stuff requires Emacs 24.4+, not 24.3+.
;; 2016/11/15 dadams
;;     Added: isearchp-columns, isearchp-columns-p.
;;     isearchp-read-predicate: If input was not a completion choice then Lisp-read it (e.g. lambda form).
;; 2016/11/09 dadams
;;     isearchp-add(-regexp)-filter-predicate(-1), isearchp-or-filter-predicate, isearchp-near*:
;;         Rename READ-* args to FLIP-READ-*.
;;     isearchp-read-near-args: Provide a default prompt.
;; 2016/11/06 dadams
;;     Added redefinition of isearch-query-replace: Keep advised filter predicate while query-replacing.
;;     Added: isearchp-show-filter-prompt-prefixes-flag, isearchp-toggle-showing-filter-prompt-prefixes.
;;            Bound isearchp-toggle-showing-filter-prompt-prefixes to C-z p.
;;     Removed: isearchp-filters-message.
;;     isearch-message-prefix, isearchp-add-filter-predicate-1:
;;       Respect isearchp-show-filter-prompt-prefixes-flag.
;;     isearchp-filter-predicates-alist:
;;       Changed to use [...] as NAME and PREFIX, for in- entries.  E.g., [#09] instead of in-decimal-number.
;;     isearchp-read-predicate: Remove isearchp- from annotations, for brevity.
;;     isearchp-show-filters:
;;       Removed MSGP arg.  Include orig filter.  Use `' only if name has whitespace.  Removed keys reminder.
;;     isearchp-add-filter-predicate-1, isearchp-remove-filter-predicate, isearchp-set-filter-predicate:
;;       Use isearchp-show-filters, not isearchp-filters-message (removed).
;;     isearchp-remove-filter-predicate: Forgot MSGP in interactive spec.
;; 2016/11/01 dadams
;;     isearchp-complement-filter: Include original predicate in message.
;;     isearch-forward doc string: Mention more options.
;;                                 Add ZERO WIDTH SPACE to get doc link for isearchp-prompt-for-filter-name.
;; 2016/10/31 dadams
;;     Added: isearchp-filter-map, and bound it to C-z.  Moved all dynamic filtering keys to it.
;;     Changed binding of isearchp-show-filters from M-h to h (from M-? h to C-z h).
;; 2016/10/30 dadams
;;     Added: Faces: isearchp-prompt-filter-prefix, isearchp-prompt-filter-prefix-auto-save.  Other:
;;       isearchp-add-regexp-filter-predicate, isearchp-toggle-auto-save-filter-predicate,
;;       isearchp-auto-save-filter-predicate-flag, isearchp-filter-predicates-alist, isearchp-in-color-p,
;;       isearchp-in-comment-p, isearchp-in-comment-or-delim-p, isearchp-in-decimal-number-p,
;;       isearchp-in-defun-p, isearchp-in-email-address-p, isearchp-in-file-name-p, isearchp-in-file-or-url-p,
;;       isearchp-in-hex-number-p, isearchp-in-line-p, isearchp-in-lisp-variable-p, isearchp-in-list-p,
;;       isearchp-in-number-p, isearchp-in-page-p, isearchp-in-paragraph-p, isearchp-in-sentence-p,
;;       isearchp-in-sexp-p, isearchp-in-string-or-comment-p, isearchp-in-string-p, isearchp-in-symbol-p,
;;       isearchp-in-url-p, isearchp-in-word-p, isearchp-add-filter-predicate-1,
;;       isearchp-read-regexp-during-search, isearchp-match-regexp-filter-predicate.
;;     Renamed: isearchp-prompt-for-isearch-prompt-prefix to isearchp-prompt-for-prompt-prefix-flag.
;;     Require thingatpt.el, soft-require thingatpt+.el (Emacs 24.3+).
;;     isearch-done: Set isearchp-saved-filter-predicate or isearch-filter-predicate.
;;     isearch-message: Change regexp from " +$" to "\\`[ \t]+\\'" (not important).
;;     isearch-message-prefix (Emacs 24.4+): Highlight filter prefixes, per *-auto-save-filter-predicate-flag.
;;     Bound isearchp-add-regexp-filter-predicate to M-? %.
;;     Bound isearchp-toggle-auto-save-filter-predicate to M-? S.
;;     isearchp-(add|or)-filter-predicate: Use isearchp-add-filter-predicate-1.
;;     isearchp-add-filter-predicate-1:
;;       Handle all isearchp-filter-predicates-alist formats for PREDICATE.
;;       Moved non-highlighting of empty or whitespace prefix here, from isearchp-read-prompt-prefix.
;;     isearchp-read-prompt-prefix: Move adding comma and space to callers.
;;     isearchp-read-predicate: Use completing-read prompt w/ isearchp-filter-predicates-alist.
;;     isearchp-near-predicate: Updated for feature of specifying units (forgot to do it).
;; 2016/10/22 dadams
;;     Added: isearchp-movement-unit-alist, isearchp-read-measure.
;;     isearchp-read-near-args: Use isearchp-read-measure.
;;     isearchp-near-(before|after)-predicate: DISTANCE is now a cons from isearchp-read-measure.
;; 2016/10/16 dadams
;;     New feature: adding filter predicates on the fly.
;;       Added:
;;         isearchp-add-filter-predicate, isearchp-complement-filter,
;;         isearchp-defun-filter-predicate, isearchp-filters-message, isearchp-near,
;;         isearchp-near-after, isearchp-near-after-predicate, isearchp-near-before,
;;         isearchp-near-before-predicate, isearchp-near-predicate, isearchp-not-pred,
;;         isearchp-or-filter-predicate, isearchp-prompt-for-filter-name, isearchp-read-filter-name,
;;         isearchp-read-near-args, isearchp-read-predicate, isearchp-read-prompt-prefix,
;;         isearchp-remove-filter-predicate, isearchp-reset-filter-predicate,
;;         isearchp-save-filter-predicate, isearchp-saved-filter-predicate,
;;         isearchp-set-filter-predicate, isearchp-show-filters.
;;       Bind M-? prefix map for dynamic filter predicates: &, |, -, ~, !, s, n, 0, <, >, @, M-h.
;;       isearchp-resume-with-last-when-empty-flag:
;;         Updated doc string to say that it applies to newly emptied, not just empty, search string.
;;       with-isearch-suspended:
;;         Bind orig-empty-search-string-p and isearch-filter-predicate.
;;         Let resuming after suspension start again with empty search string, only if empty before.
;;     (NOTE: You can no longer byte-compile this file using an Emacs version prior to Emacs 24.3 and expect
;;            the isearch+.elc file to work with Emacs 24.3 and later.)
;;      isearch-message-prefix: Reverse the order of the filter prefixes.
;; 2016/09/30 dadams
;;     Added: isearch-unread-key-sequence (vanilla def from Emacs 24.3).  (They removed it from Emacs 24.4+.)
;;     Changed guard of *-init-edit, *-update-edit-init-commands, *-initiate-edit-commands to just > Emacs 21.
;;     isearchp-initiate-edit-commands: Unbind isearchp-init-edit first, to take care of deletions too.
;; 2016/07/04 dadams
;;     isearchp-replace-on-demand: Doubled backslashes in doc string.
;;     isearch-lazy-highlight-update: lazy-highlight, not variable lazy-highlight-face. Thx to Tino Calancha.
;; 2016/02/27 dadams
;;     Temporarily remove soft-require of character-fold+.el.
;;     Updated for Emacs 25 isearch.el changes:
;;       with-isearch-suspended: Restore the minibuffer message before moving point.
;;       isearch-update: Use pos-visible-in-window-group-p, not pos-visible-in-window-p, for Emacs 25+.
;;                       Do isearch-lazy-highlight-new-loop after running isearch-update-post-hook.
;;       isearch-search: Ensure isearch-message-function is bound before evaluating.
;;       isearch-lazy-highlight-(search|update): Use window-group-(start|end), not window-(start|end).
;;       isearch-lazy-highlight-update: Test window against isearch-lazy-highlight-window-group, not *-window.
;;                                      Do not set window property of overlay to selected window.
;;     isearch-update: Fix for Emacs 24.1 and 24.2 (window-body-width arity changed).
;; 2016/02/12 dadams
;;     isearchp-repeat-search-if-fail-flag: Removed message and sit-for.
;; 2016/01/18 dadams
;;     isearchp-repeat-search-if-fail-flag: Added :set function.
;; 2016/01/15 dadams
;;     Added: isearchp-repeat-search-if-fail-flag, isearchp-toggle-repeat-search-if-fail,
;;            isearchp--repeat-search-if-fail-repeated.
;;     Bind isearchp-toggle-repeat-search-if-fail to M-s M-k.
;;     Bind isearchp-toggle-set-region to M-s M-SPC, not to C-SPC.
;;     isearch-forward: Mention isearchp-toggle-repeat-search-if-fail in doc string.
;;     isearchp-replace-literally: Corrected doc string: C-M-`, not M-`.
;; 2015/11/28 dadams
;;     isearchp-toggle-symmetric-char-fold: Mention toggling lazy highlighting in message.
;; 2015/11/27 dadams
;;     Added: isearchp-toggle-symmetric-char-fold.  Bound to M-s =.
;;     Added: isearchp-toggle-lazy-highlighting.  Bound to M-x h L.
;;     isearch-message-prefix: Fix yesterday's update for Emacs 25.
;;     Soft-require character-fold+.el.
;; 2015/11/26 dadams
;;     Updated for Emacs 25:
;;      isearch(-lazy-highlight)-word -> isearch(-lazy-highlight)-regexp-function.  Declare variable aliases.
;;      with-isearch-suspended: Ensure multi-isearch-* vars are bound.
;;      isearch-toggle-word: Do not redefine for Emacs 25+.
;;      isearch-update: Use isearch--current-buffer only if live buffer.
;;                      Use window-body-width if defined.
;;      isearch-(forward|backward)(-regexp): Use multi-isearch-buffers only if defined.
;;      isearch-mode: Added optional arg REGEXP-FUNCTION.  Bind isearch-regexp-function to it.
;;                    Use value of search-default-regexp-mode instead of character-fold-search.
;;                    Removed LOCAL arg for add-hook.
;;      isearch--describe-word-mode: Do not redefine for Emacs 25+.
;;      isearch-message-prefix: For Emacs 25: Use isearch--describe-regexp-mode.  Handle multi-file too.
;;      Removed Emacs 25 advertised bindings for isearch-toggle-case-fold, isearch-toggle-regexp,
;;        isearch-edit-string.
;; 2015/11/23 dadams
;;     Added: isearchp-remove-failed-part-or-last-char.  Bound to C-backspace.
;;     Renamed isearchp-if-empty-prefer-resuming-with-last to isearchp-resume-with-last-when-empty-flag, and
;;      made it a defcustom.
;;     isearchp-remove-failed-part:
;;       Simplified and fixed problem of isearch-delete-char afterward restoring failed part.
;;     with-isearch-suspended: Do not call isearch-ring-adjust1 if isearch-string is empty.
;;     (put 'with-isearch-suspended 'common-lisp-indent-function '(&body))
;; 2015/10/26 dadams
;;     with-isearch-suspended: Applied Tino Calancha's fix for Emacs bug #21663.
;; 2015/09/01 dadams
;;     Added: redefinition of isearch--describe-word-mode.
;;     isearch-toggle-word, isearch-mode: Updated for Emacs 25 (char folding).
;;     isearch-message-prefix: Use isearch--describe-word-mode.
;;     multi-isearch-end: Updated guard - advice not needed for Emacs 24.5+ (Emacs bug #20234).
;; 2015/08/28 dadams
;;     isearch-lazy-highlight-search, isearch-lazy-highlight-update:
;;       Fix Emacs bug #21092, at least for a nil value of lazy-highlight-max-at-a-time.
;;     isearch-lazy-highlight-search: Update wrt Emacs 25 code: Extend bound to match whole string at point.
;; 2015/08/15 dadams
;;     isearchp-ring-bell-function: Use function ignore as the default - see inline comment.
;; 2015/07/22 dadams
;;     Added: isearchp-drop-mismatch-regexp-flag, isearchp-toggle-lazy-highlight-cleanup, isearchp-complete,
;;            isearchp-complete-past-string.
;;     Bind isearchp-toggle-lazy-highlight-cleanup to M-s h l.  Bind isearchp-complete to M-TAB, C-M-i.
;;     isearchp-drop-mismatch, isearch-forward: Mention isearchp-drop-mismatch-regexp-flag in doc string.
;;     isearchp-remove-mismatch, isearch-printing-char:
;;       Now a no-op when regexp and not isearchp-drop-mismatch-regexp-flag.
;; 2015/07/11 dadams
;;     isearchp-eval-sexp-and-insert: Use pp-read-expression-map if available.
;; 2015/06/28 dadams
;;     Added: face isearchp-overwrapped.
;;     Face isearchp-wrapped: Default uses just a blue overline, not a deep-pink foreground.
;;     isearch-message-prefix: Use face isearchp-overwrapped.
;;     isearchp-highlight-lighter: Show overwrapping too, using face isearchp-overwrapped.
;;                                 Show regexp vs literal too, using R*search instead of Isearch
;;     isearch-update:
;;       Updated for Emacs 25 2015-06-29 snapshot: handle cursor-sensor-inhibit, isearch--current-buffer.
;; 2015/05/26 dadams
;;     Added: isearchp--replacing-on-demand.
;;     isearchp-replace-on-demand: Negative prefix arg now toggles auto-replacing, instead of turning it on.
;;                                 And replace current hit, to start with (was skipped before).
;; 2015/04/29 dadams
;;     Added: isearchp-if-empty-prefer-resuming-with-last.
;;     with-isearch-suspended: If isearchp-if-empty-prefer-resuming-with-last is nil then empty means empty.
;; 2015/04/12 dadams
;;     isearchp-act-on-demand:
;;       Bind isearch-mode-end-hook, remove isearchp-property-finish from it temporarily.
;; 2015/04/02 dadams
;;     isearchp-drop-mismatch: in :set, test VAL, not SYM.
;; 2015/03/31 dadams
;;     Added: isearchp-ring-bell-function, isearchp-orig-ring-bell-fn.
;;     Added advice of isearch-done: Reset ring-bell-function to isearchp-orig-ring-bell-fn.
;;     with-isearch-suspended: Temporarily restore ring-bell-function.
;;     isearch-mode: Save isearchp-orig-ring-bell-fn.  Use isearchp-ring-bell-function.
;;     isearch-repeat: Added version for Emacs < 24.3.
;;                     Both versions: Do not use isearchp-remove-mismatch on isearch-update-post-hook.
;; 2015/03/30 dadams
;;     isearchp-drop-mismatch: Added :set to defcustom.
;; 2015/03/27 dadams
;;     Added: isearchp-remove-failed-part.  Bound to C-M-l.
;; 2015/02/23 dadams
;;     Added: isearchp-eval-sexp-and-insert.  Bound to M-:.
;; 2014/10/08 dadams
;;     Added: isearchp-append-register.  Bound to C-x r g.
;; 2014/09/03 dadams
;;     Changed C-c binding for isearchp-yank-char to C-z.
;; 2014/09/02 dadams
;;     isearchp-replace-match: Temporary (?) fix for braindead Emacs 24.4 regression (bug #18388).
;; 2014/04/15 dadams
;;     isearch-printing-char: Update version test for Emacs 24.4 pretest - use version<.
;; 2014/01/10 dadams
;;     isearch-mouse-2: Do not call isearchp-set-sel-and-yank unless mark is defined.
;;     isearchp-set-sel-and-yank: No-op unless mark is defined.
;; 2013/12/16 dadams
;;     isearch-mode: Update for vanilla Emacs 24: set isearch--saved-overriding-local-map.  (Bug#16035)
;; 2013/10/16 dadams
;;     with-isearch-suspended: Do not (goto-char old-other-end) after BODY if we just replaced text.
;; 2013/10/15 dadams
;;     Added: isearchp-toggle-literal-replacement, isearchp-user-error, isearchp-replace-fixed-case-p,
;;            isearchp-replace-match, isearchp-pref-arg, isearchp-replace-literally.
;;     Removed: isearchp-replace-string.
;;     isearchp-act-on-demand: Bind isearchp-pref-arg to prefix arg.
;;                             Call isearchp-on-demand-action-function with no args.
;;     isearch-update: Call isearchp-nomodify-action-hook and isearchp-noprompt-action-function with no args.
;;     isearchp-replace-on-demand:
;;       Removed the args - none now.  Use (match-string 0) instead of STRING arg.
;;       Use isearchp-pref-arg, not current-prefix-arg.  Use isearchp-replace-match, not *-replace-string.
;;       Reset replace-count where applicable.  Set this-command to isearchp-act-on-demand at end.
;;     isearchp-replace-multiple:
;;       Use replace-count, not COUNT.  Use isearchp-replace-match instead of deleting and inserting.
;;     Bind isearchp-toggle-literal-replacement to C-M-`.
;; 2013/10/11 dadams
;;     Added: isearchp-replace-multiple.
;;     isearch-mode: Add hooks: isearch-(pre|post)-command-hook, for new Emacs 24.4 snapshot.
;;     with-isearch-suspended: It is for all versions now.  Added catch.  Put first in file.
;;     isearchp-replace-on-demand: Plain C-u: replace only current. N>0: replace N. N=0: replace all.
;;     isearch-forward: Updated doc string.
;; 2013/10/06 dadams
;;     Define isearch-update-post-hook only if not already defined.
;;     isearchp-update-post-hook -> isearch-update-post-hook.
;;     isearchp-replace-on-demand: Use <, not <= (typo).
;; 2013/10/03 dadams
;;     Added: isearchp-nomodify-action-hook, isearchp-noprompt-action-function,
;;            isearchp-on-demand-action-function, isearchp-act-on-demand, isearchp-replace-on-demand,
;;            isearchp-replacement, isearchp-replace-string, isearchp-update-post-hook (renamed from
;;            isearch-*), isearchp-barf-if-use-minibuffer, isearchp-reset-noprompt-action-fn.
;;     Removed defadvice for isearch-update.
;;     Added redefinition of isearch-update.
;;     Added redefinition of isearch-dehighlight, for Emacs 20.
;;     Bind isearchp-act-on-demand to C-M-return.
;; 2013/09/30 dadams
;;     Do not soft-require isearch-prop.el unless Emacs 23+.
;; 2013/09/12 dadams
;;     isearchp-reg-(beg|end): Changed default value to nil.
;      isearch-mode: save-restriction and widen, to get region limits.
;;     isearch-search, isearch-repeat, isearch-lazy-highlight-search, isearch-lazy-highlight-update:
;;       handle null isearchp-reg-(beg|end) case per vanilla.
;; 2013/09/10 dadams
;;     Added support for limiting search to active region (Emacs 24.3+):
;;       Added: isearchp-deactivate-region-flag, isearchp-restrict-to-region-flag, isearchp-reg-beg,
;;              isearchp-reg-end, isearchp-toggle-region-restriction.
;;       Added redefinitions: isearch-search, isearch-repeat, isearch-lazy-highlight-search,
;;             isearch-lazy-highlight-update.
;;       Bound isearchp-toggle-region-restriction to C-x n.
;;       isearch-mode: Save isearchp-reg-beg|end.  Deactivate region.
;;     isearch-forward: Updated doc string.
;; 2013/09/08 dadams
;;     Moved all character-property code to new library isearch-prop.el.  Soft-require it.
;;       Moved: *-char-prop*, *-put-prop-on-region, *-some, *-filter-predicate-orig.
;; 2013/09/06 dadams
;;     isearchp-put-prop-on-region: Restore buffer-modified-p after adding property.
;;     isearchp-char-prop-1: Show message only when already in isearch-mode.
;; 2013/06/29 dadams
;;     isearchp-drop-mismatch: Removed quote in const.
;; 2013/06/28 dadams
;;     Bind C-x 8 RET even for Emacs 24.4+ (where it is true by default), because we set C-x to nil.
;; 2013/06/27 dadams
;;     Renamed: isearchp-toggle-invisible to isearchp-toggle-search-invisible.
;;     Added: isearchp-toggle-option-flag, isearchp-toggle-option-toggle, isearchp-case-fold,
;;            isearchp-last-non-nil-case-fold, isearch-toggle-invisible (redef).
;;     Added defvar for isearch-invisible, for older Emacs versions and to provide doc string.
;;     isearchp-toggle-invisible:
;;       Respect isearchp-toggle-option-flag.  Set isearch-invisible, isearch-success, isearch-adjusted.
;;     isearch-toggle-case-fold:
;;       Handle like isearch-toggle-invisible: respect isearchp-toggle-option-flag for isearchp-case-fold.
;;     Bind isearch-toggle-invisible to M-s i, as in vanilla Emacs 24.4.
;;     Bind isearchp-toggle-option-toggle to M-s v.
;;     isearch-mode: Updated per Emacs 24 dev version: bind isearch-invisible.
;;     isearch-forward: Updated doc string.
;;     isearch-printing-char: Put back version with no args for Emacs < 24.4.
;; 2013/06/25 dadams
;;     Updated some wrt vanilla isearch.el.
;;       Replaced isearch-insert-char-by-name with isearch-char-by-name (which now has optional args).
;;       Updated isearch-printing-char:  now has optional args.
;;       Removed mention of isearch-nonincremental-exit-minibuffer (obsolete in Emacs 24.4+) in doc strings.
;;       Mention isearch-allow-prefix (new in Emacs 24.4) in doc strings that mention isearch-allow-scroll.
;;       isearchp-char-prop-filter-pred: Allow also for isearch-invisible (new).
;;     isearchp-toggle-invisible: Better message - show current value.
;; 2013/05/31 dadams
;;     Require cl.el at compile time, for case.
;; 2013/05/13 dadams
;;     isearchp-highlight-lighter: Use face isearchp-wrapped only if defined (Emacs 22+).
;; 2013/04/10 dadams
;;     Define with-isearch-suspended for Emacs 24.3 too.  Apparently it did not make it into the release.
;; 2013/03/30 dadams
;;     Added: isearchp-yank-char, isearchp-yank-word-or-char, isearchp-yank-line, isearchp-repeat-command.
;;     Renamed isearchp-yank(-sexp)-symbol-or-char to isearchp-yank(-sexp)-symbol-or-char-1.
;;     isearchp-yank(-sexp)-symbol-or-char (new): Redefined as repeatable, using *-1 helper.
;;     Bind isearch-toggle-case-fold to M-c, not C-c.  Bind isearchp-yank-char to C-c.
;;     Bind isearchp-yank-(char|line|(word|symbol|sexp-symbol)-or-char) to C-y + control char.
;;     Do not bind vanilla isearch-yank commands to C-y prefix.
;;
;; 2013/03/29 dadams
;;     Added: isearchp-kill-ring-save.  Bind it to M-w, instead of isearch-toggle-word (which is now M-s w).
;;     Bind isearch-toggle-word to M-s w for Emacs < 23.
;;     Renamed: isearchp-with-search-suspended to with-isearch-suspended,
;;              isearchp-insert-char-by-name to isearch-insert-char-by-name.
;;     For Emacs 24.3+, do not define with-isearch-suspended or isearch-insert-char-by-name
;;       (vanilla has same definitions).  Do not duplicate key binding for isearch-insert-char-by-name.
;;     Make C-y a prefix key, and put all yank commands on it:
;;       C-y C-_   isearchp-yank-symbol-or-char
;;       C-y C-(   isearchp-yank-sexp-symbol-or-char
;;       C-y C-2   isearch-yank-secondary
;;       C-y C-c   isearchp-yank-char
;;       C-y C-e   isearchp-yank-line
;;       C-y C-w   isearchp-yank-word-or-char
;;       C-y C-y   isearch-yank-kill
;;       C-y M-y   isearch-yank-pop
;;     Moved key bindings and hooks to the end of the file.
;; 2013/01/28 dadams
;;     Advise isearch-forward to add Isearch+ doc.
;; 2013/01/16 dadams
;;     New feature: C-g restores window position of start.  Fixes Emacs bug #12253.
;;       Added redefinitions of isearch-cancel, isearch-mode.  Added: isearchp-win-pt-line.
;; 2012/12/15 dadams
;;     Added redefinition of isearch-printing-char.
;;     Renamed/replaced: isearchp-toggle-mismatch-removal with isearchp-cycle-mismatch-removal,
;;                       isearchp-mismatch-removal-flag   with isearchp-drop-mismatch.
;;     isearchp-cycle-mismatch-removal, isearchp-drop-mismatch: Handle replace-last case.
;; 2012/12/13 dadams
;;     Advise: isearch-update (Emacs 20-23).
;;     Added: isearchp-toggle-mismatch-removal, isearchp-mismatch-removal-flag,
;;            isearchp-remove-mismatch, isearchp-open-recursive-edit.
;;     Bind isearchp-toggle-mismatch-removal to M-k, isearchp-open-recursive-edit to C-x o.
;;     Put isearchp-highlight-lighter on isearch-update-post-hook for Emacs 20-23 also.
;;     Updated to fit Juri's vanilla Emacs version of macro isearchp-use-new-search-string:
;;       Renamed: isearchp-use-new-search-string: to isearchp-with-search-suspended,
;;                isearchp-read-unicode-char to isearchp-insert-char-by-name.
;;       Updated isearch-edit-string, isearchp-insert-char-by-name to use new macro definition.
;; 2012/12/12 dadams
;;     Bind C-x and C-x 8 to nil in isearch-mode-map, for Emacs 23.
;; 2012/12/09 dadams
;;     Added: Macro isearchp-use-new-search-string (Emacs 22+) - factored from isearch-edit-string.
;;            isearchp-read-unicode-char (Emacs 23+).
;;     Bind C-x 8 RET in isearch-mode-map and minibuffer-local-isearch-map.
;;     isearch-edit-string:
;;       Define using isearchp-use-new-search-string.
;;       Update per Emacs 24: Save/restore *-case-fold-search. Bind history-add-new-input to nil.
;; 2012/10/09 dadams
;;     isearchp-read-face-names: Bind icicle-multi-completing-p to t.
;; 2012/09/30 dadams
;;     Added: isearchp-last-quit(-regexp)-search, isearchp-retrieve-last-quit-search,
;;            redefinition of isearch-abort.
;;     Bound isearchp-retrieve-last-quit-search to M-g.
;; 2012/08/27 dadams
;;     isearch(p)-message-(prefix|suffix): Emacs 24.2 turned out to use the same code as 24.1.
;; 2012/08/12 dadams
;;     isearch-edit-string: isearchp-fail-pos -> (or (isearch-fail-pos) (length isearch-string)).
;; 2012/08/08 dadams
;;     Added: isearchp-message-prefix, isearchp-message-suffix.  Use everywhere in place of vanilla.
;;     isearch-message, isearch-fail-pos:
;;       Redefine only for Emacs 22 & 23.  Use vanilla defs (Emacs 24, but with Emacs 22/23 vars).
;;       Removed isearchp-fail-pos - replaced it with isearch-fail-pos.
;;     isearch-message-prefix: Added redefinition for Emacs 24.2+ (changed arglist).
;; 2012/02/08 dadams
;;     isearchp-remove-duplicates: Redefined to use a hash table.
;; 2012/01/11 dadams
;;     Added isearch-message-prefix (redefinition).
;;     Added faces: isearchp-(wrapped|regexp|word|multi).
;;     isearchp-highlight-lighter: Propertize lighter if wrapped.
;; 2011/12/01 dadams
;;     isearchp-toggle-(invisible|regexp-quote-yank|set-region|case-fold):
;;       Added sit-for after message.
;;     isearchp-toggle-(regexp-quote-yank|set-region): Added isearch-update after message + sit-for.
;;     isearch-toggle-word: Redefine even for Emacs versions that have it, to get message etc.
;;                          Added message and sit-for.
;; 2011/11/14 dadams
;;     Bind switch-frame event to ignore in Isearch.
;;     Added and commented out: isearchp-switch-frame-or-exit.
;; 2011/11/13 dadams
;;     Added: isearchp-set-sel-and-yank.
;;     isearch-mouse-2: Use isearchp-set-sel-and-yank, even for nil case.
;;                      Don't require transient-mark-mode.
;; 2011/11/11 dadams
;;     Added defgroup for isearch-plus.  Added: isearchp-mouse-2-flag.
;;     Added redefinition of isearch-mouse-2 that respects isearchp-mouse-2-flag.
;;       And it works for Windows too and all Emacs versions.
;;     Bind mouse-2 to isearch-mouse-2 and down-mouse-2 to ignore.
;;     isearchp-highlight-lighter: Delete (isearch-mode isearch-mode) also from alist.
;;     isearch-yank-string: Updated wrt Emacs 24 code.
;; 2011/09/25 dadams
;;     Added: isearchp-put-prop-on-region.
;;     isearchp-read-face-names: Added optional args empty-means-none-p, only-one-p.
;;     isearchp-read-sexps: Added optional arg only-one-p.
;; 2011/09/23 dadams
;;     Added (renamed from icicle- versions): isearchp-char-prop-default-match-fn,
;;                                            isearchp-char-prop-matches-p, isearchp-some.
;;     isearchp-char-prop-1: Use isearchp-read-sexps, not icicle-sexp-list.
;;     isearchp-char-prop-filter-pred: Use isearchp-char-prop-matches-p,
;;                                     isearchp-char-prop-default-match-fn, not icicle-*.
;; 2011/09/22 dadams
;;     Added: isearchp-char-prop-(backward|forward)(-regexp), isearchp-fontify-buffer-now,
;;            isearchp-char-prop-(1|end|filter-pred), isearchp-char-properties-in-buffer,
;;            isearchp-read-face-names, isearchp-read-face-names--read, isearchp-read-sexps,
;;            isearchp-remove-duplicates, isearchp-char-prop-prop, isearchp-char-prop-type,
;;            isearchp-char-prop-values, isearchp-filter-predicate-orig.
;;     Renamed: set-region-around-search-target to isearchp-set-region-around-search-target.
;;     Bound isearchp-char-prop-forward(-regexp) to C-t, C-M-t.
;;     Define keys here, instead of on isearch-mode-hook.  So we rely on eval-after-load.
;;     Changed key for isearch-toggle-regexp to same as vanilla Emacs: M-r.
;; 2011/09/12 dadams
;;     isearchp-fail-pos: Replaced isearch-message* with isearch-string*.  Thx to Juri Linkov.
;; 2011/09/08 dadams
;;     Added isearchp-init-edit (from anonymous fn), so can see it in keymap help.
;; 2011/07/07 dadams
;;     Added: isearchp-highlight-lighter, isearch-toggle-case-fold (redefinition).
;;     Put isearchp-highlight-lighter on isearch-update-post-hook.
;; 2011/06/03 dadams
;;     isearchp-initiate-edit-commands: Added left-word.
;; 2011/05/27 dadams
;;     Added: isearchp-initiate-edit-commands, isearchp-update-edit-init-commands.
;; 2011/05/16 dadams
;;     Added: isearchp-fail-pos, redefinition of isearch-edit-string.
;;     Removed: isearchp-goto-success-end (not needed - go there by default now).
;; 2011/01/04 dadams
;;     Added autoload cookies for defcustom and commands.
;; 2010/12/05 dadams
;;     Added: isearchp-toggle-invisible, isearchp-last-non-nil-invisible.
;; 2010/10/18 dadams
;;     isearch-mode-hook: Protect isearchp-goto-success-end with fboundp.
;; 2010/06/23 dadams
;;     Added: isearchp-yank(-sexp)-symbol-or-char.  Bound to C-_, C-(.
;; 2010/04/22 dadams
;;     Added: isearchp-toggle-regexp-quote-yank, isearchp-regexp-quote-yank-flag,
;;            isearch-yank-string (redefinition).
;; 2009/06/09 dadams
;;     Bind isearch-repeat-(forward|backward) to (next|prior) in isearch-mode-map.
;; 2008/11/10 dadams
;;     Added: isearchp-goto-success-end.
;; 2008/05/25 dadams
;;     Don't add C-M-tab to isearch-mode-map if already defined.
;; 2008/05/24 dadams
;;     Don't bind C-j to isearch-edit-string.  Bind M-e to isearch-edit-string (for Emacs 20).
;; 2008/02/28 dadams
;;     isearch-message: Protect from Emacs 21 also.
;; 2008/02/24 dadams
;;     isearch-message:
;;       Juri's fix for M-r (was losing failed text) and C-M-s [a-z] (highlighted only ]).
;; 2008/02/23 dadams
;;     isearch-message:
;;       isearch-fail face: Provide better defaults.
;;       Juri's fix for M-p: Use isearch-message for succ-msg, if diff from first msg of
;;         isearch-cmds (isearch-edit-string sets it).
;; 2007/09/10 dadams
;;     Bound goto-longest-line to C-s C-end.  Added soft require of misc-cmds.el.
;; 2007/09/07 dadams
;;     isearch-message:
;;       regexp-quote succ-msg. put-text-property, not propertize, for trailing whitespace.
;; 2007/07/10 dadams
;;     isearchp-set-region: Do nothing unless transient-mark-mode.
;; 2007/02/02 dadams
;;     isearch-message: Fixed when succ-msg matches whole isearch-message (no highlight).
;; 2007/01/23 dadams
;;     isearch-message: For Emacs 22+ only.
;; 2006/12/12 dadams
;;     Added isearch-toggle-word (from Juri Linkov), and bound it.
;; 2006/10/28 dadams
;;     Added: isearch-fail, isearch-message (redefinition).
;; 2006/07/30 dadams
;;     Added: set-region-around-search-target.
;; 2006/07/29 dadams
;;     Added: isearchp-toggle-set-region,isearchp-set-region(-flag). Thx to Andreas Roehler
;; 2006/01/24 dadams
;;     On MS Windows, bind isearch-complete* to C-tab.
;; 1999/03/17 dadams
;;     Updated to corrspond to Emacs 34.1 version.
;; 1996/04/24 dadams
;;     Added redefinition of isearch-search.  Require cl.el.
;; 1995/12/28 dadams
;;     Changed isearch-edit-string binding.
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

(eval-when-compile (require 'cl)) ;; case

 ;; Cannot do (require 'isearch), because `isearch.el' does no `provide'.
 ;; We do not want to do a (load-library "isearch"), because it would not
 ;; allow doing (eval-after-load "isearch" '(progn (require 'isearch+)))

(when (> emacs-major-version 22) (require 'isearch-prop nil t)) ;; (no error if not found)


;; Library `thingatpt+.el' greatly improves the behavior provided by `thingatpt.el'.
;; You can use only the latter, if you want, but be aware that its behavior is bugged.
;;
(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3)))

  (require 'thingatpt)
  (require 'thingatpt+ nil t)

  )



;;; $$$$$$ TEMPORARILY REMOVE THIS.  They changed the Emacs 25 `character-fold.el' code,
;;;        and `character-fold+.el' has not yet been updated accordingly.
;;; $$$$$$ (when (> emacs-major-version 24) (require 'character-fold+ nil t)) ;; (no error if not found)

(require 'misc-cmds nil t) ;; (no error if not found): goto-longest-line


;; Quiet the byte compiler.
(defvar bidi-display-reordering)         ; Emacs 24+, built-in.
(defvar char-fold-symmetric)             ; In `character-fold+.el' (Emacs 25+).
(defvar cursor-sensor-inhibit)           ; In `isearch.el' (Emacs 25+).
(defvar disable-point-adjustment)        ; Built-in, Emacs 22+.
(defvar eval-expression-debug-on-error)  ; In `simple.el', Emacs 22+.
(defvar icicle-WYSIWYG-Completions-flag) ; In `icicles-opt.el'.
(defvar isearch--current-buffer)         ; Emacs 25+
(defvar isearch-filter-predicate)        ; In `isearch.el' (Emacs 24+).
(defvar isearch-invalid-regexp)          ; In `isearch.el' (Emacs 20-21).
(defvar isearch-last-case-fold-search)   ; In `isearch.el'.
(defvar isearch-lax-whitespace)          ; In `isearch.el' (Emacs 24.3+).
(defvar isearch-lazy-highlight)          ; In `isearch.el' (Emacs 21+).
(defvar isearch-lazy-highlight-case-fold-search) ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-end)      ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-end-limit) ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-forward)  ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-last-string) ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-lax-whitespace) ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-overlays) ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-regexp)   ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-regexp-function) ; In `isearch.el' (Emacs 25+).
(defvar isearch-lazy-highlight-regexp-lax-whitespace) ; In `isearch.el' (Emacs 24.3+).
(defvar isearch-lazy-highlight-start)    ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-start-limit) ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-timer)    ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-window)   ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-window-group) ; In `isearch.el' (Emacs 25+).
(defvar isearch-lazy-highlight-word)     ; In `isearch.el' (Emacs 24).
(defvar isearch-lazy-highlight-wrapped)  ; In `isearch.el' (Emacs 24+).
(defvar isearch-message-function)        ; In `isearch.el' (Emacs 24+).
(defvar isearch-message-prefix-add)      ; In `isearch.el'.
(defvar isearch-new-message)             ; In `with-isearch-suspended' (here).
(defvar isearch-new-string)              ; In `with-isearch-suspended' (here).
(defvar isearch-new-word)                ; In `with-isearch-suspended' (Emacs 23-24).
(defvar isearch-original-minibuffer-message-timeout) ; In `isearch.el'.
(defvar isearch-push-state-function)     ; In `isearch.el'.
(defvar isearch-regexp-function)         ; In `isearch.el' (Emacs 25+).
(defvar isearch-regexp-lax-whitespace)   ; In `isearch.el' (Emacs 24.3+).
(defvar isearch--saved-overriding-local-map) ; In `isearch.el'.
(defvar isearch-start-hscroll)           ; In `isearch.el'.
(defvar isearch-within-brackets)         ; In `isearch.el'.
(defvar isearch-wrap-function)           ; In `isearch.el'.
(defvar isearchp-auto-keep-filter-predicate-flag) ; Here (Emacs 24.4+).
(defvar isearchp-current-filter-preds-alist) ; Here (Emacs 24.4+).
(defvar isearchp-deactivate-region-flag) ; Here (Emacs 24.3+).
(defvar isearchp-filter-map)             ; Here (Emacs 24.4+).
(defvar isearchp-filter-predicates-alist) ; Here (Emacs 24.4+).
(defvar isearchp-initiate-edit-commands) ; Here (Emacs 22+).
(defvar isearchp-in-lazy-highlight-update-p) ; Here (Emacs 24.3+).
(defvar isearchp-movement-unit-alist) ; Here (Emacs 24.4+).
(defvar isearchp-nomodify-action-hook)   ; Here (Emacs 22+).
(defvar isearchp-on-demand-action-function) ; Here (Emacs 22+).
(defvar isearchp-prompt-for-filter-name) ; Here (Emacs 24.4+).
(defvar isearchp-prompt-for-prompt-prefix-flag) ; Here (Emacs 24.4+).
(defvar isearchp-replacement)            ; Here (Emacs 22+).
(defvar isearchp-repeat-search-if-fail-flag) ; Here (Emacs 22+).
(defvar isearchp--repeat-search-if-fail-repeated) ; Here (Emacs 22+).
(defvar isearchp-restrict-to-region-flag) ; Here (Emacs 24.3+).
(defvar isearchp-show-filter-prompt-prefixes-flag) ; Here (Emacs 24.4+).
(defvar isearchp-update-filter-predicates-alist-flag) ; Here (Emacs 24.4+).
(defvar last-repeatable-command)         ; In `repeat.el'.
(defvar lazy-highlight-cleanup)          ; In `isearch.el' (Emacs 22+).
(defvar lazy-highlight-interval)         ; In `isearch.el' (Emacs 24+).
(defvar lazy-highlight-max-at-a-time)    ; In `isearch.el' (Emacs 24+).
(defvar minibuffer-message-timeout)      ; In Emacs C code.
(defvar multi-isearch-buffer-list)       ; In `isearch.el'.
(defvar multi-isearch-current-buffer)    ; In `isearch.el'.
(defvar multi-isearch-buffer-list)       ; In `isearch.el'.
(defvar multi-isearch-file-list)         ; In `isearch.el'.
(defvar multi-isearch-next-buffer-current-function) ; In `isearch.el'.
(defvar replace-count)                   ; In `replace.el'.
(defvar search-default-regexp-mode)      ; In `isearch.el' (Emacs 25+).
(defvar subword-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Faces and Variables")

;;; Faces and Variables ----------------------------------------------
(defgroup isearch-plus nil
  "Isearch enhancements."
  :prefix "isearchp-" :group 'isearch
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
Isearch+ bug: \
&body=Describe bug here, starting with `emacs -Q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "http://www.emacswiki.org/isearch+.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/IsearchPlus")
  :link '(emacs-commentary-link :tag "Commentary" "isearch+"))

;; Needed only for Emacs < 22.
;;

(when (< emacs-major-version 22)

  ;; From vanilla Emacs 24.5
  (defface isearch
      '((((class color) (min-colors 88) (background light))
         ;; The background must not be too dark, for that means
         ;; the character is hard to see when the cursor is there.
         (:background "magenta3" :foreground "lightskyblue1"))
        (((class color) (min-colors 88) (background dark))
         (:background "palevioletred2" :foreground "brown4"))
        (((class color) (min-colors 16))
         (:background "magenta4" :foreground "cyan1"))
        (((class color) (min-colors 8))
         (:background "magenta4" :foreground "cyan1"))
        (t (:inverse-video t)))
    "Face used to highlight Isearch search hit."
    :group 'isearch :group 'basic-faces)

  (defvar isearch-error nil "Error message for failed search.")

  )

(when (< emacs-major-version 24)
  (defvar isearch-face 'isearch "Face used to highlight Isearch search hit."))

(when (> emacs-major-version 21)        ; Emacs 22+

  (defface isearch-fail
      '((((class color) (min-colors 88) (background dark))
         (:foreground "white" :background "#22225F5F2222")) ; a dark green
        (((class color) (min-colors 88) (background light))
         (:foreground "Black" :background "Plum"))
        (((class color) (min-colors 8)) (:background "red"))
        (((type tty) (class mono)) :inverse-video t)
        (t :background "gray"))
    "*Face for highlighting failed part in Isearch echo-area message."
    :group 'isearch-plus)

  (defface isearchp-multi
      '((((class color) (min-colors 8)) (:foreground "DarkViolet"))
        (t :underline t))
    "*Face for highlighting multi-buffer indicator in Isearch echo-area message."
    :group 'isearch-plus)

  (defface isearchp-overwrapped
      `((((class color) (min-colors 88))
         ,(if (> emacs-major-version 23)
              '(:overline "DeepPink" :underline (:color "DeepPink" :style wave))
              '(:overline "DeepPink" :underline "DeepPink")))
        (t :overline t))
    "*Face for highlighting overwrapped search."
    :group 'isearch-plus)

  (defface isearchp-regexp
      '((((class color) (min-colors 8)) (:foreground "Firebrick"))
        (t :underline t))
    "*Face for highlighting regexp-search indicator in Isearch echo-area message."
    :group 'isearch-plus)

  (defface isearchp-word
      '((((class color) (min-colors 8)) (:foreground "DarkGreen"))
        (t :underline t))
    "*Face for highlighting word-search indicator in Isearch echo-area message."
    :group 'isearch-plus)

  (defface isearchp-wrapped
      '((((class color) (min-colors 88)) (:overline "Blue"))
        (t :overline t))
    "*Face for highlighting wrapped search."
    :group 'isearch-plus)
  )


;; Regexp group highlighting.
;;
(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3)))

  (require 'hexrgb nil t) ;; No error if not found

  (defface isearchp-regexp-level-1 (if (and (facep 'hlt-regexp-level-1) ; In `hexrgb.el'
                                            (> emacs-major-version 21))
                                       '((t (:inherit hlt-regexp-level-1)))
                                     '((((background dark)) (:background "#071F473A0000")) ; a dark green
                                       (t (:background "#FA6CC847FFFF")))) ; a light magenta
    "*Face used to highlight subgroup level 1 of your search context.
This highlighting is done during regexp searching whenever
`isearchp-highlight-regexp-group-levels-flag' is non-nil."
    :group 'isearch-plus :group 'faces)

  (defface isearchp-regexp-level-2 (if (and (facep 'hlt-regexp-level-2) ; In `hexrgb.el'
                                            (> emacs-major-version 21))
                                       '((t (:inherit hlt-regexp-level-2)))
                                     '((((background dark)) (:background "#507400002839")) ; a dark red
                                       (t (:background "#C847FFFFE423")))) ; a light cyan
    "*Face used to highlight subgroup level 2 of your search context.
This highlighting is done during regexp searching whenever
`isearchp-highlight-regexp-group-levels-flag' is non-nil."
    :group 'isearch-plus :group 'faces)

  (defface isearchp-regexp-level-3 (if (and (facep 'hlt-regexp-level-3) ; In `hexrgb.el'
                                            (> emacs-major-version 21))
                                       '((t (:inherit hlt-regexp-level-3)))
                                     '((((background dark)) (:background "#4517305D0000")) ; a dark brown
                                       (t (:background "#C847D8FEFFFF")))) ; a light blue
    "*Face used to highlight subgroup level 3 of your search context.
This highlighting is done during regexp searching whenever
`isearchp-highlight-regexp-group-levels-flag' is non-nil."
    :group 'isearch-plus :group 'faces)

  (defface isearchp-regexp-level-4 (if (and (facep 'hlt-regexp-level-4) ; In `hexrgb.el'
                                            (> emacs-major-version 21))
                                       '((t (:inherit hlt-regexp-level-4)))
                                     '((((background dark)) (:background "#176900004E0A")) ; a dark blue
                                       (t (:background "#EF47FFFFC847")))) ; a light yellow
    "*Face used to highlight subgroup level 4 of your search context.
This highlighting is done during regexp searching whenever
`isearchp-highlight-regexp-group-levels-flag' is non-nil."
    :group 'isearch-plus :group 'faces)

  (defface isearchp-regexp-level-5 (if (and (facep 'hlt-regexp-level-5) ; In `hexrgb.el'
                                            (> emacs-major-version 21))
                                       '((t (:inherit hlt-regexp-level-5)))
                                     '((((background dark)) (:background "#04602BC00000")) ; a very dark green
                                       (t (:background "#FCFCE1E1FFFF")))) ; a light magenta
    "*Face used to highlight subgroup level 5 of your search context.
This highlighting is done during regexp searching whenever
`isearchp-highlight-regexp-group-levels-flag' is non-nil."
    :group 'isearch-plus :group 'faces)

  (defface isearchp-regexp-level-6 (if (and (facep 'hlt-regexp-level-6) ; In `hexrgb.el'
                                            (> emacs-major-version 21))
                                       '((t (:inherit hlt-regexp-level-6)))
                                     '((((background dark)) (:background "#32F200001979")) ; a very dark red
                                       (t (:background "#E1E1FFFFF0F0")))) ; a light cyan
    "*Face used to highlight subgroup level 6 of your search context.
This highlighting is done during regexp searching whenever
`isearchp-highlight-regexp-group-levels-flag' is non-nil."
    :group 'isearch-plus :group 'faces)

  (defface isearchp-regexp-level-7 (if (and (facep 'hlt-regexp-level-7) ; In `hexrgb.el'
                                            (> emacs-major-version 21))
                                       '((t (:inherit hlt-regexp-level-7)))
                                     '((((background dark)) (:background "#316B22970000")) ; a very dark brown
                                       (t (:background "#E1E1EAEAFFFF")))) ; a light blue
    "*Face used to highlight subgroup level 7 of your search context.
This highlighting is done during regexp searching whenever
`isearchp-highlight-regexp-group-levels-flag' is non-nil."
    :group 'isearch-plus :group 'faces)

  (defface isearchp-regexp-level-8 (if (and (facep 'hlt-regexp-level-8) ; In `hexrgb.el'
                                            (> emacs-major-version 21))
                                       '((t (:inherit hlt-regexp-level-8)))
                                     '((((background dark)) (:background "#12EC00003F0E")) ; a very dark blue
                                       (t (:background "#F6F5FFFFE1E1")))) ; a light yellow
    "*Face used to highlight subgroup level 8 of your search context.
This highlighting is done during regexp searching whenever
`isearchp-highlight-regexp-group-levels-flag' is non-nil."
    :group 'isearch-plus :group 'faces)

  (defface isearchp-lazy-odd-regexp-groups
      `((((background dark))
         (:background "paleturquoise2"))
        (t (:background "paleturquoise3")))
    "*Face used to lazy-highlight odd subgroups of your search context.
This highlighting is done during regexp searching whenever
`isearchp-highlight-regexp-group-levels-flag' is non-nil."
    :group 'isearch-plus :group 'faces)

  (defvar isearchp-lazy-regexp-level-overlays nil
    "Overlays used to lazy-highlight odd subgroups of your search context.")

  )


;; Dynamic search filtering.
(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3)))

  (defface isearchp-prompt-filter-prefix
      '((((class color) (min-colors 88)) (:inherit font-lock-doc-face))
        (t :foreground "gray"))
    "*Face for Isearch prompt filter prefix when not autosaving."
    :group 'isearch-plus)

  (defface isearchp-prompt-filter-prefix-auto-keep
      '((((class color) (min-colors 88)) (:inherit font-lock-constant-face))
        (t :foreground "gray"))
    "*Face for Isearch prompt filter prefix when autosaving.
Like `isearchp-prompt-filter-prefix', but used only when
`isearchp-auto-keep-filter-predicate-flag' is non-nil."
    :group 'isearch-plus)

  )

;;;###autoload
(defcustom isearchp-case-fold nil
  "*Whether incremental search is case sensitive.
nil   means search is always case sensitive
t     means search is never  case sensitive
`yes' means search case-sensitivity follows option `search-upper-case'"
  :type '(choice
          (const :tag "Case sensitive"                      nil)
          (const :tag "Respect option `search-upper-case'"  t)
          (const :tag "Case insensitive"                    yes))
  :group 'isearch-plus)


;; Dynamic search filtering.
(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3)))

  (defvar isearchp-filter-map nil "Keymap containing bindings for dynamic Isearch filtering commands.")

  (define-prefix-command 'isearchp-filter-map)
  (define-key isearch-mode-map (kbd "C-z") isearchp-filter-map) ; Put filtering commands on prefix `C-z'.

  (define-key isearchp-filter-map (kbd "C-h")  'isearchp-describe-prefix-bindings)              ; `C-z C-h'
  (define-key isearchp-filter-map (kbd "&")    'isearchp-add-filter-predicate)                  ; `C-z &'
  (define-key isearchp-filter-map (kbd "%")    'isearchp-add-regexp-filter-predicate)           ; `C-z %'
  (define-key isearchp-filter-map (kbd ".")    'isearchp-add-inline-regexp-filter-predicate)    ; `C-z .'
  (define-key isearchp-filter-map (kbd "-")    'isearchp-remove-filter-predicate)               ; `C-z -'
  (define-key isearchp-filter-map (kbd "||")   'isearchp-or-filter-predicate)                   ; `C-z ||'
  (define-key isearchp-filter-map (kbd "|1")   'isearchp-or-last-filter)                        ; `C-z |1'
  (define-key isearchp-filter-map (kbd "~~")   'isearchp-complement-filter)                     ; `C-z ~~'
  (define-key isearchp-filter-map (kbd "~1")   'isearchp-negate-last-filter)                    ; `C-z ~1'
  (define-key isearchp-filter-map (kbd "!")    'isearchp-set-filter-predicate)                  ; `C-z !'
  (when (featurep 'bookmark+)
    (define-key isearchp-filter-map (kbd "b")  'isearchp-bookmark-current-filter-predicate))    ; `C-z b'
  (define-key isearchp-filter-map (kbd "c")    'isearchp-columns)                               ; `C-z c'
  (define-key isearchp-filter-map (kbd "p")    'isearchp-toggle-showing-filter-prompt-prefixes) ; `C-z p'
  (define-key isearchp-filter-map (kbd "s")    'isearchp-keep-filter-predicate)                 ; `C-z s'
  (define-key isearchp-filter-map (kbd "S")    'isearchp-toggle-auto-keep-filter-predicate)     ; `C-z S'
  (define-key isearchp-filter-map (kbd "n")    'isearchp-defun-filter-predicate)                ; `C-z n'
  (define-key isearchp-filter-map (kbd "0")    'isearchp-reset-filter-predicate)                ; `C-z 0'
  (define-key isearchp-filter-map (kbd "<")    'isearchp-near-before)                           ; `C-z <'
  (define-key isearchp-filter-map (kbd ">")    'isearchp-near-after)                            ; `C-z >'
  (define-key isearchp-filter-map (kbd "@")    'isearchp-near)                                  ; `C-z @'
  (define-key isearchp-filter-map (kbd "?")    'isearchp-show-filters)                          ; `C-z ?'

  (define-key isearch-mode-map (kbd "M-s h d") 'isearchp-toggle-dimming-filter-failures)        ; `M-s h d'

  )

(unless (boundp 'isearch-update-post-hook)
  (defvar isearch-update-post-hook ()
    "Function(s) called after each Isearch command.
More precisely, called at the end of `isearch-update'."))

;;;###autoload
(defcustom isearchp-drop-mismatch nil
  "*Non-nil means remove or replace a search-string mismatch.
There are three possible values:

`replace-last' - Replace the last mismatch in the search string with
                 the latest input (e.g., replace the last typed char
                 or last yanked text).
nil            - Never remove mismatched text from the search string.
anything else  - Always remove mismatched text from the search string.

* Vanilla Isearch has the behavior of a nil value.

* Non-nil, non-`replace-last' means the search string never contains
  mismatched characters.

* `replace-last' means you see only the latest mismatched input, and
  it is available for editing, using \\<isearch-mode-map>`\\[isearch-edit-string]'.

You can cycle among the three possible values using \
`\\[isearchp-cycle-mismatch-removal]'.

See also option `isearchp-drop-mismatch-regexp-flag'.  It controls
whether regexp search respects or ignores `isearchp-drop-mismatch'.
If `nil' (the default value) then regexp search acts as if
`isearchp-drop-mismatch' were nil.  This is because typing a regexp
such as `[a-w]' can be problematic when mismatches are automatically
replaced."
  :type  '(choice
           (const :tag "Replace last mismatch"  replace-last)
           (const :tag "Never remove mismatch"  nil)
           (other :tag "Always remove mismatch" t))
  :set   #'(lambda (sym val)
             (custom-set-default sym val)
             (if (and val  (not (eq 'replace-last val)))
                 (add-hook 'isearch-update-post-hook 'isearchp-remove-mismatch)
               (remove-hook 'isearch-update-post-hook 'isearchp-remove-mismatch)))
  :group 'isearch-plus)

;;;###autoload
(defcustom isearchp-drop-mismatch-regexp-flag nil
  "*Non-nil means respect `isearchp-drop-mismatch' for regexp search too.
Otherwise (nil), regexp search ignores `isearchp-drop-mismatch',
acting as if it were nil.

Turning off automatic mismatch replacement can help during regexp
search when you type a pattern such as `[a-z]', because there likely
is no match when you type `[' and if not turned off then your typing
is automatically replaced by `a'.

There is no problem for many regexp patterns however, so you might
prefer customizing `isearchp-drop-mismatch-regexp-flag' to non-`nil'
and just using `M-k' to turn `isearchp-drop-mismatch' off temporarily
when needed."
  :type 'boolean :group 'isearch-plus)

(when (> emacs-major-version 21)        ; Emacs 22+

  ;; Emacs 24.4 removed this.  This is the original definition (from Emacs 24.3).
  ;;
  (unless (fboundp 'isearch-unread-key-sequence)
    (defun isearch-unread-key-sequence (keylist)
      "Unread the given key-sequence KEYLIST.
Scroll-bar or mode-line events are processed appropriately."
      (cancel-kbd-macro-events)
      (apply 'isearch-unread keylist)
      (when (and (> (length keylist) 1)  (symbolp (car keylist))  (listp (cadr keylist))
                 (not (numberp (posn-point (event-start (cadr keylist))))))
        (pop unread-command-events))))

  (defun isearchp-init-edit (&rest ignored)
    "Invoke current key sequence, but after calling `isearch-edit-string'."
    (interactive)
    (isearch-unread-key-sequence (listify-key-sequence (this-command-keys)))
    (isearch-edit-string))

  (defun isearchp-update-edit-init-commands ()
    "Make `isearchp-initiate-edit-commands' edit the search string."
    (dolist (cmd  isearchp-initiate-edit-commands)
      (substitute-key-definition cmd 'isearchp-init-edit isearch-mode-map (current-global-map))))

  ;; No autoload cookie - need function `isearchp-update-edit-init-commands'.
  (defcustom isearchp-initiate-edit-commands
    '(backward-char                     ; `C-b'
      left-char                         ; `left' (Emacs 24+)
      ;; backward-delete-char                ; `DEL'
      ;; backward-delete-char-untabify       ; `DEL'
      ;; backward-kill-paragraph             ; `C-backspace'
      ;; backward-kill-sentence              ; `C-x DEL'
      ;; backward-kill-sexp                  ; `C-M-backspace'
      ;; backward-kill-word                  ; `M-DEL'
      ;; backward-list                       ; `C-M-p'
      ;; backward-page                       ; `C-x ['
      ;; backward-paragraph                  ; `C-up', `M-{'
      ;; backward-sentence                   ; `M-a'
      backward-sexp                     ; `C-M-b', `C-M-left'
      ;; backward-to-indentation             ; Not bound by default
      ;; backward-up-list                    ; `C-M-u', `C-M-up'
      backward-word                     ; `M-b', `M-left'
      left-word                         ; `C-left'
      ;; delete-backward-char
      ;; kill-backward-up-list               ; Not bound by default
      ;; beginning-of-buffer                 ; `M-<', `C-home'
      ;; beginning-of-defun                  ; `C-M-a', `C-M-home',
      ;; beginning-of-line                   ; `C-a', `home'
      ;; beginning-of-line+                  ; `C-a', `home'
      ;; beginning-of-line-text              ; Not bound by default
      ;; beginning-of-visual-line            ; `C-a', `home'
      )
    "*Commands whose key bindings initiate Isearch edit.
When invoked by a key sequence, Isearch edits the search string,
applying the command to it immediately.

Commands you might want to include here are typically commands that
move point to the left, possibly deleting text along the way.

Set this to `nil' if you always want all such commands to exit Isearch
and act on the buffer text."
    :set #'(lambda (sym defs)
             (substitute-key-definition 'isearchp-init-edit nil isearch-mode-map) ; Get rid of any old ones.
             (custom-set-default sym defs)
             (isearchp-update-edit-init-commands)) ; Apply the current ones.
    :type '(repeat (restricted-sexp :tag "Command"
                    ;; Use `symbolp' instead of `functionp' or `fboundp', in
                    ;; case the library defining the function is not loaded.
                    :match-alternatives (symbolp) :value ignore))
    :group 'isearch-plus)

  )

(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3)))

  (defcustom isearchp-deactivate-region-flag t
    "Non-nil means isearching deactivates the region.
See also option `isearchp-restrict-to-region-flag'."
    :type 'boolean :group 'isearch-plus)

  (defcustom isearchp-restrict-to-region-flag t
    "Non-nil means restrict isearching to the active region.
See also option `isearchp-deactivate-region-flag'."
    :type 'boolean :group 'isearch-plus)

  )


;; Dynamic search filtering.
;;
(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3)))

  (defcustom isearchp-auto-keep-filter-predicate-flag nil
    "Non-nil means automatically apply `\\[isearchp-keep-filter-predicate]'.
Changes to `isearch-filter-predicate' are automatically kept for
subsequent searches in this Emacs session when you exit Isearch'."
    :type 'boolean :group 'isearch-plus)

  (defcustom isearchp-filter-predicates-alist
    `(
      ("[color]"      isearchp-in-color-p             "[COLOR]")
      ("[;]"          isearchp-in-comment-p           "[;]")
      ("[;+]"         isearchp-in-comment-or-delim-p  "[;+]")
      ("[defun]"      isearchp-in-defun-p             "[DEFUN]")
      ("[email]"      isearchp-in-email-address-p     "[EMAIL]")
      ("[file]"       isearchp-in-file-name-p         "[FILE]")
      ("[file|url])"  isearchp-in-file-or-url-p       "[FILE|URL])")
      ("[line]"       isearchp-in-line-p              "[LINE]")
      ("[()]"         isearchp-in-list-p              "[()]")
      ("[#]"          isearchp-in-number-p            "[#]")
      ("[page]"       isearchp-in-page-p              "[PAGE]")
      ("[para]"       isearchp-in-paragraph-p         "[PARA]")
      ("[sent]"       isearchp-in-sentence-p          "[SENT]")
      ("[sexp]"       isearchp-in-sexp-p              "[SEXP]")
      ("[\"|;]"       isearchp-in-string-or-comment-p "[\"|;]")
      ("[\"]"         isearchp-in-string-p            "[\"]")
      ("[symb]"       isearchp-in-symbol-p            "[SYMB]")
      ("[url]"        isearchp-in-url-p               "[URL]")
      ("[var]"        isearchp-in-lisp-variable-p     "[VAR]")
      ("[word]"       isearchp-in-word-p              "[WORD]")
      ,@(and (featurep 'thingatpt+)
             '(("[#09]" isearchp-in-decimal-number-p  "[#09]")
               ("[#0F]" isearchp-in-hex-number-p      "[#0F]")))
      ,@(and (featurep 'crosshairs)
             '(("crosshairs" isearchp-show-hit-w-crosshairs)))
      ("near<..."     isearchp-near-before-predicate)
      ("near>..."     isearchp-near-after-predicate)
      ("not..."       isearchp-not-predicate)
      ("or..."        isearchp-or-predicates)
      )
    "Alist of filter predicates to choose from.

\(If you use library Bookmark+ then you can also choose a
filter-predicate bookmark.  Those are not included as entries in this
alist.)

Each alist entry has one of these forms, where NAME and PREFIX are
strings, and FUNCTION is a predicate symbol or a lambda form suitable as a
value of `isearch-filter-predicate'.

* (NAME FUNCTION PREFIX)
* (NAME FUNCTION)
* (FUNCTION PREFIX)
* (FUNCTION)

NAME is typically a short name for FUNCTION.  You can use it, for
example, to remove FUNCTION from `isearch-filter-predicate', using
`C-z -'.

FUNCTION can also be a function symbol that is invoked with no
arguments, and that then returns a predicate suitable for
`isearch-filter-predicate'.  The use case here is to let you choose a
function that prompts you for some additional information, which it
uses to build a predicate.  In this case, symbol FUNCTION must have a
non-nil `isearchp-part-pred' symbol-property value.

Function `isearchp-near-before-predicate' is an example of this is
kind of indirection: when you choose it, it prompts you for another
pattern for Isearch to match, a max number of units of nearness, and
which units to measure with.

The alist is used for completion when you are prompted to add a filter
predicate.  If NAME is present for an entry then it is the completion
candidate that your input must match, and FUNCTION is shown next to it
as an annotation in buffer `*Completions*'.  If NAME is not present
then FUNCTION itself is the candidate.

See function `isearchp-read-predicate' for more information."
    :type '(repeat
            (choice
             (list :tag "Short name, predicate, prompt prefix"
              (string :tag "Short name")
              (restricted-sexp :match-alternatives (functionp) :tag "Predicate (Lisp sexp)")
              (string :tag "Prompt prefix"))
             (list :tag "Short name, predicate"
              (string :tag "Short name")
              (restricted-sexp :match-alternatives (functionp) :tag "Predicate (Lisp sexp)"))
             (list :tag "Predicate, prompt prefix"
              (restricted-sexp :match-alternatives (functionp) :tag "Predicate (Lisp sexp)")
              (string :tag "Prompt prefix"))
             (list :tag "Predicate"
              (restricted-sexp :match-alternatives (functionp) :tag "Predicate (Lisp sexp)"))))
    :group 'isearch-plus
    :set (lambda (sym defs)            ; Synchronize the associated defvar.
           (custom-set-default sym defs)
           (setq isearchp-current-filter-preds-alist  isearchp-filter-predicates-alist)))

  (defcustom isearchp-lazy-dim-filter-failures-flag t
    "Non-nil means lazy-highlight filter failures with a dim background.
A nil value means do not lazy-highlight filter failures at all."
    :type 'boolean :group 'isearch-plus)

  (defcustom isearchp-movement-unit-alist '((?w . forward-word)
                                            (?x . forward-sexp)
                                            (?l . forward-list)
                                            (?s . forward-sentence)
                                            (?c . forward-char))
    "Alist of unit characters and associated forward movement functions.
Each entry is a cons (CHARACTER . FORWARD-THING-FUNCTION), where:

* CHARACTER stands for the unit of forward movement specified by
  FORWARD-THING-FUNCTION.

* FORWARD-THING-FUNCTION must accept an integer argument NUM and move
  point forward by NUM units.

For example, `(?w . forward-word)' says that if you specify unit `w'
for an `isearchp-near-*' command then it looks within a given number
of words."
    :type '(repeat (cons character function))
    :group 'isearch-plus)

  (defcustom isearchp-prompt-for-filter-name 'non-symbol
    "Whether and when to prompt you for a name for a filter predicate.
You can reverse the behavior by using a non-positive prefix arg: If
you would normally be prompted then you are not prompted, and vice
versa."
    :type '(choice
            (const :tag "Do not prompt"                              nil)
            (const :tag "Prompt always"                              always)
            (const :tag "Prompt if non-symbol (e.g. anonymous (lambda))"  non-symbol))
    :group 'isearch-plus)

  (defcustom isearchp-prompt-for-prompt-prefix-flag t
    "Whether to prompt you for a prefix for the Isearch prompt.
You can reverse the behavior by using a non-negative prefix arg: If
you would normally be prompted then you are not prompted, and vice
versa."
    :type 'boolean :group 'isearch-plus)

  (defcustom isearchp-show-filter-prompt-prefixes-flag t
    "Whether to show prefixes for filters in the Isearch prompt."
    :type 'boolean :group 'isearch-plus)

  (defcustom isearchp-update-filter-predicates-alist-flag nil
    "Non-nil means filter changes affect `isearchp-filter-predicates-alist'.
When you add filters dynamically, or you remove filters that you have
added dynamically, option `isearchp-filter-predicates-alist' is
updated to reflect those changes."
    :type 'boolean :group 'isearch-plus)

  )

;; Regexp group highlighting.
;;
(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3)))

  (defcustom isearchp-highlight-regexp-group-levels-flag t
    "*Non-nil means highlight 1-8 regexp group levels, within search hit.
You can toggle this value using \\<isearch-mode-map>`\\[isearchp-toggle-highlighting-regexp-groups]' \
during Isearch."
    :type 'boolean :group 'isearch-plus)

  )

;;;###autoload
(defcustom isearchp-mouse-2-flag t
  "*Non-nil means clicking `mouse-2' during Isearch yanks the selection.
In that case, you can select text with the mouse, then hit `C-s' to
search for it.

If the value is nil, yank only if the `mouse-2' click is in the echo
area.  If not in the echo area, invoke whatever `mouse-2' is bound to
outside of Isearch."
  :type 'boolean :group 'isearch-plus)

(when (> emacs-major-version 21)

  (defcustom isearchp-on-demand-action-function 'isearchp-replace-on-demand
    "*Function invoked by command `isearchp-act-on-demand'.
It is called with no arguments.

It can access the raw prefix argument used for command
`isearchp-act-on-demand' as the value of variable `isearchp-pref-arg'.

The default value, `isearchp-replace-on-demand', replaces the search
hit with the value of `isearchp-replacement'."
    :group 'isearch-plus :type 'function)

  )

;;;###autoload
(defcustom isearchp-regexp-quote-yank-flag t
  "*Non-nil means escape special chars in text yanked for a regexp isearch.
You can toggle this using `isearchp-toggle-regexp-quote-yank', bound
to `C-`' during Isearch."
  :type 'boolean :group 'isearch-plus)

(when (fboundp 'isearch-fail-pos)       ; Emacs 22+
  (defcustom isearchp-repeat-search-if-fail-flag nil
    "*Non-nil means on failure, restart search automatically from search limit.
You can toggle this using `isearchp-toggle-repeat-search-if-fail', bound to
`M-s M-k' during Isearch."
    :type 'boolean
    :set   #'(lambda (sym val)
               (custom-set-default sym val)
               (if val
                   (add-hook 'isearch-update-post-hook 'isearchp-repeat-search-if-fail)
                 (remove-hook 'isearch-update-post-hook 'isearchp-repeat-search-if-fail)))
    :group 'isearch-plus))

;;;###autoload
(defcustom isearchp-resume-with-last-when-empty-flag t
  "If non-nil, newly empty search string means resume with last one.
This applies to resumption of search after `with-isearch-suspended'.

If the search string is empty after suspending search (because you set
`isearch-new-string' = \"\"), and it was not empty before suspending,
then a non-nil value of this option means resume searching with the
last search string.  Otherwise, resume searching with the empty search
string.

This option has no effect for Emacs releases prior to Emacs 22."
  :type 'boolean :group 'isearch-plus)

;;;###autoload
(defcustom isearchp-ring-bell-function #'ignore
  ;; (if (and (> emacs-major-version 21)  (require 'echo-bell nil t)) #'echo-bell ring-bell-function)
  ;; `echo-bell' is nice, but if you search in zones (e.g. the region or `isearchp-zones-*') then the
  ;; cursor can be seen to bounce briefly to a hit outside the search zones, when there are no more
  ;; hits inside the zones.  This is because of the slight delay to show you the echo-bell message.
  ;; So for Isearch in general, `ignore' probably makes a better default value.
  "*Function that Isearch+ uses to ring the bell during search, or nil.
This does not affect the use of `C-g'.
If nil then use the value of `ring-bell-function'.

Possible functions you can use:
 `echo-bell' - Indication shown in echo area (requires `echo-bell.el')
 `ignore'    - Do nothing - no sound or visible indication"
  :type '(choice
          (function :tag "Alternative bell (function)")
          (const :tag "Standard behavior (respect `ring-bell-function')" nil))
  :group 'isearch-plus)

;;;###autoload
(defcustom isearchp-set-region-flag nil
  "*Non-nil means set region around search target.
This is used only for Transient Mark mode.
You can toggle this using `isearchp-toggle-set-region', bound to
`M-s M-SPC' during Isearch."
  :type 'boolean :group 'isearch-plus)

;;;###autoload
(defcustom isearchp-toggle-option-flag nil
  "*Non-nil means Isearch toggling commands can affect option values.
If nil, the option value remains unchanged - the effect is temporary.

Applies to toggle commands for behavior that has an associated user
option.  Currently this means `M-s i' (`isearch-toggle-invisible') and
`M-c' (`isearch-toggle-case-fold')."
  :type 'boolean :group 'isearch-plus)


(defvar isearchp-last-non-nil-case-fold (or isearchp-case-fold  t)
  "Last non-nil value of option `isearchp-case-fold'.")

(defvar isearchp-last-non-nil-invisible (or search-invisible  'open)
  "Last non-nil value of option `search-invisible'.")

(defvar isearchp-last-quit-search nil
  "Last successful search string when you hit `C-g' to quit Isearch.")

(defvar isearchp-last-quit-regexp-search nil
  "Last successful search regexp when you hit `C-g' to quit regexp Isearch.")

(when (> emacs-major-version 21)

  (defvar isearchp-nomodify-action-hook nil
    "Functions invoked after visiting and highlighting each search hit.
Each function is invoked passing no arguments.  It can access the
current search hit using the match data.

NOTE: The functions must not update the buffer text (in a way
noticeable by Isearch), or else they will likely lead to a call-stack
overflow.  This is because they are called with Isearch suspended
during `isearch-update' (which can itself be invoked by the
action...).")

  )

(defvar isearchp-noprompt-action-function nil
  "Function invoked after visiting and highlighting each search hit.
This is reset to nil when you quit Isearch.

The function cannot use the minibuffer.  It is called with no
arguments.  It can access the current search hit using the match
data.")

(defvar isearchp-orig-ring-bell-fn ring-bell-function
  "Original value of `ring-bell-function', restored after Isearch.")

(defvar isearchp-pref-arg  nil
  "Raw prefix arg value when you invoked `isearchp-act-on-demand'.")

(defvar isearchp-reg-beg nil            ; Used only for Emacs 24.3+
  "Beginning of the nonempty active region or nil.
If `isearchp-restrict-to-region-flag' then the former.
Set when Isearch is started.")

(defvar isearchp-reg-end nil            ; Used only for Emacs 24.3+
  "End of the nonempty active region or nil.
If `isearchp-restrict-to-region-flag' then the former.
Set when Isearch is started.")

(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3)))

  (defvar isearchp-regexp-level-overlays nil
    "Overlays used to highlight context levels other than the top level.")

  )

(when (> emacs-major-version 21)

  (defvar isearchp-replace-literally nil ; Toggle using `C-M-`'.
    "Non-nil means to treat replacement text literally.
Otherwise (nil), interpret `\\' specially in replacement text, as in
the LITERAL argument to `replace-match'.
You can toggle this using `isearchp-toggle-literal-replacement', bound
to `C-M-`' during Isearch.")

  (defvar isearchp-replacement ""
    "Replacement string used by `isearchp-replace-on-demand'.")

  (defvar isearchp--repeat-search-if-fail-repeated nil
    "Non-nil means `isearchp-repeat-search-if-fail' was repeated.")

  )

;; Dynamic search filtering.
(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3)))

  (defvar isearchp-user-entered-new-filter-p nil
    "Non-nil means user entered a new filter predicate.
This means we need to update `isearchp-current-filter-preds-alist'.")

  (defvar isearchp-current-filter-preds-alist isearchp-filter-predicates-alist
    "`isearchp-filter-predicates-alist', plus predicates added by input.")

  (defvar isearchp-kept-filter-predicate isearch-filter-predicate
    "Value to which `isearch-filter-predicate' is set in `isearch-done'.")

  )

(defvar isearchp-win-pt-line nil
  "Line number of point before searching, relative to `window-start'.")

;; Vanilla - no-op, but with a doc string.
(defvar isearch-invisible  search-invisible
  "Whether or not to search invisible text.
Values are the same as for option `search-invisible'.
This variable has an effect only for the current search.")

;; `isearch-word' and `isearch-lazy-highlight-word' were declared obsolete by Emacs 25.1.
(when (and (fboundp 'defvaralias)  (< emacs-major-version 25))

  (defvaralias 'isearch-regexp-function                'isearch-word)
  (defvaralias 'isearch-lazy-highlight-regexp-function 'isearch-lazy-highlight-word)

  )
 
;;(@* "Macros")

;;; Macros -----------------------------------------------------------

(defmacro isearchp-user-error (&rest args)
  "`user-error' if defined, otherwise `error'."
  `(if (fboundp 'user-error) (user-error ,@args) (error ,@args)))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Provided for older Emacs versions also.  And if `isearch-new-word' is bound, use that.
;; 2. Add `catch', and update `isearch-success' with thrown value.
;; 3. Temporarily restore `ring-bell-function'.
;;
(put 'with-isearch-suspended 'common-lisp-indent-function '(&body))

(defmacro with-isearch-suspended (&rest body)
  "Exit Isearch mode, run BODY, and reinvoke the pending search.
BODY can involve use of the minibuffer, including recursive minibuffers.

You can update the global isearch variables by setting new values to
`isearch-new-string', `isearch-new-message', `isearch-new-forward',
`isearch-new-regexp-function', and `isearch-new-case-fold'.

If BODY `throw's a non-nil value, it is taken as the position from
which to resume searching.  Otherwise, searching resumes where it was
suspended."
  ;; This code is very hairy for several reasons, explained in the code.
  ;; Mainly, `isearch-mode' must be terminated while suspended and then restarted.
  ;; If there were a way to catch any change of buffer from the minibuffer, this could be
  ;; simplified greatly.
  ;;
  ;; This code does not back up the search point. Should it, for use with `isearch-edit-string'?
  `(let ((newpoint
          (catch 'with-isearch-suspended
            (condition-case nil
                (progn
                  (let ((enable-recursive-minibuffers        t)
                        (isearch-nonincremental              isearch-nonincremental)
                        ;; Locally bind all isearch global vars to protect them from recursive isearching.
                        ;; isearch-string -message and -forward are not bound, so they can be changed.
                        ;; Instead, save the values.
                        (isearch-new-string                  isearch-string)
                        (orig-empty-search-string-p          (= 0 (length isearch-string)))
                        (isearch-new-message                 isearch-message)
                        (isearch-new-forward                 isearch-forward)
                        (isearch-new-regexp-function         isearch-regexp-function)
                        (isearch-new-word                    (if (boundp 'isearch-word) ; Backward compat.
                                                                 isearch-word
                                                               isearch-regexp-function))
                        (isearch-new-case-fold               isearch-case-fold-search)
                        (isearch-regexp                      isearch-regexp)
                        (isearch-op-fun                      isearch-op-fun)
                        (isearch-cmds                        isearch-cmds)
                        (isearch-success                     isearch-success)
                        (isearch-wrapped                     isearch-wrapped)
                        (isearch-barrier                     isearch-barrier)
                        (isearch-adjusted                    isearch-adjusted)
                        (isearch-yank-flag                   isearch-yank-flag)
                        (isearch-error                       isearch-error)
                        (multi-isearch-file-list-new         (and (boundp 'multi-isearch-file-list)
                                                                  multi-isearch-file-list))
                        (multi-isearch-buffer-list-new       (and (boundp 'multi-isearch-buffer-list)
                                                                  multi-isearch-buffer-list))
                        (multi-isearch-next-buffer-function  (and (boundp
                                                                   'multi-isearch-next-buffer-current-function)
                                                                  multi-isearch-next-buffer-current-function))
                        (multi-isearch-current-buffer-new    (and (boundp 'multi-isearch-current-buffer)
                                                                  multi-isearch-current-buffer))
                        ;; Do not bind this.  We want `isearch-search', below, to set it.  And the old value
                        ;; does not matter after that.
                        ;; (isearch-other-end               isearch-other-end)
                        ;;
                        ;; Perhaps some of these other variables should be bound for a shorter period,
                        ;; ending before the next isearch-search.  But there doesn't seem to be a real bug,
                        ;; so let's not risk it now.
                        (isearch-opoint                      isearch-opoint)
                        (isearch-slow-terminal-mode          isearch-slow-terminal-mode)
                        (isearch-small-window                isearch-small-window)
                        (isearch-recursive-edit              isearch-recursive-edit)
                        ;; Save the current configuration so we can restore it.
                        (isearch-window-configuration        (current-window-configuration))
                        ;; This could protect the index of the search rings, but we can't reliably count the
                        ;; number of typed `M-p' in `read-from-minibuffer' to adjust the index accordingly.
                        ;; So when the following is commented out, `isearch-mode' below resets the index to
                        ;; the predictable value nil.
                        ;; (search-ring-yank-pointer        search-ring-yank-pointer)
                        ;; (regexp-search-ring-yank-pointer regexp-search-ring-yank-pointer)

                        ;; Temporarily restore `ring-bell-function'.
                        (ring-bell-function                  isearchp-orig-ring-bell-fn)
                        (isearchp-orig-ring-bell-fn          isearchp-orig-ring-bell-fn)
                        (isearch-filter-predicate            (and (boundp 'isearch-filter-predicate)
                                                                  isearch-filter-predicate))
                        ;; Temporarily restore `minibuffer-message-timeout'.
                        (minibuffer-message-timeout          isearch-original-minibuffer-message-timeout)
                        (isearch-original-minibuffer-message-timeout
                         isearch-original-minibuffer-message-timeout)
                        old-point  old-other-end)
                    ;; Suspend isearching until BODY is done.  This is so that the user can do anything
                    ;; without failure, like switch buffers and start another isearch, and return.
                    (condition-case nil (isearch-done 'NOPUSH 'EDIT) (exit nil)) ; was recursive editing
                    ;; Save old point and `isearch-other-end' before reading from minibuffer, which can
                    ;; change their values.
                    (setq old-point      (point)
                          old-other-end  isearch-other-end)
                    (unwind-protect (progn ,@body)
                      ;; Always resume isearching by restarting it.
                      (isearch-mode isearch-forward isearch-regexp isearch-op-fun nil isearch-regexp-function)
                      ;; Copy new local values to isearch globals
                      (setq isearch-string                isearch-new-string
                            isearch-message               isearch-new-message
                            isearch-forward               isearch-new-forward
                            isearch-regexp-function       (if (boundp 'isearch-new-word)
                                                              isearch-new-word
                                                            isearch-new-regexp-function)
                            isearch-case-fold-search      isearch-new-case-fold
                            multi-isearch-current-buffer  (and (boundp 'multi-isearch-current-buffer-new)
                                                               multi-isearch-current-buffer-new)
                            multi-isearch-file-list       (and (boundp 'multi-isearch-file-list-new)
                                                               multi-isearch-file-list-new)
                            multi-isearch-buffer-list     (and (boundp 'multi-isearch-buffer-list-new)
                                                               multi-isearch-buffer-list-new))
                      ;; Restore the minibuffer message before moving point.
                      (if (and (boundp 'isearch-message-function)  isearch-message-function)
                          (funcall isearch-message-function nil t)
                        (isearch-message nil t))
                      ;; Set point at the start (end) of old match if forward (backward), so after exiting
                      ;; minibuffer isearch resumes at the start (end) of this match and can find it again.
                      (when (and old-other-end
                                 (eq old-point (point))
                                 (eq isearch-forward isearch-new-forward)
                                 (not (eq last-command 'isearchp-act-on-demand)))
                        (goto-char old-other-end)))
                    (cond (;; Newly empty `isearch-string' means use default.
                           (and (= 0 (length isearch-string))  (not orig-empty-search-string-p))
                           (setq isearch-string   (or (and isearchp-resume-with-last-when-empty-flag
                                                           (car (if isearch-regexp
                                                                    regexp-search-ring
                                                                  search-ring)))
                                                      "")
                                 isearch-message  (mapconcat 'isearch-text-char-description isearch-string ""))
                           ;; After taking the last element, adjust ring to previous one.
                           (unless (equal isearch-string "") (isearch-ring-adjust1 nil)))
                          (;; Empty string originally, so make `C-s' and `C-r' use it, by pushing it to ring.
                           orig-empty-search-string-p
                           (let ((ring  (if isearch-regexp regexp-search-ring search-ring)))
                             (push "" ring)))))
                  ;; This used to push the state as of before this `C-s', but it adds an inconsistent state
                  ;; where some of the variables are from the previous search (e.g. `isearch-success'), and
                  ;; some of the variables were just entered from the minibuffer (e.g. `isearch-string').
                  ;; (isearch-push-state)

                  (isearch-search)      ; Reinvoke the pending search.
                  (isearch-push-state)  ; Push the correct state.
                  (isearch-update)
                  (when isearch-nonincremental
                    ;; (sit-for 1) ;; needed if `isearch-done' does: (message "")
                    (isearch-done)
                    ;; The search-done message is confusing when the string is empty, so erase it.
                    (when (equal isearch-string "") (message ""))))
              ;; Handle `abort-recursive-edit' outside of `let' to restore outside global values.
              (quit (isearch-abort)))
            nil)))
    (when newpoint (setq isearch-success  newpoint))))
 
;;(@* "Commands")

;;; Commands ---------------------------------------------------------

(when (> emacs-major-version 21)        ; Emacs 22+, for `with-isearch-suspended'.

  (defun isearchp-eval-sexp-and-insert ()
    "Prompt for Lisp sexp, eval it, and append value to the search string."
    (interactive)
    (with-isearch-suspended
      (let ((sexp  (read-from-minibuffer "Eval: " nil (if (boundp 'pp-read-expression-map) ; In `pp+.el'.
                                                          pp-read-expression-map
                                                        read-expression-map)
                                         t 'read-expression-history)))
        (message "Evaluating...")
        (if (or (not (boundp 'eval-expression-debug-on-error))
                (null eval-expression-debug-on-error))
            (setq values  (cons (eval sexp) values))
          (let ((old-value  (make-symbol "t"))
                new-value)
            ;; Bind `debug-on-error' to something unique so that we can detect when evaled code changes it.
            (let ((debug-on-error  old-value))
              (setq values     (cons (eval sexp) values)
                    new-value  debug-on-error))
            ;; If eval'd code changed value of `debug-on-error', propagate change to the global binding.
            (unless (eq old-value new-value) (setq debug-on-error  new-value))))
        (setq isearch-new-string  (concat isearch-string (prin1-to-string (car values) 'NOESCAPE))))))

  (defun isearchp-act-on-demand (arg)   ; Bound to `C-M-RET' in `isearch-mode-map'.
    "Invoke the value of `isearchp-on-demand-action-function'.
This suspends Isearch, performs the action, then reinvokes Isearch.
By default, replace the search hit - see `isearchp-replace-on-demand'.
Bound to `\\<isearch-mode-map>\\[isearchp-act-on-demand]' during Isearch."
    (interactive "P")
    (let ((isearch-mode-end-hook  isearch-mode-end-hook))
      (remove-hook 'isearch-mode-end-hook 'isearchp-property-finish)
      (when (and isearch-success  (not isearch-error)  (not isearch-just-started))
        (with-isearch-suspended
          (let ((isearchp-pref-arg  arg))
            (funcall isearchp-on-demand-action-function))))))

  (defun isearchp-remove-failed-part-or-last-char () ; Bound to `C-<backspace>' in `isearch-mode-map'.
    "Remove failed part of search string, or last char if successful.
Do nothing if search string is empty to start with."
    (interactive)
    (if (equal isearch-string "")
        (isearch-update)                ; No-op.  Just redisplay prompt.
      (if isearch-success
          (isearch-delete-char)
        (while (isearch-fail-pos) (isearch-pop-state)))
      (isearch-update)))

  (defun isearchp-remove-failed-part () ; Bound to `C-M-l' in `isearch-mode-map'.
    "Remove failed part of search string, if any.
Do nothing if the search string is empty to start with or the search
was successful."
    (interactive)
    (if (equal isearch-string "")
        (isearch-update)                ; No-op.  Just redisplay prompt.
      (while (isearch-fail-pos) (isearch-pop-state))
      (isearch-update)))

  (defun isearchp-repeat-search-if-fail ()
    "On failure, restart search automatically from the search limit.
Fail only if there is no search hit within the search limits.

The limit is the buffer limit or, if the region is active and
`isearchp-restrict-to-region-flag' is non-nil, the region limit.
\(The region limit is used only for Emacs 24.3 and later.)

Note: You cannot use `DEL' (Backspace) to remove the failed portion of
      the search string.  To do that, use \\<isearch-mode-map>\
`\\[isearchp-remove-failed-part]' or `\\[isearchp-remove-failed-part-or-last-char]'."
    (if (or isearch-success  isearchp--repeat-search-if-fail-repeated)
        (setq isearchp--repeat-search-if-fail-repeated  nil)
      (unless isearchp--repeat-search-if-fail-repeated
        (setq isearch-wrapped                     t
              isearchp--repeat-search-if-fail-repeated  t)
        (if isearch-wrap-function
            (funcall isearch-wrap-function)
          (goto-char (if isearch-forward
                         (or isearchp-reg-beg  (point-min))
                       (or isearchp-reg-end  (point-max))))
          (isearch-repeat (if isearch-forward 'forward 'backward))))))

  (defun isearchp-toggle-repeat-search-if-fail () ; Bound to `M-s M-k' in `isearch-mode-map'.
    "Toggle the value of user option `isearchp-repeat-search-if-fail-flag'."
    (interactive)
    (setq isearchp-repeat-search-if-fail-flag  (not isearchp-repeat-search-if-fail-flag))
    (if isearchp-repeat-search-if-fail-flag
        (add-hook 'isearch-update-post-hook 'isearchp-repeat-search-if-fail)
      (remove-hook 'isearch-update-post-hook 'isearchp-repeat-search-if-fail))
    (message "Restart search automatically on failure is now %s"
             (if isearchp-repeat-search-if-fail-flag 'ON 'OFF))
    (sit-for 1))

  )

;;;###autoload
(defun isearchp-cycle-mismatch-removal () ; Bound to `M-k' in `isearch-mode-map'.
  "Cycle option `isearchp-drop-mismatch'.
See also option `isearchp-drop-mismatch-regexp-flag'."
  (interactive)
  (setq isearchp-drop-mismatch  (case isearchp-drop-mismatch
                                  (replace-last  nil)
                                  ((nil)         t)
                                  (otherwise     'replace-last)))
  (if (and isearchp-drop-mismatch  (not (eq 'replace-last isearchp-drop-mismatch)))
      (add-hook 'isearch-update-post-hook 'isearchp-remove-mismatch)
    (remove-hook 'isearch-update-post-hook 'isearchp-remove-mismatch))
  (case isearchp-drop-mismatch
    (replace-last  (message "Automatic REPLACEMENT of last mismatched input is now ON"))
    ((nil)         (message "Automatic removal of mismatched input is now OFF"))
    (otherwise     (message "Automatic removal of ALL mismatched input is now ON")))
  (sit-for 1)
  (isearch-update))

(defun isearchp-remove-mismatch ()
  "Remove the mismatched portion of the search string.
Do nothing when regexp searching and option
`isearchp-drop-mismatch-regexp-flag' is nil."
  (when (or isearchp-drop-mismatch-regexp-flag  (not isearch-regexp))
    (while (or (not isearch-success)  (if (< emacs-major-version 22) isearch-invalid-regexp isearch-error))
      (isearch-pop-state))
    (remove-hook 'isearch-update-post-hook 'isearchp-remove-mismatch)
    (isearch-update)
    (add-hook 'isearch-update-post-hook 'isearchp-remove-mismatch)))


(when (< emacs-major-version 25)


  ;; REPLACE ORIGINAL in `isearch.el' (Emacs 22-24).
  ;;
  ;; 1. Turn off `isearch-regexp' when `isearch-word'.
  ;; 2. Show message about new state.
  ;;
  ;; From Juri Linkov, 2006-10-29, to emacs-devel@gnu.org
  ;; From Stefan Monnier, 2006-11-23, to help-gnu-emacs@gnu.org
  ;;
  (defun isearch-toggle-word ()         ; Bound to `M-s w' in `isearch-mode-map'.
    "Toggle word searching on or off."
    ;; The status stack is left unchanged.
    (interactive)
    (setq isearch-word  (if (eq isearch-word t) nil t))
    (when isearch-word (setq isearch-regexp  nil)) ; Added to Juri's code by Stefan.
    (setq isearch-success   t
          isearch-adjusted  t)
    (message "Whole word search is now %s" (if isearch-word 'ON 'OFF))
    (sit-for 1)
    (isearch-update))

  )


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Respect `isearchp-toggle-option-flag', possibly toggling option `isearchp-case-fold'.
;; 2. Added prefix arg to flip handling of `isearchp-toggle-option-flag'.
;; 3. Update minor-mode mode-line lighter to reflect case sensitivity.
;;
;;;###autoload
(defun isearch-toggle-case-fold (flip)  ; Bound to `M-c' in `isearch-mode-map'.
  "Toggle case sensitivity on or off during incremental searching.
The minor-mode lighter shows `ISEARCH' for case-insensitive, `Isearch'
for case-sensitive.

If `isearchp-toggle-option-flag' is non-nil then toggle the value of
option `isearchp-case-fold'.  If it is nil then toggle the behavior
only temporarily, so that the option value is unchanged for subsequent
searches.

A prefix argument flips the sense of the last paragraph, so that the
option is updated only if `isearchp-toggle-option-flag' is nil instead
of non-nil.

To use a prefix argument you must set either `isearch-allow-scroll' or
`isearch-allow-prefix' (if available) to non-nil.  Otherwise, a prefix
arg during Isearch exits Isearch.

When toggling case-sensitive searching on, restores the last behavior
according to option `isearchp-case-fold': t or `yes'."
  (interactive "P")
  (let ((current-only-p  (or (and (not isearchp-toggle-option-flag)  (not flip))
                             (and isearchp-toggle-option-flag  flip))))
    (if current-only-p
        (setq isearch-case-fold-search  (if isearch-case-fold-search
                                            nil
                                          (or isearchp-case-fold  t)))
      (when isearchp-case-fold (setq isearchp-last-non-nil-case-fold  isearchp-case-fold))
      (setq isearchp-case-fold   (if isearchp-case-fold nil isearchp-last-non-nil-case-fold)
            isearch-case-fold-search  isearchp-case-fold))
    (let ((message-log-max  nil))
      (message "%s%s [case %ssensitive%s]"
               (isearchp-message-prefix nil nil isearch-nonincremental)
               isearch-message
               (if isearch-case-fold-search "IN" "")
               (if (not current-only-p) " FROM NOW ON" ""))))
  (setq isearch-success   t
        isearch-adjusted  t)
  (isearchp-highlight-lighter)
  (sit-for 1)
  (isearch-update))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Respect `isearchp-toggle-option-flag', possibly toggling option `search-invisible'.
;; 2. Added prefix arg to flip handling of `isearchp-toggle-option-flag'.
;;
;;;###autoload
(defun isearch-toggle-invisible (flip)  ; Bound to `M-s i'.
  "Toggle searching in invisible text on or off.
If `isearchp-toggle-option-flag' is non-nil then toggle the value of
option `search-invisible'.  If it is nil then toggle the behavior only
temporarily, so that the option value is unchanged for subsequent
searches.

A prefix argument flips the sense of the last paragraph, so that the
option is updated only if `isearchp-toggle-option-flag' is nil instead
of non-nil.

To use a prefix argument you must set either `isearch-allow-scroll' or
`isearch-allow-prefix' (if available) to non-nil.  Otherwise, a prefix
arg during Isearch exits Isearch.

When toggling invisible searching on, restores the last behavior
according to option `search-invisible': t or `open'."
  (interactive "P")
  (let ((current-only-p  (or (and (not isearchp-toggle-option-flag)  (not flip))
                             (and isearchp-toggle-option-flag  flip))))
    (if current-only-p
        (setq isearch-invisible  (if isearch-invisible nil (or search-invisible  'open)))
      (when search-invisible (setq isearchp-last-non-nil-invisible  search-invisible))
      (setq search-invisible   (if search-invisible nil isearchp-last-non-nil-invisible)
            isearch-invisible  search-invisible))
    (let ((message-log-max  nil))
      (message "%s%s [match %s text%s]" (isearch-message-prefix nil isearch-nonincremental)
               isearch-message (if isearch-invisible "INvisible" "only VISIBLE")
               (if (not current-only-p) " FROM NOW ON" ""))))
  (setq isearch-success   t
        isearch-adjusted  t)
  (sit-for 1)
  (isearch-update))

;;;###autoload
(defun isearchp-toggle-search-invisible () ; Bound to `C-+' in `isearch-mode-map'.
  "Toggle the value of user option `search-invisible'.
Toggles between nil and the last non-nil value."
  (interactive)
  (when search-invisible (setq isearchp-last-non-nil-invisible  search-invisible))
  (setq search-invisible   (if search-invisible nil isearchp-last-non-nil-invisible)
        isearch-invisible  search-invisible)
  (message "Option `search-invisible' is now `%s'" (case search-invisible
                                                     (open  'OPEN)
                                                     ((nil) 'OFF)
                                                     (t     'ON)))
  (setq isearch-success   t
        isearch-adjusted  t)
  (sit-for 1)
  (isearch-update))

;;;###autoload
(defun isearchp-toggle-option-toggle () ; Bound to `M-s v' in `isearch-mode-map'.
  "Toggle the value of option `isearchp-toggle-option-flag'."
  (interactive)
  (setq isearchp-toggle-option-flag  (not isearchp-toggle-option-flag))
  (message "Option `isearchp-toggle-option-flag' is now %s" (if isearchp-toggle-option-flag 'ON 'OFF))
  (sit-for 1))

;;;###autoload
(defun isearchp-toggle-regexp-quote-yank () ; Bound to `C-`' in `isearch-mode-map'.
  "Toggle `isearchp-regexp-quote-yank-flag'."
  (interactive)
  (setq isearchp-regexp-quote-yank-flag  (not isearchp-regexp-quote-yank-flag))
  (message "Escaping regexp special chars for yank is now %s" (if isearchp-regexp-quote-yank-flag 'ON 'OFF))
  (sit-for 1)
  (isearch-update))

;;;###autoload
(defun isearchp-toggle-set-region ()    ; Bound to `M-s M-SPC' in `isearch-mode-map'.
  "Toggle `isearchp-set-region-flag'."
  (interactive)
  (setq isearchp-set-region-flag  (not isearchp-set-region-flag))
  (message "Setting region around search target is now %s" (if isearchp-set-region-flag 'ON 'OFF))
  (sit-for 1)
  (isearch-update))

;; Dynamic search filtering.
(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3)))

  (defun isearchp-toggle-auto-keep-filter-predicate () ; Bound to `C-z S' in `isearch-mode-map'.
    "Toggle `isearchp-auto-keep-filter-predicate-flag'.
If turning it on, save it now (for this Emacs session).
Note that turning it off does not reset `isearch-filter-predicate'.
Use \\<isearch-mode-map>`\\[isearchp-reset-filter-predicate]' to do that."
    (interactive)
    (setq isearchp-auto-keep-filter-predicate-flag  (not isearchp-auto-keep-filter-predicate-flag))
    (when isearchp-auto-keep-filter-predicate-flag (setq isearchp-kept-filter-predicate  isearch-filter-predicate))
    (message "Automatic saving of filter-predicate changes (for this Emacs session) is now %s"
             (if isearchp-auto-keep-filter-predicate-flag 'ON 'OFF))
    (sit-for 1)
    (isearch-update))

  (defun isearchp-toggle-dimming-filter-failures () ; Bound to `M-s h d' in `isearch-mode-map'.
    "Toggle option `isearchp-lazy-dim-filter-failures-flag'."
    (interactive)
    (customize-set-value 'isearchp-lazy-dim-filter-failures-flag
                         (not isearchp-lazy-dim-filter-failures-flag))
    (message "Lazy filter-failure highlighting is now %s"
             (if isearchp-lazy-dim-filter-failures-flag 'ON 'OFF))
    (sit-for 1)
    (isearchp-redo-lazy-highlighting))

  (defun isearchp-toggle-showing-filter-prompt-prefixes () ; Bound to `C-z p' in `isearch-mode-map'.
    "Toggle `isearchp-show-filter-prompt-prefixes-flag'."
    (interactive)
    (setq isearchp-show-filter-prompt-prefixes-flag  (not isearchp-show-filter-prompt-prefixes-flag))
    (message "Showing filter-predicate prefixes in prompt is now %s"
             (if isearchp-show-filter-prompt-prefixes-flag 'ON 'OFF))
    (sit-for 1)
    (isearch-update))

  (defun isearchp-toggle-region-restriction () ; Bound to `C-x n' in `isearch-mode-map'.
    "Toggle option `isearchp-restrict-to-region-flag'."
    (interactive)
    (setq isearchp-restrict-to-region-flag  (not isearchp-restrict-to-region-flag))
    (message "Restricting search to active region is now %s" (if isearchp-restrict-to-region-flag 'ON 'OFF))
    (sit-for 1)
    (isearch-update))

  )

;;;###autoload
(defun isearchp-set-region-around-search-target ()
  "Set the region around the last search or query-replace target."
  (interactive)
  (case last-command
    ((isearch-forward isearch-backward isearch-forward-regexp isearch-backward-regexp)
     (push-mark isearch-other-end t 'activate))
    (t (push-mark (match-beginning 0) t 'activate)))
  (setq deactivate-mark  nil))

(defun isearchp-message-prefix (&optional arg1 arg2 arg3)
  "Version of `isearch-message-prefix' that works for all Emacs releases."
  (if (or (< emacs-major-version 24)
          (and (= emacs-major-version 24)  (< emacs-minor-version 3)
               (not (string-match "^[0-9]+\\.[0-9]+\\.[0-9]+" emacs-version))))
      (isearch-message-prefix arg1 arg2 arg3) ; Emacs 20 through 24.2.
    (isearch-message-prefix arg1 arg2))) ; Emacs 24.1.N and 24.3+

(defun isearchp-message-suffix (&optional arg1 arg2)
  "Version of `isearch-message-suffix' that works for all Emacs releases."
  (if (or (< emacs-major-version 24)
          (and (= emacs-major-version 24)  (< emacs-minor-version 3)
               (not (string-match "^[0-9]+\\.[0-9]+\\.[0-9]+" emacs-version))))
      (isearch-message-suffix arg1 arg2) ; Emacs 20 through 24.2.
    (isearch-message-suffix arg1)))     ; Emacs 24.1.N and  24.3+


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. End Isearch: does `isearch-done' and `isearch-clean-overlays' instead of `isearch-update'.
;; 2. List isearch bindings too.
;;
;;;###autoload
(defun isearch-mode-help ()             ; Bound to `C-h' in `isearch-mode-map'.
  "Display information on interactive search in buffer *Help*."
  (interactive)
  (describe-function 'isearch-forward)
  (isearch-done)
  (isearch-clean-overlays)
  (with-current-buffer "*Help*"
    (goto-char (point-max))
    (let ((buffer-read-only  nil)) (insert (substitute-command-keys "

Bindings in Isearch minor mode:
------------------------------

\\{isearch-mode-map}")))))


(when (> emacs-major-version 21)        ; Emacs 22+


  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; 1. Start with point at the mismatch position - use `isearchp-message-prefix'.
  ;; 2. Use macro `with-isearch-suspended'.
  ;;
  (defun isearch-edit-string ()         ; Bound to `M-e' in `isearch-mode-map'.
    "Edit the search string in the minibuffer.
The following additional command keys are active while editing.
\\<minibuffer-local-isearch-map>
\\[exit-minibuffer] to resume incremental searching with the edited string.
\\[isearch-forward-exit-minibuffer] to resume isearching forward.
\\[isearch-reverse-exit-minibuffer] to resume isearching backward.
\\[isearch-complete-edit] to complete the search string using the search ring.
\\[insert-char] to insert a Unicode character by name (with completion)."
    (interactive)
    (with-isearch-suspended
      (let* ((message-log-max            nil)
             ;; Do not add a new search string to the search ring here in `read-from-minibuffer'.
             ;; It should be added only by `isearch-update-ring', called from `isearch-done'.
             (history-add-new-input      nil)
             (minibuffer-history-symbol  nil)) ; Workaround for some incompatibility with `gmhist'.
        ;; FREE VARS here: `isearch-new-string', `isearch-new-message'.
        ;; Bound in `with-isearch-suspended'.
        (setq isearch-new-string   (read-from-minibuffer
                                    (isearchp-message-prefix nil nil isearch-nonincremental)
                                    (cons isearch-string (1+ (or (isearch-fail-pos)
                                                                 (length isearch-string))))
                                    minibuffer-local-isearch-map  nil
                                    (if isearch-regexp
                                        (cons 'regexp-search-ring
                                              (1+ (or regexp-search-ring-yank-pointer  -1)))
                                      (cons 'search-ring (1+ (or search-ring-yank-pointer  -1))))
                                    nil t)
              isearch-new-message  (mapconcat 'isearch-text-char-description isearch-new-string "")))))

  ;; Suggested by Juri Linkov: http://lists.gnu.org/archive/html/emacs-devel/2012-12/msg00281.html.
  ;;
  (defun isearchp-open-recursive-edit () ; Bound to `C-x o' in `isearch-mode-map'.
    "Invoke the editor command loop recursively, during Isearch.
Use `\\[exit-recursive-edit]' to end the recursive edit and resume searching from there.
Or use `abort-recursive-edit' to exit the recursive edit and cancel the previous search."
    (interactive)
    (with-isearch-suspended (recursive-edit)))

  )

(when (< emacs-major-version 22)        ; Emacs 20-21.

  ;; Parts of the definition were taken from later Emacs versions, but are compatible with Emacs 20.
  (defun isearch-edit-string ()
    "Edit the search string in the minibuffer.
The following additional command keys are active while editing.
\\<minibuffer-local-isearch-map>
\\[exit-minibuffer] to resume incremental searching with the edited string.
\\[isearch-nonincremental-exit-minibuffer] to do one nonincremental search.
\\[isearch-forward-exit-minibuffer] to resume isearching forward.
\\[isearch-reverse-exit-minibuffer] to resume isearching backward.
\\[isearch-complete-edit] to complete the search string using the search ring.
\\<isearch-mode-map>
If first char entered is \\[isearch-yank-word], then do word search instead."
    ;; This code is very hairy for several reasons, explained in the code.
    ;; Mainly, `isearch-mode' must be terminated while editing and then restarted.
    ;; If there were a way to catch any change of buffer from the minibuffer, this could be
    ;; simplified greatly.
    ;; This code does not back up the search point. Should it, for use with `isearch-edit-string'?
    (interactive)
    (condition-case nil
        (progn (let ((isearch-nonincremental        isearch-nonincremental)
                     ;; Locally bind all isearch global variables to protect them from recursive
                     ;; isearching.  Do not bind `isearch-string', `isearch-message', and
                     ;; `isearch-forward', so they can be changed.  Instead, save their values.
                     (isearch-new-string            isearch-string)
                     (isearch-new-message           isearch-message)
                     (isearch-new-forward           isearch-forward)
                     (isearch-new-regexp-function   isearch-regexp-function)
                     (isearch-new-word              (if (boundp 'isearch-word) ; For backward compatibility
                                                        isearch-word
                                                      isearch-regexp-function))
                     (isearch-regexp                isearch-regexp)
                     (isearch-op-fun                isearch-op-fun)
                     (isearch-cmds                  isearch-cmds)
                     (isearch-success               isearch-success)
                     (isearch-wrapped               isearch-wrapped)
                     (isearch-barrier               isearch-barrier)
                     (isearch-adjusted              isearch-adjusted)
                     (isearch-yank-flag             isearch-yank-flag)
                     (isearch-invalid-regexp        isearch-invalid-regexp)
                     (isearch-within-brackets       isearch-within-brackets)
            ;;; Do not bind this.  We want isearch-search, below, to set it.
            ;;; And the old value won't matter after that.
            ;;;      (isearch-other-end             isearch-other-end)
            ;;; Perhaps some of these other variables should be bound for a
            ;;; shorter period, ending before the next isearch-search.
            ;;; But there doesn't seem to be a real bug, so let's not risk it now.
                     (isearch-opoint                isearch-opoint)
                     (isearch-slow-terminal-mode    isearch-slow-terminal-mode)
                     (isearch-small-window          isearch-small-window)
                     (isearch-recursive-edit        isearch-recursive-edit)
                     ;; Save current configuration so we can restore it here.
                     (isearch-window-configuration  (current-window-configuration))
                     old-point old-other-end)
                 ;; Actually terminate isearching until editing is done.
                 ;; This is so that the user can do anything without failure,
                 ;; like switch buffers and start another isearch, and return.
                 (condition-case nil (isearch-done 'NOPUSH 'EDIT) (exit nil)) ; was recursive editing
                 ;; Save old point and `isearch-other-end' before reading from minibuffer, which
                 ;; can change their values.
                 (setq old-point      (point)
                       old-other-end  isearch-other-end)
                 (unwind-protect
                      (let* ((message-log-max            nil)
                             ;; Binding `minibuffer-history-symbol' to nil is a workaround for some
                             ;; incompatibility with `gmhist'.
                             (minibuffer-history-symbol  nil))
                        (setq isearch-new-string   (read-from-minibuffer
                                                    (isearchp-message-prefix nil nil
                                                                             isearch-nonincremental)
                                                    (cons isearch-string (1+ (length isearch-string)))
                                                    minibuffer-local-isearch-map nil
                                                    (if isearch-regexp
                                                        (cons 'regexp-search-ring
                                                              (1+ (or regexp-search-ring-yank-pointer
                                                                      -1)))
                                                      (cons 'search-ring
                                                            (1+ (or search-ring-yank-pointer  -1))))
                                                    nil t)
                              isearch-new-message  (mapconcat 'isearch-text-char-description
                                                              isearch-new-string "")))
                   ;; Set point at the start (end) of old match if forward (backward),
                   ;; so after exiting minibuffer isearch resumes at the start (end)
                   ;; of this match and can find it again.
                   (if (and old-other-end  (eq old-point (point))  (eq isearch-forward isearch-new-forward))
                       (goto-char old-other-end))
                   ;; Always resume isearching by restarting it.
                   (isearch-mode isearch-forward isearch-regexp isearch-op-fun nil isearch-regexp-function)
                   ;; Copy new local values to isearch globals
                   (setq isearch-string           isearch-new-string
                         isearch-message          isearch-new-message
                         isearch-forward          isearch-new-forward
                         isearch-regexp-function  (if (boundp 'isearch-new-word)
                                                      isearch-new-word
                                                    isearch-new-regexp-function)))
                 ;; Empty isearch-string means use default.
                 (if (= 0 (length isearch-string))
                     (setq isearch-string   (or (car (if isearch-regexp
                                                         regexp-search-ring
                                                       search-ring))
                                                "")
                           isearch-message  (mapconcat 'isearch-text-char-description
                                                       isearch-string ""))
                   ;; This used to set the last search string, but it is not right to do that here.
                   ;; Only the string actually used should be saved.
                   ))

               ;; This used to push the state as of before this `C-s', but it adds an inconsistent
               ;; state where some of variables are from the previous search (e.g.
               ;; `isearch-success'), and some of variables are just entered from the minibuffer
               ;; (e.g. `isearch-string').
               ;; (isearch-push-state)

               (isearch-search)         ; Reinvoke the pending search.
               (isearch-push-state)     ; Push the correct state.
               (isearch-update)
               (when isearch-nonincremental
                 ;; (sit-for 1) ;; needed if `isearch-done' does: (message "")
                 (isearch-done)
                 ;; The search done message is confusing when the string
                 ;; is empty, so erase it.
                 (if (equal isearch-string "")
                     (message ""))))
      ;; Handle `abort-recursive-edit' outside of let to restore outside global values.
      (quit (isearch-abort))))

  )

(when (and (> emacs-major-version 22)   ; Emacs 23 (bc supports Unicode) through 24.2
           (or (< emacs-major-version 24)  (and (= emacs-major-version 24)  (< emacs-minor-version 4))))

  (defun isearch-char-by-name (&optional count)
    "Read a character by its Unicode name and append it to the search string.
Completion is available as in `read-char-by-name', used by `insert-char'.
With a numeric prefix arg, append that many copies of the character."
    (interactive "p")
    (with-isearch-suspended
      (let ((char  (read-char-by-name "Append char to search string (Unicode name or hex): ")))
        (when char
          (let ((string  (if (and (integerp count)  (> count 1))
                             (make-string count char)
                           (char-to-string char))))
            (setq isearch-new-string   (concat isearch-string string)
                  isearch-new-message  (concat isearch-message (mapconcat 'isearch-text-char-description
                                                                          string ""))))))))

  )

(when (fboundp 'isearch-yank-internal)  ; Emacs 22+
  (defun isearchp-yank-char ()          ; Bound to `C-y C-c' in `isearch-mode-map'.
    "Yank next character from buffer onto search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearch-yank-char))

  (defun isearchp-yank-word-or-char ()  ; Bound to `C-w' and `C-y C-w' in `isearch-mode-map'.
    "Yank next word or character from buffer onto search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearch-yank-word-or-char))

  (defun isearchp-yank-line ()          ; Bound to `C-y C-e' in `isearch-mode-map'.
    "Yank text from buffer up to end of line onto search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearch-yank-line))

  (defun isearchp-yank-symbol-or-char-1 ()
    "Helper for `isearchp-yank-symbol-or-char'.
Not intended/needed as a user command."
    (interactive)
    (isearch-yank-internal
     (lambda ()
       (if (or (memq (char-syntax (or (char-after)  0)) '(?w ?_))
               (memq (char-syntax (or (char-after (1+ (point)))  0)) '(?w ?_)))
           (if (and (boundp 'subword-mode)  subword-mode) (subword-forward 1) (forward-symbol 1))
         (forward-char 1))
       (point))))

  (defun isearchp-yank-symbol-or-char () ; Bound to `C-_' and `C-y C-_' in `isearch-mode-map'.
    "Yank char, subword, word, or symbol from buffer into search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearchp-yank-symbol-or-char-1))

  (defun isearchp-yank-sexp-symbol-or-char-1 ()
    "Helper function for `isearchp-yank-sexp-symbol-or-char'.
Not intended/needed as a user command."
    (interactive)
    (isearch-yank-internal
     (lambda ()
       (if (or (= (char-syntax (or (char-after)  0)) ?\( )
               (= (char-syntax (or (char-after (1+ (point)))  0)) ?\( ))
           (forward-sexp 1)
         (if (or (memq (char-syntax (or (char-after)  0)) '(?w ?_))
                 (memq (char-syntax (or (char-after (1+ (point)))  0)) '(?w ?_)))
             (if (and (boundp 'subword-mode)  subword-mode) (subword-forward 1) (forward-symbol 1))
           (forward-char 1)))
       (point))))

  (defun isearchp-yank-sexp-symbol-or-char () ; Bound to `C-(' and `C-y C-(' in `isearch-mode-map'.
    "Yank sexp, symbol, subword, word, or char into search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearchp-yank-sexp-symbol-or-char-1)))

(defun isearchp-kill-ring-save ()       ; Bound to `M-w' in `isearch-mode-map'.
  "Copy the current search string to the kill ring.
For example, you can then use `C-s M-y' to search for the same thing
in another Emacs session."
  (interactive)
  (kill-new isearch-string)
  (let ((message-log-max  nil)) (message "Copied search string as kill"))
  (sit-for 1)
  (isearch-update))

(defun isearchp-append-register ()      ; Bound to `C-x r g', the same as `insert-register' globally.
  "Insert register contents at point in search string.
You are prompted for the register to use."
  (interactive)
  (let ((current-prefix-arg  t)
        string)
    (with-temp-buffer
      (call-interactively 'insert-register)
      (setq string  (buffer-substring (point-min) (point-max))))
    (isearch-yank-string string)))

(defun isearchp-retrieve-last-quit-search () ; Bound to `M-g' in `isearch-mode-map'.
  "Insert last successful search string from when you hit `C-g' in Isearch.
Bound to `\\<isearch-mode-map>\\[isearchp-retrieve-last-quit-search]' during Isearch."
  (interactive)
  (cond ((and isearch-regexp  isearchp-last-quit-regexp-search)
         (let ((isearchp-regexp-quote-yank-flag  nil))
           (isearch-yank-string isearchp-last-quit-regexp-search)))
        (isearchp-last-quit-search
         (isearch-yank-string isearchp-last-quit-search))))

(when (> emacs-major-version 20)

  (defun isearchp-fontify-buffer-now ()
    "Fontify buffer completely, right now.
This differs from `font-lock-fontify-buffer', which is lazy and does
not necessarily fontify the whole buffer."
    (interactive)
    (jit-lock-fontify-now))

  )


;; Poor man's version of `icicle-isearch-complete'.  Bound to `M-TAB' (`ESC TAB'), `C-M-i'.
;;
(unless (featurep 'icicles)

  ;; Similar to `icicle-isearch-complete'.
  (defun isearchp-complete ()           ; Bound to `M-TAB', `C-M-TAB' in `isearch-mode-map'.
    "Complete the search string using candidates from the search ring."
    (interactive)
    (isearchp-complete-past-string)
    (setq isearch-message  (mapconcat 'isearch-text-char-description isearch-string ""))
    (isearch-edit-string))

  ;; Similar to `icicle-isearch-complete-past-string'.
  (defun isearchp-complete-past-string ()
    "Set `isearch-string' to a past search string chosen by completion."
    (isearch-done 'NOPUSH)
    (let ((completion-ignore-case                 case-fold-search)
          (enable-recursive-minibuffers           t)
          (ring-var                               (if isearch-regexp 'regexp-search-ring 'search-ring)))
      (setq isearch-string  (completing-read "Search string (completing): "
                                             (mapcar #'list (isearchp-remove-duplicates
                                                             (symbol-value ring-var)))
                                             nil nil isearch-string ring-var))))

  (define-key isearch-mode-map [remap isearch-complete] 'isearchp-complete)

  )

(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3)))

  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; Keep any advice of `isearch-filter-predicate'.  But keep it only for the duration of query-replacing,
  ;; unless `isearchp-auto-keep-filter-predicate-flag' is non-nil.
  ;;
  (defun isearch-query-replace (&optional arg regexp-flag)
    "Start `query-replace' with string to replace from last search string.
The ARG (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.  A negative prefix
arg means replace backward.  Note that using the prefix arg
is possible only when `isearch-allow-scroll' is non-nil or
`isearch-allow-prefix' is non-nil, and it doesn't always provide the
correct matches for `query-replace', so the preferred way to run word
replacements from Isearch is `M-s w ... M-%'."
    (interactive "P")
    (barf-if-buffer-read-only)
    (when regexp-flag (setq isearch-regexp  t))
    (let ((orig-auto-keep-filter  isearchp-auto-keep-filter-predicate-flag))
      (unwind-protect
           (let ((isearch-filter-predicate        isearch-filter-predicate)
                 (isearchp-kept-filter-predicate  isearch-filter-predicate))
             (unless orig-auto-keep-filter (isearchp-toggle-auto-keep-filter-predicate))
             (let ((case-fold-search                isearch-case-fold-search)
                   ;; Bind `search-upper-case' to nil, to prevent calling `isearch-no-upper-case-p'
                   ;; in `perform-replace'.
                   (search-upper-case               nil)
                   (search-invisible                isearch-invisible)
                   (replace-lax-whitespace          isearch-lax-whitespace)
                   (replace-regexp-lax-whitespace   isearch-regexp-lax-whitespace)
                   (delimited                       (and arg  (not (eq arg '-))))
                   (backward                                  (and arg  (eq arg '-)))
                   ;; Bind `isearch-recursive-edit' to nil, to prevent calling `exit-recursive-edit' in
                   ;; `isearch-done' that terminates the execution of this command when
                   ;; `isearch-recursive-edit' is non-nil.
                   ;; We call `exit-recursive-edit' explicitly at the end, below.
                   (isearch-recursive-edit          nil))
               (isearch-done nil t)
               (isearch-clean-overlays)
               (when (and isearch-other-end
                          (if backward (> isearch-other-end (point)) (< isearch-other-end (point)))
                          (not (and transient-mark-mode mark-active
                                    (if backward (> (mark) (point)) (< (mark) (point))))))
                 (goto-char isearch-other-end))
               (set query-replace-from-history-variable
                    (cons isearch-string (symbol-value query-replace-from-history-variable)))
               (perform-replace
                isearch-string
                (query-replace-read-to
                 isearch-string
                 (concat "Query replace"
                         (and (or delimited  isearch-word)
                              (let* ((symbol  (or delimited  isearch-word))
                                     (string  (and symbol  (symbolp symbol)
                                                   (get symbol 'isearch-message-prefix))))
                                (if (stringp string)
                                    (replace-regexp-in-string ; Move space from end to beginning.
                                     "\\(.*\\) \\'" " \\1" string)
                                  " word")))
                         (and isearch-regexp  " regexp")
                         (and backward  " backward")
                         (and transient-mark-mode mark-active)  " in region")
                 isearch-regexp)
                t isearch-regexp (or delimited  isearch-word) nil nil
                (and transient-mark-mode  mark-active  (region-beginning))
                (and transient-mark-mode  mark-active  (region-end))
                backward))
             (and isearch-recursive-edit  (exit-recursive-edit)))
        (when (and isearchp-auto-keep-filter-predicate-flag  (not orig-auto-keep-filter))
          (setq isearchp-auto-keep-filter-predicate-flag  nil)
          (isearch-done)
          (isearch-clean-overlays)))))

  )

;; If you use a separate `*Help*' frame then `describe-prefix-bindings' (on MS Windows) during Isearch loses the
;; focus to that newly created frame.
;;
(defun isearchp-describe-prefix-bindings ()
  "Same as `describe-prefix-bindings', but keep the same frame focused."
  (interactive)
  (save-selected-window (describe-prefix-bindings)))



;;; $$$$$$ No longer used.  `M-e' puts point at this position automatically.
;;;   (defun isearchp-goto-success-end ()   ; `M-e' in `minibuffer-local-isearch-map'.
;;;     "Go to end of search string text that matches."
;;;     (interactive)
;;;     (goto-char (point-max))
;;;     (let ((cmds  isearch-cmds)
;;;           succ-msg)
;;;       (when (or (not isearch-success)  isearch-error)
;;;         (while (or (not (isearch-success-state (car cmds)))  (isearch-error-state (car cmds)))
;;;           (pop cmds))
;;;         (setq succ-msg  (and cmds  (isearch-message-state (car cmds))))
;;;         (backward-char (- (length isearch-string) (length succ-msg)))))))
 
;;(@* "Non-Interactive Functions")

;;; Non-Interactive Functions


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Highlight 1-8 regexp group levels, within each search hit.
;;
(defun isearch-highlight (beg end)
  (when search-highlight
    (if isearch-overlay
        ;; Overlay already exists, just move it.
        (move-overlay isearch-overlay beg end (current-buffer))
      ;; Overlay doesn't exist, create it.
      (setq isearch-overlay (make-overlay beg end))
      ;; 1001 is higher than lazy's 1000 and ediff's 100+
      (overlay-put isearch-overlay 'priority 1001)
      (overlay-put isearch-overlay 'face isearch-face))

    ;; Regexp-group highlighting - Emacs 24.4+.
    (when (and (boundp 'isearchp-highlight-regexp-group-levels-flag)
               isearchp-highlight-regexp-group-levels-flag
               isearch-regexp
               isearch-success
               (not isearch-error))
      (while isearchp-regexp-level-overlays
        (delete-overlay (car isearchp-regexp-level-overlays))
        (setq isearchp-regexp-level-overlays  (cdr isearchp-regexp-level-overlays)))
      ;; Highlight each regexp group differently.
      (save-match-data
        (let ((level         1)
              (max-levels    (min (regexp-opt-depth isearch-string) 8))
              (ise-priority  (or (overlay-get isearch-overlay 'priority) ; `isearch-overlay' is 1001.
                                 1001)))
          (save-excursion
            (goto-char beg)
            (when (looking-at isearch-string)
              (condition-case nil
                  (while (<= level max-levels)
                    (unless (equal (match-beginning level) (match-end level))
                      (let ((ov  (make-overlay (match-beginning level) (match-end level))))
                        (push ov isearchp-regexp-level-overlays)
                        (overlay-put ov 'priority (+ ise-priority 200 level))
                        (overlay-put ov 'face (intern (concat "isearchp-regexp-level-"
                                                              (number-to-string level))))))
                    (setq level  (1+ level)))
                (error nil)))))))))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Added unused arg (needed only for Emacs 20).  Added doc string.
;; Delete overlays in `isearchp-regexp-level-overlays'.
;;
(defun isearch-dehighlight (&rest __)
  "Delete `isearch-overlay' and overlays in `isearchp-regexp-level-overlays'."
  (when isearch-overlay (delete-overlay isearch-overlay))
  (when (boundp 'isearchp-regexp-level-overlays)
    (while isearchp-regexp-level-overlays
      (delete-overlay (car isearchp-regexp-level-overlays))
      (setq isearchp-regexp-level-overlays  (cdr isearchp-regexp-level-overlays)))))


;; ADVISE `isearch-update' to run `isearch-update-post-hook', in Emacs 20-21.
(when (< emacs-major-version 22)

  (defadvice isearch-update (after run-isearch-update-post-hook activate)
    "Run `isearch-update-post-hook' at the end."
    (run-hooks 'isearch-update-post-hook))

  )


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Run `isearchp-nomodify-action-hook' if successful (Emacs 22+).
;; Call `isearchp-noprompt-action-function' if successful.
;;
(when (> emacs-major-version 21)        ; Emacs 22+, for `with-isearch-suspended'.

  (defun isearch-update ()
    "This is called after every isearch command, to update the display.
After visiting a search hit, run `isearchp-nomodify-action-hook'
 (Emacs 22+) and invoke `isearchp-noprompt-action-function'.
At the end, run `isearch-update-post-hook' and lazy-highlight again."
    (unless (or (not (boundp 'isearch--current-buffer)) ; Emacs < 25+
                (eq (current-buffer) isearch--current-buffer))
      (when (buffer-live-p isearch--current-buffer)
        (with-current-buffer isearch--current-buffer
          (setq cursor-sensor-inhibit  (delq 'isearch cursor-sensor-inhibit))))
      (setq isearch--current-buffer  (current-buffer))
      (make-local-variable 'cursor-sensor-inhibit)
      (unless (boundp 'cursor-sensor-inhibit) (setq cursor-sensor-inhibit nil))
      ;; Suspend things like `cursor-intangible' during Isearch so we can search even within intangible text.
      (push 'isearch cursor-sensor-inhibit))
    (unless (or unread-command-events  executing-kbd-macro)
      (unless (input-pending-p)
        (if (and (boundp 'isearch-message-function)  isearch-message-function)
            (funcall isearch-message-function)
          (isearch-message)))
      (if (and isearch-slow-terminal-mode  (not (or isearch-small-window
                                                    (if (fboundp 'pos-visible-in-window-group-p) ; Emacs 25+
                                                        (pos-visible-in-window-group-p)
                                                      (pos-visible-in-window-p)))))
          (let ((found-point  (point)))
            (setq isearch-small-window  t)
            (move-to-window-line 0)
            (let ((window-min-height  1))
              (split-window nil (if (< search-slow-window-lines 0)
                                    (1+ (- search-slow-window-lines))
                                  (- (window-height) (1+ search-slow-window-lines)))))
            (if (not (< search-slow-window-lines 0))
                (other-window 1)
              (vertical-motion (- 1 search-slow-window-lines))
              (set-window-start (next-window) (point))
              (set-window-hscroll (next-window) (window-hscroll))
              (set-window-hscroll (selected-window) 0))
            (goto-char found-point))
        (let ((current-scroll  (window-hscroll)) ; Keep same hscrolling as at search start.
              visible-p)
          (set-window-hscroll (selected-window) isearch-start-hscroll)
          (unless (if (or (not (fboundp 'window-body-width)) ; Emacs < 24
                          (< (cdr (subr-arity (symbol-function 'window-body-width))) 2)) ; Emacs < 24.4
                      (pos-visible-in-window-p)
                    (setq visible-p  (if (fboundp 'pos-visible-in-window-group-p) ; Emacs 25+
                                         (pos-visible-in-window-group-p nil nil t)
                                       (pos-visible-in-window-p nil nil t)))
                    (or (not visible-p)
                        ;; When point not visible because of hscroll, `pos-visible-in-window(-group)-p' returns
                        ;; non-nil, but the X coordinate it returns is 1 pixel beyond the last visible one.
                        (< (car visible-p) (window-body-width nil t))))
            (set-window-hscroll (selected-window) current-scroll))))
      (if (not isearch-other-end)
          (isearch-dehighlight)
        (if (< isearch-other-end (point)) ; Just use `isearch-forward'?
            (isearch-highlight isearch-other-end (point))
          (isearch-highlight (point) isearch-other-end))
        (when (and (> emacs-major-version 21)  isearchp-nomodify-action-hook)
          (with-isearch-suspended (run-hooks 'isearchp-nomodify-action-hook)))
        (when isearchp-noprompt-action-function
          (unwind-protect
               (progn (add-hook 'minibuffer-setup-hook 'isearchp-barf-if-use-minibuffer)
                      (funcall isearchp-noprompt-action-function))
            (remove-hook 'minibuffer-setup-hook 'isearchp-barf-if-use-minibuffer)))))
    (setq  isearch-adjusted   nil
           isearch-yank-flag  nil
           ;; quit-flag       nil  ; No, not for `isearch-mode'.
           )
    ;; Prevent point moving to end of composition when part of it has just been searched.
    (when (boundp 'disable-point-adjustment) (setq disable-point-adjustment  t))
    (run-hooks 'isearch-update-post-hook)
    (when (and (boundp 'isearch-lazy-highlight)  isearch-lazy-highlight) ; < Emacs 25: was before post hook.
      (isearch-lazy-highlight-new-loop)))

  )

(defun isearchp-barf-if-use-minibuffer ()
  (error "Tried to use minibuffer in `isearchp-noprompt-action-function'"))

(defun isearchp-reset-noprompt-action-fn () (setq isearchp-noprompt-action-function nil))
(add-hook 'isearch-mode-end-hook 'isearchp-reset-noprompt-action-fn)


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Let prefix arg invoke `multi-isearch-buffers' (Emacs 23+).
;;
;; 2. Add Isearch+ stuff to doc string.
;;
;; NOTE: There is a zero-width space at the end of the doc-string line for
;;       `isearchp-on-demand-action-function'.  This is so that the option on the following line is not
;;       considered by `C-h f' to be a function, due to the keyword "function" at the end of the previous line.
;;       This makes `ediff' in old Emacs versions barf, so work around it, if you must compare nearby.
;;
(defun isearch-forward (&optional arg no-recursive-edit)
  "\
Search forward incrementally - Isearch+ version.

With a non-negative prefix arg, do an incremental regular expression
search instead.

With a negative prefix arg, do a (plain, not regexp) incremental
search across multiple buffers (Emacs 23+):
 * If the prefix arg is `-' (from `M--') then you are prompted for the
   list of buffers.
 * Otherwise, (e.g. `M-- 2'), you are prompted for a regexp that
   matches the names of the buffers to be searched.

If you try to exit with the search string empty then nonincremental
search is used.

\\<isearch-mode-map>
As you type characters, they add to the search string and are found.
The following non-printing keys are bound in `isearch-mode-map'.

Options
-------
`isearchp-auto-keep-filter-predicate-flag'\t- auto-keep predicates?
`isearchp-case-fold'\t\t\t- search is case sensitive?
`isearchp-deactivate-region-flag'\t- search deactivates region?
`isearchp-dim-outside-search-area-flag' [*] - dim non-search zones?
`isearchp-dimming-color' [*]\t\t- color for non-search zones
`isearchp-drop-mismatch'\t- handling input after search mismatch
`isearchp-drop-mismatch-regexp-flag'\t- regexp search drop mismatch?
`isearchp-filter-predicates-alist'\t- filter predicates to choose
`isearchp-ignore-comments-flag' [*]\t- ignore THINGs in comments?
`isearchp-hide-whitespace-before-comment-flag' [*] - precomment space?
`isearchp-initiate-edit-commands'\t- keys that edit, not exit
`isearchp-mouse-2-flag'\t\t- `mouse-2' anywhere yanks selection?
`isearchp-movement-unit-alist'\t- units and their movement functions
`isearchp-on-demand-action-function'\t- on-demand action function​
`isearchp-prompt-for-filter-name'\t- when to ask for filter name
`isearchp-prompt-for-prompt-prefix-flag'\t- prompt for prefix?
`isearchp-regexp-quote-yank-flag'\t- regexp-quote yanked text?
`isearchp-repeat-search-if-fail-flag'\t- restart search from limit?
`isearchp-restrict-to-region-flag'\t- restrict search to region?
`isearchp-resume-with-last-when-empty-flag'\t- resume last search?
`isearchp-ring-bell-function'\t- ring-bell function during search
`isearchp-set-region-flag'\t\t- select last search target?
`isearchp-toggle-option-flag'\t\t- toggle options too?
`isearchp-update-filter-predicates-alist-flag'\t- update completions?

 [*] Requires library `isearch-prop.el'.

Commands
--------
\\<isearch-mode-map>\
\\[isearch-repeat-forward]\t- search again forward, \\[isearch-repeat-backward] backward

\\[isearch-delete-char]\t- cancel last input item from end of search string
\\[isearch-del-char]\t- delete char from end of search string
\\[isearchp-remove-failed-part]\t- remove failed part of search string, if any
\\[isearchp-remove-failed-part-or-last-char]\t- remove failed part or last char
\\[isearch-abort]\t- remove failed part of search string, or cancel if none
\\[isearch-exit]\t- exit, leaving point at location found

\\[isearchp-yank-word-or-char]\t- yank a word or char from buffer onto search string
\\[isearchp-yank-char]\t- yank a char from buffer onto search string
\\[isearchp-yank-line]\t- yank text up to end of line onto search string
\\[isearch-yank-kill]\t- yank the last string of killed or copied text
\\[isearch-yank-pop]\t- replace string just yanked with string killed/copied before it
\\[isearchp-kill-ring-save]\t- copy current search string to kill ring
\\[isearchp-yank-symbol-or-char]\t- yank a symbol or char from buffer onto search string
\\[isearchp-yank-sexp-symbol-or-char]\t- yank sexp, symbol, or char from buffer onto search string
\\[isearch-quote-char]\t- quote a control character, to search for it
\\[isearch-char-by-name]\t- add a Unicode char to search string by Unicode name
\\[isearchp-open-recursive-edit]\t- invoke Emacs command loop recursively, during Isearch
\\[isearchp-retrieve-last-quit-search]\t- insert successful search string from when you hit `C-g'
\\[isearch-edit-string]\t- edit the search string in the minibuffer
\\[isearch-ring-advance], \\[isearch-ring-retreat]\t- search for next/previous item in search ring
\\[isearchp-complete]\t- complete the search string using the search ring
\\[isearch-query-replace]\t- run `query-replace' to replace search-string matches
\\[isearch-query-replace-regexp]\t- run `query-replace-regexp'
\\[isearch-occur]\t- run `occur' for search-string matches
\\[isearch-highlight-regexp]\t- run `highlight-regexp' to highlight search-string matches
\\[isearchp-fontify-buffer-now]\t- fontify whole buffer
\\[isearchp-set-region-around-search-target]\t- select last search

\\[isearch-describe-bindings]\t- list all Isearch key bindings
\\[isearch-describe-key]\t- show documentation of an Isearch key
\\[isearch-describe-mode]\t- show documentation for Isearch mode

\\[isearchp-cycle-mismatch-removal]\t- cycle option `isearchp-drop-mismatch'
\\[isearchp-repeat-search-if-fail-flag]\t- toggle restarting search on failure
\\[isearch-toggle-case-fold]\t- toggle case-sensitivity (for current search or more: `C-u')
\\[isearch-toggle-character-fold]\t- toggle character folding
\\[isearchp-toggle-symmetric-char-fold]\t- toggle character folding being symmetric
\\[isearchp-toggle-lazy-highlight-cleanup]\t- option `lazy-highlight-cleanup'
\\[isearchp-toggle-lazy-highlighting]\t- option `isearch-lazy-highlight'
\\[isearchp-toggle-search-invisible]\t- toggle searching invisible text
\\[isearch-toggle-invisible]\t- toggle searching invisible text, for current search or more
\\[isearchp-toggle-option-toggle]\t- toggle option `isearchp-toggle-option-flag'
\\[isearchp-toggle-region-restriction]\t- toggle restricting search to active region
\\[isearchp-toggle-set-region]\t- toggle setting region around search target
\\[isearchp-toggle-regexp-quote-yank]\t- toggle quoting (escaping) of regexp special characters
\\[isearch-toggle-word]\t- toggle word-searching
\\[isearch-toggle-symbol]\t- toggle symbol-searching
\\[isearch-toggle-lax-whitespace]\t- toggle whitespace matching

A `SPC' char normally matches all whitespace defined by variable
`search-whitespace-regexp'.  See also variables
`isearch-lax-whitespace' and `isearch-regexp-lax-whitespace'.

Commands that Require Library `isearch-prop.el'
-----------------------------------------------

\\[isearchp-property-forward]\t- search for a character (overlay or text) property
\\[isearchp-property-forward-regexp]\t- regexp-search for a character (overlay or text) property
\\[isearchp-toggle-complementing-domain]\t- toggle searching complements of normal search contexts
\\[isearchp-toggle-dimming-outside-search-area]\t- toggle dimming non-search zones
\\[isearchp-toggle-ignoring-comments]\t- toggle ignoring comments for `isearchp-thing'
\\[isearchp-toggle-hiding-comments]\t- hide or (`C-u') show comments

\\[isearchp-put-prop-on-region]\t- add a text property to region
\\[isearchp-add-regexp-as-property]\t- add prop to regexp matches
\\[isearchp-regexp-context-search]\t- search regexp contexts
\\[isearchp-regexp-define-contexts]\t- define regexp contexts

\\[isearchp-imenu] \t- search Emacs-Lisp definitions
\\[isearchp-imenu-command] \t- search Emacs command definitions
\\[isearchp-imenu-non-interactive-function] \t- search non-commands
\\[isearchp-imenu-macro] \t- search Emacs-Lisp macro definitions

\\[isearchp-thing]\t- search THING search contexts
\\[isearchp-thing-define-contexts]\t- define THING contexts
\\[isearchp-previous-visible-thing]\t- go to previous visible THING
\\[isearchp-next-visible-thing]\t- go to next visible THING

Input Methods
-------------

If an input method is turned on in the current buffer, that input
method is also active while you are typing characters to search.
To toggle the input method, type \\[isearch-toggle-input-method].  \
It also toggles the input
method in the current buffer.

To use a different input method for searching, type \
\\[isearch-toggle-specified-input-method],
and specify an input method you want to use.

---

The above keys, bound in `isearch-mode-map', are often controlled by
user options - do \\[apropos] on search-.* to find them.

If either option `isearch-allow-prefix' or option
`isearch-allow-scroll' is non-nil then you can use a prefix arg with
an Isearch key.  If option `isearch-allow-scroll' is non-nil then you
can use scrolling keys without exiting Isearch.

If these options are both nil then other control and meta chars
terminate the search and are then used normally (depending on
`search-exit-option').  Likewise for function keys and mouse button
events.

If this function is called non-interactively with nil argument
NO-RECURSIVE-EDIT then it does not return to the calling function
until the search is done.  See function `isearch-mode'."
  (interactive "P\np")
  (let ((numarg  (prefix-numeric-value arg)))
    (cond ((and (eq arg '-)  (fboundp 'multi-isearch-buffers))
           (let ((current-prefix-arg  nil)) (call-interactively #'multi-isearch-buffers)))
          ((and arg  (fboundp 'multi-isearch-buffers)  (< numarg 0))
           (call-interactively #'multi-isearch-buffers))
          (t (isearch-mode t (not (null arg)) nil (not no-recursive-edit))))))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Let prefix arg invoke `multi-isearch-buffers'.
;;
(defun isearch-backward (&optional arg no-recursive-edit)
  "Do incremental search backward.
See command `isearch-forward' for more information."
  (interactive "P\np")
  (let ((numarg  (prefix-numeric-value arg)))
    (cond ((and (eq arg '-)  (fboundp 'multi-isearch-buffers))
           (let ((current-prefix-arg  nil)) (call-interactively #'multi-isearch-buffers)))
          ((and arg  (fboundp 'multi-isearch-buffers)  (< numarg 0))
           (call-interactively #'multi-isearch-buffers))
          (t (isearch-mode nil (not (null arg)) nil (not no-recursive-edit))))))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Let prefix arg invoke `multi-isearch-buffers-regexp'.
;;
(defun isearch-forward-regexp (&optional arg no-recursive-edit)
  "Do incremental search forward for regular expression.
Like `isearch-forward' (ordinary incremental search) except that your
input is treated as a regexp.

With a non-negative prefix arg, do an plain (not regexp) incremental
search instead.

With a negative prefix arg, do a regexp incremental search across
multiple buffers (Emacs 23+):
 * If the prefix arg is `-' (from `M--') then you are prompted for the
   list of buffers.
 * Otherwise, (e.g. `M-- 2'), you are prompted for a regexp that
   matches the names of the buffers to be searched.

A `SPC' char normally matches all whitespace defined by variable
`search-whitespace-regexp'.  See also variables
`isearch-lax-whitespace' and `isearch-regexp-lax-whitespace'.
To search for a literal space and nothing else, use `C-q SPC'.
To toggle whitespace matching, use \\<isearch-mode-map>`\\[isearch-toggle-lax-whitespace]'.

This command does not support character folding."
  (interactive "P\np")
  (let ((numarg  (prefix-numeric-value arg)))
    (cond ((and (eq arg '-)  (fboundp 'multi-isearch-buffers))
           (let ((current-prefix-arg  nil)) (call-interactively #'multi-isearch-buffers-regexp)))
          ((and arg  (fboundp 'multi-isearch-buffers)  (< numarg 0))
           (call-interactively #'multi-isearch-buffers-regexp))
          (t (isearch-mode t (null arg) nil (not no-recursive-edit))))))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Let prefix arg invoke `multi-isearch-buffers-regexp'.
;;
(defun isearch-backward-regexp (&optional arg no-recursive-edit)
  "Do incremental search backward for regular expression.
See command `isearch-forward-regexp' for more information."
  (interactive "P\np")
  (let ((numarg  (prefix-numeric-value arg)))
    (cond ((and (eq arg '-)  (fboundp 'multi-isearch-buffers))
           (let ((current-prefix-arg  nil)) (call-interactively #'multi-isearch-buffers-regexp)))
          ((and arg  (fboundp 'multi-isearch-buffers)  (< numarg 0))
           (call-interactively #'multi-isearch-buffers-regexp))
          (t (isearch-mode nil (null arg) nil (not no-recursive-edit))))))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Works with multiple Emacs versions (20 - 25).
;; 2. Save `isearchp-win-pt-line'.
;; 3. Save `isearchp-reg-beg' and `isearchp-reg-end'.  (Used only for Emacs 24.3+.)
;; 4. Save `isearchp-orig-ring-bell-fn'.  Use `isearchp-ring-bell-function'.
;; 5. Deactivate region (Emacs 24.3+ only).
;;
(defun isearch-mode (forward &optional regexp op-fun recursive-edit regexp-function)
  "Start Isearch minor mode.  Called by `isearch-forward' and similar.
Deactivate the region, if active.
Non-nil argument FORWARD means search in the forward direction.
Non-nil argument REGEXP means regular expression search.
Argument OP-FUN is a function to be called after each input character
 is processed.  (It is not called after chars that exit the search.)
Non-nil RECURSIVE-EDIT means this function behaves modally.  It does
 not return to the calling function until the search is completed.
 It enters a recursive edit, which it exits that search is finished.

Non-nil argument REGEXP-FUNCTION:

 * If a function, call it to convert the search string to a regexp
   to use for regexp searching.

 * If not a function (or if Emacs < 25), search for a sequence of
   words, ignoring punctuation."
  (setq isearch-forward                  forward ; Initialize global vars.
        isearch-regexp                   (or regexp  (and (not regexp-function)
                                                          (boundp 'search-default-regexp-mode)
                                                          (eq t search-default-regexp-mode)))
        isearch-regexp-function          (or regexp-function  (and (boundp 'search-default-regexp-mode)
                                                                   (functionp search-default-regexp-mode)
                                                                   (not regexp)
                                                                   search-default-regexp-mode))
        isearch-op-fun                   op-fun
        isearch-last-case-fold-search    isearch-case-fold-search
        isearch-case-fold-search         case-fold-search
        isearch-invisible                search-invisible
        isearch-string                   ""
        isearch-message                  ""
        isearch-cmds                     ()
        isearch-success                  t
        isearch-wrapped                  nil
        isearch-barrier                  (point)
        isearch-adjusted                 nil
        isearch-yank-flag                nil
        isearch-invalid-regexp           nil ; Only for Emacs < 22.
        isearch-within-brackets          nil ; Only for Emacs < 22.
        isearch-error                    nil
        isearch-slow-terminal-mode       (and (<= baud-rate search-slow-speed)
                                              (> (window-height) (* 4 (abs search-slow-window-lines))))
        isearch-other-end                nil
        isearch-small-window             nil
        isearch-just-started             t
        isearch-start-hscroll            (window-hscroll)
        isearch-opoint                   (point)
        isearchp-win-pt-line             (- (line-number-at-pos) (line-number-at-pos (window-start)))
        isearchp-reg-beg                 (save-restriction
                                           (widen)
                                           (if (and (boundp 'isearchp-restrict-to-region-flag)
                                                    isearchp-restrict-to-region-flag
                                                    (use-region-p))
                                               (region-beginning)
                                             nil))
        isearchp-reg-end                 (save-restriction
                                           (widen)
                                           (if (and (boundp 'isearchp-restrict-to-region-flag)
                                                    isearchp-restrict-to-region-flag
                                                    (use-region-p))
                                               (region-end)
                                             nil))
        search-ring-yank-pointer         nil
        isearch-opened-overlays          ()
        isearch-input-method-function    input-method-function
        isearch-input-method-local-p     (local-variable-p 'input-method-function)
        regexp-search-ring-yank-pointer  nil
        ;; Save original value of `ring-bell-function', then set it to `isearchp-ring-bell-function'.
        isearchp-orig-ring-bell-fn       ring-bell-function
        ring-bell-function               isearchp-ring-bell-function
        ;; Save original value of `minibuffer-message-timeout'.
        ;; Then reset it to nil, so Isearch messages do not time-out.
        isearch-original-minibuffer-message-timeout (and (boundp 'minibuffer-message-timeout)
                                                         minibuffer-message-timeout)
        minibuffer-message-timeout       nil)
  (when (and (boundp 'isearchp-deactivate-region-flag)  isearchp-deactivate-region-flag) ; Emacs 24.3+
    (deactivate-mark))
  ;; Bypass input method while reading key.  When a user types a printable char, appropriate
  ;; input method is turned on in minibuffer to read multibyte characters.
  (unless isearch-input-method-local-p (make-local-variable 'input-method-function))
  (setq input-method-function  nil)
  (looking-at "")
  (setq isearch-window-configuration  (if isearch-slow-terminal-mode (current-window-configuration) nil))
  ;; Maybe make minibuffer frame visible and/or raise it.
  (let ((frame  (window-frame (minibuffer-window))))
    (unless (memq (frame-live-p frame) '(nil t))
      (unless (frame-visible-p frame) (make-frame-visible frame))
      (when minibuffer-auto-raise (raise-frame frame))))
  (setq isearch-mode  " Isearch")       ; forward? regexp?
  (force-mode-line-update)
  (setq overriding-terminal-local-map  isearch-mode-map)
  (run-hooks 'isearch-mode-hook)
  ;; Remember the initial map, possibly modified by external packages in `isearch-mode-hook'.  (Bug#16035)
  (when (boundp 'isearch--saved-overriding-local-map)
    (setq isearch--saved-overriding-local-map overriding-terminal-local-map))
  ;; Pushing initial state used to be before running `isearch-mode-hook', but a hook might set
  ;; `isearch-push-state-function' used in `isearch-push-state' to save mode-specific initial state.
  ;; (Bug#4994)
  (isearch-push-state)
  (isearch-update)
  (when (fboundp 'isearch-pre-command-hook) ; Emacs 24.4+
    (add-hook 'pre-command-hook  'isearch-pre-command-hook)
    (add-hook 'post-command-hook 'isearch-post-command-hook))
  (add-hook 'mouse-leave-buffer-hook 'isearch-done)
  (add-hook 'kbd-macro-termination-hook 'isearch-done)
  ;; `isearch-mode' can be made modal (in the sense of not returning to the calling function until
  ;; searching is completed) by entering a recursive-edit and exiting it when done isearching.
  (when recursive-edit (let ((isearch-recursive-edit  t)) (recursive-edit)))
  isearch-success)


(defadvice isearch-done (after reset-ring-bell-fn activate)
  "Reset `ring-bell-function' to `isearchp-orig-ring-bell-fn'."
  (setq ring-bell-function  isearchp-orig-ring-bell-fn))

;; Dynamic search filtering.
(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3)))

  (defadvice isearch-done (after isearchp-restore/update-filter-pred activate)
    "Reset `isearch-filter-predicate' or `isearchp-kept-filter-predicate'.
If `isearchp-auto-keep-filter-predicate-flag' is non-nil then set
`isearchp-kept-filter-predicate' to the current value of
`isearch-filter-predicate'.  Otherwise, do the opposite."
    (if isearchp-auto-keep-filter-predicate-flag
        (setq isearchp-kept-filter-predicate  isearch-filter-predicate)
      (setq isearch-filter-predicate  isearchp-kept-filter-predicate)))

  )


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Restore cursor position relative to window (`isearchp-win-pt-line').  Fixes Emacs bug #12253.
;;
(cond ((or (> emacs-major-version 23)   ; Emacs 23.2+
           (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
       (defun isearch-cancel ()
         "Terminate the search and go back to the starting point."
         (interactive)
         (if (and isearch-push-state-function  isearch-cmds)
             ;; For defined push-state function, restore the first state.
             ;; This calls pop-state function and restores original point.
             (let ((isearch-cmds  (last isearch-cmds)))
               (if (fboundp 'isearch--set-state)
                   (isearch--set-state (car isearch-cmds)) ; Emacs 24.3+.
                 (isearch-top-state))   ; Emacs 23.2 to 24.2.
               (when isearchp-win-pt-line (recenter isearchp-win-pt-line)))
           (goto-char isearch-opoint)
           (when isearchp-win-pt-line (recenter isearchp-win-pt-line)))
         (isearch-done 'NOPUSH)
         (isearch-clean-overlays)
         (signal 'quit nil)))
      (t                                ; Emacs 20 to 23.1.
       (defun isearch-cancel ()
         "Terminate the search and go back to the starting point."
         (interactive)
         (when (and (fboundp 'isearch-pop-fun-state) ; Emacs 22+.
                    (functionp (isearch-pop-fun-state (car (last isearch-cmds)))))
           (funcall (isearch-pop-fun-state (car (last isearch-cmds))) (car (last isearch-cmds))))
         (goto-char isearch-opoint)
         (when isearchp-win-pt-line (recenter isearchp-win-pt-line))
         (isearch-done 'NOPUSH)
         (isearch-clean-overlays)
         (signal 'quit nil))))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Save last successful search string or regexp as `isearchp-last-quit-search' or
;; `isearchp-last-quit-regexp-search', for retrieval via `isearchp-retrieve-last-quit-search'.
;;
(defun isearch-abort ()
  "Abort incremental search mode if searching is successful, signaling quit.
Otherwise, revert to previous successful search and continue searching.
Save last successful search string or regexp for later retrieval
 during Isearch, using \\<isearch-mode-map>`\\[isearchp-retrieve-last-quit-search]'.
Use `isearch-exit' to quit without signaling."
  (interactive)
  ;; (ding)  signal instead below, if quitting
  (discard-input)
  (if (and isearch-success  (not (if (< emacs-major-version 22) isearch-invalid-regexp isearch-error)))
      ;; If search is successful and has no incomplete regexp, move back to starting point and quit.
      (progn (setq isearch-success  nil)
             (set (if isearch-regexp 'isearchp-last-quit-regexp-search 'isearchp-last-quit-search)
                  isearch-string)
             ;; Exit isearch and pass on quit signal.
             (if (fboundp 'isearch-cancel) ; Emacs 22+
                 (isearch-cancel)
               (goto-char isearch-opoint) ; Emacs 20-21
               (isearch-done 'NOPUSH)
               (isearch-clean-overlays)
               (signal 'quit nil)))
    ;; If search is failing, or has an incomplete regexp, rub out until it is once more successful.
    (while (or (not isearch-success)  (if (< emacs-major-version 22) isearch-invalid-regexp isearch-error))
      (isearch-pop-state))
    (isearch-update)))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Respect `isearchp-regexp-quote-yank-flag'.
;;
(defun isearch-yank-string (string)
  "Yank STRING into Isearch search string."
  ;; Downcase the string if not supposed to case-fold yanked strings.
  (if (and isearch-case-fold-search  (eq 'not-yanks search-upper-case))
      (setq string  (downcase string)))
  (when (and isearch-regexp  isearchp-regexp-quote-yank-flag) (setq string  (regexp-quote string)))
  (setq isearch-yank-flag  t)           ; Don't move cursor in reverse search.
  (if (fboundp 'isearch-process-search-string) ; Emacs 24
      (isearch-process-search-string string (mapconcat 'isearch-text-char-description string ""))
    (setq isearch-string   (concat isearch-string string)
          isearch-message  (concat isearch-message (mapconcat 'isearch-text-char-description string
                                                              "")))
    (isearch-search-and-update)))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Respect `isearchp-mouse-2-flag'.
;;
;; 2. Works for older Emacs versions too: Set X selection, so `x-get-selection' returns non-nil.
;;
(defun isearch-mouse-2 (click)          ; Bound to `mouse-2' in `isearch-mode-map'.
  "Handle `mouse-2' in Isearch mode.
If `isearchp-mouse-2-flag' is non-nil, yank the X selection.
If `isearchp-mouse-2-flag' is nil, yank it only if the `mouse-2' click
is in the echo area.  Otherwise, invoke whatever `mouse-2' is bound to
outside of Isearch."
  (interactive "e")
  ;; For both the nil and non-nil `isearchp-mouse-2-flag' cases we need to explicitly set the X
  ;; selection, otherwise things won't work for older Emacs versions and depending on your
  ;; platform.  If not for that need, in Emacs 24+ we could simply use this for the non-nil case,
  ;; and make no change at all for the nil case:
  ;;
  ;; (let ((select-active-regions  t))
  ;;   (deactivate-mark)
  ;;   (isearch-yank-x-selection))
  ;;
  (if (and isearchp-mouse-2-flag  (mark))
      (when (/= (region-beginning) (region-end)) (isearchp-set-sel-and-yank))
    (let ((win                            (posn-window (event-start click)))
          (overriding-terminal-local-map  nil)
          (binding                        (key-binding (this-command-keys-vector) t)))
      (if (and (window-minibuffer-p win)  (not (minibuffer-window-active-p win)) ; In echo area
               (mark))
          (isearchp-set-sel-and-yank)
        (when (functionp binding) (call-interactively binding))))))

(defun isearchp-set-sel-and-yank ()
  "Set X selection and yank it into echo area."
  (when (mark)
    (if (fboundp 'gui-set-selection)
        (gui-set-selection
         'PRIMARY (buffer-substring-no-properties (region-beginning) (region-end))) ; Emacs 25.1+.
      (x-set-selection
       'PRIMARY (buffer-substring-no-properties (region-beginning) (region-end))))
    (deactivate-mark)
    (isearch-yank-x-selection)))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. If `isearchp-drop-mismatch' is `replace-last' then remove the last mismatched input.
;; 2. Use ?\ , not ?\s, so compatible with older Emacs versions.
;;
(if (or (> emacs-major-version 24)  (and (= emacs-major-version 24)
                                         (not (version< emacs-version "24.3.50"))))
    (defun isearch-printing-char (&optional char count)
      "Append ordinary printing character CHAR to the search string, then search.
CHAR defaults to the last printing character typed.
With a numeric prefix arg, append that many copies of CHAR."
      (interactive (list last-command-event (prefix-numeric-value current-prefix-arg)))
      (when (and (eq isearchp-drop-mismatch 'replace-last)
                 (or isearchp-drop-mismatch-regexp-flag  (not isearch-regexp)))
        (while (or (not isearch-success)  isearch-error)
          (isearch-pop-state)))
      (let ((char  (or char  last-command-event)))
        (when (= char ?\S-\ ) (setq char  ?\  ))
        (if current-input-method
            (isearch-process-search-multibyte-characters char count)
          (isearch-process-search-char char count))))

  (defun isearch-printing-char ()       ; Emacs < 24.4
    "Add ordinary printing character to the search string, then search."
    (interactive)
    (when (and (eq isearchp-drop-mismatch 'replace-last)
               (or isearchp-drop-mismatch-regexp-flag  (not isearch-regexp)))
      (while (or (not isearch-success)  isearch-error)
        (isearch-pop-state)))
    (let ((char  last-command-event))
      (when (= char ?\S-\ ) (setq char  ?\  ))
      (if current-input-method
          (isearch-process-search-multibyte-characters char)
        (isearch-process-search-char char)))))


(when (or (and (= emacs-major-version 24) (> emacs-minor-version 3))
          (and (= emacs-major-version 25) (< emacs-minor-version 3)))


  (put 'universal-argument-more 'isearch-scroll t)


  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; Fixes Emacs bug #25302, to allow `C-u C-u' etc. in prefix arg.
  ;;
  (defun isearch-pre-command-hook ()
    "Decide whether to exit Isearch mode before executing the command.
Don't exit Isearch if the key sequence that invoked this command
is bound in `isearch-mode-map', or if the invoked command is
a prefix argument command (when `isearch-allow-prefix' is non-nil),
or it is a scrolling command (when `isearch-allow-scroll' is non-nil).
Otherwise, exit Isearch (when `search-exit-option' is non-nil)
before the command is executed globally with terminated Isearch."
    (let* ((key         (this-single-command-keys))
           (main-event  (aref key 0)))
      (cond
        ;; Don't exit Isearch if we're in the middle of some
        ;; `set-transient-map' thingy like `universal-argument--mode'.
        ((not (eq overriding-terminal-local-map isearch--saved-overriding-local-map)))
        ;; Don't exit Isearch for isearch key bindings.
        ((commandp (lookup-key isearch-mode-map key nil)))
        ;; Optionally edit the search string instead of exiting.
        ((eq search-exit-option 'edit)
         (setq this-command 'isearch-edit-string))
        ;; Handle a scrolling function or prefix argument.
        ((or (and isearch-allow-prefix
                  (memq this-command '(universal-argument
                                       universal-argument-more ; Fixes bug #25302.
                                       digit-argument negative-argument)))
             (and isearch-allow-scroll
                  (symbolp this-command)
                  (or (eq (get this-command 'isearch-scroll) t)
                      (eq (get this-command 'scroll-command) t))))
         (when isearch-allow-scroll
           (setq isearch-pre-scroll-point  (point))))
        ;; A mouse click on the isearch message starts editing the search string.
        ((and (eq (car-safe main-event) 'down-mouse-1)
              (window-minibuffer-p (posn-window (event-start main-event))))
         ;; Swallow the up-event.
         (read-event)
         (setq this-command  'isearch-edit-string))
        ;; Other characters terminate the search and are then executed normally.
        (search-exit-option
         (isearch-done)
         (isearch-clean-overlays))
        ;; If search-exit-option is nil, run the command without exiting Isearch.
        (t
         (isearch-process-search-string key key)))))

  )

;; $$$$$$
;; (when (> emacs-major-version 21)        ; Emacs 22+
;;   (defun isearch-message (&optional c-q-hack ellipsis)
;;     ;; Generate and print the message string.
;;     (let ((cursor-in-echo-area  ellipsis)
;;           (cmds                 isearch-cmds)
;;           succ-msg m)
;;       (while (not (isearch-success-state (car cmds))) (pop cmds))
;;       (setq succ-msg  (if (equal (isearch-message-state (car isearch-cmds)) isearch-message)
;;                           (and cmds  (isearch-message-state (car cmds)))
;;                         isearch-message))
;;       (setq m  (concat
;;                 (isearchp-message-prefix c-q-hack ellipsis isearch-nonincremental)
;;                 succ-msg
;;                 (and (not isearch-success)
;;                      (string-match (regexp-quote succ-msg) isearch-message)
;;                      (not (string= succ-msg isearch-message))
;;                      (propertize (substring isearch-message (match-end 0))
;;                                  'face 'isearch-fail))))
;;       (when (and (not isearch-success)  (string-match " +$" m))
;;         (put-text-property (match-beginning 0) (length m) 'face 'trailing-whitespace m))
;;       (setq m  (concat m (isearchp-message-suffix c-q-hack ellipsis)))
;;       (if c-q-hack m (let ((message-log-max  nil)) (message "%s" m))))))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Highlight failed part of search string in echo area, in face `isearch-fail'.
;; (Same as what I added to vanilla Emacs 24+.)
;;
(when (or (= emacs-major-version 22)  (= emacs-major-version 23)) ; Emacs 22 & 23.

  (defun isearch-message (&optional c-q-hack ellipsis)
    ;; Generate and print the message string.
    (let ((cursor-in-echo-area  ellipsis)
          (msg                  isearch-message)
          (fail-pos             (isearch-fail-pos t)))
      ;; Highlight failed part
      (when fail-pos
        (setq msg  (copy-sequence msg))
        (add-text-properties fail-pos (length msg) '(face isearch-fail) msg)
        (when (string-match "\\`[ \t]+\\'" msg) ; Highlight trailing whitespace
          (add-text-properties (match-beginning 0) (match-end 0) '(face trailing-whitespace) msg)))
      (setq msg  (concat (isearch-message-prefix c-q-hack ellipsis isearch-nonincremental)
                         msg
                         (isearch-message-suffix c-q-hack ellipsis)))
      (if c-q-hack  msg  (let ((message-log-max  nil)) (message "%s" msg)))))

  (defun isearch-fail-pos (&optional msg)
    "Return position of first mismatch in search string, or nil if none.
If MSG is non-nil, use `isearch-message', otherwise `isearch-string'."
    (let ((cmds      isearch-cmds)
          (curr-msg  (if msg isearch-message isearch-string))
          succ-msg)
      (when (or (not isearch-success)  isearch-error)
        (while (or (not (isearch-success-state (car cmds)))  (isearch-error-state (car cmds)))
          (pop cmds))
        (setq succ-msg  (and cmds  (if msg
                                       (isearch-message-state (car cmds))
                                     (isearch-string-state (car cmds)))))
        (if (and (stringp succ-msg)
                 (< (length succ-msg) (length curr-msg))
                 (equal succ-msg (substring curr-msg 0 (length succ-msg))))
            (length succ-msg)
          0))))

  )


(unless (> emacs-major-version 24)      ; Emacs <24


  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; Put face `isearchp-word' on returned string.
  ;;
  (defun isearch--describe-word-mode (word-mode &optional space-before)
    "Return a string describing WORD-MODE.
If SPACE-BEFORE is non-nil,  put a space before, instead of after it."
    (let ((description  (propertize (cond ((and (symbolp word-mode)  (get word-mode 'isearch-message-prefix))
                                           (get word-mode 'isearch-message-prefix))
                                          (word-mode "word ")
                                          (t ""))
                                    'face 'isearchp-word)))
      (if space-before
          (replace-regexp-in-string "\\(.*\\) \\'" " \\1" description) ; Move space from end to beginning.
        description)))

  )


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Highlight message according to search characteristics.
;;
(when (and (> emacs-major-version 21)   ; Emacs 22 through Emacs 24.2
           (or (< emacs-major-version 24)  (and (= emacs-major-version 24)  (< emacs-minor-version 3))))

  (defun isearch-message-prefix (&optional _c-q-hack ellipsis nonincremental)
    ;; If about to search, and previous search regexp was invalid, check that it still is.
    ;; If it is valid now, let the message we display while searching say that it is valid.
    (and isearch-error  ellipsis  (condition-case ()
                                      (progn (re-search-forward isearch-string (point) t)
                                             (setq isearch-error  nil))
                                    (error nil)))
    ;; If currently failing, display no ellipsis.
    (unless isearch-success (setq ellipsis  nil))
    (let ((mm  (concat (and (not isearch-success)  (propertize "failing " 'face 'minibuffer-prompt))
                       (and isearch-adjusted  (propertize "pending " 'face 'minibuffer-prompt))
                       (and isearch-wrapped
                            (not isearch-wrap-function)
                            (if isearch-forward (> (point) isearch-opoint) (< (point) isearch-opoint))
                            (propertize "over" 'face 'isearchp-overwrapped))
                       (and isearch-wrapped  (propertize "wrapped " 'face 'isearchp-wrapped))
                       (isearch--describe-word-mode isearch-word)
                       (and isearch-regexp  (propertize "regexp " 'face 'isearchp-regexp))
                       (and (boundp 'multi-isearch-next-buffer-current-function) ; Emacs 23+
                            multi-isearch-next-buffer-current-function
                            (propertize "multi " 'face 'isearchp-multi))
                       (and (boundp 'isearch-message-prefix-add) ; Emacs 23+
                            isearch-message-prefix-add
                            (propertize isearch-message-prefix-add 'face 'minibuffer-prompt))
                       (propertize (if nonincremental "search" "I-search") 'face 'minibuffer-prompt)
                       (and (not isearch-forward)  (propertize " backward" 'face 'minibuffer-prompt))
                       (propertize (if (and (boundp 'bidi-display-reordering) ; Emacs 24+
                                            current-input-method)
                                       ;; Input methods for RTL languages use RTL chars for their
                                       ;; title.  That messes up display of search text after prompt.
                                       (bidi-string-mark-left-to-right
                                        (concat " [" current-input-method-title "]: "))
                                     ": ")
                                   'face 'minibuffer-prompt))))
      (concat (upcase (substring mm 0 1)) (substring mm 1))))

  )


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Highlight message according to search characteristics.
;; 2. Reverse the order of the filter prefixes.
;;
(when (or (> emacs-major-version 24)  (and (= emacs-major-version 24)  (> emacs-minor-version 2)))

  (defun isearch-message-prefix (&optional ellipsis nonincremental)
    ;; If about to search, and previous search regexp was invalid, check that it still is.
    ;; If it is valid now, let the message we display while searching say that it is valid.
    (and isearch-error  ellipsis  (condition-case ()
                                      (progn (re-search-forward isearch-string (point) t)
                                             (setq isearch-error  nil))
                                    (error nil)))
    ;; If currently failing, display no ellipsis.
    (unless isearch-success (setq ellipsis  nil))
    (let ((mmm  (concat (and (not isearch-success)  (propertize "failing " 'face 'minibuffer-prompt))
                        (and isearch-adjusted  (propertize "pending " 'face 'minibuffer-prompt))
                        (and isearch-wrapped
                             (not isearch-wrap-function)
                             (if isearch-forward (> (point) isearch-opoint) (< (point) isearch-opoint))
                             (propertize "over" 'face 'isearchp-overwrapped))
                        (and isearch-wrapped  (propertize "wrapped " 'face 'isearchp-wrapped))
                        (and (fboundp 'advice-function-mapc) ; Emacs 24.4+
                             isearchp-show-filter-prompt-prefixes-flag
                             (let ((prefix  ""))
                               (advice-function-mapc (lambda (_ props)
                                                       (let ((np  (cdr (assq 'isearch-message-prefix props))))
                                                         (when np (setq prefix  (concat prefix np)))))
                                                     isearch-filter-predicate)
                               (propertize prefix 'face (if isearchp-auto-keep-filter-predicate-flag
                                                            'isearchp-prompt-filter-prefix-auto-keep
                                                          'isearchp-prompt-filter-prefix))))
                        (if (> emacs-major-version 24)
                            (isearch--describe-regexp-mode isearch-regexp-function)
                          (isearch--describe-word-mode isearch-regexp-function))
                        (let ((multi  (propertize "multi " 'face 'isearchp-multi)))
                          (if (> emacs-major-version 24)
                              (cond (multi-isearch-file-list   (concat multi "file "))
                                    (multi-isearch-buffer-list (concat multi "buffer "))
                                    (t                         ""))
                            (concat (and isearch-regexp  (propertize "regexp " 'face 'isearchp-regexp))
                                    (and multi-isearch-next-buffer-current-function  multi))))
                        (and isearch-message-prefix-add
                             (propertize isearch-message-prefix-add 'face 'minibuffer-prompt))

                        (propertize (if nonincremental "search" "I-search") 'face 'minibuffer-prompt)
                        (and (not isearch-forward)  (propertize " backward" 'face 'minibuffer-prompt))
                        (propertize (if current-input-method
                                        ;; Input methods for RTL languages use RTL chars for their
                                        ;; title.  That messes up display of search text after prompt.
                                        (bidi-string-mark-left-to-right
                                         (concat " [" current-input-method-title "]: "))
                                      ": ")
                                    'face 'minibuffer-prompt))))

      ;; This is an ugly hack for what seems to be an Emacs bug (?).  Without re-propertizing the face
      ;; on the first character, it is lost, apparently because of the use of `isearchp-read-prompt-prefix'
      ;; within `with-isearch-suspended', in `isearchp-add-filter-predicate-1'.  I haven't isolated the bug,
      ;; hence this hack.
      (let ((face  (get-text-property 0 'face mmm)))
        (concat (propertize (upcase (substring mmm 0 1)) 'face face) (substring mmm 1)))))

  )


;; Fix for Emacs bug #20234.  (Fixed in Emacs 24.5.)

(when (and (featurep 'misearch)  (or (< emacs-major-version 24) ; Emacs 23 through 24.4.
                                     (and (= emacs-major-version 24)  (< emacs-minor-version 5))))

  (defadvice multi-isearch-end (after reset-buff-list activate)
    "Reset `multi-isearch-buffer-list' to nil."
    (setq multi-isearch-buffer-list  ()))

  )


;;; Replacement on demand.  Emacs 22+
;;;
(when (> emacs-major-version 21)

  (defvar isearchp--replacing-on-demand nil
    "Non-nil means search keys are automatically replacing search hits.
That is, keys such as `C-s' replace hits as you visit them, without
your needing to hit `C-M-RET'.")

  (defun isearchp-replace-on-demand ()
    "Replace current search hit by the value of `isearchp-replacement'.
This is the default value of `isearchp-on-demand-action-function'.
Isearch is not exited after replacing.

By default, `isearchp-replacement' is \"\", so with no prefix arg, by
default, this just deletes the search hit.

With a prefix arg, you are prompted for the replacement text, which
updates `isearchp-replacement'.  (If you clear the minibuffer and hit
`RET', then the search hit is just deleted.)

* A plain prefix arg (`C-u'), or `C-1', means replace only the current
  search hit.

* A negative prefix arg (e.g. `C--') toggles making search keys (e.g.
  `C-s') replace search hits you visit, that is, without your needing
  to hit `C-M-RET'.

* A positive prefix arg N means replace N search hits (but stop at the
  search limit).

* A zero prefix arg (e.g. `C-0') means replace *all* remaining search
  hits (up to the search limit).

To use this command with a prefix arg, you must set
 `isearch-allow-prefix' or `isearch-allow-scroll' to non-nil.

You can use `C-M-`' anytime during Isearch to toggle whether the
replacement text is taken literally or interpreted as using the
special regexp replacement constructs.  These are the same as in
`query-replace-regexp': `\\&`, `\\=\\N', `\\#', and `\\,' (but not `\\?')."
    (let ((numarg  (and isearchp-pref-arg  (prefix-numeric-value isearchp-pref-arg))))
      (when isearchp-pref-arg
        (when (consp isearchp-pref-arg) (setq numarg 1)) ; Treat plain `C-u' like `C-1'.
        (when (and isearchp--replacing-on-demand  isearchp-pref-arg  (< numarg 0))
          (setq isearchp--replacing-on-demand  nil
                numarg                         1))
        (setq isearchp-replacement
              (read-string (format "%sReplacement: "
                                   (cond ((> numarg 0) "")
                                         ((zerop numarg) "Replace ALL, to the limit.  ")
                                         (t "Replace just by VISITING.  ")))
                           nil nil
                           (or (and (not (equal isearchp-replacement ""))  isearchp-replacement)
                               (match-string 0)))))
      (cond ((or (not isearchp-pref-arg) (= 1 numarg))
             (unless (eq this-command 'isearchp-act-on-demand) (setq replace-count  0))
             (isearchp-replace-match))
            ((natnump numarg)
             (isearchp-replace-multiple numarg))
            (t
             (isearchp-replace-match)
             (setq isearchp--replacing-on-demand      t
                   replace-count                      0
                   isearchp-noprompt-action-function  'isearchp-replace-match))))
    (setq this-command  'isearchp-act-on-demand))

  ;; $$$$$$ TO DO: The cursor is left at the right place, but when resume search it resumes from the end
  ;;               of the last search hit (not the last replacement).
  (defun isearchp-replace-multiple (arg)
    "Replace ARG search hits, but stopping at the search limit.
If ARG is 0 then replace *all* remaining search hits, up to the limit."
    (let ((replace-count  0))
      (if (and (not (zerop arg))  (>= replace-count arg))
          (throw 'with-isearch-suspended (point))
        (while (and isearch-success  (or (zerop arg)  (< replace-count arg)))
          (isearchp-replace-match)
          (when (or (zerop arg)  (< replace-count arg))
            (isearch-resume isearch-string isearch-regexp isearch-regexp-function
                            isearch-forward isearch-message `,isearch-case-fold-search)))
        (throw 'with-isearch-suspended (point))))) ; If hit limit, put point at end of last hit.

  (defun isearchp-replace-match ()
    "Replace current match with `isearchp-replacement', interpreting escapes.
Treat the replacement string as does `query-replace-regexp'."
    (let ((compiled                      (save-match-data
                                           (query-replace-compile-replacement
                                            isearchp-replacement (not isearchp-replace-literally))))
          (enable-recursive-minibuffers  t)) ; So we can read input from \?.
      (condition-case isearchp-replace-match
          (replace-match-maybe-edit
           (if (consp compiled)
               (funcall (car compiled) (cdr compiled) (setq replace-count  (1+ replace-count)))
             compiled)
           (isearchp-replace-fixed-case-p (match-string 0)) isearchp-replace-literally nil (match-data))
        (wrong-number-of-arguments
         (condition-case isearchp-replace-match-2
             (replace-match-maybe-edit
              (if (consp compiled)
                  (funcall (car compiled) (cdr compiled) (setq replace-count  (1+ replace-count)))
                compiled)
              (isearchp-replace-fixed-case-p (match-string 0)) isearchp-replace-literally nil (match-data)
              nil)                      ; BACKWARD parameter for Emacs 24.4+ - see bug #18388
           (buffer-read-only (ding) (isearchp-user-error "Buffer is read-only"))
           (error (isearchp-user-error "No match for `%s'" isearchp-replacement))))
        (buffer-read-only (ding) (isearchp-user-error "Buffer is read-only"))
        (error (isearchp-user-error "No match for `%s'" isearchp-replacement)))))

  (defun isearchp-replace-fixed-case-p (from)
    "Return non-nil if FROM should be replaced without transferring case.
FROM is a string or nil.  If FROM is nil, then return nil.
Retuns non-nil if FROM is a string and one of the following holds:
 * FROM is not all lowercase
 * `case-replace' or `case-fold-search' is nil"
    (and from  (not (and case-fold-search  case-replace  (string= from (downcase from))))))

  (defun isearchp-toggle-literal-replacement () ; Bound to `C-M-`' in `isearch-mode-map'.
    "Toggle escaping of regexp special chars in replacement text.
This toggles variable `isearchp-replace-literally'.
Bound to `C-M-`' during Isearch."
    (interactive)
    (setq isearchp-replace-literally  (not isearchp-replace-literally))
    (message "Replacement of text literally is now %s" (if isearchp-replace-literally "ON" "OFF")))

  )

;; Basic lazy highlighting.
;;
(when (boundp 'isearch-lazy-highlight)  ; Emacs 22+

  (defvar isearchp-lazy-highlight-face 'lazy-highlight
    "Face currently used for lazy-highlighting.")

  (defun isearchp-redo-lazy-highlighting ()
    "Redisplay lazy highlighting."
    (setq isearch-lazy-highlight-last-string  nil) ; To force `isearch-lazy-highlight-new-loop' to act.
    (isearch-lazy-highlight-new-loop)
    (isearch-update))

  (defun isearchp-toggle-lazy-highlight-cleanup () ; Bound to `M-s h l' in `isearch-mode-map'.
    "Toggle `lazy-highlight-cleanup'."
    (interactive)
    (setq lazy-highlight-cleanup  (not lazy-highlight-cleanup))
    (message "Automatic removal of lazy highlights is now %s" (if lazy-highlight-cleanup 'ON 'OFF))
    (sit-for 1)
    (isearch-update))

  (define-key isearch-mode-map (kbd "M-s h l") 'isearchp-toggle-lazy-highlight-cleanup)

  (defun isearchp-toggle-lazy-highlighting () ; Bound to `M-s h L' in `isearch-mode-map'.
    "Toggle option `isearch-lazy-highlight'."
    (interactive)
    (customize-set-value 'isearch-lazy-highlight (not isearch-lazy-highlight))
    (unless isearch-lazy-highlight (lazy-highlight-cleanup 'FORCE))
    (message "Lazy highlighting is now %s" (if isearch-lazy-highlight 'ON 'OFF))
    (sit-for 1)
    (isearch-update))

  (define-key isearch-mode-map (kbd "M-s h L") 'isearchp-toggle-lazy-highlighting)

  )


;; Regexp group highlighting.
;;
(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3)))

  (defun isearchp-toggle-highlighting-regexp-groups () ; Bound to `M-s h R' in `isearch-mode-map'.
    "Toggle option `isearchp-highlight-regexp-group-levels-flag'."
    (interactive)
    (customize-set-value 'isearchp-highlight-regexp-group-levels-flag
                         (not isearchp-highlight-regexp-group-levels-flag))
    (while isearchp-regexp-level-overlays
      (delete-overlay (car isearchp-regexp-level-overlays))
      (setq isearchp-regexp-level-overlays  (cdr isearchp-regexp-level-overlays)))
    (isearchp-redo-lazy-highlighting)
    (message "Highlighting of regexp-group matches is now %s"
             (if isearchp-highlight-regexp-group-levels-flag 'ON 'OFF))
    (sit-for 1)
    (isearch-update))


  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; Delete also `isearchp-lazy-regexp-level-overlays'.
  ;;
  (defun lazy-highlight-cleanup (&optional force)
    "Stop lazy highlighting and remove extra highlighting from current buffer.
FORCE non-nil means do it whether or not `lazy-highlight-cleanup'
is nil.  This function is called when exiting an incremental search if
`lazy-highlight-cleanup' is non-nil."
    (interactive '(t))
    (when (or force lazy-highlight-cleanup)
      (while isearch-lazy-highlight-overlays
        (delete-overlay (car isearch-lazy-highlight-overlays))
        (setq isearch-lazy-highlight-overlays
              (cdr isearch-lazy-highlight-overlays)))
      (when (boundp 'isearchp-lazy-regexp-level-overlays) ; Emacs 24.4+
        (while isearchp-lazy-regexp-level-overlays
          (delete-overlay (car isearchp-lazy-regexp-level-overlays))
          (setq isearchp-lazy-regexp-level-overlays  (cdr isearchp-lazy-regexp-level-overlays)))))
    (when isearch-lazy-highlight-timer
      (cancel-timer isearch-lazy-highlight-timer)
      (setq isearch-lazy-highlight-timer nil)))

  )


(when (featurep 'character-fold+)

  (defun isearchp-toggle-symmetric-char-fold () ; Bound to `M-s =' in `isearch-mode-map'.
    "Toggle option `char-fold-symmetric'.
This does not also toggle character folding.

Note that symmetric character folding can slow down search.  Use
longer search strings to reduce this problem, or use `M-s h L' to turn
off lazy highlighting.

You need library `character-fold+.el' for this command."
    (interactive)
    ;; Must use `customize-set-variable', to invoke the `:set' function.
    (customize-set-variable 'char-fold-symmetric (not char-fold-symmetric))
    (message "Character folding is now %s  (`%s' toggles lazy highlighting)"
             (if char-fold-symmetric 'SYMMETRIC "ONE-WAY ONLY")
             (substitute-command-keys "\\<isearch-mode-map>\\[isearchp-toggle-lazy-highlighting]"))
    (sit-for 2)
    (lazy-highlight-cleanup 'FORCE)     ; Seems to be needed.
    (isearch-update))

  (define-key isearch-mode-map (kbd "M-s =") 'isearchp-toggle-symmetric-char-fold)

  )


(when (or (> emacs-major-version 24)    ; Emacs 24.3+
          (and (= emacs-major-version 24)  (> emacs-minor-version 2)))


  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; 1. Instead of just `isearch-success', test also that point is within `isearchp-reg-(beg|end)'.
  ;; 2. Goto char `isearchp-reg-(beg|end)', not `point-min|max'.
  ;; 3. Instead of testing bobp|eobp, test whether point is beyond `isearchp-reg-(beg|end)'.
  ;; 4. Do not use `isearchp-remove-mismatch' on `isearch-update-post-hook'.
  ;; 5. Added doc string.
  ;;
  (defun isearch-repeat (direction)
    "Utility for `isearch-repeat-forward' and `isearch-repeat--backward'."
    (if (eq isearch-forward (eq direction 'forward))
        (if (equal isearch-string "")   ; `C-s' in forward or `C-r' in reverse.
            ;; If search string is empty, use last one.
            (if (null (if isearch-regexp regexp-search-ring search-ring))
                (setq isearch-error  "No previous search string")
              (setq isearch-string            (car (if isearch-regexp regexp-search-ring search-ring))
                    isearch-message           (mapconcat 'isearch-text-char-description isearch-string "")
                    isearch-case-fold-search  isearch-last-case-fold-search)
              (isearch-ring-adjust1 nil)) ; After taking the last element, adjust ring to previous one.
          (or (and isearch-success      ; If already have what to search for, repeat it.
                   (if isearch-forward
                       (or (not isearchp-reg-end)  (<= (point) isearchp-reg-end))
                     (or (not isearchp-reg-beg)  (>= (point) isearchp-reg-beg))))
              (progn (setq isearch-wrapped  t) ; Set isearch-wrapped before calling isearch-wrap-function.
                     (if isearch-wrap-function
                         (funcall isearch-wrap-function)
                       (goto-char (if isearch-forward
                                      (or isearchp-reg-beg  (point-min))
                                    (or isearchp-reg-end  (point-max))))))))
      (setq isearch-forward  (not isearch-forward) ; C-s in reverse or C-r in forward, change direction.
            isearch-success  t))
    (setq isearch-barrier  (point))     ; For subsequent \| if regexp.
    (if (equal isearch-string "")
        (setq isearch-success  t)
      (if (and isearch-success  (equal (point) isearch-other-end)  (not isearch-just-started))
          ;; If repeating a search that found an empty string, ensure that we advance.
          (if (if isearch-forward
                  (or (eobp)  (and isearchp-reg-end  (> (point) isearchp-reg-end)))
                (or (bobp)  (and isearchp-reg-beg  (< (point) isearchp-reg-beg))))
              ;; If there's nowhere to advance to, fail (and wrap next time).
              (progn (setq isearch-success  nil) (ding))
            (forward-char (if isearch-forward 1 -1))
            (isearch-search))
        (isearch-search)))
    (isearch-push-state)
    (let ((isearch-update-post-hook  (if isearch-success
                                         isearch-update-post-hook
                                       (remove 'isearchp-remove-mismatch isearch-update-post-hook))))
      (isearch-update)))

  )


(when (< emacs-minor-version 24)        ; OK for Emacs 20-23, so far.


  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; 1. Do not use `isearchp-remove-mismatch' on `isearch-update-post-hook'.
  ;; 2. Added doc string.
  ;;
  (defun isearch-repeat (direction)
    "Utility for `isearch-repeat-forward' and `isearch-repeat--backward'."
    ;; Utility for isearch-repeat-forward and -backward.
    (if (eq isearch-forward (eq direction 'forward))
        ;; C-s in forward or C-r in reverse.
        (if (equal isearch-string "")
            ;; If search string is empty, use last one.
            (if (null (if isearch-regexp regexp-search-ring search-ring))
                (setq isearch-error "No previous search string")
              (setq isearch-string
                    (if isearch-regexp
                        (car regexp-search-ring)
                      (car search-ring))
                    isearch-message
                    (mapconcat 'isearch-text-char-description
                               isearch-string "")
                    isearch-case-fold-search isearch-last-case-fold-search)
              ;; After taking the last element, adjust ring to previous one.
              (unless (< emacs-major-version 24) (isearch-ring-adjust1 nil)))
          ;; If already have what to search for, repeat it.
          (or isearch-success
              (progn
                ;; Set isearch-wrapped before calling isearch-wrap-function.
                (setq isearch-wrapped t)
                (if (and (boundp 'isearch-wrap-function)  isearch-wrap-function)
                    (funcall isearch-wrap-function)
                  (goto-char (if isearch-forward (point-min) (point-max)))))))
      ;; C-s in reverse or C-r in forward, change direction.
      (setq isearch-forward (not isearch-forward)
            isearch-success t))
    (setq isearch-barrier (point))      ; For subsequent \| if regexp.
    (if (equal isearch-string "")
        (setq isearch-success t)
      (if (and isearch-success  (equal (point) isearch-other-end)
               (not isearch-just-started))
          ;; If repeating a search that found an empty string, ensure we advance.
          (if (if isearch-forward (eobp) (bobp))
              ;; If there's nowhere to advance to, fail (and wrap next time).
              (progn (setq isearch-success nil)
                     (ding))
            (forward-char (if isearch-forward 1 -1))
            (isearch-search))
        (isearch-search)))
    (isearch-push-state)
    ;; `isearchp-remove-mismatch' would prevent wrapping.  And since repeating, there cannot be a mismatch.
    (let ((isearch-update-post-hook
           (if isearch-success
               isearch-update-post-hook
             (remove 'isearchp-remove-mismatch isearch-update-post-hook))))
      (isearch-update)))

  )


;;; Support for limiting search to active region and other things.
;;;
(when (or (> emacs-major-version 24)    ; Emacs 24.3+
          (and (= emacs-major-version 24)  (> emacs-minor-version 2)))


  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; 1. Pass `isearchp-reg-(beg|end)', not nil, as BOUND arg to `isearch-search-string'.
  ;; 2. Instead of testing bobp|eobp, test whether point is beyond `isearchp-reg-(beg|end)'.
  ;; 3. Added doc string.
  ;;
  (defun isearch-search ()
    "Search using the current search string."
    (when (< emacs-major-version 25) ; $$$$$$ Should this message invocation be removed altogether?
      (if (and (boundp 'isearch-message-function)  isearch-message-function)
          (funcall isearch-message-function nil t)
        (isearch-message nil t)))
    (when (and (eq isearch-case-fold-search t)  search-upper-case)
      (setq isearch-case-fold-search  (isearch-no-upper-case-p isearch-string isearch-regexp)))
    (condition-case lossage
        (let ((inhibit-point-motion-hooks  isearch-invisible)
              (inhibit-quit                nil)
              (case-fold-search            isearch-case-fold-search)
              (search-invisible            isearch-invisible)
              (retry                       t))
          (setq isearch-error  nil)
          (while retry
            (setq isearch-success  (isearch-search-string isearch-string (if isearch-forward
                                                                             isearchp-reg-end
                                                                           isearchp-reg-beg)
                                                          t))
            ;; Clear RETRY unless the search predicate says to skip this search hit.
            (when (or (not isearch-success)
                      (if isearch-forward
                          (or (eobp)  (and isearchp-reg-end  (> (point) isearchp-reg-end)))
                        (or (bobp)  (and isearchp-reg-beg  (< (point) isearchp-reg-beg))))
                      (= (match-beginning 0) (match-end 0))
                      (funcall isearch-filter-predicate (match-beginning 0) (match-end 0)))
              (setq retry  nil)))
          (setq isearch-just-started  nil)
          (when isearch-success
            (setq isearch-other-end  (if isearch-forward (match-beginning 0) (match-end 0)))))
      (quit           (isearch-unread ?\C-g) (setq isearch-success  nil))
      (invalid-regexp (setq isearch-error  (cadr lossage))
                      (cond ((string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid " isearch-error)
                             (setq isearch-error  "incomplete input"))
                            ((and (not isearch-regexp)
                                  (string-match "\\`Regular expression too big" isearch-error))
                             (cond (isearch-regexp-function (setq isearch-error  "Too many words"))
                                   ((and isearch-lax-whitespace  search-whitespace-regexp)
                                    (setq isearch-error  "Too many spaces for whitespace matching"))))))
      (search-failed  (setq isearch-success  nil
                            isearch-error    (nth 2 lossage)))
      (error          (setq isearch-error  (format "%s" lossage)))) ; Stack overflow in regexp search.
    (if isearch-success
        nil
      (and (isearch--state-success  (car isearch-cmds)) ; Failed this time after succeeding last time.
           (ding))
      (when (functionp (isearch--state-pop-fun (car isearch-cmds)))
        (funcall (isearch--state-pop-fun (car isearch-cmds)) (car isearch-cmds)))
      (goto-char (isearch--state-point (car isearch-cmds)))))


  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; 1. Use `isearchp-reg-(beg|end)', not point-min|max.
  ;; 2. Fixes Emacs bug #21092, at least for a nil value of `lazy-highlight-max-at-a-time'.
  ;;
  (defun isearch-lazy-highlight-search ()
    "Search ahead for the next or previous match, for lazy highlighting.
Attempt to do the search exactly the way the pending Isearch would."
    (condition-case nil
        (let ((case-fold-search               isearch-lazy-highlight-case-fold-search)
              (isearch-regexp                 isearch-lazy-highlight-regexp)
              (isearch-regexp-function        isearch-lazy-highlight-regexp-function)
              (isearch-lax-whitespace         isearch-lazy-highlight-lax-whitespace)
              (isearch-regexp-lax-whitespace  isearch-lazy-highlight-regexp-lax-whitespace)
              (isearch-forward                isearch-lazy-highlight-forward)
              (search-invisible               nil) ; Do not match invisible text.
              (retry                          t)
              (success                        nil)
              (dim-face                       (if (fboundp 'isearchp-dim-face-spec) ; In `isearch-prop.el'.
                                                  (let ((isearchp-dim-outside-search-area-flag  t))
                                                    (isearchp-dim-face-spec))
                                                '(:background "#9abfca")))
              (bound
               (if isearch-lazy-highlight-forward
                   (min (or isearch-lazy-highlight-end-limit  isearchp-reg-end  (point-max))
                        (if isearch-lazy-highlight-wrapped
                            ;; Extend bound to match whole string at point
                            (+ isearch-lazy-highlight-start (1- (length isearch-lazy-highlight-last-string)))
                          (if lazy-highlight-max-at-a-time
                              (if (fboundp 'window-group-end)
                                  (window-group-end) ; Emacs 25+
                                (window-end))
                            (point-max))))
                 (max (or isearch-lazy-highlight-start-limit  isearchp-reg-beg  (point-min))
                      (if isearch-lazy-highlight-wrapped
                          ;; Extend bound to match whole string at point
                          (- isearch-lazy-highlight-end (1- (length isearch-lazy-highlight-last-string)))
                        (if lazy-highlight-max-at-a-time
                            (if (fboundp 'window-group-start)
                                (window-group-start) ; Emacs 25+
                              (window-start))
                          (point-max)))))))
          (while retry         ; Use a loop, like in `isearch-search'.
            (setq success  (isearch-search-string isearch-lazy-highlight-last-string bound t))
            (let (filter-OK)
              ;; Clear RETRY, if `isearchp-lazy-dim-filter-failures-flag' is non-nil or if no search hit.
              (when (or (and (boundp 'isearchp-lazy-dim-filter-failures-flag)
                             isearchp-lazy-dim-filter-failures-flag)
                        (not success)
                        (= (point) bound) ; like (bobp) (eobp) in `isearch-search'.
                        (= (match-beginning 0) (match-end 0)))
                (setq retry  nil))
              ;; Check filter predicate.  If `isearchp-lazy-dim-filter-failures-flag' is non-nil
              ;; then set face according to filter success.  Otherwise, clear RETRY if filter succeeded. 
              (setq filter-OK  (funcall isearch-filter-predicate (match-beginning 0) (match-end 0)))
              (if (and (boundp 'isearchp-lazy-dim-filter-failures-flag)
                       isearchp-lazy-dim-filter-failures-flag)
                  (setq isearchp-lazy-highlight-face  (if filter-OK 'lazy-highlight dim-face))
                (setq isearchp-lazy-highlight-face  'lazy-highlight)
                (when filter-OK (setq retry  nil)))))
          success)
      (error nil)))

  (defvar isearchp-in-lazy-highlight-update-p nil
    "Non-nil means `isearch-lazy-highlight-update' is processing.")


  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; 1. Use `isearchp-reg-(beg|end)', not point-min|max.
  ;; 2. Fixes Emacs bug #21092, at least for nil `lazy-highlight-max-at-a-time'.
  ;; 3. Binds `isearchp-in-lazy-highlight-update-p', as a convenience (e.g., for filter predicates).
  ;; 4. Lazy-highlights odd regexp groups using face `isearchp-lazy-odd-regexp-groups'.
  ;;
  (defun isearch-lazy-highlight-update ()
    "Update highlighting of other matches for current search."
    (let ((max                                  lazy-highlight-max-at-a-time)
          (looping                              t)
          (isearchp-in-lazy-highlight-update-p  t)
          nomore)
      (with-local-quit
        (save-selected-window
          (when (and (window-live-p isearch-lazy-highlight-window)
                     (if (fboundp 'isearch-lazy-highlight-window-group)
                         (not (memq (selected-window) isearch-lazy-highlight-window-group))
                       (not (eq (selected-window) isearch-lazy-highlight-window))))
            (select-window isearch-lazy-highlight-window))
          (save-excursion
            (save-match-data
              (goto-char (if isearch-lazy-highlight-forward
                             isearch-lazy-highlight-end
                           isearch-lazy-highlight-start))
              (while looping
                (let ((found  (isearch-lazy-highlight-search)))
                  (when max
                    (setq max  (1- max))
                    (when (<= max 0) (setq looping  nil)))
                  (when found
                    (let ((mb  (match-beginning 0))
                          (me  (match-end 0)))
                      (if (= mb me)     ; Zero-length match
                          (if isearch-lazy-highlight-forward
                              (if (= mb (if isearch-lazy-highlight-wrapped
                                            isearch-lazy-highlight-start
                                          (if max
                                              (if (fboundp 'window-group-end) ; Emacs 25+
                                                  (window-group-end)
                                                (window-end))
                                            (point-max))))
                                  (setq found  nil)
                                (forward-char 1))
                            (if (= mb (if isearch-lazy-highlight-wrapped
                                          isearch-lazy-highlight-end
                                        (if max
                                            (if (fboundp 'window-group-start) ; Emacs 25+
                                                (window-group-start)
                                              (window-start))
                                          (point-min))))
                                (setq found  nil)
                              (forward-char -1)))
                        (if (and (boundp 'isearchp-highlight-regexp-group-levels-flag) ; Emacs 24.4+
                                 isearchp-highlight-regexp-group-levels-flag
                                 isearch-lazy-highlight-regexp
                                 (> (regexp-opt-depth isearch-lazy-highlight-last-string) 0))
                            (save-match-data
                              (let ((level         1)
                                    (ise-priority  1000))
                                (save-excursion
                                  (goto-char mb)
                                  (when (looking-at isearch-lazy-highlight-last-string)
                                    (condition-case nil
                                        (while (not (equal (match-beginning level) (match-end level)))
                                          (unless (equal (match-beginning level) (match-end level))
                                            (let ((ov  (make-overlay (match-beginning level) (match-end level))))
                                              (push ov isearchp-lazy-regexp-level-overlays)
                                              (overlay-put ov 'priority 1000)
                                              (overlay-put ov 'face
                                                           (if (isearchp-oddp level)
                                                               'isearchp-lazy-odd-regexp-groups
                                                             isearchp-lazy-highlight-face))))
                                          (setq level  (+ level 1)))
                                      (error nil))))))
                          (let ((ov  (make-overlay mb me))) ; Non-zero-length match
                            (push ov isearch-lazy-highlight-overlays)
                            ;; 1000 is higher than ediff's 100+, but lower than isearch main overlay's 1001
                            (overlay-put ov 'priority 1000)
                            (overlay-put ov 'face isearchp-lazy-highlight-face))))
;;;                       (overlay-put ov 'window (selected-window)))) ; Emacs 25+ commented this out.
                      ;; Remember current point for next call of `isearch-lazy-highlight-update' when
                      ;; `lazy-highlight-max-at-a-time' is too small.
                      (if isearch-lazy-highlight-forward
                          (setq isearch-lazy-highlight-end  (point))
                        (setq isearch-lazy-highlight-start  (point)))))
                  (unless found ; Not found or zero-length match at the search bound
                    (if isearch-lazy-highlight-wrapped
                        (setq looping  nil
                              nomore   t)
                      (setq isearch-lazy-highlight-wrapped  t)
                      (if isearch-lazy-highlight-forward
                          (progn (setq isearch-lazy-highlight-end
                                       (if (fboundp 'window-group-start) ; Emacs 25+
                                           (window-group-start)
                                         (window-start)))
                                 (goto-char
                                  (max (or isearch-lazy-highlight-start-limit  isearchp-reg-beg  (point-min))
                                       (if (fboundp 'window-group-start) ; Emacs 25+
                                           (window-group-start)
                                         (window-start)))))
                        (setq isearch-lazy-highlight-start  (if (fboundp 'window-group-end) ; Emacs 25+
                                                                (window-group-end)
                                                              (window-end)))
                        (goto-char (min (or isearch-lazy-highlight-end-limit  isearchp-reg-end  (point-max))
                                        (if (fboundp 'window-group-end) ; Emacs 25+
                                            (window-group-end)
                                          (window-end)))))))))
              (unless nomore
                (setq isearch-lazy-highlight-timer  (run-at-time lazy-highlight-interval nil
                                                                 'isearch-lazy-highlight-update)))))))))

  )


;; Dynamic search filtering.
(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3)))

  ;; Helper functions.
  (defun isearchp-last-isearch-advice ()
    "Return the last advice applied to `isearch-filter-predicate'.
Raise an error if `isearch-filter-predicate' is not advised."
    (advice--car isearch-filter-predicate))

  (defun isearchp-first-isearch-advice ()
    "Return the first advice applied to `isearch-filter-predicate'.
Raise an error if `isearch-filter-predicate' is not advised."
    (advice--cd*r isearch-filter-predicate))

  (defun isearchp-current-filter-predicates ()
    "Return a list of the current filter predicates.
Each is an advice of `isearch-filter-predicate'."
    (let ((predicates  ()))
      (advice-function-mapc (lambda (pred props)
                              (push (cond ((cdr (assq 'name props)))
                                          ((symbolp pred) (symbol-name pred))
                                          (t (format "%s" pred))) ; Should not be needed.
                                    predicates))
                            isearch-filter-predicate)
      predicates))

  ;;---

  (defun isearchp-add-filter-predicate (predicate ; `C-z +'
                                        &optional flip-read-name-p flip-read-prefix-p msgp)
    "Read a PREDICATE and add it to `isearch-filter-predicate'.
PREDICATE can be a function symbol or a lambda form, but it must be
suitable as `isearch-filter-predicate'.

Option `isearchp-prompt-for-filter-name' controls whether you are
prompted to name the predicate.  The default option value means you
are prompted if PREDICATE is a non-symbol, such as a lambda form.
Using a non-positive prefix arg here reverses the behavior: If you
would normally be prompted then you are not prompted, and vice versa.

Option `isearchp-prompt-for-prompt-prefix-flag' controls whether you
are prompted for prefix text to prepend to the Isearch prompt.  (But
if `isearchp-show-filter-prompt-prefixes-flag' is nil then you are not
prompted - this has no effect.)  The default option value means you
are prompted.  Using a non-negative prefix arg here means you are not
prompted."
    (interactive (list (isearchp-read-predicate "Add filter predicate: ")
                       (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
                       (and current-prefix-arg  (>= (prefix-numeric-value current-prefix-arg) 0))
                       t))
    (isearchp-add-filter-predicate-1 :after-while predicate flip-read-name-p flip-read-prefix-p msgp))

  (defun isearchp-or-filter-predicate (predicate ;  `C-z ||'
                                       &optional flip-read-name-p flip-read-prefix-p msgp)
    "Read a PREDICATE and combine with the current one, using `or'.
`isearch-filter-predicate' is updated to return non-nil according to
either its previous test or the predicate you enter.  The latter is
tested first.

See `isearchp-add-filter-predicate' for descriptions of the args."
    (interactive (list (isearchp-read-predicate "Filter predicate to `or' with current: ")
                       (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
                       (and current-prefix-arg  (>= (prefix-numeric-value current-prefix-arg) 0))
                       t))
    (isearchp-add-filter-predicate-1 :before-until predicate flip-read-name-p flip-read-prefix-p msgp))

  (defun isearchp-or-last-filter (predicate ;  `C-z |1'
                                  &optional flip-read-name-p flip-read-prefix-p msgp)
    "Read a PREDICATE and combine with the last-added filter, using `or'.
The last-added filter predicate is replaced with a predicate that
`or's it with the predicate that you enter.

See `isearchp-add-filter-predicate' for descriptions of the args."
    (interactive (list (isearchp-read-predicate (format "Filter predicate to `or' with `%s': "
                                                        (if (advice--p isearch-filter-predicate)
                                                            (isearchp-last-isearch-advice)
                                                          isearch-filter-predicate)))
                       (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
                       (and current-prefix-arg  (>= (prefix-numeric-value current-prefix-arg) 0))
                       t))
    (cond ((advice--p isearch-filter-predicate)
           (let ((last  (isearchp-last-isearch-advice)))
             (remove-function isearch-filter-predicate last)
             (isearchp-add-filter-predicate-1 :after-while
                                              `(lambda (bg nd) (isearchp-or-preds ',predicate ',last bg nd))
                                              flip-read-name-p flip-read-prefix-p msgp)))
          (t
           (isearchp-add-filter-predicate-1 :before-until predicate flip-read-name-p flip-read-prefix-p msgp)))
    (when msgp (isearchp-show-filters)))

  (defun isearchp-or-preds (pred1 pred2 beg end)
    "Return non-nil if calling PRED1 or PRED2 on BEG and END returns non-nil."
    (or (funcall pred1 beg end)  (funcall pred2 beg end)))

  (put 'isearchp-or-predicates 'isearchp-part-pred t)
  (defun isearchp-or-predicates (&optional pred1 pred2)
    "Read two predicates and return a predicate that is their disjunction."
    (unless (and pred1 pred2)
      (setq pred1  (isearchp-read-predicate "First filter predicate: ")
            pred2  (isearchp-read-predicate "Second filter predicate: ")))
    `(lambda (beg end) (isearchp-or-preds ',pred1 ',pred2 beg end)))

  (defun isearchp-add-regexp-filter-predicate (regexp ; `C-z %'
                                               &optional flip-read-name-p flip-read-prefix-p msgp)
    "Add a predicate that matches REGEXP against the current search hit.
The predicate is added to `isearch-filter-predicate'.

See `isearchp-add-filter-predicate' for descriptions of the args
other than REGEXP."
    (interactive (list (isearchp-read-regexp-during-search "Regexp (for predicate): ")
                       (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
                       (and current-prefix-arg  (>= (prefix-numeric-value current-prefix-arg) 0))
                       t))
    (isearchp-add-filter-predicate-1 :after-while (isearchp-match-regexp-filter-predicate regexp)
                                     flip-read-name-p flip-read-prefix-p msgp))

  (defun isearchp-add-inline-regexp-filter-predicate (regexp ; `C-z .'
                                                      &optional flip-read-name-p flip-read-prefix-p msgp)
    "Add a predicate that matches `.*REGEXP.*' against the search hit.
The predicate is added to `isearch-filter-predicate'.

This just provides a shorthand way of entering a full-line regexp
for `C-z \\<isearchp-filter-map>\\[isearchp-add-regexp-filter-predicate]'.  Use this when regexp-searching \
for full lines
with regexp `.+'.

See `isearchp-add-filter-predicate' for descriptions of the args
other than REGEXP."
    (interactive (list (isearchp-read-regexp-during-search "Inline regexp (for predicate): ")
                       (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
                       (and current-prefix-arg  (>= (prefix-numeric-value current-prefix-arg) 0))
                       t))
    (setq regexp  (concat ".*" regexp ".*"))
    (isearchp-add-filter-predicate-1 :after-while (isearchp-match-regexp-filter-predicate regexp)
                                     flip-read-name-p flip-read-prefix-p msgp))

  (defun isearchp-add-filter-predicate-1 (where predicate flip-read-name-p flip-read-prefix-p msgp)
    "Helper for `isearchp-add-filter-predicate' and similar commands.
WHERE is passed to `add-function'.
See `isearchp-add-filter-predicate' for descriptions of other args."
    (let ((pred    predicate)
          (name    nil)
          (prefix  nil))
      (when (and (consp pred)  (not (eq 'lambda (car pred))))
        (if (stringp (nth 0 pred))      ; (NAME PREDICATE [PREFIX])
            (setq name    (nth 0 pred)
                  prefix  (nth 2 pred)
                  pred    (nth 1 pred))
          (if (nth 1 pred)
              (setq prefix  (nth 1 pred)  ; (PREDICATE PREFIX)
                    pred    (nth 0 pred)) ; (PREDICATE)
            (setq pred  (nth 0 pred)))))
      (add-function where isearch-filter-predicate pred
                    (append (and (or name  (case isearchp-prompt-for-filter-name
                                             (always      (not flip-read-name-p))
                                             (non-symbol  (if (not (symbolp pred))
                                                              (not flip-read-name-p)
                                                            flip-read-name-p))
                                             ((nil)       flip-read-name-p)
                                             (t           (not flip-read-name-p))))
                                 `((name . ,(setq name  (or name  (isearchp-read-filter-name))))))
                            (and isearchp-show-filter-prompt-prefixes-flag
                                 (or prefix  (if isearchp-prompt-for-prompt-prefix-flag
                                                 (not flip-read-prefix-p)
                                               flip-read-prefix-p))
                                 ;; Do not let empty or whitespace prefix get highlighted.
                                 (let ((prfix+  (setq prefix  (or prefix
                                                                  (if isearchp-user-entered-new-filter-p
                                                                      (isearchp-read-prompt-prefix)
                                                                    "")))))
                                   (unless (or (= 0 (length prfix+))
                                               (string-match-p "\\`[ \t]+\\'" prfix+))
                                     (setq prfix+  (concat prfix+ ", "))) ; Add a comma and a space.
                                   (and (> (length prfix+) 0)
                                        `((isearch-message-prefix . ,prfix+)))))))
      ;; Update `isearchp-current-filter-preds-alist' and possibly `isearchp-filter-predicates-alist'.
      (when isearchp-user-entered-new-filter-p
        (add-to-list 'isearchp-current-filter-preds-alist
                     (if name (list name pred prefix) (if prefix (list pred prefix) (list pred)))
                     :APPEND)
        (when isearchp-update-filter-predicates-alist-flag
          (customize-set-value 'isearchp-filter-predicates-alist isearchp-current-filter-preds-alist)
          (setq isearchp-current-filter-preds-alist  ())))
      (isearchp-redo-lazy-highlighting)
      (when msgp (isearchp-show-filters))))

  (defun isearchp-read-filter-name ()
    "Read a name for predicate being added."
    (let ((isearchp-resume-with-last-when-empty-flag  nil)
          name)
      (with-isearch-suspended (setq name  (read-string "Name this predicate: ")))
      (let ((oname  nil)) ; Ensure neither (1) empty name mor (2) pre-existing name.
        (while (catch 'isearchp-read-filter-name
                 (when (= 0 (length name)) (throw 'isearchp-read-filter-name ""))
                 (advice-function-mapc (lambda (pred props)
                                         (when (equal name (setq oname  (cdr (assq 'name props))))
                                           (throw 'isearchp-read-filter-name oname)))
                                       isearch-filter-predicate)
                 nil)
          (with-isearch-suspended
            (setq name  (read-string (if (= 0 (length name))
                                         "Name (cannot be empty): "
                                       (format "`%s' exists.  Another name: " name)))))))
      name))

  (defun isearchp-read-prompt-prefix ()
    "Read text to prepend to Isearch prompt."
    (let ((isearchp-resume-with-last-when-empty-flag  nil)
          prefix)
      (with-isearch-suspended (setq prefix  (read-string "Add Isearch prompt text: ")))
      prefix))

  (defun isearchp-read-predicate (&optional prompt)
    "Read a predicate usable as `isearch-filter-predicate'.
You can input a short NAME or FUNCTION that is the first element of an
`isearchp-filter-predicates-alist' entry.

If you use library `Bookmark+' then you can alternatively input the
name of an Isearch filter bookmark.  Filter bookmarks give you an easy
way to persist Isearch filters.

Completion is available for those short names and functions, plus any
such that you have added during Isearch (using \\<isearch-mode-map>`\
\\[isearchp-add-filter-predicate]', for example).  If you use
`Bookmark+' then completion also applies to filter-bookmark names.

This completion is available, but you can enter any predicate suitable
as `isearch-filter-predicate', instead of one of the completion
candidates.  (In other words, completion is lax.)  You can enter the
predicate as a symbol or a lambda-form.

For `isearchp-filter-predicates-alist' entries (and any added
dynamically) that include short names, the associated predicates are
shown as annotations next to the short names in buffer
`*Completions*'.  (If the predicate name starts with `isearchp-', this
prefix is removed from the annotation, for brevity.)

If you choose one of the functions or short names provided as a
completion candidate then you are not prompted subsequently for either
a short name or a prompt prefix to associate with it.  Otherwise, you
might be.

If you choose a completion candidate that does not name a function
symbol that has property `isearchp-part-pred', then the corresponding
full completion-candidate entry is returned by
`isearchp-read-predicate'.

For example, if you choose candidate `[url]' then the value returned
is the 3-element list `(\"[url]\" isearchp-in-url-p \"[URL]\")', where
the filter predicate is `isearchp-in-url-p', its short name is
`[url]', and the prompt prefix for it is `[URL]'.

If you choose a candidate that names a function symbol that has
property `isearchp-part-pred', then that function is invoked with no
arguments, and the filter predicate it returns is the value returned
by `isearchp-read-predicate'.

If you do not choose a completion candidate then the value returned by
`isearchp-read-predicate' is the predicate that you entered."
    (let ((isearchp-resume-with-last-when-empty-flag  nil)
          (filter-alist                               (append isearchp-current-filter-preds-alist
                                                              (isearchp-filter-bookmark-alist-only)))
          (completion-extra-properties
           '(:annotation-function
             (lambda (cand)
               (let* ((full  (assoc cand isearchp-current-filter-preds-alist))
                      (prd   (nth 1 full)))
                 (save-match-data
                   (when (and prd  (string-match "\\`isearchp-" (symbol-name prd)))
                     (setq prd  (substring (symbol-name prd) (match-end 0)))))
                 (and prd  (format " %s" prd))))))
          input choice pred result)
      (unless prompt  (setq prompt  "Predicate: "))
      (with-isearch-suspended
        (while (or (not pred)
                   (and (not (functionp pred))
                        (not (advice-function-member-p pred isearch-filter-predicate))))
          (setq input   (completing-read prompt filter-alist nil nil nil 'isearchp-predicate-history)
                choice  (assoc input filter-alist)
                choice  (or choice  (assoc (intern input) filter-alist)))
          (setq result  (or choice  (read input)) ; For example, input was a lambda form.
                pred    (if choice
                            (if (stringp (nth 0 choice))
                                (let ((bmk-filt  (cdr (assq 'isearchp-filter choice))))
                                  (if bmk-filt
                                      (setq result  bmk-filt) ; BOOKMARK
                                    (nth 1 choice))) ; (NAME FUNCTION [PREFIX])
                              (nth 0 choice)) ; (FUNCTION [PREFIX])
                          result))
          (unless (or (functionp pred)  (advice-function-member-p pred isearch-filter-predicate))
            (message "Not a function: `%S'" pred) (sit-for 1))
          (when (and (symbolp pred)  (get pred 'isearchp-part-pred))
            (setq pred    (funcall pred)
                  result  pred
                  choice  nil))
          (setq isearchp-user-entered-new-filter-p  (not choice))))
      result))

  (defun isearchp-read-regexp-during-search (&optional prompt)
    "Read and return a regexp, during Isearch, prompting with PROMPT."
    (let ((isearchp-resume-with-last-when-empty-flag  nil)
          regexp)
      (with-isearch-suspended (setq regexp  (read-regexp (or prompt  "Regexp: "))))
      regexp))

  (defun isearchp-match-regexp-filter-predicate (regexp)
    "Return a predicate that matches REGEXP against the current search hit.
The predicate is suitable for `isearch-filter-predicate'"
    `(lambda (beg end) (save-match-data (string-match-p ,regexp (buffer-substring beg end)))))

  (defun isearchp-remove-filter-predicate (predicate &optional msgp) ; `C-z -'
    "Remove PREDICATE from `isearch-filter-predicate' (default: last-added).
PREDICATE is passed to `remove-function'.  It can be a function or its
associated `name'."
    (interactive (let ((isearchp-resume-with-last-when-empty-flag  nil)
                       (preds                                      (isearchp-current-filter-predicates))
                       name)
                   (with-isearch-suspended
                     (setq name  (completing-read "Remove filter predicate: " preds nil t nil nil
                                                  (and (advice--p isearch-filter-predicate)
                                                       (isearchp-last-isearch-advice)))))
                   (list name t)))
    ;; FUNCTION arg to `remove-function' can be a string that is a `name' property value,
    ;; or it can be a function, e.g., a symbol.
    (remove-function isearch-filter-predicate
                     (let ((oname  nil))
                       (catch 'isearchp-remove-filter-predicate
                         (advice-function-mapc (lambda (pred props)
                                                 (when (equal predicate
                                                              (setq oname  (cdr (assq 'name props))))
                                                   (throw 'isearchp-remove-filter-predicate oname)))
                                               isearch-filter-predicate)
                         (setq predicate  (intern predicate)))))
    ;; Update `isearchp-current-filter-preds-alist' and possibly `isearchp-filter-predicates-alist'.
    (isearchp-assoc-delete-all predicate isearchp-current-filter-preds-alist)
    (when isearchp-update-filter-predicates-alist-flag
      (customize-set-value 'isearchp-filter-predicates-alist isearchp-current-filter-preds-alist)
      (setq isearchp-current-filter-preds-alist  ()))
    (isearchp-redo-lazy-highlighting)
    (when msgp (isearchp-show-filters)))

  (defun isearchp-complement-filter (&optional msgp) ; `C-z ~~'
    "Complement the current Isearch predicate."
    (interactive "p")
    (let ((preds  (isearchp-current-filter-predicates))
          already-complementing-p)
      (let ((opred  (if preds (isearchp-first-isearch-advice) isearch-filter-predicate)))
        (setq opred  (if (symbolp opred) (symbol-name opred) (format "%s" opred)))
        (push opred preds))
      (setq preds                    (nreverse preds)
            already-complementing-p  (equal "not" (car preds)))
      (cond (already-complementing-p
             (isearchp-remove-filter-predicate "not") ; Just turn off complementing current.
             (isearchp-redo-lazy-highlighting)
             (when msgp (message (substitute-command-keys "No longer complementing: %s  \
\[use \\<isearch-mode-map>`\\[isearchp-keep-filter-predicate]' to keep, \
`\\[isearchp-defun-filter-predicate]' to name]")
                                 (mapconcat 'identity (cdr preds) ", "))))
            (t
             (add-function :around isearch-filter-predicate 'isearchp-not-pred
                           '((name . "not") (isearch-message-prefix . "NOT ")))
             (isearchp-redo-lazy-highlighting)
             (when msgp (message (substitute-command-keys "NOT: %s  [use \\<isearch-mode-map>`\
\\[isearchp-keep-filter-predicate]' to keep, `\\[isearchp-defun-filter-predicate]' to name]")
                                 (mapconcat 'identity preds ", ")))))))

  (defun isearchp-negate-last-filter (&optional msgp) ;  `C-z ~1'
    "Replace the last-added filter predicate with its negation."
    (interactive "p")
    (cond ((advice--p isearch-filter-predicate)
           (let ((last  (isearchp-last-isearch-advice)))
             (remove-function isearch-filter-predicate last)
             (add-function :after-while isearch-filter-predicate `(lambda (bg nd) (isearchp-not-pred ',last bg nd)))))
          (t
           (add-function :around isearch-filter-predicate 'isearchp-not-pred
                         '((name . "not") (isearch-message-prefix . "NOT ")))))
    (when msgp (isearchp-show-filters)))

  (defun isearchp-not-pred (filter-predicate beg end)
    "Return non-nil if calling FILTER-PREDICATE on BEG and END returns nil."
    (not (funcall filter-predicate beg end)))

  (put 'isearchp-not-predicate 'isearchp-part-pred t)
  (defun isearchp-not-predicate (&optional predicate)
    "Read a PREDICATE and return a predicate that is its complement."
    (unless predicate (setq predicate  (isearchp-read-predicate "Filter predicate to negate: ")))
    `(lambda (beg end) (isearchp-not-pred ',predicate beg end)))

  (defun isearchp-filters-description ()
    "Description that lists the current filter predicates."
    (let ((preds  (isearchp-current-filter-predicates)))
      (let ((opred  (isearchp-first-isearch-advice)))
        (setq opred  (if (symbolp opred) (symbol-name opred) (format "%s" opred)))
        (push opred preds))
      (if preds
          (format "Filters: %s" (mapconcat (lambda (pred)
                                             (if (string-match-p "[ \t]" pred)
                                                 (format "`%s'" pred)
                                               pred))
                                           (nreverse preds)
                                           ", "))
        "NO filters")))

  (defun isearchp-show-filters ()       ; `C-z ?'
    "Print a message listing the current filter predicates."
    (interactive)
    (message (isearchp-filters-description)))

  (defun isearchp-set-filter-predicate (predicate &optional msgp) ; `C-z !'
    "Set `isearch-filter-predicate' to PREDICATE.
You can use `isearchp-reset-filter-predicate' (\\<isearch-mode-map>`\\[isearchp-reset-filter-predicate]' \
during
Isearch) to reset it to the default value."
    (interactive (list (isearchp-read-predicate "Set filter predicate: ") t))
    (setq isearch-filter-predicate  predicate)
    (isearchp-redo-lazy-highlighting)
    (when msgp (isearchp-show-filters)))

  (defun isearchp-defun-filter-predicate (function-symbol &optional action msgp) ; `C-z n'
    "Name the current filter predicate: Define a named function for it.
With a plain prefix arg (`C-u'), set the current filter predicate to
the function, and save the function definition persistently.  With a
double plain prefix arg (`C-u C-u'), do the same but also keep the
filter current for subsequent searches.  In both cases you are
prompted for the file to save it in.  The definition is appended to
any existing file contents.

With a non-positive prefix arg, set the current filter predicate to
the named function (as if you had also used `\\[isearchp-set-filter-predicate]'), so you can refer
to it by name.

With a non-negative prefix arg, keep the current filter predicate for
subsequent searches (as if you had also used `\\[isearchp-keep-filter-predicate]')."
    (interactive
     (let ((isearchp-resume-with-last-when-empty-flag  nil)
           fsymb)
       (with-isearch-suspended (setq fsymb  (read-string "Function name for current filter predicate: ")))
       (while (and (fboundp (intern-soft fsymb))
                   (not (y-or-n-p (format "Function `%s' exists.  Redefine? " fsymb))))
         (with-isearch-suspended (setq fsymb  (read-string "Name current filter predicate: "))))
       (setq fsymb  (intern fsymb))
       (list fsymb current-prefix-arg t)))
    (defalias function-symbol isearch-filter-predicate)
    (let* ((write-p  (consp action))
           (set-p    (or write-p  (and action  (<= (prefix-numeric-value action) 0))))
           (keep-p   (if write-p
                         (> (car action) 4)
                       (and action  (>= (prefix-numeric-value action) 0))))
           file)
      (when write-p
        (with-isearch-suspended (setq file  (read-file-name "Append filter to file: ")))
        (write-region (format "(defalias '%S %S %S)\n"
                              function-symbol isearch-filter-predicate (isearchp-filters-description))
                      nil file 'APPEND nil nil 'CONFIRM-IF-EXISTS))
      (when set-p (isearchp-set-filter-predicate function-symbol))
      (when keep-p (isearchp-keep-filter-predicate))
      (add-to-list 'isearchp-current-filter-preds-alist (list function-symbol) :APPEND)
      (when isearchp-update-filter-predicates-alist-flag
        (customize-set-value 'isearchp-filter-predicates-alist isearchp-current-filter-preds-alist)
        (setq isearchp-current-filter-preds-alist  ()))
      (when msgp (message "Filter predicate `%S' defined%s" function-symbol
                          (cond ((and write-p  keep-p) (format ", set, kept current, and saved to file `%s'" file))
                                (write-p               (format ", set and saved to file `%s'" file))
                                ((and set-p  keep-p)           ", set, and kept")
                                (set-p                         " and set")
                                (keep-p                        " and kept")
                                (t ""))))))

  (defun isearchp-keep-filter-predicate (&optional msgp) ; `C-z s'
    "Keep current filter predicate for subsequent searches in this session.
This does not save the predicate persistently."
    (interactive "p")
    (setq isearchp-kept-filter-predicate  isearch-filter-predicate)
    (when msgp (message "Current filter predicate KEPT for next Isearch")))

  (defun isearchp-reset-filter-predicate (&optional alist-also-p msgp) ; `C-z 0'
    "Reset `isearch-filter-predicate' to its default value.
By default, this is `isearch-filter-visible'.
With a prefix arg, also reset the alist of filter predicates,
`isearchp-current-filter-preds-alist'."
    (interactive "P\np")
    (setq isearch-filter-predicate        #'isearch-filter-visible
          isearchp-kept-filter-predicate  isearch-filter-predicate)
    (when alist-also-p
      (setq isearchp-current-filter-preds-alist  isearchp-filter-predicates-alist))
    (isearchp-redo-lazy-highlighting)
    (when msgp
      (if alist-also-p
          (message "`isearch-filter-predicate' and `isearchp-current-filter-preds-alist' RESET")
        (message "`isearch-filter-predicate' is RESET to default: %s" isearch-filter-predicate))))

  (defun isearchp-reset-filter-preds-alist (&optional msgp) ; Not bound.
    "Reset current filter predicates alist to saved value.
This means reset `isearchp-current-filter-preds-alist', which is used
for completion, to the value of `isearchp-filter-predicates-alist',
removing any filters you have added while searching."
    (interactive "p")
    (setq isearchp-current-filter-preds-alist  isearchp-filter-predicates-alist)
    (when msgp (message "`isearchp-current-filter-preds-alist' is RESET")))

  (defun isearchp-near (pattern distance &optional flip-read-name-p flip-read-prefix-p msgp) ; `C-z @'
    "Add Isearch predicate to match PATTERN within DISTANCE of search hit.
Matching can be either before or after the search hit.
You are prompted for the PATTERN and DISTANCE.

* PATTERN is a regexp.

* DISTANCE is measured in the units you specify (default: `c', meaning
  characters).  The available measurement units are defined by user
  option `isearchp-movement-unit-alist'.

You might also be prompted for a predicate name or an Isearch prompt
prefix - see `isearchp-add-filter-predicate'."
    (interactive (isearchp-read-near-args "Near regexp: "))
    (isearchp-add-filter-predicate
     (isearchp-near-predicate pattern distance) flip-read-name-p flip-read-prefix-p msgp))

  (defun isearchp-near-before (pattern distance &optional flip-read-name-p flip-read-prefix-p msgp) ; `C-z <'
    "Add Isearch predicate to match PATTERN within DISTANCE before search hit.
See `isearchp-near' for the args and the prompting behavior (and how
to control it)."
    (interactive (isearchp-read-near-args "Near-before regexp: "))
    (isearchp-add-filter-predicate
     (isearchp-near-before-predicate pattern distance) flip-read-name-p flip-read-prefix-p msgp))

  (defun isearchp-near-after (pattern distance &optional flip-read-name-p flip-read-prefix-p msgp) ; `C-z >'
    "Add Isearch predicate to match PATTERN within DISTANCE after search hit.
See `isearchp-near' for the args and the prompting behavior (and how
to control it)."
    (interactive (isearchp-read-near-args "Near-after regexp: "))
    (isearchp-add-filter-predicate
     (isearchp-near-after-predicate pattern distance) flip-read-name-p flip-read-prefix-p msgp))

  (defun isearchp-read-near-args (&optional regexp-prompt)
    "Read arguments for `isearchp-near*' commands.
REGEXP-PROMPT is the prompt to read the regexp for the nearby text."
    (let ((isearchp-resume-with-last-when-empty-flag  nil)
          pat
          dist)
      (with-isearch-suspended (setq pat   (read-regexp (or regexp-prompt  "Regexp: "))
                                    dist  (isearchp-read-measure)))
      (list pat
            dist
            (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
            (and current-prefix-arg  (>= (prefix-numeric-value current-prefix-arg) 0))
            t)))

  (defun isearchp-read-measure ()
    "Read a number, then read a unit name.
Only the first character of the unit name you type is used.
The default is `c', meaning the given number of characters.

The other characters recognized are those of option
`isearchp-movement-unit-alist'.

Returns a cons (NUMBER . FORWARD-THING-FUNCTION), where NUMBER is the
number of units to move and FORWARD-THING-FUNCTION is a function that
moves one unit forward."
    (cons (read-number "Distance: ")
          (let ((input  (read-from-minibuffer "Unit: " nil nil 'READ nil "c")))
            (if (functionp input)
                input
              (setq input (elt (symbol-name input) 0))
              (while (not (or (assq input isearchp-movement-unit-alist)
                              (functionp input)))
                (setq input  (read-from-minibuffer "Unit: " nil nil 'READ nil "c"))
                (unless (functionp input) (setq input  (elt (symbol-name input) 0))))
              (if (functionp input)
                  input
                (cdr (assq input isearchp-movement-unit-alist)))))))

  (defun isearchp-near-predicate (pattern distance)
    "Return a predicate that tests if PATTERN is within DISTANCE.
The predicate returns non-nil if PATTERN is found either before or
after the search hit, within DISTANCE.

DISTANCE is a cons returned by function `isearchp-read-measure'."
    `(lambda (beg end)
       (or (save-excursion
             (goto-char beg)
             (let ((dist     ,(car distance))
                   (unit-fn  ',(cdr distance))
                   unit-pos)
               (save-excursion (condition-case nil (funcall unit-fn (- dist)) (error nil))
                               (setq unit-pos  (point)))
               (save-match-data (re-search-backward ,pattern (max (point-min) unit-pos) t))))
           (save-excursion
             (goto-char end)
             (let ((dist     ,(car distance))
                   (unit-fn  ',(cdr distance))
                   unit-pos)
               (save-excursion (condition-case nil (funcall unit-fn dist) (error nil))
                               (setq unit-pos  (point)))
               (save-match-data (re-search-forward ,pattern (min (point-max) unit-pos) t)))))))


  (put 'isearchp-near-before-predicate 'isearchp-part-pred t)
  (defun isearchp-near-before-predicate (&optional pattern distance)
    "Return a predicate that tests if PATTERN precedes hit within DISTANCE.
If PATTERN or DISTANCE is nil then you are prompted for them both.
The predicate returns non-nil if PATTERN is found before the search
hit, within DISTANCE.

DISTANCE is a cons returned by function `isearchp-read-measure'."
    (unless (and pattern  distance)
      (let ((all  (isearchp-read-near-args "Near-before regexp: ")))
        (setq pattern   (nth 0 all)
              distance  (nth 1 all))))
    `(lambda (beg _)
       (save-excursion
         (goto-char beg)
         (let ((dist     ,(car distance))
               (unit-fn  ',(cdr distance))
               unit-pos)
           (save-excursion (condition-case nil (funcall unit-fn (- dist)) (error nil))
                           (setq unit-pos  (point)))
           (save-match-data (re-search-backward ,pattern (max (point-min) unit-pos) t))))))


  (put 'isearchp-near-after-predicate 'isearchp-part-pred t)
  (defun isearchp-near-after-predicate (&optional pattern distance)
    "Return a predicate that tests if PATTERN succeeds hit within DISTANCE.
If PATTERN or DISTANCE is nil then you are prompted for them both.
The predicate returns non-nil if PATTERN is found after the search
hit, within DISTANCE.

DISTANCE is a cons returned by function `isearchp-read-measure'."
    (unless (and pattern  distance)
      (let ((all  (isearchp-read-near-args "Near-after regexp: ")))
        (setq pattern   (nth 0 all)
              distance  (nth 1 all))))
    `(lambda (_ end)
       (save-excursion
         (goto-char end)
         (let ((dist     ,(car distance))
               (unit-fn  ',(cdr distance))
               unit-pos)
           (save-excursion (condition-case nil (funcall unit-fn dist) (error nil))
                           (setq unit-pos  (point)))
           (save-match-data (re-search-forward ,pattern (min (point-max) unit-pos) t))))))

  (defun isearchp-columns (min max &optional flip-read-name-p flip-read-prefix-p msgp) ; `C-z c'
    "Add a predicate that restrict searching between two columns (inclusive).
You are prompted for the minumum and maximum columns, in that order.
Defaults: 0 for the minimum, largest integer for the maximum.
Example: Enter 71 as min, default as max, to search past column 70.

You might also be prompted for a predicate name or an Isearch prompt
prefix - see `isearchp-add-filter-predicate'."
    (interactive
     (let ((isearchp-resume-with-last-when-empty-flag  nil)
           mn mx)
       (with-isearch-suspended 
         (setq mn  (read-number "Minimum column: " 0))
         (while (not (natnump mn))
           (setq mn  (read-number "Minimum column (0,1,2,...): " 0)))
         (setq mx  (read-number "Maximum column: " most-positive-fixnum))
         (while (not (natnump mx))
           (setq mx  (read-number "Maximum column (0,1,2,...): " most-positive-fixnum))))
       (list mn mx 
             (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
             (and current-prefix-arg  (>= (prefix-numeric-value current-prefix-arg) 0))
             t)))
    (isearchp-add-filter-predicate (isearchp-columns-p min max) flip-read-name-p flip-read-prefix-p msgp))

  (defun isearchp-columns-p (min max)
    "Return t if all chars in search hit are between columns MIN and MAX."
    `(lambda (beg end)
       (and (>= (save-excursion (goto-char beg) (current-column)) ,min)
            (<= (save-excursion (goto-char end) (current-column)) ,max))))

  (defun isearchp-show-hit-w-crosshairs (beg end)
    "Show the current search hit using crosshairs, at END.
END is the end of the search hit (farthest position, in the search
direction).

This is a *pseudo* filter predicate: it always returns t.

This function requires library `crosshairs.el'."
    (unless (require 'crosshairs nil t)
      (error "`isearchp-show-hit-w-crosshairs' requires library `crosshairs.el'"))
    (save-excursion (goto-char end))
    ;; Avoid calling `crosshairs' when inside `isearch-lazy-highlight-search'.
    (unless isearchp-in-lazy-highlight-update-p (crosshairs))
    t)

  (defun isearchp-in-comment-p (beg end)
    "Return t if all chars in the search hit are in the same comment.
BEG and END are the search-hit limits.

The comment delimiters are not considered to be in the comment.  See
`isearchp-in-comment-or-delim-p' for a predicate that includes the
delimiters, `comment-start' and `comment-end'."
    (let ((result  t)
          (pos     beg))
      (save-excursion
        (goto-char pos)
        (setq result  (catch 'isearchp-in-comment-p
                        (while (<= pos end)
                          (unless (nth 4 (syntax-ppss pos))
                            (throw 'isearchp-in-comment-p nil))
                          (setq pos  (1+ pos)))
                        t)))))

  (defun isearchp-in-comment-or-delim-p (beg end)
    "Like `isearchp-in-comment-p', plus `comment-start' and `comment-end'.
BEG and END are the search-hit limits."
    (let ((result  t)
          (pos     beg))
      (save-excursion
        (goto-char pos)
        (setq result  (catch 'isearchp-in-comment-or-delim-p
                        (while (<= pos end)
                          (unless (or (nth 4 (syntax-ppss pos))
                                      (and comment-start-skip  (looking-at comment-start-skip)))
;;; (and comment-end-skip  (looking-at comment-end-skip))
                            (throw 'isearchp-in-comment-or-delim-p nil))
                          (setq pos  (1+ pos)))
                        t)))))

  (defun isearchp-in-string-p (beg end)
    "Return t if all chars in the search hit are in the same string.
\(The string delimiters are not considered to be in the string.)
BEG and END are the search-hit limits."
    (let ((result  t)
          (pos     beg))
      (save-excursion
        (goto-char pos)
        (setq result  (catch 'isearchp-in-string-p
                        (while (<= pos end)
                          (unless (nth 3 (syntax-ppss pos))
                            (throw 'isearchp-in-string-p nil))
                          (setq pos  (1+ pos)))
                        t)))))

  (defun isearchp-in-string-or-comment-p (beg end)
    "Return t if all chars in the search hit are in a string or in a comment.
\(The string and comment delimiters are not considered to be in the
string or comment, respectively.)
BEG and END are the search-hit limits."
    (let ((result  t)
          (pos     beg))
      (save-excursion
        (goto-char pos)
        (setq result  (catch 'isearchp-in-string-or-comment-p
                        (while (<= pos end)
                          (unless (nth 8 (syntax-ppss pos))
                            (throw 'isearchp-in-string-or-comment-p nil))
                          (setq pos  (1+ pos)))
                        t)))))

  (defvar isearchp-ffap-max-region-size 1024 ; See also Emacs bug #25243.
    "Max size of active region used to obtain file-name defaults.
An active region larger than this many characters prevents
`isearchp-ffap-guesser' from calling `ffap-guesser'.")

  (defun isearchp-ffap-guesser ()
    "`ffap-guesser', but deactivate a large active region first."
    (and (require 'ffap nil t)
         ;; Prevent using a large active region to guess ffap: Emacs bug #25243.
         (let ((mark-active  (and mark-active  (< (buffer-size) isearchp-ffap-max-region-size))))
           (ffap-guesser))))

  (defun isearchp-in-file-or-url-p (beg end)
    "Return t if all chars in the search hit are in the same URL.
\(This is quite lax; it uses `isearchp-ffap-guesser'.)
BEG and END are the search-hit limits."
    (require 'ffap)
    (let ((result  t)
          (pos     beg))
      (save-excursion
        (goto-char pos)
        (setq result  (catch 'isearchp-in-file-or-url-p
                        (while (<= pos end)
                          (unless (save-match-data (isearchp-ffap-guesser))
                            (throw 'isearchp-in-file-or-url-p nil))
                          (setq pos  (1+ pos)))
                        t)))))

  (defun isearchp-in-list-p (beg end)
    "Return t if all chars in the search hit are in the same list.
BEG and END are the search-hit limits."
    (let ((result  t)
          (pos     beg))
      (save-excursion
        (goto-char pos)
        (setq result  (catch 'isearchp-in-list-p
                        (while (<= pos end)
                          (when (= 0 (nth 0 (syntax-ppss pos)))
                            (throw 'isearchp-in-list-p nil))
                          (setq pos  (1+ pos)))
                        t)))))

  (defun isearchp-in-lisp-variable-p (__%$_BEG_+-*&__ __%$_END_+-*&__)
    "Return t if all chars in the search hit are in the same Lisp variable.
BEG and END are the search-hit limits."
    ;; Just ignore the arguments, as variables (dumb, ugly hack, but good enough).
    (save-excursion
      (goto-char __%$_BEG_+-*&__)
      (catch 'isearchp-in-lisp-variable-p
        (while (<= __%$_BEG_+-*&__ __%$_END_+-*&__)
          (unless (and (symbolp (variable-at-point))
                       (not (memq (variable-at-point) '(__%$_BEG_+-*&__ __%$_END_+-*&__))))
            (throw 'isearchp-in-lisp-variable-p nil))
          (setq __%$_BEG_+-*&__  (1+ __%$_BEG_+-*&__)))
        t)))

  (defun isearchp-in-list-p (beg end)
    "Return t if all chars in the search hit are in the same list.
BEG and END are the search-hit limits."
    (let ((result  t)
          (pos     beg))
      (save-excursion
        (goto-char pos)
        (setq result  (catch 'isearchp-in-list-p
                        (while (<= pos end)
                          (when (= 0 (nth 0 (syntax-ppss pos)))
                            (throw 'isearchp-in-list-p nil))
                          (setq pos  (1+ pos)))
                        t)))))

  (defun isearchp-in-thing-p (thing beg end)
    "Return t if chars from BEG through END are in the same THING."
    (require 'thingatpt+ nil t)
    (let ((thg-fn  (if (fboundp 'tap-bounds-of-thing-at-point) ; In `thingatpt+.el'.
                       'tap-bounds-of-thing-at-point
                     'bounds-of-thing-at-point))
          bounds)
      (save-excursion (goto-char beg)
                      (setq bounds  (save-match-data (funcall thg-fn thing)))
                      (and bounds  (<= end (cdr bounds))))))

  (defun isearchp-in-color-p (beg end)
    "Return t if all chars in the search hit are in the same color name.
Such a name is any that satisfies `color-defined-p', which includes
`#' followed by RGB triples.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'color beg end))

  (defun isearchp-in-email-address-p (beg end)
    "Return t if all chars in the search hit are in the same email address.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'email beg end))

  (defun isearchp-in-url-p (beg end)
    "Return t if all chars in the search hit are in the same URL.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'url beg end))

  (defun isearchp-in-file-name-p (beg end)
    "Return t if all chars in the search hit are in the same file name.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'filename beg end))

  (defun isearchp-in-defun-p (beg end)
    "Return t if all chars in the search hit are in the same defun.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'defun beg end))

  (defun isearchp-in-sentence-p (beg end)
    "Return t if all chars in the search hit are in the same sentence.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'sentence beg end))

  (defun isearchp-in-paragraph-p (beg end)
    "Return t if all chars in the search hit are in the same paragraph.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'paragraph beg end))

  (defun isearchp-in-line-p (beg end)
    "Return t if all chars in the search hit are in the same line.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'line beg end))

  (defun isearchp-in-page-p (beg end)
    "Return t if all chars in the search hit are in the same page.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'page beg end))

  (defun isearchp-in-sexp-p (beg end)
    "Return t if all chars in the search hit are in the same sexp.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'sexp beg end))

  (defun isearchp-in-symbol-p (beg end)
    "Return t if all chars in the search hit are in the same symbol.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'symbol beg end))

  (defun isearchp-in-word-p (beg end)
    "Return t if all chars in the search hit are in the same word.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'word beg end))

  (defun isearchp-in-number-p (beg end)
    "Return t if all chars in the search hit are in the same number.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'number beg end))

  (defun isearchp-in-decimal-number-p (beg end)
    "Return t if all chars in the search hit are in the same number.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'decimal-number beg end))

  (defun isearchp-in-hex-number-p (beg end)
    "Return t if all search-hit chars are in the same hexadecimal number.
BEG and END are the search-hit limits."
    (isearchp-in-thing-p 'hex-number beg end))


  (when (featurep 'bookmark+)

    (defun isearchp-filter-bookmark-p (bookmark)
      "Return non-nil if BOOKMARK is an Isearch filter bookmark.
BOOKMARK is a bookmark name or a bookmark record.
If it is a record then it need not belong to `bookmark-alist'."
      (bookmark-prop-get bookmark 'isearchp-filter))

    (defun isearchp-filter-bookmark-alist-only ()
      "`bookmark-alist', filtered to retain only Isearch filter bookmarks.
A new list is returned (no side effects)."
      (bookmark-maybe-load-default-file)
      (bmkp-remove-if-not #'isearchp-filter-bookmark-p bookmark-alist))

    (defun isearchp-bookmark-current-filter-predicate (bookmark-name &optional msgp) ; `C-z b'
      "Bookmark the current filter predicate.
The bookmark created is an Isearch filter bookmark.
You need library `bookmark+.el' for this."
      (interactive
       (let (bname)
         (unless (require 'bookmark+ nil t) (error "You need library `bookmark+.el' for this command"))
         (with-isearch-suspended
           (setq bname  (bmkp-completing-read-lax
                         "Bookmark name (and filter name)" nil
                         (bmkp-remove-if-not #'isearchp-filter-bookmark-p bookmark-alist))))
         (list bname t)))
      (bookmark-maybe-load-default-file)
      (let ((bmk  `(,bookmark-name
                    ,@(bookmark-make-record-default 'NO-FILE 'NO-CONTEXT)
                    (isearchp-filter . ,isearch-filter-predicate)
                    (filter-description . ,(isearchp-filters-description))
                    (handler . ignore)))) ; Cannot invoke bookmark.  It just records data.
        (push bmk bookmark-alist)
        (setq bookmark-alist-modification-count  (1+ bookmark-alist-modification-count))
        (when (bookmark-time-to-save-p) (bookmark-save))
        (setq bookmark-current-bookmark bookmark-name)
        (bookmark-bmenu-surreptitiously-rebuild-list)
        (when msgp (message "Bookmarked current filter as bookmark `%s'" bookmark-name))
        bmk))

    )

  )


;; Miscellaneous
;;

(defun isearchp-read-face-names  (&optional empty-means-none-p only-one-p)
  "Read face names with completion, and return a list of their symbols.
If user hits `RET' with empty input immediately, then include all
faces.  Otherwise, read faces one by one, until user hits `RET' twice
consecutively.

Non-nil optional arg EMPTY-MEANS-NONE-P means return nil (no face
names) for empty user input.

Non-nil optional arg ONLY-ONE-P means read only one face name and
return its symbol."
  (let ((icicle-multi-completing-p                   t)
        (icicle-list-nth-parts-join-string           ": ")
        (icicle-list-join-string                     ": ")
        (icicle-list-use-nth-parts                   '(1))
        (icicle-proxy-candidates
         (and (boundp 'icicle-add-proxy-candidates-flag)  icicle-add-proxy-candidates-flag
              (append (and (fboundp 'eyedrop-face-at-point)  (list "*point face name*"))
                      (let ((ipc  ()))
                        (mapatoms
                         (lambda (cand)
                           (when (and (user-variable-p cand)  (eq (get cand 'custom-type) 'face))
                             (push `,(concat "'" (symbol-name cand) "'") ipc))))
                        ipc))))
        (face-cands                                  (mapcar
                                                      (if (and (boundp 'icicle-mode)  icicle-mode)
                                                          #'icicle-make-face-candidate
                                                        (lambda (face) (list (symbol-name face))))
                                                      (face-list)))
        (faces                                       ())
        (prompt1                                     "Face (RET for each, empty input to finish): ")
        (prompt2                                     "Face: ")
        (icicle-unpropertize-completion-result-flag  t)
        face)
    (when (and (boundp 'icicle-mode)  icicle-mode)
      (put-text-property 0 1 'icicle-fancy-candidates t prompt1)
      (put-text-property 0 1 'icicle-fancy-candidates t prompt2))
    (setq face  (isearchp-read-face-names--read prompt1 face-cands))
    (if (and empty-means-none-p  (string= "" face))
        ()
      (if only-one-p
          face
        (if (string= "" face)
            (setq faces  (face-list))
          (setq face-cands  (delete (assoc face face-cands) face-cands))
          (while (not (string= "" face))
            (add-to-list 'faces (intern face))
            (setq face        (isearchp-read-face-names--read prompt2 face-cands)
                  face-cands  (delete (assoc face face-cands) face-cands)))
          (nreverse faces))))))

(defun isearchp-read-face-names--read (prompt candidates)
  "Read a face name using PROMPT and face-name completion CANDIDATES."
  (if (and (boundp 'icicle-mode)  icicle-mode)
      (icicle-transform-multi-completion
       (completing-read
        prompt candidates nil (not (stringp icicle-WYSIWYG-Completions-flag)) nil
        (if (boundp 'face-name-history) 'face-name-history 'icicle-face-name-history)))
    (completing-read prompt candidates nil t nil 'face-name-history)))

(defun isearchp-read-sexps  (&optional only-one-p)
  "Read sexps with completion, and return them as a list.
Read sexps one by one, until user hits `RET' twice consecutively.
Non-nil ONLY-ONE-P means read only one sexp and return it."
  (let ((sexp-cands                         (mapcar #'list (isearchp-remove-duplicates
                                                            read-expression-history)))
        (sexps                              ())
        (prompt1                            "Sexp (RET for each, empty input to finish): ")
        (prompt2                            "Sexp: ")
        sexp)
    (setq sexp        (completing-read (if only-one-p prompt2 prompt1) sexp-cands
                                       nil nil nil 'read-expression-history)
          sexp-cands  (delete (assoc sexp sexp-cands) sexp-cands))
    (if only-one-p
        (car (read-from-string sexp))
      (while (not (string= "" sexp))
        (add-to-list 'sexps sexp)
        (setq sexp        (completing-read prompt2 sexp-cands nil nil nil 'read-expression-history)
              sexp-cands  (delete (assoc sexp sexp-cands) sexp-cands)))
      (prog1 (setq sexps  (nreverse (delete "" sexps)) ; Return the list of sexps.
                   sexps  (mapcar (lambda (sx) (car (read-from-string sx))) sexps))
        (when (interactive-p) (message "Sexps: %S" sexps))))))

(when (and (fboundp 'cl-puthash)  (not (fboundp 'puthash))) ; Emacs 20 with `cl-extra.el' loaded.
  (defalias 'puthash 'cl-puthash))

;; Same as `icicle-remove-duplicates'.
(if (fboundp 'puthash)                  ; Emacs 21+, or Emacs 20 with `cl-extra.el' loaded.
    (defun isearchp-remove-duplicates (sequence &optional test)
      "Copy of SEQUENCE with duplicate elements removed.
Optional arg TEST is the test function.  If nil, test with `equal'.
See `make-hash-table' for possible values of TEST."
      (setq test  (or test  #'equal))
      (let ((htable  (make-hash-table :test test)))
        (loop for elt in sequence
              unless (gethash elt htable)
              do     (puthash elt elt htable)
              finally return (loop for i being the hash-values in htable collect i))))

  (defun isearchp-remove-duplicates (list &optional use-eq)
    "Copy of LIST with duplicate elements removed.
Test using `equal' by default, or `eq' if optional USE-EQ is non-nil."
    (let ((tail  list)
          new)
      (while tail
        (unless (if use-eq (memq (car tail) new) (member (car tail) new))
          (push (car tail) new))
        (pop tail))
      (nreverse new))))

(defun isearchp-repeat-command (command)
  "Repeat COMMAND."
  (let ((repeat-message-function  'ignore))
    (setq last-repeatable-command  command)
    (repeat nil)))

(defun isearchp-assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is `equal' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist)) (equal (car (car alist)) key))  (setq alist  (cdr alist)))
  (let ((tail  alist)
        tail-cdr)
    (while (setq tail-cdr  (cdr tail))
      (if (and (consp (car tail-cdr))  (equal (car (car tail-cdr)) key))
          (setcdr tail (cdr tail-cdr))
        (setq tail  tail-cdr))))
  alist)

;; Same as `cl-oddp'.
;;
(defun isearchp-oddp (integer)
  "Return t if INTEGER is odd."
  (eq (logand integer 1) 1))
 
;;(@* "Keys and Hooks")

;;; Keys and Hooks ---------------------------------------------------

(define-key isearch-mode-map [mouse-2]            'isearch-mouse-2)
;; Must not be just `nil'.  Need to override a global binding such as `mouse-flash-position-or-M-x'.
(define-key isearch-mode-map [down-mouse-2]       'ignore)

;; Must not be just `nil'.  Otherwise, if click `mouse-2' in a standalone minibuffer frame then
;; the `switch-frame' event exits Isearch and the following `down-mouse-2' invokes, e.g.,
;; `mouse-flash-position-or-M-x'.
(define-key isearch-mode-map [switch-frame]       'ignore)

;;; Use this instead of `ignore' for `switch-frame', if you want it to exit Isearch when you switch
;;; to any frame other than a standalone minibuffer frame.
;;; (defun isearchp-switch-frame-or-exit ()
;;;   "Return nil if switch to minibuffer frame.  Else exit Isearch.
;;; Bind to `switch-frame' event."
;;;   (interactive)
;;;   (let* ((vec   (this-command-keys-vector))
;;;          (evnt  (aref vec 0)))
;;;     (unless (and (consp evnt)  (eq 'switch-frame (car evnt))
;;;                  (cadr evnt)   (window-minibuffer-p (frame-selected-window (cadr evnt))))
;;;       (isearch-done)
;;;       (isearch-clean-overlays))))

;;; (define-key isearch-mode-map [switch-frame]    'isearchp-switch-frame-or-exit)


;; Fix unwise advertised bindings as `M-s...'.
(put 'isearch-toggle-case-fold :advertised-binding "\M-c")
(put 'isearch-toggle-regexp    :advertised-binding "\M-r")
(put 'isearch-edit-string      :advertised-binding "\M-e")


(define-key isearch-mode-map [(control ?+)]       'isearchp-toggle-search-invisible)
(define-key isearch-mode-map [(control ?`)]       'isearchp-toggle-regexp-quote-yank)
(define-key isearch-mode-map "\C-h"               'isearch-mode-help)

;; An alternative to binding `isearch-edit-string' (but less flexible):
;; (setq search-exit-option  'edit) ; M- = edit search string, not exit.

(when (fboundp 'isearchp-eval-sexp-and-insert)
  (define-key isearch-mode-map "\M-:"             'isearchp-eval-sexp-and-insert))
(define-key isearch-mode-map (kbd "C-M-`")        'isearchp-toggle-literal-replacement)
(define-key isearch-mode-map "\M-c"               'isearch-toggle-case-fold)
;; This one is needed only for Emacs 20.  It is automatic after release 20.
(define-key isearch-mode-map "\M-e"               'isearch-edit-string)
(define-key isearch-mode-map "\M-g"               'isearchp-retrieve-last-quit-search)
(define-key isearch-mode-map "\M-k"               'isearchp-cycle-mismatch-removal)
;; This one is needed only for Emacs 20.  It is automatic after release 20.
(define-key isearch-mode-map "\M-r"               'isearch-toggle-regexp)
(define-key isearch-mode-map (kbd "M-s h R")      'isearchp-toggle-highlighting-regexp-groups)
(define-key isearch-mode-map "\M-si"              'isearch-toggle-invisible)
(define-key isearch-mode-map "\M-sv"              'isearchp-toggle-option-toggle)
(when (< emacs-major-version 23)
  (define-key isearch-mode-map "\M-sw"            'isearch-toggle-word))
(when (fboundp 'isearchp-toggle-repeat-search-if-fail)
  (define-key isearch-mode-map "\M-s\M-k"         'isearchp-toggle-repeat-search-if-fail))
(define-key isearch-mode-map [(meta ?s) (meta ? )] 'isearchp-toggle-set-region)
(define-key isearch-mode-map "\M-w"               'isearchp-kill-ring-save)
(when (fboundp 'isearch-yank-internal)
  (define-key isearch-mode-map "\C-_"             'isearchp-yank-symbol-or-char)
  (define-key isearch-mode-map [(control ?\()]    'isearchp-yank-sexp-symbol-or-char))
(when (and (fboundp 'goto-longest-line)  window-system) ; Defined in `misc-cmds.el'
  (define-key isearch-mode-map [(control end)]    'goto-longest-line))
(define-key isearch-mode-map [next]               'isearch-repeat-forward)
(define-key isearch-mode-map [prior]              'isearch-repeat-backward)
(when (and (eq system-type 'windows-nt) ; Windows uses M-TAB for something else.
           (not (lookup-key isearch-mode-map [C-M-tab]))
           (not (featurep 'icicles)))
  (define-key isearch-mode-map [C-M-tab]          'isearchp-complete))
(when (> emacs-major-version 21)
  (define-key isearch-mode-map "\C-x"             nil)
  (when (or (> emacs-major-version 24)  ; Emacs 24.3+
            (and (= emacs-major-version 24)  (> emacs-minor-version 2)))
    (define-key isearch-mode-map "\C-xn"          'isearchp-toggle-region-restriction)) ; `n'arrow to region
  (define-key isearch-mode-map "\C-xo"            'isearchp-open-recursive-edit) ; `o'pen edit session
  ;; Do this even for Emacs 24.4+ (where it is true by default), because we set `C-x' to nil.
  (when (> emacs-major-version 22)      ; Emacs 23+ (supports Unicode)
    (define-key isearch-mode-map "\C-x8"          nil)
    (define-key isearch-mode-map "\C-x8\r"        'isearch-char-by-name)))

(define-key isearch-mode-map "\C-xrg" 'isearchp-append-register)

(define-key isearch-mode-map "\C-y"               nil) ; Put all yanking commands on prefix `C-y'.
(when (fboundp 'isearch-yank-internal)
  (define-key isearch-mode-map (kbd "C-y C-c")    'isearchp-yank-char)
  (define-key isearch-mode-map (kbd "C-y C-e")    'isearchp-yank-line)
  (define-key isearch-mode-map (kbd "C-y C-w")    'isearchp-yank-word-or-char)
  (define-key isearch-mode-map (kbd "C-y C-_")    'isearchp-yank-symbol-or-char)
  (define-key isearch-mode-map (kbd "C-y C-(")    'isearchp-yank-sexp-symbol-or-char))

(define-key isearch-mode-map (kbd "M-s C-e")      'isearchp-yank-line) ; Replace vanilla `isearch-yank-line'.

(eval-after-load "second-sel"
  '(progn
    (define-key isearch-mode-map (kbd "C-y C-2")  'isearch-yank-secondary)
    (define-key isearch-mode-map (kbd "C-M-y")    'isearch-yank-secondary)))
(define-key isearch-mode-map "\C-y\C-y"           'isearch-yank-kill)
(define-key isearch-mode-map "\C-y\M-g"           'isearchp-retrieve-last-quit-search)
(when (fboundp 'isearch-yank-pop)
  (define-key isearch-mode-map "\C-y\M-y"         'isearch-yank-pop)) ; It is also just `M-y'.

(when (fboundp 'isearchp-act-on-demand) ; Emacs 22+
  (define-key isearch-mode-map (kbd "C-M-<return>") 'isearchp-act-on-demand))

(when (fboundp 'isearchp-remove-failed-part) ; Emacs 22+
  (define-key isearch-mode-map (kbd "C-M-l")      'isearchp-remove-failed-part))

(when (fboundp 'isearchp-remove-failed-part-or-last-char) ; Emacs 22+
  (define-key isearch-mode-map (kbd "C-<backspace>") 'isearchp-remove-failed-part-or-last-char))

(when (and (eq system-type 'windows-nt) ; Windows uses `M-TAB' for something else.
           (not (lookup-key minibuffer-local-isearch-map [C-M-tab])))
  (define-key minibuffer-local-isearch-map [C-M-tab] 'isearch-complete-edit))
(when (> emacs-major-version 22)
  (define-key minibuffer-local-isearch-map "\C-x8\r" 'insert-char))

(defun isearchp-set-region ()
  "Set the region around the search target, if `isearchp-set-region-flag'.
This is used only for Transient Mark mode."
  (when (and isearchp-set-region-flag  transient-mark-mode)
    (push-mark isearch-other-end t 'activate)))

(add-hook 'isearch-mode-end-hook 'isearchp-set-region)

(defun isearchp-highlight-lighter ()
  "Update Isearch mode-line lighter to reflect search state."
  (let ((case-fold-search  isearch-case-fold-search))
    (when (and (eq case-fold-search t)  search-upper-case)
      (setq case-fold-search  (isearch-no-upper-case-p isearch-string isearch-regexp)))
    ;; Vanilla Isearch uses the symbol `isearch-mode', hence the first of these.
    (setq minor-mode-alist  (delete '(isearch-mode isearch-mode) minor-mode-alist)
          minor-mode-alist  (delete '(isearch-mode " ISEARCH")   minor-mode-alist)
          minor-mode-alist  (delete '(isearch-mode " Isearch")   minor-mode-alist)
          minor-mode-alist  (delete '(isearch-mode " R*SEARCH")   minor-mode-alist)
          minor-mode-alist  (delete '(isearch-mode " R*search")   minor-mode-alist))
    (let ((lighter  (if case-fold-search
                        (if isearch-regexp " R*SEARCH" " ISEARCH")
                      (if isearch-regexp " R*search" " Isearch"))))
      (add-to-list
       'minor-mode-alist
       `(isearch-mode ,(cond ((and isearch-wrapped  (facep 'isearchp-overwrapped)
                                   (not isearch-wrap-function)
                                   (if isearch-forward
                                       (> (point) isearch-opoint)
                                     (< (point) isearch-opoint)))
                              (propertize lighter 'face 'isearchp-overwrapped))
                             ((and isearch-wrapped  (facep 'isearchp-wrapped))
                              (propertize lighter 'face 'isearchp-wrapped))
                             (t lighter))))))
  (condition-case nil
      (if (fboundp 'redisplay) (redisplay t) (force-mode-line-update t))
    (error nil)))

(add-hook 'isearch-update-post-hook 'isearchp-highlight-lighter)

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'isearch+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch+.el ends here

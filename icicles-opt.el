;;; icicles-opt.el --- User options (customizable variables) for Icicles
;;
;; Filename: icicles-opt.el
;; Description: User options (customizable variables) for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2017, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:22:14 2006
;; Last-Updated: Wed Jul 26 08:21:13 2017 (-0700)
;;           By: dradams
;;     Update #: 6189
;; URL: https://www.emacswiki.org/emacs/download/icicles-opt.el
;; Doc URL: https://www.emacswiki.org/emacs/Icicles
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `avoid', `bookmark', `bookmark+',
;;   `bookmark+-1', `bookmark+-bmu', `bookmark+-key',
;;   `bookmark+-lit', `cl', `cus-theme', `el-swank-fuzzy', `ffap',
;;   `ffap-', `fit-frame', `frame-fns', `fuzzy', `fuzzy-match',
;;   `help+20', `hexrgb', `info', `info+20', `kmacro', `levenshtein',
;;   `menu-bar', `menu-bar+', `misc-cmds', `misc-fns', `naked',
;;   `package', `pp', `pp+', `regexp-opt', `second-sel', `strings',
;;   `thingatpt', `thingatpt+', `unaccent', `w32browser-dlgopen',
;;   `wid-edit', `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  user options (variables).  For Icicles documentation, see
;;  `icicles-doc1.el' and `icicles-doc2.el'.
;;
;;  Widgets defined here:
;;
;;    `icicle-key-definition'.
;;
;;  Constants defined here:
;;
;;    `icicle-anychar-regexp', `icicle-Completions-misc-submenu',
;;    `icicle-Completions-save/retrieve-submenu',
;;    `icicle-Completions-sets-submenu',
;;    `icicle-Completions-sorting-submenu',
;;    `icicle-Completions-this-candidate-submenu',
;;    `icicle-Completions-toggle-submenu', `icicle-doremi-submenu'.
;;
;;  Commands defined here:
;;
;;    `icicle-read-kbd-macro'.
;;
;;  User options defined here (in Custom group `Icicles'):
;;
;;    `icicle-act-before-cycle-flag',
;;    `icicle-add-proxy-candidates-flag',
;;    `icicle-alternative-actions-alist',
;;    `icicle-alternative-sort-comparer',
;;    `icicle-anything-transform-candidates-flag',
;;    `icicle-apropos-complete-keys',
;;    `icicle-apropos-complete-no-display-keys',
;;    `icicle-apropos-cycle-next-keys',
;;    `icicle-apropos-cycle-next-action-keys',
;;    `icicle-apropos-cycle-next-alt-action-keys',
;;    `icicle-apropos-cycle-next-help-keys',
;;    `icicle-apropos-cycle-previous-keys',
;;    `icicle-apropos-cycle-previous-action-keys',
;;    `icicle-apropos-cycle-previous-alt-action-keys',
;;    `icicle-apropos-cycle-previous-help-keys',
;;    `icicle-auto-complete-key-delay' (Emacs 22+),
;;    `icicle-bookmark-name-length-max',
;;    `icicle-bookmark-refresh-cache-flag',
;;    `icicle-buffer-candidate-key-bindings', `icicle-buffer-configs',
;;    `icicle-buffer-extras',
;;    `icicle-buffer-ignore-space-prefix-flag',
;;    `icicle-buffer-include-cached-files-nflag',
;;    `icicle-buffer-include-recent-files-nflag',
;;    `icicle-buffer-match-regexp', `icicle-buffer-no-match-regexp',
;;    `icicle-buffer-predicate', `icicle-buffer-prefix-arg-filtering',
;;    `icicle-buffer-require-match-flag',
;;    `icicle-buffer-skip-functions', `icicle-buffer-sort',
;;    `icicle-buffers-ido-like-flag', `icicle-candidate-action-keys',
;;    `icicle-candidate-help-keys', `icicle-candidate-width-factor',
;;    `icicle-cand-preds-all', `icicle-cand-preds-for-bookmark',
;;    `icicle-cand-preds-for-buffer', `icicle-cand-preds-for-color',
;;    `icicle-cand-preds-for-face', `icicle-cand-preds-for-file',
;;    `icicle-cand-preds-for-frame', `icicle-cand-preds-for-misc',
;;    `icicle-cand-preds-for-package', `icicle-cand-preds-for-symbol',
;;    `icicle-cand-preds-for-variable',
;;    `icicle-cand-preds-for-window',
;;    `icicle-change-region-background-flag',
;;    `icicle-change-sort-order-completion',
;;    `icicle-C-l-uses-completion-flag',
;;    `icicle-cmpl-include-cdabbrev-flag',
;;    `icicle-cmpl-max-candidates-to-cycle', `icicle-color-themes',
;;    `icicle-comint-dynamic-complete-replacements',
;;    `icicle-command-abbrev-alist',
;;    `icicle-command-abbrev-match-all-parts-flag',
;;    `icicle-command-abbrev-priority-flag',
;;    `icicle-complete-key-anyway-flag',
;;    `icicle-complete-keys-ignored-prefix-keys' (Eamcs 22+),
;;    `icicle-complete-keys-self-insert-ranges',
;;    `icicle-complete-keys-separator',
;;    `icicle-completing-read+insert-keys',
;;    `icicle-completion-history-max-length',
;;    `icicle-completion-key-bindings',
;;    `icicle-completion-list-key-bindings',
;;    `icicle-completion-style-sets' (Emacs 23+),
;;    `icicle-Completions-display-min-input-chars',
;;    `icicle-completions-format', `icicle-Completions-max-columns',
;;    `icicle-Completions-mouse-3-menu-entries',
;;    `icicle-Completions-text-scale-decrease' (Emacs 23+),
;;    `icicle-Completions-window-max-height',
;;    `icicle-customize-save-flag',
;;    `icicle-customize-save-variable-function',
;;    `icicle-custom-themes' (Emacs 24+),
;;    `icicle-custom-themes-accumulate-flag' (Emacs 24+),
;;    `icicle-custom-themes-update-flag' (Emacs 24+),
;;    `icicle-default-in-prompt-format-function',
;;    `icicle-default-cycling-mode', `icicle-default-thing-insertion',
;;    `icicle-default-value', `icicle-define-alias-commands-flag',
;;    `icicle-deletion-action-flag', `icicle-dot-show-regexp-flag',
;;    `icicle-dot-string', `icicle-expand-input-to-common-match',
;;    `icicle-expand-input-to-common-match-alt', `icicle-file-extras',
;;    `icicle-file-match-regexp', `icicle-file-no-match-regexp',
;;    `icicle-file-predicate', `icicle-file-require-match-flag',
;;    `icicle-file-search-dir-as-dired-flag',
;;    `icicle-file-skip-functions', `icicle-file-sort',
;;    `icicle-files-ido-like-flag',
;;    `icicle-filesets-as-saved-completion-sets-flag',
;;    `icicle-find-file-expand-directory-flag',
;;    `icicle-functions-to-redefine', `icicle-guess-commands-in-path',
;;    `icicle-help-in-mode-line-delay',
;;    `icicle-hide-common-match-in-Completions-flag',
;;    `icicle-hide-non-matching-lines-flag',
;;    `icicle-hide-whitespace-before-comment-flag',
;;    `icicle-highlight-historical-candidates-flag',
;;    `icicle-highlight-input-completion-failure',
;;    `icicle-highlight-input-completion-failure-delay',
;;    `icicle-highlight-input-completion-failure-threshold',
;;    `icicle-highlight-input-initial-whitespace-flag',
;;    `icicle-highlight-lighter-flag',
;;    `icicle-highlight-saved-candidates-flag',
;;    `icicle-icomplete-mode-max-candidates',
;;    `icicle-ignore-comments-flag', `icicle-ignored-directories',
;;    `icicle-image-files-in-Completions',
;;    `icicle-image-preview-in-tooltip',
;;    `icicle-incremental-completion',
;;    `icicle-incremental-completion-delay',
;;    `icicle-incremental-completion-threshold',
;;    `icicle-Info-highlight-visited-nodes' (Emacs 22+),
;;    `icicle-inhibit-advice-functions', `icicle-inhibit-ding-flag',
;;    `icicle-input-string', `icicle-inter-candidates-min-spaces',
;;    `icicle-isearch-complete-keys',
;;    `icicle-isearch-history-insert-keys',
;;    `icicle-keep-Completions-for-sole-dir',
;;    `icicle-key-complete-keys',
;;    `icicle-key-complete-keys-for-minibuffer',
;;    `icicle-key-descriptions-use-<>-flag',
;;    `icicle-key-descriptions-use-angle-brackets-flag' (Emacs 22+),
;;    `icicle-keymaps-for-key-completion',
;;    `icicle-kill-visited-buffers-flag', `icicle-kmacro-ring-max'
;;    (Emacs 22+), `icicle-levenshtein-distance',
;;    `icicle-list-join-string', `icicle-list-nth-parts-join-string',
;;    `icicle-mark-position-in-candidate', `icicle-max-candidates',
;;    `icicle-menu-items-to-history-flag',
;;    `icicle-minibuffer-key-bindings',
;;    `icicle-minibuffer-setup-hook', `icicle-modal-cycle-down-keys',
;;    `icicle-modal-cycle-down-action-keys',
;;    `icicle-modal-cycle-down-alt-action-keys',
;;    `icicle-modal-cycle-down-help-keys',
;;    `icicle-modal-cycle-up-keys',
;;    `icicle-modal-cycle-up-action-keys',
;;    `icicle-modal-cycle-up-alt-action-keys',
;;    `icicle-modal-cycle-up-help-keys',
;;    `icicle-move-Completions-frame', `icicle-no-match-hook',
;;    `icicle-option-type-prefix-arg-list',
;;    `icicle-network-drive-means-remote-flag',
;;    `icicle-point-position-in-candidate',
;;    `icicle-populate-interactive-history-flag' (Emacs 23+),
;;    `icicle-pp-eval-expression-print-length',
;;    `icicle-pp-eval-expression-print-level',
;;    `icicle-prefix-complete-keys',
;;    `icicle-prefix-complete-no-display-keys',
;;    `icicle-prefix-cycle-next-keys',
;;    `icicle-prefix-cycle-next-action-keys',
;;    `icicle-prefix-cycle-next-alt-action-keys',
;;    `icicle-prefix-cycle-next-help-keys',
;;    `icicle-prefix-cycle-previous-keys',
;;    `icicle-prefix-cycle-previous-action-keys',
;;    `icicle-prefix-cycle-previous-alt-action-keys',
;;    `icicle-prefix-cycle-previous-help-keys',
;;    `icicle-quote-shell-file-name-flag',
;;    `icicle-read-char-by-name-multi-completion-flag' (Emacs 23+),
;;    `icicle-read+insert-file-name-keys', `icicle-regexp-quote-flag',
;;    `icicle-regexp-search-ring-max', `icicle-region-background',
;;    `icicle-require-match-flag', `icicle-saved-completion-sets',
;;    `icicle-search-cleanup-flag', `icicle-search-from-isearch-keys',
;;    `icicle-search-highlight-all-current-flag',
;;    `icicle-search-highlight-context-levels-flag',
;;    `icicle-search-highlight-threshold', `icicle-search-hook',
;;    `icicle-search-key-prefix',
;;    `icicle-search-replace-common-match-flag',
;;    `icicle-search-replace-literally-flag',
;;    `icicle-search-replace-whole-candidate-flag',
;;    `icicle-search-ring-max', `icicle-search-whole-word-flag',
;;    `icicle-shell-command-candidates-cache',
;;    `icicle-show-annotations-flag',
;;    `icicle-show-Completions-help-flag',
;;    `icicle-show-Completions-initially-flag',
;;    `icicle-show-multi-completion-flag', `icicle-sort-comparer',
;;    `icicle-sorting-max-candidates', `icicle-sort-orders-alist',
;;    `icicle-special-candidate-regexp',
;;    `icicle-S-TAB-completion-methods-alist',
;;    `icicle-S-TAB-completion-methods-per-command',
;;    `icicle-swank-prefix-length', `icicle-swank-timeout',
;;    `icicle-TAB-completion-methods',
;;    `icicle-TAB-completion-methods-per-command',
;;    `icicle-TAB-shows-candidates-flag',
;;    `icicle-TAB/S-TAB-only-completes-flag', `icicle-recenter',
;;    `icicle-test-for-remote-files-flag',
;;    `icicle-thing-at-point-functions',
;;    `icicle-top-level-key-bindings',
;;    `icicle-top-level-when-sole-completion-delay',
;;    `icicle-top-level-when-sole-completion-flag',
;;    `icicle-touche-pas-aux-menus-flag', `icicle-type-actions-alist',
;;    `icicle-unpropertize-completion-result-flag',
;;    `icicle-update-input-hook', `icicle-use-~-for-home-dir-flag',
;;    `icicle-use-C-for-actions-flag',
;;    `icicle-use-anything-candidates-flag',
;;    `icicle-use-candidates-only-once-flag',
;;    `icicle-widgets-to-redefine', `icicle-word-completion-keys',
;;    `icicle-WYSIWYG-Completions-flag', `icicle-yank-function',
;;    `icicle-zap-to-char-candidates' (Emacs 23+).
;;
;;  Functions defined here:
;;
;;    `icicle-bind-top-level-commands',
;;    `icicle-buffer-sort-*...*-last', `icicle-color-defined-p' (Emacs
;;    22+), `icicle-compute-shell-command-candidates',
;;    `icicle-edmacro-parse-keys', `icicle-ffap-guesser',
;;    `icicle-kbd', `icicle-remap', `icicle-thing-at-point',
;;    `icicle-widgetp'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-current-TAB-method', `icicle-delete-candidate-object',
;;    `icicle-ffap-max-region-size'.
;;
;;  For descriptions of changes to this file, see `icicles-chg.el'.
;;
;;  Note: Occasionally I have renamed or removed an Icicles option.
;;  If you have customized such an option, then your customization
;;  will no longer have any effect.  With the exception of options
;;  `icicle-mode' and `icicle-mode-hook', library `icicles-opt.el'
;;  always contains the complete set of Icicles options.  If your
;;  custom file or init file contains an Icicles option that is not
;;  listed above, then you can remove it because it is obsolete.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  https://www.emacswiki.org/emacs/download/linkd.el.
;;
;;  (@> "Constants used to define user options")
;;  (@> "User options, organized alphabetically, except for dependencies")
 
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

;; Emacs 20 does not DTRT wrt `:type' and `:set' sexps at compile time,
;; so there seems no way around this, short of coding without push and dolist.
;; This MUST be `eval-and-compile', even though in principle `eval-when-compile' should be enough.
(eval-and-compile (when (< emacs-major-version 21) (require 'cl))) ;; dolist, push

(eval-when-compile (require 'cl))       ; incf

(require 'thingatpt)        ;; symbol-at-point, thing-at-point, thing-at-point-url-at-point

(when (and (require 'thingatpt+ nil t)  ; (no error if not found)
           (fboundp 'tap-put-thing-at-point-props)) ; >= 2012-08-21
  (tap-define-aliases-wo-prefix)
  (tap-put-thing-at-point-props))
 ;; list-nearest-point-as-string, non-nil-symbol-name-nearest-point, word-nearest-point

(require 'hexrgb nil t) ;; (no error if not found):
 ;; hexrgb-approx-equal, hexrgb-increment-hue, hexrgb-increment-value, hexrgb-saturation

;; Quiet the byte-compiler.
(defvar shell-completion-execonly)      ; In `shell.el'.

(defvar icicle-dot-string-internal)
(defvar icicle-mode-map)
(defvar icicle-top-level-key-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Constants used to define user options")

;;; Constants used to define user options --

(defconst icicle-Completions-misc-submenu
    '(misc-menu
      menu-item
      "Miscellaneous"
      (keymap
       (complete-for-past-completion menu-item "Complete for Past Completion Input"
        icicle-retrieve-previous-input
        :visible (or (and icicle-C-l-uses-completion-flag (not current-prefix-arg))
                  (and (not icicle-C-l-uses-completion-flag) current-prefix-arg)))
       (previous-completion-input menu-item "Previous Completion Input"
        icicle-retrieve-previous-input
        :visible (not (or (and icicle-C-l-uses-completion-flag (not current-prefix-arg))
                       (and (not icicle-C-l-uses-completion-flag) current-prefix-arg))))
       (next-completion-input menu-item "Next Completion Input"
        icicle-retrieve-next-input)
       (one-off-eval menu-item "One-Off Eval..."
        icicle-pp-eval-expression-in-minibuffer)
       (sep-misc "--")
       (icicles-help menu-item "Icicles Help" icicle-minibuffer-help)))
  "Submenu for miscellaneous operations on completions.")

(defconst icicle-Completions-save/retrieve-submenu
    '(save-retrieve-menu
      menu-item
      "Save/Retrieve"
      (keymap
       (save-all menu-item "Save All" icicle-candidate-set-save)
       (save-all-var menu-item "             to Variable..."
        icicle-candidate-set-save-to-variable)
       (save-all-cache menu-item "             to Cache File..."
        icicle-candidate-set-save-persistently)
       (add-all-to-saved menu-item "Add All to Saved" icicle-candidate-set-save-more)
       (save-selected menu-item "Save Selected (Region) Candidates"
        icicle-candidate-set-save-selected
        :enable (and mark-active (> (region-end) (region-beginning))))
       (clear-saved menu-item "Clear Saved Candidates"
        icicle-candidate-set-save-selected
        :enable (and (boundp 'icicle-saved-completion-candidates)
                 icicle-saved-completion-candidates))
       (add-selected-to-saved menu-item "Add Selected (Region) Candidates"
        icicle-candidate-set-save-more-selected
        :enable (and mark-active (> (region-end) (region-beginning))))
       (sep-save/retrieve-2 "--")
       (retrieve-saved menu-item "Retrieve Saved" icicle-candidate-set-retrieve
        :enable (and (boundp 'icicle-saved-completion-candidates)
                 icicle-saved-completion-candidates))
       (retrieve-more-saved menu-item "Retrieve More Saved"
        icicle-candidate-set-retrieve-more
        :enable (and (boundp 'icicle-saved-completion-candidates)
                 icicle-saved-completion-candidates))))
  "Submenu for saving and retrieving completion candidates.")

(defconst icicle-Completions-sets-submenu
    '(sets-menu
      menu-item
      "Sets"
      (keymap
       (complement menu-item "Complement" icicle-candidate-set-complement)
       (widen menu-item "Or Match Alternative..." icicle-widen-candidates)
       (narrow menu-item "Match Also Regexp..." icicle-narrow-candidates)
       (save-pred-read-var menu-item "Save Predicate to Variable...  (`C-u')"
        icicle-save-predicate-to-variable
        :visible current-prefix-arg)
       (save-pred-std-var menu-item "Save Predicate to `icicle-input-string'"
        icicle-save-predicate-to-variable
        :visible (not current-prefix-arg))
       (intersect menu-item "Intersect Saved" icicle-candidate-set-intersection
        :enable icicle-saved-completion-candidates)
       (difference menu-item "Subtract Saved" icicle-candidate-set-difference
        :enable icicle-saved-completion-candidates)
       (union menu-item "Add (Union) Saved" icicle-candidate-set-union
        :enable icicle-saved-completion-candidates)
       (keep-past-chrono menu-item "Only Previously Entered, By Time  (`C-u')"
        icicle-keep-only-past-inputs
        :visible current-prefix-arg)
       (keep-past-alpha menu-item "Only Previously Entered"
        icicle-keep-only-past-inputs
        :visible (not current-prefix-arg))))
  "Submenu for set operations on completion candidates.")

(defconst icicle-Completions-sorting-submenu
    '(sorting-menu
      menu-item
      "Sorting"
      (keymap
       (change-sort-order menu-item "Change Sort Order  (`C-,')" icicle-change-sort-order
        :visible (let ((use-completion-p  (if (integerp icicle-change-sort-order-completion)
                                              (> (length (icicle-current-sort-functions))
                                                 icicle-change-sort-order-completion)
                                            icicle-change-sort-order-completion)))
                   (or (and (not current-prefix-arg)  use-completion-p)
                       (and current-prefix-arg        (not use-completion-p)))))
       (next-sort-order menu-item "Next Sort Order  (`C-,')" icicle-change-sort-order
        :visible (not (let ((use-completion-p  (if (integerp icicle-change-sort-order-completion)
                                                   (> (length (icicle-current-sort-functions))
                                                      icicle-change-sort-order-completion)
                                                 icicle-change-sort-order-completion)))
                        (or (and (not current-prefix-arg)  use-completion-p)
                            (and current-prefix-arg        (not use-completion-p))))))
       (change-alt-sort menu-item "Change Alternative Sort Order  (`M-,')"
        icicle-change-alternative-sort-order
        :visible (let ((use-completion-p  (if (integerp icicle-change-sort-order-completion)
                                              (> (length (icicle-current-sort-functions))
                                                 icicle-change-sort-order-completion)
                                            icicle-change-sort-order-completion)))
                   (or (and (not current-prefix-arg)  use-completion-p)
                       (and current-prefix-arg        (not use-completion-p)))))
       (next-alt-sort menu-item "Next Alternative Sort Order  (`M-,')"
        icicle-change-alternative-sort-order
        :visible (not (let ((use-completion-p  (if (integerp icicle-change-sort-order-completion)
                                                   (> (length (icicle-current-sort-functions))
                                                      icicle-change-sort-order-completion)
                                                 icicle-change-sort-order-completion)))
                        (or (and (not current-prefix-arg)  use-completion-p)
                            (and current-prefix-arg        (not use-completion-p))))))
       (swap-sort menu-item "Swap Alternative/Normal Sort"
        icicle-toggle-alternative-sorting)))
  "Submenu for sorting completion candidates.")

(defconst icicle-Completions-this-candidate-submenu
    '(this-candidate-menu
      menu-item
      "This Candidate"
      (keymap
       (help-on-cand menu-item "Help About" icicle-help-on-candidate)
       (sep-this-1 "--")
       (action menu-item "Act On  (`C-mouse-2')" icicle-candidate-action)
       (read-fn-invoke menu-item "Apply a Function To...  (`M-mouse-2')"
        icicle-candidate-read-fn-invoke)
       (insert-in-minibuffer menu-item "Insert in Minibuffer  (`C-insert')"
        (lambda ()
          (interactive)
          (select-window (active-minibuffer-window))
          (goto-char (icicle-minibuffer-prompt-end))
          (icicle-clear-minibuffer)
          (insert icicle-last-completion-candidate))
        :help "Insert candidate in minibuffer")
       (sep-this-2 "--")
       (all-cands menu-item "Act on Each Individually" icicle-all-candidates-action)
       (all-list menu-item "Act on All as a List" icicle-all-candidates-list-action)))
  "Submenu for acting on candidate under the mouse.")

(defconst icicle-Completions-toggle-submenu
    '(toggle-menu
      menu-item
      "Toggle/Cycle/Change"
      (keymap
       ;; This one is not a toggle or cycle.
       (regexp-quote-input menu-item "Regexp-Quote Current Input"
        icicle-regexp-quote-input
        :visible (not (and mark-active (> (region-end) (region-beginning)))))
       ;; This one is not a toggle or cycle.
       (regexp-quote-region menu-item "Regexp-Quote Input Region"
        icicle-regexp-quote-input
        :visible (and mark-active (> (region-end) (region-beginning))))
       (next-thumbnail-setting menu-item "Next Image-File Thumbnail Setting"
        icicle-cycle-image-file-thumbnail
        :visible (fboundp 'icicle-cycle-image-file-thumbnail))
       (oneoff-next-S-TAB menu-item "ONE-OFF Next S-TAB Completion Method"
        icicle-next-S-TAB-completion-method
        :visible current-prefix-arg)
       (next-S-TAB menu-item "Next S-TAB Completion Method"
        icicle-next-S-TAB-completion-method
        :visible (not current-prefix-arg))
       (oneoff-next-TAB menu-item "ONE-OFF Next TAB Completion Method"
        icicle-next-TAB-completion-method
        :visible current-prefix-arg)
       (next-TAB menu-item "Next TAB Completion Method"
        icicle-next-TAB-completion-method
        :visible (not current-prefix-arg))
       (comp-mode-keys menu-item "Completion Mode Keys"
        icicle-toggle-completion-mode-keys)
       (using-C-for-actions menu-item "Toggle Using `C-' for Actions"
        icicle-toggle-C-for-actions)
       (using-~-for-home menu-item "Toggle Using `~' for $HOME"
        icicle-toggle-~-for-home-dir)
       (removing-dups menu-item "Toggle Duplicate Removal" icicle-toggle-transforming)
       (proxy-candidates menu-item "Toggle Including Proxy Candidates"
        icicle-toggle-proxy-candidates)
       (case-sensitivity menu-item "Toggle Case Sensitivity  (`C-A')"
        icicle-toggle-case-sensitivity)
       (highlighting-past menu-item "Toggle Highlighting Past Inputs"
        icicle-toggle-highlight-historical-candidates)
       (highlighting-saved menu-item "Toggle Highlighting Saved Candidates"
        icicle-toggle-highlight-saved-candidates)
       (WYSIWYG menu-item "Toggle WYSIWYG for `*Completions*'" icicle-toggle-WYSIWYG-Completions)
       (angle-brackets menu-item "Toggle Using Angle Brackets" icicle-toggle-angle-brackets)
       (remote-file-testing menu-item "Toggle Remote File Handling  (`C-^')"
        icicle-toggle-remote-file-testing)
       (expanding-directories menu-item "Toggle Expanding Directories  (`C-x /')"
        icicle-toggle-expand-directory)
       (ignored-files menu-item "Toggle Ignored File Extensions  (`C-.')"
        icicle-toggle-ignored-extensions)
       (ignoring-space-prefix menu-item "Toggle Ignoring Space Prefix"
        icicle-toggle-ignored-space-prefix)
       (ignoring-comments menu-item "Toggle Ignoring Comments"
        icicle-toggle-ignoring-comments)
       (expanding-to-common menu-item "Toggle Common Match Expansion"
        icicle-toggle-expand-to-common-match)
       (hiding-common-match menu-item "Toggle Hiding Common Match  (`C-x .')"
        icicle-toggle-hiding-common-match)
       (hiding-non-matching-lines menu-item "Toggle Hiding Non-Matching Lines  (`C-u C-x .')"
        icicle-toggle-hiding-non-matching-lines)
       (completions-format menu-item "Toggle Horizontal/Vertical Layout"
        icicle-toggle-completions-format)
       (multi-completions menu-item "Toggle Showing Multi-Completions"
        icicle-toggle-show-multi-completion)
       (incremental-completion menu-item "Cycle Incremental Completion"
        icicle-cycle-incremental-completion)
       (icomplete-mode menu-item "Toggle Icomplete Mode"
        icicle-toggle-icomplete-mode)
       (matching-of-newlines menu-item "Toggle `.' Matching of Newlines Too"
        icicle-toggle-dot)
       (literal-vs-regexp menu-item "Toggle Escaping Special Chars"
        icicle-toggle-regexp-quote)
       (sep-toggle-2 "--")
       (search-highlight-all menu-item "Toggle All-Current Search Highlighting  (`C-^')"
        icicle-toggle-highlight-all-current)
       (search-complementing-domain menu-item "Toggle Searching Complement"
        icicle-toggle-search-complementing-domain)
       (search-whole-word menu-item "Toggle Whole-Word Searching  (`M-q')"
        icicle-toggle-search-whole-word)
       (search-replace-whole menu-item "Toggle Replacing Whole Search Hit  (`M-_')"
        icicle-toggle-search-replace-whole)
       (search-replace-common menu-item "Toggle Replacing Longest Common Match"
        icicle-toggle-search-replace-common-match)
       (search-cleanup menu-item "Toggle Removal of Search Highlighting  (`C-.')"
        icicle-toggle-search-cleanup)
       (sep-toggle-1 "--")
       (option menu-item "+ Toggle Option..." icicle-toggle-option
        :visible (and current-prefix-arg (wholenump (prefix-numeric-value current-prefix-arg))))
       (any-var menu-item "+ Toggle Any Variable..." icicle-toggle-option
        :visible (and current-prefix-arg
                  (not (wholenump (prefix-numeric-value current-prefix-arg)))))
       (boolean menu-item "+ Toggle Boolean Option..."
        :visible (not current-prefix-arg))
       ;; This one is not a toggle or cycle.
       (reset-var menu-item "+ Set Any Variable to `nil'..." icicle-reset-option-to-nil
        :visible current-prefix-arg)
       ;; This one is not a toggle or cycle.
       (reset-option menu-item "+ Set Option to `nil'..."icicle-reset-option-to-nil
        :visible (not current-prefix-arg))
       ;; This one is not a toggle or cycle.
       (set-option-to-t menu-item "+ Set Option to `t'..." icicle-set-option-to-t
        :visible (and current-prefix-arg (wholenump (prefix-numeric-value current-prefix-arg))))
       ;; This one is not a toggle or cycle.
       (set-var-to-t menu-item "+ Set Any Variable to `t'..." icicle-set-option-to-t
        :visible (and current-prefix-arg
                  (not (wholenump (prefix-numeric-value current-prefix-arg)))))
       ;; This one is not a toggle or cycle.
       (set-boolean-to-t menu-item "+ Set Boolean Option to `t'..." icicle-set-option-to-t
        :visible (not current-prefix-arg))))
  "Submenu for toggling, cycling or changing a variable or a behavior.")

(defvar icicle-current-TAB-method nil
  "*Current completion method for \
`\\<minibuffer-local-completion-map>\\[icicle-prefix-complete]'.")

(defconst icicle-doremi-submenu
    '(doremi-menu
      menu-item
      "Do Re Mi"
      (keymap
       (zoom "*Completions* Zoom Factor  (`C-x -')" icicle-doremi-zoom-Completions+
        :visible (fboundp 'text-scale-increase) ) ; Emacs 23+.
       (spacing "*Completions* Candidate Spacing  (`C-x |')"
        icicle-doremi-inter-candidates-min-spaces+)
       (column-width "*Completions* Column Width  (`C-x w')" icicle-doremi-candidate-width-factor+)
       (swank-prefix "Swank Min Match Chars  (`C-x 2')" icicle-doremi-increment-swank-prefix-length+
        :visible (eq (icicle-current-TAB-method) 'swank))
       (swank-timeout "Swank Timeout  (`C-x 1')" icicle-doremi-increment-swank-timeout+
        :visible (eq (icicle-current-TAB-method) 'swank))
       (max-completions "Max # of Completions  (`C-x #')" icicle-doremi-increment-max-candidates+)))
  "Submenu for Do Re Mi incrementation operations.")
 
;;(@* "User options, organized alphabetically, except for dependencies")

;;; User options, organized alphabetically, except for dependencies --


;;; But first some functions and a widget that are used in option definitions.

;; Same as `naked-edmacro-parse-keys' in `naked.el'.
;; Based on `edmacro-parse-keys' in standard library `edmacro.el'
;; Differences are:
;;
;; 1. Addition of optional arg ANGLES.
;; 2. Ensure same behavior as `edmacro-parse-keys', if ANGLES is non-nil.
;; 2. Handle angle brackets, whether ANGLES is nil or non-nil.
;; 3. Handle `TAB' correctly, if ANGLES is nil.
;; 4. Handle names without angle brackets, if ANGLES is nil.
;; 5. Works for all Emacs versions.
;;
(defun icicle-edmacro-parse-keys (string &optional need-vector angles)
  "Like `edmacro-parse-keys', but does not use angle brackets, by default.
Non-nil optional arg ANGLES means to use angle brackets, exactly like
`edmacro-parse-keys'.  See `icicle-read-kbd-macro' for more about
ANGLES."
  (let ((case-fold-search  nil)
	(len               (length string)) ; We won't alter string in the loop below.
        (pos               0)
        (res               []))
    (while (and (< pos len)  (string-match "[^ \t\n\f]+" string pos))
      (let* ((word-beg  (match-beginning 0))
	     (word-end  (match-end 0))
	     (word      (substring string word-beg len))
	     (times     1)
             (key       nil))
	;; Try to catch events of the form "<as df>".
        (if (string-match "\\`<[^ <>\t\n\f][^>\t\n\f]*>" word)
            (setq word  (match-string 0 word)
                  pos   (+ word-beg (match-end 0)))
          (setq word  (substring string word-beg word-end)
                pos   word-end))
        (when (string-match "\\([0-9]+\\)\\*." word)
          (setq times  (string-to-number (substring word 0 (match-end 1)))
                word   (substring word (1+ (match-end 1)))))
        (cond ((string-match "^<<.+>>$" word)
               (setq key  (vconcat (if (eq (key-binding [?\M-x])
                                           'execute-extended-command)
                                       [?\M-x]
                                     (or (car (where-is-internal 'execute-extended-command))  [?\M-x]))
                                   (substring word 2 -2) "\r")))

              ;; Must test this before [ACHMsS]- etc., to prevent match.
              ((or (equal word "REM")  (string-match "^;;" word))
               (setq pos  (string-match "$" string pos)))

              ;; Straight `edmacro-parse-keys' case - ensure same behavior.
              ;; Includes same bugged handling of `TAB'.  That is Emacs bug #12535.
              ;; The bug fix is to add `TAB' to the list in this clause.
	      ((and angles  (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(.+\\)>$" word)
		    (progn
		      (setq word  (concat (substring word (match-beginning 1)
                                                     (match-end 1))
                                          (substring word (match-beginning 3)
                                                     (match-end 3))))
		      (not (string-match "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$" word))))
	       (setq key  (list (intern word))))

              ;; NaKeD handling of <...>.  Recognize it anyway, even without non-nil ANGLES.
              ;; But unlike `edmacro-parse-keys', include <TAB>, to handle it correctly.
              ((and (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(..+\\)>$" word)
                    (progn (setq word  (concat (substring word (match-beginning 1) (match-end 1))
                                               (substring word (match-beginning 3) (match-end 3))))
                           (not (string-match "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\|TAB\\)$"
                                              word))))
               (setq key  (list (intern word))))

              ;; NaKeD handling of names without <...>.
              ((and (not angles)
                    (string-match "^\\(\\([ACHMsS]-\\)*\\)\\([^ \t\f\n][^ \t\f\n]+\\)$" word)
                    ;; Do not count `C-' etc. when at end of string.
                    (save-match-data (not (string-match "\\([ACHMsS]-.\\)+$" word)))
                    (progn (setq word  (concat (substring word (match-beginning 1) (match-end 1))
                                               (substring word (match-beginning 3) (match-end 3))))
                           (not (string-match "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\|TAB\\)$"
                                              word))))
               (setq key  (list (intern word))))

              (t
               (let ((orig-word  word)
                     (prefix     0)
                     (bits       0))
                 (while (string-match "^[ACHMsS]-." word)
                   (incf bits (cdr (assq (aref word 0) '((?A . ?\A-\^@) (?C . ?\C-\^@)
                                                         (?H . ?\H-\^@) (?M . ?\M-\^@)
                                                         (?s . ?\s-\^@) (?S . ?\S-\^@)))))
                   (incf prefix 2)
                   (callf substring word 2))
                 (when (string-match "^\\^.$" word)
                   (incf bits ?\C-\^@)
                   (incf prefix)
                   (callf substring word 1))
                 (let ((found  (assoc word '(("NUL" . "\0") ("RET" . "\r") ("LFD" . "\n")
                                             ("ESC" . "\e") ("SPC" . " ") ("DEL" . "\177")
                                             ("TAB" . "\t")))))
                   (when found (setq word  (cdr found))))
                 (when (string-match "^\\\\[0-7]+$" word)
                   (loop for ch across word
                         for n = 0 then (+ (* n 8) ch -48)
                         finally do (setq word  (vector n))))
                 (cond ((= bits 0) (setq key  word))
                       ((and (= bits ?\M-\^@)  (stringp word)  (string-match "^-?[0-9]+$" word))
                        (setq key  (loop for x across word collect (+ x bits))))
                       ((/= (length word) 1)
                        (error "%s must prefix a single character, not %s"
                               (substring orig-word 0 prefix) word))
                       ((and (/= (logand bits ?\C-\^@) 0)  (stringp word)
                             ;; Used to accept `.' and `?' here, but `.' is simply wrong,
                             ;; and `C-?' is not used (so use `DEL' instead).
                             (string-match "[@-_a-z]" word))
                        (setq key  (list (+ bits (- ?\C-\^@) (logand (aref word 0) 31)))))
                       (t (setq key  (list (+ bits (aref word 0)))))))))
        (when key (loop repeat times do (callf vconcat res key)))))
    (when (and (>= (length res) 4)  (eq (aref res 0) ?\C-x)  (eq (aref res 1) ?\( )
               (eq (aref res (- (length res) 2)) ?\C-x)  (eq (aref res (- (length res) 1)) ?\)))
      (setq res  (edmacro-subseq res 2 -2)))
    (if (and (not need-vector)
	     (loop for ch across res
		   always (and (if (fboundp 'characterp) (characterp ch) (char-valid-p ch))
			       (let ((ch2  (logand ch (lognot ?\M-\^@))))
				 (and (>= ch2 0)  (<= ch2 127))))))
	(concat (loop for ch across res collect (if (= (logand ch ?\M-\^@) 0) ch (+ ch 128))))
      res)))

;; Same as `naked-read-kbd-macro' in `naked.el'.
(defun icicle-read-kbd-macro (start &optional end angles)
  "Read the region as a keyboard macro definition.
Like `read-kbd-macro', but does not use angle brackets, by default.

With a prefix arg use angle brackets, exactly like `read-kbd-macro'.
That is, with non-nil arg ANGLES, expect key descriptions to use angle
brackets (<...>).  Otherwise, expect key descriptions not to use angle
brackets.  For example:

 (icicle-read-kbd-macro  \"mode-line\"  t) returns [mode-line]
 (icicle-read-kbd-macro \"<mode-line>\" t t)   returns [mode-line]"
  (interactive "r\P")
  (if (stringp start)
      (icicle-edmacro-parse-keys start end angles)
    (setq last-kbd-macro  (icicle-edmacro-parse-keys (buffer-substring start end) nil angles))))

(put 'icicle-kbd 'pure t)
;; Same as `naked' in `naked.el'.
(defun icicle-kbd (keys &optional angles)
  "Like `kbd', but does not use angle brackets, by default.
With non-nil optional arg ANGLES, expect key descriptions to use angle
brackets (<...>), exactly like `kbd'.  Otherwise, expect key
descriptions not to use angle brackets.  For example:

 (icicle-kbd \"mode-line\")     returns [mode-line]
 (icicle-kbd \"<mode-line>\" t) returns [mode-line]

The default behavior lets you use, e.g., \"C-x delete\" and \"C-delete\"
instead of \"C-x <delete>\" and \"C-<delete>\"."
  (icicle-read-kbd-macro keys nil angles))

;; Must be before first use, which is currently in `icicle-buffer-candidate-key-bindings'.
(define-widget 'icicle-key-definition 'lazy
  "Key definition type for Icicle mode keys.
A list of three components: KEY, COMMAND, CONDITION, that represents
an `icicle-mode-map' binding of COMMAND according to KEY, if CONDITION
evaluates to non-nil.

KEY is either a key sequence (string or vector) or a command.
COMMAND is a command.
CONDITION is a sexp.

If KEY is a command, then the binding represented is its remapping to
COMMAND."
  :indent 1 :offset 0 :tag ""           ; $$$$$ "Icicle Mode Key Definition"
  :type
  '(list
    (choice
     (key-sequence :tag "Key" :value [ignore])
     ;; Use `symbolp' instead of `commandp', in case the library defining the
     ;; command is not loaded.
     (restricted-sexp :tag "Command to remap" :match-alternatives (symbolp) :value ignore))
     ;; Use `symbolp' instead of `commandp'...
    (restricted-sexp :tag "Command" :match-alternatives (symbolp) :value ignore)
    (sexp :tag "Condition")))

(defcustom icicle-act-before-cycle-flag nil
  "*Non-nil means act on current candidate, then cycle to next/previous.
Otherwise (nil), cycle to the next or previous candidate, and then act
on it.

This affects keys such as the following:

 `C-down',   `C-wheel-down',   `C-next',   `C-end',
 `C-M-down', `C-M-wheel-down', `C-M-next', `C-M-end',
 `C-S-down', `C-S-wheel-down', `C-S-next', `C-S-end'.

Note: A few Icicles commands ignore this setting, in order to \"do the
right thing\".

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Key-Bindings :group 'Icicles-Miscellaneous)

(defcustom icicle-add-proxy-candidates-flag nil ; Toggle with `C-M-_'.
  "*Non-nil means to include proxy candidates whenever possible.
A proxy candidate is a candidate (shown in `*Completions*' using face
`icicle-proxy-candidate') whose name is a placeholder for the real
candidate.  The proxy candidate typically stands for some value
obtained from the cursor position or by some action such as clicking
the mouse.  Example candidates include a color or file name, named by
proxy candidates such as `*copied foreground*' or `*file at point*'.

You can toggle this option at any time from the minibuffer using
`\\<minibuffer-local-completion-map>\\[icicle-toggle-proxy-candidates]'.  However, for \
commands that provide many proxy candidates, if
the flag is off initially when input is read, then you must re-invoke
the completing command for the new value to take effect.  (This is for
performance reasons.)

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Matching)

(defcustom icicle-alternative-actions-alist ()
  "*Alist of Emacs commands and alternative action functions.
This always overrides any alternative action defined by
`icicle-candidate-alt-action-fn'.

Each alist element has the form (COMMAND . FUNCTION), where COMMAND is
a command (a symbol) that reads input and FUNCTION is the
alternative-action function it uses.  To disable alternative action
for a given command, use `ignore' as the FUNCTION.

This option has no effect on `icicle-all-candidates-list-alt-action',
that is, `M-|', but it does affect `C-|'."
  :type '(alist
          :key-type   (symbol   :tag "Command")
          :value-type (function :tag "Alternative action (function)"))
  :group 'Icicles-Miscellaneous)

(defcustom icicle-alternative-sort-comparer 'icicle-historical-alphabetic-p ; Toggle with `C-M-,'.
  "*An alternative sort function, in place of `icicle-sort-comparer'.
You can swap this with `icicle-sort-comparer' at any time by using
`icicle-toggle-alternative-sorting' (\\<minibuffer-local-completion-map>\
`\\[icicle-toggle-alternative-sorting]' in the minibuffer)."
  :type '(choice
          (const    :tag "None (do not sort)" nil)
          (function :tag "Sorting Predicate" :value icicle-case-string-less-p)
          (list     :tag "Sorting Multi-Predicate"
           (repeat (function :tag "Component Predicate"))
           (choice
            (const    :tag "None" nil)
            (function :tag "Final Predicate" :value icicle-case-string-less-p))))
  :group 'Icicles-Completions-Display)

;; Must be before `icicle-dot-string'.
(defconst icicle-anychar-regexp (let ((strg  (copy-sequence "\\(.\\|[\n]\\)")))
                                  (set-text-properties 0 (length strg)
                                                       '(display "." face highlight)
                                                       strg)
                                  strg)
  "Regexp that matches any single character, including newline.")

(defcustom icicle-anything-transform-candidates-flag nil
  "*Non-nil means `icicle-anything' transforms completion candidates.
Function `anything-transform-candidates' is used for the transforming.

The advantage of a nil value is that `icicle-anything' then acts as a
multi-command: you can act on multiple candidates, or apply multiple
actions for the same candidate, within a single invocation of
`icicle-anything' (or related commands).

The advantage of a non-nil value is that some of the displayed
Anything candidates might be more readable.

This option has no effect if library `anything.el' cannot be loaded.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Completions-Display :group 'Icicles-Matching)

(defcustom icicle-apropos-complete-keys (if (> emacs-major-version 23) ; `S-TAB'
                                            '([backtab])
                                          '([S-tab] [S-iso-lefttab]))
  ;; In Emacs 22 and later, `backtab' is the canonical key that represents both `S-tab' and
  ;; `S-iso-lefttab', so in principle that could be used in the default value for Emacs 22+.
  ;;
  ;; In other words, the following should be sufficient:
  ;;   (if (> emacs-major-version 21)
  ;;       '([backtab])
  ;;     '([S-tab] [S-iso-lefttab]))
  ;;
  ;; However, some Emacs 22+ libraries, such as `info.el', are brain-dead and explicitly
  ;; bind both `backtab' and `S-tab'.  I filed Emacs bug #1281.
  "*Key sequences to use for `icicle-apropos-complete'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards - for example, `S-tab' and `S-iso-lefttab'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-apropos-complete-no-display-keys '([C-M-S-tab] ; `C-M-S-TAB'
                                                     [C-M-S-iso-lefttab])
  "*Key sequences to use for `icicle-apropos-complete-no-display'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards - for example, `C-M-S-tab' and `C-M-S-iso-lefttab'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-apropos-cycle-next-keys '([next]) ; `next'
  "*Key sequences for apropos completion to cycle to the next candidate.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-apropos-cycle-next-action-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-apropos-cycle-next-action-keys '([C-next]) ; `C-next'
  "*Keys for apropos completion to cycle next and perform action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-apropos-cycle-next-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-apropos-cycle-next-alt-action-keys '([C-S-next]) ; `C-S-next'
  "*Keys for apropos completion to cycle next and perform alt action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-apropos-cycle-next-help-keys '([C-M-next]) ; `C-M-next'
  "*Keys for apropos completion to cycle next and show candidate help.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-apropos-cycle-previous-keys '([prior]) ; `prior'
  "*Key sequences for apropos completion to cycle to the previous candidate.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-apropos-cycle-previous-action-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-apropos-cycle-previous-action-keys '([C-prior]) ; `C-prior'
  "*Keys for apropos completion to cycle previous and perform action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-apropos-cycle-previous-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-apropos-cycle-previous-alt-action-keys '([C-S-prior]) ; `C-S-prior'
  "*Keys for apropos completion to cycle previous and perform alt action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-apropos-cycle-previous-help-keys '([C-M-prior]) ; `C-M-prior'
  "*Keys for apropos completion to cycle previous and show candidate help.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(when (fboundp 'map-keymap)             ; Emacs 22+.
  (defcustom icicle-auto-complete-key-delay 1.0
    "*Seconds to wait when idle, before automatically completing a key.
This has an effect only when `icicle-auto-complete-keys-mode' is on.
If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
    :type 'number :group 'Icicles-Key-Completion))

(defcustom icicle-bookmark-name-length-max 70
  "*Maximum number of characters used to name a bookmark.
When `icicle-bookmark-cmd' is used with a non-negative numeric prefix
arg, the name of the bookmark that is set has at most this many chars.

If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'integer :group 'Icicles-Miscellaneous)

(defcustom icicle-bookmark-refresh-cache-flag t
  "*t means `icicle-bookmark' refreshes the bookmark-list cache.
Use nil to speed up `icicle-bookmark(-other-window)' if you have a lot
of bookmarks, at the cost of having the bookmark list possibly not be
up to date.  Use t if you want to be sure the list is refreshed.

If nil, the list of bookmarks is updated only if you use `C-u'.
If t, the list is always updated unless you use `C-u'.

This affects only commands such as `icicle-bookmark' that use the full
bookmark list.  It does not affect more specific Icicles bookmark
commands such as `\\[icicle-bookmark-dired-other-window]' or the use
of a negative prefix arg with
`\\[icicle-bookmark-cmd]'.

Regardless of the option value, the cache is refreshed whenever you
use `S-delete' to delete a candidate bookmark.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Completions-Display :group 'Icicles-Matching)

(defcustom icicle-buffer-candidate-key-bindings
  `(
    (,(icicle-kbd "C-x m")         icicle-bookmark-non-file-other-window             ; `C-x m'
     (require 'bookmark+ nil t))
    (,(icicle-kbd "C-x * -")       icicle-remove-buffer-cands-for-modified        t) ; `C-x * -'
    (,(icicle-kbd "C-x * +")       icicle-keep-only-buffer-cands-for-modified     t) ; `C-x * +'
    (,(icicle-kbd "C-x M -")       icicle-remove-buffer-cands-for-mode            t) ; `C-x M -'
    (,(icicle-kbd "C-x M +")       icicle-keep-only-buffer-cands-for-mode         t) ; `C-x M +'
    (,(icicle-kbd "C-x C-m -")     icicle-remove-buffer-cands-for-derived-mode    t) ; `C-x C-m -'
    (,(icicle-kbd "C-x C-m +")     icicle-keep-only-buffer-cands-for-derived-mode t) ; `C-x C-m +'
    (,(icicle-kbd "C-x v -")       icicle-remove-buffer-cands-for-visible         t) ; `C-x v -'
    (,(icicle-kbd "C-x v +")       icicle-keep-only-buffer-cands-for-visible      t) ; `C-x v +'
    (,(icicle-kbd "C-x F"  )       icicle-toggle-include-cached-files             t) ; `C-x F'
    (,(icicle-kbd "C-x R"  )       icicle-toggle-include-recent-files             t) ; `C-x R'
    )
  "*List of minibuffer key bindings for use with buffer-name completion.
The option value has the same form as that of option
`icicle-top-level-key-bindings' (which see).
Each list element is of custom type `icicle-key-definition' and has
the form (KEY COMMAND CONDITION)."
  :type (if (> emacs-major-version 21)
            '(repeat icicle-key-definition)
          '(repeat
            (list
             (choice
              (restricted-sexp :tag "Key"
               :match-alternatives ((lambda (x) (or (stringp x)  (vectorp x))))
               :value [ignore])
              (restricted-sexp :tag "Command to remap"
               ;; Use `symbolp' instead of `commandp', in case the library defining the
               ;; command is not loaded.
               :match-alternatives (symbolp) :value ignore))
             ;; Use `symbolp' instead of `commandp'...
             (restricted-sexp :tag "Command"
              :match-alternatives (symbolp) :value ignore)
             (sexp :tag "Condition"))))
  :group 'Icicles-Key-Bindings)

(defcustom icicle-buffer-extras nil
  "*List of additional buffer-name candidates added to the normal list.
List elements are strings."
  :type '(repeat string) :group 'Icicles-Buffers :group 'Icicles-Matching)

(defcustom icicle-buffer-ignore-space-prefix-flag t
  "*Non-nil means ignore buffer-name completions that start with a space.
You can toggle this option from the minibuffer using
`\\<minibuffer-local-completion-map>\\[icicle-dispatch-M-_]' (except during Icicles searching).  \
You can also use
multi-command `icicle-toggle-option' anytime (`M-i M-i' during
completion) to toggle the option."
  :type 'boolean :group 'Icicles-Buffers :group 'Icicles-Matching)

(defcustom icicle-buffer-include-cached-files-nflag (if (boundp 'most-positive-fixnum)
                                                        (- most-positive-fixnum)
                                                      -99999999)
  "*An integer > 0 means include cached files during buffer-name completion.
This means names cached using the Emacs file-name cache - see (emacs)
`File Name Cache'.  An integer < 0 means do not include them.
When they are included, the value is the maximum number of such
candidates to include.

You can toggle this option (between + and -) using `C-x F' in the
minibuffer during buffer-name completion."
  :type '(restricted-sexp :tag "Max number of cached file candidates (+/-)"
          :match-alternatives ((lambda (x) (and (integerp x)  (not (zerop x)))))
          :value ignore)
  :group 'Icicles-Files :group 'Icicles-Buffers :group 'Icicles-Matching)

(defcustom icicle-buffer-include-recent-files-nflag (if (boundp 'most-positive-fixnum)
                                                        (- most-positive-fixnum)
                                                      -99999999)
  "*An integer > 0 means include recent files during buffer-name completion.
This means file names managed by `recentf-mode' - see (emacs) `File
Conveniences'.  An integer < 0 means do not include them.
When they are included, the value is the maximum number of such
candidates to include.

You can toggle this option (between + and -) using `C-x R' in the
minibuffer during buffer-name completion.

This option has no effect prior to Emacs 21 (no library `recentf.el')."
  :type '(restricted-sexp :tag "Max number of recent file candidates (+/-)"
          :match-alternatives ((lambda (x) (and (integerp x)  (not (zerop x)))))
          :value ignore)
  :group 'Icicles-Files :group 'Icicles-Buffers :group 'Icicles-Matching)

(defcustom icicle-buffer-match-regexp nil
  "*nil or a regexp that buffer-name completion candidates must match.
If nil, then this does nothing.  If a regexp, then show only
candidates that match it (and match the user input).
See also `icicle-buffer-no-match-regexp'."
  :type '(choice
          (const :tag "None" nil)
          (regexp :value "^[^ ]"))
  :group 'Icicles-Buffers :group 'Icicles-Matching)

(defcustom icicle-buffer-no-match-regexp nil
  "*nil or a regexp that buffer-name completion candidates must not match.
If nil, then this does nothing.  If a regexp, then show only
candidates that do not match it.
See also `icicle-buffer-match-regexp'."
  :type '(choice (const :tag "None" nil)
          (regexp :value "^[ ]"))
  :group 'Icicles-Buffers :group 'Icicles-Matching)

(defcustom icicle-buffer-predicate nil
  "*nil or a predicate that buffer-name candidates must satisfy.
If nil, then this does nothing.  Otherwise, this is a function of one
argument, a candidate, and only candidates that satisfy the predicate
are displayed.  For example, this value will show only buffers that
are associated with files:

  (lambda (bufname) (buffer-file-name (get-buffer bufname)))

This predicate is applied after matching against user input.  It thus
corresponds to `icicle-must-pass-after-match-predicate', not to
`icicle-must-pass-predicate'."
  :type '(choice
          (const :tag "None" nil)
          (function :value (lambda (bufname) (buffer-file-name (get-buffer bufname)))))
  :group 'Icicles-Buffers :group 'Icicles-Matching)

(defcustom icicle-buffer-prefix-arg-filtering 'use-default
  "*Conditions that filter buffers for buffer-name candidates.
This filtering is in addition to removing the names of minibuffers.

If the value is `use-default' then use the default Icicles behavior
\(see the doc).

Otherwise, it is a list of conditions that are applied, in order,
until one matches, to filter out buffers.  If none match then no
filtering is done.

Each condition is a list of two items: (PREF-ARG-TEST BUF-TEST).

PREF-ARG-TEST is a predicate that accepts a raw prefix argument value,
such as is returned by `current-prefix-arg', as its argument.

If PREF-ARG-TEST returns non-nil then BUF-TEST is used to filter
buffers (see next).  If PREF-ARG-TEST returns nil then the next
condition is checked, and so on.  If none of the conditions match then
no filtering is done.

BUF-TEST is a predicate that accepts a buffer as argument.  If it
returns non-nil then the name of that buffer is removed as a
completion candidate.  Alternatively, BUF-TEST can be the constant
`nil', which has the same effect as applying (lambda (buf) nil) as the
filter predicate: the buffer is not removed.

As an example, this value of `icicle-buffer-prefix-arg-filtering'
provides the same behavior as value `use-default' (but it is slower):

  (((lambda (cpa)                                 ; (no prefix arg)
      (null cpa))
    nil)                                          ; Any (no filtering)

   ((lambda (cpa)                                 ; `C-u C-u C-u'
      (and (consp cpa)
           (> (prefix-numeric-value cpa) 16)))
    (lambda (bf) (get-buffer-window bf 0)))       ; Invisible

   ((lambda (cpa)                                 ; `C-u C-u'
      (and (consp cpa)
           (> (prefix-numeric-value cpa) 4)))
    (lambda (bf) (not (get-buffer-window bf 0)))) ; Visible

   ((lambda (cpa)                                 ; `C-u'
      (and (consp current-prefix-arg)
           (fboundp 'derived-mode-p)))
    (lambda (bf)                                  ; Derived mode
      (not (derived-mode-p
             (with-current-buffer bf major-mode)))))

   ((lambda (cpa)                                 ; = 0
      (zerop (prefix-numeric-value cpa)))
    (lambda (bf)
      (let ((this-mode  major-mode))              ; Same mode
        (with-current-buffer bf
          (not (eq major-mode this-mode))))))

   ((lambda (cpa)                                 ; < 0
      (< (prefix-numeric-value cpa) 0))
    (lambda (bf)                                  ; Same frame
      (not (member bf (cdr (assq 'buffer-list (frame-parameters)))))))

   ((lambda (cpa)                                 ; > 0
      cpa)
    (lambda (bf)                                  ; File or dir
      (and (not (buffer-file-name bf))
           (with-current-buffer bf
             (not (eq major-mode 'dired-mode)))))))

Note that when the buffer-name predicate returns non-nil the buffer
name is removed as a candidate.  Hence the descriptions in the
comments above are opposite the predicates."
  :type '(choice
          (const  :tag "Use the default Icicles behavior" use-default)
          (repeat :tag "Conditions tested in order, to filter buffer-name candidates"
           (list
            (function  :tag "Predicate to test raw prefix arg"
             :value (lambda (cpa) (and (consp cpa) (> (prefix-numeric-value cpa) 16)))) ; C-u C-u C-u
            (choice
             (function :tag "Predicate to filter out a buffer name" ; Visible
              :value (lambda (bf) (get-buffer-window bf 0)))
             (const    :tag "No filtering - include all buffer names" nil)))))
  :group 'Icicles-Buffers)

(defcustom icicle-buffer-require-match-flag nil
  "*Override `icicle-require-match-flag' for `icicle-buffer*' commands.
Controls the REQUIRE-MATCH arg to `completing-read' and `read-file-name'.
The possible values are as follows:
- nil means this option imposes nothing on completion;
  the REQUIRE-MATCH argument provided to the function governs behavior
- `no-match-required' means the same as a nil value for REQUIRE-MATCH
- `partial-match-ok' means the same as a t value for REQUIRE-MATCH
- `full-match-required' means the same as a non-nil, non-t value for
  REQUIRE-MATCH

Note: This option is provided mainly for use (binding) in
`icicle-define-command' and `icicle-define-file-command'.
You probably do not want to set this globally, but you can."
  :type '(choice
          (const :tag "Do not impose any match behavior"   nil)
          (const :tag "Do not require a match"             no-match-required)
          (const :tag "Require a partial match, with RET"  partial-match-ok)
          (const :tag "Require a full match"               full-match-required))
  :group 'Icicles-Buffers :group 'Icicles-Matching)

(defcustom icicle-buffer-skip-functions nil
  "*Hook run by `icicle-buffer' on each matching buffer name.
The value is a list of functions.  Each is applied to the buffer-name
candidate (after transforming it, if it is a multi-completion), until
one of them returns a non-nil value.

If any of the functions returns non-nil then the buffer content is not
searched.  Use this to skip visiting and trying to search non-text
buffers, such as PDFs and images, or buffers that might be
time-consuming to access.

See also the `icicle-buffer' doc for other ways to filter the set of
candidate buffers."
  :type 'hook :group 'Icicles-Buffers :group 'Icicles-Matching)

(defcustom icicle-buffer-sort 'icicle-buffer-sort-*...*-last
  "*A sort function for buffer names, or nil.
Examples of sort functions are `icicle-buffer-sort-*...*-last' and
`string<'.  If nil, then buffer names are not sorted."
  :type '(choice
          (const :tag "None" nil)
          (function :value icicle-buffer-sort-*...*-last))
  :group 'Icicles-Buffers :group 'Icicles-Completions-Display)

(defcustom icicle-buffers-ido-like-flag nil
  "*t means `icicle-buffer' and similar commands act more Ido-like.
Specifically, those commands then bind these options to t:
 `icicle-show-Completions-initially-flag'
 `icicle-top-level-when-sole-completion-flag'
 `icicle-default-value'

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Buffers :group 'Icicles-Completions-Display :group 'Icicles-Matching)

(defcustom icicle-candidate-action-keys '([C-return]) ; `C-return'
  "*Keys for acting on the current completion candidate.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-candidate-help-keys '([C-M-return] ; `C-M-return'
                                        [C-help] ; `C-help'
                                        [C-M-help] ; `C-M-help'
                                        [C-f1] ; `C-f1'
                                        [C-M-f1]) ; `C-M-f1'
  "*Keys for showing help about the current completion candidate.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-candidate-width-factor 80
  "*Percentage of widest candidate width to use for calculating columns.
The number of columns of candidates displayed in `*Completions*' is no
more than the window width divided by this percentage of the maximum
candidate width.

Increasing this toward 100 spreads columns out. Decreasing it
compresses columns together.  The higher the value, the more
candidates will form well-defined columns, but the likelier that
horizontal space will be wasted between them.  The lower the value,
the more candidates will not line up in columns, but the less
horizontal space will be wasted between them.

When most candidates are almost as wide as the widest candidate, a
high value works well.  When most candidates are much shorter than the
widest candidate, a low value works well.

If you use Do Re Mi (library `doremi.el') then you can modify this
option incrementally during completion, seeing the effect as it
changes.  Use `C-x w' from the minibuffer, then use the `right' and
`left' arrow keys or the mouse wheel to increment and decrement the
value.  WYSIWYG.

If you use `doremi.el' then you can also use multi-command
`icicle-increment-option' anytime to change the option value
incrementally.

See also option `icicle-inter-candidates-min-spaces' and (starting
with Emacs 23) option `icicle-Completions-text-scale-decrease'."
  :type 'integer :group 'Icicles-Completions-Display)

(defcustom icicle-cand-preds-for-bookmark (and (require 'bookmark+ nil t)
                                               '("icicle-bookmark-annotated-p"
                                                 "icicle-bookmark-autofile-p"
                                                 "icicle-bookmark-autonamed-p"
                                                 "icicle-bookmark-autonamed-this-buffer-p"
                                                 "icicle-bookmark-bookmark-file-p"
                                                 "icicle-bookmark-bookmark-list-p"
                                                 "icicle-bookmark-desktop-p"
                                                 "icicle-bookmark-dired-p"
                                                 "icicle-bookmark-dired-this-dir-p"
                                                 "icicle-bookmark-dired-wildcards-p"
                                                 "icicle-bookmark-file-p"
                                                 "icicle-bookmark-file-this-dir-p"
                                                 "icicle-bookmark-flagged-p"
                                                 "icicle-bookmark-function-p"
                                                 "icicle-bookmark-gnus-p"
                                                 "icicle-bookmark-icicle-search-hits-p"
                                                 "icicle-bookmark-image-p"
                                                 "icicle-bookmark-info-p"
                                                 ;; "icicle-bookmark-last-specific-buffer-p"
                                                 ;; "icicle-bookmark-last-specific-file-p"
                                                 "icicle-bookmark-lighted-p"
                                                 "icicle-bookmark-local-directory-p"
                                                 "icicle-bookmark-local-file-p"
                                                 "icicle-bookmark-man-p"
                                                 "icicle-bookmark-marked-p"
                                                 "icicle-bookmark-modified-p"
                                                 "icicle-bookmark-navlist-p"
                                                 "icicle-bookmark-non-dir-file-p"
                                                 "icicle-bookmark-non-file-p"
                                                 "icicle-bookmark-omitted-p"
                                                 "icicle-bookmark-orphaned-file-p"
                                                 "icicle-bookmark-orphaned-local-file-p"
                                                 "icicle-bookmark-orphaned-remote-file-p"
                                                 "icicle-bookmark-region-p"
                                                 "icicle-bookmark-remote-file-p"
                                                 "icicle-bookmark-sequence-p"
                                                 "icicle-bookmark-snippet-p"
                                                 "icicle-bookmark-tagged-p"
                                                 "icicle-bookmark-temporary-p"
                                                 "icicle-bookmark-this-buffer-p"
                                                 ;; "icicle-bookmark-this-file-p"
                                                 "icicle-bookmark-url-p"
                                                 "icicle-bookmark-url-browse-p"
                                                 "icicle-bookmark-variable-list-p"
                                                 "icicle-bookmark-w3m-p"))
  "*Predicates for bookmark candidates.
Useful for `icicle-narrow-candidates-with-predicate'.
Each value is a string that can be successfully read by `read' and for
which `read' returns a predicate (Boolean function) that accepts a
full completion candidate as argument.  Typically, the strings are
predicate names, but they can also be lambda forms.

Depending on the command, the argument to the predicate can take any
form acceptable as a full completion candidate (alist entry with
string car, symbol, etc.)

To make these predicates available for narrowing for one of your
commands, just bind `icicle-bookmark-completing-p' to non-nil in the
command."
  :type '(repeat (function :tag "Predicate to keep candidate")) :group 'Icicles-Matching)

(defcustom icicle-cand-preds-for-buffer (append '("icicle-buffer-modified-p"
                                                  "icicle-compilation-buffer-p"
                                                  "icicle-special-display-p")
                                                (and (fboundp 'icicle-interesting-buffer-p)
                                                     '("icicle-interesting-buffer-p"))
                                                (and (fboundp 'icicle-next-error-buffer-p)
                                                     '("icicle-next-error-buffer-p")))
  "*Predicates for buffer candidates.
Useful for `icicle-narrow-candidates-with-predicate'.
Each value is a string that can be successfully read by `read' and for
which `read' returns a predicate (Boolean function) that accepts a
full completion candidate as argument.  Typically, the strings are
predicate names, but they can also be lambda forms.

Depending on the command, the argument to the predicate can take any
form acceptable as a full completion candidate (alist entry with
string car, symbol, etc.)

To make these predicates available for narrowing for one of your
commands, just bind `icicle-buffer-completing-p' to non-nil in the
command."
  :type '(repeat (function :tag "Predicate to keep candidate"))
  :group 'Icicles-Matching :group 'Icicles-Buffers)

(when (fboundp 'color-defined-p)        ; Emacs 22+
  (defun icicle-color-defined-p (color &optional frame)
    "Return non-nil if COLOR is supported on frame FRAME.
COLOR should be a string naming a color (e.g. \"white\"), or a
string specifying a color's RGB components (e.g. \"#ff12ec\"), or
the symbol `unspecified'.

If COLOR is a cons with a string car, then the car is used as the
effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is a color name and whose second part is hex RGB.
In this case, the second part is tested.

This function returns nil if COLOR is the symbol `unspecified',
or one of the strings \"unspecified-fg\" or \"unspecified-bg\".

If FRAME is omitted or nil, use the selected frame."
    (when (consp color) (setq color  (car color)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(2)))
        (setq color  (icicle-transform-multi-completion color))))
    (and (not (member color '(unspecified "unspecified-bg" "unspecified-fg")))
         (if (member (framep (or frame  (selected-frame))) '(x w32 ns))
             (xw-color-defined-p color frame)
           (numberp (tty-color-translate color frame))))))

(defcustom icicle-cand-preds-for-color (and (fboundp 'icicle-color-defined-p)
                                            '("icicle-color-defined-p"
                                              "icicle-color-gray-p"
                                              "icicle-color-supported-p"))
  "*Predicates for color candidates.
Useful for `icicle-narrow-candidates-with-predicate'.
Each value is a string that can be successfully read by `read' and for
which `read' returns a predicate (Boolean function) that accepts a
full completion candidate as argument.  Typically, the strings are
predicate names, but they can also be lambda forms.

Depending on the command, the argument to the predicate can take any
form acceptable as a full completion candidate (alist entry with
string car, symbol, etc.)

To make these predicates available for narrowing for one of your
commands, just bind `icicle-color-completing-p' to non-nil in the
command."
  :type '(repeat (function :tag "Predicate to keep candidate")) :group 'Icicles-Matching)

(defcustom icicle-cand-preds-for-face '("icicle-face-bold-p"
                                        "icicle-face-differs-from-default-p"
                                        "icicle-face-inverse-video-p"
                                        "icicle-face-italic-p"
                                        "icicle-face-nontrivial-p"
                                        "icicle-face-underline-p")
  "*Predicates for face-name candidates.
Useful for `icicle-narrow-candidates-with-predicate'.
Each value is a string that can be successfully read by `read' and for
which `read' returns a predicate (Boolean function) that accepts a
full completion candidate as argument.  Typically, the strings are
predicate names, but they can also be lambda forms.

Depending on the command, the argument to the predicate can take any
form acceptable as a full completion candidate (alist entry with
string car, symbol, etc.)

To make these predicates available for narrowing for one of your
commands, just bind `icicle-face-completing-p' to non-nil in the
command."
  :type '(repeat (function :tag "Predicate to keep candidate")) :group 'Icicles-Matching)

(defcustom icicle-cand-preds-for-file (append '("icicle-file-accessible-directory-p"
                                                "icicle-file-compressed-p"
                                                "icicle-file-desktop-p"
                                                "icicle-file-directory-p"
                                                "icicle-file-elc-p"
                                                "icicle-file-executable-p"
                                                "icicle-file-exists-p"
                                                "icicle-file-locked-p"
                                                "icicle-file-name-absolute-p"
                                                "icicle-file-readable-p"
                                                "icicle-file-regular-p"
                                                "icicle-file-remote-p"
                                                "icicle-file-symlink-p"
                                                "icicle-file-writable-p"
                                                "icicle-image-file-p"
                                                "icicle-looks-like-dir-name-p"
                                                "icicle-nondirectory-p")
                                              (and (fboundp 'icicle-recentf-include-p)
                                                   '("icicle-recentf-include-p"
                                                     "icicle-recentf-keep-p"))
                                              (and (fboundp 'icicle-ffap-file-remote-p)
                                                   '("icicle-ffap-file-remote-p"
                                                     "icicle-ffap-url-p")))
  "*Predicates for file-name and directory-name candidates.
Useful for `icicle-narrow-candidates-with-predicate'.
Each value is a string that can be successfully read by `read' and for
which `read' returns a predicate (Boolean function) that accepts a
full completion candidate as argument.  Typically, the strings are
predicate names, but they can also be lambda forms.

Depending on the command, the argument to the predicate can take any
form acceptable as a full completion candidate (alist entry with
string car, symbol, etc.)

To make these predicates available for narrowing for one of your
commands, just bind `icicle-file-completing-p' to non-nil in the
command."
  :type '(repeat (function :tag "Predicate to keep candidate"))
  :group 'Icicles-Matching :group 'Icicles-Files)

(defcustom icicle-cand-preds-for-frame (append '("icicle-frame-invisible-p"
                                                 "icicle-frame-splittable-p"
                                                 "icicle-frame-unsplittable-p")
                                               (and (fboundp 'frcmds-frame-iconified-p) ; `frame-cmds.el'
                                                    '("icicle-frame-iconified-p"))
                                               (and (fboundp 'thumfr-thumbnail-frame-p) ; `thumb-frm.el'
                                                    '("icicle-frame-thumbnail-p")))
  "*Predicates for frame candidates.
Useful for `icicle-narrow-candidates-with-predicate'.
Each value is a string that can be successfully read by `read' and for
which `read' returns a predicate (Boolean function) that accepts a
full completion candidate as argument.  Typically, the strings are
predicate names, but they can also be lambda forms.

Depending on the command, the argument to the predicate can take any
form acceptable as a full completion candidate (alist entry with
string car, symbol, etc.)

To make these predicates available for narrowing for one of your
commands, just bind `icicle-frame-completing-p' to non-nil in the
command."
  :type '(repeat (function :tag "Predicate to keep candidate")) :group 'Icicles-Matching)

(defcustom icicle-cand-preds-for-misc '("icicle-not-special-candidate-p"
                                        "icicle-special-candidate-p")
  "*Predicates for miscellaneous candidates.
Useful for `icicle-narrow-candidates-with-predicate'.
Each value is a string that can be successfully read by `read' and for
which `read' returns a predicate (Boolean function) that accepts a
full completion candidate as argument.  Typically, the strings are
predicate names, but they can also be lambda forms.

Depending on the command, the argument to the predicate can take any
form acceptable as a full completion candidate (alist entry with
string car, symbol, etc.)"
  :type '(repeat (function :tag "Predicate to keep candidate")) :group 'Icicles-Matching)

(defcustom icicle-cand-preds-for-package (and (require 'package nil t)
                                              '("icicle-package-built-in-p"
                                                "icicle-package-disabled-p"
                                                "icicle-package-installed-p"))
  "*Predicates for package candidates.
Useful for `icicle-narrow-candidates-with-predicate'.
Each value is a string that can be successfully read by `read' and for
which `read' returns a predicate (Boolean function) that accepts a
full completion candidate as argument.  Typically, the strings are
predicate names, but they can also be lambda forms.

Depending on the command, the argument to the predicate can take any
form acceptable as a full completion candidate (alist entry with
string car, symbol, etc.)

To make these predicates available for narrowing for one of your
commands, just bind `icicle-package-completing-p' to non-nil in the
command."
  :type '(repeat (function :tag "Predicate to keep candidate")) :group 'Icicles-Matching)

(defcustom icicle-cand-preds-for-symbol '("icicle-binary-option-p"
                                          "icicle-defined-thing-p")
  "*Predicates for symbol-name candidates.
Useful for `icicle-narrow-candidates-with-predicate'.
Each value is a string that can be successfully read by `read' and for
which `read' returns a predicate (Boolean function) that accepts a
full completion candidate as argument.  Typically, the strings are
predicate names, but they can also be lambda forms.

Depending on the command, the argument to the predicate can take any
form acceptable as a full completion candidate (alist entry with
string car, symbol, etc.)

To make these predicates available for narrowing for one of your
commands, just bind `icicle-symbol-completing-p' to non-nil in the
command."
  :type '(repeat (function :tag "Predicate to keep candidate")) :group 'Icicles-Matching)

(defcustom icicle-cand-preds-for-variable (append '("icicle-custom-variable-p")
                                                  (and (fboundp 'icicle-special-variable-p)
                                                       '("icicle-special-variable-p")))
  "*Predicates for variable-name candidates.
Useful for `icicle-narrow-candidates-with-predicate'.
Each value is a string that can be successfully read by `read' and for
which `read' returns a predicate (Boolean function) that accepts a
full completion candidate as argument.  Typically, the strings are
predicate names, but they can also be lambda forms.

Depending on the command, the argument to the predicate can take any
form acceptable as a full completion candidate (alist entry with
string car, symbol, etc.)

To make these predicates available for narrowing for one of your
commands, just bind `icicle-variable-completing-p' to non-nil in the
command."
  :type '(repeat (function :tag "Predicate to keep candidate")) :group 'Icicles-Matching)

(defcustom icicle-cand-preds-for-window (append '("icicle-window-dedicated-p")
                                                (and (fboundp 'icicle-window-at-top-p)
                                                     '("icicle-window-at-bottom-p"
                                                       "icicle-window-at-left-p"
                                                       "icicle-window-at-right-p"
                                                       "icicle-window-at-top-p"
                                                       "icicle-window-deletable-p"
                                                       "icicle-window-invisible-p")))
  "*Predicates for window candidates.
Useful for `icicle-narrow-candidates-with-predicate'.
Each value is a string that can be successfully read by `read' and for
which `read' returns a predicate (Boolean function) that accepts a
full completion candidate as argument.  Typically, the strings are
predicate names, but they can also be lambda forms.

Depending on the command, the argument to the predicate can take any
form acceptable as a full completion candidate (alist entry with
string car, symbol, etc.)

To make these predicates available for narrowing for one of your
commands, just bind `icicle-window-completing-p' to non-nil in the
command."
  :type '(repeat (function :tag "Predicate to keep candidate")) :group 'Icicles-Matching)

;; Must come after all `icicle-cand-preds-for-*' options.
(defcustom icicle-cand-preds-all (append icicle-cand-preds-for-bookmark
                                         icicle-cand-preds-for-buffer
                                         icicle-cand-preds-for-color
                                         icicle-cand-preds-for-face
                                         icicle-cand-preds-for-file
                                         icicle-cand-preds-for-frame
                                         icicle-cand-preds-for-misc
                                         icicle-cand-preds-for-package
                                         icicle-cand-preds-for-symbol
                                         icicle-cand-preds-for-variable
                                         icicle-cand-preds-for-window)
  "*List of all predicates for completion candidates.
Useful for `icicle-narrow-candidates-with-predicate'.
Each value is a string that can be successfully read by `read' and for
which `read' returns a predicate (Boolean function) that accepts a
full completion candidate as argument.  Typically, the strings are
predicate names, but they can also be lambda forms.

Depending on the command, the argument to the predicate can take any
form acceptable as a full completion candidate (alist entry with
string car, symbol, etc.)"
  :type '(repeat (function :tag "Predicate To Accept Candidate")) :group 'Icicles-Matching)

;; Must be before `icicle-change-region-background-flag'.
(defcustom icicle-mark-position-in-candidate 'input-end
  "*Position of mark when you cycle through completion candidates.
This is the mark position in the minibuffer.
Possible values are those for `icicle-point-position-in-candidate'."
  :type '(choice
          (const :tag "Leave mark at the beginning of the minibuffer input"  input-start)
          (const :tag "Leave mark at the end of the minibuffer input"        input-end)
          (const :tag "Leave mark at the beginning of the completion root"   root-start)
          (const :tag "Leave mark at the end of the completion root"         root-end))
  :group 'Icicles-Minibuffer-Display)

;; Must be before `icicle-change-region-background-flag'.
(defcustom icicle-point-position-in-candidate 'root-end
  "*Position of cursor when you cycle through completion candidates.
This is the cursor position in the minibuffer.
Possible values are:
 `input-start': beginning of the minibuffer input
 `input-end':   end of the minibuffer input
 `root-start':  beginning of the completion root
 `root-end':    end of the completion root
When input is expected to be a file name, `input-start' is just after
the directory, which is added automatically during completion cycling.
See also `icicle-mark-position-in-candidate'."
  :type '(choice
          (const :tag "Leave cursor at the beginning of the minibuffer input"  input-start)
          (const :tag "Leave cursor at the end of the minibuffer input"        input-end)
          (const :tag "Leave cursor at the beginning of the completion root"   root-start)
          (const :tag "Leave cursor at the end of the completion root"         root-end))
  :group 'Icicles-Minibuffer-Display)

(defcustom icicle-change-region-background-flag
  (not (eq icicle-point-position-in-candidate icicle-mark-position-in-candidate))
  "*Non-nil means use color `icicle-region-background' during input.
See `icicle-region-background'.  If you load library `hexrgb.el'
before Icicles, then `icicle-region-background' will be a slightly
different hue from your normal background color.  This makes
minibuffer input easier to read than if your normal `region' face were
used.  This has an effect only during minibuffer input.  A non-nil
value for this option is particularly useful if you use
delete-selection mode.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Minibuffer-Display)

(defcustom icicle-change-sort-order-completion 7
  "*Whether `icicle-change-sort-order' uses completion, by default.
The behavior of `icicle-change-sort-order' with no prefix arg depends
on the option value as follows:

 - nil means cycle to the next order; do not use completion to choose
   the new order.

 - An integer means use completion if there are currently more than
   that number of sort orders to choose from.  Otherwise, cycle next.

 - Any other non-nil value means use completion."
  :type '(choice
          (integer :tag "Complete if more than this number of sort orders" :value 7)
          (const   :tag "Complete always"                                  :value t)
          (const   :tag "Cycle always"                                     :value nil))
  :group 'Icicles-Completions-Display :group 'Icicles-Matching)

(defcustom icicle-C-l-uses-completion-flag nil
  "*Non-nil means \\<minibuffer-local-completion-map>\
`\\[icicle-retrieve-previous-input]' uses completion for choosing completion history
entries, by default.  Otherwise, it cycles among the possible previous
inputs.  You can override the behavior by using `C-u' with `\\[icicle-retrieve-previous-input]'.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Minibuffer-Display :group 'Icicles-Matching)

(defcustom icicle-cmpl-include-cdabbrev-flag nil
  "*Non-nil means includes `CDABBREV' candidates for `icicle-complete'.
This applies only to Icicles minibuffer completion during command
`icicle-complete', so it applies only when`dynamic-completion-mode' is
on.  That mode is defined in Emacs library `completion.el'.

If nil then Icicles completion for `icicle-complete' includes only
completions from the completions database.  If non-nil then it
includes also completions found dynamically from the currently
available windows.  These candidates are highlighted using face
`icicle-special-candidate' so you can distinguish them.

This is the so-called `CDABBREV' completion method defined in
`completion.el'.  It is similar to how `dabbrev' finds candidates but
with these differences:
* It is sometimes faster, since it does not use regexps.  It searches
  backwards looking for names that start with the text before point.
* Case-sensitivity is handled as for other `completion.el' completion.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Matching)

(defcustom icicle-cmpl-max-candidates-to-cycle 7
  "*Max number of completions to cycle through, or nil for no maximum.
This has an effect only when `dynamic-completion-mode' is on.  That
mode is defined in Emacs library `completion.el'.

If there are more candidate completions than this for the name prefix
before point then use Icicles minibuffer completion.  Cycling here
refers to `completions.el' cycling, not to Icicles candidate cycling.
Thus:
* A value of zero (0) means to always use Icicles completion.
* A value of nil means to never automatically use Icicles
  completion (you can use `C-u C-u' to get Icicles completion)."
  :type '(choice
          (restricted-sexp
           :tag "Use Icicles when there are more completions than this"
           :match-alternatives ((lambda (x) (and (integerp x)  (> x 0)))) :value ignore)
          (const :tag "No limit - use Icicles only via `C-u C-u'" nil))
  :group 'Icicles-Miscellaneous)

;; Replace this list by your favorite color themes. Each must be the name of a defined function.
;; By default, this includes all color themes defined globally (variable `color-themes').
;;
;; NOTE: We need the `condition-case' because of a BUG in `directory-files' for Emacs 20.
;; Bug reported to `color-theme.el' maintainer 2009-11-22.  The problem is that the default value
;; of `color-theme-libraries' concats `file-name-directory', which ends in `/', with `/themes',
;; not with `themes'.  So the result is `...//themes'.  That is tolerated by Emacs 21+
;; `directory-files', but not for Emacs 20.  Until this `color-theme.el' bug is fixed, Emacs 20
;; users will need to manually load `color-theme-libraries.el'.
(defcustom icicle-color-themes ()
  "*List of color themes to cycle through using `M-x icicle-color-theme'.
Note: Starting with Color Theme version 6.6.0, you will need to put
library `color-theme-library.el', as well as library `color-theme.el',
in your `load-path'."
  :type 'hook :group 'Icicles-Miscellaneous)

(defcustom icicle-comint-dynamic-complete-replacements
  '((shell-command-completion            'icicle-shell-dynamic-complete-command) ; Emacs 24+
    (shell-dynamic-complete-command      'icicle-shell-dynamic-complete-command) ; Emacs 20-23
    (shell-dynamic-complete-environment-variable
     'icicle-shell-dynamic-complete-environment-variable)
    (shell-dynamic-complete-filename     'icicle-shell-dynamic-complete-filename)
    ((pcomplete-completions-at-point
      comint-filename-completion
      shell-filename-completion)
     (lambda () (and (comint-match-partial-filename)  #'icicle-comint-dynamic-complete-filename)))
    )
  "*List of function replacements for `comint-dynamic-complete-functions'.
Instead of using `comint-dynamic-complete-functions' as is, command
`icicle-comint-dynamic-complete' uses a modified version of that list
according to the value of this option.

You can use this option to provide Icicles completion for various
modes that inherit from Comint mode or otherwise use
`comint-dynamic-complete'.

Each option list element is itself a list of two elements, (OLD NEW).
OLD specifies a function in `comint-dynamic-complete-functions'.  NEW
is a sexp that evaluates to an Icicles completion function to use
instead of OLD.

If OLD is a symbol then the value of NEW, in effect, replaces OLD in
`comint-dynamic-complete-functions'.

If OLD is a list then the value of NEW is inserted in
`comint-dynamic-complete-functions' before whichever element of OLD
occurs first in `comint-dynamic-complete-functions'.  That ensures
that NEW is invoked before OLD when attempting completion.  OLD is
invoked only if NEW cannot find a completion.

For example, this list element says to replace completion function
`foo' by completion function `my-foo': (foo 'my-foo).  And this one
says to try completing with function `mine' before `foo' or `bar':
\((foo bar) 'mine)."
  :type '(repeat (list
                  (choice
                   (symbol :tag "OLD to replace")
                   (repeat :tag "OLD to insert before"
                    (sexp :value pcomplete-completions-at-point)))
                  (sexp :tag "NEW (must eval to completion function)")))
  :group 'Icicles-Miscellaneous)

(defcustom icicle-command-abbrev-alist ()
  "*Alist of command abbreviations and commands, with frequency of use.
Each element has the form (COMMAND ABBREV N), where ABBREV is an
abbreviation of COMMAND and N is the number of times COMMAND has been
invoked via ABBREV.  Both COMMAND and ABBREV are symbols."
  :type '(alist
          :key-type (symbol :tag "Command")
          :value-type (list (symbol :tag "Abbreviation") (integer :tag "Times Used")))
  :group 'Icicles-Matching)

(defcustom icicle-command-abbrev-match-all-parts-flag nil
  "*Non-nil means `icicle-command-abbrev' matches each command-name part.
Otherwise, an abbrev need match only a prefix of the command name.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Matching)

(defcustom icicle-command-abbrev-priority-flag nil
  "*nil means commands take precedence over abbreviations for `\\<icicle-mode-map>\
\\[icicle-command-abbrev]'.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Matching)

(defcustom icicle-complete-key-anyway-flag nil
  "*Non-nil means bind `S-TAB' for key completion even if already
bound.  If nil, then each of the keys in `icicle-key-complete-keys' is
bound to `icicle-complete-keys' in each keymap of
`icicle-keymaps-for-key-completion' only if `S-TAB' is not already
bound in the keymap.

Note: the keys in `icicle-key-complete-keys' are always bound to
`icicle-complete-keys' in `icicle-mode-map'.  This option affects only
the binding of those keys in `icicle-keymaps-for-key-completion'.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Key-Completion :group 'Icicles-Key-Bindings)

(when (fboundp 'map-keymap)             ; Emacs 22+.
  (defcustom icicle-complete-keys-ignored-prefix-keys '([menu-bar icicles])
    "*Prefix keys for `icicle-complete-keys' to ignore.
The value is a list of key sequences assumed to be prefix keys.

If `icicle-complete-keys' is bound to a key on one of these prefix
keys then it does not complete that prefix key.  Instead, it acts as
though invoked at the top level.  This lets you bind key completion to
a key sequence that starts with a prefix key, so you can invoke it
from there just as if you had invoked it at top level.

Here is an example of adding two such prefix keys using Customize:

 Click [INS] to insert a new, empty entry.
 Type `M-s' in the edit field.

 Click [INS] to insert another new, empty entry.
 Type `<menu-bar> <edit>' in the edit field.

 Use the `State' menu to update the option value to reflect the edit.

This adds these two prefix keys to the option value:

 * [134217843], which is represented externally by `M-s'
 * [menu-bar edit], which represents the `Edit' menu-bar menu.

This example means that if you bind `icicle-complete-keys' to a key on
either of these prefix keys then invoking that key sequence will
complete keys at the top level - it will not complete that prefix key."
    :type '(repeat (key-sequence :tag "Prefix Key" :value [ignore])) :group 'Icicles-Key-Bindings)

  (defcustom icicle-complete-keys-self-insert-ranges ()
    "*Non-nil means `icicle-complete-keys' includes self-inserting keys.
That means keys bound to `self-insert-command'.

For Emacs 22, this is effectively Boolean: any non-nil value allows
all self-inserting keys as candidates.

In Emacs 23+, there are thousands of self-inserting keys, so it is not
practical to allow all as candidates.  Instead, a non-nil value is a
list of character ranges of the form (MIN . MAX).  Characters in the
inclusive range MIN through MAX are possible key-completion
candidates.

For Emacs 23+, if you use a non-nil value then use only small ranges
for better performance, e.g., `((0 . 687))' covers Latin characters.

In general, leave the value as nil.  Use `C-x 8 RET' in Emacs 23+ to
insert characters by completing against their Unicode names.  With
Icicles, you can see the characters themselves in `*Completions*'
whether you use key completion or `C-x 8 RET', but in both cases you
complete against the character name.

`C-x 8 RET' is bound by default in Emacs to command `insert-char'
\(called `ucs-insert' prior to Emacs 24).  If you use my library
`ucs-cmds.el' then you might want to remap that command to command
`ucsc-insert', which is an enhancement.

For reference, below are the ranges supported by `C-x 8 RET' (Emacs
23+).  But unless you have a very powerful computer, choose only only
one or two small ranges of characters you actually might use.

BMP ranges:
 (0 . 13311)       = (#x0000 . #x33FF)
 (19904 . 19967)   = (#x4DC0 . #x4DFF)
 (40960 . 55295)   = (#xA000 . #x0D7FF)
 (64256 . 65533)   = (#xFB00 . #xFFFD)

Upper ranges:
 (65536 . 79103)   = (#x10000 . #x134FF)
 (118784 . 131071) = (#x1D000 . #x1FFFF)
 (917504 . 918015) = (#xE0000 . #xE01FF)"
    :type '(alist :key-type integer :value-type integer) :group 'Icicles-Key-Completion))

(defcustom icicle-complete-keys-separator "  =  "
  "Characters that separate keys from their commands, in key completions."
  :type 'string :group 'Icicles-Key-Completion)

(defcustom icicle-completing-read+insert-keys '([(control meta shift ?c)]) ; `C-M-S-c'
  "*Key sequences to invoke `icicle-completing-read+insert'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Such a key has no effect unless
`icicle-completing-read+insert-candidates' is non-nil."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-completion-history-max-length (if icicle-C-l-uses-completion-flag 1000 100)
  "*Maximum number of inputs to save in the completion history.
This is the history that you access using \\<minibuffer-local-completion-map>\
`\\[icicle-retrieve-previous-input]' and `\\[icicle-retrieve-next-input]'.

If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'integer :group 'Icicles-Miscellaneous)

(defvar icicle-delete-candidate-object nil
  "Defines deletion action for command `icicle-delete-candidate-object'.
The value can be a function or a symbol bound to an alist.

If the value is a function, then the function is called on the current
completion candidate (a string) to delete some corresponding object.

If the value is a symbol (variable) bound to an alist, then
`icicle-delete-current-candidate-object' is called to delete the
corresponding object from that alist.  If the variable is also a user
option, then the option is saved after the candidate is deleted.

Note that if the value is a variable and you use multi-completion
candidates during completion, then the alist value of the variable
must itself contain multi-completions.  Otherwise, no candidate will
be deleted, because `icicle-delete-current-candidate-object' deletes
the full candidate object.")

(defcustom icicle-completion-key-bindings
  `((,(icicle-kbd "M-return")  icicle-candidate-read-fn-invoke t)                     ;`M-RET'
                                                                                      ; (`M-return')
    (,(icicle-kbd "C-M-m")     icicle-candidate-read-fn-invoke t)                     ;`M-RET'
                                                                                      ; (`ESC RET')
    (,(icicle-kbd "C-S-return") icicle-candidate-alt-action t)                        ; `C-S-RET'
                                                                                      ; (`C-S-return')
    (,(icicle-kbd "delete")    icicle-remove-candidate t)                             ; `delete'
    (,(icicle-kbd "S-delete")  icicle-delete-candidate-object t)                      ; `S-delete'
    (,(icicle-kbd "C-w")       icicle-kill-region t)                                  ; `C-w'
    (,(icicle-kbd "C-!")       icicle-all-candidates-action t)                        ; `C-!'
    (,(icicle-kbd "C-|")       icicle-all-candidates-alt-action t)                    ; `C-|'
    (,(icicle-kbd "M-!")       icicle-all-candidates-list-action t)                   ; `M-!'
    (,(icicle-kbd "M-|")       icicle-all-candidates-list-alt-action t)               ; `M-|'
    (,(icicle-kbd "M-h")       icicle-history t)                                      ; `M-h'
    (,(icicle-kbd "M-pause")   icicle-keep-only-past-inputs t) ; `M-pause'
    (,(icicle-kbd "C-pause")   icicle-toggle-highlight-historical-candidates t)       ; `C-pause'
    (,(icicle-kbd "S-pause")   icicle-toggle-highlight-saved-candidates t)            ; `S-pause'
    (,(icicle-kbd "C-S-pause") icicle-toggle-WYSIWYG-Completions t)                   ; `C-S-pause'
    ;;$$$$$$  (,(icicle-kbd "C-M-pause") 'icicle-other-history) ; `C-M-pause'
    (,(icicle-kbd "C-insert")  icicle-switch-to-Completions-buf t)                    ; `C-insert'
    (,(icicle-kbd "insert")    icicle-save/unsave-candidate t)                        ; `insert'

    ;; In Emacs 22+, local is parent of local-completion
    ;; Note: `setup-keys.el' binds `C-o' to `1on1-fit-minibuffer-frame' if defined.
    (,(icicle-kbd "C-a")     icicle-beginning-of-line+
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-a'
    (,(icicle-kbd "C-e")     icicle-end-of-line+
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-e'
    (,(icicle-kbd "C-M-v")   icicle-scroll-forward
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-M-v'
    (,(icicle-kbd "C-M-S-v") icicle-scroll-backward
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-M-V'
                                                                                      ; (`C-M-S-v')
    (,(icicle-kbd "C-=")     icicle-insert-string-from-variable
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-='
    ;; Replaces `kill-sentence':
    (,(icicle-kbd "M-k")     icicle-erase-minibuffer-or-history-element
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-k'
    (,(icicle-kbd "M-K")     icicle-clear-current-history
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-K'
    (,(icicle-kbd "M-o")     icicle-insert-history-element
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-o'
    (,(icicle-kbd "M-.")     icicle-insert-string-at-point
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-.'
    (,(icicle-kbd "C-x C-f") icicle-resolve-file-name
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-x C-f'
    (,(icicle-kbd "M-:")     icicle-pp-eval-expression-in-minibuffer
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-:'
    (,(icicle-kbd "C-M-y") icicle-yank-secondary
     (and (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))
      (fboundp 'icicle-yank-secondary)))                                              ; `C-M-y'
    (,(icicle-kbd "C-M-pause")  icicle-other-history
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-M-pause'
    (,(icicle-kbd "M-S-backspace") icicle-erase-minibuffer
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-S-backspace'
    (,(icicle-kbd "M-S-delete") icicle-erase-minibuffer
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-S-delete'

    ;; Need `C-g', even if `minibuffer-local-completion-map' inherits from `minibuffer-local-map'.
    (,(icicle-kbd "C-g")       icicle-abort-recursive-edit t)                         ; `C-g'
    (,(icicle-kbd "M-q")       icicle-dispatch-M-q t)                                 ; `M-q'
    (,(icicle-kbd "C-l")       icicle-retrieve-previous-input t)                      ; `C-l'
    (,(icicle-kbd "C-S-l")     icicle-retrieve-next-input t)                          ; `C-L' (`C-S-l')
    (,(icicle-kbd "M-$")       icicle-candidate-set-truncate t)                       ; `M-$'
    (,(icicle-kbd "C-~")       icicle-candidate-set-complement t)                     ; `C-~'
    (,(icicle-kbd "C--")       icicle-candidate-set-difference t)                     ; `C--'
    (,(icicle-kbd "C-+")       icicle-candidate-set-union t)                          ; `C-+'
    (,(icicle-kbd "C-*")       icicle-candidate-set-intersection t)                   ; `C-*'
    (,(icicle-kbd "C->")       icicle-candidate-set-save-more t)                      ; `C->'
    (,(icicle-kbd "C-M->")     icicle-candidate-set-save t)                           ; `C-M->'
    (,(icicle-kbd "C-S-<tab>") icicle-toggle-completion-mode-keys t)                  ; `C-S-<tab>'
    (,(icicle-kbd "C-(")       icicle-next-TAB-completion-method t)                   ; `C-('
    (,(icicle-kbd "C-M-(")     icicle-next-completion-style-set
     (fboundp 'icicle-next-completion-style-set))                                     ; `C-M-('
    (,(icicle-kbd "M-(")       icicle-next-S-TAB-completion-method t)                 ; `M-('
    (,(icicle-kbd "C-)")       icicle-candidate-set-save-more-selected t)             ; `C-)'
    (,(icicle-kbd "C-M-)")     icicle-candidate-set-save-selected t)                  ; `C-M-)'
    (,(icicle-kbd "C-M-<")     icicle-candidate-set-retrieve t)                       ; `C-M-<'
    (,(icicle-kbd "C-M-}")     icicle-candidate-set-save-to-variable t)               ; `C-M-}'
    (,(icicle-kbd "C-M-{")     icicle-candidate-set-retrieve-from-variable t)         ; `C-M-{'
    (,(icicle-kbd "C-}")       icicle-candidate-set-save-persistently t)              ; `C-}'
    (,(icicle-kbd "C-{")       icicle-candidate-set-retrieve-persistent t)            ; `C-{'
    (,(icicle-kbd "C-%")       icicle-candidate-set-swap t)                           ; `C-%'
    (,(icicle-kbd "M-%")       icicle-regexp-quote-input t)                           ; `M-%'
    (,(icicle-kbd "C-:")       icicle-candidate-set-define t)                         ; `C-:'
    (,(icicle-kbd "C-M-j")     icicle-insert-list-join-string t)                      ; `C-M-j'
    (,(icicle-kbd "C-,")       icicle-change-sort-order t)                            ; `C-,'
    (,(icicle-kbd "C-M-\;")     icicle-toggle-ignoring-comments t)                    ; `C-M-;'
    (,(icicle-kbd "C-`")       icicle-toggle-regexp-quote t)                          ; `C-`'
    (,(icicle-kbd "C-M-.")     icicle-toggle-dot t)                                   ; `C-M-.'
    (,(icicle-kbd "C-M-`")     icicle-toggle-literal-replacement t)                   ; `C-M-`'
    (,(icicle-kbd "C-<")       icicle-candidate-set-retrieve-more t)                  ; `C-<'
    (,(icicle-kbd "C-M-_")     icicle-toggle-proxy-candidates t)                      ; `C-M-_'
    (,(icicle-kbd "C-$")       icicle-toggle-transforming t)                          ; `C-$'
;;;     ;; In Emacs 22+, local is parent of local-completion
;;;     ;; $$$$$$ Keep `C-?' also for a while, undocumented, for backward compatibility only.
;;;     (,(icicle-kbd "C-?")     icicle-minibuffer-help
;;;      (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-?'
    (,(icicle-kbd "M-?")     icicle-minibuffer-help
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-?'
    (,(icicle-kbd "C-.")       icicle-dispatch-C-. t)                                 ; `C-.'
    (,(icicle-kbd "C-#")       icicle-cycle-incremental-completion t)                 ; `C-#'
    (,(icicle-kbd "C-M-#")     icicle-toggle-icomplete-mode t)                        ; `C-M-#'
    (,(icicle-kbd "C-\"")      icicle-toggle-expand-to-common-match t)                ; `C-"'
    (,(icicle-kbd "C-M-\"")    icicle-cycle-expand-to-common-match t)                 ; `C-M-"'
    (,(icicle-kbd "M-\;")      icicle-toggle-search-replace-common-match t)           ; `M-;'
    (,(icicle-kbd "C-^")       icicle-dispatch-C-^ t)                                 ; `C-^'
    (,(icicle-kbd "C-M-^")     icicle-toggle-completions-format t)                    ; `C-M-^'
    (,(icicle-kbd "C-S-a")     icicle-toggle-case-sensitivity t)                      ; `C-A' (`C-S-a')
    (,(icicle-kbd "M-~")       icicle-toggle-~-for-home-dir t)                        ; `M-~'
    (,(icicle-kbd "C-M-~")     icicle-toggle-search-complementing-domain t)           ; `C-M-~'
    (,(icicle-kbd "M-g")       icicle-toggle-C-for-actions t)                         ; `M-g'
    (,(icicle-kbd "M-,")       icicle-dispatch-M-comma t)                             ; `M-,'
    (,(icicle-kbd "C-M-,")     icicle-toggle-alternative-sorting t)                   ; `C-M-,'
    (,(icicle-kbd "C-M-+")     icicle-plus-saved-sort t)                              ; `C-M-+'
    (,(icicle-kbd "M-+")       icicle-widen-candidates t)                             ; `M-+'
    (,(icicle-kbd "M-*")       icicle-narrow-candidates t)                            ; `M-*'
    (,(icicle-kbd "M-&")       icicle-narrow-candidates-with-predicate t)             ; `M-&'
    (,(icicle-kbd "M-_")       icicle-dispatch-M-_ t)                                 ; `M-_'
    (,(icicle-kbd "C-M-&")     icicle-save-predicate-to-variable t)                   ; `C-M-&'
    (,(icicle-kbd "S-SPC")     icicle-apropos-complete-and-narrow t)                  ; `S-SPC'
    (,(icicle-kbd "S-return")  icicle-apropos-complete-and-exit t)                    ; `S-return'
    (,(icicle-kbd "S-backspace") icicle-apropos-complete-and-widen t)                 ; `S-backspace'
    (,(icicle-kbd "C-v")       icicle-scroll-Completions-forward t)                   ; `C-v'
    (,(icicle-kbd "M-v")       icicle-scroll-Completions-backward t)                  ; `M-v'
    (,(icicle-kbd ".")         icicle-insert-dot-command t)                           ; `.'
    (,(icicle-kbd "M-m")       icicle-toggle-show-multi-completion t)                 ; `M-m'
    (,(icicle-kbd "M-r")       icicle-roundup t)                                      ; `M-r'
    (,(icicle-kbd "C-x .")     icicle-dispatch-C-x. t)                                ; `C-x .'
    (,(icicle-kbd "C-x :")     icicle-toggle-network-drives-as-remote t)              ; `C-x :'
    (,(icicle-kbd "C-x /")     icicle-toggle-expand-directory t)                      ; `C-x /'
    (,(icicle-kbd "C-x C-a")   icicle-toggle-annotation t)                            ; `C-x C-a'
    (,(icicle-kbd "C-x C-0")   icicle-recomplete-from-original-domain t)              ; `C-x C-0'
    (,(icicle-kbd "C-x C-M-l") icicle-display-candidates-in-Completions t)            ; `C-x C-M-l'
    (,(icicle-kbd "C-x t")     icicle-cycle-image-file-thumbnail                      ; `C-x t'
     (fboundp 'icicle-cycle-image-file-thumbnail))
    (,(icicle-kbd "C-x w")     icicle-doremi-candidate-width-factor+                  ; `C-x w'
     (fboundp 'doremi))
    (,(icicle-kbd "C-x |")     icicle-doremi-inter-candidates-min-spaces+             ; `C-x |'
     (fboundp 'doremi))
    (,(icicle-kbd "C-x #")     icicle-doremi-increment-max-candidates+                ; `C-x #'
     (fboundp 'doremi))
    (,(icicle-kbd "C-x -")     icicle-doremi-zoom-Completions+                        ; `C-x -'
     (and (fboundp 'doremi)
      (fboundp 'text-scale-increase)))
    (,(icicle-kbd "C-x 1")     icicle-doremi-increment-swank-timeout+
     (and (fboundp 'doremi)  (eq (icicle-current-TAB-method) 'swank)))
    ;; NO - NEED TO DO THE SWANK PART AT RUNTIME, in icicles-mode.el
    (,(icicle-kbd "C-x 2") icicle-doremi-increment-swank-prefix-length+
     (and (fboundp 'doremi)  (eq (icicle-current-TAB-method) 'swank)))
    (,(icicle-kbd "C-x C-M->") bmkp-set-icicle-search-hits-bookmark                   ; `C-x C-M->'
      (fboundp 'bmkp-set-icicle-search-hits-bookmark))
    (,(icicle-kbd "C-x C-M-<") bmkp-retrieve-icicle-search-hits                       ; `C-x C-M-<'
      (fboundp 'bmkp-set-icicle-search-hits-bookmark))
    (,(icicle-kbd "C-x C-<")   bmkp-retrieve-more-icicle-search-hits                  ; `C-x C-<'
      (fboundp 'bmkp-set-icicle-search-hits-bookmark))
    ;; `minibuffer-completion-help' got wiped out by remap for self-insert.
    (,(icicle-kbd "?")         icicle-self-insert t)                                  ; `?
    (,(icicle-kbd "SPC")       icicle-self-insert t)                                  ; " "
    ;; In Emacs 22+, local is parent of local-completion
    (,(icicle-kbd "C-j")     icicle-insert-newline-in-minibuffer
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-j
    )
  "*List of minibuffer key bindings during completion in Icicle mode.
The option value has the same form as that of option
`icicle-top-level-key-bindings' (which see).
Each list element is of custom type `icicle-key-definition' and has
the form (KEY COMMAND CONDITION).

If you customize this option then you must exit and re-enter Icicle
mode to ensure that the change takes effect.  This is really necessary
only if your changes would undefine a key.

For this option to have an effect upon startup, it must be set before
you enter Icicle mode.  This means that you must ensure that the code
that sets it is invoked before you enter Icicle mode.  If you use
Customize to change this option, then ensure that the code inserted by
Customize into your `user-init-file' or your `custom-file' is invoked
before you enter Icicle mode."
  :type (if (> emacs-major-version 21)
            '(repeat icicle-key-definition)
          '(repeat
            (list
             (choice
              (restricted-sexp :tag "Key"
               :match-alternatives ((lambda (x) (or (stringp x)  (vectorp x)))) :value [ignore])
              (restricted-sexp :tag "Command to remap"
               ;; Use `symbolp' instead of `commandp', in case the library defining the
               ;; command is not loaded.
               :match-alternatives (symbolp) :value ignore))
             ;; Use `symbolp' instead of `commandp'...
             (restricted-sexp :tag "Command"
              :match-alternatives (symbolp) :value ignore)
             (sexp :tag "Condition"))))
  :group 'Icicles-Key-Bindings)

(defcustom icicle-completion-list-key-bindings
  `(
;;;    (,(icicle-kbd "C-?")                icicle-minibuffer-help t)                         ; `C-?'
    (,(icicle-kbd "M-?")                icicle-minibuffer-help t)                         ; `M-?'
    (,(icicle-kbd "q")                  icicle-abort-recursive-edit t)                    ; `q'
    (,(icicle-kbd "C-insert")           icicle-insert-completion t)                       ; `C-insert'
    (,(icicle-kbd "down")               icicle-next-line t)                               ; `down'
    (,(icicle-kbd "up")                 icicle-previous-line t)                           ; `up'
    (,(icicle-kbd "left")               icicle-move-to-previous-completion t)             ; `left'
    (,(icicle-kbd "backtab")            icicle-move-to-previous-completion                ; `S-TAB'
     (> emacs-major-version 23))
    (,(icicle-kbd "S-tab")              icicle-move-to-previous-completion                ; `S-TAB'
     (< emacs-major-version 24))
    (,(icicle-kbd "S-iso-lefttab")      icicle-move-to-previous-completion                ; `S-TAB'
     (< emacs-major-version 24))
    (,(icicle-kbd "right")              icicle-move-to-next-completion t)                 ; `right'
    (,(icicle-kbd "C-i")                icicle-move-to-next-completion t)                 ; `TAB'
    (,(icicle-kbd "tab")                icicle-move-to-next-completion t)                 ; `TAB'
    ,@(and (boundp 'mouse-wheel-down-event) ;  Emacs 22+
           `((,(vector mouse-wheel-down-event)   icicle-scroll-Completions-backward t)    ; `wheel-down'
             (,(vector mouse-wheel-up-event)   icicle-scroll-Completions-forward t)))     ; `wheel-up'
    (,(icicle-kbd "S-down-mouse-2")     icicle-mouse-remove-candidate t)                  ; `S-mouse-2'
    (,(icicle-kbd "S-mouse-2")          ignore t)
    (,(icicle-kbd "C-S-down-mouse-2")   icicle-mouse-candidate-alt-action t)              ; `C-S-mouse-2'
    (,(icicle-kbd "C-S-mouse-2")        ignore t)
    (,(icicle-kbd "C-down-mouse-2")     icicle-mouse-candidate-action t)                  ; `C-mouse-2'
    (,(icicle-kbd "C-mouse-2")          ignore t)
    (,(icicle-kbd "C-M-down-mouse-2")   icicle-mouse-help-on-candidate t)                 ; `C-M-mouse-2'
    (,(icicle-kbd "C-M-mouse-2")        ignore t)
    (,(icicle-kbd "M-S-down-mouse-2")   icicle-mouse-save/unsave-candidate t)             ; `M-S-mouse-2'
    (,(icicle-kbd "M-S-mouse-2")        ignore t)
    (,(icicle-kbd "M-down-mouse-2")     icicle-mouse-candidate-read-fn-invoke t)          ; `M-mouse-2'
    (,(icicle-kbd "M-mouse-2")          ignore t)
    (,(icicle-kbd "C-down-mouse-3")     icicle-Completions-mouse-3-menu t)                ; `C-mouse-3'
    (,(icicle-kbd "C-mouse-3")          ignore t)
    (,(icicle-kbd "M-down-mouse-3")     icicle-mouse-candidate-set-save-more t)           ; `M-mouse-3'
    (,(icicle-kbd "M-mouse-3")          ignore t)
    (,(icicle-kbd "M-S-down-mouse-3")   icicle-mouse-candidate-set-save t)                ; `M-S-mouse-3'
    (,(icicle-kbd "M-S-mouse-3")        ignore t)
    (,(icicle-kbd "mouse-3")            icicle-mouse-save-then-kill t)                    ; `mouse-3'
    (,(icicle-kbd "C->")                icicle-candidate-set-save-more t)                 ; `C->'
    (,(icicle-kbd "C-M->")              icicle-candidate-set-save t)                      ; `C-M->'
    (,(icicle-kbd "C-)")                icicle-candidate-set-save-more-selected t)        ; `C-)'
    (,(icicle-kbd "C-M-)")              icicle-candidate-set-save-selected t)             ; `C-M-)'
    (,(icicle-kbd "C-M-<")              icicle-candidate-set-retrieve t)                  ; `C-M-<'
    (,(icicle-kbd "C-l")                icicle-retrieve-previous-input t)                 ; `C-l'
    (,(icicle-kbd "C-a")                icicle-beginning-of-line+ t)                      ; `C-a'
    (,(icicle-kbd "C-e")                icicle-end-of-line+ t)                            ; `C-e'
    )
  "*List of key bindings in `*Completions*' for use in Icicle mode.
The option value has the same form as that of option
`icicle-top-level-key-bindings' (which see).
Each list element is of custom type `icicle-key-definition' and has
the form (KEY COMMAND CONDITION).

If you customize this option then you must exit and re-enter Icicle
mode to ensure that the change takes effect.  This is really necessary
only if your changes would undefine a key.

For this option to have an effect upon startup, it must be set before
you enter Icicle mode.  This means that you must ensure that the code
that sets it is invoked before you enter Icicle mode.  If you use
Customize to change this option, then ensure that the code inserted by
Customize into your `user-init-file' or your `custom-file' is invoked
before you enter Icicle mode."
  :type (if (> emacs-major-version 21)
            '(repeat icicle-key-definition)
          '(repeat
            (list
             (choice
              (restricted-sexp :tag "Key"
               :match-alternatives ((lambda (x) (or (stringp x)  (vectorp x)))) :value [ignore])
              (restricted-sexp :tag "Command to remap"
               ;; Use `symbolp' instead of `commandp', in case the library defining the
               ;; command is not loaded.
               :match-alternatives (symbolp) :value ignore))
             ;; Use `symbolp' instead of `commandp'...
             (restricted-sexp :tag "Command"
              :match-alternatives (symbolp) :value ignore)
             (sexp :tag "Condition"))))
  :group 'Icicles-Key-Bindings)

(when (boundp 'completion-styles)       ; Emacs 23+
  (defcustom icicle-completion-style-sets (cons (copy-sequence completion-styles)
                                                (mapcar #'list (mapcar #'car completion-styles-alist)))
    "Possible `completion-styles' values for when `TAB' completion method is `vanilla'."
    :type '(repeat (repeat (symbol :tag "Completion style")))
    :group 'Icicles))

(defcustom icicle-Completions-display-min-input-chars 0
  "*`*Completions*' window is removed if fewer chars than this are input.
You might want to set this to, say 1 or 2, to avoid display of a large
set of candidates during incremental completion.  The default value of
0 causes this option to have no effect: `*Completions*' is never
removed based only on the number of input characters.

If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'integer :group 'Icicles-Completions-Display)

(defcustom icicle-completions-format (if (boundp 'completions-format) ; Defined in Emacs 23+.
                                         completions-format
                                       'horizontal)
  "*Layout of completion candidates in buffer `*Completions*'.
`vertical' means display down columns first, then to the right.
`horizontal' or nil means display across rows first, then down.

Note that multi-line candidates are always displayed in a single
column, and in this case it makes no difference what the value of the
option is - the effect is the same.

You can toggle this option at any time from the minibuffer using
`\\<minibuffer-local-completion-map>\\[icicle-toggle-completions-format]'."
  :type '(choice
          (const :tag "Display vertically"    vertical)
          (other :tag "Display horizontally"  horizontal))
  :group 'Icicles-Completions-Display)

(defcustom icicle-move-Completions-frame 'right
  "*Non-nil means move `*Completions*' frame to the edge of the display.
This is done by `icicle-candidate-action'.
It only happens if `*Completions*' is alone in its frame.
This can be useful to make `*Completions*' more visible.
Possible values are `right', `left', and nil (do not move)."
  :type '(choice
          (const :tag "Move to right edge"  right)
          (const :tag "Move to right edge"  left)
          (const :tag "Do not move"         nil))
  :group 'Icicles-Completions-Display)

(defcustom icicle-Completions-max-columns nil
  "*Maximum number of columns to use in buffer `*Completions*'.
If nil, the number is calculated automatically.  I recommend that you
leave this nil and use options `icicle-inter-candidates-min-spaces'
and `icicle-candidate-width-factor' to control columns and candidate
spacing.

If the value is an integer and you use Do Re Mi (library `doremi.el')
then you can use multi-command `icicle-increment-option' anytime to
change the option value incrementally."
  :type '(choice
          (const   :tag "Calculate columns automatically"  nil)
          (integer :tag "Maximum number of columns" :value 1))
  :group 'Icicles-Completions-Display)

(defcustom icicle-Completions-mouse-3-menu-entries (if (fboundp 'doremi)
                                                       `(,icicle-Completions-this-candidate-submenu
                                                         ,icicle-Completions-sorting-submenu
                                                         ,icicle-doremi-submenu
                                                         ,icicle-Completions-save/retrieve-submenu
                                                         ,icicle-Completions-sets-submenu
                                                         ,icicle-Completions-toggle-submenu
                                                         ,icicle-Completions-misc-submenu)
                                                     `(,icicle-Completions-this-candidate-submenu
                                                       ,icicle-Completions-sorting-submenu
                                                       ,icicle-Completions-save/retrieve-submenu
                                                       ,icicle-Completions-sets-submenu
                                                       ,icicle-Completions-toggle-submenu
                                                       ,icicle-Completions-misc-submenu))
  "*Entries for the `mouse-3' popup menu in `*Completions*'.
The menu is created by `icicle-Completions-mouse-3-menu'.

The option value is a list.  Each element defines a submenu or a menu
item.  A null element (`nil') is ignored.

Several alternative entry formats are available.  When customizing,
choose an alternative in the Customize `Value Menu'.

In this description:
 SYMBOL      is a symbol identifying the menu entry.
 `menu-item' is just that text, literally.
 NAME        is a string naming the menu item or submenu.
 COMMAND     is the command to be invoked by an item.
 MENU-KEYMAP is a menu keymap or a var whose value is a menu keymap.
 KEYWORDS    is a property list of menu keywords (`:enable',
             `:visible', `:filter', `:keys', etc.).

1. Single menu item.  For a selectable item, use
   (SYMBOL menu-item NAME COMMAND . KEYWORDS).  For a non-selectable
   item such as a separator, use (SYMBOL NAME) or
   (SYMBOL menu-item NAME nil . KEYWORDS).

2. Items taken from a menu-keymap variable, such as
   `menu-bar-edit-menu'.  Just use the name of the variable (a
   symbol).  The items appear at the top level of the popup menu, not
   in a submenu.

3. Submenu.  Use (SYMBOL menu-item NAME MENU-KEYMAP . KEYWORDS) or
   (SYMBOL NAME . MENU-KEYMAP).  Remember that MENU-KEYMAP can also be
   a variable (symbol) whose value is a menu keymap.

All of these are standard menu elements, with the exception of the use
of a keymap variable to represent its value.

See also:
 * (elisp) Format of Keymaps
 * (elisp) Classifying Events
 * (elisp) Extended Menu Items

Example submenu element:
 (toto menu-item \"Toto\" menu-bar-toto-menu)

Example selectable menu-item element:
 (foo menu-item \"Foo\"   foo-command
       :visible (not buffer-read-only))"
  :type  '(repeat
           ;; $$$$$$ Perhaps should provide default :value for each choice.
           (choice
            ;; These could be combined, but it's better for users to see separate choices.
            (restricted-sexp
             :tag "Submenu (SYMBOL menu-item NAME MENU-KEYMAP . KEYWORDS) or (SYMBOL NAME . MENU-KEYMAP)"
             :match-alternatives
             ((lambda (x)
                (and (consp x) (symbolp (car x))
                     (or (and (stringp (cadr x)) (cddr x)) ; (SYMBOL NAME . MENU-KEYMAP)
                         ;; (SYMBOL menu-item NAME MENU-KEYMAP . KEYWORDS)
                         (and (eq 'menu-item (cadr x))
                              (stringp (car (cddr x)))
                              (or (keymapp (car (cdr (cddr x)))) ; Can be a keymap var.
                                  (and (symbolp (car (cdr (cddr x))))
                                       (boundp (car (cdr (cddr x))))
                                       (keymapp (symbol-value (car (cdr (cddr x)))))))))))
              'nil))
            (restricted-sexp
             :tag "Items from a keymap variable's value."
             :match-alternatives ((lambda (x) (and (symbolp x)  (keymapp (symbol-value x))))
                                  'nil))
            (restricted-sexp
             :tag "Selectable item (SYMBOL menu-item NAME COMMAND . KEYWORDS)"
             :match-alternatives ((lambda (x) (and (consp x)  (symbolp (car x))
                                                   (eq 'menu-item (cadr x))
                                                   (stringp (car (cddr x)))
                                                   (commandp (car (cdr (cddr x))))))
                                  'nil))
            (restricted-sexp
             :tag "Non-selectable item (SYMBOL NAME) or (SYMBOL menu-item NAME nil . KEYWORDS)"
             :match-alternatives ((lambda (x) (and (consp x)  (symbolp (car x))
                                                   (or (and (stringp (cadr x))  (null (caddr x)))
                                                       (and (eq 'menu-item (cadr x))
                                                            (stringp (car (cddr x)))
                                                            (null (car (cdr (cddr x))))))))
                                  'nil))))
  :group 'Icicles-Completions-Display)

(when (fboundp 'text-scale-decrease)    ; Emacs 23+
  (defcustom icicle-Completions-text-scale-decrease 0.75
    "*Initial height decrease for text in buffer `*Completions*'.
A value of 0.0 means the height is not decreased at all.
This is used as the argument to function `text-scale-decrease'.

If you use library Do Re Mi (library `doremi.el') then you can use
`C-x -' to incrementally resize the text during completion.

If you use `doremi.el' then you can also use multi-command
`icicle-increment-option' anytime to change the option value
incrementally.

See also options `icicle-candidate-width-factor' and
`icicle-inter-candidates-min-spaces'."
    :type 'number :group 'Icicles-Completions-Display))

(defcustom icicle-Completions-window-max-height 30
  "*Maximum height of `*Completions*' window, in lines.
The window is fit to the buffer size, with this as maximum height.
Not used if `*Completions*' is a special buffer with its own frame.
Not used in Emacs releases prior to 21.

If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'integer :group 'Icicles-Completions-Display)

(defcustom icicle-customize-save-flag t
  "*Non-nil means save some updated Icicles options when you quit Emacs.
That is, add some functions to `kill-emacs-hook' that call
`customize-save-variable'.  Currently, this includes only function
`icicle-command-abbrev-save', which saves updated option
`icicle-command-abbrev-alist'.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Minibuffer-Display)

(defcustom icicle-customize-save-variable-function 'customize-save-variable
  "*Function used to save user option changes.
I RECOMMEND that you do NOT change this.

The option value is a function that has the same signature as
`customize-save-variable' (perhaps with additional arguments after VAR
and VAL, the variable to save and its new value.

If you do not want changes that Icicles commands make to certain user
options to be saved automatically, you can set this to the function
\(symbol) `ignore'.  If you want to use your own function to somehow
save the current value, you can set this to your function."
  :type 'function :group 'Icicles-Miscellaneous)


;; Emacs 22-23 `cus-themes.el' has no `provide', and only Emacs 24 version
;; has `custom-available-themes'.
(when (condition-case nil (require 'cus-theme nil t) (error nil)) ; Emacs 24+

  (defcustom icicle-custom-themes ()
    "*List of custom themes to cycle through using `icicle-custom-theme'."
    :type '(repeat (restricted-sexp
                    :match-alternatives
                    ((lambda (symb) (memq symb (custom-available-themes))))))
    :group 'Icicles-Miscellaneous)

  (defcustom icicle-custom-themes-accumulate-flag nil
    "*Non-nil does not disable other custom themes when cycling to a theme.
Note: Setting this to non-nil can considerably slow down cycling.  The
more themes you cycle through, the slower it gets."
    :type 'boolean :group 'Icicles-Miscellaneous)

  (defcustom icicle-custom-themes-update-flag nil
    "*Non-nil means choosing a custom theme saves the updated list of themes.
This applies to commands `icicle-custom-theme' and
`icicle-color-theme' and their respective options
`icicle-custom-themes' and `icicle-color-themes'.

A prefix argument to `icicle-custom-theme' flips the option value for
the current invocation of the command."
    :type 'boolean :group 'Icicles-Miscellaneous))

(defcustom icicle-default-in-prompt-format-function (lambda (default) (format " (%s)" default))
  "*Function that formats the default value to include in the prompt.
The function must accept the default value (a string) as its (first)
argument and return a string, which is prepended to the first
occurrence of `: ' in the prompt.  This option has no effect unless
`icicle-default-value' is t."
  :group 'Icicles-Miscellaneous :type 'function)

(defcustom icicle-default-cycling-mode 'prefix
  "*Default completion mode for per-mode cycling.
When you hit a completion key (`TAB' or `S-TAB'), it sets the current
completion mode (prefix or apropos, respectively).  That determines
the kind of completion to be used by the per-mode cycling keys.

This option controls which completion mode to use if you cycle using a
per-mode key (e.g. `down') *before* hitting a completion key.

 - `prefix'  means cycle prefix completions
 - `apropos' means cycle apropos completions
 - Any other non-nil value means cycle inputs from the input history
 - nil means do not cycle - you must first hit a completion key

The per-mode cycling keys are the values of
`icicle-modal-cycle-up-keys' (backward) and
`icicle-modal-cycle-down-keys' (forward).  By default, these are keys
`up' and `down' as well as the mouse wheel.

For example, if the value is `prefix' (the default) then you can
immediately cycle prefix completions using `up', `down', or the mouse
wheel, without first hitting `TAB'.

Once you have used `TAB' or `S-TAB', the only way to traverse the
history is using `M-p' and `M-n' (they always traverse the history).

This option affects only cycling with the per-mode keys.  You can
always use the mode-specific cycling keys instead to cycle according
to a particular mode.  The mode-specific keys are (by default):

 - `end'  and `home'  for prefix completion
 - `next' and `prior' for apropos completion

\(By default there is no conflict between the cycling keys that are
mode-specific and those that are per-mode.  But if you customize them
in such a way that you set a key to both, the mode-specific use takes
priority.)

After you change the value of this option, toggle Icicle mode off,
then on again, for the change to take effect in the same session."
  :type '(choice
          (const :tag "Prefix cycling for per-mode keys, by default"                     prefix)
          (const :tag "Apropos cycling for per-mode keys, by default"                    apropos)
          (const :tag "No per-mode cycling - invoke completion first (`TAB', `S-TAB')"   nil)
          (other :tag "History cycling for per-mode keys, by default"                    t))
  :group 'Icicles-Key-Bindings)

(defcustom icicle-default-thing-insertion 'alternatives
  "*Behavior of successive `\\<minibuffer-local-map>\\[icicle-insert-string-at-point]'.
If `alternatives', then the next function in the `car' of
`icicle-thing-at-point-functions' is used to retrieve the text to be
inserted.
If `more-of-the-same', then the function that is the `cdr' of
`icicle-thing-at-point-functions' is used to retrieve the text to be
inserted."
  :type `(choice
          (const :tag ,(substitute-command-keys
                        "Successive calls to `\\<minibuffer-local-map>\
\\[icicle-insert-string-at-point]' use different text-grabbing functions.")
           alternatives)
          (const :tag ,(substitute-command-keys
                        "Successive calls to `\\<minibuffer-local-map>\
\\[icicle-insert-string-at-point]' grab more text at point.")
           more-of-the-same))
  :group 'Icicles-Key-Bindings)

;; We don't use `define-obsolete-variable-alias' so that byte-compilation in older Emacs
;; works for newer Emacs too.
(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'icicle-init-value-flag 'icicle-default-value)
  (make-obsolete-variable 'icicle-init-value-flag 'icicle-default-value "2008-04-18"))

(defcustom icicle-default-value t
  "*How to treat the default value when reading minibuffer input.
These are the possible option values:

  nil               - Do not insert default value or add it to prompt.
  t                 - Add default value to `completing-read' prompt.
                      Do not insert it.
  `insert-start'    - Insert default value and leave cursor at start.
  `insert-end'      - Insert default value and leave cursor at end.
  `preselect-start' - Insert and preselect default value;
                      leave cursor at beginning.
  `preselect-end'   - Insert and preselect default value;
                      leave cursor at end.

This option controls how a non-nil default-value argument to functions
such as `completing-read', `read-file-name', `read-from-minibuffer',
and `read-string' is handled.

When it is non-nil and the initial-input argument is nil or \"\", the
default value can be inserted into the minibuffer as the initial
input.

For `completing-read' and `read-file-name', if the option value is `t'
then the default value is normally added to the prompt as a hint.

However, for `read-file-name', if `insert-default-directory' is
non-nil, then to avoid duplication:

* If the default value is the same as `default-directory' it is not
  added to the prompt.

* If the default value is added to the prompt it is first made
  relative to `default-directory'.

Adding the default value to the prompt corresponds to the more or less
conventional behavior of vanilla Emacs.  But vanilla Emacs does not do
this systematically for `completing-read' (or for any of the
input-reading functions).  Instead, it hard-codes default values into
prompts in the commands that call these functions.

By design, Icicles commands never add the default value to the prompt
themselves.  This includes Icicles versions of standard commands that
might do so.  Icicles instead tries to give you the choice, using
option `icicle-default-value'.

Function `completing-read' is the only input-reading function for
which Icicles adds the default value to the prompt (for option value
`t').  Other such functions, like `(icicle-)read-from-minibuffer' and
`(icicle-)read-file-name', treat empty input (just `RET') specially -
see their doc for details.

Inserting the default value in the minibuffer as the initial input has
the advantage of not requiring you to use `M-n' to retrieve it.  It
has the disadvantage of making you use `M-p' (or do something else) to
get rid of the default value in the minibuffer if you do not want to
use or edit it.

If you often want to use or edit the default value, then set
`icicle-default-value' to non-nil and non-t.  If you rarely do so,
then set it to nil or t.

If the default value is inserted in the minibuffer, the value of this
option also determines whether or not the inserted text is
preselected, and where the cursor is left: at the beginning or end of
the text.

Preselection can be useful in Delete Selection mode or PC Selection
mode.  It makes it easy to replace the value by typing characters, or
delete it by hitting `C-d' or `DEL' (backspace).  However, all of the
initial input is lost if you type or hit `C-d' or `DEL'.  That is
inconvenient if you want to keep most of it and edit it only slightly.

My own preference for the option value is `insert-end', but the
default value is `t', which is closest to what vanilla Emacs does."
  :type '(choice
          (const :tag "Do not insert default value or add it to prompt"            nil)
          (const :tag "Add default value to prompt (do not insert in minibuffer)"  t)
          (const :tag "Insert default value.  Leave cursor at beginning"           insert-start)
          (const :tag "Insert default value.  Leave cursor at end"                 insert-end)
          (const :tag "Insert default value, select it, leave cursor at beginning" preselect-start)
          (const :tag "Insert default value, select it, leave cursor at end"       preselect-end))
  :group 'Icicles-Miscellaneous)

(defcustom icicle-define-alias-commands-flag t
  "*Non-nil means define some commands that do not begin with `icicle-'.
For convenience, a few top-level commands are defined, typically as
aliases for commands with longer names.  For example, command `toggle'
is defined as an alias for command `icicle-toggle-option'.  In any
case, no such command is ever defined by Icicles if a function with
the same name is already defined.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
   :type 'boolean :group 'Icicles-Miscellaneous)

(defcustom icicle-deletion-action-flag t
  "*Non-nil means `S-delete' during completion deletes the current object.
More precisely, it deletes the object named by the current completion
candidate, if a deletion action is defined for the current command.
If no deletion action is defined, then the value of this option has no
effect.

If you are worried about inadvertently deleting an object by
accidentally hitting `S-delete', you can customize this to nil to
inhibit `S-delete' object deletion during completion.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Miscellaneous)

(defcustom icicle-dot-show-regexp-flag nil
  "*Non-nil means show `icicle-anychar-regexp' explicitly for `.'.
Otherwise, display it as a highlighted `.' only.
This has no effect for Emacs versions prior to 21: acts as if non-nil.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Matching :group 'Icicles-Minibuffer-Display)

(defcustom icicle-dot-string "."
  "*String inserted by `icicle-insert-dot-command'.
It is either \".\" or the value of `icicle-anychar-regexp'.
You can toggle this at any time using command `icicle-toggle-dot',
bound to \\<minibuffer-local-completion-map>`\\[icicle-toggle-dot]' during completion."
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (setq icicle-dot-string-internal  icicle-dot-string))
  :type `(choice
          (const :tag "Match any char EXCEPT newline"       ".")
          (const :tag "Match any char, including NEWLINE"   ,icicle-anychar-regexp))
  :group 'Icicles-Matching :group 'Icicles-Minibuffer-Display)

(defcustom icicle-expand-input-to-common-match 4 ; Cycle with `C-M-"'.
  "*Expansion of your minibuffer input to the longest common match.
The expansion replaces your input in the minibuffer.

See the Icicles doc, section `Expanded-Common-Match Completion' for
information about what is meant by the \"longest\" common match.

This option controls when such expansion occurs.  You can cycle among
the possible values using \\<minibuffer-local-completion-map>\
`\\[icicle-cycle-expand-to-common-match]' in the minibuffer.

0 Do not expand your input ever, except when you use `C-M-TAB' or
  `C-M-S-TAB', which does not display `*Completions*'.

1 Do not expand your input automatically, during incremental
  completion.  Expand it only when you complete explictly, i.e., when
  you use `TAB' or `S-TAB'.

2 Like 1, but expand your input also when it matches only one
  completion candidate.

3 Like 2, but expand your input also during incremental prefix
  completion.

4 Expand your input always.  Like 3, but expand it also during
  incremental apropos completion.

As the value increases there are thus more contexts in which your
input can be expanded to the common match.  The expansion contexts for
each value include those for all lower values.

If you want to return to your original, unexpanded input, use \\<minibuffer-local-completion-map>\
`\\[icicle-retrieve-previous-input]'.

For apropos completion, your input is, in general, a regexp.  Setting
the option to a value other than four (4) lets you more easily work
with a regexp in the minibuffer for apropos completion - your regexp
is not replaced automatically by the expanded common match.

If you want to just toggle between the current value of this option
and one of the other values, then see also option
`icicle-expand-input-to-common-match-alt'.  You can toggle between the
values of these two options using \\<minibuffer-local-completion-map>\
`\\[icicle-toggle-expand-to-common-match]' in the minibuffer."
  :type '(choice
          (const :tag "Never expand (except for `C-M-TAB' and `C-M-S-TAB')"       0)
          (const :tag "No auto-expansion.  Expand only for explicit completion"   1)
          (const :tag "Auto-expand when only one matching completion"             2)
          (const :tag "Auto-expand for prefix completion or when only one match"  3)
          (const :tag "Auto-expand always: both prefix and apropos completion"    4))
  :group 'Icicles-Matching)

(defcustom icicle-expand-input-to-common-match-alt 3 ; Toggle with `C-"'.
  "*Other value for toggling `icicle-expand-input-to-common-match'.
The values of the two options should be different.  The value choices
are the same.  You can use \\<minibuffer-local-completion-map>\
`\\[icicle-toggle-expand-to-common-match]' to toggle between the two values."
  :type '(choice
          (const :tag "Never expand (except for `C-M-TAB' and `C-M-S-TAB')"       0)
          (const :tag "No auto-expansion.  Expand only for explicit completion"   1)
          (const :tag "Auto-expand when only one completion candidate matches"    2)
          (const :tag "Auto-expand for prefix completion or when only one match"  3)
          (const :tag "Auto-expand always: both prefix and apropos completion"    4))
  :group 'Icicles-Matching)

(defcustom icicle-file-extras nil
  "*List of additional file-name candidates added to the normal list.
List elements are strings."
  :type '(repeat string) :group 'Icicles-Files :group 'Icicles-Matching)

(defcustom icicle-file-match-regexp nil
  "*nil or a regexp that file-name completion candidates must match.
If nil, then this does nothing.  If a regexp, then show only
candidates that match it (and match the user input).
See also `icicle-file-no-match-regexp'."
  :type '(choice
          (const :tag "None" nil)
          (regexp :value "^[^.]"))
  :group 'Icicles-Files :group 'Icicles-Matching)

(defcustom icicle-file-no-match-regexp nil
  "*nil or a regexp that file-name completion candidates must not match.
If nil, then this does nothing.  If a regexp, then show only
candidates that do not match it.
See also `icicle-file-match-regexp'."
  :type '(choice
          (const :tag "None" nil)
          (regexp :value "^[.]"))
  :group 'Icicles-Files :group 'Icicles-Matching)

(defcustom icicle-file-predicate nil
  "*nil or a predicate that file-name candidates must satisfy.
If nil, then this does nothing.  Otherwise, this is a function of one
argument, a candidate, and only candidates that satisfy the predicate
are displayed.  For example, this value will show only names of files
with more than 5000 bytes:

  (lambda (file) (and (numberp (nth 7 (file-attributes file)))
                      (> (nth 7 (file-attributes file)) 5000)))

This predicate is applied after matching against user input.  It thus
corresponds to `icicle-must-pass-after-match-predicate', not to
`icicle-must-pass-predicate'."
  :type '(choice
          (const :tag "None" nil)
          (function :value file-readable-p))
  :group 'Icicles-Files :group 'Icicles-Matching)

(defcustom icicle-file-require-match-flag nil
  "*Override `icicle-require-match-flag' for file-name completion.
The possible values are as follows:
- nil means this option imposes nothing on completion;
  the REQUIRE-MATCH argument provided to the function governs behavior
- `no-match-required' means the same as a nil value for REQUIRE-MATCH
- `partial-match-ok' means the same as a t value for REQUIRE-MATCH
- `full-match-required' means the same as a non-nil, non-t value for
  REQUIRE-MATCH

Note: This option is provided mainly for use (binding) in
`icicle-define-command' and `icicle-define-file-command'.
You probably do not want to set this globally, but you can."
  :type '(choice
          (const :tag "Do not impose any match behavior"   nil)
          (const :tag "Do not require a match"             no-match-required)
          (const :tag "Require a partial match, with RET"  partial-match-ok)
          (const :tag "Require a full match"               full-match-required))
  :group 'Icicles-Files :group 'Icicles-Matching)

(defcustom icicle-file-sort nil
  "*A sort function for file names, or nil.
Examples of sort functions are `icicle-dirs-first-p',
`icicle-latest-access-first-p', and `icicle-latest-modification-first-p'.
If nil, then file names are not sorted."
  :type '(choice
          (const :tag "None" nil)
          (function :value icicle-dirs-first-p))
  :group 'Icicles-Files :group 'Icicles-Completions-Display)

(defcustom icicle-files-ido-like-flag nil
  "*t means `icicle-file' and similar commands act more Ido-like.
Specifically, those commands then bind these options to t:
 `icicle-show-Completions-initially-flag'
 `icicle-top-level-when-sole-completion-flag'
 `icicle-default-value'
This option has no effect for Emacs 20.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean
  :group 'Icicles-Files :group 'Icicles-Completions-Display :group 'Icicles-Matching)

(defcustom icicle-filesets-as-saved-completion-sets-flag t
  "*Non-nil means you can use filesets to save candidates persistently.
This means that you can save file-name candidates in a persistent
Icicles saved completion set (cache file) or in in an Emacs fileset.
It also means that an Icicles persistent completion set can contain
filesets, in addition to file names: any number of filesets, and
filesets of different type.  Available only for Emacs 22 and later,
and you must load library `filesets.el'.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Matching)

(defcustom icicle-find-file-expand-directory-flag nil
  "*Non-nil means that acting on a directory candidate descends into it.
That is, instead of opening Dired on the directory, the current set of
completion candidates are replaced by the contents of the directory.

You can toggle this option at any time from the minibuffer using
`\\<minibuffer-local-completion-map>\\[icicle-toggle-expand-directory]'."
  :type 'boolean :group 'Icicles-Files :group 'Icicles-Miscellaneous)

(defcustom icicle-file-search-dir-as-dired-flag nil
  "Non-nil means `icicle-file' searches directories as Dired listings.
This applies to `icicle-file' and similar multi-completion commands
that let you match file content as well as file names.

The option has no effect if your input has no content-matching part.

The default value of nil prevents a command from visiting a directory
in Dired mode to search for the content-matching part of your
multi-completion input.

A non-nil value means that such Dired visiting and searching is
governed instead by the value of option `find-file-run-dired'.

Option `icicle-file-search-dir-as-dired-flag' is specifically for
content-matching.  A nil value lets you prevent content matching but
still allow file-finding commands to visit a directory in Dired (by
way of non-nil `find-file-run-dired')."
  :type 'boolean :group 'Icicles-Files :group 'Icicles-Matching :group 'Icicles-Searching)

(defcustom icicle-file-skip-functions '(icicle-image-file-p icicle-file-elc-p)
  "*Hook run by file-visiting commands on each matching file name.
The value is a list of functions.  Each is applied to the file-name
candidate (after transforming it, if it is a multi-completion), until
one of them returns a non-nil value.

If any of the functions returns non-nil then the file content is not
searched.  Use this to skip visiting and trying to search non-text
files, such as PDFs and images, or files that might be time-consuming
to access, such as compressed files.

Note that the file names passed to the hook can be absolute or
relative, depending on the command used.

Also run by `icicle-buffer' to exclude filenames in the set of recent
files or the Emacs file cache from being included as candidates.

This option has no effect for Emacs versions prior to Emacs 23.

See also option `icicle-buffer-skip-functions'."
  :type 'hook :group 'Icicles-Files :group 'Icicles-Matching)

(defcustom icicle-functions-to-redefine
  `(;; bbdb-complete-name               ; For older BBDB versions such as 2.35
    bbdb-complete-mail                  ; For BBDB versions such as 3.02 and later
    ,@(if (> emacs-major-version 23) '(comint-completion-at-point) '(comint-dynamic-complete))
    comint-dynamic-complete-filename comint-replace-by-expanded-filename
    complete

    ;; This is to work around Emacs bug #24676.
    ,@(and (fboundp 'completion-pcm--all-completions) '(completion-pcm--all-completions))

    ;; Uncomment `dired-read-shell-command' and `read-shell-command' if you want Icicles completion for
    ;; shell commands.  See https://www.emacswiki.org/emacs/Icicles_-_Shell-Command_Enhancements.
    ;; dired-read-shell-command             read-shell-command

    ess-complete-object-name             gud-gdb-complete-command
    Info-goto-node                       Info-index
    Info-menu
    lisp-complete-symbol
    ;; Emacs 25+ uses `elisp-completion-at-point', not `lisp-completion-at-point'.
    ,@(if (fboundp 'elisp-completion-at-point) '(elisp-completion-at-point) '(lisp-completion-at-point))
    minibuffer-default-add-completions
    read-char-by-name                    read-color
    read-from-minibuffer                 read-string
    recentf-make-menu-items)
  "*List of symbols representing functions to be redefined in Icicle mode.
In Icicle mode, each such FUNCTION is aliased to Icicles function
`icicle-FUNCTION'.  The original functions are restored when you exit
Icicle mode, by aliasing each FUNCTION to `icicle-ORIG-FUNCTION', that
is, using the prefix `icicle-ORIG-' instead of `icicle-'.

Aliasing takes place only if `icicle-ORIG-FUNCTION' is defined.
Icicles predefines each `icicle-ORIG-FUNCTION' found in the default
value, as well as each corresponding `icicle-FUNCTION' .  If you add
additional functions of your own choosing, then you will also need to
define `icicle-ORIG-FUNCTION' and `icicle-FUNCTION' accordingly - see
the Icicles code for examples.

If you customize this option, then you must exit and re-enter Icicle
mode to ensure that the change takes effect.

For this option to have an effect upon startup, it must be set before
you enter Icicle mode.  This means that you must ensure that the code
that sets it is invoked before you enter Icicle mode.  If you use
Customize to change this option, then ensure that the code inserted by
Customize into your `user-init-file' or your `custom-file' is invoked
before you enter Icicle mode.

See also option `icicle-top-level-key-bindings'.

Note: If you want the special Icicles completion for shell commands,
then include these two functions in the option value:
`dired-read-shell-command' and `read-shell-command'."
  :type '(repeat (restricted-sexp :tag "Command"
                  ;; Use `symbolp' instead of `functionp' or `fboundp', in case the library
                  ;; defining the function is not loaded.
                  :match-alternatives (symbolp) :value ignore))
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (when (boundp 'icicle-mode-map) ; Avoid error on initialization.
           (icicle-redefine-standard-functions)))
  :initialize #'custom-initialize-default
  :group 'Icicles-Miscellaneous)

(defcustom icicle-guess-commands-in-path nil
  "*Non-nil means all shell commands are available for completion.
This is used in Icicle mode whenever a shell-command is read.

If non-nil, then all executable files (or all files, if option
`shell-completion-execonly' is nil) in your search path are included
among the completion candidates, in addition to any commands that are
guessed as being appropriate for the target files (e.g. marked files
in Dired).

If non-nil and if option `icicle-shell-command-candidates-cache' is
nil, then the list of commands is computed once and cached as the
value of `icicle-shell-command-candidates-cache'.  The particular
non-nil value of `icicle-guess-commands-in-path' determines when the
cache is filled, as follows:

- If the value is `load', then the cache is filled when Icicles is
  first loaded, and it is saved persistently.

- If the value is `first-use', then the cache is filled when you first
  complete a shell command, and the computed list is not saved
  persistently.

If the value is not `load', then whenever you enter Icicle mode the
cache is emptied.

If your environment changes and you want to update the cached list,
you can use command `icicle-recompute-shell-command-candidates'.  With
a prefix argument, that command also saves the cache persistently."
  :type '(choice
          (const :tag "Do not add shell commands from search path"              nil)
          (const :tag "Compute shell commands from path when Icicles is loaded" load)
          (const :tag "Compute shell commands from path upon first use"         first-use))
  :group 'Icicles-Miscellaneous)

(defcustom icicle-help-in-mode-line-delay 5
  "*Seconds to show help in the mode-line for individual completions.
If buffer `*Completions*' is displayed, then use its mode-line.
Otherwise, use the mode-line of the current buffer.

The help is shown when you cycle among completion candidates and when
your input is completed (entirely) to a candidate.

Face `icicle-mode-line-help' is used for the help.

A value of zero means do not show such help at all.  In any case, a
user event (e.g. a key press) always interrupts this display.

Note that `post-command-hook' actions do not take place until this
display is finished.  For example, if the help is shown because your
input is complete, then Icomplete will not show additional candidates
\(e.g. with the same input as a prefix) until the mode-line help has
finished.  This is because Icomplete display is a `post-command-hook'
action.

If you use library Do Re Mi (`doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'number :group 'Icicles-Completions-Display :group 'Icicles-Miscellaneous)

(defcustom icicle-hide-common-match-in-Completions-flag nil
  "*Non-nil means hide the common match for your input, in `*Completions*'.
The common match used here is governed by option
`icicle-expand-input-to-common-match'.  It is elided using
ellipsis (`...').

You can toggle this option during completion using `C-x .' (no prefix
arg).  You can also use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle the option value.

See also option `icicle-hide-non-matching-lines-flag'."
  :type 'boolean :group 'Icicles-Miscellaneous)

(defcustom icicle-hide-non-matching-lines-flag nil
  "*Non-nil means hide search candidate lines that do not match input.
This applies only to multi-line candidates in buffer `*Completions*'.
Lines that do not contain text matched by your current
minibuffer input are elided using ellipsis (`...').

You can toggle this option during completion using `C-u C-x .'.  You
can also use multi-command `icicle-toggle-option' anytime to toggle
\(`M-i M-i' during completion) the option value.

See also option `icicle-hide-common-match-in-Completions-flag'."
  :type 'boolean :group 'Icicles-Completions-Display)

;; Same as `hide-whitespace-before-comment-flag' in `hide-comnt.el'.
;; Same as `isearchp-whitespace-before-comment-flag' in `isearch-prop.el'.
;;
(defcustom icicle-hide-whitespace-before-comment-flag t
  "*Non-nil means hide whitespace preceding a comment.
Empty lines (newline chars) are not hidden, however.
Used by `icicle-hide/show-comments'."
  :type 'boolean :group 'Icicles-Miscellaneous)

(defcustom icicle-highlight-historical-candidates-flag t ; Toggle with `C-pause'.
  "*Non-nil means highlight `*Completions*' candidates that have been used.
This is done using face `icicle-historical-candidate'.
Historical candidates are those that you have entered (using `RET' or
`S-RET') previously.

You can toggle this option from the minibuffer at any time using
`C-pause'.  You can also use multi-command `icicle-toggle-option'
\(`M-i M-i' during completion) anytime to toggle the option value."
  :type 'boolean :group 'Icicles-Completions-Display)

(defcustom icicle-highlight-input-completion-failure 'implicit-strict
  "*Non-nil means highlight the part of your input that does not complete.
This is done using face `icicle-input-completion-fail' or
`icicle-input-completion-fail-lax'.

You can use `\\<minibuffer-local-completion-map>\\[icicle-goto/kill-failed-input]' \
to go to the start of the highlighted part.
Repeat to kill it.

This highlighting can have a negative impact on performance, because
it can mean recomputing completion candidates multiple times, in order
to determine the longest part that completes.  For this reason, you
can fine tune when you want this highlighting to occur.  The values of
this option and options
`icicle-highlight-input-completion-failure-delay' and
`icicle-highlight-input-completion-failure-threshold' determine when
the highlighting can take place.

In particular, highlighting the non-matching part of remote file names
can be slow.  Two values of this option allow remote file name
highlighting: `always' and `explicit-remote'.  The other values do not
highlight remote file names.  You probably do not want to use a value
of `always'.

If the value is nil, then highlighting never occurs.  If the value is
`explicit-strict', `explicit', or `explicit-remote', then highlighting
occurs only upon demand: when you hit `TAB' or `S-TAB' to request
completion.  If the value is `implicit-strict', `implicit', or
`always', then highlighting occurs also when you update your input
during incremental completion.

If the value is `implicit-strict' or `implicit', then highlighting
occurs not only upon demand but also during incremental completion if
`icicle-incremental-completion' is non-nil.  Remember that you can
cycle incremental completion, using `C-#' in the minibuffer.

I use a value of `implicit' myself, but the default value is
`implicit-strict' because, depending on your setup and use cases,
`implicit' can impact performance for file-name completion (which is
lax, not strict).  I suggest you try `implicit' to see - this feature
is especially useful for file names.

Summary of choices for when to highlight:

nil               Never
`explicit-strict' When you hit `TAB'/`S-TAB' for strict completion
`explicit'        When you hit `TAB'/`S-TAB'
`explicit-remote' When you hit `TAB'/`S-TAB', including remote files
`implicit-strict' During strict completion
`implicit'        During lax or strict completion
`always'          Always, even for names of remote files

After highlighting, you can use `C-M-l' to move the cursor to the
start of the mismatch, for editing there.  You can use a second
`C-M-l' to kill (delete) the mismatch up to the next input line (if
any).  You can repeat `C-M-l' to kill additional input lines.

See also:
* `icicle-highlight-input-completion-failure-delay'
* `icicle-highlight-input-completion-failure-threshold'"
  :type '(choice
          (const :tag "Never"                                               nil)
          (const :tag "Explicit (`TAB'/`S-TAB') strict completion"          explicit-strict)
          (const :tag "Explicit (`TAB'/`S-TAB') lax and strict completion"  explicit)
          (const :tag "Explicit completion, even of remote file names"      explicit-remote)
          (const :tag "Strict completion"                                   implicit-strict)
          (const :tag "Lax and strict completion"                           implicit)
          (const :tag "Always (including for remote file names)"            always))
  :group 'Icicles-Minibuffer-Display)

(defcustom icicle-highlight-input-completion-failure-delay 0.7
  "*Seconds to wait before highlighting non-completing part of your input.
Zero means there is no wait.

If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'number :group 'Icicles-Minibuffer-Display)

(defcustom icicle-highlight-input-completion-failure-threshold 1000
  "*More candidates means do not highlight non-completing part of input.
See also `icicle-highlight-input-completion-failure'.

If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'integer :group 'Icicles-Minibuffer-Display)

(defcustom icicle-highlight-input-initial-whitespace-flag t
  "*Non-nil means highlight initial whitespace in your input.
This is done using face `icicle-whitespace-highlight'.
Purpose: Otherwise, you might not notice that you accidentally typed
some whitespace at the beginning of your input, so you might not
understand the set of matching candidates (or lack thereof).

Note: Highlighting input completion failure (see option
`icicle-highlight-input-completion-failure') subsumes
initial-whitespace highlighting.  This means that if no completion
candidate starts with whitespace, and if Icicles is highlighting input
completion failure, then only that highlighting is shown.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Minibuffer-Display)

(defcustom icicle-highlight-lighter-flag t
  "*Non-nil means highlight the `Icy' mode-line lighter during completion.
See the Icicles doc, section `Nutshell View of Icicles', subsection
`Completion Status Indicators' for more information.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Miscellaneous)

(defcustom icicle-highlight-saved-candidates-flag t ; Toggle with `S-pause'.
  "*Non-nil means highlight `*Completions*' candidates that have been saved.
This is done using face `icicle-saved-candidate'.
You save candidates using, for example, `C-M->'.

You can toggle this option from the minibuffer at any time using
`S-pause'.  You can also use multi-command `icicle-toggle-option'
\(`M-i M-i' during completion) anytime to toggle the option value."
  :type 'boolean :group 'Icicles-Completions-Display)

(defcustom icicle-icomplete-mode-max-candidates 30
  "*Automatically turn Icomplete mode off if there are more candidates.
This takes effect (only) whenever there is an attempt to complete your
input, whether that happens manually (`TAB', `S-TAB') or automatically
due to Icicles incremental completion.

If Icomplete mode was enabled before completion was initiated, then it
is reenabled when the number of candidates falls below the option
value.  Icicles does not turn Icomplete mode on unless it was on when
the minibuffer was activated.

If the option value is not an integer, then it must be nil.  In this
case, Icicles does not turn Icomplete mode off and on.

\(Note that this is about Emacs `icomplete-mode', not Icicles
incremental completion.)

This option has no effect for Emacs versions prior to Emacs 23."
  :type '(choice
          (integer :tag "Max number of candidates before inhibiting Icomplete mode" :value 10)
          (const :tag "No maximum - do not automatically inhibit Icomplete mode"))
  :group 'Icicles-Miscellaneous)

(defcustom icicle-ignore-comments-flag t
  "*Non-nil means `icicle-with-comments-hidden' hides comments.
You can toggle this option using `C-M-;' in the minibuffer, but to see
the effect you might need to invoke the current command again.

You can also use multi-command `icicle-toggle-option' anytime to
\(`M-i M-i' during completion) toggle the option value."
  :type 'boolean :group 'Icicles-Searching)

(defcustom icicle-ignored-directories (and (boundp 'vc-directory-exclusion-list)
                                           vc-directory-exclusion-list)
  "*Directories ignored by `icicle-locate-file'."
  :type '(repeat string) :group 'Icicles-Files)

;; Do not check here whether (if (fboundp 'display-graphic-p) (display-graphic-p) window-system).
;; Do that only at runtime, where we display the thumbnails.
(defcustom icicle-image-files-in-Completions (and (fboundp 'image-file-name-regexp)  t)
  "*Non-nil means show thumbnail images for image files in `*Completions*'.
This has no effect if your Emacs version does not have image support.

 `nil'        means show only file names.
 `image-only' means show only thumbnail images.
 `t'          means show both file names and thumbnail images.

You can cycle the value during completion using `C-x t'."
  :type '(choice
          (const :tag "Both name and thumbnail"  t)
          (const :tag "Thumbnail image only"     image-only)
          (const :tag "File name only"           nil))
  :group 'Icicles-Completions-Display)

(defcustom icicle-image-preview-in-tooltip (if icicle-image-files-in-Completions
                                               'full
                                             (if (boundp 'diredp-image-preview-in-tooltip)
                                                 diredp-image-preview-in-tooltip
                                               (or (and (boundp 'image-dired-thumb-size)
                                                        image-dired-thumb-size)
                                                   100)))
  "*Whether & what kind of image preview to show in a *Completions* tooltip.
The possible values are:

 `nil'       : do not show a tooltip preview
 integer N>0 : show a thumbnail preview of that size
 `full'      : show a full-size preview of the image

A tooltip image preview is shown only if either
`icicle-image-files-in-Completions' is nil or
`icicle-image-preview-in-tooltip' is `full'.  (A thumbnail tooltip
preview is not shown on mouseover if thumbnails are already shown in
`*Completions*'.)

To enable tooltip image preview you must turn on `tooltip-mode' and
load library `image-dired.el'.

This option has no effect for Emacs versions prior to Emacs 22."
  :type '(choice
          (restricted-sexp :tag "Show a thumnail image of size"
           :match-alternatives ((lambda (x) (and (wholenump x)  (not (zerop x))))))
          (const :tag "Show a full-size image preview"      full)
          (const :tag "OFF: Do not show an image preview"   nil))
  :group 'Icicles-Completions-Display)

(defcustom icicle-incremental-completion-delay 0.7
  "*Number of seconds to wait before updating `*Completions*' incrementally.
There is no wait if the number of completion candidates is less than
or equal to `icicle-incremental-completion-threshold'.
See also `icicle-incremental-completion'.

If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'number :group 'Icicles-Completions-Display)

(defcustom icicle-incremental-completion t ; Cycle with `C-#'.
  "*Non-nil means update `*Completions*' buffer incrementally as you type.
nil means do not update `*Completions*' incrementally, as you type.

t means do nothing if `*Completions*' is not already displayed.
Non-nil and non-t means display `*Completions*' and update it.

You can cycle this among the possible values using `C-#' from the
minibuffer at any time.

Note: Incremental completion is effectively turned off when a remote
file name is read, that is, whenever your file-name input matches a
remote-file syntax.

See also `icicle-incremental-completion-delay' and
`icicle-incremental-completion-threshold'."
  :type '(choice
          (const :tag "Do not update `*Completions*' incrementally"                nil)
          (const :tag "Update `*Completions*' incrementally if already displayed"  t)
          (other :tag "Update `*Completions*' incrementally always"                always))
  :group 'Icicles-Completions-Display)

(defcustom icicle-incremental-completion-threshold 1000
  "*More candidates means apply `icicle-incremental-completion-delay'.
See also `icicle-incremental-completion' and
`icicle-incremental-completion-delay'.
This threshold is also used to decide when to display the message
 \"Displaying completion candidates...\".

If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'integer :group 'Icicles-Completions-Display)

(when (> emacs-major-version 21)        ; Emacs 22+
  (defcustom icicle-Info-highlight-visited-nodes nil
    "When to automatically highlight visited Info node names in `*Completions*'.
Regardless of the value, on-demand highlighting is always available,
using `C-x C-M-l'.

Automatic highlighting occurs only if this option value is non-nil and
the value of option `icicle-highlight-historical-candidates-flag' is
also non-nil."
    :type '(choice
            (const   :tag "Always highlight visited candidate Info node names"        t)
            (integer :tag "Max number of candidates for highlighting visited nodes"   :value 10)
            (const   :tag "Never highlight visited nodes (highlight on demand only)"  nil))
    :group 'Icicles-Completions-Display))

(defcustom icicle-inhibit-advice-functions
  `(choose-completion  choose-completion-string  completing-read
    completion-setup-function
    ,@(and (not (fboundp 'read-shell-command))  '(dired-smart-shell-command)) ; Emacs < 23
    display-completion-list  exit-minibuffer  face-valid-attribute-values
    minibuffer-complete-and-exit  mouse-choose-completion
    next-history-element  read-face-name  read-file-name
    ,@(and (fboundp 'read-number)  '(read-number)) ; Emacs 22+
    ,@(and (not (fboundp 'read-shell-command))  '(shell-command shell-command-on-region)) ; Emacs < 23
    switch-to-completions  completing-read-multiple)
  "*Functions that Icicles redefines, and for which advice is deactivated.
Icicle mode deactivates all advice for such functions.  The advice is
reactivated when you leave Icicle mode."
  :type '(repeat (function :tag "Function for which Icicles deactivates advice"))
  :group 'Icicles-Miscellaneous)

(defcustom icicle-inhibit-ding-flag nil
  "*Non-nil means Icicles never uses an audible bell (ding).
If nil, Icicles sometimes signals you with a sound.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Miscellaneous)

(defcustom icicle-input-string ".*"
  "*String to insert in minibuffer via `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]'.
Typically, this is a regexp or a portion of a regexp."
  :type 'string :group 'Icicles-Miscellaneous)

(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'icicle-key-descriptions-use-angle-brackets-flag
      'icicle-key-descriptions-use-<>-flag))

(defcustom icicle-inter-candidates-min-spaces 1
  "*Min number of spaces between candidates displayed in `*Completions*'.
If you use Do Re Mi (library `doremi.el'), then you can modify this
option incrementally during completion, seeing the effect as it
changes.  Use `\\<minibuffer-local-completion-map>\
\\[icicle-doremi-inter-candidates-min-spaces+]' from the minibuffer, then use the `up' and
`down' arrow keys or the mouse wheel to increment and decrement the
value.  WYSIWYG.

If you use `doremi.el' then you can also use multi-command
`icicle-increment-option' anytime to change the option value
incrementally.

See also option `icicle-candidate-width-factor' and (starting with
Emacs 23) option `icicle-Completions-text-scale-decrease'."
  :type 'integer :group 'Icicles-Completions-Display)

(defcustom icicle-isearch-complete-keys '([C-M-tab] ; `M-TAB', `C-M-TAB'
                                          [(control meta ?i)]
                                          [M-tab] ; Replace vanilla.
                                          [escape tab])
  "*Key sequences to use for `icicle-isearch-complete'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.

The default value includes `M-TAB', which replaces the vanilla binding
of `isearch-complete'.

It also includes `ESC TAB' and `C-M-TAB', because some operating
systems intercept `M-TAB' for their own use.  (Note: For some versions
of MS Windows, you can use (w32-register-hot-key [M-tab]) to allow
Emacs to use `M-TAB'.)"
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-isearch-history-insert-keys '([(meta ?o)]) ; `M-o', like Icicles minibuffer
  "*Key sequences to use for `icicle-isearch-history-insert'.
A list of values that each has the same form as a key-sequence
argument to `define-key'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-key-complete-keys (if (> emacs-major-version 23) ; `S-TAB'
                                        '([backtab])
                                      '([S-tab] [S-iso-lefttab]))
  ;; In Emacs 22 and later, `backtab' is the canonical key that represents both `S-tab' and
  ;; `S-iso-lefttab', so that is used in the default value.  If, for some reason, `backtab' is not being
  ;; translated to `S-tab' and `S-iso-lefttab' on your platform, you might want to customize the value
  ;; to ([S-tab] [S-iso-lefttab]).  And if your Emacs version is 22 or later, please file an Emacs bug
  ;; about the lack of translation.

  ;; The reason that the default value here is not just ([backtab]) for Emacs < 24 is that some Emacs
  ;; libraries, such as `info.el', explicitly bind both `backtab' and `S-tab'.  I filed Emacs bug #1281,
  ;; which took care of this for Emacs 24+.
  "*Key sequences to use for `icicle-complete-keys'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards - for example, `S-tab' and `S-iso-lefttab'."
  :type '(repeat sexp) :group 'Icicles-Key-Completion :group 'Icicles-Key-Bindings)

(defcustom icicle-key-complete-keys-for-minibuffer '([M-backtab] [ESC backtab]) ; `M-S-TAB', `ESC S-TAB'
  "*Key sequences to use for `icicle-complete-keys' in the minibuffer.
A list of values that each has the same form as a key-sequence
argument to `define-key'.

Note: Some operating systems intercept `M-S-TAB' for their own use.
For some versions of MS Windows, you can use
\(w32-register-hot-key [M-S-tab]) to allow Emacs to use `M-S-TAB'."
  :type '(repeat sexp) :group 'Icicles-Key-Completion :group 'Icicles-Key-Bindings)

(defcustom icicle-key-descriptions-use-<>-flag nil
  "*Non-nil means Icicles key descriptions use angle brackets (<>).
For example, non-nil gives `<mode-line>'; nil gives `mode-line'.
This does not affect Emacs key descriptions outside of Icicles.
This has no effect for versions of Emacs prior to 21, because
they never use angle brackets.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Key-Completion :group 'Icicles-Minibuffer-Display)

(defcustom icicle-keymaps-for-key-completion
  '(apropos-mode-map bookmark-bmenu-mode-map bmkp-jump-map bmkp-jump-other-window-map
    calendar-mode-map dired-mode-map facemenu-keymap help-mode-map icicle-toggle-map
    jde-mode-map jde-jdb-mode-map senator-mode-map srecode-mode-map synonyms-mode-map
    vc-dired-mode-map)
  "*List of keymaps in which to bind `S-TAB' to `icicle-complete-keys'.
List elements are symbols that are bound to keymaps.

Each keymap should have at least one prefix key.  `S-TAB' is bound in
each keymap, so that you can use it to complete the prefix keys.

If one of the keymaps is not defined when Icicle mode is entered, then
it is ignored.  If you later define it, then just exit and reenter
Icicle mode, to bind `S-TAB' in the newly defined map.  For example,
use `M-x icy-mode' twice after entering Calendar mode, to be able to
complete `calendar-mode' prefix keys such as `A'.

Do not add `global-map' or any keymaps, such as `ctl-x-map', that are
accessible from the global keymap to the list - they are already
treated, by default.

Do not add any of the translation keymaps, `function-key-map',
`key-translation-map', or `iso-transl-ctl-x-8-map' to the list - that
will not work."
  :type '(repeat symbol) :group 'Icicles-Key-Completion :group 'Icicles-Key-Bindings)

(defcustom icicle-kill-visited-buffers-flag t
  "*Non-nil means kill buffers visited temporarily to search files.
This applies to commands such as `icicle-find-file-of-content', which
search files that match your completion input.  If non-nil then any
such buffers for files that you do not actually choose are killed when
the command is finished.  If nil then they are not killed.

However, note that for some commands a prefix argument can reverse the
sense of this flag."
  :type 'boolean :group 'Icicles-Buffers :group 'Icicles-Files :group 'Icicles-Matching)

(when (require 'kmacro nil t)           ; Emacs 22+
  (defcustom icicle-kmacro-ring-max (if (boundp 'most-positive-fixnum)
                                        most-positive-fixnum
                                      67108863) ; 1/2 of `most-positive-fixnum' on Windows.
    "*Icicles version of `kmacro-ring-max'.
If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
    :type 'integer :group 'Icicles-Miscellaneous))

(defcustom icicle-levenshtein-distance 1
  "*Levenshtein distance allowed for strings to be considered as matching.
Icicles matching function `icicle-levenshtein-match' considers a
string to match another if the first string is within this distance of
some substring of the second.
This option is used only if you have library `levenshtein.el'.

If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'integer :group 'Icicles-Matching)

;; Note: If your copy of this file does not have the two-character string "^G^J"
;; (Control-G, Control-J) or, equivalently, \007\012, as the default value, you will want
;; to change the file to have that.  To insert these control characters in the file, use
;; `C-q'.  Emacs Wiki loses the ^G from the file, so I use \007, which works OK.
;;
(defcustom icicle-list-join-string (let ((strg  (copy-sequence "\007\012")))
                                     ;; Emacs 20 ignores `display', so don't bother.
                                     ;; Emacs 21 has a big bug, which interprets `display' badly.
                                     (when (> emacs-major-version 21) ; Avoid Emacs 21 bug.
                                       (set-text-properties 0 (length strg) '(display "\n") strg))
                                     strg)
  "*String joining items in a completion that is a list of strings.
When a completion candidate is a list of strings, this string is used
to join the strings in the list, for display and matching purposes.
When completing input, you type regexps that match the strings,
separating them pairwise by the value of `icicle-list-join-string'.
Actually, what you enter is interpreted as a single regexp to be
matched against the joined strings.  Typically, the candidate list
contains two strings: a name and its doc string.

A good value for this option is a string that:
 1) does not normally occur in doc strings,
 2) visually separates the two strings it joins, and
 3) is not too difficult or too long to type.

The default value is \"^G\^J\", that is, control-g followed by
control-j (newline):
 1) ^G does not normally occur in doc strings
 2) a newline visually separates the multiple component strings, which
    helps readability in buffer `*Completions*'
 3) you can type the value using `C-q C-g C-q C-j'.

For readability (in Emacs 22 and later), the default value has a
`display' property that makes it appear as simply a newline in
`*Completions*' - the `^G' is hidden.  you can also make the default
value appear this way in your minibuffer input also, by using \
`\\<minibuffer-local-completion-map>\\[icicle-insert-list-join-string].'

If you like the default value of `^G^J', but you prefer that the `^G'
not be hidden, then just customize this option.  In Customize, use
`Show initial Lisp expression' after clicking the `State' button, to
be able to edit the default value.  Remove the `set-text-properties'
expression, which sets text property `display' to \"\"."
  :type 'string :group 'Icicles-Completions-Display)

(defcustom icicle-list-nth-parts-join-string " "
  "*String joining candidate parts split by `icicle-list-use-nth-parts'.
This has an effect on multi-completion candidates only, and only if
the current command uses `icicle-list-use-nth-parts'."
  :type 'string :group 'Icicles-Completions-Display)

(defcustom icicle-max-candidates nil
  "*Non-nil means truncate completion candidates to at most this many.
If you use library `doremi.el' then you can use `C-x #' during
completion to increment or decrement the option value using the
vertical arrow keys or the mouse wheel.  A numeric prefix argument for
`C-x #' sets the increment size.  A plain prefix argument (`C-u')
resets `icicle-max-candidates' to nil, meaning no truncation.

If the value is an integer and you use Do Re Mi (library `doremi.el')
then you can use multi-command `icicle-increment-option' anytime to
change the option value incrementally."
  :type '(choice
          (const :tag "None" nil)
          (integer :value 200))
  :group 'Icicles-Completions-Display :group 'Icicles-Matching
  :group 'Icicles-Buffers :group 'Icicles-Files)

(defcustom icicle-menu-items-to-history-flag t
  "*Non-nil means to add menu-item commands to the command history.
This history is `extended-command-history'.

After you change the value of this option, toggle Icicle mode off,
then on again, for the change to take effect in the same session.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Miscellaneous)

;; Inspired from `icomplete-minibuffer-setup-hook'.
(defcustom icicle-minibuffer-setup-hook nil
  "*Functions run at the end of minibuffer setup for Icicle mode."
  :type 'hook :group 'Icicles-Miscellaneous)

(defcustom icicle-minibuffer-key-bindings
  `(
;;;    (,(icicle-kbd "C-?")           icicle-minibuffer-help t)                          ; `C-?'
    (,(icicle-kbd "M-?")           icicle-minibuffer-help t)                          ; `M-?'
    (,(icicle-kbd "M-S-backspace") icicle-erase-minibuffer t)                         ; `M-S-backspace'
    (,(icicle-kbd "M-S-delete")    icicle-erase-minibuffer t)                         ; `M-S-delete'
    (,(icicle-kbd "M-.")           icicle-insert-string-at-point t)                   ; `M-.'
    (,(icicle-kbd "C-x C-f")       icicle-resolve-file-name t)                        ; `C-x C-f'
    (,(icicle-kbd "C-=")           icicle-insert-string-from-variable t)              ; `C-='
    (,(icicle-kbd "M-k")           icicle-erase-minibuffer-or-history-element t)      ; `M-k'
    (,(icicle-kbd "M-K")           icicle-clear-current-history t)                    ; `M-K'
    (,(icicle-kbd "M-o")           icicle-insert-history-element t)                   ; `M-o'
    (,(icicle-kbd "M-R")           icicle-multi-inputs-act t)                         ; `M-R'
    (,(icicle-kbd "M-S")           icicle-multi-inputs-save t)                        ; `M-S'
    (,(icicle-kbd "M-:")           icicle-pp-eval-expression-in-minibuffer t)         ; `M-:'
    (,(icicle-kbd "C-a")           icicle-beginning-of-line+ t)                       ; `C-a'
    (,(icicle-kbd "C-e")           icicle-end-of-line+ t)                             ; `C-e'
    (,(icicle-kbd "C-g")           icicle-abort-recursive-edit t)                     ; `C-g'
    (,(icicle-kbd "C-M-S-t")       icicle-top-level t)                                ; `C-M-S-t'
                                                                                      ; ( `C-M-T')
    (,(icicle-kbd "C-M-v")         icicle-scroll-forward t)                           ; `C-M-v'
    (,(icicle-kbd "C-M-S-v")       icicle-scroll-backward t)                          ; `C-M-S-v'
                                                                                      ; ( `C-M-V')
    (,(icicle-kbd "C-M-pause")     icicle-other-history t)                            ; `C-M-pause'
    (,(icicle-kbd "C-j")           icicle-insert-newline-in-minibuffer t)             ; `C-j'
    (,(icicle-kbd "C-M-y")         icicle-yank-secondary (fboundp 'icicle-yank-secondary)) ; `C-M-y'
    )
  "*List of minibuffer commands to bind for use in Icicle mode.
These bindings are available in the minibuffer regardless of whether
completion is available.  See `icicle-completion-key-bindings' for
bindings available only during completion.

The option value has the same form as that of option
`icicle-top-level-key-bindings' (which see).
Each list element is of custom type `icicle-key-definition' and has
the form (KEY COMMAND CONDITION).

If you customize this option then you must exit and re-enter Icicle
mode to ensure that the change takes effect.  This is really necessary
only if your changes would undefine a key.

For this option to have an effect upon startup, it must be set before
you enter Icicle mode.  This means that you must ensure that the code
that sets it is invoked before you enter Icicle mode.  If you use
Customize to change this option, then ensure that the code inserted by
Customize into your `user-init-file' or your `custom-file' is invoked
before you enter Icicle mode."
  :type (if (> emacs-major-version 21)
            '(repeat icicle-key-definition)
          '(repeat
            (list
             (choice
              (restricted-sexp :tag "Key"
               :match-alternatives ((lambda (x) (or (stringp x)  (vectorp x)))) :value [ignore])
              (restricted-sexp :tag "Command to remap"
               ;; Use `symbolp' instead of `commandp', in case the library defining the
               ;; command is not loaded.
               :match-alternatives (symbolp) :value ignore))
             ;; Use `symbolp' instead of `commandp'...
             (restricted-sexp :tag "Command"
              :match-alternatives (symbolp) :value ignore)
             (sexp :tag "Condition"))))
  :group 'Icicles-Key-Bindings)

(defcustom icicle-modal-cycle-down-keys ; `down', `wheel-down'
  (if (boundp 'mouse-wheel-down-event)  ; Emacs 22+
      (list [down] (vector nil mouse-wheel-up-event) (vector mouse-wheel-up-event))
    '([down]))
  "*Key sequences to use for modal cycling to the next candidate.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-modal-cycle-down-action-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-modal-cycle-down-action-keys ; `C-down', `C-wheel-down'
  (if (boundp 'mouse-wheel-up-event)    ; Emacs 22+
      (list
       [C-down]
       (vector nil (list 'control mouse-wheel-up-event))
       (vector (list 'control mouse-wheel-up-event)))
    '([C-down]))
  "*Keys for modal completion to cycle next and perform action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-modal-cycle-down-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-modal-cycle-down-alt-action-keys ; `C-S-down', `C-S-wheel-down'
  (if (boundp 'mouse-wheel-up-event)    ;Emacs22+
      (list
       [C-S-down]
       (vector nil (list 'control 'shift mouse-wheel-up-event))
       (vector (list 'control 'shift  mouse-wheel-up-event)))
    '([C-S-down]))
  "*Keys for modal completion to cycle next and perform alt action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-modal-cycle-down-help-keys ; `C-M-down', `C-M-wheel-down'
  (if (boundp 'mouse-wheel-up-event)    ; Emacs 22+
      (list
       [C-M-down]
       (vector nil (list 'control 'meta mouse-wheel-up-event))
       (vector (list 'control 'meta mouse-wheel-up-event)))
    '([C-M-down]))
  "*Keys for modal completion to cycle next and show candidate help.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-modal-cycle-up-keys   ; `up', `wheel-up'
  (if (boundp 'mouse-wheel-down-event)  ; Emacs 22+
      (list
       [up]
       (vector nil mouse-wheel-down-event)
       (vector mouse-wheel-down-event))
    '([up]))
  "*Key sequences to use for modal cycling to the previous candidate.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-modal-cycle-up-action-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-modal-cycle-up-action-keys ; `C-up', `C-wheel-up'
  (if (boundp 'mouse-wheel-down-event)  ; Emacs 22+
      (list
       [C-up]
       (vector nil (list 'control mouse-wheel-down-event))
       (vector (list 'control mouse-wheel-down-event)))
    '([C-up]))
  "*Keys for modal completion to cycle previous and perform action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-modal-cycle-up-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-modal-cycle-up-alt-action-keys ; `C-S-up', `C-S-wheel-up'
  (if (boundp 'mouse-wheel-down-event)  ; Emacs 22+
      (list
       [C-S-up]
       (vector nil (list 'control 'shift mouse-wheel-down-event))
       (vector (list 'control 'shift mouse-wheel-down-event)))
    '([C-S-up]))
  "*Keys for modal completion to cycle previous and perform alt action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-modal-cycle-up-help-keys ; `C-M-up', `C-M-wheel-up'
  (if (boundp 'mouse-wheel-down-event)  ; Emacs 22+
      (list
       [C-M-up]
       (vector nil (list 'control 'meta mouse-wheel-down-event))
       (vector (list 'control 'meta mouse-wheel-down-event)))
    '([C-M-up]))
  "*Keys for modal completion to cycle previous and show candidate help.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-no-match-hook nil
  "*List of hook functions run during completion when there are no matches."
  :type 'hook :group 'Icicles-Miscellaneous)

(defcustom icicle-option-type-prefix-arg-list '(direct inherit inherit-or-value direct-or-value
                                                inherit-or-regexp direct-or-regexp)
  "*Symbols controlling prefix args for `icicle-describe-option-of-type'.
A list of six symbols taken from this list:

  direct            inherit             inherit-or-value
  direct-or-value   inherit-or-regexp   direct-or-regexp

Choose the order you like.  The list members map, in order left to
right, to these prefix argument keys:

 `C-u C-u'           `C-0'            `C-u'
 `C-9' (positive)    no prefix arg    `C--' (negative)

For the meanings of the symbols, see the doc string of
`icicle-describe-option-of-type', which describes the default
prefix-argument bindings for the command."
  :type '(list symbol symbol symbol symbol symbol symbol) :group 'Icicles-Key-Bindings)

(defcustom icicle-network-drive-means-remote-flag t
  "*Non-nil means that a file on a network drive is considered remote.
This pertains to an MS Windows mapped netword drive, such as `f:'.

You can use `C-x :' during completion to toggle this option."
  :type 'boolean :group 'Icicles-Files)

(when (> emacs-major-version 22)        ; Emacs 23+
  (defcustom icicle-populate-interactive-history-flag nil
    "*Non-nil means populate `icicle-interactive-history'.
That means add commands invoked interactively to that history, for use
during completion by `C-M-pause'.

After you change the value of this option, toggle Icicle mode off,
then on again, for the change to take effect in the same session.

Be aware that this history can become quite long.

Furthermore, there is an Emacs bug (#3984) that causes interactiveness
tests (`interactive-p' and `called-interactively-p') to fail, whenever
`call-interactively' is advised (which is how Icicles implements this
feature).

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
    :type 'boolean :group 'Icicles-Miscellaneous))

(defcustom icicle-pp-eval-expression-print-length nil
  "*Value for `print-length' while printing value in `pp-eval-expression'.
A value of nil means no limit.

If the value is an integer and you use Do Re Mi (library `doremi.el')
then you can use multi-command `icicle-increment-option' anytime to
change the option value incrementally."
  :type '(choice
          (const :tag "No Limit" nil)
          (integer :value 5000))
  :group 'Icicles-Miscellaneous)

(defcustom icicle-pp-eval-expression-print-level nil
  "*Value for `print-level' while printing value in `pp-eval-expression'.
A value of nil means no limit.

If the value is an integer and you use Do Re Mi (library `doremi.el')
then you can use multi-command `icicle-increment-option' anytime to
change the option value incrementally."
  :type '(choice
          (const :tag "No Limit" nil)
          (integer :value 8))
  :group 'Icicles-Miscellaneous)

(defcustom icicle-prefix-complete-keys '([?\t]  [tab]  [(control ?i)]) ; `C-i' is `TAB'.
  "*Key sequences to use for `icicle-prefix-complete'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-prefix-complete-no-display-keys '([C-M-tab]) ; `C-M-TAB'
  "*Key sequences to use for `icicle-prefix-complete-no-display'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-prefix-cycle-next-keys '([end]) ; `end'
  "*Key sequences for prefix completion to cycle to the next candidate.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-prefix-cycle-next-action-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-prefix-cycle-next-action-keys '([C-end]) ; `C-end'
  "*Keys for prefix completion to cycle next and perform action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-prefix-cycle-next-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-prefix-cycle-next-alt-action-keys '([C-S-end]) ; `C-S-end'
  "*Keys for prefix completion to cycle next and perform alt action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-prefix-cycle-next-help-keys '([C-M-end]) ; `C-M-end'
  "*Keys for prefix completion to cycle next and show candidate help.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-prefix-cycle-previous-keys '([home]) ; `home'
  "*Key sequences for prefix completion to cycle to the previous candidate.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-prefix-cycle-previous-action-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-prefix-cycle-previous-action-keys '([C-home]) ; `C-home'
  "*Keys for prefix completion to cycle previous and perform action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-prefix-cycle-previous-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-prefix-cycle-previous-alt-action-keys '([C-S-home]) ; `C-S-home'
  "*Keys for prefix completion to cycle previous and perform alt action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-prefix-cycle-previous-help-keys '([C-M-home]) ; `C-M-home'
  "*Keys for prefix completion to cycle previous and show candidate help.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-quote-shell-file-name-flag t
  "*Non-nil means to double-quote the file name that starts a shell command.
This is used by `icicle-read-shell-command-completing'.

If this is nil, then Emacs commands such as `M-!' will not quote a
shell-command file name such as `c:/Program Files/My Dir/mycmd.exe'.
In that case, a shell such as `bash' fails for a shell command such as
`c:/Program Files/My Dir/mycmd.exe arg1 arg2 &', because it interprets
only `c:/Program' as the shell command.  That is, it interprets the
space characters in the file name as separators.  If this is non-nil,
then input such as `c:/Program Files/My Dir/mycmd.exe arg1 arg2 &' is
passed to the shell as
`\"c:/Program Files/My Dir/mycmd.exe\" arg1 arg2 &'.

See the doc string of `icicle-quote-file-name-part-of-cmd' for
information about the characters that, like SPC, lead to quoting.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Miscellaneous)

(when (fboundp 'read-char-by-name)      ; Emacs 23+
  (defcustom icicle-read-char-by-name-multi-completion-flag t
    "*Non-nil means `icicle-read-char-by-name' uses multi-completion.
If nil then a candidate is just as in vanilla Emacs.
If non-nil then it is a 3-part multi-completion: NAME CODE CHAR,
showing three ways to represent the character as text:

* NAME is the Unicode name
* CODE is the Unicode code point, as a hexidecimal numeral
* CHAR is the char itself (as it appears in text, not as an integer)

In addition, if non-nil then properties `help-echo' and
`icicle-mode-line-help' are put on NAME, showing both NAME and the
code point (in hex, octal, and decimal).

Setting this option to nil can speed up reading a character
considerably, but it does not give you the advantages of seeing the
character (WYSIWYG) or matching its code point.

Instead of using a nil value, you can also speed things up by:
* turning off incremental completion
* choosing a strong input pattern, before asking for candidate
  matching."
    :type 'boolean :group 'Icicles-Completions-Display :group 'Icicles-Matching))

(defcustom icicle-read+insert-file-name-keys '([(control meta shift ?f)]) ; `C-M-S-f'
  "*Key sequences to invoke `icicle-read+insert-file-name'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-regexp-quote-flag nil ; Toggle with `C-`'.
  "*Non-nil means special characters in regexps are escaped.
This means that no characters are recognized as special: they match
themselves.  This turns apropos completion into simple substring
completion.  It also turns Icicles searching into literal searching.

You can toggle this option from the minibuffer at any time using
`C-`'.  You can also use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle the option value."
  :type 'boolean :group 'Icicles-Matching)

(defcustom icicle-regexp-search-ring-max (if (boundp 'most-positive-fixnum)
                                             (/ most-positive-fixnum 10)
                                           13421772) ; 1/10 of `most-positive-fixnum' on Windows.
  "*Icicles version of `regexp-search-ring-max'.
If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'integer :group 'Icicles-Searching)

;; You can use `hexrgb-increment-value' in place of `hexrgb-increment-hue', if you prefer highlighting
;; background to be slightly darker instead of a slightly different hue.
;;
(defcustom icicle-region-background
  (if (featurep 'hexrgb)
      (let* ((bg   (or (and (boundp '1on1-active-minibuffer-frame-background)
                            1on1-active-minibuffer-frame-background) ; In `oneonone.el'.
                       (let ((frame-bg  (cdr (assq 'background-color (frame-parameters)))))
                         (when (member frame-bg '(nil unspecified "unspecified-bg"))
                           (setq frame-bg  (if (eq (frame-parameter nil 'background-mode) 'dark)
                                               "Black"
                                             "White")))
                         (and frame-bg  (x-color-defined-p frame-bg)  frame-bg))
                       (face-background 'region)))
             (sat  (condition-case nil (hexrgb-saturation bg) (error nil))))
        (if sat
            (if (hexrgb-approx-equal sat 0.0)
                ;; Grayscale - change bg value slightly.
                (hexrgb-increment-value bg (if (eq (frame-parameter nil 'background-mode) 'dark)
                                               0.20
                                             -0.10))
              (hexrgb-increment-hue bg 0.24)) ; Color - change bg hue slightly.
          (face-background 'region)))
    (face-background 'region))          ; Use normal region background.
  "*Background color to use for the region during minibuffer cycling.
This has no effect if `icicle-change-region-background-flag' is nil.
If you do not define this explicitly, and if you have loaded library
`hexrgb.el' (recommended), then this color will be slightly
different from your frame background.  This still lets you notice the
region, but it makes the region less conspicuous, so you can more
easily read your minibuffer input."
  :type (if (and (require 'wid-edit nil t)  (get 'color 'widget-type)) 'color 'string)
  :group 'Icicles-Minibuffer-Display)

(defcustom icicle-keep-Completions-for-sole-dir 'update-if-showing
  "*What to do when the only match for your input is a directory name.
This applies only to non-absolute file-name completion.

* If `nil' then remove the `*Completions*' window.

* If the symbol `pop-up' then update `*Completions*' unconditionally
  to show the sole candidate.  Show `*Completions*' if it was not yet
  showing.

* If any other non-`nil' value then update `*Completions*' if it is
  showing, and do nothing otherwise.  This is the default behavior."
  :type '(choice
          (const :tag "Remove `*Completions*' unconditionally"            nil)
          (const :tag "Show and update `*Completions*' unconditionally"   pop-up)
          (other :tag "Update `*Completions*' if showing; else remove it" update-if-showing))
  :group 'Icicles-Completions-Display :group 'Icicles-Files)

(defcustom icicle-require-match-flag nil
  "*Control REQUIRE-MATCH arg to `completing-read' and `read-file-name'.
The possible values are as follows:
- nil means this option imposes nothing on completion;
  the REQUIRE-MATCH argument provided to the function governs behavior
- `no-match-required' means the same as a nil value for REQUIRE-MATCH
- `partial-match-ok' means the same as a t value for REQUIRE-MATCH
- `full-match-required' means the same as a non-nil, non-t value for
  REQUIRE-MATCH

Note: This option is provided mainly for use (binding) in
`icicle-define-command' and `icicle-define-file-command'.
You probably do not want to set this globally, but you can."
  :type '(choice
          (const :tag "Do not impose any match behavior"   nil)
          (const :tag "Do not require a match"             no-match-required)
          (const :tag "Require a partial match, with RET"  partial-match-ok)
          (const :tag "Require a full match"               full-match-required))
  :group 'Icicles-Matching)

(defcustom icicle-saved-completion-sets nil
  "*Completion sets available for `icicle-candidate-set-retrieve'.
The form is ((SET-NAME . CACHE-FILE-NAME)...), where SET-NAME is the
name of a set of completion candidates and CACHE-FILE-NAME is the
absolute name of the cache file that contains those candidates.
You normally do not customize this directly, statically.
Instead, you add or remove sets using commands
`icicle-add/update-saved-completion-set' and
`icicle-remove-saved-completion-set'."
  :type '(repeat (cons string file)) :group 'Icicles-Matching)

(defcustom icicle-search-cleanup-flag t
  "*Controls whether to remove highlighting after a search.
If this is nil, highlighting can be removed manually with
`\\[icicle-search-highlight-cleanup]'.

You can toggle this option from the minibuffer during Icicles search
\(e.g., `C-c`') using `C-.'.  You can also use multi-command
`icicle-toggle-option' (`M-i M-i' during completion) anytime to
toggle the option value."
  :type 'boolean :group 'Icicles-Searching)

(defcustom icicle-search-from-isearch-keys (if (> emacs-major-version 23) ; `S-TAB'
                                               '([backtab])
                                             '([S-tab] [S-iso-lefttab]))

  ;; $$$$$ The following should be sufficient, but some Emacs 22+ libraries, such as `info.el',
  ;; are brain-dead and explicitly bind both `backtab' and `S-tab'.  I filed Emacs bug #1281.
  ;;   (if (> emacs-major-version 21)
  ;;       '([backtab])
  ;;     '([S-tab] [S-iso-lefttab]))
  "*Key sequences to use to start `icicle-search' from Isearch.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards - for example, `S-tab' and `S-iso-lefttab'."
  ;; In Emacs 22 and later, `backtab' is the canonical key that represents
  ;; both `S-tab' and `S-iso-lefttab', so that is used in the default
  ;; value.  If, for some reason, `backtab' is not being translated to
  ;; `S-tab' and `S-iso-lefttab' on your platform, you might want to
  ;; customize the value to ([S-tab] [S-iso-lefttab]).  And if your Emacs
  ;; version is 22 or later, please file an Emacs bug about the lack of
  ;; translation.
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-search-highlight-all-current-flag nil ; Toggle with `C-^'.
  "*Non-nil means highlight input match in each context search hit.
Setting this to non-nil can impact performance negatively, because the
highlighting is updated with each input change.

You can toggle this option from the minibuffer during Icicles search
\(e.g., `C-c`') using `C-^'.  You can also use multi-command
`icicle-toggle-option' (`M-i M-i' during completion) anytime to
toggle the option value."
  :type 'boolean :group 'Icicles-Searching)

(defcustom icicle-search-highlight-context-levels-flag t
  "*Non-nil means highlight 1-8 context levels, within the search context.
Level highlighting is done only when this is non-nil and a subgroup is
not used as the search context, that is, the context corresponds to
the entire search regexp.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Searching)

(defcustom icicle-search-highlight-threshold 100000
  "*Max number of context search hits to highlight at once.
If the value is `t' then there is no limit.
This highlighting uses face `icicle-search-main-regexp-others'.

If the value is an integer and you use Do Re Mi (library `doremi.el')
then you can use multi-command `icicle-increment-option' anytime to
change the option value incrementally."
  :type '(choice
          (const    :tag "Highlight all search hits (no limit)" t)
          (integer  :tag "Max number of search hits to highlight" :value 100000))
  :group 'Icicles-Searching)

(defcustom icicle-search-hook nil
  "*List of functions run by `icicle-search' after you visit a search hit.
See `run-hooks'."
  :type 'hook :group 'Icicles-Searching)

(defcustom icicle-search-key-prefix "\M-s\M-s"
  "*Key sequence prefix for keys bound to Icicles search commands.
Has the same form as a key-sequence argument to `define-key'.

This same prefix key sequence, followed by `m', is used in some major
modes for a mode-specific Icicles search command.  E.g., if the prefix
key is `M-s M-s' then `M-s M-s m' is bound in Dired mode to
`icicle-search-dired-marked-recursive', which searches the marked
files."
  :type 'sexp :group 'Icicles-Key-Bindings)

(defcustom icicle-search-replace-common-match-flag t ; Toggle with `M-;'.
  "*Non-nil means to replace the expanded common match of your input.
This has no effect if `icicle-search-highlight-all-current-flag' is
nil or `icicle-expand-input-to-common-match' does not cause expansion.

You can cycle those options from the minibuffer during Icicles search
using `C-^' and `C-M-\"', respectively.  You can toggle
`icicle-search-replace-common-match-flag' using `M-;'.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Searching)

(defcustom icicle-search-replace-literally-flag nil ; Toggle with `C-M-`'.
  "*Non-nil means to treat replacement text literally.
Otherwise (nil), interpret `\\' specially in replacement text, as in
the LITERAL argument to `replace-match'.

You can use `C-M-`' to toggle this at any time during Icicles search.
You can also use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle the option value."
  :type 'boolean :group 'Icicles-Searching)

(defcustom icicle-search-replace-whole-candidate-flag t ; Toggle with `M-_'.
  "*Non-nil means replacement during search replaces the entire search hit.
Otherwise (nil), replace only what matches your current input.

You can use `\\<minibuffer-local-completion-map>\\[icicle-dispatch-M-_]' to \
toggle this at any time during Icicles search.
You can also use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle the option value."
  :type 'boolean :group 'Icicles-Searching)

(defcustom icicle-search-ring-max (if (boundp 'most-positive-fixnum)
                                      (/ most-positive-fixnum 10)
                                    13421772) ; 1/10 of `most-positive-fixnum' on Windows.
  "*Icicles version of `search-ring-max'.
If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'integer :group 'Icicles-Searching)

(defcustom icicle-search-whole-word-flag nil ; Toggle with `M-q'.
  "*Non-nil means that `icicle-search' looks for a whole word.
Whole-word searching here means that matches can contain embedded
strings of non word-constituent chars (they are skipped over, when
matching, included in the match), and any leading or trailing
word-constituent chars in the search string are dropped (ignored for
matching, not included in the match).  This means, for instance, that
you can match `foo-bar' as a word, even in contexts (such as Emacs
Lisp) where `-' is not a word-constituent character.  Similarly, you
can include embedded whitespace in a \"word\", e.g., `foo bar'.

You can use `M-q' to toggle this at any time during Icicles search;
the new value takes effect for the next complete search.  You can also
use multi-command `icicle-toggle-option' anytime (`M-i M-i' during
completion)  to toggle the option value."
  :type 'boolean :group 'Icicles-Searching)

;; Based more or less on `shell-dynamic-complete-as-command'.
;; Must be before `icicle-shell-command-candidates-cache'.
(defun icicle-compute-shell-command-candidates ()
  "*Compute shell command candidates from search path, and return them.
The candidates are the executable files in your search path or, if
`shell-completion-execonly' is nil, all files in your search path."
  (require 'shell)                      ; `shell-completion-execonly'
  (message "Finding commands in search path...")
  (let* ((filenondir         "")
         (path-dirs          (cdr (reverse exec-path)))
         (cwd                (file-name-as-directory (expand-file-name default-directory)))
         (ignored-extensions (and comint-completion-fignore
                                  (mapconcat (lambda (x) (concat (regexp-quote x) "$"))
                                             comint-completion-fignore "\\|")))
         (dir                "")
         (comps-in-dir       ())
         (file               "")
         (abs-file-name      "")
         (completions        ()))
    ;; Go through each dir in the search path, finding completions.
    (while path-dirs
      (setq dir           (file-name-as-directory (comint-directory (or (car path-dirs)  ".")))
            comps-in-dir  (and (file-accessible-directory-p dir)
                               (file-name-all-completions filenondir dir)))
      ;; Go  see whether it should be used.
      (while comps-in-dir
        (setq file           (car comps-in-dir)
              abs-file-name  (concat dir file))
        (when (and (not (member file completions))
                   (not (and ignored-extensions  (string-match ignored-extensions file)))
                   (or (string-equal dir cwd)  (not (file-directory-p abs-file-name)))
                   (or (null shell-completion-execonly)  (file-executable-p abs-file-name)))
          (setq completions  (cons file completions)))
        (setq comps-in-dir  (cdr comps-in-dir)))
      (setq path-dirs  (cdr path-dirs)))
    completions))

(defcustom icicle-shell-command-candidates-cache (and (eq icicle-guess-commands-in-path 'load)
                                                      (icicle-compute-shell-command-candidates))
  "*Cache for shell command candidates.
You typically do not need to customize this option.
It is an option mainly to persist its value.
See `icicle-guess-commands-in-path'."
  :type '(repeat sexp) :group 'Icicles-Miscellaneous)

(defcustom icicle-show-annotations-flag t
  "*Non-nil means display candidate annotations in `*Completions*'.
Annotations are available only in some contexts, and only for some
candidates.  They are highlighted using face `icicle-annotation'.

You can toggle this option from the minibuffer using `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-annotation]'.  You can also use multi-command `icicle-toggle-option'
anytime (`M-i M-i' during completion) to toggle the option value."
  :type 'boolean :group 'Icicles-Completions-Display)

(defcustom icicle-show-Completions-help-flag t
  "*Non-nil means display help lines at the top of buffer `*Completions*'.
A nil value means show only the completion candidates themselves.
But if there are no candidates then say so in `*Completions*'.
Note that vanilla Emacs option `completion-show-help' has no effect in
Icicle mode.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Completions-Display)

(defcustom icicle-show-Completions-initially-flag nil
  "*Non-nil means to show buffer `*Completions*' even without user input.
nil means that `*Completions*' is shown upon demand, via `TAB' or
`S-TAB'.

For an alternative but similar behavior to using non-nil for
`icicle-show-Completions-initially-flag', you can set option
`icicle-incremental-completion' to a value that is neither nil nor t.
That displays buffer `*Completions*' as soon as you type or delete
input, but not initially.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Completions-Display)

(defcustom icicle-show-multi-completion-flag t
  "*Non-nil means to show completion candidates as multi-completions.
This has an effect only where multi-completion is available.
Also, some commands, such as `icicle-locate-file', use a prefix arg to
determine whether to show multi-completions.  Such commands generally
ignore this option.

A typical example of showing multi-completions is adding buffer names
to candidates to show which buffer they are associated with.  Some
commands, such as `icicle-search', append the name of the associated
buffer, highlighted, to the normal completion candidate.  This lets
you easily see which buffer the candidate applies to.  Also, the
buffer name is part of the candidate, so you can match against it.

Note: Even when the option value is nil, you can use `C-M-mouse-2' and
so on to see information about a candidate.  This information
typically includes whatever a non-nil value of the option would have
shown.

You can toggle this option from the minibuffer using `M-m'.  The new
value takes effect after you exit the minibuffer (i.e., for the next
command).  You can also use multi-command `icicle-toggle-option'
anytime (`M-i M-i' during completion) to toggle the option value."
  :type 'boolean :group 'Icicles-Completions-Display)

;; This is similar to `bmkp-sort-comparer'.
(defcustom icicle-sort-comparer 'icicle-case-string-less-p ; Cycle with `C-,'.
  "*Predicate or predicates for sorting (comparing) two items.
Used in particular to sort completion candidates.  In that case, this
determines the order of candidates when cycling and their order in
buffer `*Completions*'.

You can cycle completion sort orders at any time using `C-,' in the
minibuffer.

Although this is a user option, it may be changed by program
locally, for use in particular contexts.  In particular, you can bind
this to nil in an Emacs-Lisp function, to inhibit sorting in that
context.

Various sorting commands change the value of this option dynamically
\(but they do not save the changed value).

The value must be one of the following:

* nil, meaning do not sort

* a predicate that takes two items as args

* a list of the form ((PRED...) FINAL-PRED), where each PRED and
  FINAL-PRED are binary predicates

If the value is a non-empty list, then each PRED is tried in turn
until one returns a non-nil value.  In that case, the result is the
car of that value.  If no non-nil value is returned by any PRED, then
FINAL-PRED is used and its value is the result.

Each PRED should return `(t)' for true, `(nil)' for false, or nil for
undecided.  A nil value means that the next PRED decides (or
FINAL-PRED, if there is no next PRED).

Thus, a PRED is a special kind of predicate that indicates either a
boolean value (as a singleton list) or \"I cannot decide - let the
next guy else decide\".  (Essentially, each PRED is a hook function
that is run using `run-hook-with-args-until-success'.)

Examples:

 nil           - No sorting.

 string-lessp  - Single predicate that returns nil or non-nil.

 ((p1 p2))     - Two predicates `p1' and `p2', which each return
                 (t) for true, (nil) for false, or nil for undecided.

 ((p1 p2) string-lessp)
               - Same as previous, except if both `p1' and `p2' return
                 nil, then the return value of `string-lessp' is used.

Note that these two values are generally equivalent, in terms of their
effect (*):

 ((p1 p2))
 ((p1) p2-plain) where p2-plain is (icicle-make-plain-predicate p2)

Likewise, these three values generally act equivalently:

 ((p1))
 (() p1-plain)
 p1-plain        where p1-plain is (icicle-make-plain-predicate p1)

The PRED form lets you easily combine predicates: use `p1' unless it
cannot decide, in which case try `p2', and so on.  The value ((p2 p1))
tries the predicates in the opposite order: first `p2', then `p1' if
`p2' returns nil.

Using a single predicate or FINAL-PRED makes it easy to reuse an
existing predicate that returns nil or non-nil.

You can also convert a PRED-type predicate (which returns (t), (nil),
or nil) into an ordinary predicate, by using function
`icicle-make-plain-predicate'.  That lets you reuse elsewhere, as
ordinary predicates, any PRED-type predicates you define.

Note: As a convention, predefined Icicles PRED-type predicate names
have the suffix `-cp' (for \"component predicate\") instead of `-p'."
  ;; We don't bother to define a `icicle-reverse-multi-sort-order'
  ;; analogous to `bmkp-reverse-multi-sort-order'.  If we did, the doc
  ;; string would need to be updated to say what the doc string of
  ;; `bmkp-sort-comparer' says about `bmkp-reverse-multi-sort-order'.
  :type '(choice
          (const    :tag "None (do not sort)" nil)
          (function :tag "Sorting Predicate" :value icicle-case-string-less-p)
          (list     :tag "Sorting Multi-Predicate"
           (repeat (function :tag "Component Predicate"))
           (choice
            (const    :tag "None" nil)
            (function :tag "Final Predicate" :value icicle-case-string-less-p))))
  :group 'Icicles-Matching :group 'Icicles-Completions-Display)

(defcustom icicle-sorting-max-candidates 10000
  "*Automatically turn off completion sorting when there are more candidates.
If not an integer, the value must be nil, meaning do not automatically
turn off sorting.

If sorting was enabled before initiating completion then it is
reenabled when the number of candidates falls below this option
value."
  :type '(choice
          (integer :tag "Max number of completion candidates before inhibiting sorting" :value 1000)
          (const :tag "No maximum - do not automatically inhibit candidate sorting"))
  :group 'Icicles-Miscellaneous)

(defcustom icicle-buffer-configs
  `(("All" nil nil nil nil ,icicle-sort-comparer)
    ("Files" nil nil (lambda (bufname) (buffer-file-name (get-buffer bufname))) nil
     ,icicle-sort-comparer)
    ("Files and Scratch" nil nil (lambda (bufname) (buffer-file-name (get-buffer bufname)))
     ("*scratch*") ,icicle-sort-comparer)
    ("All, *...* Buffers Last" nil nil nil nil icicle-buffer-sort-*...*-last))
  "*List of option configurations available for `icicle-buffer-config'.
The form is (CONFIG...), where CONFIG is a list of these items:

 - Configuration name                    (string)
 - `icicle-buffer-match-regexp' value    (regexp string)
 - `icicle-buffer-no-match-regexp' value (regexp string)
 - `icicle-buffer-predicate' value       (function)
 - `icicle-buffer-extras' value          (list of strings)
 - `icicle-buffer-sort' value            (function)

A configuration describes which buffer names are displayed during
completion and their order."
  :type '(repeat (list
                  (string :tag "Configuration name") ; Configuration name
                  (choice (const :tag "None" nil) (regexp :tag "Match regexp" :value "^[^ ]"))
                  (choice (const :tag "None" nil) (regexp :tag "No-match regexp" :value "^[ ]"))
                  (choice
                   (const :tag "None" nil)
                   (function :tag "Predicate"
                    :value (lambda (bufname) (buffer-file-name (get-buffer bufname)))))
                  (choice
                   (const :tag "None" nil)
                   (repeat :tag "Extra buffers" (string :tag "Extra buffer" :value "*scratch*")))
                  (choice
                   (const :tag "None" nil)
                   (function :tag "Sort function" :value icicle-buffer-sort-*...*-last))))
  :group 'Icicles-Buffers)

(defun icicle-buffer-sort-*...*-last (buf1 buf2)
  "Return non-nil if BUF1 is `string<' BUF2 or only BUF2 starts with \"*\"."
  (let ((b1  (if completion-ignore-case (downcase buf1) buf1))
        (b2  (if completion-ignore-case (downcase buf2) buf2)))
    (if (string-match "^\\*" b1)
        (and (string-match "^\\*" b2)  (string< b1 b2))
      (or (string-match "^\\*" b2)  (string< b1 b2)))))

(when (> emacs-major-version 20)
  (defcustom icicle-sort-orders-alist ()
    "*Alist of available sort functions.
This is a pseudo option - you probably do NOT want to customize this.
Instead, use macro `icicle-define-sort-command' to define a new sort
function and automatically add it to this list.

Each alist element has the form (SORT-ORDER . COMPARER):

 SORT-ORDER is a short string or symbol describing the sort order.
 Examples: \"by date\", \"alphabetically\", \"directories first\".

 COMPARER compares two items.  It must be acceptable as a value of
 `icicle-sort-comparer'."
    ;; $$$$$$ Provide default :value for choices?
    :type '(alist
            :key-type (choice :tag "Sort order" string symbol)
            :value-type (choice
                         (const    :tag "None (do not sort)" nil)
                         (function :tag "Sorting Predicate")
                         (list     :tag "Sorting Multi-Predicate"
                          (repeat (function :tag "Component Predicate"))
                          (choice
                           (const    :tag "None" nil)
                           (function :tag "Final Predicate")))))
    :group 'Icicles-Completions-Display :group 'Icicles-Matching))

(unless (> emacs-major-version 20)      ; Emacs 20: custom type `alist' doesn't exist.
  (defcustom icicle-sort-orders-alist ()
    "*Alist of available sort functions.
This is a pseudo option - you probably do NOT want to customize this.
Instead, use macro `icicle-define-sort-command' to define a new sort
function and automatically add it to this list.

Each alist element has the form (SORT-ORDER . COMPARER):

 SORT-ORDER is a short string or symbol describing the sort order.
 Examples: \"by date\", \"alphabetically\", \"directories first\".

 COMPARER compares two items.  It must be acceptable as a value of
 `icicle-sort-comparer'."
    ;; $$$$$$ Provide default :value for choices?
    :type '(repeat
            (cons
             (choice :tag "Sort order" string symbol)
             (choice
              (const    :tag "None (do not sort)" nil)
              (function :tag "Sorting Predicate")
              (list     :tag "Sorting Multi-Predicate"
               (repeat (function :tag "Component Predicate"))
               (choice
                (const    :tag "None" nil)
                (function :tag "Final Predicate"))))))
    :group 'Icicles-Completions-Display :group 'Icicles-Matching))

(defcustom icicle-special-candidate-regexp nil
  "*Regexp to match special completion candidates, or nil to do nothing.
The candidates are highlighted in buffer `*Completions*' using face
`icicle-special-candidate'."
  :type '(choice (const :tag "None" nil) regexp) ; $$$$$$ Default :value?
  :group 'Icicles-Completions-Display)

(defcustom icicle-S-TAB-completion-methods-alist ; Cycle with `M-('.
  `(("apropos" . string-match)
    ("scatter" . icicle-scatter-match)
    ,@(and (require 'fuzzy nil t)       ; `fuzzy.el', part of library Autocomplete.
           '(("Jaro-Winkler" . fuzzy-match)))
    ,@(and (require 'levenshtein nil t)
           '(("Levenshtein" . icicle-levenshtein-match)
             ("Levenshtein strict" . icicle-levenshtein-strict-match))))
  "*Alist of completion methods used by `S-TAB'.
Each element has the form (NAME . FUNCTION), where NAME is a string
name and FUNCTION is the completion match function.  NAME is used in
messages to indicate the type of completion matching.

By default, `S-TAB' is the key for this completion. The actual keys
used are the value of option `icicle-apropos-complete-keys'.

See also options `icicle-TAB-completion-methods' and
`icicle-S-TAB-completion-methods-per-command'."
  :type '(alist
          :key-type   (string :tag "Name used in messages")
          :value-type (symbol :tag "Completion matching function"))
  :group 'Icicles-Matching)

(defcustom icicle-S-TAB-completion-methods-per-command ()
  "*Alist of commands and their available S-TAB completion methods.
Each command is advised so that when invoked only the specified S-TAB
completion methods are available for it when you use `M-('.  (This
makes sense only for commands that read input from the minibuffer.)

This option gives you greater control over which completion methods
are available.  See also option
`icicle-TAB-completion-methods-per-command', which does the same thing
for `TAB' completion.  The default behavior is provided by option
`icicle-S-TAB-completion-methods-alist' (and
`icicle-TAB-completion-methods' for `TAB').

NOTE: If you remove an entry from this list, that does NOT remove the
advice for that command.  To do that you will need to explicitly
invoke command `icicle-set-S-TAB-methods-for-command' using a negative
prefix argument (or else start a new Emacs session)."
  :type (let ((methods  ()))
          (when (require 'levenshtein nil t)
            (push '(const :tag "Levenshtein strict"
                    ("Levenshtein strict" . icicle-levenshtein-strict-match))
                  methods)
            (push '(const :tag "Levenshtein" ("Levenshtein" . icicle-levenshtein-match))
                  methods))
          (when (require 'fuzzy nil t)  ; `fuzzy.el', part of library Autocomplete.
            (push '(const :tag "Jaro-Winkler" ("Jaro-Winkler" . fuzzy-match)) methods))
          (push '(const :tag "scatter" ("scatter" . icicle-scatter-match)) methods)
          (push '(const :tag "apropos" ("apropos" . string-match)) methods)
          `(alist
            :key-type   (restricted-sexp
                         :tag "Command"
                         ;; Use `symbolp' instead of `commandp', in case the library
                         ;; defining the command is not yet loaded.
                         :match-alternatives (symbolp) :value ignore)
            :value-type (repeat :tag "S-TAB completion methods" (choice ,@methods))))
  :set (lambda (sym val)
         (custom-set-default sym val)
         (when (fboundp 'icicle-set-S-TAB-methods-for-command)
           (dolist (entry  val)
             (icicle-set-S-TAB-methods-for-command (car entry) (cdr entry)))))
  :initialize #'custom-initialize-default
  :group 'Icicles-Matching)

(defcustom icicle-swank-prefix-length 1
  "*Length (chars) of symbol prefix that much match, for swank completion.
If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'integer :group 'Icicles-Matching)

(defcustom icicle-swank-timeout 3000
  "*Number of msec before swank (fuzzy symbol) completion gives up.
If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'integer :group 'Icicles-Matching)

(defcustom icicle-TAB-completion-methods ; Cycle with `C-('.
  (let ((methods  ()))
    ;; Unfortunately, `el-swankfuzzy.el' requires `cl.el' at runtime.
    ;; Comment this first line out if you do not want that.
    (when (require 'el-swank-fuzzy nil t) (push 'swank        methods))
    (when (require 'fuzzy-match nil t)    (push 'fuzzy        methods))
    (push 'basic methods)
    (when (boundp 'completion-styles)     (push 'vanilla      methods))
    methods)
  "*List of completion methods to use for \
`\\<minibuffer-local-completion-map>\\[icicle-prefix-complete]'.
The first method in the list is the default method.

The available methods can include these:

1. `vanilla' (provided you have Emacs 23 or later)
2. `basic'
3. `fuzzy'   (provided you have library `fuzzy-match.el')
4. `swank'   (provided you have library `el-swank-fuzzy.el')

1. Vanilla completion respects option `completion-styles' (new in
Emacs 23), so that `TAB' behaves similarly in Icicles to what it does
in vanilla Emacs.  The vanilla method also completes environment
variables during file-name completion and in shell commands.  The
non-vanilla methods do not complete environment variables, but the
variables are expanded to their values when you hit `RET'.

2. Basic completion means ordinary prefix completion. It is the
`basic' completion style of Emacs 23 or later, and it is essentially
the completion style prior to Emacs 23 (Emacs 22 completion was
slightly different - see Emacs 23 option `custom-styles' for more
info).

3. Fuzzy-match completion is a form of prefix completion in which
matching finds the candidates that have the most characters in common
with your input, in the same order, and with a minimum of non-matching
characters.  It can skip over non-matching characters, as long as the
number of characters skipped in the candidate is less that those
following them that match.  After the matching candidates are found,
they are sorted by skip length and then candidate length.

Fuzzy-match completion is described in detail in the commentary of
library `fuzzy-match.el'.  There is no fuzzy-match completion of file
names - fuzzy-match completion is the same as basic for file names.
Fuzzy-match completion is always case-sensitive.

4. Swank completion in Icicles is the same as fuzzy-match completion,
except regarding symbols.  That is, swank completion per se applies
only to symbols.  Symbols are completed using the algorithm of library
`el-swank-fuzzy.el'.

Icicles options `icicle-swank-timeout' and
`icicle-swank-prefix-length' give you some control over the behavior.
When the `TAB' completion method is `swank', you can use `C-x 1'
\(`icicle-doremi-increment-swank-timeout+') and `C-x 2'
\(`icicle-doremi-increment-swank-prefix-length+') in the minibuffer to
increment these options on the fly using the arrow keys `up' and
`down'.

Swank symbol completion uses heuristics that relate to supposedly
typical patterns found in symbol names.  It also uses a timeout that
can limit the number of matches.  It is generally quite a bit slower
than fuzzy-match completion, and it sometimes does not provide all
candidates that you might think should match, even when all of your
input is a prefix (or even when it is already complete!).  If swank
completion produces no match when you think it should, remember that
you can use `\\[icicle-next-TAB-completion-method]' on the fly to \
change the completion method.


If you do not customize `icicle-TAB-completion-methods', then the
default value (that is, the available `TAB' completion methods) will
reflect your current Emacs version and whether you have loaded
libraries `fuzzy-match.el' and `el-swank-fuzzy.el'.

By default, `TAB' is the key for this completion. The actual keys
used are the value of option `icicle-prefix-complete-keys'.

See also options `icicle-TAB-completion-methods-per-command'
`icicle-S-TAB-completion-methods-alist'."
  :type (let ((methods  ()))
          ;; Unfortunately, `el-swankfuzzy.el' requires `cl.el' at runtime.
          ;; Comment this first sexp out if you do not want that.
          (when (require 'el-swank-fuzzy nil t)
            (push '(const :tag "Swank (Fuzzy Symbol)" swank) methods))
          (when (require 'fuzzy-match nil t)
            (push '(const :tag "Fuzzy" fuzzy) methods))
          (when (boundp 'completion-styles)
            (push '(const :tag "Vanilla `completion-styles'" vanilla) methods))
          (push '(const :tag "Basic" basic) methods)
          `(repeat (choice ,@methods)))
  :group 'Icicles-Matching)

(defcustom icicle-TAB-completion-methods-per-command ()
  "*Alist of commands and their available TAB completion methods.
Each command is advised so that when invoked only the specified TAB
completion methods are available for it when you use `C-('.  (This
makes sense only for commands that read input from the minibuffer.)

This option gives you greater control over which completion methods
are available.  See also option
`icicle-S-TAB-completion-methods-per-command', which does the same
thing for `S-TAB' completion.  The default behavior is provided by
option `icicle-TAB-completion-methods' (and
`icicle-S-TAB-completion-methods-alist' for `S-TAB').

NOTE: If you remove an entry from this list, that does NOT remove the
advice for that command.  To do that you will need to explicitly
invoke command `icicle-set-TAB-methods-for-command' using a negative
prefix argument (or else start a new Emacs session)."
  :type (let ((methods  ()))
          ;; Unfortunately, `el-swankfuzzy.el' requires `cl.el' at runtime.
          ;; Comment this first sexp out if you do not want that.
          (when (require 'el-swank-fuzzy nil t)
            (push '(const :tag "Swank (Fuzzy Symbol)" swank) methods))
          (when (require 'fuzzy-match nil t)
            (push '(const :tag "Fuzzy" fuzzy) methods))
          (when (boundp 'completion-styles)
            (push '(const :tag "Vanilla `completion-styles'" vanilla) methods))
          (push '(const :tag "Basic" basic) methods)
          `(alist
            :key-type   (restricted-sexp
                         :tag "Command"
                         ;; Use `symbolp' instead of `commandp', in case the library
                         ;; defining the command is not yet loaded.
                         :match-alternatives (symbolp) :value ignore)
            :value-type (repeat :tag "TAB completion methods" (choice ,@methods))))
  :set (lambda (sym val)
         (when (fboundp 'icicle-set-TAB-methods-for-command)
           (let ((old-val  (symbol-value sym)))
             (dolist (entry  old-val)
               (icicle-set-TAB-methods-for-command (car entry) nil))
             (custom-set-default sym val)
             (dolist (entry  val)
               (icicle-set-TAB-methods-for-command (car entry) (cdr entry))))))
  :initialize #'custom-initialize-default
  :group 'Icicles-Matching)


(defcustom icicle-TAB-shows-candidates-flag t
  "*Non-nil means that `TAB' always shows completion candidates.
Otherwise (nil), follow the standard Emacs behavior of completing to
the longest common prefix, and only displaying the candidates after a
second `TAB'.

Actually, the concerned keys are those defined by option
`icicle-prefix-complete-keys', not necessarily `TAB'.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Key-Bindings)

(defcustom icicle-TAB/S-TAB-only-completes-flag nil
  "Non-nil means keys bound to completion commands do not also cycle.
That is, `TAB' and `S-TAB' perform only completion, not cycling."
  :type 'boolean :group 'Icicles-Key-Bindings)

(defcustom icicle-recenter -4
  "*Argument passed to `recenter' to recenter point in the target window.
Used during functions such as `icicle-search' when the destination to
visit would otherwise be off-screen.

If the value is an integer and you use Do Re Mi (library `doremi.el')
then you can use multi-command `icicle-increment-option' anytime to
change the option value incrementally."
  :type '(choice
	  (const   :tag "To mid-window" nil)
	  (integer :tag "On this line number (negative: from bottom)" :value -4))
  :group 'Icicles-Miscellaneous)

(defcustom icicle-test-for-remote-files-flag t ; Toggle with `C-^'.
  "*Non-nil means Icicles tests for remote file names.
A value of nil turns off all handling of remote file names by Tramp,
including file-name completion.

The testing due to a non-nil value takes a little time, but the test
result saves time with Tramp handling, and it is used to avoid some
other costly operations when a file is determined to be remote.  These
operations are (a) incremental completion and (b) highlighting of the
part of your current input that does not complete.

Use a nil value only when you are sure that the file names you are
completing are local.  The effect will be a slight speed increase for
operations (a) and (b) for local files.

In addition, a nil value has the effect of ignoring the restriction of
input mismatch highlighting to strict completion.  That is, it treats
an `icicle-highlight-input-completion-failure' value of
`explicit-strict' or `implicit-strict' as if it were `implicit'.  The
assumption here is that you use these highlighting values only to
avoid the cost of remote file name completion.

You can toggle this option from the minibuffer using `C-^' (except
during Icicles search).  You can also use multi-command
`icicle-toggle-option' anytime (`M-i M-i' during completion) to toggle
the option value."
  :initialize (lambda (opt-name val) (set opt-name t))
  :set (lambda (opt-name val)
         (or (not (require 'tramp nil t))  (prog1 (set opt-name (not val))
                                             (icicle-toggle-remote-file-testing))))
  :type 'boolean :group 'Icicles-Matching)

(defun icicle-thing-at-point (thing &optional syntax-table)
  "`thingatpt+.el' version of `thing-at-point', if possible.
`tap-thing-at-point' if defined, else `thing-at-point'.
If SYNTAX-TABLE is a syntax table, use it for the duration."
  (if (fboundp 'tap-thing-at-point)
      (tap-thing-at-point thing syntax-table)
    (if (and (syntax-table-p syntax-table)  (fboundp 'with-syntax-table)) ; Emacs 21+.
        (with-syntax-table syntax-table (thing-at-point thing))
      (thing-at-point thing))))         ; Ignore any SYNTAX-TABLE arg for Emacs 20, for vanilla.

(defvar icicle-ffap-max-region-size 1024 ; See also Emacs bug #25243.
  "Max size of active region used to obtain file-name defaults.
An active region larger than this many characters prevents
`icicle-ffap-guesser' from calling `ffap-guesser'.")

(defun icicle-ffap-guesser ()
  "`ffap-guesser', but deactivate a large active region first."
  (and (require 'ffap nil t)
       ;; Prevent using a large active region to guess ffap: Emacs bug #25243.
       (let ((mark-active  (and mark-active  (< (buffer-size) icicle-ffap-max-region-size))))
         (ffap-guesser))))

(defcustom icicle-thing-at-point-functions
  (progn (or (require 'ffap- nil t)  (require 'ffap nil t)) ; Try `ffap-.el' first.
         (cons
          `(,(if (fboundp 'non-nil-symbol-name-nearest-point)
                 'non-nil-symbol-name-nearest-point
                 (lambda () (symbol-name (symbol-at-point))))
            ,(if (fboundp 'word-nearest-point)
                 'word-nearest-point
                 (lambda () (icicle-thing-at-point 'word)))
            ,@(and (fboundp 'list-nearest-point-as-string)  '(list-nearest-point-as-string))
            ,@(and (fboundp 'list-nearest-point-as-string)
                   '((lambda () (list-nearest-point-as-string 2))))
            ,@(and (fboundp 'list-nearest-point-as-string)
                   '((lambda () (list-nearest-point-as-string 3))))
            ,@(and (fboundp 'ffap-guesser)  '(icicle-ffap-guesser))
            thing-at-point-url-at-point)
          'forward-word))
  "*Functions that return a string at or near point, or else nil.
One of the functions is called when you hit `M-.' in the minibuffer.
A cons cell whose car and cdr may each be empty.

The car of the cons cell is a list of functions that grab different
kinds of strings at or near point.  They are used in sequence by
command `icicle-insert-string-at-point' (bound to `M-.').  I recommend
that you also use library `thingatpt+.el', so that `M-.' can take
advantage of the string-grabbing functions it defines.

The cdr of the cons cell is nil or a function that advances point one
text thing.  Each time command `icicle-insert-string-at-point' is
called successively, this is called to grab more things of text (of
the same kind).  By default, successive words are grabbed.

If either the car or cdr is empty, then the other alone determines the
behavior of `icicle-insert-string-at-point'.  Otherwise, option
`icicle-default-thing-insertion' determines whether the car or cdr is
used by `icicle-insert-string-at-point'.  `C-u' with no number
reverses the meaning of `icicle-default-thing-insertion'."
  :type
  '(cons
    (choice
     (repeat
      (function :tag "Function to grab some text at point and insert it in minibuffer"
       :value word-at-point))
     (const :tag "No alternative text-grabbing functions" nil))
    (choice
     (const :tag "No function to successively grab more text" nil)
     (function :tag "Function to advance point one text thing" :value forward-word)))
  :group 'Icicles-Miscellaneous)

;; Must be before `icicle-top-level-key-bindings'.
(defun icicle-remap (old new map &optional oldmap)
  "Bind command NEW in MAP to all keys currently bound to OLD.
If command remapping is available, use that.  Otherwise, bind NEW to
whatever OLD is bound to in MAP, or in OLDMAP, if provided."
  (if (fboundp 'command-remapping)
      (define-key map (vector 'remap old) new) ; Ignore OLDMAP for Emacs 22.
    (substitute-key-definition old new map oldmap)))

;; Must be before `icicle-top-level-key-bindings'.
(defun icicle-bind-top-level-commands (&optional defs)
  "Bind top-level commands for Icicle mode."
  (let ((icicle-mode  (and (boundp 'icicle-mode)  icicle-mode))
        key command condition)
    (unless icicle-mode  (icy-mode 1))  ; Need `icicle-mode-map', which is unbound unless in Icicle mode.
    (unless defs  (setq defs  icicle-top-level-key-bindings))
    (dolist (key-def  defs)
      (setq key        (car key-def)
            command    (cadr key-def)
            condition  (car (cddr key-def)))
      (when (eval condition)
        (if (symbolp key)
            (icicle-remap key command icicle-mode-map (current-global-map))
          (define-key icicle-mode-map key command))))))

;; Must be before `icicle-top-level-key-bindings'.
(defcustom icicle-yank-function 'yank
  "*Yank function.  A function that takes a prefix argument.  This
should be a command that is bound to whatever key you use to yank
text, whether in Icicle mode or not.  In Icicle mode, command
`icicle-yank-maybe-completing' calls this function, except when
`icicle-yank-maybe-completing' is called from the minibuffer or called
with a negative prefix argument.  `icicle-yank-maybe-completing'
passes the raw prefix argument to `icicle-yank-function'.

By default (see option `icicle-top-level-key-bindings'), the command
that is the value of this option is remapped to
`icicle-yank-maybe-completing' the first time you enter Icicle mode.
If you customize `icicle-yank-function', then, to take advantage of
this default remapping behavior, you will need to save your
customization and then restart Emacs.

Alternatively, you can customize both `icicle-yank-function' and the
corresponding entry in `icicle-top-level-key-bindings', and then
toggle Icicle mode off and then back on."
  :type 'function :group 'Icicles-Miscellaneous)

(defcustom icicle-top-level-key-bindings
  `(([pause]                       icicle-switch-to/from-minibuffer    t) ; `pause'
    ("\C-c`"                       icicle-search-generic               t) ; `C-c `'
    ("\C-c$"                       icicle-search-word                  t) ; `C-c $'
    ("\C-c^"                       icicle-search-keywords              t) ; `C-c ^'
    ("\C-c'"                       icicle-occur                        t) ; `C-c ''
    ("\C-c="                       icicle-imenu                        t) ; `C-c ='
    ("\C-c\""                      icicle-search-text-property         t) ; `C-c "'
    ("\C-c/"                       icicle-complete-thesaurus-entry ; `C-c /'
     (fboundp 'icicle-complete-thesaurus-entry))
    ("\C-x\M-e"                    icicle-execute-named-keyboard-macro t) ; `C-x M-e'
    ;; Emacs 25 stole `C-x SPC' for `rectangle-mark-mode'.
    ;; ("\C-x "                    icicle-command-abbrev               t) ; `C-x SPC'
    (,(kbd "ESC C-M-x")            icicle-command-abbrev               t) ; `M-ESC C-x'
                                                                        ;;; =`ESC C-M-x' = `ESC ESC C-x'
    ("\C-x5o"                      icicle-select-frame                 t) ; `C-x 5 o'
    ("\C-h\C-o"                    icicle-describe-option-of-type      t) ; `C-h C-o'
    ,@(and (require 'kmacro nil t)      ; (Emacs 22+)
           '(([S-f4]               icicle-kmacro                       t))) ; `S-f4'
    (abort-recursive-edit          icicle-abort-recursive-edit         t) ; `C-]'
    (apropos                       icicle-apropos                      t)
    (apropos-command               icicle-apropos-command              t) ; `C-h a'
    (apropos-value                 icicle-apropos-value                t)
    (apropos-user-option           icicle-apropos-option
     (and (fboundp 'icicle-apropos-option)  (fboundp 'apropos-user-option))) ; Emacs 24.4+
    (apropos-variable              icicle-apropos-option
     (and (fboundp 'icicle-apropos-option)  (not (fboundp 'apropos-user-option)))) ; Emacs < 24.4
    (apropos-variable              icicle-apropos-variable ; Emacs < 24.4 and not `apropos-fn+var.el'
     (not (fboundp 'icicle-apropos-option)))
    (apropos-zippy                 icicle-apropos-zippy                t)
    (bookmark-jump                 icicle-bookmark
     t)                                 ; `C-x j j', `C-x p b', `C-x r b'
    (bookmark-jump-other-window    icicle-bookmark-other-window
     t)                                 ; `C-x 4 j j', `C-x p j', `C-x p o', `C-x p q'
    (bmkp-bookmark-set-confirm-overwrite  icicle-bookmark-cmd
     (fboundp 'bmkp-bookmark-set-confirm-overwrite))                      ; `C-x r m'
    (bookmark-set                  icicle-bookmark-cmd                 t) ; `C-x r m'
    (customize-apropos             icicle-customize-apropos            t)
    (customize-apropos-faces       icicle-customize-apropos-faces      t)
    (customize-apropos-groups      icicle-customize-apropos-groups     t)
    (customize-apropos-options     icicle-customize-apropos-options    t)
    (customize-face                icicle-customize-face               t)
    (customize-face-other-window   icicle-customize-face-other-window  t)
    (dabbrev-completion            icicle-dabbrev-completion           t)
    ;; Overrides previous for `C-M-/', but not for any other keys.
    ([?\C-\M-/]                    icicle-dispatch-C-M-/               t) ; `C-M-/'
    (delete-window                 icicle-delete-window                t) ; `C-x 0'
    (delete-windows-for            icicle-delete-window                t) ; `C-x 0' (`frame-cmds.el')

    (describe-package              icicle-describe-package                ; `C-h P'
     (fboundp 'describe-package))
    (dired                         icicle-dired
     (not (featurep 'dired+)))                                            ; `C-x d'
    (dired-other-window            icicle-dired-other-window
     (not (featurep 'dired+)))                                            ; `C-x 4 d'
    (exchange-point-and-mark       icicle-exchange-point-and-mark      t) ; `C-x C-x'
    (execute-extended-command      icicle-execute-extended-command     t) ; `M-x'
    (find-file                     icicle-file                         t) ; `C-x C-f'
    (find-file-other-window        icicle-file-other-window            t) ; `C-x 4 f'
    (find-file-read-only           icicle-find-file-read-only          t) ; `C-x C-r'
    (find-file-read-only-other-window
     icicle-find-file-read-only-other-window                           t) ; `C-x 4 r'
    ;; There are no key bindings in vanilla Emacs for `insert-buffer'.
    ;; If you use `setup-keys.el', then these are its bindings: `C-S-insert', `M-S-f1'.
    (insert-buffer                 icicle-insert-buffer                t) ; `C-S-insert'
    (kill-buffer                   icicle-kill-buffer                  t) ; `C-x k'
    (kill-buffer-and-its-windows   icicle-kill-buffer                  t) ; `C-x k' (`misc-cmds.el')
    (load-library                  icicle-load-library                 (> emacs-major-version 20))
    (minibuffer-keyboard-quit      icicle-abort-recursive-edit ; `C-g' (minibuffer - `delsel.el')
     (fboundp 'minibuffer-keyboard-quit))
    (other-window                  icicle-other-window-or-frame        t) ; `C-x o'
    (other-window-or-frame         icicle-other-window-or-frame        t) ; `C-x o' (`frame-cmds.el')
    (pop-global-mark
     icicle-goto-global-marker-or-pop-global-mark                      t) ; `C-x C-@', `C-x C-SPC'
    (repeat-complex-command        icicle-repeat-complex-command       t) ; `C-x M-:', `C-x M-ESC'
    (set-mark-command
     icicle-goto-marker-or-set-mark-command                            t) ; `C-@', `C-SPC'
    (switch-to-buffer              icicle-buffer                       t) ; `C-x b'
    (switch-to-buffer-other-window icicle-buffer-other-window          t) ; `C-x 4 b'
    ;; You might want to use these two instead of the previous two.
    ;; (switch-to-buffer              icicle-buffer-no-search               t)
    ;; (switch-to-buffer-other-window icicle-buffer-no-search-other-window  t)
    (where-is                      icicle-where-is                     t) ; `C-h w'
    (,icicle-yank-function         icicle-yank-maybe-completing        t) ; `C-y'
    (yank-pop                      icicle-yank-pop-commands            (featurep 'second-sel)) ; `M-y'
    (yank-pop-commands             icicle-yank-pop-commands            (featurep 'second-sel)) ; `M-y'
    (zap-to-char                   icicle-zap-to-char (fboundp 'read-char-by-name)) ; `M-z' (Emacs 23+)

    ;; The following are available only if you use library `bookmark+.el'.

    ;; Bookmark `read-file-name' autofile commands
    (bmkp-autofile-set icicle-bookmark-a-file  (fboundp 'bmkp-bookmark-a-file))        ; `C-x p c a'
    (bmkp-tag-a-file icicle-tag-a-file         (fboundp 'bmkp-tag-a-file))             ; `C-x p t + a'
    (bmkp-untag-a-file icicle-untag-a-file     (fboundp 'bmkp-untag-a-file))           ; `C-x p t - a'

    ;; Bookmark jump commands
    (bmkp-find-file
     icicle-find-file-handle-bookmark (fboundp 'bmkp-find-file))                       ; `C-x j C-f'
    (bmkp-find-file-other-window
     icicle-find-file-handle-bookmark-other-window
     (fboundp 'bmkp-find-file-other-window))                                           ; `C-x 4 j C-f'
    (bmkp-autofile-jump icicle-bookmark-autofile (fboundp 'bmkp-autofile-jump))        ; `C-x j a'
    (bmkp-autofile-jump-other-window
     icicle-bookmark-autofile-other-window (fboundp 'bmkp-autofile-jump))              ; `C-x 4 j a'
    (bmkp-autonamed-jump icicle-bookmark-autonamed (fboundp 'bmkp-autonamed-jump))     ; `C-x j #'
    (bmkp-autonamed-jump-other-window
     icicle-bookmark-autonamed-other-window (fboundp 'bmkp-autonamed-jump))            ; `C-x 4 j #'
    (bmkp-autonamed-this-buffer-jump
     icicle-bookmark-autonamed-this-buffer (fboundp 'bmkp-autonamed-this-buffer-jump)) ; `C-x j , #'
    ;;     (bmkp-autonamed-this-buffer-jump-other-window
    ;;      icicle-bookmark-autonamed-this-buffer-other-window
    ;;      (fboundp 'bmkp-autonamed-jump-this-buffer-other-window)) ; `C-x 4 j , #'
    (bmkp-bookmark-file-jump            ;   (Other-window means nothing for a bookmark file.)
     icicle-bookmark-bookmark-file (fboundp 'bmkp-bookmark-file-jump))                 ; `C-x j y'
    ;;   (Other-window means nothing for a bookmark list.)
    (bmkp-bookmark-list-jump
     icicle-bookmark-bookmark-list (fboundp 'bmkp-bookmark-list-jump))                 ; `C-x j B'
    ;;   (Other-window means nothing for a desktop.)
    (bmkp-desktop-jump icicle-bookmark-desktop (fboundp 'bmkp-desktop-jump))           ; `C-x j K'
    (bmkp-dired-jump icicle-bookmark-dired (fboundp 'bmkp-dired-jump))                 ; `C-x j d'
    (bmkp-dired-jump-other-window
     icicle-bookmark-dired-other-window (fboundp 'bmkp-dired-jump))                    ; `C-x 4 j d'
    (bmkp-file-jump icicle-bookmark-file (fboundp 'bmkp-file-jump))                    ; `C-x j f'
    (bmkp-file-jump-other-window
     icicle-bookmark-file-other-window (fboundp 'bmkp-file-jump))                      ; `C-x 4 j f'
    (bmkp-file-this-dir-jump
     icicle-bookmark-file-this-dir (fboundp 'bmkp-file-this-dir-jump))                 ; `C-x j . f'
    (bmkp-file-this-dir-jump-other-window
     icicle-bookmark-file-this-dir-other-window (fboundp 'bmkp-file-this-dir-jump))    ; `C-x 4 j . f'
    (bmkp-gnus-jump icicle-bookmark-gnus (fboundp 'bmkp-gnus-jump))                    ; `C-x j g'
    (bmkp-gnus-jump-other-window
     icicle-bookmark-gnus-other-window (fboundp 'bmkp-gnus-jump))                      ; `C-x 4 j g'
    (bmkp-image-jump icicle-bookmark-image (fboundp 'bmkp-image-jump))                 ; `C-x j M-i'
    (bmkp-image-jump-other-window
     icicle-bookmark-image-other-window (fboundp 'bmkp-image-jump))                    ; `C-x 4 j M-i'
    (bmkp-info-jump icicle-bookmark-info (fboundp 'bmkp-info-jump))                    ; `C-x j i'
    (bmkp-info-jump-other-window
     icicle-bookmark-info-other-window (fboundp 'bmkp-info-jump))                      ; `C-x 4 j i'
    (bmkp-local-file-jump icicle-bookmark-local-file (fboundp 'bmkp-local-file-jump))  ; `C-x j l'
    (bmkp-local-file-jump-other-window
     icicle-bookmark-local-file-other-window (fboundp 'bmkp-local-file-jump))          ; `C-x 4 j l'
;;;     (bmkp-local-non-dir-file-jump icicle-bookmark-local-non-dir-file
;;;      (fboundp 'bmkp-local-non-dir-file-jump))                                      ; Not bound
;;;     (bmkp-local-non-dir-file-jump-other-window
;;;      icicle-bookmark-local-non-dir-file-other-window
;;;      (fboundp 'bmkp-local-non-dir-file-jump))                                      ; Not bound
    (bmkp-man-jump icicle-bookmark-man  (fboundp 'bmkp-man-jump))                      ; `C-x j m'
    (bmkp-man-jump-other-window icicle-bookmark-man-other-window  (fboundp 'bmkp-man-jump)) ; `C-x 4 j m'
;;;     (bmkp-non-dir-file-jump icicle-bookmark-non-dir-file
;;;      (fboundp 'bmkp-non-dir-file-jump))                                            ; Not bound
;;;     (bmkp-non-dir-file-jump-other-window icicle-bookmark-non-dir-file-other-window
;;;      (fboundp 'bmkp-non-dir-file-jump))                                            ; Not bound
    (bmkp-non-file-jump icicle-bookmark-non-file (fboundp 'bmkp-non-file-jump))        ; `C-x j b'
    (bmkp-non-file-jump-other-window
     icicle-bookmark-non-file-other-window (fboundp 'bmkp-non-file-jump))              ; `C-x 4 j b'
    (bmkp-region-jump icicle-bookmark-region (fboundp 'bmkp-region-jump))              ; `C-x j r'
    (bmkp-region-jump-other-window
     icicle-bookmark-region-other-window (fboundp 'bmkp-region-jump))                  ; `C-x 4 j r'
    (bmkp-remote-file-jump icicle-bookmark-remote-file (fboundp 'bmkp-remote-file-jump)) ; `C-x j n'
    (bmkp-remote-file-jump-other-window
     icicle-bookmark-remote-file-other-window (fboundp 'bmkp-remote-file-jump))        ; `C-x 4 j n'
;;;     (bmkp-remote-non-dir-file-jump icicle-bookmark-remote-non-dir-file
;;;      (fboundp 'bmkp-remote-non-dir-file-jump))                                     ; Not bound
;;;     (bmkp-remote-non-dir-file-jump-other-window
;;;      icicle-bookmark-remote-non-dir-file-other-window
;;;      (fboundp 'bmkp-remote-non-dir-file-jump))                                     ; Not bound
    (bmkp-specific-buffers-jump
     icicle-bookmark-specific-buffers (fboundp 'bmkp-specific-buffers-jump))           ; `C-x j = b'
    (bmkp-specific-buffers-jump-other-window
     icicle-bookmark-specific-buffers-other-window (fboundp 'bmkp-specific-buffers-jump)) ; `C-x 4 j = b'
    (bmkp-specific-files-jump
     icicle-bookmark-specific-files (fboundp 'bmkp-specific-files-jump))               ; `C-x j = f'
    (bmkp-specific-files-jump-other-window
     icicle-bookmark-specific-files-other-window (fboundp 'bmkp-specific-files-jump))  ; `C-x 4 j = f'
    (bmkp-temporary-jump icicle-bookmark-temporary (fboundp 'bmkp-temporary-jump))     ; `C-x j x'
    (bmkp-temporary-jump-other-window
     icicle-bookmark-temporary-other-window (fboundp 'bmkp-temporary-jump))            ; `C-x 4 j x'
    (bmkp-this-buffer-jump icicle-bookmark-this-buffer (fboundp 'bmkp-this-buffer-jump)) ; `C-x j , ,'
    (bmkp-this-buffer-jump-other-window
     icicle-bookmark-this-buffer-other-window (fboundp 'bmkp-this-buffer-jump))        ; `C-x 4 j , ,'
    (bmkp-url-jump icicle-bookmark-url (fboundp 'bmkp-url-jump))                       ; `C-x j u'
    (bmkp-url-jump-other-window icicle-bookmark-url-other-window (fboundp 'bmkp-url-jump)) ; `C-x 4 j u'
    (bmkp-w3m-jump icicle-bookmark-w3m (fboundp 'bmkp-w3m-jump))                       ; `C-x j w'
    (bmkp-w3m-jump-other-window icicle-bookmark-w3m-other-window (fboundp 'bmkp-w3m-jump)) ; `C-x 4 j w'

    ;; Bookmark tags jump commands
    ("\C-xjtj"                     icicle-bookmark-tagged                           ; `C-x j t j'
     (featurep 'bookmark+))
    ("\C-x4jtj"                    icicle-bookmark-tagged-other-window              ; `C-x 4 j t j'
     (featurep 'bookmark+))
    ("\C-xjt\C-f\C-f"              icicle-find-file-tagged                          ; `C-x j t C-f C-f'
     (featurep 'bookmark+))
    ("\C-x4jt\C-f\C-f"             icicle-find-file-tagged-other-window             ; `C-x 4 j t C-f C-f'
     (featurep 'bookmark+))
    (bmkp-find-file-all-tags
     icicle-find-file-all-tags (fboundp 'bmkp-find-file-all-tags))                  ; `C-x j t C-f *'
    (bmkp-find-file-all-tags-other-window
     icicle-find-file-all-tags-other-window (fboundp 'bmkp-find-file-all-tags))     ; `C-x 4 j t C-f *'
    (bmkp-find-file-all-tags-regexp
     icicle-find-file-all-tags-regexp
     (fboundp 'bmkp-find-file-all-tags-regexp))                                     ; `C-x j t C-f % *'
    (bmkp-find-file-all-tags-regexp-other-window
     icicle-find-file-all-tags-regexp-other-window
     (fboundp 'bmkp-find-file-all-tags-regexp-other-window))                        ; `C-x 4 j t C-f % *'
    (bmkp-find-file-some-tags
     icicle-find-file-some-tags (fboundp 'bmkp-find-file-some-tags))                ; `C-x j t C-f +'
    (bmkp-find-file-some-tags-other-window
     icicle-find-file-some-tags-other-window
     (fboundp 'bmkp-find-file-some-tags-other-window))                               ; `C-x 4 j t C-f +'
    (bmkp-find-file-some-tags-regexp
     icicle-find-file-some-tags-regexp (fboundp 'bmkp-find-file-some-tags-regexp))   ; `C-x j t C-f % +'
    (bmkp-find-file-some-tags-regexp-other-window
     icicle-find-file-some-tags-regexp-other-window
     (fboundp 'bmkp-find-file-some-tags-regexp-other-window))                       ; `C-x 4 j t C-f % +'
    (bmkp-autofile-all-tags-jump
     icicle-bookmark-autofile-all-tags (fboundp 'bmkp-autofile-all-tags-jump))      ; `C-x j t a *'
    (bmkp-autofile-all-tags-jump-other-window
     icicle-bookmark-autofile-all-tags-other-window
     (fboundp 'bmkp-autofile-all-tags-jump))                                        ; `C-x 4 j t a *'
    (bmkp-autofile-all-tags-regexp-jump
     icicle-bookmark-autofile-all-tags-regexp
     (fboundp 'bmkp-autofile-all-tags-regexp-jump))                                 ; `C-x j t a % *'
    (bmkp-autofile-all-tags-regexp-jump-other-window
     icicle-bookmark-autofile-all-tags-regexp-other-window
     (fboundp 'bmkp-autofile-all-tags-regexp-jump))                                 ; `C-x 4 j t a % *'
    (bmkp-autofile-some-tags-jump
     icicle-bookmark-autofile-some-tags (fboundp 'bmkp-autofile-some-tags-jump))    ; `C-x j t a +'
    (bmkp-autofile-some-tags-jump-other-window
     icicle-bookmark-autofile-some-tags-other-window
     (fboundp 'bmkp-autofile-some-tags-jump))                                       ; `C-x 4 j t a +'
    (bmkp-autofile-some-tags-regexp-jump
     icicle-bookmark-autofile-some-tags-regexp
     (fboundp 'bmkp-autofile-some-tags-regexp-jump))                                ; `C-x j t a % +'
    (bmkp-autofile-some-tags-regexp-jump-other-window
     icicle-bookmark-autofile-some-tags-regexp-other-window
     (fboundp 'bmkp-autofile-some-tags-regexp-jump))                                ; `C-x 4 j t a % +'
    (bmkp-all-tags-jump icicle-bookmark-all-tags (fboundp 'bmkp-all-tags-jump))     ; `C-x j t *'
    (bmkp-all-tags-jump-other-window
     icicle-bookmark-all-tags-other-window (fboundp 'bmkp-all-tags-jump))           ; `C-x 4 j t *'
    (bmkp-all-tags-regexp-jump
     icicle-bookmark-all-tags-regexp (fboundp 'bmkp-all-tags-regexp-jump))          ; `C-x j t % *'
    (bmkp-all-tags-regexp-jump-other-window
     icicle-bookmark-all-tags-regexp-other-window (fboundp 'bmkp-all-tags-regexp-jump)) ; `C-x 4 j t % *'
    (bmkp-some-tags-jump icicle-bookmark-some-tags (fboundp 'bmkp-some-tags-jump))  ; `C-x j t +'
    (bmkp-some-tags-jump-other-window
     icicle-bookmark-some-tags-other-window (fboundp 'bmkp-some-tags-jump))         ; `C-x 4 j t +'
    (bmkp-some-tags-regexp-jump
     icicle-bookmark-some-tags-regexp (fboundp 'bmkp-some-tags-regexp-jump))        ; `C-x j t % +'
    (bmkp-some-tags-regexp-jump-other-window
     icicle-bookmark-some-tags-regexp-other-window
     (fboundp 'bmkp-some-tags-regexp-jump))                                         ; `C-x 4 j t % +'
    (bmkp-file-all-tags-jump
     icicle-bookmark-file-all-tags (fboundp 'bmkp-file-all-tags-jump))              ; `C-x j t f *'
    (bmkp-file-all-tags-jump-other-window
     icicle-bookmark-file-all-tags-other-window (fboundp 'bmkp-file-all-tags-jump)) ; `C-x 4 j t f *'
    (bmkp-file-all-tags-regexp-jump
     icicle-bookmark-file-all-tags-regexp (fboundp 'bmkp-file-all-tags-regexp-jump)) ; `C-x j t f % *'
    (bmkp-file-all-tags-regexp-jump-other-window
     icicle-bookmark-file-all-tags-regexp-other-window
     (fboundp 'bmkp-file-all-tags-regexp-jump))                                     ; `C-x 4 j t f % *'
    (bmkp-file-some-tags-jump
     icicle-bookmark-file-some-tags (fboundp 'bmkp-file-some-tags-jump))            ; `C-x j t f +'
    (bmkp-file-some-tags-jump-other-window
     icicle-bookmark-file-some-tags-other-window (fboundp 'bmkp-file-some-tags-jump)) ; `C-x 4 j t f +'
    (bmkp-file-some-tags-regexp-jump
     icicle-bookmark-file-some-tags-regexp (fboundp 'bmkp-file-some-tags-regexp-jump)) ; `C-x j t f % +'
    (bmkp-file-some-tags-regexp-jump-other-window
     icicle-bookmark-file-some-tags-regexp-other-window
     (fboundp 'bmkp-file-some-tags-regexp-jump))                                    ; `C-x 4 j t f % +'
    (bmkp-file-this-dir-all-tags-jump
     icicle-bookmark-file-this-dir-all-tags
     (fboundp 'bmkp-file-this-dir-all-tags-jump))                                   ; `C-x j t . *'
    (bmkp-file-this-dir-all-tags-jump-other-window
     icicle-bookmark-file-this-dir-all-tags-other-window
     (fboundp 'bmkp-file-this-dir-all-tags-jump))                                   ; `C-x 4 j t . *'
    (bmkp-file-this-dir-all-tags-regexp-jump
     icicle-bookmark-file-this-dir-all-tags-regexp                                  ; `C-x j t . % *'
     (fboundp 'bmkp-file-this-dir-all-tags-regexp-jump))
    (bmkp-file-this-dir-all-tags-regexp-jump-other-window
     icicle-bookmark-file-this-dir-all-tags-regexp-other-window
     (fboundp 'bmkp-file-this-dir-all-tags-regexp-jump))                            ; `C-x 4 j t . % *'
    (bmkp-file-this-dir-some-tags-jump
     icicle-bookmark-file-this-dir-some-tags
     (fboundp 'bmkp-file-this-dir-some-tags-jump))                                  ; `C-x j t . +'
    (bmkp-file-this-dir-some-tags-jump-other-window
     icicle-bookmark-file-this-dir-some-tags-other-window
     (fboundp 'bmkp-file-this-dir-some-tags-jump))                                  ; `C-x 4 j t . +'
    (bmkp-file-this-dir-some-tags-regexp-jump
     icicle-bookmark-file-this-dir-some-tags-regexp
     (fboundp 'bmkp-file-this-dir-some-tags-regexp-jump))                           ; `C-x j t . % +'
    (bmkp-file-this-dir-some-tags-regexp-jump-other-window
     icicle-bookmark-file-this-dir-some-tags-regexp-other-window
     (fboundp 'bmkp-file-this-dir-some-tags-regexp-jump))                           ; `C-x 4 j t . % +'

    ;; Don't let Emacs 20 or 21 use `substitute-key-definition' on `M-.' or `M-*', since we need
    ;; these keys for the minibuffer.  Leave them unbound in `icicle-mode-map' until Emacs 22+.
    (find-tag                      icicle-find-tag              (fboundp 'command-remapping)) ; `M-.'
    (find-tag-other-window         icicle-find-first-tag-other-window  t)           ; `C-x 4 .'
    (pop-tag-mark        icicle-pop-tag-mark          (fboundp 'command-remapping)) ; `M-*'
    (eval-expression     icicle-pp-eval-expression    (fboundp 'command-remapping)) ; `M-:'
    (pp-eval-expression  icicle-pp-eval-expression    (fboundp 'command-remapping)) ;`M-:' (`pp+.el')
    ([S-f10]             icicle-complete-menu-bar     (fboundp 'icicle-complete-menu-bar)) ; `S-f10'
    ;; For La Carte (`lacarte.el'), not Icicles, but it's convenient to do this here.
    ("\e\M-x"            lacarte-execute-command                                    ; `ESC M-x'
     (fboundp 'lacarte-execute-command))
    ("\M-`"                        lacarte-execute-menu-command ; `M-`' - replaces `tmm-menubar'.
     (fboundp 'lacarte-execute-menu-command))
    ([f10]                         lacarte-execute-menu-command ; `f10' - replaces `menu-bar-open'.
     (fboundp 'lacarte-execute-menu-command))
    )
  "*List of top-level key bindings in Icicle mode.
Each list element is of custom type `icicle-key-definition' and has
the form (KEY COMMAND CONDITION).

KEY is either a key sequence (string or vector) to bind COMMAND to or
a command to remap to COMMAND.
COMMAND is bound according to the value of KEY, unless the result of
evaluating CONDITION is nil.

In Customize, to specify a key sequence, choose `Key' in the `Value
Menu', then enter a key description such as that returned by `C-h k'.
For convenience, you can insert each key in the key description by
hitting `C-q' then the key.  For example, to enter the key description
`C-c M-k' you can use `C-q C-c C-q M-k'.

If you customize this option then you must exit and re-enter Icicle
mode to ensure that the change takes effect.  This is really necessary
only if your changes would undefine a key.

For this option to have an effect upon startup, it must be set before
you enter Icicle mode.  This means that you must ensure that the code
that sets it is invoked before you enter Icicle mode.  If you use
Customize to change this option, then ensure that the code inserted by
Customize into your `user-init-file' or your `custom-file' is invoked
before you enter Icicle mode.

See also option `icicle-functions-to-redefine'."
  :type (if (> emacs-major-version 21)
            '(repeat icicle-key-definition)
          '(repeat
            (list
             (choice
              (restricted-sexp :tag "Key"
               :match-alternatives ((lambda (x) (or (stringp x)  (vectorp x)))) :value [ignore])
              (restricted-sexp :tag "Command to remap"
               ;; Use `symbolp' instead of `commandp', in case the library defining the
               ;; command is not loaded.
               :match-alternatives (symbolp) :value ignore))
             ;; Use `symbolp' instead of `commandp'...
             (restricted-sexp :tag "Command"
              :match-alternatives (symbolp) :value ignore)
             (sexp :tag "Condition"))))
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (when (boundp 'icicle-mode-map) ; Avoid error on initialization.
           (icicle-bind-top-level-commands defs)))
  :initialize #'custom-initialize-default
  :group 'Icicles-Key-Bindings)

(defcustom icicle-top-level-when-sole-completion-delay 1.0
  "*Number of secs to wait to return to top level if only one completion.
This has no effect if `icicle-top-level-when-sole-completion-flag' is
nil.  Editing the completion (typing or deleting a character) before
the delay expires prevents its automatic acceptance.

Do not set this to 0.0.  Set it to slightly more than zero if you want
instant action.

If you use Do Re Mi (library `doremi.el') then you can use
multi-command `icicle-increment-option' anytime to change the option
value incrementally."
  :type 'number :group 'Icicles-Matching)

(defcustom icicle-top-level-when-sole-completion-flag nil
  "*Non-nil means to return to top level if only one matching completion.
The sole completion is accepted.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Matching)

(defcustom icicle-touche-pas-aux-menus-flag nil
  "*Non-nil means do not add items to menus except `Minibuf' and `Icicles'.
Put differently, non-nil means that Icicles menu items are
consolidated in a single menu: `Icicles'.  Otherwise (if nil), they
are instead placed in relevant existing menus: `File', `Search', etc.

So if you want all Icicles menu items in the same place, set this to
non-nil.

The option value is used only when Icicles mode is initially
established, so changing this has no effect after Icicles has been
loaded.  However, you can change it and save the new value so it will
be used next time.

For this option to have an effect upon startup, it must be set before
you enter Icicle mode.  This means that you must ensure that the code
that sets it is invoked before you enter Icicle mode.  If you use
Customize to change this option, then ensure that the code inserted by
Customize into your `user-init-file' or your `custom-file' is invoked
before you enter Icicle mode.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Key-Bindings)

(defcustom icicle-type-actions-alist
  '(("buffer"
     (lambda (b) (with-current-buffer b (ps-print-buffer))) ; E.g. showing you can use lambda.
     1-window-frames-on  another-buffer  browse-url-of-buffer  buffer-disable-undo
     buffer-enable-undo  buffer-file-name  buffer-local-variables  buffer-modified-p
     buffer-name  buffer-size  bury-buffer  choose-grep-buffer  current-line-string
     delete-1-window-frames-on  delete-windows-for  delete-windows-on
     delete/iconify-windows-on  describe-buffer-bindings  diff-buffer-with-file
     display-buffer  display-buffer-other-frame  echo-in-buffer  eval-buffer  fontify-buffer
     generate-new-buffer  get-buffer  get-buffer-process  get-buffer-window
     get-buffer-window-list  grepp-choose-grep-buffer  ibuffer-jump-to-buffer
     icicle-char-properties-in-buffer  icicle-delete-window  icicle-delete-windows-on
     icicle-help-line-buffer  icicle-kill-a-buffer  insert-buffer  insert-buffer-substring
     insert-buffer-substring-as-yank  insert-buffer-substring-no-properties  kill-buffer
     kill-buffer-and-its-windows  kill-buffer-if-not-modified  last-buffer
     multi-window-frames-on  other-buffer  pop-to-buffer  pr-interface  remove-windows-on
     replace-buffer-in-windows  smiley-buffer  switch-to-buffer  switch-to-buffer-other-frame
     switch-to-buffer-other-window  view-buffer  view-buffer-other-frame
     view-buffer-other-window)
    ("color"
     color-defined-p  color-gray-p  color-supported-p  color-values  colors
     doremi-set-background-color  doremi-set-foreground-color  eyedrop-color-message
     facemenu-set-background  facemenu-set-foreground  hexrgb-blue  hexrgb-color-name-to-hex
     hexrgb-complement  hexrgb-green  hexrgb-hex-to-color-values  hexrgb-hue  hexrgb-red
     hexrgb-saturation  hexrgb-value  icicle-color-help icicle-color-name-w-bg
     palette-brightness-scale  palette-color-message  palette-complement-or-alternative
     palette-hex-info  palette-hsv-info  palette-rgb-info  palette-swatch  ps-e-color-values
     ps-e-x-color-values  set-background-color  set-border-color  set-cursor-color
     set-foreground-color  set-mouse-color  tty-color-canonicalize  tty-color-desc
     tty-color-standard-values  tty-color-translate  tty-color-values  x-color-defined-p
     x-color-values)
    ("command"
     command-remapping  define-prefix-command  describe-command  disable-command
     doremi-push-frame-config-for-command  enable-command  Info-find-emacs-command-nodes
     Info-goto-emacs-command-node)
    ("face"
     color-theme-spec  custom-facep  customize-face  customize-face-other-window
     custom-theme-recalc-face  describe-face  face-all-attributes  face-attr-construct
     face-background  face-background-20+  face-background-pixmap  face-bold-p
     face-default-spec  face-differs-from-default-p  face-doc-string  face-documentation
     face-font  face-foreground  face-foreground-20+  face-id  face-inverse-video-p
     face-italic-p  face-nontrivial-p  face-spec-reset-face  face-stipple  face-underline-p
     face-user-default-spec  facemenu-add-face facemenu-add-new-face  facemenu-set-face
     find-face-definition  hlt-choose-default-face  hlt-show-default-face
     hlt-unhighlight-region-for-face  icicle-customize-face
     icicle-customize-face-other-window  invert-face  make-face  make-face-bold
     make-face-bold-italic  make-face-italic  make-face-unbold  make-face-unitalic
     moccur-face-check  modify-face  ps-face-attributes  read-all-face-attributes
     read-face-font)
    ("file"
     abbreviate-file-name  ange-ftp-chase-symlinks  ange-ftp-file-modtime  apropos-library
     auto-coding-alist-lookup  bookmark-file-or-variation-thereof  bookmark-load
     browse-url-of-file  byte-compile-file  check-declare-file  comint-append-output-to-file
     comint-quote-filename  comint-substitute-in-file-name  comint-unquote-filename
     comint-write-output  compilation-get-file-structure  cookie-insert  create-file-buffer
     delete-file  describe-file  dired-delete-file  diredp-mouse-diff  dired-show-file-type
     dir-locals-find-file  dir-locals-read-from-file  do-after-load-evaluation  ebnf-eps-file
     ebnf-print-file  ebnf-spool-file  ebnf-syntax-file  ediff-backup  epa-decrypt-file
     epa-import-keys  epa-verify-file  eval-next-after-load  ffap-file-remote-p
     ffap-locate-file  file-attributes  file-cache-add-file  file-chase-links
     file-dependents  file-directory-p  file-executable-p  file-exists-p
     file-loadhist-lookup  file-local-copy  file-modes  file-name-nondirectory
     file-newest-backup  file-nlinks  file-ownership-preserved-p  file-provides
     file-readable-p  file-regular-p  file-relative-name  file-remote-p  file-requires
     file-symlink-p  file-system-info  file-truename  file-writable-p  find-alternate-file
     find-alternate-file-other-window  find-buffer-visiting  finder-commentary  find-file
     find-file-at-point  find-file-binary  find-file-literally  find-file-noselect
     find-file-other-frame  find-file-other-window find-file-read-only
     find-file-read-only-other-frame  find-file-read-only-other-window  find-file-text
     get-file-buffer  gnus-audio-play  gnus-convert-png-to-face  hexl-find-file
     highlight-compare-with-file   icicle-add-file-to-fileset
     icicle-delete-file-or-directory  icicle-describe-file  icicle-file-remote-p
     icicle-help-line-file  icicle-search-file  icicle-shell-command-on-file
     image-type-from-file-header  image-type-from-file-name  Info-find-file  Info-index-nodes
     info-lookup-file  Info-toc-nodes  info-xref-check  insert-file  insert-file-literally
     insert-image-file  list-tags  lm-commentary  lm-creation-date  lm-keywords  lm-keywords-list
     lm-last-modified-date  lm-summary  lm-synopsis  lm-verify  lm-version  load  load-file
     load-history-regexp  make-backup-file-name  move-file-to-trash  open-dribble-file
     open-termscript  play-sound-file  pr-ps-file-preview  pr-ps-file-print
     pr-ps-file-ps-print  pr-ps-file-using-ghostscript  recentf-add-file  recentf-push
     recentf-remove-if-non-kept  recover-file  rmail-input  rmail-output  set-file-times
     set-visited-file-name  substitute-in-file-name  system-move-file-to-trash
     untranslated-canonical-name  untranslated-file-p  url-basepath  vc-backend
     vc-delete-automatic-version-backups  vc-file-clearprops  vc-insert-file
     vc-make-version-backup  vc-name  vc-state  vc-working-revision  view-file
     view-file-other-frame  view-file-other-window  visit-tags-table  w32-browser
     w32-long-file-name  w32-short-file-name  w32explore  woman-find-file  write-file
     xml-parse-file)
    ("frame"
     current-window-configuration  delete-frame  delete-other-frames  thumfr-dethumbify-frame
     doremi-undo-last-frame-color-change  thumfr-fisheye  fit-frame  fit-frame-maximize-frame
     fit-frame-minimize-frame  fit-frame-restore-frame  frame-char-height  frame-char-width
     frame-current-scroll-bars  frame-extra-pixels-height  frame-extra-pixels-width
     frame-face-alist  frame-first-window  frame-focus  frame-height  frame-iconified-p
     frame-parameters  frame-pixel-height  frame-pixel-width frame-root-window
     frame-selected-window  frame-set-background-mode  frame-terminal
     frame-visible-p  frame-width  get-a-frame  get-frame-name
     hide-frame  icicle-select-frame-by-name  iconify-frame  lower-frame
     make-frame-invisible  make-frame-visible  maximize-frame  maximize-frame-horizontally
     maximize-frame-vertically  menu-bar-open  minimize-frame  next-frame
     thumfr-only-raise-frame  previous-frame  raise-frame  really-iconify-frame
     redirect-frame-focus  redraw-frame  restore-frame  restore-frame-horizontally
     restore-frame-vertically  select-frame   select-frame-set-input-focus  set-frame-name
     show-frame  thumfr-thumbify-frame  thumfr-thumbify-other-frames  thumfr-thumbnail-frame-p
     thumfr-toggle-thumbnail-frame  toggle-max-frame  toggle-max-frame-horizontally
     toggle-max-frame-vertically  toggle-zoom-frame  tty-color-alist  tty-color-clear
     w32-focus-frame  window-list  window-system  window-tree  x-focus-frame  zoom-frm-in
     zoom-frm-out  zoom-frm-unzoom)
    ("function"
     cancel-function-timers  describe-function  elp-instrument-function  find-function
     find-function-other-frame  find-function-other-window  symbol-function  trace-function
     trace-function-background)
    ("option" custom-note-var-changed  customize-option  customize-option-other-window
     describe-option  icicle-binary-option-p  tell-customize-var-has-changed)
    ("process"
     accept-process-output  anything-kill-async-process  clone-process  continue-process
     delete-process  get-process  interrupt-process  kill-process  process-buffer
     process-coding-system  process-command  process-contact  process-exit-status
     process-filter  process-filter-multibyte-p  process-id
     process-inherit-coding-system-flag  process-kill-without-query  process-mark
     process-name  process-plist  process-query-on-exit-flag  process-running-child-p
     process-send-eof  process-sentinel  process-status  process-tty-name  process-type
     quit-process  set-process-coding-system  stop-process  tooltip-process-prompt-regexp
     tq-create)
    ("symbol"
     apropos-describe-plist  apropos-macrop  apropos-score-symbol  byte-compile-const-symbol-p
     custom-guess-type  custom-unlispify-tag-name  custom-variable-type  default-boundp
     default-value  describe-minor-mode-from-symbol  fmakunbound
     icicle-help-on-candidate-symbol  info-lookup-symbol  makunbound  symbol-file
     symbol-function  symbol-plist  symbol-value)
    ("variable"
     custom-type  custom-variable-documentation  custom-variable-p  custom-variable-type
     describe-variable  find-variable  find-variable-noselect  find-variable-other-frame
     find-variable-other-window  help-custom-type  icicle-custom-type  kill-local-variable
     local-variable-if-set-p  local-variable-p  make-local-variable  make-variable-buffer-local
     make-variable-frame-local  symbol-value  user-variable-p  variable-binding-locus)
    ("window"
     balance-windows  browse-kill-ring-fit-window  compilation-set-window-height
     delete-other-windows  delete-other-windows-vertically  delete-window
     delete/iconify-window  fit-frame-max-window-size  fit-window-to-buffer
     mouse-drag-vertical-line-rightward-window  mouse-drag-window-above  next-window
     previous-window  remove-window  select-window  shrink-window-if-larger-than-buffer
     split-window  truncated-partial-width-window-p  window--display-buffer-1
     window--even-window-heights  window--try-to-split-window  window-body-height
     window-buffer  window-buffer-height  window-current-scroll-bars  window-dedicated-p
     window-display-table  window-edges  window-end  window-fixed-size-p  window-frame
     window-fringes  window-full-width-p  window-height  window-hscroll  window-inside-edges
     window-inside-pixel-edges  window-margins  window-minibuffer-p  window-parameters
     window-pixel-edges  window-point  window-safely-shrinkable-p  window-scroll-bars
     window-start  window-text-height  window-vscroll  window-width))
  "*Alist of Emacs object types and associated actions.
Each element has the form (TYPE FUNCTION...), where TYPE names an
object type, and each FUNCTION accepts an object of type TYPE as its
only required argument

A FUNCTION here can be a symbol or a lambda form.  You can use a
symbol that is not yet `fboundp', that is, one that does not yet have
a function definition.  Any symbols that do not have function
definitions when this option is used are simply filtered out.

However, just because a function is defined at runtime does not mean
that it will work.  For example, function `buffer-size' is included in
the default value for type `buffer', but in Emacs 20 `buffer-size'
accepts no arguments, so applying it to a buffer name raises an error.

\[Note: If you have suggestions or corrections for the default value,
send them in, using `\\[icicle-send-bug-report]'.  The initial list
was drawn up quickly by looking at functions with the type in their
name and that accept a value of that type as only required argument.
There is no doubt that the default value could be improved.]"
  :type '(alist
          :key-type   (string :tag "Object type")
          ;; We cannot use type `function' because some symbols might not yet be `fboundp'.
          :value-type (repeat (restricted-sexp :tag "Action (function)"
                               :match-alternatives (functionp symbolp))))
  :group 'Icicles-Miscellaneous)

(defcustom icicle-unpropertize-completion-result-flag nil
  "*Non-nil means strip text properties from the completion result.
Set or bind this option to non-nil only if you need to ensure, for
some other library, that the string returned by `completing-read' and
\(starting with Emacs 23) `read-file-name' has no text properties.

Typically, you will not use a non-nil value.  Internal text properties
added by Icicles are always removed anyway.  A non-nil value lets you
also remove properties such as `face'.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Miscellaneous)

(defcustom icicle-update-input-hook nil
  "*Functions run when minibuffer input is updated (typing or deleting)."
  :type 'hook :group 'Icicles-Miscellaneous)

(defcustom icicle-use-~-for-home-dir-flag t ; Toggle with `M-~'.
  "*Non-nil means abbreviate your home directory using `~'.
You can toggle this option from the minibuffer at any time using
`M-~'.  You can also use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle the option value."
  :type 'boolean :group 'Icicles-Key-Bindings)

(defcustom icicle-use-C-for-actions-flag t ; Toggle with `M-g'.
  "*Non-nil means use modifier `C-' (Control) for multi-command actions.
If nil, then you need no `C-' modifier for actions, and, instead, you
need a `C-' modifier for ordinary candidate cycling.

It is not strictly correct to speak in terms of the `C-' modifier -
that is only the default behavior.  The actual keys concerned are
those defined by these options:

 `icicle-modal-cycle-down-action-keys'
 `icicle-modal-cycle-up-action-keys'
 `icicle-apropos-cycle-next-action-keys'
 `icicle-apropos-cycle-previous-action-keys'
 `icicle-prefix-cycle-next-action-keys'
 `icicle-prefix-cycle-previous-action-keys'

You can toggle this option from the minibuffer at any time using
`M-g'.  You can also use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle the option value."
  :type 'boolean :group 'Icicles-Key-Bindings)

(defcustom icicle-use-anything-candidates-flag t
  "*Non-nil means Icicles can include Anything candidates for completion.
When non-nil, Anything actions are used for candidate alternate
actions in some Icicles commands, and Anything types and actions are
used by command `icicle-object-action' (aka `a' and `what-which-how').

This option has no effect if library `anything.el' cannot be loaded.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Matching)

(defcustom icicle-use-candidates-only-once-flag nil
  "*Non-nil means remove each candidate from the set after using it.
When you use a multi-command and act on a candidate (for example, with
`C-RET'), the candidate is removed from those available if this is
non-nil.  If this is nil, then the candidate is not removed, so you
can act on it again.

You can customize this option if you prefer the non-nil behavior all
of the time.  However, most users will not want to do that.

If you write Emacs-Lisp code, you can bind this to non-nil during
completion in contexts where it makes little sense for users to act on
the same candidate more than once.  That way, users cannot choose it
again, and they are not distracted seeing it as a candidate.

See also non-option variable `icicle-use-candidates-only-once-alt-p'.

Remember that you can use multi-command `icicle-toggle-option' anytime
\(`M-i M-i' during completion) to toggle an option value."
  :type 'boolean :group 'Icicles-Matching)

(defun icicle-widgetp (widget)
  "Return non-nil if WIDGET is a widget.
Same as `widgetp' in Emacs 22+.  Defined for Emacs 20 and 21."
  (if (symbolp widget)
      (get widget 'widget-type)
    (and (consp widget)  (symbolp (car widget))  (get (car widget) 'widget-type))))

;; To redefine widget `color' to `icicle-color', you need library `wid-edit+.el'.
(defcustom icicle-widgets-to-redefine `(file ,@(and (require 'wid-edit+ nil t)  '(color)))
  "*List of widgets to be redefined to provide Icicles completion.
Each widget must be a symbol with property `widget-type'.
When in Icicle mode, Icicles completion is available.  Otherwise,
vanilla completion is available.  In other words, with Icicle mode
turned off, you should get the ordinary behavior.

For this option to have an effect upon startup, it must be set before
you enter Icicle mode.  This means that you must ensure that the code
that sets it is invoked before you enter Icicle mode.  If you use
Customize to change this option, then ensure that the code inserted by
Customize into your `user-init-file' or your `custom-file' is invoked
before you enter Icicle mode.  (Alternatively, you can toggle Icicle
mode twice.)"
  :type '(repeat (restricted-sexp :tag "Widget (a symbol)"
                  :match-alternatives (lambda (obj) (and (symbolp obj)  (icicle-widgetp obj)))
                  :value ignore))
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (when (boundp 'icicle-mode-map) ; Avoid error on initialization.
           (icicle-redefine-standard-widgets)))
  :initialize #'custom-initialize-default
  :group 'Icicles-Miscellaneous)

(defcustom icicle-word-completion-keys '([(meta ?\ )]) ; `M-SPC'
  "*Key sequences to use for minibuffer prefix word completion.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Because file names, in particular, can contain spaces, some people
prefer such a key sequence to be non-printable, such as `M-SPC'.  This
is the default value in Icicles.

But because the spacebar is such a convenient key to hit, other people
prefer to use `SPC' for word completion, and to insert a space some
other way.  The usual way to do that is via `C-q SPC', but command
`icicle-insert-a-space' is provided for convenience.  You can bind
this to `M-SPC', for instance, in `minibuffer-local-completion-map',
`minibuffer-local-completion-map', and
`minibuffer-local-must-match-map'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

(defcustom icicle-WYSIWYG-Completions-flag "MMMM"
  "*Non-nil means show candidates in `*Completions*' using WYSIWYG.
This has an effect only for completion of things like faces, fonts,
and colors.

For face and color candidates, the particular non-nil value determines
the appearance:

* If t, the candidate displays its meaning: WYSIWYG.
* If a string, the string is propertized and then appended to the
  candidate,  to serve as a color swatch.

Some commands might override a string value with different text.  This
is the case for `icicle-read-color-WYSIWYG', for instance: the color
swatch text is always the color's RGB code.

Note that, starting with Emacs 22, if this option is non-nil, then
command `describe-face' does not use `completing-read-multiple', since
that (non-Icicles) function does not support WYSIWYG candidates."
  :type '(choice
          (string :tag "Show candidate plus a WYSIWYG swatch with text..."  :value "MMMM")
          (const  :tag "Show candidate itself using WYSIWYG"                t)
          (const  :tag "Show candidate as is, with no text properties"      nil))
  :group 'Icicles-Completions-Display)

(when (fboundp 'read-char-by-name)      ; Emacs 23+
  (defcustom icicle-zap-to-char-candidates nil
    "*Names to use for `icicle-zap-to-char' when completing.
Either a function that returns a list of the same form as `ucs-names',
or nil, which means the Unicode chars that have been read previously."
    :type '(choice
            (const    :tag "All Unicode chars"      icicle-ucs-names)
            (const    :tag "Previously used chars"  nil)
            (function :tag "Invoke function" :value icicle-ucs-names))
    :group 'Icicles-Matching))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-opt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-opt.el ends here

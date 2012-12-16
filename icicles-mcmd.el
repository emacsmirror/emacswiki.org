;;; icicles-mcmd.el --- Minibuffer commands for Icicles
;;
;; Filename: icicles-mcmd.el
;; Description: Minibuffer commands for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2012, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:25:04 2006
;; Version: 22.0
;; Last-Updated: Sat Dec 15 16:10:38 2012 (-0800)
;;           By: dradams
;;     Update #: 18689
;; URL: http://www.emacswiki.org/icicles-mcmd.el
;; Doc URL: http://www.emacswiki.org/Icicles
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `cl', `doremi', `el-swank-fuzzy',
;;   `ffap', `ffap-', `fuzzy', `fuzzy-match', `hexrgb',
;;   `icicles-face', `icicles-fn', `icicles-opt', `icicles-var',
;;   `image-dired', `kmacro', `levenshtein', `mouse3', `mwheel',
;;   `naked', `regexp-opt', `ring', `ring+', `thingatpt',
;;   `thingatpt+', `wid-edit', `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  commands to be used mainly in the minibuffer or buffer
;;  `*Completions*' (and a few non-interactive functions used in those
;;  commands).  For top-level commands, see `icicles-cmd1.el' and
;;  `icicles-cmd2.el'.  For Icicles documentation, see
;;  `icicles-doc1.el' and `icicles-doc2.el'.
;;
;;  Commands defined here:
;;
;;    `cycle-icicle-expand-to-common-match',
;;    `cycle-icicle-image-file-thumbnail',
;;    `cycle-icicle-incremental-completion',
;;    `cycle-icicle-sort-order',
;;    `cycle-icicle-S-TAB-completion-method',
;;    `cycle-icicle-TAB-completion-method',
;;    `icicle-abort-recursive-edit', `icicle-add-file-to-fileset',
;;    `icicle-add/update-saved-completion-set',
;;    `icicle-all-candidates-action',
;;    `icicle-all-candidates-alt-action',
;;    `icicle-all-candidates-list-action',
;;    `icicle-all-candidates-list-alt-action',
;;    `icicle-apropos-complete', `icicle-apropos-complete-and-exit',
;;    `icicle-apropos-complete-and-narrow',
;;    `icicle-apropos-complete-and-widen',
;;    `icicle-apropos-complete-no-display',
;;    `icicle-backward-char-magic',
;;    `icicle-backward-delete-char-untabify',
;;    `icicle-backward-kill-paragraph',
;;    `icicle-backward-kill-sentence', `icicle-backward-kill-sexp',
;;    `icicle-backward-kill-word', `icicle-beginning-of-line+',
;;    `icicle-candidate-action', `icicle-candidate-alt-action',
;;    `icicle-candidate-read-fn-invoke',
;;    `icicle-candidate-set-complement',
;;    `icicle-candidate-set-define',
;;    `icicle-candidate-set-difference',
;;    `icicle-candidate-set-intersection',
;;    `icicle-candidate-set-retrieve',
;;    `icicle-candidate-set-retrieve-from-variable',
;;    `icicle-candidate-set-retrieve-more',
;;    `icicle-candidate-set-retrieve-persistent',
;;    `icicle-candidate-set-save', `icicle-candidate-set-save-more',
;;    `icicle-candidate-set-save-more-selected',
;;    `icicle-candidate-set-save-persistently',
;;    `icicle-candidate-set-save-selected',
;;    `icicle-candidate-set-save-to-variable',
;;    `icicle-candidate-set-swap', `icicle-candidate-set-truncate',
;;    `icicle-candidate-set-union',
;;    `icicle-change-alternative-sort-order',
;;    `icicle-change-history-variable', `icicle-change-sort-order',
;;    `icicle-choose-completion', `icicle-completing-read+insert',
;;    `icicle-Completions-mouse-3-menu',
;;    `icicle-cycle-expand-to-common-match',
;;    `icicle-cycle-image-file-thumbnail',
;;    `icicle-cycle-incremental-completion',
;;    `icicle-delete-backward-char', `icicle-delete-candidate-object',
;;    `icicle-delete-char', `icicle-delete-windows-on',
;;    `icicle-describe-file', `icicle-digit-argument',
;;    `icicle-dispatch-C-^', `icicle-dispatch-C-.',
;;    `icicle-dispatch-C-x.', `icicle-dispatch-M-_',
;;    `icicle-dispatch-M-comma', `icicle-dispatch-M-q',
;;    `icicle-doremi-candidate-width-factor+',
;;    `icicle-doremi-increment-max-candidates+',
;;    `icicle-doremi-increment-swank-prefix-length+',
;;    `icicle-doremi-increment-swank-timeout+',
;;    `icicle-doremi-inter-candidates-min-spaces+',
;;    `icicle-doremi-zoom-Completions+', `icicle-end-of-line+',
;;    `icicle-erase-minibuffer',
;;    `icicle-erase-minibuffer-or-history-element',
;;    `icicle-exit-minibuffer', `icicle-forward-char-magic',
;;    `icicle-goto/kill-failed-input', `icicle-help-on-candidate',
;;    `icicle-help-on-next-apropos-candidate',
;;    `icicle-help-on-next-prefix-candidate',
;;    `icicle-help-on-previous-apropos-candidate',
;;    `icicle-help-on-previous-prefix-candidate',
;;    `icicle-help-string-completion',
;;    `icicle-help-string-non-completion', `icicle-history',
;;    `icicle-insert-completion', `icicle-insert-dot-command',
;;    `icicle-insert-history-element',
;;    `icicle-insert-key-description',
;;    `icicle-insert-list-join-string',
;;    `icicle-insert-newline-in-minibuffer',
;;    `icicle-insert-string-at-point',
;;    `icicle-insert-string-from-variable', `icicle-isearch-complete',
;;    `icicle-keep-only-past-inputs', `icicle-kill-line',
;;    `icicle-kill-paragraph', `icicle-kill-region',
;;    `icicle-kill-region-wimpy', `icicle-kill-sentence',
;;    `icicle-kill-sexp', `icicle-kill-word', `icicle-make-directory',
;;    `icicle-minibuffer-complete-and-exit', `icicle-minibuffer-help',
;;    `icicle-mouse-candidate-action',
;;    `icicle-mouse-candidate-alt-action',
;;    `icicle-mouse-candidate-read-fn-invoke',
;;    `icicle-mouse-candidate-set-save',
;;    `icicle-mouse-candidate-set-save-more',
;;    `icicle-mouse-choose-completion',
;;    `icicle-mouse-help-on-candidate',
;;    `icicle-mouse-remove-candidate',
;;    `icicle-mouse-save/unsave-candidate',
;;    `icicle-mouse-save-then-kill', `icicle-mouse-yank-secondary',
;;    `icicle-move-to-next-completion',
;;    `icicle-move-to-previous-completion',
;;    `icicle-narrow-candidates',
;;    `icicle-narrow-candidates-with-predicate',
;;    `icicle-negative-argument', `icicle-next-apropos-candidate',
;;    `icicle-next-apropos-candidate-action',
;;    `icicle-next-apropos-candidate-alt-action',
;;    `icicle-next-candidate-per-mode',
;;    `icicle-next-candidate-per-mode-action',
;;    `icicle-next-candidate-per-mode-alt-action',
;;    `icicle-next-history-element', `icicle-next-line',
;;    `icicle-next-prefix-candidate',
;;    `icicle-next-prefix-candidate-action',
;;    `icicle-next-prefix-candidate-alt-action',
;;    `icicle-next-S-TAB-completion-method',
;;    `icicle-next-TAB-completion-method',
;;    `icicle-ORIG-choose-completion', `icicle-ORIG-exit-minibuffer',
;;    `icicle-ORIG-minibuffer-complete-and-exit',
;;    `icicle-ORIG-mouse-choose-completion',
;;    `icicle-ORIG-next-history-element', `icicle-ORIG-sit-for',
;;    `icicle-ORIG-switch-to-completions', `icicle-other-history',
;;    `icicle-plus-saved-sort',
;;    `icicle-pp-eval-expression-in-minibuffer',
;;    `icicle-prefix-complete', `icicle-prefix-complete-no-display',
;;    `icicle-prefix-word-complete',
;;    `icicle-previous-apropos-candidate',
;;    `icicle-previous-apropos-candidate-action',
;;    `icicle-previous-apropos-candidate-alt-action',
;;    `icicle-previous-candidate-per-mode',
;;    `icicle-previous-candidate-per-mode-action',
;;    `icicle-previous-candidate-per-mode-alt-action',
;;    `icicle-previous-line', `icicle-previous-prefix-candidate',
;;    `icicle-previous-prefix-candidate-action',
;;    `icicle-previous-prefix-candidate-alt-action',
;;    `icicle-read+insert-file-name', `icicle-regexp-quote-input',
;;    `icicle-remove-candidate', `icicle-remove-Completions-window',
;;    `icicle-resolve-file-name', `icicle-retrieve-last-input',
;;    `icicle-retrieve-next-input', `icicle-retrieve-previous-input',
;;    `icicle-reverse-sort-order',
;;    `icicle-save-predicate-to-variable',
;;    `icicle-save/unsave-candidate',
;;    `icicle-scroll-Completions-backward',
;;    `icicle-scroll-Completions-forward', `icicle-scroll-backward',
;;    `icicle-scroll-forward', `icicle-search-define-replacement',
;;    `icicle-self-insert', `icicle-sit-for',
;;    `icicle-sort-alphabetical', `icicle-sort-by-abbrev-frequency',
;;    `icicle-sort-by-directories-first',
;;    `icicle-sort-by-directories-last', `icicle-sort-by-file-type',
;;    `icicle-sort-by-last-file-access-time',
;;    `icicle-sort-by-last-file-modification-time',
;;    `icicle-sort-by-last-use-as-input',
;;    `icicle-sort-by-previous-use-alphabetically',
;;    `icicle-sort-by-2nd-parts-alphabetically',
;;    `icicle-sort-case-insensitive',
;;    `icicle-sort-extra-candidates-first',
;;    `icicle-sort-proxy-candidates-first',
;;    `icicle-sort-special-candidates-first',
;;    `icicle-sort-turned-OFF', `icicle-switch-to-Completions-buf',
;;    `icicle-switch-to-completions',
;;    `icicle-switch-to/from-minibuffer', `icicle-toggle-.',
;;    `icicle-toggle-~-for-home-dir',
;;    `icicle-toggle-alternative-sorting',
;;    `icicle-toggle-angle-brackets', `icicle-toggle-annotation',
;;    `icicle-toggle-case-sensitivity', `icicle-toggle-C-for-actions',
;;    `icicle-toggle-completions-format', `icicle-toggle-dot',
;;    `icicle-toggle-expand-to-common-match',
;;    `icicle-toggle-hiding-common-match',
;;    `icicle-toggle-hiding-non-matching-lines',
;;    `icicle-toggle-highlight-all-current',
;;    `icicle-toggle-highlight-historical-candidates',
;;    `icicle-toggle-highlight-saved-candidates',
;;    `icicle-toggle-ignored-extensions',
;;    `icicle-toggle-ignored-space-prefix',
;;    `icicle-toggle-ignoring-comments',
;;    `icicle-toggle-include-cached-files',
;;    `icicle-toggle-include-recent-files',
;;    `icicle-toggle-literal-replacement',
;;    `icicle-toggle-network-drives-as-remote',
;;    `icicle-toggle-proxy-candidates', `icicle-toggle-regexp-quote',
;;    `icicle-toggle-remote-file-testing',
;;    `icicle-toggle-search-cleanup',
;;    `icicle-toggle-search-complementing-domain',
;;    `icicle-toggle-search-replace-common-match',
;;    `icicle-toggle-search-replace-whole',
;;    `icicle-toggle-search-whole-word',
;;    `icicle-toggle-show-multi-completion', `icicle-toggle-sorting',
;;    `icicle-toggle-transforming',
;;    `icicle-toggle-WYSIWYG-Completions', `icicle-transpose-chars',
;;    `icicle-transpose-sexps', `icicle-transpose-words',
;;    `icicle-universal-argument', `icicle-universal-argument-minus',
;;    `icicle-universal-argument-more',
;;    `icicle-universal-argument-other-key', `icicle-up-directory',
;;    `icicle-use-interactive-command-history',
;;    `icicle-widen-candidates', `icicle-yank', `icicle-yank-pop',
;;    `icicle-yank-secondary', `toggle-icicle-.',
;;    `toggle-icicle-~-for-home-dir',
;;    `toggle-icicle-alternative-sorting',
;;    `toggle-icicle-angle-brackets', `toggle-icicle-annotation',
;;    `toggle-icicle-case-sensitivity', `toggle-icicle-C-for-actions',
;;    `toggle-icicle-completions-format', `toggle-icicle-dot',
;;    `toggle-icicle-expand-to-common-match',
;;    `toggle-icicle-hiding-common-match',
;;    `toggle-icicle-hiding-non-matching-lines',
;;    `toggle-icicle-highlight-all-current',
;;    `toggle-icicle-highlight-historical-candidates',
;;    `toggle-icicle-highlight-saved-candidates',
;;    `toggle-icicle-ignored-extensions',
;;    `toggle-icicle-ignored-space-prefix',
;;    `toggle-icicle-include-cached-files',
;;    `toggle-icicle-include-recent-files',
;;    `toggle-icicle-incremental-completion',
;;    `toggle-icicle-literal-replacement',
;;    `toggle-icicle-network-drives-as-remote',
;;    `toggle-icicle-proxy-candidates', `toggle-icicle-regexp-quote',
;;    `toggle-icicle-remote-file-testing',
;;    `toggle-icicle-search-cleanup',
;;    `toggle-icicle-search-complementing-domain',
;;    `toggle-icicle-search-replace-common-match',
;;    `toggle-icicle-search-replace-whole',
;;    `toggle-icicle-search-whole-word',
;;    `toggle-icicle-show-multi-completion', `toggle-icicle-sorting',
;;    `toggle-icicle-transforming',
;;    `toggle-icicle-WYSIWYG-Completions'.
;;
;;  Non-interactive functions defined here:
;;
;;    `icicle-all-candidates-action-1', `icicle-all-exif-data',
;;    `icicle-anychar-regexp', `icicle-apply-to-saved-candidate',
;;    `icicle-apropos-complete-1', `icicle-apropos-complete-2',
;;    `icicle-autofile-action',
;;    `icicle-backward-delete-char-untabify-magic',
;;    `icicle-bind-buffer-candidate-keys',
;;    `icicle-bind-file-candidate-keys', `icicle-candidate-action-1',
;;    `icicle-candidate-set-retrieve-1',
;;    `icicle-candidate-set-save-1',
;;    `icicle-candidate-set-save-selected-1',
;;    `icicle-column-wise-cand-nb', `icicle-Completions-popup-choice',
;;    `icicle-Completions-popup-choice-1', `icicle-convert-dots',
;;    `icicle-current-completion-in-Completions',
;;    `icicle-current-sort-functions', `icicle-current-sort-order',
;;    `icicle-delete-backward-char-magic',
;;    `icicle-delete-candidate-object-1', `icicle-delete-char-magic',
;;    `icicle-delete-current-candidate-object',
;;    `icicle-ensure-overriding-map-is-bound',
;;    `icicle-help-on-candidate-symbol',
;;    `icicle-input-is-a-completion-p', `icicle-insert-dot',
;;    `icicle-insert-input', `icicle-insert-thing',
;;    `icicle-looking-at-p', `icicle-looking-back-at-p',
;;    `icicle-markers-to-readable',
;;    `icicle-maybe-multi-completion-completing-p',
;;    `icicle-mouse-candidate-action-1', `icicle-nb-Completions-cols',
;;    `icicle-nb-of-cand-at-Completions-pos',
;;    `icicle-nb-of-cand-in-Completions-horiz',
;;    `icicle-prefix-complete-1', `icicle-prefix-complete-2',
;;    `icicle-raise-Completions-frame',
;;    `icicle-remove-cand-from-lists',
;;    `icicle-remove-candidate-display-others',
;;    `icicle-replace-input-w-parent-dir',
;;    `icicle-retrieve-candidates-from-set',
;;    `icicle-row-wise-cand-nb', `icicle-signum',
;;    `icicle-substitute-keymap-vars', `icicle-successive-action',
;;    `icicle-transform-sole-candidate',
;;    `icicle-transpose-chars-magic',
;;    `icicle-unbind-buffer-candidate-keys',
;;    `icicle-unbind-file-candidate-keys',
;;    `icicle-upcase-if-ignore-case', `icicle-update-and-next'.
;;
;;  Internal variables defined here:
;;
;;    `overriding-map-is-bound', `saved-overriding-map'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `exit-minibuffer'              - Remove *Completion* window and
;;                                   input mismatch highlighting
;;  `minibuffer-complete-and-exit' - Use Icicles prefix completion
;;
;;
;;  ***** NOTE: The following function defined in `mouse.el' has
;;              been REDEFINED HERE:
;;
;;  `choose-completion'       - Don't iconify frame or bury buffer.
;;  `mouse-choose-completion' - Return the number of the completion.
;;
;;
;;  ***** NOTE: The following function defined in `simple.el' has
;;              been REDEFINED HERE:
;;
;;  `switch-to-completions' - Always selects `*Completions*' window.
;;
;;
;;  Key bindings made by Icicles: See "Key Bindings" in
;;  `icicles-doc2.el'.
;;
;;  For descriptions of changes to this file, see `icicles-chg.el'.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Redefined standard commands")
;;  (@> "Icicles commands")
;;    (@> "Minibuffer editing commands")
;;    (@> "Commands to sort completion candidates")
;;    (@> "Other commands to be used mainly in the minibuffer")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; case, flet, lexical-let, loop
                                  ;; plus, for Emacs < 21: dolist, push
(eval-when-compile (require 'filesets nil t)) ; Emacs 22+.
  ;; filesets-data, filesets-entry-get-files, filesets-entry-mode, filesets-entry-set-files,
  ;; filesets-files-equalp, filesets-init, filesets-member, filesets-set-config

(eval-when-compile
 (or (condition-case nil
         (load-library "icicles-mac")   ; Use load-library to ensure latest .elc.
       (error nil))
     (require 'icicles-mac)))           ; Require, so can load separately if not on `load-path'.
  ;; icicle-assoc-delete-all, icicle-define-sort-command
(require 'icicles-opt)                  ; (This is required anyway by `icicles-var.el'.)
  ;; icicle-alternative-sort-comparer, icicle-buffer-ignore-space-prefix-flag,
  ;; icicle-move-Completions-frame, icicle-Completions-mouse-3-menu-entries, icicle-default-cycling-mode,
  ;; icicle-default-thing-insertion, icicle-expand-input-to-common-match,
  ;; icicle-hide-common-match-in-Completions-flag, icicle-hide-non-matching-lines-flag,
  ;; icicle-incremental-completion, icicle-input-string, icicle-kbd,
  ;; icicle-key-descriptions-use-<>-flag, icicle-regexp-quote-flag, icicle-saved-completion-sets,
  ;; icicle-search-cleanup-flag, icicle-search-highlight-all-current-flag, icicle-sort-comparer,
  ;; icicle-sort-orders-alist, icicle-TAB-shows-candidates-flag, icicle-thing-at-point-functions,
  ;; icicle-transform-function
(eval-and-compile (require 'icicles-var)) ; (This is required anyway by `icicles-fn.el'.)
  ;; lacarte-menu-items-alist, icicle-buffer-name-input-p, icicle-candidate-action-fn,
  ;; icicle-candidate-nb, icicle-complete-keys-alist, icicle-completion-candidates, 
  ;; icicle-current-completion-candidate-overlay, icicle-current-completion-mode,
  ;; icicle-current-input, icicle-current-raw-input, icicle-default-thing-insertion-flipped-p,
  ;; icicle-edit-update-p, icicle-general-help-string, icicle-get-alist-candidate-function,
  ;; icicle-ignored-extensions, icicle-ignored-extensions-regexp, icicle-incremental-completion-p,
  ;; icicle-insert-string-at-pt-end, `icicle-insert-string-at-pt-start, icicle-last-completion-candidate,
  ;; icicle-last-completion-command, icicle-last-input, icicle-last-sort-comparer,
  ;; icicle-last-transform-function, icicle-nb-of-other-cycle-candidates, icicle-pre-minibuffer-buffer,
  ;; icicle-saved-candidates-variables-obarray, icicle-saved-completion-candidates,
  ;; icicle-saved-ignored-extensions, icicle-successive-grab-count, icicle-thing-at-pt-fns-pointer,
  ;; icicle-universal-argument-map, icicle-variable-name-history
(require 'icicles-fn)
  ;; icicle-isearch-complete-past-string, icicle-minibuf-input-sans-dir,
  ;; icicle-toggle-icicle-mode-twice

(require 'doremi nil t) ;; (no error if not found):
                        ;; doremi, doremi(-boost)-(up|down)-keys, doremi-limit, doremi-wrap
(when (> emacs-major-version 22) (require 'help-fns+ nil t)) ;; (no error if not found):
                                                             ;; help-commands-to-key-buttons

(eval-when-compile (require 'fit-frame nil t)) ;; (no error if not found): fit-frame
(eval-when-compile
 (when (> emacs-major-version 21) (require 'linkd nil t))) ;; (no error if not found): linkd-mode

;; Byte-compiling this file, you will likely get some byte-compiler warning messages.
;; These are probably benign - ignore them.  Icicles is designed to work with multiple
;; versions of Emacs, and that fact provokes compiler warnings.  If you get byte-compiler
;; errors (not warnings), then please report a bug, using `M-x icicle-send-bug-report'.

;; Some defvars to quiet byte-compiler a bit:

(when (< emacs-major-version 22)
  (defvar overriding-map-is-bound)
  (defvar read-file-name-completion-ignore-case) ; In `minibuffer.el'
  (defvar read-file-name-predicate)
  (defvar saved-overriding-map))

(when (< emacs-major-version 23)
  (defvar read-buffer-completion-ignore-case)
  (defvar mouse-drag-copy-region))

(defvar doremi-boost-down-keys)         ; In `doremi.el'
(defvar doremi-boost-up-keys)           ; In `doremi.el'
(defvar doremi-down-keys)               ; In `doremi.el'
(defvar doremi-up-keys)                 ; In `doremi.el'
(defvar filesets-data)                  ; In `filesets.el'.
(defvar icicle-ido-like-mode)           ; In `icicles-cmd2.el' (implicit)
(defvar ignore-comments-flag)           ; In `thing-cmds.el'.
(defvar minibuffer-confirm-exit-commands) ; In `minibuffer.el' in Emacs 23+.
(defvar minibuffer-local-filename-completion-map) ; In Emacs 22+.
(defvar minibuffer-local-filename-must-match-map) ; In Emacs 23.2 (but not Emacs 24+).
(defvar minibuffer-local-must-match-filename-map) ; In Emacs 22+.
(defvar recentf-list)                   ; In `recentf.el' (Emacs 21+).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Redefined standard commands")

;;; Redefined standard commands --------------------------------------


;; REPLACE ORIGINAL `next-history-element' in `simple.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Selects minibuffer contents and leaves point at its beginning.
;;
(unless (fboundp 'icicle-ORIG-next-history-element)
  (defalias 'icicle-ORIG-next-history-element (symbol-function 'next-history-element)))

;;;###autoload (autoload 'icicle-next-history-element "icicles")
(defun icicle-next-history-element (arg) ; Bound to `M-n' in minibuffer.
  "Insert the next element of the minibuffer history in the minibuffer.
With argument N, it uses the Nth following element."
  (interactive "p")
  (icicle-ORIG-next-history-element (prefix-numeric-value arg))
  (when (and icicle-mode  (memq icicle-default-value '(preselect-start preselect-end)))
    (icicle-select-minibuffer-contents)
    (setq deactivate-mark  nil)))


;; REPLACE ORIGINAL `exit-minibuffer' (built-in function),
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Remove input mismatch highlighting.
;; Remove *Completion* window.
;;
(unless (fboundp 'icicle-ORIG-exit-minibuffer)
  (defalias 'icicle-ORIG-exit-minibuffer (symbol-function 'exit-minibuffer)))

;;;###autoload (autoload 'icicle-exit-minibuffer "icicles")
(defun icicle-exit-minibuffer ()        ; Bound to `C-m' (`RET') in minibuffer.
  "Terminate this minibuffer argument.
Remove `*Completions*' window.  Remove Icicles minibuffer faces."
  ;; This removal lets users retrieve candidates that have other faces, and saves input-history space.
  (interactive)
  (when (active-minibuffer-window)
    (with-current-buffer (window-buffer (minibuffer-window))
      (let ((pos                (icicle-minibuffer-prompt-end))
            (icy-minibuf-faces  '(icicle-input-completion-fail  icicle-input-completion-fail-lax
                                  icicle-whitespace-highlight   icicle-match-highlight-minibuffer
                                  icicle-complete-input))
            (keep-faces         ()))
        (while (< pos (point-max))
          (let ((faces  (get-text-property pos 'face)))
            (when (or (and (consp faces)  (cdr faces)  (atom (cdr faces))) ; (background-color . "abc")
                      (and faces  (atom faces))) ; face name
              (setq faces  (list faces))) ; No-op: (foo (background-color . "abc") (:foreground "abc"))
            (setq keep-faces  (icicle-set-union keep-faces
                                                (icicle-set-difference faces icy-minibuf-faces))))
          (setq pos  (1+ pos)))
        ;; If KEEP-FACES is (), use `remove-text-properties' instead of just `put-text-property',
        ;; so we do not add a nil `face' property.
        (if keep-faces
            (put-text-property (icicle-minibuffer-prompt-end) (point-max) 'face keep-faces)
          (remove-text-properties (icicle-minibuffer-prompt-end) (point-max) '(face nil))))
      ;; $$$$$  (let ((pos  (icicle-minibuffer-prompt-end)))
      ;;     (while (< pos (point-max))
      ;;       (when (memq (get-text-property pos 'face)
      ;;                   '(icicle-input-completion-fail icicle-input-completion-fail-lax))
      ;;         (remove-text-properties pos (point-max) '(face))
      ;;         (setq pos  (point-max)))
      ;;       (setq pos  (1+ pos))))
      ))
  (icicle-remove-Completions-window)
  (icicle-ORIG-exit-minibuffer))


;; REPLACE ORIGINAL `minibuffer-complete-and-exit' (built-in function),
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Use Icicles completion.
;;
(unless (fboundp 'icicle-ORIG-minibuffer-complete-and-exit)
  (defalias 'icicle-ORIG-minibuffer-complete-and-exit (symbol-function 'minibuffer-complete-and-exit)))

;; Bound to `C-m' (`RET') in must-match minibuffer completion maps.
;;;###autoload (autoload 'icicle-minibuffer-complete-and-exit "icicles")
(defun icicle-minibuffer-complete-and-exit ()
  "If the minibuffer contents is a valid completion, then exit.
Otherwise try to complete it."
  (interactive)
  (let ((last-cmd  last-command))
    (cond ((string= "" (if (icicle-file-name-input-p) ;  Empty input - exit.
                           (icicle-minibuf-input-sans-dir)
                         (icicle-input-from-minibuffer)))
           (icicle-exit-minibuffer))
          ;; This case serves when property `icicle-display-string' is used.
          ;; What's returned is the replacement display string, not the original candidate.
          ;; If you want to get the original candidate back, you'll need to search the obarray for a
          ;; symbol that has this `icicle-display-string' value.  Or put the symbol on the display
          ;; string as a text property.
          ((icicle-input-is-a-completion-p) (icicle-exit-minibuffer))
          ((eq minibuffer-completion-confirm 'confirm) ; User wants it anyway? - Emacs 23+.
           (if (eq last-cmd this-command)
               (icicle-exit-minibuffer)
             (minibuffer-message "Confirm")
             nil))
          ((eq minibuffer-completion-confirm 'confirm-after-completion) ; Emacs 23+.
           ;; Similar to `confirm', but only if trying to exit immediately
           ;; after completing (this catches most minibuffer typos).
           (if (not (memq last-cmd (and (boundp 'minibuffer-confirm-exit-commands)
                                        (append icicle-confirm-exit-commands
                                                minibuffer-confirm-exit-commands))))
               (icicle-exit-minibuffer)
             (minibuffer-message "Confirm")
             nil))
          (t
           (setq icicle-current-input  (icicle-input-from-minibuffer))
           (let* (;; Bind these first two to suppress (a) the throw or (b) the message, highlighting,
                  ;; mode-line help, and the wait involved in completing again.
                  (icicle-prefix-complete-and-exit-p   t)
                  (icicle-apropos-complete-and-exit-p  t)

                  (candidates
                   ;; If we're not using `icicle-candidates-alist', complete the input again.
                   ;; If we're using `icicle-candidates-alist', try filtering it against just the
                   ;; input.
                   ;;   If the input is already complete, then we're done.  If not, then filtering
                   ;;   will give nil and we will just continue to display the candidates.  If there
                   ;;   are multiple matches, then the user can either cycle or complete again.
                   (if (not icicle-candidates-alist)
                       (if (eq icicle-current-completion-mode 'apropos)
                           (icicle-apropos-complete-no-display 'nomsg)
                         (icicle-prefix-complete-no-display 'nomsg))
                     (icicle-filter-alist icicle-candidates-alist (list icicle-current-input)))))
             (cond ((and (eq icicle-require-match-p t) ; Don't exit if non-nil and non-t.
                         (icicle-input-is-a-completion-p))
                    (icicle-exit-minibuffer))
                   (t
                    (icicle-display-candidates-in-Completions))))))))

;;; $$$$$$ Should probably rename it.  It's no longer necessarily about apropos completion.
;;;###autoload (autoload 'icicle-apropos-complete-and-exit "icicles")
(defun icicle-apropos-complete-and-exit () ; Bound to `S-return' (`S-RET') in minibuffer completion maps.
  "If minibuffer content is a valid completion or has a single match, exit.
If the content is not complete, try to complete it.  If completion
leads to a valid completion, then exit.

This differs from `icicle-minibuffer-complete-and-exit' (bound to
`RET' in must-match minibuffer maps) in these ways:

 * If there is only one completion, this completes the input, rather
   than accepting it uncompleted.
 * This does not bother with the various special cases: empty input or
   non-nil `minibuffer-completion-confirm' values."
  (interactive)
  (setq icicle-current-input  (icicle-input-from-minibuffer))
  (let* (;; Bind these first two to suppress (a) the throw or (b) the message, highlighting,
         ;; mode-line help, and the wait involved in completing again.
         (icicle-prefix-complete-and-exit-p   t)
         (icicle-apropos-complete-and-exit-p  t)
         (candidates
          ;; If we're not using `icicle-candidates-alist', complete the input again.
          ;; If we're using `icicle-candidates-alist', try filtering it against just the
          ;; input.
          ;;   If the input is already complete, then we're done.  If not, then filtering
          ;;   will give nil and we will just continue to display the candidates.  If there
          ;;   are multiple matches, then the user can either cycle or complete again.
          (if (not icicle-candidates-alist)
              (if (eq icicle-current-completion-mode 'apropos)
                  (icicle-apropos-complete-no-display 'nomsg)
                (icicle-prefix-complete-no-display 'nomsg))
            (icicle-filter-alist icicle-candidates-alist (list icicle-current-input)))))
    (when (and candidates  (null (cdr candidates))) (icicle-ORIG-exit-minibuffer)))) ; Single candidate.


;; REPLACE ORIGINAL `choose-completion' in `simple.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Don't iconify frame or bury buffer.
;; Don't strip text properties.
;;
(unless (fboundp 'icicle-ORIG-choose-completion)
  (defalias 'icicle-ORIG-choose-completion (symbol-function 'choose-completion)))

;;;###autoload (autoload 'icicle-choose-completion "icicles")
(defun icicle-choose-completion ()
  "Choose the completion that point is in or next to."
  (interactive)
  (let ((buffer     completion-reference-buffer)
	(base-size  completion-base-size)
        beg end completion)
    (when (and (not (eobp))  (get-text-property (point) 'mouse-face))
      (setq end  (point)
            beg  (1+ (point))))
    (when (and (>= (point) (icicle-start-of-candidates-in-Completions))
               (get-text-property (max (point-min) (1- (point))) 'mouse-face))
      (setq end  (max (point-min) (1- (point)))
            beg  (point)))
    (unless beg	(error "No completion here"))
    (setq beg         (previous-single-property-change beg 'mouse-face)
          end         (or (next-single-property-change end 'mouse-face)  (point-max))
          ;; $$$$ completion  (buffer-substring-no-properties beg end))
          completion  (buffer-substring beg end))
    ;; (let ((owindow  (selected-window)))
    ;;   (if (and (one-window-p t 'selected-frame)  (window-dedicated-p (selected-window)))
    ;;    (iconify-frame (selected-frame)) ; Iconify special buffer's frame
    ;;  (or (window-dedicated-p (selected-window))  (bury-buffer)))
    ;;   (select-window owindow))
    (unless (or (not (member completion icicle-extra-candidates))
                icicle-extra-candidates-dir-insert-p)
      (setq base-size  0))
    (choose-completion-string completion buffer base-size)))


;; REPLACE ORIGINAL `mouse-choose-completion' in `mouse.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Return the number of the completion.
;; Don't strip text properties.
;;
(when (and (fboundp 'mouse-choose-completion)  (not (fboundp 'icicle-ORIG-mouse-choose-completion)))
  (defalias 'icicle-ORIG-mouse-choose-completion (symbol-function 'mouse-choose-completion)))

;;;###autoload (autoload 'icicle-mouse-choose-completion "icicles")
(defun icicle-mouse-choose-completion (event) ; Bound to `mouse-2' in `*Completions*'.
  "Click a completion candidate in buffer `*Completions*', to choose it.
Return the number of the candidate: 0 for first, 1 for second, ..."
  (interactive "e")
  ;; $$$$$ (unless (active-minibuffer-window) (error "Minibuffer is not active"))
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (let ((buffer  (window-buffer))
        ;; $$$$$$ (icicle-orig-buff  buffer)
        choice base-size)
    (with-current-buffer (window-buffer (posn-window (event-start event)))
      (save-excursion
        (when completion-reference-buffer (setq buffer  completion-reference-buffer))
        (setq base-size  completion-base-size)
        (save-excursion
          (goto-char (posn-point (event-start event)))
          (let (beg end)
            (when (and (not (eobp))  (get-text-property (point) 'mouse-face))
              (setq end  (point)
                    beg  (1+ (point))))
            (unless beg (error "No completion here"))
            (setq beg  (previous-single-property-change beg 'mouse-face)
                  end  (or (next-single-property-change end 'mouse-face)  (point-max)))
            ;; $$$$$$ (setq choice  (buffer-substring-no-properties beg end)))))
            (setq choice  (buffer-substring beg end))))))
    ;; $$$$$ (if (eq icicle-orig-buff (get-buffer "*Completions*"))
    ;;    (icicle-remove-Completions-window)
    ;;    (save-selected-window (icicle-remove-Completions-window)))
    (setq icicle-candidate-nb  (icicle-nb-of-cand-at-Completions-pos (posn-point (event-start event))))
    (when (and (icicle-file-name-input-p)  insert-default-directory
               (or (not (member choice icicle-extra-candidates))
                   icicle-extra-candidates-dir-insert-p))
      (let ((dir  (icicle-file-name-directory-w-default icicle-current-input)))
        (with-current-buffer buffer
          (icicle-clear-minibuffer)
          (insert dir)
          (setq choice     (concat dir choice)
                base-size  0))))
    (choose-completion-string choice buffer base-size))
  icicle-candidate-nb)

(defun icicle-nb-of-cand-at-Completions-pos (position)
  "Return number of candidate at POSITION in `*Completions*'.
POSITION is a buffer position."
  (let ((hor-nb  (icicle-nb-of-cand-in-Completions-horiz position)))
    (save-excursion
      (with-current-buffer (get-buffer "*Completions*")
        (goto-char position)
        (if (memq icicle-completions-format '(horizontal nil))
            hor-nb
          (let* ((cols      (icicle-nb-Completions-cols))
                 (nb-cands  (length icicle-completion-candidates))
                 (rows      (/ nb-cands cols)))
            (unless (zerop (% nb-cands cols)) (setq rows  (1+ rows)))
            (icicle-column-wise-cand-nb hor-nb nb-cands rows cols)))))))

(defun icicle-nb-of-cand-in-Completions-horiz (position)
  "Return number of horizontal candidate at POSITION in `*Completions*'.
POSITION is a buffer position."
  (let ((compl-buf  (get-buffer "*Completions*")))
    (unless compl-buf (error "No `*Completions*' buffer"))
    (save-window-excursion
      (set-buffer compl-buf)
      (goto-char position)
      ;; If in a completion, move to its start, and set POSITION there.
      (let ((prop  (get-text-property (max (point-min) (1- (point))) 'mouse-face)))
        (when (and prop  (eq prop (get-text-property (point) 'mouse-face)))
          (goto-char (previous-single-property-change (point) 'mouse-face nil
                                                      (icicle-start-of-candidates-in-Completions)))))
      (setq position  (point))
      ;; Binary search.
      (let ((cand-nb                             (/ (length icicle-completion-candidates) 2))
            (last-nb                             0)
            (icicle-completions-format           'horizontal)
            delta)
        (goto-char (point-min))
        (icicle-move-to-next-completion cand-nb t)
        (while (/= (point) position)
          (setq delta    (max 1 (/ (abs (- cand-nb last-nb)) 2))
                last-nb  cand-nb)
          (cond ((< (point) position)
                 (icicle-move-to-next-completion delta t)
                 (setq cand-nb  (+ cand-nb delta)))
                (t
                 (icicle-move-to-next-completion (- delta) t)
                 (setq cand-nb  (- cand-nb delta)))))
        (set-buffer-modified-p nil)
        (1- cand-nb)))))

(defun icicle-nb-Completions-cols ()
  "Return the number of candidate columns in `*Completions*'."
  (let* ((start       (icicle-start-of-candidates-in-Completions))
         (eol         (save-excursion (goto-char start) (line-end-position)))
         (mouse-chgs  0)
         mousef)
    (save-excursion
      (goto-char start)
      (while (< (point) eol)
        (setq mousef  (next-single-property-change (point) 'mouse-face nil eol))
        (when mousef
          (goto-char mousef)
          (setq mouse-chgs  (1+ mouse-chgs)))))
    ;; Handle the case where the `while' loop is skipped so `mouse-chgs' is still 0.
    (max 1 (/ (1+ mouse-chgs) 2))))     ; Return # of columns.

(defun icicle-column-wise-cand-nb (horiz-nb nb-cands rows cols)
  "Column-wise number of horizontal candidate number HORIZ-NB."
  (let ((row-lim  (- rows (- (* rows cols) nb-cands)))
        (row      (/ horiz-nb cols))
        (col      (mod horiz-nb cols))
        nb)
    (setq nb  (+ row (* col rows)))
    (when (>= row row-lim)
      (setq cols      (1- cols)
            horiz-nb  (- horiz-nb row-lim)
            row       (/ horiz-nb cols)
            col       (mod horiz-nb cols)
            nb        (+ row (* col rows))))
    nb))

(defun icicle-row-wise-cand-nb (vert-nb nb-cands rows cols)
  "Row-wise number of vertical candidate number VERT-NB."
  (let* ((row  (mod vert-nb rows))
         (col  (/ vert-nb rows))
         (nb   (+ col (* row cols)))
         (lim  (- rows (- (* rows cols) nb-cands))))
    (when (> row lim) (setq nb  (- nb (- row lim))))
    nb))


;; REPLACE ORIGINAL `switch-to-completions' defined in `simple.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Selects `*Completions*' window even if on another frame.
;;
(unless (fboundp 'icicle-ORIG-switch-to-completions)
  (defalias 'icicle-ORIG-switch-to-completions (symbol-function 'switch-to-completions)))

;;;###autoload (autoload 'icicle-switch-to-completions "icicles")
(defun icicle-switch-to-completions ()
  "Select the completion list window, `*Completions*'."
  (interactive)
  ;; Make sure we have a completions window.
  (or (get-buffer-window "*Completions*")  (minibuffer-completion-help))
  (let ((window  (get-buffer-window "*Completions*" 0))) ; Added 0 arg.
    (when window
      (select-window window)
      (goto-char (icicle-start-of-candidates-in-Completions)))))

;; The branch that deletes a history element is based on Juri Linkov's
;; `delete-history-element', proposed for Emacs 22 but rejected by RMS.
;;
;;;###autoload (autoload 'icicle-erase-minibuffer-or-history-element "icicles")
(defun icicle-erase-minibuffer-or-history-element () ; Bound to `M-k' in minibuffer.
  "`icicle-erase-minibuffer' or, if using history, delete history element."
  (interactive)
  (if (not (memq last-command '(previous-history-element next-history-element
                                icicle-erase-minibuffer-or-history-element
                                previous-matching-history-element next-matching-history-element)))
      (icicle-erase-minibuffer)
    (let* ((curr-pos  (1- minibuffer-history-position))
           (current   (nth curr-pos (and (boundp minibuffer-history-variable)
                                         (symbol-value minibuffer-history-variable)))))
      (cond ((= minibuffer-history-position 1)
             (set minibuffer-history-variable (and (boundp minibuffer-history-variable)
                                                   (cdr (symbol-value minibuffer-history-variable)))))
            ((> minibuffer-history-position 1)
             (setcdr (nthcdr (- minibuffer-history-position 2)
                             (and (boundp minibuffer-history-variable)
                                  (symbol-value minibuffer-history-variable)))
                     (nthcdr minibuffer-history-position
                             (and (boundp minibuffer-history-variable)
                                  (symbol-value minibuffer-history-variable))))))
      (condition-case nil
          (cond ((memq last-command '(next-history-element next-matching-history-element))
                 (next-history-element 1)
                 (setq this-command  'next-history-element))
                ((memq last-command '(previous-history-element previous-matching-history-element))
                 (next-history-element 1)
                 (previous-history-element 1)
                 (setq this-command  'previous-history-element)))
        (error (icicle-condition-case-no-debug nil
                   (cond ((memq last-command '(next-history-element next-matching-history-element))
                          (previous-history-element 1)
                          (setq this-command  'previous-history-element))
                         ((memq last-command
                                '(previous-history-element previous-matching-history-element))
                          (next-history-element 1)
                          (setq this-command  'next-history-element)))
                 (error nil))))
      (when (and current  (wholenump curr-pos))
        (icicle-msg-maybe-in-minibuffer "Deleted `%s'"
                                        (icicle-propertize current 'face 'icicle-msg-emphasis))))))
 
;;(@* "Icicles commands")

;;; Icicles commands -------------------------------------------------


;;(@* "Minibuffer editing commands")

;;; Minibuffer editing commands  . . . . . . . . . . . . . . . . . . .
;;;
;;; All except `icicle-erase-minibuffer' are bound in the minibuffer to whatever the same
;;; command without `icicle-' is bound to globally.

(defun icicle-looking-at-p (string)
  "Return non-nil if STRING immediately succeeds point."
  (let ((len  (length string)))
    (save-excursion (save-match-data (search-forward string (min (+ (point) len) (point-max)) t)))))

(defun icicle-looking-back-at-p (string)
  "Return non-nil if STRING immediately precedes point."
  (let ((len  (length string)))
    (save-excursion (save-match-data
                      (search-backward string (max (- (point) len) (icicle-minibuffer-prompt-end)) t)))))

;; Used only in `icicle-transpose-chars-magic'.
;;;###autoload (autoload 'icicle-forward-char-magic "icicles")
(defun icicle-forward-char-magic (&optional n)
  "Move forward N chars (backward if N is negative).
Handles Icicles dots (`.') and `icicle-list-join-string'."
  (interactive "p")
  (let ((len-dot   (length icicle-anychar-regexp))
        (len-join  (length icicle-list-join-string)))
    (dotimes (i  (abs n))
      (or (save-match-data
            (if (wholenump n)
                (search-forward icicle-anychar-regexp (min (+ (point) len-dot) (point-max)) t)
              (search-backward icicle-anychar-regexp
                               (max (- (point) len-dot) (icicle-minibuffer-prompt-end)) t)))
          (save-match-data
            (if (wholenump n)
                (search-forward icicle-list-join-string (min (+ (point) len-join) (point-max)) t)
              (search-backward icicle-list-join-string
                               (max (- (point) len-join) (icicle-minibuffer-prompt-end)) t)))
          (forward-char (if (wholenump n) 1 -1))))))

;; Not used.
;;;###autoload (autoload 'icicle-backward-char-magic "icicles")
(defun icicle-backward-char-magic (&optional n)
  "Move backward N chars (forward if N is negative).
Handles Icicles dots (`.') and `icicle-list-join-string'."
  (interactive "p")
  (icicle-forward-char-magic (- n)))


;; Make delete-selection mode recognize it, so region is deleted.
(put 'icicle-backward-delete-char-untabify 'delete-selection 'supersede)

;;;###autoload (autoload 'icicle-backward-delete-char-untabify "icicles")
(defun icicle-backward-delete-char-untabify (n &optional killflag)
  "`backward-delete-char-untabify' + update `*Completions*' with matches.
Handles Icicles dots (`.') and `icicle-list-join-string'."
  (interactive "*p\nP")
  (icicle-call-then-update-Completions #'icicle-backward-delete-char-untabify-magic n killflag))

(defun icicle-backward-delete-char-untabify-magic (n killflag)
  "`backward-delete-char-untabify', but also handle magic chars.
That is, handle dots (`.') and `icicle-list-join-string'."
  (let ((len-dot   (length icicle-anychar-regexp))
        (len-join  (length icicle-list-join-string)))
    (dotimes (i  (abs n))
      (cond ((icicle-looking-back-at-p icicle-anychar-regexp)
             (backward-delete-char-untabify len-dot  killflag))
            ((icicle-looking-at-p icicle-list-join-string)
             (backward-delete-char-untabify len-join killflag))
            (t
             (backward-delete-char-untabify 1        killflag))))))


;; Make delete-selection mode recognize it, so region is deleted.
(put 'icicle-delete-backward-char 'delete-selection 'supersede)

;;;###autoload (autoload 'icicle-delete-backward-char "icicles")
(defun icicle-delete-backward-char (n &optional killflag) ; Bound to `DEL' in minibuffer.
  "`delete-backward-char' and update `*Completions*' with input matches.
Handles Icicles dots (`.') and `icicle-list-join-string'."
  (interactive "*p\nP")
  (icicle-call-then-update-Completions #'icicle-delete-backward-char-magic n killflag))

(defun icicle-delete-backward-char-magic (n killflag)
  "`delete-backward-char', but also handle dots (`.') and join string."
  (let ((len-dot   (length icicle-anychar-regexp))
        (len-join  (length icicle-list-join-string)))
    (dotimes (i  (abs n))
      (cond ((icicle-looking-back-at-p icicle-anychar-regexp)   (delete-char (- len-dot)  killflag))
            ((icicle-looking-back-at-p icicle-list-join-string) (delete-char (- len-join) killflag))
            (t                                                  (delete-char -1           killflag))))))


;; Make delete-selection mode recognize it, so region is deleted.
(put 'icicle-delete-char 'delete-selection 'supersede)

;;;###autoload (autoload 'icicle-delete-char "icicles")
(defun icicle-delete-char (n &optional killflag) ; Bound to `C-d' in minibuffer.
  "`delete-char' and update `*Completions*' with input matches.
Handles Icicles dots (`.') and `icicle-list-join-string'."
  (interactive "*p\nP")
  (icicle-call-then-update-Completions #'icicle-delete-char-magic n killflag))

(defun icicle-delete-char-magic (n killflag)
  "`delete-char', but also handle dot (`.') and `icicle-list-join-string'."
  (let ((len-dot   (length icicle-anychar-regexp))
        (len-join  (length icicle-list-join-string)))
    (dotimes (i  (abs n))
      (cond ((icicle-looking-at-p icicle-anychar-regexp)   (delete-char len-dot  killflag))
            ((icicle-looking-at-p icicle-list-join-string) (delete-char len-join killflag))
            (t                                             (delete-char 1        killflag))))))

;;;###autoload (autoload 'icicle-backward-kill-word "icicles")
(defun icicle-backward-kill-word (arg)  ; Bound to `M-DEL' (`M-backspace') in minibuffer.
  "`backward-kill-word' and update `*Completions*' with input matches.
See description of `backward-kill-word'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-word arg))

;;;###autoload (autoload 'icicle-kill-word "icicles")
(defun icicle-kill-word (arg)           ; Bound to `M-d' in minibuffer.
  "`kill-word' and update `*Completions*' with regexp input matches.
See description of `kill-word'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-word arg))

;;;###autoload (autoload 'icicle-backward-kill-sexp "icicles")
(defun icicle-backward-kill-sexp (arg)  ; Bound to `C-M-backspace' in minibuffer.
  "`backward-kill-sexp' and update `*Completions*' with input matches.
See description of `backward-kill-sexp'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-sexp arg))

;;;###autoload (autoload 'icicle-kill-sexp "icicles")
(defun icicle-kill-sexp (arg)           ; Bound to `C-M-delete' and `C-M-k' in minibuffer.
  "`kill-sexp' and update `*Completions*' with regexp input matches.
See description of `kill-sexp'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-sexp arg))

;;;###autoload (autoload 'icicle-backward-kill-sentence "icicles")
(defun icicle-backward-kill-sentence (arg) ; Bound to `C-x DEL' in minibuffer.
  "`backward-kill-sentence' and update `*Completions*' with input matches.
See description of `backward-kill-sentence'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-sentence arg))

;;;###autoload (autoload 'icicle-kill-sentence "icicles")
(defun icicle-kill-sentence (arg)
  "`kill-sentence' and update `*Completions*' with regexp input matches.
See description of `kill-sentence'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-sentence arg))

;;;###autoload (autoload 'icicle-backward-kill-paragraph "icicles")
(defun icicle-backward-kill-paragraph (arg) ; Bound to `C-backspace' in minibuffer, except for files.
  "`backward-kill-paragraph' and update `*Completions*' with input matches.
See description of `backward-kill-paragraph'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-paragraph arg))

;;;###autoload (autoload 'icicle-kill-paragraph "icicles")
(defun icicle-kill-paragraph (arg)      ; Bound to `C-delete' in minibuffer.
  "`kill-paragraph' and update `*Completions*' with regexp input matches.
See description of `kill-paragraph'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-paragraph arg))

;;;###autoload (autoload 'icicle-kill-line "icicles")
(defun icicle-kill-line (arg)           ; Bound to `C-k' and `deleteline' in minibuffer.
  "`kill-line' and update `*Completions*' with regexp input matches.
See description of `kill-line'."
  (interactive "P")
  (icicle-call-then-update-Completions #'kill-line arg))

;;;###autoload (autoload 'icicle-kill-region "icicles")
(defun icicle-kill-region (beg end)     ; Bound to `C-w' in minibuffer.
;; Don't bother with Emacs 22 optional 3rd arg.
  "`kill-region' and update `*Completions*' with regexp input matches.
See description of `kill-region'."
  (interactive "r")
  (icicle-call-then-update-Completions #'kill-region beg end))

(when (fboundp 'kill-region-wimpy)
  (defun icicle-kill-region-wimpy (beg end) ; Bound to `C-w' in minibuffer.
    "`kill-region-wimpy' and update `*Completions*' with input matches.
See description of `kill-region-wimpy'."
    (interactive "r")
    (icicle-call-then-update-Completions #'kill-region-wimpy beg end)))

;;;###autoload (autoload 'icicle-make-directory "icicles")
(defun icicle-make-directory (dir)
  "Create a directory."
  (interactive
   (let ((enable-recursive-minibuffers  t))
     (list (funcall
            (if (fboundp 'read-directory-name) #'read-directory-name #'read-file-name)
            "Create directory: " default-directory (icicle-file-name-directory-w-default
                                                    (icicle-input-from-minibuffer))))))
  (setq dir  (directory-file-name (expand-file-name dir)))
  (while (file-exists-p dir)            ; This will cause Tramp to access if remote, but that's OK here.
    (message "%s already exists" dir) (sit-for 1)
    (let ((enable-recursive-minibuffers  t))
      (setq dir  (funcall (if (fboundp 'read-directory-name) #'read-directory-name #'read-file-name)
                          "Create directory: " default-directory (icicle-file-name-directory-w-default
                                                                  (icicle-input-from-minibuffer))))))
  ;;(setq dir  (directory-file-name (expand-file-name dir)))
  (if (not (y-or-n-p (format "Really create %s? " (file-name-as-directory dir))))
      (message "Directory creation canceled")
    (make-directory dir 'PARENTS-TOO)
    (unless (file-accessible-directory-p dir)
      (error "Could not create %s" (file-name-as-directory dir)))
    (message "Created %s" (file-name-as-directory dir))))

;;;###autoload (autoload 'icicle-up-directory "icicles")
(defun icicle-up-directory () ; Bound to `C-backspace' in minibuffer, for file-name completion.
  "Replace minibuffer input with parent directory, then upate `*Completions*'."
  (interactive)
  (icicle-call-then-update-Completions #'icicle-replace-input-w-parent-dir))

;;;###autoload (autoload 'icicle-replace-input-w-parent-dir "icicles")
(defun icicle-replace-input-w-parent-dir ()
  "Replace minibuffer input with the parent directory."
  (interactive)
  (goto-char (point-max))
  (let ((directoryp  (equal ?/ (char-before)))
        (bob         (icicle-minibuffer-prompt-end)))
    (while (and (> (point) bob)  (not (equal ?/ (char-before))))  (delete-char -1))
    (when directoryp
      (delete-char -1)
      (while (and (> (point) bob)  (not (equal ?/ (char-before))))  (delete-char -1)))))

;;; ;;;###autoload (autoload 'icicle-kill-failed-input "icicles")
;;; (defun icicle-kill-failed-input ()      ; Bound to `C-M-l' in minibuffer during completion.
;;;   "Kill (delete) the part of the input that does not complete.
;;; Repeat to delete more."
;;;   (interactive)
;;;   (goto-char (max (point-min) (1- (point))))
;;;   (while (and (not (bobp))
;;;               (memq (get-text-property (point) 'face)
;;;                     '(icicle-input-completion-fail icicle-input-completion-fail-lax)))
;;;     (delete-char 1)
;;;     (backward-char 1))
;;;   (unless (eobp) (forward-char))
;;;   (icicle-highlight-input-noncompletion))

;;;###autoload (autoload 'icicle-goto/kill-failed-input "icicles")
(defun icicle-goto/kill-failed-input () ; Bound to `C-M-l' in minibuffer during completion.
  "Go to start of input portion that does not complete.  Repeat to kill.
Kill (delete) the part of the input that does not complete.
Repeat to delete more."
  (interactive)
  (if (eq last-command this-command)
      (unless (eobp) (kill-line))
    (when (and (overlayp icicle-input-completion-fail-overlay)
               (overlay-start icicle-input-completion-fail-overlay))
      (goto-char (overlay-start icicle-input-completion-fail-overlay)))))

;;;###autoload (autoload 'icicle-transpose-chars "icicles")
(defun icicle-transpose-chars (arg)     ; Bound to `C-t' in minibuffer.
  "`transpose-chars' and update `*Completions*' with regexp input matches.
Handles Icicles dots (`.') and `icicle-list-join-string'."
  (interactive "*P")
  (icicle-call-then-update-Completions #'icicle-transpose-chars-magic arg))

(defun icicle-transpose-chars-magic (arg)
  "`transpose-chars', but handle dots (`.') and `icicle-list-join-string'."
  (and (null arg)  (eolp)  (icicle-forward-char-magic -1))
  (transpose-subr 'icicle-forward-char-magic (prefix-numeric-value arg)))

;;;###autoload (autoload 'icicle-transpose-words "icicles")
(defun icicle-transpose-words (arg)     ; Bound to `M-t' in minibuffer.
  "`transpose-words' and update `*Completions*' with regexp input matches.
See description of `transpose-words'."
  (interactive "*p")
  (icicle-call-then-update-Completions #'transpose-words arg))

;;;###autoload (autoload 'icicle-transpose-sexps "icicles")
(defun icicle-transpose-sexps (arg)    ; Bound to `C-M-t' in minibuffer.
  "`transpose-sexps' and update `*Completions*' with regexp input matches.
See description of `transpose-sexps'."
  (interactive "*p")
  (icicle-call-then-update-Completions #'transpose-sexps arg))

;;;###autoload (autoload 'icicle-yank "icicles")
(defun icicle-yank (arg)                ; Bound to `C-y' and `S-insert' in minibuffer.
  "`yank' and update `*Completions*' with regexp input matches.
See description of `yank'."
  (interactive "*P")
  (icicle-call-then-update-Completions #'yank arg))

;;;###autoload (autoload 'icicle-yank-pop "icicles")
(defun icicle-yank-pop (arg)            ; Bound to `M-y' and `M-insert' in minibuffer.
  "`yank-pop' and update `*Completions*' with regexp input matches.
See description of `yank-pop'."
  (interactive "*p")
  (icicle-call-then-update-Completions #'yank-pop arg))

(eval-after-load "second-sel"
  '(progn (defun icicle-yank-secondary () ; Bound to `C-M-y' in minibuffer.
            "Insert the secondary selection at point.
Move point to the end of the inserted text.  Does not change mark."
            (interactive "*")
            (icicle-call-then-update-Completions #'yank-secondary))

    ;; Tell `delete-selection-mode' to replace active region by yanked secondary selection.
    (put 'icicle-yank-secondary 'delete-selection 'yank)))


;; Tell `delete-selection-mode' to replace active region by yanked secondary selection.
(put 'icicle-mouse-yank-secondary 'delete-selection 'yank)

;;;###autoload (autoload 'icicle-mouse-yank-secondary "icicles")
(defun icicle-mouse-yank-secondary (event) ; Bound to `M-mouse-2' in minibuffer.
  "Insert the secondary selection where you click.
Move point to the end of the inserted text.
If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
  (interactive "*e")
  (if (fboundp 'yank-secondary)         ; In `mouse+.el'.
      (icicle-call-then-update-Completions #'mouse-yank-secondary event current-prefix-arg)
    (icicle-call-then-update-Completions #'mouse-yank-secondary event)))


;; Make delete-selection mode recognize self-insertion, so it replaces region text.
(put 'icicle-self-insert 'delete-selection t)

;;;###autoload (autoload 'icicle-self-insert "icicles")
(defun icicle-self-insert (n) ;; Bound in minibuffer to stuff bound globally to `self-insert-command'.
  "`self-insert' and update `*Completions*' with regexp input matches.
See description of `self-insert'."
  (interactive "p")
  (if executing-kbd-macro
      (funcall #'self-insert-command n)
    (icicle-call-then-update-Completions #'self-insert-command n)))

;;;###autoload (autoload 'icicle-insert-a-space "icicles")
(defun icicle-insert-a-space ()
  "Insert a space.
For convenience in the minibuffer - does the same thing as `C-q SPC'.
To use this, bind it to some key sequence in keymaps
`minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', and
`minibuffer-local-must-match-map'."
  (interactive) (insert ?\ ))

;;;###autoload (autoload 'icicle-insert-dot-command "icicles")
(defun icicle-insert-dot-command (&optional arg) ; Bound to `.' in minibuffer during completion.
  "Insert `icicle-dot-string': either `.' or `icicle-anychar-regexp'.
With a numeric prefix argument, insert the dot that many times.

With a plain prefix arg (`C-u'), insert the opposite kind of dot
\(once) from what is indicated by the current value of
`icicle-dot-string'."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (if (consp arg)
      (let ((opposite  (if (string= icicle-dot-string-internal ".")
                           (icicle-anychar-regexp)
                         (let ((strg  "."))
                           (add-text-properties
                            0 1 '(icicle-user-plain-dot t rear-nonsticky (icicle-user-plain-dot))
                            strg)
                           strg))))
        (if executing-kbd-macro
            (insert opposite)
          (icicle-call-then-update-Completions (lambda () (insert opposite)))))
    (setq arg  (prefix-numeric-value arg))
    (if executing-kbd-macro
        (funcall #'icicle-insert-dot arg)
      (icicle-call-then-update-Completions #'icicle-insert-dot arg))))

(defun icicle-insert-dot (n)
  "Insert `icicle-dot-string' N times."
  (dotimes (i n)
    (if (not (string= icicle-dot-string-internal "."))
        (insert (icicle-anychar-regexp))
      (insert ".")
      (add-text-properties (max (point-min) (1- (point))) (point)
                           '(icicle-user-plain-dot t rear-nonsticky t)))))

(defun icicle-anychar-regexp ()
  "Return a regexp that matches any single character, including newline.
The value returned is like that of constant `icicle-anychar-regexp',
but the `display' string is unique for each call."
  (let ((strg  (copy-sequence "\\(.\\|[\n]\\)")))
    (set-text-properties 0 (length strg)
                         (if icicle-dot-show-regexp-flag
                             '(face highlight rear-nonsticky t)
                           `(display ,(copy-sequence ".") face highlight rear-nonsticky t))
                         strg)
    strg))

;;;###autoload (autoload 'icicle-erase-minibuffer "icicles")
(defun icicle-erase-minibuffer ()       ; Bound to `M-S-backspace', `M-S-delete' in minibuffer.
  "Delete all user input in the minibuffer, then update completions."
  (interactive)
  (icicle-call-then-update-Completions #'icicle-clear-minibuffer))
 
;;(@* "Commands to sort completion candidates")

;;; Commands to sort completion candidates . . . . . . . . . . . . . .

;; We don't bother to define a command for the sort functions `icicle-prefix-keys-first-p' and
;; `icicle-command-names-alphabetic-p'.  They are bound in `icicle-complete-keys'.

;; The order here defines the reverse order of `icicle-sort-orders-alist'.
;; The first here is also the default sort order.  Entries are traversed by `C-,' in
;; `icicle-sort-orders-alist' order.

;;;###autoload (autoload 'icicle-sort-alphabetical "icicles-mcmd")
(icicle-define-sort-command "alphabetical" ; `icicle-sort-alphabetical'
    icicle-case-string-less-p
  "Sort completion candidates alphabetically.
Ignore letter case if `completion-ignore-case' or `case-fold-search'
is non-nil.")

;;;###autoload (autoload 'icicle-sort-special-candidates-first "icicles-mcmd")
(icicle-define-sort-command "special candidates first" ; `icicle-sort-special-candidates-first'
    icicle-special-candidates-first-p
  "Sort completion candidates by putting special candidates first.
Otherwise, sorting is alphabetical.  Ignore letter case if
`completion-ignore-case' or `case-fold-search' is non-nil.")

;;;###autoload (autoload 'icicle-sort-extra-candidates-first "icicles-mcmd")
(icicle-define-sort-command "extra candidates first" ; `icicle-sort-extra-candidates-first'
    icicle-extra-candidates-first-p
  "Sort completion candidates by putting extra candidates first.
Otherwise, sorting is alphabetical.  Ignore letter case if
`completion-ignore-case' or `case-fold-search' is non-nil.
An extra candidate is one that is a member of
`icicle-extra-candidates'.")

;;;###autoload (autoload 'icicle-sort-proxy-candidates-first "icicles-mcmd")
(icicle-define-sort-command "proxy candidates first" ; `icicle-sort-proxy-candidates-first'
    icicle-proxy-candidate-first-p
  "Sort completion candidates by putting proxy candidates first.
Otherwise, sorting is alphabetical.  Ignore letter case if
`completion-ignore-case' or `case-fold-search' is non-nil.")

;;;###autoload (autoload 'icicle-sort-case-insensitive "icicles-mcmd")
(icicle-define-sort-command "case insensitive" ; `icicle-sort-case-insensitive'
    icicle-case-insensitive-string-less-p
  "Sort completion candidates alphabetically, but case-insenstively.")

;;;###autoload (autoload 'icicle-sort-by-2nd-parts-alphabetically "icicles-mcmd")
(icicle-define-sort-command "by 2nd parts alphabetically" ; `icicle-sort-by-2nd-parts-alphabetically'
    icicle-2nd-part-string-less-p
  "Sort multi-completion candidates alphabetically by their second parts.
After that, sort alphabetically by the first parts.  Ignore letter
case if `completion-ignore-case' or `case-fold-search' is non-nil.")

;;;###autoload (autoload 'icicle-sort-by-last-file-access-time "icicles-mcmd")
(icicle-define-sort-command "by last file access time"
    icicle-last-accessed-first-p        ; `icicle-sort-by-last-file-access-time'
  "Sort file-name completion candidates in order of last access.
If not doing file-name completion, then sort alphabetically.")

;;;###autoload (autoload 'icicle-sort-by-last-file-modification-time "icicles-mcmd")
(icicle-define-sort-command "by last file modification time"
    icicle-last-modified-first-p        ; `icicle-sort-by-last-file-modification-time'
  "Sort file-name completion candidates in order of last modification.
If not doing file-name completion, then sort alphabetically.")

;;;###autoload (autoload 'icicle-sort-by-file-type "icicles-mcmd")
(icicle-define-sort-command "by file type" ; `icicle-sort-by-file-type'
    icicle-file-type-less-p
  "Sort file-name completion candidates by file type.
Directories sort first, alphabetically.
Then sort by file type (extension), alphabetically.
Sort names that have the same extension alphabetically.
If not doing file-name completion, sort candidates alphabetically.")

;;;###autoload (autoload 'icicle-sort-by-directories-first "icicles-mcmd")
(icicle-define-sort-command "by directories first" ; `icicle-sort-by-directories-first'
    icicle-dirs-first-p
  "Sort file-name completion candidates so that directories are first.
If not doing file-name completion, then sort alphabetically.")

;;;###autoload (autoload 'icicle-sort-by-directories-last "icicles-mcmd")
(icicle-define-sort-command "by directories last" ; `icicle-sort-by-directories-last'
    icicle-dirs-last-p
  "Sort file-name completion candidates so that directories are last.
If not doing file-name completion, then sort alphabetically.")

;;;###autoload (autoload 'icicle-sort-by-last-use-as-input "icicles-mcmd")
(icicle-define-sort-command "by last use as input" ; `icicle-sort-by-last-use-as-input'
    icicle-most-recent-first-p
  "Sort completion candidates in order of last use as minibuffer input.")

;;;###autoload (autoload 'icicle-sort-by-previous-use-alphabetically "icicles-mcmd")
(icicle-define-sort-command "by previous use alphabetically"
    icicle-historical-alphabetic-p      ; `icicle-sort-by-previous-use-alphabetically'
  "Sort completion candidates by previous use and alphabetically.
Candidates matching previous inputs are available first.  Candidates
are in two groups, each of which is sorted alphabetically separately:
those matching previous inputs, followed by those that have not yet
been used.")

;;;###autoload (autoload 'icicle-sort-by-abbrev-frequency "icicles-mcmd")
(icicle-define-sort-command "by abbrev frequency" ; `icicle-sort-by-abbrev-frequency'
    icicle-command-abbrev-used-more-p
  "Sort abbrev completion candidates by frequency of use
Otherwise, sort alphabetically.  Ignore letter case if
`completion-ignore-case' or `case-fold-search' is non-nil.")

;;;###autoload (autoload 'icicle-sort-turned-OFF "icicles-mcmd")
(icicle-define-sort-command "turned OFF" nil ; `icicle-sort-turned-OFF'
  "Do not sort completion candidates.")

;;;###autoload (autoload 'icicle-dispatch-M-_ "icicles")
(defun icicle-dispatch-M-_ ()           ; Bound to `M-_' in minibuffer.
  "Do the right thing for `M-_'.
During Icicles search, call `icicle-toggle-search-replace-whole'.
Otherwise, call `icicle-toggle-ignored-space-prefix'.
Bound to `M-_' in the minibuffer."
  (interactive)
  (if icicle-searching-p (icicle-toggle-search-replace-whole) (icicle-toggle-ignored-space-prefix)))

;;; No longer used.
;;; (defun icicle-dispatch-C-comma ()
;;;   "Do the right thing for `C-,'.
;;; When candidate sorting is possible, call `icicle-change-sort-order'.
;;; When searching, call `icicle-toggle-search-replace-whole'.
;;; Otherwise, do nothing.
;;;
;;; Bound to `C-,' in the minibuffer."
;;;   (interactive)
;;;   (cond (icicle-searching-p (icicle-toggle-search-replace-whole))
;;;         (icicle-inhibit-sort-p (message "Cannot sort candidates now"))
;;;         (t (call-interactively #'icicle-change-sort-order))))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-ignoring-comments "icicles")
(defalias 'toggle-icicle-ignoring-comments 'icicle-toggle-ignoring-comments)
;;;###autoload (autoload 'icicle-toggle-ignoring-comments "icicles")
(defun icicle-toggle-ignoring-comments () ; Bound to `C-M-;' in minibuffer.
  "Toggle the value of option `icicle-ignore-comments-flag'.
If option `ignore-comments-flag' is defined (in library
`thing-cmds.el') then it too is toggled.
Bound to `C-M-;' in the minibuffer."
  (interactive)
  (setq icicle-ignore-comments-flag  (not icicle-ignore-comments-flag))
  (when (boundp 'ignore-comments-flag) (setq ignore-comments-flag  (not ignore-comments-flag)))
  (icicle-msg-maybe-in-minibuffer
   "Ignoring comments is now %s" (icicle-propertize (if icicle-ignore-comments-flag "ON" "OFF")
                                                    'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-search-replace-common-match "icicles")
(defalias 'toggle-icicle-search-replace-common-match 'icicle-toggle-search-replace-common-match)
;;;###autoload (autoload 'icicle-toggle-search-replace-common-match "icicles")
(defun icicle-toggle-search-replace-common-match () ; Bound to `M-;' in minibuffer.
  "Toggle the value of `icicle-search-replace-common-match-flag'.
Note that that option has no effect if the value of option
`icicle-expand-input-to-common-match' does not imply expansion.
Bound to `M-;' in the minibuffer."
  (interactive)
  (setq icicle-search-replace-common-match-flag  (not icicle-search-replace-common-match-flag))
  (icicle-msg-maybe-in-minibuffer
   "Replacing expanded common match is now %s"
   (icicle-propertize (if icicle-search-replace-common-match-flag "ON" "OFF")
                      'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-search-replace-whole "icicles")
(defalias 'toggle-icicle-search-replace-whole 'icicle-toggle-search-replace-whole)
;;;###autoload (autoload 'icicle-toggle-search-replace-whole "icicles")
(defun icicle-toggle-search-replace-whole ()
  "Toggle the value of `icicle-search-replace-whole-candidate-flag'.
Bound to `M-_' in the minibuffer when searching."
  (interactive)
  (setq icicle-search-replace-whole-candidate-flag  (not icicle-search-replace-whole-candidate-flag))
  (icicle-msg-maybe-in-minibuffer
   "Replacing whole search context is now %s"
   (icicle-propertize (if icicle-search-replace-whole-candidate-flag "ON" "OFF")
                      'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-dot "icicles")
(defalias 'toggle-icicle-dot 'icicle-toggle-dot)
;;;###autoload (autoload 'toggle-icicle-. "icicles")
(defalias 'toggle-icicle-.   'icicle-toggle-dot)
;;;###autoload (autoload 'icicle-toggle-. "icicles")
(defalias 'icicle-toggle-.   'icicle-toggle-dot)
;;;###autoload (autoload 'icicle-toggle-dot "icicles")
(defun icicle-toggle-dot ()             ; Bound to `C-M-.' in minibuffer.
  "Toggle `icicle-dot-string' between `.' and `icicle-anychar-regexp'.
Bound to `C-M-.' in the minibuffer."
  (interactive)
  (setq icicle-dot-string  (if (string= icicle-dot-string ".") (icicle-anychar-regexp) "."))
  (icicle-msg-maybe-in-minibuffer
   (cond ((string= icicle-dot-string ".")
          (icicle-convert-dots (equal icicle-current-input icicle-last-input) t)
          (format "`%s' now matches any char %s newline"
                  (icicle-propertize "." 'face 'icicle-msg-emphasis)
                  (icicle-propertize "EXCEPT" 'face 'icicle-msg-emphasis)))
         (t
          (icicle-convert-dots (equal icicle-current-input icicle-last-input))
          (format "`%s' now matches any char, including %s"
                  (icicle-propertize "." 'face 'icicle-msg-emphasis)
                  (icicle-propertize "NEWLINE" 'face 'icicle-msg-emphasis)))))
  (setq icicle-dot-string-internal  icicle-dot-string))

(defun icicle-convert-dots (&optional no-confirm-p plainp)
  "Convert existing dots.
Optional arg NO-CONFIRM-P means don't ask user for confirmation.
Optional arg PLAINP means convert to plain `.'.
  Otherwise, convert to `icicle-anychar-regexp'."
  (if plainp
      (save-excursion
        (when (and (goto-char (icicle-minibuffer-prompt-end))
                   (search-forward icicle-anychar-regexp nil t))
          (goto-char (icicle-minibuffer-prompt-end))
          (while (search-forward icicle-anychar-regexp nil t)
            (replace-match "." nil t))))
    (save-excursion
      (when (and (goto-char (icicle-minibuffer-prompt-end))  (search-forward "." nil t))
        (goto-char (icicle-minibuffer-prompt-end))
        (let ((allp  nil))
          (while (search-forward "." nil t)
            ;; If we hit a plain dot inserted by user explicitly, ask if we should convert all such.
            (when (and (not allp)
                       (get-text-property (match-beginning 0) 'icicle-user-plain-dot)
                       (not no-confirm-p)
                       (y-or-n-p "Should all dots (`.') in current input match newlines too? "))
              (setq allp  t))
            (when (or allp  (not (get-text-property (match-beginning 0) 'icicle-user-plain-dot)))
              (replace-match (icicle-anychar-regexp) nil t))))))))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
(when (require 'image-dired nil t)      ; Emacs 22+.
  (defalias 'cycle-icicle-image-file-thumbnail 'icicle-toggle-show-image-file-thumbnail)
  (defun icicle-cycle-image-file-thumbnail () ; Bound to `C-x t' in minibuffer.
    "Toggle `icicle-image-files-in-Completions'.
This has no effect if you do not have library `image-dired.el' (Emacs 23+).
Bound to `C-x t' in the minibuffer."
    (interactive)
    (if (not (require 'image-dired nil t))
        (message "No-op: this command requires library `image-dired.el'")
      (setq icicle-image-files-in-Completions  (case icicle-image-files-in-Completions
                                                 ((nil)       'image-only)
                                                 (image-only  t)
                                                 (t           nil)))
      (icicle-complete-again-update)
      (icicle-msg-maybe-in-minibuffer
       (case icicle-image-files-in-Completions
         ((nil)       (format "Image files in `*Completions*': showing only %s  Next: only IMAGES"
                              (icicle-propertize "NAMES" 'face 'icicle-msg-emphasis)))
         (image-only  (format "Image files in `*Completions*': showing only %s  Next: IMAGES and NAMES"
                              (icicle-propertize "IMAGES" 'face 'icicle-msg-emphasis)))
         (t           (format "Image files in `*Completions*': showing %s  Next: only NAMES"
                              (concat (icicle-propertize "IMAGES" 'face 'icicle-msg-emphasis)
                                      " and "
                                      (icicle-propertize "NAMES" 'face 'icicle-msg-emphasis)))))))))

;; This works hand in hand with `icicle-maybe-sort-maybe-truncate'.  Update both together.
;;
;;;###autoload (autoload 'icicle-doremi-increment-max-candidates+ "icicles")
(defun icicle-doremi-increment-max-candidates+ (&optional increment) ; `C-x #' in minibuffer
  "Change `icicle-max-candidates' incrementally.
Use `up', `down' or the mouse wheel to increase or decrease.  You can
 use the `Meta' key (e.g. `M-up') to increment in larger steps.
You can use a numeric prefix arg to specify the increment.
A plain prefix arg (`C-u') resets `icicle-max-candidates' to nil,
 meaning no limit."
  (interactive "P")
  (cond ((consp increment)
         (setq icicle-max-candidates  'RESET) ; `icicle-maybe-sort-maybe-truncate' will reset to nil.
         (icicle-msg-maybe-in-minibuffer "No longer any limit on number of candidates"))
        (t
         (setq increment  (prefix-numeric-value increment))
         (unless (require 'doremi nil t) (error "This command needs library `doremi.el'."))
         (let ((mini  (active-minibuffer-window)))
           (unwind-protect
                (save-selected-window
                  (select-window (minibuffer-window))
                  (unless icicle-completion-candidates (message "Hit `TAB' or `S-TAB'"))
                  (let ((enable-recursive-minibuffers  t)
                        (nb-cands                      (length icicle-completion-candidates)))
                    (when (or (not (integerp icicle-max-candidates)) ; Not `RESET' or nil.
                              (> icicle-max-candidates nb-cands))
                      (setq icicle-max-candidates  nb-cands))
                    (when (zerop icicle-max-candidates) (setq icicle-max-candidates  10))
                    (doremi (lambda (new-val)
                              (setq icicle-max-candidates  (setq new-val  (doremi-limit new-val 2 nil)))
                              (unless (input-pending-p)
                                (let ((icicle-edit-update-p  t)
                                      (icicle-last-input     nil))
                                  (funcall (or icicle-last-completion-command
                                               (if (eq icicle-current-completion-mode 'prefix)
                                                   #'icicle-prefix-complete
                                                 #'icicle-apropos-complete)))
                                  (run-hooks 'icicle-update-input-hook)))
                              new-val)
                            icicle-max-candidates
                            increment))
                  (setq unread-command-events  ()))
             (unless mini (icicle-remove-Completions-window)))))))

;;;###autoload (autoload 'icicle-doremi-increment-swank-timeout+ "icicles")
(defun icicle-doremi-increment-swank-timeout+ () ; Bound to `C-x 1' in minibuffer (swank only)
  "Change `icicle-swank-timeout' incrementally.
Use `up', `down' or the mouse wheel to increase or decrease.  You can
use the `Meta' key (e.g. `M-up') to increment in larger steps."
  (interactive)
  (icicle-doremi-increment-variable+ 'icicle-swank-timeout 1000))

;;;###autoload (autoload 'icicle-doremi-increment-swank-prefix-length+ "icicles")
(defun icicle-doremi-increment-swank-prefix-length+ () ; Bound to `C-x 2' in minibuffer (swank only)
  "Change `icicle-swank-prefix-length' incrementally.
Use `up', `down' or the mouse wheel to increase or decrease.  You can
use the `Meta' key (e.g. `M-up') to increment in larger steps."
  (interactive)
  (icicle-doremi-increment-variable+ 'icicle-swank-prefix-length 1))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'cycle-icicle-TAB-completion-method "icicles")
(defalias 'cycle-icicle-TAB-completion-method 'icicle-next-TAB-completion-method)
;;;###autoload (autoload 'icicle-next-TAB-completion-method "icicles")
(defun icicle-next-TAB-completion-method (temporary-p) ; Bound to `C-(' in minibuffer.
  "Cycle to the next `TAB' completion method.
Bound to \\<minibuffer-local-completion-map>`\\[icicle-next-TAB-completion-method]' \
in the minibuffer.
Option `icicle-TAB-completion-methods' determines the TAB completion
methods that are available.

With a prefix argument, the newly chosen method is used only for the
current command.  More precisely, the previously active method is
restored as soon as you return to the top level."
  (interactive "P")
  (unless icicle-current-TAB-method     ; nil means the same as the default (first).
    (setq icicle-current-TAB-method  (car icicle-TAB-completion-methods)))
  (if temporary-p
      (unless (get 'icicle-last-top-level-command 'icicle-current-TAB-method)
        (put 'icicle-last-top-level-command 'icicle-current-TAB-method icicle-current-TAB-method))
    (put 'icicle-last-top-level-command 'icicle-current-TAB-method nil))

  (let ((now  (memq icicle-current-TAB-method icicle-TAB-completion-methods))
        following)
    (setq icicle-current-TAB-method  (or (cadr now)  (car icicle-TAB-completion-methods))
          following                  (or (cadr (memq icicle-current-TAB-method
                                                     icicle-TAB-completion-methods))
                                         (car icicle-TAB-completion-methods)))
    ;; Skip any method that is not currently supported.
    (while (or (and (eq icicle-current-TAB-method 'fuzzy)        (not (featurep 'fuzzy-match)))
               (and (eq icicle-current-TAB-method 'vanilla)      (not (boundp 'completion-styles)))
               (and (eq icicle-current-TAB-method 'swank)        (not (featurep 'el-swank-fuzzy))))
      (setq now                        (memq icicle-current-TAB-method icicle-TAB-completion-methods)
            icicle-current-TAB-method  (or (cadr now)  (car icicle-TAB-completion-methods))))
    ;; Skip any method that is not currently supported.
    (while (or (and (eq following 'fuzzy)    (not (featurep 'fuzzy-match)))
               (and (eq following 'vanilla)  (not (boundp 'completion-styles)))
               (and (eq following 'swank)    (not (featurep 'el-swank-fuzzy))))
      (setq following  (or (cadr (memq icicle-current-TAB-method icicle-TAB-completion-methods))
                           (car icicle-TAB-completion-methods))))
    ;; $$$$$$ Inhibiting sorting is not correct for file-name completion, and sorting would not be
    ;;        restored when change back to non-fuzzy.
    ;; (when (eq 'fuzzy icicle-current-TAB-method) (setq icicle-inhibit-sort-p  t))
    (icicle-msg-maybe-in-minibuffer
     "TAB completion is %s %s  Next: %s"
     (icicle-propertize (icicle-upcase (symbol-name icicle-current-TAB-method))
                        'face 'icicle-msg-emphasis)
     (if temporary-p (concat "for " (icicle-propertize "this command" 'face 'icicle-msg-emphasis)) "now.")
     (if temporary-p "" (icicle-upcase (symbol-name following)))))
  (cond ((and (eq icicle-current-TAB-method 'swank)  (fboundp 'doremi))
         (define-key minibuffer-local-completion-map (icicle-kbd "C-x 1")
           'icicle-doremi-increment-swank-timeout+)
         (define-key minibuffer-local-must-match-map (icicle-kbd "C-x 1")
           'icicle-doremi-increment-swank-timeout+)
         (define-key minibuffer-local-completion-map (icicle-kbd "C-x 2")
           'icicle-doremi-increment-swank-prefix-length+)
         (define-key minibuffer-local-must-match-map (icicle-kbd "C-x 2")
           'icicle-doremi-increment-swank-prefix-length+))
        ((fboundp 'doremi)
         (define-key minibuffer-local-completion-map (icicle-kbd "C-x 1") nil)
         (define-key minibuffer-local-must-match-map (icicle-kbd "C-x 1") nil)
         (define-key minibuffer-local-completion-map (icicle-kbd "C-x 2") nil)
         (define-key minibuffer-local-must-match-map (icicle-kbd "C-x 2") nil))))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'cycle-icicle-S-TAB-completion-method "icicles")
(defalias 'cycle-icicle-S-TAB-completion-method 'icicle-next-S-TAB-completion-method)
;;;###autoload (autoload 'icicle-next-S-TAB-completion-method "icicles")
(defun icicle-next-S-TAB-completion-method (temporary-p) ; Bound to `M-(' in minibuffer.
  "Cycle to the next `S-TAB' completion method.
Bound to `M-(' in the minibuffer.
Option `icicle-S-TAB-completion-methods-alist' customizes the
available TAB completion methods.

With a prefix argument, the newly chosen method is used only for the
current command.  More precisely, the previously active method is
restored as soon as you return to the top level."
  (interactive "P")
  (if temporary-p
      (unless (get 'icicle-last-top-level-command 'icicle-apropos-complete-match-fn)
        (put 'icicle-last-top-level-command 'icicle-apropos-complete-match-fn
             icicle-apropos-complete-match-fn))
    (put 'icicle-last-top-level-command 'icicle-apropos-complete-match-fn nil))
  (let ((entry  (rassq icicle-apropos-complete-match-fn icicle-S-TAB-completion-methods-alist))
        following)
    (setq icicle-apropos-complete-match-fn       (or (cdadr (member
                                                             entry icicle-S-TAB-completion-methods-alist))
                                                     (cdar icicle-S-TAB-completion-methods-alist))
          following                              (or (caadr (member
                                                             (rassq icicle-apropos-complete-match-fn
                                                                    icicle-S-TAB-completion-methods-alist)
                                                             icicle-S-TAB-completion-methods-alist))
                                                     (caar icicle-S-TAB-completion-methods-alist))
          icicle-last-apropos-complete-match-fn  icicle-apropos-complete-match-fn) ; Backup copy.
    (icicle-msg-maybe-in-minibuffer
     "S-TAB completion is %s%s %s  Next: %s"
     (icicle-propertize (icicle-upcase (car (rassq icicle-apropos-complete-match-fn
                                                   icicle-S-TAB-completion-methods-alist)))
                        'face 'icicle-msg-emphasis)
     (if (memq icicle-apropos-complete-match-fn
               '(icicle-levenshtein-match icicle-levenshtein-strict-match))
         (icicle-propertize (format " (%d)" icicle-levenshtein-distance) 'face 'icicle-msg-emphasis)
       "")
     (if temporary-p (concat "for " (icicle-propertize "this command" 'face 'icicle-msg-emphasis)) "now.")
     (if temporary-p "" (icicle-upcase following)))))
    ;; (icicle-complete-again-update) ; No - too slow for some completion methods.

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'cycle-icicle-sort-order "icicles")
(defalias 'cycle-icicle-sort-order 'icicle-change-sort-order)
;;;###autoload (autoload 'icicle-change-sort-order "icicles")
(defun icicle-change-sort-order (&optional arg alternativep) ; Bound to `C-,' in minibuffer.
  "Choose a sort order.
With a numeric prefix arg, reverse the current sort order.

If plain `C-u' is used or `C-u' is not used at all:

- Use completion if `icicle-change-sort-order-completion-flag' is
  non-nil and no prefix arg is used, or if it is nil and a prefix arg
  is used.

- Otherwise, just cycle to the next sort order.

This command updates `icicle-sort-comparer'.  Non-interactively,
optional arg ALTERNATIVEP means change the current alternative sort
order instead, updating `icicle-alternative-sort-comparer'."
  (interactive "P")
  (setq icicle-sort-orders-alist  (delq nil icicle-sort-orders-alist)) ; Purge any nil entries.
  (if (and (interactive-p)  icicle-inhibit-sort-p)
      (icicle-msg-maybe-in-minibuffer "Cannot sort candidates now")
    (if (and arg  (not (consp arg)))
        (icicle-reverse-sort-order)
      (let ((following-order  nil)
            next-order)
        (cond ((or (and icicle-change-sort-order-completion-flag        (not arg)) ; Use completion.
                   (and (not icicle-change-sort-order-completion-flag)  arg))
               (setq next-order  (let ((icicle-whole-candidate-as-text-prop-p   nil)
                                       (enable-recursive-minibuffers            t)
                                       (icicle-must-pass-after-match-predicate  nil))
                                   (save-selected-window
                                     (completing-read
                                      (format "New %ssort order: " (if alternativep "alternative " ""))
                                      (icicle-current-sort-functions)
                                      nil t))))
               (set (if alternativep 'icicle-alternative-sort-comparer 'icicle-sort-comparer)
                    (cdr (assoc next-order icicle-sort-orders-alist))))
              (t                        ; Cycle to next sort order.
               (let ((orders  (mapcar #'car (icicle-current-sort-functions))))
                 (setq next-order       (or (cadr (memq (icicle-current-sort-order alternativep) orders))
                                            (car orders))
                       following-order  (or (cadr (memq next-order orders))  (car orders)))
                 (set (if alternativep 'icicle-alternative-sort-comparer 'icicle-sort-comparer)
                      (cdr (assoc next-order icicle-sort-orders-alist))))))
        (icicle-complete-again-update)
        (icicle-msg-maybe-in-minibuffer
         "%sorting is now %s.  Reverse: `C-9 C-,'%s"
         (if alternativep "Alternative s" "S")
         (icicle-propertize (concat next-order (and icicle-reverse-sort-p  ", REVERSED"))
                            'face 'icicle-msg-emphasis)
         (if following-order (format ".  Next: %s" following-order) ""))))))

(defun icicle-current-sort-functions ()
  "Subset of `icicle-sort-orders-alist' that is currently appropriate.
For some common kinds of completion, remove simple sort functions (not
multi-sort comparers) that are not pertinent for the current kind of
completion."
  (icicle-remove-if (lambda (pred)
                      (setq pred  (cdr pred))
                      (and pred
                           (symbolp pred) ; Do not handle multi-sort comparers.
                           (not (eq pred icicle-allowed-sort-predicate))
                           (or (and (get pred 'icicle-proxy-sort-predicate)
                                    (not icicle-add-proxy-candidates-flag))
                               (and (get pred 'icicle-file-name-sort-predicate)
                                    (not (or (icicle-file-name-input-p)  icicle-abs-file-candidates)))
                               ;; Not really needed yet, because we only add such sorts dynamically.
                               (and (get pred 'icicle-buffer-name-sort-predicate) ; Better than nothing.
                                    (not (eq minibuffer-history-variable 'buffer-name-history)))
                               (and (get pred 'icicle-command-sort-predicate)
                                    (not (and (eq minibuffer-completion-table obarray)
                                              ;; But this will fail if predicate is more complex.
                                              ;; So be sure to bind `icicle-allowed-sort-predicate'
                                              ;; in that case, to avoid this restriction.
                                              (eq minibuffer-completion-predicate 'commandp))))
                               ;; Sort order for multi-completions. `minibuffer-completion-table'
                               ;; could be a function (e.g. `icicle-describe-opt-of-type-complete')
                               ;; or it could be a list of multi-completions.
                               (and (get pred 'icicle-multi-completion-sort-predicate)
                                    (not (icicle-maybe-multi-completion-completing-p))))))
                    icicle-sort-orders-alist))

(defun icicle-maybe-multi-completion-completing-p ()
  "Returns non-nil if we might currently be multi-completion completing.
Note: If `minibuffer-completion-table' is a function, multi-completion
is possible but not sure.  Return non-nil in that case."
  (or (functionp minibuffer-completion-table)  icicle-list-use-nth-parts))

;;;###autoload (autoload 'icicle-dispatch-M-comma "icicles")
(defun icicle-dispatch-M-comma ()       ; Bound to `M-,' in minibuffer.
  "Do the right thing for `M-,'.
If sorting is possible, call `icicle-change-alternative-sort-order'.
If using `icicle-search', call `icicle-search-define-replacement'.
Otherwise, do nothing.

Bound to `M-,' in the minibuffer."
  (interactive)
  (cond (icicle-searching-p (icicle-search-define-replacement))
        (icicle-inhibit-sort-p (message "Cannot sort candidates now"))
        (t (icicle-change-alternative-sort-order))))

;; Free vars here: `icicle-scan-fn-or-regexp' is bound in `icicle-search'.
;;
;;;###autoload (autoload 'icicle-search-define-replacement "icicles")
(defun icicle-search-define-replacement () ; Bound to `M-,' in minibuffer during `icicle-search'.
  "Prompt user and set new value of `icicle-search-replacement'.
Bound to `M-,' in the minibuffer."
  (interactive)
  (save-selected-window
    (icicle-remove-Completions-window)) ; Prevent incremental completion kicking in from the get-go.
  (setq icicle-search-replacement
        (let ((enable-recursive-minibuffers   t)
              (icicle-incremental-completion  t) ; Override current upgrade to `always'.
              (icicle-completion-candidates   icicle-completion-candidates)
              (icicle-current-input           icicle-current-input)
              (icicle-candidate-nb            icicle-candidate-nb)
              (icicle-update-input-hook       nil))
          (icicle-completing-read-history "Replacement string: " 'icicle-search-replacement-history)))
  ;; Just a sanity check.  Cannot really test equivalence of two regexps.
  (while (if icicle-search-replace-whole-candidate-flag
             (equal icicle-search-replacement icicle-scan-fn-or-regexp)
           (equal icicle-search-replacement icicle-current-input))
    (setq icicle-search-replacement
          (let ((enable-recursive-minibuffers   t)
                (icicle-incremental-completion  t) ; Override current upgrade to `always'.
                (icicle-completion-candidates   icicle-completion-candidates)
                (icicle-current-input           icicle-current-input)
                (icicle-candidate-nb            icicle-candidate-nb)
                (icicle-update-input-hook       nil))
            (icicle-completing-read-history "Replacement = replaced.  Replacement string: "
                                            'icicle-search-replacement-history)))))

;;;###autoload (autoload 'icicle-change-alternative-sort-order "icicles")
(defun icicle-change-alternative-sort-order (&optional arg) ; Bound to `M-,' in minibuffer (not search).
  "Choose an alternative sort order.
Similar to command `icicle-change-sort-order', but change the
alternative sort order, not the current sort order."
  (interactive "P")
  (if (and (interactive-p)  icicle-inhibit-sort-p)
      (icicle-msg-maybe-in-minibuffer "Cannot sort candidates now")
    (icicle-change-sort-order arg t)))

(defun icicle-current-sort-order (alternativep)
  "Current sort order, or nil if sorting is inactive.
If ALTERNATIVEP is non-nil, the alternative sort order is returned."
  (car (rassq (if alternativep icicle-alternative-sort-comparer icicle-sort-comparer)
              icicle-sort-orders-alist)))

;;;###autoload (autoload 'icicle-reverse-sort-order "icicles")
(defun icicle-reverse-sort-order ()
  "Reverse the current sort order."
  (interactive)
  (if (and (interactive-p)  icicle-inhibit-sort-p)
      (icicle-msg-maybe-in-minibuffer "Cannot sort candidates now")
    (setq icicle-reverse-sort-p  (not icicle-reverse-sort-p))
    (icicle-display-candidates-in-Completions icicle-reverse-sort-p)
    (icicle-complete-again-update)
    (icicle-msg-maybe-in-minibuffer
     "Sort order is %s" (icicle-propertize (concat (icicle-current-sort-order nil)
                                                   (and icicle-reverse-sort-p  ", REVERSED"))
                                           'face 'icicle-msg-emphasis))))

;;;###autoload (autoload 'icicle-plus-saved-sort "icicles")
(defun icicle-plus-saved-sort ()        ; Bound to `C-M-+' during completion.
  "Sort candidates by combining their current order with the saved order."
  (interactive)
  (let ((icicle-sort-comparer  'icicle-merge-saved-order-less-p)
        (cands                 (copy-sequence icicle-completion-candidates)))
    (setq icicle-completion-candidates
          (if (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
              (icicle-strip-ignored-files-and-sort cands)
            (icicle-maybe-sort-maybe-truncate cands))))
  (when (get-buffer-window "*Completions*" 0) (icicle-display-candidates-in-Completions))
  (when (interactive-p) (icicle-msg-maybe-in-minibuffer "Added in the saved sort order")))

 
;;(@* "Other commands to be used mainly in the minibuffer")

;;; Other commands to be used mainly in the minibuffer . . . . . . . .

;; $$ Probably need to do something to work around problem of Windows
;; selecting the new frame, when `pop-up-frames' is non-nil.  Need to
;; redirect focus back to the frame with the minibuffer.  Leave it as
;; is, for now, in hopes Emacs will eventually fix this.
;;
;;;###autoload (autoload 'icicle-minibuffer-help "icicles")
(defun icicle-minibuffer-help ()        ; Bound to `M-?' (and `C-?') in the minibuffer.
  "Describe Icicles minibuffer and *Completion* buffer bindings."
  (interactive)
  (let ((cur-buf  (current-buffer)))
    (with-output-to-temp-buffer "*Help*"
      (help-setup-xref (list #'icicle-minibuffer-help) (interactive-p))
      (when (icicle-completing-p)
        (princ (concat "You are completing input" (and icicle-candidate-action-fn
                                                       " for an Icicles multi-command")
                       ".\n\n"))
        (princ "To show help on individual completion candidates:
     Current candidate                       C-M-RET, C-M-mouse-2
     Next, previous candidate                C-M-down, C-M-up,
                                              C-M- plus mouse wheel
                    prefix-match candidate   C-M-end, C-M-home
                    apropos-match candidate  C-M-next, C-M-prior\n\n")
        (when icicle-candidate-action-fn
          (princ "To act on individual candidates:
     Current candidate                       C-RET, C-mouse-2
     Next, previous candidate                C-down, C-up,
                                              C- plus mouse wheel
                    prefix-match candidate   C-end, C-home
                    apropos-match candidate  C-next, C-prior
     All candidates at once                  C-! (each) or M-! (list)
     Delete object named by candidate        S-delete
     Object-action: apply a fn to candidate  M-RET"))
        (when icicle-candidate-alt-action-fn
          (princ "\n\nFor alt action, use `C-S-' instead of `C-', but use `C-|' or `M-|',\n\
     instead of `C-!' or `M-!', to act on all.\n")))
      (if icicle-completing-p
          (with-current-buffer standard-output
            (insert (concat "\n" (icicle-help-string-completion))))
        (princ (icicle-help-string-non-completion))))
    ;; Don't bother to do this for Emacs 21.3.  Its `help-insert-xref-button' signature is different.
    (when (and (> emacs-major-version 21)
               (require 'help-mode nil t)
               (fboundp 'help-insert-xref-button)) ; In `help-mode.el'.
      (save-excursion
        (with-current-buffer (get-buffer "*Help*")
          (let ((buffer-read-only  nil))
            (goto-char (point-min))
            (help-insert-xref-button "[Icicles Help on the Web]" 'icicle-help-button)
            (insert "                        ")
            (help-insert-xref-button "[Icicles Doc, Part 1]" 'icicle-commentary1-button)
            (insert "\n")
            (help-insert-xref-button "[Icicles Options & Faces]" 'icicle-customize-button)
            (insert "                        ")
            (help-insert-xref-button "[Icicles Doc, Part 2]" 'icicle-commentary2-button)
            (insert "\n\n")
            (goto-char (point-max))
            (insert (make-string 70 ?_))
            (insert (funcall
                     (if (fboundp 'help-commands-to-key-buttons) ; In `help-fns.el'.
                         #'help-commands-to-key-buttons
                       #'substitute-command-keys)
                     "\n\nSend an Icicles bug report: `\\[icicle-send-bug-report]'.\n\n"))
            (help-insert-xref-button "[Icicles Help on the Web]" 'icicle-help-button)
            (insert "                        ")
            (help-insert-xref-button "[Icicles Doc, Part 1]" 'icicle-commentary1-button)
            (insert "\n")
            (help-insert-xref-button "[Icicles Options & Faces]" 'icicle-customize-button)
            (insert "                        ")
            (help-insert-xref-button "[Icicles Doc, Part 2]" 'icicle-commentary2-button)
            (insert "\n\n")
            (goto-char (point-min))))))
    (when (memq cur-buf (list (window-buffer (minibuffer-window)) (get-buffer "*Completions*")))
      (select-window (minibuffer-window))
      (select-frame-set-input-focus (selected-frame)))))

(defun icicle-help-string-completion ()
  "Update the bindings within the Icicles completion help string."
  (icicle-S-iso-lefttab-to-S-TAB
   (funcall
    (if (fboundp 'help-commands-to-key-buttons) ; In `help-fns+.el'.
        #'help-commands-to-key-buttons
      #'substitute-command-keys)
    (concat
     (format "\\<minibuffer-local-completion-map> 

                    Icicles Minibuffer Completion
                    -----------------------------

Minibuffer input can be completed in several ways.
These are the main Icicles actions and their minibuffer key bindings:

 * Show Icicles minibuffer help (this).      \\[icicle-minibuffer-help]
     For help on individual completion candidates, see \"Show help on
     individual completion candidates\", below.

 * Abandon or commit your input.
     Abandon input                           \\[icicle-abort-recursive-edit]
     Commit input to Emacs                   RET
       Complete partial input, then commit   \\<minibuffer-local-must-match-map>\
\\[icicle-apropos-complete-and-exit]\\<minibuffer-local-completion-map>

 * Toggle/cycle Icicles options on the fly.  Key:   \tCurrently:
     Highlighting of past inputs             \\[icicle-toggle-highlight-historical-candidates]\t%S
     Highlighting of saved candidates        \\[icicle-toggle-highlight-saved-candidates]\t%S
     Removal of duplicate candidates         \\[icicle-toggle-transforming]\t%s
     Sort order                              \\[icicle-change-sort-order]\t%s
     Alternative sort order                  \\[icicle-dispatch-M-comma]\t%s
     Swap alternative/normal sort            \\[icicle-toggle-alternative-sorting]
     Case sensitivity                        \\[icicle-toggle-case-sensitivity]\t%S
     `.' matching newlines too (any char)    \\[icicle-toggle-dot]\t%S
     Escaping of special regexp chars        \\[icicle-toggle-regexp-quote]\t%S
     Incremental completion                  \\[icicle-cycle-incremental-completion]\t%s
     Input expansion to common match (toggle)\\[icicle-toggle-expand-to-common-match]\t%S
     Input expansion to common match (cycle) \\[icicle-cycle-expand-to-common-match]\t%s
     Hiding common match in `*Completions*'  \\[icicle-dispatch-C-x.]\t%S
     Hiding no-match lines in `*Completions*' C-u \\[icicle-dispatch-C-x.]\t%s
     Horizontal/vertical candidate layout    \\[icicle-toggle-completions-format]\t%s
     S-TAB completion method                 \\[icicle-next-S-TAB-completion-method]\t%s
     TAB completion method                   \\[icicle-next-TAB-completion-method]\t%s
     Showing image-file thumbnails (E22+)    C-x t\t%s
     Showing candidate annotations           \\[icicle-toggle-annotation]\t%S
     Inclusion of proxy candidates           \\[icicle-toggle-proxy-candidates]\t%S
     Ignoring certain file extensions        \\[icicle-dispatch-C-.]\t%S
     Checking for remote file names          \\[icicle-dispatch-C-^]\t%S"
             (if icicle-highlight-historical-candidates-flag 'yes 'no)
             (if icicle-highlight-saved-candidates-flag 'yes 'no)
             (cond ((not icicle-transform-function) "no")
                   ((or (eq icicle-transform-function 'icicle-remove-duplicates)
                        (and icicle-extra-candidates
                             (eq icicle-transform-function 'icicle-remove-dups-if-extras)))
                    "yes")
                   ((eq 'icicle-remove-dups-if-extras icicle-transform-function)
                    "yes in general, but not now")
                   (t icicle-transform-function))
             (icicle-current-sort-order nil)
             (icicle-current-sort-order 'ALTERNATIVE)
             (if case-fold-search 'no 'yes)
             (if (string= icicle-dot-string icicle-anychar-regexp) 'yes 'no)
             (if icicle-regexp-quote-flag 'yes 'no)
             (case icicle-incremental-completion
               ((nil) "no")
               ((t)   "yes, if *Completions* showing")
               (t     "yes, always (eager)"))
             (if (eq icicle-expand-input-to-common-match 0) 'no 'yes)
             (case icicle-expand-input-to-common-match
               (0 "never")
               (1 "explicit completion")
               (2 "only one completion")
               (3 "`TAB' or only one")
               (t "always"))
             (if icicle-hide-common-match-in-Completions-flag 'yes 'no)
             (if icicle-hide-non-matching-lines-flag 'yes 'no)
             icicle-completions-format
             (car (rassq icicle-apropos-complete-match-fn icicle-S-TAB-completion-methods-alist))
             (icicle-current-TAB-method)
             (case icicle-image-files-in-Completions
               ((nil) "no")
               (image "image only")
               (t "image and name"))
             (if icicle-show-annotations-flag 'yes 'no)
             (if icicle-add-proxy-candidates-flag 'yes 'no)
             (if completion-ignored-extensions 'yes 'no)
             (if icicle-test-for-remote-files-flag 'yes 'no))

     (and (memq system-type '(ms-dos windows-nt cygwin)) ; MS Windows only.
          (format "\\<minibuffer-local-completion-map>
     Considering network drives as remote    \\[icicle-toggle-network-drives-as-remote]\t%S"
                  (if icicle-network-drive-means-remote-flag 'yes 'no)))

     (format "\\<minibuffer-local-completion-map>
     Ignoring space prefix for buffer names  \\[icicle-dispatch-M-_]\t%S
     Using `C-' for multi-command actions    \\[icicle-toggle-C-for-actions]\t%S
     Using `~' for your home directory       \\[icicle-toggle-~-for-home-dir]\t%S
     `icicle-search' all-current highlights  \\[icicle-dispatch-C-^]\t%S
     Whole-word searching                    \\[icicle-dispatch-M-q]\t%S
     Removal of `icicle-search' highlighting \\[icicle-dispatch-C-.]\t%S
     Replacement of whole search hit         \\[icicle-dispatch-M-_]\t%S
     Replacement of expanded common match    \\[icicle-toggle-search-replace-common-match]\t%S

 * Regexp-quote input, then apropos-complete \\[icicle-regexp-quote-input]

 * Change the set of completion candidates.  Modify your input.
     Edit your input                         (just edit in minibuffer)
     Erase your input (clear minibuffer)     \\[icicle-erase-minibuffer-or-history-element]
     Goto/kill non-matching portion of input \\[icicle-goto/kill-failed-input]
     Retrieve previous completion inputs     \\[icicle-retrieve-previous-input], \
\\[icicle-retrieve-next-input]
     Match another regexp (chaining)         \\[icicle-narrow-candidates]
     Satisfy another predicate (chaining)    \\[icicle-narrow-candidates-with-predicate]
     Remove a candidate from set of matches  delete, S-mouse-2
     Yank text at cursor into minibuffer     \\[icicle-insert-string-at-point]
     Insert text (string) from a variable    \\[icicle-insert-string-from-variable]
     Insert `icicle-list-join-string'        \\[icicle-insert-list-join-string]
     Insert previously entered input         \\[icicle-insert-history-element]
     Insert key description (key completion) \\[icicle-dispatch-M-q]

 * Complete your current input in the minibuffer.
     Apropos (regexp) completion             \\[icicle-apropos-complete]
       Without displaying candidates         \\[icicle-apropos-complete-no-display]
       Complete and match another regexp     \\[icicle-apropos-complete-and-narrow]
     Prefix completion
       As much as possible                   \\[icicle-prefix-complete]
         Without displaying candidates       \\[icicle-prefix-complete-no-display]
       A word at a time                      \\[icicle-prefix-word-complete]
     Complete and commit \\<minibuffer-local-must-match-map>\
\\[icicle-apropos-complete-and-exit]\\<minibuffer-local-completion-map>
     Complete search string using past input \\[icicle-apropos-complete]

 * Display/navigate completions for current input (in `*Completions*').
     Show completion candidates
       Prefix completion                     \\[icicle-prefix-complete] (repeat)
       Apropos completion                    \\[icicle-apropos-complete]
     Move between minibuffer and list        \\<completion-list-mode-map>\
\\[icicle-insert-completion]
     Cycle among completion candidates       right, left, \
\\[icicle-move-to-next-completion], \\[icicle-move-to-previous-completion]
       Within a `*Completions*' column       down, up
     Choose a completion candidate           \\[choose-completion], \
\\[mouse-choose-completion]\\<minibuffer-local-completion-map>

 * Cycle among input candidates.
     Completion candidates
       Current mode                          down, up, mouse wheel
       Prefix completion                     end, home
       Apropos completion                    next, prior
     Minibuffer history items                \\[next-history-element], \
\\[previous-history-element]
     Completion history items                \\[icicle-retrieve-previous-input], \
\\[icicle-retrieve-next-input]

 * Show help on individual completion candidates.
     Current candidate                       C-M-RET, C-M-mouse-2
     Next, previous candidate                C-M-down, C-M-up,
                                              C-M- plus mouse wheel
                    prefix-match candidate   C-M-end, C-M-home
                    apropos-match candidate  C-M-next, C-M-prior

 * Choose a previous input from the minibuffer history.
     Complete to insert a previous input     \\[icicle-insert-history-element]
     Complete against history items          \\[icicle-history], \
\\[icicle-keep-only-past-inputs]
     Restrict candidates to history items    \\[icicle-keep-only-past-inputs]
     Change to another history               \\[icicle-other-history]
     List history items first in Completions \\[icicle-toggle-alternative-sorting]
     Cycle among minibuffer history items    \\[next-history-element], \
\\[previous-history-element]
     Search among minibuffer history items   \
\\[next-matching-history-element], \\[previous-matching-history-element]

 * Delete history entries
     Delete current entry (cycling)          \\[icicle-erase-minibuffer-or-history-element]
     Delete any or all entries               \\[icicle-clear-current-history]

 * Multi-commands: Act on completion candidates.
   For alternative action, use `C-S-' instead of `C-', but
   `C-|' and `M-|' are alternative action versions of `C-!' and `M-!'.
     Current candidate                       C-RET, C-mouse-2
     Next, previous candidate                C-down, C-up,
                                             C- with mouse wheel
                    prefix-match candidate   C-end, C-home
                    apropos-match candidate  C-next, C-prior
     Act on each matching candidate, in turn C-!
     Act on the list of matching candidates  M-!
     Delete object named by candidate        S-delete
     Remove candidate from set of matches    delete, S-mouse-2
     Save candidate (add to those saved)     insert, M-S-mouse-2
     Object-action: apply a fn to candidate  M-RET

 * Search and replace (e.g. `C-c `').  See also `icicle-search'.
     Use action keys (prefix `C-') to navigate.
     Use alternative action keys (prefix `C-S-') to replace matches.
     Toggle input highlighting at all hits   \\[icicle-dispatch-C-^]
     Toggle whole-word searching             \\[icicle-dispatch-M-q]
     Toggle `.' matching newlines too        \\[icicle-toggle-dot]
     Toggle escaping of special regexp chars \\[icicle-toggle-regexp-quote]
     Toggle removal of search highlighting   \\[icicle-dispatch-C-.]

     Replace all                             M-|
     Redefine the replacement string         \\[icicle-dispatch-M-comma]
     Toggle literal replacement              \\[icicle-toggle-literal-replacement]
     Toggle replacement of whole search hit  \\[icicle-dispatch-M-_]
     Toggle replacement of common match      \\[icicle-toggle-search-replace-common-match]

 * Perform set operations on candidate sets.
     Remove candidate from current set       delete, S-mouse-2
     Add current candidate to saved set      insert, M-S-mouse-2
     Retrieve saved candidates from...
       `icicle-saved-completion-candidates'  \\[icicle-candidate-set-retrieve]
       another variable                      \\[icicle-candidate-set-retrieve-from-variable]
       a cache file                          \\[icicle-candidate-set-retrieve-persistent]
     Retrieve more saved candidates          \\[icicle-candidate-set-retrieve-more]
     Save candidates in current set to...
       `icicle-saved-completion-candidates'  \\[icicle-candidate-set-save]
       another variable                      \\[icicle-candidate-set-save-to-variable]
       a cache file                          \\[icicle-candidate-set-save-persistently]
     Save more candidates to current set     \\[icicle-candidate-set-save-more]
     Save, save more selected candidates     \\[icicle-candidate-set-save-selected], \
\\[icicle-candidate-set-save-more-selected]  with region
     Clear all saved candidates              \\[icicle-candidate-set-save-selected] \
with empty region
     Add new or update existing saved set
       \\[icicle-add/update-saved-completion-set]
     Remove a saved completion set
       \\[icicle-remove-saved-completion-set]
     Swap current and saved sets             \\[icicle-candidate-set-swap]
     Define current set by evaluating sexp   \\[icicle-candidate-set-define]
     Restrict candidates to history items    \\[icicle-keep-only-past-inputs]
     Set complement                          \\[icicle-candidate-set-complement]
     Set difference                          \\[icicle-candidate-set-difference]
     Set union                               \\[icicle-candidate-set-union]
     Set intersection                        \\[icicle-candidate-set-intersection]
     Set intersection using regexp           \\[icicle-narrow-candidates]
     Set intersection using predicate        \\[icicle-narrow-candidates-with-predicate]
       Save current predicate to a variable  \\[icicle-save-predicate-to-variable]
       Insert string variable as input       \\[icicle-insert-string-from-variable]

 * Adjust Icicles options incrementally on the fly (uses Do Re Mi).
     `icicle-candidate-width-factor'        \\[icicle-doremi-candidate-width-factor+]
     `icicle-max-candidates'                \\[icicle-doremi-increment-max-candidates+]
     `icicle-swank-timeout'                 C-x 1
     `icicle-swank-prefix-length'           C-x 2
     `icicle-inter-candidates-min-spaces'   \\[icicle-doremi-inter-candidates-min-spaces+]
     Zoom `*Completions*' (not an option)   C-x -   (Emacs 23+)

Remember: You can always input any character (e.g. \\[icicle-prefix-complete]) that is bound
          to a command by preceding it with \\<global-map>\\[quoted-insert].

Though it has no direct connection with completion, you can use \
`\\<minibuffer-local-completion-map>\\[icicle-pp-eval-expression-in-minibuffer]'
in the minibuffer at any time to evaluate an Emacs-Lisp expression.
This calls `icicle-pp-eval-expression-in-minibuffer', which displays
the result in the echo area or a popup buffer, *Pp Eval Output*.
It also provides some of the Emacs-Lisp key bindings during expression
editing."
             (if icicle-buffer-ignore-space-prefix-flag 'yes 'no)
             (if icicle-use-C-for-actions-flag 'yes 'no)
             (if icicle-use-~-for-home-dir-flag 'yes 'no)
             (if icicle-search-highlight-all-current-flag 'yes 'no)
             (if icicle-search-whole-word-flag 'yes 'no)
             (if icicle-search-cleanup-flag 'yes 'no)                
             (if icicle-search-replace-whole-candidate-flag 'yes 'no)
             (if icicle-search-replace-common-match-flag 'yes 'no))
     icicle-general-help-string
     " 

These are all of the minibuffer bindings during completion:

\\{minibuffer-local-completion-map}"))))

(defun icicle-help-string-non-completion ()
  "Description of Icicles minibuffer bindings when not completing input."
  (icicle-S-iso-lefttab-to-S-TAB
   (substitute-command-keys
    (concat "\\<minibuffer-local-completion-map>\
              Icicles Minibuffer Input when Not Completing
              --------------------------------------------

These are the main Icicles minibuffer key bindings when completion is
not available:

 * Show this help.                           \\[icicle-minibuffer-help]

 * Abandon your input.                       \\[icicle-abort-recursive-edit]

 * Commit your input to Emacs.               RET

 * Modify your input.
     Edit your input                         (just edit in minibuffer)
     Erase your input (clear minibuffer)     \\[icicle-erase-minibuffer-or-history-element]
     Yank text at cursor into minibuffer     \\[icicle-insert-string-at-point]
     Insert text (string) from a variable    \\[icicle-insert-string-from-variable]
     Insert previously entered input         \\[icicle-insert-history-element]

 * Choose a previous input from the minibuffer history.
     Complete to insert a previous input     \\[icicle-insert-history-element]
     Cycle among minibuffer history items    \\[next-history-element], \
\\[previous-history-element]
     Search among minibuffer history items   \
\\[next-matching-history-element], \\[previous-matching-history-element]

 * Delete history entries
     Delete current entry (cycling)          \\[icicle-erase-minibuffer-or-history-element]
     Delete any or all entries               \\[icicle-clear-current-history]

 * Evaluate an Emacs-Lisp sexp on the fly    \\[icicle-pp-eval-expression-in-minibuffer]

Remember: You can always input any character that is bound to a
          command by preceding it with \\<global-map>\\[quoted-insert]."
            icicle-general-help-string

            " 
These are the minibuffer bindings when not completing input:

\\{minibuffer-local-map}"))))

(when (and (> emacs-major-version 21)
           (require 'help-mode nil t)
           (get 'help-xref 'button-category-symbol)) ; In `button.el'
  (define-button-type 'icicle-help-button
      :supertype 'help-xref
      'help-function (lambda () (browse-url "http://www.emacswiki.org/cgi-bin/wiki/Icicles"))
      'help-echo
      (purecopy "mouse-2, RET: Icicles documentation on the Emacs Wiki (requires Internet access)"))
  (define-button-type 'icicle-commentary1-button
      :supertype 'help-xref
      'help-function (lambda ()
                       (finder-commentary "icicles-doc1")
                       (when (require 'linkd nil t) (linkd-mode 1))
                       (when (require 'fit-frame nil t) (fit-frame)))
      'help-echo (purecopy "mouse-2, RET: Icicles documentation, Part 1 (no Internet needed)"))
  (define-button-type 'icicle-commentary2-button
      :supertype 'help-xref
      'help-function (lambda ()
                       (finder-commentary "icicles-doc2")
                       (when (require 'linkd nil t) (linkd-mode 1))
                       (when (require 'fit-frame nil t) (fit-frame)))
      'help-echo (purecopy "mouse-2, RET: Icicles documentation, Part 2 (no Internet needed)"))
  (define-button-type 'icicle-customize-button
      :supertype 'help-xref
      'help-function (lambda () (customize-group-other-window 'Icicles))
      'help-echo (purecopy "mouse-2, RET: Customize/Browse Icicles Options & Faces")))


;; This is just the macro expansion of the following:
;; `(def-completion-wrapper icicle-abort-recursive-edit :minibuffer-separator)'.
;; Taken from the definition of `def-completion-wrapper' in `completion.el'.
(put 'icicle-abort-recursive-edit 'completion-function 'use-completion-minibuffer-separator)

;;;###autoload (autoload 'icicle-abort-recursive-edit "icicles")
(defun icicle-abort-recursive-edit ()   ; Bound to `C-]',`C-g' in minibuf, `C-g',`q' in `*Completions*'.
  "Abort command that requested this recursive edit or minibuffer input.
This calls `abort-recursive-edit' after killing the `*Completions*'
buffer or (if called from the minibuffer) removing its window.

By default, Icicle mode remaps all key sequences that are normally
bound to `abort-recursive-edit' to `icicle-abort-recursive-edit'.  If
you do not want this remapping, then customize option
`icicle-top-level-key-bindings'."
  (interactive)
  (if (not (active-minibuffer-window))
      (when (get-buffer "*Completions*") (kill-buffer (get-buffer "*Completions*")))
    (when (and (boundp '1on1-fit-minibuffer-frame-flag) ; In `oneonone.el'.
               1on1-fit-minibuffer-frame-flag  (require 'fit-frame nil t))
      (1on1-fit-minibuffer-frame 'RESET))
    (icicle-remove-Completions-window 'FORCE))
  (abort-recursive-edit))

(unless (fboundp 'save&set-overriding-map) ; Only Emacs 20-23 use `ensure-overriding-map-is-bound'.
  (defun icicle-ensure-overriding-map-is-bound ()
    "Set `overriding-terminal-local-map' to `icicle-universal-argument-map'."
    (if (not (boundp 'overriding-map-is-bound)) ; Emacs 20, 21.
        (setq overriding-terminal-local-map  icicle-universal-argument-map)
      (unless overriding-map-is-bound   ; Emacs 22+.
        (setq saved-overriding-map           overriding-terminal-local-map
              overriding-terminal-local-map  icicle-universal-argument-map
              overriding-map-is-bound        t)))))

;;;###autoload (autoload 'icicle-digit-argument "icicles")
(defun icicle-digit-argument (arg)      ; Bound to `C-<0-9>', `M-<0-9>', `C-M-<0-9>' in minibuffer.
  "`digit-argument', but also echo the prefix."
  (interactive "P")
  (let* ((char   (if (integerp last-command-event)
                     last-command-event
                   (icicle-get-safe last-command-event 'ascii-character)))
         (digit  (- (logand char ?\177) ?0)))
    (cond ((integerp arg)
           (setq prefix-arg  (+ (* arg 10) (if (< arg 0) (- digit) digit))))
          ((eq arg '-)
           ;; Treat -0 as just -, so that -01 will work.
           (setq prefix-arg  (if (zerop digit) '- (- digit))))
          (t
           (setq prefix-arg  digit))))
  (setq universal-argument-num-events  (length (this-command-keys)))
  (if (fboundp 'save&set-overriding-map) ; Emacs 24+
      (save&set-overriding-map icicle-universal-argument-map)
    (icicle-ensure-overriding-map-is-bound))
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload (autoload 'icicle-negative-argument "icicles")
(defun icicle-negative-argument (arg)   ; Bound to `M--', `C-M--' in minibuffer.
  "`negative-argument', but also echo the prefix."
  (interactive "P")
  (cond ((integerp arg) (setq prefix-arg  (- arg)))
        ((eq arg '-) (setq prefix-arg  nil))
        (t (setq prefix-arg  '-)))
  (setq universal-argument-num-events  (length (this-command-keys)))
  (if (fboundp 'save&set-overriding-map) ; Emacs 24+
      (save&set-overriding-map icicle-universal-argument-map)
    (icicle-ensure-overriding-map-is-bound))
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload (autoload 'icicle-universal-argument "icicles")
(defun icicle-universal-argument ()     ; Bound to `C-u' in minibuffer.
  "`universal-argument', but also echo the prefix."
  (interactive)
  (setq prefix-arg                     (list 4)
        universal-argument-num-events  (length (this-command-keys)))
  (if (fboundp 'save&set-overriding-map) ; Emacs 24+
      (save&set-overriding-map icicle-universal-argument-map)
    (icicle-ensure-overriding-map-is-bound))
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload (autoload 'icicle-universal-argument-more "icicles")
(defun icicle-universal-argument-more (arg)
  "`universal-argument-more', but also echo the prefix."
  (interactive "P")
  (universal-argument-more arg)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload (autoload 'icicle-universal-argument-other-key "icicles")
(defun icicle-universal-argument-other-key (arg)
  "`universal-argument-other-key', but also echo the prefix."
  (interactive "P")
  (universal-argument-other-key arg)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload (autoload 'icicle-universal-argument-minus "icicles")
(defun icicle-universal-argument-minus (arg)
  "`universal-argument-minus', but also echo the prefix."
  (interactive "P")
  (universal-argument-minus arg)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))


;; REPLACE ORIGINAL `sit-for' in `subr.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; 1. Ensure that `sit-for' after `C-u' in the minibuffer is immediately interrupted by user input.
;;    This fix is not needed for Emacs < 23.
;;
;; 2. Bind `inhibit-quit' to t, so `C-g' is handled after `sit-for', by `icicle-abort-recursive-edit'.
;;
(unless (fboundp 'icicle-ORIG-sit-for)
  (defalias 'icicle-ORIG-sit-for (symbol-function 'sit-for)))

(when (> emacs-major-version 22)
  (defun icicle-sit-for (seconds &optional nodisp obsolete)
    "Perform redisplay, then wait for SECONDS seconds or until input is available.
SECONDS may be a floating-point value.
\(On operating systems that do not support waiting for fractions of a
second, floating-point values are rounded down to the nearest integer.)

If optional arg NODISP is t, don't redisplay, just wait for input.
Redisplay does not happen if input is available before it starts.

Value is t if waited the full time with no input arriving, and nil otherwise.

An obsolete, but still supported form is
\(sit-for SECONDS &optional MILLISECONDS NODISP)
where the optional arg MILLISECONDS specifies an additional wait period,
in milliseconds; this was useful when Emacs was built without
floating point support."
    (if (numberp nodisp)
        (setq seconds  (+ seconds (* 1e-3 nodisp))
              nodisp   obsolete)
      (if obsolete (setq nodisp  obsolete)))
    (cond (noninteractive
           (sleep-for seconds)
           t)
          ((input-pending-p)
           nil)
          ((<= seconds 0)
           (or nodisp  (redisplay)))
          (t
           (or nodisp  (redisplay))
           (let* ((inhibit-quit  t)
                  (read          (read-event nil nil seconds)))
             (or (null read)
                 (progn
                   ;; If last command was a prefix arg, e.g. C-u, push this event onto
                   ;; `unread-command-events' as (t . EVENT) so it will be added to
                   ;; `this-command-keys' by `read-key-sequence'.
                   (when (memq overriding-terminal-local-map
                               (list universal-argument-map icicle-universal-argument-map))
                     (setq read  (cons t read)))
                   (push read unread-command-events)
                   nil)))))))

;;;###autoload (autoload 'icicle-retrieve-next-input "icicles")
(defun icicle-retrieve-next-input (&optional arg) ; Bound to `C-S-l' (`C-L') in minibuffer.
  "Retrieve next minibuffer input.
Like `icicle-retrieve-previous-input', but traverses history toward
the present.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-retrieve-next-input]')."
  (interactive "P")
  (icicle-retrieve-previous-input arg 'interactive-p)) ; Must be `interactive-p'.

;;;###autoload (autoload 'icicle-retrieve-previous-input "icicles")
(defun icicle-retrieve-previous-input (&optional arg reversep allow-empty-p) ; `C-l' in minibuffer.
  "Retrieve previous minibuffer input.
The possible inputs were not necessarily those entered with `RET'.
With a negative prefix arg, this just empties the completion history.
Otherwise:
 Use completion if `icicle-C-l-uses-completion-flag' is non-nil and no
   prefix arg is used, or if it is nil and a prefix arg is used, or if
   `icicle-retrieve-previous-input' is not used interactively.
 Otherwise, just cycle to the previous input.

Non-interactively:
 Non-nil argument REVERSEP means reverse the history order: return the
  next, not the previous, input.
 Non-nil ALLOW-EMPTY-P means the retrieved input can be \"\".

You can use this command only from buffer *Completions or from the
minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-retrieve-previous-input]')."
  (interactive "P")
  (let ((interactive-p       (or (interactive-p)  (eq reversep 'interactive-p)))
        (prev-inputs-var     (if (icicle-file-name-input-p)
                                 'icicle-previous-raw-file-name-inputs
                               'icicle-previous-raw-non-file-name-inputs))
        ;; `irpi-was-cycling-p' is used to remember, for the second `C-l' in a row, that the first
        ;; `C-l' came after cycling.  In that case, the second `C-l' restores the current raw input.
        (irpi-was-cycling-p  icicle-cycling-p))
    (when interactive-p (icicle-barf-if-outside-Completions-and-minibuffer))
    (cond ((wholenump (prefix-numeric-value arg))
           (let ((input  ""))
             (save-selected-window
               (select-window (minibuffer-window))
               (icicle-clear-minibuffer)
               (let ((prev-inputs
                      (if allow-empty-p
                          (symbol-value prev-inputs-var)
                        (icicle-remove-if (lambda (x) (string= "" x)) ; Exclude "".
                                          (symbol-value prev-inputs-var)))))
                 (setq input
                       (if (and interactive-p  (or (and icicle-C-l-uses-completion-flag        (not arg))
                                                   (and (not icicle-C-l-uses-completion-flag)  arg)))
                           (let ((icicle-whole-candidate-as-text-prop-p   nil)
                                 (enable-recursive-minibuffers            t)
                                 (icicle-show-Completions-initially-flag  t))
                             (prog1 (completing-read
                                     "Retrieve input: " (mapcar #'list prev-inputs) nil t)
                               (setq icicle-last-input  nil)))
                         (if (or (not interactive-p)
                                 (not (memq last-command '(icicle-retrieve-next-input
                                                           icicle-retrieve-previous-input))))
                             ;; We use this one, to exclude common-match expansions from completion
                             ;; history, and to save the typed input only when you complete.
                             (let ((try  (if icicle-cycling-p
                                             icicle-last-input
                                           icicle-current-raw-input)))
                               (if (or allow-empty-p  (not (equal "" try))) try (car prev-inputs)))

                           ;; You can use this one instead, if you want to include common-match
                           ;; expansions and save the typed input even when you don't complete.
                           ;; (or icicle-last-input  icicle-current-raw-input)
                           
                           (let ((next  (member icicle-current-raw-input prev-inputs)))
                             (unless next (setq next  prev-inputs))
                             (if reversep
                                 (or (let ((res     ())
                                           (inputs  prev-inputs))
                                       (while (and (consp inputs)  (not (eq inputs next)))
                                         (push (pop inputs) res))
                                       (car res))
                                     (car (last prev-inputs)))
                               ;; If we were cycling before the first `C-l', then need to pick up the
                               ;; current raw input.  Otherwise, we need to pick up the previous one.
                               (prog1 (if irpi-was-cycling-p (car next) (cadr next))
                                 (setq irpi-was-cycling-p  nil))))))) ; So third `C-l' acts normally.

                 ;; $$$$ (when input (icicle-call-then-update-Completions #'insert input))))
                 ;; $$$$$$ (when input (insert input))))

;;; $$$$$$ REPLACED by previous line only.
                 (when input
                   (setq icicle-current-raw-input  input)
                   (insert input)
                   (icicle-highlight-initial-whitespace input) ; (e.g. user typo).
                   (icicle-place-cursor input 'deactivate-mark))))

;;;              (let ((icicle-edit-update-p  t))
;;;                (funcall (or icicle-last-completion-command  'icicle-apropos-complete))
;;;                ;; Restore raw input.  Cycling resets it to "", so `icicle-save-or-restore-input'
;;;                ;; doesn't use out-of-date raw input (cycling does not necessarily follow completion
;;;                ;; or completion of the same kind).
;;;                (setq icicle-current-raw-input  input))

             (setq icicle-current-raw-input  input ; So we can keep cycling.
                   icicle-last-input         nil ; So `TAB' expands it - `icicle-save-or-restore-input'.
                   icicle-cycling-p          irpi-was-cycling-p))) ; Let next `C-l' know the state.
          (t
           (set prev-inputs-var nil)
           (setq icicle-current-raw-input  "")
           (icicle-msg-maybe-in-minibuffer "Cleared completion history")))))

;; $$ No longer bound.  Now we bind `icicle-retrieve-previous-input', instead, to `C-l'.
;;
;; ;;;###autoload (autoload 'icicle-retrieve-last-input "icicles")
(defun icicle-retrieve-last-input ()
  "Put the last real input into the minibuffer.
Use this to replace a completion candidate inserted during cycling or
because of input expansion due to the value of option
`icicle-expand-input-to-common-match'.

If you are cycling and expansion is also in effect, then use this
twice in succession: once to restore the expanded common match string,
and a second time to restore your unexpanded original input.

You can use this command only from buffer `*Completions' or from the
minibuffer."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (save-selected-window
    (select-window (minibuffer-window))
    (icicle-clear-minibuffer)
    (if (and (eq last-command 'icicle-retrieve-last-input)
             (or (and (eq icicle-current-completion-mode 'apropos)
                      (eq icicle-expand-input-to-common-match 4))
                 (and (eq icicle-current-completion-mode 'prefix)
                      (memq icicle-expand-input-to-common-match '(3 4)))))
        (insert icicle-current-raw-input)
      (insert icicle-current-input))
    ;;$$$ (when (interactive-p) (setq icicle-last-completion-command  nil))
    (let ((input  (if (and (eq last-command this-command)
                           (or (and (eq icicle-current-completion-mode 'apropos)
                                    (eq icicle-expand-input-to-common-match 4))
                               (and (eq icicle-current-completion-mode 'prefix)
                                    (memq icicle-expand-input-to-common-match '(3 4)))))
                      icicle-current-raw-input
                    icicle-current-input)))
      (icicle-highlight-initial-whitespace input) ; Highlight initial whitespace (e.g. user typo).
      (icicle-place-cursor input 'deactivate-mark))))

;; $$ No longer used.  It was originally used in `icicle-retrieve-last-input'.
(defun icicle-insert-input (input)
  "Insert INPUT.  Prepend the directory if appropriate."
  (insert (if (and (icicle-file-name-input-p)
                   insert-default-directory
                   (or (not (member input icicle-extra-candidates))
                       icicle-extra-candidates-dir-insert-p))
              (icicle-expand-file-or-dir-name input (icicle-file-name-directory input))
            input)))

;;;###autoload (autoload 'icicle-insert-history-element "icicles")
(defun icicle-insert-history-element () ; Bound to `M-o' in minibuffer.
  "Use completion to insert a previously entered input in the minibuffer.
Always available for any minibuffer input, not just during completion."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (when (and (boundp minibuffer-history-variable)  (consp (symbol-value minibuffer-history-variable)))
    (let ((enable-recursive-minibuffers  t))
      (insert (icicle-completing-read-history "Choose input: " minibuffer-history-variable))))
  (when (and icicle-mode  (memq icicle-default-value '(preselect-start preselect-end)))
    (icicle-select-minibuffer-contents)
    (setq deactivate-mark  nil)))

;;;###autoload (autoload 'icicle-insert-string-at-point "icicles")
(defun icicle-insert-string-at-point (&optional arg) ; Bound to `M-.' in minibuffer.
  "Insert text at the cursor into the minibuffer.
Each time this command is called, some text at or near the cursor is
inserted into the minibuffer.  One of two things happens, depending on
the value of option `icicle-default-thing-insertion' and whether or
not you use `C-u'.

See the doc for option `icicle-thing-at-point-functions' for a
complete description of its behavior.  What follows is an overview.

`icicle-thing-at-point-functions' is a cons of two parts - call them
ALTERNATIVES and FORWARD-THING.

If ALTERNATIVES is not nil and one of the following is true:
 - FORWARD-THING is nil
 - the value of `icicle-default-thing-insertion' is `alternatives' and
   you have not used plain `C-u' in this series of `M-.'
 - the value of `icicle-default-thing-insertion' is `more-of-the-same'
   and you have used plain `C-u' in this series of `M-.'
then the next function in ALTERNATIVES is used to retrieve the text to
be inserted.

If FORWARD-THING is not nil and one of the following is true:
 - ALTERNATIVES is nil
 - the value of `icicle-default-thing-insertion' is `more-of-the-same'
   and you have not used `C-u' in this series of `M-.'
 - the value of `icicle-default-thing-insertion' is `alternatives' and
   you have used `C-u' in this series of `M-.'
then function FORWARD-THING is used to retrieve the text to be
inserted.

If you use a numeric prefix arg (not just plain `C-u'), the behavior
is as follows.

* If a function in ALTERNATIVES is used (see above), then the text
  that is grabbed at or near point is read as a Lisp sexp and
  evaluated, and the value is inserted instead of the grabbed text.

  Yes, this means you need to know when the particular ALTERNATIVES
  function that you want is coming up next, and use, say, `C-9' just
  before hitting `M-.' for that alternative.  So if, e.g., you want to
  evaluate the active region and insert the value, then you use
  `M-. C-9 M-.', since it is the second `M-.' that grabs the region.

* If the FORWARD-THING is being used, then the prefix arg determines
  the number of things to grab, and the direction of grabbing.: A
  negative argument grabs text to the left of the cursor; a positive
  argument grabs text to the right.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-at-point]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (when (consp icicle-thing-at-point-functions) ; Option should always be a cons cell.
    (unless (eq last-command this-command) (setq icicle-default-thing-insertion-flipped-p  nil))
    (let ((alt-fns       (car icicle-thing-at-point-functions))
          (fwd-thing-fn  (cdr icicle-thing-at-point-functions))
          (flipped       (or icicle-default-thing-insertion-flipped-p ; Already flipped.
                             (setq icicle-default-thing-insertion-flipped-p  (consp arg)))))
      (cond
        ;; Use alternative text-grabbing functions successively.
        ((and alt-fns  (or (if (eq 'alternatives icicle-default-thing-insertion)
                               (not flipped) ; Normal behavior for `alternatives'.
                             flipped)    ; Flipped behavior for `more-of-the-same'.
                           (not fwd-thing-fn))) ; No alternative.
         (setq icicle-successive-grab-count  1 ; In this mode, reset other mode's accumulator.
               icicle-thing-at-pt-fns-pointer
               (if (eq last-command this-command) ; If repeated, get next text-grabbing function.
                   (mod (1+ icicle-thing-at-pt-fns-pointer) (length alt-fns))
                 0))
         (let ((thing   "")
               (alt-fn  (nth icicle-thing-at-pt-fns-pointer alt-fns)))
           (save-excursion (with-current-buffer icicle-pre-minibuffer-buffer
                             (setq thing  (funcall alt-fn))))
           (setq thing  (or thing  "nil"))
           (when (and arg  (atom arg))   ; Numeric prefix arg.
             (setq thing  (condition-case err
                              (format "%s" (eval (car (read-from-string thing))))
                            (error thing))))
           (icicle-insert-thing thing)
           (icicle-msg-maybe-in-minibuffer (format "`%s'" alt-fn))))

        ;; Use same text-grabbing function successively.
        ((and fwd-thing-fn  (or (if (eq 'alternatives icicle-default-thing-insertion)
                                    flipped ; Flipped behavior for `alternatives'.
                                  (not flipped)) ; Normal behavior for `more-of-the-same'.
                                (not alt-fns))) ; No alternative.
         (if (and arg  (atom arg))

             ;; Explicit numeric arg.  If it doesn't change direction, then increment
             ;; existing count.  Otherwise, set count absolutely.
             (if (eq last-command this-command)
                 (if (= (icicle-signum icicle-successive-grab-count) ; Repeated `M-.'.
                        (icicle-signum (prefix-numeric-value arg)))
                     (setq icicle-successive-grab-count ; Same direction - increment count.
                           (* (icicle-signum icicle-successive-grab-count)
                              (+ (abs icicle-successive-grab-count)
                                 (abs (prefix-numeric-value arg)))))
                   (setq icicle-successive-grab-count  (prefix-numeric-value arg))) ; New dir - set.
               (setq icicle-successive-grab-count  (prefix-numeric-value arg))) ; First `M-.' - set.

           ;; No explicit numeric arg.
           ;; If first `M-.' or plain `C-u', set count. Otherwise, increment count.
           (if (eq last-command this-command)
               (setq icicle-successive-grab-count ; Repeated `M-.'.
                     (if (consp arg)
                         ;; We're here from plain `C-u' with `alternatives' - use 1, not 4.
                         (if (wholenump icicle-successive-grab-count) 1 -1)
                       (if (wholenump icicle-successive-grab-count) ; Increment count.
                           (+ icicle-successive-grab-count (abs (prefix-numeric-value arg)))
                         (- icicle-successive-grab-count (abs (prefix-numeric-value arg))))))
             (setq icicle-successive-grab-count  1))) ; First `M-.' - reset count.
         (let ((things  ""))
           (save-excursion
             (with-current-buffer (cadr (buffer-list))
               (setq things  (buffer-substring-no-properties
                              (point)
                              (save-excursion (funcall fwd-thing-fn icicle-successive-grab-count)
                                              (point))))))
           (icicle-insert-thing things)))))))

(defun icicle-signum (num)
  "Return 1 if NUM is positive, -1 if negative, 0 if zero."
  (cond ((< num 0) -1) ((> num 0) 1) (t 0)))

(defun icicle-insert-thing (text &optional no-replace-p)
  "Insert TEXT in the minibuffer.
TEXT replaces the last text that was inserted, if this command repeats
the last and NO-REPLACE-P is nil."
  (when (and (stringp text)  (not (string= "" text)))
    (remove-text-properties 0 (length text) '(face nil) text)
    (when (and (eq last-command this-command)
               (not no-replace-p)
               icicle-insert-string-at-pt-start) ; Ensure that we've defined the ends.
      (delete-region icicle-insert-string-at-pt-start icicle-insert-string-at-pt-end))
    (setq icicle-insert-string-at-pt-start  (point))
    (insert text)
    (setq icicle-insert-string-at-pt-end  (point))))

;;;###autoload (autoload 'icicle-insert-string-from-variable "icicles")
(defun icicle-insert-string-from-variable (askp) ; Bound to `C-=' in minibuffer.
  "Insert text into the minibuffer from a variable.
By default, the variable is user option `icicle-input-string'.  To
insert from a different variable, use a prefix argument.  You are then
prompted for the variable to use.  Completion candidates for this
include all string-valued variables.

You can use command `icicle-save-string-to-variable' to save a string
to a variable.  Typically, you store a regexp or part of a regexp in
the variable.  This command is bound in the minibuffer to `C-=', by
default.  This is especially useful when used with command
`icicle-search'.

Some regexps that you might want to assign to variables:

 \"[A-Za-z0-9_.-]+@[A-Za-z0-9_.-]+\"          ; Email address
 \"\\\\([0-9]+\\\.[0-9]+\\\.[0-9]+\\\.[0-9]+\\\\)\"     ; IP address
 \"[0-9]\\\\\\\={4\\\\}-[0-9]\\\\\\\={2\\\\}-[0-9]\\\\\\\={2\\\\}\"   ; Date: 2006-04-14, Time:
 \"^[ \\\=\\t]*[0-9]?[0-9]\\\\([:.]?[0-9][0-9]\\\\)?\\\\(am\\\\|pm\\\\|AM\\\\|PM\\\\)?\"
 \"`\\\\(\\\\sw\\\\sw+\\\\)'\"                        ; Words inside `_'
 \"\\\\*.*\\\\*\"                                 ; Special buffer name: *_*

Standard Emacs Lisp libraries are full of regexps that you can assign
to variables for use with `C-='.
 See `align.el' for regexps for programming languages.
 See `url-dav.el' for regexps matching iso8601 dates.
 See `rmail.el', `sendmail.el', and `mh-show.el' for regexps matching
 mail-header fields.

Imenu regexps occurring as parts of different values of
`imenu-generic-expression' for different buffer types can be used as
variable values for `C-='.  They all work fine with `icicle-search',
turning it into a browser or navigator for the given mode.

See, for example, `generic-x.el' and `lisp-mode.el'.  Here is a regexp
for Javascript function definitions from `generic-x.el':

 \"^function\\\\s-+\\\\([A-Za-z0-9_]+\\\\)\"

And `lisp-imenu-generic-expression' (in `lisp-mode.el') provides
regexps for Lisp function, variable, and type definitions.  Here is
the variable-definition regexp:

 \"^\\\\s-*(\\\\(def\\\\(c\\\\(onst\\\\(ant\\\\)?\\\\|ustom\\\\)\\\\|ine-symbol-macro\\\\|
 parameter\\\\|var\\\\)\\\\)\\\\s-+\\\\(\\\\(\\\\sw\\\\|\\\\s_\\\\)+\\\\)\"

Command `icicle-imenu' exploits this to automatically let you browse
definitions.  It is a specialization of `icicle-search' for Imenu.

For more useful regexps, grep for `font-lock-keywords' in Emacs `lisp'
directory and subdirs.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (save-selected-window
    (select-window (minibuffer-window))
    (if askp
        (let* ((icicle-whole-candidate-as-text-prop-p   nil)
               ;; If we didn't use this here we'd at least have to bind it to
               ;; `orig-must-pass-after-match-predicate', because of `icicle-execute-extended-command'.
               (icicle-must-pass-after-match-predicate  (lambda (s)
                                                          (let ((sym  (intern-soft s)))
                                                            (and sym  (boundp (intern s))
                                                                 (condition-case nil
                                                                     (icicle-var-is-of-type-p
                                                                      sym '(string color regexp)
                                                                      'inherit-or-value)
                                                                   (error nil))))))
               (enable-recursive-minibuffers            t)
               (var
                (intern (completing-read "Insert text from variable: " obarray  nil  nil nil
                                         (if (boundp 'variable-name-history)
                                             'variable-name-history
                                           'icicle-variable-name-history))))
               ;; Make sure we use the buffer-local value of the variable, if there is one.
               (text
                (with-current-buffer (cadr (buffer-list)) (symbol-value var))))
          (icicle-insert-thing text 'no-replace))
      (icicle-insert-thing icicle-input-string 'no-replace))))

;;;###autoload (autoload 'icicle-insert-list-join-string "icicles")
(defun icicle-insert-list-join-string () ; Bound to `C-M-j' in minibuffer during completion.
  "Insert `icicle-list-join-string' in the minibuffer.
Then, if `1on1-fit-minibuffer-frame-flag' is defined and non-nil, fit
a standalone minibuffer frame to the new minibuffer contents.
You need library `fit-frame.el' for the frame-fitting part."
  (interactive)
  (icicle-insert-thing icicle-list-join-string 'no-replace)
  (let ((len  (length icicle-list-join-string)))
    (when (and (string= "\C-j" (substring icicle-list-join-string (1- len) len))
               (boundp '1on1-fit-minibuffer-frame-flag) ; In `oneonone.el'.
               1on1-fit-minibuffer-frame-flag
               (require 'fit-frame nil t))
      (1on1-fit-minibuffer-frame))))

;;;###autoload (autoload 'icicle-dispatch-M-q "icicles")
(defun icicle-dispatch-M-q (&optional arg) ; Bound to `M-q' in minibuffer.
  "Do the right thing for `M-q'.
If searching, call `icicle-toggle-search-whole-word'.
Otherwise, call `icicle-insert-key-description'.
Bound to `M-q' in the minibuffer."
  (interactive "P") ; Argument is ignored for `icicle-toggle-search-whole-word'.
  (cond (icicle-searching-p (icicle-toggle-search-whole-word))
        (t (icicle-insert-key-description arg))))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-search-whole-word "icicles")
(defalias 'toggle-icicle-search-whole-word 'icicle-toggle-search-whole-word)
;;;###autoload (autoload 'icicle-toggle-search-whole-word "icicles")
(defun icicle-toggle-search-whole-word () ; Bound to `M-q' in minibuffer during Icicles search.
  "Toggle the value of `icicle-search-whole-word-flag'.
The new value takes effect for the next Icicles search command.
Bound to `M-q' in the minibuffer when searching."
  (interactive)
  (setq icicle-search-whole-word-flag  (not icicle-search-whole-word-flag))
  (icicle-msg-maybe-in-minibuffer
   "Whole-word searching is now %s, starting with next search"
   (icicle-propertize (if icicle-search-whole-word-flag "ON" "OFF") 'face 'icicle-msg-emphasis)))

;;;###autoload (autoload 'icicle-insert-key-description "icicles")
(defun icicle-insert-key-description (toggle-angle-brackets-p) ; Bound to `M-q' in minibuffer.
  "Read key and insert its description.
For example, if the key read is ^F, then \"C-f\" is inserted.

For Emacs 21+, `icicle-key-descriptions-use-<>-flag' determines
whether angle brackets (`<', `>') are used for named keys, such as
function keys, but a prefix argument reverses the meaning of
`icicle-key-descriptions-use-<>-flag'.

Bound to `M-q' in the minibuffer during key completion (Emacs 22+)."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let* ((enable-recursive-minibuffers  t)
         (key                           (progn (minibuffer-message " [Quoting key]") (read-event))))
    (insert (if (< emacs-major-version 21)
                (single-key-description key)
              (single-key-description key (if toggle-angle-brackets-p
                                              icicle-key-descriptions-use-<>-flag
                                            (not icicle-key-descriptions-use-<>-flag)))))))

;;;###autoload (autoload 'icicle-pp-eval-expression-in-minibuffer "icicles")
(defun icicle-pp-eval-expression-in-minibuffer (insert-value) ; Bound to `M-:' in minibuffer.
  "Evaluate an Emacs-Lisp expression and pretty-print its value.
This just calls `icicle-pp-eval-expression' from a recursive minibuffer."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (let ((enable-recursive-minibuffers  t))
    (call-interactively 'icicle-pp-eval-expression))
  (select-window (minibuffer-window))
  (select-frame-set-input-focus (selected-frame)))

;;;###autoload (autoload 'icicle-insert-newline-in-minibuffer "icicles")
(defun icicle-insert-newline-in-minibuffer (arg) ; Bound to `C-j' in minibuffer.
  "Insert a newline character (`C-j'), in the minibuffer.
Then, if `1on1-fit-minibuffer-frame-flag' is defined and non-nil, fit
a standalone minibuffer frame to the new minibuffer contents.
You need library `fit-frame.el' for the frame-fitting part."
  (interactive "p")
  (icicle-self-insert arg)
  (when (and (boundp '1on1-fit-minibuffer-frame-flag) ; In `oneonone.el'.
             1on1-fit-minibuffer-frame-flag
             (require 'fit-frame nil t))
    (1on1-fit-minibuffer-frame)))

;; Bound in minibuffer to keys in `icicle-modal-cycle-down-keys' (`down', `wheel-down').
;;
;;;###autoload (autoload 'icicle-next-candidate-per-mode "icicles")
(defun icicle-next-candidate-per-mode (&optional nth)
  "Replace input by NTH next completion candidate.
The default value of NTH is 1, meaning use the next candidate.
Negative NTH means use a previous, not subsequent, candidate.

Interactively, NTH is  the numeric prefix argument.
A plain prefix arg (`C-u') means use the first candidate.

Uses the next prefix or apropos completion command, depending on
`icicle-current-completion-mode'.  If that is nil and
`icicle-default-cycling-mode' is non-nil, uses the next history
element instead.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-candidate-per-mode]')."
  (interactive "p")
  (when (and current-prefix-arg  (consp current-prefix-arg))
    (setq icicle-candidate-nb  0
          nth                  0))
  (unless nth (setq nth  1))
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (case icicle-current-completion-mode
    (prefix
     (setq this-command
           (if (wholenump nth) 'icicle-next-prefix-candidate 'icicle-previous-prefix-candidate))
     (icicle-next-prefix-candidate nth))
    (apropos
     (setq this-command
           (if (wholenump nth) 'icicle-next-apropos-candidate 'icicle-previous-apropos-candidate))
     (icicle-next-apropos-candidate nth))
    ((nil)
     (when icicle-default-cycling-mode (next-history-element (or nth  1))))))

;; Bound in minibuffer to keys in `icicle-modal-cycle-up-keys' (`up', `wheel-up').
;;
;;;###autoload (autoload 'icicle-previous-candidate-per-mode "icicles")
(defun icicle-previous-candidate-per-mode (&optional nth)
  "Replace input by NTH previous completion candidate.
The default value of NTH is 1, meaning use the previous candidate.
Negative NTH means use a subsequent, not previous, candidate.

Interactively, NTH is  the numeric prefix argument.
A plain prefix arg (`C-u') means use the first candidate.

Uses the previous prefix or apropos completion command, depending on
`icicle-current-completion-mode'. If that is nil and
`icicle-default-cycling-mode' is non-nil, uses the previous history
element instead.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-candidate-per-mode]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-next-candidate-per-mode (- (or nth  1))))


;; Bound in minibuffer to keys in `icicle-prefix-cycle-previous-keys' (`home').
(put 'icicle-previous-prefix-candidate 'icicle-cycling-command         'backward)
(put 'icicle-previous-prefix-candidate 'icicle-prefix-cycling-command  'backward)

;;;###autoload (autoload 'icicle-previous-prefix-candidate "icicles")
(defun icicle-previous-prefix-candidate (&optional nth)
  "Replace input by NTH previous prefix completion for an input.
Default value of NTH is 1, meaning use the previous prefix completion.
Negative NTH means use a subsequent, not previous, prefix completion.

Interactively, NTH is  the numeric prefix argument.
A plain prefix arg (`C-u') means use the first candidate.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-prefix-candidate]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-next-prefix-candidate (- (or nth  1))))


;; Bound in minibuffer to keys in `icicle-next-cycle-previous-keys' (`end').
(put 'icicle-next-prefix-candidate 'icicle-cycling-command         'forward)
(put 'icicle-next-prefix-candidate 'icicle-prefix-cycling-command  'forward)

;;;###autoload (autoload 'icicle-next-prefix-candidate "icicles")
(defun icicle-next-prefix-candidate (&optional nth)
  "Replace input by NTH next prefix completion for an input.
The default value of NTH is 1, meaning use the next prefix completion.
Negative NTH means use a previous, not subsequent, prefix completion.

Interactively, NTH is  the numeric prefix argument.
A plain prefix arg (`C-u') means use the first candidate.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-prefix-candidate]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-completion-mode         'prefix
        icicle-next-apropos-complete-cycles-p  nil)
  (when (and current-prefix-arg  (consp current-prefix-arg))
    (setq icicle-candidate-nb  0
          nth                  0))
  (unless nth (setq nth  1))
  (icicle-next-candidate nth (if (icicle-file-name-input-p)
                                 'icicle-file-name-prefix-candidates
                               'icicle-prefix-candidates)))


;; Bound in minibuffer to keys in `icicle-apropos-cycle-previous-keys' (`prior').
(put 'icicle-previous-apropos-candidate 'icicle-cycling-command         'backward)
(put 'icicle-previous-apropos-candidate 'icicle-apropos-cycling-command 'backward)

;;;###autoload (autoload 'icicle-previous-apropos-candidate "icicles")
(defun icicle-previous-apropos-candidate (&optional nth)
  "Replace input by NTH previous apropos completion for an input.
Default value of NTH is 1, meaning use previous apropos completion.
Negative NTH means use a subsequent, not previous, apropos completion.

Interactively, NTH is  the numeric prefix argument.
A plain prefix arg (`C-u') means use the first candidate.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-apropos-candidate]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-next-apropos-candidate (- (or nth  1))))


;; Bound in minibuffer to keys in `icicle-apropos-cycle-next-keys' (`next').
(put 'icicle-next-apropos-candidate 'icicle-cycling-command         'forward)
(put 'icicle-next-apropos-candidate 'icicle-apropos-cycling-command 'forward)

;;;###autoload (autoload 'icicle-next-apropos-candidate "icicles")
(defun icicle-next-apropos-candidate (&optional nth)
  "Replace input by NTH next apropos completion for an input.
Default value of NTH is 1, meaning use the next apropos completion.
Negative NTH means use a previous, not subsequent, apropos completion.

Interactively, NTH is  the numeric prefix argument.
A plain prefix arg (`C-u') means use the first candidate.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-apropos-candidate]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-completion-mode        'apropos
        icicle-next-prefix-complete-cycles-p  nil)
  (when (and current-prefix-arg  (consp current-prefix-arg))
    (setq icicle-candidate-nb  0
          nth                  0))
  (unless nth (setq nth  1))
  (icicle-next-candidate nth (if (icicle-file-name-input-p)
                                 'icicle-file-name-apropos-candidates
                               'icicle-apropos-candidates)
                         'regexp-p))

;; This is not helpful or needed, because the command sets `this-command' to the proper cycling command.
;; (put 'icicle-previous-candidate-per-mode-action 'icicle-action-command t)

;; Bound  in minibuffer to keys in `icicle-modal-cycle-up-action-keys' (`C-up').
;;
;;;###autoload (autoload 'icicle-previous-candidate-per-mode-action "icicles")
(defun icicle-previous-candidate-per-mode-action (&optional nth)
  "`icicle-previous-candidate-per-mode' and `icicle-candidate-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-previous-candidate-per-mode'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-candidate-per-mode-action]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-candidate-per-mode #'icicle-candidate-action nth))

;; Bound  in minibuffer to keys in `icicle-modal-cycle-up-alt-action-keys' (`C-S-up').
;;
;;;###autoload (autoload 'icicle-previous-candidate-per-mode-alt-action "icicles")
(defun icicle-previous-candidate-per-mode-alt-action (&optional nth)
  "`icicle-previous-candidate-per-mode' and `icicle-candidate-alt-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-previous-candidate-per-mode'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-candidate-per-mode-alt-action]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-candidate-per-mode #'icicle-candidate-alt-action nth))


;; This is not helpful or needed, because the command sets `this-command' to the proper cycling command.
;;(put 'icicle-next-candidate-per-mode-action 'icicle-action-command t)

;; Bound in minibuffer to keys in `icicle-modal-cycle-down-action-keys' (`C-down').
;;
;;;###autoload (autoload 'icicle-next-candidate-per-mode-action "icicles")
(defun icicle-next-candidate-per-mode-action (&optional nth)
  "`icicle-next-candidate-per-mode' and `icicle-candidate-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-next-candidate-per-mode'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-candidate-per-mode-action]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-candidate-per-mode #'icicle-candidate-action nth))

;; Bound in minibuffer to keys in `icicle-modal-cycle-down-alt-action-keys' (`C-S-down').
;;
;;;###autoload (autoload 'icicle-next-candidate-per-mode-alt-action "icicles")
(defun icicle-next-candidate-per-mode-alt-action (&optional nth)
  "`icicle-next-candidate-per-mode' and `icicle-candidate-alt-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-next-candidate-per-mode'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-candidate-per-mode-alt-action]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-candidate-per-mode #'icicle-candidate-alt-action nth))

;; Bound in minibuffer to keys in `icicle-modal-cycle-up-help-keys' (`C-M-up').
;;
;;;###autoload (autoload 'icicle-previous-candidate-per-mode-help "icicles")
(defun icicle-previous-candidate-per-mode-help (&optional nth)
  "`icicle-previous-candidate-per-mode' and `icicle-help-on-candidate'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-previous-candidate-per-mode'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-candidate-per-mode-help]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-candidate-per-mode #'icicle-help-on-candidate nth))

;; Bound in minibuffer to keys in `icicle-modal-cycle-down-help-keys' (`C-M-down').
;;
;;;###autoload (autoload 'icicle-next-candidate-per-mode-help "icicles")
(defun icicle-next-candidate-per-mode-help (&optional nth)
  "`icicle-next-candidate-per-mode' and `icicle-help-on-candidate'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-next-candidate-per-mode'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-candidate-per-mode-help]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-candidate-per-mode #'icicle-help-on-candidate nth))


;; Bound in minibuffer to keys in `icicle-prefix-cycle-previous-action-keys' (`C-home').
(put 'icicle-previous-prefix-candidate-action 'icicle-cycling-command         'backward)
(put 'icicle-previous-prefix-candidate-action 'icicle-prefix-cycling-command  'backward)
(put 'icicle-previous-prefix-candidate-action 'icicle-action-command          t)

;;;###autoload (autoload 'icicle-previous-prefix-candidate-action "icicles")
(defun icicle-previous-prefix-candidate-action (&optional nth)
  "`icicle-previous-prefix-candidate' and `icicle-candidate-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-previous-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-prefix-candidate-action]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-prefix-candidate #'icicle-candidate-action nth))


;; Bound in minibuffer to keys in `icicle-prefix-cycle-next-action-keys' (`C-end').
(put 'icicle-next-prefix-candidate-action 'icicle-cycling-command         'forward)
(put 'icicle-next-prefix-candidate-action 'icicle-prefix-cycling-command  'forward)
(put 'icicle-next-prefix-candidate-action 'icicle-action-command          t)

;;;###autoload (autoload 'icicle-next-prefix-candidate-action "icicles")
(defun icicle-next-prefix-candidate-action (&optional nth)
  "`icicle-next-prefix-candidate' and `icicle-candidate-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-next-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-prefix-candidate-action]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-prefix-candidate #'icicle-candidate-action nth))


;; Bound in minibuffer to keys in `icicle-apropos-cycle-previous-action-keys' (`C-prior').
(put 'icicle-previous-apropos-candidate-action 'icicle-cycling-command         'backward)
(put 'icicle-previous-apropos-candidate-action 'icicle-apropos-cycling-command 'backward)
(put 'icicle-previous-apropos-candidate-action 'icicle-action-command          t)

;;;###autoload (autoload 'icicle-previous-apropos-candidate-action "icicles")
(defun icicle-previous-apropos-candidate-action (&optional nth)
  "`icicle-previous-apropos-candidate' and `icicle-candidate-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-previous-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-apropos-candidate-action]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-apropos-candidate #'icicle-candidate-action nth))


;; Bound in minibuffer to keys in `icicle-apropos-cycle-next-action-keys' (`C-next').
(put 'icicle-next-apropos-candidate-action 'icicle-cycling-command         'forward)
(put 'icicle-next-apropos-candidate-action 'icicle-apropos-cycling-command 'forward)
(put 'icicle-next-apropos-candidate-action 'icicle-action-command          t)

;;;###autoload (autoload 'icicle-next-apropos-candidate-action "icicles")
(defun icicle-next-apropos-candidate-action (&optional nth)
  "`icicle-next-apropos-candidate' and `icicle-candidate-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-next-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-apropos-candidate-action]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-apropos-candidate #'icicle-candidate-action nth))


;; Bound in minibuffer to keys in `icicle-prefix-cycle-previous-alt-action-keys' (`C-S-home').
(put 'icicle-previous-prefix-candidate-alt-action 'icicle-cycling-command         'backward)
(put 'icicle-previous-prefix-candidate-alt-action 'icicle-prefix-cycling-command  'backward)

;;;###autoload (autoload 'icicle-previous-prefix-candidate-alt-action "icicles")
(defun icicle-previous-prefix-candidate-alt-action (&optional nth)
  "`icicle-previous-prefix-candidate' and `icicle-candidate-alt-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for `icicle-previous-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-prefix-candidate-alt-action]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-prefix-candidate #'icicle-candidate-alt-action nth))


;; Bound in minibuffer to keys in `icicle-prefix-cycle-next-alt-action-keys' (`C-S-end').
(put 'icicle-next-prefix-candidate-alt-action 'icicle-cycling-command         'forward)
(put 'icicle-next-prefix-candidate-alt-action 'icicle-prefix-cycling-command  'forward)

;;;###autoload (autoload 'icicle-next-prefix-candidate-alt-action "icicles")
(defun icicle-next-prefix-candidate-alt-action (&optional nth)
  "`icicle-next-prefix-candidate' and `icicle-candidate-alt-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-next-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-prefix-candidate-alt-action]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-prefix-candidate #'icicle-candidate-alt-action nth))


;; Bound in minibuffer to keys in `icicle-apropos-cycle-previous-alt-action-keys' (`C-S-prior').
(put 'icicle-previous-apropos-candidate-alt-action 'icicle-cycling-command         'backward)
(put 'icicle-previous-apropos-candidate-alt-action 'icicle-apropos-cycling-command 'backward)

;;;###autoload (autoload 'icicle-previous-apropos-candidate-alt-action "icicles")
(defun icicle-previous-apropos-candidate-alt-action (&optional nth)
  "`icicle-previous-apropos-candidate' and `icicle-candidate-alt-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-previous-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-apropos-candidate-alt-action]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-apropos-candidate #'icicle-candidate-alt-action nth))


;; Bound in minibuffer to keys in `icicle-apropos-cycle-next-alt-action-keys' (`C-S-next').
(put 'icicle-next-apropos-candidate-alt-action 'icicle-cycling-command         'forward)
(put 'icicle-next-apropos-candidate-alt-action 'icicle-apropos-cycling-command 'forward)

;;;###autoload (autoload 'icicle-next-apropos-candidate-alt-action "icicles")
(defun icicle-next-apropos-candidate-alt-action (&optional nth)
  "`icicle-next-apropos-candidate' and `icicle-candidate-alt-action'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-next-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-apropos-candidate-alt-action]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-apropos-candidate #'icicle-candidate-alt-action nth))


;; Bound in minibuffer to keys in `icicle-prefix-cycle-previous-help-keys' (`C-M-home').
(put 'icicle-help-on-previous-prefix-candidate 'icicle-cycling-command         'backward)
(put 'icicle-help-on-previous-prefix-candidate 'icicle-prefix-cycling-command  'backward)

;;;###autoload (autoload 'icicle-help-on-previous-prefix-candidate "icicles")
(defun icicle-help-on-previous-prefix-candidate (&optional nth)
  "`icicle-previous-prefix-candidate' and `icicle-help-on-candidate'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-previous-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-help-on-previous-prefix-candidate]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-prefix-candidate #'icicle-help-on-candidate nth))


;; Bound in minibuffer to keys in `icicle-prefix-cycle-next-help-keys' (`C-M-end').
(put 'icicle-help-on-next-prefix-candidate 'icicle-cycling-command         'forward)
(put 'icicle-help-on-next-prefix-candidate 'icicle-prefix-cycling-command  'forward)

;;;###autoload (autoload 'icicle-help-on-next-prefix-candidate "icicles")
(defun icicle-help-on-next-prefix-candidate (&optional nth)
  "`icicle-next-prefix-candidate' and `icicle-help-on-candidate'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-next-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-help-on-next-prefix-candidate]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-prefix-candidate #'icicle-help-on-candidate nth))


;; Bound in minibuffer to keys in `icicle-apropos-cycle-previous-help-keys' (`C-M-prior').
(put 'icicle-help-on-previous-apropos-candidate 'icicle-cycling-command         'backward)
(put 'icicle-help-on-previous-apropos-candidate 'icicle-apropos-cycling-command 'backward)

;;;###autoload (autoload 'icicle-help-on-previous-apropos-candidate "icicles")
(defun icicle-help-on-previous-apropos-candidate (&optional nth)
  "`icicle-previous-apropos-candidate' and `icicle-help-on-candidate'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-previous-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-help-on-previous-apropos-candidate]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-previous-apropos-candidate #'icicle-help-on-candidate nth))


;; Bound in minibuffer to keys in `icicle-apropos-cycle-previous-help-keys' (`C-M-next').
(put 'icicle-help-on-next-apropos-candidate 'icicle-cycling-command         'forward)
(put 'icicle-help-on-next-apropos-candidate 'icicle-apropos-cycling-command 'forward)

;;;###autoload (autoload 'icicle-help-on-next-apropos-candidate "icicles")
(defun icicle-help-on-next-apropos-candidate (&optional nth)
  "`icicle-next-apropos-candidate' and `icicle-help-on-candidate'.
Option `icicle-act-before-cycle-flag' determines which occurs first.

Optional argument NTH (the numeric prefix argument) is as for
`icicle-next-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-help-on-next-apropos-candidate]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-successive-action #'icicle-next-apropos-candidate #'icicle-help-on-candidate nth))

(defun icicle-successive-action (nav-fn action-fn nth)
  "Call NAV-FN and ACTION-FN.  Pass argument NTH to NAV-FN.
Set `icicle-current-completion-mode'.
The order between NAV-FN and ACTION-FN respects the value of
`icicle-act-before-cycle-flag'."
  ;; Set mode only if known.  Otherwise, leave it alone (e.g. for per-mode functions).
  (cond ((icicle-get-safe nav-fn 'icicle-apropos-cycling-command)
         (setq icicle-current-completion-mode        'apropos
               icicle-next-prefix-complete-cycles-p  nil))
        ((icicle-get-safe nav-fn 'icicle-prefix-cycling-command)
         (setq icicle-current-completion-mode         'prefix
               icicle-next-apropos-complete-cycles-p  nil)))

  ;; We bind `icicle-acting-on-next/prev' to non-nil (and the direction) while calling the action
  ;; function.  This is used by Icicles search-and-replace (`icicle-search-highlight-and-maybe-replace')
  ;; to ensure the correct candidate number for a series of replacements.
  ;; (Not currently used for the `icicle-act-before-cycle-flag' case, but we do it there also, anyway.)
  (cond (icicle-act-before-cycle-flag
         (let ((icicle-acting-on-next/prev  (icicle-get-safe nav-fn 'icicle-cycling-command)))
           (save-excursion (save-selected-window (funcall action-fn))))
         (funcall nav-fn nth))
        (t
         ;; Inhibit showing help in mode-line while moving to next/previous candidate
         ;; in `*Completions*', because help sits for `icicle-help-in-mode-line-delay' sec.
         ;; Display the help after we do the action.
         (let ((icicle-help-in-mode-line-delay  0)) (funcall nav-fn nth))
         (let ((icicle-acting-on-next/prev  (icicle-get-safe nav-fn 'icicle-cycling-command)))
           (save-excursion (save-selected-window (funcall action-fn))))
         (when (stringp icicle-last-completion-candidate)
           (icicle-show-help-in-mode-line icicle-last-completion-candidate)))))


;; Bound in minibuffer to keys in `icicle-prefix-complete-keys' (`TAB').
(put 'icicle-prefix-complete 'icicle-cycling-command t)
(put 'icicle-prefix-complete 'icicle-prefix-cycling-command t)
(put 'icicle-prefix-complete 'icicle-completing-command t)
(put 'icicle-prefix-complete 'icicle-prefix-completing-command t)

;;;###autoload (autoload 'icicle-prefix-complete "icicles")
(defun icicle-prefix-complete ()
  "Complete the minibuffer contents as far as possible, as a prefix.
Repeat this to cycle among candidate completions.
If no characters can be completed, display the possible completions.
Candidate completions are appropriate names whose prefix is the
minibuffer input, where appropriateness is determined by the context
\(command, variable, and so on).
Return nil if there is no valid completion.
Otherwise, return the list of completion candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (unless (string= icicle-dot-string-internal ".")
    (icicle-convert-dots t t)
    (setq icicle-dot-string-internal  "."))
  (icicle-prefix-complete-1))


;; Bound in minibuffer to keys in `icicle-prefix-complete-no-display-keys' (`C-M-TAB').
(put 'icicle-prefix-complete-no-display 'icicle-cycling-command t)
(put 'icicle-prefix-complete-no-display 'icicle-prefix-cycling-command t)
(put 'icicle-prefix-complete-no-display 'icicle-completing-command t)
(put 'icicle-prefix-complete-no-display 'icicle-prefix-completing-command t)

;;;###autoload (autoload 'icicle-prefix-complete-no-display "icicles")
(defun icicle-prefix-complete-no-display (&optional no-msg-p) ; Bound to `C-M-TAB' in minibuffer.
  "Like `icicle-prefix-complete', but without displaying `*Completions*'.
Optional arg NO-MSG-P non-nil means do not show a minibuffer message
indicating that candidates were updated.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete-no-display]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-prefix-complete-1 (if no-msg-p 'no-msg 'no-display)))


;; Bound in minibuffer to keys in `icicle-word-completion-keys' (`M-SPC').
(put 'icicle-prefix-word-complete 'icicle-cycling-command t)
(put 'icicle-prefix-word-complete 'icicle-prefix-cycling-command t)
(put 'icicle-prefix-word-complete 'icicle-completing-command t)
(put 'icicle-prefix-word-complete 'icicle-prefix-completing-command t)

;;;###autoload (autoload 'icicle-prefix-word-complete "icicles")
(defun icicle-prefix-word-complete ()
  "Complete the minibuffer contents at most a single word.
Repeating this completes additional words.
Spaces and hyphens in candidates are considered word separators.
If only a single candidate matches, the input is completed entirely.
Return nil if there is no valid completion, else t.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-prefix-word-complete]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-prefix-complete-1 nil t))

(defun icicle-prefix-complete-1 (&optional no-display-p word-p)
  "Helper function for `icicle-prefix-complete(-no-display)'.
Return the list of completion candidates.
Optional argument NO-DISPLAY-P non-nil means do not display buffer
 `*Completions*'.  If the value is `no-msg', then do not show any
  message either.  NO-DISPLAY-P is passed to
 `icicle-display-candidates-in-Completions' as its second arg.
Optional argument WORD-P non-nil means complete only a word at a time."
  (when (and icicle-multi-completing-p  (icicle-string-match-p (regexp-quote icicle-list-join-string)
                                                               (icicle-input-from-minibuffer)))
    (icicle-msg-maybe-in-minibuffer
     (substitute-command-keys
     "Use APROPOS completion (`S-TAB') to match multi-completions past first part")))
  (let ((ipc1-was-cycling-p  icicle-cycling-p)
        (mode-line-help      nil))
    (setq icicle-current-input                   (if (and icicle-last-input
                                                          icicle-cycling-p
                                                          (not icicle-edit-update-p)
                                                          (eq icicle-current-completion-mode 'prefix)
                                                          (or (not word-p)
                                                              (eq this-command last-command))
                                                          (symbolp last-command)
                                                          (or (get last-command 'icicle-cycling-command)
                                                              (get last-command 'icicle-action-command))
                                                          icicle-completion-candidates)
                                                     icicle-last-input
                                                   (if (icicle-file-name-input-p)
                                                       (abbreviate-file-name
                                                        (icicle-input-from-minibuffer 'leave-envar))
                                                     (icicle-input-from-minibuffer)))
          icicle-current-completion-mode         'prefix
          icicle-next-apropos-complete-cycles-p  nil
          icicle-input-fail-pos                  nil
          icicle-cycling-p                       nil)
    (when icicle-edit-update-p (setq icicle-next-prefix-complete-cycles-p  nil))
    (let ((word-complete-input      "")
          (input-before-completion  icicle-current-input)
          return-value)
      (unless (and (stringp icicle-current-input) (stringp icicle-last-input)
                   (string= icicle-current-input icicle-last-input)
                   (symbolp last-command)
                   (or (get last-command 'icicle-prefix-completing-command)
                       (get last-command 'icicle-action-command))
                   (not word-p))
        (unless (or icicle-edit-update-p  (get-buffer-window "*Completions*" 0)  no-display-p)
          (message "Computing completion candidates..."))
        (if (not word-p)
            (setq icicle-completion-candidates
                  (icicle-condition-case-no-debug nil
                      (if (icicle-file-name-input-p)
                          (icicle-file-name-prefix-candidates icicle-current-input)
                        (icicle-prefix-candidates icicle-current-input))
                    (error icicle-completion-candidates))) ; No change if completion error.
          ;; Complete a word.  Save input before trying to complete.
          ;; Update `icicle-current-input': `minibuffer-complete-word' might have completed the input
          ;; beyond a complete candidate - e.g. `forwar-char' to `forward-char-'.
          (setq word-complete-input   (icicle-input-from-minibuffer)
                return-value
                (let ((temp-buffer-show-hook       nil) ; Don't let it fit frame here.
                      (completion-auto-help        nil) ; Don't show `*Completions*'.
                      (minibuffer-message-timeout  0)) ; No timeout.
                  (icicle-clear-minibuffer)
                  (insert icicle-current-input)
                  (save-selected-window (minibuffer-complete-word)))
                icicle-current-input  (icicle-input-from-minibuffer)) ; Update input.
          ;; If incremental compl., or completed some, or not repeated, then update input and recompute.
          (when (or icicle-edit-update-p
                    (> (length icicle-current-input) (length word-complete-input))
                    (not (eq this-command last-command)))
            (setq word-complete-input           icicle-current-input
                  icicle-completion-candidates  (icicle-condition-case-no-debug nil
                                                    (if (icicle-file-name-input-p)
                                                        (icicle-file-name-prefix-candidates
                                                         icicle-current-input)
                                                      (icicle-prefix-candidates icicle-current-input))
                                                  (error icicle-completion-candidates)))))
        (message nil))                  ; Clear out "Computing completion candidates..." message.
      (unless word-p (setq return-value  icicle-completion-candidates)) ; Word returns special value.
      (icicle-save-or-restore-input)
      (cond ((null icicle-completion-candidates)
             (setq icicle-nb-of-other-cycle-candidates  0)
             (let ((icicle-incremental-completion ; Upgrade if OK for explicit.
                    (or (memq icicle-highlight-input-completion-failure
                              '(explicit-strict explicit explicit-remote))
                        icicle-incremental-completion)))
               (icicle-highlight-input-noncompletion))
             (save-selected-window (icicle-remove-Completions-window))
             (run-hooks 'icicle-no-match-hook)
             (unless (eq no-display-p 'no-msg)
               (minibuffer-message (case (icicle-current-TAB-method)
                                     (fuzzy        "  [No fuzzy completions]")
                                     (vanilla      "  [No vanilla completions]")
                                     (swank        "  [No swank (fuzzy symbol) completions]")
                                     (t            "  [No prefix completions]")))))

            ;; Single matching candidate.
            ((null (cdr icicle-completion-candidates))
             (setq icicle-current-input  (if (not (icicle-file-name-input-p))
                                             ;; Transfer any `icicle-whole-candidate' property from
                                             ;; candidate to `icicle-current-input', so things that use
                                             ;; `icicle-candidates-alist' will work.
                                             (car icicle-completion-candidates)
                                           (if (string= "" icicle-current-input)
                                               (or (icicle-file-name-directory icicle-current-input)  "")
                                             (directory-file-name
                                              (icicle-abbreviate-or-expand-file-name
                                               (car icicle-completion-candidates)
                                               (icicle-file-name-directory icicle-current-input))))))


             (setq icicle-nb-of-other-cycle-candidates  0)
             (cond (;; Do not expand input in minibuffer - just show `*Completions*'.
                    (and (not no-display-p) ; Always expand for NO-DISPLAY-P.
                         (or (eq 0 icicle-expand-input-to-common-match) ; `never'
                             (and icicle-edit-update-p ; `explicit' means no auto-expansion.
                                  (eq 1 icicle-expand-input-to-common-match)))) ; `explicit'
                    (when icicle-incremental-completion-p  (sit-for icicle-incremental-completion-delay))
                    (icicle-display-candidates-in-Completions))
                   (t
                    ;; Expand input to sole match in minibuffer.
                    (unless (and icicle-edit-update-p
                                 (or (not icicle-incremental-completion-p)
                                     (not (sit-for icicle-incremental-completion-delay))))
                      (icicle-clear-minibuffer)
                      (let ((cand  (car icicle-completion-candidates)))
                        (if (icicle-file-name-input-p)
                            (cond ((string= "" cand) ; This indicates an empty dir.
                                   (setq icicle-last-completion-candidate  icicle-current-input))
                                  ((eq ?\/  (aref cand (1- (length cand))))
                                   ;; Add `/', so cycling expands dir.
                                   (setq icicle-current-input (file-name-as-directory
                                                               icicle-current-input)
                                         icicle-last-completion-candidate  icicle-current-input))
                                  (t    ; Non-dir - use the candidate file, but without any dir.
                                   (setq icicle-last-completion-candidate
                                         (icicle-file-name-nondirectory cand))))
                          (setq icicle-last-completion-candidate  cand)))
                      (let ((inserted  (if (and (icicle-file-name-input-p) insert-default-directory
                                                (or (not (member icicle-last-completion-candidate
                                                                 icicle-extra-candidates))
                                                    icicle-extra-candidates-dir-insert-p))
                                           (icicle-abbreviate-or-expand-file-name
                                            icicle-last-completion-candidate
                                            (icicle-file-name-directory-w-default icicle-current-input))
                                         icicle-last-completion-candidate)))
                        (insert inserted))
                      (save-selected-window (icicle-remove-Completions-window))
                      ;; Do not transform multi-completion here.  It should be done in the function that
                      ;; acts on the chosen completion candidate.  For a multi-command, that means it
                      ;; should be done in the action function.
                      ;; $$$$$$ (icicle-transform-sole-candidate)
                      icicle-current-input)))
             (unless (boundp 'icicle-prefix-complete-and-exit-p)
               (icicle-highlight-complete-input)
               (cond ((and icicle-top-level-when-sole-completion-flag
                           (or (and (not (and (boundp 'icicle-ido-like-mode) icicle-ido-like-mode))
                                    (not icicle-files-ido-like-flag))
                               (and (not (icicle-file-name-input-p))
                                    (not icicle-abs-file-candidates))
                               (string= "" (car icicle-completion-candidates)) ; Empty directory
                               (not (eq ?\/  (aref (car icicle-completion-candidates)
                                                   (1- (length (car icicle-completion-candidates)))))))
                           (sit-for icicle-top-level-when-sole-completion-delay))
                      (set minibuffer-history-variable
                           (cons icicle-current-input (symbol-value minibuffer-history-variable)))
                      (icicle-condition-case-no-debug icicle-prefix-complete-1
                          (throw 'icicle-read-top
                            (if (and (icicle-file-name-input-p)
                                     insert-default-directory
                                     (or (not (member icicle-current-input icicle-extra-candidates))
                                         icicle-extra-candidates-dir-insert-p))
                                (expand-file-name icicle-current-input)
                              icicle-current-input))
                        (no-catch (icicle-retrieve-last-input)
                                  icicle-current-input)
                        (error (message "%s" (error-message-string icicle-prefix-complete-1)))))
                     ((and icicle-edit-update-p  (not (eq no-display-p 'no-msg)))
                      (minibuffer-message
                       (format (case (icicle-current-TAB-method)
                                 (fuzzy        "  [One fuzzy completion: %s]")
                                 (vanilla      "  [One vanilla completion: %s]")
                                 (swank        "  [One swank (fuzzy symbol) completion: %s]")
                                 (t            "  [One prefix completion: %s]"))
                               icicle-current-input))
                      (setq mode-line-help  icicle-current-input))
                     ((not (eq no-display-p 'no-msg))
                      (minibuffer-message (case (icicle-current-TAB-method)
                                            (fuzzy        "  [Sole fuzzy completion]")
                                            (vanilla      "  [Sole vanilla completion]")
                                            (swank        "  [Sole swank (fuzzy symbol) completion]")
                                            (t            "  [Sole prefix completion]")))
                      (setq mode-line-help  icicle-current-input)))))

            ;; Multiple candidates.

            (;; Do not expand input in minibuffer - just show `*Completions*'.
             (and (not no-display-p) ; Always expand for NO-DISPLAY-P.
                  (or (eq 0 icicle-expand-input-to-common-match) ; `never'
                      (and icicle-edit-update-p ; No autoexpansion for `explicit', `sole-match'
                           (memq icicle-expand-input-to-common-match '(1 2)))))
             (when icicle-incremental-completion-p  (sit-for icicle-incremental-completion-delay))
             (icicle-display-candidates-in-Completions))

            (t
             ;; Complete: expand input to match common prefix.
             (if icicle-edit-update-p
                 (if (or (not icicle-incremental-completion-p)
                         (not (sit-for icicle-incremental-completion-delay)))
                     ;; Even though we do this unconditionally, we need to do it after the `sit-for',
                     ;; not before, because displaying causes Emacs to think that user input might be
                     ;; pending.
                     (icicle-display-candidates-in-Completions nil no-display-p)
                   (icicle-display-candidates-in-Completions nil no-display-p)
                   (when (icicle-prefix-complete-2 word-p)
                     (setq mode-line-help  (icicle-minibuf-input-sans-dir icicle-current-input))))
               (when (icicle-prefix-complete-2 word-p)
                 (setq mode-line-help  (icicle-minibuf-input-sans-dir icicle-current-input)))
               (cond (;; Candidates visible.  If second prefix complete, cycle, else update candidates.
                      (get-buffer-window "*Completions*" 0)
                      (if (and (or ipc1-was-cycling-p  icicle-next-prefix-complete-cycles-p)
                               (icicle-get-safe icicle-last-completion-command
                                                'icicle-prefix-completing-command)
                               (if word-p
                                   ;; Word completion cycles only if both of these are true:
                                   ;; * Input is not yet complete (null `return-value').
                                   ;; * Either last command was an edit and input does not end in `-',
                                   ;;          or the current input is from cycling.
                                   ;; E.g. `M-x fo M-SPC r M-SPC' cycles among `foreground-color' etc.
                                   (and (not return-value)
                                        (or (and (not (or (icicle-get-safe
                                                           last-command 'icicle-prefix-completing-command)
                                                          (icicle-get-safe
                                                           last-command 'icicle-action-command)))
                                                 (not (eq ?-  (aref icicle-current-input
                                                                    (1- (length icicle-current-input))))))
                                            (not (string= icicle-last-input word-complete-input))))
                                 (and (symbolp last-command)
                                      (or (get last-command 'icicle-prefix-completing-command)
                                          (get last-command 'icicle-action-command)
                                          ;; This is necessary because when this option is non-nil
                                          ;; `icicle-(prefix|apropos)-complete' is called from code,
                                          ;; so `last-command' has not been set to it.
                                          icicle-show-Completions-initially-flag))))
                          ;; Second prefix complete in a row.  Cycle down.
                          (icicle-next-candidate 1 (if (icicle-file-name-input-p)
                                                       'icicle-file-name-prefix-candidates
                                                     'icicle-prefix-candidates))
                        ;; User did something else (e.g. changed input).  Update the candidates.
                        (icicle-display-candidates-in-Completions nil no-display-p)))
                     (;; No candidates shown.  Could be first completion or could follow `C-M-(S-)TAB'.
                      icicle-TAB-shows-candidates-flag
                      (if (not (and (or ipc1-was-cycling-p  icicle-next-prefix-complete-cycles-p)
                                    (icicle-get-safe icicle-last-completion-command
                                                     'icicle-prefix-completing-command)
                                    (symbolp last-command)
                                    (or (get last-command 'icicle-prefix-completing-command)
                                        (get last-command 'icicle-action-command))
                                    (not word-p)))
                          ;; First prefix complete is enough to update candidates.
                          (icicle-display-candidates-in-Completions nil no-display-p)
                        ;; Second prefix complete.  If `TAB', then it follows `C-M-TAB', so show window.
                        (unless no-display-p (icicle-display-candidates-in-Completions nil))
                        (icicle-next-candidate 1 (if (icicle-file-name-input-p)
                                                     'icicle-file-name-prefix-candidates
                                                   'icicle-prefix-candidates))))
                     (;; No candidates shown.  Second prefix complete.
                      ;; If NO-DISPLAY-P and either not WORD-P or input is complete, then cycle down.
                      ;; Else, vanilla Emacs: second `TAB' shows candidates.
                      (and (icicle-get-safe icicle-last-completion-command
                                            'icicle-prefix-completing-command)
                           (symbolp last-command)
                           (or (get last-command 'icicle-prefix-completing-command)
                               (get last-command 'icicle-action-command))
                           completion-auto-help)
                      (if (or (not no-display-p)  (and word-p  (not return-value)))
                          (icicle-display-candidates-in-Completions nil)
                        (icicle-next-candidate 1 (if (icicle-file-name-input-p)
                                                     'icicle-file-name-prefix-candidates
                                                   'icicle-prefix-candidates))))
                     ;; Input is complete, but exist other candidates with same prefix.
                     ((and (member icicle-current-input icicle-completion-candidates)
                           (not (eq no-display-p 'no-msg)))
                      (minibuffer-message "  [Complete, but not unique]"))))))
      (setq icicle-last-completion-command        (if word-p
                                                      'icicle-prefix-word-complete
                                                    (if no-display-p
                                                        'icicle-prefix-complete-no-display
                                                      'icicle-prefix-complete))
            ;; $$$$$$ Trying this - added wrapper (or (not word-p)...)
            icicle-next-prefix-complete-cycles-p  (or (not word-p)
                                                      (equal input-before-completion
                                                             (icicle-input-from-minibuffer
                                                              'leave-envvars))))
      (when mode-line-help (icicle-show-help-in-mode-line mode-line-help))
      return-value)))

(defun icicle-prefix-complete-2 (word-p)
  "Replace minibuffer content with highlighted current input.
Call `icicle-highlight-initial-whitespace'.
If completing file name, set default dir based on current completion.

If input is complete and we are not in the process of exiting the
minibuffer, then call `icicle-highlight-complete-input'.  Return the
value of this condition: nil or non-nil.

Non-nil WORD-P means this is word completion, so just highlight
existing content (do not replace it), and set default dir."
  (unless word-p
    (icicle-clear-minibuffer)
    (save-window-excursion
      ;; Shouldn't need to explicitly select minibuffer like this, since `*Completions*' input is
      ;; directed there.  But there seems to be an Emacs bug somewhere, because although using just
      ;; `insert' inserts the input in the minibuffer OK, in some cases the cursor might not follow the
      ;; insertion.
      (select-window (active-minibuffer-window))
      (insert icicle-current-input))
    ;; Shouldn't need to do this if it is on `post-command-hook', but it seems we need to.
    (when (and (boundp '1on1-fit-minibuffer-frame-flag)
               1on1-fit-minibuffer-frame-flag
               (require 'fit-frame nil t))
      (1on1-fit-minibuffer-frame)))
  (deactivate-mark)
  (icicle-highlight-initial-whitespace icicle-current-input)
  (let ((complete-&-not-exiting-p  (and (not (boundp 'icicle-prefix-complete-and-exit-p))
                                        (icicle-input-is-a-completion-p icicle-current-input))))
    (when complete-&-not-exiting-p (icicle-highlight-complete-input))
    complete-&-not-exiting-p))

(defun icicle-input-is-a-completion-p (&optional input)
  "Return non-nil if the input is a valid completion.
Optional arg INPUT is passed to `icicle-minibuffer-input-sans-dir'.
This is essentially a `member' test, except for environment vars, for
which the initial `$' is ignored."
  (let* ((input-sans-dir  (icicle-minibuf-input-sans-dir input))
         (env-var-name    (and (icicle-not-basic-prefix-completion-p)
                               (> (length input-sans-dir) 0)
                               (eq ?\$ (aref input-sans-dir 0))
                               (substring input-sans-dir 1))))
    (unless (or icicle-last-completion-candidate  input)
      (setq icicle-completion-candidates  (funcall (if (eq icicle-current-completion-mode 'prefix)
                                                       #'icicle-prefix-candidates
                                                     #'icicle-apropos-candidates)
                                                   input-sans-dir)))
    (member (icicle-upcase-if-ignore-case (or env-var-name  input-sans-dir))
            (mapcar #'icicle-upcase-if-ignore-case icicle-completion-candidates))))

(defun icicle-upcase-if-ignore-case (string)
  "Return (icicle-upcase STRING) if `completion-ignore-case', else STRING."
  (if completion-ignore-case (icicle-upcase string) string))


;; Bound in minibuffer to keys in `icicle-apropos-complete-keys' (`S-TAB').
(put 'icicle-apropos-complete 'icicle-cycling-command t)
(put 'icicle-apropos-complete 'icicle-apropos-cycling-command t)
(put 'icicle-apropos-complete 'icicle-completing-command t)
(put 'icicle-apropos-complete 'icicle-apropos-completing-command t)

;;;###autoload (autoload 'icicle-apropos-complete "icicles")
(defun icicle-apropos-complete ()
  "Complete the minibuffer contents as far as possible.
Repeat this to cycle among candidate completions.
This uses \"apropos completion\", defined as follows:
A completion contains the minibuffer input somewhere, as a substring.
Display a list of possible completions in buffer `*Completions*'.
Candidate completions are appropriate names that match the current
input, taken as a regular expression, where appropriateness is
determined by the context (command, variable, and so on).
Return nil if there is no valid completion.
Otherwise, return the list of completion candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-apropos-complete]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (when (and (string= icicle-dot-string icicle-anychar-regexp)
             (not (string= icicle-dot-string-internal icicle-anychar-regexp)))
    (icicle-convert-dots (equal icicle-current-input icicle-last-input)) ; No confirm if same input.
    (setq icicle-dot-string-internal  (icicle-anychar-regexp)))
  (let* ((error-msg  nil)               ; Apropos complete.
         (candidates
          (icicle-condition-case-no-debug lossage
              (icicle-apropos-complete-1)
            (invalid-regexp
             (setq error-msg  (cadr lossage))
             (when (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid " error-msg)
               (setq error-msg  "incomplete input")))
            (error (setq error-msg  (error-message-string lossage))))))
    (when error-msg (minibuffer-message (concat "  " error-msg)))
    candidates))


;; Bound in minibuffer to keys in `icicle-apropos-complete-no-display-keys' (`C-M-S-TAB').
(put 'icicle-apropos-complete-no-display 'icicle-cycling-command t)
(put 'icicle-apropos-complete-no-display 'icicle-apropos-cycling-command t)
(put 'icicle-apropos-complete-no-display 'icicle-completing-command t)
(put 'icicle-apropos-complete-no-display 'icicle-apropos-completing-command t)

;;;###autoload (autoload 'icicle-apropos-complete-no-display "icicles")
(defun icicle-apropos-complete-no-display (&optional no-msg-p)
  "Like `icicle-apropos-complete', but without displaying `*Completions*'.
Optional arg NO-MSG-P non-nil means do not show a minibuffer message
indicating that candidates were updated.
You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-apropos-complete-no-display]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let* ((error-msg  nil)
         (candidates
          (icicle-condition-case-no-debug lossage
              (icicle-apropos-complete-1 (if no-msg-p 'no-msg 'no-display))
            (invalid-regexp
             (setq error-msg  (cadr lossage))
             (when (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid " error-msg)
               (setq error-msg  "incomplete input")))
            (error (setq error-msg  (error-message-string lossage))))))
    (when error-msg (minibuffer-message (concat "  " error-msg)))
    candidates))

(defun icicle-apropos-complete-1 (&optional no-display-p)
  "Helper function for `icicle-apropos-complete(-no-display)'.
This does everything except deal with regexp-match errors.
Return the list of completion candidates.

Optional argument NO-DISPLAY-P non-nil means do not display buffer
`*Completions*'.  If the value is `no-msg', then do not show any
message either.  NO-DISPLAY-P is passed to
`icicle-display-candidates-in-Completions' as its second arg."
  (let ((iac1-was-cycling-p  icicle-cycling-p)
        (mode-line-help      nil)
        input-before-completion)
    (setq icicle-current-input                  (if (and icicle-last-input
                                                         icicle-cycling-p
                                                         (not icicle-edit-update-p)
                                                         (eq icicle-current-completion-mode 'apropos)
                                                         (symbolp last-command)
                                                         (or (get last-command 'icicle-cycling-command)
                                                             (get last-command 'icicle-action-command))
                                                         icicle-completion-candidates)
                                                    icicle-last-input
                                                  (icicle-input-from-minibuffer))
          icicle-current-completion-mode        'apropos
          icicle-next-prefix-complete-cycles-p  nil
          icicle-input-fail-pos                 nil
          icicle-cycling-p                      nil)
    (when icicle-edit-update-p (setq icicle-next-apropos-complete-cycles-p  nil))
    (when (icicle-file-name-input-p)
      (setq icicle-current-input  (abbreviate-file-name
                                   (if icicle-regexp-quote-flag
                                       (substitute-in-file-name icicle-current-input)
                                     icicle-current-input))))
    (setq input-before-completion  icicle-current-input)
    (unless (or icicle-edit-update-p  (get-buffer-window "*Completions*" 0) no-display-p)
      (message "Computing completion candidates..."))
    (unless (and (stringp icicle-current-input)
                 (stringp icicle-last-input)
                 (string= icicle-current-input icicle-last-input)
                 (symbolp last-command)
                 (or (get last-command 'icicle-apropos-completing-command)
                     (get last-command 'icicle-action-command)))
      (setq icicle-completion-candidates
            (icicle-condition-case-no-debug nil
                (if (icicle-file-name-input-p)
                    (icicle-file-name-apropos-candidates icicle-current-input)
                  (icicle-apropos-candidates icicle-current-input))
              (error icicle-completion-candidates)))) ; No change if completion error.
    (icicle-save-or-restore-input)
    (cond ((null icicle-completion-candidates)
           (setq icicle-nb-of-other-cycle-candidates  0)
           (let ((icicle-incremental-completion ; Upgrade if OK for explicit.
                  (or (memq icicle-highlight-input-completion-failure
                            '(explicit-strict explicit explicit-remote))
                      icicle-incremental-completion)))
             (icicle-highlight-input-noncompletion))
           (save-selected-window (icicle-remove-Completions-window))
           (run-hooks 'icicle-no-match-hook)
           (unless (eq no-display-p 'no-msg)
             (minibuffer-message (let ((typ  (car (rassq icicle-apropos-complete-match-fn
                                                         icicle-S-TAB-completion-methods-alist))))
                                   (concat "  [No " typ (and typ  " ") "completion]")))))

          ;; Single matching candidate.
          ((null (cdr icicle-completion-candidates))
           (setq icicle-current-input  (if (not (icicle-file-name-input-p))
                                           ;; Transfer any `icicle-whole-candidate' property from
                                           ;; candidate to `icicle-current-input', so things that use
                                           ;; `icicle-candidates-alist' will work.
                                           (car icicle-completion-candidates)
                                         (if (string= "" icicle-current-input)
                                             (or (icicle-file-name-directory icicle-current-input)  "")
                                           (directory-file-name
                                            (icicle-abbreviate-or-expand-file-name
                                             (car icicle-completion-candidates)
                                             (icicle-file-name-directory icicle-current-input))))))
           (setq icicle-nb-of-other-cycle-candidates  0)
           (cond (;; Do not expand input in minibuffer - just show `*Completions*'.
                  (and (not no-display-p) ; Always expand for NO-DISPLAY-P.
                       (or (eq 0 icicle-expand-input-to-common-match) ; `never'
                           (and icicle-edit-update-p ; `explicit' means no auto-expansion.
                                (eq 1 icicle-expand-input-to-common-match)))) ; `explicit'
                  (when icicle-incremental-completion-p  (sit-for icicle-incremental-completion-delay))
                  (icicle-display-candidates-in-Completions))
                 (t                     ; Expand input to sole match in minibuffer.
                  (setq icicle-current-input
                        (if (not (icicle-file-name-input-p))
                            ;; Transfer any `icicle-whole-candidate' property from
                            ;; candidate to `icicle-current-input', so things that use
                            ;; `icicle-candidates-alist' will work.
                            (car icicle-completion-candidates)
                          (if (string= "" icicle-current-input)
                              (or (icicle-file-name-directory icicle-current-input)  "")
                            (directory-file-name
                             (icicle-abbreviate-or-expand-file-name
                              (car icicle-completion-candidates)
                              (icicle-file-name-directory icicle-current-input))))))
                  (unless (and icicle-edit-update-p
                               (or (not icicle-incremental-completion-p)
                                   (not (sit-for icicle-incremental-completion-delay))))
                    (icicle-clear-minibuffer)
                    (let ((cand  (car icicle-completion-candidates)))
                      (if (icicle-file-name-input-p)
                          (cond ((string= "" cand) ; This indicates an empty dir.
                                 (setq icicle-last-completion-candidate  icicle-current-input))
                                ((eq ?\/  (aref cand (1- (length cand))))
                                 ;; Add `/', so cycling expands dir.
                                 (setq icicle-current-input              (file-name-as-directory
                                                                          icicle-current-input)
                                       icicle-last-completion-candidate  icicle-current-input))
                                (t      ; Non-dir - use the candidate file.
                                 (setq icicle-last-completion-candidate  cand)))
                        (setq icicle-last-completion-candidate  cand)))
                    (let ((inserted  (if (and (icicle-file-name-input-p) insert-default-directory
                                              (or (not (member icicle-last-completion-candidate
                                                               icicle-extra-candidates))
                                                  icicle-extra-candidates-dir-insert-p))
                                         (icicle-abbreviate-or-expand-file-name
                                          icicle-last-completion-candidate
                                          (icicle-file-name-directory-w-default icicle-current-input))
                                       icicle-last-completion-candidate)))
                      (insert inserted)))
                  (save-selected-window (icicle-remove-Completions-window))
                  ;; Do not transform multi-completion here.  It should be done in the function that acts
                  ;; on the chosen completion candidate.  For a multi-command, that means it should be
                  ;; done in the action function.
                  ;; $$$$$$ (icicle-transform-sole-candidate)
                  icicle-current-input))
           (unless (boundp 'icicle-apropos-complete-and-exit-p)
             (icicle-highlight-complete-input)
             (cond ((and icicle-top-level-when-sole-completion-flag
                         (or (and (not (and (boundp 'icicle-ido-like-mode) icicle-ido-like-mode))
                                  (not icicle-files-ido-like-flag))
                             (and (not (icicle-file-name-input-p))
                                  (not icicle-abs-file-candidates))
                             (string= "" (car icicle-completion-candidates)) ; Empty directory
                             (not (eq ?\/  (aref (car icicle-completion-candidates)
                                                 (1- (length (car icicle-completion-candidates)))))))
                         (sit-for icicle-top-level-when-sole-completion-delay))
                    (set minibuffer-history-variable (cons (car icicle-completion-candidates)
                                                           (symbol-value minibuffer-history-variable)))
                    (icicle-condition-case-no-debug icicle-apropos-complete-1
                        (throw 'icicle-read-top
                          (if (and (icicle-file-name-input-p)  insert-default-directory
                                   (or (not (member (car icicle-completion-candidates)
                                                    icicle-extra-candidates))
                                       icicle-extra-candidates-dir-insert-p))
                              (expand-file-name (car icicle-completion-candidates))
                            (car icicle-completion-candidates)))
                      (no-catch (setq icicle-current-input  (car icicle-completion-candidates))
                                (icicle-retrieve-last-input)
                                icicle-current-input)
                      (error (message "%s" (error-message-string icicle-apropos-complete-1)))))
                   ((and icicle-edit-update-p  (not (eq no-display-p 'no-msg)))
                    (minibuffer-message (format "  [One apropos completion: %s]"
                                                (car icicle-completion-candidates)))
                    (setq mode-line-help  (car icicle-completion-candidates)))
                   ((not (eq no-display-p 'no-msg))
                    (minibuffer-message "  [Sole apropos completion]")
                    (setq mode-line-help  (car icicle-completion-candidates))))))

          ;; Multiple candidates.

          (;; Do not expand input in minibuffer - just show `*Completions*'.
           (and (not no-display-p)      ; Always expand for NO-DISPLAY-P.
                (or (eq 0 icicle-expand-input-to-common-match) ; `never'
                    (and icicle-edit-update-p ; No autoexpansion for `explicit', `sole-match', `prefix'
                         (memq icicle-expand-input-to-common-match '(1 2 3)))))
           (when icicle-incremental-completion-p  (sit-for icicle-incremental-completion-delay))
           (icicle-display-candidates-in-Completions))

          (t
           ;; Complete: expand input to match common prefix.
           (if icicle-edit-update-p
               (if (or (not icicle-incremental-completion-p)
                       (not (sit-for icicle-incremental-completion-delay)))
                   ;; Even though we do this unconditionally, we need to do it after the `sit-for',
                   ;; not before, because displaying causes Emacs to think that user input might be
                   ;; pending.
                   (icicle-display-candidates-in-Completions nil no-display-p)
                 (icicle-display-candidates-in-Completions nil no-display-p)
                 (let ((complete-input-sans-dir  (icicle-apropos-complete-2)))
                   (when complete-input-sans-dir  (setq mode-line-help  complete-input-sans-dir))))
             (let ((complete-input-sans-dir  (icicle-apropos-complete-2)))
               (when complete-input-sans-dir  (setq mode-line-help  complete-input-sans-dir)))
             (cond (;; Candidates visible.  If second `S-TAB', cycle, else update candidates.
                    (get-buffer-window "*Completions*" 0)
                    (if (and (or iac1-was-cycling-p  icicle-next-apropos-complete-cycles-p)
                             (icicle-get-safe icicle-last-completion-command
                                              'icicle-apropos-completing-command)
                             (symbolp last-command)
                             (or (get last-command 'icicle-apropos-completing-command)
                                 (get last-command 'icicle-action-command)
                                 ;; This is necessary because when this option is non-nil
                                 ;; `icicle-(prefix|apropos)-complete' is called from code,
                                 ;; so `last-command' has not been set to it.
                                 icicle-show-Completions-initially-flag))
                        ;; Second `S-TAB' in a row.  Cycle down.
                        (icicle-next-candidate 1 (if (icicle-file-name-input-p)
                                                     'icicle-file-name-apropos-candidates
                                                   'icicle-apropos-candidates)
                                               'regexp-p)
                      ;; User did something else (e.g. changed input).  (Possibly) update the display.
                      (icicle-display-candidates-in-Completions nil no-display-p)))
                   (t
                    (if (not (and (or iac1-was-cycling-p  icicle-next-apropos-complete-cycles-p)
                                  (icicle-get-safe icicle-last-completion-command
                                                   'icicle-apropos-completing-command)
                                  (symbolp last-command)
                                  (or (get last-command 'icicle-apropos-completing-command)
                                      (get last-command 'icicle-action-command))))
                        (icicle-display-candidates-in-Completions nil no-display-p)
                      ;; Second apropos complete.  If `S-TAB', it follows `C-M-S-TAB', so show window.
                      (unless no-display-p (icicle-display-candidates-in-Completions nil))
                      (icicle-next-candidate 1 (if (icicle-file-name-input-p)
                                                   'icicle-file-name-apropos-candidates
                                                 'icicle-apropos-candidates)
                                             'regexp-p)))))))
    (setq icicle-last-completion-command         (if no-display-p
                                                     'icicle-apropos-complete-no-display
                                                   'icicle-apropos-complete)
          ;; $$$$$$ Trying without the condition arg - just pass `t'
          ;; icicle-next-apropos-complete-cycles-p  (equal input-before-completion
          ;;                                               (icicle-input-from-minibuffer)))
          icicle-next-apropos-complete-cycles-p  t)
    (when mode-line-help (icicle-show-help-in-mode-line mode-line-help))
    icicle-completion-candidates))

(defun icicle-apropos-complete-2 ()
  "Replace minibuffer content with highlighted current input.
Call `icicle-highlight-initial-whitespace'.
If completing file name, set default dir based on current completion.

If input is complete and we are not in the process of exiting the
minibuffer, then call `icicle-highlight-complete-input'.  Return the
value of this condition: nil or non-nil."
  (icicle-clear-minibuffer)
  (insert icicle-current-input)         ; Update minibuffer.
  ;; Shouldn't need to do this if it is on `post-command-hook', but it seems we need to.
  (when (and (boundp '1on1-fit-minibuffer-frame-flag)  1on1-fit-minibuffer-frame-flag
             (require 'fit-frame nil t))
    (1on1-fit-minibuffer-frame))        ; In `oneonone.el'.
  (deactivate-mark)
  (icicle-highlight-initial-whitespace icicle-current-input)
  (let* ((input-sans-dir            (icicle-minibuf-input-sans-dir icicle-current-input))
         (complete-&-not-exiting-p  (and (member (icicle-upcase-if-ignore-case input-sans-dir)
                                                 (mapcar #'icicle-upcase-if-ignore-case
                                                         icicle-completion-candidates))
                                         (not (boundp 'icicle-apropos-complete-and-exit-p)))))
    (when complete-&-not-exiting-p (icicle-highlight-complete-input))
    (and complete-&-not-exiting-p  input-sans-dir)))

;; Not used anymore.
(defun icicle-transform-sole-candidate ()
  "Transform matching candidate according to `icicle-list-use-nth-parts'."
  (when (and icicle-list-use-nth-parts  icicle-current-input)
    ;; $$$$$$ (let ((newcand  (icicle-transform-multi-completion (car icicle-completion-candidates))))
    (let ((newcand  (icicle-transform-multi-completion icicle-current-input)))
      (icicle-clear-minibuffer)
      (insert newcand)
      (setq icicle-completion-candidates      (list newcand)
            icicle-last-completion-candidate  newcand))))

;;;###autoload (autoload 'icicle-switch-to-Completions-buf "icicles")
(defun icicle-switch-to-Completions-buf () ; Bound to `C-insert' in minibuffer.
  "Select the completion list window.
The cursor is placed on the first occurrence of the current minibuffer
content.  You can use \\<completion-list-mode-map>\
`\\[icicle-insert-completion]' to get back to the minibuffer.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-switch-to-Completions-buf]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let ((window     (get-buffer-window "*Completions*" 0))
        (search-fn  'search-forward))
    (unless window                      ; Make sure we have a completions window.
      (icicle-apropos-complete)
      (setq window     (get-buffer-window "*Completions*" 0)
            search-fn  're-search-forward)) ; Use regexp search: input is not yet complete.
    (when window
      (select-window window)
      (let ((case-fold-search
             ;; Don't bother to detect buffer completion and check `read-buffer-completion-ignore-case'.
             (if (and (icicle-file-name-input-p)
                      (boundp 'read-file-name-completion-ignore-case))
                 read-file-name-completion-ignore-case
               completion-ignore-case)))
        (goto-char (icicle-start-of-candidates-in-Completions))
        (cond (icicle-candidate-nb
               (icicle-move-to-next-completion icicle-candidate-nb 'NO-MINIBUFFER-FOLLOW-P))
              (t
               (let ((inp  (icicle-minibuf-input-sans-dir icicle-current-input)))
                 (when (and (icicle-get-safe icicle-last-completion-command
                                             'icicle-apropos-completing-command)
                            ;; $$ Previously allowed the -action's.
                            (not (icicle-get-safe last-command 'icicle-cycling-command)))
                   (setq search-fn  're-search-forward)) ; Use regexp search: input is not yet complete.
                 (while (and (not (eobp))
                             (save-restriction
                               (narrow-to-region (point)
                                                 (next-single-property-change (point) 'mouse-face
                                                                              nil (point-max)))
                               (not (funcall search-fn inp nil 'leave-at-end))))))))
        (unless (eobp)
          (unless icicle-candidate-nb (goto-char (match-beginning 0)))
          (let ((prop  (get-text-property (max (point-min) (1- (point))) 'mouse-face)))
            ;; If in a completion, move to the start of it.
            (when (and prop  (eq prop (get-text-property (point) 'mouse-face)))
              (goto-char (previous-single-property-change (point) 'mouse-face nil (point-min)))))
          (icicle-place-overlay
           (point) (next-single-property-change (point) 'mouse-face nil (point-max))
           'icicle-current-completion-candidate-overlay 'icicle-current-candidate-highlight
           100 (current-buffer)))))))

;;;###autoload (autoload 'icicle-insert-completion "icicles")
(defun icicle-insert-completion (&optional completion) ; Bound to `C-insert' in `*Completions*'.
  "Select the active minibuffer window.  Insert current completion.
The current candidate in `*Completions*' (under the cursor) is
inserted into the minibuffer as the current input.  You can use \\<minibuffer-local-completion-map>\
`\\[icicle-switch-to-Completions-buf]'
to switch to the `*Completions*' window.

You can use this command only from buffer `*Completions*' (`\\<completion-list-mode-map>\
\\[icicle-insert-completion]').

Non-interactively, optional arg COMPLETION is the completion to insert."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (when (active-minibuffer-window)
    (unwind-protect                     ; If no current completion, return to minibuffer anyway.
         (progn
           (setq completion                        (or completion
                                                       (icicle-current-completion-in-Completions))
                 icicle-last-completion-candidate  completion
                 icicle-candidate-nb               (icicle-nb-of-cand-at-Completions-pos (point)))
           (select-window (active-minibuffer-window))
           (with-current-buffer (window-buffer) ; Needed if `*Completions*' is redirected to minibuffer.
             (goto-char (icicle-minibuffer-prompt-end))
             (icicle-clear-minibuffer)
             (icicle-insert-cand-in-minibuffer completion
                                               (not (eq icicle-current-completion-mode 'prefix)))
             (setq icicle-last-input  icicle-current-input
                   icicle-cycling-p   t)
             (icicle-show-help-in-mode-line icicle-last-completion-candidate)))
      (select-window (active-minibuffer-window)))))

(defun icicle-current-completion-in-Completions ()
  "The completion candidate under the cursor in buffer `*Completions*'.
Return the name as a string."           ; See also `choose-completion' and `mouse-choose-completion'.
  (let ((buffer          completion-reference-buffer)
        (base-size       completion-base-size)
        (start-of-cands  (icicle-start-of-candidates-in-Completions))
        beg end)
    (when (and (not (eobp))  (get-text-property (point) 'mouse-face))
      (setq end  (point)
            beg  (1+ (point))))
    (when (and (> (point) start-of-cands)  (get-text-property (max (point-min) (1- (point))) 'mouse-face))
      (setq end  (max (point-min) (1- (point)))
            beg  (point)))
    (setq beg  (previous-single-property-change (or beg  (point)) 'mouse-face nil start-of-cands)
          end  (next-single-property-change (or end  (point)) 'mouse-face nil (point-max)))
    (unless beg (error "No completion here"))
    ;; $$$$ (buffer-substring-no-properties beg end)))
    (buffer-substring beg end)))

;;;###autoload (autoload 'icicle-switch-to/from-minibuffer "icicles")
(defun icicle-switch-to/from-minibuffer () ; Bound to `pause' in Icicle mode.
  "Switch to minibuffer or previous buffer, in other window.
If current buffer is the minibuffer, then switch to the buffer that
was previously current.  Otherwise, switch to the minibuffer."
  (interactive)
  (unless (active-minibuffer-window) (error "Minibuffer is not active"))
  (if (eq (selected-window) (active-minibuffer-window))
      (switch-to-buffer-other-window icicle-pre-minibuffer-buffer)
    (select-window (active-minibuffer-window))))


;; Replaces `previous-completion' (defined in `simple.el').
;;
;;;###autoload (autoload 'icicle-move-to-previous-completion "icicles")
(defun icicle-move-to-previous-completion (n) ; Bound to `left', `S-TAB' in `*Completions*'.
  "Move to the previous item in the completion list.

You can use this command only from buffer `*Completions*' (`\\<completion-list-mode-map>\
\\[icicle-move-to-previous-completion]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (setq n  (or n  0))
  (icicle-move-to-next-completion (- n)))


;; Replaces `next-completion' (defined in `simple.el').
;; This is similar, except:
;; 1. This highlights the current candidate.
;; 2. This wraps around from first to last and last to first.
;; 3. Properly handles completions laid out vertically.
;;
;;;###autoload (autoload 'icicle-move-to-next-completion "icicles")
(defun icicle-move-to-next-completion (n ; Bound to `right', `TAB' in `*Completions*'.
                                       &optional no-minibuffer-follow-p)
  "Move to the next item in the completion list.
With prefix argument N, move N items (negative N means move backward).
Optional second argument, if non-nil, means do not copy the completion
back to the minibuffer.

You can use this command only from buffer `*Completions*' (`\\<completion-list-mode-map>\
\\[icicle-move-to-next-completion]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (setq n  (or n  0))
  (when (eq icicle-completions-format 'vertical)
    (let* ((cols      (icicle-nb-Completions-cols))
           (nb-cands  (length icicle-completion-candidates))
           (rows      (/ nb-cands cols)))
      (unless (zerop (% nb-cands cols)) (setq rows  (1+ rows)))
      (setq n  (icicle-row-wise-cand-nb n nb-cands rows cols))))
  (let ((beg  (icicle-start-of-candidates-in-Completions))
        (end  (point-max)))
 
    ;; Forward: n > 0.
    (while (and (> n 0)  (not (eobp)))
      (when (get-text-property (point) 'mouse-face) ; If in a candidate, move to its end.
        (goto-char (next-single-property-change (point) 'mouse-face nil end)))
      (unless (get-text-property (point) 'mouse-face) ; Move to start of next candidate.
        (goto-char (or (next-single-property-change (point) 'mouse-face)  beg))) ; Wrap back to first.
      (setq n  (1- n)))

    ;; Backward: n < 0.
    (while (and (< n 0)  (>= (count-lines 1 (point)) (if icicle-show-Completions-help-flag 2 0)))
      (let ((prop  (get-text-property (max (point-min) (1- (point))) 'mouse-face)))
        (when (and prop  (eq prop (get-text-property (point) 'mouse-face))) ; If in cand, move to start.
          (goto-char (previous-single-property-change (point) 'mouse-face nil beg))))
      (unless (or (< (count-lines 1 (point)) ; Move to end of previous candidate.
                     (if icicle-show-Completions-help-flag 2 0))
                  (and (not (bobp))  (get-text-property (1- (point)) 'mouse-face)))
        (goto-char (or (previous-single-property-change (point) 'mouse-face)
                       ;; Wrap back to end of last candidate.  (Back over final space with no mouse-face.)
                       (1- end))))

      ;; Move to the start of that candidate.
      (goto-char (previous-single-property-change (point) 'mouse-face nil beg))
      (setq n  (1+ n)))

    (icicle-place-overlay
     (point) (next-single-property-change (point) 'mouse-face nil end)
     'icicle-current-completion-candidate-overlay 'icicle-current-candidate-highlight
     100 (current-buffer)))
  (unless no-minibuffer-follow-p (save-excursion (save-window-excursion (icicle-insert-completion)))))

;;;###autoload (autoload 'icicle-previous-line "icicles")
(defun icicle-previous-line ()          ; Bound to `up' in `*Completions*'.
  "Move up a line, in `*Completions*' buffer.  Wrap around first to last.
You can use this command only from buffer `*Completions*' (`\\<completion-list-mode-map>\
\\[icicle-previous-line]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (let ((opoint          (point))
        (curr-col        1)
        (next-line-cols  1)
        (eol             (line-end-position)))
    (save-excursion
      (beginning-of-line)
      (while (and (< (point) opoint)  (re-search-forward "[^ ] +" eol t))
        (setq curr-col  (1+ curr-col))))
    (cond ((and (not icicle-show-Completions-help-flag)
                (<= (count-lines (icicle-start-of-candidates-in-Completions) (point)) (if (bolp) 0 1)))
           (goto-char (point-max)) (beginning-of-line)) ; Wrap around
          (t
           (forward-line -1)
           (when (and icicle-show-Completions-help-flag
                      (< (point) (icicle-start-of-candidates-in-Completions)))
             (goto-char (point-max)) (beginning-of-line) ; Wrap around
             (unless (get-text-property (point) 'mouse-face) (forward-line -1))))) ; eobp, extra newline.
    (let ((eol  (line-end-position)))
      (save-excursion
        (beginning-of-line)
        (while (re-search-forward "[^ ] +[^ ]" eol t) (setq next-line-cols  (1+ next-line-cols)))))
    (icicle-move-to-next-completion
     (cond ((eq 1 curr-col)              -1)
           ((> curr-col next-line-cols)  (1- next-line-cols))
           (t                            (1- curr-col))))))

;;;###autoload (autoload 'icicle-next-line "icicles")
(defun icicle-next-line ()              ; Bound to `down' in `*Completions*'.
  "Move down a line, in `*Completions*' buffer.  Wrap around last to first.
You can use this command only from buffer `*Completions*' (`\\<completion-list-mode-map>\
\\[icicle-next-line]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (let ((opoint          (point))
        (curr-col        1)
        (next-line-cols  1)
        (eol             (line-end-position)))
    (save-excursion
      (beginning-of-line)
      (while (and (< (point) opoint)  (re-search-forward "[^ ] +" eol t))
        (setq curr-col  (1+ curr-col))))
    (forward-line 1)
    (when (eobp) (goto-char (icicle-start-of-candidates-in-Completions))) ; Wrap around
    (let ((eol  (line-end-position)))
      (save-excursion
        (beginning-of-line)
        (while (re-search-forward "[^ ] +[^ ]" eol t) (setq next-line-cols  (1+ next-line-cols)))))
    (icicle-move-to-next-completion
     (cond ((eq 1 curr-col)              1)
           ((> curr-col next-line-cols)  (1- next-line-cols))
           (t                            (1- curr-col))))))

;; Same as `end-of-line+' in `misc-cmds.el'.
;;
;;;###autoload (autoload 'icicle-end-of-line+ "icicles")
(defun icicle-end-of-line+ (&optional n) ; Bound to `C-e' in minibuffer and in `*Completions*'.
  "Move cursor to end of current line or end of next line if repeated.
This is similar to `end-of-line', but:
  If called interactively with no prefix arg:
     If the previous command was also `end-of-line+', then move to the
     end of the next line.  Else, move to the end of the current line.
  Otherwise, move to the end of the Nth next line (Nth previous line
     if N<0).  Command `end-of-line', by contrast, moves to the end of
     the (N-1)th next line."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 0)))
  (unless n (setq n  0))                ; non-interactive with no arg
  (if (and (eq this-command last-command)  (not current-prefix-arg))
      (forward-line 1)
    (forward-line n))
  (let ((inhibit-field-text-motion  t)) ; Emacs 22+, so we get past the end of the prompt field.
    (end-of-line)))

;; Same as `beginning-of-line+' in `misc-cmds.el'.
;;
;;;###autoload (autoload 'icicle-beginning-of-line+ "icicles")
(defun icicle-beginning-of-line+ (&optional n) ; Bound to `C-a' in minibuffer and in `*Completions*'.
  "Move cursor to beginning of current line or next line if repeated.
This is the similar to `beginning-of-line', but:
1. With arg N, the direction is the opposite: this command moves
   backward, not forward, N lines.
2. If called interactively with no prefix arg:
      If the previous command was also `beginning-of-line+', then move
      to the beginning of the previous line.  Else, move to the
      beginning of the current line.
   Otherwise, move to the beginning of the Nth previous line (Nth next
      line if N<0).  Command `beginning-of-line', by contrast, moves to
      the beginning of the (N-1)th next line."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 0)))
  (unless n (setq n  0))                ; non-interactive with no arg
  (if (and (eq this-command last-command)  (not current-prefix-arg))
      (forward-line -1)
    (forward-line (- n)))
  (when (bobp) (goto-char (icicle-minibuffer-prompt-end))))

;; Same as `resolve-file-name' in `misc-cmds.el'.
;;
;;;###autoload (autoload 'icicle-resolve-file-name "icicles")
(defun icicle-resolve-file-name (bounds &optional killp) ; Bound to `C-x C-f' in minibuffer.
  "Replace the file name at/near point by its absolute, true file name.
If the region is active, replace its content instead, treating it as a
file name.

If library `thingatpt+.el' is available then use the file name
*nearest* point.  Otherwise, use the file name *at* point.

With a prefix arg, add both the original file name and the true name
to the kill ring.  Otherwise, add neither to the kill ring.  (If the
region was active then its content was already added to the ring.)"
  (interactive
   (let* ((regionp   (and transient-mark-mode  mark-active))
          (thg+bnds  (and (not regionp)
                          (require 'thingatpt+ nil t)
                          (tap-define-aliases-wo-prefix) ; Dispense with `tap-' prefix.
                          (tap-put-thing-at-point-props)
                          (thing-nearest-point-with-bounds 'filename)))
          (bnds      (if regionp
                         (cons (region-beginning) (region-end))
                       (if thg+bnds
                           (cdr thg+bnds)
                         (icicle-bounds-of-thing-at-point 'filename))))
          (fname     (if bnds
                         (buffer-substring (car bnds) (cdr bnds))
                       (message "No file name at point"))))
     (list bnds current-prefix-arg)))
  (when bounds
    (let* ((file       (buffer-substring (car bounds) (cdr bounds)))
           (absfile    (expand-file-name (buffer-substring (car bounds) (cdr bounds))))
           (dir        (or (file-name-directory absfile)  default-directory))
           (true-dir   (file-truename dir))
           (relfile    (file-name-nondirectory absfile))
           (true-file  (concat true-dir relfile)))
      (unless (equal file true-file)
        (cond (killp
               (if (and transient-mark-mode  mark-active)
                   (delete-region (car bounds) (cdr bounds)) ; Don't add it twice.
                 (kill-region (car bounds) (cdr bounds)))
               (insert (kill-new true-file)))
              (t
               (delete-region (car bounds) (cdr bounds))
               (insert true-file)))))))


(put 'icicle-all-candidates-action 'icicle-action-command t)

;;;###autoload (autoload 'icicle-all-candidates-action "icicles")
(defun icicle-all-candidates-action ()  ; Bound to `C-!' in minibuffer.
  "Take action on each completion candidate, in turn.
Apply `icicle-candidate-action-fn' successively to each saved
completion candidate (if any) or each candidate that matches the
current input (a regular expression).  The candidates that were not
successfully acted upon are listed in buffer *Help*.

If there are saved completion candidates, then they are acted on;
if not, then all current matching candidates are acted on.

If `icicle-candidate-action-fn' is nil but
`icicle-all-candidates-list-action-fn' is not, then apply the latter
to the list of candidates as a whole, instead.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-all-candidates-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (unless (or icicle-all-candidates-list-action-fn  icicle-candidate-action-fn)
    (error "No action defined"))
  (if icicle-candidate-action-fn
      (icicle-all-candidates-action-1 icicle-candidate-action-fn nil)
    (icicle-all-candidates-action-1 icicle-all-candidates-list-action-fn 'LISTP)))


(put 'icicle-all-candidates-alt-action 'icicle-action-command t)

;;;###autoload (autoload 'icicle-all-candidates-alt-action "icicles")
(defun icicle-all-candidates-alt-action () ; Bound to `C-|' in minibuffer.
  "Take alternative action on each completion candidate, in turn.
Apply `icicle-candidate-alt-action-fn' successively to each saved
completion candidate (if any) or each candidate that matches the
current input (a regular expression).  The candidates that were not
successfully acted upon are listed in buffer *Help*.

If there are saved completion candidates, then they are acted on; if
not, then all current matching candidates are acted on.

If `icicle-candidate-alt-action-fn' is nil but
`icicle-all-candidates-list-alt-action-fn' is not, then apply the
latter to the list of candidates as a whole, instead.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-all-candidates-alt-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (unless (or icicle-all-candidates-list-alt-action-fn  icicle-candidate-alt-action-fn)
    (error "No alternative action defined"))
  (if icicle-candidate-alt-action-fn
      (icicle-all-candidates-action-1 icicle-candidate-alt-action-fn nil 'ALTP)
    (icicle-all-candidates-action-1 icicle-all-candidates-list-alt-action-fn 'LISTP)))


(put 'icicle-all-candidates-list-action 'icicle-action-command t)

;;;###autoload (autoload 'icicle-all-candidates-list-action "icicles")
(defun icicle-all-candidates-list-action () ; Bound to `M-!' in minibuffer.
  "Take action on the list of all completion candidates.
Apply `icicle-all-candidates-list-action-fn' to the list of saved
completion candidates or the list of candidates that match the current
input (a regular expression).

If there are saved completion candidates, then they are acted on; if
not, then all current matching candidates are acted on.

If `icicle-all-candidates-list-action-fn' is nil but
`icicle-candidate-action-fn' is not, then apply the latter to each
matching candidate in turn, and print the candidates that were not
successfully acted upon in buffer *Help*.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-all-candidates-list-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (unless (or icicle-all-candidates-list-action-fn  icicle-candidate-action-fn)
    (error "No action defined"))
  (if icicle-all-candidates-list-action-fn
      (icicle-all-candidates-action-1 icicle-all-candidates-list-action-fn 'LISTP)
    (icicle-all-candidates-action-1 icicle-candidate-action-fn nil)))


(put 'icicle-all-candidates-list-alt-action 'icicle-action-command t)

;;;###autoload (autoload 'icicle-all-candidates-list-alt-action "icicles")
(defun icicle-all-candidates-list-alt-action () ; Bound to `M-|' in minibuffer.
  "Take alternative action on the list of all completion candidates.
Apply `icicle-all-candidates-list-alt-action-fn' to the list of saved
completion candidates or the list of completion candidates that match
the current input (a regular expression).

If there are saved completion candidates, then they are acted on;
if not, then all current matching candidates are acted on.

If `icicle-all-candidates-list-alt-action-fn' is nil but
`icicle-candidate-alt-action-fn' is not, then apply the latter to each
matching candidate in turn, and print the candidates that were not
successfully acted upon in buffer *Help*.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-all-candidates-list-alt-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (unless (or icicle-all-candidates-list-alt-action-fn  icicle-candidate-alt-action-fn)
    (error "No alternative action defined"))
  (unless icicle-completion-candidates
    (error "No completion candidates.  Did you use `TAB' or `S-TAB'?"))
  (if icicle-all-candidates-list-alt-action-fn
      (icicle-all-candidates-action-1 icicle-all-candidates-list-alt-action-fn 'LISTP)
    (icicle-all-candidates-action-1 icicle-candidate-alt-action-fn nil 'ALTP)))

(defun icicle-all-candidates-action-1 (fn-var listp &optional altp)
  "Helper function for `icicle-all-candidates(-alt)-action'.
ALTP is used only if LISTP is nil.
ALTP is passed to `icicle-candidate-action-1'."
  (let* ((local-saved
          (catch 'i-a-c-a-1
            (dolist (cand  icicle-saved-completion-candidates  icicle-saved-completion-candidates)
              (unless (member cand icicle-completion-candidates) (throw 'i-a-c-a-1 nil)))))
         (candidates                      (or local-saved  icicle-completion-candidates))
         (failures                        nil)
         (icicle-minibuffer-message-ok-p  listp) ; Avoid `icicle-msg-maybe-in-minibuffer' delays.
         (icicle-help-in-mode-line-delay  0) ; Avoid delays for individual candidate help.
         (icicle-all-candidates-action    t))
    (when local-saved (setq icicle-completion-candidates  local-saved))
    (if listp
        (funcall fn-var candidates)
      (while candidates
        (let ((error-msg  (icicle-condition-case-no-debug act-on-each
                              (icicle-candidate-action-1 fn-var altp (car candidates))
                            (error (error-message-string act-on-each)))))
          (when error-msg (setq failures  (cons (cons (car candidates) error-msg) failures)))
          (setq candidates  (cdr candidates))))
      (when failures
        (with-output-to-temp-buffer "*Help*"
          (princ "Action failures:")(terpri)(terpri)
          (mapcar (lambda (entry)
                    (princ (car entry)) (princ ":") (terpri) (princ "  ")
                    (princ (cdr entry)) (terpri))
                  failures))))))
;; $$$$$$ (icicle-abort-recursive-edit))


(put 'icicle-candidate-action 'icicle-action-command t)

;;;###autoload (autoload 'icicle-candidate-action "icicles")
(defun icicle-candidate-action ()       ; Bound to `C-return' in minibuffer.
  "Take action on the current minibuffer-completion candidate.
If `icicle-candidate-action-fn' is non-nil, it is a function to apply
to the current candidate, to perform the action.

If no action is available in the current context, help on the
candidate is shown - see `icicle-help-on-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-candidate-action-1 icicle-candidate-action-fn))


(put 'icicle-candidate-alt-action 'icicle-action-command t)

;;;###autoload (autoload 'icicle-candidate-alt-action "icicles")
(defun icicle-candidate-alt-action ()   ; Bound to `C-S-return' in minibuffer.
  "Take alternative action on the current completion candidate.
If `icicle-candidate-alt-action-fn' is non-nil, it is a
function to apply to the current candidate, to perform the action.

For many Icicles commands, if `icicle-candidate-alt-action-fn' is nil,
you are prompted to choose an alternative action, using completion.

In any case, any alternative action defined for the current context by
user option `icicle-alternative-actions-alist' always overrides
`icicle-candidate-alt-action'.  That is, if
`icicle-alternative-actions-alist' says to use function `foo', then
Icicles uses `foo' as the alternative action, regardless of the value
of `icicle-candidate-alt-action'.

If no alternative action is available in the current context, help on
the candidate is shown - see `icicle-help-on-candidate'.  

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-alt-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let ((alt-fn  (or (cdr (assq icicle-cmd-reading-input icicle-alternative-actions-alist))
                     icicle-candidate-alt-action-fn)))
    (icicle-candidate-action-1 alt-fn 'alternative-p)))

(defun icicle-candidate-action-1 (fn-var &optional altp cand)
  "Helper function for `icicle-candidate(-alt)-action'.
FN-VAR is an Icicles action function or alternative action function.
Optional arg ALTP non-nil means FN-VAR is alternative action function.
Optional arg CAND non-nil means it is the candidate to act on."
  (when cand (setq icicle-last-completion-candidate  cand))
  (cond ((not fn-var) (icicle-help-on-candidate cand)) ; It doesn't `icicle-raise-Completions-frame'.
        ((icicle-require-match-p)
         ;; If no last candidate, then reset to first candidate matching input.
         (unless (stringp icicle-last-completion-candidate)
           (setq icicle-last-completion-candidate  icicle-current-input
                 last-command                      (if altp
                                                       'icicle-candidate-alt-action
                                                     'icicle-candidate-action))
           (let ((icicle-help-in-mode-line-delay  0)) ; Avoid delay for candidate help.
             (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                          'icicle-prefix-candidates
                                        'icicle-apropos-candidates)
                                    (not (eq icicle-current-completion-mode 'prefix)))))

         ;; NOTE: We no longer save and restore these things here.
         ;; We purposely allow an action function to modify these for subsequent actions.
         ;; If you need to save and restore these for a particular action function you define,
         ;; then you must do so in the action function itself.  This might be the case, for instance,
         ;; if your action function does its own completion (e.g. calls `completing-read'), that is, if
         ;; it uses a recursive minibuffer.  (But do not save and restore if you want the side effect.)
         (let (;; (icicle-candidate-nb               icicle-candidate-nb)              ; $$$$$$
               ;; (icicle-last-completion-candidate  icicle-last-completion-candidate) ; $$$$$$
               ;; (icicle-completion-candidates  icicle-completion-candidates)         ; $$$$$$
               )
           (when icicle-completion-candidates (funcall fn-var icicle-last-completion-candidate)))
         (when (or icicle-use-candidates-only-once-flag
                   (and altp  icicle-use-candidates-only-once-alt-p))
           (icicle-remove-candidate-display-others 'all))
         (icicle-raise-Completions-frame))
        (t
         (let ((icicle-last-input  (or cand  (icicle-input-from-minibuffer))))
           ;; NOTE: We no longer save and restore these things here.
           ;; We purposely allow an action function to modify these for subsequent actions.
           ;; If you need to save and restore these for a particular action function you define,
           ;; then you must do so in the action function itself.  This might be the case, for instance,
           ;; if your action function does its own completion (e.g. calls `completing-read'), that is,
           ;; uses a recursive minibuffer.  (But do not save and restore if you want the side effect.)
           (let (;; (icicle-candidate-nb               icicle-candidate-nb)              ; $$$$$$
                 ;; (icicle-last-completion-candidate  icicle-last-completion-candidate) ; $$$$$$
                 ;; (icicle-completion-candidates      icicle-completion-candidates)     ; $$$$$$
                 )
             (funcall fn-var icicle-last-input))
           (when (and (or icicle-use-candidates-only-once-flag
                          (and altp  icicle-use-candidates-only-once-alt-p))
                      (equal icicle-last-input
                             (if (icicle-file-name-input-p)
                                 (expand-file-name icicle-last-completion-candidate
                                                   (and icicle-last-input ; User did not use `(S-)TAB'.
                                                        (icicle-file-name-directory icicle-last-input)))
                               icicle-last-completion-candidate)))
             (icicle-remove-candidate-display-others 'all))
           (icicle-raise-Completions-frame)))))


;; Bound to `C-down-mouse-2' (`C-mouse-2') in `*Completions*'.
(put 'icicle-mouse-candidate-action 'icicle-action-command t)

;;;###autoload (autoload 'icicle-mouse-candidate-action "icicles")
(defun icicle-mouse-candidate-action (event) ; `C-mouse-2'
  "Take action on the completion candidate clicked by `mouse-2'.
If `icicle-candidate-action-fn' is non-nil, it is a function to apply
to the clicked candidate, to perform the action.

If `icicle-candidate-action-fn' is nil, the default action is
performed: display help on the candidate - see
`icicle-help-on-candidate'."
  (interactive "e")
  (icicle-mouse-candidate-action-1 event icicle-candidate-action-fn))


; Bound to `C-S-down-mouse-2' (`C-S-mouse-2') in `*Completions*'.
(put 'icicle-mouse-candidate-alt-action 'icicle-action-command t)

;;;###autoload (autoload 'icicle-mouse-candidate-alt-action "icicles")
(defun icicle-mouse-candidate-alt-action (event) ; `C-S-mouse-2'
  "Take alternative action on the candidate clicked by `mouse-2'.
If `icicle-candidate-alt-action-fn' is non-nil, it is a
function to apply to the clicked candidate, to perform the action.

If `icicle-candidate-action-fn' is nil, the default action is
performed: display help on the candidate - see
`icicle-help-on-candidate'."
  (interactive "e")
  (icicle-mouse-candidate-action-1 event icicle-candidate-alt-action-fn))

(defun icicle-mouse-candidate-action-1 (event fn-var)
  "Helper function for `icicle-mouse-candidate(-alt)-action'."
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((posn-buf  (window-buffer (posn-window (event-start event))))
        (posn-pt   (posn-point (event-start event)))
        (posn-col  (car (posn-col-row (event-start event))))
        (posn-row  (cdr (posn-col-row (event-start event))))
        choice)
    (read-event)                        ; Swallow mouse up event.
    (with-current-buffer posn-buf
      (save-excursion
        (goto-char posn-pt)
        (let (beg end)
          (when (and (not (eobp))  (get-text-property (point) 'mouse-face))
            (setq end  (point)
                  beg  (1+ (point))))
          (unless beg (error "No completion here"))
          (setq beg  (previous-single-property-change beg 'mouse-face)
                end  (or (next-single-property-change end 'mouse-face)  (point-max)))
          (setq choice  (buffer-substring beg end))
          (remove-text-properties 0 (length choice) '(mouse-face nil) choice))))
    (let ((icicle-last-input  choice))
      (setq icicle-candidate-nb  (icicle-nb-of-cand-at-Completions-pos posn-pt))
      (save-window-excursion
        (select-window (active-minibuffer-window))
        (delete-region (icicle-minibuffer-prompt-end) (point-max))
        (insert choice))
      (if (not fn-var)
          (with-current-buffer (or icicle-orig-buff  posn-buf)
            (icicle-help-on-candidate)) ; Does not `icicle-raise-Completions-frame'.
        ;; NOTE: We no longer save and restore these things here.
        ;; We purposely allow an action function to modify these for subsequent actions.
        ;; If you need to save and restore these for a particular action function you define,
        ;; then you must do so in the action function itself.  This might be the case, for instance,
        ;; if your action function does its own completion (e.g. calls `completing-read'), that is, if
        ;; it uses a recursive minibuffer.  (But do not save and restore if you want the side effect.)
        (let (;; (icicle-candidate-nb               icicle-candidate-nb)              ; $$$$$$
              ;; (icicle-last-completion-candidate  icicle-last-completion-candidate) ; $$$$$$
              ;; (icicle-completion-candidates      icicle-completion-candidates)     ; $$$$$$
              )
          (funcall fn-var icicle-last-input))
        (when (and icicle-use-candidates-only-once-flag
                   (equal icicle-last-input
                          (if (icicle-file-name-input-p)
                              (expand-file-name icicle-last-completion-candidate
                                                (and icicle-last-input ; User did not use `(S-)TAB'.
                                                     (icicle-file-name-directory icicle-last-input)))
                            icicle-last-completion-candidate)))
          (icicle-remove-candidate-display-others 'all))
        (icicle-raise-Completions-frame posn-col posn-row)))))


;; $$$$$ ??? (put 'icicle-remove-candidate 'icicle-action-command t)
;;
;;;###autoload (autoload 'icicle-remove-candidate "icicles")
(defun icicle-remove-candidate ()       ; Bound to `delete' in minibuffer during completion.
  "Remove current completion candidate from the set of candidates.
This has no effect on the object, if any, represented by the
candidate; in particular, that object is not deleted.

Note: For Emacs versions prior to 22, this does not really remove a
file-name candidate as a possible candidate.  If you use \\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete] or \\[icicle-apropos-complete],
it will reappear as a possible candidate.

You can use this command only from the minibuffer (`\\[icicle-remove-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-remove-candidate-display-others))


;; $$$$$ ??? (put 'icicle-mouse-remove-candidate 'icicle-action-command t)
;;
;;;###autoload (autoload 'icicle-mouse-remove-candidate "icicles")
(defun icicle-mouse-remove-candidate (event) ; Bound to `S-mouse-2' in `*Completions*'.
  "Remove clicked completion candidate from the set of candidates.
This has no effect on the object, if any, represented by the
candidate; in particular, that object is not deleted.

See `icicle-remove-candidate' for more information."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((posn-buf  (window-buffer (posn-window (event-start event))))
        (posn-pt   (posn-point (event-start event)))
        beg end)
    (read-event)                        ; Swallow mouse up event.
    (with-current-buffer posn-buf
      (save-excursion
        (goto-char posn-pt)
        (when (and (not (eobp))  (get-text-property (point) 'mouse-face))
          (setq end  (point)
                beg  (1+ (point))))
        (unless beg (error "No completion here"))
        (setq beg  (previous-single-property-change beg 'mouse-face)
              end  (or (next-single-property-change end 'mouse-face)  (point-max)))
        (setq icicle-candidate-nb               (icicle-nb-of-cand-at-Completions-pos posn-pt)
              icicle-last-completion-candidate  (buffer-substring beg end)))))
  (icicle-remove-candidate-display-others))

(defun icicle-remove-candidate-display-others (&optional allp)
  "Remove current completion candidate from list of possible candidates.
Redisplay `*Completions*', unless there is only one candidate left.
Non-nil optional argument ALLP means remove all occurrences of the
current candidate.  Otherwise (nil) means remove only the current
occurrence."
  (unless (stringp icicle-last-completion-candidate)
    (setq icicle-last-completion-candidate  icicle-current-input
          last-command                      'icicle-delete-candidate-object)
    (let ((icicle-help-in-mode-line-delay  0)) ; Avoid delay for candidate help.
      (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                   'icicle-prefix-candidates
                                 'icicle-apropos-candidates)
                             (not (eq icicle-current-completion-mode 'prefix)))))
  (let ((maybe-mct-cand  (cond ((consp minibuffer-completion-table)
                                (icicle-mctized-display-candidate icicle-last-completion-candidate))
                               ((arrayp minibuffer-completion-table)
                                (intern icicle-last-completion-candidate))
                               (t
                                icicle-last-completion-candidate))))
    (icicle-remove-cand-from-lists icicle-last-completion-candidate maybe-mct-cand allp))
  (icicle-update-and-next))


(put 'icicle-delete-candidate-object 'icicle-action-command t)

;;;###autoload (autoload 'icicle-delete-candidate-object "icicles")
(defun icicle-delete-candidate-object (&optional allp) ; Bound to `S-delete' in minibuffer.
  "Delete the object named by the current completion candidate.
With a prefix argument, delete *ALL* objects named by the current set
of candidates, after confirmation.

Do nothing if `icicle-deletion-action-flag' is nil.

Otherwise:

* If the value of variable `icicle-delete-candidate-object' is a
  function, then apply it to the current completion candidate.  This
  should delete some object named by the completion candidate.

* If `icicle-delete-candidate-object' is not a function, then it
  should be a symbol bound to an alist.  In this case, invoke
  `icicle-delete-candidate-object' to delete the object named by the
  current completion candidate from that alist.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-delete-candidate-object]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (when icicle-deletion-action-flag
    (if (null icicle-completion-candidates)
        (message "Nothing to delete - use `S-TAB', `TAB', or a cycle key")
      (if allp
          (if (not (let ((icicle-completion-candidates  icicle-completion-candidates))
                     (yes-or-no-p "Are you SURE you want to DELETE ALL of the matching objects? ")))
              (message "OK, nothing deleted")
            (dolist (cand icicle-completion-candidates) (icicle-delete-candidate-object-1 cand t))
            (icicle-erase-minibuffer))
        ;; If no last candidate, then reset to first candidate matching input.
        (unless (stringp icicle-last-completion-candidate)
          (setq icicle-last-completion-candidate  icicle-current-input
                last-command                      'icicle-delete-candidate-object)
          (let ((icicle-help-in-mode-line-delay  0)) ; Avoid delay for candidate help.
            (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                         'icicle-prefix-candidates
                                       'icicle-apropos-candidates)
                                   (not (eq icicle-current-completion-mode 'prefix)))))
        (icicle-delete-candidate-object-1 icicle-last-completion-candidate)))))

(defun icicle-delete-candidate-object-1 (cand &optional no-display-p)
  "Helper function for `icicle-delete-candidate-object'.
Delete object named CAND.
Optional arg NO-DISPLAY-P non-nil means don't update `*Completions*'."
  (let ((display-cand  cand)            ; Use local vars: values might change.
        (maybe-mct-cand
         (cond ((consp minibuffer-completion-table) (icicle-mctized-display-candidate cand))
               ((arrayp minibuffer-completion-table) (intern cand))
               (t cand))))
    (save-selected-window
      (let ((icicle-completion-candidates  icicle-completion-candidates)) ; In case recursive minibuf.
        (if (functionp icicle-delete-candidate-object)
            (funcall icicle-delete-candidate-object cand)
          (icicle-delete-current-candidate-object cand))))
    (icicle-remove-cand-from-lists display-cand maybe-mct-cand nil) ; Use local vars.
    (unless no-display-p (message "Deleted object named: `%s'" display-cand) (sit-for 1.0)))
  (unless no-display-p (icicle-update-and-next))
  (select-window (minibuffer-window))
  (select-frame-set-input-focus (selected-frame)))

(defun icicle-delete-current-candidate-object (&optional cand)
  "Delete the object(s) corresponding to the current completion candidate.
The value of `icicle-delete-candidate-object' must be a symbol
\(variable) that is bound to a list of completion-candidate objects.

The entries in the list must be completion candidates for the current
call to `completing-read', but the list itself need not be the
COLLECTION argument to `completing-read'.  For example, the list might
be a list of symbols, and the COLLECTION argument might be an obarray
that contains those symbols.

The list can be an alist, a list of strings, or a list of symbols.
Delete, from this list, the objects that correspond to the current
completion candidate.  If the variable is also a user option, then
save the option, after deleting the candidate object.

The full candidate object is what is deleted.  If the list contains
multiple identical objects that correspond to the current completion
candidate, they are all deleted."
  (setq cand  (or cand  icicle-last-completion-candidate))
  (let ((val  (and (symbolp icicle-delete-candidate-object)
                   (symbol-value icicle-delete-candidate-object))))
    ;; The message could more accurately say "Value of `icicle-delete-candidate-object' must be
    ;; a symbol bound to a list", but this makes more sense.
    (unless (and val  (consp val)) (error "Cannot delete candidate objects now"))
    (set icicle-delete-candidate-object ; Update the variable.
         (cond ((or icicle-whole-candidate-as-text-prop-p  icicle-candidates-alist)
                (delete (funcall icicle-get-alist-candidate-function cand) val))
               ((consp (car val))
                (icicle-assoc-delete-all cand val))
               ((stringp (car val)) (delete cand val))
               ((symbolp (car val)) (delete (intern cand) val))
               (t (error "Entry in list value of `icicle-delete-candidate-object' is \
not a cons, string, or symbol")))))
  (when (user-variable-p icicle-delete-candidate-object) ; Save the new user-option value.
    (funcall icicle-customize-save-variable-function
             icicle-delete-candidate-object
             (symbol-value icicle-delete-candidate-object))))

(defun icicle-remove-cand-from-lists (disp-cand mct-cand allp)
  "Delete first occurence or all occurences of candidate.
The appropriate form of the candidate is removed from each of these:
 `icicle-candidates-alist'
 `icicle-completion-candidates'
 `minibuffer-completion-table' (if it is an alist)
DISP-CAND is the display form of the candidate to delete.
MCT-CAND is the MCT alist candidate that corresponds to DISP-CAND.
If any of these conditions is true, remove all occurrences of CAND:
 * ALLP is non-nil
 * `icicle-transform-function' is `icicle-remove-duplicates'
 * `icicle-transform-function' is `icicle-remove-dups-if-extras'
   and `icicle-extra-candidates' is non-nil"
  (setq allp  (or allp
                  (eq icicle-transform-function 'icicle-remove-duplicates)
                  (and icicle-extra-candidates
                       (eq icicle-transform-function 'icicle-remove-dups-if-extras))))
  (when icicle-candidates-alist
    (setq icicle-candidates-alist
          (if allp
              (icicle-assoc-delete-all disp-cand icicle-candidates-alist)
            (delete (funcall icicle-get-alist-candidate-function disp-cand) icicle-candidates-alist))))
  (when (consp icicle-completion-candidates)
    (setq icicle-completion-candidates
          (if allp                      ; Delete only the first occurrence, or all if ALLP.
              (delete disp-cand icicle-completion-candidates)
            (icicle-delete-count disp-cand icicle-completion-candidates 1))))

  ;; Update `minibuffer-completion-predicate' or `read-file-name-predicate'
  ;; to effectively remove this candidate.
  ;; The logic here is the same as for `icicle-narrow-candidates-with-predicate'.
  (cond
    ;; File name input, Emacs 20 or 21.  We can do nothing for file name.
    ;; `TAB' or `S-TAB' will bring it back as a candidate.
    ((and (icicle-file-name-input-p)  (< emacs-major-version 22)))

    ;; File name input, Emacs 22+.  Update `read-file-name-predicate'.
    ((and (icicle-file-name-input-p)  (= emacs-major-version 22))
     (setq read-file-name-predicate
           (if read-file-name-predicate
               (lexical-let ((curr-pred  read-file-name-predicate))
                 `(lambda (file-cand)
                   (and (not (equal ',disp-cand file-cand))  (funcall ',curr-pred file-cand))))
             `(lambda (file-cand) (not (equal ',disp-cand file-cand))))))
    ;; File name input, Emacs 23+.  And non-file name input, all Emacs versions.
    ;; Update `minibuffer-completion-predicate'.
    (t
     (setq minibuffer-completion-predicate
           (if minibuffer-completion-predicate
               ;; Add excluding of candidate to the existing predicate.
               (lexical-let ((curr-pred  minibuffer-completion-predicate))
                 `(lambda (cand) (and (not (equal cand ',mct-cand))  (funcall ',curr-pred cand))))
             ;; Set predicate to excluding the candidate.
             `(lambda (cand) (not (equal cand ',mct-cand))))))))

;; $$$$$$$$$$$$ COULD USE THIS INSTEAD of updating the predicate,
;; but it works only when `minibuffer-completion-table' is an alist.
;;   (when (consp minibuffer-completion-table)
;;     (setq minibuffer-completion-table
;;           (if allp
;;               (delete mct-cand minibuffer-completion-table)
;;             (icicle-delete-count mct-cand minibuffer-completion-table 1)))))

(defun icicle-update-and-next ()
  "Update `*Completions*' and make next candidate current.
If we don't know which candidate number this is, just display."
  (cond ((and icicle-completion-candidates
              (cdr icicle-completion-candidates) ; > 1 candidates left.
              (not (input-pending-p)))  ; Do nothing if user hit another key.
         (icicle-maybe-sort-and-strip-candidates)
         (message "Displaying completion candidates...")
         (save-selected-window (icicle-display-candidates-in-Completions))
         (when (wholenump icicle-candidate-nb)
           (with-current-buffer "*Completions*"
             (goto-char (icicle-start-of-candidates-in-Completions))
             (icicle-move-to-next-completion
              (mod icicle-candidate-nb (length icicle-completion-candidates)))
             (set-window-point (get-buffer-window "*Completions*" 0) (point))
             (setq icicle-last-completion-candidate  (icicle-current-completion-in-Completions))
             (set-buffer-modified-p nil))))
        (icicle-completion-candidates   ; Single candidate left
         (save-selected-window (icicle-remove-Completions-window))
         (let ((completion  (icicle-transform-multi-completion (car icicle-completion-candidates))))
           (select-window (active-minibuffer-window))
           (with-current-buffer (window-buffer) ; Needed if `*Completions*' redirected to minibuffer.
             (goto-char (icicle-minibuffer-prompt-end))
             (icicle-clear-minibuffer)
             (insert (if (and (icicle-file-name-input-p)
                              insert-default-directory
                              (or (not (member icicle-current-input icicle-extra-candidates))
                                  icicle-extra-candidates-dir-insert-p))
                         (icicle-file-name-directory-w-default icicle-current-input)
                       "")
                     completion))))
        (t                              ; No candidates left
         ;; $$$$$$$$ `icicle-abort-recursive-edit' and `exit-recursive-edit' don't work,
         ;; because they take us back to top level.
         ;; $$$$ DO NOTHING? Do (icicle-remove-Completions-window)? Do (icicle-erase-minibuffer)?
         (icicle-erase-minibuffer))))

(defun icicle-autofile-action (action)
  "Return a function that creates/sets an autofile or adds/removes tags.
If ACTION is:
 `add', the function prompts for tags to add, then adds them
 `remove', the function prompts for tags to remove, then removes them
 Anything else, the function just creates/sets an autofile bookmark

If `icicle-full-cand-fn' is non-nil and `icicle-file-name-input-p' is
nil, then update the current candidate to reflect the tag changes.
Then update `*Completions*' and make the next candidate current.

The candidate is updated as follows:
1. Apply `icicle-transform-multi-completion' to it.
2. Apply `icicle-full-cand-fn' to the result of #1.
3. Mctize the result of #2.  That is, make it usable for
   `minibuffer-completion-table'."
  `(lambda ()
    (interactive)
    (if (not icicle-last-completion-candidate)
        (icicle-msg-maybe-in-minibuffer "No current candidate - cycle to one")
      (let ((mct-cand  (icicle-mctized-display-candidate icicle-last-completion-candidate))
            (cand      (icicle-transform-multi-completion icicle-last-completion-candidate))
            (tags      (and (memq ',action '(add remove))
                            (let ((enable-recursive-minibuffers      t) ; Save and restore all of this.
                                  (minibuffer-completion-predicate   minibuffer-completion-predicate)
                                  (icicle-candidate-nb               icicle-candidate-nb)
                                  (icicle-last-completion-candidate  icicle-last-completion-candidate)
                                  (icicle-completion-candidates      icicle-completion-candidates)
                                  (icicle-current-input              icicle-current-input))
                              (bmkp-read-tags-completing)))))
        (if (memq ',action '(add remove))
            (funcall ',(if (eq 'add action) 'bmkp-autofile-add-tags 'bmkp-autofile-remove-tags)
                     cand tags nil nil 'MSG)
          (bmkp-bookmark-a-file cand nil nil 'MSG))
        (when (and icicle-full-cand-fn  (not (icicle-file-name-input-p)))
          (icicle-replace-mct-cand-in-mct mct-cand (icicle-mctized-full-candidate
                                                    (funcall icicle-full-cand-fn cand)))
          (let ((icicle-edit-update-p  t)) ; Update the display to show changed candidate.
            (funcall (or icicle-last-completion-command
                         (if (eq icicle-current-completion-mode 'prefix)
                             #'icicle-prefix-complete
                           #'icicle-apropos-complete)))))))))


(put 'icicle-mouse-help-on-candidate 'icicle-action-command t)

;;;###autoload (autoload 'icicle-mouse-help-on-candidate "icicles")
(defun icicle-mouse-help-on-candidate (event) ; Bound to `C-M-mouse-2' in minibuffer.
  "Display help on the minibuffer-completion candidate clicked by mouse."
  (interactive "e")
  (let ((icicle-candidate-action-fn  nil)) (icicle-mouse-candidate-action event)))


;; Free vars here: `icicle-orig-buff' is bound in `icicle-complete-keys'.
;;                 `icicle-complete-keys-alist' is bound in `icicles-var.el'.
(put 'icicle-help-on-candidate 'icicle-action-command t)

;;;###autoload (autoload 'icicle-help-on-candidate "icicles")
(defun icicle-help-on-candidate (&optional cand) ; Bound to `C-M-return', `C-help', `C-M-help',
                                        ; `C-f1', and `C-M-f1' in minibuffer and in *Completions.
  "Display help on the current minibuffer-completion candidate.
The help displayed depends on the type of candidate, as follows:

 menu item - the corresponding command is described using
             `describe-function' (only if `lacarte.el' is loaded)
 command or other function - described using `describe-function'
 keymap variable - described using `describe-keymap'
                   (if available - see library `help-fns+.el')
 user option or other variable - described using `describe-variable'
 face - described using `describe-face'
 command abbreviation - described using `apropos-command' for matches
 property list - described using `apropos-describe-plist'
 buffer name - modes described using `describe-mode' (Emacs > 20)
 file name - file properties described

If the same candidate names a function, a variable, and a face, or any
two of these, then all such documentation is shown (Emacs 22+).

In the minibuffer, you can also use `C-M-down', `C-M-up',
`C-M-wheel-down', `C-M-wheel-up', `C-M-next', `C-M-prior', `C-M-end',
and `C-M-home', to display help on the candidate and then move to the
next or previous candidate.  See, for example,
`icicle-help-on-next-apropos-candidate'.

You can use this command only from the minibuffer or `*Completions*'
\(`\\[icicle-help-on-candidate]')."
  (interactive)                         ; Interactively, just describes itself.
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (let ((frame-with-focus  (selected-frame))
        (cand-symb         nil)
        transformed-cand)
    (cond (cand (setq icicle-last-completion-candidate  cand))
          ((eq (current-buffer) (get-buffer "*Completions*"))
           (setq icicle-last-completion-candidate  (icicle-current-completion-in-Completions)))
          ;; If no last candidate, then reset to first candidate matching input.
          ((not (stringp icicle-last-completion-candidate))
           (setq icicle-last-completion-candidate  icicle-current-input
                 last-command                      'icicle-help-on-candidate)
           (let ((icicle-help-in-mode-line-delay  0)) ; Avoid delay for candidate help.
             (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                          'icicle-prefix-candidates
                                        'icicle-apropos-candidates)
                                    (not (eq icicle-current-completion-mode 'prefix))))))
    (cond (;; Use special help function.
           icicle-candidate-help-fn
           ;; We do not transform a multi-completion candidate before passing it to
           ;; `icicle-candidate-help-fn'.  It is that function that should do that (using
           ;; `icicle-transform-multi-completion').  We thus give that function access to the full,
           ;; untransformed candidate so that it can transform it anyway it wants.
           (funcall icicle-candidate-help-fn icicle-last-completion-candidate))

          (;; Call to `lacarte-execute(-menu)-command' (defined in `lacarte.el').
           ;; Use command associated with menu item.
           (consp lacarte-menu-items-alist) ; `lacarte-menu-items-alist' is in `lacarte.el'.
           (setq cand-symb  (cdr (assoc icicle-last-completion-candidate lacarte-menu-items-alist)))
           (if cand-symb
               (icicle-help-on-candidate-symbol cand-symb)
             (icicle-msg-maybe-in-minibuffer "No help"))) ; Menu item with lambda definition.

          (;; A key-completion candidate.  Get the true command from the candidate.
           icicle-completing-keys-p
           (save-match-data
             (string-match "\\(.+\\)  =  \\(.+\\)" icicle-last-completion-candidate)
             (setq cand-symb  (intern-soft (substring icicle-last-completion-candidate
                                                      (match-beginning 2) (match-end 2))))
             (cond ((eq '\.\.\. cand-symb) ; Prefix key - describe its binding.
                    (with-current-buffer (or icicle-orig-buff  (current-buffer))
                      (describe-key (car-safe
                                     (cdr-safe
                                      (assq (intern-soft
                                             (substring icicle-last-completion-candidate
                                                        (match-beginning 0) (match-end 0)))
                                            icicle-complete-keys-alist))))))
                   (cand-symb (icicle-help-on-candidate-symbol cand-symb)) ; Describe key's command.
                   (t (icicle-msg-maybe-in-minibuffer "No help")))))

          (t;; Transform candidate, in case it's a multi-completion.
           (setq transformed-cand  (icicle-transform-multi-completion icicle-last-completion-candidate))
           ;; If buffer or file, describe its properties.  Otherwise, create symbol and get its help.
           (cond ((and (bufferp (get-buffer transformed-cand))
                       (with-current-buffer transformed-cand
                         (if (fboundp 'describe-buffer) ; Defined in `help-fns+.el', Emacs 23+.
                             (describe-buffer)
                           (describe-mode)) t)))
                 ((or (icicle-file-remote-p transformed-cand) ; Don't let Tramp try to access it.
                      (file-exists-p transformed-cand))
                  (icicle-describe-file transformed-cand current-prefix-arg 'NO-ERROR-P))
                 (t (icicle-help-on-candidate-symbol (intern transformed-cand))))))
    ;;$$$ (icicle-raise-Completions-frame)

    ;; This is a hack for MS Windows - otherwise, we can't continue to get more candidates,
    ;; because the *Help* frame takes the focus away from the minibuffer frame.
    ;; MS Windows always gives focus to a newly created frame - in this case, *Help*.
    (let* ((help-window  (get-buffer-window "*Help*" 0))
           (help-frame   (and help-window  (window-frame help-window))))
      (when help-frame (redirect-frame-focus help-frame frame-with-focus))))
  (message nil))                        ; Let minibuffer contents show immediately.

(defun icicle-help-on-candidate-symbol (symb)
  "Helper function for `icicle-help-on-candidate'.  The arg is a symbol."
  (cond ((and (fboundp 'describe-keymap)  (boundp symb)  (keymapp (symbol-value symb)))
         (describe-keymap symb))
        ((and (fboundp 'help-follow-symbol) ; Emacs 22+
              (or (fboundp symb)  (boundp symb)  (facep symb)))
         (with-current-buffer (get-buffer-create "*Help*")
           ;; $$$$$$ (let ((help-xref-following  t)) (help-xref-interned symb)))
           (help-xref-interned symb))
         (when (fboundp 'fit-frame-if-one-window)
           (save-selected-window (select-window (get-buffer-window "*Help*" 'visible))
                                 (fit-frame-if-one-window))))
        ((fboundp symb) (describe-function symb))
        ((boundp symb) (describe-variable symb))
        ((facep symb) (describe-face symb))
        ((assq symb (mapcar #'cdr icicle-command-abbrev-alist))
         (let ((regexp  (icicle-command-abbrev-regexp symb))
               (sel-fr  (selected-frame)))
           (unwind-protect (apropos-command regexp) (select-frame-set-input-focus sel-fr))))
        ((symbol-plist symb) (apropos-describe-plist symb))
        (t
         (setq symb  (symbol-name symb)) ; Convert symbol to string, and try some more.
         (cond ((and (bufferp (get-buffer symb))
                     (with-current-buffer (get-buffer symb)
                       (if (fboundp 'describe-buffer) ; Defined in `help-fns+.el', Emacs 23+.
                           (describe-buffer)
                         (describe-mode))
                       t)))
               ((or (icicle-file-remote-p symb) ; Don't let Tramp try to access it.
                    (file-exists-p symb))
                (icicle-describe-file symb current-prefix-arg 'NO-ERROR-P))
               (t (icicle-msg-maybe-in-minibuffer "No help"))))))

;; This is the same as `describe-file' in `help-fns+.el', but we avoid requiring that library.
;; This is a top-level command, but we put it here to avoid library require cycles.
(if (and (not (fboundp 'icicle-describe-file))  (fboundp 'describe-file))
    (defalias 'icicle-describe-file (symbol-function 'describe-file))
  (defun icicle-describe-file (filename &optional internal-form-p no-error-p)
                                        ; Suggestion: bind to `C-h M-f'.
    "Describe the file named FILENAME.
If FILENAME is nil, describe current directory (`default-directory').

Starting with Emacs 22, if the file is an image file then:
 * Show a thumbnail of the image as well.
 * If you have command-line tool `exiftool' installed and in your
   `$PATH' or `exec-path', then show EXIF data (metadata) about the
   image.  See standard Emacs library `image-dired.el' for more
   information about `exiftool'.

If FILENAME is the name of an autofile bookmark and you use library
`Bookmark+', then show also the bookmark information (tags etc.).  In
this case, a prefix arg shows the internal form of the bookmark.

In Lisp code:

Non-nil optional arg INTERNAL-FORM-P shows the internal form.
Non-nil optional arg NO-ERROR-P prints an error message but does not
 raise an error."
    (interactive "FDescribe file: \nP")
    (unless filename (setq filename  default-directory))
    (help-setup-xref `(icicle-describe-file ,filename ,internal-form-p ,no-error-p) (interactive-p))
    (let ((attrs  (file-attributes filename))
          ;; Functions `bmkp-*' are defined in `bookmark+.el'.
          (bmk    (and (fboundp 'bmkp-get-autofile-bookmark)  (bmkp-get-autofile-bookmark filename))))
      (if (not attrs)
          (if no-error-p
              (message "Cannot open file `%s'" filename)
            (error "Cannot open file `%s'" filename))
        (let* ((type            (nth 0 attrs))
               (numlinks        (nth 1 attrs))
               (uid             (nth 2 attrs))
               (gid             (nth 3 attrs))
               (last-access     (nth 4 attrs))
               (last-mod        (nth 5 attrs))
               (last-status-chg (nth 6 attrs))
               (size            (nth 7 attrs))
               (permissions     (nth 8 attrs))
               ;; Skip 9: t iff file's gid would change if file were deleted and recreated.
               (inode           (nth 10 attrs))
               (device          (nth 11 attrs))
               (thumb-string    (and (fboundp 'image-file-name-regexp) ; In `image-file.el' (Emacs 22+).
                                     (icicle-string-match-p (image-file-name-regexp) filename)
                                     (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
                                     (require 'image-dired nil t)
                                     (image-dired-get-thumbnail-image filename)
                                     (apply #'propertize "XXXX"
                                            `(display ,(append (image-dired-get-thumbnail-image filename)
                                                               '(:margin 10))
                                              rear-nonsticky (display)
                                              mouse-face highlight
                                              follow-link t
                                              help-echo "`mouse-2' or `RET': Show full image"
                                              keymap
                                              (keymap
                                               (mouse-2 . (lambda (e) (interactive "e")
                                                                  (find-file ,filename)))
                                               (13 . (lambda () (interactive)
                                                             (find-file ,filename))))))))
               (image-info      (and (require 'image-dired nil t)
                                     (fboundp 'image-file-name-regexp)
                                     (icicle-string-match-p (image-file-name-regexp) filename)
                                     (progn (message "Gathering image data...") t)
                                     (icicle-condition-case-no-debug nil
                                         (let ((all  (icicle-all-exif-data (expand-file-name filename))))
                                           (concat
                                            (and all
                                                 (not (zerop (length all)))
                                                 (format "\nImage Data (EXIF)\n-----------------\n%s"
                                                         all))))
                                       (error nil))))
               (help-text
                (concat
                 (format "`%s'\n%s\n\n" filename (make-string (+ 2 (length filename)) ?-))
                 (format "File Type:                  %s\n"
                         (cond ((eq t type) "Directory")
                               ((stringp type) (format "Symbolic link to `%s'" type))
                               (t "Normal file")))
                 (format "Permissions:                %s\n" permissions)
                 (and (not (eq t type))  (format "Size in bytes:              %g\n" size))
                 (format-time-string
                  "Time of last access:        %a %b %e %T %Y (%Z)\n" last-access)
                 (format-time-string
                  "Time of last modification:  %a %b %e %T %Y (%Z)\n" last-mod)
                 (format-time-string
                  "Time of last status change: %a %b %e %T %Y (%Z)\n" last-status-chg)
                 (format "Number of links:            %d\n" numlinks)
                 (format "User ID (UID):              %s\n" uid)
                 (format "Group ID (GID):             %s\n" gid)
                 (format "Inode:                      %S\n" inode)
                 (format "Device number:              %s\n" device)
                 image-info)))
          (with-output-to-temp-buffer "*Help*"
            (when bmk
              (if internal-form-p
                  (let* ((bname     (bookmark-name-from-full-record bmk))
                         (bmk-defn  (format "Bookmark `%s'\n%s\n\n%s" bname
                                            (make-string (+ 11 (length bname)) ?-)
                                            (pp-to-string bmk))))
                    (princ bmk-defn) (terpri) (terpri))
                (princ (bmkp-bookmark-description bmk 'NO-IMAGE)) (terpri) (terpri)))
            (princ help-text))
          (when thumb-string
            (with-current-buffer "*Help*"
              (save-excursion
                (goto-char (point-min))
                (let ((buffer-read-only  nil))
                  (when (re-search-forward "Device number:.+\n" nil t) (insert thumb-string))))))
          help-text)))))                ; Return displayed text.

;; This is the same as `help-all-exif-data' in `help-fns+.el', but we avoid requiring that library.
(defun icicle-all-exif-data (file)
  "Return all EXIF data from FILE, using command-line tool `exiftool'."
  (with-temp-buffer
    (delete-region (point-min) (point-max))
    (unless (eq 0 (call-process shell-file-name nil t nil shell-command-switch
                                (format "exiftool -All \"%s\"" file)))
      (error "Could not get EXIF data"))
    (buffer-substring (point-min) (point-max))))

;;;###autoload (autoload 'icicle-candidate-read-fn-invoke "icicles")
(defun icicle-candidate-read-fn-invoke () ; Bound to `M-return' in minibuffer.
  "Read function name.  Invoke function on current completion candidate.
Set `icicle-candidate-action-fn' to the interned name.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-read-fn-invoke]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  ;; If no last candidate, then reset to first candidate matching input.
  (unless (stringp icicle-last-completion-candidate)
    (setq icicle-last-completion-candidate  icicle-current-input
          last-command                      'icicle-candidate-action)
    (let ((icicle-help-in-mode-line-delay  0)) ; Avoid delay for candidate help.
      (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                   'icicle-prefix-candidates
                                 'icicle-apropos-candidates)
                             (not (eq icicle-current-completion-mode 'prefix)))))
  (let ((icicle-whole-candidate-as-text-prop-p  nil)
        (enable-recursive-minibuffers            t)
        (icicle-must-pass-after-match-predicate  (lambda (s) (functionp (intern s))))
        (icicle-saved-completion-candidate       icicle-last-completion-candidate)
        (icicle-candidate-action-fn              'icicle-apply-to-saved-candidate))
    (icicle-apply-to-saved-candidate
     (completing-read (format "Function to apply to `%s': " icicle-saved-completion-candidate)
                      obarray))))

;;;###autoload (autoload 'icicle-mouse-candidate-read-fn-invoke "icicles")
(defun icicle-mouse-candidate-read-fn-invoke (event) ; Bound to `M-mouse-2' in `*Completions*'.
  "Read function name.  Invoke function on candidate clicked by mouse."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let (;;$$$$$$ (buffer    (window-buffer))
        (posn-win  (posn-window (event-start event)))
        (posn-col  (car (posn-col-row (event-start event))))
        (posn-row  (cdr (posn-col-row (event-start event))))
        choice base-size)
    ;; (read-event)                 ; Swallow mouse up event. $$ Not needed if bound to up event.
    (with-current-buffer (window-buffer posn-win)
      (save-excursion
        ;; $$$$$$ (when completion-reference-buffer (setq buffer  completion-reference-buffer))
        (setq base-size  completion-base-size)
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp))  (get-text-property (point) 'mouse-face))
            (setq end  (point)
                  beg  (1+ (point))))
          (unless beg (error "No completion here"))
          (setq beg     (previous-single-property-change beg 'mouse-face)
                end     (or (next-single-property-change end 'mouse-face)  (point-max))
                choice  (buffer-substring-no-properties beg end)))))
    (setq icicle-candidate-nb               (icicle-nb-of-cand-at-Completions-pos
                                             (posn-point (event-start event)))
          icicle-last-completion-candidate  choice)
    (let ((icicle-whole-candidate-as-text-prop-p  nil)
          (enable-recursive-minibuffers            t)
          (icicle-must-pass-after-match-predicate  (lambda (s) (functionp (intern s))))
          (icicle-saved-completion-candidate       icicle-last-completion-candidate)
          (icicle-candidate-action-fn              'icicle-apply-to-saved-candidate))
      (icicle-apply-to-saved-candidate
       (completing-read (format "Function to apply to `%s': " icicle-saved-completion-candidate)
                        obarray)))))

(defun icicle-apply-to-saved-candidate (function &optional use-icicle-candidates-alist-p type)
  "Apply FUNCTION to `icicle-saved-completion-candidate'.
If `current-prefix-arg' is non-nil, then pretty-print the result using
`icicle-pp-eval-expression'.
The string FUNCTION is read to obtain the real function to apply.
If optional arg USE-ICICLE-CANDIDATES-ALIST-P is non-nil, then try to
get the real function using `icicle-get-alist-candidate-function'.
If that returns nil, then read string FUNCTION.
Optional arg TYPE is the type of object that FUNCTION applies to."
  (let ((real-fn   (or (and use-icicle-candidates-alist-p
                            (cdr (funcall icicle-get-alist-candidate-function function 'no-error-no-msg)))
                       (car (read-from-string function))))
        (real-obj  (if (equal type "buffer") ; $$$$$$$ Eventually, perhaps look up TYPE in a list etc.
                       (get-buffer icicle-saved-completion-candidate)
                     icicle-saved-completion-candidate)))
    ;; Actually, we should test more than `functionp', to rule out macros and special forms.
    (unless (functionp real-fn) (error "Not a function: `%S'" real-fn))
    (icicle-condition-case-no-debug icicle-apply-to-saved-candidate
        (if current-prefix-arg
            (icicle-pp-eval-expression '(funcall real-fn real-obj))
          (funcall real-fn real-obj)
          (when (and (not icicle-all-candidates-action)  (current-message))
            (sit-for 3)))               ; In case the function displays a message.
      (error (message  "ERROR invoking `%S' on `%s': %s" real-fn icicle-saved-completion-candidate
                       (error-message-string icicle-apply-to-saved-candidate))
             (sleep-for 6)))
    (select-window (minibuffer-window))
    (select-frame-set-input-focus (selected-frame))
    (icicle-raise-Completions-frame)))

(defun icicle-raise-Completions-frame (&optional mouse-col mouse-row)
  "Raise `*Completions*' frame, if displayed.
This helps keep `*Completions*' on top.

If `icicle-move-Completions-frame' is non-nil and `*Completions*' is
in its own frame, then move that frame to the display edge, out of the
way.

Non-nil optional args MOUSE-COL and MOUSE-ROW move the mouse pointer
to column MOUSE-COL and row MOUSE-ROW.  Do this because
`icicle-candidate-action-fn' can call `select-frame-set-input-focus',
which can position mouse pointer on a standalone minibuffer frame."
  ;; Raise `*Completions*' frame, if displayed.  This helps keep `*Completions*' on top.
  (let ((compl-win  (get-buffer-window "*Completions*" 'visible)))
    (when compl-win
      (save-window-excursion
        (select-window compl-win)
        ;; Move frame to the right, out of the way.
        (when (and (one-window-p t)  icicle-move-Completions-frame)
          (modify-frame-parameters
           (selected-frame)             ; Hard-code 7 here - what does it depend on?
           (if (eq icicle-move-Completions-frame 'left)
               '((left . 0))
             `((left . ,(- (x-display-pixel-width) (+ (frame-pixel-width) 7))))))
          (raise-frame)
          (when (and (integerp mouse-col)  (integerp mouse-row))
            (set-mouse-position (selected-frame) mouse-col mouse-row)))))))

;;;###autoload (autoload 'icicle-Completions-mouse-3-menu "icicles")
(defun icicle-Completions-mouse-3-menu (event) ; Bound to `C-mouse-3' in `*Completions*'.
  "Pop-up menu on `C-mouse-3' for the current candidate in `*Completions*'."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let (;; $$$$$$ (buffer    (window-buffer))
        (posn-win  (posn-window (event-start event)))
        (posn-col  (car (posn-col-row (event-start event))))
        (posn-row  (cdr (posn-col-row (event-start event))))
        candidate base-size)
    ;; (read-event)                 ; Swallow mouse up event. $$ Not needed if bound to up event.
    (with-current-buffer (window-buffer posn-win)
      (save-excursion
        ;; $$$$$$ (when completion-reference-buffer (setq buffer  completion-reference-buffer))
        (setq base-size  completion-base-size)
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp))  (get-text-property (point) 'mouse-face))
            (setq end  (point)
                  beg  (1+ (point))))
          (unless beg (error "No completion here"))
          (setq beg       (previous-single-property-change beg 'mouse-face)
                end       (or (next-single-property-change end 'mouse-face)  (point-max))
                candidate (buffer-substring-no-properties beg end)))))
    (setq icicle-candidate-nb               (icicle-nb-of-cand-at-Completions-pos
                                             (posn-point (event-start event)))
          icicle-last-completion-candidate  candidate)
    (let* ((menus   `((keymap "Completion" ,@(icicle-substitute-keymap-vars
                                              icicle-Completions-mouse-3-menu-entries))))
           (choice  (x-popup-menu event menus)))
      (icicle-Completions-popup-choice menus choice))))

(defun icicle-substitute-keymap-vars (menu-entries)
  "In MENU-ENTRIES, replace keymap vars by their values."
  (let ((new  ()))
    (dolist (jj  menu-entries)
      (cond ((and (symbolp jj)  (boundp 'jj)  (keymapp (symbol-value jj))) ; Just a keymap var.
             (setq jj  (symbol-value jj))
             (dolist (ii  jj) (push ii new)))
            ;; (SYMBOL menu-item NAME MENU-KEYMAP . KEYWORDS), with a keymap var.
            ((and (consp jj)  (symbolp (car jj))  (eq 'menu-item (cadr jj))
                  (stringp (car (cddr jj)))
                  (symbolp (car (cdr (cddr jj))))
                  (not (commandp (car (cdr (cddr jj)))))  (boundp (car (cdr (cddr jj))))
                  (keymapp (symbol-value (car (cdr (cddr jj))))))
             (setq jj  `(,(car jj) menu-item ,(car (cddr jj))
                         ,(symbol-value (car (cdr (cddr jj)))) ; Replace keymap var by its value.
                         ,@(cdr (cdr (cddr jj))))) ; Keywords.
             (push jj new))
            ((and (consp jj)  (symbolp (car jj))  (stringp (cadr jj)) ; (SYMBOL NAME . MENU-KEYMAP)
                  (symbolp (cddr jj)) (boundp (cddr jj)) (keymapp (symbol-value (cddr jj))))
             (setq jj  `(,(car jj) ,(cadr jj) ,@(symbol-value (cddr jj)))) ; Replace keymap var by val.
             (push jj new))
            (t (push jj new))))
    (nreverse new)))

;; This is the same as `mouse3-region-popup-choice' in `mouse3.el'.
(if (require 'mouse3 nil t)
    (defalias 'icicle-Completions-popup-choice 'mouse3-region-popup-choice)
  (defun icicle-Completions-popup-choice (menus choice)
    "Invoke the command from MENUS that is represented by user's CHOICE.
MENUS is a list that is acceptable as the second argument for
`x-popup-menu'.  That is, it is one of the following, where MENU-TITLE
is the menu title and PANE-TITLE is a submenu title.

* a keymap - MENU-TITLE is its `keymap-prompt'
* a list of keymaps - MENU-TITLE is the first keymap's `keymap-prompt'
* a menu of multiple panes, which has this form: (MENU-TITLE PANE...),
  where each PANE has this form: (PANE-TITLE ITEM...),
  where each ITEM has one of these forms:
  - STRING - an unselectable menu item
  - (STRING . COMMAND) - a selectable item that invokes COMMAND"
    (catch 'icicle-Completions-popup-choice (icicle-Completions-popup-choice-1 menus choice))))

;; This is the same as `mouse3-region-popup-choice-1' in `mouse3.el'.
(if (require 'mouse3 nil t)
    (defalias 'icicle-Completions-popup-choice-1 'mouse3-region-popup-choice-1)
  (defun icicle-Completions-popup-choice-1 (menus choice)
    "Helper function for `icicle-Completions-popup-choice'."
    (cond((keymapp menus)
          ;; Look up each ITEM-LIST entry in keymap MENUS.
          ;;   If what is found is a keymap, use that as MENUS for next iteration.
          ;;   If what is found is a command, invoke it (done).
          (let (binding)
            (while choice
              (setq binding  (lookup-key menus (vector (car choice))))
              (cond ((keymapp binding)
                     (setq menus   binding
                           choice  (cdr choice)))
                    ((commandp binding)
                     ;; You get only one.
                     (throw 'icicle-Completions-popup-choice (call-interactively binding)))
                    (t (error "`icicle-Completions-popup-choice', binding: %s" binding))))))
         ((consp menus)                 ; A list of keymaps or panes.
          (dolist (menu  menus)
            (if (keymapp menu)
                (icicle-Completions-popup-choice-1 menu choice)
              (when choice              ; MENU is a pane.
                (throw 'icicle-Completions-popup-choice (call-interactively choice)))))))))

;;;###autoload (autoload 'icicle-widen-candidates "icicles")
(defun icicle-widen-candidates ()       ; Bound to `M-+' in minibuffer.
  "Complete, allowing also candidates that match an alternative regexp.
You are prompted for the alternative input pattern.  Use `RET' to
enter it.

To (apropos) complete using a wider set of candidates, you use this
command after you have completed (`TAB' or `S-TAB').  A shortcut is to
use `\\<minibuffer-local-completion-map>\\[icicle-apropos-complete-and-widen]' - \
it is the same as `S-TAB' followed by `\\[icicle-widen-candidates]'."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (unless icicle-completion-candidates
    (error "No completion candidates.  Did you use `TAB' or `S-TAB'?"))
  (let* ((raw-input                     (icicle-minibuf-input-sans-dir icicle-current-raw-input))
         (enable-recursive-minibuffers  t)
         (new-regexp                    (icicle-read-string "Or match alternative (use RET): "
                                                            nil regexp-history)))
    (setq icicle-current-raw-input
          (concat (if (< emacs-major-version 22) "\\(" "\\(?:") raw-input "\\|" new-regexp "\\)")))
  (icicle-clear-minibuffer)
  (insert icicle-current-raw-input)
  (let ((icicle-edit-update-p                 t)
        (icicle-expand-input-to-common-match  2)) ; Only explicit `TAB' or `S-TAB' or sole match.
    (icicle-apropos-complete)))

;;;###autoload (autoload 'icicle-narrow-candidates "icicles")
(defun icicle-narrow-candidates ()      ; Bound to `M-*' in minibuffer.
  "Narrow the set of completion candidates using another input regexp.
This, in effect, performs a set intersection operation on 1) the set
of candidates in effect before the operation and 2) the set of
candidates that match the current input.  You can repeatedly use this
command to continue intersecting candidate sets, progressively
narrowing the set of matches.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-narrow-candidates]')."
  ;; We handle `no-catch' errors here because `icicle-ORIG-completing-read' and
  ;; `icicle-ORIG-read-file-file-name' can still be called in Icicle mode by, for instance, an
  ;; `interactive' spec (e.g. (interactive "bBuffer: ")).  In that case, we throw to a
  ;; non-existant catch.  After doing that, we just insert the result, to pass it to the
  ;; next-higher recursive minibuffer.
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-completion-mode  'apropos)
  (let (;; Restore match function, in case it was bound to nil, e.g., by `C-h C-o'.
        (icicle-apropos-complete-match-fn  icicle-last-apropos-complete-match-fn)
        (icicle-progressive-completing-p   t) ; Inhibit completion by `icicle-minibuffer-setup'.
        (enable-recursive-minibuffers      t))
    (cond ((and icicle-completion-candidates  (null (cdr icicle-completion-candidates)))
           (if (not (and icicle-top-level-when-sole-completion-flag
                         (sit-for icicle-top-level-when-sole-completion-delay)))
               (minibuffer-message "  [Sole completion]")
             (set minibuffer-history-variable (cons (car icicle-completion-candidates)
                                                    (symbol-value minibuffer-history-variable)))
             ;; $$$$$$ Should this use `icicle-current-input' instead of
             ;; (car icicle-completion-candidates), for PCM?
             (icicle-condition-case-no-debug i-narrow-candidates
                 (throw 'icicle-read-top
                   (if (and (icicle-file-name-input-p)
                            insert-default-directory
                            (or (not (member (car icicle-completion-candidates)
                                             icicle-extra-candidates))
                                icicle-extra-candidates-dir-insert-p))
                       (expand-file-name (car icicle-completion-candidates))
                     (car icicle-completion-candidates)))
               (no-catch (setq icicle-current-input  (car icicle-completion-candidates))
                         (icicle-retrieve-last-input)
                         icicle-current-input)
               (error (message "%s" (error-message-string i-narrow-candidates))))))
          (t
           (let* ((minibuffer-setup-hook   (cons ; Make sure new minibuffer is completion reference buf.
                                            (lambda ()
                                              (with-current-buffer (get-buffer-create "*Completions*")
                                                (set (make-local-variable 'completion-reference-buffer)
                                                     (window-buffer (active-minibuffer-window)))))
                                            minibuffer-setup-hook))
                  (icicle-cands-to-narrow  icicle-completion-candidates)
                  (icicle-narrow-regexp    (and icicle-compute-narrowing-regexp-p
                                                ;; (regexp-opt ()) returns nil in older Emacs versions.
                                                icicle-cands-to-narrow
                                                (regexp-opt icicle-cands-to-narrow)))
                  ;; Handle all Emacs versions the same way for file-name completion.  We can do that
                  ;; because we use `icicle-must-pass-predicate', so we do not depend on `read-file-name'
                  ;; needing a PREDICATE arg.
                  ;;
                  ;; The choice between `icicle-must-pass-predicate' and
                  ;; `icicle-must-pass-after-match-predicate' could be per-command, by binding a variable.
                  ;; Without such a conditional choice, the former is better, because matching can be
                  ;; complex (costly).
                  (result                  (cond ((icicle-file-name-input-p)
                                                  (let ((icicle-must-pass-predicate
                                                         `(lambda (c) (member c ',icicle-cands-to-narrow))))
                                                    (read-file-name "Match also (regexp): "
                                                                    (icicle-file-name-directory-w-default
                                                                     icicle-current-input)
                                                                    nil icicle-require-match-p)))
                                                 ((functionp minibuffer-completion-table)
                                                  (let ((icicle-must-pass-predicate
                                                         `(lambda (c) (member c ',icicle-cands-to-narrow))))
                                                    (completing-read "Match also (regexp): "
                                                                     minibuffer-completion-table
                                                                     nil icicle-require-match-p nil
                                                                     minibuffer-history-variable)))
                                                 (t ; cons, obarray, etc.
                                                  (completing-read
                                                   "Match also (regexp): "
                                                   (if icicle-whole-candidate-as-text-prop-p
                                                       (mapcar
                                                        (lambda (cand)
                                                          (funcall icicle-get-alist-candidate-function
                                                                   (car cand)))
                                                        (icicle-filter-alist minibuffer-completion-table
                                                                             icicle-completion-candidates))
                                                     (mapcar #'list icicle-completion-candidates))
                                                   nil icicle-require-match-p nil
                                                   minibuffer-history-variable)))))
             ;; Normally, `icicle-narrow-candidates' is called from the minibuffer.
             ;; If not, just return the result read.
             (if (> (minibuffer-depth) 0)
                 (icicle-condition-case-no-debug i-narrow-candidates
                     (throw 'icicle-read-top result)
                   (no-catch (setq icicle-current-input  result)
                             (icicle-retrieve-last-input)
                             icicle-current-input)
                   (error (message "%s" (error-message-string i-narrow-candidates))))
               result))))))

;;;###autoload (autoload 'icicle-apropos-complete-and-widen "icicles")
(defun icicle-apropos-complete-and-widen () ; Bound to `S-backspace' in minibuffer.
  "Apropos complete, then `icicle-widen-candidates'.
You must enter the new, alternative input pattern using `RET'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-apropos-complete-and-widen]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  ;; $$$$$ (let ((icicle-top-level-when-sole-completion-flag  t))
  (when (and (eq icicle-current-completion-mode 'prefix)
             (eq (icicle-current-TAB-method) 'basic)
             icicle-last-input)
    (let ((icicle-incremental-completion-p  nil)
          (regexp-quoted-input              (regexp-quote icicle-last-input)))
      (setq regexp-quoted-input  (if (icicle-file-name-input-p)
                                     (concat (icicle-file-name-directory regexp-quoted-input) "^"
                                             (file-name-nondirectory regexp-quoted-input))
                                   (concat "^" regexp-quoted-input)))
      (icicle-erase-minibuffer)
      (insert regexp-quoted-input)))
  (if (eq icicle-last-completion-command 'icicle-apropos-complete-no-display)
      (icicle-apropos-complete-no-display)
    (icicle-apropos-complete))
  (icicle-widen-candidates))

;;;###autoload (autoload 'icicle-apropos-complete-and-narrow "icicles")
(defun icicle-apropos-complete-and-narrow () ; Bound to `S-SPC' in minibuffer.
  "Apropos complete, then `icicle-narrow-candidates'.
You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-apropos-complete-and-narrow]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  ;; $$$$$ (let ((icicle-top-level-when-sole-completion-flag  t))
  (when (and (eq icicle-current-completion-mode 'prefix)
             (eq (icicle-current-TAB-method) 'basic)
             icicle-last-input)
    (let ((icicle-incremental-completion-p  nil)
          (regexp-quoted-input              (regexp-quote icicle-last-input)))
      (setq regexp-quoted-input  (if (icicle-file-name-input-p)
                                     (concat (icicle-file-name-directory regexp-quoted-input) "^"
                                             (file-name-nondirectory regexp-quoted-input))
                                   (concat "^" regexp-quoted-input)))
      (icicle-erase-minibuffer)
      (insert regexp-quoted-input)))
  (setq icicle-next-apropos-complete-cycles-p  nil)
  (if (eq icicle-last-completion-command 'icicle-apropos-complete-no-display)
      (icicle-apropos-complete-no-display)
    (icicle-apropos-complete))
  (icicle-narrow-candidates))

;;;###autoload (autoload 'icicle-narrow-candidates-with-predicate "icicles")
(defun icicle-narrow-candidates-with-predicate (&optional predicate) ; Bound to `M-&' in minibuffer.
  "Narrow the set of completion candidates by applying a predicate.
You can repeatedly use this command to apply additional predicates,
progressively narrowing the set of candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-narrow-candidates-with-predicate]').

When called from Lisp with non-nil arg PREDICATE, use that to narrow."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let (;; Restore match function, in case it was bound to nil, e.g., by `C-h C-o'.
        (icicle-apropos-complete-match-fn  icicle-last-apropos-complete-match-fn)
        (icicle-progressive-completing-p   t) ; Inhibit completion by `icicle-minibuffer-setup'.
        (last-completion-cmd               (or icicle-last-completion-command  'icicle-apropos-complete))
        (enable-recursive-minibuffers      t))
    (cond ((null icicle-completion-candidates)
           (error "No completion candidates.  Did you use `TAB' or `S-TAB'?"))
          ((null (cdr icicle-completion-candidates))
           (if (not (and icicle-top-level-when-sole-completion-flag
                         (sit-for icicle-top-level-when-sole-completion-delay)))
               (minibuffer-message "  [Sole completion]")
             (set minibuffer-history-variable (cons (car icicle-completion-candidates)
                                                    (symbol-value minibuffer-history-variable)))
             ;; $$$$$$ Should this now use `icicle-current-input'
             ;;        instead of (car icicle-completion-candidates), for PCM?
             (icicle-condition-case-no-debug i-narrow-candidates
                 (throw 'icicle-read-top
                   (if (and (icicle-file-name-input-p)
                            insert-default-directory
                            (or (not (member (car icicle-completion-candidates) icicle-extra-candidates))
                                icicle-extra-candidates-dir-insert-p))
                       (expand-file-name (car icicle-completion-candidates))
                     (car icicle-completion-candidates)))
               (no-catch (setq icicle-current-input  (car icicle-completion-candidates))
                         (icicle-retrieve-last-input)
                         icicle-current-input)
               (error (message "%s" (error-message-string i-narrow-candidates))))))
          (t                            ; Read new predicate and incorporate it.
           (let ((pred  (or predicate
                            (icicle-read-from-minibuf-nil-default
                             "Additional predicate to apply: "
                             nil read-expression-map t (if (boundp 'function-name-history)
                                                           'function-name-history
                                                         'icicle-function-name-history)))))
             ;; Update `read-file-name-predicate' or `minibuffer-completion-predicate'
             ;; to also use new predicate, PRED.
             ;; The logic here is the same as for `icicle-remove-cand-from-lists'.
             (cond (;; File name input, Emacs 22+.  Update `read-file-name-predicate'.
                    (and (icicle-file-name-input-p)  (> emacs-major-version 21))
                    (setq read-file-name-predicate
                          (if read-file-name-predicate
                              (lexical-let ((curr-pred  read-file-name-predicate))
                                `(lambda (file-cand)
                                  (and (funcall ',curr-pred file-cand)  (funcall ',pred file-cand))))
                            pred)))

                   ;; File name input, Emacs 20 or 21.  We can do nothing for file name.
                   ;; `TAB' or `S-TAB' will unfortunately bring it back as a candidate.
                   ((icicle-file-name-input-p))

                   (t;; Non-file name input, all versions.  Update `minibuffer-completion-predicate'.
                    (setq minibuffer-completion-predicate
                          (if minibuffer-completion-predicate
                              ;; Add PRED to the existing predicate.
                              (lexical-let ((curr-pred  minibuffer-completion-predicate))
                                `(lambda (cand)
                                  (and (funcall ',curr-pred cand)  (funcall ',pred cand))))
                            ;; Set predicate to PRED.
                            pred)))))))
    (funcall last-completion-cmd)))

;;;###autoload (autoload 'icicle-save-predicate-to-variable "icicles")
(defun icicle-save-predicate-to-variable (askp) ; Bound to `C-M-&' in minibuffer.
  "Save the current completion predicate to a variable.
By default, the variable is `icicle-input-string'.  If you use a
prefix argument, then you are prompted for the variable to use.

You can retrieve the saved predicate as a string using `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-save-predicate-to-variable]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (let* ((pred                                    minibuffer-completion-predicate)
         (icicle-whole-candidate-as-text-prop-p   nil)
         (enable-recursive-minibuffers            t)
         (icicle-must-pass-after-match-predicate  (lambda (s) (boundp (intern s))))
         (var                                     (if askp
                                                      (intern
                                                       (completing-read
                                                        "Save candidates in variable: " obarray nil
                                                        nil nil (if (boundp 'variable-name-history)
                                                                    'variable-name-history
                                                                  'icicle-variable-name-history)))
                                                    'icicle-input-string)))
    (set var (prin1-to-string pred))
    (save-selected-window (select-window (minibuffer-window))
                          (minibuffer-message (format "  [Predicate SAVED to `%s']" var)))))

;;;###autoload (autoload 'icicle-completing-read+insert "icicles")
(defun icicle-completing-read+insert () ; Bound to `C-M-S-c' (`C-M-C') in minibuffer.
  "Read something with completion, and insert it.
Be sure to bind `icicle-completing-read+insert-candidates' to the set
of candidates.
Option `icicle-completing-read+insert-keys' controls which keys are
bound to this command.
Return the string that was inserted."
  (interactive)
  (if icicle-completing-read+insert-candidates
      (let ((enable-recursive-minibuffers  t)
            (use-dialog-box                nil)
            (result
             (icicle-completing-read "Choose: " icicle-completing-read+insert-candidates)))
        (insert result)
        result)
    (icicle-msg-maybe-in-minibuffer "On-demand completion not available")))

;;;###autoload (autoload 'icicle-read+insert-file-name "icicles")
(defun icicle-read+insert-file-name (dir-too-p) ; Bound to `C-M-S-f' (`C-M-F') in minibuffer.
  "Read a file name and insert it, without its directory, by default.
With a prefix argument, insert its directory also.
Option `icicle-read+insert-file-name-keys' controls which keys are
 bound to this command.
Return the string that was inserted."
  (interactive "P")
  (let ((completion-ignore-case                  (memq system-type '(ms-dos windows-nt cygwin)))
        (enable-recursive-minibuffers            t)
        (use-dialog-box                          nil)
        (minibuffer-local-completion-map
         (let ((map  (make-sparse-keymap)))
           (set-keymap-parent map minibuffer-local-completion-map)
           (define-key map (icicle-kbd "C-backspace") 'icicle-up-directory) ; `C-backspace'
           (define-key map (icicle-kbd "C-c +")       'icicle-make-directory) ; `C-c +'
           map))
        (minibuffer-local-must-match-map
         (let ((map  (make-sparse-keymap)))
           (set-keymap-parent map minibuffer-local-must-match-map)
           (define-key map (icicle-kbd "C-backspace") 'icicle-up-directory)
           (define-key map (icicle-kbd "C-c +")       'icicle-make-directory)
           map))
        (icicle-must-pass-after-match-predicate  nil)
        result)
    (setq result  (icicle-read-file-name "Choose file: "))
    (unless dir-too-p                   ; Remove parent dir.
      (setq result  (if (file-directory-p result)
                        (file-name-as-directory (file-name-nondirectory (directory-file-name result)))
                      (file-name-nondirectory result))))
    (insert result)
    result))

;;;###autoload (autoload 'icicle-bind-buffer-candidate-keys "icicles")
(defun icicle-bind-buffer-candidate-keys () ; Use in first code of buffer-candidate commands.
  "Bind specific keys for acting on the current buffer-name candidate."
  (when (require 'bookmark+ nil t)
    (define-key minibuffer-local-completion-map (icicle-kbd "C-x m") ; `C-x m'
      'icicle-bookmark-non-file-other-window)
    (define-key minibuffer-local-must-match-map (icicle-kbd "C-x m") ; `C-x m'
      'icicle-bookmark-non-file-other-window))
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x M -") ; `C-x M -'
    'icicle-remove-buffer-cands-for-mode)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x M -") ; `C-x M -'
    'icicle-remove-buffer-cands-for-mode)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x M +") ; `C-x M +'
    'icicle-keep-only-buffer-cands-for-mode)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x M +") ; `C-x M +'
    'icicle-keep-only-buffer-cands-for-mode)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x C-m -") ; `C-x C-m -', aka `C-x RET -'
    'icicle-remove-buffer-cands-for-derived-mode)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x C-m -") ; `C-x C-m -', aka `C-x RET -'
    'icicle-remove-buffer-cands-for-derived-mode)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x C-m +") ; `C-x C-m +', aka `C-x RET +'
    'icicle-keep-only-buffer-cands-for-derived-mode)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x C-m +") ; `C-x C-m +', aka `C-x RET +'
    'icicle-keep-only-buffer-cands-for-derived-mode)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x F") ; `C-x F'
    'icicle-toggle-include-cached-files)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x F") ; `C-x F'
    'icicle-toggle-include-cached-files)
  (when (> emacs-major-version 20)
    (define-key minibuffer-local-completion-map (icicle-kbd "C-x R")   ; `C-x R'
      'icicle-toggle-include-recent-files)
    (define-key minibuffer-local-must-match-map (icicle-kbd "C-x R")   ; `C-x R'
      'icicle-toggle-include-recent-files)))

;;;###autoload (autoload 'icicle-unbind-buffer-candidate-keys "icicles")
(defun icicle-unbind-buffer-candidate-keys () ; Use in last code of buffer-candidate commands.
  "Unbind specific keys for acting on the current buffer-name candidate."
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x m")     nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x m")     nil)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x M -")   nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x M -")   nil)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x M +")   nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x M +")   nil)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x M")     nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x M")     nil)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x C-m -") nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x C-m -") nil)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x C-m +") nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x C-m +") nil)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x C-m")   nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x C-m")   nil)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x F")     nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x F")     nil)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x R")     nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x R")     nil))


;; `minibuffer-local-filename-completion-map' and `minibuffer-local-must-match-filename-map'
;; were introduced in Emacs 22, and they inherit from `minibuffer-local-completion' and
;; `minibuffer-local-must-match-map', respectively.  For Emacs 23.1,
;; `minibuffer-local-must-match-filename-map' is an alias for
;; `minibuffer-local-filename-must-match-map'.  But for Emacs 23.2, there is no such alias!
;; And for Emacs 24+, there is no longer a `minibuffer-local-filename-must-match-map'.
;;
;;;###autoload (autoload 'icicle-bind-file-candidate-keys "icicles")
(defun icicle-bind-file-candidate-keys ()
  "Bind specific keys for acting on the current file-name candidate."
  (cond ((boundp 'minibuffer-local-filename-completion-map)
         (define-key minibuffer-local-filename-completion-map (icicle-kbd "C-backspace") ; `C-backspace'
           'icicle-up-directory)
         (define-key minibuffer-local-filename-completion-map (icicle-kbd "C-c +") ; `C-c +'
           'icicle-make-directory))
        (t
         (define-key minibuffer-local-completion-map (icicle-kbd "C-backspace") ; `C-backspace'
           'icicle-up-directory)
         (define-key minibuffer-local-completion-map (icicle-kbd "C-c +") ; `C-c +'
           'icicle-make-directory)))
  (cond ((boundp 'minibuffer-local-filename-must-match-map)
         (define-key minibuffer-local-filename-must-match-map (icicle-kbd "C-backspace") ; `C-backspace'
           'icicle-up-directory)
         (define-key minibuffer-local-filename-must-match-map (icicle-kbd "C-c +") ; `C-c +'
           'icicle-make-directory))
        ((boundp 'minibuffer-local-must-match-filename-map)
         (define-key minibuffer-local-must-match-filename-map (icicle-kbd "C-backspace") ; `C-backspace'
           'icicle-up-directory)
         (define-key minibuffer-local-must-match-filename-map (icicle-kbd "C-c +") ; `C-c +'
           'icicle-make-directory))
        (t
         (define-key minibuffer-local-must-match-map (icicle-kbd "C-backspace") ; `C-backspace'
           'icicle-up-directory)
         (define-key minibuffer-local-must-match-map (icicle-kbd "C-c +") ; `C-c +'
           'icicle-make-directory)))
  (when (require 'bookmark+ nil t)
    (cond ((boundp 'minibuffer-local-filename-completion-map)
           (define-key minibuffer-local-filename-completion-map (icicle-kbd "C-x m") ; `C-x m'
             'icicle-bookmark-file-other-window)
           (define-key minibuffer-local-filename-completion-map (icicle-kbd "C-x a +") ; `C-x a +'
             (icicle-autofile-action 'add))
           (define-key minibuffer-local-filename-completion-map (icicle-kbd "C-x a -") ; `C-x a -'
             (icicle-autofile-action 'remove))
           (define-key minibuffer-local-filename-completion-map (icicle-kbd "C-x a a") ; `C-x a a'
             (icicle-autofile-action 'create/set)))
          (t
           (define-key minibuffer-local-completion-map (icicle-kbd "C-x m") ; `C-x m'
             'icicle-bookmark-file-other-window)
           (define-key minibuffer-local-completion-map (icicle-kbd "C-x a +") ; `C-x a +'
             (icicle-autofile-action 'add))
           (define-key minibuffer-local-completion-map (icicle-kbd "C-x a -") ; `C-x a -'
             (icicle-autofile-action 'remove))
           (define-key minibuffer-local-completion-map (icicle-kbd "C-x a a") ; `C-x a a'
             (icicle-autofile-action 'create/set))))
    (cond ((boundp 'minibuffer-local-filename-must-match-map)
           (define-key minibuffer-local-filename-must-match-map (icicle-kbd "C-x m") ; `C-x m'
             'icicle-bookmark-file-other-window)
           (define-key minibuffer-local-filename-must-match-map (icicle-kbd "C-x a +") ; `C-x a +'
             (icicle-autofile-action 'add))
           (define-key minibuffer-local-filename-must-match-map (icicle-kbd "C-x a -") ; `C-x a -'
             (icicle-autofile-action 'remove))
           (define-key minibuffer-local-filename-must-match-map (icicle-kbd "C-x a a") ; `C-x a a'
             (icicle-autofile-action 'create/set)))
          ((boundp 'minibuffer-local-must-match-filename-map)
           (define-key minibuffer-local-must-match-filename-map (icicle-kbd "C-x m") ; `C-x m'
             'icicle-bookmark-file-other-window)
           (define-key minibuffer-local-must-match-filename-map (icicle-kbd "C-x a +") ; `C-x a +'
             (icicle-autofile-action 'add))
           (define-key minibuffer-local-must-match-filename-map (icicle-kbd "C-x a -") ; `C-x a -'
             (icicle-autofile-action 'remove))
           (define-key minibuffer-local-must-match-filename-map (icicle-kbd "C-x a a") ; `C-x a a'
             (icicle-autofile-action 'create/set)))
          (t
           (define-key minibuffer-local-must-match-map (icicle-kbd "C-x m") ; `C-x m'
             'icicle-bookmark-file-other-window)
           (define-key minibuffer-local-must-match-map (icicle-kbd "C-x a +") ; `C-x a +'
             (icicle-autofile-action 'add))
           (define-key minibuffer-local-must-match-map (icicle-kbd "C-x a -") ; `C-x a -'
             (icicle-autofile-action 'remove))
           (define-key minibuffer-local-must-match-map (icicle-kbd "C-x a a") ; `C-x a a'
             (icicle-autofile-action 'create/set)))))
  ;; When using `completing-read', not `read-file-name', regardless of the Emacs version.
  (unless (icicle-file-name-input-p)
    (define-key minibuffer-local-completion-map (icicle-kbd "C-backspace") ; `C-backspace'
      'icicle-up-directory)
    (define-key minibuffer-local-completion-map (icicle-kbd "C-c +") ; `C-c +'
      'icicle-make-directory)
    (define-key minibuffer-local-completion-map (icicle-kbd "C-x m") ; `C-x m'
      'icicle-bookmark-file-other-window)
    (define-key minibuffer-local-completion-map (icicle-kbd "C-x a +") ; `C-x a +'
      (icicle-autofile-action 'add))
    (define-key minibuffer-local-completion-map (icicle-kbd "C-x a -") ; `C-x a -'
      (icicle-autofile-action 'remove))
    (define-key minibuffer-local-completion-map (icicle-kbd "C-x a a") ; `C-x a a'
      (icicle-autofile-action 'create/set))

    (define-key minibuffer-local-must-match-map (icicle-kbd "C-backspace") ; `C-backspace'
      'icicle-up-directory)
    (define-key minibuffer-local-must-match-map (icicle-kbd "C-c +") ; `C-c +'
      'icicle-make-directory)
    (define-key minibuffer-local-must-match-map (icicle-kbd "C-x m") ; `C-x m'
      'icicle-bookmark-file-other-window)
    (define-key minibuffer-local-must-match-map (icicle-kbd "C-x a +") ; `C-x a +'
      (icicle-autofile-action 'add))
    (define-key minibuffer-local-must-match-map (icicle-kbd "C-x a -") ; `C-x a -'
      (icicle-autofile-action 'remove))
    (define-key minibuffer-local-must-match-map (icicle-kbd "C-x a a") ; `C-x a a'
      (icicle-autofile-action 'create/set))))

;;;###autoload (autoload 'icicle-unbind-file-candidate-keys "icicles")
(defun icicle-unbind-file-candidate-keys ()
  "Unbind specific keys for acting on the current file-name candidate."
  (when (boundp 'minibuffer-local-filename-completion-map)
    (define-key minibuffer-local-filename-completion-map (icicle-kbd "C-backspace") nil)
    (define-key minibuffer-local-filename-completion-map (icicle-kbd "C-c +")       nil)
    (define-key minibuffer-local-filename-completion-map (icicle-kbd "C-x m")       nil)
    (define-key minibuffer-local-filename-completion-map (icicle-kbd "C-x a +")     nil)
    (define-key minibuffer-local-filename-completion-map (icicle-kbd "C-x a -")     nil)
    (define-key minibuffer-local-filename-completion-map (icicle-kbd "C-x a a")     nil)
    (define-key minibuffer-local-filename-completion-map (icicle-kbd "C-x a")       nil))
  (when (boundp 'minibuffer-local-filename-must-match-map)
    (define-key minibuffer-local-filename-must-match-map (icicle-kbd "C-backspace") nil)
    (define-key minibuffer-local-filename-must-match-map (icicle-kbd "C-c +")       nil)
    (define-key minibuffer-local-filename-must-match-map (icicle-kbd "C-x m")       nil)
    (define-key minibuffer-local-filename-must-match-map (icicle-kbd "C-x a +")     nil)
    (define-key minibuffer-local-filename-must-match-map (icicle-kbd "C-x a -")     nil)
    (define-key minibuffer-local-filename-must-match-map (icicle-kbd "C-x a a")     nil)
    (define-key minibuffer-local-filename-must-match-map (icicle-kbd "C-x a")       nil))
  (when (boundp 'minibuffer-local-must-match-filename-map)
    (define-key minibuffer-local-must-match-filename-map (icicle-kbd "C-backspace") nil)
    (define-key minibuffer-local-must-match-filename-map (icicle-kbd "C-c +")       nil)
    (define-key minibuffer-local-must-match-filename-map (icicle-kbd "C-x m")       nil)
    (define-key minibuffer-local-must-match-filename-map (icicle-kbd "C-x a +")     nil)
    (define-key minibuffer-local-must-match-filename-map (icicle-kbd "C-x a -")     nil)
    (define-key minibuffer-local-must-match-filename-map (icicle-kbd "C-x a a")     nil)
    (define-key minibuffer-local-must-match-filename-map (icicle-kbd "C-x a")       nil))
  (define-key minibuffer-local-completion-map (icicle-kbd "C-backspace")            nil)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-c +")                  nil)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x m")                  nil)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x a +")                nil)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x a -")                nil)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x a a")                nil)
  (define-key minibuffer-local-completion-map (icicle-kbd "C-x a")                  nil)

  (define-key minibuffer-local-must-match-map (icicle-kbd "C-backspace")            nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-c +")                  nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x m")                  nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x a +")                nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x a -")                nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x a a")                nil)
  (define-key minibuffer-local-must-match-map (icicle-kbd "C-x a")                  nil))

;;;###autoload (autoload 'icicle-candidate-set-swap "icicles")
(defun icicle-candidate-set-swap ()     ; Bound to `C-%' in minibuffer.
  "Swap the saved set and current sets of completion candidates.
You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-swap]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (setq icicle-saved-completion-candidates
        (prog1 icicle-completion-candidates
          (setq icicle-completion-candidates  icicle-saved-completion-candidates)))
  (minibuffer-message "  [Saved set of candidates SWAPPED with current]"))

;;;###autoload (autoload 'icicle-candidate-set-define "icicles")
(defun icicle-candidate-set-define ()   ; Bound to `C-:' in minibuffer.
  "Define the set of current completion candidates by evaluating a sexp.
The Lisp sexp must evaluate to a list of strings, such as is returned
by `all-completions'.

You can use this command at top level or from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-define]')."
  (interactive)
  (let* ((enable-recursive-minibuffers  t)
         (evald-sexp                    (eval-minibuffer
                                         "Set the completion candidates to sexp (eval): ")))
    (when (and evald-sexp  (or (atom evald-sexp)  (not (stringp (car evald-sexp)))))
      (error "Sexp did not evaluate to a list of strings: %S" evald-sexp))
    (setq icicle-completion-candidates  evald-sexp))
  (icicle-maybe-sort-and-strip-candidates)
  (message "List of completion candidates DEFINED: %S" icicle-completion-candidates)
  (when (> (minibuffer-depth) 0)
    (message "Displaying completion candidates...")
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list icicle-completion-candidates))
    (icicle-narrow-candidates)))

;;;###autoload (autoload 'icicle-candidate-set-difference "icicles")
(defun icicle-candidate-set-difference () ; Bound to `C--' in minibuffer.
  "Take the set difference between the current and saved candidates.
The new set of candidates is the set of candidates prior to executing
this command minus the saved set of candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-difference]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (message "Computing set difference: current minus saved candidates...")
  (icicle-candidate-set-1 'icicle-set-difference "  [saved set of candidates SUBTRACTED]"))

;;;###autoload (autoload 'icicle-candidate-set-union "icicles")
(defun icicle-candidate-set-union ()    ; Bound to `C-+' in minibuffer.
  "Take the set union between the current and saved candidates.
The new set of candidates is the union of the saved set of candidates
and the set of candidates prior to executing this command.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-union]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (message "Computing set union: current plus saved candidates...")
  (icicle-candidate-set-1 'icicle-set-union "  [saved set of candidates ADDED]"))

;;;###autoload (autoload 'icicle-candidate-set-intersection "icicles")
(defun icicle-candidate-set-intersection () ; Bound to `C-*' in minibuffer.
  "Take the set intersection between the current and saved candidates.
The new set of candidates is the intersection of the saved set of
candidates and the set of candidates prior to executing this command.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-intersection]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (message "Computing set intersection: current and saved candidates...")
  (icicle-candidate-set-1 'icicle-set-intersection
                          "  [INTERSECTION of saved and current sets of candidates]"))

;;;###autoload (autoload 'icicle-candidate-set-complement "icicles")
(defun icicle-candidate-set-complement () ; Bound to `C-~' in minibuffer.
  "Complement the set of current completion candidates.
The new set of candidates is the set of all candidates in the initial
completion domain minus the set of matching candidates prior to
executing this command - that is, all possible completions of the
appropriate type, except for those that are in the current set of
completions.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-complement]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (message "Complementing current set of candidates...")
  (let ((initial-cands  (icicle-all-completions
                         "" minibuffer-completion-table
                         minibuffer-completion-predicate
                         (and icicle-buffer-name-input-p ; Used only by Emacs < 23.2.
                              icicle-buffer-ignore-space-prefix-flag))))
    (setq icicle-completion-candidates  (icicle-set-difference
                                         (if icicle-must-pass-after-match-predicate
                                             (icicle-remove-if-not
                                              icicle-must-pass-after-match-predicate initial-cands)
                                           initial-cands)
                                         icicle-completion-candidates)))
  (icicle-maybe-sort-and-strip-candidates)
  (message "Displaying completion candidates...")
  (with-output-to-temp-buffer "*Completions*" (display-completion-list icicle-completion-candidates))
  (minibuffer-message "  [Set of candidates COMPLEMENTED]")
  (icicle-narrow-candidates))

(defun icicle-candidate-set-truncate (n) ; Bound to `M-$' in minibuffer.
  "Trim the set of current completion candidates at the end.
The first N candidates are kept.  N is read."
  ;; Ugly hack: `icicle-saved-completion-candidates-internal'.  No way to bind a variable
  ;; in `interactive' and have the binding be active in the function body.
  (interactive
   (list (let ((enable-recursive-minibuffers  t))
           (setq icicle-saved-completion-candidates-internal  icicle-completion-candidates)
           (if current-prefix-arg
               (prefix-numeric-value current-prefix-arg)
             (read-number "Number of candidates to keep: ")))))
  (setq icicle-completion-candidates  icicle-saved-completion-candidates-internal)
  (setcdr (nthcdr (1- n) icicle-completion-candidates) nil)
  (icicle-maybe-sort-and-strip-candidates)
  (message "Displaying completion candidates...")
  (with-output-to-temp-buffer "*Completions*" (display-completion-list icicle-completion-candidates))
  (message (format "  [Set of candidates TRUNCATED to %d]" n))
  (icicle-narrow-candidates))

;;;###autoload (autoload 'icicle-candidate-set-retrieve "icicles")
(defun icicle-candidate-set-retrieve (&optional arg) ; Bound to `C-M-<' in minibuffer.
  "Retrieve a saved set of completion candidates, making it current.
This retrieves candidates saved with `\\<minibuffer-local-completion-map>\
\\[icicle-save/unsave-candidate]', `M-S-mouse-2',
`\\<minibuffer-local-completion-map>\\[icicle-candidate-set-save]', \
`\\[icicle-candidate-set-save-to-variable]', or `\\[icicle-candidate-set-save-persistently]'.

With no prefix arg, retrieve candidates from variable
 `icicle-saved-completion-candidates'.
With a numeric prefix arg, retrieve candidates from another variable.
With a plain prefix arg (`C-u'), retrieve candidates from a cache file
 or, if option `icicle-filesets-as-saved-completion-sets-flag' is
 non-nil, an Emacs fileset name (Emacs 22 or later).  To use filesets,
 you must also load library `filesets' and use `(filesets-init)'.

Completion is available when you are prompted for a cache file,
fileset, or variable name.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (icicle-candidate-set-retrieve-1 arg))

(defun icicle-candidate-set-retrieve-1 (arg &optional morep)
  "Helper function for `icicle-candidate-set-retrieve(-more)'.
ARG is the same as the raw prefix arg for `icicle-candidate-set-retrieve'.
MOREP non-nil means add the saved candidates, don't replace existing."
  (let ((name        nil)
        (variablep   (and arg  (atom arg)))
        (curr-cands  icicle-completion-candidates)
        saved-cands)
    (if arg
        (let ((icicle-whole-candidate-as-text-prop-p  nil)
              (enable-recursive-minibuffers           t))
          (if variablep
              ;; Retrieve from a variable.  Prompt user for the variable to use.
              (setq saved-cands
                    (append (and morep  curr-cands)
                            (symbol-value
                             (setq name  (intern
                                          (completing-read ; Variable name.
                                           "Retrieve candidates from variable: "
                                           icicle-saved-candidates-variables-obarray
                                           nil nil nil (if (boundp 'variable-name-history)
                                                           'variable-name-history
                                                         'icicle-variable-name-history)))))))
            ;; Retrieve from a persistent set (and save to `icicle-saved-completion-candidates').
            (setq name  (completing-read "Retrieve candidates from persistent set: "
                                         (if (and icicle-filesets-as-saved-completion-sets-flag
                                                  (featurep 'filesets)  filesets-data)
                                             (append filesets-data icicle-saved-completion-sets)
                                           icicle-saved-completion-sets)
                                         nil nil nil 'icicle-completion-set-history))
            (icicle-retrieve-candidates-from-set name)
            (setq saved-cands  (append (and morep  curr-cands) icicle-saved-completion-candidates))))
      ;; Retrieve from the default variable, `icicle-saved-completion-candidates'.
      (setq saved-cands  (append (and morep  curr-cands) icicle-saved-completion-candidates)))
    (cond ((null saved-cands)
           (deactivate-mark)
           (icicle-display-candidates-in-Completions)
           (message "No saved candidates to restore") (sit-for 2))
          (t
           (setq icicle-completion-candidates ; Remove directories if completing file names
                 (if (icicle-file-name-input-p) ; using `read-file-name'.
                     (mapcar #'file-name-nondirectory saved-cands)
                   saved-cands))
           (cond ((and (consp icicle-completion-candidates)  (null (cdr icicle-completion-candidates)))
                  ;; $$$$$$ Should this now use `icicle-current-input'
                  ;;        instead of (car icicle-completion-candidates), for PCM?
                  (icicle-remove-Completions-window)
                  (icicle-insert-completion (car icicle-completion-candidates)) ; Insert sole cand.
                  (minibuffer-message "  [Sole candidate restored]")
                  (save-selected-window (select-window (minibuffer-window))
                                        (icicle-highlight-complete-input))
                  (icicle-show-help-in-mode-line (car icicle-completion-candidates)))
                 ((consp icicle-completion-candidates)
                  (deactivate-mark)
                  (icicle-display-candidates-in-Completions)
                  (save-selected-window
                    (select-window (minibuffer-window))
                    (minibuffer-message (if name
                                            (format "  [Saved candidates RESTORED from %s `%s']"
                                                    (if variablep "variable" "cache file") name)
                                          "  [Saved candidates RESTORED]")))
                  (let ((icicle-minibuffer-setup-hook ; Pre-complete
                         (cons (if (eq icicle-last-completion-command
                                       'icicle-apropos-complete-no-display)
                                   'icicle-apropos-complete-no-display
                                 'icicle-apropos-complete)
                               icicle-minibuffer-setup-hook)))
                    (icicle-narrow-candidates))))))))

;;;###autoload (autoload 'icicle-candidate-set-retrieve-more "icicles")
(defun icicle-candidate-set-retrieve-more (&optional arg) ; Bound to `C-<' in minibuffer.
  "Retrieve a saved set of completion candidates.
The saved candidates are added to those already current.
A prefix argument acts as for `icicle-candidate-set-retrieve'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-more]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (icicle-candidate-set-retrieve-1 arg t))

;;;###autoload (autoload 'icicle-candidate-set-retrieve-from-variable "icicles")
(defun icicle-candidate-set-retrieve-from-variable () ; Bound to `C-M-{' in minibuffer.
  "Retrieve a saved set of completion candidates, making it current.
This retrieves candidates saved with `\\<minibuffer-local-completion-map>\
\\[icicle-save/unsave-candidate]', `M-S-mouse-2', or
`\\[icicle-candidate-set-save-to-variable]' (or `\\[icicle-candidate-set-save]' with a numeric \
prefix arg).

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-from-variable]')."
  (interactive)
  (icicle-candidate-set-retrieve 99))

;;;###autoload (autoload 'icicle-candidate-set-retrieve-persistent "icicles")
(defun icicle-candidate-set-retrieve-persistent () ; Bound to `C-{' in minibuffer.
  "Retrieve a saved set of completion candidates, making it current.
This retrieves candidates saved with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-persistently]' or `C-u \\[icicle-candidate-set-save]'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-persistent]')."
  (interactive)
  (icicle-candidate-set-retrieve '(1)))

(defun icicle-retrieve-candidates-from-set (set-name)
  "Retrieve the saved set of completion candidates named SET-NAME.
SET-NAME names an Icicles saved completion set or, if
 `icicle-filesets-as-saved-completion-sets-flag' is non-nil, an Emacs
 fileset.  If that option is non-nil and SET-NAME names a saved
 completion set that contains Emacs filesets, then the files specified
 for the filesets are also retrieved. 
The candidates are retrieved to `icicle-saved-completion-candidates',
and `icicle-candidates-alist' is updated."
  (setq icicle-saved-completion-candidates  (icicle-get-candidates-from-saved-set set-name))
  (when icicle-candidates-alist         ; Redefine `icicle-candidates-alist'.
    (let ((icicle-whole-candidate-as-text-prop-p  t))
      (setq icicle-candidates-alist  (mapcar icicle-get-alist-candidate-function
                                             icicle-saved-completion-candidates)))))

;;;###autoload (autoload 'icicle-save/unsave-candidate "icicles")
(defun icicle-save/unsave-candidate ()  ; Bound to `insert' in minibuffer.
  "Add/remove current candidate to/from `icicle-saved-completion-candidates'.
If the candidate is already saved, then unsave it; otherwise, save it.
You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-save/unsave-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (if (not (wholenump icicle-candidate-nb))
      (save-selected-window (select-window (minibuffer-window))
                            (minibuffer-message "  [No current candidate]"))
    (let ((cand  (elt icicle-completion-candidates icicle-candidate-nb)))
      (cond ((member cand icicle-saved-completion-candidates)
             (setq icicle-saved-completion-candidates
                   (delete icicle-last-completion-candidate icicle-saved-completion-candidates))
             (save-selected-window (select-window (minibuffer-window))
                                   (minibuffer-message "  [Candidate UNsaved]")))
            (t
             (push cand icicle-saved-completion-candidates)
             (save-selected-window (select-window (minibuffer-window))
                                   (minibuffer-message "  [Candidate SAVED]")))))))

;;;###autoload (autoload 'icicle-mouse-save/unsave-candidate "icicles")
(defun icicle-mouse-save/unsave-candidate (event) ; Bound to `M-S-mouse-2' in *Completions.
  "Add/remove clicked candidate to/from `icicle-saved-completion-candidates'.
If the candidate is already saved, then unsave it; otherwise, save it."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let (;; $$$$$$ (buffer    (window-buffer))
        (posn-win  (posn-window (event-start event)))
        (posn-col  (car (posn-col-row (event-start event))))
        (posn-row  (cdr (posn-col-row (event-start event))))
        choice base-size)
    (read-event)                        ; Swallow mouse up event.
    (with-current-buffer (window-buffer posn-win)
      (save-excursion
        ;; $$$$$$ (when completion-reference-buffer (setq buffer  completion-reference-buffer))
        (setq base-size  completion-base-size)
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp))  (get-text-property (point) 'mouse-face))
            (setq end  (point)
                  beg  (1+ (point))))
          (unless beg (error "No completion here"))
          (setq beg     (previous-single-property-change beg 'mouse-face)
                end     (or (next-single-property-change end 'mouse-face)  (point-max))
                choice  (buffer-substring-no-properties beg end)))))
    (setq icicle-candidate-nb               (icicle-nb-of-cand-at-Completions-pos
                                             (posn-point (event-start event)))
          icicle-last-completion-candidate  choice)
    (cond ((member icicle-last-completion-candidate icicle-saved-completion-candidates)
           (setq icicle-saved-completion-candidates
                 (delete icicle-last-completion-candidate icicle-saved-completion-candidates))
           (save-selected-window (select-window (minibuffer-window))
                                 (minibuffer-message "  [Candidate UNsaved]")))
          (t
           (push icicle-last-completion-candidate icicle-saved-completion-candidates)
           (save-selected-window (select-window (minibuffer-window))
                                 (minibuffer-message "  [Candidate SAVED]"))))
    (deactivate-mark)
    (icicle-display-candidates-in-Completions)
    (icicle-raise-Completions-frame posn-col posn-row)))

;;;###autoload (autoload 'icicle-mouse-candidate-set-save "icicles")
(defun icicle-mouse-candidate-set-save (ignore &optional arg) ; `M-S-mouse-3' in `*Completions*'.
  "`icicle-candidate-set-save(-selected)'.
If the region is active in `*Completions*', then
`icicle-candidate-set-save-selected'.  Otherwise,
`icicle-candidate-set-save'."
  (interactive "e\nP")
  (if (and (get-buffer "*Completions*")
           (save-current-buffer
             (set-buffer (get-buffer "*Completions*"))
             (and mark-active  (mark) (/= (point) (mark)))))
      (icicle-candidate-set-save-selected arg)
    (icicle-candidate-set-save arg)))

;;;###autoload (autoload 'icicle-mouse-candidate-set-save-more "icicles")
(defun icicle-mouse-candidate-set-save-more (ignore &optional arg) ; `M-mouse-3' in `*Completions*'.
  "`icicle-candidate-set-save-more(-selected)'.
If the region is active in `*Completions*', then
`icicle-candidate-set-save-more-selected'.  Otherwise,
`icicle-candidate-set-save-more'."
  (interactive "e\nP")
  (if (and (get-buffer "*Completions*")
           (save-current-buffer
             (set-buffer (get-buffer "*Completions*"))
             (and mark-active  (mark)  (/= (point) (mark)))))
      (icicle-candidate-set-save-more-selected arg)
    (icicle-candidate-set-save-more arg)))

;;; `mouse-3' in `*Completions*'.
(cond ((require 'mouse3 nil t)
       (defun icicle-mouse-save-then-kill (click &optional arg)
         "`mouse-save-then-kill', but click same place saves selected candidates."
         (interactive "e\nP")
         (let ((mouse3-save-then-kill-command  `(lambda (event prefix-arg)
                                                 (icicle-mouse-candidate-set-save-more nil ,arg))))
           (mouse-save-then-kill click))
         (setq this-command  'mouse-save-then-kill)))

      ((< emacs-major-version 24)
       (defun icicle-mouse-save-then-kill (click &optional arg)
         "`mouse-save-then-kill', but click same place saves selected candidates."
         (interactive "e\nP")
         (flet ((mouse-save-then-kill-delete-region (beg end)
                  (icicle-mouse-candidate-set-save-more nil arg)))
           (mouse-save-then-kill click))
         (setq this-command  'mouse-save-then-kill)))

      (t
       ;; The only thing Icicles-specific here is replacing killing or deleting the region by a call to
       ;; `icicle-mouse-candidate-set-save-more'.  Otherwise, this is just `mouse-save-then-kill'.
       (defun icicle-mouse-save-then-kill (click &optional arg) ; `mouse-3' in `*Completions*'.
         "`mouse-save-then-kill', but click same place saves selected candidates."
         (interactive "e\nP")
         (mouse-minibuffer-check click)
         (let* ((posn          (event-start click))
                (click-pt      (posn-point posn))
                (window        (posn-window posn))
                (buf           (window-buffer window))
                (this-command  this-command) ; Don't let subsequent kill cmd append to this one.
                ;; Check if the user has multi-clicked to select words/lines.
                (click-count   (if (and (eq mouse-selection-click-count-buffer buf)
                                        (with-current-buffer buf (mark t)))
                                   mouse-selection-click-count
                                 0)))
           (cond ((not (numberp click-pt)) nil)
                 ((and (eq last-command 'icicle-mouse-save-then-kill) ; User clicked at same position.
                       (eq click-pt mouse-save-then-kill-posn)
                       (eq window (selected-window)))
                  ;; Here is the Icicles difference from vanilla `mouse-save-then-kill'.
                  ;; Instead of killing/deleting the region, save the selected candidates.
                  (icicle-mouse-candidate-set-save-more nil arg)
                  (setq mouse-selection-click-count  0
                        mouse-save-then-kill-posn    nil))
                 ;; If there is a suitable region, adjust it by moving the closest end to CLICK-PT.
                 ((or (with-current-buffer buf (region-active-p))
                      (and (eq window (selected-window))
                           (mark t)
                           (or (and (eq last-command 'icicle-mouse-save-then-kill)
                                    mouse-save-then-kill-posn)
                               (and (memq last-command '(mouse-drag-region mouse-set-region))
                                    (or mark-even-if-inactive  (not transient-mark-mode))))))
                  (select-window window)
                  (let* ((range  (mouse-start-end click-pt click-pt click-count)))
                    (if (< (abs (- click-pt (mark t))) (abs (- click-pt (point))))
                        (set-mark (car range))
                      (goto-char (nth 1 range)))
                    (setq deactivate-mark  nil)
                    (mouse-set-region-1)
                    (when mouse-drag-copy-region
                      ;; Previous region was copied to kill-ring, so replace with adjusted region.
                      (kill-new (filter-buffer-substring (mark t) (point)) t))
                    (setq mouse-save-then-kill-posn  click-pt))) ; Repeated `mouse-3' kills region.
                 (t                     ; Set the mark where point is and move to CLICK-PT.
                  (select-window window)
                  (mouse-set-mark-fast click)
                  (let ((before-scroll (with-current-buffer buf point-before-scroll)))
                    (when before-scroll (goto-char before-scroll)))
                  (exchange-point-and-mark)
                  (mouse-set-region-1)
                  (when mouse-drag-copy-region (kill-new (filter-buffer-substring (mark t) (point))))
                  (setq mouse-save-then-kill-posn  click-pt)))))))

;;;###autoload (autoload 'icicle-candidate-set-save "icicles")
(defun icicle-candidate-set-save (&optional arg) ; Bound to `C-M->' in minibuffer.
  "Save the set of current completion candidates, for later recall.
Saves candidates in variable `icicle-saved-completion-candidates', by
default.
With a plain prefix arg (`C-u'), save candidates in a cache file.
With a non-zero numeric prefix arg (`C-u N'), save candidates in a
 variable for which you are prompted.
With a zero prefix arg (`C-0'), save candidates in a fileset (Emacs 22
 or later).  Use this only for file-name candidates, obviously.  To
 subsequently use a fileset for candidate retrieval, option
 `icicle-filesets-as-saved-completion-sets-flag' must be non-nil.

You can retrieve the saved set of candidates with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (icicle-candidate-set-save-1 icicle-completion-candidates arg))

;;;###autoload (autoload 'icicle-candidate-set-save-more "icicles")
(defun icicle-candidate-set-save-more (&optional arg) ; Bound to `C->' in minibuffer.
  "Add current completion candidates to saved candidates set.
The current candidates are added to those already saved.
A prefix argument acts the same as for `icicle-candidate-set-save'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-more]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (icicle-candidate-set-save-1 icicle-completion-candidates arg t))

;;;###autoload (autoload 'icicle-candidate-set-save-selected "icicles")
(defun icicle-candidate-set-save-selected (&optional arg) ; Bound to `C-M-)' in minibuffer.
  "`icicle-candidate-set-save', but only for the selected candidates.
Candidates at least partially in the region are saved.
A prefix argument acts the same as for `icicle-candidate-set-save'.

As a special case, if no candidates are selected, then this empties
the current set of saved candidates.  That is, it UNsaves all saved
candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-selected]')."
  (interactive "P")
  (icicle-candidate-set-save-selected-1 arg nil 'no-error))

;;;###autoload (autoload 'icicle-candidate-set-save-more-selected "icicles")
(defun icicle-candidate-set-save-more-selected (&optional arg) ; Bound to `C-)' in minibuffer.
  "`icicle-candidate-set-save-more', but only for the selected candidates.
Candidates at least partially in the region are added to those saved.
A prefix argument acts the same as for `icicle-candidate-set-save'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-more-selected]')."
  (interactive "P")
  (icicle-candidate-set-save-selected-1 arg t))

;; $$$$$$$ Maybe should also allow rectangle selection.
(defun icicle-candidate-set-save-selected-1 (arg &optional morep no-error-p)
  "Helper function for `icicle-candidate-set-save(-more)(-region)'."
  (when (or (get-buffer-window "*Completions*" 0)  no-error-p)
    (let ((beg-cand-nb       0)
          (end-cand-nb       0)
          (candidates        ())
          (icicle-orig-buff  (current-buffer)))
      (when (get-buffer-window "*Completions*" 0) ; Do nothing if not displayed.
        (with-current-buffer "*Completions*"
          (when (and mark-active  (mark)  (/= (point) (mark))  icicle-completion-candidates)
            (let ((bob  (icicle-start-of-candidates-in-Completions))
                  (eob  (point-max))
                  (beg  (region-beginning))
                  (end  (region-end))
                  temp)

              ;; Extend region ends to include all of first and last selected candidates.
              (unless (get-text-property beg 'mouse-face)
                (if (setq temp  (next-single-property-change beg 'mouse-face))
                    (setq beg  temp)
                  (setq beg  (next-single-property-change temp 'mouse-face))))

              (when (> beg end)
                (error "No candidates selected")) ; Active region but none selected.

              (unless (get-text-property end 'mouse-face)
                (if (setq temp  (previous-single-property-change end 'mouse-face))
                    (setq end  temp)
                  (setq end  (previous-single-property-change temp 'mouse-face))))
              (when (> beg end) (setq beg  (prog1 end (setq end  beg)))) ; Swap them.
              (while (and (>= beg bob)  (get-text-property beg 'mouse-face)) (setq beg  (1- beg)))
              (while (and (<= end eob)  (get-text-property end 'mouse-face)) (setq end  (1+ end)))
              (setq beg          (1+ beg)
                    end          (1- end)
                    beg-cand-nb  (icicle-nb-of-cand-at-Completions-pos beg)
                    end-cand-nb  (icicle-nb-of-cand-at-Completions-pos end))
              (when (> beg-cand-nb end-cand-nb) ; Swap them
                (setq beg-cand-nb  (prog1 end-cand-nb (setq end-cand-nb  beg-cand-nb))))
              (while (<= beg-cand-nb end-cand-nb)
                (push (elt icicle-completion-candidates beg-cand-nb) candidates)
                (setq beg-cand-nb  (1+ beg-cand-nb)))))))
      (when (and morep  (null candidates)) (error "No candidates selected")) ; Need selection for MOREP.
      (setq candidates  (nreverse candidates))
      (icicle-candidate-set-save-1 candidates arg morep t no-error-p)
      (let ((win  (get-buffer-window icicle-orig-buff 'visible)))
        (when win (select-window win))))))


;;; Note: This can be called from the minibuffer or top level.
;;;       It cannot make any assumptions about the selected window being the active minibuffer etc.
;;;
(defun icicle-candidate-set-save-1 (new-cands arg &optional morep only-selected-p no-error-p)
  "Helper function for `icicle-candidate-set-save*' functions.
NEW-CANDS are the candidates to save.
ARG is the same as the raw prefix arg for `icicle-candidate-set-save'.
MOREP non-nil means add the candidates, do not replace existing set.
ONLY-SELECTED-P non-nil means NEW-CANDS are those selected in
 `*Completions*'.
NO-ERROR-P non-nil means don't raise an error if NEW-CANDS is nil."
  (unless (or new-cands  no-error-p)
    (error "Cannot save empty candidates set - did you use `S-TAB' or `TAB'?"))
  (let ((in-minibuf-p  (minibuffer-window-active-p (minibuffer-window)))
        where)
    (if arg
        (let ((enable-recursive-minibuffers  t))
          (cond ((consp arg)
                 ;; Save to cache file (and to `icicle-saved-completion-candidates').
                 (let* ((file-name
                         (prog1 (let ((icicle-completion-candidates  icicle-completion-candidates))
                                  (icicle-add/update-saved-completion-set))
                           (when in-minibuf-p
                             (with-output-to-temp-buffer "*Completions*" ; Redisplay.
                               (display-completion-list icicle-completion-candidates)))))
                           ;;; $$$$$$ (select-window (minibuffer-window))
                        (list-buf   (and morep  (find-file-noselect file-name 'nowarn 'raw)))
                        (old-cands  ()))
                   (when morep
                     (unwind-protect
                          (condition-case nil
                              (setq old-cands  (read list-buf))
                            (end-of-file
                             (icicle-msg-maybe-in-minibuffer "No completion candidates in file `%s'"
                                                             file-name)))
                       (kill-buffer list-buf)))
                   ;; Convert to readable alist form, from propertized text.  Convert any markers
                   ;; to the form (icicle-file-marker FILE POS) or (icicle-marker BUFFER POS).
                   (when (and new-cands  (get-text-property 0 'icicle-whole-candidate (car new-cands)))
                     (setq new-cands
                           (mapcar (lambda (cand)
                                     (icicle-markers-to-readable
                                      (or (funcall icicle-get-alist-candidate-function cand)  cand)))
                                   new-cands)))
                   (setq icicle-saved-completion-candidates  (append new-cands old-cands)
                         where                               (format "cache file `%s'" file-name))
                   (with-temp-message (format "Writing candidates to cache file `%s'..." file-name)
                     (condition-case err
                         (with-temp-file file-name
                           (prin1 icicle-saved-completion-candidates (current-buffer)))
                       (error (error "Could not write to cache file.  %s"
                                     (error-message-string err)))))))
                ((zerop (prefix-numeric-value arg))
                 ;; Save to a fileset (and to `icicle-saved-completion-candidates').
                 (unless (require 'filesets nil t)
                   (error "Cannot save to a fileset - feature `filesets' not provided"))
                 (filesets-init)
                 (let ((icicle-completion-candidates  icicle-completion-candidates))
                   (setq where  (completing-read "Save to fileset: " filesets-data)))
                 (unless (assoc where filesets-data)
                   (if (not (y-or-n-p (format "Fileset `%s' does not exist. Create it? " where)))
                       (error "Operation cancelled - no fileset")
                     (add-to-list 'filesets-data (list where (list :files)))
                     (icicle-msg-maybe-in-minibuffer
                      "Fileset created.  Use `M-x filesets-save-config' to save it.")))
                 (dolist (cand  new-cands) (icicle-add-file-to-fileset cand where))
                 (when in-minibuf-p
                   (with-output-to-temp-buffer "*Completions*" ; Redisplay.
                     (display-completion-list icicle-completion-candidates)))
                 ;; $$$$$$ (select-window (minibuffer-window))
                 (setq where  (format "`%s'" where)))
                (t                      ; Save to a variable.  Prompt for the variable to use.
                 (let* ((varname
                         (prog1
                             (let ((icicle-completion-candidates           icicle-completion-candidates)
                                   (icicle-whole-candidate-as-text-prop-p  nil))
                               (completing-read (if morep
                                                    "Add candidates to variable: "
                                                  "Save candidates in variable: ")
                                                icicle-saved-candidates-variables-obarray
                                                nil nil nil (if (boundp 'variable-name-history)
                                                                'variable-name-history
                                                              'icicle-variable-name-history)
                                                "icicle-saved-completion-candidates"))
                           (when in-minibuf-p
                             (with-output-to-temp-buffer "*Completions*"
                               (display-completion-list icicle-completion-candidates)))))
                        ;; $$$$$$ (select-window (minibuffer-window))
                        (var  (intern varname))) ; Intern in standard `obarray'.
                   (intern varname icicle-saved-candidates-variables-obarray) ; For completion.
                   (set var (if (and morep  (boundp var)  (listp (symbol-value var)))
                                (append new-cands (symbol-value var))
                              new-cands))
                   (setq where  (format "`%s'" var))))))
      ;; Save to default variable, `icicle-saved-completion-candidates'.
      (setq where  "`icicle-saved-completion-candidates'"
            icicle-saved-completion-candidates
            (if (and morep  (listp icicle-saved-completion-candidates))
                (append new-cands icicle-saved-completion-candidates)
              new-cands)))
    (deactivate-mark)
    (when (and in-minibuf-p  (get-buffer-window "*Completions*" 'visible))
      (icicle-display-candidates-in-Completions))
    (icicle-msg-maybe-in-minibuffer
     (if morep
         (if new-cands
             (format "%sandidates ADDED to %s" (if only-selected-p "Selected c" "C") where)
           "  [NO candidates selected to add]")
       (if new-cands
           (format "%sandidates SAVED to %s" (if only-selected-p "Selected c" "C") where)
         "Saved candidates reset to NONE")))))

;; This is actually a top-level command, but it is in this file because it is used by
;; `icicle-candidate-set-save-1'.
;;
;; We don't define this using `icicle-define-add-to-alist-command', because we want to
;; return the cache-file name.
;;
;;;###autoload (autoload 'icicle-add/update-saved-completion-set "icicles")
(defun icicle-add/update-saved-completion-set ()
  "Add or update an entry in `icicle-saved-completion-sets'.
That is, create a new saved completion set or update an existing one.
You are prompted for the name of a set of completion candidates and
its cache file.  By default, the cache file name is the set name
without spaces, and with file extension `icy'.  List
`icicle-saved-completion-sets' is updated to have an entry with these
set and file names.  Return the cache-file name."
  (interactive)
  (let* ((icicle-whole-candidate-as-text-prop-p  nil)
         (set-name                               (icicle-substring-no-properties
                                                  (completing-read
                                                   "Saved completion set: "
                                                   icicle-saved-completion-sets nil nil nil
                                                   'icicle-completion-set-history)))
         (file-name                              ""))
    (setq file-name  (expand-file-name
                      (read-file-name "Cache file for the set: " default-directory nil nil
                                      (concat (icicle-delete-whitespace-from-string set-name) ".icy"))))
    (while (not (icicle-file-writable-p file-name))
      (setq file-name  (expand-file-name
                        (read-file-name
                         "Cannot write to that file. Cache file: " default-directory nil nil
                         (concat (icicle-delete-whitespace-from-string set-name) ".icy")))))
    (setq icicle-saved-completion-sets  ; Remove any old definition of this set.
          (icicle-assoc-delete-all set-name icicle-saved-completion-sets))
    (push (cons set-name file-name) icicle-saved-completion-sets) ; Add new set definition.
    (funcall icicle-customize-save-variable-function
             'icicle-saved-completion-sets
             icicle-saved-completion-sets)
    (message "Added set to `icicle-saved-completion-sets': `%s'" set-name)
    file-name))                         ; Return cache-file name.

;; Similar to `filesets-add-buffer', but that insists on a buffer.  This is actually a top-level
;; command, but it is in this file because it is used by `icicle-candidate-set-save-1'.
;;
;;;###autoload (autoload 'icicle-add-file-to-fileset "icicles")
(defun icicle-add-file-to-fileset (&optional file name)
  "Add FILE to the fileset called NAME.
If FILE is nil, use file of current buffer.
If NAME is nil, prompt for the fileset."
  (interactive)
  (unless (require 'filesets nil t) (error "Cannot find library `filesets'"))
  (setq file  (or file  (buffer-file-name)  (and (interactive-p)
                                                 (read-file-name "File to add: " nil nil t))
                  (error "Current buffer has no associated file"))
        name  (or name  (and (interactive-p)
                             (completing-read (format "Add `%s' to fileset: " file) filesets-data))
                  (error "No fileset")))
  (let ((entry  (or (assoc name filesets-data)
                    (and (interactive-p)
                         (when (y-or-n-p (format "Fileset `%s' does not exist. Create it? " name))
                           (add-to-list 'filesets-data (list name (list :files)))
                           (message "Fileset created.  Use `M-x filesets-save-config' to save it.")
                           (car filesets-data))))))
    (if (not entry)
        (when (interactive-p) (message "Operation cancelled - no fileset"))
      (let* ((files  (filesets-entry-get-files entry)))
        (cond ((filesets-member file files :test 'filesets-files-equalp)
               (message "`%s' is already in fileset `%s'" file name))
              ((and file  (eq (filesets-entry-mode entry) ':files))
               (filesets-entry-set-files entry (cons file files) t)
               (filesets-set-config name 'filesets-data filesets-data))
              (t (error "Cannot add file. Fileset `%s' is not of type Files (:files)" name)))))))

(defun icicle-markers-to-readable (cand)
  "Convert (serialize) candidate CAND to Lisp-readable representation.
CAND is a full completion candidate (collection alist entry).
A Lisp-readable candidate uses one of the following forms to represent
a marker:
  (icicle-file-marker  FILE-NAME    MARKER-POSITION)
  (icicle-marker       BUFFER-NAME  MARKER-POSITION)"
  (if (atom cand)
      (if (markerp cand)
          (let ((buf  (marker-buffer cand)))
            (unless buf (error "Marker in no buffer"))
            (list (if (buffer-file-name buf) 'icicle-file-marker 'icicle-marker)
                  (or (buffer-file-name buf)  (buffer-name buf))
                  (marker-position cand)))
        cand)
    (cons (icicle-markers-to-readable (car cand)) (icicle-markers-to-readable (cdr cand)))))

;;;###autoload (autoload 'icicle-candidate-set-save-to-variable "icicles")
(defun icicle-candidate-set-save-to-variable () ; Bound to `C-M-}' in minibuffer.
  "Save the set of current completion candidates in a variable you choose.
You can retrieve the saved set of candidates with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-from-variable]' (or `\\[icicle-candidate-set-retrieve]'
with a numeric prefix arg).
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-to-variable]')."
  (interactive)
  (icicle-candidate-set-save 99))

;;;###autoload (autoload 'icicle-candidate-set-save-persistently "icicles")
(defun icicle-candidate-set-save-persistently (filesetp) ; Bound to `C-}' in minibuffer.
  "Save the set of current completion candidates persistently.
With no prefix arg, save in a cache file.
With a prefix arg, save in an Emacs fileset (Emacs 22 or later).

You can retrieve the saved set of candidates with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-persistent]' or `C-u \\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-persistently]')."
  (interactive "P")
  (icicle-candidate-set-save (if filesetp 0 '(1))))

;;;###autoload (autoload 'icicle-keep-only-past-inputs "icicles")
(defun icicle-keep-only-past-inputs (&optional recent-first) ; Bound to`M-pause' in minibuffer.
  "Narrow completion candidates to those that have been used previously.
This filters the set of current completion candidates, keeping only
those that have been used before.  (You must first use `TAB' or
`S-TAB' to establish an explicit candidate set.)

With a prefix arg, the previous inputs are sorted chronologically,
most recent first.

Note that whatever completion mode (prefix or apropos) was in effect
before you use `\\<minibuffer-local-completion-map>\
\\[icicle-keep-only-past-inputs]' remains in \ effect for
`icicle-keep-only-past-inputs'.  This command does not use a recursive
minibuffer; it simply co-opts the current completion, changing it to
completion against the history.

You can use this command only from the minibuffer \
\(`\\[icicle-keep-only-past-inputs]').

See also `\\[icicle-history]' (`icicle-history')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (if (and recent-first  (interactive-p)  icicle-inhibit-sort-p)
      (icicle-msg-maybe-in-minibuffer "Cannot sort candidates now")
    (let ((icicle-sort-comparer  (if recent-first 'icicle-most-recent-first-p icicle-sort-comparer)))
      (when (or recent-first  (eq icicle-last-completion-command 'icicle-keep-only-past-inputs))
        (icicle-complete-again-update 'no-display))
      (if (null icicle-completion-candidates)
          (minibuffer-message "  [No completion candidates to filter]")
        (unless (boundp minibuffer-history-variable) (set minibuffer-history-variable nil))
        (when (consp (symbol-value minibuffer-history-variable))
          (setq icicle-completion-candidates
                (lexical-let* ((filep    (or (icicle-file-name-input-p)  icicle-abs-file-candidates))
                               (dir      (and filep
                                              icicle-last-input
                                              (icicle-file-name-directory icicle-last-input)))
                               (histvar  (and (symbolp minibuffer-history-variable)
                                              (boundp minibuffer-history-variable)
                                              minibuffer-history-variable))
                               (hist     (and histvar
                                              (if filep
                                                  (let ((default-directory  dir))
                                                    (mapcar #'expand-file-name (symbol-value histvar)))
                                                (symbol-value histvar)))))
                  (icicle-remove-if-not
                   (lambda (candidate)
                     (if filep
                         (let ((default-directory  dir))  (member (expand-file-name candidate) hist))
                       (member candidate hist)))
                   icicle-completion-candidates)))
          (cond ((null icicle-completion-candidates)
                 (save-selected-window (icicle-remove-Completions-window))
                 (minibuffer-message "  [None of the completions have been used before]"))
                (t
                 (cond ((icicle-get-safe last-command 'icicle-cycling-command)
                        (setq icicle-current-input  icicle-last-input)
                        (icicle-retrieve-last-input))
                       (t
                        (setq icicle-current-input  (icicle-input-from-minibuffer))))
                 (cond ((null icicle-completion-candidates)
                        (setq icicle-nb-of-other-cycle-candidates  0)
                        (save-selected-window (icicle-remove-Completions-window))
                        (minibuffer-message "  [No matching history element]"))
                       ((null (cdr icicle-completion-candidates)) ; Single cand. Update minibuffer.
                        (setq icicle-nb-of-other-cycle-candidates  0)
                        (icicle-clear-minibuffer)
                        (setq icicle-last-completion-candidate  (car icicle-completion-candidates))
                        (let ((inserted  (if (and (icicle-file-name-input-p) insert-default-directory
                                                  (or (not (member icicle-last-completion-candidate
                                                                   icicle-extra-candidates))
                                                      icicle-extra-candidates-dir-insert-p))
                                             (icicle-abbreviate-or-expand-file-name
                                              icicle-last-completion-candidate
                                              (icicle-file-name-directory-w-default
                                               icicle-current-input))
                                           icicle-last-completion-candidate)))
                          (insert inserted))
                        (save-selected-window (icicle-remove-Completions-window))
                        (icicle-highlight-complete-input)
                        (icicle-show-help-in-mode-line icicle-last-completion-candidate)
                        (minibuffer-message (format "  [One matching history element]")))
                       (t
                        (when (member icicle-current-input icicle-completion-candidates)
                          (icicle-highlight-complete-input)
                          (icicle-show-help-in-mode-line icicle-current-input))
                        (icicle-display-candidates-in-Completions)
                        (save-window-excursion
                          (select-window (active-minibuffer-window))
                          (minibuffer-message
                           (concat "  [Filtered to (matching) historical candidates"
                                   (and recent-first  ", most recent first")
                                   "]")))))
                 (setq icicle-last-completion-command  'icicle-keep-only-past-inputs)))))
      icicle-completion-candidates)))

;;;###autoload (autoload 'icicle-other-history "icicles")
(defun icicle-other-history (arg)       ; Bound to `C-M-pause' in minibuffer.
  "Choose a history, or complete against `icicle-interactive-history'.
For Emacs 23 or later, if no prefix arg and you are completing a
command, abbrev, or keyboard macro name, then complete against
\(non-nil) `icicle-interactive-history'.

Otherwise, prompt with completion for a minibuffer history to use.
The history choice lasts only for the current (main) minibuffer
reading.  You can then cycle through that history or use \
\\<minibuffer-local-map>`\\[icicle-insert-history-element]' to
complete against it."
  (interactive "P")
  (if (and (> emacs-major-version 22)
           (memq minibuffer-history-variable
                 '(extended-command-history icicle-command-abbrev-history icicle-kmacro-history))
           (not arg)
           icicle-interactive-history)
      (icicle-use-interactive-command-history)
    (call-interactively #'icicle-change-history-variable)))

;;;###autoload (autoload 'icicle-use-interactive-command-history "icicles")
(defun icicle-use-interactive-command-history ()
  "Complete input against `icicle-interactive-history'.
This is a history of all Emacs commands called interactively.
This history is available only for Emacs 23 and later, and only if
option `icicle-populate-interactive-history-flag' is not nil."
  (interactive)
  (icicle-change-history-variable "icicle-interactive-history")
  (icicle-history))

;;;###autoload (autoload 'icicle-change-history-variable "icicles")
(defun icicle-change-history-variable (hist-var)
  "Choose a history variable to use now for `minibuffer-history-variable'.
Use completion to choose the history to use.
The choice lasts only for the current (main) completion.
Non-interactively, arg HIST-VAR is the (string) name of a history var."
  (interactive
   (let ((enable-recursive-minibuffers  t)
         (icicle-hist-vars              `((,(symbol-name minibuffer-history-variable))
                                          (,(symbol-name 'icicle-previous-raw-file-name-inputs))
                                          (,(symbol-name 'icicle-previous-raw-non-file-name-inputs)))))
     (when (and (boundp 'icicle-populate-interactive-history-flag) ; Emacs 23+.
                icicle-populate-interactive-history-flag)
       (push (symbol-name 'icicle-interactive-history)  icicle-hist-vars))
     (mapatoms (lambda (x) (when (and (boundp x)  (consp (symbol-value x))
                                      (stringp (car (symbol-value x)))
                                      (string-match "-\\(history\\|ring\\)\\'" (symbol-name x)))
                             (push (list (symbol-name x)) icicle-hist-vars))))
     (list (completing-read "Use history: " icicle-hist-vars nil t nil nil nil))))
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (setq minibuffer-history-variable  (intern hist-var)))

;;;###autoload (autoload 'icicle-scroll-forward "icicles")
(defun icicle-scroll-forward (&optional arg) ; `C-M-v' in minibuffer.
  "Scroll `icicle-other-window' forward."
  (interactive "P")
  (let ((win  (if (window-live-p icicle-other-window)
                  icicle-other-window
                (if (window-live-p icicle-orig-window)
                    icicle-orig-window
                  (get-buffer-window "*Completions*" 0)))))
    (when win (save-selected-window (select-window win) (scroll-up arg)))))

;;;###autoload (autoload 'icicle-scroll-backward "icicles")
(defun icicle-scroll-backward (&optional arg) ; `C-M-S-v' (aka `C-M-V') in minibuffer.
  "Scroll `icicle-other-window' backward."
  (interactive "P")
  (let ((win  (if (window-live-p icicle-other-window)
                  icicle-other-window
                (if (window-live-p icicle-orig-window)
                    icicle-orig-window
                  (get-buffer-window "*Completions*" 0)))))
    (when win (save-selected-window (select-window win) (scroll-down arg)))))

;;;###autoload (autoload 'icicle-scroll-Completions-forward "icicles")
(defun icicle-scroll-Completions-forward (&optional reverse) ; `C-v' minib; `wheel-down' *Completions*.
  "Scroll the `*Completions*' window forward.
With a prefix argument, or if `icicle-scroll-Completions-reverse-p' is
non-nil, scroll backward."
  (interactive "P")
  (when (get-buffer-window "*Completions*" 0)
    (save-selected-window
      (select-window (get-buffer-window "*Completions*" 0))
      (when (if (interactive-p) reverse current-prefix-arg) ; Non-interactive use is for `TAB', `S-TAB'.
        (setq icicle-scroll-Completions-reverse-p  (not icicle-scroll-Completions-reverse-p)))
      (cond (icicle-scroll-Completions-reverse-p
             (if (not (= (window-start) (point-min)))
                 (scroll-down nil)
               (unless (= (window-end) (point-max))
                 (goto-char (point-max))
                 (scroll-down (1- (/ (window-height) 2)))
                 (beginning-of-line))))
            (t
             (if (not (= (window-end) (point-max)))
                 (scroll-up nil)
               (unless (= (window-start) (point-min))
                 (goto-char (icicle-start-of-candidates-in-Completions)))))))))

;;;###autoload (autoload 'icicle-scroll-Completions-backward "icicles")
(defun icicle-scroll-Completions-backward () ; `M-v' in minibuf; `wheel-up' in `*Completions*'.
  "Scroll the `*Completions*' window backward.
If `icicle-scroll-Completions-reverse-p' is non-nil, scroll forward."
  (interactive)
  (let ((icicle-scroll-Completions-reverse-p  (not icicle-scroll-Completions-reverse-p)))
    (icicle-scroll-Completions-forward)))


;; Consider `icicle-history' as both kinds of completion command,
;; so that a first `TAB' or `S-TAB' cycles, depending on `icicle-default-cycling-mode'.
(put 'icicle-history 'icicle-cycling-command            t)
(put 'icicle-history 'icicle-prefix-completing-command  t)
(put 'icicle-history 'icicle-apropos-completing-command t)

;;;###autoload (autoload 'icicle-history "icicles")
(defun icicle-history ()                ; Bound to `M-h' in minibuffer.
  "Access the appropriate history list using completion or cycling.
Complete the current minibuffer input against items in the history
list that is in use for the current command.

NOTE:

1. If the required input is a file or directory name, then the entire
minibuffer input is what is matched against the history list.  The
reason for this is that file names in the history list are usually
absolute.  This is unlike the case for normal file-name completion,
which assumes the default directory.

Keep this in mind for apropos (regexp) completion; it means that to
match a file-name using a substring you must, in the minibuffer,
either not specify a directory or explicitly use \".*\" before the
file-name substring.

For example, `/foo/bar/lph' will not apropos-match the previously
input file name `/foo/bar/alphabet-soup.el'; you should use either
`/foo/bar/.*lph' or `lph' (no directory).

2. This also represents a difference in behavior compared to the
similar command `icicle-keep-only-past-inputs' \
\(`\\<minibuffer-local-completion-map>\
\\[icicle-keep-only-past-inputs]' in the
minibuffer).  That command simply filters the current set of
completion candidates, which in the case of file-name completion is a
set of relative file names.

3. Whatever completion mode (prefix or apropos) was in effect before
you use `\\<minibuffer-local-completion-map>\ \\[icicle-history]' remains in \
effect for `icicle-history'.  This command
does not use a recursive minibuffer; it simply co-opts the current
completion, changing it to completion against the history.

You can use this command only from the minibuffer \
\(`\\[icicle-history]').

See also `\\[icicle-keep-only-past-inputs]' (`icicle-keep-only-past-inputs')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (when (icicle-file-name-input-p)
    (setq minibuffer-completion-predicate  nil
          minibuffer-completing-file-name  nil))
  (when (and (arrayp minibuffer-completion-table)  minibuffer-completion-predicate)
    (setq minibuffer-completion-predicate
          `(lambda (elt) (funcall ',minibuffer-completion-predicate
                          (intern (if (consp elt) (car elt) elt))))))
  (when (and (boundp minibuffer-history-variable)  (consp (symbol-value minibuffer-history-variable)))
    (setq minibuffer-completion-table
          (mapcar #'list (icicle-remove-duplicates
                          ;; `command-history' is an exception: its entries are not strings.
                          (if (eq 'command-history minibuffer-history-variable)
                              (mapcar #'prin1-to-string (symbol-value minibuffer-history-variable))
                            (symbol-value minibuffer-history-variable))))))
  (save-selected-window
    (unless icicle-last-completion-command
      (setq icicle-last-completion-command  (case icicle-default-cycling-mode
                                              (apropos    'icicle-apropos-complete)
                                              (otherwise  'icicle-prefix-complete))) ; Prefix, by default.
      (funcall icicle-last-completion-command)))
  (cond (icicle-cycling-p;; $$$ (icicle-get-safe last-command 'icicle-cycling-command)
         (setq icicle-current-input  icicle-last-input)
         (icicle-retrieve-last-input))
        (t
         (setq icicle-current-input  (icicle-input-from-minibuffer)
               icicle-last-input     nil ; So `icicle-save-or-restore-input' thinks input has changed.
               ;; $$$$ last-command          'icicle-history
               )
         (funcall icicle-last-completion-command))))

;; Not actually a minibuffer command, since `isearch' technically uses the echo area.  This is not
;; shadowed by any `icicle-mode-map' binding, since `isearch-mode-map' is also a minor mode map.
;;
;;;###autoload (autoload 'icicle-isearch-complete "icicles")
(defun icicle-isearch-complete ()       ; Bound to `M-TAB' and `M-o' in `isearch-mode-map'.
  "Complete the search string using candidates from the search ring."
  (interactive)
  (cond ((icicle-completing-p)          ; Cannot use the var here, since not sure to be in minibuf.
         (setq isearch-string  (if (fboundp 'field-string) (field-string) (buffer-string)))
         (when (icicle-isearch-complete-past-string)
           (if (fboundp 'delete-field) (delete-field) (erase-buffer))
           (insert isearch-string)))
        (t
         (icicle-isearch-complete-past-string)
         (setq isearch-message  (mapconcat 'isearch-text-char-description isearch-string ""))
         (isearch-edit-string))))

(when (fboundp 'text-scale-increase)    ; Bound to `C-x -' in the minibuffer (Emacs 23+).
  (defun icicle-doremi-zoom-Completions+ (&optional increment)
    "Zoom the text in buffer `*Completions*' incrementally.
Use `=', `-', or the mouse wheel to increase or decrease text
size.  You can use the `Meta' key (`M-=' or `M--') to increment in
larger steps."
    (interactive "p")
    (unless (require 'doremi-frm nil t) (error "This command needs library `doremi-frm.el'."))
    (unless (get-buffer-window "*Completions*" 'visible)
      (if icicle-completion-candidates
          (icicle-display-candidates-in-Completions)
        (icicle-msg-maybe-in-minibuffer "Did you hit `TAB' or `S-TAB'?")))
    (let ((mini  (active-minibuffer-window)))
      (unwind-protect
           (save-selected-window
             (select-window (get-buffer-window "*Completions*" 'visible))
             (let ((enable-recursive-minibuffers  t)
                   (doremi-up-keys                '(?=))
                   (doremi-down-keys              '(?-))
                   (doremi-boost-up-keys          '(?\M-=))
                   (doremi-boost-down-keys        '(?\M--)))
               (doremi-buffer-font-size+ increment))
             (setq unread-command-events  ()))
        (unless mini (icicle-remove-Completions-window))))))

;;;###autoload (autoload 'icicle-doremi-candidate-width-factor+ "icicles")
(defun icicle-doremi-candidate-width-factor+ (&optional increment) ; Bound to `C-x w' in minibuffer.
  "Change `icicle-candidate-width-factor' incrementally.
Use `right', `left' or mouse wheel to increase or decrease.  You can
use the `Meta' key (e.g. `M-right') to increment in larger steps.

Use `up', `down', or the mouse wheel to adjust
`icicle-inter-candidates-min-spaces'."
  (interactive "p")
  (unless (require 'doremi nil t) (error "This command needs library `doremi.el'."))
  (let ((mini  (active-minibuffer-window)))
    (unwind-protect
         (save-selected-window
           (select-window (minibuffer-window))
           (unless icicle-completion-candidates (message "Hit `TAB' or `S-TAB'"))
           (let ((enable-recursive-minibuffers  t)
                 (doremi-up-keys                '(left)) ; Rebind, so more intuitive for width.
                 (doremi-boost-up-keys          '(M-left))
                 (doremi-down-keys              '(right))
                 (doremi-boost-down-keys        '(M-right)))
             (doremi (lambda (new-val)
                       (setq new-val                        (doremi-wrap new-val 1 100)
                             icicle-candidate-width-factor  new-val)
                       (icicle-display-candidates-in-Completions)
                       new-val)
                     icicle-candidate-width-factor
                     (- increment)))    ; Reverse, so arrows correspond.
           (when (member (car unread-command-events)
                         (append doremi-up-keys   doremi-boost-up-keys 
                                 doremi-down-keys doremi-boost-down-keys))
             (icicle-doremi-inter-candidates-min-spaces+ increment))
           (setq unread-command-events  ()))
      (unless mini (icicle-remove-Completions-window)))))

;;;###autoload (autoload 'icicle-doremi-inter-candidates-min-spaces+ "icicles")
(defun icicle-doremi-inter-candidates-min-spaces+ (&optional increment) ; Bound to `C-x |' in minibuf.
  "Change `icicle-inter-candidates-min-spaces' incrementally.
Use `up', `down' or the mouse wheel to increase or decrease.  You can
 use the `Meta' key (e.g. `M-right') to increment in larger steps.
Use `left', `right', or the mouse wheel to adjust
`icicle-candidate-width-factor'."
  (interactive "p")
  (unless (require 'doremi nil t) (error "This command needs library `doremi.el'."))
  (let ((mini  (active-minibuffer-window)))
    (unwind-protect
         (save-selected-window
           (select-window (minibuffer-window))
           (unless icicle-completion-candidates (message "Hit `TAB' or `S-TAB'"))
           (let* ((enable-recursive-minibuffers  t))
             (doremi (lambda (new-val)
                       (setq new-val                             (doremi-limit new-val 1 nil)
                             icicle-inter-candidates-min-spaces  new-val)
                       (icicle-display-candidates-in-Completions)
                       new-val)
                     icicle-inter-candidates-min-spaces
                     increment))
           (when (member (car unread-command-events)'(left right M-left M-right))
             (icicle-doremi-candidate-width-factor+ increment))
           (setq unread-command-events  ()))
      (unless mini (icicle-remove-Completions-window)))))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-WYSIWYG-Completions "icicles")
(defalias 'toggle-icicle-WYSIWYG-Completions 'icicle-toggle-WYSIWYG-Completions)
;;;###autoload (autoload 'icicle-toggle-WYSIWYG-Completions "icicles")
(defun icicle-toggle-WYSIWYG-Completions ()
  "Toggle the value of option `icicle-WYSIWYG-Completions-flag'."
  (interactive)
  (setq icicle-WYSIWYG-Completions-flag  (not icicle-WYSIWYG-Completions-flag))
  (icicle-msg-maybe-in-minibuffer
   "Using WYSIWYG for `*Completions*' display is now %s"
   (icicle-propertize (if icicle-WYSIWYG-Completions-flag "ON" "OFF") 'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-~-for-home-dir "icicles")
(defalias 'toggle-icicle-~-for-home-dir 'icicle-toggle-~-for-home-dir)
;;;###autoload (autoload 'icicle-toggle-~-for-home-dir "icicles")
(defun icicle-toggle-~-for-home-dir ()  ; Bound to `M-~' in minibuffer.
  "Toggle the value of option `icicle-use-~-for-home-dir-flag'.
Bound to `M-~' in the minibuffer."
  (interactive)
  (setq icicle-use-~-for-home-dir-flag  (not icicle-use-~-for-home-dir-flag))
  (icicle-msg-maybe-in-minibuffer
   "Using `~' for home directory is now %s"
   (icicle-propertize (if icicle-use-~-for-home-dir-flag "ON" "OFF") 'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-C-for-actions "icicles")
(defalias 'toggle-icicle-C-for-actions 'icicle-toggle-C-for-actions)
;;;###autoload (autoload 'icicle-toggle-C-for-actions "icicles")
(defun icicle-toggle-C-for-actions ()   ; Bound to `M-g' in minibuffer.
  "Toggle the value of option `icicle-use-C-for-actions-flag'.
Bound to `M-g' in the minibuffer."
  (interactive)
  (setq icicle-use-C-for-actions-flag  (not icicle-use-C-for-actions-flag))
  (icicle-toggle-icicle-mode-twice)
  (icicle-msg-maybe-in-minibuffer
   "Using `C-' prefix for multi-command actions is now %s"
   (icicle-propertize (if icicle-use-C-for-actions-flag "ON" "OFF") 'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-alternative-sorting "icicles")
(defalias 'toggle-icicle-alternative-sorting 'icicle-toggle-alternative-sorting)
;;;###autoload (autoload 'icicle-toggle-alternative-sorting "icicles")
(defun icicle-toggle-alternative-sorting () ; Bound to `C-M-,' in minibuffer.
  "Toggle alternative sorting of minibuffer completion candidates.
This swaps `icicle-alternative-sort-comparer' and `icicle-sort-comparer'.
Bound to `C-M-,' in the minibuffer."
  (interactive)
  (let ((alt-sort-fn  icicle-alternative-sort-comparer))
    (setq icicle-alternative-sort-comparer  (or icicle-sort-comparer  icicle-last-sort-comparer)
          icicle-sort-comparer              (or alt-sort-fn  icicle-last-sort-comparer))
    (icicle-complete-again-update)
    (icicle-msg-maybe-in-minibuffer
     "Sorting: `%s', Alternative: `%s'"
     (icicle-propertize icicle-sort-comparer             'face 'icicle-msg-emphasis)
     (icicle-propertize icicle-alternative-sort-comparer 'face 'icicle-msg-emphasis))))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-sorting "icicles")
(defalias 'toggle-icicle-sorting 'icicle-toggle-sorting)
;;;###autoload (autoload 'icicle-toggle-sorting "icicles")
(defun icicle-toggle-sorting ()         ; Not bound to a key.
  "Toggle sorting of minibuffer completion candidates.
When sorting is active, comparison is done by `icicle-sort-comparer'."
  (interactive)
  (if (and (interactive-p)  icicle-inhibit-sort-p)
      (icicle-msg-maybe-in-minibuffer "Cannot sort candidates now")
    (if icicle-sort-comparer
        (setq icicle-last-sort-comparer  icicle-sort-comparer ; Save it, for restoring.
              icicle-sort-comparer       nil)
      (setq icicle-sort-comparer  icicle-last-sort-comparer)) ; Restore it.
    (icicle-complete-again-update)
    (icicle-msg-maybe-in-minibuffer
     "Completion-candidate sorting is now %s"
     (icicle-propertize (if icicle-sort-comparer "ON" "OFF") 'face 'icicle-msg-emphasis))))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-angle-brackets "icicles")
(defalias 'toggle-icicle-angle-brackets 'icicle-toggle-angle-brackets)
;;;###autoload (autoload 'icicle-toggle-angle-brackets "icicles")
(defun icicle-toggle-angle-brackets ()
  "Toggle `icicle-key-descriptions-use-<>-flag'."
  (interactive)
  (setq icicle-key-descriptions-use-<>-flag  (not icicle-key-descriptions-use-<>-flag))
  (icicle-complete-again-update)
  (icicle-msg-maybe-in-minibuffer
   (if (< emacs-major-version 21)
       "This command has no effect prior to Emacs 21"
     "Displaying <...> in key descriptions is now %s"
     (icicle-propertize (if icicle-key-descriptions-use-<>-flag "ON" "OFF")
                        'face 'icicle-msg-emphasis))))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-annotation "icicles")
(defalias 'toggle-icicle-annotation 'icicle-toggle-annotation)
;;;###autoload (autoload 'icicle-toggle-annotation "icicles")
(defun icicle-toggle-annotation ()      ; Bound to `C-x C-a' in minibuffer.
  "Toggle `icicle-show-annotations-flag'.
Bound to `\\<minibuffer-local-completion-map>\\[icicle-toggle-annotation]' in the minibuffer."
  (interactive)
  (setq icicle-show-annotations-flag  (not icicle-show-annotations-flag))
  (icicle-complete-again-update)
  (icicle-msg-maybe-in-minibuffer "Displaying candidate annotations is now %s"
                                  (icicle-propertize (if icicle-show-annotations-flag "ON" "OFF")
                                                     'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-proxy-candidates "icicles")
(defalias 'toggle-icicle-proxy-candidates 'icicle-toggle-proxy-candidates)
;;;###autoload (autoload 'icicle-toggle-proxy-candidates "icicles")
(defun icicle-toggle-proxy-candidates () ; Bound to `C-M-_' in minibuffer.
  "Toggle `icicle-add-proxy-candidates-flag'.
Bound to `\\<minibuffer-local-completion-map>\\[icicle-toggle-proxy-candidates]' in the minibuffer.
With some commands, you must re-invoke the command for the new value
to take effect.  (This is for performance reasons.)"
  (interactive)
  (setq icicle-add-proxy-candidates-flag  (not icicle-add-proxy-candidates-flag)
        icicle-saved-proxy-candidates     (prog1 icicle-proxy-candidates
                                            (setq icicle-proxy-candidates
                                                  icicle-saved-proxy-candidates)))
  (icicle-complete-again-update)
  (icicle-msg-maybe-in-minibuffer
   "Including proxy candidates is now %s"
   (icicle-propertize (if icicle-add-proxy-candidates-flag "ON" "OFF") 'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-transforming "icicles")
(defalias 'toggle-icicle-transforming 'icicle-toggle-transforming)
;;;###autoload (autoload 'icicle-toggle-transforming "icicles")
(defun icicle-toggle-transforming ()    ; Bound to `C-$' in minibuffer.
  "Toggle transforming of minibuffer completion candidates.
When transforming is active, it is done by `icicle-transform-function'.
Bound to `C-$' in the minibuffer.  See the doc for individual
commands, for how `C-$' might affect them."
  (interactive)
  (let ((dups-fns  '(icicle-remove-duplicates icicle-remove-dups-if-extras)))
    ;; 1. If either is one of the DUPS-FNS, set the other to it and set it to nil.
    ;; 2. Else if both are other functions (and different), swap them, so you can toggle between them.
    ;; 3. Else if either is a function, set the other to it and set it to nil.
    ;; 4. Else (both are nil), set last to remove-dups and set this to nil.
    ;;
    ;; #1 is needed because we do not want to just swap them (#2) in that case.
    ;;
    (cond ((memq icicle-transform-function dups-fns) ; Swap with nil
           (setq icicle-last-transform-function  icicle-transform-function
                 icicle-transform-function       nil))
          ((memq icicle-last-transform-function dups-fns) ; Swap with nil
           (setq icicle-transform-function  icicle-last-transform-function
                 icicle-last-transform-function       nil))
          ((and icicle-transform-function  icicle-last-transform-function
                (not (eq icicle-transform-function  icicle-last-transform-function))) ; Swap them
           (setq icicle-transform-function
                 (prog1 icicle-last-transform-function
                   (setq icicle-last-transform-function  icicle-transform-function))))
          (icicle-transform-function    ; Swap with nil
           (setq icicle-last-transform-function  icicle-transform-function
                 icicle-transform-function       nil))
          (icicle-last-transform-function ; Swap with nil
           (setq icicle-transform-function  icicle-last-transform-function
                 icicle-last-transform-function       nil))
          (t                            ; Default: last removes dups, this does nothing.
           (setq icicle-last-transform-function  'icicle-remove-duplicates
                 icicle-transform-function       nil)))
    (icicle-complete-again-update)
    (icicle-msg-maybe-in-minibuffer icicle-toggle-transforming-message
                                    (icicle-propertize (if icicle-transform-function "ON" "OFF")
                                                       'face 'icicle-msg-emphasis))))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'cycle-icicle-incremental-completion "icicles")
(defalias 'cycle-icicle-incremental-completion 'icicle-cycle-incremental-completion)
;;;###autoload (autoload 'icicle-cycle-incremental-completion "icicles")
(defun icicle-cycle-incremental-completion () ; Bound to `C-#' in minibuffer.
  "Cycle the value of option `icicle-incremental-completion'.
If the current value is nil      then it is set to t.
If the current value is t        then it is set to `always'.
If the current value is `always' then it is set to nil.

Bound to `C-#' in the minibuffer."
  (interactive)
  (setq icicle-incremental-completion    (case icicle-incremental-completion
                                           ((nil)      t)
                                           ((t)        'always)
                                           (otherwise  nil))
        icicle-incremental-completion-p  icicle-incremental-completion)
  (icicle-msg-maybe-in-minibuffer
   "Incremental completion is now %s"
   (icicle-propertize (case icicle-incremental-completion
                        ((nil)      "OFF")
                        ((t)        "ON")
                        (otherwise  "EAGER"))
                      'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'cycle-icicle-expand-to-common-match "icicles")
(defalias 'cycle-icicle-expand-to-common-match 'icicle-cycle-expand-to-common-match)
;;;###autoload (autoload 'icicle-cycle-expand-to-common-match "icicles")
(defun icicle-cycle-expand-to-common-match () ; Bound to `C-M-"' in minibuffer.
  "Cycle the value of option `icicle-expand-input-to-common-match'.
Bound to \\<minibuffer-local-completion-map>\
`\\[icicle-cycle-expand-to-common-match]' in the minibuffer.

This cycles among all possible values of the option.  See also
`icicle-toggle-expand-to-common-match' (\\<minibuffer-local-completion-map>\
`\\[icicle-toggle-expand-to-common-match]' in the minibuffer)."
  (interactive)
  (setq icicle-expand-input-to-common-match  (mod (1+ icicle-expand-input-to-common-match) 5))
  (icicle-msg-maybe-in-minibuffer
   "Expanding input to common match is now %s"
   (icicle-propertize (case icicle-expand-input-to-common-match
                        (0  "0 - NEVER")
                        (1  "1 - `TAB', `S-TAB' ONLY")
                        (2  "2 - SOLE MATCH")
                        (3  "3 - PREFIX OR SOLE MATCH")
                        (t  "4 - ALWAYS"))
                      'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-expand-to-common-match "icicles")
(defalias 'toggle-icicle-expand-to-common-match 'icicle-toggle-expand-to-common-match)
;;;###autoload (autoload 'icicle-toggle-expand-to-common-match "icicles")
(defun icicle-toggle-expand-to-common-match () ; Bound to `C-"' in minibuffer.
  "Toggle the value of option `icicle-expand-input-to-common-match'.
The alternative values are those of that option and option
`icicle-expand-input-to-common-match-alt'.

Bound to \\<minibuffer-local-completion-map>\
`\\[icicle-toggle-expand-to-common-match]' in the minibuffer."
  (interactive)
  (setq icicle-expand-input-to-common-match
        (prog1 icicle-expand-input-to-common-match-alt
          (setq icicle-expand-input-to-common-match-alt  icicle-expand-input-to-common-match)))
  (icicle-msg-maybe-in-minibuffer
   "Expanding input to common match is now %s"
   (icicle-propertize (case icicle-expand-input-to-common-match
                        (0  "0 - NEVER")
                        (1  "1 - `TAB', `S-TAB' ONLY")
                        (2  "2 - SOLE MATCH")
                        (3  "3 - PREFIX OR SOLE MATCH")
                        (t  "4 - ALWAYS"))
                      'face 'icicle-msg-emphasis)))

;;;###autoload (autoload 'icicle-dispatch-C-^ "icicles")
(defun icicle-dispatch-C-^ ()           ; Bound to `C-^' in minibuffer.
  "Do the right thing for `C-^'
When Icicles searching, call `icicle-toggle-highlight-all-current'.
Otherwise, call `icicle-toggle-remote-file-testing'.
Bound to `C-^' in the minibuffer."
  (interactive)
  (if icicle-searching-p (icicle-toggle-highlight-all-current) (icicle-toggle-remote-file-testing)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-remote-file-testing "icicles")
(defalias 'toggle-icicle-remote-file-testing 'icicle-toggle-remote-file-testing)
;;;###autoload (autoload 'icicle-toggle-remote-file-testing "icicles")
(defun icicle-toggle-remote-file-testing () ; Bound to `C-^' in minibuffer.
  "Toggle `icicle-test-for-remote-files-flag'.
If you use Tramp for accessing remote files, then turning this off
also turns off Tramp file-name completion.  Therefore, if you use this
command to turn off testing of remote file names, then use it also to
turn testing back on (instead of just setting the option to non-nil).

Bound to `C-^' in the minibuffer, except during Icicles searching."
  (interactive)
  (setq icicle-test-for-remote-files-flag  (not icicle-test-for-remote-files-flag))
  (when (require 'tramp nil t)
    (if (not icicle-test-for-remote-files-flag)
        (tramp-unload-file-name-handlers) ; Turn off Tramp remote file-name completion.
      ;; Bind `partial-completion-mode' to force Tramp file-name handlers unconditionally, for older
      ;; Tramp versions than 2.1 (ugly HACK).  This code should work for all Tramp versions.
      (let ((non-essential            t) ; Emacs 23.2+
            (partial-completion-mode  t))
        (condition-case nil
            (tramp-register-file-name-handlers) ; Emacs 22+
          (void-function
           (tramp-register-file-name-handler) ; The order of these two matters.
           (tramp-register-completion-file-name-handler))))))
  (message "Updating completions...")
  (icicle-complete-again-update)
  (icicle-msg-maybe-in-minibuffer
   "Testing remote file names is now %s"
   (icicle-propertize (if icicle-test-for-remote-files-flag "ON" "OFF") 'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-network-drives-as-remote "icicles")
(when (memq system-type '(ms-dos windows-nt cygwin))
  (defalias 'toggle-icicle-network-drives-as-remote 'icicle-toggle-network-drives-as-remote)
;;;###autoload (autoload 'icicle-toggle-network-drives-as-remote "icicles")
  (defun icicle-toggle-network-drives-as-remote () ; Bound to `C-x :' in minibuffer.
    "Toggle `icicle-network-drive-means-remote-flag'.
Bound to `C-x :' in the minibuffer."
    (interactive)
    (setq icicle-network-drive-means-remote-flag  (not icicle-network-drive-means-remote-flag))
    (icicle-complete-again-update)
    (icicle-msg-maybe-in-minibuffer
     "MS Windows network drives are now considered %s"
     (icicle-propertize (if icicle-network-drive-means-remote-flag "REMOTE" "LOCAL")
                        'face 'icicle-msg-emphasis))))

;; NOT a top-level command (most toggle commands can be used at top-level).
;;
;;;###autoload (autoload 'toggle-icicle-highlight-all-current "icicles")
(defalias 'toggle-icicle-highlight-all-current 'icicle-toggle-highlight-all-current)
;;;###autoload (autoload 'icicle-toggle-highlight-all-current "icicles")
(defun icicle-toggle-highlight-all-current () ; Bound to `C-^' in minibuffer.
  "Toggle `icicle-search-highlight-all-current-flag'.
Bound to `C-^' in the minibuffer during Icicles searching (only)."
  (interactive)
  (icicle-barf-if-outside-Completions-and-minibuffer)
  (setq icicle-search-highlight-all-current-flag  (not icicle-search-highlight-all-current-flag))
  ;; Rehighlight to see effect of toggle.
  (let ((icicle-candidate-nb  icicle-candidate-nb))
    (let ((icicle-current-input             icicle-current-input)
          (icicle-incremental-completion-p  nil))
      (icicle-erase-minibuffer))
    (icicle-retrieve-last-input)
    (funcall (or icicle-last-completion-command  (if (eq icicle-current-completion-mode 'prefix)
                                                     #'icicle-prefix-complete
                                                   #'icicle-apropos-complete))))
  (icicle-search-highlight-all-input-matches icicle-current-input)
  (when icicle-candidate-nb (icicle-search-action "DUMMY")) ; Get back to current.
  (select-window (minibuffer-window))
  (select-frame-set-input-focus (selected-frame))
  (icicle-msg-maybe-in-minibuffer
   "Highlighting current input match in each main search hit is now %s"
   (icicle-propertize (if icicle-search-highlight-all-current-flag "ON" "OFF")
                      'face 'icicle-msg-emphasis)))

;;;###autoload (autoload 'icicle-dispatch-C-x. "icicles")
(defun icicle-dispatch-C-x. (arg)       ; Bound to `C-x .' in minibuffer.
  "Do the right thing for `C-x .'.
With a prefix arg, call `icicle-toggle-hiding-non-matching-lines'.
With no prefix arg, call `icicle-toggle-hiding-common-match'.
Bound to `C-x .' in the minibuffer."
  (interactive "P")
  (if arg (icicle-toggle-hiding-non-matching-lines) (icicle-toggle-hiding-common-match)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-hiding-common-match "icicles")
(defalias 'toggle-icicle-hiding-common-match 'icicle-toggle-hiding-common-match)
;;;###autoload (autoload 'icicle-toggle-hiding-common-match "icicles")
(defun icicle-toggle-hiding-common-match () ; Bound to `C-x .' in minibuf, via `icicle-dispatch-C-x.'.
  "Toggle `icicle-hide-common-match-in-Completions-flag'.
Bound to `C-x .' (no prefix arg) in the minibuffer.
See also option `icicle-hide-non-matching-lines-flag'."
  (interactive)
  (setq icicle-hide-common-match-in-Completions-flag
        (not icicle-hide-common-match-in-Completions-flag))
  (icicle-complete-again-update)
  (icicle-msg-maybe-in-minibuffer
   "Hiding common match in `*Completions*' is now %s"
   (icicle-propertize (if icicle-hide-common-match-in-Completions-flag "ON" "OFF")
                      'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-hiding-non-matching-lines "icicles")
(defalias 'toggle-icicle-hiding-non-matching-lines 'icicle-toggle-hiding-non-matching-lines)
;;;###autoload (autoload 'icicle-toggle-hiding-non-matching-lines "icicles")
(defun icicle-toggle-hiding-non-matching-lines () ; Bound to `C-u C-x .' in minibuffer.
  "Toggle `icicle-hide-non-matching-lines-flag'.
Bound to `C-u C-x .' in the minibuffer.
See also option `icicle-hide-common-match-in-Completions-flag'."
  (interactive)
  (setq icicle-hide-non-matching-lines-flag  (not icicle-hide-non-matching-lines-flag))
  (icicle-complete-again-update)
  (icicle-msg-maybe-in-minibuffer
   "Hiding non-matching candidate lines in `*Completions*' is now %s"
   (icicle-propertize (if icicle-hide-non-matching-lines-flag "ON" "OFF") 'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-show-multi-completion "icicles")
(defalias 'toggle-icicle-show-multi-completion 'icicle-toggle-show-multi-completion)
;;;###autoload (autoload 'icicle-toggle-show-multi-completion "icicles")
(defun icicle-toggle-show-multi-completion () ; Bound to `M-m' in minibuffer.
  "Toggle `icicle-show-multi-completion-flag'.
Bound to `M-m' in the minibuffer."
  (interactive)
  (setq icicle-show-multi-completion-flag  (not icicle-show-multi-completion-flag))
  (icicle-complete-again-update)
  (icicle-msg-maybe-in-minibuffer
   "Showing multi-completions (when available) is now %s"
   (icicle-propertize (if icicle-show-multi-completion-flag "ON" "OFF") 'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-highlight-historical-candidates "icicles")
(defalias 'toggle-icicle-highlight-historical-candidates
    'icicle-toggle-highlight-historical-candidates)
;;;###autoload (autoload 'icicle-toggle-highlight-historical-candidates "icicles")
(defun icicle-toggle-highlight-historical-candidates () ; Bound to `C-pause' in minibuffer.
  "Toggle `icicle-highlight-historical-candidates-flag'.
Bound to `C-pause' in the minibuffer."
  (interactive)
  (setq icicle-highlight-historical-candidates-flag  (not icicle-highlight-historical-candidates-flag))
  (icicle-complete-again-update)
  (icicle-msg-maybe-in-minibuffer
   "Highlighting previously used inputs in `*Completions*' is now %s"
   (icicle-propertize (if icicle-highlight-historical-candidates-flag "ON" "OFF")
                      'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-highlight-saved-candidates "icicles")
(defalias 'toggle-icicle-highlight-saved-candidates
    'icicle-toggle-highlight-saved-candidates)
;;;###autoload (autoload 'icicle-toggle-highlight-saved-candidates "icicles")
(defun icicle-toggle-highlight-saved-candidates () ; Bound to `S-pause' in minibuffer.
  "Toggle `icicle-highlight-saved-candidates-flag'.
Bound to `S-pause' in the minibuffer."
  (interactive)
  (setq icicle-highlight-saved-candidates-flag  (not icicle-highlight-saved-candidates-flag))
  (icicle-complete-again-update)
  (icicle-msg-maybe-in-minibuffer
   "Highlighting saved candidates in `*Completions*' is now %s"
   (icicle-propertize (if icicle-highlight-saved-candidates-flag "ON" "OFF")
                      'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-completions-format "icicles")
(defalias 'toggle-icicle-completions-format
    'icicle-toggle-completions-format)
;;;###autoload (autoload 'icicle-toggle-completions-format "icicles")
(defun icicle-toggle-completions-format () ; Bound to `C-M-^' in minibuffer.
  "Toggle `icicle-completions-format' between vertical and horizontal.
Bound to `C-M-^' in the minibuffer."
  (interactive)
  (setq icicle-completions-format  (if (eq 'vertical icicle-completions-format)
                                       'horizontal
                                     'vertical))
  (icicle-complete-again-update)
  (icicle-msg-maybe-in-minibuffer
   "Layout of candidates in `*Completions*' is now %s"
   (icicle-propertize (if (eq 'vertical icicle-completions-format) "VERTICAL" "HORIZONTAL")
                      'face 'icicle-msg-emphasis)))

;;;###autoload (autoload 'icicle-dispatch-C-. "icicles")
(defun icicle-dispatch-C-. ()           ; Bound to `C-.' in minibuffer.
  "Do the right thing for `C-.'.
When using Icicles search (`icicle-search' and similar commands), call
 `icicle-toggle-search-cleanup'.
Otherwise, call `icicle-toggle-ignored-extensions'.

Bound to `C-.' in the minibuffer."
  (interactive)
  (if icicle-searching-p (icicle-toggle-search-cleanup) (icicle-toggle-ignored-extensions)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-ignored-extensions "icicles")
(defalias 'toggle-icicle-ignored-extensions 'icicle-toggle-ignored-extensions)
;;;###autoload (autoload 'icicle-toggle-ignored-extensions "icicles")
(defun icicle-toggle-ignored-extensions () ; Bound to `C-.' in minibuffer except in Icicles search.
  "Toggle respect of `completion-ignored-extensions'.
Bound to `C-.' in minibuffer during file-name input."
  (interactive)
  (if (consp completion-ignored-extensions)
      (setq icicle-saved-ignored-extensions   completion-ignored-extensions ; Save it.
            completion-ignored-extensions     ()
            icicle-ignored-extensions-regexp  nil)
    (setq completion-ignored-extensions  icicle-saved-ignored-extensions ; Restore it.
          icicle-ignored-extensions-regexp ; Make regexp for ignored file extensions.
          (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "$\\|") "$\\)\\'")))
  ;; Flag to prevent updating `icicle-ignored-extensions-regexp' unless
  ;; `completion-ignored-extensions' changes.
  (setq icicle-ignored-extensions  completion-ignored-extensions)
  (icicle-complete-again-update)
  (icicle-msg-maybe-in-minibuffer
   "Ignoring selected file extensions is now %s"
   (icicle-propertize (if completion-ignored-extensions "ON" "OFF") 'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-ignored-space-prefix "icicles")
(defalias 'toggle-icicle-ignored-space-prefix 'icicle-toggle-ignored-space-prefix)
;;;###autoload (autoload 'icicle-toggle-ignored-space-prefix "icicles")
(defun icicle-toggle-ignored-space-prefix ()
                                        ; Bound to `M-_' in minibuffer, except when searching.
  "Toggle `icicle-buffer-ignore-space-prefix-flag'.
Bound to `M-_' in the minibuffer, except during Icicles searching."
  (interactive)
  (setq icicle-buffer-ignore-space-prefix-flag  (not icicle-buffer-ignore-space-prefix-flag))
  (icicle-complete-again-update)
  (icicle-msg-maybe-in-minibuffer
   "Ignoring space prefix in buffer names is now %s"
   (icicle-propertize (if icicle-buffer-ignore-space-prefix-flag "ON" "OFF") 'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-include-cached-files "icicles")
(defalias 'toggle-icicle-include-cached-files 'icicle-toggle-include-cached-files)
;;;###autoload (autoload 'icicle-toggle-include-cached-files "icicles")
(defun icicle-toggle-include-cached-files (&optional newval) ; Bound to `C-x F' in minibuffer for buffers.
  "Toggle the sign of option `icicle-buffer-include-cached-files-nflag'.
A prefix arg sets the option value to the numeric prefix value.
Bound to `C-x F' in the minibuffer during buffer-name completion."
  (interactive "P")
  (setq newval  (and newval  (prefix-numeric-value newval)))
  (setq icicle-buffer-include-cached-files-nflag
        (or newval  (- icicle-buffer-include-cached-files-nflag)))
  ;; Just in case someone uses setq instead of Customize.
  (when (zerop icicle-buffer-include-cached-files-nflag)
    (setq icicle-buffer-include-cached-files-nflag  20))
  (when (> icicle-buffer-include-cached-files-nflag 0)
    (unless (require 'filecache nil t)
      (error "Option toggled, but you need library `filecache.el' to use it")))
  (icicle-msg-maybe-in-minibuffer
   "Including cached file names for buffer completion is now %s (max: %s)"
   (icicle-propertize (if (> icicle-buffer-include-cached-files-nflag 0) "ON" "OFF")
                      'face 'icicle-msg-emphasis)
   (icicle-propertize (format "%d" icicle-buffer-include-cached-files-nflag) 'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-include-recent-files "icicles")
(defalias 'toggle-icicle-include-recent-files 'icicle-toggle-include-recent-files)
;;;###autoload (autoload 'icicle-toggle-include-recent-files "icicles")
(defun icicle-toggle-include-recent-files (&optional newval) ; Bound to `C-x R' in minibuffer for buffers.
  "Toggle the sign of option `icicle-buffer-include-recent-files-nflag'.
A prefix arg sets the option value to the numeric prefix value.
Bound to `C-x R' in the minibuffer during buffer-name completion."
  (interactive "P")
  (setq newval  (and newval  (prefix-numeric-value newval)))
  (setq icicle-buffer-include-recent-files-nflag
        (or newval  (- icicle-buffer-include-recent-files-nflag)))
  ;; Just in case someone uses setq instead of Customize.
  (when (zerop icicle-buffer-include-recent-files-nflag)
    (setq icicle-buffer-include-recent-files-nflag  20))
  (when (> icicle-buffer-include-recent-files-nflag 0)
    (if (require 'recentf nil t)
        (unless recentf-list (recentf-load-list))
      (error "Option toggled, but you need library `recentf.el' to use it")))
  (icicle-msg-maybe-in-minibuffer
   "Including recent file names for buffer completion is now %s (max: %s)"
   (icicle-propertize (if (> icicle-buffer-include-recent-files-nflag 0) "ON" "OFF")
                       'face 'icicle-msg-emphasis)
   (icicle-propertize (format "%d" icicle-buffer-include-recent-files-nflag) 'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-search-cleanup "icicles")
(defalias 'toggle-icicle-search-cleanup 'icicle-toggle-search-cleanup)
;;;###autoload (autoload 'icicle-toggle-search-cleanup "icicles")
(defun icicle-toggle-search-cleanup ()  ; Bound to `C-.' in minibuffer during Icicles search.
  "Toggle removal of `icicle-search' highlighting after a search.
This toggles option `icicle-search-cleanup-flag'.
Bound to `C-.' in the minibuffer during Icicles search."
  (interactive)
  (setq icicle-search-cleanup-flag  (not icicle-search-cleanup-flag))
  (icicle-complete-again-update)
  (icicle-msg-maybe-in-minibuffer
   "Removal of Icicles search highlighting is now %s"
   (icicle-propertize (if icicle-search-cleanup-flag "ON" "OFF") 'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-search-complementing-domain "icicles")
(defalias 'toggle-icicle-search-complementing-domain 'icicle-toggle-search-complementing-domain)
;;;###autoload (autoload 'icicle-toggle-search-complementing-domain "icicles")
(defun icicle-toggle-search-complementing-domain () ; Bound to `C-M-~' in minibuffer.
  "Toggle searching the complements of the normal search contexts.
This toggles internal variable `icicle-search-complement-domain-p'.
If toggled during search it affects the next, not the current, search.
Bound to `C-M-~' in the minibuffer."
  (interactive)
  (setq icicle-search-complement-domain-p  (not icicle-search-complement-domain-p))
  (icicle-msg-maybe-in-minibuffer
   "Future Icicles searches %suse the %s of the search domain"
   (if icicle-search-complement-domain-p
       ""
     (concat "do " (icicle-propertize "NOT " 'face 'icicle-msg-emphasis)))
   (if icicle-search-complement-domain-p
       "COMPLEMENT"
     (icicle-propertize "COMPLEMENT" 'face 'icicle-msg-emphasis))))

;;$$$ (defun icicle-dispatch-C-backquote ()   ; Bound to `C-`' in minibuffer.
;;   "Do the right thing for `C-`'.
;; When searching, call `icicle-toggle-literal-replacement'.
;; Otherwise, call `icicle-toggle-regexp-quote'.

;; Bound to `C-`' in the minibuffer."
;;   (interactive)
;;   (if icicle-searching-p (icicle-toggle-literal-replacement) (icicle-toggle-regexp-quote)))


;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-regexp-quote "icicles")
(defalias 'toggle-icicle-regexp-quote 'icicle-toggle-regexp-quote)
;;;###autoload (autoload 'icicle-toggle-regexp-quote "icicles")
(defun icicle-toggle-regexp-quote ()    ; Bound to `C-`' in minibuffer.
  "Toggle escaping of regexp special chars (`icicle-regexp-quote-flag').
Bound to `C-`' in the minibuffer."
  (interactive)
  (setq icicle-regexp-quote-flag  (not icicle-regexp-quote-flag))
  (icicle-complete-again-update)
  (icicle-msg-maybe-in-minibuffer
   "Escaping of regexp special characters is now %s"
   (icicle-propertize (if icicle-regexp-quote-flag "ON" "OFF") 'face 'icicle-msg-emphasis)))

;;;###autoload (autoload 'icicle-regexp-quote-input "icicles")
(defun icicle-regexp-quote-input (beg end) ; Bound to `M-%' in minibuffer.
  "Regexp quote current input or its active region, then apropos-complete.
Use this if you want to literally match all of what is currently in
the minibuffer or selected text there, but you also want to use that
literal text as part of a regexp for apropos completion.
Bound to `M-%' in the minibuffer."
  (interactive (if (and mark-active  (mark))
                   (list (region-beginning) (region-end))
                 (list (point-max) (point-max))))
  (icicle-barf-if-outside-Completions-and-minibuffer)
  (let ((regionp  (and mark-active  (mark)  (/= (point) (mark))))
        quoted-part)
    (save-excursion
      (save-restriction
        (narrow-to-region (if regionp beg (icicle-minibuffer-prompt-end)) (if regionp end (point-max)))
        (setq quoted-part  (regexp-quote (icicle-input-from-minibuffer)))
        (delete-region (icicle-minibuffer-prompt-end) (point-max))
        (insert quoted-part))))
  (setq icicle-current-input  (icicle-input-from-minibuffer))
  (let ((icicle-edit-update-p                 t)
        (icicle-expand-input-to-common-match  2)) ; Only explicit `TAB'/`S-TAB' or sole candidate match.
    (icicle-apropos-complete)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-literal-replacement "icicles")
(defalias 'toggle-icicle-literal-replacement 'icicle-toggle-literal-replacement)
;;;###autoload (autoload 'icicle-toggle-literal-replacement "icicles")
(defun icicle-toggle-literal-replacement () ; Bound to `C-M-`' in minibuffer.
  "Toggle escaping of regexp special chars in replacement text.
This toggles option `icicle-search-replace-literally-flag'.

Bound to `C-M-`' in the minibuffer."
  (interactive)
  (setq icicle-search-replace-literally-flag  (not icicle-search-replace-literally-flag))
  (icicle-msg-maybe-in-minibuffer
   "Replacement of text literally is now %s"
   (icicle-propertize (if icicle-search-replace-literally-flag "ON" "OFF")
                      'face 'icicle-msg-emphasis)))

;; Top-level commands.  Could instead be in `icicles-cmd2.el'.
;;
;;;###autoload (autoload 'toggle-icicle-case-sensitivity "icicles")
(defalias 'toggle-icicle-case-sensitivity 'icicle-toggle-case-sensitivity)
;;;###autoload (autoload 'icicle-toggle-case-sensitivity "icicles")
(defun icicle-toggle-case-sensitivity (file+buff-p) ; Bound to `C-S-a' in minibuffer, i.e., `C-A'.
  "Toggle case sensitivity.

This toggles `case-fold-search' and `completion-ignore-case'.  With a
prefix arg, it also toggles `read-file-name-completion-ignore-case'
\(Emacs 22 and later) and `read-buffer-completion-ignore-case' (Emacs
23 and later).

More precisely, this command first toggles the default value of
`case-fold-search', and then it sets the other variables to the value
of `case-fold-search'.

Note:
1. This toggles the default value of `case-fold-search'.  This means
that it does not matter which buffer is current when you call this
command - all buffers are affected henceforth.

2. Some Icicles commands bind one or more of these variables, so
invoking this command during command execution will not necessarily
toggle the global values of all of the variables.

Bound to `C-A' in the minibuffer, that is, `C-S-a'."
  (interactive "P")
  (setq-default case-fold-search        (not case-fold-search)
                completion-ignore-case  case-fold-search)
  (when file+buff-p
    (when (boundp 'read-file-name-completion-ignore-case) ; Emacs 22+
      (setq read-file-name-completion-ignore-case  case-fold-search))
    (when (boundp 'read-buffer-completion-ignore-case) ; Emacs 23+
      (setq read-buffer-completion-ignore-case  case-fold-search)))
  (icicle-complete-again-update)
  (icicle-highlight-lighter)
  (icicle-msg-maybe-in-minibuffer
   "Case-sensitive comparison is now %s"
   (cond ((and case-fold-search
               (or (not (boundp 'read-file-name-completion-ignore-case))
                   read-file-name-completion-ignore-case)
               (or (not (boundp 'read-buffer-completion-ignore-case))
                   read-buffer-completion-ignore-case))
          (concat (icicle-propertize "OFF" 'face 'icicle-msg-emphasis) ", everywhere"))
         (case-fold-search
          (concat (icicle-propertize "OFF" 'face 'icicle-msg-emphasis) ", "
                  (icicle-propertize "except" 'face 'icicle-msg-emphasis)
                  " for files and buffers"))
         (t (concat (icicle-propertize "ON" 'face 'icicle-msg-emphasis) ", everywhere")))))

;; `icicle-delete-window' (`C-x 0') does this in minibuffer.
;; `icicle-abort-recursive-edit' call this with non-nil FORCE.
;;
;;;###autoload (autoload 'icicle-remove-Completions-window "icicles")
(defun icicle-remove-Completions-window (&optional force)
  "Remove the `*Completions*' window.
If not called interactively and `*Completions*' is the selected
window, then do not remove it unless optional arg FORCE is non-nil."
  (interactive)
  ;; We do nothing if `*Completions*' is the selected window
  ;; or the minibuffer window is selected and `*Completions*' window was selected just before.
  (let ((swin  (selected-window)))
    ;; Emacs 20-21 has no `minibuffer-selected-window' function, but we just ignore that.
    (when (and (window-minibuffer-p swin)  (fboundp 'minibuffer-selected-window))
      (setq swin  (minibuffer-selected-window)))
    (cond (;; `*Completions*' is shown in the selected frame.
           (and (get-buffer-window "*Completions*")
                (or force               ; Let user use `C-g' to get rid of it even if selected.
                    (and (window-live-p swin) ; Not sure needed.
                         (not (eq (window-buffer swin) (get-buffer "*Completions*"))))
                    (interactive-p)))
           ;; Ignore error, in particular, "Attempt to delete the sole visible or iconified frame".
           (condition-case nil (delete-window (get-buffer-window "*Completions*")) (error nil))
           (bury-buffer (get-buffer "*Completions*")))
          (;; `*Completions*' is shown in a different frame.
           (and (get-buffer-window "*Completions*" 'visible)
                (or force               ; Let user use `C-g' to get rid of it even if selected.
                    (and (window-live-p swin)
                         (not (eq (window-buffer swin) (get-buffer "*Completions*"))))
                    (interactive-p)))
           ;; Ignore error, in particular, "Attempt to delete the sole visible or iconified frame".
           (when (window-dedicated-p (get-buffer-window "*Completions*" 'visible))
             (condition-case nil (icicle-delete-windows-on "*Completions*")  (error nil)))
           (bury-buffer (get-buffer "*Completions*"))))))

;; This is actually a top-level command, but it is in this file because it is used by
;; `icicle-remove-Completions-window'.
;;
;;;###autoload (autoload 'icicle-delete-windows-on "icicles")
(defun icicle-delete-windows-on (buffer)
  "Delete all windows showing BUFFER.
If such a window is alone in its frame, then delete the frame - unless
it is the only frame or a standalone minibuffer frame."
  (interactive
   (list (let ((enable-recursive-minibuffers  t))
           (read-buffer "Remove all windows showing buffer: " (current-buffer) 'existing))))
  (setq buffer  (get-buffer buffer))    ; Convert to buffer.
  (when buffer                          ; Do nothing if null BUFFER.
    ;; Avoid error message "Attempt to delete minibuffer or sole ordinary window".
    (let* ((this-buffer-frames  (icicle-frames-on buffer t))
           (this-frame          (car this-buffer-frames)))
      (unless (and this-frame
                   (frame-visible-p this-frame)
                   (null (cdr this-buffer-frames)) ; Only one frame shows BUFFER.
                   (eq (cdr (assoc 'minibuffer (frame-parameters this-frame)))
                       (active-minibuffer-window)) ; Has an active minibuffer.
                   (save-window-excursion
                     (select-frame this-frame)
                     (one-window-p t 'SELECTED-FRAME-ONLY))) ; Only one window.
        (let (win)
          (dolist (fr  this-buffer-frames)
            (setq win  (get-buffer-window buffer fr))
            (select-window win)
            (if (and (one-window-p t)  (cdr (visible-frame-list))) ; Sole window but not sole frame.
                (delete-frame)
              (delete-window (selected-window)))))))))

;; Free var here: `icicle-bufflist' is bound by `icicle-buffer-bindings'.
;;;###autoload (autoload 'icicle-remove-buffer-cands-for-mode "icicles")
(defun icicle-remove-buffer-cands-for-mode (&optional derivedp keep-p)
  "Prompt for a major mode, then remove buffer candidates with that mode.
Repeat this to progressively remove buffers with different modes.

Non-nil DERIVEDP (prefix arg) means remove buffers with or derived
from the mode.

Non-nil KEEP-P means do the opposite: keep only such buffer
candidates, instead of removing them."
  (interactive "P")
  (save-selected-window (icicle-remove-Completions-window))
  (let* ((orig-buff                     icicle-pre-minibuffer-buffer)
         (enable-recursive-minibuffers  t)
         (buffer-modes                  (icicle-remove-duplicates
                                         (mapcar (lambda (buf)
                                                   (with-current-buffer buf
                                                     (list (symbol-name major-mode))))
                                                 icicle-bufflist)))
         (mode
          (let ((icicle-must-pass-after-match-predicate  nil))
            (intern (completing-read
                     (format "%s candidates %s mode: "
                             (if keep-p "Keep only" "Remove")
                             (if derivedp "derived from" "with"))
                     (if derivedp
                         (let ((modes  buffer-modes)
                               parent ancestors)
                           (dolist (buf  icicle-bufflist)
                             (setq ancestors  ()
                                   parent     (get (with-current-buffer buf major-mode)
                                                   'derived-mode-parent))
                             (while parent
                               (add-to-list 'modes (list (symbol-name parent)))
                               (setq parent  (icicle-get-safe parent 'derived-mode-parent))))
                           modes)
                       buffer-modes)
                     nil t))))
         (new-pred
          (if (and derivedp  (fboundp 'derived-mode-p))
              (if keep-p
                  `(lambda (buf) (with-current-buffer buf (derived-mode-p ',mode)))
                `(lambda (buf) (not (with-current-buffer buf (derived-mode-p ',mode)))))
            (if keep-p
                `(lambda (buf) (with-current-buffer buf (eq major-mode ',mode)))
              `(lambda (buf) (with-current-buffer buf (not (eq major-mode ',mode))))))))
    (setq icicle-must-pass-after-match-predicate
          (if icicle-must-pass-after-match-predicate
              (lexical-let ((curr-pred  icicle-must-pass-after-match-predicate))
                `(lambda (buf)
                   (and (funcall ',curr-pred buf)  (funcall ',new-pred buf))))
            new-pred)))
  (icicle-complete-again-update))

;;;###autoload (autoload 'icicle-remove-buffer-cands-for-derived-mode "icicles")
(defun icicle-remove-buffer-cands-for-derived-mode ()
  "Prompt for a major mode, then remove buffer candidates derived from it.
Repeat this to progressively remove buffers with different modes."
  (interactive)
  (icicle-remove-buffer-cands-for-mode 'DERIVEDP))

;;;###autoload (autoload 'icicle-keep-only-buffer-cands-for-mode "icicles")
(defun icicle-keep-only-buffer-cands-for-mode (&optional derivedp)
  "Prompt for a major mode.  Keep only buffer candidates with that mode.
Non-nil DERIVEDP (prefix arg) means keep only buffers with or derived
from the mode."
  (interactive "P")
  (icicle-remove-buffer-cands-for-mode derivedp 'KEEP-P))
  
;;;###autoload (autoload 'icicle-keep-only-buffer-cands-for-derived-mode "icicles")
(defun icicle-keep-only-buffer-cands-for-derived-mode ()
  "Prompt for a major mode.  Keep only buffer candidates derived from it."
  (interactive)
  (icicle-remove-buffer-cands-for-mode 'DERIVEDP 'KEEP-P))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-mcmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-mcmd.el ends here

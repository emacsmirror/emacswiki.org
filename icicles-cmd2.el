;;; icicles-cmd2.el --- Top-level commands for Icicles
;;
;; Filename: icicles-cmd2.el
;; Description: Top-level commands for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2012, Drew Adams, all rights reserved.
;; Created: Thu May 21 13:31:43 2009 (-0700)
;; Version: 22.0
;; Last-Updated: Sat Dec  1 16:40:43 2012 (-0800)
;;           By: dradams
;;     Update #: 6187
;; URL: http://www.emacswiki.org/icicles-cmd2.el
;; Doc URL: http://www.emacswiki.org/Icicles
;; Keywords: extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `avoid', `cl', `cus-edit',
;;   `cus-face', `cus-load', `cus-start', `doremi', `easymenu',
;;   `el-swank-fuzzy', `ffap', `ffap-', `frame-cmds', `frame-fns',
;;   `fuzzy', `fuzzy-match', `hexrgb', `icicles-cmd1',
;;   `icicles-face', `icicles-fn', `icicles-mcmd', `icicles-opt',
;;   `icicles-var', `image-dired', `kmacro', `levenshtein',
;;   `misc-fns', `mouse3', `mwheel', `naked', `regexp-opt', `ring',
;;   `ring+', `second-sel', `strings', `thingatpt', `thingatpt+',
;;   `wid-edit', `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  top-level commands (and a few non-interactive functions used in
;;  those commands).  This is a continuation of library
;;  `icicles-cmd1.el' (a single file for all top-level commands would
;;  be too large to upload to Emacs Wiki).
;;
;;  For commands to be used mainly in the minibuffer or buffer
;;  `*Completions*', see `icicles-mcmd.el'.
;;
;;  For Icicles documentation, see `icicles-doc1.el' and
;;  `icicles-doc2.el'.
;;
;;  If you use the byte-compiled version of this library,
;;  `icicles-cmd2.elc', in Emacs 23, then it must be byte-compiled
;;  using Emacs 23.  Otherwise, Icicles key completion (and perhaps
;;  other things?) will not work correctly.
;;
;;  Macros defined here:
;;
;;    `icicle-search-modes', `icicle-with-comments-hidden'.
;;
;;  Widgets defined here:
;;
;;    `icicle-color', `icicle-ORIG-color'.
;;
;;  Commands defined here - (+) means a multi-command:
;;
;;    (+)`a', (+)`any', (+)`buffer', (+)`file', (+)`icicle-anything',
;;    (+)`icicle-apply', (+)`icicle-bookmark-a-file',
;;    (+)`icicle-choose-faces', (+)`icicle-choose-invisible-faces',
;;    (+)`icicle-choose-visible-faces', (+)`icicle-comint-command',
;;    (+)`icicle-comint-search', (+)`icicle-compilation-search',
;;    (+)`icicle-complete-keys', `icicle-complete-thesaurus-entry',
;;    (+)`icicle-doc', (+)`icicle-exchange-point-and-mark',
;;    (+)`icicle-find-file-all-tags',
;;    (+)`icicle-find-file-all-tags-other-window',
;;    (+)`icicle-find-file-all-tags-regexp',
;;    (+)`icicle-find-file-all-tags-regexp-other-window',
;;    (+)`icicle-find-file-handle-bookmark',
;;    (+)`icicle-find-file-handle-bookmark-other-window',
;;    (+)`icicle-find-file-some-tags',
;;    (+)`icicle-find-file-some-tags-other-window',
;;    (+)`icicle-find-file-some-tags-regexp',
;;    (+)`icicle-find-file-some-tags-regexp-other-window',
;;    (+)`icicle-find-file-tagged',
;;    (+)`icicle-find-file-tagged-other-window', (+)`icicle-font',
;;    (+)`icicle-frame-bg', (+)`icicle-frame-fg', (+)`icicle-fundoc',
;;    (+)`icicle-goto-global-marker',
;;    (+)`icicle-goto-global-marker-or-pop-global-mark',
;;    (+)`icicle-goto-marker',
;;    (+)`icicle-goto-marker-or-set-mark-command',
;;    (+)`icicle-hide-faces', (+)`icicle-hide-only-faces',
;;    `icicle-hide/show-comments', (+)`icicle-imenu',
;;    (+)`icicle-imenu-command', (+)`icicle-imenu-command-full',
;;    (+)`icicle-imenu-face-full', (+)`icicle-imenu-face-full',
;;    (+)`icicle-imenu-full', (+)`icicle-imenu-key-explicit-map',
;;    (+)`icicle-imenu-key-explicit-map-full',
;;    (+)`icicle-imenu-key-implicit-map',
;;    (+)`icicle-imenu-key-implicit-map-full',
;;    (+)`icicle-imenu-macro', (+)`icicle-imenu-macro-full',
;;    (+)`icicle-imenu-non-interactive-function',
;;    (+)`icicle-imenu-non-interactive-function-full',
;;    (+)`icicle-imenu-user-option',
;;    (+)`icicle-imenu-user-option-full', (+)`icicle-imenu-variable',
;;    (+)`icicle-imenu-variable-full', `icicle-ido-like-mode',
;;    (+)`icicle-Info-goto-node', (+)`icicle-Info-index',
;;    (+)`icicle-Info-index-20', (+)`icicle-Info-menu',
;;    (+)`icicle-Info-menu-cmd', `icicle-Info-virtual-book',
;;    (+)`icicle-insert-thesaurus-entry', (+)`icicle-map',
;;    `icicle-next-visible-thing', `icicle-non-whitespace-string-p',
;;    (+)`icicle-object-action', (+)`icicle-occur',
;;    (+)`icicle-pick-color-by-name', (+)`icicle-plist',
;;    `icicle-previous-visible-thing', `icicle-read-color',
;;    `icicle-read-color-wysiwyg', `icicle-save-string-to-variable',
;;    (+)`icicle-search', (+)`icicle-search-all-tags-bookmark',
;;    (+)`icicle-search-all-tags-regexp-bookmark',
;;    (+)`icicle-search-autofile-bookmark',
;;    (+)`icicle-search-autonamed-bookmark',
;;    (+)`icicle-search-bookmark',
;;    (+)`icicle-search-bookmark-list-bookmark',
;;    `icicle-search-bookmark-list-marked',
;;    (+)`icicle-search-bookmarks-together',
;;    (+)`icicle-search-buffer', (+)`icicle-search-buff-menu-marked',
;;    (+)`icicle-search-char-property', (+)`icicle-search-defs',
;;    (+)`icicle-search-defs-full', (+)`icicle-search-dired-bookmark',
;;    (+)`icicle-search-dired-marked-recursive',
;;    (+)`icicle-search-file', (+)`icicle-search-file-bookmark',
;;    (+)`icicle-search-generic', (+)`icicle-search-gnus-bookmark',
;;    `icicle-search-highlight-cleanup',
;;    (+)`icicle-search-ibuffer-marked',
;;    (+)`icicle-search-info-bookmark', (+)`icicle-search-keywords',
;;    (+)`icicle-search-lines',
;;    (+)`icicle-search-local-file-bookmark',
;;    (+)`icicle-search-man-bookmark',
;;    (+)`icicle-search-non-file-bookmark',
;;    (+)`icicle-search-overlay-property',
;;    (+)`icicle-search-paragraphs', (+)`icicle-search-pages',
;;    (+)`icicle-search-region-bookmark',
;;    (+)`icicle-search-remote-file-bookmark',
;;    (+)`icicle-search-sentences',
;;    (+)`icicle-search-some-tags-bookmark',
;;    (+)`icicle-search-some-tags-regexp-bookmark',
;;    (+)`icicle-search-specific-buffers-bookmark',
;;    (+)`icicle-search-specific-files-bookmark',
;;    (+)`icicle-search-temporary-bookmark',
;;    (+)`icicle-search-text-property', (+)`icicle-search-thing',
;;    (+)`icicle-search-this-buffer-bookmark',
;;    (+)`icicle-search-url-bookmark',
;;    `icicle-search-w-isearch-string',
;;    (+)`icicle-search-w3m-bookmark', (+)`icicle-search-word',
;;    (+)`icicle-search-xml-element',
;;    (+)`icicle-search-xml-element-text-node',
;;    (+)`icicle-select-frame', `icicle-select-frame-by-name',
;;    (+)`icicle-select-text-at-point',
;;    `icicle-set-S-TAB-methods-for-command',
;;    `icicle-set-TAB-methods-for-command', (+)`icicle-show-faces',
;;    (+)`icicle-show-only-faces', (+)`icicle-synonyms',
;;    (+)`icicle-tag-a-file', (+)`icicle-tags-search',
;;    (+)`icicle-untag-a-file', (+)`icicle-vardoc',
;;    (+)`icicle-where-is', (+)`synonyms', (+)`what-which-how'.
;;
;;  Non-interactive functions defined here:
;;
;;    `icicle-add-key+cmd', `icicle-anything-candidate-value',
;;    `icicle-apply-action', `icicle-apply-list-action',
;;    `icicle-char-properties-in-buffer',
;;    `icicle-char-properties-in-buffers',
;;    `icicle-choose-anything-candidate',
;;    `icicle-choose-candidate-of-type',
;;    `icicle-cmd2-after-load-bookmark+',
;;    `icicle-cmd2-after-load-hexrgb',
;;    `icicle-cmd2-after-load-highlight',
;;    `icicle-cmd2-after-load-palette',
;;    `icicle-cmd2-after-load-synonyms',
;;    `icicle-cmd2-after-load-wid-edit+', `icicle-color-blue-lessp',
;;    `icicle-color-completion-setup',
;;    `icicle-color-distance-hsv-lessp',
;;    `icicle-color-distance-rgb-lessp', `icicle-color-green-lessp',
;;    `icicle-color-help', `icicle-color-hsv-lessp',
;;    `icicle-color-hue-lessp', `icicle-color-red-lessp',
;;    `icicle-color-saturation-lessp', `icicle-color-value-lessp',
;;    `icicle-comint-hook-fn',
;;    `icicle-comint-search-get-final-choice',
;;    `icicle-comint-search-get-minibuffer-input',
;;    `icicle-comint-search-send-input', `icicle-compilation-hook-fn',
;;    `icicle-compilation-search-in-context-fn',
;;    `icicle-complete-keys-1', `icicle-complete-keys-action',
;;    `icicle-defined-thing-p', `icicle-doc-action',
;;    `icicle-fn-doc-minus-sig', `icicle-font-w-orig-size',
;;    `icicle-get-anything-actions-for-type',
;;    `icicle-get-anything-cached-candidates',
;;    `icicle-get-anything-candidates',
;;    `icicle-get-anything-candidates-of-type',
;;    `icicle-get-anything-default-actions-for-type',
;;    `icicle-get-anything-input-delay',
;;    `icicle-get-anything-req-pat-chars',
;;    `icicle-get-anything-types', `icicle-goto-marker-1',
;;    `icicle-goto-marker-1-action', `icicle-group-regexp',
;;    `icicle-imenu-command-p', `icicle-imenu-in-buffer-p',
;;    `icicle-imenu-non-interactive-function-p',
;;    `icicle-Info-book-order-p',
;;    `icicle-Info-build-node-completions',
;;    `icicle-Info-build-node-completions-1',
;;    `icicle-Info-goto-node-1', `icicle-Info-goto-node-action',
;;    `icicle-Info-index-action', `icicle-Info-read-node-name',
;;    `icicle-insert-thesaurus-entry-cand-fn',
;;    `icicle-invisible-face-p', `icicle-invisible-p',
;;    `icicle-keys+cmds-w-prefix', `icicle-make-color-candidate',
;;    `icicle-marker+text', `icicle-markers',
;;    `icicle-next-single-char-property-change',
;;    `icicle-next-visible-thing-1', `icicle-next-visible-thing-2',
;;    `icicle-next-visible-thing-and-bounds',
;;    `icicle-ORIG-read-color', `icicle-ORIG-widget-color-complete',
;;    `icicle-pick-color-by-name-action',
;;    `icicle-previous-single-char-property-change',
;;    `icicle-read-args-for-set-completion-methods',
;;    `icicle-read-var-value-satisfying',
;;    `icicle-region-or-buffer-limits', `icicle-search-action',
;;    `icicle-search-action-1', `icicle-search-bookmark-action',
;;    `icicle-search-char-property-scan',
;;    `icicle-search-char-prop-matches-p',
;;    `icicle-search-choose-buffers', `icicle-search-cleanup',
;;    `icicle-search-define-candidates',
;;    `icicle-search-define-candidates-1', `icicle-search-final-act',
;;    `icicle-search-help',
;;    `icicle-search-highlight-all-input-matches',
;;    `icicle-search-highlight-and-maybe-replace',
;;    `icicle-search-highlight-input-matches-here',
;;    `icicle-search-in-context-default-fn',
;;    `icicle-search-property-args',
;;    `icicle-search-property-default-match-fn',
;;    `icicle-search-quit-or-error',
;;    `icicle-search-read-context-regexp', `icicle-search-read-word',
;;    `icicle-search-regexp-scan',
;;    `icicle-search-replace-all-search-hits',
;;    `icicle-search-replace-cand-in-alist',
;;    `icicle-search-replace-cand-in-mct',
;;    `icicle-search-replace-fixed-case-p',
;;    `icicle-search-replace-match',
;;    `icicle-search-replace-search-hit', `icicle-search-thing-args',
;;    `icicle-search-thing-scan', `icicle-search-where-arg',
;;    `icicle-set-completion-methods-for-command',
;;    `icicle-things-alist', `icicle-this-command-keys-prefix',
;;    `icicle-widget-color-complete'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-active-map', `icicle-info-buff', `icicle-info-window',
;;    `icicle-key-prefix', `icicle-key-prefix-2',
;;    `icicle-last-thing-type', `icicle-named-colors',
;;    `icicle-orig-extra-cands', `icicle-orig-font',
;;    `icicle-orig-frame', `icicle-orig-menu-bar',
;;    `icicle-orig-pixelsize', `icicle-orig-pointsize',
;;    `icicle-orig-show-initially-flag',
;;    `icicle-orig-sort-orders-alist', `icicle-this-cmd-keys'.
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
;;  (@> "Icicles Commands for Other Packages")
;;  (@> "Icicles Top-Level Commands, Part 2")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
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

(eval-when-compile (require 'cl)) ;; case, loop
                                  ;; plus, for Emacs < 21: dolist, push
(eval-when-compile (when (>= emacs-major-version 22) (require 'edmacro))) ;; edmacro-subseq
(eval-when-compile (require 'comint))
  ;; comint-check-proc, comint-copy-old-input, comint-get-old-input, comint-input-ring,
  ;; comint-prompt-regexp, comint-send-input
(eval-when-compile (require 'imenu)) ;; imenu-syntax-alist
(eval-when-compile (require 'compile)) ;; compilation-find-buffer
(eval-when-compile (require 'info)) ;; Info-goto-node
(eval-when-compile (require 'etags)) ;; tags-case-fold-search, tags-table-files,
                                     ;; visit-tags-table-buffer
(eval-when-compile (when (> emacs-major-version 21)
                     (require 'anything nil t))) ;; (no error if not found):
  ;; anything-candidate-cache, anything-get-sources, anything-idle-delay, anything-pattern,
  ;; anything-sources, anything-transform-candidates
(require 'strings nil t) ;; (no error if not found): read-number (my version)
(eval-when-compile (require 'bookmark+ nil t)) ;; (no error if not found):
  ;; bmkp-bmenu-barf-if-not-in-menu-list, bmkp-bmenu-get-marked-files, bmkp-bookmark-last-access-cp,
  ;; bmkp-buffer-last-access-cp, bmkp-describe-bookmark, bmkp-describe-bookmark-internals,
  ;; bmkp-file-alpha-cp, bmkp-get-buffer-name, bmkp-get-end-position, bmkp-get-tags, bmkp-gnus-cp,
  ;; bmkp-handler-cp, bmkp-info-cp, bmkp-local-file-accessed-more-recently-cp,
  ;; bmkp-local-file-size-cp, bmkp-local-file-type-cp, bmkp-local-file-updated-more-recently-cp,
  ;; bmkp-marked-cp, bmkp-non-file-filename, bmkp-read-tags-completing, bmkp-region-alist-only,
  ;; bmkp-region-bookmark-p, bmkp-sorted-alist, bmkp-sort-omit, bmkp-url-cp, bmkp-visited-more-cp
(eval-when-compile (require 'hexrgb nil t)) ;; (no error if not found):
  ;; hexrgb-color-name-to-hex, hexrgb-defined-colors, hexrgb-defined-colors-alist, hexrgb-hex-to-hsv,
  ;; hexrgb-hex-to-rgb, hexrgb-read-color, hexrgb-(red|green|blue|hue|saturation|value),
  ;; hexrgb-rgb-hex-string-p, hexrgb-rgb-to-hsv, hexrgb-value
(eval-when-compile (require 'highlight nil t)) ;; (no error if not found):
  ;; hlt-act-on-any-face-flag, hlt-hide-default-face, hlt-highlight-faces-in-buffer,
  ;; hlt-region-or-buffer-limits, hlt-show-default-face
(eval-when-compile
 (or (condition-case nil
         (load-library "icicles-mac")   ; Use load-library to ensure latest .elc.
       (error nil))
     (require 'icicles-mac)))           ; Require, so can load separately if not on `load-path'.
  ;; icicle-bind-file-candidate-keys, icicle-define-command, icicle-define-file-command,
  ;; icicle-file-bindings, icicle-unbind-file-candidate-keys
(require 'icicles-mcmd)
  ;; icicle-search-define-replacement
(require 'icicles-opt)                  ; (This is required anyway by `icicles-var.el'.)
  ;; icicle-alternative-sort-comparer, icicle-buffer-extras, icicle-buffer-ignore-space-prefix-flag,
  ;; icicle-buffer-match-regexp, icicle-buffer-no-match-regexp, icicle-buffer-predicate,
  ;; icicle-buffer-require-match-flag, icicle-buffer-sort, icicle-complete-keys-self-insert-ranges,
  ;; icicle-key-descriptions-use-<>-flag, icicle-recenter, icicle-require-match-flag,
  ;; icicle-saved-completion-sets, icicle-search-cleanup-flag, icicle-kbd,
  ;; icicle-search-highlight-all-current-flag, icicle-search-highlight-threshold, icicle-search-hook,
  ;; icicle-sort-comparer, icicle-transform-function
(require 'icicles-var)                  ; (This is required anyway by `icicles-fn.el'.)
  ;; icicle-abs-file-candidates, icicle-acting-on-next/prev, icicle-all-candidates-action,
  ;; icicle-all-candidates-list-action-fn, icicle-all-candidates-list-alt-action-fn,
  ;; icicle-apply-nomsg, icicle-apropos-complete-match-fn, icicle-bookmark-history,
  ;; icicle-buffer-sort-first-time-p, icicle-candidate-action-fn, icicle-candidate-alt-action-fn,
  ;; icicle-candidate-entry-fn, icicle-candidate-help-fn, icicle-candidate-nb,
  ;; icicle-candidate-properties-alist, icicle-candidates-alist, icicle-char-property-value-history,
  ;; icicle-color-history, icicle-complete-keys-alist, icicle-completing-keys-p,
  ;; icicle-completion-candidates, icicle-completion-set-history, icicle-current-completion-mode,
  ;; icicle-current-input, icicle-delete-candidate-object, icicle-dictionary-history,
  ;; icicle-doc-last-initial-cand-set, icicle-explore-final-choice, icicle-explore-final-choice-full,
  ;; icicle-extra-candidates, icicle-extra-candidates-dir-insert-p, icicle-face-name-history,
  ;; icicle-fancy-candidates-p, icicle-font-name-history, icicle-full-cand-fn,
  ;; icicle-function-name-history, icicle-fundoc-last-initial-cand-set,
  ;; icicle-get-alist-candidate-function, icicle-hist-cands-no-highlight, icicle-hist-var,
  ;; icicle-Info-only-rest-of-book-p, icicle-key-prefix-description, icicle-last-completion-candidate,
  ;; icicle-last-completion-command, icicle-last-input, icicle-last-sort-comparer,
  ;; icicle-last-transform-function, icicle-list-use-nth-parts, icicle-minibuffer-message-ok-p,
  ;; icicle-must-match-regexp, icicle-must-not-match-regexp, icicle-must-pass-after-match-predicate,
  ;; icicle-nb-of-other-cycle-candidates, icicle-orig-buff, icicle-orig-pt-explore, icicle-orig-window,
  ;; icicle-orig-win-explore, icicle-other-window, icicle-plist-last-initial-cand-set,
  ;; icicle-predicate-types-alist, icicle-pref-arg, icicle-prompt, icicle-proxy-candidate-regexp,
  ;; icicle-proxy-candidates, icicle-require-match-p, icicle-saved-completion-candidate,
  ;; icicle-saved-completion-candidates, icicle-scan-fn-or-regexp, icicle-search-command,
  ;; icicle-search-complement-domain-p, icicle-search-context-level, icicle-search-context-regexp,
  ;; icicle-search-current-overlay, icicle-search-final-choice, icicle-search-history,
  ;; icicle-search-in-context-fn, icicle-searching-p, icicle-search-level-overlays, icicle-search-modes,
  ;; icicle-search-overlays, icicle-search-refined-overlays, icicle-search-replacement,
  ;; icicle-transform-before-sort-p, icicle-vardoc-last-initial-cand-set,
  ;; icicle-variable-name-history, icicle-whole-candidate-as-text-prop-p
(require 'icicles-fn)                   ; (This is required anyway by `icicles-mcmd.el'.)
  ;; icicle-candidate-short-help, icicle-completing-read-history,
  ;; icicle-highlight-lighter, icicle-insert-cand-in-minibuffer, icicle-kill-a-buffer, icicle-some
(require 'icicles-cmd1)
  ;; custom-variable-p, icicle-bookmark-cleanup,
  ;; icicle-bookmark-cleanup-on-quit, icicle-bookmark-cmd, icicle-bookmark-help-string,
  ;; icicle-bookmark-propertize-candidate, icicle-buffer-list,
  ;; icicle-explore, icicle-face-list, icicle-file-list, icicle-keyword-list, icicle-make-bookmark-candidate,
  ;; icicle-make-frame-alist, icicle-select-bookmarked-region

;;; (require 'icicles-mode)
;;;   ;; icicle-ORIG-Info-goto-node, icicle-ORIG-Info-index, icicle-ORIG-Info-menu



;; Byte-compiling this file, you will likely get some byte-compiler warning messages.
;; These are probably benign - ignore them.  Icicles is designed to work with multiple
;; versions of Emacs, and that fact provokes compiler warnings.  If you get byte-compiler
;; errors (not warnings), then please report a bug, using `M-x icicle-send-bug-report'.

;;; Some defvars to quiet byte-compiler a bit:

(defvar anything-sources)               ; In `anything.el'
(defvar anything-candidate-cache)       ; In `anything.el'
(defvar anything-idle-delay)            ; In `anything.el'
(defvar bmkp-non-file-filename)         ; In `bookmark+-1.el'
(defvar bmkp-sorted-alist)              ; In `bookmark+-1.el'
(defvar er/try-expand-list)             ; In `expand-region.el'
(defvar eyedrop-picked-background)      ; In `eyedrop.el' or `palette.el'
(defvar eyedrop-picked-foreground)      ; In `eyedrop.el' or `palette.el'
(defvar hlt-act-on-any-face-flag)       ; In `highlight.el'
(defvar icicle-complete-keys-self-insert-ranges) ; In `icicles-var.el' (Emacs 22+)
(defvar icicle-search-ecm)              ; In `icicle-search'
(defvar icicle-track-pt)                ; In `icicle-insert-thesaurus-entry'
(defvar replace-count)                  ; In `replace.el'.

;; (< emacs-major-version 21)
(defvar tooltip-mode)                   ; In `tooltip.el'

;; (< emacs-major-version 22)
(defvar compilation-current-error)
(defvar Info-history-list)              ; In `info.el'
(defvar Info-menu-entry-name-re)        ; In `info.el'
(defvar Info-read-node-completion-table) ; In `info.el'
(defvar list-colors-sort)               ; In `facemenu.el' (Emacs 23+)
(defvar palette-current-color)          ; In `palette.el'
(defvar palette-last-color)             ; In `palette.el'
(defvar palette-mode-map)               ; In `palette.el'
(defvar palette-popup-map)              ; In `palette.el'
(defvar read-file-name-completion-ignore-case) ; In `minibuffer.el'
(defvar synonyms-append-result-flag)    ; IN `synonyms.el'
(defvar synonyms-match-more-flag)       ; In `synonyms.el'
(defvar synonyms-obarray)               ; In `synonyms.el'
(defvar tags-case-fold-search)          ; In `etags.el'

;; (> emacs-major-version 21)
(defvar Info-saved-nodes)               ; In `info+.el'

;; (< emacs-major-version 23)
(defvar read-buffer-completion-ignore-case)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Icicles Commands for Other Packages")

;;; Icicles Commands for Other Packages ------------------------------

;; Put this first

(defun icicle-cmd2-after-load-bookmark+ ()
  "Things to do for `icicles-cmd2.el' after loading `bookmark+.el'."

  (icicle-define-file-command icicle-bookmark-a-file ; `C-x p c a'
    "Bookmark a file (create an autofile bookmark).
\(You need library `Bookmark+' for this command.)
When prompted for the file you can use `M-n' to pick up the file name
at point, or if none then the visited file.
The autofile bookmark created has the same name as the file.

During file-name completion\\<minibuffer-local-completion-map>:
 You can use `C-x a +' or `C-x a -' to add or remove tags from the
  current-candidate file.  You are prompted for the tags.
  (This action requires library `Bookmark+'.)
 You can use `C-c C-d' (a la `cd') to change the `default-directory'.
 You can use `C-c +' to create a new directory.
 You can use `\\[icicle-all-candidates-list-alt-action]' to open Dired on currently matching file names.
 You can use `\\[icicle-delete-candidate-object]' to delete a candidate file or (empty) dir."
    (lambda (file) (bmkp-bookmark-a-file file nil nil 'MSG))
    "File to bookmark (autofile): " nil nil nil nil nil ; `read-file-name' args
    (icicle-file-bindings               ; Bindings
     ((icicle-use-candidates-only-once-flag  t)
      ;; This binding is for `icicle-autofile-action', in `icicle-bind-file-candidate-keys'.
      (icicle-full-cand-fn                   #'icicle-make-bookmark-candidate)
      (icicle-all-candidates-list-alt-action-fn ; `M-|'
       (lambda (files) (let ((enable-recursive-minibuffers  t))
                         (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
    (icicle-bind-file-candidate-keys)   ; First code
    nil                                 ; Undo code
    (icicle-unbind-file-candidate-keys)) ; Last code

  (icicle-define-file-command icicle-tag-a-file ; `C-x p t + a'
    "Tag a file (an autofile bookmark) with one or more tags.
You are prompted for the tags, then the file name.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag.  Completion is lax: you are
not limited to existing tags.

When prompted for the file you can use `M-n' to pick up the file name
at point, or if none then the visited file.

The tags are added to an autofile bookmark for the same file name and
directory.  If the bookmark does not yet exist it is created.
Candidate help shows information about the file's autofile bookmark if
it already exists, or the file itself if not."
    (lambda (file) (bmkp-autofile-add-tags file tags nil nil 'MSG))
    "File to tag: " nil nil nil nil nil ; `read-file-name' args
    (icicle-file-bindings               ; Bindings
     ((tags                                  (bmkp-read-tags-completing))
      (icicle-use-candidates-only-once-flag  t))))

  (icicle-define-file-command icicle-untag-a-file ; `C-x p t - a'
    "Remove one or more tags from a file (an autofile bookmark).
You are prompted for the tags, then the file name.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag.  Completion is lax: you are
not limited to existing tags.

When prompted for the file you can use `M-n' to pick up the file name
at point, or if none then the visited file.

The tags are removed from an autofile bookmark for the same file name
and directory.  During file-name completion, only files tagged with
all of the given input tags are completion candidates."
    (lambda (file) (bmkp-autofile-remove-tags file tags nil nil 'MSG))
    "File to untag: " nil nil t nil (and icompletep  pred) ; `read-file-name' args
    (icicle-file-bindings               ; Bindings
     ((tags                                    (bmkp-read-tags-completing)) ; Pre bindings
      (icicle-use-candidates-only-once-flag    t))
     ((pred                                    (lambda (ff) ; Post bindings
                                                 ;; Expand relative file name, using dir from minibuffer.
                                                 (setq ff  (expand-file-name
                                                            ff (icicle-file-name-directory-w-default
                                                                (icicle-input-from-minibuffer))))
                                                 (let* ((bmk   (bmkp-get-autofile-bookmark ff))
                                                        (btgs  (and bmk  (bmkp-get-tags bmk))))
                                                   (and btgs  (catch 'icicle-untag-a-file
                                                                (dolist (tag  tags)
                                                                  (unless (member tag btgs)
                                                                    (throw 'icicle-untag-a-file nil)))
                                                                t)))))
      (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
      (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred)))))

  ;;$$$  Do not bother with autofiles that have a PREFIX.
  (icicle-define-command icicle-find-file-tagged ; `C-x j t C-f C-f'.
    "Find one or more files with tags that match your input.
By default, only tagged files are candidates.  With a prefix argument,
all autofiles are candidates.  (Autofiles are autofile bookmarks - you
need library `Bookmark+' for this command.)

Each completion candidate is a multi-completion composed of these
fields: an absolute file name plus the file's tags, all separated by
`icicle-list-join-string' (\"^G^J\", by default).  As always, you can
type `C-M-j' to insert this separator into the minibuffer.

For this command, by default `.' in your input matches any character,
including a newline.  As always, you can use `C-M-.' to toggle
this (so `.' does not match newline).

You can match your input against the file name or tags or both.

E.g., type:

 `red S-TAB'                    to match all files with the tag `red'
 `red M-SPC green M-SPC blue'   to match all files with tags `red',
                                `green', and `blue' (in any order)

That assumes that these tags do not also match any file names.

If you need to match against a particular field (e.g. the file name or
a specific tag position), then use the field separator.  Otherwise,
just use progressive completion, as shown above.

E.g., to match only tags and not the filename, start with `C-M-j' to
get past the file-name field.  To match both file name and tags, type
something to match the file name before the `C-M-j'.  E.g., type:

 `2011 C-M-j red M-SPC blue'    to match all files tagged `red' and
                                `blue' that have `2011' in their names

During completion (`*': requires library `Bookmark+')\\<minibuffer-local-completion-map>:

 *You can use `C-x a +' or `C-x a -' to add or remove tags from the
   current-candidate file.  You are prompted for the tags.
 *You can use `C-x m' to access file bookmarks (not just autofiles).
  You can use `C-c C-d' (a la `cd') to change the `default-directory'.
  You can use `C-c +' to create a new directory.
  You can use `\\[icicle-all-candidates-list-alt-action]' to open Dired on currently matching file names.
  You can use `\\[icicle-delete-candidate-object]' to delete a candidate file or (empty) dir." ; Doc string
    (lambda (f) (bmkp-find-file (icicle-transform-multi-completion f) 'WILDCARDS)) ; Action function
    prompt icicle-abs-file-candidates   ; `completing-read' args
    nil nil nil 'icicle-filetags-history nil nil
    (icicle-file-bindings               ; Bindings
     ((prompt                             "FILE `C-M-j' TAGS: ")
      ;; This binding is for `icicle-autofile-action', in `icicle-bind-file-candidate-keys'.
      (icicle-full-cand-fn                    (lambda (file)
                                                (list (cons file (bmkp-get-tags
                                                                  (bmkp-get-autofile-bookmark file))))))
      (icicle-abs-file-candidates       ; An alist whose items are ((FILE TAG...)).
       (let ((result  ()))
         (dolist (autofile  (bmkp-autofile-alist-only))
           (let ((tags  (bmkp-get-tags autofile)))
             (when (or tags  current-prefix-arg)
               (push (list (cons (bookmark-get-filename autofile) tags)) result))))
         result))
      (icicle-dot-string                      (icicle-anychar-regexp))
      (icicle-candidate-properties-alist      '((1 (face icicle-candidate-part))))
      (icicle-multi-completing-p              t)
      (icicle-list-use-nth-parts              '(1))
      (icicle-whole-candidate-as-text-prop-p  t)))
    (progn                              ; First code
      (put-text-property 0 1 'icicle-fancy-candidates t prompt)
      (icicle-highlight-lighter)
      (message "Gathering tagged files...")
      (icicle-bind-file-candidate-keys))
    nil                                 ; Undo code
    (icicle-unbind-file-candidate-keys)) ; Last code

  (icicle-define-command icicle-find-file-tagged-other-window ; `C-x 4 j t C-f C-f'
    "Same as `icicle-find-file-tagged', except uses another window." ; Doc string
    (lambda (f) (bmkp-find-file-other-window (icicle-transform-multi-completion f) 'WILDCARDS)) ; Action
    prompt icicle-abs-file-candidates   ; `completing-read' args
    nil nil nil 'icicle-filetags-history nil nil
    (icicle-file-bindings               ; Bindings
     ((prompt                                 "FILE `C-M-j' TAGS: ")
      ;; This binding is for `icicle-autofile-action', in `icicle-bind-file-candidate-keys'.
      (icicle-full-cand-fn                    (lambda (file)
                                                (list (cons file (bmkp-get-tags
                                                                  (bmkp-get-autofile-bookmark file))))))
      (icicle-abs-file-candidates       ; An alist whose items are ((FILE TAG...)).
       (let ((result  ()))
         (dolist (autofile  (bmkp-autofile-alist-only))
           (let ((tags  (bmkp-get-tags autofile)))                  
             (when (or tags  current-prefix-arg)
               (push (list (cons (bookmark-get-filename autofile) tags)) result))))
         result))
      (icicle-dot-string                      (icicle-anychar-regexp))
      (icicle-candidate-properties-alist      '((1 (face icicle-candidate-part))))
      (icicle-multi-completing-p              t)
      (icicle-list-use-nth-parts              '(1))
      (icicle-whole-candidate-as-text-prop-p  t)))
    (progn                              ; First code
      (put-text-property 0 1 'icicle-fancy-candidates t prompt)
      (icicle-highlight-lighter)
      (message "Gathering tagged files...")
      (icicle-bind-file-candidate-keys))
    nil                                 ; Undo code
    (icicle-unbind-file-candidate-keys)) ; Last code

  (icicle-define-file-command icicle-find-file-handle-bookmark ; `C-x j C-f'
    "Visit a file or directory, respecting any associated autofile handlers.
This is similar to `icicle-find-file', But the file is accessed using
`bmkp-find-file', which means that if it has an associated handler in
`bmkp-default-handlers-for-file-types' then that handler is used to
visit the file.

If you use a prefix arg when acting on a completion candidate then an
autofile bookmark is created for the file, unless it already has one.

When prompted for the file name you can use `M-n' to pick up the file
name at point, or if none then the visited file."
    bmkp-find-file
    "Find file: " nil nil t nil nil     ; `read-file-name' args
    (icicle-file-bindings               ; Bindings
     ((init-pref-arg  current-prefix-arg) ; Pre bindings
      (icicle-all-candidates-list-alt-action-fn ; `M-|'
       (lambda (files) (let ((enable-recursive-minibuffers  t))
                         (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
    (icicle-bind-file-candidate-keys)   ; First code.
    nil                                 ; Undo code.
    (icicle-unbind-file-candidate-keys)) ; Last code.

  (icicle-define-file-command icicle-find-file-handle-bookmark-other-window ; `C-x 4 j C-f'
    "Same as `icicle-find-file-handle-bookmark', except uses another window."
    bmkp-find-file-other-window
    "Find file: " nil nil t nil nil     ; `read-file-name' args
    (icicle-file-bindings               ; Bindings
     ((init-pref-arg  current-prefix-arg) ; Pre bindings
      (icicle-all-candidates-list-alt-action-fn ; `M-|'
       (lambda (files) (let ((enable-recursive-minibuffers  t))
                         (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
    (icicle-bind-file-candidate-keys)   ; First code.
    nil                                 ; Undo code.
    (icicle-unbind-file-candidate-keys)) ; Last code.

  (icicle-define-file-command icicle-find-file-all-tags ; `C-x j t C-f *'
    "Visit a file or directory that has all of the tags you enter.
Only tagged autofiles are candidates.

This is essentially a multi-command versions of `bmkp-find-file-all-tags'.

You are prompted first for the tags.  Hit `RET' to enter each tag,
then hit `RET' again after the last tag.  You can use completion to
enter each tag.  This completion is lax: you are not limited to
existing tags.

By default, the tag choices for completion are NOT refreshed, to save
time.  Use a prefix argument if you want to refresh them.

You are then prompted for the file name.  This is read using
`read-file-name', so you can browse up and down the file hierarchy.
\(The completion candidates are file names, not bookmark names.)

If you specify no tags, then every file that has some tags is a
candidate.

When prompted for the file you can use `M-n' to pick up the file name
at point, or if none then the visited file."
    (lambda (file) (bmkp-find-file file 'MUST-EXIST)) ; Function to perform the action
    "Find file: " nil nil t nil (and icompletep  pred) ; `read-file-name' args
    (icicle-file-bindings               ; Bindings
     ((tags                                    (bmkp-read-tags-completing ; Pre bindings
                                                nil nil current-prefix-arg))
      (icicle-all-candidates-list-alt-action-fn ; `M-|'
       (lambda (files) (let ((enable-recursive-minibuffers  t))
                         (dired-other-window (cons (read-string "Dired buffer name: ") files))))))
     ((pred                                    (lambda (ff) ; Post bindings
                                                 (let* ((bmk   (bmkp-get-autofile-bookmark ff))
                                                        (btgs  (and bmk  (bmkp-get-tags bmk))))
                                                   (and btgs  (bmkp-every (lambda (tag)
                                                                            (bmkp-has-tag-p bmk tag))
                                                                          tags)))))
      (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
      (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))))
    (icicle-bind-file-candidate-keys)   ; First code.
    nil                                 ; Undo code.
    (icicle-unbind-file-candidate-keys)) ; Last code.

  (icicle-define-file-command icicle-find-file-all-tags-other-window ; `C-x 4 j t C-f *'
    "Same as `icicle-find-file-all-tags', except uses another window."
    (lambda (file) (bmkp-find-file-other-window file 'MUST-EXIST)) ; Function to perform the action
    "Find file: " nil nil t nil (and icompletep  pred) ; `read-file-name' args
    (icicle-file-bindings               ; Bindings
     ((tags                                    (bmkp-read-tags-completing ; Pre bindings
                                                nil nil current-prefix-arg))
      (icicle-all-candidates-list-alt-action-fn ; `M-|'
       (lambda (files) (let ((enable-recursive-minibuffers  t))
                         (dired-other-window (cons (read-string "Dired buffer name: ") files))))))
     ((pred                                    (lambda (ff) ; Post bindings
                                                 (let* ((bmk   (bmkp-get-autofile-bookmark ff))
                                                        (btgs  (and bmk  (bmkp-get-tags bmk))))
                                                   (and btgs  (bmkp-every (lambda (tag)
                                                                            (bmkp-has-tag-p bmk tag))
                                                                          tags)))))
      (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
      (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))))
    (icicle-bind-file-candidate-keys)   ; First code.
    nil                                 ; Undo code.
    (icicle-unbind-file-candidate-keys)) ; Last code.

  (icicle-define-file-command icicle-find-file-all-tags-regexp ; `C-x j t C-f % *'
    "Visit a file or directory that has each tag matching a regexp you enter.
When prompted for the file you can use `M-n' to pick up the file name
at point, or if none then the visited file."
    (lambda (file) (bmkp-find-file file 'MUST-EXIST)) ; Function to perform the action
    "Find file: " nil nil t nil (and icompletep  pred) ; `read-file-name' args
    (icicle-file-bindings               ; Bindings
     ((regexp                                  (read-string "Regexp for tags: ")) ; Pre bindings
      (icicle-all-candidates-list-alt-action-fn ; `M-|'
       (lambda (files) (let ((enable-recursive-minibuffers  t))
                         (dired-other-window (cons (read-string "Dired buffer name: ") files))))))
     ((pred                                    (lambda (ff) ; Post bindings
                                                 (let* ((bmk   (bmkp-get-autofile-bookmark ff))
                                                        (btgs  (and bmk  (bmkp-get-tags bmk))))
                                                   (and btgs  (bmkp-every (lambda (tag)
                                                                            (string-match
                                                                             regexp (bmkp-tag-name tag)))
                                                                          btgs)))))
      (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
      (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))))
    (icicle-bind-file-candidate-keys)   ; First code.
    nil                                 ; Undo code.
    (icicle-unbind-file-candidate-keys)) ; Last code.

  (icicle-define-file-command icicle-find-file-all-tags-regexp-other-window ; `C-x 4 j t C-f % *'
    "Same as `icicle-find-file-all-tags-regexp', except uses another window."
    (lambda (file) (bmkp-find-file-other-window file 'MUST-EXIST)) ; Function to perform the action
    "Find file: " nil nil t nil (and icompletep  pred) ; `read-file-name' args
    (icicle-file-bindings               ; Bindings
     ((regexp                                  (read-string "Regexp for tags: ")) ; Pre bindings
      (icicle-all-candidates-list-alt-action-fn ; `M-|'
       (lambda (files) (let ((enable-recursive-minibuffers  t))
                         (dired-other-window (cons (read-string "Dired buffer name: ") files))))))
     ((pred                                    (lambda (ff) ; Post bindings
                                                 (let* ((bmk   (bmkp-get-autofile-bookmark ff))
                                                        (btgs  (and bmk  (bmkp-get-tags bmk))))
                                                   (and btgs  (bmkp-every (lambda (tag)
                                                                            (string-match
                                                                             regexp (bmkp-tag-name tag)))
                                                                          btgs)))))
      (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
      (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))))
    (icicle-bind-file-candidate-keys)   ; First code.
    nil                                 ; Undo code.
    (icicle-unbind-file-candidate-keys)) ; Last code.

  (icicle-define-file-command icicle-find-file-some-tags ; `C-x j t C-f +'
    "Visit a file or directory that has at least one of the tags you enter.
You are prompted for the tags, then the file name.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag.  Completion is lax: you are
not limited to existing tags.

When prompted for the file you can use `M-n' to pick up the file name
at point, or if none then the visited file."
    (lambda (file) (bmkp-find-file file 'MUST-EXIST)) ; Function to perform the action
    "Find file: " nil nil t nil (and icompletep  pred) ; `read-file-name' args
    (icicle-file-bindings               ; Bindings
     ((tags                                    (bmkp-read-tags-completing ; Pre bindings
                                                nil nil current-prefix-arg))
      (icicle-all-candidates-list-alt-action-fn ; `M-|'
       (lambda (files) (let ((enable-recursive-minibuffers  t))
                         (dired-other-window (cons (read-string "Dired buffer name: ") files))))))
     ((pred                                    (lambda (ff) ; Post bindings
                                                 (let* ((bmk   (bmkp-get-autofile-bookmark ff))
                                                        (btgs  (and bmk  (bmkp-get-tags bmk))))
                                                   (and btgs  (bmkp-some (lambda (tag)
                                                                           (bmkp-has-tag-p bmk tag))
                                                                         tags)))))
      (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
      (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))))
    (icicle-bind-file-candidate-keys)   ; First code.
    nil                                 ; Undo code.
    (icicle-unbind-file-candidate-keys)) ; Last code.

  (icicle-define-file-command icicle-find-file-some-tags-other-window ; `C-x 4 j t C-f +'
    "Same as `icicle-find-file-some-tags', except uses another window."
    (lambda (file) (bmkp-find-file-other-window file 'MUST-EXIST)) ; Function to perform the action
    "Find file: " nil nil t nil (and icompletep  pred) ; `read-file-name' args
    (icicle-file-bindings               ; Bindings
     ((tags                                    (bmkp-read-tags-completing ; Pre bindings
                                                nil nil current-prefix-arg))
      (icicle-all-candidates-list-alt-action-fn ; `M-|'
       (lambda (files) (let ((enable-recursive-minibuffers  t))
                         (dired-other-window (cons (read-string "Dired buffer name: ") files))))))
     ((pred                                    (lambda (ff) ; Post bindings
                                                 (let* ((bmk   (bmkp-get-autofile-bookmark ff))
                                                        (btgs  (and bmk  (bmkp-get-tags bmk))))
                                                   (and btgs  (bmkp-some (lambda (tag)
                                                                           (bmkp-has-tag-p bmk tag))
                                                                         tags)))))
      (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
      (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))))
    (icicle-bind-file-candidate-keys)   ; First code.
    nil                                 ; Undo code.
    (icicle-unbind-file-candidate-keys)) ; Last code.

  (icicle-define-file-command icicle-find-file-some-tags-regexp ; `C-x j t C-f % +'
    "Visit a file or directory that has a tag matching a regexp you enter.
When prompted for the file you can use `M-n' to pick up the file name
at point, or if none then the visited file."
    (lambda (file) (bmkp-find-file-other-window file 'MUST-EXIST)) ; Function to perform the action
    "Find file: " nil nil t nil (and icompletep  pred) ; `read-file-name' args
    (icicle-file-bindings               ; Bindings
     ((regexp                                  (read-string "Regexp for tags: ")) ; Pre bindings
      (icicle-all-candidates-list-alt-action-fn ; `M-|'
       (lambda (files) (let ((enable-recursive-minibuffers  t))
                         (dired-other-window (cons (read-string "Dired buffer name: ") files))))))
     ((pred                                    (lambda (ff) ; Post bindings
                                                 (let* ((bmk   (bmkp-get-autofile-bookmark ff))
                                                        (btgs  (and bmk  (bmkp-get-tags bmk))))
                                                   (and btgs  (bmkp-some (lambda (tag)
                                                                           (string-match
                                                                            regexp (bmkp-tag-name tag)))
                                                                         btgs)))))
      (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
      (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))))
    (icicle-bind-file-candidate-keys)   ; First code.
    nil                                 ; Undo code.
    (icicle-unbind-file-candidate-keys)) ; Last code.

  (icicle-define-file-command icicle-find-file-some-tags-regexp-other-window ; `C-x 4 j t C-f % +'
    "Same as `icicle-find-file-some-tags-regexp', except uses another window."
    (lambda (file) (bmkp-find-file-other-window file 'MUST-EXIST)) ; Function to perform the action
    "Find file: " nil nil t nil (and icompletep  pred) ; `read-file-name' args
    (icicle-file-bindings               ; Bindings
     ((regexp                                  (read-string "Regexp for tags: ")) ; Pre bindings
      (icicle-all-candidates-list-alt-action-fn ; `M-|'
       (lambda (files) (let ((enable-recursive-minibuffers  t))
                         (dired-other-window (cons (read-string "Dired buffer name: ") files))))))
     ((pred                                    (lambda (ff) ; Post bindings
                                                 (let* ((bmk   (bmkp-get-autofile-bookmark ff))
                                                        (btgs  (and bmk  (bmkp-get-tags bmk))))
                                                   (and btgs  (bmkp-some (lambda (tag)
                                                                           (string-match
                                                                            regexp (bmkp-tag-name tag)))
                                                                         btgs)))))
      (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
      (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))))
    (icicle-bind-file-candidate-keys)   ; First code.
    nil                                 ; Undo code.
    (icicle-unbind-file-candidate-keys)) ; Last code.
  )


(defun icicle-cmd2-after-load-hexrgb ()
  "Things to do for `icicles-cmd2.el' after loading `hexrgb.el'."
  (when (and (fboundp 'read-color)  (not (fboundp 'icicle-ORIG-read-color))) ; Exists with Emacs 23+.
    (defalias 'icicle-ORIG-read-color (symbol-function 'read-color))) ; Not used, but save it anyway.

  ;; See also `hexrgb-read-color' in `hexrgb.el'.
  (defun icicle-read-color (&optional prompt convert-to-RGB-p allow-empty-name-p msgp)
    "Read a color name or hex RGB hexadecimal color value #RRRRGGGGBBBB.
Return the name or the RGB hex string for the chosen color.

By default (see option `icicle-functions-to-redefine'), this is used
in place of standard command `read-color' when you are in Icicle mode,
so that any existing code that calls that command invokes this one
instead.

`icicle-read-color' has the advantage of being an Icicles
multi-command that provides WYSIWYG completion, color-variable proxy
candidates, alternate candidate actions, candidate help, and multiple
color-related candidate sort orders.

In this it is like command `icicle-read-color-wysiwyg' (which see),
but it is less flexible and powerful than that command.
`icicle-read-color' always returns the RGB hex string, and your input
cannot be an arbitrary string - it must match a color candidate (or be
empty if ALLOW-EMPTY-NAME-P is non-nil).

In Lisp code that you write, and for interactive use,
`icicle-read-color-wysiwyg' is generally a better choice than
`icicle-read-color'.

Optional argument PROMPT is the prompt to use (default \"Color: \").

Interactively, or if CONVERT-TO-RGB-P is non-nil, return the RGB hex
string for the chosen color.  If nil, return the color name.

Optional arg ALLOW-EMPTY-NAME-P controls what happens if you enter an
empty color name (that is, you just hit `RET').  If non-nil, then
`icicle-read-color' returns an empty color name, \"\".  If nil, then
it raises an error.  Calling programs must test for \"\" if
ALLOW-EMPTY-NAME-P is non-nil.  They can then perform an appropriate
action in case of empty input.

Interactively, or with non-nil MSGP, show chosen color in echo area."
    (interactive "i\np\ni\np")          ; Always convert to RGB interactively.
    (let* ((icicle-require-match-p  (not allow-empty-name-p))
           (color                   (icicle-read-color-wysiwyg (if convert-to-RGB-p 2 1) prompt)))
      ;; `icicle-read-color-wysiwyg' transforms input to get the 2nd multi-completion component.
      ;; But if the user did not complete but just entered a color name, then this transformation returns "".
      ;; In that case, get the color name that was input from the input history, and convert that to RGB.
      (when (and (not allow-empty-name-p)  (string= "" color))
        (let ((col  (car-safe (symbol-value minibuffer-history-variable))))
          (if (or (equal "" col)  (not (stringp col)))
              (error "No such color: %S" color)
            (setq color  (hexrgb-color-name-to-hex col)))))
      (let ((icicle-multi-completing-p  t)
            (icicle-list-use-nth-parts  (if convert-to-RGB-p '(2) '(1)))
            (colr                       (icicle-transform-multi-completion color)))
        (when msgp (message "Color: `%s'" (icicle-propertize colr 'face 'icicle-msg-emphasis)))
        colr)))

  (defun icicle-read-color-wysiwyg (&optional arg prompt initial-input msgp)
    "Read a color name or hex RGB color value #RRRRGGGGBBBB.
Return a string value.
Interactively, optional argument ARG is the prefix arg - see below.
Optional argument PROMPT is the prompt to use (default \"Color: \").
Optional argument INITIAL-INPUT is a initial input to insert in the
minibuffer for completion.  It is passed to `completing-read'.
Interactively, or with non-nil MSGP, show chosen color in echo area.

In addition to standard color names and RGB (red, green, blue) hex
values, the following are also available as proxy color candidates,
provided `icicle-add-proxy-candidates-flag' is non-nil and library
`palette.el' or `eyedropper.el' is used.  In each case, the
corresponding color is used.

* `*copied foreground*'  - last copied foreground, if available
* `*copied background*'  - last copied background, if available
* `*mouse-2 foreground*' - foreground where you click `mouse-2'
* `*mouse-2 background*' - background where you click `mouse-2'
* `*point foreground*'   - foreground under the text cursor
* `*point background*'   - background under the text cursor

\(You can copy a color using eyedropper commands such as
`eyedrop-pick-foreground-at-mouse'.)

In addition, the names of user options (variables) whose custom type
is `color' are also proxy candidates, but with `'' as a prefix and
suffix.  So, for example, option `icicle-region-background' appears as
proxy color candidate `'icicle-region-background''.  If you choose
such a candidate then (only) the variable's value is returned.

As always, you can toggle the use of proxy candidates using `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-proxy-candidates]' in
the minibuffer.

With plain `C-u', use `hexrgb-read-color', which lets you complete a
color name or input any valid RGB hex value (without completion).

With no prefix arg, return a string with both the color name and the
RGB value, separated by `icicle-list-nth-parts-join-string'.

With a numeric prefix arg of 0 or 1, return the color name.  With any
other numeric prefix arg, return the RGB value.

In the plain `C-u' case, your input is checked to ensure that it
represents a valid color.

In all other cases:

- You can complete your input against the color name, the RGB value,
  or both.

- If you enter input without completing or cycling, the input is not
  checked: whatever is entered is returned as the string value.

You can, as usual in Icicles, use \\<minibuffer-local-completion-map>`\\[icicle-change-sort-order]' \
to cycle among various sort
orders.  There is a rich variety of orders, including HSV and RGB
distance from a color you specify.

From Emacs Lisp, ARG controls what is returned.  If ARG is nil,
`icicle-list-use-nth-parts' can also be used to control the behavior.

Note: Duplicate color names are removed by downcasing and removing
whitespace.  For example, \"AliceBlue\" and \"alice blue\" are both
treated as \"aliceblue\".  Otherwise, candidates with different names
but the same RGB values are not considered duplicates, so, for
example, input can match either \"darkred\" or \"red4\", which both
have RGB #8b8b00000000.  You can toggle duplicate removal at any time
using `\\[icicle-toggle-transforming]'.

During completion, candidate help (e.g. `\\[icicle-help-on-candidate]') shows you the RGB
and HSV (hue, saturation, value) color components.

This command is intended only for use in Icicle mode (but it can be
used with `C-u', with Icicle mode turned off)."
    (interactive "P\ni\ni\np")
    (unless (featurep 'hexrgb) (error "`icicle-read-color-wysiwyg' requires library `hexrgb.el'"))
    (let (color)
      (if (consp arg)                   ; Plain `C-u': complete against color name only, and be able to
          (setq color  (hexrgb-read-color nil 'CONVERT-TO-RGB)) ; input any valid RGB string.

        ;; Complete against name+RGB pairs, but user can enter invalid value without completing.
        (when arg (setq arg  (prefix-numeric-value arg))) ; Convert `-' to -1.
        (let ((icicle-multi-completing-p  t)
              (icicle-list-use-nth-parts
               (or (and arg  (if (< arg 2) '(1) '(2))) ; 1 or 2, either by program or via `C-1' or `C-2'.
                   icicle-list-use-nth-parts ; Bound externally by program.
                   '(1 2)))             ; Both parts, by default.
              (mouse-pseudo-color-p  nil)

              icicle-candidate-help-fn           completion-ignore-case
              icicle-transform-function          icicle-sort-orders-alist
              icicle-list-nth-parts-join-string  icicle-list-join-string
              ;; $$$$$$ icicle-list-end-string
              icicle-proxy-candidate-regexp      icicle-named-colors
              icicle-proxy-candidates)
          ;; Copy the prompt string because `icicle-color-completion-setup' puts a text prop on it.
          ;; Use `icicle-prompt' from now on, since that's what `icicle-color-completion-setup'
          ;; sets up.
          (setq icicle-prompt  (copy-sequence (or prompt  "Color: ")))
          (icicle-color-completion-setup)
          (setq icicle-proxy-candidates
                (append icicle-proxy-candidates
                        (mapcar         ; Convert multi-completions to strings.
                         ;; $$$$$$ (lambda (entry)
                         ;;         (concat (mapconcat #'identity (car entry)
                         ;;                            icicle-list-join-string)
                         ;;                 icicle-list-end-string)) ; $$$$$$
                         (lambda (entry) (mapconcat #'identity (car entry) icicle-list-join-string))
                         '((("*mouse-2 foreground*")) (("*mouse-2 background*")))))
                color  (let ((icicle-orig-window  (selected-window))
                             (icicle-candidate-alt-action-fn
                              (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "color")))
                             (icicle-all-candidates-list-alt-action-fn
                              (or icicle-all-candidates-list-alt-action-fn
                                  (icicle-alt-act-fn-for-type "color"))))
                         (completing-read icicle-prompt icicle-named-colors nil nil initial-input)))
          (cond ((string-match "\\`'.+': " color)
                 (let ((icicle-list-nth-parts-join-string  ": ")
                       (icicle-list-join-string            ": ")
                       (icicle-list-use-nth-parts          '(2)))
                   (setq color  (icicle-transform-multi-completion color))))
                ((fboundp 'eyedrop-foreground-at-point)
                 (cond ((string-match "^\*mouse-2 foreground\*" color)
                        (setq color  (prog1 (eyedrop-foreground-at-mouse
                                             (read-event
                                              "Click `mouse-2' anywhere to choose foreground color"))
                                       (read-event)) ; Discard mouse up event.
                              mouse-pseudo-color-p  t))
                       ((string-match "^\*mouse-2 background\*" color)
                        (setq color  (prog1 (eyedrop-background-at-mouse
                                             (read-event
                                              "Click `mouse-2' anywhere to choose background color"))
                                       (read-event)) ; Discard mouse up event.
                              mouse-pseudo-color-p  t)))
                 (if mouse-pseudo-color-p
                     (let ((icicle-list-nth-parts-join-string  ": ")
                           (icicle-list-join-string            ": ")
                           ;; $$$$$$ (icicle-list-end-string             "")
                           (icicle-list-use-nth-parts
                            (or (and arg  (if (< arg 2) '(1) '(2))) ; 1 or 2, via program or `C-1' or `C-2'.
                                icicle-list-use-nth-parts ; Bound externally by program.
                                '(1 2)))) ; Both parts, by default.
                       (setq color  (icicle-transform-multi-completion
                                     (concat color ": " (hexrgb-color-name-to-hex color)))))
                   (setq color  (icicle-transform-multi-completion color)))))
                   (when (string= "" color)
                     (let ((col  (car-safe (symbol-value minibuffer-history-variable))))
                       (if (or (equal "" col)  (not (stringp col)))
                           (error "No such color: %S" color)
                         ;; Cannot use `case', since that uses `eql', not `equal'.
                         (setq color  (cond ((equal '(1) icicle-list-use-nth-parts)  col)
                                            ((equal '(2) icicle-list-use-nth-parts)
                                             (hexrgb-color-name-to-hex col))
                                            (t  (let ((icicle-list-nth-parts-join-string  ": ")
                                                      (icicle-list-join-string            ": "))
                                                  (icicle-transform-multi-completion color))))))))))
      (when msgp (message "Color: `%s'" (icicle-propertize color 'face 'icicle-msg-emphasis)))
      color))

  (icicle-define-command icicle-frame-bg ; Command name
    "Change background of current frame.
Read color name or hex RGB color value #RRRRGGGGBBBB with completion.
In addition to standard color names and RGB (red, green, blue) hex
values, the following are also available as proxy color candidates,
provided `icicle-add-proxy-candidates-flag' is non-nil and library
`palette.el' or `eyedropper.el' is used.  In each case, the
corresponding color is used.

* `*copied foreground*'  - last copied foreground, if available
* `*copied background*'  - last copied background, if available
* `*point foreground*'   - foreground under the text cursor
* `*point background*'   - background under the text cursor

\(You can copy a color using eyedropper commands such as
`eyedrop-pick-foreground-at-mouse'.)

In addition, the names of user options (variables) whose custom type
is `color' are also proxy candidates, but with `'' as a prefix and
suffix.  So, for example, option `icicle-region-background' appears as
proxy color candidate `'icicle-region-background''.

As always, you can toggle the use of proxy candidates using `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-proxy-candidates]' in
the minibuffer.

You can complete your input against the color name, the RGB value, or
both.

Note: Duplicate color names are removed by downcasing and removing
whitespace.  For example, \"AliceBlue\" and \"alice blue\" are both
treated as \"aliceblue\".  Otherwise, candidates with different names
but the same RGB values are not considered duplicates, so, for
example, input can match either \"darkred\" or \"red4\", which both
have RGB #8b8b00000000.  You can toggle duplicate removal at any time
using \\<minibuffer-local-completion-map>`\\[icicle-toggle-transforming]'.

During completion, candidate help (e.g. `\\[icicle-help-on-candidate]') shows you the RGB
and HSV (hue, saturation, value) color components.

This command is intended only for use in Icicle mode." ; Doc string
    (lambda (color)                     ; Action function
      (modify-frame-parameters
       icicle-orig-frame (list (cons 'background-color (icicle-transform-multi-completion color)))))
    icicle-prompt icicle-named-colors nil t nil ; `completing-read' args
    (if (boundp 'color-history) 'color-history 'icicle-color-history) nil nil
    ((icicle-orig-frame                  (selected-frame)) ; Bindings
     (orig-bg                            (frame-parameter nil 'background-color))
     (icicle-prompt                      "Background color: ")
     (icicle-multi-completing-p          t)
     (icicle-list-use-nth-parts          '(2)) ; Use RGB part.
     (icicle-candidate-alt-action-fn
      (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "color")))
     (icicle-all-candidates-list-alt-action-fn
      (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "color")))

     icicle-candidate-help-fn     completion-ignore-case             icicle-transform-function
     icicle-sort-orders-alist     icicle-list-nth-parts-join-string  icicle-list-join-string
     ;; $$$$$$ icicle-list-end-string
     icicle-proxy-candidate-regexp      icicle-named-colors          icicle-proxy-candidates)
    (icicle-color-completion-setup)     ; First code - needs `hexrgb.el'
    (modify-frame-parameters icicle-orig-frame (list (cons 'background-color orig-bg))) ; Undo code
    nil)                                ; Last code

  (icicle-define-command icicle-frame-fg ; Command name
    "Change foreground of current frame.
See `icicle-frame-bg' - but this is for foreground, not background." ; Doc string
    (lambda (color)                     ; Action function
      (modify-frame-parameters
       icicle-orig-frame (list (cons 'foreground-color (icicle-transform-multi-completion color)))))
    icicle-prompt icicle-named-colors nil t nil ; `completing-read' args
    (if (boundp 'color-history) 'color-history 'icicle-color-history) nil nil
    ((icicle-orig-frame                  (selected-frame)) ; Bindings
     (orig-bg                            (frame-parameter nil 'foreground-color))
     (icicle-prompt                      "Foreground color: ")
     (icicle-multi-completing-p          t)
     (icicle-list-use-nth-parts          '(2)) ; Use RGB part.
     (icicle-candidate-alt-action-fn
      (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "color")))
     (icicle-all-candidates-list-alt-action-fn
      (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "color")))

     icicle-candidate-help-fn     completion-ignore-case             icicle-transform-function
     icicle-sort-orders-alist     icicle-list-nth-parts-join-string  icicle-list-join-string
     ;; $$$$$$ icicle-list-end-string
     icicle-proxy-candidate-regexp      icicle-named-colors          icicle-proxy-candidates)
    (icicle-color-completion-setup)     ; First code - needs `hexrgb.el'
    (modify-frame-parameters icicle-orig-frame (list (cons 'foreground-color orig-bg))) ; Undo code
    nil)                                ; Last code

  ;; Free vars here:
  ;; `icicle-prompt', `icicle-candidate-help-fn', `completion-ignore-case',
  ;; `icicle-transform-function', `icicle-sort-orders-alist', `icicle-list-nth-parts-join-string',
  ;; `icicle-list-join-string', `icicle-proxy-candidate-regexp', `icicle-named-colors',
  ;; `icicle-proxy-candidates'.
  (defun icicle-color-completion-setup ()
    "Set up for color-name/RGB-value completion (helper function).
Sets these variables, which are assumed to be already `let'-bound:
  `icicle-prompt'
  `icicle-candidate-help-fn'
  `completion-ignore-case'
  `icicle-transform-function'
  `icicle-sort-orders-alist'
  `icicle-list-nth-parts-join-string'
  `icicle-list-join-string'
  `icicle-proxy-candidate-regexp'
  `icicle-named-colors'
  `icicle-proxy-candidates'
Puts property `icicle-fancy-candidates' on string `icicle-prompt'."
    (if (< emacs-major-version 22)
        (require 'eyedropper nil t)
      (or (require 'palette nil t)  (require 'eyedropper nil t)))
    (when (stringp icicle-prompt)       ; Sanity check - should be true.
      (put-text-property 0 1 'icicle-fancy-candidates t icicle-prompt))
    (icicle-highlight-lighter)
    (setq icicle-candidate-help-fn           'icicle-color-help
          completion-ignore-case             t
          icicle-sort-orders-alist
          '(("by color name" . icicle-part-1-lessp)
            ("by color hue"  . (lambda (s1 s2) (not (icicle-color-hue-lessp s1 s2))))
            ("by color purity (saturation)"
             . (lambda (s1 s2) (not (icicle-color-saturation-lessp s1 s2))))
            ("by color brightness (value)"
             . (lambda (s1 s2) (not (icicle-color-value-lessp s1 s2))))
            ("by color hsv"       . (lambda (s1 s2) (not (icicle-color-hsv-lessp s1 s2))))
            ("by hsv distance"    . (lambda (s1 s2) (icicle-color-distance-hsv-lessp s1 s2)))
            ("by amount of red"   . (lambda (s1 s2) (not (icicle-color-red-lessp s1 s2))))
            ("by amount of green" . (lambda (s1 s2) (not (icicle-color-green-lessp s1 s2))))
            ("by amount of blue"  . (lambda (s1 s2) (not (icicle-color-blue-lessp s1 s2))))
            ("by color rgb"       . (lambda (s1 s2) (not (icicle-color-rgb-lessp s1 s2))))
            ("by rgb distance"    . (lambda (s1 s2) (icicle-color-distance-rgb-lessp s1 s2)))
            ("turned OFF"))
          ;; Make the two `*-join-string' variables the same, so past inputs are recognized.
          ;; Do not use " " as the value, because color names such as "white smoke" would be
          ;; split, and "smoke" would not be recognized as a color name when trying to list
          ;; candidates in `*Completions*'.
          icicle-list-nth-parts-join-string  ": "
          icicle-list-join-string            ": "
          ;; $$$$$$ icicle-list-end-string             ""
          icicle-proxy-candidate-regexp      "^[*'].+[*']"

          icicle-named-colors                (mapcar #'icicle-make-color-candidate
                                                     (hexrgb-defined-colors))
          icicle-proxy-candidates
          (mapcar                       ; Convert multi-completions to strings.
           (lambda (entry)
             ;; $$$$$$ (concat (mapconcat #'identity (car entry) icicle-list-join-string)
             ;;                icicle-list-end-string) ; $$$$$$
             (mapconcat #'identity (car entry) icicle-list-join-string))
           (append
            (and (fboundp 'eyedrop-foreground-at-point)
                 (append (and eyedrop-picked-foreground ; Multi-completions.
                              `(,(icicle-make-color-candidate
                                  "*copied foreground*" (downcase (hexrgb-color-name-to-hex
                                                                   eyedrop-picked-foreground)))))
                         (and eyedrop-picked-background
                              `(,(icicle-make-color-candidate
                                  "*copied background*" (downcase (hexrgb-color-name-to-hex
                                                                   eyedrop-picked-background)))))
                         `(,(icicle-make-color-candidate
                             "*point foreground*" (downcase (hexrgb-color-name-to-hex
                                                             (eyedrop-foreground-at-point))))
                           ,(icicle-make-color-candidate
                             "*point background*" (downcase (hexrgb-color-name-to-hex
                                                             (eyedrop-background-at-point)))))))
            (let ((ipc  ()))
              (mapatoms
               (lambda (cand)
                 (when (and (user-variable-p cand)
                            (condition-case nil (icicle-var-is-of-type-p cand '(color)) (error nil))
                            ;; This should not be necessary, but type `color' isn't
                            ;; enforced - it just means `string' (so far).
                            (x-color-defined-p (symbol-value cand)))
                   (push `,(icicle-make-color-candidate
                            (concat "'" (symbol-name cand) "'")
                            (downcase (hexrgb-color-name-to-hex (symbol-value cand))))
                         ipc))))
              ipc)))))

  (defun icicle-color-help (color)
    "Display help on COLOR.
COLOR is a color name, an RGB string, or a multi-completion of both.
If only a color name, then just say \"No help\"."
    (if (not (member icicle-list-use-nth-parts '((1 2) (2))))
        (icicle-msg-maybe-in-minibuffer "No help")
      (with-output-to-temp-buffer "*Help*"
        (princ (format "Color: %s" color)) (terpri) (terpri)
        (let* ((icicle-list-use-nth-parts  '(2))
               (colr                       (icicle-transform-multi-completion color))
               (rgb                        (hexrgb-hex-to-rgb colr))
               (hsv                        (apply #'hexrgb-rgb-to-hsv rgb)))
          (princ "RGB:")
          (mapcar (lambda (component) (princ (format "  %.18f" component))) rgb)
          (terpri) (terpri)
          (princ "HSV:")
          (mapcar (lambda (component) (princ (format "  %.18f" component))) hsv)))))
          
  (defun icicle-make-color-candidate (color-name &optional hex-rgb)
    "Return multi-completion candidate of COLOR-NAME and its hex RGB string.
If `icicle-WYSIWYG-Completions-flag' is non-nil, then the hex RGB
string has the color as its background text property.
Optional arg HEX-RGB is the hex RGB string.  If HEX-RGB is nil, then
COLOR-NAME is used to determine the hex RGB string."
    (let* ((rgb-string  (or hex-rgb  (hexrgb-color-name-to-hex color-name)))
           (value       (hexrgb-value rgb-string)))
      (when icicle-WYSIWYG-Completions-flag
        (put-text-property 0 (length rgb-string) 'face
                           (list (cons 'foreground-color (if (< value 0.6) "White" "Black"))
                                 (cons 'background-color rgb-string))
                           rgb-string))
      (when (or (> icicle-help-in-mode-line-delay 0) ; Construct help only if user will see it.
                (and (boundp 'tooltip-mode)  tooltip-mode))
        (let* ((rgb   (hexrgb-hex-to-rgb rgb-string))
               (hsv   (apply #'hexrgb-rgb-to-hsv rgb))
               (help  (format "RGB: %.6f, %.6f, %.6f;  HSV: %.6f, %.6f, %.6f"
                              (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)
                              (nth 0 hsv) (nth 1 hsv) (nth 2 hsv))))
          (icicle-candidate-short-help help color-name)
          (icicle-candidate-short-help help rgb-string)))
      (list (list color-name rgb-string))))

  ;; This predicate is used for color completion.
  (defun icicle-color-red-lessp (s1 s2)
    "Non-nil means the RGB in S1 has less red than in S2.
The strings are assumed to have at least two parts, with the parts
separated by `icicle-list-join-string' The RGB values are assumed to
be the second parts of the strings, and they are assumed to start with
`#'."
    (let ((rgb1  (elt (split-string s1 icicle-list-join-string) 1))
          (rgb2  (elt (split-string s2 icicle-list-join-string) 1)))
      (and rgb1  rgb2  (< (hexrgb-red rgb1) (hexrgb-red rgb2))))) ; Just in case strings were not multipart.

  ;; This predicate is used for color completion.
  (defun icicle-color-green-lessp (s1 s2)
    "Non-nil means the RGB in S1 has less green than in S2.
The strings are assumed to have at least two parts, with the parts
separated by `icicle-list-join-string' The RGB values are assumed to
be the second parts of the strings, and they are assumed to start with
`#'."
    (let ((rgb1  (elt (split-string s1 icicle-list-join-string) 1))
          (rgb2  (elt (split-string s2 icicle-list-join-string) 1)))
      (and rgb1 rgb2  (< (hexrgb-green rgb1) (hexrgb-green rgb2))))) ; Just in case strings not multipart.

  ;; This predicate is used for color completion.
  (defun icicle-color-blue-lessp (s1 s2)
    "Non-nil means the RGB in S1 has less blue than in S2.
The strings are assumed to have at least two parts, with the parts
separated by `icicle-list-join-string' The RGB values are assumed to
be the second parts of the strings, and they are assumed to start with
`#'."
    (let ((rgb1  (elt (split-string s1 icicle-list-join-string) 1))
          (rgb2  (elt (split-string s2 icicle-list-join-string) 1)))
      (and rgb1 rgb2  (< (hexrgb-blue rgb1) (hexrgb-blue rgb2))))) ; Just in case strings were not multipart.

  ;; This predicate is used for color completion.
  (defun icicle-color-distance-rgb-lessp (s1 s2)
    "Return non-nil if color S1 is RGB-closer than S2 to the base color.
S1 and S2 are color names (strings).

The base color name is the cdr of option `list-colors-sort', whose car
must be `rgb-dist'.  If the option value is not already a cons with
car `rgb-dist' then it is made so: you are prompted for the base color
name to use."
    (let* ((base-color  (if (and (boundp 'list-colors-sort) ; Emacs 23+
                                 (consp list-colors-sort) (eq 'rgb-dist (car list-colors-sort)))
                            (cdr list-colors-sort) ; `list-colors-sort' is free here.
                          (cdr (setq list-colors-sort
                                     (cons 'rgb-dist
                                           (let ((enable-recursive-minibuffers  t)
                                                 (icicle-sort-comparer          nil))
                                             (icicle-read-color-wysiwyg ; Use the color name only.
                                              0 "With RGB close to color: ")))))))
           (base-rgb    (hexrgb-hex-to-rgb (hexrgb-color-name-to-hex base-color)))
           (base-red    (nth 0 base-rgb))
           (base-green  (nth 1 base-rgb))
           (base-blue   (nth 2 base-rgb))
           (s1-rgb      (hexrgb-hex-to-rgb (elt (split-string s1 icicle-list-join-string) 1)))
           (s2-rgb      (hexrgb-hex-to-rgb (elt (split-string s2 icicle-list-join-string) 1))))
      (< (+ (expt (- (nth 0 s1-rgb) base-red) 2)
            (expt (- (nth 1 s1-rgb) base-green) 2)
            (expt (- (nth 2 s1-rgb) base-blue) 2))
         (+ (expt (- (nth 0 s2-rgb) base-red) 2)
            (expt (- (nth 1 s2-rgb) base-green) 2)
            (expt (- (nth 2 s2-rgb) base-blue) 2)))))

  ;; This predicate is used for color completion.
  (defun icicle-color-hue-lessp (s1 s2)
    "Non-nil means the RGB hue in S1 is less than that in S2.
The strings are assumed to have at least two parts, with the parts
separated by `icicle-list-join-string' The RGB values are assumed to
be the second parts of the strings, and they are assumed to start with
`#'."
    (let ((rgb1  (elt (split-string s1 icicle-list-join-string) 1))
          (rgb2  (elt (split-string s2 icicle-list-join-string) 1)))
      (and rgb1  rgb2  (< (hexrgb-hue rgb1) (hexrgb-hue rgb2))))) ; Just in case strings were not multipart.

  ;; This predicate is used for color completion.
  (defun icicle-color-saturation-lessp (s1 s2)
    "Non-nil means the RGB in S1 is less saturated than in S2.
The strings are assumed to have at least two parts, with the parts
separated by `icicle-list-join-string' The RGB values are assumed to
be the second parts of the strings, and they are assumed to start with
`#'."
    (let ((rgb1  (elt (split-string s1 icicle-list-join-string) 1))
          (rgb2  (elt (split-string s2 icicle-list-join-string) 1)))
      (and rgb1  rgb2  (< (hexrgb-saturation rgb1) (hexrgb-saturation rgb2))))) ; For non-multipart strings.

  ;; This predicate is used for color completion.
  (defun icicle-color-value-lessp (s1 s2)
    "Non-nil means the RGB value in S1 is darker than that in S2.
The strings are assumed to have at least two parts, with the parts
separated by `icicle-list-join-string' The RGB values are assumed to
be the second parts of the strings, and they are assumed to start with
`#'."
    (let ((rgb1  (elt (split-string s1 icicle-list-join-string) 1))
          (rgb2  (elt (split-string s2 icicle-list-join-string) 1)))
      (and rgb1  rgb2  (< (hexrgb-value rgb1) (hexrgb-value rgb2))))) ; Just in case strings not multipart.

  ;; This predicate is used for color completion.
  (defun icicle-color-hsv-lessp (s1 s2)
    "Non-nil means the HSV components of S1 are less than those of S2.
Specifically, the hues are compared first, then if hues are equal then
saturations are compared, then if those are also equal values are
compared.
The strings are assumed to have at least two parts, with the parts
separated by `icicle-list-join-string' The second parts of the strings
are RGB triplets that start with `#'."
    (let* ((rgb1  (elt (split-string s1 icicle-list-join-string) 1))
           (hsv1  (and rgb1  (hexrgb-hex-to-hsv rgb1)))
           (rgb2  (elt (split-string s2 icicle-list-join-string) 1))
           (hsv2  (and rgb2  (hexrgb-hex-to-hsv rgb2))))
      (and hsv1  hsv2                   ; Just in case strings were not multipart.
           (or (< (nth 0 hsv1) (nth 0 hsv2))
               (and (= (nth 0 hsv1) (nth 0 hsv2))
                    (< (nth 1 hsv1) (nth 1 hsv2)))
               (and (= (nth 0 hsv1) (nth 0 hsv2))
                    (= (nth 1 hsv1) (nth 1 hsv2))
                    (< (nth 2 hsv1) (nth 2 hsv2)))))))

  ;; This predicate is used for color completion.
  (defun icicle-color-distance-hsv-lessp (s1 s2)
    "Return non-nil if color S1 is HSV-closer than S2 to the base color.
S1 and S2 are color names (strings).

The base color name is the cdr of option `list-colors-sort', whose car
must be `hsv-dist'.  If the option value is not already a cons with
car `hsv-dist' then it is made so: you are prompted for the base color
name to use."
    (let* ((base-color  (if (and (boundp 'list-colors-sort) ; Emacs 23+
                                 (consp list-colors-sort)
                                 (eq 'hsv-dist (car list-colors-sort)))
                            (cdr list-colors-sort) ; `list-colors-sort' is free here.
                          (cdr (setq list-colors-sort
                                     (cons 'hsv-dist
                                           (let ((enable-recursive-minibuffers  t)
                                                 (icicle-sort-comparer          nil))
                                             (icicle-read-color-wysiwyg ; Use the color name only.
                                              0 "With HSV close to color: ")))))))
           (base-hsv    (hexrgb-hex-to-hsv (hexrgb-color-name-to-hex base-color)))
           (base-hue    (nth 0 base-hsv))
           (base-sat    (nth 1 base-hsv))
           (base-val    (nth 2 base-hsv))
           (s1-hsv      (apply #'hexrgb-rgb-to-hsv
                               (hexrgb-hex-to-rgb
                                (elt (split-string s1 icicle-list-join-string) 1))))
           (s2-hsv      (apply #'hexrgb-rgb-to-hsv
                               (hexrgb-hex-to-rgb
                                (elt (split-string s2 icicle-list-join-string) 1)))))
      (< (+ (expt (- (nth 0 s1-hsv) base-hue) 2)
            (expt (- (nth 1 s1-hsv) base-sat) 2)
            (expt (- (nth 2 s1-hsv) base-val) 2))
         (+ (expt (- (nth 0 s2-hsv) base-hue) 2)
            (expt (- (nth 1 s2-hsv) base-sat) 2)
            (expt (- (nth 2 s2-hsv) base-val) 2)))))
  )


(defun icicle-cmd2-after-load-highlight ()
  "Things to do for `icicles-cmd2.el' after loading `highlight.el'."
  (when (fboundp 'next-single-char-property-change) ; Don't bother, for Emacs 20.

    (icicle-define-command icicle-choose-faces
      "Choose a list of face names (strings).
Option `hlt-act-on-any-face-flag' determines whether only highlighting
faces in the buffer are candidates.  The list of names (strings) is
returned."
      (lambda (name) (push name face-names)) ; Action function
      prompt                            ; `completing-read' args
      (mapcar #'icicle-make-face-candidate
              (if hlt-act-on-any-face-flag
                  (face-list)
                (hlt-highlight-faces-in-buffer (point-min) (point-max))))
      nil (not (stringp icicle-WYSIWYG-Completions-flag)) nil
      (if (boundp 'face-name-history) 'face-name-history 'icicle-face-name-history) nil nil
      ((icicle-list-nth-parts-join-string  ": ") ; Additional bindings
       (icicle-list-join-string            ": ")
       ;; $$$$$$ (icicle-list-end-string             "")
       (icicle-multi-completing-p          t)
       (icicle-list-use-nth-parts          '(1))
       (prompt                             (copy-sequence "Choose face (`RET' when done): "))
       (face-names                         ()))
      (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code.
      nil                               ; Undo code.
      (prog1 (setq face-names  (delete "" face-names)) ; Return the list of faces.
        (when (interactive-p) (message "Faces: %S" face-names))))

    (icicle-define-command icicle-choose-invisible-faces
      "Choose a list of face names (strings) from currently invisible faces.
Option `hlt-act-on-any-face-flag' determines whether only highlighting
faces in the buffer are candidates.  The list of names (strings) is
returned."
      (lambda (name) (push name face-names)) ; Action function
      prompt                            ; `completing-read' args
      (mapcar #'icicle-make-face-candidate
              (icicle-remove-if-not #'icicle-invisible-face-p
                                    (if hlt-act-on-any-face-flag
                                        (face-list)
                                      (hlt-highlight-faces-in-buffer (point-min) (point-max)))))
      nil (not (stringp icicle-WYSIWYG-Completions-flag)) nil
      (if (boundp 'face-name-history) 'face-name-history 'icicle-face-name-history) nil nil
      ((icicle-list-nth-parts-join-string  ": ") ; Additional bindings
       (icicle-list-join-string            ": ")
       ;; $$$$$$ (icicle-list-end-string             "")
       (icicle-multi-completing-p          t)
       (icicle-list-use-nth-parts          '(1))
       (prompt                             (copy-sequence "Choose face (`RET' when done): "))
       (face-names                         ()))
      (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code.
      nil                               ; Undo code.
      (prog1 (setq face-names  (delete "" face-names)) ; Return the list of faces.
        (when (interactive-p) (message "Faces: %S" face-names))))

    (icicle-define-command icicle-choose-visible-faces
      "Choose a list of face names (strings) from currently visible faces.
Option `hlt-act-on-any-face-flag' determines whether only highlighting
faces in the buffer are candidates.  The list of names (strings) is
returned."
      (lambda (name) (push name face-names)) ; Action function
      prompt                            ; `completing-read' args
      (mapcar #'icicle-make-face-candidate
              (icicle-remove-if #'icicle-invisible-face-p
                                (if hlt-act-on-any-face-flag
                                    (face-list)
                                  (hlt-highlight-faces-in-buffer (point-min) (point-max)))))
      nil (not (stringp icicle-WYSIWYG-Completions-flag)) nil
      (if (boundp 'face-name-history) 'face-name-history 'icicle-face-name-history) nil nil
      ((icicle-list-nth-parts-join-string  ": ") ; Additional bindings
       (icicle-list-join-string            ": ")
       ;; $$$$$$ (icicle-list-end-string   "")
       (icicle-multi-completing-p          t)
       (icicle-list-use-nth-parts          '(1))
       (prompt                             (copy-sequence "Choose face (`RET' when done): "))
       (face-names                         ()))
      (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code.
      nil                               ; Undo code.
      (prog1 (setq face-names  (delete "" face-names)) ; Return the list of faces.
        (when (interactive-p) (message "Faces: %S" face-names))))
  
    (defun icicle-show-only-faces (&optional start end faces)
      "Show only the faces you choose, hiding all others.
Non-nil `hlt-act-on-any-face-flag' means choose from among all
faces.  Nil means choose only from among faces used to highlight.

When choosing faces, completion and cycling are available. During
cycling, these keys with prefix `C-' act on the current face name\\<minibuffer-local-completion-map>:

`C-mouse-2', `C-RET' - Choose current face candidate only
`C-down'  - Choose, then move to next prefix-completion candidate
`C-up'    - Choose, then move to previous prefix-completion candidate
`C-next'  - Choose, then move to next apropos-completion candidate
`C-prior' - Choose, then move to previous apropos-completion candidate
`\\[icicle-all-candidates-action]'     - Choose *all* matching face names"
      (interactive `(,@(hlt-region-or-buffer-limits)
                     ,(mapcar #'intern (icicle-choose-faces)))) ; An Icicles multi-command
      (dolist (face (if hlt-act-on-any-face-flag
                        (face-list)
                      (hlt-highlight-faces-in-buffer start end)))
        (if (memq face faces)
            (hlt-show-default-face face)
          (hlt-hide-default-face start end face))))
  
    (defun icicle-hide-only-faces (&optional start end faces)
      "Hide only the faces you choose, showing all others.
Non-nil `hlt-act-on-any-face-flag' means choose from among all
faces.  Nil means choose only from among faces used to highlight.

When choosing faces, completion and cycling are available. During
cycling, these keys with prefix `C-' act on the current face name\\<minibuffer-local-completion-map>:

`C-mouse-2', `C-RET' - Choose current face candidate only
`C-down'  - Choose, then move to next prefix-completion candidate
`C-up'    - Choose, then move to previous prefix-completion candidate
`C-next'  - Choose, then move to next apropos-completion candidate
`C-prior' - Choose, then move to previous apropos-completion candidate
`\\[icicle-all-candidates-action]'     - Choose *all* matching face names"
      (interactive `(,@(hlt-region-or-buffer-limits)
                     ,(mapcar #'intern (icicle-choose-faces)))) ; An Icicles multi-command
      (dolist (face (if hlt-act-on-any-face-flag
                        (face-list)
                      (hlt-highlight-faces-in-buffer start end)))
        (if (memq face faces)
            (hlt-hide-default-face start end face)
          (hlt-show-default-face face))))

    (defun icicle-show-faces (faces)
      "Show invisible faces that you choose.  Do nothing to other faces.
Non-nil `hlt-act-on-any-face-flag' means choose from among all
invisible faces.  Nil means choose only from among invisible  faces
used to highlight.

When choosing faces, completion and cycling are available. During
cycling, these keys with prefix `C-' act on the current face name\\<minibuffer-local-completion-map>:

`C-mouse-2', `C-RET' - Choose current face candidate only
`C-down'  - Choose, then move to next prefix-completion candidate
`C-up'    - Choose, then move to previous prefix-completion candidate
`C-next'  - Choose, then move to next apropos-completion candidate
`C-prior' - Choose, then move to previous apropos-completion candidate
`\\[icicle-all-candidates-action]'     - Choose *all* matching face names"
      (interactive
       (list (let ((fs  (icicle-remove-if-not #'icicle-invisible-face-p
                                              (if hlt-act-on-any-face-flag
                                                  (face-list)
                                                (hlt-highlight-faces-in-buffer
                                                 (point-min) (point-max))))))
               (if fs
                   (mapcar #'intern (icicle-choose-invisible-faces)) ; An Icicles multi-command
                 (error "No%s faces are invisible"
                        (if hlt-act-on-any-face-flag "" " highlight"))))))
      (dolist (face faces) (hlt-show-default-face face)))
  
    (defun icicle-hide-faces (&optional start end faces)
      "Hide visible faces that you choose.  Do nothing to other faces.
Non-nil `hlt-act-on-any-face-flag' means choose from among all
visible faces.  Nil means choose only from among visible faces used to
highlight.

When choosing faces, completion and cycling are available. During
cycling, these keys with prefix `C-' act on the current face name\\<minibuffer-local-completion-map>:

`C-mouse-2', `C-RET' - Choose current face candidate only
`C-down'  - Choose, then move to next prefix-completion candidate
`C-up'    - Choose, then move to previous prefix-completion candidate
`C-next'  - Choose, then move to next apropos-completion candidate
`C-prior' - Choose, then move to previous apropos-completion candidate
`\\[icicle-all-candidates-action]'     - Choose *all* matching face names"
      (interactive `(,@(hlt-region-or-buffer-limits)
                     ,(mapcar #'intern (icicle-choose-faces)))) ; An Icicles multi-command
      (dolist (face faces) (hlt-hide-default-face start end face)))))

  
(defun icicle-cmd2-after-load-wid-edit+ ()
  "Things to do for `icicles-cmd2.el' after loading `wid-edit+.el'."

  ;; Save vanilla `color' widget as `icicle-ORIG-color' widget, for restoring when you quit Icicle mode.
  (unless (get 'icicle-ORIG-color 'widget-type)
    (put 'icicle-ORIG-color 'widget-type          (get 'color 'widget-type))
    (put 'icicle-ORIG-color 'widget-documentation (get 'color 'widget-documentation)))

;;;###autoload
  (define-widget 'icicle-color 'editable-field
    "Icicles version of the `color' widget.
`M-TAB' completes the color name using Icicles WYSIWYG completion.
See `icicle-widget-color-complete'."
    :format   "%{%t%}: %v (%{sample%})\n"
    :size     (1+ (apply #'max (mapcar #'length (x-defined-colors))))
    :tag      "Color"
    :match    'widgetp-color-match
    :validate 'widgetp-color-validate
    :value    "black"
    :complete 'icicle-widget-color-complete
    :sample-face-get 'widget-color-sample-face-get
    :notify   'widget-color-notify
    :action   'widget-color-action)

  ;; Emacs < 24 defines `widget-color-complete'.  Save that as `icicle-ORIG-*'.  Do nothing for Emacs 24+.
  (unless (or (> emacs-major-version 23)  (fboundp 'icicle-ORIG-widget-color-complete))
    (require 'wid-edit)
    (when (fboundp 'widget-color-complete)
      (defalias 'icicle-ORIG-widget-color-complete (symbol-function 'widget-color-complete))))

;;;###autoload (autoload 'icicle-lisp-complete-symbol "icicles")
  (defun icicle-widget-color-complete (widget)
    "Complete the color name in `color' widget WIDGET.
If you use Icicles, then you get Icicles completion (apropos,
progressive, complementing...).

If, in addition, option `icicle-WYSIWYG-Completions-flag' is non-nil:

 * Completion is WYSIWYG.  Each candidate is a color name followed by
   its RGB value as a color swatch.  You can complete against any of
   this text (name, RGB, or part or all of both).  Or you can enter an
   RGB value that has no color name without completing.

 * With a prefix arg, when you choose a completion its RGB value is
   used, not the color name.

If, in addition, option `icicle-add-proxy-candidates-flag' is non-nil
and library `palette.el' or `eyedropper.el' is available, then the
following Icicles proxy candidates are available during completion:

* `*copied foreground*'  - last copied foreground, if available
* `*copied background*'  - last copied background, if available
* `*mouse-2 foreground*' - foreground where you click `mouse-2'
* `*mouse-2 background*' - background where you click `mouse-2'
* `*point foreground*'   - foreground under the text cursor
* `*point background*'   - background under the text cursor

\(You can copy a color using eyedropper commands such as
`eyedrop-pick-foreground-at-mouse'.)

In addition, the names of user options (variables) whose custom type
is `color' are also proxy candidates, but with `'' as a prefix and
suffix.  So, for example, option `icicle-region-background' appears as
proxy color candidate `'icicle-region-background''.  If you choose
such a candidate then (only) the variable's value (color name or RGB)
is returned.

As always in Icicles, you can toggle the use of proxy candidates using
`\\<minibuffer-local-completion-map>\\[icicle-toggle-proxy-candidates]' in the minibuffer.

See `icicle-read-color-wysiwyg' for more information."
    (let* ((prefix      (buffer-substring-no-properties
                         (widget-field-start widget) (point)))
           ;; Free variables here: `eyedrop-picked-foreground', `eyedrop-picked-background'.
           ;; They are defined in library `palette.el' or library `eyedropper.el'.
           (colors      (if (fboundp 'hexrgb-defined-colors-alist) ; Defined in `hexrgb.el'.
                            (if (fboundp 'eyedrop-foreground-at-point)
                                (append (and eyedrop-picked-foreground  '(("*copied foreground*")))
                                        (and eyedrop-picked-background  '(("*copied background*")))
                                        '(("*mouse-2 foreground*") ("*mouse-2 background*")
                                          ("*point foreground*") ("*point background*"))
                                        (hexrgb-defined-colors-alist))
                              (hexrgb-defined-colors-alist))
                          (mapcar #'list (x-defined-colors))))
           (completion  (try-completion prefix colors)))
      (cond ((null completion)
             (widgetp-remove-Completions)
             (error "No completion for \"%s\"" prefix))
            ((eq completion t)
             (widgetp-remove-Completions)
             (message "Sole completion"))
            ((and (not (string-equal prefix completion))
                  (or (not (boundp 'icicle-mode))  (not icicle-mode)))
             (insert-and-inherit (substring completion (length prefix)))
             (message "Making completion list...")
             (widgetp-display-Completions prefix colors)
             (message "Completed, but not unique"))
            ((or (not (boundp 'icicle-mode))  (not icicle-mode))
             (message "Making completion list...")
             (widgetp-display-Completions prefix colors))
            (t
             (let* ((enable-recursive-minibuffers                (active-minibuffer-window))
                    (icicle-top-level-when-sole-completion-flag  t)
                    (icicle-show-Completions-initially-flag      t)
                    (icicle-unpropertize-completion-result-flag  t)
                    (completion-ignore-case                      t)
                    (field                                       (widget-field-find (point)))
                    (beg                                         (widget-field-start field))
                    (end                                         (max (point)
                                                                      (if (fboundp 'widget-field-text-end)
                                                                          (widget-field-text-end field)
                                                                        (widget-field-end field))))

                    (color
                     (if (and (fboundp 'icicle-read-color-wysiwyg)  icicle-WYSIWYG-Completions-flag)
                         (icicle-read-color-wysiwyg (if current-prefix-arg 99 0)
                                                    "Color (name or #R+G+B+): " prefix 'MSGP)
                       (completing-read "Color: " colors nil nil prefix))))
               (delete-region beg end)
               (insert-and-inherit color)
               (message "Completed"))))))
  )


(defun icicle-cmd2-after-load-palette ()
  "Things to do for `icicles-cmd2.el' after loading `palette.el'."

  (icicle-define-command icicle-pick-color-by-name ; Bound to `c' in color palette.
    "Set the current color to a color you name.
Instead of a color name, you can use an RGB string #XXXXXXXXXXXX,
where each X is a hex digit.  The number of Xs must be a multiple of
3, with the same number of Xs for each of red, green, and blue.
If you enter an empty color name, then a color is picked randomly.
The new current color is returned."     ; Doc string
    icicle-pick-color-by-name-action    ; Action function
    "Color (name or #R+G+B+): "         ; `completing-read' arguments
    (hexrgb-defined-colors-alist) nil nil nil nil nil nil
    ((completion-ignore-case  t)))      ; Bindings

  (defun icicle-pick-color-by-name-action (color)
    "Action function for `icicle-pick-color-by-name'."
    (if (string= "" color)
        (let* ((colors  (hexrgb-defined-colors))
               (rand    (random (length colors)))) ; Random color.
          (setq color  (elt colors rand)))
      (let ((hex-string  (hexrgb-rgb-hex-string-p color t)))
        (when (and hex-string  (not (eq 0 hex-string))) (setq color  (concat "#" color))) ; Add #.
        (if (not (or hex-string  (if (fboundp 'test-completion) ; Not defined in Emacs 20.
                                     (test-completion color (hexrgb-defined-colors-alist))
                                   (try-completion color (hexrgb-defined-colors-alist)))))
            (error "No such color: %S" color)
          (setq color  (hexrgb-color-name-to-hex color))))
      (setq palette-last-color  palette-current-color)
      (save-selected-window
        (setq color  (hexrgb-color-name-to-hex color)) ; Needed if not interactive.
        (palette-set-current-color color)
        (palette-where-is-color color)
        (palette-brightness-scale)
        (palette-swatch))
      palette-current-color))

  (define-key palette-mode-map (icicle-kbd "c")  'icicle-pick-color-by-name)
  (define-key palette-popup-map [pick-color-by-name] ; Use same name as in `palette.el'.
    `(menu-item "Choose Color By Name" icicle-pick-color-by-name
      :help "Set the current color to a color you name"))
  )


(defun icicle-cmd2-after-load-synonyms ()
  "Things to do for `icicles-cmd2.el' after loading `synonyms.el'."
  (defalias 'synonyms 'icicle-synonyms)
  (icicle-define-command icicle-synonyms ; Command
    "Show synonyms that match a regular expression (e.g. a word or phrase).
You are prompted for the regexp.  By default, it is the text
of the region, if it is active and `transient-mark-mode' is enabled,
or the nearest word to the cursor, if not.

Option `synonyms-match-more-flag' non-nil means additional thesaurus
  entries can be matched.  This can be more time-consuming.  It means
  two things:

  1) Input can match parts of synonyms, in addition to whole synonyms.
  2) All synonyms are shown, even if input matches a thesaurus entry.

Option `synonyms-append-result-flag' non-nil means to append search
  result to previous results.

A prefix argument toggles the meaning of each of those options for the
duration of the command:

  If `C-u' or `C-u C-u', then toggle `synonyms-match-more-flag'.
  If negative or `C-u C-u', then toggle `synonyms-append-result-flag'.

\(`C-u C-u' thus means toggle both options.)

When called from Lisp, optional second argument REGEXP is the regexp
to match (no prompting)."               ; Doc string
    synonyms-action                     ; Action function,  defined in `synonyms.el'.
    "Show synonyms for word or phrase (regexp): " ; `completing-read' arguments
    synonyms-obarray nil nil nil 'synonyms-history (synonyms-default-regexp) nil
    ((num-arg (prefix-numeric-value current-prefix-arg)) ; Bindings
     (morep (eq synonyms-match-more-flag (atom current-prefix-arg)))
     (appendp (eq synonyms-append-result-flag (and (wholenump num-arg)  (/= 16 num-arg))))
     (icicle-sort-function 'icicle-case-insensitive-string-less-p))
    (synonyms-ensure-synonyms-read-from-cache)) ; Fill `synonyms-obarray' initially, for completion.

  (icicle-define-command icicle-insert-thesaurus-entry ; Command name
    "Insert an entry from a thesaurus.
Library `synonyms.el' is needed for this.  If you have never used
command `synonyms' before, then the first use of
`icicle-insert-thesaurus-entry' will take a while, because it will
build a cache file of synonyms that are used for completion.  See
`synonyms.el'.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-cycle-incremental-completion] to toggle incremental completion." ; Doc string
    icicle-insert-thesaurus-entry-cand-fn ; Action function
    "Thesaurus entry to match: " synonyms-obarray ; `completing-read' args
    nil t nil 'icicle-dictionary-history nil nil
    ((icicle-track-pt  (point)))        ; Bindings
    (progn                              ; First code
      (unless (or (boundp 'synonyms-obarray)  (require 'synonyms nil t))
        (error "You must first load library `synonyms.el'"))
      (synonyms-ensure-synonyms-read-from-cache))
    nil                                 ; Undo code
    (when (window-live-p icicle-orig-window) ; Last code
      (select-window icicle-orig-window)
      (select-frame-set-input-focus (selected-frame))
      (goto-char icicle-track-pt)))

  ;; Free vars here: `icicle-orig-buff' is bound in `icicle-insert-thesaurus-entry'.
  (defun icicle-insert-thesaurus-entry-cand-fn (string)
    "Action function for `icicle-insert-thesaurus-entry'.
Insert STRING, followed by a space, at position TRACK-PT of buffer
ORIG-BUFF."
    (set-buffer icicle-orig-buff)
    (goto-char icicle-track-pt)
    (insert string " ")
    (setq icicle-track-pt  (point))
    (unless (pos-visible-in-window-p) (recenter icicle-recenter))
    (with-current-buffer (window-buffer (minibuffer-window)) (icicle-clear-minibuffer))
    (save-selected-window (icicle-remove-Completions-window)))

  (defun icicle-complete-thesaurus-entry (word) ; Bound to `C-c /' in Icicle mode.
    "Complete WORD to an entry from a thesaurus.
The default value of WORD is the word at the cursor.
Library `synonyms.el' is needed for this.  If you have never used
command `synonyms' before, then the first use of
`icicle-insert-thesaurus-entry' will take a while, because it will
build a cache file of synonyms that are used for completion.  See
`synonyms.el'."
    (interactive (list (word-at-point)))
    (unless word (error "No word at point to complete"))
    (unless (or (boundp 'synonyms-obarray)  (require 'synonyms nil t))
      (error "You must first load library `synonyms.el'"))
    (synonyms-ensure-synonyms-read-from-cache)
    (when (and (looking-at "\\b")  (not (looking-at "\\s-"))) (forward-word 1))
    (delete-region (progn (forward-word -1) (point)) (progn (forward-word 1) (point)))
    (insert (completing-read "Thesaurus entry to match: " synonyms-obarray
                             nil nil word 'icicle-dictionary-history word))
    (unless (looking-at "\\s-") (insert " ")))
  )



;;; Library `Bookmark+' - Icicles multi-commands.
;;;
;;;###autoload (autoload 'icicle-tag-a-file                              "icicles")
;;;###autoload (autoload 'icicle-untag-a-file                            "icicles")
;;;###autoload (autoload 'icicle-find-file-handle-bookmark               "icicles")
;;;###autoload (autoload 'icicle-find-file-handle-bookmark-other-window  "icicles")
;;;###autoload (autoload 'icicle-find-file-tagged                        "icicles")
;;;###autoload (autoload 'icicle-find-file-tagged-other-window           "icicles")
;;;###autoload (autoload 'icicle-find-file-all-tags                      "icicles")
;;;###autoload (autoload 'icicle-find-file-all-tags-other-window         "icicles")
;;;###autoload (autoload 'icicle-find-file-all-tags-regexp               "icicles")
;;;###autoload (autoload 'icicle-find-file-all-tag-regexp-other-windows  "icicles")
;;;###autoload (autoload 'icicle-find-file-some-tags                     "icicles")
;;;###autoload (autoload 'icicle-find-file-some-tags-other-window        "icicles")
;;;###autoload (autoload 'icicle-find-file-some-tags-regexp              "icicles")
;;;###autoload (autoload 'icicle-find-file-some-tags-regexp-other-window "icicles")
(eval-after-load "bookmark+" '(icicle-cmd2-after-load-bookmark+))


;;; Library `hexrgb.el' - Icicles multi-commands.
;;;
;;;###autoload (autoload 'icicle-frame-bg "icicles")
;;;###autoload (autoload 'icicle-frame-fg "icicles")
(eval-after-load "hexrgb" '(icicle-cmd2-after-load-hexrgb))


;;; Library `highlight.el' - Icicles multi-commands.  Emacs 21+.
;;;
;;;###autoload (autoload 'icicle-choose-faces           "icicles")
;;;###autoload (autoload 'icicle-choose-invisible-faces "icicles")
;;;###autoload (autoload 'icicle-choose-visible-faces   "icicles")
;;;###autoload (autoload 'icicle-hide-faces             "icicles")
;;;###autoload (autoload 'icicle-hide-only-faces        "icicles")
;;;###autoload (autoload 'icicle-show-faces             "icicles")
;;;###autoload (autoload 'icicle-show-only-faces        "icicles")
(eval-after-load "highlight" '(icicle-cmd2-after-load-highlight))


;;; Library `palette.el' - Icicles multi-commands.
;;;
;;;###autoload (autoload 'icicle-pick-color-by-name "icicles")
(eval-after-load "palette" '(icicle-cmd2-after-load-palette))


;;; Library `synonyms.el' - Icicles multi-commands.
;;;
;;;###autoload (autoload 'synonyms                        "icicles")
;;;###autoload (autoload 'icicle-synonyms                 "icicles")
;;;###autoload (autoload 'icicle-insert-thesaurus-entry   "icicles")
;;;###autoload (autoload 'icicle-complete-thesaurus-entry "icicles")
(eval-after-load "synonyms" '(icicle-cmd2-after-load-synonyms))


;;; Library `wid-edit+.el' - Icicles function and widget.
;;;
;;;###autoload (autoload 'icicle-widget-color-complete  "icicles")
(eval-after-load "wid-edit+" '(icicle-cmd2-after-load-wid-edit+))
 
;;(@* "Icicles Top-Level Commands, Part 2")

;;; Icicles Top-Level Commands, Part 2 -------------------------------


(defvar icicle-orig-font nil
  "Font of selected frame, before command.")

(defvar icicle-orig-frame nil
  "Selected frame, before command.")

(defvar icicle-orig-menu-bar nil
  "`menu-bar-lines' of selected frame, before command.")

(defvar icicle-orig-pixelsize nil
  "Size of font of selected frame in pixels, before command.")

(defvar icicle-orig-pointsize nil
  "Size of font of selected frame in points, before command.")

;;;###autoload (autoload 'icicle-font "icicles")
(icicle-define-command icicle-font      ; Command name
  "Change font of current frame."       ; Doc string
  (lambda (font) (modify-frame-parameters icicle-orig-frame (list (cons 'font font)))) ; Action fn
  "Font: "                              ; `completing-read' args
  (let ((fonts  ())
        fws)
    (dolist (ft  (x-list-fonts "*")  fonts) ; Just avoiding two traversals, one to remove nil elts.
      (when (setq fws  (icicle-font-w-orig-size ft)) (push fws fonts)))) ; Ignore nil entries.
  nil t nil (if (boundp 'font-name-history) 'font-name-history 'icicle-font-name-history) nil nil
  ((icicle-orig-frame      (selected-frame)) ; Bindings
   (icicle-orig-font       (frame-parameter nil 'font))
   (icicle-orig-pixelsize  (aref (x-decompose-font-name icicle-orig-font)
                                 xlfd-regexp-pixelsize-subnum))
   (icicle-orig-pointsize  (aref (x-decompose-font-name icicle-orig-font)
                                 xlfd-regexp-pointsize-subnum))
   (icicle-orig-menu-bar   (assq 'menu-bar-lines (frame-parameters icicle-orig-frame))))
  ;; First code - remove menu-bar, to avoid Emacs bug that resizes frame.
  (modify-frame-parameters icicle-orig-frame (list '(menu-bar-lines . 0)))
  (modify-frame-parameters icicle-orig-frame ; Undo code.
                           (list (cons 'font icicle-orig-font) icicle-orig-menu-bar))
  (modify-frame-parameters icicle-orig-frame (list icicle-orig-menu-bar))) ; Last code.

;; Free var here: `icicle-orig-pixelsize' is bound in `icicle-font'.
(defun icicle-font-w-orig-size (font)
  "Return a font like FONT, but with pixel size `icicle-orig-pixelsize'.
Return nil if `x-decompose-font-name' returns nil for FONT.
`icicle-orig-pixelsize' is the original pixel size for `icicle-font'."
  (let ((xlfd-fields  (x-decompose-font-name font)))
    (if (not xlfd-fields)               ; Can't handle such font names - return nil.
        nil
      (aset xlfd-fields xlfd-regexp-pixelsize-subnum icicle-orig-pixelsize)
      (aset xlfd-fields xlfd-regexp-pointsize-subnum icicle-orig-pointsize)
      (let* ((sized-font   (x-compose-font-name xlfd-fields))
             (font-info    (and (or (> icicle-help-in-mode-line-delay 0) ; Only if user will see it.
                                    (and (boundp 'tooltip-mode)  tooltip-mode))
                                (font-info sized-font)))
             (iii          (if (< emacs-major-version 21) 3 2))
             (help-string  (if font-info
                               (format "width: %s, height: %s, offset: %s, compose: %s"
                                       (aref font-info iii) (aref font-info (+ iii 1))
                                       (aref font-info (+ iii 2)) (aref font-info (+ iii 3)))
                             "Font is not yet loaded (used)")))
        (icicle-candidate-short-help help-string sized-font)
        (list sized-font)))))

(defvar icicle-named-colors ()
  "Named colors.")


(defvar icicle-info-buff nil
  "Info buffer before command was invoked.")

(defvar icicle-info-window nil
  "Info window before command was invoked.")

;;;###autoload (autoload 'icicle-Info-index "icicles")
(defun icicle-Info-index (&optional topic)
  "Like vanilla `Info-index', but you can use multi-command keys `C-RET', `C-up' etc.
Also, for Emacs 22 and later, completion candidates (index topics) for
nodes already visited are highlighted with face
`icicle-historical-candidate-other' if there are fewer matching
candidates than `icicle-Info-visited-max-candidates'"
  ;; We allow an arg only for non-interactive use.  E.g., `Info-virtual-index' calls (Info-index TOPIC).
  (interactive)
  (unless (and (featurep 'info)  (eq major-mode 'Info-mode))
    (error "You must be in Info mode to use this command"))
  (when (and (boundp 'Info-current-file)  (equal Info-current-file "dir"))
    (error "The Info directory node has no index; use `m' to select a manual"))
  (let ((icicle-info-buff            (current-buffer))
        (icicle-info-window          (selected-window))
        (icicle-candidate-action-fn  'icicle-Info-index-action)
        (C-x-m                       (lookup-key minibuffer-local-completion-map "\C-xm"))
        ;; These next 3 are used as FREE vars
        ;; in `icicle-Info-node-is-indexed-by-topic' and `icicle-display-candidates-in-Completions'
        (icicle-Info-index-nodes     (and (fboundp 'Info-index-nodes)  (Info-index-nodes))) ; Emacs 22+
        (icicle-Info-manual          Info-current-file)
        (icicle-Info-hist-list       (and (boundp 'Info-history-list)  Info-history-list)) ; Emacs 22+
        (icicle-transform-function   'icicle-remove-duplicates)) ; See Emacs bug #12705.
    (when (and (require 'bookmark+ nil t)  (fboundp 'icicle-bookmark-info-other-window))
      (define-key minibuffer-local-completion-map (icicle-kbd "C-x m")
        'icicle-bookmark-info-other-window))
    (unwind-protect
         (if topic
             (icicle-ORIG-Info-index topic)
           (call-interactively (if (> emacs-major-version 21) 'icicle-ORIG-Info-index 'icicle-Info-index-20)))
      (define-key minibuffer-local-completion-map (icicle-kbd "C-x m") C-x-m))))

;; Thx to Tamas Patrovics for this Emacs 20 version.
;;
;;;###autoload (autoload 'icicle-Info-index-20 "icicles")
(defun icicle-Info-index-20 ()
  "Like `Info-index', but you can use completion for the index topic."
  (interactive)
  (let* ((symb (or (and (fboundp 'symbol-nearest-point) ; `icicles-opt.el' soft-requires `thingatpt+.el'.
                        (symbol-nearest-point))
                   (symbol-at-point)))
         (topic (and symb  (symbol-name symb))))
    (icicle-ORIG-Info-index "")
    (let ((pattern     "\\* +\\([^:]*\\):.")
          (candidates  ()))
      (goto-char (point-min))
      (while (re-search-forward pattern nil t) (push (list (match-string 1)) candidates))
      (icicle-ORIG-Info-index (completing-read "Index topic: " candidates nil t nil nil topic)))))

;; Free vars here: `icicle-info-buff' and `icicle-info-window' are bound in `icicle-Info-index'.
(defun icicle-Info-index-action (topic)
  "Completion action function for `icicle-Info-index'."
  (let ((minibuf-win  (selected-window)))
    (set-buffer icicle-info-buff)
    (select-window icicle-info-window)
    (icicle-ORIG-Info-index topic)
    (select-window minibuf-win)))

(defun icicle-Info-menu (&optional menu-item fork)
  "Go to a menu node.
See `icicle-ORIG-Info-menu'."
  (interactive)
  (if menu-item
      (if (< emacs-major-version 21)
          (icicle-ORIG-Info-menu menu-item)
        (icicle-ORIG-Info-menu menu-item fork))
    (call-interactively #'icicle-Info-menu-cmd)))

;; Free vars here: `Info-menu-entry-name-re' is bound in `info.el'.
(icicle-define-command icicle-Info-menu-cmd
  "Go to a menu node."                  ; Doc string
  (lambda (m) (icicle-Info-goto-node (cdr (funcall icicle-get-alist-candidate-function m)))) ; Action
  "Menu item: " icicle-candidates-alist ; `completing-read' args
  nil t nil nil (save-excursion
                  (goto-char (point-min))
                  (unless (search-forward "\n* menu:" nil t) (error "No menu in this node"))
                  (setq menu-eol  (point))
                  (and (< menu-eol opoint)
                       (save-excursion
                         (goto-char opoint) (end-of-line)
                         (and (re-search-backward (concat "\n\\* +\\("
                                                          (if (boundp 'Info-menu-entry-name-re)
                                                              Info-menu-entry-name-re
                                                            "[^:\t\n]*")
                                                          "\\):")
                                                  menu-eol t)
                              (match-string-no-properties 1)))))
  nil
  ((opoint                                 (point)) ; Bindings
   (completion-ignore-case                 t)
   (case-fold-search                       t)
   (icicle-sort-comparer                   nil)
   (icicle-whole-candidate-as-text-prop-p  t)
   (Info-complete-menu-buffer              (current-buffer))
   (icicle-candidates-alist                (mapcar (lambda (m) (cons m (Info-extract-menu-item m)))
                                                   (reverse (all-completions "" 'Info-complete-menu-item))))
   menu-eol))

;;;###autoload (autoload 'icicle-Info-goto-node "icicles")
(defun icicle-Info-goto-node (nodename &optional arg)
  "Go to Info node named NODENAME.
NODENAME has the form NODE or (FILE)NODE-IN-FILE, where:
 NODE names a node in the current Info file or one of its subfiles.
 FILE names an Info file containing node NODE-IN-FILE.
Completion is available for node names in the current Info file.

With a prefix argument:

 * Plain `C-u' means prepend the current Info file name (manual name)
   to each node name.  For example: `(emacs)Paragraphs' instead of
   just `Paragraphs'.

 * A negative numeric prefix arg (e.g. `C--') means present completion
   candidates in book order, and limit the candidates to the current
   node and the rest of the book following it.  In this case, the
   first candidate is `..', which means go up.

 * A non-negative numeric prefix arg (e.g. `C-1') means show the
   target node in a new Info buffer (not available prior to Emacs 21).
   (This applies only to the final completion choice, not to
   intermediate candidate actions using, e.g., `C-RET'.)

In Lisp code, if optional argument ARG is a string, then show the node
in a new Info buffer named `*info-ARG*'.

With no prefix argument, or with a non-negative prefix arg, you can
use `C-,' to choose how to sort completion candidates.  By default,
they are sorted alphabetically.

Input-candidate completion and cycling are available.  While cycling,
these keys with prefix `C-' are active:

`C-mouse-2', `C-RET' - Go to current completion candidate (node)
`C-down'  - Go to next completion candidate
`C-up'    - Go to previous completion candidate
`C-next'  - Go to next apropos-completion candidate
`C-prior' - Go to previous apropos-completion candidate
`C-end'   - Go to next prefix-completion candidate
`C-home'  - Go to previous prefix-completion candidate

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate, or
`C-g' to quit.

This is an Icicles command - see command `icicle-mode'."
  (interactive
   (let* ((icicle-info-buff                 (current-buffer))
          (icicle-info-window               (selected-window))
          (icicle-candidate-action-fn       'icicle-Info-goto-node-action)
          (icicle-Info-only-rest-of-book-p  (< (prefix-numeric-value current-prefix-arg) 0))
          (icicle-sort-orders-alist         (cons '("in book order" .  icicle-Info-book-order-p)
                                                  icicle-sort-orders-alist))
          (icicle-sort-comparer             (if icicle-Info-only-rest-of-book-p
                                                #'icicle-Info-book-order-p
                                              icicle-sort-comparer)))
     (list (icicle-Info-read-node-name "Go to node: " (consp current-prefix-arg))
           current-prefix-arg)))
  (icicle-Info-goto-node-1 nodename arg))

(defun icicle-Info-goto-node-1 (nodename &optional arg)
  "Same as vanilla `Info-goto-node', but go up for `..' pseudo-node."
  (if (and (string= nodename "..")  (Info-check-pointer "up"))
      (Info-up)
    (if (> emacs-major-version 20)
        (icicle-ORIG-Info-goto-node nodename (and (not icicle-Info-only-rest-of-book-p)  arg))
      (icicle-ORIG-Info-goto-node nodename))))

(defun icicle-Info-read-node-name (prompt &optional include-file-p)
  "Read a node name, prompting with PROMPT.
Non-nil INCLUDE-FILE-P means include current Info file in the name.
You can use `C-x m' during completion to access Info bookmarks, if you
 use library `Bookmark+'."
  (let ((C-x-m  (lookup-key minibuffer-local-completion-map "\C-xm")))
    (when (and (require 'bookmark+ nil t)  (fboundp 'icicle-bookmark-info-other-window))
      (define-key minibuffer-local-completion-map (icicle-kbd "C-x m")
        'icicle-bookmark-info-other-window))
    (unwind-protect
         (let* ((completion-ignore-case           t)
                (Info-read-node-completion-table  (icicle-Info-build-node-completions include-file-p))
                (nodename                         (completing-read prompt 'Info-read-node-name-1 nil nil)))
           (if (equal nodename "") (icicle-Info-read-node-name prompt) nodename))
      (define-key minibuffer-local-completion-map (icicle-kbd "C-x m") C-x-m))))

(defun icicle-Info-build-node-completions (&optional include-file-p)
  "Build completions list for Info nodes.
This takes `icicle-Info-only-rest-of-book-p' into account.
Non-nil INCLUDE-FILE-P means include current Info file in the name."
  (icicle-highlight-lighter)
  (if (or (not icicle-Info-only-rest-of-book-p)  (string= Info-current-node "Top"))
      (icicle-Info-build-node-completions-1 include-file-p)
    (reverse (cons '("..")
                   (member (list Info-current-node)
                           (reverse (icicle-Info-build-node-completions-1 include-file-p)))))))

(defun icicle-Info-build-node-completions-1 (&optional include-file-p)
  "Helper function for `icicle-Info-build-node-completions'.
Use `Info-build-node-completions' to build node list for completion.
Non-nil INCLUDE-FILE-P means include current Info file in the name.
Remove pseudo-node `*'.  (This just fixes a bug in Emacs 21 and 22.1.)"
  (let ((comps  (Info-build-node-completions)))
    (when (equal (car comps) '("*")) (setq comps  (cdr comps)))
    (if include-file-p
        (let ((file  (concat "(" (cond ((stringp Info-current-file)
                                        (replace-regexp-in-string
                                         "%" "%%" (file-name-nondirectory Info-current-file)))
                                       (Info-current-file (format "*%S*" Info-current-file))
                                       (t ""))
                             ")")))
          (mapcar (lambda (node) (cons (concat file (car node)) (cdr node))) comps))
      comps)))

;; Free vars here:
;; `icicle-info-buff' and `icicle-info-window' are bound in `icicle-Info-goto-node'.
;; `Info-read-node-completion-table' is bound in `info.el'.
(defun icicle-Info-goto-node-action (node)
  "Completion action function for `icicle-Info-goto-node'."
  (set-buffer icicle-info-buff)
  (select-window icicle-info-window)
  (icicle-Info-goto-node-1 node)
  (when icicle-Info-only-rest-of-book-p
    (setq Info-read-node-completion-table  (icicle-Info-build-node-completions)
          icicle-current-input             "")
    (icicle-complete-again-update)
    (if (and (string= Info-current-node "Top")  Info-history)
        (let* ((hist  Info-history)
               (last  (car (cdr (car hist)))))
          (while (string= "Top" (car (cdr (car hist)))) (pop hist))
          (setq icicle-candidate-nb
                (1- (length (reverse (member (list (car (cdr (car hist))))
                                             (icicle-Info-build-node-completions-1)))))))
      (setq icicle-candidate-nb  1))     ; Skip `..'.

    ;; $$$$$$ Maybe factor this out. Same thing in several places.  However, here we don't do
    ;; `icicle-maybe-sort-and-strip-candidates' at beginning of first clause.
    (cond ((and icicle-completion-candidates  (cdr icicle-completion-candidates)) ; > 1 left.
           (message "Displaying completion candidates...")
           (save-selected-window (icicle-display-candidates-in-Completions))
           (with-current-buffer "*Completions*"
             (goto-char (icicle-start-of-candidates-in-Completions))
             (icicle-move-to-next-completion
              (mod icicle-candidate-nb (length icicle-completion-candidates)))
             (set-window-point (get-buffer-window "*Completions*" 0) (point))
             (setq icicle-last-completion-candidate  (icicle-current-completion-in-Completions))
             (set-buffer-modified-p nil)))
          (icicle-completion-candidates ; Single candidate left
           (save-selected-window (icicle-remove-Completions-window))
           (let ((completion  (icicle-transform-multi-completion
                               (car icicle-completion-candidates))))
             (select-window (active-minibuffer-window))
             (with-current-buffer (window-buffer) ; Need if `*Completions*' redirected to minibuffer.
               (goto-char (icicle-minibuffer-prompt-end))
               (icicle-clear-minibuffer)
               (insert (if (and (icicle-file-name-input-p)
                                insert-default-directory
                                (or (not (member completion icicle-extra-candidates))
                                    icicle-extra-candidates-dir-insert-p))
                           (icicle-file-name-directory-w-default icicle-current-input)
                         "")
                       completion))))
          (t                            ; No candidates left
           (select-window (active-minibuffer-window))
           (with-current-buffer (window-buffer) ; Needed if `*Completions*' redirected to minibuffer.
             (goto-char (icicle-minibuffer-prompt-end))
             (icicle-clear-minibuffer)))))
  (select-window (active-minibuffer-window)))

(defun icicle-Info-book-order-p (s1 s2)
  "Non-nil if Info node S1 comes before node S2 in the book."
  t)        ; This just reverses the default order, which is reversed.

(when (> emacs-major-version 21)
  (defun icicle-Info-virtual-book (nodeset)
    "Open Info on a virtual book of saved Info nodes.
You need library `info+.el' to use this command.
With a prefix arg, you are prompted to choose a persistent saved
 completion set from `icicle-saved-completion-sets'.  The set you
 choose should be a set of saved Info node names.
With no prefix arg, use `icicle-saved-completion-candidates', which
 should be a set of Info node names.  If that is empty, then use
 `Info-saved-nodes'.
Non-interactively, argument NODESET is a list of Info node names."
    (interactive
     (progn (unless (and (require 'info+ nil t)  (fboundp 'Info-virtual-book))
              (error "You need library `info+.el' for this command"))
            (list (if (not current-prefix-arg)
                      "Virtual Book"
                    (save-selected-window
                      (completing-read "Saved Info node set: " icicle-saved-completion-sets nil t nil
                                       'icicle-completion-set-history))))))
    (let ((nodes  (and (consp nodeset)  nodeset))) ; (), if interactive - NODESET is a string then.
      (when (interactive-p)
        (if (not current-prefix-arg)
            (setq nodes  icicle-saved-completion-candidates)
          (let ((file-name  (cdr (assoc nodeset icicle-saved-completion-sets))))
            (unless (icicle-file-readable-p file-name)
              (error "Cannot read cache file `%s'" file-name))
            (let ((list-buf  (find-file-noselect file-name 'nowarn 'raw)))
              (unwind-protect
                   (condition-case icicle-Info-virtual-book
                       (when (listp (setq nodes  (read list-buf)))
                         (message "Set `%s' read from file `%s'" nodeset file-name))
                     (error (error "Bad cache file.  %s"
                                   (error-message-string icicle-Info-virtual-book))))
                (kill-buffer list-buf))
              (unless (consp nodes) (error "Bad data in cache file `%s'" file-name))))))
      (unless nodes (setq nodes  Info-saved-nodes)) ; In `info+.el'.
      (unless (and nodes  (stringp (car nodes))) (error "No saved Info nodes")) ; Minimal check.
      (unless (stringp nodeset) (setq nodeset "Virtual Book")) ; Non-interactive - NODESET is a list.
      (Info-virtual-book nodeset nodes))))

;;;###autoload (autoload 'icicle-where-is "icicles")
(icicle-define-command icicle-where-is  ; Command name
  "Show keyboard/menu/mouse sequences that invoke specified command.
This is a multi-command version of `where-is'.

With no prefix argument, only commands actually bound to keys are
completion candidates.  With a prefix argument, all commands are
candidates.  NOTE: This is a significant difference from vanilla
`where-is', which shows all commands as candidates, even those that
are not bound.

With a plain (non-numeric) prefix argument, `C-u', insert the message
in the current buffer.  (This is the same for vanilla `where-is'.)

By default, Icicle mode remaps all key sequences that are normally
bound to `where-is' to `icicle-where-is'.  If you do not want this
remapping, then customize option `icicle-top-level-key-bindings'." ; Doc string
  (lambda (x) (let ((symb  (intern-soft x))) ; Action function
                (where-is symb (and pref-arg  (consp pref-arg)))))
  (if pref-arg "Where is command: " "Where is bound command: ")
  obarray (and icompletep  pred) t nil nil ; `completing-read' args
  (let ((fn  (or (and (fboundp 'tap-symbol-nearest-point) ; Defined in `thingatpt+.el'.
                      (tap-symbol-nearest-point))
                 (function-called-at-point))))
    (and fn  (symbol-name fn)))
  t
  ((pref-arg  current-prefix-arg)       ; Bindings
   (pred                                    (if pref-arg
                                                (lambda (cand)
                                                  (unless (symbolp cand) (setq cand  (intern cand)))
                                                  (commandp cand))
                                              (lambda (cand)
                                                (unless (symbolp cand) (setq cand  (intern cand)))
                                                (with-current-buffer icicle-orig-buff
                                                  (and (commandp cand)
                                                       (where-is-internal cand overriding-local-map
                                                                          'non-ascii))))))
   (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
   (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
   (icicle-candidate-help-fn
    (lambda (cand)
      (with-current-buffer icicle-orig-buff
        (let* ((keys   (where-is-internal (intern-soft cand) overriding-local-map))
               (keys1  (mapconcat #'icicle-key-description keys "', `")))
          (message (if (string= "" keys1)
                       (format "`%s' is not on any key" cand)
                     (format "`%s' is on `%s'" cand (icicle-propertize keys1 'face 'icicle-msg-emphasis))))
          (sit-for 3)))))
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "command")))
   (icicle-all-candidates-list-alt-action-fn
    (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "command")))))

;;;###autoload (autoload 'icicle-vardoc "icicles")
(icicle-define-command icicle-vardoc    ; Command name
  "Choose a variable description.
Each candidate for completion is a variable name plus its
documentation.  They are separated by `icicle-list-join-string'
\(\"^G^J\", by default).  You can match an input regexp against the
variable name or the documentation or both.  Use `C-M-j' (equivalent
here to `C-q C-g C-j') to input the default separator.

For example, use input

\"dired.*^G
\[^^G]*list\"

with `S-TAB' to match all variables whose names contain \"dired\" and
whose documentation contains \"list\".  Here, `[^^G]' matches any
character except ^G, which includes newline.  If you use `.*' here,
instead, then only the first lines of doc strings are searched.

With a prefix argument, use the same documentation that was gathered
the last time `icicle-vardoc' was called.  Use a prefix arg to save
the time that would be needed to gather the documentation.

You can use `C-$' during completion to toggle limiting the domain of
initial candidates to functions that are commands (interactive).

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-cycle-incremental-completion] to toggle incremental completion.

See also: `icicle-apropos-value'."      ; Doc string
  icicle-doc-action                     ; Action function
  prompt                                ; `completing-read' args
  (let ((result  (and pref-arg  icicle-vardoc-last-initial-cand-set)))
    (unless result                      ; COLLECTION arg is an alist whose items are ((SYMB DOC)).
      (mapatoms (lambda (symb)          ; Each completion candidate is a list of strings.
                  (when (and (boundp symb)
                             (or (wholenump (prefix-numeric-value pref-arg))
                                 (user-variable-p symb)))
                    (let ((doc  (documentation-property symb 'variable-documentation)))
                      (when (icicle-non-whitespace-string-p doc)
                        (push (list (list (symbol-name symb) doc)) result))))))
      (setq icicle-vardoc-last-initial-cand-set  result))
    result)
  nil nil nil 'icicle-doc-history nil nil
  ((prompt                             "VAR `C-M-j' DOC: ") ; Bindings
   (icicle-toggle-transforming-message "Filtering to user options is now %s")
   (icicle-transform-function           nil) ; No transformation: all symbols.
   (icicle-last-transform-function      (lambda (cands) ; `C-$': only options.
                                          (loop
                                           for cc in cands
                                           with symb
                                           do (setq symb  (intern (icicle-transform-multi-completion cc)))
                                           if (user-variable-p symb)
                                           collect cc)))
   (icicle-candidate-properties-alist  '((1 (face icicle-candidate-part))))
   (icicle-multi-completing-p          t)
   (icicle-list-use-nth-parts          '(1))
   (pref-arg                           current-prefix-arg))
  (progn
    (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
    (icicle-highlight-lighter)
    (message "Gathering variable descriptions...")))

;;; $$$$$$ (defun icicle-funvardoc-action (entry)
;;;   "Action function for `icicle-vardoc', `icicle-fundoc', `icicle-plist'."
;;;   (with-output-to-temp-buffer "*Help*" (princ entry)))

;;;###autoload (autoload 'icicle-fundoc "icicles")
(icicle-define-command icicle-fundoc    ; Command name
  "Choose a function description.
Each candidate for completion is a function name plus its
documentation.  They are separated by `icicle-list-join-string'
\(\"^G^J\", by default).  You can match an input regexp against the
function name or the documentation or both.  Use `C-M-j' (equivalent
here to `C-q C-g C-j') to input the default separator.

For example, use input

\"dired.*^G
\[^^G]*file\"

with `S-TAB' to match all functions whose names contain \"dired\" and
whose documentation contains \"file\".  Here, `[^^G]' matches any
character except ^G, which includes newline.  If you use `.*' here,
instead, then only the first lines of doc strings are searched.

With a prefix argument, use the same documentation that was gathered
the last time `icicle-fundoc' was called.  Use a prefix arg to save
the time that would be needed to gather the documentation.

You can use `C-$' during completion to toggle limiting the domain of
initial candidates to functions that are commands (interactive).

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-cycle-incremental-completion] to toggle incremental completion.

See also: `icicle-apropos-value', using a negative prefix arg." ; Doc string
  icicle-doc-action                     ; Action function
  prompt                                ; `completing-read' args
  (let ((result  (and pref-arg  icicle-fundoc-last-initial-cand-set)))
    (unless result                      ; COLLECTION arg is an alist whose items are ((symb doc)).
      (mapatoms
       (lambda (symb)                   ; Each completion candidate is a list of strings.
         (when (fboundp symb)
           ;; Ignore symbols that produce errors.  Example: In Emacs 20, `any', which is defalias'd
           ;; to `icicle-anything', raises this error: "Symbol's function definition is void: any".
           ;; This is caused by the `after' advice `ad-advised-docstring' that is defined by Emacs
           ;; itself for function `documentation'.  It is not a problem for Emacs 22+.
           (let ((doc  (condition-case nil (documentation symb) (error nil))))
             (when (and doc  (icicle-non-whitespace-string-p (icicle-fn-doc-minus-sig doc)))
               (push (list (list (symbol-name symb) doc)) result))))))
      (setq icicle-fundoc-last-initial-cand-set  result))
    result)
  nil nil nil 'icicle-doc-history nil nil
  ((prompt                             "FUNC `C-M-j' DOC: ") ; Bindings
   (icicle-toggle-transforming-message "Filtering to commands is now %s")
   (icicle-transform-function           nil) ; No transformation: all symbols.
   (icicle-last-transform-function      (lambda (cands) ; `C-$': only commands.
                                          (loop
                                           for cc in cands
                                           with symb
                                           do (setq symb  (intern (icicle-transform-multi-completion cc)))
                                           if (commandp symb)
                                           collect cc)))
   (icicle-candidate-properties-alist  '((1 (face icicle-candidate-part))))
   (icicle-multi-completing-p          t)
   (icicle-list-use-nth-parts          '(1))
   (pref-arg                           current-prefix-arg))
  (progn
    (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
    (icicle-highlight-lighter)
    (message "Gathering function descriptions...")))

(defun icicle-fn-doc-minus-sig (docstring)
  "Return DOCSTRING minus the function signature (usage info)."
  (let ((sig-p  (string-match "\n\n(fn\\(\\( .*\\)?)\\)\\'" docstring)))
    (if sig-p (substring docstring 0 (match-beginning 0)) docstring)))

;;;###autoload (autoload 'icicle-plist "icicles")
(icicle-define-command icicle-plist     ; Command name
  "Choose a symbol and its property list.
Each candidate for completion is a symbol name plus its property list
\(as a string).  They are separated by `icicle-list-join-string'
\(^G^J, by default).  You can match an input regexp against the symbol
name or the property list or both.  Use `C-M-j' (equivalent here to
`C-q C-g C-j') to input the default separator.

With a positive prefix argument, use the same initial set of
candidates that were gathered the last time `icicle-plist' was called.
Use a positive prefix arg to save the time that would be needed to
gather the plists.

With a negative prefix arg, do not pretty-print each property list, in
buffers `*Help* and `*Completions*'.  Generation of the complete set
of candidates is about twice as fast when not pretty-printed, but the
time to match your input and display candidates is the same, and the
match-and-display time for empty input is much longer than the
generation time.

The time to repeat (positive prefix arg) is the same, whether or not
candidates were pretty-printed the first time.

Note: Plists are never pretty-printed for Emacs 20, because that seems
to cause an Emacs crash.

You can use `C-$' during completion to toggle limiting the domain of
initial candidates to functions that are commands (interactive).

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-cycle-incremental-completion] to toggle incremental completion.

See also: `icicle-apropos-value', using a positive prefix arg." ; Doc string
  icicle-doc-action                     ; Action function
  prompt                                ; `completing-read' args
  (let ((result  (and pref-arg  (wholenump (prefix-numeric-value pref-arg))
                      icicle-plist-last-initial-cand-set)))
    (unless result                      ; COLLECTION arg: an alist with items ((symb plist-string))
      (mapatoms
       (lambda (symb)                 ; Each completion candidate is a list of strings.
         (condition-case nil          ; Ignore symbols that produce errors.
             (let ((plist  (symbol-plist symb)))
               (when plist
                 (push (list (list (symbol-name symb)
                                   (if (or (< (prefix-numeric-value pref-arg) 0)
                                           (< emacs-major-version 21)) ; Emacs 20 crash if pprint.
                                       (format "%s" plist)
                                     (pp-to-string plist))))
                       result)))
           (error nil))))
      (setq icicle-plist-last-initial-cand-set  result))
    result)
  nil nil nil nil nil nil
  ((prompt                             "SYMB `C-M-j' PLIST: ") ; Bindings
   (icicle-toggle-transforming-message "Filtering to faces is now %s")
   (icicle-transform-function           nil) ; No transformation: all symbols.
   (icicle-last-transform-function
    (lambda (cands)                     ; `C-$': only faces.
      (loop for cc in cands
            with symb
            do (setq symb  (intern (icicle-transform-multi-completion cc)))
            if (facep symb)
            collect cc)))
   (icicle-candidate-properties-alist  '((1 (face icicle-candidate-part))))
   (icicle-multi-completing-p          t)
   (icicle-list-use-nth-parts          '(1))
   (pref-arg                           current-prefix-arg))
  (progn
    (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
    (icicle-highlight-lighter)
    (message "Gathering property lists...")))

;;;###autoload (autoload 'icicle-doc "icicles")
(icicle-define-command icicle-doc       ; Command name
  "Choose documentation for a symbol.
Each candidate for completion is the description of a function,
variable, or face.  Displays the documentation and returns the symbol.

Each candidate for completion is a symbol name plus its type
\(FUNCTION, VARIABLE, or FACE) and its documentation.  These candidate
components are separated by `icicle-list-join-string' (\"^G^J\", by
default).  You can match an input regexp against the symbol name,
type, or the documentation or any combination of the three.  Use
`C-M-j' (equivalent here to `C-q C-g C-j') to input the default
separator.

With a prefix argument, use the same documentation that was gathered
the last time `icicle-doc' was called.  Use a prefix arg to save the
time that would be needed to gather the documentation.

You can use `C-$' during completion to toggle filtering the domain of
initial candidates between all functions, variables, and faces and
only commands, user options and faces.

Remember that you can use \\<minibuffer-local-completion-map>\
`\\[icicle-cycle-incremental-completion]' to toggle incremental completion.

See also: `icicle-apropos-value'."      ; Doc string
  icicle-doc-action                     ; Action function: display the doc.
  prompt                                ; `completing-read' args
  (let ((result  (and pref-arg  icicle-doc-last-initial-cand-set))
        doc)                            ; Each completion candidate is a list of strings.
    (unless result                      ; COLLECTION arg is an alist with items (doc . symb).
      (mapatoms
       (lambda (symb)
         (progn
           (when (and (functionp symb)  ; Function's doc.
                      ;; Ignore symbols that produce errors.  See comment for `icicle-fundoc'.
                      (setq doc  (condition-case nil (documentation symb) (error nil)))
                      (setq doc  (icicle-fn-doc-minus-sig doc)) ; Need separate `setq', for `and'.
                      (icicle-non-whitespace-string-p doc)
                      (setq doc  (concat doc "\n\n")))
             (push (cons (list (concat (symbol-name symb) icicle-list-join-string "FUNCTION") doc)
                         symb)
                   result))
           (when (and (boundp symb)     ; Variable's doc (and keymap var's bindings if remove nil)
                      (setq doc  (documentation-property symb 'variable-documentation))
                      (icicle-non-whitespace-string-p doc))
             (when (and nil             ; $$$ Remove nil to get keymaps, but it slows things down.
                        (fboundp 'describe-keymap)
                        (keymapp (symbol-value symb)))
               (setq doc  (concat (symbol-name symb) ":\n" doc "\n\n" ; Keymap variable's doc.
                                  (substitute-command-keys
                                   (concat "\\{" (symbol-name symb) "}"))
                                  "\n\n")))
             (setq doc  (concat doc "\n\n"))
             (push (cons (list (concat (symbol-name symb) icicle-list-join-string "VARIABLE") doc)
                         symb)
                   result))
           (when (and (facep symb)
                      (setq doc  (documentation-property symb 'face-documentation)))
             (push (cons (list (concat (symbol-name symb) icicle-list-join-string "FACE") doc)
                         symb)
                   result)))))
      (setq icicle-doc-last-initial-cand-set  result))
    result)
  nil nil nil 'icicle-doc-history nil nil
  ((prompt                             "Find doc using regexp: ") ; Bindings
   ;; $$$$$$ (icicle-transform-function          'icicle-remove-duplicates) ; Duplicates are due to `fset's.
   (icicle-toggle-transforming-message  "Filtering to OPTIONS, COMMANDS, & FACES is now %s")
   (icicle-transform-function           nil) ; No transformation: all symbols.
   (icicle-last-transform-function      (lambda (cands) ; `C-$': only user options, commands, or faces.
                                          (loop for cc in cands
                                                with symb
                                                do (setq symb (intern (icicle-transform-multi-completion cc)))
                                                if (or (user-variable-p symb)  (commandp symb)  (facep symb))
                                                collect cc)))
   (icicle-candidate-properties-alist  '((1 (face icicle-candidate-part))))
   (icicle-multi-completing-p          t)
   (icicle-list-use-nth-parts          '(1))
   (icicle-candidate-help-fn           'icicle-doc-action)
   (pref-arg                           current-prefix-arg))
  (progn
    (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
    (icicle-highlight-lighter)
    (message "Gathering documentation...")))

(defun icicle-doc-action (entry)
  "Completion action function for `icicle-doc': Display the doc."
  (let ((symb  (intern (icicle-transform-multi-completion entry))))
    (cond ((fboundp symb)               (describe-function symb))
          ;; $$$ This works fine, but it slows things down:
          ;; ((and (fboundp 'describe-keymap)  (boundp symb)  (keymapp (symbol-value symb)))
          ;;  (describe-keymap symb))
          ((and symb  (boundp symb))    (describe-variable symb))
          ((facep symb)                 (describe-face symb)))
    symb))

;;;###autoload (autoload 'icicle-non-whitespace-string-p "icicles")
(defun icicle-non-whitespace-string-p (string)
  "Return non-nil if STRING is a string and contains a non-whitespace char.
The `standard-syntax-table' definition of whitespace is used."
  (interactive "s")
  (let ((orig-syntable  (syntax-table)))
    (unwind-protect
       (progn
         (set-syntax-table (standard-syntax-table))
         (and (stringp string)  (> (length string) 0)  (string-match "\\S-" string)))
      (set-syntax-table orig-syntable))))

;;;###autoload (autoload 'icicle-map "icicles")
(defalias 'icicle-map 'icicle-apply)
;;;###autoload (autoload 'icicle-apply "icicles")
(defun icicle-apply (alist fn &optional nomsg predicate initial-input hist def inherit-input-method)
  "Selectively apply a function to elements in an alist.
Argument ALIST is an alist such as can be used as the COLLECTION
argument for Icicles `completing-read'.  Its elements can represent
multi-completions, for example.  Interactively, COLLECTION is a
variable (a symbol) whose value is an alist.

Argument FN is a function.

Optional argument NOMSG non-nil means do not display an informative
message each time FN is applied.  If nil, then a message shows the key
of the alist element that FN is applied to and the result of the
application.

The remaining arguments are optional.  They are the arguments
PREDICATE, INITIAL-INPUT, HIST, DEF, and INHERIT-INPUT-METHOD for
`completing-read' (that is, all of the `completing-read' args other
than PROMPT, COLLECTION, and REQUIRE-MATCH).  During `icicle-apply'
completion, a match is required (REQUIRE-MATCH is t).

Interactively, you are prompted for both arguments.  Completion is
available for each.  The completion list for ALIST candidates is the
set of variables whose value is a cons.  With no prefix argument, the
names of these variables must end with \"alist\".  With a prefix
argument, the first car of each variable value must itself be a cons.

After choosing the ALIST and FN, you are prompted to choose one or
more keys of the alist elements, and FN is applied to each element
that has a key that you choose.  Multi-command completion is available
for choosing these candidates: you can apply FN to any number of
elements, any number of times.

Examples: If ALIST is `auto-mode-alist' and FN is `cdr', then the
completion candidates are the keys of the alist, and the result of
applying FN to an alist element is simply the value of that key.  If
you choose, for example, candidate \"\\.el\\'\", then the result is
`cdr' applied to the alist element (\"\\.el\\'\" . emacs-lisp-mode),
which is the symbol `emacs-lisp-mode'.  In this case, the function
performs simple lookup.

If FN were instead (lambda (x) (describe-function (cdr x))), then the
result of choosing candidate \"\\.el\\'\" would be to display the help
for function `emacs-lisp-mode'.

NOTE: `icicle-apply' does not, by itself, impose any particular sort
order.  Neither does it inhibit sorting.  If you call this function
from Lisp code and you want it to use a certain sort order or you want
no sorting, then bind `icicle-sort-comparer' accordingly.

During completion you can use multi-command keys.  Each displays the
value of applying FN to an alist element whose key is a completion
candidate.\\<minibuffer-local-completion-map>

`C-RET'   - Act on current completion candidate only
`C-down'  - Move to next completion candidate and act
`C-up'    - Move to previous completion candidate and act
`C-next'  - Move to next apropos-completion candidate and act
`C-prior' - Move to previous apropos-completion candidate and act
`C-end'   - Move to next prefix-completion candidate and act
`C-home'  - Move to previous prefix-completion candidate and act
`\\[icicle-all-candidates-action]'     - Act on *each* candidate (or each that is saved), in turn.
`\\[icicle-all-candidates-list-action]'     - Act on the list of *all* candidates (or all saved).

Note that `\\[icicle-all-candidates-list-action]' applies FN to the *list* of chosen alist elements,
whereas `\\[icicle-all-candidates-action]' applies FN to each chosen element, in turn.  For
example, if FN is `length' and your input is `\.el', then `\\[icicle-all-candidates-list-action]' displays
the result of applying `length' to the list of chosen elements:

 ((\"\\.el\\'\" . emacs-lisp-mode) (\"\\.elc'\" . emacs-lisp-mode))

which is 2.

When candidate action and cycling are combined (e.g. `C-next'), option
`icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`\\[icicle-help-on-candidate]', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate, or
`C-g' to quit.  This is an Icicles command - see command
`icicle-mode'.

`icicle-apply' overrides `icicle-buffer-ignore-space-prefix-flag',
binding it to nil so that candidates with initial spaces can be
matched."
  (interactive
   (list (symbol-value
          (intern
           (let* ((pred                                    `(lambda (s)
                                                             (unless (symbolp s)  (setq s  (intern s)))
                                                             (and (boundp s)  (consp (symbol-value s))
                                                              ,(if current-prefix-arg
                                                                   '(consp (car (symbol-value s)))
                                                                   '(string-match "alist$"
                                                                     (symbol-name s))))))
                  (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
                  (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
                  (icicle-candidate-alt-action-fn          (or icicle-candidate-alt-action-fn
                                                               (icicle-alt-act-fn-for-type "variable")))
                  (icicle-all-candidates-list-alt-action-fn
                   (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "variable"))))
             (completing-read
              "Alist (variable): " obarray (and icompletep  pred) t nil
              (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history)))))
         (read
          (let* ((pred                                    (lambda (s)
                                                            (unless (symbolp s) (setq s  (intern s)))
                                                            (functionp s)))
                 (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
                 (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
                 (icicle-candidate-alt-action-fn          (or icicle-candidate-alt-action-fn
                                                              (icicle-alt-act-fn-for-type "function")))
                 (icicle-all-candidates-list-alt-action-fn
                  (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "function"))))
            (completing-read
             "Function: " obarray (and icompletep  pred) nil nil
             (if (boundp 'function-name-history) 'function-name-history 'icicle-function-name-history))))))

  (setq icicle-candidate-entry-fn  fn)  ; Save in global variable - used by `icicle-apply-action'.
  (let ((icicle-candidate-action-fn              'icicle-apply-action)
        (icicle-all-candidates-list-action-fn    'icicle-apply-list-action)
        (icicle-buffer-ignore-space-prefix-flag  nil)
        (icicle-apply-nomsg                      nomsg)
        (enable-recursive-minibuffers            t))
    (icicle-explore
     (lambda ()
       (setq icicle-candidates-alist    ; Ensure that keys of ALIST are strings or conses.
             (mapcar (lambda (key+val)
                       (if (consp (car key+val))
                           key+val      ; Multi-completion candidate: (("aaa" "bbb") . ccc)
                         (cons (format "%s" (car key+val)) (cdr key+val))))
                     alist)))
     (lambda ()
       (let ((result  (funcall icicle-candidate-entry-fn icicle-explore-final-choice-full)))
         (unless nomsg
           (message "Key: %s,  Result: %s"
                    (icicle-propertize (car icicle-explore-final-choice-full)
                                       'face 'icicle-msg-emphasis)
                    (icicle-propertize result 'face 'icicle-msg-emphasis)))
         result))                       ; Return result.
     nil nil nil "Choose an occurrence: " predicate t initial-input hist def inherit-input-method)))

(defun icicle-apply-action (string)
  "Completion action function for `icicle-apply'."
  (unwind-protect
      (icicle-condition-case-no-debug icicle-apply-action
	  (progn
	    (icicle-highlight-candidate-in-Completions)
	    ;; Apply function to candidate element and display it.
	    (let* ((key+value  (funcall icicle-get-alist-candidate-function string))
		   (result     (funcall icicle-candidate-entry-fn key+value)))
	      (unless icicle-apply-nomsg
		(icicle-msg-maybe-in-minibuffer
                 "Key: %s,  Result: %s"
                 (icicle-propertize (car key+value) 'face 'icicle-msg-emphasis)
                 (icicle-propertize result 'face 'icicle-msg-emphasis))))
	    nil)			; Return nil for success.
	(error "%s" (error-message-string icicle-apply-action))) ; Return error msg.
    (select-window (minibuffer-window))
    (select-frame-set-input-focus (selected-frame))))

(defun icicle-apply-list-action (strings)
  "Completion action list function for `icicle-apply'."
  (unwind-protect
       (icicle-condition-case-no-debug icicle-apply-list-action
           (progn                       ; Apply function to candidate element and display it.
             (icicle-msg-maybe-in-minibuffer
              "Result: %s"
              (icicle-propertize
               (funcall icicle-candidate-entry-fn (mapcar icicle-get-alist-candidate-function strings))
               'face 'icicle-msg-emphasis))
             nil)                       ; Return nil for success.
         (error "%s" (error-message-string icicle-apply-list-action))) ; Return error msg.
    (select-window (minibuffer-window))
    (select-frame-set-input-focus (selected-frame))))

;;;###autoload (autoload 'icicle-goto-marker-or-set-mark-command "icicles")
(defun icicle-goto-marker-or-set-mark-command (arg) ; Bound to `C-@', `C-SPC'.
  "With prefix arg < 0, `icicle-goto-marker'; else `set-mark-command'.
By default, Icicle mode remaps all key sequences that are normally
bound to `set-mark-command' to
`icicle-goto-marker-or-set-mark-command'.  If you do not want this
remapping, then customize option `icicle-top-level-key-bindings'."
  (interactive "P")
  (if (not (wholenump (prefix-numeric-value arg)))
      (icicle-goto-marker)
    (setq this-command 'set-mark-command) ; Let `C-SPC C-SPC' activate if not `transient-mark-mode'.
    (set-mark-command arg)))

;;;###autoload (autoload 'icicle-goto-global-marker-or-pop-global-mark "icicles")
(defun icicle-goto-global-marker-or-pop-global-mark (arg) ; Bound to `C-x C-@', `C-x C-SPC'.
  "With prefix arg < 0, `icicle-goto-global-marker'; else `pop-global-mark'.
By default, Icicle mode remaps all key sequences that are normally
bound to `pop-global-mark' to
`icicle-goto-global-marker-or-pop-global-mark'.  If you do not want
this remapping, then customize option
`icicle-top-level-key-bindings'."
  (interactive "P")
  (if (wholenump (prefix-numeric-value arg))
      (pop-global-mark)
    (icicle-goto-global-marker)))

;;;###autoload (autoload 'icicle-goto-marker "icicles")
(defun icicle-goto-marker ()            ; Bound to `C-- C-@', `C-- C-SPC'.
  "Go to a marker in this buffer, choosing it by the line that includes it.
If `crosshairs.el' is loaded, then the target position is highlighted.

By default, candidates are sorted in marker order, that is, with
respect to their buffer positions.  Use `C-M-,' or `C-,' to change the
sort order.

During completion you can use these keys\\<minibuffer-local-completion-map>:

`C-RET'   - Goto marker named by current completion candidate
`C-down'  - Goto marker named by next completion candidate
`C-up'    - Goto marker named by previous completion candidate
`C-next'  - Goto marker named by next apropos-completion candidate
`C-prior' - Goto marker named by previous apropos-completion candidate
`C-end'   - Goto marker named by next prefix-completion candidate
`C-home'  - Goto marker named by previous prefix-completion candidate
`\\[icicle-delete-candidate-object]' - Delete marker named by current completion candidate

When candidate action and cycling are combined (e.g. `C-next'), option
`icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`\\[icicle-help-on-candidate]', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to choose a candidate as the final
destination, or `C-g' to quit.  This is an Icicles command - see
command `icicle-mode'."
  (interactive)
  (let ((icicle-sort-orders-alist  (cons '("by position" .  icicle-cdr-lessp) icicle-sort-orders-alist))
        (icicle-sort-comparer      'icicle-cdr-lessp))
    (icicle-goto-marker-1 mark-ring)))

;;;###autoload (autoload 'icicle-goto-global-marker "icicles")
(defun icicle-goto-global-marker ()     ; Bound to `C-- C-x C-@', `C-- C-x C-SPC'.
  "Like `icicle-goto-marker', but visits global, not local, markers.
If user option `icicle-show-multi-completion-flag' is non-nil, then
each completion candidate is annotated (prefixed) with the name of the
marker's buffer, to facilitate orientation."
  (interactive)
  (let ((icicle-multi-completing-p          icicle-show-multi-completion-flag)
        (icicle-list-nth-parts-join-string  "\t")
        (icicle-list-join-string            "\t")
        ;; $$$$$$ (icicle-list-end-string             "")
        (icicle-sort-orders-alist           (cons '("by buffer, then by position" . icicle-part-1-cdr-lessp)
                                                  icicle-sort-orders-alist))
        (icicle-sort-comparer               'icicle-part-1-cdr-lessp)
        (icicle-candidate-properties-alist  (and icicle-show-multi-completion-flag
                                                 '((1 (face icicle-candidate-part))))))
    (icicle-goto-marker-1 global-mark-ring)))

(defun icicle-goto-marker-1 (ring)
  "Helper function for `icicle-goto-marker', `icicle-goto-global-marker'.
RING is the marker ring to use."
  (unwind-protect
       (let* ((global-ring-p
               (memq this-command '(icicle-goto-global-marker
                                    icicle-goto-global-marker-or-pop-global-mark)))
              (markers
               (if (and (not global-ring-p)  (marker-buffer (mark-marker)))
                   (cons (mark-marker) (icicle-markers ring))
                 (icicle-markers ring)))
              (icicle-delete-candidate-object
               (lambda (cand)
                 (let ((mrkr+txt  (funcall icicle-get-alist-candidate-function cand)))
                   (move-marker (cdr mrkr+txt) nil))))
              (icicle-alternative-sort-comparer  nil)
              (icicle-last-sort-comparer         nil)
              (icicle-orig-buff                  (current-buffer)))
         (unless (consp markers)
           (error (if global-ring-p "No global markers" "No markers in this buffer")))
         (cond ((cdr markers)
                (icicle-apply (mapcar (lambda (mrkr) (icicle-marker+text mrkr global-ring-p))
                                      markers)
                              #'icicle-goto-marker-1-action
                              'nomsg
                              (lambda (cand)
                                (marker-buffer (cdr cand)))))
               ((= (point) (car markers)) (message "Already at marker: %d" (point)))
               (t
                (icicle-goto-marker-1-action (icicle-marker+text (car markers) global-ring-p)))))
    (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch))))

(defun icicle-goto-marker-1-action (cand)
  "Action function for `icicle-goto-marker-1'."
  (pop-to-buffer (marker-buffer (cdr cand)))
  (select-frame-set-input-focus (selected-frame))
  (goto-char (cdr cand))
  (unless (pos-visible-in-window-p) (recenter icicle-recenter))
  (when (fboundp 'crosshairs-highlight) (crosshairs-highlight)))

(defun icicle-marker+text (marker &optional globalp)
  "Cons of text line that includes MARKER with MARKER itself.
If the marker is on an empty line, then text \"<EMPTY LINE>\" is used.
If both optional argument GLOBALP and option
`icicle-show-multi-completion-flag' are non-nil, then the text is
prefixed by MARKER's buffer name."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (let ((line  (let ((inhibit-field-text-motion  t)) ; Just to be sure, for `line-end-position'.
                     (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (buff  (and globalp  icicle-show-multi-completion-flag  (buffer-name)))
            (help  (and (or (> icicle-help-in-mode-line-delay 0) ; Get it only if user will see it.
                            (and (boundp 'tooltip-mode)  tooltip-mode))
                        (format "Line: %d, Char: %d" (line-number-at-pos) (point)))))
        (when (string= "" line) (setq line  "<EMPTY LINE>"))
        (when help
          (icicle-candidate-short-help help line)
          (when (and globalp  icicle-show-multi-completion-flag)
            (icicle-candidate-short-help help buff)))
        (if (and globalp  icicle-show-multi-completion-flag)
            (cons (list buff line) marker)
          (cons line marker))))))

(defun icicle-markers (ring)
  "Marks in mark RING that are in live buffers other than a minibuffer."
  (let ((markers  ()))
    (dolist (mkr  ring)
      (when (and (buffer-live-p (marker-buffer mkr))
                 (not (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name (marker-buffer mkr)))))
        (push mkr markers)))
    markers))

;;;###autoload (autoload 'icicle-exchange-point-and-mark "icicles")
(defun icicle-exchange-point-and-mark (&optional arg) ; Bound to `C-x C-x'.
  "`exchange-point-and-mark' or save a region or select a saved region.
With no prefix arg, invoke `exchange-point-and-mark'.
If you use library `Bookmark+', then you can use a prefix arg.

 * Plain `C-u':

   Emacs 22+: what vanilla Emacs does - temporary Transient Mark mode.
   Emacs 20/21: same as `C-u C-u'.

 * Plain `C-u C-u': select (activate) one or more bookmarked regions.

 * Numeric prefix arg: bookmark (save) the active region using
   `icicle-bookmark-cmd'.

   Arg < 0: Prompt for the bookmark name.
   Arg > 0: Do not prompt for the bookmark name.  Use the buffer name
            plus a prefix of the region text as the bookmark name.
   Arg = 0: Same as > 0, except do not overwrite any existing bookmark
            with the same name.

By default, Icicle mode remaps all key sequences that are normally
bound to `exchange-point-and-mark' to
`icicle-exchange-point-and-mark'.  If you do not want this remapping,
then customize option `icicle-top-level-key-bindings'."
  (interactive "P")
  (cond ((not arg) (exchange-point-and-mark))
        ((and (consp arg)  (< (prefix-numeric-value arg) 16)  (> emacs-major-version 21))
         (exchange-point-and-mark arg))
        ((not (featurep 'bookmark+))
         (error "You must load library `Bookmark+' to use a prefix arg"))
        ((atom arg)
         (unless (and transient-mark-mode mark-active  (not (eq (mark) (point))))
           (error "Cannot bookmark inactive region: you must activate it first"))
         (icicle-bookmark-cmd (and (natnump (prefix-numeric-value arg))  9)))
        (t
         (bookmark-maybe-load-default-file)
         (unless (consp (bmkp-region-alist-only)) (error "No bookmarked regions"))
         (call-interactively #'icicle-select-bookmarked-region))))

;;;###autoload (autoload 'icicle-search-generic "icicles")
(defun icicle-search-generic ()         ; Bound to `C-c `'.
  "Run `icicle-search-command'.  By default, this is `icicle-search'.
In Compilation and Grep modes, this is `icicle-compilation-search'.
In Comint, Shell, GUD, and Inferior Lisp modes, this is
   `icicle-comint-search'."
  (interactive)
  (call-interactively icicle-search-command))

;;;###autoload (autoload 'icicle-search "icicles")
(defun icicle-search (beg end scan-fn-or-regexp require-match ; Bound to `M-s M-s M-s', `C-c `'.
                      &optional where &rest args)
  "Search for matches, with completion, cycling, and hit replacement.
Search a set of contexts, which are defined interactively by
specifying a regexp (followed by `RET').

After specifying the regexp that defines the search contexts, type
input (e.g. regexp or other pattern) to match within the contexts.
The contexts that match your input are available as completion
candidates.  You can use `\\<minibuffer-local-completion-map>\
\\[icicle-apropos-complete-and-narrow]' to further narrow the candidates,
typing additional patterns to match.

By default, candidates are in order of buffer occurrence, but you can
sort them in various ways using `\\[icicle-change-sort-order]'.

You can replace individual matches with another string, as in
`query-replace' or `query-replace-regexp'.  See the Icicles Search doc
for more info.

Non-interactively, search can be for regexp matches or any other kind
of matches.  Argument SCAN-FN-OR-REGEXP is the regexp to match, or it
is a function that defines an alist of buffer zones to search.  You
can navigate among the matching buffer zones (defined either as regexp
matches or via function), called search \"contexts\", and you can
match another regexp against the text in a search context.  See the
end of this description for information about the other arguments.

If the search-context regexp contains regexp subgroups, that is,
subexpressions of the form `\(...\)', then you are prompted for the
subgroup to use to define the search contexts.  Subgroup 0 means the
context is whatever matches the whole regexp.  Subgroup 1 means the
context is whatever matches the first subgroup, and so on.  The
subgroup number is the number of occurrences of `\(', starting at the
beginning of the regexp.

Search respects `icicle-regexp-quote-flag' and
`icicle-search-whole-word-flag'.  You can toggle these during search,
by using `\\[icicle-toggle-regexp-quote]' and `\\[icicle-dispatch-M-q]', respectively.  \
If `icicle-regexp-quote-flag'
is non-nil, then regexp special characters are quoted, so that they
become non-special.  If `icicle-search-whole-word-flag' is non-nil,
then whole-word searching is done.  (You can also use `M-s M-s w' to
perform word search.)

For each of the predefined Icicles search commands, including for
`icicle-search' itself, you can alternatively choose to search, not
the search contexts as you define them, but the non-contexts, that is,
the buffer text that is outside (in between) the search contexts as
defined.  For example, if you use `icicle-search-thing' and you define
sexps as the search contexts, then this feature lets you search the
zones of text that are not within a sexp.

To do this, use `\\[icicle-toggle-search-complementing-domain]' (`icicle-toggle-search-complementing-domain')
during completion to turn on `icicle-search-complement-domain-p'.
\(This is a toggle, and it affects only future search commands, not
the current one.)


Optional Behaviors: Prefix Argument
-----------------------------------

By default, search only the current buffer.  Search the active region,
or, if there is none, then search the entire buffer.

With a prefix argument, you can search multiple buffers, files, or
bookmarks, as follows:

- With a plain prefix arg (`C-u'), search bookmarks.  This is the
same as command `icicle-search-bookmarks-together'.  (To search
bookmarks one at a time instead of together, use multi-command
`icicle-search-bookmark'.)

- With a positive numeric prefix arg, search multiple buffers
completely.  You are prompted for the buffers to search - all of each
buffer is searched.  Any existing buffers can be chosen.  If the
prefix arg is 99, then only buffers visiting files are candidates.
This is the same as command `icicle-search-buffer'.

- With a negative numeric prefix arg, search multiple files
completely.  You are prompted for the files to search - all of each
file is searched.  Any existing files in the current directory can be
chosen.  This is the same as command `icicle-search-file'.

- With a zero (0) prefix arg, search contexts that are determined in a
  context-dependent way.  For example:

  . in Dired, search the marked files
  . in a `buffer-menu' or `ibuffer' buffer, search the marked buffers
  . in a bookmark list buffer, search the marked bookmarks


Navigation and Help
-------------------

The use of completion for this command is special.  It is not unusual
in this context to have multiple completion candidates that are
identical - only the positions of their occurrences in the search
buffer(s) differ.  In that case, you cannot choose one simply by
completing it in the minibuffer, because the destination would be
ambiguous.  That is, simply completing your input and entering the
completion with `RET' will not take you to its occurrence in the
search buffer, unless it is unique.

Instead, choose search hits to visit using any of the candidate-action
keys: `C-RET', `C-mouse-2', `C-down', `C-up', `C-next', `C-prior',
`C-end', and `C-home'.  All but the first two of these cycle among the
search hits.  The current candidate in `*Completions*' corresponds to
the current location visited (it is not off by one, as is usually the
case in Icicles).

As always, the `C-M-' keys provide help on individual candidates:
`\\[icicle-help-on-candidate]', `C-M-mouse-2', `C-M-down', `C-M-up', `C-M-next',
`C-M-prior', `C-M-end', and `C-M-home'.  For `icicle-search', they
indicate the buffer and position of the search hit.

You can cycle among candidates without moving to their occurrences in
the search buffer, using `down', `up', `next', `prior', `end', or
`home' (no `C-' modifier).


Highlighting
------------

In the search buffer (that is, where the hits are), `icicle-search'
does the following:

- Highlights the current match (buffer zone) for the initial (context)
  regexp, using face `icicle-search-main-regexp-current'.

- Highlights the first `icicle-search-highlight-threshold' context
  matches (or all, if the option value is `t'), using face
  `icicle-search-main-regexp-others'.

- Highlights 1-8 context levels, within each search context.  This
  happens only if your initial (context) regexp has \\(...\\) groups
  and option `icicle-search-highlight-context-levels-flag' is non-nil.

- Highlights the match for your current input, using face
  `icicle-search-current-input'.  Highlights all such matches if
  option `icicle-search-highlight-all-current-flag' is non-nil;
  otherwise, highlights just the currently visited match.
  You can toggle this option using `\\[icicle-dispatch-C-^]'.

If user option `icicle-search-cleanup-flag' is non-nil (the default),
then all search highlighting is removed from the search buffer when
you are finished searching.  If it is nil, then you can remove this
highlighting later using command `icicle-search-highlight-cleanup'.
You can toggle `icicle-search-cleanup-flag' during Icicles search
using `\\[icicle-dispatch-C-.]' in the minibuffer.


`*Completions*' Display
-----------------------

In buffer `*Completions*', in addition to eliding the common match
\(option `icicle-hide-common-match-in-Completions-flag', toggled
anytime using `\\[icicle-dispatch-C-x.]' - no prefix arg), you can elide all lines of a
multi-line candidate that do not match your current minibuffer input.

This hiding is governed by option
`icicle-hide-non-matching-lines-flag', which you can toggle anytime
during completion using `C-u \\[icicle-dispatch-C-x.]' (this is not specfic to Icicles
search).  This can be useful when candidates are very long, as can be
the case for instance for the `icicle-imenu-*-full' commands.


Search and Replace
------------------

You can replace the current search match by using any of the
alternative action keys: `\\[icicle-candidate-alt-action]', `C-S-mouse-2' (in
`*Completions*'), `\\[icicle-next-candidate-per-mode-alt-action]', \
`\\[icicle-previous-candidate-per-mode-alt-action]', `\\[icicle-next-apropos-candidate-alt-action]', \
`\\[icicle-help-on-previous-apropos-candidate]',
`\\[icicle-next-prefix-candidate-alt-action]', and `\\[icicle-previous-prefix-candidate-alt-action]'.  \
You can use `\\[icicle-all-candidates-list-alt-action]' to replace all matches
at once.  (And remember that you can activate the region to limit the
search-and-replace space.)


At the first use of any of these, you are prompted for the replacement
string; it is used thereafter, or until you use `\\[icicle-dispatch-M-comma]'
\(`icicle-search-define-replacement') to change it (anytime).

Unlike `query-replace', you need not visit search matches successively
or exhaustively.  You can visit and replace selected matches in any
order.

What is meant here by a \"search match\"?  It can be either an entire
search context or just a part of the context that your current
minibuffer input matches.

`\\[icicle-dispatch-M-_]' toggles option `icicle-search-replace-whole-candidate-flag'.  By
default, the entire current search context is replaced, that is,
whatever matches the context regexp that you entered initially using
`RET'.  However, you can use `\\[icicle-dispatch-M-_]' anytime during searching to toggle
between this default behavior and replacement of whatever your current
minibuffer input matches.

Remember this:

 - If `icicle-search-replace-whole-candidate-flag' is non-nil, then
   the granularity of replacement is a complete search context.  In
   this case, replacement behaves similarly to `query-replace-regexp'.
   You can still use minibuffer input to filter the set of search
   contexts, but replacement is on a whole-context basis.

 - If `icicle-search-replace-whole-candidate-flag' is nil, then you
   can replace multiple input matches separately within a search
   context (using `\\[icicle-candidate-alt-action]').  This behavior is unique to Icicles.
   You cannot, however skip over one input match and replace the next
   one in the same context - `\\[icicle-candidate-alt-action]' always replaces the first
   available match.

If `icicle-search-replace-whole-candidate-flag' is non-nil, then you
can use the navigational alternative action keys, `\\[icicle-next-candidate-per-mode-alt-action]',
`\\[icicle-previous-candidate-per-mode-alt-action]', `\\[icicle-help-on-next-apropos-candidate]', \
`\\[icicle-previous-apropos-candidate-alt-action]', `\\[icicle-next-prefix-candidate-alt-action]', and \
`\\[icicle-previous-prefix-candidate-alt-action]',
repeatedly to replace successive search contexts.  At the buffer
limits, these commands wraps around to the other buffer limit (last
search context to first, and vice versa).

Search traversal using these go-to-next-context-and-replace keys is
always by search context, not by individual input match.  This means
that you cannot use these keys to replace input matches within a
search context (except for the first such match, if
`icicle-search-replace-whole-candidate-flag' is nil).

If your input matches multiple parts of a search context, and you want
to replace these in order, then use `\\[icicle-candidate-alt-action]' repeatedly.  You can
traverse all matches of your input in the order they appear in the
buffer by repeating `\\[icicle-candidate-alt-action]' (provided the replacement text does
not also match your input - see below).  At the buffer limits,
repeating `\\[icicle-candidate-alt-action]' wraps around too.

`\\[icicle-candidate-alt-action]' always replaces the first input match in the current
search context or, if there are no matches, then the first input match
in the next context.  This behavior has these important consequences:

* If you repeat `\\[icicle-candidate-alt-action]' and the previous replacement no longer
  matches your input, then `\\[icicle-candidate-alt-action]' moves on to the next input
  match (which is now the first one) and replaces that.  This is why
  you can usually just repeat `\\[icicle-candidate-alt-action]' to successively replaces
  matches of  your input, including from one context to the next.

* If, on the other hand, after replacement the text still matches your
  input, then repeating `\\[icicle-candidate-alt-action]' will just replace that match.
  For example, if you replace the input match `ab' by `abcd', then
  repeating `\\[icicle-candidate-alt-action]' produces `abcd', then `abcdcd', then
  `abcdcd'...

* You cannot replace an input match, skip the next match, and then
  replace the following one, all in the same context.  You can,
  however, replace some matches and then skip (e.g. `C-next') to the
  next context.

What your input matches, hence what gets replaced if
`icicle-search-replace-whole-candidate-flag' is nil, depends on a few
Icicles options:

 - `icicle-regexp-quote-flag' determines whether to use regexp
   matching or literal matching.

 - `icicle-expand-input-to-common-match',
   `icicle-search-highlight-all-current-flag', and
   `icicle-search-replace-common-match-flag' together determine
   whether to replace exactly what your input matches in the current
   search hit or the expanded common match (ECM) of your input among
   all search hits.  If the first of these does not call for automatic
   input expansion, or if either of the other two is nil, then your
   exact input match is replaced.  Otherwise, the ECM is replaced.

Finally, the replacement string can be nearly anything that is allowed
as a replacement by `query-replace-regexp'.  In Emacs 22 or later,
this includes Lisp sexp evaluation via `\,' and constructs such as
`\#' and `\N' (back references).  You can also use `\?', but it is not
very useful - you might as well use `M-,' instead, to change the
replacement text.


Using Regexps
-------------

At any time, you can use `\\[icicle-insert-string-from-variable]' (command
`icicle-insert-string-from-variable') to insert text (e.g. a regexp)
from a variable into the minibuffer.  For example, you can search for
ends of sentences by using `C-u \\[icicle-insert-string-from-variable]' and choosing variable
`sentence-end' as the variable.  And you can use
`\\[icicle-save-string-to-variable]' to save a string to a variable
for later use by `\\[icicle-insert-string-from-variable]'.

When employed with useful regexps, `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]' can turn `icicle-search' into
a general navigator or browser of code, mail messages, and many other
types of buffer.  Imenu regexps work fine, for example - command
`icicle-imenu' simply uses `icicle-search' this way.  See
`icicle-insert-string-from-variable' for more tips on inserting
regexps from variables.


Additional Information
----------------------

If user option `icicle-show-multi-completion-flag' is non-nil, then
each candidate is annotated with the name of the buffer where the
search hit occurs, to facilitate orientation.  Note that even when the
value is nil, you can use `C-M-mouse-2' and so on to see the buffer
name, as well as the position of the hit in the buffer.

Completion is lax if `icicle-show-multi-completion-flag' is non-nil;
otherwise, it is strict.

After you visit a completion candidate, the hooks in variable
`icicle-search-hook' are run.

`icicle-search' sets `icicle-search-final-choice' to the final user
choice, which might not be one of the search candidates if
REQUIRE-MATCH is nil.


Non-Interactive Use
-------------------

Function `icicle-search' is not only a powerful command, it is also a
building block for creating your own Icicles search-and-replace
commands.  When called non-interactively, these are the
`icicle-search' arguments:

BEG is the beginning of the region to search; END is the end.
SCAN-FN-OR-REGEXP: Regexp or function that determines the set of
  initial candidates (match zones).  If a function, it is passed, as
  arguments, the buffer to search, the beginning and end of the search
  region in that buffer, and ARGS.
REQUIRE-MATCH is passed to `completing-read'.
Optional arg WHERE is a list of bookmarks, buffers, or files to be
  searched.  If nil, then search only the current buffer or region.
  (To search bookmarks you must also use library `Bookmark+').
ARGS are arguments that are passed to function SCAN-FN-OR-REGEXP.

Note that if SCAN-FN-OR-REGEXP is a regexp string, then function
`icicle-search-regexp-scan' is used to determine the set of match
zones.  You can limit hits to regexp matches that also satisfy a
predicate, by using `(PREDICATE)' as ARGS: PREDICATE is then passed to
`icicle-search-regexp-scan' as its PREDICATE argument.

This command is intended for use only in Icicle mode."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (setq icicle-search-context-regexp  (and (stringp scan-fn-or-regexp)  scan-fn-or-regexp))
  (let ((icicle-candidate-action-fn                  (or icicle-candidate-action-fn  'icicle-search-action))
        (icicle-candidate-help-fn                    'icicle-search-help)
        (icicle-all-candidates-list-alt-action-fn
         (or icicle-all-candidates-list-alt-action-fn  'icicle-search-replace-all-search-hits))
        (icicle-candidate-alt-action-fn
         (or icicle-candidate-alt-action-fn  'icicle-search-replace-search-hit))
        (icicle-scan-fn-or-regexp           scan-fn-or-regexp) ; Used free in `M-,'.
        (icicle-update-input-hook           (list 'icicle-search-highlight-all-input-matches))
        (icicle-search-ecm                  nil)
        (icicle-searching-p                 t)
        (icicle-search-replacement          nil)
        (icicle-current-input               "")
        (icicle-list-nth-parts-join-string  "\t")
        (icicle-list-join-string            "\t")
        ;; $$$$$$ (icicle-list-end-string   "")
        ;; In general, we do not assume that `C-0' implies the use of multi-completions.
        (icicle-multi-completing-p          (and current-prefix-arg
                                                 (not (zerop (prefix-numeric-value current-prefix-arg)))
                                                 icicle-show-multi-completion-flag))
        (icicle-list-use-nth-parts          '(1))
        (icicle-sort-comparer               nil)

        ;; Alternative: If we used `icicle-search-replace-cand-in-alist', then we would inhibit
        ;; sorting, because we would be depending on the alist order.
        ;;    (icicle-inhibit-sort-p          t)

        (icicle-no-match-hook               icicle-no-match-hook)
        (completion-ignore-case             case-fold-search)
        (replace-count                      0)) ; Defined in `replace.el'.  Used for replacement.
    (add-hook 'icicle-no-match-hook (lambda () (when (overlayp icicle-search-current-overlay)
                                                 (delete-overlay icicle-search-current-overlay))))
    (setq icicle-search-final-choice
          (icicle-explore (lambda () (icicle-search-define-candidates beg end scan-fn-or-regexp
                                                                        require-match where args))
                          #'icicle-search-final-act #'icicle-search-quit-or-error
                          #'icicle-search-quit-or-error #'icicle-search-cleanup
                          "Choose an occurrence: " nil require-match nil 'icicle-search-history))))

;; This is the same as `region-or-buffer-limits' in `misc-fns.el'.
(defun icicle-region-or-buffer-limits ()
  "Return the start and end of the region as a list, smallest first.
If the region is not active or is empty, then use bob and eob."
  (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
      (list (point-min) (point-max))
    (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))

(defun icicle-search-read-context-regexp (&optional prompt pred init hist def i-i-m)
  "Read context regexp and determine `icicle-search-context-level'.
Read the regexp using completion against previous regexp input.
The arguments are for use by `completing-read' to read the regexp.
 HIST (or `regexp-history' if HIST is nil) is used for the
 `completing-read' COLLECTION argument.
 The REQUIRE-MATCH arg to `completing-read' is nil.
 A default prompt is used if PROMPT is nil."
  (setq hist    (or hist  'regexp-history)
        prompt  (or prompt  (format "Search %swithin contexts (regexp): "
                                    (if icicle-search-complement-domain-p "*NOT* " ""))))
  (let* ((icicle-candidate-action-fn  nil)
         (icicle-candidate-help-fn    nil)
         (regexp                      (icicle-completing-read-history
                                       prompt 'regexp-history pred init def i-i-m)))
    (while (string= "" regexp)
      (message "Regexp cannot be empty.  Try again...") (sit-for 2)
      (setq regexp  (icicle-completing-read-history prompt 'regexp-history pred init def i-i-m)))
    (setq prompt                       "Subgroup to use as search context [0, 1, 2,...]: "
          icicle-search-context-level  (if (string-match "\\\\(" regexp)
                                           (truncate (if (fboundp 'read-number)
                                                         (read-number prompt 0)
                                                       (read-from-minibuffer ; Hope for a number.
                                                        prompt nil nil nil nil 0)))
                                         0))
    regexp))

(defmacro icicle-search-modes ()
  `(case major-mode
    ,@(append icicle-search-modes
              '((t (error "Icicles search WHERE is not implemented for this mode")))
              ())))

(defun icicle-search-where-arg ()
  "Return WHERE arg for `icicle-search*' commands, based on prefix arg."
  (cond ((consp current-prefix-arg)
         (unless (require 'bookmark+ nil t) (error "This requires library `Bookmark+'"))
         (message "Searching multiple bookmarks...") (sit-for 1)
         ;; $$$$$$ Originally, we just did this: (bmkp-region-alist-only)).  Now we let users choose.
         (let ((icicle-show-Completions-initially-flag  t)
               (icicle-prompt
                "Choose bookmarks to search (`RET' when done): "))
           (save-selected-window (icicle-bookmark-list))))

        ((= 0 (prefix-numeric-value current-prefix-arg)) (icicle-search-modes))
        ((wholenump current-prefix-arg)
         (message "Searching multiple buffers...") (sit-for 1)
         (icicle-search-choose-buffers (= 99 (prefix-numeric-value current-prefix-arg))))
        (current-prefix-arg
         (message "Searching multiple files...") (sit-for 1)
         (let ((icicle-show-Completions-initially-flag  t)
               (icicle-prompt                           "Choose file to search (`RET' when done): "))
           (save-selected-window (icicle-file-list))))
        (t nil)))

(defun icicle-search-choose-buffers (files-only-p)
  "Choose multiple buffers to search.
FILES-ONLY-P non-nil means that only buffers visiting files are
candidates."
  (let ((icicle-show-Completions-initially-flag  t))
    (mapcar #'get-buffer
            (let ((icicle-buffer-require-match-flag  'partial-match-ok)
                  (current-prefix-arg                files-only-p)
                  (icicle-prompt
                   (format "Choose %sbuffer to search (`RET' when done): "
                           (if files-only-p "file " ""))))
              (save-selected-window (icicle-buffer-list))))))

;;; $$$$$$ (defun icicle-search-read-word ()
;;;   "Read a word to search for (whole-word search).
;;; Regexp special characters within the word are escaped (quoted)."
;;;   (setq icicle-search-context-level  0)
;;;   (concat "\\b"
;;;           (regexp-quote (icicle-completing-read-history "Search for whole word: "
;;;                                                         'icicle-search-history))
;;;           "\\b"))

(defun icicle-search-read-word ()
  "Read a word to search for (whole-word search).
The search string is regarded as a whole word, but a \"word\" here can
contain embedded strings of non word-constituent chars (they are
skipped over, when matching, included in the match), and any leading
or trailing word-constituent chars in the search string are dropped
\(ignored for matching, not included in the match): matches begin and
end on a word boundary."
  (setq icicle-search-context-level  0)
  (concat "\\b" (replace-regexp-in-string
                 "\\W+" "\\W+" (replace-regexp-in-string
                                "^\\W+\\|\\W+$" ""
                                (icicle-completing-read-history "Search for whole word: "
                                                                'icicle-search-history))
                 nil t)
          "\\b"))

(defun icicle-search-final-act ()
  "Go to the final search hit choice, then run `icicle-search-hook'.
The hit's frame is raised and selected."
  (let* ((marker  (cdr icicle-explore-final-choice-full))
         (buf     (marker-buffer marker)))
    (unless (bufferp buf) (error "No such buffer: %s" buf))
    (pop-to-buffer buf)
    (raise-frame)
    (goto-char (marker-position marker))
    (unless (pos-visible-in-window-p) (recenter icicle-recenter))
    (select-frame-set-input-focus (selected-frame))
    (run-hooks 'icicle-search-hook)))

;; Free vars here: `icicle-orig-pt-explore', `icicle-orig-win-explore' are bound in `icicle-explore'.
(defun icicle-search-quit-or-error ()
  "Return to the starting point."
  (when (window-live-p icicle-orig-win-explore)
    (select-window icicle-orig-win-explore)
    (goto-char icicle-orig-pt-explore)))

;; Free vars here: `icicle-orig-win-explore' is bound in `icicle-explore'.
(defun icicle-search-cleanup ()
  "Clean up search highlighting, if `icicle-search-cleanup-flag'.
Select original window."
  (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
  (when (window-live-p icicle-orig-win-explore)
    (select-window icicle-orig-win-explore)
    (select-frame-set-input-focus (selected-frame))))

(defun icicle-search-define-candidates (beg end scan-fn-or-regexp require-match where args)
  "Define completion candidates for `icicle-search'.
The arguments are the same as for `icicle-search'."
  (when (and icicle-regexp-quote-flag  (not icicle-search-whole-word-flag)  (stringp scan-fn-or-regexp))
    (setq scan-fn-or-regexp  (regexp-quote scan-fn-or-regexp)))
  (let ((message-log-max  nil)
        (nb-contexts      0)
        (nb-objects       (length where)))
    (cond ((and (consp where)  (bufferp (car where))) ; List of buffers - search buffers.
           (dolist (buf  where)
             (incf nb-contexts)
             (message "%d contexts; searching %d/%d: `%s'"
                      (length icicle-candidates-alist) nb-contexts nb-objects
                      buf)
             (icicle-search-define-candidates-1 buf nil nil scan-fn-or-regexp args)))
          ((and (consp where)  (stringp (car where)) ; List of files - search files.  (Check only the first.)
                (or (icicle-file-remote-p (car where)) ; Don't let Tramp try to access it.
                    (file-exists-p (car where))))
           (dolist (file  where)
             (incf nb-contexts)
             (message "%d contexts; searching %d/%d: `%s'"
                      (length icicle-candidates-alist) nb-contexts nb-objects
                      (file-relative-name file default-directory))
             (icicle-search-define-candidates-1 (find-file-noselect file 'nowarn) nil nil
                                                scan-fn-or-regexp args)))
          ((and (consp where)  (consp (car where))) ; Search bookmarks - or just their regions if defined.
           (unless (require 'bookmark+ nil t) (error "This requires library `Bookmark+'"))
           (let ((non-existent-buffers  ())
                 buf+beg  buf  beg  end)
             (dolist (bmk  where)
               (incf nb-contexts)
               (message "%d contexts; searching %d/%d: `%s'"
                        (length icicle-candidates-alist) nb-contexts nb-objects
                        bmk)
               (setq buf+beg  (bookmark-jump-noselect bmk)
                     buf      (car buf+beg)
                     beg      (cdr buf+beg)
                     end      (bmkp-get-end-position bmk))
               (when (and beg  end  (= beg end)) ; Search whole buffer if bookmarked region is empty.
                 (setq beg  nil
                       end  nil))
               (if (bufferp buf)
                   (icicle-search-define-candidates-1 buf beg end scan-fn-or-regexp args)
                 (push buf non-existent-buffers)))
             (when non-existent-buffers
               (message "Skipping non-existent buffers: `%s'"
                        (mapconcat #'identity (icicle-remove-duplicates non-existent-buffers)
                                   "', `"))
               (sit-for 3))))
          (t                            ; Search this buffer only.
           (icicle-search-define-candidates-1 nil beg end scan-fn-or-regexp args))))
  (when (and icicle-candidates-alist  (null (cdr icicle-candidates-alist)))
    (message "Moving to sole context") (sit-for 1.5))
  (unless icicle-candidates-alist  (if (functionp scan-fn-or-regexp)
                                       (error "No %ssearch contexts"
                                              (if icicle-search-complement-domain-p "COMPLEMENT " ""))
                                     (error "No %ssearch contexts for `%s'"
                                            (if icicle-search-complement-domain-p "COMPLEMENT " "")
                                            scan-fn-or-regexp)))
  (setq mark-active  nil))              ; Remove any region highlighting, so we can see search hits.

(defun icicle-search-define-candidates-1 (buffer beg end scan-fn-or-regexp args)
  "Helper function for `icicle-search-define-candidates'.
BUFFER is a buffer to scan for candidates.
The other arguments are the same as for `icicle-search'."
  (if (functionp scan-fn-or-regexp)
      (apply scan-fn-or-regexp buffer beg end args)
    (apply 'icicle-search-regexp-scan buffer beg end scan-fn-or-regexp args)))

(defun icicle-search-regexp-scan (buffer beg end regexp &optional predicate action)
  "Scan BUFFER for REGEXP, pushing hits onto `icicle-candidates-alist'.
If BUFFER is nil, scan the current buffer.
If BEG and END are non-nil, scan only between positions BEG and END.
If REGEXP has subgroups, then use what the Nth subgroup matches as the
 search context (hit), where N = `icicle-search-context-level'.
 If N=0, then use the overall match of REGEXP as the search context.
PREDICATE is nil or a boolean function that takes these arguments:
  - the search-context string
  - a marker at the end of the search-context
If PREDICATE is non-nil, then push only the hits for which it holds.

Highlight the matches in face `icicle-search-main-regexp-others'."
  (setq regexp  (or regexp  (icicle-search-read-context-regexp)))
  (let ((add-bufname-p  (and buffer  icicle-show-multi-completion-flag))
        (temp-list      ())
        (last-beg       nil))
    (unless buffer (setq buffer  (current-buffer)))
    (when (bufferp buffer)              ; Do nothing if BUFFER is not a buffer.
      (with-current-buffer buffer
        (unless (and beg  end) (setq beg  (point-min)
                                     end  (point-max)))
        (icicle-condition-case-no-debug icicle-search-regexp-scan
            (save-excursion
              (goto-char (setq last-beg  beg))
              (while (and beg  (< beg end)  (not (eobp))
                          (progn (while (and (setq beg  (re-search-forward regexp end t))
                                             (eq last-beg beg)
                                             (not (eobp)))
                                   ;; Matched again, same place.  Advance 1 char.
                                   (forward-char) (setq beg  (1+ beg)))
                                 ;; Stop if no more match.  But if complementing then continue until eobp.
                                 (or beg  icicle-search-complement-domain-p)))
                (unless (or (not beg)  (match-beginning icicle-search-context-level))
                  (error "Search context has no subgroup of level %d - try a lower number"
                         icicle-search-context-level))
                (let* ((hit-beg     (if icicle-search-complement-domain-p
                                        last-beg
                                      (match-beginning icicle-search-context-level)))
                       (hit-end     (if icicle-search-complement-domain-p
                                        (if beg
                                            (match-beginning icicle-search-context-level)
                                          (point-max))
                                      (match-end icicle-search-context-level)))
                       (IGNORE      (when action
                                      (save-excursion
                                        (funcall action)
                                        (setq hit-end  (point)))))
                       (hit-string  (buffer-substring-no-properties hit-beg hit-end))
                       end-marker)
                  (when (and (not (string= "" hit-string)) ; Do nothing if empty hit.
                             (setq end-marker  (copy-marker hit-end))
                             (or (not predicate)
                                 (save-match-data (funcall predicate hit-string end-marker))))
                    (icicle-candidate-short-help
                     (concat (and add-bufname-p
                                  (format "Buffer: `%s', " (buffer-name (marker-buffer end-marker))))
                             (format "Position: %d, Length: %d"
                                     (marker-position end-marker) (length hit-string)))
                     hit-string)
                    ;; Add whole candidate to `temp-list'.  Whole candidate is
                    ;; (`hit-string' . `end-marker') or ((`hit-string' BUFNAME) . `end-marker').
                    (push (cons (if add-bufname-p
                                    (list hit-string
                                          (let ((string  (copy-sequence (buffer-name))))
                                            (put-text-property 0 (length string) 'face
                                                               'icicle-candidate-part string)
                                            string))
                                  hit-string)
                                end-marker)
                          temp-list)
                    ;; Highlight search context in buffer.
                    (when (or (eq t icicle-search-highlight-threshold)
                              (<= (+ (length temp-list) (length icicle-candidates-alist))
                                  icicle-search-highlight-threshold))
                      (let ((ov  (make-overlay hit-beg hit-end)))
                        (push ov icicle-search-overlays)
                        (overlay-put ov 'priority 200) ; > ediff's 100+, < isearch-overlay's 1001.
                        (overlay-put ov 'face 'icicle-search-main-regexp-others)))))
                (setq last-beg  beg))
              (setq icicle-candidates-alist  (append icicle-candidates-alist (nreverse temp-list))))
          (quit (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup)))
          (error (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
                 (error "%s" (error-message-string icicle-search-regexp-scan))))))))

;; Free var here: `icicle-search-ecm' is bound in `icicle-search'.
(defun icicle-search-highlight-all-input-matches (&optional input)
  "Highlight, inside each search context, what INPUT matches."
  (save-excursion
    ;; Update by deleting (if it exists) and then creating.
    ;; If a single overlay exists, it means that the user just changed
    ;; `icicle-search-highlight-threshold' to non-zero.
    ;; Otherwise, it's nil or a list of overlays.
    (when (overlayp icicle-search-refined-overlays)
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays  ()))
    (while icicle-search-refined-overlays
      (delete-overlay (car icicle-search-refined-overlays))
      (setq icicle-search-refined-overlays  (cdr icicle-search-refined-overlays))))
  (when icicle-search-highlight-all-current-flag
    (setq input  (or input  icicle-current-input))
    (unless (or (string= "" input)  (null icicle-search-overlays))
      (let ((hits  ())
            pos)
        (save-excursion
          (dolist (ov  icicle-search-overlays)
            (set-buffer (overlay-buffer ov))
            (unless (equal (overlay-start ov) (overlay-end ov))
              (save-restriction         ; Search within the current search context.
                (narrow-to-region (overlay-start ov) (overlay-end ov))
                (goto-char (point-min))
                (when (condition-case nil (re-search-forward input nil 'move-to-end) (error nil))
                  (push (buffer-substring-no-properties (point-min) (point-max)) hits)))))
          (when (and hits  (or (and (eq icicle-current-completion-mode 'apropos)
                                    (eq icicle-expand-input-to-common-match 4))
                               (and (eq icicle-current-completion-mode 'prefix)
                                    (memq icicle-expand-input-to-common-match '(3 4)))))
            (setq icicle-search-ecm  (icicle-expanded-common-match input hits))
            (when (string= "" icicle-search-ecm) (setq icicle-search-ecm  nil)))
          (when (or icicle-search-ecm  (and input  (not (string= "" input))))
            (dolist (ov  icicle-search-overlays)
              (set-buffer (overlay-buffer ov))
              (unless (equal (overlay-start ov) (overlay-end ov))
                (save-restriction       ; Search within the current search context.
                  (narrow-to-region (overlay-start ov) (overlay-end ov))
                  (when (member (buffer-substring-no-properties (point-min) (point-max)) hits)
                    (goto-char (setq pos  (point-min)))
                    (save-match-data
                      (while (and (not (eobp))  (condition-case nil
                                                    (re-search-forward (or icicle-search-ecm  input)
                                                                       nil 'move-to-end)
                                                  (error nil)))
                        (if (or (and (equal (match-beginning 0) (match-end 0))  (not (eobp)))
                                (equal (point) pos))
                            (forward-char)
                          (setq pos  (point))
                          (unless (equal (match-beginning 0) (match-end 0))
                            (setq ov  (make-overlay (match-beginning 0) (match-end 0)))
                            (push ov icicle-search-refined-overlays)
                            (overlay-put ov 'priority 220)
                            (overlay-put ov 'face 'icicle-search-current-input)))))))))))))))

(defun icicle-search-replace-search-hit (candidate) ; Bound to `C-S-return' (`icicle-search').
  "Replace search hit CANDIDATE with `icicle-search-replacement'."
  ;; NOTE: We allow side effects during replacement.
  ;; In particular, `icicle-completion-candidates', `icicle-candidate-nb', and `icicle-last-input'
  ;; can change.

  (let (;; (icicle-candidate-nb          icicle-candidate-nb)
        ;; (icicle-completion-candidates icicle-completion-candidates)
        ;; (icicle-last-input            icicle-last-input)
        (icicle-last-completion-command  icicle-last-completion-command)
        (compl-win                       (get-buffer-window "*Completions*" 0)))
    (unless (or icicle-candidate-nb  icicle-all-candidates-action)
      (error "No current candidate.  Cycle or complete to get to a candidate"))
    (unless icicle-search-replacement
      (icicle-search-define-replacement)
      (when (and compl-win  icicle-completion-candidates)
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list icicle-completion-candidates)))))
  (setq icicle-candidate-nb  (or icicle-candidate-nb  0)) ; Replace-all has nil, so use 0.
  (funcall icicle-candidate-action-fn candidate icicle-search-replacement)) ; Call with second arg.

(defun icicle-search-replace-all-search-hits (candidates) ; Bound to `M-|' (for `icicle-search').
  "Default alternative list action function for `icicle-search'.
CANDIDATES is a list of search-hit strings.  They are all matched by
the initial regexp (context regexp)."
  (let ((icicle-last-completion-command  icicle-last-completion-command)
        (compl-win                       (get-buffer-window "*Completions*" 0)))
;;; $$$$$$ These are now avoided always for all candidates, in `icicle-all-candidates-action-1'.
;;;     (icicle-minibuffer-message-ok-p  nil) ; Avoid delays from `icicle-msg-maybe-in-minibuffer'.
;;;     (icicle-help-in-mode-line-delay  0)) ; Avoid delays for individual candidate help.
    (unless icicle-search-replacement
      (icicle-search-define-replacement)
      (when (and compl-win  icicle-completion-candidates)
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list icicle-completion-candidates))))
    (dolist (cand+mrker  (mapcar icicle-get-alist-candidate-function candidates))
      (icicle-search-action-1 cand+mrker icicle-search-replacement)))
  (select-window (minibuffer-window))
  (select-frame-set-input-focus (selected-frame)))

(defun icicle-search-action (string &optional replace-string) ; Bound to `C-return' (`icicle-search').
  "Default completion action function for `icicle-search'.
STRING is a search-hit string.  It is matched by the initial regexp
\(context regexp).

1. Move to the STRING occurrence in original buffer.  Highlight it.
2. If `icicle-search-highlight-threshold' is zero, highlight what the
   current input matches, inside the STRING occurrence.
3. If REPLACE-STRING is non-nil, replace the current match with it.
   If `icicle-search-replace-whole-candidate-flag' is non-nil, replace
   the entire STRING occurrence.  Otherwise, replace only the part
   that matches the current input.
4. Highlight the current candidate in `*Completions*'.

   Note: The replacement can be nearly anything allowed as a
   replacement by `query-replace-regexp', including Lisp-evaluation
   constructs (`\,...')."
  (prog1
      (let* ((icicle-whole-candidate-as-text-prop-p  t)
             ;; Alternative: If we used `icicle-search-replace-cand-in-alist', then we would bind that
             ;; to nil to force using the alist, because we would be performing side effects on it.
             (cand+mrker  (funcall icicle-get-alist-candidate-function string)))
        (icicle-search-action-1 cand+mrker replace-string))
    (select-window (minibuffer-window))
    (select-frame-set-input-focus (selected-frame))))

;; Free vars here: `icicle-orig-win-explore' is bound in `icicle-explore'.
(defun icicle-search-action-1 (cand+mrker &optional replace-string)
  "Same as `icicle-search-action', but using full candidate, not string.
CAND+MRKER is a full alist completion-candidate entry, not just a
display string as in `icicle-search-action'."
  (when icicle-completion-candidates
    (icicle-condition-case-no-debug icicle-search-action-1
        (progn
          ;; Move cursor to the match in the original buffer and highlight it.
          (let* ((candidate                    (if (consp (car-safe cand+mrker))
                                                   (car-safe (car-safe cand+mrker))
                                                 (car-safe cand+mrker)))
                 (marker                       (cdr-safe cand+mrker))
                 (icicle-search-in-context-fn  (or icicle-search-in-context-fn
                                                   'icicle-search-in-context-default-fn)))
            (when (get-buffer-window (marker-buffer marker) 0)
              (setq icicle-other-window  (get-buffer-window (marker-buffer marker) 0)))
            (unless marker (error "No such occurrence"))
            (icicle-condition-case-no-debug icicle-search-action-1-save-window
                (save-selected-window
                  (when (window-live-p icicle-orig-win-explore)
                    (select-window icicle-orig-win-explore))
                  (let ((completion-ignore-case  case-fold-search)
                        (buf                     (marker-buffer marker)))
                    (unless (bufferp buf) (error "No such buffer: %s" buf))
                    (pop-to-buffer buf)
                    (raise-frame)
                    (goto-char marker)
                    (unless (pos-visible-in-window-p) (recenter icicle-recenter))
                    ;; Highlight current search context using `icicle-search-main-regexp-current'.
                    (icicle-place-overlay (- marker (length candidate)) marker
                                          'icicle-search-current-overlay
                                          'icicle-search-main-regexp-current
                                          202 buf)
                    (funcall icicle-search-in-context-fn cand+mrker replace-string)
                    (icicle-highlight-candidate-in-Completions)
                    (run-hooks 'icicle-search-hook)))
              (error                    ; Ignore disappearance of `*Completions*'.
               (unless (string-match "Wrong type argument: window-live-p,"
                                     (error-message-string icicle-search-action-1-save-window))
                 (error "%s" (error-message-string icicle-search-action-1-save-window)))))
            nil))                       ; Return nil for success.
      (error (message "%s" (error-message-string icicle-search-action-1))
             (error-message-string icicle-search-action-1))))) ; Return the error string.

(defun icicle-search-in-context-default-fn (cand+mrker replace-string)
  "Default value of `icicle-search-in-context-fn'."
  (let ((candidate  (if (consp (car-safe cand+mrker))
                        (car-safe (car-safe cand+mrker))
                      (car-safe cand+mrker)))
        (marker     (cdr-safe cand+mrker)))
    ;; Highlight match and possibly replace.  If replacement tried, then update the dialog state.
    (when (save-excursion (save-restriction ; Search within the current search context.
                            (narrow-to-region (- marker (length candidate)) marker)
                            (icicle-search-highlight-and-maybe-replace cand+mrker replace-string)))
      ;; Update, since replacement might have changed the current candidate:
      ;; Rehighlight current context, update last candidate, update candidate in minibuffer.
      (if icicle-search-highlight-all-current-flag
          (let ((icicle-search-highlight-all-current-flag  nil))
            (icicle-search-highlight-input-matches-here))
        (let ((ov  icicle-search-current-overlay))
          (save-restriction (narrow-to-region (overlay-start ov) (overlay-end ov))
                            (icicle-search-highlight-input-matches-here))))
      (if (null icicle-completion-candidates) ; If have already replaced all, then no candidates.
          (when (overlayp icicle-search-current-overlay)
            (delete-overlay icicle-search-current-overlay))
        (let* ((cand+mrker  (funcall icicle-get-alist-candidate-function
                                     (setq icicle-last-completion-candidate
                                           (elt icicle-completion-candidates icicle-candidate-nb))))
               (marker      (cdr-safe cand+mrker)))
          (with-current-buffer (marker-buffer marker)
            (goto-char marker)
            ;; Highlight current search context using `icicle-search-main-regexp-current'.
            (icicle-place-overlay (- marker (if (consp (car cand+mrker))
                                                (length (caar cand+mrker))
                                              (length (car cand+mrker))))
                                  marker 'icicle-search-current-overlay
                                  'icicle-search-main-regexp-current 202 (current-buffer))
            (unless icicle-search-highlight-all-current-flag
              (let ((ov  icicle-search-current-overlay))
                (save-restriction (narrow-to-region (overlay-start ov) (overlay-end ov))
                                  (icicle-search-highlight-input-matches-here)))))
          (save-selected-window
            (select-window (minibuffer-window))
            (icicle-clear-minibuffer)
            (setq icicle-nb-of-other-cycle-candidates  (length icicle-completion-candidates))
            (icicle-insert-cand-in-minibuffer icicle-last-completion-candidate t)
            (icicle-show-help-in-mode-line icicle-last-completion-candidate))))))
  (let ((icicle-candidate-nb               icicle-candidate-nb)
        (icicle-last-completion-candidate  icicle-last-completion-candidate)
        (icicle-completion-candidates      icicle-completion-candidates))
    (icicle-complete-again-update)))

;; Free var here: `icicle-search-ecm' is bound in `icicle-search'.
(defun icicle-search-highlight-and-maybe-replace (cand+mrker replace-string)
  "Highlight within search context and replace using REPLACE-STRING.
If REPLACE-STRING is nil, no replacement occurs.
Arguments are the same as for `icicle-search-in-context-fn'.
Return non-nil if replacement occurred, nil otherwise."
  (icicle-search-highlight-context-levels)
  (icicle-search-highlight-input-matches-here)
  (let ((replacement-p  nil))
    (when replace-string
      (setq replacement-p  t)
      (goto-char (point-min))
      (let ((candidate  (if (consp (car-safe cand+mrker))
                            (car-safe (car-safe cand+mrker))
                          (car-safe cand+mrker)))
            (ecm        (and icicle-search-replace-common-match-flag  icicle-search-ecm)))
        (cond (icicle-search-replace-whole-candidate-flag
               (cond ((string= candidate replace-string) ; Sanity check only.
                      (save-restriction (widen) (message "Replacement = candidate, and \
current input matches candidate") (sit-for 2))
                      (setq replacement-p  nil))
                     (t
                      (set-match-data (list (point-min) (point-max)))
                      (icicle-search-replace-match replace-string
                                                   (icicle-search-replace-fixed-case-p
                                                    icicle-search-context-regexp)))))
              ((not (save-excursion (re-search-forward (or ecm  icicle-current-input) nil t)))
               (save-restriction (widen)
                                 (message "Text to be replaced not found in candidate") (sit-for 2))
               (setq replacement-p  nil))
              (t
               (save-match-data
                 (let ((first-p  t))
                   ;; The condition order is important.  Don't search unless first time (or all)
                   (while (and (or first-p  icicle-all-candidates-action)
                               (re-search-forward (or ecm  icicle-current-input) nil 'move-to-end))
                     (setq first-p  nil)
                     (icicle-search-replace-match replace-string
                                                  (icicle-search-replace-fixed-case-p
                                                   icicle-current-input)))))))
        (when replacement-p
          ;; Update the alist and `minibuffer-completion-table' with the new text.

          ;; An ALTERNATIVE approach would be to use `icicle-search-replace-cand-in-alist'.
          ;; In that case we would:
          ;; 1. Bind `icicle-whole-candidate-as-text-prop-p' to nil (in `icicle-search-action'
          ;;    and `icicle-search-help').
          ;; 2. Use these two lines, instead of calling `icicle-search-replace-cand-in-mct'.
          ;;    (icicle-search-replace-cand-in-alist cand+mrker
          ;;                                         (buffer-substring (point-min) (point-max)))
          ;;    (setq minibuffer-completion-table
          ;;          (car (icicle-mctize-all icicle-candidates-alist nil)))
          ;;  If we used that method (as we used to), then users could not sort the search hits.

          (icicle-search-replace-cand-in-mct cand+mrker (buffer-substring (point-min) (point-max)))

          ;; If we are replacing input matches within a search context, and there are no more matches
          ;; in the current context, then this context is removed as a candidate. If the current
          ;; action command is one that moves to the next or previous candidate, then we might need
          ;; to adjust the current candidate number, to compensate for the removal.
          ;;
          ;; If the current action command is one (e.g. `C-S-next'), that moves to the next candidate
          ;; to do its action, then move back one.  If the current action acts on the previous
          ;; candidate (e.g. `C-S-prior'), and that previous candidate is the last one, then move
          ;; forward one candidate, to the first.
          (when (and icicle-acting-on-next/prev
                     (not (save-excursion (goto-char (point-min))
                                          (re-search-forward icicle-current-input nil t))))
            (let ((nb-cands  (1- (length icicle-completion-candidates)))) ; -1 for replaced one.
              (unless (wholenump nb-cands) (setq nb-cands  0))
              (setq icicle-candidate-nb  (cond ((not icicle-candidate-nb) 0)
                                               ((eq icicle-acting-on-next/prev 'forward)
                                                (if (zerop icicle-candidate-nb)
                                                    (1- nb-cands)
                                                  (1- icicle-candidate-nb)))
                                               ((eq icicle-candidate-nb nb-cands)  0)
                                               (t icicle-candidate-nb)))
              (when (> icicle-candidate-nb nb-cands) (setq icicle-candidate-nb  0))
              (when (< icicle-candidate-nb 0) (setq icicle-candidate-nb  nb-cands))))

          (let ((icicle-candidate-nb             icicle-candidate-nb)
                (icicle-minibuffer-message-ok-p  nil)) ; Inhibit no-candidates message.
            (icicle-complete-again-update))

          ;; If we are using `C-S-RET' and we are on the last candidate, then wrap to the first one.
          (when (and (not icicle-acting-on-next/prev)
                     (or (not icicle-candidate-nb)  (>= icicle-candidate-nb
                                                        (length icicle-completion-candidates))))
            (setq icicle-candidate-nb  0))
          (icicle-highlight-candidate-in-Completions)
          (icicle-search-highlight-context-levels))))
    replacement-p))                     ; Return indication of whether we tried to replace something.

(defun icicle-search-replace-match (replace-string fixedcase)
  "Replace current match with REPLACE-STRING, interpreting escapes.
Treat REPLACE-STRING as it would be treated by `query-replace-regexp'.
FIXEDCASE is as for `replace-match'.  Non-nil means do not alter case."
  (if (fboundp 'query-replace-compile-replacement) ; Emacs 22.
      (let ((compiled
             (save-match-data
               (query-replace-compile-replacement replace-string
                                                  (not icicle-search-replace-literally-flag)))))
        (condition-case icicle-search-replace-match1
            (let ((enable-recursive-minibuffers    t) ; So we can read input from \?.
                  ;; Save and restore these, because we might read input from \?.
                  (icicle-last-completion-command  icicle-last-completion-command)
                  (icicle-last-input               icicle-last-input))
              (replace-match-maybe-edit
               (if (consp compiled)
                   ;; `replace-count' is free here, bound in `icicle-search'.
                   (funcall (car compiled) (cdr compiled) (setq replace-count  (1+ replace-count)))
                 compiled)
               fixedcase icicle-search-replace-literally-flag nil (match-data)))
          (buffer-read-only (ding) (error "Buffer is read-only"))
          (error (icicle-remove-Completions-window) (error "No match for `%s'" replace-string))))
    (condition-case icicle-search-replace-match2 ; Emacs < 22.  Try to interpret `\'.
        (replace-match replace-string fixedcase icicle-search-replace-literally-flag)
      (error (replace-match replace-string fixedcase t))))) ;   If error, replace literally.

(defun icicle-search-highlight-context-levels ()
  "Highlight context levels differently (up to 8 levels).
No such highlighting is done if any of these conditions holds:
 * `icicle-search-context-level' is not 0 (search context < regexp).
 * `icicle-search-highlight-context-levels-flag' is nil.
 * `icicle-search-context-regexp' is nil (non-regexp searching)."
  (unless (or (/= icicle-search-context-level 0)
              (not icicle-search-highlight-context-levels-flag)
              (not icicle-search-context-regexp)) ; E.g. text-property searching
    (while icicle-search-level-overlays
      (delete-overlay (car icicle-search-level-overlays))
      (setq icicle-search-level-overlays  (cdr icicle-search-level-overlays)))
    (save-match-data
      (let ((level       1)
            (max-levels  (min (regexp-opt-depth icicle-search-context-regexp) 8)))
        (goto-char (point-min))
        (re-search-forward icicle-search-context-regexp nil t)
        (condition-case nil
            (while (<= level max-levels)
              (unless (equal (match-beginning level) (match-end level))
                (let ((ov  (make-overlay (match-beginning level) (match-end level))))
                  (push ov icicle-search-level-overlays)
                  (overlay-put ov 'priority (+ 205 level)) ; > ediff's 100+, < isearch-overlay's 1001.
                  (overlay-put ov 'face (intern (concat "icicle-search-context-level-"
                                                        (number-to-string level))))))
              (setq level  (1+ level)))
          (error nil))))))

;; Free var here: `icicle-search-ecm' is bound in `icicle-search'.
(defun icicle-search-highlight-input-matches-here ()
  "Highlight all input matches in the current search context."
  (unless (or (eq t icicle-search-highlight-threshold)
              (> 0 icicle-search-highlight-threshold)
              (string= "" icicle-current-input))
    (goto-char (point-min))
    (when (and (not icicle-search-highlight-all-current-flag)  (overlayp icicle-search-refined-overlays))
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays  ()))
    (unless icicle-search-highlight-all-current-flag
      (while icicle-search-refined-overlays
        (delete-overlay (car icicle-search-refined-overlays))
        (setq icicle-search-refined-overlays  (cdr icicle-search-refined-overlays))))
    (let ((ov  nil))
      (save-match-data
        (while (and (not (eobp))  (re-search-forward (or icicle-search-ecm  icicle-current-input)
                                                     nil 'move-to-end))
          (if (equal (match-beginning 0) (match-end 0))
              (forward-char 1)
            (setq ov  (make-overlay (match-beginning 0) (match-end 0)))
            (push ov icicle-search-refined-overlays)
            (overlay-put ov 'priority 220) ; Greater than any possible context-level priority (213).
            (overlay-put ov 'face 'icicle-search-current-input)))))))

(defun icicle-search-replace-fixed-case-p (from)
  "Return non-nil if FROM should be replaced without transferring case.
FROM is a string or nil.  If FROM is nil, then return nil.
Retuns non-nil if FROM is a string and one of the following holds:
 * FROM is not all lowercase
 * `case-replace' or `case-fold-search' is nil"
  (and from  (not (and case-fold-search  case-replace  (string= from (downcase from))))))

;; Not used for now - this could replace using mct.  In that case, user must not be able to sort.
(defun icicle-search-replace-cand-in-alist (cand+mrker new-cand)
  "In `icicle-candidates-alist', replace car of CAND+MRKER by NEW-CAND.
Replace only the first occurrence of CAND+MRKER in
`icicle-candidates-alist'.  (There should be only one.)"
  (let ((newlist  icicle-candidates-alist))
    (catch 'icicle-search-replace-cand-in-alist
      (while newlist
        (when (equal (car newlist) cand+mrker)
          (setcar newlist (cons new-cand (cdr-safe cand+mrker)))
          (throw 'icicle-search-replace-cand-in-alist nil))
        (setq newlist  (cdr newlist))))
    icicle-candidates-alist))

(defun icicle-search-replace-cand-in-mct (cand+mrker new-cand)
  "Replace candidate in `minibuffer-completion-table'.
Update CAND+MRKER itself to use NEW-CAND (replacement string).
Any text properties on CAND+MRKER's string are preserved.
Use this only with a `minibuffer-completion-table' derived from an alist."
  (let ((newlist  minibuffer-completion-table))
    (catch 'icicle-search-replace-cand-in-mct
      ;; CAND+MRKER: ("aa" . c) or (("aa" "bb") . c)
      ;; `minibuffer-completion-table' entry: ("aa" "aa" . c) or ("aa^G^Jbb" . (("aa" "bb") . c))
      (while newlist
        (when (equal (cdr (car newlist)) cand+mrker)
          (let ((new-compl  (if (consp (car cand+mrker)) ; New completion: "QQ" or ("QQ" "bb")
                                (cons new-cand (cdar cand+mrker))
                              new-cand))
                (old-cand   (if (consp (car cand+mrker)) (caar cand+mrker) (car cand+mrker)))
                rep-cand)
            (setcar newlist (icicle-mctized-full-candidate (cons new-compl (cdr-safe cand+mrker))))
            ;; NEWLIST is done.
            ;; Now update CAND+MRKER to reflect the replacement but with the text properties it had.
            ;; (cdar NEWLIST) is the new cand+mrker.  Its car or caar is the replaced candidate.
            ;; It is the first field of the multi-completion, in the latter case.
            (setq rep-cand  (if (consp (car cand+mrker)) (caar (cdar newlist)) (car (cdar newlist))))
            (let ((len-old  (length old-cand))
                  (len-rep  (length rep-cand))
                  (ii       0)
                  props)
              (while (< ii len-old)
                (setq props  (text-properties-at ii old-cand))
                (when (< ii len-rep) (add-text-properties ii (1+ ii) props rep-cand))
                (setq ii  (1+ ii)))
              (let ((last-props  (text-properties-at (1- len-old) old-cand)))
                (when (> len-rep len-old)
                  (add-text-properties len-old len-rep last-props rep-cand))))
            (if (consp (car cand+mrker))
                (setcar (car cand+mrker) rep-cand)
              (setcar cand+mrker rep-cand)))
          (throw 'icicle-search-replace-cand-in-mct nil))
        (setq newlist  (cdr newlist))))
    minibuffer-completion-table))

(defun icicle-search-help (cand)
  "Use as `icicle-candidate-help-fn' for `icicle-search' commands."
  (icicle-msg-maybe-in-minibuffer
   (let* ((icicle-whole-candidate-as-text-prop-p  t)
          ;; Alternative: If we used `icicle-search-replace-cand-in-alist', then we would bind that
          ;; to nil to force using the alist, because we would be performing side effects on it.
          (marker  (cdr (funcall icicle-get-alist-candidate-function cand))))
     (concat "Buffer: `" (buffer-name (marker-buffer marker))
             (format "', Position: %d" (marker-position marker))))))

;;;###autoload (autoload 'icicle-search-keywords "icicles")
(defun icicle-search-keywords (beg end keywords require-match ; Bound to `M-s M-s k', `C-c ^'.
                               &optional where &rest args)
  "Search with one or more keywords, which can each be a regexp.
Text that matches *any* of the keywords is found.

You can use completion to choose one or more previously entered
regexps (using `C-RET', `C-mouse-2', `C-next', and so on), or you can
enter new keywords (using `C-RET').  Use `RET' or `mouse-2' to choose
the last keyword.

Keywords are interpreted as regexps.  You can change to substring
completion instead, matching regexp special characters literally, by
using `C-`' during completion to toggle `icicle-regexp-quote-flag'.

You can alternatively choose to search, not the search contexts as
defined by keyword matches, but the non-contexts, that is, the text in
the buffer that does not match the keyword patterns.  To do this, use
`C-M-~' during completion.  (This is a toggle, and it affects only
future search commands, not the current one.)

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the `icicle-search'
documentation."
  (interactive
   `(,@(icicle-region-or-buffer-limits)
       ,(icicle-group-regexp (mapconcat #'icicle-group-regexp (icicle-keyword-list) "\\|"))
       ,(not icicle-show-multi-completion-flag)
       ,(icicle-search-where-arg)))
  (icicle-search beg end keywords (not icicle-show-multi-completion-flag) where))

(defun icicle-group-regexp (regexp)
  "Wrap REGEXP between regexp parens, as a regexp group."
  (concat "\\(" regexp "\\)"))

;;;###autoload (autoload 'icicle-search-bookmark "icicles")
(icicle-define-command icicle-search-bookmark ; Bound to `M-s M-s j'.
  "Search bookmarked text.
See also `icicle-search-bookmarks-together', which searches bookmarks
together instead of one at a time.

1. Enter a context regexp (using `RET'), to define the possible
   search-hit contexts.
2. Choose a bookmark using completion.  It is opened/visited/handled.
3. (Optional) Type some text to be matched in the search contexts.
4. Navigate to matches (search hits) using `C-next' etc.
5. Finish with that bookmark using `RET' (stay) or `C-g' (skip).
6. (Optional) Repeat steps 2-5 for other bookmarks.

If you use library `Bookmark+' then:

a. If a bookmark specifies a nonempty region, then search only the text
  in that region.  Otherwise, search the whole bookmarked buffer/file.

b. The candidate bookmarks are those in the current `*Bookmark List*'
  display (list `bmkp-sorted-alist', to be precise).  This means that
  you can limit the candidates to bookmarks of a certain type (e.g.,
  only autofiles, using `A S'), bookmarks with certain tags (e.g.,
  those with tags matching a regexp using `T m %' followed by `>'),
  and so on.

\(b) provides you with a great deal of flexibility.  However, for your
convenience, if you use `Bookmark+' then Icicles also provides some
special-purpose commands for searching the content of bookmarks of
various types.  For example, `icicle-search-autofile-bookmark'
searches autofiles.  And you can define your own such commands using
macro `icicle-define-search-bookmark-command'.

You can alternatively choose to search, not the search contexts as
defined by the context regexp, but the non-contexts, that is, the text
in the bookmarked buffer that does not match the regexp.  To do this,
use `C-M-~' during completion.  (This is a toggle, and it affects only
future search commands, not the current one.)" ; Doc string
  icicle-search-bookmark-action         ; Action function
  prompt icicle-candidates-alist nil (not icicle-show-multi-completion-flag) ; `completing-read' args
  nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
  (and (boundp 'bookmark-current-bookmark)  bookmark-current-bookmark) nil
  ((enable-recursive-minibuffers             t) ; In case we read input, e.g. File changed on disk...
   (completion-ignore-case                   bookmark-completion-ignore-case)
   (prompt                                   "Search bookmark: ")
   (icicle-search-context-regexp             (icicle-search-read-context-regexp))
   (bookmark-automatically-show-annotations  nil) ; Do not show annotations
   (icicle-multi-completing-p                icicle-show-multi-completion-flag)
   (icicle-list-use-nth-parts                '(1))
   (icicle-candidate-properties-alist        (if (not icicle-show-multi-completion-flag)
                                                 ()
                                               (if (facep 'file-name-shadow)
                                                   '((2 (face file-name-shadow))
                                                     (3 (face bookmark-menu-heading)))
                                                 '((3 (face bookmark-menu-heading))))))
   (icicle-transform-function                (if (interactive-p) nil icicle-transform-function))
   (icicle-whole-candidate-as-text-prop-p    t)
   (icicle-transform-before-sort-p           t)
   (icicle-delete-candidate-object           'icicle-bookmark-delete-action)
   (icicle-candidates-alist
    (if (not (featurep 'bookmark+))
        (mapcar (lambda (cand)
                  (list (icicle-candidate-short-help (icicle-bookmark-help-string cand)
                                                     (icicle-bookmark-propertize-candidate cand))))
                (bookmark-all-names))   ; Loads bookmarks file, defining `bookmark-alist'.
      (bookmark-maybe-load-default-file) ; Loads bookmarks file, defining `bookmark-alist'.
      (mapcar #'icicle-make-bookmark-candidate
              (or (and (or (and (not icicle-bookmark-refresh-cache-flag)  (not (consp current-prefix-arg)))
                           (and      icicle-bookmark-refresh-cache-flag        (consp current-prefix-arg)))
                       bmkp-sorted-alist)
                  (setq bmkp-sorted-alist  (bmkp-sort-omit bookmark-alist))))))
   (icicle-sort-orders-alist
    (append '(("in *Bookmark List* order") ; Renamed from "turned OFF'.
              ("by bookmark name" . icicle-alpha-p))
            (and (featurep 'bookmark+)
                 '(("by last bookmark access" (bmkp-bookmark-last-access-cp) icicle-alpha-p)
                   ("by bookmark visit frequency" (bmkp-visited-more-cp) icicle-alpha-p)
                   ("by last buffer or file access" (bmkp-buffer-last-access-cp
                                                     bmkp-local-file-accessed-more-recently-cp)
                    icicle-alpha-p)
                   ("marked before unmarked (in *Bookmark List*)" (bmkp-marked-cp)
                    icicle-alpha-p)
                   ("by local file type" (bmkp-local-file-type-cp) icicle-alpha-p)
                   ("by file name" (bmkp-file-alpha-cp) icicle-alpha-p)
                   ("by local file size" (bmkp-local-file-size-cp) icicle-alpha-p)
                   ("by last local file access" (bmkp-local-file-accessed-more-recently-cp)
                    icicle-alpha-p)
                   ("by last local file update" (bmkp-local-file-updated-more-recently-cp)
                    icicle-alpha-p)
                   ("by Info location" (bmkp-info-cp) icicle-alpha-p)
                   ("by Gnus thread" (bmkp-gnus-cp) icicle-alpha-p)
                   ("by URL" (bmkp-url-cp) icicle-alpha-p)
                   ("by bookmark type" (bmkp-info-cp bmkp-url-cp bmkp-gnus-cp
                                        bmkp-local-file-type-cp bmkp-handler-cp)
                    icicle-alpha-p)))
            '(("by previous use alphabetically" . icicle-historical-alphabetic-p)
              ("case insensitive" . icicle-case-insensitive-string-less-p))))
   (icicle-candidate-help-fn
    (lambda (cand)
      (when (and (featurep 'bookmark+)  icicle-show-multi-completion-flag)
        (setq cand  (funcall icicle-get-alist-candidate-function cand))
        (setq cand  (cons (caar cand) (cdr cand))))
      (if (featurep 'bookmark+)
          (if current-prefix-arg
              (bmkp-describe-bookmark-internals cand)
            (bmkp-describe-bookmark cand))
        (icicle-msg-maybe-in-minibuffer (icicle-bookmark-help-string cand))))))
  (progn                                ; First code
    (require 'bookmark)
    (when (featurep 'bookmark+)
      ;; Bind keys to narrow bookmark candidates by type.  Lax is for multi-completion case.
      (dolist (map  '(minibuffer-local-must-match-map minibuffer-local-completion-map))
        (define-key (symbol-value map) (icicle-kbd "C-M-b") 'icicle-bookmark-non-file-narrow) ; `C-M-b'
        (define-key (symbol-value map) (icicle-kbd "C-M-d") 'icicle-bookmark-dired-narrow) ; `C-M-d'
        (define-key (symbol-value map) (icicle-kbd "C-M-f") 'icicle-bookmark-file-narrow) ; `C-M-f'
        (define-key (symbol-value map) (icicle-kbd "C-M-g") 'icicle-bookmark-gnus-narrow) ; `C-M-g'
        (define-key (symbol-value map) (icicle-kbd "C-M-i") 'icicle-bookmark-info-narrow) ; `C-M-i'
        (define-key (symbol-value map) (icicle-kbd "C-M-m") 'icicle-bookmark-man-narrow) ; `C-M-m'
        (define-key (symbol-value map) (icicle-kbd "C-M-r") 'icicle-bookmark-region-narrow) ; `C-M-r'
        (define-key (symbol-value map) (icicle-kbd "C-M-u") 'icicle-bookmark-url-narrow) ; `C-M-u'
        (define-key (symbol-value map) (icicle-kbd "C-M-w") 'icicle-bookmark-w3m-narrow) ; `C-M-w'
        (define-key (symbol-value map) (icicle-kbd "C-M-@") 'icicle-bookmark-remote-file-narrow) ; `C-M-@'
        (define-key (symbol-value map) (icicle-kbd "C-M-B") ; `C-M-B'
          'icicle-bookmark-bookmark-list-narrow)
        (define-key (symbol-value map) (icicle-kbd "C-M-F") ; `C-M-F'
          'icicle-bookmark-local-file-narrow)
        (define-key (symbol-value map) (icicle-kbd "C-M-K") ; `C-M-K'
          'icicle-bookmark-desktop-narrow))))
  (icicle-bookmark-cleanup-on-quit)     ; Undo code
  (icicle-bookmark-cleanup))            ; Last code

(defun icicle-search-bookmark-action (bookmark-name)
  "Action function for `icicle-search-bookmark'."
  (setq bookmark-name  (icicle-transform-multi-completion bookmark-name))
  (bookmark-jump-other-window bookmark-name)
  (setq mark-active  nil)               ; Unhighlight region, so you can see search hits etc.
  (let ((icicle-show-Completions-initially-flag  t)
        (icicle-candidate-action-fn              'icicle-search-action)
        (enable-recursive-minibuffers            t)
        (beg
         (or (and (featurep 'bookmark+)  (bmkp-region-bookmark-p bookmark-name)
                  (bookmark-get-position bookmark-name))
             (point-min)))
        (end
         (or (and (featurep 'bookmark+)  (bmkp-region-bookmark-p bookmark-name)
                  (bmkp-get-end-position bookmark-name))
             (point-max))))
    (when (= beg end) (setq beg  (point-min)    end  (point-max)))
    (icicle-search beg end icicle-search-context-regexp t))
  (with-current-buffer (window-buffer (minibuffer-window)) (icicle-erase-minibuffer)))


;; The following sexps macro-expand to define these commands:
;;  `icicle-search-all-tags-bookmark'
;;  `icicle-search-all-tags-regexp-bookmark'
;;  `icicle-search-autofile-bookmark'
;;  `icicle-search-autonamed-bookmark'
;;  `icicle-search-bookmark-list-bookmark'
;;  `icicle-search-dired-bookmark'
;;  `icicle-search-file-bookmark'
;;  `icicle-search-gnus-bookmark'
;;  `icicle-search-info-bookmark'
;;  `icicle-search-local-file-bookmark'
;;  `icicle-search-man-bookmark'
;;  `icicle-search-non-file-bookmark'
;;  `icicle-search-region-bookmark'
;;  `icicle-search-remote-file-bookmark'
;;  `icicle-search-some-tags-bookmark'
;;  `icicle-search-some-tags-regexp-bookmark'
;;  `icicle-search-specific-buffers-bookmark'
;;  `icicle-search-specific-files-bookmark'
;;  `icicle-search-temporary-bookmark'
;;  `icicle-search-this-buffer-bookmark'
;;  `icicle-search-url-bookmark'
;;  `icicle-search-w3m-bookmark'

(icicle-define-search-bookmark-command "all-tags" nil (bmkp-read-tags-completing))
(icicle-define-search-bookmark-command "all-tags-regexp" nil (bmkp-read-tags-completing))
(icicle-define-search-bookmark-command "autofile")
(icicle-define-search-bookmark-command "autonamed")
(icicle-define-search-bookmark-command "bookmark-list")
(icicle-define-search-bookmark-command "dired")
(icicle-define-search-bookmark-command "file")
(icicle-define-search-bookmark-command "gnus")
(icicle-define-search-bookmark-command "info")
(icicle-define-search-bookmark-command "local-file")
(icicle-define-search-bookmark-command "man")
(icicle-define-search-bookmark-command "non-file")
(icicle-define-search-bookmark-command "region" "Search region: ")
(icicle-define-search-bookmark-command "remote-file")
(icicle-define-search-bookmark-command "some-tags" nil (bmkp-read-tags-completing))
(icicle-define-search-bookmark-command "some-tags-regexp" nil (bmkp-read-tags-completing))
(icicle-define-search-bookmark-command "specific-buffers" nil (icicle-bookmarked-buffer-list))
(icicle-define-search-bookmark-command "specific-files" nil (icicle-bookmarked-file-list))
(icicle-define-search-bookmark-command "temporary")
(icicle-define-search-bookmark-command "this-buffer")
(icicle-define-search-bookmark-command "url")
(icicle-define-search-bookmark-command "w3m")

;;; Same as `thgcmd-last-thing-type' in `thing-cmds.el'.
(defvar icicle-last-thing-type (if (boundp 'thgcmd-last-thing-type) thgcmd-last-thing-type 'sexp)
  "Type of thing last used by `icicle-next-visible-thing' (or previous).")

;;;###autoload (autoload 'icicle-search-thing "icicles")
(defun icicle-search-thing (thing &optional beg end require-match where ; Bound to `M-s M-s t'.
                            predicate transform-fn)
  "`icicle-search' with THINGs as search contexts.
Enter the type of THING to search: `sexp', `sentence', `list',
`string', `comment', etc.

Possible THINGs are those for which `icicle-bounds-of-thing-at-point'
returns non-nil (and for which the bounds are not equal: an empty
thing).  This does not include everything THING that is defined as a
thing-at-point type.

You can search the region, buffer, multiple buffers, or multiple
files.  See `icicle-search' for a full explanation.

If user option `icicle-ignore-comments-flag' is nil then include
THINGs located within comments.  Non-nil means to ignore comments for
searching.  You can toggle this option using `C-M-;' in the
minibuffer, but depending on when you do so you might need to invoke
this command again.

Non-interactively, if optional arg PREDICATE is non-nil then it is a
predicate that acceptable things must satisfy.  It is passed the thing
in the form of the cons returned by
`icicle-next-visible-thing-and-bounds'.

Non-interactively, if optional arg TRANSFORM-FN is non-nil then it is
a function to apply to each thing plus its bounds and which returns
the actual search-target to push to `icicle-candidates-alist' in place
of THING.  Its argument is the same as PREDICATE's.  It returns the
replacement for the thing plus its bounds, in the same form: a
cons (STRING START . END), where STRING is the search hit string and
START and END are its bounds).

You can alternatively choose to search, not the THINGs as search
contexts, but the non-THINGs (non-contexts), that is, the buffer text
that is outside THINGs.  To do this, use `C-M-~' during completion.
\(This is a toggle, and it affects only future search commands, not
the current one.)

This command is intended only for use in Icicle mode.

NOTE:

1. For best results, use also library `thingatpt+.el'.
2. In some cases it can take a while to gather the candidate THINGs.
   Use the command on an active region when you do not need to search
   THINGS throughout an entire buffer.
3. In `nxml-mode', remember that option `nxml-sexp-element-flag'
   controls what a `sexp' means.  To use whole XML elements as search
   contexts, set it to t, not nil.  (This is already done for the
   predefined Icicles XML search commands.)
4. Remember that if there is only one THING in the buffer or active
   region then no search is done.  Icicles search does nothing when
   there is only one possible search hit.
5. The scan candidate things moves forward a THING at a time.  In
   particular, if either PREDICATE or TRANSFORM-FN disqualifies the
   thing being scanned currently, then scanning skips forward to the
   next thing.  The scan does not dig inside the current thing to look
   for a qualified THING.
6. In some Emacs releases, especially prior to Emacs 23, the
   thing-at-point functions can sometimes behave incorrectly.  Thus,
   `icicle-search-thing' also behaves incorrectly in such cases.
7. Prior to Emacs 21 there is no possibility of ignoring comments."
  (interactive (icicle-search-thing-args))
  (setq icicle-search-context-level  0)
  (icicle-search beg end 'icicle-search-thing-scan require-match where thing predicate transform-fn))

(defun icicle-search-thing-args ()
  "Read and return interactive arguments for `icicle-search-thing'."
  (let* ((where    (icicle-search-where-arg))
         (beg+end  (icicle-region-or-buffer-limits))
         (beg1     (car beg+end))
         (end1     (cadr beg+end))
         (thing    (intern
                    (completing-read
                     (format "%shing (type): " (if icicle-search-complement-domain-p "*NOT* t" "T"))
                     (icicle-things-alist) nil nil nil nil (symbol-name icicle-last-thing-type)))))
    (when (and (eq thing 'comment)  icicle-ignore-comments-flag)
      (message "Use `%s' if you do not want to ignore comments"
               (icicle-propertize "C-M-;" 'face 'icicle-msg-emphasis))
      (sit-for 2))
    `(,thing ,beg1 ,end1 ,(not icicle-show-multi-completion-flag) ,where)))

;;; Same as `thgcmd-things-alist' in `thing-cmds.el'.
(defun icicle-things-alist ()
  "Alist of most thing types currently defined.
Each is a cons (STRING), where STRING names a type of text entity for
which there is a either a corresponding `forward-'thing operation, or
corresponding `beginning-of-'thing and `end-of-'thing operations.  The
list includes the names of the symbols that satisfy
`icicle-defined-thing-p', but with these excluded: `thing', `buffer',
`point'."
  (let ((types  ()))
    (mapatoms
     (lambda (tt)
       (when (icicle-defined-thing-p tt) (push (symbol-name tt) types))))
    (dolist (typ  '("thing" "buffer" "point")) ; Remove types that do not make sense.
      (setq types (delete typ types)))
    (setq types  (sort types #'string-lessp))
    (mapcar #'list types)))

;;; Same as `thgcmd-defined-thing-p' in `thing-cmds.el'.
(defun icicle-defined-thing-p (thing)
  "Return non-nil if THING (type) is defined as a thing-at-point type."
  (let ((forward-op    (or (get thing 'forward-op)  (intern-soft (format "forward-%s" thing))))
        (beginning-op  (get thing 'beginning-op))
        (end-op        (get thing 'end-op))
        (bounds-fn     (get thing 'bounds-of-thing-at-point))
        (thing-fn      (get thing 'thing-at-point)))
    (or (functionp forward-op)
        (and (functionp beginning-op)  (functionp end-op))
        (functionp bounds-fn)
        (functionp thing-fn))))

;; Same as `hide/show-comments' in `hide-comnt.el'.
;;
;;;###autoload (autoload 'icicle-hide/show-comments "icicles")
(defun icicle-hide/show-comments (&optional hide/show start end)
  "Hide or show comments from START to END.
Interactively, hide comments, or show them if you use a prefix arg.
Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

Uses `save-excursion', restoring point.

Be aware that using this command to show invisible text shows *all*
such text, regardless of how it was hidden.  IOW, it does not just
show invisible text that you previously hid using this command.

From Lisp, a HIDE/SHOW value of `hide' hides comments.  Other values
show them.

This function does nothing in Emacs versions prior to Emacs 21,
because it needs `comment-search-forward'."
  (interactive `(,(if current-prefix-arg 'show 'hide) ,@(icicle-region-or-buffer-limits)))
  (when (require 'newcomment nil t)     ; `comment-search-forward'
    (comment-normalize-vars)            ; Per Stefan, should call this first.
    (unless start (setq start  (point-min)))
    (unless end   (setq end    (point-max)))
    (unless (<= start end) (setq start  (prog1 end (setq end  start))))
    (let ((bufmodp           (buffer-modified-p))
          (buffer-read-only  nil)
          cbeg cend)
      (unwind-protect
           (save-excursion
             (goto-char start)
             (while (and (< start end)  (setq cbeg  (comment-search-forward end 'NOERROR)))
               (setq cend  (if (string= "" comment-end)
                               (min (1+ (line-end-position)) (point-max))
                             (search-forward comment-end end 'NOERROR)))
               (when (and cbeg  cend)
                 (if (eq 'hide hide/show)
                     (put-text-property cbeg cend 'invisible t)
                   (put-text-property cbeg cend 'invisible nil)))))
        (set-buffer-modified-p bufmodp)))))

;;; Same as `with-comments-hidden' in `hide-comnt.el', except doc here mentions `C-M-;'.
(defmacro icicle-with-comments-hidden (start end &rest body)
  "Evaluate the forms in BODY while comments are hidden from START to END.
But if `icicle-ignore-comments-flag' is nil, just evaluate BODY,
without hiding comments.  Show comments again when BODY is finished.
You can toggle `icicle-ignore-comments-flag' using `C-M-;' in the
minibuffer, but depending on when you do so you might need to invoke
the current command again.

See `icicle-hide/show-comments', which is used to hide and show the
comments.  Note that prior to Emacs 21, this never hides comments."
  (let ((result  (make-symbol "result"))
        (ostart  (make-symbol "ostart"))
        (oend    (make-symbol "oend")))
    `(let ((,ostart  ,start)
           (,oend    ,end)
           ,result)
      (unwind-protect
           (setq ,result  (progn (when icicle-ignore-comments-flag
                                   (icicle-hide/show-comments 'hide ,ostart ,oend))
                                 ,@body))
        (when icicle-ignore-comments-flag (icicle-hide/show-comments 'show ,ostart ,oend))
        ,result))))

(defun icicle-search-thing-scan (buffer beg end thing &optional predicate transform-fn)
  "Scan BUFFER from BEG to END for things of type THING.
Push the things found onto `icicle-candidates-alist'.
If BUFFER is nil, scan the current buffer.
Highlight the matches using face `icicle-search-main-regexp-others'.
If BEG and END are nil, scan entire BUFFER.

If PREDICATE is non-nil then it is a predicate that acceptable things
must satisfy.  It is passed the thing plus its bounds, in the form of
the cons returned by `icicle-next-visible-thing-and-bounds'.

If TRANSFORM-FN is non-nil then it is a function to apply to each
thing plus its bounds.  Its argument is the same as PREDICATE's.  It
returns the actual search-target to push to `icicle-candidates-alist'
in place of THING.  That is, it returns the replacement for the thing
plus its bounds, in the same form: a cons (STRING START . END), where
STRING is the search hit string and START and END are its bounds).  It
can also return nil, in which case it acts as another predicate: the
thing is not included as a candidate.

NOTE: The scan moves forward a THING at a time.  In particular, if
either PREDICATE or TRANSFORM-FN disqualifies the thing being scanned
currently, then scanning skips forward to the next thing.  The scan
does not dig inside the current thing to look for a qualified THING.

This function respects both `icicle-search-complement-domain-p' and
`icicle-ignore-comments-flag'."
  (let ((add-bufname-p  (and buffer  icicle-show-multi-completion-flag))
        (temp-list      ())
        (last-beg       nil))
    (unless buffer (setq buffer  (current-buffer)))
    (when (bufferp buffer)              ; Do nothing if BUFFER is not a buffer.
      (with-current-buffer buffer
        (unless beg (setq beg  (point-min)))
        (unless end (setq end  (point-max)))
        (unless (< beg end) (setq beg  (prog1 end (setq end  beg)))) ; Ensure BEG is before END.
        (icicle-with-comments-hidden
         beg end
         (icicle-condition-case-no-debug icicle-search-thing-scan
             (save-excursion
               (goto-char (setq last-beg  beg)) ; `icicle-next-visible-thing-and-bounds' uses point.
               (while (and last-beg  (< last-beg end))
                 (while (and (< beg end)  (icicle-invisible-p beg)) ; Skip invisible, overlay or text.
                   (when (get-char-property beg 'invisible)
                     (setq beg  (icicle-next-single-char-property-change beg 'invisible nil end))))
                 (let ((thg+bnds  (icicle-next-visible-thing-and-bounds thing beg end)))
                   (if (and (not thg+bnds)  (not icicle-search-complement-domain-p))
                       (setq beg  end)
                     (let* ((thg-beg       (cadr thg+bnds))
                            (thg-end       (cddr thg+bnds))
                            (tr-thg-beg    thg-beg)
                            (tr-thg-end    thg-end)
                            (hit-beg       (if icicle-search-complement-domain-p
                                               last-beg
                                             tr-thg-beg))
                            (hit-end       (if icicle-search-complement-domain-p
                                               (or tr-thg-beg  end)
                                             tr-thg-end))
                            (hit-string    (buffer-substring hit-beg hit-end))
                            (end-marker    (copy-marker hit-end))
                            (filteredp     (or (not predicate)
                                               (not thg+bnds)
                                               (funcall predicate thg+bnds)))
                            (new-thg+bnds  (if icicle-search-complement-domain-p
                                               thg+bnds
                                             (and filteredp
                                                  thg+bnds
                                                  (if transform-fn
                                                      (funcall transform-fn thg+bnds)
                                                    thg+bnds)))))
                       (when (and (not (string= "" hit-string)) ; No-op if empty hit.
                                  (or new-thg+bnds  icicle-search-complement-domain-p))
                         (when (and transform-fn  (not icicle-search-complement-domain-p))
                           (setq hit-string  (car  new-thg+bnds)
                                 tr-thg-beg  (cadr new-thg+bnds)
                                 tr-thg-end  (cddr new-thg+bnds)
                                 end-marker  (copy-marker tr-thg-end)))

                         (when (and icicle-ignore-comments-flag  icicle-search-complement-domain-p)
                           (put-text-property 0 (length hit-string) 'invisible nil hit-string))
                         
                         (icicle-candidate-short-help
                          (concat (and add-bufname-p  (format "Buffer: `%s', "
                                                              (buffer-name (marker-buffer end-marker))))
                                  (format "Bounds: (%d, %d), Length: %d"
                                          hit-beg hit-end (length hit-string)))
                          hit-string)
                         (push (cons (if add-bufname-p
                                         (list hit-string
                                               (let ((string  (copy-sequence (buffer-name))))
                                                 (put-text-property
                                                  0 (length string)
                                                  'face 'icicle-candidate-part string)
                                                 string))
                                       hit-string)
                                     end-marker)
                               temp-list)
                         ;; Highlight search context in buffer.
                         (when (and (not (equal hit-beg hit-end))
                                    (or (eq t icicle-search-highlight-threshold)
                                        (<= (+ (length temp-list) (length icicle-candidates-alist))
                                            icicle-search-highlight-threshold)))
                           (let ((ov  (make-overlay hit-beg hit-end)))
                             (push ov icicle-search-overlays)
                             (overlay-put ov 'priority 200) ; > ediff's 100+, but < isearch overlays
                             (overlay-put ov 'face 'icicle-search-main-regexp-others))))
                       (if thg-end
                           ;; $$$$$$
                           ;; The correct code here is (setq beg end).  However, unless you use my
                           ;; library `thingatpt+.el' or unless Emacs bug #9300 is fixed (hopefully
                           ;; in Emacs 24), that will loop forever.  In that case we move forward a
                           ;; char to prevent looping, but that means that the position just after
                           ;; a THING is considered to be covered by the THING (which is incorrect).
                           (setq beg  (if (or (featurep 'thingatpt+)  (> emacs-major-version 23))
                                          thg-end
                                        (1+ thg-end)))
                         ;; If visible then no more things - skip to END.
                         (unless (icicle-invisible-p beg) (setq beg  end)))))
                   (setq last-beg  beg)))
               (setq icicle-candidates-alist  (append icicle-candidates-alist (nreverse temp-list))))
           (quit (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup)))
           (error (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
                  (error "%s" (error-message-string icicle-search-thing-scan)))))))))

;; Same as `thgcmd-invisible-p' in `thing-cmds.el'.
(defun icicle-invisible-p (position)
  "Return non-nil if the character at POSITION is invisible."
  (if (fboundp 'invisible-p)            ; Emacs 22+
      (invisible-p position)
    (let ((prop  (get-char-property position 'invisible))) ; Overlay or text property.
      (if (eq buffer-invisibility-spec t)
          prop
        (or (memq prop buffer-invisibility-spec)  (assq prop buffer-invisibility-spec))))))

(defun icicle-invisible-face-p (face)
  "Return non-nil if FACE is currently invisibile."
  (and (consp buffer-invisibility-spec)
       (or (memq face buffer-invisibility-spec)  (assq face buffer-invisibility-spec))))

(defun icicle-next-visible-thing-and-bounds (thing start end)
  "Return the next visible THING and its bounds.
Start at BEG and end at END, when searching for THING.
Return (THING THING-START . THING-END), with THING-START and THING-END
 the bounds of THING.  Return nil if no such THING is found.

The \"visible\" in the name refers to ignoring things that are within
invisible text, such as hidden comments.

You can toggle hiding of comments using `C-M-;' in the minibuffer, but
depending on when you do so you might need to invoke the current
command again."
  (save-excursion (icicle-next-visible-thing thing start end)))

;; Simple version of `previous-visible-thing' from `thing-cmds.el'.
;;
;;;###autoload (autoload 'icicle-previous-visible-thing "icicles")
(defun icicle-previous-visible-thing (thing start &optional end)
  "Same as `icicle-next-visible-thing', except it moves backward."
  (interactive
   (list (or (and (memq last-command '(icicle-next-visible-thing icicle-previous-visible-thing))
                  icicle-last-thing-type)
             (prog1 (intern (completing-read "Thing (type): " (icicle-things-alist) nil nil nil nil
                                             (symbol-name icicle-last-thing-type)))))
         (point)
         (if mark-active (min (region-beginning) (region-end)) (point-min))))
  (if (interactive-p)
      (icicle-with-comments-hidden start end (icicle-next-visible-thing thing start end 'BACKWARD))
    (icicle-next-visible-thing thing start end 'BACKWARD)))

;; Simple version of `next-visible-thing' from `thing-cmds.el'.
;;
;;;###autoload (autoload 'icicle-next-visible-thing "icicles")
(defun icicle-next-visible-thing (thing &optional start end backward)
  "Go to the next visible THING.
Start at START.  If END is non-nil then look no farther than END.
Interactively:
 - START is point.
 - If the region is not active, END is the buffer end.  If the region
   is active, END is the region end: the greater of point and mark.

Ignores (skips) comments if `icicle-ignore-comments-flag' is non-nil.
You can toggle this ignoring of comments using `C-M-;' in the
minibuffer, but depending on when you do so you might need to invoke
the current command again.

If you use this command or `icicle-previous-visible-thing'
successively, even mixing the two, you are prompted for the type of
THING only the first time.  You can thus bind these two commands to
simple, repeatable keys (e.g. `f11', `f12'), to navigate among things
quickly.

Non-interactively, THING is a symbol, and optional arg BACKWARD means
go to the previous thing.

Return (THING THING-START . THING-END), with THING-START and THING-END
the bounds of THING.  Return nil if no such THING is found."
  (interactive
   (list (or (and (memq last-command '(icicle-next-visible-thing icicle-previous-visible-thing))
                  icicle-last-thing-type)
             (prog1 (intern (completing-read "Thing (type): " (icicle-things-alist) nil nil nil nil
                                             (symbol-name icicle-last-thing-type)))))
         (point)
         (if mark-active (max (region-beginning) (region-end)) (point-max))))
  (setq icicle-last-thing-type  thing)
  (unless start (setq start  (point)))
  (unless end   (setq end    (if backward (point-min) (point-max))))
  (cond ((< start end) (when backward (setq start  (prog1 end (setq end  start)))))
        ((> start end) (unless backward (setq start  (prog1 end (setq end  start))))))
  (if (interactive-p)
      (icicle-with-comments-hidden start end (icicle-next-visible-thing-1 thing start end backward))
    (icicle-next-visible-thing-1 thing start end backward)))

;;; Same as `thgcmd-next-visible-thing-1' in `thing-cmds.el'.
(if (fboundp 'thgcmd-next-visible-thing-1)
    (defalias 'icicle-next-visible-thing-1 'thgcmd-next-visible-thing-1)
  (defun icicle-next-visible-thing-1 (thing start end backward)
    "Helper for `icicle-next-visible-thing'.  Get thing past point."
    (let ((thg+bds  (icicle-next-visible-thing-2 thing start end backward)))
      (if (not thg+bds)
          nil
        ;; $$$$$$ Which is better, > or >=, < or <=, for the comparisons?
        ;; $$$$$$ Seems that < is better than <=, at least for `icicle-search-thing':
        ;; $$$$$$ for XML elements and lists, <= misses the first one.
        (while (and thg+bds  (if backward  (> (cddr thg+bds) (point))  (< (cadr thg+bds) (point))))
          (if backward
              (setq start  (max end (1- (cadr thg+bds))))
            (setq start  (min end (1+ (cddr thg+bds)))))
          (setq thg+bds  (icicle-next-visible-thing-2 thing start end backward)))
        (when thg+bds (goto-char (cadr thg+bds)))
        thg+bds))))

;;; Same as `thgcmd-next-visible-thing-2' in `thing-cmds.el'.
(if (fboundp 'thgcmd-next-visible-thing-2)
    (defalias 'icicle-next-visible-thing-2 'thgcmd-next-visible-thing-2)
  (defun icicle-next-visible-thing-2 (thing start end &optional backward)
    "Helper for `icicle-next-visible-thing-1'.  Thing might not be past START."
    (and (not (= start end))
         (save-excursion
           (let ((bounds  nil))
             ;; If BACKWARD, swap START and END.
             (cond ((< start end) (when   backward (setq start  (prog1 end (setq end  start)))))
                   ((> start end) (unless backward (setq start  (prog1 end (setq end  start))))))
             (catch 'icicle-next-visible-thing-2
               (while (if backward (> start end) (< start end))
                 (goto-char start)
                 ;; Skip invisible text.
                 (when (and (if backward (> start end) (< start end))  (icicle-invisible-p start))
                   (setq start  (if (get-text-property start 'invisible) ; Text prop.
                                    (if backward
                                        (previous-single-property-change start 'invisible nil end)
                                      (next-single-property-change start 'invisible nil end))
                                  (if backward ; Overlay prop.
                                      (previous-overlay-change start)
                                    (next-overlay-change start))))
                   (goto-char start))
                 (when (and (setq bounds  (icicle-bounds-of-thing-at-point thing))
                            (not (equal (car bounds) (cdr bounds)))) ; Not an empty thing, "".
                   (throw 'icicle-next-visible-thing-2
                     (cons (buffer-substring (car bounds) (cdr bounds)) bounds)))
                 (setq start  (if backward (1- start) (1+ start))))
               nil))))))

;;;###autoload (autoload 'icicle-search-xml-element "icicles")
(defun icicle-search-xml-element (beg end require-match where element)
  "`icicle-search' with XML ELEMENTs as search contexts.
ELEMENT is a regexp that is matched against actual element names.

The search contexts are the top-level matching elements within the
search limits, BEG and END.  Those elements might or might not contain
descendent elements that are themselves of type ELEMENT.

You can alternatively choose to search, not the search contexts as
defined by the element-name regexp, but the non-contexts, that is, the
buffer text that is outside such elements.  To do this, use `C-M-~'
during completion.  (This is a toggle, and it affects only future
search commands, not the current one.)

You will no doubt need nXML for this command.  It is included in
vanilla Emacs, starting with Emacs 23.  And you will need to load
`thingatpt+.el', because of bugs in vanilla `thingatpt.el'."
  (interactive
   (let* ((where    (icicle-search-where-arg))
          (beg+end  (icicle-region-or-buffer-limits))
          (beg1     (car beg+end))
          (end1     (cadr beg+end))
          (elt      (read-string "XML element (name regexp, no markup): " nil 'regexp-history)))
     `(,beg1 ,end1 ,(not icicle-show-multi-completion-flag) ,where ,elt)))
  (let ((nxml-sexp-element-flag  t))
    (icicle-search-thing
     'sexp beg end require-match where
     `(lambda (thg+bds)
       (and thg+bds  (icicle-string-match-p ,(format "\\`\\s-*<\\s-*%s\\s-*>" element) (car thg+bds)))))))

;;;###autoload (autoload 'icicle-search-xml-element-text-node "icicles")
(defun icicle-search-xml-element-text-node (beg end require-match where element)
  "`icicle-search', with text() nodes of XML ELEMENTs as search contexts.
ELEMENT is a regexp that is matched against actual XML element names.

The search contexts are the text() nodes of the top-level matching
elements within the search limits, BEG and END.  (Those elements might
or might not contain descendent elements that are themselves of type
ELEMENT.)

You can alternatively choose to search, not the search contexts as
defined by the element-name regexp, but the non-contexts, that is, the
buffer text that is outside the text nodes of such elements.  To do
this, use `C-M-~' during completion.  (This is a toggle, and it
affects only future search commands, not the current one.)

You will no doubt need nXML for this command.  It is included in
vanilla Emacs, starting with Emacs 23.  And you will need to load
`thingatpt+.el', because of bugs in vanilla `thingatpt.el'."
  (interactive
   (let* ((where    (icicle-search-where-arg))
          (beg+end  (icicle-region-or-buffer-limits))
          (beg1     (car beg+end))
          (end1     (cadr beg+end))
          (elt      (read-string "XML element (name regexp, no markup): " nil 'regexp-history)))
     `(,beg1 ,end1 ,(not icicle-show-multi-completion-flag) ,where ,elt)))
  (let ((nxml-sexp-element-flag  t))
    (icicle-search-thing
     'sexp beg end require-match where
     `(lambda (thg+bds)
       (and thg+bds  (icicle-string-match-p ,(format "\\`\\s-*<\\s-*%s\\s-*>" element) (car thg+bds))))
     `(lambda (thg+bds)
       (save-excursion
         (let* ((tag-end     (string-match ,(format "\\`\\s-*<\\s-*%s\\s-*>" element)
                                           (car thg+bds)))
                (child       (icicle-next-visible-thing
                              'sexp (+ (match-end 0) (cadr thg+bds)) (cddr thg+bds)))
                (tag-regexp  (concat "\\`\\s-*<\\s-*"
                                     (if (> emacs-major-version 20)
                                         (if (boundp 'xmltok-ncname-regexp)
                                             xmltok-ncname-regexp
                                           "\\(?:[_[:alpha:]][-._[:alnum:]]*\\)")
                                       "\\([_a-zA-Z][-._a-zA-Z0-9]\\)")
                                     "\\s-*>")))
           (and child  (not (icicle-string-match-p tag-regexp (car child)))  child)))))))

;;;###autoload (autoload 'icicle-search-char-property "icicles")
(defun icicle-search-char-property (beg end require-match ; Bound to `M-s M-s c'.
                                    &optional where prop values predicate match-fn)
  "Search for text that has a character property with a certain value.
By \"character property\" is meant either an overlay property or a
text property.  If you want to search for only an overlay property or
only a text property, then use `icicle-search-overlay-property' or
`icicle-search-text-property' instead.

Interactively, you are prompted for the property to search and its
value.  By default, an actual value of the property matches the value
you specify if it is `equal'.  Properties `mumamo-major-mode' and
`face' (or `font-lock-face') are exceptions.

For `mumamo-major-mode' you specify the major mode whose zones of text
you want to search.  The actual property value is a list whose car is
the major mode symbol.

For properties `face' and `font-lock-face', you can pick multiple
faces, using completion (e.g., `C-RET' and finally `RET').  Text is
searched that has a face property that includes any of the faces you
choose.  If you choose no face (empty input), then text with any face
at all is searched.

You can alternatively choose to search, not the search contexts as
defined, but the zones of buffer text that do NOT have the given
character property value.  In other words, search the complement
zones.  To toggle such complementing, use `C-M-~' anytime during
completion.  (This toggling affects only future search commands, not
the current one.)

Non-interactively, arguments BEG, END, REQUIRE-MATCH, and WHERE are as
for `icicle-search'.  Arguments PROP, VALUES, and PREDICATE are passed
to `icicle-search-char-property-scan' to define the search contexts.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive (icicle-search-property-args))
  (icicle-search beg end 'icicle-search-char-property-scan require-match where prop values nil
                 predicate match-fn))

;;;###autoload (autoload 'icicle-search-overlay-property "icicles")
(defun icicle-search-overlay-property (beg end require-match where ; Bound to `M-s M-s O'
                                       prop values predicate match-fn)
  "Same as `icicle-search-char-property', except only overlay property.
That is, do not also search a text property."
  (interactive (icicle-search-property-args))
  (icicle-search beg end 'icicle-search-char-property-scan require-match where prop values 'overlay
                 predicate match-fn))

;;;###autoload (autoload 'icicle-search-text-property "icicles")
(defun icicle-search-text-property (beg end require-match ; Bound to `M-s M-s T', `C-c "'.
                                    where prop values predicate match-fn)
  "Same as `icicle-search-char-property', except only text property.
That is, do not also search an overlay property."
  (interactive (icicle-search-property-args))
  ;; Because font-lock is just-in-time or lazy, if PROP is `face' we fontify the region.
  ;; Should this be an option, so users who want to can font-lock on demand instead of each time?
  (when (and (eq prop 'face)  font-lock-mode)
    (save-excursion (font-lock-fontify-region beg end 'VERBOSE)))
  (icicle-search beg end 'icicle-search-char-property-scan require-match where prop values 'text
                 predicate match-fn))

(defun icicle-search-property-args ()
  "Read and return interactive arguments for `icicle-search-*-property'."
  (let* ((where            (icicle-search-where-arg))
         (beg+end          (icicle-region-or-buffer-limits))
         (beg1             (car beg+end))
         (end1             (cadr beg+end))
         (props            (mapcar (lambda (prop) (list (symbol-name prop)))
                                   (icicle-char-properties-in-buffers where beg1 end1)))
         (prop             (intern (completing-read
                                    (format "Property %sto search: "
                                            (if icicle-search-complement-domain-p "*NOT* " ""))
                                    props nil nil nil nil "face")))
         (icicle-prompt    "Choose property value (`RET' when done): ")
         (icicle-hist-var  'icicle-char-property-value-history)
         (values           (if (memq prop '(face font-lock-face))
                               (let ((faces  (icicle-face-list)))
                                 (if faces (mapcar #'intern faces) (face-list))) ; Default: all faces.
                             (icicle-sexp-list)))
         (match-fn         (icicle-search-property-default-match-fn prop)))
    `(,beg1 ,end1 ,(not icicle-show-multi-completion-flag) ,where ,prop ,values nil ,match-fn)))

(defun icicle-search-property-default-match-fn (prop)
  "Return the default match function for text or overlay property PROP.
Properties `face' and `mumamo-major-mode' are handled specially.
For other properties the values are matched using `equal'."
  (case prop
    ((face font-lock-face) (lambda (val rprop)
                             (if (consp rprop)
                                 (condition-case nil ; Allow for dotted cons.
                                     (member val rprop)
                                   (error nil))
                               (eq val rprop))))
    ((mumamo-major-mode)   (lambda (val rprop) (equal val (car rprop))))
    (t                     #'equal)))

(defun icicle-char-properties-in-buffers (where beg end &optional type)
  "List of all character properties in WHERE.
The other arguments are passed to `icicle-char-properties-in-buffer'.
Only the character properties are included, not their values.
WHERE is a list of buffers, a list of files, or a list of region
  bookmarks (in which case you must also use library `Bookmark+').
  If nil, then only the current buffer is used.
TYPE can be `overlay', `text', or nil, meaning overlay properties,
text properties, or both, respectively."
  (cond ((and (consp where)  (bufferp (car where))) ; List of buffers - search buffers.
         (dolist (buf  where) (icicle-char-properties-in-buffer buf nil nil type)))
        ((and (consp where)  (stringp (car where)) ; List of files - search files.  (Check only the first.)
              (or (icicle-file-remote-p (car where)) ; Don't let Tramp try to access it.
                  (file-exists-p (car where))))
         (dolist (file  where)
           (icicle-char-properties-in-buffer (find-file-noselect file) nil nil type)))
        ((consp where)                  ; Search bookmarked regions.
         (unless (require 'bookmark+ nil t) (error "This requires library `Bookmark+'"))
         (let (buf+beg buf beg end)
           (dolist (bmk  where)
             (setq buf+beg  (bookmark-jump-noselect bmk)
                   buf      (car buf+beg)
                   beg      (cdr buf+beg)
                   end      (bmkp-get-end-position bmk))
             (when (bufferp buf) (icicle-char-properties-in-buffer (get-buffer buf) beg end type)))))
        (t                              ; Search this buffer only.
         (icicle-char-properties-in-buffer (current-buffer) beg end type))))

(defun icicle-char-properties-in-buffer (&optional buffer beg end type)
  "List of all character properties in BUFFER between BEG and END.
Only the character properties are included, not their values.
TYPE can be `overlay', `text', or nil, meaning overlay properties,
text properties, or both, respectively."
  (unless buffer (setq buffer  (current-buffer)))
  (let ((props  ())
        ovrlays curr-props)
    (when (bufferp buffer)              ; Do nothing if BUFFER is not a buffer.
      (with-current-buffer buffer
        (unless (and beg  end) (setq beg  (point-min)
                                     end  (point-max)))
        (when (or (not type)  (eq type 'overlay)) ; Get overlay properties.
          (setq ovrlays  (overlays-in beg end))
          (dolist (ovrly  ovrlays)
            (setq curr-props  (overlay-properties ovrly))
            (while curr-props
              (unless (memq (car curr-props) props) (push (car curr-props) props))
              (setq curr-props  (cddr curr-props)))))
        (when (or (not type)  (eq type 'text)) ; Get text properties.
          (while (< beg end)
            (setq beg         (or (next-property-change beg nil end)  end)
                  curr-props  (text-properties-at beg))
            (while curr-props
              (unless (memq (car curr-props) props) (push (car curr-props) props))
              (setq curr-props  (cddr curr-props)))))))
    props))

(defun icicle-search-char-property-scan (buffer beg end prop values type predicate match-fn)
  "Scan BUFFER from BEG to END for character property PROP with VALUES.
Push hits onto `icicle-candidates-alist'.
If BUFFER is nil, scan the current buffer.
Highlight the matches in face `icicle-search-main-regexp-others'.
If BEG and END are nil, scan entire BUFFER.

Find text with a PROP value that overlaps with VALUES.  That is, if
the value of PROP is an atom, then it must be a member of VALUES; if
it is a list, then at least one list element must be a member of
VALUES.

TYPE is `overlay', `text', or nil, and specifies the type of character
property - nil means look for both overlay and text properties.

If PREDICATE is non-nil, then push only the hits for which it holds.
PREDICATE is nil or a Boolean function that takes these arguments:
  - the search-context string
  - a marker at the end of the search-context

MATCH-FN is a binary predicate that is applied to each item of VALUES
and a zone of text with property PROP.  If it returns non-nil then the
zone is a search hit."
  (setq match-fn  (or match-fn  (icicle-search-property-default-match-fn prop)))
  (let ((add-bufname-p  (and buffer  icicle-show-multi-completion-flag))
        (temp-list      ())
        (last           nil)
        (zone-end       nil))
    (unless buffer (setq buffer  (current-buffer)))
    (when (bufferp buffer)              ; Do nothing if BUFFER is not a buffer.
      (with-current-buffer buffer
        (unless (and beg  end) (setq beg  (point-min)
                                     end  (point-max)))
        (icicle-condition-case-no-debug icicle-search-char-property-scan
            (save-excursion
              (setq last  beg)
              (while (and beg  (< beg end) ; Skip to first zone.
                          (not (icicle-search-char-prop-matches-p type prop values match-fn beg)))
                (setq beg  (icicle-next-single-char-property-change beg prop nil end)))
              (while (and beg  (< last end)) ; We have a zone.
                (setq zone-end  (or (icicle-next-single-char-property-change beg prop nil end)  end))
                (let ((hit-beg  (if icicle-search-complement-domain-p last beg))
                      (hit-end  (if icicle-search-complement-domain-p beg zone-end))
                      saved-hit-beg saved-hit-end)
                  (when (and (not (= hit-beg hit-end)) ; Do nothing if hit is empty.
                             (setq hit-end  (if (markerp hit-end) hit-end (copy-marker hit-end)))
                             (or (not predicate)
                                 (save-match-data
                                   (funcall predicate (buffer-substring hit-beg hit-end) hit-end))))
                    (setq saved-hit-beg  hit-beg
                          saved-hit-end  hit-end)
                    ;; Try to extend zone.
                    (let (hit-beg hit-end)
                      (while (and beg  (< last end)
                                  (icicle-search-char-prop-matches-p type prop values match-fn beg))
                        (setq zone-end  (or (icicle-next-single-char-property-change beg prop nil end)  end))
                        (setq hit-beg  (if icicle-search-complement-domain-p last beg)
                              hit-end  (if icicle-search-complement-domain-p beg zone-end))
                        (when (and (not (= hit-beg hit-end)) ; Do nothing if hit is empty.
                                   (setq hit-end  (if (markerp hit-end) hit-end (copy-marker hit-end)))
                                   (or (not predicate)
                                       (save-match-data
                                         (funcall predicate (buffer-substring hit-beg hit-end) hit-end))))
                          (setq saved-hit-end  hit-end))
                        (setq beg   zone-end
                              last  zone-end))
                      zone-end)
                    ;; Add candidate to `temp-list'.
                    (let ((hit-string  (buffer-substring hit-beg saved-hit-end)))
                      (icicle-candidate-short-help
                       (concat (and add-bufname-p  (format "Buffer: `%s', "
                                                           (buffer-name (marker-buffer saved-hit-end))))
                               (format "Position: %d, Length: %d"
                                       (marker-position saved-hit-end) (length hit-string)))
                       hit-string)
                      (push (cons (if add-bufname-p
                                      (list hit-string
                                            (let ((string  (copy-sequence (buffer-name))))
                                              (put-text-property 0 (length string)
                                                                 'face 'icicle-candidate-part string)
                                              string))
                                    hit-string)
                                  saved-hit-end)
                            temp-list)
                      ;; Highlight search context in buffer.
                      (when (or (eq t icicle-search-highlight-threshold)
                                (<= (+ (length temp-list) (length icicle-candidates-alist))
                                    icicle-search-highlight-threshold))
                        (let ((ov  (make-overlay hit-beg saved-hit-end)))
                          (push ov icicle-search-overlays)
                          (overlay-put ov 'priority 200) ; > ediff's 100+, but < isearch overlays
                          (overlay-put ov 'face 'icicle-search-main-regexp-others))))))
                (setq beg   zone-end
                      last  zone-end)
                (while (and beg  (< beg end) ; Skip to next zone.
                            (not (icicle-search-char-prop-matches-p type prop values match-fn beg)))
                  (setq beg  (icicle-next-single-char-property-change beg prop nil end))))
              (setq icicle-candidates-alist  (append icicle-candidates-alist (nreverse temp-list))))
          (quit (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup)))
          (error (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
                 (error "%s" (error-message-string icicle-search-char-property-scan))))))))

(defun icicle-search-char-prop-matches-p (type property values match-fn position)
  "Return non-nil if POSITION has PROPERTY with a value matching VALUES.
TYPE, VALUES, MATCH-FN are as in `icicle-search-char-property-scan'."
  (let* ((ovlyval  (and (or (not type)  (eq type 'overlay))  (get-char-property position property)))
         (textval  (and (or (not type)  (eq type 'text))     (get-text-property position property))))
    (or (and ovlyval  (icicle-some values ovlyval match-fn))
        (and textval  (icicle-some values textval match-fn)))))

(if (fboundp 'next-single-char-property-change)
    (defalias 'icicle-next-single-char-property-change 'next-single-char-property-change)
  (defun icicle-next-single-char-property-change (position prop &optional object limit)
    "Position of next change of PROP for text property or overlay change.
Scans characters forward from buffer position POSITION until property
PROP changes.  Returns the position of that change.

POSITION is a buffer position (integer or marker).

Optional third arg OBJECT is ignored.  It is present for compatibility
 with Emacs 22+.

If optional fourth arg LIMIT is non-nil, search stops at position
LIMIT.  LIMIT is returned if nothing is found before reaching LIMIT.

The property values are compared with `eq'.  If the property is
constant all the way to the end of the buffer, then return the last
valid buffer position."
    (save-excursion
      (goto-char position)
      (let ((propval  (get-char-property (point) prop))
            (end      (min limit (point-max))))
        (while (and (< (point) end)  (eq (get-char-property (point) prop) propval))
          (goto-char (min (next-overlay-change (point))
                          (next-single-property-change (point) prop nil end)))))
      (point))))

(if (fboundp 'previous-single-char-property-change)
    (defalias 'icicle-previous-single-char-property-change 'previous-single-char-property-change)
  (defun icicle-previous-single-char-property-change (position prop &optional object limit)
    "Position of previous change of PROP for text property or overlay change.
Scans characters backward from buffer position POSITION until property
PROP changes.  Returns the position of that change.

POSITION is a buffer position (integer or marker).

Optional third arg OBJECT is ignored.  It is present for compatibility
 with Emacs 22+.

If optional fourth arg LIMIT is non-nil, search stops at position
LIMIT.  LIMIT is returned if nothing is found before reaching LIMIT.

The property values are compared with `eq'.  If the property is
constant all the way to the start of the buffer, then return the first
valid buffer position."
    (save-excursion
      (goto-char position)
      (let ((propval  (get-char-property (point) prop))
            (end      (max limit (point-min))))
        (while (and (> (point) end)  (eq (get-char-property (point) prop) propval))
          (goto-char (max (next-overlay-change (point))
                          (next-single-property-change (point) prop nil end)))))
      (point))))

;;;###autoload (autoload 'icicle-search-highlight-cleanup "icicles")
(defun icicle-search-highlight-cleanup ()
  "Remove all highlighting from the last use of `icicle-search'."
  (interactive)
  (let ((inhibit-quit  t))
    (message "Removing search highlighting...")
    (while icicle-search-overlays
      (delete-overlay (car icicle-search-overlays))
      (setq icicle-search-overlays  (cdr icicle-search-overlays)))
    (while icicle-search-level-overlays
      (delete-overlay (car icicle-search-level-overlays))
      (setq icicle-search-level-overlays  (cdr icicle-search-level-overlays)))
    (when (overlayp icicle-search-current-overlay)
      (delete-overlay icicle-search-current-overlay))
    (when (overlayp icicle-search-refined-overlays)
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays  ()))
    (while icicle-search-refined-overlays
      (delete-overlay (car icicle-search-refined-overlays))
      (setq icicle-search-refined-overlays  (cdr icicle-search-refined-overlays)))
    (message "Removing search highlighting...done")))

;;;###autoload (autoload 'icicle-search-word "icicles")
(defun icicle-search-word (beg end word-regexp require-match ; Bound to `M-s M-s w', `C-c $'.
                           &optional where &rest args)
  "Search for a whole word.
The search string is regarded as a whole word, but a \"word\" here can
contain embedded strings of non word-constituent chars (they are
skipped over, when matching, included in the match), and any leading
or trailing word-constituent chars in the search string are dropped
\(ignored for matching, not included in the match): matches begin and
end on a word boundary.

At the prompt for a word, you can use completion against previous
Icicles search inputs to choose the word, or you can enter a new word.

Non-interactively, WORD-REGEXP should be a regexp that matches a word.
The other arguments are the same as for `icicle-search'.

You can alternatively choose to search, not the word search contexts
you define, but the buffer text that is outside these contexts: the
non-word text.  To do this, use `C-M-~' during completion.  \(This is
a toggle, and it affects only future search commands, not the current
one.)

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(icicle-search-read-word)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (icicle-search beg end word-regexp (not icicle-show-multi-completion-flag) where))

;;;###autoload (autoload 'icicle-search-bookmarks-together "icicles")
(defun icicle-search-bookmarks-together (scan-fn-or-regexp require-match ; Bound to `M-s M-s J'.
                                         &rest args)
  "Search bookmarks, together.
The arguments are the same as for `icicle-search', but without
arguments BEG, END, and WHERE.

This is the same as using a plain prefix arg, `C-u', with
`icicle-search'.

You choose the bookmarks to search.
If `icicle-show-multi-completion-flag' is non-nil, then completion
candidates are multi-completions, with first part the bookmark name
and second part the bookmark's file or buffer name.  Otherwise, the
candidates are just the bookmark names.

You can alternatively choose to search, not the search contexts as
defined by the context regexp, but the non-contexts, that is, the text
in the bookmarked buffer that does not match the regexp.  To do this,
use `C-M-~' during completion.  (This is a toggle, and it affects only
future search commands, not the current one.)

An alternative is multi-command `icicle-search-bookmark', which
searches the bookmarked regions/buffers you choose one at a time."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-show-multi-completion-flag)))
  (let ((icicle-multi-completing-p  icicle-show-multi-completion-flag))
    (apply #'icicle-search nil nil scan-fn-or-regexp require-match
           (let ((current-prefix-arg  '(4))) (icicle-search-where-arg))
           args)))

;;;###autoload (autoload 'icicle-search-buffer "icicles")
(defun icicle-search-buffer (scan-fn-or-regexp require-match &rest args) ; Bound to `M-s M-s b'.
  "Search multiple buffers completely.
Same as using a non-negative numeric prefix arg, such as `C-9', with
`icicle-search'.  You are prompted for the buffers to search.  All of
each buffer is searched.  Any existing buffers can be chosen.
Arguments are the same as for `icicle-search', but without arguments
BEG, END, and WHERE.

You can alternatively choose to search, not the search contexts as
defined by the context regexp you provide, but the non-contexts, that
is, the text in the buffers that does not match the regexp.  To do
this, use `C-M-~' during completion.  (This is a toggle, and it
affects only future search commands, not the current one.)"
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-show-multi-completion-flag)))
  (let ((icicle-multi-completing-p  icicle-show-multi-completion-flag))
    (apply #'icicle-search nil nil scan-fn-or-regexp require-match
           (let ((icicle-show-Completions-initially-flag  t))
             (mapcar #'get-buffer
                     (let ((icicle-buffer-require-match-flag  'partial-match-ok)
                           (icicle-prompt
                            "Choose buffer to search (`RET' when done): "))
                       (icicle-buffer-list))))
           args)))

;;;###autoload (autoload 'icicle-search-file "icicles")
(defun icicle-search-file (scan-fn-or-regexp require-match &rest args) ; Bound to `M-s M-s f'.
  "Search multiple files completely.
Same as using a negative numeric prefix arg, such as `C--', with
`icicle-search'.  You are prompted for the files to search.  All of
each file is searched.  Any existing files in the current directory
can be chosen.  Arguments are the same as for `icicle-search', but
without arguments BEG, END, and WHERE.

You can alternatively choose to search, not the search contexts as
defined by the context regexp you provide, but the non-contexts, that
is, the text in the files that does not match the regexp.  To do this,
use `C-M-~' during completion.  (This is a toggle, and it affects only
future search commands, not the current one.)"
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-show-multi-completion-flag)))
  (let ((icicle-multi-completing-p  icicle-show-multi-completion-flag))
    (apply #'icicle-search nil nil scan-fn-or-regexp require-match
           (let ((icicle-show-Completions-initially-flag  t)
                 (icicle-prompt                           "Choose file to search (`RET' when done): "))
             (icicle-file-list))
           args)))

;;;###autoload (autoload 'icicle-search-bookmark-list-marked "icicles")
(defun icicle-search-bookmark-list-marked (scan-fn-or-regexp require-match ; Bound to `M-s M-s m'.
                                           &rest args) ; Bound also to `C-0 M-s M-s M-s', `C-0 C-`'.
  "Search the files of the marked bookmarks in `*Bookmark List*'.
Same as using `C-0' with `icicle-search' in `*Bookmark List*'. 
Arguments are the same as for `icicle-search', but without arguments
BEG, END, and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-show-multi-completion-flag)))
  (unless (fboundp 'bmkp-bmenu-get-marked-files)
    (error "Command `icicle-bookmark-save-marked-files' requires library Bookmark+"))
  (bmkp-bmenu-barf-if-not-in-menu-list)
  (let ((icicle-multi-completing-p  icicle-show-multi-completion-flag))
    (apply #'icicle-search nil nil scan-fn-or-regexp require-match (bmkp-bmenu-get-marked-files) args)))

;;;###autoload (autoload 'icicle-search-dired-marked-recursive "icicles")
(defun icicle-search-dired-marked-recursive (ignore-marks-p scan-fn-or-regexp require-match &rest args)
                                        ; Bound to `M-s M-s m' in Dired with Dired+.
                                        ; Bound also to `C-0 M-s M-s M-s', `C-0 C-c `' in Dired.
  "Search marked files in Dired, including in marked subdirs, recursively.
You need library `Dired+' for this command.

With a prefix arg, ignore Dired markings: search all files in the
directory.  (Non-interactively, do this if argument IGNORE-MARKS-P is
non-nil.)

Otherwise, search only the marked files in the directory, or all of
the files if none are marked.

Marked subdirectories are handled recursively in the same way: If they
have a Dired buffer then search their marked files, or all of their
files if none are marked.  If a marked directory at any level has no
Dired buffer then search all of its files.  If a marked directory has
more than one Dired buffer then raise an error.

Because you might not be aware of existing Dired buffers for some
marked directories, you are asked to confirm searching their marked
files.  If you do not confirm this then *all* files in marked
directories are searched, regardless of whether directories might have
Dired buffers with marked files.  That is, Dired buffers are ignored
if you do not confirm using them.

This is bound by default to `M-s M-s m' in Dired.  This is the same as
using `C-0' with `icicle-search' in Dired, that is, `C-0 M-s M-s M-s'
or `C-0 C-c `', but in that case you cannot use the prefix arg to
ignore markings.

Non-interactively, the arguments other than IGNORE-MARKS-P are the
same as for `icicle-search', but without arguments BEG, END, and
WHERE."
  (interactive
   (progn
     (unless (fboundp 'diredp-get-files) (error "This command requires library `dired+.el'"))
     (diredp-get-confirmation-recursive) ; Make user confirm, since this can explore *lots* of files.
     `(,current-prefix-arg
       ,(if icicle-search-whole-word-flag
            (icicle-search-read-word)
            (icicle-search-read-context-regexp))
       ,(not icicle-show-multi-completion-flag))))
  (unless (fboundp 'diredp-get-files) (error "This command requires library `dired+.el'"))
  (unless (eq major-mode 'dired-mode) (error "This command must be called from a Dired buffer"))
  (apply #'icicle-search nil nil scan-fn-or-regexp require-match (diredp-get-files ignore-marks-p) args))

;;;###autoload (autoload 'icicle-search-ibuffer-marked "icicles")
(defun icicle-search-ibuffer-marked (scan-fn-or-regexp require-match ; Bound to `M-s M-s m' in Ibuffer.
                                     &rest args) ; Bound also to `C-0 M-s M-s M-s', `C-0 C-`' in Ibuffer.
  "Search the marked buffers in Ibuffer, in order.
Same as using `C-0' with `icicle-search' in Ibuffer.
Arguments are the same as for `icicle-search', but without arguments
BEG, END, and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-show-multi-completion-flag)))
  (unless (eq major-mode 'ibuffer-mode)
    (error "Command `icicle-search-ibuffer-marked' must be called from an Ibuffer buffer"))
  (let ((marked-bufs (nreverse (ibuffer-get-marked-buffers))))
    (unless marked-bufs (setq marked-bufs  (list (ibuffer-current-buffer t))))
    (apply #'icicle-search nil nil scan-fn-or-regexp require-match marked-bufs args)))

;;;###autoload (autoload 'icicle-search-buff-menu-marked "icicles")
(defun icicle-search-buff-menu-marked (scan-fn-or-regexp require-match ; Bound to `M-s M-s m' in buff menu
                                       &rest args) ; Bound also to `C-0 M-s M-s M-s', `C-0 C-`' there.
  "Search the marked buffers in Buffer Menu, in order.
Same as using `C-0' with `icicle-search' in `*Buffer List*'. 
Arguments are the same as for `icicle-search', but without arguments
BEG, END, and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-show-multi-completion-flag)))
  (unless (eq major-mode 'Buffer-menu-mode)
    (error "Command `icicle-search-buff-menu-marked' must be called from a Buffer Menu buffer"))
  (let ((marked-bufs  ()))
    (save-excursion
      (Buffer-menu-beginning)
      (while (re-search-forward "^>" nil t) (push (Buffer-menu-buffer t) marked-bufs)))
    (setq marked-bufs  (nreverse marked-bufs))
    (unless marked-bufs (setq marked-bufs  (list (Buffer-menu-buffer t))))
    (apply #'icicle-search nil nil scan-fn-or-regexp require-match marked-bufs args)))

;;;###autoload (autoload 'icicle-search-lines "icicles")
(defalias 'icicle-search-lines 'icicle-occur) ; Bound to `M-s M-s l'.
;;;###autoload (autoload 'icicle-occur "icicles")
(defun icicle-occur (beg end &optional where) ; Bound to `M-s M-s o', `C-c ''.
  "`icicle-search' with a regexp of \".*\".  An `occur' with icompletion.
Type a regexp to match within each line of one or more buffers, files,
or bookmarks.

Use `S-TAB' to show matching lines.  Use `C-RET' or `C-mouse-2' to go
to the line of the current candidate.  Use `C-down', `C-up', `C-next',
`C-prior', `C-end', or `C-home', to cycle among the matching lines.

If you use this command with a prefix argument then multiple buffers,
files, or bookmarks are used (see `icicle-search' for information
about prefix arg behavior).

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits) ,(icicle-search-where-arg)))
  (let ((icicle-multi-completing-p  (and current-prefix-arg
                                         (not (zerop (prefix-numeric-value current-prefix-arg)))
                                         icicle-show-multi-completion-flag))
        (fg (face-foreground        'icicle-search-main-regexp-others))
        (bg (face-background        'icicle-search-main-regexp-others))
        (icicle-transform-function  (if (interactive-p) nil icicle-transform-function)))
    (unwind-protect
         (progn (set-face-foreground 'icicle-search-main-regexp-others nil)
                (set-face-background 'icicle-search-main-regexp-others nil)
                (icicle-search beg end ".*" (not icicle-show-multi-completion-flag) where))
      (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

;;;###autoload (autoload 'icicle-search-sentences "icicles")
(defun icicle-search-sentences (beg end &optional where) ; Bound to `M-s M-s s'.
  "`icicle-search' with sentences as contexts.
Type a regexp to match within each sentence of one or more buffers,
files, or bookmarks.

Use `S-TAB' to show matching sentences.  Use `C-RET' or `C-mouse-2' to
go to the line of the current candidate.  Use `C-down', `C-up',
`C-next', `C-prior', `C-end', or `C-home' to cycle among the matching
sentences.

If you use this command with a prefix argument then multiple buffers,
files, or bookmarks are used (see `icicle-search' for information
about prefix arg behavior).

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

You can alternatively choose to search, not the search contexts
\(sentences), but the non-sentences, that is, the text in the buffer
that is outside sentences.  To do this, use `C-M-~' during completion.
\(This is a toggle, and it affects only future search commands, not
the current one.)

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits) ,(icicle-search-where-arg)))
  (let ((icicle-multi-completing-p  (and current-prefix-arg
                                         (not (zerop (prefix-numeric-value current-prefix-arg)))
                                         icicle-show-multi-completion-flag))
        (fg (face-foreground        'icicle-search-main-regexp-others))
        (bg (face-background        'icicle-search-main-regexp-others))
        (icicle-transform-function  (if (interactive-p) nil icicle-transform-function)))
    (unwind-protect
         (progn (set-face-foreground 'icicle-search-main-regexp-others nil)
                (set-face-background 'icicle-search-main-regexp-others nil)
                (icicle-search beg end (concat "[A-Z][^.?!]+[.?!]")
                               (not icicle-show-multi-completion-flag) where))
      (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

;;;###autoload (autoload 'icicle-search-paragraphs "icicles")
(defun icicle-search-paragraphs (beg end &optional where) ; Bound to `M-s M-s p'.
  "`icicle-search' with paragraphs as contexts.
Type a regexp to match within each paragraph of one or more buffers,
files, or bookmarks.

Use `S-TAB' to show matching paragraphs.  Use `C-RET' or `C-mouse-2'
to go to the line of the current candidate.  Use `C-down', `C-up',
`C-next', `C-prior', `C-end', or `C-home' to cycle among the matching
paragraphs.

If you use this command with a prefix argument then multiple buffers,
files, or bookmarks are used (see `icicle-search' for information
about prefix arg behavior).

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits) ,(icicle-search-where-arg)))
  (let ((icicle-multi-completing-p  (and current-prefix-arg
                                         (not (zerop (prefix-numeric-value current-prefix-arg)))
                                         icicle-show-multi-completion-flag))
        (fg (face-foreground        'icicle-search-main-regexp-others))
        (bg (face-background        'icicle-search-main-regexp-others))
        (icicle-transform-function  (if (interactive-p) nil icicle-transform-function)))
    (unwind-protect
         (progn (set-face-foreground 'icicle-search-main-regexp-others nil)
                (set-face-background 'icicle-search-main-regexp-others nil)
                (icicle-search beg end "\\(.+\n\\)+" (not icicle-show-multi-completion-flag) where))
      (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

;;;###autoload (autoload 'icicle-search-pages "icicles")
(defun icicle-search-pages (beg end &optional where) ; Bound to `M-s M-s C-l'.
  "`icicle-search' with pages as contexts.
Type a regexp to match within each page of one or more buffers, files,
or bookmarks.

Use `S-TAB' to show matching page.  Use `C-RET' or `C-mouse-2' to go
to the line of the current candidate.  Use `C-down', `C-up', `C-next',
`C-prior', `C-end', or `C-home', to cycle among the matching pages.

If you use this command with a prefix argument then multiple buffers,
files, or bookmarks are used (see `icicle-search' for information
about prefix arg behavior).

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits) ,(icicle-search-where-arg)))
  (let ((icicle-multi-completing-p  (and current-prefix-arg
                                         (not (zerop (prefix-numeric-value current-prefix-arg)))
                                         icicle-show-multi-completion-flag))
        (fg (face-foreground        'icicle-search-main-regexp-others))
        (bg (face-background        'icicle-search-main-regexp-others))
        (icicle-transform-function  (if (interactive-p) nil icicle-transform-function)))
    (unwind-protect
         (progn (set-face-foreground 'icicle-search-main-regexp-others nil)
                (set-face-background 'icicle-search-main-regexp-others nil)
                (icicle-search beg end "\\([^\f]*[\f]\\|[^\f]+$\\)"
                               (not icicle-show-multi-completion-flag) where))
      (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

;;;###autoload (autoload 'icicle-comint-search "icicles")
(defun icicle-comint-search (beg end)   ; Bound to `M-s M-s M-s', `C-c `' in `comint-mode'.
  "Use `icicle-search' to pick up a previous input for reuse.
Use this in a `comint-mode' buffer, such as *shell* or
*inferior-lisp*.  This searches your interactive history in the buffer
for a match to your current input, which you can change dynamically.
When you choose a previous input, it is copied to the current prompt,
for reuse.  If the region is active, then only it is searched;
otherwise, the entire buffer is searched.

Use `C-RET' or `C-mouse-2' to choose a previous input for reuse.  Use
`down', `up', `next', `prior', `end', or `home' to cycle among your
previous inputs.  (You probably do NOT want to use `C-next' etc.,
since such keys will not only cycle to another candidate but also
reuse it immediately.)

As for other Icicles search commands, your current input narrows the
set of possible candidates.  See `icicle-search' for more
information.

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

Note that previous commands are identified by looking through the
shell buffer for a shell prompt.  This is not foolproof.  If, for
instance you use command `ls', the output includes an auto-save file
such as #foo.el#, and `#' in the first column represents a shell
prompt, then #foo.el# will be misinterpreted as a previous command.

Also, depending on your shell, you might want to customize variables
such as the following:

`shell-prompt-pattern',`telnet-prompt-pattern'.

Being a search command, `icicle-comint-search' cannot give you access
to previous shell commands that are not visible in the current buffer.
See also \\<comint-mode-map>\\[icicle-comint-command] for another way to reuse commands,
including those from previous sessions.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments, see the doc for command
`icicle-search'."
  ;; $$$$$$ It would be good to somehow rebind C-next etc. to just what next etc. does.
  (interactive (icicle-region-or-buffer-limits))
  ;; Is there a better test we can use, to make sure the current mode inherits from `comint-mode'?
  (unless (where-is-internal 'comint-send-input (keymap-parent (current-local-map)))
    (error "Current mode must be derived from comint mode"))
  (let ((orig-search-hook           icicle-search-hook)
        (icicle-transform-function  'icicle-remove-duplicates))
    (add-hook 'icicle-search-hook 'icicle-comint-search-send-input)
    (unwind-protect
         (icicle-search beg end (concat comint-prompt-regexp "\\S-.*") nil) ; Match not required (edit).
      (remove-hook 'icicle-search-hook 'icicle-comint-search-send-input)))
  (goto-char (point-max))
  (unless (pos-visible-in-window-p) (recenter icicle-recenter)))

(defun icicle-comint-search-send-input ()
  "Grab current completion input and use that for comint input."
  (unless (comint-check-proc (current-buffer))
    (error "No live process associated with this buffer"))
  (let ((comint-get-old-input
         (if (minibuffer-window-active-p (minibuffer-window))
             'icicle-comint-search-get-minibuffer-input ; Use minibuffer input (e.g. for action fn).
           'icicle-comint-search-get-final-choice))) ; Use final choice.
    (comint-copy-old-input))
  (comint-send-input))

(defun icicle-comint-search-get-minibuffer-input ()
  "Return the minibuffer input, beyond the prompt."
  (let* ((cand         (icicle-minibuf-input))
         (input-start  (and (string-match comint-prompt-regexp cand)  (match-end 0))))
    (if input-start (substring cand input-start) cand)))

(defun icicle-comint-search-get-final-choice ()
  "Return the final choice, beyond the prompt."
  (let ((input-start  (and (string-match comint-prompt-regexp icicle-explore-final-choice)  (match-end 0))))
    (if input-start
        (substring icicle-explore-final-choice input-start)
      icicle-explore-final-choice)))

;;;###autoload (autoload 'icicle-comint-command "icicles")
(icicle-define-command icicle-comint-command ; Bound to `C-c TAB' in `comint-mode'.
  "Retrieve a previously used command.
Use this in a `comint-mode' buffer such as `*shell*' or
`*inferior-lisp*'.

Note, depending on your shell, you might want to customize variables
such as the following:

`shell-prompt-pattern',`telnet-prompt-pattern'.

See also \\<comint-mode-map>\\[icicle-comint-search] for another way to reuse commands." ; Doc string
  insert                                ; Action function
  "Choose a previous command: "         ; `completing-read' args
  (mapcar #'list (cddr comint-input-ring)) nil nil nil 'shell-command-history
  (aref (cddr comint-input-ring) 0) nil
  ((icicle-transform-function  'icicle-remove-duplicates))) ; Bindings

(defun icicle-comint-hook-fn ()
  "Hook to set up Comint mode for Icicles."
  (set (make-local-variable 'icicle-search-command) 'icicle-comint-search))

;;;###autoload (autoload 'icicle-compilation-search "icicles")
(defun icicle-compilation-search (beg end) ; Bound to `M-s M-s M-s, `C-c `' in `compilation(-minor)-mode'.
  "Like `icicle-search', but show the matching compilation-buffer hit.
Use this in a compilation buffer, such as `*grep*', searching for a
regexp as with `icicle-search'.  Use `C-RET' or `C-mouse-2' to show
the target-buffer hit corresponding to the current completion
candidate.  Use `C-down', `C-up', `C-next', `C-prior', `C-end', or
`C-home' to cycle among the target-buffer hits.

As for `icicle-search', you can further narrow the match candidates by
typing a second regexp to search for among the first matches.  See
`icicle-search' for more information.

Altogether, using this with `grep' gives you two or three levels of
regexp searching: 1) the `grep' regexp, 2) the major `icicle-search'
regexp, and optionally 3) the refining `icicle-search' regexp.

In Emacs 22 and later, you can replace search-hit text, as in
`icicle-search'.  In earlier Emacs versions, you cannot replace text.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments, see the doc for command
`icicle-search'."
  (interactive (icicle-region-or-buffer-limits))
  (unless (condition-case nil (eq (current-buffer) (compilation-find-buffer)) (error nil))
    (error "Current buffer must be a compilation buffer"))
  (save-excursion (goto-char (point-min))
                  (compilation-next-error 1)
                  (setq beg  (if beg (max beg (point)) (point))))
  (let ((icicle-transform-function    (if (interactive-p) nil icicle-transform-function))
        (icicle-candidate-alt-action-fn
         (if (boundp 'compilation-highlight-overlay) ; Emacs 22 test.
             icicle-candidate-alt-action-fn
           (lambda (cand)
             (message "Cannot replace matching text in Emacs before version 22"))))
        (next-error-highlight
         ;; Highlight indefinitely.  `until-move' should be part of Emacs (patch sent), but it's not.
         (if (and (featurep 'compile+)  (featurep 'simple+)) 'until-move 1000000))
        (icicle-search-in-context-fn  'icicle-compilation-search-in-context-fn)
        (fg (face-foreground          'icicle-search-main-regexp-others))
        (bg (face-background          'icicle-search-main-regexp-others)))
    (unwind-protect
         (progn
           (set-face-foreground 'icicle-search-main-regexp-others nil)
           (set-face-background 'icicle-search-main-regexp-others nil)
           (icicle-search beg end ".*" t))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

(defun icicle-compilation-search-in-context-fn (cand+mrker replace-string)
  "`icicle-search-in-context-fn' used for `icicle-compilation-search'.
If `crosshairs.el' is loaded, then the target position is highlighted."
  (if (not (fboundp 'compilation-next-error-function))
      (compile-goto-error)              ; Emacs 20, 21.
    (setq compilation-current-error  (point)) ; Emacs 22+.
    (compilation-next-error-function 0 nil))
  (save-excursion
    (save-restriction
      (let ((inhibit-field-text-motion  t)) ; Just to be sure, for `line-end-position'.
        (narrow-to-region (line-beginning-position) (line-end-position)))
      (icicle-search-highlight-and-maybe-replace cand+mrker replace-string)))
  (when (fboundp 'crosshairs-highlight) (crosshairs-highlight 'line-only 'nomsg))
  (let ((icicle-candidate-nb  icicle-candidate-nb)) (icicle-complete-again-update)))

(defun icicle-compilation-hook-fn ()
  "Hook setting `icicle-search-command' for compilation modes.
Used on `compilation-mode-hook' and `compilation-minor-mode-hook'."
  (set (make-local-variable 'icicle-search-command) 'icicle-compilation-search))

;;;###autoload (autoload 'icicle-search-w-isearch-string "icicles")
(defun icicle-search-w-isearch-string (&optional use-context-p) ; Bound to `S-TAB' in Isearch.
  "Icicles-search the buffer using an Isearch string chosen by completion.
The Isearch string you choose is used as the Icicles search context.
You can navigate among its occurrences or search within those
occurrences for a subpattern.

For Emacs 22 and later, if option `isearch-allow-scroll' is non-nil
then a prefix argument changes the behavior, as follows:

1. You are prompted for an Icicles search-context regexp.
2. You choose an Isearch string using completion.  It is copied to the
   `kill-ring'.
3. You can yank that string anytime during Icicles search, to search
   for it within the search contexts defined by the regexp matches."
  (interactive "P")
  (isearch-done)
  (if (or (not use-context-p)  (not (boundp 'isearch-allow-scroll)))
      (icicle-search (point-min) (point-max) (icicle-isearch-complete-past-string) t)
    (let ((regexp  (icicle-search-read-context-regexp)))
      (kill-new (icicle-isearch-complete-past-string))
      (icicle-search (point-min) (point-max) regexp t))))

;;;###autoload (autoload 'icicle-search-defs "icicles")
(defalias 'icicle-search-defs 'icicle-imenu) ; Bound to `M-s M-s d'.
;;;###autoload (autoload 'icicle-imenu "icicles")
(defun icicle-imenu (beg end require-match &optional where) ; Bound to `M-s M-s i', `C-c ='.
  "Search/go to an Imenu entry using `icicle-search'.
Recommended: Use library `imenu+.el' also.
In Emacs-Lisp mode, `imenu+.el' classifies definitions using these
submenus:

 1. Keys         - keys in the global keymap
 2. Keys in Maps - keys in keymaps other than global keymap
 3. Functions    - functions, whether interactive or not
 4. Macros       - macros defined with `defmacro'
 5. User Options - user variables, from `defcustom'
 6. Variables    - other variables (non-options), from `defvar'
 7. Faces        - faces, from `defface'
 8. Other        - other definitions

If you use this command with a prefix argument then multiple buffers,
files, or bookmarks are used (see `icicle-search' for information
about prefix arg behavior).  When this is the case, the Imenu mode
\(and `imenu-generic-expression') of the current buffer at time of
command invocation determines what kinds of definitions are found.
So, if you want to search for definitions in a certain language, then
invoke this command from a buffer in that language.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'.

See also these type-specific Icicles Imenu multi-commands:

* `icicle-imenu-command' - search/go to Emacs command definitions
* `icicle-imenu-non-interactive-function' - non-command Lisp fn defs
* `icicle-imenu-macro' - search/go to Lisp macro definitions
* `icicle-imenu-user-option' - search/go to user option definitions
* `icicle-imenu-variable' - search/go to Lisp variable definitions
* `icicle-imenu-face' - search/go to Emacs face definitions
* `icicle-imenu-key-explicit-map', `icicle-imenu-key-implicit-map'
  - search/go to Emacs key definitions

In addition, there are commands like each of the Imenu commands
mentioned above, but with the suffix `-full'.  These commands use
\"full\" definitions as completion candidates, rather than using only
whatever the buffer's Imenu regexps matches.

A \"full\" candidate is obtained by first matching the Imenu regexp,
then moving forward one sexp from the match beginning.  For a Lisp
function, for instance, the regexp match starts at the `defun' sexp's
opening parenthesis, and the full candidate is the entire `defun'
sexp.

Outside of Lisp, \"full\" does not always mean that the candidate is
larger.  Example: a C-language procedure/function definition.  The
text between the regexp match beginning and `forward-sexp' is just the
procedure name."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (icicle-imenu-1 nil beg end require-match where))

;;;###autoload (autoload 'icicle-search-defs-full "icicles")
(defalias 'icicle-search-defs-full 'icicle-imenu-full) ; Bound to `M-s M-s D'.
;;;###autoload (autoload 'icicle-imenu-full "icicles")
(defun icicle-imenu-full (beg end require-match &optional where) ; Bound to `M-s M-s I'.
  "Search/go to an Imenu entry using `icicle-search'.
Same as `icicle-imenu', except candidates are full definitions.
This really means that a candidate is the text between the beginning
of the Imenu regexp match and `forward-sexp' from there.

Remember that non-nil option `icicle-hide-non-matching-lines-flag'
hides, in `*Completions*', all lines of multi-line candidates that do
not match your current minibuffer input.  You can toggle this at
anytime during completion using `C-u C-x .'"
  ;; Note: For this command, the candidate does not correspond to the regexp match.
  ;; Instead, it corresponds to that match plus text at the end to complete the definition.
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (icicle-imenu-1 'FULL beg end require-match where))

;;;###autoload (autoload 'icicle-imenu-command "icicles")
(defun icicle-imenu-command (beg end require-match &optional where)
  "Search/go to an Emacs command definition using `icicle-search'.
This uses `commandp', so it finds only currently defined commands.
That is, if the buffer has not been evaluated, then its function
definitions are NOT considered commands by `icicle-imenu-command'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode"))
  (icicle-imenu-1 nil beg end require-match where 'icicle-imenu-command-p
                  (lambda (menus)
                    (or (car (assoc "Functions" menus))
                        (car (assoc "Other" menus))
                        (error "No command definitions in buffer")))))

;;;###autoload (autoload 'icicle-imenu-command-full "icicles")
(defun icicle-imenu-command-full (beg end require-match &optional where)
  "Search/go to an Emacs command definition using `icicle-search'.
Same as `icicle-imenu-command', except candidates are complete command
definitions.

Remember that non-nil option `icicle-hide-non-matching-lines-flag'
hides, in `*Completions*', all lines of multi-line candidates that do
not match your current minibuffer input.  You can toggle this at
anytime during completion using `C-u C-x .'"
  ;; Note: For this command, the candidate does not correspond to the regexp match.
  ;; Instead, it corresponds to that match plus text at the end to complete the definition.
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode"))
  (icicle-imenu-1 (eq major-mode 'emacs-lisp-mode) beg end require-match where
                  'icicle-imenu-command-p
                  (lambda (menus)
                    (or (car (assoc "Functions" menus))
                        (car (assoc "Other" menus))
                        (error "No command definitions in buffer")))))

(defun icicle-imenu-command-p (ignored-hit-string ignored-marker)
  "Return non-nil for a command definition.
Predicate for `icicle-search'.
Both arguments are ignored here."
  (let ((indx  (if (< emacs-major-version 21) 6 2)))
    (commandp (intern-soft
               (buffer-substring-no-properties (match-beginning indx) (match-end indx))))))

;;;###autoload (autoload 'icicle-imenu-non-interactive-function "icicles")
(defun icicle-imenu-non-interactive-function (beg end require-match &optional where)
  "Search/go to an Emacs non-command function definition with `icicle-search'.
This uses `commandp' to distinguish currently defined commands from
other functions.  This means that if the buffer has not yet been
evaluated, then ALL of its function definitions are considered
non-interactive by `icicle-imenu-non-interactive-function'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (memq major-mode '(emacs-lisp-mode lisp-mode)))
    (error "This command is only for Emacs-Lisp mode"))
  (icicle-imenu-1 nil beg end require-match where 'icicle-imenu-non-interactive-function-p
                  (lambda (menus)
                    (or (car (assoc "Functions" menus))
                        (car (assoc "Other" menus))
                        (error "No non-command function definitions in buffer")))))

;;;###autoload (autoload 'icicle-imenu-non-interactive-function-full "icicles")
(defun icicle-imenu-non-interactive-function-full (beg end require-match &optional where)
  "Search/go to an Emacs non-command function definition with `icicle-search'.
Same as `icicle-imenu-non-interactive-function', except candidates are
complete function definitions.

Remember that non-nil option `icicle-hide-non-matching-lines-flag'
hides, in `*Completions*', all lines of multi-line candidates that do
not match your current minibuffer input.  You can toggle this at
anytime during completion using `C-u C-x .'"
  ;; Note: For this command, the candidate does not correspond to the regexp match.
  ;; Instead, it corresponds to that match plus text at the end to complete the definition.
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (memq major-mode '(emacs-lisp-mode lisp-mode)))
    (error "This command is only for Emacs-Lisp mode"))
  (icicle-imenu-1 (memq major-mode '(emacs-lisp-mode lisp-mode)) beg end require-match where
                  'icicle-imenu-non-interactive-function-p
                  (lambda (menus)
                    (or (car (assoc "Functions" menus))
                        (car (assoc "Other" menus))
                        (error "No non-command function definitions in buffer")))))

(defun icicle-imenu-non-interactive-function-p (ignored-hit-string ignored-marker)
  "Return non-nil for a non-interactive function definition.
Predicate for `icicle-search'.  Both arguments are ignored."
  (let* ((indx  (if (< emacs-major-version 21) 6 2))
         (fn    (intern-soft
                 (buffer-substring-no-properties (match-beginning indx) (match-end indx)))))
    (and (fboundp fn)  (not (commandp fn)))))

;;;###autoload (autoload 'icicle-imenu-macro "icicles")
(defun icicle-imenu-macro (beg end require-match &optional where)
  "Search/go to an Emacs macro definition using `icicle-search'.
This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (memq major-mode '(emacs-lisp-mode lisp-mode)))
    (error "This command is only for Emacs-Lisp mode or Lisp mode"))
  (icicle-imenu-1 nil beg end require-match where 'icicle-imenu-macro-p
                  (lambda (menus)
                    (or (car (assoc "Macros" menus))
                        (car (assoc "Other" menus))
                        (error "No macro definitions in buffer")))))

;;;###autoload (autoload 'icicle-imenu-macro-full "icicles")
(defun icicle-imenu-macro-full (beg end require-match &optional where)
  "Search/go to an Emacs macro definition using `icicle-search'.
Same as `icicle-imenu-non-interactive-function', except candidates are
complete function definitions.

Remember that non-nil option `icicle-hide-non-matching-lines-flag'
hides, in `*Completions*', all lines of multi-line candidates that do
not match your current minibuffer input.  You can toggle this at
anytime during completion using `C-u C-x .'"
  ;; Note: For this command, the candidate does not correspond to the regexp match.
  ;; Instead, it corresponds to that match plus text at the end to complete the definition.
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (memq major-mode '(emacs-lisp-mode lisp-mode)))
    (error "This command is only for Emacs-Lisp mode"))
  (icicle-imenu-1 (memq major-mode '(emacs-lisp-mode lisp-mode)) beg end require-match where
                  'icicle-imenu-macro-p
                  (lambda (menus)
                    (or (car (assoc "Macro" menus))
                        (car (assoc "Other" menus))
                        (error "No macro definitions in buffer")))))

;;;###autoload (autoload 'icicle-imenu-variable "icicles")
(defun icicle-imenu-variable (beg end require-match &optional where)
  "Search/go to an Emacs non-option variable definition using `icicle-search'.
This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (memq major-mode '(emacs-lisp-mode lisp-mode)))
    (error "This command is only for Emacs-Lisp mode or Lisp mode"))
  (icicle-imenu-1 nil beg end require-match where nil
                  (lambda (menus)
                    (or (car (assoc "Variables" menus))
                        (car (assoc "Other" menus))
                        (error "No non-option variable definitions in buffer")))))

;;;###autoload (autoload 'icicle-imenu-variable-full "icicles")
(defun icicle-imenu-variable-full (beg end require-match &optional where)
  "Search/go to an Emacs non-option variable definition using `icicle-search'.
Same as `icicle-imenu-variable', except candidates are complete
variable definitions.

Remember that non-nil option `icicle-hide-non-matching-lines-flag'
hides, in `*Completions*', all lines of multi-line candidates that do
not match your current minibuffer input.  You can toggle this at
anytime during completion using `C-u C-x .'"
  ;; Note: For this command, the candidate does not correspond to the regexp match.
  ;; Instead, it corresponds to that match plus text at the end to complete the definition.
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (memq major-mode '(emacs-lisp-mode lisp-mode)))
    (error "This command is only for Emacs-Lisp mode or Lisp mode"))
  (icicle-imenu-1 (memq major-mode '(emacs-lisp-mode lisp-mode)) beg end require-match where nil
                  (lambda (menus)
                    (or (car (assoc "Variables" menus))
                        (car (assoc "Other" menus))
                        (error "No non-option variable definitions in buffer")))))

;;;###autoload (autoload 'icicle-imenu-user-option "icicles")
(defun icicle-imenu-user-option (beg end require-match &optional where)
  "Search/go to an Emacs user option definition using `icicle-search'.
This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode"))
  (icicle-imenu-1 nil beg end require-match where nil
                  (lambda (menus)
                    (or (car (assoc "User Options" menus))
                        (car (assoc "Other" menus))
                        (error "No user option definitions in buffer")))))

;;;###autoload (autoload 'icicle-imenu-user-option-full "icicles")
(defun icicle-imenu-user-option-full (beg end require-match &optional where)
  "Search/go to an Emacs user option definition using `icicle-search'.
Same as `icicle-imenu-user-option', except candidates are complete
option definitions.

Remember that non-nil option `icicle-hide-non-matching-lines-flag'
hides, in `*Completions*', all lines of multi-line candidates that do
not match your current minibuffer input.  You can toggle this at
anytime during completion using `C-u C-x .'"
  ;; Note: For this command, the candidate does not correspond to the regexp match.
  ;; Instead, it corresponds to that match plus text at the end to complete the definition.
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode"))
  (icicle-imenu-1 (eq major-mode 'emacs-lisp-mode) beg end require-match where nil
                  (lambda (menus)
                    (or (car (assoc "User Options" menus))
                        (car (assoc "Other" menus))
                        (error "No user option definitions in buffer")))))

;;;###autoload (autoload 'icicle-imenu-key-implicit-map "icicles")
(defun icicle-imenu-key-implicit-map (beg end require-match &optional where)
  "Search/go to a global/local Emacs key definition using `icicle-search'.
This means a definition where no key map is specified explicitly -
e.g., `global-set-key'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode"))
  (icicle-imenu-1 nil beg end require-match where nil
                  (lambda (menus)
                    (or (car (assoc "Keys" menus))
                        (car (assoc "Other" menus))
                        (error "No implicit-map key definitions in buffer")))))

;;;###autoload (autoload 'icicle-imenu-key-implicit-map-full "icicles")
(defun icicle-imenu-key-implicit-map-full (beg end require-match &optional where)
  "Search/go to a global/local Emacs key definition using `icicle-search'.
Same as `icicle-imenu-key-implicit-map', except candidates are complete key
definitions."
  ;; Note: For this command, the candidate does not correspond to the regexp match.
  ;; Instead, it corresponds to that match plus text at the end to complete the definition.
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode"))
  (icicle-imenu-1 (eq major-mode 'emacs-lisp-mode) beg end require-match where nil
                  (lambda (menus)
                    (or (car (assoc "Keys" menus))
                        (car (assoc "Other" menus))
                        (error "No implicit-map key definitions in buffer")))))

;;;###autoload (autoload 'icicle-imenu-key-explicit-map "icicles")
(defun icicle-imenu-key-explicit-map (beg end require-match &optional where)
  "Search/go to an Emacs key definition for a named map using `icicle-search'.
This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode"))
  (icicle-imenu-1 nil beg end require-match where nil
                  (lambda (menus)
                    (or (car (assoc "Keys in Maps" menus))
                        (car (assoc "Other" menus))
                        (error "No explicit-map key definitions in buffer")))))

;;;###autoload (autoload 'icicle-imenu-key-explicit-map-full "icicles")
(defun icicle-imenu-key-explicit-map-full (beg end require-match &optional where)
  "Search/go to an Emacs key definition for a named map using `icicle-search'.
Same as `icicle-imenu-key-explicit-map', except candidates are
complete key definitions."
  ;; Note: For this command, the candidate does not correspond to the regexp match.
  ;; Instead, it corresponds to that match plus text at the end to complete the definition.
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode"))
  (icicle-imenu-1 (eq major-mode 'emacs-lisp-mode) beg end require-match where nil
                  (lambda (menus)
                    (or (car (assoc "Keys in Maps" menus))
                        (car (assoc "Other" menus))
                        (error "No explicit-map key definitions in buffer")))))

;;;###autoload (autoload 'icicle-imenu-face "icicles")
(defun icicle-imenu-face (beg end require-match &optional where)
  "Search/go to an Emacs face definition using `icicle-search'.
This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode"))
  (icicle-imenu-1 nil beg end require-match where nil
                  (lambda (menus)
                    (or (car (assoc "Faces" menus))
                        (car (assoc "Other" menus))
                        (error "No face definitions in buffer")))))

;;;###autoload (autoload 'icicle-imenu-face-full "icicles")
(defun icicle-imenu-face-full (beg end require-match &optional where)
  "Search/go to an Emacs face definition using `icicle-search'.
Same as `icicle-imenu-face', except candidates are complete face
definitions."
  ;; Note: For this command, the candidate does not correspond to the regexp match.
  ;; Instead, it corresponds to that match plus text at the end to complete the definition.
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where  (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode"))
  (icicle-imenu-1 (eq major-mode 'emacs-lisp-mode) beg end require-match where nil
                  (lambda (menus)
                    (or (car (assoc "Faces" menus))
                        (car (assoc "Other" menus))
                        (error "No face definitions in buffer")))))

(defun icicle-imenu-1 (fullp beg end require-match &optional where predicate submenu-fn)
  "Helper for `icicle-imenu*' commands.
Non-nil FULLP means candidate is from the regexp match beginning
 through `forward-sexp' from there.
Optional arg SUBMENU-FN is a function to apply to the list of Imenu
 submenus to choose one.  If nil then the user chooses one using
 completion.
The other args are as for `icicle-search'."
  (unless imenu-generic-expression (error "No Imenu pattern for this buffer"))
  (let ((icicle-multi-completing-p  (and current-prefix-arg
                                         (not (zerop (prefix-numeric-value current-prefix-arg)))
                                         icicle-show-multi-completion-flag))
        (case-fold-search           (if (or (local-variable-p 'imenu-case-fold-search)
                                            (not (local-variable-p 'font-lock-defaults)))
                                        imenu-case-fold-search
                                      (nth 2 font-lock-defaults)))
        (old-table                  (syntax-table))
        (table                      (copy-syntax-table (syntax-table)))
        (slist                      imenu-syntax-alist))
    (dolist (syn  slist)                ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))
          (modify-syntax-entry (car syn) (cdr syn) table) ; Single character.
        (dolist (char  (car syn))  (modify-syntax-entry char (cdr syn) table)))) ; String.
    (unwind-protect
         (save-match-data
           (set-syntax-table table)
           (let* ((others   0)
                  (menus    (mapcar (lambda (menu)
                                      (when (equal (car menu) "Other")
                                        (setq others  (1+ others))
                                        (when (> others 1)
                                          (setcar menu (format "Other<%d>" others))))
                                      menu)
                                    (icicle-remove-if-not
                                     #'icicle-imenu-in-buffer-p ; Use only menus that match buffer.
                                     (mapcar (lambda (menu) ; Name unlabeled menu(s) `Other[<N>]'.
                                               (if (stringp (car menu))
                                                   menu
                                                 (cons "Other" (cdr menu))))
                                             imenu-generic-expression))))
                  (submenu  (if submenu-fn
                                (funcall submenu-fn menus)
                              (if (cadr menus)
                                  (let ((icicle-show-Completions-initially-flag  t)
                                        (completion-ignore-case                  t))
                                    (completing-read "Choose: " menus nil t))
                                (caar menus)))) ; Only one submenu, so use it.
                  (regexp   (cadr (assoc submenu menus)))
                  (icicle-transform-function
                   (if (interactive-p) nil icicle-transform-function)))
             (unless (stringp regexp) (error "No match"))
             (icicle-search
              beg end regexp require-match where predicate
              ;; We rely on the match data having been preserved.
              ;; $$$$$$ An alternative fn for Lisp only: (lambda () (up-list -1) (forward-sexp))))))
              (and fullp  (lambda ()
                            (goto-char (match-beginning 0))
                            (condition-case icicle-imenu-1
                                (forward-sexp)
                              (error (goto-char (match-end 0))))))))) ; Punt: just use regexp match.
      (set-syntax-table old-table))))

(defun icicle-imenu-in-buffer-p (menu)
  "Return non-nil if the regexp in MENU has a match in the buffer."
  (save-excursion (goto-char (point-min)) (re-search-forward (cadr menu) nil t)))

;;;###autoload (autoload 'icicle-tags-search "icicles")
(defun icicle-tags-search (regexp &optional arg) ; Bound to `M-s M-s ,'
  "Search all source files listed in tags tables for matches for REGEXP.
You are prompted for the REGEXP to match.  Enter REGEXP with `RET'.
You do not need `M-,' - you see all matches as search hits to visit.

All tags in a tags file are used, including duplicate tags from the
same or different source files.

By default, all tags files are used, but if you provide a prefix
argument then only the current tag table is used.

If your TAGS file references source files that no longer exist, those
files are listed.  In that case, you might want to update your TAGS
file.


You can alternatively choose to search, not the search contexts as
defined by the context regexp you provide, but the non-contexts, that
is, the text in the files that does not match the regexp.  To do this,
use `C-M-~' during completion.  (This is a toggle, and it affects only
future search commands, not the current one.)"
  (interactive
   (let ((completion-ignore-case  (if (and (boundp 'tags-case-fold-search)
                                           (memq tags-case-fold-search '(t nil)))
                                      tags-case-fold-search
                                    case-fold-search)))
     (require 'etags)
     (list (icicle-search-read-context-regexp (format
                                               "Search files with tags %smatching regexp: "
                                               (if icicle-search-complement-domain-p "*NOT* " "")))
           current-prefix-arg)))
  (let ((files  ()))
    (save-excursion
      (let ((first-time  t)
            (morep       t))
        (while (and morep  (visit-tags-table-buffer (not first-time)))
          (when arg (setq morep  nil))
          (setq first-time  nil)
          (let ((tail  (last files)))
            (if tail
                (setcdr tail (mapcar 'expand-file-name (tags-table-files)))
              (setq files  (mapcar 'expand-file-name (tags-table-files))))))))
    (let ((tail              files)     ; Remove names of non-existent or unreadable files.
          (unreadable-files  ()))
      (while tail
        (if (file-readable-p (car tail))
            (setq tail  (cdr tail))
          (push (car tail) unreadable-files)
          (setcar tail (cadr tail))
          (setcdr tail (cddr tail))))
      (when unreadable-files
        (with-output-to-temp-buffer "*Unreadable Files*"
          (princ "These missing or unreadable files were ignored:") (terpri) (terpri)
          (dolist (file  unreadable-files) (princ file) (terpri)))))
    (select-window (minibuffer-window))
    (select-frame-set-input-focus (selected-frame))
    (icicle-search nil nil regexp nil files)))

;;;###autoload (autoload 'icicle-save-string-to-variable "icicles")
(defun icicle-save-string-to-variable (askp)
  "Save a string (text) to a variable.
You are prompted for the string to save.  Typically, you store a
regexp or part of a regexp in the variable.

By default, the variable is user option `icicle-input-string'.
To save to a different variable, use a prefix argument; you are then
prompted for the variable to use.

You can use `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]' to insert a string from a
variable."
  (interactive "P")
  (let* ((enable-recursive-minibuffers  t)
         (pred                                    (lambda (s)
                                                    (unless (symbolp s) (setq s  (intern s)))
                                                    (boundp s)))
         (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
         (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
         (var
          (if askp
              (let ((icicle-candidate-alt-action-fn
                     (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "variable")))
                    (icicle-all-candidates-list-alt-action-fn
                     (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "variable"))))
                (intern (completing-read "Variable: " obarray (and icompletep  pred) nil nil
                                         (if (boundp 'variable-name-history)
                                             'variable-name-history
                                           'icicle-variable-name-history)
                                         (symbol-name 'icicle-input-string))))
            'icicle-input-string))
         (text                          (icicle-completing-read-history
                                         (format "Text to save in `%s': " var))))
    (set var text)))

(when (> emacs-major-version 21)

  (when (and icicle-define-alias-commands-flag  (not (fboundp 'any)))
    (defalias 'any 'icicle-anything))

  (defun icicle-anything (type)
    "Act on an object of type TYPE.
You are prompted for the type, then for an object of that type.  The
type is either the declared `type' of an Anything source, or its
`name' if it has no `type'.

This command is available only if you use library `anything.el'.

This is an Icicles multi-command: You can act on multiple objects in
multiple ways during a single command invocation.  When you choose an
object using `RET' or `mouse-2', the default action is applied to it.
The default action is also applied to the current completion candidate
when you use `C-RET', `C-mouse-2', and so on.

You can apply a different action by using an alternative action key\\<minibuffer-local-completion-map>:
`\\[icicle-candidate-alt-action]', `C-S-mouse-2', and so on.  This lets you choose the action
to apply using completion. You can use `C-RET', `C-mouse-2', and so
on, to perform multiple actions.

This command is intended for use only in Icicle mode."
    (interactive
     (let ((icicle-show-Completions-initially-flag  t)
           (icicle-whole-candidate-as-text-prop-p   icicle-anything-transform-candidates-flag))
       (unless (require 'anything nil t) (error "You must load library `anything.el' first"))
       (list (intern (completing-read "What (type): " (icicle-remove-duplicates
                                                       (mapcar #'list (icicle-get-anything-types)))
                                      nil t)))))
    (icicle-object-action type)))

(when (and icicle-define-alias-commands-flag  (not (fboundp 'file)))
  (defun file ()
    "Act on a file.  You are prompted for the file and the action.
During file-name completion, you can delete the file named by the
current candidate, using \\<minibuffer-local-completion-map>`\\[icicle-delete-candidate-object]'.

This is just `icicle-object-action' with type `file'."
    (interactive) (icicle-object-action 'file)))

(when (and icicle-define-alias-commands-flag  (not (fboundp 'buffer)))
  (defun buffer ()
    "Act on a buffer.  You are prompted for the buffer and the action.
During buffer-name completion, you can kill the buffer named by the
current candidate, using \\<minibuffer-local-completion-map>`\\[icicle-delete-candidate-object]'.

This is just `icicle-object-action' with type `buffer'."
    (interactive) (icicle-object-action 'buffer)))

(when (and icicle-define-alias-commands-flag  (not (fboundp 'a)))
  (defalias 'a 'icicle-object-action))
(when (and icicle-define-alias-commands-flag  (not (fboundp 'what-which-how)))
  (defalias 'what-which-how 'icicle-object-action))
;;;###autoload (autoload 'icicle-object-action "icicles")
(defun icicle-object-action (&optional type)
  "Act on an object of type TYPE (a symbol).
You are prompted for the type (\"What\"), then for an object of that
type (\"Which\"), then for the action function to apply to the
object (\"How\").  For Anything types (see below), you are not
prompted for the action function.

The \"type\" of an object is one of these:

a. A type defining an entry `icicle-predicate-types-alist'.
   These are type predicates, such as `bufferp', `keywordp', or `atom'.

b. The `type' of an Anything source, or its `name' if it has no
   `type'.  This is available only if you use library `anything.el'
   and option `icicle-use-anything-candidates-flag' is non-nil.

c. A type defining an entry in user option
   `icicle-type-actions-alist'.

In the case of Anything types (only), this is a multi-command\\<minibuffer-local-completion-map>:
* `C-RET', `C-mouse-2', and so on perform the default action.
* `\\[icicle-candidate-alt-action]', `C-S-mouse-2', and so on let you choose the action using
  completion.

Though this is not a multi-command for non-Anything types, for types
`buffer' and `file' you can use `\\[icicle-delete-candidate-object]' during completion to delete
the object (buffer or file) named by the current completion candidate.

Objects of types (b) and (c) are easily associated with names.  Their
names are the completion candidates.  So, for instance, if you choose
type `buffer', then you can act on a buffer by choosing its name.

Objects of predicate type (type a) are not necessarily named.  The
completion candidates for these objects are variables (symbols) whose
values are the objects acted upon.  So, for instance, if you choose
type `bufferp', then you can choose a variable whose value is a
buffer, in order to act on that buffer.  Whereas a buffer is always
named, an object of type `stringp' is not.  The value of variable
`emacs-version' is one such string that you can act on.

Anything types and Anything actions are highlighted when used as
candidates in `*Completions*', using face `icicle-special-candidate'.

Be aware that the action function you choose must accommodate the
object you choose as its only an argument.  Also, completion of the
function candidate itself is not strict, so you can enter a lambda
form.

With a prefix argument, the result of applying the function to the
object is pretty-printed using `icicle-pp-eval-expression'.
Otherwise, the function is called for its effect only, and its value
is not displayed.

You can use a prefix argument similarly when you act on an individual
function (\"How\") candidate to apply it to the object, without ending
completion.  That is, `C-u C-RET', `C-u C-mouse-2', and so on, will
pretty-print the result of the individual action.

This command is intended for use only in Icicle mode."
  (interactive)
  (let* ((anything-loaded-p         (and (> emacs-major-version 21)
                                         icicle-use-anything-candidates-flag
                                         (require 'anything nil t)))
         (anything-types            (and (not type)  anything-loaded-p  (icicle-get-anything-types)))
         (typ
          (or type  (let ((icicle-show-Completions-initially-flag  t))
                      (intern
                       (completing-read "What (type): "
                                        (icicle-remove-duplicates (append (mapcar #'list anything-types)
                                                                          icicle-type-actions-alist
                                                                          icicle-predicate-types-alist))
                                        nil t)))))
         (predicate-type-p          (and (assoc (symbol-name typ) icicle-predicate-types-alist)
                                         (not (memq (symbol-name typ) anything-types))))
         (anything-candidates       (and anything-loaded-p
                                         (not predicate-type-p)
                                         (icicle-get-anything-candidates-of-type typ)))
         (anything-default-actions  (and anything-candidates
                                         (icicle-get-anything-default-actions-for-type typ)))
         (anything-actions          (and anything-candidates
                                         (icicle-get-anything-actions-for-type typ)))
         (icicle-saved-completion-candidate
          (cond (predicate-type-p (icicle-read-var-value-satisfying typ))
                (anything-candidates
                 (icicle-choose-anything-candidate typ anything-candidates
                                                   anything-default-actions anything-actions))
                ((member (symbol-name typ) (and anything-loaded-p  (icicle-get-anything-types)))
                 (error "No candidates for type `%s'" (symbol-name typ)))
                (t (icicle-choose-candidate-of-type typ))))
         (icicle-candidate-action-fn    ; For "how".
          (lambda (fn) (icicle-apply-to-saved-candidate fn anything-candidates typ)))
         (icicle-candidate-alt-action-fn ; For "how".
          (and anything-candidates  (lambda (fn) (icicle-apply-to-saved-candidate fn t typ)))))
    (funcall (icicle-alt-act-fn-for-type
              (if predicate-type-p
                  (or (cdr (assoc (symbol-name typ) icicle-predicate-types-alist))  (symbol-name typ))
                (symbol-name typ)))
             icicle-saved-completion-candidate)))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-types ()
    "Return list of types defined in `anything-sources'.  See `anything.el'."
    (and (boundp 'anything-sources)  (consp anything-sources)
         (let ((types  ())
               type)
           (dolist (source  (anything-get-sources))
             (if (setq type  (assoc-default 'type source))
                 (push (symbol-name type) types)
               (when (setq type  (assoc-default 'name source)) (push type types))))
           (setq types
                 (mapcar (lambda (typ)
                           (setq typ  (copy-sequence typ))
                           (put-text-property 0 (length typ) 'face 'icicle-special-candidate typ)
                           typ)
                         (icicle-remove-duplicates types)))))))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-candidates-of-type (type)
    "Return list of Anything candidates for type TYPE.
Used only when `anything-sources' is non-nil - see `anything.el'."
    (and (boundp 'anything-sources)  (consp anything-sources)
         (let ((anything-candidate-cache  ())
               (candidates                ()))
           (dolist (source  (anything-get-sources))
             (let ((init-fn  (assoc-default 'init source))) (when init-fn (funcall init-fn)))
             (when (or (eq type (assoc-default 'type source))
                       (string= (symbol-name type) (assoc-default 'name source)))
               (setq candidates  (icicle-get-anything-cached-candidates source))))
           (when (and (not (functionp candidates))  (consp candidates))
             (mapcar (lambda (cand) (if (consp cand) cand (list cand))) candidates))
           candidates))))

;; Similar to `anything-get-cached-candidates' in `anything.el', but ignores processes.
;; Free var here: `anything-candidate-cache'.
(when (> emacs-major-version 21)
  (defun icicle-get-anything-cached-candidates (source)
    "Return cached value of candidates for Anything SOURCE.
Cache the candidates if there is not yet a cached value."
    (let* ((source-name      (assoc-default 'name source))
           (candidate-cache  (assoc source-name anything-candidate-cache))
           candidates)
      (if candidate-cache
          (setq candidates  (cdr candidate-cache))
        (setq candidates  (icicle-get-anything-candidates source))
        (when (processp candidates) (setq candidates  ()))
        (setq candidate-cache  (cons source-name candidates))
        (push candidate-cache anything-candidate-cache))
      candidates)))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-candidates (source)
    "Return the list of candidates from Anything SOURCE."
    (let* ((candidate-source  (assoc-default 'candidates source))
           (candidates
            (cond ((functionp candidate-source)
                   `(lambda (string pred mode)
                     (let ((anything-pattern  icicle-current-input))
                       (setq string  anything-pattern)
                       (let ((all-cands  (funcall ,candidate-source)))
                         (setq all-cands
                               (icicle-remove-if-not
                                (lambda (cand)
                                  (string-match (if (eq 'prefix icicle-current-completion-mode)
                                                    (concat "^" (regexp-quote string))
                                                  string)
                                                cand))
                                all-cands))
                         (cond ((eq mode t) all-cands)
                               ((eq mode nil)
                                (icicle-expanded-common-match icicle-current-input all-cands))
                               ((eq mode 'lambda) t))))))
                  ((listp candidate-source)
                   candidate-source)
                  ((and (symbolp candidate-source)  (boundp candidate-source))
                   (symbol-value candidate-source))
                  (t
                   (error
                    (concat "Source `candidates' value is not a function, variable or list: %s")
                    candidate-source)))))
      (if (or (not icicle-anything-transform-candidates-flag)  (processp candidates))
          candidates
        (anything-transform-candidates candidates source)))))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-actions-for-type (type)
    "Set and return `icicle-candidates-alist' of actions for type TYPE.
The display string for each action is highlighted using face
`icicle-special-candidate'."
    (setq icicle-candidates-alist  ())
    (let ((all-sources-actions  ())
          this-source-actions  faced-act)
      (dolist (source  (anything-get-sources))
        (when (or (eq type (assoc-default 'type source))
                  (string= (symbol-name type) (assoc-default 'name source)))
          (setq this-source-actions  (assoc-default 'action source))
          (dolist (action  this-source-actions)
            (unless (member action all-sources-actions)
              (setq faced-act  (copy-sequence (car action))) ; Highlight Anything action.
              (put-text-property 0 (length faced-act) 'face 'icicle-special-candidate faced-act)
              (push (cons faced-act (cdr action)) all-sources-actions)))))
      (setq icicle-candidates-alist  (sort all-sources-actions
                                           (lambda (a1 a2)
                                             (funcall icicle-sort-comparer (car a1) (car a2))))))))
(when (> emacs-major-version 21)
  (defun icicle-choose-anything-candidate (type candidates default-actions actions)
    "Read an Anything object of type TYPE with completion, and return it.
During completion, you can act on selected completion candidates, in
turn, using the action keys (`C-RET', `C-mouse-2', `C-down', etc.).
CANDIDATES is the list of candidates of type TYPE.
DEFAULT-ACTIONS is the list of default actions for type TYPE.
ACTIONS is the list of all actions for type TYPE."
    (let* ((win                                         (selected-window))
           (icicle-sort-comparer                        nil)
           (icicle-transform-function                   nil)
           (icicle-Completions-display-min-input-chars  (icicle-get-anything-req-pat-chars type))
           (icicle-incremental-completion-delay         (icicle-get-anything-input-delay type))
           (icicle-whole-candidate-as-text-prop-p       icicle-anything-transform-candidates-flag)
           (icicle-candidates-alist                     (if (or (functionp candidates)
                                                                icicle-whole-candidate-as-text-prop-p)
                                                            candidates
                                                          icicle-candidates-alist))
           (icicle-candidate-action-fn
            (lambda (obj)
              (when icicle-whole-candidate-as-text-prop-p
                (setq obj  (icicle-anything-candidate-value obj)))
              (let ((enable-recursive-minibuffers  t))
                (with-selected-window win
                  (if (null (cdr default-actions))
                      (funcall (cdar default-actions) obj)
                    (funcall (completing-read "How (action): " default-actions nil t) obj))))
              (select-window (minibuffer-window))
              (select-frame-set-input-focus (selected-frame))
              (icicle-raise-Completions-frame)))
           (icicle-candidate-alt-action-fn
            `(lambda (obj)
              (when icicle-whole-candidate-as-text-prop-p
                (setq obj  (icicle-anything-candidate-value obj)))
              (let ((icicle-show-Completions-initially-flag  t)
                    (icicle-saved-completion-candidate       obj)
                    (icicle-candidates-alist                 actions)
                    (enable-recursive-minibuffers            t))
                (with-selected-window win
                  (icicle-apply-to-saved-candidate
                   (let ((enable-recursive-minibuffers      t)
                         (icicle-last-completion-candidate  icicle-last-completion-candidate)
                         (icicle-candidate-alt-action-fn    nil)
                         (icicle-candidate-action-fn
                          `(lambda (actn) (with-selected-window win
                                            (let ((enable-recursive-minibuffers  t)
                                                  (icicle-candidates-alist       actions))
                                              (icicle-apply-to-saved-candidate actn t ,type))))))
                     (completing-read "How (action): " actions nil t))
                   t
                   ,type)))))
           (orig-action-fn  icicle-candidate-action-fn)
           (icicle-candidate-help-fn
            (if icicle-whole-candidate-as-text-prop-p
                (lambda (obj)
                  (let ((icicle-candidate-help-fn  nil))
                    (icicle-help-on-candidate-symbol
                     (intern (icicle-anything-candidate-value obj)))))
              icicle-candidate-help-fn))
           (icicle-candidate-action-fn
            (if icicle-whole-candidate-as-text-prop-p
                (lambda (obj)
                  (let ((icicle-last-input  (icicle-anything-candidate-value obj)))
                    (funcall orig-action-fn obj)))
              icicle-candidate-action-fn)))
      (if icicle-whole-candidate-as-text-prop-p
          (icicle-anything-candidate-value
           (completing-read (concat "Which (" (symbol-name type) "): ") candidates nil t))
        (completing-read (concat "Which (" (symbol-name type) "): ") candidates nil t)))))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-req-pat-chars (type)
    "Return max `required-pattern' value for sources of type TYPE.
The value returned is also always at least as big as
`icicle-Completions-display-min-input-chars'."
    (let ((req-pat              icicle-Completions-display-min-input-chars)
          (req-pat-this-source  nil))
      (dolist (source  (anything-get-sources))
        (when (and (or (eq type (assoc-default 'type source))
                       (string= (symbol-name type) (assoc-default 'name source)))
                   (setq req-pat-this-source  (assoc-default 'requires-pattern source)))
          (setq req-pat  (max req-pat req-pat-this-source))))
      req-pat)))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-input-delay (type)
    "Return max `delay' value for sources of type TYPE.
The value returned is also always at least as big as
`icicle-incremental-completion-delay'."
    (let ((delay              icicle-incremental-completion-delay)
          (delay-this-source  nil))
      (dolist (source  (anything-get-sources))
        (when (and (or (eq type (assoc-default 'type source))
                       (string= (symbol-name type) (assoc-default 'name source)))
                   (setq delay-this-source  (and (assoc 'delayed source)  anything-idle-delay)))
          (setq delay  (max delay delay-this-source))))
      delay)))

(when (> emacs-major-version 21)
  (defun icicle-anything-candidate-value (candidate)
    "Return the real value associated with string CANDIDATE."
    (or (cdr-safe (funcall icicle-get-alist-candidate-function candidate))  candidate)))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-default-actions-for-type (type)
    "Set and return `icicle-candidates-alist' of default actions for type TYPE."
    (setq icicle-candidates-alist  ())
    (let ((all-sources-actions  ())
          this-source-actions)
      (dolist (source  (anything-get-sources))
        (when (or (eq type (assoc-default 'type source))
                  (string= (symbol-name type) (assoc-default 'name source)))
          (setq this-source-actions  (assoc-default 'action source))
          (unless (memq (car this-source-actions) all-sources-actions)
            (push (car this-source-actions) all-sources-actions))))
      (setq icicle-candidates-alist
            (sort all-sources-actions   ; Must sort, or `icicle-candidate-nb' will be wrong.
                  (lambda (a1 a2) (funcall icicle-sort-comparer (car a1) (car a2))))))))

(defun icicle-choose-candidate-of-type (type)
  "Read an object of type TYPE (a symbol) with completion, and return it.
These options, when non-nil, control buffer candidate matching and
filtering:
 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'               - Extra buffers to display
 `icicle-buffer-match-regexp'         - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'      - Regexp buffers must not match
 `icicle-buffer-predicate'            - Predicate buffer must satisfy
 `icicle-buffer-sort'                 - Sort function for candidates"
  (let ((icicle-orig-window  (selected-window))) ; For alternative actions.
    (case type
      (buffer
       (let* ((completion-ignore-case
               (or (and (boundp 'read-buffer-completion-ignore-case)  read-buffer-completion-ignore-case)
                   completion-ignore-case))
              (icicle-must-match-regexp                icicle-buffer-match-regexp)
              (icicle-must-not-match-regexp            icicle-buffer-no-match-regexp)
              (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
              (icicle-must-pass-after-match-predicate  icicle-buffer-predicate)
              (icicle-require-match-flag               icicle-buffer-require-match-flag)
              (icicle-extra-candidates                 icicle-buffer-extras)
              (icicle-delete-candidate-object          'icicle-kill-a-buffer) ; `S-delete' kills buf
              (icicle-transform-function               'icicle-remove-dups-if-extras)
              (icicle--temp-orders
               (append (list '("by last access") ; Renamed from "turned OFF'.
                             '("*...* last" . icicle-buffer-sort-*...*-last)
                             '("by buffer size" . icicle-buffer-smaller-p)
                             '("by major mode name" . icicle-major-mode-name-less-p)
                             (and (fboundp 'icicle-mode-line-name-less-p)
                                  '("by mode-line mode name" . icicle-mode-line-name-less-p))
                             '("by file/process name" . icicle-buffer-file/process-name-less-p))
                       (delete '("turned OFF") (copy-sequence icicle-sort-orders-alist))))
              ;; Put `icicle-buffer-sort' first.  If already in list, move it, else add it, to start.
              (icicle-sort-orders-alist
               (progn (when (and icicle-buffer-sort-first-time-p  icicle-buffer-sort)
                        (setq icicle-sort-comparer             icicle-buffer-sort
                              icicle-buffer-sort-first-time-p  nil))
                      (if icicle-buffer-sort
                          (let ((already-there  (rassq icicle-buffer-sort icicle--temp-orders)))
                            (if already-there
                                (cons already-there (setq icicle--temp-orders
                                                          (delete already-there icicle--temp-orders)))
                              (cons `("by `icicle-buffer-sort'" . ,icicle-buffer-sort)
                                    icicle--temp-orders)))
                        icicle--temp-orders)))
              (icicle-candidate-alt-action-fn
               (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "buffer")))
              (icicle-all-candidates-list-alt-action-fn
               (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "buffer"))))
         (get-buffer-create
          (completing-read "Which (buffer): " (mapcar (lambda (buf) (list (buffer-name buf)))
                                                      (buffer-list))
                           (and icompletep
                                icicle-buffer-predicate
                                (lambda (buf) (funcall icicle-buffer-predicate (car buf))))
                           (and (fboundp 'confirm-nonexistent-file-or-buffer) ; Emacs 23.
                                (confirm-nonexistent-file-or-buffer))
                           nil 'buffer-name-history nil nil))))
      (color (icicle-read-color-wysiwyg 1)) ; Use the color name (only).
      (command (let* ((pred                                    (lambda (s)
                                                                 (unless (symbolp s) (setq s  (intern s)))
                                                                 (commandp s)))
                      (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
                      (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
                      (icicle-candidate-alt-action-fn          (or icicle-candidate-alt-action-fn
                                                                   (icicle-alt-act-fn-for-type "command")))
                      (icicle-all-candidates-list-alt-action-fn
                       (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "command"))))
                 (intern (completing-read "Which (command): " obarray (and icompletep  pred)))))
      (face (let ((icicle-candidate-alt-action-fn
                   (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "face")))
                  (icicle-all-candidates-list-alt-action-fn
                   (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "face"))))
              (intern (completing-read "Which (face): " (mapcar (lambda (x) (list (format "%s" x)))
                                                                (face-list))))))
      (file (let ((icicle-candidate-alt-action-fn
                   (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "file")))
                  (icicle-all-candidates-list-alt-action-fn
                   (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "file")))
                  (icicle-delete-candidate-object  'icicle-delete-file-or-directory)) ; `S-delete'
              (read-file-name "Which (file): " nil
                              (and (eq major-mode 'dired-mode)
                                   (fboundp 'dired-get-file-for-visit) ; Emacs 22+.
                                   (condition-case nil ; E.g. error because not on file line (ignore)
                                       (abbreviate-file-name (dired-get-file-for-visit))
                                     (error nil))))))
      (frame (let ((frame-alist  (icicle-make-frame-alist))
                   (icicle-candidate-alt-action-fn
                    (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "frame")))
                   (icicle-all-candidates-list-alt-action-fn
                    (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "frame"))))
               (cdr (assoc (completing-read "Which (frame): " frame-alist) frame-alist))))
      (function (let* ((pred                                    (lambda (s)
                                                                  (unless (symbolp s) (setq s  (intern s)))
                                                                  (fboundp s)))
                       (icompletep                              (and (boundp 'icomplete-mode)
                                                                     icomplete-mode))
                       (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
                       (icicle-candidate-alt-action-fn
                        (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "function")))
                       (icicle-all-candidates-list-alt-action-fn
                        (or icicle-all-candidates-list-alt-action-fn
                            (icicle-alt-act-fn-for-type "function"))))
                  (intern (completing-read "Which (function): " obarray (and icompletep  pred)))))
      (option (let* ((pred                                    (lambda (s)
                                                                (unless (symbolp s) (setq s  (intern s)))
                                                                (user-variable-p s)))
                     (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
                     (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
                     (icicle-candidate-alt-action-fn
                      (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "option")))
                     (icicle-all-candidates-list-alt-action-fn
                      (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "option"))))
                (intern (completing-read "Which (user option): " obarray (and icompletep  pred)))))
      (process (let ((icicle-candidate-alt-action-fn
                      (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "process")))
                     (icicle-all-candidates-list-alt-action-fn
                      (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "process"))))
                 (get-process
                  (completing-read
                   "Which (process): " (mapcar (lambda (proc) (list (process-name proc)))
                                               (process-list))))))
      (symbol (let ((icicle-candidate-alt-action-fn
                     (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "symbol")))
                    (icicle-all-candidates-list-alt-action-fn
                     (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "symbol"))))
                (intern (completing-read "Which (symbol): " obarray))))
      (variable (let* ((pred                                    (lambda (s)
                                                                  (unless (symbolp s) (setq s  (intern s)))
                                                                  (boundp s)))
                       (icompletep                              (and (boundp 'icomplete-mode)
                                                                     icomplete-mode))
                       (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
                       (icicle-candidate-alt-action-fn          (or icicle-candidate-alt-action-fn
                                                                    (icicle-alt-act-fn-for-type "variable")))
                       (icicle-all-candidates-list-alt-action-fn
                        (or icicle-all-candidates-list-alt-action-fn
                            (icicle-alt-act-fn-for-type "variable"))))
                  (intern (completing-read "Which (variable): " obarray (and icompletep  pred)))))
      (window (let ((icicle-candidate-alt-action-fn
                     (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "window")))
                    (icicle-all-candidates-list-alt-action-fn
                     (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "window")))
                    (buffers  ()))
                (walk-windows (lambda (win)
                                (push (list (format "%s" (window-buffer win))) buffers))
                              nil t)
                (get-buffer-window (completing-read "Window showing buffer: " buffers) 0)))
      (otherwise (error "Bad object type: %S" type)))))

(defun icicle-read-var-value-satisfying (predicate)
  "Read a variable that satisfies PREDICATE and returns its value."
  (symbol-value
   (let* ((icicle-orig-window                      (selected-window))
          (pred                                    `(lambda (s)
                                                     (unless (symbolp s) (setq s  (intern s)))
                                                     (and (boundp s)
                                                      (funcall ',predicate (symbol-value s)))))
          (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
          (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
          (icicle-candidate-alt-action-fn          (or icicle-candidate-alt-action-fn
                                                       (icicle-alt-act-fn-for-type "variable")))
          (icicle-all-candidates-list-alt-action-fn
           (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "variable"))))
     (intern
      (completing-read (format "Which (%s value of variable): " predicate) obarray (and icompletep  pred))))))

(icicle-define-command icicle-select-text-at-point
  "Invoke a function in `er/try-expand-list' to select text at point.
This command requires library `expand-region.el'."
  (lambda (fn) (ignore-errors (funcall (intern fn)))) ; Action function
  "Text selection function: "           ; `completing-read' args
  (mapcar (lambda (fn) (list (symbol-name fn))) (and (boundp 'er/try-expand-list)  er/try-expand-list))
  nil t nil nil "er/mark-word" nil
  ((icicle-sort-comparer nil))          ; Bindings
  (unless (require 'expand-region nil t) ; First code
    (error "This command requires library `expand-region.el'")))
  
(defvar icicle-key-prefix nil
  "A prefix key.")

(defvar icicle-key-prefix-2 nil
  "A prefix key.")

(defvar icicle-active-map nil
  "An active keymap.")

;;; $$$$$$(defvar icicle-orig-buff-key-complete nil
;;;         "Current buffer when you invoked `icicle-complete-keys'.")

(defvar icicle-orig-extra-cands ()
  "Value of `icicle-extra-candidates', before command.")

(defvar icicle-orig-show-initially-flag nil
  "Value of `icicle-show-Completions-initially-flag', before command.")

(defvar icicle-orig-sort-orders-alist ()
  "Value of `icicle-sort-orders-alist', before command.")

;;; $$$$$$(defvar icicle-orig-win-key-complete nil
;;;         "Selected window when you invoked `icicle-complete-keys'.")

(defvar icicle-this-cmd-keys ()
  "Value of `this-command-keys-vector' at some point in key completion.")

(when (fboundp 'map-keymap)             ; Emacs 22+.

  (defun icicle-complete-keys ()        ; Bound to prefix keys followed by `S-TAB' (unless defined).
    "Complete a key sequence for the currently invoked prefix key.
Input-candidate completion and cycling are available.

You can navigate the key-binding hierarchy (prefix-key hierarchy),
just as would navigate a file-system hierarchy (to complete directory
and file names) or a menu hierarchy (to complete submenu and menu-item
names).

Completion candidates generally have the form `KEY  =  COMMAND'.

If COMMAND is `...', then KEY is a prefix key; choosing it updates the
completion candidates list to the keys under that prefix.  For
example, choosing `C-x = ...' changes the candidates to those with
prefix `C-x'.

The special candidate `..' means to go up one level of the key-binding
hierarchy and complete candidates there.  For example, if you are
currently completing prefix key `C-x 5', and you choose candidate
`..', then you will be completing prefix `C-x', the parent of `C-x 5'.

Except at the top level, the default value for completion is `..'.

You can use `C-M-,' at any time to switch between sorting with local
bindings first and sorting with prefix keys first.  You can use `C-,'
at any time to change the sort order among these two and sorting by
command name.

If option `icicle-complete-keys-self-insert-ranges' is non-`nil', then
some keys that are bound to `self-insert-command' are included as
possible key-completion candidates; otherwise they are not.  The
default value is `nil'.

For Emacs 22, this option is effectively Boolean: any non-`nil' value
means allow all self-inserting keys as candidates.

In Emacs 23+, there are thousands of self-inserting keys, so it is not
practical to allow all as candidates.  Instead, a non-`nil' value is a
list of character ranges of the form (MIN . MAX).  Characters in the
inclusive range MIN through MAX are possible key-completion
candidates.

Most of the thousands of self-inserting characters for Emacs 23+ are
Unicode characters.  For a self-inserting character CHAR, the
completion candidate is generally `CHAR  =  self-insert-command', but
if CHAR is a Unicode character then it is `CHAR = UNICODE-NAME',
where UNICODE-NAME is the name of the Unicode character.  This is so
that you can complete against the name.

For Emacs 23+, if you use a non-`nil' value for
`icicle-complete-keys-self-insert-ranges' then I recommend that you
use only small ranges for good performance.  In general, you might
want to leave this option value as `nil' and use command `ucs-insert'
\(`C-x 8 RET') to insert characters.  In Icicle mode, `ucs-insert'
displays the character itself after its name, though only the name is
used for completion.  So it is WYSIWYG.  And you can of course use all
Icicles completion features when matching the character name.

While cycling, these keys describe candidates\\<minibuffer-local-completion-map>:

`C-RET'   - Describe command of current completion candidate only
`C-down'  - Move to next completion candidate and describe
`C-up'    - Move to previous completion candidate and describe
`C-next'  - Move to next apropos-completion candidate and describe
`C-prior' - Move to previous apropos-completion candidate and describe
`C-end'   - Move to next prefix-completion candidate and describe
`C-home'  - Move to previous prefix-completion candidate and describe
`\\[icicle-all-candidates-action]'     - Describe *all* candidates (or all that are saved),
            successively - use the [back] button in buffer *Help* to
            visit the descriptions

When candidate action and cycling are combined (e.g. `C-next'), option
`icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`\\[icicle-help-on-candidate]', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate, or
`C-g' to quit.  This is an Icicles command - see command
`icicle-mode'."
    (interactive)
    (let* ((icicle-transform-function               'icicle-remove-duplicates)
           (icicle-orig-sort-orders-alist           icicle-sort-orders-alist) ; For recursive use.
           (icicle-orig-show-initially-flag         icicle-show-Completions-initially-flag)
           (icicle-show-Completions-initially-flag  t)
           (icicle-candidate-action-fn              'icicle-complete-keys-action)
           (enable-recursive-minibuffers            t)
           ;; $$$$$$ (icicle-orig-buff-key-complete           (current-buffer))
           ;; $$$$$$ (icicle-orig-win-key-complete            (selected-window))
           (icicle-orig-buff                        (current-buffer))
           (icicle-orig-window                      (selected-window))
           (icicle-completing-keys-p                t) ; Provide a condition to test key completion.
           (icicle-sort-comparer                    'icicle-local-keys-first-p)
           (icicle-alternative-sort-comparer        'icicle-prefix-keys-first-p)
           (icicle-sort-orders-alist
            '(("by key name, local bindings first" . icicle-local-keys-first-p)
              ("by key name, prefix keys first" . icicle-prefix-keys-first-p)
              ("by command name" . icicle-command-names-alphabetic-p)
              ("turned OFF")))
           (icicle-hist-cands-no-highlight          '("..")))
      (icicle-complete-keys-1 (icicle-this-command-keys-prefix))))

  (defun icicle-this-command-keys-prefix ()
    "Return the prefix of the currently invoked key sequence."
    (let ((this-key  (this-command-keys))) (substring this-key 0 (1- (length this-key)))))

  ;; Free vars here: `icicle-complete-keys-alist' is bound in `icicles-var.el'.
  (defun icicle-complete-keys-1 (prefix) ; PREFIX is a free var in `icicle-complete-keys-action'.
    "Complete a key sequence for prefix key PREFIX (a vector)."
    ;; `icicle-orig-extra-cands' is free in `icicle-complete-keys-action'.
    (let ((icicle-orig-extra-cands  icicle-extra-candidates)
          (icicle-key-prefix        prefix))
      (unwind-protect
           (progn
             (icicle-keys+cmds-w-prefix prefix)
             (unless icicle-complete-keys-alist (error "No keys for prefix `%s'" prefix))
             (let* ((icicle-this-cmd-keys ; For error report - e.g. mouse command.
                     (this-command-keys-vector)) ; Free var in `icicle-complete-keys-action'.
                    (icicle-key-prefix-description
                     (icicle-key-description prefix nil icicle-key-descriptions-use-<>-flag))
                    (prompt  (concat "Complete keys"
                                     (and (not (string= "" icicle-key-prefix-description))
                                          (concat " " icicle-key-prefix-description))
                                     ": ")))
               (put-text-property 0 1 'icicle-fancy-candidates t prompt)
               (icicle-complete-keys-action
                (completing-read prompt icicle-complete-keys-alist nil t nil nil
                                 ;;$$ (if (equal [] prefix) nil "\\.\\.")
                                 ))))
        (dolist (cand  icicle-complete-keys-alist)
          (put (car cand) 'icicle-special-candidate nil))))) ; Reset the property.

  ;; Free vars here:
  ;; `icicle-orig-buff', `icicle-orig-window', bound in `icicle-complete-keys'.
  ;; `icicle-orig-extra-cands', `icicle-this-cmd-keys', `icicle-key-prefix',
  ;; bound in `icicle-complete-keys-1'.
  (defun icicle-complete-keys-action (candidate)
    "Completion action function for `icicle-complete-keys'."
    (let* ((key+binding    (cdr-safe (assq (intern candidate) icicle-complete-keys-alist)))
           (key            (car-safe key+binding))
           (binding        (cdr-safe key+binding))
           (cmd-name       nil)
           (action-window  (selected-window)))
      (unwind-protect
           (progn
             ;; $$$$$$ (set-buffer icicle-orig-buff-key-complete)
             ;; $$$$$$ (select-window icicle-orig-win-key-complete)
             (set-buffer icicle-orig-buff)
             (select-window icicle-orig-window)
             (if (string= ".." candidate)
                 (setq cmd-name  "..")
               (unless (and (string-match "\\(.+\\)  =  \\(.+\\)" candidate)  (match-beginning 2))
                 (error "No match"))
               (setq cmd-name  (substring candidate (match-beginning 2) (match-end 2))))
             (cond ((string= ".." cmd-name) ; Go back up to parent prefix.
                    (setq last-command  'icicle-complete-keys)
                    (icicle-complete-keys-1 (vconcat (nbutlast (append icicle-key-prefix ())))))
                   ((and key  (string= "..." cmd-name)) ; Go down to prefix.
                    (setq last-command  'icicle-complete-keys)
                    (icicle-complete-keys-1 (vconcat icicle-key-prefix key)))
                   (t
                    (setq this-command             binding
                          last-command             binding
                          icicle-extra-candidates  icicle-orig-extra-cands) ; Restore it.
                    (when (eq 'self-insert-command binding)
                      (unless key (error "Cannot insert `%s'" key))
                      (setq last-command-event  (aref key 0)))
                    (when (eq 'digit-argument binding)
                      (setq last-command-event  (aref key 0))
                      (icicle-msg-maybe-in-minibuffer "Numeric argument"))
                    (when (eq 'negative-argument binding)
                      (icicle-msg-maybe-in-minibuffer "Negative argument"))
                    (setq last-nonmenu-event  1) ; So `*Completions*' mouse-click info is ignored.
                    ;; Bind so vanilla context when invoke chosen command.
                    (let ((icicle-candidate-action-fn              nil)
                          (icicle-completing-keys-p                nil)
                          (icicle-sort-orders-alist                icicle-orig-sort-orders-alist)
                          (icicle-sort-comparer                    'icicle-case-string-less-p)
                          (icicle-alternative-sort-comparer
                           'icicle-historical-alphabetic-p)
                          (icicle-show-Completions-initially-flag
                           icicle-orig-show-initially-flag))
                      (call-interactively binding nil icicle-this-cmd-keys)))))
        (select-window action-window))))

  (defun icicle-keys+cmds-w-prefix (prefix)
    "Fill `icicle-complete-keys-alist' for prefix key PREFIX (a vector)."
    (let ((prefix-map           nil)
          (icicle-key-prefix-2  prefix))
      (setq icicle-complete-keys-alist  ())
      (dolist (icicle-active-map  (current-active-maps t))
        (setq prefix-map  (lookup-key icicle-active-map prefix))
        ;; NOTE: `icicle-add-key+cmd' uses `icicle-key-prefix-2' and `icicle-active-map' as free vars.
        (when (keymapp prefix-map) (map-keymap #'icicle-add-key+cmd prefix-map)))
      (unless (equal [] prefix)
        (push (list (intern (propertize ".." 'face 'icicle-multi-command-completion)))
              icicle-complete-keys-alist))
      ;; Delete duplicates.  Shadowing of bindings can produce dups.  E.g., `ESC' can be in submaps of
      ;; different active keymaps, giving more than one `ESC ...' entries.  Also, one binding for a
      ;; command can shadow another to the same command.
      (icicle-delete-alist-dups icicle-complete-keys-alist)))

  ;; Free vars here: `icicle-key-prefix-2' and `icicle-active-map' are bound in
  ;; `icicle-keys+cmds-w-prefix'.
  (defun icicle-add-key+cmd (event binding)
    "Add completion for EVENT and BINDING to `icicle-complete-keys-alist'."
    (cond
      ;; (menu-item ITEM-STRING): non-selectable item - skip it.
      ((and (eq 'menu-item (car-safe binding))  (null (cdr-safe (cdr-safe binding))))
       (setq binding  nil))             ; So `keymapp' test, below, fails.

      ;; (ITEM-STRING): non-selectable item - skip it.
      ((and (stringp (car-safe binding))  (null (cdr-safe binding)))
       (setq binding  nil))             ; So `keymapp' test, below, fails.

      ;; (menu-item ITEM-STRING REAL-BINDING . PROPERTIES)
      ((eq 'menu-item (car-safe binding))
       (let ((enable-condition  (memq ':enable (cdr-safe (cdr-safe (cdr-safe binding))))))
         (if (or (not enable-condition)
                 (condition-case nil    ; Don't enable if we can't check the condition.
                     (eval (cadr enable-condition))
                   (error nil)))
             (setq binding  (car-safe (cdr-safe (cdr-safe binding))))
           (setq binding  nil))))

      ;; (ITEM-STRING . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] (KEYBD-SHORTCUTS) . REAL-BINDING)
      ((stringp (car-safe binding))
       (setq binding  (cdr binding))
       ;; Skip HELP-STRING
       (when (stringp (car-safe binding)) (setq binding  (cdr binding)))
       ;; Skip (KEYBD-SHORTCUTS): cached key-equivalence data for menu items.
       (when (and (consp binding)  (consp (car binding))) (setq binding  (cdr binding)))))

    ;; `icicle-key-prefix-2' and `icicle-active-map' are free here, bound in
    ;; `icicle-keys+cmds-w-prefix'.
    (cond ((and (eq binding 'self-insert-command) ; Insert `self-insert-command' char ranges.
                icicle-complete-keys-self-insert-ranges
                (consp event)           ; Emacs 23+ uses char ranges.
                (fboundp 'characterp)
                (characterp (car event)))
           (let ((chr1  (car event))
                 (chr2  (cdr event)))
             (loop for range in icicle-complete-keys-self-insert-ranges do
                   (loop for char from (max chr1 (car range)) to (min chr2  (cdr range)) do
                         (let* ((key-desc   (propertize
                                             (single-key-description
                                              char (not icicle-key-descriptions-use-<>-flag))
                                             'face 'icicle-candidate-part))
                                (name.char  (rassq char (ucs-names)))
                                (candidate  (and (or (not name.char)  (not (string= "" (car name.char))))
                                                 ;; $$$$$$  (and (not (string= "" (car name.char)))
                                                 ;;              ;; $$$$$$ Maybe make this optional?
                                                 ;;              (not (string-match
                                                 ;;                    "\\`VARIATION SELECTOR"
                                                 ;;                    (car name.char)))))
                                                 (intern (concat key-desc
                                                                 (if name.char
                                                                     (concat "  =  " (car name.char))
                                                                   "  =  self-insert-command"))))))
                           (when candidate
                             (push (cons candidate (cons (vector char) 'self-insert-command))
                                   icicle-complete-keys-alist)
                             (when (eq icicle-active-map (current-local-map))
                               (put candidate 'icicle-special-candidate t))))))))
          ((and
            ;; Include BINDING if key (EVENT) is on `icicle-key-prefix-2'.
            ;; Do not include a shadowed binding to a command.  Always include binding if it's a keymap,
            ;; because the same prefix key can be bound to different keymaps without any of those keymaps
            ;; shadowing all of the bindings in another of them.
            (or (keymapp binding)
                (and (commandp binding)
                     (equal binding (key-binding (vconcat icicle-key-prefix-2 (vector event)) nil 'NO-REMAP))
                     (not (eq binding 'icicle-complete-keys))))
            ;; Include BINDING if it is not `self-insert-command' or user has OK'd use of such.
            (or (not (eq binding 'self-insert-command)) ; Command, keymap.
                (and icicle-complete-keys-self-insert-ranges ; Insert char (Emacs 22).
                     (char-valid-p event))))
           (when (and (if (fboundp 'characterp) (characterp event) (char-valid-p event)) ; `ESC' -> `M-'.
                      (eq event meta-prefix-char)
                      (keymapp binding))
             (map-keymap (lambda (key bndg)
                           (when (if (fboundp 'characterp) (characterp key) (char-valid-p key))
                             (icicle-add-key+cmd (event-apply-modifier key 'meta 27 "M-") bndg)))
                         binding))
           (when (and (functionp binding)  (commandp binding)) ; Follow remapped command to target command.
             (setq binding  (key-binding (vconcat icicle-key-prefix-2 (vector event)))))
           (let* ((key-desc   (propertize (single-key-description
                                           event
                                           (not icicle-key-descriptions-use-<>-flag))
                                          'face 'icicle-candidate-part))
                  (candidate  (intern (concat key-desc "  =  " (if (keymapp binding)
                                                                   "..."
                                                                 (prin1-to-string binding))))))
             ;; Skip keys bound to `undefined'.
             (unless (string= "undefined" (prin1-to-string binding))
               (push (cons candidate (cons (vector event) binding)) icicle-complete-keys-alist))
             (when (eq icicle-active-map (current-local-map))
               (put candidate 'icicle-special-candidate t))))
          ((and (integerp event)  (generic-char-p event) ; Insert generic char (Emacs 22).
                (eq 'self-insert-command binding))
           (ignore))))                  ; Placeholder for future use.

;;;   ;; $$$$$$ No longer used.  Was used in `icicle-complete-keys-1'.
;;;   (defun icicle-read-single-key-description (string need-vector &optional angles)
;;;     "If STRING contains a space, then the vector containing the symbol named STRING.
;;; Otherwise, call `icicle-read-kbd-macro'.
;;; Other args are as for `icicle-read-kbd-macro'."
;;;     (cond ((and (not angles)  (string-match " " string))
;;;            (vector (intern string)))
;;;           ((string-match "^<\\([^>]* [^>]*\\)>" string)
;;;            (vector (intern (substring string (match-beginning 1) (match-end 1)))))
;;;           (t (icicle-read-kbd-macro string need-vector angles))))

  ;; $$ No longer used.  Was used as `icicle-candidate-action-fn' in `icicle-complete-keys'.
  (defun icicle-complete-keys-help (candidate)
    "Describe the command associated with the current completion candidate."
    (interactive)                       ; Interactively, just describes itself.
    (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
    (string-match "\\(.+\\)  =  \\(.+\\)" candidate)
    (let ((frame-with-focus  (selected-frame))
          (cmd               (intern-soft (substring candidate (match-beginning 2) (match-end 2)))))
      (if (not (fboundp cmd))
          (icicle-msg-maybe-in-minibuffer "No help")
        (describe-function cmd))
      (icicle-raise-Completions-frame)
      ;; This is a hack for MS Windows - otherwise, we can't continue to get more candidates,
      ;; because the *Help* frame takes the focus away from the minibuffer frame.
      ;; MS Windows always gives focus to a newly created frame - in this case, *Help*.
      (let* ((help-window  (get-buffer-window "*Help*" 0))
             (help-frame   (and help-window  (window-frame help-window))))
        (when help-frame (redirect-frame-focus help-frame frame-with-focus))))
    (message nil))                      ; Let minibuffer contents show immediately.

  )

(when (fboundp 'define-minor-mode)      ; Emacs 21+ ------------
  (eval '(define-minor-mode icicle-ido-like-mode
          "Ido-like mode for use with Icicles.
No, this mode does not pretend to give you exactly the Ido behavior.

Turning the mode ON sets these options to t:
 `icicle-show-Completions-initially-flag'
 `icicle-top-level-when-sole-completion-flag'
Turning the mode OFF sets those options to non-nil.

A positive prefix arg turns the mode on and also sets option
`icicle-max-candidates' to the prefix-arg numeric value.  By default,
that option is nil, meaning that there is no limit to the number of
completion candidates.

Since Ido shows only a few completion candidates, you might want to
customize that option or use a prefix arg with this mode to set it.
You can also use `C-x #' in the minibuffer to increment or decrement
the option at any time during completion.

Turning the mode off by toggling (no prefix arg) resets option
`icicle-max-candidates' to nil.  If you have customized that option to
a non-nil value and do not want to lose that preference, then use a
zero or negative prefix arg to turn the mode off.

See also these options, which control how much time you have to edit
input before automatic incremental completion and automatic acceptance
of a sole candidate kick in:

 `icicle-incremental-completion-delay'
 `icicle-top-level-when-sole-completion-delay'

When you use this mode, you might also want to use nil or t as the
value of option `icicle-default-value', in order to not insert the
default value in the minibuffer.  If you want to change that option
dynamically for the mode, use `icicle-ido-like-mode-hook'.  E.g.:

 (add-hook 'icicle-ido-like-mode-hook
           (lambda () (setq icicle-default-value
                       (if icicle-ido-like-mode t 'insert-end))))"
          nil nil nil :global t :group 'Icicles-Miscellaneous
          (setq
           icicle-show-Completions-initially-flag      icicle-ido-like-mode
           icicle-top-level-when-sole-completion-flag  icicle-ido-like-mode)
          (if icicle-ido-like-mode
              (when (and current-prefix-arg  (not (eq 'toggle current-prefix-arg)))
                (setq icicle-max-candidates  (prefix-numeric-value current-prefix-arg)))
            (unless (and current-prefix-arg  (not (eq 'toggle current-prefix-arg)))
              (setq icicle-max-candidates  nil))))))

(defun icicle-set-TAB-methods-for-command (command methods &optional arg msgp)
  "Set the possible TAB completion methods for COMMAND.
This works by advising COMMAND.
With a negative prefix arg, restore the original, unadvised behavior.
With a non-negative prefix arg, advise but do not enable the advice.
 You can later enable the advice and activate it, using
 `ad-enable-advice' and `ad-activate'.  The advice name is
 `icicle-TAB-completion-methods' (same name as option).
See also `icicle-set-S-TAB-methods-for-command'."
  (interactive (icicle-read-args-for-set-completion-methods 'icicle-TAB-completion-methods))
  (icicle-set-completion-methods-for-command
   command methods 'icicle-TAB-completion-methods arg msgp))

(defun icicle-set-S-TAB-methods-for-command (command methods &optional arg msgp)
  "Set the possible S-TAB completion methods for COMMAND.
This works by advising COMMAND.
With a negative prefix arg, restore the original, unadvised behavior.
With a non-negative prefix arg, advise but do not enable the advice.
 You can later enable the advice and activate it, using
 `ad-enable-advice' and `ad-activate'.  The advice name is
 `icicle-S-TAB-completion-methods-alist' (same name as option).
See also `icicle-set-TAB-methods-for-command'."
  (interactive (icicle-read-args-for-set-completion-methods 'icicle-S-TAB-completion-methods-alist))
  (icicle-set-completion-methods-for-command
   command methods 'icicle-S-TAB-completion-methods-alist arg msgp))

(defun icicle-read-args-for-set-completion-methods (var)
  "Read arguments for `icicle-set-(S-)TAB-methods-for-command'.
VAR is symbol `icicle-(S-)TAB-completion-methods(-alist)'."
  (let ((command  (intern
                   (let* ((pred                                    (lambda (c)
                                                                     (unless (symbolp c)
                                                                       (setq c  (intern c)))
                                                                     (commandp c)))
                          (icompletep                              (and (boundp 'icomplete-mode)
                                                                        icomplete-mode))
                          (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred)))
                     (completing-read "Command: " obarray (and icompletep  pred) t))))
        (methods  ()))
    (unless (< (prefix-numeric-value current-prefix-arg) 0)
      (let ((prompt  (format "%s methods (empty `RET' to end): "
                             (if (eq var 'icicle-TAB-completion-methods) "TAB" "S-TAB")))
            (cmeths  (symbol-value var))
            meth)
        (setq cmeths  (if (consp (car cmeths))
                          cmeths
                        (mapcar (lambda (m) (list (symbol-name m))) cmeths)))
        (while (not (string= "" (setq meth  (completing-read prompt cmeths nil t))))
          (if (eq var 'icicle-TAB-completion-methods)
              (push (intern meth) methods)
            (push (assoc meth icicle-S-TAB-completion-methods-alist) methods)))
        (setq methods  (nreverse methods))))
    (list command methods current-prefix-arg (prefix-numeric-value current-prefix-arg))))

(defun icicle-set-completion-methods-for-command (command methods var arg msgp)
  "Set the possible completion methods for COMMAND.
This works by advising COMMAND.
VAR is the methods option to affect, `icicle-TAB-completion-methods'
 or `icicle-S-TAB-completion-methods-alist'.
Negative ARG means restore the original, unadvised behavior.
Non-negative ARG means advise but do not enable the advice.
Null ARG means advise and enable."
  (let ((type  (if (eq var 'icicle-TAB-completion-methods) "TAB methods" "S-TAB methods")))
    (cond ((< (prefix-numeric-value arg) 0)
           (condition-case nil
               (ad-remove-advice command 'around var)
             (error nil))
           (ad-activate command)        ; This restores original definition unless otherwise advised.
           (when msgp (message "`%s' now uses default %s" command type)))
          (t
           (ad-add-advice command `(,var
                                    nil ,(not arg)
                                    (advice . (lambda () (let ((,var  ',methods)) ad-do-it))))
                          'around 'first)
           (ad-activate command)
           (when msgp
             (message "`%s' %s: %s" command type
                      (icicle-propertize
                       (if (consp (car methods)) (mapcar #'car methods) methods)
                       'face 'icicle-msg-emphasis)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-cmd2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-cmd2.el ends here

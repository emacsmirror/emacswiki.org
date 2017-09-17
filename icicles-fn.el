;;; icicles-fn.el --- Non-interactive functions for Icicles
;;
;; Filename: icicles-fn.el
;; Description: Non-interactive functions for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2017, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:25:53 2006
;; Last-Updated: Sun Sep 17 09:36:04 2017 (-0700)
;;           By: dradams
;;     Update #: 15238
;; URL: https://www.emacswiki.org/emacs/download/icicles-fn.el
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
;;   `ffap-', `fit-frame', `flx', `frame-fns', `fuzzy',
;;   `fuzzy-match', `help+20', `hexrgb', `icicles-opt',
;;   `icicles-var', `info', `info+20', `kmacro', `levenshtein',
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
;;  non-interactive functions.  For Icicles documentation, see
;;  `icicles-doc1.el' and `icicles-doc2.el'.
;;
;;  Macros defined here:
;;
;;    `icicle-maybe-cached-action', `minibuffer-with-setup-hook'.
;;
;;  Commands defined here:
;;
;;    `icicle-dired-smart-shell-command',
;;    `icicle-minibuffer-default-add-dired-shell-commands',
;;    `icicle-shell-command', `icicle-shell-command-on-region',
;;
;;  Non-interactive functions defined here:
;;
;;    `assq-delete-all', `icicle-2nd-part-string-less-p',
;;    `icicle-abbreviate-or-expand-file-name',
;;    `icicle-alist-key-match', `icicle-all-completions',
;;    `icicle-alpha-p', `icicle-alt-act-fn-for-type',
;;    `icicle-any-candidates-p', `icicle-apropos-any-candidates-p',
;;    `icicle-apropos-any-file-name-candidates-p',
;;    `icicle-apropos-candidates', `icicle-assoc-delete-all',
;;    `icicle-barf-if-outside-Completions',
;;    `icicle-barf-if-outside-Completions-and-minibuffer',
;;    `icicle-barf-if-outside-minibuffer',
;;    `icicle-bookmark-annotated-p', `icicle-bookmark-autofile-p',
;;    `icicle-bookmark-autonamed-p',
;;    `icicle-bookmark-autonamed-this-buffer-p',
;;    `icicle-bookmark-bookmark-file-p',
;;    `icicle-bookmark-bookmark-list-p', `icicle-bookmark-desktop-p',
;;    `icicle-bookmark-dired-p', `icicle-bookmark-dired-this-dir-p',
;;    `icicle-bookmark-dired-wildcards-p', `icicle-bookmark-file-p',
;;    `icicle-bookmark-file-this-dir-p', `icicle-bookmark-flagged-p',
;;    `icicle-bookmark-function-p', `icicle-bookmark-gnus-p',
;;    `icicle-bookmark-icicle-search-hits-p',
;;    `icicle-bookmark-image-p', `icicle-bookmark-info-p',
;;    `icicle-bookmark-lighted-p',
;;    `icicle-bookmark-local-directory-p',
;;    `icicle-bookmark-local-file-p', `icicle-bookmark-man-p',
;;    `icicle-bookmark-marked-p', `icicle-bookmark-modified-p',
;;    `icicle-bookmark-navlist-p', `icicle-bookmark-non-dir-file-p',
;;    `icicle-bookmark-non-file-p', `icicle-bookmark-omitted-p',
;;    `icicle-bookmark-orphaned-file-p',
;;    `icicle-bookmark-orphaned-local-file-p',
;;    `icicle-bookmark-orphaned-remote-file-p',
;;    `icicle-bookmark-region-p', `icicle-bookmark-remote-file-p',
;;    `icicle-bookmark-sequence-p', `icicle-bookmark-snippet-p',
;;    `icicle-bookmark-tagged-p', `icicle-bookmark-temporary-p',
;;    `icicle-bookmark-this-buffer-p', `icicle-bookmark-url-p',
;;    `icicle-bookmark-url-browse-p',
;;    `icicle-bookmark-variable-list-p', `icicle-bookmark-w3m-p',
;;    `icicle-bounds-of-thing-at-point',
;;    `icicle-buffer-file/process-name-less-p',
;;    `icicle-buffer-modified-p', `icicle-buffer-smaller-p',
;;    `icicle-call-then-update-Completions', `icicle-candidate-set-1',
;;    `icicle-candidate-short-help',
;;    `icicle-case-insensitive-string-less-p',
;;    `icicle-case-string-less-p', `icicle-cdr-lessp',
;;    `icicle-char-cands-from-charlist',
;;    `icicle-choose-completion-string', `icicle-clear-lighter',
;;    `icicle-clear-minibuffer', `icicle-color-gray-p',
;;    `icicle-color-name-w-bg', `icicle-color-rgb-lessp',
;;    `icicle-color-supported-p', `icicle-command-abbrev-save',
;;    `icicle-command-abbrev-used-more-p',
;;    `icicle-command-names-alphabetic-p',
;;    `icicle-compilation-buffer-p', `icicle-complete-again-update',
;;    `icicle-completing-p', `icicle-completing-read',
;;    `icicle-completing-read-default',
;;    `icicle-completing-read-multiple',
;;    `icicle-completing-read-history',
;;    `icicle-completion-all-completions',
;;    `icicle-completion-pcm--all-completions',
;;    `icicle-completion-setup-function',
;;    `icicle-completion--embedded-envvar-table',
;;    `icicle-completion-try-completion', `icicle-create-thumb',
;;    `icicle-current-TAB-method', `icicle-custom-rogue-p',
;;    `icicle-custom-type', `icicle-custom-variable-p',
;;    `icicle-defaults-at-point', `icicle-define-crm-completion-map',
;;    `icicle-defined-thing-p', `icicle-delete-alist-dups',
;;    `icicle-delete-count', `icicle-delete-dups',
;;    `icicle-delete-whitespace-from-string',
;;    `icicle-dired-read-shell-command',
;;    `icicle-dir-prefix-wo-wildcards',
;;    `icicle-dirs-and-latest-use-first-p', `icicle-dirs-first-p',
;;    `icicle-dirs-last-p', `icicle-displayable-cand-from-saved-set',
;;    `icicle-display-cand-from-full-cand',
;;    `icicle-display-completion-list', `icicle-display-Completions',
;;    `icicle-expanded-common-match',
;;    `icicle-expanded-common-match-1', `icicle-expand-file-name-20',
;;    `icicle-expand-file-or-dir-name',
;;    `icicle-explicit-saved-completion-candidates',
;;    `icicle-extra-candidates-first-p', `icicle-face-bold-p',
;;    `icicle-face-differs-from-default-p',
;;    `icicle-face-inverse-video-p', `icicle-face-italic-p',
;;    `icicle-face-nontrivial-p', `icicle-face-underline-p',
;;    `icicle-face-valid-attribute-values',
;;    `icicle-ffap-file-remote-p', `icicle-ffap-url-p',
;;    `icicle-file-accessible-directory-p',
;;    `icicle-file-compressed-p', `icicle-file-desktop-p',
;;    `icicle-file-directory-p', `icicle-file-elc-p',
;;    `icicle-file-executable-p', `icicle-file-exists-p',
;;    `icicle-file-locked-p', `icicle-file-name-absolute-p',
;;    `icicle-file-name-apropos-candidates',
;;    `icicle-file-name-directory',
;;    `icicle-file-name-directory-w-default',
;;    `icicle-file-name-input-p', `icicle-file-name-nondirectory',
;;    `icicle-file-name-prefix-candidates', `icicle-file-readable-p',
;;    `icicle-file-regular-p', `icicle-file-remote-p',
;;    `icicle-file-symlink-p', `icicle-file-writable-p',
;;    `icicle-filesets-files-under', `icicle-file-type-less-p',
;;    `icicle-files-within', `icicle-files-within-1',
;;    `icicle-filter-alist', `icicle-filter-wo-input',
;;    `icicle-find-tag-default-as-regexp',
;;    `icicle-first-matching-candidate', `icicle-first-N',
;;    `icicle-fit-completions-window', `icicle-fix-default-directory',
;;    `icicle-flat-list', `icicle-flx-score-greater-p' (Emacs 24.3+),
;;    `icicle-frame-iconified-p', `icicle-frame-invisible-p',
;;    `icicle-frames-on', `icicle-frame-splittable-p',
;;    `icicle-frame-thumbnail-p', `icicle-frame-unsplittable-p',
;;    `icicle-fuzzy-candidates', `icicle-get-alist-candidate',
;;    `icicle-get-candidates-from-saved-set', `icicle-get-safe',
;;    `icicle-dired-guess-shell-command',
;;    `icicle-handle-default-for-prompt',
;;    `icicle-highlight-candidate-in-Completions',
;;    `icicle-highlight-complete-input',
;;    `icicle-highlight-initial-whitespace',
;;    `icicle-highlight-input-noncompletion',
;;    `icicle-highlight-input-noncompletion-rest',
;;    `icicle-highlight-lighter', `icicle-historical-alphabetic-p',
;;    `icicle-image-file-p', `icicle-increment-cand-nb+signal-end',
;;    `icicle-Info-node-is-indexed-by-topic',
;;    `icicle-input-from-minibuffer', `icicle-insert-candidates',
;;    `icicle-insert-cand-in-minibuffer',
;;    `icicle-insert-Completions-help-string',
;;    `icicle-interesting-buffer-p', `icicle-join-nth-parts',
;;    `icicle-key-description', `icicle-kill-a-buffer',
;;    `icicle-latest-access-first-p', `icicle-latest-input-first-p',
;;    `icicle-latest-modification-first-p',
;;    `icicle-latest-use-first-p', `icicle-levenshtein-match',
;;    `icicle-levenshtein-one-match', `icicle-levenshtein-one-regexp',
;;    `icicle-levenshtein-strict-match', `icicle-list-position',
;;    `icicle-looks-like-dir-name-p', `icicle-local-keys-first-p',
;;    `icicle-lru-window-for-buffer' (Emacs 24+),
;;    `icicle-make-char-candidate', `icicle-make-face-candidate',
;;    `icicle-make-plain-predicate', `icicle-major-mode-name-less-p',
;;    `icicle-maybe-sort-and-strip-candidates',
;;    `icicle-maybe-sort-maybe-truncate', `icicle-mctize-all',
;;    `icicle-mctized-display-candidate',
;;    `icicle-mctized-full-candidate', `icicle-member-ignore-case',
;;    `icicle-merge-saved-order-less-p',
;;    `icicle-minibuffer-default-add-completions',
;;    `icicle-minibuf-input', `icicle-minibuf-input-sans-dir',
;;    `icicle-minibuffer-prompt-end', `icicle-mode-line-name-less-p',
;;    `icicle-mouseover-help', `icicle-mru-window-for-buffer' (Emacs
;;    24+), `icicle-msg-maybe-in-minibuffer',
;;    `icicle-ms-windows-NET-USE',
;;    `icicle-multi-comp-apropos-complete-match', `icicle-multi-sort',
;;    `icicle-next-candidate', `icicle-next-error-buffer-p',
;;    `icicle-nondirectory-p', `icicle-not-basic-prefix-completion-p',
;;    `icicle-not-special-candidate-p',
;;    `icicle-ORIG-choose-completion-string',
;;    `icicle-ORIG-completing-read',
;;    `icicle-ORIG-completing-read-multiple',
;;    `icicle-ORIG-completion-pcm--all-completions',
;;    `icicle-ORIG-completion-setup-function',
;;    `icicle-ORIG-dired-smart-shell-command',
;;    `icicle-ORIG-display-completion-list',
;;    `icicle-ORIG-face-valid-attribute-values',
;;    `icicle-ORIG-minibuffer-default-add-completions',
;;    `icicle-ORIG-read-buffer', `icicle-ORIG-read-char-by-name',
;;    `icicle-ORIG-read-face-name',
;;    `icicle-ORIG-read-from-minibuffer', `icicle-ORIG-read-number',
;;    `icicle-ORIG-read-string', `icicle-ORIG-shell-command',
;;    `icicle-ORIG-shell-command-on-region',
;;    `icicle-package-built-in-p', `icicle-package-disabled-p',
;;    `icicle-package-installed-p', `icicle-part-1-cdr-lessp',
;;    `icicle-part-1-lessp', `icicle-part-2-lessp',
;;    `icicle-part-3-lessp', `icicle-part-4-lessp',
;;    `icicle-part-N-lessp', `icicle-place-cursor',
;;    `icicle-place-overlay', `icicle-position',
;;    `icicle-prefix-any-candidates-p',
;;    `icicle-prefix-any-file-name-candidates-p',
;;    `icicle-prefix-candidates', `icicle-prefix-keys-first-p',
;;    `icicle-propertize', `icicle-proxy-candidate-first-p',
;;    `icicle-put-at-head', `icicle-put-whole-cand-prop',
;;    `icicle-quote-file-name-part-of-cmd',
;;    `icicle-readable-to-markers', `icicle-read-buffer',
;;    `icicle-read-char-by-name', `icicle-read-char-exclusive',
;;    `icicle-read-char-maybe-completing', `icicle-read-face-name',
;;    `icicle-read-file-name', `icicle-read-file-name-default',
;;    `icicle-read-from-minibuffer',
;;    `icicle-read-from-minibuf-nil-default', `icicle-read-number',
;;    `icicle-read-regexp', `icicle-read-shell-command',
;;    `icicle-read-shell-command-completing', `icicle-read-string',
;;    `icicle-read-string-completing', `icicle-repeat-command',
;;    `icicle-recentf-include-p', `icicle-recentf-keep-p',
;;    `icicle-recentf-make-menu-items', `icicle-recompute-candidates',
;;    `icicle-remove-color-duplicates', `icicle-remove-dots',
;;    `icicle-remove-duplicates', `icicle-remove-dups-if-extras',
;;    `icicle-remove-if', `icicle-remove-if-not',
;;    `icicle-remove-property', `icicle-replace-mct-cand-in-mct',
;;    `icicle-require-match-p', `icicle-restore-standard-commands',
;;    `icicle-restore-standard-options',
;;    `icicle-restore-std-completion-fns', `icicle-reversible-sort',
;;    `icicle-saved-fileset-p', `icicle-save-or-restore-input',
;;    `icicle-save-raw-input', `icicle-scatter',
;;    `icicle-scatter-match', `icicle-scroll-or-update-Completions',
;;    `icicle-set-difference', `icicle-set-intersection',
;;    `icicle-set-union', `icicle-some', `icicle-special-candidate-p',
;;    `icicle-special-candidates-first-p', `icicle-special-display-p',
;;    `icicle-special-variable-p',
;;    `icicle-start-of-candidates-in-Completions',
;;    `icicle-string-match-p', `icicle-strip-ignored-files-and-sort',
;;    `icicle-subst-envvar-in-file-name',
;;    `icicle-substring-no-properties', `icicle-substrings-of-length',
;;    `icicle-take', `icicle-toggle-icicle-mode-twice',
;;    `icicle-transform-candidates',
;;    `icicle-transform-multi-completion', `icicle-try-switch-buffer',
;;    `icicle-ucs-names', `icicle-unhighlight-lighter',
;;    `icicle-unlist', `icicle-unpropertize-completion',
;;    `icicle-unsorted-apropos-candidates',
;;    `icicle-unsorted-file-name-apropos-candidates',
;;    `icicle-unsorted-file-name-prefix-candidates',
;;    `icicle-unsorted-prefix-candidates', `icicle-upcase',
;;    `icicle-value-satisfies-type-p', `icicle-var-inherits-type-p',
;;    `icicle-var-is-of-type-p', `icicle-var-matches-type-p',
;;    `icicle-var-val-satisfies-type-p', `icicle-window-at-bottom-p',
;;    `icicle-window-at-left-p', `icicle-window-at-right-p',
;;    `icicle-window-at-top-p', `icicle-window-dedicated-p',
;;    `icicle-window-deletable-p', `icicle-window-invisible-p',
;;    `select-frame-set-input-focus'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-crm-local-completion-map',
;;    `icicle-crm-local-must-match-map', `icicle-dirs-done',
;;    `icicle-files', `icicle-ORIG-crm-local-completion-map',
;;    `icicle-ORIG-crm-local-must-match-map'.
;;
;;
;;  ***** NOTE: This vanilla Emacs function is defined here for
;;              Emacs 20, where it does not exist.
;;
;;    `replace-regexp-in-string' (Emacs 20).
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;    `face-valid-attribute-values'  - (See doc string.)
;;    `read-buffer'                  - (See doc string.)
;;    `read-string'                  - (See doc string.)
;;
;;
;;  ***** NOTE: The following functions defined in `crm.el'
;;              have been REDEFINED HERE:
;;
;;    `completing-read-multiple'     - Inhibit Icicles features.
;;
;;
;;  ***** NOTE: The following functions defined in `dired-aux.el' and
;;              `dired-x.el' have been REDEFINED HERE:
;;
;;    `dired-read-shell-command'     - Use Icicles completion.
;;                                   - Added optional arg HISTORY.
;;    `dired-smart-shell-command'    - Icicles completion (Emacs<23).
;;
;;
;;  ***** NOTE: The following functions defined in `faces.el'
;;              have been REDEFINED HERE:
;;
;;    `read-face-name'    -  Show face names with the faces they name.
;;
;;
;;  ***** NOTE: The following function defined in `filesets.el' has
;;              been REDEFINED HERE:
;;
;;    `filesets-get-filelist'        - Fixed for Emacs bug #976.
;;
;;
;;  ***** NOTE: The following function defined in `files-x.el' is
;;              ADVISED HERE:
;;
;;    `read-file-local-variable'     - Provide predicates for `M-&'.
;;
;;
;;  ***** NOTE: The following functions defined in `minibuffer.el'
;;              have been REDEFINED HERE:
;;
;;    `completing-read'              - (See doc string.)
;;    `display-completion-list'      - (See doc string.)
;;    `read-file-name' Emacs 20, 21 only - (See doc string.)
;;    `read-from-minibuffer'         - (See doc string.)
;;
;;
;;  ***** NOTE: The following functions defined in `mule-cmds.el'
;;              have been REDEFINED HERE:
;;
;;    `read-char-by-name'            - Use `icicle-ucs-names'.
;;                                     Display the char also.
;;                                     Added optional arg NAMES.
;;
;;
;;  ***** NOTE: The following function defined in `recentf.el' has
;;              been REDEFINED HERE:
;;
;;    `recentf-make-menu-items'      - Add Icicles submenu.
;;
;;
;;  ***** NOTE: The following functions defined in `simple.el' have
;;              been REDEFINED HERE:
;;
;;    `choose-completion-string'  - Don't exit minibuffer after
;;                                  `(icicle-)-lisp-complete-symbol'
;;                                  completion.
;;    `completion-setup-function' - 1. Put faces on inserted strings.
;;                                  2. Help on help.
;;    `minibuffer-default-add-completions' - Respect Icicles filters.
;;    `read-shell-command'        - Use Icicles completion.
;;    `repeat-complex-command'    - Use `completing-read'.
;;    `shell-command'             - Use Icicles completion (Emacs<23).
;;    `shell-command-on-region'   - Use Icicles completion (Emacs<23).
;;
;;
;;  ***** NOTE: The following functions defined in `subr.el' have
;;              been REDEFINED HERE:
;;
;;  `read-number'                  - You can enter a numeric var name.
;;                                   Allow completion.  Handle errors.
;;
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
;;  (@> "Macros")
;;  (@> "Redefined Standard Functions")
;;  (@> "Icicles Functions - Completion Display (Not Cycling)")
;;  (@> "Icicles Functions - TAB Completion Cycling")
;;  (@> "Icicles Functions - S-TAB Completion Cycling")
;;  (@> "Icicles Functions - Common Helper Functions")
;;  (@> "Icicles Functions - Sort Functions")
;;  (@> "Icicles Predicates for Different Candidate Types")
;;    (@> "Bookmark-Completion Predicates")
;;    (@> "Buffer-Completion Predicates")
;;    (@> "Color-Completion Predicates")
;;    (@> "Face-Completion Predicates")
;;    (@> "File- and Directory-Completion Predicates")
;;    (@> "Frame-Completion Predicates")
;;    (@> "Package-Completion Predicates")
;;    (@> "Special Candidate-Completion Predicates")
;;    (@> "Symbol-Completion Predicates")
;;    (@> "Window-Completion Predicates")
 
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

(eval-when-compile (require 'cl)) ;; case, lexical-let, loop

(require 'hexrgb nil t) ;; (no error if not found): hexrgb-color-name-to-hex
(require 'wid-edit+ nil t) ;; (no error if not found):
                           ;; redefined color widget (for icicle-var-is-of-type-p)

(eval-when-compile
 (or (condition-case nil
         (load-library "icicles-mac")   ; Use load-library to ensure latest .elc.
       (error nil))
     (require 'icicles-mac)))           ; Require, so can load separately if not on `load-path'.
  ;; icicle-with-selected-window

(require 'icicles-opt)                  ; (This is required anyway by `icicles-var.el'.)
  ;; icicle-add-proxy-candidates-flag, icicle-buffer-ignore-space-prefix-flag,
  ;; icicle-ffap-guesser, icicle-Completions-display-min-input-chars, icicle-current-TAB-method,
  ;; icicle-expand-input-to-common-match, icicle-hide-common-match-in-Completions-flag,
  ;; icicle-hide-non-matching-lines-flag, icicle-highlight-historical-candidates-flag,
  ;; icicle-highlight-input-initial-whitespace-flag, icicle-incremental-completion-delay,
  ;; icicle-incremental-completion, icicle-incremental-completion-threshold,
  ;; icicle-default-value, icicle-list-join-string, icicle-mark-position-in-candidate,
  ;; icicle-point-position-in-candidate, icicle-regexp-quote-flag, icicle-require-match-flag,
  ;; icicle-shell-command-candidates-cache, icicle-show-Completions-help-flag, icicle-sort-comparer,
  ;; icicle-sort-orders-alist, icicle-special-candidate-regexp, icicle-transform-function,
  ;; icicle-use-~-for-home-dir-flag

(require 'icicles-var)
  ;; icicle-abs-file-candidates, icicle-all-candidates-action, icicle-apropos-complete-match-fn,
  ;; icicle-auto-no-icomplete-mode-p, icicle-auto-no-sort-p, icicle-buffer-name-input-p,
  ;; icicle-candidate-alt-action-fn, icicle-candidate-nb, icicle-candidate-action-fn,
  ;; icicle-candidate-properties-alist, icicle-candidates-alist, icicle-cmd-calling-for-completion,
  ;; icicle-common-match-string, icicle-comp-base-is-default-dir-p, icicle-complete-input-overlay,
  ;; icicle-completing-keys-p, icicle-completing-p (variable), icicle-completion-candidates,
  ;; icicle-current-completion-mode, icicle-current-input, icicle-current-raw-input, icicle-cycling-p,
  ;; icicle-dir-candidate-can-exit-p, icicle-edit-update-p, icicle-exclude-default-proxies,
  ;; icicle-extra-candidates, icicle-extra-candidates-dir-insert-p, icicle-fancy-candidates-p,
  ;; icicle-fancy-cands-internal-p, icicle-file-name-completion-table, icicle-filtered-default-value,
  ;; icicle-hist-cands-no-highlight, icicle-ignored-extensions-regexp, icicle-incremental-completion-p,
  ;; icicle-initial-value, icicle-input-completion-fail-overlay, icicle-input-fail-pos,
  ;; icicle-last-completion-candidate, icicle-last-icomplete-mode-value, icicle-last-input,
  ;; icicle-last-sort-comparer, icicle-last-top-level-command, icicle-lighter-truncation,
  ;; icicle-list-use-nth-parts, icicle-minibuffer-message-ok-p, icicle-mode-line-help,
  ;; icicle-ms-windows-drive-hash, icicle-multi-completing-p, icicle-must-match-regexp,
  ;; icicle-must-not-match-regexp, icicle-must-pass-predicate, icicle-must-pass-after-match-predicate,
  ;; icicle-nb-candidates-before-truncation, icicle-nb-of-other-cycle-candidates,
  ;; icicle-orig-must-pass-after-match-pred, icicle-orig-read-file-name-fn, icicle-orig-window,
  ;; icicle-pre-minibuffer-buffer, icicle-previous-raw-file-name-inputs,
  ;; icicle-previous-raw-non-file-name-inputs, icicle-proxy-candidate-regexp, icicle-proxy-candidates,
  ;; icicle-read-char-history, icicle-require-match-p, icicle-remove-icicles-props-p, icicle-re-no-dot,
  ;; icicle-reverse-multi-sort-p, icicle-reverse-sort-p, icicle-saved-candidate-overlays,
  ;; icicle-saved-completion-candidate, icicle-saved-completion-candidates, icicle-transform-before-sort-p,
  ;; icicle-whole-candidate-as-text-prop-p, lacarte-menu-items-alist

;; This requirement is real, but leads to recursion.
;; You should, in any case, just load everything by loading `icicles.el'.
;; (require 'icicles-mode) ;; icicle-mode


;; Byte-compiling this file, you will likely get some error or warning
;; messages due to differences between different versions of Emacs.


;;; Defvars to quiet the byte-compiler:

(when (< emacs-major-version 22)
  (defvar completion-annotate-function)
  (defvar completion-common-substring)
  (defvar completion-extra-properties)
  (defvar completion-list-insert-choice-function)
  (defvar completion-root-regexp)
  (defvar minibuffer-completing-symbol)
  (defvar minibuffer-prompt-properties)
  (defvar mouse-1-click-follows-link)
  (defvar partial-completion-mode)
  (defvar read-file-name-completion-ignore-case)
  (defvar minibuffer-local-filename-completion-map)
  (defvar minibuffer-local-must-match-filename-map)
  (defvar minibuffer-local-filename-must-match-map)
  (defvar read-file-name-predicate)
  (defvar tooltip-mode))

(when (< emacs-major-version 23)
  (defvar completion--embedded-envvar-re) ; In `minibuffer.el'.
  (defvar completion-styles)            ; In `minibuffer.el'
  (defvar icicle-Completions-text-scale-decrease) ; In `icicles-opt.el' (for Emacs 23+)
  (defvar icicle-read-char-by-name-multi-completion-flag)) ; In `icicles-opt.el' (for Emacs 23+)

(defvar completion-root-regexp)         ; In `simple.el' (for Emacs 22 and 23.1)
(defvar crm-local-completion-map)       ; In `crm.el'
(defvar crm-local-must-match-map)       ; In `crm.el'
(defvar crm-separator)                  ; In `crm.el'
(defvar doremi-boost-down-keys)         ; In `doremi.el'
(defvar doremi-boost-up-keys)           ; In `doremi.el'
(defvar doremi-down-keys)               ; In `doremi.el'
(defvar doremi-up-keys)                 ; In `doremi.el'
(defvar eyedrop-picked-background)      ; In `eyedrop.el' and `palette.el'
(defvar eyedrop-picked-foreground)      ; In `eyedrop.el' and `palette.el'
(defvar ffap-alist)                     ; In `ffap.el'
(defvar ffap-url-regexp)                ; In `ffap.el'
(defvar ffap-shell-prompt-regexp)       ; In `ffap.el'
(defvar ffap-machine-p-known)           ; In `ffap.el'
(defvar filesets-data)                  ; In `filesets.el'
(defvar font-width-table)               ; In C code.
(defvar font-weight-table)              ; In C code.
(defvar font-slant-table)               ; In C code.
(defvar history-delete-duplicates)      ; In C code for Emacs 22+.
(defvar icicle-file-name-completion-table) ; In `icicles-var.el' for Emacs 24+.
(defvar icicle-Info-index-nodes)        ; In `icicles-cmd2.el'
(defvar icicle-Info-manual)             ; In `icicles-cmd2.el'
(defvar icicle-read-char-history)       ; In `icicles-var.el' for Emacs 23+.
(defvar image-dired-thumb-height)       ; In `image-dired.el'.
(defvar image-dired-thumb-width)        ; In `image-dired.el'.
(defvar last-repeatable-command)        ; Defined in `repeat.el'.
(defvar list-colors-sort)               ; In `facemenu.el'
(defvar 1on1-*Completions*-frame-flag)  ; In `oneonone.el'
(defvar minibuffer-default-in-prompt-regexps) ; In `minibuf-eldef.el'.
(defvar minibuffer-local-filename-syntax) ; In `minibuffer.el' for Emacs 24+.
(defvar read-buffer-completion-ignore-case) ; Emacs 23+.
(defvar recentf-list)                   ; In `recentf.el'
(defvar recentf-menu-filter-commands)
(defvar recentf-menu-filter)
(defvar recentf-max-menu-items)
(defvar recentf-menu-open-all-flag)
(defvar recentf-menu-filter-commands)
(defvar recentf-menu-items-for-commands)
(defvar shell-completion-execonly)      ; In `shell.el'
(defvar ucs-names)                      ; In `mule-cmds.el'.




;; The name changed during development of Emacs 23.  They aliased it for 23.1, but removed it for 23.2.
;; Use the new name and alias the old, but don't declare old obsolete (let Emacs 23 do that.)
(when (and (boundp 'minibuffer-local-must-match-filename-map)  (fboundp 'defvaralias)) ; Emacs 22
  (defvar minibuffer-local-filename-must-match-map minibuffer-local-must-match-filename-map
    "Local keymap for minibuffer input with completion for filenames with exact match.")
  (defvaralias 'minibuffer-local-must-match-filename-map 'minibuffer-local-filename-must-match-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



 
;;(@* "Macros")

;;; Macros -----------------------------------------------------------

(defmacro icicle-maybe-cached-action (action)
  "Evaluate and return ACTION or `icicle-all-candidates-action'.
If `icicle-all-candidates-action' is nil, use ACTION.
If it is t, then set it to the value of ACTION, so the next call
 returns the same value."
  `(if icicle-all-candidates-action
    (if (eq icicle-all-candidates-action t)
        (setq icicle-all-candidates-action  ,action)
      icicle-all-candidates-action)
    ,action))

;; Same as vanilla definition.  Needed for byte-compiling.
(defmacro minibuffer-with-setup-hook (fun &rest body)
  "Temporarily add FUN to `minibuffer-setup-hook' while executing BODY.
BODY should use the minibuffer at most once.
Recursive uses of the minibuffer are unaffected (FUN is not
called additional times).

This macro actually adds an auxiliary function that calls FUN,
rather than FUN itself, to `minibuffer-setup-hook'."
  ;; (declare (indent 1) (debug t))
  (let ((hook  (make-symbol "setup-hook")))
    `(let (,hook)
      (setq ,hook  (lambda ()
                     ;; Clear out this hook so it does not interfere
                     ;; with any recursive minibuffer usage.
                     (remove-hook 'minibuffer-setup-hook ,hook)
                     (funcall ,fun)))
      (unwind-protect
           (progn (add-hook 'minibuffer-setup-hook ,hook) ,@body)
        (remove-hook 'minibuffer-setup-hook ,hook)))))
 
;;(@* "Redefined Standard Functions")

;;; Redefined Standard Functions -------------------------------------


;; REPLACE ORIGINAL `choose-completion-string' in `simple.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Don't exit minibuffer if this is just `(icicle-)lisp-complete-symbol' completion.
;; Go to point-max before insert choice.  Respect `icicle-dir-candidate-can-exit-p'.
;;
;; Free variable `completion-reference-buffer' is defined in `simple.el'.
;;
(unless (fboundp 'icicle-ORIG-choose-completion-string)
  (defalias 'icicle-ORIG-choose-completion-string (symbol-function 'choose-completion-string)))

(cond ((= emacs-major-version 22)                
       (defun icicle-choose-completion-string (choice &optional buffer base-size)
         "Switch to BUFFER and insert the completion choice CHOICE.
BASE-SIZE, if non-nil, says how many characters of BUFFER's text
to keep.  If it is nil, we call `choose-completion-delete-max-match'
to decide what to delete.
If BUFFER is the minibuffer, then exit the minibuffer, unless one of
the following is true:
   - it is reading a file name, CHOICE is a directory, and
     `icicle-dir-candidate-can-exit-p' is nil
   - `completion-no-auto-exit' is non-nil
   - this is just `(icicle-)lisp-complete-symbol' completion."
         (let* ((buffer  (or buffer  completion-reference-buffer))
                (mini-p  (minibufferp buffer)))
           ;; If BUFFER is a minibuffer, barf unless it's currently active.
           (if (and mini-p  (or (not (active-minibuffer-window))
                                (not (equal buffer (window-buffer (active-minibuffer-window))))))
               (icicle-user-error "Minibuffer is not active for completion")
             ;; Set buffer so buffer-local `choose-completion-string-functions' works.
             (set-buffer buffer)
             (unless (run-hook-with-args-until-success 'choose-completion-string-functions
                                                       choice buffer mini-p base-size)
;;; $$$$$$ Removed this because it led to an error in Emacs 24, since base-size is nil there.
;;;        Anyway, Icicles doesn't really need or use base-size or `choose-completion-delete-max-match'.
;;;                ;; Insert the completion into the buffer where completion was requested.
;;;                (if base-size
;;;                    (delete-region (+ base-size (if mini-p (minibuffer-prompt-end) (point-min)))
;;;                                   (if mini-p (point-max) (point)))
;;;                  (choose-completion-delete-max-match choice))

               ;; Forget about base-size altogether.  Replace the whole input always.
               (delete-region (+ (or base-size  0) (if mini-p (minibuffer-prompt-end) (point-min)))
                              (if mini-p (point-max) (point)))
               (when mini-p (goto-char (point-max))) ; $$$$$ (was unconditional)
               (insert choice)
               (remove-text-properties (- (point) (length choice)) (point) '(mouse-face nil))
               ;; Update point in the window that BUFFER is showing in.
               (let ((window  (get-buffer-window buffer 0))) (set-window-point window (point)))
               ;; If completing for the minibuffer, exit it with this choice,
               ;; unless this was `(icicle-)lisp-complete-symbol' completion.
               (and (not completion-no-auto-exit)
                    (equal buffer (window-buffer (minibuffer-window)))
                    (or minibuffer-completion-table
                        (and icicle-mode  (or icicle-extra-candidates  icicle-proxy-candidates)))
                    (not (memq icicle-cmd-calling-for-completion
                               '(icicle-lisp-complete-symbol lisp-complete-symbol)))
                    ;; Exit the minibuffer if `icicle-dir-candidate-can-exit-p',
                    ;; or not reading a file name, or chosen file is not a directory.
                    (if (or icicle-dir-candidate-can-exit-p
                            (not (eq minibuffer-completion-table 'read-file-name-internal))
                            (not (file-directory-p (field-string (point-max)))))
                        (exit-minibuffer)
                      (let ((mini  (active-minibuffer-window)))
                        (select-window mini)
                        (when minibuffer-auto-raise (raise-frame (window-frame mini)))))))))))

      ((and (= emacs-major-version 23)  (= emacs-minor-version 1)) ; Emacs 23.1
       (defun icicle-choose-completion-string (choice &optional buffer base-size)
         "Switch to BUFFER and insert the completion choice CHOICE.
BASE-SIZE, if non-nil, says how many characters of BUFFER's text
to keep.  If it is nil, we call `choose-completion-delete-max-match'
to decide what to delete.
If BUFFER is the minibuffer, then exit the minibuffer, unless one of
the following is true:
   - it is reading a file name, CHOICE is a directory, and
     `icicle-dir-candidate-can-exit-p' is nil
   - `completion-no-auto-exit' is non-nil
   - this is just `(icicle-)lisp-complete-symbol' completion."
         (let* ((buffer  (or buffer  completion-reference-buffer))
                (mini-p  (minibufferp buffer)))
           ;; If BUFFER is a minibuffer, barf unless it's currently active.
           (if (and mini-p  (or (not (active-minibuffer-window))
                                (not (equal buffer (window-buffer (active-minibuffer-window))))))
               (icicle-user-error "Minibuffer is not active for completion")
             (set-buffer buffer)        ; So buffer-local `choose-completion-string-functions' works.
             (unless (run-hook-with-args-until-success 'choose-completion-string-functions
                                                       choice buffer mini-p base-size)
               ;; Insert the completion into the buffer where it was requested.
               ;; Vanilla Emacs FIXME:
               ;; - There may not be a field at point, or there may be a field but it is not a
               ;;   "completion field", in which case we have to call `choose-completion-delete-max-match',
               ;;   even if BASE-SIZE is set.
               ;; - We may need to delete further than (point) to (field-end), depending on the
               ;;   `completion-style', and for that we need extra data `completion-extra-size'.
               (if base-size
                   (delete-region (+ base-size (field-beginning)) (point))
                 (choose-completion-delete-max-match choice))
               (insert choice)
               (remove-text-properties (- (point) (length choice)) (point) '(mouse-face nil))
               ;; Update point in the window that BUFFER is showing in.
               (let ((window  (get-buffer-window buffer 0))) (set-window-point window (point)))
               ;; If completing for the minibuffer, exit it with this choice,
               ;; unless this was `(icicle-)lisp-complete-symbol' completion.
               (and (not completion-no-auto-exit)
                    (minibufferp buffer)
                    (or minibuffer-completion-table
                        (and icicle-mode  (or icicle-extra-candidates  icicle-proxy-candidates)))
                    (not (memq icicle-cmd-calling-for-completion
                               '(icicle-lisp-complete-symbol lisp-complete-symbol)))
                    ;; Exit the minibuffer if `icicle-dir-candidate-can-exit-p',
                    ;; or not reading a file name, or chosen file is not a directory.
                    (if (or icicle-dir-candidate-can-exit-p
                            (not (eq minibuffer-completion-table 'read-file-name-internal))
                            (not (file-directory-p (field-string (point-max)))))
                        (exit-minibuffer)
                      (let ((mini  (active-minibuffer-window)))
                        (select-window mini)
                        (when minibuffer-auto-raise (raise-frame (window-frame mini)))))))))))

      ((or (> emacs-major-version 23)   ; Emacs 23.2+
           (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
       (defun icicle-choose-completion-string (choice &optional buffer base-position insert-function)
         "Switch to BUFFER and insert the completion choice CHOICE.
BASE-POSITION should be a cons whose car is the position where the
 choice is inserted.  It is ignored if not a cons.
INSERT-FUNCTION says how to insert the completion and falls
 back on `completion-list-insert-choice-function' when nil.
If BUFFER is the minibuffer, then exit the minibuffer, unless one of
 the following is true:
   - it is reading a file name, CHOICE is a directory, and
     `icicle-dir-candidate-can-exit-p' is nil
   - `completion-no-auto-exit' is non-nil
   - this is just `(icicle-)lisp-complete-symbol' completion."
         (unless (consp base-position)  ; Older code may pass BASE-SIZE instead of BASE-POSITION.  Ignore it.
           ;; No, do not display this message.
           ;; (message "Obsolete BASE-SIZE argument passed to `choose-completion-string'")
           (setq base-position  nil))
         (let* ((buffer  (or buffer  completion-reference-buffer))
                (mini-p  (minibufferp buffer)))
           ;; If BUFFER is a minibuffer, barf unless it is currently active.
           (if (and mini-p  (or (not (active-minibuffer-window))
                                (not (equal buffer (window-buffer (active-minibuffer-window))))))
               (icicle-user-error "Minibuffer is not active for completion")
             ;; Set buffer so buffer-local `choose-completion-string-functions' works.
             (set-buffer buffer)
             (unless (run-hook-with-args-until-success
                      'choose-completion-string-functions
                      ;; 4th arg used to be MINI-P, but it was useless and unused - can just use
                      ;; (minibufferp BUFFER).  The last arg used to be BASE-SIZE - keep it to avoid
                      ;; breaking older code.
                      choice buffer base-position nil)
               (let ((choice-copy  (copy-sequence choice)) ; Do not modify original string.
                     (start        (if mini-p
                                       (minibuffer-prompt-end)
                                     (previous-single-property-change (point) 'read-only nil (point-min))))
                     (end          (if mini-p
                                       (point-max)
                                     (next-single-property-change (point) 'read-only nil (point-max)))))
                 ;; Do not assume that properties have been removed - remove `mouse-face' here.
                 (remove-text-properties 0 (length choice-copy) '(mouse-face nil) choice-copy)
                 (delete-region start end) ; Replace the whole input always.
                 (if (or insert-function  (boundp 'completion-list-insert-choice-function))
                     (funcall (or insert-function  completion-list-insert-choice-function)
                              (or (car  base-position)  (point))
                              (or (cadr base-position)  (point))
                              choice-copy)
                   (insert choice-copy)))

               ;; $$$$$$$$ (remove-text-properties (- (point) (length choice)) (point) '(mouse-face nil)))

               ;; Update point in the window where BUFFER is showing.
               (let ((window  (get-buffer-window buffer t))) (set-window-point window (point)))
               ;; If completing for the minibuffer, exit it with this choice,
               ;; unless this was `(icicle-)lisp-complete-symbol' completion.
               (and (not completion-no-auto-exit)
                    (minibufferp buffer)
                    (or minibuffer-completion-table
                        (and icicle-mode  (or icicle-extra-candidates  icicle-proxy-candidates)))
                    (not (memq icicle-cmd-calling-for-completion
                               '(icicle-lisp-complete-symbol lisp-complete-symbol)))
                    ;; Exit the minibuffer if `icicle-dir-candidate-can-exit-p',
                    ;; or not reading a file name, or chosen file is not a directory.
                    (let* ((result  (buffer-substring (field-beginning) (point)))
                           (bounds  (completion-boundaries result minibuffer-completion-table
                                                           minibuffer-completion-predicate "")))
                      (if (or icicle-dir-candidate-can-exit-p
                              (not (eq (car bounds) (length result))))
                          ;; $$$$$$ (not (eq minibuffer-completion-table 'read-file-name-internal))
                          ;; $$$$$$ (not (file-directory-p (field-string (point-max)))))
                          (exit-minibuffer)
                        ;; The candidate chosen leads to a new set of candidates (e.g., it is a dir).
                        ;; Do not exit the minibuffer yet.
                        (let ((mini  (active-minibuffer-window)))
                          (select-window mini)
                          (when minibuffer-auto-raise (raise-frame (window-frame mini))))))))))))

      ((= emacs-major-version 21)       ; Emacs 21
       (defun icicle-choose-completion-string (choice &optional buffer base-size)
         "Switch to BUFFER and insert the completion choice CHOICE.
BASE-SIZE, if non-nil, says how many characters of BUFFER's text
to keep.  If it is nil, we call `choose-completion-delete-max-match'
to decide what to delete.
If BUFFER is the minibuffer, then exit the minibuffer, unless one of
the following is true:
   - it is reading a file name, CHOICE is a directory, and
     `icicle-dir-candidate-can-exit-p' is nil
   - `completion-no-auto-exit' is non-nil
   - this is just `(icicle-)lisp-complete-symbol' completion."
         (let* ((buffer  (or buffer  completion-reference-buffer))
                (mini-p  (icicle-string-match-p "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))))
           ;; If BUFFER is a minibuffer, barf unless it's currently active.
           (if (and mini-p  (or (not (active-minibuffer-window))
                                (not (equal buffer (window-buffer (active-minibuffer-window))))))
               (icicle-user-error "Minibuffer is not active for completion")
             ;; Insert the completion into the buffer where completion was requested.
             (set-buffer buffer)
             (if base-size
                 (delete-region (+ base-size (if mini-p (icicle-minibuffer-prompt-end) (point-min)))
                                (if mini-p (point-max) (point)))
               (choose-completion-delete-max-match choice))
             (when mini-p (goto-char (point-max))) ; $$$$$ (was unconditional)
             (insert choice)
             (remove-text-properties (- (point) (length choice)) (point) '(mouse-face nil))
             ;; Update point in the window that BUFFER is showing in.
             (let ((window  (get-buffer-window buffer 0))) (set-window-point window (point)))
             ;; If completing for the minibuffer, exit it with this choice,
             ;; unless this was `(icicle-)lisp-complete-symbol' completion.
             (and (not completion-no-auto-exit)
                  (equal buffer (window-buffer (minibuffer-window)))
                  (or minibuffer-completion-table
                      (and icicle-mode  (or icicle-extra-candidates  icicle-proxy-candidates)))
                  (not (memq icicle-cmd-calling-for-completion
                               '(icicle-lisp-complete-symbol lisp-complete-symbol)))
                  ;; Exit the minibuffer if `icicle-dir-candidate-can-exit-p',
                  ;; or not reading a file name, or chosen file is not a directory.
                  (if (or icicle-dir-candidate-can-exit-p
                          (not (eq minibuffer-completion-table 'read-file-name-internal))
                          (not (file-directory-p (field-string (point-max)))))
                      (exit-minibuffer)
                    (let ((mini  (active-minibuffer-window)))
                      (select-window mini)
                      (when minibuffer-auto-raise (raise-frame (window-frame mini))))))))))

      (t                                ; Emacs 20
       (defun icicle-choose-completion-string (choice &optional buffer base-size)
         "Switch to BUFFER and insert the completion choice CHOICE.
 BASE-SIZE, if non-nil, says how many characters of BUFFER's text
 to keep.  If it is nil, we call `choose-completion-delete-max-match'
 to decide what to delete.
 If BUFFER is the minibuffer, then exit the minibuffer, unless one of
 the following is true:
    - it is reading a file name, CHOICE is a directory, and
      `icicle-dir-candidate-can-exit-p' is nil
    - `completion-no-auto-exit' is non-nil
    - this is just `(icicle-)lisp-complete-symbol' completion."
         (let* ((buffer  (or buffer  completion-reference-buffer))
                (mini-p  (icicle-string-match-p "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))))
           ;; If BUFFER is a minibuffer, barf unless it's currently active.
           (when (and mini-p  (or (not (active-minibuffer-window))
                                  (not (equal buffer (window-buffer (active-minibuffer-window))))))
             (icicle-user-error "Minibuffer is not active for completion"))
           ;; Insert the completion into the buffer where completion was requested.
           (set-buffer buffer)
           (if base-size
               (delete-region (+ base-size (point-min)) (if mini-p (point-max) (point)))
             (choose-completion-delete-max-match choice))
           (when mini-p (goto-char (point-max))) ; $$$$$ (was unconditional)
           (insert choice)
           (remove-text-properties (- (point) (length choice)) (point) '(mouse-face nil))
           ;; Update point in the window that BUFFER is showing in.
           (let ((window  (get-buffer-window buffer 0))) (set-window-point window (point)))
           ;; If completing for the minibuffer, exit it with this choice,
           ;; unless this was `(icicle-)lisp-complete-symbol' completion.
           (and (not completion-no-auto-exit)
                (equal buffer (window-buffer (minibuffer-window)))
                (or minibuffer-completion-table
                    (and icicle-mode  (or icicle-extra-candidates  icicle-proxy-candidates)))
                (not (memq icicle-cmd-calling-for-completion
                               '(icicle-lisp-complete-symbol lisp-complete-symbol)))
                ;; Exit the minibuffer if `icicle-dir-candidate-can-exit-p',
                ;; or not reading a file name, or chosen file is not a directory.
                (if (or icicle-dir-candidate-can-exit-p
                        (not (eq minibuffer-completion-table 'read-file-name-internal))
                        (not (file-directory-p (buffer-string))))
                    (exit-minibuffer)
                  (select-window (active-minibuffer-window))))))))


;; REPLACE ORIGINAL `completion-setup-function' in `simple.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Don't print the help lines here.  Do that in `icicle-display-completion-list' instead.
;; That's so we can fit the `*Completions*' window to the buffer, including the help lines.
;;
(unless (fboundp 'icicle-ORIG-completion-setup-function)
  (defalias 'icicle-ORIG-completion-setup-function (symbol-function 'completion-setup-function)))

(when (< emacs-major-version 22)
  (defun icicle-completion-setup-function ()
    "Set up for completion.  This goes in `completion-setup-hook'
so it is called after completion-list buffer text is written."
    (save-excursion
      (let* ((mainbuf        (current-buffer))
             (mbuf-contents  (icicle-input-from-minibuffer))
             (dir-of-input   (and minibuffer-completing-file-name
                                  ;; Emacs 20 bug: `substitute-in-file-name' barfs on "foo$": use condition-case.
                                  (condition-case nil
                                      (icicle-file-name-directory
                                       (expand-file-name (substitute-in-file-name mbuf-contents)))
                                    (error nil)))))
        ;; If reading file name and either `icicle-comp-base-is-default-dir-p' is nil or this is a
        ;; completion command, then set `default-directory' so it will be copied into `*Completions*'.
        (when (and dir-of-input  (or (icicle-get-safe this-command 'icicle-completing-command)
                                     (not icicle-comp-base-is-default-dir-p)))
          (with-current-buffer mainbuf (setq default-directory  dir-of-input)))
        (with-current-buffer standard-output
          (completion-list-mode)
          (set (make-local-variable 'completion-reference-buffer) mainbuf)
          (setq completion-base-size
                (cond ((and (eq minibuffer-completion-table 'read-file-name-internal)
                            icicle-comp-base-is-default-dir-p
                            (length default-directory)))
                      ((eq minibuffer-completion-table 'read-file-name-internal)
                       ;; For file name completion, use the number of chars before
                       ;; the start of the file name component at point.
                       (with-current-buffer mainbuf
                         (save-excursion (skip-chars-backward "^/")
                                         (- (point) (icicle-minibuffer-prompt-end)))))
                      ((icicle-string-match-p "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name mainbuf))
                       ;; Otherwise, in minibuffer, the whole input is being completed.
                       0))))))))

(when (or (= emacs-major-version 22)    ; Emacs 22 or 23.1
          (and (= emacs-major-version 23)  (= emacs-minor-version 1)))
  (defun icicle-completion-setup-function ()
    "Set up for completion.  This goes in `completion-setup-hook'
so it is called after completion-list buffer text is written."
    (save-excursion
      (let* ((mainbuf        (current-buffer))
             (mbuf-contents  (minibuffer-completion-contents)) ; Get contents only up to point.
             (dir-of-input   (and minibuffer-completing-file-name
                                  (icicle-file-name-directory
                                   (expand-file-name (substitute-in-file-name mbuf-contents)))))
             common-string-length)
        ;; If reading file name and either `icicle-comp-base-is-default-dir-p' is nil or this is a
        ;; completion command, then set `default-directory' so it will be copied into `*Completions*'.
        (when (and dir-of-input  (or (icicle-get-safe this-command 'icicle-completing-command)
                                     (not icicle-comp-base-is-default-dir-p)))
          (with-current-buffer mainbuf (setq default-directory  dir-of-input)))
        (with-current-buffer standard-output
          (completion-list-mode)
          (set (make-local-variable 'completion-reference-buffer) mainbuf)
          (setq completion-base-size
                (cond ((and minibuffer-completing-file-name  icicle-comp-base-is-default-dir-p
                            (length default-directory)))
                      ((icicle-get-safe minibuffer-completion-table 'completion-base-size-function)
                       ;; To compute base size, a function can use the global value of
                       ;; `completion-common-substring' or `minibuffer-completion-contents'.
                       (with-current-buffer mainbuf
                         (funcall (get minibuffer-completion-table 'completion-base-size-function))))
                      (minibuffer-completing-file-name
                       ;; For file name completion, use the number of chars before
                       ;; the start of the file name component at point.
                       (with-current-buffer mainbuf
                         (save-excursion (skip-chars-backward completion-root-regexp)
                                         (- (point) (minibuffer-prompt-end)))))
                      ((and (boundp 'minibuffer-completing-symbol)  minibuffer-completing-symbol)
                       nil)
                      ;; Otherwise, in minibuffer, the base size is 0.
                      ((minibufferp mainbuf) 0)))
          (setq common-string-length
                (cond (completion-common-substring (length completion-common-substring))
                      (completion-base-size (- (length mbuf-contents) completion-base-size))))
          ;; Put faces on first uncommon characters and common parts.
          (when (and (integerp common-string-length)  (>= common-string-length 0))
            (let ((element-start  (point-min))
                  (maxp           (point-max))
                  element-common-end)
              (while (and (setq element-start  (next-single-property-change element-start 'mouse-face))
                          (< (setq element-common-end  (+ element-start common-string-length))
                             maxp))
                (when (get-char-property element-start 'mouse-face)
                  (when (and (> common-string-length 0)
                             (get-char-property (1- element-common-end) 'mouse-face))
                    (put-text-property element-start element-common-end
                                       'font-lock-face 'completions-common-part))
                  (when (get-char-property element-common-end 'mouse-face)
                    (put-text-property element-common-end (1+ element-common-end)
                                       'font-lock-face 'completions-first-difference)))))))))))

(when (or (> emacs-major-version 23)    ; Emacs 23.2+
          (and (= emacs-major-version 23)  (>= emacs-minor-version 2)))
  (defun icicle-completion-setup-function ()
    "Set up for completion.  This goes in `completion-setup-hook'
so it is called after completion-list buffer text is written."
    ;; I could perhaps get rid of even more of the vanilla vestiges here...
    (save-excursion
      (let ((mainbuf       (current-buffer))
            (dir-of-input  (and minibuffer-completing-file-name
                                (icicle-file-name-directory
                                 (expand-file-name
                                  (substitute-in-file-name (minibuffer-completion-contents)))))))
        ;; If reading file name and either `icicle-comp-base-is-default-dir-p' is nil or this is a
        ;; completion command, then set `default-directory' so it will be copied into `*Completions*'.
        (when (and dir-of-input  (or (icicle-get-safe this-command 'icicle-completing-command)
                                     (not icicle-comp-base-is-default-dir-p)))
          (with-current-buffer mainbuf (setq default-directory  dir-of-input)))
        (with-current-buffer standard-output
          (completion-list-mode)
          (set (make-local-variable 'completion-reference-buffer) mainbuf))))))

(defun icicle-insert-Completions-help-string ()
  "Add or remove help in `*Completions*'.
This is controlled by `icicle-show-Completions-help-flag'.  Show help
only if that option is non-nil."
  (if icicle-show-Completions-help-flag
      (let ((instruction2  (or (and icicle-mode  (substitute-command-keys
                                                  (concat "(\\<minibuffer-local-completion-map>"
                                                          "\\[icicle-minibuffer-help]: help) ")))
                               ""))
            instruction1)
        (cond ((< emacs-major-version 22)
               (setq instruction1  (if window-system ; We have a mouse.
                                       (substitute-command-keys "Click \\<completion-list-mode-map>\
\\[mouse-choose-completion] on a completion to select it.  ")
                                     (substitute-command-keys ; No mouse.
                                      "In this buffer, type \\<completion-list-mode-map>\
\\[choose-completion] to select the completion near point.  "))))
              ((>= emacs-major-version 22)
               (setq instruction1  (if (display-mouse-p) ; We have a mouse.
                                       (substitute-command-keys
                                        (format "Use `%s' or \\<completion-list-mode-map>`\
\\[choose-completion]' on a completion to select it.  "
                                                (if (and (boundp 'mouse-1-click-follows-link)
                                                         mouse-1-click-follows-link)
                                                    'mouse-1
                                                  'mouse-2)))
                                     (substitute-command-keys ; No mouse.
                                      "In this buffer, type \\<completion-list-mode-map>\
\\[choose-completion] to select the completion near point.  ")))))
        (goto-char (point-min))
        (put-text-property 0 (length instruction1) 'face 'icicle-Completions-instruction-1
                           instruction1)
        (put-text-property 0 (length instruction2) 'face 'icicle-Completions-instruction-2
                           instruction2)
        (insert instruction1 instruction2 "\n"))

    ;; Not showing help.  Remove standard Emacs help string.
    (goto-char (point-min))
    (re-search-forward "Possible completions are:\n")
    (delete-region (point-min) (point))))

(defun icicle-read-from-minibuf-nil-default (prompt &optional initial-contents keymap read hist
                                             default-value inherit-input-method)
  "Like `read-from-minibuffer', but return nil for empty input.
Args are as for `read-from-minibuffer'.
If nothing is input, then nil is returned."
  (let ((input  (read-from-minibuffer prompt initial-contents keymap nil hist default-value
                                      inherit-input-method)))
    (if (string= "" input) nil (if read (car (read-from-string input)) input))))

(defun icicle-completing-read-history (prompt &optional hist pred init-input def inherit-i-m)
  "Lax `completing-read' against entries in history HIST.
Arguments are as for `completing-read'.  HIST is a symbol that is a
history variable.  It defaults to `minibuffer-history'.  Completion is
lax: a match is not required."
  (setq hist  (or hist  'minibuffer-history))
  (let ((hist-val  (icicle-remove-duplicates (symbol-value hist))))
    (when (and (consp hist-val)  (not (stringp (car hist-val)))) ; Convert, e.g. `comand-history'.
      (setq hist-val  (mapcar #'prin1-to-string hist-val)))
    (completing-read prompt (mapcar #'list hist-val) pred nil init-input hist def inherit-i-m)))

;; $$$$$$$$ Same as `completing-read-default', except: (a) Added optional arg KEYMAP and
;;                                                     (b) works for all Emacs versions.
(defun icicle-completing-read-default (prompt collection &optional predicate require-match
                                       initial-input hist def inherit-input-method keymap)
  "Default way to read from the minibuffer with completion.
Optional arg KEYMAP, if non-nil, should be a completion keymap.
See `completing-read' for the other arguments."

  (when (consp initial-input)
    ;; `completing-read' uses a 0-based index, but `read-from-minibuffer' uses a 1-based index.
    (setq initial-input  (cons (car initial-input) (1+ (cdr initial-input)))))
  (let* ((minibuffer-completion-table      collection)
         (minibuffer-completion-predicate  predicate)
         (minibuffer-completion-confirm    (and (not (eq require-match t))  require-match))
         (base-map                         (if require-match
                                               minibuffer-local-must-match-map
                                             minibuffer-local-completion-map))
         (map
          (or keymap
              (if (not require-match)   ; keymap
                  (if (or (not minibuffer-completing-file-name)
                          (eq minibuffer-completing-file-name 'lambda)
                          (not (boundp 'minibuffer-local-filename-completion-map)))
                      minibuffer-local-completion-map
                    (if (fboundp 'make-composed-keymap) ; Emacs 24+
                        (make-composed-keymap minibuffer-local-filename-completion-map
                                              minibuffer-local-completion-map)
                      minibuffer-local-filename-completion-map))
                (if (or (not minibuffer-completing-file-name)
                        (eq minibuffer-completing-file-name 'lambda)
                        (and (not (fboundp 'make-composed-keymap)) ; Emacs 24+
                             (not (boundp 'minibuffer-local-filename-must-match-map))))
                    minibuffer-local-must-match-map
                  (if (fboundp 'make-composed-keymap) ; Emacs 24+
                      (make-composed-keymap minibuffer-local-filename-completion-map
                                            minibuffer-local-must-match-map)
                    minibuffer-local-filename-must-match-map)))))
         (result                          (read-from-minibuffer prompt initial-input map
                                                                nil hist def inherit-input-method)))
    ;; Use `icicle-filtered-default-value', not DEF, because `read-from-minibuffer' filters it.
    (when (and (equal result "")  icicle-filtered-default-value)
      (setq result  (if (consp icicle-filtered-default-value) ; Emacs 23+ lets DEF be a list - use first string.
                        (car icicle-filtered-default-value)
                      icicle-filtered-default-value)))
    result))


;; REPLACE ORIGINAL `completing-read' (built-in function),
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Allows for completion candidates that are lists of strings.
;; Allows for reading and returning completion candidates that are strings with properties.
;; Adds completion status indicator to minibuffer and mode-line lighter.
;; Removes `*Completions*' window.
;;
;; We use HIST-m@%=!$+&^*z instead of HIST, to avoid name capture by `minibuffer-history-variable's
;; value.  If we didn't need to be Emacs 20-compatible, then we could employ
;; `#1=#:hist'...`#1#'...`#1' read syntax to use an uninterned symbol.
;;
(unless (fboundp 'icicle-ORIG-completing-read)
  (defalias 'icicle-ORIG-completing-read (symbol-function 'completing-read)))

(defun icicle-completing-read (prompt collection &optional predicate require-match
                               initial-input hist-m@%=!$+&^*z def inherit-input-method keymap)
  "Read string in minibuffer, with completion and cycling of completions.
Prefix completion via \\<minibuffer-local-completion-map>\
`\\[icicle-prefix-word-complete]' (word) and `\\[icicle-prefix-complete]' (full).
Apropos (regexp) completion via `\\[icicle-apropos-complete]'.

Prefix cycling of candidate completions via `\\[icicle-previous-prefix-candidate]' and \
`\\[icicle-next-prefix-candidate]'.
Apropos cycling of candidate completions via `\\[icicle-previous-apropos-candidate]' and \
`\\[icicle-next-apropos-candidate]'.

Cycling of past minibuffer inputs via `\\[previous-history-element]' and \
`\\[next-history-element]'.
Completing past minibuffer inputs via `\\[icicle-insert-history-element]'.

Case is ignored if `completion-ignore-case' is non-nil.
Position of the cursor (point) and the mark during completion cycling
  is determined by `icicle-point-position-in-candidate' and
  `icicle-mark-position-in-candidate', respectively.
Highlighting of the matched part of completion candidates during
  cycling is determined by `icicle-match-highlight-minibuffer',
  `icicle-match-highlight-Completions', and
  `icicle-common-match-highlight-Completions'.

Use `\\[icicle-minibuffer-help]' during completion for more information on completion and key
bindings in Icicle mode.

PROMPT is a string to prompt with. It normally ends in a colon and a
space.  If PROMPT has non-nil text property `icicle-fancy-candidates'
on its first character, then completion candidates can be fancy - they
can have properties.  However, if all of the candidates would be
acceptable to vanilla Emacs, then PROMPT need not use property
`icicle-fancy-candidates', even for candidates that have text
properties.  Property `icicle-fancy-candidates' is needed only for
candidates that require encoding and decoding to store and retrieve
properties.  See the Icicles doc, section `Programming with Fancy
Candidates'.

COLLECTION is an obarray or an alist whose elements' cars are strings.
It can also be a function that performs the completion itself.
In Emacs 22 or later, it can also be a hash table or list of strings.

In Icicle mode, the car of an alist entry can also be a list of
strings.  In this case, the completion candidate is a
multi-completion.  The strings are joined pairwise with
`icicle-list-join-string' to form the completion candidate seen by the
user.  You can use variable `icicle-candidate-properties-alist' to
control the appearance of multi-completions in buffer `*Completions*'.
You can use variables `icicle-list-use-nth-parts' and
`icicle-list-nth-parts-join-string' to control the minibuffer behavior
of multi-completions.  See the Icicles documentation for more
information.

PREDICATE limits completion to a subset of COLLECTION.

See `try-completion' and `all-completions' for more details on
completion, COLLECTION, and PREDICATE.

REQUIRE-MATCH can take any of these values:
* nil means the user can exit using any input.
* t means the user can exit only if the input is (or completes to) an
  element of COLLECTION or is null.
* In Emacs 23 or later:
  - `confirm' means the user can exit with any input, but if the input
    is not an element of COLLECTION then confirmation is needed.
  - `confirm-after-completion' is similar, except that with
    non-matching input exit is allowed only just after completing.
* Anything else behaves like t, except that hitting `\\[exit-minibuffer]' does not
  exit if it performs non-null completion.

Regardless of the value of REQUIRE-MATCH, if the user input is empty
then the function returns a string that is based on the value of DEF:

* DEF, if DEF is a string
* the first element of DEF, if DEF is a non-empty list
* the empty string, if DEF is nil

If option `icicle-require-match-flag' is non-nil, it overrides the
value of REQUIRE-MATCH.

If INITIAL-INPUT is non-nil, insert it in the minibuffer initially,
with point positioned at the end.  If it is (STRING . POSITION), the
initial input is STRING, but point is placed at zero-indexed position
POSITION in STRING.  (This is different from `read-from-minibuffer'
and related functions, which use one-indexing for POSITION.)

INITIAL-INPUT is considered deprecated by vanilla Emacs, but not by
Icicles.  If INITIAL-INPUT is nil and DEF is non-nil, the user can use
`next-history-element' to yank DEF into the minibuffer.

HIST, if non-nil, specifies a history list and optionally the initial
position in the list.  It can be a symbol, which is the history list
variable to use, or it can be a cons cell (HISTVAR . HISTPOS).  If a
cons cell, HISTVAR is the history list variable to use, and HISTPOS is
the initial position (the position in the list used by the minibuffer
history commands).  For consistency, you should also specify that
element of the history as the value of INITIAL-INPUT.  Positions are
counted starting from 1 at the beginning of the list.  The variable
`history-length' controls the maximum length of a history list.

DEF, if non-nil, is the default value or (Emacs 23+ only) the list of
default values.  Option `icicle-default-value' controls the treatment
of the default value (or the first default value, if DEF is a list):
whether it is shown in the prompt, substituted for an empty
INITIAL-INPUT, and so on.  If `icicle-default-value' is t then option
`icicle-default-in-prompt-format-function' is used to format DEF for
its addition to PROMPT.

If INHERIT-INPUT-METHOD is non-nil, the minibuffer inherits the
current input method and the setting of `enable-multibyte-characters'.

Optional arg KEYMAP, if non-nil, should be a completion keymap to use.

Both completion candidates and DEF are filtered using these Icicles
variables:
  `icicle-must-match-regexp'
  `icicle-must-not-match-regexp'
  `icicle-must-pass-predicate'

Completion ignores case when `completion-ignore-case' is non-nil."
  (unless (stringp icicle-initial-value) (setq icicle-initial-value  "")) ; Convert nil to "".
  (unless initial-input (setq initial-input  icicle-initial-value))
  (if (consp initial-input)
      (setq icicle-initial-value  (car initial-input))
    (setq initial-input         (format "%s" initial-input) ; Convert symbol to string
          icicle-initial-value  initial-input))
  (setq icicle-nb-of-other-cycle-candidates  0)

  ;; Use DEF for INITIAL-INPUT also, if `icicle-default-value' says so.
  (when (and def  icicle-default-value  (not (eq icicle-default-value t))
             (stringp initial-input)  (string= "" initial-input))
    ;; Filter DEF using `icicle-filter-wo-input'.  Done in `read-from-minibuffer' anyway, but we
    ;; must also do it here, to reuse the correct default value for the init value.
    (if (atom def)
        (setq initial-input  (or (icicle-filter-wo-input def)  "")) ; Ensure that it is non-nil.
      (let ((found  nil)
            (def1   def))
        (while (and (not found)  def1)
          (setq found  (icicle-filter-wo-input (car def1))
                def1   (cdr def1)))
        (setq initial-input  (or found  ""))))
    (when (memq icicle-default-value '(insert-start preselect-start))
      (setq initial-input  (cons initial-input 0))))

  ;; Override REQUIRE-MATCH as needed.
  (setq require-match           (case icicle-require-match-flag
                                  ((nil)               require-match)
                                  (no-match-required   nil)
                                  (partial-match-ok    t)
                                  (full-match-required 'full-match-required))
        icicle-require-match-p  require-match)
  (icicle-highlight-lighter)
  (let* ((minibuffer-history-variable       minibuffer-history-variable)
         ;; $$$$$$$$$$ `minibuffer-completion-table' binding needed?  `setq' in `*-lisp-vanilla-*'.
         (minibuffer-allow-text-properties  t) ; This is nil for completion in vanilla Emacs.
         (minibuffer-completion-table       collection)
         (icicle-fancy-cands-internal-p     (or icicle-whole-candidate-as-text-prop-p
                                                icicle-fancy-candidates-p
                                                (get-text-property 0 'icicle-fancy-candidates prompt)))
         result)
    ;; Transform a cons collection to what is expected for `minibuffer-completion-table'.
    (when icicle-fancy-cands-internal-p
      (let ((c+p  (icicle-mctize-all collection predicate)))
        (setq collection  (car c+p)     ; After banalizing for vanilla Emacs.
              predicate   (cadr c+p))))
    ;; $$$$$$ (setq minibuffer-completion-table  collection)

    (when def
      (let ((def1  (if (listp def) (car def) def))) ; Use only the first default (for `file-relative-name').
        (setq prompt  (icicle-handle-default-for-prompt
                       prompt
                       ;; If `insert-default-directory' then make DEF in prompt relative to `default-directory'.
                       (if (and def1  (eq icicle-default-value t)  insert-default-directory)
                           (file-relative-name def1)
                         def1)
                       (and (eq icicle-default-value t)
                            ;; Include in prompt only if `insert-default-directory' does not insert it as input.
                            (or (not insert-default-directory)
                                (not (icicle-file-name-input-p))
                                (not (equal def1 default-directory))))))))
    (cond ((not icicle-mode)
           (setq result  (icicle-completing-read-default prompt collection predicate require-match initial-input
                                                         hist-m@%=!$+&^*z def inherit-input-method keymap)))
          (t
           (let* ((minibuffer-prompt-properties             (and (boundp 'minibuffer-prompt-properties)
                                                                 (icicle-remove-property ; Emacs 21+ only
                                                                  'face minibuffer-prompt-properties)))
                  ;; Can't be file-name completion unless it's a function.
                  (minibuffer-completing-file-name          (and (functionp collection)
                                                                 minibuffer-completing-file-name))
                  ;; If not a recursive minibuffer, save original domain-defining variables,
                  ;; so user can restore them using `icicle-recomplete-from-original-domain'.
                  (top-level-p                              (< (minibuffer-depth) 1))
                  (icicle-orig-minibuffer-completion-table  (if top-level-p
                                                                minibuffer-completion-table
                                                              icicle-orig-minibuffer-completion-table))
                  (icicle-orig-minibuffer-completion-pred   (if top-level-p
                                                                predicate
                                                              icicle-orig-minibuffer-completion-pred))
                  (icicle-orig-must-pass-after-match-pred   (if top-level-p
                                                                icicle-must-pass-after-match-predicate
                                                              icicle-orig-must-pass-after-match-pred))
                  (icicle-orig-must-match-regexp            (if top-level-p
                                                                icicle-must-match-regexp
                                                              icicle-orig-must-match-regexp))
                  (icicle-orig-must-not-match-regexp        (if top-level-p
                                                                icicle-must-not-match-regexp
                                                              icicle-orig-must-not-match-regexp))
                  (icicle-orig-must-pass-predicate          (if top-level-p
                                                                icicle-must-pass-predicate
                                                              icicle-orig-must-pass-predicate)))
             (when (< emacs-major-version 21)
               (setq prompt  (concat (and icicle-candidate-action-fn  "+ ") prompt)))
             (setq result  (catch 'icicle-read-top
                             (icicle-completing-read-default prompt collection predicate require-match
                                                             initial-input hist-m@%=!$+&^*z def
                                                             inherit-input-method keymap)))
             (icicle-unpropertize-completion result))))
    ;; HACK.  Without this, when REQUIRE-MATCH is non-nil, `*Completions*' window
    ;; does not disappear.
    (when require-match (icicle-remove-Completions-window))
    result))

(defun icicle-handle-default-for-prompt (prompt default include)
  "Return PROMPT, possibly changed to format or remove the DEFAULT value.
Argument INCLUDE:
 * nil means do not include DEFAULT in prompt.  Remove it if there.
 * non-nil means include DEFAULT, formatted according to
   `icicle-default-in-prompt-format-function'.

In the existing PROMPT before modification, recognizes inclusion of
a default value according to these possible patterns:

 `minibuffer-default-in-prompt-regexps'
 \"(default ___):\"
 \"(default is ___):\"
 \" [___] \""
  (when (consp default) (setq default  (car default)))
  ;; Remove the default, if already there.
  (dolist (rgx  (if (boundp 'minibuffer-default-in-prompt-regexps) ; In `minibuf-eldef.el'.
                    minibuffer-default-in-prompt-regexps
                  '(("\\( (default\\(?: is\\)? \\(.*\\))\\):? \\'"  1)
                    ("\\( \\[.*\\]\\):? *\\'"                       1))))
    (setq prompt  (replace-regexp-in-string  (car rgx) "" prompt nil nil (cadr rgx))))
  ;; $$$$$$$$$ (when (icicle-file-name-input-p) (setq default  (file-name-nondirectory default)))
  ;; Add non-nil DEFAULT, if INCLUDE.
  (if (and default  include)
      (replace-regexp-in-string ".*\\(\\): *\\'"
                                (funcall icicle-default-in-prompt-format-function default)
                                prompt nil t 1)
    prompt))


(defun icicle-mctize-all (coll pred)
  "Transform collection COLL and predicate PRED for vanilla completion.
COLL is an Icicles collection argument acceptable to
  `icicle-completing-read' but not necessarily to vanilla
  `completing-read': COLL can contain multi-completions.
PRED is a predicate.

Returns a new two-element list of the new collection and predicate:

 * If COLL is not a list other than a lambda form then the new list
   returned is just (COLL PRED).

 * Otherwise, it is (MCT NEWPRED), where MCT is COLL transformed and
   NEWPRED is PRED transformed.  MCT is a collection suitable for
   vanilla `completing-read'.

COLL is transformed to MCT by applying `icicle-mctized-full-candidate'
to each of its elements.

If PRED is non-nil, then NEWPRED is a predicate that applies PRED to
the cdr of an MCT entry.  If PRED is nil, so is NEWPRED."
  (when (and (consp coll)  (not (functionp coll))) ; Exclude lambda form.
    ;; Copy alist collection COLL, so we don't change the original alist in any way.
    ;; Change each entry in COLL using `icicle-mctized-full-candidate'.
    (setq coll  (mapcar #'icicle-mctized-full-candidate coll))
    ;; Convert non-nil PRED so that, for a cons entry with a string car, PRED uses the cdr
    ;; (which is the original entry) instead.
    (and pred  (lexical-let ((new-pred  pred))
                 (setq pred  (lambda (x)
                               (funcall new-pred (if (and (consp x)  (stringp (car x))) (cdr x) x)))))))
  (list coll pred))

(defun icicle-mctized-full-candidate (cand)
  "Return MCT candidate that corresponds to full candidate CAND.
See the source code for details."
  ;; If neither `icicle-fancy-cands-internal-p' nor `icicle-whole-candidate-as-text-prop-p' is
  ;;   non-nil, then just return CAND.
  ;; Otherwise:
  ;;   If CAND is a string A, we change it to (A) and then treat that (as follows).
  ;;   If CAND is (A . B), where A is a string, then we change it to (S A . B), where S is a copy
  ;;     of A.  This way, the cdr of each MCT candidate is the original alist candidate, (A . B).
  ;;   If CAND is (M . B), where M is a multi-completion (X Y Z...), then we change it to
  ;;     (M' A . B), where M' is the display string for the multi-completion M.
  ;;   Otherwise, we make no change to CAND.
  ;;   If `icicle-whole-candidate-as-text-prop-p' is non-nil and the MCT candidate is a cons (X A . B)
  ;;     with a string car X, then we put the cdr, (A . B), as a text property on the car X, so
  ;;     we can get back the original (A . B) from the car.
  (if (not (or icicle-fancy-cands-internal-p  icicle-whole-candidate-as-text-prop-p))
      cand
    (let ((new-cand
           (cond ((and (consp cand)     ; Multi-completion: (("aa" "bb") . cc) ->
                       (consp (car cand)) ; ("aa^G\nbb\n\n" ("aa" "bb") . cc)
                       (stringp (caar cand)))
                  ;; $$$$$$ (cons (mapconcat #'identity (car cand) icicle-list-join-string)
                  (cons (mapconcat #'identity (car cand) icicle-list-join-string) cand))
                 ((and (consp cand)  (stringp (car cand))) ; ("aa" . cc) -> ("aa" "aa" . cc)
                  (cons (copy-sequence (car cand)) cand))
                 ((stringp cand)        ; "aa" -> ("aa" "aa")
                  (list (copy-sequence cand) cand))
                 (t                     ; Anything else: (aa), aa -> no change
                  cand))))
      ;; Put original alist candidates on display candidates (strings), as a text property.
      (when (and icicle-whole-candidate-as-text-prop-p  (consp new-cand)  (stringp (car new-cand)))
        (icicle-put-whole-cand-prop new-cand))
      new-cand)))

(defun icicle-put-whole-cand-prop (cand)
  "Put cdr of CAND on its car, as text property `icicle-whole-candidate'.
This has no side effects.
Returns a new propertized string corresponding to (car CAND)."
  (let ((text-cand  (copy-sequence (car cand))))
    (put-text-property 0 (length text-cand) 'icicle-whole-candidate (cdr cand) text-cand)
    (setcar cand text-cand)
    text-cand))

(defun icicle-mctized-display-candidate (cand)
  "Return MCT candidate that corresponds to display candidate CAND."
  (let ((full-cand  (or (funcall icicle-get-alist-candidate-function cand)  (list cand))))
    (cons cand full-cand)))

(defun icicle-replace-mct-cand-in-mct (old new)
  "Replace OLD candidate with NEW in `minibuffer-completion-table'.
Both OLD and NEW have been mctized.  That is, they are ready for
`minibuffer-completion-table'."
  (let ((newlist  minibuffer-completion-table))
    (catch 'icicle-replace-cand-in-mct
      (while newlist
        (when (equal (car newlist) old)
          (setcar newlist new)
          (throw 'icicle-replace-cand-in-mct nil))
        (setq newlist  (cdr newlist))))
    minibuffer-completion-table))

(defun icicle-read-file-name (prompt &optional dir default-filename
                              require-match initial-input predicate history)
  "Read file name, prompting with PROMPT and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself.
DIR should be an absolute directory name.  It defaults to the value of
 `default-directory'.
Default the name to DEFAULT-FILENAME if user exits the minibuffer with
the same non-empty string that was inserted by this function.
 (If DEFAULT-FILENAME is omitted, the visited file name is used,
  but if INITIAL-INPUT is specified, that combined with DIR is used.)
If the user exits with an empty minibuffer, this function returns
an empty string.  (This can only happen if the user erased the
pre-inserted contents or if `insert-default-directory' is nil.)
Fourth arg REQUIRE-MATCH non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL-INPUT specifies text to start with.
If optional sixth arg PREDICATE is non-nil, possible completions and
 the resulting file name must satisfy `(funcall predicate NAME)'.
 This argument is only available starting with Emacs 22.
Sixth arg HISTORY is an alternative minibuffer to use, instead of
 `file-name-history', which is used by default.  (HISTORY is not
 available for vanilla `read-file-name'.)

Both completion candidates and DEFAULT-FILENAME are filtered using
these Icicles variables:
  `icicle-must-match-regexp'
  `icicle-must-not-match-regexp'
  `icicle-must-pass-predicate'

Directory names are highlighted in `*Completions*' using face
`icicle-special-candidate'.

If option `icicle-require-match-flag' is non-nil, it overrides the
value of REQUIRE-MATCH.

If option `icicle-add-proxy-candidates-flag' is non-nil, then the
following proxy file-name candidates are included.  (This inclusion
can be toggled at any time from the minibuffer, using `C-M-_'.)

* `*mouse-2 file name*' - Click `mouse-2' on a file name to choose it.
* `*point file name*'   - Use the file name at point (cursor).
* Single-quoted file-name variables - Use the variable's value.

Candidates `*mouse-2 file name*' and `*point file name*' are available
only if library `ffap.el' can be loaded.  A file-name variable has
custom type `file' or (file :must-match t).

If this command was invoked with the mouse, use a file dialog box if
`use-dialog-box' is non-nil, and the window system or X toolkit in use
provides a file dialog box.

See also `read-file-name-completion-ignore-case' (Emacs version > 21)
and `read-file-name-function'."
  (unwind-protect
       (let* ((mouse-file                       "*mouse-2 file name*")
              (icicle-special-candidate-regexp  (or icicle-special-candidate-regexp  ".+/$"))
              (minibuffer-completing-file-name  t)
              (read-file-name-predicate         (and (boundp 'read-file-name-predicate)
                                                     read-file-name-predicate))
              (ffap-available-p                 (or (require 'ffap- nil t)  (require 'ffap nil t)))

              ;; These four `ffap-*' bindings would speed up `ffap-guesser' - see `ffap.el' about them.
              ;; Do not bind them for Emacs 23+, however, so users can get multiple default values for `M-n'.
              (emacs-23+                        (>= emacs-major-version 23))
              (ffap-alist                       (and emacs-23+  ffap-alist))
              (ffap-url-regexp                  (and emacs-23+  ffap-url-regexp))
              (ffap-shell-prompt-regexp         (and emacs-23+  ffap-shell-prompt-regexp))
              (ffap-machine-p-known             (if emacs-23+ ffap-machine-p-known 'accept))

              (fap
               (if (and (eq major-mode 'dired-mode)  (fboundp 'dired-get-file-for-visit))
                   (condition-case nil (abbreviate-file-name (dired-get-file-for-visit)) (error nil))
                 (and ffap-available-p  (icicle-ffap-guesser))))
              (icicle-proxy-candidates
               (append
                (and icicle-add-proxy-candidates-flag  (not icicle-exclude-default-proxies)
                     (append (and fap  (list "*point file name*"))
                             (and ffap-available-p  (list mouse-file))
                             (let ((ipc  ()))
                               (mapatoms
                                (lambda (cand)
                                  (when (and (user-variable-p cand)
                                             (condition-case nil
                                                 (icicle-var-is-of-type-p cand '(file (file :must-match t)))
                                               (error nil)))
                                    (push (concat "'" (symbol-name cand) "'") ipc))))
                               ipc)))
                icicle-proxy-candidates))
              result)

         ;;  ;; $$$$$$ Does Emacs 23+ need explicit directory? If so, add these three lines
         ;;  (unless dir (setq dir  default-directory))
         ;;  (unless (file-name-absolute-p dir) (setq dir  (expand-file-name dir)))
         ;;  (setq dir  (abbreviate-file-name dir)) ; Use `~' for home directory.

         (setq result  (icicle-read-file-name-1 prompt dir default-filename
                                                require-match initial-input predicate history))
         (when ffap-available-p
           (cond ((icicle-string-match-p "*point file name\\*$" result)
                  (setq result  fap))
                 ((icicle-string-match-p "*mouse-2 file name\\*$" result)
                  (setq result  (progn (let ((e  (read-event "Click `mouse-2' on file name")))
                                         (read-event) ; Get rid of mouse up event.
                                         (save-excursion
                                           (mouse-set-point e)
                                           (if (and (eq major-mode 'dired-mode)
                                                    (fboundp 'dired-get-file-for-visit)) ; In `dired+.el'.
                                               (condition-case nil ; E.g. error: not on file line (ignore)
                                                   (abbreviate-file-name (dired-get-file-for-visit))
                                                 (error "No such file"))
                                             (or (icicle-ffap-guesser)  (error "No such file"))))))))))
         (icicle-unpropertize-completion result)
         (let* ((temp  (member (file-name-nondirectory result) icicle-proxy-candidates))
                (symb  (and temp  (intern (substring (car temp) 1 (1- (length (car temp))))))))
           (when (and symb  (boundp symb)) (setq result  (symbol-value symb))))
         result)
    ;; Because we do this here, if a command that uses `icicle-read-file-name' needs the proxies
    ;; afterward then it needs to save a copy of them.
    (setq icicle-proxy-candidates  ())))

(defun icicle-read-file-name-1 (prompt &optional dir default-filename
                                require-match initial-input predicate history)
  "Helper function for `icicle-read-file-name'."
  (setq icicle-nb-of-other-cycle-candidates  0
        icicle-initial-value                 (or initial-input  (if (stringp icicle-initial-value)
                                                                    icicle-initial-value
                                                                  "")))
  (icicle-fix-default-directory)        ; Make sure there are no backslashes in it.
  (unless (string= "" icicle-initial-value) (setq initial-input  icicle-initial-value))

  ;; Use DEFAULT-FILENAME for INITIAL-INPUT also, if `icicle-default-value' says so.
  ;; But if so, remove the directory part first.
  ;; Note that if DEFAULT-FILENAME is null, then we let INITIAL-INPUT remain null too.
  (when (and default-filename  icicle-default-value  (not (eq icicle-default-value t))
             ;; We don't use the same test as for `completing-read':
             ;; (stringp initial-input) (string= "" initial-input))
             (string= "" icicle-initial-value))
    ;; Filter DEFAULT-FILENAME using `icicle-filter-wo-input'.  Done in `read-from-minibuffer'
    ;; anyway, but we must also do it here, to reuse the correct default value for the init value.
    (if (atom default-filename)
        (setq initial-input  (icicle-filter-wo-input (file-name-nondirectory default-filename)))
      (let ((found  nil)
            (def1   default-filename))
        (while (and (not found)  def1)
          (setq found  (icicle-filter-wo-input (file-name-nondirectory (car def1)))
                def1   (cdr def1)))
        (setq initial-input  (or found  "")))))

  ;; Override REQUIRE-MATCH as needed.
  (setq require-match           (case icicle-require-match-flag
                                  ((nil) require-match)
                                  (no-match-required nil)
                                  (partial-match-ok t)
                                  (full-match-required 'full-match-required))
        icicle-require-match-p  require-match)
  (icicle-highlight-lighter)
  (let ((read-file-name-function      nil)
        (minibuffer-history-variable  (or history  minibuffer-history-variable))
        result)
    (let ((minibuffer-prompt-properties
           (and (boundp 'minibuffer-prompt-properties) ; Emacs 21+ only
                (icicle-remove-property 'face minibuffer-prompt-properties))))
      (when (< emacs-major-version 21)
        (setq prompt  (concat (and icicle-candidate-action-fn  "+ ") prompt)))
      (if (and history  (eq icicle-orig-read-file-name-fn 'read-file-name-default))
          ;; Use `icicle-read-file-name-default', which accepts a HISTORY arg.
          (setq result  (catch 'icicle-read-top
                          (funcall #'icicle-read-file-name-default
                                   prompt dir default-filename require-match initial-input predicate
                                   (or history  'file-name-history))))
        (condition-case nil             ; If Emacs 22+, use PREDICATE arg.
            (setq result  (catch 'icicle-read-top
                            (funcall (or icicle-orig-read-file-name-fn  'read-file-name)
                                     prompt dir default-filename require-match initial-input predicate)))
          (wrong-number-of-arguments
           (setq result  (catch 'icicle-read-top ; Try with neither (Emacs 20-21).
                           (funcall (or icicle-orig-read-file-name-fn  'read-file-name) prompt dir
                                    default-filename require-match initial-input)))))))
    ;; HACK.  Without this, when REQUIRE-MATCH is non-nil, `*Completions*' window
    ;; does not disappear.
    (when require-match (icicle-remove-Completions-window))
    result))

(defun icicle-read-file-name-default (prompt &optional dir default-filename mustmatch initial predicate history)
  "Same as vanilla `read-file-name-default', except accepts HISTORY too."
  (setq history  (or history 'file-name-history))
  (unless dir (setq dir  default-directory))
  (unless (file-name-absolute-p dir) (setq dir  (expand-file-name dir)))
  (unless default-filename
    (setq default-filename  (if initial (expand-file-name initial dir) buffer-file-name)))
  (setq dir  (abbreviate-file-name dir)) ; If DIR starts with user's homedir, change that to ~.
  (when default-filename                ; Likewise for DEFAULT-FILENAME.
    (setq default-filename  (if (consp default-filename)
                                (mapcar 'abbreviate-file-name default-filename)
                              (abbreviate-file-name default-filename))))
  (let ((insdef  (cond ((and insert-default-directory  (stringp dir))
                        (if initial
                            (cons (minibuffer--double-dollars (concat dir initial))
                                  (length (minibuffer--double-dollars dir)))
                          (minibuffer--double-dollars dir)))
                       (initial (cons (minibuffer--double-dollars initial) 0)))))
    (let ((completion-ignore-case           read-file-name-completion-ignore-case)
          (minibuffer-completing-file-name  t)
          (pred                             (or predicate  'file-exists-p))
          (add-to-history                   nil))
      (let* ((val                 (if (or (not (next-read-file-uses-dialog-p))
                                          (file-remote-p dir)) ; File dialogs can't handle remote files (Bug#99).
                                      ;; We used to pass DIR to `read-file-name-internal' by abusing arg
                                      ;; PREDICATE.  It is better to just use `default-directory', but to avoid
                                      ;; changing `default-directory' in the current buffer, we do not
                                      ;; `let'-bind it.
                                      (let ((dir  (file-name-as-directory (expand-file-name dir))))
                                        (minibuffer-with-setup-hook
                                         (lambda ()
                                           (setq default-directory  dir)
                                           ;; When first default in `minibuffer-default' duplicates initial input
                                           ;; INSDEF, reset `minibuffer-default' to nil.
                                           (when (equal (or (car-safe insdef)  insdef)
                                                        (or (car-safe minibuffer-default)  minibuffer-default))
                                             (setq minibuffer-default  (cdr-safe minibuffer-default)))
                                           ;; Upon first `M-n' request, fill `minibuffer-default' with a list of
                                           ;; defaults relevant for file-name reading.
                                           (set (make-local-variable 'minibuffer-default-add-function)
                                                (lambda ()
                                                  (with-current-buffer (window-buffer
                                                                        (minibuffer-selected-window))
                                                    (read-file-name--defaults dir initial))))
                                           (set-syntax-table minibuffer-local-filename-syntax))
                                         (completing-read prompt 'read-file-name-internal pred mustmatch insdef
                                                          history default-filename)))
                                    ;; If DEFAULT-FILENAME not supplied and DIR contains a file name, split it.
                                    (let ((file               (file-name-nondirectory dir))
                                          ;; When using a dialog, revert to nil and non-nil interpretation of
                                          ;; MUSTMATCH.  Confirmation options need to be interpreted as nil,
                                          ;; otherwise it is impossible to create new files using dialogs with
                                          ;; the default settings.
                                          (dialog-mustm  (not (memq mustmatch
                                                                    '(nil confirm confirm-after-completion)))))
                                      (when (and (not default-filename)  (not (zerop (length file))))
                                        (setq default-filename  file
                                              dir               (file-name-directory dir)))
                                      (when default-filename
                                        (setq default-filename  (expand-file-name (if (consp default-filename)
                                                                                      (car default-filename)
                                                                                    default-filename)
                                                                                  dir)))
                                      (setq add-to-history  t)
                                      (x-file-dialog prompt dir default-filename dialog-mustm
                                                     (eq predicate 'file-directory-p)))))
             (replace-in-history  (eq (car-safe (symbol-value history)) val)))
        (setq history  (symbol-value history))
        ;; If `completing-read' returned the inserted default string itself (rather than a new string with
        ;; the same contents), it has to mean that the user typed RET with the minibuffer empty.
        ;; In that case, we really want to return "" so that commands such as `set-visited-file-name' can
        ;; distinguish.
        (when (consp default-filename) (setq default-filename  (car default-filename)))
        (when (eq val default-filename)
          ;; In this case, `completing-read' has not added an element to the history.  Maybe we should.
          (unless replace-in-history (setq add-to-history  t))
          (setq val  ""))
        (unless val (error "No file name specified"))
        (when (and default-filename  (string-equal val (if (consp insdef) (car insdef) insdef)))
          (setq val  default-filename))
        (setq val  (substitute-in-file-name val))
        (if replace-in-history
            ;; Replace what `Fcompleting_read' added to the history with what we will actually return.
            ;; As an exception, if that's the same as the second item in HISTORY, it's really a repeat
            ;; (Bug#4657).
            (let ((val1  (minibuffer--double-dollars val)))
              (when history-delete-duplicates
                (setcdr history (delete val1 (cdr history))))
              (if (string= val1 (cadr history))
                  (pop history)
                (setcar history val1)))
          (when add-to-history
            ;; Add the value to HISTORY, unless it matches the last value already there.
            (let ((val1  (minibuffer--double-dollars val)))
              (unless (and (consp history)  (equal (car history) val1))
                (setq history  (cons val1 (if history-delete-duplicates (delete val1 history) history)))))))
	val))))

(defun icicle-fix-default-directory ()
  "Convert backslashes in `default-directory' to slashes."
  ;; This is a hack.  If you do `C-x 4 f' from a standalone minibuffer
  ;; frame, `default-directory' on MS Windows has this form:
  ;; `C:\some-dir/'.  There is a backslash character in the string.  This
  ;; is not a problem for standard Emacs, but it is a problem for Icicles,
  ;; because we interpret backslashes using regexp syntax - they are not
  ;; file separators for Icicles.  So, we call `substitute-in-file-name' to
  ;; change all backslashes in `default-directory' to slashes.  This
  ;; shouldn't hurt, because `default-directory' is an absolute directory
  ;; name - it doesn't contain environment variables.  For example, we
  ;; convert `C:\some-dir/' to `c:/some-directory/'."
  (setq default-directory  (icicle-abbreviate-or-expand-file-name (substitute-in-file-name
                                                                   default-directory))))

(defun icicle-remove-property (prop plist)
  "Remove property PROP from property-list PLIST, non-destructively.
Returns the modified copy of PLIST."
  (let ((cpy     plist)
        (result  ()))
    (while cpy
      (unless (eq prop (car cpy)) (setq result  `(,(cadr cpy) ,(car cpy) ,@result)))
      (setq cpy  (cddr cpy)))
    (nreverse result)))


;; REPLACE ORIGINAL `read-from-minibuffer' (built-in function),
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Respect `icicle-default-value'.
;;
;; We use HIST-m@%=!$+&^*z instead of HIST, to avoid name capture by `minibuffer-history-variable's
;; value.  If we didn't need to be Emacs 20-compatible, then we could employ
;; `#1=#:hist'...`#1#'...`#1' read syntax to use an uninterned symbol.
;;
(unless (fboundp 'icicle-ORIG-read-from-minibuffer)
  (defalias 'icicle-ORIG-read-from-minibuffer (symbol-function 'read-from-minibuffer)))

(defun icicle-read-from-minibuffer (prompt &optional initial-contents keymap read
                                    hist-m@%=!$+&^*z default-value inherit-input-method)
  "Read a string from the minibuffer, prompting with string PROMPT.
The optional second arg INITIAL-CONTENTS is an alternative to
  DEFAULT-VALUE.  Vanilla Emacs considers it to be obsolete, but
  Icicles does not.  It is discussed in more detail below.

Third arg KEYMAP is a keymap to use while reading;
  if omitted or nil, the default is `minibuffer-local-map'.

If fourth arg READ is non-nil, then interpret the result as a Lisp
  object and return that object.  In other words, return this:

   (car (read-from-string INPUT-STRING))

Fifth arg HIST, if non-nil, specifies a history list and optionally
  the initial position in the list.  It can be a symbol, which is the
  history list variable to use, or it can be a cons cell
  (HISTVAR . HISTPOS).  If a cons cell, HISTVAR is the history list
  variable to use and HISTPOS is the initial position for use by the
  minibuffer history commands.  For consistency, you should also
  specify that element of the history as the value of
  INITIAL-CONTENTS.  Positions are counted starting from 1 at the
  beginning of the list.

Sixth arg DEFAULT-VALUE is a string, nil, or (for Emacs 23 and later)
  a non-empty list of strings.  The strings are available to the user
  as input via `\\<minibuffer-local-map>\\[next-history-element]'.

  NOTE: Unlike a default-value parameter for some other functions such
  as `completing-read', if the user hits `RET' with empty input then
  DEFAULT-VALUE is NOT returned.  In that case, if READ is nil then
  the empty string, \"\", is returned.  If READ is non-nil then the
  DEFAULT-VALUE string (or the first string in DEFAULT-VALUE if
  DEFAULT-VALUE is a list) is read.

Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer
  inherits the current input method and the setting of
  `enable-multibyte-characters'.

If variable `minibuffer-allow-text-properties' is non-nil then the
  string returned includes whatever text properties were present in
  the minibuffer.  Otherwise the return value has no text properties.

Option `icicle-default-value' controls how the default value,
DEFAULT-VALUE, is treated.

The remainder of this documentation string describes parameter
INITIAL-CONTENTS in more detail.

If non-nil, INITIAL-CONTENTS is a string to be inserted into the
minibuffer before reading input.  Normally, point is put at the end of
that string.  However, if INITIAL-CONTENTS is (STRING . POSITION), the
initial input is STRING and point is placed at one-indexed position
POSITION in the minibuffer.  Any integer value less than or equal to
one puts point at the beginning of the string.  Note that this
behavior differs from the way such arguments are used in
`completing-read' and some other functions, which use zero-indexing
for POSITION."
  (unless initial-contents (setq initial-contents  ""))

  ;; Filter DEFAULT-VALUE using `icicle-filter-wo-input'.
  (when default-value
    (setq default-value
          (if (atom default-value)
              (icicle-filter-wo-input default-value)
            (delq nil (mapcar #'icicle-filter-wo-input default-value)))))
  (when (and (< emacs-major-version 23)  (consp default-value)) ; Emacs <23 does not accept a list.
    (setq default-value  (car default-value)))
  ;; Save new default value for caller (e.g. `icicle-completing-read-default'.
  (setq icicle-filtered-default-value  default-value)

  ;; If a list of strings, use the first one for prompt etc.
  (let ((def-value  (icicle-unlist default-value)))
    ;; Maybe use DEFAULT-VALUE for INITIAL-CONTENTS also.
    (when (and icicle-default-value  (not (eq icicle-default-value t))
               def-value             (equal "" initial-contents))
      (setq initial-contents  (if (integerp def-value) ; Character
                                  (string def-value)
                                def-value)))
;;; $$$$$$    (when (and def-value  (eq icicle-default-value t)) ; Add DEFAULT-VALUE to PROMPT.
;;;       (when (icicle-file-name-input-p) (setq def-value  (file-name-nondirectory def-value)))
;;;       (setq prompt  (if (string-match "\\(.*\\)\\(: *\\)$" prompt)
;;;                         (concat (substring prompt (match-beginning 1) (match-end 1)) " (" def-value
;;;                                 ")" (substring prompt (match-beginning 2) (match-end 2)))
;;;                       (concat prompt def-value))))
    )
  (icicle-ORIG-read-from-minibuffer
   prompt initial-contents keymap read hist-m@%=!$+&^*z default-value inherit-input-method))


;; REPLACE ORIGINAL `minibuffer-default-add-completions' defined in `simple.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Respect Icicles global filters, so you don't see, as defaults, candidates that were filtered out.
;;
(when (fboundp 'minibuffer-default-add-completions) ; Emacs 23+.
  (unless (fboundp 'icicle-ORIG-minibuffer-default-add-completions)
    (defalias 'icicle-ORIG-minibuffer-default-add-completions
        (symbol-function 'minibuffer-default-add-completions)))

  ;; Use this as `minibuffer-default-add-function'.
  (defun icicle-minibuffer-default-add-completions ()
    "Like `icicle-ORIG-minibuffer-default-add-completions', but respect global filters."
    (let ((def  minibuffer-default)
          (all  (icicle-all-completions "" minibuffer-completion-table
                                        minibuffer-completion-predicate 'HIDE-SPACES)))
      (setq all  (icicle-remove-if-not (lambda (cand)
                                         (let ((case-fold-search  completion-ignore-case))
                                           (icicle-filter-wo-input cand)))
                                       all))
      (if (listp def) (append def all) (cons def (delete def all))))))


;; REPLACE ORIGINAL `read-buffer' (built-in).
;;
;; 1. Interactively, uses `another-buffer' or `other-buffer' if no default.
;; 2. Emacs 23+ compatible: handles `read-buffer-function'
;;    and `read-buffer-completion-ignore-case'.
;; 3. Respects `icicle-buffer-ignore-space-prefix-flag'.
;;
(unless (fboundp 'icicle-ORIG-read-buffer)
  (defalias 'icicle-ORIG-read-buffer (symbol-function 'read-buffer)))

(defun icicle-read-buffer (prompt &optional default require-match)
  "Read the name of a buffer and return it as a string.
Prompt with first arg, PROMPT (a string).

If user input is empty (just `RET') then return the default value,
which is:
 - optional second arg DEFAULT, if non-nil
 - `another-buffer' or `other-buffer', otherwise.

If `another-buffer' is undefined, then use `other-buffer'.

Starting with Emacs 23, DEFAULT can be a list of names (strings), in
which case the first name in the list is returned on empty input.

Non-nil REQUIRE-MATCH means to allow only names of existing buffers.
It is the same as for `completing-read'.

Case sensitivity is determined by
`read-buffer-completion-ignore-case', if defined, or
`completion-ignore-case' otherwise.

This binds variable `icicle-buffer-name-input-p' to non-nil."
  (let ((icicle-buffer-name-input-p  t))
    (if (and (boundp 'read-buffer-function)  read-buffer-function)
        (funcall read-buffer-function prompt default require-match)
      (when (interactive-p)
        (setq default  (or default  (if (fboundp 'another-buffer) ; In `misc-fns.el'.
                                        (another-buffer nil t)
                                      (other-buffer (current-buffer))))))
      (when (bufferp default) (setq default  (buffer-name default))) ; Need a string as default.
      (let ((completion-ignore-case  (if (boundp 'read-buffer-completion-ignore-case)
                                         read-buffer-completion-ignore-case
                                       completion-ignore-case)))
        (completing-read prompt
                         (cond ((and (eq icicle-buffer-complete-fn 'internal-complete-buffer)
                                     icicle-buffer-ignore-space-prefix-flag)
                                'internal-complete-buffer) ; Emacs 22+
                               (icicle-buffer-complete-fn)
                               (t
                                (mapcar (lambda (buf) (and (buffer-live-p buf)  (list (buffer-name buf))))
                                        (buffer-list))))
                         nil require-match nil 'buffer-name-history default nil)))))


;; REPLACE ORIGINAL `read-number' defined in `subr.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; 1. Let user enter a numeric variable name, for its value.  Allow completion.
;; 2. Allow for error reading input.
;; 3. Call `ding' if not a number, and don't redisplay for `sit-for'.
;;
(when (fboundp 'read-number)            ; Emacs 22+
  (unless (fboundp 'icicle-ORIG-read-number)
    (defalias 'icicle-ORIG-read-number (symbol-function 'read-number)))

  (defun icicle-read-number (prompt &optional default)
    "Read a number in the minibuffer, prompting with PROMPT (a string).
DEFAULT is returned if the user hits `RET' without typing anything.

If option `icicle-add-proxy-candidates-flag' is non-nil, the user can
also enter the name of a numeric variable - its value is returned.
Completion is available for this.  A numeric variable is a variable
whose value or whose custom type is compatible with type `integer',
`number', or `float'."
    (unwind-protect
         (let ((num  nil)
               (icicle-proxy-candidates
                (append
                 (and icicle-add-proxy-candidates-flag  (not icicle-exclude-default-proxies)
                      (let ((ipc  ()))
                        (mapatoms
                         (lambda (cand)
                           (when (and (user-variable-p cand)
                                      (condition-case nil
                                          (icicle-var-is-of-type-p cand (if (>= emacs-major-version 22)
                                                                            '(number integer float)
                                                                          '(number integer)))
                                        (error nil)))
                             (push (symbol-name cand) ipc))))
                        ipc))
                 icicle-proxy-candidates))

               ;; Emacs 23 allows DEFAULT to be a list of strings - use the first one for prompt etc.
               (default1  (if (atom default) default (setq default  (delq nil default))  (car default))))
           (when default
             (save-match-data
               (setq prompt  (if (string-match "\\(\\):[ \t]*\\'" prompt)
                                 (replace-match (format " (default %s)" default1) t t prompt 1)
                               (replace-regexp-in-string
                                "[ \t]*\\'" (format " (default %s) " default1) prompt t t)))))
           (when icicle-proxy-candidates (put-text-property 0 1 'icicle-fancy-candidates t prompt))
           (while (progn
                    (let ((str  (completing-read prompt nil nil nil nil nil
                                                 (if (consp default)
                                                     (mapcar #'number-to-string default)
                                                   (and default1  (number-to-string default1)))))
                          temp)
                      (setq num  (cond ((zerop (length str)) default1)
                                       ((setq temp  (member str icicle-proxy-candidates))
                                        (symbol-value (intern (car temp))))
                                       ((stringp str) (condition-case nil (read str) (error nil))))))
                    (unless (numberp num)
                      (icicle-ding) (message "Not a number.  Try again.") (sit-for 0.5 nil t)
                      t)))
           num)
      ;; Because we do this here, if a command that uses `icicle-read-number' needs the proxies
      ;; afterward then it needs to save a copy of them.
      (setq icicle-proxy-candidates  ()))))

;; Can't replace standard `read-char-exclusive' with this, because, starting with Emacs 22, it has
;; an optional SECONDS arg that cannot be simulated using `completing-read'.
(defun icicle-read-char-exclusive (prompt &optional inherit-input-method)
  "Read a character in the minibuffer, prompting with PROMPT (a string).
It is returned as a number.
Optional arg INHERIT-INPUT-METHOD is as for `completing-read'.

If option `icicle-add-proxy-candidates-flag' is non-nil, the user can
also enter the name of a character variable - its value is returned.
Completion is available for this.  A character variable is a variable
whose value is compatible with type `character'."
  (unwind-protect
       (let* ((char  nil)
              (icicle-proxy-candidates
               (append (and icicle-add-proxy-candidates-flag  (not icicle-exclude-default-proxies)
                            (let ((ipc  ()))
                              (mapatoms (lambda (cand)
                                          (when (and (user-variable-p cand)
                                                     (condition-case nil
                                                         (icicle-var-is-of-type-p cand '(character))
                                                       (error nil)))
                                            (push (symbol-name cand) ipc))))
                              ipc))
                       icicle-proxy-candidates))
              str temp)
         (when icicle-proxy-candidates (put-text-property 0 1 'icicle-fancy-candidates t prompt))
         (setq str   (completing-read prompt nil nil nil nil nil nil inherit-input-method)
               char  (cond ((zerop (length str)) (error "No character read"))
                           ((setq temp  (member str icicle-proxy-candidates))
                            (symbol-value (intern (car temp))))
                           ((stringp str) (condition-case nil
                                              (progn (when (> (length str) 1)
                                                       (message "First char is used: `%c'"
                                                                (elt str 0)) (sit-for 2))
                                                     (elt str 0))
                                            (error nil)))))
         char)
    ;; Because we do this here, if a command that uses `icicle-read-char-exclusive' needs the proxies
    ;; afterward then it needs to save a copy of them.
    (setq icicle-proxy-candidates  ())))

;; Not used in Icicles code, but used by other libraries.
(defun icicle-read-string-completing (prompt &optional default pred hist)
  "Read a string in the minibuffer, prompting with PROMPT (a string).
If the user hits `RET' without typing anything, return DEFAULT, or \"\"
  if DEFAULT is nil.
PRED is a predicate that filters the variables available for completion.
HIST is the history list to use, as for `completing-read'.

If option `icicle-add-proxy-candidates-flag' is non-nil, the user can
also enter the name of a string variable - its value is returned.
Completion is available for this.  A string variable is a variable
whose value or whose custom type is compatible with type `string'."
  (unwind-protect
       (let ((strg      nil)
             (default1  (icicle-unlist default)) ; Emacs 23+ lets DEFAULT be a list of strings - use the first.
             (icicle-proxy-candidates
              (append
               (and icicle-add-proxy-candidates-flag  (not icicle-exclude-default-proxies)
                    (let ((ipc  ()))
                      (mapatoms (lambda (cand)
                                  (when (and (user-variable-p cand)
                                             (condition-case nil
                                                 (icicle-var-is-of-type-p cand '(string color regexp))
                                               (error nil)))
                                    (push (symbol-name cand) ipc))))
                      ipc))
               icicle-proxy-candidates)))
         (when default
           (save-match-data
             (setq prompt  (if (string-match "\\(\\):[ \t]*\\'" prompt)
                               (replace-match (format " (default %s)" default1) t t prompt 1)
                             (replace-regexp-in-string
                              "[ \t]*\\'" (format " (default %s) " default1) prompt t t)))))
         (when icicle-proxy-candidates (put-text-property 0 1 'icicle-fancy-candidates t prompt))
         (let ((strg-read  (completing-read prompt nil pred nil
                                            (and (consp hist)  (nth (cdr hist) (symbol-value (car hist))))
                                            hist default))
               temp)
           (setq strg  (cond ((zerop (length strg-read)) (or default1  ""))
                             ((setq temp  (member strg-read icicle-proxy-candidates))
                              (setq temp  (symbol-value (intern (car temp))))
                              (cond ((and (symbolp hist)  (consp (symbol-value hist)))
                                     (setcar (symbol-value hist) temp))
                                    ((and (consp hist)  (symbolp (car hist))
                                          (consp (symbol-value (car hist))))
                                     (setcar (symbol-value (car hist)) temp)))
                              temp)
                             (t strg-read))))
         strg)
    ;; Because we do this here, if a command that uses `icicle-read-string-completing' needs the proxies
    ;; afterward then it needs to save a copy of them.
    (setq icicle-proxy-candidates  ())))

;; Same as `help-var-is-of-type-p'.
(defun icicle-var-is-of-type-p (variable types &optional mode)
  "Return non-nil if VARIABLE satisfies one of the custom types in TYPES.
TYPES is a list of `defcustom' type sexps or a list of regexp strings.
TYPES are matched, in order, against VARIABLE's type definition or
VARIABLE's current value, until one is satisfied or all are tried.

If TYPES is a list of regexps, then each is regexp-matched against
VARIABLE's custom type.

Otherwise, TYPES is a list of type sexps, each of which is a
definition acceptable for `defcustom' :type or the first symbol of
such a definition (e.g. `choice').  In this case, two kinds of type
comparison are possible:

1. VARIABLE's custom type, or its first symbol, is matched using
  `equal' against each type in TYPES.

2. VARIABLE's current value is checked against each type in TYPES to
   see if it satisfies one of them.  In this case, VARIABLE's own type
   is not used; VARIABLE might not even be typed - it could be a
   variable not defined using `defcustom'.

For any of the comparisons against VARIABLE's type, either that type
can be checked directly or its supertypes (inherited types) can also
be checked.

These different type-checking possibilities depend on the value of
argument MODE, as follows, and they determine the meaning of the
returned value:

`direct':   VARIABLE's type matches a member of list TYPES
`inherit':  VARIABLE's type matches or is a subtype of a TYPES member
`value':    VARIABLE is bound, and its value satisfies a type in TYPES
`inherit-or-value': `inherit' or `value', tested in that order
`direct-or-value':  `direct' or `value', tested in that order
anything else (default): `inherit'

VARIABLE's current value cannot satisfy a regexp type: it is
impossible to know which concrete types a value must match."
  (case mode
    ((nil inherit)     (icicle-var-inherits-type-p variable types))
    (inherit-or-value  (or (icicle-var-inherits-type-p variable types)
                           (icicle-var-val-satisfies-type-p variable types)))
    (value             (icicle-var-val-satisfies-type-p variable types))
    (direct            (icicle-var-matches-type-p variable types))
    (direct-or-value   (or (member (icicle-get-safe variable 'custom-type) types)
                           (icicle-var-val-satisfies-type-p variable types)))
    (otherwise         (icicle-var-inherits-type-p variable types))))

(defun icicle-var-matches-type-p (variable types)
  "VARIABLE's type matches a member of TYPES."
  (catch 'icicle-type-matches
    (let ((var-type  (icicle-get-safe variable 'custom-type)))
      (dolist (type types)
        (when (if (stringp type)
                  (icicle-string-match-p type (format "%s" (format "%S" var-type)))
                (equal var-type type))
          (throw 'icicle-type-matches t))))
    nil))

(defun icicle-var-inherits-type-p (variable types)
  "VARIABLE's type matches or is a subtype of a member of list TYPES."
  (catch 'icicle-type-inherits
    (let ((var-type  (icicle-get-safe variable 'custom-type)))
      (dolist (type types)
        (while var-type
          (when (or (and (stringp type)  (icicle-string-match-p type (format "%s" (format "%S" var-type))))
                    (equal type var-type))
            (throw 'icicle-type-inherits t))
          (when (consp var-type) (setq var-type  (car var-type)))
          (when (or (and (stringp type)  (icicle-string-match-p type (format "%s" (format "%S" var-type))))
                    (equal type var-type))
            (throw 'icicle-type-inherits t))
          (setq var-type  (car (icicle-get-safe var-type 'widget-type))))
        (setq var-type  (icicle-get-safe variable 'custom-type))))
    nil))

(defun icicle-var-val-satisfies-type-p (variable types)
  "VARIABLE is bound, and its value satisfies a type in the list TYPES."
  (and (boundp variable)
       (let ((val  (symbol-value variable)))
         (and (widget-convert (icicle-get-safe variable 'custom-type))
              (icicle-value-satisfies-type-p val types)))))

(defun icicle-value-satisfies-type-p (value types)
  "Return non-nil if VALUE satisfies a type in the list TYPES."
  (catch 'icicle-type-value-satisfies
    (dolist (type types)
      (unless (stringp type)            ; Skip, for regexp type.
        (setq type  (widget-convert type))
        ;; Satisfies if either :match or :validate.
        (when (condition-case nil
                  (progn (when (and (widget-get type :match)  (widget-apply type :match value))
                           (throw 'icicle-type-value-satisfies t))
                         (when (and (widget-get type :validate)
                                    (progn (widget-put type :value value)
                                           (not (widget-apply type :validate))))
                           (throw 'icicle-type-value-satisfies t)))
                (error nil))
          (throw 'icicle-type-value-satisfies t))))
    nil))

(defun icicle-custom-type (variable)
  "Returns the `defcustom' type of VARIABLE.
Returns nil if VARIABLE is not a user option.

Note: If the library that defines VARIABLE has not yet been loaded,
then `icicle-custom-type' loads it.  Be sure you want to do that
before you call this function."
  (and (custom-variable-p variable)
       (or (get variable 'custom-type)
           (progn (custom-load-symbol variable) (get variable 'custom-type)))))

(when (fboundp 'read-char-by-name)      ; Emacs 23+
  (defun icicle-read-char-maybe-completing (&optional prompt names inherit-input-method seconds)
    "Read a char with PROMPT, possibly completing against NAMES.
If the character read is `C-q' then read another character.
Otherwise, if the character read is a completing key (e.g. `TAB'),
then complete.

Elements of alist NAMES have the form of `ucs-names' elements:
 (CHAR-NAME . CHAR-CODE)
NAMES defaults to the subset of `ucs-names' that corresponds to the
 characters that have been read previously.
The other arguments are as in `read-char-by-name'."
    (unless names (setq names  (or (icicle-char-cands-from-charlist)  (icicle-ucs-names))))
    (let ((chr  (read-char prompt inherit-input-method seconds)))
      (if (eq chr ?\C-q)
          (setq chr  (read-char prompt inherit-input-method seconds)) ; ^Q - read next
        (when (member (vector chr) (append icicle-prefix-complete-keys icicle-apropos-complete-keys))
          (add-to-list 'unread-command-events chr)
          (setq chr  (icicle-read-char-by-name prompt names))))
      chr))

  (defun icicle-char-cands-from-charlist (&optional chars)
    "Characters in list CHARS that are listed in `icicle-ucs-names'.
CHARS defaults to the value of `icicle-read-char-history'."
    (unless chars (setq chars  icicle-read-char-history))
    (let ((cands  ())
          name.char)
      (dolist (char  chars)
        (when (setq name.char  (rassq char (icicle-ucs-names)))
          (push name.char cands)))
      cands)))


;; REPLACE ORIGINAL `read-char-by-name' in `mule-cmds.el' (Emacs 23+).
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; 1. Use `icicle-ucs-names', not `ucs-names'.
;; 2. Exclude character names "" and "VARIATION SELECTOR*".
;; 3. If `icicle-read-char-by-name-multi-completion-flag' is non-nil, show the character itself, after its name,
;;    in `*Completions*'.
;; 4. Added optional arg NAMES.
;; 5. Add char read to `icicle-read-char-history'.
;; 6. See doc string for the rest.
;;
(when (fboundp 'read-char-by-name)      ; Emacs 23+

  (defun icicle-make-char-candidate (name.char)
    "Return multi-completion candidate for NAME.CHAR.
NAME.CHAR has the form of an element of `ucs-names':
* The car is the character name.
* The cdr is the character itself.

The multi-completion candidate is a cons whose cdr is still the
character, but whose car is a list (NAME CODE SCHAR), where:
* CODE is a string representation of the Unicode code point of CHAR,
  as a hexidecimal numeral
* SCHAR is a string representation of CHAR

Properties `help-echo' and `icicle-mode-line-help' are put on NAME,
showing both NAME and the code point (in hex, octal, and decimal)."
    (and (not (string= "" (car name.char)))
         (if icicle-read-char-by-name-multi-completion-flag
             ;; $$$$$$ Maybe make this optional?
             ;; (not (string-match "\\`VARIATION SELECTOR" (car name.char))))
             (let* ((name  (copy-sequence (car name.char)))
                    (char  (cdr name.char)))
               (icicle-candidate-short-help
                (format "Char: %-10cCode Point: x%X, o%o, %d" char char char char) name)
               (cons (list name (format "%X" char) (format "%c" char)) char))
           name.char)))

  (unless (fboundp 'icicle-ORIG-read-char-by-name)
    (defalias 'icicle-ORIG-read-char-by-name (symbol-function 'read-char-by-name)))

  (defun icicle-read-char-by-name (prompt &optional names)
    "Read a character by its Unicode name or hex number string.
Display PROMPT and read a string that represents a character by its
Unicode property `name' or `old-name'.  Return the char as a number.

You can use completion against the Unicode name of the character.

In Icicle mode, if `icicle-read-char-by-name-multi-completion-flag' is
non-nil:

* The Unicode code point of the char and the char itself appear next
  to the char name in `*Completions*' - WYSIWYG.

* The completion candidate is a multi-completion.  Its first part is
  the char name.  Its second part is the code point, as a hexadecimal
  numeral.  Its third part is the character.  This means that you can
  alternatively type the code point or the character to see what the
  name is.  You can complete the name or the code point, or both.

* When you cycle among candidates, regardless of whether buffer
  `*Completions*' is shown, the current character and its code point
  are shown in the mode line (provided user option
  `icicle-help-in-mode-line-delay' is greater than zero).  The code
  point is shown in hexadecimal, octal, and decimal notation.

If you use a dedicated `*Completions*' frame, then the font used in
`*Completions*' is the same as the frame from which you invoked
completion.

If you use library `doremi-frm.el' then you can increase the font size
for `*Completions*' dynamically using `C-x -'.

As an alternative to completing the Unicode name or code point, you
can just input the code point as a hexidecimal numeral or a number in
hash notation: #o21430 for octal, #x2318 for hex, or #10r8984 for
decimal.

Non-nil optional arg NAMES is an alist of names to use in place of the
value returned by `icicle-ucs-names'.  It must have the same form as
such a return value: (CHAR-NAME . CHAR-CODE)."
    (unless names  (setq names  (icicle-ucs-names)))
    (let* ((cands                                  (delq nil (mapcar #'icicle-make-char-candidate names)))
           (new-prompt                             (copy-sequence prompt))
           (enable-recursive-minibuffers           t)
           (completion-ignore-case                 t)
           (icicle-multi-completing-p              (and icicle-read-char-by-name-multi-completion-flag
                                                        icicle-show-multi-completion-flag))
           (icicle-list-use-nth-parts              '(1))
           (icicle-transform-before-sort-p         t)
           (icicle-list-join-string                "\t")
           (icicle-candidate-properties-alist      '((3 (face icicle-candidate-part))))
           (icicle-whole-candidate-as-text-prop-p  icicle-multi-completing-p)
           (mctized-cands                          (car (icicle-mctize-all cands nil)))
           (collection-fn                          `(lambda (string pred action)
                                                     (if (eq action 'metadata)
                                                         '(metadata (category . unicode-name))
                                                       (complete-with-action
                                                        action ',mctized-cands string pred))))
           (input                                  (completing-read new-prompt collection-fn))
           chr)
      (setq chr  (cond ((string-match-p "\\`[0-9a-fA-F]+\\'" input)  (string-to-number input 16))
                       ((string-match-p "^#" input)                  (read input))
                       ((if icicle-multi-completing-p
                            ;; Either user completed and INPUT is a multi-completion or user did not complete
                            ;; and INPUT is a character name.
                            (or (cddr (assoc-string input mctized-cands t))
                                (cdr (assoc-string input names t)))
                          (cdr (assoc-string input mctized-cands t)))) ; INPUT is a character name.
                       (icicle-multi-completing-p
                        (let ((completion  (try-completion input collection-fn)))
                          (and (stringp completion)
                               ;; INPUT is not a multi-completion, but it may match a single multi-completion.
                               ;; In particular, it might match just the NAME or CODE part of it.
                               (let* ((name                       (icicle-transform-multi-completion
                                                                   completion))
                                      (icicle-list-use-nth-parts  '(2))
                                      (code                       (icicle-transform-multi-completion
                                                                   completion))
                                      ;; To have property `icicle-whole-candidate', COMPLETION must be complete.
                                      (char                       (cdr
                                                                   (get-text-property
                                                                    0 'icicle-whole-candidate completion)))
                                      (case-fold-search           t))
                                 (and (or (and name  (string-match-p input name))
                                          (and code  (string-match-p input code)))
                                      char)))))))
      (unless (characterp chr) (error "Invalid character: `%s'" input))
      (add-to-list 'icicle-read-char-history chr)
      chr))

  ;; This would not be needed if there were not STILL Emacs bug #9653.
  (defun icicle-ucs-names ()
    "Same as `ucs-names', except remove entries with an empty name: \"\"."
    (setq ucs-names  (assq-delete-all "" (ucs-names))))) ; Free var here: `ucs-names'.


;; REPLACE ORIGINAL `read-string' (built-in function),
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Respect `icicle-default-value' (via use of `read-from-minibuffer').
;;
;; We use HIST-m@%=!$+&^*z instead of HISTORY, to avoid name capture by `minibuffer-history-variable's
;; value.  If we didn't need to be Emacs 20-compatible, then we could employ
;; `#1=#:hist'...`#1#'...`#1' read syntax to use an uninterned symbol.
;;
(unless (fboundp 'icicle-ORIG-read-string)
  (defalias 'icicle-ORIG-read-string (symbol-function 'read-string)))

(defun icicle-read-string (prompt &optional initial-input hist-m@%=!$+&^*z default-value inherit-input-method)
  "Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
  Vanilla Emacs considers it to be obsolete, but Icicles does not.  It
  behaves like argument INITIAL-CONTENTS in `read-from-minibuffer'.
  See the documentation string of `read-from-minibuffer' for details.
The third arg HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
  See `read-from-minibuffer' for details of HISTORY argument.
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
 for history commands, and as the value to return if the user enters
 the empty string.
Fifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of enable-multibyte-characters."
  (when default-value
    (setq prompt  (icicle-handle-default-for-prompt prompt default-value (eq icicle-default-value t))))
  (let ((value  (read-from-minibuffer prompt initial-input nil nil hist-m@%=!$+&^*z
                                      default-value inherit-input-method)))
    (when (and default-value  (equal value ""))
      (setq value  (icicle-unlist default-value)))
    value))


;; REPLACE ORIGINAL `read-face-name' in `faces.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Show face names in `*Completions*' with the faces they name.
;;
(unless (fboundp 'icicle-ORIG-read-face-name)
  (defalias 'icicle-ORIG-read-face-name (symbol-function 'read-face-name)))

(cond ((< emacs-major-version 21)
       (defun icicle-read-face-name (prompt) ; Emacs 20
         "Read a face name with completion and return its face symbol.
PROMPT is the prompt.

If option `icicle-add-proxy-candidates-flag' is non-nil, then you can
also enter the name of a face-name variable - its value is returned.
A face-name variable is a variable with custom-type `face'.

If library `eyedropper.el' is used, then you can also choose proxy
candidate `*point face name*' to use the face at point."
         (require 'eyedropper nil t)
         (let ((icicle-multi-completing-p          t)
               (icicle-list-nth-parts-join-string  ": ")
               (icicle-list-join-string            ": ")
               (icicle-list-use-nth-parts          '(1))
               (icicle-proxy-candidates
                (append
                 (and icicle-add-proxy-candidates-flag  (not icicle-exclude-default-proxies)
                      (append (and (fboundp 'eyedrop-face-at-point)  (list "*point face name*"))
                              (let ((ipc  ()))
                                (mapatoms
                                 (lambda (cand)
                                   (when (and (user-variable-p cand)  (eq (get cand 'custom-type) 'face))
                                     (push `,(concat "'" (symbol-name cand) "'") ipc))))
                                ipc)))
                 icicle-proxy-candidates))
               face)
           (setq prompt  (copy-sequence prompt)) ; So we can modify it by adding property.
           (put-text-property 0 1 'icicle-fancy-candidates t prompt)
           (while (= (length face) 0)
             (setq face  (icicle-transform-multi-completion
                          (completing-read prompt (mapcar #'icicle-make-face-candidate (face-list))
                                           nil (not (stringp icicle-WYSIWYG-Completions-flag)) nil
                                           (if (boundp 'face-name-history)
                                               'face-name-history
                                             'icicle-face-name-history)))))
           (let ((proxy  (car (member face icicle-proxy-candidates))))
             (cond ((icicle-string-match-p "*point face name\\*$" face) (eyedrop-face-at-point))
                   (proxy (symbol-value (intern (substring proxy 1 (1- (length proxy))))))
                   (t (intern face)))))))

      ((= emacs-major-version 21)       ; Emacs 21
       (defun icicle-read-face-name (prompt)
         "Read a face name with completion and return its face symbol.
PROMPT is the prompt.

If option `icicle-add-proxy-candidates-flag' is non-nil, then you can
also enter the name of a face-name variable - its value is returned.
A face-name variable is a variable with custom-type `face'.

If library `eyedropper.el' is used, then you can also choose proxy
candidate `*point face name*' to use the face at point."
         (require 'eyedropper nil t)
         (let ((icicle-multi-completing-p          t)
               (icicle-list-nth-parts-join-string  ": ")
               (icicle-list-join-string            ": ")
               (icicle-list-use-nth-parts          '(1))
               (icicle-proxy-candidates
                (append
                 (and icicle-add-proxy-candidates-flag  (not icicle-exclude-default-proxies)
                      (append (and (fboundp 'eyedrop-face-at-point)  (list "*point face name*"))
                              (let ((ipc ()))
                                (mapatoms
                                 (lambda (cand)
                                   (when (and (user-variable-p cand)  (eq (get cand 'custom-type) 'face))
                                     (push `,(concat "'" (symbol-name cand) "'") ipc))))
                                ipc)))
                 icicle-proxy-candidates))
               (face-list  (face-list))
               (def        (icicle-thing-at-point 'symbol))
               face)
           (cond ((assoc def face-list) (setq prompt  (concat prompt " (default " def "): ")))
                 (t (setq def     nil
                          prompt  (concat prompt ": "))))
           (setq prompt  (copy-sequence prompt)) ; So we can modify it by adding property.
           (put-text-property 0 1 'icicle-fancy-candidates t prompt)
           (while (equal "" (setq face  (icicle-transform-multi-completion
                                         (completing-read
                                          prompt (mapcar #'icicle-make-face-candidate face-list) nil
                                          (not (stringp icicle-WYSIWYG-Completions-flag)) nil
                                          (if (boundp 'face-name-history)
                                              'face-name-history
                                            'icicle-face-name-history)
                                          def)))))
           (let ((proxy  (car (member face icicle-proxy-candidates))))
             (cond ((icicle-string-match-p "*point face name\\*$" face) (eyedrop-face-at-point))
                   (proxy (symbol-value (intern (substring proxy 1 (1- (length proxy))))))
                   (t (intern face)))))))

      ((< emacs-major-version 24)       ; Emacs 22-23
       (defun icicle-read-face-name (prompt &optional string-describing-default multiple)
         "Read a face name with completion and return its face symbol
By default, use the face(s) on the character after point.  If that
character has the property `read-face-name', that overrides the `face'
property.

PROMPT should be a string that describes what the caller will do with the face;
  it should not end in a space.
STRING-DESCRIBING-DEFAULT should describe what default the caller will use if
  the user just types RET; you can omit it.
If MULTIPLE is non-nil, return a list of faces (possibly only one).
Otherwise, return a single face.

If option `icicle-add-proxy-candidates-flag' is non-nil, then you can
also enter the name of a face-name variable - its value is returned.
A face-name variable is a variable with custom-type `face'.

If library `palette.el' or `eyedropper.el' is used, then you can also
choose proxy candidate `*point face name*' to use the face at point."
         (or (require 'palette nil t)  (require 'eyedropper nil t))
         (let ((faceprop       (or (get-char-property (point) 'read-face-name)
                                   (get-char-property (point) 'face)))
               (aliasfaces     ())
               (nonaliasfaces  ())
               (icicle-proxy-candidates
                (append (and icicle-add-proxy-candidates-flag  (not icicle-exclude-default-proxies)
                             (let ((ipc  ()))
                               (mapatoms
                                (lambda (cand)
                                  (when (and (user-variable-p cand)  (eq (get cand 'custom-type) 'face))
                                    (push `,(concat "'" (symbol-name cand) "'") ipc))))
                               ipc))
                        icicle-proxy-candidates))
               faces)
           (save-match-data             ; Undo Emacs 22+ brain-dead treatment of PROMPT arg.
             (when (string-match "\\(:\\s *$\\|:?\\s +$\\)" prompt)
               (setq prompt  (substring prompt 0 (- (length (match-string 0 prompt)))))))
           ;; Try to get a face name from the buffer.
           (when (memq (intern-soft (icicle-thing-at-point 'symbol)) (face-list))
             (setq faces  (list (intern-soft (icicle-thing-at-point 'symbol)))))
           ;; Add the named faces that the `face' property uses.
           (if (and (consp faceprop)
                    ;; Don't treat an attribute spec as a list of faces.
                    (not (keywordp (car faceprop)))
                    (not (memq (car faceprop) '(foreground-color background-color))))
               (dolist (f faceprop) (when (symbolp f) (push f faces)))
             (when (and faceprop  (symbolp faceprop)) (push faceprop faces)))
           (delete-dups faces)
           (cond (multiple
                  ;; We leave this branch as it is.  Icicles does nothing special with
                  ;; `completing-read-multiple'.
                  (require 'crm)
                  (mapatoms (lambda (symb) (when (custom-facep symb) ; Build up the completion tables.
                                             (if (get symb 'face-alias)
                                                 (push (symbol-name symb) aliasfaces)
                                               (push (symbol-name symb) nonaliasfaces)))))
                  (let* ((input   (completing-read-multiple ; Read the input.
                                   (if (or faces  string-describing-default)
                                       (format "%s (default %s): "
                                               prompt (if faces
                                                          (mapconcat 'symbol-name faces ",")
                                                        string-describing-default))
                                     (format "%s: " prompt))
                                   ;; This lambda expression is the expansion of Emacs 22 macro
                                   ;; (complete-in-turn nonaliasfaces aliasfaces).  We expand it so
                                   ;; this can be compiled also in Emacs < 22 to work for Emacs 22.
                                   (lambda (string predicate mode)
                                     (cond ((eq mode t)
                                            (or (all-completions string nonaliasfaces predicate)
                                                (all-completions string aliasfaces predicate)))
                                           ((eq mode nil)
                                            (or (try-completion string nonaliasfaces predicate)
                                                (try-completion string aliasfaces predicate)))
                                           (t
                                            (or (test-completion string nonaliasfaces predicate)
                                                (test-completion string aliasfaces predicate)))))
                                   nil t nil (if (boundp 'face-name-history)
                                                 'face-name-history
                                               'icicle-face-name-history)
                                   (and faces  (mapconcat 'symbol-name faces ","))))
                         (output  (cond ((or (equal input "")  (equal input '(""))) ; Canonicalize.
                                         faces)
                                        ((stringp input)
                                         (mapcar 'intern (split-string input ", *" t)))
                                        ((listp input)
                                         (mapcar 'intern input))
                                        (input))))
                    output))            ; Return the list of faces
                 (t
                  (when (consp faces) (setq faces  (list (car faces))))
                  (let ((icicle-multi-completing-p          t)
                        (icicle-list-nth-parts-join-string  ": ")
                        (icicle-list-join-string            ": ")
                        (icicle-list-use-nth-parts          '(1))
                        (face-list                          (face-list))
                        (def                                (if faces
                                                                (mapconcat 'symbol-name faces ",")
                                                              string-describing-default))
                        face)
                    (setq prompt  (copy-sequence prompt)) ; So we can modify it by adding property.
                    (put-text-property 0 1 'icicle-fancy-candidates t prompt)
                    (while (equal "" (setq face  (icicle-transform-multi-completion
                                                  (completing-read
                                                   (if def
                                                       (format "%s (default %s): " prompt def)
                                                     (format "%s: " prompt))
                                                   (mapcar #'icicle-make-face-candidate face-list)
                                                   nil (not (stringp icicle-WYSIWYG-Completions-flag))
                                                   nil (if (boundp 'face-name-history)
                                                           'face-name-history
                                                         'icicle-face-name-history)
                                                   def)))))
                    (let ((proxy  (car (member face icicle-proxy-candidates))))
                      (if proxy
                          (symbol-value (intern (substring proxy 1 (1- (length proxy)))))
                        (intern face)))))))))
      (t
       (defun icicle-read-face-name (prompt &optional default multiple)
         "Read a face name with completion and return its face symbol.
PROMPT should not end in a space or a colon.

If non-nil, DEFAULT should be a face (a symbol), a face name (a
string) or a list of faces (symbols).

DEFAULT determines what is returned if the user just hits `RET' (empty
input), as follows:

 If DEFAULT is nil then return nil.
 If DEFAULT is a single face, then return its name.
 If DEFAULT is a list of faces, then:

   If MULTIPLE is nil, return the name of the first face in the list.
   If MULTIPLE is non-nil, return DEFAULT.

If MULTIPLE is non-nil, read multiple face names and return them as a
list.  If MULTIPLE is nil, read and return a single face name.

If option `icicle-add-proxy-candidates-flag' is non-nil, then you can
also enter the name of a face-name variable - its value is returned.
A face-name variable is a variable with custom-type `face'.

If library `palette.el' or `eyedropper.el' is used, then you can also
choose proxy candidate `*point face name*' to use the face at point."
         (or (require 'palette nil t)  (require 'eyedropper nil t))
         (when (and default  (not (stringp default)))
           (setq default  (cond ((symbolp default) (symbol-name default))
                                (multiple (mapconcat (lambda (fc) (if (symbolp fc) (symbol-name fc) fc))
                                                     default ", "))
                                (t (symbol-name (car default))))))
         (when (and default  (not multiple))
           (require 'crm)
           ;; For compatibility with `completing-read-multiple' use `crm-separator' to define DEFAULT.
           (setq default  (car (split-string default crm-separator t))))
         (save-match-data               ; Undo Emacs 22+ brain-dead treatment of PROMPT arg.
           (when (string-match "\\(:\\s *$\\|:?\\s +$\\)" prompt)
             (setq prompt  (substring prompt 0 (- (length (match-string 0 prompt)))))))
         (let ((prompt         (if default
                                   (format "%s (default is %s): " prompt (if (equal default "all faces")
                                                                             "ALL faces"
                                                                           (format "`%s'" default)))
                                 (format "%s: " prompt)))
               (icicle-proxy-candidates
                (append (and icicle-add-proxy-candidates-flag  (not icicle-exclude-default-proxies)
                             (let ((ipc  ()))
                               (mapatoms
                                (lambda (cand)
                                  (when (and (user-variable-p cand)  (eq (get cand 'custom-type) 'face))
                                    (push `,(concat "'" (symbol-name cand) "'") ipc))))
                               ipc))
                        icicle-proxy-candidates)))
           (cond (multiple
                  ;; We leave this branch as it is.  Icicles does nothing special with
                  ;; `completing-read-multiple'.
                  (require 'crm)
                  (let ((faces  ())
                        (aliasfaces     ())
                        (nonaliasfaces  ()))
                    (mapatoms (lambda (s) (when (facep s) ; Build up the completion tables.
                                            (if (get s 'face-alias)
                                                (push (symbol-name s) aliasfaces)
                                              (push (symbol-name s) nonaliasfaces)))))
                    (dolist (face  (completing-read-multiple prompt (completion-table-in-turn nonaliasfaces
                                                                                              aliasfaces)
                                                             nil t nil 'face-name-history default))
                      ;; Ignore elements that are not faces (e.g., because DEFAULT was brain-dead "all faces").
                      (if (facep face) (push (intern face) faces)))
                    (nreverse faces)))  ; Return the list of faces
                 (t
                  (let ((icicle-multi-completing-p          t)
                        (icicle-list-nth-parts-join-string  ": ")
                        (icicle-list-join-string            ": ")
                        (icicle-list-use-nth-parts          '(1))
                        (face-list                          (face-list))
                        face)
                    (setq prompt  (copy-sequence prompt)) ; So we can modify it by adding property.
                    (put-text-property 0 1 'icicle-fancy-candidates t prompt)
                    (while (equal "" (setq face  (icicle-transform-multi-completion
                                                  (completing-read
                                                   prompt
                                                   (mapcar #'icicle-make-face-candidate face-list)
                                                   nil (not (stringp icicle-WYSIWYG-Completions-flag))
                                                   nil (if (boundp 'face-name-history)
                                                           'face-name-history
                                                         'icicle-face-name-history)
                                                   default)))))
                    (let ((proxy  (car (member face icicle-proxy-candidates))))
                      (if proxy
                          (symbol-value (intern (substring proxy 1 (1- (length proxy)))))
                        (intern face))))))
           ))
       ))

(defun icicle-make-face-candidate (face)
  "Return a completion candidate for FACE.
The value of option `icicle-WYSIWYG-Completions-flag' determines the
kind of candidate to use.
 If nil, then the face name is used (a string).

 If a string, then a multi-completion candidate is used, with the face
 name followed by a sample swatch using FACE on the string's text.

 If t, then the candidate is the face name itself, propertized with
FACE."
  (if (stringp icicle-WYSIWYG-Completions-flag)
      (let ((swatch  (copy-sequence icicle-WYSIWYG-Completions-flag)))
        (put-text-property 0 (length icicle-WYSIWYG-Completions-flag) 'face face swatch)
        (list (list (symbol-name face) swatch)))
    (let ((face-name  (copy-sequence (symbol-name face))))
      (when icicle-WYSIWYG-Completions-flag
        (put-text-property 0 (length face-name) 'face face face-name))
      (list face-name))))


;; REPLACE ORIGINAL `face-valid-attribute-values' in `faces.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Show color names in `*Completions*' with the (background) colors they name.
;; This is really so that commands such as `modify-face' take advantage of colored candidates.
;; We don't bother to try the same thing for Emacs 20, but the fix (directly to `modify-face') is
;; similar and trivial.
;;
(when (fboundp 'face-valid-attribute-values) ; Emacs 21+.
  (unless (fboundp 'icicle-ORIG-face-valid-attribute-values)
    (defalias 'icicle-ORIG-face-valid-attribute-values (symbol-function 'face-valid-attribute-values)))

  (if (fboundp 'window-system)          ; Emacs 23+
      ;; Emacs 23+ `font-family-list' is strings, not conses of strings like older `x-font-family-list'.
      (defun icicle-face-valid-attribute-values (attribute &optional frame)
        "Return valid values for face attribute ATTRIBUTE.
The optional argument FRAME is used to determine available fonts
and colors.  If it is nil or not specified, the selected frame is
used.  Value is an alist of (NAME . VALUE) if ATTRIBUTE expects a value
out of a set of discrete values.  Value is `integerp' if ATTRIBUTE expects
an integer value."
        (let ((valid
               (case attribute
                 (:family (if (window-system frame)
                              (mapcar (lambda (x) (cons x x)) ; Just strings, so don't take car.
                                      (font-family-list))
                            ;; Only one font on TTYs.
                            (list (cons "default" "default"))))
                 (:foundry
                  (list nil))
                 (:width
                  (mapcar (lambda (x) (cons (symbol-name (aref x 1)) (aref x 1)))  font-width-table))
                 (:weight
                  (mapcar (lambda (x) (cons (symbol-name (aref x 1)) (aref x 1)))  font-weight-table))
                 (:slant
                  (mapcar (lambda (x) (cons (symbol-name (aref x 1)) (aref x 1)))  font-slant-table))
                 (:inverse-video
                  (mapcar (lambda (x) (cons (symbol-name x) x))
                          (internal-lisp-face-attribute-values attribute)))
                 ((:underline :overline :strike-through :box)
                  (if (window-system frame)
                      (nconc (mapcar (lambda (x) (cons (symbol-name x) x))
                                     (internal-lisp-face-attribute-values attribute))
                             (mapcar (lambda (c) (cons c c))
                                     (mapcar #'icicle-color-name-w-bg (defined-colors frame))))
                    (mapcar (lambda (x) (cons (symbol-name x) x))
                            (internal-lisp-face-attribute-values attribute))))
                 ((:foreground :background)
                  (mapcar (lambda (c) (cons c c))
                          (mapcar #'icicle-color-name-w-bg (defined-colors frame))))
                 ((:height) 'integerp)
                 (:stipple (and (memq (window-system frame) '(x ns)) ; No stipple on w32
                                (mapcar #'list (apply #'nconc (mapcar (lambda (dir)
                                                                        (and (file-readable-p dir)
                                                                             (file-directory-p dir)
                                                                             (directory-files dir)))
                                                                      x-bitmap-file-path)))))
                 (:inherit (cons '("none" . nil)
                                 (mapcar (lambda (c) (cons (symbol-name c) c)) (face-list))))
                 (t
                  (error "`icicle-face-valid-attribute-values': YOU SHOULD NOT SEE THIS; \
Use `M-x icicle-send-bug-report'")))))
          (if (and (listp valid)  (not (memq attribute '(:inherit))))
              (nconc (list (cons "unspecified" 'unspecified)) valid)
            valid)))
    (defun icicle-face-valid-attribute-values (attribute &optional frame) ; Emacs 21-22.
      "Return valid values for face attribute ATTRIBUTE.
The optional argument FRAME is used to determine available fonts
and colors.  If it is nil or not specified, the selected frame is
used.  Value is an alist of (NAME . VALUE) if ATTRIBUTE expects a value
out of a set of discrete values.  Value is `integerp' if ATTRIBUTE expects
an integer value."
      (let ((valid
             (case attribute
               (:family (if window-system
                            (mapcar (lambda (x) (cons (car x) (car x)))
                                    (if (fboundp 'font-family-list) (font-family-list) (x-font-family-list)))
                          ;; Only one font on TTYs.
                          (list (cons "default" "default"))))
               ((:width :weight :slant :inverse-video)
                (mapcar (lambda (x) (cons (symbol-name x) x))
                        (internal-lisp-face-attribute-values attribute)))
               ((:underline :overline :strike-through :box)
                (if window-system
                    (nconc (mapcar (lambda (x) (cons (symbol-name x) x))
                                   (internal-lisp-face-attribute-values attribute))
                           (mapcar (lambda (c) (cons c c))
                                   (mapcar #'icicle-color-name-w-bg (x-defined-colors frame))))
                  (mapcar (lambda (x) (cons (symbol-name x) x))
                          (internal-lisp-face-attribute-values attribute))))
               ((:foreground :background)
                (mapcar (lambda (c) (cons c c))  (mapcar #'icicle-color-name-w-bg (x-defined-colors frame))))
               ((:height) 'integerp)
               (:stipple (and (memq window-system '(x w32 mac))
                              (mapcar #'list (apply #'nconc (mapcar (lambda (dir)
                                                                      (and (file-readable-p dir)
                                                                           (file-directory-p dir)
                                                                           (directory-files dir)))
                                                                    x-bitmap-file-path)))))
               (:inherit (cons '("none" . nil)
                               (mapcar (lambda (c) (cons (symbol-name c) c)) (face-list))))
               (t
                (error "`icicle-face-valid-attribute-values': YOU SHOULD NOT SEE THIS; \
Use `M-x icicle-send-bug-report'")))))
        (if (and (listp valid)  (not (memq attribute '(:inherit))))
            (nconc (list (cons "unspecified" 'unspecified)) valid)
          valid))))

  (defun icicle-color-name-w-bg (color-name)
    "Return copy of string COLOR-NAME with its background of that color.
If `hexrgb.el' is not loaded, then just return COLOR-NAME."
    (if (featurep 'hexrgb)
        (let ((propertized-name  (copy-sequence color-name)))
          (put-text-property 0 (length propertized-name)
                             'face (cons 'background-color (hexrgb-color-name-to-hex color-name))
                             propertized-name)
          propertized-name)
      color-name)))


;; REPLACE ORIGINAL `completing-read-multiple' stuff in `crm.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Essentially, we just inhibit Icicles features for Icicle mode.
;;
(eval-after-load "crm"
  '(progn
    (when (fboundp 'crm-init-keymaps) (crm-init-keymaps)) ; Emacs 22, but not 23.
    ;; Save vanilla CRM stuff as `icicle-ORIG-' stuff.
    (unless (fboundp 'icicle-ORIG-completing-read-multiple)
      (fset 'icicle-ORIG-completing-read-multiple (symbol-function 'completing-read-multiple)))
    (defvar icicle-ORIG-crm-local-completion-map crm-local-completion-map "Original CRM completion map.")
    (defvar icicle-ORIG-crm-local-must-match-map crm-local-must-match-map "Original CRM must-match map.")

    ;; Define CRM stuff to use in Icicle mode.  Basically, just inhibit Icicles features.
    (defun icicle-completing-read-multiple (prompt collection &optional predicate require-match
                                            initial-input hist def inherit-input-method)
      "Read multiple strings in the minibuffer, with completion.
Return the strings read, as a list.

By using this functionality, you can specify multiple strings at a
single prompt, optionally using completion.

Most Icicles completions features are available, but because `TAB'
here performs `crm' completion it does not also cycle among completion
candidates.  You can, as always, use `down' to do that.

You specify multiple strings by separating the strings with a
prespecified separator regexp (separator character, prior to Emacs
24.3).  For example, if the separator regexp is \",\" then you specify
the strings 'alice', 'bob', and 'eve' as 'alice,bob,eve'.

The separator regexp is the value of variable `crm-separator', whose
default value is the value of `crm-default-separator'.

Contiguous strings of non-separator-characters are referred to as
\"elements\".  In the above example, the elements are 'alice', 'bob',
and 'eve'.

Completion is available on a per-element basis.  For example, if your
input in the minibuffer is 'alice,bob,eve' and point is between the
'l' and the 'i', pressing `TAB' operates on element 'alice'.

See `completing-read' for details about the arguments."
      (let ((icicle-highlight-input-completion-failure  nil))
        (icicle-ORIG-completing-read-multiple prompt collection predicate require-match
                                      initial-input hist def inherit-input-method)))

    ;; Helper function - workaround because of a lack of multiple inheritance for keymaps.
    (defun icicle-define-crm-completion-map (map)
      "Make basic bindings for keymap MAP, a crm completion map."
      (set-keymap-parent map minibuffer-local-completion-map)
      (define-key map [remap minibuffer-complete] ; Emacs 22, 23.
        (if (fboundp 'crm-complete) #'crm-complete #'crm-minibuffer-complete))
      (when (fboundp 'crm-complete-word)
        (define-key map [remap minibuffer-complete-word] #'crm-complete-word))
      (when (and (boundp 'icicle-word-completion-keys)  (fboundp 'crm-complete-word))
        (dolist (key icicle-word-completion-keys) (define-key map key #'crm-complete-word)))
      (define-key map [remap minibuffer-completion-help] ; Emacs 22, 23.
        (if (fboundp 'crm-completion-help) #'crm-completion-help #'crm-minibuffer-completion-help))
      (define-key map "?" #'crm-completion-help) ; Put back `?' as help (self-insert for Icicles).
      (when (boundp 'icicle-prefix-complete-keys) ; Don't use Icicles completion.
        (dolist (key icicle-prefix-complete-keys)
          (define-key map key           ; Emacs 22, 23.
            (if (fboundp 'crm-complete) #'crm-complete #'crm-minibuffer-complete)))))

    (defvar icicle-crm-local-completion-map
      (let ((map  (make-sparse-keymap)))
        (icicle-define-crm-completion-map map)
        map)
      "Local keymap for minibuffer multiple input with completion.
Analog of `minibuffer-local-completion-map'.")

    (defvar icicle-crm-local-must-match-map
      (let ((map  (make-sparse-keymap)))
        (icicle-define-crm-completion-map map)
        (define-key map [remap minibuffer-complete-and-exit]
          (if (fboundp 'crm-complete-and-exit) #'crm-complete-and-exit #'crm-minibuffer-complete-and-exit))
        map)
      "Local keymap for minibuffer multiple input with exact match completion.
Analog of `minibuffer-local-must-match-map' for crm.")

    ;; Now, toggle Icicle mode, to take into account loading `crm.el' and redefining its stuff.
    (eval-after-load "icicles-mode" '(icicle-toggle-icicle-mode-twice))))


;; REPLACE ORIGINAL `read-shell-command' defined in `simple.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Use Icicles completion.
;;
(defun icicle-read-shell-command (prompt &optional initial-contents hist default-value
                                  inherit-input-method)
  "Read a shell command.
Use file-name completion, unless INITIAL-CONTENTS is non-nil.
For completion, pass args to `icicle-read-shell-command-completing'."
  (if initial-contents
      (if (fboundp 'icicle-ORIG-read-shell-command) ; Emacs < 23
          (icicle-ORIG-read-shell-command prompt initial-contents hist default-value inherit-input-method)
        (error "`icicle-read-shell-command': YOU SHOULD NOT SEE THIS; Use `M-x icicle-send-bug-report'"))
    (minibuffer-with-setup-hook
     (lambda ()
       (set (make-local-variable 'minibuffer-default-add-function)
            'minibuffer-default-add-shell-commands))
     (icicle-read-shell-command-completing prompt initial-contents (or hist  'shell-command-history)
                                           default-value inherit-input-method))))


;; REPLACE ORIGINAL `dired-smart-shell-command' defined in `dired-x.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Use Icicles completion.
;;
;; Not needed for Emacs 23+ - Icicles completion is automatic via `icicle-read-shell-command'.
;;
(unless (fboundp 'read-shell-command)
  ;; Emacs < 23 only
  (defun icicle-dired-smart-shell-command (command &optional output-buffer error-buffer)
    "Like `icicle-shell-command', but in the current Virtual Dired directory.
Uses Icicles completion - see `icicle-read-shell-command-completing'."
    (interactive
     (list (icicle-read-shell-command "Shell command: " nil nil
                                      (cond (buffer-file-name (file-relative-name buffer-file-name))
                                            ((eq major-mode 'dired-mode) (dired-get-filename t t))))
           current-prefix-arg
           shell-command-default-error-buffer))
    (let ((default-directory  (if (fboundp 'dired-default-directory) ; Emacs 21+.
                                  (dired-default-directory)
                                (default-directory))))
      (icicle-shell-command command output-buffer error-buffer))))


;; REPLACE ORIGINAL `shell-command' defined in `simple.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Use Icicles completion.
;;
;; Not needed for Emacs 23+ - Icicles completion is automatic via `icicle-read-shell-command'.
;;
(unless (fboundp 'read-shell-command)
  ;; Emacs < 23 only
  (unless (fboundp 'icicle-ORIG-shell-command)
    (defalias 'icicle-ORIG-shell-command (symbol-function 'shell-command)))

  (defun icicle-shell-command (command &optional output-buffer error-buffer)
    "Execute string COMMAND in inferior shell; display output, if any.
Uses Icicles completion - see `icicle-read-shell-command-completing'.

With prefix argument, insert the COMMAND's output at point.

If COMMAND ends in ampersand, execute it asynchronously.
The output appears in the buffer `*Async Shell Command*'.
That buffer is in shell mode.

Otherwise, COMMAND is executed synchronously.  The output appears in
the buffer `*Shell Command Output*'.  If the output is short enough to
display in the echo area (which is determined by the variables
`resize-mini-windows' and `max-mini-window-height'), it is shown
there, but it is nonetheless available in buffer `*Shell Command
Output*' even though that buffer is not automatically displayed.

To specify a coding system for converting non-ASCII characters
in the shell command output, use \\[universal-coding-system-argument] \
before this command.

Noninteractive callers can specify coding systems by binding
`coding-system-for-read' and `coding-system-for-write'.

The optional second argument OUTPUT-BUFFER, if non-nil,
says to put the output in some other buffer.
If OUTPUT-BUFFER is a buffer or buffer name, put the output there.
If OUTPUT-BUFFER is not a buffer and not nil,
insert output in current buffer.  (This cannot be done asynchronously.)
In either case, the output is inserted after point (leaving mark after it).

If the command terminates without error, but generates output,
and you did not specify \"insert it in the current buffer\",
the output can be displayed in the echo area or in its buffer.
If the output is short enough to display in the echo area
\(determined by the variable `max-mini-window-height' if
`resize-mini-windows' is non-nil), it is shown there.
Otherwise,the buffer containing the output is displayed.

If there is output and an error, and you did not specify \"insert it
in the current buffer\", a message about the error goes at the end
of the output.

If there is no output, or if output is inserted in the current buffer,
then `*Shell Command Output*' is deleted.

If the optional third argument ERROR-BUFFER is non-nil, it is a buffer
or buffer name to which to direct the command's standard error output.
If it is nil, error output is mingled with regular output.
In an interactive call, the variable `shell-command-default-error-buffer'
specifies the value of ERROR-BUFFER."
    (interactive
     (list (icicle-read-shell-command "Shell command: " nil nil
                                      (and buffer-file-name  (file-relative-name buffer-file-name)))
           current-prefix-arg
           shell-command-default-error-buffer))
    (icicle-ORIG-shell-command command output-buffer error-buffer)))


;; REPLACE ORIGINAL `shell-command-on-region' defined in `simple.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Use Icicles completion.
;;
;; Not needed for Emacs 23+ - Icicles completion is automatic via `icicle-read-shell-command'.
;;
(unless (fboundp 'read-shell-command)
  ;; Emacs < 23 only
  (unless (fboundp 'icicle-ORIG-shell-command-on-region)
    (defalias 'icicle-ORIG-shell-command-on-region (symbol-function 'shell-command-on-region)))

  (defun icicle-shell-command-on-region (start end command &optional output-buffer replace
                                         error-buffer display-error-buffer)
    "Execute string COMMAND in inferior shell with region as input.
Uses Icicles completion - see `icicle-read-shell-command-completing'.

Normally, display any output in temp buffer `*Shell Command Output*';
Prefix arg means replace the region with it.  Return the exit code of
COMMAND.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell command, use \\[universal-coding-system-argument]
before this command.  By default, the input (from the current buffer)
is encoded in the same coding system that will be used to save the file,
`buffer-file-coding-system'.  If the output is going to replace the region,
then it is decoded from that same coding system.

The noninteractive arguments are START, END, COMMAND,
OUTPUT-BUFFER, REPLACE, ERROR-BUFFER, and DISPLAY-ERROR-BUFFER.
Noninteractive callers can specify coding systems by binding
`coding-system-for-read' and `coding-system-for-write'.

If the command generates output, the output may be displayed
in the echo area or in a buffer.
If the output is short enough to display in the echo area
\(determined by the variable `max-mini-window-height' if
`resize-mini-windows' is non-nil), it is shown there.  Otherwise
it is displayed in the buffer `*Shell Command Output*'.  The output
is available in that buffer in both cases.

If there is output and an error, a message about the error
appears at the end of the output.

If there is no output, or if output is inserted in the current buffer,
then `*Shell Command Output*' is deleted.

If the optional fourth argument OUTPUT-BUFFER is non-nil,
that says to put the output in some other buffer.
If OUTPUT-BUFFER is a buffer or buffer name, put the output there.
If OUTPUT-BUFFER is not a buffer and not nil,
insert output in the current buffer.
In either case, the output is inserted after point (leaving mark after it).

If REPLACE, the optional fifth argument, is non-nil, that means insert
the output in place of text from START to END, putting point and mark
around it.

If optional sixth argument ERROR-BUFFER is non-nil, it is a buffer
or buffer name to which to direct the command's standard error output.
If it is nil, error output is mingled with regular output.
If DISPLAY-ERROR-BUFFER is non-nil, display the error buffer if there
were any errors.  (This is always t, interactively.)  This argument is
not available before Emacs 22.
In an interactive call, the variable `shell-command-default-error-buffer'
specifies the value of ERROR-BUFFER."
    (interactive (let (string)
                   (unless (mark) (icicle-user-error "The mark is not set now, so no region"))
                   ;; Do this before calling region-beginning and region-end, in case subprocess
                   ;; output relocates them while we are in the minibuffer.
                   (setq string  (icicle-read-shell-command "Shell command on region: "))
                   ;; call-interactively recognizes region-beginning and region-end specially,
                   ;; leaving them in the history.
                   (list (region-beginning) (region-end) string current-prefix-arg current-prefix-arg
                         shell-command-default-error-buffer (= emacs-major-version 22))))
    (if (= emacs-major-version 22)      ; `icicle-shell-command-on-region' not defined for Emacs 23+.
        (icicle-ORIG-shell-command-on-region start end command output-buffer replace error-buffer
                                     display-error-buffer)
      (icicle-ORIG-shell-command-on-region start end command output-buffer replace error-buffer))))

(defvar icicle-files () "A files list")


;; REPLACE ORIGINAL `dired-read-shell-command' defined in `dired-aux.el'
;; and redefined in `dired-x.el', saving it for restoration when you toggle `icicle-mode'.
;;
;; Use Icicles completion.
;; Use `icicle-minibuffer-default-add-dired-shell-commands', not
;; `minibuffer-default-add-dired-shell-commands'.
;; Bind `icicle-files' for use as free var elsewhere.
;; Added optional arg HISTORY.
;;
(defun icicle-dired-read-shell-command (prompt arg files &optional history)
  "Read a shell command for FILES using file-name completion.
Uses Icicles completion - see `icicle-read-shell-command-completing'.
ARG is passed to `dired-mark-prompt' as its first arg, for the prompt.
FILES are the files for which the shell command should be appropriate.
Optional arg HISTORY is an alternative minibuffer history to use,
 instead of the default, `shell-command-history'.  (HISTORY is not
 available for vanilla `dired-read-shell-command'.)"
  (let ((icicle-files  files))
    (minibuffer-with-setup-hook
     (lambda ()
       (set (make-local-variable 'minibuffer-default-add-function)
            'icicle-minibuffer-default-add-dired-shell-commands))
     (dired-mark-pop-up  nil 'shell files 'icicle-dired-guess-shell-command
                         (format prompt (dired-mark-prompt arg files)) files
                         (or history  'shell-command-history)))))

(defun icicle-dired-guess-shell-command (prompt files &optional history)
  "Read a shell command for FILES using file-name completion.
Call `icicle-read-shell-command-completing', passing the arguments.
If HISTORY is nil or not present then pass `shell-command-history'."
  (icicle-read-shell-command-completing prompt nil  (or history  'shell-command-history) nil nil files))

;; Similar to `minibuffer-default-add-dired-shell-commands', but if Dired-X is available
;; we include also the commands from `dired-guess-default'.
;;
;; Free var here: `icicle-files' is bound in `icicle-dired-read-shell-command'.
;;
(defun icicle-minibuffer-default-add-dired-shell-commands ()
  "Return a list of all commands associated with current dired files.
The commands are from `minibuffer-default-add-dired-shell-commands',
and if `dired-x.el' is used, `dired-guess-default'."
  (interactive)
  (let ((dired-guess-cmds  (and (boundp 'icicle-files)  (fboundp 'dired-guess-default)
                                (dired-guess-default icicle-files)))
        (mailcap-cmds      (and (boundp 'icicle-files)  (require 'mailcap nil t)
                                (mailcap-file-default-commands icicle-files))))
    (when (stringp dired-guess-cmds) (setq dired-guess-cmds  (list dired-guess-cmds)))
    (if (listp minibuffer-default)
        (append minibuffer-default dired-guess-cmds mailcap-cmds)
      (cons minibuffer-default (append dired-guess-cmds mailcap-cmds)))))

(defun icicle-read-shell-command-completing (prompt &optional initial-contents hist default-value
                                             _inherit-input-method files)
  "Read a shell command using file-name completion.
FILES name some files for which the command might be appropriate.
The other arguments are the same as those for `read-from-minibuffer',
except that READ and KEYMAP are missing, HIST defaults to
`shell-command-history', and _INHERIT-INPUT-METHOD is not used.

Completion is lax, so you can use any shell command you want, not
just a completion candidate, and you can edit the completed input to
add options and arguments etc.

In addition to file-name candidates, the following are combined to
produce extra completion candidates (which are indicated using face
`icicle-extra-candidates' in buffer `*Completions*'):

* If you use Dired X, then the rules defined by user option
  `dired-guess-shell-alist-user' and variable
  `dired-guess-shell-alist-default' provide candidates appropriate for
  the marked files in Dired.

* MIME-type associations provide candidates appropriate for the marked
  files (Emacs 23 and later), 

* If option `icicle-guess-commands-in-path' is non-nil, then
  executable files (or all files, if `shell-completion-execonly' is
  nil) in your search path provide candidates.

In addition, if `icicle-extra-candidates' is non-nil, its elements are
also included as extra candidates.

Help is available for individual candidates, using `C-M-RET',
`C-M-mouse-2', and so on.  For an extra candidate (that is, for a
shell command guessed to be appropriate), help is provided by the
`apropos' shell command (if available).  For a file name, help shows
the file's properties."
  (let* ((dired-guess-files                           (and files  (fboundp 'dired-guess-default)
                                                           (dired-guess-default files)))
         (icicle-sort-comparer                        'icicle-extra-candidates-first-p)
         (completion-ignore-case                      (memq system-type '(ms-dos windows-nt cygwin)))
         (insert-default-directory                    nil)
         (icicle-extra-candidates-dir-insert-p        nil)
         (icicle-point-position-in-candidate          'input-end)
         (icicle-candidate-help-fn                    (lambda (cand)
                                                        (if (member cand icicle-extra-candidates)
                                                            (icicle-with-help-window "*Help*"
                                                              (princ (shell-command-to-string
                                                                      (concat "apropos "
                                                                              (shell-quote-argument cand)))))
                                                          (icicle-describe-file cand nil 'NO-ERROR-P))))
         (icicle-extra-candidates                     icicle-extra-candidates)
         (icicle-must-match-regexp                    icicle-file-match-regexp)
         (icicle-must-not-match-regexp                icicle-file-no-match-regexp)
         (icicle-must-pass-after-match-predicate      icicle-file-predicate)
         (icicle-transform-function                   'icicle-remove-dups-if-extras)
         ;; (icicle-sort-comparer                        (or icicle-file-sort  icicle-sort-comparer))
         (icicle-require-match-flag                   icicle-file-require-match-flag)
         (icicle-default-value          ; Let user get default via `M-n', but don't insert it.
          (and (memq icicle-default-value '(t nil))  icicle-default-value)))
    (when (and dired-guess-files  (atom dired-guess-files))
      (setq dired-guess-files  (list dired-guess-files)))
    ;; Add dired-guess guesses and mailcap guesses to `icicle-extra-candidates'.
    (setq icicle-extra-candidates  (append dired-guess-files (and files  (require 'mailcap nil t) ; Emacs 23+.
                                                                  (fboundp 'mailcap-file-default-commands)
                                                                  (mailcap-file-default-commands files))
                                           icicle-extra-candidates))
    (when icicle-guess-commands-in-path ; Add commands available from user's search path.
      (setq icicle-extra-candidates  (append icicle-extra-candidates
                                             (or icicle-shell-command-candidates-cache
                                                 (icicle-recompute-shell-command-candidates)))))
    (when icicle-extra-candidates
      (setq prompt  (copy-sequence prompt)) ; So we can modify it by adding property.
      (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (let ((cmd  (icicle-read-file-name prompt nil default-value nil initial-contents nil hist)))
      (when icicle-quote-shell-file-name-flag (setq cmd (icicle-quote-file-name-part-of-cmd cmd)))
      cmd)))

(defun icicle-quote-file-name-part-of-cmd (strg)
  "Double-quote the file name that starts string STRG, for the shell.
This assumes a UNIX-style shell, for which the following characters
normally need to be escaped in file names: [ \t\n;<>&|()'\"#$].
This is appropriate, for example, if you use Cygwin with MS Windows.

STRG is assumed to be a shell command, possibly including arguments
and possibly ending with `&' to indicate asynchronous execution.

The beginning of STRG is assumed to be a file name, possibly including
the characters [ \t\n;<>&|()'\"#$].  This function double-quotes the
file name only, not the rest of STRG.

Example: If STRG is `c:/Program Files/My Dir/mycmd.exe arg1 arg2 &',
and file c:/Program Files/My Dir/mycmd.exe exists, then this returns
`\"c:/Program Files/My Dir/mycmd.exe\" arg1 arg2 &'."
  (save-match-data
    (if (not (string-match "[ \t\n;<>&|()'\"#$]" strg))
        strg
      (let ((indx         0)
            (compl        "")
            (filename     "")
            (quoted-strg  strg)
            prefix)
        (while (and indx                ; Find longest prefix that matches a file name.
                    (setq indx    (1+ (length compl)))
                    (<= indx (length strg))
                    (setq prefix  (substring strg 0 indx))
                    (setq compl   (try-completion prefix 'read-file-name-internal
                                                  (if (> emacs-major-version 22)
                                                      minibuffer-completion-predicate
                                                    default-directory))))
          (when (and (<= (length compl) (length strg))  (string-match compl strg 0)
                     (or (icicle-file-remote-p compl) ; Don't let Tramp try to access it.
                         (file-exists-p compl)))
            (setq filename compl)))
        (if (or (string= "" filename)
                (not (or (icicle-file-remote-p filename) ; Don't let Tramp try to access it.
                         (file-exists-p filename))))
            strg
          (setq quoted-strg  (concat "\"" filename "\""))
          (setq quoted-strg  (concat quoted-strg (substring strg (length filename)))))))))


;; REPLACE ORIGINAL `recentf-make-menu-items' defined in `recentf.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Add Icicles submenu to `Open Recent' menu.
;;
(defun icicle-recentf-make-menu-items (&optional menu)
  "Make menu items from the recent list.
This is a menu filter function which ignores the MENU argument."
  (setq recentf-menu-filter-commands  ())
  (let* ((recentf-menu-shortcuts  0)
         (file-items              (icicle-condition-case-no-debug err
                                      (mapcar 'recentf-make-menu-item
                                              (recentf-apply-menu-filter recentf-menu-filter
                                                                         (recentf-menu-elements
                                                                          recentf-max-menu-items)))
                        (error (message "recentf update menu failed: %s" (error-message-string err))))))
    (append (or file-items  '(["No files" t :help "No recent file to open" :active nil]))
            (if recentf-menu-open-all-flag
                '(["All..." recentf-open-files :help "Open recent files through a dialog" :active t])
              (and (< recentf-max-menu-items (length recentf-list)) ; `recentf-list' is free here.
                   '(["More..." recentf-open-more-files
                      :help "Open files not in the menu through a dialog" :active t])))
            (and recentf-menu-filter-commands     '("---")) recentf-menu-filter-commands
            (and recentf-menu-items-for-commands  '("---")) recentf-menu-items-for-commands
            (and icicle-mode
                 '(("Icicles"
                    ["+ Open Recent File..." icicle-recent-file]
                    ["+ Open Recent File (Other Window)..." icicle-recent-file-other-window]
                    ["+ Remove from Recent Files List..." icicle-remove-file-from-recentf-list]))))))

(when (fboundp 'read-file-local-variable) ; Emacs 23+, after `files-x.el' is loaded.
  (defadvice read-file-local-variable (around icicle-bind-variable-completing-p activate)
    "Provide predefined variable predicates for `M-&'."
    (let ((icicle-variable-completing-p  t))
      ad-do-it)))
 
;;(@* "Icicles Functions - Completion Display (Not Cycling)")

;;; Icicles Functions - Completion Display (Not Cycling) -------------

;; Similar to `diredp-mouseover-help'.
(defun icicle-mouseover-help (window buffer pos)
  "Show `help-echo' help for a file-name completion candidate.
If `tooltip-mode' is on, file named at POS is an image file, and
`icicle-image-preview-in-tooltip' is non-nil, then show image preview.
Otherwise, show textual help."
  (let ((image-dired-thumb-width   (or (and (wholenump icicle-image-preview-in-tooltip)
                                            icicle-image-preview-in-tooltip)
                                       image-dired-thumb-width))
        (image-dired-thumb-height  (or (and (wholenump icicle-image-preview-in-tooltip)
                                            icicle-image-preview-in-tooltip)
                                       image-dired-thumb-height))
        file)
    (or (and (boundp 'tooltip-mode)  tooltip-mode
             (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
             (fboundp 'image-file-name-regexp) ; Emacs 22+, `image-file.el'.
             icicle-image-preview-in-tooltip
             (condition-case nil
                 (and (with-current-buffer buffer
                        (goto-char pos)
                        (icicle-string-match-p (image-file-name-regexp)
                                               (setq file  (icicle-expand-file-name-20
                                                            (icicle-transform-multi-completion
                                                             (icicle-current-completion-in-Completions))
                                                            (icicle-file-name-directory-w-default
                                                             icicle-current-input)))))
                      (let ((img-file  (if (eq 'full icicle-image-preview-in-tooltip)
                                           file
                                         (icicle-create-thumb file))))
                        (propertize " " 'display (create-image img-file))))
               (error nil)))
        "mouse-2: visit this file in another window")))

;; Similar to `diredp-image-dired-create-thumb'.
(defun icicle-create-thumb (file &optional msgp)
  "Create thumbnail image file for FILE.
Return the name of the thumbnail image file, or nil if none."
  (interactive "fFile: \np")
  (let ((thumb-name  (image-dired-thumb-name file))
        result)
    (unless (file-exists-p thumb-name)
      (image-dired-create-thumb file thumb-name))
    (setq result  (and (file-exists-p thumb-name)  thumb-name))
    (when msgp (if result
                   (message "Created thumbnail file `%s'" thumb-name)
                 (message "Could not create thumbnail for `%s'" file)))
    result))

(when (> emacs-major-version 21)
  (defun icicle-Info-node-is-indexed-by-topic (node topic)
    "Return non-nil if Info NODE is indexed by TOPIC.
The value returned is (TOPIC NODE Info-current-file), and it is
cached in `icicle-Info-index-cache'."
    ;; FREE vars used here (bound in `icicle-Info-index'): `icicle-Info-index-nodes', `icicle-Info-manual'.
    (let ((pattern
           (format "\n\\* +\\([^\n]*%s[^\n]*\\):[ \t]+\\([^\n]*\\)\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?"
                   (regexp-quote topic)))
          ;; Bind these to nil to not perform menu fontification, to avoid its slowdown
          (Info-fontify-maximum-menu-size  nil)
          (Info-fontify-visited-nodes      nil)
          (Info-hide-note-references       nil)
          (index-nodes                     icicle-Info-index-nodes)
          nextnode)
      (and index-nodes
           (catch 'icicle-Info-node-is-indexed-by-topic
             (condition-case nil
                 (with-temp-buffer
                   (Info-mode)
                   (if (and (featurep 'info+)  (> emacs-major-version 21))
                       (Info-find-node icicle-Info-manual (car index-nodes) nil 'NOMSG)
                     (Info-find-node icicle-Info-manual (car index-nodes)))
                   (setq index-nodes  (cdr index-nodes))
                   (while (progn (goto-char (point-min))
                                 (while (re-search-forward pattern nil t)
                                   (when (string= (match-string-no-properties 2) node)
                                     (add-to-list 'icicle-Info-index-cache
                                                  (list topic node Info-current-file))
                                     (throw 'icicle-Info-node-is-indexed-by-topic
                                       (list topic node Info-current-file))))
                                 (setq index-nodes  (cdr index-nodes)
                                       nextnode     (car index-nodes)))
                     (Info-goto-node nextnode)))
               (error nil))
             nil)))))


;; REPLACE ORIGINAL `display-completion-list' (built-in function),
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; 1. Does not remove text properties from candidates when it displays them in `*Completions*'.
;; 2. Adjusts number of columns and their widths to window size.
;; 3. The optional second arg is ignored.  In vanilla Emacs < 23, this is a string
;;    representing a common prefix, and faces `completions-first-difference' and
;;    `completions-common-part' are used on candidates.
;;
(unless (fboundp 'icicle-ORIG-display-completion-list)
  (defalias 'icicle-ORIG-display-completion-list (symbol-function 'display-completion-list)))

(defun icicle-display-completion-list (completions &optional ignored nb-cands)
  "Display the list of completions, COMPLETIONS, using `standard-output'.
Each element may be just a symbol or string or may be a list of two
strings to be printed as if concatenated.
If it is a list of two strings, the first is the actual completion
alternative, the second serves as annotation.
`standard-output' must be a buffer.
The actual completion alternatives, as inserted, are given the
`mouse-face' property of `highlight'.
At the end, this runs the normal hook `completion-setup-hook'.
It can find the completion buffer in `standard-output'.
The optional second arg is ignored.
Non-nil optional third arg NB-CANDS is the length of COMPLETIONS."
  (if (not (bufferp standard-output))
      (let ((standard-output  (current-buffer))) (icicle-display-completion-list completions))
    (let ((mainbuf  (current-buffer)))  ; $$$$$$ For Emacs 23 crap that puts base-size in last cdr.
      (with-current-buffer standard-output
        (goto-char (point-max))
        (let ((cand-intro-string  (if completions
                                      "Possible completions are:\n"
                                    "There are no possible completions of what you have typed.")))
          (when (and icicle-show-Completions-help-flag  completions)
            (icicle-insert-Completions-help-string)
            (put-text-property 0 (length cand-intro-string) 'face 'icicle-Completions-instruction-1
                               cand-intro-string))
          ;; Show `There are no possible...' anyway, even if not `icicle-show-Completions-help-flag'.
          (when (or icicle-show-Completions-help-flag  (null completions))
            (insert cand-intro-string)))
        ;; $$$$$$$$ Emacs 23 nonsense.  Revisit this when Stefan finally removes that crud.
        ;; This is done in Emacs 23 `display-completion-list'.
        (when (and completions  (fboundp 'completion-all-sorted-completions)) ; Emacs 23
          (let ((last  (last completions)))
            ;; Set base-size from the tail of the list.
            (set (make-local-variable 'completion-base-size)
                 (or (cdr last)  (and (minibufferp mainbuf)  0)))
            (setcdr last nil)))         ; Make completions a properly nil-terminated list.
        (icicle-insert-candidates completions nb-cands)))
    ;; In vanilla Emacs < 23, the hook is run with `completion-common-substring' bound to
    ;; what is here called IGNORED.
    (run-hooks 'completion-setup-hook)
    nil))

(defun icicle-insert-candidates (candidates &optional number-of-candidates)
  "Insert completion candidates from list CANDIDATES into the current buffer.
Optional arg NUMBER-OF-CANDIDATES is the length of CANDIDATES."
  (when (consp candidates)
    (let ((annotation-fn  (or           ; Emacs 23+
                           (and icicle-last-completion-candidate
                                (fboundp 'completion-metadata-get)
                                (completion-metadata-get
                                 (completion-metadata icicle-last-completion-candidate
                                                      minibuffer-completion-table
                                                      minibuffer-completion-predicate)
                                 'annotation-function))
                           (and (boundp 'completion-extra-properties)  (plist-get completion-extra-properties
                                                                                  :annotation-function))
                           (and (boundp 'completion-annotate-function)  completion-annotate-function))))
      (when annotation-fn               ; Emacs 23+ uses a list (CAND ANNOTATION) for the candidate.
        (setq candidates  (mapcar (lambda (cand)
                                    (let ((ann  (condition-case nil
                                                    (funcall annotation-fn cand)
                                                  (error nil))))
                                      (if ann (list cand ann) cand)))
                                  candidates))))
    (let* ((multilinep       (lambda (cand)
                               (if (consp cand)
                                   (or (string-match "\n" (car cand))  (string-match "\n" (cadr cand)))
                                 (string-match "\n" cand))))
           (any-multiline-p  (loop for cand in candidates
                                   if (funcall multilinep cand) return t
                                   finally return nil))
           (max-cand-len     (apply #'max (mapcar (lambda (cand)
                                                    (if (consp cand)
                                                        (+ (length (car cand)) (length (cadr cand)))
                                                      (length cand)))
                                                  candidates)))
           (comp-win         (get-buffer-window (current-buffer) 0))
           (wwidth
            (let ((spcl-frame-params  (special-display-p (buffer-name))))
              (cond ((and spcl-frame-params ; Special-buffer.  Use its default frame width.
                          (or (and (consp spcl-frame-params)
                                   (cdr (assq 'width (cadr spcl-frame-params))))
                              (cdr (assq 'width special-display-frame-alist))
                              (cdr (assq 'width default-frame-alist)))))
                    (comp-win (1- (window-width comp-win))) ; Width picked by `display-buffer'.
                    (t 40))))           ; Failsafe.
           (nb-cands         (or number-of-candidates  (length candidates)))
           (columns          (if any-multiline-p
                                 1
                               (or icicle-Completions-max-columns
                                   (max 1 (min (/ (* 100 wwidth)
                                                  (* icicle-candidate-width-factor max-cand-len))
                                               nb-cands)))))
           (colwidth         (if (eq 1 columns) (min max-cand-len wwidth) (/ wwidth columns)))
           (column-nb        0)
           (rows             (ceiling nb-cands columns))
 	   (row              0)
           startpos endpos string)

      ;; Turn Icomplete mode on or off, depending on NB-CANDS.
      ;; Turn it on only if it has already been turned off here (non-nil `icicle-auto-no-icomplete-mode-p'),
      ;; for this minibuffer reading.  When turn it off, set flag `icicle-auto-no-icomplete-mode-p'.
      (when (and (featurep 'icomplete)  (natnump icicle-icomplete-mode-max-candidates)
                 (> emacs-major-version 22)) ; `icomplete-tidy' does not use overlay with Emacs < 23.
        (save-excursion
          (with-current-buffer (if (active-minibuffer-window)
                                   (window-buffer (active-minibuffer-window))
                                 (current-buffer))
            (if (< nb-cands icicle-icomplete-mode-max-candidates)
                (if (and icicle-auto-no-icomplete-mode-p  (not icomplete-mode)) ; Was turned off automatically.
                    (progn
                      (if (not icicle-last-icomplete-mode-value)
                          (icomplete-mode -1)
                        (icomplete-mode 1) ; Turn it back on.
                        (icomplete-exhibit))
                      (setq icicle-auto-no-icomplete-mode-p  nil)) ; And reset this.
                  (when icomplete-mode (icomplete-exhibit)))
              (setq icicle-last-icomplete-mode-value
                    (or icomplete-mode
                        icicle-last-icomplete-mode-value
                        (let ((cval  (or (get 'icomplete-mode 'saved-value)
                                         (get 'icomplete-mode 'standard-value))))
                          (condition-case nil (eval (car cval)) (error nil)))))
              (icomplete-tidy)
              (icomplete-mode -1)
              (setq icicle-auto-no-icomplete-mode-p  t)))))

      ;; Turn sorting on or off, depending on NB-CANDS.
      ;; Turn it on only if it has already been turned off here (non-nil `icicle-auto-no-sort-p'), for this
      ;; minibuffer reading.  When turn it off, set flag `icicle-auto-no-sort-p'.
      (when (natnump icicle-sorting-max-candidates)
        (if (< nb-cands icicle-sorting-max-candidates)
            (when (and icicle-auto-no-sort-p  (not icicle-sort-comparer)) ; Was turned off automatically.
              (setq icicle-sort-comparer   icicle-last-sort-comparer ; Turn it back on.
                    icicle-auto-no-sort-p  nil)) ; And reset this.
          (setq icicle-last-sort-comparer  (or icicle-sort-comparer
                                               icicle-last-sort-comparer
                                               (let ((cval  (or (get 'icicle-sort-comparer 'saved-value)
                                                                (get 'icicle-sort-comparer 'standard-value))))
                                                 (condition-case nil (eval (car cval)) (error nil))))
                icicle-sort-comparer       nil
                icicle-auto-no-sort-p      t)))
      (when (eq 1 columns) (setq wwidth  colwidth))
      (dolist (cand  candidates)
        (setq endpos  (point))
        (cond ((eq icicle-completions-format 'vertical) ; Vertical layout.
               (when (>= row rows)
                 (forward-line (- rows))
                 (setq column-nb  (+ column-nb colwidth)
                       row        0))
               (when (> column-nb 0)
                 (end-of-line)
                 (let ((cand-end  (point)))
                   (indent-to column-nb icicle-inter-candidates-min-spaces)
                   (put-text-property cand-end (point) 'mouse-face nil) ; Turn off `mouse-face', `face'
                   (put-text-property cand-end (point) 'face nil))))
              (t                        ; Horizontal layout (`horizontal' or nil).
               (unless (bolp)
                 (put-text-property (point) (point) 'mouse-face nil) ; Turn off `mouse-face'
                 (indent-to (* (max 1 column-nb) colwidth) icicle-inter-candidates-min-spaces)
                 (when (< wwidth (+ (max colwidth (if (consp cand)
                                                      (+ (length (car cand)) (length (cadr cand)))
                                                    (length cand)))
                                    (current-column)))
                   (save-excursion      ; This is like `fixup-whitespace', but only forward.
                     (delete-region (point) (progn (skip-chars-forward " \t") (point)))
                     (unless (or (looking-at "^\\|\\s)")
                                 (save-excursion (forward-char -1) (looking-at "$\\|\\s(\\|\\s'")))
                       (insert ?\ )))
                   (insert "\n")
                   (setq column-nb  columns))) ; End of the row. Simulate being in farthest column.
               (when (< endpos (point)) (set-text-properties endpos (point) nil))))
        ;; Convert candidate (but not annotation) to unibyte or to multibyte, if needed.
        (setq string  (icicle-unlist cand))
        (cond ((and (null enable-multibyte-characters)  (multibyte-string-p string))
               (setq string  (string-make-unibyte string)))
              ((and enable-multibyte-characters  (not (multibyte-string-p string)))
               (setq string  (string-make-multibyte string))))
        (put-text-property (point) (progn (insert string) (point)) 'mouse-face 'highlight) ; Insert cand.
        ;; Insert annotation, if `icicle-show-annotations-flag'.
        (when (and icicle-show-annotations-flag  (consp cand)  (cadr cand))
          (set-text-properties (point) (progn (insert (cadr cand)) (point)) '(face icicle-annotation)))
        (if (not (eq icicle-completions-format 'vertical))
            (setq column-nb  (mod (1+ column-nb) columns))
          (if (> column-nb 0) (forward-line) (insert "\n")) ; Vertical layout.
          (setq row  (1+ row)))
        (when (funcall multilinep cand) (insert (if (eq 'vertical icicle-completions-format) "\n" "\n\n"))))
      (when (eq icicle-completions-format 'vertical) ; Remove extra newline we inserted at eob.
        (save-excursion (goto-char (point-max)) (when (bolp) (delete-backward-char 1)))))))

;; ARG is not used in any calls yet/currently.
(defun icicle-fit-completions-window (&optional arg)
  "Fit the height of the window that is showing completions to its contents.
Optional ARG determines what the effect is, as follows:

 `fit-only'    - fit window to contents, but do not scale text size
 `scale-only'  - scale text size but do not fit window to contents
 anything else - scale text size and fit window to contents

Window fitting is available only for Emacs 24+, because
`fit-window-to-buffer' is broken for Emacs 21-23 (it can remove
windows).

Text size scaling uses `icicle-Completions-text-scale-decrease' and is
available only for Emacs 23+.  (No scaling in any case if using
`oneonone.el' with a `*Completions*' frame.)."
  (unless (or (eq arg 'scale-only)
              (= emacs-major-version 23) ; `fit-window-to-buffer' is broken before 24: removes windows.
              (= emacs-major-version 22))
    (when (and (eq major-mode 'completion-list-mode)  (fboundp 'fit-window-to-buffer))
      (let ((win  (get-buffer-window "*Completions*" 0)))
        (unless (< (window-width win) (frame-width)) ; Don't shrink if split horizontally.
          (fit-window-to-buffer
           win
           (or (icicle-get-safe icicle-last-top-level-command 'icicle-Completions-window-max-height)
               icicle-Completions-window-max-height))))))
  (unless (eq arg 'fit-only)
    (when (and (boundp 'icicle-Completions-text-scale-decrease) ; Emacs 23+
               (eq major-mode 'completion-list-mode)
               (or (not (boundp '1on1-*Completions*-frame-flag))  (not 1on1-*Completions*-frame-flag)))
      (text-scale-decrease icicle-Completions-text-scale-decrease))))

(defun icicle-highlight-initial-whitespace (input)
  "Highlight any initial whitespace in your input.
Only if `icicle-highlight-input-initial-whitespace-flag' is non-nil.
INPUT is the current user input, that is, the completion root.
This must be called in the minibuffer."
  (when (and icicle-highlight-input-initial-whitespace-flag  (not (string= "" input)))
    (let ((case-fold-search
           ;; Don't bother with buffer completion and `read-buffer-completion-ignore-case'.
           (if (and (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
                    (boundp 'read-file-name-completion-ignore-case))
               read-file-name-completion-ignore-case
             completion-ignore-case)))
      (save-excursion
        (goto-char (icicle-minibuffer-prompt-end))
        (when (and (icicle-file-name-input-p)  insert-default-directory)
          (search-forward (icicle-file-name-directory-w-default input) nil t)) ; Skip directory.
        (save-excursion
          (save-restriction
            (narrow-to-region (point) (point-max)) ; Search within completion candidate.
            (while (and (not (eobp))  (looking-at "\\(\\s-\\|\n\\)+"))
              (put-text-property (point) (min (point-max) (1+ (point)))
                                 'face 'icicle-whitespace-highlight)
              (forward-char 1))
            ;; Remove any previous whitespace highlighting that is no longer part of prefix.
            (while (not (eobp))
              (when (eq (get-text-property (point) 'face) 'icicle-whitespace-highlight)
                (put-text-property (point) (min (point-max) (1+ (point))) 'face nil))
              (forward-char 1))))))))

(defun icicle-minibuffer-prompt-end ()
  "Buffer position of end of minibuffer prompt, or `point-min'.
Version of `minibuffer-prompt-end' that works for Emacs 20 and later."
  (if (fboundp 'minibuffer-prompt-end) (minibuffer-prompt-end) (point-min)))
 
;;(@* "Icicles Functions - TAB Completion Cycling")

;;; Icicles Functions - TAB Completion Cycling --------------------

(defun icicle-prefix-candidates (input)
  "List of prefix or fuzzy completions for the current partial INPUT.
INPUT is a string.  Each candidate is a string."
  (setq icicle-candidate-nb  nil)
  (if (or (and (eq 'fuzzy (icicle-current-TAB-method))  (featurep 'fuzzy-match))
          (and (eq 'swank (icicle-current-TAB-method))  (featurep 'el-swank-fuzzy)))
      (condition-case nil
          (icicle-transform-candidates (append icicle-extra-candidates icicle-proxy-candidates
                                               (icicle-fuzzy-candidates input)))
        (quit (top-level)))             ; Let `C-g' stop it.
    (let ((cands  (icicle-unsorted-prefix-candidates input)))
      (if icicle-abs-file-candidates
          (icicle-strip-ignored-files-and-sort cands)
        (icicle-maybe-sort-maybe-truncate cands)))))

(defun icicle-fuzzy-candidates (input)
  "Return fuzzy matches for INPUT.  Handles also swank fuzzy symbol match."
  (condition-case nil
      (let ((candidates  ()))
        ;; $$$$ Should treat other `minibuffer-completion-table' types also.
        (cond ((and (vectorp minibuffer-completion-table)
                    (not (eq (icicle-current-TAB-method) 'swank)))
               (mapatoms (lambda (symb) (when (or (null minibuffer-completion-predicate)
                                                  (funcall minibuffer-completion-predicate symb))
                                          (push (symbol-name symb) candidates)))
                         minibuffer-completion-table)
               (setq candidates  (if (equal "" input)
                                     (sort (all-completions "" minibuffer-completion-table
                                                            minibuffer-completion-predicate)
                                           #'icicle-case-string-less-p)
                                   (FM-all-fuzzy-matches input candidates))))
              ((vectorp minibuffer-completion-table)
               (setq candidates  (mapcar #'car
                                         (car (el-swank-fuzzy-completions
                                               input icicle-swank-timeout
                                               (or minibuffer-completion-predicate  'fboundp)
                                               (min (length input) icicle-swank-prefix-length))))))
              ((and (consp minibuffer-completion-table)  (consp (car minibuffer-completion-table)))
               (dolist (cand minibuffer-completion-table)
                 (when (or (null minibuffer-completion-predicate)
                           (funcall minibuffer-completion-predicate cand))
                   (push (car cand) candidates)))
               (setq candidates  (if (equal "" input)
                                     (sort (all-completions "" minibuffer-completion-table
                                                            minibuffer-completion-predicate)
                                           #'icicle-case-string-less-p)
                                   (FM-all-fuzzy-matches input candidates)))))
        (let ((icicle-extra-candidates
               (icicle-remove-if-not (lambda (cand) (icicle-string-match-p input cand)) icicle-extra-candidates))
              (icicle-proxy-candidates
               (icicle-remove-if-not (lambda (cand) (icicle-string-match-p input cand)) icicle-proxy-candidates))
              (filtered-candidates
               (icicle-transform-candidates
                (append icicle-extra-candidates icicle-proxy-candidates
                        (icicle-remove-if-not
                         (lambda (cand)
                           (let ((case-fold-search  completion-ignore-case))
                             (and (icicle-filter-wo-input cand)
                                  (or (not icicle-must-pass-after-match-predicate)
                                      (funcall icicle-must-pass-after-match-predicate cand)))))
                         candidates)))))
          (setq icicle-common-match-string  (and filtered-candidates
                                                 (not (eq icicle-expand-input-to-common-match 0))
                                                 (icicle-expanded-common-match input filtered-candidates)))
          filtered-candidates))
    (quit (top-level))))                ; Let `C-g' stop it.

(defun icicle-unsorted-prefix-candidates (input)
  "Unsorted list of prefix completions for the current partial INPUT.
Unless `icicle-expand-input-to-common-match' = 0 this also sets
`icicle-common-match-string' to the expanded common match of the input
over all candidates."
  (condition-case nil
      (let* ((m-c-table
              ;; Prevent Emacs 23.2+ from using `internal-complete-buffer' if not ignoring space prefixes.
              ;; This lets `icicle-toggle-ignored-space-prefix' refresh to include space prefixes.
              (if (or (not (eq minibuffer-completion-table 'internal-complete-buffer))
                      icicle-buffer-ignore-space-prefix-flag)
                  minibuffer-completion-table
                (mapcar (lambda (buf) (and (buffer-live-p buf)  (list (buffer-name buf)))) (buffer-list))))
             (candidates
              (if (icicle-not-basic-prefix-completion-p)
                  (icicle-completion-all-completions input m-c-table minibuffer-completion-predicate
                                                     ;; $$$$$$ (- (point) (field-beginning)))
                                                     (length input)
                                                     (and (fboundp 'completion--field-metadata) ;Emacs 24
                                                          (completion--field-metadata (field-beginning))))
                (icicle-all-completions input m-c-table minibuffer-completion-predicate
                                        (and icicle-buffer-name-input-p ; Used only by Emacs < 23.2.
                                             icicle-buffer-ignore-space-prefix-flag))))
             (icicle-extra-candidates
              (icicle-remove-if-not (lambda (cand) (icicle-string-match-p (concat "^" (regexp-quote input)) cand))
                                    icicle-extra-candidates))
             (icicle-proxy-candidates
              (icicle-remove-if-not (lambda (cand) (icicle-string-match-p (concat "^" (regexp-quote input)) cand))
                                    icicle-proxy-candidates))
             (filtered-candidates
              (icicle-transform-candidates
               (append icicle-extra-candidates icicle-proxy-candidates
                       (icicle-remove-if-not
                        (lambda (cand)
                          (let ((case-fold-search  completion-ignore-case))
                            (and (icicle-filter-wo-input cand)
                                 (or (not icicle-must-pass-after-match-predicate)
                                     (funcall icicle-must-pass-after-match-predicate cand)))))
                        candidates)))))
        (setq icicle-common-match-string
              (and filtered-candidates
                   (not (eq icicle-expand-input-to-common-match 0))
                   (let ((common-prefix
                          (if (icicle-not-basic-prefix-completion-p)
                              (icicle-completion-try-completion input m-c-table minibuffer-completion-predicate
                                                                ;; $$$$$$ (- (point) (field-beginning)))
                                                                (length input)
                                                                (and (fboundp 'completion--field-metadata)
                                                                     (completion--field-metadata ; Emacs 24
                                                                      (field-beginning))))
                            (try-completion input m-c-table minibuffer-completion-predicate))))
                     (if icicle-must-pass-after-match-predicate
                         (icicle-expanded-common-match input filtered-candidates)
                       (if (eq t common-prefix) input common-prefix)))))
        filtered-candidates)
    (quit (top-level))))                ; Let `C-g' stop it.

(defun icicle-file-name-prefix-candidates (input)
  "List of prefix completions for partial file name INPUT.
INPUT is a string.
Candidates can be directories.  Each candidate is a string."
  (setq icicle-candidate-nb  nil)
  ;; $$$$$$ (let ((default-directory  (icicle-file-name-directory-w-default input)))
  ;; $$$$$$   (icicle-unsorted-file-name-prefix-candidates
  ;; $$$$$$     (or (icicle-file-name-nondirectory input)  ""))))
  (icicle-strip-ignored-files-and-sort (icicle-unsorted-file-name-prefix-candidates input)))

(defun icicle-unsorted-file-name-prefix-candidates (input)
  "Unsorted list of prefix completions for the current file-name INPUT.
Unless `icicle-expand-input-to-common-match' = 0 this also sets
`icicle-common-match-string' to the expanded common match of the input
over all candidates."
  (setq input  (substitute-in-file-name input))
  (let ((minibuffer-completion-table  (if (> emacs-major-version 23)
                                          icicle-file-name-completion-table
                                        minibuffer-completion-table)))
    (condition-case nil
        (let* ((pred  (if (< emacs-major-version 23) default-directory minibuffer-completion-predicate))
               (candidates
                (if (icicle-not-basic-prefix-completion-p)
                    (icicle-completion-all-completions
                     input  minibuffer-completion-table  pred  (length input)
                     (and (fboundp 'completion--field-metadata) ; Emacs 24
                          (completion--field-metadata (field-beginning))))
                  (icicle-all-completions input minibuffer-completion-table pred)))
               (icicle-extra-candidates
                (let ((relname  (file-name-nondirectory input)))
                  (icicle-remove-if-not
                   (lambda (cand) (icicle-string-match-p (concat "^" (regexp-quote relname)) cand))
                   icicle-extra-candidates)))
               (icicle-proxy-candidates
                (let ((relname  (file-name-nondirectory input)))
                  (icicle-remove-if-not
                   (lambda (cand) (icicle-string-match-p (concat "^" (regexp-quote relname)) cand))
                   icicle-proxy-candidates)))
               (filtered-candidates
                (icicle-transform-candidates
                 (append icicle-extra-candidates icicle-proxy-candidates
                         (icicle-remove-if-not
                          (lambda (cand)
                            (let ((case-fold-search
                                   (if (boundp 'read-file-name-completion-ignore-case)
                                       read-file-name-completion-ignore-case
                                     completion-ignore-case)))
                              (if (member cand '("../" "./"))
                                  (member input '(".." ".")) ; Prevent "" from matching "../"
                                (and
;;; $$$$$$ REMOVED - This was no good for PCM - e.g. input `ic-o' and candidates `icicles-opt.el[c]'.
;;;                  We don't do it for non-file-name completion, anyway, and it doesn't seem needed.
;;;                                  (save-match-data
;;;                                    (string-match (concat "^" (regexp-quote input)) cand))
                                 (icicle-filter-wo-input cand)
                                 (or (not icicle-must-pass-after-match-predicate)
                                     (funcall icicle-must-pass-after-match-predicate cand))))))
                          candidates)))))
          (setq icicle-common-match-string
                (and filtered-candidates
                     (not (eq icicle-expand-input-to-common-match 0))
                     (let ((common-prefix
                            (if (icicle-not-basic-prefix-completion-p)
                                (icicle-completion-try-completion input minibuffer-completion-table
                                                                  minibuffer-completion-predicate
                                                                  (length input)
                                                                  (and (fboundp 'completion--field-metadata)
                                                                       (completion--field-metadata ; Emacs 24
                                                                        (field-beginning))))
                              (try-completion input minibuffer-completion-table pred))))

                       ;; If common prefix matches an empty directory, use that dir as the sole completion.
                       (when (and (stringp common-prefix)
                                  (icicle-string-match-p "/\\.$" common-prefix)) ; Matches /., /..
                         (setq common-prefix  (substring common-prefix 0 (- (length common-prefix) 2))))
                       (if icicle-must-pass-after-match-predicate
                           (icicle-expanded-common-match input filtered-candidates)
                         (if (eq t common-prefix) input common-prefix)))))
          filtered-candidates)
      (quit (top-level)))))             ; Let `C-g' stop it.

;;; Similar to the vanilla function.  Only the `let' in each of the cond clauses is different.
;;; We do not remove a `$' prefix.  (So we return 0 as the first boundary.)
(when (> emacs-major-version 23)
  (defun icicle-completion--embedded-envvar-table (string _pred action)
    "Completion table for environment variables embedded in a string.
The envvar syntax (and escaping) rules followed by this table are the
same as for `substitute-in-file-name'."
    ;; Ignore _PRED arg, because predicates passed via `read-file-name-internal' are not 100% correct and
    ;; fail here: e.g. predicates like `file-directory-p', whereas the filename completed needs to be passed
    ;; through `substitute-in-file-name' before it can be passed to `file-directory-p'.
    (when (string-match completion--embedded-envvar-re string)
      (let* ((beg     (or (match-beginning 2)  (match-beginning 1)))
             (table   (completion--make-envvar-table))
             (prefix  (substring string 0 beg)))
        (cond ((eq action 'lambda)
               ;; This table is expected to be used in conjunction with some other table that provides the
               ;; main completion.  Let the other table handle the test-completion case.
               nil)
              ((or (eq (car-safe action) 'boundaries)  (eq action 'metadata))
               ;; Return boundaries/metadata only if there's something to complete, since otherwise when
               ;; used in `completion-table-in-turn', we could return boundaries and let some subsequent
               ;; table return a list of completions.
               ;;
               ;; FIXME: Maybe it should rather be fixed in `completion-table-in-turn' instead, but it's
               ;; difficult to do it efficiently there.
               (when (try-completion (substring string beg) table nil)
                 ;; Compute the boundaries of the subfield to which this completion applies.
                 (if (eq action 'metadata)
                     '(metadata (category . environment-variable))
                   (let ((suffix  (cdr action)))
                     `(boundaries 0     ; Return 0 as first boundary, since we do not remove `$' prefix.
                       . ,(when (string-match "[^[:alnum:]_]" suffix) (match-beginning 0)))))))
              (t
               (when (eq ?{  (aref string (1- beg)))
                 (setq table  (apply-partially 'completion-table-with-terminator "}" table)))
               ;; Envvar completion must be case-sensitive, even when file-name completion is not.
               (let* ((completion-ignore-case  nil)
                      (comp                    (complete-with-action action table (substring string beg)
                                                                     (lambda (&rest args) t))))
                 (if (stringp comp) (concat prefix comp) (mapcar (lambda (s) (concat prefix s)) comp)))))))))
 
;;(@* "Icicles Functions - S-TAB Completion Cycling")

;;; Icicles Functions - S-TAB Completion Cycling -------------------

(defun icicle-apropos-candidates (input)
  "List of candidate apropos completions for the current partial INPUT.
INPUT is a string.  Each candidate is a string."
  (setq icicle-candidate-nb  nil)
  (let ((cands  (icicle-unsorted-apropos-candidates input)))
    (if icicle-abs-file-candidates
        (icicle-strip-ignored-files-and-sort cands)
      (icicle-maybe-sort-maybe-truncate cands))))

(defun icicle-unsorted-apropos-candidates (input)
  "Unsorted list of apropos completions for the current partial INPUT.
Unless `icicle-expand-input-to-common-match' = 0 this also sets
`icicle-common-match-string' to the expanded common match of the input
over all candidates."
  (condition-case nil
      (progn
        (when icicle-regexp-quote-flag  (setq input  (regexp-quote input)))
        (let* ((m-c-table
                ;; Prevent Emacs 23.2+ from using `internal-complete-buffer' if not ignoring space prefixes.
                ;; This lets `icicle-toggle-ignored-space-prefix' refresh to include space prefixes.
                (if (or (not (eq minibuffer-completion-table 'internal-complete-buffer))
                        icicle-buffer-ignore-space-prefix-flag)
                    minibuffer-completion-table
                  (mapcar (lambda (buf) (and (buffer-live-p buf)  (list (buffer-name buf)))) (buffer-list))))
               (candidates
                (if (and (functionp m-c-table)  (not icicle-apropos-complete-match-fn))
                    ;; Let the function do it all.
                    (icicle-all-completions input m-c-table minibuffer-completion-predicate
                                            (and icicle-buffer-name-input-p ; Used only by Emacs < 23.2.
                                                 icicle-buffer-ignore-space-prefix-flag))
                  (icicle-all-completions "" m-c-table minibuffer-completion-predicate
                                          (and icicle-buffer-name-input-p ; Used only by Emacs < 23.2.
                                               icicle-buffer-ignore-space-prefix-flag))))
               (icicle-extra-candidates
                (icicle-remove-if-not (lambda (cand) (icicle-string-match-p input cand)) icicle-extra-candidates))
               (icicle-proxy-candidates
                (icicle-remove-if-not (lambda (cand) (icicle-string-match-p input cand)) icicle-proxy-candidates))
               (filtered-candidates
                (icicle-transform-candidates
                 (append icicle-extra-candidates icicle-proxy-candidates
                         (icicle-remove-if-not
                          (lambda (cand)
                            (let ((case-fold-search  completion-ignore-case))
                              (and (icicle-filter-wo-input cand)
                                   (or (not icicle-apropos-complete-match-fn)
                                       ;; Assume no match if error - e.g. due to `string-match' with
                                       ;; binary data in Emacs 20.  Do this everywhere we call
                                       ;; `icicle-apropos-complete-match-fn'.
                                       (condition-case nil
                                           (funcall icicle-apropos-complete-match-fn input cand)
                                         (error nil)))
                                   (or (not icicle-must-pass-after-match-predicate)
                                       (funcall icicle-must-pass-after-match-predicate cand)))))
                          candidates)))))
          (setq icicle-common-match-string  (and filtered-candidates
                                                 (not (eq icicle-expand-input-to-common-match 0))
                                                 (icicle-expanded-common-match input filtered-candidates)))
          filtered-candidates))         ; Return candidates.
    (quit (top-level))))                ; Let `C-g' stop it.

(defun icicle-file-name-apropos-candidates (input)
  "List of apropos completions for partial file-name INPUT.
INPUT is a string.
Candidates can be directories.  Each candidate is a string."
  (setq icicle-candidate-nb  nil)
  (let ((default-directory  (icicle-file-name-directory-w-default input)))
    (icicle-strip-ignored-files-and-sort
     (icicle-unsorted-file-name-apropos-candidates (or (icicle-file-name-nondirectory input)  "")))))

(defun icicle-unsorted-file-name-apropos-candidates (input)
  "Unsorted list of apropos completions for the partial file-name INPUT.
Unless `icicle-expand-input-to-common-match' = 0 this also sets
`icicle-common-match-string' to the expanded common match of the input
over all candidates."
  (condition-case nil
      (progn
        (when icicle-regexp-quote-flag (setq input  (regexp-quote input)))
        (let* ((pred  (if (< emacs-major-version 23) default-directory minibuffer-completion-predicate))
               (candidates
                ;; $$$$$ Should we remove string test for Emacs 23?
                (if (and (not (stringp minibuffer-completion-predicate))
                         (not icicle-apropos-complete-match-fn)
                         (functionp minibuffer-completion-table))
                    ;; Let the function do it all.
                    (icicle-all-completions input minibuffer-completion-table pred)
                  (icicle-all-completions "" minibuffer-completion-table pred)))
               (icicle-extra-candidates
                (icicle-remove-if-not (lambda (cand) (icicle-string-match-p input cand)) icicle-extra-candidates))
               (icicle-proxy-candidates
                (icicle-remove-if-not (lambda (cand) (icicle-string-match-p input cand)) icicle-proxy-candidates))
               (filtered-candidates
                (icicle-transform-candidates
                 (append icicle-extra-candidates icicle-proxy-candidates
                         (icicle-remove-if-not
                          (lambda (cand)
                            (let ((case-fold-search
                                   (if (boundp 'read-file-name-completion-ignore-case)
                                       read-file-name-completion-ignore-case
                                     completion-ignore-case)))
                              (if (member cand '("../" "./"))
                                  (member input '(".." ".")) ; Prevent "" from matching "../"
                                (and (icicle-filter-wo-input cand)
                                     (or (not icicle-apropos-complete-match-fn)
                                         ;; Assume no match if error - e.g. due to `string-match'
                                         ;; with binary data in Emacs 20.  Do this everywhere we
                                         ;; call `icicle-apropos-complete-match-fn'.
                                         (condition-case nil
                                             (funcall icicle-apropos-complete-match-fn input cand)
                                           (error nil)))
                                     (or (not icicle-must-pass-after-match-predicate)
                                         (funcall icicle-must-pass-after-match-predicate cand))))))
                          candidates)))))
          (setq icicle-common-match-string  (and filtered-candidates
                                                 (not (eq icicle-expand-input-to-common-match 0))
                                                 (icicle-expanded-common-match input filtered-candidates)))
          filtered-candidates))         ; Return candidates.
    (quit (top-level))))                ; Let `C-g' stop it.

(defun icicle-expanded-common-match (input candidates)
  "Return the expanded common match for INPUT among all CANDIDATES.
This assumes that INPUT matches each string in list CANDIDATES.
Return nil if there is no common match.

The expanded common match is typically, but not always, the longest
common match.  See the documentation, section `Expanded-Common-Match
Completion', for details."
  ;; Since `icicle-expanded-common-match-1' checks only the first match for a single candidate,
  ;; we call it twice, once using the first candidate and once using the second.
  ;; Typically, one of these tries will give us the longest common match.
  (catch 'ecm-error
    (let ((first-try   (icicle-expanded-common-match-1 input candidates))
          (second-try  nil))
      (when (and first-try  (cadr candidates))
        (setq second-try  (icicle-expanded-common-match-1
                           input (cons (cadr candidates) (cons (car candidates) (cddr candidates))))))
      (if (> (length second-try) (length first-try))  second-try  first-try))))

(defun icicle-expanded-common-match-1 (input candidates)
  "Helper function for `icicle-expanded-common-match."
  ;; This does not always give a longest common match, because it looks only at the first match
  ;; of INPUT with the first candidate.  What it returns is the longest match that is common to
  ;; all CANDIDATES and also contains the first match in the first candidate.
  (let ((case-fold-search
         ;; Do not bother with buffer completion and `read-buffer-completion-ignore-case'.
         (if (and (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
                  (boundp 'read-file-name-completion-ignore-case))
             read-file-name-completion-ignore-case
           completion-ignore-case))
        (first  (car candidates)))
    ;; Apart from this condition, use `string-match' in this function, not `icicle-apropos-complete-match-fn'.
    ;; `string-match' should always be the right thing, and `icicle-apropos-complete-match-fn' is sometimes
    ;; inappropriate - e.g., when it is `icicle-apropos-complete-match-fn'.
    (and icicle-apropos-complete-match-fn ; Return nil if no match function.
         (save-match-data
           ;; Assume no common match in case of error - e.g. due to `string-match' with binary data
           ;; in Emacs 20.  Do this throughout, whenever we call `string-match' or
           ;; `icicle-apropos-complete-match-fn'.
           (unless (condition-case nil
                       ;; Use `string-match' here, not `icicle-apropos-complete-match-fn'.
                       (string-match input first)
                     (error (throw 'ecm-error nil)))
             (error (throw 'ecm-error nil))) ; If input doesn't match candidate, return nil.
           (let* ((len-first       (length first))
                  (beg             0)
                  (end             len-first)
                  (orig-match-beg  (match-beginning 0))
                  (orig-match-end  (match-end 0))
                  (ecm             first) ; "ecm" for "expanded common match".
                  (rest            (cdr candidates))
                  beg-ecm beg-next)
             (if (= orig-match-beg end)
                 (setq ecm  "")         ; INPUT was, for instance, "$" or "\\>$; return "".
               ;; Compare with the rest of the candidates, reducing as needed.
               (while (and rest  ecm)
                 (condition-case nil
                     ;; Use `string-match' here, not `icicle-apropos-complete-match-fn'.
                     (string-match input (car rest))
                   (error (throw 'ecm-error nil))) ; If input doesn't match candidate, return nil.
                 (setq beg-next  (match-beginning 0))
                 ;; Remove any prefix that doesn't match some other candidate.
                 (while (and (< beg orig-match-beg)
                             (not (condition-case nil
                                      ;; Use `string-match' here, not `icicle-apropos-complete-match-fn'.
                                      (string-match (regexp-quote (substring ecm 0 (- orig-match-end beg)))
                                                    (car rest))
                                    (error (throw 'ecm-error nil))))
                             (progn (setq beg-ecm  (match-beginning 0))  (>= beg-ecm beg-next)))
                   ;; Take a character off of the left.
                   (setq ecm  (substring ecm 1)
                         beg  (1+ beg)))
                 ;; Remove any suffix that doesn't match some other candidate.
                 (while (and (> end 0)
                             (not (condition-case nil
                                      ;; Use `string-match' here, not `icicle-apropos-complete-match-fn'.
                                      (string-match (regexp-quote ecm) (car rest))
                                    (error (throw 'ecm-error nil)))))
                   ;; Take a character off of the right.
                   (setq ecm  (substring ecm 0 (1- (length ecm)))
                         end  (1- end)))
                 (unless (and (condition-case nil
                                  ;; Use `string-match' here, not `icicle-apropos-complete-match-fn'.
                                  (string-match (regexp-quote ecm) (car rest))
                                (error (throw 'ecm-error nil)))
                              (condition-case nil ; Input must match the substring that is common.
                                  ;; Use `string-match' here, not `icicle-apropos-complete-match-fn'.
                                  (string-match input ecm)
                                (error (throw 'ecm-error nil))))
                   (setq ecm  nil))     ; No possible expansion
                 (pop rest))
               ecm))))))

(defun icicle-multi-comp-apropos-complete-match (input candidate)
  "Match function for progressive completion with multi-completions.
Return non-nil if current multi-completion INPUT matches CANDIDATE.
INPUT is split by `icicle-list-join-string' and rejoined, adding the
equivalent of `.*' to the join strings, where `.' matches newline too.
The resulting regexp is matched against CANDIDATE."
  (let* ((any      (concat icicle-dot-string "*"))
         (len-any  (length any))
         (fields   (save-match-data (split-string input (regexp-quote icicle-list-join-string))))
         (first    (car fields))
         (regexps  ()))
    (unless (and (>= (length first) len-any)  (string= any (substring first (- len-any))))
      (setq first  (concat first any)))
    (setq fields  (cdr fields))
    (dolist (field  fields)
      (unless (or (string= "" field)
                  (and (>= (length field) len-any)  (string= any (substring field 0 len-any))))
        (setq field  (concat any field)))
      (push field regexps))
    (string-match (concat first icicle-list-join-string (mapconcat #'identity (nreverse regexps)
                                                                   icicle-list-join-string))
                  candidate)))

(defun icicle-scatter-match (string completion)
  "Returns non-nil if STRING scatter-matches COMPLETION.
This means that all of the characters in STRING are also in string
COMPLETION, in the same order, but perhaps scattered among other
characters.  For example, STRING = \"ure\" matches COMPLETION
\"curried\"."
  (string-match (icicle-scatter string) completion))

(defun icicle-scatter (string)
  "Returns a regexp that matches a scattered version of STRING.
The regexp will match any string that contains the characters in
STRING, in the same order, but possibly with other characters as well.
Returns, e.g., \"a[^b]*b[^c]*c[^d]*d\" for input string \"abcd\"."
  ;; This backtracking version is a bit slower, esp. for Emacs 20:
  ;; (if (> emacs-major-version 21)
  ;;     (mapconcat #'regexp-quote (split-string string "" t) ".*")
  ;;   (mapconcat #'regexp-quote (split-string string "") ".*"))
  ;;
  ;; This version is from Emacs bug #12796.
  (let ((first  t))
    (mapconcat (lambda (ch)
                 (if (not first)
                     (concat "[^" (string ch) "]*" (regexp-quote (string ch)))
                   (setq first  nil)
                   (regexp-quote (string ch))))
               string
               "")))

(defun icicle-levenshtein-strict-match (s1 s2)
  "String S1 is within `icicle-levenshtein-distance' of string S2.
This means that S1 differs by at most `icicle-levenshtein-distance'
character deletions, insertions, or replacements from S2.  The string
lengths too must differ by at most `icicle-levenshtein-distance'.
You probably want to turn off incremental completion (`C-#') if you
use this match method; it is quite slow.
To use this match method, you must also have library `levenshtein.el'."
  (unless (require 'levenshtein nil t)  (icicle-user-error "You need library `levenshtein.el' for this"))
  (<= (levenshtein-distance s1 s2) icicle-levenshtein-distance))

(defun icicle-levenshtein-match (s1 s2)
  "String S1 is within `icicle-levenshtein-distance' of a substring of S2.
S1 and S2 are strings.  This means that S1 and some substring of S2
differ by at most `icicle-levenshtein-distance' character deletions,
insertions, or replacements.

You will probably want to turn off incremental completion (`C-#') if
you use this match method; it can be quite slow, especially with a
large value of `icicle-levenshtein-distance'.  To use this method with
a value other than 1, you must also have library `levenshtein.el'."
  (if (= icicle-levenshtein-distance 1)
      (icicle-levenshtein-one-match s1 s2)
    (unless (require 'levenshtein nil t)  (icicle-user-error "You need library `levenshtein.el' for this"))
    (catch 'icicle-levenshtein-match
      (dolist (sub  (icicle-substrings-of-length s2 (length s1)))
        (when (<= (levenshtein-distance s1 sub) icicle-levenshtein-distance)
          (throw 'icicle-levenshtein-match t)))
      nil)))

;; This is much faster than testing with `levenshtein-distance' and a value of 1.
(defun icicle-levenshtein-one-match (s1 s2)
  "S1 is within a Levenshtein distance of one of some substring of S2.
That is, S1 with 0 or 1 char inserted, deleted or replaced is a
substring of S2.  S1 and S2 are strings.
You do not need library `levenshtein.el' to use this function."
  (string-match (icicle-levenshtein-one-regexp s1) s2))

(defun icicle-levenshtein-one-regexp (string)
  "Return a regexp for strings that are 1 Levenshtein unit from STRING."
  (let ((indx    0)
        (regexp  "\\("))
    (dotimes (indx  (length string))
      (setq regexp (concat regexp (substring string 0 indx) ".?" (substring string (1+ indx)) "\\|"
                           (substring string 0 indx) "."  (substring string indx)      "\\|")))
    (setq regexp (concat (substring regexp 0 -1) ")"))))

(defun icicle-substrings-of-length (string &optional len)
  "Return a list of substrings of STRING that have length LEN.
If LEN is nil, treat it as the length of STRING."
  (unless len (setq len  (length string)))
  (if (zerop len)
      (list "")
    (let ((subs  ()))
      (dotimes (idx (- (length string) (1- len)))  (push (substring string idx (+ idx len))  subs))
      (nreverse subs))))
 
;;(@* "Icicles Functions - Common Helper Functions")

;;; Icicles Functions - Common Helper Functions ----------------------

(defun icicle-try-switch-buffer (buffer)
  "Try to switch to BUFFER, first in same window, then in other window.
If the selected window already shows BUFFER, then do nothing."
  (when (and (buffer-live-p buffer)  (not icicle-inhibit-try-switch-buffer))
    (condition-case err-switch-to
        (unless (eq (window-buffer) buffer) (switch-to-buffer buffer))
      (error (and (string= "Cannot switch buffers in minibuffer window"
                           (error-message-string err-switch-to))
                  ;; Try another window.  Don't bother if the buffer to switch to is a minibuffer.
                  (condition-case err-switch-other
                      (unless (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))
                        (switch-to-buffer-other-window buffer))
                    (error (error-message-string err-switch-other)))))))) ; Return error message string.

;; Main cycling function - used by `icicle-next-prefix-candidate', `icicle-next-apropos-candidate'.
(defun icicle-next-candidate (nth candidates-fn &optional regexp-p)
  "Replace input by NTH next or previous completion for an input.
Default value of NTH is 1, meaning use the next completion.
Negative NTH means use a previous, not subsequent, completion.

CANDIDATES-FN is a function that returns the list of candidate
completions for its argument, the current partial input (a string).

Optional arg REGEXP-P non-nil means that CANDIDATES-FN uses regexp
matching. This is used to highlight the appropriate matching root.

If option `icicle-help-in-mode-line-delay' is positive, then help on
the current candidate is shown in the mode line."
  (let ((saved-last-input  icicle-last-input)) ; For call to `icicle-recompute-candidates'.
    (unless (stringp icicle-last-completion-candidate)
      (setq icicle-last-completion-candidate  icicle-initial-value))
    (setq nth                   (or nth  1)
          icicle-current-input  (if (icicle-file-name-input-p) ; But not for `icicle-abs-file-candidates'.
                                    (abbreviate-file-name (icicle-input-from-minibuffer 'leave-envar))
                                  (icicle-input-from-minibuffer))
          icicle-cycling-p      t)
    (unless (and (icicle-get-safe this-command 'icicle-apropos-cycling-command)
                 (or (icicle-get-safe last-command 'icicle-apropos-cycling-command)
                     (memq last-command
                           '(icicle-candidate-action
                             icicle-remove-candidate icicle-mouse-remove-candidate
                             icicle-apropos-complete icicle-apropos-complete-no-display))))
      (setq icicle-common-match-string  nil)) ; Don't use old one, in `icicle-save-or-restore-input'.
    (icicle-save-or-restore-input)
    (unless (eq this-command last-command)
      (icicle-recompute-candidates nth candidates-fn saved-last-input))
    (icicle-save-or-restore-input)      ; Again, based on updated `icicle-common-match-string'.
    (cond ((null icicle-completion-candidates)
           (save-selected-window (icicle-remove-Completions-window))
           (minibuffer-message "  [No completion]"))
          (t
           (icicle-clear-minibuffer)
           (let ((nb-cands  (length icicle-completion-candidates))
                 (unit      (if (wholenump nth) 1 -1))
                 next)
             ;; So `icomplete+' can append the number of other candidates to the minibuffer.
             (setq icicle-nb-of-other-cycle-candidates  (1- nb-cands))
             (icicle-increment-cand-nb+signal-end nth nb-cands)
             (setq next  (elt icicle-completion-candidates icicle-candidate-nb))
             (while (null next)         ; Skip null candidates.
               (icicle-increment-cand-nb+signal-end unit nb-cands)
               (setq next  (elt icicle-completion-candidates icicle-candidate-nb)))

             ;; Update last-candidate to NEXT.  Need a copy, because we change its text properties.
             (setq icicle-last-completion-candidate  (copy-sequence next))

             (icicle-insert-cand-in-minibuffer icicle-last-completion-candidate regexp-p)

             ;; Highlight current completion candidate, if `*Completions*' is displayed.
             (when (get-buffer-window "*Completions*" 0)

               ;; Refresh `*Completions*', updating it to reflect the current candidates.
               (unless (or (and (icicle-get-safe this-command 'icicle-apropos-cycling-command)
                                (or (icicle-get-safe last-command 'icicle-apropos-cycling-command)
                                    (memq last-command '(icicle-candidate-action
                                                         icicle-remove-candidate
                                                         icicle-mouse-remove-candidate))))
                           (and (icicle-get-safe this-command 'icicle-prefix-cycling-command)
                                (or (icicle-get-safe last-command 'icicle-prefix-cycling-command)
                                    (memq last-command '(icicle-candidate-action
                                                         icicle-remove-candidate
                                                         icicle-mouse-remove-candidate)))))
                 (icicle-display-candidates-in-Completions))
               (save-selected-window
                 (select-window (get-buffer-window "*Completions*" 'visible))
                 (if (fboundp 'thumfr-only-raise-frame) (thumfr-only-raise-frame) (raise-frame)))
               (icicle-highlight-candidate-in-Completions))
             (setq icicle-mode-line-help  icicle-last-completion-candidate))))))

(defun icicle-insert-cand-in-minibuffer (candidate regexp-p)
  "Insert CANDIDATE in minibuffer.  Highlight root and initial whitespace.
REGEXP-P non-nil means use regexp matching to highlight root."
  ;; Highlight any initial whitespace (probably a user typo).
  (icicle-highlight-initial-whitespace (if regexp-p icicle-current-raw-input icicle-current-input))

  ;; Underline the root that was completed, in the minibuffer.
  (let ((inp  (icicle-minibuf-input-sans-dir icicle-current-input))
        (case-fold-search
         ;; Don't bother with buffer completion and `read-buffer-completion-ignore-case'.
         (if (and (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
                  (boundp 'read-file-name-completion-ignore-case))
             read-file-name-completion-ignore-case
           completion-ignore-case))
        indx)
    (unless (and regexp-p  (not icicle-regexp-quote-flag)) (setq inp  (regexp-quote inp)))
    (save-match-data
      (setq indx  (condition-case nil   ; Ignore errors, in case INP is, say, "\".
                      (string-match inp icicle-last-completion-candidate)
                    (error nil)))
      (when indx
        ;; Should not need to ignore errors, but `*-last-completion-candidate' has been a read-only object (?)
        (condition-case nil
            (put-text-property indx (match-end 0) 'face 'icicle-match-highlight-minibuffer
                               icicle-last-completion-candidate)
          (error nil)))))

  (goto-char (icicle-minibuffer-prompt-end)) ; Need for Emacs 22+, or can get `Text read-only' error.
  ;; Insert candidate in minibuffer, and place cursor.
  (insert (if (and (icicle-file-name-input-p)
                   ;; $$$$$$ insert-default-directory ; (2012-06-28)
                   (or (not (member icicle-last-completion-candidate icicle-extra-candidates))
                       icicle-extra-candidates-dir-insert-p))
              (icicle-dir-prefix-wo-wildcards icicle-current-input)
            "")
          candidate)
  (icicle-place-cursor icicle-current-input))

(defun icicle-dir-prefix-wo-wildcards (filename)
  "Return the directory portion of FILENAME.
If using partial completion, this is the portion before the first
occurrence of `*'.  Otherwise, this is just `file-name-directory'."
  (if (and (icicle-not-basic-prefix-completion-p)  (boundp 'completion-styles)
           (member 'partial-completion completion-styles)
           (string-match "/[^/]*\\*" filename))
      (substring filename 0 (1+ (match-beginning 0)))
    (or (file-name-directory filename)  ""))) ; Don't return nil, in any case.

(defun icicle-recompute-candidates (nth candidates-fn saved-last-input)
  "Recompute `icicle-completion-candidates', if needed.
If buffer `*Completions*' is already displayed, it is updated.
This does nothing, unless the user changed the minibuffer input or the
completion type has changed (from apropos to prefix or vice versa).
NTH < 0 means candidate order is reversed in `*Completions*'.
Argument CANDIDATES-FN is a function that recomputes the candidates.
SAVED-LAST-INPUT is the last input, as in `icicle-last-input'."
  (unless (and icicle-last-completion-command
               (string= icicle-current-input saved-last-input) ; No change in user input.
               ;; No change in completion type: apropos vs prefix.
               (or (and (or (icicle-get-safe icicle-last-completion-command
                                             'icicle-apropos-completing-command)
                            (memq icicle-last-completion-command
                                  '(icicle-candidate-set-complement icicle-mouse-remove-candidate
                                    icicle-keep-only-past-inputs)))
                        (symbolp 'this-command)
                        (or (get this-command 'icicle-apropos-completing-command)
                            (get this-command 'icicle-apropos-cycling-command)))
                   (and (or (icicle-get-safe icicle-last-completion-command
                                             'icicle-prefix-completing-command)
                            (memq icicle-last-completion-command
                                  '(icicle-candidate-set-complement icicle-mouse-remove-candidate
                                    icicle-keep-only-past-inputs)))
                        (symbolp this-command)
                        (or (get this-command 'icicle-prefix-completing-command)
                            (get this-command 'icicle-prefix-cycling-command)))))
    (when (string= icicle-current-input saved-last-input) ; Changed completion type, not user input.
      ;; Set `icicle-last-completion-command', to record new completion type.
      (cond ((icicle-get-safe this-command 'icicle-prefix-cycling-command)
             (setq icicle-last-completion-command
                   (if (eq icicle-last-completion-command 'icicle-apropos-complete-no-display)
                       'icicle-prefix-complete-no-display
                     'icicle-prefix-complete)))
            ((icicle-get-safe this-command 'icicle-apropos-cycling-command)
             (setq icicle-last-completion-command
                   (if (eq icicle-last-completion-command 'icicle-prefix-complete-no-display)
                       'icicle-apropos-complete-no-display
                     'icicle-apropos-complete)))))

    ;; Recompute and redisplay completion candidates.  Reset candidate number.
    (setq icicle-completion-candidates
          (condition-case nil
              (funcall candidates-fn icicle-current-input)
            (error icicle-completion-candidates))) ; No change if completion error.
    (when (get-buffer-window "*Completions*" 0) ; Update `*Completions*' display or remove it.
      (if icicle-completion-candidates
          (icicle-display-candidates-in-Completions (not (wholenump nth)))
        (save-selected-window (icicle-remove-Completions-window))))))

(defun icicle-save-raw-input ()
  "Save `icicle-current-raw-input' as the latest previous input.
It is saved to `icicle-previous-raw-file-name-inputs', if completing a
file name, or `icicle-previous-raw-non-file-name-inputs', otherwise."
  (let* ((prev-inputs-var  (if (icicle-file-name-input-p)
                               'icicle-previous-raw-file-name-inputs
                             'icicle-previous-raw-non-file-name-inputs))
         (prev-inputs      (symbol-value prev-inputs-var)))
    (unless (string= "" icicle-current-raw-input)
      (set prev-inputs-var (icicle-put-at-head prev-inputs-var icicle-current-raw-input)))
    (when (> (length prev-inputs) icicle-completion-history-max-length)
      (setcdr (nthcdr (1- icicle-completion-history-max-length) prev-inputs) ()))))

(defun icicle-save-or-restore-input ()
  "Save the current minibuffer input, or restore the last input.
If there is a previous input and we are cycling, then restore the last
 input.  (Cycled completions don't count as input.)
Otherwise, save the current input for use by `C-l', and then compute
 the expanded common match.

There are several particular cases that modulate the behavior - see
the code."
  (cond
    ;; Restore last input, if there is some to restore and we are cycling.
    ((and icicle-last-input  icicle-cycling-p  icicle-last-completion-candidate)
     (setq icicle-current-input  icicle-last-input)) ; Return `icicle-current-input'.
    (t
     (cond
       ;; Save the current input for `C-l', then update it to the expanded common match.
       ;; Do *NOT* do this if:
       ;;   or there is no common match string
       ;;   or the last command was a cycling command
       ;;   or the input and the completion mode have not changed
       ;;      (so saved regexp will not be overwritten).
       ((not (or

;;;       ;;      the user does not want automatic expansion to the common match for this completion mode
;;; $$$$$$        (and (eq icicle-current-completion-mode 'apropos)
;;;                    (not (eq icicle-expand-input-to-common-match 4)))
;;;               (and (eq icicle-current-completion-mode 'prefix)
;;;                    (not (memq icicle-expand-input-to-common-match '(3 4))))
              (not icicle-common-match-string)
              (and (symbolp last-command)
                   (get last-command 'icicle-cycling-command)
                   (not (get last-command 'icicle-completing-command))) ; Not `TAB' or `S-TAB'.
              (and (equal icicle-last-input icicle-current-input)
                   (eq icicle-current-completion-mode
                       (if (icicle-get-safe icicle-last-completion-command
                                            'icicle-prefix-completing-command)
                           'prefix
                         'apropos)))))

        ;; Expand current input to expanded common match, after saving it for `C-l'.
        (let ((common  (if (icicle-file-name-input-p)
                           ;; $$$$$$ (and (icicle-file-name-input-p) ; (2012-06-28)
                           ;;             insert-default-directory)
                           (if (string= "" icicle-common-match-string)
                               (or (icicle-file-name-directory icicle-current-input)  "")
                             (directory-file-name (icicle-abbreviate-or-expand-file-name
                                                   icicle-common-match-string
                                                   (icicle-file-name-directory icicle-current-input)
                                                   (not insert-default-directory))))
                         icicle-common-match-string)))

          ;; Save current input for `C-l', then save common match as current input.
          ;; Do NOT do anything if we're ignoring letter case and that is the only difference
          ;; between the common match and the input (e.g. MS Windows file names).
          (unless (and case-fold-search
                       (string= (icicle-upcase icicle-current-input) (icicle-upcase common))
                       (not (string= icicle-current-input common)))

            ;; Save input for `C-l' if this is not `C-l' or `C-L'.
            ;; Save it also if this is the first cycling command, or the first after completion.
            (unless (or (memq this-command '(icicle-retrieve-previous-input
                                             icicle-retrieve-next-input))
                        (and icicle-cycling-p
                             (or icicle-candidate-nb ; Not the first cycling command.
                                 (icicle-get-safe last-command 'icicle-completing-command))))
              (setq icicle-current-raw-input  icicle-current-input)
              ;; Save it for `C-l', unless it is "".  Drop old entries when too big.
              (icicle-save-raw-input))

            ;; Save expanded common match as current input, unless input is a directory.
            ;; Use `icicle-looks-like-dir-name-p'.
            ;; `file-directory-p' fails to consider "~/foo//usr/" a directory.
            ;; $$$$$$ We could use the `icicle-looks-like-dir-name-p' code with `icicle-file-name-directory'
            ;;        instead of `icicle-file-name-directory-w-default', if that presents a problem.
            (unless (and (icicle-file-name-input-p)  (icicle-looks-like-dir-name-p icicle-current-input))
              (setq icicle-current-input  common)))))

       ;; Save input for `C-l'.
       ;; Do NOT do this if:
       ;;      this command is `C-l' or `C-L'
       ;;   or we are cycling or the last command was a cycling command
       ;;   or this command is the same as last command.
       ((not (or (memq this-command '(icicle-retrieve-previous-input icicle-retrieve-next-input))
                 icicle-cycling-p
                 (and (symbolp last-command)
                      (get last-command 'icicle-cycling-command)
                      (not (get this-command 'icicle-completing-command)))
                 ;;$$$ (icicle-get-safe last-command 'icicle-completing-command)
                 (eq last-command this-command)))
        (setq icicle-current-raw-input  icicle-current-input)
        ;; Save it for `C-l', unless it is "".  Drop old entries when too big.
        (icicle-save-raw-input))
       ;; Forget last raw input, so it is not highlighted in `*Completions*'.
       ;; Do NOT do this if we are cycling.
       ((not icicle-cycling-p)
        (setq icicle-current-raw-input  "")))))
  (setq icicle-last-input  icicle-current-input)) ; Return `icicle-current-input'.

(defun icicle-put-at-head (list-var element)
  "Put ELEMENT at the front of the value of LIST-VAR.
If ELEMENT is already a member of the list, then it is moved to the
front.  Otherwise, it is added to the front.  Membership is tested
with `equal'.  The return value is the new value of LIST-VAR.
This is a destructive operation: the list structure is changed."
  (let* ((lis  (symbol-value list-var))
         (tl   (member element lis)))
    (cond ((null lis) (set list-var (list element)))
          ;;;((eq tl lis) (set list-var (cdr lis)))
          ((not (eq tl lis))
           (when tl (setcdr (nthcdr (1- (- (length lis) (length tl))) lis) (cdr tl)))
           (set list-var (cons element lis)))))
  (symbol-value list-var))

(defun icicle-remove-dots (filename)
  "Strip leading string through last ../ or ./ from FILENAME."
  (let ((newname  filename))
    (save-match-data
      (while (or (string-match "\\.\\./" newname)
                 (string-match "\\./" newname)
                 ;; Emacs 21+ `file-relative-name' returns ".." and "." (no slash) for "" first arg
                 (string-match "^\\.\\.$" newname)
                 (string-match "^\\.$" newname))
        (setq newname  (substring newname (match-end 0)))))
    newname))

(defun icicle-increment-cand-nb+signal-end (incr max)
  "Increment candidate number by INCR modulo MAX, and signal end of cycle."
  (setq icicle-candidate-nb  (if icicle-candidate-nb
                                 (+ incr icicle-candidate-nb)
                               (if (natnump incr) 0 (1- max))))
  (let ((wrapped  (mod icicle-candidate-nb max)))
    (when (and (/= wrapped icicle-candidate-nb)  (eq last-command this-command))
      (let ((visible-bell  t))  (ding)))
    (setq icicle-candidate-nb  wrapped)))

(defun icicle-place-cursor (input &optional dont-activate-p)
  "Position point and mark with respect to the minibuffer candidate.
Positions are `icicle-point-position-in-candidate' and
`icicle-mark-position-in-candidate', respectively.
INPUT is the current user input, that is, the completion root.
Optional argument DONT-ACTIVATE-P means do not activate the mark."
  (let ((case-fold-search
         ;; Do not bother with buffer completion and `read-buffer-completion-ignore-case'.
         (if (and (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
                  (boundp 'read-file-name-completion-ignore-case))
             read-file-name-completion-ignore-case
           completion-ignore-case))
        input-start-position)
    (goto-char (icicle-minibuffer-prompt-end))
    (setq input-start-position  (point))
    (when (and (icicle-file-name-input-p)  insert-default-directory)
      (search-forward (icicle-file-name-directory-w-default input) nil t)
      (setq input-start-position  (point))) ; Skip directory.
    ;; Locate completion root within current completion candidate.
    (when (or (memq icicle-point-position-in-candidate '(root-start root-end))
              (memq icicle-mark-position-in-candidate  '(root-start root-end)))
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (point-max)) ; Search within the completion candidate.
          (condition-case lossage
              (re-search-forward (if icicle-regexp-quote-flag
                                     (regexp-quote (icicle-minibuf-input-sans-dir input))
                                   (icicle-minibuf-input-sans-dir input))
                                 nil t)
            (invalid-regexp  (when (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
                                                 (cadr lossage))
                               (goto-char (point-max))))))))
    ;; Position point.
    (case icicle-point-position-in-candidate
      (input-start (goto-char input-start-position))
      (input-end (goto-char (point-max)))
      (root-start (goto-char (max input-start-position (match-beginning 0))))
      (root-end (goto-char (max input-start-position (match-end 0)))))
    ;; Position mark.
    (unless (eq icicle-point-position-in-candidate icicle-mark-position-in-candidate)
      (push-mark (case icicle-mark-position-in-candidate
                   (input-start input-start-position)
                   (input-end (point-max))
                   (root-start (max input-start-position (match-beginning 0)))
                   (root-end (max input-start-position (match-end 0))))
                 'nomsg
                 (not dont-activate-p)))))

(defun icicle-highlight-candidate-in-Completions ()
  "Highlight the current candidate in `*Completions*'."
  (let ((compl-win  (get-buffer-window "*Completions*" 0))
        curr-cand-pos)
    (when compl-win
      (set-window-dedicated-p compl-win t)
      (save-window-excursion (select-window compl-win)
                             (goto-char (icicle-start-of-candidates-in-Completions))
                             (icicle-move-to-next-completion icicle-candidate-nb t)
                             (set-buffer-modified-p nil)
                             (setq curr-cand-pos  (point)))
      (set-window-point compl-win curr-cand-pos))))

(defun icicle-place-overlay (start end overlay face priority buffer &rest properties)
  "Put OVERLAY with FACE and PRIORITY between START and END in BUFFER.
OVERLAY is a symbol whose value is the overlay.  If nil, the overlay
  is created.  If non-nil, it is simply moved.
PROPERTIES are additional overlay properties to add: pairs of a
property and a value."
  (if (symbol-value overlay)            ; Overlay exists, just move it.
      (move-overlay (symbol-value overlay) start end buffer)
    (set overlay (make-overlay start end buffer))
    (overlay-put (symbol-value overlay) 'face face)
    (overlay-put (symbol-value overlay) 'priority priority)))

(defun icicle-strip-ignored-files-and-sort (candidates)
  "Remove file names with ignored extensions, and \".\".  Sort CANDIDATES.
If `icicle-sort-comparer' is nil, then do not sort."
  (when (fboundp 'completion-ignored-build-apply) ; In `completion-ignored-build.el'.
    (let ((completion-ignored-extensions  completion-ignored-extensions))
      (completion-ignored-build-apply)
      (icicle-update-ignored-extensions-regexp)))
  (let* ((pred1           (lambda (cand) (or (save-match-data
                                               (string-match icicle-ignored-extensions-regexp cand))
                                             (string= "./" cand))))
         (pred2           (lambda (cand) (string= "./" cand)))
         (new-candidates  (icicle-remove-if (if icicle-ignored-extensions-regexp pred1 pred2)
                                            candidates)))
    ;; If the only candidates have ignored extensions, then use them.
    (unless new-candidates (setq new-candidates  (icicle-remove-if pred2 candidates)))
    (icicle-maybe-sort-maybe-truncate new-candidates)))

(defun icicle-transform-candidates (candidates)
  "Apply `icicle-transform-function' to CANDIDATES.
If `icicle-transform-function' is nil, return CANDIDATES.

This transformation is applied before completion candidates are made
available to the user, in particular, before they are displayed in
`*Completions*'.

This transformation has nothing to do with that performed by
`icicle-transform-multi-completion'."
  (if icicle-transform-function (funcall icicle-transform-function candidates) candidates))

(defun icicle-transform-multi-completion (candidate)
  "Transform display CANDIDATE according to `icicle-list-use-nth-parts'.
If CANDIDATE is not a multi-completion, return CANDIDATE unchanged.
Return the possibly transformed candidate."
  ;; Note: For Emacs 20, `split-string' trims off an empty string before or after the join string.
  ;;       That means that for Emacs 20, if the input starts with the join string then the first
  ;;       part of the input is ignored.  So for example, with `(1)' as the join part, you get the
  ;;       second part, not the first part, if the first part is empty.  This is not correct behavior.
  (if (and icicle-list-use-nth-parts  (not (equal "" candidate)))
      (let ((parts  (split-string candidate icicle-list-join-string)))  (icicle-join-nth-parts parts))
    candidate))

(defun icicle-join-nth-parts (parts)
  "Join the elements in PARTS using `icicle-list-nth-parts-join-string'.
The parts to join are specified by `icicle-list-use-nth-parts'."
  (let* ((maxpart  (length parts))
         (indexes  icicle-list-use-nth-parts)
         (cand     "")
         (firstp   t)
         partnum)
    (if (null parts)
        ""                              ; Nothing to join.
      (while indexes
        (setq partnum  (car indexes))
        (unless firstp (setq cand  (concat cand icicle-list-nth-parts-join-string)))
        (setq firstp  nil)
        (unless (> partnum maxpart) (setq cand  (concat cand (nth (1- partnum) parts))))
        (setq indexes  (cdr indexes)))
      cand)))

(defun icicle-display-cand-from-full-cand (cand)
  "Return the display candidate corresponding to full candidate CAND."
  (let ((parts  (car cand)))
    (if (atom parts)
        parts                           ; Not a multi-completion.
      (if icicle-list-use-nth-parts
          (icicle-join-nth-parts parts) ; Join mult-completion parts per `icicle-list-use-nth-parts'.
        ;; Multi-completion, but no joining specified.  Reconstitute the display candidate.
        ;; $$$$$$ (mapconcat #'identity parts icicle-list-join-string)
        (mapconcat #'identity parts icicle-list-join-string)))))

(defun icicle-file-name-directory (filename)
  "Like `file-name-directory', but backslash is not a directory separator.
Do not treat backslash as a directory separator, even on MS Windows.
Escape any backslashes, then call `file-name-directory' and return
what it returns."
  (let ((max-char-in-name  0)
        (repl-char         0))          ; NULL char: ?\^@
    ;; Set REPL-CHAR to 1+ the highest char code used in FILENAME, or NULL if that is not possible.
    (dolist (char  (append filename ())) ; `string-to-list'
      (when (> char max-char-in-name) (setq max-char-in-name  char)))
    ;; Make sure we do not go past the max allowable char for Emacs.  If so, just use NULL char.
    ;; Emacs 20-22 has no `max-char' function, so just try adding 1 and see if result is valid.
    (when (or (and (fboundp 'max-char)  ; Emacs 23+
                   (< (1+ max-char-in-name) (max-char)))
              (char-valid-p (1+ max-char-in-name))) ; Emacs 20-22.
      (setq repl-char  (1+ max-char-in-name)))
    (let* ((escaped-file  (subst-char-in-string ?\\ repl-char ; Replace \ by REPL-CHAR
                                                filename 'IN-PLACE))
           (dir           (file-name-directory escaped-file)))
      (setq filename  (and dir  (subst-char-in-string repl-char ?\\ ; Replace REPL-CHAR by \
                                                      dir 'IN-PLACE)))))
  filename)

(defun icicle-file-name-directory-w-default (file)
  "`icicle-file-name-directory', or `default-directory' if that is nil."
  (or (icicle-file-name-directory file)  default-directory))

(defun icicle-file-name-nondirectory (filename)
  "Like `file-name-nondirectory', but does not treat backslash specially.
That is, backslash is never treated as a directory separator."
  (let ((max-char-in-name  0)
        (repl-char         0))          ; NULL char: ?\^@
    ;; Set REPL-CHAR to 1+ the highest char code used in FILENAME, or NULL if that is not possible.
    (dolist (char  (append filename ())) ; `string-to-list'
      (when (> char max-char-in-name) (setq max-char-in-name  char)))
    ;; Make sure we do not go past the max allowable char for Emacs.  If so, just use NULL char.
    ;; Emacs 20-22 has no `max-char' function, so just try adding 1 and see if result is valid.
    (when (or (and (fboundp 'max-char)  ; Emacs 23+
                   (< (1+ max-char-in-name) (max-char)))
              (char-valid-p (1+ max-char-in-name))) ; Emacs 20-22.
      (setq repl-char  (1+ max-char-in-name)))
    (setq filename  (subst-char-in-string repl-char ?\\ ; Replace REPL-CHAR by \
                                          (file-name-nondirectory
                                           (subst-char-in-string ?\\ repl-char ; Replace \ by REPL-CHAR
                                                                 filename 'IN-PLACE))
                                          'IN-PLACE)))
  filename)

;; $$$$$
;; (defun icicle-file-name-input-p ()
;;   "Return non-nil if expected input is a file name.
;; This is used, instead of variable `minibuffer-completing-file-name',
;; because we sometimes complete against an explicit alist of file names,
;; even in the overall context of file-name input.  In that case, we do
;; not want to use file-name completion.  An example of this is
;; completing against a history list of file names, using
;; `icicle-history'."
;;   ;; Note that some Emacs 20 code uses this as the equivalent of
;;   ;; `minibuffer-completing-file-name':
;;   ;; (memq minibuffer-completion-table '(read-file-name-internal read-directory-name-internal))
;;   (and (symbolp minibuffer-completion-table)  (stringp minibuffer-completion-predicate)))

(defun icicle-file-name-input-p ()
  "Return non-nil if reading a file name using `read-file-name'.
This means that completion candidates are relative file names.
If instead you want to test whether input is a file name, absolute or
relative, use this test:
 (or (icicle-file-name-input-p)  icicle-abs-file-candidates)"
  minibuffer-completing-file-name)

(defun icicle-minibuf-input ()
  "Return the user minibuffer input as a string, without text-properties."
  (save-selected-window (select-window (minibuffer-window)) (icicle-input-from-minibuffer)))

;;$$$ Do we need to double all $'s in output from `icicle-subst-envvar-in-file-name',
;;      before calling `substitute-in-file-name'?
(defun icicle-input-from-minibuffer (&optional leave-envvars-p)
  "Return the minibuffer input as a string, without text-properties.
Unless optional arg LEAVE-ENVVARS-P is non-nil, substitute any
environment vars by their values.
The current buffer must be a minibuffer."
  (let ((input  (if (fboundp 'minibuffer-contents)
                    (minibuffer-contents) ; e.g. Emacs 22
                  (buffer-substring (point-min) (point-max))))) ; e.g. Emacs 20
    ;; $$$$$$$$ (if (fboundp 'minibuffer-contents-no-properties)
    ;;              (minibuffer-contents-no-properties) ; e.g. Emacs 22
    ;;            (buffer-substring-no-properties (point-min) (point-max))))) ; e.g. Emacs 20
    (when (and (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
               (not (string= "" input)) ; Do nothing if user deleted everything in minibuffer.
               (not leave-envvars-p))
      (let ((last-char       "")
            (max-input-char  0)
            (repl-char       0))        ; NULL char: ?\^@
        (when (eq ?\$ (aref input (1- (length input))))
          (setq last-char  "$"
                input      (substring input 0 (1- (length input)))))
        ;; Set REPL-CHAR to 1+ the highest char code used in INPUT, or NULL if that is not possible.
        (dolist (char  (append input ())) ; `string-to-list'
          (when (> char max-input-char) (setq max-input-char  char)))
        ;; Make sure we do not go past the max allowable char for Emacs.  If so, just use NULL char.
        ;; Emacs 20-22 has no `max-char' function, so just try adding 1 and see if result is valid.
        (when (or (and (fboundp 'max-char) ; Emacs 23+
                       (< (1+ max-input-char) (max-char)))
                  (char-valid-p (1+ max-input-char))) ; Emacs 20-22.
          (setq repl-char  (1+ max-input-char)))
        (setq input
              (save-match-data          ; Need `save-match-data' around `icicle-subst-envvar-in-file-name'.
                (concat (subst-char-in-string
                         repl-char ?\\  ; Replace REPL-CHAR by \
                         (condition-case nil
                             (substitute-in-file-name
                              (icicle-subst-envvar-in-file-name
                               (subst-char-in-string ?\\ repl-char ; Replace \ by REPL-CHAR
                                                     input 'IN-PLACE)))
                           (error input))
                         'IN-PLACE)
                        last-char)))))
    input))

(defun icicle-minibuf-input-sans-dir (&optional input)
  "Return the user input, except for a directory portion if reading a file."
  (unless input (setq input  (icicle-minibuf-input)))
  (if (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
      (icicle-file-name-nondirectory input)
    input))

(defun icicle-subst-envvar-in-file-name (input)
  "Substitute any environment vars in INPUT by their values.
Unlike `substitute-in-file-name', this does not make any other
changes, such as switching `\\' to `/' on MS Windows."
  (let ((pat1  "[^$]\\([$]{\\([^$}]+\\)}\\)") ; e.g. aaa${HOME}
        (pat2  "^[$]{\\([^$}]+\\)}")          ; e.g. ${HOME}
        (pat3  "[^$]\\([$]\\([^$]+\\)\\)")    ; e.g. aaa$HOME
        (pat4  "^[$]\\([^$]+\\)"))            ; e.g. $HOME
    (cond ((string-match pat1 input)
           (replace-regexp-in-string pat1 (or (getenv (match-string 2 input))
                                              (concat "$" (match-string 2 input)))
                                     input t t 1))
          ((string-match pat2 input)
           (replace-regexp-in-string pat2 (or (getenv (match-string 1 input))
                                              (concat "$" (match-string 1 input)))
                                     input t t))
          ((string-match pat3 input)
           (replace-regexp-in-string pat3 (or (getenv (match-string 2 input))
                                              (concat "$" (match-string 2 input)))
                                     input t t 1))
          ((string-match pat4 input)
           (replace-regexp-in-string pat4 (or (getenv (match-string 1 input))
                                              (concat "$" (match-string 1 input)))
                                     input t t))
          (t input))))

;; Provide for Emacs 20.
;;
(unless (fboundp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (regexp rep string &optional
                                   fixedcase literal subexp start)
    "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\\\(foo\\\\).*\\\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

    ;; To avoid excessive consing from multiple matches in long strings,
    ;; don't just call `replace-match' continually.  Walk down the
    ;; string looking for matches of REGEXP and building up a (reversed)
    ;; list MATCHES.  This comprises segments of STRING which weren't
    ;; matched interspersed with replacements for segments that were.
    ;; [For a `large' number of replacements it's more efficient to
    ;; operate in a temporary buffer; we can't tell from the function's
    ;; args whether to choose the buffer-based implementation, though it
    ;; might be reasonable to do so for long enough STRING.]
    (let ((l      (length string))
          (start  (or start  0))
          matches str mb me)
      (save-match-data
        (while (and (< start l)  (string-match regexp string start))
          (setq mb  (match-beginning 0)
                me  (match-end 0))
          ;; If we matched the empty string, make sure we advance by one char
          (when (= me mb) (setq me  (min l (1+ mb))))
          ;; Generate a replacement for the matched substring.
          ;; Operate only on the substring to minimize string consing.
          ;; Set up match data for the substring for replacement;
          ;; presumably this is likely to be faster than munging the
          ;; match data directly in Lisp.
          (string-match regexp (setq str  (substring string mb me)))
          (setq matches  (cons (replace-match (if (stringp rep)
                                                  rep
                                                (funcall rep (match-string 0 str)))
                                              fixedcase literal str subexp)
                               (cons (substring string start mb) matches))) ; unmatched prefix
          (setq start  me))
        ;; Reconstruct a string from the pieces.
        (setq matches  (cons (substring string start l) matches)) ; leftover
        (apply #'concat (nreverse matches))))))

(defun icicle-filter-wo-input (candidate)
  "Filter completion (string) CANDIDATE using regexps and predicate.
If CANDIDATE passes the filtering, return CANDIDATE.  Else return nil.

In addition to filtering out empty-string candidates, these variables
are used for the filtering:
  `icicle-must-match-regexp'
  `icicle-must-not-match-regexp'
  `icicle-must-pass-predicate'

This filtering is in addition to and prior to matching user input.
Users do not see any candidates filtered out here.
This filtering does not affect proxy candidates or extra candidates.

See also variable `icicle-must-pass-after-match-predicate', which is
similar to `icicle-must-pass-predicate' but is used after filtering
using the user input."
  (and (not (string= "" candidate))     ; Filter out empty strings.
       (or (not icicle-must-match-regexp)  (icicle-string-match-p icicle-must-match-regexp candidate))
       (or (not icicle-must-not-match-regexp)
           (not (icicle-string-match-p icicle-must-not-match-regexp candidate)))
       (or (not icicle-must-pass-predicate)  (funcall icicle-must-pass-predicate candidate))
       candidate))

(defun icicle-complete-again-update (&optional no-display)
  "Complete again and update completions list.
Update display too, if already shown and NO-DISPLAY is nil."
  (setq icicle-completion-candidates
        (condition-case nil
            (funcall (case icicle-last-completion-command
                       ((icicle-prefix-complete icicle-prefix-complete-no-display
                                                icicle-prefix-word-complete)
                        (if (icicle-file-name-input-p)
                            #'icicle-file-name-prefix-candidates
                          #'icicle-prefix-candidates))
                       (t
                        (if (icicle-file-name-input-p)
                            #'icicle-file-name-apropos-candidates
                          #'icicle-apropos-candidates)))
                     icicle-current-input)
          (error icicle-completion-candidates))) ; No change if completion error.
  (when (and (get-buffer-window "*Completions*" 0) (not no-display))
    (icicle-display-candidates-in-Completions)))

(defun icicle-msg-maybe-in-minibuffer (format-string &rest args)
  "Display FORMAT-STRING as a message.
If called with the minibuffer inactive, use `message'.
Otherwise:
 If `icicle-minibuffer-message-ok-p', then use `minibuffer-message'.
 Else do nothing (no message display)."
  (if (active-minibuffer-window)
      (when icicle-minibuffer-message-ok-p
        (save-selected-window
          (select-window (minibuffer-window))
          (minibuffer-message (apply #'format (concat "  [" format-string "]") args))))
    (apply #'message format-string args)))

(defun icicle-delete-count (elt elts count)
  "Delete by side effect the first COUNT occurrences of ELT from list ELTS.
This is like `delete', but it deletes only the first COUNT `equal'
occurrences."
  (while (and elts  (equal elt (car elts))  (>= (setq count  (1- count)) 0))
    (setq elts  (cdr elts)))
  (let ((tail  elts)
        (nn    count))
    (if (cdr tail)
        (while (and (cdr tail)  (> nn 0))
          (when (equal elt (cadr tail))
            (setq nn  (1- nn))
            (setcdr tail (cddr tail)))
          (setq tail  (cdr tail)))
      (when (and (equal elt (car tail))  (> count 0))
        (setq tail  (cdr tail)))))       ; Remove matching singleton.
  elts)

(defun icicle-unlist (object)
  "If OBJECT is a cons, return its car; else return OBJECT."
  (if (consp object) (car object) object))

(when (fboundp 'window-use-time)        ; Emacs 24+

  ;; Emacs 24 `time-less-p' does not work for integer time values, so need to convert to list time values.

  (defun icicle-lru-window-for-buffer (buffer &optional minibuf all-frames)
    "Return the least recently used window for BUFFER.
Optional args MINIBUF and ALL-FRAMES are as for `get-buffer-window-list'."
    (let* ((wins      (get-buffer-window-list buffer minibuf all-frames))
           (lru-win   (car wins))
           (lru-time  (window-use-time lru-win))
           wtime)
      (unless (listp lru-time) (setq lru-time  (seconds-to-time lru-time)))
      (dolist (win  (cdr wins))
        (setq wtime  (window-use-time win))
        (unless (listp wtime) (setq wtime  (seconds-to-time wtime)))
        (when (time-less-p wtime lru-time)
          (setq lru-time  wtime
                lru-win   win)))
      lru-win))

  (defun icicle-mru-window-for-buffer (buffer &optional minibuf all-frames)
    "Return the most recently used window for BUFFER.
Optional args MINIBUF and ALL-FRAMES are as for `get-buffer-window-list'."
    (let* ((wins      (get-buffer-window-list buffer minibuf all-frames))
           (mru-win   (car wins))
           (mru-time  (window-use-time mru-win))
           wtime)
      (unless (listp mru-time) (setq mru-time  (seconds-to-time mru-time)))
      (dolist (win  (cdr wins))
        (setq wtime  (window-use-time win))
        (unless (listp wtime) (setq wtime  (seconds-to-time wtime)))
        (unless (time-less-p wtime mru-time)
          (setq mru-time  wtime
                mru-win   win)))
      mru-win))

  )

(defun icicle-position (item list)
  "Zero-based position of first occurrence of ITEM in LIST, else nil."
  (let ((index  0))
    (catch 'icicle-position
      (dolist (xx list)
        (when (equal xx item) (throw 'icicle-position index))
        (setq index  (1+ index)))
      nil)))

(defun icicle-remove-if (pred xs)
  "A copy of list XS with no elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x xs) (unless (funcall pred x) (push x result)))
    (nreverse result)))

(defun icicle-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x xs) (when (funcall pred x) (push x result)))
    (nreverse result)))

(defun icicle-frames-on (buffer &optional frame) ; From `frames-on' in `frame-fns.el'.
  "List of all live frames showing BUFFER (a buffer or its name).
The optional FRAME argument is as for function `get-buffer-window'."
  (filtered-frame-list (function (lambda (fr) (get-buffer-window buffer fr)))))

(defun icicle-candidate-set-1 (set-fn msg)
  "Helper function for defining Icicle set commands.
SET-FN is the function to apply to the current and saved candidates.
MESSAGE is the confirmation message to display in the minibuffer."
  (setq icicle-completion-candidates
        (funcall set-fn icicle-completion-candidates icicle-saved-completion-candidates))
  (if (null icicle-completion-candidates)
      (save-selected-window (select-window (minibuffer-window)) (minibuffer-message "  [EMPTY SET]"))
    (icicle-maybe-sort-and-strip-candidates)
    (icicle-scroll-or-update-Completions msg)))

(defun icicle-maybe-sort-and-strip-candidates ()
  "Sort `icicle-completion-candidates'.  Strip ignored file names too."
  (if (or (icicle-file-name-input-p)  icicle-abs-file-candidates) ; File names: relative or absolute.
      (setq icicle-completion-candidates
            (icicle-strip-ignored-files-and-sort icicle-completion-candidates))
    (setq icicle-completion-candidates  (icicle-maybe-sort-maybe-truncate icicle-completion-candidates))))

(defun icicle-scroll-or-update-Completions (msg)
  "Scroll `*Completions*' if this command was repeated; else update it."
  (if (get-buffer-window "*Completions*" 0)
      (if (eq last-command this-command)
          ;; User repeated the command.  Scroll window around.
          (icicle-scroll-Completions-forward)
        ;; User did something else (e.g. changed input).  Update the display.
        (icicle-display-candidates-in-Completions)
        (save-selected-window (select-window (minibuffer-window)) (minibuffer-message msg)))
    ;; No window yet.  Show window.
    (icicle-display-candidates-in-Completions)
    (save-selected-window (select-window (minibuffer-window)) (minibuffer-message msg))))

;; $$ No longer used.
(defun icicle-display-Completions ()
  "Display `*Completions*' buffer."
  (let ((completions  (icicle-all-completions "" minibuffer-completion-table
                                              minibuffer-completion-predicate
                                              (and icicle-buffer-name-input-p ; Used only by Emacs < 23.2.
                                                   icicle-buffer-ignore-space-prefix-flag))))
    (when (> (length icicle-completion-candidates) icicle-incremental-completion-threshold)
      (message "Displaying completion candidates..."))
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list (icicle-maybe-sort-maybe-truncate completions)))))

;; This works hand in hand with `icicle-doremi-increment-max-candidates+'.  Update both together.
(defun icicle-maybe-sort-maybe-truncate (cands)
  "Return a copy of candidate list CANDS, maybe sorted, maybe truncated.
Sort according to `icicle-sort-comparer'.
Truncate according to `icicle-max-candidates'."
  (let ((new-cands  cands))
    (setq new-cands  (icicle-reversible-sort new-cands))
    (when icicle-max-candidates
      (let ((lighter  (cadr (assoc 'icicle-mode minor-mode-alist)))
            (regexp   (concat (regexp-quote icicle-lighter-truncation) "$")))
        (cond ((and new-cands
                    (integerp icicle-max-candidates) ; Not `RESET'.
                    (< icicle-max-candidates ; Save total number before truncation
                       (setq icicle-nb-candidates-before-truncation  (length new-cands))))
               (unless (string-match regexp lighter)
                 (icicle-clear-lighter 'not-truncated)
                 (add-to-list 'minor-mode-alist `(icicle-mode ,(concat lighter icicle-lighter-truncation)))))
              (new-cands
               ;; Save total number before truncation in `icicle-nb-candidates-before-truncation'.
               (setq icicle-nb-candidates-before-truncation  (length new-cands))
               (when (string-match regexp lighter)
                 (icicle-clear-lighter 'truncated)
                 (add-to-list
                  'minor-mode-alist
                  `(icicle-mode
                    ,(substring lighter 0 (- (length lighter) (length icicle-lighter-truncation))))))))
        (if (eq 'RESET icicle-max-candidates) ; `RESET' is from `icicle-doremi-increment-max-candidates+'.
            (setq icicle-max-candidates  nil)
          (setq new-cands  (icicle-take icicle-max-candidates new-cands)))))
    new-cands))

(defun icicle-take (num xs)
  "Return a new list with the first NUM elements of list XS.
No error handling.  NUM must be in the range 0 to (length XS)."
  ;; A recursive version would be just this:
  ;; (and xs  (not (zerop num))  (cons (car xs) (icicle-take (1- num) (cdr xs)))))
  (let (newlist)
    (while (and xs  (> num 0))
      (setq newlist  (cons (car xs) newlist)
            num      (1- num)
            xs       (cdr xs)))
    (nreverse newlist)))

;; From `cl-seq.el', function `union', without keyword treatment.
(defun icicle-set-union (list1 list2)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or
LIST2.  This is a non-destructive function; it copies the data if
necessary."
  (cond ((null list1)         list2)
        ((null list2)         list1)
        ((equal list1 list2)  list1)
        (t
         (unless (>= (length list1) (length list2))
           (setq list1  (prog1 list2 (setq list2  list1)))) ; Swap them.
         (while list2
           (unless (member (car list2) list1)  (setq list1  (cons (car list2) list1)))
           (setq list2  (cdr list2)))
         list1)))

;; From `cl-seq.el', function `intersection', without keyword treatment.
(defun icicle-set-intersection (list1 list2)
  "Set intersection of lists LIST1 and LIST2.
This is a non-destructive operation: it copies the data if necessary."
  (and list1  list2  (if (equal list1 list2)
                         list1
                       (let ((result  ()))
                         (unless (>= (length list1) (length list2))
                           (setq list1  (prog1 list2 (setq list2  list1)))) ; Swap them.
                         (while list2
                           (when (member (car list2) list1)  (setq result  (cons (car list2) result)))
                           (setq list2  (cdr list2)))
                         result))))

;; Like `cl-seq.el', function `set-difference', but without keyword treatment.  No TEST arg, just KEY.
;; Same as `frame-cmds-set-difference' in `frame-cmds.el'.
(defun icicle-set-difference (list1 list2 &optional key)
  "Combine LIST1 and LIST2 using a set-difference operation.
Optional arg KEY is a function used to extract the part of each list
item to compare.  Comparison is done using `equal'.

The result list contains all items that appear in LIST1 but not LIST2.
This is non-destructive; it makes a copy of the data if necessary, to
avoid corrupting the original LIST1 and LIST2."
  (if (or (null list1)  (null list2))
      list1
    (let ((keyed-list2  (and key  (mapcar key list2)))
          (result       ()))
      (while list1
        (unless (if key
                    (member (funcall key (car list1)) keyed-list2)
                  (member (car list1) list2))
          (setq result  (cons (car list1) result)))
        (setq list1  (cdr list1)))
      result)))

;; Same as `isearchp-some' in `isearch-prop.el'.
(defun icicle-some (lst arg2 predicate)
  "Apply binary PREDICATE successively to an item of list LST and ARG2.
Return the first non-nil value returned by PREDICATE, or nil if none.
PREDICATE must be a function with two required arguments."
  (let ((result  nil))
    (catch 'icicle-some
      (dolist (arg1  lst)
        (when (setq result  (funcall predicate arg1 arg2))  (throw 'icicle-some result))))
    result))

;; No longer used.
(defun icicle-flat-list (val1 val2)
  "Return a flat list with all values in VAL1 and VAL2."
  (let ((result  nil))
    (unless (listp val1) (setq val1  (list val1)))
    (unless (listp val2) (setq val2  (list val2)))
    (while val1 (add-to-list 'result (pop val1)))
    (while val2 (add-to-list 'result (pop val2)))
    result))

(defun icicle-get-candidates-from-saved-set (set-name &optional dont-expand-filesets-p)
  "Return the saved set of completion candidates named SET-NAME.
SET-NAME can be the name of either an Icicles saved completion set or,
 if `icicle-filesets-as-saved-completion-sets-flag', an Emacs fileset.
If optional arg DONT-EXPAND-FILESETS-P is non-nil, then don't expand
 fileset entries in a saved completion set.  Instead, return them as
string candidates."
  (let ((cache-file  (cdr (assoc set-name icicle-saved-completion-sets)))
        fst)
    (cond ((and (not cache-file)        ; Fileset - get explicit file list.
                icicle-filesets-as-saved-completion-sets-flag
                (featurep 'filesets)
                filesets-data
                (setq fst  (filesets-get-fileset-from-name set-name)))
           (icicle-explicit-saved-completion-candidates (list fst)))
          ((not cache-file) (error "No such saved set: `%s'" set-name))
          ((not (icicle-file-readable-p cache-file)) (error "Cannot read cache file `%s'" cache-file))
          (t                            ; Icicles saved completion set.
           (let ((list-buf    (find-file-noselect cache-file 'nowarn))
                 (cands-read  ())
                 (candidates  ()))
             (message "Retrieving saved candidates from `%s'..." cache-file)
             (unwind-protect
                  (condition-case err
                      (when (listp (setq cands-read  (read list-buf)))
                        (message "Set `%s' read from file `%s'" set-name cache-file))
                    (error (error "Could not read cache file.  %s" (error-message-string err))))
               (icicle-kill-a-buffer list-buf))
             (unless cands-read (error "No completion candidates in file `%s'" cache-file))
             (dolist (cand  (nreverse cands-read)) ; Convert saved to displayable candidates.
               (if (not (icicle-saved-fileset-p cand))
                   (push (icicle-displayable-cand-from-saved-set cand) candidates)
                 (condition-case err
                     (require 'filesets)
                   (error "Set `%s' includes a fileset, but cannot load `fileset.el'" set-name))
                 (filesets-init)
                 (if dont-expand-filesets-p
                     (push cand candidates)
                   (setq candidates
                         (append (mapcar #'icicle-displayable-cand-from-saved-set
                                         (icicle-get-candidates-from-saved-set (cadr cand)))
                                 candidates)))))
             candidates)))))

(defun icicle-explicit-saved-completion-candidates (&optional saved-set)
  "Return the list of files represented by a saved completion set.
Any fileset entries in the saved set are expanded to an explicit list
of file names.
Optional arg SAVED-SET is the Icicles saved completion set to use.
 It can be the set itself or its name.
 If SAVED-SET is nil, use `icicle-saved-completion-candidates'."
  (unless saved-set (setq saved-set  icicle-saved-completion-candidates))
  (when (stringp saved-set)  (setq saved-set  (icicle-get-candidates-from-saved-set saved-set)))
  (let ((files  ())
        (mode   nil))
    (dolist (entry  saved-set)
      (cond ((atom entry) (push entry files))
            ((and (featurep 'filesets)
                  (or (setq mode  (filesets-entry-mode entry)) ; ("my-fs" (:files "a" "b"))
                      (setq entry  (cons "dummy" entry) ; (:files "a" "b")
                            mode   (filesets-entry-mode entry))))
             (message "Gathering file names...")
             (dolist (file  (filesets-get-filelist entry mode)) (push file files)))
            (t (error "Bad `icicle-saved-completion-candidates' entry: `%S'" entry))))
    (nreverse files)))

(defun icicle-saved-fileset-p (entry)
  "Return non-nil if ENTRY is a fileset entry in a saved completion set.
ENTRY is a list whose car is `:fileset' - it is not a fileset name."
  (and (consp entry)  (eq (car entry) ':fileset)))

(defun icicle-displayable-cand-from-saved-set (cand)
  "Return display candidate for saved candidate CAND.
If CAND is an atom, then return it as is."
  (let ((cand-w-mrkrs  (icicle-readable-to-markers cand)))
    (if (atom cand-w-mrkrs)
        cand-w-mrkrs
      (let ((icicle-whole-candidate-as-text-prop-p  t))
        (car (icicle-mctized-full-candidate cand-w-mrkrs))))))

(defun icicle-readable-to-markers (cand)
  "Convert (deserialize) Lisp-readable representation CAND of candidate.
A Lisp-readable candidate uses the following to represent a marker:
   (icicle-file-marker FILE-NAME   MARKER-POSITION)
or (icicle-marker      BUFFER-NAME MARKER-POSITION)"
  (if (and (consp cand)  (consp (cdr cand))  (consp (cddr cand))  (null (cdr (cddr cand)))
           (memq (car cand) '(icicle-file-marker icicle-marker)))
      (let ((file-or-buf  (cadr cand))
            (pos          (car (cddr cand)))
            mrker buf)
        (if (eq (car cand) 'icicle-file-marker)
            (let ((buf  (find-file-noselect file-or-buf)))
              (unless buf (error "Cannot find file `%s'" file-or-buf))
              (setq file-or-buf  buf))
          (unless (get-buffer file-or-buf)
            (icicle-user-error "You must first visit buffer `%s'" file-or-buf)))
        (set-marker (setq mrker  (make-marker)) pos (get-buffer file-or-buf))
        mrker)
    (if (consp cand)
        (cons (icicle-readable-to-markers (car cand)) (icicle-readable-to-markers (cdr cand)))
      cand)))


;; REPLACE ORIGINAL `filesets-get-filelist' in `filesets.el'.
;;  The original is bugged (I filed Emacs bug #976 on 2008-09-13).
;; For `:tree':
;;  * First get the tree from the ENTRY.
;;  * Return all matching files under the directory, including in subdirs up to
;;    `filesets-tree-max-level' for the entry.
;;
(eval-after-load 'filesets
  '(defun filesets-get-filelist (entry &optional mode event)
    "Get all files for fileset ENTRY.
Assume MODE (see `filesets-entry-mode'), if provided."
    (let* ((mode  (or mode  (filesets-entry-mode entry)))
           (fl    (case mode
                    ((:files)   (filesets-entry-get-files entry))
                    ((:file)    (list (filesets-entry-get-file entry)))
                    ((:ingroup) (let ((entry  (expand-file-name
                                               (if (stringp entry)
                                                   entry
                                                 (filesets-entry-get-master entry)))))
                                  (cons entry (filesets-ingroup-cache-get entry))))
                    ((:tree)    (let* ((dirpatt  (filesets-entry-get-tree entry)) ; Added this line.
                                       (dir      (nth 0 dirpatt)) ; Use DIRPATT, not ENTRY.
                                       (patt     (nth 1 dirpatt)) ; Use DIRPATT, not ENTRY.
                                       (depth    (or (filesets-entry-get-tree-max-level entry)
                                                     filesets-tree-max-level)))
                                  (icicle-filesets-files-under 0 depth entry dir patt
                                                               (and icicle-mode
                                                                    (icicle-file-name-input-p)))))
                    ((:pattern) (let ((dirpatt  (filesets-entry-get-pattern entry)))
                                  (if dirpatt
                                      (let ((dir   (filesets-entry-get-pattern--dir dirpatt))
                                            (patt  (filesets-entry-get-pattern--pattern dirpatt)))
                                        ;;(filesets-message 3 "Filesets: scanning %s" dirpatt)
                                        (filesets-directory-files dir patt ':files t))
                                    ;; (message "Filesets: malformed entry: %s" entry)))))))
                                    (filesets-error 'error "Filesets: malformed entry: " entry)))))))
      (filesets-filter-list fl (lambda (file) (not (filesets-filetype-property file event)))))))

(defun icicle-filesets-files-under (level depth entry dir patt &optional relativep)
  "Files under DIR that match PATT.
LEVEL is the current level under DIR.
DEPTH is the maximal tree scanning depth for ENTRY.
ENTRY is a fileset.
DIR is a directory.
PATT is a regexp that included file names must match.
RELATIVEP non-nil means use relative file names."
  (and (or (= depth 0)  (< level depth))
       (let* ((dir         (file-name-as-directory dir))
              (files-here  (filesets-directory-files dir patt nil (not relativep)
                                                     (filesets-entry-get-filter-dirs-flag entry)))
              (subdirs     (filesets-filter-dir-names files-here)) ; Subdirectories at this level.
              (files       (filesets-filter-dir-names ; Remove directory names.
                            (apply #'append
                                   files-here
                                   (mapcar (lambda (subdir) ; Files below this level.
                                             (let* ((subdir       (file-name-as-directory subdir))
                                                    (full-subdir  (concat dir subdir)))
                                               (icicle-filesets-files-under
                                                (+ level 1) depth entry full-subdir patt)))
                                           subdirs))
                            t)))
         files)))

;; Note that initial and trailing spaces will not be noticeable.  That's OK.
(defun icicle-highlight-complete-input ()
  "Highlight minibuffer input, showing that it is a sole completion.
Overlay `icicle-complete-input-overlay' is created with face
`icicle-complete-input', unless it exists."
  (let ((case-fold-search
         ;; Don't bother with buffer completion and `read-buffer-completion-ignore-case'.
         (if (and (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
                  (boundp 'read-file-name-completion-ignore-case))
             read-file-name-completion-ignore-case
           completion-ignore-case))
        input-start-position)
    (save-excursion
      (goto-char (icicle-minibuffer-prompt-end))
      (setq input-start-position  (point))
      (when (and (icicle-file-name-input-p)  insert-default-directory)
        (search-forward (icicle-file-name-directory-w-default
                         (icicle-input-from-minibuffer 'leave-envvars))
                        nil t)
        (setq input-start-position  (point))) ; Skip directory.
      (if icicle-complete-input-overlay ; Don't recreate if exists.
          (move-overlay icicle-complete-input-overlay
                        input-start-position (point-max) (current-buffer))
        (setq icicle-complete-input-overlay  (make-overlay input-start-position (point-max)))
        (overlay-put icicle-complete-input-overlay 'face 'icicle-complete-input)))))

(defun icicle-call-then-update-Completions (fn &rest args)
  "Call FN with ARGS, then update `*Completions*' with input matches."
  (save-match-data
    (apply fn args)
    ;;$$$ (let ((tramp-completion-mode  t))    ; Fool Tramp into thinking it is in completion mode.
    (setq icicle-current-input   (icicle-input-from-minibuffer)
          icicle-input-fail-pos  nil)
    (setq icicle-last-input  nil);; $$$$$$$$ So icicle-save-or-restore-input => recompute candidates.
    (when (overlayp icicle-complete-input-overlay) (delete-overlay icicle-complete-input-overlay))
    (icicle-highlight-initial-whitespace icicle-current-input)
    (if (< (length icicle-current-input) icicle-Completions-display-min-input-chars)
        (save-selected-window (icicle-remove-Completions-window))
      ;; `icicle-highlight-input-noncompletion' return value saves call to `icicle-file-remote-p'.
      (let ((remote-test  (icicle-highlight-input-noncompletion)))
        ;; If ALL of the following are true, then update `*Completions*' (complete again):
        ;;
        ;;   * Incremental completion is turned on.
        ;;
        ;;   * At LEAST ONE of the following is true:
        ;;     - `icicle-highlight-input-noncompletion' determined that it's a local file.
        ;;     - We're not completing file names.
        ;;     - The user said not to test for remote file names (i.e. assume files are local).
        ;;     - `icicle-highlight-input-noncompletion' did not determine that it is a remote file
        ;;       AND we test and determine now that it is not a remote file.
        ;;
        ;;   * `*Completions*' is already displayed
        ;;     OR `icicle-incremental-completion-p' is neither t nor nil (e.g. `always').
        ;;
        ;;   * There are not too many candidates
        ;;     OR we have waited the full delay.
        (when (and icicle-incremental-completion-p
                   (or (eq remote-test 'file-local-p)
                       (not (icicle-file-name-input-p))
                       (not icicle-test-for-remote-files-flag) ; This means user claims it's local.
                       (and (not (eq remote-test 'file-remote-p))
                            ;; Might still be remote if `icicle-highlight-input-completion-failure'
                            ;; is `always' or `explicit-remote' - cannot tell from `remote-test'.
                            (not (icicle-file-remote-p icicle-current-input))))
                   (or (get-buffer-window "*Completions*" 0) ; Already displayed.
                       ;; If value is, say, `always' or `display' then update anyway.
                       (not (eq t icicle-incremental-completion-p)))
                   (let ((len  (length icicle-completion-candidates)))

;;; $$$$$$           (or (and (> len 1)  (> icicle-incremental-completion-threshold len)) ; Not many
;;; REPLACED PREVIOUS LINE with next one: `sit-for' even if only one candidate.
                     (or (> icicle-incremental-completion-threshold len) ; Not many
                         (sit-for icicle-incremental-completion-delay)))) ; Wait, unless input.
;;; $$$$$$ OLD
;;;         (when (and icicle-incremental-completion-p
;;;                    (or (memq remote-test '(file-local-p file-remote-p))
;;;                        (not (icicle-file-name-input-p))
;;;                        (not icicle-test-for-remote-files-flag)
;;;                        ;; Might still be remote if `icicle-highlight-input-completion-failure'
;;;                        ;; is `always' or `explicit-remote' - cannot tell from `remote-test'.
;;;                        (and (not (eq remote-test 'file-local-p)) ; We don't know if it's local.
;;;                             (not (icicle-file-remote-p icicle-current-input))))
;;;                    (or (get-buffer-window "*Completions*" 0) ; Already displayed.
;;;                        ;; If value is, say, `always' or `display' then update anyway.
;;;                        (not (eq t icicle-incremental-completion-p)))
;;;                    (let ((len  (length icicle-completion-candidates)))
;;;                      (or (and (> len 1)  (> icicle-incremental-completion-threshold len)) ; Not many
;;;                          (sit-for icicle-incremental-completion-delay)))) ; Wait, unless input.
          (let ((icicle-edit-update-p  t))
            (funcall (or icicle-last-completion-command
                         (if (eq icicle-current-completion-mode 'prefix)
                             #'icicle-prefix-complete
                           #'icicle-apropos-complete)))
            (run-hooks 'icicle-update-input-hook)))))
    (setq mark-active  nil)))

(defun icicle-highlight-input-noncompletion ()
  "Highlight the portion of the current input that does not complete.
See the doc strings of `icicle-highlight-input-completion-failure' and
`icicle-test-for-remote-files-flag' for information about when this
highlighting occurs.

If we know the input is a remote file name, return `file-remote-p'.
If we know it is a local file name, return `file-local-p'.
If part of the input matches candidates, return that matching part.
If no highlighting was attempted, return nil."
  (let ((input-start   (icicle-minibuffer-prompt-end))
        (input         (icicle-input-from-minibuffer))
        (file-local-p  nil))
    (cond
      ;; No input.
      ((string= "" input) "")           ; Return string: highlighting attempted.

      ;; One of these: pending input,
      ;;               not highlighting,
      ;;               highlighting `explicit-*' but not explicitly completing (TAB/S-TAB),
      ;;               highlighting `implicit-*' but not incrementally completing,
      ;;               highlighting `*-strict'   but not strict completion (and testing remote files)
      ;;               there are more candidates than the threshold for highlighting.
      ((or (input-pending-p)
           (not icicle-highlight-input-completion-failure)
           (and (not (icicle-get-safe this-command 'icicle-completing-command))
                (memq icicle-highlight-input-completion-failure
                      '(explicit explicit-strict explicit-remote)))
           (and (not icicle-incremental-completion)
                (memq icicle-highlight-input-completion-failure '(implicit implicit-strict)))
           (and (not (icicle-require-match-p))
                icicle-test-for-remote-files-flag ; nil flag ignores strict setting for highlighting
                (memq icicle-highlight-input-completion-failure '(implicit-strict explicit-strict)))
           (let ((len  (length icicle-completion-candidates)))
             (and (> len 1)  (> len icicle-highlight-input-completion-failure-threshold))))
       nil)                             ; Return nil: no highlighting attempted.

      ;; Cursor is to the left of the last mismatch position.
      ((and icicle-input-fail-pos  (< (point) icicle-input-fail-pos))
       (setq icicle-input-fail-pos  nil) ; Reset failure position.
       ;; Remove vestigial highlighting on matched part (e.g. from another completion mode).
       (when (and (> (or icicle-input-fail-pos  (point-max)) input-start)
                  (overlayp icicle-input-completion-fail-overlay))
         (delete-overlay icicle-input-completion-fail-overlay))
       nil)                             ; Return nil: no highlighting attempted.

      ;; Remote file-name input, user did not say to skip testing for remote files,
      ;; and highlighting is not `always' or `explicit-remote'.
      ((and (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
            (not (memq icicle-highlight-input-completion-failure '(always explicit-remote)))
            icicle-test-for-remote-files-flag
            (let ((remotep  (icicle-file-remote-p input)))
              (unless remotep (setq file-local-p  'file-local-p)) ; We know it's local, so save that.
              remotep))
       ;; Do the same as for the previous, except return indication that we know it is a remote file.
       (setq icicle-input-fail-pos  nil)

       (when (and (> (or icicle-input-fail-pos  (point-max)) input-start)
                  (overlayp icicle-input-completion-fail-overlay))
         (delete-overlay icicle-input-completion-fail-overlay))
       'file-remote-p)                  ; Return `file-remote-p': we know it is a remote file.

      ((and icicle-highlight-input-completion-failure-delay
            (progn (message nil)        ; Clear any message, e.g. "Computing completion candidates..."
                   (sit-for icicle-highlight-input-completion-failure-delay)))
       ;; First, a quick check through last two chars.
       ;; If last succeeds, then done.
       ;; If last fails and next-to-last succeeds, then done.
       ;; Otherwise, highlight the others using a binary search.
       (let ((matchp  (icicle-any-candidates-p input))) ; Entire input, through last char.
         (unless matchp
           ;; Record failure position and highlight last char.
           (setq icicle-input-fail-pos  (if icicle-input-fail-pos
                                            (min icicle-input-fail-pos (point-max))
                                          (point-max)))

           (cond (icicle-input-completion-fail-overlay ; Don't recreate if exists.
                  ;; Specify buffer in case overlay exists but is in a diff (e.g. recursive) minibuffer.
                  (move-overlay icicle-input-completion-fail-overlay
                                (1- icicle-input-fail-pos) (point-max)
                                (window-buffer (active-minibuffer-window)))
                  (overlay-put icicle-input-completion-fail-overlay
                               'face (if (icicle-require-match-p)
                                         'icicle-input-completion-fail
                                       'icicle-input-completion-fail-lax)))
                 (t
                  (setq icicle-input-completion-fail-overlay (make-overlay (1- icicle-input-fail-pos)
                                                                           (point-max)))
                  (overlay-put icicle-input-completion-fail-overlay
                               'face (if (icicle-require-match-p)
                                         'icicle-input-completion-fail
                                       'icicle-input-completion-fail-lax))))
           ;; See if next-to-last char gives a match.  Typical use case: mistyping a char at end.
           (setq input  (substring input 0 (1- (length input))))
           (unless (string= "" input)
             (setq matchp  (icicle-any-candidates-p input))
             ;; If more than just the last char fails, highlight the others using binary search.
             (unless matchp (icicle-highlight-input-noncompletion-rest)))))
       ;; Highlighting attempted, so return non-nil.  If we know it's local, return `file-local-p'.
       ;; If we don't know that, return the part of INPUT that matches.
       (or file-local-p  input))
      (t nil))))                        ; Return nil: no highlighting attempted.

(defun icicle-highlight-input-noncompletion-rest ()
  "Helper function for `icicle-highlight-input-noncompletion'."
  (let* ((input-start  (icicle-minibuffer-prompt-end))
         (pos          (1- icicle-input-fail-pos))
         (delta        pos)
         (last-pos     input-start)
         (matchp       nil)
         input)
    (while (and (> pos input-start)  (or (not matchp)  (< pos icicle-input-fail-pos))) ; Binary search.
      (setq input   (buffer-substring input-start pos)
            delta   (max 1 (/ (abs (- pos last-pos)) 2))
            matchp  (icicle-any-candidates-p input))
      ;; $$$$$$ Emacs BUG (prefix completion): c:/foo/$$ does not highlight the `$$', because
      ;; (try-completion "c:/foo/$" 'read-file-name-internal "c:/foo/") returns "c:/foo/$".
      ;; (However, c:/foo/$ highlights the `$' correctly.)
      (unless matchp (setq icicle-input-fail-pos  (min pos icicle-input-fail-pos)))
      (setq last-pos  pos
            pos       (if matchp (+ pos delta) (- pos delta))))
    (unless (or (< pos input-start)  (> pos icicle-input-fail-pos))
      (cond (icicle-input-completion-fail-overlay ; Don't recreate if exists.
             (move-overlay icicle-input-completion-fail-overlay (1- icicle-input-fail-pos) (point-max))
             (overlay-put icicle-input-completion-fail-overlay
                          'face (if (icicle-require-match-p)
                                    'icicle-input-completion-fail
                                  'icicle-input-completion-fail-lax)))
            (t
             (setq icicle-input-completion-fail-overlay (make-overlay (1- icicle-input-fail-pos)
                                                                      (point-max)))
             (overlay-put icicle-input-completion-fail-overlay
                          'face (if (icicle-require-match-p)
                                    'icicle-input-completion-fail
                                  'icicle-input-completion-fail-lax)))))
    input))                             ; Return part of INPUT that matches.

(defun icicle-ms-windows-NET-USE (drive)
  "Return result of calling MS Windows `NET USE' command on DRIVE.
DRIVE is a Windows drive name, such as `f:'.
A return value of zero means DRIVE is a mapped network drive."
  (if (and (fboundp 'puthash)  (hash-table-p icicle-ms-windows-drive-hash))
      (let ((lookup  (gethash drive icicle-ms-windows-drive-hash 'no-assoc)))
        (if (eq lookup 'no-assoc)
            (puthash drive (call-process shell-file-name nil nil nil shell-command-switch
                                         (concat "NET USE " drive))
                     icicle-ms-windows-drive-hash)
          lookup))
    ;; Don't bother to hash for Emacs 20, 21, unless `cl.el' happens to be loaded.
    (call-process shell-file-name nil nil nil shell-command-switch (concat "NET USE " drive))))

;;; $$$$$ Should these `*-any-*' fns call `icicle-transform-candidates'?  For now, no, to save time.
(defun icicle-any-candidates-p (input)
  "Return non-nil if there is any completion for INPUT, nil otherwise."
  (condition-case nil
      (funcall (case icicle-current-completion-mode
                 (apropos (if (icicle-file-name-input-p)
                              #'icicle-apropos-any-file-name-candidates-p
                            #'icicle-apropos-any-candidates-p))
                 (otherwise (if (icicle-file-name-input-p)
                                #'icicle-prefix-any-file-name-candidates-p
                              #'icicle-prefix-any-candidates-p)))
               input)
    (error nil)))

(defun icicle-prefix-any-candidates-p (input)
  "Return non-nil if current partial INPUT has prefix completions."
  (setq input  (concat "^" (regexp-quote input)))
  (icicle-apropos-any-candidates-p input))

(defun icicle-prefix-any-file-name-candidates-p (input)
  "Return non-nil if partial file-name INPUT has prefix completions."
  (setq input  (regexp-quote input)
        input  (concat (file-name-directory input) "^" (file-name-nondirectory input)))
  (icicle-apropos-any-file-name-candidates-p input))

(defun icicle-apropos-any-candidates-p (input)
  "Return non-nil if current partial INPUT has apropos completions."
  (when icicle-regexp-quote-flag (setq input  (regexp-quote input)))
  (let* ((minibuffer-completion-table      minibuffer-completion-table)
         (minibuffer-completion-predicate  minibuffer-completion-predicate)
         (all                              (icicle-all-completions
                                            "" minibuffer-completion-table
                                            minibuffer-completion-predicate
                                            (and icicle-buffer-name-input-p ; Used only by Emacs < 23.2.
                                                 icicle-buffer-ignore-space-prefix-flag))))
    (catch 'icicle-apropos-any-candidates-p
      (dolist (cand all)
        ;; Assume no match if error - e.g. due to `string-match' with binary data in Emacs 20.
        ;; Do this everywhere we call `icicle-apropos-complete-match-fn'.
        (when (or (not icicle-apropos-complete-match-fn)
                  (condition-case nil
                      (and (funcall icicle-apropos-complete-match-fn input cand)
                           (or (not icicle-must-pass-after-match-predicate)
                               (funcall icicle-must-pass-after-match-predicate cand)))
                    (error nil)))
          (throw 'icicle-apropos-any-candidates-p cand)))
      nil)))

(defun icicle-apropos-any-file-name-candidates-p (input)
  "Return non-nil if partial file-name INPUT has apropos completions."
  (when (and input  (not (string= "" input))  (eq (aref input (1- (length input))) ?\/))
    (setq input  (substring input 0 (1- (length input))))) ; So we don't non-match highlight the /.
  (let* ((default-directory                (icicle-file-name-directory-w-default input))
         (minibuffer-completion-table      minibuffer-completion-table)
         (minibuffer-completion-predicate  minibuffer-completion-predicate))
    (setq input  (or (icicle-file-name-nondirectory input)  ""))
    (condition-case nil
        (progn (when icicle-regexp-quote-flag (setq input  (regexp-quote input)))
               (let* ((pred              (if (< emacs-major-version 23)
                                             default-directory
                                           minibuffer-completion-predicate))
                      (candidates        (icicle-all-completions "" minibuffer-completion-table pred))
                      (case-fold-search  (if (boundp 'read-file-name-completion-ignore-case)
                                             read-file-name-completion-ignore-case
                                           completion-ignore-case)))
                 (catch 'icicle-apropos-any-file-name-candidates-p
                   (dolist (cand candidates)
                     (when (if (member cand '("../" "./"))
                               (member input '(".." ".")) ; Prevent "" from matching "../"
                             (or (not icicle-apropos-complete-match-fn)
                                 ;; Assume no match if error - e.g. due to `string-match' with
                                 ;; binary data in Emacs 20.  Do this everywhere we call
                                 ;; `icicle-apropos-complete-match-fn'.
                                 (condition-case nil
                                     (and (funcall icicle-apropos-complete-match-fn input cand)
                                          (or (not icicle-must-pass-after-match-predicate)
                                              (funcall icicle-must-pass-after-match-predicate cand)))
                                   (error nil))))
                       (throw 'icicle-apropos-any-file-name-candidates-p cand)))
                   nil)))
      (quit (top-level)))))             ; Let `C-g' stop it.

(defun icicle-clear-minibuffer ()
  "Delete all user input in the minibuffer.
This must be called from the minibuffer."
  (if (fboundp 'delete-minibuffer-contents) (delete-minibuffer-contents) (erase-buffer)))

;; Same as `member-ignore-case' from Emacs 22+.
(if (fboundp 'member-ignore-case)
    (defalias 'icicle-member-ignore-case (symbol-function 'member-ignore-case))
  (defun icicle-member-ignore-case (elt list)
    "Like `member', but ignore differences in case and text representation.
ELT must be a string: ignore non-string elements of LIST.
Treat uppercase and lowercase letters as equal.
Convert Unibyte strings to multibyte, for the comparison."
    (while (and list  (not (and (stringp (car list))
                                (eq t (compare-strings elt 0 nil (car list) 0 nil t)))))
      (setq list  (cdr list)))))

(defun icicle-assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is `equal' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist))  (equal (car (car alist)) key))
    (setq alist  (cdr alist)))
  (let ((tail  alist)
        tail-cdr)
    (while (setq tail-cdr  (cdr tail))
      (if (and (consp (car tail-cdr))  (equal (car (car tail-cdr)) key))
          (setcdr tail (cdr tail-cdr))
        (setq tail  tail-cdr))))
  alist)

(defun icicle-delete-alist-dups (alist)
  "Destructively remove conses from ALIST with `equal' `car's.
Store the result in ALIST and return it.  ALIST must be a proper list.
Of several element with `equal' `car's in ALIST, the first one is
kept."
  (let ((tail  alist))
    (while tail
      (when (consp (car tail))
        (setcdr tail (icicle-assoc-delete-all (caar tail) (cdr tail))))
      (setq tail  (cdr tail))))
  alist)

;; Same as `delete-dups' from Emacs 22+.
(if (fboundp 'delete-dups)
    (defalias 'icicle-delete-dups (symbol-function 'delete-dups))
  (defun icicle-delete-dups (list)
    "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept."
    (let ((tail list))
      (while tail
        (setcdr tail (delete (car tail) (cdr tail)))
        (setq tail (cdr tail))))
    list))

(if (> emacs-major-version 20)          ; Emacs 21+
    (defun icicle-remove-duplicates (sequence &optional test)
      "Copy of SEQUENCE with duplicate elements removed.
Optional arg TEST is the test function.  If nil, test with `equal'.
See `make-hash-table' for possible values of TEST."
      (setq test  (or test  #'equal))
      (let ((htable  (make-hash-table :test test)))
        (loop for elt in sequence
              unless (gethash elt htable)
              do     (puthash elt elt htable)
              finally return (loop for i being the hash-values in htable collect i))))

  (defun icicle-remove-duplicates (list &optional use-eq)
    "Copy of LIST with duplicate elements removed.
Test using `equal' by default, or `eq' if optional USE-EQ is non-nil."
    (let ((tail  list)
          new)
      (while tail
        (unless (if use-eq (memq (car tail) new) (member (car tail) new))
          (push (car tail) new))
        (pop tail))
      (nreverse new))))

(defun icicle-remove-dups-if-extras (list)
  "`icicle-remove-duplicates' if `icicle-extra-candidates' is non-nil.
If `icicle-extra-candidates' is nil, then just return LIST.

Note: When you use this as the value of `icicle-transform-function',
be aware that during completion and before applying this function,
`icicle-extra-candidates' is redefined locally by removing its
candidates that do not match the current input.  So this function then
has the effect of removing any duplicates that match the input.  If
there are no such matching candidates, then LIST is returned."
  (if icicle-extra-candidates (icicle-remove-duplicates list) list))

;; Same as `bmkp-list-position' in `bookmark+-1.el'.
;; Simple version of `cl-position' for all Emacs versions.
(defun icicle-list-position (item items &optional test)
  "Find the first occurrence of ITEM in list ITEMS.
Return the index of the matching item, or nil if not found.
Items are compared using binary predicate TEST, or `equal' if TEST is
nil."
  (unless test (setq test  'equal))
  (let ((pos  0))
    (catch 'icicle-list-position
      (dolist (itm  items)
        (when (funcall test item itm) (throw 'icicle-list-position pos))
        (setq pos  (1+ pos)))
      nil)))

;; Same as `bmkp-repeat-command' in `bookmark+-1.el'.
(defun icicle-repeat-command (command)
  "Repeat COMMAND."
  (let ((repeat-message-function  'ignore))
    (setq last-repeatable-command  command)
    (repeat nil)))

(defvar icicle-dirs-done ()
  "Directories already processed.")


;; Args INCLUDE-DIRS-P and PREDICATE are not used in the Icicles code yet
;;
;; This is the SAME as `diredp-files-within' in Dired+.  This code must be kept in sync with that.
;;
(defun icicle-files-within (file-list accum &optional no-symlinks-p include-dirs-p predicate)
  "List of readable files in FILE-LIST, handling directories recursively.
FILE-LIST is a list of file names or a function that returns such.
If a function then invoke it with no args to get the list of files.

Accessible directories in the list of files are processed recursively
to include their files and the files in their subdirectories.  The
directories themselves are not included, unless optional arg
INCLUDE-DIRS-P is non-nil.  (Directories in
`icicle-ignored-directories' are skipped.)

But if there is a Dired buffer for such a directory, and if FILE-LIST
is a function, then it is invoked in that Dired buffer to return the
list of files to use.  E.g., if FILE-LIST is `dired-get-marked-files'
then only the marked files and subdirectories are included.  If you
have more than one Dired buffer for a directory that is processed
here, then only the first one in `dired-buffers' is used.

The list of files is accumulated in ACCUM, which is used for recursive
calls.

Non-nil optional arg NO-SYMLINKS-P means do not follow symbolic links.

Non-nil optional arg INCLUDE-DIRS-P means include directory names
along with the names of non-directories.

Non-nil optional arg PREDICATE must be a function that accepts a
file-name argument.  Only files (and possibly directories) that
satisfy PREDICATE are included in the result."
  (if (fboundp 'diredp-files-within)    ; The code SHOULD be kept the same!
      (diredp-files-within file-list accum no-symlinks-p include-dirs-p predicate)
    ;; Binds `icicle-dirs-done' for use as free var in `icicle-files-within-1'."
    (let ((icicle-dirs-done  ()))
      (nreverse (icicle-files-within-1 file-list accum no-symlinks-p include-dirs-p predicate)))))

;; This is the SAME as `diredp-files-within-1' in Dired+.  This code must be kept in sync with that.
;;
(defun icicle-files-within-1 (file-list accum no-symlinks-p include-dirs-p predicate)
  "Helper for `icicle-files-within'."   ; `icicle-dirs-done' is free here.
  (if (fboundp 'diredp-files-within-1)
      (diredp-files-within-1 file-list accum no-symlinks-p include-dirs-p predicate)
    (let ((files  (if (functionp file-list) (funcall file-list) file-list))
          (res    accum)
          file)
      (when (and files  predicate) (setq files  (icicle-remove-if-not predicate files)))
      (while files
        (setq file  (car files))
        (unless (and no-symlinks-p  (file-symlink-p file))
          (if (file-directory-p file)
              ;; Skip directory if ignored, already treated, or inaccessible.
              (when (and (not (member (file-name-nondirectory file) icicle-ignored-directories))
                         (not (member (file-truename file) icicle-dirs-done))
                         (file-accessible-directory-p file))
                (setq res  (icicle-files-within-1
                            (or (and (functionp file-list)
                                     (dired-buffers-for-dir file) ; Removes killed buffers.
                                     (with-current-buffer
                                         (cdr (assoc (file-name-as-directory file) dired-buffers))
                                       (funcall file-list)))
                                (directory-files file 'FULL icicle-re-no-dot))
                            res
                            no-symlinks-p
                            include-dirs-p
                            predicate))
                (when include-dirs-p (push file res))
                (push (file-truename file) icicle-dirs-done))
            (when (file-readable-p file) (push file res))))
        (pop files))
      res)))

(defun icicle-delete-whitespace-from-string (string &optional from to)
  "Remove whitespace from substring of STRING from FROM to TO.
If FROM is nil, then start at the beginning of STRING (FROM = 0).
If TO is nil, then end at the end of STRING (TO = length of STRING).
FROM and TO are zero-based indexes into STRING.
Character FROM is affected (possibly deleted).  Character TO is not."
  (setq from  (or from  0)
        to    (or to  (length string)))
  (with-temp-buffer
    (insert string)
    (goto-char (+ from (point-min)))
    (let ((count  from)
          char)
      (while (and (not (eobp))  (< count to))
        (setq char  (char-after))
        (if (memq char '(?\  ?\t ?\n))  (delete-char 1)  (forward-char 1))
        (setq count  (1+ count)))
      (buffer-string))))

(defun icicle-barf-if-outside-minibuffer ()
  "Raise an error if `this-command' is called outside the minibuffer.
Return non-nil otherwise."
  (or (eq (current-buffer) (window-buffer (minibuffer-window)))
      (icicle-user-error "Command `%s' must be called from the minibuffer" this-command)))

(defun icicle-barf-if-outside-Completions ()
  "Raise error if `this-command' is called outside buffer `*Completions*'.
Return non-nil otherwise."
  (or (eq (current-buffer) (get-buffer "*Completions*"))
      (icicle-user-error "Command `%s' must be called from `*Completions*' buffer" this-command)))

(defun icicle-barf-if-outside-Completions-and-minibuffer ()
  "Error if `this-command' called outside `*Completions*' and minibuffer.
Return non-nil otherwise."
  (or (or (eq (current-buffer) (window-buffer (minibuffer-window)))
          (eq (current-buffer) (get-buffer "*Completions*")))
      (icicle-user-error "`%s' must be called from `*Completions*' or minibuffer" this-command)))

(defun icicle-command-abbrev-save ()
  "Save `icicle-command-abbrev-alist'.  Used on `kill-emacs-hook'."
  (icicle-condition-case-no-debug err   ; Don't raise an error, since it's on `kill-emacs-hook'.
      (let ((sav  (get 'icicle-command-abbrev-alist 'saved-value)))
        (unless (and (or (null sav)
                         (and (consp sav)  (consp (car sav))  (consp (cdar sav))
                              (consp (car (cdar sav)))))
                     (equal icicle-command-abbrev-alist (car (cdar sav))))
          (funcall icicle-customize-save-variable-function 'icicle-command-abbrev-alist
                   icicle-command-abbrev-alist)))
    (error (message "Cannot save new value of `icicle-command-abbrev-alist'") (sleep-for 3))))

(defun icicle-expand-file-or-dir-name (input dir)
  "Expand file-name INPUT in directory DIR.
Similar to `expand-file-name', except:

 - If INPUT does not end in a slash, and DIR/INPUT is a directory,
   add a trailing slash.

 - If INPUT ends in a slash, but DIR/INPUT is not a directory, then
   remove the trailing slash.

 - if INPUT or DIR contains consecutive slashes (`/'), do not collapse
   them to a single slash."
  (let ((expanded-input  (directory-file-name (icicle-expand-file-name-20 input dir))))
    ;; Add trailing slash if input is a directory.
    (when (file-directory-p expanded-input)
      (setq expanded-input  (file-name-as-directory expanded-input)))
    expanded-input))

(defun icicle-expand-file-name-20 (input &optional dir)
  "Emacs 20's `expand-file-name': does not collapse consecutive slashes."
  ;; Replace // with five ^Gs, then replace back again.
  (let ((escaped-input  (and input  (replace-regexp-in-string "//" (make-string 5 7) input)))
        (escaped-dir    (and dir  (replace-regexp-in-string "//" (make-string 5 7) dir))))
    (replace-regexp-in-string (make-string 5 7) "//" (expand-file-name escaped-input escaped-dir))))

(defun icicle-start-of-candidates-in-Completions ()
  "Return buffer position of the first candidate in `*Completions*'."
  (save-excursion
    (goto-char (point-min))
    (when icicle-show-Completions-help-flag (forward-line 2))
    (point)))

(defun icicle-key-description (keys &optional prefix angles)
  "Like `key-description', but does not use angle brackets, by default.
Non-nil optional arg ANGLES means use angle brackets."
  (let ((result  (if (< emacs-major-version 22)
                     (key-description keys)
                   (key-description keys prefix))))
    (unless angles                      ; Assume that spaces separate function keys.
      (setq result  (replace-regexp-in-string "<\\([^>]+\\)>" "\\1" result 'fixed-case)))
    result))

;; $$ Not used.
;; (defun icicle-alist-delete-all (key alist &optional test)
;;     "Delete from ALIST all elements whose car is the same as KEY.
;; Optional arg TEST is the equality test to use.  If nil, `eq' is used.
;; Return the modified alist.
;; Elements of ALIST that are not conses are ignored."
;;     (setq test  (or test  #'eq))
;;     (while (and (consp (car alist))  (funcall test (car (car alist)) key))
;;       (setq alist  (cdr alist)))
;;     (let ((tail  alist) tail-cdr)
;;       (while (setq tail-cdr  (cdr tail))
;;         (if (and (consp (car tail-cdr))  (funcall test (car (car tail-cdr)) key))
;;             (setcdr tail (cdr tail-cdr))
;;           (setq tail  tail-cdr))))
;;     alist)

;; Standard Emacs 22+ function, defined here for Emacs 20, 21.
(unless (fboundp 'select-frame-set-input-focus) ; Defined in Emacs 22.
  (defun select-frame-set-input-focus (frame)
    "Select FRAME, raise it, and set input focus, if possible."
    (select-frame frame)
    (raise-frame frame)
    ;; Ensure, if possible, that frame gets input focus.
    (cond ((eq window-system 'x) (x-focus-frame frame))
          ((eq window-system 'w32) (w32-focus-frame frame)))
    (cond (focus-follows-mouse (set-mouse-position (selected-frame) (1- (frame-width)) 0)))))

;; Standard Emacs 21+ function, defined here for Emacs 20.
(unless (fboundp 'assq-delete-all)
  (defun assq-delete-all (key alist)
    "Delete from ALIST all elements whose car is `eq' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
    (while (and (consp (car alist))  (eq (car (car alist)) key))  (setq alist  (cdr alist)))
    (let ((tail  alist) tail-cdr)
      (while (setq tail-cdr  (cdr tail))
        (if (and (consp (car tail-cdr))  (eq (car (car tail-cdr)) key))
            (setcdr tail (cdr tail-cdr))
          (setq tail  tail-cdr))))
    alist))

(defun icicle-first-N (n list)
  "Return a new list of at most the N first elements of LIST."
  (let ((firstN  ()))
    (while (and list  (> n 0))
      (push (car list) firstN)
      (setq n     (1- n)
            list  (cdr list)))
    (setq firstN (nreverse firstN))))

(defun icicle-abbreviate-or-expand-file-name (filename &optional dir dont-add-default-dir-p)
  "Expand FILENAME, and abbreviate it if `icicle-use-~-for-home-dir-flag'.
If FILENAME is not absolute, call `icicle-expand-file-name-20' to make
 it absolute.  This does not collapse consecutive slashes (`/').
If `icicle-use-~-for-home-dir-flag', call `abbreviate-file-name'.

If DIR is absolute, pass it to `icicle-expand-file-name-20'.
Otherwise, ignore it (treat it as nil)."
  (unless (file-name-absolute-p filename)
    (when (and dir  (not dont-add-default-dir-p)  (not (file-name-absolute-p dir)))
      (setq dir  nil))                  ; Do not use a relative dir.
    (setq filename (icicle-expand-file-name-20 filename dir)))
  (if icicle-use-~-for-home-dir-flag (abbreviate-file-name filename) filename))

(defun icicle-reversible-sort (list &optional key)
  "`sort' LIST using `icicle-sort-comparer' and return the result.
Reverse the result if `icicle-reverse-sort-p' is non-nil.
If `icicle-sort-comparer' is a cons (other than a lambda form), then
 use `icicle-multi-sort' as the sort predicate.
Otherwise, use `icicle-sort-comparer' as the sort predicate.

If `icicle-sort-comparer' is nil and `icicle-reverse-sort-p' is
non-nil then return LIST reversed.  If `icicle-reverse-sort-p' is nil
then just return LIST.

Optional arg KEY is a selector function to apply to each item to be be
compared.  If nil, then the entire item is used."
  ;;$$ (when (and icicle-edit-update-p  icicle-completion-candidates
  ;;              (> (length icicle-completion-candidates) icicle-incremental-completion-threshold))
  ;;     (message "Sorting candidates..."))
  (unless key (setq key  'identity))
  (let ((sort-fn  (and icicle-sort-comparer
                       (lambda (s1 s2)
                         (when icicle-transform-before-sort-p
                           (setq s1  (icicle-transform-multi-completion s1)
                                 s2  (icicle-transform-multi-completion s2)))
                         ;; If we have an inappropriate sort order, get rid of it.  This can happen if
                         ;; the user chooses a sort appropriate to one kind of candidate and then
                         ;; tries completion for a different kind of candidate.
                         (condition-case nil
                             (and icicle-sort-comparer ; nil in case of error earlier in list.
                                  (if (and (not (functionp icicle-sort-comparer))
                                           (consp icicle-sort-comparer))
                                      (icicle-multi-sort (funcall key s1) (funcall key s2))
                                    (funcall icicle-sort-comparer (funcall key s1) (funcall key s2))))
                           (error (message "Inappropriate sort order - reverting to UNSORTED")
                                  (sit-for 1)
                                  (setq icicle-sort-comparer  nil)
                                  nil))))))
    (when (or sort-fn  icicle-reverse-sort-p)
      (setq list  (if sort-fn
                      (sort list (if icicle-reverse-sort-p
                                     (lambda (a b) (not (funcall sort-fn a b)))
                                   sort-fn))
                    (nreverse list)))))
  list)

;; Essentially the same as `bmkp-multi-sort'.
(defun icicle-multi-sort (s1 s2)
  "Try predicates in `icicle-sort-comparer', in order, until one decides.
The (binary) predicates are applied to S1 and S2.
See the description of `icicle-sort-comparer'.
If `icicle-reverse-multi-sort-p' is non-nil, then reverse the order
for using multi-sorting predicates."
  (let ((preds       (car icicle-sort-comparer))
        (final-pred  (cadr icicle-sort-comparer))
        (result      nil))
    (when icicle-reverse-multi-sort-p (setq preds  (reverse preds)))
    (catch 'icicle-multi-sort
      (dolist (pred  preds)
        (setq result  (funcall pred s1 s2))
        (when (consp result)
          (when icicle-reverse-multi-sort-p (setq result  (list (not (car result)))))
          (throw 'icicle-multi-sort (car result))))
      (and final-pred  (if icicle-reverse-multi-sort-p
                           (not (funcall final-pred s1 s2))
                         (funcall final-pred s1 s2))))))

(defun icicle-make-plain-predicate (pred &optional final-pred)
  "Return a plain predicate that corresponds to component-predicate PRED.
PRED and FINAL-PRED correspond to their namesakes in
`icicle-sort-comparer' (which see).

PRED should return `(t)', `(nil)', or nil.

Optional arg FINAL-PRED is the final predicate to use if PRED cannot
decide (returns nil).  If FINAL-PRED is nil, then `icicle-alpha-p' is
used as the final predicate."
  `(lambda (b1 b2)
    (let ((res  (funcall ',pred b1 b2)))
      (if res (car res) (funcall ',(or final-pred  'icicle-alpha-p) b1 b2)))))

(defun icicle-alpha-p (s1 s2)
  "True if string S1 sorts alphabetically before string S2.
Comparison respects `case-fold-search'."
  (when case-fold-search (setq s1  (icicle-upcase s1)
                               s2  (icicle-upcase s2)))
  (string-lessp s1 s2))

(defun icicle-get-alist-candidate (string &optional no-error-p)
  "Return full completion candidate that corresponds to displayed STRING.
STRING is the name of the candidate, as shown in `*Completions*'.
Non-nil optional argument NO-ERROR-P means display a message and
return nil instead of raising an error if STRING is ambiguous.
If the value of NO-ERROR-P is `no-error-no-msg', then show no message
and just return nil.

If `icicle-whole-candidate-as-text-prop-p' is non-nil, then the full
candidate might be available as text property `icicle-whole-candidate'
of STRING.  If so, then that is used.

Otherwise, the full candidate is obtained from
`icicle-candidates-alist'.  In this case:
 If the user cycled among candidates or used `mouse-2', then use the
   current candidate number, and ignore STRING.
 Otherwise:
   If only one candidate matches STRING, use that.
   Else respect NO-ERROR-P and tell user to use cycling or `mouse-2'."
  (or (and icicle-whole-candidate-as-text-prop-p
           (get-text-property 0 'icicle-whole-candidate string))
      (and icicle-candidates-alist
           (let ((cand-entries  (icicle-filter-alist icicle-candidates-alist
                                                     icicle-completion-candidates)))
             (if (wholenump icicle-candidate-nb) ; Cycled or used `mouse-2' to choose the candidate.
                 (elt cand-entries (mod icicle-candidate-nb (length icicle-candidates-alist)))
               ;; If `icicle-completion-candidates' is nil, because user didn't use `TAB' or `S-TAB',
               ;; then `icicle-candidates-alist' can contain non-matches.  So, we check for more than
               ;; one match.  However, we cannot just use `assoc', because candidates might be
               ;; multi-completions (lists).
               (let ((first-match  (icicle-first-matching-candidate string icicle-candidates-alist)))
                 (if (and first-match  (not (icicle-first-matching-candidate
                                             string
                                             (setq cand-entries  (delete first-match cand-entries)))))
                     first-match        ; Only one match, so use it.
                   (let ((msg  "Ambiguous choice. Cycle or use `mouse-2' to choose unique matching \
candidate."))
                     (unless no-error-p (error msg))
                     (unless (eq no-error-p 'no-error-no-msg) (icicle-msg-maybe-in-minibuffer msg))
                     nil))))))))        ; Return nil for ambiguous string if NO-ERROR-P.

(defun icicle-filter-alist (alist filter-keys)
  "Filter ALIST, keeping items whose cars match FILTER-KEYS, in order.
The original ALIST is not altered; a copy is filtered and returned.
If FILTER-KEYS is empty, then ALIST is returned, not a copy."
  (if filter-keys
      (icicle-remove-if-not
       (lambda (item)
         (member (if (consp (car item))
                     ;; $$$$$$ (mapconcat #'identity (car item) icicle-list-join-string)
                     (mapconcat #'identity (car item) icicle-list-join-string)
                   (car item))
                 filter-keys))
       alist)
    alist))

;;; $$$$$$$$$$$$$$$$$$
;;; (defun icicle-first-matching-candidate (cand candidates)
;;;   "Return the first element of alist CANDIDATES that matches CAND.
;;; If CANDIDATES is a normal list of completion candidates, then this is
;;; just `assoc'.
;;; If CANDIDATES contains multi-completions, then matching means matching
;;; the concatenated multi-completion parts, joined by
;;; `icicle-list-join-string'."
;;;   (cond ((null candidates) nil)
;;;         ((if (consp (caar candidates))  ; Multi-completion candidate
;;;              (save-match-data
;;;                (string-match cand (mapconcat #'identity (caar candidates)
;;;                                              icicle-list-join-string)))
;;;            (equal cand (caar candidates))) ; This case is just `assoc'.
;;;          (car candidates))
;;;         (t (icicle-first-matching-candidate cand (cdr candidates)))))

(defun icicle-first-matching-candidate (cand candidates)
  "Return the first element of alist CANDIDATES that matches CAND.
Return nil if there is no such element.
If CANDIDATES is a normal list of completion candidates, then this is
just `assoc'.
If CANDIDATES contains multi-completions, then matching means matching
the concatenated multi-completion parts, joined by
`icicle-list-join-string'."
  (let ((res  nil))
    (if (null candidates)
        (setq res  nil)
      (while (and candidates  (not res))
        (when (or (and (consp (caar candidates)) ; Multi-completion candidate
                       (save-match-data
                         (string-match (regexp-quote cand)
                                       ;; $$$$$$ (mapconcat #'identity (caar candidates) icicle-list-join-string)
                                       (mapconcat #'identity (caar candidates)
                                                  icicle-list-join-string))))
                  (equal cand (caar candidates)))
          (setq res  (car candidates)))
        (setq candidates  (cdr candidates))))
    res))

;;; $$$$$$$$
;;; (defun icicle-completing-p ()
;;;   "Non-nil if reading minibuffer input with completion.
;;; This caches the value returned in variable `icicle-completing-p'.
;;; Use the function, not the variable, to test, if not sure to be in the
;;; minibuffer."
;;;   (setq icicle-completing-p             ; Cache the value.
;;;         (and (active-minibuffer-window)
;;;              ;; We used to include filename keymaps in MAPS, but that does not work for
;;;              ;; Emacs > 24.3 - it uses a composed keymap that it creates on the fly.
;;;              ;; So instead we just check `minibuffer-completing-file-name' now for Emacs 22+.
;;;              (or (and minibuffer-completing-file-name  (> emacs-major-version 21))
;;;                  (let* ((loc-map  (current-local-map))
;;;                         (parent   (keymap-parent loc-map))
;;;                         (maps     (list minibuffer-local-completion-map
;;;                                         minibuffer-local-must-match-map)))
;;;                    (and (or (and parent (member parent maps))  (member loc-map maps))
;;;                         t))))))              ; Cache t, not the keymap portion.

(defun icicle-completing-p ()
  "Non-nil if reading minibuffer input with completion.
This caches the value returned in variable `icicle-completing-p'.
Use the function, not the variable, to test, if not sure to be in the
minibuffer."
  (let ((mini-win  (active-minibuffer-window)))
    (setq icicle-completing-p           ; Cache the value.
          (and mini-win  (icicle-with-selected-window mini-win
                           (local-key-binding [icicle-is-completion-map]))))))

;; This is just `substring-no-properties', defined also for Emacs < 22.
(defun icicle-substring-no-properties (string &optional from to)
  "Return a substring of STRING, without text properties.
It starts at index FROM and ending before TO.
TO may be nil or omitted; then the substring runs to the end of STRING.
If FROM is nil or omitted, the substring starts at the beginning of STRING.
If FROM or TO is negative, it counts from the end.

With one argument, just copy STRING without its properties."
  (if (fboundp 'substring-no-properties)
      (substring-no-properties string from to) ; Emacs 22.
    (let ((substrg  (copy-sequence (substring string (or from  0) to))))
      (set-text-properties 0 (length substrg) nil substrg)
      substrg)))

(defun icicle-highlight-lighter ()
  "Highlight `Icy' mode-line indicator of Icicle mode.
Highlighting indicates the current completion status."
  (when icicle-highlight-lighter-flag
    (let ((strg
           ;; Don't bother with buffer completion and `read-buffer-completion-ignore-case'.
           (if (if (and (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
                        (boundp 'read-file-name-completion-ignore-case))
                   read-file-name-completion-ignore-case
                 completion-ignore-case)
               " ICY"
             " Icy"))
          (face  (cond ((and icicle-candidate-action-fn  (icicle-require-match-p))
                        '(icicle-multi-command-completion icicle-mustmatch-completion))
                       (icicle-candidate-action-fn
                        'icicle-multi-command-completion)
                       ((icicle-require-match-p)
                        '(icicle-completion icicle-mustmatch-completion))
                       (t
                        'icicle-completion))))
      (when icicle-candidate-action-fn (setq strg  (concat strg "+")))
      (when (and icicle-multi-completing-p  icicle-show-multi-completion-flag)
        (setq strg  (concat strg "||")))
      (put-text-property 0 (length strg) 'face face strg)
      (icicle-clear-lighter)
      (add-to-list 'minor-mode-alist `(icicle-mode ,strg)))
    (condition-case nil
        (if (fboundp 'redisplay) (redisplay t) (force-mode-line-update t))
      (error nil))))                    ; Ignore errors from, e.g., killed buffers.

(defun icicle-unhighlight-lighter ()
  "Unhighlight `Icy' mode-line indicator of Icicle mode."
  (when icicle-highlight-lighter-flag
    (let ((strg  (if case-fold-search  " ICY"  " Icy")))
      (icicle-clear-lighter)
      (add-to-list 'minor-mode-alist `(icicle-mode ,strg)))
    (condition-case nil
        (if (fboundp 'redisplay) (redisplay t) (force-mode-line-update t))
      (error nil))))                    ; Ignore errors from, e.g., killed buffers.

(defun icicle-clear-lighter (&optional only)
  "Remove Icicle mode lighter from `minor-mode-alist'."
  (unless (eq only 'truncated)
    (setq minor-mode-alist  (delete '(icicle-mode " Icy")    minor-mode-alist)
          minor-mode-alist  (delete '(icicle-mode " Icy+")   minor-mode-alist)
          minor-mode-alist  (delete '(icicle-mode " ICY")    minor-mode-alist)
          minor-mode-alist  (delete '(icicle-mode " ICY+")   minor-mode-alist)
          minor-mode-alist  (delete '(icicle-mode " Icy||")  minor-mode-alist)
          minor-mode-alist  (delete '(icicle-mode " Icy+||") minor-mode-alist)
          minor-mode-alist  (delete '(icicle-mode " ICY||")  minor-mode-alist)
          minor-mode-alist  (delete '(icicle-mode " ICY+||") minor-mode-alist)))
  (unless (eq only 'not-truncated)
    (setq minor-mode-alist  (delete `(icicle-mode ,(concat " Icy"    icicle-lighter-truncation))
                                    minor-mode-alist)
          minor-mode-alist  (delete `(icicle-mode ,(concat " Icy+"   icicle-lighter-truncation))
                                    minor-mode-alist)
          minor-mode-alist  (delete `(icicle-mode ,(concat " ICY"    icicle-lighter-truncation))
                                    minor-mode-alist)
          minor-mode-alist  (delete `(icicle-mode ,(concat " ICY+"   icicle-lighter-truncation))
                                    minor-mode-alist)
          minor-mode-alist  (delete `(icicle-mode ,(concat " Icy||"  icicle-lighter-truncation))
                                    minor-mode-alist)
          minor-mode-alist  (delete `(icicle-mode ,(concat " Icy+||" icicle-lighter-truncation))
                                    minor-mode-alist)
          minor-mode-alist  (delete `(icicle-mode ,(concat " ICY||"  icicle-lighter-truncation))
                                    minor-mode-alist)
          minor-mode-alist  (delete `(icicle-mode ,(concat " ICY+||" icicle-lighter-truncation))
                                    minor-mode-alist))))

(defun icicle-ding ()
  "Same as `ding', but respects `icicle-inhibit-ding-flag'."
  (unless icicle-inhibit-ding-flag (ding)))

(defun icicle-kill-a-buffer (buf &optional nomsg)
  "Kill buffer BUF.
Optional arg NOMSG non-nil means don't display an error message."
  (save-selected-window
    (setq buf  (get-buffer buf))
    (if buf
        (icicle-condition-case-no-debug err
            (if (not (buffer-live-p buf))
                (unless nomsg (message "Buffer already deleted: `%s'" buf))
              (let ((enable-recursive-minibuffers  t)) ; In case called from minibuffer, and modified.
                (if (fboundp 'kill-buffer-and-its-windows)
                    (kill-buffer-and-its-windows buf) ; Defined in `misc-cmds.el'.
                  (kill-buffer buf))))
          (error nil))
      (unless nomsg
        (message "No such live buffer: `%s'"
                 (icicle-propertize (format "%s" buf) 'face 'icicle-msg-emphasis))))))

(if (fboundp 'find-tag-default-as-regexp)
    (defalias 'icicle-read-regexp 'read-regexp) ; Emacs 24.3+

  ;; Same as `bmkp-find-tag-default-as-regexp' in `bookmark+-1.el'.
  (if (fboundp 'bmkp-find-tag-default-as-regexp)
      (defalias 'icicle-find-tag-default-as-regexp 'bmkp-find-tag-default-as-regexp)

    (defun icicle-find-tag-default-as-regexp () ; Emacs < 24.3
      "Return a regexp that matches the default tag at point.
If there is no tag at point, return nil.

When in a major mode that does not provide its own
`find-tag-default-function', return a regexp that matches the
symbol at point exactly."
      (let* ((tagf  (or find-tag-default-function
                        (get major-mode 'find-tag-default-function)
                        'find-tag-default))
             (tag   (funcall tagf)))
        (and tag  (if (eq tagf 'find-tag-default)
                      (format "\\_<%s\\_>" (regexp-quote tag))
                    (regexp-quote tag))))))

  ;; Same as `bmkp-read-regexp' in `bookmark+-1.el'.
  (if (fboundp 'bmkp-read-regexp)
      (defalias 'icicle-read-regexp 'bmkp-read-regexp)

    (if (fboundp 'find-tag-default)
        (defun icicle-read-regexp (prompt &optional default history) ; Emacs 22-24.2
          "Read and return a regular expression as a string.
If PROMPT does not end with a colon and possibly whitespace then
append \": \" to it.

Optional argument DEFAULT is a string or a list of the form
\(DEFLT . SUGGESTIONS), where DEFLT is a string or nil.

The string DEFAULT or DEFLT is added to the prompt and is returned as
the default value if the user enters empty input.  The empty string is
returned if DEFAULT or DEFLT is nil and the user enters empty input.

SUGGESTIONS is used only for Emacs 23 and later.  It is a list of
strings that can be inserted into the minibuffer using `\\<minibuffer-local-map>\\[next-history-element]'.
The values supplied in SUGGESTIONS are prepended to the list of
standard suggestions, which include the tag at point, the last isearch
regexp, the last isearch string, and the last replacement regexp.

Optional argument HISTORY is a symbol to use for the history list.
If nil then use `regexp-history'."
          (let* ((deflt                  (icicle-unlist default))
                 (suggestions            (and (> emacs-major-version 22)
                                              (if (listp default) default (list default))))
                 (suggestions            (and (> emacs-major-version 22)
                                              (append
                                               suggestions
                                               (list (icicle-find-tag-default-as-regexp)
                                                     (car regexp-search-ring)
                                                     (regexp-quote (or (car search-ring)  ""))
                                                     (car (symbol-value
                                                           query-replace-from-history-variable))))))
                 (suggestions            (and (> emacs-major-version 22)
                                              (delete-dups (delq nil (delete "" suggestions)))))
                 (history-add-new-input  nil) ; Do not automatically add default to history for empty input.
                 (input                  (read-from-minibuffer
                                          (cond ((icicle-string-match-p ":[ \t]*\\'" prompt) prompt)
                                                (deflt (format "%s (default %s): " prompt
                                                               (query-replace-descr deflt)))
                                                (t (format "%s: " prompt)))
                                          nil nil nil (or history  'regexp-history) suggestions t)))
            (if (equal input "")
                (or deflt  input)       ; Return the default value when the user enters empty input.
              (prog1 input              ; Add non-empty input to the history and return input.
                (add-to-history (or history  'regexp-history) input)))))

      (defun icicle-read-regexp (prompt &optional default history) ; Emacs 20-21
        "Read and return a string.
Optional arg DEFAULT is a string that is returned when the user enters
empty input.  It can also be a list of strings, of which only the
first is used.
Optional arg HISTORY is a symbol to use for the history list.  If nil,
use `regexp-history'."
        (when (consp default) (setq default  (car default)))
        (read-string (cond ((icicle-string-match-p ":[ \t]*\\'" prompt) prompt)
                           (default (format "%s (default %s): " prompt
                                            (mapconcat #'isearch-text-char-description default "")))
                           (t (format "%s: " prompt)))
                     nil (or history  'regexp-history) default)))))

;; Same as `tap-string-match-p' in `thingatpt+.el'.
;; Do NOT alias `string-match-p', because that is a `defsubst'.
;;
(defun icicle-string-match-p (regexp string &optional start)
  "Like `string-match', but this saves and restores the match data."
  (save-match-data (string-match regexp string start)))

(defun icicle-propertize (object &rest properties)
  "Like `propertize', but for all Emacs versions.
If OBJECT is not a string, then use `prin1-to-string' to get a string."
  (let ((new  (if (stringp object) (copy-sequence object) (prin1-to-string object))))
    (add-text-properties 0 (length new) properties new)
    new))

(defun icicle-unpropertize-completion (string)
  "Remove text properties from STRING.
STRING is typicaly a completion candidate or result.
If STRING is not a string, just return it (raise no error).
If `icicle-remove-icicles-props-p' is nil, just return STRING.  This
 is the case for some Icicles functions that need to further process
 the completion result.
Otherwise, if option `icicle-unpropertize-completion-result-flag' is
 non-nil, then remove all text properties.
Otherwise remove only Icicles internal text properties:
 1. any text properties in `icicle-candidate-properties-alist'.
 2. The following internal text properties added by Icicles:
    `display', `help-echo', `icicle-fancy-candidates',
    `icicle-mode-line-help', `icicle-special-candidate',
    `icicle-user-plain-dot', `icicle-whole-candidate', `invisible'.
    \(Property `mouse-face' is removed by `choose-completion-string'.\)"
  (when (and (stringp string)  icicle-remove-icicles-props-p) ; Do nothing if we're inhibiting removal.
    (let ((len  (length string)))
      (if icicle-unpropertize-completion-result-flag
          (set-text-properties 0 len nil string)
        (remove-text-properties
         0 len '(display                  nil
                 help-echo                nil
                 rear-nonsticky           nil
                 icicle-fancy-candidates  nil
                 icicle-mode-line-help    nil
                 icicle-special-candidate nil
                 icicle-user-plain-dot    nil
                 icicle-whole-candidate   nil
                 invisible                nil)
         string)
        (dolist (entry  icicle-candidate-properties-alist)
          (put-text-property 0 len (car (cadr entry)) nil string)))))
  string)


;; REPLACE ORIGINAL `completion-pcm--all-completions' defined in `minibuffer.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; $$$$$$ Filed Emacs BUG #24676.  Vanilla `completion-pcm--all-completions' reverses the candidate order.
;;
(when (fboundp 'completion-pcm--all-completions)

  (unless (fboundp 'icicle-ORIG-completion-pcm--all-completions)
    (defalias 'icicle-ORIG-completion-pcm--all-completions
        (symbol-function 'completion-pcm--all-completions)))

  (defun icicle-completion-pcm--all-completions (prefix pattern table pred)
    "Find all completions for PATTERN in TABLE obeying PRED.
PATTERN is as returned by `completion-pcm--string->pattern'."
    ;; (cl-assert (= (car (completion-boundaries prefix table pred ""))
    ;;            (length prefix)))
    ;; Find an initial list of possible completions.
    (if (completion-pcm--pattern-trivial-p pattern)
        ;; Minibuffer contains no delimiters -- simple case!
        (all-completions (concat prefix (car pattern)) table pred)
      ;; Use `all-completions' to do an initial cull.  This is a big win, since `all-completions' is written in C!
      (let* (;; Convert search pattern to a standard regular expression.
             (regex                   (completion-pcm--pattern->regex pattern))
             (case-fold-search        completion-ignore-case)
             (completion-regexp-list  (cons regex completion-regexp-list))
             (compl                   (all-completions (concat prefix
                                                               (if (stringp (car pattern)) (car pattern) ""))
                                                       table pred)))
        (if (not (functionp table))     ; The internal functions already obeyed completion-regexp-list.
            compl
          (let ((poss  ()))
            (dolist (c  compl)
              (when (icicle-string-match-p regex c) (push c poss)))
            (nreverse poss)))))))

;; $$$$$$ Filed Emacs BUG #8795.  They added a non-optional arg, METADATA (with no doc).
;;
(defun icicle-completion-all-completions (string table pred point &optional metadata)
  "Icicles version of `completion-all-completions'.
1. Handle all Emacs versions.
2. Append `$' to each candidate, if current input ends in `$'.
3. Remove the last cdr, which might hold the base size.
4. METADATA is optional and defaults to `completion--field-metadata'
   at `field-beginning'."
  (let* ((mdata  (and (fboundp 'completion--field-metadata)
                      (or metadata  (completion--field-metadata (field-beginning)))))
         ;; $$$$$$$$ UNLESS BUG #8795 is fixed, need METADATA even if nil.
         (res    (if (fboundp 'completion--field-metadata) ; Emacs 24 added a 5th arg, METADATA.
                     (completion-all-completions string table pred point mdata)
                   (completion-all-completions string table pred point))))
    (when (consp res)  (let ((last  (last res)))  (when last (setcdr last nil))))
    (let* ((input-sans-dir  (icicle-minibuf-input-sans-dir icicle-current-input))
           (env-var-p       (and (icicle-not-basic-prefix-completion-p)
                                 (> (length input-sans-dir) 0)
                                 (eq ?\$ (aref input-sans-dir 0)))))
      (when env-var-p (setq res  (mapcar (lambda (cand) (concat "$" cand)) res))))
    res))

;; $$$$$$ Filed Emacs BUG #4708.  `completion-try-completion' does not return nil when it should.
;; E.g. (completion-try-completion "c:/some-dir/$HOMj" nil 17) returns: ("c:/some-dir/$$HOMj" . 18)
;;
;; This causes `icicle-highlight-input-noncompletion' not to highlight the `j' in the above example.
;;
;; $$$$$$ Filed Emacs BUG #8795.  They added a non-optional arg, METADATA (with no doc).
;;
(defun icicle-completion-try-completion (string table pred point &optional metadata)
  "Icicles version of `completion-try-completion'.
1. Handle all Emacs versions.
2. Remove the last cdr, which might hold the base size.
3. METADATA is optional and defaults to `completion--field-metadata'
   at `field-beginning'."
  (let* ((mdata  (and (fboundp 'completion--field-metadata)
                      (or metadata  (completion--field-metadata (field-beginning)))))
         ;; $$$$$$$$ UNLESS BUG #8795 is fixed, still need METADATA, even if nil.
         (res    (if (fboundp 'completion--field-metadata) ; Emacs 24 added a 5th arg, METADATA.
                     (completion-try-completion string table pred point mdata)
                   (completion-try-completion string table pred point))))
    (when (consp res) (setq res (car  res)))
    res))

(defun icicle-require-match-p ()
  "Return non-nil if completion is strict.
Return non-nil if current REQUIRE-MATCH arg to `completing-read' or
`read-file-name' really means require match (sheesh!)."
  (if (> emacs-major-version 22) (eq t icicle-require-match-p) icicle-require-match-p))

;; Note: Property `icicle-mode-line-help' with a function value is not used yet in Icicles code.
(defun icicle-candidate-short-help (help string)
  "Put HELP on STRING as text properties.
HELP is a help string or a function that takes a display completion
candidate as argument.  To display help for a candidate, the function
is applied to the candidate to obtain its help string.

Put `help-echo' property if `tooltip-mode' is non-nil.
Put `icicle-mode-line-help' property (on the first character only) if
 `icicle-help-in-mode-line-delay' is positive.
Return STRING, whether propertized or not."
  (unless (equal "" string)
    (when (> icicle-help-in-mode-line-delay 0)
      (put-text-property 0 1 'icicle-mode-line-help help string))
    (when (and (boundp 'tooltip-mode)  tooltip-mode)
      (put-text-property 0 (length string) 'help-echo help string)))
  string)

;; This is not used by Icicles, since the color functions require `hexrgb.el'.
(defun icicle-remove-color-duplicates (list)
  "Copy of LIST with duplicate color candidates removed.
Candidates are considered duplicates if they have the same color name,
abstracting from whitespace and letter case."
  (let ((tail  list)
        new)
    (save-match-data (while tail
                       (let* ((this            (car tail))
                              (pseudo-color-p  (string-match "^\*" this)))
                         (string-match ": " this)
                         (unless pseudo-color-p
                           (setq this  (icicle-delete-whitespace-from-string
                                        (downcase this) 0 (match-beginning 0))))
                         (unless (member this new) (push this new)))
                       (pop tail)))
    (nreverse new)))

(defun icicle-alt-act-fn-for-type (type)
  "Returns an action function chosen by user for type TYPE (a string).
Typical use: Bind `icicle-candidate-alt-action-fn' and
`icicle-all-candidates-list-alt-action-fn' to the return value.
However, you must first bind `icicle-orig-window' to the window that
is current before user input is read from the minibuffer."
  (lexical-let ((type  type))           ; Does this binding really help?
    (lambda (cands)
      (unless (listp cands) (setq cands (list cands))) ; So it works for both single and all cands.
      (let* ((enable-recursive-minibuffers     t)
             (anything-actions                 (and (> emacs-major-version 21)
                                                    icicle-use-anything-candidates-flag
                                                    (require 'anything nil t)
                                                    (icicle-get-anything-actions-for-type (intern type))))
             (actions                   ; Must sort, for `icicle-candidates-alist',
              (sort                     ; or else `icicle-candidate-nb' will be wrong.
               (append anything-actions
                       (mapcar (lambda (act) (cons (format "%s" act) act))
                               (icicle-remove-if-not #'functionp
                                                     (cdr (assoc type icicle-type-actions-alist)))))
               (lambda (a1 a2) (funcall 'string-lessp (car a1) (car a2)))))
             (icicle-sort-comparer             'string-lessp) ; Must be the same order as actions.
             (icicle-candidate-action-fn ; For "how".
              (lambda (fn)
                (let ((icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "function"))
                      icicle-saved-completion-candidate)
                  (icicle-with-selected-window ; A macro in `icicles-mac.el'.
                      (if (and (boundp 'icicle-orig-window)  (window-live-p icicle-orig-window))
                          icicle-orig-window
                        (selected-window)) ; Punt wo `icicle-orig-window'.
                    (dolist (cand  cands)
                      (setq icicle-saved-completion-candidate  cand)
                      (icicle-apply-to-saved-candidate fn t type))))))
             ;; Save & restore these, so `icomplete-exhibit' on `post-command-hook' has no error.
             (minibuffer-completion-table      minibuffer-completion-table)
             (minibuffer-completion-predicate  minibuffer-completion-predicate))

        (setq cands  (mapcar (lambda (obj)
                               (setq obj  (icicle-transform-multi-completion obj))
                               (cond ((not (stringp obj))  obj)
                                     ((memq (intern type)
                                            '(command face function option symbol variable))
                                      (intern obj))
                                     ((and (eq (intern type) 'frame)  (fboundp 'get-a-frame))
                                      (get-a-frame obj))
                                     (t  obj)))
                             cands))
        (setq icicle-candidates-alist  actions)
        (let (icicle-saved-completion-candidate)
          (cond ((null actions)
                 ;; Undefined TYPE - provide all Emacs `functionp' symbol names as candidates.
                 (let* ((icicle-must-pass-after-match-predicate  (lambda (s) (functionp (intern s))))
                        (action                                  (icicle-maybe-cached-action
                                                                  (completing-read "How (action): "
                                                                                   obarray))))
                   (dolist (cand  cands)
                     (setq icicle-saved-completion-candidate  cand)
                     (icicle-apply-to-saved-candidate action))))
                ((null (cdr actions))
                 (dolist (cand  cands)  (funcall (icicle-maybe-cached-action (cdar actions)) cand)))
                (t
                 (let* ((icicle-show-Completions-initially-flag  t)
                        (action                                  (icicle-maybe-cached-action
                                                                  (completing-read "How (action): "
                                                                                   actions))))
                   (icicle-with-selected-window
                       (if (and (boundp 'icicle-orig-window)  (window-live-p icicle-orig-window))
                           icicle-orig-window
                         (selected-window)) ; Punt: no `icicle-orig-window'.
                     (let ((icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "function")))
                       (dolist (cand  cands)
                         (setq icicle-saved-completion-candidate  cand)
                         (icicle-apply-to-saved-candidate action t type))))))))))))

(defun icicle-toggle-icicle-mode-twice ()
  "Toggle Icicle mode twice.  Load `icicles-mode.el' if not loaded."
  ;; Just a convenience function, to avoid Emacs warning about calling `icy-mode' with no arg.
  (require 'icicles-mode)
  (let ((curr  (if (and (boundp 'icicle-mode)  icicle-mode) 1 -1)))
    (icy-mode (- curr))  (icy-mode curr)))

(defun icicle-current-TAB-method ()
  "Current completion method for \
`\\<minibuffer-local-completion-map>\\[icicle-prefix-complete]'.
This resets variable `icicle-current-TAB-method' when needed."
  (or (car (memq icicle-current-TAB-method icicle-TAB-completion-methods))
      (car icicle-TAB-completion-methods)))

(defun icicle-not-basic-prefix-completion-p ()
  "`icicle-current-TAB-method' is `vanilla', and Emacs > release 22."
  (and (eq 'vanilla (icicle-current-TAB-method))  (boundp 'completion-styles)))

(defun icicle-all-completions (string collection &optional predicate hide-spaces)
  "Version of vanilla `all-completions' that works for all Emacs releases.
Starting with Emacs 23.2, `all-completions' no longer accepts a fourth
argument, so we drop that arg in that case."
  (condition-case nil                   ; Emacs 23.2+ has no 4th parameter.
      (all-completions string collection predicate hide-spaces)
    (wrong-number-of-arguments (all-completions string collection predicate))))

(defun icicle-custom-rogue-p (symbol)
  "Return non-nil if SYMBOL value differs from persistent/customized value."
  (let ((cval (or (get symbol 'customized-value) ; User changed it in Customize
                  (get symbol 'saved-value)      ; User saved it
                  (get symbol 'standard-value)))) ; Predefined value
    (and cval
         (default-boundp symbol)        ; Just to be sure.
         (not (equal (eval (car cval)) (default-value symbol))))))

(defun icicle-bounds-of-thing-at-point (thing &optional syntax-table)
  "`thingatpt+.el' version of `bounds-of-thing-at-point', if possible.
`tap-bounds-of-thing-at-point' if defined, else
`bounds-of-thing-at-point'.
if non-nil, set SYNTAX-TABLE for the duration."
  (if (fboundp 'tap-bounds-of-thing-at-point)
      (tap-bounds-of-thing-at-point thing syntax-table)
    (if (and (fboundp 'with-syntax-table)  (syntax-table-p syntax-table)) ; Emacs 21+.
        (with-syntax-table syntax-table (bounds-of-thing-at-point thing syntax-table))
      (bounds-of-thing-at-point thing)))) ; Punt - ignore any SYNTAX-TABLE arg.

(defun icicle-defaults-at-point ()
  "Return default(s) obtained from things at/near point.
For Emacs 23 and later, return a list of defaults (strings).
For Emacs 22 and prior, return a single default (a string)."
  (cond ((car icicle-thing-at-point-functions)
         (if (> emacs-major-version 22)
             (mapcar #'funcall (car icicle-thing-at-point-functions))
           (funcall (caar icicle-thing-at-point-functions))))
        ((fboundp 'non-nil-symbol-name-nearest-point)
         (funcall #'non-nil-symbol-name-nearest-point))
        ((icicle-thing-at-point 'symbol))
        ((function-called-at-point))))

(defun icicle-alist-key-match (regexp alist)
  "Return non-nil if REGEXP matches a key of ALIST.
The non-nil value returned is the first element whose key matches."
  (catch 'icicle-alist-key-match
    (dolist (key.val  alist)
      (when (string-match regexp (car key.val)) (throw 'icicle-alist-key-match key.val)))
    nil))
 
;;(@* "Icicles Functions - Sort Functions")

;;; Icicles Functions - Sort Functions -------------------------------

(defun icicle-merge-saved-order-less-p (s1 s2)
  "String S1 has a lower index than S2 in current and saved candidates list."
  (let ((cs1  (icicle-position s1 icicle-completion-candidates))
        (cs2  (icicle-position s2 icicle-completion-candidates))
        (ss1  (icicle-position s1 icicle-saved-completion-candidates))
        (ss2  (icicle-position s2 icicle-saved-completion-candidates))
        len)
    (unless cs1 (error "`%s' is not currently a candidate" s1))
    (unless cs2 (error "`%s' is not currently a candidate" s2))
    (unless ss1 (setq ss1  (setq len  (length icicle-saved-completion-candidates))))
    (unless ss2 (setq ss2  (or len  (length icicle-saved-completion-candidates))))
    (< (+ cs1 ss1) (+ cs2 ss2))))

(defun icicle-historical-alphabetic-p (s1 s2)
  "Non-nil means S1 is a past input and S2 is not or S1 < S2 (alphabet).
Return non-nil if S1 is a previous input and either S2 is not or
S1 `icicle-case-string-less-p' S2.  S1 and S2 must be strings.

When used as a comparison function for completion candidates, this
makes candidates matching previous inputs available first (at the top
of buffer `*Completions*').  Candidates are effectively in two groups,
each of which is sorted alphabetically separately: matching previous
inputs, followed by matching candidates that have not yet been used."
  ;; We could use `icicle-delete-duplicates' to shorten the history, but that takes time too.
  ;; And, starting in Emacs 22, histories will not contain duplicates anyway.
  (let ((hist  (and (symbolp minibuffer-history-variable)  (boundp minibuffer-history-variable)
                    (symbol-value minibuffer-history-variable)))
        (dir   (and (icicle-file-name-input-p)
                    (icicle-file-name-directory-w-default (or icicle-last-input  icicle-current-input)))))
    (if (not (consp hist))
        (icicle-case-string-less-p s1 s2)
      (when dir (setq s1  (abbreviate-file-name (expand-file-name s1 dir))
                      s2  (abbreviate-file-name (expand-file-name s2 dir))))
      (let ((s1-previous-p  (member s1 hist))
            (s2-previous-p  (member s2 hist)))
        (or (and (not s1-previous-p)  (not s2-previous-p)  (icicle-case-string-less-p s1 s2))
            (and s1-previous-p  (not s2-previous-p))
            (and s1-previous-p  s2-previous-p  (icicle-case-string-less-p s1 s2)))))))

;; $$ Alternative definition, but it doesn't seem any faster, and is slightly less clear.
;; (defun icicle-latest-input-first-p (s1 s2)
;;   "Non-nil means S1 was used more recently than S2.
;; Also:
;;  S1 < S2 if S1 was used previously but S2 was not.
;;  S1 < S2 if neither was used previously
;;   and S1 `icicle-case-string-less-p' S2."
;;   ;; We could use `icicle-delete-duplicates' to shorten the history, but that takes time too.
;;   ;; And, starting in Emacs 22, histories will not contain duplicates anyway.
;;   (let ((hist  (and (symbolp minibuffer-history-variable)
;;                     (symbol-value minibuffer-history-variable)))
;;         (dir   (and (icicle-file-name-input-p)
;;                     (icicle-file-name-directory-w-default (or icicle-last-input  icicle-current-input))))
;;         (s1-in-hist nil)
;;         (s2-in-hist nil))
;;     (if (not (consp hist))
;;         (icicle-case-string-less-p s1 s2)
;;       (when dir (setq s1  (abbreviate-file-name (expand-file-name s1 dir))
;;                       s2  (abbreviate-file-name (expand-file-name s2 dir))))
;;       (while (and hist  (not (setq s1-in-hist  (equal s1 (car hist)))))
;;         (when (setq s2-in-hist  (equal s2 (car hist))) (setq hist  nil))
;;         (setq hist  (cdr hist)))
;;       (or (and hist  s1-in-hist)  (and (not s2-in-hist)  (icicle-case-string-less-p s1 s2))))))

(defun icicle-latest-input-first-p (s1 s2)
  "Non-nil means S1 was used as input more recently than S2.
Also:
 S1 < S2 if S1 was used as input previously but S2 was not.
 S1 < S2 if neither was used as input previously
  and S1 `icicle-case-string-less-p' S2."
  ;; We could use `icicle-delete-duplicates' to shorten the history, but that takes time too.
  ;; And, starting in Emacs 22, histories do not contain duplicates anyway.
  (let ((hist     (and (symbolp minibuffer-history-variable) (boundp minibuffer-history-variable)
                       (symbol-value minibuffer-history-variable)))
        (dir      (and (icicle-file-name-input-p)
                       (icicle-file-name-directory-w-default (or icicle-last-input  icicle-current-input))))
        (s1-tail  ())
        (s2-tail  ()))
    (if (not (consp hist))
        (icicle-case-string-less-p s1 s2)
      (when dir (setq s1  (abbreviate-file-name (expand-file-name s1 dir))
                      s2  (abbreviate-file-name (expand-file-name s2 dir))))
      (setq s1-tail  (member s1 hist)
            s2-tail  (member s2 hist))
      (cond ((and s1-tail  s2-tail)  (>= (length s1-tail) (length s2-tail)))
            (s1-tail                 t)
            (s2-tail                 nil)
            (t                       (icicle-case-string-less-p s1 s2))))))


(put 'icicle-buffer-smaller-p 'icicle-buffer-sort-predicate t)
;; This predicate is used for buffer-name completion.
(defun icicle-buffer-smaller-p (b1 b2)
  "Non-nil means buffer named B1 is smaller than buffer named B2."
  (< (with-current-buffer b1 (buffer-size)) (with-current-buffer b2 (buffer-size))))

;; Emacs 24.3+.  `flx.el' requires `cl-lib.el', for `cl-loop', `cl-incf', `cl-cddar'.
(when (condition-case nil (require 'flx nil t) (error nil))

  (defun icicle-flx-score-greater-p (s1 s2)
    "Non-nil means the `flx-score' of S1 is greater than that of S2.
That is, the cars of the `flx-score' values are compared.

If `flx-score' returns nil for either argument, then they are compared
using `icicle-case-string-less-p'.

This function requires library `flx.el'."
    (let* ((input   (if (and (icicle-file-name-input-p)  insert-default-directory)
                        (file-name-nondirectory icicle-current-input)
                      icicle-current-input))
           (score1  (flx-score s1 input))
           (score2  (flx-score s2 input)))
      (if (and score1  score2)
          (> (car score1) (car score2))
        (icicle-case-string-less-p s1 s2))))
  )

(put 'icicle-major-mode-name-less-p 'icicle-buffer-sort-predicate t)
;; This predicate is used for buffer-name completion.
(defun icicle-major-mode-name-less-p (b1 b2)
  "Non-nil means major mode name of buffer B1 is `string-less-p' that of B2.
If those names are identical, then buffer names are compared.
Comparison is not case-sensitive."
  (let ((bm1  (icicle-upcase (symbol-name (with-current-buffer b1 major-mode))))
        (bm2  (icicle-upcase (symbol-name (with-current-buffer b2 major-mode)))))
    (if (string= bm1 bm2) (string-lessp b1 b2) (string-lessp bm1 bm2))))


(when (fboundp 'format-mode-line)       ; Emacs 22+
  (put 'icicle-mode-line-name-less-p 'icicle-buffer-sort-predicate t)
  ;; This predicate is used for buffer-name completion.
  (defun icicle-mode-line-name-less-p (b1 b2)
    "Non-nil means buffer B1 mode in mode line is `string-less-p' that of B2.
If those names are identical, then buffer names are compared.
Comparison is not case-sensitive."
    (let ((bm1  (icicle-upcase (with-current-buffer b1 (format-mode-line mode-name))))
          (bm2  (icicle-upcase (with-current-buffer b2 (format-mode-line mode-name)))))
      (if (string= bm1 bm2) (string-lessp b1 b2) (string-lessp bm1 bm2)))))


(put 'icicle-buffer-file/process-name-less-p 'icicle-buffer-sort-predicate t)
;; This predicate is used for buffer-name completion.
(defun icicle-buffer-file/process-name-less-p (b1 b2)
  "Non-nil means file/process name of buffer B1 is `string-less-p' that of B2.
The absolute file name of a buffer is used, not the relative name.
Comparison is case-insensitive on systems where file-name case is
 insignificant.

Buffers not associated with files or processes are sorted last."
  (setq b1  (get-buffer b1)
        b2  (get-buffer b2))
  (let ((fp-b1  (or (buffer-file-name b1)  (let ((pb1  (get-buffer-process b1)))
                                             (and (processp pb1)  (process-name pb1)))))
        (fp-b2  (or (buffer-file-name b2)  (let ((pb2  (get-buffer-process b2)))
                                             (and (processp pb2)  (process-name pb2))))))
    (and fp-b1  (or (not fp-b2)
                    (if (memq system-type '(ms-dos windows-nt cygwin))
                        (string-lessp (icicle-upcase fp-b1) (icicle-upcase fp-b2))
                      (string-lessp fp-b1 fp-b2))))))


(put 'icicle-file-type-less-p 'icicle-file-name-sort-predicate t)
;; This predicate is used for file-name completion.
(defun icicle-file-type-less-p (s1 s2)
  "Non-nil means type of file S1 is less than that of S2, or S1 < S2 (alpha).
A directory has a lower file type than a non-directory.
The type of a non-directory is its extension.  Extensions are compared
 alphabetically.
If not doing file-name completion then this is the same as
`icicle-case-string-less-p'.

The directory test is only syntactic: whether it looks like a
directory name"
  (if (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
      (let ((s1-dir-p  (icicle-looks-like-dir-name-p s1))
            (s2-dir-p  (icicle-looks-like-dir-name-p s2)))
        (cond ((and s1-dir-p  s2-dir-p) (icicle-case-string-less-p s1 s2)) ; Both are dirs, so alpha.
              ((not (or s1-dir-p  s2-dir-p)) ; Neither is a dir.  Compare extensions.
               (let ((es1  (file-name-extension s1 t))
                     (es2  (file-name-extension s2 t)))
                 (if (string= es1 es2)  ; If extensions the same, then compare file names.
                     (icicle-case-string-less-p s1 s2)
                   (icicle-case-string-less-p es1 es2))))
              (s1-dir-p)))              ; Directories come before files.
    (icicle-case-string-less-p s1 s2)))


(put 'icicle-dirs-and-latest-use-first-p 'icicle-file-name-sort-predicate t)
;; This predicate is used for file-name completion.
(defun icicle-dirs-and-latest-use-first-p (s1 s2)
  "Non-nil means S1 is a dir and S2 not, or S1 used more recently than S2.
\"Use\" here refers, first, to use as your input, second, to access.

If both S1 and S2 are the same type (dir or file) then:
 S1 < S2 if S1 was used as input previously but S2 was not.
 S1 < S2 if neither was used as input previously and:
  and S1 was accessed more recently than S2.

The directory test is only syntactic: whether it looks like a
directory name"
  (if (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
      (let ((s1-dir-p  (icicle-looks-like-dir-name-p s1))
            (s2-dir-p  (icicle-looks-like-dir-name-p s2)))
        (if (or (and s1-dir-p  s2-dir-p) ; Both or neither are directories.
                (not (or s1-dir-p  s2-dir-p)))
            (icicle-latest-use-first-p s1 s2) ; Compare same type using last use.
          s1-dir-p))                    ; Directories come before files.
    (icicle-latest-use-first-p s1 s2)))

(put 'icicle-dirs-first-p 'icicle-file-name-sort-predicate t)
;; This predicate is used for file-name completion.
(defun icicle-dirs-first-p (s1 s2)
  "Non-nil means S1 is a dir and S2 a file, or S1 < S2 (alphabet).
If not doing file-name completion then this is the same as
`icicle-case-string-less-p'.

The directory test is only syntactic: whether it looks like a
directory name"
  (if (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
      (let ((s1-dir-p  (icicle-looks-like-dir-name-p s1))
            (s2-dir-p  (icicle-looks-like-dir-name-p s2)))
        (if (or (and s1-dir-p  s2-dir-p) ; Both or neither are directories.
                (not (or s1-dir-p  s2-dir-p)))
            (icicle-case-string-less-p s1 s2)  ; Compare equals.
          s1-dir-p))                 ; Directories come before files.
    (icicle-case-string-less-p s1 s2)))


(put 'icicle-dirs-last-p 'icicle-file-name-sort-predicate t)
;; This predicate is used for file-name completion.
(defun icicle-dirs-last-p (s1 s2)
  "Non-nil means S1 is a file and S2 a dir, or S1 < S2 (alphabet).
If not doing file-name completion then this is the same as
`icicle-case-string-less-p'.

The directory test is only syntactic: whether it looks like a
directory name"
  (if (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
      (let ((s1-dir-p  (icicle-looks-like-dir-name-p s1))
            (s2-dir-p  (icicle-looks-like-dir-name-p s2)))
        (if (or (and s1-dir-p  s2-dir-p) ; Both or neither are directories.
                (not (or s1-dir-p  s2-dir-p)))
            (icicle-case-string-less-p s1 s2)  ; Compare equals.
          s2-dir-p))                 ; Files come before directories.
    (icicle-case-string-less-p s1 s2)))


(put 'icicle-2nd-part-string-less-p 'icicle-multi-completion-sort-predicate t)
;; This predicate is used for multi-completion.
(defun icicle-2nd-part-string-less-p (s1 s2)
  "`icicle-case-string-less-p' for second parts, then for first parts.
S1 and S2 are multi-completion strings.
Returns non-nil if either of these is true:

* The second parts of S1 and S2 are the equivalent and the first part
  of S1 comes before the first part of S2, alphabetically.

* The second part of S1 comes before the second part of S2,
  alphabetically.

Alphabetical comparison is done using `icicle-case-string-less-p'."
  (let* ((icicle-list-use-nth-parts  '(2))
         (s1-2nd                     (icicle-transform-multi-completion s1))
         (s2-2nd                     (icicle-transform-multi-completion s2)))
    (or (icicle-case-string-less-p s1-2nd s2-2nd)
        (and (string= s1-2nd s2-2nd)
             (let* ((icicle-list-use-nth-parts  '(1))
                    (s1-1st                     (icicle-transform-multi-completion s1))
                    (s2-1st                     (icicle-transform-multi-completion s2))))))))


(put 'icicle-latest-use-first-p 'icicle-file-name-sort-predicate t)
;; This predicate is used for file-name completion.
(defun icicle-latest-use-first-p (s1 s2)
  "Non-nil means S1 was used more recently than S2.
\"Use\" here refers, first, to use as your input, second, to access.
S1 < S2 if S1 was used as input previously but S2 was not.
S1 < S2 if neither was used as input previously
 and S1 was accessed more recently than S2.

If not doing file-name completion then this is the same as
`icicle-latest-input-first-p'."
  (if (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
      ;; We could use `icicle-delete-duplicates' to shorten the history, but that takes time too.
      ;; And, starting in Emacs 22, histories do not contain duplicates anyway.
      (let ((hist     (and (symbolp minibuffer-history-variable)  (boundp minibuffer-history-variable)
                           (symbol-value minibuffer-history-variable)))
            (dir      (and (icicle-file-name-input-p)
                           (icicle-file-name-directory-w-default (or icicle-last-input  icicle-current-input))))
            (s1-tail  ())
            (s2-tail  ()))
        (if (not (consp hist))
            (icicle-latest-access-first-p s1 s2)
          (when dir (setq s1  (abbreviate-file-name (expand-file-name s1 dir))
                          s2  (abbreviate-file-name (expand-file-name s2 dir))))
          (setq s1-tail  (member s1 hist)
                s2-tail  (member s2 hist))
          (cond ((and s1-tail  s2-tail)  (>= (length s1-tail) (length s2-tail)))
                (s1-tail                 t)
                (s2-tail                 nil)
                (t                       (icicle-latest-access-first-p s1 s2)))))
    (icicle-latest-input-first-p s1 s2)))

(put 'icicle-latest-access-first-p 'icicle-file-name-sort-predicate t)
;; This predicate is used for file-name completion.
(defun icicle-latest-access-first-p (s1 s2)
  "Non-nil means file S1 was last accessed after S2 was.
If not doing file-name completion then this is the same as
`icicle-case-string-less-p'."
  (if (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
      (let ((acc-date1  (nth 4 (file-attributes s1)))
            (acc-date2  (nth 4 (file-attributes s2))))
        (or (< (car acc-date2)  (car acc-date1)) ; High-order bits.
            (and (= (car acc-date2) (car acc-date1)) ; Low-order bits.
                 (< (cadr acc-date2) (cadr acc-date1)))))
    (icicle-case-string-less-p s1 s2)))


(put 'icicle-latest-modification-first-p 'icicle-file-name-sort-predicate t)
;; This predicate is used for file-name completion.
(defun icicle-latest-modification-first-p (s1 s2)
  "Non-nil means file S1 was last modified after S2 was.
If not doing file-name completion then this is the same as
`icicle-case-string-less-p'."
  (if (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
      (let ((mod-date1  (nth 5 (file-attributes s1)))
            (mod-date2  (nth 5 (file-attributes s2))))
        (or (< (car mod-date2)  (car mod-date1)) ; High-order bits.
            (and (= (car mod-date2) (car mod-date1)) ; Low-order bits.
                 (< (cadr mod-date2) (cadr mod-date1)))))
    (icicle-case-string-less-p s1 s2)))


(put 'icicle-command-abbrev-used-more-p 'icicle-command-sort-predicate t)
;; This predicate is used for command and abbreviation completion.
(defun icicle-command-abbrev-used-more-p (s1 s2)
  "Return non-nil if S1 was invoked more often than S2 via an abbrev.
S1 and S2 are strings naming commands.
If neither was invoked or both were invoked the same number of times,
then return non-nil if S1 is `string-lessp' S2."
  (let* ((alist-tails  (mapcar #'cdr icicle-command-abbrev-alist))
         (s1-entry     (assq (intern s1) alist-tails))
         (s2-entry     (assq (intern s2) alist-tails)))
    (if (and (not s1-entry)  (not s2-entry))
        (string-lessp s1 s2)
      (let ((s1-rank  (elt s1-entry 1))
            (s2-rank  (elt s2-entry 1)))
        (cond ((and (not s1-rank)  (not s2-rank))            (string-lessp s1 s2))
              ((and s1-rank  s2-rank  (eq s1-rank s2-rank))  (string-lessp s1 s2))
              (t                                             (>= (or s1-rank  0) (or s2-rank  0))))))))

(defun icicle-part-N-lessp (n s1 s2)
  "`icicle-case-string-less-p' applied to the Nth parts of S1 and S2.
The strings each have at least N parts, separated by
`icicle-list-join-string'.  Parts other than the Nth are ignored.
Return non-nil if and only if the Nth part of S1 is less than the Nth
part of S2.  The Nth parts are compared lexicographically without
regard to letter case.  N is one-based, so a value of 1 means compare
the first parts."
  (unless (and (wholenump n)  (> n 0)) (error "`icicle-part-N-lessp': N must be > 0"))
  (let ((case-fold-search  t)
        (part-N-s1         (elt (split-string s1 icicle-list-join-string) (1- n)))
        (part-N-s2         (elt (split-string s2 icicle-list-join-string) (1- n))))
    (and part-N-s1  part-N-s2           ; In case strings were not multipart.
         (icicle-case-string-less-p part-N-s1 part-N-s2))))

(defun icicle-part-1-lessp (s1 s2)
  "`icicle-part-N-lessp', with N = 1."
  (icicle-part-N-lessp 1 s1 s2))

(defun icicle-part-2-lessp (s1 s2)
  "`icicle-part-N-lessp', with N = 2."
  (icicle-part-N-lessp 2 s1 s2))

(defun icicle-part-3-lessp (s1 s2)
  "`icicle-part-N-lessp', with N = 3."
  (icicle-part-N-lessp 3 s1 s2))

(defun icicle-part-4-lessp (s1 s2)
  "`icicle-part-N-lessp', with N = 4."
  (icicle-part-N-lessp 4 s1 s2))

(defun icicle-cdr-lessp (s1 s2)
  "Non-nil means the cdr of S1's entry < the cdr of S2's entry.
Entry here means the complete alist element candidate that corresponds
to the displayed candidate (string) S1 or S2.
Returns nil if comparing the cdrs using `<' would raise an error."
  (condition-case nil
      (< (cdr (funcall icicle-get-alist-candidate-function s1))
         (cdr (funcall icicle-get-alist-candidate-function s2)))
    (error nil)))

(defun icicle-part-1-cdr-lessp (s1 s2)
  "First part and cdr of S1 are less than those of S2."
  (or (icicle-part-1-lessp s1 s2)
      (and (not (icicle-part-1-lessp s2 s1))  (icicle-cdr-lessp s1 s2))))


;; This predicate is used for color completion.
(defun icicle-color-rgb-lessp (s1 s2)
  "Non-nil means the RGB components of S1 are less than those of S2.
Specifically, the red components are compared first, then if they are
equal the blue components are compared, then if those are also equal
the green components are compared.

The strings are assumed to have at least two parts, with the parts
separated by `icicle-list-join-string' The second parts of the strings
are RGB triplets that start with `#'."
  (icicle-part-2-lessp s1 s2))          ; Just compare lexicographically.

;; This predicate is used for key completion.
(defun icicle-prefix-keys-first-p (s1 s2)
  "Non-nil if S1 is a prefix key and S2 is not or S1 < S2 (alphabet).
For this function, a prefix key is represented by a string that ends
in \"...\".

When used as a comparison function for completion candidates, this
makes prefix keys that match your input available first (at the top of
buffer `*Completions*').  Candidates are effectively in two groups,
each of which is sorted alphabetically separately: prefix keys,
followed by non-prefix keys.  Letter case is ignored.

The special key representation \"..\" is, however, less than all other
keys, including prefix keys."
  (let* ((prefix-string           (concat icicle-complete-keys-separator "\\.\\.\\.$"))
         (parent-string           "..")
         (s1-prefix-p             (icicle-string-match-p prefix-string s1))
         (s2-prefix-p             (icicle-string-match-p prefix-string s2))
         (completion-ignore-case  t))
    (and (not (string= parent-string s2))
         (or (string= parent-string s1)
             (and (not s1-prefix-p)  (not s2-prefix-p)  (icicle-case-string-less-p s1 s2))
             (and s1-prefix-p  (not s2-prefix-p))
             (and s1-prefix-p  s2-prefix-p  (icicle-case-string-less-p s1 s2))))))

;; This predicate is used for key completion.
(defun icicle-local-keys-first-p (s1 s2)
  "Non-nil if S1 is a local key and S2 is not or S1 < S2 (alphabet).
For this function, a local key is highlighted as a special candidate.

When used as a comparison function for completion candidates, this
makes local keys that match your input available first (at the top of
buffer `*Completions*').  Candidates are effectively in two groups,
each of which is sorted alphabetically separately: local keys,
followed by non-prefix keys.  Letter case is ignored.

The special key representation \"..\" is, however, less than all other
keys, including local keys."
  (or (string= ".." s1)
      (and (not (string= ".." s2))  (icicle-special-candidates-first-p s1 s2))))

;; This predicate is used for key completion.
(defun icicle-command-names-alphabetic-p (s1 s2)
  "Non-nil if command name of S1 `icicle-case-string-less-p' that of S2.
When used as a comparison function for completion candidates, this
assumes that each candidate, S1 and S2, is composed of a key name
followed by the value of `icicle-complete-keys-separator', followed by
the corresponding command name."
  (let ((icicle-list-join-string  icicle-complete-keys-separator)) ; Fake a multi-completion.
    (icicle-part-2-lessp s1 s2)))

(defun icicle-special-candidates-first-p (s1 s2)
  "Non-nil if S1 is special candidate and S2 is not or S1<S2 (alphabet).
That is, S1 < S2 if S1 is a special candidate and S2 is not or S1
`icicle-case-string-less-p' S2 and either both or neither are special
candidates."
  (let ((s1-special  (icicle-special-candidate-p s1))
        (s2-special  (icicle-special-candidate-p s2)))
    (when (or case-fold-search
              completion-ignore-case
              ;; Don't bother with buffer completion and `read-buffer-completion-ignore-case'.
              (and (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
                   (boundp 'read-file-name-completion-ignore-case)
                   read-file-name-completion-ignore-case))
      (setq s1  (icicle-upcase s1)
            s2  (icicle-upcase s2)))
    (or (and s1-special  (not s2-special))
        (and (not s1-special)  (not s2-special)  (icicle-case-string-less-p s1 s2))
        (and      s1-special        s2-special   (icicle-case-string-less-p s1 s2)))))

(defun icicle-extra-candidates-first-p (s1 s2)
  "Non-nil if S1 is an extra candidate and S2 is not or S1<S2 (alphabet).
That is, S1 < S2 if S1 is an extra candidate and S2 is not or S1
`icicle-case-string-less-p' S2 and either both or neither are extra
candidates.  An extra candidate is one that is a member of
`icicle-extra-candidates'."
  (let ((s1-extra  (member s1 icicle-extra-candidates))
        (s2-extra  (member s2 icicle-extra-candidates)))
    (when (or case-fold-search
              completion-ignore-case
              ;; Don't bother with buffer completion and `read-buffer-completion-ignore-case'.
              (and (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
                   (boundp 'read-file-name-completion-ignore-case)
                   read-file-name-completion-ignore-case))
      (setq s1  (icicle-upcase s1)
            s2  (icicle-upcase s2)))
    (or (and s1-extra  (not s2-extra))
        (and s1-extra  s2-extra  (icicle-case-string-less-p s1 s2))
        (and (not s1-extra)  (not s2-extra)  (icicle-case-string-less-p s1 s2)))))

(put 'icicle-proxy-candidate-first-p 'icicle-proxy-sort-predicate t)
;; This predicate is used when there are proxy candidates.
(defun icicle-proxy-candidate-first-p (s1 s2)
  "Return non-nil if S1 is a proxy candidate and S2 is not.
Return nil if S2 is a proxy candidate and S1 is not.
Otherwise, return non-nil if S1 is `string-lessp' S2."
  (let ((s1-proxy-p  (or (member s1 icicle-proxy-candidates)
                         (and icicle-proxy-candidate-regexp
                              (icicle-string-match-p icicle-proxy-candidate-regexp s1))))
        (s2-proxy-p  (or (member s2 icicle-proxy-candidates)
                         (and icicle-proxy-candidate-regexp
                              (icicle-string-match-p icicle-proxy-candidate-regexp s2)))))
    (or (and (not s1-proxy-p)  (not s2-proxy-p)  (icicle-case-string-less-p s1 s2))
        (and s1-proxy-p  (not s2-proxy-p))
        (and s1-proxy-p  s2-proxy-p  (icicle-case-string-less-p s1 s2)))))


(defun icicle-case-insensitive-string-less-p (string1 string2)
  "Like `string-lessp', but case is ignored, so `A' = `a' , and so on."
  (string-lessp (icicle-upcase string1) (icicle-upcase string2)))

(defun icicle-case-string-less-p (s1 s2)
  "Like `string-lessp', but respects `completion-ignore-case'."
  (when (if icicle-completing-p         ; Use var, not fn, `icicle-completing-p', or else too slow.
            ;; Don't bother with buffer completion and `read-buffer-completion-ignore-case'.
            (if (and (or (icicle-file-name-input-p)  icicle-abs-file-candidates)
                     (boundp 'read-file-name-completion-ignore-case))
                read-file-name-completion-ignore-case
              completion-ignore-case)
          case-fold-search)
    (setq s1  (icicle-upcase s1)
          s2  (icicle-upcase s2)))
  ;;     (when completion-ignore-case ; Alternative.
  ;;       (setq s1  (icicle-upcase s1)   s2  (icicle-upcase s2)))
  (string-lessp s1 s2))

(defun icicle-upcase (string)
  "`upcase', but in case of error, return original STRING.
This works around an Emacs 20 problem that occurs if STRING contains
binary data (weird chars)."
  ;; E.g. Emacs 20 for plist of `dired-revert' put through (format "%s").
  (condition-case nil (upcase string) (error string)))

(defun icicle-get-safe (object property)
  "If OBJECT is a symbol, `get' its PROPERTY value.  Else return nil."
  (and (symbolp object)  (get object property)))
 
;;(@* "Icicles Predicates for Different Candidate Types")

;;; Icicles Predicates for Different Candidate Types -----------------


;;(@* "Bookmark-Completion Predicates")
;;  ** Bookmark-Completion Predicates **

(when (require 'bookmark+ nil t)

  (defun icicle-bookmark-annotated-p (bookmark)
    "Return non-nil if BOOKMARK has an annotation.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name.
In this case, the second part is tested."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-annotated-bookmark-p bookmark))

  (defun icicle-bookmark-autofile-p (bookmark)
    "Return non-nil if BOOKMARK is an autofile bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name.
In this case, the second part is tested."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-autofile-bookmark-p bookmark))

  (defun icicle-bookmark-autonamed-p (bookmark)
    "Return non-nil if BOOKMARK is an autonamed bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-autonamed-bookmark-p bookmark))

  (defun icicle-bookmark-autonamed-this-buffer-p (bookmark)
    "Return non-nil if BOOKMARK is an autonamed bookmark for this buffer.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (with-current-buffer icicle-orig-buff ; FREE here: ICICLE-ORIG-BUFF.
      (bmkp-autonamed-this-buffer-bookmark-p bookmark)))

  (defun icicle-bookmark-bookmark-file-p (bookmark)
    "Return non-nil if BOOKMARK is a bookmark-file bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-bookmark-file-bookmark-p bookmark))

  (defun icicle-bookmark-bookmark-list-p (bookmark)
    "Return non-nil if BOOKMARK is a bookmark-list bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-bookmark-list-bookmark-p bookmark))

  (defun icicle-bookmark-desktop-p (bookmark)
    "Return non-nil if BOOKMARK is a desktop bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-desktop-bookmark-p bookmark))

  (defun icicle-bookmark-dired-p (bookmark)
    "Return non-nil if BOOKMARK is a Dired bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-dired-bookmark-p bookmark))

  (defun icicle-bookmark-dired-this-dir-p (bookmark)
    "Return non-nil if BOOKMARK is a Dired bookmark for this buffer.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (with-current-buffer icicle-orig-buff ; FREE here: ICICLE-ORIG-BUFF.
      (bmkp-dired-this-dir-bookmark-p bookmark)))

  (defun icicle-bookmark-dired-wildcards-p (bookmark)
    "Return non-nil if BOOKMARK bookmarks a Dired buffer with wildcards.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-dired-wildcards-bookmark-p bookmark))

  (defun icicle-bookmark-file-p (bookmark)
    "Return non-nil if BOOKMARK is a file bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-file-bookmark-p bookmark))

  (defun icicle-bookmark-file-this-dir-p (bookmark)
    "Return non-nil if BOOKMARK is a file bookmark for this directory.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (with-current-buffer icicle-orig-buff ; FREE here: ICICLE-ORIG-BUFF.
      (bmkp-file-this-dir-bookmark-p bookmark)))

  (defun icicle-bookmark-flagged-p (bookmark)
    "Return non-nil if BOOKMARK is flagged for deletion in `*Bookmark List*'.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-flagged-bookmark-p bookmark))

  (defun icicle-bookmark-function-p (bookmark)
    "Return non-nil if BOOKMARK is a function bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-function-bookmark-p bookmark))

  (defun icicle-bookmark-gnus-p (bookmark)
    "Return non-nil if BOOKMARK is a Gnus bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-gnus-bookmark-p bookmark))

  (defun icicle-bookmark-icicle-search-hits-p (bookmark)
    "Return non-nil if BOOKMARK records a list of Icicles search hits.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-icicle-search-hits-bookmark-p bookmark))

  (defun icicle-bookmark-image-p (bookmark)
    "Return non-nil if BOOKMARK is an image-file bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-image-bookmark-p bookmark))

  (defun icicle-bookmark-info-p (bookmark)
    "Return non-nil if BOOKMARK is an Info bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-info-bookmark-p bookmark))

;;;;   (defun icicle-bookmark-last-specific-buffer-p (bookmark)
;;;;     "Return non-nil if BOOKMARK's buffer is `bmkp-last-specific-buffer'.
;;;; That is the buffer last used by command `bmkp-this-buffer-bmenu-list'
;;;; to list bookmarks for a specific buffer in `*Bookmark List*'.

;;;; If BOOKMARK is a cons with a string car, then the car is used as the
;;;; effective argument.  This is so that the function can be used to
;;;; filter completion candidates.  The string can be a multi-completion
;;;; whose first part is the bookmark name."
;;;;     (when (consp bookmark) (setq bookmark  (car bookmark)))
;;;;     (when icicle-multi-completing-p
;;;;       (let ((icicle-list-use-nth-parts  '(1)))
;;;;         (setq bookmark  (icicle-transform-multi-completion bookmark))))
;;;;     (bmkp-last-specific-buffer-p bookmark))

;;;;   (defun icicle-bookmark-last-specific-file-p (bookmark)
;;;;     "Return non-nil if BOOKMARK's file is `bmkp-last-specific-file'.
;;;; That is the file last used by command `bmkp-this-file-bmenu-list' to
;;;; list bookmarks for a specific file in `*Bookmark List*'.

;;;; If BOOKMARK is a cons with a string car, then the car is used as
;;;; the effective argument.  This is so that the function can be used to
;;;; filter completion candidates.  The string can be a multi-completion
;;;; whose first part is the bookmark name."
;;;;     (when (consp bookmark) (setq bookmark  (car bookmark)))
;;;;     (when icicle-multi-completing-p
;;;;       (let ((icicle-list-use-nth-parts  '(1)))
;;;;         (setq bookmark  (icicle-transform-multi-completion bookmark))))
;;;;     (bmkp-last-specific-file-p bookmark))

  (when (require 'bookmark+-lit nil t)
    (defun icicle-bookmark-lighted-p (bookmark)
      "Return non-nil if BOOKMARK is a highlighted bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
      (when (consp bookmark) (setq bookmark  (car bookmark)))
      (when icicle-multi-completing-p
        (let ((icicle-list-use-nth-parts  '(1)))
          (setq bookmark  (icicle-transform-multi-completion bookmark))))
      (bmkp-lighted-p bookmark)))

  (defun icicle-bookmark-local-directory-p (bookmark)
    "Return non-nil if BOOKMARK is a local-directory bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-local-directory-bookmark-p bookmark))

  (defun icicle-bookmark-local-file-p (bookmark)
    "Return non-nil if BOOKMARK is a local-file bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-local-file-bookmark-p bookmark))

  (defun icicle-bookmark-man-p (bookmark)
    "Return non-nil if BOOKMARK is a `man'-page bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-man-bookmark-p bookmark))

  (defun icicle-bookmark-marked-p (bookmark)
    "Return non-nil if BOOKMARK is marked in `*Bookmark List*'.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-marked-bookmark-p bookmark))

  (defun icicle-bookmark-modified-p (bookmark)
    "Return non-nil if BOOKMARK is a modified (unsaved) bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-modified-bookmark-p bookmark))

  (defun icicle-bookmark-navlist-p (bookmark)
    "Return non-nil if BOOKMARK is in the bookmark navigation list.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-navlist-bookmark-p bookmark))

  (defun icicle-bookmark-non-dir-file-p (bookmark)
    "Return non-nil if BOOKMARK is a non-directory file bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-non-dir-file-bookmark-p bookmark))

  (defun icicle-bookmark-non-file-p (bookmark)
    "Return non-nil if BOOKMARK is a non-file bookmark (e.g `*scratch*').
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-non-file-bookmark-p bookmark))

  (defun icicle-bookmark-omitted-p (bookmark)
    "Return non-nil if BOOKMARK is omitted in `*Bookmark List*'.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-omitted-bookmark-p bookmark))

  (defun icicle-bookmark-orphaned-file-p (bookmark)
    "Return non-nil if BOOKMARK is an orphaned-file bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-orphaned-file-bookmark-p bookmark))

  (defun icicle-bookmark-orphaned-local-file-p (bookmark)
    "Return non-nil if BOOKMARK is a orphaned-local-file bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-orphaned-local-file-bookmark-p bookmark))

  (defun icicle-bookmark-orphaned-remote-file-p (bookmark)
    "Return non-nil if BOOKMARK is a orphaned-remote-file bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-orphaned-remote-file-bookmark-p bookmark))

  (defun icicle-bookmark-region-p (bookmark)
    "Return non-nil if BOOKMARK has region information.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-region-bookmark-p bookmark))

  (defun icicle-bookmark-remote-file-p (bookmark)
    "Return non-nil if BOOKMARK is a remote-file bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-remote-file-bookmark-p bookmark))

  (defun icicle-bookmark-sequence-p (bookmark)
    "Return non-nil if BOOKMARK is a sequence (composite) bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-sequence-bookmark-p bookmark))

  (defun icicle-bookmark-snippet-p (bookmark)
    "Return non-nil if BOOKMARK is a snippet bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-snippet-bookmark-p bookmark))

  (defun icicle-bookmark-tagged-p (bookmark)
    "Return non-nil if BOOKMARK is a tagged bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-tagged-bookmark-p bookmark))

  (defun icicle-bookmark-temporary-p (bookmark)
    "Return non-nil if BOOKMARK is a temporary bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-temporary-bookmark-p bookmark))

  (defun icicle-bookmark-this-buffer-p (bookmark)
    "Return non-nil if BOOKMARK is a bookmark for this buffer.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (with-current-buffer icicle-orig-buff ; FREE here: ICICLE-ORIG-BUFF.
      (bmkp-this-buffer-p bookmark)))

;;;   (defun icicle-bookmark-this-file-p (bookmark)
;;;     "Return non-nil if BOOKMARK is a bookmark for this file.
;;; If BOOKMARK is a cons with a string car, then the car is used as
;;; the effective argument.  This is so that the function can be used to
;;; filter completion candidates.  The string can be a multi-completion
;;; whose first part is the bookmark name."
;;;     (when (consp bookmark) (setq bookmark  (car bookmark)))
;;;     (when icicle-multi-completing-p
;;;       (let ((icicle-list-use-nth-parts  '(1)))
;;;         (setq bookmark  (icicle-transform-multi-completion bookmark))))
;;;     (with-current-buffer icicle-orig-buff ; FREE here: ICICLE-ORIG-BUFF.
;;;       (bmkp-this-file-p bookmark)))

  (defun icicle-bookmark-url-p (bookmark)
    "Return non-nil if BOOKMARK is a URL bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-url-bookmark-p bookmark))

  (defun icicle-bookmark-url-browse-p (bookmark)
    "Return non-nil if BOOKMARK is a `browse-url' bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-url-browse-bookmark-p bookmark))

  (defun icicle-bookmark-variable-list-p (bookmark)
    "Return non-nil if BOOKMARK is a variable-list bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-variable-list-bookmark-p bookmark))

  (defun icicle-bookmark-w3m-p (bookmark)
    "Return non-nil if BOOKMARK is a W3M bookmark.
If BOOKMARK is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
whose first part is the bookmark name."
    (when (consp bookmark) (setq bookmark  (car bookmark)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(1)))
        (setq bookmark  (icicle-transform-multi-completion bookmark))))
    (bmkp-w3m-bookmark-p bookmark))

  )


;;(@* "Buffer-Completion Predicates")
;;  ** Buffer-Completion Predicates **

(when (fboundp 'interesting-buffer-p)   ; In `misc-cmds.el'.
  (defun icicle-interesting-buffer-p (buffer-or-name)
    "Return non-nil if BUFFER-OR-NAME is or names an interesting buffer.
This means that the buffer is live and its name does not start with a
space.

If BUFFER-OR-NAME is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates."
    (when (consp buffer-or-name) (setq buffer-or-name  (car buffer-or-name)))
    (when (stringp buffer-or-name) (setq buffer-or-name  (get-buffer buffer-or-name)))
    (interesting-buffer-p buffer-or-name)))

(when (fboundp 'next-error-buffer-p)    ; Emacs 22+
  (defun icicle-next-error-buffer-p (buffer-or-name &optional avoid-current
                                     extra-test-inclusive extra-test-exclusive)
    "Return non-nil if BUFFER-OR-NAME is or names a `next-error' buffer.
If BUFFER-OR-NAME is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates.

If AVOID-CURRENT is non-nil, treat the current buffer only as an
absolute last resort.

EXTRA-TEST-INCLUSIVE, if non-nil, is a function that is called in each
buffer that normally would not qualify.  If it returns non-nil then
so does `next-error-buffer-p'.

EXTRA-TEST-EXCLUSIVE, if non-nil, is a function that is called in each
buffer that normally would qualify.  If it returns nil then so does
`next-error-buffer-p'."
    (when (consp buffer-or-name) (setq buffer-or-name  (car buffer-or-name)))
    (when (stringp buffer-or-name) (setq buffer-or-name  (get-buffer buffer-or-name)))
    (next-error-buffer-p buffer-or-name avoid-current extra-test-inclusive extra-test-exclusive)))

(defun icicle-compilation-buffer-p (buffer-or-name)
  "Return non-nil if BUFFER-OR-NAME is or names a compilation buffer.
If BUFFER-OR-NAME is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates."
  (when (consp buffer-or-name) (setq buffer-or-name  (car buffer-or-name)))
  (when (stringp buffer-or-name) (setq buffer-or-name  (get-buffer buffer-or-name)))
  (compilation-buffer-p buffer-or-name))

(defun icicle-special-display-p (buffer-name)
  "Return non-nil if a buffer named BUFFER-NAME gets a special frame.
More precisely, return t if `special-display-buffer-names' or
`special-display-regexps' contains a string entry equaling or matching
BUFFER-NAME, respectively.  If `special-display-buffer-names' or
`special-display-regexps' contains a list entry whose car equals or
matches BUFFER-NAME, the return value is the cdr of that entry.

If BUFFER-NAME is a cons with a string car, then the car is used as
the effective argument.  This is so that the function can be used to
filter completion candidates."
  (when (consp buffer-name) (setq buffer-name  (car buffer-name)))
  (special-display-p buffer-name))

(defun icicle-buffer-modified-p (buffer-or-name)
  "Return non-nil if BUFFER-OR-NAME is or names a buffer that is modified.
Same as `buffer-modified-p' except that it returns non-nil if the
argument is a string naming a modified buffer.  If the argument is a
cons with a string car, then the car is used as the effective
argument.  This is so that the function can be used to filter
completion candidates."
  (when (consp buffer-or-name) (setq buffer-or-name  (car buffer-or-name)))
  (when (stringp buffer-or-name) (setq buffer-or-name  (get-buffer buffer-or-name)))
  (and buffer-or-name  (buffer-modified-p buffer-or-name)))

;;(@* "Color-Completion Predicates")
;;  ** Color-Completion Predicates **

(when (fboundp 'color-gray-p)        ; Emacs 22+
  (defun icicle-color-gray-p (color &optional frame)
    "Return non-nil if COLOR is a shade of gray (or white or black).
See `icicle-color-defined-p' for COLOR.
FRAME specifies the frame and thus the display for interpreting COLOR.
If FRAME is nil or omitted, use the selected frame."
    (when (consp color) (setq color  (car color)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(2)))
        (setq color  (icicle-transform-multi-completion color))))
    (color-gray-p color frame)))

(when (fboundp 'color-supported-p)        ; Emacs 22+
  (defun icicle-color-supported-p (color &optional frame background-p)
    "Return non-nil if COLOR can be displayed on FRAME.
See `icicle-color-defined-p' for COLOR.
BACKGROUND-P non-nil means COLOR is used as a background.
Otherwise, this function tells whether it can be used as a foreground.
If FRAME is nil or omitted, use the selected frame."
    (when (consp color) (setq color  (car color)))
    (when icicle-multi-completing-p
      (let ((icicle-list-use-nth-parts  '(2)))
        (setq color  (icicle-transform-multi-completion color))))
    (color-supported-p color frame background-p)))

;;(@* "Face-Completion Predicates")
;;  ** Face-Completion Predicates **

(defun icicle-face-bold-p (face &optional frame inherit)
  "Return non-nil if the font of FACE is bold on FRAME.
If FACE is a cons with a string car, then the car is used as the
name of the face.  This is so that the function can be used to
filter completion candidates.  The string can be a multi-completion
as produced by `icicle-make-face-candidate'.

If optional argument FRAME is given, report on FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame.
Optional argument INHERIT is passed to `face-attribute'.
Use `face-attribute' for finer control."
  (when (consp face) (setq face  (car face)))
  (when (and icicle-multi-completing-p  (stringp face))
    (let ((icicle-list-use-nth-parts  '(1)))
      (setq face  (icicle-transform-multi-completion face))))
  (when (stringp face) (setq face  (intern face)))
  (if (> emacs-major-version 21)
      (face-bold-p face frame inherit)
    (face-bold-p face frame)))

(defun icicle-face-differs-from-default-p (face &optional frame)
  "Return non-nil if FACE displays differently from the default face.
See `icicle-face-bold-p' for FACE.
If optional argument FRAME is given, report on FACE in that frame.
If FRAME is t, report on the defaults for FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (when (consp face) (setq face  (car face)))
  (when (and icicle-multi-completing-p  (stringp face))
    (let ((icicle-list-use-nth-parts  '(1)))
      (setq face  (icicle-transform-multi-completion face))))
  (when (stringp face) (setq face  (intern face)))
  (face-differs-from-default-p face frame))

(defun icicle-face-inverse-video-p (face &optional frame inherit)
  "Return non-nil if FACE specifies a non-nil inverse-video.
See `icicle-face-bold-p' for arguments."
  (when (consp face) (setq face  (car face)))
  (when (and icicle-multi-completing-p  (stringp face))
    (let ((icicle-list-use-nth-parts  '(1)))
      (setq face  (icicle-transform-multi-completion face))))
  (when (stringp face) (setq face  (intern face)))
  (if (> emacs-major-version 21)
      (face-inverse-video-p face frame inherit)
    (face-inverse-video-p face frame)))

(defun icicle-face-italic-p (face &optional frame inherit)
  "Return non-nil if the font of FACE is italic on FRAME.
See `icicle-face-bold-p' for arguments."
  (when (consp face) (setq face  (car face)))
  (when (and icicle-multi-completing-p  (stringp face))
    (let ((icicle-list-use-nth-parts  '(1)))
      (setq face  (icicle-transform-multi-completion face))))
  (when (stringp face) (setq face  (intern face)))
  (if (> emacs-major-version 21)
      (face-italic-p face frame inherit)
    (face-italic-p face frame)))

(defun icicle-face-nontrivial-p (face &optional frame)
  "Return non-nil if FACE has some non-nil attribute.
See `icicle-face-bold-p' for FACE.
If optional argument FRAME is given, report on FACE in that frame.
If FRAME is t, report on the defaults for FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (when (consp face) (setq face  (car face)))
  (when (and icicle-multi-completing-p  (stringp face))
    (let ((icicle-list-use-nth-parts  '(1)))
      (setq face  (icicle-transform-multi-completion face))))
  (when (stringp face) (setq face  (intern face)))
  (face-differs-from-default-p face frame))

(defun icicle-face-underline-p (face &optional frame inherit)
  "Return non-nil if the font of FACE specifies non-nil underlining.
See `icicle-face-bold-p' for arguments."
  (when (consp face) (setq face  (car face)))
  (when (and icicle-multi-completing-p  (stringp face))
    (let ((icicle-list-use-nth-parts  '(1)))
      (setq face  (icicle-transform-multi-completion face))))
  (when (stringp face) (setq face  (intern face)))
  (if (> emacs-major-version 21)
      (face-underline-p face frame inherit)
    (face-underline-p face frame)))

;;(@* "File- and Directory-Completion Predicates")
;;  ** File- and Directory-Completion Predicates **

(defun icicle-file-accessible-directory-p (file-or-dir)
  "Return non-nil if FILE-OR-DIR names a directory you can open.
For the value to be non-nil, FILE-OR-DIR must specify the name of a
directory as a file, and the directory must let you open files in it.
In order to use a directory as the current directory of a buffer, this
predicate must return non-nil.  A directory-name spec may be given
instead; then the value is non-nil if the directory so specified
exists and really is a readable and searchable directory.

FILE-OR-DIR is normally a string, but it can also be a cons whose car
is a string.  This is so that the function can be used to filter
absolute file-name completion candidates."
  (when (consp file-or-dir) (setq file-or-dir  (car file-or-dir)))
  (file-accessible-directory-p file-or-dir))

;; Similar to `bmkp-desktop-file-p' in `bookmark+-1.el'.
;; This is better than using `find-file-noselect', which visits the file and leaves its buffer.
(defun icicle-file-desktop-p (filename)
  "Return non-nil if FILENAME names a desktop file.
FILENAME is normally a string, but it can also be a cons whose car is
a string.  This is so that the function can be used to filter absolute
file-name completion candidates."
  (when (consp filename) (setq filename  (car filename)))
  (and (stringp filename)
       (file-readable-p filename)
       (not (file-directory-p filename))
       (with-temp-buffer
         (insert-file-contents-literally filename nil 0 1000)
         (goto-char (point-min))
         (and (zerop (forward-line 2))
              (icicle-looking-at-p "^;; Desktop File for Emacs"))))) ; No $, because maybe eol chars (e.g. ^M).

(defun icicle-file-directory-p (file-or-dir)
  "Return t if FILE-OR-DIR names an existing directory.
Symbolic links to directories count as directories.
See `file-symlink-p' to distinguish symlinks.

FILE-OR-DIR is normally a string, but it can also be a cons whose car
is a string.  This is so that the function can be used to filter
absolute file-name completion candidates."
  (when (consp file-or-dir) (setq file-or-dir  (car file-or-dir)))
  (file-directory-p file-or-dir))

(defun icicle-looks-like-dir-name-p (file-or-dir)
  "Return non-nil if FILE-OR-DIR looks like a directory name.
If FILE-OR-DIR is not a string, return nil.  Otherwise, FILE-OR-DIR
can be an absolute or a relative file name.

This compares FILE-OR-DIR with the directory part of its name, or with
`default-directory' if there is no directory part.

This does not do the file-handler processing that `file-directory-p'
does, so it is not a replacement for that function.  And unlike
`file-directory-p', this returns non-nil for an argument like
\"~/foo//usr/\"."
  (when (consp file-or-dir) (setq file-or-dir  (car file-or-dir)))
  (and (stringp file-or-dir)  (string= file-or-dir (icicle-file-name-directory-w-default file-or-dir))))

;; Same as `dired-nondirectory-p', except this accepts also a cons.
(defun icicle-nondirectory-p (file-or-dir)
  "Return non-nil if FILE-OR-DIR does not name directory.
Symbolic links to directories count as directories.
See `file-symlink-p' to distinguish symlinks.

FILE-OR-DIR is normally a string, but it can also be a cons whose car
is a string.  This is so that the function can be used to filter
absolute file-name completion candidates."
  (when (consp file-or-dir) (setq file-or-dir  (car file-or-dir)))
  (not (file-directory-p file-or-dir)))

(defun icicle-file-compressed-p (filename)
  "Return non-nil if FILENAME names a compressed file.
FILENAME is normally a string, but it can also be a cons whose car is
a string.  This is so that the function can be used to filter absolute
file-name completion candidates."
  (when (consp filename) (setq filename  (car filename)))
  (and (require 'jka-compr nil t)  (icicle-string-match-p (jka-compr-build-file-regexp) filename)))

(when (fboundp 'ffap-file-remote-p)     ; In `ffap.el'
  (defun icicle-ffap-file-remote-p (filename)
    "Return non-nil if FILENAME looks like it names a remote file.
The non-nil value is FILENAME (maybe slightly improved)."
    (when (consp filename) (setq filename  (car filename)))
    (ffap-file-remote-p filename))

  (defun icicle-ffap-url-p (url)
    "Return non-nil if URL looks like a URL.
The non-nil value is URL (maybe slightly improved)."
    (when (consp url) (setq url  (car url)))
    (ffap-url-p url)))

(when (fboundp 'recentf-include-p)
  (defun icicle-recentf-include-p (filename)
    "Return non-nil if FILENAME is not to be excluded by `recentf-exclude'.
FILENAME is normally a string, but it can also be a cons whose car
is a string.  This is so that the function can be used to filter
absolute file-name completion candidates."
    (when (consp filename) (setq filename  (car filename)))
    (recentf-include-p filename)))

(when (fboundp 'recentf-keep-p)         ; In `recentf.el'
  (defun icicle-recentf-keep-p (filename)
    "Return non-nil if FILENAME is to be kept, according to `recentf-keep'.
FILENAME is normally a string, but it can also be a cons whose car
is a string.  This is so that the function can be used to filter
absolute file-name completion candidates."
    (when (consp filename) (setq filename  (car filename)))
    (recentf-keep-p filename)))

(defun icicle-file-elc-p (file)
  "Return non-nil if FILE has extension `elc'.
Usually this means that FILE is an Emacs-Lisp byte-compiled file.
FILE is normally a string, but it can also be a cons whose car is a
string.  This is so that the function can be used to filter absolute
file-name completion candidates."
  (when (consp file) (setq file  (car file)))
  (equal "elc" (file-name-extension file)))

(defun icicle-file-executable-p (file-or-dir)
  "Return non-nil if FILE-OR-DIR can be executed by you.
For a directory, this means you can access files in that directory.
\(It is generally better to use `file-accessible-directory-p' for that
purpose, however.)

FILE-OR-DIR is normally a string, but it can also be a cons whose car
is a string.  This is so that the function can be used to filter
absolute file-name completion candidates."
  (when (consp file-or-dir) (setq file-or-dir  (car file-or-dir)))
  (file-executable-p file-or-dir))

(defun icicle-file-exists-p (file)
  "Return non-nil if FILE exists (whether or not you can read it).
FILE is normally a string, but it can also be a cons whose car is a
string.  This is so that the function can be used to filter absolute
file-name completion candidates."
  (when (consp file) (setq file  (car file)))
  (file-exists-p file))

(defun icicle-file-locked-p (file)
  "Return a value indicating whether FILE is locked.
The value is nil if FILE is not locked, t if it is locked by you, else
a string saying which user has locked it.

FILE is normally a string, but it can also be a cons whose car is a
string.  This is so that the function can be used to filter absolute
file-name completion candidates."
  (when (consp file) (setq file  (car file)))
  (file-locked-p file))

(defun icicle-file-name-absolute-p (filename)
  "Return non-nil if FILENAME specifies an absolute file or directory name.
On Unix, this is a name starting with `/' or `~'.
FILENAME is normally a string, but it can also be a cons whose car is
a string.  This is so that the function can be used to filter absolute
file-name completion candidates."
  (when (consp filename) (setq filename  (car filename)))
  (file-name-absolute-p filename))

;; NOTE: This is *not* a general substitute for `file-readable-p'.
(defun icicle-file-readable-p (filename)
  "Return non-nil if FILENAME names a readable file.
Return nil if FILENAME is \"\" or it names a directory.

FILENAME is normally a string, but it can also be a cons whose car is
a string.  This is so that the function can be used to filter absolute
file-name completion candidates."
  (when (consp filename) (setq filename  (car filename)))
  (and (not (string= "" filename))  (file-readable-p filename)  (not (file-directory-p filename))))

(defun icicle-file-regular-p (filename)
  "Return non-nil if FILENAME names a regular file.
This is the sort of file that holds an ordinary stream of data bytes.
Symbolic links to regular files count as regular files.
See `file-symlink-p' to distinguish symlinks.

FILENAME is normally a string, but it can also be a cons whose car is
a string.  This is so that the function can be used to filter absolute
file-name completion candidates."
  (when (consp filename) (setq filename  (car filename)))
  (file-regular-p filename))


;; NOTE: This is *not* a general substitute for `file-remote-p'.
(defun icicle-file-remote-p (filename)
  "Non-nil means FILENAME is likely to name a remote file or directory.
For MS Windows, if `icicle-network-drive-means-remote-flag' is non-nil
then this includes a file on a mapped network drive.

Otherwise, use `file-remote-p' if defined, or return nil if not.

FILENAME is normally a string, but it can also be a cons whose car is
a string.  This is so that the function can be used to filter absolute
file-name completion candidates."
  (when (consp filename) (setq filename  (car filename)))
  (or (and (eq system-type 'windows-nt)
           ;; $$$$  (save-match-data   ; IS THIS NEEDED?
           (let ((case-fold-search  t)) (string-match "\\`\\([a-z]:\\)" filename))
           (eq 0 (condition-case nil
                     (icicle-ms-windows-NET-USE (match-string 1 filename))
                   (error nil)))
           icicle-network-drive-means-remote-flag)
      (and (fboundp 'file-remote-p)  (file-remote-p filename))
      (and (stringp filename)  (string-match "\\`/[^/]+:" filename)  (match-string 0 filename))))

(defun icicle-file-symlink-p (filename)
  "Return non-nil if FILENAME is the name of a symbolic link.
The value is the link target, as a string.  Otherwise it returns nil.
This function does not check whether the link target exists.

FILENAME is normally a string, but it can also be a cons whose car is
a string.  This is so that the function can be used to filter absolute
file-name completion candidates."
  (when (consp filename) (setq filename  (car filename)))
  (file-symlink-p filename))

;; NOTE: This is *not* a general substitute for `file-writable-p'.
(defun icicle-file-writable-p (filename)
  "Return non-nil if FILENAME names a writable file.
Return nil if FILENAME is \"\" or it names a directory.
FILENAME is normally a string, but it can also be a cons whose car is
a string.  This is so that the function can be used to filter absolute
file-name completion candidates."
  (when (consp filename) (setq filename  (car filename)))
  (and (not (string= "" filename))  (file-writable-p filename)  (not (file-directory-p filename))))

(defun icicle-image-file-p (filename)
  "Return non-nil if FILENAME names an image file.
The regexp value of `image-file-name-regexp' is used for the test.
Returns nil if library `image-file.el' cannot be loaded, so use this
only for Emacs 23 and later."
  (when (consp filename) (setq filename  (car filename)))
  (and (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
       (fboundp 'image-file-name-regexp)
       (require 'image-file nil t)
       (icicle-string-match-p (image-file-name-regexp) filename)))

;;(@* "Frame-Completion Predicates")
;;  ** Frame-Completion Predicates **

(defun icicle-frame-splittable-p (&optional frame)
  "Return non-nil if FRAME can be split.
This means that frame parameter `unsplittable' is absent or nil.
FRAME can be a frame or a cons (FNAME . FR), as for an element in
the return value of `icicle-make-frame-alist', in which case it is
frame FR that is tested."
  (when (consp frame) (setq frame  (cdr frame)))
  (not (icicle-frame-unsplittable-p frame)))

(defun icicle-frame-unsplittable-p (&optional frame)
  "Return non-nil if FRAME cannot be split.
This means that frame parameter `unsplittable' is non-nil.
FRAME can be a frame or a cons (FNAME . FR), as for an element in
the return value of `icicle-make-frame-alist', in which case it is
frame FR that is tested."
  (when (consp frame) (setq frame  (cdr frame)))
  (cdr (assq 'unsplittable (frame-parameters frame))))

(defun icicle-frame-invisible-p (frame)
  "Return non-nil if FRAME can is currently invisible.
This means that FRAME is live and `frame-visible-p' returns nil.
FRAME can be a frame or a cons (FNAME . FR), as for an element in
the return value of `icicle-make-frame-alist', in which case it is
frame FR that is tested."
  (when (consp frame) (setq frame  (cdr frame)))
  (not (frame-visible-p frame)))


(when (fboundp 'frcmds-frame-iconified-p)
  (defun icicle-frame-iconified-p (frame)
    "Return non-nil if FRAME is iconified.
This means that FRAME is live and `frame-visible-p' returns `icon'.
FRAME can be a frame or a cons (FNAME . FR), as for an element in
the return value of `icicle-make-frame-alist', in which case it is
frame FR that is tested."
    (when (consp frame) (setq frame  (cdr frame)))
    (frcmds-frame-iconified-p frame)))

(when (fboundp 'thumfr-thumbnail-frame-p) ; In `thumb-frm.el'
  (defun icicle-frame-thumbnail-p (&optional frame)
    "Return non-nil if FRAME is a thumbnail frame.
This means that frame parameter `thumfr-thumbnail-frame' is non-nil.
FRAME can be a frame or a cons (FNAME . FR), as for an element in
the return value of `icicle-make-frame-alist', in which case it is
frame FR that is tested."
    (when (consp frame) (setq frame  (cdr frame)))
    (thumfr-thumbnail-frame-p frame)))

;;(@* "Package-Completion Predicates")
;;  ** Package-Completion Predicates **

(when (require 'package nil t)          ; Emacs 24+
  (defun icicle-package-built-in-p (package &optional min-version)
    "Same as `package-built-in-p', but PACKAGE can be a string or a cons.
PACKAGE is normally a symbol, but it can also be a string that names a
package or a cons whose car is such a string.  This is so that the
function can be used to filter completion candidates."
    (when (consp package) (setq package  (car package)))
    (when (stringp package) (setq package  (intern package)))
    (package-built-in-p package min-version))

  (defun icicle-package-disabled-p (package &optional version)
    "Same as `package-disabled-p', but PACKAGE can be a string or a cons.
PACKAGE is normally a symbol, but it can also be a string that names a
package or a cons whose car is such a string.  This is so that the
function can be used to filter completion candidates."
    (when (consp package) (setq package  (car package)))
    (when (stringp package) (setq package  (intern package)))
    (package-disabled-p package version))

  (defun icicle-package-installed-p (package &optional min-version)
    "Same as `package-installed-p', but PACKAGE can be a string or a cons.
PACKAGE is normally a symbol, but it can also be a string that names a
package or a cons whose car is such a string.  This is so that the
function can be used to filter completion candidates."
    (when (consp package) (setq package  (car package)))
    (when (stringp package) (setq package  (intern package)))
    (package-installed-p package min-version)))

;;(@* "Special Candidate-Completion Predicates")
;;  ** Special Candidate-Completion Predicates **

(defun icicle-special-candidate-p (candidate)
  "Return non-nil if CANDIDATE is a special candidate.
The meaning of \"special candidate\" depends on the context.
For example, during key completion local key bindings are special
candidates.

Special candidates are highlighted in buffer `*Completions*' using
face `icicle-special-candidate'."
  (when (consp candidate) (setq candidate  (car candidate)))
  (or (and (symbolp candidate)  (get candidate 'icicle-special-candidate))
      (and (stringp candidate)
           (stringp icicle-special-candidate-regexp)
           (icicle-string-match-p icicle-special-candidate-regexp candidate))
      ;; UGLY hack.  Unfortunately, it is `icicle-display-candidates-in-Completions' that puts face
      ;; `icicle-special-candidate' on elements of `icicle-completion-candidates', and it is called
      ;; AFTER `icicle-(prefix|apropos)-candidates' is called, and it is there that sorting is done.
      (and (stringp candidate)
           (let ((symb   (intern-soft candidate))
                 (alist  (or icicle-candidates-alist  icicle-complete-keys-alist)))
             (and symb  (assq symb alist)  (get symb 'icicle-special-candidate))))
      ;; Keep this anyway, at least for now.
      (and (stringp candidate)
           (let ((fprop  (get-text-property 0 'face candidate)))
             (if (consp fprop)
                 (memq 'icicle-special-candidate fprop)
               (eq 'icicle-special-candidate fprop))))))

(defun icicle-not-special-candidate-p (candidate)
  "Return non-nil if CANDIDATE is not a special candidate."
  (not (icicle-special-candidate-p candidate)))

;;(@* "Symbol-Completion Predicates")
;;  ** Symbol-Completion Predicates **

;; Do not bother with `user-variable-p'.  For Emacs > 22 it is the same as `custom-variable-p'.
(defun icicle-custom-variable-p (variable)
  "Return non-nil if VARIABLE is a customizable variable.
A customizable variable is either (i) a variable whose property
list contains a non-nil `standard-value' or `custom-autoload'
property, or (ii) an alias for another customizable variable.

If VARIABLE is a cons with a string car, then the car is used as the
name of the face.  This is so that the function can be used to filter
completion candidates.  The string can be a multi-completion
whose first part is the variable name."
  (when (consp variable) (setq variable  (car variable)))
  (when (and icicle-multi-completing-p  (stringp variable))
    (let ((icicle-list-use-nth-parts  '(1)))
      (setq variable  (icicle-transform-multi-completion variable))))
  (when (stringp variable) (setq variable  (intern variable)))
  (custom-variable-p variable))

(defun icicle-binary-option-p (symbol)
  "Return non-nil if SYMBOL is a user option with custom-type `boolean'.
SYMBOL is normally a symbol, but it can also be a string that names a
symbol or a cons whose car is such a string.  This is so that the
function can be used to filter completion candidates."
  (when (consp symbol) (setq symbol  (car symbol)))
  (when (stringp symbol) (setq symbol  (intern symbol)))
  (eq (icicle-get-safe symbol 'custom-type) 'boolean))

(when (fboundp 'special-variable-p)     ; Emacs 24+
  (defun icicle-special-variable-p (variable)
    "Return non-nil if VARIABLE's global binding is special.
A special variable is one that is bound dynamically, even in a context
where binding is lexical by default.

If VARIABLE is a cons with a string car, then the car is used as the
name of the face.  This is so that the function can be used to filter
completion candidates."
    (when (consp variable) (setq variable  (car variable)))
    (when (stringp variable) (setq variable  (intern variable)))
    (special-variable-p variable)))

;;; Same as `thgcmd-defined-thing-p' in `thing-cmds.el', except this accepts also a cons or a string, so it
;;; can be used as an element of `icicle-cand-preds-all', to filter completion candidates.
(defun icicle-defined-thing-p (thing)
  "Return non-nil if THING is defined as a thing-at-point type.
THING is normally a symbol, but it can also be a string that names a
symbol or a cons whose car is such a string.  This is so that the
function can be used to filter completion candidates."
  (when (consp thing) (setq thing  (car thing)))
  (when (stringp thing) (setq thing  (intern thing)))
  (let ((forward-op    (or (get thing 'forward-op)  (intern-soft (format "forward-%s" thing))))
        (beginning-op  (get thing 'beginning-op))
        (end-op        (get thing 'end-op))
        (bounds-fn     (get thing 'bounds-of-thing-at-point))
        (thing-fn      (get thing 'thing-at-point)))
    (or (functionp forward-op)
        (and (functionp beginning-op)  (functionp end-op))
        (functionp bounds-fn)
        (functionp thing-fn))))

;;(@* "Window-Completion Predicates")
;;  ** Window-Completion Predicates **

(when (fboundp 'window-at-side-p)       ; Emacs 24+
  (defun icicle-window-at-bottom-p (window)
    "Return non-nil if WINDOW is at the bottom of its frame.
WINDOW can be a window or a cons (WNAME . WIN), as for an element in
the return value of `icicle-make-window-alist', in which case it is
window WIN that is tested."
    (when (consp window) (setq window  (cdr window)))
    (window-at-side-p window 'bottom))

  (defun icicle-window-at-left-p (window)
    "Return non-nil if WINDOW is at the left side of its frame.
WINDOW can be a window or a cons (WNAME . WIN), as for an element in
the return value of `icicle-make-window-alist', in which case it is
window WIN that is tested."
    (when (consp window) (setq window  (cdr window)))
    (window-at-side-p window 'left))

  (defun icicle-window-at-right-p (window)
    "Return non-nil if WINDOW is at the right side of its frame.
WINDOW can be a window or a cons (WNAME . WIN), as for an element in
the return value of `icicle-make-window-alist', in which case it is
window WIN that is tested."
    (when (consp window) (setq window  (cdr window)))
    (window-at-side-p window 'right))

  (defun icicle-window-at-top-p (window)
    "Return non-nil if WINDOW is at the top of its frame.
WINDOW can be a window or a cons (WNAME . WIN), as for an element in
the return value of `icicle-make-window-alist', in which case it is
window WIN that is tested."
    (when (consp window) (setq window  (cdr window)))
    (window-at-side-p window 'top)))

(when (fboundp 'window-deletable-p)     ; Emacs 24+
  (defun icicle-window-deletable-p (&optional window)
    "Return non-nil if WINDOW can be safely deleted from its frame.
Same as `window-deletable-p', but WINDOW can be a cons (WNAME . WIN).
If it is such a cons, it is window WIN that is tested."
    (when (consp window) (setq window  (cdr window)))
    (window-deletable-p window)))

(defun icicle-window-dedicated-p (&optional window)
  "Same as `window-dedicated-p', but WINDOW can be a cons (WNAME . WIN).
If it is such a cons, it is window WIN that is tested."
  (when (consp window) (setq window  (cdr window)))
  (window-dedicated-p window))

(defun icicle-window-invisible-p (&optional window)
  "Return non-nil if WINDOW is invisible, that is, on an invisible frame.
WINDOW can be a window or a cons (WNAME . WIN), where WNAME is a name.
If it is such a cons, it is window WIN that is tested."
  (when (consp window) (setq window  (cdr window)))
  (not (frame-visible-p (window-frame window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-fn.el ends here

;;; icicles-cmd1.el --- Top-level commands for Icicles
;;
;; Filename: icicles-cmd1.el
;; Description: Top-level commands for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2012, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:25:04 2006
;; Version: 22.0
;; Last-Updated: Sun Dec 16 17:49:32 2012 (-0800)
;;           By: dradams
;;     Update #: 25327
;; URL: http://www.emacswiki.org/icicles-cmd1.el
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
;;   `fuzzy', `fuzzy-match', `hexrgb', `icicles-face', `icicles-fn',
;;   `icicles-mcmd', `icicles-opt', `icicles-var', `image-dired',
;;   `kmacro', `levenshtein', `misc-fns', `mouse3', `mwheel',
;;   `naked', `regexp-opt', `ring', `ring+', `second-sel', `strings',
;;   `thingatpt', `thingatpt+', `wid-edit', `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  top-level commands (and a few non-interactive functions used in
;;  those commands).  Library `icicles-cmd2.el' is a continuation of
;;  this file (a single file for all top-level commands would be too
;;  large to upload to Emacs Wiki).
;;
;;  For commands to be used mainly in the minibuffer or buffer
;;  `*Completions*', see `icicles-mcmd.el'.
;;
;;  For Icicles documentation, see `icicles-doc1.el' and
;;  `icicles-doc2.el'.
;;
;;  If you use the byte-compiled version of this library,
;;  `icicles-cmd1.elc', in Emacs 23, then it must be byte-compiled
;;  using Emacs 23.  Otherwise, Icicles key completion (and perhaps
;;  other things?) will not work correctly.
;;
;;  Widgets defined here:
;;
;;    `icicle-file', `icicle-ORIG-file'.
;;
;;  Commands defined here - (+) means a multi-command:
;;
;;    (+)`clear-option', (+)`icicle-add-buffer-candidate',
;;    (+)`icicle-add-buffer-config',
;;    `icicle-add-entry-to-saved-completion-set', `icicle-apropos',
;;    `icicle-apropos-command', `icicle-apropos-function',
;;    `icicle-apropos-option', (+)`icicle-apropos-options-of-type',
;;    (+)`icicle-apropos-value', `icicle-apropos-variable',
;;    (+)`icicle-apropos-vars-w-val-satisfying',
;;    `icicle-apropos-zippy', `icicle-bbdb-complete-mail',
;;    `icicle-bbdb-complete-name', (+)`icicle-bookmark',
;;    (+)`icicle-bookmark-all-tags',
;;    (+)`icicle-bookmark-all-tags-other-window',
;;    (+)`icicle-bookmark-all-tags-regexp',
;;    (+)`icicle-bookmark-all-tags-regexp-other-window',
;;    (+)`icicle-bookmark-autofile',
;;    (+)`icicle-bookmark-autofile-all-tags',
;;    (+)`icicle-bookmark-autofile-all-tags-other-window',
;;    (+)`icicle-bookmark-autofile-all-tags-regexp',
;;    (+)`icicle-bookmark-autofile-all-tags-regexp-other-window',
;;    `icicle-bookmark-autofile-narrow',
;;    (+)`icicle-bookmark-autofile-other-window',
;;    (+)`icicle-bookmark-autofile-some-tags',
;;    (+)`icicle-bookmark-autofile-some-tags-other-window',
;;    (+)`icicle-bookmark-autofile-some-tags-regexp',
;;    (+)`icicle-bookmark-autofile-some-tags-regexp-other-window',
;;    (+)`icicle-bookmark-autonamed',
;;    `icicle-bookmark-autonamed-narrow',
;;    (+)`icicle-bookmark-autonamed-other-window',
;;    (+)`icicle-bookmark-autonamed-this-buffer',
;;    `icicle-bookmark-autonamed-this-buffer-narrow',
;;    (+)`icicle-bookmark-autonamed-this-buffer-other-window',
;;    (+)`icicle-bookmark-bookmark-file',
;;    `icicle-bookmark-bookmark-file-narrow',
;;    (+)`icicle-bookmark-bookmark-list',
;;    `icicle-bookmark-bookmark-list-narrow',
;;    (+)`icicle-bookmark-cmd', (+)`icicle-bookmark-desktop',
;;    `icicle-bookmark-desktop-narrow', (+)`icicle-bookmark-dired',
;;    `icicle-bookmark-dired-narrow',
;;    (+)`icicle-bookmark-dired-other-window',
;;    (+)`icicle-bookmarked-buffer-list',
;;    (+)`icicle-bookmarked-file-list', (+)`icicle-bookmark-file',
;;    (+)`icicle-bookmark-file-all-tags',
;;    (+)`icicle-bookmark-file-all-tags-other-window',
;;    (+)`icicle-bookmark-file-all-tags-regexp',
;;    (+)`icicle-bookmark-file-all-tags-regexp-other-window',
;;    (+)`icicle-bookmark-file-other-window',
;;    `icicle-bookmark-file-narrow',
;;    (+)`icicle-bookmark-file-some-tags',
;;    (+)`icicle-bookmark-file-some-tags-other-window',
;;    (+)`icicle-bookmark-file-some-tags-regexp',
;;    (+)`icicle-bookmark-file-some-tags-regexp-other-window',
;;    (+)`icicle-bookmark-file-this-dir',
;;    (+)`icicle-bookmark-file-this-dir-other-window',
;;    (+)`icicle-bookmark-file-this-dir-all-tags',
;;    (+)`icicle-bookmark-file-this-dir-all-tags-other-window',
;;    (+)`icicle-bookmark-file-this-dir-all-tags-regexp',
;;    (+)`icicle-bookmark-file-this-dir-all-tags-regexp-other-window',
;;    `icicle-bookmark-file-this-dir-narrow',
;;    (+)`icicle-bookmark-file-this-dir-some-tags',
;;    (+)`icicle-bookmark-file-this-dir-some-tags-other-window',
;;    (+)`icicle-bookmark-file-this-dir-some-tags-regexp',
;;    (+)`icicle-bookmark-file-this-dir-some-tags-regexp-other-window',
;;    (+)`icicle-bookmark-gnus', `icicle-bookmark-gnus-narrow',
;;    (+)`icicle-bookmark-gnus-other-window',
;;    (+)`icicle-bookmark-image', `icicle-bookmark-image-narrow',
;;    (+)`icicle-bookmark-image-other-window',
;;    (+)`icicle-bookmark-info', `icicle-bookmark-info-narrow',
;;    (+)`icicle-bookmark-info-other-window', `icicle-bookmark-jump',
;;    `icicle-bookmark-jump-other-window', (+)`icicle-bookmark-list',
;;    (+)`icicle-bookmark-local-file',
;;    `icicle-bookmark-local-file-narrow',
;;    (+)`icicle-bookmark-local-file-other-window',
;;    (+)`icicle-bookmark-man', `icicle-bookmark-man-narrow',
;;    (+)`icicle-bookmark-man-other-window',
;;    (+)`icicle-bookmark-non-file',
;;    `icicle-bookmark-non-file-narrow',
;;    (+)`icicle-bookmark-non-file-other-window',
;;    (+)`icicle-bookmark-other-window', (+)`icicle-bookmark-region',
;;    `icicle-bookmark-region-narrow',
;;    (+)`icicle-bookmark-region-other-window',
;;    (+)`icicle-bookmark-remote-file',
;;    `icicle-bookmark-remote-file-narrow',
;;    (+)`icicle-bookmark-remote-file-other-window',
;;    `icicle-bookmark-save-marked-files',
;;    `icicle-bookmark-save-marked-files-as-project',
;;    `icicle-bookmark-save-marked-files-more',
;;    `icicle-bookmark-save-marked-files-persistently',
;;    `icicle-bookmark-save-marked-files-to-variable',
;;    `icicle-bookmark-set', (+)`icicle-bookmark-some-tags',
;;    (+)`icicle-bookmark-some-tags-other-window',
;;    (+)`icicle-bookmark-some-tags-regexp',
;;    (+)`icicle-bookmark-some-tags-regexp-other-window',
;;    (+)`icicle-bookmark-specific-buffers',
;;    `icicle-bookmark-specific-buffers-narrow',
;;    (+)`icicle-bookmark-specific-buffers-other-window',
;;    (+)`icicle-bookmark-specific-files',
;;    `icicle-bookmark-specific-files-narrow',
;;    (+)`icicle-bookmark-specific-files-other-window',
;;    (+)`icicle-bookmark-temporary',
;;    `icicle-bookmark-temporary-narrow',
;;    (+)`icicle-bookmark-temporary-other-window',
;;    (+)`icicle-bookmark-this-buffer',
;;    `icicle-bookmark-this-buffer-narrow',
;;    (+)`icicle-bookmark-this-buffer-other-window',
;;    (+)`icicle-bookmark-url', `icicle-bookmark-url-narrow',
;;    (+)`icicle-bookmark-url-other-window', (+)`icicle-bookmark-w3m',
;;    `icicle-bookmark-w3m-narrow',
;;    (+)`icicle-bookmark-w3m-other-window', (+)`icicle-buffer',
;;    (+)`icicle-buffer-config', (+)`icicle-buffer-list',
;;    (+)`icicle-buffer-no-search',
;;    (+)`icicle-buffer-no-search-other-window',
;;    (+)`icicle-buffer-other-window', `icicle-cd-for-abs-files',
;;    `icicle-cd-for-loc-files', (+)`icicle-clear-history',
;;    (+)`icicle-clear-current-history', (+)`icicle-color-theme',
;;    `icicle-comint-dynamic-complete',
;;    `icicle-comint-dynamic-complete-filename',
;;    `icicle-comint-replace-by-expanded-filename',
;;    (+)`icicle-command-abbrev', (+)`icicle-command-abbrev-command',
;;    (+)`icicle-completing-yank', `icicle-customize-apropos',
;;    `icicle-customize-apropos-faces',
;;    `icicle-customize-apropos-groups',
;;    `icicle-customize-apropos-options',
;;    (+)`icicle-customize-apropos-options-of-type',
;;    (+)`icicle-customize-apropos-opts-w-val-satisfying',
;;    (+)`icicle-customize-face',
;;    (+)`icicle-customize-face-other-window',
;;    `icicle-customize-icicles-group', `icicle-dabbrev-completion',
;;    (+)`icicle-delete-file', (+)`icicle-delete-window',
;;    (+)`icicle-describe-option-of-type', `icicle-describe-process',
;;    (+)`icicle-describe-var-w-val-satisfying',
;;    (+)`icicle-delete-windows', (+)`icicle-directory-list',
;;    (+)`icicle-dired', `icicle-dired-chosen-files',
;;    `icicle-dired-chosen-files-other-window',
;;    (+)`icicle-dired-insert-as-subdir',
;;    (+)`icicle-dired-other-window', `icicle-dired-project',
;;    `icicle-dired-project-other-window',
;;    `icicle-dired-saved-file-candidates',
;;    `icicle-dired-saved-file-candidates-other-window',
;;    `icicle-dired-save-marked',
;;    `icicle-dired-save-marked-as-project',
;;    `icicle-dired-save-marked-more',
;;    `icicle-dired-save-marked-more-recursive',
;;    `icicle-dired-save-marked-persistently',
;;    `icicle-dired-save-marked-recursive',
;;    `icicle-dired-save-marked-to-cache-file-recursive',
;;    `icicle-dired-save-marked-to-fileset-recursive',
;;    `icicle-dired-save-marked-to-variable',
;;    `icicle-dired-save-marked-to-variable-recursive',
;;    `icicle-doremi-increment-variable+',
;;    `icicle-ess-complete-filename',
;;    `icicle-ess-complete-object-name',
;;    `icicle-ess-internal-complete-object-name',
;;    `icicle-ess-R-complete-object-name',
;;    (+)`icicle-execute-extended-command',
;;    (+)`icicle-execute-named-keyboard-macro', (+)`icicle-face-list',
;;    (+)`icicle-file', (+)`icicle-file-list',
;;    (+)`icicle-file-other-window', (+)`icicle-find-file',
;;    (+)`icicle-find-file-absolute',
;;    (+)`icicle-find-file-absolute-other-window',
;;    (+)`icicle-find-file-in-tags-table',
;;    (+)`icicle-find-file-in-tags-table-other-window',
;;    (+)`icicle-find-file-of-content',
;;    (+)`icicle-find-file-of-content-other-window',
;;    (+)`icicle-find-file-other-window',
;;    (+)`icicle-find-file-read-only',
;;    (+)`icicle-find-file-read-only-other-window',
;;    (+)`icicle-find-first-tag',
;;    (+)`icicle-find-first-tag-other-window', (+)`icicle-find-tag',
;;    `icicle-grep-saved-file-candidates',
;;    `icicle-gud-gdb-complete-command', (+)`icicle-increment-option',
;;    (+)`icicle-increment-variable', (+)`icicle-insert-buffer',
;;    (+)`icicle-keyword-list', (+)`icicle-kill-buffer',
;;    (+)`icicle-kmacro', `icicle-lisp-complete-symbol',
;;    (+)`icicle-locate', (+)`icicle-locate-file',
;;    (+)`icicle-locate-file-no-symlinks',
;;    (+)`icicle-locate-file-no-symlinks-other-window',
;;    (+)`icicle-locate-file-other-window',
;;    (+)`icicle-locate-other-window', `icicle-ORIG-customize-face',
;;    `icicle-ORIG-customize-face-other-window',
;;    `icicle-ORIG-dabbrev-completion',
;;    `icicle-ORIG-lisp-complete-symbol',
;;    `icicle-ORIG-lisp-completion-at-point',
;;    `icicle-ORIG-repeat-complex-command',
;;    (+)`icicle-other-window-or-frame', `icicle-pop-tag-mark',
;;    `icicle-pp-eval-expression', (+)`icicle-recent-file',
;;    (+)`icicle-recent-file-other-window',
;;    `icicle-recompute-shell-command-candidates',
;;    (+)`icicle-regexp-list', (+)`icicle-remove-buffer-candidate',
;;    (+)`icicle-remove-buffer-config',
;;    `icicle-remove-entry-from-saved-completion-set',
;;    (+)`icicle-remove-file-from-recentf-list',
;;    (+)`icicle-remove-saved-completion-set',
;;    `icicle-repeat-complex-command',
;;    (+)`icicle-reset-option-to-nil',
;;    (+)`icicle-select-bookmarked-region', (+)`icicle-select-frame',
;;    `icicle-select-frame-by-name', (+)`icicle-select-window',
;;    `icicle-select-window-by-name', `icicle-send-bug-report',
;;    (+)`icicle-send-signal-to-process', (+)`icicle-set-option-to-t',
;;    (+)`icicle-sexp-list', `icicle-shell-dynamic-complete-command',
;;    `icicle-shell-dynamic-complete-environment-variable',
;;    `icicle-shell-dynamic-complete-filename',
;;    (+)`icicle-string-list', (+)`icicle-toggle-option',
;;    (+)`icicle-visit-marked-file-of-content',
;;    (+)`icicle-visit-marked-file-of-content-other-window',
;;    `icicle-widget-file-complete',
;;    (+)`icicle-yank-maybe-completing',
;;    (+)`icicle-yank-pop-commands', `icicle-zap-to-char',
;;    (+)`toggle'.
;;
;;  Non-interactive functions defined here:
;;
;;    `custom-variable-p', `icicle-apropos-opt-action',
;;    `icicle-binary-option-p',
;;    `icicle-bookmark-bind-narrow-commands',
;;    `icicle-bookmark-cleanup', `icicle-bookmark-cleanup-on-quit',
;;    `icicle-bookmark-delete-action', `icicle-bookmark-help-string',
;;    `icicle-bookmark-jump-1',
;;    `icicle-buffer-apropos-complete-match',
;;    `icicle-buffer-multi-complete', `icicle-buffer-name-prompt',
;;    `icicle-cached-files-without-buffers', `icicle-clear-history-1',
;;    `icicle-clear-history-entry',
;;    `icicle-comint-completion-at-point',
;;    `icicle-comint-dynamic-complete-as-filename',
;;    `icicle-comint-dynamic-simple-complete',
;;    `icicle-comint-replace-orig-completion-fns',
;;    `icicle-command-abbrev-action',
;;    `icicle-command-abbrev-matching-commands',
;;    `icicle-command-abbrev-record', `icicle-command-abbrev-regexp',
;;    `icicle-customize-apropos-opt-action', `icicle-customize-faces',
;;    `icicle-dabbrev--abbrev-at-point',
;;    `icicle-default-buffer-names',
;;    `icicle-delete-file-or-directory', `icicle-describe-opt-action',
;;    `icicle-describe-opt-of-type-complete',
;;    `icicle-execute-extended-command-1', `icicle-explore',
;;    `icicle-file-of-content-apropos-complete-match',
;;    `icicle-find-file-of-content-multi-complete',
;;    `icicle-find-first-tag-action',
;;    `icicle-find-first-tag-other-window-action',
;;    `icicle-find-tag-action', `icicle-find-tag-define-candidates',
;;    `icicle-find-tag-define-candidates-1',
;;    `icicle-find-tag-final-act', `icicle-find-tag-help',
;;    `icicle-find-tag-quit-or-error', `icicle-insert-for-yank',
;;    `icicle-kill-a-buffer-and-update-completions',
;;    `icicle-kmacro-action', `icicle-lisp-completion-at-point',
;;    (+)`icicle-locate-file-1', `icicle-locate-file-action',
;;    `icicle-locate-file-other-window-action',
;;    `icicle-make-bookmark-candidate',
;;    `icicle-make-file+date-candidate', `icicle-make-frame-alist',
;;    `icicle-make-window-alist',
;;    `icicle-bookmark-propertize-candidate',
;;    `icicle-pp-display-expression',
;;    `icicle-read-args-w-val-satisfying',
;;    `icicle-recent-files-without-buffers.',
;;    `icicle-remove-buffer-candidate-action',
;;    `icicle-remove-buffer-config-action',
;;    `icicle-remove-from-recentf-candidate-action',
;;    `icicle-remove-saved-set-action',
;;    `icicle-shell-command-on-file',
;;    `icicle-shell-dynamic-complete-as-command',
;;    `icicle-shell-dynamic-complete-as-environment-variable'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-locate-file-action-fn',
;;    `icicle-locate-file-no-symlinks-p',
;;    `icicle-locate-file-use-locate-p'.
;;
;;
;;  ***** NOTE: The following functions defined in `dabbrev.el' have
;;              been REDEFINED HERE:
;;
;;  `dabbrev-completion' - Use Icicles minibuffer completion when there
;;                         are multiple candidates.
;;
;;
;;  ***** NOTE: The following functions defined in `bbdb-com.el' have
;;              been REDEFINED HERE:
;;              (BBDB is available here: http://bbdb.sourceforge.net/.)
;;
;;  `bbdb-complete-name' - Use Icicles minibuffer completion when there
;;                         are multiple candidates.
;;
;;
;;  ***** NOTE: The following functions defined in `lisp.el' have
;;              been REDEFINED in Icicles:
;;
;;  `lisp-complete-symbol' - Selects `*Completions*' window even if on
;;                           another frame.
;;
;;
;;  ***** NOTE: The following function defined in `simple.el' has
;;              been REDEFINED HERE:
;;
;;  `repeat-complex-command' - Use `completing-read' to read command.
;;
;;
;;  ***** NOTE: The following functions defined in `cus-edit.el' have
;;              been REDEFINED HERE:
;;
;;  `customize-apropos', `customize-apropos-faces',
;;  `customize-apropos-groups', `customize-apropos-options' -
;;     Use `completing-read' to read the regexp.
;;  `customize-face', `customize-face-other-window' - Multi-commands.
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
;;  (@> "Icicles Top-Level Commands, Part 1")
 
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

(eval-when-compile (require 'cl)) ;; lexical-let[*], pushnew
                                  ;; plus, for Emacs < 21: dolist, push
(eval-when-compile (when (>= emacs-major-version 21) (require 'recentf))) ;; recentf-mode
(require 'apropos-fn+var nil t) ;; (no error if not found):
  ;; apropos-command, apropos-function, apropos-option, apropos-variable
(eval-when-compile (require 'dabbrev))
  ;; dabbrev-case-fold-search, dabbrev-upcase-means-case-search, dabbrev--last-obarray,
  ;; dabbrev--last-completion-buffer, dabbrev--last-abbreviation, dabbrev--check-other-buffers,
  ;; dabbrev-case-replace, dabbrev--reset-global-variables, dabbrev--minibuffer-origin,
  ;; dabbrev--find-all-expansions, dabbrev--substitute-expansion
(eval-when-compile (require 'bookmark))
  ;; bookmark-all-names, bookmark-buffer-name, bookmark-current-bookmark
(eval-when-compile (require 'comint))
  ;; comint-completion-addsuffix, comint-completion-autolist, comint-completion-fignore,
  ;; comint-completion-recexact, comint-directory, comint-dynamic-complete-filename,
  ;; comint-dynamic-complete-functions, comint-line-beginning-position,
  ;; comint-match-partial-filename, comint-quote-filename
(eval-when-compile (require 'cookie1 nil t)) ;; (no error if not found): cookie-cache
(eval-when-compile (require 'shell)) ;; shell-backward-command, shell-completion-execonly,
  ;; shell-dynamic-complete-command, shell-dynamic-complete-environment-variable,
  ;; shell-dynamic-complete-filename, shell-match-partial-variable
(eval-when-compile (require 'etags))
  ;; file-of-tag, find-tag, find-tag-default, find-tag-default-function,
  ;; find-tag-marker-ring, find-tag-other-window, goto-tag-location-function, snarf-tag-function,
  ;; tag-find-file-of-tag-noselect, tags-case-fold-search,
  ;; tags-lazy-completion-table, tags-table-files, visit-tags-table-buffer
(eval-when-compile (require 'yow nil t)) ;; (no error if not found):
  ;; apropos-zippy, yow-after-load-message, yow-file, yow-load-message

;; Commented out because `synonyms.el' soft-requires Icicles.
;; (eval-when-compile (require 'synonyms nil t)) ;; (no error if not found):
  ;; synonyms-ensure-synonyms-read-from-cache, synonyms-obarray
(eval-when-compile (require 'misc-cmds nil t)) ;; (no error if not found):
  ;; kill-buffer-and-its-windows
(eval-when-compile (require 'bbdb nil t) (require 'bbdb-com nil t)) ;; (no error if not found):
  ;; bbdb-auto-fill-function, bbdb-complete-name, bbdb-complete-name-allow-cycling,
  ;; bbdb-complete-name-cleanup, bbdb-complete-name-hooks, bbdb-completion-display-record,
  ;; bbdb-completion-predicate, bbdb-completion-type, bbdb-display-records-1,
  ;; bbdb-dwim-net-address, bbdb-expand-mail-aliases, bbdb-extract-address-components-func,
  ;; bbdb-gag-messages, bbdb-hashtable, bbdb-mapc, bbdb-pop-up-bbdb-buffer, bbdb-record-aka,
  ;; bbdb-record-name, bbdb-record-net, bbdb-search-intertwingle, bbdb-string-trim
(require 'cus-edit)
  ;; customize-apropos, customize-apropos-faces, customize-apropos-groups,
  ;; customize-apropos-options, custom-buffer-create, custom-buffer-order-groups, customize-face,
  ;; customize-face-other-window, custom-sort-items
(require 'misc-fns nil t)   ;; (no error if not found): another-buffer
(require 'frame-cmds nil t) ;; (no error if not found): delete-windows-on (my version)
(require 'second-sel nil t) ;; (no error if not found):
  ;; secondary-selection-yank-commands, secondary-selection-yank-secondary-commands,
  ;; yank-pop-secondary

(eval-when-compile
 (or (condition-case nil
         (load-library "icicles-mac")   ; Use load-library to ensure latest .elc.
       (error nil))
     (require 'icicles-mac)))           ; Require, so can load separately if not on `load-path'.
  ;; icicle-assoc-delete-all, icicle-(buffer|file)-bindings, icicle-condition-case-no-debug,
  ;; icicle-define-bookmark(-other-window)-command, icicle-define(-file)-command,
  ;; icicle-define-add-to-alist-command
(require 'icicles-mcmd)
  ;; icicle-bind-buffer-candidate-keys, icicle-bind-file-candidate-keys, icicle-unbind-buffer-candidate-keys,
  ;; icicle-unbind-file-candidate-keys, icicle-yank
(require 'icicles-opt)                  ; (This is required anyway by `icicles-var.el'.)
  ;; icicle-add-proxy-candidates-flag, icicle-buffer-configs, icicle-buffer-extras,
  ;; icicle-buffer-ignore-space-prefix-flag, icicle-buffer-match-regexp,
  ;; icicle-buffer-no-match-regexp, icicle-buffer-predicate, icicle-buffer-require-match-flag,
  ;; icicle-buffer-sort, icicle-color-themes, icicle-kbd, icicle-saved-completion-sets,
  ;; icicle-sort-comparer, icicle-transform-function
(require 'icicles-var)                  ; (This is required anyway by `icicles-fn.el'.)
  ;; icicle-abs-file-candidates, icicle-all-candidates-list-action-fn,
  ;; icicle-all-candidates-list-alt-action-fn, icicle-bookmark-history,
  ;; icicle-bookmark-list-names-only-p, icicle-bookmark-types, icicle-buffer-config-history,
  ;; icicle-bufflist, icicle-candidate-action-fn, icicle-candidate-alt-action-fn,
  ;; icicle-candidate-help-fn, icicle-candidate-nb, icicle-candidate-properties-alist,
  ;; icicle-candidates-alist, icicle-color-theme-history, icicle-command-abbrev-history,
  ;; icicle-commands-for-abbrev, icicle-comp-base-is-default-dir-p, icicle-completion-candidates,
  ;; icicle-completion-set-history, icicle-current-input, icicle-delete-candidate-object,
  ;; icicle-explore-final-choice, icicle-explore-final-choice-full, icicle-extra-candidates,
  ;; icicle-face-name-history, icicle-frame-alist, icicle-frame-name-history, icicle-full-cand-fn,
  ;; icicle-function-name-history, icicle-get-alist-candidate-function, icicle-hist-var,
  ;; icicle-incremental-completion-p, icicle-inhibit-sort-p, icicle-inhibit-try-switch-buffer,
  ;; icicle-kill-history, icicle-kmacro-alist, icicle-kmacro-history,icicle-list-use-nth-parts, 
  ;; icicle-must-match-regexp, icicle-must-not-match-regexp, icicle-must-pass-after-match-predicate,
  ;; icicle-new-last-cmd, icicle-orig-buff, icicle-orig-must-pass-after-match-pred,
  ;; icicle-orig-pt-explore, icicle-orig-window, icicle-orig-win-explore, icicle-pref-arg,
  ;; icicle-previous-raw-file-name-inputs, icicle-previous-raw-non-file-name-inputs, icicle-prompt,
  ;; icicle-proxy-candidates, icicle-read-expression-map, icicle-remove-icicles-props-p,
  ;; icicle-re-no-dot, icicle-saved-completion-candidates, icicle-search-history,
  ;; icicle-transform-before-sort-p, icicle-use-candidates-only-once-alt-p,
  ;; icicle-whole-candidate-as-text-prop-p, icicle-variable-name-history
(require 'icicles-fn)                   ; (This is required anyway by `icicles-mcmd.el'.)
  ;; icicle-delete-dups, icicle-highlight-lighter, icicle-multi-comp-apropos-complete-match,
  ;; icicle-read-from-minibuf-nil-default


;; Byte-compiling this file, you will likely get some byte-compiler warning messages.
;; These are probably benign - ignore them.  Icicles is designed to work with multiple
;; versions of Emacs, and that fact provokes compiler warnings.  If you get byte-compiler
;; errors (not warnings), then please report a bug, using `M-x icicle-send-bug-report'.

;;; Some defvars to quiet byte-compiler a bit:

(when (< emacs-major-version 21)
  (defvar eval-expression-debug-on-error))

(when (< emacs-major-version 22)
  (defvar history-delete-duplicates)
  (defvar icicle-kmacro-alist)          ; In `icicles-var.el'
  (defvar kmacro-ring)                  ; In `kmacro.el'
  (defvar read-file-name-completion-ignore-case) ;  In `minibuffer.el'
  (defvar recentf-list)                 ; In `recentf.el'
  (defvar tags-case-fold-search)        ; In `etags.el'
  (defvar tooltip-mode))                ; In `tooltip.el'

(when (< emacs-major-version 23)
  (defvar read-buffer-completion-ignore-case))

(when (< emacs-major-version 24)
  (defvar minibuffer-local-filename-syntax))

(defvar apropos-do-all)                 ; In `apropos.el'
(defvar bbdb-complete-mail-allow-cycling) ; In `bbdb-com.el'
(defvar bbdb-complete-name-allow-cycling) ; In `bbdb-com.el'
(defvar bbdb-completion-list)           ; In `bbdb-come.el'
(defvar bbdb-extract-address-components-func) ; In `bbdb-com.el'
(defvar bbdb-expand-mail-aliases)       ; In `bbdb-com.el'
(defvar bbdb-complete-name-hooks)       ; In `bbdb-com.el'
(defvar bbdb-completion-display-record) ; In `bbdb.el'
(defvar bbdb-completion-type)           ; In `bbdb.el'
(defvar bbdb-hashtable)                 ; In `bbdb.el'
(defvar bbdb-version)                   ; In `bbdb.el'
(defvar bmkp-non-file-filename)         ; In `bookmark+-1.el'
(defvar bmkp-prompt-for-tags-flag)      ; In `bookmark+-1.el'
(defvar bmkp-sorted-alist)              ; In `bookmark+-1.el'
(defvar bookmark-current-point)         ; In `bookmark.el' for Emacs < 
(defvar color-theme)                    ; In `color-theme.el'
(defvar color-themes)                   ; In `color-theme.el'
(defvar color-theme-initialized)        ; In `color-theme.el'
(defvar cookie-cache)
(defvar dabbrev--last-obarray)
(defvar dabbrev--last-completion-buffer)
(defvar ess-current-process-name)       ; In `ess-inf.el'
(defvar ess-mode-syntax-table)          ; In `ess-cust.el'
(defvar ess-use-R-completion)           ; In `ess-cust.el'
(defvar existing-bufs)                  ; `icicle-visit-marked-file-of-content', `icicle-find-file-of-content'
(defvar file-cache-alist)               ; In `filecache.el'
(defvar filesets-data)                  ; In `filesets.el'
(defvar find-tag-default-function)      ; In `etags.el'
(defvar find-tag-marker-ring)           ; In `etags.el'
(defvar goto-tag-location-function)     ; In `etags.el'
(defvar icicle-buffer-easy-files)       ; Here
(defvar icicle-clear-history-hist)      ; In `icicle-clear-history-1',`icicle-clear-current-history'
(defvar icicle--last-toggle-transforming-msg) ; Here
(defvar icicle-window-alist)            ; In `icicle-select-window'
(defvar locate-make-command-line)       ; In `locate.el'
(defvar proced-signal-list)             ; In `proced.el' (Emacs 23+)
(defvar shell-completion-execonly)      ; In `shell.el'
(defvar snarf-tag-function)             ; In `etags.el'
(defvar translation-table-for-input)    ; Built-in, Emacs 21+
(defvar w3m-current-title)              ; In `w3m.el'
(defvar yow-after-load-message)
(defvar yow-file)
(defvar yow-load-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Icicles Top-Level Commands, Part 1")
;;; Icicles Top-Level Commands, Part 1 .   .   .   .   .   .   .   .   .


;; REPLACE ORIGINAL `pp-eval-expression' defined in `pp.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; This is essentially the same as `pp-eval-expression' defined in `pp+.el'.
;;
;; 1. Read with completion, using `icicle-read-expression-map'.
;; 2. Progress message added.
;; 3. Added optional arg and insertion behavior.
;; 4. Respect `icicle-pp-eval-expression-print-length', `icicle-pp-eval-expression-print-level',
;;    and `eval-expression-debug-on-error'.
;; 5. Adjusted to work in different Emacs releases.
;;
;;;###autoload (autoload 'icicle-pp-eval-expression "icicles")
(defun icicle-pp-eval-expression (expression ; Bound to `M-:' in Icicle mode.
                                  &optional insert-value)
  "Evaluate Emacs-Lisp sexp EXPRESSION, and pretty-print its value.
Add the value to the front of the variable `values'.
With a prefix arg, insert the value into the current buffer at point.
 With a negative prefix arg, if the value is a string, then insert it
 into the buffer without double-quotes (`\"').
With no prefix arg:
 If the value fits on one line (frame width) show it in the echo area.
 Otherwise, show the value in buffer `*Pp Eval Output*'.

This command respects user options
`icicle-pp-eval-expression-print-length',
`icicle-pp-eval-expression-print-level', and
`eval-expression-debug-on-error'.

Emacs-Lisp mode completion and indentation bindings are in effect.

By default, Icicle mode remaps all key sequences that are normally
bound to `eval-expression' or `pp-eval-expression' to
`icicle-pp-eval-expression'.  If you do not want this remapping, then
customize option `icicle-top-level-key-bindings'."
  (interactive
   (list (read-from-minibuffer "Eval: " nil icicle-read-expression-map t 'read-expression-history)
         current-prefix-arg))
  (message "Evaluating...")
  (if (or (not (boundp 'eval-expression-debug-on-error))
          (null eval-expression-debug-on-error))
      (setq values  (cons (eval expression) values))
    (let ((old-value  (make-symbol "t"))
          new-value)
      ;; Bind debug-on-error to something unique so that we can
      ;; detect when evaled code changes it.
      (let ((debug-on-error  old-value))
        (setq values     (cons (eval expression) values)
              new-value  debug-on-error))
      ;; If evaled code has changed the value of debug-on-error,
      ;; propagate that change to the global binding.
      (unless (eq old-value new-value)
        (setq debug-on-error  new-value))))
  (let ((print-length  icicle-pp-eval-expression-print-length)
        (print-level   icicle-pp-eval-expression-print-level))
    (cond (insert-value
           (message "Evaluating...done.  Value inserted.")
           (setq insert-value  (prefix-numeric-value insert-value))
           (if (or (not (stringp (car values)))  (wholenump insert-value))
               (pp (car values) (current-buffer))
             (princ (car values) (current-buffer))))
          (t (icicle-pp-display-expression (car values) "*Pp Eval Output*")))))


;; REPLACE ORIGINAL in `pp.el':
;;
;; 1. Use no `emacs-lisp-mode-hook' or `change-major-mode-hook'.
;; 2. Call `font-lock-fontify-buffer'.
;;
;; Same as `pp-display-expression' definition in `pp+.el'.
;;
(defun icicle-pp-display-expression (expression out-buffer-name)
  "Prettify and show EXPRESSION in a way appropriate to its length.
If a temporary buffer is needed for representation, it is named
OUT-BUFFER-NAME."
  (let* ((old-show-function  temp-buffer-show-function)
         ;; Use this function to display the buffer.
         ;; This function either decides not to display it at all
         ;; or displays it in the usual way.
         (temp-buffer-show-function
          `(lambda (buf)
            (with-current-buffer buf
              (goto-char (point-min))
              (end-of-line 1)
              (if (or (< (1+ (point)) (point-max))
                      (>= (- (point) (point-min)) (frame-width)))
                  (let ((temp-buffer-show-function  ',old-show-function)
                        (old-selected               (selected-window))
                        (window                     (display-buffer buf)))
                    (goto-char (point-min)) ; expected by some hooks ...
                    (make-frame-visible (window-frame window))
                    (unwind-protect
                         (progn (select-window window)
                                (run-hooks 'temp-buffer-show-hook))
                      (when (window-live-p old-selected) (select-window old-selected))
                      (message "Evaluating...done.  See buffer `%s'."
                               out-buffer-name)))
                (message "%s" (buffer-substring (point-min) (point))))))))
    (with-output-to-temp-buffer out-buffer-name
      (pp expression)
      (with-current-buffer standard-output
        (setq buffer-read-only  nil)
        ;; Avoid `let'-binding because `change-major-mode-hook' is local.
        ;; IOW, avoid this runtime message:
        ;; "Making change-major-mode-hook buffer-local while locally let-bound!"
        ;; Suggestion from Stefan M.: Can just set these hooks instead of binding,
        ;; because they are not permanent-local.  They'll be emptied and
        ;; repopulated as needed by the call to emacs-lisp-mode.
        (set (make-local-variable 'emacs-lisp-mode-hook) nil)
        (set (make-local-variable 'change-major-mode-hook) nil)
        (emacs-lisp-mode)
        (set (make-local-variable 'font-lock-verbose) nil)
        (font-lock-fontify-buffer)))))

(defun icicle-shell-command-on-file (file)
  "Read a shell command and invoke it, passing FILE as an argument."
  (dired-run-shell-command
   (dired-shell-stuff-it (icicle-read-shell-command (format "! on `%s': " file)) (list file) nil)))

;;;###autoload (autoload 'icicle-recompute-shell-command-candidates "icicles")
(defun icicle-recompute-shell-command-candidates (&optional savep)
  "Update option `icicle-shell-command-candidates-cache'.
Recompute the available shell commands using your search path.
Return the new option value.
With a prefix argument, the updated option is saved persistently.

If the value of option `icicle-guess-commands-in-path' is not `load',
then turning on Icicle mode (again) resets the cache value to ().
If the value of `icicle-guess-commands-in-path' is `first-use', then
the cache is updated when you next use it, but it is not saved."
  (interactive "P")
  (setq icicle-shell-command-candidates-cache  (icicle-compute-shell-command-candidates))
  (when savep (funcall icicle-customize-save-variable-function
                       'icicle-shell-command-candidates-cache
                       icicle-shell-command-candidates-cache))
  icicle-shell-command-candidates-cache)


;; REPLACE ORIGINAL `comint-completion-at-point' defined in `comint.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;;;###autoload (autoload 'icicle-comint-completion-at-point "icicles")
(when (> emacs-major-version 23)
  (defalias 'icicle-comint-completion-at-point 'icicle-comint-dynamic-complete))


;; REPLACE ORIGINAL `comint-dynamic-complete' defined in `comint.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Use Icicles completion when there are multiple candidates.
;;
;;;###autoload (autoload 'icicle-comint-dynamic-complete "icicles")
(defun icicle-comint-dynamic-complete () ; Bound to `TAB' in Comint (and Shell) mode.
  "Dynamically perform completion at point.
Calls the functions in `comint-dynamic-complete-functions', but with
Icicles functions substituted, to perform completion until a function
returns non-nil.  Return that value."
  (interactive)
  ;; Need a symbol for `run-hook-with-args-until-success', so bind one.
  (let ((hook  (icicle-comint-replace-orig-completion-fns)))
    (run-hook-with-args-until-success 'hook)))

(defun icicle-comint-replace-orig-completion-fns ()
  "Return `comint-dynamic-complete-functions', but with Icicles functions.
Get the Icicles functions from option
`icicle-comint-dynamic-complete-replacements'.

Only one (the first matching) replacement is made for any function."
  (let ((result        ())
	(replacements  (copy-sequence icicle-comint-dynamic-complete-replacements)))
    (dolist (fn  comint-dynamic-complete-functions)
      (catch 'c-d-c-f-replacements-loop
	(dolist (rep  replacements)
	  (when (or (eq (car rep) fn)
		    (and (listp (car rep))  (memq fn (car rep))))
	    (push (eval (cadr rep)) result)
	    (unless (eq (car rep) fn)  (push fn result))
	    (setq replacements  (delete rep replacements)) ; For ((a b c) 'NEW), put NEW in front of only one.
	    (throw 'c-d-c-f-replacements-loop nil))) ; Allow only one replacement.
	(push fn result)))
    (nreverse result)))

;;;###autoload (autoload 'icicle-comint-dynamic-complete-filename "icicles")
(defun icicle-comint-dynamic-complete-filename (&optional replace-to-eol-p)
  "Dynamically complete the file name before point, using Icicles completion.
Similar to `comint-replace-by-expanded-filename', except that this
does not change parts of the file name already in the buffer before
point.  It just appends completion characters at point.

Return t if successful, nil otherwise.

With a prefix arg, replace the rest of the line after point with the
completion choice.  Otherwise, replace only the filename-matching text
before point.

Completion is dependent on the value of `comint-completion-addsuffix',
`comint-completion-recexact' and `comint-completion-fignore', and the
timing of completions listing is dependent on the value of
`comint-completion-autolist'.  See also
`comint-match-partial-filename' and
`icicle-comint-dynamic-complete-as-filename'."
  (interactive "P")
  (require 'comint)
  (when (comint-match-partial-filename)
    (unless (window-minibuffer-p (selected-window)) (message "Completing file name..."))
    (icicle-comint-dynamic-complete-as-filename replace-to-eol-p)))

(defun icicle-comint-dynamic-complete-as-filename (&optional replace-to-eol-p)
  "Dynamically complete at point as a filename.
Optional arg REPLACE-TO-EOL-P non-nil means replace the rest of the
line after point with the completion choice.
Return t if successful.
See `icicle-comint-dynamic-complete-filename'."
  (lexical-let* ((completion-ignore-case         (if (boundp 'read-file-name-completion-ignore-case)
                                                     read-file-name-completion-ignore-case
                                                   (memq system-type '(ms-dos windows-nt cygwin))))
                 (completion-ignored-extensions  comint-completion-fignore)
                 (minibuffer-p                   (window-minibuffer-p (selected-window)))
                 (success                        t)
                 (dirsuffix                      (cond ((not comint-completion-addsuffix)         "")
                                                       ((not (consp comint-completion-addsuffix)) "/")
                                                       (t  (car comint-completion-addsuffix))))
                 (filesuffix                     (cond ((not comint-completion-addsuffix)         "")
                                                       ((not (consp comint-completion-addsuffix)) " ")
                                                       (t  (cdr comint-completion-addsuffix))))
                 (filename                       (comint-match-partial-filename))
                 (filename-beg                   (if filename (match-beginning 0) (point)))
                 (filename-end                   (if filename
                                                     (if replace-to-eol-p
                                                         (line-end-position)
                                                       (match-end 0))
                                                   (point)))
                 (filename                       (or filename  ""))
                 (filedir                        (file-name-directory filename))
                 (filenondir                     (file-name-nondirectory filename))
                 (directory                      (if filedir (comint-directory filedir) default-directory))
                 (completion                     (file-name-completion filenondir directory)))
    (cond ((null completion)
           (if minibuffer-p
               (minibuffer-message (format " [No completions of `%s']" filename))
             (message "No completions of `%s'" filename))
           (setq success  nil))
          ((eq completion t)            ; Already completed: "the-file".
           (insert filesuffix)
           (unless minibuffer-p (message "Sole completion")))
          ((string-equal completion "") ; A directory: "dir/" - complete it.
           (icicle-condition-case-no-debug nil
               (let* ((icicle-show-Completions-initially-flag      t)
                      (icicle-incremental-completion-p             'display)
                      (icicle-top-level-when-sole-completion-flag  t)
                      (choice
                       (save-excursion
                         (save-window-excursion (read-file-name "Complete: " directory nil t)))))
                 (when (and choice  (not (string= choice directory)))
                   (insert (comint-quote-filename
                            (directory-file-name (file-relative-name choice directory))))
                   (insert (if (file-directory-p choice) dirsuffix filesuffix))
                   (when replace-to-eol-p (delete-region (point) (line-end-position)))))
             (error nil)))
          (t                            ; COMPLETION is the common prefix string.
           (let ((file            (concat (file-name-as-directory directory) completion))
                 (use-dialog-box  nil)) ; Inhibit use of open-file dialog box if called from menu.
             ;; Insert completion.  The completion string might have a different case from
             ;; what's in the prompt, if `read-file-name-completion-ignore-case' is non-nil.
             (delete-region filename-beg filename-end)
             (if filedir (insert (comint-quote-filename filedir)))
             (insert (comint-quote-filename (directory-file-name completion)))
             (cond ((symbolp (file-name-completion completion directory))
                    ;; We inserted a unique completion.  Add suffix.
                    (insert (if (file-directory-p file) dirsuffix filesuffix))
                    (unless minibuffer-p (message "Completed")))
                   ((and comint-completion-recexact  comint-completion-addsuffix
                         (string-equal filenondir completion)
                         (or (icicle-file-remote-p file) ; Don't let Tramp try to access it.
                             (file-exists-p file)))
                    ;; It's not unique, but user wants shortest match.
                    (insert (if (file-directory-p file) dirsuffix filesuffix))
                    (unless minibuffer-p (message "Completed shortest")))
                   ;; It's not unique.  Let user choose a completion.
                   ((or comint-completion-autolist  (string-equal filenondir completion))
                    (icicle-condition-case-no-debug nil
                        (let* ((icicle-show-Completions-initially-flag      t)
                               (icicle-incremental-completion-p             'display)
                               (icicle-top-level-when-sole-completion-flag  t)
                               (choice
                                (save-excursion
                                  (save-window-excursion
                                    (read-file-name
                                     "Complete: " directory completion nil completion
                                     (and (> emacs-major-version 21)
                                          (lambda (f) (string-match completion f)))))))) ; FREE: COMPLETION.
                          (when choice
                            (delete-backward-char (length completion))
                            (insert (comint-quote-filename
                                     (directory-file-name (file-relative-name choice directory))))
                            (insert (if (file-directory-p choice) dirsuffix filesuffix))))
                      (error nil)))
                   (t (unless minibuffer-p (message "Partially completed")))))))
    success))

;;;###autoload (autoload 'icicle-shell-dynamic-complete-command "icicles")
(defun icicle-shell-dynamic-complete-command ()
  "Dynamically complete the command at point.
Similar to `icicle-comint-dynamic-complete-filename', but this
searches `exec-path' (minus the trailing Emacs library path) for
completion candidates.  Note that this may not be the same as the
shell's idea of the path.

Completion is dependent on the value of `shell-completion-execonly',
plus those that effect file completion.
See `icicle-shell-dynamic-complete-as-command'.

Returns t if successful.

Uses Icicles completion."
  (interactive)
  (let ((filename  (comint-match-partial-filename)))
    (if (and filename
             (save-match-data (not (string-match "[~/]" filename)))
             (eq (match-beginning 0) (save-excursion (shell-backward-command 1) (point))))
        (prog2 (unless (window-minibuffer-p (selected-window))
                 (message "Completing command name..."))
            (icicle-shell-dynamic-complete-as-command)))))

(defun icicle-shell-dynamic-complete-as-command ()
  "Dynamically complete text at point as a command.
See `icicle-shell-dynamic-complete-filename'.
Return t if successful."
  (let* ((filename       (or (comint-match-partial-filename)  ""))
         (filenondir     (file-name-nondirectory filename))
         (path-dirs      (cdr (reverse exec-path)))
         (cwd            (file-name-as-directory (expand-file-name default-directory)))
         (ignored-extensions
          (and comint-completion-fignore
               (mapconcat (lambda (x) (concat (regexp-quote x) "$"))  comint-completion-fignore "\\|")))
         (dir            "")
         (comps-in-dir   ())
         (file           "")
         (abs-file-name  "")
         (completions    ()))
    (while path-dirs                    ; Go thru each dir in the search path, finding completions.
      (setq dir           (file-name-as-directory (comint-directory (or (car path-dirs)  ".")))
            comps-in-dir  (and (file-accessible-directory-p dir)
                               (file-name-all-completions filenondir dir)))
      (while comps-in-dir               ; Go thru each completion, to see whether it should be used.
        (setq file           (car comps-in-dir)
              abs-file-name  (concat dir file))
        (when (and (not (member file completions))
                   (not (and ignored-extensions  (string-match ignored-extensions file)))
                   (or (string-equal dir cwd)  (not (file-directory-p abs-file-name)))
                   (or (null shell-completion-execonly)  (file-executable-p abs-file-name)))
          (setq completions  (cons file completions)))
        (setq comps-in-dir  (cdr comps-in-dir)))
      (setq path-dirs  (cdr path-dirs)))
    (let ((success  (let ((comint-completion-addsuffix  nil)
                          (icicle-candidate-help-fn
                           (lambda (cand)
                             (with-output-to-temp-buffer "*Help*"
                               (princ (shell-command-to-string (concat "apropos "
                                                                       (shell-quote-argument cand))))))))
                      (icicle-comint-dynamic-simple-complete filenondir completions))))
      (when (and (memq success '(sole shortest))  comint-completion-addsuffix
                 (not (file-directory-p (comint-match-partial-filename))))
        (insert " "))
      success)))

;;;###autoload (autoload 'icicle-comint-replace-by-expanded-filename "icicles")
(defun icicle-comint-replace-by-expanded-filename (&optional replace-to-eol-p)
  "Dynamically complete, expand, and canonicalize the filename at point.
With a prefix arg, replace everthing past point on the current line.
Otherwise, replace only the filename-matching text before point.

Like vanilla `comint-replace-by-expanded-filename', but uses Icicles
completion."
  (interactive "P")
  (let ((filename  (comint-match-partial-filename)))
    (when filename
      (replace-match (expand-file-name filename) t t)
      (icicle-comint-dynamic-complete-filename replace-to-eol-p))))

(defun icicle-comint-dynamic-simple-complete (stub candidates)
  "Dynamically complete STUB from CANDIDATES list.
Inserts completion characters at point by completing STUB from the
strings in CANDIDATES.  Uses Icicles completion if completion is
ambiguous.

Returns nil if no completion was inserted.
Returns `sole' if completed with the only completion match.
Returns `shortest' if completed with the shortest of the completion matches.
Returns `partial' if completed as far as possible with the completion matches.
Returns `listed' if a completion listing was shown.

See also `icicle-comint-dynamic-complete-filename'."
  (let* ((completion-ignore-case  (memq system-type '(ms-dos windows-nt cygwin)))
         (minibuffer-p            (window-minibuffer-p (selected-window)))
         (suffix                  (cond ((not comint-completion-addsuffix)         "")
                                        ((not (consp comint-completion-addsuffix)) " ")
                                        (t  (cdr comint-completion-addsuffix))))
         (candidates              (mapcar #'list candidates))
         (completions             (all-completions stub candidates)))
    (cond ((null completions)
           (if minibuffer-p
               (minibuffer-message (format " [No completions of `%s']" stub))
             (message "No completions of `%s'" stub))
           nil)
          ((= 1 (length completions))
           (let ((completion  (car completions)))
             (if (string-equal completion stub)
                 (unless minibuffer-p (message "Sole completion"))
               (insert (substring completion (length stub)))
               (unless minibuffer-p (message "Completed")))
             (insert suffix)
             'sole))
          (t                            ; There's no unique completion.
           (let ((completion  (try-completion stub candidates)))
             ;; Insert the longest substring.
             (insert (substring completion (length stub)))
             (cond ((and comint-completion-recexact  comint-completion-addsuffix
                         (string-equal stub completion)
                         (member completion completions))
                    (insert suffix)     ; User wants shortest match.
                    (unless minibuffer-p (message "Completed shortest"))
                    'shortest)
                   ((or comint-completion-autolist  (string-equal stub completion))
                    (icicle-condition-case-no-debug nil ;  Let user choose a completion.
                        (let* ((icicle-show-Completions-initially-flag      t)
                               (icicle-incremental-completion-p             'display)
                               (icicle-top-level-when-sole-completion-flag  t)
                               (choice (save-excursion
                                         (completing-read "Complete: " (mapcar #'list completions)
                                                          nil t nil nil completion))))
                          (when choice
                            (delete-backward-char (length completion))
                            (insert choice suffix)))
                      (error nil))
                    'listed)
                   (t
                    (unless minibuffer-p (message "Partially completed"))
                    'partial)))))))

;;;###autoload (autoload 'icicle-shell-dynamic-complete-filename "icicles")
(defun icicle-shell-dynamic-complete-filename ()
  "Dynamically complete the filename at point.
Completes only if point is at a suitable position for a filename
argument."
  (interactive)
  (let ((opoint  (point))
        (beg     (comint-line-beginning-position)))
    (when (save-excursion
            (goto-char (if (re-search-backward "[;|&]" beg t) (match-end 0) beg))
            (re-search-forward "[^ \t][ \t]" opoint t))
      (icicle-comint-dynamic-complete-as-filename))))

;;;###autoload (autoload 'icicle-shell-dynamic-complete-environment-variable "icicles")
(defun icicle-shell-dynamic-complete-environment-variable ()
  "`shell-dynamic-complete-environment-variable' but uses Icicles completion."
  (interactive)
  (require 'shell)
  (let ((variable  (shell-match-partial-variable)))
    (if (and variable  (string-match "^\\$" variable))
        (prog2 (unless (window-minibuffer-p (selected-window))
                 (message "Completing variable name..."))
            (icicle-shell-dynamic-complete-as-environment-variable)))))

(defun icicle-shell-dynamic-complete-as-environment-variable ()
  "`shell-dynamic-complete-as-environment-variable' but uses Icicles completion."
  (require 'shell)
  (let* ((var                          (or (shell-match-partial-variable)  ""))
         (variable                     (substring var (or (string-match "[^$({]\\|$" var)  0)))
         (variables                    (mapcar (lambda (x) (substring x 0 (string-match "=" x)))
                                               process-environment))
         (addsuffix                    comint-completion-addsuffix)
         (comint-completion-addsuffix  nil)
         (success                      (icicle-comint-dynamic-simple-complete variable variables)))
    (if (memq success '(sole shortest))
        (let* ((var           (shell-match-partial-variable))
               (variable      (substring var (string-match "[^$({]" var)))
               (protection    (cond ((string-match "{" var)                                    "}")
                                    ((string-match "(" var)                                    ")")
                                    (t                                                         "")))
               (suffix        (cond ((null addsuffix)                                          "")
                                    ((file-directory-p (comint-directory (getenv variable)))   "/")
                                    (t                                                         " "))))
          (insert protection  suffix)))
    success))


;; Save vanilla `file' widget as `icicle-ORIG-file' widget, for restoring when you quit Icicle mode.
(unless (get 'icicle-ORIG-file 'widget-type)
  (put 'icicle-ORIG-file 'widget-type (get 'file 'widget-type))
  (put 'icicle-ORIG-file 'widget-documentation (get 'file 'widget-documentation)))

;;;###autoload
(define-widget 'icicle-file 'string
  "Icicles version of the `file' widget.
Reads a file name from an editable text field, with Icicles completion."
  ;; `icicle-widget-file-complete' handles both nil and non-nil `icicle-mode'.
  ;; Use the following instead of:
  ;;   :completions #'completion-file-name-table
  :complete-function #'icicle-widget-file-complete
  :prompt-value 'widget-file-prompt-value
  :format "%{%t%}: %v"
  ;; Vanilla Emacs comment: This does not work well with terminating newline.
  ;;                        :value-face 'widget-single-line-field
  :tag "File")

;;;###autoload (autoload 'icicle-widget-file-complete "icicles")
(defun icicle-widget-file-complete (&optional replace-to-eol-p)
  "Perform Icicles completion on the file name at point.
Like `widget-file-complete' (`widget-complete', for Emacs 24+), but
allows Icicles completion.

With a prefix arg, replace everthing past point on the current line.
Otherwise, replace only the filename-matching text before point."
  (interactive "P")
  (if (and (boundp 'icicle-mode)  icicle-mode)
      (let ((comint-completion-addsuffix  nil)) ; Do not append a space.
        (icicle-comint-dynamic-complete-filename replace-to-eol-p))
    (cond ((> emacs-major-version 23)
           ;; Vanilla Emacs 24+ `file' widget just has this:
           ;;   :completions #'completion-file-name-table
           ;; But we need the equivalent using `:complete-function', not `:completions'.
           ;; This is it - this is in fact the Emacs 23 `widget-file-complete'.
           ;; See `widget-default-completions' for the relations between keywords
           ;; `:completions' and `:complete-function'.
           (let* ((field  (widget-field-find (point)))
                  (start  (widget-field-start field))
                  (end    (max (point) (widget-field-text-end field))))
             (completion-in-region start end #'completion-file-name-table)))
          (t
           (widget-file-complete)))))

;;;###autoload (autoload 'icicle-ess-complete-object-name "icicles")
(defun icicle-ess-complete-object-name (&optional listcomp)
  "`ess-complete-object-name', but uses Icicles completion.
Complete `ess-language' object preceding point.
This is `icicle-ess-R-complete-object-name' if `ess-use-R-completion',
and `icicle-ess-internal-complete-object-name' otherwise."
  (interactive "P")
  (if ess-use-R-completion
      (icicle-ess-R-complete-object-name)
    (icicle-ess-internal-complete-object-name listcomp)))

;;;###autoload (autoload 'icicle-ess-internal-complete-object-name "icicles")
(defun icicle-ess-internal-complete-object-name (&optional listcomp)
  "`ess-internal-complete-object-name', but uses Icicles completion.
Complete `ess-language' object preceding point."
  (interactive "P")
  (ess-make-buffer-current)
  (if (memq (char-syntax (preceding-char)) '(?w ?_))
      (let* ((comint-completion-addsuffix  nil)
             (end                          (point))
             (buffer-syntax                (syntax-table))
             (beg                          (unwind-protect
                                                (save-excursion
                                                  (set-syntax-table ess-mode-syntax-table)
                                                  (backward-sexp 1)
                                                  (point))
                                             (set-syntax-table buffer-syntax)))
             (full-prefix                  (buffer-substring beg end))
             (pattern                      full-prefix)
             (listname                  ; See if we're indexing a list with `$'
              (and (string-match "\\(.+\\)\\$\\(\\(\\sw\\|\\s_\\)*\\)$" full-prefix)
                   (setq pattern  (if (not (match-beginning 2))
                                      ""
                                    (substring full-prefix (match-beginning 2) (match-end 2))))
                   (substring full-prefix (match-beginning 1) (match-end 1))))
             (classname                 ; Are we trying to get a slot via `@' ?
              (and (string-match "\\(.+\\)@\\(\\(\\sw\\|\\s_\\)*\\)$" full-prefix)
                   (setq pattern  (if (not (match-beginning 2))
                                      ""
                                    (substring full-prefix (match-beginning 2) (match-end 2))))
                   (progn (ess-write-to-dribble-buffer (format "(ess-C-O-Name : slots..) : patt=%s"
                                                               pattern))
                          (substring full-prefix (match-beginning 1) (match-end 1)))))
             (components
              (if listname
                  (ess-object-names listname)
                (if classname
                    (ess-slot-names classname)
                  ;; Default case: It hangs here when options (error=recoves):
                  (ess-get-object-list ess-current-process-name)))))
        ;; Return non-nil to prevent history expansions
        (or (icicle-comint-dynamic-simple-complete  pattern components)  'none))))

(defun icicle-ess-complete-filename ()
  "`ess-complete-filename', but uses Icicles completion.
Do file completion only within strings, or when `!' call is used."
  (if (comint-within-quotes
       (1- (process-mark (get-buffer-process (current-buffer)))) (point))
      (progn (if (featurep 'xemacs)
                 (icicle-comint-dynamic-complete-filename) ; Work around XEmacs bug.  GNU Emacs and
               (icicle-comint-replace-by-expanded-filename)) ; a working XEmacs return t in a string
             t)))

;;;###autoload (autoload 'icicle-ess-R-complete-object-name "icicles")
(defun icicle-ess-R-complete-object-name ()
  "`ess-R-complete-object-name', but uses Icicles completion.
Completion in R."
  (interactive)
  (ess-make-buffer-current)
  (let* ((comint-completion-addsuffix  nil)
         (bol                          (save-excursion (comint-bol nil) (point)))
         (eol                          (line-end-position))
         (line-buffer                  (buffer-substring bol eol))
         (NS                           (if (ess-current-R-at-least '2.7.0)
                                           "utils:::"
                                         "rcompgen:::"))
         (token-string                  ; Setup, including computation of the token
          (progn
            (ess-command (format (concat NS ".assignLinebuffer('%s')\n") line-buffer))
            (ess-command (format (concat NS ".assignEnd(%d)\n") (- (point) bol)))
            (car (ess-get-words-from-vector (concat NS ".guessTokenFromLine()\n")))))
         (possible-completions          ; Compute and retrieve possible completions
          (progn
            (ess-command (concat NS ".completeToken()\n"))
            (ess-get-words-from-vector (concat NS ".retrieveCompletions()\n")))))
    (or (icicle-comint-dynamic-simple-complete token-string possible-completions)  'none)))

;;;###autoload (autoload 'icicle-gud-gdb-complete-command "icicles")
(defun icicle-gud-gdb-complete-command (&optional command a b)
  "`gud-gdb-complete-command', but uses Icicles completion.
Perform completion on the GDB command preceding point."
  (interactive)
  (if command
      (setq command  (concat "p " command)) ; Used by gud-watch in mini-buffer.
    (let ((end  (point)))               ; Used in GUD buffer.
      (setq command  (buffer-substring (comint-line-beginning-position) end))))
  (let* ((command-word
          ;; Find the word break.  This match will always succeed.
          (and (string-match "\\(\\`\\| \\)\\([^ ]*\\)\\'" command)
               (substring command (match-beginning 2))))
         (complete-list
          (gud-gdb-run-command-fetch-lines (concat "complete " command)
                                           (current-buffer)
                                           ;; From string-match above.
                                           (match-beginning 2))))
    ;; Protect against old versions of GDB.
    (and complete-list
         (string-match "^Undefined command: \"complete\"" (car complete-list))
         (error "This version of GDB doesn't support the `complete' command"))
    ;; Sort the list like readline.
    (setq complete-list  (sort complete-list (function string-lessp)))
    ;; Remove duplicates.
    (let ((first   complete-list)
          (second  (cdr complete-list)))
      (while second
        (if (string-equal (car first) (car second))
            (setcdr first (setq second  (cdr second)))
          (setq first   second
                second  (cdr second)))))
    ;; Add a trailing single quote if there is a unique completion
    ;; and it contains an odd number of unquoted single quotes.
    (and (= (length complete-list) 1)
         (let ((str    (car complete-list))
               (pos    0)
               (count  0))
           (while (string-match "\\([^'\\]\\|\\\\'\\)*'" str pos)
             (setq count  (1+ count)
                   pos    (match-end 0)))
           (and (= (mod count 2) 1)
                (setq complete-list  (list (concat str "'"))))))
    ;; Let comint handle the rest.
    (icicle-comint-dynamic-simple-complete command-word complete-list)))


;; REPLACE ORIGINAL `dabbrev-completion' defined in `dabbrev.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; You can complete from an empty abbrev also.
;; Uses Icicles completion when there are multiple candidates.
;;
(when (and (fboundp 'dabbrev-completion)  (not (fboundp 'icicle-ORIG-dabbrev-completion)))
  (defalias 'icicle-ORIG-dabbrev-completion (symbol-function 'dabbrev-completion)))

;;;###autoload (autoload 'icicle-dabbrev-completion "icicles")
(defun icicle-dabbrev-completion (&optional arg) ; Bound to `C-M-/' globally.
  "Completion on current word.
Like \\[dabbrev-expand], but finds all expansions in the current buffer
and presents suggestions for completion.

With a prefix argument, it searches all buffers accepted by
`dabbrev-friend-buffer-function', to find the completions.

If the prefix argument is 16 (which comes from `C-u C-u'), then it
searches *ALL* buffers.

With no prefix argument, it reuses an old completion list
if there is a suitable one already."
  (interactive "*P")
  (unless (featurep 'dabbrev)
    (unless (require 'dabbrev nil t) (error "Library `dabbrev' not found"))
    (icicle-mode 1))                    ; Redefine `dabbrev-completion' to Icicles version.
  (dabbrev--reset-global-variables)
  (let* ((dabbrev-check-other-buffers  (and arg  t)) ; Must be t
         (dabbrev-check-all-buffers    (and arg  (= (prefix-numeric-value arg) 16)))
         (abbrev                       (icicle-dabbrev--abbrev-at-point))
         (ignore-case-p                (and (if (eq dabbrev-case-fold-search 'case-fold-search)
                                                case-fold-search
                                              dabbrev-case-fold-search)
                                            (or (not dabbrev-upcase-means-case-search)
                                                (string= abbrev (downcase abbrev)))))
         (my-obarray                   dabbrev--last-obarray)
         init)
    ;; If new abbreviation to expand, then expand it.
    (save-excursion
      (unless (and (null arg)
                   my-obarray
                   (or (eq dabbrev--last-completion-buffer (current-buffer))
                       (and (window-minibuffer-p (selected-window))
                            (eq dabbrev--last-completion-buffer (dabbrev--minibuffer-origin))))
                   dabbrev--last-abbreviation
                   (>= (length abbrev) (length dabbrev--last-abbreviation))
                   (string= dabbrev--last-abbreviation
                            (substring abbrev 0 (length dabbrev--last-abbreviation)))
                   (setq init  (try-completion abbrev my-obarray)))
        (setq dabbrev--last-abbreviation  abbrev)
        (let ((completion-list         (dabbrev--find-all-expansions abbrev ignore-case-p))
              (completion-ignore-case  ignore-case-p))
          ;; Make an obarray with all expansions
          (setq my-obarray  (make-vector (length completion-list) 0))
          (unless (> (length my-obarray) 0)
            (error "No dynamic expansion for \"%s\" found%s" abbrev
                   (if dabbrev--check-other-buffers "" " in this-buffer")))
          (dolist (string  completion-list)
            (cond ((or (not ignore-case-p)  (not dabbrev-case-replace))
                   (intern string my-obarray))
                  ((string= abbrev (icicle-upcase abbrev))
                   (intern (icicle-upcase string) my-obarray))
                  ((string= (substring abbrev 0 1) (icicle-upcase (substring abbrev 0 1)))
                   (intern (capitalize string) my-obarray))
                  (t (intern (downcase string) my-obarray))))
          (setq dabbrev--last-obarray            my-obarray
                dabbrev--last-completion-buffer  (current-buffer)
                ;; Find the expanded common string.
                init                             (try-completion abbrev my-obarray)))))
    ;; Let the user choose between the expansions
    (unless (stringp init) (setq init  abbrev))
    (cond
      ((and (not (string-equal init ""))
            (not (string-equal (downcase init) (downcase abbrev)))
            (<= (length (all-completions init my-obarray)) 1))
       (message "Completed (no other completions)")
       (if (< emacs-major-version 21)
           (dabbrev--substitute-expansion nil abbrev init)
         (dabbrev--substitute-expansion nil abbrev init nil))
       (when (window-minibuffer-p (selected-window)) (message nil)))
;;$$       ;; Complete text only up through the common root. NOT USED.
;;       ((and icicle-dabbrev-stop-at-common-root-p
;;             (not (string-equal init ""))
;;             (not (string-equal (downcase init) (downcase abbrev))))
;;        (message "Use `%s' again to complete further"
;;                 (icicle-key-description (this-command-keys) nil icicle-key-descriptions-use-<>-flag))
;;        (if (< emacs-major-version 21)
;;            (dabbrev--substitute-expansion nil abbrev init)
;;          (dabbrev--substitute-expansion nil abbrev init nil))
;;        (when (window-minibuffer-p (selected-window)) (message nil))) ; $$ NEEDED?
      (t
       ;; String is a common root already.  Use Icicles completion.
       (icicle-highlight-lighter)
       (message "Making completion list...")
       (search-backward abbrev)
       (replace-match "")
       (condition-case nil
           (let* ((icicle-show-Completions-initially-flag  t)
                  (icicle-incremental-completion-p         'display)
                  (minibuffer-completion-table             my-obarray)
                  (choice
                   (completing-read "Complete: " my-obarray nil nil init nil init)))
             (when choice (insert choice)))
         (quit (insert abbrev)))))))

(defun icicle-dabbrev--abbrev-at-point ()
  "Like `dabbrev--abbrev-at-point', but returns \"\" if there is no match.
Vanilla `dabbrev--abbrev-at-point' raises an error if no match."
  (let ((abv ""))
    (setq dabbrev--last-abbrev-location  (point)) ; Record the end of the abbreviation.
    (unless (bobp)
      (save-excursion                   ; Return abbrev at point
        ;; If we aren't right after an abbreviation, move point back to just after one.
        ;; This is so the user can get successive words by typing the punctuation followed by M-/.
        (save-match-data
          (when (and (save-excursion
                       (forward-char -1)
                       (not (looking-at
                             (concat "\\(" (or dabbrev-abbrev-char-regexp  "\\sw\\|\\s_") "\\)+"))))
                     (re-search-backward (or dabbrev-abbrev-char-regexp  "\\sw\\|\\s_") nil t))
            (forward-char 1)))
        (dabbrev--goto-start-of-abbrev) ; Now find the beginning of that one.
        (setq abv  (buffer-substring-no-properties dabbrev--last-abbrev-location (point)))))
    abv))


;; REPLACE ORIGINAL `bbdb-complete-mail' defined in `bbdb-com.el', version 3.02
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; BBDB Version 3.02, the Insidious Big Brother Database, is available here: http://melpa.milkbox.net/.
;;
;; Uses Icicles completion when there are multiple candidates.
;;
;; Free vars here: `bbdb-*' are bound in `bbdb-com.el'.
;;;###autoload (autoload 'icicle-bbdb-complete-mail "icicles")
(defun icicle-bbdb-complete-mail (&optional start-pos cycle-completion-buffer)
  "In a mail buffer, complete the user name or mail address before point.
Completes up to the preceding newline, colon or comma, or the value of
START-POS.
Returns non-nil if there is a valid completion, else return nil.
You can control completion behaviour using `bbdb-completion-list'
\(`bbdb-completion-type' in older BBDB versions).

If what has been typed is unique, insert an entry \"User Name
<mail-address>\" - but see `bbdb-mail-allow-redundancy'
\(`bbdb-dwim-net-address-allow-redundancy' in older BBDB versions).
If it is a valid completion but not unique, you can choose from the
list of completions using Icicles completion.

If your input is completed and `bbdb-complete-mail-allow-cycling' is
true (`bbdb-complete-name-allow-cycling' for older BBDB versions),
you can repeat to cycle through the nets for the matching record.

When called with a prefix arg, display a list of all mail messages
available for cycling.

See your version of BBDB for more information."
  (interactive (list nil current-prefix-arg))
  (unless (and (require 'bbdb nil t)  (require 'bbdb-com nil t)
               (fboundp 'bbdb-complete-mail))
    (error "`icicle-bbdb-complete-mail' requires a BBDB version such as 3.02"))
  (bbdb-buffer)                         ; Make sure database is initialized.
  (lexical-let* ((end                     (point))
                 (beg                     (or start-pos  (save-excursion
                                                           (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                                                           (goto-char (match-end 0)) (point))))
                 (orig                    (buffer-substring beg end))
                 (typed                   (downcase orig))
                 (pattern                 (bbdb-string-trim typed))
                 (completion-ignore-case  t)
                 (completion              (try-completion pattern bbdb-hashtable #'bbdb-completion-predicate))
                 (all-the-completions     ())
                 dwim-completions  one-record  done)
    ;; [:,] match would be interpreted as START-POS (e.g., a comma in LF-NAME).  Compensate.
    (when (and (stringp completion)  (string-match "[:,]" completion))
      (setq completion  (substring completion 0 (match-beginning 0))))
    ;; Cannot use `all-completions' to set `all-the-completions' because it converts symbols to strings.
    (all-completions pattern bbdb-hashtable (lambda (sym)
                                              (when (bbdb-completion-predicate sym)
                                                (push sym all-the-completions))))
    ;; Resolve records matching pattern.  Multiple completions could match the same record.
    (let ((records  (icicle-delete-dups (apply #'append (mapcar #'symbol-value all-the-completions)))))
      (setq one-record  (and (not (cdr records))  (car records)))) ; Only one matching record.
    (icicle-remove-Completions-window)
    (cond (one-record
           ;; Only one matching record.
           ;; Determine mail address of ONE-RECORD to use for ADDRESS.
           ;; Do we have a preferential order for the following tests?
           (let ((completion-list  (if (eq t bbdb-completion-list)
                                       '(fl-name lf-name mail aka organization)
                                     bbdb-completion-list))
                 (mails            (bbdb-record-mail one-record))
                 mail elt)
             (unless mails (error "Matching record has no `mail' field"))
             ;; (1) If PATTERN matches name, AKA, or organization of ONE-RECORD,
             ;;     then ADDRESS is the first mail address of ONE-RECORD.
             (if (try-completion pattern (append (and (memq 'fl-name completion-list)
                                                      (list (or (bbdb-record-name one-record)  "")))
                                                 (and (memq 'lf-name completion-list)
                                                      (list (or (bbdb-record-name-lf one-record)  "")))
                                                 (and (memq 'aka completion-list)
                                                      (bbdb-record-field one-record 'aka-all))
                                                 (and (memq 'organization completion-list)
                                                      (bbdb-record-organization one-record))))
                 (setq mail  (car mails)))
             ;; (2) If PATTERN matches one or multiple mail addresses of ONE-RECORD,
             ;;     then we take the first one matching PATTERN.
             (unless mail (while (setq elt  (pop mails))
                            (if (try-completion pattern (list elt))
                                (setq mail   elt
                                      mails  ()))))
             (unless mail (error "ICICLE-BBDB-COMPLETE-MAIL: No match for `%s'" pattern)) ; Indicates a bug!
             (let ((address  (bbdb-dwim-mail one-record mail)))
               (if (string= address (buffer-substring-no-properties beg end))
                   (unless (and bbdb-complete-mail-allow-cycling  (< 1 (length (bbdb-record-mail one-record))))
                     (setq done  'UNCHANGED))
                 (delete-region beg end) ; Replace text with expansion.
                 (insert address)
                 (bbdb-complete-mail-cleanup address)
                 (setq done  'UNIQUE)))))
          ;; Completed partially.
          ;; Cannot use trimmed version of pattern here, else recurse infinitely on, e.g., common first names.
          ((and (stringp completion)  (not (string= typed completion)))
           (delete-region beg end)
           (insert completion)
           (setq done  'PARTIAL))
          ;; Partial match not allowing further partial completion.
          (completion
           (let ((completion-list  (if (eq t bbdb-completion-list)
                                       '(fl-name lf-name mail aka organization)
                                     bbdb-completion-list))
                 sname  records)
             ;; Collect dwim-addresses for each completion, but only once for each record!
             ;; Add if mail is part of the completions.
             (dolist (sym  all-the-completions)
               (setq sname  (symbol-name sym))
               (dolist (record  (symbol-value sym))
                 (unless (memq record records)
                   (push record records)
                   (let ((mails  (bbdb-record-mail record))
                         accept)
                     (when mails
                       (dolist (field  completion-list)
                         (if (case field
                               (fl-name (bbdb-string= sname (bbdb-record-name record)))
                               (lf-name (bbdb-string= sname (bbdb-cache-lf-name (bbdb-record-cache record))))
                               (aka (member-ignore-case sname (bbdb-record-field record 'aka-all)))
                               (organization (member-ignore-case sname (bbdb-record-organization record)))
                               (primary (bbdb-string= sname (car mails)))
                               (otherwise nil))
                             (push (car mails) accept)
                           (when (eq field 'mail)
                             (dolist (mail  mails)
                               (when (bbdb-string= sname mail) (push mail accept))))))
                       (when accept
                         ;; If DWIM-COMPLETIONS contains only one element, set DONE to `UNIQUE' (see below)
                         ;;  and we want to know ONE-RECORD.
                         (setq one-record  record)
                         (dolist (mail  (delete-dups accept))
                           (push (bbdb-dwim-mail record mail) dwim-completions))))))))
             (cond ((not dwim-completions) (error "No mail address for \"%s\"" orig))
                   ;; DWIM-COMPLETIONS might contain only one element, if multiple completions match the
                   ;; same record.  In that case proceed with DONE set to `UNIQUE'.
                   ((eq 1 (length dwim-completions))
                    (delete-region beg end)
                    (insert (car dwim-completions))
                    (bbdb-complete-mail-cleanup (car dwim-completions))
                    (setq done  'UNIQUE))
                   (t
                    (setq done  'CHOOSE))))))
    ;; If no completion so far, consider cycling.
    ;; Completion is controlled by option `bbdb-completion-list'.  Cycling assumes that ORIG already holds
    ;; a valid RFC 822 mail address.  So cycling can consider different records than completion.
    (when (and (not done)  bbdb-complete-mail-allow-cycling)
      ;; Find the record we are working on.
      (let* ((address  (mail-extract-address-components orig))
             (record   (and (listp address)  (car (bbdb-message-search (nth 0 address) (nth 1 address)))))
             (mails    (and record  (bbdb-record-mail record))))
        (when mails
          ;; Cycle, even if MAILS has only one address. `bbdb-dwim-mail' can give something different.
          ;; E.g., header "JOHN SMITH <FOO@BAR.COM>" can be replaced by "John Smith <foo@bar.com>".
          (cond ((and (= 1 (length mails))  (string= (bbdb-dwim-mail record (car mails))
                                                     (buffer-substring-no-properties beg end)))
                 (setq done  'UNCHANGED))
                (cycle-completion-buffer ; Use completion buffer.
                 (setq dwim-completions  (mapcar (lambda (n) (bbdb-dwim-mail record n)) mails)
                       done              'CHOOSE))
                (t                      ; Use next mail
                 (let ((mail  (or (nth 1 (or (icicle-member-ignore-case (nth 1 address) mails)
                                             (icicle-member-ignore-case orig mails)))
                                  (nth 0 mails))))
                   (delete-region beg end) ; Replace with new mail address
                   (insert (bbdb-dwim-mail record mail))
                   (setq done  'CYCLE)))))))
    (when (eq done 'CHOOSE)
      ;; Icicles completion.  `completion-in-region' does not work here as `dwim-completions' is not a
      ;; collection for completion in the usual sense.  It is really a list of replacements.
      (unless (eq (selected-window) (minibuffer-window)) (message "Making completion list..."))
      (icicle-condition-case-no-debug nil
          (let* ((icicle-show-Completions-initially-flag      t)
                 (icicle-incremental-completion-p             'display)
                 (icicle-top-level-when-sole-completion-flag  t)
                 (completion-ignore-case                      t)
                 (choice
                  (save-excursion (completing-read "Complete: " (mapcar #'list dwim-completions)
                                                   nil t pattern nil pattern))))
            (when choice
              (delete-region beg end)
              (insert choice)))
        (error nil))
      (unless (eq (selected-window) (minibuffer-window)) (message "Making completion list...done")))
    done))


;; REPLACE ORIGINAL `bbdb-complete-name' defined in `bbdb-com.el' version 2.35,
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Version 2.35 is an older version of BBDB, the Insidious Big Brother Database, available here:
;;       http://bbdb.sourceforge.net/.
;;
;; Uses Icicles completion when there are multiple candidates.
;;
;; Free vars here: `bbdb-*' are bound in `bbdb-com.el'.
;;
;;;###autoload (autoload 'icicle-bbdb-complete-name "icicles")
;;
;; Avoid a byte-compile error if user has already loaded BBDB version 3+.
;; The error has to do with `bbdb-records' being a defsubst that takes no args.
(unless (eval-when-compile (and (featurep 'bbdb)  (not (string-lessp bbdb-version "3"))))
  (defun icicle-bbdb-complete-name (&optional start-pos)
    "Complete the user full-name or net-address before point.
Completes only up to the preceding newline, colon, or comma, or the
value of START-POS.

If what has been typed is unique, insert an entry of the form \"User
Name <net-addr>\" (but see `bbdb-dwim-net-address-allow-redundancy').
If it is a valid completion but not unique, you can choose from the
list of completions using Icicles completion.

If your input is completed and `bbdb-complete-name-allow-cycling' is
true, then you can repeat to cycle through the nets for the matching
record.

When called with a prefix arg, display a list of all nets.  You can
control completion behaviour using `bbdb-completion-type'."
    (interactive)
    (unless (and (require 'bbdb nil t)  (require 'bbdb-com nil t)
                 (fboundp 'bbdb-complete-name))
      (error "`icicle-bbdb-complete-name' requires a BBDB version such as 2.35"))
    (lexical-let* ((end                  (point))
                   (beg                  (or start-pos  (save-excursion (re-search-backward
                                                                         "\\(\\`\\|[\n:,]\\)[ \t]*")
                                                                        (goto-char (match-end 0)) (point))))
                   (orig                 (buffer-substring beg end))
                   (typed                (downcase orig))
                   (pattern              (bbdb-string-trim typed))
                   ;; DADAMS -
                   ;; Replaced `(bbdb-hashtable)' by its expansion (bbdb-with-db-buffer ... bbdb-hashtable),
                   ;; to avoid the silly macro altogether and simplify user byte-compiling a little.
                   (ht                   (bbdb-with-db-buffer (bbdb-records nil t) bbdb-hashtable))
                   ;; Make a list of possible completion strings (all-the-completions), and a flag to
                   ;; indicate if there's a single matching record or not (only-one-p).
                   (only-one-p           t)
                   (all-the-completions  ())
                   (pred
                    (lambda (sym)       ; FREE here: ALL-THE-COMPLETIONS, ONLY-ONE-P.
                      (and (bbdb-completion-predicate sym)
                           (progn
                             (when (and only-one-p
                                        all-the-completions
                                        (or
                                         ;; Not sure about this. More than one record attached to the symbol?
                                         ;; Does that happen?
                                         (> (length (symbol-value sym)) 1)
                                         ;; This is the doozy. Multiple syms which all match the same record.
                                         (delete t (mapcar (lambda (x) ; FREE here: SYM.
                                                             (equal (symbol-value x) (symbol-value sym)))
                                                           all-the-completions))))
                               (setq only-one-p  nil))
                             (if (memq sym all-the-completions)
                                 nil
                               (setq all-the-completions  (cons sym all-the-completions)))))))
                   (completion           (progn (all-completions pattern ht pred)
                                                (try-completion pattern ht)))
                   (exact-match          (eq completion t)))
      (cond
        ;; No matches found OR you're trying completion on an already-completed record.
        ;; In the latter case, we might have to cycle through the nets for that record.
        ((or (null completion)
             (and bbdb-complete-name-allow-cycling
                  exact-match           ; Which is a net of the record
                  (member orig (bbdb-record-net (car (symbol-value (intern-soft pattern ht)))))))
         (bbdb-complete-name-cleanup)   ; Clean up the completion buffer, if it exists
         (unless (catch 'bbdb-cycling-exit ; Check for cycling
                   ;; Jump straight out if we're not cycling
                   (unless bbdb-complete-name-allow-cycling (throw 'bbdb-cycling-exit nil))
                   ;; Find the record we're working on.
                   (lexical-let* ((addr  (funcall bbdb-extract-address-components-func orig))
                                  (rec  (and (listp addr)
                                             ;; For now, we ignore the case where this returns more than
                                             ;; one record.  Ideally, the last expansion would be stored
                                             ;; in a buffer-local variable, perhaps.
                                             (car (bbdb-search-intertwingle (caar addr)
                                                                            (car (cdar addr)))))))
                     (unless rec (throw 'bbdb-cycling-exit nil))
                     (if current-prefix-arg
                         ;; Use completion buffer
                         (let ((standard-output  (get-buffer-create "*Completions*")))
                           ;; A previously existing buffer has to be cleaned first
                           (with-current-buffer standard-output
                             (setq buffer-read-only  nil)
                             (erase-buffer))
                           (display-completion-list
                            (mapcar (lambda (n) (bbdb-dwim-net-address rec n)) ; FREE here: REC.
                                    (bbdb-record-net rec)))
                           (delete-region beg end)
                           (switch-to-buffer standard-output))
                       ;; Use next address
                       (let* ((addrs      (bbdb-record-net rec))
                              (this-addr  (or (cadr (member (car (cdar addr)) addrs))  (nth 0 addrs))))
                         (if (= (length addrs) 1)
                             (throw 'bbdb-cycling-exit t) ; No alternatives. don't signal an error.
                           ;; Replace with new mail address
                           (delete-region beg end)
                           (insert (bbdb-dwim-net-address rec this-addr))
                           (run-hooks 'bbdb-complete-name-hooks)
                           (throw 'bbdb-cycling-exit t))))))
           ;; FALL THROUGH.  Check mail aliases
           (when (and (or (not bbdb-expand-mail-aliases)  (not (expand-abbrev)))  bbdb-complete-name-hooks)
             (message "No completion for `%s'" pattern) (icicle-ding)))) ; no matches

        ;; Match for a single record. If cycling is enabled then we don't
        ;; care too much about the exact-match part.
        ((and only-one-p  (or exact-match  bbdb-complete-name-allow-cycling))
         (let* ((sym   (if exact-match (intern-soft pattern ht) (car all-the-completions)))
                (recs  (symbol-value sym))
                the-net match-recs lst primary matched)
           (while recs
             (when (bbdb-record-net (car recs))
               ;; Did we match on name?
               (let ((b-r-name  (or (bbdb-record-name (car recs))  "")))
                 (if (string= pattern (substring (downcase b-r-name) 0
                                                 (min (length b-r-name) (length pattern))))
                     (setq match-recs  (cons (car recs) match-recs)
                           matched     t)))
               ;; Did we match on aka?
               (unless matched
                 (setq lst  (bbdb-record-aka (car recs)))
                 (while lst
                   (if (string= pattern (substring (downcase (car lst)) 0
                                                   (min (length (downcase (car lst)))
                                                        (length pattern))))
                       (setq match-recs  (append match-recs (list (car recs)))
                             matched     t
                             lst         ())
                     (setq lst  (cdr lst)))))
               ;; Name didn't match name so check net matching
               (unless matched
                 (setq lst      (bbdb-record-net (car recs))
                       primary  t)      ; primary wins over secondary...
                 (while lst
                   (if (string= pattern (substring (downcase (car lst)) 0
                                                   (min (length (downcase (car lst)))
                                                        (length pattern))))
                       (setq the-net     (car lst)
                             lst         ()
                             match-recs  (if primary
                                             (cons (car recs) match-recs)
                                           (append match-recs (list (car recs))))))
                   (setq lst      (cdr lst)
                         primary  nil))))
             (setq recs     (cdr recs)  ; Next rec for loop.
                   matched  nil))
           (unless match-recs (error "Only exact matching record has net field"))
           ;; Replace the text with the expansion
           (delete-region beg end)
           (insert (bbdb-dwim-net-address (car match-recs) the-net))
           ;; If we're past fill-column, wrap at the previous comma.
           (when (and (bbdb-auto-fill-function)  (>= (current-column) fill-column))
             (let ((p  (point))
                   bol)
               (save-excursion
                 (setq bol  (line-beginning-position))
                 (goto-char p)
                 (when (search-backward "," bol t) (forward-char 1) (insert "\n   ")))))
           ;; Update the *BBDB* buffer if desired.
           (when bbdb-completion-display-record
             (let ((bbdb-gag-messages  t))
               (bbdb-pop-up-bbdb-buffer)
               (bbdb-display-records-1 match-recs t)))
           (bbdb-complete-name-cleanup)
           ;; Call the exact-completion hook
           (run-hooks 'bbdb-complete-name-hooks)))

        ;; Partial match.  Note: we can't use the trimmed version of the pattern here or
        ;; we'll recurse infinitely on e.g. common first names.
        ((and (stringp completion)  (not (string= typed completion)))
         (delete-region beg end)
         (insert completion)
         (setq end  (point))
         (let ((last                              "")
               (bbdb-complete-name-allow-cycling  nil))
           (while (and (stringp completion)  (not (string= completion last))
                       (setq last        completion
                             pattern     (downcase orig)
                             completion  (progn (all-completions pattern ht pred)
                                                (try-completion pattern ht))))
             (when (stringp completion) (delete-region beg end) (insert completion)))
           (bbdb-complete-name beg)))   ; RECURSE <================

        ;; Exact match, but more than one record
        (t
         (unless (eq (selected-window) (minibuffer-window)) (message "Making completion list..."))
         (lexical-let (dwim-completions uniq nets net name akas)
           ;; Collect all the dwim-addresses for each completion, but only once for each record.
           ;; Add if the net is part of the completions.
           (bbdb-mapc (lambda (sym)
                        (bbdb-mapc
                         ;; FREE here: AKAS, ALL-THE-COMPLETIONS, DWIM-COMPLETIONS, HT,
                         ;;            NAME, NET, NETS, SYM, UNIQ.
                         (lambda (rec)
                           (unless (member rec uniq)
                             (setq uniq  (cons rec uniq)
                                   nets  (bbdb-record-net rec)
                                   name  (downcase (or (bbdb-record-name rec)  ""))
                                   akas  (mapcar 'downcase (bbdb-record-aka rec)))
                             (while nets
                               (setq net  (car nets))
                               (when (cond
                                       ((and (member bbdb-completion-type ; Primary
                                                     '(primary primary-or-name))
                                             (member (intern-soft (downcase net) ht)
                                                     all-the-completions))
                                        (setq nets  ())
                                        t)
                                       ((and name (member bbdb-completion-type ; Name
                                                          '(nil name primary-or-name))
                                             (let ((cname  (symbol-name sym)))
                                               (or (string= cname name)  (member cname akas))))
                                        (setq name  nil)
                                        t)
                                       ((and (member bbdb-completion-type '(nil net)) ; Net
                                             (member (intern-soft (downcase net) ht) all-the-completions)))
                                       ;; (name-or-)primary
                                       ((and (member bbdb-completion-type '(name-or-primary))
                                             (let ((cname  (symbol-name sym)))
                                               (or (string= cname name)  (member cname akas))))
                                        (setq nets  ())
                                        t))
                                 (setq dwim-completions
                                       (cons (bbdb-dwim-net-address rec net)
                                             dwim-completions))
                                 (when exact-match (setq nets  ())))
                               (setq nets  (cdr nets)))))
                         (symbol-value sym)))
                      all-the-completions)
           (cond ((and dwim-completions  (null (cdr dwim-completions))) ; Insert the unique match.
                  (delete-region beg end) (insert (car dwim-completions)) (message ""))
                 (t                     ; More than one match.  Use Icicles minibuffer completion.
                  (icicle-condition-case-no-debug nil
                      (let* ((icicle-show-Completions-initially-flag      t)
                             (icicle-incremental-completion-p             'display)
                             (icicle-top-level-when-sole-completion-flag  t)
                             (completion-ignore-case                      t)
                             (choice
                              (save-excursion
                                (completing-read "Complete: " (mapcar #'list dwim-completions)
                                                 nil t pattern nil pattern))))
                        (when choice
                          (delete-region beg end)
                          (insert choice)))
                    (error nil))
                  (unless (eq (selected-window) (minibuffer-window))
                    (message "Making completion list...done"))))))))))


;; REPLACE ORIGINAL `lisp-complete-symbol' (< Emacs 23.2),
;; defined in `lisp.el', saving it for restoration when you toggle `icicle-mode'.
;;
;; Select `*Completions*' window even if on another frame.
;;
(unless (fboundp 'icicle-ORIG-lisp-complete-symbol)
  (defalias 'icicle-ORIG-lisp-complete-symbol (symbol-function 'lisp-complete-symbol)))

;;;###autoload (autoload 'icicle-lisp-complete-symbol "icicles")
(defun icicle-lisp-complete-symbol (&optional predicate) ; `M-TAB' (`C-M-i', `ESC-TAB'), globally.
  "Complete the Lisp symbol preceding point against known Lisp symbols.
If there is more than one completion, use the minibuffer to complete.

When called from a program, optional arg PREDICATE is a predicate
determining which symbols are considered, e.g. `commandp'.

If PREDICATE is nil, the context determines which symbols are
considered.  If the symbol starts just after an open-parenthesis, only
symbols with function definitions are considered.  Otherwise, all
symbols with function definitions, values or properties are
considered."
  (interactive)
  (let* ((end            (point))
         (buffer-syntax  (syntax-table))
         (beg            (unwind-protect
                              (save-excursion
                                (set-syntax-table emacs-lisp-mode-syntax-table)
                                (backward-sexp 1)
                                (while (= (char-syntax (following-char)) ?\') (forward-char 1))
                                (point))
                           (set-syntax-table buffer-syntax)))
         (pattern       (buffer-substring beg end))
         (new           (try-completion pattern obarray)))
    (unless (stringp new) (setq new  pattern))
    (delete-region beg end)
    (insert new)
    (setq end  (+ beg (length new)))
    (if (and (not (string= new ""))  (not (string= (downcase new) (downcase pattern)))
             (< (length (all-completions new obarray)) 2))
        (message "Completed (no other completions)")
      ;; Use minibuffer to choose a completion.
      (let* ((enable-recursive-minibuffers                (active-minibuffer-window))
             (icicle-top-level-when-sole-completion-flag  t)
             (icicle-orig-window                          (selected-window)) ; For alt actions.
             (alt-fn                                      nil)
             (icicle-show-Completions-initially-flag      t)
             (icicle-candidate-alt-action-fn
              (or icicle-candidate-alt-action-fn  (setq alt-fn  (icicle-alt-act-fn-for-type "symbol"))))
             (icicle-all-candidates-list-alt-action-fn ; M-|'
              (or icicle-all-candidates-list-alt-action-fn alt-fn
                  (icicle-alt-act-fn-for-type "symbol")))
             (predicate
              (or predicate
                  (save-excursion
                    (goto-char beg)
                    (if (not (eq (char-before) ?\( ))
                        (lambda (sym) ;why not just nil ?   -sm
                          (or (boundp sym)  (fboundp sym)  (symbol-plist sym)))
                      ;; If first element of parent list is not an open paren, assume that this is a
                      ;; funcall position: use `fboundp'.  If not, then maybe this is a variable in
                      ;; a `let' binding, so no predicate: use nil.
                      (and (not (condition-case nil
                                    (progn (up-list -2) (forward-char 1) (eq (char-after) ?\( ))
                                  (error nil)))
                           'fboundp))))))
        ;; $$$$$ Could bind `icicle-must-pass-after-match-predicate' to a predicate on interned
        ;;       candidate and pass nil as PRED to `completing-read'.  Don't bother for now.
        (setq new  (save-excursion (completing-read "Complete Lisp symbol: "
                                                    obarray predicate t new)))))
    (delete-region beg end)
    (insert new)))


;; REPLACE ORIGINAL `lisp-completion-at-point' (>= Emacs 23.2),
;; defined in `lisp.el', saving it for restoration when you toggle `icicle-mode'.
;;
;; Select `*Completions*' window even if on another frame.
;;
(when (fboundp 'completion-at-point)    ; Emacs 23.2+.
  (unless (fboundp 'icicle-ORIG-lisp-completion-at-point)
    (defalias 'icicle-ORIG-lisp-completion-at-point (symbol-function 'lisp-completion-at-point))
    ;; Return a function that does all of the completion.
    (defun icicle-lisp-completion-at-point () #'icicle-lisp-complete-symbol)))

;;;###autoload (autoload 'icicle-customize-icicles-group "icicles")
(defun icicle-customize-icicles-group ()
  "Customize Icicles options and faces.  View their documentation."
  (interactive)
  (customize-group-other-window 'Icicles))

;;;###autoload (autoload 'icicle-send-bug-report "icicles")
(defun icicle-send-bug-report ()
  "Send a bug report about an Icicles problem."
  (interactive)
  (browse-url (format (concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
Icicles bug: \
&body=Describe bug below, using a precise recipe that starts with `emacs -Q' or `emacs -q'.  \
Each Icicles file has a header `Update #' that you can use to identify it.\
%%0A%%0AEmacs version: %s.")
                      (emacs-version))))


;; REPLACE ORIGINAL `customize-face-other-window' defined in `cus-edit.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Multi-command version.
;;
(unless (fboundp 'icicle-ORIG-customize-face-other-window)
  (defalias 'icicle-ORIG-customize-face-other-window (symbol-function 'customize-face-other-window)))

;;;###autoload (autoload 'icicle-customize-face-other-window "icicles")
(defun icicle-customize-face-other-window (face)
  "Customize face FACE in another window.
Same as `icicle-customize-face' except it uses a different window."
  (interactive
   (list (let* ((icicle-multi-completing-p             t)
                (icicle-list-use-nth-parts             '(1))
                (icicle-candidate-action-fn
                 (lambda (x)
                   (icicle-ORIG-customize-face-other-window (intern (icicle-transform-multi-completion x)))
                   (select-window (minibuffer-window))
                   (select-frame-set-input-focus (selected-frame))))
                (icicle-all-candidates-list-action-fn  'icicle-customize-faces)
                (icicle-orig-window                    (selected-window)) ; For alt actions.
                (alt-fn                                nil)
                (icicle-candidate-alt-action-fn
                 (or icicle-candidate-alt-action-fn
                     (setq alt-fn  (icicle-alt-act-fn-for-type "face"))))
                (icicle-all-candidates-list-alt-action-fn ; M-|'
                 (or icicle-all-candidates-list-alt-action-fn
                     alt-fn
                     (icicle-alt-act-fn-for-type "face"))))
           (if (and (> emacs-major-version 21)  current-prefix-arg)
               (read-face-name "Customize face: " "all faces" t)
             (read-face-name "Customize face: ")))))
  (icicle-ORIG-customize-face-other-window face))


;; REPLACE ORIGINAL `customize-face' defined in `cus-edit.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Multi-command version.
;;
(unless (fboundp 'icicle-ORIG-customize-face)
  (defalias 'icicle-ORIG-customize-face (symbol-function 'customize-face)))

;;;###autoload (autoload 'icicle-customize-face "icicles")
(defun icicle-customize-face (face &optional other-window)
  "Customize face FACE.  If OTHER-WINDOW is non-nil, use another window.
Input-candidate completion and cycling are available.  While cycling,
these keys with prefix `C-' are active\\<minibuffer-local-completion-map>:

`C-mouse-2', `C-RET' - Act on current completion candidate only
`C-down'  - Move to next completion candidate and act
`C-up'    - Move to previous completion candidate and act
`C-next'  - Move to next apropos-completion candidate and act
`C-prior' - Move to previous apropos-completion candidate and act
`C-end'   - Move to next prefix-completion candidate and act
`C-home'  - Move to previous prefix-completion candidate and act
`\\[icicle-all-candidates-list-action]'     - Act on *all* candidates (or all that are saved):
            Customize all in the same buffer.
`\\[icicle-all-candidates-action]'     - Act on *all* candidates (or all that are saved):
            Customize each in a separate buffer.

When candidate action and cycling are combined (e.g. `C-next'), option
`icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-return', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate,
or `C-g' to quit.

With a prefix argument, you can enter multiple faces at the same time
with a single `RET' (in Emacs 22 or later).  This gives you more or
less the `crm' completion behavior of `customize-face' in vanilla
Emacs.  Most Icicles completion features are still available, but
`TAB' performs `crm' completion, so it does not also cycle among
completion candidates.  You can, as always, use `down' to do that.

A advantage of using a prefix argument is that the default value is
the list of all faces under the cursor.  A disadvantage is that face
candidates are not WYSIWYG in buffer `*Completions*'.

This is an Icicles command - see command `icicle-mode'."
  (interactive
   (list (let* ((icicle-multi-completing-p             t)
                (icicle-list-use-nth-parts             '(1))
                (icicle-candidate-action-fn
                 (lambda (x)
                   (icicle-ORIG-customize-face (intern (icicle-transform-multi-completion x)))
                   (select-window (minibuffer-window))
                   (select-frame-set-input-focus (selected-frame))))
                (icicle-all-candidates-list-action-fn  'icicle-customize-faces)
                (icicle-orig-window                    (selected-window)) ; For alt actions.
                (alt-fn                                nil)
                (icicle-candidate-alt-action-fn
                 (or icicle-candidate-alt-action-fn  (setq alt-fn  (icicle-alt-act-fn-for-type "face"))))
                (icicle-all-candidates-list-alt-action-fn ; M-|'
                 (or icicle-all-candidates-list-alt-action-fn
                     alt-fn
                     (icicle-alt-act-fn-for-type "face"))))
           (if (and (> emacs-major-version 21)  current-prefix-arg)
               (read-face-name "Customize face: " "all faces" t)
             (read-face-name "Customize face: ")))))
  (if other-window
      (if (> emacs-major-version 23)
          (icicle-ORIG-customize-face face t)
        (icicle-ORIG-customize-face-other-window face))
    (icicle-ORIG-customize-face face)))

(defun icicle-customize-faces (faces)
  "Open Customize buffer on all faces in list FACES."
  (let ((icicle-list-nth-parts-join-string  ": ")
        (icicle-list-join-string            ": ")
        ;; $$$$$$ (icicle-list-end-string   "")
        (icicle-list-use-nth-parts          '(1)))
    (custom-buffer-create
     (custom-sort-items
      (mapcar (lambda (f) (list (intern (icicle-transform-multi-completion f)) 'custom-face))  faces)
      t custom-buffer-order-groups)
     "*Customize Apropos*")))

;; Icicles replacement for `customize-apropos', defined in `cus-edit.el'.
;; 1. Uses `completing-read' to read the regexp.
;; 2. Fixes Emacs bugs #11132, #11126.
;;
;;;###autoload (autoload 'icicle-customize-apropos "icicles")
(defun icicle-customize-apropos (pattern &optional type msgp)
  "Customize all loaded user preferences matching PATTERN.
When prompted for the PATTERN, you can use completion against
preference names - e.g. `S-TAB'.  Instead of entering a pattern you
can then just hit `RET' to accept the list of matching preferences.
This lets you see which preferences will be available in the customize
buffer and dynamically change that list.

Interactively:

With no prefix arg, customize all matching preferences: groups, faces,
and options.  With a prefix arg, show those plus all matching
non-option variables in Customize (but you cannot actually customize
the latter).

Non-interactively:

If TYPE is `options', include only user options.
If TYPE is `faces', include only faces.
If TYPE is `groups', include only groups.
If TYPE is t, include variables that are not user options, as well as
 faces and groups.

PATTERN is a regexp.

Starting with Emacs 22, if PATTERN includes no regexp special chars
then it can alternatively be a list of \"words\" separated by spaces.
Two or more of the words are matched in different orders against each
preference name.  \"Word\" here really means a string of non-space
chars.

This handling of \"words\" is for compatibility with vanilla Emacs,
and is only approximative.  It can include \"matches\" that you do not
expect.  For better matching use Icicles progressive completion, i.e.,
separate the words (any strings, in fact, including regexps) using
`S-SPC', not just `SPC'."
  (interactive
   (let* ((pref-arg                                current-prefix-arg)
          (pred                                    `(lambda (s)
                                                     (unless (symbolp s)  (setq s  (intern s)))
                                                     (or (get s 'custom-group)
                                                      (custom-facep s)
                                                      (and (boundp s)
                                                       (or (get s 'saved-value)
                                                        (custom-variable-p s)
                                                        (if (null ',pref-arg)
                                                            (user-variable-p s)
                                                          (get s 'variable-documentation)))))))
          (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
          (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred)))
     (list (completing-read "Customize (pattern): " obarray (and icompletep  pred) nil nil 'regexp-history)
           pref-arg
           t)))
  (let ((found  ()))
    (when (and (> emacs-major-version 21)  (require 'apropos nil t)
               (string= (regexp-quote pattern) pattern)
               (not (string= "" pattern)))
      (setq pattern  (split-string pattern "[ \t]+" 'OMIT-NULLS)))
    (when (fboundp 'apropos-parse-pattern) (apropos-parse-pattern pattern)) ; Emacs 22+
    (when msgp (message "Gathering apropos data for customize `%s'..." pattern))
    (mapatoms `(lambda (symbol)         ; FREE here: APROPOS-REGEXP.
                (when (string-match ,(if (> emacs-major-version 21) apropos-regexp pattern)
                                    (symbol-name symbol))
                  (when (and (not (memq ,type '(faces options))) ; groups or t
                             (get symbol 'custom-group))
                    (push (list symbol 'custom-group) found))
                  (when (and (not (memq ,type '(options groups))) ; faces or t
                             (custom-facep symbol))
                    (push (list symbol 'custom-face) found))
                  (when (and (not (memq ,type '(groups faces))) ; options or t
                             (boundp symbol)
                             (or (get symbol 'saved-value)
                                 (custom-variable-p symbol)
                                 (if (memq ,type '(nil options))
                                     (user-variable-p symbol)
                                   (get symbol 'variable-documentation))))
                    (push (list symbol 'custom-variable) found)))))
    (unless found
      (error "No %s matching %s" (if (eq type t)
                                     "items"
                                   (format "%s" (if (memq type '(options faces groups))
                                                    (symbol-name type)
                                                  "customizable items")))
             pattern))
    (custom-buffer-create (custom-sort-items found t custom-buffer-order-groups) "*Customize Apropos*")))

;; Define this for Emacs 20 and 21
(unless (fboundp 'custom-variable-p)
  (defun custom-variable-p (variable)
    "Return non-nil if VARIABLE is a custom variable."
    (or (get variable 'standard-value)  (get variable 'custom-autoload))))

;; Icicles replacement for `customize-apropos-faces', defined in `cus-edit.el'.
;; 1. Uses `completing-read' to read the regexp.
;; 2. Fixes Emacs bug #11124.
;;
;;;###autoload (autoload 'icicle-customize-apropos-faces "icicles")
(defun icicle-customize-apropos-faces (pattern &optional msgp)
  "Customize all loaded faces matching PATTERN.
See `icicle-customize-apropos'."
  (interactive
   (let* ((pred                                    (lambda (s)
                                                     (unless (symbolp s) (setq s  (intern s)))
                                                     (custom-facep s)))
          (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
          (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred)))
     (list (completing-read "Customize faces (pattern): " obarray (and icompletep  pred)
                            nil nil 'regexp-history)
           t)))
  (when msgp (message "Gathering apropos data for customizing faces..."))
  (customize-apropos pattern 'faces))

;; Icicles replacement for `customize-apropos-groups', defined in `cus-edit.el'.
;; 1. Uses `completing-read' to read the regexp.
;; 2. Fixes Emacs bug #11124.
;;
;;;###autoload (autoload 'icicle-customize-apropos-groups "icicles")
(defun icicle-customize-apropos-groups (pattern &optional msgp)
  "Customize all loaded customize groups matching PATTERN.
See `icicle-customize-apropos'."
  (interactive
   (let* ((pred                                    (lambda (s)
                                                     (unless (symbolp s) (setq s  (intern s)))
                                                     (get s 'custom-group)))
          (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
          (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred)))
     (list (completing-read "Customize groups (pattern): " obarray (and icompletep  pred)
                            nil nil 'regexp-history)
           t)))
  (when msgp (message "Gathering apropos data for customizing groups..."))
  (customize-apropos pattern 'groups))

;; Icicles replacement for `customize-apropos-options', defined in `cus-edit.el'.
;; 1. Uses `completing-read' to read the regexp.
;; 2. Fixes Emacs bugs #11124, #11128.
;;
;;;###autoload (autoload 'icicle-customize-apropos-options "icicles")
(defun icicle-customize-apropos-options (pattern &optional arg msgp)
  "Customize all loaded user options matching PATTERN.
See `icicle-customize-apropos'.

With a prefix arg, include variables that are not user options as
completion candidates, and include also matching faces and groups in
the customize buffer."
  (interactive
   (let* ((pref-arg                                current-prefix-arg)
          (pred                                    `(lambda (s)
                                                     (unless (symbolp s) (setq s  (intern s)))
                                                     (and (boundp s)
                                                      (or (get s 'saved-value)
                                                       (custom-variable-p s)
                                                       (user-variable-p s)
                                                       (and ',pref-arg
                                                        (get s 'variable-documentation))))))
          (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
          (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred)))
     (list (completing-read "Customize options (pattern): " obarray (and icompletep  pred)
                            nil nil 'regexp-history)
           pref-arg
           t)))
  (when msgp (message "Gathering apropos data for customizing options..."))
  (customize-apropos pattern (or arg  'options)))

;;;###autoload (autoload 'icicle-customize-apropos-options-of-type "icicles")
(icicle-define-command icicle-customize-apropos-options-of-type
  "Customize all loaded user options of a given type.
Enter patterns for the OPTION name and TYPE definition in the
minibuffer, separated by `icicle-list-join-string', which is \"^G^J\",
by default.  (`^G' here means the Control-g character, input using
`C-h C-g'.  Likewise, for `^J'.)

OPTION is a regexp that is matched against option names.

See `icicle-describe-option-of-type', which handles input and
completion similarly, for a full description of TYPE, matching, and
the use of a prefix argument."          ; Doc string
  icicle-customize-apropos-opt-action   ; Action function
  prompt                                ; `completing-read' args
  'icicle-describe-opt-of-type-complete nil nil nil nil nil nil
  ((prompt                             "OPTION `C-M-j' TYPE: ") ; Bindings
   (icicle-multi-completing-p          t)
   (icicle-candidate-properties-alist  '((1 (face icicle-candidate-part))))
   ;; Bind `icicle-apropos-complete-match-fn' to nil to prevent automatic input matching
   ;; in `icicle-unsorted-apropos-candidates' etc., because `icicle-describe-opt-of-type-complete'
   ;; does everything.
   (icicle-apropos-complete-match-fn   nil)
   (icicle-candidate-help-fn           'icicle-describe-opt-action)
   (icicle-pref-arg                    current-prefix-arg))
  (progn (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
         (icicle-highlight-lighter)
         (message "Gathering user options and their types...")))

(defun icicle-customize-apropos-opt-action (opt+type)
  "Action function for `icicle-customize-apropos-options-of-type'."
  (let ((icicle-list-use-nth-parts  '(1)))
    (custom-buffer-create (custom-sort-items (mapcar (lambda (s) (list (intern s) 'custom-variable))
                                                     icicle-completion-candidates)
                                             t "*Customize Apropos*"))))

;;;###autoload (autoload 'icicle-apropos "icicles")
(defun icicle-apropos (pattern &optional do-all msgp)
  "Describe Lisp symbols whose names match PATTERN.
By default, show symbols only if they are defined as functions,
variables, or faces, or if they have nonempty property lists.

With a prefix argument, or if `apropos-do-all' is non-nil, describe all
matching symbols.

Return a list of the symbols and descriptions.

Like command `apropos', but you can preview the list of matches using
`S-TAB'.  Function names are highlighted using face
`icicle-special-candidate'.

When prompted for the PATTERN, you can use completion against
preference names - e.g. `S-TAB'.  Instead of entering a pattern you
can then just hit `RET' to accept the list of matching preferences.
This lets you see which preferences will be available in the customize
buffer and dynamically change that list.

PATTERN is a regexp.

Starting with Emacs 22, if PATTERN includes no regexp special chars
then it can alternatively be a list of \"words\" separated by spaces.
Two or more of the words are matched in different orders against each
preference name.  \"Word\" here really means a string of non-space
chars.

This handling of \"words\" is for compatibility with vanilla Emacs,
and is only approximative.  It can include \"matches\" that you do not
expect.  For better matching use Icicles progressive completion, i.e.,
separate the words (any strings, in fact, including regexps) using
`S-SPC', not just `SPC'."
  (interactive
   (list
    (unwind-protect
         (progn
           (mapatoms (lambda (symb) (when (fboundp symb) (put symb 'icicle-special-candidate t))))
           (let ((icicle-fancy-candidates-p  t)
                 (icicle-candidate-alt-action-fn
                  (or icicle-candidate-alt-action-fn
                      (icicle-alt-act-fn-for-type "symbol")))
                 (icicle-all-candidates-list-alt-action-fn
                  (or icicle-all-candidates-list-alt-action-fn
                      (icicle-alt-act-fn-for-type "symbol"))))
             (completing-read "Apropos symbol (regexp or words): " obarray
                              nil nil nil 'regexp-history)))
      (mapatoms (lambda (symb) (put symb 'icicle-special-candidate nil))))
    current-prefix-arg
    t))
  (when (and (> emacs-major-version 21)  (require 'apropos nil t)
             (string= (regexp-quote pattern) pattern)
             (not (string= "" pattern)))
    (setq pattern  (split-string pattern "[ \t]+" 'OMIT-NULLS)))
  (when (fboundp 'apropos-parse-pattern) (apropos-parse-pattern pattern)) ; Emacs 22+
  (when msgp (message "Gathering apropos data..."))
  (apropos pattern do-all))

(cond
  ;; Use my versions of the `apropos*' commands, defined in `apropos-fn+var.el'.
  ;; Note that unlike my versions of `apropos-option' and `apropos-command', the `icicle-'
  ;; versions here do not respect `apropos-do-all': they always work with options and commands.
  ((fboundp 'apropos-option)
   (defun icicle-apropos-variable (pattern &optional msgp)
     "Show variables that match PATTERN.
This includes variables that are not user options.
User options are highlighted using face `icicle-special-candidate'.
You can see the list of matches with `S-TAB'.
See `icicle-apropos' for a description of PATTERN."
     (interactive
      (list
       (unwind-protect
            (progn
              (mapatoms (lambda (symb) (when (user-variable-p symb) (put symb 'icicle-special-candidate t))))
              (let* ((icicle-fancy-candidates-p  t)
                     (pred                                      (lambda (s)
                                                                  (unless (symbolp s) (setq s  (intern s)))
                                                                  (and (boundp s)
                                                                       (get s 'variable-documentation))))
                     (icompletep                                (and (boundp 'icomplete-mode)
                                                                     icomplete-mode))
                     (icicle-must-pass-after-match-predicate    (and (not icompletep)  pred))
                     (icicle-candidate-alt-action-fn            (or icicle-candidate-alt-action-fn
                                                                    (icicle-alt-act-fn-for-type "variable")))
                     (icicle-all-candidates-list-alt-action-fn  (or icicle-all-candidates-list-alt-action-fn
                                                                    (icicle-alt-act-fn-for-type "variable"))))
                (completing-read
                 (concat "Apropos variable (regexp" (and (>= emacs-major-version 22)  " or words")
                         "): ")
                 obarray (and icompletep  pred) nil nil 'regexp-history)))
         (mapatoms (lambda (symb) (put symb 'icicle-special-candidate nil))))
       t))
     (when (and (> emacs-major-version 21)  (require 'apropos nil t)
                (string= (regexp-quote pattern) pattern)
                (not (string= "" pattern)))
       (setq pattern  (split-string pattern "[ \t]+" 'OMIT-NULLS)))
     (when (fboundp 'apropos-parse-pattern) (apropos-parse-pattern pattern)) ; Emacs 22+
     (when msgp (message "Gathering data apropos variables..."))
     (apropos-variable pattern))

   (defun icicle-apropos-option (pattern &optional msgp)
     "Show user options (variables) that match PATTERN.
You can see the list of matches with `S-TAB'.
See `icicle-apropos' for a description of PATTERN."
     (interactive
      (let* ((pred                                    (lambda (s)
                                                        (unless (symbolp s) (setq s  (intern s)))
                                                        (user-variable-p s)))
             (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
             (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred)))
        (list (completing-read
               (concat "Apropos user option (regexp" (and (>= emacs-major-version 22)  " or words")
                       "): ") obarray (and icompletep  pred) nil nil 'regexp-history)
              t)))
     (let ((apropos-do-all  nil)
           (icicle-candidate-alt-action-fn
            (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "option")))
           (icicle-all-candidates-list-alt-action-fn
            (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "option"))))
       (when (and (> emacs-major-version 21)  (require 'apropos nil t)
                  (string= (regexp-quote pattern) pattern)
                  (not (string= "" pattern)))
         (setq pattern  (split-string pattern "[ \t]+" 'OMIT-NULLS)))
       (when (fboundp 'apropos-parse-pattern) (apropos-parse-pattern pattern)) ; Emacs 22+
       (when msgp (message "Gathering data apropos user options..."))
       (apropos-option pattern)))

   (defun icicle-apropos-function (pattern &optional msgp)
     "Show functions that match PATTERN.
This includes functions that are not commands.
Command names are highlighted using face `icicle-special-candidate'.
You can see the list of matches with `S-TAB'.
See `icicle-apropos' for a description of PATTERN."
     (interactive
      (list
       (unwind-protect
            (progn
              (mapatoms (lambda (symb) (when (commandp symb) (put symb 'icicle-special-candidate t))))
              (let* ((icicle-fancy-candidates-p               t)
                     (pred                                     (lambda (s)
                                                                 (unless (symbolp s) (setq s  (intern s)))
                                                                 (fboundp s)))
                     (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
                     (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
                     (icicle-candidate-alt-action-fn          (or icicle-candidate-alt-action-fn
                                                                  (icicle-alt-act-fn-for-type "function")))
                     (icicle-all-candidates-list-alt-action-fn  
                      (or icicle-all-candidates-list-alt-action-fn
                          (icicle-alt-act-fn-for-type "function"))))
                (completing-read
                 (concat "Apropos function (regexp" (and (>= emacs-major-version 22)  " or words")
                         "): ") obarray (and icompletep  pred) nil nil 'regexp-history)))
         (mapatoms (lambda (symb) (put symb 'icicle-special-candidate nil))))
       t))
     (when (and (> emacs-major-version 21)  (require 'apropos nil t)
                (string= (regexp-quote pattern) pattern)
                (not (string= "" pattern)))
       (setq pattern  (split-string pattern "[ \t]+" 'OMIT-NULLS)))
     (when (fboundp 'apropos-parse-pattern) (apropos-parse-pattern pattern)) ; Emacs 22+
     (when msgp (message "Gathering data apropos functions..."))
     (apropos-function pattern))

   (defun icicle-apropos-command (pattern &optional msgp)
     "Show commands (interactively callable functions) that match PATTERN.
You can see the list of matches with `S-TAB'.
See `icicle-apropos' for a description of PATTERN."
     (interactive
      (let* ((pred                                      (lambda (s)
                                                          (unless (symbolp s) (setq s  (intern s)))
                                                          (commandp s)))
             (icompletep                                (and (boundp 'icomplete-mode)  icomplete-mode))
             (icicle-must-pass-after-match-predicate    (and (not icompletep)  pred))
             (icicle-candidate-alt-action-fn            (or icicle-candidate-alt-action-fn
                                                            (icicle-alt-act-fn-for-type "command")))
             (icicle-all-candidates-list-alt-action-fn  (or icicle-all-candidates-list-alt-action-fn
                                                            (icicle-alt-act-fn-for-type "command"))))
        (list (completing-read
               (concat "Apropos command (regexp" (and (>= emacs-major-version 22)  " or words")
                       "): ") obarray (and icompletep  pred) nil nil 'regexp-history)
              t)))
     (when (and (> emacs-major-version 21)  (require 'apropos nil t)
                (string= (regexp-quote pattern) pattern)
                (not (string= "" pattern)))
       (setq pattern  (split-string pattern "[ \t]+" 'OMIT-NULLS)))
     (when (fboundp 'apropos-parse-pattern) (apropos-parse-pattern pattern)) ; Emacs 22+
     (when msgp (message "Gathering data apropos commands..."))
     (let ((apropos-do-all  nil))  (apropos-command pattern))))

  ;; My versions are not available.  Use the vanilla Emacs versions of the `apropos...' commands.
  (t
   (defun icicle-apropos-variable (pattern &optional do-all msgp)
     "Show variables that match PATTERN.
You can see the list of matches with `S-TAB'.
See `icicle-apropos' for a description of PATTERN.

By default, only user options are candidates.  With optional prefix
DO-ALL, or if `apropos-do-all' is non-nil, all variables are
candidates.  In that case, the user-option candidates are highlighted
using face `icicle-special-candidate'."
     (interactive
      (list
       (unwind-protect
            (progn
              (unless (or (boundp 'apropos-do-all)  (require 'apropos nil t))
                (error "Library `apropos' not found"))
              (when (or current-prefix-arg  apropos-do-all)
                (mapatoms (lambda (symb)
                            (when (user-variable-p symb) (put symb 'icicle-special-candidate t)))))
              (let* ((icicle-fancy-candidates-p               (or current-prefix-arg  apropos-do-all))
                     (pred                                    (if (or current-prefix-arg  apropos-do-all)
                                                                  (lambda (s)
                                                                    (unless (symbolp s)
                                                                      (setq s  (intern s)))
                                                                    (and (boundp s)
                                                                         (get s 'variable-documentation)))
                                                                (lambda (s)
                                                                  (unless (symbolp s) (setq s  (intern s)))
                                                                  (user-variable-p s))))
                     (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
                     (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
                     (icicle-candidate-alt-action-fn          (or icicle-candidate-alt-action-fn
                                                                  (icicle-alt-act-fn-for-type
                                                                   (if icicle-fancy-candidates-p
                                                                       "variable"
                                                                     "option"))))
                     (icicle-all-candidates-list-alt-action-fn
                      (or icicle-all-candidates-list-alt-action-fn
                          (icicle-alt-act-fn-for-type (if icicle-fancy-candidates-p
                                                          "variable"
                                                        "option")))))
                (completing-read
                 (concat "Apropos " (if (or current-prefix-arg  apropos-do-all)
                                        "variable" "user option")
                         " (regexp" (and (>= emacs-major-version 22)  " or words") "): ")
                 obarray (and icompletep  pred) nil nil 'regexp-history)))
         (when (or current-prefix-arg  apropos-do-all)
           (mapatoms (lambda (symb) (put symb 'icicle-special-candidate nil)))))
       current-prefix-arg
       t))
     (when (and (> emacs-major-version 21)  (require 'apropos nil t)
                (string= (regexp-quote pattern) pattern)
                (not (string= "" pattern)))
       (setq pattern  (split-string pattern "[ \t]+" 'OMIT-NULLS)))
     (when (fboundp 'apropos-parse-pattern) (apropos-parse-pattern pattern)) ; Emacs 22+
     (when msgp (message (format "Gathering data apropos %s..." (if do-all "variables" "options"))))
     (apropos-variable pattern do-all))

   (defun icicle-apropos-command (pattern &optional do-all var-predicate msgp)
     "Show commands (interactively callable functions) that match PATTERN.
You can see the list of matches with `S-TAB'.

See `icicle-apropos' for a description of PATTERN.

With \\[universal-argument] prefix, or if `apropos-do-all' is non-nil,
also show noninteractive functions.  In that case, the command
candidates are highlighted using face `icicle-special-candidate'.

If VAR-PREDICATE is non-nil, show only variables, and only those that
satisfy the predicate VAR-PREDICATE.

Non-interactively, a string PATTERN is used as a regexp, while a list
of strings is used as a word list."
     (interactive
      (list
       (unwind-protect
            (progn
              (unless (boundp 'apropos-do-all)
                (unless (require 'apropos nil t) (error "Library `apropos' not found")))
              (when (or current-prefix-arg  apropos-do-all)
                (mapatoms (lambda (symb) (when (commandp symb) (put symb 'icicle-special-candidate t)))))
              (let* ((icicle-fancy-candidates-p               (or current-prefix-arg  apropos-do-all))
                     (pred                                    (if current-prefix-arg
                                                                  (lambda (s)
                                                                    (unless (symbolp s)
                                                                      (setq s  (intern s)))
                                                                    (fboundp s))
                                                                (lambda (s)
                                                                  (unless (symbolp s) (setq s  (intern s)))
                                                                  (commandp s))))
                     (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
                     (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
                     (icicle-candidate-alt-action-fn           (or icicle-candidate-alt-action-fn
                                                                   (icicle-alt-act-fn-for-type
                                                                    (if icicle-fancy-candidates-p
                                                                        "function"
                                                                      "command"))))
                     (icicle-all-candidates-list-alt-action-fn
                      (or icicle-all-candidates-list-alt-action-fn
                          (icicle-alt-act-fn-for-type (if icicle-fancy-candidates-p
                                                          "function"
                                                        "command")))))
                (completing-read
                 (concat "Apropos " (if (or current-prefix-arg  apropos-do-all)
                                        "command or function" "command")
                         " (regexp" (and (>= emacs-major-version 22)  " or words") "): ")
                 obarray (and icompletep  pred) nil nil 'regexp-history)))
         (when (or current-prefix-arg  apropos-do-all)
           (mapatoms (lambda (symb) (put symb 'icicle-special-candidate nil)))))
       current-prefix-arg
       nil
       t))
     (when (and (> emacs-major-version 21)  (require 'apropos nil t)
                (string= (regexp-quote pattern) pattern)
                (not (string= "" pattern)))
       (setq pattern  (split-string pattern "[ \t]+" 'OMIT-NULLS)))
     (when (fboundp 'apropos-parse-pattern) (apropos-parse-pattern pattern)) ; Emacs 22+
     (when msgp (message (format "Gathering data apropos %s..." (if do-all "functions" "commands"))))
     (apropos-command pattern do-all var-predicate))))

;;;###autoload (autoload 'icicle-apropos-options-of-type "icicles")
(icicle-define-command icicle-apropos-options-of-type
  "Show user options of a given type.
Enter patterns for the OPTION name and TYPE definition in the
minibuffer, separated by `icicle-list-join-string', which is \"^G^J\",
by default.  (`^G' here means the Control-g character, input using
`C-h C-g'.  Likewise, for `^J'.)

OPTION is a regexp that is matched against option names.

See also:
* `icicle-describe-option-of-type', which handles input and completion
  similarly, for a full description of TYPE, matching, and the use of
  a prefix argument
* `icicle-apropos-value', using `C-$' to filter to options only" ; Doc string
  icicle-apropos-opt-action             ; Action function
  prompt                       ; `completing-read' args
  'icicle-describe-opt-of-type-complete nil nil nil nil nil nil
  ((prompt                             "OPTION `C-M-j' TYPE: ") ; Bindings
   (icicle-multi-completing-p          t)
   (icicle-candidate-properties-alist  '((1 (face icicle-candidate-part))))
   ;; Bind `icicle-apropos-complete-match-fn' to nil to prevent automatic input matching
   ;; in `icicle-unsorted-apropos-candidates' etc., because `icicle-describe-opt-of-type-complete'
   ;; does everything.
   (icicle-apropos-complete-match-fn   nil)
   (icicle-candidate-help-fn           'icicle-describe-opt-action)
   (icicle-pref-arg                    current-prefix-arg))
  (progn (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
         (icicle-highlight-lighter)
         (message "Gathering user options and their types...")))

(defun icicle-apropos-opt-action (opt+type)
  "Action function for `icicle-apropos-options-of-type'."
  (let ((icicle-list-use-nth-parts  '(1)))
    (apropos-option (icicle-transform-multi-completion opt+type))))

;;;###autoload (autoload 'icicle-apropos-zippy "icicles")
(defun icicle-apropos-zippy (regexp)
  "Show all Zippy quotes matching the regular-expression REGEXP.
Return the list of matches."
  (interactive (progn (unless (boundp 'yow-file)
                        (unless (require 'yow nil t) (error "Library `yow' not found")))
                      (cookie yow-file yow-load-message yow-after-load-message)
                      (let* ((case-fold-search     t)
                             (cookie-table-symbol  (intern yow-file cookie-cache))
                             (string-table         (symbol-value cookie-table-symbol))
                             (table                (nreverse (mapcar #'list string-table))))
                        (list (completing-read "Apropos Zippy (regexp): " table
                                               nil nil nil 'regexp-history)))))
  (let ((matches  (apropos-zippy icicle-current-input)))
    (when (interactive-p)
      (with-output-to-temp-buffer "*Zippy Apropos*"
        (while matches
          (princ (car matches))
          (setq matches  (cdr matches))
          (and matches  (princ "\n\n")))))
    matches))                           ; Return matching Zippyisms.

;;;###autoload (autoload 'icicle-apropos-value "icicles")
(icicle-define-command icicle-apropos-value
  "Choose a variable, function, or other symbol description.
This is similar to vanilla command `apropos-value', but you can match
against the variable name and its printed value at the same time.

By default, each completion candidate is multi-completion composed of
a variable name plus its value.  They are separated by
`icicle-list-join-string' \(\"^G^J\", by default). 

With a prefix arg, candidates are different kinds of symbols:

 < 0: functions and their defs (but byte-compiled defs are skipped)
 > 0: symbols and their plists
 = 0: variables and their values, functions and their definitions, and
     other symbols and their plists

plain (`C-u'): use the last-computed (cached) set of candidates

You can use `C-$' during completion to toggle filtering the domain of
initial candidates according to the prefix argument, as follows:

none: only user options (+ values)
 < 0: only commands (+ definitions)
 > 0: only faces (+ plists)
 = 0: only options (+ values), commands (+ defs), faces (+ plists)

Remember that you can use \\<minibuffer-local-completion-map>\
`\\[icicle-cycle-incremental-completion]' to toggle incremental completion.

See also:
* `icicle-apropos-vars-w-val-satisfying',
  `icicle-describe-vars-w-val-satisfying' - values satisfy a predicate
* `icicle-plist' - similar to this command with positive prefix arg
* `icicle-vardoc', `icicle-fundoc', `icicle-doc' - match name & doc
* `icicle-apropos-options-of-type', `icicle-describe-option-of-type' -
  match name & defcustom type"
  icicle-doc-action                     ; Action function
  prompt                                ; `completing-read' args
  (let ((cands  (and (consp pref-arg)  icicle-apropos-value-last-initial-cand-set))
        cand)
    (unless cands                       ; COLLECTION arg is an alist whose items are ((SYMB INFO)).
      (mapatoms (lambda (symb)
                  ;; Exclude the local vars bound by this command.  They are not what the user wants to see.
                  (setq cand  (and (not (memq symb '(cands  pref-arg  num-arg  prompt
                                                     icicle-toggle-transforming-message
                                                     icicle-candidate-properties-alist
                                                     icicle-multi-completing-p  icicle-list-use-nth-parts
                                                     icicle-transform-before-sort-p  icicle-transform-function
                                                     icicle-last-transform-function  print-fn  make-cand)))
                                   (funcall make-cand symb)))
                  (when cand (push cand cands))))
      (setq icicle-apropos-value-last-initial-cand-set  cands))
    cands)
  nil nil nil nil nil nil
  ((pref-arg                            current-prefix-arg)
   (num-arg                             (prefix-numeric-value pref-arg))
   (prompt                              (format "SYMBOL `C-M-j' %s: " (if pref-arg "INFO" "VALUE"))) ; Bindings
   (icicle-toggle-transforming-message  (cond ((or (consp pref-arg)  (= num-arg 0))
                                               "Filtering to OPTIONS, COMMANDS, & FACES is now %s")
                                              ((and pref-arg  (> num-arg 0))
                                               "Filtering to FACES (+ plists) is now %s")
                                              ((< num-arg 0)
                                               "Filtering to COMMANDS (+ defs) is now %s")
                                              (t "Filtering to user OPTIONS (+ values) is now %s")))
   (icicle-candidate-properties-alist   '((1 (face icicle-candidate-part))))
   (icicle-multi-completing-p           t)
   (icicle-list-use-nth-parts           '(1))
   (icicle-transform-before-sort-p      t)
   (icicle-transform-function           nil) ; No transformation: all symbols.
   (icicle-last-transform-function      (lambda (cands) ; `C-$': only user options, commands, or faces.
                                          (loop for cc in cands
                                                with symb
                                                do (setq symb  (intern (icicle-transform-multi-completion cc)))
                                                if (cond ((or (consp `,pref-arg)  (= `,num-arg 0))
                                                          (or (user-variable-p symb)
                                                              (commandp symb)
                                                              (facep symb)))
                                                         ((and `,pref-arg  (> `,num-arg 0))
                                                          (facep symb))
                                                         ((< `,num-arg 0)
                                                          (commandp symb))
                                                         (t
                                                          (user-variable-p symb)))
                                                collect cc)))
   (print-fn                            (lambda (obj)
                                          (let ((print-circle  t))
;;; $$$$$$                                  (condition-case nil
;;;                                             (prin1-to-string obj)
;;;                                           (error "`icicle-apropos-value' printing error")))))
                                            (prin1-to-string obj))))
   (make-cand                           (cond ((< num-arg 0) ; Function
                                               (lambda (symb)
                                                 (and (fboundp symb)
                                                      `((,(symbol-name symb)
                                                         ,(if (byte-code-function-p (symbol-function symb))
                                                              ""
                                                              (funcall print-fn (symbol-function symb))))))))
                                              ((= num-arg 0) ; Do ALL
                                               (lambda (symb) ; Favor the var, then the fn, then the plist.
                                                 (cond ((boundp symb)
                                                        `((,(symbol-name symb)
                                                           ,(funcall print-fn (symbol-value symb)))))
                                                       ((fboundp symb)
                                                        `((,(symbol-name symb)
                                                           ,(if (byte-code-function-p (symbol-function symb))
                                                                ""
                                                                (funcall print-fn (symbol-function symb))))))
                                                       ((symbol-plist symb)
                                                        `((,(symbol-name symb)
                                                           ,(funcall print-fn (symbol-plist symb))))))))
                                              ((and pref-arg  (> num-arg 0)) ; Plist
                                               (lambda (symb)
                                                 (and (symbol-plist symb)
                                                      `((,(symbol-name symb)
                                                         ,(funcall print-fn (symbol-plist symb)))))))
                                              (t ; Variable
                                               (lambda (symb)
                                                 (and (boundp symb)
                                                      `((,(symbol-name symb)
                                                         ,(funcall print-fn (symbol-value symb))))))))))
  (progn (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code.
         (icicle-highlight-lighter)
         (message "Gathering %s%s..." (cond ((consp pref-arg)              'SYMBOLS)
                                            ((and pref-arg  (< num-arg 0)) 'FUNCTIONS)
                                            ((and pref-arg  (= num-arg 0)) "all SYMBOLS")
                                            ((and pref-arg  (> num-arg 0)) 'SYMBOLS)
                                            (t                             'VARIABLES))
                  (cond ((consp pref-arg) " from last invocation (cached)")
                        ((and pref-arg  (< num-arg 0)) " and their definitions")
                        ((and pref-arg  (= num-arg 0)) " and their info")
                        ((and pref-arg  (> num-arg 0)) " and their plists")
                        (t " and their values")))))

;;;###autoload (autoload 'icicle-describe-option-of-type "icicles")
(icicle-define-command icicle-describe-option-of-type ; Bound to `C-h C-o'.  Command name
  "Describe a user option that was defined with a given `defcustom' type.
Enter patterns for the OPTION name and TYPE definition in the
minibuffer, separated by `icicle-list-join-string', which is \"^G^J\",
by default.  (`^G' here means the Control-g character, input using
`C-h C-g'.  Likewise, for `^J'.)

Remember that you can insert `icicle-list-join-string' using `C-M-j'.

This command binds option `icicle-dot-string' to the value of
`icicle-anychar-regexp' for the duration, which means that `.' in your
input to this command matches any character, including a newline char.

This is for convenience because `defcustom' type sexps are often
multiline.  This is particularly important for progressive completion,
where your input definitely matches as a regexp (apropos completion).
If you do not want `.' to match newlines, use `C-M-.' during the
command.

Example use of progressive completion:

1. C-h C-o ici C-M-j choic S-TAB

   That shows all options whose names are apropos-matched by `ici' and
   whose types are matched by `choic'.

2. S-SPC om C-M-j sexp

   That limits the matches to options whose names also match `om' and
   whose types also match `sexp'.'


OPTION is a regexp that is matched against option names.

Depending on the prefix arg, TYPE is interpreted as either of these:

 - a regexp to match against the option type

 - a definition acceptable for `defcustom' :type, or its first symbol,
   for example, (choice (integer) (regexp)) or `choice'

In the second case, depending on the prefix arg, TYPE can be matched
against the option type, or it can be matched against either the
option type or one of its subtypes.

In the second case also, depending on the prefix arg, if TYPE does not
match some option's type, that option might still be a candidate, if
its current value satisfies TYPE.

In sum, the prefix arg determines the type-matching behavior, as
follows:

 - None:      OPTION is defined with TYPE or a subtype of TYPE.
              TYPE is a regexp.

 - `C-u':     OPTION is defined with TYPE or a subtype of TYPE,
                or its current value is compatible with TYPE.
              TYPE is a type definition or its first symbol.

 - Negative:  OPTION is defined with TYPE (exact match).
              TYPE is a regexp.

 - Positive:  OPTION is defined with TYPE,
                or its current value is compatible with TYPE.
              TYPE is a type definition or its first symbol.

 - Zero:      OPTION is defined with TYPE or a subtype of TYPE.
              TYPE is a type definition or its first symbol.

 - `C-u C-u': OPTION is defined with TYPE (exact match).
              TYPE is a type definition or its first symbol.

You can change these prefix-arg key sequences by customizing option
`icicle-option-type-prefix-arg-list'.  For example, if you tend to use
the matching defined here for `C-u', you might want to make that the
default behavior (no prefix arg).  You can assign any of the six
behaviors to any of the prefix-arg keys.

If TYPE is nil, then *all* options that match OPTION are candidates.

Note that options defined in libraries that have not been loaded can
be candidates, but their type will appear as nil, since it is not
known before loading the option definition.

You can match your input against the option name or the type
definition or both.  Use `C-M-j' (equivalent here to `C-q C-g C-j') to
input the default separator.

For example, to match all Icicles options whose type matches `string'
\(according to the prefix arg), use `S-TAB' with this input:

 icicle C-M-j string$

If you instead want all Icicles options whose type definition contains
`string', as in (repeat string), then use this:

 icicle C-M-j string

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-cycle-incremental-completion] to toggle incremental completion.

See also:
* `icicle-apropos-options-of-type', to show options of a given type
* `icicle-apropos-value', using `C-$' to filter to options only" ; Doc string
  icicle-describe-opt-action            ; Action function
  prompt                                ; `completing-read' args
  'icicle-describe-opt-of-type-complete nil nil nil nil nil nil
  ((prompt                                 "OPTION `C-M-j' TYPE: ") ; Bindings
   (icicle-multi-completing-p              t)
   (icicle-candidate-properties-alist      '((1 (face icicle-candidate-part))))
   (icicle-dot-string                      icicle-anychar-regexp)
   ;; Bind `icicle-apropos-complete-match-fn' to nil to prevent automatic input matching
   ;; in `icicle-unsorted-apropos-candidates' etc., because `icicle-describe-opt-of-type-complete'
   ;; does everything.
   (icicle-apropos-complete-match-fn       nil)
   (icicle-last-apropos-complete-match-fn  'icicle-multi-comp-apropos-complete-match)
   (icicle-candidate-help-fn               'icicle-describe-opt-action)
   ;; $$$ (icicle-highlight-input-completion-failure nil)
   (icicle-pref-arg                        current-prefix-arg))
  (progn (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
         (icicle-highlight-lighter)
         (message "Gathering user options and their types...")))

(defun icicle-describe-opt-action (opt+type)
  "Action function for `icicle-describe-option-of-type'."
  (let ((icicle-list-use-nth-parts  '(1)))
    (describe-variable (intern (icicle-transform-multi-completion opt+type)))))

;; Free var here: `icicle-pref-arg' - it is bound in `icicle-describe-option-of-type'.
(defun icicle-describe-opt-of-type-complete (strg pred completion-mode)
  "Completion function for `icicle-describe-option-of-type'.
This is used as the value of `minibuffer-completion-table'."
  (setq strg  icicle-current-input)
  ;; Parse strg into its option part and its type part: OPS  and TPS.
  ;; Make raw alist of all options and their types: ((a . ta) (b . tb)...).
  (lexical-let* ((num-prefix  (prefix-numeric-value icicle-pref-arg))
                 (mode        (cond ((not icicle-pref-arg) ; No prefix arg
                                     (nth 4 icicle-option-type-prefix-arg-list))
                                    ((and (consp icicle-pref-arg)  (= 16 num-prefix)) ; C-u C-u
                                     (nth 0 icicle-option-type-prefix-arg-list))
                                    ((consp icicle-pref-arg) (nth 2 icicle-option-type-prefix-arg-list)) ; C-u
                                    ((zerop num-prefix) (nth 1 icicle-option-type-prefix-arg-list)) ; C-0
                                    ((wholenump num-prefix) ; C-9
                                     (nth 3 icicle-option-type-prefix-arg-list))
                                    (t (nth 5 icicle-option-type-prefix-arg-list)))) ; C--
                 (ops         (let ((icicle-list-use-nth-parts  '(1)))
                                (icicle-transform-multi-completion strg)))
                 (tps         (let ((icicle-list-use-nth-parts  '(2)))
                                (icicle-transform-multi-completion strg)))
                 (tp          (and (not (string= "" tps))
                                   ;; Use regexp if no prefix arg or negative; else use sexp.
                                   (if (memq mode '(inherit-or-regexp direct-or-regexp)) tps (read tps))))
                 (result      ()))
    (mapatoms
     (lambda (symb)                     ; FREE here: RESULT.
       (when (if (fboundp 'custom-variable-p) (custom-variable-p symb) (user-variable-p symb))
         (condition-case nil
             (push (list symb (get symb 'custom-type)) result)
           (error nil)))))
    ;; Keep only candidates that correspond to input.
    (setq result
          (lexical-let ((ops-re  (if (memq icicle-current-completion-mode '(nil apropos))
                                     ops
                                   (concat "^" ops))))
            (icicle-remove-if-not
             (lambda (opt+typ)          ; FREE here: OPS-RE, MODE, TP.
               (and (string-match ops-re (symbol-name (car opt+typ)))
                    (or (null tp)
                        (condition-case nil
                            (icicle-var-is-of-type-p (car opt+typ) (list tp)
                                                     (case mode
                                                       ((inherit inherit-or-regexp) 'inherit)
                                                       ((direct  direct-or-regexp)  'direct)
                                                       (inherit-or-value     'inherit-or-value)
                                                       (direct-or-value      'direct-or-value)))
                          (error nil)))))
             result)))
    ;; Change alist entries to multi-completions: "op^G^Jtp".  Add short help for mode-line, tooltip.
    (setq result
          ;; FREE here: ICICLE-HELP-IN-MODE-LINE-DELAY, ICICLE-LIST-JOIN-STRING, TOOLTIP-MODE.
          (mapcar (lambda (entry)
                    (let* ((opt+typ-string
                            ;; $$$$$$ (concat (mapconcat (lambda (e) (pp-to-string e))
                            ;;                           entry icicle-list-join-string)
                            ;;                icicle-list-end-string)) ; $$$$$$
                            (mapconcat (lambda (e) (pp-to-string e))  entry  icicle-list-join-string))
                           (doc         ; Don't bother to look up doc, if user won't see it.
                            (and (or (> icicle-help-in-mode-line-delay 0)
                                     (and (boundp 'tooltip-mode)  tooltip-mode))
                                 (documentation-property (car entry) 'variable-documentation t)))
                           (doc1  (and (stringp doc)  (string-match ".+$" doc)  (match-string 0 doc))))
                      (when doc1 (icicle-candidate-short-help doc1 opt+typ-string))
                      opt+typ-string))
                  result))
    (if completion-mode
        result                          ; `all-completions', `test-completion'
      (try-completion                   ; `try-completion'
       strg (mapcar #'list result) (and pred  (lambda (ss) (funcall pred ss)))))))

;;;###autoload (autoload 'icicle-apropos-vars-w-val-satisfying "icicles")
(defun icicle-apropos-vars-w-val-satisfying (predicate pattern &optional optionp)
  "Show variables whose values satisfy PREDICATE and names match PATTERN.
You are prompted for a predicate sexp and a pattern matching the
variable names.  For the latter, before hitting `RET' you must use
completion (`S-TAB' or `TAB') to manifest the set of matching
variables.  Apropos information is shown for those variables when you
hit `RET'.

The predicate sexp must be a function symbol or a lambda form that
accepts the value of the variable as its (first) argument.

Typically the predicate is a type predicate, such as `integerp', but
it could be anything.  Instead of just `integerp', for example, it
could be `(lambda (val) (and (integerp val)  (> val 5) (< val 15)))'.

With a prefix argument, candidates are limited to user options.

See also: `icicle-apropos-value', which matches names and values."
  (interactive (icicle-read-args-w-val-satisfying "Apropos var (hit `S-TAB' or `TAB'): "
                                                  current-prefix-arg t))
  (if optionp
      (if (fboundp 'icicle-apropos-option)
          (icicle-apropos-option pattern)
        (icicle-apropos-variable pattern t))
    (icicle-apropos-variable pattern)))

;;;###autoload (autoload 'icicle-customize-apropos-opts-w-val-satisfying "icicles")
(defun icicle-customize-apropos-opts-w-val-satisfying (predicate pattern)
  "Customize options whose values satisfy PREDICATE and names match PATTERN.
You are prompted for a predicate sexp and a pattern matching the
option names.  For the latter, before hitting `RET' you must use
completion (`S-TAB' or `TAB') to manifest the set of matching options.
A Customize buffer is opened for those options when you hit `RET'.

The predicate sexp must be a function symbol or a lambda form that
accepts the value of the variable as its (first) argument.

Typically the predicate is a type predicate, such as `integerp', but
it could be anything.  Instead of just `integerp', for example, it
could be `(lambda (val) (and (integerp val)  (> val 5) (< val 15)))'."
  (interactive (let ((xxx  (icicle-read-args-w-val-satisfying "Customize vars (hit `S-TAB' or `TAB'): "
                                                              t t)))
                 (list (car xxx) (cadr xxx))))
  (icicle-customize-apropos-options pattern))

(defun icicle-read-args-w-val-satisfying (prompt optionp patternp)
  "Read args for `icicle-*-w-val-satisfying' commands.
Prompt for the variable names using PROMPT. 
Non-nil OPTIONP means allow only variables that are user options.  It
is used here during completion of the variable name, and it is
returned as the third arg for `icicle-describe-var-w-val-satisfying'.

Non-nil PATTERNP means return as the variable whatever input pattern
the user entered.  Otherwise, assume the pattern names a variable, and
return the symbol with that name."
  (let* ((enable-recursive-minibuffers            t)
         (valpred
          (let ((string-read  nil)
                (read-result  nil))
            (condition-case err
                (prog1 (setq read-result  (read (setq string-read  (completing-read
                                                                    "Predicate to satify: "
                                                                    icicle-predicate-types-alist
                                                                    nil nil nil
                                                                    (if (boundp 'function-name-history)
                                                                        'function-name-history
                                                                      'icicle-function-name-history)))))
                  (unless (functionp read-result) (error ""))) ; Read was OK, but not a function.
              (error (error "Invalid function: `%s'" string-read))))) ; Read error.
         (vardflt                                 (or (and (fboundp 'symbol-nearest-point)
                                                           (symbol-nearest-point))
                                                      (and (symbolp (variable-at-point))
                                                           (variable-at-point))))
         (symbpred                                (if optionp #'user-variable-p #'boundp))
         (varpred                                 `(lambda (sy)
                                                    (unless (symbolp sy)  (setq sy  (intern sy)))
                                                    (and
                                                     (funcall #',symbpred sy)
                                                     (funcall #',valpred (symbol-value sy)))))
         (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
         (icicle-must-pass-after-match-predicate  (and (not icompletep)  varpred))
         (varpat                                  (completing-read
                                                   prompt obarray (and icompletep  varpred) nil nil nil
                                                   (and vardflt  (symbol-name vardflt)) t)))
    (list valpred
          (if patternp
              (if icicle-completion-candidates
                  (regexp-opt icicle-completion-candidates)
                (message "Predicate %s.  You did not complete var names (`S-TAB' or `TAB')"
                         (icicle-propertize "IGNORED" 'face 'icicle-msg-emphasis))
                (sit-for 3)
                varpat)
            (intern varpat))
          optionp)))


;; REPLACE ORIGINAL `repeat-complex-command' defined in `simple.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Uses `completing-read' to read the command to repeat, letting you use `S-TAB' and
;; `TAB' to see the history list and `C-,' to toggle sorting that display.
;;
(unless (fboundp 'icicle-ORIG-repeat-complex-command)
  (defalias 'icicle-ORIG-repeat-complex-command (symbol-function 'repeat-complex-command)))

;;;###autoload (autoload 'icicle-repeat-complex-command "icicles")
(defun icicle-repeat-complex-command (arg) ; Bound to `C-x ESC ESC', `C-x M-:' in Icicle mode.
  "Edit and re-evaluate the last complex command, or ARGth from last.
A complex command is one that used the minibuffer.
ARG is the prefix argument numeric value.

You can edit the past command you choose before executing it.  The
Lisp form of the command is used.  If the command you enter differs
from the previous complex command, then it is added to the front of
the command history.

Icicles completion is available for choosing a past command.  You can
still use the vanilla Emacs bindings `\\<minibuffer-local-map>\\[next-history-element]' and \
`\\[previous-history-element]' to cycle inputs,
and `\\[repeat-matching-complex-command]' to match regexp input, but Icicles input cycling (`up',
`down', `next', `prior', `home', `end') and apropos completion
\(`S-TAB') are superior and more convenient."
  (interactive "p")
  (let ((elt  (nth (1- arg) command-history))
        newcmd)
    (if elt
        (progn
          (setq newcmd
                (let ((print-level                   nil)
                      (minibuffer-history-position   arg)
                      (minibuffer-history-sexp-flag  (1+ (minibuffer-depth))))
                  (unwind-protect
                       (let ((icicle-transform-function  'icicle-remove-duplicates))
                         (read (completing-read
                                "Redo: " (mapcar (lambda (entry) (list (prin1-to-string entry)))
                                                 command-history)
                                nil nil (prin1-to-string elt) (cons 'command-history arg)
                                (prin1-to-string elt))))
                    ;; If command was added to command-history as a string, get rid of that.
                    ;; We want only evaluable expressions there.
                    (if (stringp (car command-history))
                        (setq command-history  (cdr command-history))))))
          ;; If command to be redone does not match front of history, add it to the history.
          (or (equal newcmd (car command-history))
              (setq command-history  (cons newcmd command-history)))
          (eval newcmd))
      (if command-history
          (error "Argument %d is beyond length of command history" arg)
        (error "There are no previous complex commands to repeat")))))

;;;###autoload (autoload 'icicle-add-entry-to-saved-completion-set "icicles")
(defun icicle-add-entry-to-saved-completion-set (set-name entry type)
  "Add ENTRY to saved completion-candidates set SET-NAME.
ENTRY is normally a single candidate (a string).
 With a prefix arg, however, and if option
 `icicle-filesets-as-saved-completion-sets-flag' is non-nil, then
 ENTRY is the name of an Emacs fileset (Emacs 22 or later).
TYPE is the type of entry to add: `Fileset' or `Candidate'."
  (interactive
   (let ((typ (if (and current-prefix-arg  icicle-filesets-as-saved-completion-sets-flag
                       (prog1 (or (require 'filesets nil t)
                                  (error "Feature `filesets' not provided"))
                         (filesets-init))
                       filesets-data)
                  'Fileset
                'Candidate)))
     (list
      (save-selected-window
        (completing-read "Saved completion set: " icicle-saved-completion-sets nil t nil
                         'icicle-completion-set-history))
      (if (eq typ 'Fileset)
          (list ':fileset               ; Just save the fileset name, not the data.
                (car (assoc (completing-read "Fileset to add: " filesets-data nil t)
                            filesets-data)))
        (completing-read "Candidate to add: " (mapcar #'list icicle-saved-completion-candidates)))
      typ)))
  (let ((file-name  (cdr (assoc set-name icicle-saved-completion-sets))))
    (unless (icicle-file-readable-p file-name) (error "Cannot read cache file `%s'" file-name))
    (let ((list-buf  (find-file-noselect file-name 'NOWARN 'RAW))
          candidates newcands entry-type)
      (unwind-protect
           (condition-case icicle-add-entry-to-saved-completion-set
               (when (listp (setq newcands  (setq candidates  (read list-buf))))
                 (message "Set `%s' read from file `%s'" set-name file-name))
             (error (error "Bad cache file.  %s"
                           (error-message-string icicle-add-entry-to-saved-completion-set))))
        (kill-buffer list-buf))
      (unless (consp newcands) (error "Bad data in cache file `%s'" file-name))
      (pushnew entry newcands :test #'equal)
      (setq entry  (if (eq type 'Fileset) (caar entry) entry))
      (if (= (length candidates) (length newcands))
          (message "%s `%s' is already in saved set `%s', file `%s'" type entry set-name file-name)
        (with-temp-message (format "Writing entry to cache file `%s'..." file-name)
          (with-temp-file file-name (prin1 newcands (current-buffer))))
        (message "%s `%s' added to saved set `%s', file `%s'" type
                 (icicle-propertize entry     'face 'icicle-msg-emphasis)
                 (icicle-propertize set-name  'face 'icicle-msg-emphasis)
                 (icicle-propertize file-name 'face 'icicle-msg-emphasis))))))

;;;###autoload (autoload 'icicle-remove-entry-from-saved-completion-set "icicles")
(defun icicle-remove-entry-from-saved-completion-set (set-name)
  "Remove an entry from saved completion-candidates set SET-NAME.
SET-NAME can be an Icicles saved completions set (cache file) or the
name of an Emacs fileset.

The entry to remove can be a single completion candidate (a string) or
an Emacs fileset.  You can thus remove a file name from a fileset or
remove a fileset from an Icicles saved completion set.  (You can also
remove a file name from a saved completion set.)  If a saved set has
both a file and a fileset of the same name, then both are removed.

To use filesets here, use Emacs 22 or later, load library `filesets',
use `(filesets-init)', and ensure that option
`icicle-filesets-as-saved-completion-sets-flag' is non-nil."
  (interactive
   (list (completing-read "Saved completion set: "
                          (if (and icicle-filesets-as-saved-completion-sets-flag
                                   (featurep 'filesets)  filesets-data)
                              (append filesets-data icicle-saved-completion-sets)
                            icicle-saved-completion-sets)
                          nil t nil 'icicle-completion-set-history)))
  (let* ((file-name                              (cdr (assoc set-name icicle-saved-completion-sets)))
         (candidates                             (icicle-get-candidates-from-saved-set
                                                  set-name 'dont-expand))
         (icicle-whole-candidate-as-text-prop-p  t)
         (icicle-remove-icicles-props-p          nil) ; Need prop `icicle-whole-candidate' for now.
         (entry
          (funcall icicle-get-alist-candidate-function
                   (completing-read
                    "Candidate to remove: "
                    (mapcar (lambda (e)
                              (cond ((icicle-saved-fileset-p e) ; Swap `:fileset' with fileset name
                                     `(,(cadr e) ,(car e) ,@(cddr e)))
                                    ((consp e) e)
                                    (t (list e)))) ; Listify naked string.
                            candidates)
                    nil t))))
    (when (and (consp entry)  (eq (cadr entry) ':fileset)) ; Swap back again: `:fileset' and name.
      (setq entry  `(,(cadr entry) ,(car entry) ,@(cddr entry))))
    (when (and (consp entry)  (null (cdr entry))) (setq entry  (car entry))) ; Use just the string.
    ;; Delete any such candidate, then remove text properties used for completion.
    (setq candidates  (mapcar #'icicle-unpropertize-completion (delete entry candidates)))
    (cond (file-name
           (with-temp-message           ; Remove from cache file.
               (format "Writing remaining candidates to cache file `%s'..." file-name)
             (with-temp-file file-name (prin1 candidates (current-buffer)))))
          ((icicle-saved-fileset-p (list ':fileset set-name)) ; Remove from fileset.
           (unless (require 'filesets nil t) (error "Feature `filesets' not provided"))
           (filesets-init)
           (let ((fst  (and filesets-data  (assoc set-name filesets-data)))) ; The fileset itself.
             (unless fst (error "No such fileset: `%s'" set-name))
             (let ((fst-files  (filesets-entry-get-files fst)))
               (if (car (filesets-member entry fst-files :test 'filesets-files-equalp))
                   (if fst-files        ; Similar to code in `filesets-remove-buffer'.
                       (let ((new-fst  (list (cons ':files (delete entry fst-files)))))
                         (setcdr fst new-fst)
                         (filesets-set-config set-name 'filesets-data filesets-data))
                     (message "Cannot remove `%s' from fileset `%s'"
                              (icicle-propertize entry    'face 'icicle-msg-emphasis)
                              (icicle-propertize set-name 'face 'icicle-msg-emphasis)))
                 (message "`%s' not in fileset `%s'"
                          (icicle-propertize entry    'face 'icicle-msg-emphasis)
                          (icicle-propertize set-name 'face 'icicle-msg-emphasis)))))))
    (when entry
      (icicle-msg-maybe-in-minibuffer
       "`%s' removed from %s `%s'%s"
       (icicle-propertize (if (icicle-saved-fileset-p entry) (cadr entry) entry)
                          'face 'icicle-msg-emphasis)
       (if (icicle-saved-fileset-p entry) "fileset" "saved set")
       (icicle-propertize set-name 'face 'icicle-msg-emphasis)
       (if file-name
           (format ", file `%s'" (icicle-propertize file-name'face 'icicle-msg-emphasis))
         "")))))

;;;###autoload (autoload 'icicle-remove-saved-completion-set "icicles")
(icicle-define-command icicle-remove-saved-completion-set ; Command name
  "Remove an entry from `icicle-saved-completion-sets'.
Save the updated option.
You are prompted to also delete the associated cache file.
You can add entries to `icicle-saved-completion-sets' using command
`icicle-add/update-saved-completion-set'." ; Doc string
  icicle-remove-saved-set-action
  "Remove set of completion candidates named: " ; `completing-read' args
  icicle-saved-completion-sets nil t nil 'icicle-completion-set-history nil nil
  ((icicle-whole-candidate-as-text-prop-p  t) ; Additional bindings
   (icicle-use-candidates-only-once-flag   t))
  nil nil (icicle-remove-Completions-window)) ; First code, undo code, last code

(defun icicle-remove-saved-set-action (set-name)
  "Remove saved set SET-NAME from `icicle-saved-completion-sets'."
  (let ((enable-recursive-minibuffers  t)
        (sets                          icicle-saved-completion-sets)
        set cache)
    (save-selected-window
      (select-window (minibuffer-window))
      (while (setq set    (assoc set-name sets)
                   cache  (cdr set))
        (when (file-exists-p cache)
          (if (y-or-n-p (format "Delete cache file `%s'? "
                                (icicle-propertize cache 'face 'icicle-msg-emphasis)))
              (when (condition-case err
                        (progn (delete-file cache) t)
                      (error (progn (message "%s" (error-message-string err)) nil)))
                (message "%s `%s'" (icicle-propertize "DELETED" 'face 'icicle-msg-emphasis) cache)
                (sit-for 1))
            (message "OK, file NOT deleted") (sit-for 1)))
        (setq sets  (delete set sets)))))
  (setq icicle-saved-completion-sets
        (icicle-assoc-delete-all set-name icicle-saved-completion-sets))
  (funcall icicle-customize-save-variable-function
           'icicle-saved-completion-sets
           icicle-saved-completion-sets)
  (message "Candidate set `%s' removed" (icicle-propertize set-name 'face 'icicle-msg-emphasis)))

;;;###autoload (autoload 'icicle-bookmark-save-marked-files "icicles")
(defun icicle-bookmark-save-marked-files (&optional arg) ; Bound to `C-M->' in *Bookmark List*.
  "Save file names of marked bookmarks as a set of completion candidates.
Saves file names in variable `icicle-saved-completion-candidates', by
default.  Marked bookmarks that have no associated file are ignored.
With a plain prefix arg (`C-u'), save candidates in a cache file.
With a non-zero numeric prefix arg (`C-u N'), save candidates in a
 variable for which you are prompted.
With a zero prefix arg (`C-0'), save candidates in a fileset (Emacs 22
 or later).  Use this only for file-name candidates, obviously.
 To subsequently use a fileset for candidate retrieval, option
 `icicle-filesets-as-saved-completion-sets-flag' must be non-nil.

You can retrieve the saved set of file-name candidates during
completion using `\\<minibuffer-local-completion-map>\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from a bookmark-list display buffer
\(`*Bookmark List*')."
  (interactive "P")
  (unless (fboundp 'bmkp-bmenu-get-marked-files)
    (error "Command `icicle-bookmark-save-marked-files' requires library `Bookmark+'"))
  (bmkp-bmenu-barf-if-not-in-menu-list)
  (icicle-candidate-set-save-1 (bmkp-bmenu-get-marked-files) arg))

;;;###autoload (autoload 'icicle-bookmark-save-marked-files-more "icicles")
(defun icicle-bookmark-save-marked-files-more (&optional arg) ; Bound to `C->' in *Bookmark List*.
  "Add the file names of the marked bookmarks to the saved candidates set.
Marked bookmarks that have no associated file are ignored.
Add candidates to `icicle-saved-completion-candidates', by default.
A prefix argument acts the same as for `icicle-candidate-set-save'.

The existing saved candidates remain saved.  The current candidates
are added to those already saved.

You can retrieve the saved set of candidates with `C-M-<'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from a bookmark-list display buffer
\(`*Bookmark List*')."
  (interactive "P")
  (unless (fboundp 'bmkp-bmenu-get-marked-files)
    (error "Command `icicle-bookmark-save-marked-files-more' requires library `Bookmark+'"))
  (bmkp-bmenu-barf-if-not-in-menu-list)
  (icicle-candidate-set-save-1 (bmkp-bmenu-get-marked-files) arg t))

;;;###autoload (autoload 'icicle-bookmark-save-marked-files-to-variable "icicles")
(defun icicle-bookmark-save-marked-files-to-variable () ; Bound to `C-M-}' in *Bookmark List*.
  "Save the file names of the marked bookmarks to a variable.
Marked bookmarks that have no associated file are ignored.

You can retrieve the saved set of file-name candidates during
completion using `\\<minibuffer-local-completion-map>\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from a bookmark-list display buffer
\(`*Bookmark List*')."
  (interactive)
  (unless (fboundp 'bmkp-bmenu-get-marked-files)
    (error "Command `icicle-bookmark-save-marked-files-to-variable' requires library `Bookmark+'"))
  (bmkp-bmenu-barf-if-not-in-menu-list)
  (icicle-candidate-set-save-1 (bmkp-bmenu-get-marked-files) 99))

;;;###autoload (autoload 'icicle-bookmark-save-marked-files-as-project "icicles")
(defalias 'icicle-bookmark-save-marked-files-as-project ; Bound to `C-}' in *Bookmark List*.
    'icicle-bookmark-save-marked-files-persistently)
;;;###autoload (autoload 'icicle-bookmark-save-marked-files-persistently "icicles")
(defun icicle-bookmark-save-marked-files-persistently (filesetp)
  "Save the file names of the marked bookmarks as a persistent set.
Marked bookmarks that have no associated file are ignored.
With no prefix arg, save in a cache file.
With a prefix arg, save in an Emacs fileset (Emacs 22 or later).

You can retrieve the saved set of file-name candidates during
completion using `\\<minibuffer-local-completion-map>\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from a bookmark-list display buffer
\(`*Bookmark List*')."
  (interactive "P")
  (unless (fboundp 'bmkp-bmenu-get-marked-files)
    (error "This command requires library `Bookmark+'"))
  (bmkp-bmenu-barf-if-not-in-menu-list)
  (icicle-candidate-set-save-1 (bmkp-bmenu-get-marked-files) (if filesetp 0 '(1))))


;;;###autoload (autoload 'icicle-dired-save-marked                        "icicles")
;;;###autoload (autoload 'icicle-dired-save-marked-more                   "icicles")
;;;###autoload (autoload 'icicle-dired-save-marked-to-variable            "icicles")
;;;###autoload (autoload 'icicle-dired-save-marked-as-project             "icicles")
;;;###autoload (autoload 'icicle-dired-save-marked-persistently           "icicles")
(defun icicle-dired-save-marked (&optional arg) ; Bound to `C-M->' in Dired.
  "Save the marked file names in Dired as a set of completion candidates.
Saves file names in variable `icicle-saved-completion-candidates', by
default.
With a plain prefix arg (`C-u'), save candidates in a cache file.
With a non-zero numeric prefix arg (`C-u N'), save candidates in a
 variable for which you are prompted.
With a zero prefix arg (`C-0'), save candidates in a fileset (Emacs 22
 or later).  Use this only for file-name candidates, obviously.
 To subsequently use a fileset for candidate retrieval, option
 `icicle-filesets-as-saved-completion-sets-flag' must be non-nil.

You can retrieve the saved set of file-name candidates during
completion using `\\<minibuffer-local-completion-map>\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from a Dired buffer."
  (interactive "P")
  (unless (eq major-mode 'dired-mode)
    (error "You must be in a Dired buffer to use this command"))
  (icicle-candidate-set-save-1 (dired-get-marked-files) arg))

(defun icicle-dired-save-marked-more (&optional arg) ; Bound to `C->' in Dired.
  "Add the marked file names in Dired to the saved candidates set.
Like `icicle-dired-save-marked', but add file names to those already
saved, if any.  A prefix argument has the same effect as for
`icicle-dired-save-marked'."
  (interactive "P")
  (unless (eq major-mode 'dired-mode)
    (error "You must be in a Dired buffer to use this command"))
  (icicle-candidate-set-save-1 (dired-get-marked-files) arg t))

(defun icicle-dired-save-marked-to-variable () ; Bound to `C-M-}' in Dired.
  "Save the marked file names in Dired to a variable as a candidate set.
Same as using `icicle-dired-save-marked' with no prefix argument."
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (error "You must be in a Dired buffer to use this command"))
  (icicle-candidate-set-save-1 (dired-get-marked-files) 99))

(defalias 'icicle-dired-save-marked-as-project ; Bound to `C-}' in Dired.
    'icicle-dired-save-marked-persistently)
(defun icicle-dired-save-marked-persistently (filesetp)
  "Save the marked file names in Dired as a persistent set.
With no prefix arg, save in a cache file.
With a prefix arg, save in an Emacs fileset (Emacs 22 or later).

You can retrieve the saved set of file-name candidates during
completion using `\\<minibuffer-local-completion-map>\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from a Dired buffer."
  (interactive "P")
  (unless (eq major-mode 'dired-mode)
    (error "You must be in a Dired buffer to use this command"))
  (icicle-candidate-set-save-1 (dired-get-marked-files) (if filesetp 0 '(1))))


;;; These commands require library `Dired+'.
;;;
;;;###autoload (autoload 'icicle-dired-save-marked-recursive               "icicles")
;;;###autoload (autoload 'icicle-dired-save-marked-more-recursive          "icicles")
;;;###autoload (autoload 'icicle-dired-save-marked-to-variable-recursive   "icicles")
;;;###autoload (autoload 'icicle-dired-save-marked-to-cache-file-recursive "icicles")
;;;###autoload (autoload 'icicle-dired-save-marked-to-fileset-recursive    "icicles")
;;;###autoload (autoload 'icicle-dired-insert-as-subdir                    "icicles")
(when (fboundp 'diredp-get-files)       ; In Dired+.
  (defun icicle-dired-save-marked-recursive (&optional ignore-marks-p arg) ; Bound to `M-+ C-M->' in Dired.
    "Save the marked file names in Dired, including those in marked subdirs.
Like `icicle-dired-save-marked', but act recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

You need library `Dired+' for this command."
    (interactive (progn
                   (unless (fboundp 'diredp-get-confirmation-recursive)
                     (error "You need library `dired+.el' for this command"))
                   (diredp-get-confirmation-recursive)
                   (list current-prefix-arg 1)))
    (icicle-candidate-set-save-1 (diredp-get-files ignore-marks-p) arg))

  (defun icicle-dired-save-marked-more-recursive (&optional ignore-marks-p arg) ; Bound to `M-+ C->' in Dired.
    "Add marked files, including those in marked subdirs, to saved candidates.
Like `icicle-dired-save-marked-more', but act recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

You need library `Dired+' for this command."
    (interactive (progn
                   (unless (fboundp 'diredp-get-confirmation-recursive)
                     (error "You need library `dired+.el' for this command"))
                   (diredp-get-confirmation-recursive)
                   (list current-prefix-arg 1)))
    (icicle-candidate-set-save-1 (diredp-get-files ignore-marks-p) arg t))

  (defun icicle-dired-save-marked-to-variable-recursive (&optional ignore-marks-p) ; `M-+ C-M-}' in Dired.
    "Save marked files, including those in marked subdirs, to a variable.
Like `icicle-dired-save-marked-to-variable', but act recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

You need library `Dired+' for this command."
    (interactive (progn
                   (unless (fboundp 'diredp-get-confirmation-recursive)
                     (error "You need library `dired+.el' for this command"))
                   (diredp-get-confirmation-recursive)
                   (list current-prefix-arg)))
    (icicle-candidate-set-save-1 (diredp-get-files ignore-marks-p) 99))

  (defun icicle-dired-save-marked-to-cache-file-recursive (&optional ignore-marks-p) ; `M-+ C-}' in Dired.
    "Save marked files, including in marked subdirs, to an Icicles cache set.
Like `icicle-dired-save-marked-persistently' with no prefix arg, but
act recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

You need library `Dired+' for this command."
    (interactive (progn
                   (unless (fboundp 'diredp-get-confirmation-recursive)
                     (error "You need library `dired+.el' for this command"))
                   (diredp-get-confirmation-recursive)
                   (list current-prefix-arg)))
    (icicle-candidate-set-save-1 (diredp-get-files ignore-marks-p) '(1)))

  (defun icicle-dired-save-marked-to-fileset-recursive (&optional ignore-marks-p) ; Not bound by default.
    "Save marked files, including those in marked subdirs, to an Emacs fileset.
Like `icicle-dired-save-marked-persistently' with no prefix arg, but
act recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

You need library `Dired+' for this command."
    (interactive (progn
                   (unless (fboundp 'diredp-get-confirmation-recursive)
                     (error "You need library `dired+.el' for this command"))
                   (unless (require 'filesets nil t)
                     (error "Cannot save to a fileset - feature `filesets' not provided"))
                   (diredp-get-confirmation-recursive)
                   (list current-prefix-arg)))
    (icicle-candidate-set-save-1 (diredp-get-files ignore-marks-p) 0)))

(when (and (> emacs-major-version 21)   ; Emacs 20 has no PREDICATE arg to `read-file-name'.
           (fboundp 'diredp-insert-as-subdir))
  (icicle-define-file-command icicle-dired-insert-as-subdir
    "Choose a directory.  Insert it into a Dired ancestor listing.
If the directory you choose has a Dired buffer then its markings and
switches are preserved for the subdir listing in the ancestor Dired
buffer.

You need library `Dired+' for this command."
    (lambda (dir) (diredp-insert-as-subdir dir ancestor-dir)) ; FREE here: ANCESTOR-DIR.
    "Insert directory into ancestor Dired: " ; `read-file-name' args
    default-directory nil t nil `(lambda (ff)
                                  (and (file-directory-p  (expand-file-name ff))
                                   (dired-in-this-tree (expand-file-name ff) ',ancestor-dir)))
    ((ancestor-dir                      ; Bindings
      (completing-read "Ancestor Dired dir to insert into: "
                       (cons (list default-directory)
                             (mapcar #'list (diredp-ancestor-dirs default-directory))))))))


(put 'icicle-dired-saved-file-candidates 'icicle-Completions-window-max-height 200)
;;;###autoload (autoload 'icicle-dired-chosen-files "icicles")
(defalias 'icicle-dired-chosen-files 'icicle-dired-saved-file-candidates)
;;;###autoload (autoload 'icicle-dired-saved-file-candidates "icicles")
(defun icicle-dired-saved-file-candidates (prompt-for-dir-p)
  "Open Dired on a set of files and directories of your choice.
If you have saved a set of file names using \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]', then it is used.
If not, you are reminded to do so.
With a prefix argument, you are prompted for the default directory to use.
Otherwise, the current value of `default-directory' is used.
Names that do not correspond to existing files are ignored.
Existence of files with relative names is checked in the Dired
directory (default directory)."
  (interactive "P")
  ;; $$$$$$$ Maybe filter sets to get only file-name candidate sets?
  (unless icicle-saved-completion-candidates
    (error "%s" (substitute-command-keys "No saved completion candidates.  \
Use \\<minibuffer-local-completion-map>`\\[icicle-candidate-set-save]' to save candidates")))
  (let* ((default-directory           (if prompt-for-dir-p
                                          (read-file-name "Directory: " nil default-directory nil)
                                        default-directory))
         (icicle-multi-completing-p   t)
         (icicle-list-use-nth-parts   '(1))
         (file-names                  (icicle-remove-if
                                       (lambda (fil)
                                         (or (null fil)
                                             (not (or (icicle-file-remote-p fil) ; Avoid Tramp accessing.
                                                      (file-exists-p fil)))))
                                       (or (and icicle-saved-completion-candidates
                                                (mapcar #'icicle-transform-multi-completion
                                                        icicle-saved-completion-candidates))
                                           (icicle-file-list)))))
    (dired (cons (generate-new-buffer-name "Icy File Set") (nreverse file-names)))))

;;;###autoload (autoload 'icicle-dired-chosen-files-other-window "icicles")
(defalias 'icicle-dired-chosen-files-other-window 'icicle-dired-saved-file-candidates-other-window)
;;;###autoload (autoload 'icicle-dired-saved-file-candidates-other-window "icicles")
(defun icicle-dired-saved-file-candidates-other-window (prompt-for-dir-p) ; Bound `C-M-<' in Dired.
  "Open Dired in other window on set of files & directories of your choice.
If you have saved a set of file names using \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]', then it is used.
If not, you are reminded to do so.
With a prefix arg, you are prompted for the default directory to use.
Otherwise, the current value of `default-directory' is used.
Names that do not correspond to existing files are ignored.
Existence of files with relative names is checked in the Dired
directory (default directory)."
  (interactive "P")
  ;; $$$$$$$ Maybe filter sets to get only file-name candidate sets?
  (let* ((default-directory           (if prompt-for-dir-p
                                          (read-file-name "Directory: " nil default-directory nil)
                                        default-directory))
         (icicle-multi-completing-p   t)
         (icicle-list-use-nth-parts   '(1))
         (file-names                  (icicle-remove-if
                                       (lambda (fil)
                                         (or (null fil)
                                             (not (or (icicle-file-remote-p fil) ; Avoid Tramp accessing.
                                                      (file-exists-p fil)))))
                                       (or (and icicle-saved-completion-candidates
                                                (mapcar #'icicle-transform-multi-completion
                                                        icicle-saved-completion-candidates))
                                           (icicle-file-list)))))
    (dired-other-window (cons (generate-new-buffer-name "Icy File Set") (nreverse file-names)))))

(put 'icicle-dired-project 'icicle-Completions-window-max-height 200)
;;;###autoload (autoload 'icicle-dired-project "icicles")
(defun icicle-dired-project (prompt-for-dir-p)
  "Open Dired on a saved project.
A project is either a persistent completion set or an Emacs fileset.
With a prefix argument, you are prompted for the directory.
Otherwise, the default directory is assumed.

Project file names that do not correspond to existing files are
ignored.  Existence of files with relative names is checked in the
directory.

You can use `C-x m' during completion to access Dired bookmarks, if
you use library `Bookmark+'."
  (interactive "P")
  (when (require 'bookmark+ nil t)
    (define-key minibuffer-local-completion-map (icicle-kbd "C-x m") ; `C-x m'
      'icicle-bookmark-dired-other-window))
  (unwind-protect
       ;; $$$$$$$ Maybe filter sets to get only file-name candidate sets?
       (let ((set-name  (completing-read "Project (saved file names): "
                                         (if (and icicle-filesets-as-saved-completion-sets-flag
                                                  (featurep 'filesets) filesets-data)
                                             (append filesets-data icicle-saved-completion-sets)
                                           icicle-saved-completion-sets)
                                         nil nil nil 'icicle-completion-set-history)))
         (icicle-retrieve-candidates-from-set set-name)
         (let* ((default-directory  (if prompt-for-dir-p
                                        (read-file-name "Dired directory: " nil default-directory nil)
                                      default-directory))
                (file-names         ()))
           (dolist (ff  icicle-saved-completion-candidates)
             (when (or (icicle-file-remote-p ff) ; Don't let Tramp try to access it.
                       (file-exists-p ff))
               (push ff file-names)))
           (unless file-names (error "No files in project `%s' actually exist" set-name))
           (dired (cons (generate-new-buffer-name set-name)
                        (nreverse (mapcar (lambda (file)
                                            (if (file-name-absolute-p file)
                                                (expand-file-name file)
                                              file))
                                          file-names))))))
    (define-key minibuffer-local-completion-map (icicle-kbd "C-x m") nil)))

;;;###autoload (autoload 'icicle-dired-project-other-window "icicles")
(defun icicle-dired-project-other-window (prompt-for-dir-p) ; Bound to `C-{' in Dired.
  "Open Dired on a saved project in another window.
A project is either a persistent completion set or an Emacs fileset.
With a prefix argument, you are prompted for the directory.
Otherwise, the default directory is assumed.

Project file names that do not correspond to existing files are
ignored.  Existence of files with relative names is checked in the
directory.

You can use `C-x m' during completion to access Dired bookmarks, if
you use library `Bookmark+'."
  (interactive "P")
  (when (require 'bookmark+ nil t)
    (define-key minibuffer-local-completion-map (icicle-kbd "C-x m") ; `C-x m'
      'icicle-bookmark-dired-other-window))
  (unwind-protect
       ;; $$$$$$$ Maybe filter sets to get only file-name candidate sets?
       (let ((set-name  (completing-read "Project (saved file names): "
                                         (if (and icicle-filesets-as-saved-completion-sets-flag
                                                  (featurep 'filesets) filesets-data)
                                             (append filesets-data icicle-saved-completion-sets)
                                           icicle-saved-completion-sets)
                                         nil nil nil 'icicle-completion-set-history)))
         (icicle-retrieve-candidates-from-set set-name)
         (let* ((default-directory  (if prompt-for-dir-p
                                        (read-file-name "Dired directory: " nil default-directory nil)
                                      default-directory))
                (file-names         ()))
           (dolist (ff  icicle-saved-completion-candidates)
             (when (or (icicle-file-remote-p ff) ; Don't let Tramp try to access it.
                       (file-exists-p ff))
               (push ff file-names)))
           (unless file-names (error "No files in project `%s' actually exist" set-name))
           (dired-other-window (cons (generate-new-buffer-name set-name)
                                     (nreverse (mapcar (lambda (file)
                                                         (if (file-name-absolute-p file)
                                                             (expand-file-name file)
                                                           file))
                                                       file-names))))))
    (define-key minibuffer-local-completion-map (icicle-kbd "C-x m") nil)))

;;;###autoload (autoload 'icicle-grep-saved-file-candidates "icicles")
(defun icicle-grep-saved-file-candidates (command-args) ; Bound to `M-s M-s g' in Icicle mode.
  "Run `grep' on the set of completion candidates saved with \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]'.
Saved names that do not correspond to existing files are ignored.
Existence of files with relative names is checked in the default
directory."
  (interactive
   (list
    (let ((file-names  ()))
      (unless icicle-saved-completion-candidates
        (error "%s" (substitute-command-keys "No saved completion candidates.  \
Use \\<minibuffer-local-completion-map>`\\[icicle-candidate-set-save]' to save candidates")))
      (unless grep-command (grep-compute-defaults))
      (dolist (ff  icicle-saved-completion-candidates)
        (when (or (icicle-file-remote-p ff) ; Don't let Tramp try to access it.
                  (file-exists-p ff))
          (push ff file-names)))
      (let ((default  (and (fboundp  'grep-default-command) (grep-default-command))))
        (read-from-minibuffer
         "grep <pattern> <files> :  "
         (let ((up-to-files  (concat grep-command "   ")))
           (cons (concat up-to-files (mapconcat #'identity icicle-saved-completion-candidates " "))
                 (- (length up-to-files) 2)))
         nil nil 'grep-history default)))))
  (grep command-args))

;; Utility function.  Use it to define multi-commands that navigate.
(defun icicle-explore (define-candidates-fn final-action-fn quit-fn error-fn cleanup-fn prompt
                                            &rest compl-read-args)
  "Icicle explorer: explore complex completion candidates.
Navigate among locations or other entities represented by a set of
completion candidates.  See `icicle-search' for a typical example.

Arguments:
 DEFINE-CANDIDATES-FN:
   Function of no args called to fill `icicle-candidates-alist' with
   the candidates.
 FINAL-ACTION-FN:
   Function of no args called after the final choice of candidate
   (after both `icicle-explore-final-choice' and
   `icicle-explore-final-choice-full' have been set).  Typically uses
   `icicle-explore-final-choice-full', the full candidate.
 QUIT-FN: Function of no args called if user uses `C-g'.
 ERROR-FN: Function of no args called if an error is raised.
 CLEANUP-FN: Function of no args called after exploring.
 PROMPT: Prompt string for `completing-read'.
 COMPL-READ-ARGS: `completing-read' args other than PROMPT and
   COLLECTION.

If there is only one candidate, then FINAL-ACTION-FN is called
immediately.  The candidate is not available to act on (e.g. using
\\<minibuffer-local-completion-map>`\\[icicle-candidate-alt-action]').

Returns:
 The result of executing FINAL-ACTION-FN, if that arg is non-nil.
 Otherwise, `icicle-explore-final-choice-full'.

To use `icicle-explore' to define a multi-command, you must also bind
`icicle-candidate-action-fn'.

Though `icicle-explore' is typically used to define navigation
commands, it need not be.  It can be useful anytime you need to use
`completing-read' and also provide specific behavior for quitting
\(`C-g'), completion errors, and final actions."
  (let ((icicle-incremental-completion          'always)
        (icicle-whole-candidate-as-text-prop-p  t)
        (icicle-transform-function              (if (interactive-p) nil icicle-transform-function))
        (icicle-act-before-cycle-flag           icicle-act-before-cycle-flag)
        (icicle-orig-pt-explore                 (point-marker))
        (icicle-orig-win-explore                (selected-window))
        result)
    (setq icicle-act-before-cycle-flag      nil
          icicle-candidates-alist           ()
          icicle-explore-final-choice       nil
          icicle-explore-final-choice-full  nil)
    (icicle-highlight-lighter)
    (message "Finding candidates...")
    (when define-candidates-fn (funcall define-candidates-fn))
    (unless icicle-candidates-alist (error "No candidates defined"))
    (when (= (length icicle-candidates-alist) 1)
      (setq icicle-explore-final-choice  (icicle-display-cand-from-full-cand (car icicle-candidates-alist))))
    (unwind-protect
         (icicle-condition-case-no-debug failure
             (progn
               (unless icicle-explore-final-choice
                 (setq icicle-explore-final-choice
                       (let ((icicle-remove-icicles-props-p  nil)) ; Keep Icicles text properties.
                         (apply #'completing-read prompt icicle-candidates-alist compl-read-args))))
               (setq icicle-explore-final-choice-full
                     (funcall icicle-get-alist-candidate-function icicle-explore-final-choice 'no-error-p))
               (unless icicle-explore-final-choice-full (error "No such occurrence"))
               (setq result  (if final-action-fn
                                 (funcall final-action-fn)
                               icicle-explore-final-choice-full)))
           (quit (if quit-fn (funcall quit-fn) (keyboard-quit)))
           (error (when error-fn (funcall error-fn))
                  (error "%s" (error-message-string failure))))
      (setq result  (icicle-unpropertize-completion result)) ; Remove any Icicles text properties.
      (when cleanup-fn (funcall cleanup-fn)))
    result))

;;;###autoload (autoload 'icicle-execute-extended-command "icicles")
(icicle-define-command icicle-execute-extended-command ; Bound to `M-x' in Icicle mode.
  "Read command name, then read its arguments and call it.
This is `execute-extended-command', turned into a multi-command.

You can use `\\<minibuffer-local-completion-map>\\[icicle-toggle-transforming]' \
to toggle filtering of candidates to those that are
bound to keys.

You can use `\\[icicle-toggle-annotation]' to toggle showing key bindings as annotations.
\(Menu bindings are not shown.)

By default, Icicle mode remaps all key sequences that are normally
bound to `execute-extended-command' to
`icicle-execute-extended-command'.  If you do not want this remapping,
then customize option `icicle-top-level-key-bindings'." ; Doc string
  icicle-execute-extended-command-1     ; Function to perform the action
  (format "Execute command%s: "         ; `completing-read' args
          (if current-prefix-arg
              (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
            ""))
  obarray (and icompletep  pred) t nil 'extended-command-history nil nil
  (;; Bindings
   (last-command                            last-command) ; Save and restore the last command.
   (use-file-dialog                         nil) ; `mouse-2' in `*Completions*' won't use dialog box.
   (alt-fn                                  nil)
   (icicle-orig-must-pass-after-match-pred  icicle-must-pass-after-match-predicate)
   (pred                                    (lambda (c)
                                              (unless (symbolp c) (setq c  (intern-soft c)))
                                              (commandp c)))
   (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
   (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
   (icicle-candidate-alt-action-fn          (or icicle-candidate-alt-action-fn
                                                (setq alt-fn  (icicle-alt-act-fn-for-type "command"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn  alt-fn  (icicle-alt-act-fn-for-type "command")))
   (icicle--last-toggle-transforming-msg    icicle-toggle-transforming-message)
   (icicle-toggle-transforming-message      "Filtering to commands bound to keys is now %s")
   (icicle-last-transform-function          (lambda (cands) ; Because we bind `icicle-transform-function'.
                                              (with-current-buffer icicle-pre-minibuffer-buffer
                                                (loop for cand in cands
                                                      for symb = (intern-soft cand)
                                                      if (and (symbolp symb)
                                                              (where-is-internal symb nil 'non-ascii))
                                                      collect cand))))
   (icicle-transform-function               nil)
   (completion-annotate-function            (lambda (cand)
                                              (with-current-buffer icicle-pre-minibuffer-buffer
                                                (and (setq cand  (intern-soft cand))  (symbolp cand)
                                                     (let ((key  (where-is-internal cand nil t)))
                                                       (and key
                                                            (format "  %s" (icicle-key-description key))))))))
   icicle-new-last-cmd)                 ; Set in `icicle-execute-extended-command-1'.
  nil  nil                              ; First code, undo code
  (setq this-command  icicle-new-last-cmd)) ; Last code: this will update `last-command'

;; Free vars here: `icicle-orig-buff' and `icicle-orig-window' are bound by `icicle-define-command'.
;;                 `icicle-new-last-cmd' and `icicle-orig-must-pass-after-match-pred' are bound in
;;                 `icicle-execute-extended-command'.
(defun icicle-execute-extended-command-1 (cmd-name)
  "Action function to execute command or named keyboard macro CMD-NAME."
  (when (get-buffer icicle-orig-buff) (set-buffer icicle-orig-buff))
  (when (window-live-p icicle-orig-window) (select-window icicle-orig-window))
  (when (string= "" cmd-name) (error "No command name"))

  (let* ((cmd                                       (intern cmd-name))
         (fn                                        (symbol-function cmd))
         (count                                     (prefix-numeric-value current-prefix-arg))
         (completion-annotate-function              nil) ; Cancel value from `icicle-execute-extended-command'.
         (icicle-toggle-transforming-message        icicle--last-toggle-transforming-msg) ; Restore - FREE HERE
         ;; Rebind alternative action functions to nil, so we don't override the command we call.
         (icicle-candidate-alt-action-fn            nil)
         (icicle-all-candidates-list-alt-action-fn  nil)
         ;; Rebind `icicle-candidate-action-fn' to a function that calls the candidate command on a single
         ;; argument that it reads.  This is used only if the command itself reads an input argument with
         ;; completion.  When that is the case, you can use completion on that input, and if you do that,
         ;; you can use `C-RET' to use the candidate command as a multi-command.  In other words, this
         ;; binding allows for two levels of multi-commands.
         (icicle-candidate-action-fn
          (and icicle-candidate-action-fn ; This is nil after the command name is read.
               `(lambda (arg)
                 (setq arg  (icicle-transform-multi-completion arg))
                 (condition-case nil
                     (funcall ',cmd arg) ; Try to use string candidate `arg'.
                   ;; If that didn't work, use a symbol or number candidate.
                   (wrong-type-argument (funcall ',cmd (car (read-from-string arg))))
                   (wrong-number-of-arguments (funcall #'icicle-help-on-candidate))) ; Punt - show help.
                 (select-window (minibuffer-window))
                 (select-frame-set-input-focus (selected-frame))))))
    (cond ((arrayp fn)
           (let ((this-command  cmd)) (execute-kbd-macro fn count))
           (when (> count 1) (message "(%d times)" count)))
          (t
           (run-hooks 'post-command-hook)
           (run-hooks 'pre-command-hook)
           (let ((enable-recursive-minibuffers            t)
                 ;; Restore this before we invoke command, since it might use completion.
                 (icicle-must-pass-after-match-predicate  icicle-orig-must-pass-after-match-pred)
                 ;; Bind, don't set `this-command'.  When you use `C-next', `this-command' needs
                 ;; to be `cmd' during the `C-RET' part, but `last-command' must not be `cmd'
                 ;; during the `next' part.
                 (this-command                            cmd))
             (call-interactively cmd 'record-it))))
    ;; Message showing what CMD is bound to.  This is pretty much the same as in `execute-extended-command',
    ;; but do not show the message if we are not at the `M-x' top level, i.e., if we are acting on a
    ;; candidate command using `C-RET' instead of `RET'.
    (when (and suggest-key-bindings  (not executing-kbd-macro)
               (not (or (icicle-get-safe this-command 'icicle-action-command)
                        ;; This one is used for `*-per-mode-action', which sets `this-command' to the cycler.
                        (icicle-get-safe this-command 'icicle-cycling-command))))
      (let* ((binding   (if (> emacs-major-version 21)
                            (where-is-internal cmd overriding-local-map t 'NOINDIRECT)
                          (where-is-internal cmd overriding-local-map t)))
             (curr-msg   (current-message))
             (wait-time  (or (and (numberp suggest-key-bindings)  suggest-key-bindings)  2)))
        (when (and binding  (not (and (vectorp binding)  (eq (aref binding 0) 'mouse-movement))))
          (let ((message-log-max  nil)  ; Do not log this message.
                ;; If CMD showed a msg in echo area, wait a bit, before showing the key-reminder msg.
                (waited           (sit-for (if (current-message)  wait-time  0))))
            (when (and waited  (atom unread-command-events))
              (unwind-protect
                   (progn (message "You can invoke command `%s' using `%s'"
                                   (icicle-propertize (symbol-name cmd) 'face 'icicle-msg-emphasis)
                                   (icicle-propertize (key-description binding) 'face 'icicle-msg-emphasis))
                          (sit-for wait-time))
                (message "%s" curr-msg)))))))
    ;; After `M-x' `last-command' must be the command finally entered with `RET' or, if you end
    ;; with `C-g', the last command entered with `C-RET'.
    (setq icicle-new-last-cmd  cmd)))

;; Inspired by Emacs partial completion and by library `exec-abbrev-cmd.el' (Tassilo Horn
;; <tassilo@member.fsf.org>).  The idea of command abbreviation is combined here with normal
;; command invocation, in an Icicles multi-command.
;;
;;;###autoload (autoload 'icicle-command-abbrev "icicles")
(icicle-define-command icicle-command-abbrev ; Bound to `C-x SPC' in Icicle mode.
  "Read command name or its abbreviation, read command args, call command.
Read input, then call `icicle-command-abbrev-action' to act on it.

If `icicle-add-proxy-candidates-flag' is non-nil, then command
abbreviations, as well as commands, are available as completion
candidates.  Otherwise, only commands are available.  You can toggle
`icicle-add-proxy-candidates-flag' using `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-proxy-candidates]'in the minibuffer.

When an abbreviation is available, you can treat it just like a
command.  The rest of this description covers the behavior of choosing
an abbreviation.

Completion for an abbreviation is lax.  If you enter a new
abbreviation then it is added to option `icicle-command-abbrev-alist',
which is the list of your known abbreviations.  You can also customize
this list.

If an abbreviation that you enter matches a single command name then
that command is invoked.  If it matches more than one, then you can
use (strict) completion to choose one.

Hyphens (`-') in command names divide them into parts.  For example,
`find-file' has two parts: `find' and `file'.  Each character of a
command abbreviation corresponds to one part of each of the commands
that match the abbreviation.  For example, abbreviation `ff' matches
commands `find-file' and `focus-frame', and abbreviation `fg' matches
`find-grep'.

If user option `icicle-command-abbrev-match-all-parts-flag' is nil
then an abbreviation need not match all parts of a command name; it
need match only a prefix.  For example, if nil then abbreviation `ff'
also matches `find-file-other-window' and `fg' also matches
`find-grep-dired'.

You can use `\\<minibuffer-local-completion-map>\\[icicle-toggle-transforming]' \
to toggle filtering of candidates to those that are
bound to keys.

You can use `\\[icicle-toggle-annotation]' to toggle showing key bindings as annotations.
\(Menu bindings are not shown.)"        ; Doc string
  icicle-command-abbrev-action          ; Function to perform the action
  prompt obarray (and icompletep  pred) nil nil ; `completing-read' args
  'icicle-command-abbrev-history nil nil
  ((prompt                                  "Command or abbrev: ")
   (last-command                            last-command) ; Save and restore the last command.
   (icicle-sort-comparer                    'icicle-proxy-candidate-first-p)
   (first-sort-entries                      '(("proxy candidates first" . icicle-proxy-candidate-first-p)
                                              ("by abbrev frequency" . icicle-command-abbrev-used-more-p)))
   (icicle-sort-orders-alist                (append
                                             first-sort-entries
                                             (delete (car first-sort-entries)
                                                     (delete (cadr first-sort-entries)
                                                             (copy-sequence icicle-sort-orders-alist)))))
   (icicle-allowed-sort-predicate           'icicle-command-abbrev-used-more-p)
   (icicle-proxy-candidates                 (let ((ipc  ())
                                                  abv)
                                              (dolist (entry  icicle-command-abbrev-alist  ipc)
                                                (setq abv  (symbol-name (cadr entry)))
                                                (unless (member abv ipc) (push abv ipc)))
                                              ipc))
   (use-file-dialog                         nil) ; `mouse-2' in `*Completions*' won't use dialog box.
   (alt-fn                                  nil)
   (icicle-orig-must-pass-after-match-pred  icicle-must-pass-after-match-predicate)
   (pred                                    (lambda (c)
                                              (unless (symbolp c) (setq c  (intern-soft c)))
                                              (commandp c)))
   (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
   (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
   (icicle-candidate-alt-action-fn          (or icicle-candidate-alt-action-fn
                                                (setq alt-fn  (icicle-alt-act-fn-for-type "command"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn  alt-fn  (icicle-alt-act-fn-for-type "command")))
   (icicle-toggle-transforming-message      "Filtering to commands bound to keys is now %s")
   (icicle-last-transform-function          (lambda (cands) ; Because we bind `icicle-transform-function'.
                                              (with-current-buffer icicle-pre-minibuffer-buffer
                                                (loop for cand in cands
                                                      for symb = (intern-soft cand)
                                                      if (and (symbolp symb)
                                                              (where-is-internal symb nil 'non-ascii))
                                                      collect cand))))
   (icicle-transform-function               nil)
   (completion-annotate-function            (lambda (cand)
                                              (with-current-buffer icicle-pre-minibuffer-buffer
                                                (and (setq cand  (intern-soft cand))  (symbolp cand)
                                                     (let ((key  (where-is-internal cand nil t)))
                                                       (and key
                                                            (format "  %s" (icicle-key-description key)))))))))
  (when icicle-proxy-candidates (put-text-property 0 1 'icicle-fancy-candidates t prompt)) ; First code
  nil (setq icicle-proxy-candidates  ())) ; Undo code, last code

(defun icicle-command-abbrev-action (abbrev-or-cmd)
  "Action function for `icicle-command-abbrev'.
If ABBREV-OR-CMD is a command, call it.
If ABBREV-OR-CMD is an abbreviation for a single command, invoke it.
If ABBREV-OR-CMD is an abbreviation for multiple commands, call
`icicle-command-abbrev-command', to let user choose commands.
If ABBREV-OR-CMD is not an abbreviation or a command, raise an error."
  (setq abbrev-or-cmd  (intern abbrev-or-cmd))
  (let* ((completion-annotate-function              nil) ; Cancel value from `icicle-command-abbrev'.
         (icicle-add-proxy-candidates-flag          icicle-add-proxy-candidates-flag)
         (icicle-proxy-candidates                   icicle-proxy-candidates)
         ;; Restore this before we invoke command, since it might use completion.
         ;; Free var `orig-must-pass...' is bound in `icicle-command-abbrev'.
         (icicle-must-pass-after-match-predicate    icicle-orig-must-pass-after-match-pred)
         ;; Rebind alternative action functions to nil, so we don't override command we call.
         (icicle-candidate-alt-action-fn            nil)
         (icicle-all-candidates-list-alt-action-fn  nil)
         (not-cmdp                                  (not (commandp abbrev-or-cmd)))
         (regexp                                    (and (or not-cmdp  icicle-command-abbrev-priority-flag)
                                                         (icicle-command-abbrev-regexp abbrev-or-cmd)))
         (icicle-commands-for-abbrev
          (and (or not-cmdp  icicle-command-abbrev-priority-flag)
               (nconc (let ((result  ()))
                        (dolist (abrv  icicle-command-abbrev-alist)
                          (when (string= (symbol-name (cadr abrv)) (symbol-name abbrev-or-cmd))
                            (push (list (symbol-name (car abrv))) result)))
                        result)
                      (icicle-command-abbrev-matching-commands regexp)))))
    (cond ((and not-cmdp  (null icicle-commands-for-abbrev))
           (error "No such command or abbreviation `%s'" abbrev-or-cmd))
          (icicle-commands-for-abbrev
           (let ((cmd  (if (null (cdr icicle-commands-for-abbrev))
                           (prog1 (intern (caar icicle-commands-for-abbrev))
                             (push (caar icicle-commands-for-abbrev) extended-command-history)
                             (call-interactively (intern (caar icicle-commands-for-abbrev)) t))
                         (let ((enable-recursive-minibuffers  t)
                               (icicle-current-input          abbrev-or-cmd))
                           (icicle-remove-Completions-window)
                           (icicle-command-abbrev-command)))))
             (icicle-command-abbrev-record abbrev-or-cmd cmd)))
          ((not not-cmdp) (call-interactively abbrev-or-cmd)))))

(defun icicle-command-abbrev-regexp (abbrev)
  "Return the command-matching regexp for ABBREV."
  (let ((char-list  (append (symbol-name abbrev) ()))
        (str        "^"))
    (dolist (c  char-list) (setq str  (concat str (list c) "[^-]*-")))
    (concat (substring str 0 (1- (length str)))
            (if icicle-command-abbrev-match-all-parts-flag "$" ".*$"))))

(defun icicle-command-abbrev-matching-commands (regexp)
  "Return a completion alist of commands that match REGEXP."
  (mapcar #'list (icicle-remove-if-not
                  `(lambda (strg) (string-match ',regexp strg))
                  (lexical-let ((cmds  ()))
                    (mapatoms
                     (lambda (symb) (when (commandp symb) (push (symbol-name symb) cmds)))) ; FREE here: CMDS.
                    cmds))))

;;;###autoload (autoload 'icicle-command-abbrev-command "icicles")
(icicle-define-command icicle-command-abbrev-command
  "Read command name, then read its arguments and call command." ; Doc string
  icicle-execute-extended-command-1     ; Function to perform the action
  (format "Command abbreviated%s%s: "   ; `completing-read' arguments
          (cond ((and icicle-current-input  (not (string= "" icicle-current-input)))
                 (format " `%s'" icicle-current-input))
                (icicle-candidate-nb
                 (format " `%s'" (elt icicle-completion-candidates icicle-candidate-nb)))
                (t ""))
          (if current-prefix-arg
              (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
            ""))
  icicle-commands-for-abbrev nil t nil 'extended-command-history nil nil
  (;; Bindings
   (use-file-dialog                   nil) ; `mouse-2' in `*Completions*' shouldn't use file dialog.
   (alt-fn                            nil)
   (icicle-candidate-alt-action-fn    (or icicle-candidate-alt-action-fn
                                          (setq alt-fn  (icicle-alt-act-fn-for-type "command"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn  alt-fn  (icicle-alt-act-fn-for-type "command")))
   (icicle-add-proxy-candidates-flag  nil) ; No abbrevs - just commands here.
   (last-command                      last-command) ; Save and restore the last command.
   icicle-new-last-cmd)                 ; Set in `icicle-execute-extended-command-1'.
  nil nil                               ; First code, undo code
  (setq this-command  icicle-new-last-cmd) ; Last code: this will update `last-command'.
  'NON-INTERACTIVE)                     ; This is not a real command.

(defun icicle-command-abbrev-record (abbrev command)
  "Record ABBREV and COMMAND in `icicle-command-abbrev-alist'."
  (let ((entry  (assq command icicle-command-abbrev-alist)))
    (when (and abbrev  command)
      (if entry
          (setq icicle-command-abbrev-alist  (cons (list command abbrev (1+ (car (cddr entry))))
                                                   (delete entry icicle-command-abbrev-alist)))
        (push (list command abbrev 1) icicle-command-abbrev-alist)))))

;;;###autoload (autoload 'icicle-execute-named-keyboard-macro "icicles")
(icicle-define-command icicle-execute-named-keyboard-macro ; Bound to `C-x M-e' in Icicle mode.
  "Read the name of a keyboard macro, then execute it."
  icicle-execute-extended-command-1     ; Function to perform the action
  (format "Execute keyboard macro%s: "  ; `completing-read' args
          (if current-prefix-arg
              (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
            ""))
  obarray (and icompletep  pred) t nil 'icicle-kmacro-history nil nil
  ((last-command                            last-command) ; Save and restore the last command.
   (alt-fn                                  nil)
   (icicle-orig-must-pass-after-match-pred  icicle-must-pass-after-match-predicate)
   (pred                                    (lambda (fn)
                                              (unless (symbolp fn) (setq fn  (intern fn)))
                                              (and (commandp fn)  (arrayp (symbol-function fn)))))
   (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
   (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
   (icicle-candidate-alt-action-fn          (or icicle-candidate-alt-action-fn
                                                (setq alt-fn  (icicle-alt-act-fn-for-type "command"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn  alt-fn  (icicle-alt-act-fn-for-type "command")))))

;;;###autoload (when (locate-library "kmacro") (autoload 'icicle-kmacro "icicles"))
(when (locate-library "kmacro")
  (icicle-define-command icicle-kmacro  ; Bound to `S-f4' in Icicle mode (Emacs 22+).
    "Execute a keyboard macro according to its position in `kmacro-ring'.
Macros in the keyboard macro ring are given names `1', `2', and so on,
for use as completion candidates.

With prefix argument, repeat macro that many times (but see below).
If a macro is still being defined, end it first, then execute it.

Since this is a multi-command, you can execute any number of macros
any number of times in a single invocation.  In particular, you can
execute a given macro repeatedly using `C-RET' (be sure you use `TAB'
first, to make it the current candidate).

If you use a prefix arg for `icicle-kmacro', then each multi-command
action (e.g. `C-RET') repeats the macro that number of times.  However
if you use a prefix arg for an individual action, then the action
repeats the macro that number of times.  Without its own prefix arg,
an action uses the base prefix arg you used for `icicle-kmacro'."
    icicle-kmacro-action                ; Function to perform the action
    (format "Execute keyboard macro%s: " ; `completing-read' args
            (if current-prefix-arg
                (format " (prefix %s)" (prefix-numeric-value current-prefix-arg))
              ""))
    (lexical-let ((count  0))
      (setq icicle-kmacro-alist
            (mapcar (lambda (x) (cons (format "%d" (setq count  (1+ count))) x)) ; FREE here: COUNT.
                    (reverse (if nil kmacro-ring (cons (kmacro-ring-head) kmacro-ring))))))
    nil 'NO-EXIT-WO-MATCH nil 'icicle-kmacro-history
    (and (kmacro-ring-head)  (null kmacro-ring)  "1") nil
    ((icicle-pref-arg  current-prefix-arg))    ; Additional bindings
    (progn                              ; First code
      (unless (require 'kmacro nil t) (error "This command requires library `kmacro.el'"))
      (when defining-kbd-macro (kmacro-end-or-call-macro current-prefix-arg) (error "Done"))
      (unless (or (kmacro-ring-head)  kmacro-ring) (error "No keyboard macro defined"))))

  ;; Free vars here: `icicle-orig-buff' & `icicle-orig-window' are bound by `icicle-define-command'.
  ;;                 `icicle-pref-arg' is bound in `icicle-kmacro'.
  (defun icicle-kmacro-action (cand)
    "Action function for `icicle-kmacro'."
    (when (get-buffer icicle-orig-buff) (set-buffer icicle-orig-buff))
    (when (window-live-p icicle-orig-window) (select-window icicle-orig-window))
    (let* ((count  (if current-prefix-arg (prefix-numeric-value current-prefix-arg) icicle-pref-arg))
           (macro  (cadr (assoc cand icicle-kmacro-alist))))
      (unless macro (error "No such macro: `%s'" cand))
      (execute-kbd-macro macro count #'kmacro-loop-setup-function)
      (when (> count 1)
        (message "(%s times)" (icicle-propertize count 'face 'icicle-msg-emphasis))))))

;;;###autoload (autoload 'icicle-set-option-to-t "icicles")
(icicle-define-command icicle-set-option-to-t ; Command name
  "Set option to t.  This makes sense for binary (toggle) options.
By default, completion candidates are limited to user options that
have `boolean' custom types.  However, there are many \"binary\" options
that allow other non-nil values than t.

You can use a prefix argument to change the set of completion
candidates, as follows:

 - With a non-negative prefix arg, all user options are candidates.
 - With a negative prefix arg, all variables are candidates." ; Doc string
  (lambda (opt) (set (intern opt) t) (message "`%s' is now `t'" opt)) ; Action function
  "Set option to t: " obarray (and icompletep  pred) 'must-confirm nil ; `completing-read' args
  (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history) nil nil
  ((enable-recursive-minibuffers            t) ; Bindings
   (icicle-use-candidates-only-once-flag    t)
   (alt-fn                                  nil)
   (pred
    (cond ((and current-prefix-arg  (wholenump (prefix-numeric-value current-prefix-arg)))
           (lambda (x)
             (unless (symbolp x) (setq x  (intern x)))
             (and (boundp x)  (user-variable-p x)  (eq nil (symbol-value x)))))
          (current-prefix-arg
           (lambda (x)
             (unless (symbolp x) (setq x  (intern x)))
             (and (boundp x)  (eq nil (symbol-value x)))))
          (t
           (lambda (x)
             (unless (symbolp x) (setq x  (intern x)))
             (and (boundp x)  (icicle-binary-option-p x)  (eq nil (symbol-value x)))))))
   (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
   (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn  (setq alt-fn  (icicle-alt-act-fn-for-type "option"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn  alt-fn  (icicle-alt-act-fn-for-type "option")))))

;;;###autoload (autoload 'icicle-clear-history "icicles")
(icicle-define-command icicle-clear-history
  "Clear a minibuffer history of selected entries.
You are prompted for the history to clear, then you are prompted for
the entries to delete from it.  You can use multi-command completion
for both inputs.  That is, you can act on multiple histories and
delete multiple entries from each.

For convenience, you can use \\<minibuffer-local-completion-map>\
`\\[icicle-delete-candidate-object]' the same way as `C-RET': Each
of them deletes the current entry candidate from the history.

With a prefix argument, empty the chosen history completely
\(you are not prompted to choose history entries to delete).

`icicle-act-before-cycle-flag' is bound to t here during completion of
history entries, so `C-next' and so on act on the current candidate."
  icicle-clear-history-1                ; Function to perform the action
  "History to clear: " icicle-clear-history-hist-vars ; `completing-read' args
  nil t nil nil (symbol-name minibuffer-history-variable) nil
  ((icicle-pref-arg                 current-prefix-arg) ; Bindings
   (enable-recursive-minibuffers    t)
   (icicle-transform-function       'icicle-remove-duplicates)
   (icicle-clear-history-hist-vars  `((,(symbol-name minibuffer-history-variable))
                                      (,(symbol-name 'icicle-previous-raw-file-name-inputs))
                                      (,(symbol-name 'icicle-previous-raw-non-file-name-inputs))))
   (icicle-delete-candidate-object  'icicle-clear-history-entry))
  (mapatoms (lambda (x) (when (and (boundp x)  (consp (symbol-value x)) ; First code
                                   (stringp (car (symbol-value x)))
                                   (string-match "-\\(history\\|ring\\)\\'" (symbol-name x)))
                          (push (list (symbol-name x)) ; FREE here: ICICLE-CLEAR-HISTORY-HIST-VARS.
                                icicle-clear-history-hist-vars)))))

;; Free vars here: `icicle-pref-arg' is bound in `icicle-clear-history'.
(defun icicle-clear-history-1 (hist)
  "Action function for `icicle-clear-history' history-variable candidates."
  (setq hist  (intern hist))
  (if (not icicle-pref-arg)
      (let* ((icicle-candidate-action-fn              'icicle-clear-history-entry)
             (icicle-transform-function               'icicle-remove-duplicates)
             (icicle-clear-history-hist               hist)
             (icicle-use-candidates-only-once-flag    t)
             (icicle-show-Completions-initially-flag  t)
             (icicle-act-before-cycle-flag            t))
        (when hist                      ; Maybe there are no more, due to deletion actions.
          (funcall icicle-candidate-action-fn
                   (completing-read "Clear input: " (mapcar #'list (symbol-value hist)) nil t))))
    (set hist nil))
  (unless hist (message "History `%s' is now empty" hist))
  nil)

;; Free vars here: `icicle-clear-history-hist' is bound in `icicle-clear-history-1'
;; and in `icicle-clear-current-history'.
(defun icicle-clear-history-entry (cand)
  "Action function for history entry candidates in `icicle-clear-history'."
  (unless (string= "" cand)
    (set icicle-clear-history-hist
         (icicle-remove-if
          `(lambda (x)
            (string= (icicle-substring-no-properties ',cand) (icicle-substring-no-properties x)))
          (symbol-value icicle-clear-history-hist)))
    ;; We assume here that CAND was in fact present in the history originally.
    (message "`%s' deleted from history `%s'" cand icicle-clear-history-hist))
  nil)

;;;###autoload (autoload 'icicle-clear-current-history "icicles")
(icicle-define-command icicle-clear-current-history ; Bound to `M-i' in minibuffer.
  "Clear current minibuffer history of selected entries.
You are prompted for the history entries to delete.

With a prefix argument, however, empty the history completely
\(you are not prompted to choose history entries to delete).

`icicle-act-before-cycle-flag' is bound to t here during completion of
history entries, so `C-next' and so on act on the current candidate."
  icicle-clear-history-entry            ; Action function
  "Clear input: " (mapcar #'list (symbol-value icicle-clear-history-hist)) ; `completing-read' args
  nil t nil nil nil nil
  ((icicle-pref-arg                         current-prefix-arg) ; Bindings
   (enable-recursive-minibuffers            t)
   (icicle-transform-function               'icicle-remove-duplicates)
   (icicle-use-candidates-only-once-flag    t)
   (icicle-show-Completions-initially-flag  t)
   (icicle-clear-history-hist               minibuffer-history-variable))
  (when icicle-pref-arg                 ; First code
    (icicle-ding)                       ; Use `error' just to exit and make sure message is seen.
    (if (not (yes-or-no-p (format "Are you sure you want to empty `%s' completely? "
                                  minibuffer-history-variable)))
        (error "")
      (set minibuffer-history-variable nil)
      (error "History `%s' is now empty" minibuffer-history-variable))))

(when (and icicle-define-alias-commands-flag  (not (fboundp 'clear-option)))
  (defalias 'clear-option 'icicle-reset-option-to-nil))

;;;###autoload (autoload 'icicle-reset-option-to-nil "icicles")
(icicle-define-command icicle-reset-option-to-nil ; Command name
  "Set option to nil.  This makes sense for binary and list options.
By default, the set of completion candidates is limited to user
options.  Note: it is *not* limited to binary and list options.
With a prefix arg, all variables are candidates." ; Doc string
  (lambda (opt) (set (intern opt) nil) (message "`%s' is now `nil'" opt)) ; Action function
  "Clear option (set it to nil): " obarray (and icompletep  pred) t nil ; `completing-read' args
  (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history)
  nil nil
  ((enable-recursive-minibuffers            t) ; Bindings
   (icicle-use-candidates-only-once-flag    t)
   (alt-fn                                  nil)
   (pred                                    (if current-prefix-arg
                                                (lambda (x)
                                                  (unless (symbolp x) (setq x  (intern x)))
                                                  (and (boundp x)  (symbol-value x)))
                                              (lambda (x)
                                                (unless (symbolp x) (setq x  (intern x)))
                                                (and (boundp x)  (user-variable-p x)  (symbol-value x)))))
   (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
   (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn  (setq alt-fn  (icicle-alt-act-fn-for-type "option"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn  alt-fn  (icicle-alt-act-fn-for-type "option")))))

(when (and icicle-define-alias-commands-flag  (not (fboundp 'toggle)))
  (defalias 'toggle 'icicle-toggle-option))

;;;###autoload (autoload 'icicle-toggle-option "icicles")
(icicle-define-command icicle-toggle-option ; Command name
  "Toggle option's value.  This makes sense for binary (toggle) options.
By default, completion candidates are limited to user options that
have `boolean' custom types.  However, there are many \"binary\" options
that allow other non-nil values than t.

You can use a prefix argument to change the set of completion
candidates, as follows:

 - With a non-negative prefix arg, all user options are candidates.
 - With a negative prefix arg, all variables are candidates." ; Doc string
  (lambda (opt)                         ; Action function
    (let ((sym  (intern opt)))
      (set sym (not (eval sym)))
      (message "`%s' is now `%s'" opt (icicle-propertize (eval sym) 'face 'icicle-msg-emphasis))))
  "Toggle value of option: " obarray (and icompletep  pred) 'must-confirm nil ; `completing-read' args
  (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history) nil nil
  ((enable-recursive-minibuffers            t) ; Bindings
   (alt-fn                                  nil)
   (pred
    (cond ((and current-prefix-arg  (wholenump (prefix-numeric-value current-prefix-arg)))
           (lambda (c) (unless (symbolp c) (setq c  (intern c))) (user-variable-p c)))
          (current-prefix-arg (lambda (c) (unless (symbolp c) (setq c  (intern c))) (boundp c)))
          (t                  (lambda (c)
                                (unless (symbolp c) (setq c  (intern c))) (icicle-binary-option-p c)))))
   (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
   (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn  (setq alt-fn  (icicle-alt-act-fn-for-type "option"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn  alt-fn  (icicle-alt-act-fn-for-type "option")))))

(defun icicle-binary-option-p (symbol)
  "Non-nil if SYMBOL is a user option that has custom-type `boolean'."
  (eq (get symbol 'custom-type) 'boolean))

;;;###autoload (autoload 'icicle-increment-option "icicles")
(icicle-define-command icicle-increment-option ; Command name
  "Increment option's value using the arrow keys (`up', `down').
Completion candidates are limited to options that have `integer',
`float', and `number' custom types.
This command needs library `doremi.el'." ; Doc string
  (lambda (opt)                         ; Action function
    (let ((sym                                     (intern opt))
          ;; Restore this before we read number, since that might use completion.
          (icicle-must-pass-after-match-predicate  icicle-orig-must-pass-after-match-pred))
      (icicle-doremi-increment-variable+ sym (icicle-read-number "Increment (amount): ") t)
      (message "`%s' is now `%s'" opt (icicle-propertize (eval sym) 'face 'icicle-msg-emphasis))))
  "Increment value of option: " obarray (and icompletep  pred) 'must-confirm nil ; `completing-read' args
  (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history) nil nil
  ((enable-recursive-minibuffers            t) ; Bindings
   (alt-fn                                  nil)
   (icicle-orig-must-pass-after-match-pred  icicle-must-pass-after-match-predicate)
   (pred                                    (lambda (symb)
                                              (unless (symbolp symb) (setq symb  (intern-soft symb)))
                                              (memq (get symb 'custom-type) '(number integer float))))
   (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
   (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn  (setq alt-fn  (icicle-alt-act-fn-for-type "option"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn  alt-fn  (icicle-alt-act-fn-for-type "option"))))
  (unless (require 'doremi nil t) (error "This command needs library `doremi.el'."))) ; First code

;;;###autoload (autoload 'icicle-increment-variable "icicles")
(icicle-define-command icicle-increment-variable ; Command name
  "Increment variable's value using the arrow keys (`up', `down').
With a prefix arg, only numeric user options are candidates.
With no prefix arg, all variables are candidates, even those that are
 not numeric. 
This command needs library `doremi.el'." ; Doc string
  (lambda (var)                         ; FREE here: PREFIX-ARG.
    (let ((sym                                     (intern var))
          ;; Restore this before we read number, since that might use completion.
          (icicle-must-pass-after-match-predicate  icicle-orig-must-pass-after-match-pred))
      (icicle-doremi-increment-variable+ sym (icicle-read-number "Increment (amount): ") prefix-arg)
      (message "`%s' is now `%s'" var (icicle-propertize (eval sym) 'face 'icicle-msg-emphasis))))
  "Increment value of variable: " obarray (and icompletep  pred) 'must-confirm nil ; `completing-read' args
  (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history) nil nil
  ((enable-recursive-minibuffers            t) ; Bindings
   (prefix-arg                              current-prefix-arg)
   (alt-fn                                  nil)
   (icicle-orig-must-pass-after-match-pred  icicle-must-pass-after-match-predicate)
   (pred                                    (if prefix-arg
                                                (lambda (symb)
                                                  (unless (symbolp symb) (setq symb  (intern-soft symb)))
                                                  (memq (get symb 'custom-type) '(number integer float)))
                                              (lambda (symb)
                                                (unless (symbolp symb) (setq symb  (intern symb)))
                                                (boundp symb))))
   (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
   (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred))
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn
        (setq alt-fn  (icicle-alt-act-fn-for-type (if prefix-arg "option" "variable")))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn alt-fn
        (icicle-alt-act-fn-for-type (if prefix-arg "option" "variable")))))
  (unless (require 'doremi nil t) (error "This command needs library `doremi.el'."))) ; First code

;;;###autoload (autoload 'icicle-doremi-increment-variable+ "icicles")
(defun icicle-doremi-increment-variable+ (variable &optional increment optionp)
  "Increment VARIABLE by INCREMENT (default 1).
Use arrow key `up' or `down' or mouse wheel to increase or decrease.
You can use the `Meta' key (e.g. `M-up') to increment in larger steps.

Interactively, you can choose VARIABLE using completion.
With a prefix arg, only user options are available to choose from.
Raises an error if VARIABLE's value is not a number."
  (interactive
   (let* ((symb                                    (or (and (fboundp 'symbol-nearest-point)
                                                            (symbol-nearest-point))
                                                       (and (symbolp (variable-at-point))
                                                            (variable-at-point))))
          (enable-recursive-minibuffers            t)
          (icicle-orig-must-pass-after-match-pred  icicle-must-pass-after-match-predicate)
          (pred                                    (if current-prefix-arg
                                                       (lambda (s)
                                                         (unless (symbolp s) (setq s  (intern s)))
                                                         (user-variable-p s))
                                                     (lambda (s)
                                                       (unless (symbolp s) (setq s  (intern s)))
                                                       (boundp s))))
          (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
          (icicle-must-pass-after-match-predicate  (and (not icompletep)  pred)))
     (list (intern (completing-read "Increment variable: " obarray (and icompletep  pred) t nil nil
                                    (and symb  (symbol-name symb)) t))
           ;; Restore this before we read number, since that might use completion.
           (let ((icicle-must-pass-after-match-predicate  icicle-orig-must-pass-after-match-pred))
             (icicle-read-number "Increment (amount): "))
           current-prefix-arg)))
  (unless (require 'doremi nil t) (error "This command needs library `doremi.el'."))
  (unless increment (setq increment 1))
  (unless (numberp (symbol-value variable))
    (error "Variable's value is not a number: %S" (symbol-value variable)))
  (doremi (lambda (new-val)             ; FREE here: VARIABLE.
            (set variable  new-val)
            new-val)
          (symbol-value variable)
          increment))

;;;###autoload (autoload 'icicle-bookmark-cmd "icicles")
(defun icicle-bookmark-cmd (&optional parg) ; Bound to what `bookmark-set' is bound to (`C-x r m').
  "Set bookmark or visit bookmark(s).
With a negative prefix arg, visit bookmark(s), using
  `icicle-bookmark-other-window' (see that command for more info).

Otherwise, set a bookmark, as follows:

* No prefix arg: Prompt for the bookmark name.

  If feature `bookmark+' is present:

  . You can use (lax) completion for the bookmark name.
    The candidates are bookmarks in the current buffer (or all
    bookmarks if there are none in this buffer).

  . You can narrow the current completion candidates to bookmarks of a
    given type.  The keys for this are the same as for
    `icicle-bookmark' and for the bookmark-jumping keys at the top
    level.

  . See the `Bookmark+' doc string of redefined command `bookmark-set'
    for info about the available default values for the bookmark name.

  If feature `bookmark+' is not present, then completion is not
  available, and the default bookmark name is the last one used in
  the current buffer.

  Note: You can use command `icicle-bookmark-set' with a numeric
  prefix arg if you want to complete against all bookmark names,
  instead of those for the current buffer.

* Plain prefix arg (`C-u'): Same as no prefix arg, but do not
  overwrite any existing bookmark that has the same name.

* Non-negative numeric prefix arg: Do not prompt for bookmark name.
  If feature `bookmark+' is present and the region is active and
    nonempty, then use the buffer name followed by a prefix of the
    region text as the bookmark name.
  Otherwise, use the buffer name followed by the text of the current
    line, starting at point.
  Use at most `icicle-bookmark-name-length-max' chars, in either case.
  If the prefix arg is 0, then do not overwrite any existing bookmark
    that has the same name.

By default, Icicle mode remaps all key sequences that are normally
bound to `bookmark-set' to `icicle-bookmark-cmd'.  If you do not want
this remapping, then customize option `icicle-top-level-key-bindings'.
In particular, you might prefer to remap `bookmark-set' to
`icicle-bookmark-set' (see Note, above)."
  (interactive "P")
  (if (and parg  (< (prefix-numeric-value parg) 0))
      (icicle-bookmark-other-window)
    (if (or (not parg)  (consp parg))
        (icicle-bookmark-set nil parg 'PSEUDO-INTERACTIVEP)
      (let* ((regionp    (and (featurep 'bookmark+)  transient-mark-mode  mark-active
                              (not (eq (region-beginning) (region-end)))))
             (name-beg   (if regionp (region-beginning) (point)))
             (name-end   (if regionp (region-end) (line-end-position)))
             (def-name   (concat (buffer-name) ": " (buffer-substring name-beg name-end)))
             (trim-name  (replace-regexp-in-string
                          "\n" " " (substring def-name 0 (min icicle-bookmark-name-length-max
                                                              (length def-name))))))
        (message "Setting bookmark `%s'" trim-name) (sit-for 2)
        (bookmark-set trim-name (and parg  (or (consp parg)  (zerop (prefix-numeric-value parg)))))))))

;;;###autoload (autoload 'icicle-bookmark-set "icicles")
(defun icicle-bookmark-set (&optional name parg interactivep) ; `C-x r m'
  "With `Bookmark+', this is `bookmark-set' with Icicles multi-completions.
In particular, you can use (lax) completion for the bookmark name.
Without `Bookmark+', this is the same as vanilla Emacs `bookmark-set'.

With `Bookmark+':

 . You can use (lax) completion for the bookmark name.
   The candidates are bookmarks in the current buffer (or all
   bookmarks if there are none in this buffer).

 . You can narrow the current completion candidates to bookmarks of a
   given type.  The keys for this are the same as for
   `icicle-bookmark' and for the bookmark-jumping keys at the top
   level.

 . See the `Bookmark+' doc string of redefined command `bookmark-set'
   for info about the available default values for the bookmark name.

If feature `bookmark+' is not present, then completion is not
available, and the default bookmark name is the last one used in
the current buffer.

Note: You can use command `icicle-bookmark-set' with a numeric
prefix arg if you want to complete against all bookmark names,
instead of those for the current buffer."
  (interactive (list nil current-prefix-arg t))
  (if (not (featurep 'bookmark+))
      (bookmark-set name parg)
    (unwind-protect
         (let ((enable-recursive-minibuffers           t) ; In case read input, e.g. File changed...
               (completion-ignore-case                 bookmark-completion-ignore-case)
               (prompt                                 "Bookmark: ")
               (icicle-multi-completing-p              icicle-show-multi-completion-flag)
               (icicle-list-use-nth-parts              '(1))
               (icicle-candidate-properties-alist      (if (not icicle-show-multi-completion-flag)
                                                           ()
                                                         (if (facep 'file-name-shadow)
                                                             '((2 (face file-name-shadow))
                                                               (3 (face bookmark-menu-heading)))
                                                           '((3 (face bookmark-menu-heading))))))
               (icicle-transform-function              (if (interactive-p) nil icicle-transform-function))
               (icicle-whole-candidate-as-text-prop-p  t)
               (icicle-transform-before-sort-p         t)
               (icicle-candidates-alist
                (if (not (featurep 'bookmark+))
                    (mapcar (lambda (cand)
                              (list (icicle-candidate-short-help
                                     (icicle-bookmark-help-string cand)
                                     (icicle-bookmark-propertize-candidate cand))))
                            (bookmark-all-names)) ; Loads bookmarks file, defining `bookmark-alist'.
                  (bookmark-maybe-load-default-file) ; Loads bookmarks file, defining `bookmark-alist'.
                  (mapcar #'icicle-make-bookmark-candidate
                          (bmkp-sort-omit
                           (and (or (not parg)  (consp parg)) ; No numeric PARG: all bookmarks.
                                (or (bmkp-specific-buffers-alist-only)  bookmark-alist))))))
               (icicle-sort-orders-alist
                (append '(("in *Bookmark List* order") ; Renamed from "turned OFF'.
                          ("by bookmark name" . icicle-alpha-p))
                        (and (featurep 'bookmark+)
                             '(("by last bookmark access" (bmkp-bookmark-last-access-cp)
                                icicle-alpha-p)
                               ("by bookmark visit frequency" (bmkp-visited-more-cp)
                                icicle-alpha-p)
                               ("by last buffer or file access"
                                (bmkp-buffer-last-access-cp
                                 bmkp-local-file-accessed-more-recently-cp)
                                icicle-alpha-p)
                               ("marked before unmarked (in *Bookmark List*)" (bmkp-marked-cp)
                                icicle-alpha-p)
                               ("by local file type" (bmkp-local-file-type-cp) icicle-alpha-p)
                               ("by file name" (bmkp-file-alpha-cp) icicle-alpha-p)
                               ("by local file size" (bmkp-local-file-size-cp) icicle-alpha-p)
                               ("by last local file access"
                                (bmkp-local-file-accessed-more-recently-cp)
                                icicle-alpha-p)
                               ("by last local file update"
                                (bmkp-local-file-updated-more-recently-cp)
                                icicle-alpha-p)
                               ("by Info location" (bmkp-info-cp) icicle-alpha-p)
                               ("by Gnus thread" (bmkp-gnus-cp) icicle-alpha-p)
                               ("by URL" (bmkp-url-cp) icicle-alpha-p)
                               ("by bookmark type"
                                (bmkp-info-cp bmkp-url-cp bmkp-gnus-cp
                                 bmkp-local-file-type-cp bmkp-handler-cp)
                                icicle-alpha-p)))
                        '(("by previous use alphabetically" . icicle-historical-alphabetic-p)
                          ("case insensitive" . icicle-case-insensitive-string-less-p))))
               (icicle-candidate-help-fn
                ;; FREE here: CURRENT-PREFIX-ARG, ICICLE-GET-ALIST-CANDIDATE-FUNCTION,
                ;;            ICICLE-SHOW-MULTI-COMPLETION-FLAG.
                (lambda (cand)
                  (when (and (featurep 'bookmark+)  icicle-show-multi-completion-flag)
                    (setq cand  (funcall icicle-get-alist-candidate-function cand))
                    (setq cand  (cons (caar cand) (cdr cand))))
                  (if (featurep 'bookmark+)
                      (if current-prefix-arg
                          (bmkp-describe-bookmark-internals cand)
                        (bmkp-describe-bookmark cand))
                    (icicle-msg-maybe-in-minibuffer (icicle-bookmark-help-string cand))))))
           (require 'bookmark)
           (when (featurep 'bookmark+)
             ;; Bind keys to narrow bookmark candidates by type.  Lax is for multi-completion case.
             (dolist (map  '(minibuffer-local-must-match-map minibuffer-local-completion-map))
               (icicle-bookmark-bind-narrow-commands map)))
           (setq bookmark-current-point   (point)
                 bookmark-current-buffer  (current-buffer))
           (save-excursion (skip-chars-forward " ") (setq bookmark-yank-point  (point)))
           (let* ((record   (bookmark-make-record))
                  (defname  (or (and (stringp (car record))  (car record))
                                (cond ((eq major-mode 'w3m-mode) w3m-current-title)
                                      ((eq major-mode 'gnus-summary-mode)
                                       (elt (gnus-summary-article-header) 1))
                                      ((memq major-mode '(Man-mode woman-mode))
                                       (buffer-substring (point-min) (save-excursion
                                                                       (goto-char (point-min))
                                                                       (skip-syntax-forward "^ ")
                                                                       (point))))
                                      (t nil))))
                  (defname  (and defname  (bmkp-replace-regexp-in-string "\n" " " defname)))
                  (bname    (or name  (icicle-transform-multi-completion
                                       (bmkp-completing-read-lax "Set bookmark "
                                                                 (bmkp-new-bookmark-default-names defname)
                                                                 icicle-candidates-alist
                                                                 nil (if (boundp 'bookmark-history)
                                                                         'bookmark-history
                                                                       'icicle-bookmark-history))))))
             (when (string-equal bname "") (setq bname  defname))
             (bookmark-store bname (cdr record) (consp parg))
             (when (and interactivep  bmkp-prompt-for-tags-flag)
               (bmkp-add-tags bname (bmkp-read-tags-completing))) ; Don't bother to refresh tags. (?)
             (case (and (boundp 'bmkp-auto-light-when-set)  bmkp-auto-light-when-set)
               (autonamed-bookmark       (when (bmkp-autonamed-bookmark-p bname)
                                           (bmkp-light-bookmark bname)))
               (non-autonamed-bookmark   (unless (bmkp-autonamed-bookmark-p bname)
                                           (bmkp-light-bookmark bname)))
               (any-bookmark             (bmkp-light-bookmark bname))
               (autonamed-in-buffer      (bmkp-light-bookmarks
                                          (bmkp-remove-if-not
                                           #'bmkp-autonamed-bookmark-p
                                           (bmkp-this-buffer-alist-only)) nil interactivep))
               (non-autonamed-in-buffer  (bmkp-light-bookmarks
                                          (bmkp-remove-if
                                           #'bmkp-autonamed-bookmark-p
                                           (bmkp-this-buffer-alist-only)) nil interactivep))
               (all-in-buffer            (bmkp-light-this-buffer nil interactivep)))
             ;; Maybe make bookmark temporary.
             (if bmkp-autotemp-all-when-set-p
                 (bmkp-make-bookmark-temporary bname)
               (catch 'icicle-bookmark-set
                 (dolist (pred  bmkp-autotemp-bookmark-predicates)
                   (when (and (functionp pred)  (funcall pred bname))
                     (bmkp-make-bookmark-temporary bname)
                     (throw 'icicle-bookmark-set t)))))
             (run-hooks 'bmkp-after-set-hook)
             (if bookmark-use-annotations
                 (bookmark-edit-annotation bname)
               (goto-char bookmark-current-point)))
           (setq bookmark-yank-point      nil
                 bookmark-current-buffer  nil))
      (icicle-bookmark-cleanup))))

(defun icicle-make-bookmark-candidate (bookmark)
  "Return a propertized full bookmark candidate for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
The return value is of the form (DISPLAY . RECORD), where:

 DISPLAY is the display candidate, propertized with
  `icicle-bookmark-help-string' and `icicle-bookmark-propertize-candidate'.

 RECORD is the full BOOKMARK record.

If option `icicle-show-multi-completion-flag' is non-nil then DISPLAY
is a multi-completion with three parts:

 bookmark name
 target file for the bookmark
 tags in the bookmark

If the option value is nil then DISPLAY is just the bookmark name."
  (setq bookmark  (bookmark-get-bookmark bookmark))
  (icicle-condition-case-no-debug nil   ; Ignore errors, e.g. from bad or stale bookmark records.
      (if icicle-show-multi-completion-flag
          (let* ((bname     (bookmark-name-from-full-record bookmark))
                 (guts      (bookmark-get-bookmark-record bookmark))
                 (file      (bookmark-get-filename bookmark))
                 (buf       (bmkp-get-buffer-name bookmark))
                 (file/buf  (if (and buf  (equal file bmkp-non-file-filename))
                                buf
                              file))
                 (tags      (bmkp-get-tags bookmark)))
            (cons `(,(icicle-candidate-short-help
                      (icicle-bookmark-help-string bname)
                      (icicle-bookmark-propertize-candidate bname))
                    ,file/buf
                    ,@(and tags  (list (format "%S" tags))))
                  guts))
        (let ((bname  (bookmark-name-from-full-record bookmark))
              (guts   (bookmark-get-bookmark-record bookmark)))
          (cons (icicle-candidate-short-help
                 (icicle-bookmark-help-string bname)
                 (icicle-bookmark-propertize-candidate bname))
                guts)))
    (error nil)))

;;;###autoload (autoload 'icicle-bookmark "icicles")
(icicle-define-command icicle-bookmark  ; Bound to `C-x j j', `C-x p b', `C-x r b'.
  "Jump to a bookmark.
With a plain prefix argument (`C-u'), reverse the effect of option
`icicle-bookmark-refresh-cache-flag'.  See the doc for that option.
In particular, if the option value is nil and you try to jump to a
bookmark that is not up to date or does not exist, then try using a
prefix arg to refresh the cache.

During completion, you can use \\<minibuffer-local-completion-map>\
`\\[icicle-delete-candidate-object]' on a bookmark to delete it.

If you also use library `Bookmark+', then:

 * `C-M-return' shows detailed info about the current bookmark candidate.
   `C-u C-M-return' shows the complete, internal info for the bookmark.
   Likewise, for the other candidate help keys: `C-M-down' etc.
   (And the mode line always shows summary info about the bookmark.)
   
 * You can use `C-,' to sort bookmarks in many different ways,
   according to their properties.

 * In `*Completions*', the candidate bookmarks are highlighted
   according to their type.  You can customize the highlighting faces:

  `bmkp-bad-bookmark'              - possibly bad bookmark
  `bmkp-bookmark-list'             - bookmark list
  `bmkp-buffer'                    - buffer
  `bmkp-desktop'                   - desktop
  `bmkp-function'                  - function bookmark
  `bmkp-gnus'                      - Gnus article
  `bmkp-info'                      - Info node
  `bmkp-local-directory'           - local directory
  `bmkp-local-file-with-region'    - local file with a region
  `bmkp-local-file-without-region' - local file without a region
  `bmkp-man'                       - `man' page
  `bmkp-non-file'                  - non-file (no current buffer)
  `bmkp-remote-file'               - remote-file
  `bmkp-sequence'                  - sequence bookmark
  `bmkp-url'                       - URL

 * In `*Completions*', if option `icicle-show-multi-completion-flag'
   is non-nil, then each completion candidate is a multi-completion:

    a. the bookmark name
    b. the bookmark file or buffer name
    c. any tags

   You can match any parts of the multi-completion.  You can toggle
   the option (for the next command) using `M-m' during completion.
   For example, you can match all bookmarks that have tags by typing:

     C-M-j . * C-M-j S-TAB

   (Each `C-M-j' inserts `^G\n', which is `icicle-list-join-string'.)

 * You can narrow the current completion candidates to bookmarks of a
   given type.  The keys for this are the same as the bookmark-jumping
   keys at the top level.

   `C-x j a'   - autofile bookmarks
   `C-x j b'   - non-file (buffer) bookmarks
   `C-x j B'   - bookmark-list bookmarks
   `C-x j d'   - Dired bookmarks
   `C-x j f'   - file bookmarks
   `C-x j . f' - bookmarks to files in the current directory
   `C-x j g'   - Gnus bookmarks
   `C-x j i'   - Info bookmarks
   `C-x j M-i' - image bookmarks
   `C-x j K'   - desktop bookmarks
   `C-x j l'   - local-file bookmarks
   `C-x j m'   - `man' pages
   `C-x j n'   - remote-file bookmarks
   `C-x j r'   - bookmarks with regions
   `C-x j u'   - URL bookmarks
   `C-x j w'   - W3M (URL) bookmarks
   `C-x j x'   - temporary bookmarks
   `C-x j y'   - bookmark-file bookmarks
   `C-x j #'   - autonamed bookmarks
   `C-x j , #' - autonamed bookmarks for the current buffer
   `C-x j , ,' - bookmarks for the current buffer
   `C-x j = b' - bookmarks for specific buffers
   `C-x j = f' - bookmarks for specific files

   See also the individual multi-commands for different bookmark
   types: `icicle-bookmark-info-other-window' etc.

If you also use library `crosshairs.el', then the visited bookmark
position is highlighted."               ; Doc string
  (lambda (cand) (icicle-bookmark-jump (icicle-transform-multi-completion cand))) ; Action
  prompt icicle-candidates-alist nil (not icicle-show-multi-completion-flag) ; `completing-read' args
  nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
  (and (boundp 'bookmark-current-bookmark)  bookmark-current-bookmark) nil
  ((enable-recursive-minibuffers           t) ; In case we read input, e.g. File changed on disk...
   (completion-ignore-case                 bookmark-completion-ignore-case)
   (prompt                                 "Bookmark: ")
   (icicle-multi-completing-p              icicle-show-multi-completion-flag)
   (icicle-list-use-nth-parts              '(1))
   (icicle-candidate-properties-alist      (if (not icicle-show-multi-completion-flag)
                                               ()
                                             (if (facep 'file-name-shadow)
                                                 '((2 (face file-name-shadow))
                                                   (3 (face bookmark-menu-heading)))
                                               '((3 (face bookmark-menu-heading))))))
   (icicle-transform-function              (if (interactive-p) nil icicle-transform-function))
   (icicle-whole-candidate-as-text-prop-p  t)
   (icicle-transform-before-sort-p         t)
   (icicle-delete-candidate-object         'icicle-bookmark-delete-action)
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
    ;; FREE here: CURRENT-PREFIX-ARG, ICICLE-GET-ALIST-CANDIDATE-FUNCTION,
    ;;            ICICLE-SHOW-MULTI-COMPLETION-FLAG.
    (lambda (cand)
      (when (and (featurep 'bookmark+)  icicle-show-multi-completion-flag)
        (setq cand  (funcall icicle-get-alist-candidate-function cand))
        (setq cand  (cons (caar cand) (cdr cand))))
      (if (featurep 'bookmark+)
          (if current-prefix-arg
              (bmkp-describe-bookmark-internals cand)
            (bmkp-describe-bookmark cand))
        (icicle-msg-maybe-in-minibuffer (icicle-bookmark-help-string cand)))))
   (icicle-candidates-alist
    (if (not (featurep 'bookmark+))
        (mapcar (lambda (cand)
                  (list (icicle-candidate-short-help (icicle-bookmark-help-string cand)
                                                     (icicle-bookmark-propertize-candidate cand))))
                (bookmark-all-names))   ; Loads bookmarks file, defining `bookmark-alist'.
      (bookmark-maybe-load-default-file) ; Loads bookmarks file, defining `bookmark-alist'.
      (mapcar #'icicle-make-bookmark-candidate
              (or (and (or (and (not icicle-bookmark-refresh-cache-flag)
                                (not (consp current-prefix-arg)))
                           (and icicle-bookmark-refresh-cache-flag (consp current-prefix-arg)))
                       bmkp-sorted-alist)
                  (setq bmkp-sorted-alist  (bmkp-sort-omit bookmark-alist)))))))
  (progn                                ; First code
    (require 'bookmark)
    (when (featurep 'bookmark+)
      ;; Bind keys to narrow bookmark candidates by type.  Lax is for multi-completion case.
      (dolist (map  '(minibuffer-local-must-match-map minibuffer-local-completion-map))
        (icicle-bookmark-bind-narrow-commands map))))
  (icicle-bookmark-cleanup-on-quit)     ; Undo code
  (icicle-bookmark-cleanup))            ; Last code

;;;###autoload (autoload 'icicle-bookmark-other-window "icicles")
(icicle-define-command icicle-bookmark-other-window
                                        ; Bound to `C-x 4 j j', `C-x p j', `C-x p o', `C-x p q'.
  "Jump to a bookmark in another window.
Same as `icicle-bookmark', but uses another window." ; Doc string
  (lambda (cand) (icicle-bookmark-jump-other-window (icicle-transform-multi-completion cand)))
  prompt icicle-candidates-alist nil (not icicle-show-multi-completion-flag) ; `completing-read' args
  nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
  (and (boundp 'bookmark-current-bookmark)  bookmark-current-bookmark) nil
  ((enable-recursive-minibuffers           t) ; In case we read input, e.g. File changed on disk...
   (completion-ignore-case                 bookmark-completion-ignore-case)
   (prompt                                 "Bookmark: ")
   (icicle-multi-completing-p              icicle-show-multi-completion-flag)
   (icicle-list-use-nth-parts              '(1))
   (icicle-candidate-properties-alist      (if (not icicle-show-multi-completion-flag)
                                               ()
                                             (if (facep 'file-name-shadow)
                                                 '((2 (face file-name-shadow))
                                                   (3 (face bookmark-menu-heading)))
                                               '((3 (face bookmark-menu-heading))))))
   (icicle-transform-function              (if (interactive-p) nil icicle-transform-function))
   (icicle-whole-candidate-as-text-prop-p  t)
   (icicle-transform-before-sort-p         t)
   (icicle-delete-candidate-object         'icicle-bookmark-delete-action)
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
    ;; FREE here: CURRENT-PREFIX-ARG, ICICLE-GET-ALIST-CANDIDATE-FUNCTION,
    ;;            ICICLE-SHOW-MULTI-COMPLETION-FLAG.
    (lambda (cand)
      (when (and (featurep 'bookmark+)  icicle-show-multi-completion-flag)
        (setq cand  (funcall icicle-get-alist-candidate-function cand))
        (setq cand  (cons (caar cand) (cdr cand))))
      (if (featurep 'bookmark+)
          (if current-prefix-arg
              (bmkp-describe-bookmark-internals cand)
            (bmkp-describe-bookmark cand))
        (icicle-msg-maybe-in-minibuffer (icicle-bookmark-help-string cand)))))
   (icicle-candidates-alist
    (if (not (featurep 'bookmark+))
        (mapcar (lambda (cand)
                  (list (icicle-candidate-short-help (icicle-bookmark-help-string cand)
                                                     (icicle-bookmark-propertize-candidate cand))))
                (bookmark-all-names))   ; Loads bookmarks file, defining `bookmark-alist'.
      (bookmark-maybe-load-default-file) ; Loads bookmarks file, defining `bookmark-alist'.
      (mapcar #'icicle-make-bookmark-candidate
              (or (and (or (and (not icicle-bookmark-refresh-cache-flag)
                                (not (consp current-prefix-arg)))
                           (and icicle-bookmark-refresh-cache-flag  (consp current-prefix-arg)))
                       bmkp-sorted-alist)
                  (setq bmkp-sorted-alist  (bmkp-sort-omit bookmark-alist)))))))
  (progn                                ; First code
    (require 'bookmark)
    (when (featurep 'bookmark+)
      ;; Bind keys to narrow bookmark candidates by type.  Lax is for multi-completion case.
      (dolist (map  '(minibuffer-local-must-match-map minibuffer-local-completion-map))
        (icicle-bookmark-bind-narrow-commands map))))
  (icicle-bookmark-cleanup-on-quit)     ; Undo code
  (icicle-bookmark-cleanup))            ; Last code

(defun icicle-bookmark-bind-narrow-commands (map)
  "Bind keys to narrow bookmark candidates by type."
  (when (featurep 'bookmark+)
    ;; Lax completion is for multi-completion case.
    (dolist (map  '(minibuffer-local-must-match-map  minibuffer-local-completion-map))
      (define-key (symbol-value map) (icicle-kbd "C-x j #") ; `C-x j #'
        'icicle-bookmark-autonamed-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j , #") ; `C-x j , #'
        'icicle-bookmark-autonamed-this-buffer-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j a") ; `C-x j a'
        'icicle-bookmark-autofile-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j b") ; `C-x j b'
        'icicle-bookmark-non-file-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j B") ; `C-x j B'
        'icicle-bookmark-bookmark-list-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j d") ; `C-x j d'
        'icicle-bookmark-dired-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j f") ; `C-x j f'
        'icicle-bookmark-file-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j . f") ; `C-x j . f'
        'icicle-bookmark-file-this-dir-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j g") ; `C-x j g'
        'icicle-bookmark-gnus-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j i") ; `C-x j i'
        'icicle-bookmark-info-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j M-i") ; `C-x j M-i'
        'icicle-bookmark-image-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j K") ; `C-x j K'
        'icicle-bookmark-desktop-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j l") ; `C-x j l'
        'icicle-bookmark-local-file-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j m") ; `C-x j m'
        'icicle-bookmark-man-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j n") ; `C-x j n'
        'icicle-bookmark-remote-file-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j r") ; `C-x j r'
        'icicle-bookmark-region-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j u") ; `C-x j u'
        'icicle-bookmark-url-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j w") ; `C-x j w'
        'icicle-bookmark-w3m-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j x") ; `C-x j x'
        'icicle-bookmark-temporary-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j y") ; `C-x j y'
        'icicle-bookmark-bookmark-file-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j , ,") ; `C-x j , ,'
        'icicle-bookmark-this-buffer-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j = b") ; `C-x j = b'
        'icicle-bookmark-specific-buffers-narrow)
      (define-key (symbol-value map) (icicle-kbd "C-x j = f") ; `C-x j = f'
        'icicle-bookmark-specific-files-narrow))))

(defun icicle-bookmark-delete-action (cand)
  "Delete bookmark candidate CAND, then update `bmkp-sorted-alist'."
  (bookmark-delete (icicle-transform-multi-completion cand))
  (when (or (and (not icicle-bookmark-refresh-cache-flag)
                 (not (consp current-prefix-arg)))
            (and icicle-bookmark-refresh-cache-flag  (consp current-prefix-arg)))
    (setq bmkp-sorted-alist  (bmkp-sort-omit bookmark-alist))))

(defun icicle-bookmark-propertize-candidate (cand)
  "Return bookmark name CAND, with a face indicating its type."
  (when (featurep 'bookmark+)
    (put-text-property
     0 (length cand) 'face
     (cond ((bmkp-sequence-bookmark-p cand)        'bmkp-sequence)
           ((bmkp-function-bookmark-p cand)        'bmkp-function)
           ((bmkp-bookmark-list-bookmark-p cand)   'bmkp-bookmark-list)
           ((bmkp-desktop-bookmark-p cand)         'bmkp-desktop)
           ((bmkp-info-bookmark-p cand)            'bmkp-info)
           ((bmkp-man-bookmark-p cand)             'bmkp-man)
           ((bmkp-gnus-bookmark-p cand)            'bmkp-gnus)
           ((bmkp-url-bookmark-p cand)             'bmkp-url)
           ((bmkp-remote-file-bookmark-p cand)     'bmkp-remote-file)
           ((and (bmkp-file-bookmark-p cand)
                 (file-directory-p
                  (bookmark-get-filename cand)))   'bmkp-local-directory)
           ((and (bmkp-local-file-bookmark-p cand)
                 (bmkp-region-bookmark-p cand))    'bmkp-local-file-with-region)
           ((bmkp-local-file-bookmark-p cand)      'bmkp-local-file-without-region)
           ((and (bmkp-get-buffer-name cand)
                 (get-buffer (bmkp-get-buffer-name cand))
                 (equal (bookmark-get-filename cand)
                        bmkp-non-file-filename))   'bmkp-buffer)
           ((not (bmkp-file-bookmark-p cand))      'bmkp-non-file)
           (t                                           'bmkp-bad-bookmark))
     cand))
  cand)

;;;###autoload (autoload 'icicle-bookmark-jump "icicles")
(defun icicle-bookmark-jump (bookmark)
  "Jump to BOOKMARK.
If `crosshairs.el' is loaded, then highlight the target position.
You probably don't want to use this.  Use `icicle-bookmark' instead."
  (interactive (list (bookmark-completing-read "Jump to bookmark" bookmark-current-bookmark)))
  (icicle-bookmark-jump-1 bookmark))

;;;###autoload (autoload 'icicle-bookmark-jump-other-window "icicles")
(defun icicle-bookmark-jump-other-window (bookmark)
  "Jump to BOOKMARK in another window.
If `crosshairs.el' is loaded, then highlight the target position.
You probably don't want to use this.  Use
`icicle-bookmark-other-window' instead."
  (interactive (list (bookmark-completing-read "Jump to bookmark (other window)"
                                               bookmark-current-bookmark)))
  (icicle-bookmark-jump-1 bookmark 'other-window))

(defun icicle-bookmark-jump-1 (bookmark &optional other-window-p)
  "Helper function for `icicle-bookmark-jump(-other-window)'."
  (unless bookmark (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark)
  ;; In case the jump renames it (as for an autonamed bookmark).
  (setq bookmark  (bookmark-get-bookmark bookmark 'NOERROR))
  (if (fboundp 'bookmark--jump-via)
      (bookmark--jump-via bookmark (if other-window-p 'pop-to-buffer 'switch-to-buffer))
    (let ((cell  (bookmark-jump-noselect bookmark))) ; Emacs < 23 and without `Bookmark+'.
      (when cell
        (if other-window-p
            (pop-to-buffer (car cell) 'other-window)
          (switch-to-buffer (car cell)))
        (goto-char (cdr cell))
        (unless (pos-visible-in-window-p) (recenter icicle-recenter))
        (progn (run-hooks 'bookmark-after-jump-hook) t)
        ;; If there is an annotation for this bookmark, show it in a buffer.
        (when bookmark-automatically-show-annotations (bookmark-show-annotation bookmark)))))
  ;; Unless `bmkp-use-region' and bookmark has a region, show position using crosshairs.
  (unless (and (boundp 'bmkp-use-region)  bmkp-use-region
               (fboundp 'bmkp-get-end-position) (bmkp-get-end-position bookmark)
               (/= (bookmark-get-position bookmark) (bmkp-get-end-position bookmark)))
    (when (fboundp 'crosshairs-highlight) (crosshairs-highlight))))
;; $$$$$$   (select-window (minibuffer-window))
;; $$$$$$   (select-frame-set-input-focus (selected-frame)))

(defun icicle-bookmark-help-string (bookmark-name)
  "Return a help string for BOOKMARK-NAME." ; `bmkp-*' functions are defined in `Bookmark+'.
  ;; Use BOOKMARK-NAME, not full bookmark BMK, as arg to vanilla bookmark functions, for Emacs < 23.
  (let* ((bmk            (bookmark-get-bookmark bookmark-name))
         (buf            (and (fboundp 'bmkp-get-buffer-name)  (bmkp-get-buffer-name bmk)))
         (file           (bookmark-get-filename bookmark-name))
         (start          (bookmark-get-position bookmark-name))
         (no-position-p  (not start))
         (end            (and (fboundp 'bmkp-get-end-position)  (bmkp-get-end-position bmk)))
         (annot          (bookmark-get-annotation bookmark-name))
         (sequence-p     (and (fboundp 'bmkp-sequence-bookmark-p)
                              (bmkp-sequence-bookmark-p bmk)))
         (function-p     (and (fboundp 'bmkp-function-bookmark-p)
                              (bmkp-function-bookmark-p bmk)))
         (blist-p        (and (fboundp 'bmkp-bookmark-list-bookmark-p)
                              (bmkp-bookmark-list-bookmark-p bmk)))
         (desktop-p      (and (fboundp 'bmkp-desktop-bookmark-p)
                              (bmkp-desktop-bookmark-p bmk)))
         (dired-p        (and (fboundp 'bmkp-dired-bookmark-p)  (bmkp-dired-bookmark-p bmk)))
         (gnus-p         (and (fboundp 'bmkp-gnus-bookmark-p)  (bmkp-gnus-bookmark-p bmk)))
         (info-p         (and (fboundp 'bmkp-info-bookmark-p)  (bmkp-info-bookmark-p bmk)))
         (man-p          (and (fboundp 'bmkp-man-bookmark-p)  (bmkp-man-bookmark-p bmk)))
         (url-p          (and (fboundp 'bmkp-url-bookmark-p)  (bmkp-url-bookmark-p bmk)))
         type-info-p)
    (when (or sequence-p  function-p) (setq no-position-p  t))
    (concat (setq type-info-p
                  (cond (sequence-p (format "Sequence: %S" (bookmark-prop-get bmk 'sequence)))
                        (function-p (let ((fn  (bookmark-prop-get bmk 'function)))
                                      (if (symbolp fn) (format "Function: `%s'" fn) "Function")))
                        (desktop-p  "Desktop, ")
                        (dired-p    (format "Dired %s, " file))
                        (gnus-p     "Gnus, ")
                        (info-p     "Info, ")
                        (man-p      (let ((man-args  (bookmark-prop-get bmk 'man-args)))
                                      (if man-args
                                          (format "`man %s', " man-args)
                                        ;; WoMan has no variable for the cmd name.
                                        (format "%s, " (bookmark-prop-get bmk 'buffer-name)))))
                        (url-p      "URL, ")
                        (t nil)))
            (and (not dired-p)
                 (or (and file  (or (not (boundp 'bmkp-non-file-filename))
                                    (not (equal file bmkp-non-file-filename)))
                          (format (if type-info-p "file `%s', " "File `%s', ") file))
                     (and buf  (format (if type-info-p "buffer `%s', " "Buffer `%s', ") buf))))
            (and (not no-position-p)
                 (if (and end  (> (- end start) 0))
                     (format "from %d to %d (%d chars)" start end (- end start))
                   (format "position %d" start)))
            (and annot  (format ", %s" annot)))))

;;; MUST keep this synchronized with any general Icicle-mode `C-M-' bindings in `icicles-mode.el'.
;;  That includes things like `icicle-read+insert-file-name-keys'.
(defun icicle-bookmark-cleanup ()
  "Cleanup code for `icicle-bookmark'.
Remove crosshairs highlighting and unbind filtering keys."
  (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch))
  (when (featurep 'bookmark+)
    (dolist (map  '(minibuffer-local-must-match-map minibuffer-local-completion-map))
      (define-key (symbol-value map) (icicle-kbd "C-M-b") nil) ; `C-M-b'
      (define-key (symbol-value map) (icicle-kbd "C-M-B") nil) ; `C-M-B'
      (define-key (symbol-value map) (icicle-kbd "C-M-d") nil) ; `C-M-d'
      (define-key (symbol-value map) (icicle-kbd "C-M-f") nil) ; `C-M-f'
      (define-key (symbol-value map) (icicle-kbd "C-M-F") nil) ; `C-M-F'
      (dolist (key  icicle-read+insert-file-name-keys) ; `C-M-F' - overrides previous.
        (define-key (symbol-value map) key 'icicle-read+insert-file-name))
      (define-key (symbol-value map) (icicle-kbd "C-M-g") nil) ; `C-M-g'
      (define-key (symbol-value map) (icicle-kbd "C-M-I") nil) ; `C-M-I' (`C-M-i' is `ESC TAB')
      (define-key (symbol-value map) (icicle-kbd "C-M-K") nil) ; `C-M-K'
      (define-key (symbol-value map) (icicle-kbd "C-M-m") nil) ; `C-M-m'
      (define-key (symbol-value map) (icicle-kbd "C-M-r") nil) ; `C-M-r'
      (define-key (symbol-value map) (icicle-kbd "C-M-w") nil) ; `C-M-w'
      (define-key (symbol-value map) (icicle-kbd "C-M-@") nil) ; `C-M-@'
      (define-key (symbol-value map) (icicle-kbd "C-M-.") ; `C-M-.'
        'icicle-toggle-dot)             ; `icicles-mode.el'.
      (define-key (symbol-value map) (icicle-kbd "C-M-= b") nil) ; `C-M-= b'
      (define-key (symbol-value map) (icicle-kbd "C-M-= f") nil)))) ; `C-M-= f'

(defun icicle-bookmark-cleanup-on-quit ()
  "Do `icicle-bookmark-cleanup', then return to original window."
  (icicle-bookmark-cleanup)
  (when (window-live-p icicle-orig-window)
    (select-window icicle-orig-window)
    (select-frame-set-input-focus (selected-frame))))

;;; These are minibuffer commands, but we define them here instead of in `icicles-mcmd.el'.

;;;###autoload (autoload 'icicle-bookmark-autofile-narrow "icicles")
(defun icicle-bookmark-autofile-narrow () ; Bound to `C-x j a' in minibuffer for completion.
  "Narrow the bookmark candidates to autofile bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-autofile-bookmark-p (icicle-mctized-display-candidate (car x))))))

;;;###autoload (autoload 'icicle-bookmark-autonamed-narrow "icicles")
(defun icicle-bookmark-autonamed-narrow () ; Bound to `C-x j #' in minibuffer for completion.
  "Narrow the bookmark candidates to autonamed bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x)
     (bmkp-autonamed-bookmark-p (icicle-mctized-display-candidate (car x))))))

;;;###autoload (autoload 'icicle-bookmark-autonamed-this-buffer-narrow "icicles")
(defun icicle-bookmark-autonamed-this-buffer-narrow ()
                                        ; Bound to `C-x j , #' in minibuffer for completion.
  "Narrow bookmark candidates to autonamed bookmarks in current buffer."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x)                          ; FREE here: ICICLE-ORIG-BUFF.
     (with-current-buffer icicle-orig-buff
       (bmkp-autonamed-this-buffer-bookmark-p (icicle-transform-multi-completion (car x)))))))

;;;###autoload (autoload 'icicle-bookmark-bookmark-file-narrow "icicles")
(defun icicle-bookmark-bookmark-file-narrow () ; Bound to `C-x j y' in minibuffer for completion.
  "Narrow the bookmark candidates to bookmark-file bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-bookmark-file-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-bookmark-list-narrow "icicles")
(defun icicle-bookmark-bookmark-list-narrow () ; Bound to `C-x j B' in minibuffer for completion.
  "Narrow the bookmark candidates to bookmark-list bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-bookmark-list-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-desktop-narrow "icicles")
(defun icicle-bookmark-desktop-narrow ()   ; Bound to `C-x j K' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to desktop bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-desktop-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-dired-narrow "icicles")
(defun icicle-bookmark-dired-narrow ()   ; Bound to `C-x j d' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to Dired bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-dired-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-file-narrow "icicles")
(defun icicle-bookmark-file-narrow ()   ; Bound to `C-x j f' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to file bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-file-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-file-this-dir-narrow "icicles")
(defun icicle-bookmark-file-this-dir-narrow () ; Bound to `C-x j . f' in minibuffer for completion.
  "Narrow the bookmark candidates to bookmarked files in `default-directory'."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-file-this-dir-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-gnus-narrow "icicles")
(defun icicle-bookmark-gnus-narrow ()   ; Bound to `C-x j g' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to Gnus bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-gnus-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-image-narrow "icicles")
(defun icicle-bookmark-image-narrow ()   ; Bound to `C-x j M-i' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to image bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-image-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-info-narrow "icicles")
(defun icicle-bookmark-info-narrow ()   ; Bound to `C-x j i' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to Info bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-info-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-local-file-narrow "icicles")
(defun icicle-bookmark-local-file-narrow () ; Bound to `C-x j l' for bookmark completion.
  "Narrow the bookmark candidates to local-file bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-local-file-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-man-narrow "icicles")
(defun icicle-bookmark-man-narrow () ; Bound to `C-x j m' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to `man'-page bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-man-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-non-file-narrow "icicles")
(defun icicle-bookmark-non-file-narrow () ; Bound to `C-x j b' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to non-file (buffer-only) bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-non-file-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-region-narrow "icicles")
(defun icicle-bookmark-region-narrow () ; Bound to `C-x j r' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to bookmarks with regions."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-region-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-remote-file-narrow "icicles")
(defun icicle-bookmark-remote-file-narrow () ; Bound to `C-x j n' in minibuf for bookmark completion.
  "Narrow the bookmark candidates to remote-file bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-remote-file-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-specific-buffers-narrow "icicles")
(defun icicle-bookmark-specific-buffers-narrow (buffers) ; `C-x j = b' for bookmark completion.
  "Narrow the bookmark candidates to bookmarks for specific BUFFERS.
You are prompted for the BUFFERS."
  (interactive (let ((icicle-completion-candidates  icicle-completion-candidates))
                 (list (icicle-bookmarked-buffer-list))))
  (icicle-narrow-candidates-with-predicate
   `(lambda (x)
     (member (bmkp-get-buffer-name (icicle-transform-multi-completion (car x))) ',buffers))))

;;;###autoload (autoload 'icicle-bookmark-specific-files-narrow "icicles")
(defun icicle-bookmark-specific-files-narrow (files) ; `C-x j = f' in minibuf for bookmark completion.
  "Narrow the bookmark candidates to bookmarks for specific FILES.
You are prompted for the FILES."
  (interactive (list (icicle-bookmarked-file-list)))
  (icicle-narrow-candidates-with-predicate
   `(lambda (x)
     (member (bookmark-get-filename (icicle-transform-multi-completion (car x))) ',files))))

;;;###autoload (autoload 'icicle-bookmark-temporary-narrow "icicles")
(defun icicle-bookmark-temporary-narrow () ; Bound to `C-x j x' in minibuffer for completion.
  "Narrow the bookmark candidates to temporary bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-temporary-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-this-buffer-narrow "icicles")
(defun icicle-bookmark-this-buffer-narrow () ; `C-x j , ,' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to bookmarks for the current buffer."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x)                          ; FREE here: ICICLE-ORIG-BUFF.
     (with-current-buffer icicle-orig-buff
       (bmkp-this-buffer-p (icicle-transform-multi-completion (car x)))))))

;;;###autoload (autoload 'icicle-bookmark-url-narrow "icicles")
(defun icicle-bookmark-url-narrow ()    ; Bound to `C-x j u' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to URL bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-url-bookmark-p (icicle-transform-multi-completion (car x))))))

;;;###autoload (autoload 'icicle-bookmark-w3m-narrow "icicles")
(defun icicle-bookmark-w3m-narrow ()    ; Bound to `C-x j w' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to W3M (URL) bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   (lambda (x) (bmkp-w3m-bookmark-p (icicle-transform-multi-completion (car x))))))


;; The following sexps macro-expand to define these commands:
;;  `icicle-bookmark-autofile'                   `icicle-bookmark-autofile-other-window'
;;  `icicle-bookmark-autofile-all-tags',         `icicle-bookmark-autofile-all-tags-other-window',
;;  `icicle-bookmark-autofile-all-tags-regexp',  `icicle-bookmark-autofile-all-tags-regexp-other-window',
;;  `icicle-bookmark-autofile-some-tags',        `icicle-bookmark-autofile-some-tags-other-window',
;;  `icicle-bookmark-autofile-some-tags-regexp', `icicle-bookmark-autofile-some-tags-regexp-other-window',
;;  `icicle-bookmark-autonamed'                  `icicle-bookmark-autonamed-other-window'
;;  `icicle-bookmark-autonamed-this-buffer'      `icicle-bookmark-autonamed-this-buffer-other-window'
;;  `icicle-bookmark-bookmark-file',
;;  `icicle-bookmark-bookmark-list',
;;  `icicle-bookmark-desktop',
;;  `icicle-bookmark-dired',                     `icicle-bookmark-dired-other-window',
;;  `icicle-bookmark-file',                      `icicle-bookmark-file-other-window',
;;  `icicle-bookmark-file-all-tags',             `icicle-bookmark-file-all-tags-other-window',
;;  `icicle-bookmark-file-all-tags-regexp',      `icicle-bookmark-file-all-tags-regexp-other-window',
;;  `icicle-bookmark-file-some-tags',            `icicle-bookmark-file-some-tags-other-window',
;;  `icicle-bookmark-file-some-tags-regexp',     `icicle-bookmark-file-some-tags-regexp-other-window',
;;  `icicle-bookmark-file-this-dir',             `icicle-bookmark-file-this-dir-other-window',
;;  `icicle-bookmark-file-this-dir-all-tags',
;;  `icicle-bookmark-file-this-dir-all-tags-other-window',
;;  `icicle-bookmark-file-this-dir-all-tags-regexp',
;;  `icicle-bookmark-file-this-dir-all-tags-regexp-other-window',
;;  `icicle-bookmark-file-this-dir-some-tags',
;;  `icicle-bookmark-file-this-dir-some-tags-other-window',
;;  `icicle-bookmark-file-this-dir-some-tags-regexp',
;;  `icicle-bookmark-file-this-dir-some-tags-regexp-other-window',
;;  `icicle-bookmark-gnus',                      `icicle-bookmark-gnus-other-window',
;;  `icicle-bookmark-image',                     `icicle-bookmark-image-other-window',
;;  `icicle-bookmark-info',                      `icicle-bookmark-info-other-window',
;;  `icicle-bookmark-local-file',                `icicle-bookmark-local-file-other-window',
;;  `icicle-bookmark-man',                       `icicle-bookmark-man-other-window',
;;  `icicle-bookmark-non-file',                  `icicle-bookmark-non-file-other-window',
;;  `icicle-bookmark-region',                    `icicle-bookmark-region-other-window',
;;  `icicle-bookmark-remote-file',               `icicle-bookmark-remote-file-other-window',
;;  `icicle-bookmark-specific-buffers',          `icicle-bookmark-specific-buffers-other-window'
;;  `icicle-bookmark-specific-files',            `icicle-bookmark-specific-files-other-window'
;;  `icicle-bookmark-all-tags',                  `icicle-bookmark-all-tags-other-window'
;;  `icicle-bookmark-all-tags-regexp',           `icicle-bookmark-all-tags-regexp-other-window'
;;  `icicle-bookmark-some-tags',                 `icicle-bookmark-some-tags-other-window'
;;  `icicle-bookmark-some-tags-regexp',          `icicle-bookmark-some-tags-regexp-other-window'
;;  `icicle-bookmark-temporary'                  `icicle-bookmark-temporary-other-window'
;;  `icicle-bookmark-this-buffer',               `icicle-bookmark-this-buffer-other-window'
;;  `icicle-bookmark-url',                       `icicle-bookmark-url-other-window'
;;  `icicle-bookmark-w3m',                       `icicle-bookmark-w3m-other-window'

;;;###autoload (autoload 'icicle-bookmark-this-buffer "icicles")
(icicle-define-bookmark-command              "this-buffer")                   ; `C-x j , ,'
;;;###autoload (autoload 'icicle-bookmark-this-buffer-other-window "icicles")
(icicle-define-bookmark-other-window-command "this-buffer")                   ; `C-x 4 j , ,'
;;;###autoload (autoload 'icicle-bookmark-specific-buffers "icicles")
(icicle-define-bookmark-command              "specific-buffers" nil           ; `C-x j = b'
                                             (icicle-bookmarked-buffer-list))
;;;###autoload (autoload 'icicle-bookmark-specific-buffers-other-window "icicles")
(icicle-define-bookmark-other-window-command "specific-buffers" nil           ; `C-x 4 j = b'
                                             (icicle-bookmarked-buffer-list))
;;;###autoload (autoload 'icicle-bookmark-specific-files "icicles")
(icicle-define-bookmark-command              "specific-files" nil             ; `C-x j = f'
                                             (icicle-bookmarked-file-list))
;;;###autoload (autoload 'icicle-bookmark-specific-files-other-window "icicles")
(icicle-define-bookmark-other-window-command "specific-files" nil             ; `C-x 4 j = f'
                                             (icicle-bookmarked-file-list))
;;;###autoload (autoload 'icicle-bookmark-autofile "icicles")
(icicle-define-bookmark-command              "autofile")                      ; `C-x j a'
;;;###autoload (autoload 'icicle-bookmark-autofile-other-window "icicles")
(icicle-define-bookmark-other-window-command "autofile")                      ; `C-x 4 j a'
;;;###autoload (autoload 'icicle-bookmark-autofile-all-tags "icicles")
(icicle-define-bookmark-command              "autofile-all-tags" nil          ; `C-x j t a *'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-autofile-all-tags-other-window "icicles")
(icicle-define-bookmark-other-window-command "autofile-all-tags" nil          ; `C-x 4 j t a *'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-autofile-all-tags-regexp "icicles")
(icicle-define-bookmark-command              "autofile-all-tags-regexp" nil   ; `C-x j t a % *'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-autofile-all-tags-regexp-other-window "icicles")
(icicle-define-bookmark-other-window-command "autofile-all-tags-regexp" nil   ; `C-x 4 j t a % *'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-autofile-some-tags "icicles")
(icicle-define-bookmark-command              "autofile-some-tags" nil         ; `C-x j t a +'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-autofile-some-tags-other-window "icicles")
(icicle-define-bookmark-other-window-command "autofile-some-tags" nil         ; `C-x 4 j t a +'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-autofile-some-tags-regexp "icicles")
(icicle-define-bookmark-command              "autofile-some-tags-regexp" nil  ; `C-x j t a % +'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-autofile-some-tags-regexp-other-window "icicles")
(icicle-define-bookmark-other-window-command "autofile-some-tags-regexp" nil  ; `C-x 4 j t a % +'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-autonamed "icicles")
(icicle-define-bookmark-command              "autonamed") ; `C-x j #'
;;;###autoload (autoload 'icicle-bookmark-autonamed-other-window "icicles")
(icicle-define-bookmark-other-window-command "autonamed") ; `C-x 4 j # #'
;;;###autoload (autoload 'icicle-bookmark-autonamed-this-buffer "icicles")
(icicle-define-bookmark-command              "autonamed-this-buffer") ; `C-x j , #'
;;;###autoload (autoload 'icicle-bookmark-autonamed-this-buffer-other-window "icicles")
(icicle-define-bookmark-other-window-command "autonamed-this-buffer") ; `C-x 4 j # .'
;;;###autoload (autoload 'icicle-bookmark-non-file "icicles")
(icicle-define-bookmark-command              "non-file")                      ; `C-x j b'
;;;###autoload (autoload 'icicle-bookmark-non-file-other-window "icicles")
(icicle-define-bookmark-other-window-command "non-file")                      ; `C-x 4 j b'

;; Other-window means nothing for a bookmark list.
;;;###autoload (autoload 'icicle-bookmark-bookmark-list "icicles")
(icicle-define-bookmark-command              "bookmark-list")                 ; `C-x j B'
;;;###autoload (autoload 'icicle-bookmark-dired "icicles")
(icicle-define-bookmark-command              "dired")                         ; `C-x j d'
;;;###autoload (autoload 'icicle-bookmark-dired-other-window "icicles")
(icicle-define-bookmark-other-window-command "dired")                         ; `C-x 4 j d'
;;;###autoload (autoload 'icicle-bookmark-file "icicles")
(icicle-define-bookmark-command              "file")                          ; `C-x j f'
;;;###autoload (autoload 'icicle-bookmark-file-other-window "icicles")
(icicle-define-bookmark-other-window-command "file")                          ; `C-x 4 j f'
;;;###autoload (autoload 'icicle-bookmark-file-this-dir "icicles")
(icicle-define-bookmark-command              "file-this-dir")                 ; `C-x j . f'
;;;###autoload (autoload 'icicle-bookmark-file-this-dir-other-window "icicles")
(icicle-define-bookmark-other-window-command "file-this-dir")                 ; `C-x 4 j . f'
;;;###autoload (autoload 'icicle-bookmark-gnus "icicles")
(icicle-define-bookmark-command              "gnus")                          ; `C-x j g'
;;;###autoload (autoload 'icicle-bookmark-gnus-other-window "icicles")
(icicle-define-bookmark-other-window-command "gnus")                          ; `C-x 4 j g'
;;;###autoload (autoload 'icicle-bookmark-image "icicles")
(icicle-define-bookmark-command              "image")                         ; `C-x j M-i'
;;;###autoload (autoload 'icicle-bookmark-image-other-window "icicles")
(icicle-define-bookmark-other-window-command "image")                         ; `C-x 4 j M-i'
;;;###autoload (autoload 'icicle-bookmark-info "icicles")
(icicle-define-bookmark-command              "info")                          ; `C-x j i'
;;;###autoload (autoload 'icicle-bookmark-info-other-window "icicles")
(icicle-define-bookmark-other-window-command "info")                          ; `C-x 4 j i'

;; Other-window means nothing for a desktop.
;;;###autoload (autoload 'icicle-bookmark-desktop "icicles")
(icicle-define-bookmark-command              "desktop")                       ; `C-x j K'
;;;###autoload (autoload 'icicle-bookmark-local-file "icicles")
(icicle-define-bookmark-command              "local-file")                    ; `C-x j l'
;;;###autoload (autoload 'icicle-bookmark-local-file-other-window "icicles")
(icicle-define-bookmark-other-window-command "local-file")                    ; `C-x 4 j l'
;;;###autoload (autoload 'icicle-bookmark-man "icicles")
(icicle-define-bookmark-command              "man")                           ; `C-x j m'
;;;###autoload (autoload 'icicle-bookmark-man-other-window "icicles")
(icicle-define-bookmark-other-window-command "man")                           ; `C-x 4 j m'
;;;###autoload (autoload 'icicle-bookmark-remote-file "icicles")
(icicle-define-bookmark-command              "remote-file")                   ; `C-x j n'
;;;###autoload (autoload 'icicle-bookmark-remote-file-other-window "icicles")
(icicle-define-bookmark-other-window-command "remote-file")                   ; `C-x 4 j n'
;;;###autoload (autoload 'icicle-bookmark-region "icicles")
(icicle-define-bookmark-command              "region" "Select region: ")      ; `C-x j r'
;;;###autoload (autoload 'icicle-bookmark-region-other-window "icicles")
(icicle-define-bookmark-other-window-command "region" "Select region: ")      ; `C-x 4 j r'
;;;###autoload (autoload 'icicle-bookmark-all-tags "icicles")
(icicle-define-bookmark-command              "all-tags" nil                   ; `C-x j t *'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-all-tags-other-window "icicles")
(icicle-define-bookmark-other-window-command "all-tags" nil                   ; `C-x 4 j t *'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-some-tags "icicles")
(icicle-define-bookmark-command              "some-tags" nil                  ; `C-x j t +'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-some-tags-other-window "icicles")
(icicle-define-bookmark-other-window-command "some-tags" nil                  ; `C-x 4 j t +'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-all-tags-regexp "icicles")
(icicle-define-bookmark-command              "all-tags-regexp" nil            ; `C-x j t % *'
                                             (read-string "Regexp for tags: "))
;;;###autoload (autoload 'icicle-bookmark-all-tags-regexp-other-window "icicles")
(icicle-define-bookmark-other-window-command "all-tags-regexp" nil            ; `C-x 4 j t % *'
                                             (read-string "Regexp for tags: "))
;;;###autoload (autoload 'icicle-bookmark-some-tags-regexp "icicles")
(icicle-define-bookmark-command              "some-tags-regexp" nil           ; `C-x j t % +'
                                             (read-string "Regexp for tags: "))
;;;###autoload (autoload 'icicle-bookmark-some-tags-regexp-other-window "icicles")
(icicle-define-bookmark-other-window-command "some-tags-regexp" nil           ; `C-x 4 j t % +'
                                             (read-string "Regexp for tags: "))
;;;###autoload (autoload 'icicle-bookmark-file-all-tags "icicles")
(icicle-define-bookmark-command              "file-all-tags" nil              ; `C-x j t f *'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-file-all-tags-other-window "icicles")
(icicle-define-bookmark-other-window-command "file-all-tags" nil              ; `C-x 4 j t f *'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-file-some-tags "icicles")
(icicle-define-bookmark-command              "file-some-tags" nil             ; `C-x j t f +'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-file-some-tags-other-window "icicles")
(icicle-define-bookmark-other-window-command "file-some-tags" nil             ; `C-x 4 j t f +'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-file-all-tags-regexp "icicles")
(icicle-define-bookmark-command              "file-all-tags-regexp" nil       ; `C-x j t f % *'
                                             (read-string "Regexp for tags: "))
;;;###autoload (autoload 'icicle-bookmark-file-all-tags-regexp-other-window "icicles")
(icicle-define-bookmark-other-window-command "file-all-tags-regexp" nil       ; `C-x 4 j t f % *'
                                             (read-string "Regexp for tags: "))
;;;###autoload (autoload 'icicle-bookmark-file-some-tags-regexp "icicles")
(icicle-define-bookmark-command              "file-some-tags-regexp" nil      ; `C-x j t f % +'
                                             (read-string "Regexp for tags: "))
;;;###autoload (autoload 'icicle-bookmark-file-some-tags-regexp-other-window "icicles")
(icicle-define-bookmark-other-window-command "file-some-tags-regexp" nil      ; `C-x 4 j t f % +'
                                             (read-string "Regexp for tags: "))
;;;###autoload (autoload 'icicle-bookmark-file-this-dir-all-tags "icicles")
(icicle-define-bookmark-command              "file-this-dir-all-tags" nil ; `C-x j t . f *'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-file-this-dir-all-tags-other-window "icicles")
(icicle-define-bookmark-other-window-command "file-this-dir-all-tags" nil ; `C-x 4 j t . f *'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-file-this-dir-some-tags "icicles")
(icicle-define-bookmark-command              "file-this-dir-some-tags" nil ; `C-x j t . f +'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-file-this-dir-some-tags-other-window "icicles")
(icicle-define-bookmark-other-window-command "file-this-dir-some-tags" nil ; `C-x 4 j t . f +'
                                             (bmkp-read-tags-completing nil nil current-prefix-arg))
;;;###autoload (autoload 'icicle-bookmark-file-this-dir-all-tags-regexp "icicles")
(icicle-define-bookmark-command              "file-this-dir-all-tags-regexp" nil ; `C-x j t . f % *'
                                             (read-string "Regexp for tags: "))
;;;###autoload (autoload 'icicle-bookmark-file-this-dir-all-tags-regexp-other-window "icicles")
(icicle-define-bookmark-other-window-command "file-this-dir-all-tags-regexp" nil ; `C-x 4 j t . f % *'
                                             (read-string "Regexp for tags: "))
;;;###autoload (autoload 'icicle-bookmark-file-this-dir-some-tags-regexp "icicles")
(icicle-define-bookmark-command              "file-this-dir-some-tags-regexp" nil ; `C-x j t . f % +'
                                             (read-string "Regexp for tags: "))
;;;###autoload (autoload 'icicle-bookmark-file-this-dir-some-tags-regexp-other-window "icicles")
(icicle-define-bookmark-other-window-command "file-this-dir-some-tags-regexp" nil ; `C-x 4 j t . f % +'
                                             (read-string "Regexp for tags: "))
;;;###autoload (autoload 'icicle-bookmark-url "icicles")
(icicle-define-bookmark-command              "url")                           ; `C-x j u'
;;;###autoload (autoload 'icicle-bookmark-url-other-window "icicles")
(icicle-define-bookmark-other-window-command "url")                           ; `C-x 4 j u'
;;;###autoload (autoload 'icicle-bookmark-w3m "icicles")
(icicle-define-bookmark-command              "w3m")                           ; `C-x j w'
;;;###autoload (autoload 'icicle-bookmark-w3m-other-window "icicles")
(icicle-define-bookmark-other-window-command "w3m")                           ; `C-x 4 j w'
;;;###autoload (autoload 'icicle-bookmark-temporary "icicles")
(icicle-define-bookmark-command              "temporary")                     ; `C-x j x'
;;;###autoload (autoload 'icicle-bookmark-temporary-other-window "icicles")
(icicle-define-bookmark-other-window-command "temporary")                     ; `C-x 4 j x'

;; Other-window means nothing for a bookmark file.
;;;###autoload (autoload 'icicle-bookmark-bookmark-file "icicles")
(icicle-define-bookmark-command              "bookmark-file")                 ; `C-x j y'

;;;###autoload (autoload 'icicle-select-bookmarked-region "icicles")
(defalias 'icicle-select-bookmarked-region 'icicle-bookmark-region-other-window)

;;;###autoload (autoload 'icicle-bookmarked-buffer-list "icicles")
(defun icicle-bookmarked-buffer-list ()
  "`icicle-buffer-list', but only for bookmarked buffers."
  (interactive)
  (let ((icicle-buffer-predicate  (lambda (buf) (member buf (bmkp-buffer-names))))
        (icicle-prompt            "Choose bookmarked buffer (`RET' when done): "))
    (icicle-buffer-list)))
  
;;;###autoload (autoload 'icicle-bookmarked-file-list "icicles")
(defun icicle-bookmarked-file-list ()
  "`icicle-file-list', but only for bookmarked files."
  (interactive)
  (bookmark-maybe-load-default-file)
  (let ((use-file-dialog        nil)
        (icicle-file-predicate  (lambda (file) (member (expand-file-name file) (bmkp-file-names))))
        (icicle-prompt          "Choose bookmarked file (`RET' when done): "))
    (icicle-file-list)))

;;;###autoload (autoload 'icicle-find-first-tag "icicles")
(icicle-define-command icicle-find-first-tag ; Command name
  "Find first tag in current tags table whose name matches your input.
This is similar to standard command `find-tag', with these
differences:

* This is a multi-command, so you can visit any number of tags.

* Only the first tag of several identical tags is a candidate, so you
  cannot visit the others.  That is, there is no equivalent to using
  `M-,' (`tags-loop-continue') after `find-tag' to find additional,
  identical tags.

* If `crosshairs.el' is loaded, the target position is highlighted.

To browse all tags (including duplicates) in all tags tables, use the
more powerful Icicles multi-command `icicle-find-tag'.

By default, Icicle mode remaps all key sequences that are normally
bound to `find-tag-other-window' to `icicle-find-first-tag'.  If you
do not want this remapping, then customize option
`icicle-top-level-key-bindings'."       ; Doc string
  icicle-find-first-tag-action          ; Action function
  "Find tag: "                          ; `completing-read' args
  (if (fboundp 'tags-lazy-completion-table) (tags-lazy-completion-table) 'tags-complete-tag)
  nil nil nil nil (funcall (or find-tag-default-function  (get major-mode 'find-tag-default-function)
                               'find-tag-default))
  nil
  ((completion-ignore-case    (progn (require 'etags) ; Bindings
                                     (if (and (boundp 'tags-case-fold-search)
                                              (memq tags-case-fold-search '(t nil)))
                                         tags-case-fold-search
                                       case-fold-search)))
   (case-fold-search          completion-ignore-case))
  nil nil                               ; First code, undo code
  (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch))) ; Last code

(defun icicle-find-first-tag-action (cand)
  "Action function for `icicle-find-first-tag'."
  (find-tag cand)
  (when (fboundp 'crosshairs-highlight) (crosshairs-highlight)))

;;;###autoload (autoload 'icicle-find-first-tag-other-window "icicles")
(icicle-define-command icicle-find-first-tag-other-window ; Command name
  "Find first tag in current tags table whose name matches your input.
Same as `icicle-find-first-tag' except it uses a different window." ; Doc string
  icicle-find-first-tag-other-window-action ; Action function
  "Find tag other window: "             ; `completing-read' args
  (if (fboundp 'tags-lazy-completion-table) (tags-lazy-completion-table) 'tags-complete-tag)
  nil nil nil nil (funcall (or find-tag-default-function  (get major-mode 'find-tag-default-function)
                               'find-tag-default))
  nil
  ((completion-ignore-case  (progn (require 'etags)
                                   (if (and (boundp 'tags-case-fold-search) ; Bindings
                                            (memq tags-case-fold-search '(t nil)))
                                       tags-case-fold-search
                                     case-fold-search)))
   (case-fold-search        completion-ignore-case))
  nil nil                               ; First code, undo code
  (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch))) ; Last code

(defun icicle-find-first-tag-other-window-action (cand)
  "Action function for `icicle-find-first-tag-other-window'."
  (find-tag-other-window cand)
  (when (fboundp 'crosshairs-highlight) (crosshairs-highlight)))

;;;###autoload (autoload 'icicle-find-tag "icicles")
(defun icicle-find-tag (regexp &optional arg)
  "Navigate among all tags that match REGEXP.
You are prompted for the REGEXP to match.  Enter REGEXP with `RET'.
You can use completion to choose a tag in the current tags table as
REGEXP.  You can use `\\[icicle-pop-tag-mark]' to return to your starting point.

All matching tags are shown, including duplicate tags from the same or
different source files.  This means that you do not need `M-,' - you
see all tags as candidates to visit.

By default:

* Tags from all tags files are candidates.
* In `*Completions*', the source file name is shown after each tag.

A prefix argument changes this default behavior, as follows:

* ARG = 0 or ARG > 0: only the current tag table is used
* ARG = 0 or ARG < 0: source file names are not shown

By default, Icicle mode remaps all key sequences that are normally
bound to `find-tag' to `icicle-find-tag'.  If you do not want this
remapping, then customize option `icicle-top-level-key-bindings'.

If `crosshairs.el' is loaded, then the target position is highlighted."
  (interactive
   (let* ((completion-ignore-case  (if (and (boundp 'tags-case-fold-search)
                                            (memq tags-case-fold-search '(t nil)))
                                       tags-case-fold-search
                                     case-fold-search))
          (case-fold-search        completion-ignore-case))
     (require 'etags)
     (list (completing-read "Find tag matching regexp: "
                            ;; $$$ Or should we just read a regexp against `regexp-history'?
                            (if (fboundp 'tags-lazy-completion-table)
                                (tags-lazy-completion-table) ; Emacs 23+
                              'tags-complete-tag) ; Emacs < 23
                            nil nil nil 'find-tag-history
                            (funcall (or find-tag-default-function
                                         (get major-mode 'find-tag-default-function)
                                         'find-tag-default)))
           current-prefix-arg)))

  (unwind-protect
       (let* ((icicle-whole-candidate-as-text-prop-p  t)
              (icicle-sort-comparer                   nil)
              (icicle-inhibit-sort-p                  t)
              (icicle-candidate-action-fn             'icicle-find-tag-action)
              (icicle-candidate-help-fn               'icicle-find-tag-help)
              (completion-ignore-case                 (if (and (boundp 'tags-case-fold-search)
                                                               (memq tags-case-fold-search '(t nil)))
                                                          tags-case-fold-search
                                                        case-fold-search))
              (case-fold-search                       completion-ignore-case)
              (orig-pt-find-tag                       (point-marker)))

         (ring-insert find-tag-marker-ring orig-pt-find-tag) ; Record starting point.
         (icicle-explore `(lambda () (icicle-find-tag-define-candidates ',regexp ',arg))
                         #'icicle-find-tag-final-act #'icicle-find-tag-quit-or-error
                         #'icicle-find-tag-quit-or-error nil
                         "Choose a tag: " nil nil nil 'find-tag-history))
    (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch))))

;;;###autoload (autoload 'icicle-pop-tag-mark "icicles")
(defun icicle-pop-tag-mark ()
  "Like `pop-tag-mark', but uses `pop-to-buffer', not `switch-to-buffer'.
By default, Icicle mode remaps all key sequences that are normally
bound to `pop-tag-mark' to `icicle-pop-tag-mark'.  If you do not want
this remapping, then customize option
`icicle-top-level-key-bindings'."
  (interactive)
  (require 'etags)
  (when (ring-empty-p find-tag-marker-ring) (error "No previous locations for find-tag invocation"))
  (let ((marker  (ring-remove find-tag-marker-ring 0)))
    (pop-to-buffer (or (marker-buffer marker)  (error "The marked buffer has been deleted")))
    (goto-char (marker-position marker))
    (unless (pos-visible-in-window-p) (recenter icicle-recenter))
    (set-marker marker nil nil)))

(defun icicle-find-tag-define-candidates (regexp arg)
  "Define candidates for `icicle-find-tag'.
See `icicle-explore', argument DEFINE-CANDIDATES-FN."
  (save-excursion
    (let ((first-time  t)
          (morep       t))
      (setq icicle-candidates-alist  ())
      (while (and morep  (visit-tags-table-buffer (not first-time)))
        (when (and arg  (wholenump (prefix-numeric-value arg))) (setq morep  nil))
        (setq first-time               nil
              icicle-candidates-alist  (append icicle-candidates-alist
                                               (nreverse (icicle-find-tag-define-candidates-1
                                                          regexp (> (prefix-numeric-value arg) 0)))))))))

(defun icicle-find-tag-define-candidates-1 (regexp show-file-p)
  "Helper function for `icicle-find-tag-define-candidates'.
Returns completion alist of tag information for tags matching REGEXP.
Include file name (label) if SHOW-FILE-P is non-nil.

If SHOW-FILE-P is nil, then alist items look like this:

 (TAG TAG-INFO FILE-PATH GOTO-FUNC)

If SHOW-FILE-P is non-nil, then alist items look like this:

 ((TAG FILE-LABEL) TAG-INFO FILE-PATH GOTO-FUNC) or

 (FILE-LABEL TAG-INFO FILE-PATH GOTO-FUNC) if no matching TAG.

TAG-INFO is what `snarf-tag-function' (e.g. `etags-snarf-tag')
returns.  It is a cons (TEXT LINE . POSITION).

TEXT is the initial part of a line containing the tag.
LINE is the line number.
POSITION is the (one-based) char position of TEXT within the file.

If TEXT is t, it means the tag refers to exactly LINE or POSITION,
whichever is present, LINE having preference, no searching.
Either LINE or POSITION can be nil.  POSITION is used if present."
  (icicle-highlight-lighter)
  (message "Gathering tags for `%s'..." regexp)
  (goto-char (point-min))
  (let ((temp-list  ()))
    (while (re-search-forward (concat regexp ".*\177*") nil t) ; Look before the DEL character.
      (beginning-of-line)
      (let* ((goto-func  goto-tag-location-function) ; e.g. `etags-goto-tag-location'.
             ;; TAG-INFO: If no specific tag, (t nil (point-min)). Else, (TEXT LINE . STARTPOS).
             ;; e.g. TEXT = "(defun foo ()" or just "foo" (if explicit),
             ;;      LINE = "148", STARTPOS = "1723"
             (tag-info (save-excursion (funcall snarf-tag-function))) ; e.g. `etags-snarf-tag'.
             (tag (if (eq t (car tag-info)) nil (car tag-info)))
             ;; FILE-PATH is absolute. FILE-LABEL is relative to `default-directory'.
             (file-path (save-excursion
                          (if tag (file-of-tag) (save-excursion (next-line 1) (file-of-tag)))))
             (file-label (expand-file-name file-path (file-truename default-directory))))
        (when (and tag  (not (string= "" tag))  (= (aref tag 0) ?\( ))
          (setq tag  (concat tag " ...)")))
        (when (file-readable-p file-path)
          ;; Add item to alist.
          ;;   Item looks like this:         ((TAG FILE-LABEL) TAG-INFO FILE-PATH GOTO-FUNC)
          ;;   or like this, if no matching tag: ((FILE-LABEL) TAG-INFO FILE-PATH GOTO-FUNC)
          (cond (tag
                 (push `(,(if show-file-p
                              (list tag ; Make multi-completion cons: add file name to candidate.
                                    (progn (put-text-property 0 (length file-label) 'face
                                                              'icicle-candidate-part file-label)
                                           file-label))
                              tag)
                         ,tag-info ,file-path ,goto-func)
                       temp-list))
                (show-file-p            ; No tag.  Use only the FILE-LABEL.
                 (push `((,(progn (put-text-property 0 (length file-label) 'face
                                                     'icicle-candidate-part file-label)
                                  file-label))
                         ,tag-info ,file-path ,goto-func)
                       temp-list)))))
      (forward-line))
    temp-list))                         ; Return the alist for this TAGS file.

(defun icicle-find-tag-action (ignored-string)
  "Action function for `icicle-find-tag'."
  ;; Ignore (TAG FILE-LABEL) part.  Use only (TAG-INFO FILE-PATH GOTO-FUNC) part.
  (let* ((cand       (cdr (elt (icicle-filter-alist icicle-candidates-alist icicle-completion-candidates)
                               icicle-candidate-nb)))
         (tag-info   (nth 0 cand))
         (goto-func  (nth 2 cand)))
    (switch-to-buffer-other-window      ; Go to source file at FILE-PATH.
     (if (fboundp 'tag-find-file-of-tag-noselect)
         (tag-find-file-of-tag-noselect (nth 1 cand))
       (find-file-noselect (nth 1 cand))))
    (widen)
    (icicle-condition-case-no-debug err
        (funcall goto-func tag-info)    ; Go to text at TAG-INFO.
      (error (message "%s" (error-message-string err)) (sit-for 2) nil)))
  (when (fboundp 'crosshairs-highlight) (crosshairs-highlight))
  (select-window (minibuffer-window))
  (select-frame-set-input-focus (selected-frame)))

(defun icicle-find-tag-help (cand)
  "Use as `icicle-candidate-help-fn' for `icicle-find-tag'."
  (let* ((cand      (cdr (elt (icicle-filter-alist icicle-candidates-alist icicle-completion-candidates)
                              icicle-candidate-nb)))
         (tag-info  (nth 0 cand)))
    (message (if (eq t (car tag-info))
                 "No tag - file name itself matches"
               (format "Line: %s, Position: %s, File: %s"
                       (icicle-propertize (cadr tag-info) 'face 'icicle-msg-emphasis)
                       (icicle-propertize (cddr tag-info) 'face 'icicle-msg-emphasis)
                       (icicle-propertize (nth 1 cand)    'face 'icicle-msg-emphasis))))
    (sit-for 4)))

(defun icicle-find-tag-final-act ()
  "Go to the final tag choice."
  (let ((cand  (cdr icicle-explore-final-choice-full)))
    (unless cand (error "No such occurrence: %s" cand))
    (switch-to-buffer-other-window      ; Go to source file at FILE-PATH.
     (if (fboundp 'tag-find-file-of-tag-noselect)
         (tag-find-file-of-tag-noselect (nth 1 cand))
       (find-file-noselect (nth 1 cand))))
    (widen)
    (funcall (nth 2 cand) (nth 0 cand)))) ; Go to text at TAG-INFO.

(defun icicle-find-tag-quit-or-error ()
  "Pop back to the last tag visited."
  (icicle-pop-tag-mark)
  (raise-frame))

;;;###autoload (autoload 'icicle-other-window-or-frame "icicles")
(defun icicle-other-window-or-frame (arg) ; Bound to `C-x o' in Icicle mode.
  "Select a window or frame, by name or by order.
This command combines Emacs commands `other-window' and `other-frame',
together with Icicles multi-commands `icicle-select-window', and
`icicle-select-frame'.  Use the prefix argument to choose, as follows:

 With no prefix arg or a non-zero numeric prefix arg:
  If the selected frame has multiple windows, then this is
  `other-window'.  Otherwise, it is `other-frame'.

 With a zero prefix arg (e.g. `C-0'):
  If the selected frame has multiple windows, then this is
  `icicle-select-window' with windows in the frame as candidates.
  Otherwise (single-window frame), this is `icicle-select-frame'.

 With plain `C-u':
  If the selected frame has multiple windows, then this is
  `icicle-select-window' with windows from all visible frames as
  candidates.  Otherwise, this is `icicle-select-frame'.

By default, Icicle mode remaps all key sequences that are normally
bound to `other-window' to `icicle-other-window-or-frame'.  If you do
not want this remapping, then customize option
`icicle-top-level-key-bindings'."
  (interactive "P")
  (let ((numarg  (prefix-numeric-value arg)))
    (cond ((consp arg)
           (if (one-window-p) (icicle-select-frame) (icicle-select-window)))
          ((zerop numarg)
           (if (one-window-p)
               (icicle-select-frame)
             (let ((current-prefix-arg  nil)) (icicle-select-window))))
          (t
           (if (one-window-p) (other-frame numarg) (other-window numarg))))))

;;;###autoload (autoload 'icicle-select-frame "icicles")
(icicle-define-command icicle-select-frame ; Bound to `C-x 5 o' in Icicle mode.
  "Select frame by its name and raise it.
A frame name in this context is suffixed as needed by [NUMBER], to
make it unique.  For example, in a context where frames are named for
their buffers and you have two frames showing buffer *Help*, one of
the frames will be called `*Help*[2]' for use with this command." ; Doc string
  icicle-select-frame-by-name           ; Action function
  "Select frame: "                      ; `completing-read' args
  icicle-frame-alist nil t nil
  (if (boundp 'frame-name-history) 'frame-name-history 'icicle-frame-name-history)
  (cdr (assq 'name (frame-parameters (next-frame (selected-frame))))) nil
  ((icicle-frame-alist  (icicle-make-frame-alist)) ; Bindings
   (alt-fn              nil)
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn  (setq alt-fn  (icicle-alt-act-fn-for-type "frame"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn  alt-fn  (icicle-alt-act-fn-for-type "frame")))))

;;;###autoload (autoload 'icicle-select-frame-by-name "icicles")
(defun icicle-select-frame-by-name (name &optional frame-alist)
  "Select the frame named NAME, and raise it.
Optional argument FRAME-ALIST is an alist of frames to choose from.
Each element has the form (FNAME . FRAME), where FNAME names FRAME.
See `icicle-make-frame-alist' for more about FNAME."
  (interactive (let* ((alist    (icicle-make-frame-alist))
                      (default  (car (rassoc (selected-frame) alist)))
                      (input    (completing-read "Select Frame: " alist nil t nil
                                                 'frame-name-history default)))
                 (list (if (= (length input) 0) default input)
                       alist)))
  (unless frame-alist (setq frame-alist  (or (and (boundp 'icicle-frame-alist)  icicle-frame-alist)
                                             (icicle-make-frame-alist))))
  (let ((frame  (cdr (assoc name frame-alist))))
    (unless frame (error "No such frame: `%s'" name))
    (make-frame-visible frame)
    (select-frame-set-input-focus frame)))

(defun icicle-make-frame-alist ()
  "Return an alist of entries (FNAME . FRAME), where FNAME names FRAME.
Frame parameter `name' is used as FNAME, unless there is more than one
frame with the same name.  In that case, FNAME includes a suffix
\[NUMBER], to make it a unique name.  The NUMBER order among frame
names that differ only by their [NUMBER] is arbitrary."
  (let ((fr-alist  ())
        (count     2)
        fname new-name)
    (dolist (fr  (frame-list))
      (setq fname  (frame-parameter fr 'name))
      (if (not (assoc fname fr-alist))
          (push (cons fname fr) fr-alist)
        (setq new-name  fname)
        (while (assoc new-name fr-alist)
          (setq new-name  (format "%s[%d]" fname count)
                count     (1+ count)))
        (push (cons new-name fr) fr-alist))
      (setq count  2))
    fr-alist))

;;;###autoload (autoload 'icicle-select-window "icicles")
(icicle-define-command icicle-select-window ; Command name
  ;; Free vars here: `icicle-window-alist' is bound in Bindings form.
  "Select window by its name.
With no prefix arg, candidate windows are those of the selected frame.
With a prefix arg, windows of all visible frames are candidates.

A window name is the name of its displayed buffer, but suffixed as
needed by [NUMBER], to make the name unique.  For example, if you have
two windows showing buffer *Help*, one of the windows will be called
`*Help*[2]' for use with this command." ; Doc string
  icicle-select-window-by-name          ; Action function
  "Select window: " icicle-window-alist nil t nil nil ; `completing-read' args
  (buffer-name (window-buffer (other-window 1))) nil
  ((icicle-window-alist  (icicle-make-window-alist current-prefix-arg)))) ; Bindings

;; Free vars here: `icicle-window-alist' is bound in `icicle-select-window'.
;;
;;;###autoload (autoload 'icicle-select-window-by-name "icicles")
(defun icicle-select-window-by-name (name &optional window-alist)
  "Select the window named NAME.
Optional argument WINDOW-ALIST is an alist of windows to choose from.

Interactively:
 A prefix arg means windows from all visible frames are candidates.
 No prefix arg means windows from the selected frame are candidates.

Each alist element has the form (WNAME . WINDOW), where WNAME names
WINDOW.  See `icicle-make-window-alist' for more about WNAME.

If `crosshairs.el' is loaded, then the target position is highlighted."
  (interactive (let* ((alist    (icicle-make-window-alist current-prefix-arg))
                      (default  (car (rassoc (selected-window) alist)))
                      (input    (completing-read "Select Window: " alist nil t nil nil default)))
                 (list (if (= (length input) 0) default input) alist)))
  (unless window-alist
    (setq window-alist  (or (and (boundp 'icicle-window-alist)  icicle-window-alist)
                            (icicle-make-window-alist))))
  (let ((window  (cdr (assoc name window-alist))))
    (unless window (error "No such window: `%s'" name))
    (select-window window)
    (when (fboundp 'crosshairs-highlight) (crosshairs-highlight))
    (select-frame-set-input-focus (selected-frame))))

(defun icicle-make-window-alist (&optional all-p)
  "Return an alist of entries (WNAME . WINDOW), where WNAME names WINDOW.
The name of the buffer in a window is used as its name, unless there
is more than one window displaying the same buffer.  In that case,
WNAME includes a suffix [NUMBER], to make it a unique name.  The
NUMBER order among window names that differ only by their [NUMBER] is
arbitrary.

Non-nil argument ALL-P means use windows from all visible frames.
Otherwise, use only windows from the selected frame."
  (lexical-let ((win-alist  ())
                (count      2)
                wname new-name)
    (walk-windows (lambda (w)           ; FREE here: COUNT, NEW-NAME, WIN-ALIST, WNAME.
                    (setq wname  (buffer-name (window-buffer w)))
                    (if (not (assoc wname win-alist))
                        (push (cons wname w) win-alist)
                      (setq new-name  wname)
                      (while (assoc new-name win-alist)
                        (setq new-name  (format "%s[%d]" wname count)
                              count     (1+ count)))
                      (push (cons new-name w) win-alist))
                    (setq count  2))
                  'no-mini
                  (if all-p 'visible 'this-frame))
    win-alist))

;;;###autoload (autoload 'icicle-delete-windows "icicles")
(icicle-define-command icicle-delete-windows ; Command name
  "Delete windows showing a buffer, anywhere." ; Doc string
  delete-windows-on                     ; Action function
  "Delete windows on buffer: "          ; `completing-read' args
  (let ((cand-bufs  ()))
    (dolist (buf  (buffer-list))
      (when (get-buffer-window buf 0) (push (list (buffer-name buf)) cand-bufs)))
    cand-bufs)
  nil t nil 'buffer-name-history (buffer-name (current-buffer)) nil
  ((icicle-use-candidates-only-once-flag  t) ; Bindings
   (icicle-inhibit-try-switch-buffer      t)
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "buffer")))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "buffer")))))

;;;###autoload (autoload 'icicle-delete-window "icicles")
(defun icicle-delete-window (bufferp)   ; Bound to `C-x 0' in Icicle mode.
  "`delete-window' or prompt for buffer and delete all its windows.
When called from the minibuffer, remove the `*Completions*' window.

Otherwise:
 With no prefix argument, delete the selected window.
 With a prefix argument, prompt for a buffer and delete all windows,
   on any frame, that show that buffer.

 With a prefix argument, this is an Icicles multi-command - see
 command `icicle-mode'.  Input-candidate completion and cycling are
 available.  While cycling, these keys with prefix `C-' are active\\<minibuffer-local-completion-map>:

 `C-RET'   - Act on current completion candidate only
 `C-down'  - Move to next completion candidate and act
 `C-up'    - Move to previous completion candidate and act
 `C-next'  - Move to next apropos-completion candidate and act
 `C-prior' - Move to previous apropos-completion candidate and act
 `C-end'   - Move to next prefix-completion candidate and act
 `C-home'  - Move to previous prefix-completion candidate and act
 `\\[icicle-all-candidates-action]'     - Act on *all* candidates (or all that are saved),
             successively (careful!)

 With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
 `C-M-return', `C-M-down', and so on) provide help about candidates.

 Use `mouse-2', `RET', or `S-RET' to finally choose a candidate,
 or `C-g' to quit.

By default, Icicle mode remaps all key sequences that are normally
bound to `delete-window' to `icicle-delete-window'.  If you do not
want this remapping, then customize option
`icicle-top-level-key-bindings'."
  (interactive "P")
  (if (window-minibuffer-p (selected-window))
      (icicle-remove-Completions-window)
    (if bufferp (icicle-delete-windows) (delete-window))))

;;;###autoload (autoload 'icicle-kill-buffer "icicles")
(icicle-define-command icicle-kill-buffer ; Bound to `C-x k' in Icicle mode.
  "Kill a buffer.
See `icicle-buffer' for more information, including about buffer-name
completion candidates, default values, and additional key bindings.

By default, Icicle mode remaps all key sequences that are normally
bound to `kill-buffer' to `icicle-kill-buffer'.  If you do not want
this remapping, then customize option
`icicle-top-level-key-bindings'."       ; Doc string
  icicle-kill-a-buffer-and-update-completions ; Action function
  (icicle-buffer-name-prompt "Kill")    ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) icicle-bufflist) nil ; `icicle-bufflist' is free.
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ; Emacs 23.
  nil 'buffer-name-history (buffer-name (current-buffer)) nil
  (icicle-buffer-bindings)              ; Bindings
  ;; Actually, there is no reason to bind `C-x m' to `icicle-bookmark-non-file-other-window' here,
  ;; but to keep things simple we do it anyway.
  (icicle-bind-buffer-candidate-keys)   ; First code
  nil                                   ; Undo code
  (icicle-unbind-buffer-candidate-keys)) ; Last code

(defun icicle-buffer-name-prompt (action &optional other-window-p)
  "Return prompt for buffer-name completion.
ACTION is the command action, a string.  It starts the prompt."
  (concat  (cond ((null current-prefix-arg)
                  (format "%s buffer" action)) ; `completing-read' args
                 ((and (consp current-prefix-arg)  (fboundp 'derived-mode-p)) ; `C-u'
                  (format "%s buffer with same or ancestor mode" action))
                 ((zerop (prefix-numeric-value current-prefix-arg)) ; `C-0'
                  (format "%s buffer with same mode" action))
                 ((< (prefix-numeric-value current-prefix-arg) 0) ; `C--'
                  (format "%s buffer for same frame" action))
                 (t                     ; `C-1'
                  (format "%s file buffer" action)))
           (and other-window-p  " in other window")
           ": "))

(defun icicle-kill-a-buffer-and-update-completions (buf)
  "Kill buffer BUF and update the set of completions."
  (setq buf  (get-buffer buf))
  (if buf
      (icicle-condition-case-no-debug err
          (if (not (buffer-live-p buf))
              (message "Buffer already deleted: `%s'" buf)
            (if (fboundp 'kill-buffer-and-its-windows)
                (kill-buffer-and-its-windows buf) ; Defined in `misc-cmds.el'.
              (kill-buffer buf))
            ;; Update the set of completions, then update `*Completions*'.
            (setq minibuffer-completion-table  (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list)))
            (icicle-complete-again-update))
        (error nil))
    (message "No such live buffer: `%s'" buf)))


(put 'icicle-buffer 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-buffer "icicles")
(icicle-define-command icicle-buffer    ; Bound to `C-x b' in Icicle mode.
  "Switch to a different buffer, whose content contains a regexp match.
By default, Icicle mode remaps all key sequences that are normally
bound to `switch-to-buffer' to `icicle-buffer'.  If you do not want
this remapping, then customize option `icicle-top-level-key-bindings'.

Completion candidates are two-part multi-completions, with the second
part optional.  If both parts are present they are separated by
`icicle-list-join-string' (\"^G^J\", by default).

The first part is matched as a regexp against a buffer name.
The second part is matched as a regexp against buffer content.
Candidates that do not match are filtered out.

Your minibuffer input can match a buffer name or buffer content, or
both.  Use \\<minibuffer-local-completion-map>`C-M-j' (equivalent here to `C-q C-g C-j') to input the
default separator.

For example:

To match `foo' against buffer names, use input `foo'.
To match `bar' against buffer contents, use input `C-M-j bar'.
To match both, use input `foo C-M-j bar'.

Only the matching buffer names are shown in *Completions*, and only
the chosen buffer name is returned.  The actual content matches are
unimportant anyway: content matching is used only to filter
candidates.

This is a buffer-switching command.  If you instead want to navigate
to text searched for in buffers then use `icicle-search'.

The buffer-name portion of completion candidates is as follows,
depending on the prefix arg:

* No prefix arg: all buffers
* Numeric arg > 0: buffers visiting files or directories (Dired)
* Numeric arg < 0: buffers associated with the selected frame
* Numeric arg = 0: buffers with the same mode as the current buffer
* Plain prefix arg (`C-u'): buffers with the same mode as current, or
  with a mode that the current mode is derived from

For Emacs 23 and later, the default values (via `M-n') are the
\(buffer-name components of the) first four completion candidates
\(respecting the prefix argument).

You can use these additional keys during completion:

* `C-x F'     Toggle including cached file names as candidates (option
              `icicle-buffer-include-cached-files-nflag').
* `C-x R'     Toggle including recent file names as candidates (option
              `icicle-buffer-include-recent-files-nflag').
* `C-x m'     Visit a bookmarked buffer (only if you use Bookmark+).
* `C-x C-m -' Remove candidate buffers whose mode is derived from a
              given mode.  Repeatable.  (`C-m' = `RET'.)
* `C-x M -'   Remove buffers in a given mode.  Repeatable.
* `C-x C-m +' Keep only buffers in a mode derived from a given mode.
* `C-x M +'   Keep only buffers in a given mode.
* `\\[icicle-delete-candidate-object]'  Kill the buffer named by a completion candidate.

These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-extras'             - Extra buffers to display
 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-include-cached-files-nflag' - Include cached files
 `icicle-buffer-include-recent-files-nflag' - Include recent files
 `icicle-buffer-match-regexp'       - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'    - Regexp buffers must not match
 `icicle-buffer-predicate'          - Predicate buffer names satisfy
 `icicle-buffer-sort'               - Sort function for candidates

For example, to change the default behavior to show only buffers that
are associated with files, set `icicle-buffer-predicate' to this:

 (lambda (buf) (buffer-file-name buf))

Option `icicle-buffer-require-match-flag' can be used to override
option `icicle-require-match-flag'.

Option `icicle-buffers-ido-like' non-nil gives this command a more
Ido-like behavior.

See also command `icicle-buffer-no-search', which is `icicle-buffer'
without the multi-completion behavior that searches buffer content.

See also command `icicle-buffer-config', which lets you choose a
configuration of user options for commands such as `icicle-buffer'.

Note: The prefix arg is tested, even when this is called
noninteractively.  Lisp code can bind `current-prefix-arg' to control
the behavior."                          ; Doc string
  (lambda (buf)                         ; Action function
    (when (and (not (get-buffer buf))  (member buf icicle-buffer-easy-files))
      (setq buf  (find-file-noselect buf)))
    (switch-to-buffer buf))        
  prompt 'icicle-buffer-multi-complete nil ;  `completing-read' args
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ; Emacs 23.
  nil 'buffer-name-history (icicle-default-buffer-names current-prefix-arg) nil
  (icicle-buffer-bindings               ; Bindings
   ((prompt                                 (icicle-buffer-name-prompt "Switch to"))
    (icicle-show-multi-completion-flag      t) ; Override user setting.
    (icicle-multi-completing-p              t)
    (icicle-list-use-nth-parts              '(1))

    (icicle-candidate-help-fn               (lambda (cand)
                                              (setq cand  (icicle-transform-multi-completion cand))
                                              (when (and (bufferp (get-buffer cand))
                                                         (with-current-buffer cand
                                                           (if (fboundp 'describe-buffer) ; In `help-fns+.el'.
                                                               (describe-buffer)
                                                             (describe-mode)) t)))))
    (icicle-buffer-easy-files               ()))
   ((icicle-buffer-complete-fn              'icicle-buffer-multi-complete)
    ;; Bind `icicle-apropos-complete-match-fn' to nil to prevent automatic input matching in
    ;; `icicle-unsorted-apropos-candidates' etc., because `icicle-buffer-multi-complete' does everything.
    (icicle-apropos-complete-match-fn       nil)
    (icicle-last-apropos-complete-match-fn  'icicle-buffer-apropos-complete-match)
    ;; `icicle-bufflist' is FREE here.
    (icicle-bufflist                        (setq icicle-bufflist
                                                  (delete icicle-orig-buff icicle-bufflist)))))
  (progn (icicle-bind-buffer-candidate-keys)
         (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
         (icicle-highlight-lighter)
         (message "Matching buffer contents..."))
  nil                                   ; Undo code
  (icicle-unbind-buffer-candidate-keys)) ; Last code

;; Free var here: `icicle-bufflist' is bound by `icicle-buffer-bindings'.
(defun icicle-default-buffer-names (&optional arg)
  "Default buffer names (Emacs 23+) or name (< Emacs 23).
For Emacs 23+, up to four names are returned.

Optional ARG is used only for Emacs 23+.  Its meaning is the same as
the prefix argument in Icicles buffer commands:
 * nil       :  all buffers
 * Number > 0: buffers visiting files or directories (Dired)
 * Number < 0: buffers associated with the selected frame
 * Number = 0: buffers with the same mode as the current buffer
 * Cons      : buffers with the same mode as current, or with
               a mode that the current mode is derived from"
  (if (< emacs-major-version 23)
      (let ((bname  (buffer-name (if (fboundp 'another-buffer) ; In `misc-fns.el'.
                                     (another-buffer nil t)
                                   (other-buffer (current-buffer))))))
        (if (and icicle-bufflist  (not (member bname icicle-bufflist)))
            (car icicle-bufflist)
          bname))
    ;; Emacs 23 accepts a list of default values.  ; Just keep the first 4.  (This could be an option.)
    (let* ((bfnames  (mapcar #'buffer-name (delete (current-buffer) (or icicle-bufflist  (buffer-list))))))
      (when icicle-buffer-ignore-space-prefix-flag
        (setq bfnames  (icicle-remove-if (lambda (bfname) (icicle-string-match-p "^ " bfname)) bfnames)))
      (icicle-first-N 4 bfnames))))


(put 'icicle-buffer-other-window 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-buffer-other-window "icicles")
(icicle-define-command icicle-buffer-other-window ; Bound to `C-x 4 b' in Icicle mode.
  "Switch to a buffer whose content matches a regexp, in another window.
Same as `icicle-buffer' except it uses a different window." ; Doc string
  (lambda (buf)                         ; Action function
    (when (and (not (get-buffer buf))  (member buf icicle-buffer-easy-files))
      (setq buf  (find-file-noselect buf)))
    (switch-to-buffer-other-window buf))        
  prompt 'icicle-buffer-multi-complete nil ; `completing-read' args
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ; Emacs 23.
  nil 'buffer-name-history (icicle-default-buffer-names current-prefix-arg) nil
  (icicle-buffer-bindings               ; Bindings
   ((prompt                                 (icicle-buffer-name-prompt "Switch to"))
    (icicle-show-multi-completion-flag      t) ; Override user setting.
    (icicle-multi-completing-p              t)
    (icicle-list-use-nth-parts              '(1))
    (icicle-candidate-help-fn               (lambda (cand)
                                              (setq cand  (icicle-transform-multi-completion cand))
                                              (when (and (bufferp (get-buffer cand))
                                                         (with-current-buffer cand
                                                           (if (fboundp 'describe-buffer) ; In `help-fns+.el'.
                                                               (describe-buffer)
                                                             (describe-mode)) t)))))
    (icicle-buffer-easy-files               ()))
   ((icicle-buffer-complete-fn              'icicle-buffer-multi-complete)
    ;; Bind `icicle-apropos-complete-match-fn' to nil to prevent automatic input matching in
    ;; `icicle-unsorted-apropos-candidates' etc., because `icicle-buffer-multi-complete' does everything.
    (icicle-apropos-complete-match-fn       nil)
    (icicle-last-apropos-complete-match-fn  'icicle-buffer-apropos-complete-match)
    ;; `icicle-bufflist' is FREE here.
    (icicle-bufflist                        (setq icicle-bufflist
                                                  (delete icicle-orig-buff icicle-bufflist)))))
  (progn (icicle-bind-buffer-candidate-keys)
         (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
         (icicle-highlight-lighter)
         (message "Matching buffer contents..."))
  nil                                   ; Undo code
  (icicle-unbind-buffer-candidate-keys)) ; Last code

(defun icicle-buffer-multi-complete (strg pred completion-mode)
  "Completion function for `icicle-buffer'.
Used as the value of `icicle-buffer-complete-fn' and hence as
`minibuffer-completion-table'."
  (setq strg  icicle-current-input)
  (lexical-let* ((name-pat     (let ((icicle-list-use-nth-parts  '(1)))
                                 (icicle-transform-multi-completion strg)))
                 (name-pat     (if (memq icicle-current-completion-mode '(nil apropos))
                                   name-pat
                                 (concat "^" name-pat)))
                 (content-pat  (let ((icicle-list-use-nth-parts  '(2)))
                                 (icicle-transform-multi-completion strg)))
                 (bufs         (mapcar (lambda (buf) (buffer-name buf)) icicle-bufflist))
                 (bufs         (if icicle-buffer-ignore-space-prefix-flag
                                   (icicle-remove-if (lambda (buf) (icicle-string-match-p "^ " buf)) bufs)
                                 bufs))
                 (bufs         (icicle-remove-if-not (lambda (buf) (icicle-string-match-p name-pat buf)) bufs))

                 (bufs         (cond ((equal "" content-pat)
                                      (dolist (buf  bufs)
                                        ;; Free vars here: EXISTING-BUFFERS, NEW-BUFS--TO-KILL
                                        (when (and (boundp 'existing-bufs)  (boundp 'new-bufs--to-kill)
                                                   (not (memq (setq buf  (get-buffer buf)) existing-bufs)))
                                          (add-to-list 'new-bufs--to-kill buf)))
                                      bufs)
                                     (t
                                      (icicle-remove-if-not
                                       (lambda (buf)
                                         (prog1 (with-current-buffer buf
                                                  (save-excursion (goto-char (point-min))
                                                                  (re-search-forward content-pat nil t)))
                                           ;; Free vars here: EXISTING-BUFFERS, NEW-BUFS--TO-KILL
                                           (when (and (boundp 'existing-bufs)  (boundp 'new-bufs--to-kill)
                                                      (not (memq (setq buf  (get-buffer buf)) existing-bufs)))
                                             (add-to-list 'new-bufs--to-kill buf))))
                                       bufs))))
                 (filnames    (and (> icicle-buffer-include-recent-files-nflag 0)
                                   (require 'recentf nil t)
                                   (or recentf-list  (recentf-load-list))
                                   (icicle-recent-files-without-buffers bufs)))
                 (filnames     (append filnames (and (> icicle-buffer-include-cached-files-nflag 0)
                                                     (icicle-cached-files-without-buffers bufs))))
                 (filnames     (icicle-remove-if-not (lambda (fil) (icicle-string-match-p name-pat fil))
                                                     filnames))
                 (filnames     (if (equal "" content-pat)
                                   filnames
                                 (icicle-remove-if-not
                                  (lambda (filname)
                                    ;; Avoid the error raised by calling `find-file-noselect' on a directory
                                    ;; when `find-file-run-dired' is nil.
                                    (and (or find-file-run-dired  (not (file-directory-p filname)))
                                         (let* ((buf    (find-file-noselect filname))
                                                (found  (with-current-buffer buf
                                                          (message "Matching buffer contents...")
                                                          (save-excursion
                                                            (goto-char (point-min))
                                                            (re-search-forward content-pat nil t)))))
                                           ;; Free vars here: EXISTING-BUFFERS, NEW-BUFS--TO-KILL
                                           (when (and (boundp 'existing-bufs)  (boundp 'new-bufs--to-kill)
                                                      (not (memq buf existing-bufs)))
                                             (add-to-list 'new-bufs--to-kill buf))
                                           found)))
                                  filnames))))
    ;; `icicle-buffer-easy-files' is FREE here - bound in `icicle-buffer(-other-window)'.
    (setq bufs  (append bufs (setq icicle-buffer-easy-files  filnames)))
    (cond ((and (eq 'metadata completion-mode)  (> emacs-major-version 23))
           '(metadata (category . buffer)))
          (completion-mode
           bufs) ; `all-completions', `test-completion'
          (t
           (try-completion              ; `try-completion'
            strg (mapcar #'list bufs) (and pred  (lambda (ss) (funcall pred ss))))))))

(defun icicle-buffer-apropos-complete-match (input buffer)
  "Match function for progressive completion with `icicle-buffer'.
Return non-nil if the current multi-completion INPUT matches BUFFER.
BUFFER is a buffer name."
  (lexical-let* ((name-pat     (let ((icicle-list-use-nth-parts  '(1)))
                                 (icicle-transform-multi-completion input)))
                 (content-pat  (let ((icicle-list-use-nth-parts  '(2)))
                                 (icicle-transform-multi-completion input))))
    (and (icicle-string-match-p name-pat buffer)
         (or (equal "" content-pat)
             (with-current-buffer buffer
               (save-excursion (goto-char (point-min))
                               (re-search-forward content-pat nil t)))))))

(defun icicle-cached-files-without-buffers (buffers)
  "Return absolute file-name list represented by `file-cache-alist'.
Do not include any files that are already visited in BUFFERS, which is
a list of buffer names.  Return only the first
`icicle-buffer-include-cached-files-nflag' names."
  (and (boundp 'file-cache-alist)
       file-cache-alist
       (catch 'icicle-cached-files-without-buffers
         (let ((result     ())
               (buf-files  (mapcar #'buffer-file-name (mapcar #'get-buffer buffers)))
               file)
           (dolist (file+dirs  file-cache-alist)
             (setq file  (car file+dirs))
             (dolist (dir  (cdr file+dirs))
               (setq file  (concat (or dir  default-directory) file))
               (unless (member file buf-files) (push file result))
               (when (>= (length result) icicle-buffer-include-cached-files-nflag)
                 (throw 'icicle-cached-files-without-buffers result))))
           result))))

(defun icicle-recent-files-without-buffers (buffers)
  "Return absolute file-name list represented by `recentf-list'.
Do not include any files that are already visited in BUFFERS, which is
a list of buffer names.  Return only the first
`icicle-buffer-include-recent-files-nflag' names."
  (and (boundp 'recentf-list)
       recentf-list
       (catch 'icicle-recent-files-without-buffers
         (let ((result     ())
               (buf-files  (mapcar #'buffer-file-name (mapcar #'get-buffer buffers)))
               file)
           (dolist (file  recentf-list)
             (unless (member file buf-files) (push file result))
             (when (>= (length result) icicle-buffer-include-recent-files-nflag)
               (throw 'icicle-recent-files-without-buffers result)))
           result))))


(put 'icicle-buffer-no-search 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-buffer-no-search "icicles")
(icicle-define-command icicle-buffer-no-search ; Not bound by default
  "Switch to a different buffer.
This is like command `icicle-buffer', but without the possibility of
searching buffer contents.  That is, completion candidates are just
buffer names, not multi-completions - they contain no buffer-content
part."                                  ; Doc string
  switch-to-buffer                      ; Action function
  (icicle-buffer-name-prompt "Switch to") ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) icicle-bufflist) nil ; `icicle-bufflist' is free.
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ; Emacs 23.
  nil 'buffer-name-history (icicle-default-buffer-names current-prefix-arg) nil
  (icicle-buffer-bindings               ; Bindings
   ()
   ((icicle-bufflist  (setq icicle-bufflist  (delete icicle-orig-buff icicle-bufflist)))))
  (icicle-bind-buffer-candidate-keys)   ; First code
  nil                                   ; Undo code
  (icicle-unbind-buffer-candidate-keys)) ; Last code


(put 'icicle-buffer-no-search-other-window 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-buffer-no-search-other-window "icicles")
(icicle-define-command icicle-buffer-no-search-other-window ; Not bound by default
  "Switch to a different buffer in another window.
Same as `icicle-buffer' except it uses a different window." ; Doc string
  switch-to-buffer-other-window         ; Action function
  (icicle-buffer-name-prompt "Switch to" t) ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) icicle-bufflist) nil ; `icicle-bufflist' is free.
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ; Emacs 23.
  nil 'buffer-name-history (icicle-default-buffer-names current-prefix-arg) nil
  (icicle-buffer-bindings               ; Bindings
   ()
   ((icicle-bufflist  (setq icicle-bufflist  (delete icicle-orig-buff icicle-bufflist)))))
  (icicle-bind-buffer-candidate-keys)   ; First code
  nil                                   ; Undo code
  (icicle-unbind-buffer-candidate-keys)) ; Last code

;;;###autoload (autoload 'icicle-visit-marked-file-of-content "icicles")
(icicle-define-command icicle-visit-marked-file-of-content ; Command name
  "Visit a marked file whose content matches a regexp.
The marked files are examined, and those whose file names and/or
contents match your multi-completion input are available as candidate
buffers to visit.  This command is like `icicle-buffer'.
See that command for more information.
You must be in Dired to use this command.

When this command is finished, any unused buffers that were created
for content matching are killed, if option
`icicle-kill-visited-buffers-flag' is non-nil.  But a prefix argument
flips the behavior specified by that option." ; Doc string
  (lambda (buf)                         ; Action function.  Free var here: NEW-BUFS--TO-KEEP.
    (push (switch-to-buffer (icicle-transform-multi-completion buf))
          new-bufs--to-keep))           ; Add the visited buffer to those we will keep (not kill).
  prompt 'icicle-buffer-multi-complete nil ; `completing-read' args
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ; Emacs 23.
  nil 'buffer-name-history (icicle-default-buffer-names current-prefix-arg) nil
  (icicle-buffer-bindings               ; Bindings
   ((prompt                             (icicle-buffer-name-prompt "Visit file"))
    (icicle-show-multi-completion-flag  t) ; Override user setting.
    (icicle-multi-completing-p          t)
    (icicle-list-use-nth-parts          '(1))
    ;; Bind `icicle-apropos-complete-match-fn' to nil to prevent automatic input matching in
    ;; `icicle-unsorted-apropos-candidates' etc., because `icicle-buffer-multi-complete' does everything.
    (icicle-apropos-complete-match-fn   nil)
    (init-pref-arg                      current-prefix-arg)
    (existing-bufs                      (buffer-list))
    (new-bufs--to-kill                  ())
    (new-bufs--to-keep                  ())
    (icicle-candidate-help-fn           (lambda (cand)
                                          (setq cand  (icicle-transform-multi-completion cand))
                                          (when (and (bufferp (get-buffer cand))
                                                     (with-current-buffer cand
                                                       (if (fboundp 'describe-buffer) ; In `help-fns+.el'.
                                                           (describe-buffer)
                                                         (describe-mode)) t))))))
   ((icicle-buffer-complete-fn          'icicle-buffer-multi-complete)
    ;; `icicle-bufflist' is free here.
    (icicle-bufflist                    (save-excursion
                                          (let* ((files  (dired-get-marked-files
                                                          nil nil
                                                          (lambda (file) (not (file-directory-p file)))))
                                                 (bufs   ()))
                                            (dolist (file  files) (push (find-file-noselect file) bufs))
                                            bufs)))))
  (progn (unless (eq major-mode 'dired-mode) (error "Use this command only in Dired mode"))
         (icicle-bind-buffer-candidate-keys)
         (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
         (icicle-highlight-lighter)
         (message "Matching file contents..."))
  nil                                   ; Undo code
  (progn (icicle-unbind-buffer-candidate-keys) ; Last code
         (when (or (and init-pref-arg        (not icicle-kill-visited-buffers-flag))
                   (and (not init-pref-arg)  icicle-kill-visited-buffers-flag))
           (dolist (buf  new-bufs--to-kill)
             (unless (memq buf new-bufs--to-keep) (kill-buffer buf))))))

;;;###autoload (autoload 'icicle-visit-marked-file-of-content-other-window "icicles")
(icicle-define-command icicle-visit-marked-file-of-content-other-window ; Command name
  "Visit a marked file whose content matches a regexp, in another window.
Same as `icicle-visit-marked-file-of-content' except it uses a
different window.  You must be in Dired to use this command." ; Doc string
  (lambda (buf)                         ; Action function.  Free var here: NEW-BUFS--TO-KEEP.
    (push (switch-to-buffer-other-window (icicle-transform-multi-completion buf))
          new-bufs--to-keep))           ; Add the visited buffer to those we will keep (not kill).
  prompt 'icicle-buffer-multi-complete nil ; `completing-read' args
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ; Emacs 23.
  nil 'buffer-name-history (icicle-default-buffer-names current-prefix-arg) nil
  (icicle-buffer-bindings               ; Bindings
   ((prompt                             (icicle-buffer-name-prompt "Visit file"))
    (icicle-show-multi-completion-flag  t) ; Override user setting.
    (icicle-multi-completing-p          t)
    (icicle-list-use-nth-parts          '(1))
    ;; Bind `icicle-apropos-complete-match-fn' to nil to prevent automatic input matching in
    ;; `icicle-unsorted-apropos-candidates' etc., because `icicle-buffer-multi-complete' does everything.
    (icicle-apropos-complete-match-fn   nil)
    (init-pref-arg                      current-prefix-arg)
    (existing-bufs                      (buffer-list))
    (new-bufs--to-kill                  ())
    (new-bufs--to-keep                  ())
    (icicle-candidate-help-fn           (lambda (cand)
                                          (setq cand  (icicle-transform-multi-completion cand))
                                          (when (and (bufferp (get-buffer cand))
                                                     (with-current-buffer cand
                                                       (if (fboundp 'describe-buffer) ; In `help-fns+.el'.
                                                           (describe-buffer)
                                                         (describe-mode)) t))))))
   ((icicle-buffer-complete-fn          'icicle-buffer-multi-complete)
    ;; `icicle-bufflist' is free here.
    (icicle-bufflist                    (save-excursion
                                          (let* ((files  (dired-get-marked-files
                                                          nil nil
                                                          (lambda (file) (not (file-directory-p file)))))
                                                 (bufs   ()))
                                            (dolist (file  files) (push (find-file-noselect file) bufs))
                                            bufs)))))
  (progn (unless (eq major-mode 'dired-mode) (error "Use this command only in Dired mode"))
         (icicle-bind-buffer-candidate-keys)
         (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
         (icicle-highlight-lighter)
         (message "Matching file contents..."))
  nil                                   ; Undo code
  (progn (icicle-unbind-buffer-candidate-keys) ; Last code
         (when (or (and init-pref-arg        (not icicle-kill-visited-buffers-flag))
                   (and (not init-pref-arg)  icicle-kill-visited-buffers-flag))
           (dolist (buf  new-bufs--to-kill)
             (unless (memq buf new-bufs--to-keep) (kill-buffer buf))))))

;;;###autoload (autoload 'icicle-insert-buffer "icicles")
(icicle-define-command icicle-insert-buffer
  "Multi-command version of `insert-buffer'.
See `icicle-buffer' for more information, including about buffer-name
completion candidates, default values, and additional key bindings." ; Doc string
  insert-buffer                         ; Action function
  (icicle-buffer-name-prompt "Insert")  ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) icicle-bufflist) nil ; `icicle-bufflist' is free.
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ; Emacs 23.
  nil 'buffer-name-history (icicle-default-buffer-names current-prefix-arg) nil
  (icicle-buffer-bindings)              ; Bindings
  ;; Actually, there is no reason to bind `C-x m' to `icicle-bookmark-non-file-other-window' here,
  ;; but to keep things simple we do it anyway.
  (icicle-bind-buffer-candidate-keys)   ; First code
  nil                                   ; Undo code
  (icicle-unbind-buffer-candidate-keys)) ; Last code

;;;###autoload (autoload 'icicle-add-buffer-candidate "icicles")
(icicle-define-command icicle-add-buffer-candidate ; Command name
  "Add buffer as an always-show completion candidate.
Add the buffer to `icicle-buffer-extras'.  Save the updated option.
See `icicle-buffer' for more information, including about buffer-name
completion candidates, default values, and additional key bindings." ; Doc string
  ;; FREE here: ICICLE-BUFFER-EXTRAS, ICICLE-CUSTOMIZE-SAVE-VARIABLE-FUNCTION.
  (lambda (buf)
    (add-to-list 'icicle-buffer-extras buf) ; Action function
    (funcall icicle-customize-save-variable-function 'icicle-buffer-extras icicle-buffer-extras)
    (message "Buffer `%s' added to always-show buffers"
             (icicle-propertize buf 'face 'icicle-msg-emphasis)))
  (icicle-buffer-name-prompt "Show always") ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) icicle-bufflist) nil ; `icicle-bufflist' is free.
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ; Emacs 23.
  nil 'buffer-name-history (icicle-default-buffer-names current-prefix-arg) nil
  (icicle-buffer-bindings               ; Bindings
   ((icicle-delete-candidate-object        'icicle-remove-buffer-candidate-action) ; Override default (kill).
    (icicle-use-candidates-only-once-flag  t)))
  ;; Actually, there is no reason to bind `C-x m' to `icicle-bookmark-non-file-other-window' here,
  ;; but to keep things simple we do it anyway.
  (icicle-bind-buffer-candidate-keys)   ; First code
  nil                                   ; Undo code
  (icicle-unbind-buffer-candidate-keys)) ; Last code

;;;###autoload (autoload 'icicle-remove-buffer-candidate "icicles")
(icicle-define-command icicle-remove-buffer-candidate ; Command name
  "Remove buffer as an always-show completion candidate.
Remove the buffer from `icicle-buffer-extras'.
Save the updated option."               ; Doc string
  icicle-remove-buffer-candidate-action ; Action function
  "Remove buffer from always-show list: " ; `completing-read' args
  (mapcar #'list icicle-buffer-extras) nil t nil 'buffer-name-history (car icicle-buffer-extras) nil
  ((icicle-use-candidates-only-once-flag  t) ; Bindings
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "buffer")))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "buffer"))))
  (unless icicle-buffer-extras (error "`icicle-extra-buffers' is empty"))) ; First code

(defun icicle-remove-buffer-candidate-action (buf)
  "Action function for command `icicle-remove-buffer-candidate'."
  (setq icicle-buffer-extras  (delete buf icicle-buffer-extras))
  (funcall icicle-customize-save-variable-function 'icicle-buffer-extras icicle-buffer-extras)
  (message "Buffer `%s' removed from always-show buffers"
           (icicle-propertize buf 'face 'icicle-msg-emphasis)))

;;;###autoload (autoload 'icicle-buffer-config "icicles")
(icicle-define-command icicle-buffer-config ; Command name
  "Choose a configuration of user options for `icicle-buffer'.
You can use \\<minibuffer-local-completion-map>\
`\\[icicle-delete-candidate-object]' on any configuration during completion to
remove it.  See user option `icicle-buffer-configs'.
See also commands `icicle-add-buffer-config' and
`icicle-remove-buffer-config'."         ; Doc string
  ;; FREE here: ICICLE-BUFFER-CONFIGS, ICICLE-BUFFER-EXTRAS, ICICLE-BUFFER-MATCH-REGEXP,
  ;;            ICICLE-BUFFER-NO-MATCH-REGEXP, ICICLE-BUFFER-PREDICATE, ICICLE-BUFFER-SORT.
  (lambda (config-name)                 ; Action function
    (let ((config  (assoc config-name icicle-buffer-configs)))
      (setq icicle-buffer-match-regexp     (elt config 1)
            icicle-buffer-no-match-regexp  (elt config 2)
            icicle-buffer-predicate        (elt config 3)
            icicle-buffer-extras           (elt config 4)
            icicle-buffer-sort             (elt config 5))))
  "Configuration: " icicle-buffer-configs nil t nil ; `completing-read' args
  'icicle-buffer-config-history nil nil
  ((icicle-delete-candidate-object  'icicle-remove-buffer-config-action))) ; Bindings

;;;###autoload (autoload 'icicle-add-buffer-config "icicles")
(icicle-define-add-to-alist-command icicle-add-buffer-config ; Command name
  "Add buffer configuration to `icicle-buffer-configs'.
You are prompted for the buffer configuration components.
For the list of extra buffers to always display, you can choose them
using `C-mouse-2', `C-RET', and so on, just as you would make any
Icicles multiple choice."
  ;; FREE here: FUNCTION-NAME-HISTORY, ICICLE-BUFFER-NO-MATCH-REGEXP, ICICLE-BUFFER-PREDICATE,
  ;;            ICICLE-BUFFER-SORT.
  (lambda ()
    (let ((name            (read-from-minibuffer "Add buffer configuration.  Name: "))
          (match-regexp    (icicle-read-from-minibuf-nil-default
                            "Regexp to match: " nil nil nil 'regexp-history
                            icicle-buffer-match-regexp))
          (nomatch-regexp  (icicle-read-from-minibuf-nil-default
                            "Regexp not to match: " nil nil nil 'regexp-history
                            icicle-buffer-no-match-regexp))
          (pred            (icicle-read-from-minibuf-nil-default
                            "Predicate to satify: " nil nil nil
                            (if (boundp 'function-name-history)
                                'function-name-history
                              'icicle-function-name-history)
                            icicle-buffer-predicate))
          (sort-fn         (icicle-read-from-minibuf-nil-default
                            "Sort function: " nil nil t
                            (if (boundp 'function-name-history)
                                'function-name-history
                              'icicle-function-name-history)
                            (and icicle-buffer-sort  (symbol-name icicle-buffer-sort))))
          (extras          (let ((icicle-prompt   "Choose extra buffers to show (`RET' when done): "))
                             (icicle-buffer-list)))) ; Do last, for convenience.
      (list name match-regexp nomatch-regexp pred extras sort-fn)))
  icicle-buffer-configs)

;;;###autoload (autoload 'icicle-remove-buffer-config "icicles")
(icicle-define-command icicle-remove-buffer-config ; Command name
  "Remove buffer configuration from `icicle-buffer-configs'.
Save the updated option."               ; Doc string
  icicle-remove-buffer-config-action    ; Action function
  "Remove buffer configuration: "       ; `completing-read' args
  (mapcar (lambda (config) (list (car config))) icicle-buffer-configs)
  nil t nil 'icicle-buffer-config-history (caar icicle-buffer-configs) nil
  ((icicle-use-candidates-only-once-flag  t))) ; Bindings

(defun icicle-remove-buffer-config-action (config-name)
  "Action function for command `icicle-remove-buffer-config'."
  (setq icicle-buffer-configs  (icicle-assoc-delete-all config-name icicle-buffer-configs))
  (funcall icicle-customize-save-variable-function 'icicle-buffer-configs icicle-buffer-configs)
  (message "Buffer configuration `%s' removed"
           (icicle-propertize config-name 'face 'icicle-msg-emphasis)))

;;;###autoload (autoload 'icicle-color-theme "icicles")
(icicle-define-command icicle-color-theme ; Command name
  "Change color theme.
You can use \\<minibuffer-local-completion-map>\
`\\[icicle-delete-candidate-object]' during completion to remove the current
candidate from the list of color themes.

If you use `C-g' during this command, the previous color-theme
snapshot is used to restore that color theme.

Remember too that you can use the pseudo-theme [Reset] to restore the
last theme: `M-x color-theme-select [Reset]'.

By default, each time you invoke this command, a snapshot is first
made of the current color theme (or current colors, if no theme is
used).  Thus, by default, if you use `C-g', the colors restored are
those used before you changed themes using this command.

However, if you use a prefix arg, then this command takes no new
snapshot, unless no snapshot has ever been taken during this Emacs
session.  This can be useful when experimenting, to restore not to the
state just before this command invocation, but to some previous
snapshot.

To use this command, you must have loaded library `color-theme.el',
available from http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme." ; Doc string
  (lambda (theme)
    (when (string= "" theme) (error "No theme name entered (empty input)"))
    (funcall  (intern theme)))          ; Action function: just call the theme.
  "Theme: " icicle-color-themes nil t nil ; `completing-read' args
  (if (boundp 'color-theme-history) 'color-theme-history 'icicle-color-theme-history)
  nil nil
  ((icicle-delete-candidate-object  'icicle-color-themes) ; Bindings
   (prefix-arg                      current-prefix-arg))
  (progn (unless (prog1 (require 'color-theme nil t) ; First code
                   (when (and (fboundp 'color-theme-initialize)  (not color-theme-initialized))
                     ;; NOTE: We need the `icicle-condition-case-no-debug' because of a BUG in
                     ;; `directory-files' for Emacs 20.  Bug reported to `color-theme.el'
                     ;; maintainer 2009-11-22.  The problem is that the default value of
                     ;; `color-theme-libraries' concats `file-name-directory', which ends in `/',
                     ;; with `/themes', not with `themes'.  So the result is `...//themes'.
                     ;; That is tolerated by Emacs 21+ `directory-files', but not for Emacs 20.
                     ;; Until this `color-theme.el' bug is fixed, Emacs 20 users will need to
                     ;; manually load `color-theme-libraries.el'.
                     (icicle-condition-case-no-debug nil
                         (let ((color-theme-load-all-themes  t))
                           (color-theme-initialize)
                           (setq color-theme-initialized  t))
                       (error nil))))
           (error "This command requires library `color-theme.el'"))
         (unless icicle-color-themes
           (setq icicle-color-themes
                 (delete '("bury-buffer")
                         (mapcar (lambda (entry) (list (symbol-name (car entry))))
                                 color-themes)))) ; Free here, defined in `color-theme.el'.
         ;; Create the snapshot, if not available.  Do this so users can also undo using
         ;; pseudo-theme `[Reset]'.
         (when (or (not prefix-arg)
                   (not (assq 'color-theme-snapshot color-themes))
                   (not (commandp 'color-theme-snapshot)))
           (fset 'color-theme-snapshot (color-theme-make-snapshot))
           (setq color-themes  (delq (assq 'color-theme-snapshot color-themes) color-themes)
                 color-themes  (delq (assq 'bury-buffer color-themes) color-themes)
                 color-themes  (append '((color-theme-snapshot
                                          "[Reset]" "Undo changes, if possible.")
                                         (bury-buffer "[Quit]" "Bury this buffer."))
                                       color-themes))))
  (color-theme-snapshot))               ; Undo code


;; Make delete-selection mode recognize yanking, so it replaces region text.
(put 'icicle-yank-pop-commands 'delete-selection 'yank)

(defun icicle-yank-pop-commands (&optional arg) ; Bound to `M-y'.
  "`yank-pop', `yank-pop-secondary', or `icicle-completing-yank'.
Which of these is used depends on the previous command, as follows:

 * If the previous command was a yank-secondary command, then
   `yank-pop-secondary'.

 * Else if the previous command was a yank command (i.e. using the
   kill ring), then `yank-pop'.

 * Else `icicle-completing-yank'.

In the last case (`icicle-completing-yank'), during completion you can
use:

 * \\<minibuffer-local-completion-map>`\\[icicle-change-sort-order]' to sort the \
candidates to yank in different ways (repeat)
 * `\\[icicle-delete-candidate-object]' to remove a candidate entry from the selection ring
 * `\\[icicle-candidate-alt-action]' to copy a candidate to the other selection ring

You need library `second-sel.el' for this command."
  (interactive "p")
  (unless (featurep 'second-sel) (error "You need library `second-sel.el' for this command"))
  ;; Disable `browse-kill-ring's advice, since we handle such things here instead.
  (when (fboundp 'browse-kill-ring)
    (condition-case nil
        (ad-disable-advice 'yank-pop 'around 'kill-ring-browse-maybe)
      (error nil)))
  (cond ((memq last-command secondary-selection-yank-secondary-commands)
         (when buffer-read-only (error "Buffer is read-only: %S" (current-buffer)))
         (yank-pop-secondary arg))
        ((memq last-command secondary-selection-yank-commands)
         (when buffer-read-only (error "Buffer is read-only: %S" (current-buffer)))
         (yank-pop arg))
        (t
         (icicle-completing-yank)
         ;; Need to do this because `icicle-completing-yank' sets it to `yank'.
         (setq this-command  'icicle-yank-pop-commands))))


;; Make delete-selection mode recognize yanking, so it replaces region text.
(put 'icicle-completing-yank 'delete-selection 'yank)
;; Bound to `C-- C-y' via `icicle-yank-maybe-completing'.
;;;###autoload (autoload 'icicle-completing-yank "icicles")
(icicle-define-command icicle-completing-yank ; Bound to `M-y' unless previous command was a yank.
  "Yank an entry from a selection ring, choosing it using completion.
By default, the selection ring used is the kill ring.

If you also use library `browse-kill-ring+.el' or library
`second-sel.el' then an alternative selection ring is used if you
provide a prefix argument: `browse-kill-ring-alternative-ring' or
`secondary-selection-ring'.  This gives you a way to yank chosen items
from two different sets of selections.

When the kill ring is used, this is similar to `yank', but this does
not rotate the ring.  The mark is pushed first, so the yanked text
becomes the region.

During completion, you can use:

 * \\<minibuffer-local-completion-map>`\\[icicle-change-sort-order]' to sort the \
candidates to yank in different ways (repeat)
 * `\\[icicle-delete-candidate-object]' to remove a candidate entry from the selection ring
 * `\\[icicle-candidate-alt-action]' to copy a candidate to the other selection ring
   (requires `second-sel.el' or `browse-kill-ring+.el')" ; Doc string
  icicle-insert-for-yank                ; Action function
  "Insert: " (mapcar #'list kills-in-order) nil t nil 'icicle-kill-history ; `completing-read' args
  (car kills-in-order) nil
  ((icicle-transform-function       'icicle-remove-duplicates) ; Bindings
   (icicle-sort-comparer            nil)
   (selection-ring                  (if (not current-prefix-arg)
                                        'kill-ring
                                      (if (boundp 'browse-kill-ring-alternative-ring)
                                          browse-kill-ring-alternative-ring
                                        (if (boundp 'secondary-selection-ring)
                                            'secondary-selection-ring)
                                        'kill-ring)))
   (icicle-candidate-alt-action-fn  `(lambda (seln) ; Add selection to the front of the other ring.
                                      ;; FREE here: BROWSE-KILL-RING-ALTERNATIVE-PUSH-FUNCTION,
                                      ;;            BROWSE-KILL-RING-ALTERNATIVE-RING.
                                      (let ((other-ring  (if (eq 'kill-ring ',selection-ring)
                                                             (if (fboundp 'browse-kill-ring)
                                                                 browse-kill-ring-alternative-ring
                                                               (if (boundp 'secondary-selection-ring)
                                                                   'secondary-selection-ring
                                                                 nil))
                                                           'kill-ring)))
                                        (if (eq 'kill-ring ',selection-ring)
                                            (if (fboundp 'browse-kill-ring-alternative-push-function)
                                                (funcall browse-kill-ring-alternative-push-function
                                                         seln)
                                              (when (boundp 'secondary-selection-ring)
                                                (add-secondary-to-ring seln)))
                                          (kill-new seln))
                                        (icicle-msg-maybe-in-minibuffer
                                         (if (null other-ring)
                                             "No other selection ring"
                                           (format "Copied to `%s'" other-ring))))))
   (icicle-delete-candidate-object  selection-ring)
   (kills-in-order                  (if (eq selection-ring 'kill-ring)
                                        (append kill-ring-yank-pointer kill-ring ())
                                      (copy-sequence (symbol-value selection-ring))))))

(defun icicle-insert-for-yank (string)
  "`insert-for-yank', if defined; else, `insert' with `read-only' removed.
Pushes the mark first, so the inserted text becomes the region."
  (setq this-command  'yank)
  (push-mark)
  (if (fboundp 'insert-for-yank)        ; Defined in `subr.el' (not required).
      (insert-for-yank string)
    (let ((opoint  (point)))
      (insert string)
      (let ((inhibit-read-only  t)) (remove-text-properties opoint (point) '(read-only nil))))))


;; Make delete-selection mode recognize yanking, so it replaces region text.
(put 'icicle-yank-maybe-completing 'delete-selection 'yank)

;;;###autoload (autoload 'icicle-yank-maybe-completing "icicles")
(defun icicle-yank-maybe-completing (&optional arg) ;  Bound to `C-y' (or what `yank' was bound to).
  "`icicle-completing-yank', `icicle-yank', or `icicle-yank-function'.
If called from the minibuffer, call `icicle-yank'.
Otherwise:
 With a negative prefix argument, call `icicle-completing-yank'.
 Otherwise, call the value of user option `icicle-yank-function' (by
 default, `yank')."
  (interactive "*P")
  (if (window-minibuffer-p (selected-window))
      (icicle-yank arg)
    (if (wholenump (prefix-numeric-value arg))
        (funcall icicle-yank-function arg)
      (let ((current-prefix-arg  nil))  (icicle-completing-yank)))))

;;;###autoload (when (locate-library "proced") (autoload 'icicle-send-signal-to-process "icicles"))
(when (locate-library "proced")         ; Emacs 23+.
  (icicle-define-command icicle-send-signal-to-process
    "Send a signal to a system process.
Each candidate is a multi-completion with parts COMMAND, USER, and
PID, separated by `icicle-list-join-string' (\"^G^J\", by default).
 COMMAND is the system command associated with the process.
 USER is the user who issued COMMAND.
 PID is the process identifier.
You can match an input regexp against any combination of the parts.
You can use `C-M-j' (equivalent here to `C-q C-g C-j') to input the
default separator."
    (lambda (cand)                      ; ; FREE here: GET-ATTR, GET-PID, PROCED-SIGNAL-LIST.
      (let* ((process       (funcall get-pid cand))
             (process-name  (funcall get-attr cand 'comm))
             (sigcode       (let ((enable-recursive-minibuffers  t))
                              (completing-read
                               (format "Send signal to process %s: " process-name)
                               ;; `proced-signal-list' is free here.
                               proced-signal-list nil nil nil nil "TERM"))))
        (setq sigcode  (and (stringp sigcode)  (if (string-match "\\`[0-9]+\\'" sigcode)
                                                   (string-to-number sigcode)
                                                 (make-symbol sigcode))))
        (when sigcode (signal-process process sigcode))))
    prompt (mapcar (lambda (pid)
                     (let ((ats  (process-attributes pid)))
                       `((,(cdr (assoc 'comm ats)) ,(cdr (assoc 'user ats)) ,(number-to-string pid)))))
                   (list-system-processes))
    nil 'FORCE-MATCH-TO-PREVENT-ACCIDENTS nil nil nil nil
    ((prompt                             "COMMAND `C-M-j' USER `C-M-j' PID: ") ; Bindings
     (completion-ignore-case             t) ; For sorting.
     (icicle-candidate-properties-alist  ())
     (icicle-multi-completing-p          t)
     (icicle-list-use-nth-parts          '(3))
     (get-pid                            (lambda (cand) (string-to-number cand)))
     (get-attr                           (lambda (cand attr) ; FREE here: GET-PID.
                                           (cdr-safe (assoc attr (process-attributes
                                                                  (funcall get-pid cand))))))
     (get-user                           (lambda (cand) ; FREE here: GET-ATTR.
                                           (funcall get-attr cand 'user)))
     (icicle-candidate-help-fn           (lambda (cand) ; FREE here: GET-PID.
                                           (icicle-describe-process (funcall get-pid cand))))
     (icicle-transform-before-sort-p     t)
     (icicle-last-transform-function     nil) ; Because we bind `icicle-transform-function'.
     (icicle-transform-function          (lambda (cands)
                                           (let ((user-name  (user-login-name)))
                                             (loop for cand in cands
                                                   for user = (funcall ; FREE here: GET-USER.
                                                               get-user
                                                               (icicle-transform-multi-completion cand)) 
                                                   if (equal user-name user)
                                                   collect cand))))
     (icicle-sort-orders-alist           '(("by pid" .
                                            (lambda (s1 s2) ; FREE here: GET-PID.
                                              (< (funcall get-pid s1) (funcall get-pid s2))))
                                           ("by command name" .
                                            (lambda (s1 s2) ; FREE here: GET-ATTR.
                                              (string-lessp (upcase (funcall get-attr s1 'comm))
                                                            (upcase (funcall get-attr s2 'comm)))))
                                           ("by age" .
                                            (lambda (s1 s2) ; FREE here: GET-ATTR.
                                              (> (float-time (funcall get-attr s1 'start))
                                                 (float-time (funcall get-attr s2 'start)))))))
     (icicle-sort-comparer               (cdar icicle-sort-orders-alist)))
    (progn (unless (require 'proced nil t) (error "This command requires library `proced.el'")) ; First code
           (put-text-property 0 1 'icicle-fancy-candidates t prompt)
           (icicle-highlight-lighter)))

  (defun icicle-describe-process (pid)
    "Describe the system process that has process id PID."
    (interactive "nPID: ")
    (with-output-to-temp-buffer "*Help*"
      (let* ((attributes  (process-attributes pid))
             (comm        (cdr-safe (assoc 'comm attributes)))
             (args        (cdr-safe (assoc 'args attributes)))
             (start       (cdr-safe (assoc 'start attributes)))
             (user        (cdr-safe (assoc 'user attributes)))
             (state       (cdr-safe (assoc 'state attributes))))
        (princ (format "PID:\t\t%s\n" pid))
        (when comm  (princ (format "Command name:\t%s\n" comm)))
        (when args  (princ (format "Command line:\t%s\n" args)))
        (when user  (princ (format "User:\t\t%s\n"         user)))
        (when state  (princ (format "State:\t%s\n"       state)))
        (when start (princ (format-time-string "Started:\t%a %b %e %T %Y (%z)\n" start)))))))

;;;###autoload (autoload 'icicle-delete-file "icicles")
(icicle-define-file-command icicle-delete-file ; Command name
  "Delete a file or directory.
During completion (`*' means this requires library `Bookmark+')\\<minibuffer-local-completion-map>:
 *You can use `C-x a +' or `C-x a -' to add or remove tags from the
   current-candidate file.  You are prompted for the tags.
 *You can use `C-x m' to access file bookmarks (not just autofiles).
  You can use `C-c +' to create a new directory.
  You can use `\\[icicle-all-candidates-list-alt-action]' to open Dired \
on currently matching file names." ; Doc string
  (lambda (file)                        ; Function to perform the action
    (icicle-delete-file-or-directory file)
    (icicle-remove-candidate-display-others 'ALL))
  "Delete file or directory: " default-directory nil t nil nil ; `read-file-name' args
  (icicle-file-bindings)                ; Bindings
  (icicle-bind-file-candidate-keys)     ; First code
  nil                                   ; Undo code
  (icicle-unbind-file-candidate-keys))  ; Last code

(defun icicle-delete-file-or-directory (file)
  "Delete file or (empty) directory FILE."
  (icicle-condition-case-no-debug i-delete-file
      (if (eq t (car (file-attributes file)))
          (delete-directory file)
        (delete-file file))
    (error (message "%s" (error-message-string i-delete-file))
           (error "%s" (error-message-string i-delete-file)))))

;;;###autoload (autoload 'icicle-dired "icicles")
(icicle-define-file-command icicle-dired
  "Multi-command version of `dired'.
During completion (`*' means this requires library `Bookmark+')\\<minibuffer-local-completion-map>:
 *You can use `C-x a +' or `C-x a -' to add or remove tags from the
   current-candidate file.  You are prompted for the tags.
 *You can use `C-x m' to access file bookmarks (not just autofiles).
  You can use `C-c +' to create a new directory.
  You can use `\\[icicle-all-candidates-list-alt-action]' to open Dired on currently matching file names.
  You can use `\\[icicle-delete-candidate-object]' to delete a candidate file or (empty) dir." ; Doc string
  (lambda (dir) (dired dir switches))   ; FREE here: SWITCHES.
  "Dired (directory): " nil default-directory nil nil nil ; `read-file-name' args
  (icicle-file-bindings                 ; Bindings
   ((switches               (and current-prefix-arg
                                 (read-string "Dired listing switches: " dired-listing-switches)))
    (icicle-file-sort       (or icicle-file-sort  'icicle-dirs-first-p))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
  (icicle-bind-file-candidate-keys)     ; First code
  nil                                   ; Undo code
  (icicle-unbind-file-candidate-keys))  ; Last code

;;;###autoload (autoload 'icicle-dired-other-window "icicles")
(icicle-define-file-command icicle-dired-other-window
  "Same as `icicle-dired', except uses another window." ; Doc string
  (lambda (dir) (dired-other-window dir switches)) ; FREE here: SWITCHES.
  "Dired in other window (directory): " nil default-directory nil nil nil ; `read-file-name' args
  (icicle-file-bindings                 ; Bindings
   ((switches               (and current-prefix-arg
                                 (read-string "Dired listing switches: " dired-listing-switches)))
    (icicle-file-sort       (or icicle-file-sort  'icicle-dirs-first-p))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
  (icicle-bind-file-candidate-keys)     ; First code
  nil                                   ; Undo code
  (icicle-unbind-file-candidate-keys))  ; Last code


(put 'icicle-file 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-file "icicles")
(defun icicle-file (arg)                ; Bound to `C-x C-f' in Icicle mode.
  "Visit a file or directory.
With no prefix argument, use relative file names
 (`icicle-find-file').
With a prefix argument, use absolute file names
 (`icicle-find-file-absolute').
With a negative prefix argument, you can choose also by date:
 Completion candidates include the last modification date.

Note that when you use a prefix arg, completion matches candidates as
ordinary strings.  It knows nothing of file names per se.  In
particular, you cannot use remote file-name syntax if you use a prefix
argument.

During completion\\<minibuffer-local-completion-map>:
 You can use `C-x m' to access file bookmarks, if you use library
  `Bookmark+'.
 You can use `C-c +' to create a new directory.
 You can use `\\[icicle-all-candidates-list-alt-action]' to open Dired on the currently matching file names.
 You can use `\\[icicle-delete-candidate-object]' to delete a candidate file or (empty)
  directory.

By default, Icicle mode remaps all key sequences that are normally bound
to `find-file' to `icicle-file'.  If you do not want this remapping,
then customize option `icicle-top-level-key-bindings'."
  (interactive "P")
  (if arg
      (if (wholenump (prefix-numeric-value arg))
          (let ((current-prefix-arg  nil)) (icicle-find-file-absolute))
        (icicle-find-file-absolute))
    (icicle-find-file)))


(put 'icicle-file-other-window 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-file-other-window "icicles")
(defun icicle-file-other-window (arg)   ; Bound to `C-x 4 f' in Icicle mode.
  "Same as `icicle-file', except uses another window."
  (interactive "P")
  (if arg
      (if (wholenump (prefix-numeric-value arg))
          (let ((current-prefix-arg  nil)) (icicle-find-file-absolute-other-window))
        (icicle-find-file-absolute-other-window))
    (icicle-find-file-other-window)))


(put 'icicle-find-file-absolute 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-find-file-absolute "icicles")
(icicle-define-command icicle-find-file-absolute ; Bound to `C-u C-x f' in Icicle mode.
  "Visit a file or directory, given its absolute name.
Unlike `icicle-find-file', the completion candidates are absolute, not
relative, file names.

By default, the completion candidates are files in the current
directory, but you can substitute other candidates by retrieving a
saved candidate set.

Note that completion here matches candidates as ordinary strings.  It
knows nothing of file names per se.  In particular, you cannot use
remote file-name syntax.

Also, you cannot move up and down the file hierarchy the same way you
can for ordinary (non-absolute) file-name completion.  To change to a
different directory, with its files as candidates, use \\<minibuffer-local-completion-map>`C-c C-d' from
the minibuffer - it prompts you for the new directory.

Remember that you can use `C-x .' to hide the common match portion of
each candidate.  That can be particularly helpful for files that are
in a common directory.

With a prefix argument, you can choose also by date: Completion
candidates include the last modification date.

During completion (`*' means this requires library `Bookmark+'):

 *You can use `C-x a +' or `C-x a -' to add or remove tags from the
   current-candidate file.  You are prompted for the tags.
 *You can use `C-x m' to access file bookmarks (not just autofiles).
  You can use `C-c C-d' (a la `cd') to change the `default-directory'.
  You can use `C-c +' to create a new directory.
  You can use `\\[icicle-all-candidates-list-alt-action]' to open Dired on currently matching file names.
  You can use `\\[icicle-delete-candidate-object]' to delete a candidate file or (empty) dir.

These options, when non-nil, control candidate matching and filtering:

 `icicle-file-extras'           - Extra file names to display
 `icicle-file-match-regexp'     - Regexp that file names must match
 `icicle-file-no-match-regexp'  - Regexp file names must not match
 `icicle-file-predicate'        - Predicate file names must satisfy
 `icicle-file-sort'             - Sort function for candidates

For example, to show only names of files larger than 5000 bytes, set
`icicle-file-predicate' to:

  (lambda (file) (and (numberp (nth 7 (file-attributes file)))
                      (> (nth 7 (file-attributes file)) 5000)))

Option `icicle-file-require-match-flag' can be used to override
option `icicle-require-match-flag'.

Option `icicle-files-ido-like' non-nil gives this command a more
Ido-like behavior."                     ; Doc string
  (lambda (f) (find-file (icicle-transform-multi-completion f) 'WILDCARDS)) ; Action function
  prompt icicle-abs-file-candidates nil ; `completing-read' args
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ;Emacs 23.
  nil 'file-name-history default-directory nil
  (icicle-file-bindings                 ; Bindings
   ((prompt                             "File or dir (absolute): ")
    (icicle-full-cand-fn                `(lambda (file)
                                          (setq file  (if (file-directory-p file)
                                                          (file-name-as-directory file)
                                                        file))
                                          ,(if current-prefix-arg
                                               '(icicle-make-file+date-candidate file)
                                               '(list file))))
    (icicle-abs-file-candidates         (mapcar icicle-full-cand-fn
                                                (directory-files default-directory 'FULL nil 'NOSORT)))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))
    (icicle-special-candidate-regexp    (or icicle-special-candidate-regexp  ".+/$"))
    (icicle-candidate-properties-alist  (and current-prefix-arg  '((1 (face icicle-candidate-part)))))
    (icicle-multi-completing-p          current-prefix-arg)
    (icicle-list-use-nth-parts          (and current-prefix-arg  '(1)))))
  (progn                                ; First code
    (when current-prefix-arg (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (icicle-highlight-lighter)
    (message "Gathering files...")
    (icicle-bind-file-candidate-keys)
    (define-key minibuffer-local-completion-map "\C-c\C-d" 'icicle-cd-for-abs-files)
    (define-key minibuffer-local-must-match-map "\C-c\C-d" 'icicle-cd-for-abs-files))
  nil                                   ; Undo code
  (progn (icicle-unbind-file-candidate-keys) ; Last code
         (define-key minibuffer-local-completion-map "\C-c\C-d" nil)
         (define-key minibuffer-local-must-match-map "\C-c\C-d" nil)))


(put 'icicle-find-file-absolute-other-window 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-find-file-absolute-other-window "icicles")
(icicle-define-command icicle-find-file-absolute-other-window ; Bound to `C-u C-x 4 f'
  "Same as `icicle-find-file-absolute' except uses another window." ; Doc string
  (lambda (f) (find-file-other-window (icicle-transform-multi-completion f) 'WILDCARDS)) ; Action
  prompt icicle-abs-file-candidates nil ; `completing-read' args
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ;Emacs 23.
  nil 'file-name-history default-directory nil
  (icicle-file-bindings                 ; Bindings
   ((prompt                             "File or dir (absolute): ")
    (icicle-full-cand-fn                `(lambda (file)
                                          (setq file  (if (file-directory-p file)
                                                          (file-name-as-directory file)
                                                        file))
                                          ,(if current-prefix-arg
                                               '(icicle-make-file+date-candidate file)
                                               '(list file))))
    (icicle-abs-file-candidates         (mapcar icicle-full-cand-fn
                                                (directory-files default-directory 'FULL nil 'NOSORT)))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))
    (icicle-special-candidate-regexp    (or icicle-special-candidate-regexp  ".+/$"))
    (icicle-candidate-properties-alist  (and current-prefix-arg  '((1 (face icicle-candidate-part)))))
    (icicle-multi-completing-p          current-prefix-arg)
    (icicle-list-use-nth-parts          (and current-prefix-arg  '(1)))))
  (progn                                ; First code
    (when current-prefix-arg (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (icicle-highlight-lighter)
    (message "Gathering files...")
    (icicle-bind-file-candidate-keys)
    (define-key minibuffer-local-completion-map "\C-c\C-d" 'icicle-cd-for-abs-files)
    (define-key minibuffer-local-must-match-map "\C-c\C-d" 'icicle-cd-for-abs-files))
  nil                                   ; Undo code
  (progn (icicle-unbind-file-candidate-keys) ; Last code
         (define-key minibuffer-local-completion-map "\C-c\C-d" nil)
         (define-key minibuffer-local-must-match-map "\C-c\C-d" nil)))

;; This is a minibuffer command.  It is in this file because it is used only here.
;;
;;;###autoload (autoload 'icicle-cd-for-abs-files "icicles")
(defun icicle-cd-for-abs-files (dir)    ; Bound to `C-c C-d' in minibuffer for abs file completion.
  "Change `default-directory' during `icicle-find-file-absolute'."
  (interactive
   ;; Should not need to bind `minibuffer-completion-predicate'.  Emacs 23.2 bug, per Stefan.
   (let ((enable-recursive-minibuffers     t)
         (minibuffer-completion-predicate  minibuffer-completion-predicate))
     (list (funcall (if (fboundp 'read-directory-name)
                        #'read-directory-name
                      #'read-file-name)
                    "Change default directory: " default-directory (icicle-file-name-directory-w-default
                                                                    (icicle-input-from-minibuffer))
                    (and (member cd-path '(nil ("./")))  (null (getenv "CDPATH")))))))
  (cd dir)
  (let ((icicle-abs-file-candidates
         (mapcar (lambda (file)
                   (setq file  (if (file-directory-p file) (file-name-as-directory file) file))
                   (if icicle-multi-completing-p (icicle-make-file+date-candidate file) (list file)))
                 (directory-files default-directory 'full nil 'nosort))))
    (setq minibuffer-completion-table
          (car (icicle-mctize-all icicle-abs-file-candidates minibuffer-completion-predicate)))))


(put 'icicle-find-file 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-find-file "icicles")
(icicle-define-file-command icicle-find-file
  "Visit a file or directory.
\(Option `find-file-run-dired' determines whether you can actually
visit a directory candidate that you choose.)

If you use a prefix argument when you act on a completion candidate,
then you visit the file or dir in read-only mode.  This includes when
you act on all candidates using \\<minibuffer-local-completion-map>\
`\\[icicle-all-candidates-action]': precede the `\\[icicle-all-candidates-action]' with a prefix
arg.

If you use a prefix arg for the command itself, this reverses the
effect of using a prefix arg on individual candidates.  That is, with
a prefix arg for the command, files are visited in read-only mode by
default and a prefix arg for an individual file visits it without
read-only mode.

During completion (`*' means this requires library `Bookmark+'):

 *You can use `C-x a +' or `C-x a -' to add or remove tags from the
   current-candidate file.  You are prompted for the tags.
 *You can use `C-x m' to access file bookmarks (not just autofiles).
  You can use `C-c +' to create a new directory.
  You can use `\\[icicle-all-candidates-list-alt-action]' to open Dired on currently matching file names.
  You can use `\\[icicle-delete-candidate-object]' to delete a candidate file or (empty) dir.

These options, when non-nil, control candidate matching and filtering:

 `icicle-file-extras'           - Extra absolute file names to display
 `icicle-file-match-regexp'     - Regexp that file names must match
 `icicle-file-no-match-regexp'  - Regexp file names must not match
 `icicle-file-predicate'        - Predicate file names must satisfy
 `icicle-file-sort'             - Sort function for candidates

For example, to show only names of files larger than 5000 bytes, set
`icicle-file-predicate' to:

  (lambda (file) (and (numberp (nth 7 (file-attributes file)))
                      (> (nth 7 (file-attributes file)) 5000)))

Option `icicle-file-require-match-flag' can be used to override
option `icicle-require-match-flag'.

Option `icicle-files-ido-like' non-nil gives this command a more
Ido-like behavior."                     ; Doc string
  (lambda (file)                        ; FREE here: CURRENT-PREFIX-ARG, INIT-PREF-ARG, THIS-COMMAND.
    (let* ((r-o  (if (memq this-command '(icicle-candidate-action icicle-all-candidates-action))
                     (or (and init-pref-arg        (not current-prefix-arg))
                         (and (not init-pref-arg)  current-prefix-arg))
                   init-pref-arg))
           (fn   (if r-o 'find-file-read-only 'find-file)))
      (funcall fn file 'WILDCARDS)))
  (concat "File or directory" (and init-pref-arg  " (read-only)") ": ") ; `read-file-name' args
  nil (if (and (eq major-mode 'dired-mode)  (fboundp 'dired-get-file-for-visit)) ; Emacs 22+.
          (condition-case nil           ; E.g. error because not on file line (ignore)
              (abbreviate-file-name (dired-get-file-for-visit))
            (error nil))
        default-directory)
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ;Emacs 23.
  nil nil
  (icicle-file-bindings                 ; Bindings
   ((init-pref-arg  current-prefix-arg)
    (icicle-all-candidates-list-alt-action-fn ; `M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
  (icicle-bind-file-candidate-keys)     ; First code
  nil                                   ; Undo code
  (icicle-unbind-file-candidate-keys))  ; Last code

;;;###autoload (autoload 'icicle-find-file-other-window "icicles")
(icicle-define-file-command icicle-find-file-other-window
  "Same as `icicle-find-file', except uses another window." ; Doc string
  (lambda (file)                        ; FREE here: CURRENT-PREFIX-ARG, INIT-PREF-ARG, THIS-COMMAND.
    (let* ((r-o  (if (memq this-command '(icicle-candidate-action icicle-all-candidates-action))
                     (or (and init-pref-arg        (not current-prefix-arg))
                         (and (not init-pref-arg)  current-prefix-arg))
                   init-pref-arg))
           (fn   (if r-o 'find-file-read-only-other-window 'find-file-other-window)))
      (funcall fn file 'WILDCARDS)))
  (concat "File or directory" (and init-pref-arg  " (read-only)") ": ") ; `read-file-name' args
  nil (if (and (eq major-mode 'dired-mode)  (fboundp 'dired-get-file-for-visit)) ; Emacs 22+.
          (condition-case nil           ; E.g. error because not on file line (ignore)
              (abbreviate-file-name (dired-get-file-for-visit))
            (error nil))
        default-directory)
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ;Emacs 23.
  nil nil
  (icicle-file-bindings                 ; Bindings
   ((init-pref-arg  current-prefix-arg)
    (icicle-all-candidates-list-alt-action-fn ; `M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
  (icicle-bind-file-candidate-keys)     ; First code
  nil                                   ; Undo code
  (icicle-unbind-file-candidate-keys))  ; Last code


(put 'icicle-find-file-read-only 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-find-file-read-only "icicles")
(defun icicle-find-file-read-only ()    ; Bound to `C-x C-r' in Icicle mode.
  "Visit a file or directory in read-only mode.
If you use a prefix argument when you act on a candidate file name,
then visit the file without read-only mode.

If you use a prefix arg for the command itself, this reverses the
effect of using a prefix arg on individual candidates.  That is, with
a prefix arg for the command, files are not visited in read-only mode
by default and a prefix arg for an individual file visits it in
read-only mode.

During completion (`*' means this requires library `Bookmark+')\\<minibuffer-local-completion-map>:

 *You can use `C-x a +' or `C-x a -' to add or remove tags from the
   current-candidate file.  You are prompted for the tags.
 *You can use `C-x m' to access file bookmarks (not just autofiles).
  You can use `C-c +' to create a new directory.
  You can use `\\[icicle-all-candidates-list-alt-action]' to open Dired on currently matching file names.
  You can use `\\[icicle-delete-candidate-object]' to delete a candidate file or (empty) dir."
  (interactive)
  (let ((current-prefix-arg  (not current-prefix-arg)))
    (icicle-find-file)))

;;;###autoload (autoload 'icicle-find-file-read-only-other-window "icicles")
(defun icicle-find-file-read-only-other-window () ; Bound to `C-x 4 r' in Icicle mode.
  "Same as `icicle-find-file-read-only' except uses another window."
  (interactive)
  (let ((current-prefix-arg  (not current-prefix-arg)))
    (icicle-find-file-other-window)))


;;;###autoload (when (> emacs-major-version 22) (autoload 'icicle-find-file-of-content "icicles"))
;;;###autoload (when (> emacs-major-version 22) (autoload 'icicle-find-file-of-content-other-window "icicles"))
(when (> emacs-major-version 22)

  (put 'icicle-find-file-of-content 'icicle-Completions-window-max-height 200)

  (icicle-define-file-command icicle-find-file-of-content ; Not bound by default.
    "Visit a file or dir whose name and/or content matches.
Candidate files and directories for completion are examined, and those
whose names and/or contents match your multi-completion input are
available to visit.

\(Option `find-file-run-dired' determines whether you can actually
visit a directory candidate that you choose.)

If you use a prefix argument when you act on a completion candidate,
then you visit the file or dir in read-only mode.  This includes when
you act on all candidates using \\<minibuffer-local-completion-map>\
`\\[icicle-all-candidates-action]': precede the `\\[icicle-all-candidates-action]' with a prefix
arg.  (See below for the use of a prefix arg for the command itself.)

Completion candidates are two-part multi-completions, with the second
part optional.  If both parts are present they are separated by
`icicle-list-join-string' (\"^G^J\", by default).

The first part is matched as a regexp against a file or directory
name.  The second part is matched as a regexp against the file or
directory content.  Candidates that do not match are filtered out.

Your minibuffer input can match a name or content, or both.  Use
`C-M-j' (equivalent here to `C-q C-g C-j') to input the default
separator.

For example:

 To match `foo' against file and dir names, use input `foo'.
 To match `bar' against file and dir contents, use input `C-M-j bar'.
 To match both names and content, use input `foo C-M-j bar'.

Only the matching file and directory names are shown in buffer
`*Completions*', and only the chosen name is returned.  The actual
content matches are unimportant anyway: content matching is used only
to filter the candidates.

If your input does not include a content-matching part then this
command acts similar to `icicle-find-file' (but with a different use
of the prefix argument).

If your input includes a content-matching part then all files and
directories matching the name part of your input (or all, if no name
part) are visited.  This creates buffers visiting each matching
candidate.

For a directory, a Dired buffer is used - that is the content that is
searched.  (Actually, this is determined by option
`find-directory-functions'.)

As you would expect, content matching can be costly in time, even
though it can be quite helpful.  Use name matching to narrow the set
of files that must be visited to search their contents.

When this command is finished, any unused buffers that were created
for content matching are killed, if option
`icicle-kill-visited-buffers-flag' is non-nil.  But a prefix argument
flips the behavior specified by that option." ; Doc string
    (lambda (file)                      ; Action function
      ;; Free vars here: CURRENT-PREFIX-ARG, INIT-PREF-ARG, THIS-COMMAND, NEW-BUFS--TO-KEEP.
      (let* ((r-o  (and (memq this-command '(icicle-candidate-action icicle-mouse-candidate-action
                                             icicle-all-candidates-action))
                        current-prefix-arg))
             (fn   (if r-o 'find-file-read-only 'find-file)))
        (setq file  (icicle-transform-multi-completion file))
        (funcall fn file 'WILDCARDS)
        ;; Add the visited buffer to those we will keep (not kill).
        ;; If FILE uses wildcards then there will be multiple such buffers.
        ;; For a directory, get the Dired buffer instead of using `get-file-buffer'.
        (let ((fil2  (if (string= "" (file-name-nondirectory file))  (directory-file-name file)  file)))
          (dolist (fil  (file-expand-wildcards fil2))
            (when (setq fil  (if (file-directory-p fil)
                                 (get-buffer (file-name-nondirectory fil))
                               (get-file-buffer fil)))
              (push fil new-bufs--to-keep))))))
    prompt nil (if (eq major-mode 'dired-mode) ; `read-file-name' args
                   (condition-case nil  ; E.g. error because not on file line (ignore)
                       (abbreviate-file-name (dired-get-file-for-visit))
                     (error nil))
                 default-directory)
    (confirm-nonexistent-file-or-buffer) nil nil
    (icicle-file-bindings               ; Bindings
     ((init-pref-arg                          current-prefix-arg)
      (prompt                             "File or directory: ")
      (icicle-compute-narrowing-regexp-p      t) ; For progressive completion.
      (icicle-apropos-complete-match-fn       'icicle-file-of-content-apropos-complete-match)
      (icicle-last-apropos-complete-match-fn  'icicle-file-of-content-apropos-complete-match)
      (icicle-read-file-name-internal-fn      'icicle-find-file-of-content-multi-complete)
      (icicle-show-multi-completion-flag      t) ; Override user setting.
      (icicle-multi-completing-p              t)
      (icicle-list-use-nth-parts              '(1))
      (icicle-transform-before-sort-p         t)
      (existing-bufs                          (buffer-list))
      (new-bufs--to-kill                      ())
      (new-bufs--to-keep                      ())
      (icicle-all-candidates-list-alt-action-fn ; `M-|'
       (lambda (files) (let ((enable-recursive-minibuffers  t))
                         (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
    (progn (icicle-bind-file-candidate-keys) ; First code
           (put-text-property 0 1 'icicle-fancy-candidates t prompt)
           (icicle-highlight-lighter))
    nil                                 ; Undo code
    (progn (icicle-unbind-file-candidate-keys) ; Last code
           (when (or (and init-pref-arg        (not icicle-kill-visited-buffers-flag))
                     (and (not init-pref-arg)  icicle-kill-visited-buffers-flag))
             (dolist (buf  new-bufs--to-kill)
               (unless (memq buf new-bufs--to-keep)
                 (with-current-buffer buf
                   (restore-buffer-modified-p nil) ; Just visiting can sometimes modify the buffer
                   (kill-buffer buf)))))))


  (put 'icicle-find-file-of-content-other-window 'icicle-Completions-window-max-height 200)

  (icicle-define-file-command icicle-find-file-of-content-other-window ; Not bound by default.
    "Visit a file or dir whose name and/or content matches, in another window.
Same as `icicle-find-file-of-content' except it uses a different window." ; Doc string
    (lambda (file)                      ; Action function
      ;; Free vars here: CURRENT-PREFIX-ARG, INIT-PREF-ARG, THIS-COMMAND, NEW-BUFS--TO-KEEP.
      (let* ((r-o  (and (memq this-command '(icicle-candidate-action icicle-mouse-candidate-action
                                             icicle-all-candidates-action))
                        current-prefix-arg))
             (fn   (if r-o 'find-file-read-only-other-window 'find-file-other-window)))
        (setq file  (icicle-transform-multi-completion file))
        (funcall fn file 'WILDCARDS)
        ;; Add the visited buffer to those we will keep (not kill).
        ;; If FILE uses wildcards then there will be multiple such buffers.
        ;; For a directory, get the Dired buffer instead of using `get-file-buffer'.
        (let ((fil2  (if (string= "" (file-name-nondirectory file))  (directory-file-name file)  file)))
          (dolist (fil  (file-expand-wildcards fil2))
            (when (setq fil  (if (file-directory-p fil)
                                 (get-buffer (file-name-nondirectory fil))
                               (get-file-buffer fil)))
              (push fil new-bufs--to-keep))))))
    prompt nil (if (eq major-mode 'dired-mode) ; `read-file-name' args
                   (condition-case nil  ; E.g. error because not on file line (ignore)
                       (abbreviate-file-name (dired-get-file-for-visit))
                     (error nil))
                 default-directory)
    (confirm-nonexistent-file-or-buffer) nil nil
    (icicle-file-bindings               ; Bindings
     ((init-pref-arg                          current-prefix-arg)
      (prompt                                 "File or directory: ")
      (icicle-compute-narrowing-regexp-p      t) ; For progressive completion.
      (icicle-apropos-complete-match-fn       'icicle-file-of-content-apropos-complete-match)
      (icicle-last-apropos-complete-match-fn  'icicle-file-of-content-apropos-complete-match)
      (icicle-read-file-name-internal-fn      'icicle-find-file-of-content-multi-complete)
      (icicle-show-multi-completion-flag      t) ; Override user setting.
      (icicle-multi-completing-p              t)
      (icicle-list-use-nth-parts              '(1))
      (icicle-transform-before-sort-p         t)
      (existing-bufs                          (buffer-list))
      (new-bufs--to-kill                      ())
      (new-bufs--to-keep                      ())
      (icicle-all-candidates-list-alt-action-fn ; `M-|'
       (lambda (files) (let ((enable-recursive-minibuffers  t))
                         (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
    (progn (icicle-bind-file-candidate-keys)
           (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
           (icicle-highlight-lighter))
    nil                                 ; Undo code
    (progn (icicle-unbind-file-candidate-keys) ; Last code
           (when (or (and init-pref-arg        (not icicle-kill-visited-buffers-flag))
                     (and (not init-pref-arg)  icicle-kill-visited-buffers-flag))
             (dolist (buf  new-bufs--to-kill)
               (unless (memq buf new-bufs--to-keep)
                 (with-current-buffer buf
                   (restore-buffer-modified-p nil) ; Just visiting can sometimes modify the buffer
                   (kill-buffer buf)))))))

  (defun icicle-find-file-of-content-multi-complete (strg predicate completion-mode)
    "Completion function for `icicle-find-file-of-content'.
Used as the value of `minibuffer-completion-table'."
    (lexical-let* ((file-pat      (let ((icicle-list-use-nth-parts  '(1)))
                                    (icicle-transform-multi-completion strg)))
                   (content-pat   (let ((icicle-list-use-nth-parts  '(2)))
                                    (icicle-transform-multi-completion strg)))
                   (content-pred  (if (equal "" content-pat)
                                      predicate
                                    (lambda (filname)
                                      ;; Avoid the error raised by calling `find-file-noselect' on a directory
                                      ;; when `find-file-run-dired' is nil.
                                      (and (funcall `,predicate filname)
                                           (or find-file-run-dired  (not (file-directory-p filname)))
                                           (let* ((buf    (find-file-noselect filname))
                                                  (found  (with-current-buffer buf
                                                            (message "Matching file contents...")
                                                            (save-excursion
                                                              (goto-char (point-min))
                                                              (re-search-forward content-pat nil t)))))
                                             ;; Free vars here: EXISTING-BUFFERS, NEW-BUFS--TO-KILL
                                             (when (and (boundp 'existing-bufs)  (boundp 'new-bufs--to-kill)
                                                        (not (memq buf existing-bufs)))
                                               (add-to-list 'new-bufs--to-kill buf))
                                             found))))))
      (funcall (completion-table-in-turn #'icicle-completion--embedded-envvar-table
                                         #'completion-file-name-table)
               file-pat content-pred completion-mode)))

  (defun icicle-file-of-content-apropos-complete-match (input file-name)
    "Match function for progressive completion with `icicle-find-file-of-content'.
Return non-nil if the current multi-completion INPUT matches FILE-NAME."
    (lexical-let* ((name-pat     (let ((icicle-list-use-nth-parts  '(1)))
                                   (icicle-transform-multi-completion input)))
                   (content-pat  (let ((icicle-list-use-nth-parts  '(2)))
                                   (icicle-transform-multi-completion input))))
      (and (icicle-string-match-p name-pat file-name)
           ;; `icicle-narrow-regexp' is FREE here.  It is bound in `icicle-narrow-candidates'.
           ;; Do this to ensure we visit only the `icicle-completion-candidates' already determined so far.
           (or (not icicle-narrow-regexp)  (icicle-string-match-p icicle-narrow-regexp file-name))
           (or find-file-run-dired  (not (file-directory-p file-name)))
           (or (equal "" content-pat)
               (let ((buf    (find-file-noselect file-name)))
                 (with-current-buffer buf
                   (message "Matching buffer contents...")
                   (save-excursion (goto-char (point-min)) (re-search-forward content-pat nil t))))))))
  )


(put 'icicle-recent-file 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-recent-file "icicles")
(icicle-define-command icicle-recent-file ; Command name
  "Open a recently used file.
With a prefix argument, you can choose also by date: Completion
candidates include the last modification date.

Note that completion here matches candidates as ordinary strings.  It
knows nothing of file names per se.  In particular, you cannot use
remote file-name syntax.

Remember that you can use \\<minibuffer-local-completion-map>`C-x .' to hide the common match portion of
each candidate.  That can be particularly helpful for files that are
in a common directory.

During completion (`*' means this requires library `Bookmark+'):

 *You can use `C-x a +' or `C-x a -' to add or remove tags from the
   current-candidate file.  You are prompted for the tags.
 *You can use `C-x m' to access file bookmarks (not just autofiles).
  You can use `C-c +' to create a new directory.
  You can use `\\[icicle-all-candidates-list-alt-action]' to open Dired on currently matching file names.
  You can use `\\[icicle-delete-candidate-object]' to delete a candidate file or (empty) dir.

You can use any of the alternative-action keys, such as `\\[icicle-candidate-alt-action]', to
remove a candidate file from the recent files list, `recentf-list'.
\(The file itself is not deleted.)

These options, when non-nil, control candidate matching and filtering:

 `icicle-file-extras'           - Extra absolute file names to display
 `icicle-file-match-regexp'     - Regexp that file names must match
 `icicle-file-no-match-regexp'  - Regexp file names must not match
 `icicle-file-predicate'        - Predicate file names must satisfy
 `icicle-file-sort'             - Sort function for candidates

For example, to show only names of files larger than 5000 bytes, set
`icicle-file-predicate' to:

  (lambda (file) (and (numberp (nth 7 (file-attributes file)))
                      (> (nth 7 (file-attributes file)) 5000)))

Option `icicle-file-require-match-flag' can be used to override
option `icicle-require-match-flag'.

Option `icicle-files-ido-like' non-nil gives this command a more
Ido-like behavior."                     ; Doc string
  (lambda (f) (find-file (icicle-transform-multi-completion f) 'WILDCARDS)) ; Action function
  prompt icicle-abs-file-candidates nil ; `completing-read' args
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ;Emacs 23.
  nil 'file-name-history (car recentf-list) nil
  (icicle-file-bindings                 ; Bindings
   ((prompt                                 "Recent file (absolute): ")
    (icicle-full-cand-fn                `(lambda (file)
                                          (setq file  (if (file-directory-p file)
                                                          (file-name-as-directory file)
                                                        file))
                                          ,(if current-prefix-arg
                                               '(icicle-make-file+date-candidate file)
                                               '(list file))))
    (icicle-transform-before-sort-p         t)
    (icicle-sort-comparer                   'icicle-last-accessed-first-p)
    (icicle-abs-file-candidates
     (progn (unless (boundp 'recentf-list) (require 'recentf))
            (when (fboundp 'recentf-mode) (recentf-mode 99))
            (unless (consp recentf-list)
              (error "No recently accessed files"))
            (mapcar (lambda (file)      ; FREE here: CURRENT-PREFIX-ARG.
                      (if current-prefix-arg (icicle-make-file+date-candidate file) (list file)))
                    recentf-list)))
    (icicle-candidate-alt-action-fn         'icicle-remove-from-recentf-candidate-action)
    (icicle-use-candidates-only-once-alt-p  t)
    (icicle-candidate-properties-alist      (and current-prefix-arg  '((1 (face icicle-candidate-part)))))
    (icicle-multi-completing-p              current-prefix-arg)
    (icicle-list-use-nth-parts              (and current-prefix-arg  '(1)))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ")
                                                 (mapcar #'icicle-transform-multi-completion files))))))))
  (progn                                ; First code
    (when current-prefix-arg (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (icicle-highlight-lighter)
    (message "Gathering files...")
    (icicle-bind-file-candidate-keys))
  nil                                   ; Undo code
  (icicle-unbind-file-candidate-keys))  ; Last code

;;;###autoload (autoload 'icicle-recent-file-other-window "icicles")
(icicle-define-command icicle-recent-file-other-window ; Command name
  "Same as `icicle-recent-file' except uses another window." ; Doc string
  (lambda (f) (find-file-other-window (icicle-transform-multi-completion f) 'WILDCARDS)) ; Action
  prompt icicle-abs-file-candidates nil ; `completing-read' args
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ;Emacs 23.
  nil 'file-name-history (car recentf-list) nil
  (icicle-file-bindings                 ; Bindings
   ((prompt                                 "Recent file (absolute): ")
    (icicle-full-cand-fn                    `(lambda (file)
                                              (setq file  (if (file-directory-p file)
                                                              (file-name-as-directory file)
                                                            file))
                                              ,(if current-prefix-arg
                                                   '(icicle-make-file+date-candidate file)
                                                   '(list file))))
    (icicle-transform-before-sort-p         t)
    (icicle-sort-comparer                   'icicle-last-accessed-first-p)
    (icicle-abs-file-candidates
     (progn (unless (boundp 'recentf-list) (require 'recentf))
            (when (fboundp 'recentf-mode) (recentf-mode 99))
            (unless (consp recentf-list)
              (error "No recently accessed files"))
            (mapcar (lambda (file)      ; FREE here: CURRENT-PREFIX-ARG.
                      (if current-prefix-arg (icicle-make-file+date-candidate file) (list file)))
                    recentf-list)))
    (icicle-candidate-alt-action-fn         'icicle-remove-from-recentf-candidate-action)
    (icicle-use-candidates-only-once-alt-p  t)
    (icicle-candidate-properties-alist      (and current-prefix-arg  '((1 (face icicle-candidate-part)))))
    (icicle-multi-completing-p              current-prefix-arg)
    (icicle-list-use-nth-parts              (and current-prefix-arg  '(1)))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ")
                                                 (mapcar #'icicle-transform-multi-completion files))))))))
  (progn                                ; First code
    (when current-prefix-arg (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (icicle-highlight-lighter)
    (message "Gathering files...")
    (icicle-bind-file-candidate-keys))
  nil                                   ; Undo code
  (icicle-unbind-file-candidate-keys))  ; Last code

;;;###autoload (autoload 'icicle-remove-file-from-recentf-list "icicles")
(icicle-define-command icicle-remove-file-from-recentf-list
  "Remove file from `recentf-list' - the list of recently used files."
  icicle-remove-from-recentf-candidate-action
  "Remove from recent files list, `recentf-list': "
  (mapcar #'list (progn (unless (boundp 'recentf-list) (require 'recentf))
                        (when (fboundp 'recentf-mode) (recentf-mode 99))
                        (unless (consp recentf-list) (error "No recently accessed files"))
                        recentf-list))
  nil (and (fboundp 'confirm-nonexistent-file-or-buffer) ; Emacs 23.
           (confirm-nonexistent-file-or-buffer))
  nil 'file-name-history (car recentf-list) nil
  ((icicle-use-candidates-only-once-flag  t)))

(defun icicle-remove-from-recentf-candidate-action (file)
  "Action function for command `icicle-remove-file-from-recentf-list'."
  (setq recentf-list  (delete file recentf-list))
  (message "`%s' removed from `recentf-list'" (icicle-propertize file 'face 'icicle-msg-emphasis)))


(defvar icicle-locate-file-action-fn nil
  "Action function used in `icicle-locate-file-1'.")

(defvar icicle-locate-file-no-symlinks-p nil
  "Flag bound in `icicle-locate-file* for use by `icicle-files-within'.")

(defvar icicle-locate-file-use-locate-p nil
  "Flag bound to non-nil in `icicle-locate(-other-window)'.
Non-nil means `icicle-locate-file-1' uses external command `locate'.")


(put 'icicle-locate-file 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-locate-file "icicles")
(defun icicle-locate-file ()
  "Visit a file within one or more directories or their subdirectories.
A prefix argument determines the behavior, as follows:

* None: The default (i.e., current) directory is used.

* Plain (`C-u'): You are prompted for the directories, using
  multi-command `icicle-directory-list'.

* Non-negative (>= 0): You are prompted for the directory.

* Non-positive (<= 0): You can choose files also by date: A completion
  candidate includes the last modification date of the file.

* Double plain (`C-u C-u'): You are prompted for the directories and
  candidates include last-modification dates.

The absolute names of all files within a directory and all of its
subdirectories are targets for completion.  Regexp input is matched
against all parts of the absolute name, not just the file-name part.

Remember that you can use `C-x .' to hide the common match portion of
each candidate.  That can be particularly helpful for files that are
in a common directory.

You can use this command to find all files within your file system
that match a regexp, but be aware that gathering and matching the file
names will take some time.

See also command `icicle-locate-file-no-symlinks', which does the same
thing but without following symbolic links.

If you use Emacs on a platform that has an external program `locate',
then consider using `icicle-locate' instead of `icicle-locate-file'.

Remember that you can save the set of files matching your input using
\\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]' or \
`\\[icicle-candidate-set-save-persistently]'.  You can then retrieve quickly them later using
`\\[icicle-candidate-set-retrieve]' or \
`\\[icicle-candidate-set-retrieve-persistent]'.

Note that completion here matches candidates as ordinary strings.  It
knows nothing of file names per se.  In particular, you cannot use
remote file-name syntax.

You cannot move up and down the file hierarchy the same way you can
for ordinary (non-absolute) file-name completion.  To change to a
different directory, with its files as candidates, use \\<minibuffer-local-completion-map>`C-c C-d' from
the minibuffer - it prompts you for the new directory.

During completion (`*' means this requires library `Bookmark+'):

 *You can use `C-x a +' or `C-x a -' to add or remove tags from the
   current-candidate file.  You are prompted for the tags.
 *You can use `C-x m' to access file bookmarks (not just autofiles).
  You can use `C-c C-d' (a la `cd') to change the `default-directory'.
  You can use `C-c +' to create a new directory.
  You can use `\\[icicle-all-candidates-list-alt-action]' to open Dired on currently matching file names.
  You can use `\\[icicle-delete-candidate-object]' to delete a candidate file or (empty) dir.

Directories in `icicle-ignored-directories' are ignored (skipped).  In
addition, these options control candidate matching and filtering:

 `icicle-file-extras'           - Extra file names to display
 `icicle-file-match-regexp'     - Regexp that file names must match
 `icicle-file-no-match-regexp'  - Regexp file names must not match
 `icicle-file-predicate'        - Predicate file names must satisfy
 `icicle-file-require-match-flag' - See `icicle-require-match-flag'
 `icicle-file-sort'             - Sort function for candidates

For example, to show only names of files larger than 5000 bytes, set
`icicle-file-predicate' to:

  (lambda (file) (and (numberp (nth 7 (file-attributes file)))
                      (> (nth 7 (file-attributes file)) 5000)))"
  (interactive)
  (let ((icicle-locate-file-action-fn      'icicle-locate-file-action)
        (icicle-locate-file-no-symlinks-p  nil))
    (icicle-locate-file-1)))

;;;###autoload (autoload 'icicle-locate-file-other-window "icicles")
(defun icicle-locate-file-other-window ()
  "Same as `icicle-locate-file' except uses another window.
See also command `icicle-locate-file-no-symlinks-other-window', which
does not follow symbolic links."
  (interactive)
  (let ((icicle-locate-file-action-fn      'icicle-locate-file-other-window-action)
        (icicle-locate-file-no-symlinks-p  nil))
    (icicle-locate-file-1)))


(put 'icicle-locate 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-locate "icicles")
(defun icicle-locate ()
  "Run the external program `locate', then visit files.
Unlike `icicle-locate-file' this is a wrapper for the external program
`locate', which searches an index of files in your file system, which
is normally created by external program `updatedb'.  Because of this
indexing, this command can be much faster than `icicle-locate-file'.

`icicle-locate' first prompts for a search pattern for program
`locate', which it passes to that program.  The absolute file names
that match this pattern are targets for Icicles completion.

`icicle-locate' uses settings from library `locate.el' where
appropriate.  In particular, you can customize
`locate-make-command-line' to use either regexp matching or file-name
globbing.  Here is an example of a setup to use regexp matching:

\(setq locate-make-command-line
      (lambda (ss) (list locate-command \"--regex\" ss)))

Which particular options the external program `locate' accepts, and
how matching is performed, depend on your operating system and its
implementation of that program.

A prefix argument has the same meaning as for vanilla Emacs command
`locate': prompt for a shell command to run instead of program
`locate'.  A prefix arg has the effect of flipping the value of user
option `locate-prompt-for-command' for the duration of the command
invocation.

After you input the search pattern for program `locate', normal
Icicles input pattern matching is available for completion.  This is
absolute file-name completion, so your input can match any parts of
the name, including directory components.

Remember that you can use \\<minibuffer-local-completion-map>`C-x .' to hide the common match portion of
each candidate.  That can be particularly helpful for files that are
in a common directory.

Remember that you can save the set of files matching your input using
\\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]' or \
`\\[icicle-candidate-set-save-persistently]'.  You can then retrieve quickly them later using
`\\[icicle-candidate-set-retrieve]' or \
`\\[icicle-candidate-set-retrieve-persistent]'.

Note that completion here matches candidates as ordinary strings.  It
knows nothing of file names per se.  In particular, you cannot use
remote file-name syntax.

During completion (`*' means this requires library `Bookmark+'):

 *You can use `C-x a +' or `C-x a -' to add or remove tags from the
   current-candidate file.  You are prompted for the tags.
 *You can use `C-x m' to access file bookmarks (not just autofiles).
  You can use `C-c +' to create a new directory.
  You can use `\\[icicle-all-candidates-list-alt-action]' to open Dired on currently matching file names.
  You can use `\\[icicle-delete-candidate-object]' to delete a candidate file or (empty) dir.

These Icicles options control candidate matching and filtering:

 `icicle-file-extras'           - Extra file names to display
 `icicle-file-match-regexp'     - Regexp that file names must match
 `icicle-file-no-match-regexp'  - Regexp file names must not match
 `icicle-file-predicate'        - Predicate file names must satisfy
 `icicle-file-require-match-flag' - See `icicle-require-match-flag'
 `icicle-file-sort'             - Sort function for candidates

For example, to show only names of files larger than 5000 bytes, you
could temporarily set `icicle-file-predicate' to:

  (lambda (file) (and (numberp (nth 7 (file-attributes file)))
                      (> (nth 7 (file-attributes file)) 5000)))"
  (interactive)
  (let ((icicle-locate-file-action-fn     'icicle-locate-file-action)
        (icicle-locate-file-use-locate-p  t))
    (icicle-locate-file-1)))

;;;###autoload (autoload 'icicle-locate-other-window "icicles")
(defun icicle-locate-other-window ()
  "Same as `icicle-locate' except uses another window."
  (interactive)
  (let ((icicle-locate-file-action-fn     'icicle-locate-file-other-window-action)
        (icicle-locate-file-use-locate-p  t))
    (icicle-locate-file-1)))


(put 'icicle-locate-file-no-symlinks 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-locate-file-no-symlinks "icicles")
(defun icicle-locate-file-no-symlinks ()
  "Same as `icicle-locate-file', except do not follow symlinks."
  (interactive)
  (let ((icicle-locate-file-action-fn      'icicle-locate-file-other-window-action)
        (icicle-locate-file-no-symlinks-p  t))
    (icicle-locate-file-1)))

;;;###autoload (autoload 'icicle-locate-file-no-symlinks-other-window "icicles")
(defun icicle-locate-file-no-symlinks-other-window ()
  "Same as `icicle-locate-file-no-symlinks', except uses another window."
  (interactive)
  (let ((icicle-locate-file-action-fn      'icicle-locate-file-other-window-action)
        (icicle-locate-file-no-symlinks-p  t))
    (icicle-locate-file-1)))

(defun icicle-locate-file-action (file)
  "Action function for `icicle-locate-file'."
  (find-file (icicle-transform-multi-completion file) 'WILDCARDS))

(defun icicle-locate-file-other-window-action (file)
  "Action function for `icicle-locate-file-other-window'."
  (find-file-other-window (icicle-transform-multi-completion file) 'WILDCARDS))

;;;###autoload (autoload 'icicle-locate-file-1 "icicles")
(icicle-define-command icicle-locate-file-1
  "Helper for `icicle-locate(-file(-no-symlinks))(-other-window)'." ; Doc string
  ;; `icicle-locate-file-action-fn' and `icicle-locate-file-use-locate-p' are free here.
  (lambda (f) (funcall icicle-locate-file-action-fn f)) ; FREE here: ICICLE-LOCATE-FILE-ACTION-FN.
  prompt icicle-abs-file-candidates nil ; `completing-read' args
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ;Emacs 23.
  nil 'file-name-history nil nil
  (icicle-file-bindings                 ; Bindings
   ((prompt                             "File (absolute): ")
    (dirs                               (and (not icicle-locate-file-use-locate-p)
                                             (cond ((and current-prefix-arg  (consp current-prefix-arg))
                                                    (icicle-directory-list))
                                                   ((and current-prefix-arg
                                                         (wholenump (prefix-numeric-value current-prefix-arg)))
                                                    (read-file-name "Locate under which directory: "
                                                                    nil default-directory nil))
                                                   (t  default-directory))))
    (icicle-multi-completing-p          (and (not icicle-locate-file-use-locate-p)
                                             ;; FREE here: CURRENT-PREFIX-ARG.
                                             (or (<= (prefix-numeric-value current-prefix-arg) 0)
                                                 (and current-prefix-arg  (consp current-prefix-arg)
                                                      (= (car current-prefix-arg) 16)))))
    (icicle-full-cand-fn                (if icicle-multi-completing-p
                                            (lambda (file)
                                              (setq file  (if (file-directory-p file)
                                                              (file-name-as-directory file)
                                                            file))
                                              (icicle-make-file+date-candidate file))
                                          (lambda (file)
                                            (setq file  (if (file-directory-p file)
                                                            (file-name-as-directory file)
                                                          file))
                                            (list file))))
    (use-dialog-box                     nil)
    (icicle-candidate-properties-alist  (and icicle-multi-completing-p  '((1 (face icicle-candidate-part)))))
    (icicle-list-use-nth-parts          (and icicle-multi-completing-p  '(1)))
    (IGNORED--FOR-SIDE-EFFECT
     (progn (icicle-highlight-lighter)
            (if icicle-locate-file-use-locate-p
                (require 'locate)       ; Hard-require: error if not there.
              (message "Gathering files %s (this could take a while)..."
                       (if (or (symbolp dirs)  (consp dirs))
                           (format "in `%s'" (icicle-propertize dirs 'face 'icicle-msg-emphasis))
                         (format "within `%s'" (icicle-propertize dirs 'face 'icicle-msg-emphasis)))))))
    (icicle-abs-file-candidates
     (mapcar (if icicle-multi-completing-p #'icicle-make-file+date-candidate #'list)
             (if icicle-locate-file-use-locate-p
                 (let* ((locate-buffer-name  " *Icicles Locate*")
                        (temp-locate-buffer  (get-buffer-create locate-buffer-name)))
                   (unwind-protect
                        (with-current-buffer temp-locate-buffer
                          (let ((cands  ()))
                            (call-interactively #'locate) ; Gets `current-prefix-arg'.
                            (dired-repeat-over-lines
                             (count-lines (point-min) (point-max))
                             (lambda () (push (dired-get-filename nil t) cands))) ; FREE here: CANDS.
                            (nreverse cands)))
                     (kill-buffer temp-locate-buffer)))
               (if (not (or (symbolp dirs)  (consp dirs)))
                   (icicle-files-within (directory-files dirs 'full icicle-re-no-dot)
                                        nil icicle-locate-file-no-symlinks-p)
                 (apply #'append
                        (mapcar (if icicle-locate-file-no-symlinks-p
                                    (lambda (dir) 
                                      (icicle-remove-if #'file-symlink-p
                                                        (directory-files dir 'full icicle-re-no-dot 'NOSORT)))
                                  (lambda (dir) (directory-files dir 'full icicle-re-no-dot 'NOSORT)))
                                dirs))))))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ")
                                                 (mapcar #'icicle-transform-multi-completion files))))))))
  (progn                                ; First code
    (when icicle-multi-completing-p (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (icicle-bind-file-candidate-keys)
    (unless icicle-locate-file-use-locate-p
      (define-key minibuffer-local-completion-map "\C-c\C-d" 'icicle-cd-for-loc-files)
      (define-key minibuffer-local-must-match-map "\C-c\C-d" 'icicle-cd-for-loc-files)))
  nil                                   ; Undo code
  (progn (icicle-unbind-file-candidate-keys) ; Last code
         (unless icicle-locate-file-use-locate-p
           (define-key minibuffer-local-completion-map "\C-c\C-d" nil)
           (define-key minibuffer-local-must-match-map "\C-c\C-d" nil)))
  'NON-INTERACTIVE)                     ; This is not a real command.

;; This is a minibuffer command.  It is in this file because it is used only here.
;;
;;;###autoload (autoload 'icicle-cd-for-loc-files "icicles")
(defun icicle-cd-for-loc-files (dir &optional no-symlinks-p) ; Bound to `C-c C-d' in minibuf locate-*.
  "Change `default-directory' during `icicle-locate-file'.
Optional arg NO-SYMLINKS-P non-nil means do not follow symbolic links."
  (interactive
   (save-selected-window
     ;; Should not need to bind `minibuffer-completion-predicate'.  Emacs 23.2 bug, per Stefan.
     (let ((minibuffer-completion-predicate  minibuffer-completion-predicate))
       (list (funcall (if (fboundp 'read-directory-name)
                          #'read-directory-name
                        #'read-file-name)
                      "Change default directory: " default-directory (icicle-file-name-directory-w-default
                                                                      (icicle-input-from-minibuffer))
                      (and (member cd-path '(nil ("./")))  (null (getenv "CDPATH"))))))))
  (cd dir)
  (let ((icicle-abs-file-candidates
         (mapcar (lambda (file)         ; FREE here: ICICLE-LIST-USE-NTH-PARTS.
                   (if icicle-multi-completing-p (icicle-make-file+date-candidate file) (list file)))
                 (icicle-files-within (directory-files dir 'full icicle-re-no-dot) nil no-symlinks-p))))
    (setq minibuffer-completion-table
          (car (icicle-mctize-all icicle-abs-file-candidates minibuffer-completion-predicate)))))


(put 'icicle-find-file-in-tags-table 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-find-file-in-tags-table "icicles")
(icicle-define-command icicle-find-file-in-tags-table ; Command name
  "Visit a file listed in a tags table.
By default, the completion candidates are the file names listed in the
current tags table, but you can substitute other candidates by
retrieving a saved candidate set.  The default candidates appear as
they did in the `etags' command that created the tags table, which
typically means without directory names.

Completion here matches candidates as ordinary strings.  It knows
nothing of file names per se.  In particular, you cannot use remote
file-name syntax.  If a candidate is an absolute file name then you
can complete against any and all parts of the name (including
directory components).

`find-file' is called for the candidate(s) you choose, with the
directory of the tags file as `default-directory'.

Remember that you can use \\<minibuffer-local-completion-map>`C-x .' to hide the common match portion of
each candidate.  That can be particularly helpful for files that are
in a common directory.

With a prefix argument, you can choose also by date: Completion
candidates include the last modification date.

During completion (`*' means this requires library `Bookmark+'):

 *You can use `C-x a +' or `C-x a -' to add or remove tags from the
   current-candidate file.  You are prompted for the tags.
 *You can use `C-x m' to access file bookmarks (not just autofiles).
  You can use `C-c +' to create a new directory.
  You can use `\\[icicle-all-candidates-list-alt-action]' to open Dired on currently matching file names.
  You can use `\\[icicle-delete-candidate-object]' to delete a candidate file or (empty) dir.

These options, when non-nil, control candidate matching and filtering:

 `icicle-file-extras'           - Extra file names to display
 `icicle-file-match-regexp'     - Regexp that file names must match
 `icicle-file-no-match-regexp'  - Regexp file names must not match
 `icicle-file-predicate'        - Predicate file names must satisfy
 `icicle-file-sort'             - Sort function for candidates

For example, to show only names of files larger than 5000 bytes, you
could temporarily set `icicle-file-predicate' to:

  (lambda (file) (and (numberp (nth 7 (file-attributes file)))
                      (> (nth 7 (file-attributes file)) 5000)))

Option `icicle-file-require-match-flag' can be used to override
option `icicle-require-match-flag'.

Option `icicle-files-ido-like' non-nil gives this command a more
Ido-like behavior."                     ; Doc string
  (lambda (ff)
    (visit-tags-table-buffer 'same)     ; To pick up `default-directory' of TAGS table.
    (find-file (icicle-transform-multi-completion ff) 'WILDCARDS)) ; Action function
  prompt                                ; `completing-read' args
  (mapcar (if current-prefix-arg #'icicle-make-file+date-candidate #'list)
          (save-excursion (let ((enable-recursive-minibuffers  t)) (visit-tags-table-buffer))
                          (tags-table-files)))
  nil
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ;Emacs 23.
  nil 'file-name-history nil nil
  (icicle-file-bindings                 ; Bindings
   ((prompt                             "File (in tags table): ")
    (icicle-full-cand-fn                `(lambda (file)
                                          (setq file  (if (file-directory-p file)
                                                          (file-name-as-directory file)
                                                        file))
                                          ,(if current-prefix-arg
                                               '(icicle-make-file+date-candidate file)
                                               '(list file))))
    (icicle-special-candidate-regexp    (or icicle-special-candidate-regexp  ".+/$"))
    (icicle-candidate-properties-alist  (and current-prefix-arg  '((1 (face icicle-candidate-part)))))
    (icicle-multi-completing-p          current-prefix-arg)
    (icicle-list-use-nth-parts          (and current-prefix-arg  '(1)))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
  (progn                                ; First code
    (when current-prefix-arg (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (unless (require 'etags nil t) (error "`etags.el' is required"))
    (icicle-bind-file-candidate-keys))
  nil                                   ; Undo code
  (icicle-unbind-file-candidate-keys))  ; Last code


(put 'icicle-find-file-in-tags-table-other-window 'icicle-Completions-window-max-height 200)

;;;###autoload (autoload 'icicle-find-file-in-tags-table-other-window "icicles")
(icicle-define-command icicle-find-file-in-tags-table-other-window ; Command name
  "Same as `icicle-find-file-in-tags-table', but uses another window." ; Doc string
  (lambda (ff)
    (visit-tags-table-buffer 'same)     ; To pick up `default-directory' of TAGS table.
    (find-file (icicle-transform-multi-completion ff) 'WILDCARDS)) ; Action function
  prompt                                ; `completing-read' args
  (mapcar (if current-prefix-arg #'icicle-make-file+date-candidate #'list)
          (save-excursion (let ((enable-recursive-minibuffers  t)) (visit-tags-table-buffer))
                          (tags-table-files)))
  nil
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ;Emacs 23.
  nil 'file-name-history nil nil
  (icicle-file-bindings                 ; Bindings
   ((prompt                             "File (in tags table): ")
    (icicle-full-cand-fn                `(lambda (file)
                                          (setq file  (if (file-directory-p file)
                                                          (file-name-as-directory file)
                                                        file))
                                          ,(if current-prefix-arg
                                               '(icicle-make-file+date-candidate file)
                                               '(list file))))
    (icicle-special-candidate-regexp    (or icicle-special-candidate-regexp  ".+/$"))
    (icicle-candidate-properties-alist  (and current-prefix-arg  '((1 (face icicle-candidate-part)))))
    (icicle-multi-completing-p          current-prefix-arg)
    (icicle-list-use-nth-parts          (and current-prefix-arg  '(1)))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
  (progn                                ; First code
    (when current-prefix-arg (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (unless (require 'etags nil t) (error "`etags.el' is required"))
    (icicle-bind-file-candidate-keys))
  nil                                   ; Undo code
  (icicle-unbind-file-candidate-keys))  ; Last code

(defun icicle-make-file+date-candidate (file)
  "Return a multi-completion candidate: FILE + last modification date."
  (list (list file (format-time-string "%Y %m %d %T " (nth 5 (file-attributes file))))))

;;;###autoload (autoload 'icicle-string-list "icicles")
(icicle-define-command icicle-string-list ; Command name
  "Choose a list of strings.  The list is returned.
You can choose from strings used previously or enter new strings.
Use multi-command action keys (e.g. `C-RET', `C-mouse-2') to choose,
and a final-choice key (e.g. `RET', `mouse-2') to choose the last one.

If option `icicle-add-proxy-candidates-flag' is non-nil (toggle using
`\\<minibuffer-local-completion-map>\\[icicle-toggle-proxy-candidates]'), you can also choose the \
name of a string variable - its value
is returned.  A string variable is a variable whose value or whose
custom type is compatible with type `string'." ; Doc string
  (lambda (string)                      ; FREE here: ICICLE-PROXY-CANDIDATES, STRINGS.
    (let (temp)
      (push (if (setq temp  (member string icicle-proxy-candidates))
                (setq temp  (symbol-value (intern (car temp))))
              (setq temp  string))
            strings)
      (when (interactive-p)
        (message "Added string \"%s\"" (icicle-propertize temp 'face 'icicle-msg-emphasis))
        (sit-for 1))))
  prompt (mapcar #'list (icicle-remove-duplicates comp-strings)) ; `completing-read' args
  nil nil nil 'regexp-history nil nil
  ((icicle-proxy-candidates             ; Bindings
    (and icicle-add-proxy-candidates-flag
         (let ((ipc  ()))
           (mapatoms (lambda (cand)     ; FREE here: IPC.
                       (when (and (user-variable-p cand)
                                  (condition-case nil
                                      (icicle-var-is-of-type-p cand '(string color regexp))
                                    (error nil)))
                         (push (symbol-name cand) ipc))))
           ipc)))
   (comp-strings                          (append regexp-history regexp-search-ring search-ring
                                                  icicle-search-history kill-ring))
   (strings                               ())
   (icicle-use-candidates-only-once-flag  t)
   (prompt                                (or icicle-prompt  "Choose string (`RET' when done): ")))
  (when icicle-proxy-candidates (put-text-property 0 1 'icicle-fancy-candidates t prompt)) ; First code
  nil                                   ; Undo code
  (prog1 (setq strings  (nreverse (delete "" strings))) ; Last code - return the list of strings.
    (setq icicle-proxy-candidates  ())
    (when (interactive-p) (message "Strings: %S" strings))))

(when (fboundp 'read-char-by-name)
  (defun icicle-zap-to-char (arg char &optional names)
    "Kill up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Go backward if ARG is negative.  Raise an error if CHAR is not found.

This is the same as `zap-to-char', except if you hit a completing key
such as `TAB' then you can complete against the char names in NAMES.

If you need to zap up to a completing-key char such as `TAB', escape
the char with `C-q'.  E.g., use `C-q TAB' instead of `TAB'.

NAMES has the same form as `ucs-names'.  Interactively, NAMES is
determined by option `icicle-zap-to-char-candidates'.  By default, it
is the subset of `ucs-names' that corresponds to the characters that
have been read previously (`icicle-read-char-history'), that is, the
Unicode names you entered.  If you want to complete against all
Unicode chars, then customize option `icicle-zap-to-char-candidates'."
    (interactive
     (list (prefix-numeric-value current-prefix-arg)
           (icicle-read-char-maybe-completing "Zap to char: "
                                              (and (functionp icicle-zap-to-char-candidates)
                                                   (funcall icicle-zap-to-char-candidates)))))
    (unless names (setq names  (or (icicle-char-cands-from-charlist)  (icicle-ucs-names))))
    (with-no-warnings                   ; Avoid "obsolete" warning for `translation-table-for-input'.
        (when (char-table-p translation-table-for-input) ; Free var here.
          (setq char  (or (aref translation-table-for-input char)  char))))
    (kill-region (point) (progn (search-forward (string char) nil nil arg)
                                ;; (goto-char (if (> arg 0)
                                ;;                (max (point-min) (1- (point)))
                                ;;              (min (point-max) (1+ (point))))) ; (vanilla)
                                (point)))))

;;;###autoload (autoload 'icicle-sexp-list "icicles")
(icicle-define-command icicle-sexp-list ; Command name
  "Choose a list of sexps.  The list is returned.
The list entries are Lisp objects, not strings (unless you use \"...\").

You can choose from sexps entered previously or enter new sexps.
Use multi-command action keys (e.g. `C-RET', `C-mouse-2') to choose,
and a final-choice key (e.g. `RET', `mouse-2') to choose the last one." ; Doc string
  (lambda (sexp)                        ; FREE here: SEXPS.
    (push sexp sexps)
    (when (interactive-p)
      (message "Added sexp `%s'" (icicle-propertize sexp 'face 'icicle-msg-emphasis)) (sit-for 1)))
  prompt                                ; `completing-read' args
  (mapcar #'list (icicle-remove-duplicates (symbol-value history)))
  nil nil nil history nil nil
  ((sexps                                 ()) ; Bindings
   (icicle-use-candidates-only-once-flag  t)
   (prompt                                (or icicle-prompt  "Choose sexp (`RET' when done): "))
   (history                               (or icicle-hist-var  'read-expression-history)))
  nil nil                               ; First code, undo code
  (prog1 (setq sexps  (nreverse (delete "" sexps)) ; Last code - return the list of sexps.
               sexps  (mapcar (lambda (sx) (car (read-from-string sx))) sexps))
    (when (interactive-p) (message "Sexps: %S" sexps))))

;;;###autoload (autoload 'icicle-regexp-list "icicles")
(defalias 'icicle-regexp-list 'icicle-keyword-list)
;;;###autoload (autoload 'icicle-keyword-list "icicles")
(icicle-define-command icicle-keyword-list ; Command name
  "Choose a list of keywords. The list of keywords (strings) is returned.
Each keyword is a regexp.  The regexps are OR'd, and the resulting
regexp is usable for `icicle-search'.

You can choose from keywords entered previously or enter new keywords.
Use multi-command action keys (e.g. `C-RET', `C-mouse-2') to choose,
and a final-choice key (e.g. `RET', `mouse-2') to choose the last one." ; Doc string
  (lambda (name)                        ; FREE here: KEYWORDS.
    (push name keywords)
    (when (interactive-p)
      (message "Added keyword `%s'" (icicle-propertize name 'face 'icicle-msg-emphasis)) (sit-for 1)))
  prompt (mapcar #'list (icicle-remove-duplicates regexp-history)) ; `completing-read' args
  nil nil nil 'regexp-history nil nil
  ((keywords                              ()) ; Bindings
   (icicle-use-candidates-only-once-flag  t)
   (prompt                                (or icicle-prompt
                                              "Choose keyword (regexp) (`RET' when done): ")))
  nil nil                               ; First code, undo code
  (prog1 (setq keywords  (nreverse (delete "" keywords))) ; Last code - return the list of keywords.
    (when (interactive-p) (message "Keywords (regexps): %S" keywords))))

;;;###autoload (autoload 'icicle-face-list "icicles")
(icicle-define-command icicle-face-list ; Command name
  "Choose a list of face names.  The list of names (strings) is returned.
Use multi-command action keys (e.g. `C-RET', `C-mouse-2') to choose,
and a final-choice key (e.g. `RET', `mouse-2') to choose the last one." ; Doc string
  (lambda (name)                        ; FREE here: FACE-NAMES.
    (let ((temp  (icicle-transform-multi-completion name)))
      (push temp face-names)
      (when (interactive-p)
        (message "Added face `%s'" (icicle-propertize temp 'face 'icicle-msg-emphasis)) (sit-for 1))))
  prompt (mapcar #'icicle-make-face-candidate (face-list)) ; `completing-read' args
  nil (not (stringp icicle-WYSIWYG-Completions-flag)) nil
  (if (boundp 'face-name-history) 'face-name-history 'icicle-face-name-history)
  nil nil
  ((prompt                                (or icicle-prompt ; Allow override.
                                              "Choose face (`RET' when done): ")) ; Bindings
   (icicle-list-nth-parts-join-string     ": ")
   (icicle-list-join-string               ": ")
   ;; $$$$$$ (icicle-list-end-string      "")
   (icicle-multi-completing-p             t)
   (icicle-list-use-nth-parts             '(1))
   (icicle-use-candidates-only-once-flag  t)
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "face")))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "face")))
   (face-names                            ()))
  (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
  nil                                   ; Undo code
  (prog1 (setq face-names  (nreverse (delete "" face-names))) ; Last code - return list of faces
    (when (interactive-p) (message "Faces: %S" face-names))))

;;;###autoload (autoload 'icicle-buffer-list "icicles")
(icicle-define-command icicle-buffer-list ; Command name
  "Choose a list of buffer names.
With a positive prefix arg, only buffers visiting files or directories
\(Dired) are candidates.

With a negative prefix arg, only buffers associated with the selected
frame are candidates.

Use multi-command action keys (e.g. \\<minibuffer-local-completion-map>`C-RET', `C-mouse-2') to choose,
and a final-choice key (e.g. `RET', `mouse-2') to choose the last one.

You can use `\\[icicle-delete-candidate-object]' during completion to kill a candidate buffer.
The list of names (strings) is returned.

These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'             - Extra buffers to display
 `icicle-buffer-match-regexp'       - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'    - Regexp buffers must not match
 `icicle-buffer-predicate'          - Predicate buffer names satisfy
 `icicle-buffer-sort'               - Sort function for candidates

Note: The prefix arg is tested, even when this is called
noninteractively.  Lisp code can bind `current-prefix-arg' to control
the behavior."                          ; Doc string
  (lambda (name)                        ; Action function.  FREE here: BUF-NAMES.
    (push name buf-names)
    (when (interactive-p)
      (message "Added buffer name `%s'" (icicle-propertize name 'face 'icicle-msg-emphasis))
      (sit-for 1)))
  prompt (mapcar (lambda (buf) (list (buffer-name buf))) ; `completing-read' args
                 (if current-prefix-arg
                     (if (wholenump (prefix-numeric-value current-prefix-arg))
                         (icicle-remove-if-not (lambda (bf)
                                                 (or (buffer-file-name bf)
                                                     (with-current-buffer bf (eq major-mode 'dired-mode))))
                                               (buffer-list))
                       (cdr (assq 'buffer-list (frame-parameters))))
                   (buffer-list)))
  (and icompletep  icicle-buffer-predicate
       (lambda (buf) (funcall icicle-buffer-predicate (car buf)))) ; FREE here: ICICLE-BUFFER-PREDICATE.
  (and (fboundp 'confirm-nonexistent-file-or-buffer)  (confirm-nonexistent-file-or-buffer)) ;Emacs23.
  nil 'buffer-name-history nil nil
  ((buf-names                               ()) ; Bindings
   (prompt                                  (or icicle-prompt ; Allow override.
                                                "Choose buffer name (`RET' when done): "))
   (completion-ignore-case                  (or (and (boundp 'read-buffer-completion-ignore-case)
                                                     read-buffer-completion-ignore-case)
                                                completion-ignore-case))
   (icicle-must-match-regexp                icicle-buffer-match-regexp)
   (icicle-must-not-match-regexp            icicle-buffer-no-match-regexp)
   (icompletep                              (and (boundp 'icomplete-mode)  icomplete-mode))
   (icicle-must-pass-after-match-predicate  (and (not icompletep)  icicle-buffer-predicate))
   (icicle-require-match-flag               icicle-buffer-require-match-flag)
   (icicle-extra-candidates                 icicle-buffer-extras)
   (icicle-delete-candidate-object          'icicle-kill-a-buffer) ; `S-delete' kills current buf
   (icicle-transform-function               'icicle-remove-dups-if-extras)
   (icicle-sort-comparer                    (or icicle-buffer-sort  icicle-sort-comparer))
   (icicle-sort-orders-alist
    (append (list '("by last access")   ; Renamed from "turned OFF'.
                  '("*...* last" . icicle-buffer-sort-*...*-last)
                  '("by buffer size" . icicle-buffer-smaller-p)
                  '("by major mode name" . icicle-major-mode-name-less-p)
                  (and (fboundp 'icicle-mode-line-name-less-p)
                       '("by mode-line mode name" . icicle-mode-line-name-less-p))
                  '("by file/process name" . icicle-buffer-file/process-name-less-p))
            (delete '("turned OFF") icicle-sort-orders-alist)))
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn  (icicle-alt-act-fn-for-type "buffer")))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn  (icicle-alt-act-fn-for-type "buffer")))
   (icicle-use-candidates-only-once-flag  t))
  nil nil                               ; First code, undo code
  (prog1 (setq buf-names  (nreverse (delete "" buf-names))) ; Last code - return the list of buffers
    (when (interactive-p) (message "Buffer names: %S" buf-names))))

;;;###autoload (autoload 'icicle-bookmark-list "icicles")
(icicle-define-command icicle-bookmark-list ; Command name
  "Choose a list of bookmarks.
This is an alist whose entries are bookmark entries.  The entries have
the bookmark names as their key.  You can use the return value as a
bookmark alist or as a COLLECTION argument for `completing-read'.

With a prefix argument, this is a list of the bookmark names, not an
alist of the full bookmarks.

If `icicle-show-multi-completion-flag' is non-nil, then completion
candidates are multi-completions, with the first part being the
bookmark name and the second part being the bookmark's file or buffer
name.  Otherwise, the candidates are just the bookmark names.

If you also use library Bookmark+ (`bookmark+.el') then\\<minibuffer-local-completion-map>:

 * Candidates displayed in `*Completions*' are color-coded by type.
 * You can sort the candidates (e.g. `C-,') in many more ways.
 * When you ask for help on a candidate (e.g. `C-M-return'), detailed
   information about the bookmark is shown in `*Help*'.  If you use a
   prefix arg for this (e.g. `C-u C-M-return') then the full, internal
   form of the bookmark is shown.

Use multi-command action keys (e.g. `C-RET', `C-mouse-2') to choose,
and a final-choice key (e.g. `RET', `mouse-2') to choose the last one.

You can use `\\[icicle-delete-candidate-object]' during completion to delete a candidate bookmark.
The list of bookmark names (strings) is returned.

Non-interactively:

 * If `icicle-bookmark-list-names-only-p' is non-nil, then return a
   list of the bookmark names (just as if a prefix arg were used).
 * If `icicle-bookmark-types' is non-nil, and you use Bookmark+,
   then only bookmarks of those types are used.  You can thus bind
   this variable around the function call to specialize the behavior
   to only certain types."              ; Doc string
  (lambda (name)                        ; FREE here: CHOSEN-BMKS, NAMES-ONLY-P.
    (let ((temp  (icicle-transform-multi-completion name)))
      (push (if names-only-p
                (icicle-unpropertize-completion temp)
              (bookmark-get-bookmark (icicle-unpropertize-completion temp)))
            chosen-bmks)
      (when (interactive-p)
        (message "Added bookmark `%s'" (icicle-propertize temp 'face 'icicle-msg-emphasis))
        (sit-for 1))))
  prompt icicle-candidates-alist nil (not icicle-show-multi-completion-flag) ; `completing-read' args
  nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
  (and (boundp 'bookmark-current-bookmark)  bookmark-current-bookmark) nil
  ((prompt                                      (or icicle-prompt ; Allow override.
                                                    "Choose bookmark (`RET' when done): "))
   (enable-recursive-minibuffers                t) ; In case we read input, e.g. File changed on disk...
   (completion-ignore-case                      bookmark-completion-ignore-case)
   (icicle-multi-completing-p                   icicle-show-multi-completion-flag)
   (icicle-list-use-nth-parts                   '(1))
   (icicle-candidate-properties-alist           (if (not icicle-show-multi-completion-flag)
                                                    ()
                                                  (if (facep 'file-name-shadow)
                                                      '((2 (face file-name-shadow))
                                                        (3 (face bookmark-menu-heading)))
                                                    '((3 (face bookmark-menu-heading))))))
   (icicle-transform-function                   (if (interactive-p) nil icicle-transform-function))
   (icicle-whole-candidate-as-text-prop-p       t)
   (icicle-transform-before-sort-p              t)
   (icicle-delete-candidate-object              'icicle-bookmark-delete-action)
   (types                                       icicle-bookmark-types)
   (names-only-p                                (if (interactive-p)
                                                    current-prefix-arg
                                                  icicle-bookmark-list-names-only-p))
   (icicle-candidates-alist                     ())
   (chosen-bmks                                 ())
   (icicle-unpropertize-completion-result-flag  nil) ; Remove only Icicles internal text properties.
   (icicle-sort-orders-alist
    (append '(("in *Bookmark List* order") ; Renamed from "turned OFF'.
              ("by bookmark name" . icicle-alpha-p))
            (and (featurep 'bookmark+)
                 (append
                  '(("by last bookmark access" (bmkp-bookmark-last-access-cp) icicle-alpha-p)
                    ("by bookmark visit frequency" (bmkp-visited-more-cp) icicle-alpha-p))
                  (and (icicle-set-intersection types '("info" "region"))
                       '(("by Info location" (bmkp-info-cp) icicle-alpha-p)))
                  (and (icicle-set-intersection types '("gnus" "region"))
                       '(("by Gnus thread" (bmkp-gnus-cp) icicle-alpha-p)))
                  (and (icicle-set-intersection types '("url" "region"))
                       '(("by URL" (bmkp-url-cp) icicle-alpha-p)))
                  (and (icicle-set-difference types
                                              '("bookmark-list" "desktop" "gnus" "info" "man" "url"))
                       '(("by bookmark type" (bmkp-info-cp bmkp-url-cp bmkp-gnus-cp
                                              bmkp-local-file-type-cp bmkp-handler-cp)
                          icicle-alpha-p)))
                  (and (icicle-set-difference
                        types '("bookmark-list" "desktop" "dired" "non-file"))
                       '(("by file name" (bmkp-file-alpha-cp) icicle-alpha-p)))
                  (and (icicle-set-intersection types
                                                '("local-file" "file" "dired" "region"))
                       '(("by local file type" (bmkp-local-file-type-cp) icicle-alpha-p)
                         ("by local file size" (bmkp-local-file-size-cp) icicle-alpha-p)
                         ("by last local file access"
                          (bmkp-local-file-accessed-more-recently-cp)
                          icicle-alpha-p)
                         ("by last local file update" (bmkp-local-file-updated-more-recently-cp)
                          icicle-alpha-p)))
                  (and (not (equal types '("desktop")))
                       '(("by last buffer or file access"
                          (bmkp-buffer-last-access-cp
                           bmkp-local-file-accessed-more-recently-cp)
                          icicle-alpha-p)))
                  (and (get-buffer "*Bookmark List*")
                       '(("marked before unmarked (in *Bookmark List*)" (bmkp-marked-cp)
                          icicle-alpha-p)))))
            '(("by previous use alphabetically" . icicle-historical-alphabetic-p)
              ("case insensitive" . icicle-case-insensitive-string-less-p))))         
   (icicle-candidate-help-fn
    ;; FREE here: CURRENT-PREFIX-ARG, ICICLE-GET-ALIST-CANDIDATE-FUNCTION, ICICLE-SHOW-MULTI-COMPLETION-FLAG.
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
    (message "Gathering bookmarks...")
    (bookmark-maybe-load-default-file)  ; Load bookmarks, define `bookmark-alist'.
    (if (not (featurep 'bookmark+))
        (mapcar (lambda (cand) (list (icicle-candidate-short-help
                                      (icicle-bookmark-help-string cand)
                                      (icicle-bookmark-propertize-candidate cand))))
                bookmark-alist)
      (unless types  (setq types '(all)))
      (dolist (type  types)
        (setq icicle-candidates-alist  (nconc icicle-candidates-alist
                                              (mapcar #'icicle-make-bookmark-candidate
                                                      (bmkp-sort-omit
                                                       (if (eq type 'all)
                                                           bookmark-alist
                                                         (funcall (intern (format "bmkp-%s-alist-only"
                                                                                  type)))))))))))
  (icicle-bookmark-cleanup-on-quit)     ; Undo code
  (prog1 (setq chosen-bmks  (nreverse (delete "" chosen-bmks))) ; Last code - return the list.
    (icicle-bookmark-cleanup)
    (when (interactive-p)
      (message "Bookmarks: %S" (if names-only-p chosen-bmks (mapcar #'car chosen-bmks))))))

;; $$$$$ (icicle-define-command icicle-file-list ; Command name
;;   "Choose a list of file names.
;; You can use \\<minibuffer-local-completion-map>\
;;`\\[icicle-delete-candidate-object]' during completion to delete a candidate file.
;; The list of names (strings) is returned." ; Doc string
;;   (lambda (name) (push name file-names)) ; Function to perform the action
;;   "Choose file (`RET' when done): "     ; `completing-read' args
;;   (mapcar #'list (directory-files default-directory nil icicle-re-no-dot))
;;   nil nil nil 'file-name-history nil nil
;;   ((file-names ())                      ; Additional bindings
;;    (icicle-delete-candidate-object  'icicle-delete-file-or-directory) ; `S-delete' deletes file.
;;    (icicle-use-candidates-only-once-flag  t))
;;   nil nil                               ; First code, undo code
;;   (prog1 (setq file-names  (nreverse (delete "" file-names))) ; Last code - return files list
;;     (when (interactive-p) (message "Files: %S" file-names))))

;;;###autoload (autoload 'icicle-file-list "icicles")
(icicle-define-file-command icicle-file-list ; Command name
  "Choose a list of file and directory names (strings), and return it.
Use multi-command action keys (e.g. \\<minibuffer-local-completion-map>`C-RET', `C-mouse-2') to choose,
and a final-choice key (e.g. `RET', `mouse-2') to choose the last one.

You can navigate the directory tree, picking files and directories
anywhere in the tree.

Remember too that you can use `\\[icicle-all-candidates-action]' to gather all of the file names
matching your current input.  For example, apropos-completing with
input `foo.*bar' and hitting `\\[icicle-all-candidates-action]' adds all file names matching that
regexp.

You can use either `RET' or `C-g' to finish adding file names to the
list.

During completion (`*' means this requires library `Bookmark+'):

 *You can use `C-x a +' or `C-x a -' to add or remove tags from the
   current-candidate file.  You are prompted for the tags.
 *You can use `C-x m' to access file bookmarks (not just autofiles).
  You can use `C-c +' to create a new directory.
  You can use `\\[icicle-all-candidates-list-alt-action]' to open Dired on currently matching file names.
  You can use `\\[icicle-delete-candidate-object]' to delete a candidate file or (empty) dir.

These options, when non-nil, control candidate matching and filtering:

 `icicle-file-extras'           - Extra file names to display
 `icicle-file-match-regexp'     - Regexp that file names must match
 `icicle-file-no-match-regexp'  - Regexp file names must not match
 `icicle-file-predicate'        - Predicate file names must satisfy
 `icicle-file-sort'             - Sort function for candidates

For example, to show only names of files larger than 5000 bytes, set
`icicle-file-predicate' to:

  (lambda (file) (and (numberp (nth 7 (file-attributes file)))
                      (> (nth 7 (file-attributes file)) 5000)))

Option `icicle-file-require-match-flag' can be used to override
option `icicle-require-match-flag'.

Option `icicle-files-ido-like' non-nil gives this command a more
Ido-like behavior."                     ; Doc string
  (lambda (name)                        ; FREE here: FILE-NAMES.
    (push name file-names)
    (when (interactive-p)
      (message "Added file name `%s'" (icicle-propertize name 'face 'icicle-msg-emphasis)) (sit-for 1)))
  prompt nil nil t nil nil              ; `read-file-name' args
  (icicle-file-bindings                 ; Bindings
   ((prompt                             (or icicle-prompt ; Allow override.
                                            "Choose file (`RET' when done): "))
    (file-names                         ())
    (icicle-comp-base-is-default-dir-p  t)
    ;; $$$$$ (icicle-dir-candidate-can-exit-p (not current-prefix-arg))
    ))
  (icicle-bind-file-candidate-keys)     ; First code
  nil                                   ; Undo code
  (prog1 (setq file-names  (nreverse (delete "" file-names))) ; Last code - return list of files
    (icicle-unbind-file-candidate-keys)
    (when (interactive-p) (message "Files: %S" file-names))))

;;;###autoload (autoload 'icicle-directory-list "icicles")
(icicle-define-file-command icicle-directory-list ; Command name
  "Choose a list of directory names (strings), and return it.
You must include a slash (`/') at the end of each directory name.
Use multi-command action keys (e.g. `C-RET', `C-mouse-2') to choose,
and a final-choice key (e.g. `RET', `mouse-2') to choose the last one.

You can navigate the directory tree, picking directories anywhere in
the tree.

If `icicle-add-proxy-candidates-flag' is non-nil, then certain Emacs
variables whose values are lists of directories are available as proxy
candidates.  This includes variables such as `load-path' and
`exec-path'.  You can toggle `icicle-add-proxy-candidates-flag' using
\\<minibuffer-local-completion-map>\
`\\[icicle-toggle-proxy-candidates]'in the minibuffer.

When you choose a proxy candidate all of its directories are added to
the result list.  Non-directory elements in the variable value are
ignored - only string elements are retained.  And none of the string
elements are checked to see whether they actually correspond to an
existing directory.

Keep in mind that only those proxy candidates that match your current
input are available.  In particular, if `insert-default-directory' is
non-nil then you will want to use `\\[icicle-erase-minibuffer-or-history-element]' to remove the default
directory from the minibuffer when you want to match proxy candidates.

During completion (`*' means this requires library `Bookmark+'):

 *You can use `C-x a +' or `C-x a -' to add or remove tags from the
   current-candidate file.  You are prompted for the tags.
 *You can use `C-x m' to access file bookmarks (not just autofiles).
  You can use `C-c +' to create a new directory.
  You can use `\\[icicle-all-candidates-list-alt-action]' to open Dired on currently matching file names.
  You can use `\\[icicle-delete-candidate-object]' to delete a candidate file or (empty) dir.

These options, when non-nil, control candidate matching and filtering:

 `icicle-file-extras'           - Extra directory names to display
 `icicle-file-match-regexp'     - Regexp directory names must match
 `icicle-file-no-match-regexp'  - Regexp dir names must not match
 `icicle-file-predicate'        - Predicate the dir names must satisfy
 `icicle-file-sort'             - Sort function for candidates

Option `icicle-file-require-match-flag' can be used to override
option `icicle-require-match-flag'.

Option `icicle-files-ido-like' non-nil gives this command a more
Ido-like behavior."                     ; Doc string
  (lambda (name)                        ; FREE here: DIR-NAMES.
    (if (member (file-name-nondirectory name) ; Do this because choosing candidate adds default dir to it.
                keep-proxy-cands)
        (setq name       (symbol-value (intern (file-name-nondirectory name)))
              dir-names  (append (icicle-remove-if-not #'stringp name) dir-names))
      (push name dir-names))
    (when (interactive-p)
      (message "Added directory name `%s'" (icicle-propertize name 'face 'icicle-msg-emphasis))
      (sit-for 1)))
  prompt nil nil t nil nil              ; `read-file-name' args
  (icicle-file-bindings                 ; Bindings
   ((prompt                             (or icicle-prompt ; Allow override.
                                            "Choose directory (`RET' when done): "))
    (dir-names                          ())
    (icicle-exclude-default-proxies     t) ; Exclude non-dir file-name proxy candidates.
    (icicle-proxy-candidates            ; Remove vars whose vals are not lists or are lists with no strings.
     (let ((ipc  ()))
       (when icicle-add-proxy-candidates-flag
         (setq ipc  (mapcar #'symbol-name
                            (icicle-remove-if-not
                             (lambda (symb)
                               (and (boundp symb)  (consp (symbol-value symb))
                                    (let ((dirs  (symbol-value symb)))
                                      (catch 'icicle-directory-list
                                        (dolist (dir  dirs)
                                          (when (stringp dir) (throw 'icicle-directory-list t)))
                                        nil))))                         
                             icicle-path-variables))))
       ipc))
    (keep-proxy-cands                   icicle-proxy-candidates) ; Needed after `read-file-name' resets to nil.
    (user-file-pred                     icicle-file-predicate)
    (icicle-file-predicate              (if user-file-pred
                                            (lambda (f) ; FREE here: USER-FILE-PRED.
                                              (and (file-directory-p f)  (funcall user-file-pred f)))
                                          #'file-directory-p))
    (icicle-comp-base-is-default-dir-p  t)
    ;; $$$$$ (icicle-dir-candidate-can-exit-p (not current-prefix-arg))
    ))
  (progn (icicle-bind-file-candidate-keys) ; First code
         (when icicle-proxy-candidates (put-text-property 0 1 'icicle-fancy-candidates t prompt)))
  nil                                   ; Undo code
  (prog1 (setq dir-names  (nreverse (delete "" dir-names))) ; Last code - return the list of dirs
    (icicle-unbind-file-candidate-keys)
    (setq icicle-proxy-candidates  ())
    (when (interactive-p) (message "Directories: %S" dir-names))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-cmd1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-cmd1.el ends here

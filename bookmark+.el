;;; bookmark+.el - Bookmark+: extensions to standard library `bookmark.el'.
;;
;; Filename: bookmark+.el
;; Description: Bookmark+: extensions to standard library `bookmark.el'.
;; Author: Drew Adams, Thierry Volpiatto
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2000-2010, Drew Adams, all rights reserved.
;; Copyright (C) 2009, Thierry Volpiatto, all rights reserved.
;; Created: Fri Sep 15 07:58:41 2000
;; Last-Updated: Tue Jul 13 23:43:08 2010 (-0700)
;;           By: dradams
;;     Update #: 14914
;; URL: http://www.emacswiki.org/cgi-bin/wiki/bookmark+.el
;; Keywords: bookmarks, bookmark+, placeholders, annotations, search, info, w3m, gnus
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `bookmark', `bookmark+', `bookmark+-lit', `ffap', `pp'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Bookmark+: extensions to standard library `bookmark.el'.
;;
;;    The Bookmark+ libraries are:
;;
;;    `bookmark+.el'     - main code library (this file)
;;    `bookmark+-lit.el' - code for highlighting bookmarks (optional)
;;    `bookmark+-doc.el' - documentation (comment-only file)
;;    `bookmark+-chg.el' - change log (comment-only file)
;;
;;    The documentation (in `bookmark+-doc.el') includes how to
;;    byte-compile and install Bookmark+.  The documentation is also
;;    available in these ways:
;;
;;    1. From the bookmark list (`C-x r l'):
;;       Use `?' to show the current bookmark-list status and general
;;       help, then click link `Doc in Commentary' or link `Doc on the
;;       Web'.
;;
;;    2. From the Emacs-Wiki Web site:
;;       http://www.emacswiki.org/cgi-bin/wiki/BookmarkPlus.
;;    
;;    3. From the Bookmark+ group customization buffer:
;;       `M-x customize-group bookmark-plus', then click link
;;       `Commentary'.
;;
;;    (The commentary links in #1 and #3 work only if you have library
;;    `bookmark+-doc.el' in your `load-path'.)
;;
;;
;;    ****** NOTE ******
;;
;;      On 2010-06-18, I changed the prefix used by package Bookmark+
;;      from `bookmarkp-' to `bmkp-'.  THIS IS AN INCOMPATIBLE CHANGE.
;;      I apologize for the inconvenience, but the new prefix is
;;      preferable for a number of reasons, including easier
;;      distinction from standard `bookmark.el' names.
;;
;;      This change means that YOU MUST MANUALLY REPLACE ALL
;;      OCCURRENCES of `bookmarkp-' by `bmkp-' in the following
;;      places, if you used Bookmark+ prior to this change:
;;
;;      1. In your init file (`~/.emacs') or your `custom-file', if
;;         you have one.  This is needed if you customized any
;;         Bookmark+ features.
;;
;;      2. In your default bookmark file, `bookmark-default-file'
;;         (`.emacs.bmk'), and in any other bookmark files you might
;;         have.
;;
;;      3. In your `*Bookmark List*' state file,
;;         `bmkp-bmenu-state-file' (`~/.emacs-bmk-bmenu-state.el').
;;
;;      4. In your `*Bookmark List*' commands file,
;;         `bmkp-bmenu-commands-file' (`~/.emacs-bmk-bmenu-commands.el'),
;;         if you have one.
;;
;;      Again, sorry for this inconvenience.
 
;;(@> "Index")
;;
;;  Index
;;  -----
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Things Defined Here")
;;  (@> "Macros")
;;  (@> "Faces (Customizable)")
;;  (@> "User Options (Customizable)")
;;  (@> "Internal Variables")
;;  (@> "Compatibility Code for Older Emacs Versions")
;;  (@> "Core Replacements (`bookmark-*' except `bookmark-bmenu-*')")
;;  (@> "Menu List Replacements (`bookmark-bmenu-*')")
;;  (@> "Bookmark+ Functions (`bmkp-*')")
;;    (@> "Menu-List (`*-bmenu-*') Filter Commands")
;;    (@> "Menu-List (`*-bmenu-*') Commands Involving Marks")
;;    (@> "Omitted Bookmarks")
;;    (@> "Search-and-Replace Locations of Marked Bookmarks")
;;    (@> "Tags")
;;    (@> "General Menu-List (`-*bmenu-*') Commands and Functions")
;;    (@> "Bookmark Predicates")
;;    (@> "Filter Functions")
;;    (@> "General Utility Functions")
;;    (@> "Bookmark Entry Access Functions")
;;    (@> "Sorting - General Functions")
;;    (@> "Sorting - Commands")
;;    (@> "Sorting - General Predicates")
;;    (@> "Sorting - File-Name Predicates")
;;    (@> "Other Bookmark+ Functions (`bmkp-*')")
;;  (@> "Keymaps")
 
;;(@* "Things Defined Here")
;;
;;  Things Defined Here
;;  -------------------
;;
;;  Commands defined here:
;;
;;    `bmkp-add-tags', `bmkp-all-tags-jump',
;;    `bmkp-all-tags-jump-other-window', `bmkp-all-tags-regexp-jump',
;;    `bmkp-all-tags-regexp-jump-other-window', `bmkp-bmenu-add-tags',
;;    `bmkp-bmenu-add-tags-to-marked', `bmkp-bmenu-change-sort-order',
;;    `bmkp-bmenu-change-sort-order-repeat',
;;    `bmkp-bmenu-define-command',
;;    `bmkp-bmenu-define-full-snapshot-command',
;;    `bmkp-bmenu-define-jump-marked-command',
;;    `bmkp-bmenu-delete-marked', `bmkp-bmenu-describe-marked',
;;    `bmkp-bmenu-describe-this+move-down',
;;    `bmkp-bmenu-describe-this+move-up',
;;    `bmkp-bmenu-describe-this-bookmark',`bmkp-bmenu-dired-marked',
;;    `bmkp-bmenu-edit-bookmark',
;;    `bmkp-bmenu-filter-bookmark-name-incrementally',
;;    `bmkp-bmenu-filter-file-name-incrementally',
;;    `bmkp-bmenu-filter-tags-incrementally',
;;    `bmkp-bmenu-isearch-marked-bookmarks' (Emacs 23+),
;;    `bmkp-bmenu-isearch-marked-bookmarks-regexp' (Emacs 23+),
;;    `bmkp-bmenu-make-sequence-from-marked', `bmkp-bmenu-mark-all',
;;    `bmkp-bmenu-mark-bookmark-file-bookmarks',
;;    `bmkp-bmenu-mark-bookmarks-satisfying',
;;    `bmkp-bmenu-mark-bookmarks-tagged-all',
;;    `bmkp-bmenu-mark-bookmarks-tagged-none',
;;    `bmkp-bmenu-mark-bookmarks-tagged-not-all',
;;    `bmkp-bmenu-mark-bookmarks-tagged-regexp',
;;    `bmkp-bmenu-mark-bookmarks-tagged-some',
;;    `bmkp-bmenu-mark-desktop-bookmarks',
;;    `bmkp-bmenu-mark-dired-bookmarks',
;;    `bmkp-bmenu-mark-file-bookmarks',
;;    `bmkp-bmenu-mark-gnus-bookmarks',
;;    `bmkp-bmenu-mark-info-bookmarks',
;;    `bmkp-bmenu-mark-lighted-bookmarks',
;;    `bmkp-bmenu-mark-man-bookmarks',
;;    `bmkp-bmenu-mark-non-file-bookmarks',
;;    `bmkp-bmenu-mark-region-bookmarks',
;;    `bmkp-bmenu-mark-specific-buffer-bookmarks',
;;    `bmkp-bmenu-mark-specific-file-bookmarks',
;;    `bmkp-bmenu-mark-w3m-bookmarks', `bmkp-bmenu-mouse-3-menu',
;;    `bmkp-bmenu-mode-status-help', `bmkp-bmenu-omit',
;;    `bmkp-bmenu-omit-marked', `bmkp-bmenu-omit/unomit-marked',
;;    `bmkp-bmenu-query-replace-marked-bookmarks-regexp',
;;    `bmkp-bmenu-quit', `bmkp-bmenu-refresh-menu-list',
;;    `bmkp-bmenu-regexp-mark', `bmkp-bmenu-remove-all-tags',
;;    `bmkp-bmenu-remove-tags', `bmkp-bmenu-remove-tags-from-marked',
;;    `bmkp-bmenu-search-marked-bookmarks-regexp',
;;    `bmkp-bmenu-set-tag-value',
;;    `bmkp-bmenu-set-tag-value-for-marked', `bmkp-bmenu-show-all',
;;    `bmkp-bmenu-show-only-bookmark-files',
;;    `bmkp-bmenu-show-only-desktops', `bmkp-bmenu-show-only-dired',
;;    `bmkp-bmenu-show-only-files', `bmkp-bmenu-show-only-gnus',
;;    `bmkp-bmenu-show-only-info-nodes',
;;    `bmkp-bmenu-show-only-man-pages',
;;    `bmkp-bmenu-show-only-non-files',
;;    `bmkp-bmenu-show-only-omitted', `bmkp-bmenu-show-only-regions',
;;    `bmkp-bmenu-show-only-specific-buffer',
;;    `bmkp-bmenu-show-only-specific-file',
;;    `bmkp-bmenu-show-only-tagged'.  `bmkp-bmenu-show-only-varlists',
;;    `bmkp-bmenu-show-only-w3m-urls',
;;    `bmkp-bmenu-sort-by-bookmark-name',
;;    `bmkp-bmenu-sort-by-bookmark-visit-frequency',
;;    `bmkp-bmenu-sort-by-creation-time',
;;    `bmkp-bmenu-sort-by-file-name',
;;    `bmkp-bmenu-sort-by-Gnus-thread',
;;    `bmkp-bmenu-sort-by-Info-location',
;;    `bmkp-bmenu-sort-by-last-bookmark-access',
;;    `bmkp-bmenu-sort-by-last-buffer-or-file-access',
;;    `bmkp-bmenu-sort-by-last-local-file-access',
;;    `bmkp-bmenu-sort-by-last-local-file-update',
;;    `bmkp-bmenu-sort-by-local-file-size',
;;    `bmkp-bmenu-sort-by-local-file-type',
;;    `bmkp-bmenu-sort-by-w3m-url',
;;    `bmkp-bmenu-sort-marked-before-unmarked',
;;    `bmkp-bmenu-toggle-marks', `bmkp-bmenu-toggle-show-only-marked',
;;    `bmkp-bmenu-toggle-show-only-unmarked', `bmkp-bmenu-unmark-all',
;;    `bmkp-bmenu-unmark-bookmarks-tagged-all',
;;    `bmkp-bmenu-unmark-bookmarks-tagged-none',
;;    `bmkp-bmenu-unmark-bookmarks-tagged-not-all',
;;    `bmkp-bmenu-unmark-bookmarks-tagged-some',
;;    `bmkp-bmenu-unomit-marked', `bmkp-bmenu-w32-open',
;;    `bmkp-bmenu-w32-open-select', `bmkp-bmenu-w32-open-with-mouse',
;;    `bmkp-bookmark-file-jump', `bmkp-bookmark-list-jump',
;;    `bmkp-choose-navlist-from-bookmark-list',
;;    `bmkp-choose-navlist-of-type', `bmkp-crosshairs-highlight',
;;    `bmkp-cycle', `bmkp-cycle-other-window',
;;    `bmkp-cycle-this-buffer', `bmkp-cycle-this-buffer-other-window',
;;    `bmkp-define-tags-sort-command',
;;    `bmkp-delete-all-autonamed-for-this-buffer',
;;    `bmkp-delete-bookmarks', `bmkp-describe-bookmark',
;;    `bmkp-describe-bookmark-internals', `bmkp-desktop-change-dir',
;;    `bmkp-desktop-delete', `bmkp-desktop-jump', `bmkp-desktop-read',
;;    `bmkp-dired-jump', `bmkp-dired-jump-current',
;;    `bmkp-dired-jump-current-other-window',
;;    `bmkp-dired-jump-other-window', `bmkp-empty-file',
;;    `bmkp-file-jump', `bmkp-file-jump-other-window',
;;    `bmkp-gnus-jump', `bmkp-gnus-jump-other-window',
;;    `bmkp-info-jump', `bmkp-info-jump-other-window',
;;    `bmkp-jump-in-navlist', `bmkp-jump-in-navlist-other-window',
;;    `bmkp-jump-to-type', `bmkp-jump-to-type-other-window',
;;    `bmkp-list-all-tags', `bmkp-list-defuns-in-commands-file',
;;    `bmkp-local-file-jump', `bmkp-local-file-jump-other-window',
;;    `bmkp-make-function-bookmark', `bmkp-man-jump',
;;    `bmkp-man-jump-other-window', `bmkp-menu-jump-other-window'
;;    (Emacs 20, 21), `bmkp-navlist-bmenu-list', `bmkp-next-bookmark',
;;    `bmkp-next-bookmark-repeat', `bmkp-next-bookmark-this-buffer',
;;    `bmkp-next-bookmark-this-buffer-repeat',
;;    `bmkp-next-bookmark-w32', `bmkp-next-bookmark-w32-repeat',
;;    `bmkp-non-file-jump', `bmkp-non-file-jump-other-window',
;;    `bmkp-occur-create-autonamed-bookmarks',
;;    `bmkp-previous-bookmark', `bmkp-previous-bookmark-repeat',
;;    `bmkp-previous-bookmark-this-buffer',
;;    `bmkp-previous-bookmark-this-buffer-repeat',
;;    `bmkp-previous-bookmark-w32',
;;    `bmkp-previous-bookmark-w32-repeat',
;;    `bmkp-read-bookmark-for-type', `bmkp-region-jump',
;;    `bmkp-region-jump-other-window', `bmkp-remote-file-jump',
;;    `bmkp-remote-file-jump-other-window', `bmkp-remove-all-tags',
;;    `bmkp-remove-tags', `bmkp-remove-tags-from-all',
;;    `bmkp-rename-tag', `bmkp-reverse-multi-sort-order',
;;    `bmkp-reverse-sort-order', `bmkp-send-bug-report',
;;    `bmkp-set-autonamed-bookmark',
;;    `bmkp-set-autonamed-bookmark-at-line',
;;    `bmkp-set-autonamed-regexp-buffer',
;;    `bmkp-set-autonamed-regexp-region',
;;    `bmkp-set-bookmark-file-bookmark', `bmkp-set-desktop-bookmark',
;;    `bmkp-set-restrictions-bookmark', `bmkp-set-tag-value',
;;    `bmkp-set-tag-value-for-navlist', `bmkp-set-varlist-bookmark',
;;    `bmkp-some-tags-jump', `bmkp-some-tags-jump-other-window',
;;    `bmkp-some-tags-regexp-jump',
;;    `bmkp-some-tags-regexp-jump-other-window',
;;    `bmkp-specific-buffers-jump',
;;    `bmkp-specific-buffers-jump-other-window',
;;    `bmkp-specific-files-jump',
;;    `bmkp-specific-files-jump-other-window',
;;    `bmkp-switch-bookmark-file',
;;    `bmkp-switch-to-last-bookmark-file',
;;    `bmkp-this-buffer-bmenu-list', `bmkp-this-buffer-jump',
;;    `bmkp-this-buffer-jump-other-window',
;;    `bmkp-toggle-autonamed-bookmark-set/delete',
;;    `bmkp-toggle-bookmark-set-refreshes',
;;    `bmkp-toggle-saving-bookmark-file',
;;    `bmkp-toggle-saving-menu-list-state',
;;    `bmkp-toggle-bookmark-set-refreshes', `bmkp-unomit-all',
;;    `bmkp-use-bookmark-file-create', `bmkp-varlist-jump',
;;    `bmkp-version', `bmkp-w3m-jump', `bmkp-w3m-jump-other-window',
;;    `old-bookmark-insert', `old-bookmark-relocate'.
;;
;;  User options defined here:
;;
;;    `bmkp-autoname-bookmark-function', `bmkp-autoname-format',
;;    `bmkp-bmenu-commands-file', `bmkp-bmenu-omitted-list',
;;    `bmkp-bmenu-state-file', `bmkp-bookmark-name-length-max',
;;    `bmkp-crosshairs-flag', `bmkp-desktop-no-save-vars',
;;    `bmkp-handle-region-function', `bmkp-incremental-filter-delay',
;;    `bmkp-menu-popup-max-length', `bmkp-other-window-pop-to-flag',
;;    `bmkp-prompt-for-tags-flag', `bmkp-region-search-size',
;;    `bmkp-save-new-location-flag',
;;    `bmkp-sequence-jump-display-function',
;;    `bmkp-show-end-of-region', `bmkp-sort-comparer',
;;    `bmkp-sort-orders-alist', `bmkp-sort-orders-for-cycling-alist',
;;    `bmkp-su-or-sudo-regexp',
;;    `bmkp-this-buffer-cycle-sort-comparer', `bmkp-use-region',
;;    `bmkp-w3m-allow-multi-tabs'.
;;
;;  Faces defined here:
;;
;;    `bmkp->-mark', `bmkp-a-mark', `bmkp-bad-bookmark',
;;    `bmkp-bookmark-file', `bmkp-bookmark-list', `bmkp-buffer',
;;    `bmkp-D-mark', `bmkp-desktop', `bmkp-function', `bmkp-gnus',
;;    `bmkp-heading', `bmkp-info', `bmkp-local-directory',
;;    `bmkp-local-file-with-region', `bmkp-local-file-without-region',
;;    `bmkp-man', `bmkp-non-file', `bmkp-remote-file',
;;    `bmkp-sequence', `bmkp-su-or-sudo', `bmkp-t-mark',
;;    `bmkp-varlist', `bmkp-w3m'.
;;
;;  Macros defined here:
;;
;;    `bmkp-define-file-sort-predicate', `bmkp-define-sort-command',
;;    `bmkp-menu-bar-make-toggle'.
;;
;;  Non-interactive functions defined here:
;;
;;    `bmkext-jump-gnus', `bmkext-jump-man', `bmkext-jump-w3m',
;;    `bmkext-jump-woman', `bmkp-all-tags-alist-only',
;;    `bmkp-all-tags-regexp-alist-only', `bmkp-alpha-cp',
;;    `bmkp-alpha-p', `bmkp-assoc-delete-all',
;;    `bmkp-autoname-bookmark', `bmkp-autonamed-alist-only',
;;    `bmkp-autonamed-bookmark-for-buffer-p',
;;    `bmkp-autonamed-bookmark-p',
;;    `bmkp-autonamed-this-buffer-alist-only',
;;    `bmkp-barf-if-not-in-menu-list',
;;    `bmkp-bmenu-cancel-incremental-filtering',
;;    `bmkp-bmenu-filter-alist-by-bookmark-name-regexp',
;;    `bmkp-bmenu-filter-alist-by-file-name-regexp',
;;    `bmkp-bmenu-filter-alist-by-tags-regexp',
;;    `bmkp-bmenu-goto-bookmark-named', `bmkp-bmenu-list-1',
;;    `bmkp-bmenu-mark/unmark-bookmarks-tagged-all/none',
;;    `bmkp-bmenu-mark/unmark-bookmarks-tagged-some/not-all',
;;    `bmkp-bmenu-propertize-item', `bmkp-bmenu-read-filter-input',
;;    `bmkp-bookmark-creation-cp', `bmkp-bookmark-description',
;;    `bmkp-bookmark-last-access-cp', `bmkp-bookmark-file-alist-only',
;;    `bmkp-bookmark-list-alist-only',
;;    `bmkp-bookmark-file-bookmark-p',
;;    `bmkp-bookmark-list-bookmark-p', `bmkp-buffer-last-access-cp',
;;    `bmkp-buffer-names', `bmkp-completing-read-1',
;;    `bmkp-completing-read-buffer-name',
;;    `bmkp-completing-read-file-name', `bmkp-completing-read-lax',
;;    `bmkp-cp-not', `bmkp-create-varlist-bookmark',
;;    `bmkp-current-bookmark-list-state', `bmkp-current-sort-order',
;;    `bmkp-cycle-1', `bmkp-default-bookmark-name',
;;    `bmkp-desktop-alist-only', `bmkp-desktop-bookmark-p',
;;    `bmkp-desktop-kill', `bmkp-dired-alist-only',
;;    `bmkp-dired-bookmark-p', `bmkp-dired-subdirs',
;;    `bmkp-edit-bookmark', `bmkp-end-position-post-context',
;;    `bmkp-end-position-pre-context', `bmkp-every', `bmkp-face-prop',
;;    `bmkp-file-alist-only', `bmkp-file-alpha-cp',
;;    `bmkp-file-attribute-0-cp', `bmkp-file-attribute-1-cp',
;;    `bmkp-file-attribute-2-cp', `bmkp-file-attribute-3-cp',
;;    `bmkp-file-attribute-4-cp', `bmkp-file-attribute-5-cp',
;;    `bmkp-file-attribute-6-cp', `bmkp-file-attribute-7-cp',
;;    `bmkp-file-attribute-8-cp', `bmkp-file-attribute-9-cp',
;;    `bmkp-file-attribute-10-cp', `bmkp-file-attribute-11-cp',
;;    `bmkp-file-bookmark-p', `bmkp-file-names', `bmkp-file-remote-p',
;;    `bmkp-float-time', `bmkp-full-tag', `bmkp-function-bookmark-p',
;;    `bmkp-get-buffer-name', `bmkp-get-end-position',
;;    `bmkp-get-tag-value', `bmkp-get-tags', `bmkp-get-visit-time',
;;    `bmkp-get-visits-count', `bmkp-gnus-alist-only',
;;    `bmkp-gnus-bookmark-p', `bmkp-gnus-cp', `bmkp-goto-position',
;;    `bmkp-handle-region-default', `bmkp-handler-cp',
;;    `bmkp-has-tag-p', `bmkp-info-alist-only',
;;    `bmkp-info-bookmark-p', `bmkp-info-cp', `bmkp-isearch-bookmarks'
;;    (Emacs 23+), `bmkp-isearch-bookmarks-regexp' (Emacs 23+),
;;    `bmkp-isearch-next-bookmark-buffer' (Emacs 23+), `bmkp-jump-1',
;;    `bmkp-jump-bookmark-file', `bmkp-jump-bookmark-list',
;;    `bmkp-jump-desktop', `bmkp-jump-dired', `bmkp-jump-function',
;;    `bmkp-jump-gnus', `bmkp-jump-man', `bmkp-jump-sequence',
;;    `bmkp-jump-varlist', `bmkp-jump-w3m',
;;    `bmkp-jump-w3m-new-session', `bmkp-jump-w3m-only-one-tab',
;;    `bmkp-jump-woman', `bmkp-last-specific-buffer-alist-only',
;;    `bmkp-last-specific-buffer-p',
;;    `bmkp-last-specific-file-alist-only',
;;    `bmkp-last-specific-file-p', `bmkp-line-number-at-pos',
;;    `bmkp-list-position', `bmkp-local-directory-bookmark-p',
;;    `bmkp-local-file-accessed-more-recently-cp',
;;    `bmkp-local-file-alist-only', `bmkp-local-file-bookmark-p',
;;    `bmkp-local-file-size-cp', `bmkp-local-file-type-cp',
;;    `bmkp-local-file-updated-more-recently-cp',
;;    `bmkp-make-bookmark-file-record',
;;    `bmkp-make-bookmark-list-record', `bmkp-make-desktop-record',
;;    `bmkp-make-dired-record', `bmkp-make-gnus-record',
;;    `bmkp-make-man-record', `bmkp-make-plain-predicate',
;;    `bmkp-make-varlist-record', `bmkp-make-w3m-record',
;;    `bmkp-make-woman-record' (Emacs 21+), `bmkp-man-alist-only',
;;    `bmkp-man-bookmark-p', `bmkp-marked-bookmark-p',
;;    `bmkp-marked-bookmarks-only', `bmkp-marked-cp',
;;    `bmkp-maybe-save-bookmarks', `bmkp-msg-about-sort-order',
;;    `bmkp-multi-sort', `bmkp-non-autonamed-alist-only',
;;    `bmkp-non-file-alist-only', `bmkp-non-file-bookmark-p',
;;    `bmkp-omitted-alist-only', `bmkp-position-after-whitespace',
;;    `bmkp-position-before-whitespace', `bmkp-position-cp',
;;    `bmkp-position-post-context',
;;    `bmkp-position-post-context-region',
;;    `bmkp-position-pre-context', `bmkp-position-pre-context-region',
;;    `bmkp-printable-p', `bmkp-printable-vars+vals',
;;    `bmkp-read-tag-completing', `bmkp-read-tags',
;;    `bmkp-read-tags-completing', `bmkp-read-variable',
;;    `bmkp-read-variables-completing', `bmkp-record-visit',
;;    `bmkp-refresh-latest-bookmark-list', `bmkp-refresh-menu-list',
;;    `bmkp-regexp-filtered-bookmark-name-alist-only',
;;    `bmkp-regexp-filtered-file-name-alist-only',
;;    `bmkp-regexp-filtered-tags-alist-only',
;;    `bmkp-region-alist-only', `bmkp-region-bookmark-p',
;;    `bmkp-remote-file-alist-only', `bmkp-remote-file-bookmark-p',
;;    `bmkp-remove-assoc-dups', `bmkp-remove-dups', `bmkp-remove-if',
;;    `bmkp-remove-if-not', `bmkp-repeat-command',
;;    `bmkp-replace-regexp-in-string', `bmkp-root-or-sudo-logged-p',
;;    `bmkp-same-creation-time-p', `bmkp-same-file-p',
;;    `bmkp-save-menu-list-state', `bmkp-save-new-region-location',
;;    `bmkp-select-buffer-other-window', `bmkp-sequence-bookmark-p',
;;    `bmkp-set-tag-value-for-bookmarks', `bmkp-set-union',
;;    `bmkp-some', `bmkp-some-marked-p', `bmkp-some-tags-alist-only',
;;    `bmkp-some-tags-regexp-alist-only', `bmkp-some-unmarked-p'
;;    `bmkp-sort-and-remove-dups', `bmkp-specific-buffers-alist-only',
;;    `bmkp-specific-files-alist-only', `bmkp-tag-name',
;;    `bmkp-tags-list', `bmkp-this-buffer-alist-only',
;;    `bmkp-this-buffer-p', `bmkp-this-file-alist-only',
;;    `bmkp-this-file-p', `bmkp-unmarked-bookmarks-only',
;;    `bmkp-upcase', `bmkp-update-autonamed-bookmark',
;;    `bmkp-varlist-alist-only', `bmkp-varlist-bookmark-p',
;;    `bmkp-visited-more-cp', `bmkp-w3m-alist-only',
;;    `bmkp-w3m-bookmark-p', `bmkp-w3m-cp',
;;    `bmkp-w3m-set-new-buffer-name'.
;;
;;  Internal variables defined here:
;;
;;    `bmkp-after-set-hook', `bmkp-bmenu-before-hide-marked-alist',
;;    `bmkp-bmenu-before-hide-unmarked-alist',
;;    `bmkp-bmenu-define-command-menu', `bmkp-bmenu-filter-function',
;;    `bmkp-bmenu-filter-pattern', `bmkp-bmenu-filter-prompt',
;;    `bmkp-bmenu-filter-timer', `bmkp-bmenu-first-time-p',
;;    `bmkp-bmenu-header-lines', `bmkp-bmenu-highlight-menu',
;;    `bmkp-bmenu-line-overlay', `bmkp-bmenu-mark-menu',
;;    `bmkp-bmenu-marked-bookmarks', `bmkp-bmenu-marks-width',
;;    `bmkp-bmenu-menubar-menu', `bmkp-bmenu-omit-menu',
;;    `bmkp-bmenu-show-menu', `bmkp-bmenu-sort-menu',
;;    `bmkp-bmenu-tags-menu', `bmkp-bmenu-title',
;;    `bmkp-bookmark-file-history', `bmkp-bookmark-list-history',
;;    `bmkp-current-bookmark-file', `bmkp-current-nav-bookmark',
;;    `bmkp-desktop-history', `bmkp-dired-history',
;;    `bmkp-file-history', `bmkp-gnus-history', `bmkp-highlight-menu',
;;    `bmkp-info-history', `bmkp-isearch-bookmarks' (Emacs 23+),
;;    `bmkp-jump-display-function', `bmkp-jump-map', `bmkp-jump-menu',
;;    `bmkp-jump-other-window-map', `bmkp-last-bmenu-bookmark',
;;    `bmkp-last-bmenu-state-file', `bmkp-last-bookmark-file',
;;    `bmkp-last-save-flag-value', `bmkp-last-specific-buffer',
;;    `bmkp-last-specific-file', `bmkp-latest-bookmark-alist',
;;    `bmkp-local-file-history', `bmkp-man-history', `bmkp-nav-alist',
;;    `bmkp-non-file-filename', `bmkp-non-file-history',
;;    `bmkp-options-menu', `bmkp-region-history',
;;    `bmkp-remote-file-history', `bmkp-reverse-multi-sort-p',
;;    `bmkp-reverse-sort-p', `bmkp-sorted-alist',
;;    `bmkp-specific-buffers-history', `bmkp-specific-files-history',
;;    `bmkp-tag-history', `bmkp-tags-alist', `bmkp-types-alist',
;;    `bmkp-use-w32-browser-p', `bmkp-varlist-history',
;;    `bmkp-version-number', `bmkp-w3m-history'.
;;
;;
;;  ***** NOTE: The following commands defined in `bookmark.el'
;;              have been REDEFINED HERE:
;;
;;    `bookmark-bmenu-execute-deletions', `bookmark-bmenu-list',
;;    `bookmark-bmenu-mark', `bookmark-bmenu-1-window',
;;    `bookmark-bmenu-2-window', `bookmark-bmenu-other-window',
;;    `bookmark-bmenu-other-window-with-mouse',
;;    `bookmark-bmenu-this-window', `bookmark-bmenu-rename',
;;    `bookmark-bmenu-show-annotation',
;;    `bookmark-bmenu-switch-other-window', `bookmark-bmenu-unmark',
;;    `bookmark-delete', `bookmark-insert',
;;    `bookmark-insert-location', `bookmark-jump',
;;    `bookmark-jump-other-window', `bookmark-load',
;;    `bookmark-relocate', `bookmark-rename', `bookmark-save',
;;    `bookmark-send-edited-annotation', `bookmark-set',
;;    `bookmark-yank-word'.
;;
;;
;;  ***** NOTE: The following non-interactive functions defined in
;;              `bookmark.el' have been REDEFINED HERE:
;;
;;    `bookmark--jump-via', `bookmark-all-names',
;;    `bookmark-bmenu-bookmark', `bookmark-bmenu-check-position',
;;    `bookmark-bmenu-delete', `bookmark-bmenu-ensure-position' (Emacs
;;    23.2+), `bookmark-bmenu-hide-filenames', `bookmark-bmenu-mode',
;;    `bookmark-bmenu-show-filenames',
;;    `bookmark-bmenu-surreptitiously-rebuild-list',
;;    `bookmark-bmenu-switch-other-window' (Emacs 20-22),
;;    `bookmark-completing-read', `bookmark-default-handler',
;;    `bookmark-exit-hook-internal', `bookmark-get-bookmark' (Emacs
;;    20-22), `bookmark-get-bookmark-record' (Emacs 20-22),
;;    `bookmark-get-handler' (Emacs 20-22),
;;    `bookmark-handle-bookmark', `bookmark-jump-noselect' (Emacs
;;    20-22), `bookmark-location', `bookmark-make-record' (Emacs
;;    20-22), `bookmark-make-record-default', `bookmark-maybe-message'
;;    (Emacs 20-21), `bookmark-prop-get' (Emacs 20-22),
;;    `bookmark-prop-set', `bookmark-show-annotation',
;;    `bookmark-show-all-annotations', `bookmark-store' (Emacs 20-22),
;;    `bookmark-write-file'.
;;
;;
;;  ***** NOTE: The following variables defined in `bookmark.el'
;;              have been REDEFINED HERE:
;;
;;    `bookmark-alist' (doc string only),
;;    `bookmark-make-record-function' (Emacs 20-22).
;;
;;
;;  ***** NOTE: The following functions defined in `info.el'
;;              have been REDEFINED HERE:
;;
;;    `Info-bookmark-jump' (Emacs 20-22), `Info-bookmark-make-record'
;;    (Emacs 20-22).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
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

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'bookmark+)
(require 'bookmark+)                    ; Ensure this library is loaded before we compile it.
                                        ; So be sure to put this library in your `load-path' before
                                        ; trying to byte-compile it.

(require 'bookmark+-lit nil t)          ; Soft require - no error if not found.  If you do not want
                                        ; to use `bookmark+-lit.el' then simply do not put it in your
                                        ; `load-path'.

;;;;;;;;;;;;;;;;;;;;;;;

(require 'bookmark)
(unless (fboundp 'file-remote-p) (require 'ffap)) ;; ffap-file-remote-p
(eval-when-compile (require 'gnus)) ;; mail-header-id (really in `nnheader.el')
(eval-when-compile (require 'cl)) ;; case, multiple-value-bind

;;;;;;;;;;;;;;;;;;;;;;;

(defconst bmkp-version-number "3.1.0")

(defun bmkp-version ()
  "Show version number of library `bookmark+.el'."
  (interactive)
  (message "Bookmark+, version %s" bmkp-version-number))


;; Quiet the byte-compiler
(defvar bookmark-current-point)         ; Defined in `bookmark.el', but not in Emacs 23+.
(defvar bookmark-make-record-function)  ; Defined in `bookmark.el'.
(defvar desktop-buffer-args-list)       ; Defined in `desktop.el'.
(defvar desktop-delay-hook)             ; Defined in `desktop.el'.
(defvar desktop-dirname)                ; Defined in `desktop.el'.
(defvar desktop-file-modtime)           ; Defined in `desktop.el'.
(defvar desktop-globals-to-save)        ; Defined in `desktop.el'.
(defvar desktop-save-mode)              ; Defined in `desktop.el'.
(defvar desktop-save)                   ; Defined in `desktop.el'.
(defvar dired-actual-switches)          ; Defined in `dired.el'.
(defvar dired-buffers)                  ; Defined in `dired.el'.
(defvar dired-directory)                ; Defined in `dired.el'.
(defvar dired-mode-map)                 ; Defined in `dired.el'.
(defvar dired-re-mark)                  ; Defined in `dired.el'.
(defvar dired-subdir-alist)             ; Defined in `dired.el'.
(defvar diredp-menu-bar-subdir-menu)    ; Defined in `dired+.el'.
(defvar gnus-article-current)           ; Defined in `gnus-sum.el'.
(defvar Info-current-node)              ; Defined in `info.el'.
(defvar Info-current-file)              ; Defined in `info.el'.
(defvar Info-mode-map)                  ; Defined in `info.el'.
(defvar Info-mode-menu)                 ; Defined in `info.el'.
(defvar Man-arguments)                  ; Defined in `man.el'.
(defvar Man-mode-map)                   ; Defined in `man.el'.
;;; (defvar mouse-wheel-down-event)         ; Defined in `mwheel.el'.
;;; (defvar mouse-wheel-up-event)           ; Defined in `mwheel.el'.
(defvar read-file-name-completion-ignore-case) ; Emacs 23+.
(defvar last-repeatable-command)        ; Defined in `repeat.el'.
(defvar repeat-last-self-insert)        ; Defined in `repeat.el'.
(defvar repeat-num-input-keys-at-repeat) ; Defined in `repeat.el'.
(defvar repeat-num-input-keys-at-self-insert) ; Defined in `repeat.el'.
(defvar repeat-on-final-keystroke)      ; Defined in `repeat.el'.
(defvar repeat-previous-repeated-command) ; Defined in `repeat.el'.
(defvar repeat-too-dangerous)           ; Defined in `repeat.el'.
(defvar repeat-undo-count)              ; Defined in `repeat.el'.
(defvar tramp-file-name-regexp)         ; Defined in `tramp.el'.
(defvar w3m-current-title)              ; Defined in `w3m.el'.
(defvar w3m-current-url)                ; Defined in `w3m.el'.
(defvar w3m-minor-mode-map)             ; Defined in `w3m.el'.
(defvar w3m-mode-map)                   ; Defined in `w3m.el'.
(defvar wide-n-restrictions)            ; Defined in `wide-n.el'.
(defvar woman-last-file-name)           ; Defined in `woman.el'.
(defvar woman-menu)                     ; Defined in `woman.el'.
(defvar woman-mode-map)                 ; Defined in `woman.el'.
 
;;(@* "Macros")
;;; Macros -----------------------------------------------------------

(defmacro bmkp-menu-bar-make-toggle (name variable doc message help &rest body)
  "Return a valid `menu-bar-make-toggle' call in Emacs 20 or later.
NAME is the name of the toggle command to define.
VARIABLE is the variable to set.
DOC is the menu-item name.
MESSAGE is the toggle message, minus status.
HELP is `:help' string.
BODY is the function body to use.  If present, it is responsible for
setting the variable and displaying a status message (not MESSAGE)."
  (if (< emacs-major-version 21)
      `(menu-bar-make-toggle ,name ,variable ,doc ,message ,@body)
    `(menu-bar-make-toggle ,name ,variable ,doc ,message ,help ,@body)))

;; Used in `bmkp-define-sort-command'.
(defun bmkp-assoc-delete-all (key alist)
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

;; Used in `bmkp-define-sort-command'.
(defun bmkp-replace-regexp-in-string (regexp rep string &optional fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING and return STRING."
  (if (fboundp 'replace-regexp-in-string) ; Emacs > 20.
      (replace-regexp-in-string regexp rep string fixedcase literal subexp start)
    (if (string-match regexp string) (replace-match rep nil nil string) string))) ; Emacs 20

(defmacro bmkp-define-sort-command (sort-order comparer doc-string)
  "Define a command to sort bookmarks in the bookmark list by SORT-ORDER.
SORT-ORDER is a short string or symbol describing the sorting method.
Examples: \"by last access time\", \"by bookmark name\".

The new command is named by replacing any spaces in SORT-ORDER with
hyphens (`-') and then adding the prefix `bmkp-bmenu-sort-'.  Example:
`bmkp-bmenu-sort-by-bookmark-name', for SORT-ORDER `by bookmark name'.

COMPARER compares two bookmarks, returning non-nil if and only if the
first bookmark sorts before the second.  It must be acceptable as a
value of `bmkp-sort-comparer'.  That is, it is either nil, a
predicate, or a list ((PRED...) FINAL-PRED).  See the doc for
`bmkp-sort-comparer'.

DOC-STRING is the doc string of the new command."
  (unless (stringp sort-order) (setq sort-order  (symbol-name sort-order)))
  (let ((command  (intern (concat "bmkp-bmenu-sort-" (bmkp-replace-regexp-in-string
                                                      "\\s-+" "-" sort-order)))))
    `(progn
      (setq bmkp-sort-orders-alist  (bmkp-assoc-delete-all ,sort-order (copy-sequence
                                                                        bmkp-sort-orders-alist)))
      (push (cons ,sort-order ',comparer) bmkp-sort-orders-alist)
      (defun ,command ()
        ,(concat doc-string "\nRepeating this command cycles among normal sort, reversed \
sort, and unsorted.")
        (interactive)
        (bmkp-barf-if-not-in-menu-list)
        (cond (;; Not this sort order - make it this sort order.
               (not (equal bmkp-sort-comparer ',comparer))
               (setq bmkp-sort-comparer   ',comparer
                     bmkp-reverse-sort-p  nil))
              (;; This sort order reversed.  Change to unsorted.
               bmkp-reverse-sort-p
               (setq bmkp-sort-comparer   nil))
              (t;; This sort order - reverse it.
               (setq bmkp-reverse-sort-p  t)))
        (message "Sorting...")
        (bookmark-bmenu-ensure-position)
        (let ((current-bmk  (bookmark-bmenu-bookmark)))
          (bookmark-bmenu-surreptitiously-rebuild-list)
          (bmkp-bmenu-goto-bookmark-named current-bmk)) ; Put cursor back on right line.
        (when (interactive-p)
          (bmkp-msg-about-sort-order
           ,sort-order
           nil
           (cond ((and (not bmkp-reverse-sort-p)
                       (equal bmkp-sort-comparer ',comparer)) "(Repeat: reverse)")
                 ((equal bmkp-sort-comparer ',comparer)       "(Repeat: unsorted)")
                 (t                                           "(Repeat: sort)"))))))))

(defmacro bmkp-define-file-sort-predicate (att-nb)
  "Define a predicate for sorting bookmarks by file attribute ATT-NB.
See function `file-attributes' for the meanings of the various file
attribute numbers.

String attribute values sort alphabetically; numerical values sort
numerically; nil sorts before t.

For ATT-NB 0 (file type), a file sorts before a symlink, which sorts
before a directory.

For ATT-NB 2 or 3 (uid, gid), a numerical value sorts before a string
value.

A bookmark that has file attributes sorts before a bookmark that does
not.  A file bookmark sorts before a non-file bookmark.  Only local
files are tested for attributes - remote-file bookmarks are treated
here like non-file bookmarks."
  `(defun ,(intern (format "bmkp-file-attribute-%d-cp" att-nb)) (b1 b2)
    ,(format "Sort file bookmarks by attribute %d.
B1 and B2 are bookmarks or bookmark names.
Sort bookmarks with file attributes before those without attributes
Sort file bookmarks before non-file bookmarks.
Treat remote file bookmarks like non-file bookmarks."
             att-nb)
    (setq b1  (bookmark-get-bookmark b1))
    (setq b2  (bookmark-get-bookmark b2))
    (let (a1 a2)
      (cond (;; Both are file bookmarks.
             (and (bmkp-file-bookmark-p b1) (bmkp-file-bookmark-p b2))
             (setq a1  (file-attributes (bookmark-get-filename b1))
                   a2  (file-attributes (bookmark-get-filename b2)))
             (cond (;; Both have attributes.
                    (and a1 a2)
                    (setq a1  (nth ,att-nb a1)
                          a2  (nth ,att-nb a2))
                    ;; Convert times and maybe inode number to floats.
                    ;; The inode conversion is kludgy, but is probably OK in practice.
                    (when (consp a1) (setq a1  (bmkp-float-time a1)))
                    (when (consp a2) (setq a2  (bmkp-float-time a2)))
                    (cond (;; (1) links, (2) maybe uid, (3) maybe gid, (4, 5, 6) times
                           ;; (7) size, (10) inode, (11) device.
                           (numberp a1)
                           (cond ((< a1 a2)  '(t))
                                 ((> a1 a2)  '(nil))
                                 (t          nil)))
                          ((= 0 ,att-nb) ; (0) file (nil) < symlink (string) < dir (t)
                           (cond ((and a2 (not a1))               '(t)) ; file vs (symlink or dir)
                                 ((and a1 (not a2))               '(nil))
                                 ((and (eq t a2) (not (eq t a1))) '(t)) ; symlink vs dir
                                 ((and (eq t a1) (not (eq t a2))) '(t))
                                 ((and (stringp a1) (stringp a2))
                                  (if (string< a1 a2) '(t) '(nil)))
                                 (t                               nil)))
                          ((stringp a1) ; (2, 3) string uid/gid, (8) modes
                           (cond ((string< a1 a2)  '(t))
                                 ((string< a2 a1)  '(nil))
                                 (t                nil)))
                          ((eq ,att-nb 9) ; (9) gid would change if re-created. nil < t
                           (cond ((and a2 (not a1))  '(t))
                                 ((and a1 (not a2))  '(nil))
                                 (t                  nil)))))
                   (;; First has attributes, but not second.
                    a1
                    '(t))
                   (;; Second has attributes, but not first.
                    a2
                    '(nil))
                   (;; Neither has attributes.
                    t
                    nil)))
            (;; First is a file, second is not.
             (bmkp-local-file-bookmark-p b1)
             '(t))
            (;; Second is a file, first is not.
             (bmkp-local-file-bookmark-p b2)
             '(nil))
            (t;; Neither is a file.
             nil)))))
 
;;(@* "Faces (Customizable)")
;;; Faces (Customizable) ---------------------------------------------

(defgroup bookmark-plus nil
  "Bookmark enhancements."
  :prefix "bmkp-" :group 'bookmark
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
bookmark+.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "http://www.emacswiki.org/bookmark+.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/BookmarkPlus")
  :link '(emacs-commentary-link :tag "Commentary" "bookmark+"))

(defface bmkp->-mark '((t (:foreground "Blue")))
  ;; (:foreground "Magenta2" :box (:line-width 1 :style pressed-button))))
  "*Face used for a `>' mark in the bookmark list."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-a-mark '((t (:background "SkyBlue")))
  "*Face used for an annotation mark (`a') in the bookmark list."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-bad-bookmark '((t (:foreground "Red" :background "Chartreuse1")))
  "*Face used for a bookmark that seems to be bad: e.g., nonexistent file."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-bookmark-file
    '((((background dark)) (:foreground "DarkGreen" :background "Orange"))
      (t (:foreground "Orange" :background "DarkGreen")))
  "*Face used for a bookmark-file bookmark."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-bookmark-list
    '((((background dark)) (:foreground "LightGray" :background "DarkRed"))
      (t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for a bookmark-list bookmark."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-buffer
    '((((background dark)) (:foreground "green"))
      (t (:foreground "DarkGreen")))
  "*Face used for a bookmarked existing buffer not associated with a file."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-D-mark '((t (:foreground "Yellow" :background "Red")))
  "*Face used for a deletion mark (`D') in the bookmark list."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-desktop
    '((((background dark)) (:foreground "PaleGoldenrod" :background "DarkBlue"))
      (t (:foreground "DarkBlue" :background "PaleGoldenrod")))
  "*Face used for a bookmarked desktop."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-function
    '((((background dark)) (:foreground "DeepPink1"))
      (t (:foreground "DeepPink1")))
  "*Face used for a function bookmark: a bookmark that invokes a function."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-gnus
    '((((background dark)) (:foreground "Magenta"))
      (t (:foreground "DarkBlue")))
  "*Face used for a gnus bookmark."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-info
    '((((background dark)) (:foreground "Green"))
      (t (:foreground "DarkRed")))
  "*Face used for a bookmarked Info node."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-local-directory
    '((((background dark)) (:foreground "HoneyDew2" :background "DarkBlue"))
      (t (:foreground "DarkBlue" :background "HoneyDew2")))
  "*Face used for a bookmarked local directory."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-local-file-without-region
    '((((background dark)) (:foreground "Blue"))
      (t (:foreground "Black")))
  "*Face used for a bookmarked local file (without a region)."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-local-file-with-region
    '((((background dark)) (:foreground "Indianred2"))
      (t (:foreground "Blue")))
  "*Face used for a region bookmark in a local file."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-man
    '((((background dark)) (:foreground "Orange4"))
      (t (:foreground "Orange4")))
  "*Face used for a `man' page bookmark."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-non-file
    '((((background dark)) (:foreground "gray40"))
      (t (:foreground "gray60")))
  "*Face used for a bookmark not associated with an existing file or buffer."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-remote-file
    '((((background dark)) (:foreground "pink"))
      (t (:foreground "DarkViolet")))
  "*Face used for a bookmarked tramp remote file (/ssh:)."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-sequence
    '((((background dark)) (:foreground "DarkOrange2"))
      (t (:foreground "DarkOrange2")))
  "*Face used for a sequence bookmark: one composed of other bookmarks."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-su-or-sudo '((t (:foreground "Red")))
  "*Face used for a bookmarked tramp file (/su: or /sudo:)."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-t-mark '((t (:foreground "Red")))
  "*Face used for a tags mark (`t') in the bookmark list."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-varlist
    '((((background dark)) (:foreground "Salmon"))
      (t (:foreground "DarkCyan")))
  "*Face used for a bookmarked list of variables."
  :group 'bookmark-plus :group 'faces)

(defface bmkp-w3m
    '((((background dark)) (:foreground "yellow"))
      (t (:foreground "DarkMagenta")))
  "*Face used for a bookmarked w3m url."
  :group 'bookmark-plus :group 'faces)

;; Instead of vanilla `bookmark-menu-heading' (defined in Emacs 22+), to use a better default.
(defface bmkp-heading '((t (:foreground "Blue")))
  "*Face used to highlight the headings in various Bookmark+ buffers."
  :group 'bookmark-plus :version "22.1" :group 'faces)
 
;;(@* "User Options (Customizable)")
;;; User Options (Customizable) --------------------------------------

;;;###autoload
(defcustom bmkp-autoname-bookmark-function 'bmkp-autoname-bookmark
  "*Function to automatically name a bookmark at point (cursor position)."
  :type 'function :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-autoname-format (if (> emacs-major-version 21) "[0-9]\\{9\\} %s" "[0-9]+ %s")
  "*Format string to match an autonamed bookmark name.
It must have a single `%s' that to accept the buffer name."
  :type 'string :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-bmenu-omitted-list ()
  "List of names of omitted bookmarks.
They are generally not available for display in the bookmark list.
You can, however, use \\<bookmark-bmenu-mode-map>\
`\\[bmkp-bmenu-show-only-omitted]' to see them.
You can then mark some of them and use `\\[bmkp-bmenu-omit/unomit-marked]'
 to make those that are marked available again for the bookmark list."
  :type '(repeat (string :tag "Bookmark name")) :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-bmenu-commands-file (convert-standard-filename "~/.emacs-bmk-bmenu-commands.el")
  "*File for saving user-defined bookmark-list commands.
This must be an absolute file name (possibly containing `~') or nil
\(it is not expanded).

You can use `\\[bmkp-list-defuns-in-commands-file]' to list the
commands defined in the file and how many times each is defined.

NOTE: Each time you define a command using \\<bookmark-bmenu-mode-map>\
`\\[bmkp-bmenu-define-command]', `\\[bmkp-bmenu-define-full-snapshot-command]', \
`\\[bmkp-bmenu-define-jump-marked-command], or `\\[bmkp-define-tags-sort-command]',
it is saved in the file.  The new definition is simply appended to the
file - old definitions of the same command are not overwritten.  So
you might want to clean up the file occasionally, to remove any old,
unused definitions.  This is especially advisable if you used \
`\\[bmkp-bmenu-define-full-snapshot-command]',
because such command definitions can be very large."
  :type '(file  :tag "File for saving menu-list state") :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-bmenu-state-file (convert-standard-filename "~/.emacs-bmk-bmenu-state.el")
  "*File for saving `*Bookmark List*' state when you quit bookmark list.
This must be an absolute file name (possibly containing `~') or nil
\(it is not expanded).

The state is also saved when you quit Emacs, even if you don't quit
the bookmark list first (using \\<bookmark-bmenu-mode-map>`\\[bmkp-bmenu-quit]').

Set this to nil if you do not want to restore the bookmark list as it
was the last time you used it."
  :type '(choice
          (const :tag "Do not save and restore menu-list state" nil)
          (file  :tag "File for saving menu-list state"))
  :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-crosshairs-flag (> emacs-major-version 21)
  "*Non-nil means highlight with crosshairs when you visit a bookmark.
The highlighting is temporary - until your next action.
You need library `crosshairs.el' for this feature, and you need Emacs
22 or later.

If you use this option in Lisp code, you will want to add/remove
`bmkp-crosshairs-highlight' to/from `bookmark-after-jump-hook'."
  :set (lambda (sym new-val)
         (custom-set-default sym new-val)
         (if (and bmkp-crosshairs-flag (> emacs-major-version 21)
                  (condition-case nil (require 'crosshairs nil t) (error nil)))
             (add-hook 'bookmark-after-jump-hook 'bmkp-crosshairs-highlight)
           (remove-hook 'bookmark-after-jump-hook 'bmkp-crosshairs-highlight)))         
  :type 'boolean :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-desktop-no-save-vars '(search-ring regexp-search-ring kill-ring)
  "List of variables not to save when creating a desktop bookmark.
They are removed from `desktop-globals-to-save' for the duration of
the save (only)."
  :type '(repeat (variable :tag "Variable")) :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-this-buffer-cycle-sort-comparer '((bmkp-position-cp))
  "*`bmkp-sort-comparer' value for cycling this-buffer bookmarks.
Some values you might want to use: ((bmkp-position-cp)),
 ((bmkp-bookmark-creation-cp)), ((bmkp-visited-more-cp)).
See `bmkp-sort-comparer'."
  :type '(choice
          (const    :tag "None (do not sort)" nil)
          (function :tag "Sorting Predicate")
          (list     :tag "Sorting Multi-Predicate"
           (repeat (function :tag "Component Predicate"))
           (choice
            (const    :tag "None" nil)
            (function :tag "Final Predicate"))))
  :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-use-region t
  "*Non-nil means visiting a bookmark activates its recorded region."
  :type '(choice
          (const :tag "Activate bookmark region (except during cycling)"  t)
          (const :tag "Do not activate bookmark region"                   nil)
          (const :tag "Activate bookmark region even during cycling"      cycling-too))
  :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-prompt-for-tags-flag nil
  "*Non-nil means `bookmark-set' prompts for tags (when called interactively)."
  :type 'boolean :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-other-window-pop-to-flag t
  "*Non-nil means other-window bookmark jumping uses `pop-to-buffer'.
Use nil if you want the vanilla Emacs behavior, which uses
`switch-to-buffer-other-window'.  That creates a new window even if
there is already another window showing the buffer."
  :type 'boolean :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-region-search-size 40
  "*Same as `bookmark-search-size', but specialized for bookmark regions."
  :type 'integer :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-save-new-location-flag t
  "*Non-nil means save automatically relocated bookmarks.
If nil, then the new bookmark location is visited, but it is not saved
as part of the bookmark definition."
  :type 'boolean :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-handle-region-function 'bmkp-handle-region-default
  "*Function to handle a bookmarked region."
  :type 'function :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-sequence-jump-display-function 'pop-to-buffer
  "*Function used to display the bookmarks in a bookmark sequence."
  :type 'function :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-su-or-sudo-regexp "\\(/su:\\|/sudo:\\)"
  "*Regexp to recognize `su' or `sudo' Tramp bookmarks."
  :type 'regexp :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-w3m-allow-multi-tabs t
  "*Non-nil means jump to W3M bookmarks in a new session."
  :type 'boolean :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-show-end-of-region t
  "*Show end of region with `exchange-point-and-mark' when activating a region.
If nil show only beginning of region."
  :type 'boolean :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-bookmark-name-length-max 70
  "*Max number of chars for default name for a bookmark with a region."
  :type 'integer :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-incremental-filter-delay 0.6
  "*Seconds to wait before updating display when filtering bookmarks."
  :type 'number :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-menu-popup-max-length 20
  "*Max number of bookmarks for `bookmark-completing-read' to use a menu.
When choosing a bookmark from a list of bookmarks using
`bookmark-completing-read', this controls whether to use a menu or
minibuffer input with completion.
If t, then always use a menu.
If nil, then never use a menu.
If an integer, then use a menu only if there are fewer bookmark
 choices than the value."
  :type '(choice
          (integer :tag "Use a menu if there are fewer bookmark choices than this" 20)
          (const   :tag "Always use a menu to choose a bookmark" t)
          (const   :tag "Never use a menu to choose a bookmark" nil))
  :group 'bookmark-plus)

;;;###autoload
(defcustom bmkp-sort-comparer '((bmkp-info-cp bmkp-gnus-cp bmkp-w3m-cp bmkp-local-file-type-cp)
                                bmkp-alpha-p) ; This corresponds to `s k'.
  ;; $$$$$$ An alternative default value: `bmkp-alpha-p', which corresponds to `s n'.
  "*Predicate or predicates for sorting (comparing) bookmarks.
This defines the default sort for bookmarks in the bookmark list.

Various sorting commands, such as \\<bookmark-bmenu-mode-map>\
`\\[bmkp-bmenu-sort-by-bookmark-visit-frequency]', change the value of this
option dynamically (but they do not save the changed value).

The value must be one of the following:

* nil, meaning do not sort

* a predicate that takes two bookmarks as args

* a list of the form ((PRED...) FINAL-PRED), where each PRED and
  FINAL-PRED are predicates that take two bookmarks as args

If the value is a list of predicates, then each PRED is tried in turn
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
 ((p1) p2-plain) where p2-plain is (bmkp-make-plain-predicate p2)

Likewise, these three values generally act equivalently (*):

 ((p1))
 (() p1-plain)
 p1-plain        where p1-plain is (bmkp-make-plain-predicate p1)

The PRED form lets you easily combine predicates: use `p1' unless it
cannot decide, in which case try `p2', and so on.  The value ((p2 p1))
tries the predicates in the opposite order: first `p2', then `p1' if
`p2' returns nil.

Using a single predicate or FINAL-PRED makes it easy to reuse an
existing predicate that returns nil or non-nil.

You can also convert a PRED-type predicate (which returns (t), (nil),
or nil) into an ordinary predicate, by using function
`bmkp-make-plain-predicate'.  That lets you reuse elsewhere, as
ordinary predicates, any PRED-type predicates you define.

For example, this defines a plain predicate to compare by W3M URL:
 (defalias 'bmkp-w3m-p (bmkp-make-plain-predicate 'bmkp-w3m-cp))

Note: As a convention, predefined Bookmark+ PRED-type predicate names
have the suffix `-cp' (for \"component predicate\") instead of `-p'.

--
* If you use `\\[bmkp-reverse-multi-sort-order]', then there is a difference in \
behavior between

   (a) using a plain predicate as FINAL-PRED and
   (b) using the analogous PRED-type predicate (and no FINAL-PRED).

  In the latter case, `\\[bmkp-reverse-multi-sort-order]' affects when the predicate \
is tried and
  its return value.  See `bmkp-reverse-multi-sort-order'."
  :type '(choice
          (const    :tag "None (do not sort)" nil)
          (function :tag "Sorting Predicate")
          (list     :tag "Sorting Multi-Predicate"
           (repeat (function :tag "Component Predicate"))
           (choice
            (const    :tag "None" nil)
            (function :tag "Final Predicate"))))
  :group 'bookmark-plus)

;;;###autoload
(when (> emacs-major-version 20)
  (defcustom bmkp-sort-orders-alist ()
    "*Alist of all available sort functions.
This is a pseudo option - you probably do NOT want to customize this.
Instead:

 * To add a new sort function to this list, use macro
   `bmkp-define-sort-command'.  It defines a new sort function
   and automatically adds it to this list.

 * To have fewer sort orders available for cycling by \\<bookmark-bmenu-mode-map>\
`\\[bmkp-bmenu-change-sort-order-repeat]'...,
   customize option `bmkp-sort-orders-for-cycling-alist'.

Each alist element has the form (SORT-ORDER . COMPARER):

 SORT-ORDER is a short string or symbol describing the sort order.
 Examples: \"by last access time\", \"by bookmark name\".

 COMPARER compares two bookmarks.  It must be acceptable as a value of
 `bmkp-sort-comparer'."
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
    :group 'bookmark-plus))

;;;###autoload
(unless (> emacs-major-version 20)      ; Emacs 20: custom type `alist' doesn't exist.
  (defcustom bmkp-sort-orders-alist ()
    "*Alist of all available sort functions.
This is a pseudo option - you probably do NOT want to customize this.
Instead:

 * To add a new sort function to this list, use macro
   `bmkp-define-sort-command'.  It defines a new sort function
   and automatically adds it to this list.

 * To have fewer sort orders available for cycling by \\<bookmark-bmenu-mode-map>\
`\\[bmkp-bmenu-change-sort-order-repeat]'...,
   customize option `bmkp-sort-orders-for-cycling-alist'.

Each alist element has the form (SORT-ORDER . COMPARER):

 SORT-ORDER is a short string or symbol describing the sort order.
 Examples: \"by last access time\", \"by bookmark name\".

 COMPARER compares two bookmarks.  It must be acceptable as a value of
 `bmkp-sort-comparer'."
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
    :group 'bookmark-plus))
 
;;(@* "Internal Variables")
;;; Internal Variables -----------------------------------------------

(defconst bmkp-non-file-filename "   - no file -"
  "Name to use for `filename' entry, for non-file bookmarks.")

(defconst bmkp-bmenu-header-lines 2
  "Number of lines used for the `*Bookmark List*' header.")

(defconst bmkp-bmenu-marks-width 4
  "Number of columns (chars) used for the `*Bookmark List*' marks columns.")

(defconst bmkp-types-alist '(("bookmark-file"    . bmkp-bookmark-file-history)
                             ("bookmark-list"    . bmkp-bookmark-list-history)
                             ("desktop"          . bmkp-desktop-history)
                             ("dired"            . bmkp-dired-history)
                             ("file"             . bmkp-file-history)
                             ("gnus"             . bmkp-gnus-history)
                             ("info"             . bmkp-info-history)
                             ("local-file"       . bmkp-local-file-history)
                             ("man"              . bmkp-man-history)
                             ("non-file"         . bmkp-non-file-history)
                             ("region"           . bmkp-region-history)
                             ("remote-file"      . bmkp-remote-file-history)
                             ("specific-buffers" . bmkp-specific-buffers-history)
                             ("specific-files"   . bmkp-specific-files-history)
                             ("variable-list"    . bmkp-varlist-history)
                             ("w3m"              . bmkp-w3m-history))
  "Alist of bookmark types used by `bmkp-jump-to-type'.
Keys are bookmark type names.  Values are corresponding history variables.")

(defvar bmkp-bookmark-file-history ()    "History for bookmark-file bookmarks.")
(defvar bmkp-bookmark-list-history ()    "History for bookmark-list bookmarks.")
(defvar bmkp-desktop-history ()          "History for desktop bookmarks.")
(defvar bmkp-dired-history ()            "History for Dired bookmarks.")
(defvar bmkp-file-history ()             "History for file bookmarks.")
(defvar bmkp-gnus-history ()             "History for Gnus bookmarks.")
(defvar bmkp-info-history ()             "History for Info bookmarks.")
(defvar bmkp-local-file-history ()       "History for local-file bookmarks.")
(defvar bmkp-man-history ()              "History for `man'-page bookmarks.")
(defvar bmkp-non-file-history ()         "History for buffer (non-file) bookmarks.")
(defvar bmkp-region-history ()           "History for bookmarks that activate the region.")
(defvar bmkp-remote-file-history ()      "History for remote-file bookmarks.")
(defvar bmkp-specific-buffers-history () "History for specific-buffers bookmarks.")
(defvar bmkp-specific-files-history ()   "History for specific-files bookmarks.")
(defvar bmkp-varlist-history ()          "History for variable-list bookmarks.")
(defvar bmkp-w3m-history ()              "History for W3M bookmarks.")

(defvar bmkp-after-set-hook nil "Hook run after `bookmark-set' sets a bookmark.")

(defvar bmkp-current-bookmark-file bookmark-default-file
  "Current bookmark file.
When you start Emacs, this is initialized to `bookmark-default-file'.
When you load bookmarks using `bmkp-switch-bookmark-file', this is set
to the file you load.  When you save bookmarks using `bookmark-save'
with no prefix arg, they are saved to this file.

However, this does not change the value of `bookmark-default-file'.
The value of `bookmark-default-file' is never changed, except by your
customizations.  Each Emacs session uses `bookmark-default-file' for
the initial set of bookmarks.")

(defvar bmkp-last-bookmark-file bookmark-default-file
  "Last bookmark file used in this session (or default bookmark file).
This is a backup for `bmkp-current-bookmark-file'.")

(defvar bmkp-current-nav-bookmark nil "Current bookmark for navigation.")

(defvar bmkp-jump-display-function nil "Function used currently to display a bookmark.")

(defvar bmkp-last-specific-buffer ""
  "Name of buffer used by `bmkp-last-specific-buffer-p'.")

(defvar bmkp-last-specific-file ""
  "(Absolute) file name used by `bmkp-last-specific-file-p'.")

(defvar bmkp-nav-alist () "Current bookmark alist used for navigation.")

(defvar bmkp-reverse-sort-p nil "Non-nil means the sort direction is reversed.")

(defvar bmkp-reverse-multi-sort-p nil
  "Non-nil means the truth values returned by predicates are complemented.
This changes the order of the sorting groups, but it does not in
general reverse that order.  The order within each group is unchanged
\(not reversed).")

(defvar bmkp-use-w32-browser-p nil
  "Non-nil means use `w32-browser' in the default bookmark handler.
That is, use the default Windows application for the bookmarked file.
This has no effect if `w32-browser' is not defined.")

(defvar bmkp-latest-bookmark-alist () "Copy of `bookmark-alist' as last filtered.")

(defvar bmkp-bmenu-marked-bookmarks ()
  "Names of the marked bookmarks.
This includes possibly omitted bookmarks, that is, bookmarks listed in
`bmkp-bmenu-omitted-list'.")

(defvar bmkp-bmenu-before-hide-unmarked-alist ()
  "Copy of `bookmark-alist' made before hiding unmarked bookmarks.")

(defvar bmkp-bmenu-before-hide-marked-alist ()
  "Copy of `bookmark-alist' made before hiding marked bookmarks.")

(defvar bmkp-bmenu-filter-function  nil "Latest filtering function for `*Bookmark List*' display.")

(defvar bmkp-bmenu-title "" "Latest title for `*Bookmark List*' display.")

(defvar bmkp-bmenu-filter-pattern "" "Regexp for incremental filtering.")

(defvar bmkp-bmenu-filter-prompt "Pattern: " "Prompt for `bmkp-bmenu-filter-incrementally'.")

(defvar bmkp-bmenu-filter-timer nil "Timer used for incremental filtering.")

(defvar bmkp-bmenu-first-time-p t
  "Non-nil means bookmark list has not yet been shown after quitting it.
Quitting the list or the Emacs session resets this to t.
The first time the list is displayed, it is set to nil.")

(defvar bmkp-last-bmenu-bookmark nil "The name of the last bookmark current in the bookmark list.")

(defvar bmkp-last-bmenu-state-file nil "Last value of option `bmkp-bmenu-state-file'.")

(defvar bmkp-last-save-flag-value nil "Last value of option `bookmark-save-flag'.")

(defvar bmkp-sorted-alist ()
  "Copy of current bookmark alist, as sorted for buffer `*Bookmark List*'.
Has the same structure as `bookmark-alist'.")

(defvar bmkp-tag-history () "History of tags read from the user.")

(defvar bmkp-tags-alist ()         
  "Alist of all tags, from all bookmarks.
Each entry is a cons whose car is a tag name, a string.
This is set by function `bmkp-tags-list'.
Use that function to update the value.")


;; REPLACES ORIGINAL DOC STRING in `bookmark.el'.
;;
;; Doc string reflects Bookmark+ enhancements.
;;
(put 'bookmark-alist 'variable-documentation
     "Association list of bookmarks and their records.
Bookmark functions update the value automatically.
You probably do not want to change the value yourself.

The value is an alist with entries of the form
 (BOOKMARK-NAME . PARAM-ALIST)
or the deprecated form (BOOKMARK-NAME PARAM-ALIST).

 BOOKMARK-NAME is the name you gave to the bookmark when creating it.
 PARAM-ALIST is an alist of bookmark information.  The order of the
  entries in PARAM-ALIST is not important.  The possible entries are
  described below.  An entry with a key but null value means the entry
  is not used.

Bookmarks created using vanilla Emacs (`bookmark.el'):

 (filename . FILENAME)
 (position . POS)
 (front-context-string . STR-AFTER-POS)
 (rear-context-string  . STR-BEFORE-POS)
 (handler . HANDLER)
 (annotation . ANNOTATION)

 FILENAME names the bookmarked file.
 POS is the bookmarked buffer position (position in the file).
 STR-AFTER-POS is buffer text that immediately follows POS.
 STR-BEFORE-POS is buffer text that immediately precedes POS.
 ANNOTATION is a string that you can provide to identify the bookmark.
  See options `bookmark-use-annotations' and
  `bookmark-automatically-show-annotations'.
 HANDLER is a function that provides the bookmark-jump behavior
  for a specific kind of bookmark.  This is the case for Info
  bookmarks, for instance (starting with Emacs 23).

Bookmarks created using Bookmark+ are the same as for vanilla Emacs,
except for the following differences.

1. Visit information is recorded, using entries `visits' and `time':

 (visits . NUMBER-OF-VISITS)
 (time . TIME-LAST-VISITED)

 NUMBER-OF-VISITS is a whole-number counter.

 TIME-LAST-VISITED is an Emacs time representation, such as is
 returned by function `current-time'.

2. The buffer name is recorded, using entry `buffer-name'.  It need
not be associated with a file.

3. If no file is associated with the bookmark, then FILENAME is
   `   - no file -'.

4. Bookmarks can be tagged by users.  The tag information is recorded
using entry `tags':

 (tags . TAGS-ALIST)

 TAGS-ALIST is an alist with string keys.

5. Bookmarks can have individual highlighting, provided by users.
This overrides any default highlighting.

 (lighting . HIGHLIGHTING)

 HIGHLIGHTING is a property list that contain any of these keyword
 pairs:

   `:style' - Highlighting style.  Cdrs of `bmkp-light-styles-alist'
              entries are the possible values.
   `:face'  - Highlighting face, a symbol.
   `:when'  - A sexp to be evaluated.  Return value of `:no-light'
              means do not highlight.

6. The following additional entries are used to record region
information.  When a region is bookmarked, POS represents the region
start position.

 (end-position . END-POS)
 (front-context-region-string . STR-BEFORE-END-POS)
 (rear-context-region-string . STR-AFTER-END-POS))

 END-POS is the region end position.
 STR-BEFORE-END-POS is buffer text that precedes END-POS.
 STR-AFTER-END-POS is buffer text that follows END-POS.

The two context region strings are non-nil only when a region is
bookmarked.

 NOTE: The relative locations of `front-context-region-string' and
 `rear-context-region-string' are reversed from those of
 `front-context-string' and `rear-context-string'.  For example,
 `front-context-string' is the text that *follows* `position', but
 `front-context-region-string' *precedes* `end-position'.

7. The following additional entries are used for a Dired bookmark.

 (dired-marked . MARKED-FILES)
 (dired-switches . SWITCHES)

 MARKED-FILES is the list of files that were marked.
 SWITCHES is the string of `dired-listing-switches'.

8. The following additional entries are used for a Gnus bookmark.

 (group . GNUS-GROUP-NAME)
 (article . GNUS-ARTICLE-NUMBER)
 (message-id . GNUS-MESSAGE-ID)

 GNUS-GROUP-NAME is the name of a Gnus group.
 GNUS-ARTICLE-NUMBER is the number of a Gnus article.
 GNUS-MESSAGE-ID is the identifier of a Gnus message.

9. For a W3M bookmark, FILENAME is a W3M URL.

10. A sequence bookmark has this additional entry:

 (sequence . COMPONENT-BOOKMARKS)

 COMPONENT-BOOKMARKS is the list of component bookmark names.

11. A function bookmark has this additional entry, which records the
FUNCTION:

 (function . FUNCTION)

12. A bookmark-list bookmark has this additional entry, which records
the state of buffer `*Bookmark List*' at the time it is created:

 (bookmark-list . STATE)

 STATE records the sort order, filter function, omit list, and title.")
 
;;(@* "Compatibility Code for Older Emacs Versions")
;;; Compatibility Code for Older Emacs Versions ----------------------

;;;###autoload
(when (< emacs-major-version 23)

  ;; These definitions are for Emacs versions prior to Emacs 23.
  ;; They are the same as the vanilla Emacs 23+ definitions, except as noted.
  ;; They let older versions of Emacs handle bookmarks created with Emacs 23.

  ;; 1. Handle the shouldn't-happen case of non-string, non-cons.
  ;; 2. Document NOERROR in doc string.
  (defun bookmark-get-bookmark (bookmark &optional noerror)
    "Return the bookmark record corresponding to BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
If BOOKMARK is already a bookmark record, just return it.
Otherwise look for the corresponding bookmark in `bookmark-alist'.

Non-nil optional arg NOERROR means return nil if BOOKMARK
is not a valid bookmark - do not raise an error."
    (cond ((consp bookmark) bookmark)
          ((stringp bookmark)
           (or (if (fboundp 'assoc-string) ; Emacs 22+.
                   (assoc-string bookmark bookmark-alist bookmark-completion-ignore-case)
                 (assoc bookmark bookmark-alist))
               (unless noerror (error "Invalid bookmark: `%s'" bookmark))))
          (t (unless noerror (error "Invalid bookmark: `%s'" bookmark)))))

  (defun bookmark-get-bookmark-record (bookmark)
    "Return the guts of the entry for BOOKMARK in `bookmark-alist'.
That is, all information but the name.
BOOKMARK is a bookmark name or a bookmark record."
    (let ((alist  (cdr (bookmark-get-bookmark bookmark))))
      ;; The bookmark objects can either look like (NAME ALIST) or
      ;; (NAME . ALIST), so we have to distinguish the two here.
      (if (and (null (cdr alist)) (consp (caar alist)))
          (car alist)
        alist)))

  (defun Info-bookmark-make-record ()
    "Create an Info bookmark record."
    `(,Info-current-node
      ,@(bookmark-make-record-default 'point-only)
      (filename . ,Info-current-file)
      (info-node . ,Info-current-node)
      (handler . Info-bookmark-jump)))

  ;; Requires `info.el' explicitly (not autoloaded for `Info-find-node'.
  (defun Info-bookmark-jump (bookmark)
    "Jump to Info bookmark BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
    (require 'info)
    ;; Implements the `handler' for the record type returned by `Info-bookmark-make-record'.
    (let* ((file       (bookmark-prop-get bookmark 'filename))
           (info-node  (bookmark-prop-get bookmark 'info-node))
           (buf        (save-window-excursion ; VANILLA EMACS FIXME: doesn't work with frames!
                         (Info-find-node file info-node) (current-buffer))))
      ;; Use `bookmark-default-handler' to move to appropriate location within Info node.
      (bookmark-default-handler
       `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

  (add-hook 'Info-mode-hook (lambda () (set (make-local-variable 'bookmark-make-record-function)
                                            'Info-bookmark-make-record)))

  (defvar bookmark-make-record-function 'bookmark-make-record-default
    "Function called with no arguments, to create a bookmark record.
It should return the new record, which should be a cons cell of the
form (NAME . ALIST) or just ALIST, where ALIST is as described in
`bookmark-alist'.  If it cannot construct the record, then it should
raise an error.

NAME is a string that names the new bookmark.  NAME can be nil, in
which case a default name is used.

ALIST can contain an entry (handler . FUNCTION) which sets the handler
to FUNCTION, which is then used instead of `bookmark-default-handler'.
FUNCTION must accept the same arguments as `bookmark-default-handler'.

You can set this variable buffer-locally to enable bookmarking of
locations that should be treated specially, such as Info nodes, news
posts, images, pdf documents, etc.")

  (defun bookmark-make-record ()
    "Return a new bookmark record (NAME . ALIST) for the current location."
    (let ((record  (funcall bookmark-make-record-function)))
      ;; Set up default name.
      (if (stringp (car record))
          record                        ; The function already provided a default name.
        (when (car record) (push nil record))
        (setcar record  (or bookmark-current-bookmark (bookmark-buffer-name)))
        record)))

  (defun bookmark-store (bookmark-name alist no-overwrite)
    "Store the bookmark named BOOKMARK-NAME, giving it data ALIST.
If NO-OVERWRITE is non-nil and another bookmark of the same name already
exists in `bookmark-alist', then record the new bookmark but do not
discard the old one."
    (bookmark-maybe-load-default-file)
    (let ((stripped-name  (copy-sequence bookmark-name)))
      (unless (featurep 'xemacs)
        (set-text-properties 0 (length stripped-name) () stripped-name))
      (if (and (not no-overwrite) (bookmark-get-bookmark stripped-name 'noerror))
          ;; Existing bookmark under that name and no prefix arg means just overwrite old.
          ;; Use the new (NAME . ALIST) format.
          (setcdr (bookmark-get-bookmark stripped-name) alist)
        (push (cons stripped-name alist) bookmark-alist))
      (bmkp-maybe-save-bookmarks)
      (setq bookmark-current-bookmark  stripped-name)
      (bookmark-bmenu-surreptitiously-rebuild-list)))

  (defun bookmark-prop-get (bookmark prop)
    "Return property PROP of BOOKMARK, or nil if no such property.
BOOKMARK is a bookmark name or a bookmark record."
    (cdr (assq prop (bookmark-get-bookmark-record bookmark))))

  (defun bookmark-get-handler (bookmark)
    "Return the `handler' entry for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
    (bookmark-prop-get bookmark 'handler))

  (defun bookmark-jump-noselect (bookmark)
    "Return the location recorded for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
The return value has the form (BUFFER . POINT), where BUFFER is a
buffer and POINT is the location within BUFFER."
    (save-excursion (bookmark-handle-bookmark bookmark) (cons (current-buffer) (point)))))

;;;###autoload
(when (< emacs-major-version 22)

  ;; These definitions are for Emacs versions prior to Emacs 22.
  ;; They are the same as the vanilla Emacs 22+ definitions, except as noted.

  ;; Emacs 22+ just uses `bookmark-jump-other-window' for the menu also.
  (defun bmkp-menu-jump-other-window (event)
    "Jump to BOOKMARK (a point in some file) in another window.
See `bookmark-jump-other-window'."
    (interactive "e")
    (bookmark-popup-menu-and-apply-function 'bookmark-jump-other-window
                                            "Jump to Bookmark (Other Window)" event))

  (defun bookmark-maybe-message (fmt &rest args)
    "Apply `message' to FMT and ARGS, but only if the display is fast enough."
    (when (>= baud-rate 9600) (apply 'message fmt args))))
 
;;(@* "Core Replacements (`bookmark-*' except `bookmark-bmenu-*')")
;;; Core Replacements (`bookmark-*' except `bookmark-bmenu-*') -------


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. BUG fix: Put point back where it was (on the bookmark just annotated).
;; 2. Refresh menu list, to pick up the `a' marker.
;;
(defun bookmark-send-edited-annotation ()
  "Use buffer contents as annotation for a bookmark.
Lines beginning with `#' are ignored."
  (interactive)
  (unless (eq major-mode 'bookmark-edit-annotation-mode)
    (error "Not in bookmark-edit-annotation-mode"))
  (goto-char (point-min))
  (while (< (point) (point-max))
    (if (looking-at "^#")
        (bookmark-kill-line t)
      (forward-line 1)))
  (let ((annotation  (buffer-substring-no-properties (point-min) (point-max)))
	(bookmark    bookmark-annotation-name))
    (bookmark-set-annotation bookmark annotation)
    (setq bookmark-alist-modification-count  (1+ bookmark-alist-modification-count))
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (kill-buffer (current-buffer))
    (pop-to-buffer "*Bookmark List*")
    (bmkp-refresh-menu-list bookmark))) ; So the `a' marker is displayed (updated).


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Added optional arg ALIST.
;;
(defun bookmark-all-names (&optional alist)
  "Return a list of all bookmark names.
The names are those of the bookmarks in ALIST or, if nil,
`bookmark-alist'."
  (bookmark-maybe-load-default-file)
  (mapcar (lambda (bmk) (bookmark-name-from-full-record bmk)) (or alist bookmark-alist)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added optional args ALIST, PRED, and HIST.
;; 2. Define using helper function `bmkp-completing-read-1',
;;    which binds `icicle-delete-candidate-object' to (essentially) `bookmark-delete'.
;;
(defun bookmark-completing-read (prompt &optional default alist pred hist)
  "Read a bookmark name, prompting with PROMPT.
PROMPT is automatically suffixed with \": \", so do not include that.

Optional arg DEFAULT is a string to return if the user enters the
 empty string.
The alist argument used for completion is ALIST or, if nil,
 `bookmark-alist'.
Optional arg PRED is a predicate used for completion.
Optional arg HIST is a history variable for completion.  Default is
 `bookmark-history'.

If you access this function from a menu, then, depending on the value
of option `bmkp-menu-popup-max-length' and the number of
bookmarks in ALIST, you choose the bookmark using a menu or using
completion.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (bmkp-completing-read-1 prompt default alist pred hist nil))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Handles also regions and non-file buffers.
;;
(defun bookmark-make-record-default (&optional point-only)
  "Return the record describing the location of a new bookmark.
Point must be where the bookmark is to be set.
If POINT-ONLY is non-nil, return only the subset of the record that
pertains to the location within the buffer."
  (let* ((dired-p  (and (boundp 'dired-buffers) (car (rassq (current-buffer) dired-buffers))))
         (buf      (buffer-name))
         (ctime    (current-time))

         ;; Begin `let*' dependencies.
         (regionp  (and transient-mark-mode mark-active (not (eq (mark) (point)))))
         (beg      (if regionp (region-beginning) (point)))
         (end      (if regionp (region-end) (point)))
         (fcs      (if regionp
                       (bmkp-position-post-context-region beg end)
                     (bmkp-position-post-context beg)))
         (rcs      (if regionp
                       (bmkp-position-pre-context-region beg)
                     (bmkp-position-pre-context beg)))
         (fcrs     (when regionp (bmkp-end-position-pre-context beg end)))
         (ecrs     (when regionp (bmkp-end-position-post-context end))))
    `(,@(unless point-only `((filename . ,(cond ((buffer-file-name (current-buffer))
                                                 (bookmark-buffer-file-name))
                                                (dired-p  nil)
                                                (t        bmkp-non-file-filename)))))
      (buffer-name                 . ,buf)
      (front-context-string        . ,fcs)
      (rear-context-string         . ,rcs)
      (front-context-region-string . ,fcrs)
      (rear-context-region-string  . ,ecrs)
      (visits                      . 0)
      (time                        . ,ctime)
      (created                     . ,ctime)
      (position                    . ,beg)
      (end-position                . ,end))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Use `bookmark-make-record'.
;; 2. Use special default prompts for active region, W3M, and Gnus.
;; 3. Use `bmkp-completing-read-lax', choosing from current buffer's bookmarks.
;; 4. Numeric prefix arg (diff from plain): all bookmarks as completion candidates.
;; 5. Prompt for tags if `bmkp-prompt-for-tags-flag' is non-nil.
;; 6. Possibly highlight bookmark and other bookmarks in buffer, per `bmkp-auto-light-when-set'.
;;
(defun bookmark-set (&optional name parg interactivep) ; `C-x r m'
  "Set a bookmark named NAME, then run `bmkp-after-set-hook'.
If the region is active (`transient-mark-mode') and nonempty, then
record the region limits in the bookmark.

If NAME is nil, then prompt for the bookmark name.  The default name
for prompting is as follows (in order of priority):

 * If the region is active and nonempty, then the buffer name followed
   by \": \" and the region prefix (up to
   `bmkp-bookmark-name-length-max' chars).

 * If in W3M mode, then the current W3M title.

 * If in Gnus Summary mode, then the Gnus summary article header.

 * If on a `man' page, then the page name (command and section).

 * Otherwise, the current buffer name.

While entering a bookmark name at the prompt:

 * You can use (lax) completion against bookmarks in the same buffer.
   If there are no bookmarks in the current buffer, then all bookmarks
   are completion candidates.  (See also below, about a numeric prefix
   argument.)

 * You can use `C-w' to yank words from the buffer to the minibuffer.
   Repeating `C-w' yanks successive words.

 * You can use `C-u' to insert the name of the last bookmark used in
   the buffer.  This can be useful as an aid to track your progress
   through a large file.  (If no bookmark has yet been used, then
   `C-u' inserts the name of the visited file.)

A prefix argument changes the behavior as follows:

 * Numeric prefix arg: Use all bookmarks as completion candidates,
   instead of just the bookmarks for the current buffer.

 * Plain prefix arg (`C-u'): Do not overwrite a bookmark that has the
   same name as NAME, if such a bookmark already exists.  Instead,
   push the new bookmark onto the bookmark alist.

   The most recently set bookmark named NAME is thus the one in effect
   at any given time, but any others named NAME are still available,
   should you decide to delete the most recent one.

Use `\\[bookmark-delete]' to remove bookmarks (you give it a name, and it removes
only the first instance of a bookmark with that name from the list of
bookmarks)."
  (interactive (list nil current-prefix-arg t))
  (bookmark-maybe-load-default-file)
  (setq bookmark-current-point   (point) ; `bookmark-current-point' is a free var here.
        bookmark-current-buffer  (current-buffer))
  (save-excursion (skip-chars-forward " ") (setq bookmark-yank-point  (point)))
  (let* ((record   (bookmark-make-record))
         (regionp  (and transient-mark-mode mark-active (not (eq (mark) (point)))))
         (regname  (concat (buffer-name) ": "
                           (buffer-substring (if regionp (region-beginning) (point))
                                             (if regionp
                                                 (region-end)
                                               (save-excursion (end-of-line) (point))))))
         (defname  (bmkp-replace-regexp-in-string
                    "\n" " "
                    (cond (regionp (save-excursion (goto-char (region-beginning))
                                                   (skip-chars-forward " ")
                                                   (setq bookmark-yank-point  (point)))
                                   (substring regname 0 (min bmkp-bookmark-name-length-max
                                                             (length regname))))
                          ((eq major-mode 'w3m-mode) w3m-current-title)
                          ((eq major-mode 'gnus-summary-mode) (elt (gnus-summary-article-header) 1))
                          ((memq major-mode '(Man-mode woman-mode))
                           (buffer-substring (point-min) (save-excursion (goto-char (point-min))
                                                                         (skip-syntax-forward "^ ")
                                                                         (point))))
                          (t (car record)))))
         (doc-cmd  "`\\<minibuffer-local-map>\\[next-history-element]' for default")
         (bname    (or name  (bmkp-completing-read-lax
                              "Set bookmark " defname
                              (and (or (not parg) (consp parg)) ; No numeric PARG: all bookmarks.
                                   (bmkp-specific-buffers-alist-only))
                              nil 'bookmark-history))))
    (when (string-equal bname "") (setq bname  defname))
    (bookmark-store bname (cdr record) (consp parg))
    (when (and interactivep bmkp-prompt-for-tags-flag)
      (bmkp-add-tags bname (bmkp-read-tags-completing)))
    (case (and (boundp 'bmkp-auto-light-when-set) bmkp-auto-light-when-set)
      (autonamed-bookmark       (when (bmkp-autonamed-bookmark-p bname)
                                  (bmkp-light-bookmark bname)))
      (non-autonamed-bookmark   (unless (bmkp-autonamed-bookmark-p bname)
                                  (bmkp-light-bookmark bname)))
      (any-bookmark             (bmkp-light-bookmark bname))
      (autonamed-in-buffer      (bmkp-light-bookmarks (bmkp-remove-if-not
                                                           #'bmkp-autonamed-bookmark-p
                                                           (bmkp-this-buffer-alist-only)) nil 'MSG))
      (non-autonamed-in-buffer  (bmkp-light-bookmarks (bmkp-remove-if
                                                           #'bmkp-autonamed-bookmark-p
                                                           (bmkp-this-buffer-alist-only)) nil 'MSG))
      (all-in-buffer            (bmkp-light-this-buffer nil 'MSG)))
    (run-hooks 'bmkp-after-set-hook)
    (if bookmark-use-annotations
        (bookmark-edit-annotation bname)
      (goto-char bookmark-current-point)))) ; `bookmark-current-point' is a free var here.


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Prevent adding a newline in a bookmark name when yanking.
;; 
;;;###autoload
(defun bookmark-yank-word ()            ; Bound to `C-w' in minibuffer when setting bookmark.
  "Yank the word at point in `bookmark-current-buffer'.
Repeat to yank subsequent words from the buffer, appending them.
Newline characters are stripped out."
  (interactive)
  (let ((string  (with-current-buffer bookmark-current-buffer
                   (goto-char bookmark-yank-point)
                   (buffer-substring-no-properties (point)
                                                   (progn (forward-word 1)
                                                          (setq bookmark-yank-point  (point)))))))
    (setq string  (bmkp-replace-regexp-in-string "\n" "" string))
    (insert string)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Save DISPLAY-FUNCTION to `bmkp-jump-display-function' before calling
;;    `bookmark-handle-bookmark'.
;; 2. Update the name and position of an autonamed bookmark, in case it moved.
;; 3. Possibly highlight bookmark and other bookmarks in buffer, per `bmkp-auto-light-when-jump'.
;; 4. Added `catch', so a handler can throw to skip the rest of the processing if it wants.
;;
(defun bookmark--jump-via (bookmark display-function)
  "Display BOOKMARK using DISPLAY-FUNCTION.
Then run `bookmark-after-jump-hook' and show annotations for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (bmkp-record-visit bookmark 'batch)
  (setq bmkp-jump-display-function  display-function)
  (catch 'bookmark--jump-via
    (bookmark-handle-bookmark bookmark)
    (let ((win  (get-buffer-window (current-buffer) 0)))
      (when win (set-window-point win (point))))
    ;; If this is an autonamed bookmark, update its name and position, in case it moved.
    (when (bmkp-autonamed-bookmark-for-buffer-p bookmark (buffer-name))
      (setq bookmark  (bmkp-update-autonamed-bookmark bookmark)))
    (case (and (boundp 'bmkp-auto-light-when-jump) bmkp-auto-light-when-jump)
      (autonamed-bookmark       (when (bmkp-autonamed-bookmark-p bookmark)
                                  (bmkp-light-bookmark bookmark nil nil nil 'USE-POINT)))
      (non-autonamed-bookmark   (unless (bmkp-autonamed-bookmark-p bookmark)
                                  (bmkp-light-bookmark bookmark nil nil nil 'USE-POINT)))
      (any-bookmark             (bmkp-light-bookmark bookmark nil nil nil 'USE-POINT))
      (autonamed-in-buffer      (bmkp-light-bookmarks (bmkp-remove-if-not
                                                       #'bmkp-autonamed-bookmark-p
                                                       (bmkp-this-buffer-alist-only)) nil 'MSG))
      (non-autonamed-in-buffer  (bmkp-light-bookmarks (bmkp-remove-if
                                                       #'bmkp-autonamed-bookmark-p
                                                       (bmkp-this-buffer-alist-only)) nil 'MSG))
      (all-in-buffer            (bmkp-light-this-buffer nil 'MSG)))
    (let ((orig-buff  (current-buffer))) ; Used by `crosshairs-highlight'.
      (run-hooks 'bookmark-after-jump-hook))
    (let ((jump-fn  (bmkp-get-tag-value bookmark "bmkp-jump")))
      (when jump-fn (funcall jump-fn)))
    (when bookmark-automatically-show-annotations (bookmark-show-annotation bookmark))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Add to beginning, not end, of bookmark record.
;; 2. Do not use `nconc'.
;; 3. Respect both old and new bookmark formats.
;;
(defun bookmark-prop-set (bookmark prop val)
  "Set the property PROP of BOOKMARK to VAL."
  (let* ((bmk   (bookmark-get-bookmark bookmark))
         (cell  (assq prop (bookmark-get-bookmark-record bmk))))
    (if cell
        (setcdr cell val)
      (if (consp (car (cadr bmk)))      ; Old format: ("name" ((filename . "f")...))
          (setcdr bmk (list (cons (cons prop val) (cadr bmk))))
        (setcdr bmk (cons (cons prop val) (cdr bmk))))))) ; New: ("name" (filename . "f")...)


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added optional arg USE-REGION-P.
;; 2. Use `bmkp-default-bookmark-name' as default when interactive.
;; 3. Use `bmkp-jump-1'.
;; 4. Added note about Icicles `S-delete' to doc string.
;;
;;;###autoload
(defun bookmark-jump (bookmark-name &optional use-region-p) ; Bound to `C-x j j', `C-x r b', `C-x p g'
  "Jump to the bookmark named BOOKMARK-NAME.
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See function `bookmark-load' for more about this.

If the file pointed to by BOOKMARK-NAME no longer exists, you are
asked if you wish to give the bookmark a new location.  If so,
`bookmark-jump' jumps to the new location and saves it.

If the bookmark defines a region, then the region is activated if
`bmkp-use-region' is not-nil or it is nil and you use a prefix
argument.  A prefix arg temporarily flips the value of
`bmkp-use-region'.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Jump to bookmark" (bmkp-default-bookmark-name))
                     current-prefix-arg))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added optional arg USE-REGION-P.
;; 2. Use `bmkp-default-bookmark-name' as default when interactive.
;; 3. Use `bmkp-jump-1'.
;;
;;;###autoload
(defun bookmark-jump-other-window (bookmark-name &optional use-region-p) ; `C-x 4 j j', `C-x p o'
  "Jump to the bookmark named BOOKMARK-NAME, in another window.
See `bookmark-jump', in particular for info about using a prefix arg."
  (interactive (list (bookmark-completing-read "Jump to bookmark (in another window)"
                                               (bmkp-default-bookmark-name))
                     current-prefix-arg))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Different relocation message for non-file bookmark.
;;
(defun bookmark-handle-bookmark (bookmark)
  "Call BOOKMARK's handler, or `bookmark-default-handler' if it has none.
Changes the current buffer and point.
Returns nil or signals a `file-error'.
BOOKMARK can be a bookmark record used internally by some Emacs-Lisp
 package, or the name of a bookmark in `bookmark-alist'."
  (condition-case err
      (funcall (or (bookmark-get-handler bookmark) 'bookmark-default-handler)
               (bookmark-get-bookmark bookmark))
    (file-error
     ;; We were unable to find the marked file, so ask if user wants to
     ;; relocate the bookmark, else remind them to consider deletion.
     (when (stringp bookmark)
       ;; BOOKMARK can be either a bookmark name (found in `bookmark-alist') or a bookmark
       ;; object.  If an object, assume it's a bookmark used internally by some other package.
       (let ((file             (bookmark-get-filename bookmark))
             (use-dialog-box   nil)
             (use-file-dialog  nil))
         (when file                     ; Don't know how to relocate if file doesn't exist.
           (unless (string= file bmkp-non-file-filename) (setq file  (expand-file-name file)))
           (ding)
           (cond ((y-or-n-p (if (and (string= file bmkp-non-file-filename)
                                     (bmkp-get-buffer-name bookmark))
                                "Bookmark's buffer doesn't exist.  Re-create it? "
                              (concat (file-name-nondirectory file) " nonexistent.  Relocate \""
                                      bookmark "\"? ")))
                  (if (string= file bmkp-non-file-filename)
                      ;; This is not likely the right way to get the correct buffer, but it's
                      ;; better than nothing, and it gives the user a chance to DTRT.
                      (pop-to-buffer (bmkp-get-buffer-name bookmark)) ; Create buffer.
                    (bookmark-relocate bookmark)) ; Relocate to file.
                  (funcall (or (bookmark-get-handler bookmark) 'bookmark-default-handler)
                           (bookmark-get-bookmark bookmark))) ; Try again
                 (t
                  (message "Bookmark not relocated: `%s'" bookmark)
                  (signal (car err) (cdr err)))))))))
  (when (stringp bookmark) (setq bookmark-current-bookmark  bookmark))
  nil)                                  ; Return nil if no error.


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Support regions and buffer names.
;; 2. Handles w32 `Open' command if `bmkp-use-w32-browser-p' and if `w32-browser' is defined.
;;
(defun bookmark-default-handler (bookmark)
  "Default handler to jump to the location of BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
If BOOKMARK records a nonempty region, and `bmkp-use-region' is
 non-nil, then activate the region.

Non-nil `bmkp-use-w32-browser-p' means just call `w32-browser'
 (if defined).  That is, use the default MS Windows application for
 the bookmarked file.

Return nil or signal `file-error'."
  (let* ((bmk            (bookmark-get-bookmark bookmark))
         (file           (bookmark-get-filename bmk))
         (buf            (bookmark-prop-get bmk 'buffer))
         (bufname        (bmkp-get-buffer-name bmk))
         (pos            (bookmark-get-position bmk))
         (end-pos        (bmkp-get-end-position bmk))
         (old-info-node  (and (not (bookmark-get-handler bookmark))
                              (bookmark-prop-get bmk 'info-node))))
    (if (and bmkp-use-w32-browser-p (fboundp 'w32-browser) file)
        (w32-browser file)
      (if old-info-node                 ; Emacs 20-21 Info bookmarks - no handler entry.
          (progn (require 'info) (Info-find-node file old-info-node) (goto-char pos))
        (if (not (and bmkp-use-region end-pos (/= pos end-pos)))
            ;; Single-position bookmark (no region).  Go to it.
            (bmkp-goto-position bmk file buf bufname pos
                                (bookmark-get-front-context-string bmk)
                                (bookmark-get-rear-context-string bmk))
          ;; Bookmark with a region.  Go to it and activate the region.
          (if (and file (file-readable-p file) (not (buffer-live-p buf)))
              (with-current-buffer (find-file-noselect file) (setq buf  (buffer-name)))
            ;; No file found.  If no buffer either, then signal that file doesn't exist.
            (unless (or (and buf (get-buffer buf))
                        (and bufname (get-buffer bufname) (not (string= buf bufname))))
              (signal 'file-error `("Jumping to bookmark" "No such file or directory"
                                    ,(bookmark-get-filename bmk)))))
          (set-buffer (or buf bufname))
          (when bmkp-jump-display-function
            (save-current-buffer (funcall bmkp-jump-display-function (current-buffer)))
            (raise-frame))
          (goto-char (min pos (point-max)))
          (when (> pos (point-max)) (error "Bookmark position is beyond buffer end"))
          ;; Activate region.  Relocate it if it moved.  Save relocated bookmark if confirm.
          (funcall bmkp-handle-region-function bmk))))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added bookmark default for interactive use.
;; 2. Added note about `S-delete' to doc string.
;; 3. Changed arg name: BOOKMARK -> BOOKMARK-NAME.
;; 4. Refresh menu list, to show new location.
;;
(or (fboundp 'old-bookmark-relocate)
(fset 'old-bookmark-relocate (symbol-function 'bookmark-relocate)))

;;;###autoload
(defun bookmark-relocate (bookmark-name) ; Not bound
  "Relocate the bookmark named BOOKMARK-NAME to another file.
You are prompted for the new file name.
Changes the file associated with the bookmark.
Useful when a file has been renamed after a bookmark was set in it.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Bookmark to relocate"
                                               (bmkp-default-bookmark-name))))
  (old-bookmark-relocate bookmark-name)
  (when (and (get-buffer-window (get-buffer-create "*Bookmark List*") 0)
             bookmark-bmenu-toggle-filenames)
    (bmkp-refresh-menu-list bookmark-name))) ; So the new location is displayed.


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added bookmark default for interactive use.
;; 2. Do not add any text properties here.  That's done in `bmkp-bmenu-propertize-item'.
;; 3. Added note about `S-delete' to doc string.
;; 4. Changed arg name: BOOKMARK -> BOOKMARK-NAME.
;;
;;;###autoload
(defun bookmark-insert-location (bookmark-name &optional no-history) ; `C-x p I' (original: `C-x p f')
  "Insert file or buffer name for the bookmark named BOOKMARK-NAME.
If a file is bookmarked, insert the recorded file name.
If a non-file buffer is bookmarked, insert the recorded buffer name.

Optional second arg NO-HISTORY means do not record this in the
minibuffer history list `bookmark-history'.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (if (> emacs-major-version 21)
                   (list (bookmark-completing-read "Insert bookmark location"
                                                   (bmkp-default-bookmark-name)))
                 (bookmark-completing-read "Insert bookmark location"
                                           (bmkp-default-bookmark-name))))
  (or no-history (bookmark-maybe-historicize-string bookmark-name))
  (insert (bookmark-location bookmark-name))) ; Return the line inserted.


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Location returned can be a buffer name, instead of a file name.
;;
(defun bookmark-location (bookmark)
  "Return the name of the file or buffer associated with BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
Return \"\" if no such name can be found." ; $$$$$$
  (bookmark-maybe-load-default-file)
  (or (bookmark-prop-get bookmark 'location)
      (bookmark-get-filename bookmark)
      (bmkp-get-buffer-name bookmark)
      (bookmark-prop-get bookmark 'buffer)
      "-- Unknown location --"))
      ;; $$$$$$$$$ ""))
      ;; $$$$ (error "Bookmark has no file or buffer name: %S" bookmark)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added bookmark default for interactive use.
;; 2. Added note about `S-delete' to doc string.
;; 3. Added BATCH arg.
;; 4. Refresh menu list, to show new name.
;;
;;;###autoload
(defun bookmark-rename (old &optional new batch) ; Bound to `C-x p r'
  "Change bookmark's name from OLD to NEW.
Interactively:
 If called from the keyboard, then prompt for OLD.
 If called from the menubar, select OLD from a menu.
If NEW is nil, then prompt for its string value.

If BATCH is non-nil, then do not rebuild the bookmark list.

While the user enters the new name, repeated `C-w' inserts consecutive
words from the buffer into the new bookmark name.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Old bookmark name"
                                               (bmkp-default-bookmark-name))))
  (bookmark-maybe-historicize-string old)
  (bookmark-maybe-load-default-file)
  (setq bookmark-current-point  (point)) ; `bookmark-current-point' is a free var here.
  (save-excursion (skip-chars-forward " ") (setq bookmark-yank-point  (point)))
  (setq bookmark-current-buffer  (current-buffer))
  (let ((newname  (or new  (read-from-minibuffer "New name: " nil
                                                 (let ((now-map  (copy-keymap minibuffer-local-map)))
                                                   (define-key now-map  "\C-w" 'bookmark-yank-word)
                                                   now-map)
                                                 nil 'bookmark-history))))
    (bookmark-set-name old newname)
    (setq bookmark-current-bookmark  newname)
    (unless batch
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (when (get-buffer-window (get-buffer-create "*Bookmark List*") 0)
        (bmkp-refresh-menu-list newname))) ; So the new name is displayed.
    (bmkp-maybe-save-bookmarks)
    newname))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added bookmark default for interactive use.
;; 2. Added note about `S-delete' to doc string.
;; 3. Changed arg name: BOOKMARK -> BOOKMARK-NAME.
;;
(or (fboundp 'old-bookmark-insert)
(fset 'old-bookmark-insert (symbol-function 'bookmark-insert)))

;;;###autoload
(defun bookmark-insert (bookmark-name)  ; Bound to `C-x p i'
  "Insert the text of a bookmarked file.
BOOKMARK-NAME is the name of the bookmark.
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See function `bookmark-load' for more about this.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Insert bookmark contents"
                                               (bmkp-default-bookmark-name))))
  (old-bookmark-insert bookmark-name))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added note about `S-delete' to doc string.
;; 2. Changed arg name: BOOKMARK -> BOOKMARK-NAME.
;; 3. Use `bmkp-default-bookmark-name' as default when interactive.
;; 4. Remove highlighting for the bookmark.
;; 5. Update `bmkp-latest-bookmark-alist' and `bmkp-bmenu-omitted-list'.
;; 6. Increment `bookmark-alist-modification-count' even when using `batch' arg.
;;
;;;###autoload
(defun bookmark-delete (bookmark-name &optional batch) ; Bound to `C-x p d'
  "Delete the bookmark named BOOKMARK-NAME from the bookmark list.
Removes only the first instance of a bookmark with that name.
If there are other bookmarks with the same name, they are not deleted.
Defaults to the \"current\" bookmark (that is, the one most recently
used in this file), if it exists.  Optional second arg BATCH means do
not update the bookmark list buffer (probably because we were called
from there).

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate.  In this way, you can delete multiple bookmarks."
  (interactive
   (list (bookmark-completing-read "Delete bookmark" (bmkp-default-bookmark-name))))
  (bookmark-maybe-historicize-string bookmark-name)
  (bookmark-maybe-load-default-file)
  (let ((bmk  (bookmark-get-bookmark bookmark-name 'NOERROR)))
    (when (fboundp 'bmkp-unlight-bookmark) (bmkp-unlight-bookmark bookmark-name 'NOERROR))
    (setq bookmark-alist              (delq bmk bookmark-alist)
          bmkp-latest-bookmark-alist  (delq bmk bmkp-latest-bookmark-alist)
          bmkp-bmenu-omitted-list     (delete bookmark-name bmkp-bmenu-omitted-list)))
  ;; Added by DB.  `bookmark-current-bookmark' should be nil if last occurrence was deleted.
  (unless (bookmark-get-bookmark bookmark-current-bookmark 'noerror)
    (setq bookmark-current-bookmark  nil))
  ;; Don't rebuild the list when using `batch' arg
  (unless batch (bookmark-bmenu-surreptitiously-rebuild-list))
  (bmkp-maybe-save-bookmarks))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Use `bmkp-current-bookmark-file', not `bookmark-default-file'.
;;
;;;###autoload
(defun bookmark-save (&optional parg file) ; Bound to `C-x p s'
  "Save currently defined bookmarks.
Save by default in the file named by variable
`bmkp-current-bookmark-file'.  With a prefix arg, you are prompted for
the file to save to.

To load bookmarks from a specific file, use `\\[bookmark-load]'
\(`bookmark-load').

If called from Lisp:
 Witn nil PARG, use file `bmkp-current-bookmark-file'.
 With non-nil PARG and non-nil FILE, use file FILE.
 With non-nil PARG and nil FILE, prompt the user for the file to use."
  (interactive "P")
  (bookmark-maybe-load-default-file)
  (cond ((and (not parg) (not file)) (bookmark-write-file bmkp-current-bookmark-file))
        ((and (not parg) file) (bookmark-write-file file))
        ((and parg (not file))
         (let ((file  (read-file-name "File to save bookmarks in: ")))  (bookmark-write-file file)))
        (t (bookmark-write-file file)))
  ;; Indicate by the count that we have synced the current bookmark file.
  ;; If an error has already occurred somewhere, the count will not be set, which is what we want.
  (setq bookmark-alist-modification-count 0))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added optional arg ALT-MSG.
;; 2. Insert code piecewise, to improve performance when saving `bookmark-alist'.
;;    (Do not let `pp' parse all of `bookmark-alist' at once.)
;; 3. Remove any text properties from bookmark title and file name.
;; 4. Use `case', not `cond'.
;;
(defun bookmark-write-file (file &optional alt-msg)
  "Write `bookmark-alist' to FILE.
Non-nil ALT-MSG is a message format string to use in place of the
default, \"Saving bookmarks to file `%s'...\".  The string must
contain a `%s' construct, so that it can be passed along with FILE to
`format'.  At the end, \"done\" is appended to the message."
  (let ((msg  (or alt-msg "Saving bookmarks to file `%s'..." file)))
    (bookmark-maybe-message (or alt-msg "Saving bookmarks to file `%s'...") file)
    (with-current-buffer (get-buffer-create " *Bookmarks*")
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (let ((print-length  nil)
            (print-level   nil)
            bname fname last-fname)
        (bookmark-insert-file-format-version-stamp)
        (insert "(")
        (dolist (bmk  bookmark-alist)
          (setq bname  (car bmk)
                fname  (bookmark-get-filename bmk))
          (set-text-properties 0 (length bname) () bname)
          (when fname (set-text-properties 0 (length fname) () fname))
          (setcar bmk bname)
          (when (setq last-fname  (assq 'filename bmk)) (setcdr last-fname fname))
          (pp bmk (current-buffer)))
        (insert ")")
        (let ((version-control  (case bookmark-version-control
                                  ((nil)      nil)
                                  (never      'never)
                                  (nospecial  version-control)
                                  (t          t))))
          (condition-case nil
              (write-region (point-min) (point-max) file)
            (file-error (message "Cannot write file `%s'" file)))
          (kill-buffer (current-buffer))
          (bookmark-maybe-message (concat msg "done") file))))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Prefix arg means OVERWRITE.
;; 2. Update `bmkp-current-bookmark-file' if OVERWRITE is non-nil.
;; 3. New doc string.
;; 4. Final msg says whether overwritten.
;; 5. Call `bmkp-bmenu-refresh-menu-list' at end.
;;
;;;###autoload
(defun bookmark-load (file &optional overwrite no-msg) ; Bound to `C-x p l'
  "Load bookmarks from FILE (which must be in the standard format).
Without a prefix argument (argument OVERWRITE is nil), add the newly
loaded bookmarks to those already current.  They will be saved to the
current bookmark file when bookmarks are saved.  If you have never
switched bookmark files, then this is the default file,
`bookmark-default-file'.

If you do not use a prefix argument, then no existing bookmarks are
overwritten.  If you load some bookmarks that have the same names as
bookmarks already defined in your Emacs session, numeric suffixes
\"<2>\", \"<3>\",... are appended as needed to the names of those new
bookmarks to distinguish them.

With a prefix argument, switch the bookmark file currently used,
*replacing* all currently existing bookmarks with the newly loaded
bookmarks.  The value of `bmkp-current-bookmark-file' is changed to
FILE, so bookmarks will subsequently be saved to FILE.  The value
`bookmark-default-file' is unaffected, so your next Emacs session will
still use the same default set of bookmarks.

When called from Lisp, non-nil NO-MSG means do not display any
messages while loading.

You do not need to manually load your default bookmark file
\(`bookmark-default-file') - it is loaded automatically by Emacs the
first time you use bookmarks in a session.  Use `bookmark-load' only
to load extra bookmarks (with no prefix arg) or an alternative set of
bookmarks (with a prefix arg).

If you use `bookmark-load' to load a file that does not contain a
proper bookmark alist, then when bookmarks are saved the current
bookmark file will likely become corrupted.  You should load only
bookmark files that were created using the bookmark functions."
  (interactive
   (list
    (let* ((bfile                     (if (bmkp-same-file-p bmkp-current-bookmark-file
                                                            bmkp-last-bookmark-file)
                                          bookmark-default-file
                                        bmkp-last-bookmark-file))
           (dir                       (or (file-name-directory bfile) "~/"))
           (insert-default-directory  t))
      (read-file-name (if current-prefix-arg "Switch to bookmark file: " "Add bookmarks from file: ")
                      dir bfile 'confirm))
    current-prefix-arg))
  (setq file  (expand-file-name file))
  (unless (file-readable-p file) (error "Cannot read bookmark file `%s'" file))
  (unless no-msg (bookmark-maybe-message "Loading bookmarks from `%s'..." file))
  (with-current-buffer (let ((enable-local-variables nil)) (find-file-noselect file))
    (goto-char (point-min))
    (bookmark-maybe-upgrade-file-format)
    (let ((blist  (bookmark-alist-from-buffer)))
      (unless (listp blist) (error "Invalid bookmark list in `%s'" file))
      (if overwrite
          (setq bmkp-last-bookmark-file            bmkp-current-bookmark-file
                bmkp-current-bookmark-file         file
                bookmark-alist                     blist
                bookmark-alist-modification-count  0)
        (bookmark-import-new-list blist)
        (setq bookmark-alist-modification-count  (1+ bookmark-alist-modification-count)))
      (when (string-equal (expand-file-name bookmark-default-file) file)
        (setq bookmarks-already-loaded  t))
      (bookmark-bmenu-surreptitiously-rebuild-list))
    (kill-buffer (current-buffer)))
  (unless no-msg (message "%s bookmark file `%s'" (if overwrite "Switched to" "Loaded") file))
  (let ((bmklistbuf  (get-buffer "*Bookmark List*")))
    (when bmklistbuf (with-current-buffer bmklistbuf (bmkp-bmenu-refresh-menu-list)))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Added optional arg MSGP.  Show message if no annotation.
;; Name buffer after the bookmark.
;; Do not `save-excursion' (makes no sense, since we `pop-to-buffer').
;; Raise error if no annotation.
;; Use `view-mode'.  `q' uses `quit-window'.
;; Bind `buffer-read-only' to nil since existing buffer is in View mode.
;; Fit frame to buffer if `one-windowp'.
;;
(defun bookmark-show-annotation (bookmark &optional msgp)
  "Display the annotation for BOOKMARK."
  (let* ((bmk       (bookmark-get-bookmark bookmark))
         (bmk-name  (bookmark-name-from-full-record bmk))
         (ann       (bookmark-get-annotation bmk)))
    (if (not (and ann (not (string-equal ann ""))))
        (when msgp (message "Bookmark has no annotation"))
      (pop-to-buffer (get-buffer-create (format "*`%s' Annotation*" bmk-name)) t)
      (let ((buffer-read-only  nil))
        (delete-region (point-min) (point-max))
        (insert (concat "Annotation for bookmark '" bmk-name "':\n\n"))
        (put-text-property (line-beginning-position -1) (line-end-position 1)
                           'face 'bmkp-heading)
        (insert ann))
      (goto-char (point-min))
      (view-mode-enter (cons (selected-window) (cons nil 'quit-window)))
      (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window)))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Do not `save-selected-window' (makes no sense, since we `pop-to-buffer').
;; Use name `*Bookmark Annotations*', not `*Bookmark Annotation*'.
;; Don't list bookmarks with no annotation.
;; Highlight bookmark names.  Don't indent annotations.  Add a blank line after each annotation.
;; Use `view-mode'.  `q' uses `quit-window'.
;; Fit frame to buffer if `one-windowp'.
;;
(defun bookmark-show-all-annotations ()
  "Display the annotations for all bookmarks."
  (pop-to-buffer (get-buffer-create "*Bookmark Annotations*") t)
  (setq buffer-read-only  nil)
  (delete-region (point-min) (point-max))
  (dolist (full-record  bookmark-alist)
    (let ((ann  (bookmark-get-annotation full-record)))
      (when (and ann (not (string-equal ann "")))
        (insert (concat (bookmark-name-from-full-record full-record) ":\n"))
        (put-text-property (line-beginning-position 0) (line-end-position 0) 'face 'bmkp-heading)
        (insert ann) (unless (bolp) (insert "\n\n")))))
  (goto-char (point-min))
  (view-mode-enter (cons (selected-window) (cons nil 'quit-window)))
  (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Save menu-list state to `bmkp-bmenu-state-file'.
;;
(defun bookmark-exit-hook-internal ()
  "Save currently defined bookmarks and perhaps bookmark menu-list state.
Run `bookmark-exit-hook', then save bookmarks if they were updated.
Then save menu-list state to file `bmkp-bmenu-state-file', but only if
that option is non-nil."
  (run-hooks 'bookmark-exit-hook)
  (when (and bookmark-alist (bookmark-time-to-save-p t)) (bookmark-save))
  (bmkp-save-menu-list-state))
 
;;(@* "Menu List Replacements (`bookmark-bmenu-*')")
;;; Menu List Replacements (`bookmark-bmenu-*') ----------------------


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Return t.  Value doesn't mean anything (didn't anyway), but must be non-nil for vanilla Emacs.
;; 2. Don't count lines.  Just make sure we're on a bookmark line.
;;
(defalias 'bookmark-bmenu-check-position 'bookmark-bmenu-ensure-position)
(defun bookmark-bmenu-ensure-position ()
  "Move to the beginning of the nearest bookmark line."
  (beginning-of-line)
  (unless (bookmark-bmenu-bookmark)
    (if (and (bolp) (eobp))
        (beginning-of-line 0)
      (goto-char (point-min))
      (forward-line bmkp-bmenu-header-lines)))
  t)                                    ; Older vanilla bookmark code depends on non-nil value.


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Add bookmark to `bmkp-bmenu-marked-bookmarks'.
;; 2. Don't call `bookmark-bmenu-ensure-position' again at end.
;; 3. Raise error if not in `*Bookmark List*'.
;;
;;;###autoload
(defun bookmark-bmenu-mark ()           ; Bound to `m' in bookmark list
  "Mark the bookmark on this line, using mark `>'."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (beginning-of-line)
  (let ((inhibit-read-only  t))
    (push (bookmark-bmenu-bookmark) bmkp-bmenu-marked-bookmarks)
    (delete-char 1) (insert ?>) (put-text-property (1- (point)) (point) 'face 'bmkp->-mark)
    (forward-line 1)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Remove bookmark from `bmkp-bmenu-marked-bookmarks'.
;; 2. Don't call `bookmark-bmenu-ensure-position' again at end.
;; 3. Raise error if not in `*Bookmark List*'.
;;
;;;###autoload
(defun bookmark-bmenu-unmark (&optional backup) ; Bound to `u' in bookmark list
  "Unmark the bookmark on this line, then move down to the next.
Optional BACKUP means move up instead."
  (interactive "P")
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (beginning-of-line)
  (let ((inhibit-read-only  t))
    (delete-char 1) (insert " ")
    (setq bmkp-bmenu-marked-bookmarks  (delete (bookmark-bmenu-bookmark)
                                               bmkp-bmenu-marked-bookmarks)))
  (forward-line (if backup -1 1)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Do not use `bookmark-bmenu-ensure-position' as a test - it always returns non-nil anyway.
;;    And don't call it at again the end.
;; 2. Use face `bmkp-bad-bookmark' on the `D' flag.
;; 3. Raise error if not in buffer `*Bookmark List*'.
;;
;;;###autoload
(defun bookmark-bmenu-delete ()         ; Bound to `d', `k' in bookmark list
  "Flag this bookmark for deletion, using mark `D'.
Use `\\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-execute-deletions]' to carry out \
the deletions."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (beginning-of-line)
  (bookmark-bmenu-ensure-position)
  (let ((inhibit-read-only  t))
    (delete-char 1) (insert ?D) (put-text-property (1- (point)) (point) 'face 'bmkp-D-mark)
    (setq bmkp-bmenu-marked-bookmarks  (delete (bookmark-bmenu-bookmark) bmkp-bmenu-marked-bookmarks))
    (forward-line 1)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Rebuild the menu list using the last filtered alist in use, `bmkp-latest-bookmark-alist'.
;; 2. Update the menu-list title.
;;
(defun bookmark-bmenu-surreptitiously-rebuild-list ()
  "Rebuild the bookmark list, if it exists."
  (when (get-buffer "*Bookmark List*")
    (save-excursion (save-window-excursion (let ((bookmark-alist  bmkp-latest-bookmark-alist))
                                             (bookmark-bmenu-list 'filteredp))))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added args TITLE, FILTEREDP, DONT-TOGGLE-FILENAMES-P.
;; 2. Handles also region bookmarks and buffer (non-file) bookmarks.
;; 3. Uses `pop-to-buffer', not `switch-to-buffer', so we respect `special-display-*'
;;    (but keep `one-window-p' if that's the case).
;; 4. If option `bmkp-bmenu-state-file' is non-nil, then the first time since the last quit
;;     (or the last Emacs session) restores the last menu-list state.
;; 5. If option `bmkp-bmenu-commands-file' is non-nil, then read that file, which contains
;;    user-defined `*Bookmark List*' commands.
;;
(defalias 'list-bookmarks 'bookmark-bmenu-list)
;;;###autoload
(defun bookmark-bmenu-list (&optional filteredp) ; Bound to `C-x r l'
  "Display a list of existing bookmarks, in buffer `*Bookmark List*'.
The leftmost column of a bookmark entry shows `D' if the bookmark is
 flagged for deletion, or `>' if it is marked normally.
The second column shows `t' if the bookmark has tags.
The third  column shows `a' if the bookmark has an annotation.

The following faces are used for the list entries.
Use `customize-face' if you want to change the appearance.

 `bmkp-bad-bookmark', `bmkp-bookmark-list', `bmkp-buffer',
 `bmkp-desktop', `bmkp-function', `bmkp-gnus', `bmkp-info',
 `bmkp-local-directory', `bmkp-local-file-without-region',
 `bmkp-local-file-with-region', `bmkp-man', `bmkp-non-file',
 `bmkp-remote-file', `bmkp-sequence', `bmkp-su-or-sudo',
 `bmkp-varlist', `bmkp-w3m'.

If option `bmkp-bmenu-state-file' is non-nil then the state of the
displayed bookmark-list is saved when you quit it, and it is restored
when you next use this command.  That saved state is not restored,
however, if it represents a different file from the current bookmark
file.

If you call this interactively when buffer `*Bookmark List*' exists,
that buffer is refreshed to show all current bookmarks, and any
markings are removed.  If you instead want to show the buffer in its
latest state then just do that: use `C-x b' or similar.  If you want
to refresh the displayed buffer, to show the latest state, reflecting
any additions, deletions, renamings, and so on, use \\<bookmark-bmenu-mode-map>\
`\\[bmkp-bmenu-refresh-menu-list]'.

In Lisp code, non-nil optional argument FILTEREDP means the bookmark
list has been filtered, which means:
 * Use `bmkp-bmenu-title' not the default menu-list title.
 * Do not reset `bmkp-latest-bookmark-alist' to `bookmark-alist'."
  (interactive)
  (bookmark-maybe-load-default-file)
  (when (and bmkp-bmenu-first-time-p bmkp-bmenu-commands-file
             (file-readable-p bmkp-bmenu-commands-file))
    (with-current-buffer (let ((enable-local-variables  ())
                               (emacs-lisp-mode-hook    nil))
                           (find-file-noselect bmkp-bmenu-commands-file))
      (goto-char (point-min))
      (while (not (eobp)) (condition-case nil (eval (read (current-buffer))) (error nil)))
      (kill-buffer (current-buffer))))
  (cond ((and bmkp-bmenu-first-time-p  bmkp-bmenu-state-file ; Restore from state file.
              (file-readable-p bmkp-bmenu-state-file))
         (let ((state  ()))
           (with-current-buffer (let ((enable-local-variables  nil)
                                      (emacs-lisp-mode-hook    nil))
                                  (find-file-noselect bmkp-bmenu-state-file))
             (goto-char (point-min))
             (setq state  (condition-case nil (read (current-buffer)) (error nil)))
             (kill-buffer (current-buffer)))
           (let ((last-bookmark-file-from-state  (cdr (assq 'last-bookmark-file state))))
             (when (and (consp state)
                        ;; If bookmark file has changed, then do not use state saved from other file.
                        (or (not last-bookmark-file-from-state)
                            (bmkp-same-file-p last-bookmark-file-from-state
                                              bmkp-current-bookmark-file)))
               (setq bmkp-sort-comparer                (cdr (assq 'last-sort-comparer          state))
                     bmkp-reverse-sort-p               (cdr (assq 'last-reverse-sort-p         state))
                     bmkp-reverse-multi-sort-p         (cdr (assq 'last-reverse-multi-sort-p   state))
                     bmkp-latest-bookmark-alist        (cdr (assq 'last-latest-bookmark-alist  state))
                     bmkp-bmenu-omitted-list           (cdr (assq 'last-omitted-list           state))
                     bmkp-bmenu-marked-bookmarks       (cdr (assq 'last-bmenu-marked-bookmarks state))
                     bmkp-bmenu-filter-function        (cdr (assq 'last-bmenu-filter-function  state))
                     bmkp-bmenu-filter-pattern
                     (or (cdr (assq 'last-bmenu-filter-pattern   state)) "")
                     bmkp-bmenu-title                  (cdr (assq 'last-bmenu-title            state))
                     bmkp-last-bmenu-bookmark          (cdr (assq 'last-bmenu-bookmark         state))
                     bmkp-last-specific-buffer         (cdr (assq 'last-specific-buffer        state))
                     bmkp-last-specific-file           (cdr (assq 'last-specific-file          state))
                     bookmark-bmenu-toggle-filenames   (cdr (assq 'last-bmenu-toggle-filenames state))
                     bmkp-last-bookmark-file           bmkp-current-bookmark-file
                     bmkp-current-bookmark-file        last-bookmark-file-from-state
                     bmkp-bmenu-before-hide-marked-alist
                     (cdr (assq 'last-bmenu-before-hide-marked-alist   state))
                     bmkp-bmenu-before-hide-unmarked-alist
                     (cdr (assq 'last-bmenu-before-hide-unmarked-alist state))))))
         (setq bmkp-bmenu-first-time-p  nil)
         (let ((bookmark-alist  (or bmkp-latest-bookmark-alist bookmark-alist)))
           (bmkp-bmenu-list-1 'filteredp nil (interactive-p)))
         (when bmkp-last-bmenu-bookmark
           (with-current-buffer (get-buffer "*Bookmark List*")
             (bmkp-bmenu-goto-bookmark-named bmkp-last-bmenu-bookmark))))
        (t
         (setq bmkp-bmenu-first-time-p  nil)
         (bmkp-bmenu-list-1
          filteredp  (or (interactive-p) (not (get-buffer "*Bookmark List*")))  (interactive-p)))))

(defun bmkp-bmenu-list-1 (filteredp reset-marked-p interactivep)
  "Helper for `bookmark-bmenu-list'.
See `bookmark-bmenu-list' for the description of FILTEREDP.
Non-nil RESET-MARKED-P resets `bmkp-bmenu-marked-bookmarks'.
Non-nil INTERACTIVEP means `bookmark-bmenu-list' was called
 interactively, so pop to the bookmark list and communicate the sort
 order."
  (when reset-marked-p (setq bmkp-bmenu-marked-bookmarks  ()))
  (unless filteredp (setq bmkp-latest-bookmark-alist  bookmark-alist))
  (if interactivep
      (let ((one-win-p  (one-window-p)))
        (pop-to-buffer (get-buffer-create "*Bookmark List*"))
        (when one-win-p (delete-other-windows)))
    (set-buffer (get-buffer-create "*Bookmark List*")))
  (let* ((inhibit-read-only  t)
         (title              (if (and filteredp bmkp-bmenu-title (not (equal "" bmkp-bmenu-title)))
				 bmkp-bmenu-title
			       "All Bookmarks")))
    (erase-buffer)
    (insert (format "%s\n%s\n" title (make-string (length title) ?-)))
    (add-text-properties (point-min) (point) (bmkp-face-prop 'bmkp-heading))
    (let ((max-width  0)
          name markedp tags annotation start)
      (setq bmkp-sorted-alist  (bmkp-sort-and-remove-dups
                                bookmark-alist
                                (and (not (eq bmkp-bmenu-filter-function 'bmkp-omitted-alist-only))
                                     bmkp-bmenu-omitted-list)))
      (dolist (bmk  bmkp-sorted-alist)
        (setq max-width  (max max-width (length (bookmark-name-from-full-record bmk)))))
      (setq max-width  (+ max-width bmkp-bmenu-marks-width))
      (dolist (bmk  bmkp-sorted-alist)
        (setq name        (bookmark-name-from-full-record bmk)
              markedp     (bmkp-marked-bookmark-p bmk)
              tags        (bmkp-get-tags bmk)
              annotation  (bookmark-get-annotation bmk)
              start       (+ bmkp-bmenu-marks-width (point)))
        (if (not markedp)
            (insert " ")
          (insert ">") (put-text-property (1- (point)) (point) 'face 'bmkp->-mark))
        (if (null tags)
            (insert " ")
          (insert "t") (put-text-property (1- (point)) (point) 'face 'bmkp-t-mark))
        (if (not (and annotation (not (string-equal annotation ""))))
            (insert " ")
          (insert "a") (put-text-property (1- (point)) (point) 'face 'bmkp-a-mark))
        (insert " ")
        (when (and (featurep 'bookmark+-lit) (bmkp-get-lighting bmk)) ; Highlight highlight overrides.
          (put-text-property (1- (point)) (point) 'face 'bmkp-light-mark))
        (insert name)
        (move-to-column max-width t)
        (bmkp-bmenu-propertize-item name start (point))
        (insert "\n")))
    (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
    (bookmark-bmenu-mode)
    (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-toggle-filenames t))
    (when (and (fboundp 'fit-frame-if-one-window)
               (eq (selected-window) (get-buffer-window (get-buffer-create "*Bookmark List*") 0)))
      (fit-frame-if-one-window)))
  (when (and interactivep bmkp-sort-comparer)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Redefined.  Get name of the current bookmark from text property `bmkp-bookmark-name'.
;; Use `condition-case' in case we're at eob (so cannot advance).
;;
(defun bookmark-bmenu-bookmark ()
  "Return the name of the bookmark on this line."
  (condition-case nil
      (save-excursion (forward-line 0) (forward-char (1+ bmkp-bmenu-marks-width))
                      (get-text-property (point) 'bmkp-bookmark-name))
    (error nil)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Only the doc string is different.
;;
(defun bookmark-bmenu-mode ()
  "Major mode for editing a list of bookmarks.
Each line represents an Emacs bookmark.\\<bookmark-bmenu-mode-map>

More bookmarking help below.  Keys without prefix `C-x' are available
only in buffer `*Bookmark List*'.  Others are available everywhere.


Help (Describe)
---------------

\\[bmkp-bmenu-describe-this-bookmark]\t- Show information about this bookmark (`C-u': \
internal form)
\\[bmkp-bmenu-describe-this+move-down]\t- Show the info, then move to next bookmark
\\[bmkp-bmenu-describe-this+move-up]\t- Show the info, then move to previous bookmark
\\[bmkp-bmenu-describe-marked]\t- Show info about the marked bookmarks (`C-u': internal form)
\\[bookmark-bmenu-locate]\t- Show the location of this bookmark in the minibuffer
\\[bookmark-bmenu-show-annotation]\t- Show this bookmark's annotation
\\[bookmark-bmenu-show-all-annotations]\t- Show the annotations of all annotated bookmarks
\\[bookmark-bmenu-toggle-filenames]\t- Toggle showing filenames next to bookmarks

\\[bmkp-list-defuns-in-commands-file]
\t- List the commands defined in `bmkp-bmenu-commands-file'


General
-------

\\[bmkp-bmenu-refresh-menu-list]\t- Refresh (revert) to up-to-date bookmark list
\\[bmkp-bmenu-quit]\t- Quit (`*Bookmark List*')
\\[bmkp-bmenu-dired-marked]\t- Open Dired for the marked files and directories

\\[bookmark-bmenu-load]\t- Add bookmarks from a different bookmark file (extra load)
\\[bmkp-switch-bookmark-file]\t- Switch to a different bookmark file      (overwrite load)
C-u \\[bmkp-switch-bookmark-file]\t- Switch back to the last bookmark file    (overwrite load)
\\[bmkp-set-bookmark-file-bookmark]\t- Create a bookmark to a bookmark file \
\(`\\[bmkp-bookmark-file-jump]' to load)

\\[bookmark-bmenu-save]\t- Save bookmarks (`C-u': prompt for the bookmark file to use)
\\[bmkp-toggle-saving-bookmark-file]\t- Toggle autosaving the bookmark file
\\[bmkp-toggle-saving-menu-list-state]\t- Toggle autosaving the bookmark-list display state

\\[bmkp-choose-navlist-of-type]\t- Set the navlist to the bookmarks of a type you choose
\\[bmkp-choose-navlist-from-bookmark-list]\t- Set the navlist to the bookmarks of a \
bookmark-list bookmark
\\[bmkp-navlist-bmenu-list]\t- Open `*Bookmark List*' for bookmarks in navlist
\\[bmkp-this-buffer-bmenu-list]\t- Open `*Bookmark List*' for bookmarks in current buffer
\\[bmkp-delete-bookmarks]\t- Delete some bookmarks at point or all in buffer

\\[bmkp-toggle-bookmark-set-refreshes]
\t- Toggle whether `bookmark-set' refreshes the bookmark list
\\[bmkp-make-function-bookmark]
\t- Create a function bookmark
\\[bmkp-bmenu-make-sequence-from-marked]
\t- Create a sequence bookmark from the marked bookmarks


Jump to (Visit)
---------------

\\[bookmark-bmenu-select]\t- Jump to this bookmark and also visit bookmarks marked `>'
\\[bookmark-bmenu-this-window]\t- Jump to this bookmark in the same window
\\[bookmark-bmenu-other-window]\t- Jump to this bookmark in another window
\\[bookmark-bmenu-switch-other-window]\t- Visit this bookmark in other window, without selecting it
\\[bookmark-bmenu-1-window]\t- Jump to this bookmark in a full-frame window
\\[bookmark-bmenu-2-window]\t- Jump to this bookmark and last-visited bookmark

\\[bookmark-jump]\t- Jump to a bookmark by name
\\[bmkp-jump-to-type]\t- Jump to a bookmark by type
\\[bmkp-jump-in-navlist]\t- Jump to a bookmark in the navigation list
\\[bmkp-lighted-jump]\t- Jump to a highlighted bookmark
\\[bmkp-desktop-jump]\t- Jump to a desktop bookmark
\\[bmkp-bookmark-list-jump]\t- Jump to a bookmark-list bookmark
\\[bmkp-bookmark-file-jump]\t- Jump to a bookmark-file bookmark
\\[bmkp-dired-jump]\t- Jump to a Dired bookmark
\\[bmkp-file-jump]\t- Jump to a file bookmark
\\[bmkp-local-file-jump]\t- Jump to a local-file bookmark
\\[bmkp-remote-file-jump]\t- Jump to a remote-file bookmark
\\[bmkp-region-jump]\t- Jump to a region bookmark
\\[bmkp-info-jump]\t- Jump to an Info bookmark
\\[bmkp-man-jump]\t- Jump to a `man'-page bookmark
\\[bmkp-non-file-jump]\t- Jump to a non-file (buffer) bookmark
\\[bmkp-gnus-jump]\t- Jump to a Gnus bookmark
\\[bmkp-varlist-jump]\t- Jump to a variable-list bookmark
\\[bmkp-w3m-jump]\t- Jump to a W3M (URL) bookmark
\\[bmkp-some-tags-jump]\t- Jump to a bookmark with some tags you specify
\\[bmkp-some-tags-regexp-jump]\t- Jump to a bookmark with some tags matching a regexp
\\[bmkp-all-tags-jump]\t- Jump to a bookmark with all tags you specify
\\[bmkp-all-tags-regexp-jump]\t- Jump to a bookmark with all tags matching a regexp


Cycle Bookmarks and Autonamed Bookmarks
---------------------------------------

\\[bmkp-toggle-autonamed-bookmark-set/delete]\t- Create/delete autonamed bookmark at point
C-x p n n ...\t- Next     bookmark in buffer  (C-x p C-n, C-x p down)
C-x p p p ...\t- Previous bookmark in buffer  (C-x p C-p, C-x p up)
C-x p f f ...\t- Next    bookmark in navlist (C-x p C-f, C-x p right)
C-x p b b ...\t- Previous bookmark in navlist (C-x p C-b, C-x p left)
C-x p next  ...\t- MS Windows `Open' next     bookmark in navlist
C-x p prior ...\t- MS Windows `Open' previous bookmark in navlist
C-x C-down  ...\t- Next     highlighted bookmark in buffer
C-x C-up    ...\t- Previous highlighted bookmark in buffer

\\[bmkp-delete-all-autonamed-for-this-buffer]
\t- Delete all autonamed bookmarks in current buffer


Search-and-Replace Targets (in sort order)
--------------------------

\\[bmkp-bmenu-search-marked-bookmarks-regexp]\t\t- Regexp-search the marked file bookmarks
\\[bmkp-bmenu-query-replace-marked-bookmarks-regexp]\t\t- Query-replace the marked file \
bookmarks
M-s a C-s\t- Isearch the marked bookmarks (Emacs 23+)
M-s a C-M-s\t- Regexp Isearch the marked bookmarks (Emacs 23+)


Mark/Unmark
-----------

\(Mark means `>'.  Flag means `D'.   See also `Tags', below.)

\\[bookmark-bmenu-delete]\t- Flag this bookmark `D' for deletion, then move down
\\[bookmark-bmenu-delete-backwards]\t- Flag this bookmark `D' for deletion, then move up

\\[bookmark-bmenu-mark]\t- Mark this bookmark
\\[bookmark-bmenu-unmark]\t- Unmark this bookmark (`C-u': move up one line)
\\[bookmark-bmenu-backup-unmark]\t- Unmark previous bookmark (move up, then unmark)

\\[bmkp-bmenu-mark-all]\t- Mark all bookmarks
\\[bmkp-bmenu-regexp-mark]\t- Mark all bookmarks whose names match a regexp
\\[bmkp-bmenu-unmark-all]\t- Unmark all bookmarks (`C-u': interactive query)
\\[bmkp-bmenu-toggle-marks]\t- Toggle marks: unmark the marked and mark the unmarked

\\[bmkp-bmenu-mark-non-file-bookmarks]\t- Mark non-file (i.e. buffer) bookmarks
\\[bmkp-bmenu-mark-dired-bookmarks]\t- Mark Dired bookmarks
\\[bmkp-bmenu-mark-file-bookmarks]\t- Mark file & directory bookmarks (`C-u': local only)
\\[bmkp-bmenu-mark-gnus-bookmarks]\t- Mark Gnus bookmarks
\\[bmkp-bmenu-mark-lighted-bookmarks]\t- Mark the highlighted bookmarks
\\[bmkp-bmenu-mark-info-bookmarks]\t- Mark Info bookmarks
\\[bmkp-bmenu-mark-desktop-bookmarks]\t- Mark desktop bookmarks
\\[bmkp-bmenu-mark-bookmark-file-bookmarks]\t- Mark bookmark-file bookmarks
\\[bmkp-bmenu-mark-man-bookmarks]\t- Mark `man' page bookmarks (that's `M' twice, not Meta-M)
\\[bmkp-bmenu-mark-region-bookmarks]\t- Mark region bookmarks
\\[bmkp-bmenu-mark-w3m-bookmarks]\t- Mark W3M (URL) bookmarks


Modify
------

\(See also `Tags', next.)

\\[bookmark-bmenu-edit-annotation]\t- Edit this bookmark's annotation
\\[bmkp-bmenu-edit-bookmark]\t- Rename and relocate this bookmark
\\[bookmark-bmenu-rename]\t- Rename this bookmark
\\[bookmark-bmenu-relocate]\t- Relocate this bookmark (change file)
\\[bookmark-bmenu-execute-deletions]\t- Delete (visible) bookmarks flagged `D'
\\[bmkp-bmenu-delete-marked]\t- Delete (visible) bookmarks marked `>'


Tags
----

\\[bmkp-add-tags]\t- Add some tags to a bookmark
\\[bmkp-remove-tags]\t- Remove some tags from a bookmark
\\[bmkp-remove-all-tags]\t- Remove all tags from a bookmark
\\[bmkp-remove-tags-from-all]\t- Remove some tags from all bookmarks
\\[bmkp-rename-tag]\t- Rename a tag in all bookmarks
\\[bmkp-list-all-tags]\t- List all tags used in any bookmarks (`C-u': show tag values)
\\[bmkp-bmenu-set-tag-value]\t- Set the value of a tag (as attribute)

\\[bmkp-bmenu-add-tags-to-marked]\t- Add some tags to the marked bookmarks
\\[bmkp-bmenu-remove-tags-from-marked]\t- Remove some tags from the marked bookmarks

\\[bmkp-bmenu-mark-bookmarks-tagged-regexp]\t- Mark bookmarks having at least one \
tag that matches a regexp

\\[bmkp-bmenu-mark-bookmarks-tagged-some]\t- Mark bookmarks having at least one tag \
in a set    (OR)
\\[bmkp-bmenu-mark-bookmarks-tagged-all]\t- Mark bookmarks having all of the tags \
in a set     (AND)
\\[bmkp-bmenu-mark-bookmarks-tagged-none]\t- Mark bookmarks not having any of the tags \
in a set (NOT OR)
\\[bmkp-bmenu-mark-bookmarks-tagged-not-all]\t- Mark bookmarks not having all of the \
tags in a set (NOT AND)

\\[bmkp-bmenu-unmark-bookmarks-tagged-some]\t- Unmark bookmarks having at least one \
tag in a set  (OR)
\\[bmkp-bmenu-unmark-bookmarks-tagged-all]\t- Unmark bookmarks having all of the tags \
in a set   (AND)
\\[bmkp-bmenu-unmark-bookmarks-tagged-none]\t- Unmark bookmarks not having any tags \
in a set      (NOT OR)
\\[bmkp-bmenu-unmark-bookmarks-tagged-not-all]\t- Unmark bookmarks not having all tags \
in a set      (NOT AND)


Bookmark Highlighting
---------------------

\\[bmkp-bmenu-show-only-lighted]\t- Show only the highlighted bookmarks
\\[bmkp-bmenu-set-lighting]\t- Set highlighting for this bookmark
\\[bmkp-bmenu-set-lighting-for-marked]\t- Set highlighting for marked bookmarks
\\[bmkp-bmenu-light]\t- Highlight this bookmark
\\[bmkp-bmenu-unlight]\t- Unhighlight this bookmark
\\[bmkp-bmenu-mark-lighted-bookmarks]\t- Mark the highlighted bookmarks
\\[bmkp-bmenu-light-marked]\t- Highlight the marked bookmarks
\\[bmkp-bmenu-unlight-marked]\t- Unhighlight the marked bookmarks
\\[bmkp-light-bookmark-this-buffer]\t- Highlight a bookmark in current buffer
\\[bmkp-unlight-bookmark-this-buffer]\t- Unhighlight a bookmark in current buffer
\\[bmkp-light-bookmarks]\t- Highlight bookmarks (see prefix arg)
\\[bmkp-unlight-bookmarks]\t- Unhighlight bookmarks (see prefix arg)
\\[bmkp-bookmarks-lighted-at-point]\t- List bookmarks highlighted at point
\\[bmkp-unlight-bookmark-here]\t- Unhighlight a bookmark at point or on same line


Sort
----

\(Repeat to cycle normal/reversed/off, except as noted.)

\\[bmkp-bmenu-sort-marked-before-unmarked]\t- Sort marked bookmarks first
\\[bmkp-bmenu-sort-by-last-buffer-or-file-access]\t- Sort by last buffer or file \
access
\\[bmkp-bmenu-sort-by-Gnus-thread]\t- Sort by Gnus thread: group, article, message
\\[bmkp-bmenu-sort-by-Info-location]\t- Sort by Info manual, node, position
\\[bmkp-bmenu-sort-by-bookmark-type]\t- Sort by bookmark type
\\[bmkp-bmenu-sort-by-bookmark-name]\t- Sort by bookmark name
\\[bmkp-bmenu-sort-by-creation-time]\t- Sort by bookmark creation time
\\[bmkp-bmenu-sort-by-last-bookmark-access]\t- Sort by last bookmark access time
\\[bmkp-bmenu-sort-by-bookmark-visit-frequency]\t- Sort by bookmark visit frequency
\\[bmkp-bmenu-sort-by-w3m-url]\t- Sort by W3M URL

\\[bmkp-bmenu-sort-by-local-file-type]\t- Sort by local file type: file, symlink, dir
\\[bmkp-bmenu-sort-by-file-name]\t- Sort by file name
\\[bmkp-bmenu-sort-by-local-file-size]\t- Sort by local file size
\\[bmkp-bmenu-sort-by-last-local-file-access]\t- Sort by last local file access
\\[bmkp-bmenu-sort-by-last-local-file-update]\t- Sort by last local file update (edit)

\\[bmkp-reverse-sort-order]\t- Reverse current sort direction       (repeat to toggle)
\\[bmkp-reverse-multi-sort-order]\t- Complement sort predicate decisions  (repeat \
to toggle)
\\[bmkp-bmenu-change-sort-order-repeat]\t- Cycle sort orders                    (repeat \
to cycle)


Hide/Show
---------

\\[bmkp-bmenu-show-all]\t- Show all bookmarks
\\[bmkp-bmenu-toggle-show-only-marked]\t- Toggle showing only marked bookmarks
\\[bmkp-bmenu-toggle-show-only-unmarked]\t- Toggle showing only unmarked bookmarks
\\[bmkp-bmenu-show-only-non-files]\t- Show only non-file (i.e. buffer) bookmarks
\\[bmkp-bmenu-show-only-dired]\t- Show only Dired bookmarks
\\[bmkp-bmenu-show-only-files]\t- Show only file & directory bookmarks (`C-u': local only)
\\[bmkp-bmenu-show-only-gnus]\t- Show only Gnus bookmarks
\\[bmkp-bmenu-show-only-info-nodes]\t- Show only Info bookmarks
\\[bmkp-bmenu-show-only-desktops]\t- Show only desktop bookmarks
\\[bmkp-bmenu-show-only-bookmark-files]\t- Show only bookmark-file bookmarks
\\[bmkp-bmenu-show-only-man-pages]\t- Show only `man' page bookmarks
\\[bmkp-bmenu-show-only-regions]\t- Show only region bookmarks
\\[bmkp-bmenu-show-only-varlists]\t- Show only variable-list bookmarks
\\[bmkp-bmenu-show-only-w3m-urls]\t- Show only W3M (URL) bookmarks
\\[bmkp-bmenu-filter-bookmark-name-incrementally]\t- Incrementally show only bookmarks \
whose names match a regexp
\\[bmkp-bmenu-filter-file-name-incrementally]\t- Incrementally show only bookmarks whose \
files match a regexp
\\[bmkp-bmenu-filter-tags-incrementally]\t- Incrementally show only bookmarks whose tags \
match a regexp
\\[bmkp-bmenu-show-only-tagged]\t- Show only bookmarks that have tags


Omit/Un-omit
------------

\\[bmkp-bmenu-show-only-omitted]\t- Show (only) the omitted bookmarks
\\[bmkp-bmenu-show-all]\t- Show the un-omitted bookmarks (all)
\\[bmkp-bmenu-omit/unomit-marked]\t- Omit the marked bookmarks; un-omit them if after \
`\\[bmkp-bmenu-show-only-omitted]'
\\[bmkp-unomit-all]\t- Un-omit all omitted bookmarks


Define Commands for `*Bookmark List*'
-------------------------------------

\\[bmkp-bmenu-define-command]\t- Define a command to restore the current sort order & filter
\\[bmkp-bmenu-define-full-snapshot-command]\t- Define a command to restore the current \
bookmark-list state
\\[bmkp-define-tags-sort-command]\t- Define a command to sort bookmarks by tags
\\[bmkp-bmenu-define-jump-marked-command]\t- Define a command to jump to a bookmark that is \
now marked


Options for `*Bookmark List*'
-----------------------------

bookmark-bmenu-file-column       - Bookmark width if files are shown
bookmark-bmenu-toggle-filenames  - Show filenames initially?

bmkp-bmenu-omitted-list     - List of omitted bookmarks
bmkp-bmenu-state-file       - File to save bookmark-list state
                                   (\"home\") nil: do not save/restore
bmkp-sort-comparer          - Initial sort
bmkp-sort-orders-for-cycling-alist
\t - Sort orders that `\\[bmkp-bmenu-change-sort-order-repeat]'... cycles


Other Options
-------------

bookmark-automatically-show-annotations  - Show annotation when visit?
bookmark-completion-ignore-case  - Case-sensitive completion?
bookmark-default-file            - File to save bookmarks in
bookmark-menu-length             - Max size of bookmark-name menu item
bookmark-save-flag               - Whether and when to save
bookmark-use-annotations         - Query for annotation when saving?
bookmark-version-control         - Numbered backups of bookmark file?

bmkp-autoname-format        - Format of autonamed bookmark name
bmkp-crosshairs-flag        - Highlight position when visit?
bmkp-menu-popup-max-length  - Use menus to choose bookmarks?
bmkp-save-new-location-flag - Save if bookmark relocated?
bmkp-sequence-jump-display-function - How to display a sequence
bmkp-sort-comparer          - Predicates for sorting bookmarks
bmkp-su-or-sudo-regexp      - Bounce-show each end of region?
bmkp-this-buffer-cycle-sort-comparer -  This-buffer cycling sort
bmkp-use-region             - Activate saved region when visit?"
  (kill-all-local-variables)
  (use-local-map bookmark-bmenu-mode-map)
  (setq truncate-lines    t
        buffer-read-only  t
        major-mode        'bookmark-bmenu-mode
        mode-name         "Bookmark Menu")
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'bookmark-bmenu-mode-hook)
    (run-hooks 'bookmark-bmenu-mode-hook)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Put `mouse-face' on whole line, with the same help-echo as for the bookmark name.
;; 2. Fit one-window frame.
;; 3. Added doc string.
;;
(defun bookmark-bmenu-show-filenames (&optional force)
  "Show file names."
  (if (and (not force) bookmark-bmenu-toggle-filenames)
      nil                               ; Already shown, so do nothing.
    (save-excursion
      (save-window-excursion
        (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
        (setq bookmark-bmenu-hidden-bookmarks  ())
        (let ((inhibit-read-only  t))
          (while (< (point) (point-max))
            (let ((bmk  (bookmark-bmenu-bookmark)))
              (setq bookmark-bmenu-hidden-bookmarks  (cons bmk bookmark-bmenu-hidden-bookmarks))
	      (let ((start  (save-excursion (end-of-line) (point))))
		(move-to-column bookmark-bmenu-file-column t))
	      (delete-region (point) (progn (end-of-line) (point)))
              (insert "  ")
              (bookmark-insert-location bmk t) ; Pass the NO-HISTORY arg.
              (when (if (fboundp 'display-color-p) ; Emacs 21+.
                        (and (display-color-p) (display-mouse-p))
                      window-system)
                (let ((help  (get-text-property (+ bmkp-bmenu-marks-width
                                                   (line-beginning-position)) 'help-echo)))
                  (put-text-property (+ bmkp-bmenu-marks-width (line-beginning-position))
                                     (point) 'mouse-face 'highlight)
                  (when help  (put-text-property (+ bmkp-bmenu-marks-width (line-beginning-position))
                                                 (point) 'help-echo help))))
              (forward-line 1))))))
    (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Add text properties when hiding filenames.
;; 2. Do not set or use `bookmark-bmenu-bookmark-column' - use `bmkp-bmenu-marks-width' always.
;; 3. Fit one-window frame.
;; 4. Added doc string.
;;
(defun bookmark-bmenu-hide-filenames (&optional force)
  "Hide filenames in bookmark-list buffer.
If either optional arg FORCE or `bookmark-bmenu-toggle-filenames' is
non-nil, then do nothing."
  (when (and (not force)  bookmark-bmenu-toggle-filenames)
    (save-excursion
      (save-window-excursion
        (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
        (setq bookmark-bmenu-hidden-bookmarks  (nreverse bookmark-bmenu-hidden-bookmarks))
        (let ((max-width  0))
          (dolist (name  bookmark-bmenu-hidden-bookmarks)
            (setq max-width  (max max-width (length name))))
          (setq max-width  (+ max-width bmkp-bmenu-marks-width))
          (save-excursion
            (let ((inhibit-read-only  t))
              (while bookmark-bmenu-hidden-bookmarks
                (move-to-column bmkp-bmenu-marks-width t)
                (bookmark-kill-line)
                (let ((name   (car bookmark-bmenu-hidden-bookmarks))
                      (start  (point))
                      end)
                  (insert name)
                  (move-to-column max-width t)
                  (setq end  (point))
                  (bmkp-bmenu-propertize-item name start end))
                (setq bookmark-bmenu-hidden-bookmarks  (cdr bookmark-bmenu-hidden-bookmarks))
                (forward-line 1)))))))
    (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Prefix arg reverses `bmkp-use-region'.
;; 2. Raise error if not in buffer `*Bookmark List*'.
;;
(defun bookmark-bmenu-1-window (&optional use-region-p) ; Bound to `1' in bookmark list
  "Select this line's bookmark, alone, in full frame.
See `bookmark-jump' for info about the prefix arg."
  (interactive "P")
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (bmkp-jump-1 (bookmark-bmenu-bookmark) 'pop-to-buffer use-region-p)
  (bury-buffer (other-buffer))
  (delete-other-windows))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Prefix arg reverses `bmkp-use-region'.
;; 2. Raise error if not in buffer `*Bookmark List*'.
;;
(defun bookmark-bmenu-2-window (&optional use-region-p) ; Bound to `2' in bookmark list
  "Select this line's bookmark, with previous buffer in second window.
See `bookmark-jump' for info about the prefix arg."
  (interactive "P")
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (let ((bookmark-name   (bookmark-bmenu-bookmark))
        (menu            (current-buffer))
        (pop-up-windows  t))
    (delete-other-windows)
    (switch-to-buffer (other-buffer))
    ;; (let ((bookmark-automatically-show-annotations nil)) ; $$$$$$ Needed?
    (bmkp-jump-1 bookmark-name 'pop-to-buffer use-region-p)
    (bury-buffer menu)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Prefix arg reverses `bmkp-use-region'.
;; 2. Raise error if not in buffer `*Bookmark List*'.
;;
(defun bookmark-bmenu-this-window (&optional use-region-p) ; Bound to `RET' in bookmark list
  "Select this line's bookmark in this window.
See `bookmark-jump' for info about the prefix arg."
  (interactive "P")
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (let ((bookmark-name  (bookmark-bmenu-bookmark)))
    (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Use `pop-to-buffer', not `switch-to-buffer-other-window'.
;; 2. Prefix arg reverses `bmkp-use-region'.
;; 3. Raise error if not in buffer `*Bookmark List*'.
;;
(defun bookmark-bmenu-other-window (&optional use-region-p) ; Bound to `o' in bookmark list
  "Select this line's bookmark in other window.  Show `*Bookmark List*' still.
See `bookmark-jump' for info about the prefix arg."
  (interactive "P")
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (let ((bookmark-name  (bookmark-bmenu-bookmark)))
    ;; (bookmark-automatically-show-annotations  t)) ; $$$$$$ Needed?
    (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Prefix arg reverses `bmkp-use-region'.
;; 2. Raise error if not in buffer `*Bookmark List*'.
;; 3. Use `bmkp-jump-1', not `bookmark--jump-via'.
;;
(defun bookmark-bmenu-switch-other-window (&optional use-region-p) ; Bound to `C-o' in bookmark list
  "Make the other window select this line's bookmark.
The current window remains selected.
See `bookmark-jump' for info about the prefix arg."
  (interactive "P")
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (let ((bookmark-name             (bookmark-bmenu-bookmark))
        (pop-up-windows            t)
        (same-window-buffer-names  ())
        (same-window-regexps       ()))
    ;; (bookmark-automatically-show-annotations t)) ; $$$$$$ Needed?
    (bmkp-jump-1 bookmark-name 'display-buffer use-region-p)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Prefix arg reverses `bmkp-use-region'.
;; 2. Raise error if not in buffer `*Bookmark List*'.
;;
(defun bookmark-bmenu-other-window-with-mouse (event &optional use-region-p)
  "Select clicked bookmark in other window.  Show `*Bookmark List*' still."
  (interactive "e\nP")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (save-excursion (goto-char (posn-point (event-end event)))
                    (bookmark-bmenu-other-window use-region-p))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added optional arg MSGP.
;; 2. Call `bookmark-show-annotation' with arg MSGP.
;; 3. Raise error if not in buffer `*Bookmark List*'.
;;
(defun bookmark-bmenu-show-annotation (msgp)
  "Show the annotation for the current bookmark in another window."
  (interactive "p")
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (let ((bookmark  (bookmark-bmenu-bookmark)))
    (bookmark-show-annotation bookmark msgp)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added optional arg MARKEDP: handle bookmarks marked `>', not just those flagged `D'.
;; 2. Use `bookmark-bmenu-surreptitiously-rebuild-list', instead of using
;;    `bookmark-bmenu-list', updating the modification count, and saving.
;; 3. Update `bmkp-latest-bookmark-alist' to reflect the deletions.
;; 4. Use `bmkp-bmenu-goto-bookmark-named'.
;; 5. Added status messages.
;; 6. Raise error if not in buffer `*Bookmark List*'.
;;
;;;###autoload
(defun bookmark-bmenu-execute-deletions (&optional markedp) ; Bound to `x' in bookmark list
  "Delete (visible) bookmarks flagged `D'.
With a prefix argument, delete the bookmarks marked `>' instead, after
confirmation."
  (interactive "P")
  (bmkp-barf-if-not-in-menu-list)
  (if (or (not markedp) (yes-or-no-p "Delete bookmarks marked `>' (not `D') "))
      (let* ((mark-type  (if markedp "^>" "^D"))
             (o-str      (and (not (looking-at mark-type)) (bookmark-bmenu-bookmark)))
             (o-point    (point))
             (count      0))
        (message "Deleting bookmarks...")
        (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
        (while (re-search-forward mark-type (point-max) t)
          (let ((bmk  (bookmark-bmenu-bookmark)))
            (bookmark-delete bmk 'batch)
            (setq count                       (1+ count)
                  bmkp-latest-bookmark-alist  (delete (assoc bmk bmkp-latest-bookmark-alist)
                                                      bmkp-latest-bookmark-alist))))
        (if (<= count 0)
            (message (if markedp "No marked bookmarks" "No bookmarks flagged for deletion"))
          (bookmark-bmenu-surreptitiously-rebuild-list)
          (message "Deleted %d bookmarks" count))
        (if o-str
            (bmkp-bmenu-goto-bookmark-named o-str)
          (goto-char o-point)
          (beginning-of-line)))
    (message "OK, nothing deleted")))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Do not call `bookmark-bmenu-list' (it was already called).
;; 2. Raise error if not in buffer `*Bookmark List*'.
;;
;;;###autoload
(defun bookmark-bmenu-rename ()         ; Bound to `r' in bookmark list
  "Rename bookmark on current line.  Prompts for a new name."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (let ((new-name  (bookmark-rename (bookmark-bmenu-bookmark))))
    (when (or (search-forward new-name (point-max) t) (search-backward new-name (point-min) t))
      (beginning-of-line))))
 
;;(@* "Bookmark+ Functions (`bmkp-*')")
;;; Bookmark+ Functions (`bmkp-*') -----------------------------------

(defun bmkp-completing-read-lax (prompt &optional default alist pred hist)
  "Same as `bookmark-completing-read', but completion is lax."
  (unwind-protect
       (progn (define-key minibuffer-local-completion-map "\C-w" 'bookmark-yank-word)
              (define-key minibuffer-local-completion-map "\C-u" 'bookmark-insert-current-bookmark)
              (bmkp-completing-read-1 prompt default alist pred hist t))
    (define-key minibuffer-local-completion-map "\C-w" nil)
    (define-key minibuffer-local-completion-map "\C-u" nil)))

(defun bmkp-completing-read-1 (prompt default alist pred hist laxp)
  "Helper for `bookmark-completing-read(-lax)'.
LAXP non-nil means use lax completion."
  (bookmark-maybe-load-default-file)
  (setq alist  (or alist bookmark-alist))
  (if (and (not laxp)
           (listp last-nonmenu-event)
           (or (eq t bmkp-menu-popup-max-length)
               (and (integerp bmkp-menu-popup-max-length)
                    (< (length alist) bmkp-menu-popup-max-length))))
      (bookmark-menu-popup-paned-menu
       t prompt
       (if bmkp-sort-comparer           ; Test whether to sort, but always use `string-lessp'.
           (sort (bookmark-all-names alist) 'string-lessp)
         (bookmark-all-names alist)))
    (let* ((icicle-delete-candidate-object  (lambda (cand) ; For `S-delete' in Icicles.
                                              (bookmark-delete
                                               (icicle-transform-multi-completion cand))))
           (completion-ignore-case          bookmark-completion-ignore-case)
           (default                         default)
           (prompt                          (if default
                                                (concat prompt (format " (%s): " default))
                                              (concat prompt ": ")))
           (str                             (completing-read
                                             prompt alist pred (not laxp) nil
                                             (or hist 'bookmark-history) default)))
      (if (and (string-equal "" str) default) default str))))

(defun bmkp-jump-1 (bookmark-name display-function use-region-p)
  "Helper function for `bookmark-jump' commands."
  (unless bookmark-name (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark-name)
  (let ((bmkp-use-region  (if use-region-p (not bmkp-use-region) bmkp-use-region)))
    (bookmark--jump-via bookmark-name display-function)))

(defun bmkp-select-buffer-other-window (buffer)
  "Select BUFFER in another window.
If `bmkp-other-window-pop-to-flag' is non-nil, then use
`pop-to-buffer'.  Otherwise, use `switch-to-buffer-other-window'."
  (if bmkp-other-window-pop-to-flag
      (pop-to-buffer buffer t)
    (switch-to-buffer-other-window buffer)))  

(defun bmkp-maybe-save-bookmarks ()
  "Increment save counter and maybe save `bookmark-alist'."
  (setq bookmark-alist-modification-count  (1+ bookmark-alist-modification-count))
  (when (bookmark-time-to-save-p) (bookmark-save)))

(defun bmkp-edit-bookmark (bookmark-name)
  "Edit bookmark's name and file name, and maybe save them.
BOOKMARK-NAME is the current (old) name of the bookmark."
  (let* ((bookmark-filename  (bookmark-get-filename bookmark-name))
         (new-bmk-name       (read-from-minibuffer "New bookmark name: " nil nil nil nil
                                                   bookmark-name))
         (new-filename       (read-from-minibuffer "New file name (location): " nil nil nil nil
                                                   bookmark-filename)))
    (when (and (not (equal new-bmk-name "")) (not (equal new-filename ""))
               (y-or-n-p "Save changes? "))
      (bookmark-rename bookmark-name new-bmk-name 'batch)
      (bookmark-set-filename new-bmk-name new-filename)
      ;; Change location for Dired too, but not if different from original file name (e.g. a cons).
      (let ((dired-dir  (bookmark-prop-get bookmark-name 'dired-directory)))
        (when (and dired-dir (equal dired-dir bookmark-filename))
          (bookmark-prop-set bookmark-name 'dired-directory new-filename)))
      (bmkp-maybe-save-bookmarks)
      (list new-bmk-name new-filename))))

(defun bmkp-record-visit (bookmark &optional batch)
  "Update the data recording a visit to BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
This increments the `visits' entry and sets the `time' entry to the
current time.  If either an entry is not present, it is added (with 0
value for `visits')."
  (let ((vis  (bookmark-prop-get bookmark 'visits)))
    (if vis  (bookmark-prop-set bookmark 'visits (1+ vis))  (bookmark-prop-set bookmark 'visits 0))
    (bookmark-prop-set bookmark 'time (current-time))
    (unless batch (bookmark-bmenu-surreptitiously-rebuild-list))
    (let ((bookmark-save-flag  nil))  (bmkp-maybe-save-bookmarks))))

(defun bmkp-default-bookmark-name (&optional alist)
  "Default bookmark name.
If in buffer `*Bookmark List*' then the current line's bookmark.
Otherwise, the last-used bookmark in the current buffer.

Non-nil ALIST means return nil unless the default names a bookmark in
ALIST."
  (let ((name  (if (equal (buffer-name (current-buffer)) "*Bookmark List*")
                   (bookmark-bmenu-bookmark)
                 bookmark-current-bookmark)))
    (when alist (setq name  (car (assoc name alist))))
    name))

(defun bmkp-buffer-names ()
  "Buffer names used by existing bookmarks that really have buffers.
This excludes buffers for bookmarks such as desktops that are not
really associated with a buffer."
  (let ((bufs  ())
        buf)
    (dolist (bmk  bookmark-alist)
      (when (and (not (bmkp-desktop-bookmark-p        bmk))
                 (not (bmkp-bookmark-file-bookmark-p  bmk))
                 (not (bmkp-sequence-bookmark-p       bmk))
                 (not (bmkp-function-bookmark-p       bmk))
                 (not (bmkp-varlist-bookmark-p        bmk))
                 (setq buf  (bmkp-get-buffer-name     bmk)))
        (add-to-list 'bufs buf)))
    bufs))

(defun bmkp-file-names ()
  "The absolute file names used by the existing bookmarks.
This excludes the pseudo file name `bmkp-non-file-filename'."
  (let ((files  ())
        file)
    (dolist (bmk  bookmark-alist)
      (when (and (setq file  (bookmark-get-filename bmk))  (not (equal file bmkp-non-file-filename)))
        (add-to-list 'files file)))
    files))

;;;###autoload
(defun bmkp-send-bug-report ()          ; Not bound
  "Send a bug report about a Bookmark+ problem."
  (interactive)
  (browse-url (format (concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
Bookmark+ bug: \
&body=Describe bug below, using a precise recipe that starts with `emacs -Q' or `emacs -q'.  \
Be sure to mention the `Update #' from the `bookmark+.el' file header.%%0A%%0AEmacs version: %s")
                      (emacs-version))))

;;;###autoload
(defun bmkp-toggle-bookmark-set-refreshes () ; Not bound
  "Toggle `bookmark-set' refreshing `bmkp-latest-bookmark-alist'.
Add/remove `bmkp-refresh-latest-bookmark-list' to/from
`bmkp-after-set-hook'."
  (interactive)
  (if (member 'bmkp-refresh-latest-bookmark-list bmkp-after-set-hook)
      (remove-hook 'bmkp-after-set-hook 'bmkp-refresh-latest-bookmark-list)
    (add-hook 'bmkp-after-set-hook 'bmkp-refresh-latest-bookmark-list)))

(defun bmkp-refresh-latest-bookmark-list ()
  "Refresh `bmkp-latest-bookmark-alist' to reflect `bookmark-alist'."
  (setq bmkp-latest-bookmark-alist  (if bmkp-bmenu-filter-function
                                        (funcall bmkp-bmenu-filter-function)
                                      bookmark-alist)))

;;;###autoload
(defun bmkp-toggle-saving-menu-list-state () ; Bound to `M-l' in bookmark list
  "Toggle the value of option `bmkp-bmenu-state-file'.
Tip: You can use this before quitting Emacs, to not save the state.
If the initial value of `bmkp-bmenu-state-file' is nil, then this
command has no effect."
  (interactive)
  (unless (or bmkp-last-bmenu-state-file bmkp-bmenu-state-file)
    (error "Cannot toggle: initial value of `bmkp-bmenu-state-file' is nil"))
  (setq bmkp-last-bmenu-state-file  (prog1 bmkp-bmenu-state-file
                                      (setq bmkp-bmenu-state-file  bmkp-last-bmenu-state-file)))
  (message (if bmkp-bmenu-state-file
               "Autosaving of bookmark list state is now ON"
             "Autosaving of bookmark list state is now OFF")))

;;;###autoload
(defun bmkp-toggle-saving-bookmark-file () ; Bound to `M-~' in bookmark list
  "Toggle the value of option `bookmark-save-flag'.
If the initial value of `bookmark-save-flag' is nil, then this
command has no effect."
  (interactive)
  (unless (or bmkp-last-save-flag-value bookmark-save-flag)
    (error "Cannot toggle: initial value of `bookmark-save-flag' is nil"))
  (setq bmkp-last-save-flag-value  (prog1 bookmark-save-flag
                                     (setq bookmark-save-flag  bmkp-last-save-flag-value)))
  (message (if bookmark-save-flag
               "Autosaving of current bookmark file is now ON"
             "Autosaving of current bookmark file is now OFF")))

;;;###autoload
(defun bmkp-make-function-bookmark (bookmark-name function) ; Not bound
  "Create a bookmark that invokes FUNCTION when \"jumped\" to.
You are prompted for the bookmark name and the name of the function.
Returns the new bookmark (internal record)."
  (interactive (list (read-string "Bookmark: ") (completing-read "Function: " obarray 'functionp)))
  (bookmark-store bookmark-name `((filename . ,bmkp-non-file-filename)
                                  (position . 0)
                                  (function . ,(read function))
                                  (handler  . bmkp-jump-function))
                  nil)
  (let ((new  (bookmark-get-bookmark bookmark-name 'noerror)))
    (unless (memq new bmkp-latest-bookmark-alist)
      (setq bmkp-latest-bookmark-alist  (cons new bmkp-latest-bookmark-alist)))
    (bookmark-bmenu-surreptitiously-rebuild-list)
    new))

;;;###autoload
(defun bmkp-switch-bookmark-file (file &optional no-msg) ; Bound to `L' in bookmark list
  "Switch to a different bookmark file, FILE.
Replace all currently existing bookmarks with the newly loaded
bookmarks.  Change the value of `bmkp-current-bookmark-file' to FILE,
so bookmarks will subsequently be saved to FILE.

`bookmark-default-file' is unaffected, so your next Emacs session will
still use `bookmark-default-file' for the initial set of bookmarks."
  (interactive
   (list
    (let* ((bfile                     (if (bmkp-same-file-p bmkp-current-bookmark-file
                                                            bmkp-last-bookmark-file)
                                          bookmark-default-file
                                        bmkp-last-bookmark-file))
           (dir                       (or (file-name-directory bfile) "~/"))
           (insert-default-directory  t))
      (read-file-name "Switch to bookmark file: " dir bfile 'confirm))))
  (bookmark-load file t no-msg))

;;;###autoload
(defun bmkp-switch-to-last-bookmark-file (&optional no-msg) ; Not bound
  "Switch back to the last-used bookmarkfile.
Replace all currently existing bookmarks with those newly loaded from
the last-used file.  Swap the values of `bmkp-last-bookmark-file' and
`bmkp-current-bookmark-file'."
  (interactive)
  (bookmark-load (or bmkp-last-bookmark-file bookmark-default-file) t no-msg))

;;;###autoload
(defun bmkp-use-bookmark-file-create (file) ; Not bound
  "Switch current bookmark file to FILE, creating it if it does not exist.
Interactively, you are prompted for the file name.  The default is
`.emacs.bmk' in the current directory, but you can enter any file
name, anywhere.

If there is no file with the name you provide then a new, an empty
bookmark file with that name is created.

You are prompted to confirm the bookmark-file switch.

Returns FILE."
  (interactive
   (list
    (let* ((insert-default-directory  t)
           (bmk-file                  (expand-file-name (read-file-name
                                                         "Use bookmark file: " nil
                                                         (if (> emacs-major-version 22)
                                                             (list ".emacs.bmk" bookmark-default-file)
                                                           ".emacs.bmk")))))
      bmk-file)))
  (unless (file-readable-p file) (bmkp-empty-file file))
  (when (y-or-n-p (format "Use `%s' as the current bookmark file? " file))
    (bmkp-switch-bookmark-file file))
  file)

;;;###autoload
(defun bmkp-crosshairs-highlight ()     ; Not bound
  "Call `crosshairs-highlight', unless the region is active.
You can add this to hook `bookmark-after-jump-hook'.
You need library `crosshairs.el' to use this command."
  (interactive)
  (when (> emacs-major-version 21)      ; No-op for Emacs 20-21.
    (unless (condition-case nil (require 'crosshairs nil t) (error nil))
      (error "You need library `crosshairs.el' to use this command"))
    (unless mark-active
      (let ((crosshairs-overlay-priority  (and (boundp 'bmkp-light-priorities)
                                               (1+ (apply #'max
                                                          (mapcar #'cdr bmkp-light-priorities))))))
        (crosshairs-highlight)))))

;;;###autoload
(defun bmkp-choose-navlist-from-bookmark-list (bookmark-name &optional alist) ; Bound to `C-x p B'
  "Choose a bookmark-list bookmark and set the bookmark navigation list.
The navigation-list variable, `bmkp-nav-alist', is set to the list of
bookmarks that would be displayed by `bookmark-bmenu-list' (`C-x r l')
for the chosen bookmark-list bookmark, sorted and filtered as
appropriate.

Instead of choosing a bookmark-list bookmark, you can choose the
pseudo-bookmark `CURRENT *Bookmark List*'.  The bookmarks used for the
navigation list are those that would be currently shown in the
`*Bookmark List*' (even if the list is not currently displayed)."
  (interactive
   (let ((bookmark-alist  (cons (bmkp-current-bookmark-list-state) (bmkp-bookmark-list-alist-only))))
     (list (bmkp-read-bookmark-for-type "bookmark-list " bookmark-alist nil nil
                                        'bmkp-bookmark-list-history)
           bookmark-alist)))
  (let ((state  (let ((bookmark-alist  (or alist (cons (bmkp-current-bookmark-list-state)
                                                       (bmkp-bookmark-list-alist-only)))))
                  (bookmark-prop-get bookmark-name 'bookmark-list))))
    (let ((bmkp-sort-comparer               (cdr (assq 'last-sort-comparer          state)))
          (bmkp-reverse-sort-p              (cdr (assq 'last-reverse-sort-p         state)))
          (bmkp-reverse-multi-sort-p        (cdr (assq 'last-reverse-multi-sort-p   state)))
          (bmkp-bmenu-omitted-list          (cdr (assq 'last-omitted-list           state)))
          (bmkp-bmenu-filter-function       (cdr (assq 'last-bmenu-filter-function  state)))
          (bmkp-bmenu-filter-pattern        (or (cdr (assq 'last-bmenu-filter-pattern   state)) ""))
          (bmkp-bmenu-title                 (cdr (assq 'last-bmenu-title            state)))
          (bookmark-bmenu-toggle-filenames  (cdr (assq 'last-bmenu-toggle-filenames state))))
      (setq bmkp-nav-alist             (bmkp-sort-and-remove-dups
                                        (if bmkp-bmenu-filter-function
                                            (funcall bmkp-bmenu-filter-function)
                                          (if (string= "CURRENT *Bookmark List*" bookmark-name)
                                              (or bmkp-latest-bookmark-alist bookmark-alist)
                                            bookmark-alist))
                                        (and (not (eq bmkp-bmenu-filter-function
                                                      'bmkp-omitted-alist-only))
                                             bmkp-bmenu-omitted-list))
            bmkp-current-nav-bookmark  (car bmkp-nav-alist))))
  (message "Bookmark navigation list is now %s"
           (if (and (string= "CURRENT *Bookmark List*" bookmark-name)
                    (not (get-buffer "*Bookmark List*")))
               "the global bookmark list"
             (format "`%s'" bookmark-name))))

(defun bmkp-current-bookmark-list-state ()
  "Pseudo-bookmark for the current `*Bookmark List*' state."
  (bookmark-bmenu-surreptitiously-rebuild-list)
  (cons "CURRENT *Bookmark List*" (bmkp-make-bookmark-list-record)))

(defun bmkp-choose-navlist-of-type (type) ; Bound to `C-x p :'
  "Set the bookmark navigation list to the bookmarks of a type you choose.
The pseudo-type `any' sets the navigation list to all bookmarks.
This sets variable `bmkp-nav-alist'."
  (interactive (let ((completion-ignore-case  t)
                     (type                    (completing-read "Type: "
                                                               (cons '("any" . bookmark-history)
                                                                     bmkp-types-alist)
                                                               nil t)))
                 (list type)))
  (setq bmkp-nav-alist  (if (equal "any" type)
                            bookmark-alist
                          (funcall (intern (format "bmkp-%s-alist-only" type)))))
  (unless bmkp-nav-alist (error "No bookmarks"))
  (setq bmkp-current-nav-bookmark  (car bmkp-nav-alist))
  (message "Bookmark navigation list is now %s"
           (if (equal "any" type) "all bookmarks" (format "for type `%s'" type))))

(defun bmkp-autonamed-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a (valid) autonamed bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (setq bookmark  (bookmark-get-bookmark bookmark 'NOERROR))
  (if (not bookmark)
      nil
    (string-match (format bmkp-autoname-format ".*")
                  (bookmark-name-from-full-record bookmark))))

(defun bmkp-autonamed-bookmark-for-buffer-p (bookmark buffer-name)
  "Return non-nil if BOOKMARK is a (valid) autonamed bookmark for BUFFER.
BOOKMARK is a bookmark name or a bookmark record.
BUFFER-NAME is a string matching the buffer-name part of an autoname."
  (setq bookmark  (bookmark-get-bookmark bookmark 'NOERROR))
  (if (not bookmark)
      nil
    (string-match (format bmkp-autoname-format (regexp-quote buffer-name))
                  (bookmark-name-from-full-record bookmark))))

(defun bmkp-update-autonamed-bookmark (bookmark)
  "Update the name and position of the autonamed BOOKMARK at point.
BOOKMARK is a bookmark name or a bookmark record.
Return the updated BOOKMARK: If input was a bookmark name, then return
 then new name, else the new (full) bookmark.
It is a good idea to set BOOKMARK to the result of this call."
  (let ((namep  (stringp bookmark)))
    (setq bookmark  (bookmark-get-bookmark bookmark))
    (bookmark-set-position bookmark (point))
    ;; Autonamed bookmarks do not have regions.  Update `end-position' to be the same as `position'.
    (when (bmkp-get-end-position bookmark)
      (bookmark-prop-set bookmark 'end-position (point)))
    (let ((newname  (funcall bmkp-autoname-bookmark-function (point))))
      (bookmark-rename (bookmark-name-from-full-record bookmark) newname 'batch)
      (when (get-buffer-create "*Bookmark List*")
        (bmkp-refresh-menu-list (bookmark-name-from-full-record bookmark))) ; So display new name.
      (bmkp-maybe-save-bookmarks))
    (if namep (bookmark-name-from-full-record bookmark) bookmark))) ; Return updated bookmark or name.


;;(@* "Menu-List (`*-bmenu-*') Filter Commands")
;;  *** Menu-List (`*-bmenu-*') Filter Commands ***

;;;###autoload
(defun bmkp-bmenu-show-only-bookmark-files () ; Bound to `X S' in bookmark list
  "Display (only) the bookmark-file bookmarks."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-bmenu-filter-function  'bmkp-bookmark-file-alist-only
        bmkp-bmenu-title            "Bookmark-File Bookmarks")
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order) "Only bookmark-file bookmarks are shown")))

;;;###autoload
(defun bmkp-bmenu-show-only-desktops () ; Bound to `K S' in bookmark list
  "Display (only) the desktop bookmarks."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-bmenu-filter-function  'bmkp-desktop-alist-only
        bmkp-bmenu-title            "Desktop Bookmarks")
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order) "Only desktop bookmarks are shown")))

;;;###autoload
(defun bmkp-bmenu-show-only-dired ()    ; Bound to `M-d M-s' in bookmark list
  "Display (only) the Dired bookmarks."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-bmenu-filter-function  'bmkp-dired-alist-only
        bmkp-bmenu-title            "Dired Bookmarks")
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order) "Only Dired bookmarks are shown")))

;;;###autoload
(defun bmkp-bmenu-show-only-files (arg) ; Bound to `F S' in bookmark list
  "Display a list of file and directory bookmarks (only).
With a prefix argument, do not include remote files or directories."
  (interactive "P")
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-bmenu-filter-function  (if arg 'bmkp-local-file-alist-only 'bmkp-file-alist-only)
        bmkp-bmenu-title            (if arg
                                        "Local File and Directory Bookmarks"
                                      "File and Directory Bookmarks"))
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order) "Only file bookmarks are shown")))

;;;###autoload
(defun bmkp-bmenu-show-only-non-files () ; Bound to `B S' in bookmark list
  "Display (only) the non-file bookmarks."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-bmenu-filter-function  'bmkp-non-file-alist-only
        bmkp-bmenu-title            "Non-File Bookmarks")
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order) "Only non-file bookmarks are shown")))

;;;###autoload
(defun bmkp-bmenu-show-only-gnus ()     ; Bound to `G S' in bookmark list
  "Display (only) the Gnus bookmarks."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-bmenu-filter-function  'bmkp-gnus-alist-only
        bmkp-bmenu-title            "Gnus Bookmarks")
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order) "Only Gnus bookmarks are shown")))

;;;###autoload
(defun bmkp-bmenu-show-only-info-nodes () ; Bound to `I S' in bookmark list
  "Display (only) the Info bookmarks."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-bmenu-filter-function  'bmkp-info-alist-only
        bmkp-bmenu-title            "Info Bookmarks")
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order) "Only Info bookmarks are shown")))

;;;###autoload
(defun bmkp-bmenu-show-only-man-pages () ; Bound to `M S' in bookmark list
  "Display (only) the `man' page bookmarks."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-bmenu-filter-function  'bmkp-man-alist-only
        bmkp-bmenu-title            "`man' Page Bookmarks")
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order) "Only `man' page bookmarks are shown")))

;;;###autoload
(defun bmkp-bmenu-show-only-regions ()  ; Bound to `R S' in bookmark list
  "Display (only) the bookmarks that record a region."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-bmenu-filter-function  'bmkp-region-alist-only
        bmkp-bmenu-title            "Region Bookmarks")
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order) "Only bookmarks with regions are shown")))

;;;###autoload
(defun bmkp-bmenu-show-only-varlists () ; Bound to `V S' in bookmark list
  "Display (only) the variable-list bookmarks."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-bmenu-filter-function  'bmkp-varlist-alist-only
        bmkp-bmenu-title            "Variable-list Bookmarks")
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order) "Only variable-list bookmarks are shown")))

;;;###autoload
(defun bmkp-bmenu-show-only-specific-buffer (&optional buffer) ; Bound to `= b S' in bookmark list
  "Display (only) the bookmarks for BUFFER.
Interactively, read the BUFFER name.
If BUFFER is non-nil, set `bmkp-last-specific-buffer' to it."
  (interactive (list (bmkp-completing-read-buffer-name)))
  (bmkp-barf-if-not-in-menu-list)
  (when buffer (setq bmkp-last-specific-buffer  buffer))
  (setq bmkp-bmenu-filter-function  'bmkp-last-specific-buffer-alist-only
        bmkp-bmenu-title            (format "Buffer `%s' Bookmarks" bmkp-last-specific-buffer))
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order)
                               (format "Only bookmarks for buffer `%s' are shown"
                                       bmkp-last-specific-buffer))))

;;;###autoload
(defun bmkp-bmenu-show-only-specific-file (&optional file) ; Bound to `= f S' in bookmark list
  "Display (only) the bookmarks for FILE, an absolute file name.
Interactively, read the FILE name.
If FILE is non-nil, set `bmkp-last-specific-file' to it."
  (interactive (list (bmkp-completing-read-file-name)))
  (bmkp-barf-if-not-in-menu-list)
  (when file (setq bmkp-last-specific-file  file))
  (setq bmkp-bmenu-filter-function  'bmkp-last-specific-file-alist-only
        bmkp-bmenu-title            (format "File `%s' Bookmarks" bmkp-last-specific-file))
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order)
                               (format "Only bookmarks for file `%s' are shown"
                                       bmkp-last-specific-file))))

;;;###autoload
(defun bmkp-this-buffer-bmenu-list ()   ; Bound to `C-x p .'
  "Show the bookmark list just for bookmarks for the current buffer.
Set `bmkp-last-specific-buffer' to the current buffer name."
  (interactive)
  (setq bmkp-last-specific-buffer   (buffer-name)
        bmkp-bmenu-filter-function  'bmkp-last-specific-buffer-alist-only
        bmkp-bmenu-title            (format "Buffer `%s' Bookmarks" bmkp-last-specific-buffer))
  (let ((bookmark-alist         (funcall bmkp-bmenu-filter-function))
        (bmkp-bmenu-state-file  nil))   ; Prevent restoring saved state.
    (unless bookmark-alist (error "No bookmarks for buffer `%s'" bmkp-last-specific-buffer))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (pop-to-buffer (get-buffer-create "*Bookmark List*"))
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order)
                               (format "Only bookmarks for buffer `%s' are shown"
                                       bmkp-last-specific-buffer)))
  (raise-frame))

;;;###autoload
(defun bmkp-navlist-bmenu-list ()       ; Bound to `C-x p N'
  "Show the bookmark list just for bookmarks from the navigation list."
  (interactive)
  (unless bmkp-nav-alist
    (bookmark-maybe-load-default-file)
    (setq bmkp-nav-alist  bookmark-alist)
    (unless bmkp-nav-alist (error "No bookmarks"))
    (setq bmkp-current-nav-bookmark  (car bmkp-nav-alist))
    (message "Bookmark navigation list is now the global bookmark list") (sit-for 2))
  (setq bmkp-bmenu-title  "Current Navlist Bookmarks")
  (let ((bookmark-alist         bmkp-nav-alist)
        (bmkp-bmenu-state-file  nil))   ; Prevent restoring saved state.
    (unless bookmark-alist (error "No bookmarks"))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (pop-to-buffer (get-buffer-create "*Bookmark List*"))
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order)
                               "Only bookmarks for the navigation list are shown"))
  (raise-frame))

(defun bmkp-completing-read-buffer-name ()
  "Read the name of a buffer associated with a bookmark.
The candidates are the buffers in `bmkp-buffer-names'.
The default is the current buffer's name, or the value of
`bmkp-last-specific-buffer' if the current buffer has no bookmarks."
  (bookmark-maybe-load-default-file)
  (completing-read "Buffer: " (mapcar #'list (bmkp-buffer-names)) nil t nil 'buffer-name-history
                   (if (member (buffer-name) (bmkp-buffer-names))
                       (buffer-name)
                     bmkp-last-specific-buffer)))

(defun bmkp-completing-read-file-name ()
  "Read the name of a file associated with a bookmark.
The candidates are the absolute file names in `bmkp-file-names'.
The default is the current buffer's file name, if any, or the value of
`bmkp-last-specific-file' if the current buffer has no associated file
or the file has no bookmarks."
  (bookmark-maybe-load-default-file)
  (let ((completion-ignore-case  (if (boundp 'read-file-name-completion-ignore-case)
                                     read-file-name-completion-ignore-case
                                   (memq system-type '(ms-dos windows-nt darwin cygwin)))))
    (completing-read "File: " (mapcar #'list (bmkp-file-names)) nil t nil 'file-name-history
                     (let ((file  (buffer-file-name)))
                       (if (and file (member file (bmkp-file-names)))
                           file
                         bmkp-last-specific-file)))))

;;;###autoload
(defun bmkp-bmenu-show-only-w3m-urls () ; Bound to `W S' in bookmark list
  "Display (only) the w3m bookmarks."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-bmenu-filter-function  'bmkp-w3m-alist-only
        bmkp-bmenu-title            "W3M Bookmarks")
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order) "Only W3M bookmarks are shown")))

;;;###autoload
(defun bmkp-bmenu-show-all ()           ; Bound to `.' in bookmark list
  "Show all bookmarks known to the bookmark list (aka \"menu list\").
Omitted bookmarks are not shown, however.
Also, this does not revert the bookmark list, to bring it up to date.
To revert the list, use `\\<bookmark-bmenu-mode-map>\\[bmkp-bmenu-refresh-menu-list]'."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-bmenu-filter-function  nil
        bmkp-bmenu-title            "All Bookmarks"
        bmkp-latest-bookmark-alist  bookmark-alist)
  (bookmark-bmenu-list)
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order) "All bookmarks are shown")))

;;;###autoload
(defun bmkp-bmenu-refresh-menu-list ()  ; Bound to `g' in bookmark list
  "Refresh (revert) the bookmark list (\"menu list\").
This brings the displayed list up to date.  It does not change the
current filtering or sorting of the displayed list.

If you want setting a bookmark to refresh the list automatically, you
can use command `bmkp-toggle-bookmark-set-refreshes'."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (bmkp-refresh-menu-list (bookmark-bmenu-bookmark)))

(defun bmkp-refresh-menu-list (&optional bookmark)
  "Refresh (revert) the bookmark list (\"menu list\").
This brings the displayed list up to date.  It does not change the
current filtering or sorting of the displayed list.
Non-nil optional arg BOOKMARK means move cursor to BOOKMARK's line."
  (let ((bookmark-alist  (if bmkp-bmenu-filter-function
                             (funcall bmkp-bmenu-filter-function)
                           bookmark-alist)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp)
    (when bookmark
      (with-current-buffer (get-buffer "*Bookmark List*")
        (bmkp-bmenu-goto-bookmark-named bookmark)
        (let ((bmenu-win  (get-buffer-window (current-buffer) 0)))
          (when bmenu-win (set-window-point bmenu-win (point))))))))

;;;###autoload
(defun bmkp-bmenu-filter-bookmark-name-incrementally () ; Bound to `P B' in bookmark list
  "Incrementally filter bookmarks by bookmark name using a regexp."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (unwind-protect
       (progn (setq bmkp-bmenu-filter-timer
                    (run-with-timer 0 bmkp-incremental-filter-delay
                                    #'bmkp-bmenu-filter-alist-by-bookmark-name-regexp))
              (bmkp-bmenu-read-filter-input))
    (bmkp-bmenu-cancel-incremental-filtering)))

(defun bmkp-bmenu-filter-alist-by-bookmark-name-regexp ()
  "Filter bookmarks by bookmark name, then refresh the bookmark list."
  (setq bmkp-bmenu-filter-function  'bmkp-regexp-filtered-bookmark-name-alist-only
        bmkp-bmenu-title            (format "Bookmarks with Names Matching Regexp `%s'"
                                            bmkp-bmenu-filter-pattern))
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp)))

;;;###autoload
(defun bmkp-bmenu-filter-file-name-incrementally () ; Bound to `P F' in bookmark list
  "Incrementally filter bookmarks by file name using a regexp."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (unwind-protect
       (progn (setq bmkp-bmenu-filter-timer
                    (run-with-timer 0 bmkp-incremental-filter-delay
                                    #'bmkp-bmenu-filter-alist-by-file-name-regexp))
              (bmkp-bmenu-read-filter-input))
    (bmkp-bmenu-cancel-incremental-filtering)))

(defun bmkp-bmenu-filter-alist-by-file-name-regexp ()
  "Filter bookmarks by file name, then refresh the bookmark list."
  (setq bmkp-bmenu-filter-function  'bmkp-regexp-filtered-file-name-alist-only
        bmkp-bmenu-title            (format "Bookmarks with File Names Matching Regexp `%s'"
                                            bmkp-bmenu-filter-pattern))
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp)))

;;;###autoload
(defun bmkp-bmenu-filter-tags-incrementally () ; Bound to `P T' in bookmark list
  "Incrementally filter bookmarks by tags using a regexp."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (unwind-protect
       (progn (setq bmkp-bmenu-filter-timer
                    (run-with-timer 0 bmkp-incremental-filter-delay
                                    #'bmkp-bmenu-filter-alist-by-tags-regexp))
              (bmkp-bmenu-read-filter-input))
    (bmkp-bmenu-cancel-incremental-filtering)))

(defun bmkp-bmenu-filter-alist-by-tags-regexp ()
  "Filter bookmarks by tags, then refresh the bookmark list."
  (setq bmkp-bmenu-filter-function  'bmkp-regexp-filtered-tags-alist-only
        bmkp-bmenu-title            (format "Bookmarks with Tags Matching Regexp `%s'"
                                            bmkp-bmenu-filter-pattern))
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp)))

(defun bmkp-bmenu-read-filter-input ()
  "Read input and add it to `bmkp-bmenu-filter-pattern'."
  (setq bmkp-bmenu-filter-pattern  "")
  (let ((tmp-list  ())
        char)
    (catch 'bmkp-bmenu-read-filter-input-1
      (while t
        (catch 'bmkp-bmenu-read-filter-input-2
          (condition-case nil
              (setq char  (read-char (concat bmkp-bmenu-filter-prompt bmkp-bmenu-filter-pattern)))
            (error (throw 'bmkp-bmenu-read-filter-input-1 nil)))
          (case char
            ((?\e ?\r) (throw 'bmkp-bmenu-read-filter-input-1 nil)) ; Break and exit.
            (?\d
             (pop tmp-list)             ; Delete last char of `bmkp-bmenu-filter-pattern'.
             (setq bmkp-bmenu-filter-pattern  (mapconcat 'identity (reverse tmp-list) ""))
             (throw 'bmkp-bmenu-read-filter-input-2 nil))
            (t
             (push (text-char-description char) tmp-list)
             (setq bmkp-bmenu-filter-pattern  (mapconcat 'identity (reverse tmp-list) ""))
             (throw 'bmkp-bmenu-read-filter-input-2 nil))))))))

(defun bmkp-bmenu-cancel-incremental-filtering ()
  "Cancel timer used for incrementally filtering bookmarks."
  (cancel-timer bmkp-bmenu-filter-timer)
  (setq bmkp-bmenu-filter-timer  nil))

;;;###autoload
(defun bmkp-bmenu-toggle-show-only-unmarked () ; Bound to `<' in bookmark list
  "Hide all marked bookmarks.  Repeat to toggle, showing all."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (if (or (bmkp-some-marked-p bmkp-latest-bookmark-alist)
          (bmkp-some-marked-p bmkp-bmenu-before-hide-marked-alist))
      (let ((bookmark-alist  bmkp-latest-bookmark-alist)
            status)
        (if bmkp-bmenu-before-hide-marked-alist
            (setq bookmark-alist                       bmkp-bmenu-before-hide-marked-alist
                  bmkp-bmenu-before-hide-marked-alist  ()
                  bmkp-latest-bookmark-alist           bookmark-alist
                  status                               'shown)
          (setq bmkp-bmenu-before-hide-marked-alist  bmkp-latest-bookmark-alist
                bookmark-alist                       (bmkp-unmarked-bookmarks-only)
                bmkp-latest-bookmark-alist           bookmark-alist
                status                               'hidden))
        (bookmark-bmenu-surreptitiously-rebuild-list)
        (cond ((eq status 'hidden)
               (bookmark-bmenu-ensure-position)
               (message "Marked bookmarks are now hidden"))
              (t
               (goto-char (point-min))
               (when (re-search-forward "^>" (point-max) t)  (forward-line 0))
               (message "Marked bookmarks no longer hidden"))))
    (message "No marked bookmarks to hide"))
  (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window)))

;;;###autoload
(defun bmkp-bmenu-toggle-show-only-marked () ; Bound to `>' in bookmark list
  "Hide all unmarked bookmarks.  Repeat to toggle, showing all."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (if (or (bmkp-some-unmarked-p bmkp-latest-bookmark-alist)
          (bmkp-some-unmarked-p bmkp-bmenu-before-hide-unmarked-alist))
      (let ((bookmark-alist  bmkp-latest-bookmark-alist)
            status)
        (if bmkp-bmenu-before-hide-unmarked-alist
            (setq bookmark-alist                         bmkp-bmenu-before-hide-unmarked-alist
                  bmkp-bmenu-before-hide-unmarked-alist  ()
                  bmkp-latest-bookmark-alist             bookmark-alist
                  status                                 'shown)
          (setq bmkp-bmenu-before-hide-unmarked-alist  bmkp-latest-bookmark-alist
                bookmark-alist                         (bmkp-marked-bookmarks-only)
                bmkp-latest-bookmark-alist             bookmark-alist
                status                                 'hidden))
        (bookmark-bmenu-surreptitiously-rebuild-list)
        (cond ((eq status 'hidden)
               (bookmark-bmenu-ensure-position)
               (message "Unmarked bookmarks are now hidden"))
              (t
               (goto-char (point-min))
               (when (re-search-forward "^>" (point-max) t)  (forward-line 0))
               (message "Unmarked bookmarks no longer hidden"))))
    (message "No unmarked bookmarks to hide"))
  (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window)))


;;(@* "Menu-List (`*-bmenu-*') Commands Involving Marks")
;;  *** Menu-List (`*-bmenu-*') Commands Involving Marks ***

;;;###autoload
(defun bmkp-bmenu-mark-all ()           ; Bound to `M-m' in bookmark list
  "Mark all bookmarks, using mark `>'."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (save-excursion  
    (let ((count  0))
      (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
      (while (not (eobp)) (bookmark-bmenu-mark) (setq count  (1+ count)))
      (message "Marked: %d" count))))

;; This is similar to `dired-unmark-all-files'.
;;;###autoload
(defun bmkp-bmenu-unmark-all (mark &optional arg) ; Bound to `M-DEL', `U' in bookmark list
  "Remove a mark from each bookmark.
Hit the mark character (`>' or `D') to remove those marks,
 or hit `RET' to remove all marks (both `>' and `D').
With a prefix arg, you are queried to unmark each marked bookmark.
Use `\\[help-command]' during querying for help."
  (interactive "cRemove marks (RET means all): \nP")
  (bmkp-barf-if-not-in-menu-list)
  (require 'dired-aux)
  (save-excursion
    (let* ((count              0)
           (inhibit-read-only  t)
           (case-fold-search   nil)
           (query              nil)
           (string             (format "\n%c" mark))
           (help-form          "Type SPC or `y' to unmark one bookmark, DEL or `n' to skip to next,
`!' to unmark all remaining bookmarks with no more questions."))
      (goto-char (point-min))
      (forward-line (if (eq mark ?\r)
                        bmkp-bmenu-header-lines
                      (1- bmkp-bmenu-header-lines))) ; Because STRING starts with a newline.
      (while (and (not (eobp))
                  (if (eq mark ?\r)
                      (re-search-forward dired-re-mark nil t)
                    (let ((case-fold-search  t)) ; Treat `d' the same as `D'.
                      (search-forward string nil t))))
        (when (or (not arg)  (let ((bmk  (bookmark-bmenu-bookmark)))
                               (and bmk (dired-query 'query "Unmark bookmark `%s'? " bmk))))
          (bookmark-bmenu-unmark) (forward-line -1)
          (setq count  (1+ count))))
      (if (= 1 count) (message "1 mark removed") (message "%d marks removed" count)))))

;;;###autoload
(defun bmkp-bmenu-regexp-mark (regexp)  ; Bound to `% m' in bookmark list
  "Mark bookmarks that match REGEXP.
The entire bookmark line is tested: bookmark name and possibly file name."
  (interactive "sRegexp: ")
  (bmkp-barf-if-not-in-menu-list)
  (save-excursion
    (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
    (let ((count  0))
      (while (and (not (eobp)) (re-search-forward regexp (point-max) t))
        (bookmark-bmenu-mark)
        (setq count  (1+ count)))
      (if (= 1 count) (message "1 bookmark matched") (message "%d bookmarks matched" count)))))

;;;###autoload
(defun bmkp-bmenu-mark-bookmark-file-bookmarks () ; Bound to `X M' in bookmark list
  "Mark bookmark-file bookmarks."
  (interactive)
  (bmkp-bmenu-mark-bookmarks-satisfying 'bmkp-bookmark-file-bookmark-p))

;;;###autoload
(defun bmkp-bmenu-mark-desktop-bookmarks () ; Bound to `K M' in bookmark list
  "Mark desktop bookmarks."
  (interactive)
  (bmkp-bmenu-mark-bookmarks-satisfying 'bmkp-desktop-bookmark-p))

;;;###autoload
(defun bmkp-bmenu-mark-dired-bookmarks () ; Bound to `M-d M-m' in bookmark list
  "Mark Dired bookmarks."
  (interactive)
  (bmkp-bmenu-mark-bookmarks-satisfying 'bmkp-dired-bookmark-p))

;;;###autoload
(defun bmkp-bmenu-mark-file-bookmarks (arg) ; Bound to `F M' in bookmark list
  "Mark file bookmarks.
With a prefix argument, do not mark remote files or directories."
  (interactive "P")
  (bmkp-bmenu-mark-bookmarks-satisfying
   (if arg 'bmkp-local-file-bookmark-p 'bmkp-file-bookmark-p)))

;;;###autoload
(defun bmkp-bmenu-mark-gnus-bookmarks () ; Bound to `G M' in bookmark list
  "Mark Gnus bookmarks."
  (interactive)
  (bmkp-bmenu-mark-bookmarks-satisfying 'bmkp-gnus-bookmark-p))

;;;###autoload
(defun bmkp-bmenu-mark-info-bookmarks () ; Bound to `I M' in bookmark list
  "Mark Info bookmarks."
  (interactive)
  (bmkp-bmenu-mark-bookmarks-satisfying 'bmkp-info-bookmark-p))

;;;###autoload
(defun bmkp-bmenu-mark-man-bookmarks () ; Bound to `M M' in bookmark list
  "Mark `man' page bookmarks."
  (interactive)
  (bmkp-bmenu-mark-bookmarks-satisfying 'bmkp-man-bookmark-p))

;;;###autoload
(defun bmkp-bmenu-mark-non-file-bookmarks () ; Bound to `B M' in bookmark list
  "Mark non-file bookmarks."
  (interactive)
  (bmkp-bmenu-mark-bookmarks-satisfying 'bmkp-non-file-bookmark-p))

;;;###autoload
(defun bmkp-bmenu-mark-region-bookmarks () ; Bound to `R M' in bookmark list
  "Mark bookmarks that record a region."
  (interactive)
  (bmkp-bmenu-mark-bookmarks-satisfying 'bmkp-region-bookmark-p))

;;;###autoload
(when (featurep 'bookmark+-lit)
  (defun bmkp-bmenu-mark-lighted-bookmarks () ; Bound to `H M' in bookmark list
    "Mark the highlighted bookmarks."
    (interactive)
    (bmkp-bmenu-mark-bookmarks-satisfying 'bmkp-lighted-p)))

;;;###autoload
(defun bmkp-bmenu-mark-specific-buffer-bookmarks (&optional buffer) ; `= b M' in bookmark list
  "Mark bookmarks for BUFFER.
Interactively, read the name of the buffer.
If BUFFER is non-nil, set `bmkp-last-specific-buffer' to it."
  (interactive (list (bmkp-completing-read-buffer-name)))
  (when buffer (setq bmkp-last-specific-buffer  buffer))
  (bmkp-bmenu-mark-bookmarks-satisfying 'bmkp-last-specific-buffer-p))

;;;###autoload
(defun bmkp-bmenu-mark-specific-file-bookmarks (&optional file) ; Bound to `= f M' in bookmark list
  "Mark bookmarks for FILE, an absolute file name.
Interactively, read the file name.
If FILE is non-nil, set `bmkp-last-specific-file' to it."
  (interactive (list (bmkp-completing-read-file-name)))
  (when file (setq bmkp-last-specific-file  file))
  (bmkp-bmenu-mark-bookmarks-satisfying 'bmkp-last-specific-file-p))

;;;###autoload
(defun bmkp-bmenu-mark-w3m-bookmarks () ; Bound to `W M' in bookmark list
  "Mark W3M (URL) bookmarks."
  (interactive)
  (bmkp-bmenu-mark-bookmarks-satisfying 'bmkp-w3m-bookmark-p))

;;;###autoload
(defun bmkp-bmenu-mark-bookmarks-satisfying (pred) ; Not bound
  "Mark bookmarks that satisfy predicate PRED.
If you use this interactively, you are responsible for entering a
symbol that names a unnary predicate.  The function you provide is not
checked - it is simply applied to each bookmark to test it."
  (interactive "aPredicate: ")
  (bmkp-barf-if-not-in-menu-list)
  (save-excursion
    (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
    (let ((count  0)
          bmk)
      (while (not (eobp))
        (setq bmk  (bookmark-bmenu-bookmark))
        (if (not (funcall pred bmk))
            (forward-line 1)
          (bookmark-bmenu-mark)
          (setq count  (1+ count))))
      (if (= 1 count) (message "1 bookmark matched") (message "%d bookmarks matched" count)))))

;;;###autoload
(defun bmkp-bmenu-toggle-marks ()       ; Bound to `t' in bookmark list
  "Toggle marks: Unmark all marked bookmarks; mark all unmarked bookmarks.
This affects only the `>' mark, not the `D' flag."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (let ((marked-count    0)
        (unmarked-count  0))
    (save-excursion
      (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
      (if (not (bmkp-some-marked-p bmkp-latest-bookmark-alist))
          (bmkp-bmenu-mark-all)
        (while (not (eobp))
          (cond ((member (bookmark-bmenu-bookmark) bmkp-bmenu-marked-bookmarks)
                 (bookmark-bmenu-unmark)
                 (setq unmarked-count  (1+ unmarked-count)))
                (t
                 (bookmark-bmenu-mark)
                 (setq marked-count  (1+ marked-count)))))
        (message "Marked: %d, unmarked: %d" marked-count unmarked-count)))))

;;;###autoload
(defun bmkp-bmenu-dired-marked (dirbufname) ; Bound to `M-d >' in bookmark list
  "Dired in another window for the marked file and directory bookmarks.

Absolute file names are used for the entries in the Dired buffer.
The only entries are for the marked files and directories.  These can
be located anywhere.  (In Emacs versions prior to release 23.2, remote
bookmarks are ignored, because of Emacs bug #5478.)

You are prompted for the Dired buffer name.  The `default-directory'
of the buffer is the same as that of buffer `*Bookmark List*'."
  (interactive (list (read-string "Dired buffer name: ")))
  (bmkp-barf-if-not-in-menu-list)
  (let ((files  ())
        file)
    (dolist (bmk  (bmkp-sort-and-remove-dups (bmkp-marked-bookmarks-only)))
      (when (or (bmkp-local-file-bookmark-p bmk)
                (> emacs-major-version 23)
                (and (= emacs-major-version 23) (> emacs-minor-version 1)))
        (setq file  (bookmark-get-filename bmk))
        (unless (file-name-absolute-p file) (setq file (expand-file-name file))) ; Should not happen.
        (push file files)))
    (dired-other-window (cons dirbufname files))))

;;;###autoload
(defun bmkp-bmenu-delete-marked ()      ; Bound to `D' in bookmark list
  "Delete all (visible) bookmarks that are marked `>', after confirmation."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-execute-deletions 'marked))

;;;###autoload
(defun bmkp-bmenu-make-sequence-from-marked (bookmark-name &optional dont-omit-p) ; Not bound
  "Create or update a sequence bookmark from the visible marked bookmarks.
The bookmarks that are currently marked are recorded as a sequence, in
their current order in buffer `*Bookmark List*'.
When you \"jump\" to the sequence bookmark, the bookmarks in the
sequence are processed in order.

By default, omit the marked bookmarks, after creating the sequence.
With a prefix arg, do not omit them.

If a bookmark with the specified name already exists, it is
overwritten.  If a sequence bookmark with the name already exists,
then you are prompted whether to add the marked bookmarks to the
beginning of the existing sequence (or simply replace it).

Note that another existing sequence bookmark can be marked, and thus
included in the sequence bookmark created or updated.  That is, you
can include other sequences within a sequence bookmark.

Returns the bookmark (internal record) created or updated."
  (interactive "sName of sequence bookmark: \nP")
  (bmkp-barf-if-not-in-menu-list)
  (unless (get-buffer "*Bookmark List*") (bookmark-bmenu-list))
  (let ((marked-bmks  ())
        (count        0))
    (message "Making sequence from marked bookmarks...")
    (save-excursion
      (with-current-buffer "*Bookmark List*"
        (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
        (while (re-search-forward "^>" (point-max) t)
          (push (bookmark-bmenu-bookmark) marked-bmks)
          (setq count  (1+ count)))))
    (when (zerop count) (error "No marked bookmarks"))
    (let ((new-seq  (nreverse marked-bmks))
          (bmk      (bookmark-get-bookmark bookmark-name 'noerror)))
      (when (and bmk (bmkp-sequence-bookmark-p bmk))
        (if (y-or-n-p (format "ADD marked to existing sequence `%s' (otherwise, REPLACES it)? "
                              bookmark-name))
            (setq new-seq  (nconc new-seq (bookmark-prop-get bmk 'sequence)))
          "OK, existing sequence will be replaced"))
      (bookmark-store bookmark-name
                      `((filename . ,bmkp-non-file-filename)
                        (position . 0)
                        (sequence ,@new-seq)
                        (handler  . bmkp-jump-sequence))
                      nil)))
  (let ((new  (bookmark-get-bookmark bookmark-name 'noerror)))
    (unless (memq new bmkp-latest-bookmark-alist)
      (setq bmkp-latest-bookmark-alist  (cons new bmkp-latest-bookmark-alist)))
    (unless dont-omit-p
      (bmkp-bmenu-omit-marked)
      (message "Marked bookmarks now OMITTED - use `bmkp-bmenu-show-only-omitted' to show"))
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (bmkp-bmenu-goto-bookmark-named bookmark-name)
    new))


;;(@* "Omitted Bookmarks")
;;  *** Omitted Bookmarks ***

;;;###autoload
(defun bmkp-bmenu-omit ()               ; Not bound
  "Omit this bookmark."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (setq bmkp-bmenu-omitted-list  (cons (bookmark-bmenu-bookmark) bmkp-bmenu-omitted-list))
  (bookmark-bmenu-surreptitiously-rebuild-list)
  (message "Omitted 1 bookmark"))

;;;###autoload
(defun bmkp-bmenu-omit/unomit-marked () ; Bound to `O >' in bookmark list
  "Omit all marked bookmarks or, if showing only omitted ones, unomit."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (if (eq bmkp-bmenu-filter-function  'bmkp-omitted-alist-only)
      (bmkp-bmenu-unomit-marked)
    (bmkp-bmenu-omit-marked)))

;;;###autoload
(defun bmkp-bmenu-omit-marked ()        ; Bound to `O >' in bookmark list
  "Omit all marked bookmarks.
They will henceforth be invisible to the bookmark list.
You can, however, use \\<bookmark-bmenu-mode-map>`\\[bmkp-bmenu-show-only-omitted]' to see them.
You can then mark some of them and use `\\[bmkp-bmenu-omit/unomit-marked]' to make those marked
 available again for the bookmark list."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (let ((o-str    (and (not (looking-at "^>")) (bookmark-bmenu-bookmark)))
        (o-point  (point))
        (count    0))
    (message "Omitting marked bookmarks...")
    (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
    (while (re-search-forward "^>" (point-max) t)
      (setq bmkp-bmenu-omitted-list  (cons (bookmark-bmenu-bookmark) bmkp-bmenu-omitted-list)
            count                    (1+ count)))
    (if (<= count 0)
        (message "No marked bookmarks")
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (message "Omitted %d bookmarks" count))
    (if o-str
        (bmkp-bmenu-goto-bookmark-named o-str)
      (goto-char o-point)
      (beginning-of-line)))
  (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window)))

;;;###autoload
(defun bmkp-bmenu-unomit-marked ()      ; `O >' in bookmark list when showing omitted bookmarks
  "Remove all marked bookmarks from the list of omitted bookmarks.
They will henceforth be available for display in the bookmark list.
\(In order to see and then mark omitted bookmarks you must use \\<bookmark-bmenu-mode-map>\
`\\[bmkp-bmenu-show-only-omitted]'.)"
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (unless bmkp-bmenu-omitted-list (error "No omitted bookmarks to UN-omit"))
  (unless (eq bmkp-bmenu-filter-function  'bmkp-omitted-alist-only)
    (error "You must use command `bmkp-bmenu-show-only-omitted' first"))
  (let ((count    0))
    (message "UN-omitting marked bookmarks...")
    (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
    (while (re-search-forward "^>" (point-max) t)
      (let ((bmk-name  (bookmark-bmenu-bookmark)))
        (when (member bmk-name bmkp-bmenu-omitted-list)
          (setq bmkp-bmenu-omitted-list  (delete bmk-name bmkp-bmenu-omitted-list)
                count                    (1+ count)))))
    (if (<= count 0)
        (message "No marked bookmarks")
      (setq bmkp-bmenu-filter-function  nil
            bmkp-bmenu-title            "All Bookmarks"
            bmkp-latest-bookmark-alist  bookmark-alist)
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (message "UN-omitted %d bookmarks" count)))
  (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window)))

;;;###autoload
(defun bmkp-unomit-all ()               ; Bound to `O U' in bookmark list
  "Remove all bookmarks from the list of omitted bookmarks.
All bookmarks will henceforth be available for display."
  (interactive)
  (unless bmkp-bmenu-omitted-list (error "No omitted bookmarks to UN-omit"))
  (message "UN-omitting ALL omitted bookmarks...")
  (let ((count  0))
    (dolist (bmk-name  bmkp-bmenu-omitted-list)
      (setq bmkp-bmenu-omitted-list  (delete bmk-name bmkp-bmenu-omitted-list)
            count                    (1+ count)))
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (message "UN-omitted %d bookmarks" count))
  (when (equal (buffer-name (current-buffer)) "*Bookmark List*") (bmkp-bmenu-show-all))
  (when (and (fboundp 'fit-frame-if-one-window)
             (equal (buffer-name (current-buffer)) "*Bookmark List*"))
    (fit-frame-if-one-window)))

;;;###autoload
(defun bmkp-bmenu-show-only-omitted ()  ; Bound to `O S' in bookmark list to show only omitted
  "Show only the omitted bookmarks.
You can then mark some of them and use `bmkp-bmenu-unomit-marked' to
 make those that are marked available again for the bookmark list."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (unless bmkp-bmenu-omitted-list (error "No omitted bookmarks"))
  (setq bmkp-bmenu-filter-function  'bmkp-omitted-alist-only
        bmkp-bmenu-title            "Omitted Bookmarks")
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order) "Only omitted bookmarks are shown now")))

(defun bmkp-omitted-alist-only ()
  "`bookmark-alist', filtered to retain only the omitted bookmarks.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-omitted-bookmark-p bookmark-alist))

(defun bmkp-omitted-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is an omitted bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (unless (stringp bookmark) (setq bookmark  (car bookmark)))
  (member bookmark bmkp-bmenu-omitted-list))


;;(@* "Search-and-Replace Locations of Marked Bookmarks")
;;  *** Search-and-Replace Locations of Marked Bookmarks ***

;;;###autoload
(when (> emacs-major-version 22)

  (defvar bmkp-isearch-bookmarks nil
    "List of bookmarks whose locations are to be incrementally searched.")

  (defun bmkp-isearch-next-bookmark-buffer (&optional bookmark wrap)
    "Return the next buffer in a series of bookmark buffers.
Used as a value for `multi-isearch-next-buffer-function', for Isearch
of multiple bookmarks.

Variable `bmkp-isearch-bookmarks' is a list of bookmark names.
Each bookmark in that list is visited by `bookmark--jump-via', and the
corresponding bookmark buffer is returned."
    (let ((bookmarks  (if isearch-forward bmkp-isearch-bookmarks (reverse bmkp-isearch-bookmarks))))
      (bookmark--jump-via
       (if wrap
           (car bookmarks)
         (let ((this-bmk  (catch 'bmkp-isearch-next-bookmark-buffer
                            (dolist (bmk  bookmarks)
                              (when (if (bmkp-get-buffer-name bmk)
                                        (equal (bmkp-get-buffer-name bmk) (buffer-name))
                                      (equal (bookmark-get-filename bmk) (buffer-file-name)))
                                (throw 'bmkp-isearch-next-bookmark-buffer bmk)))
                            (car bookmarks))))
           (cadr (member this-bmk bookmarks))))
       'ignore)
      (current-buffer)))

  (defun bmkp-isearch-bookmarks (bookmarks)
    "Start multi-bookmark Isearch on BOOKMARKS."
    (let ((multi-isearch-next-buffer-function  'bmkp-isearch-next-bookmark-buffer)
          (bmkp-isearch-bookmarks              bookmarks))
      (bookmark-jump (car bookmarks))
      (goto-char (if isearch-forward (point-min) (point-max)))
      (isearch-forward)))

  (defun bmkp-isearch-bookmarks-regexp (bookmarks)
    "Start multi-bookmark regexp Isearch on BOOKMARKS."
    (let ((multi-isearch-next-buffer-function  'bmkp-isearch-next-bookmark-buffer)
          (bmkp-isearch-bookmarks              bookmarks))
      (bookmark-jump (car bookmarks))
      (goto-char (if isearch-forward (point-min) (point-max)))
      (isearch-forward-regexp)))

  (defun bmkp-bmenu-isearch-marked-bookmarks () ; Bound to `M-s a C-s' in bookmark list
    "Isearch the marked bookmark locations, in their current order."
    (interactive)
    (bmkp-barf-if-not-in-menu-list)
    (let ((bookmarks             (mapcar #'car (bmkp-sort-and-remove-dups
                                                (bmkp-marked-bookmarks-only))))
          (bmkp-use-region  nil))  ; Suppress region handling.
      (bmkp-isearch-bookmarks bookmarks)))

  (defun bmkp-bmenu-isearch-marked-bookmarks-regexp () ; Bound to `M-s a M-C-s' in bookmark list
    "Regexp Isearch the marked bookmark locations, in their current order."
    (interactive)
    (bmkp-barf-if-not-in-menu-list)
    (let ((bookmarks             (mapcar #'car (bmkp-sort-and-remove-dups
                                                (bmkp-marked-bookmarks-only))))
          (bmkp-use-region  nil))  ; Suppress region handling.
      (bmkp-isearch-bookmarks-regexp bookmarks))))

(defun bmkp-bmenu-search-marked-bookmarks-regexp (regexp) ; Bound to `M-a' in bookmark list
  "Search the marked file bookmarks, in their current order, for REGEXP.
Use `\\[tags-loop-continue]' to advance among the search hits.
Marked directory and non-file bookmarks are ignored."
  (interactive "sSearch marked file bookmarks (regexp): ")
  (bmkp-barf-if-not-in-menu-list)
  (tags-search regexp '(let ((files  ())
                             file)
                        (dolist (bmk  (bmkp-sort-and-remove-dups (bmkp-marked-bookmarks-only)))
                          (setq file  (bookmark-get-filename bmk))
                          (when (and (not (equal bmkp-non-file-filename file))
                                     (not (file-directory-p file)))
                            (push file files)))
                        (setq files  (nreverse files)))))

(defun bmkp-bmenu-query-replace-marked-bookmarks-regexp (from to ; Bound to `M-q' in bookmark list
                                                         &optional delimited)
  "`query-replace-regexp' FROM with TO, for all marked file bookmarks.
DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (`\\[keyboard-quit]', `RET' or `q'), you can use \
`\\[tags-loop-continue]' to resume where
you left off."
  (interactive (let ((common  (query-replace-read-args "Query replace regexp in marked files" t t)))
                 (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (bmkp-barf-if-not-in-menu-list)
  (tags-query-replace from to delimited
		      '(let ((files  ())
                             file)
                        (dolist (bmk  (bmkp-sort-and-remove-dups (bmkp-marked-bookmarks-only)))
                          (setq file  (bookmark-get-filename bmk))
                          (let ((buffer  (get-file-buffer file)))
                            (when (and buffer  (with-current-buffer buffer buffer-read-only))
                              (error "File `%s' is visited read-only" file)))
                          (when (and (not (equal bmkp-non-file-filename file))
                                     (not (file-directory-p file)))
                            (push file files)))
                        (setq files  (nreverse files)))))


;;(@* "Tags")
;;  *** Tags ***

(defun bmkp-get-tags (bookmark)
  "Return the `tags' entry for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (bookmark-prop-get bookmark 'tags))

(defun bmkp-get-tag-value (bookmark tag &optional msgp)
  "Return value of BOOKMARK's tag TAG.
BOOKMARK is a bookmark name or a bookmark record.
TAG is a string.
Return nil if BOOKMARK has no such TAG or if TAG has no value."
  (assoc-default tag (bmkp-get-tags bookmark)))

(defun bmkp-has-tag-p (bookmark tag &optional msgp)
  "Return non-nil if BOOKMARK is tagged with TAG.
BOOKMARK is a bookmark name or a bookmark record.
TAG is a string."
  (assoc-default tag (bmkp-get-tags bookmark) nil t))

;; NOT USED currently - we use `bmkp-read-tags-completing' instead.
(defun bmkp-read-tags ()
  "Read tags one by one, and return them as a list."
  (let ((tag    (read-string "Tag (RET for each, empty input to finish): "))
        (btags  ()))
    (while (not (string= "" tag))
      (push tag btags)
      (setq tag  (read-string "Tag: ")))
    btags))

(defun bmkp-read-tag-completing (&optional prompt candidate-tags require-match update-tags-alist-p)
  "Read a tag with completion, and return it as a string.
PROMPT is the prompt string.  If nil, then use \"Tag: \".
CANDIDATE-TAGS is an alist of tags to use for completion.
 If nil, then all tags from all bookmarks are used for completion.
 The set of all tags is taken from variable `bmkp-tags-alist'.
REQUIRE-MATCH is passed to `completing-read'.
Non-nil UPDATE-TAGS-ALIST-P means update var `bmkp-tags-alist'."
  (bookmark-maybe-load-default-file)
  (let ((cand-tags  (copy-sequence
                     (or candidate-tags
                         (and (not update-tags-alist-p) bmkp-tags-alist) ; Use cached list.
                         (bmkp-tags-list))))) ; Update the cache.
    (completing-read (or prompt "Tag: ") cand-tags nil require-match nil 'bmkp-tag-history)))

(defun bmkp-read-tags-completing (&optional candidate-tags require-match update-tags-alist-p)
  "Read tags with completion, and return them as a list of strings.
Reads tags one by one, until you hit `RET' twice consecutively.
CANDIDATE-TAGS is an alist of tags to use for completion.
 If nil, then all tags from all bookmarks are used for completion.
 The set of all tags is taken from variable `bmkp-tags-alist'.
REQUIRE-MATCH is passed to `completing-read'.
Non-nil UPDATE-TAGS-ALIST-P means update var `bmkp-tags-alist'."
  (bookmark-maybe-load-default-file)
  (let ((cands    ())
        (btags    ())
        (prompt1  "Tag (RET for each, empty input to finish): ")
        (prompt2  "Tag: ")
        tag old-tag)
    ;; Make a new candidates alist, with just one entry per tag name.  The original cdr is discarded.
    (dolist (full-tag  (or candidate-tags
                           (and (not update-tags-alist-p) bmkp-tags-alist) ; Use cached list.
                           (bmkp-tags-list)))
      (add-to-list 'cands (list (if (consp full-tag) (car full-tag) full-tag))))
    (setq tag    (completing-read prompt1 cands nil require-match nil 'bmkp-tag-history)
          cands  (delete (assoc tag cands) cands)) ; Tag read is no longer a candidate.
    (while (not (string= "" tag))
      (if (member tag btags)            ; User can enter it more than once, if not REQUIRE-MATCH.
          (message "Tag `%s' already included" tag)
        (push tag btags))               ; But we only add it once.
      (setq tag    (completing-read prompt2 cands nil require-match nil 'bmkp-tag-history)
            cands  (delete (assoc tag cands) cands)))
    (nreverse btags)))

;;;###autoload
(defun bmkp-bmenu-show-only-tagged ()   ; Bound to `T S' in bookmark list
  "Display (only) the bookmarks that have tags."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-bmenu-filter-function  'bmkp-some-tags-alist-only
        bmkp-bmenu-title            "Tagged Bookmarks")
  (let ((bookmark-alist  (funcall bmkp-bmenu-filter-function)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bmkp-msg-about-sort-order (bmkp-current-sort-order) "Only tagged bookmarks are shown")))

;;;###autoload
(defun bmkp-list-all-tags (fullp)       ; Bound to `T l' in bookmark list
  "List all tags used for any bookmarks.
With a prefix argument, list the full alist of tags.
Otherwise, list only the tag names.

Note that when the full alist is shown, the same tag name will appear
once for each of its different values.

Show list in minibuffer or, if not enough space, buffer `*All Tags*'."
  (interactive "P")
  (require 'pp)
  (pp-display-expression (bmkp-tags-list (not fullp)) "*All Tags"))
  
(defun bmkp-tags-list (&optional names-only-p)
  "Current list of all tags, from all bookmarks.
Non-nil NAMES-ONLY-P means return a list of only the tag names.
Otherwise, return an alist of the full tags and set variable
`bmkp-tags-alist' to that alist, as a cache."
  (bookmark-maybe-load-default-file)
  (let ((tags  ())
        bmk-tags)
    (dolist (bmk  bookmark-alist)
      (setq bmk-tags  (bmkp-get-tags bmk))
      (dolist (tag  bmk-tags)
        (add-to-list 'tags (if names-only-p (bmkp-tag-name tag) (bmkp-full-tag tag)))))
    (unless names-only-p (setq bmkp-tags-alist  tags))
    tags))

(defun bmkp-tag-name (tag)
  "Name of TAG.  If TAG is an atom, then TAG, else (car TAG)."
  (if (atom tag) tag (car tag)))

(defun bmkp-full-tag (tag)
  "Full cons entry for TAG.  If TAG is a cons, then TAG, else (list TAG)."
  (if (consp tag) tag (list tag)))

;;;###autoload
(defun bmkp-remove-all-tags (bookmark &optional msgp) ; Bound to `T 0' in bookmark list
  "Remove all tags from BOOKMARK.
Non-nil optional arg MSGP means display a message about the removal."
  (interactive (list (bookmark-completing-read "Bookmark" (bmkp-default-bookmark-name)) 'msg))
  (when (and msgp (null (bmkp-get-tags bookmark))) (error "Bookmark has no tags to remove"))
  (let ((nb-removed  (and (interactive-p) (length (bmkp-get-tags bookmark)))))
    (bookmark-prop-set bookmark 'tags ())
    (bmkp-maybe-save-bookmarks)
    (when (and msgp nb-removed) (message "%d tags removed" nb-removed)))
  (when (get-buffer-window (get-buffer-create "*Bookmark List*") 0)
    (bmkp-refresh-menu-list bookmark))) ; So the `t' markers are removed.

;;;###autoload
(defun bmkp-bmenu-remove-all-tags (&optional must-confirm-p) ; Not bound
  "Remove all tags from this bookmark.
Interactively, you are required to confirm."
  (interactive "p")
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (let ((bookmark  (bookmark-bmenu-bookmark)))
    (when (and must-confirm-p (null (bmkp-get-tags bookmark)))
      (error "Bookmark has no tags to remove"))
    (when (or (not must-confirm-p) (y-or-n-p "Remove all tags from this bookmark? "))
      (bmkp-remove-all-tags bookmark))))

;;;###autoload
(defun bmkp-add-tags (bookmark tags &optional msgp no-cache-update-p) ; `T +' in bookmark list
  "Add TAGS to BOOKMARK.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter the bookmark name and each tag.
Completion for the bookmark name is strict.
Completion for tags is lax: you are not limited to existing tags.

TAGS is a list of strings.
Non-nil MSGP means display a message about the addition.
Non-nil NO-CACHE-UPDATE-P means do not update `bmkp-tags-alist'.
Return the number of tags added."
  (interactive (list (bookmark-completing-read "Bookmark" (bmkp-default-bookmark-name))
                     (bmkp-read-tags-completing)
                     'msg))
  (let* ((newtags  (copy-alist (bmkp-get-tags bookmark)))
         (olen     (length newtags)))
    (dolist (tag  tags)  (unless (or (assoc tag newtags) (member tag newtags))  (push tag newtags)))
    (bookmark-prop-set bookmark 'tags newtags)
    (unless no-cache-update-p (bmkp-tags-list)) ; Update the tags cache.
    (bmkp-maybe-save-bookmarks)
    (when (get-buffer-window (get-buffer-create "*Bookmark List*") 0)
      (bmkp-refresh-menu-list bookmark)) ; So the `t' markers are displayed (updated).
    (let ((nb-added  (- (length newtags) olen)))
      (when msgp (message "%d tags added. Now: %S" nb-added ; Echo just the tag names.
                          (let ((ts  (mapcar #'bmkp-tag-name newtags)))
                            (setq ts (sort ts #'string<)))))
      nb-added)))

;;;###autoload
(defun bmkp-bmenu-add-tags ()           ; Only on `mouse-3' menu in bookmark list.
  "Add some tags to this bookmark."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (bmkp-add-tags (bookmark-bmenu-bookmark) (bmkp-read-tags-completing)))

;; $$$$$$ NOT YET USED
;;;###autoload
(defun bmkp-set-tag-value-for-navlist (tag value) ; Not bound
  "Set the value of TAG to VALUE, for each bookmark in the navlist.
If any of the bookmarks has no tag named TAG, then add one with VALUE."
  (interactive (list (bmkp-read-tag-completing) (read (read-string "Value: ")) 'msg))
  (bmkp-set-tag-value-for-bookmarks bmkp-nav-alist tag value))

;; $$$$$$ NOT YET USED
(defun bmkp-set-tag-value-for-bookmarks (bookmarks tag value) ; Not bound
  "Set the value of TAG to VALUE, for each of the BOOKMARKS.
If any of the BOOKMARKS has no tag named TAG, then add one with VALUE."
  (dolist (bmk  bookmarks) (bmkp-set-tag-value bmk tag value)))

;;;###autoload
(defun bmkp-set-tag-value (bookmark tag value &optional msgp) ; Not bound
  "For BOOKMARK's TAG, set the value to VALUE.
If BOOKMARK has no tag named TAG, then add one with value VALUE."
  (interactive
   (let* ((bmk  (bookmark-completing-read "Bookmark" (bmkp-default-bookmark-name)))
          (tag  (bmkp-read-tag-completing "Tag: " (mapcar 'bmkp-full-tag (bmkp-get-tags bmk)))))
     (list bmk tag (read (read-string "Value: ")) 'msg)))
  (unless (bmkp-has-tag-p bookmark tag) (bmkp-add-tags bookmark (list tag)))
  (let* ((newtags     (copy-alist (bmkp-get-tags bookmark)))
         (assoc-tag   (assoc tag newtags))
         (member-tag  (and (not assoc-tag) (member tag newtags))))
    (if assoc-tag (setcdr assoc-tag value) (setcar member-tag (cons (car member-tag) value)))
    (bookmark-prop-set bookmark 'tags newtags))
  (when msgp "Tag value added"))

;;;###autoload
(defun bmkp-bmenu-set-tag-value ()      ; Bound to `T v' in bookmark list
  "Set the value of one of this bookmark's tags."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (let ((this-bmk  (bookmark-bmenu-bookmark)))
    (bmkp-set-tag-value
     this-bmk
     (bmkp-read-tag-completing "Tag: " (mapcar 'bmkp-full-tag (bmkp-get-tags this-bmk)))
     (read (read-string "Value: "))
     'msg)))

;; $$$$$$ NOT YET USED
;;;###autoload
(defun bmkp-bmenu-set-tag-value-for-marked (tag value &optional msgp) ; Not bound
  "Set the value of TAG to VALUE, for each of the marked bookmarks.
If any of the bookmarks has no tag named TAG, then add one with VALUE."
  (interactive (list (bmkp-read-tag-completing) (read (read-string "Value: ")) 'msg))
  (bmkp-barf-if-not-in-menu-list)
  (when msgp (message "Setting tag values..."))
  (let ((marked  (bmkp-marked-bookmarks-only)))
    (unless marked (error "No marked bookmarks"))
    (when msgp (message "Setting tag values..."))
    (bmkp-set-tag-value-for-bookmarks marked tag value))
  (when (and msgp tag) (message "Setting tag values...done")))

;;;###autoload
(defun bmkp-remove-tags (bookmark tags &optional msgp no-cache-update-p) ; `T -' in bookmark list
  "Remove TAGS from BOOKMARK.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter the bookmark name and each tag.

TAGS is a list of strings.  The corresponding tags are removed.
Non-nil MSGP means display messages.
Non-nil NO-CACHE-UPDATE-P means do not update `bmkp-tags-alist'."
  (interactive (let ((bmk  (bookmark-completing-read "Bookmark" (bmkp-default-bookmark-name))))
                 (list bmk
                       (bmkp-read-tags-completing (mapcar 'bmkp-full-tag (bmkp-get-tags bmk)) t)
                       'msg)))
  (let* ((newtags  (copy-alist (bmkp-get-tags bookmark)))
         (olen     (length newtags)))
    (if (null newtags)
        (when msgp (message "Bookmark has no tags to remove")) ; Do nothing if bookmark has no tags.
      (setq newtags  (bmkp-remove-if #'(lambda (tag)
                                         (if (atom tag) (member tag tags) (member (car tag) tags)))
                                     newtags))
      (bookmark-prop-set bookmark 'tags newtags)
      (unless no-cache-update-p (bmkp-tags-list)) ; Update the tags cache.
      (bmkp-maybe-save-bookmarks)
      (when (get-buffer-window (get-buffer-create "*Bookmark List*") 0)
        (bmkp-refresh-menu-list bookmark)) ; So the `t' markers are removed.
      (let ((nb-removed  (- olen (length newtags))))
        (when msgp (message "%d tags removed. Now: %S" nb-removed ; Echo just the tag names.
                            (let ((ts  (mapcar #'bmkp-tag-name newtags)))
                              (setq ts (sort ts #'string<)))))
        nb-removed))))

;;;###autoload
(defun bmkp-bmenu-remove-tags (&optional msgp) ; Only on `mouse-3' menu in bookmark list.
  "Remove some tags from this bookmark."
  (interactive "p")
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (let ((bmk  (bookmark-bmenu-bookmark)))
    (bmkp-remove-tags bmk
                      (bmkp-read-tags-completing (mapcar 'bmkp-full-tag (bmkp-get-tags bmk)) t)
                      msgp)))

;;;###autoload
(defun bmkp-remove-tags-from-all (tags &optional msgp) ; Bound to `T d' in bookmark list
  "Remove TAGS from all bookmarks.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag.
This affects all bookmarks, even those not showing in bookmark list.

TAGS is a list of strings.  The corresponding tags are removed.
Non-nil optional arg MSGP means display a message about the deletion."
  (interactive
   (if (not (y-or-n-p
             "Delete the tags you specify from ALL bookmarks, even those not shown? "))
       (error "Deletion cancelled")
     (list (bmkp-read-tags-completing nil t)  'MSG)))
  (dolist (bmk  (bookmark-all-names)) (bmkp-remove-tags bmk tags nil 'NO-CACHE-UPDATE))
  (bmkp-tags-list)                      ; Update the tags cache (only once, at end).
  (when msgp (message "Tags removed from all bookmarks: %S" tags)))

;;;###autoload
(defun bmkp-rename-tag (tag newname &optional msgp) ; Bound to `T r' in bookmark list
  "Rename TAG to NEWNAME in all bookmarks, even those not showing.
Non-nil optional arg MSGP means display a message about the deletion."
  (interactive (list (bmkp-read-tag-completing "Tag (old name): ")
                     (bmkp-read-tag-completing "New name: ")
                     'MSG))
  (let ((tag-exists-p  nil))
    (dolist (bmk  (bookmark-all-names))
      (let ((newtags  (copy-alist (bmkp-get-tags bmk))))
        (when newtags
          (let* ((assoc-tag   (assoc tag newtags))
                 (member-tag  (and (not assoc-tag) (member tag newtags))))
            (cond (assoc-tag  (setcar assoc-tag newname))
                  (member-tag (setcar member-tag newname)))
            (when (or assoc-tag member-tag)
              (setq tag-exists-p  t)
              (bookmark-prop-set bmk 'tags newtags))))))
    (if tag-exists-p
        (bmkp-tags-list)                ; Update the tags cache.
      (error "No such tag: `%s'" tag))
    (when msgp (message "Renamed"))))

;;;###autoload
(defun bmkp-bmenu-add-tags-to-marked (tags &optional msgp) ; Bound to `T > +' in bookmark list
  "Add TAGS to each of the marked bookmarks.
TAGS is a list of strings.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag, but you are not limited to
choosing existing tags."
  (interactive (list (bmkp-read-tags-completing) 'msg))
  (bmkp-barf-if-not-in-menu-list)
  (when msgp (message "Adding tags..."))
  (let ((marked  (bmkp-marked-bookmarks-only)))
    (unless marked (error "No marked bookmarks"))
    (when msgp (message "Adding tags..."))
    (dolist (bmk  (mapcar #'car marked)) (bmkp-add-tags bmk tags nil 'NO-CACHE-UPDATE)))
  (bmkp-tags-list)                      ; Update the tags cache (only once, at end).
  (when (and msgp tags) (message "Tags added: %S" tags)))

;;;###autoload
(defun bmkp-bmenu-remove-tags-from-marked (tags &optional msgp) ; Bound to `T > -' in bookmark list
  "Remove TAGS from each of the marked bookmarks.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag."
  (interactive (let ((cand-tags  ()))
                 (dolist (bmk  (bmkp-marked-bookmarks-only))
                   (setq cand-tags  (bmkp-set-union cand-tags (bmkp-get-tags bmk))))
                 (unless cand-tags (error "No tags to remove"))
                 (list (bmkp-read-tags-completing cand-tags t) 'msg)))
  (bmkp-barf-if-not-in-menu-list)
  (let ((marked  (bmkp-marked-bookmarks-only)))
    (unless marked (error "No marked bookmarks"))
    (when msgp (message "Removing tags..."))
    (dolist (bmk  (mapcar #'car marked)) (bmkp-remove-tags bmk tags nil 'NO-CACHE-UPDATE)))
  (bmkp-tags-list)                      ; Update the tags cache (only once, at end).
  (when (and msgp tags) (message "Tags removed: %S" tags)))

;;;###autoload
(defun bmkp-bmenu-mark-bookmarks-tagged-regexp (regexp &optional notp) ; `T m %' in bookmark list
  "Mark bookmarks any of whose tags match REGEXP.
With a prefix arg, mark all that are tagged but with no tags that match."
  (interactive "sRegexp: \nP")
  (bmkp-barf-if-not-in-menu-list)
  (save-excursion
    (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
    (let ((count  0)
          tags anyp)
      (while (not (eobp))
        (setq tags  (bmkp-get-tags (bookmark-bmenu-bookmark))
              anyp  (and tags (bmkp-some (lambda (tag) (string-match regexp (bmkp-tag-name tag)))
                                         tags)))
        (if (not (and tags (if notp (not anyp) anyp)))
            (forward-line 1)
          (bookmark-bmenu-mark)
          (setq count  (1+ count))))
      (if (= 1 count) (message "1 bookmark matched") (message "%d bookmarks matched" count)))))

;;;###autoload
(defun bmkp-bmenu-mark-bookmarks-tagged-all (tags &optional nonep msgp) ; `T m *' in bookmark list
  "Mark all visible bookmarks that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
any tags at all (i.e., at least one tag).

With a prefix arg, mark all that are *not* tagged with *any* TAGS."
  (interactive (list (bmkp-read-tags-completing) current-prefix-arg 'msg))
  (bmkp-barf-if-not-in-menu-list)
  (bmkp-bmenu-mark/unmark-bookmarks-tagged-all/none tags nonep nil msgp))

;;;###autoload
(defun bmkp-bmenu-mark-bookmarks-tagged-none (tags &optional allp msgp) ; `T m ~ +' in bookmark list
  "Mark all visible bookmarks that are not tagged with *any* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
no tags at all.

With a prefix arg, mark all that are tagged with *each* tag in TAGS."
  (interactive (list (bmkp-read-tags-completing) current-prefix-arg 'msg))
  (bmkp-barf-if-not-in-menu-list)
  (bmkp-bmenu-mark/unmark-bookmarks-tagged-all/none tags (not allp) nil msgp))

;;;###autoload
(defun bmkp-bmenu-mark-bookmarks-tagged-some (tags &optional somenotp msgp) ; `T m +' in bookmark list
  "Mark all visible bookmarks that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
any tags at all.

With a prefix arg, mark all that are *not* tagged with *all* TAGS.

Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag."
  (interactive (list (bmkp-read-tags-completing) current-prefix-arg 'msg))
  (bmkp-barf-if-not-in-menu-list)
  (bmkp-bmenu-mark/unmark-bookmarks-tagged-some/not-all tags somenotp nil msgp))

;;;###autoload
(defun bmkp-bmenu-mark-bookmarks-tagged-not-all (tags &optional somep msgp) ; `T m ~ *' in bmk list
  "Mark all visible bookmarks that are *not* tagged with *all* TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
no tags at all.

With a prefix arg, mark all that are tagged with *some* tag in TAGS."
  (interactive (list (bmkp-read-tags-completing) current-prefix-arg 'msg))
  (bmkp-barf-if-not-in-menu-list)
  (bmkp-bmenu-mark/unmark-bookmarks-tagged-some/not-all tags (not somep) nil msgp))

;;;###autoload
(defun bmkp-bmenu-unmark-bookmarks-tagged-all (tags &optional nonep msgp) ; `T u *' in bookmark list
  "Unmark all visible bookmarks that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
any tags at all.

With a prefix arg, unmark all that are *not* tagged with *any* TAGS."
  (interactive (list (bmkp-read-tags-completing) current-prefix-arg 'msg))
  (bmkp-barf-if-not-in-menu-list)
  (bmkp-bmenu-mark/unmark-bookmarks-tagged-all/none tags nonep 'unmark msgp))

;;;###autoload
(defun bmkp-bmenu-unmark-bookmarks-tagged-none (tags &optional allp msgp) ; `T u ~ +' in bookmark list
  "Unmark all visible bookmarks that are *not* tagged with *any* TAGS.
With a prefix arg, unmark all that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
no tags at all."
  (interactive (list (bmkp-read-tags-completing) current-prefix-arg 'msg))
  (bmkp-barf-if-not-in-menu-list)
  (bmkp-bmenu-mark/unmark-bookmarks-tagged-all/none tags (not allp) 'unmark msgp))

;;;###autoload
(defun bmkp-bmenu-unmark-bookmarks-tagged-some (tags &optional somenotp msgp) ; `T u +' in bmk list
  "Unmark all visible bookmarks that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then unmark the bookmarks that have
any tags at all.

With a prefix arg, unmark all that are *not* tagged with *all* TAGS."
  (interactive (list (bmkp-read-tags-completing) current-prefix-arg 'msg))
  (bmkp-barf-if-not-in-menu-list)
  (bmkp-bmenu-mark/unmark-bookmarks-tagged-some/not-all tags somenotp 'unmark msgp))

;;;###autoload
(defun bmkp-bmenu-unmark-bookmarks-tagged-not-all (tags &optional somep msgp) ; `T u ~ *' in bmk list
  "Unmark all visible bookmarks that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
no tags at all.
With a prefix arg, unmark all that are *not* tagged with *all* TAGS."
  (interactive (list (bmkp-read-tags-completing) current-prefix-arg 'msg))
  (bmkp-barf-if-not-in-menu-list)
  (bmkp-bmenu-mark/unmark-bookmarks-tagged-some/not-all tags (not somep) 'unmark msgp))

(defun bmkp-bmenu-mark/unmark-bookmarks-tagged-all/none (tags &optional nonep unmarkp msgp)
  "Mark or unmark visible bookmarks tagged with all or none of TAGS.
TAGS is a list of strings, the tag names.
NONEP non-nil means mark/unmark bookmarks that have none of the TAGS.
UNMARKP non-nil means unmark; nil means mark.
MSGP means display a status message.

As a special case, if TAGS is empty, then mark or unmark the bookmarks
that have any tags at all, or if NONEP is non-nil then mark or unmark
those that have no tags at all."
  (with-current-buffer "*Bookmark List*"
    (save-excursion  
      (let ((count  0)
            bmktags presentp)
        (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
        (while (not (eobp))
          (setq bmktags  (bmkp-get-tags (bookmark-bmenu-bookmark)))
          (if (not (if (null tags)
                       (if nonep (not bmktags) bmktags)
                     (and bmktags  (catch 'bmkp-b-mu-b-t-an
                                     (dolist (tag  tags)
                                       (setq presentp  (assoc-default tag bmktags nil t))
                                       (unless (if nonep (not presentp) presentp)
                                         (throw 'bmkp-b-mu-b-t-an nil)))
                                     t))))
              (forward-line 1)
            (if unmarkp (bookmark-bmenu-unmark) (bookmark-bmenu-mark))
            (setq count  (1+ count))))
        (when msgp (if (= 1 count)
                       (message "1 bookmark matched")
                     (message "%d bookmarks matched" count)))))))

(defun bmkp-bmenu-mark/unmark-bookmarks-tagged-some/not-all (tags &optional notallp unmarkp msgp)
  "Mark or unmark visible bookmarks tagged with any or not all of TAGS.
TAGS is a list of strings, the tag names.
NOTALLP non-nil means mark/unmark bookmarks that do not have all TAGS.
UNMARKP non-nil means unmark; nil means mark.
MSGP means display a status message.

As a special case, if TAGS is empty, then mark or unmark the bookmarks
that have any tags at all, or if NOTALLP is non-nil then mark or
unmark those that have no tags at all."
  (with-current-buffer "*Bookmark List*"
    (save-excursion  
      (let ((count  0)
            bmktags presentp)
        (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
        (while (not (eobp))
          (setq bmktags  (bmkp-get-tags (bookmark-bmenu-bookmark)))
          (if (not (if (null tags)
                       (if notallp (not bmktags) bmktags)
                     (and bmktags  (catch 'bmkp-b-mu-b-t-sna
                                     (dolist (tag  tags)
                                       (setq presentp  (assoc-default tag bmktags nil t))
                                       (when (if notallp (not presentp) presentp)
                                         (throw 'bmkp-b-mu-b-t-sna t)))
                                     nil))))
              (forward-line 1)
            (if unmarkp (bookmark-bmenu-unmark) (bookmark-bmenu-mark))
            (setq count  (1+ count))))
        (when msgp
          (if (= 1 count) (message "1 bookmark matched") (message "%d bookmarks matched" count)))))))


;;(@* "General Menu-List (`-*bmenu-*') Commands and Functions")
;;  *** General Menu-List (`-*bmenu-*') Commands and Functions ***

;;;###autoload
(defun bmkp-empty-file (file)           ; Bound to `C-x p 0'
  "Empty the bookmark file FILE, or create FILE (empty) if it does not exist.
In either case, FILE will become an empty bookmark file.  Return FILE.

NOTE: If FILE already exists and you confirm emptying it, no check is
      made that it is in fact a bookmark file before emptying it.
      It is simply replaced by an empty bookmark file and saved.

This does NOT also make FILE the current bookmark file.  To do that,
use `\\[bmkp-switch-bookmark-file]' (`bmkp-switch-bookmark-file')."
  (interactive (list (read-file-name "Create empty bookmark file: " "~/")))
  (setq file  (expand-file-name file))
  (bookmark-maybe-load-default-file)
  (when (and (file-exists-p file)
             (not (y-or-n-p (format "CONFIRM: Empty the existing file `%s'? " file))))
    (error "OK, cancelled"))
  (let ((bookmark-alist  ()))
    (bookmark-write-file file (if (file-exists-p file)
                                  "Emptying bookmark file `%s'..."
                                "Creating new, empty bookmark file `%s'...")))
  file)

;;;###autoload
(defun bmkp-bmenu-w32-open ()           ; Bound to `M-RET' in bookmark list.
  "Use `w32-browser' to open this bookmark."
  (interactive) (let ((bmkp-use-w32-browser-p  t))  (bookmark-bmenu-this-window)))

;;;###autoload
(defun bmkp-bmenu-w32-open-with-mouse (event) ; Bound to `M-mouse-2' in bookmark list.
  "Use `w32-browser' to open the bookmark clicked."
  (interactive "e")
  (save-excursion
    (with-current-buffer (window-buffer (posn-window (event-end event)))
      (save-excursion (goto-char (posn-point (event-end event)))
                      (let ((bmkp-use-w32-browser-p  t))  (bookmark-bmenu-other-window))))))

;;;###autoload
(defun bmkp-bmenu-w32-open-select ()    ; Bound to `M-o' in bookmark-list.
  "Use `w32-browser' to open this bookmark and all marked bookmarks."
  (interactive) (let ((bmkp-use-w32-browser-p  t))  (bookmark-bmenu-select)))

;;;###autoload
(defun bmkp-bmenu-mode-status-help ()   ; Bound to `C-h m' and `?' in bookmark list
  "`describe-mode' + current status of `*Bookmark List*' + face legend."
  (interactive)
  (unless (string= (buffer-name) "*Help*") (bmkp-barf-if-not-in-menu-list))
  (with-current-buffer (get-buffer-create "*Help*")
    (with-output-to-temp-buffer "*Help*"
      (let ((buffer-read-only  nil)
            top)
        (erase-buffer)
        (save-excursion
          (let ((standard-output  (current-buffer)))
            (if (> emacs-major-version 21)
                (describe-function-1 'bookmark-bmenu-mode)
              (describe-function-1 'bookmark-bmenu-mode nil t)))
          (help-setup-xref (list #'bmkp-bmenu-mode-status-help) (interactive-p))
          (goto-char (point-min))
          ;; This text must be the same as the last line of `bookmark-bmenu-mode' doc string.
          (search-forward "Each line represents an Emacs bookmark.\n\n\n" nil t)
          (delete-region (point-min) (point)) ; Get rid of intro from `describe-function'.
          (insert "*************************** Bookmark List ***************************\n\n")
          (insert "Major mode for editing a list of bookmarks.\n")
          (insert "Each line represents an Emacs bookmark.\n\n")
          (setq top  (point))
          ;; Add buttons to access help and Customize.
          ;; Not for Emacs 21.3 - its `help-insert-xref-button' signature is different.
          (when (and (> emacs-major-version 21) ; In `help-mode.el'.
                     (condition-case nil (require 'help-mode nil t) (error nil))
                     (fboundp 'help-insert-xref-button))
            (help-insert-xref-button "[Doc in Commentary]" 'bmkp-commentary-button)
            (insert "           ")
            (help-insert-xref-button "[Doc on the Web]" 'bmkp-help-button)
            (insert "           ")
            (help-insert-xref-button "[Customize]" 'bmkp-customize-button)
            (insert "\n\n")
            (setq top  (point))
            (goto-char (point-max))
            (insert "\nSend a Bookmark+ bug report: `\\[icicle-send-bug-report]'.\n\n")
            (help-insert-xref-button "[Doc in Commentary]" 'bmkp-commentary-button)
            (insert "           ")
            (help-insert-xref-button "[Doc on the Web]" 'bmkp-help-button)
            (insert "           ")
            (help-insert-xref-button "[Customize]" 'bmkp-customize-button)
            (insert "\n\n")
            (goto-char (point-min))
            (forward-line 2))
          (goto-char top)
          (insert (format
                   "\nCurrent Status\n-------------------------------\n
Sorted:\t\t%s\nFiltering:\t%s\nMarked:\t\t%d\nOmitted:\t%d\nBookmark file:\t%s\n\n\n"
                   (if (not bmkp-sort-comparer)
                       "no"
                     (format "%s%s" (bmkp-current-sort-order)
                             ;; Code essentially the same as found in `bmkp-msg-about-sort-order'.
                             (if (not (and (consp bmkp-sort-comparer) ; Ordinary single predicate
                                           (consp (car bmkp-sort-comparer))))
                                 (if bmkp-reverse-sort-p "; reversed" "")
                               (if (not (cadr (car bmkp-sort-comparer)))
                                   ;; Single PRED.
                                   (if (or (and bmkp-reverse-sort-p (not bmkp-reverse-multi-sort-p))
                                           (and bmkp-reverse-multi-sort-p (not bmkp-reverse-sort-p)))
                                       "; reversed"
                                     "")
                                 ;; In case we want to distinguish:
                                 ;; (if (and bmkp-reverse-sort-p
                                 ;;          (not bmkp-reverse-multi-sort-p))
                                 ;;     "; reversed"
                                 ;;   (if (and bmkp-reverse-multi-sort-p
                                 ;;            (not bmkp-reverse-sort-p))
                                 ;;       "; reversed +"
                                 ;;     ""))
                                 
                                 ;; At least two PREDs.
                                 (cond ((and bmkp-reverse-sort-p (not bmkp-reverse-multi-sort-p))
                                        "; reversed")
                                       ((and bmkp-reverse-multi-sort-p (not bmkp-reverse-sort-p))
                                        "; each predicate group reversed")
                                       ((and bmkp-reverse-multi-sort-p bmkp-reverse-sort-p)
                                        "; order of predicate groups reversed")
                                       (t ""))))))
                   (or (and bmkp-bmenu-filter-function (downcase bmkp-bmenu-title)) "None")
                   (length bmkp-bmenu-marked-bookmarks)
                   (length bmkp-bmenu-omitted-list)
                   bmkp-current-bookmark-file))
          ;; Add face legend.
          (let ((gnus             "Gnus\n")
                (info             "Info node\n")
                (man              "Man page\n")
                (w3m              "W3M (URL)\n")
                (local-no-region  "Local file with no region\n")
                (local-w-region   "Local file with a region\n")
                (buffer           "Buffer\n")
                (no-buf           "No current buffer\n")
                (bad              "Possibly invalid bookmark\n")
                (remote           "Remote file or directory\n")
                (sudo             "Remote accessed by `su' or `sudo'\n")
                (local-dir        "Local directory\n")
                (bookmark-list    "*Bookmark List*\n")
                (bookmark-file    "Bookmark file\n")
                (desktop          "Desktop\n")
                (sequence         "Sequence\n")
                (varlist          "Variable list\n")
                (function         "Function\n"))
            (put-text-property 0 (1- (length gnus))     'face 'bmkp-gnus         gnus)
            (put-text-property 0 (1- (length info))     'face 'bmkp-info         info)
            (put-text-property 0 (1- (length man))      'face 'bmkp-man          man)
            (put-text-property 0 (1- (length w3m))      'face 'bmkp-w3m          w3m)
            (put-text-property 0 (1- (length local-no-region))
                               'face 'bmkp-local-file-without-region             local-no-region)
            (put-text-property 0 (1- (length local-w-region))
                               'face 'bmkp-local-file-with-region                local-w-region)
            (put-text-property 0 (1- (length buffer))   'face 'bmkp-buffer       buffer)
            (put-text-property 0 (1- (length no-buf))   'face 'bmkp-non-file     no-buf)
            (put-text-property 0 (1- (length bad))      'face 'bmkp-bad-bookmark bad)
            (put-text-property 0 (1- (length remote))   'face 'bmkp-remote-file  remote)
            (put-text-property 0 (1- (length sudo))     'face 'bmkp-su-or-sudo   sudo)
            (put-text-property 0 (1- (length local-dir))
                               'face 'bmkp-local-directory                       local-dir)
            (put-text-property 0 (1- (length bookmark-list))
                               'face 'bmkp-bookmark-list                         bookmark-list)
            (put-text-property 0 (1- (length bookmark-file))
                               'face 'bmkp-bookmark-file                         bookmark-file)
            (put-text-property 0 (1- (length desktop))  'face 'bmkp-desktop      desktop)
            (put-text-property 0 (1- (length sequence)) 'face 'bmkp-sequence     sequence)
            (put-text-property 0 (1- (length varlist))  'face 'bmkp-varlist      varlist)
            (put-text-property 0 (1- (length function)) 'face 'bmkp-function     function)
            (insert "Face Legend for Bookmark Types\n------------------------------\n\n")
            (insert gnus) (insert info) (insert man) (insert w3m) (insert local-no-region)
            (insert local-w-region) (insert buffer) (insert no-buf) (insert bad) (insert remote)
            (insert sudo) (insert local-dir) (insert bookmark-list) (insert bookmark-file)
            (insert desktop) (insert sequence) (insert varlist) (insert function)
            (insert "\n\n")))))))

(when (and (> emacs-major-version 21)
           (condition-case nil (require 'help-mode nil t) (error nil))
           (get 'help-xref 'button-category-symbol)) ; In `button.el'
  (define-button-type 'bmkp-help-button
      :supertype 'help-xref
      'help-function #'(lambda () (browse-url "http://www.emacswiki.org/emacs/BookmarkPlus"))
      'help-echo
      (purecopy "mouse-2, RET: Bookmark+ documentation on the Emacs Wiki (requires Internet access)"))
  (define-button-type 'bmkp-commentary-button
      :supertype 'help-xref
      'help-function #'(lambda ()
                         (message "Getting Bookmark+ doc from file commentary...")
                         (finder-commentary "bookmark+-doc")
                         (when (condition-case nil (require 'linkd nil t) (error nil)) (linkd-mode 1))
                         (when (condition-case nil (require 'fit-frame nil t) (error nil))
                           (fit-frame)))
      'help-echo (purecopy "mouse-2, RET: Bookmark+ documentation (no Internet needed)"))
  (define-button-type 'bmkp-customize-button
      :supertype 'help-xref
      'help-function #'(lambda () (customize-group-other-window 'bookmark-plus))
      'help-echo (purecopy "mouse-2, RET: Customize/Browse Bookmark+ Options & Faces")))

;;;###autoload
(defun bmkp-bmenu-define-jump-marked-command () ; Bound to `M-c' in bookmark list
  "Define a command to jump to a bookmark that is one of those now marked.
The bookmarks marked now will be those that are completion candidates
for the command (but omitted bookmarks are excluded).
Save the command definition in `bmkp-bmenu-commands-file'."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (let* ((cands  (mapcar #'list (bmkp-remove-if #'(lambda (bmk) (member bmk bmkp-bmenu-omitted-list))
                                                bmkp-bmenu-marked-bookmarks)))
         (fn     (intern (read-string "Define command to jump to a bookmark now marked: " nil
                                      'bmkp-bmenu-define-command-history)))
         (def    `(defun ,fn (bookmark-name &optional use-region-p)
                   (interactive (list (bmkp-read-bookmark-for-type nil ',cands t) current-prefix-arg))
                   (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))))
    (eval def)
    (with-current-buffer (get-buffer-create " *User Bookmark List Commands*")
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (let ((print-length  nil)
            (print-level   nil))
        (pp def (current-buffer))
        (insert "\n")
        (condition-case nil
            (write-region (point-min) (point-max) bmkp-bmenu-commands-file 'append)
          (file-error (error "Cannot write `%s'" bmkp-bmenu-commands-file)))
        (kill-buffer (current-buffer))))
    (message "Command `%s' defined and saved in file `%s'" fn bmkp-bmenu-commands-file)))

;;;###autoload
(defun bmkp-bmenu-define-command ()     ; Bound to `c' in bookmark list
  "Define a command to use the current sort order, filter, and omit list.
Prompt for the command name.  Save the command definition in
`bmkp-bmenu-commands-file'.

The current sort order, filter function, omit list, and title for
buffer `*Bookmark List*' are encapsulated as part of the command.
Use the command at any time to restore them."
  (interactive)
  (let* ((fn   (intern (read-string "Define sort+filter command: " nil
                                    'bmkp-bmenu-define-command-history)))
         (def  `(defun ,fn ()
                 (interactive)
                 (setq
                  bmkp-sort-comparer               ',bmkp-sort-comparer
                  bmkp-reverse-sort-p              ',bmkp-reverse-sort-p
                  bmkp-reverse-multi-sort-p        ',bmkp-reverse-multi-sort-p
                  bmkp-bmenu-filter-function       ',bmkp-bmenu-filter-function
                  bmkp-bmenu-filter-pattern        ',bmkp-bmenu-filter-pattern
                  bmkp-bmenu-omitted-list          ',bmkp-bmenu-omitted-list
                  bmkp-bmenu-title                 ',bmkp-bmenu-title
                  bookmark-bmenu-toggle-filenames  ',bookmark-bmenu-toggle-filenames)
                 (bmkp-bmenu-refresh-menu-list)
                 (when (interactive-p)
                   (bmkp-msg-about-sort-order
                    (car (rassoc bmkp-sort-comparer bmkp-sort-orders-alist)))))))
    (eval def)
    (with-current-buffer (get-buffer-create " *User Bookmark List Commands*")
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (let ((print-length  nil)
            (print-level   nil))
        (pp def (current-buffer))
        (insert "\n")
        (condition-case nil
            (write-region (point-min) (point-max) bmkp-bmenu-commands-file 'append)
          (file-error (error "Cannot write `%s'" bmkp-bmenu-commands-file)))
        (kill-buffer (current-buffer))))
    (message "Command `%s' defined and saved in file `%s'" fn bmkp-bmenu-commands-file)))

;;;###autoload
(defun bmkp-bmenu-define-full-snapshot-command () ; Bound to `C' in bookmark list
  "Define a command to restore the current bookmark-list state.
Prompt for the command name.  Save the command definition in
`bmkp-bmenu-commands-file'.

Be aware that the command definition can be quite large, since it
copies the current bookmark list and accessory lists (hidden
bookmarks, marked bookmarks, etc.).  For a lighter weight command, use
`bmkp-bmenu-define-full-snapshot-command' instead.  That records only
the omit list and the sort & filter information."
  (interactive)
  (let* ((fn   (intern (read-string "Define restore-snapshot command: " nil
                                    'bmkp-bmenu-define-command-history)))
         (def  `(defun ,fn ()
                 (interactive)
                 (setq
                  bmkp-sort-comparer                     ',bmkp-sort-comparer
                  bmkp-reverse-sort-p                    ',bmkp-reverse-sort-p
                  bmkp-reverse-multi-sort-p              ',bmkp-reverse-multi-sort-p
                  bmkp-latest-bookmark-alist             ',bmkp-latest-bookmark-alist
                  bmkp-bmenu-omitted-list                ',bmkp-bmenu-omitted-list
                  bmkp-bmenu-marked-bookmarks            ',bmkp-bmenu-marked-bookmarks
                  bmkp-bmenu-filter-function             ',bmkp-bmenu-filter-function
                  bmkp-bmenu-filter-pattern              ',bmkp-bmenu-filter-pattern
                  bmkp-bmenu-title                       ',bmkp-bmenu-title
                  bmkp-last-bmenu-bookmark               ',(and (get-buffer "*Bookmark List*")
                                                                (with-current-buffer
                                                                    (get-buffer "*Bookmark List*")
                                                                  (bookmark-bmenu-bookmark)))
                  bmkp-last-specific-buffer              ',bmkp-last-specific-buffer
                  bmkp-last-specific-file                ',bmkp-last-specific-file
                  bookmark-bmenu-toggle-filenames        ',bookmark-bmenu-toggle-filenames
                  bmkp-bmenu-before-hide-marked-alist    ',bmkp-bmenu-before-hide-marked-alist
                  bmkp-bmenu-before-hide-unmarked-alist  ',bmkp-bmenu-before-hide-unmarked-alist
                  bmkp-last-bookmark-file                ',bmkp-last-bookmark-file
                  bmkp-current-bookmark-file             ',bmkp-current-bookmark-file)
                 ;;(bmkp-bmenu-refresh-menu-list)
                 (let ((bookmark-alist  (or bmkp-latest-bookmark-alist bookmark-alist)))
                   (bmkp-bmenu-list-1 'filteredp nil (interactive-p)))
                 (when bmkp-last-bmenu-bookmark
                   (with-current-buffer (get-buffer "*Bookmark List*")
                     (bmkp-bmenu-goto-bookmark-named bmkp-last-bmenu-bookmark)))
                 (when (interactive-p)
                   (bmkp-msg-about-sort-order
                    (car (rassoc bmkp-sort-comparer bmkp-sort-orders-alist)))))))
    (eval def)
    (with-current-buffer (get-buffer-create " *User Bookmark List Commands*")
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (let ((print-length  nil)
            (print-level   nil))
        (pp def (current-buffer))
        (insert "\n")
        (condition-case nil
            (write-region (point-min) (point-max) bmkp-bmenu-commands-file 'append)
          (file-error (error "Cannot write `%s'" bmkp-bmenu-commands-file)))
        (kill-buffer (current-buffer))))
    (message "Command `%s' defined and saved in file `%s'" fn bmkp-bmenu-commands-file)))

;;;###autoload
(defun bmkp-define-tags-sort-command (tags &optional msgp) ; Bound to `T s' in bookmark list
  "Define a command to sort bookmarks in the bookmark list by tags.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.

The new command sorts first by the first tag in TAGS, then by the
second, and so on.

Besides sorting for these specific tags, any bookmark that has a tag
sorts before one that has no tags.  Otherwise, sorting is by bookmark
name, alphabetically.

The name of the new command is `bmkp-bmenu-sort-' followed by the
specified tags, in order, separated by hyphens (`-').  E.g., for TAGS
\(\"alpha\" \"beta\"), the name is `bmkp-bmenu-sort-alpha-beta'."
  (interactive (list (bmkp-read-tags-completing) 'msg))
  (let ((sort-order  (concat "tags-" (mapconcat #'identity tags "-")))
        (doc-string  (read-string "Doc string for command: "))
        (comparer    ())
        def)
    (dolist (tag  tags)
      (push `(lambda (b1 b2)
              (let ((tags1  (bmkp-get-tags b1))
                    (tags2  (bmkp-get-tags b2)))
                (cond ((and (assoc-default ,tag tags1 nil t)
                            (assoc-default ,tag tags2 nil t))  nil)
                      ((assoc-default ,tag tags1 nil t)        '(t))
                      ((assoc-default ,tag tags2 nil t)        '(nil))
                      ((and tags1 tags2)                       nil)
                      (tags1                                   '(t))
                      (tags2                                   '(nil))
                      (t                                       nil))))
            comparer))
    (setq comparer  (nreverse comparer)
          comparer  (list comparer 'bmkp-alpha-p))
    (eval (setq def  (macroexpand `(bmkp-define-sort-command ,sort-order ,comparer ,doc-string))))
    (with-current-buffer (get-buffer-create " *User Bookmark List Commands*")
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (let ((print-length  nil)
            (print-level   nil))
        (pp def (current-buffer))
        (insert "\n")
        (condition-case nil
            (write-region (point-min) (point-max) bmkp-bmenu-commands-file 'append)
          (file-error (error "Cannot write `%s'" bmkp-bmenu-commands-file)))
        (kill-buffer (current-buffer))))
    (when msgp (message "Defined and saved command `%s'"
                        (concat "bmkp-bmenu-sort-" sort-order)))))

;;;###autoload
(defun bmkp-bmenu-edit-bookmark ()      ; Bound to `E' in bookmark list
  "Edit the bookmark under the cursor: its name and file name."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (let* ((new-data  (bmkp-edit-bookmark (bookmark-bmenu-bookmark)))
         (new-name  (car new-data)))
    (if (not new-data)
        (message "No changes made")
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (goto-char (point-min))
      (while (not (equal new-name (bookmark-bmenu-bookmark))) (forward-line 1))
      (forward-line 0))))

(defun bmkp-bmenu-propertize-item (bookmark-name start end)
  "Add text properties to BOOKMARK-NAME, from START to END."
  (let* ((buffp            (bmkp-get-buffer-name bookmark-name))
         (sequencep        (bmkp-sequence-bookmark-p bookmark-name))
         (functionp        (bmkp-function-bookmark-p bookmark-name))
         (varlistp         (bmkp-varlist-bookmark-p bookmark-name))
         (w3m-p            (bmkp-w3m-bookmark-p bookmark-name))
         (gnus-p           (bmkp-gnus-bookmark-p bookmark-name))
         (desktop-p        (bmkp-desktop-bookmark-p bookmark-name))
         (bookmark-file-p  (bmkp-bookmark-file-bookmark-p bookmark-name))
         (handlerp         (bookmark-get-handler bookmark-name))
         (info-p           (bmkp-info-bookmark-p bookmark-name))
         (man-p            (bmkp-man-bookmark-p bookmark-name))
         (blist-p          (bmkp-bookmark-list-bookmark-p bookmark-name))
         (regionp          (bmkp-region-bookmark-p bookmark-name))

         ;; Begin `let*' dependencies.
         (filep            (bookmark-get-filename bookmark-name))
         (remotep          (and filep  (bmkp-file-remote-p filep)))
         (tramp-p          (and filep  (boundp 'tramp-file-name-regexp)
                                (save-match-data (string-match tramp-file-name-regexp filep))))
         (su-p             (and tramp-p (string-match bmkp-su-or-sudo-regexp filep))))
    (put-text-property start end 'bmkp-bookmark-name bookmark-name)
    (add-text-properties
     start  end
     (cond (sequencep        (append (bmkp-face-prop 'bmkp-sequence)
                                     '(mouse-face highlight follow-link t
                                       help-echo "mouse-2: Invoke the bookmarks in this sequence")))
           (functionp        (append (bmkp-face-prop 'bmkp-function)
                                     '(mouse-face highlight follow-link t
                                       help-echo "mouse-2: Invoke this function bookmark")))
           (varlistp         (append (bmkp-face-prop 'bmkp-varlist)
                                     '(mouse-face highlight follow-link t
                                       help-echo "mouse-2: Invoke this variable-list bookmark")))
           (blist-p          (append (bmkp-face-prop 'bmkp-bookmark-list)
                                     '(mouse-face highlight follow-link t
                                       help-echo "mouse-2: Invoke this bookmark-list bookmark")))
           (desktop-p        (append (bmkp-face-prop 'bmkp-desktop)
                                     '(mouse-face highlight follow-link t
                                       help-echo "mouse-2: Jump to this desktop bookmark")))
           (bookmark-file-p  (append (bmkp-face-prop 'bmkp-bookmark-file)
                                     '(mouse-face highlight follow-link t
                                       help-echo "mouse-2: Load this bookmark's bookmark file")))
           (info-p           (append (bmkp-face-prop 'bmkp-info)
                                     '(mouse-face highlight follow-link t
                                       help-echo "mouse-2: Jump to this Info bookmark")))
           (man-p            (append (bmkp-face-prop 'bmkp-man)
                                     '(mouse-face highlight follow-link t
                                       help-echo (format "mouse-2 Goto `man' page"))))
           (gnus-p           (append (bmkp-face-prop 'bmkp-gnus)
                                     '(mouse-face highlight follow-link t
                                       help-echo "mouse-2: Jump to this Gnus bookmark")))
           (w3m-p            (append (bmkp-face-prop 'bmkp-w3m)
                                     `(mouse-face highlight follow-link t
                                       help-echo (format "mouse-2: Jump to URL `%s'" ,filep))))
           ((and su-p (not (bmkp-root-or-sudo-logged-p))) ; Root/sudo not logged
            (append (bmkp-face-prop 'bmkp-su-or-sudo)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Jump to (visit) file `%s'" ,filep))))
           ;; Make sure we test for remoteness before any other tests of the file itself
           ;; (e.g. `file-exists-p'). We don't want to prompt for a password etc.
           ((and remotep (not su-p))    ; Remote file (ssh, ftp)
            (append (bmkp-face-prop 'bmkp-remote-file)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Jump to (visit) remote file `%s'" ,filep))))
           ((and filep (file-directory-p filep)) ; Local directory
            (append (bmkp-face-prop 'bmkp-local-directory)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Dired directory `%s'" ,filep))))
           ((and filep (file-exists-p filep) regionp) ; Local file with region
            (append (bmkp-face-prop 'bmkp-local-file-with-region)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Activate region in file `%s'" ,filep))))
           ((and filep (file-exists-p filep)) ; Local file without region
            (append (bmkp-face-prop 'bmkp-local-file-without-region)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Jump to (visit) file `%s'" ,filep))))
           ((and buffp (get-buffer buffp) (equal filep bmkp-non-file-filename)) ; Buffer
            (append (bmkp-face-prop 'bmkp-buffer)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Jump to buffer `%s'" ,buffp))))
           ((and buffp  (or (not filep) (equal filep bmkp-non-file-filename)
                            (not (file-exists-p filep)))) ; Buffer bookmark, but no buffer.
            (append (bmkp-face-prop 'bmkp-non-file)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Jump to buffer `%s'" ,buffp))))
           (t (append (bmkp-face-prop 'bmkp-bad-bookmark)
                      `(mouse-face highlight follow-link t
                        help-echo (format "BAD BOOKMARK (maybe): `%s'" ,filep))))))))

;;;###autoload
(defun bmkp-bmenu-quit ()               ; Bound to `q' in bookmark list
  "Quit the bookmark list (aka \"menu list\").
If `bmkp-bmenu-state-file' is non-nil, then save the state, to be
restored the next time the bookmark list is shown.  Otherwise, reset
the internal lists that record menu-list markings."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (if (not bmkp-bmenu-state-file)
      (setq bmkp-bmenu-marked-bookmarks            ()
            bmkp-bmenu-before-hide-marked-alist    ()
            bmkp-bmenu-before-hide-unmarked-alist  ())
    (bmkp-save-menu-list-state)
    (setq bmkp-bmenu-first-time-p  t))
  (quit-window))

(defun bmkp-save-menu-list-state ()
  "Save menu-list state, unless not saving or list has not yet been shown."
  (when (and (not bmkp-bmenu-first-time-p) bmkp-bmenu-state-file)
    (let ((config-list
           `((last-sort-comparer                    . ,bmkp-sort-comparer)
             (last-reverse-sort-p                   . ,bmkp-reverse-sort-p)
             (last-reverse-multi-sort-p             . ,bmkp-reverse-multi-sort-p)
             (last-latest-bookmark-alist            . ,bmkp-latest-bookmark-alist)
             (last-omitted-list                     . ,bmkp-bmenu-omitted-list)
             (last-bmenu-marked-bookmarks           . ,bmkp-bmenu-marked-bookmarks)
             (last-bmenu-filter-function            . ,bmkp-bmenu-filter-function)
             (last-bmenu-filter-pattern             . ,bmkp-bmenu-filter-pattern)
             (last-bmenu-title                      . ,bmkp-bmenu-title)
             (last-bmenu-bookmark                   . ,(and (get-buffer "*Bookmark List*")
                                                            (with-current-buffer
                                                                (get-buffer "*Bookmark List*")
                                                              (bookmark-bmenu-bookmark))))
             (last-specific-buffer                  . ,bmkp-last-specific-buffer)
             (last-specific-file                    . ,bmkp-last-specific-file)
             (last-bmenu-toggle-filenames           . ,bookmark-bmenu-toggle-filenames)
             (last-bmenu-before-hide-marked-alist   . ,bmkp-bmenu-before-hide-marked-alist)
             (last-bmenu-before-hide-unmarked-alist . ,bmkp-bmenu-before-hide-unmarked-alist)
             (last-bookmark-file                    . ,(convert-standard-filename
                                                        (expand-file-name
                                                         bmkp-current-bookmark-file))))))
      (with-current-buffer (get-buffer-create " *Menu-List State*")
        (goto-char (point-min))
        (delete-region (point-min) (point-max))
        (let ((print-length  nil)
              (print-level   nil))
          (pp config-list (current-buffer))
          (condition-case nil
              (write-region (point-min) (point-max) bmkp-bmenu-state-file)
            (file-error (message "Cannot write `%s'" bmkp-bmenu-state-file)))
          (kill-buffer (current-buffer)))))))

(defun bmkp-bmenu-goto-bookmark-named (name)
  "Go to the first bookmark whose name matches NAME."
  (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
  (while (and (not (eobp)) (not (equal name (bookmark-bmenu-bookmark))))  (forward-line 1))
  (bookmark-bmenu-ensure-position))     ; Just in case we fall off the end.

(defun bmkp-barf-if-not-in-menu-list ()
  "Raise an error if current buffer is not `*Bookmark List*'."
  (unless (equal (buffer-name (current-buffer)) "*Bookmark List*")
    (error "You can only use this command in buffer `*Bookmark List*'")))


;;(@* "Bookmark Predicates")
;;  *** Bookmark Predicates ***

(defun bmkp-bookmark-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a bookmark-file bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'bmkp-jump-bookmark-file))

(defun bmkp-bookmark-list-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a bookmark-list bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'bmkp-jump-bookmark-list))

(defun bmkp-desktop-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a desktop bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'bmkp-jump-desktop))

;; Note: To avoid remote access, if bookmark does not have the Dired handler, then we insist
;; that it be for a local directory.  IOW, we do not include remote directories that were not
;; bookmarked by Bookmark+ (and so do not have the Dired handler).
(defun bmkp-dired-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Dired bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (eq (bookmark-get-handler bookmark) 'bmkp-jump-dired)
      (bmkp-local-directory-bookmark-p bookmark)))

(defun bmkp-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a file or directory.
BOOKMARK is a bookmark name or a bookmark record.
This excludes bookmarks of a more specific kind (Info, Gnus, and W3M)."
  (let* ((filename   (bookmark-get-filename bookmark))
         (nonfile-p  (equal filename bmkp-non-file-filename))
         (handler    (bookmark-get-handler bookmark)))
    (and filename (not nonfile-p) (or (not handler) (eq handler 'bmkp-jump-dired))
         (not (and (bookmark-prop-get bookmark 'info-node)))))) ; Emacs 20-21 Info: no handler.

(defun bmkp-function-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a function bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'bmkp-jump-function))

(defun bmkp-gnus-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Gnus bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (memq (bookmark-get-handler bookmark) '(bmkp-jump-gnus bmkext-jump-gnus)))

(defun bmkp-info-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is an Info bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (eq (bookmark-get-handler bookmark) 'Info-bookmark-jump)
      (string= "*info*" (bmkp-get-buffer-name bookmark))
      ;; Emacs 20-21 form - no handler (and no `buffer-name' entry).
      (bookmark-prop-get bookmark 'info-node)))

(defun bmkp-local-directory-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a local directory.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((file  (bookmark-get-filename bookmark)))
    (and (bmkp-local-file-bookmark-p bookmark) (file-directory-p file))))

(defun bmkp-local-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a local file or directory.
BOOKMARK is a bookmark name or a bookmark record.
This excludes bookmarks of a more specific kind (Info, Gnus, and W3M)."
  (and (bmkp-file-bookmark-p bookmark) (not (bmkp-remote-file-bookmark-p bookmark))))

(defun bmkp-man-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a `man' page bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (memq (bookmark-get-handler bookmark) '(bmkp-jump-man bmkp-jump-woman
                                          bmkext-jump-man bmkext-jump-woman)))

(defun bmkp-marked-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a marked bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (unless (stringp bookmark) (setq bookmark  (car bookmark)))
  (member bookmark bmkp-bmenu-marked-bookmarks))

(defun bmkp-non-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a non-file bookmark (e.g *scratch*).
This excludes bookmarks of a more specific kind (Info, Gnus, and W3M).
It includes bookmarks to existing buffers, as well as bookmarks
defined for buffers that do not currently exist."
  (let* ((filename   (bookmark-get-filename bookmark))
         (nonfile-p  (equal filename bmkp-non-file-filename))) 
    (and (bmkp-get-buffer-name bookmark)
         (or (not filename) nonfile-p
             ;; Ensure not remote before calling `file-exists-p'.  (Do not prompt for password.)
             (and (not (bmkp-file-remote-p filename)) (not (file-exists-p filename))))
         (not (bookmark-get-handler bookmark)))))

(defun bmkp-region-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK has region information.
BOOKMARK is a bookmark name or a bookmark record."
  (and (bmkp-get-end-position bookmark)
       (/= (bookmark-get-position bookmark) (bmkp-get-end-position bookmark))))

(defun bmkp-remote-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a remote file or directory.
BOOKMARK is a bookmark name or a bookmark record.
This includes remote Dired bookmarks, but otherwise excludes bookmarks
with handlers (Info, Gnus, and W3M)."
  (let* ((handler   (bookmark-get-handler bookmark))
         (file      (bookmark-get-filename bookmark))
         (rem-file  (and file  (bmkp-file-remote-p file))))
    (and rem-file  (or (not handler) (eq handler 'bmkp-jump-dired)))))

(defun bmkp-this-buffer-p (bookmark)
  "Return non-nil if BOOKMARK's buffer is the current buffer.
But return nil for bookmarks, such as desktops, that are not really
associated with a buffer, even if they have a `buffer-name' entry.
BOOKMARK is a bookmark name or a bookmark record."
  (and (equal (bmkp-get-buffer-name bookmark) (buffer-name))
       (not (bmkp-desktop-bookmark-p        bookmark))
       (not (bmkp-bookmark-file-bookmark-p  bookmark))
       (not (bmkp-sequence-bookmark-p       bookmark))
       (not (bmkp-function-bookmark-p       bookmark))
       (not (bmkp-varlist-bookmark-p        bookmark))))

(defun bmkp-this-file-p (bookmark)
  "Return non-nil if BOOKMARK's filename is the current (absolute) file name.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((bmk-file   (bookmark-get-filename bookmark))
        (this-file  (or (buffer-file-name)
                        (and (eq major-mode 'dired-mode)  (if (consp dired-directory)
                                                              (car dired-directory)
                                                            dired-directory)))))
    (and bmk-file (equal bmk-file this-file))))

(defun bmkp-last-specific-buffer-p (bookmark)
  "Return t if BOOKMARK's `buffer-name' is `bmkp-last-specific-buffer'.
But return nil for bookmarks, such as desktops, that are not really
associated with a buffer, even if they have a `buffer-name' entry.
It does not matter whether the buffer exists.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((buf  (bmkp-get-buffer-name bookmark)))
    (and buf (string= buf bmkp-last-specific-buffer)
         (not (bmkp-desktop-bookmark-p        bookmark))
         (not (bmkp-bookmark-file-bookmark-p  bookmark))
         (not (bmkp-sequence-bookmark-p       bookmark))
         (not (bmkp-function-bookmark-p       bookmark))
         (not (bmkp-varlist-bookmark-p        bookmark)))))

(defun bmkp-last-specific-file-p (bookmark)
  "Return t if BOOKMARK's `filename' is `bmkp-last-specific-file'.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((file  (bookmark-get-filename bookmark)))
    (and file (string= file bmkp-last-specific-file))))

(defun bmkp-sequence-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a sequence bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'bmkp-jump-sequence))

(defun bmkp-varlist-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a variable-list bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'bmkp-jump-varlist))

(defun bmkp-w3m-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a W3M bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (memq (bookmark-get-handler bookmark) '(bmkp-jump-w3m bmkext-jump-w3m)))


;;(@* "Filter Functions")
;;  *** Filter Functions ***

(defun bmkp-all-tags-alist-only (tags)
  "`bookmark-alist', but with only bookmarks having all their tags in TAGS.
A new list is returned (no side effects)."
  (bmkp-remove-if-not
   #'(lambda (bmk)
       (let ((bmk-tags  (bmkp-get-tags bmk)))
         (and bmk-tags (bmkp-every #'(lambda (tag) (bmkp-has-tag-p bmk tag)) tags))))
   bookmark-alist))

(defun bmkp-all-tags-regexp-alist-only (regexp)
  "`bookmark-alist', but with only bookmarks having all tags match REGEXP.
A new list is returned (no side effects)."
  (bmkp-remove-if-not
   #'(lambda (bmk)
       (let ((bmk-tags  (bmkp-get-tags bmk)))
         (and bmk-tags (bmkp-every #'(lambda (tag) (string-match regexp (bmkp-tag-name tag)))
                                   bmk-tags))))
   bookmark-alist))

(defun bmkp-autonamed-alist-only ()
  "`bookmark-alist', with only autonamed bookmarks (from any buffers).
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-autonamed-bookmark-p bookmark-alist))

(defun bmkp-autonamed-this-buffer-alist-only ()
  "`bookmark-alist', with only autonamed bookmarks for the current buffer.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not (lambda (bmk) (bmkp-autonamed-bookmark-for-buffer-p bmk (buffer-name)))
                      bookmark-alist))

(defun bmkp-bookmark-file-alist-only ()
  "`bookmark-alist', filtered to retain only bookmark-file bookmarks.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-bookmark-file-bookmark-p bookmark-alist))

(defun bmkp-bookmark-list-alist-only ()
  "`bookmark-alist', filtered to retain only bookmark-list bookmarks.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-bookmark-list-bookmark-p bookmark-alist))

(defun bmkp-desktop-alist-only ()
  "`bookmark-alist', filtered to retain only desktop bookmarks.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-desktop-bookmark-p bookmark-alist))

(defun bmkp-dired-alist-only ()
  "`bookmark-alist', filtered to retain only Dired bookmarks.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-dired-bookmark-p bookmark-alist))

(defun bmkp-file-alist-only ()
  "`bookmark-alist', filtered to retain only file and directory bookmarks.
This excludes bookmarks that might contain file information but are
particular in some way - for example, Info bookmarks or Gnus bookmarks.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-file-bookmark-p bookmark-alist))

(defun bmkp-gnus-alist-only ()
  "`bookmark-alist', filtered to retain only Gnus bookmarks.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-gnus-bookmark-p bookmark-alist))

(defun bmkp-info-alist-only ()
  "`bookmark-alist', filtered to retain only Info bookmarks.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-info-bookmark-p bookmark-alist))

(defun bmkp-last-specific-buffer-alist-only ()
  "`bookmark-alist', but only for `bmkp-last-specific-buffer'.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-last-specific-buffer-p bookmark-alist))

(defun bmkp-last-specific-file-alist-only ()
  "`bookmark-alist', but only for `bmkp-last-specific-file'.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-last-specific-file-p bookmark-alist))

(defun bmkp-man-alist-only ()
  "`bookmark-alist', filtered to retain only `man' page bookmarks.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-man-bookmark-p bookmark-alist))

(defun bmkp-local-file-alist-only ()
  "`bookmark-alist', filtered to retain only local-file bookmarks.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-local-file-bookmark-p bookmark-alist))

(defun bmkp-non-autonamed-alist-only ()
  "`bookmark-alist', with only non-autonamed bookmarks (from any buffers).
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not (lambda (bmk) (not (bmkp-autonamed-bookmark-p bmk))) bookmark-alist))

(defun bmkp-non-file-alist-only ()
  "`bookmark-alist', filtered to retain only non-file bookmarks.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-non-file-bookmark-p bookmark-alist))

(defun bmkp-regexp-filtered-bookmark-name-alist-only ()
  "`bookmark-alist' for bookmarks matching `bmkp-bmenu-filter-pattern'."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not
   #'(lambda (bmk) (string-match bmkp-bmenu-filter-pattern (car bmk))) bookmark-alist))

(defun bmkp-regexp-filtered-file-name-alist-only ()
  "`bookmark-alist' for files matching `bmkp-bmenu-filter-pattern'."
  (bookmark-maybe-load-default-file)
  (let (fname)
    (bmkp-remove-if-not #'(lambda (bmk) (and (setq fname  (bookmark-get-filename bmk))
                                             (string-match bmkp-bmenu-filter-pattern fname)))
                        bookmark-alist)))

(defun bmkp-regexp-filtered-tags-alist-only ()
  "`bookmark-alist' for tags matching `bmkp-bmenu-filter-pattern'."
  (bookmark-maybe-load-default-file)
  (let (tags)
    (bmkp-remove-if-not
     #'(lambda (bmk) (and (setq tags  (bmkp-get-tags bmk))
                          (bmkp-some (lambda (tag)
                                       (string-match bmkp-bmenu-filter-pattern (bmkp-tag-name tag)))
                                     tags)))
     bookmark-alist)))

(defun bmkp-region-alist-only ()
  "`bookmark-alist', filtered to retain only bookmarks that have regions.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-region-bookmark-p bookmark-alist))

(defun bmkp-remote-file-alist-only ()
  "`bookmark-alist', filtered to retain only remote-file bookmarks.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-remote-file-bookmark-p bookmark-alist))

(defun bmkp-some-tags-alist-only (tags)
  "`bookmark-alist', but with only bookmarks having some tags in TAGS.
A new list is returned (no side effects)."
  (bmkp-remove-if-not
   #'(lambda (bmk) (bmkp-some #'(lambda (tag) (bmkp-has-tag-p bmk tag)) tags))
   bookmark-alist))

(defun bmkp-some-tags-regexp-alist-only (regexp)
  "`bookmark-alist', but with only bookmarks having some tags match REGEXP.
A new list is returned (no side effects)."
  (bmkp-remove-if-not
   #'(lambda (bmk)
       (bmkp-some #'(lambda (tag) (string-match regexp (bmkp-tag-name tag))) (bmkp-get-tags bmk)))
   bookmark-alist))

(defun bmkp-specific-buffers-alist-only (&optional buffers)
  "`bookmark-alist', filtered to retain only bookmarks to buffers BUFFERS.
BUFFERS is a list of buffer names.
It defaults to a singleton list with the current buffer's name.
A new list is returned (no side effects).

Note: Bookmarks created by vanilla Emacs do not record the buffer
name.  They are therefore excluded from the returned alist."
  (unless buffers  (setq buffers  (list (buffer-name))))
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not (lambda (bmk) (and (not (bmkp-desktop-bookmark-p       bmk)) ; Exclude these
                                         (not (bmkp-bookmark-file-bookmark-p bmk))
                                         (not (bmkp-sequence-bookmark-p      bmk))
                                         (not (bmkp-function-bookmark-p      bmk))
                                         (not (bmkp-varlist-bookmark-p       bmk))
                                         (member (bmkp-get-buffer-name bmk) buffers)))
                      bookmark-alist))

(defun bmkp-specific-files-alist-only (&optional files)
  "`bookmark-alist', filtered to retain only bookmarks to files FILES.
FILES is a list of absolute file names.
It defaults to a singleton list with the current buffer's file name.
A new list is returned (no side effects)."
  (unless files  (setq files  (list (buffer-file-name))))
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not (lambda (bmk) (member (bookmark-get-filename bmk) files)) bookmark-alist))

(defun bmkp-this-buffer-alist-only ()
  "`bookmark-alist', with only bookmarks for the current buffer.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-this-buffer-p bookmark-alist))

(defun bmkp-this-file-alist-only ()
  "`bookmark-alist', with only bookmarks for the current file.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-this-file-p bookmark-alist))

(defun bmkp-varlist-alist-only ()
  "`bookmark-alist', filtered to retain only variable-list bookmarks.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-varlist-bookmark-p bookmark-alist))

(defun bmkp-w3m-alist-only ()
  "`bookmark-alist', filtered to retain only W3M bookmarks.
A new list is returned (no side effects)."
  (bookmark-maybe-load-default-file)
  (bmkp-remove-if-not #'bmkp-w3m-bookmark-p bookmark-alist))


;;; Marked bookmarks

(defun bmkp-marked-bookmarks-only ()
  "Return the list of marked bookmarks."
  (bmkp-remove-if-not #'bmkp-marked-bookmark-p bookmark-alist))

(defun bmkp-unmarked-bookmarks-only ()
  "Return the list of unmarked bookmarks."
  (bmkp-remove-if #'bmkp-marked-bookmark-p bookmark-alist))

(defun bmkp-some-marked-p (alist)
  "Return non-nil if ALIST is nonempty and includes a marked bookmark."
  (catch 'break (dolist (i  alist)  (and (bmkp-marked-bookmark-p i)  (throw 'break t)))))

(defun bmkp-some-unmarked-p (alist)
  "Return non-nil if ALIST is nonempty and includes an unmarked bookmark."
  (catch 'break (dolist (i  alist)  (and (not (bmkp-marked-bookmark-p i))  (throw 'break t)))))


;;(@* "General Utility Functions")
;;  *** General Utility Functions ***

(defun bmkp-remove-dups (list)
  "Copy of LIST with duplicate elements removed.  Tested with `equal'."
  (let ((tail  list)
        new)
    (while tail
      (unless (member (car tail) new) (push (car tail) new))
      (pop tail))
    (nreverse new)))
  
(defun bmkp-remove-assoc-dups (alist &optional omit)
  "Shallow copy of ALIST without elements that have duplicate keys.
Only the first element of those with the same key is kept.
Keys are compared using `equal'.
If optional arg OMIT is non-nil, then omit from the return value any
elements with keys in list OMIT."
  (let ((new  ()))
    (dolist (ii  alist)  (unless (or (assoc (car ii) new) (member (car ii) omit))  (push ii new)))
    (nreverse new)))

(defun bmkp-remove-if (pred xs)
  "A copy of list XS with no elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x  xs)  (unless (funcall pred x) (push x result)))
    (nreverse result)))

(defun bmkp-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x  xs)  (when (funcall pred x) (push x result)))
    (nreverse result)))

;; Similar to `every' in `cl-extra.el', without non-list sequences and multiple sequences.
(defun bmkp-every (predicate list)
  "Return t if PREDICATE is true for all elements of LIST; else nil."
  (let ((res  nil))
    (while (and list (funcall predicate (car list))) (setq list  (cdr list)))
    (null list)))

;; Similar to `some' in `cl-extra.el', without non-list sequences and multiple sequences.
(defun bmkp-some (predicate list)
  "Return non-nil if PREDICATE is true for some element of LIST; else nil.
Return the first non-nil value returned by PREDICATE."
  (let ((res  nil))
    (while (and list (not (setq res  (funcall predicate (pop list))))))
    res))

;; From `cl-seq.el', function `union', without keyword treatment.
;; (Same as `simple-set-union' in `misc-fns.el' and `icicle-set-union' in `icicles-fn.el'.)
(defun bmkp-set-union (list1 list2)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or
LIST2.  Comparison is done using `equal'.  This is a non-destructive
function; it copies the data if necessary."
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

(defun bmkp-upcase (string)
  "`upcase', but in case of error, return original STRING.
This works around an Emacs 20 problem that occurs if STRING contains
binary data (weird chars)."
  (condition-case nil (upcase string) (error string)))

(defun bmkp-same-file-p (file1 file2)
  "Return non-nil if FILE1 and FILE2 name the same file.
If either name is not absolute, then it is considered relative to
`default-directory'."
  (string= (file-truename (expand-file-name file1)) (file-truename (expand-file-name file2))))

(defun bmkp-file-remote-p (file-name)
  "Returns non-nil if string FILE-NAME is likely to name a remote file."
  (if (fboundp 'file-remote-p)
      (file-remote-p file-name)
    (and (fboundp 'ffap-file-remote-p) (ffap-file-remote-p file-name))))

(defun bmkp-float-time (&optional specified-time)
  "Same as `float-time'.  (Needed for Emacs 20.)"
  (if (fboundp 'float-time)
      (float-time specified-time)
    (unless specified-time (setq specified-time  (current-time)))
    (+ (* (float (nth 0 specified-time)) (expt 2 16))  (nth 1 specified-time))))

(defun bmkp-face-prop (value)
  "Return a list with elements `face' or `font-lock-face' and VALUE.
Starting with Emacs 22, the first element is `font-lock-face'."
  (list (if (> emacs-major-version 21) 'font-lock-face 'face) value))  

(defun bmkp-make-plain-predicate (pred &optional final-pred)
  "Return a plain predicate that corresponds to component-predicate PRED.
PRED and FINAL-PRED correspond to their namesakes in
`bmkp-sort-comparer' (which see).

PRED should return `(t)', `(nil)', or nil.

Optional arg FINAL-PRED is the final predicate to use if PRED cannot
decide (returns nil).  If FINAL-PRED is nil, then `bmkp-alpha-p', the
plain-predicate equivalent of `bmkp-alpha-cp' is used as the final
predicate."
  `(lambda (b1 b2) (let ((res  (funcall ',pred b1 b2)))
                     (if res (car res) (funcall ',(or final-pred 'bmkp-alpha-p) b1 b2)))))

(defun bmkp-repeat-command (command)
  "Repeat COMMAND."
  (let ((repeat-previous-repeated-command  command)
        (repeat-message-function           'ignore)
        (last-repeatable-command           'repeat))
    (repeat nil)))


;;; If you need this for some reason, uncomment it.
;;; (defun bmkp-fix-bookmark-alist-and-save ()
;;;   "Update format of `bookmark-default-file' created in summer of 2009.
;;; You DO NOT NEED THIS, unless you happen to have used `bookmark+.el' in
;;; the summer of 2009 to create non-file bookmarks.  If you did that,
;;; then some of those bookmarks might cause vanilla Emacs (emacs -Q) to
;;; raise an error.  You can use this command to fix that problem: it
;;; modifies your existing `bookmark-default-file' (`.emacs.bmk'), after
;;; backing up that file (suffixing the name with \"_saveNUMBER\")."
;;;   (interactive)
;;;   (require 'cl)                         ; For `gensym'
;;;   (if (not (yes-or-no-p
;;;              "This will modify your bookmark file, after backing it up.  OK? "))
;;;       (message "OK, nothing done")
;;;     (bookmark-maybe-load-default-file)
;;;     (let ((bkup-file  (concat bookmark-default-file "_" (symbol-name (gensym "save")))))
;;;       (when (condition-case err
;;;                 (progn
;;;                   (with-current-buffer (find-file-noselect bookmark-default-file)
;;;                     (write-file bkup-file))
;;;                   (dolist (bmk  bookmark-alist)
;;;                     (let ((fn-tail  (member '(filename) bmk))
;;;                           (hdlr     (bookmark-get-handler (car bmk))))
;;;                       (cond (fn-tail
;;;                              (setcar fn-tail (cons 'filename bmkp-non-file-filename)))
;;;                             ((and (eq hdlr 'bmkp-jump-gnus)
;;;                                   (not (assoc 'filename bmk)))
;;;                              (setcdr bmk (cons (cons 'filename bmkp-non-file-filename)
;;;                                                (cdr bmk)))))))
;;;                   t)                    ; Be sure `dolist' exit with t to allow saving.
;;;               (error (error "No changes made. %s" (error-message-string err))))
;;;         (bookmark-save)
;;;         (message "Bookmarks file fixed.  Old version is `%s'" bkup-file)))))


;;(@* "Bookmark Entry Access Functions")
;;  *** Bookmark Entry Access Functions ***

(defun bmkp-get-buffer-name (bookmark)
  "Return the `buffer-name' value for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (bookmark-prop-get bookmark 'buffer-name))

(defun bmkp-get-end-position (bookmark)
  "Return the `end-position' value for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (bookmark-prop-get bookmark 'end-position))

(defun bmkp-get-visits-count (bookmark)
  "Return the `visits' count for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (bookmark-prop-get bookmark 'visits))

(defun bmkp-get-visit-time (bookmark)
  "Return the `time' value for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  ;; Should just be a prop-get, but when first implemented, we used a float
  ;; instead of a time cons, so we need to convert any such obsolete recorded times.
  (let ((vt  (bookmark-prop-get bookmark 'time)))
    (when (numberp vt)                  ; Convert mid-2009 time values (floats) to cons form.
      (setq vt  (if (boundp 'seconds-to-time)
                    (seconds-to-time vt)
                  (list (floor vt 65536) ; Inlined `seconds-to-time', for Emacs 20-21.
                        (floor (mod vt 65536))
                        (floor (* (- vt (ffloor vt)) 1000000))))))
    vt))


;;(@* "Sorting - General Functions")
;;  *** Sorting - General Functions ***

(defun bmkp-sort-and-remove-dups (alist &optional omit)
  "Remove duplicates from a copy of ALIST, then sort it and return it.
Do not sort if `bmkp-sort-comparer' is nil.
Always remove duplicates.  Keep only the first element with a given
key.  This is a non-destructive operation: ALIST is not modified.

Sorting is done using using `bmkp-sort-comparer'.
If `bmkp-reverse-sort-p' is non-nil, then reverse the sort order.
Keys are compared for sorting using `equal'.
If optional arg OMIT is non-nil, then omit from the return value any
elements with keys in list OMIT."
  (let ((new-alist  (bmkp-remove-assoc-dups alist omit))
        (sort-fn  (and bmkp-sort-comparer  (if (and (not (functionp bmkp-sort-comparer))
                                                    (consp bmkp-sort-comparer))
                                               'bmkp-multi-sort
                                             bmkp-sort-comparer))))
    (when sort-fn
      (setq new-alist  (sort new-alist (if bmkp-reverse-sort-p
                                           (lambda (a b) (not (funcall sort-fn a b)))
                                         sort-fn))))
    new-alist))

;;; KEEP this simpler version also.  This uses `run-hook-with-args-until-success', but it
;;; does not respect `bmkp-reverse-multi-sort-p'.
;;; (defun bmkp-multi-sort (b1 b2)
;;;   "Try predicates in `bmkp-sort-comparer', in order, until one decides.
;;; See the description of `bmkp-sort-comparer'."
;;;   (let* ((preds   (append (car bmkp-sort-comparer) (cdr bmkp-sort-comparer)))
;;;          (result  (run-hook-with-args-until-success 'preds b1 b2)))
;;;     (if (consp result)
;;;         (car result)
;;;       result)))

;; This Lisp definition respects `bmkp-reverse-multi-sort-p', and can be extended.
(defun bmkp-multi-sort (b1 b2)
  "Try predicates in `bmkp-sort-comparer', in order, until one decides.
See the description of `bmkp-sort-comparer'.
If `bmkp-reverse-multi-sort-p' is non-nil, then reverse the order for
using multi-sorting predicates."
  (let ((preds       (car bmkp-sort-comparer))
        (final-pred  (cadr bmkp-sort-comparer))
        (result      nil))
    (when bmkp-reverse-multi-sort-p (setq preds  (reverse preds)))
    (catch 'bmkp-multi-sort
      (dolist (pred  preds)
        (setq result  (funcall pred b1 b2))
        (when (consp result)
          (when bmkp-reverse-multi-sort-p (setq result  (list (not (car result)))))
          (throw 'bmkp-multi-sort (car result))))
      (and final-pred  (if bmkp-reverse-multi-sort-p
                           (not (funcall final-pred b1 b2))
                         (funcall final-pred b1 b2))))))

;; The message is only approximate.  The effect of `bmkp-reverse-multi-sort-p' is not
;; always intuitive, but it can often be useful.  What's not always intuitive is the placement
;; (the order) of bookmarks that are not sorted by the PREDs.
;; 
(defun bmkp-msg-about-sort-order (order &optional prefix-msg suffix-msg)
  "Display a message mentioning the current sort ORDER and direction.
Optional arg PREFIX-MSG is prepended to the constructed message, and
terminated with a period.
Similarly, SUFFIX-MSG is appended, after appending \".  \"."
  (let ((msg  (if (not bmkp-sort-comparer)
                  "Bookmarks NOT sorted"
                (format "%s%s" (concat "Sorted " order)
                        (if (not (and (consp bmkp-sort-comparer) ; Ordinary single predicate.
                                      (consp (car bmkp-sort-comparer))))
                            (if bmkp-reverse-sort-p "; REVERSED" "")
                          (if (not (cadr (car bmkp-sort-comparer)))
                              ;; Single PRED.
                              (if (or (and bmkp-reverse-sort-p (not bmkp-reverse-multi-sort-p))
                                      (and bmkp-reverse-multi-sort-p (not bmkp-reverse-sort-p)))
                                  "; REVERSED"
                                "")

                            ;; In case we want to distinguish:
                            ;; (if (and bmkp-reverse-sort-p (not bmkp-reverse-multi-sort-p))
                            ;;     "; reversed"
                            ;;   (if (and bmkp-reverse-multi-sort-p (not bmkp-reverse-sort-p))
                            ;;       "; reversed +"
                            ;;     ""))

                            ;; At least two PREDs.
                            (cond ((and bmkp-reverse-sort-p (not bmkp-reverse-multi-sort-p))
                                   "; REVERSED")
                                  ((and bmkp-reverse-multi-sort-p (not bmkp-reverse-sort-p))
                                   "; each predicate group reversed")
                                  ((and bmkp-reverse-multi-sort-p bmkp-reverse-sort-p)
                                   "; order of predicate groups reversed")
                                  (t ""))))))))
    (when prefix-msg (setq msg  (concat prefix-msg ".  " msg)))
    (when suffix-msg (setq msg  (concat msg ".  " suffix-msg)))
    (message msg)))


;;(@* "Sorting - Commands")
;;  *** Sorting - Commands ***

;;;###autoload
(defun bmkp-bmenu-change-sort-order-repeat (arg) ; Bound to `s s'... in bookmark list
  "Cycle to the next sort order.
With a prefix arg, reverse current sort order.
This is a repeatable version of `bmkp-bmenu-change-sort-order'."
  (interactive "P")
  (require 'repeat)
  (bmkp-repeat-command 'bmkp-bmenu-change-sort-order))

;;;###autoload
(defun bmkp-bmenu-change-sort-order (&optional arg)
  "Cycle to the next sort order.
With a prefix arg, reverse the current sort order."
  (interactive "P")
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-sort-orders-for-cycling-alist  (delq nil bmkp-sort-orders-for-cycling-alist))
  (if arg
      (bmkp-reverse-sort-order)
    (let ((current-bmk  (bookmark-bmenu-bookmark))
          next-order)
      (let ((orders  (mapcar #'car bmkp-sort-orders-for-cycling-alist)))
        (setq next-order          (or (cadr (member (bmkp-current-sort-order) orders))  (car orders))
              bmkp-sort-comparer  (cdr (assoc next-order bmkp-sort-orders-for-cycling-alist))))
      (message "Sorting...")
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (bmkp-bmenu-goto-bookmark-named current-bmk) ; Put cursor back on the right line.
      (when (interactive-p) (bmkp-msg-about-sort-order next-order)))))

(defun bmkp-current-sort-order ()
  "Current sort order or sort function, as a string suitable in a message."
  (or (car (rassoc bmkp-sort-comparer bmkp-sort-orders-alist))  (format "%s" bmkp-sort-comparer)))

;;;###autoload
(defun bmkp-reverse-sort-order ()       ; Bound to `s r' in bookmark list
  "Reverse the current bookmark sort order.
If you combine this with \\<bookmark-bmenu-mode-map>\
`\\[bmkp-reverse-multi-sort-order]', then see the doc for that command."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-reverse-sort-p  (not bmkp-reverse-sort-p))
  (let ((current-bmk  (bookmark-bmenu-bookmark)))
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (bmkp-bmenu-goto-bookmark-named current-bmk)) ; Put cursor back on the right line.
  (when (interactive-p) (bmkp-msg-about-sort-order (bmkp-current-sort-order))))

;;;###autoload
(defun bmkp-reverse-multi-sort-order () ; Bound to `s C-r' in bookmark list
  "Reverse the application of multi-sorting predicates.
These are the PRED predicates described for option
`bmkp-sort-comparer'.

This reverses the order in which the predicates are tried, and it
also complements the truth value returned by each predicate.

For example, if the list of multi-sorting predicates is (p1 p2 p3),
then the predicates are tried in the order: p3, p2, p1.  And if a
predicate returns true, `(t)', then the effect is as if it returned
false, `(nil)', and vice versa.

The use of multi-sorting predicates tends to group bookmarks, with the
first predicate corresponding to the first bookmark group etc.

The effect of \\<bookmark-bmenu-mode-map>`\\[bmkp-reverse-multi-sort-order]' is \
roughly as follows:

 - without also `\\[bmkp-reverse-sort-order]', it reverses the bookmark order in each \
group

 - combined with `\\[bmkp-reverse-sort-order]', it reverses the order of the bookmark
   groups, but not the bookmarks within a group

This is a rough description.  The actual behavior can be complex,
because of how each predicate is defined.  If this description helps
you, fine.  If not, just experiment and see what happens. \;-)

Remember that ordinary `\\[bmkp-reverse-sort-order]' reversal on its own is \
straightforward.
If you find `\\[bmkp-reverse-multi-sort-order]' confusing or not helpful, then do not \
use it."
  (interactive)
  (bmkp-barf-if-not-in-menu-list)
  (setq bmkp-reverse-multi-sort-p  (not bmkp-reverse-multi-sort-p))
  (let ((current-bmk  (bookmark-bmenu-bookmark)))
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (bmkp-bmenu-goto-bookmark-named current-bmk)) ; Put cursor back on the right line.
  (when (interactive-p) (bmkp-msg-about-sort-order (bmkp-current-sort-order))))


;; The ORDER of the macro calls here defines the REVERSE ORDER of
;; `bmkp-sort-orders-alist'.  The first here is thus also the DEFAULT sort order.
;; Entries are traversed by `s s'..., in `bmkp-sort-orders-alist' order.

(bmkp-define-sort-command               ; Bound to `s k' in bookmark list (`k' for "kind")
 "by bookmark type"                     ; `bmkp-bmenu-sort-by-bookmark-type'
 ((bmkp-info-cp bmkp-gnus-cp bmkp-w3m-cp bmkp-local-file-type-cp bmkp-handler-cp)
  bmkp-alpha-p)
 "Sort bookmarks by type: Info, Gnus, W3M, files, other.")

(bmkp-define-sort-command               ; Bound to `s w' in bookmark list
 "by w3m url"                           ; `bmkp-bmenu-sort-by-w3m-url'
 ((bmkp-w3m-cp) bmkp-alpha-p)
 "Sort W3M bookmarks alphabetically by their URL/filename.
When two bookmarks are not comparable this way, compare them by
bookmark name.")

(bmkp-define-sort-command               ; Bound to `s g' in bookmark list
 "by Gnus thread"                       ; `bmkp-bmenu-sort-by-Gnus-thread'
 ((bmkp-gnus-cp) bmkp-alpha-p)
 "Sort Gnus bookmarks by group, then by article, then by message.
When two bookmarks are not comparable this way, compare them by
bookmark name.")

(bmkp-define-sort-command               ; Bound to `s i' in bookmark list
 "by Info location"                     ; `bmkp-bmenu-sort-by-Info-location'
 ((bmkp-info-cp) bmkp-alpha-p)
 "Sort Info bookmarks by file name, then node name, then position.
When two bookmarks are not comparable this way, compare them by
bookmark name.")

(bmkp-define-sort-command               ; Bound to `s f u' in bookmark list
 "by last local file update"            ; `bmkp-bmenu-sort-by-last-local-file-update'
 ((bmkp-local-file-updated-more-recently-cp) bmkp-alpha-p)
 "Sort bookmarks by last local file update time.
Sort a local file before a remote file, and a remote file before other
bookmarks.  Otherwise, sort by bookmark name.")

(bmkp-define-sort-command               ; Bound to `s f t' in bookmark list
 "by last local file access"            ; `bmkp-bmenu-sort-by-last-local-file-access'
 ((bmkp-local-file-accessed-more-recently-cp) bmkp-alpha-p)
 "Sort bookmarks by last local file access time.
A local file sorts before a remote file, which sorts before other
bookmarks.  Otherwise, sort by bookmark name.")

(bmkp-define-sort-command               ; Bound to `s f s' in bookmark list
 "by local file size"                   ; `bmkp-bmenu-sort-by-local-file-size'
 ((bmkp-local-file-size-cp) bmkp-alpha-p)
 "Sort bookmarks by local file size.
A local file sorts before a remote file, which sorts before other
bookmarks.  Otherwise, sort by bookmark name.")

(bmkp-define-sort-command               ; Bound to `s f n' in bookmark list
 "by file name"                         ; `bmkp-bmenu-sort-by-file-name'
 ((bmkp-file-alpha-cp) bmkp-alpha-p)
 "Sort bookmarks by file name.
When two bookmarks are not comparable by file name, compare them by
bookmark name.")

(bmkp-define-sort-command               ; Bound to `s f d' in bookmark list (`d' for "directory")
 "by local file type"                   ; `bmkp-bmenu-sort-by-local-file-type'
 ((bmkp-local-file-type-cp) bmkp-alpha-p)
 "Sort bookmarks by local file type: file, symlink, directory.
A local file sorts before a remote file, which sorts before other
bookmarks.  Otherwise, sort by bookmark name.")

(bmkp-define-sort-command               ; Bound to `s >' in bookmark list
 "marked before unmarked"               ; `bmkp-bmenu-sort-marked-before-unmarked'
 ((bmkp-marked-cp) bmkp-alpha-p)
 "Sort bookmarks by putting marked before unmarked.
Otherwise alphabetize by bookmark name.")

(bmkp-define-sort-command               ; Bound to `s b' in bookmark list
 "by last buffer or file access"        ; `bmkp-bmenu-sort-by-last-buffer-or-file-access'
 ((bmkp-buffer-last-access-cp bmkp-local-file-accessed-more-recently-cp)
  bmkp-alpha-p)
 "Sort bookmarks by last buffer access or last local file access.
Sort a bookmark accessed more recently before one accessed less
recently or not accessed.  Sort a bookmark to an existing buffer
before a local file bookmark.  When two bookmarks are not comparable
by such critera, sort them by bookmark name.  (In particular, sort
remote-file bookmarks by bookmark name.")

(bmkp-define-sort-command               ; Bound to `s v' in bookmark list
 "by bookmark visit frequency"          ; `bmkp-bmenu-sort-by-bookmark-visit-frequency'
 ((bmkp-visited-more-cp) bmkp-alpha-p)
 "Sort bookmarks by the number of times they were visited as bookmarks.
When two bookmarks are not comparable by visit frequency, compare them
by bookmark name.")

(bmkp-define-sort-command               ; Bound to `s t' in bookmark list
 "by last bookmark access"              ; `bmkp-bmenu-sort-by-last-bookmark-access'
 ((bmkp-bookmark-last-access-cp) bmkp-alpha-p)
 "Sort bookmarks by the time of their last visit as bookmarks.
When two bookmarks are not comparable by visit time, compare them
by bookmark name.")

(bmkp-define-sort-command               ; Bound to `s 0' in bookmark list
 "by creation time"                     ; `bmkp-bmenu-sort-by-creation-time'
 ((bmkp-bookmark-creation-cp) bmkp-alpha-p)
 "Sort bookmarks by the time of their creation.
When one or both of the bookmarks does not have a `created' entry),
compare them by bookmark name.")

(bmkp-define-sort-command               ; Bound to `s n' in bookmark list
 "by bookmark name"                     ; `bmkp-bmenu-sort-by-bookmark-name'
 bmkp-alpha-p
 "Sort bookmarks by bookmark name, respecting `case-fold-search'.")


;; These definitions MUST COME AFTER the calls to macro `bmkp-define-sort-command'.
;; Otherwise, they won't pick up a populated `bmkp-sort-orders-alist'.
;;;###autoload
(when (> emacs-major-version 20)
  (defcustom bmkp-sort-orders-for-cycling-alist (copy-sequence bmkp-sort-orders-alist)
    "*Alist of sort orders used for cycling via `s s'...
This is a subset of the complete list of available sort orders,
`bmkp-sort-orders-alist'.  This lets you cycle among fewer sort
orders, if there are some that you do not use often.

See the doc for `bmkp-sort-orders-alist', for the structure of
this value."
    :type '(alist
            :key-type (choice :tag "Sort order" string symbol)
            :value-type (choice
                         (const :tag "None (do not sort)" nil)
                         (function :tag "Sorting Predicate")
                         (list :tag "Sorting Multi-Predicate"
                          (repeat (function :tag "Component Predicate"))
                          (choice
                           (const :tag "None" nil)
                           (function :tag "Final Predicate")))))
    :group 'bookmark-plus))

;;;###autoload
(unless (> emacs-major-version 20)      ; Emacs 20: custom type `alist' doesn't exist.
  (defcustom bmkp-sort-orders-for-cycling-alist (copy-sequence bmkp-sort-orders-alist)
    "*Alist of sort orders used for cycling via `s s'...
This is a subset of the complete list of available sort orders,
`bmkp-sort-orders-alist'.  This lets you cycle among fewer sort
orders, if there are some that you do not use often.

See the doc for `bmkp-sort-orders-alist', for the structure of this
value."
    :type '(repeat
            (cons
             (choice :tag "Sort order" string symbol)
             (choice
              (const :tag "None (do not sort)" nil)
              (function :tag "Sorting Predicate")
              (list :tag "Sorting Multi-Predicate"
               (repeat (function :tag "Component Predicate"))
               (choice
                (const :tag "None" nil)
                (function :tag "Final Predicate"))))))
    :group 'bookmark-plus))


;;(@* "Sorting - General Predicates")
;;  *** Sorting - General Predicates ***

(defun bmkp-marked-cp (b1 b2)
  "True if bookmark B1 is marked and bookmark B2 is not.
B1 and B2 are bookmarks or bookmark names.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if incomparable as described."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (let ((m1  (bmkp-marked-bookmark-p b1))
        (m2  (bmkp-marked-bookmark-p b2)))
    (cond ((and m1 m2)  nil)
          (m1           '(t))
          (m2           '(nil))
          (t            nil))))

(defun bmkp-visited-more-cp (b1 b2)
  "True if bookmark B1 was visited more often than B2.
B1 and B2 are bookmarks or bookmark names.
True also if B1 was visited but B2 was not.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if incomparable as described."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (let ((v1  (bmkp-get-visits-count b1))
        (v2  (bmkp-get-visits-count b2)))
    (cond ((and v1 v2)
           (cond ((> v1 v2)  '(t))
                 ((> v2 v1)  '(nil))
                 (t          nil)))
          (v1                '(t))
          (v2                '(nil))
          (t                 nil))))

(defun bmkp-bookmark-creation-cp (b1 b2)
  "True if bookmark B1 was created more recently than B2.
B1 and B2 are bookmarks or bookmark names.
True also if B1 has a `created' entry but B2 has none.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if incomparable as described."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (let ((t1  (bookmark-prop-get b1 'created))
        (t2  (bookmark-prop-get b2 'created)))
    (cond ((and t1 t2)
           (setq t1  (bmkp-float-time t1)
                 t2  (bmkp-float-time t2))
           (cond ((> t1 t2)  '(t))
                 ((> t2 t1)  '(nil))
                 (t          nil)))
          (t1                '(t))
          (t2                '(nil))
          (t                 nil))))

;; Not used currently.
(defun bmkp-same-creation-time-p (b1 b2)
  "Return non-nil if `B1 and B2 have same `created' entry.
B1 and B2 are bookmarks or bookmark names.
If neither has a `created' entry (vanilla bookmarks), then return
non-nil if the full bookmarks are `equal'."
  (let ((time1  (bookmark-prop-get b1 'created))
        (time2  (bookmark-prop-get b2 'created)))
    (if (or time1 time2)
        (equal time1 time2)
      (equal b1 b2))))

(defun bmkp-bookmark-last-access-cp (b1 b2)
  "True if bookmark B1 was visited more recently than B2.
B1 and B2 are bookmarks or bookmark names.
True also if B1 was visited but B2 was not.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if incomparable as described."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (let ((t1  (bmkp-get-visit-time b1))
        (t2  (bmkp-get-visit-time b2)))
    (cond ((and t1 t2)
           (setq t1  (bmkp-float-time t1)
                 t2  (bmkp-float-time t2))
           (cond ((> t1 t2)  '(t))
                 ((> t2 t1)  '(nil))
                 (t          nil)))
          (t1                '(t))
          (t2                '(nil))
          (t                 nil))))

(defun bmkp-buffer-last-access-cp (b1 b2)
  "True if bookmark B1's buffer or file was visited more recently than B2's.
B1 and B2 are bookmarks or bookmark names.
A bookmark to an existing buffer sorts before a file bookmark, even if
the buffer has not been visited during this session.

True also if B1 has a buffer but B2 does not.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if incomparable as described."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (let ((buf1  (bmkp-get-buffer-name b1))
        (buf2  (bmkp-get-buffer-name b2))
        f1 f2 t1 t2)
    (setq buf1  (and buf1 (get-buffer buf1))
          buf2  (and buf2 (get-buffer buf2)))
    (cond ((and buf1 buf2)              ; Both buffers exist.   See whether they were accessed.
           (when buf1 (setq buf1  (member buf1 (buffer-list))
                            buf1  (length buf1)))
           (when buf2 (setq buf2  (member buf2 (buffer-list))
                            buf2  (length buf2)))
           (cond ((and buf1 buf2)       ; Both were accessed.  Priority to most recent access.
                  (cond ((< buf1 buf2)  '(t))
                        ((< buf2 buf1)  '(nil))
                        (t              nil)))
                 (buf1                  '(t)) ; Only buf1 was accessed.
                 (buf2                  '(nil)) ; Only buf2 was accessed.
                 (t                     nil))) ; Neither was accessed.

          (buf1                         '(t)) ; Only buf1 exists.
          (buf2                         '(nil)) ; Only buf2 exists.
          (t                            nil)))) ; Neither buffer exists

(defun bmkp-handler-cp (b1 b2)
  "True if bookmark B1's handler name sorts alphabetically before B2's.
B1 and B2 are bookmarks or bookmark names.
Two bookmarks with handlers are compared alphabetically, by their
handler-function names, respecting `case-fold-search'.
True also if B1 has a handler but B2 has not.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (let ((h1  (bookmark-get-handler b1))
        (h2  (bookmark-get-handler b2)))
    (cond ((and h1 h2 (symbolp h1) (symbolp h2))
           ;; Pretend woman bookmarks are man bookmarks, to keep them together.
           (when (eq h1 'bmkp-jump-woman) (setq h1  'bmkp-jump-man))
           (when (eq h2 'bmkp-jump-woman) (setq h2  'bmkp-jump-man))
           (setq h1  (symbol-name h1)
                 h2  (symbol-name h2))
           (when case-fold-search (setq h1  (bmkp-upcase h1)
                                        h2  (bmkp-upcase h2)))
           (cond ((string-lessp h1 h2)  '(t))
                 ((string-lessp h2 h1)  '(nil))
                 (t                     nil)))
          (h1                           '(t))
          (h2                           '(nil))
          (t                            nil))))

(defun bmkp-info-cp (b1 b2)
  "True if bookmark B1 sorts as an Info bookmark before B2.
B1 and B2 are bookmarks or bookmark names.
Two Info bookmarks are compared first by file name (corresponding to
the manual), then by node name, then by position.
True also if B1 is an Info bookmark but B2 is not.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (let ((i1  (bmkp-info-bookmark-p b1))
        (i2  (bmkp-info-bookmark-p b2)))
    (cond ((and i1 i2)
           (setq i1  (abbreviate-file-name (bookmark-get-filename b1))
                 i2  (abbreviate-file-name (bookmark-get-filename b2)))
           (when case-fold-search (setq i1  (bmkp-upcase i1)
                                        i2  (bmkp-upcase i2)))
           (cond ((string-lessp i1 i2)                  '(t)) ; Compare manuals (file names).
                 ((string-lessp i2 i1)                  '(nil))
                 (t                     ; Compare node names.
                  (setq i1  (bookmark-prop-get b1 'info-node)
                        i2  (bookmark-prop-get b2 'info-node))
                  (cond ((string-lessp i1 i2)           '(t))
                        ((string-lessp i2 i1)           '(nil))
                        (t
                         (setq i1  (bookmark-get-position b1)
                               i2  (bookmark-get-position b2))
                         (cond ((or (not i1) (not i2))  '(t)) ; Fallback if no `position' entry.
                               ((<= i1 i2)              '(t))
                               ((< i2 i1)               '(nil))))))))
          (i1                                           '(t))
          (i2                                           '(nil))
          (t                                            nil))))

(defun bmkp-gnus-cp (b1 b2)
  "True if bookmark B1 sorts as a Gnus bookmark before B2.
B1 and B2 are bookmarks or bookmark names.
Two Gnus bookmarks are compared first by Gnus group name, then by
article number, then by message ID.
True also if B1 is a Gnus bookmark but B2 is not.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (let ((g1  (bmkp-gnus-bookmark-p b1))
        (g2  (bmkp-gnus-bookmark-p b2)))
    (cond ((and g1 g2)
           (setq g1  (bookmark-prop-get b1 'group)
                 g2  (bookmark-prop-get b2 'group))
           (cond ((string-lessp g1 g2)                '(t)) ; Compare groups.
                 ((string-lessp g2 g1)                '(nil))
                 (t                     ; Compare article numbers.
                  (setq g1  (bookmark-prop-get b1 'article)
                        g2  (bookmark-prop-get b2 'article))
                  (cond ((< g1 g2)                    '(t))
                        ((< g2 g1)                    '(nil))
                        (t
                         (setq g1  (bookmark-prop-get b1 'message-id)
                               g2  (bookmark-prop-get b2 'message-id))
                         (cond ((string-lessp g1 g2)  '(t)) ; Compare message IDs.
                               ((string-lessp g2 g1)  '(nil))
                               (t                     nil)))))))   
          (g1                                         '(t))
          (g2                                         '(nil))
          (t                                          nil))))

(defun bmkp-w3m-cp (b1 b2)
  "True if bookmark B1 sorts as a W3M URL bookmark before B2.
B1 and B2 are bookmarks or bookmark names.
Two W3M URL bookmarks are compared alphabetically, by their URLs.
True also if B1 is a W3M bookmark but B2 is not.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (let ((w1  (bmkp-w3m-bookmark-p b1))
        (w2  (bmkp-w3m-bookmark-p b2)))
    (cond ((and w1 w2)
           (setq w1  (bookmark-get-filename b1)
                 w2  (bookmark-get-filename b2))
           (cond ((string-lessp w1 w2)  '(t))
                 ((string-lessp w2 w1)  '(nil))
                 (t                     nil)))
          (w1                           '(t))
          (w2                           '(nil))
          (t                            nil))))

(defun bmkp-position-cp (b1 b2)
  "True if the `position' of B1 is not greater than that of B2.
B1 and B2 are bookmarks or bookmark names.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if B1 and B2 do not bookmark the same buffer or they have
the same `position' value."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (let ((buf1  (bmkp-get-buffer-name b1))
        (buf2  (bmkp-get-buffer-name b2)))
    (and buf1 buf2 (equal buf1 buf2)
         (let ((i1  (bookmark-get-position b1))
               (i2  (bookmark-get-position b2)))
           (cond ((or (not i1) (not i2))  '(t)) ; Fallback if no `position' entry.
                 ((<= i1 i2)              '(t))
                 ((< i2 i1)               '(nil)))))))

(defun bmkp-alpha-cp (b1 b2)
  "True if bookmark B1's name sorts alphabetically before B2's.
B1 and B2 are bookmarks or bookmark names.
The bookmark names are compared, respecting `case-fold-search'.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (let ((s1  (car b1))
        (s2  (car b2)))
    (when case-fold-search (setq s1  (bmkp-upcase s1)
                                 s2  (bmkp-upcase s2)))
    (cond ((string-lessp s1 s2)  '(t))
          ((string-lessp s2 s1)  '(nil))
          (t                     nil))))

;; Do not use `bmkp-make-plain-predicate', because it falls back on `bookmark-alpha-p'.
;; Return nil if `bookmark-alpha-cp' cannot decide.
(defun bmkp-alpha-p (b1 b2)
  "True if bookmark B1's name sorts alphabetically before B2's.
B1 and B2 are bookmarks or bookmark names.
The bookmark names are compared, respecting `case-fold-search'."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (car (bmkp-alpha-cp b1 b2)))


;;(@* "Sorting - File-Name Predicates")
;;  *** Sorting - File-Name Predicates ***

(defun bmkp-file-alpha-cp (b1 b2)
  "True if bookmark B1's file name sorts alphabetically before B2's.
B1 and B2 are bookmarks or bookmark names.
The file names are shortened using `abbreviate-file-name', then they
are compared respecting `case-fold-search'.

Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (let ((f1  (bmkp-file-bookmark-p b1))
        (f2  (bmkp-file-bookmark-p b2)))
    (cond ((and f1 f2)
           ;; Call `abbreviate-file-name' mainly to get letter case right per platform.
           (setq f1  (abbreviate-file-name (bookmark-get-filename b1))
                 f2  (abbreviate-file-name (bookmark-get-filename b2)))
           (when case-fold-search (setq f1  (bmkp-upcase f1)
                                        f2  (bmkp-upcase f2)))
           (cond ((string-lessp f1 f2)  '(t))
                 ((string-lessp f2 f1)  '(nil))
                 (t                     nil)))
          (f1                           '(t))
          (f2                           '(nil))
          (t                            nil))))

;; We define all file-attribute predicates, in case you want to use them.
;;
;; `bmkp-file-attribute-0-cp'  - type
;; `bmkp-file-attribute-1-cp'  - links
;; `bmkp-file-attribute-2-cp'  - uid
;; `bmkp-file-attribute-3-cp'  - gid
;; `bmkp-file-attribute-4-cp'  - last access time
;; `bmkp-file-attribute-5-cp'  - last update time
;; `bmkp-file-attribute-6-cp'  - last status change
;; `bmkp-file-attribute-7-cp'  - size
;; `bmkp-file-attribute-8-cp'  - modes
;; `bmkp-file-attribute-9-cp'  - gid change
;; `bmkp-file-attribute-10-cp' - inode
;; `bmkp-file-attribute-11-cp' - device
;;
(bmkp-define-file-sort-predicate 0) ; Type: file, symlink, dir
(bmkp-define-file-sort-predicate 1) ; Links
(bmkp-define-file-sort-predicate 2) ; Uid
(bmkp-define-file-sort-predicate 3) ; Gid
(bmkp-define-file-sort-predicate 4) ; Last access time
(bmkp-define-file-sort-predicate 5) ; Last modification time
(bmkp-define-file-sort-predicate 6) ; Last status-change time
(bmkp-define-file-sort-predicate 7) ; Size
(bmkp-define-file-sort-predicate 8) ; Modes
(bmkp-define-file-sort-predicate 9) ; Gid would change if re-created
(bmkp-define-file-sort-predicate 10) ; Inode
(bmkp-define-file-sort-predicate 11) ; Device

(defun bmkp-local-file-accessed-more-recently-cp (b1 b2)
  "True if bookmark B1's local file was accessed more recently than B2's.
B1 and B2 are bookmarks or bookmark names.
A local file sorts before a remote file, which sorts before other
bookmarks.  Two remote files are considered incomparable - their
access times are not examined.

Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (cond ((and (bmkp-local-file-bookmark-p b1) (bmkp-local-file-bookmark-p b2))
         (bmkp-cp-not (bmkp-file-attribute-4-cp b1 b2)))
        ((bmkp-local-file-bookmark-p b1)         '(t))
        ((bmkp-local-file-bookmark-p b2)         '(nil))
        ((and (bmkp-remote-file-bookmark-p b1)
              (bmkp-remote-file-bookmark-p b2))  nil)
        ((bmkp-remote-file-bookmark-p b1)        '(t))
        ((bmkp-remote-file-bookmark-p b2)        '(nil))
        (t                                       nil)))

(defun bmkp-local-file-updated-more-recently-cp (b1 b2)
  "True if bookmark B1's local file was updated more recently than B2's.
B1 and B2 are bookmarks or bookmark names.
A local file sorts before a remote file, which sorts before other
bookmarks.  Two remote files are considered incomparable - their
update times are not examined.

Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (cond ((and (bmkp-local-file-bookmark-p b1) (bmkp-local-file-bookmark-p b2))
         (bmkp-cp-not (bmkp-file-attribute-5-cp b1 b2)))
        ((bmkp-local-file-bookmark-p b1)         '(t))
        ((bmkp-local-file-bookmark-p b2)         '(nil))
        ((and (bmkp-remote-file-bookmark-p b1)
              (bmkp-remote-file-bookmark-p b2))  nil)
        ((bmkp-remote-file-bookmark-p b1)        '(t))
        ((bmkp-remote-file-bookmark-p b2)        '(nil))
        (t                                       nil)))

(defun bmkp-local-file-size-cp (b1 b2)
  "True if bookmark B1's local file is larger than B2's.
B1 and B2 are bookmarks or bookmark names.
A local file sorts before a remote file, which sorts before other
bookmarks.  Two remote files are considered incomparable - their
sizes are not examined.

Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (cond ((and (bmkp-local-file-bookmark-p b1) (bmkp-local-file-bookmark-p b2))
         (bmkp-cp-not (bmkp-file-attribute-7-cp b1 b2)))
        ((bmkp-local-file-bookmark-p b1)         '(t))
        ((bmkp-local-file-bookmark-p b2)         '(nil))
        ((and (bmkp-remote-file-bookmark-p b1)
              (bmkp-remote-file-bookmark-p b2))  nil)
        ((bmkp-remote-file-bookmark-p b1)        '(t))
        ((bmkp-remote-file-bookmark-p b2)        '(nil))
        (t                                       nil)))

(defun bmkp-local-file-type-cp (b1 b2)
  "True if bookmark B1 sorts by local file type before B2.
B1 and B2 are bookmarks or bookmark names.
For two local files, a file sorts before a symlink, which sorts before
a directory.

A local file sorts before a remote file, which sorts before other
bookmarks.  Two remote files are considered incomparable - their file
types are not examined.

Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (setq b1  (bookmark-get-bookmark b1)
        b2  (bookmark-get-bookmark b2))
  (cond ((and (bmkp-local-file-bookmark-p b1) (bmkp-local-file-bookmark-p b2))
         (bmkp-file-attribute-0-cp b1 b2))
        ((bmkp-local-file-bookmark-p b1)         '(t))
        ((bmkp-local-file-bookmark-p b2)         '(nil))
        ((and (bmkp-remote-file-bookmark-p b1)
              (bmkp-remote-file-bookmark-p b2))  nil)
        ((bmkp-remote-file-bookmark-p b1)        '(t))
        ((bmkp-remote-file-bookmark-p b2)        '(nil))
        (t                                       nil)))

(defun bmkp-cp-not (truth)
  "Return the negation of boolean value TRUTH.
If TRUTH is (t), return (nil), and vice versa.
If TRUTH is nil, return nil."
  (and truth (if (car truth) '(nil) '(t))))


;;(@* "Other Bookmark+ Functions (`bmkp-*')")
;;  *** Other Bookmark+ Functions (`bmkp-*') ***

;;;###autoload
(defun bmkp-bmenu-describe-this+move-down (&optional defn) ; Bound to `C-down' in bookmark list
  "Describe bookmark of current line, then move down to the next bookmark.
With a prefix argument, show the internal definition of the bookmark."
  (interactive "P")
  (bmkp-bmenu-describe-this-bookmark) (forward-line 1))

;;;###autoload
(defun bmkp-bmenu-describe-this+move-up (&optional defn) ; Bound to `C-up' in bookmark list
  "Describe bookmark of current line, then move down to the next bookmark.
With a prefix argument, show the internal definition of the bookmark."
  (interactive "P")
  (bmkp-bmenu-describe-this-bookmark) (forward-line -1))

;;;###autoload
(defun bmkp-bmenu-describe-this-bookmark (&optional defn) ; Bound to `C-h RET' in bookmark list
  "Describe bookmark of current line.
With a prefix argument, show the internal definition of the bookmark."
  (interactive "P")
  (bmkp-barf-if-not-in-menu-list)
  (if defn
      (bmkp-describe-bookmark-internals (bookmark-bmenu-bookmark))
    (bmkp-describe-bookmark (bookmark-bmenu-bookmark))))

;;;###autoload
(defun bmkp-bmenu-describe-marked (&optional defn) ; Bound to `C-h >' in bookmark list
  "Describe the marked bookmarks.
With a prefix argument, show the internal definitions."
  (interactive "P")
  (bmkp-barf-if-not-in-menu-list)
  (help-setup-xref (list #'bmkp-describe-bookmark-marked) (interactive-p))
  (with-output-to-temp-buffer "*Help*"
    (dolist (bmk  (bmkp-marked-bookmarks-only))
      (if defn
          (let* ((bname      (bookmark-name-from-full-record bmk))
                 (help-text  (format "%s\n%s\n\n%s" bname (make-string (length bname) ?-)
                                     (pp-to-string bmk))))
            (princ help-text) (terpri))
        (princ (bmkp-bookmark-description bmk)) (terpri)))))

;;;###autoload
(defun bmkp-describe-bookmark (bookmark &optional defn)
  "Describe BOOKMARK.
With a prefix argument, show the internal definition of the bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (interactive (list (bookmark-completing-read "Describe bookmark" (bmkp-default-bookmark-name))
                     current-prefix-arg))
  (if defn
      (bmkp-describe-bookmark-internals bookmark)
    (setq bookmark     (bookmark-get-bookmark bookmark))
    (help-setup-xref (list #'bmkp-describe-bookmark bookmark) (interactive-p))
    (let ((help-text  (bmkp-bookmark-description bookmark)))
      (with-output-to-temp-buffer "*Help*" (princ help-text))
      help-text)))

(defun bmkp-bookmark-description (bookmark)
  "Help-text description of BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (setq bookmark  (bookmark-get-bookmark bookmark))
  (let ((bname            (bookmark-name-from-full-record bookmark))
        (buf              (bmkp-get-buffer-name bookmark))
        (file             (bookmark-get-filename bookmark))
        (start            (bookmark-get-position bookmark))
        (end              (bmkp-get-end-position bookmark))
        (created          (bookmark-prop-get bookmark 'created))
        (time             (bmkp-get-visit-time bookmark))
        (visits           (bmkp-get-visits-count bookmark))
        (tags             (mapcar #'bmkp-tag-name (bmkp-get-tags bookmark)))
        (sequence-p       (bmkp-sequence-bookmark-p bookmark))
        (function-p       (bmkp-function-bookmark-p bookmark))
        (varlist-p        (bmkp-varlist-bookmark-p bookmark))
        (desktop-p        (bmkp-desktop-bookmark-p bookmark))
        (bookmark-file-p  (bmkp-bookmark-file-bookmark-p bookmark))
        (dired-p          (bmkp-dired-bookmark-p bookmark))
        (gnus-p           (bmkp-gnus-bookmark-p bookmark))
        (info-p           (bmkp-info-bookmark-p bookmark))
        (man-p            (bmkp-man-bookmark-p bookmark))
        (w3m-p            (bmkp-w3m-bookmark-p bookmark))
        (annot            (bookmark-get-annotation bookmark))
        no-position-p)
    (when (or sequence-p function-p varlist-p) (setq no-position-p  t))
    (let* ((help-text
            (concat
             (format "%s\n%s\n\n" bname (make-string (length bname) ?-))
             (cond (sequence-p       (format "Sequence:\n%s\n"
                                             (pp-to-string
                                              (bookmark-prop-get bookmark 'sequence))))
                   (function-p       (let ((fn  (bookmark-prop-get bookmark 'function)))
                                       (if (symbolp fn)
                                           (format "Function:\t\t%s\n" fn)
                                         (format "Function:\n%s\n"
                                                 (pp-to-string
                                                  (bookmark-prop-get bookmark 'function))))))
                   (varlist-p        (format "Variable list:\n%s\n"
                                             (pp-to-string (bookmark-prop-get bookmark 'variables))))
                   (gnus-p           (format "Gnus, group:\t\t%s, article: %s, message-id: %s\n"
                                             (bookmark-prop-get bookmark 'group)
                                             (bookmark-prop-get bookmark 'article)
                                             (bookmark-prop-get bookmark 'message-id)))
                   (man-p            (format "UNIX `man' page for:\t`%s'\n"
                                             (or (bookmark-prop-get bookmark 'man-args)
                                                 ;; WoMan has no variable for the cmd name.
                                                 (bookmark-prop-get bookmark 'buffer-name))))
                   (info-p           (format "Info node:\t\t(%s) %s\n"
                                             (file-name-nondirectory file)
                                             (bookmark-prop-get bookmark 'info-node)))
                   (w3m-p            (format "W3M URL:\t\t%s\n" file))
                   (desktop-p        (format "Desktop file:\t\t%s\n"
                                             (bookmark-prop-get bookmark 'desktop-file)))
                   (bookmark-file-p  (format "Bookmark file:\t\t%s\n"
                                             (bookmark-prop-get bookmark 'bookmark-file)))
                   (dired-p          (let ((switches  (bookmark-prop-get bookmark 'dired-switches))
                                           (marked    (length (bookmark-prop-get bookmark
                                                                                 'dired-marked)))
                                           (inserted  (length (bookmark-prop-get bookmark
                                                                                 'dired-subdirs)))
                                           (hidden    (length
                                                       (bookmark-prop-get bookmark
                                                                          'dired-hidden-dirs))))
                                       (format "Dired%s:%s\t\t%s\nMarked:\t\t\t%s\n\
Inserted subdirs:\t%s\nHidden subdirs:\t\t%s\n"
                                               (if switches (format " `%s'" switches) "")
                                               (if switches "" (format "\t"))
                                               (expand-file-name file)
                                               marked inserted hidden)))
                   ((equal file bmkp-non-file-filename)
                    (format "Buffer:\t\t\t%s\n" (bmkp-get-buffer-name bookmark)))
                   (file        (format "File:\t\t\t%s\n" (expand-file-name file)))
                   (t           "Unknown\n"))
             (unless no-position-p
               (if (bmkp-region-bookmark-p bookmark)
                   (format "Region:\t\t\t%d to %d (%d chars)\n" start end (- end start))
                 (format "Position:\t\t%d\n" start)))
             (if visits  (format "Visits:\t\t\t%d\n" visits) "")
             (if time    (format "Last visit:\t\t%s\n" (format-time-string "%c" time)) "")
             (if created (format "Creation:\t\t%s\n" (format-time-string "%c" created)) "")
             (if tags    (format "Tags:\t\t\t%S\n" tags) "")
             (if annot   (format "\nAnnotation:\n%s" annot)))))
      help-text)))

;;;###autoload
(defun bmkp-describe-bookmark-internals (bookmark)
  "Show the internal definition of the bookmark BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (interactive (list (bookmark-completing-read "Describe bookmark" (bmkp-default-bookmark-name))))
  (setq bookmark  (bookmark-get-bookmark bookmark))
  (help-setup-xref (list #'bmkp-describe-bookmark-internals bookmark) (interactive-p))
  (let* ((bname      (bookmark-name-from-full-record bookmark))
         (help-text  (format "%s\n%s\n\n%s" bname (make-string (length bname) ?-)
                             (pp-to-string bookmark))))
    (with-output-to-temp-buffer "*Help*" (princ help-text))
    help-text))

;;;###autoload
(defun bmkp-list-defuns-in-commands-file ()
  "List the functions defined in `bmkp-bmenu-commands-file'.
Typically, these are all commands."
  (interactive)
  (when (and bmkp-bmenu-commands-file (file-readable-p bmkp-bmenu-commands-file))
    (let ((fns  ())
          (buf  (let ((enable-local-variables  nil))
                  (find-file-noselect bmkp-bmenu-commands-file))))
      (help-setup-xref (list #'bmkp-list-defuns-in-commands-file) (interactive-p))
      (with-current-buffer buf
        (goto-char (point-min))
        (while (not (eobp))
          (when (re-search-forward "\\s-*(defun \\([^ \t\n(\"]+\\)[ \t\n(]" nil 'move)
            (push (match-string 1) fns)))
        (setq fns  (nreverse fns)
              fns  (sort fns 'string-lessp)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (with-output-to-temp-buffer "*Help*"
        (princ "Bookmark Commands You Defined (in `bmkp-bmenu-commands-file')") (terpri)
        (princ "------------------------------------------------------------------") (terpri)
        (terpri)
        (let ((non-dups  (bmkp-remove-dups fns)))
          (dolist (fn  non-dups)
            (if (and (fboundp (intern fn)) (fboundp 'help-insert-xref-button))
                (with-current-buffer "*Help*"
                  (goto-char (point-max))
                  (help-insert-xref-button fn 'help-function (intern fn) (commandp (intern fn))))
              (princ fn))
            (let ((dups   (member fn fns)) ; Sorted, so all dups are together.
                  (count  0))
              (while (equal fn (car dups))
                (setq count  (1+ count)
                      dups   (cdr dups)))
              (when (> count 1) (princ (format "  (%d times)" count))))
            (terpri)))
        (help-make-xrefs (current-buffer)))
      fns)))

(defun bmkp-root-or-sudo-logged-p ()
  "Return t if the user logged in using Tramp as `root' or `sudo'.
Otherwise, return nil."
  (catch 'break
    (dolist (ii  (mapcar #'buffer-name (buffer-list)))
      (when (string-match "*tramp/\\(su\\|sudo\\) ." ii) (throw 'break t)))))

(defun bmkp-position-post-context (breg)
  "Return `bookmark-search-size' chars, starting at position BREG.
Return nil if there are not that many chars.
This is text that follows the bookmark's `position'.
This is used for a non-region bookmark."
  (and (>= (- (point-max) breg) bookmark-search-size)
       (buffer-substring-no-properties breg (+ breg bookmark-search-size))))

(defun bmkp-position-post-context-region (breg ereg)
  "Return the region prefix, at BREG.
Return at most `bmkp-region-search-size' or (- EREG BREG) chars.
This is text that follows the bookmark's `position'.
This is used for a region bookmark."
  (buffer-substring-no-properties breg (+ breg (min bmkp-region-search-size (- ereg breg)))))

(defun bmkp-position-pre-context (breg)
  "Return `bookmark-search-size' chars that precede BREG.
Return nil if there are not that many chars.
This is text that precedes the bookmark's `position'.
This is used for a non-region bookmark."
  (and (>= (- breg (point-min)) bookmark-search-size)
       (buffer-substring-no-properties breg (- breg bookmark-search-size))))

(defun bmkp-position-pre-context-region (breg)
  "Return the text preceding the region beginning, BREG.
Return at most `bmkp-region-search-size' chars.
This is text that precedes the bookmark's `position'.
This is used for a region bookmark."
  (buffer-substring-no-properties (max (- breg bmkp-region-search-size) (point-min)) breg))

(defun bmkp-end-position-pre-context (breg ereg)
  "Return the region suffix, ending at EREG.
Return at most `bmkp-region-search-size' or (- EREG BREG) chars.
This is text that precedes the bookmark's `end-position'."
  (buffer-substring-no-properties (- ereg (min bmkp-region-search-size (- ereg breg))) ereg))

(defun bmkp-end-position-post-context (ereg)
  "Return the text following the region end, EREG.
Return at most `bmkp-region-search-size' chars.
This is text that follows the bookmark's `end-position'."
  (buffer-substring-no-properties ereg
                                  (+ ereg (min bmkp-region-search-size (- (point-max) (point))))))

(defun bmkp-position-after-whitespace (position)
  "Move forward from POSITION, skipping over whitespace.  Return point."
  (goto-char position)  (skip-chars-forward " \n\t" (point-max))  (point))

(defun bmkp-position-before-whitespace (position)
  "Move backward from POSITION, skipping over whitespace.  Return point."
  (goto-char position)  (skip-chars-backward " \n\t" (point-min))  (point))

(defun bmkp-save-new-region-location (bookmark beg end)
  "Update and save `bookmark-alist' for BOOKMARK, relocating its region.
BOOKMARK is a bookmark record.
BEG and END are the new region limits for BOOKMARK.
Do nothing and return nil if `bmkp-save-new-location-flag' is nil.
Otherwise, return non-nil if region was relocated."
  (and bmkp-save-new-location-flag
       (y-or-n-p "Region relocated.  Do you want to save new region limits? ")
       (progn
         (bookmark-prop-set bookmark 'front-context-string (bmkp-position-post-context-region
                                                            beg end))
         (bookmark-prop-set bookmark 'rear-context-string (bmkp-position-pre-context-region beg))
         (bookmark-prop-set bookmark 'front-context-region-string (bmkp-end-position-pre-context
                                                                   beg end))
         (bookmark-prop-set bookmark 'rear-context-region-string (bmkp-end-position-post-context end))
         (bookmark-prop-set bookmark 'position beg)
         (bookmark-prop-set bookmark 'end-position end)
         (bmkp-maybe-save-bookmarks)
         t)))

(defun bmkp-handle-region-default (bookmark)
  "Default function to handle BOOKMARK's region.
BOOKMARK is a bookmark name or a bookmark record.
Relocate the region if necessary, then activate it.
If region was relocated, save it if user confirms saving."
  ;; Relocate by searching from the beginning (and possibly the end) of the buffer.
  (let* (;; Get bookmark object once and for all.
         ;; Actually, we know BOOKMARK is a bookmark object (not a name), but play safe.
         (bmk              (bookmark-get-bookmark bookmark))
         (bor-str          (bookmark-get-front-context-string bmk))
         (eor-str          (bookmark-prop-get bmk 'front-context-region-string))
         (br-str           (bookmark-get-rear-context-string bmk))
         (ar-str           (bookmark-prop-get bookmark 'rear-context-region-string))
         (pos              (bookmark-get-position bmk))
         (end-pos          (bmkp-get-end-position bmk))
         (reg-retrieved-p  t)
         (reg-relocated-p  nil))
    (unless (and (string= bor-str (buffer-substring-no-properties (point) (+ (point)
                                                                             (length bor-str))))
                 (save-excursion
                   (goto-char end-pos)
                   (string= eor-str (buffer-substring-no-properties (point) (- (point)
                                                                               (length bor-str))))))
      ;; Relocate region by searching from beginning (and possibly from end) of buffer.
      (let ((beg  nil)
            (end  nil))
        ;;  Go to bob and search forward for END.
        (goto-char (point-min))
        (if (search-forward eor-str (point-max) t) ; Find END, using `eor-str'.
            (setq end  (point))
          ;; Verify that region is not before context.
          (unless (search-forward br-str (point-max) t)
            (when (search-forward ar-str (point-max) t) ; Find END, using `ar-str'.
              (setq end  (match-beginning 0)
                    end  (and end (bmkp-position-before-whitespace end))))))
        ;; If failed to find END, go to eob and search backward for BEG.
        (unless end (goto-char (point-max)))
        (if (search-backward bor-str (point-min) t) ; Find BEG, using `bor-str'.
            (setq beg  (point))
          ;; Verify that region is not after context.
          (unless (search-backward ar-str (point-min) t)
            (when (search-backward br-str (point-min) t) ; Find BEG, using `br-str'.
              (setq beg  (match-end 0)
                    beg  (and beg (bmkp-position-after-whitespace beg))))))
        (setq reg-retrieved-p  (or beg end)
              reg-relocated-p  reg-retrieved-p
              ;; If only one of BEG or END was found, the relocated region is only
              ;; approximate (keep the same length).  If both were found, it is exact.
              pos              (or beg  (and end (- end (- end-pos pos)))  pos)
              end-pos          (or end  (and beg (+ pos (- end-pos pos)))  end-pos))))
    ;; Region is available. Activate it and maybe save it.
    (cond (reg-retrieved-p
           (goto-char pos)
           (push-mark end-pos 'nomsg 'activate)
           (setq deactivate-mark  nil)
           (when bmkp-show-end-of-region
             (let ((end-win  (save-excursion (forward-line (window-height)) (end-of-line) (point))))
               ;; Bounce point and mark.
               (save-excursion (sit-for 0.6) (exchange-point-and-mark) (sit-for 1))
               ;; Recenter if region end is not visible.
               (when (> end-pos end-win) (recenter 1))))
           ;; Maybe save region.
           (if (and reg-relocated-p (bmkp-save-new-region-location bmk pos end-pos))
               (message "Saved relocated region (from %d to %d)" pos end-pos)
             (message "Region is from %d to %d" pos end-pos)))
          (t                            ; No region.  Go to old start.  Don't push-mark.
           (goto-char pos) (forward-line 0)
           (message "No region from %d to %d" pos end-pos)))))

;; Same as `line-number-at-pos', which is not available until Emacs 22.
(defun bmkp-line-number-at-pos (&optional pos)
  "Buffer line number at position POS. Current line number if POS is nil.
Counting starts at (point-min), so any narrowing restriction applies."
  (1+ (count-lines (point-min) (save-excursion (when pos (goto-char pos)) (forward-line 0) (point)))))

(defun bmkp-goto-position (bookmark file buf bufname pos forward-str behind-str)
  "Go to a bookmark that has no region.
Update the recorded position if `bmkp-save-new-location-flag'.
Arguments are respectively the bookmark, its file, buffer, buffer
name, recorded position, and the context strings for the position."
  (if (and file (file-readable-p file) (not (buffer-live-p buf)))
      (with-current-buffer (find-file-noselect file) (setq buf  (buffer-name)))
    ;; No file found.  See if a non-file buffer exists for this.  If not, raise error.
    (unless (or (and buf (get-buffer buf))
                (and bufname (get-buffer bufname) (not (string= buf bufname))))
      (signal 'file-error `("Jumping to bookmark" "No such file or directory" ,file))))
  (set-buffer (or buf bufname))
  (when bmkp-jump-display-function
    (save-current-buffer (funcall bmkp-jump-display-function (current-buffer))))
  (setq deactivate-mark  t)
  (raise-frame)
  (goto-char pos)
  ;; Try to relocate position.
  ;; Search forward first.  Then, if FORWARD-STR exists and was found in the file, search
  ;; backward for BEHIND-STR.  The rationale is that if text was inserted between the two
  ;; in the file, then it's better to end up before point, so you can see the text, rather
  ;; than after it and not see it.
  (when (and forward-str (search-forward forward-str (point-max) t))
    (goto-char (match-beginning 0)))
  (when (and behind-str (search-backward behind-str (point-min) t)) (goto-char (match-end 0)))
  (when (and (/= pos (point))  bmkp-save-new-location-flag)
    (bookmark-prop-set bookmark 'position     (point))
    (bookmark-prop-set bookmark 'end-position (point))
    (bmkp-maybe-save-bookmarks))
  (/= pos (point)))                     ; Return value indicates whether POS was accurate.

(defun bmkp-jump-sequence (bookmark)
  "Handle a sequence bookmark BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
Handler function for sequence bookmarks."
  (dolist (bmk  (bookmark-prop-get bookmark 'sequence))
    (bookmark--jump-via bmk bmkp-sequence-jump-display-function))
  (message "Done invoking bookmarks in sequence `%s'"
           (if (stringp bookmark) bookmark (bookmark-name-from-full-record bookmark))))

(defun bmkp-jump-function (bookmark)
  "Handle a function bookmark BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
Handler function for function bookmarks."
  (funcall (bookmark-prop-get bookmark 'function)))

(defun bmkp-make-bookmark-list-record ()
  "Create and return a bookmark-list bookmark record.
This records the current state of buffer `*Bookmark List*': the sort
order, filter function, regexp pattern, title, and omit list."
  (let ((state  `((last-sort-comparer          . ,bmkp-sort-comparer)
                  (last-reverse-sort-p         . ,bmkp-reverse-sort-p)
                  (last-reverse-multi-sort-p   . ,bmkp-reverse-multi-sort-p)
                  (last-bmenu-filter-function  . ,bmkp-bmenu-filter-function)
                  (last-bmenu-filter-pattern   . ,bmkp-bmenu-filter-pattern)
                  (last-omitted-list           . ,bmkp-bmenu-omitted-list)
                  (last-bmenu-title            . ,bmkp-bmenu-title)
                  (last-bmenu-toggle-filenames . ,bookmark-bmenu-toggle-filenames))))
    `(,@(bookmark-make-record-default 'point-only)
      (filename      . ,bmkp-non-file-filename)
      (bookmark-list . ,state)
      (handler       . bmkp-jump-bookmark-list))))

(add-hook 'bookmark-bmenu-mode-hook
          #'(lambda () (set (make-local-variable 'bookmark-make-record-function)
                            'bmkp-make-bookmark-list-record)))

(defun bmkp-jump-bookmark-list (bookmark)
  "Jump to bookmark-list bookmark BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
Handler function for record returned by `bmkp-make-bookmark-list-record'."
  (let ((state  (bookmark-prop-get bookmark 'bookmark-list)))
    (setq bmkp-sort-comparer               (cdr (assq 'last-sort-comparer          state))
          bmkp-reverse-sort-p              (cdr (assq 'last-reverse-sort-p         state))
          bmkp-reverse-multi-sort-p        (cdr (assq 'last-reverse-multi-sort-p   state))
          bmkp-bmenu-filter-function       (cdr (assq 'last-bmenu-filter-function  state))
          bmkp-bmenu-filter-pattern        (or (cdr (assq 'last-bmenu-filter-pattern   state)) "")
          bmkp-bmenu-omitted-list          (cdr (assq 'last-omitted-list           state))
          bmkp-bmenu-title                 (cdr (assq 'last-bmenu-title            state))
          bookmark-bmenu-toggle-filenames  (cdr (assq 'last-bmenu-toggle-filenames state))))
  (let ((bookmark-alist  (if bmkp-bmenu-filter-function
                             (funcall bmkp-bmenu-filter-function)
                           bookmark-alist)))
    (setq bmkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp)
    (when (get-buffer "*Bookmark List*") (pop-to-buffer "*Bookmark List*"))))

;; Bookmark-file bookmarks.
;;;###autoload
(defun bmkp-set-bookmark-file-bookmark (file &optional msgp) ; Bound to `C-x p x'
  "Create a bookmark that loads bookmark-file FILE when \"jumped\" to.
You are prompted for the names of the bookmark file and the bookmark."
  (interactive (list (let ((insert-default-directory  t))
                       (read-file-name
                        "Create bookmark to load bookmark file: " "~/"
                        ;; Default file as default choice, unless it's already current.
                        (and (not (bmkp-same-file-p bookmark-default-file bmkp-current-bookmark-file))
                             bookmark-default-file)
                        'confirm))
                     'MSG))
  (setq file  (expand-file-name file))
  (unless (file-readable-p file) (error "Unreadable bookmark file `%s'" file))
  (with-current-buffer (let ((enable-local-variables  ())) (find-file-noselect file))
    (goto-char (point-min))
    (condition-case nil                 ; Check whether it's a valid bookmark file.
        (progn (bookmark-maybe-upgrade-file-format)
               (unless (listp (bookmark-alist-from-buffer)) (error "")))
      (error (error "Not a valid bookmark file: `%s'"))))
  (let ((bookmark-make-record-function  #'(lambda () (bmkp-make-bookmark-file-record file))))
    (call-interactively #'bookmark-set))
  (when msgp (message "Set bookmark-file bookmark")))

(defun bmkp-make-bookmark-file-record (bookmark-file)
  "Create and return a bookmark-file bookmark record.
Records the BOOKMARK-FILE name.
Adds a handler that tests the prefix arg and loads the bookmark file
either as a replacement for the current bookmark file or as a
supplement to it."
  `(,@(bookmark-make-record-default 'point-only)
    (filename      . ,bookmark-file)
    (bookmark-file . ,bookmark-file)
    (handler       . bmkp-jump-bookmark-file)))

(defun bmkp-jump-bookmark-file (bookmark &optional switchp no-msg)
  "Jump to bookmark-file bookmark BOOKMARK, which loads the bookmark file.
BOOKMARK is a bookmark name or a bookmark record.
Non-nil SWITCHP means overwrite the current bookmark list.
Handler function for record returned by `bmkp-make-bookmark-file-record'."
  (let ((file        (bookmark-prop-get bookmark 'bookmark-file))
        (overwritep  (and (or switchp current-prefix-arg)
                          (y-or-n-p "Switch to new bookmark file, instead of just adding it? "))))
    (bookmark-load file overwritep))
  ;; This `throw' is only for the case where this handler is is called from `bookmark--jump-via'.
  ;; It just tells `bookmark--jump-via' to skip the rest of what it does after calling the handler.
  (condition-case nil
      (throw 'bookmark--jump-via 'BOOKMARK-FILE-JUMP)
    (no-catch nil)))

;;;###autoload
(defun bmkp-bookmark-file-jump (bookmark-name &optional switchp no-msg) ; `C-x j x'
  "Jump to a bookmark-file bookmark, which means load its bookmark file.
With a prefix argument, switch to the new bookmark file.
Otherwise, load it to supplement the current bookmark list."
  (interactive
   (let ((alist  (bmkp-bookmark-file-alist-only)))
     (list (bmkp-read-bookmark-for-type "bookmark-file " alist nil nil 'bmkp-bookmark-file-history)
           current-prefix-arg)))
  (bmkp-jump-bookmark-file bookmark-name switchp no-msg))

;; Desktop bookmarks
;;;###autoload
(defun bmkp-set-desktop-bookmark (desktop-file) ; Bound globally to `C-x p K', `C-x r K'
  "Save the desktop as a bookmark.
You are prompted for the desktop-file location and the bookmark name."
  (interactive (list (read-file-name "Save desktop in file: ")))
  (set-text-properties 0 (length desktop-file) nil desktop-file)
  (unless (file-name-absolute-p desktop-file) (setq desktop-file  (expand-file-name desktop-file)))
  (unless (condition-case nil (require 'desktop nil t) (error nil))
    (error "You must have library `desktop.el' to use this command"))
  (let ((desktop-basefilename     (file-name-nondirectory desktop-file)) ; Emacs < 22
        (desktop-base-file-name   (file-name-nondirectory desktop-file)) ; Emacs 23+
        (desktop-dir              (file-name-directory desktop-file))
        (desktop-restore-eager    t)    ; Don't bother with lazy restore.
        (desktop-globals-to-save  (bmkp-remove-if #'(lambda (elt)
                                                      (memq elt bmkp-desktop-no-save-vars))
                                                  desktop-globals-to-save)))
    (if (< emacs-major-version 22)
        (desktop-save desktop-dir)      ; Emacs < 22 has no locking.
      (desktop-save desktop-dir 'RELEASE))
    (message "Desktop saved in `%s'" desktop-file)
    (let ((bookmark-make-record-function  #'(lambda () (bmkp-make-desktop-record desktop-file))))
      (call-interactively #'bookmark-set))))

(defun bmkp-make-desktop-record (desktop-file)
  "Create and return a desktop bookmark record.
DESKTOP-FILE is the absolute file name of the desktop file to use."
  `(,@(bookmark-make-record-default 'point-only)
    (filename     . ,bmkp-non-file-filename)
    (desktop-file . ,desktop-file)
    (handler      . bmkp-jump-desktop)))

(defun bmkp-jump-desktop (bookmark)
  "Jump to desktop bookmark BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
Handler function for record returned by `bmkp-make-desktop-record'."
  (let ((desktop-file  (bookmark-prop-get bookmark 'desktop-file)))
    (unless (condition-case nil (require 'desktop nil t) (error nil))
      (error "You must have library `desktop.el' to use this command"))
    ;; (unless desktop-file (error "Not a desktop-bookmark: %S" bookmark)) ; Shouldn't happen.
    (bmkp-desktop-change-dir desktop-file)
    (unless (bmkp-desktop-read desktop-file) (error "Could not load desktop file"))))

;; Derived from code in `desktop-change-dir'.
;;;###autoload
(defun bmkp-desktop-change-dir (desktop-file)
  "Change to desktop saved in DESKTOP-FILE.
Kill the desktop as specified by variables `desktop-save-mode' and
 `desktop-save' (starting with Emacs 22).
Clear the desktop and load DESKTOP-FILE DIRNAME."
  (interactive (list (read-file-name "Change to desktop file: ")))
  (set-text-properties 0 (length desktop-file) nil desktop-file)
  (unless (file-name-absolute-p desktop-file) (setq desktop-file  (expand-file-name desktop-file)))
  (unless (condition-case nil (require 'desktop nil t) (error nil))
    (error "You must have library `desktop.el' to use this command"))
  (let ((desktop-basefilename     (file-name-nondirectory desktop-file)) ; Emacs < 22
        (desktop-base-file-name   (file-name-nondirectory desktop-file)) ; Emacs 23+
        (desktop-dir              (file-name-directory desktop-file))
        (desktop-restore-eager    t)    ; Don't bother with lazy restore.
        (desktop-globals-to-save  (bmkp-remove-if #'(lambda (elt)
                                                      (memq elt bmkp-desktop-no-save-vars))
                                                  desktop-globals-to-save)))
    (bmkp-desktop-kill)
    (desktop-clear)
    (if (< emacs-major-version 22) (desktop-read) (desktop-read desktop-dir))))
    
;; Derived from code in `desktop-kill'.
(defun bmkp-desktop-kill ()
  "If `desktop-save-mode' is non-nil, do what `desktop-save' says to do.
This does nothing in Emacs versions prior to Emacs 22."
  ;; We assume here: `desktop.el' has been loaded and `desktop-dirname' is defined.
  (when (and (and (boundp 'desktop-save-mode) desktop-save-mode) ; Not defined in Emacs 20-21.
             (let ((exists  (file-exists-p (desktop-full-file-name))))
               (or (eq desktop-save t)
                   (and exists (memq desktop-save '(ask-if-new if-exists)))
                   (and (or (memq desktop-save '(ask ask-if-new))
                            (and exists (eq desktop-save 'ask-if-exists)))
                        (y-or-n-p "Save current desktop? ")))))
    (condition-case err
	(if (< emacs-major-version 22)
            (desktop-save desktop-dirname) ; Emacs < 22 has no locking.
          (desktop-save desktop-dirname 'RELEASE))
      (file-error (unless (yes-or-no-p "Error while saving the desktop.  Ignore? ")
                    (signal (car err) (cdr err))))))
  ;; Just release lock, regardless of whether this Emacs process is the owner.
  (when (fboundp 'desktop-release-lock) (desktop-release-lock))) ; Not defined for Emacs 20.

;; Derived from code in `desktop-read'.
;;;###autoload
(defun bmkp-desktop-read (file)
  "Load desktop-file FILE, then run `desktop-after-read-hook'.
Return t if FILE was loaded, nil otherwise."
  (interactive)
  (unless (file-name-absolute-p file) (setq file  (expand-file-name file))) ; Shouldn't happen.
  (setq desktop-dirname  (file-name-directory file))
  (if (not (file-readable-p file))
      nil                               ; Return nil, meaning not loaded.
    (let ((desktop-restore-eager      t) ; Don't bother with lazy restore.
          (desktop-first-buffer       nil)
          (desktop-buffer-ok-count    0)
          (desktop-buffer-fail-count  0)
          (desktop-save               nil)) ; Prevent desktop saving during eval of desktop buffer.
      (when (fboundp 'desktop-lazy-abort) (desktop-lazy-abort)) ; Emacs 22+.
      (load file t t t)
      (when (boundp 'desktop-file-modtime) ; Emacs 22+
        (setq desktop-file-modtime  (nth 5 (file-attributes file))))
      ;; `desktop-create-buffer' puts buffers at end of the buffer list.
      ;; We want buffers existing prior to evaluating the desktop (and not reused) to be placed
      ;; at the end of the buffer list, so we move them here.
      (mapc 'bury-buffer (nreverse (cdr (memq desktop-first-buffer (nreverse (buffer-list))))))
      (switch-to-buffer (car (buffer-list)))
      (run-hooks 'desktop-delay-hook)
      (setq desktop-delay-hook  ())
      (run-hooks 'desktop-after-read-hook)
      (when (boundp 'desktop-buffer-ok-count) ; Emacs 22+
        (message "Desktop: %d buffer%s restored%s%s." desktop-buffer-ok-count
                 (if (= 1 desktop-buffer-ok-count) "" "s")
                 (if (< 0 desktop-buffer-fail-count)
                     (format ", %d failed to restore" desktop-buffer-fail-count)
                   "")
                 (if (and (boundp 'desktop-buffer-args-list) desktop-buffer-args-list)
                     (format ", %d to be restored lazily" (length desktop-buffer-args-list))
                   "")))
      t)))                              ; Return t, meaning successfully loaded.

;;;###autoload
(defun bmkp-desktop-delete (bookmark-name)
  "Delete desktop bookmark BOOKMARK-NAME, and delete its desktop file.
Release the lock file in that desktop's directory.
\(This means that if you currently have locked a different desktop
in the same directory, then you will need to relock it.)"
  (interactive (let ((alist  (bmkp-desktop-alist-only)))
                 (list (bmkp-read-bookmark-for-type "desktop " alist nil nil 'bmkp-desktop-history
                                                    "Delete "))))
  (let ((desktop-file  (bookmark-prop-get bookmark-name 'desktop-file)))
    (unless desktop-file (error "Not a desktop-bookmark: `%s'" bookmark-name))
    ;; Release the desktop lock file in the same directory as DESKTOP-FILE.
    ;; This will NOT be the right thing to do if a desktop file different from DESKTOP-FILE
    ;; is currently locked in the same directory.
    (let ((desktop-dir  (file-name-directory desktop-file)))
      (when (fboundp 'desktop-release-lock) (desktop-release-lock))) ; Not defined for Emacs 20.
    (when (file-exists-p desktop-file) (delete-file desktop-file)))
  (bookmark-delete bookmark-name))

;; Variable-list bookmarks
;;;###autoload
(when (boundp 'wide-n-restrictions)
  (defun bmkp-set-restrictions-bookmark ()
    "Save the ring of restrictions for the current buffer as a bookmark.
You need library `wide-n.el' to use the bookmark created."
    (interactive)
    (let ((bookmark-make-record-function
           (lambda () (bmkp-make-varlist-record
                       `((wide-n-restrictions
                          . ,(mapcar (lambda (x)
                                       (if (eq x 'all)
                                           'all
                                         (let ((beg  (car x)) ; Convert markers to number positions.
                                               (end  (cdr x)))
                                           (cons (if (markerp beg) (marker-position beg) beg)
                                                 (if (markerp end) (marker-position end) end)))))
                                     wide-n-restrictions)))))))
      (call-interactively #'bookmark-set)
      (unless (featurep 'wide-n) (message "Bookmark created, but you need `wide-n.el' to use it")))))

;;;###autoload
(defun bmkp-set-varlist-bookmark (variables)
  "Save a list of variables as a bookmark.
Interactively, read the variables to save, using
`bmkp-read-variables-completing'."
  (interactive (list (bmkp-read-variables-completing)))
  (let ((bookmark-make-record-function  #'(lambda () (bmkp-make-varlist-record variables))))
    (call-interactively #'bookmark-set)))

(defun bmkp-create-varlist-bookmark (bookmark-name vars vals &optional buffer-name)
  "Create a varlist bookmark named BOOKMARK-NAME from VARS and VALS.
VARS and VALS are corresponding lists of variables and their values.

Optional arg BUFFER-NAME is the buffer name to use for the bookmark (a
string).  This is useful if some of the variables are buffer-local.
If BUFFER-NAME is nil, the current buffer name is recorded."
  (eval `(multiple-value-bind ,vars ',vals
          (let ((bookmark-make-record-function  (lambda ()
                                                  (bmkp-make-varlist-record ',vars ,buffer-name))))
            (bookmark-set ,bookmark-name nil t)))))

(defun bmkp-read-variables-completing (&optional option)
  "Read variable names with completion, and return them as a list of symbols.
Reads names one by one, until you hit `RET' twice consecutively.
Non-nil argument OPTION means read only user option names."
  (bookmark-maybe-load-default-file)
  (let ((var   (bmkp-read-variable "Variable (RET for each, empty input to finish): " option))
        (vars  ())
        old-var)
    (while (not (string= "" var))
      (add-to-list 'vars var)
      (setq var  (bmkp-read-variable "Variable: " option)))
    (nreverse vars)))

(defun bmkp-read-variable (prompt &optional option default-value)
  "Read name of a variable and return it as a symbol.
Prompt with string PROMPT.  
With non-nil OPTION, read the name of a user option.
The default value is DEFAULT-VALUE if non-nil, or the nearest symbol
to the cursor if it is a variable."
  (setq option  (if option 'user-variable-p 'boundp))
  (let ((symb                          (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                                             ((fboundp 'symbol-at-point) (symbol-at-point))
                                             (t nil)))
        (enable-recursive-minibuffers  t))
    (when (and default-value (symbolp default-value))
      (setq default-value  (symbol-name default-value)))
    (intern (completing-read prompt obarray option t nil 'minibuffer-history
                             (or default-value (and (funcall option symb) (symbol-name symb)))
                             t))))

(defun bmkp-make-varlist-record (variables &optional buffer-name)
  "Create and return a variable-list bookmark record.
VARIABLES is the list of variables to save.
Each entry in VARIABLES is either a variable (a symbol) or a cons
 whose car is a variable and whose cdr is the variable's value.

Optional arg BUFFER-NAME is the buffer to use for the bookmark.  This
is useful if some of the variables are buffer-local.  If BUFFER-NAME
is nil, the current buffer is used."
  (let ((record  `(,@(bookmark-make-record-default 'point-only)
                   (filename     . ,bmkp-non-file-filename)
                   (variables    . ,(or (bmkp-printable-vars+vals variables)
                                        (error "No variables to bookmark")))
                   (handler      . bmkp-jump-varlist))))
    (when buffer-name  (let ((bname  (assq 'buffer-name record)))  (setcdr bname buffer-name)))
    record))

(defun bmkp-printable-vars+vals (variables)
  "Return an alist of printable VARIABLES paired with their values.
Display a message for any variables that are not printable.
VARIABLES is the list of variables.  Each entry in VARIABLES is either
 a variable (a symbol) or a cons whose car is a variable and whose cdr
 is the variable's value."
  (let ((vars+vals     ())
        (unprintables  ()))
    (dolist (var  variables)
      (let ((val  (if (consp var) (cdr var) (symbol-value var))))
        (if (bmkp-printable-p val)
            (add-to-list 'vars+vals (if (consp var) var (cons var val)))
          (add-to-list 'unprintables var))))
    (when unprintables (message "Unsavable (unreadable) vars: %S" unprintables)  (sit-for 3))
    vars+vals))

;; Same as `savehist-printable'.
(defun bmkp-printable-p (value)
  "Return non-nil if VALUE would be Lisp-readable if printed using `prin1'."
  (or (stringp value) (numberp value) (symbolp value)
      (with-temp-buffer                 ; Print and try to read.
        (condition-case nil
            (let ((print-readably  t)
                  (print-level     nil))
              (prin1 value (current-buffer))
              (read (point-min-marker))
              t)
          (error nil)))))

(defun bmkp-jump-varlist (bookmark)
  "Jump to variable-list bookmark BOOKMARK, restoring the recorded values.
BOOKMARK is a bookmark name or a bookmark record.
Handler function for record returned by `bmkp-make-varlist-record'."
  (let ((buf        (bmkp-get-buffer-name bookmark))
        (vars+vals  (bookmark-prop-get bookmark 'variables)))
    (unless (get-buffer buf)
      (message "Bookmarked for non-existent buffer `%s', so using current buffer" buf) (sit-for 3)
      (setq buf (current-buffer)))
    (with-current-buffer buf
      (dolist (var+val  vars+vals)
        (set (car var+val)  (cdr var+val))))
    (message "Variables restored in buffer `%s': %S" buf (mapcar #'car vars+vals))
    (sit-for 3)))

;; W3M support
(defun bmkp-make-w3m-record ()
  "Make a special entry for w3m buffers."
  (require 'w3m)                        ; For `w3m-current-url'.
  `(,@(bookmark-make-record-default 'point-only)
    (filename . ,w3m-current-url)
    (handler . bmkp-jump-w3m)))

(add-hook 'w3m-mode-hook #'(lambda () (set (make-local-variable 'bookmark-make-record-function)
                                           'bmkp-make-w3m-record)))

(defun bmkp-w3m-set-new-buffer-name ()
  "Set the w3m buffer name according to the number of w3m buffers already open."
  (let ((len  (length (w3m-list-buffers 'nosort))))
    (if (eq len 0)  "*w3m*"  (format "*w3m*<%d>" (1+ len)))))

(defun bmkp-jump-w3m-new-session (bookmark)
  "Jump to W3M bookmark BOOKMARK, setting a new tab."
  (let ((buf   (bmkp-w3m-set-new-buffer-name)))
    (w3m-browse-url (bookmark-prop-get bookmark 'filename) 'newsession)
    (while (not (get-buffer buf)) (sit-for 1)) ; Be sure we have the W3M buffer.
    (with-current-buffer buf
      (goto-char (point-min))
      ;; Wait until data arrives in buffer, before setting region.
      (while (eq (line-beginning-position) (line-end-position)) (sit-for 1)))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

(defun bmkp-jump-w3m-only-one-tab (bookmark)
  "Close all W3M sessions and jump to BOOKMARK in a new W3M buffer."
  (w3m-quit 'force)                     ; Be sure we start with an empty W3M buffer.
  (w3m-browse-url (bookmark-prop-get bookmark 'filename))
  (with-current-buffer "*w3m*" (while (eq (point-min) (point-max)) (sit-for 1)))
  (bookmark-default-handler
   `("" (buffer . ,(buffer-name (current-buffer))) . ,(bookmark-get-bookmark-record bookmark))))

(defalias 'bmkext-jump-w3m 'bmkp-jump-w3m)
(defun bmkp-jump-w3m (bookmark)
  "Handler function for record returned by `bmkp-make-w3m-record'.
BOOKMARK is a bookmark name or a bookmark record.
Use multi-tabs in W3M if `bmkp-w3m-allow-multi-tabs' is non-nil."
  (if bmkp-w3m-allow-multi-tabs
      (bmkp-jump-w3m-new-session bookmark)
    (bmkp-jump-w3m-only-one-tab bookmark)))

;; GNUS support.  Does not handle regions.
(defun bmkp-make-gnus-record ()
  "Make a bookmark entry for a Gnus buffer."
  (require 'gnus)
  (unless (and (eq major-mode 'gnus-summary-mode) gnus-article-current)
    (error "Please retry from the Gnus summary buffer")) ;[1]
  (let* ((grp   (car gnus-article-current))
         (art   (cdr gnus-article-current))
         (head  (gnus-summary-article-header art))
         (id    (mail-header-id head)))
    `(,@(bookmark-make-record-default 'point-only)
      (filename . ,bmkp-non-file-filename) (group . ,grp) (article . ,art) (message-id . ,id)
      (handler . bmkp-jump-gnus))))

(add-hook 'gnus-summary-mode-hook #'(lambda ()
                                      (set (make-local-variable 'bookmark-make-record-function)
                                           'bmkp-make-gnus-record)))

;; Raise an error if we try to bookmark from here [1]
(add-hook 'gnus-article-mode-hook #'(lambda ()
                                      (set (make-local-variable 'bookmark-make-record-function)
                                           'bmkp-make-gnus-record)))

(defalias 'bmkext-jump-gnus 'bmkp-jump-gnus)
(defun bmkp-jump-gnus (bookmark)
  "Handler function for record returned by `bmkp-make-gnus-record'.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((group    (bookmark-prop-get bookmark 'group))
        (article  (bookmark-prop-get bookmark 'article))
        (id       (bookmark-prop-get bookmark 'message-id))
        (buf      (bookmark-prop-get bookmark 'buffer)))
    (gnus-fetch-group group (list article))
    (gnus-summary-insert-cached-articles)
    (gnus-summary-goto-article id nil 'force)
    (bookmark-default-handler `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

(when (> emacs-major-version 20)
  (defun bmkp-make-woman-record ()
    "Create bookmark record for `man' page bookmark created by `woman'."
    `(,@(bookmark-make-record-default 'point-only)
      (filename . ,woman-last-file-name) (handler . bmkp-jump-woman)))

  (add-hook 'woman-mode-hook #'(lambda ()
                                 (set (make-local-variable 'bookmark-make-record-function)
                                      'bmkp-make-woman-record))))

(defun bmkp-make-man-record ()
  "Create bookmark record for `man' page bookmark created by `man'."
  `(,@(bookmark-make-record-default 'point-only)
    (filename . ,bmkp-non-file-filename)
    (man-args . ,Man-arguments) (handler . bmkp-jump-man)))

  (add-hook 'Man-mode-hook #'(lambda () (set (make-local-variable 'bookmark-make-record-function)
                                             'bmkp-make-man-record)))

(defalias 'bmkext-jump-woman 'bmkp-jump-woman)
(defun bmkp-jump-woman (bookmark)
  "Handler function for `man' page bookmark created by `woman'."
  (unless (> emacs-major-version 20)
    (error "`woman' bookmarks are not supported in Emacs prior to Emacs 21"))
  (bookmark-default-handler
   `("" (buffer . ,(save-window-excursion (woman-find-file (bookmark-get-filename bookmark))
                                          (current-buffer)))
     . ,(bookmark-get-bookmark-record bookmark))))

(defalias 'bmkext-jump-man 'bmkp-jump-man)
(defun bmkp-jump-man (bookmark)
  "Handler function for `man' page bookmark created by `man'."
  (require 'man)
  (let ((Man-notify-method  (case bmkp-jump-display-function
                              (display-buffer    'quiet)
                              (switch-to-buffer  'pushy)
                              (t                 'friendly)))) ; pop-to
    (Man-getpage-in-background (bookmark-prop-get bookmark 'man-args))
    (while (get-process "man") (sit-for 0.2))
    (bookmark-default-handler (bookmark-get-bookmark bookmark))))

(defun bmkp-make-dired-record ()
  "Create and return a Dired bookmark record."
  (let ((hidden-dirs  (save-excursion (dired-remember-hidden))))
    (unwind-protect
         (let ((dir      (expand-file-name (if (consp dired-directory)
                                               (file-name-directory (car dired-directory))
                                             dired-directory)))
               (subdirs  (bmkp-dired-subdirs))
               (mfiles   (mapcar #'(lambda (mm) (car mm))
                                 (dired-remember-marks (point-min) (point-max)))))
           `(,dir
             ,@(bookmark-make-record-default 'point-only)
             (filename . ,dir) (dired-directory . ,dired-directory)
             (dired-marked . ,mfiles) (dired-switches . ,dired-actual-switches)
             (dired-subdirs . ,subdirs) (dired-hidden-dirs . ,hidden-dirs)
             (handler . bmkp-jump-dired)))
      (save-excursion			; Hide subdirs that were hidden.
        (dolist (dir  hidden-dirs)  (when (dired-goto-subdir dir) (dired-hide-subdir 1)))))))

(defun bmkp-dired-subdirs ()
  "Alist of inserted subdirectories, without their positions (markers).
This is like `dired-subdir-alist' but without the top-level dir and
without subdir positions (markers)."
  (interactive)
  (let ((subdir-alist      (cdr (reverse dired-subdir-alist))) ; Remove top.
        (subdirs-wo-posns  ()))
    (dolist (sub  subdir-alist)  (push (list (car sub)) subdirs-wo-posns))
    subdirs-wo-posns))

(add-hook 'dired-mode-hook #'(lambda () (set (make-local-variable 'bookmark-make-record-function)
                                             'bmkp-make-dired-record)))

(defun bmkp-jump-dired (bookmark)
  "Jump to Dired bookmark BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
Handler function for record returned by `bmkp-make-dired-record'."
  (let ((dir          (bookmark-prop-get bookmark 'dired-directory))
        (mfiles       (bookmark-prop-get bookmark 'dired-marked))
        (switches     (bookmark-prop-get bookmark 'dired-switches))
        (subdirs      (bookmark-prop-get bookmark 'dired-subdirs))
        (hidden-dirs  (bookmark-prop-get bookmark 'dired-hidden-dirs)))
    (case bmkp-jump-display-function
      (switch-to-buffer  (dired dir switches))
      (t                 (dired-other-window dir switches)))
    (let ((inhibit-read-only  t))
      (dired-insert-old-subdirs subdirs)
      (dired-mark-remembered (mapcar #'(lambda (mf) (cons (expand-file-name mf dir) 42)) mfiles))
      (save-excursion
        (dolist (dir  hidden-dirs) (when (dired-goto-subdir dir) (dired-hide-subdir 1)))))
    (goto-char (bookmark-get-position bookmark))))

(defun bmkp-read-bookmark-for-type (type alist &optional other-win pred hist action)
  "Read name of a bookmark of type TYPE.
ALIST is the alist used for completion - nil means `bookmark-alist'.
Non-nil OTHER-WIN means append \" in another window\" to the prompt.
Non-nil PRED is a predicate used for completion.
Non-nil HIST is a history symbol.  Default is `bookmark-history'.
ACTION is the action to mention in the prompt.  `Jump to ', if nil."
  (unless alist (error "No bookmarks of type %s" type))
  (bookmark-completing-read
   (concat (or action "Jump to ") type "bookmark" (and other-win " in another window"))
   (bmkp-default-bookmark-name alist)
   alist pred hist))

;;;###autoload
(defun bmkp-jump-to-type (bookmark-name &optional use-region-p) ; `C-x j :'
  "Jump to a bookmark of a given type.  You are prompted for the type.
Otherwise, this is the same as `bookmark-jump' - see that, in
particular, for info about using a prefix argument."
  (interactive
   (let* ((completion-ignore-case  t)
          (type-cands              bmkp-types-alist)
          (type                    (completing-read "Type of bookmark: " type-cands nil t))
          (alist                   (funcall (intern (format "bmkp-%s-alist-only" type))))
          (history                 (assoc-default type type-cands))
          (bmk-name                (bmkp-read-bookmark-for-type (concat type " ") alist nil nil
                                                                history)))
     (list bmk-name  (or (equal type "Region") current-prefix-arg))))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-jump-to-type-other-window (bookmark-name &optional use-region-p) ; `C-x 4 j :'
  "Jump to a bookmark of a given type.  You are prompted for the type.
See `bmkp-jump-to-type'."
  (interactive
   (let* ((completion-ignore-case  t)
          (type-cands              bmkp-types-alist)
          (type                    (completing-read "Type of bookmark: " type-cands nil t))
          (alist                   (funcall (intern (format "bmkp-%s-alist-only" type))))
          (history                 (assoc-default type type-cands))
          (bmk-name                (bmkp-read-bookmark-for-type (concat type " ") alist t nil
                                                                history)))
     (list bmk-name (or (equal type "Region") current-prefix-arg))))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-bookmark-list-jump (bookmark-name &optional use-region-p) ; `C-x j B'
  "Jump to a bookmark-list bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((alist  (bmkp-bookmark-list-alist-only)))
     (list (bmkp-read-bookmark-for-type "bookmark-list " alist nil nil 'bmkp-bookmark-list-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-desktop-jump (bookmark-name &optional use-region-p) ; `C-x j K'
  "Jump to a desktop bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((alist  (bmkp-desktop-alist-only)))
     (list (bmkp-read-bookmark-for-type "desktop " alist nil nil 'bmkp-desktop-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-dired-jump (bookmark-name &optional use-region-p) ; `C-x j d'
  "Jump to a Dired bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((alist  (bmkp-dired-alist-only)))
     (list (bmkp-read-bookmark-for-type "Dired " alist nil nil 'bmkp-dired-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-dired-jump-other-window (bookmark-name &optional use-region-p) ; `C-x 4 j d'
  "Jump to a Dired bookmark in another window.
See `bmkp-dired-jump'."
  (interactive
   (let ((alist  (bmkp-dired-alist-only)))
     (list (bmkp-read-bookmark-for-type "Dired " alist t nil 'bmkp-dired-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-dired-jump-current (bookmark-name &optional use-region-p)
  "Jump to a Dired bookmark for the current directory.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((alist  (bmkp-dired-alist-only)))
     (list (bmkp-read-bookmark-for-type "Dired " alist nil
                                        (lambda (bmk-name)
                                          (let ((dir  (bookmark-get-filename bmk-name)))
                                            (string= dir default-directory)))
                                        'bmkp-dired-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-dired-jump-current-other-window (bookmark-name &optional use-region-p)
  "Jump to a Dired bookmark for the current directory in another window.
See `bmkp-dired-jump-current'."
  (interactive
   (let ((alist  (bmkp-dired-alist-only)))
     (list (bmkp-read-bookmark-for-type "Dired " alist t
                                        (lambda (bmk-name)
                                          (let ((dir  (bookmark-get-filename bmk-name)))
                                            (string= dir default-directory)))
                                        'bmkp-dired-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-file-jump (bookmark-name &optional use-region-p) ; `C-x j f'
  "Jump to a file or directory bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((alist  (bmkp-file-alist-only)))
     (list (bmkp-read-bookmark-for-type "file " alist nil nil 'bmkp-file-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-file-jump-other-window (bookmark-name &optional use-region-p) ; `C-x 4 j f'
  "Jump to a file or directory bookmark in another window.
See `bmkp-file-jump'."
  (interactive
   (let ((alist  (bmkp-file-alist-only)))
     (list (bmkp-read-bookmark-for-type "file " alist t nil 'bmkp-file-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-gnus-jump (bookmark-name &optional use-region-p) ; `C-x j g'
  "Jump to a Gnus bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((alist  (bmkp-gnus-alist-only)))
     (list (bmkp-read-bookmark-for-type "Gnus " alist nil nil 'bmkp-gnus-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-gnus-jump-other-window (bookmark-name &optional use-region-p) ; `C-x 4 j g'
  "Jump to a Gnus bookmark in another window.
See `bmkp-gnus-jump'."
  (interactive
   (let ((alist  (bmkp-gnus-alist-only)))
     (list (bmkp-read-bookmark-for-type "Gnus " alist t nil 'bmkp-gnus-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-info-jump (bookmark-name &optional use-region-p) ; `C-x j i'
  "Jump to an Info bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((alist  (bmkp-info-alist-only)))
     (list (bmkp-read-bookmark-for-type "Info " alist nil nil 'bmkp-info-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-info-jump-other-window (bookmark-name &optional use-region-p) ; `C-x 4 j i'
  "Jump to an Info bookmark in another window.
See `bmkp-info-jump'."
  (interactive
   (let ((alist  (bmkp-info-alist-only)))
     (list (bmkp-read-bookmark-for-type "Info " alist t nil 'bmkp-info-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-local-file-jump (bookmark-name &optional use-region-p) ; `C-x j l'
  "Jump to a local file or directory bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((alist  (bmkp-local-file-alist-only)))
     (list (bmkp-read-bookmark-for-type "local file " alist nil nil 'bmkp-local-file-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-local-file-jump-other-window (bookmark-name &optional use-region-p) ; `C-x 4 j l'
  "Jump to a local file or directory bookmark in another window.
See `bmkp-local-file-jump'."
  (interactive
   (let ((alist  (bmkp-local-file-alist-only)))
     (list (bmkp-read-bookmark-for-type "local file " alist t nil 'bmkp-local-file-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-man-jump (bookmark-name &optional use-region-p) ; `C-x j m'
  "Jump to a `man'-page bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((alist  (bmkp-man-alist-only)))
     (list (bmkp-read-bookmark-for-type "`man' " alist nil nil 'bmkp-man-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-man-jump-other-window (bookmark-name &optional use-region-p) ; `C-x 4 j m'
  "Jump to a `man'-page bookmark in another window.
See `bmkp-man-jump'."
  (interactive
   (let ((alist  (bmkp-man-alist-only)))
     (list (bmkp-read-bookmark-for-type "`man' " alist t nil 'bmkp-man-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-non-file-jump (bookmark-name &optional use-region-p) ; `C-x j b'
  "Jump to a non-file (buffer) bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((alist  (bmkp-non-file-alist-only)))
     (list (bmkp-read-bookmark-for-type "non-file (buffer) " alist nil nil 'bmkp-non-file-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-non-file-jump-other-window (bookmark-name &optional use-region-p) ; `C-x 4 j b'
  "Jump to a non-file (buffer) bookmark in another window.
See `bmkp-non-file-jump'."
  (interactive
   (let ((alist  (bmkp-non-file-alist-only)))
     (list (bmkp-read-bookmark-for-type "non-file (buffer) " alist t nil 'bmkp-non-file-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-region-jump (bookmark-name) ; `C-x j r'
  "Jump to a region bookmark.
This is a specialization of `bookmark-jump', but without a prefix arg."
  (interactive (list (bmkp-read-bookmark-for-type "region " (bmkp-region-alist-only) nil nil
                                                  'bmkp-region-history)))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer t))

;;;###autoload
(defun bmkp-region-jump-other-window (bookmark-name) ; `C-x 4 j r'
  "Jump to a region bookmark in another window.
See `bmkp-region-jump'."
  (interactive (list (bmkp-read-bookmark-for-type "region " (bmkp-region-alist-only) t nil
                                                  'bmkp-region-history)))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window t))

;;;###autoload
(defun bmkp-remote-file-jump (bookmark-name &optional use-region-p) ; `C-x j n'
  "Jump to a remote file or directory bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((alist  (bmkp-remote-file-alist-only)))
     (list (bmkp-read-bookmark-for-type "remote file " alist nil nil 'bmkp-remote-file-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-remote-file-jump-other-window (bookmark-name &optional use-region-p) ; `C-x 4 j n'
  "Jump to a remote file or directory bookmark in another window.
See `bmkp-remote-file-jump'."
  (interactive
   (let ((alist  (bmkp-remote-file-alist-only)))
     (list (bmkp-read-bookmark-for-type "remote file " alist t nil 'bmkp-remote-file-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-specific-buffers-jump (buffers bookmark-name &optional use-region-p) ; `C-x j = b'
  "Jump to a bookmark for a buffer in list BUFFERS.
Interactively, read buffer names and bookmark name, with completion.

This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((buffs  ())
         buff)
     (while (and (setq buff  (bmkp-completing-read-buffer-name)) (not (string= "" buff)))
       (add-to-list 'buffs buff))
     (let ((alist  (bmkp-specific-buffers-alist-only buffs)))
       (list buffs (bmkp-read-bookmark-for-type "specific-buffers " alist) current-prefix-arg))))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-specific-buffers-jump-other-window (buffers bookmark-name
                                                &optional use-region-p) ; `C-x 4 j = b'
  "Jump to a bookmark for a buffer in list BUFFERS in another window.
See `bmkp-specific-buffers-jump'."
  (interactive
   (let ((buffs  ())
         buff)
     (while (and (setq buff  (bmkp-completing-read-buffer-name)) (not (string= "" buff)))
       (add-to-list 'buffs buff))
     (let ((alist  (bmkp-specific-buffers-alist-only buffs)))
       (list buffs (bmkp-read-bookmark-for-type "specific-buffers " alist) current-prefix-arg))))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-specific-files-jump (files bookmark-name &optional use-region-p) ; `C-x j = f'
  "Jump to a bookmark for a file in list FILES.
Interactively, read file names and bookmark name, with completion.

This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((use-file-dialog  nil)
         (files            ())
         file)
     (while (and (setq file  (bmkp-completing-read-file-name)) (not (string= "" file)))
       (add-to-list 'files file))
     (let ((alist  (bmkp-specific-files-alist-only files)))
       (list files (bmkp-read-bookmark-for-type "specific-files " alist) current-prefix-arg))))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-specific-files-jump-other-window (files bookmark-name
                                              &optional use-region-p) ; `C-x 4 j = f'
  "Jump to a bookmark for a buffer in list BUFFERS in another window.
See `bmkp-specific-buffers-jump'."
  (interactive
   (let ((use-file-dialog  nil)
         (files            ())
         file)
     (while (and (setq file  (bmkp-completing-read-file-name)) (not (string= "" file)))
       (add-to-list 'files file))
     (let ((alist  (bmkp-specific-files-alist-only files)))
       (list files (bmkp-read-bookmark-for-type "specific-files " alist) current-prefix-arg))))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-this-buffer-jump (bookmark-name &optional use-region-p) ; `C-x j .'
  "Jump to a bookmark for the current buffer.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((alist  (bmkp-this-buffer-alist-only)))
     (unless alist  (error "No bookmarks for this buffer"))
     (list (bookmark-completing-read "Jump to bookmark for this buffer"
                                     (bmkp-default-bookmark-name alist) alist)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-this-buffer-jump-other-window (bookmark-name &optional use-region-p) ; `C-x 4 j .'
  "Jump to a bookmark for the current buffer in another window.
See `bmkp-this-buffer-jump'."
  (interactive
   (let ((alist  (bmkp-this-buffer-alist-only)))
     (unless alist  (error "No bookmarks for this buffer"))
     (list (bookmark-completing-read "Jump to bookmark for this buffer in another window"
                                     (bmkp-default-bookmark-name alist) alist)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;; ;;;###autoload
;;; (defun bmkp-this-file-jump (bookmark-name &optional use-region-p)
;;;   "Jump to a bookmark for the current file (absolute file name).
;;; This is a specialization of `bookmark-jump' - see that, in particular
;;; for info about using a prefix argument."
;;;   (interactive
;;;    (progn (unless (or (buffer-file-name) (and (eq major-mode 'dired-mode)
;;;                                               (if (consp dired-directory)
;;;                                                   (car dired-directory)
;;;                                                 dired-directory)))
;;;             (error "This buffer is not associated with a file"))
;;;           (let ((alist  (bmkp-this-file-alist-only)))
;;;             (unless alist  (error "No bookmarks for this file"))
;;;             (list (bookmark-completing-read "Jump to bookmark for this file"
;;;                                             (bmkp-default-bookmark-name alist) alist)
;;;                   current-prefix-arg))))
;;;   (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;; ;;;###autoload
;;; (defun bmkp-this-file-jump-other-window (bookmark-name &optional use-region-p)
;;;   "Jump to a bookmark for the current file in another window.
;;; See `bmkp-this-file-jump'."
;;;   (interactive
;;;    (progn (unless (or (buffer-file-name) (and (eq major-mode 'dired-mode)
;;;                                               (if (consp dired-directory)
;;;                                                   (car dired-directory)
;;;                                                 dired-directory)))
;;;             (error "This buffer is not associated with a file"))
;;;           (let ((alist  (bmkp-this-file-alist-only)))
;;;             (unless alist  (error "No bookmarks for this file"))
;;;             (list (bookmark-completing-read "Jump to bookmark for this file in another window"
;;;                                             (bmkp-default-bookmark-name alist) alist)
;;;                   current-prefix-arg))))
;;;   (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-varlist-jump (bookmark-name) ; `C-x j v'
  "Jump to a variable-list bookmark.
This is a specialization of `bookmark-jump'."
  (interactive
   (let ((alist  (bmkp-varlist-alist-only)))
     (list (bmkp-read-bookmark-for-type "varlist " alist nil nil 'bmkp-varlist-history))))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer nil))

;;;###autoload
(defun bmkp-w3m-jump (bookmark-name &optional use-region-p) ; `C-x j n'
  "Jump to a W3M bookmark.
This is a specialization of `bookmark-jump' - see that, in particular
for info about using a prefix argument."
  (interactive
   (let ((alist  (bmkp-w3m-alist-only)))
     (list (bmkp-read-bookmark-for-type "W3M " alist nil nil 'bmkp-w3m-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-w3m-jump-other-window (bookmark-name &optional use-region-p) ; `C-x 4 j w'
  "Jump to an W3M bookmark in another window.
See `bmkp-w3m-jump'."
  (interactive
   (let ((alist  (bmkp-w3m-alist-only)))
     (list (bmkp-read-bookmark-for-type "W3M " alist t nil 'bmkp-w3m-history)
           current-prefix-arg)))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-all-tags-jump (tags bookmark) ; `C-x j t *'
  "Jump to a BOOKMARK that has all of the TAGS.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter the bookmark name and each tag.
If you specify no tags, then every bookmark that has some tags is a
candidate."
  (interactive
   (let* ((ts     (bmkp-read-tags-completing))
          (alist  (bmkp-all-tags-alist-only ts)))
     (unless alist (error "No bookmarks have all of the specified tags"))
     (list ts (bookmark-completing-read "Bookmark" (bmkp-default-bookmark-name alist) alist))))
  (bookmark-jump bookmark))

;;;###autoload
(defun bmkp-all-tags-jump-other-window (tags bookmark) ; `C-x 4 j t *'
  "Jump to a BOOKMARK that has all of the TAGS, in another window.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter the bookmark name and each tag.
If you specify no tags, then every bookmark that has some tags is a
candidate."
  (interactive
   (let* ((ts     (bmkp-read-tags-completing))
          (alist  (bmkp-all-tags-alist-only ts)))
     (unless alist (error "No bookmarks have all of the specified tags"))
     (list ts (bookmark-completing-read "Bookmark" (bmkp-default-bookmark-name alist) alist))))
  (bookmark-jump-other-window bookmark))

;;;###autoload
(defun bmkp-all-tags-regexp-jump (regexp bookmark) ; `C-x j t % *'
  "Jump to a BOOKMARK that has each tag matching REGEXP.
You are prompted for the REGEXP.
Then you are prompted for the BOOKMARK (with completion)."
  (interactive
   (let* ((rgx    (read-string "Regexp for tags: "))
          (alist  (bmkp-all-tags-regexp-alist-only rgx)))
     (unless alist (error "No bookmarks have tags that match `%s'" rgx))
     (list rgx (bookmark-completing-read "Bookmark" (bmkp-default-bookmark-name alist) alist))))
  (bookmark-jump bookmark))

;;;###autoload
(defun bmkp-all-tags-regexp-jump-other-window (regexp bookmark) ; `C-x 4 j t % *'
  "Jump to a BOOKMARK that has each tag matching REGEXP, in another window.
You are prompted for the REGEXP.
Then you are prompted for the BOOKMARK (with completion)."
  (interactive
   (let* ((rgx    (read-string "Regexp for tags: "))
          (alist  (bmkp-all-tags-regexp-alist-only rgx)))
     (unless alist (error "No bookmarks have tags that match `%s'" rgx))
     (list rgx (bookmark-completing-read "Bookmark" (bmkp-default-bookmark-name alist) alist))))
  (bookmark-jump-other-window bookmark))

;;;###autoload
(defun bmkp-some-tags-jump (tags bookmark) ; `C-x j t +'
  "Jump to a BOOKMARK that has at least one of the TAGS.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter the bookmark name and each tag."
  (interactive
   (let* ((ts     (bmkp-read-tags-completing))
          (alist  (bmkp-some-tags-alist-only ts)))
     (unless ts (error "You did not specify any tags"))
     (unless alist (error "No bookmarks have any of the specified tags"))
     (list ts (bookmark-completing-read "Bookmark" (bmkp-default-bookmark-name alist) alist))))
  (bookmark-jump bookmark))

;;;###autoload
(defun bmkp-some-tags-jump-other-window (tags bookmark) ; `C-x 4 j t +'
  "Jump to a BOOKMARK that has at least one of the TAGS, in another window.
Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter the bookmark name and each tag."
  (interactive
   (let* ((ts     (bmkp-read-tags-completing))
          (alist  (bmkp-some-tags-alist-only ts)))
     (unless ts (error "You did not specify any tags"))
     (unless alist (error "No bookmarks have any of the specified tags"))
     (list ts (bookmark-completing-read "Bookmark" (bmkp-default-bookmark-name alist) alist))))
  (bookmark-jump-other-window bookmark))

;;;###autoload
(defun bmkp-some-tags-regexp-jump (regexp bookmark) ; `C-x j t % +'
  "Jump to a BOOKMARK that has a tag matching REGEXP.
You are prompted for the REGEXP.
Then you are prompted for the BOOKMARK (with completion)."
  (interactive
   (let* ((rgx    (read-string "Regexp for tags: "))
          (alist  (bmkp-some-tags-regexp-alist-only rgx)))
     (unless alist (error "No bookmarks have tags that match `%s'" rgx))
     (list rgx (bookmark-completing-read "Bookmark" (bmkp-default-bookmark-name alist) alist))))
  (bookmark-jump bookmark))

;;;###autoload
(defun bmkp-some-tags-regexp-jump-other-window (regexp bookmark) ; `C-x 4 j t % +'
  "Jump to a BOOKMARK that has a tag matching REGEXP, in another window.
You are prompted for the REGEXP.
Then you are prompted for the BOOKMARK (with completion)."
  (interactive
   (let* ((rgx    (read-string "Regexp for tags: "))
          (alist  (bmkp-some-tags-regexp-alist-only rgx)))
     (unless alist (error "No bookmarks have tags that match `%s'" rgx))
     (list rgx (bookmark-completing-read "Bookmark" (bmkp-default-bookmark-name alist) alist))))
  (bookmark-jump-other-window bookmark))

;;;###autoload
(defun bmkp-jump-in-navlist (bookmark-name &optional use-region-p) ; `C-x j N'
  "Jump to a bookmark, choosing from those in the navigation list."
  (interactive
   (progn (unless bmkp-nav-alist
            (bookmark-maybe-load-default-file)
            (setq bmkp-nav-alist  bookmark-alist)
            (unless bmkp-nav-alist (error "No bookmarks"))
            (setq bmkp-current-nav-bookmark  (car bmkp-nav-alist))
            (message "Bookmark navigation list is now the global bookmark list") (sit-for 2))
          (let ((bookmark-alist  bmkp-nav-alist))
            (list (bookmark-completing-read "Jump to bookmark (in another window)"
                                            (bmkp-default-bookmark-name))
                  current-prefix-arg))))
  (bmkp-jump-1 bookmark-name 'switch-to-buffer use-region-p))

;;;###autoload
(defun bmkp-jump-in-navlist-other-window (bookmark-name &optional use-region-p) ; `C-x 4 j N'
  "Same as `bmkp-jump-in-navlist', but use another window."
  (interactive
   (progn (unless bmkp-nav-alist
            (bookmark-maybe-load-default-file)
            (setq bmkp-nav-alist  bookmark-alist)
            (unless bmkp-nav-alist (error "No bookmarks"))
            (setq bmkp-current-nav-bookmark  (car bmkp-nav-alist))
            (message "Bookmark navigation list is now the global bookmark list") (sit-for 2))
          (let ((bookmark-alist  bmkp-nav-alist))
            (list (bookmark-completing-read "Jump to bookmark (in another window)"
                                            (bmkp-default-bookmark-name))
                  current-prefix-arg))))
  (bmkp-jump-1 bookmark-name 'bmkp-select-buffer-other-window use-region-p))

;;;###autoload
(defun bmkp-cycle (increment &optional other-window startoverp)
  "Cycle through bookmarks in the navlist by INCREMENT (default: 1).
Positive INCREMENT cycles forward.  Negative INCREMENT cycles backward.
Interactively, the prefix arg determines INCREMENT:
 Plain `C-u': 1
 otherwise: the numeric prefix arg value

Plain `C-u' also means start over at first bookmark.

You can set the navigation list using commands
 `bmkp-choose-navlist-from-bookmark-list' and
 `bmkp-choose-navlist-of-type'.

You can cycle among bookmarks in the current buffer using
 `bmkp-cycle-this-buffer' and
 `bmkp-cycle-this-buffer-other-window.'

In Lisp code:
 Non-nil OTHER-WINDOW means jump to the bookmark in another window.
 Non-nil STARTOVERP means reset `bmkp-current-nav-bookmark' to the
 first bookmark in the navlist."
  (interactive (let ((startovr  (consp current-prefix-arg)))
                 (list (if startovr 1 (prefix-numeric-value current-prefix-arg)) nil startovr)))
  (unless bmkp-nav-alist
    (bookmark-maybe-load-default-file)
    (setq bmkp-nav-alist  bookmark-alist)
    (unless bmkp-nav-alist (error "No bookmarks"))
    (setq bmkp-current-nav-bookmark  (car bmkp-nav-alist))
    (message "Bookmark navigation list is now the global bookmark list") (sit-for 2))
  (unless (and bmkp-current-nav-bookmark (not startoverp))
    (setq bmkp-current-nav-bookmark  (car bmkp-nav-alist)))
  (if (bmkp-cycle-1 increment other-window startoverp)
      (unless (or (bmkp-sequence-bookmark-p bmkp-current-nav-bookmark)
                  (bmkp-function-bookmark-p bmkp-current-nav-bookmark))
        (message "Position: %d, Bookmark: `%s'"
                 (point) (bookmark-name-from-full-record bmkp-current-nav-bookmark)))
    (message "Invalid bookmark: `%s'" (bookmark-name-from-full-record bmkp-current-nav-bookmark))))

;;;###autoload
(defun bmkp-cycle-other-window (increment)
  "Cycle through bookmarks by INCREMENT (default: 1) in another window.
Positive INCREMENT cycles forward.  Negative INCREMENT cycles backward."
  (interactive "p")
  (bmkp-cycle increment 'other-window))

;;;###autoload
(defun bmkp-cycle-this-buffer (increment &optional other-window startoverp)
  "Cycle through bookmarks in this buffer by INCREMENT (default: 1).
Positive INCREMENT cycles forward.  Negative INCREMENT cycles backward.
Interactively, the prefix arg determines INCREMENT:
 Plain `C-u': 1
 otherwise: the numeric prefix arg value 

Plain `C-u' also means start over at first bookmark.

You can cycle among bookmarks beyond the current buffer using
`bmkp-cycle' and `bmkp-cycle-other-window.'

You can set your preferred sort order for this-buffer bookmarks by
customizing option `bmkp-this-buffer-cycle-sort-comparer'.

To change the sort order without customizing, you can use \
`\\[bmkp-this-buffer-bmenu-list]' to
show the `*Bookmark List*' with only this buffer's bookmarks, sort
them there, and use `\\[bmkp-choose-navlist-from-bookmark-list]', choosing \
`CURRENT *Bookmark List*' as
the navigation list.

Then you can cycle the bookmarks using `bookmark-cycle'
\(`\\[bmkp-next-bookmark-repeat]' etc.), instead of `bookmark-cycle-this-buffer'.

In Lisp code:
 Non-nil OTHER-WINDOW means jump to the bookmark in another window.
 Non-nil STARTOVERP means reset `bmkp-current-nav-bookmark' to the
 first bookmark in the navlist."
  (interactive (let ((startovr  (consp current-prefix-arg)))
                 (list (if startovr 1 (prefix-numeric-value current-prefix-arg)) nil startovr)))
  (bookmark-maybe-load-default-file)
  (let ((bmkp-sort-comparer  bmkp-this-buffer-cycle-sort-comparer))
    (setq bmkp-nav-alist  (bmkp-sort-and-remove-dups (bmkp-this-buffer-alist-only))))
  (unless bmkp-nav-alist (error "No bookmarks in this buffer"))
  (unless (and bmkp-current-nav-bookmark (not startoverp)
               (bookmark-get-bookmark bmkp-current-nav-bookmark 'NOERROR)
               (bmkp-this-buffer-p bmkp-current-nav-bookmark)) ; Exclude desktops etc.
    (setq bmkp-current-nav-bookmark  (car bmkp-nav-alist)))
  (if (bmkp-cycle-1 increment other-window startoverp)
      (unless (or (bmkp-sequence-bookmark-p bmkp-current-nav-bookmark)
                  (bmkp-function-bookmark-p bmkp-current-nav-bookmark))
        (message "Position: %9d, Bookmark: `%s'"
                 (point) (bookmark-name-from-full-record bmkp-current-nav-bookmark)))
    (message "Invalid bookmark: `%s'" (bookmark-name-from-full-record bmkp-current-nav-bookmark))))

;;;###autoload
(defun bmkp-cycle-this-buffer-other-window (increment &optional startoverp)
  "Cycle through bookmarks in this buffer by INCREMENT in another window.
Positive INCREMENT cycles forward.  Negative INCREMENT cycles backward.
INCREMENT defaults to 1."
  (interactive (let ((startovr  (consp current-prefix-arg)))
                 (list (if startovr 1 (prefix-numeric-value current-prefix-arg)) startovr)))
  (bmkp-cycle-this-buffer increment 'OTHER-WINDOW startoverp))

(defun bmkp-cycle-1 (increment &optional other-window startoverp)
  "Helper for `bookmark-cycle' and `bookmark-cycle-this-buffer'.
Do nothing if `bmkp-current-nav-bookmark' is an invalid bookmark.
Return `bmkp-current-nav-bookmark', or nil if invalid.

NOTE: If `pop-up-frames' is non-nil, then cycling inhibits automatic
showing of annotations (`bookmark-automatically-show-annotations').
This is to prevent change of frame focus, so cycling can continue
properly.

See `bmkp-cycle' for descriptions of the arguments."
  (let ((bookmark-alist   bmkp-nav-alist)
        (bookmark         (bookmark-get-bookmark bmkp-current-nav-bookmark 'no-error))
        (bmkp-use-region  (eq 'cycling-too bmkp-use-region)))
    (unless bookmark-alist (error "No bookmarks for cycling"))
    (when bookmark                      ; Skip bookmarks with bad names.
      (setq bmkp-current-nav-bookmark
            (if startoverp
                (car bookmark-alist)
              (let ((index  (bmkp-list-position bookmark bookmark-alist #'eq)))
                (if index
                    (nth (mod (+ increment index) (length bookmark-alist)) bookmark-alist)
                  (message "bmkp-cycle-1: Bookmark `%s' is not in navlist"
                           (bookmark-name-from-full-record bmkp-current-nav-bookmark))
                  (car bookmark-alist)))))
      (let ((bookmark-automatically-show-annotations ; Prevent possible frame focus change.
             (and bookmark-automatically-show-annotations (not pop-up-frames))))
        (if other-window
            (bookmark-jump-other-window (bookmark-name-from-full-record bmkp-current-nav-bookmark))
          (save-selected-window (bookmark-name-from-full-record
                                 (bookmark-jump bmkp-current-nav-bookmark))))))
    (and bookmark bmkp-current-nav-bookmark))) ; Return nil if not a valid bookmark.

(defun bmkp-list-position (item items &optional test)
  "Find the first occurrence of ITEM in list ITEMS.
Return the index of the matching item, or nil if not found.
Items are compared using binary predicate TEST, or `equal' if TEST is
nil."
  (unless test (setq test  'equal))
  (let ((pos  0))
    (catch 'bmkp-list-position
      (dolist (itm  items)
        (when (funcall test item itm) (throw 'bmkp-list-position pos))
        (setq pos  (1+ pos)))
      nil)))

;;;###autoload
(defun bmkp-next-bookmark (n &optional startoverp) ; You can bind this to a repeatable key
  "Jump to the Nth next bookmark in the bookmark navigation list.
N defaults to 1, meaning the next bookmark.
Plain `C-u' means start over at first bookmark.
See also `bmkp-cycle'."
  (interactive (let ((startovr  (consp current-prefix-arg)))
                 (list (if startovr 1 (prefix-numeric-value current-prefix-arg)) startovr)))
  (bmkp-cycle n nil startoverp))

;;;###autoload
(defun bmkp-previous-bookmark (n &optional startoverp) ; You can bind this to a repeatable key
  "Jump to the Nth previous bookmark in the bookmark navigation list.
See `bmkp-next-bookmark'."
  (interactive (let ((startovr  (consp current-prefix-arg)))
                 (list (if startovr 1 (prefix-numeric-value current-prefix-arg)) startovr)))
  (bmkp-cycle (- n) nil startoverp))

;;;###autoload
(defun bmkp-next-bookmark-repeat (arg)  ; `C-x p right', `C-x p f', `C-x p C-f'
  "Jump to the Nth-next bookmark in the bookmark navigation list.
This is a repeatable version of `bmkp-next-bookmark'.
N defaults to 1, meaning the next bookmark.
Plain `C-u' means start over at the first bookmark (and no repeat)."
  (interactive "P")
  (require 'repeat)
  (bmkp-repeat-command 'bmkp-next-bookmark))

;;;###autoload
(defun bmkp-previous-bookmark-repeat (arg) ; `C-x p left', `C-x p b', `C-x p C-b'
  "Jump to the Nth-previous bookmark in the bookmark navigation list.
See `bmkp-next-bookmark-repeat'."
  (interactive "P")
  (require 'repeat)
  (bmkp-repeat-command 'bmkp-previous-bookmark))

;;;###autoload
(defun bmkp-next-bookmark-this-buffer (n &optional startoverp) ; Bind to repeatable key, e.g. `S-f2'
  "Jump to the Nth-next bookmark in the current buffer.
N defaults to 1, meaning the next one.
Plain `C-u' means start over at the first one.
See also `bmkp-cycle-this-buffer'."
  (interactive (let ((startovr  (consp current-prefix-arg)))
                 (list (if startovr 1 (prefix-numeric-value current-prefix-arg)) startovr)))
  (bmkp-cycle-this-buffer n nil startoverp))

;;;###autoload
(defun bmkp-previous-bookmark-this-buffer (n &optional startoverp) ; Bind to repeatable key, e.g. `f2'
  "Jump to the Nth-previous bookmark in the current buffer.
See `bmkp-next-bookmark-this-buffer'."
  (interactive (let ((startovr  (consp current-prefix-arg)))
                 (list (if startovr 1 (prefix-numeric-value current-prefix-arg)) startovr)))
  (bmkp-cycle-this-buffer (- n) nil startoverp))

;;;###autoload
(defun bmkp-next-bookmark-this-buffer-repeat (arg) ; `C-x p down', `C-x p n', `C-x p C-n'
  "Jump to the Nth next bookmark in the current buffer.
This is a repeatable version of `bmkp-next-bookmark-this-buffer'.
N defaults to 1, meaning the next one.
Plain `C-u' means start over at the first one (and no repeat)."
  (interactive "P")
  (require 'repeat)
  (bmkp-repeat-command 'bmkp-next-bookmark-this-buffer))

;;;###autoload
(defun bmkp-previous-bookmark-this-buffer-repeat (arg) ; `C-x p up', `C-x p p', `C-x p C-p'
  "Jump to the Nth previous bookmark in the current buffer.
See `bmkp-next-bookmark-this-buffer-repeat'."
  (interactive "P")
  (require 'repeat)
  (bmkp-repeat-command 'bmkp-previous-bookmark-this-buffer))

;;;###autoload
(defun bmkp-next-bookmark-w32 (n &optional startoverp)       ; You can bind this to a repeatable key
  "Windows `Open' the Nth next bookmark in the bookmark navigation list.
MS Windows only.  Invokes the program associated with the file type.
N defaults to 1, meaning the next one.
Plain `C-u' means start over at the first one.
See also `bmkp-cycle'."
  (interactive (let ((startovr  (consp current-prefix-arg)))
                 (list (if startovr 1 (prefix-numeric-value current-prefix-arg)) startovr)))
  (let ((bmkp-use-w32-browser-p  t))  (bmkp-cycle n nil startoverp)))

;;;###autoload
(defun bmkp-previous-bookmark-w32 (n &optional startoverp)   ; You can bind this to a repeatable key
  "Windows `Open' the Nth previous bookmark in the bookmark navlist.
See `bmkp-next-bookmark-w32'."
  (interactive (let ((startovr  (consp current-prefix-arg)))
                 (list (if startovr 1 (prefix-numeric-value current-prefix-arg)) startovr)))
  (let ((bmkp-use-w32-browser-p  t))  (bmkp-cycle (- n) nil startoverp)))

;;;###autoload
(defun bmkp-next-bookmark-w32-repeat (arg) ; `C-x p next'
  "Windows `Open' the Nth next bookmark in the bookmark navigation list.
This is a repeatable version of `bmkp-next-bookmark'.
N defaults to 1, meaning the next bookmark.
Plain `C-u' means start over at the first one (and no repeat)."
  (interactive "P")
  (require 'repeat)
  (let ((bmkp-use-w32-browser-p  t))  (bmkp-repeat-command 'bmkp-next-bookmark)))

;;;###autoload
(defun bmkp-previous-bookmark-w32-repeat (arg) ; `C-x p prior'
  "Windows `Open' the Nth previous bookmark in the bookmark navlist.
See `bmkp-next-bookmark-w32-repeat'."
  (interactive "P")
  (require 'repeat)
  (let ((bmkp-use-w32-browser-p  t))  (bmkp-repeat-command 'bmkp-previous-bookmark)))

;;;###autoload
(defun bmkp-toggle-autonamed-bookmark-set/delete (position &optional allp) ; Bound to `C-x p RET'
  "If there is an autonamed bookmark at point, delete it, else create one.
The bookmark created has no region.  Its name is formatted according
to option `bmkp-autoname-bookmark-function'.

With a prefix arg, delete *ALL* autonamed bookmarks for this buffer.

Non-interactively, act at POSITION, not point."
  (interactive "d\nP")
  (if allp
      (bmkp-delete-all-autonamed-for-this-buffer)
    (let ((bmk-name  (funcall bmkp-autoname-bookmark-function position)))
      (if (not (bookmark-get-bookmark bmk-name 'noerror))
          (let ((mark-active  nil))     ; Do not set a region bookmark.
            (bookmark-set bmk-name)
            (message "Set bookmark `%s'" bmk-name))
        (bookmark-delete bmk-name)
        (message "Deleted bookmark `%s'" bmk-name)))))

;;;###autoload
(defun bmkp-set-autonamed-bookmark (position &optional msgp)
  "Set an autonamed bookmark at point.
The bookmark created has no region.  Its name is formatted according
to option `bmkp-autoname-bookmark-function'.
Non-interactively, act at POSITION, not point."
  (interactive (list (point) t))
  (let ((bmk-name     (funcall bmkp-autoname-bookmark-function position))
        (mark-active  nil))             ; Do not set a region bookmark.
    (bookmark-set bmk-name)
    (when msgp (message "Set bookmark `%s'" bmk-name))))

;;;###autoload
(defun bmkp-set-autonamed-bookmark-at-line (number)
  "Set an autonamed bookmark at the beginning of the given line NUMBER."
  (interactive "nSet bookmark on line: ")
  (save-excursion
    (goto-char (point-min))
    (unless (zerop (forward-line (1- number)))
      (error "No such line: %d (%d lines total)" number (1+ (count-lines (point-min) (point-max)))))
    (bmkp-set-autonamed-bookmark (point))))

;;;###autoload
(defun bmkp-occur-create-autonamed-bookmarks ( &optional msgp)
  "Create an autonamed bookmark for each `occur' hit.
You can use this only in `Occur' mode (commands such as `occur' and
`multi-occur')."
  (interactive (list 'MSG))
  (unless (eq major-mode 'occur-mode) (error "You must be in `occur-mode'"))
  (let ((count  0))
    (save-excursion
      (goto-char (point-min))
      (while (condition-case nil (progn (occur-next) t) (error nil))
        (let* ((pos   (get-text-property (point) 'occur-target))
               (buf   (marker-buffer pos)))
          (with-current-buffer buf
            (goto-char pos)
            (bmkp-set-autonamed-bookmark (point)))
          (setq count  (1+ count)))))
    (when msgp (message "Created %d autonamed bookmarks" count))))

;;;###autoload
(defun bmkp-set-autonamed-regexp-buffer (regexp &optional msgp)
  "Set autonamed bookmarks at matches for REGEXP in the buffer."
  (interactive (list (read-string "Regexp: " nil 'regexp-history)
                     t))
  (bmkp-set-autonamed-regexp-region regexp (point-min) (point-max) 'MSG))

;;;###autoload
(defun bmkp-set-autonamed-regexp-region (regexp beg end &optional msgp)
  "Set autonamed bookmarks at matches for REGEXP in the region."
  (interactive (list (read-string "Regexp: " nil 'regexp-history)
                     (region-beginning) (region-end)
                     t))
  (let ((count  0))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp end t)
	(bmkp-set-autonamed-bookmark (point))
        (setq count  (1+ count))
	(forward-line 1)))
    (when msgp (message "Set %d autonamed bookmarks" count))))

(defun bmkp-autoname-bookmark (position)
  "Return a bookmark name using POSITION and the current buffer name.
The name is composed as follows:
 POSITION followed by a space and then the buffer name.
 The position value is prefixed with zeros to comprise 9 characters.
 For example, for POSITION value 31416 and current buffer `my-buffer',
 the name returned would be `000031416 my-buffer'"
  (format "%09d %s" (abs position) (buffer-name)))

;;;###autoload
(defun bmkp-delete-all-autonamed-for-this-buffer ()
  "Delete all autonamed bookmarks for the current buffer.
To be deleted, a bookmark name must be an autonamed bookmark whose
buffer part names the current buffer."
  (interactive)
  (let ((bmks-to-delete  (mapcar #'bookmark-name-from-full-record
                                 (bmkp-autonamed-this-buffer-alist-only))))
    (if (null bmks-to-delete)
        (message "No autonamed bookmarks for buffer `%s'" (buffer-name))
      (when (y-or-n-p (format "Delete ALL autonamed bookmarks for buffer `%s'? " (buffer-name)))
        (dolist (bmk  bmks-to-delete)  (bookmark-delete bmk))
        (message "Deleted all bookmarks for buffer `%s'" (buffer-name))))))

;;;###autoload
(defun bmkp-delete-bookmarks (position allp &optional alist) ; Bound to `C-x p delete'
  "Delete some bookmarks at point or all bookmarks in the buffer.
With no prefix argument, delete some bookmarks at point.
If there is more than one, require confirmation for each.

With a prefix argument, delete *ALL* bookmarks in the current buffer.

Non-interactively, delete at POSITION.
Optional arg ALIST is the alist of bookmarks.  It defaults to
`bookmark-alist'."
  (interactive "d\nP")
  (let ((bmks-to-delete  (and allp (mapcar #'bookmark-name-from-full-record
                                           (bmkp-this-buffer-alist-only))))
        (bmks-deleted    ())
        bmk-pos)
    (cond ((and bmks-to-delete  (y-or-n-p (format "Delete ALL bookmarks in buffer `%s'? "
                                                  (buffer-name))))
           (dolist (bmk bmks-to-delete) (bookmark-delete bmk))
           (message "Deleted all bookmarks in buffer `%s'" (buffer-name)))
          (bmks-to-delete (message "Canceled - nothing deleted"))
          (allp (message "No bookmarks in buffer `%s' to delete" (buffer-name)))
          (t
           (dolist (bmk  (or alist bookmark-alist))
             (when (eq (setq bmk-pos  (bookmark-get-position bmk)) position)
               (add-to-list 'bmks-to-delete (bookmark-name-from-full-record bmk))))
           (if bmks-to-delete
               (cond ((cadr bmks-to-delete)
                      (dolist (bmk  bmks-to-delete)
                        (when (y-or-n-p (format "Delete bookmark `%s'? " bmk))
                          (bookmark-delete bmk)
                          (add-to-list 'bmks-deleted bmk)))
                      (message (if bmks-deleted
                                   (format "Deleted bookmarks: %s" bmks-deleted)
                                 "No bookmarks deleted")))
                     (t
                      (bookmark-delete (car bmks-to-delete))
                      (message "Deleted bookmark `%s'" (car bmks-to-delete))))
             (when (interactive-p) (message "No bookmarks at point to delete")))))))
 
;;(@* "Keymaps")
;;; Keymaps ----------------------------------------------------------

;; `bookmark-map'

;;;###autoload
(define-key ctl-x-map "p" bookmark-map)
;;;###autoload
(define-key ctl-x-map "pj" 'bookmark-jump-other-window)
;;;###autoload
(define-key ctl-x-map "rK" 'bmkp-set-desktop-bookmark)
;;;###autoload
(define-key bookmark-map "0"      'bmkp-empty-file)
;;;###autoload
(define-key bookmark-map "B"      'bmkp-choose-navlist-from-bookmark-list)
;;;###autoload
(define-key bookmark-map "I"      'bookmark-insert-location) ; The original in `bookmark.el' was `f'.
;;;###autoload
(define-key bookmark-map "K"      'bmkp-set-desktop-bookmark)
;;;###autoload
(define-key bookmark-map "L"      'bmkp-switch-bookmark-file)
;;;###autoload
(define-key bookmark-map "N"      'bmkp-navlist-bmenu-list)
;;;###autoload
(define-key bookmark-map "o"      'bookmark-jump-other-window)
;;;###autoload
(define-key bookmark-map "q"      'bookmark-jump-other-window)
;;;###autoload
(define-key bookmark-map "x"      'bmkp-set-bookmark-file-bookmark)
;;;###autoload
(when (featurep 'bookmark+-lit)
  (define-key bookmark-map "h"    'bmkp-light-bookmark-this-buffer)
  (define-key bookmark-map "H"    'bmkp-light-bookmarks)
  (define-key bookmark-map "u"    'bmkp-unlight-bookmark-this-buffer)
  (define-key bookmark-map "U"    'bmkp-unlight-bookmarks)
  (define-key bookmark-map "\C-u" 'bmkp-unlight-bookmark-here)
  (define-key bookmark-map "="    'bmkp-bookmarks-lighted-at-point))
;;;###autoload
(define-key bookmark-map "."      'bmkp-this-buffer-bmenu-list)
;;;###autoload
(define-key bookmark-map "?"      'bmkp-describe-bookmark)
;;;###autoload
(define-key bookmark-map ":"      'bmkp-choose-navlist-of-type)
;;;###autoload
(define-key bookmark-map "\r"     'bmkp-toggle-autonamed-bookmark-set/delete)
;;;###autoload
(define-key bookmark-map [delete] 'bmkp-delete-bookmarks)

;; If you use Emacs before Emacs 22, then you will want to bind the commands
;; whose names do *not* end in `-repeat' to keys that are easily repeatable.
;; For example, you might want to bind `bmkp-next-bookmark-this-buffer'
;; (not `bmkp-next-bookmark-this-buffer-repeat') to a key such as [f2].
;;
;;;###autoload
(when (> emacs-major-version 21)
  (define-key bookmark-map [down]       'bmkp-next-bookmark-this-buffer-repeat)
  (define-key bookmark-map "n"          'bmkp-next-bookmark-this-buffer-repeat)
  (define-key bookmark-map "\C-n"       'bmkp-next-bookmark-this-buffer-repeat)

  ;; This will have to wait for the fix I provided for Emacs bug #6542.
  ;; Until then, you can at least bind the wheel event to `bmkp-next-bookmark-this-buffer'
  ;; in the global map.  IOW, for now a mouse event won't work with `repeat'.
  ;;   (define-key bookmark-map (vector (list mouse-wheel-up-event))
  ;;     'bmkp-next-bookmark-this-buffer-repeat)
  (define-key bookmark-map [up]         'bmkp-previous-bookmark-this-buffer-repeat)
  (define-key bookmark-map "p"          'bmkp-previous-bookmark-this-buffer-repeat)
  (define-key bookmark-map "\C-p"       'bmkp-previous-bookmark-this-buffer-repeat)

  ;; This will have to wait for the fix I provided for Emacs bug #6542.
  ;; Until then, you can at least bind the wheel event to `bmkp-previous-bookmark-this-buffer'
  ;; in the global map.  IOW, for now a mouse event won't work with `repeat'.
  ;;   (define-key bookmark-map (vector (list mouse-wheel-down-event))
  ;;     'bmkp-previous-bookmark-this-buffer-repeat)
  (define-key bookmark-map [right]      'bmkp-next-bookmark-repeat)
  (define-key bookmark-map "f"          'bmkp-next-bookmark-repeat)
  (define-key bookmark-map "\C-f"       'bmkp-next-bookmark-repeat)
  (define-key bookmark-map [left]       'bmkp-previous-bookmark-repeat)
  (define-key bookmark-map "b"          'bmkp-previous-bookmark-repeat)
  (define-key bookmark-map "\C-b"       'bmkp-previous-bookmark-repeat)
  (define-key bookmark-map [next]       'bmkp-next-bookmark-w32-repeat)
  (define-key bookmark-map [prior]      'bmkp-previous-bookmark-w32-repeat)
  (when (featurep 'bookmark+-lit)
    (define-key bookmark-map [C-down]   'bmkp-next-lighted-this-buffer-repeat)
    (define-key bookmark-map [C-up]     'bmkp-previous-lighted-this-buffer-repeat)))

;; `bookmark-bmenu-mode-map'

;;;###autoload
(when (< emacs-major-version 21)
  (define-key bookmark-bmenu-mode-map (kbd "RET")          'bookmark-bmenu-this-window))
;;;###autoload
(define-key bookmark-bmenu-mode-map "."                    'bmkp-bmenu-show-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map ">"                    'bmkp-bmenu-toggle-show-only-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "<"                    'bmkp-bmenu-toggle-show-only-unmarked)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "M-<DEL>")        'bmkp-bmenu-unmark-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "="                    nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "=bM"                  'bmkp-bmenu-mark-specific-buffer-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "=fM"                  'bmkp-bmenu-mark-specific-file-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "=bS"                  'bmkp-bmenu-show-only-specific-buffer)
;;;###autoload
(define-key bookmark-bmenu-mode-map "=fS"                  'bmkp-bmenu-show-only-specific-file)
;;;###autoload
(define-key bookmark-bmenu-mode-map "%"                    nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "%m"                   'bmkp-bmenu-regexp-mark)
;;;###autoload
(define-key bookmark-bmenu-mode-map "*"                    nil) ; For Emacs 20
;;;###autoload
(when (< emacs-major-version 21)
  (define-key bookmark-bmenu-mode-map "*m"                 'bookmark-bmenu-mark))
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-a"                 'bmkp-bmenu-search-marked-bookmarks-regexp)
;;;###autoload
(define-key bookmark-bmenu-mode-map "B"                    nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "BM"                   'bmkp-bmenu-mark-non-file-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "BS"                   'bmkp-bmenu-show-only-non-files)
;;;###autoload
(define-key bookmark-bmenu-mode-map "c"                    'bmkp-bmenu-define-command)
;;;###autoload
(define-key bookmark-bmenu-mode-map "C"                    'bmkp-bmenu-define-full-snapshot-command)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-c"                 'bmkp-bmenu-define-jump-marked-command)
;;;###autoload
(define-key bookmark-bmenu-mode-map "D"                    'bmkp-bmenu-delete-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-d"                 nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-d>"                'bmkp-bmenu-dired-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-d\M-m"             'bmkp-bmenu-mark-dired-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-d\M-s"             'bmkp-bmenu-show-only-dired)
;;;###autoload
(define-key bookmark-bmenu-mode-map "E"                    'bmkp-bmenu-edit-bookmark)
;;;###autoload
(define-key bookmark-bmenu-mode-map "F"                    nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "FM"                   'bmkp-bmenu-mark-file-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "FS"                   'bmkp-bmenu-show-only-files)
;;;###autoload
(define-key bookmark-bmenu-mode-map "g"                    'bmkp-bmenu-refresh-menu-list)
;;;###autoload
(define-key bookmark-bmenu-mode-map "G"                    nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "GM"                   'bmkp-bmenu-mark-gnus-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "GS"                   'bmkp-bmenu-show-only-gnus)
;;;###autoload
(if (fboundp 'command-remapping)
    (define-key bookmark-bmenu-mode-map [remap describe-mode] 'bmkp-bmenu-mode-status-help)
  ;; In Emacs < 22, the `substitute-...' affects only `?', not `C-h m', so we add it separately.
  (substitute-key-definition 'describe-mode 'bmkp-bmenu-mode-status-help bookmark-bmenu-mode-map)
  (define-key bookmark-bmenu-mode-map "\C-hm"              'bmkp-bmenu-mode-status-help))
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "C-h >")          'bmkp-bmenu-describe-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "C-h RET")        'bmkp-bmenu-describe-this-bookmark)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "C-h C-<return>") 'bmkp-bmenu-describe-this-bookmark)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "C-<down>")       'bmkp-bmenu-describe-this+move-down)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "C-<up>")         'bmkp-bmenu-describe-this+move-up)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "M-<return>")     'bmkp-bmenu-w32-open)
;;;###autoload
(define-key bookmark-bmenu-mode-map [M-mouse-2]            'bmkp-bmenu-w32-open-with-mouse)
;;;###autoload
(when (featurep 'bookmark+-lit)
  (define-key bookmark-bmenu-mode-map "H"                  nil) ; For Emacs 20
  (define-key bookmark-bmenu-mode-map "H+"                 'bmkp-bmenu-set-lighting)
  (define-key bookmark-bmenu-mode-map "H>+"                'bmkp-bmenu-set-lighting-for-marked)
  (define-key bookmark-bmenu-mode-map "H>H"                'bmkp-bmenu-light-marked)
  (define-key bookmark-bmenu-mode-map "HH"                 'bmkp-bmenu-light)
  (define-key bookmark-bmenu-mode-map "HM"                 'bmkp-bmenu-mark-lighted-bookmarks)
  (define-key bookmark-bmenu-mode-map "HS"                 'bmkp-bmenu-show-only-lighted)
  (define-key bookmark-bmenu-mode-map "H>U"                'bmkp-bmenu-unlight-marked)
  (define-key bookmark-bmenu-mode-map "HU"                 'bmkp-bmenu-unlight))
;;;###autoload
(define-key bookmark-bmenu-mode-map "I"                    nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "IM"                   'bmkp-bmenu-mark-info-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "IS"                   'bmkp-bmenu-show-only-info-nodes)
;;;###autoload
(define-key bookmark-bmenu-mode-map "K"                    nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "KM"                   'bmkp-bmenu-mark-desktop-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "KS"                   'bmkp-bmenu-show-only-desktops)
;;;###autoload
(define-key bookmark-bmenu-mode-map "L"                    'bmkp-switch-bookmark-file)
;;;###autoload
(define-key bookmark-bmenu-mode-map "M"                    nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "MM"                   'bmkp-bmenu-mark-man-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "MS"                   'bmkp-bmenu-show-only-man-pages)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-m"                 'bmkp-bmenu-mark-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "O"                    nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "O>"                   'bmkp-bmenu-omit/unomit-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "OS"                   'bmkp-bmenu-show-only-omitted)
;;;###autoload
(define-key bookmark-bmenu-mode-map "OU"                   'bmkp-unomit-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "P"                    nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "PB"               'bmkp-bmenu-filter-bookmark-name-incrementally)
;;;###autoload
(define-key bookmark-bmenu-mode-map "PF"                   'bmkp-bmenu-filter-file-name-incrementally)
;;;###autoload
(define-key bookmark-bmenu-mode-map "PT"                   'bmkp-bmenu-filter-tags-incrementally)
;;;###autoload
(define-key bookmark-bmenu-mode-map "q"                    'bmkp-bmenu-quit)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-q"          'bmkp-bmenu-query-replace-marked-bookmarks-regexp)
;;;###autoload
(define-key bookmark-bmenu-mode-map "R"                    nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "RM"                   'bmkp-bmenu-mark-region-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "RS"                   'bmkp-bmenu-show-only-regions)
;;;###autoload
(when (fboundp 'bookmark-bmenu-relocate)
  (define-key bookmark-bmenu-mode-map "\M-r"               'bookmark-bmenu-relocate)) ; `R' in Emacs
;;;###autoload
(define-key bookmark-bmenu-mode-map "S"                    'bookmark-bmenu-save) ; `s' in Emacs
;;;###autoload
(define-key bookmark-bmenu-mode-map "s"                    nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "s>"                   'bmkp-bmenu-sort-marked-before-unmarked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "s0"                   'bmkp-bmenu-sort-by-creation-time)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sb"               'bmkp-bmenu-sort-by-last-buffer-or-file-access)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sfd"                  'bmkp-bmenu-sort-by-local-file-type)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sfn"                  'bmkp-bmenu-sort-by-file-name)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sfs"                  'bmkp-bmenu-sort-by-local-file-size)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sft"                  'bmkp-bmenu-sort-by-last-local-file-access)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sfu"                  'bmkp-bmenu-sort-by-last-local-file-update)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sg"                   'bmkp-bmenu-sort-by-Gnus-thread)
;;;###autoload
(define-key bookmark-bmenu-mode-map "si"                   'bmkp-bmenu-sort-by-Info-location)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sk"                   'bmkp-bmenu-sort-by-bookmark-type)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sn"                   'bmkp-bmenu-sort-by-bookmark-name)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sr"                   'bmkp-reverse-sort-order)
;;;###autoload
(define-key bookmark-bmenu-mode-map "s\C-r"                'bmkp-reverse-multi-sort-order)
;;;###autoload
(define-key bookmark-bmenu-mode-map "ss"                   'bmkp-bmenu-change-sort-order-repeat)
;;;###autoload
(define-key bookmark-bmenu-mode-map "st"                   'bmkp-bmenu-sort-by-last-bookmark-access)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sv"                 'bmkp-bmenu-sort-by-bookmark-visit-frequency)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sw"                   'bmkp-bmenu-sort-by-w3m-url)
;;;###autoload
(when (> emacs-major-version 22)        ; Emacs 23+
 (define-key bookmark-bmenu-mode-map (kbd "M-s a C-s")     'bmkp-bmenu-isearch-marked-bookmarks)
 (define-key bookmark-bmenu-mode-map (kbd "M-s a M-C-s") 'bmkp-bmenu-isearch-marked-bookmarks-regexp))
;;;###autoload
(define-key bookmark-bmenu-mode-map "T"                    nil) ; For Emacs20
;;;###autoload
(define-key bookmark-bmenu-mode-map "T0"                   'bmkp-remove-all-tags)
;;;###autoload
(define-key bookmark-bmenu-mode-map "T+"                   'bmkp-add-tags)
;;;###autoload
(define-key bookmark-bmenu-mode-map "T-"                   'bmkp-remove-tags)
;;;###autoload
(define-key bookmark-bmenu-mode-map "T>+"                  'bmkp-bmenu-add-tags-to-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "T>-"                  'bmkp-bmenu-remove-tags-from-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Td"                   'bmkp-remove-tags-from-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tl"                   'bmkp-list-all-tags)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tm*"                  'bmkp-bmenu-mark-bookmarks-tagged-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tm%"                  'bmkp-bmenu-mark-bookmarks-tagged-regexp)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tm+"                  'bmkp-bmenu-mark-bookmarks-tagged-some)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tm~*"                 'bmkp-bmenu-mark-bookmarks-tagged-not-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tm~+"                 'bmkp-bmenu-mark-bookmarks-tagged-none)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tr"                   'bmkp-rename-tag)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Ts"                   'bmkp-define-tags-sort-command)
;;;###autoload
(define-key bookmark-bmenu-mode-map "TS"                   'bmkp-bmenu-show-only-tagged)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tu*"                  'bmkp-bmenu-unmark-bookmarks-tagged-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tu+"                  'bmkp-bmenu-unmark-bookmarks-tagged-some)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tu~*"                'bmkp-bmenu-unmark-bookmarks-tagged-not-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tu~+"                 'bmkp-bmenu-unmark-bookmarks-tagged-none)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tv"                   'bmkp-bmenu-set-tag-value)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-l"                 'bmkp-toggle-saving-menu-list-state)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-~"                 'bmkp-toggle-saving-bookmark-file)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-t"            'bookmark-bmenu-toggle-filenames) ; `t' in Emacs
;;;###autoload
(define-key bookmark-bmenu-mode-map "t"                    'bmkp-bmenu-toggle-marks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "U"                    'bmkp-bmenu-unmark-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "V"                    nil) ; For Emacs20
;;;###autoload
(define-key bookmark-bmenu-mode-map "VS"                   'bmkp-bmenu-show-only-varlists)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-o"                 'bmkp-bmenu-w32-open-select)
;;;###autoload
(define-key bookmark-bmenu-mode-map "W"                    nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "WM"                   'bmkp-bmenu-mark-w3m-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "WS"                   'bmkp-bmenu-show-only-w3m-urls)
;;;###autoload
(define-key bookmark-bmenu-mode-map "X"                    nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "XM"                   'bmkp-bmenu-mark-bookmark-file-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "XS"                   'bmkp-bmenu-show-only-bookmark-files)


;; `bookmark-jump-map' and `bmkp-jump-other-window-map'

(defvar bmkp-jump-map nil "Keymap containing bindings for bookmark jump commands.")

(defvar bmkp-jump-other-window-map nil
  "Keymap containing bindings for bookmark jump other-window commands.")

;;;###autoload
(define-prefix-command 'bmkp-jump-map)
;;;###autoload
(define-prefix-command 'bmkp-jump-other-window-map)
;;;###autoload
(define-key ctl-x-map   "j" bmkp-jump-map)
;;;###autoload
(define-key ctl-x-4-map "j" bmkp-jump-other-window-map)

;;;###autoload
(define-key bmkp-jump-map              "."    'bmkp-this-buffer-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "."    'bmkp-this-buffer-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "="    nil) ; For Emacs 20
;;;###autoload
(define-key bmkp-jump-other-window-map "="    nil) ; For Emacs 20
;;;###autoload
(define-key bmkp-jump-map              "=b"   'bmkp-specific-buffers-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "=b"   'bmkp-specific-buffers-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "=f"   'bmkp-specific-files-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "=f"   'bmkp-specific-files-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "b"    'bmkp-non-file-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "b"    'bmkp-non-file-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "B"    'bmkp-bookmark-list-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "B"    'bmkp-bookmark-list-jump) ; Same
;;;###autoload
(define-key bmkp-jump-map              "d"    'bmkp-dired-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "d"    'bmkp-dired-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "f"    'bmkp-file-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "f"    'bmkp-file-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "g"    'bmkp-gnus-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "g"    'bmkp-gnus-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "h"    'bmkp-lighted-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "h"    'bmkp-lighted-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "i"    'bmkp-info-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "i"    'bmkp-info-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "j"    'bookmark-jump)
(put 'bookmark-jump :advertised-binding "\C-xjj")
;;;###autoload
(define-key bmkp-jump-other-window-map "j"    'bookmark-jump-other-window)
(put 'bookmark-jump-other-window :advertised-binding "\C-x4jj")
(put 'jump-other :advertised-binding "\C-x4jj")
;;;###autoload
(define-key bmkp-jump-map              "K"    'bmkp-desktop-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "K"    'bmkp-desktop-jump) ; Same
;;;###autoload
(define-key bmkp-jump-map              "l"    'bmkp-local-file-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "l"    'bmkp-local-file-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "m"    'bmkp-man-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "m"    'bmkp-man-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "n"    'bmkp-remote-file-jump) ; "_n_etwork"
;;;###autoload
(define-key bmkp-jump-other-window-map "n"    'bmkp-remote-file-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "N"    'bmkp-jump-in-navlist)
;;;###autoload
(define-key bmkp-jump-other-window-map "N"    'bmkp-jump-in-navlist-other-window)
;;;###autoload
(define-key bmkp-jump-map              "r"    'bmkp-region-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "r"    'bmkp-region-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "t"    nil) ; For Emacs 20
;;;###autoload
(define-key bmkp-jump-other-window-map "t"    nil) ; For Emacs 20
;;;###autoload
(define-key bmkp-jump-map              "t*"   'bmkp-all-tags-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "t*"   'bmkp-all-tags-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "t+"   'bmkp-some-tags-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "t+"   'bmkp-some-tags-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "t%"   nil) ; For Emacs 20
;;;###autoload
(define-key bmkp-jump-other-window-map "t%"   nil) ; For Emacs 20
;;;###autoload
(define-key bmkp-jump-map              "t%*"  'bmkp-all-tags-regexp-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "t%*"  'bmkp-all-tags-regexp-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "t%+"  'bmkp-some-tags-regexp-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "t%+"  'bmkp-some-tags-regexp-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "v"    'bmkp-varlist-jump)
;;;###autoload
(define-key bmkp-jump-map              "w"    'bmkp-w3m-jump)
;;;###autoload
(define-key bmkp-jump-other-window-map "w"    'bmkp-w3m-jump-other-window)
;;;###autoload
(define-key bmkp-jump-map              "x"    'bmkp-bookmark-file-jump)
;;;###autoload
(define-key bmkp-jump-map              ":"    'bmkp-jump-to-type)
;;;###autoload
(define-key bmkp-jump-other-window-map ":"    'bmkp-jump-to-type-other-window)


;; Add jump commands to other keymaps: Buffer-menu, Dired, Gnus, Info, Man, Woman, W3M.
(add-hook 'buffer-menu-mode-hook
          #'(lambda () (unless (lookup-key Buffer-menu-mode-map "j")
                         (define-key Buffer-menu-mode-map "j" 'bmkp-non-file-jump))))
(add-hook 'dired-mode-hook
          #'(lambda ()
              (let ((now  (lookup-key dired-mode-map "J")))
                ;; Uppercase, since `j' is `dired-goto-file'.
                (unless (and now (not (eq now 'undefined))) ; `dired+.el' uses `undefined'.
                  (define-key dired-mode-map "J" 'bmkp-dired-jump))
                (setq now  (lookup-key dired-mode-map "\C-j"))
                (unless (and now (not (eq now 'undefined))) ; `dired+.el' uses `undefined'.
                  (define-key dired-mode-map "\C-j" 'bmkp-dired-jump-current)))
              (let ((map   dired-mode-map)
                    (sep   '(menu-bar subdir separator-bmkp))
                    (bdj   '(menu-bar subdir bmkp-dired-jump))
                    (bdjc  '(menu-bar subdir bmkp-dired-jump-current)))
                (when (boundp 'diredp-menu-bar-subdir-menu) ; In `dired+el'.
                  (setq map   diredp-menu-bar-subdir-menu
                        sep   (cddr sep)
                        bdj   (cddr bdj)
                        bdjc  (cddr bdjc)))
                (define-key map (apply #'vector sep) '("--"))
                (define-key map (apply #'vector bdj)
                  '(menu-item "Jump to a Dired Bookmark" bmkp-dired-jump
                    :help "Jump to a bookmarked Dired buffer"))
                (define-key map (apply #'vector bdjc)
                  '(menu-item "Show This Dir Using a Bookmark" bmkp-dired-jump-current
                    :help "Use a bookmarked version of this directory")))))
(add-hook 'gnus-summary-mode-hook
          #'(lambda () (unless (lookup-key gnus-summary-mode-map "j")
                         (define-key gnus-summary-mode-map "j" 'bmkp-gnus-jump))))
(add-hook 'Info-mode-hook
          #'(lambda ()
              (unless (lookup-key Info-mode-map "j")
                (define-key Info-mode-map "j" 'bmkp-info-jump))
              (define-key-after Info-mode-menu [bmkp-info-jump]
                '(menu-item "Jump to an Info Bookmark" bmkp-info-jump
                  :help "Jump to a bookmarked Info node")
                'Go\ to\ Node\.\.\.)))  ; Used by `info(+).el' - corresponds to `Info-goto-node'.
(add-hook 'Man-mode-hook
          #'(lambda () (unless (lookup-key Man-mode-map "j")
                         (define-key Man-mode-map "j" 'bmkp-man-jump))))
(add-hook 'woman-mode-hook
          #'(lambda ()
              (unless (lookup-key woman-mode-map "j") (define-key woman-mode-map "j" 'bmkp-man-jump))
              (when (boundp 'woman-menu)
                (define-key-after woman-menu [bmkp-man-jump]
                  '(menu-item "Jump to a `man'-page Bookmark" bmkp-man-jump
                    :help "Jump to a bookmarked `man' page")
                  'WoMan\.\.\.))))      ; Used by `woman.el' - corresponds to command `woman'.
(add-hook 'w3m-minor-mode-hook
          #'(lambda () (unless (lookup-key w3m-minor-mode-map "j")
                         (define-key w3m-minor-mode-map "j" 'bmkp-w3m-jump))))
(add-hook 'w3m-mode-hook
          #'(lambda () (unless (lookup-key w3m-mode-map "j")
                         (define-key w3m-mode-map "j" 'bmkp-w3m-jump))))


;; Add `bmkp-occur-create-autonamed-bookmarks' to `occur-mode-map' as `C-c b'.
(add-hook 'occur-mode-hook
          #'(lambda () (unless (lookup-key occur-mode-map "\C-cb")
                         (define-key occur-mode-map "\C-cb" 'bmkp-occur-create-autonamed-bookmarks))))


;;; Vanilla Emacs `Bookmarks' menu (see also [jump] from `Bookmark+' menu, below).

(define-key-after menu-bar-bookmark-map [set]
  '(menu-item "Set Bookmark..." (lambda () (interactive) (call-interactively #'bookmark-set))
    :help "Set a bookmark at point")
  'jump)
(define-key-after menu-bar-bookmark-map [bmkp-toggle-autoname-bookmark-set]
  '(menu-item "Set Autonamed Bookmark" bmkp-toggle-autonamed-bookmark-set/delete
    :help "Set an autonamed bookmark at point"
    :visible (not (bookmark-get-bookmark (funcall bmkp-autoname-bookmark-function (point))
                   'noerror)))
  'set)
(define-key-after menu-bar-bookmark-map [bmkp-toggle-autoname-bookmark-delete]
  '(menu-item "Delete Autonamed Bookmark" bmkp-toggle-autonamed-bookmark-set/delete
    :help "Delete the autonamed bookmark at point"
    :visible (bookmark-get-bookmark (funcall bmkp-autoname-bookmark-function (point))
              'noerror))
  'set)
(define-key-after menu-bar-bookmark-map [bmkp-delete-all-autonamed-for-this-buffer]
  '(menu-item "Delete All Autonamed Bookmarks Here..."
    bmkp-delete-all-autonamed-for-this-buffer
    :help "Delete all autonamed bookmarks for the current buffer"
    :enable (mapcar #'bookmark-name-from-full-record (bmkp-autonamed-this-buffer-alist-only)))
  'bmkp-toggle-autonamed-bookmark-set/delete)
(define-key-after menu-bar-bookmark-map [bmkp-delete-bookmarks]
  '(menu-item "Delete Bookmarks Here..." bmkp-delete-bookmarks
    :help "Delete some bookmarks at point or, with `C-u', all bookmarks in the buffer"
    :enable (mapcar #'bookmark-name-from-full-record (bmkp-this-buffer-alist-only)))
  'bmkp-delete-all-autonamed-for-this-buffer)
(define-key-after menu-bar-bookmark-map [delete]
  '(menu-item "Delete Bookmark..." bookmark-delete
    :help "Delete the bookmark you choose by name" :enable bookmark-alist)
  'bmkp-delete-bookmarks)
(define-key-after menu-bar-bookmark-map [rename]
  '(menu-item "Rename Bookmark..." bookmark-rename
    :help "Rename the bookmark you choose by name" :enable bookmark-alist)
  'delete)

(define-key-after menu-bar-bookmark-map [separator-0] '("--") 'rename)
(define-key-after menu-bar-bookmark-map [edit]
  '(menu-item "Bookmark List" bookmark-bmenu-list
    :help "Open the list of bookmarks in buffer `*Bookmark List*'")
  'separator-0)
(define-key-after menu-bar-bookmark-map [bmkp-this-buffer-bmenu-list]
  '(menu-item "Bookmark List for This Buffer" bmkp-this-buffer-bmenu-list
    :help "Open `*Bookmark List*' for the bookmarks in the current buffer (only)"
    :enable (mapcar #'bookmark-name-from-full-record (bmkp-this-buffer-alist-only)))
  'edit)
(define-key-after menu-bar-bookmark-map [bmkp-navlist-bmenu-list]
  '(menu-item "Bookmark List for Navlist" bmkp-navlist-bmenu-list
    :help "Open `*Bookmark List*' for bookmarks in navlist (only)"
    :enable bmkp-nav-alist)
  'bmkp-this-buffer-bmenu-list)

(define-key-after menu-bar-bookmark-map [separator-2] '("--") 'bmkp-navlist-bmenu-list)
(define-key-after menu-bar-bookmark-map [bmkp-choose-navlist-of-type]
  '(menu-item "Set Navlist to Bookmarks of Type..." bmkp-choose-navlist-of-type
    :help "Set the navigation list to the bookmarks of a certain type")
  'separator-2)
(define-key-after menu-bar-bookmark-map [bmkp-choose-navlist-from-bookmark-list]
  '(menu-item "Set Navlist from Bookmark-List Bookmark..." bmkp-choose-navlist-from-bookmark-list
    :help "Set the navigation list from a bookmark-list bookmark")
  'bmkp-choose-navlist-of-type)
(define-key-after menu-bar-bookmark-map [bmkp-list-defuns-in-commands-file]
  '(menu-item "List User-Defined Bookmark Commands" bmkp-list-defuns-in-commands-file
    :help "List the functions defined in `bmkp-bmenu-commands-file'"
    :enable (and bmkp-bmenu-commands-file (file-readable-p bmkp-bmenu-commands-file)))
  'bmkp-choose-navlist-from-bookmark-list)
(define-key-after menu-bar-bookmark-map [bmkp-make-function-bookmark]
  '(menu-item "New Function Bookmark..." bmkp-make-function-bookmark
    :help "Create a bookmark that will invoke FUNCTION when \"jumped\" to")
  'bmkp-list-defuns-in-commands-file)

(define-key-after menu-bar-bookmark-map [insert]
  '(menu-item "Insert Bookmark Contents..." bookmark-insert :help "Insert bookmarked text")
  'bmkp-make-function-bookmark)
(define-key-after menu-bar-bookmark-map [locate]
  '(menu-item "Insert Bookmark Location..." bookmark-locate ; Alias for `bookmark-insert-location'.
    :help "Insert a bookmark's file or buffer name")
  'insert)

(define-key-after menu-bar-bookmark-map [separator-3] '("--") 'locate)
(define-key-after menu-bar-bookmark-map [save]
  '(menu-item "Save Bookmarks" bookmark-save :help "Save currently defined bookmarks")
  'separator-3)
(define-key-after menu-bar-bookmark-map [write]
  '(menu-item "Save Bookmarks As..." bookmark-write
    :help "Write current bookmarks to a bookmark file")
  'save)
(define-key-after menu-bar-bookmark-map [bmkp-empty-file]
  '(menu-item "New (Empty) Bookmark File..." bmkp-empty-file
    :help "Create a new, empty bookmark file, or empty an existing bookmark file")
  'write)
(define-key-after menu-bar-bookmark-map [load]
  '(menu-item "Add Bookmarks from File..." bookmark-load
    :help "Load additional bookmarks from a bookmark file")
  'bmkp-empty-file)
(define-key-after menu-bar-bookmark-map [load-read-only]
  '(menu-item "Switch to Bookmark File..." bmkp-switch-bookmark-file
    :help "Switch to a different bookmark file, *replacing* the current set of bookmarks")
  'load)

(when (featurep 'bookmark+-lit)
  (defvar bmkp-highlight-menu (make-sparse-keymap "Highlight")
    "`Highlight' submenu for menu-bar `Bookmark' menu.")
  (define-key menu-bar-bookmark-map [highlight] (cons "Highlight" bmkp-highlight-menu))

  (define-key bmkp-highlight-menu [bmkp-unlight-bookmarks]
    '(menu-item "Unhighlight All" bmkp-unlight-bookmarks
      :help "Unhighlight all bookmarks (everywhere)."))
  (define-key bmkp-highlight-menu [bmkp-unlight-this-buffer]
    '(menu-item "Unhighlight All in Buffer" bmkp-unlight-this-buffer
      :help "Unhighlight all bookmarks in this buffer."))
  (define-key bmkp-highlight-menu [bmkp-unlight-non-autonamed-this-buffer]
    '(menu-item "Unhighlight All Non-Autonamed in Buffer" bmkp-unlight-non-autonamed-this-buffer
      :help "Unhighlight all non-autonamed bookmarks in this buffer."))
  (define-key bmkp-highlight-menu [bmkp-unlight-autonamed-this-buffer]
    '(menu-item "Unhighlight All Autonamed in Buffer" bmkp-unlight-autonamed-this-buffer
      :help "Unhighlight all autonamed bookmarks in this buffer."))
  (define-key bmkp-highlight-menu [bmkp-unlight-bookmark]
    '(menu-item "Unhighlight One..." bmkp-unlight-bookmark
      :help "Unhighlight a bookmark."))
  (define-key bmkp-highlight-menu [bmkp-unlight-bookmark-this-buffer]
    '(menu-item "Unhighlight One in Buffer..." bmkp-unlight-bookmark-this-buffer
      :help "Unhighlight a bookmark in this buffer."))
  (define-key bmkp-highlight-menu [bmkp-unlight-bookmark-here]
    '(menu-item "Unhighlight This One" bmkp-unlight-bookmark-here
      :help "Unhighlight a bookmark at point or on its line."))
  (define-key bmkp-highlight-menu [separator-1] '("--"))
  (define-key bmkp-highlight-menu [bmkp-light-bookmarks-in-region]
    '(menu-item "Highlight All in Region" bmkp-light-bookmarks-in-region
      :help "Highlight all bookmarks in the region."))
  (define-key bmkp-highlight-menu [bmkp-light-this-buffer]
    '(menu-item "Highlight All in Buffer" bmkp-light-this-buffer
      :help "Highlight all bookmarks in this buffer."))
  (define-key bmkp-highlight-menu [bmkp-light-non-autonamed-this-buffer]
    '(menu-item "Highlight All Non-Autonamed in Buffer" bmkp-light-non-autonamed-this-buffer
      :help "Highlight all non-autonamed bookmarks in this buffer."))
  (define-key bmkp-highlight-menu [bmkp-light-autonamed-this-buffer]
    '(menu-item "Highlight All Autonamed in Buffer" bmkp-light-autonamed-this-buffer
      :help "Highlight all autonamed bookmarks in this buffer."))
  (define-key bmkp-highlight-menu [bmkp-light-navlist-bookmarks]
    '(menu-item "Highlight All in Navigation List" bmkp-light-navlist-bookmarks
      :help "Highlight all bookmarks in the navigation list."))
  (define-key bmkp-highlight-menu [bmkp-light-bookmark-this-buffer]
    '(menu-item "Highlight One in Buffer..." bmkp-light-bookmark-this-buffer
      :help "Highlight a bookmark in this buffer."))
  (define-key bmkp-highlight-menu [bmkp-light-bookmark]
    '(menu-item "Highlight One..." bmkp-light-bookmark
      :help "Highlight a bookmark."))
  (define-key bmkp-highlight-menu [separator-0] '("--"))
  (define-key bmkp-highlight-menu [bmkp-next-lighted-this-buffer]
    '(menu-item "Next in Buffer" bmkp-next-lighted-this-buffer
      :help "Cycle to the next highlighted bookmark in this buffer."))
  (define-key bmkp-highlight-menu [bmkp-previous-lighted-this-buffer]
    '(menu-item "Previous in Buffer" bmkp-previous-lighted-this-buffer
      :help "Cycle to the previous highlighted bookmark in this buffer."))
  (define-key bmkp-highlight-menu [bmkp-bookmarks-lighted-at-point]
    '(menu-item "List Highlighted at Point" bmkp-bookmarks-lighted-at-point
      :help "List the bookmarks at point that are highlighted."))
  (define-key bmkp-highlight-menu [bmkp-set-lighting-for-bookmark]
    '(menu-item "Set Highlighting for One..." bmkp-set-lighting-for-bookmark
      :help "Set individual highlighting for a bookmark.")))

(defvar bmkp-options-menu (make-sparse-keymap "Toggle Option")
  "`Toggle Option' submenu for menu-bar `Bookmark' menu.")
(define-key menu-bar-bookmark-map [options] (cons "Toggle Option" bmkp-options-menu))

(define-key bmkp-options-menu [bmkp-crosshairs-flag]
  (bmkp-menu-bar-make-toggle bmkp-crosshairs-flag bmkp-crosshairs-flag
                             "Highlight Jump using Crosshairs"
                             "Crosshairs highlighting is %s"
                             "Temporarily highlight visited bookmarks using crosshairs"))
(define-key bmkp-options-menu [bmkp-save-new-location-flag]
  (bmkp-menu-bar-make-toggle bmkp-save-new-location-flag bmkp-save-new-location-flag
                             "Save after Relocating"
                             "Saving relocated bookmarks is %s"
                             "Save a bookmark after automatically relocating it"))
(define-key bmkp-options-menu [bmkp-prompt-for-tags-flag]
  (bmkp-menu-bar-make-toggle bmkp-prompt-for-tags-flag bmkp-prompt-for-tags-flag
                             "Prompt for Tags when Setting"
                             "Prompting for tags when setting a bookmark is %s"
                             "Prompt for tags when setting a bookmark interactively"))

(defvar bmkp-jump-menu (make-sparse-keymap "Jump To")
  "`Jump To' submenu for menu-bar `Bookmark' menu.")
;; Add jump menu to vanilla Emacs `Bookmarks' menu and remove the two jump commands already there.
(define-key menu-bar-bookmark-map [jump] nil)
(define-key menu-bar-bookmark-map [jump-other] nil)
(define-key menu-bar-bookmark-map [bmkp-jump] (cons "Jump To Bookmark" bmkp-jump-menu))

(define-key bmkp-jump-menu [bmkp-all-tags-regexp-jump-other-window]
  '(menu-item "All Tags Matching Regexp..." bmkp-all-tags-regexp-jump-other-window
    :help "Jump to a bookmark that has each tag matching a regexp that you enter"))
(define-key bmkp-jump-menu [bmkp-some-tags-regexp-jump-other-window]
  '(menu-item "Any Tag Matching Regexp..." bmkp-some-tags-regexp-jump-other-window
    :help "Jump to a bookmark that has at least one tag matching a regexp that you enter"))
(define-key bmkp-jump-menu [bmkp-all-tags-jump-other-window]
  '(menu-item "All Tags in Set..." bmkp-all-tags-jump-other-window
    :help "Jump to a bookmark that has all of a set of tags that you enter"))
(define-key bmkp-jump-menu [bmkp-some-tags-jump-other-window]
  '(menu-item "Any Tag in Set..." bmkp-some-tags-jump-other-window
    :help "Jump to a bookmark that has some of a set of tags that you enter"))
(define-key bmkp-jump-menu [jump-sep1] '("--"))
(define-key bmkp-jump-menu [bmkp-specific-files-jump-other-window]
  '(menu-item "For Specific Files..." bmkp-specific-files-jump-other-window
    :help "Jump to a bookmark for specific files"))
(define-key bmkp-jump-menu [bmkp-specific-buffers-jump-other-window]
  '(menu-item "For Specific Buffers..." bmkp-specific-buffers-jump-other-window
    :help "Jump to a bookmark for specific buffers"))
(define-key bmkp-jump-menu [bmkp-this-buffer-jump]
  '(menu-item "For This Buffer..." bmkp-this-buffer-jump
    :help "Jump to a bookmark for the current buffer"
    :enable (mapcar #'bookmark-name-from-full-record (bmkp-this-buffer-alist-only))))
(when (featurep 'bookmark+-lit)
  (define-key bmkp-jump-menu [bmkp-lighted-jump-other-window]
    '(menu-item "Highlighted..." bmkp-lighted-jump-other-window
      :help "Jump to a highlighted bookmark"
      :enable (bmkp-lighted-alist-only))))
(define-key bmkp-jump-menu [bmkp-jump-in-navlist-other-window]
  '(menu-item "In Navigation List..." bmkp-jump-in-navlist-other-window
    :help "Jump to a bookmark that is in the navigation list" :enable bmkp-nav-alist))
(define-key bmkp-jump-menu [bmkp-w3m-jump-other-window]
  '(menu-item "URL (W3M)..." bmkp-w3m-jump-other-window :help "Jump to an W3M bookmark"
    :enable (bmkp-w3m-alist-only)))
(define-key bmkp-jump-menu [bmkp-gnus-jump-other-window]
  '(menu-item "Gnus..." bmkp-gnus-jump-other-window :help "Jump to a Gnus bookmark"
    :enable (bmkp-gnus-alist-only)))
(define-key bmkp-jump-menu [bmkp-man-jump-other-window]
  '(menu-item "Man Page..." bmkp-man-jump-other-window :help "Jump to a `man'-page bookmark"
    :enable (bmkp-man-alist-only)))
(define-key bmkp-jump-menu [bmkp-info-jump-other-window]
  '(menu-item "Info Node..." bmkp-info-jump-other-window :help "Jump to an Info bookmark"
    :enable (bmkp-info-alist-only)))
(define-key bmkp-jump-menu [bmkp-non-file-jump-other-window]
  '(menu-item "Buffer (Non-File)..." bmkp-non-file-jump-other-window
    :help "Jump to a non-file (buffer) bookmark" :enable (bmkp-non-file-alist-only)))
(define-key bmkp-jump-menu [bmkp-region-jump-other-window]
  '(menu-item "Region..." bmkp-region-jump-other-window
    :help "Jump to a bookmark that defines the active region" :enable (bmkp-region-alist-only)))
(define-key bmkp-jump-menu [bmkp-remote-file-jump-other-window]
  '(menu-item "Remote File..." bmkp-remote-file-jump-other-window
    :help "Jump to a remote file or directory bookmark" :enable (bmkp-remote-file-alist-only)))
(define-key bmkp-jump-menu [bmkp-local-file-jump-other-window]
  '(menu-item "Local File..." bmkp-local-file-jump-other-window
    :help "Jump to a local file or directory bookmark" :enable (bmkp-local-file-alist-only)))
(define-key bmkp-jump-menu [bmkp-file-jump-other-window]
  '(menu-item "File..." bmkp-file-jump-other-window :help "Jump to a file or directory bookmark"
    :enable (bmkp-file-alist-only)))
(define-key bmkp-jump-menu [bmkp-dired-jump-other-window]
  '(menu-item "Dired..." bmkp-dired-jump-other-window
    :help "Jump to a Dired bookmark, restoring the recorded Dired state"
    :enable (bmkp-dired-alist-only)))
(define-key bmkp-jump-menu [bmkp-varlist-jump]
  '(menu-item "Variable List..." bmkp-varlist-jump :help "Jump to a variable-list bookmark"
    :enable (bmkp-varlist-alist-only)))
(define-key bmkp-jump-menu [bmkp-bookmark-file-jump]
  '(menu-item "Bookmark File..." bmkp-bookmark-file-jump
    :help "Jump to (load) a bookmark-file bookmark" :enable (bmkp-bookmark-file-alist-only)))
(define-key bmkp-jump-menu [bmkp-bookmark-list-jump]
  '(menu-item "Bookmark List..." bmkp-bookmark-list-jump :help "Jump to a bookmark-list bookmark"
    :enable (bmkp-bookmark-list-alist-only)))
(define-key bmkp-jump-menu [bmkp-desktop-jump]
  '(menu-item "Desktop..." bmkp-desktop-jump :help "Jump to a desktop bookmark"
    :enable (bmkp-desktop-alist-only)))
(define-key bmkp-jump-menu [bmkp-jump-to-type-other-window]
  '(menu-item "Of Type..." bmkp-jump-to-type-other-window
    :help "Jump to a bookmark of a type that you specify"))
(define-key bmkp-jump-menu [bookmark-jump-other-window]
  '(menu-item "Any in Other Window..." bookmark-jump-other-window
    :help "Jump to a bookmark of any type, in another window"))
(define-key bmkp-jump-menu [bookmark-jump]
  '(menu-item "Any..." bookmark-jump :help "Jump to a bookmark of any type, in this window"))


;;; `Bookmark+' menu-bar menu in `*Bookmark List*'

(defvar bmkp-bmenu-menubar-menu (make-sparse-keymap "Bookmark+") "`Boomark+' menu-bar menu.")
(define-key bookmark-bmenu-mode-map [menu-bar bmkp]
  (cons "Bookmark+" bmkp-bmenu-menubar-menu))

;; Top level
(define-key bmkp-bmenu-menubar-menu [bmkp-bmenu-quit]
  '(menu-item "Quit" bmkp-bmenu-quit
    :help "Quit the bookmark list, saving its state and the current set of bookmarks"))
(define-key bmkp-bmenu-menubar-menu [bmkp-bmenu-describe-marked]
  '(menu-item "Describe Marked Bookmarks" bmkp-bmenu-describe-marked
    :help "Describe the marked bookmarks.  With `C-u' show internal format."))
(define-key bmkp-bmenu-menubar-menu [bmkp-bmenu-describe-this-bookmark]
  '(menu-item "Describe This Bookmark" bmkp-bmenu-describe-this-bookmark
    :help "Describe this line's bookmark.  With `C-u' show internal format."))
(define-key bmkp-bmenu-menubar-menu [bmkp-bmenu-mode-status-help]
  '(menu-item "Current Status, Mode Help" bmkp-bmenu-mode-status-help :keys "?"
    :help "Describe `*Bookmark List*' and show its current status"))
(define-key bmkp-bmenu-menubar-menu [top-sep2] '("--"))
(define-key bmkp-bmenu-menubar-menu [bmkp-toggle-saving-menu-list-state]
  '(menu-item "Toggle Autosaving Display State" bmkp-toggle-saving-menu-list-state
    :help "Toggle the value of option `bmkp-bmenu-state-file'"))
(define-key bmkp-bmenu-menubar-menu [bmkp-toggle-saving-bookmark-file]
  '(menu-item "Toggle Autosaving Bookmark File" bmkp-toggle-saving-bookmark-file
    :help "Toggle the value of option `bookmark-save-flag'"))
(define-key bmkp-bmenu-menubar-menu [bmkp-switch-bookmark-file]
  '(menu-item "Switch to Bookmark File..." bmkp-switch-bookmark-file
    :help "Switch to a different bookmark file, *replacing* the current set of bookmarks"))
(define-key bmkp-bmenu-menubar-menu [bookmark-bmenu-load]
  '(menu-item "Add Bookmarks from File..." bookmark-bmenu-load
    :help "Load additional bookmarks from a bookmark file"))
(define-key bmkp-bmenu-menubar-menu [bmkp-empty-file]
  '(menu-item "New (Empty) Bookmark File..." bmkp-empty-file
    :help "Create a new, empty bookmark file, or empty an existing bookmark file"))
(define-key bmkp-bmenu-menubar-menu [bookmark-write]
  '(menu-item "Save As..." bookmark-write
    :help "Write the current set of bookmarks to a file whose name you enter"))
(define-key bmkp-bmenu-menubar-menu [bookmark-bmenu-save]
  '(menu-item "Save" bookmark-bmenu-save
    :help "Save the current set of bookmarks to the current bookmark file"))
(define-key bmkp-bmenu-menubar-menu [bmkp-bmenu-refresh-menu-list]
  '(menu-item "Refresh (Revert)" bmkp-bmenu-refresh-menu-list
    :help "Update the displayed bookmark list to reflect the currently defined bookmarks"))
(define-key bmkp-bmenu-menubar-menu [top-sep1] '("--"))

(define-key bmkp-bmenu-menubar-menu [bmkp-make-function-bookmark]
  '(menu-item "New Function Bookmark..." bmkp-make-function-bookmark
    :help "Create a bookmark that will invoke FUNCTION when \"jumped\" to"))
(define-key bmkp-bmenu-menubar-menu [bmkp-bmenu-make-sequence-from-marked]
  '(menu-item "New Sequence Bookmark from Marked..." bmkp-bmenu-make-sequence-from-marked
    :help "Create or update a sequence bookmark from the visible marked bookmarks"))
(define-key bmkp-bmenu-menubar-menu [bmkp-choose-navlist-from-bookmark-list]
  '(menu-item "Set Navlist from Bookmark-List Bookmark..." bmkp-choose-navlist-from-bookmark-list
    :help "Set the navigation list from a bookmark-list bookmark"))
(define-key bmkp-bmenu-menubar-menu [bmkp-choose-navlist-of-type]
  '(menu-item "Set Navlist to Bookmarks of Type..." bmkp-choose-navlist-of-type
    :help "Set the navigation list to the bookmarks of a certain type"))
(define-key bmkp-bmenu-menubar-menu [bmkp-list-defuns-in-commands-file]
  '(menu-item "List User-Defined Bookmark Commands" bmkp-list-defuns-in-commands-file
    :help "List the functions defined in `bmkp-bmenu-commands-file'"))

(defvar bmkp-bmenu-define-command-menu (make-sparse-keymap "Define Command")
    "`Define Command' submenu for menu-bar `Bookmark+' menu.")
(define-key bmkp-bmenu-menubar-menu [define-command]
  (cons "Define Command" bmkp-bmenu-define-command-menu))

(define-key bmkp-bmenu-define-command-menu [bmkp-bmenu-define-full-snapshot-command]
  '(menu-item "To Restore Full Bookmark-List State..." bmkp-bmenu-define-full-snapshot-command
    :help "Define a command to restore the current bookmark-list state"))
(define-key bmkp-bmenu-define-command-menu [bmkp-bmenu-define-command]
  '(menu-item "To Restore Current Order and Filter..." bmkp-bmenu-define-command
    :help "Define a command to use the current sort order, filter, and omit list"))
(define-key bmkp-bmenu-define-command-menu [bmkp-define-tags-sort-command]
  '(menu-item "To Sort by Specific Tags..." bmkp-define-tags-sort-command
    :help "Define a command to sort bookmarks in the bookmark list by certain tags"))
(define-key bmkp-bmenu-define-command-menu [bmkp-bmenu-define-jump-marked-command]
  '(menu-item "To Jump to a Bookmark Now Marked..." bmkp-bmenu-define-jump-marked-command
    :help "Define a command to jump to one of the bookmarks that is now marked"
    :enable bmkp-bmenu-marked-bookmarks))

;; `Jump To': Add jump menu also to the `Bookmark+' menu, and remove the two jump commands there.
(define-key bmkp-bmenu-menubar-menu [jump] (cons "Jump To" bmkp-jump-menu))

(when (featurep 'bookmark+-lit)
  (defvar bmkp-bmenu-highlight-menu (make-sparse-keymap "Highlight")
    "`Highlight' submenu for menu-bar `Bookmark+' menu.")
  (define-key bmkp-bmenu-menubar-menu [highlight] (cons "Highlight" bmkp-bmenu-highlight-menu))

  (define-key bmkp-bmenu-highlight-menu [bmkp-bmenu-show-only-lighted]
    '(menu-item "Show Only Highlighted" bmkp-bmenu-show-only-lighted
      :help "Display (only) highlighted bookmarks"))
  (define-key bmkp-bmenu-highlight-menu [bmkp-bmenu-set-lighting-for-marked]
    '(menu-item "Set Highlighting for Marked" bmkp-bmenu-set-lighting-for-marked
      :help "Set specific highlighting for the marked bookmarks"
      :enable bmkp-bmenu-marked-bookmarks))
  (define-key bmkp-bmenu-highlight-menu [bmkp-bmenu-unlight-marked]
    '(menu-item "Unhighlight Marked" bmkp-bmenu-unlight-marked
      :help "Unhighlight the marked bookmarks"
      :enable bmkp-bmenu-marked-bookmarks))
  (define-key bmkp-bmenu-highlight-menu [bmkp-bmenu-light-marked]
    '(menu-item "Highlight Marked" bmkp-bmenu-light-marked
      :help "Highlight the marked bookmarks"
      :enable bmkp-bmenu-marked-bookmarks)))

(defvar bmkp-bmenu-tags-menu (make-sparse-keymap "Tags")
    "`Tags' submenu for menu-bar `Bookmark+' menu.")
(define-key bmkp-bmenu-menubar-menu [tags] (cons "Tags" bmkp-bmenu-tags-menu))

(define-key bmkp-bmenu-tags-menu [bmkp-list-all-tags]
  '(menu-item "List All Tags" bmkp-list-all-tags :help "List all tags used for any bookmarks"))
(define-key bmkp-bmenu-tags-menu [bmkp-rename-tag]
  '(menu-item "Rename Tag..." bmkp-rename-tag
    :help "Rename a tag in all bookmarks, even those not showing"))
(define-key bmkp-bmenu-tags-menu [bmkp-remove-tags-from-all]
  '(menu-item "Remove Some Tags from All..." bmkp-remove-tags-from-all
    :help "Remove a set of tags from all bookmarks"))
(define-key bmkp-bmenu-tags-menu [bmkp-bmenu-remove-tags-from-marked]
  '(menu-item "Remove Some Tags from Marked..." bmkp-bmenu-remove-tags-from-marked
    :help "Remove a set of tags from each of the marked bookmarks"))
(define-key bmkp-bmenu-tags-menu [bmkp-bmenu-add-tags-to-marked]
  '(menu-item "Add Some Tags to Marked..." bmkp-bmenu-add-tags-to-marked
    :help "Add a set of tags to each of the marked bookmarks"))

(defvar bmkp-bmenu-sort-menu (make-sparse-keymap "Sort")
    "`Sort' submenu for menu-bar `Bookmark+' menu.")
(define-key bmkp-bmenu-menubar-menu [sort] (cons "Sort" bmkp-bmenu-sort-menu))

(define-key bmkp-bmenu-sort-menu [bmkp-bmenu-sort-by-w3m-url]
  '(menu-item "By URL (W3M)" bmkp-bmenu-sort-by-w3m-url
    :help "Sort W3M bookmarks alphabetically by their URL/filename"))
(define-key bmkp-bmenu-sort-menu [bmkp-bmenu-sort-by-Gnus-thread]
  '(menu-item "By Gnus Thread" bmkp-bmenu-sort-by-Gnus-thread
    :help "Sort Gnus bookmarks by group, then by article, then by message"))
(define-key bmkp-bmenu-sort-menu [bmkp-bmenu-sort-by-Info-location]
  '(menu-item "By Info Node" bmkp-bmenu-sort-by-Info-location
    :help "Sort Info bookmarks by file name, then node name, then position"))
(define-key bmkp-bmenu-sort-menu [bmkp-bmenu-sort-by-last-local-file-update]
  '(menu-item "By Last Local File Update" bmkp-bmenu-sort-by-last-local-file-update
    :help "Sort bookmarks by last local file update time"))
(define-key bmkp-bmenu-sort-menu [bmkp-bmenu-sort-by-last-buffer-or-file-access]
  '(menu-item "By Last Buffer/File Access" bmkp-bmenu-sort-by-last-buffer-or-file-access
    :help "Sort bookmarks by time of last buffer access or local-file access"))
(define-key bmkp-bmenu-sort-menu [bmkp-bmenu-sort-by-local-file-size]
  '(menu-item "By Local File Size" bmkp-bmenu-sort-by-local-file-size
    :help "Sort bookmarks by local file size"))
(define-key bmkp-bmenu-sort-menu [bmkp-bmenu-sort-by-local-file-type]
  '(menu-item "By Local File Type" bmkp-bmenu-sort-by-local-file-type
    :help "Sort bookmarks by local file type: file, symlink, directory"))
(define-key bmkp-bmenu-sort-menu [bmkp-bmenu-sort-by-bookmark-type]
  '(menu-item "By Type" bmkp-bmenu-sort-by-bookmark-type
    :help "Sort bookmarks by type: Info, Gnus, W3M, files, other"))
(define-key bmkp-bmenu-sort-menu [bmkp-bmenu-sort-by-file-name]
  '(menu-item "By File Name" bmkp-bmenu-sort-by-file-name :help "Sort bookmarks by file name"))
(define-key bmkp-bmenu-sort-menu [bmkp-bmenu-sort-by-bookmark-name]
  '(menu-item "By Bookmark Name" bmkp-bmenu-sort-by-bookmark-name
    :help "Sort bookmarks by bookmark name, respecting `case-fold-search'"))
(define-key bmkp-bmenu-sort-menu [bmkp-bmenu-sort-by-creation-time]
  '(menu-item "By Creation Time" bmkp-bmenu-sort-by-creation-time
    :help "Sort bookmarks by the time of their creation"))
(define-key bmkp-bmenu-sort-menu [bmkp-bmenu-sort-by-last-bookmark-access]
  '(menu-item "By Last Bookmark Access" bmkp-bmenu-sort-by-last-bookmark-access
    :help "Sort bookmarks by the time of their last visit as bookmarks"))
(define-key bmkp-bmenu-sort-menu [bmkp-bmenu-sort-by-bookmark-visit-frequency]
  '(menu-item "By Bookmark Use" bmkp-bmenu-sort-by-bookmark-visit-frequency
    :help "Sort bookmarks by the number of times they were visited as bookmarks"))
(define-key bmkp-bmenu-sort-menu [bmkp-bmenu-sort-marked-before-unmarked]
  '(menu-item "Marked Before Unmarked" bmkp-bmenu-sort-marked-before-unmarked
    :help "Sort bookmarks by putting marked before unmarked"))
(define-key bmkp-bmenu-sort-menu [bmkp-reverse-sort-order]
  '(menu-item "Reverse" bmkp-reverse-sort-order :help "Reverse the current bookmark sort order"))

(defvar bmkp-bmenu-show-menu (make-sparse-keymap "Show")
    "`Show' submenu for menu-bar `Bookmark+' menu.")
(define-key bmkp-bmenu-menubar-menu [show] (cons "Show" bmkp-bmenu-show-menu))

(define-key bmkp-bmenu-show-menu [bookmark-bmenu-show-all-annotations]
  '(menu-item "Show Annotations" bookmark-bmenu-show-all-annotations
    :help "Show the annotations for all bookmarks (in another window)"))
(define-key bmkp-bmenu-show-menu [bookmark-bmenu-toggle-filenames]
  '(menu-item "Show/Hide File Names" bookmark-bmenu-toggle-filenames
    :help "Toggle whether filenames are shown in the bookmark list"))
(define-key bmkp-bmenu-show-menu [show-sep1] '("--"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-show-all]
  '(menu-item "Show All" bmkp-bmenu-show-all
    :help "Show all bookmarks currently known to the bookmark list"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-filter-tags-incrementally]
  '(menu-item "Show Only Tag Matches..." bmkp-bmenu-filter-tags-incrementally
    :help "Incrementally filter bookmarks by tags using a regexp"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-filter-file-name-incrementally]
  '(menu-item "Show Only File Name Matches..." bmkp-bmenu-filter-file-name-incrementally
    :help "Incrementally filter bookmarks by file name using a regexp"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-filter-bookmark-name-incrementally]
  '(menu-item "Show Only Name Matches..." bmkp-bmenu-filter-bookmark-name-incrementally
    :help "Incrementally filter bookmarks by bookmark name using a regexp"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-show-only-specific-file]
  '(menu-item "Show Only for Specific File" bmkp-bmenu-show-only-specific-file
    :help "Display (only) the bookmarks for a specific file"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-show-only-specific-buffer]
  '(menu-item "Show Only for Specific Buffer" bmkp-bmenu-show-only-specific-buffer
    :help "Display (only) the bookmarks for a specific buffer"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-show-only-w3m-urls]
  '(menu-item "Show Only URLs (W3M)" bmkp-bmenu-show-only-w3m-urls
    :help "Display (only) the w3m bookmarks"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-show-only-gnus]
  '(menu-item "Show Only Gnus Messages" bmkp-bmenu-show-only-gnus
    :help "Display (only) the Gnus bookmarks"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-show-only-man-pages]
  '(menu-item "Show Only UNIX Manual Pages" bmkp-bmenu-show-only-man-pages
    :help "Display (only) the `man' page bookmarks"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-show-only-info-nodes]
  '(menu-item "Show Only Info Nodes" bmkp-bmenu-show-only-info-nodes
    :help "Display (only) the Info bookmarks"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-show-only-dired]
  '(menu-item "Show Only Dired Buffers" bmkp-bmenu-show-only-dired
    :help "Display (only) the Dired bookmarks"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-show-only-bookmark-files]
  '(menu-item "Show Only Bookmark Files" bmkp-bmenu-show-only-bookmark-files
    :help "Display (only) the bookmark-file bookmarks"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-show-only-desktops]
  '(menu-item "Show Only Desktops" bmkp-bmenu-show-only-desktops
    :help "Display (only) the desktop bookmarks"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-show-only-regions]
  '(menu-item "Show Only Regions" bmkp-bmenu-show-only-regions
    :help "Display (only) the bookmarks that record a region"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-show-only-non-files]
  '(menu-item "Show Only Non-Files (Buffers)" bmkp-bmenu-show-only-non-files
    :help "Display (only) the non-file bookmarks"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-show-only-files]
  '(menu-item "Show Only Files" bmkp-bmenu-show-only-files
    :help "Display (only) the file and directory bookmarks"))
(when (featurep 'bookmark+-lit)
  (define-key bmkp-bmenu-show-menu [bmkp-bmenu-show-only-lighted]
    '(menu-item "Show Only Highlighted" bmkp-bmenu-show-only-lighted
      :help "Display (only) highlighted bookmarks")))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-toggle-show-only-unmarked]
  '(menu-item "Show Only Unmarked" bmkp-bmenu-toggle-show-only-unmarked
    :help "Hide all marked bookmarks.  Repeat to toggle, showing all"))
(define-key bmkp-bmenu-show-menu [bmkp-bmenu-toggle-show-only-marked]
  '(menu-item "Show Only Marked" bmkp-bmenu-toggle-show-only-marked
    :help "Hide all unmarked bookmarks.  Repeat to toggle, showing all"))

(defvar bmkp-bmenu-omit-menu (make-sparse-keymap "Omit")
  "`Omit' submenu for menu-bar `Bookmark+' menu.")
(define-key bmkp-bmenu-menubar-menu [omitting] (cons "Omit" bmkp-bmenu-omit-menu))

(define-key bmkp-bmenu-omit-menu [bmkp-bmenu-show-all]
  '(menu-item "Show All" bmkp-bmenu-show-all
    :visible (eq bmkp-bmenu-filter-function 'bmkp-omitted-alist-only)
    :help "Show all bookmarks (except omitted)"))
(define-key bmkp-bmenu-omit-menu [bmkp-bmenu-show-only-omitted]
  '(menu-item "Show Only Omitted" bmkp-bmenu-show-only-omitted
    :visible (not (eq bmkp-bmenu-filter-function 'bmkp-omitted-alist-only))
    :enable bmkp-bmenu-omitted-list :help "Show only the omitted bookmarks"))
(define-key bmkp-bmenu-omit-menu [bmkp-unomit-all]
  '(menu-item "Un-Omit All" bmkp-unomit-all
    :visible bmkp-bmenu-omitted-list :help "Un-omit all omitted bookmarks"))
(define-key bmkp-bmenu-omit-menu [bmkp-bmenu-unomit-marked]
  '(menu-item "Un-Omit Marked" bmkp-bmenu-unomit-marked
    :visible (eq bmkp-bmenu-filter-function 'bmkp-omitted-alist-only)
    :enable (and bmkp-bmenu-omitted-list
             (save-excursion (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
                             (re-search-forward "^>" (point-max) t)))
    :help "Un-omit the marked bookmarks" :keys "\\[bmkp-bmenu-omit/unomit-marked]"))
(define-key bmkp-bmenu-omit-menu [bmkp-bmenu-omit-marked]
  '(menu-item "Omit Marked" bmkp-bmenu-omit-marked
    :visible (not (eq bmkp-bmenu-filter-function 'bmkp-omitted-alist-only))
    :enable (and (save-excursion (goto-char (point-min)) (forward-line bmkp-bmenu-header-lines)
                                 (re-search-forward "^>" (point-max) t)))
    :help "Omit the marked bookmarks" :keys "\\[bmkp-bmenu-omit/unomit-marked]"))

(defvar bmkp-bmenu-mark-menu (make-sparse-keymap "Mark")
    "`Mark' submenu for menu-bar `Bookmark+' menu.")
(define-key bmkp-bmenu-menubar-menu [marking] (cons "Mark" bmkp-bmenu-mark-menu))

(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-unmark-bookmarks-tagged-not-all]
  '(menu-item "Unmark If Not Tagged with All..." bmkp-bmenu-unmark-bookmarks-tagged-not-all
    :help "Unmark all visible bookmarks that are tagged with *some* tag in a set you specify"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-unmark-bookmarks-tagged-none]
  '(menu-item "Unmark If Tagged with None..." bmkp-bmenu-unmark-bookmarks-tagged-none
    :help "Unmark all visible bookmarks that are *not* tagged with *any* tag you specify"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-unmark-bookmarks-tagged-all]
  '(menu-item "Unmark If Tagged with All..." bmkp-bmenu-unmark-bookmarks-tagged-all
    :help "Unmark all visible bookmarks that are tagged with *each* tag you specify"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-unmark-bookmarks-tagged-some]
  '(menu-item "Unmark If Tagged with Some..." bmkp-bmenu-unmark-bookmarks-tagged-some
    :help "Unmark all visible bookmarks that are tagged with *some* tag in a set you specify"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-bookmarks-tagged-not-all]
  '(menu-item "Mark If Not Tagged with All..." bmkp-bmenu-mark-bookmarks-tagged-not-all
    :help "Mark all visible bookmarks that are *not* tagged with *all* tags you specify"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-bookmarks-tagged-none]
  '(menu-item "Mark If Tagged with None..." bmkp-bmenu-mark-bookmarks-tagged-none
    :help "Mark all visible bookmarks that are not tagged with *any* tag you specify"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-bookmarks-tagged-all]
  '(menu-item "Mark If Tagged with All..." bmkp-bmenu-mark-bookmarks-tagged-all
    :help "Mark all visible bookmarks that are tagged with *each* tag you specify"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-bookmarks-tagged-some]
  '(menu-item "Mark If Tagged with Some..." bmkp-bmenu-mark-bookmarks-tagged-some
    :help "Mark all visible bookmarks that are tagged with *some* tag in a set you specify"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-bookmarks-tagged-regexp]
  '(menu-item "Mark If Tagged Matching Regexp..." bmkp-bmenu-mark-bookmarks-tagged-regexp
    :help "Mark bookmarks any of whose tags match a regexp you enter"))
(define-key bmkp-bmenu-mark-menu [mark-sep1] '("--"))
(when (featurep 'bookmark+-lit)
  (define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-lighted-bookmarks]
    '(menu-item "Mark Highlighted" bmkp-bmenu-mark-lighted-bookmarks
      :help "Mark highlighted bookmarks")))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-specific-file-bookmarks]
  '(menu-item "Mark for Specific File" bmkp-bmenu-mark-specific-file-bookmarks
    :help "Mark bookmarks for a specific file"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-specific-buffer-bookmarks]
  '(menu-item "Mark for Specific Buffer" bmkp-bmenu-mark-specific-buffer-bookmarks
    :help "Mark bookmarks for a specific buffer"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-w3m-bookmarks]
  '(menu-item "Mark URLs (W3M)" bmkp-bmenu-mark-w3m-bookmarks :help "Mark W3M (URL) bookmarks"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-gnus-bookmarks]
  '(menu-item "Mark Gnus Messages" bmkp-bmenu-mark-gnus-bookmarks :help "Mark Gnus bookmarks"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-man-bookmarks]
  '(menu-item "Mark UNIX Manual Pages" bmkp-bmenu-mark-man-bookmarks
    :help "Mark `man' page bookmarks"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-info-bookmarks]
  '(menu-item "Mark Info Nodes" bmkp-bmenu-mark-info-bookmarks :help "Mark Info bookmarks"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-dired-bookmarks]
  '(menu-item "Mark Dired Buffers" bmkp-bmenu-mark-dired-bookmarks :help "Mark Dired bookmarks"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-bookmark-file-bookmarks]
  '(menu-item "Mark Bookmark Files" bmkp-bmenu-mark-bookmark-file-bookmarks
    :help "Mark the bookmark-file bookmarks"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-desktop-bookmarks]
  '(menu-item "Mark Desktops" bmkp-bmenu-mark-desktop-bookmarks
    :help "Mark desktop bookmarks"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-region-bookmarks]
  '(menu-item "Mark Regions" bmkp-bmenu-mark-region-bookmarks
    :help "Mark bookmarks that record a region"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-non-file-bookmarks]
  '(menu-item "Mark Non-Files (Buffers)" bmkp-bmenu-mark-non-file-bookmarks
    :help "Mark non-file bookmarks"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-file-bookmarks]
  '(menu-item "Mark Files" bmkp-bmenu-mark-file-bookmarks :help "Mark file bookmarks"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-unmark-all]
  '(menu-item "Unmark All" bmkp-bmenu-unmark-all
    :help "Remove a mark you specify (> or D) from each bookmark (RET to remove both kinds)"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-mark-all]
  '(menu-item "Mark All" bmkp-bmenu-mark-all :help "Mark all bookmarks, using mark `>'"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-toggle-marks]
  '(menu-item "Toggle Marked/Unmarked" bmkp-bmenu-toggle-marks
    :help "Unmark all marked bookmarks; mark all unmarked bookmarks"))
(define-key bmkp-bmenu-mark-menu [bmkp-bmenu-regexp-mark]
  '(menu-item "Mark Regexp Matches..." bmkp-bmenu-regexp-mark
    :help "Mark bookmarks that match a regexp that you enter"))

(define-key bmkp-bmenu-menubar-menu [bookmark-bmenu-execute-deletions]
  '(menu-item "Delete Flagged (D)" bookmark-bmenu-execute-deletions
    :help "Delete the (visible) bookmarks flagged `D'"))
(define-key bmkp-bmenu-menubar-menu [bmkp-bmenu-delete-marked]
  '(menu-item "Delete Marked (>)" bmkp-bmenu-delete-marked
    :help "Delete all (visible) bookmarks marked `>', after confirmation"))
(define-key bmkp-bmenu-menubar-menu [bmkp-bmenu-query-replace-marked-bookmarks-regexp]
  '(menu-item "Query-Replace Marked..." bmkp-bmenu-query-replace-marked-bookmarks-regexp
    :help "`query-replace-regexp' over all files whose bookmarks are marked"))
(when (fboundp 'bmkp-bmenu-isearch-marked-bookmarks)
  (define-key bmkp-bmenu-menubar-menu [bmkp-bmenu-isearch-marked-bookmarks-regexp]
    '(menu-item "Regexp-Isearch Marked..." bmkp-bmenu-isearch-marked-bookmarks-regexp
      :help "Regexp Isearch the marked bookmark locations, in their current order"))
  (define-key bmkp-bmenu-menubar-menu [bmkp-bmenu-isearch-marked-bookmarks]
    '(menu-item "Isearch Marked..." bmkp-bmenu-isearch-marked-bookmarks
      :help "Isearch the marked bookmark locations, in their current order")))
(define-key bmkp-bmenu-menubar-menu [bmkp-bmenu-search-marked-bookmarks-regexp]
  '(menu-item "Search Marked..." bmkp-bmenu-search-marked-bookmarks-regexp
    :help "Regexp-search the files whose bookmarks are marked, in their current order"))
(define-key bmkp-bmenu-menubar-menu [bookmark-bmenu-select]
  '(menu-item "Jump to Marked" bookmark-bmenu-select
    :help "Jump to this line's bookmark.  Also visit each bookmark marked with `>'"))


;;; Mouse-3 menu binding.

;;;###autoload
(defvar bmkp-bmenu-line-overlay nil
  "Overlay to highlight the current line for `bmkp-bmenu-mouse-3-menu'.")
(define-key bookmark-bmenu-mode-map [mouse-3] 'bmkp-bmenu-mouse-3-menu)

;;;###autoload
(defun bmkp-bmenu-mouse-3-menu (event)
  "Pop-up menu on `mouse-3' for a bookmark listed in `*Bookmark List*'."
  (interactive "e")
  (let* ((mouse-pos                  (event-start event))
         (inhibit-field-text-motion  t) ; Just in case.
         bol eol
         (bmk-name                   (save-excursion
                                       (with-current-buffer (window-buffer (posn-window mouse-pos))
                                         (save-excursion
                                           (goto-char (posn-point mouse-pos))
                                           (save-excursion
                                             (setq bol  (progn (beginning-of-line) (point))
                                                   eol  (progn (end-of-line) (point))))
                                           (if bmkp-bmenu-line-overlay ; Don't recreate.
                                               (move-overlay bmkp-bmenu-line-overlay bol eol
                                                             (current-buffer))
                                             (setq bmkp-bmenu-line-overlay  (make-overlay bol eol))
                                             (overlay-put bmkp-bmenu-line-overlay 'face 'region))
                                           (bookmark-bmenu-bookmark))))))
    (sit-for 0)
    (let ((menu-choice
           (x-popup-menu event
                         (list "This Bookmark"
                               (if bmk-name
                                   (list bmk-name
                                         (if (member bmk-name bmkp-bmenu-marked-bookmarks)
                                             '("Unmark" . bookmark-bmenu-unmark)
                                           '("Mark" . bookmark-bmenu-mark))
                                         (save-excursion
                                           (goto-char (posn-point mouse-pos))
                                           (beginning-of-line)
                                           (if (looking-at "^D")
                                               '("Unmark" . bookmark-bmenu-unmark)
                                             '("Flag for Deletion" . bookmark-bmenu-delete)))
                                         '("Omit" . bmkp-bmenu-omit)
                                         '("--") ; ----------------------------------------
                                         '("Jump To" . bookmark-bmenu-this-window)
                                         '("Jump To in Other Window" . bookmark-bmenu-other-window)
                                         '("--") ; ----------------------------------------
                                         '("Add Some Tags..." . bmkp-bmenu-add-tags)
                                         '("Remove Some Tags..." . bmkp-bmenu-remove-tags)
                                         '("Remove All Tags..." . bmkp-bmenu-remove-all-tags)
                                         '("Set Tag Value..." . bmkp-bmenu-set-tag-value)
                                         (and (featurep 'bookmark+-lit)
                                              '("--")) ; ----------------------------------------
                                         (and (featurep 'bookmark+-lit)
                                              '("Highlight"   . bmkp-bmenu-light))
                                         (and (featurep 'bookmark+-lit)
                                              '("Unhighlight" . bmkp-bmenu-unlight))
                                         (and (featurep 'bookmark+-lit)
                                              '("Set Lighting" . bmkp-bmenu-set-lighting))
                                         '("--") ; ----------------------------------------
                                         '("Show Annotation" . bookmark-bmenu-show-annotation)
                                         '("Edit Annotation..." . bookmark-bmenu-edit-annotation)
                                         '("Edit Name, File Name..." . bmkp-bmenu-edit-bookmark)
                                         '("Rename..." . bookmark-bmenu-rename)
                                         (and (fboundp 'bookmark-bmenu-relocate)
                                              '("Relocate..." . bookmark-bmenu-relocate))
                                         '("--") ; ----------------------------------------
                                         '("Describe" . bmkp-bmenu-describe-this-bookmark))
                                 '("" (""))))))) ; No menu: not on a bookmark line.
      (when bmkp-bmenu-line-overlay (delete-overlay bmkp-bmenu-line-overlay))
      (and menu-choice  (save-excursion (goto-char (posn-point mouse-pos))
                                        (call-interactively menu-choice))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bookmark+.el ends here

;;; bookmark+.el - Extensions to standard library `bookmark.el'.
;;
;; Filename: bookmark+.el
;; Description: Extensions to standard library `bookmark.el'.
;; Author: Drew Adams
;;         Thierry Volpiatto
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;;             Thierry Volpiatto
;; Copyright (C) 2000-2009, Drew Adams, all rights reserved.
;; Copyright (C) 2009, Thierry Volpiatto, all rights reserved.
;; Created: Fri Sep 15 07:58:41 2000
;; Last-Updated: Wed Oct 21 15:38:48 2009 (-0700)
;;           By: dradams
;;     Update #: 6709
;; URL: http://www.emacswiki.org/cgi-bin/wiki/bookmark+.el
;; Keywords: bookmarks, placeholders, annotations, search, info, w3m, gnus
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `bookmark', `ffap', `pp'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to standard library `bookmark.el'.
;;
;;    More description below.
 
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
;;  (@> "Documentation")
;;    (@> "Bookmark+ Features")
;;    (@> "Installing Bookmark+")
;;    (@> "Bookmarks Menu List")
;;      (@> "Marking and Unmarking Bookmarks")
;;      (@> "Filtering: Hiding and Showing Bookmarks")
;;      (@> "Sorting Bookmarks")
;;    (@> "Bookmark Compatibility with Vanilla Emacs (`bookmark.el')")
;;    (@> "New Bookmark Structure")
;;  (@> "Change log")
;;  (@> "Macros")
;;  (@> "Keymaps")
;;  (@> "Faces (Customizable)")
;;  (@> "User Options (Customizable)")
;;  (@> "Internal Variables")
;;  (@> "Compatibility Code for Older Emacs Versions")
;;  (@> "Core Replacements (`bookmark-*' except `bookmark-bmenu-*')")
;;  (@> "Menu List Replacements (`bookmark-bmenu-*')")
;;  (@> "Bookmark+ Functions (`bookmarkp-*')")
;;    (@> "Menu-List (`*-bmenu-*') Filter Commands")
;;    (@> "Menu-List (`*-bmenu-*') Commands Involving Marks")
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
;;    (@> "Other Bookmark+ Functions (`bookmarkp-*')")
 
;;(@* "Things Defined Here")
;;
;;  Things Defined Here
;;  -------------------
;;
;;  Commands defined here:
;;
;;    `bookmarkp-add-tags', `bookmarkp-bmenu-add-tags-to-marked',
;;    `bookmarkp-bmenu-change-sort-order',
;;    `bookmarkp-bmenu-change-sort-order-repeat',
;;    `bookmarkp-bmenu-delete-marked',
;;    `bookmarkp-bmenu-describe-this-bookmark',
;;    `bookmarkp-bmenu-edit-bookmark',
;;    `bookmarkp-bmenu-filter-bookmark-name-incrementally',
;;    `bookmarkp-bmenu-filter-file-name-incrementally',
;;    `bookmarkp-bmenu-filter-tags-incrementally',
;;    `bookmarkp-bmenu-isearch-marked-bookmarks' (Emacs 23+),
;;    `bookmarkp-bmenu-isearch-marked-bookmarks-regexp' (Emacs 23+),
;;    `bookmarkp-bmenu-mark-all',
;;    `bookmarkp-bmenu-mark-bookmarks-tagged-all',
;;    `bookmarkp-bmenu-mark-bookmarks-tagged-none',
;;    `bookmarkp-bmenu-mark-bookmarks-tagged-not-all',
;;    `bookmarkp-bmenu-mark-bookmarks-tagged-regexp',
;;    `bookmarkp-bmenu-mark-bookmarks-tagged-some',
;;    `bookmarkp-bmenu-query-replace-marked-bookmarks-regexp',
;;    `bookmarkp-bmenu-quit', `bookmarkp-bmenu-refresh-menu-list',
;;    `bookmarkp-bmenu-regexp-mark',
;;    `bookmarkp-bmenu-remove-tags-from-marked',
;;    `bookmarkp-bmenu-show-all', `bookmarkp-bmenu-show-only-dired',
;;    `bookmarkp-bmenu-show-only-files',
;;    `bookmarkp-bmenu-show-only-gnus',
;;    `bookmarkp-bmenu-show-only-info-nodes',
;;    `bookmarkp-bmenu-show-only-non-files',
;;    `bookmarkp-bmenu-show-only-regions',
;;    `bookmarkp-bmenu-show-only-w3m-urls',
;;    `bookmarkp-bmenu-sort-by-bookmark-name',
;;    `bookmarkp-bmenu-sort-by-bookmark-visit-frequency',
;;    `bookmarkp-bmenu-sort-by-file-name',
;;    `bookmarkp-bmenu-sort-by-gnus-thread',
;;    `bookmarkp-bmenu-sort-by-Info-location',
;;    `bookmarkp-bmenu-sort-by-last-bookmark-access',
;;    `bookmarkp-bmenu-sort-by-last-buffer-or-file-access',
;;    `bookmarkp-bmenu-sort-by-last-local-file-access',
;;    `bookmarkp-bmenu-sort-by-last-local-file-update',
;;    `bookmarkp-bmenu-sort-by-local-file-size',
;;    `bookmarkp-bmenu-sort-by-local-file-type',
;;    `bookmarkp-bmenu-sort-by-w3m-url',
;;    `bookmarkp-bmenu-sort-marked-before-unmarked',
;;    `bookmarkp-bmenu-toggle-marks',
;;    `bookmarkp-bmenu-toggle-show-only-marked',
;;    `bookmarkp-bmenu-toggle-show-only-unmarked',
;;    `bookmarkp-bmenu-unmark-all',
;;    `bookmarkp-bmenu-unmark-bookmarks-tagged-all',
;;    `bookmarkp-bmenu-unmark-bookmarks-tagged-none',
;;    `bookmarkp-bmenu-unmark-bookmarks-tagged-not-all',
;;    `bookmarkp-bmenu-unmark-bookmarks-tagged-some',
;;    `bookmarkp-describe-bookmark',
;;    `bookmarkp-menu-jump-other-window' (Emacs 20,21),
;;    `bookmarkp-remove-all-tags', `bookmarkp-remove-tags',
;;    `bookmarkp-remove-tags-from-all', `bookmarkp-rename-tag',
;;    `bookmarkp-reverse-multi-sort-order',
;;    `bookmarkp-reverse-sort-order', `bookmarkp-version',
;;    `old-bookmark-insert', `old-bookmark-relocate'.
;;
;;  User options defined here:
;;
;;    `bookmarkp-bookmark-name-length-max',
;;    `bookmarkp-handle-region-function',
;;    `bookmarkp-incremental-filter-delay',
;;    `bookmarkp-region-search-size',
;;    `bookmarkp-save-new-location-flag',
;;    `bookmarkp-show-end-of-region', `bookmarkp-sort-comparer',
;;    `bookmarkp-sort-orders-alist',
;;    `bookmarkp-sort-orders-for-cycling-alist',
;;    `bookmarkp-su-or-sudo-regexp', `bookmarkp-use-region-flag',
;;    `bookmarkp-w3m-allow-multi-tabs'.
;;
;;  Faces defined here:
;;
;;    `bookmarkp-bad-bookmark', `bookmarkp-buffer', `bookmarkp-gnus',
;;    `bookmarkp-info', `bookmarkp-local-directory',
;;    `bookmarkp-local-file-with-region',
;;    `bookmarkp-local-file-without-region', `bookmarkp-non-file',
;;    `bookmarkp-remote-file', `bookmarkp-su-or-sudo',
;;    `bookmarkp-w3m'.
;;
;;  Macros defined here:
;;
;;    `bookmarkp-define-file-sort-predicate',
;;    `bookmarkp-define-sort-command'.
;;
;;  Non-interactive functions defined here:
;;
;;    `bookmarkp-alpha-cp', `bookmarkp-alpha-p',
;;    `bookmarkp-assoc-delete-all',
;;    `bookmarkp-barf-if-not-in-menu-list',
;;    `bookmarkp-bmenu-cancel-incremental-filtering',
;;    `bookmarkp-bmenu-filter-alist-by-bookmark-name-regexp',
;;    `bookmarkp-bmenu-filter-alist-by-file-name-regexp',
;;    `bookmarkp-bmenu-filter-alist-by-tags-regexp',
;;    `bookmarkp-bmenu-goto-bookmark-named',
;;    `bookmarkp-bmenu-mark/unmark-bookmarks-tagged-all/none',
;;    `bookmarkp-bmenu-mark/unmark-bookmarks-tagged-some/not-all',
;;    `bookmarkp-bmenu-propertize-item',
;;    `bookmarkp-bmenu-read-filter-input',
;;    `bookmarkp-bookmark-last-access-cp',
;;    `bookmarkp-bookmark-marked-p',
;;    `bookmarkp-buffer-last-access-cp', `bookmarkp-cp-not',
;;    `bookmarkp-current-sort-order', `bookmarkp-dired-alist-only',
;;    `bookmarkp-dired-bookmark-p', `bookmarkp-edit-bookmark',
;;    `bookmarkp-end-position-post-context',
;;    `bookmarkp-end-position-pre-context', `bookmarkp-face-prop',
;;    `bookmarkp-file-alist-only', `bookmarkp-file-alpha-cp',
;;    `bookmarkp-file-attribute-0-cp',
;;    `bookmarkp-file-attribute-1-cp',
;;    `bookmarkp-file-attribute-2-cp',
;;    `bookmarkp-file-attribute-3-cp',
;;    `bookmarkp-file-attribute-4-cp',
;;    `bookmarkp-file-attribute-5-cp',
;;    `bookmarkp-file-attribute-6-cp',
;;    `bookmarkp-file-attribute-7-cp',
;;    `bookmarkp-file-attribute-8-cp',
;;    `bookmarkp-file-attribute-9-cp',
;;    `bookmarkp-file-attribute-10-cp',
;;    `bookmarkp-file-attribute-11-cp', `bookmarkp-file-bookmark-p',
;;    `bookmarkp-file-remote-p', `bookmarkp-float-time',
;;    `bookmarkp-get-buffer-name', `bookmarkp-get-end-position',
;;    `bookmarkp-get-tags', `bookmarkp-get-visit-time',
;;    `bookmarkp-get-visits-count', `bookmarkp-gnus-alist-only',
;;    `bookmarkp-gnus-bookmark-p', `bookmarkp-gnus-cp',
;;    `bookmarkp-goto-position', `bookmarkp-handle-region-default',
;;    `bookmarkp-has-tag-p', `bookmarkp-info-alist-only',
;;    `bookmarkp-info-bookmark-p', `bookmarkp-info-cp',
;;    `bookmarkp-isearch-bookmarks' (Emacs 23+),
;;    `bookmarkp-isearch-bookmarks-regexp' (Emacs 23+),
;;    `bookmarkp-isearch-next-bookmark-buffer' (Emacs 23+),
;;    `bookmarkp-jump-dired', `bookmarkp-jump-gnus',
;;    `bookmarkp-jump-w3m', `bookmarkp-jump-w3m-new-session',
;;    `bookmarkp-jump-w3m-only-one-tab',
;;    `bookmarkp-line-number-at-pos',
;;    `bookmarkp-local-directory-bookmark-p',
;;    `bookmarkp-local-file-accessed-more-recently-cp',
;;    `bookmarkp-local-file-alist-only',
;;    `bookmarkp-local-file-bookmark-p',
;;    `bookmarkp-local-file-size-cp', `bookmarkp-local-file-type-cp',
;;    `bookmarkp-local-file-updated-more-recently-cp',
;;    `bookmarkp-make-dired-record', `bookmarkp-make-gnus-record',
;;    `bookmarkp-make-plain-predicate', `bookmarkp-make-w3m-record',
;;    `bookmarkp-marked-bookmarks-only', `bookmarkp-marked-cp',
;;    `bookmarkp-maybe-save-bookmarks',
;;    `bookmarkp-msg-about-sort-order', `bookmarkp-multi-sort',
;;    `bookmarkp-non-file-alist-only',
;;    `bookmarkp-non-file-bookmark-p',
;;    `bookmarkp-position-after-whitespace',
;;    `bookmarkp-position-before-whitespace',
;;    `bookmarkp-position-post-context',
;;    `bookmarkp-position-post-context-region',
;;    `bookmarkp-position-pre-context',
;;    `bookmarkp-position-pre-context-region', `bookmarkp-read-tags',
;;    `bookmarkp-read-tags-completing', `bookmarkp-record-visit',
;;    `bookmarkp-regexp-filtered-bookmark-name-alist-only',
;;    `bookmarkp-regexp-filtered-file-name-alist-only',
;;    `bookmarkp-regexp-filtered-tags-alist-only',
;;    `bookmarkp-region-alist-only', `bookmarkp-region-bookmark-p',
;;    `bookmarkp-remote-file-alist-only',
;;    `bookmarkp-remote-file-bookmark-p',
;;    `bookmarkp-remove-assoc-dups', `bookmarkp-remove-if',
;;    `bookmarkp-remove-if-not', `bookmarkp-repeat-command',
;;    `bookmarkp-replace-regexp-in-string',
;;    `bookmarkp-root-or-sudo-logged-p',
;;    `bookmarkp-save-new-region-location', `bookmarkp-set-union',
;;    `bookmarkp-some', `bookmarkp-some-marked-p',
;;    `bookmarkp-some-unmarked-p' `bookmarkp-sort-and-remove-dups',
;;    `bookmarkp-unmarked-bookmarks-only', `bookmarkp-upcase',
;;    `bookmarkp-visited-more-cp', `bookmarkp-w3m-alist-only',
;;    `bookmarkp-w3m-bookmark-p', `bookmarkp-w3m-cp',
;;    `bookmarkp-w3m-set-new-buffer-name'.
;;
;;  Internal variables defined here:
;;
;;    `bookmarkp-bmenu-before-hide-marked-alist',
;;    `bookmarkp-bmenu-before-hide-unmarked-alist',
;;    `bookmarkp-bmenu-filter-function',
;;    `bookmarkp-bmenu-filter-pattern',
;;    `bookmarkp-bmenu-filter-prompt', `bookmarkp-bmenu-filter-timer',
;;    `bookmarkp-bmenu-marked-bookmarks', `bookmarkp-bmenu-title',
;;    `bookmarkp-isearch-bookmarks' (Emacs 23+),
;;    `bookmarkp-jump-display-function',
;;    `bookmarkp-latest-bookmark-alist',
;;    `bookmarkp-latest-sorted-alist', `bookmarkp-non-file-filename',
;;    `bookmarkp-reverse-multi-sort-p', `bookmarkp-reverse-sort-p',
;;    `bookmarkp-tag-history', `bookmarkp-version-number'.
;;
;;
;;  ***** NOTE: The following commands defined in `bookmark.el'
;;              have been REDEFINED HERE:
;;
;;    `bookmark-bmenu-execute-deletions', `bookmark-bmenu-list',
;;    `bookmark-bmenu-mark', `bookmark-bmenu-other-window',
;;    `bookmark-bmenu-rename', `bookmark-bmenu-unmark',
;;    `bookmark-delete', `bookmark-insert',
;;    `bookmark-insert-location', `bookmark-jump',
;;    `bookmark-jump-other-window' (Emacs 20-21), `bookmark-relocate',
;;    `bookmark-rename', `bookmark-set', `bookmark-yank-word'.
;;
;;
;;  ***** NOTE: The following non-interactive functions defined in
;;              `bookmark.el' have been REDEFINED HERE:
;;
;;    `bookmark--jump-via', `bookmark-bmenu-bookmark',
;;    `bookmark-bmenu-delete', `bookmark-bmenu-hide-filenames',
;;    `bookmark-bmenu-mode' (advised, for doc string),
;;    `bookmark-bmenu-show-filenames',
;;    `bookmark-bmenu-surreptitiously-rebuild-list',
;;    `bookmark-completing-read', `bookmark-default-handler',
;;    `bookmark-get-bookmark' (Emacs 20-22),
;;    `bookmark-get-bookmark-record' (Emacs 20-22),
;;    `bookmark-get-handler' (Emacs 20-22),
;;    `bookmark-handle-bookmark', `bookmark-jump-noselect' (Emacs
;;    20-22), `bookmark-location', `bookmark-make-record' (Emacs
;;    20-22), `bookmark-make-record-default', `bookmark-maybe-message'
;;    (Emacs 20-21), `bookmark-prop-get' (Emacs 20-22),
;;    `bookmark-prop-set', `bookmark-store' (Emacs 20-22),
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
 
;;(@* "Documentation")
;;
;;  Documentation
;;  -------------
;;
;;(@* "Bookmark+ Features")
;;  ** Bookmark+ Features **
;;
;;  In addition to the kinds of bookmarks provided by vanilla Emacs:
;;
;;    - You can bookmark a buffer that is not associated with a file.
;;
;;    - You can bookmark a Gnus article.
;;
;;    - You can bookmark a Dired buffer, recording and restoring its
;;      switches and marked files.
;;
;;    - For any bookmark (except Gnus), you can bookmark a region of
;;      text, not just a position.  By default, when you jump to a
;;      bookmark that records a region, the region is activated.  See
;;      option `bookmarkp-use-region-flag'.  `C-u' reverses the
;;      behavior specified by the value of the option.
;;
;;    - Bookmarks can record the number of visits and the time of last
;;      visit.
;;
;;    - You can tag bookmarks.  A bookmark can have any number of
;;      tags, and multiple bookmarks can have the same tag.
;;
;;  Bookmark relocation when the contextual text changes is better
;;  than for vanilla Emacs.
;;
;;  Improvements for the menu list (buffer `*Bookmark List*'):
;;
;;    - You can show only bookmarks of a given type (e.g. file, Info).
;;    - You can sort bookmarks in multiple ways.
;;    - Enhanced marking/unmarking, similar to that in Dired.
;;    - Faces to distinguish bookmarks of different types.
;;    - You can edit a bookmark.
;;    - You can search and query-replace marked bookmarks, in the
;;      current bookmark sort order.  For Emacs 23 and later, this
;;      includes incremental multi-search.
;;
;;  If you also use Icicles, then you can use `S-delete' during
;;  completion for a bookmark name, to delete the current bookmark
;;  candidate.  See http://www.emacswiki.org/cgi-bin/wiki/Icicles.
;;
;;(@* "Installing Bookmark+")
;;  ** Installing Bookmark+ **
;;
;;  Put this library in your `load-path'.
;;  Add this to your init file (~/.emacs) : (require 'bookmark+)
;;
;;(@* "Bookmarks Menu List")
;;  ** Bookmarks Menu List **
;;
;;  Bookmark+ enhances the bookmarks list (the "menu list") that is
;;  displayed in buffer `*Bookmark List*' when you use `C-x r l'
;;  (`bookmark-bmenu-list').  Bookmarks are highlighted to indicate
;;  their type. You can show or hide bookmarks of particular types,
;;  mark and unmark bookmarks, and much more.  Use `C-h m' in buffer
;;  `*Bookmark List*' for more information.
;;
;;  (Note: Bookmark+ uses option `bookmarkp-sort-comparer'; it ignores
;;  vanilla Emacs option `bookmark-sort-flag'.)
;;
;;(@* "Marking and Unmarking Bookmarks")
;;  *** Marking and Unmarking Bookmarks ***
;;
;;  Bookmark+ enhances marking and unmarking of bookmarks in several
;;  ways.  In general, these enhancements are similar to features
;;  offered by Dired and Dired-X.
;;
;;  * You can use `% m' to mark bookmarks that match a regexp.  The
;;    entire bookmark line in the menu list is checked for a match.
;;
;;  * You can use `M-DEL' (or `U') to unmark all bookmarks or all that
;;    are marked `>' or all that are flagged `D'.
;;
;;  * You can use `t' to toggle (swap) marked and unmarked bookmarks:
;;    the marked become unmarked, and vice versa.
;;
;;  * You can use `>' to show only the marked bookmarks or `<' to show
;;    only the unmarked bookmarks.  Repeat to show all again.
;;
;;(@* "Filtering: Hiding and Showing Bookmarks")
;;  *** Filtering: Hiding and Showing Bookmarks ***
;;
;;  You can hide and show different sets of bookmarks in the menu
;;  list.  There are commands to show only bookmarks of a particular
;;  type (e.g. `I' to show only Info bookmarks).  And, again, you can
;;  show only the marked or only the unmarked bookmarks (using `>' and
;;  `<').
;;
;;  You can also filter to show only the bookmarks that match a
;;  regexp.  There are two ways to do this:
;;
;;  * Use `P B' (for "pattern", "bookmark") and type a regexp.  The
;;    bookmarks are filtered incrementally, as you type.  Only the
;;    bookmark name is matched.  Hit any non-inserting key, such as
;;    `RET', to stop defining the pattern.
;;
;;    Similarly, hit `P F' for bookmarks whose file names match a
;;    regexp, and `P T' for bookmarks with one or more tags that match
;;    a regexp.  See (@> Bookmark Tags), below, for information about
;;    tags.
;;
;;  * Just as in Dired, use `% m' to mark the bookmarks that match a
;;    regexp.  The entire bookmark line in the menu list (bookmark
;;    name and perhaps file name) is searched for a match.
;;
;;    Then use `>' to show only the marked bookmarks.  This method has
;;    the advantage that you can show the complement: the bookmarks
;;    that do *not* match the regexp, by using `<' instead of `>'.  It
;;    also has the advantage/disadvantage that matching checks the
;;    combination of bookmark name and file name.
;;
;;  Commands that operate on the current bookmark or on the marked or
;;  the unmarked bookmarks act only on bookmarks that are displayed
;;  (not hidden).  This includes the commands that mark or unmark
;;  bookmarks.  This means that you can easily define any given set of
;;  bookmarks.
;;
;;  For example:
;;
;;    Use `F' to show only bookmarks associated with files.
;;    Use `% m' to mark those that match a particular regexp.
;;    Use `R' to show only bookmarks that have regions.
;;    Use `m' to mark some of those region bookmarks.
;;    Use `.' to show all bookmarks.
;;    Use `t' to swap marked and unmarked (so unmarked are now marked)
;;    Use `D' to delete all of the marked bookmarks (after confirming)
;;
;;  That deletes all file bookmarks that match the regexp and all
;;  region bookmarks that you selectively marked.
;;
;;(@* "Sorting Bookmarks")
;;  *** Sorting Bookmarks ***
;;
;;  Filtering hides certain kinds of bookmarks.  Sometimes, you want
;;  to see bookmarks of various kinds, but you want them to be grouped
;;  or sorted in different ways, for easy recognition, comparison, and
;;  access.
;;
;;  Bookmarks shown in the menu list are sorted using the current
;;  value of option `bookmark-sort-function'.  (If that is nil, they
;;  are unsorted, which means they appear in reverse chronological
;;  order of their creation.)
;;
;;  You can use `s s'... (repeat hitting the `s' key) to cycle among
;;  the various sort orders possible.  By default, all available sort
;;  orders are cycled, but you can shorten the cycling list by
;;  customizing option `bookmarkp-sort-orders-for-cycling-alist'.
;;
;;  You can also change directly to one of the main sort orders
;;  (without cycling) using `s n', `s f n', etc. - use `C-h m' for
;;  more info.
;;
;;  You can reverse the current sort direction (ascending/descending)
;;  using `s r'.
;;
;;  For a complex sort, which involves composing several sorting
;;  conditions, you can also use `s C-r' to reverse the order of
;;  bookmark sorting groups or the order within each group (depending
;;  on whether `s r' is also used).  Be aware that this can be a bit
;;  unintuitive.  If it does not do what you expect or want, or if it
;;  confuses you, then don't use it. ;-) (`s C-r' has no noticeable
;;  effect on simple sorting.)
;;
;;  Remember that you can combine sorting with hiding/showing
;;  different sets of bookmarks - bookmarks of different kinds
;;  (e.g. Info) or bookmarks that are marked or unmarked.
;;
;;  Finally, you can easily define your own sorting commands and sort
;;  orders.  See macro `bookmarkp-define-sort-command' and the
;;  documentation for option `bookmarkp-sort-comparer'.
;;
;;(@* Bookmark Tags)
;; ** Bookmark Tags **
;;
;;  With Bookmark+ you can bookmark several kinds of Emacs object.
;;  Bookmarks record locations - that is their primary purpose.  They
;;  can also record annotations: general free-text descriptions of
;;  your choosing.
;;
;;  Bookmark+ bookmarks can also be tagged, in del.icio.us style, as a
;;  way to organize them, which also means as a way to organize the
;;  objects bookmarked.  A bookmark tag is a string that contains no
;;  newline characters.
;;
;;  You can add as many tags as you like to any bookmark, and multiple
;;  bookmarks can have the same tag(s).  In fact, that's the whole
;;  idea behind using them for organizing.
;;
;;  This feature is unrelated to the fact that bookmarks record
;;  locations and are useful for navigating.  You can, if you want,
;;  use bookmarks to tag files in various ways purely for purposes of
;;  organizing them (e.g. into projects), whether or not you ever use
;;  the bookmarks as a way to visit them.
;;
;;  For example, if you also use library `dired+.el', then you can use
;;  `M-b' (`diredp-do-bookmark') to create a bookmark for each of the
;;  marked files in a Dired buffer.  Even if you never use those
;;  bookmarks for navigating to the files, you can use them with tags
;;  to organize the files.
;;
;;  To make tags more useful, Bookmark+ provides lots of commands: for
;;  adding or removing tags, and for marking or unmarking bookmarks
;;  that are tagged in various ways.  When combined with other
;;  Bookmark+ commands (e.g. search, query-replace) that apply to the
;;  marked bookmarks in the `*Bookmarks List*' window, you can really
;;  do quite a lot using bookmark tags.  Use your imagination!
;;
;;  There are lots of tags-related bookmark commands, and they are all
;;  bound to keys in buffer `*Bookmarks List*'.  How to keep them
;;  straight or remember the keys?  `C-h m' is your friend, of course.
;;  And the tag-related keys are organized as follows:
;;
;;    They all have the prefix key `T'.
;;
;;    `m' means mark
;;    `u' means unmark
;;    `>' stands for the marked bookmarks
;;    `*' means AND (set intersection; all)
;;    `+' means OR  (set union; some/any)
;;    `~' means NOT (set complement)
;;
;;  The key `T m *', for instance, marks the bookmarks that are tagged
;;  with all of a given set of tags.  It prompts you for one or more
;;  tags that the bookmarks must have, and it marks all bookmarks that
;;  have all of the tags you enter.
;;
;;  The key `T u ~ +' unmarks the bookmarks that do not have any of
;;  the tags you specify.  And so on.  Marking and unmarking commands
;;  compare the tags a bookmark has with tags you enter.  Any
;;  bookmarks that have no tags are ignored - they are neither marked
;;  nor unmarked by these commands.
;;
;;  `+' and `-' can also mean add and remove tags, respectively, and
;;  `>' stands for the marked bookmarks.  So `T > +' adds one or more
;;  tags to all of the marked bookmarks.
;;
;;  In general, the tag-related commands let you enter a set of tags,
;;  one at a time.  Thus, instead of having a command to add a single
;;  tag to the current bookmark, you have a command to add any number
;;  of tags to it.  To add just a single tag, you hit RET twice: once
;;  to enter the tag, and once again to indicate that it's the last
;;  one.
;;
;;  If you just hit RET immediately, specifying an empty set of tags,
;;  then each of the commands does something reasonable.  For `T m *',
;;  for instance, an empty list of tags means to mark the bookmarks
;;  that have any tags at all.
;;
;;  Finally, for the marking/unmarking commands, a prefix argument
;;  flips the sense of the command:
;;
;;    "some" -> "some are not", i.e., "not all are" (and vice versa)
;;    "all"  -> "all are not", i.e., "none are" (and vice versa)
;;
;;  IOW:
;;
;;    C-u T m *    =  T m ~ +  (all are not  = not some are)
;;    C-u T m ~ +  =  T m *    (not some are = all are not)
;;    C-u T m +    =  T m ~ *  (some are not = not all are)
;;    C-u T m ~ *  =  T m +    (not all are  = some are not)
;;
;;  You'll figure it out.
;;
;;  The best way to think about tags is as names of sets.  All
;;  bookmarks tagged `blue' constitute the bookmark set named `blue'.
;;
;;  The marking commands make it easy to combine sets as unions or
;;  intersections.  And you can give the result a name for quick
;;  access later, just by adding a new tag.  IOW, do the
;;  set-definition work only once, and name the result.  For example,
;;  to tag as `Java IDE Projects' the bookmarks that are already
;;  tagged both `Java' and `ide':
;;
;;    1. Mark those bookmarks, using `T m * Java RET ide RET RET'.
;;    2. Tag them, using `T + Java IDE Projects RET RET.
;;
;;  How would you query-replace the set of files that are tagged with
;;  any of the tags `alpha', `beta', and `gamma', but are not tagged
;;  `blue' or `moon'?
;;
;;    1. `F', to show only the file bookmarks.
;;    2. `T m + alpha RET beta RET gamma RET RET', to mark the
;;       bookmarks that have at least one of those tags.
;;    3. `T u + blue RET red', to unmark those that are blue or red.
;;    4. `M-q' to query-replace the marked files.
;;
;;  If that's a set of files that you use often, then name the set by
;;  giving the files a new tag.
;;
;;(@* "Bookmark Compatibility with Vanilla Emacs (`bookmark.el')")
;;  ** Bookmark Compatibility with Vanilla Emacs (`bookmark.el') **
;;
;;  Library `bookmark+.el' is generally compatible with GNU Emacs
;;  versions 20 through 23.
;;
;;  1. All bookmarks created using any version of vanilla Emacs
;;     (library `bookmark.el') continue to work with `bookmark+.el'.
;;
;;  2. All bookmarks created using library `bookmark+.el' will work
;;     with all Emacs versions (20-23), provided you use library
;;     `bookmark+.el' to access them.
;;
;;  3. Most bookmarks created using `bookmark+.el' will not interfere
;;     with the behavior of vanilla Emacs, versions 21-23.  The new
;;     bookmark types are simply ignored by vanilla Emacs.  For
;;     example:
;;
;;     - A bookmark with a region is treated like a simple position
;;       bookmark: the destination is the region start position.
;;
;;     - A Gnus bookmark does not work; it is simply ignored.
;;
;;     However, there are two cases in which `bookmark+.el' bookmarks
;;     will raise an error in vanilla Emacs:
;;
;;     * You cannot use non-file (e.g. buffer-only) bookmarks with any
;;       version of vanilla Emacs.
;;
;;     * You cannot use any bookmarks created using `bookmark+.el'
;;       with vanilla Emacs 20.
;;
;;     The Emacs bookmark data structure has changed from version to
;;     version.  Library `bookmark+.el' always creates bookmarks that
;;     have the most recent structure (Emacs 23).  As is the case for
;;     any bookmarks that have the Emacs 23 structure, these bookmarks
;;     will not work in vanilla Emacs 20 (that is, without
;;     `bookmark+.el').
;;
;;  Bottom line: Use `bookmark+.el' to access bookmarks created using
;;  `bookmark+.el'.
;;
;;(@* "New Bookmark Structure")
;;  ** New Bookmark Structure **
;;
;;  The bookmark data structure, variable `bookmark-alist', has been
;;  enhanced to support new bookmark types.  For a description of this
;;  enhanced structure, use `C-h v bookmark-alist'.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;(@* "Change log")
;;
;; 2009/10/21 dadams
;;     Added: bookmarkp-barf-if-not-in-menu-list.  Use in place of its body.
;;     Added: bookmarkp-bmenu-mark-bookmarks-tagged-regexp.  Bound to T m %.
;;     Added: bookmarkp-record-visit.  Use in *--jump-via.  Replaces next two removals.
;;     Removed: bookmarkp-add-or-update-time, bookmarkp-increment-visits.
;;     Renamed: *-record-(end|front|rear)-context(-region)-string'.
;;              New names: bookmarkp-(end-)position-(pre|post)-context(-region).
;;     *-bmenu-describe-this-bookmark: Added *-barf-if-not-in-menu-list.
;;     *-bmenu-(un)mark-all, *-bmenu-regexp-mark, *-bmenu-toggle-marks:
;;       Removed with-current-buffer.
;; 2009/10/20 dadams
;;     Added: bookmarkp-bmenu-filter-function, bookmarkp-bmenu-title.
;;     Removed: bookmarkp-bmenu-called-from-inside-p.
;;     *-bmenu-list:
;;       Removed TITLE arg.  Get title from bookmarkp-bmenu-title or default.
;;       Use interactive-p and absence of menu list, not *-bmenu-called-from-inside-p, as the
;;         criterion for removing marks.  Fixes bugs such as bookmark creation removing marks.
;;     *-define-sort-command, *-bmenu-execute-deletions, *-increment-visits,
;;       *-add-or-update-time, *-bmenu-show-only-*, *-bmenu-show-all,
;;       *-bmenu-refresh-menu-list, *-bmenu-toggle-show-only-(un)marked,
;;       *-bmenu-filter-alist-by-regexp, *-bmenu-reverse(-multi-sort)-order,
;;       *-bmenu-change-sort-order:
;;         Do not bind or set *-called-from-inside-p.
;;     *-bmenu-show-only-*, *-bmenu-show-all, *-bmenu-filter-alist-by-regexp:
;;       Set *-bmenu-filter-function, *-bmenu-title.
;;     *-bmenu-show-all:
;;       Set *-latest-bookmark-alist to bookmark-alist.
;;     *-bmenu-refresh-menu-list: Fix so that it in fact refreshes.
;;       Do not use *-bmenu-surreptitiously-rebuild-list and *-bmenu-check-position.
;;       Bind bookmark-alist to last alist (filtered or not), and call *-bmenu-list.
;;     *-bmenu-surreptitiously-rebuild-list:
;;       Do not use save*-excursion.  Do not get current title and pass it to *-bmenu-list.
;;     *-file-alist-only:
;;       Removed optional arg.  We have *-local-file-alist-only for that.
;;     *-regexp-filtered-alist-only, *-bmenu-filter-alist-by-regexp:
;;       Remove REGEXP arg - use bookmarkp-bmenu-filter-pattern.
;;     *-bmenu-filter-incrementally:
;;       Raise error if not in *Bookmark List*.
;;       Use just bookmarkp-bmenu-filter-alist-by-regexp in timer - pass no regexp arg.
;;     Added: bookmarkp-some, *-bmenu-filter-(file-name|tags)-incrementally,
;;            *-bmenu-filter-alist-by-(file-name|tags)-regexp,
;;            *-regexp-filtered-(file-name|tags)-alist-only.
;;     Renamed: *-bmenu-filter-incrementally to *-bmenu-filter-bookmark-name-incrementally,
;;              *-bmenu-filter-alist-by-regexp to *-bmenu-filter-alist-by-bookmark-name-regexp,
;;              *-regexp-filtered-alist-only to *-regexp-filtered-bookmark-name-alist-only.
;;     Bound these commands to P B, P F, and P T.  Updated bookmark-bmenu-mode doc string.
;; 2009/10/18 dadams
;;     Added: *-bmenu-filter-(incrementally|delay|prompt|pattern|timer|alist-by-regexp),
;;            *-bmenu-read-filter-input, *-regexp-filtered-alist-only,
;;            *-bmenu-cancel-incremental-filtering.
;;     *-bmenu-execute-deletions: Don't update menu list if this is a no-op.
;;     Updated Commentary.
;;     Thx to Thierry Volpiatto.
;;     Added: *-marked-cp, *-bmenu-sort-marked-before-unmarked.  Bound to s >.
;;     *-define-sort-command: Use copy-sequence for default value.
;; 2009/10/17 dadams
;;     Added: *-read-tags-completing, *-set-union, *-tag-history, *-describe-bookmark,
;;            *-bmenu-describe-this-bookmark.  Bound *-bmenu-describe-this-bookmark to C-h RET.
;;     Use *-read-tags-completing instead of *-read-tags.
;;     *-sort-orders-for-cycling-alist: Use copy-sequence.
;;     *-bmenu-change-sort-order: Use member, not memq.
;;     *-get-bookmark: Handle case of non-string, non-cons. Document NOERROR in doc string.
;;     *-bmenu-execute-deletions: Fix so marks aren't removed if when delete.  Thx to Thierry.
;;     Convert recorded time to an Emacs time spec:
;;       *-make-record-default, -add-or-update-time: Use current-time, not bookmark-float-time.
;;       *-get-visit-time: Convert a deprecated time entry to an Emacs time spec.
;;       *-bookmark-last-access-cp: Convert recorded time to a number for comparison.
;;     Added: *-bmenu-show-filenames (redef of vanilla: put props on whole line, fit frame).
;;     Removed: old-bookmark-insert-location.
;;     *-insert-location: Do not call original.  Redefined: do not add text properties.
;;     *-bmenu-list, *-bmenu-hide-filenames: Put properties on line up to max width.
;;     *-bmenu-goto-bookmark-named: Allow trailing whitespace, since we use the whole line now.
;;     *-bmenu-list: Use pop-to-buffer, not switch-to-buffer.  Use do-list, not mapcar.
;;     *-bmenu-hide-filenames: fit-frame-if-one-window.
;;     *-bmenu-propertize-item: Better help-echo text.
;;     Updated bookmark-alist doc string to mention visits, time, and tags entries.
;; 2009/10/16 dadams
;;     Added tags feature.
;;       Added: *-(get|read)-tags, *-has-tag-p, *-remove(-all)-tags(-from-all),
;;              *-bmenu-remove-tags-from-marked, *-add-tags(-to-marked), *-rename-tag,
;;              *-bmenu-(un)mark-bookmarks-tagged-(all|none|some|not-all),
;;              *-bmenu-mark/unmark-bookmarks-tagged-(all/none|some/not-all).
;;       Bound to prefix key T.
;;       *-bmenu-mode: Updated doc string.
;;     Added: bookmarkp-default-bookmark-name.  Use as default instead of *-current-bookmark.
;;     Renamed: *-maybe-save-bookmark to *-maybe-save-bookmarks.
;;     Menu-list commands: Raise an error if command is used outside the menu list.
;; 2009/10/15 dadams
;;     Added: *-bmenu-(search|query-replace)-marked-bookmarks-regexp.  Bound to M-a, M-q.
;;     Renamed: *-non-marked-bookmarks-only to *-unmarked-bookmarks-only,
;;              *-bookmark-marked-alist to *-bmenu-marked-bookmarks.
;;     *-increment-visits, *-add-or-update-time:
;;       Set *-bmenu-called-from-inside-p to t, so we don't remove marks.
;;     Redefined *-bmenu-bookmark to get name from *-latest-sorted-alist.  Thx to Thierry V.
;;       *-bmenu-surreptitiously-rebuild-list, *-bmenu-list:
;;         Removed optional arg DONT-TOGGLE-FILENAMES-P.
;;       *-bmenu-execute-deletions, *-bmenu-toggle-show-only-(un)marked, *-bmenu-(un)mark-all,
;;         *-bmenu-regexp-mark, *-bmenu-toggle-marks:
;;           Do not bother with *-bmenu-toggle-filenames and rebuilding the menu list.
;; 2009/10/14 dadams
;;     Added: *-bmenu-delete (redefinition), *-isearch-bookmarks,
;;            *-bmenu-isearch(-marked)-bookmarks(-regexp), *-isearch-next-bookmark-buffer.
;;            Bound multi-isearch commands to to M-s a C(-M)-s.
;; 2009/10/13 dadams
;;     Added: *-make-dired-record, *-jump-dired, *-dired-bookmark-p, *-dired-alist-only,
;;            *-bmenu-show-only-dired.  Bound *-bmenu-show-only-dired to M-d.
;;     bookmarkp-file-bookmark-p: Include bookmarks that have the Dired handler.
;;     Moved *-sort-orders-for-cycling-alist defcustoms after *-define-sort-command calls.
;;     Call bookmarkp-msg-about-sort-order only when interactive.
;;     *-add-or-update-time, *-increment-visits: Do not save each time we access a bookmark.
;;     Updated doc string of bookmark-alist and Commentary.
;; 2009/10/09 dadams
;;     Added: bookmarkp-bmenu-delete-marked.  Bound it to D.
;;            bookmarkp-sort-orders-for-cycling-alist.
;;     Renamed: bookmarkp-sort-functions-alist to bookmarkp-sort-orders-alist,
;;     bookmark-bmenu-execute-deletions: Added optional arg, for *-bmenu-delete-marked.
;;     *-sort-function: Changed default value to sorting by bookmark type (`s k').
;;     *-bmenu-change-sort-order: Use *-sort-orders-for-cycling-alist, not all sort orders.
;;     Updated Commentary and doc string (bookmark-bmenu-mode).
;; 2009/10/08 dadams
;;     Added: *-bmenu-sort-by-(w3m-url|gnus-thread), *-(gnus|w3m)-cp, *-cp-not,
;;            *-local-file-(accessed|updated)-more-recently-cp, *-bmenu-sort-by-bookmark-type.
;;     Renamed: *-bmenu-sort-by(-last)-file-(size|type|access|update) to
;;              *-bmenu-sort-by(-last)-local-file-(size|typeaccess|update),
;;              *-file-visited-more-recently-cp to *-local-file-accessed-more-recently-cp,
;;              *-file-(size|type)-cp to *-local-file-(size|type)-cp.
;;     Removed: *-file-(device|gid(-chg)|inode|last-(access|update|status-change)|links|modes
;;                            |uid)-cp.
;;     Bound *-bmenu-sort-by-bookmark-type to `s k'.
;;     *-define-file-sort-predicate: Use *-file-bookmark-p, not *-local-file-bookmark-p.
;;     *-msg-about-sort-order: Added optional arg PREFIX-ARG.  Use in: *-show-(all|only-*).
;; 2009/10/07 dadams
;;     Renamed: *-bmenu-sort-by-last-visit-time to *-bmenu-sort-by-last-bookmark-access,
;;              *-bmenu-sort-by-visit-frequency to *-bmenu-sort-by-bookmark-visit-frequency,
;;              *-visited-more-recently-cp to *-bookmark-last-access-cp.
;; 2009/10/06 dadams
;;     Added: bookmarkp-msg-about-sort-order.
;;     bookmark-completing-read: Simple sort when use menu-bar menu.
;; 2009/10/05 dadams
;;     Added: *-make-plain-predicate, *-reverse-multi-sort-order, *-multi-sort,
;;            *-define-file-sort-predicate, *-bmenu-sort-by-file-*, *-file-attribute-*-cp,
;;            and aliases *-file-*-cp, *-current-sort-order.
;;     Redefined sorting to allow multi-predicates:
;;       Redefined: *-sort-function, *-sort-and-remove-dups, *-define-sort-command,
;;                  *-sort-functions-alist.
;;     Bound keys with `s f' prefix to file-sorting commands
;;     *-current-sort-order: Use rassoc instead of rassq now.
;;     Swap keys s and S.  S is now bookmark-bmenu-save.  s is not the sorting prefix key.
;;     bookmark-bmenu-mode: Mention S key explicitly here (even though it is also
;;                          mentioned in the vanilla part of the doc string).
;; 2009/10/04 dadams
;;     *-bmenu-change-sort-order-repeat: Require repeat.el.
;;     Renamed: bookmarkp-current-sec-time to bookmarkp-float-time.
;;     *-float-time: Added arg, so it's the same as float-time (for Emacs 20).
;;     Bind *-reverse-sort-order to `S R'.
;;     *-remote-file-bookmark-p: Removed extra rem-file in last and.
;;     *-non-file-bookmark-p: Ensure it's not a remote file, before calling file-exists-p.
;; 2009/10/03 dadams
;;     Added: bookmarkp-file-remote-p, bookmarkp-buffer (face).
;;     bookmarkp-non-file (face): Changed to gray.
;;     *-default-handler, *-bmenu-propertize-item, *-(info|file)-bookmark-p:
;;       Support Emacs 20-21 Info-node bookmarks.
;;     bookmarkp-bmenu-propertize-item: Use different face for existing buffers.
;;                                      Use bookmarkp-non-file-filename.
;;     bookmarkp-non-file-bookmark-p: Include buffer bookmarks for nonexistent buffers.
;;     bookmarkp-remote-file-bookmark-p: Use bookmarkp-file-remote-p.
;;     bookmark-handle-bookmark:
;;       Redefine for all Emacs versions.  Handle buffer (non-file) bookmarks.
;;     Reordered some function definitions.
;; 2009/10/02 dadams
;;     Added: bookmarkp-bmenu-goto-bookmark-named, bookmarkp-latest-sorted-alist.
;;     *-sort-and-remove-dups: Set *-latest-sorted-alist (not used yet).
;;     *-define-sort-command, *-bmenu-change-sort-order, *-reverse-sort-order:
;;       Bind *-bmenu-called-from-inside-p to t, to prevent losing marks.
;;       Restore cursor position to same bookmark after sorting - use *-goto-bookmark-named.
;;     *-bmenu-surreptitiously-rebuild-list, *-bmenu-list: Added arg DONT-TOGGLE-FILENAMES-P.
;;     *-bmenu-execute-deletions, *-bmenu-toggle-show-only-(un)marked:
;;       Call *-surreptitiously-* with arg DONT-TOGGLE-FILENAMES-P.
;;     *-bmenu-hide-filenames: Simplify - don't get to position by searching backward.
;;     *-handle-region-default: Use forward-line, not goto-line.
;;     Thx to Thierry V.
;; 2009/10/01 dadams
;;     Added: bookmarkp-some-unmarked-p.
;;     Renamed: *-bmenu-toggle-show-only-<TYPE> to *-bmenu-show-only-<TYPE>,
;;              *-bmenu-called-from-inside-flag to *-bmenu-called-from-inside-p.
;;     bookmarkp-some-marked-p: Do not test bookmarkp-bookmark-marked-alist.
;;                              Arg must be required (explicit).  Changed calls accordingly.
;;     bookmark-bmenu-mode: Cleaned up doc string.
;;     bookmark-bmenu-((un)mark|rename|edit-*|toggle-marks|surreptitiously-rebuild-list),
;;       bookmarkp-root-or-sudo-logged-p, bookmarkp-jump-w3m-(new-session|only-one-tab),
;;       bookmarkp-some-marked-p:
;;         Inline let vars used only once.
;;     bookmarkp-bmenu-toggle-show-only-marked:
;;       Test bookmarkp-some-unmarked-p, not bookmarkp-some-marked-p,
;;            and include *-before-hide-unmarked in the test.
;;     bookmarkp-bmenu(-toggle)-show-only-*: Display status message.
;;     bookmarkp-bmenu-toggle-show-only-(un)marked: Fit frame.
;;     bookmark-prop-set: Fixed, so it handles old bookmark format also.
;; 2009/10/01 Thierry Volpiatto
;;     Removed: bookmarkp-bmenu-restore-marks.
;;     bookmark-bmenu-list:
;;       Do the mark restoration in line, at the same time as the annotation * restoration.
;;       Simplify use of START and END.
;; 2009/09/30 dadams
;;     bookmarkp-bmenu-regexp-mark: Remove binding of bookmark-alist.
;;     bookmark-bmenu-(un)mark, bookmarkp-bmenu-edit-bookmark (remove first call only),
;;       bookmark-bmenu-other-window, bookmark-bmenu-rename, bookmarkp-bmenu-restore-marks:
;;         Remove bookmark-bmenu-check-position (done by bookmark-bmenu-bookmark anyway).
;;     bookmark-insert-location: Fix interactive spec for Emacs < 22.
;;     bookmark-location: Return "" instead of raising error, if no location found.
;;     bookmarkp-current-sec-time: Move the let: do not call current-time unless we need to.
;;     bookmarkp-bmenu-unmark-all: forward-line only 1, not 2.  Thx to Thierry.
;;     bookmark-bmenu-mode: Updated doc string - bindings and mention options.
;;     bookmarkp-bmenu-propertize-item: For buffer, check also against "   - no file -".
;; 2009/09/29 dadams
;;     bookmark-bmenu-unmark: Use delete, not remove.
;;     Removed: bookmark-bmenu-check-position, bookmarkp-maybe-sort.
;;     Added: bookmarkp-sort-and-remove-dups, bookmarkp-remove-assoc-dups,
;;            bookmarkp-face-prop, bookmarkp-bad-bookmark, bookmark-menu-heading (Emacs 20,21),
;;            bookmark-bmenu-bookmark (redefinition).
;;     *-bmenu-toggle-show-only-*: Do not call-interactively.
;;     bookmarkp-bmenu-(un)mark-all:
;;       Handle bookmark-bmenu-toggle-filenames (wrapper).
;;       Remove bookmark-bmenu-check-position - just ensure within menu list.
;;     bookmarkp-bmenu-mark-all: Move save-excursion so it applies to all movements.
;;                               Message stating number marked.
;;     bookmarkp-bmenu-unmark-all: Use with-current-buffer ensure in menu list.
;;                                 Use bookmark-bmenu-unmark.
;;     Fixed U bindings for bookmarkp-bmenu-unmark-all.
;;     bookmarkp-bmenu-regexp-mark:
;;       Remove bookmark-bmenu-check-position - just ensure in menu list.
;;     bookmarkp-bmenu-toggle-marks: Use forward-line 2, to ensure in menu list.
;;                                   Message stating number marked.
;;     bookmark-bmenu-list, bookmarkp-bmenu-propertize-item: Use bookmarkp-face-prop.
;;     bookmark-bmenu-list: Don't start applying the faces until column 2.
;;     Removed key bindings in bookmark-map for *-toggle-show-only-*.
;;     Redefined faces, esp. for a light background.
;;     Use font-lock-face or face property, depending on Emacs version.
;;
;; 2009-06-09 to 2009-09-27 Thierry Volpiatto and dadams
;;     New features, as follows.
;;       See also the change log at
;;         http://mercurial.intuxication.org/hg/bookmark-icicle-region/.
;;       2090-09-27 Rewrote sorting and unmarking code.  (+ Updates to doc, names.)
;;                    Unmarking is now like Dired & query-replace.
;;                    Sorting is via one sort function; sort predicates do all the sorting.
;;                    Can now cycle sort orders with S S S...
;;                    Sort cmds now cycle among normal, reverse, off.
;;                    Add: *-define-sort-command (macro), *-assoc-delete-all, *-upcase,
;;                         *-get-visits-count, *-get-visit-time, *-sort-functions-alist.
;;                  Remove docstring from defalias (for Emacs 20).
;;       2009-09-26 Fix *-bmenu-mode doc (defadvice).
;;       2009-09-25 *-bmenu-edit, *-bmenu-sort-1: Fix bmk retrieval code.
;;                  Redefine *-bmenu-unmark.  Add: *-bmenu-toggle-marks.
;;                  Bind *-bmenu-unmark-all-bookmarks to M-DEL.  Reorder code.
;;                  Rename: *-bmenu-unmark-all-(bookmarks|(delete|mark)-flag)',
;;                          *-bmenu-unmark-all-bookmarks-1.
;;                  Change sort predicates in defalias.  Rename bmk entry visit to visits.
;;                  Remove: *-bmenu-show-number-of-visit.
;;       2009-09-22 Rewrote sorting code.  Renamed unmarking fns.
;;       2009-09-21 Rename mark/unmark cmds to have -bmenu.
;;                  Add: *-bmenu-called-from-inside-flag - set it in (un)hide marks fns.
;;       2009-09-20 *-write-file: Remove text properties before saving.
;;                  Remove all marks only in current display.
;;       2009-09-19 *-current-sec-time: Protect with fboundp for Emacs 20.
;;                  *-bmenu-sort-1: Add msg showing sort method.
;;                  Change key S-A to S-S (A is annotations).
;;       2009-09-18 Improve sorting by visit frequency.  Always increment when jump.
;;                  Fix increment visit fn.  Allow sorting by last visited.
;;                  When visit values are equal, sort with string-lessp.
;;                  Add TIME bookmark-alist entry.  *-make-record-default: Add time entry.
;;                  Fix: bad parens, errors in sorting predicate.  Rename fns.  
;;                  Use single fn to sort using diff methods.
;;                  Add: *-bmenu-refresh-alist (bind to g).
;;       2009-09-16 Add: *-toggle-sorting-by-most-visited, *-reset-visit-flag,
;;                       *-bmenu-show-number-of-visit.
;;                  Redefine *-prop-set.  Improve *-toggle-sorting-by-most-visited.
;;                  Add auto-doc to header.  *-bmenu-mode: Add missing key.
;;                  Update menu-list after jumping.
;;                  Increment save counter when jump with visit flag.
;;       2009-09-15 Record number of visits.  Added sorting by most visits.
;;       2009-09-14 Add doc string.  Update defadvice doc string wrt keys.
;;       2009-09-12 Add: fns to mark all, unmark D or > or all, *-bmenu-reset-alist.
;;                  Fix keymap (Emacs 20).  *-unmark-all-bookmarks1: Move the save-excursion.
;;       2009-09-11 Add: *-bmenu-check-position (redef to improve performance),
;;                       *-unmark-all-bookmarks, *-current-list-have-marked-p,
;;                       *-bookmark-marked-p, *-(non-)marked-bookmarks-only.
;;                  *-bmenu-hide-unmarked: Add toggling.  Restore lost fn.
;;                  Reorder code.  Bind cmds in *-bmenu-mode-map.
;;                  *-bmenu-hide-marked: Do not hide if no marked in current filter.
;;                  Improve: *-bmenu-hide(-not)-marked-bookmark, (un)hide marked fns.
;;       2009-09-10 Fix *--restore-all-mark, *-bmenu-regexp-mark.
;;                  *-bmenu-list: Add *-restore-all-mark.
;;                  *-bmenu-mark: Push marked bmk to marked list.
;;                  Add: bookmarkp-bookmark-marked-list, *-bmenu-quit.
;;       2009-09-09 *-maybe-sort-alist: Use copy-sequence.
;;                  So remove fixes for *-rename, *-edit-bookmark.
;;                  *-yank, *-rename', *-set: Fix yanking.
;;                  Remove non-file bmks from file-only list.
;;                  Add: *-bmenu-list-only-non-file-bookmarks, *-maybe-message (Emacs 20),
;;                       *-bmenu-mark-bookmark-matching-regexp, *-bmenu-hide-marked-bookmark,
;;                       *-bmenu-hide-not-marked-bookmark, *-bmenu-mark (redefinition).
;;                  *-write-file: Improve performance.
;;                  *-non-file-alist-only: Remove unused def.
;;                  Fix: hide marked/unmarked with toggle-filenames, keymap for Emacs 20.
;;                  Improve comments, doc strings.
;;                  *-bmenu-mark-bookmark-matching-regexp: Fix while loop.
;;       2009-09-08 bookmark-store: Remove duplicate setq of *-current-bookmark.
;;       2009-09-07 Reorganize (reorder), add comments, improve doc strings.
;;                  Change binding of *-bmenu-relocate from R to M-r.
;;       2009-09-06 bookmark-rename: Redefine with new arg BATCH.
;;                  *-bmenu-rename: Go to new pos, not old.
;;                  *-edit-bookmark, bookmark-rename: Fix display update for Emacs 20.
;;       2009-09-05 Add: *-edit-bookmark, *-bmenu-edit-bookmark, *-maybe-save-bookmark.
;;       2009-09-04 Require cl.  Allow RET in Emacs 20.  Add doc string.
;;                  *-fix-bookmark-alist-and-save:
;;       2009-09-03 Fix *-fix-bookmark-alist-and-save:
;;                    Use error, not message.  Change value for setcdr.
;;                    Do not use push with non-var (cl).
;;                  bookmark-delete:
;;                    Redefine, to fix vanilla bug: increment count even when batch.
;;                  *-non-file-name: Change to - no file -.  *-bmenu-list: Add arg FILTER-ON.
;;                  *-bmenu-execute-deletions: Use delete, not remove.
;;                  Add: *-replace-regexp-in-string.
;;                  bookmark-set: Fix *-yank-point for region case.  Fix bad parens.
;;       2009-09-02 Add: *-non-file-filename.  *-fix-bookmark-alist-and-save: Fix msg.
;;                  Require cl (gensym).  *-at-bol/eol' -> line-*-position (for Emacs 20).
;;                  bookmark-delete: increment *-count if batch arg (fixes vanilla bug).
;;                  Redefine *-bmenu-execute-deletions,
;;                           *-bmenu-surreptitiously-rebuild-list. 
;;                  Update current filtered display - do not reload & display all bmks.
;;                  Add redefinitions of *-bmenu-rename', *-yank-word to fix vanilla bugs:
;;                    *-bmenu-rename: Do not call *-bmenu-list twice.
;;                  *-yank-word: Do not insert whitespace.
;;                  Rename *-last-bookmark-alist-in-use to *-latest-bookmark-alist.
;;       2009-09-01 Fix: Loading of new doc for bookmark-alist (add vacuous defvar).
;;                       *-bmenu-(list|hide-filenames): start -> end, end -> start.
;;                  Removed extraneous quote mark that caused problems.
;;                  Save only if condition-case exits OK.
;;       2009-08-31 Fixes: Test for non-file bmk.  Filename for Gnus bmks.
;;                         Compatibility of bookmark-alist with vanilla Emacs.
;;                         Require info.el and ffap.el when needed.
;;                  Add: *-line-number-at-pos (for Emacs 20),
;;                       *-bmenu-other-window (redefinition).
;;                  Rename *-propertize-bookmark-list to *-propertize-bmenu-item.
;;       2009-08-30 Fix: Increment *-alist-modification-count when relocate region,
;;                       and maybe save.
;;                  Move code adding properties to bookmarks out of *-bmenu-list.
;;                  mapc -> mapcar.  *-bmenu-hide-filenames: Redefine.
;;       2009-08-29 Remove refresh code.
;;       2009-08-27 Added: *-local-directory-bookmark-p, *-local-file-alist-only,
;;                         *-non-file-alist-only.
;;                  *-file-bookmark-p: Redefined to exclude bmks with handlers.
;;                  Renamed fns and faces.
;;       2009-08-25 Fit frame after display menu list.
;;                  Refresh list when toggle filename visibility.
;;       2009-08-24 Fix: *-bmenu-list for remote files, bookmark-set,
;;                       *-remote-file-bookmark-p.
;;                  Ensure arg to file-remote-p is not nil.
;;                  Recenter region only if it is visible.
;;       2009-08-23 Remove old *-location.  *-bmenu-list: Add isw3m.
;;                  bookmark-set:
;;                    Redefine for older Emacs. Use a default prompt for gnus, w3m.
;;                    Use *-name-length-max for title when region is active.
;;                    Ensure bookmark is on one line.
;;       2009-08-22 Try to handle tramp ftp files.
;;                  Do not fail if bookmark has empty filename entry.
;;                  Show region end pos using exchange-point-and-mark.
;;       2009-08-21 Remove all cl stuff (list*, loop, etc.).  Add *-remove-if(-not).
;;                  Remove compile-time require of cl.
;;                  Add predicates *-(region|gnus|w3m|info|(remote-|local-)file)-bookmark-p.
;;                  Redefine alist-only functions to optimize and use new preds.
;;       2009-08-20 *--jump-via: Fix to show relocated region before ask whether to save.
;;                  *-relocate-region: Fix ar-str.  Rename a var.  Add: *-region-activated-p.
;;                  Revert chgs.
;;       2009-08-19 Update/fix commentary: bookmark-alist, linkd.
;;                  *-default-handler, *-handle-region-default:
;;                    Get bmk record from name only once.
;;                  *-save-new-region-location: Move t inside progn.
;;       2009-08-16 Use prefix bookmarkp where appropriate.
;;       2009-08-15 Improve comments, doc strings.  Rename fns, vars.
;;                  Add :group with prefix bookmarkp.
;;       2009-08-09 Fix doc strings.
;;       2009-08-08 bookmark-set: Update doc string.  Show keys in C-h m.
;;       2009-08-07 *-jump: Fix to jump in same window (thx to Henry Atting).
;;                  *-at-bol/eol' -> line-*-position.
;;       2009-08-01 bookmark-store: Fix for Emacs 20.
;;       2009-07-27 Ensure we start on an empty w3m buffer.  Add: *-w3m-allow-multi-tabs.
;;       2009-07-24 *-bmenu-mode-map: Define some new keys.
;;       2009-07-23 *-bmenu-list: Add path to file in help-echo.
;;       2009-07-19 Fix title underline.  Highlight bookmark if root logged with tramp.
;;                  Add face for sudo.
;;       2009-07-18 Add: filter functions, option for bookmark-bmenu-list.
;;                  Remove toggle region.
;;                  *-bmenu-list-only-files-entries: Add prefix arg to show remote.
;;       2009-07-14 Add a forgotten test.
;;       2009-07-13 Fix errors.  Give pos in msg even if no search.
;;                  *-from-bob/eob: Fixed like old strict.
;;                  Remove *-relocate-region-(method|from-limits).
;;                  Remove unused code in record fns.  Add: *-relocate-region-function.
;;       2009-07-12 Do not pass args to relocation routines.  Remove use of flet.
;;                  Use skip-chars-*ward.  Use forward-line, not beginning-of-line.
;;                  Remove save-excursion around message.  Correct typo f(ree var BMK).
;;       2009-07-11 Fix *-relocate-region-strict.  Rename fns, adjust defcustom.
;;                  Save relocated region after pushing mark.  Add comments.
;;       2009-07-10 New retrieve fn.  Add looking-* fns.
;;       2009-07-08 Simplify record fns.  Add doc strings.  Add: *-save-relocated-position.
;;                  Fix: updating when relocate (wrt new record fns), string closing,
;;                       free vars, parens, names.
;;       2009-07-06 Fix: *-bmenu-list wrt windows, Info colors.
;;       2009-07-04 Rename fns to record, vars, args of retrieve fns.  Big changes, fixes.
;;       2009-07-01 Fix comments.  *-retrieve-region-strict: improve when out of context.
;;       2009-06-30 Fix: free vars, *-retrieve-region, provide (name).
;;       2009-06-29 Fix: w3m handler, file name for non-file, *-simple-retrieve-position.
;;                  Add: *-retrieve-region-strict.
;;       2009-06-28 Add: *-retrieve-region-method-is, *-retrieve-region-lax,
;;                       fns to retrieve regions.
;;                  Use buffer again, not buffer-name.
;;       2009-06-27 Fix wrong-type error no such file.  Renamed faces.  Add: *-prop-set.
;;                  Load gnus at compile time.  Protect tramp-file-name-regexp with boundp.
;;       2009-06-25 Fixes for older Emacs compatibility.
;;       2009-06-24 Improve *-default-handler.
;;                  Add: *-always-save-relocated-position, *-prop-get.
;;       2009-06-23 Use search-regexp, not re-search-regexp.  Add Gnus bmks.  Add doc.
;;                  Fix *-bmenu-list.
;;       2009-06-21 Fix: *-default-handler for Info.  Improve doc strings, commentary.
;;                  Fixes to be compatible with Emacs 20-22.
;;                  Use defcustom for *-list-only-regions-flag.
;;                  *jump: Put prefix arg in interactive spec.  Use buffer-name, not buffer.
;;                  Remove require of Tramp and inline Tramp fns.
;;                  Remove tests for display-(color|mouse)-p.
;;                  w3m-bookmark-(jump|make-record): require w3m.
;;       2009-06-20 Update context strings when relocate.
;;                  Fix: bookmark-get-*: search from point-min.
;;       2009-06-19 Fix: *-make-record-default, *-toggle-use-only-regions, *-default-handler,
;;                       bookmarking Dired.
;;                  Handle 4 positions in *-default-handler.
;;       2009-06-17 Fix: case where some bookmarked text was removed, *-use-region.
;;       2009-06-15 Fix *-region-alist-only, *-get-buffername, *-location,
;;                      non-file (buffer) bookmarks.
;;                  Support w3m similar to Info.
;;       2009-06-14 Fix bookmark+version, region relocation.  Info support.  Error handling.
;;       2009-06-13 Fix: *-list-only-regions, *-region-handler, *-make-record, keymap, faces.
;;                  Put region & info handling in *-default-handler, not separate handlers.
;;                  Merge *-make-record-region to *-make-record-default.
;;                  *-jump now negates *-use-region if prefix arg.  Raise error if bad bmk.
;;       2009-06-12 Add: *-get-endposition, *-region-alist-only-names.
;;                  Add filter to show only region bookmarks.
;;                  Faces for menu list.  Change region color.
;;       2009-06-11 Add: *-region-search-size, *-get-buffername, *-use-region.
;;                  Redefine *-handle-bookmark, *-jump, to fit bookmark-use-region.
;;                  Add condtions to bookmark-make-record.  Support w3m.  Support t command.
;;       2009-06-10 Fix search regexp.  Fix region in Info. Save bookmark if region moves.
;;       2009-06-09 Added: bookmark-make-record(-region), bookmark-region-handler.
;;                  Relocation.
;; 2009/05/25 dadams
;;     Added redefinition of bookmark-get-bookmark-record.
;; 2008/10/16 dadams
;;     bookmark-jump-other-window: Don't define it for Emacs 23+ (not needed).
;; 2008/04/04 dadams
;;     bookmark-jump-other-window: Updated wrt Emacs 22.2.
;; 2007/10/07 dadams
;;     Added: bookmark-completing-read, bookmark-delete, bookmark-insert(-location),
;;            bookmark-jump, bookmark-relocate, bookmark-rename.
;;     bookmark-jump-other-window: Use new bookmark-completing-read.
;; 2007/07/13 dadams
;;     Replaced Emacs version tests to reflect Emacs 22 release.
;; 2006/03/08 dadams
;;     bookmark-jump-other-window: Handle nil arg.
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2004/11/20 dadams
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/10/26 dadams
;;     Different menu-bar command, depending on Emacs version.
;; 2004/09/21 dadams
;;     Only define bookmark-menu-jump-other-window if < Emacs 22.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(require 'bookmark)
(unless (fboundp 'file-remote-p) (require 'ffap)) ;; ffap-file-remote-p
(eval-when-compile (require 'gnus)) ;; mail-header-id (really in `nnheader.el')
(eval-when-compile (require 'cl)) ;; case, gensym, loop, pushnew


(defconst bookmarkp-version-number "2.6.2")

(defun bookmarkp-version ()
  "Show version number of library `bookmark+.el'."
  (interactive)
  (message "Bookmark+, version %s" bookmarkp-version-number))


;; Quiet the byte-compiler
(defvar w3m-current-url)                ; Defined in `w3m.el'.
(defvar w3m-current-title)              ; Defined in `w3m.el'.
(defvar gnus-article-current)           ; Defined in `gnus-sum.el'.
(defvar tramp-file-name-regexp)         ; Defined in `tramp.el'.
(defvar bookmark-make-record-function)  ; Defined in `bookmark.el'.
(defvar Info-current-node)              ; Defined in `info.el'.
(defvar Info-current-file)              ; Defined in `info.el'.

;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Macros")
;;; Macros -----------------------------------------------------------

(defmacro bookmarkp-define-sort-command (sort-order comparer doc-string)
  "Define a command to sort bookmarks in the menu list by SORT-ORDER.
SORT-ORDER is a short string or symbol describing the sorting method.
Examples: \"by last access time\", \"by bookmark name\".

The new command is named by replacing any spaces in SORT-ORDER with
hyphens (`-') and then adding the prefix `bookmarkp-bmenu-sort-'.
Example: `bookmarkp-bmenu-sort-by-bookmark-name', for SORT-ORDER `by
bookmark name'.

COMPARER compares two bookmarks, returning non-nil if and only if the
first bookmark sorts before the second.  It must be acceptable as a
value of `bookmarkp-sort-comparer'.  That is, it is either nil, a
predicate, or a list ((PRED...) FINAL-PRED).  See the doc for
`bookmarkp-sort-comparer'.

DOC-STRING is the doc string of the new command."
  (unless (stringp sort-order) (setq sort-order  (symbol-name sort-order)))
  (let ((command  (intern (concat "bookmarkp-bmenu-sort-"
                                  (bookmarkp-replace-regexp-in-string
                                   "\\s-+" "-" sort-order)))))
    `(progn
      (setq bookmarkp-sort-orders-alist
       (bookmarkp-assoc-delete-all ,sort-order (copy-sequence bookmarkp-sort-orders-alist)))
      (push (cons ,sort-order ',comparer) bookmarkp-sort-orders-alist)
      (defun ,command ()
        ,(concat doc-string "\nRepeating this command cycles among normal sort, reversed \
sort, and unsorted.")
        (interactive)
        (bookmarkp-barf-if-not-in-menu-list)
        (cond (;; Not this sort order - make it this sort order.
               (not (equal bookmarkp-sort-comparer ',comparer))
               (setq bookmarkp-sort-comparer   ',comparer
                     bookmarkp-reverse-sort-p  nil))
              (;; This sort order reversed.  Change to unsorted.
               bookmarkp-reverse-sort-p
               (setq bookmarkp-sort-comparer   nil
                     bookmarkp-reverse-sort-p  t))
              (t;; This sort order - reverse it.
               (setq bookmarkp-reverse-sort-p  t)))
        (message "Sorting...")
        (bookmark-bmenu-check-position)
        (let ((current-bmk  (bookmark-bmenu-bookmark)))
          (bookmark-bmenu-surreptitiously-rebuild-list)
          (bookmarkp-bmenu-goto-bookmark-named current-bmk)) ; Put cursor back on right line.
        (when (interactive-p) (bookmarkp-msg-about-sort-order ,sort-order))))))

(defmacro bookmarkp-define-file-sort-predicate (att-nb)
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
  `(defun ,(intern (format "bookmarkp-file-attribute-%d-cp" att-nb)) (b1 b2)
    ,(format "Sort files by attribute %d, then file names with no files, \
then the rest."
             att-nb)
    (let (a1 a2)
      (cond (;; Both are file bookmarks.
             (and (bookmarkp-file-bookmark-p b1)
                  (bookmarkp-file-bookmark-p b2))
             (setq a1  (file-attributes (bookmark-get-filename b1))
                   a2  (file-attributes (bookmark-get-filename b2)))
             (cond (;; Both have attributes.
                    (and a1 a2)
                    (setq a1  (nth ,att-nb a1)
                          a2  (nth ,att-nb a2))
                    ;; Convert times and maybe inode number to floats.
                    ;; The inode conversion is kludgy, but is probably OK in practice.
                    (when (consp a1) (setq a1  (bookmarkp-float-time a1)))
                    (when (consp a2) (setq a2  (bookmarkp-float-time a2)))
                    (cond (;; (1) links, (2) maybe uid, (3) maybe gid, (4, 5, 6) times
                           ;; (7) size, (10) inode, (11) device.
                           (numberp a1)
                           (cond ((< a1 a2) '(t))
                                 ((> a1 a2) '(nil))
                                 (t nil)))
                          ((= 0 ,att-nb) ; (0) file (nil) < symlink (string) < dir (t)
                           (cond ((and a2 (not a1)) '(t)) ; file vs (symlink or dir)
                                 ((and a1 (not a2)) '(nil))
                                 ((and (eq t a2) (not (eq t a1))) '(t)) ; symlink vs dir
                                 ((and (eq t a1) (not (eq t a2))) '(t))
                                 ((and (stringp a1) (stringp a2))
                                  (if (string< a1 a2) '(t) '(nil)))
                                 (t nil)))
                          ((stringp a1) ; (2, 3) string uid/gid, (8) modes
                           (cond ((string< a1 a2) '(t))
                                 ((string< a2 a1) '(nil))
                                 (t nil)))
                          ((eq ,att-nb 9) ; (9) gid would change if re-created. nil < t
                           (cond ((and a2 (not a1)) '(t))
                                 ((and a1 (not a2)) '(nil))
                                 (t nil)))))
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
             (bookmarkp-local-file-bookmark-p b1)
             '(t))
            (;; Second is a file, first is not.
             (bookmarkp-local-file-bookmark-p b2)
             '(nil))
            (t;; Neither is a file.
             nil)))))
  
;;(@* "Keymaps")
;;; Keymaps ----------------------------------------------------------

;; `bookmark-map'

;;;###autoload
(define-key ctl-x-map "p" bookmark-map)
;;;###autoload
(define-key ctl-x-map "pj" 'bookmark-jump-other-window)
;;;###autoload
(define-key bookmark-map "o" 'bookmark-jump-other-window)
;;;###autoload
(define-key bookmark-map "q" 'bookmark-jump-other-window)

;; `bookmark-bmenu-mode-map'

;;;###autoload
(when (< emacs-major-version 21)
  (define-key bookmark-bmenu-mode-map (kbd "RET") 'bookmark-bmenu-this-window))
;;;###autoload
(define-key bookmark-bmenu-mode-map "." 'bookmarkp-bmenu-show-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map ">" 'bookmarkp-bmenu-toggle-show-only-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "<" 'bookmarkp-bmenu-toggle-show-only-unmarked)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "M-<DEL>") 'bookmarkp-bmenu-unmark-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "%" nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "%m" 'bookmarkp-bmenu-regexp-mark)
;;;###autoload
(define-key bookmark-bmenu-mode-map "*" nil) ; For Emacs 20
;;;###autoload
(when (< emacs-major-version 21)
  (define-key bookmark-bmenu-mode-map "*m" 'bookmark-bmenu-mark))
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-a" 'bookmarkp-bmenu-search-marked-bookmarks-regexp)
;;;###autoload
(define-key bookmark-bmenu-mode-map "B" 'bookmarkp-bmenu-show-only-non-files)
;;;###autoload
(define-key bookmark-bmenu-mode-map "D" 'bookmarkp-bmenu-delete-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-d" 'bookmarkp-bmenu-show-only-dired)
;;;###autoload
(define-key bookmark-bmenu-mode-map "E" 'bookmarkp-bmenu-edit-bookmark)
;;;###autoload
(define-key bookmark-bmenu-mode-map "F" 'bookmarkp-bmenu-show-only-files)
;;;###autoload
(define-key bookmark-bmenu-mode-map "g" 'bookmarkp-bmenu-refresh-menu-list)
;;;###autoload
(define-key bookmark-bmenu-mode-map "G" 'bookmarkp-bmenu-show-only-gnus)
;;;###autoload
(define-key bookmark-bmenu-mode-map "P" nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "PB" 'bookmarkp-bmenu-filter-bookmark-name-incrementally)
;;;###autoload
(define-key bookmark-bmenu-mode-map "PF" 'bookmarkp-bmenu-filter-file-name-incrementally)
;;;###autoload
(define-key bookmark-bmenu-mode-map "PT" 'bookmarkp-bmenu-filter-tags-incrementally)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "C-h RET") 'bookmarkp-bmenu-describe-this-bookmark)
;;;###autoload
(define-key bookmark-bmenu-mode-map "I" 'bookmarkp-bmenu-show-only-info-nodes)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-m" 'bookmarkp-bmenu-mark-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "R" 'bookmarkp-bmenu-show-only-regions)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-r" 'bookmark-bmenu-relocate) ; `R' in Emacs
;;;###autoload
(define-key bookmark-bmenu-mode-map "q" 'bookmarkp-bmenu-quit)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-q"
  'bookmarkp-bmenu-query-replace-marked-bookmarks-regexp)
;;;###autoload
(define-key bookmark-bmenu-mode-map "S" 'bookmark-bmenu-save) ; `s' in Emacs
;;;###autoload
(define-key bookmark-bmenu-mode-map "s" nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "s>" 'bookmarkp-bmenu-sort-marked-before-unmarked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sb" 'bookmarkp-bmenu-sort-by-last-buffer-or-file-access)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sfd" 'bookmarkp-bmenu-sort-by-local-file-type)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sfn" 'bookmarkp-bmenu-sort-by-file-name)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sfs" 'bookmarkp-bmenu-sort-by-local-file-size)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sft" 'bookmarkp-bmenu-sort-by-last-local-file-access)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sfu" 'bookmarkp-bmenu-sort-by-last-local-file-update)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sg" 'bookmarkp-bmenu-sort-by-gnus-thread)
;;;###autoload
(define-key bookmark-bmenu-mode-map "si" 'bookmarkp-bmenu-sort-by-Info-location)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sk" 'bookmarkp-bmenu-sort-by-bookmark-type)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sn" 'bookmarkp-bmenu-sort-by-bookmark-name)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sr" 'bookmarkp-reverse-sort-order)
;;;###autoload
(define-key bookmark-bmenu-mode-map "s\C-r" 'bookmarkp-reverse-multi-sort-order)
;;;###autoload
(define-key bookmark-bmenu-mode-map "ss" 'bookmarkp-bmenu-change-sort-order-repeat)
;;;###autoload
(define-key bookmark-bmenu-mode-map "st" 'bookmarkp-bmenu-sort-by-last-bookmark-access)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sv" 'bookmarkp-bmenu-sort-by-bookmark-visit-frequency)
;;;###autoload
(define-key bookmark-bmenu-mode-map "sw" 'bookmarkp-bmenu-sort-by-w3m-url)
;;;###autoload
(when (> emacs-major-version 22)        ; Emacs 23+
 (define-key bookmark-bmenu-mode-map (kbd "M-s a C-s")
   'bookmarkp-bmenu-isearch-marked-bookmarks)
 (define-key bookmark-bmenu-mode-map (kbd "M-s a M-C-s")
   'bookmarkp-bmenu-isearch-marked-bookmarks-regexp))
;;;###autoload
(define-key bookmark-bmenu-mode-map "T" nil) ; For Emacs20
;;;###autoload
(define-key bookmark-bmenu-mode-map "T0" 'bookmarkp-remove-all-tags)
;;;###autoload
(define-key bookmark-bmenu-mode-map "T+" 'bookmarkp-add-tags)
;;;###autoload
(define-key bookmark-bmenu-mode-map "T-" 'bookmarkp-remove-tags)
;;;###autoload
(define-key bookmark-bmenu-mode-map "T>+" 'bookmarkp-bmenu-add-tags-to-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "T>-" 'bookmarkp-bmenu-remove-tags-from-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Td" 'bookmarkp-remove-tags-from-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tm*" 'bookmarkp-bmenu-mark-bookmarks-tagged-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tm%" 'bookmarkp-bmenu-mark-bookmarks-tagged-regexp)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tm+" 'bookmarkp-bmenu-mark-bookmarks-tagged-some)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tm~*" 'bookmarkp-bmenu-mark-bookmarks-tagged-not-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tm~+" 'bookmarkp-bmenu-mark-bookmarks-tagged-none)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tr" 'bookmarkp-rename-tag)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tu*" 'bookmarkp-bmenu-unmark-bookmarks-tagged-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tu+" 'bookmarkp-bmenu-unmark-bookmarks-tagged-some)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tu~*" 'bookmarkp-bmenu-unmark-bookmarks-tagged-not-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "Tu~+" 'bookmarkp-bmenu-unmark-bookmarks-tagged-none)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-t" 'bookmark-bmenu-toggle-filenames) ; `t' in Emacs
;;;###autoload
(define-key bookmark-bmenu-mode-map "t" 'bookmarkp-bmenu-toggle-marks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "U" 'bookmarkp-bmenu-unmark-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "W" 'bookmarkp-bmenu-show-only-w3m-urls)


(defadvice bookmark-bmenu-mode (before bookmark+-add-keymap () activate)
  "
***************************** Bookmark+ *****************************\
\\<bookmark-bmenu-mode-map>

Hide/show bookmarks
-------------------

\\[bookmarkp-bmenu-show-all]\t- Show all bookmarks
\\[bookmarkp-bmenu-toggle-show-only-marked]\t- Toggle showing only marked bookmarks
\\[bookmarkp-bmenu-toggle-show-only-unmarked]\t- Toggle showing only unmarked bookmarks
\\[bookmarkp-bmenu-show-only-non-files]\t- Show only non-file bookmarks
\\[bookmarkp-bmenu-show-only-files]\t- Show only file & directory bookmarks (`C-u': local only)
\\[bookmarkp-bmenu-show-only-dired]\t- Show only Dired bookmarks
\\[bookmarkp-bmenu-show-only-gnus]\t- Show only Gnus bookmarks
\\[bookmarkp-bmenu-show-only-info-nodes]\t- Show only Info bookmarks
\\[bookmarkp-bmenu-show-only-regions]\t- Show only region bookmarks
\\[bookmarkp-bmenu-show-only-w3m-urls]\t- Show only W3M bookmarks
\\[bookmarkp-bmenu-filter-bookmark-name-incrementally]\t- Incrementally show only bookmarks \
whose names match a regexp
\\[bookmarkp-bmenu-filter-file-name-incrementally]\t- Incrementally show only bookmarks whose \
files match a regexp
\\[bookmarkp-bmenu-filter-tags-incrementally]\t- Incrementally show only bookmarks whose tags \
match a regexp


Mark/unmark bookmarks (see also `Tags', below)
----------------------------------------------

\\[bookmarkp-bmenu-mark-all]\t- Mark all bookmarks
\\[bookmarkp-bmenu-regexp-mark]\t- Mark all bookmarks whose names match a regexp
\\[bookmarkp-bmenu-unmark-all]\t- Unmark all bookmarks (`C-u': interactive query)
\\[bookmarkp-bmenu-toggle-marks]\t- Toggle marks: unmark marked and mark unmarked


Sort bookmarks (repeat to cycle normal/reversed/off, except as noted)
---------------------------------------------------------------------

\\[bookmarkp-bmenu-sort-marked-before-unmarked]\t- Sort marked bookmarks first
\\[bookmarkp-bmenu-sort-by-last-buffer-or-file-access]\t- Sort by last buffer or file \
access
\\[bookmarkp-bmenu-sort-by-gnus-thread]\t- Sort by Gnus thread: group, article, message
\\[bookmarkp-bmenu-sort-by-Info-location]\t- Sort by Info manual, node, position
\\[bookmarkp-bmenu-sort-by-bookmark-type]\t- Sort by bookmark type
\\[bookmarkp-bmenu-sort-by-bookmark-name]\t- Sort by bookmark name
\\[bookmarkp-bmenu-sort-by-last-bookmark-access]\t- Sort by last bookmark access time
\\[bookmarkp-bmenu-sort-by-bookmark-visit-frequency]\t- Sort by bookmark visit frequency
\\[bookmarkp-bmenu-sort-by-w3m-url]\t- Sort by W3M URL

\\[bookmarkp-bmenu-sort-by-local-file-type]\t- Sort by local file type: file, symlink, dir
\\[bookmarkp-bmenu-sort-by-file-name]\t- Sort by file name
\\[bookmarkp-bmenu-sort-by-local-file-size]\t- Sort by local file size
\\[bookmarkp-bmenu-sort-by-last-local-file-access]\t- Sort by last local file access
\\[bookmarkp-bmenu-sort-by-last-local-file-update]\t- Sort by last local file update (edit)

\\[bookmarkp-reverse-sort-order]\t- Reverse current sort direction       (repeat to toggle)
\\[bookmarkp-reverse-multi-sort-order]\t- Complement sort predicate decisions  (repeat \
to toggle)
\\[bookmarkp-bmenu-change-sort-order-repeat]\t- Cycle sort orders                    (repeat \
to cycle)


Search-and-replace bookmark locations (in sort order)
-----------------------------------------------------

\\[bookmarkp-bmenu-search-marked-bookmarks-regexp]\t- Regexp-search the marked file bookmarks
\\[bookmarkp-bmenu-query-replace-marked-bookmarks-regexp]\t- Query-replace the marked file \
bookmarks
M-x a C-s\t- Isearch the marked bookmarks (Emacs 23+)
M-x a C-M-s\t- Regexp Isearch the marked bookmarks (Emacs 23+)


Tags
----

\\[bookmarkp-add-tags]\t- Add some tags to a bookmark
\\[bookmarkp-remove-tags]\t- Remove some tags from a bookmark
\\[bookmarkp-remove-all-tags]\t- Remove all tags from a bookmark
\\[bookmarkp-remove-tags-from-all]\t- Remove some tags from all bookmarks
\\[bookmarkp-rename-tag]\t- Rename a tag in all bookmarks

\\[bookmarkp-bmenu-add-tags-to-marked]\t- Add some tags to the marked bookmarks
\\[bookmarkp-bmenu-remove-tags-from-marked]\t- Remove some tags from the marked bookmarks

\\[bookmarkp-bmenu-mark-bookmarks-tagged-regexp]\t- Mark bookmarks having at least one \
tag that matches a regexp

\\[bookmarkp-bmenu-mark-bookmarks-tagged-some]\t- Mark bookmarks having at least one tag \
in a set    (OR)
\\[bookmarkp-bmenu-mark-bookmarks-tagged-all]\t- Mark bookmarks having all of the tags \
in a set     (AND)
\\[bookmarkp-bmenu-mark-bookmarks-tagged-none]\t- Mark bookmarks not having any of the tags \
in a set (NOT OR)
\\[bookmarkp-bmenu-mark-bookmarks-tagged-not-all]\t- Mark bookmarks not having all of the \
tags in a set (NOT AND)

\\[bookmarkp-bmenu-unmark-bookmarks-tagged-some]\t- Unmark bookmarks having at least one \
tag in a set  (OR)
\\[bookmarkp-bmenu-unmark-bookmarks-tagged-all]\t- Unmark bookmarks having all of the tags \
in a set   (AND)
\\[bookmarkp-bmenu-unmark-bookmarks-tagged-none]\t- Unmark bookmarks not having any tags \
in a set      (NOT OR)
\\[bookmarkp-bmenu-unmark-bookmarks-tagged-not-all]\t- Unmark bookmarks not having all tags \
in a set      (NOT AND)


Miscellaneous
-------------

\\[bookmarkp-describe-bookmark]\t- Show information about this bookmark
\\[bookmarkp-bmenu-refresh-menu-list]\t- Refresh (revert) to up-to-date bookmarks list
\\[bookmarkp-bmenu-delete-marked]\t- Delete visible bookmarks marked `>' (not `D')
\\[bookmarkp-bmenu-edit-bookmark]\t- Edit bookmark
\\[bookmark-bmenu-save]\t- Save bookmarks (`C-u': prompt for the bookmarks file to use)
\\[bookmarkp-bmenu-quit]\t- Quit (the bookmarks list)


Options affecting *Bookmarks List*
----------------------------------

`bookmarkp-sort-comparer'          - Initial sort
`bookmarkp-sort-orders-for-cycling-alist' -
                                   - Sort orders that \
`\\[bookmarkp-bmenu-change-sort-order-repeat]'... cycles
`bookmark-bmenu-toggle-filenames'  - Show filenames initially?
`bookmark-bmenu-file-column'       - Bookmark width if files are shown


Other options
-------------

`bookmark-default-file'            - File to save bookmarks in
`bookmarkp-save-new-location-flag' - Save if bookmark relocated?
`bookmark-save-flag'               - Whether and when to save
`bookmark-use-annotations'         - Saving queries for an annotation?
`bookmarkp-use-region-flag'        - Activate saved region when visit?
`bookmarkp-su-or-sudo-regexp'      - Bounce-show each end of region?")
 
;;(@* "Faces (Customizable)")
;;; Faces (Customizable) ---------------------------------------------

(defface bookmarkp-gnus
    '((((background dark)) (:foreground "Magenta"))
      (t (:foreground "DarkBlue")))
  "*Face used for a gnus bookmark."
  :group 'bookmarkp)

(defface bookmarkp-info
    '((((background dark)) (:foreground "Green"))
      (t (:foreground "DarkRed")))
  "*Face used for a bookmarked Info node."
  :group 'bookmarkp)

(defface bookmarkp-local-directory
    '((((background dark)) (:foreground "LightGray" :background "DarkRed"))
      (t (:foreground "DarkBlue" :background "HoneyDew2")))
  "*Face used for a bookmarked local directory."
  :group 'bookmarkp)

(defface bookmarkp-local-file-with-region
    '((((background dark)) (:foreground "Indianred2"))
      (t (:foreground "Blue")))
  "*Face used for a region bookmark in a local file."
  :group 'bookmarkp)

(defface bookmarkp-local-file-without-region
    '((((background dark)) (:foreground "Blue"))
      (t (:foreground "Black")))
  "*Face used for a bookmarked local file (without a region)."
  :group 'bookmarkp)

(defface bookmarkp-buffer
    '((((background dark)) (:foreground "green"))
      (t (:foreground "DarkGreen")))
  "*Face used for a bookmarked existing buffer not associated with a file."
  :group 'bookmarkp)

(defface bookmarkp-non-file
    '((((background dark)) (:foreground "gray40"))
      (t (:foreground "gray60")))
  "*Face used for a bookmark not associated with an existing file or buffer."
  :group 'bookmarkp)

(defface bookmarkp-remote-file
    '((((background dark)) (:foreground "pink"))
      (t (:foreground "DarkViolet")))
  "*Face used for a bookmarked tramp remote file (/ssh:)."
  :group 'bookmarkp)

(defface bookmarkp-su-or-sudo
    '((t (:foreground "Red")))
  "*Face used for a bookmarked tramp file (/su: or /sudo:)."
  :group 'bookmarkp)
 
(defface bookmarkp-w3m
    '((((background dark)) (:foreground "yellow"))
      (t (:foreground "DarkMagenta")))
  "*Face used for a bookmarked w3m url."
  :group 'bookmarkp)

(defface bookmarkp-bad-bookmark
    '((t (:foreground "Red" :background "Chartreuse1")))
  "*Face used for a bookmark that seems to be bad: e.g., nonexistent file.
Also used for `D' (deletion) flags."
  :group 'bookmarkp)

(when (< emacs-major-version 22)
  (defface bookmark-menu-heading
      '((t (:foreground "ForestGreen")))
    "Face used to highlight the heading in bookmark menu buffers."
    :group 'bookmarkp :version "22.1"))
  
;;(@* "User Options (Customizable)")
;;; User Options (Customizable) --------------------------------------

(defgroup bookmark-plus nil
  "Bookmark enhancements."
  :prefix "bookmarkp-" :group 'bookmark
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
bookmark+.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "http://www.emacswiki.org/bookmark+.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/BookmarkPlus")
  :link '(emacs-commentary-link :tag "Commentary" "bookmark+"))

(defcustom bookmarkp-use-region-flag t
  "*Non-nil means visiting a bookmark activates its recorded region."
  :type 'boolean :group 'bookmarkp)

(defcustom bookmarkp-region-search-size 40
  "*Same as `bookmark-search-size', but specialized for bookmark regions."
  :type 'integer :group 'bookmarkp)

(defcustom bookmarkp-save-new-location-flag t
  "*Non-nil means save relocated bookmarks.
If nil, then the new bookmark location is visited, but it is not saved
as part of the bookmark definition."
  :type 'boolean :group 'bookmarkp)

(defcustom bookmarkp-handle-region-function 'bookmarkp-handle-region-default
  "*Function to handle a bookmarked region."
  :type 'function :group 'bookmarkp)

(defcustom bookmarkp-su-or-sudo-regexp "\\(/su:\\|/sudo:\\)"
  "*Regexp to recognize `su' or `sudo' Tramp bookmarks."
  :type 'regexp :group 'bookmarkp)

(defcustom bookmarkp-w3m-allow-multi-tabs t
  "*Non-nil means jump to W3M bookmarks in a new session."
  :type 'boolean :group 'bookmarkp)

(defcustom bookmarkp-show-end-of-region t
  "*Show end of region with `exchange-point-and-mark' when activating a region.
If nil show only beginning of region."
  :type 'boolean :group 'bookmarkp)

(defcustom bookmarkp-bookmark-name-length-max 70
  "*Max number of chars for default name for a bookmark with a region."
  :type 'integer :group 'bookmarkp)

(defcustom bookmarkp-incremental-filter-delay 0.6
  "*Seconds to wait before updating display when filtering bookmarks."
  :type 'integer :group 'bookmarkp)

(defcustom bookmarkp-sort-comparer '((bookmarkp-info-cp bookmarkp-gnus-cp
                                      bookmarkp-w3m-cp bookmarkp-local-file-type-cp)
                                     bookmarkp-alpha-p) ; This corresponds to `s k'.
  ;; $$$$$$ An alternative default value: `bookmarkp-alpha-p', which corresponds to `s n'.
  "*Predicate or predicates for sorting (comparing) bookmarks.
This defines the default sort for bookmarks in the menu list.

Various sorting commands, such as \\<bookmark-bmenu-mode-map>\
`\\[bookmarkp-bmenu-sort-by-bookmark-visit-frequency]', change the value of this
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
 ((p1) p2-plain) where p2-plain is (bookmarkp-make-plain-predicate p2)

Likewise, these three values generally act equivalently (*):

 ((p1))
 (() p1-plain)
 p1-plain        where p1-plain is (bookmarkp-make-plain-predicate p1)

The PRED form lets you easily combine predicates: use `p1' unless it
cannot decide, in which case try `p2', and so on.  The value ((p2 p1))
tries the predicates in the opposite order: first `p2', then `p1' if
`p2' returns nil.

Using a single predicate or FINAL-PRED makes it easy to reuse an
existing predicate that returns nil or non-nil.

You can also convert a PRED-type predicate (which returns (t), (nil),
or nil) into an ordinary predicate, by using function
`bookmarkp-make-plain-predicate'.  That lets you reuse elsewhere, as
ordinary predicates, any PRED-type predicates you define.

For example, this defines a plain predicate to compare bookmark names:
 (defalias 'bookmarkp-alpha-p
           (bookmarkp-make-plain-predicate 'bookmarkp-alpha-cp))

Note: As a convention, predefined Bookmark+ PRED-type predicate names
have the suffix `-cp' (for \"component predicate\") instead of `-p'.

--
* If you use `\\[bookmarkp-reverse-multi-sort-order]', then there is a difference in \
behavior between

   (a) using a plain predicate as FINAL-PRED and
   (b) using the analogous PRED-type predicate (and no FINAL-PRED).

  In the latter case, `\\[bookmarkp-reverse-multi-sort-order]' affects when the predicate \
is tried and
  its return value.  See `bookmarkp-reverse-multi-sort-order'."
  :type '(choice
          (const :tag "None (do not sort)" nil)
          (function :tag "Sorting Predicate")
          (list :tag "Sorting Multi-Predicate"
           (repeat (function :tag "Component Predicate"))
           (choice
            (const :tag "None" nil)
            (function :tag "Final Predicate"))))
  :group 'bookmarkp)

(when (> emacs-major-version 20)
  (defcustom bookmarkp-sort-orders-alist ()
    "*Alist of all available sort functions.
This is a pseudo option - you probably do NOT want to customize this.
Instead:

 * To add a new sort function to this list, use macro
   `bookmarkp-define-sort-command'.  It defines a new sort function
   and automatically adds it to this list.

 * To have fewer sort orders available for cycling by \\<bookmark-bmenu-mode-map>\
`\\[bookmarkp-bmenu-change-sort-order-repeat]'...,
   customize option `bookmarkp-sort-orders-for-cycling-alist'.

Each alist element has the form (SORT-ORDER . COMPARER):

 SORT-ORDER is a short string or symbol describing the sort order.
 Examples: \"by last access time\", \"by bookmark name\".

 COMPARER compares two bookmarks.  It must be acceptable as a value of
 `bookmarkp-sort-comparer'."
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
    :group 'bookmarkp))

(unless (> emacs-major-version 20)      ; Emacs 20: custom type `alist' doesn't exist.
  (defcustom bookmarkp-sort-orders-alist ()
    "*Alist of all available sort functions.
This is a pseudo option - you probably do NOT want to customize this.
Instead:

 * To add a new sort function to this list, use macro
   `bookmarkp-define-sort-command'.  It defines a new sort function
   and automatically adds it to this list.

 * To have fewer sort orders available for cycling by \\<bookmark-bmenu-mode-map>\
`\\[bookmarkp-bmenu-change-sort-order-repeat]'...,
   customize option `bookmarkp-sort-orders-for-cycling-alist'.

Each alist element has the form (SORT-ORDER . COMPARER):

 SORT-ORDER is a short string or symbol describing the sort order.
 Examples: \"by last access time\", \"by bookmark name\".

 COMPARER compares two bookmarks.  It must be acceptable as a value of
 `bookmarkp-sort-comparer'."
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
    :group 'bookmarkp))
 
;;(@* "Internal Variables")
;;; Internal Variables --------------------------------------------------

(defconst bookmarkp-non-file-filename "   - no file -"
  "Name to use for `filename' entry, for non-file bookmarks.")

(defvar bookmarkp-jump-display-function nil
  "Function used currently to display a bookmark.")

(defvar bookmarkp-reverse-sort-p nil
  "Non-nil means the sort direction is reversed.")

(defvar bookmarkp-reverse-multi-sort-p nil
  "Non-nil means the truth values returned by predicates are complemented.
This changes the order of the sorting groups, but it does not in
general reverse that order.  The order within each group is unchanged
\(not reversed).")

(defvar bookmarkp-latest-bookmark-alist ()
  "Copy of `bookmark-alist' as last filtered.")

(defvar bookmarkp-latest-sorted-alist ()
  "Copy of `bookmark-alist' as last sorted.")

(defvar bookmarkp-bmenu-marked-bookmarks ()
  "Names of the marked bookmarks.")

(defvar bookmarkp-bmenu-before-hide-unmarked-alist ()
  "Copy of `bookmark-alist' made before hiding unmarked bookmarks.")

(defvar bookmarkp-bmenu-before-hide-marked-alist ()
  "Copy of `bookmark-alist' made before hiding marked bookmarks.")

(defvar bookmarkp-bmenu-filter-function  nil
  "Latest filtering function for `*Bookmark List*' display.")

(defvar bookmarkp-bmenu-title ""
  "Latest title for `*Bookmark List*' display.")

(defvar bookmarkp-bmenu-filter-pattern ""
  "Regexp for incremental filtering.")

(defvar bookmarkp-bmenu-filter-prompt "Pattern: "
  "Prompt for `bookmarkp-bmenu-filter-incrementally'.")

(defvar bookmarkp-bmenu-filter-timer nil
  "Timer used for incremental filtering.")
 
(defvar bookmarkp-tag-history ()
  "History of tags read from the user.")


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

2. Bookmarks can be tagged by users.  The tag information is recorded
using entry `tags':

 (tags . TAGS-LIST)

 TAGS-LIST is a list of strings, the tags.

3. If no file is associated with the bookmark, then FILENAME is
   `   - no file -'.

4. The following additional entries are used to record region
information.  The values are non-nil only when a region is bookmarked.
When a region is bookmarked, POS represents the region start position.

 (buffer-name . BUFFER-NAME)
 (end-position . END-POS)
 (front-context-region-string . STR-BEFORE-END-POS)
 (rear-context-region-string . STR-AFTER-END-POS))

 BUFFER-NAME is the name of a bookmarked buffer, which might not be
  associated with any file (see #1).
 END-POS is the region end position.
 STR-BEFORE-END-POS is buffer text that precedes END-POS.
 STR-AFTER-END-POS is buffer text that follows END-POS.

 NOTE: The relative locations of `front-context-region-string' and
 `rear-context-region-string' are reversed from those of
 `front-context-string' and `rear-context-string'.  For example,
 `front-context-string' is the text that *follows* `position', but
 `front-context-region-string' that *precedes* `end-position'.

5. The following additional entries are used for a Dired bookmark.

 (dired-marked . MARKED-FILES)
 (dired-switches . SWITCHES)

 MARKED-FILES is the list of files that were marked.
 SWITCHES is the string of `dired-listing-switches'.

6. The following additional entries are used for a Gnus bookmark.

 (group . GNUS-GROUP-NAME)
 (article . GNUS-ARTICLE-NUMBER)
 (message-id . GNUS-MESSAGE-ID)

 GNUS-GROUP-NAME is the name of a Gnus group.
 GNUS-ARTICLE-NUMBER is the number of a Gnus article.
 GNUS-MESSAGE-ID is the identifier of a Gnus message.

7. For a W3M bookmark, FILENAME is a W3M URL.")
 
;;(@* "Compatibility Code for Older Emacs Versions")
;;; Compatibility Code for Older Emacs Versions ----------------------

;;;###autoload
(when (< emacs-major-version 23)

  ;; These definitions are for Emacs versions prior to Emacs 23.
  ;; They are the same as the vanilla Emacs 23+ definitions, except as noted.
  ;; They let older versions of Emacs handle bookmarks created with Emacs 23.

  ;; 1. Handle shouldn't-happen case of non-string, non-cons.
  ;; 2. Document NOERROR in doc string.
  (defun bookmark-get-bookmark (bookmark &optional noerror)
    "Return the bookmark record corresponding to BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
If BOOKMARK is already a bookmark record, just return it.
Otherwise look for the corresponding bookmark in `bookmark-alist'.

Non-nil optional arg NOERROR means do not raise an error if BOOKMARK
is not a valid bookmark."
    (cond ((consp bookmark) bookmark)
          ((stringp bookmark)
           (or (if (fboundp 'assoc-string)  ; Emacs 22+.
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
           (buf
            (save-window-excursion      ; VANILLA EMACS FIXME: doesn't work with frames!
              (Info-find-node file info-node) (current-buffer))))
      ;; Use `bookmark-default-handler' to move to appropriate location within Info node.
      (bookmark-default-handler
       `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

  (add-hook 'Info-mode-hook (lambda ()
                              (set (make-local-variable 'bookmark-make-record-function)
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
      (if (and (not no-overwrite)
               (condition-case nil
                   (bookmark-get-bookmark stripped-name)
                 (error nil)))
          ;; Existing bookmark under that name and no prefix arg means just overwrite old.
          ;; Use the new (NAME . ALIST) format.
          (setcdr (bookmark-get-bookmark stripped-name) alist)
        (push (cons stripped-name alist) bookmark-alist))
      (bookmarkp-maybe-save-bookmarks)
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
  (defun bookmarkp-menu-jump-other-window (event)
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
;; Binds `icicle-delete-candidate-object' to `bookmark-delete'.
;;
(defun bookmark-completing-read (prompt &optional default)
  "Read a bookmark name, with completion, prompting with PROMPT.
PROMPT is automatically suffixed with \": \", so do not include that.
Optional second arg DEFAULT is a string to return if the user enters
the empty string.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (bookmark-maybe-load-default-file)
  (if (listp last-nonmenu-event)
      (bookmark-menu-popup-paned-menu
       t prompt
       (if bookmarkp-sort-comparer      ; Test whether to sort, but always use `string-lessp'.
           (sort (bookmark-all-names) 'string-lessp)
         (bookmark-all-names)))
    (let* ((icicle-delete-candidate-object  'bookmark-delete) ; For `S-delete'.
           (completion-ignore-case          bookmark-completion-ignore-case)
           (default                         default)
           (prompt                          (if default
                                                (concat prompt (format " (%s): " default))
                                              (concat prompt ": ")))
           (str                             (completing-read prompt bookmark-alist nil 0 nil
                                                             'bookmark-history)))
      (if (string-equal "" str) default str))))

;;;###autoload
(if (> emacs-major-version 21)
    (define-key-after menu-bar-bookmark-map [jump-other]
      '("Jump to Bookmark (Other Window)" . bookmark-jump-other-window) 'jump)
  (define-key-after menu-bar-bookmark-map [jump-other]
    '("Jump to Bookmark (Other Window)" . bookmarkp-menu-jump-other-window) 'jump))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Handles also regions and non-file buffers.
;;
(defun bookmark-make-record-default (&optional point-only)
  "Return the record describing the location of a new bookmark.
Point must be where the bookmark is to be set.
If POINT-ONLY is non-nil, return only the subset of the record that
pertains to the location within the buffer."
  (let* ((isregion  (and transient-mark-mode mark-active (not (eq (mark) (point)))))
         (isdired   (car (rassq (current-buffer) dired-buffers)))
         (beg       (if isregion (region-beginning) (point)))
         (end       (if isregion (region-end) (point)))
         (buf       (buffer-name))
         (fcs       (if isregion
                        (bookmarkp-position-post-context-region beg end)
                      (bookmarkp-position-post-context beg)))
         (rcs       (if isregion
                        (bookmarkp-position-pre-context-region beg)
                      (bookmarkp-position-pre-context beg)))
         (fcrs      (when isregion (bookmarkp-end-position-pre-context beg end)))
         (ecrs      (when isregion (bookmarkp-end-position-post-context end)))
         (ctime     (current-time)))
    `(,@(unless point-only `((filename . ,(cond ((buffer-file-name (current-buffer))
                                                 (bookmark-buffer-file-name))
                                                (isdired)
                                                (t  bookmarkp-non-file-filename)))))
      (buffer-name . ,buf)
      (front-context-string . ,fcs)
      (rear-context-string . ,rcs)
      (front-context-region-string . ,fcrs)
      (rear-context-region-string . ,ecrs)
      (visits . 0)
      (time . ,ctime)
      (position . ,beg)
      (end-position . ,end))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Use `bookmark-make-record'.
;; 2. Use special default prompts for active region, W3M, and Gnus.
;;
(defun bookmark-set (&optional name parg) ; `C-x r m'
  "Set a bookmark named NAME.
If the region is active (`transient-mark-mode') and nonempty, then
record the region limits in the bookmark.

If NAME is nil, then prompt for the bookmark name.  The default name
for prompting is as follows (in order of priority):

 * If the region is active and nonempty, then the buffer name followed
   by \": \" and the region prefix (up to
   `bookmarkp-bookmark-name-length-max' chars).

 * If in W3M mode, then the current W3M title.

 * If in Gnus Summary mode, then the Gnus summary article header.

 * Otherwise, the current buffer name.

While entering a bookmark name at the prompt:

 * You can use `C-w' to yank words from the buffer to the minibuffer.
   Repeating `C-w' yanks successive words.

 * You can use `C-u' to insert the name of the last bookmark used in
   the buffer.  This can be useful as an aid to track your progress
   through a large file.  (If no bookmark has yet been used, then
   `C-u' inserts the name of the visited file.)

With a prefix argument, do not overwrite a bookmark that has the same
name as NAME, if such a bookmark already exists.  Instead, push the
new bookmark onto the bookmark alist.

The most recently set bookmark named NAME is thus the one in effect at
any given time, but any others named NAME are still available, should
you decide to delete the most recent one.

Use `\\[bookmark-delete]' to remove bookmarks (you give it a name, and it removes
only the first instance of a bookmark with that name from the list of
bookmarks.)"
  (interactive (list nil current-prefix-arg))
  (bookmark-maybe-load-default-file)
  (setq bookmark-current-point   (point)
        bookmark-current-buffer  (current-buffer))
  (save-excursion (skip-chars-forward " ") (setq bookmark-yank-point  (point)))
  (let* ((record   (bookmark-make-record))
         (regionp  (and transient-mark-mode mark-active (not (eq (mark) (point)))))
         (regname  (concat (buffer-name) ": "
                           (buffer-substring (if regionp (region-beginning) (point))
                                             (if regionp
                                                 (region-end)
                                               (save-excursion (end-of-line) (point))))))
         (defname  (bookmarkp-replace-regexp-in-string
                    "\n" " "
                    (cond (regionp
                           (save-excursion
                             (goto-char (region-beginning))
                             (skip-chars-forward " ") (setq bookmark-yank-point  (point)))
                           (substring regname 0 (min bookmarkp-bookmark-name-length-max
                                                     (length regname))))
                          ((eq major-mode 'w3m-mode) w3m-current-title)
                          ((eq major-mode 'gnus-summary-mode)
                           (elt (gnus-summary-article-header) 1))
                          (t (car record)))))
         (doc-cmd  "`\\<minibuffer-local-map>\\[next-history-element]' \ for default")
         (bname    (or name
                       (read-from-minibuffer
                        (format "Set bookmark (%s): "
                                (if (> emacs-major-version 21)
                                    (substitute-command-keys doc-cmd)
                                  defname))
                        nil
                        (let ((map  (copy-keymap minibuffer-local-map)))
                          (define-key map "\C-w" 'bookmark-yank-word)
                          (define-key map "\C-u" 'bookmark-insert-current-bookmark)
                          map)
                        nil nil defname))))
    (when (string-equal bname "") (setq bname  defname))
    (bookmark-store bname (cdr record) parg)
    (if bookmark-use-annotations
        (bookmark-edit-annotation bname)
      (goto-char bookmark-current-point))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Prevent adding a newline in a bookmark name when yanking.
;; 
;;;###autoload
(defun bookmark-yank-word ()            ; `C-w' in minibuffer when setting bookmark.
  "Yank the word at point in `bookmark-current-buffer'.
Repeat to yank subsequent words from the buffer, appending them.
Newline characters are stripped out."
  (interactive)
  (let ((string  (with-current-buffer bookmark-current-buffer
                   (goto-char bookmark-yank-point)
                   (buffer-substring-no-properties
                    (point)
                    (progn (forward-word 1) (setq bookmark-yank-point  (point)))))))
    (setq string  (bookmarkp-replace-regexp-in-string "\n" "" string))
    (insert string)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Add bookmark to `bookmarkp-bmenu-marked-bookmarks',
;; 2. Do not call `bookmark-bmenu-check-position'.
;; 3. Raise error if not in `*Bookmark List*'.
;;
;;;###autoload
(defun bookmark-bmenu-mark ()           ; `m' in menu list
  "Mark the bookmark on this line, using mark `>'."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (beginning-of-line)
  (let ((inhibit-read-only  t))
    (push (bookmark-bmenu-bookmark) bookmarkp-bmenu-marked-bookmarks)
    (delete-char 1) (insert ?>)
    (forward-line 1)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Remove bookmark from `bookmarkp-bmenu-marked-bookmarks'.
;; 2. Do not call `bookmark-bmenu-check-position'.
;; 3. Raise error if not in `*Bookmark List*'.
;;
;;;###autoload
(defun bookmark-bmenu-unmark (&optional backup) ; `u' in menu list
  "Unmark the bookmark on this line, then move down to the next.
Optional BACKUP means move up instead."
  (interactive "P")
  (bookmarkp-barf-if-not-in-menu-list)
  (beginning-of-line)
  (let ((inhibit-read-only  t))
    (delete-char 1) (insert " ")
    (setq bookmarkp-bmenu-marked-bookmarks  (delete (bookmark-bmenu-bookmark)
                                                    bookmarkp-bmenu-marked-bookmarks)))
  (forward-line (if backup -1 1)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Save DISPLAY-FUNCTION to `bookmarkp-jump-display-function' before calling
;; `bookmark-handle-bookmark'.
;;
(defun bookmark--jump-via (bookmark display-function)
  "Helper function for `bookmark-jump(-other-window)'.
BOOKMARK is a bookmark name or a bookmark record.
DISPLAY-FUNCTION is the function that displays the bookmark."
  (bookmarkp-record-visit bookmark)
  (setq bookmarkp-jump-display-function  display-function)
  (bookmark-handle-bookmark bookmark)
  (let ((win  (get-buffer-window (current-buffer) 0)))
    (when win (set-window-point win (point))))
  ;; VANILLA EMACS FIXME: we used to only run `bookmark-after-jump-hook' in
  ;; `bookmark-jump' itself, but in none of the other commands.
  (run-hooks 'bookmark-after-jump-hook)
  (when bookmark-automatically-show-annotations (bookmark-show-annotation bookmark)))


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
;; 2. Added note about Icicles `S-delete' to doc string.
;; 3. Use `bookmarkp-default-bookmark-name' as default when interactive.
;;
;;;###autoload
(defun bookmark-jump (bookmark-name &optional use-region-p) ; `C-x r b', `C-x p g'
  "Jump to the bookmark named BOOKMARK-NAME.
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See function `bookmark-load' for more about this.

If the file pointed to by BOOKMARK-NAME no longer exists, you are
asked if you wish to give the bookmark a new location.  If so,
`bookmark-jump' jumps to the new location and saves it.

If the bookmark represents a region, then the region is activated if
`bookmarkp-use-region-flag' is not-nil or it is nil and you use a
prefix argument.  A prefix arg temporarily flips the value of
`bookmarkp-use-region-flag'.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Jump to bookmark"
                                               (bookmarkp-default-bookmark-name))
                     current-prefix-arg))
  (unless bookmark-name (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark-name)
  (let ((bookmarkp-use-region-flag  (if use-region-p
                                        (not bookmarkp-use-region-flag)
                                      bookmarkp-use-region-flag)))
    (bookmark--jump-via bookmark-name 'switch-to-buffer)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added optional arg USE-REGION-P.
;; 2. Use `bookmarkp-default-bookmark-name' as default when interactive.
;;
;;;###autoload
(defun bookmark-jump-other-window (bookmark-name &optional use-region-p) ; `C-x p o'
  "Jump to the bookmark named BOOKMARK-NAME, in another window.
See `bookmark-jump'."
  (interactive (list (bookmark-completing-read "Jump to bookmark (in another window)"
                                               (bookmarkp-default-bookmark-name))
                     current-prefix-arg))
  (unless bookmark-name (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark-name)
  (let ((bookmarkp-use-region-flag  (if use-region-p
                                        (not bookmarkp-use-region-flag)
                                      bookmarkp-use-region-flag)))
    (bookmark--jump-via bookmark-name 'switch-to-buffer-other-window)))


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
       (let ((file  (bookmark-get-filename bookmark)))
         (when file                     ; Don't know how to relocate if file doesn't exist.
           (unless (string= file bookmarkp-non-file-filename)
             (setq file  (expand-file-name file)))
           (ding)
           (cond ((y-or-n-p (if (and (string= file bookmarkp-non-file-filename)
                                     (bookmarkp-get-buffer-name bookmark))
                                "Bookmark's buffer doesn't exist.  Re-create it? "
                              (concat (file-name-nondirectory file)
                                      " nonexistent.  Relocate \"" bookmark "\"? ")))
                  (if (string= file bookmarkp-non-file-filename)
                      ;; This is not likely the right way to get the correct buffer, but it's
                      ;; better than nothing, and it gives the user a chance to DTRT.
                      (pop-to-buffer (bookmarkp-get-buffer-name bookmark)) ; Create buffer.
                    (bookmark-relocate bookmark)) ; Relocate to file.
                  (funcall (or (bookmark-get-handler bookmark) ; Try again
                               'bookmark-default-handler)
                           (bookmark-get-bookmark bookmark)))
                 (t
                  (message "Bookmark not relocated \(%s\)" bookmark)
                  (signal (car err) (cdr err)))))))))
  (when (stringp bookmark) (setq bookmark-current-bookmark  bookmark))
  nil)                                  ; Return nil if no error.


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Support regions and buffer names.
;;
(defun bookmark-default-handler (bookmark)
  "Default handler to jump to the location of BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
If BOOKMARK records a nonempty region, and `bookmarkp-use-region-flag'
 is non-nil, then activate the region.
Return nil or signal `file-error'."
  (let* ((bmk            (bookmark-get-bookmark bookmark))
         (file           (bookmark-get-filename bmk))
         (buf            (bookmark-prop-get bmk 'buffer))
         (bufname        (bookmarkp-get-buffer-name bmk))
         (pos            (bookmark-get-position bmk))
         (end-pos        (bookmarkp-get-end-position bmk))
         (old-info-node  (and (not (bookmark-get-handler bookmark))
                              (bookmark-prop-get bmk 'info-node))))
    (if old-info-node                   ; Emacs 20-21 Info bookmarks - no handler entry.
        (progn (require 'info) (Info-find-node file old-info-node) (goto-char pos))
      (if (not (and bookmarkp-use-region-flag end-pos (/= pos end-pos)))
          ;; Single-position bookmark (no region).  Go to it.
          (bookmarkp-goto-position file buf bufname pos
                                   (bookmark-get-front-context-string bmk)
                                   (bookmark-get-rear-context-string bmk))
        ;; Bookmark with a region.  Go to it and activate the region.
        (if (and file (file-readable-p file) (not (buffer-live-p buf)))
            (with-current-buffer (find-file-noselect file) (setq buf  (buffer-name)))
          ;; No file found.  If no buffer either, then signal that file doesn't exist.
          (unless (or (and buf (get-buffer buf))
                      (and bufname (get-buffer bufname) (not (string= buf bufname))))
            (signal 'file-error `("Jumping to bookmark" "No such file or directory"
                                  (bookmark-get-filename bmk)))))
        (set-buffer (or buf bufname))
        (save-current-buffer (funcall bookmarkp-jump-display-function (current-buffer)))
        (raise-frame)
        (goto-char (min pos (point-max)))
        (when (> pos (point-max)) (error "Bookmark position is beyond buffer end"))
        ;; Activate region.  Relocate it if it moved.  Save relocated bookmark if confirm.
        (funcall bookmarkp-handle-region-function bmk)))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added note about `S-delete' to doc string.
;; 2. Changed arg name: BOOKMARK -> BOOKMARK-NAME.
;;
(or (fboundp 'old-bookmark-relocate)
(fset 'old-bookmark-relocate (symbol-function 'bookmark-relocate)))

;;;###autoload
(defun bookmark-relocate (bookmark-name)
  "Relocate the bookmark named BOOKMARK-NAME to another file.
You are prompted for the new file name.
Changes the file associated with the bookmark.
Useful when a file has been renamed after a bookmark was set in it.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Bookmark to relocate")))
  (old-bookmark-relocate bookmark-name))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Do not add any text properties here.  That's done in `bookmarkp-bmenu-propertize-item'.
;; 2. Added note about `S-delete' to doc string.
;; 3. Changed arg name: BOOKMARK -> BOOKMARK-NAME.
;;
;;;###autoload
(defun bookmark-insert-location (bookmark-name &optional no-history) ; `C-x p f'
  "Insert file or buffer name for the bookmark named BOOKMARK-NAME.
If a file is bookmarked, insert the recorded file name.
If a non-file buffer is bookmarked, insert the recorded buffer name.

Optional second arg NO-HISTORY means do not record this in the
minibuffer history list `bookmark-history'.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (if (> emacs-major-version 21)
                   (list (bookmark-completing-read "Insert bookmark location"))
                 (bookmark-completing-read "Insert bookmark location")))
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
  (or (bookmark-get-filename bookmark)
      (bookmarkp-get-buffer-name bookmark)
      (bookmark-prop-get bookmark 'buffer)
      ""))
      ;; $$$$$$ (error "Bookmark has no file or buffer name: %S" bookmark)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added note about `S-delete' to doc string.
;; 2. Added BATCH arg.
;;
;;;###autoload
(defun bookmark-rename (old &optional new batch) ; `C-x p r'
  "Change bookmark's name from OLD to NEW.
Interactively:
 If called from the keyboard, then prompt for OLD.
 If called from the menubar, select OLD from a menu.
If NEW is nil, then prompt for its string value.

If BATCH is non-nil, then do not rebuild the menu list.

While the user enters the new name, repeated `C-w' inserts consecutive
words from the buffer into the new bookmark name.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Old bookmark name")))
  (bookmark-maybe-historicize-string old)
  (bookmark-maybe-load-default-file)
  (setq bookmark-current-point  (point))
  (save-excursion (skip-chars-forward " ") (setq bookmark-yank-point  (point)))
  (setq bookmark-current-buffer  (current-buffer))
  (let ((newname  (or new  (read-from-minibuffer
                            "New name: " nil
                            (let ((now-map  (copy-keymap minibuffer-local-map)))
                              (define-key now-map  "\C-w" 'bookmark-yank-word)
                              now-map)
                            nil 'bookmark-history))))
    (bookmark-set-name old newname)
    (setq bookmark-current-bookmark  newname)
    (unless batch (bookmark-bmenu-surreptitiously-rebuild-list))
    (bookmarkp-maybe-save-bookmarks)
    newname))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added note about `S-delete' to doc string.
;; 2. Changed arg name: BOOKMARK -> BOOKMARK-NAME.
;;
(or (fboundp 'old-bookmark-insert)
(fset 'old-bookmark-insert (symbol-function 'bookmark-insert)))

;;;###autoload
(defun bookmark-insert (bookmark-name)  ; `C-x p i'
  "Insert the text of a bookmarked file.
BOOKMARK-NAME is the name of the bookmark.
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See function `bookmark-load' for more about this.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Insert bookmark contents")))
  (old-bookmark-insert bookmark-name))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added note about `S-delete' to doc string.
;; 2. Changed arg name: BOOKMARK -> BOOKMARK-NAME.
;; 3. Use `bookmarkp-default-bookmark-name' as default when interactive.
;; 4. Increment `bookmark-alist-modification-count' even when using `batch' arg.
;;
;;;###autoload
(defun bookmark-delete (bookmark-name &optional batch) ; `C-x p d'
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
   (list (bookmark-completing-read "Delete bookmark" (bookmarkp-default-bookmark-name))))
  (bookmark-maybe-historicize-string bookmark-name)
  (bookmark-maybe-load-default-file)
  (let ((will-go  (bookmark-get-bookmark bookmark-name 'noerror)))
    (setq bookmark-alist  (delq will-go bookmark-alist))
    ;; Added by DB.  `bookmark-current-bookmark' is nil if last occurrence was deleted.
    (unless (bookmark-get-bookmark bookmark-current-bookmark 'noerror)
      (setq bookmark-current-bookmark  nil)))
  ;; Don't rebuild the list when using `batch' arg
  (unless batch (bookmark-bmenu-surreptitiously-rebuild-list))
  (bookmarkp-maybe-save-bookmarks))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Insert code piecewise, to improve performance when saving `bookmark-alist'.
;;    (Do not let `pp' parse all of `bookmark-alist' at once.)
;; 2. Remove any text properties from bookmark title.
;;
(defun bookmark-write-file (file)
  "Write `bookmark-alist' to `bookmark-default-file'."
  (bookmark-maybe-message "Saving bookmarks to file `%s'..." file)
  (with-current-buffer (get-buffer-create " *Bookmarks*")
    (goto-char (point-min))
    (delete-region (point-min) (point-max))
    (let ((print-length  nil)
          (print-level   nil))
      (bookmark-insert-file-format-version-stamp)
      (progn (insert "(")
             (dolist (ii  bookmark-alist)
               (let ((str  (car ii)))
                 (set-text-properties 0 (length str) () str)
                 (setcar ii str)
                 (pp ii (current-buffer))))
             (insert ")"))
      (let ((version-control  (cond ((null bookmark-version-control) nil)
                                    ((eq 'never bookmark-version-control) 'never)
                                    ((eq 'nospecial bookmark-version-control) version-control)
                                    (t t))))
        (condition-case nil
            (write-region (point-min) (point-max) file)
          (file-error (message "Cannot write file `%s'" file)))
        (kill-buffer (current-buffer))
        (bookmark-maybe-message "Saving bookmarks to file `%s'...done" file)))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Do not use `bookmark-bmenu-check-position' as a test - it always returns non-nil anyway.
;; 2. Use face `bookmarkp-bad-bookmark' on the `D' flag.
;; 3. Raise error if not in buffer `*Bookmark List*'.
;;
(defun bookmark-bmenu-delete ()         ; `d', `k'
  "Flag this bookmark for deletion, using mark `D'.
Use `\\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-execute-deletions]' to carry out \
the deletions."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (beginning-of-line)
  (bookmark-bmenu-check-position)
  (let ((inhibit-read-only  t))
    (delete-char 1)
    (insert ?D)
    (put-text-property (1- (point)) (point) 'face 'bookmarkp-bad-bookmark)
    (forward-line 1)
    (bookmark-bmenu-check-position)))
 
;;(@* "Menu List Replacements (`bookmark-bmenu-*')")
;;; Menu List Replacements (`bookmark-bmenu-*') ----------------------


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Rebuild `bookmark-alist' using the last filtered alist in use.
;; 2. Update the menu-list title.
;;
(defun bookmark-bmenu-surreptitiously-rebuild-list ()
  "Rebuild the bookmarks list, if it exists.
Optional arg DONT-TOGGLE-FILENAMES-P is passed to
`bookmark-bmenu-list'."
  (when (get-buffer "*Bookmark List*")
    (let ((bookmark-alist  bookmarkp-latest-bookmark-alist))
      (bookmark-bmenu-list 'filteredp))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added args TITLE, FILTEREDP, DONT-TOGGLE-FILENAMES-P.
;; 2. Handles also region bookmarks and buffer (non-file) bookmarks.
;; 3. Uses `pop-to-buffer', not `switch-to-buffer', so we respect `special-display-*'
;;    (but keep `one-window-p' if that's the case).
;;
;;;###autoload
(defun bookmark-bmenu-list (&optional filteredp) ; `C-x r l'
  "Display a list of existing bookmarks, in buffer `*Bookmark List*'.
The leftmost column of a bookmark entry shows `D' if the bookmark is
 flagged for deletion, or `>' if it is marked normally.
The second column shows `*' if the bookmark has an annotation.

The following faces are used for the list entries.
Use `customize-face' if you want to change the appearance.

  `bookmarkp-local-directory', `bookmarkp-local-file-without-region',
  `bookmarkp-local-file-with-region', `bookmarkp-gnus',
  `bookmarkp-info', `bookmarkp-non-file', `bookmarkp-remote-file',
  `bookmarkp-su-or-sudo', `bookmarkp-w3m'.

Non-nil FILTEREDP means the menu list has been filtered, so:
 * Use `bookmarkp-bmenu-title' not the default menu-list title.
 * Do not reset `bookmarkp-latest-bookmark-alist' to `bookmark-alist'."
  (interactive)
  (bookmark-maybe-load-default-file)
  (when (or (interactive-p) (not (get-buffer "*Bookmark List*")))
    (setq bookmarkp-bmenu-marked-bookmarks  ()))
  (unless filteredp (setq bookmarkp-latest-bookmark-alist  bookmark-alist))
  (if (interactive-p)
      (let ((one-win-p  (one-window-p)))
        (pop-to-buffer (get-buffer-create "*Bookmark List*"))
        (when one-win-p (delete-other-windows)))
    (set-buffer (get-buffer-create "*Bookmark List*")))
  (let* ((inhibit-read-only  t)
         (real-title         (if filteredp bookmarkp-bmenu-title "% All Bookmarks"))
         (len-title          (- (length real-title) 2)))
    (erase-buffer)
    (insert (format "%s\n- %s\n" real-title (make-string len-title ?-)))
    (add-text-properties (point-min) (point) (bookmarkp-face-prop 'bookmark-menu-heading))
    (let ((max-width     0)
          (sorted-alist  (bookmarkp-sort-and-remove-dups bookmark-alist))
          name annotation markedp start)
      (dolist (bmk  sorted-alist)
        (setq max-width  (max max-width (length (bookmark-name-from-full-record bmk)))))
      (setq max-width  (+ 2 max-width))
      (dolist (bmk  sorted-alist)
        (setq name        (bookmark-name-from-full-record bmk)
              annotation  (bookmark-get-annotation bmk)
              markedp     (bookmarkp-bookmark-marked-p bmk)
              start       (point))
        (insert (cond ((and annotation (not (string-equal annotation "")) markedp)  ">*")
                      ((and annotation (not (string-equal annotation "")))          " *")
                      (markedp                                                      "> ")
                      (t                                                            "  "))
                name)
        (move-to-column max-width t)
        (bookmarkp-bmenu-propertize-item name start (point))
        (insert "\n")))
    (goto-char (point-min))  (forward-line 2)
    (bookmark-bmenu-mode)
    (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-toggle-filenames t))
    (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window)))
  (when (and (interactive-p) bookmarkp-sort-comparer)
    (bookmarkp-msg-about-sort-order (bookmarkp-current-sort-order))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Redefined.  Get name of the current bookmark from `bookmarkp-latest-sorted-alist'.
;;
(defun bookmark-bmenu-bookmark ()
  "Return the name of the bookmark on this line."
  (let ((pos  (- (bookmarkp-line-number-at-pos) 3)))
    (car (nth pos bookmarkp-latest-sorted-alist))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Add text properties when hiding filenames.
;; 2. Do not set or use `bookmark-bmenu-bookmark-column' - use column 2 always.
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
        (goto-char (point-min))
        (forward-line 2)
        (setq bookmark-bmenu-hidden-bookmarks  (nreverse bookmark-bmenu-hidden-bookmarks))
        (let ((max-width  0))
          (dolist (name  bookmark-bmenu-hidden-bookmarks)
            (setq max-width  (max max-width (length name))))
          (setq max-width  (+ 2 max-width))
          (save-excursion
            (let ((inhibit-read-only  t))
              (while bookmark-bmenu-hidden-bookmarks
                (move-to-column 2 t)
                (bookmark-kill-line)
                (let ((name   (car bookmark-bmenu-hidden-bookmarks))
                      (start  (point))
                      end)
                  (insert name)
                  (move-to-column max-width t)
                  (setq end  (point))
                  (bookmarkp-bmenu-propertize-item name start end))
                (setq bookmark-bmenu-hidden-bookmarks  (cdr bookmark-bmenu-hidden-bookmarks))
                (forward-line 1)))))))
    (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window))))


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
        (goto-char (point-min))
        (forward-line 2)
        (setq bookmark-bmenu-hidden-bookmarks  ())
        (let ((inhibit-read-only  t))
          (while (< (point) (point-max))
            (let ((bmk  (bookmark-bmenu-bookmark)))
              (setq bookmark-bmenu-hidden-bookmarks
                    (cons bmk bookmark-bmenu-hidden-bookmarks))
	      (let ((start  (save-excursion (end-of-line) (point))))
		(move-to-column bookmark-bmenu-file-column t))
	      (delete-region (point) (progn (end-of-line) (point)))
              (insert "  ")
              (bookmark-insert-location bmk t) ; Pass the NO-HISTORY arg.
              (when (if (fboundp 'display-color-p) ; Emacs 21+.
                        (and (display-color-p) (display-mouse-p))
                      window-system)
                (let ((help  (get-text-property (+ 2 (line-beginning-position)) 'help-echo)))
                  (put-text-property (+ 2 (line-beginning-position)) (point)
                                     'mouse-face 'highlight)
                  (when help
                    (put-text-property (+ 2 (line-beginning-position)) (point)
                                       'help-echo help))))
              (forward-line 1))))))
    (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Use `pop-to-buffer', not `switch-to-buffer-other-window'.
;; 2. Do not call `bookmark-bmenu-check-position' (done by `bookmark-bmenu-bookmark').
;; 3. Raise error if not in buffer `*Bookmark List*'.
;;
(defun bookmark-bmenu-other-window ()   ; `o' in menu list
  "Select this line's bookmark in other window, leaving bookmark menu visible."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (let ((bookmark  (bookmark-bmenu-bookmark))
        (bookmark-automatically-show-annotations  t)) ; FIXME: needed?
    (bookmark--jump-via bookmark 'pop-to-buffer)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added optional arg MARKEDP: handle bookmarks marked `>', not just those flagged `D'.
;; 2. Use `bookmark-bmenu-surreptitiously-rebuild-list', instead of using
;;    `bookmark-bmenu-list', updating the modification count, and saving.
;; 3. Update `bookmarkp-latest-bookmark-alist' to reflect the deletions.
;; 4. Use `bookmarkp-bmenu-goto-bookmark-named'.
;; 5. Added status messages.
;; 6. Raise error if not in buffer `*Bookmark List*'.
;;
;;;###autoload
(defun bookmark-bmenu-execute-deletions (&optional markedp) ; `x' in menu list
  "Delete (visible) bookmarks flagged `D'.
With a prefix argument, delete the bookmarks marked `>' instead, after
confirmation."
  (interactive "P")
  (bookmarkp-barf-if-not-in-menu-list)
  (if (or (not markedp) (yes-or-no-p "Delete bookmarks marked `>' (not `D') "))
      (let* ((mark-type  (if markedp "^>" "^D"))
             (o-str      (and (not (looking-at mark-type)) (bookmark-bmenu-bookmark)))
             (o-point    (point))
             (count      0))
        (message "Deleting bookmarks...")
        (goto-char (point-min))
        (forward-line 2)
        (while (re-search-forward mark-type (point-max) t)
          (let ((bmk  (bookmark-bmenu-bookmark)))
            (bookmark-delete bmk 'batch)
            (setq count                            (1+ count)
                  bookmarkp-latest-bookmark-alist  (delete
                                                    (assoc bmk bookmarkp-latest-bookmark-alist)
                                                    bookmarkp-latest-bookmark-alist))))
        (if (<= count 0)
            (message (if markedp "No marked bookmarks" "No bookmarks flagged for deletion"))
          (bookmark-bmenu-surreptitiously-rebuild-list)
          (message "Deleted %d bookmarks" count))
        (if o-str
            (bookmarkp-bmenu-goto-bookmark-named o-str)
          (goto-char o-point)
          (beginning-of-line)))
    (message "OK, nothing deleted")))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Do not call `bookmark-bmenu-list' (it was already called).
;; 2. Do not call `bookmark-bmenu-check-position' (done by `bookmark-bmenu-bookmark').
;; 3. Raise error if not in buffer `*Bookmark List*'.
;;
;;;###autoload
(defun bookmark-bmenu-rename ()         ; `r' in menu list
  "Rename bookmark on current line.  Prompts for a new name."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (let ((new-name  (bookmark-rename (bookmark-bmenu-bookmark))))
    (when (or (search-forward new-name (point-max) t) (search-backward new-name (point-min) t))
      (beginning-of-line))))
 
;;(@* "Bookmark+ Functions (`bookmarkp-*')")
;;; Bookmark+ Functions (`bookmarkp-*') ------------------------------

(defun bookmarkp-maybe-save-bookmarks ()
  "Increment save counter and maybe save `bookmark-alist'."
  (setq bookmark-alist-modification-count  (1+ bookmark-alist-modification-count))
  (when (bookmark-time-to-save-p) (bookmark-save)))

(defun bookmarkp-edit-bookmark (bookmark-name)
  "Edit bookmark's name and file name, and maybe save them.
BOOKMARK-NAME is the current (old) name of the bookmark."
  (let* ((bookmark-filename  (bookmark-get-filename bookmark-name))
         (new-bmk-name       (read-from-minibuffer "New bookmark name: " nil nil nil nil
                                                   bookmark-name))
         (new-filename       (read-from-minibuffer "New file name: " nil nil nil nil
                                                   bookmark-filename)))
    (when (and (not (equal new-bmk-name "")) (not (equal new-filename ""))
               (y-or-n-p "Save changes? "))
      (bookmark-rename bookmark-name new-bmk-name 'batch)
      (bookmark-set-filename new-bmk-name new-filename)
      (bookmarkp-maybe-save-bookmarks)
      (list new-bmk-name new-filename))))

(defun bookmarkp-record-visit (bookmark &optional batch)
  "Update the data recording a visit to BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
This increments the `visits' entry and sets the `time' entry to the
current time.  If either an entry is not present, it is added (with 0
value for `visits')."
  (let ((vis  (bookmark-prop-get bookmark 'visits)))
    (if vis
        (bookmark-prop-set bookmark 'visits (1+ vis))
      (bookmark-prop-set bookmark 'visits 0))
    (bookmark-prop-set bookmark 'time (current-time))
    (unless batch
      (bookmark-bmenu-surreptitiously-rebuild-list))
    (let ((bookmark-save-flag  nil))  (bookmarkp-maybe-save-bookmarks))))

(defun bookmarkp-default-bookmark-name ()
  "Default bookmark name.
If in `*Bookmark List*' then the current line's bookmark.
Otherwise, the last-used bookmark in the current buffer."
  (if (equal (buffer-name (current-buffer)) "*Bookmark List*")
      (bookmark-bmenu-bookmark)
    bookmark-current-bookmark))


;;(@* "Menu-List (`*-bmenu-*') Filter Commands")
;;  *** Menu-List (`*-bmenu-*') Filter Commands ***

;;;###autoload
(defun bookmarkp-bmenu-show-only-files (arg) ; `F' in menu list
  "Display a list of file and directory bookmarks (only).
With a prefix argument, do not include remote files or directories."
  (interactive "P")
  (bookmarkp-barf-if-not-in-menu-list)
  (setq bookmarkp-bmenu-filter-function  (if arg
                                             'bookmarkp-local-file-alist-only
                                           'bookmarkp-file-alist-only)
        bookmarkp-bmenu-title            (if arg
                                             "% Local File and Directory Bookmarks"
                                           "% File and Directory Bookmarks"))
  (let ((bookmark-alist  (funcall bookmarkp-bmenu-filter-function)))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p) (bookmarkp-msg-about-sort-order (bookmarkp-current-sort-order)
                                                        "Only file bookmarks are shown")))

;;;###autoload
(defun bookmarkp-bmenu-show-only-non-files () ; `B' in menu list
  "Display (only) the non-file bookmarks."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (setq bookmarkp-bmenu-filter-function  'bookmarkp-non-file-alist-only
        bookmarkp-bmenu-title            "% Non-File Bookmarks")
  (let ((bookmark-alist  (funcall bookmarkp-bmenu-filter-function)))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p) (bookmarkp-msg-about-sort-order (bookmarkp-current-sort-order)
                                                        "Only non-file bookmarks are shown")))
    
;;;###autoload
(defun bookmarkp-bmenu-show-only-info-nodes () ; `I' in menu list
  "Display (only) the Info bookmarks."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (setq bookmarkp-bmenu-filter-function  'bookmarkp-info-alist-only
        bookmarkp-bmenu-title            "% Info Bookmarks")
  (let ((bookmark-alist  (funcall bookmarkp-bmenu-filter-function)))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p) (bookmarkp-msg-about-sort-order (bookmarkp-current-sort-order)
                                                        "Only Info bookmarks are shown")))

;;;###autoload
(defun bookmarkp-bmenu-show-only-dired () ; No key binding, at least for now
  "Display (only) the Dired bookmarks."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (setq bookmarkp-bmenu-filter-function  'bookmarkp-dired-alist-only
        bookmarkp-bmenu-title            "% Dired Bookmarks")
  (let ((bookmark-alist  (funcall bookmarkp-bmenu-filter-function)))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p) (bookmarkp-msg-about-sort-order (bookmarkp-current-sort-order)
                                                        "Only Dired bookmarks are shown")))

;;;###autoload
(defun bookmarkp-bmenu-show-only-w3m-urls () ; `W' in menu list
  "Display (only) the w3m bookmarks."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (setq bookmarkp-bmenu-filter-function  'bookmarkp-w3m-alist-only
        bookmarkp-bmenu-title            "% W3M Bookmarks")
  (let ((bookmark-alist  (funcall bookmarkp-bmenu-filter-function)))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p) (bookmarkp-msg-about-sort-order (bookmarkp-current-sort-order)
                                                        "Only W3M bookmarks are shown")))

;;;###autoload
(defun bookmarkp-bmenu-show-only-gnus () ; `G' in menu list
  "Display (only) the Gnus bookmarks."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (setq bookmarkp-bmenu-filter-function  'bookmarkp-gnus-alist-only
        bookmarkp-bmenu-title            "% Gnus Bookmarks")
  (let ((bookmark-alist  (funcall bookmarkp-bmenu-filter-function)))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p) (bookmarkp-msg-about-sort-order (bookmarkp-current-sort-order)
                                                        "Only Gnus bookmarks are shown")))

;;;###autoload
(defun bookmarkp-bmenu-show-all ()      ; `.' in menu list
  "Show all bookmarks already known to the bookmarks menu list.
This does not revert the menu list, to bring it up to date.
To revert the list, use `\\<bookmark-bmenu-mode-map>\\[bookmarkp-bmenu-refresh-menu-list]'."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (setq bookmarkp-bmenu-filter-function  nil
        bookmarkp-bmenu-title            "% All Bookmarks"
        bookmarkp-latest-bookmark-alist  bookmark-alist)
  (bookmark-bmenu-list)
  (when (interactive-p)
    (bookmarkp-msg-about-sort-order (bookmarkp-current-sort-order) "All bookmarks are shown")))

;;;###autoload
(defun bookmarkp-bmenu-refresh-menu-list () ; `g' in menu list
  "Refresh (revert) bookmarks menu list, bringing it up to date.
This does not change the current filtering or sorting."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (let ((bookmark-alist  (if bookmarkp-bmenu-filter-function
                             (funcall bookmarkp-bmenu-filter-function)
                           bookmark-alist)))
    (bookmark-bmenu-list 'filteredp)))

;;;###autoload
(defun bookmarkp-bmenu-show-only-regions () ; `R' in menu list
  "Display (only) the bookmarks that record a region."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (setq bookmarkp-bmenu-filter-function  'bookmarkp-region-alist-only
        bookmarkp-bmenu-title            "% Region Bookmarks")
  (let ((bookmark-alist  (funcall bookmarkp-bmenu-filter-function)))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp))
  (when (interactive-p)
    (bookmarkp-msg-about-sort-order (bookmarkp-current-sort-order)
                                    "Only bookmarks with regions are shown")))

;;;###autoload
(defun bookmarkp-bmenu-filter-bookmark-name-incrementally () ; `P B' in menu list
  "Incrementally filter bookmarks by bookmark name using a regexp."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (unwind-protect
       (progn (setq bookmarkp-bmenu-filter-timer
                    (run-with-timer 0 bookmarkp-incremental-filter-delay
                                    #'bookmarkp-bmenu-filter-alist-by-bookmark-name-regexp))
              (bookmarkp-bmenu-read-filter-input))
    (bookmarkp-bmenu-cancel-incremental-filtering)))

(defun bookmarkp-bmenu-filter-alist-by-bookmark-name-regexp ()
  "Filter bookmarks by bookmark name, then refresh the menu list."
  (setq bookmarkp-bmenu-filter-function  'bookmarkp-regexp-filtered-bookmark-name-alist-only
        bookmarkp-bmenu-title
        (format "%% Bookmarks with Names Matching Regexp `%s'" bookmarkp-bmenu-filter-pattern))
  (let ((bookmark-alist  (funcall bookmarkp-bmenu-filter-function)))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp)))

;;;###autoload
(defun bookmarkp-bmenu-filter-file-name-incrementally () ; `P F' in menu list
  "Incrementally filter bookmarks by file name using a regexp."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (unwind-protect
       (progn (setq bookmarkp-bmenu-filter-timer
                    (run-with-timer 0 bookmarkp-incremental-filter-delay
                                    #'bookmarkp-bmenu-filter-alist-by-file-name-regexp))
              (bookmarkp-bmenu-read-filter-input))
    (bookmarkp-bmenu-cancel-incremental-filtering)))

(defun bookmarkp-bmenu-filter-alist-by-file-name-regexp ()
  "Filter bookmarks by file name, then refresh the menu list."
  (setq bookmarkp-bmenu-filter-function  'bookmarkp-regexp-filtered-file-name-alist-only
        bookmarkp-bmenu-title
        (format "%% Bookmarks with File Names Matching Regexp `%s'"
                bookmarkp-bmenu-filter-pattern))
  (let ((bookmark-alist  (funcall bookmarkp-bmenu-filter-function)))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp)))

;;;###autoload
(defun bookmarkp-bmenu-filter-tags-incrementally () ; `P T' in menu list
  "Incrementally filter bookmarks by tags using a regexp."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (unwind-protect
       (progn (setq bookmarkp-bmenu-filter-timer
                    (run-with-timer 0 bookmarkp-incremental-filter-delay
                                    #'bookmarkp-bmenu-filter-alist-by-tags-regexp))
              (bookmarkp-bmenu-read-filter-input))
    (bookmarkp-bmenu-cancel-incremental-filtering)))

(defun bookmarkp-bmenu-filter-alist-by-tags-regexp ()
  "Filter bookmarks by tags, then refresh the menu list."
  (setq bookmarkp-bmenu-filter-function  'bookmarkp-regexp-filtered-tags-alist-only
        bookmarkp-bmenu-title
        (format "%% Bookmarks with Tags Matching Regexp `%s'" bookmarkp-bmenu-filter-pattern))
  (let ((bookmark-alist  (funcall bookmarkp-bmenu-filter-function)))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list 'filteredp)))

(defun bookmarkp-bmenu-read-filter-input ()
  "Read input and add it to `bookmarkp-bmenu-filter-pattern'."
  (setq bookmarkp-bmenu-filter-pattern  "")
  (let ((tmp-list  ())
        char)
    (catch 'bookmarkp-bmenu-read-filter-input-1
      (while t
        (catch 'bookmarkp-bmenu-read-filter-input-2
          (condition-case nil
              (setq char  (read-char (concat bookmarkp-bmenu-filter-prompt
                                             bookmarkp-bmenu-filter-pattern)))
            (error (throw 'bookmarkp-bmenu-read-filter-input-1 nil)))
          (case char
            ((?\e ?\r)
             (throw 'bookmarkp-bmenu-read-filter-input-1 nil)) ; RET or ESC break and exit.
            (?\d
             (pop tmp-list)             ; Delete last char of `bookmarkp-bmenu-filter-pattern'.
             (setq bookmarkp-bmenu-filter-pattern  (mapconcat 'identity (reverse tmp-list) ""))
             (throw 'bookmarkp-bmenu-read-filter-input-2 nil))
            (t
             (push (text-char-description char) tmp-list)
             (setq bookmarkp-bmenu-filter-pattern  (mapconcat 'identity (reverse tmp-list) ""))
             (throw 'bookmarkp-bmenu-read-filter-input-2 nil))))))))

(defun bookmarkp-bmenu-cancel-incremental-filtering ()
  "Cancel timer used for incrementally filtering bookmarks."
  (cancel-timer bookmarkp-bmenu-filter-timer)
  (setq bookmarkp-bmenu-filter-timer  nil))

;;;###autoload
(defun bookmarkp-bmenu-toggle-show-only-unmarked () ; `<' in menu list
  "Hide all marked bookmarks.  Repeat to toggle, showing all."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (if (or (bookmarkp-some-marked-p bookmarkp-latest-bookmark-alist)
          (bookmarkp-some-marked-p bookmarkp-bmenu-before-hide-marked-alist))
      (let ((bookmark-alist  bookmarkp-latest-bookmark-alist)
            status)
        (if bookmarkp-bmenu-before-hide-marked-alist
            (setq bookmark-alist  bookmarkp-bmenu-before-hide-marked-alist
                  bookmarkp-bmenu-before-hide-marked-alist  ()
                  bookmarkp-latest-bookmark-alist           bookmark-alist
                  status                                    'shown)
          (setq bookmarkp-bmenu-before-hide-marked-alist  bookmarkp-latest-bookmark-alist
                bookmark-alist                            (bookmarkp-unmarked-bookmarks-only)
                bookmarkp-latest-bookmark-alist           bookmark-alist
                status                                    'hidden))
        (bookmark-bmenu-surreptitiously-rebuild-list)
        (cond ((eq status 'hidden)
               (bookmark-bmenu-check-position)
               (message "Marked bookmarks are now hidden"))
              (t
               (goto-char (point-min))
               (when (re-search-forward "^>" (point-max) t)  (forward-line 0))
               (message "Marked bookmarks no longer hidden"))))
    (message "No marked bookmarks to hide"))
  (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window)))

;;;###autoload
(defun bookmarkp-bmenu-toggle-show-only-marked () ; `>' in menu list
  "Hide all unmarked bookmarks.  Repeat to toggle, showing all."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (if (or (bookmarkp-some-unmarked-p bookmarkp-latest-bookmark-alist)
          (bookmarkp-some-unmarked-p bookmarkp-bmenu-before-hide-unmarked-alist))
      (let ((bookmark-alist                        bookmarkp-latest-bookmark-alist)
            status)
        (if bookmarkp-bmenu-before-hide-unmarked-alist
            (setq bookmark-alist  bookmarkp-bmenu-before-hide-unmarked-alist
                  bookmarkp-bmenu-before-hide-unmarked-alist  ()
                  bookmarkp-latest-bookmark-alist             bookmark-alist
                  status                                      'shown)
          (setq bookmarkp-bmenu-before-hide-unmarked-alist  bookmarkp-latest-bookmark-alist
                bookmark-alist                              (bookmarkp-marked-bookmarks-only)
                bookmarkp-latest-bookmark-alist             bookmark-alist
                status                                      'hidden))
        (bookmark-bmenu-surreptitiously-rebuild-list)
        (cond ((eq status 'hidden)
               (bookmark-bmenu-check-position)
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
(defun bookmarkp-bmenu-mark-all ()      ; `M-m' in menu list
  "Mark all bookmarks, using mark `>'."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (save-excursion  
    (let ((count  0))
      (goto-char (point-min))
      (forward-line 2)
      (while (not (eobp)) (bookmark-bmenu-mark) (setq count  (1+ count)))
      (message "Marked: %d" count))))

;; This is similar to `dired-unmark-all-files'.
;;;###autoload
(defun bookmarkp-bmenu-unmark-all (mark &optional arg) ; `M-DEL', `U' in menu list
  "Remove a mark from each bookmark.
Hit the mark character (`>' or `D') to remove those marks,
 or hit `RET' to remove all marks (both `>' and `D').
With a prefix arg, you are queried to unmark each marked bookmark.
Use `\\[help-command]' during querying for help."
  (interactive "cRemove marks (RET means all): \nP")
  (bookmarkp-barf-if-not-in-menu-list)
  (require 'dired-aux)
  (save-excursion
    (let* ((count                0)
           (inhibit-read-only    t)
           (case-fold-search     nil)
           (query                nil)
           (string               (format "\n%c" mark))
           (help-form            "\
Type SPC or `y' to unmark one bookmark, DEL or `n' to skip to next,
`!' to unmark all remaining bookmarks with no more questions."))
      (goto-char (point-min))
      (forward-line 1)                  ; Only one, because STRING starts with a newline.
      (while (and (not (eobp))
                  (if (eq mark ?\r)
                      (re-search-forward dired-re-mark nil t)
                    (let ((case-fold-search  t)) ; Treat `d' the same as `D'.
                      (search-forward string nil t))))
        (when (or (not arg)
                  (let ((bmk  (bookmark-bmenu-bookmark)))
                    (and bmk (dired-query 'query "Unmark bookmark `%s'? " bmk))))
          (bookmark-bmenu-unmark) (forward-line -1)
          (setq count  (1+ count))))
      (if (= 1 count) (message "1 mark removed") (message "%d marks removed" count)))))

;;;###autoload
(defun bookmarkp-bmenu-regexp-mark (regexp) ; `% m' in menu list
  "Mark bookmarks that match REGEXP.
The entire bookmark line is tested: bookmark name and possibly file name."
  (interactive "sRegexp: ")
  (bookmarkp-barf-if-not-in-menu-list)
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (let ((count  0))
      (while (and (not (eobp)) (re-search-forward regexp (point-max) t))
        (bookmark-bmenu-mark)
        (setq count  (1+ count)))
      (if (= 1 count) (message "1 bookmark matched") (message "%d bookmarks matched" count)))))

;;;###autoload
(defun bookmarkp-bmenu-toggle-marks ()  ; `t' in menu list
  "Toggle marks: Unmark all marked bookmarks; mark all unmarked bookmarks.
This affects only the `>' mark, not the `D' flag."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (let ((marked-count    0)
        (unmarked-count  0))
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (if (not (bookmarkp-some-marked-p bookmarkp-latest-bookmark-alist))
          (bookmarkp-bmenu-mark-all)
        (while (not (eobp))
          (cond ((member (bookmark-bmenu-bookmark) bookmarkp-bmenu-marked-bookmarks)
                 (bookmark-bmenu-unmark)
                 (setq unmarked-count  (1+ unmarked-count)))
                (t
                 (bookmark-bmenu-mark)
                 (setq marked-count  (1+ marked-count)))))
        (message "Marked: %d, unmarked: %d" marked-count unmarked-count)))))

;;;###autoload
(defun bookmarkp-bmenu-delete-marked () ; `D' in menu list
  "Delete all (visible) bookmarks that are marked `>', after confirmation."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (bookmark-bmenu-execute-deletions 'marked))


;;(@* "Search-and-Replace Locations of Marked Bookmarks")
;;  *** Search-and-Replace Locations of Marked Bookmarks ***

;;;###autoload
(when (> emacs-major-version 22)

  (defvar bookmarkp-isearch-bookmarks nil)

  (defun bookmarkp-isearch-next-bookmark-buffer (&optional bookmark wrap)
    "Return the next buffer in a series of bookmark buffers.
Used as a value for `multi-isearch-next-buffer-function', for Isearch
of multiple bookmarks.

Variable `bookmarkp-isearch-bookmarks' is a list of bookmark names.
Each bookmark in that list is visited by `bookmark--jump-via', and the
corresponding bookmark buffer is returned."
    (let ((bookmarks  (if isearch-forward
                          bookmarkp-isearch-bookmarks
                        (reverse bookmarkp-isearch-bookmarks))))
      (bookmark--jump-via
       (if wrap
           (car bookmarks)
         (let ((this-bmk  (catch 'bookmarkp-isearch-next-bookmark-buffer
                            (dolist (bmk  bookmarks)
                              (when (if (bookmarkp-get-buffer-name bmk)
                                        (equal (bookmarkp-get-buffer-name bmk) (buffer-name))
                                      (equal (bookmark-get-filename bmk) (buffer-file-name)))
                                (throw 'bookmarkp-isearch-next-bookmark-buffer bmk)))
                            (car bookmarks))))
           (cadr (member this-bmk bookmarks))))
       'ignore)
      (current-buffer)))

  (defun bookmarkp-isearch-bookmarks (bookmarks)
    "Start multi-bookmark Isearch on BOOKMARKS."
    (let ((multi-isearch-next-buffer-function  'bookmarkp-isearch-next-bookmark-buffer)
          (bookmarkp-isearch-bookmarks         bookmarks))
      (bookmark-jump (car bookmarks))
      (goto-char (if isearch-forward (point-min) (point-max)))
      (isearch-forward)))

  (defun bookmarkp-isearch-bookmarks-regexp (bookmarks)
    "Start multi-bookmark regexp Isearch on BOOKMARKS."
    (let ((multi-isearch-next-buffer-function  'bookmarkp-isearch-next-bookmark-buffer)
          (bookmarkp-isearch-bookmarks         bookmarks))
      (bookmark-jump (car bookmarks))
      (goto-char (if isearch-forward (point-min) (point-max)))
      (isearch-forward-regexp)))

  (defun bookmarkp-bmenu-isearch-marked-bookmarks () ; `M-s a C-s' in menu list
    "Isearch the marked bookmark locations, in their current order."
    (interactive)
    (bookmarkp-barf-if-not-in-menu-list)
    (let ((bookmarks                  (mapcar #'car (bookmarkp-sort-and-remove-dups
                                                     (bookmarkp-marked-bookmarks-only))))
          (bookmarkp-use-region-flag  nil)) ; Suppress region handling.
      (bookmarkp-isearch-bookmarks bookmarks)))

  (defun bookmarkp-bmenu-isearch-marked-bookmarks-regexp () ; `M-s a M-C-s' in menu list
    "Regexp Isearch the marked bookmark locations, in their current order."
    (interactive)
    (bookmarkp-barf-if-not-in-menu-list)
    (let ((bookmarks                  (mapcar #'car (bookmarkp-sort-and-remove-dups
                                                     (bookmarkp-marked-bookmarks-only))))
          (bookmarkp-use-region-flag  nil)) ; Suppress region handling.
      (bookmarkp-isearch-bookmarks-regexp bookmarks))))

(defun bookmarkp-bmenu-search-marked-bookmarks-regexp (regexp) ; `M-a' in menu list
  "Search the marked file bookmarks, in their current order, for REGEXP.
Use `\\[tags-loop-continue]' to advance among the search hits.
Marked directory and non-file bookmarks are ignored."
  (interactive "sSearch marked file bookmarks (regexp): ")
  (bookmarkp-barf-if-not-in-menu-list)
  (tags-search regexp '(let ((files  ())
                             file)
                        (dolist (bmk  (bookmarkp-sort-and-remove-dups
                                       (bookmarkp-marked-bookmarks-only)))
                          (setq file  (bookmark-get-filename bmk))
                          (when (and (not (equal bookmarkp-non-file-filename file))
                                     (not (file-directory-p file)))
                            (push file files)))
                        (setq files  (nreverse files)))))

(defun bookmarkp-bmenu-query-replace-marked-bookmarks-regexp (from to ; `M-q' in menu list
                                                              &optional delimited)
  "`query-replace-regexp' FROM with TO, for all marked file bookmarks.
DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (`\\[keyboard-quit]', `RET' or `q'), you can use \
`\\[tags-loop-continue]' to resume where
you left off."
  (interactive (let ((common  (query-replace-read-args
                               "Query replace regexp in marked files" t t)))
                 (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (bookmarkp-barf-if-not-in-menu-list)
  (tags-query-replace from to delimited
		      '(let ((files  ())
                             file)
                        (dolist (bmk  (bookmarkp-sort-and-remove-dups
                                       (bookmarkp-marked-bookmarks-only)))
                          (setq file  (bookmark-get-filename bmk))
                          (let ((buffer  (get-file-buffer file)))
                            (when (and buffer
                                       (with-current-buffer buffer buffer-read-only))
                              (error "File `%s' is visited read-only" file)))
                          (when (and (not (equal bookmarkp-non-file-filename file))
                                     (not (file-directory-p file)))
                            (push file files)))
                        (setq files  (nreverse files)))))


;;(@* "Tags")
;;  *** Tags ***

(defun bookmarkp-get-tags (bookmark)
  "Return the `tags' entry for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (bookmark-prop-get bookmark 'tags))

(defun bookmarkp-has-tag-p (bookmark tag &optional msgp)
  "Return non-nil if BOOKMARK is tagged with TAG.
BOOKMARK is a bookmark name or a bookmark record."
  (member tag (bookmarkp-get-tags bookmark)))

;; Not used currently - we use `bookmarkp-read-tags-completing' instead.
(defun bookmarkp-read-tags ()
  "Read tags one by one, and return them as a list."
  (let ((tag    (read-string "Tag (RET for each, empty input to finish): "))
        (btags  ()))
    (while (not (string= "" tag))
      (push tag btags)
      (setq tag  (read-string "Tag: ")))
    btags))

(defun bookmarkp-read-tags-completing (&optional candidate-tags require-match)
  "Read tags one by one with completion, and return them as a list.
CANDIDATE-TAGS is a list of strings, possible completions.
REQUIRE-MATCH is passed to `completing-read'."
  (let* ((cand-tags  (copy-sequence candidate-tags))
         (tag        (completing-read "Tag (RET for each, empty input to finish): "
                                      (mapcar #'list cand-tags) nil require-match nil
                                      'bookmarkp-tag-history))
         (btags      ()))
    (setq cand-tags  (delete tag cand-tags))
    (while (not (string= "" tag))
      (push tag btags)
      (setq tag        (completing-read "Tag: " (mapcar #'list cand-tags) nil
                                        require-match nil 'bookmarkp-tag-history)
            cand-tags  (delete tag cand-tags)))
    (nreverse btags)))

;;;###autoload
(defun bookmarkp-remove-all-tags (bookmark &optional msgp) ; `T 0' in menu list
  "Remove all tags from BOOKMARK.
Non-nil optional arg MSGP means display a message about the removal."
  (interactive (list (bookmark-completing-read "Bookmark" (bookmarkp-default-bookmark-name))
                     'msg))
  (let ((nb-removed  (and (interactive-p) (length (bookmark-prop-get bookmark 'tags)))))
    (bookmark-prop-set bookmark 'tags ())
    (bookmarkp-maybe-save-bookmarks)
    (when (and msgp nb-removed) (message "%d tags removed" nb-removed))))

;;;###autoload
(defun bookmarkp-add-tags (bookmark tags &optional msgp) ; `T +' in menu list
  "Add TAGS to BOOKMARK.
TAGS is a list of tags (strings) or a single tag.
Non-nil optional arg MSGP means display a message about the addition.
Return the number of tags added."
  (interactive (list (bookmark-completing-read "Bookmark" (bookmarkp-default-bookmark-name))
                     (bookmarkp-read-tags-completing)
                     'msg))
  (when (and tags (atom tags)) (setq tags  (list tags)))
  (let* ((newtags  (copy-sequence (bookmarkp-get-tags bookmark)))
         (olen     (length newtags)))
    (dolist (tag  tags) (pushnew tag newtags :test #'equal))
    (bookmark-prop-set bookmark 'tags newtags)
    (bookmarkp-maybe-save-bookmarks)
    (let ((nb-added  (- (length newtags) olen)))
      (when msgp
        (message "%d tags added. Now: %S" nb-added
                 (let ((tt  (copy-sequence newtags))) (setq tt (sort tt #'string<)))))
      nb-added)))

;;;###autoload
(defun bookmarkp-remove-tags (bookmark tags &optional msgp) ; `T -' in menu list
  "Remove TAGS from BOOKMARK.
TAGS is a list of tags (strings) or a single tag.
Non-nil optional arg MSGP means display a message about the removal."
  (interactive
   (let ((bmk  (bookmark-completing-read "Bookmark" (bookmarkp-default-bookmark-name))))
     (list bmk (bookmarkp-read-tags-completing (bookmarkp-get-tags bmk) t) 'msg)))
  (when (and tags (atom tags)) (setq tags  (list tags)))
  (let* ((newtags  (bookmarkp-get-tags bookmark))
         (olen     (length newtags)))
    (if (null newtags)
        (when msgp (message "Bookmark has no tags")) ; Do nothing if bookmark has no tags.
      (setq newtags  (bookmarkp-remove-if #'(lambda (tt) (member tt tags))
                                          (bookmarkp-get-tags bookmark)))
      (bookmark-prop-set bookmark 'tags newtags)
      (bookmarkp-maybe-save-bookmarks)
      (let ((nb-removed  (- olen (length newtags))))
        (when msgp
          (message "%d tags removed. Now: %S" nb-removed
                   (let ((tt  (copy-sequence newtags))) (setq tt (sort tt #'string<)))))
        nb-removed))))

;;;###autoload
(defun bookmarkp-remove-tags-from-all (tags &optional msgp) ; `T d'
  "Remove TAGS from all bookmarks, even those not showing in `*Bookmark List*'.
This in effect deletes the tags entirely.
TAGS is a list of tags (strings) or a single tag.
Non-nil optional arg MSGP means display a message about the deletion."
  (interactive
   (if (not (y-or-n-p
             "Delete the tags you specify from ALL bookmarks, even those not shown? "))
       (error "Deletion cancelled")
     (let ((cand-tags  ()))
       (dolist (bmk  bookmark-alist)
         (setq cand-tags  (bookmarkp-set-union cand-tags (bookmarkp-get-tags bmk))))
       (list (bookmarkp-read-tags-completing cand-tags t) 'msg))))
  (dolist (bmk  (mapcar #'car bookmark-alist))
    (bookmarkp-remove-tags bmk tags))
  (when msgp (message "Tags removed from all bookmarks: %S" tags)))

;;;###autoload
(defun bookmarkp-rename-tag (tag newname &optional msgp) ; `T r'
  "Rename TAG to NEWNAME in all bookmarks, even those not showing.
Non-nil optional arg MSGP means display a message about the deletion."
  (interactive
   (list (read-string "Tag (old name): ") (read-string "New name: ") 'msg))
  (dolist (bmk  (mapcar #'car bookmark-alist))
    (let* ((bmktags  (copy-sequence (bookmarkp-get-tags bmk)))
           (newtags  bmktags))
      (when newtags
        (catch 'bookmarkp-rename-tag
          (while bmktags
            (when (equal (car bmktags) tag)
              (setcar bmktags newname)
              (throw 'bookmarkp-rename-tag nil)) ; Assume only one tag with the same name.
            (setq bmktags  (cdr bmktags))))
        (bookmark-prop-set bmk 'tags newtags))))
  (when msgp (message "Renamed")))

;;;###autoload
(defun bookmarkp-bmenu-add-tags-to-marked (tags &optional msgp) ; `T > +' in menu list
  "Add TAGS to each of the marked bookmarks."
  (interactive (list (bookmarkp-read-tags-completing) 'msg))
  (bookmarkp-barf-if-not-in-menu-list)
  (when msgp (message "Adding tags..."))
  (dolist (bmk  (mapcar #'car (bookmarkp-marked-bookmarks-only)))
    (bookmarkp-add-tags bmk tags))
  (when msgp (message "Tags added: %S" tags)))

;;;###autoload
(defun bookmarkp-bmenu-remove-tags-from-marked (tags &optional msgp) ; `T > -' in menu list
  "Remove TAGS from each of the marked bookmarks."
  (interactive (let ((cand-tags  ()))
                 (dolist (bmk  (bookmarkp-marked-bookmarks-only))
                   (setq cand-tags  (bookmarkp-set-union cand-tags (bookmarkp-get-tags bmk))))
                 (list (bookmarkp-read-tags-completing cand-tags t) 'msg)))
  (bookmarkp-barf-if-not-in-menu-list)
  (when msgp (message "Removing tags..."))
  (dolist (bmk  (mapcar #'car (bookmarkp-marked-bookmarks-only)))
    (bookmarkp-remove-tags bmk tags))
  (when msgp (message "Tags removed: %S" tags)))

;;;###autoload
(defun bookmarkp-bmenu-mark-bookmarks-tagged-regexp (regexp &optional notp) ; `T m %' in menu
  "Mark bookmarks any of whose tags match REGEXP.
With a prefix arg, mark all that are tagged but with no tags that match."
  (interactive "sRegexp: \nP")
  (bookmarkp-barf-if-not-in-menu-list)
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (let ((count  0)
          tags anyp)
      (while (not (eobp))
        (setq tags  (bookmarkp-get-tags (bookmark-bmenu-bookmark))
              anyp  (and tags (bookmarkp-some (lambda (tag) (string-match regexp tag)) tags)))
        (when (and tags (if notp (not anyp) anyp))
          (bookmark-bmenu-mark)
          (setq count  (1+ count)))
        (forward-line 1))
      (if (= 1 count) (message "1 bookmark matched") (message "%d bookmarks matched" count)))))

;;;###autoload
(defun bookmarkp-bmenu-mark-bookmarks-tagged-all (tags ; `T m *' in menu list
                                                  &optional nonep msgp)
  "Mark all visible bookmarks that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
any tags at all.

With a prefix arg, mark all that are *not* tagged with *any* TAGS."
  (interactive (let ((cand-tags  ()))
                 (dolist (bmk  bookmark-alist)
                   (setq cand-tags  (bookmarkp-set-union cand-tags (bookmarkp-get-tags bmk))))
                 (list (bookmarkp-read-tags-completing cand-tags) current-prefix-arg 'msg)))
  (bookmarkp-barf-if-not-in-menu-list)
  (bookmarkp-bmenu-mark/unmark-bookmarks-tagged-all/none tags nonep nil msgp))

;;;###autoload
(defun bookmarkp-bmenu-mark-bookmarks-tagged-none (tags ; `T m ~ +' in menu list
                                                   &optional allp msgp)
  "Mark all visible bookmarks that are not tagged with *any* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
no tags at all.

With a prefix arg, mark all that are tagged with *each* tag in TAGS."
  (interactive (let ((cand-tags  ()))
                 (dolist (bmk  bookmark-alist)
                   (setq cand-tags  (bookmarkp-set-union cand-tags (bookmarkp-get-tags bmk))))
                 (list (bookmarkp-read-tags-completing cand-tags) current-prefix-arg 'msg)))
  (bookmarkp-barf-if-not-in-menu-list)
  (bookmarkp-bmenu-mark/unmark-bookmarks-tagged-all/none tags (not allp) nil msgp))

;;;###autoload
(defun bookmarkp-bmenu-mark-bookmarks-tagged-some (tags ; `T m +' in menu list
                                                   &optional somenotp msgp)
  "Mark all visible bookmarks that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
any tags at all.

With a prefix arg, mark all that are *not* tagged with *all* TAGS."
  (interactive (let ((cand-tags  ()))
                 (dolist (bmk  bookmark-alist)
                   (setq cand-tags  (bookmarkp-set-union cand-tags (bookmarkp-get-tags bmk))))
                 (list (bookmarkp-read-tags-completing cand-tags) current-prefix-arg 'msg)))
  (bookmarkp-barf-if-not-in-menu-list)
  (bookmarkp-bmenu-mark/unmark-bookmarks-tagged-some/not-all tags somenotp nil msgp))

;;;###autoload
(defun bookmarkp-bmenu-mark-bookmarks-tagged-not-all (tags ; `T m ~ *' in menu list
                                                      &optional somep msgp)
  "Mark all visible bookmarks that are *not* tagged with *all* TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
no tags at all.

With a prefix arg, mark all that are tagged with *some* tag in TAGS."
  (interactive (let ((cand-tags  ()))
                 (dolist (bmk  bookmark-alist)
                   (setq cand-tags  (bookmarkp-set-union cand-tags (bookmarkp-get-tags bmk))))
                 (list (bookmarkp-read-tags-completing cand-tags) current-prefix-arg 'msg)))
  (bookmarkp-barf-if-not-in-menu-list)
  (bookmarkp-bmenu-mark/unmark-bookmarks-tagged-some/not-all tags (not somep) nil msgp))

;;;###autoload
(defun bookmarkp-bmenu-unmark-bookmarks-tagged-all (tags ; `T u *' in menu list
                                                    &optional nonep msgp)
  "Unmark all visible bookmarks that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
any tags at all.

With a prefix arg, unmark all that are *not* tagged with *any* TAGS."
  (interactive (let ((cand-tags  ()))
                 (dolist (bmk  bookmark-alist)
                   (setq cand-tags  (bookmarkp-set-union cand-tags (bookmarkp-get-tags bmk))))
                 (list (bookmarkp-read-tags-completing cand-tags) current-prefix-arg 'msg)))
  (bookmarkp-barf-if-not-in-menu-list)
  (bookmarkp-bmenu-mark/unmark-bookmarks-tagged-all/none tags nonep 'unmark msgp))

;;;###autoload
(defun bookmarkp-bmenu-unmark-bookmarks-tagged-none (tags ; `T u ~ +' in menu list
                                                     &optional allp msgp)
  "Unmark all visible bookmarks that are *not* tagged with *any* TAGS.
With a prefix arg, unmark all that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
no tags at all."
  (interactive (let ((cand-tags  ()))
                 (dolist (bmk  bookmark-alist)
                   (setq cand-tags  (bookmarkp-set-union cand-tags (bookmarkp-get-tags bmk))))
                 (list (bookmarkp-read-tags-completing cand-tags) current-prefix-arg 'msg)))
  (bookmarkp-barf-if-not-in-menu-list)
  (bookmarkp-bmenu-mark/unmark-bookmarks-tagged-all/none tags (not allp) 'unmark msgp))

;;;###autoload
(defun bookmarkp-bmenu-unmark-bookmarks-tagged-some (tags ; `T u +' in menu list
                                                     &optional somenotp msgp)
  "Unmark all visible bookmarks that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then unmark the bookmarks that have
any tags at all.

With a prefix arg, unmark all that are *not* tagged with *all* TAGS."
  (interactive (let ((cand-tags  ()))
                 (dolist (bmk  bookmark-alist)
                   (setq cand-tags  (bookmarkp-set-union cand-tags (bookmarkp-get-tags bmk))))
                 (list (bookmarkp-read-tags-completing cand-tags) current-prefix-arg 'msg)))
  (bookmarkp-barf-if-not-in-menu-list)
  (bookmarkp-bmenu-mark/unmark-bookmarks-tagged-some/not-all tags somenotp 'unmark msgp))

;;;###autoload
(defun bookmarkp-bmenu-unmark-bookmarks-tagged-not-all (tags ; `T u ~ *' in menu list
                                                        &optional somep msgp)
  "Unmark all visible bookmarks that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then mark the bookmarks that have
no tags at all.
With a prefix arg, unmark all that are *not* tagged with *all* TAGS."
  (interactive (let ((cand-tags  ()))
                 (dolist (bmk  bookmark-alist)
                   (setq cand-tags  (bookmarkp-set-union cand-tags (bookmarkp-get-tags bmk))))
                 (list (bookmarkp-read-tags-completing cand-tags) current-prefix-arg 'msg)))
  (bookmarkp-barf-if-not-in-menu-list)
  (bookmarkp-bmenu-mark/unmark-bookmarks-tagged-some/not-all tags (not somep) 'unmark msgp))

(defun bookmarkp-bmenu-mark/unmark-bookmarks-tagged-all/none (tags &optional nonep
                                                              unmarkp msgp)
  "Mark or unmark visible bookmarks tagged with all or none of TAGS.
NONEP non-nil means mark/unmark bookmarks that have none of the TAGS.
UNMARKP non-nil means unmark; nil means mark.
MSGP means display a status message.

As a special case, if TAGS is empty, then mark or unmark the bookmarks
that have any tags at all, or if NONEP is non-nil then mark or unmark
those that have no tags at all."
  (with-current-buffer "*Bookmark List*"
    (save-excursion  
      (let ((count  0)
            bmktags)
        (goto-char (point-min))
        (forward-line 2)
        (while (not (eobp))
          (setq bmktags  (bookmarkp-get-tags (bookmark-bmenu-bookmark)))
          (if (not (if (null tags)
                       (if nonep (not bmktags) bmktags)
                     (and bmktags
                          (catch 'bookmarkp-mark-bookmarks-tagged
                            (dolist (tag  tags)
                              (unless (if nonep
                                          (not (member tag bmktags))
                                        (member tag bmktags))
                                (throw 'bookmarkp-mark-bookmarks-tagged nil)))
                            t))))
              (forward-line 1)
            (if unmarkp
                (bookmark-bmenu-unmark)
              (bookmark-bmenu-mark))
            (setq count  (1+ count))))
        (when msgp
          (if (= 1 count)
              (message "1 bookmark matched")
            (message "%d bookmarks matched" count)))))))

(defun bookmarkp-bmenu-mark/unmark-bookmarks-tagged-some/not-all (tags &optional notallp
                                                                  unmarkp msgp)
  "Mark or unmark visible bookmarks tagged with any or not all of TAGS.
NOTALLP non-nil means mark/unmark bookmarks that do not have all TAGS.
UNMARKP non-nil means unmark; nil means mark.
MSGP means display a status message.

As a special case, if TAGS is empty, then mark or unmark the bookmarks
that have any tags at all, or if NOTALLP is non-nil then mark or
unmark those that have no tags at all."
  (with-current-buffer "*Bookmark List*"
    (save-excursion  
      (let ((count  0)
            bmktags)
        (goto-char (point-min))
        (forward-line 2)
        (while (not (eobp))
          (setq bmktags  (bookmarkp-get-tags (bookmark-bmenu-bookmark)))
          (if (not (if (null tags)
                       (if notallp (not bmktags) bmktags)
                     (and bmktags
                          (catch 'bookmarkp-mark-bookmarks-tagged
                            (dolist (tag  tags)
                              (when (if notallp
                                        (not (member tag bmktags))
                                      (member tag bmktags))
                                (throw 'bookmarkp-mark-bookmarks-tagged t)))
                            nil))))
              (forward-line 1)
            (if unmarkp
                (bookmark-bmenu-unmark)
              (bookmark-bmenu-mark))
            (setq count  (1+ count))))
        (when msgp
          (if (= 1 count)
              (message "1 bookmark matched")
            (message "%d bookmarks matched" count)))))))


;;(@* "General Menu-List (`-*bmenu-*') Commands and Functions")
;;  *** General Menu-List (`-*bmenu-*') Commands and Functions ***

;;;###autoload
(defun bookmarkp-bmenu-edit-bookmark () ; `E' in menu list
  "Edit the bookmark under the cursor."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (let* ((new-data  (bookmarkp-edit-bookmark (bookmark-bmenu-bookmark)))
         (new-name  (car new-data)))
    (if (not new-data)
        (message "No changes made")
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (goto-char (point-min))
      (while (not (equal new-name (bookmark-bmenu-bookmark))) (forward-line 1))
      (forward-line 0)
      (bookmark-bmenu-check-position))))

(defun bookmarkp-bmenu-propertize-item (bookmark-name start end)
  "Add text properties to BOOKMARK-NAME, from START to END."
  (let* ((isfile        (bookmark-get-filename bookmark-name))
         (isremote      (and isfile  (bookmarkp-file-remote-p isfile)))
         (istramp       (and isfile  (boundp 'tramp-file-name-regexp)
                             (save-match-data (string-match tramp-file-name-regexp isfile))))
         (isw3m         (bookmarkp-w3m-bookmark-p bookmark-name))
         (issu          (and istramp (string-match bookmarkp-su-or-sudo-regexp isfile)))
         (isregion      (bookmarkp-region-bookmark-p bookmark-name))
         (isannotation  (bookmark-get-annotation bookmark-name))
         (ishandler     (bookmark-get-handler bookmark-name))
         (isgnus        (bookmarkp-gnus-bookmark-p bookmark-name))
         (isbuf         (bookmarkp-get-buffer-name bookmark-name)))
    (add-text-properties
     start  end
     (cond ((or (eq ishandler 'Info-bookmark-jump) (string= isbuf "*info*") ; Info
                ;; Emacs 20-21 form - no handler (and no `buffer-name' entry).
                (bookmark-prop-get bookmark-name 'info-node))
            (append (bookmarkp-face-prop 'bookmarkp-info)
                    '(mouse-face highlight follow-link t
                      help-echo "mouse-2: Visit this Info bookmark")))
           (isgnus                      ; Gnus
            (append (bookmarkp-face-prop 'bookmarkp-gnus)
                    '(mouse-face highlight follow-link t
                      help-echo "mouse-2: Visit this Gnus bookmark")))
           (isw3m                       ; W3M
            (append (bookmarkp-face-prop 'bookmarkp-w3m)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Visit URL `%s'" ,isfile))))
           ((and issu (not (bookmarkp-root-or-sudo-logged-p))) ; Root/sudo not logged
            (append (bookmarkp-face-prop 'bookmarkp-su-or-sudo)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Visit file `%s'" ,isfile))))
           ;; Make sure we test for remoteness before any other tests of the file itself
           ;; (e.g. `file-exists-p'). We don't want to prompt for a password etc.
           ((and isremote (not issu))   ; Remote file (ssh, ftp)
            (append (bookmarkp-face-prop 'bookmarkp-remote-file)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Visit remote file `%s'" ,isfile))))
           ((and isfile (file-directory-p isfile)) ; Local directory
            (append (bookmarkp-face-prop 'bookmarkp-local-directory)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Dired directory `%s'" ,isfile))))
           ((and isfile (file-exists-p isfile) isregion) ; Local file with region
            (append (bookmarkp-face-prop 'bookmarkp-local-file-with-region)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Activate region in file `%s'" ,isfile))))
           ((and isfile (file-exists-p isfile)) ; Local file without region
            (append (bookmarkp-face-prop 'bookmarkp-local-file-without-region)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Visit file `%s'" ,isfile))))
           ((and isbuf (get-buffer isbuf) (equal isfile bookmarkp-non-file-filename)) ; Buffer
            (append (bookmarkp-face-prop 'bookmarkp-buffer)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Visit buffer `%s'" ,isbuf))))
           ((and isbuf (or (not isfile) (equal isfile bookmarkp-non-file-filename)
                           (not (file-exists-p isfile)))) ; Buffer bookmark, but no buffer.
            (append (bookmarkp-face-prop 'bookmarkp-non-file)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2: Visit buffer `%s'" ,isbuf))))
           (t (append (bookmarkp-face-prop 'bookmarkp-bad-bookmark)
                      `(mouse-face highlight follow-link t
                        help-echo (format "BAD BOOKMARK (maybe): `%s'" ,isfile))))))))

;;;###autoload
(defun bookmarkp-bmenu-quit ()          ; `q' in menu list
  "Reset the marked bookmark lists and quit."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (setq bookmarkp-bmenu-marked-bookmarks            ()
        bookmarkp-bmenu-before-hide-marked-alist    ()
        bookmarkp-bmenu-before-hide-unmarked-alist  ())
  (quit-window))

(defun bookmarkp-bmenu-goto-bookmark-named (name)
  "Go to the first bookmark whose name (visible portion) matches NAME."
  (goto-char (point-min))
  (if (not bookmark-bmenu-toggle-filenames)
      (re-search-forward (concat (regexp-quote name) "\\s-*$"))
    (let ((len    (length name))
          (limit  (- bookmark-bmenu-file-column 2)))
      (setq name  (if (>= len limit)
                      (substring name 0 limit)
                    (format (format "%%-%ds" limit) name))))
    (search-forward name nil t))
  (forward-line 0))

(defun bookmarkp-barf-if-not-in-menu-list ()
  "Raise an error if current buffer is not `*Bookmark List*'."
  (unless (equal (buffer-name (current-buffer)) "*Bookmark List*")
    (error "Use this command only in buffer `*Bookmark List*'")))


;;(@* "Bookmark Predicates")
;;  *** Bookmark Predicates ***

(defun bookmarkp-region-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK has region information.
BOOKMARK is a bookmark name or a bookmark record."
  (and (bookmarkp-get-end-position bookmark)
       (/= (bookmark-get-position bookmark)
           (bookmarkp-get-end-position bookmark))))

(defun bookmarkp-gnus-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Gnus bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-gnus))

(defun bookmarkp-w3m-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a W3M bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-w3m))

(defun bookmarkp-info-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is an Info bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (eq (bookmark-get-handler bookmark) 'Info-bookmark-jump)
      (bookmark-prop-get bookmark 'info-node))) ; Emacs 20-21 Info bookmark - no handler.

;; Note: To avoid remote access, if bookmark does not have the Dired handler, then we insist
;; that it be for a local directory.  IOW, we do not include remote directories that were not
;; bookmarked by Bookmark+ (and so do not have the Dired handler).
(defun bookmarkp-dired-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Dired bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-dired)
      (bookmarkp-local-directory-bookmark-p bookmark)))

(defun bookmarkp-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a file or directory.
BOOKMARK is a bookmark name or a bookmark record.
This excludes bookmarks of a more specific kind (Info, Gnus, and W3M)."
  (let* ((filename   (bookmark-get-filename bookmark))
         (isnonfile  (equal filename bookmarkp-non-file-filename))
         (handler    (bookmark-get-handler bookmark)))
    (and filename (not isnonfile)
         (or (not handler) (eq handler 'bookmarkp-jump-dired))
         (not (and (bookmark-prop-get bookmark 'info-node)))))) ; Emacs 20-21 Info: no handler.

(defun bookmarkp-non-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a non-file bookmark (e.g *scratch*).
This excludes bookmarks of a more specific kind (Info, Gnus, and W3M).
It includes bookmarks to existing buffers, as well as bookmarks
defined for buffers that do not currently exist."
  (let* ((filename   (bookmark-get-filename bookmark))
         (isnonfile  (equal filename bookmarkp-non-file-filename))) 
    (and (bookmarkp-get-buffer-name bookmark)
         (or (not filename) isnonfile
             ;; Ensure not remote before calling `file-exists-p'! (Don't prompt for password!)
             (and (not (bookmarkp-file-remote-p filename))
                  (not (file-exists-p filename))))
         (not (bookmark-get-handler bookmark)))))

(defun bookmarkp-remote-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a remote file or directory.
BOOKMARK is a bookmark name or a bookmark record."
  (let* ((file      (bookmark-get-filename bookmark))
         (rem-file  (and file  (bookmarkp-file-remote-p file))))
    (and rem-file  (not (bookmark-get-handler bookmark)))))

(defun bookmarkp-local-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a local file or directory.
BOOKMARK is a bookmark name or a bookmark record.
This excludes bookmarks of a more specific kind (Info, Gnus, and W3M)."
  (and (bookmarkp-file-bookmark-p bookmark)
       (not (bookmarkp-remote-file-bookmark-p bookmark))))

(defun bookmarkp-local-directory-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a local directory.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((file  (bookmark-get-filename bookmark)))
    (and (bookmarkp-local-file-bookmark-p bookmark) (file-directory-p file))))

(defun bookmarkp-bookmark-marked-p (bookmark)
  "Return non-nil if BOOKMARK is a marked bookmark."
  (member (car bookmark) bookmarkp-bmenu-marked-bookmarks))


;;(@* "Filter Functions")
;;  *** Filter Functions ***

(defun bookmarkp-region-alist-only ()
  "`bookmark-alist', filtered to retain only bookmarks that have regions.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-region-bookmark-p bookmark-alist))

(defun bookmarkp-gnus-alist-only ()
  "`bookmark-alist', filtered to retain only Gnus bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-gnus-bookmark-p bookmark-alist))

(defun bookmarkp-w3m-alist-only ()
  "`bookmark-alist', filtered to retain only W3M bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-w3m-bookmark-p bookmark-alist))

(defun bookmarkp-info-alist-only ()
  "`bookmark-alist', filtered to retain only Info bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-info-bookmark-p bookmark-alist))

(defun bookmarkp-dired-alist-only ()
  "`bookmark-alist', filtered to retain only Dired bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-dired-bookmark-p bookmark-alist))

(defun bookmarkp-remote-file-alist-only ()
  "`bookmark-alist', filtered to retain only remote-file bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-remote-file-bookmark-p bookmark-alist))

(defun bookmarkp-local-file-alist-only ()
  "`bookmark-alist', filtered to retain only local-file bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-local-file-bookmark-p bookmark-alist))

(defun bookmarkp-file-alist-only ()
  "`bookmark-alist', filtered to retain only file and directory bookmarks.
This excludes bookmarks that might contain file information but are
particular in some way - for example, Info bookmarks or Gnus bookmarks.

A new list is returned (no side effects)."
  (bookmarkp-remove-if #'(lambda (bookmark)
                           (or (bookmarkp-non-file-bookmark-p bookmark)
                               (bookmarkp-gnus-bookmark-p bookmark)
                               (bookmarkp-w3m-bookmark-p bookmark)
                               (bookmarkp-info-bookmark-p bookmark)))
                       bookmark-alist))

(defun bookmarkp-non-file-alist-only ()
  "`bookmark-alist', filtered to retain only non-file bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-non-file-bookmark-p bookmark-alist))

(defun bookmarkp-regexp-filtered-bookmark-name-alist-only ()
  "`bookmark-alist', filtered for bookmark-name matches."
  (let ((res  ()))
    (dolist (bmk  bookmark-alist)
      (when (string-match bookmarkp-bmenu-filter-pattern (car bmk))
        (push bmk res)))
    res))

(defun bookmarkp-regexp-filtered-file-name-alist-only ()
  "`bookmark-alist', filtered for file-name matches."
  (let ((res  ())
        fname)
    (dolist (bmk  bookmark-alist)
      (setq fname  (bookmark-get-filename bmk))
      (when (and fname (string-match bookmarkp-bmenu-filter-pattern fname))
        (push bmk res)))
    res))

(defun bookmarkp-regexp-filtered-tags-alist-only ()
  "`bookmark-alist', filtered for tag matches."
  (let ((res  ())
        tags)
    (dolist (bmk  bookmark-alist)
      (setq tags  (bookmarkp-get-tags bmk))
      (when (and tags  (bookmarkp-some (lambda (tt)
                                         (string-match bookmarkp-bmenu-filter-pattern tt))
                                       tags))
        (push bmk res)))
    res))


;;; Marked bookmarks

(defun bookmarkp-marked-bookmarks-only ()
  "Return the list of marked bookmarks."
  (bookmarkp-remove-if-not #'bookmarkp-bookmark-marked-p bookmark-alist))

(defun bookmarkp-unmarked-bookmarks-only ()
  "Return the list of unmarked bookmarks."
  (bookmarkp-remove-if #'bookmarkp-bookmark-marked-p bookmark-alist))

(defun bookmarkp-some-marked-p (alist)
  "Return non-nil if ALIST is nonempty and includes a marked bookmark."
  (catch 'break
    (dolist (i  alist)
      (and (bookmarkp-bookmark-marked-p i) (throw 'break t)))))

(defun bookmarkp-some-unmarked-p (alist)
  "Return non-nil if ALIST is nonempty and includes an unmarked bookmark."
  (catch 'break
    (dolist (i  alist)
      (and (not (bookmarkp-bookmark-marked-p i)) (throw 'break t)))))


;;(@* "General Utility Functions")
;;  *** General Utility Functions ***

(defun bookmarkp-remove-assoc-dups (alist)
  "Copy of ALIST with elements that have duplicate keys removed.
Only the first element of those with the same key is kept.
Keys are compared using `equal'."
  (let ((new   ()))
    (dolist (ii alist) (unless (assoc (car ii) new) (push ii new)))
    (nreverse new)))

(defun bookmarkp-remove-if (pred xs)
  "A copy of list XS with no elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x  xs) (unless (funcall pred x) (push x result)))
    (nreverse result)))

(defun bookmarkp-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x  xs) (when (funcall pred x) (push x result)))
    (nreverse result)))

(defun bookmarkp-assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is `equal' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist)) (equal (car (car alist)) key))
    (setq alist  (cdr alist)))
  (let ((tail  alist)
        tail-cdr)
    (while (setq tail-cdr  (cdr tail))
      (if (and (consp (car tail-cdr))  (equal (car (car tail-cdr)) key))
          (setcdr tail (cdr tail-cdr))
        (setq tail  tail-cdr))))
  alist)

;; Similar to `some' in `cl-seq.el', without non-list sequences and multiple sequences.
(defun bookmarkp-some (predicate list)
  "Return non-nil if PREDICATE is true for any element of LIST.
Returns the first non-nil value returned by PREDICATE."
  (let ((res  nil))
    (while (and list (not (setq res  (funcall predicate (pop list))))))
    res))

;; From `cl-seq.el', function `union', without keyword treatment.
;; (Same as `simple-set-union' in `misc-fns.el' and `icicle-set-union' in `icicles-fn.el'.)
(defun bookmarkp-set-union (list1 list2)
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

(defun bookmarkp-upcase (string)
  "`upcase', but in case of error, return original STRING.
This works around an Emacs 20 problem that occurs if STRING contains
binary data (weird chars)."
  (condition-case nil (upcase string) (error string)))

(defun bookmarkp-file-remote-p (file-name)
  "Returns non-nil if string FILE-NAME is likely to name a remote file."
  (if (fboundp 'file-remote-p)
      (file-remote-p file-name)
    (and (fboundp 'ffap-file-remote-p) (ffap-file-remote-p file-name))))

(defun bookmarkp-replace-regexp-in-string (regexp rep string
                                           &optional fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING and return STRING."
  (if (fboundp 'replace-regexp-in-string) ; Emacs > 20.
      (replace-regexp-in-string regexp rep string fixedcase literal subexp start)
    (if (string-match regexp string)    ; Emacs 20
        (replace-match rep nil nil string)
      string)))

(defun bookmarkp-float-time (&optional specified-time)
  "Same as `float-time'.  (Needed for Emacs 20.)"
  (if (fboundp 'float-time)
      (float-time specified-time)
    (unless specified-time (setq specified-time  (current-time)))
    (+ (* (float (nth 0 specified-time)) (expt 2 16))
       (nth 1 specified-time))))

(defun bookmarkp-face-prop (value)
  "Return a list with elements `face' or `font-lock-face' and VALUE.
Starting with Emacs 22, the first element is `font-lock-face'."
  (list (if (> emacs-major-version 21) 'font-lock-face 'face) value))  

(defun bookmarkp-make-plain-predicate (pred &optional final-pred)
  "Return a plain predicate that corresponds to component-predicate PRED.
PRED and FINAL-PRED correspond to their namesakes in
`bookmarkp-sort-comparer' (which see).

PRED should return `(t)', `(nil)', or nil.

Optional arg FINAL-PRED is the final predicate to use if PRED cannot
decide (returns nil).  If FINAL-PRED is nil, then `bookmarkp-alpha-p',
the plain-predicate equivalent of `bookmarkp-alpha-cp' is used as the
final predicate."
  `(lambda (b1 b2)
    (let ((res  (funcall ',pred b1 b2)))
      (if res
          (car res)
        (funcall ',(or final-pred 'bookmarkp-alpha-p) b1 b2)))))

;;; If you need this for some reason, uncomment it.
;;; (defun bookmarkp-fix-bookmark-alist-and-save ()
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
;;;              "This will modify your bookmarks file, after backing it up.  OK? "))
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
;;;                              (setcar fn-tail (cons 'filename bookmarkp-non-file-filename)))
;;;                             ((and (eq hdlr 'bookmarkp-jump-gnus)
;;;                                   (not (assoc 'filename bmk)))
;;;                              (setcdr bmk (cons (cons 'filename bookmarkp-non-file-filename)
;;;                                                (cdr bmk)))))))
;;;                   t)                    ; Be sure `dolist' exit with t to allow saving.
;;;               (error (error "No changes made. %s" (error-message-string err))))
;;;         (bookmark-save)
;;;         (message "Bookmarks file fixed.  Old version is `%s'" bkup-file)))))


;;(@* "Bookmark Entry Access Functions")
;;  *** Bookmark Entry Access Functions ***

(defun bookmarkp-get-buffer-name (bookmark)
  "Return the `buffer-name' value for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (bookmark-prop-get bookmark 'buffer-name))

(defun bookmarkp-get-end-position (bookmark)
  "Return the `end-position' value for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (bookmark-prop-get bookmark 'end-position))

(defun bookmarkp-get-visits-count (bookmark)
  "Return the `visits' count for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (bookmark-prop-get bookmark 'visits))

(defun bookmarkp-get-visit-time (bookmark)
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

(defun bookmarkp-sort-and-remove-dups (list)
  "Return a copy of LIST, sorted and with no duplicate keys.
Only the first element with a given key is kept.
Keys are compared using `equal'.
Sorting is done using using `bookmarkp-sort-comparer'.
Do nothing if `bookmarkp-sort-comparer' is nil.
If `bookmarkp-reverse-sort-p' is non-nil, then reverse the sort order."
  (let ((newlist  (bookmarkp-remove-assoc-dups list))
        (sort-fn  (and bookmarkp-sort-comparer
                       (if (and (not (functionp bookmarkp-sort-comparer))
                                (consp bookmarkp-sort-comparer))
                           'bookmarkp-multi-sort
                         bookmarkp-sort-comparer))))
    (when sort-fn
      (setq newlist  (sort newlist (if bookmarkp-reverse-sort-p
                                       (lambda (a b) (not (funcall sort-fn a b)))
                                     sort-fn))))
    (setq bookmarkp-latest-sorted-alist  newlist)))

;;; KEEP this simpler version also.  This uses `run-hook-with-args-until-success', but it
;;; does not respect `bookmarkp-reverse-multi-sort-p'.
;;; (defun bookmarkp-multi-sort (b1 b2)
;;;   "Try predicates in `bookmarkp-sort-comparer', in order, until one decides.
;;; See the description of `bookmarkp-sort-comparer'."
;;;   (let* ((preds   (append (car bookmarkp-sort-comparer) (cdr bookmarkp-sort-comparer)))
;;;          (result  (run-hook-with-args-until-success 'preds b1 b2)))
;;;     (if (consp result)
;;;         (car result)
;;;       result)))

;; This Lisp definition respects `bookmarkp-reverse-multi-sort-p', and can be extended.
(defun bookmarkp-multi-sort (b1 b2)
  "Try predicates in `bookmarkp-sort-comparer', in order, until one decides.
See the description of `bookmarkp-sort-comparer'.
If `bookmarkp-reverse-multi-sort-p' is non-nil, then reverse the order
for using multi-sorting predicates."
  (let ((preds       (car bookmarkp-sort-comparer))
        (final-pred  (cadr bookmarkp-sort-comparer))
        (result      nil))
    (when bookmarkp-reverse-multi-sort-p (setq preds  (reverse preds)))
    (catch 'bookmarkp-multi-sort
      (dolist (pred  preds)
        (setq result  (funcall pred b1 b2))
        (when (consp result)
          (when bookmarkp-reverse-multi-sort-p (setq result  (list (not (car result)))))
          (throw 'bookmarkp-multi-sort (car result))))
      (and final-pred  (if bookmarkp-reverse-multi-sort-p
                           (not (funcall final-pred b1 b2))
                         (funcall final-pred b1 b2))))))

;; The message is only approximate.  The effect of `bookmarkp-reverse-multi-sort-p' is not
;; always intuitive, but it can often be useful.  What's not always intuitive is the placement
;; (the order) of bookmarks that are not sorted by the PREDs.
;; 
(defun bookmarkp-msg-about-sort-order (order &optional prefix-msg)
  "Display a message mentioning the current sort ORDER and direction.
Optional arg PREFIX-MSG is prepended to the constructed message, and
terminated with a period."
  (let ((msg
         (if (not bookmarkp-sort-comparer)
             "Bookmarks not sorted"
           (format
            "%s%s" (concat "Sorted " order)
            (if (not (and (consp bookmarkp-sort-comparer) ; Ordinary single predicate.
                          (consp (car bookmarkp-sort-comparer))))
                (if bookmarkp-reverse-sort-p "; reversed" "")
              (if (not (cadr (car bookmarkp-sort-comparer)))
                  ;; Single PRED.
                  (if (or (and bookmarkp-reverse-sort-p (not bookmarkp-reverse-multi-sort-p))
                          (and bookmarkp-reverse-multi-sort-p (not bookmarkp-reverse-sort-p)))
                      "; reversed"
                    "")

                ;; In case we want to distinguish:
                ;; (if (and bookmarkp-reverse-sort-p (not bookmarkp-reverse-multi-sort-p))
                ;;     "; reversed"
                ;;   (if (and bookmarkp-reverse-multi-sort-p (not bookmarkp-reverse-sort-p))
                ;;       "; reversed +"
                ;;     ""))

                ;; At least two PREDs.
                (cond ((and bookmarkp-reverse-sort-p (not bookmarkp-reverse-multi-sort-p))
                       "; reversed")
                      ((and bookmarkp-reverse-multi-sort-p (not bookmarkp-reverse-sort-p))
                       "; each predicate group reversed")
                      ((and bookmarkp-reverse-multi-sort-p bookmarkp-reverse-sort-p)
                       "; order of predicate groups reversed")
                      (t ""))))))))
    (when prefix-msg (setq msg  (concat prefix-msg ".  " msg)))
    (message msg)))
               

;;(@* "Sorting - Commands")
;;  *** Sorting - Commands ***

(defun bookmarkp-repeat-command (command)
  "Repeat COMMAND."
 (let ((repeat-previous-repeated-command  command)
       (repeat-message-function           'ignore)
       (last-repeatable-command           'repeat))
   (repeat nil)))

(defun bookmarkp-bmenu-change-sort-order-repeat (arg) ; `s s'... in menu list
  "Cycle to the next sort order.
With a prefix arg, reverse current sort order.
This is a repeatable version of `bookmarkp-bmenu-change-sort-order'."
  (interactive "P")
  (require 'repeat)
  (bookmarkp-repeat-command 'bookmarkp-bmenu-change-sort-order))

(defun bookmarkp-bmenu-change-sort-order (&optional arg)
  "Cycle to the next sort order.
With a prefix arg, reverse the current sort order."
  (interactive "P")
  (bookmarkp-barf-if-not-in-menu-list)
  (setq bookmarkp-sort-orders-for-cycling-alist
        (delq nil bookmarkp-sort-orders-for-cycling-alist))
  (if arg
      (bookmarkp-reverse-sort-order)
    (let ((current-bmk  (bookmark-bmenu-bookmark))
          next-order)
      (let ((orders  (mapcar #'car bookmarkp-sort-orders-for-cycling-alist)))
        (setq next-order  (or (cadr (member (bookmarkp-current-sort-order) orders))
                              (car orders))
              bookmarkp-sort-comparer  (cdr (assoc next-order
                                                   bookmarkp-sort-orders-for-cycling-alist))))
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (bookmarkp-bmenu-goto-bookmark-named current-bmk) ; Put cursor back on the right line.
      (when (interactive-p) (bookmarkp-msg-about-sort-order next-order)))))

(defun bookmarkp-current-sort-order ()
  "Current sort order or sort function, as a string suitable in a message."
  (or (car (rassoc bookmarkp-sort-comparer bookmarkp-sort-orders-alist))
      (format "%s" bookmarkp-sort-comparer)))

(defun bookmarkp-reverse-sort-order ()  ; `s r' in menu list
  "Reverse the current sort order.
If you combine this with \\<bookmark-bmenu-mode-map>\
`\\[bookmarkp-reverse-multi-sort-order]', then see the doc for that command."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (setq bookmarkp-reverse-sort-p  (not bookmarkp-reverse-sort-p))
  (let ((current-bmk  (bookmark-bmenu-bookmark)))
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (bookmarkp-bmenu-goto-bookmark-named current-bmk)) ; Put cursor back on the right line.
  (when (interactive-p) (bookmarkp-msg-about-sort-order (bookmarkp-current-sort-order))))

(defun bookmarkp-reverse-multi-sort-order () ; `s C-r' in menu list
  "Reverse the application of multi-sorting predicates.
These are the PRED predicates described for option
`bookmark-sort-function'.

This reverses the order in which the predicates are tried, and it
also complements the truth value returned by each predicate.

For example, if the list of multi-sorting predicates is (p1 p2 p3),
then the predicates are tried in the order: p3, p2, p1.  And if a
predicate returns true, `(t)', then the effect is as if it returned
false, `(nil)', and vice versa.

The use of multi-sorting predicates tends to group bookmarks, with the
first predicate corresponding to the first bookmark group etc.

The effect of \\<bookmark-bmenu-mode-map>`\\[bookmarkp-reverse-multi-sort-order]' is \
roughly as follows:

 - without also `\\[bookmarkp-reverse-sort-order]', it reverses the bookmark order in each \
group

 - combined with `\\[bookmarkp-reverse-sort-order]', it reverses the order of the bookmark
   groups, but not the bookmarks within a group

This is a rough description.  The actual behavior can be complex,
because of how each predicate is defined.  If this description helps
you, fine.  If not, just experiment and see what happens. \;-)

Remember that ordinary `\\[bookmarkp-reverse-sort-order]' reversal on its own is \
straightforward.
If you find `\\[bookmarkp-reverse-multi-sort-order]' confusing or not helpful, then do not \
use it."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (setq bookmarkp-reverse-multi-sort-p  (not bookmarkp-reverse-multi-sort-p))
  (let ((current-bmk  (bookmark-bmenu-bookmark)))
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (bookmarkp-bmenu-goto-bookmark-named current-bmk)) ; Put cursor back on the right line.
  (when (interactive-p) (bookmarkp-msg-about-sort-order (bookmarkp-current-sort-order))))


;; The ORDER of the macro calls here defines the REVERSE ORDER of
;; `bookmarkp-sort-orders-alist'.  The first here is thus also the DEFAULT sort order.
;; Entries are traversed by `s s'..., in `bookmarkp-sort-orders-alist' order.

(bookmarkp-define-sort-command          ; `s k' in menu list (`k' for "kind")
 "by bookmark type"                     ; `bookmarkp-bmenu-sort-by-bookmark-type'
 ((bookmarkp-info-cp bookmarkp-gnus-cp bookmarkp-w3m-cp bookmarkp-local-file-type-cp)
  bookmarkp-alpha-p)
 "Sort bookmarks by type: Info, Gnus, W3M, files, other.")

(bookmarkp-define-sort-command          ; `s w' in menu list
 "by w3m url"                           ; `bookmarkp-bmenu-sort-by-w3m-url'
 ((bookmarkp-w3m-cp) bookmarkp-alpha-p)
 "Sort W3M bookmarks alphabetically by their URL/filename.
When two bookmarks are not comparable this way, compare them by
bookmark name.")

(bookmarkp-define-sort-command          ; `s g' in menu list
 "by gnus thread"                       ; `bookmarkp-bmenu-sort-by-gnus-thread'
 ((bookmarkp-gnus-cp) bookmarkp-alpha-p)
 "Sort Gnus bookmarks by group, then by article, then by message.
When two bookmarks are not comparable this way, compare them by
bookmark name.")

(bookmarkp-define-sort-command          ; `s i' in menu list
 "by Info location"                     ; `bookmarkp-bmenu-sort-by-Info-location'
 ((bookmarkp-info-cp) bookmarkp-alpha-p)
 "Sort Info bookmarks by file name, then node name, then position.
When two bookmarks are not comparable this way, compare them by
bookmark name.")

(bookmarkp-define-sort-command          ; `s f u' in menu list
 "by last local file update"            ; `bookmarkp-bmenu-sort-by-last-local-file-update'
 ((bookmarkp-local-file-updated-more-recently-cp) bookmarkp-alpha-p)
 "Sort bookmarks by last local file update time.
Sort a local file before a remote file, and a remote file before other
bookmarks.  Otherwise, sort by bookmark name.")

(bookmarkp-define-sort-command          ; `s f t' in menu list
 "by last local file access"            ; `bookmarkp-bmenu-sort-by-last-local-file-access'
 ((bookmarkp-local-file-accessed-more-recently-cp) bookmarkp-alpha-p)
 "Sort bookmarks by last local file access time.
A local file sorts before a remote file, which sorts before other
bookmarks.  Otherwise, sort by bookmark name.")

(bookmarkp-define-sort-command          ; `s f s' in menu list
 "by local file size"                   ; `bookmarkp-bmenu-sort-by-local-file-size'
 ((bookmarkp-local-file-size-cp) bookmarkp-alpha-p)
 "Sort bookmarks by local file size.
A local file sorts before a remote file, which sorts before other
bookmarks.  Otherwise, sort by bookmark name.")

(bookmarkp-define-sort-command          ; `s f n' in menu list
 "by file name"                         ; `bookmarkp-bmenu-sort-by-file-name'
 ((bookmarkp-file-alpha-cp) bookmarkp-alpha-p)
 "Sort bookmarks by file name.
When two bookmarks are not comparable by file name, compare them by
bookmark name.")

(bookmarkp-define-sort-command          ; `s f d' in menu list (`d' for "directory")
 "by local file type"                   ; `bookmarkp-bmenu-sort-by-local-file-type'
 ((bookmarkp-local-file-type-cp) bookmarkp-alpha-p)
 "Sort bookmarks by local file type: file, symlink, directory.
A local file sorts before a remote file, which sorts before other
bookmarks.  Otherwise, sort by bookmark name.")

(bookmarkp-define-sort-command          ; `s >' in menu list
 "marked before unmarked"               ; `bookmarkp-bmenu-sort-marked-before-unmarked'
 ((bookmarkp-marked-cp) bookmarkp-alpha-p)
 "Sort bookmarks by putting marked before unmarked.
Otherwise alphabetize by bookmark name.")

(bookmarkp-define-sort-command          ; `s b' in menu list
 "by last buffer or file access"        ; `bookmarkp-bmenu-sort-by-last-buffer-or-file-access'
 ((bookmarkp-buffer-last-access-cp bookmarkp-local-file-accessed-more-recently-cp)
  bookmarkp-alpha-p)
 "Sort bookmarks by last buffer access or last local file access.
Sort a bookmark accessed more recently before one accessed less
recently or not accessed.  Sort a bookmark to an existing buffer
before a local file bookmark.  When two bookmarks are not comparable
by such critera, sort them by bookmark name.  (In particular, sort
remote-file bookmarks by bookmark name.")

(bookmarkp-define-sort-command          ; `s v' in menu list
 "by bookmark visit frequency"          ; `bookmarkp-bmenu-sort-by-bookmark-visit-frequency'
 ((bookmarkp-visited-more-cp) bookmarkp-alpha-p)
 "Sort bookmarks by the number of times they were visited as bookmarks.
When two bookmarks are not comparable by visit frequency, compare them
by bookmark name.")

(bookmarkp-define-sort-command          ; `s t' in menu list
 "by last bookmark access"              ; `bookmarkp-bmenu-sort-by-last-bookmark-access'
 ((bookmarkp-bookmark-last-access-cp) bookmarkp-alpha-p)
 "Sort bookmarks by the time of their last visit as bookmarks.
When two bookmarks are not comparable by visit time, compare them
by bookmark name.")

(bookmarkp-define-sort-command          ; `s n' in menu list
 "by bookmark name"                     ; `bookmarkp-bmenu-sort-by-bookmark-name'
 bookmarkp-alpha-p
 "Sort bookmarks by bookmark name, respecting `case-fold-search'.")


;; These definitions MUST COME AFTER the calls to macro `bookmarkp-define-sort-command'.
;; Otherwise, they won't pick up a populated `bookmarkp-sort-orders-alist'.
(when (> emacs-major-version 20)
  (defcustom bookmarkp-sort-orders-for-cycling-alist (copy-sequence
                                                      bookmarkp-sort-orders-alist)
    "*Alist of sort orders used for cycling via `s s'...
This is a subset of the complete list of available sort orders,
`bookmarkp-sort-orders-alist'.  This lets you cycle among fewer sort
orders, if there are some that you do not use often.

See the doc for `bookmarkp-sort-orders-alist', for the structure of
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
    :group 'bookmarkp))

(unless (> emacs-major-version 20)      ; Emacs 20: custom type `alist' doesn't exist.
  (defcustom bookmarkp-sort-orders-for-cycling-alist (copy-sequence
                                                      bookmarkp-sort-orders-alist)
    "*Alist of sort orders used for cycling via `s s'...
This is a subset of the complete list of available sort orders,
`bookmarkp-sort-orders-alist'.  This lets you cycle among fewer sort
orders, if there are some that you do not use often.

See the doc for `bookmarkp-sort-orders-alist', for the structure of
this value."
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
    :group 'bookmarkp))


;;(@* "Sorting - General Predicates")
;;  *** Sorting - General Predicates ***

(defun bookmarkp-marked-cp (b1 b2)
  "True if bookmark B1 is marked and bookmark B2 is not.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if incomparable as described."
  (let ((m1  (bookmarkp-bookmark-marked-p b1))
        (m2  (bookmarkp-bookmark-marked-p b2)))
    (cond ((and m1 m2) nil)
          (m1 '(t))
          (m2 '(nil))
          (t nil))))

(defun bookmarkp-visited-more-cp (b1 b2)
  "True if bookmark B1 was visited more often than B2.
True also if B1 was visited but B2 was not.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if incomparable as described."
  (let ((v1  (bookmarkp-get-visits-count b1))
        (v2  (bookmarkp-get-visits-count b2)))
    (cond ((and v1 v2)
           (cond ((> v1 v2) '(t))
                 ((> v2 v1) '(nil))
                 (t nil)))
          (v1 '(t))
          (v2 '(nil))
          (t nil))))

(defun bookmarkp-bookmark-last-access-cp (b1 b2)
  "True if bookmark B1 was visited more recently than B2.
True also if B1 was visited but B2 was not.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if incomparable as described."
  (let ((t1  (bookmarkp-get-visit-time b1))
        (t2  (bookmarkp-get-visit-time b2)))
    (cond ((and t1 t2)
           (setq t1  (bookmarkp-float-time t1)
                 t2  (bookmarkp-float-time t2))
           (cond ((> t1 t2) '(t))
                 ((> t2 t1) '(nil))
                 (t nil)))
          (t1 '(t))
          (t2 '(nil))
          (t nil))))

(defun bookmarkp-buffer-last-access-cp (b1 b2)
  "True if bookmark B1's buffer or file was visited more recently than B2's.
A bookmark to an existing buffer sorts before a file bookmark, even if
the buffer has not been visited during this session.

True also if B1 has a buffer but B2 does not.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if incomparable as described."
  (let ((buf1  (bookmarkp-get-buffer-name b1))
        (buf2  (bookmarkp-get-buffer-name b2))
        f1 f2 t1 t2)
    (setq buf1  (and buf1 (get-buffer buf1))
          buf2  (and buf2 (get-buffer buf2)))
    (cond ((and buf1 buf2)              ; Both buffers exist.   See whether they were accessed.
           (when buf1 (setq buf1  (member buf1 (buffer-list))
                            buf1  (length buf1)))
           (when buf2 (setq buf2  (member buf2 (buffer-list))
                            buf2  (length buf2)))
           (cond ((and buf1 buf2)       ; Both were accessed.  Priority to most recent access.
                  (cond ((< buf1 buf2) '(t))
                        ((< buf2 buf1) '(nil))
                        (t nil)))
                 (buf1 '(t))            ; Only buf1 was accessed.
                 (buf2 '(nil))          ; Only buf2 was accessed.
                 (t nil)))              ; Neither was accessed.

          (buf1 '(t))                   ; Only buf1 exists.
          (buf2 '(nil))                 ; Only buf2 exists.
          (t nil))))                    ; Neither buffer exists

(defun bookmarkp-info-cp (b1 b2)
  "True if bookmark B1 sorts as an Info bookmark before B2.
Two Info bookmarks are compared first by file name (corresponding to
the manual), then by node name, then by position.
True also if B1 is an Info bookmark but B2 is not.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (let ((i1  (bookmarkp-info-bookmark-p b1))
        (i2  (bookmarkp-info-bookmark-p b2)))
    (cond ((and i1 i2)
           (setq i1  (abbreviate-file-name (bookmark-get-filename b1))
                 i2  (abbreviate-file-name (bookmark-get-filename b2)))
           (when case-fold-search
             (setq i1  (bookmarkp-upcase i1)
                   i2  (bookmarkp-upcase i2)))
           (cond ((string-lessp i1 i2) '(t)) ; Compare manuals (file names).
                 ((string-lessp i2 i1) '(nil))
                 (t                     ; Compare node names.
                  (setq i1  (bookmark-prop-get b1 'info-node)
                        i2  (bookmark-prop-get b2 'info-node))
                  (cond ((string-lessp i1 i2) '(t))
                        ((string-lessp i2 i1) '(nil))
                        (t
                         (setq i1  (bookmark-get-position b1)
                               i2  (bookmark-get-position b2))
                         (cond ((or (not i1) (not i2)) '(t)) ; Fallback if no `position' entry.
                               ((<= i1 i2) '(t))
                               ((< i2 i1) '(nil))))))))
          (i1 '(t))
          (i2 '(nil))
          (t nil))))

(defun bookmarkp-gnus-cp (b1 b2)
  "True if bookmark B1 sorts as a Gnus bookmark before B2.
Two Gnus bookmarks are compared first by Gnus group name, then by
article number, then by message ID.
True also if B1 is a Gnus bookmark but B2 is not.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (let ((g1  (bookmarkp-gnus-bookmark-p b1))
        (g2  (bookmarkp-gnus-bookmark-p b2)))
    (cond ((and g1 g2)
           (setq g1  (bookmark-prop-get b1 'group)
                 g2  (bookmark-prop-get b2 'group))
           (cond ((string-lessp g1 g2) '(t)) ; Compare groups.
                 ((string-lessp g2 g1) '(nil))
                 (t                     ; Compare article numbers.
                  (setq g1  (bookmark-prop-get b1 'article)
                        g2  (bookmark-prop-get b2 'article))
                  (cond ((< g1 g2) '(t))
                        ((< g2 g1) '(nil))
                        (t
                         (setq g1  (bookmark-prop-get b1 'message-id)
                               g2  (bookmark-prop-get b2 'message-id))
                         (cond ((string-lessp g1 g2) '(t)) ; Compare message IDs.
                               ((string-lessp g2 g1) '(nil))
                               (t nil)))))))   
          (g1 '(t))
          (g2 '(nil))
          (t nil))))

(defun bookmarkp-w3m-cp (b1 b2)
  "True if bookmark B1 sorts as a W3M URL bookmark before B2.
Two W3M URL bookmarks are compared alphabetically, by their URLs.
True also if B1 is a Gnus bookmark but B2 is not.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (let ((w1  (bookmarkp-w3m-bookmark-p b1))
        (w2  (bookmarkp-w3m-bookmark-p b2)))
    (cond ((and w1 w2)
           (setq w1  (bookmark-get-filename b1)
                 w2  (bookmark-get-filename b2))
           (cond ((string-lessp w1 w2) '(t))
                 ((string-lessp w2 w1) '(nil))
                 (t nil)))
          (w1 '(t))
          (w2 '(nil))
          (t nil))))

(defun bookmarkp-alpha-cp (b1 b2)
  "True if bookmark B1's name sorts alphabetically before B2's.
The bookmark names are compared, respecting `case-fold-search'.
Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (let ((s1  (car b1))
        (s2  (car b2)))
    (when case-fold-search
      (setq s1  (bookmarkp-upcase s1)
            s2  (bookmarkp-upcase s2)))
    (cond ((string-lessp s1 s2) '(t))
          ((string-lessp s2 s1) '(nil))
          (t nil))))

(defalias 'bookmarkp-alpha-p (bookmarkp-make-plain-predicate 'bookmarkp-alpha-cp))


;;(@* "Sorting - File-Name Predicates")
;;  *** Sorting - File-Name Predicates ***

(defun bookmarkp-file-alpha-cp (b1 b2)
  "True if bookmark B1's file name sorts alphabetically before B2's.
The file names are shortened using `abbreviate-file-name', then they
are compared respecting `case-fold-search'.

Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (let ((f1  (bookmarkp-file-bookmark-p b1))
        (f2  (bookmarkp-file-bookmark-p b2)))
    (cond ((and f1 f2)
           ;; Call `abbreviate-file-name' mainly to get letter case right per platform.
           (setq f1  (abbreviate-file-name (bookmark-get-filename b1))
                 f2  (abbreviate-file-name (bookmark-get-filename b2)))
           (when case-fold-search
             (setq f1  (bookmarkp-upcase f1)
                   f2  (bookmarkp-upcase f2)))
           (cond ((string-lessp f1 f2) '(t))
                 ((string-lessp f2 f1) '(nil))
                 (t nil)))
          (f1 '(t))
          (f2 '(nil))
          (t nil))))

;; We define all file-attribute predicates, in case you want to use them.
;;
;; `bookmarkp-file-attribute-0-cp'  - type
;; `bookmarkp-file-attribute-1-cp'  - links
;; `bookmarkp-file-attribute-2-cp'  - uid
;; `bookmarkp-file-attribute-3-cp'  - gid
;; `bookmarkp-file-attribute-4-cp'  - last access time
;; `bookmarkp-file-attribute-5-cp'  - last update time
;; `bookmarkp-file-attribute-6-cp'  - last status change
;; `bookmarkp-file-attribute-7-cp'  - size
;; `bookmarkp-file-attribute-8-cp'  - modes
;; `bookmarkp-file-attribute-9-cp'  - gid change
;; `bookmarkp-file-attribute-10-cp' - inode
;; `bookmarkp-file-attribute-11-cp' - device
;;
(bookmarkp-define-file-sort-predicate 0) ; Type: file, symlink, dir
(bookmarkp-define-file-sort-predicate 1) ; Links
(bookmarkp-define-file-sort-predicate 2) ; Uid
(bookmarkp-define-file-sort-predicate 3) ; Gid
(bookmarkp-define-file-sort-predicate 4) ; Last access time
(bookmarkp-define-file-sort-predicate 5) ; Last modification time
(bookmarkp-define-file-sort-predicate 6) ; Last status-change time
(bookmarkp-define-file-sort-predicate 7) ; Size
(bookmarkp-define-file-sort-predicate 8) ; Modes
(bookmarkp-define-file-sort-predicate 9) ; Gid would change if re-created
(bookmarkp-define-file-sort-predicate 10) ; Inode
(bookmarkp-define-file-sort-predicate 11) ; Device

(defun bookmarkp-local-file-accessed-more-recently-cp (b1 b2)
  "True if bookmark B1's local file was accessed more recently than B2's.
A local file sorts before a remote file, which sorts before other
bookmarks.  Two remote files are considered incomparable - their
access times are not examined.

Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (cond ((and (bookmarkp-local-file-bookmark-p b1) (bookmarkp-local-file-bookmark-p b2))
         (bookmarkp-cp-not (bookmarkp-file-attribute-4-cp b1 b2)))
        ((bookmarkp-local-file-bookmark-p b1) '(t))
        ((bookmarkp-local-file-bookmark-p b2) '(nil))
        ((and (bookmarkp-remote-file-bookmark-p b1) (bookmarkp-remote-file-bookmark-p b2)) nil)
        ((bookmarkp-remote-file-bookmark-p b1) '(t))
        ((bookmarkp-remote-file-bookmark-p b2) '(nil))
        (t nil)))

(defun bookmarkp-local-file-updated-more-recently-cp (b1 b2)
  "True if bookmark B1's local file was updated more recently than B2's.
A local file sorts before a remote file, which sorts before other
bookmarks.  Two remote files are considered incomparable - their
update times are not examined.

Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (cond ((and (bookmarkp-local-file-bookmark-p b1) (bookmarkp-local-file-bookmark-p b2))
         (bookmarkp-cp-not (bookmarkp-file-attribute-5-cp b1 b2)))
        ((bookmarkp-local-file-bookmark-p b1) '(t))
        ((bookmarkp-local-file-bookmark-p b2) '(nil))
        ((and (bookmarkp-remote-file-bookmark-p b1) (bookmarkp-remote-file-bookmark-p b2)) nil)
        ((bookmarkp-remote-file-bookmark-p b1) '(t))
        ((bookmarkp-remote-file-bookmark-p b2) '(nil))
        (t nil)))

(defun bookmarkp-local-file-size-cp (b1 b2)
  "True if bookmark B1's local file is larger than B2's.
A local file sorts before a remote file, which sorts before other
bookmarks.  Two remote files are considered incomparable - their
sizes are not examined.

Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (cond ((and (bookmarkp-local-file-bookmark-p b1) (bookmarkp-local-file-bookmark-p b2))
         (bookmarkp-cp-not (bookmarkp-file-attribute-7-cp b1 b2)))
        ((bookmarkp-local-file-bookmark-p b1) '(t))
        ((bookmarkp-local-file-bookmark-p b2) '(nil))
        ((and (bookmarkp-remote-file-bookmark-p b1) (bookmarkp-remote-file-bookmark-p b2)) nil)
        ((bookmarkp-remote-file-bookmark-p b1) '(t))
        ((bookmarkp-remote-file-bookmark-p b2) '(nil))
        (t nil)))

(defun bookmarkp-local-file-type-cp (b1 b2)
  "True if bookmark B1 sorts by local file type before B2.
For two local files, a file sorts before a symlink, which sorts before
a directory.

A local file sorts before a remote file, which sorts before other
bookmarks.  Two remote files are considered incomparable - their file
types are not examined.

Reverse the roles of B1 and B2 for a false value.
A true value is returned as `(t)', a false value as `(nil)'.
Return nil if neither sorts before the other."
  (cond ((and (bookmarkp-local-file-bookmark-p b1) (bookmarkp-local-file-bookmark-p b2))
         (bookmarkp-file-attribute-0-cp b1 b2))
        ((bookmarkp-local-file-bookmark-p b1) '(t))
        ((bookmarkp-local-file-bookmark-p b2) '(nil))
        ((and (bookmarkp-remote-file-bookmark-p b1) (bookmarkp-remote-file-bookmark-p b2)) nil)
        ((bookmarkp-remote-file-bookmark-p b1) '(t))
        ((bookmarkp-remote-file-bookmark-p b2) '(nil))
        (t nil)))

(defun bookmarkp-cp-not (truth)
  "Return the negation of boolean value TRUTH.
If TRUTH is (t), return (nil), and vice versa.
If TRUTH is nil, return nil."
  (and truth (if (car truth) '(nil) '(t))))


;;(@* "Other Bookmark+ Functions (`bookmarkp-*')")
;;  *** Other Bookmark+ Functions (`bookmarkp-*') ***

;;;###autoload
(defun bookmarkp-bmenu-describe-this-bookmark ()
  "Describe bookmark of current line."
  (interactive)
  (bookmarkp-barf-if-not-in-menu-list)
  (bookmarkp-describe-bookmark (bookmark-bmenu-bookmark)))

;;;###autoload
(defun bookmarkp-describe-bookmark (bookmark)
  "Describe BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (interactive
   (list (bookmark-completing-read "Describe bookmark" (bookmarkp-default-bookmark-name))))
  (setq bookmark  (bookmark-get-bookmark bookmark))
  (let ((bname     (bookmark-name-from-full-record bookmark))
        (buf       (bookmarkp-get-buffer-name bookmark))
        (file      (bookmark-get-filename bookmark))
        (start     (bookmark-get-position bookmark))
        (end       (bookmarkp-get-end-position bookmark))
        (time      (bookmarkp-get-visit-time bookmark))
        (visits    (bookmarkp-get-visits-count bookmark))
        (tags      (bookmarkp-get-tags bookmark))
        (diredp    (bookmarkp-dired-bookmark-p bookmark))
        (gnusp     (bookmarkp-gnus-bookmark-p bookmark))
        (infop     (bookmarkp-info-bookmark-p bookmark))
        (w3mp      (bookmarkp-w3m-bookmark-p bookmark))
        (tags      (bookmarkp-get-tags bookmark))
        (annot     (bookmark-get-annotation bookmark)))

    (help-setup-xref (list #'bookmarkp-describe-bookmark bookmark) (interactive-p))
    (let* ((help-text
            (concat
             (format "%s\n%s\n\n" bname (make-string (length bname) ?-))
             (cond (diredp
                    (let ((switches  (bookmark-prop-get bookmark 'dired-switches)))
                      (format "Dired%s:\t\t%s\n"
                              (if switches (format " `%s'" switches) "")
                              (expand-file-name file))))
                   (gnusp  (format "Gnus, group:\t%s, article: %s, message-id: %s\n"
                                   (bookmark-prop-get bookmark 'group)
                                   (bookmark-prop-get bookmark 'article)
                                   (bookmark-prop-get bookmark 'message-id)))
                   (infop  (format
                            "Info node:\t(%s) %s\n"
                            (file-name-nondirectory (bookmark-get-filename bookmark))
                            (bookmark-prop-get bookmark 'info-node)))
                   (w3mp   (format "W3M URL:\t%s\n" (bookmark-get-filename bookmark)))
                   ((equal file bookmarkp-non-file-filename)
                    (format "Buffer:\t\t%s\n" (bookmarkp-get-buffer-name bookmark)))
                   (file   (format "File:\t\t%s\n" (expand-file-name file)))
                   (t      "Unknown\n"))
             (if (bookmarkp-region-bookmark-p bookmark)
                 (format "Region:\t\t%d to %d (%d chars)\n" start end (- end start))
               (format "Position:\t%d\n" start))
             (if visits (format "Visits:\t\t%d\n" visits) "")
             (if time (format "Last visit:\t%s\n" (format-time-string "%c" time)) "")
             (if tags (format "Tags:\t\t%S\n" tags) "")
             (if annot (format "\nAnnotation:\n%s" annot)))))
      (with-output-to-temp-buffer "*Help*" (princ help-text))
      help-text)))

(defun bookmarkp-root-or-sudo-logged-p ()
  "Return t if the user logged in using Tramp as `root' or `sudo'.
Otherwise, return nil."
  (catch 'break
    (dolist (ii  (mapcar #'buffer-name (buffer-list)))
      (when (string-match "*tramp/\\(su\\|sudo\\) ." ii) (throw 'break t)))))

(defun bookmarkp-position-post-context (breg)
  "Return `bookmark-search-size' chars, starting at position BREG.
Return nil if there are not that many chars.
This is text that follows the bookmark's `position'.
This is used for a non-region bookmark."
  (and (>= (- (point-max) breg) bookmark-search-size)
       (buffer-substring-no-properties breg (+ breg bookmark-search-size))))

(defun bookmarkp-position-post-context-region (breg ereg)
  "Return the region prefix, at BREG.
Return at most `bookmarkp-region-search-size' or (- EREG BREG) chars.
This is text that follows the bookmark's `position'.
This is used for a region bookmark."
  (buffer-substring-no-properties
   breg (+ breg (min bookmarkp-region-search-size (- ereg breg)))))

(defun bookmarkp-position-pre-context (breg)
  "Return `bookmark-search-size' chars that precede BREG.
Return nil if there are not that many chars.
This is text that precedes the bookmark's `position'.
This is used for a non-region bookmark."
  (and (>= (- breg (point-min)) bookmark-search-size)
       (buffer-substring-no-properties breg (- breg bookmark-search-size))))

(defun bookmarkp-position-pre-context-region (breg)
  "Return the text preceding the region beginning, BREG.
Return at most `bookmarkp-region-search-size' chars.
This is text that precedes the bookmark's `position'.
This is used for a region bookmark."
  (buffer-substring-no-properties
   (max (- breg bookmarkp-region-search-size) (point-min))
   breg))

(defun bookmarkp-end-position-pre-context (breg ereg)
  "Return the region suffix, ending at EREG.
Return at most `bookmarkp-region-search-size' or (- EREG BREG) chars.
This is text that precedes the bookmark's `end-position'."
  (buffer-substring-no-properties
   (- ereg (min bookmarkp-region-search-size (- ereg breg)))
   ereg))

(defun bookmarkp-end-position-post-context (ereg)
  "Return the text following the region end, EREG.
Return at most `bookmarkp-region-search-size' chars.
This is text that follows the bookmark's `end-position'."
  (buffer-substring-no-properties
   ereg (+ ereg (min bookmarkp-region-search-size (- (point-max) (point))))))

(defun bookmarkp-position-after-whitespace (position)
  "Move forward from POSITION, skipping over whitespace.  Return point."
  (goto-char position)
  (skip-chars-forward " \n\t" (point-max))
  (point))

(defun bookmarkp-position-before-whitespace (position)
  "Move backward from POSITION, skipping over whitespace.  Return point."
  (goto-char position)
  (skip-chars-backward " \n\t" (point-min))
  (point))

(defun bookmarkp-save-new-region-location (bookmark beg end)
  "Update and save `bookmark-alist' for BOOKMARK, relocating its region.
BOOKMARK is a bookmark record.
BEG and END are the new region limits for BOOKMARK.
Do nothing and return nil if `bookmarkp-save-new-location-flag' is nil.
Otherwise, return non-nil if region was relocated."
  (and bookmarkp-save-new-location-flag
       (y-or-n-p "Region relocated.  Do you want to save new region limits? ")
       (progn
         (bookmark-prop-set bookmark 'front-context-string
                            (bookmarkp-position-post-context-region beg end))
         (bookmark-prop-set bookmark 'rear-context-string
                            (bookmarkp-position-pre-context-region beg))
         (bookmark-prop-set bookmark 'front-context-region-string
                            (bookmarkp-end-position-pre-context beg end))
         (bookmark-prop-set bookmark 'rear-context-region-string
                            (bookmarkp-end-position-post-context end))
         (bookmark-prop-set bookmark 'position beg)
         (bookmark-prop-set bookmark 'end-position end)
         (bookmarkp-maybe-save-bookmarks)
         t)))

(defun bookmarkp-handle-region-default (bookmark)
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
         (end-pos          (bookmarkp-get-end-position bmk))
         (reg-retrieved-p  t)
         (reg-relocated-p  nil))
    (unless (and (string= bor-str (buffer-substring-no-properties
                                   (point) (+ (point) (length bor-str))))
                 (save-excursion
                   (goto-char end-pos)
                   (string= eor-str (buffer-substring-no-properties
                                     (point) (- (point) (length bor-str))))))
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
                    end  (and end (bookmarkp-position-before-whitespace end))))))
        ;; If failed to find END, go to eob and search backward for BEG.
        (unless end (goto-char (point-max)))
        (if (search-backward bor-str (point-min) t) ; Find BEG, using `bor-str'.
            (setq beg  (point))
          ;; Verify that region is not after context.
          (unless (search-backward ar-str (point-min) t)
            (when (search-backward br-str (point-min) t) ; Find BEG, using `br-str'.
              (setq beg  (match-end 0)
                    beg  (and beg (bookmarkp-position-after-whitespace beg))))))
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
           (when bookmarkp-show-end-of-region
             (let ((end-win  (save-excursion (forward-line (window-height))
                                             (end-of-line) (point))))
               ;; Bounce point and mark.
               (save-excursion (sit-for 0.6) (exchange-point-and-mark) (sit-for 1))
               ;; Recenter if region end is not visible.
               (when (> end-pos end-win) (recenter 1))))
           ;; Maybe save region.
           (if (and reg-relocated-p (bookmarkp-save-new-region-location bmk pos end-pos))
               (message "Saved relocated region (from %d to %d)" pos end-pos)
             (message "Region is from %d to %d" pos end-pos)))
          (t                            ; No region.  Go to old start.  Don't push-mark.
           (goto-char pos) (forward-line 0)
           (message "No region from %d to %d" pos end-pos)))))

;; Same as `line-number-at-pos', which is not available until Emacs 22.
(defun bookmarkp-line-number-at-pos (&optional pos)
  "Buffer line number at position POS. Current line number if POS is nil.
Counting starts at (point-min), so any narrowing restriction applies."
  (1+ (count-lines (point-min) (save-excursion (when pos (goto-char pos))
                                               (forward-line 0) (point)))))

(defun bookmarkp-goto-position (file buf bufname pos forward-str behind-str)
  "Go to a bookmark that has no region.
Arguments are, respectively, the bookmark's file, buffer, buffer name,
position, and the context strings for the position."
  (if (and file (file-readable-p file) (not (buffer-live-p buf)))
      (with-current-buffer (find-file-noselect file) (setq buf  (buffer-name)))
    ;; No file found.  See if a non-file buffer exists for this.  If not, raise error.
    (unless (or (and buf (get-buffer buf))
                (and bufname (get-buffer bufname) (not (string= buf bufname))))
      (signal 'file-error `("Jumping to bookmark" "No such file or directory" file))))
  (set-buffer (or buf bufname))
  (save-current-buffer (funcall bookmarkp-jump-display-function (current-buffer)))
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
  (when (and behind-str (search-backward behind-str (point-min) t))
    (goto-char (match-end 0)))
  nil)                                  ; $$$$$ Why return nil?

;; W3M support
(defun bookmarkp-make-w3m-record ()
  "Make a special entry for w3m buffers."
  (require 'w3m)                        ; For `w3m-current-url'.
  `(,@(bookmark-make-record-default 'point-only)
    (filename . ,w3m-current-url)
    (handler . bookmarkp-jump-w3m)))

(add-hook 'w3m-mode-hook
          #'(lambda ()
              (set (make-local-variable 'bookmark-make-record-function)
                   'bookmarkp-make-w3m-record)))

(defun bookmarkp-w3m-set-new-buffer-name ()
  "Set the w3m buffer name according to the number of w3m buffers already open."
  (let ((len  (length (w3m-list-buffers 'nosort))))
    (if (eq len 0)  "*w3m*"  (format "*w3m*<%d>" (1+ len)))))

(defun bookmarkp-jump-w3m-new-session (bookmark)
  "Jump to W3M bookmark BOOKMARK, setting a new tab."
  (let ((buf   (bookmarkp-w3m-set-new-buffer-name)))
    (w3m-browse-url (bookmark-prop-get bookmark 'filename) 'newsession)
    (while (not (get-buffer buf)) (sit-for 1)) ; Be sure we have the W3M buffer.
    (with-current-buffer buf
      (goto-char (point-min))
      ;; Wait until data arrives in buffer, before setting region.
      (while (eq (line-beginning-position) (line-end-position)) (sit-for 1)))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

(defun bookmarkp-jump-w3m-only-one-tab (bookmark)
  "Close all W3M sessions and jump to BOOKMARK in a new W3M buffer."
  (w3m-quit 'force)                     ; Be sure we start with an empty W3M buffer.
  (w3m-browse-url (bookmark-prop-get bookmark 'filename))
  (with-current-buffer "*w3m*" (while (eq (point-min) (point-max)) (sit-for 1)))
  (bookmark-default-handler `("" (buffer . ,(buffer-name (current-buffer))) .
                              ,(bookmark-get-bookmark-record bookmark))))

(defun bookmarkp-jump-w3m (bookmark)
  "Handler function for record returned by `bookmarkp-make-w3m-record'.
BOOKMARK is a bookmark name or a bookmark record.
Use multi-tabs in W3M if `bookmarkp-w3m-allow-multi-tabs' is non-nil."
  (if bookmarkp-w3m-allow-multi-tabs
      (bookmarkp-jump-w3m-new-session bookmark)
    (bookmarkp-jump-w3m-only-one-tab bookmark)))

;; GNUS support.  Does not handle regions.
(defun bookmarkp-make-gnus-record ()
  "Make a bookmark entry for a Gnus buffer."
  (require 'gnus)
  (unless (and (eq major-mode 'gnus-summary-mode) gnus-article-current)
    (error "Please retry from the Gnus summary buffer")) ;[1]
  (let* ((grp   (car gnus-article-current))
         (art   (cdr gnus-article-current))
         (head  (gnus-summary-article-header art))
         (id    (mail-header-id head)))
    `(,@(bookmark-make-record-default 'point-only)
        (filename . ,bookmarkp-non-file-filename)
        (group . ,grp) (article . ,art)
        (message-id . ,id) (handler . bookmarkp-jump-gnus))))

(add-hook 'gnus-summary-mode-hook
          #'(lambda () (set (make-local-variable 'bookmark-make-record-function)
                            'bookmarkp-make-gnus-record)))

;; Raise an error if we try to bookmark from here [1]
(add-hook 'gnus-article-mode-hook
          #'(lambda () (set (make-local-variable 'bookmark-make-record-function)
                            'bookmarkp-make-gnus-record)))

(defun bookmarkp-jump-gnus (bookmark)
  "Handler function for record returned by `bookmarkp-make-gnus-record'.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((group    (bookmark-prop-get bookmark 'group))
        (article  (bookmark-prop-get bookmark 'article))
        (id       (bookmark-prop-get bookmark 'message-id))
        (buf      (bookmark-prop-get bookmark 'buffer)))
    (gnus-fetch-group group (list article))
    (gnus-summary-insert-cached-articles)
    (gnus-summary-goto-article id nil 'force)
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

(defun bookmarkp-make-dired-record ()
  "Create and return a Dired bookmark record."
  (let ((dir       (expand-file-name (if (consp dired-directory)
                                         (car dired-directory)
                                       dired-directory)))
        (mfiles    (dired-get-marked-files 'relative nil nil 'distinguish-one-marked)))
    (when (null (cadr mfiles)) (setq mfiles nil)) ; Don't count unmarked current file.
    `(,dir
      ,@(bookmark-make-record-default 'point-only)
      (filename . ,dir)
      (dired-marked . ,mfiles)
      (dired-switches . ,dired-actual-switches)
      (handler . bookmarkp-jump-dired))))
    
(add-hook 'dired-mode-hook
          #'(lambda () (set (make-local-variable 'bookmark-make-record-function)
                            'bookmarkp-make-dired-record)))

(defun bookmarkp-jump-dired (bookmark)
  "Jump to Dired bookmark BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
Handler function for record returned by `bookmarkp-make-dired-record'."
  (let ((dir       (bookmark-prop-get bookmark 'filename))
        (mfiles    (bookmark-prop-get bookmark 'dired-marked))
        (switches  (bookmark-prop-get bookmark 'dired-switches)))
    (dired dir switches)
    (let ((inhibit-read-only  t))
      (dired-mark-remembered (mapcar #'(lambda (mf) (cons (concat dir mf) 42)) mfiles)))
    (goto-char (bookmark-get-position bookmark))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'bookmark+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bookmark+.el ends here


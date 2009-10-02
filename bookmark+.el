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
;; Last-Updated: Thu Oct  1 15:17:37 2009 (-0700)
;;           By: dradams
;;     Update #: 3917
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
;;    (@> "How To Use Bookmark+")
;;    (@> "Bookmark Compatibility with Vanilla Emacs (`bookmark.el')")
;;    (@> "New Bookmark Structure")
;;    (@> "Bookmarks Menu List")
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
 
;;(@* "Things Defined Here")
;;
;;  Things Defined Here
;;  -------------------
;;
;;  Commands defined here:
;;
;;    `bookmarkp-bmenu-edit-bookmark', `bookmarkp-bmenu-mark-all',
;;    `bookmarkp-bmenu-quit', `bookmarkp-bmenu-refresh-menu-list',
;;    `bookmarkp-bmenu-regexp-mark', `bookmarkp-bmenu-show-all',
;;    `bookmarkp-bmenu-show-only-files',
;;    `bookmarkp-bmenu-show-only-gnus',
;;    `bookmarkp-bmenu-show-only-info-nodes',
;;    `bookmarkp-bmenu-show-only-non-files',
;;    `bookmarkp-bmenu-show-only-regions',
;;    `bookmarkp-bmenu-show-only-w3m-urls',
;;    `bookmarkp-bmenu-sort-alphabetically',
;;    `bookmarkp-bmenu-sort-by-last-visit-time',
;;    `bookmarkp-bmenu-sort-by-visit-frequency',
;;    `bookmarkp-bmenu-toggle-marks',
;;    `bookmarkp-bmenu-toggle-show-only-marked',
;;    `bookmarkp-bmenu-toggle-show-only-unmarked',
;;    `bookmarkp-bmenu-unmark-all', `bookmarkp-version',
;;    `old-bookmark-insert', `old-bookmark-insert-location',
;;    `old-bookmark-relocate'.
;;
;;  Commands redefined here (from `bookmark.el'):
;;
;;    `bookmark-bmenu-execute-deletions', `bookmark-bmenu-list',
;;    `bookmark-bmenu-mark', `bookmark-bmenu-other-window',
;;    `bookmark-bmenu-rename', `bookmark-bmenu-unmark',
;;    `bookmark-delete', `bookmark-insert',
;;    `bookmark-insert-location', `bookmark-jump',
;;    `bookmark-jump-other-window', `bookmark-relocate',
;;    `bookmark-rename', `bookmark-set', `bookmark-yank-word'.
;;
;;  User options defined here:
;;
;;    `bookmarkp-bookmark-name-length-max',
;;    `bookmarkp-handle-region-function',
;;    `bookmarkp-region-search-size',
;;    `bookmarkp-save-new-location-flag',
;;    `bookmarkp-show-end-of-region', `bookmarkp-sort-function',
;;    `bookmarkp-sort-functions-alist', `bookmarkp-su-or-sudo-regexp',
;;    `bookmarkp-use-region-flag', `bookmarkp-w3m-allow-multi-tabs'.
;;
;;  Faces defined here:
;;
;;    `bookmarkp-bad-bookmark', `bookmarkp-gnus', `bookmarkp-info',
;;    `bookmarkp-local-directory', `bookmarkp-local-file-with-region',
;;    `bookmarkp-local-file-without-region', `bookmarkp-non-file',
;;    `bookmarkp-remote-file', `bookmarkp-su-or-sudo',
;;    `bookmarkp-w3m'.
;;
;;  Macros defined here:
;;
;;    `bookmarkp-define-sort-command'.
;;
;;  Non-interactive functions defined here:
;;
;;    `bookmarkp-add-or-update-time', `bookmarkp-assoc-delete-all',
;;    `bookmarkp-bmenu-propertize-item',
;;    `bookmarkp-bookmark-marked-p', `bookmarkp-current-sec-time',
;;    `bookmarkp-edit-bookmark', `bookmarkp-face-prop',
;;    `bookmarkp-file-alist-only', `bookmarkp-file-bookmark-p',
;;    `bookmarkp-get-buffer-name', `bookmarkp-get-end-position',
;;    `bookmarkp-get-visit-time', `bookmarkp-get-visits-count',
;;    `bookmarkp-gnus-alist-only', `bookmarkp-gnus-bookmark-p',
;;    `bookmarkp-goto-position', `bookmarkp-handle-region-default',
;;    `bookmarkp-increment-visits', `bookmarkp-info-alist-only',
;;    `bookmarkp-info-bookmark-p', `bookmarkp-jump-gnus',
;;    `bookmarkp-jump-w3m', `bookmarkp-jump-w3m-new-session',
;;    `bookmarkp-jump-w3m-only-one-tab',
;;    `bookmarkp-line-number-at-pos',
;;    `bookmarkp-local-directory-bookmark-p',
;;    `bookmarkp-local-file-alist-only',
;;    `bookmarkp-local-file-bookmark-p', `bookmarkp-make-gnus-record',
;;    `bookmarkp-make-w3m-record', `bookmarkp-marked-bookmarks-only',
;;    `bookmarkp-maybe-save-bookmark',
;;    `bookmarkp-non-file-alist-only',
;;    `bookmarkp-non-file-bookmark-p',
;;    `bookmarkp-non-marked-bookmarks-only',
;;    `bookmarkp-position-after-whitespace',
;;    `bookmarkp-position-before-whitespace',
;;    `bookmarkp-record-end-context-region-string',
;;    `bookmarkp-record-front-context-region-string',
;;    `bookmarkp-record-front-context-string',
;;    `bookmarkp-record-rear-context-string',
;;    `bookmarkp-region-alist-only', `bookmarkp-region-bookmark-p',
;;    `bookmarkp-region-record-front-context-string',
;;    `bookmarkp-region-record-rear-context-string',
;;    `bookmarkp-remote-file-alist-only',
;;    `bookmarkp-remote-file-bookmark-p',
;;    `bookmarkp-remove-assoc-dups', `bookmarkp-remove-if',
;;    `bookmarkp-remove-if-not', `bookmarkp-replace-regexp-in-string',
;;    `bookmarkp-root-or-sudo-logged-p',
;;    `bookmarkp-save-new-region-location', `bookmarkp-some-marked-p',
;;    `bookmarkp-some-unmarked-p' `bookmarkp-sort-and-remove-dups',
;;    `bookmarkp-upcase', `bookmarkp-w3m-alist-only',
;;    `bookmarkp-w3m-bookmark-p', `bookmarkp-w3m-set-new-buffer-name'.
;;
;;  Non-interactive functions redefined here (from `bookmark.el'):
;;
;;    `bookmark--jump-via', `bookmark-bmenu-bookmark',
;;    `bookmark-bmenu-hide-filenames', `bookmark-bmenu-mode',
;;    `bookmark-bmenu-surreptitiously-rebuild-list',
;;    `bookmark-completing-read', `bookmark-default-handler',
;;    `bookmark-location', `bookmark-make-record-default',
;;    `bookmark-prop-set', `bookmark-write-file'.
;;
;;  Functions defined here for Emacs versions < 23
;;
;;    `bookmarkp-menu-jump-other-window'.
;;
;;  Internal variables defined here:
;;
;;    `bookmarkp-bmenu-before-hide-marked-alist',
;;    `bookmarkp-bmenu-before-hide-unmarked-alist',
;;    `bookmarkp-bmenu-called-from-inside-p',
;;    `bookmarkp-bookmark-marked-alist',
;;    `bookmarkp-jump-display-function',
;;    `bookmarkp-latest-bookmark-alist',
;;    `bookmarkp-non-file-filename', `bookmarkp-reverse-sort-p',
;;    `bookmarkp-version-number'.
;;
;;  ***** NOTE: The following functions defined in `bookmark.el'
;;              have been REDEFINED OR ADVISED HERE for Emacs versions < 23:
;;
;;    `bookmark-get-bookmark', `bookmark-get-bookmark-record',
;;    `bookmark-get-handler', `bookmark-handle-bookmark',
;;    `bookmark-jump-noselect', `bookmark-make-record',
;;    `bookmark-maybe-message', `bookmark-prop-get', `bookmark-store'.
;;
;;  ***** NOTE: The following functions defined in `info.el'
;;              have been REDEFINED HERE (Emacs 20-22):
;;
;;    `Info-bookmark-jump', `Info-bookmark-make-record'.
;;
;;  ***** NOTE: The following variables defined in `bookmark.el'
;;              have been REDEFINED HERE for Emacs versions < 23
;;
;;    `bookmark-make-record-function'.
;;
;;  ***** NOTE: The following variables defined in `bookmark.el'
;;              have been REDEFINED HERE.
;;
;;    `bookmark-alist', `bookmark-make-record-function'.
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
;;    - For any bookmark (except Gnus), you can bookmark a region of
;;      text, not just a position.  By default, when you jump to a
;;      bookmark that records a region, the region is activated.  See
;;      option `bookmarkp-use-region-flag'.  `C-u' reverses the
;;      behavior specified by the value of the option.
;;
;;    - Bookmarks can record the number of visits and the time of last
;;      visit.
;;
;;  Better bookmark relocation, if the contextual text changes.
;;
;;  Improvements for the menu list (buffer `*Bookmark List*'):
;;
;;    - You can show only bookmarks of a given type (e.g. file, Info).
;;    - You can sort bookmarks in multiple ways.
;;    - Enhanced marking/unmarking, similar to that in Dired.
;;    - Faces to distinguish bookmarks of different types.
;;    - You can edit a bookmark.
;;
;;(@* "How To Use Bookmark+")
;;  ** How To Use Bookmark+ **
;;
;;  Put this library in your `load-path'.
;;  Add this to your init file (~/.emacs) : (require 'bookmark+)
;;
;;  Some of the commands defined here bind `S-delete', to delete the
;;  current bookmark candidate during completion in Icicle mode (see
;;  Icicles: http://www.emacswiki.org/cgi-bin/wiki/Icicles).
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
;;    - A bookmark with a region is treated like a simple position
;;      bookmark: the destination is the region start position.
;;
;;    - A Gnus bookmark does not work; it is simply ignored.
;;
;;  However, there are two cases in which `bookmark+.el' bookmarks
;;  will raise an error in vanilla Emacs:
;;
;;  * You cannot use non-file bookmarks with any version of vanilla
;;    Emacs.
;;
;;  * You cannot use any bookmarks created using `bookmark+.el' with
;;    vanilla Emacs 20.
;;
;;    The Emacs bookmark data structure has changed from version to
;;    version.  Library `bookmark+.el' always creates bookmarks that
;;    have the most recent structure (Emacs 23).  As is the case for
;;    any bookmarks that have the Emacs 23 structure, these bookmarks
;;    will not work in vanilla Emacs 20 (that is, without
;;    `bookmark+.el').
;;
;;  Bottom line: Use `bookmark+.el' to access bookmarks created using
;;  `bookmark+.el'.
;;
;;(@* "New Bookmark Structure")
;;  ** New Bookmark Structure **
;;
;;  The bookmark data structure, variable `bookmark-alist', has been
;;  enhanced to support the new bookmark types.  For a description of
;;  this enhanced structure, use `C-h v bookmark-alist'.
;;
;;(@* "Bookmarks Menu List")
;;  ** Bookmarks Menu List **
;;
;;  Bookmark+ enhances the bookmarks list (the "menu list") that is
;;  displayed when you use `bookmark-bmenu-list' (`C-x r l'), in many
;;  ways.  You can show or hide bookmarks of particular types, mark
;;  and unmark bookmarks, and so forth.  Use `C-h m' in buffer
;;  `*Bookmark List*' for more information.
;;
;;  (Note: Bookmark+ uses option `bookmarkp-sort-function'; it does
;;  not use vanilla Emacs option `bookmark-sort-flag'.)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;(@* "Change log")
;;
;; 2009/10/01 dradams
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
;;     Removed: bookmarkp-bmenu-propertize-item.
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
;; 2009-06-09 to 2009-09-27 Thierry Volpiatto and dradams
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
(eval-when-compile (require 'cl)) ;; gensym, case, (plus, for Emacs 20: push, pop, dolist)


(defconst bookmarkp-version-number "2.6.0")

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

(defmacro bookmarkp-define-sort-command (sort-order comparison-fn doc-string)
  "Define a command to sort bookmarks in the menu list by SORT-ORDER.
SORT-ORDER is a short string (or symbol) describing the sort order.
 Examples: \"by last access time\", \"alphabetically\".

The new command is named by replacing any spaces in SORT-ORDER with
hyphens (`-') and then adding the prefix `bookmarkp-bmenu-sort-'.

COMPARISON-FN is a function that compares two bookmarks, returning
 non-nil if and only if the first bookmark sorts before the second.

DOC-STRING is the doc string of the new command."
  (unless (stringp sort-order) (setq sort-order  (symbol-name sort-order)))
  (let ((command  (intern (concat "bookmarkp-bmenu-sort-"
                                  (bookmarkp-replace-regexp-in-string
                                   "\\s-+" "-" sort-order)))))
    `(progn
      (setq bookmarkp-sort-functions-alist  (bookmarkp-assoc-delete-all
                                             ,sort-order bookmarkp-sort-functions-alist))
      (push (cons ,sort-order ',comparison-fn) bookmarkp-sort-functions-alist)
      (defun ,command (&optional reversep)
        ,doc-string
        (interactive)
        (cond (;; Not this sort order - make it this sort order.
               (not (eq bookmarkp-sort-function ',comparison-fn))
               (setq bookmarkp-sort-function   ',comparison-fn
                     bookmarkp-reverse-sort-p  nil))
              (;; This sort order reversed - make it not reversed.
               bookmarkp-reverse-sort-p
               (setq bookmarkp-sort-function   nil
                     bookmarkp-reverse-sort-p  t))
              (t;; This sort order - reverse it.
               (setq bookmarkp-reverse-sort-p  t)))
        (message "Sorting %s%s" (if bookmarkp-sort-function ,sort-order "turned OFF")
                 (if (and bookmarkp-sort-function bookmarkp-reverse-sort-p) ", REVERSED" ""))
        (bookmark-bmenu-surreptitiously-rebuild-list)))))
  
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
(define-key bookmark-bmenu-mode-map "." 'bookmarkp-bmenu-show-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "U") 'bookmarkp-bmenu-unmark-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "M-<DEL>") 'bookmarkp-bmenu-unmark-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-m" 'bookmarkp-bmenu-mark-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-t" 'bookmark-bmenu-toggle-filenames) ; `t' in Emacs
;;;###autoload
(define-key bookmark-bmenu-mode-map "t" 'bookmarkp-bmenu-toggle-marks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "q" 'bookmarkp-bmenu-quit)
;;;###autoload
(define-key bookmark-bmenu-mode-map "E" 'bookmarkp-bmenu-edit-bookmark)
;;;###autoload
(define-key bookmark-bmenu-mode-map "F" 'bookmarkp-bmenu-show-only-files)
;;;###autoload
(define-key bookmark-bmenu-mode-map "G" 'bookmarkp-bmenu-show-only-gnus)
;;;###autoload
(define-key bookmark-bmenu-mode-map "W" 'bookmarkp-bmenu-show-only-w3m-urls)
;;;###autoload
(define-key bookmark-bmenu-mode-map "I" 'bookmarkp-bmenu-show-only-info-nodes)
;;;###autoload
(define-key bookmark-bmenu-mode-map "B" 'bookmarkp-bmenu-show-only-non-files)
;;;###autoload
(define-key bookmark-bmenu-mode-map "R" 'bookmarkp-bmenu-show-only-regions)
;;;###autoload
(define-key bookmark-bmenu-mode-map ">" 'bookmarkp-bmenu-toggle-show-only-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "<" 'bookmarkp-bmenu-toggle-show-only-unmarked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "S" nil) ; For Emacs 20
;;;###autoload
(define-key bookmark-bmenu-mode-map "SS" 'bookmarkp-bmenu-change-sort-order-repeat)
;;;###autoload
(define-key bookmark-bmenu-mode-map "SV" 'bookmarkp-bmenu-sort-by-visit-frequency)
;;;###autoload
(define-key bookmark-bmenu-mode-map "ST" 'bookmarkp-bmenu-sort-by-last-visit-time)
;;;###autoload
(define-key bookmark-bmenu-mode-map "SA" 'bookmarkp-bmenu-sort-alphabetically)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-r" 'bookmark-bmenu-relocate) ; `R' in Emacs
;;;###autoload
(define-key bookmark-bmenu-mode-map "g" 'bookmarkp-bmenu-refresh-menu-list)
;;;###autoload
(define-key bookmark-bmenu-mode-map "%" nil) ; For Emacs20
;;;###autoload
(define-key bookmark-bmenu-mode-map "%m" 'bookmarkp-bmenu-regexp-mark)
;;;###autoload
(define-key bookmark-bmenu-mode-map "*" nil) ; For Emacs20
;;;###autoload
(when (< emacs-major-version 21)
  (define-key bookmark-bmenu-mode-map (kbd "RET") 'bookmark-bmenu-this-window)
  (define-key bookmark-bmenu-mode-map (kbd "*m") 'bookmark-bmenu-mark))

(defadvice bookmark-bmenu-mode (before bookmark+-add-keymap () activate)
  "
***************************** Bookmark+ *****************************\
\\<bookmark-bmenu-mode-map>

Hide/show bookmarks:

\\[bookmarkp-bmenu-show-all]\t- Show all bookmarks
\\[bookmarkp-bmenu-show-only-files]\t- Show only file & directory bookmarks (`C-u': local only)
\\[bookmarkp-bmenu-show-only-non-files]\t- Show only non-file bookmarks
\\[bookmarkp-bmenu-show-only-gnus]\t- Show only Gnus bookmarks
\\[bookmarkp-bmenu-show-only-info-nodes]\t- Show only Info bookmarks
\\[bookmarkp-bmenu-show-only-regions]\t- Show only region bookmarks
\\[bookmarkp-bmenu-show-only-w3m-urls]\t- Show only W3M bookmarks
\\[bookmarkp-bmenu-toggle-show-only-marked]\t- Toggle showing only marked bookmarks
\\[bookmarkp-bmenu-toggle-show-only-unmarked]\t- Toggle showing only unmarked bookmarks

Mark/unmark bookmarks:

\\[bookmarkp-bmenu-mark-all]\t- Mark all
\\[bookmarkp-bmenu-regexp-mark]\t- Mark all that match a regexp
\\[bookmarkp-bmenu-unmark-all]\t- Unmark all (`C-u': interactive query)
\\[bookmarkp-bmenu-toggle-marks]\t- Toggle marks: unmark marked and mark unmarked

Sort bookmarks:

\\[bookmarkp-bmenu-sort-by-visit-frequency]\t- Sort by visit frequency (repeat: reverse/off)
\\[bookmarkp-bmenu-sort-by-last-visit-time]\t- Sort by last time visited (repeat: reverse/off)
\\[bookmarkp-bmenu-sort-alphabetically]\t- Sort alphabetically (repeat: reverse/off)
\\[bookmarkp-bmenu-change-sort-order-repeat]\t- Cycle sort orders (repeat)

Misc:

\\[bookmarkp-bmenu-edit-bookmark]\t- Edit bookmark
\\[bookmarkp-bmenu-refresh-menu-list]\t- Refresh (revert) to up-to-date bookmarks list
\\[bookmarkp-bmenu-quit]\t- Quit bookmarks list

Options Affecting Bookmarks List (`\\[bookmark-bmenu-list]'):

`bookmarkp-sort-function'          - Initial sort
`bookmark-bmenu-toggle-filenames'  - Show filenames initially?
`bookmark-bmenu-file-column'       - Bookmark width if files shown

Other Options:

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

(defface bookmarkp-non-file
    '((((background dark)) (:foreground "grey"))
      (t (:foreground "DarkGreen")))
  "*Face used for a bookmarked buffer not associated with an existing file."
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
  "*Face used for a bookmark that seems to be bad: e.g., nonexistent file."
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
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/bookmark+.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/BookMarks#BookmarkPlus")
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
  "*Non-nil means jump to W3m bookmarks in a new session."
  :type 'boolean :group 'bookmarkp)

(defcustom bookmarkp-show-end-of-region t
  "*Show end of region with `exchange-point-and-mark' when activating a region.
If nil show only beginning of region."
  :type 'boolean :group 'bookmarkp)

(defcustom bookmarkp-bookmark-name-length-max 70
  "*Max number of chars for default name for a bookmark with a region."
  :type 'integer :group 'bookmarkp)

(defcustom bookmarkp-sort-function 'bookmarkp-alpha-p
  "*Predicate for sorting (comparing) bookmarks."
  :type '(choice (const :tag "None" nil) function) :group 'bookmarkp)

(when (> emacs-major-version 20)
  (defcustom bookmarkp-sort-functions-alist ()
    "*Alist of sort functions.
You probably do not want to customize this option.  Instead, use macro
`bookmarkp-define-sort-command' to define a new sort function and add
it to this alist.
Each alist element has the form (SORT-ORDER . COMPARISON-FUNCTION).
SORT-ORDER is a short string (or symbol) describing the sort order.
 Examples: \"by last access time\", \"alphabetically\".
COMPARISON-FN is a function that compares two bookmarks, returning
 non-nil if and only if the first bookmark sorts before the second."
    :type '(alist
            :key-type (choice :tag "Sort order" string symbol)
            :value-type
            (choice (function :tag "Comparison function") (const :tag "Do not sort" nil)))
    :group 'bookmarkp))

(unless (> emacs-major-version 20)      ; Emacs 20: custom type `alist' doesn't exist.
  (defcustom bookmarkp-sort-functions-alist ()
    "*Alist of sort functions.
You probably do not want to customize this option.  Instead, use macro
`bookmarkp-define-sort-command' to define a new sort function and add
it to this alist.
Each alist element has the form (SORT-ORDER . COMPARISON-FUNCTION).
SORT-ORDER is a short string (or symbol) describing the sort order.
 Examples: \"by last access time\", \"alphabetically\".
COMPARISON-FN is a function that compares two bookmarks, returning
 non-nil if and only if the first bookmark sorts before the second."
    :type '(repeat
            (cons
             (choice :tag "Sort order" string symbol)
             (choice (function :tag "Comparison function") (const :tag "Do not sort" nil))))
    :group 'bookmarkp))
 
;;(@* "Internal Variables")
;;; Internal Variables --------------------------------------------------

(defconst bookmarkp-non-file-filename "   - no file -"
  "Name to use for `filename' entry, for non-file bookmarks.")

(defvar bookmarkp-jump-display-function nil
  "Function used currently to display a bookmark.")

(defvar bookmarkp-reverse-sort-p nil
  "Non-nil means the sort direction is reversed.")

(defvar bookmarkp-latest-bookmark-alist ()
  "Copy of `bookmark-alist' as last filtered.")

(defvar bookmarkp-bookmark-marked-alist ()
  "Copy of `bookmark-alist' that contains the marked bookmarks.")

(defvar bookmarkp-bmenu-before-hide-unmarked-alist ()
  "Copy of `bookmark-alist' made before hiding unmarked bookmarks.")

(defvar bookmarkp-bmenu-before-hide-marked-alist ()
  "Copy of `bookmark-alist' made before hiding marked bookmarks.")

(defvar bookmarkp-bmenu-called-from-inside-p nil
  "Non-nil means `bookmark-bmenu-list' is called from menu-list buffer.")


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

 BOOKMARK-NAME is the name you provided for the bookmark.
 PARAM-ALIST is an alist of bookmark information.  The order of the
  entries in PARAM-ALIST is not important.  The possible entries are
  described below.  A nil value means the entry is not used.

Bookmarks created using vanilla Emacs (`bookmark.el'):

 (filename . FILENAME)
 (position . POS)
 (front-context-string . STR-AFTER-POS)
 (rear-context-string  . STR-BEFORE-POS)
 (annotation . ANNOTATION)
 (handler . HANDLER)

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

1. If no file is associated with the bookmark, then FILENAME is nil.

2. The following additional entries are used.  Their values are
non-nil when a region is bookmarked; they are nil otherwise.  When a
region is bookmarked, POS represents the region start position.

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

3. The following additional entries are used for a Gnus bookmark.

 (group . GNUS-GROUP-NAME)
 (article . GNUS-ARTICLE-NUMBER)
 (message-id . GNUS-MESSAGE-ID)

 GNUS-GROUP-NAME is the name of a Gnus group.
 GNUS-ARTICLE-NUMBER is the number of a Gnus article.
 GNUS-MESSAGE-ID is the identifier of a Gnus message.

4. For a W3m bookmark, FILENAME is a W3m URL.")
 
;;(@* "Compatibility Code for Older Emacs Versions")
;;; Compatibility Code for Older Emacs Versions ----------------------

;;;###autoload
(when (< emacs-major-version 23)

  ;; These definitions are for Emacs versions prior to Emacs 23.
  ;; They are the same as the vanilla Emacs 23+ definitions, except as noted.
  ;; They let older versions of Emacs handle bookmarks created with Emacs 23.

  (defun bookmark-get-bookmark (bookmark &optional noerror)
    "Return the bookmark record corresponding to BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
If BOOKMARK is already a bookmark record, just return it,
Otherwise look for the corresponding bookmark in `bookmark-alist'."
    (cond
      ((consp bookmark) bookmark)
      ((stringp bookmark)
       (or (if (fboundp 'assoc-string)  ; Emacs 22+.
               (assoc-string bookmark bookmark-alist bookmark-completion-ignore-case)
             (assoc bookmark bookmark-alist))
           (unless noerror (error "Invalid bookmark: `%s'" bookmark))))))

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
      (bookmarkp-maybe-save-bookmark)
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
    (save-excursion (bookmark-handle-bookmark bookmark) (cons (current-buffer) (point))))

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
         ;; object.  If an object, assume it's a bookmark used internally by some other
         ;; package.
         (let ((file  (bookmark-get-filename bookmark)))
           (when file                   ; Don't know how to relocate if file doesn't exist.
             (setq file  (expand-file-name file)) (ding)
             (cond ((y-or-n-p (concat (file-name-nondirectory file)
                                      " nonexistent.  Relocate \""
                                      bookmark "\"? "))
                    (bookmark-relocate bookmark) ; Try again
                    (funcall (or (bookmark-get-handler bookmark) 'bookmark-default-handler)
                             (bookmark-get-bookmark bookmark)))
                   (t
                    (message "Bookmark not relocated \(%s\)." bookmark)
                    (signal (car err) (cdr err)))))))))
    (when (stringp bookmark) (setq bookmark-current-bookmark  bookmark))
    nil))                               ; Return nil if no error.

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
                                            "Jump to Bookmark (in another window)" event))

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
  (bookmark-maybe-load-default-file)    ; paranoia
  (if (listp last-nonmenu-event)
      (bookmark-menu-popup-paned-menu t prompt (bookmark-all-names))
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
                        (bookmarkp-region-record-front-context-string beg end)
                      (bookmarkp-record-front-context-string beg)))
         (rcs       (if isregion
                        (bookmarkp-region-record-rear-context-string beg)
                      (bookmarkp-record-rear-context-string beg)))
         (fcrs      (when isregion (bookmarkp-record-front-context-region-string beg end)))
         (ecrs      (when isregion (bookmarkp-record-end-context-region-string end)))
         (ctime     (bookmarkp-current-sec-time)))
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
;; Use `bookmark-make-record'.
;; Use special default prompts for active region, W3m, and Gnus.
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

 * If in W3m mode, then the current W3m title.

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
;; Add bookmark to `bookmarkp-bookmark-marked-alist',
;; Do not call `bookmark-bmenu-check-position'.
;;
;;;###autoload
(defun bookmark-bmenu-mark ()           ; `m' in menu list
  "Mark the bookmark on this line, using mark `>'."
  (interactive)
  (beginning-of-line)
  (let ((inhibit-read-only  t))
    (push (bookmark-bmenu-bookmark) bookmarkp-bookmark-marked-alist)
    (delete-char 1) (insert ?>)
    (forward-line 1)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Remove bookmark from `bookmarkp-bookmark-marked-alist'.
;; Do not call `bookmark-bmenu-check-position'.
;;
;;;###autoload
(defun bookmark-bmenu-unmark (&optional backup) ; `u' in menu list
  "Unmark the bookmark on this line, then move down to the next.
Optional BACKUP means move up instead."
  (interactive "P")
  (beginning-of-line)
  (let ((inhibit-read-only  t))
    (delete-char 1) (insert " ")
    (setq bookmarkp-bookmark-marked-alist  (delete (bookmark-bmenu-bookmark)
                                                   bookmarkp-bookmark-marked-alist)))
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
  (bookmarkp-increment-visits bookmark)
  (bookmarkp-add-or-update-time bookmark)
  (setq bookmarkp-jump-display-function  display-function)
  (bookmark-handle-bookmark bookmark)
  (let ((win  (get-buffer-window (current-buffer) 0)))
    (when win (set-window-point win (point))))
  ;; VANILLA EMACS FIXME: we used to only run `bookmark-after-jump-hook' in
  ;; `bookmark-jump' itself, but in none of the other commands.
  (run-hooks 'bookmark-after-jump-hook)
  (when bookmark-automatically-show-annotations
    (bookmark-show-annotation bookmark)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Add to beginning, not end, of bookmark record. Do not use `nconc'.
;; (Respect both old and new bookmark formats.)
;;
(defun bookmark-prop-set (bookmark prop val)
  "Set the property PROP of BOOKMARK to VAL."
  (let ((bmk   (bookmark-get-bookmark bookmark))
        (cell  (assq prop (bookmark-get-bookmark-record bookmark))))
    (if cell
        (setcdr cell val)
      (if (consp (car (cadr bmk)))      ; Old format: ("name" ((filename . "f")...))
          (setcdr bmk (list (cons (cons prop val) (cadr bmk))))
        (setcdr bmk (cons (cons prop val) (cdr bmk))))))) ; New: ("name" (filename . "f")...)


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added optional arg USE-REGION-P.
;; 2. Add note about Icicles `S-delete' to doc string.
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
  (interactive (list (bookmark-completing-read "Jump to bookmark" bookmark-current-bookmark)
                     current-prefix-arg))
  (unless bookmark-name (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark-name)
  (let ((bookmarkp-use-region-flag  (if use-region-p
                                        (not bookmarkp-use-region-flag)
                                      bookmarkp-use-region-flag)))
    (bookmark--jump-via bookmark-name 'switch-to-buffer)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Added optional arg USE-REGION-P.
;;
;;;###autoload
(defun bookmark-jump-other-window (bookmark-name &optional use-region-p) ; `C-x p o'
  "Jump to the bookmark named BOOKMARK-NAME, in another window.
See `bookmark-jump'."
  (interactive (list (bookmark-completing-read
                      "Jump to bookmark (in another window)"
                      bookmark-current-bookmark)
                     current-prefix-arg))
  (unless bookmark-name (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark-name)
  (let ((bookmarkp-use-region-flag  (if use-region-p
                                        (not bookmarkp-use-region-flag)
                                      bookmarkp-use-region-flag)))
    (bookmark--jump-via bookmark-name 'switch-to-buffer-other-window)))


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
  (let* ((bmk      (bookmark-get-bookmark bookmark)) ; Get bookmark object once and for all.
         (file     (bookmark-get-filename bmk))
         (buf      (bookmark-prop-get bmk 'buffer))
         (bufname  (bookmarkp-get-buffer-name bmk))
         (pos      (bookmark-get-position bmk))
         (end-pos  (bookmarkp-get-end-position bmk)))
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
      (funcall bookmarkp-handle-region-function bmk))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Add note about `S-delete' to doc string.
;; Change arg name: BOOKMARK -> BOOKMARK-NAME.
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
;; Add note about `S-delete' to doc string.
;; Change arg name: BOOKMARK -> BOOKMARK-NAME.
;;
(or (fboundp 'old-bookmark-insert-location)
(fset 'old-bookmark-insert-location (symbol-function 'bookmark-insert-location)))

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
  (old-bookmark-insert-location bookmark-name no-history))


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
;; Added note about `S-delete' to doc string.
;; Added BATCH arg.
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
    (bookmarkp-maybe-save-bookmark) newname))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Add note about `S-delete' to doc string.
;; Change arg name: BOOKMARK -> BOOKMARK-NAME.
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
;; Add note about `S-delete' to doc string.
;; Change arg name: BOOKMARK -> BOOKMARK-NAME.
;; Increment `bookmark-alist-modification-count' even when using `batch' arg.
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
   (list (bookmark-completing-read "Delete bookmark" bookmark-current-bookmark)))
  (bookmark-maybe-historicize-string bookmark-name)
  (bookmark-maybe-load-default-file)
  (let ((will-go  (bookmark-get-bookmark bookmark-name 'noerror)))
    (setq bookmark-alist  (delq will-go bookmark-alist))
    ;; Added by DB.  `bookmark-current-bookmark' is nil if last occurrence was deleted.
    (unless (bookmark-get-bookmark bookmark-current-bookmark 'noerror)
      (setq bookmark-current-bookmark  nil)))
  ;; Don't rebuild the list when using `batch' arg
  (unless batch (bookmark-bmenu-surreptitiously-rebuild-list))
  (bookmarkp-maybe-save-bookmark))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Insert code piecewise, to improve performance when saving `bookmark-alist'.
;; (Do not let `pp' parse all of `bookmark-alist' at once.)
;;
;; Remove any text properties in bookmark title.
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
 
;;(@* "Menu List Replacements (`bookmark-bmenu-*')")
;;; Menu List Replacements (`bookmark-bmenu-*') ----------------------


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Rebuild `bookmark-alist' using the last filtered alist in use.
;; 2. Update the menu-list title.
;;
(defun bookmark-bmenu-surreptitiously-rebuild-list ()
  "Rebuild the bookmarks list, if it exists."
  (when (get-buffer "*Bookmark List*")
    (save-excursion
      (save-window-excursion
        (let ((bookmark-alist  bookmarkp-latest-bookmark-alist))
          (bookmark-bmenu-list (with-current-buffer "*Bookmark List*" ; Title
                                 (goto-char (point-min))
                                 (buffer-substring (line-beginning-position)
                                                   (line-end-position)))
                               'filteredp))))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added optional arguments TITLE and FILTER-ON.
;; 2. Handles also region bookmarks and buffer (non-file) bookmarks.
;;
;;;###autoload
(defun bookmark-bmenu-list (&optional title filteredp) ; `C-x r l'
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

The optional arguments are for non-interactive use.
TITLE is a string to be used as the title. (default: `% Bookmarks').
Non-nil FILTEREDP indicates that `bookmark-alist' has been filtered
\(e.g gnus, w3m, info, files, or regions).  In that case,
`bookmarkp-latest-bookmark-alist' is not reset to `bookmark-alist'."
  (interactive)
  (bookmark-maybe-load-default-file)
  (unless bookmarkp-bmenu-called-from-inside-p (setq bookmarkp-bookmark-marked-alist  ()))
  (unless filteredp (setq bookmarkp-latest-bookmark-alist  bookmark-alist))
  (if (interactive-p)
      (switch-to-buffer (get-buffer-create "*Bookmark List*"))
    (set-buffer (get-buffer-create "*Bookmark List*")))
  (let* ((inhibit-read-only  t)
         (alternate-title    (if title title "% Bookmarks"))
         (len-alt-title      (- (length alternate-title) 2)))
    (erase-buffer)
    (insert (format "%s\n- %s\n" alternate-title (make-string len-alt-title ?-)))
    (add-text-properties (point-min) (point) (bookmarkp-face-prop 'bookmark-menu-heading))
    (mapcar (lambda (full-record)
              ;; If a bookmark has an annotation, prepend a "*" in the list of bookmarks.
              (let ((name        (bookmark-name-from-full-record full-record))
                    (annotation  (bookmark-get-annotation full-record))
                    (marked      (bookmarkp-bookmark-marked-p full-record))
                    (start       (+ 2 (point))) ; + 2 to skip marks area
                    end)
                (insert (cond ((and annotation (not (string-equal annotation "")) marked)
                               ">*")
                              ((and annotation (not (string-equal annotation "")))
                               " *")
                              (marked
                               "> ")
                              (t
                               "  "))
                        name)
                (setq end  (point))
                (bookmarkp-bmenu-propertize-item name start end)
                (insert "\n")))
            (bookmarkp-sort-and-remove-dups bookmark-alist))
    (goto-char (point-min))  (forward-line 2)
    (bookmark-bmenu-mode)
    (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-toggle-filenames t))
    (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Do not set or use `bookmark-bmenu-bookmark-column' - use column 2 always.
;;
(defun bookmark-bmenu-bookmark ()
  "Return the name of the bookmark on this line."
  (bookmark-bmenu-check-position)
  (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-hide-filenames))
  (save-excursion
    (save-window-excursion
      (beginning-of-line)
      (move-to-column 2 t)
      (prog1 (buffer-substring-no-properties (point) (save-excursion (end-of-line) (point)))
        (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-toggle-filenames t))))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Add text properties when hiding filenames.
;; Do not set or use `bookmark-bmenu-bookmark-column' - use column 2 always.
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
        (save-excursion
          (let ((inhibit-read-only  t))
            (while bookmark-bmenu-hidden-bookmarks
              (move-to-column 2 t)
              (bookmark-kill-line)
              (let ((name  (car bookmark-bmenu-hidden-bookmarks))
                    start
                    (end   (point)))
                (insert name)
                (setq start  (save-excursion (re-search-backward "[^ \t]") (1+ (point))))
                (bookmarkp-bmenu-propertize-item name start end))
              (setq bookmark-bmenu-hidden-bookmarks  (cdr bookmark-bmenu-hidden-bookmarks))
              (forward-line 1))))))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Use `pop-to-buffer', not `switch-to-buffer-other-window'.
;; Do not call `bookmark-bmenu-check-position' (done by `bookmark-bmenu-bookmark').
;;
(defun bookmark-bmenu-other-window ()   ; `o' in menu list
  "Select this line's bookmark in other window, leaving bookmark menu visible."
  (interactive)
  (let ((bookmark  (bookmark-bmenu-bookmark))
        (bookmark-automatically-show-annotations  t)) ; FIXME: needed?
    (bookmark--jump-via bookmark 'pop-to-buffer)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Use  `bookmark-bmenu-surreptitiously-rebuild-list', instead of using
;; `bookmark-bmenu-list', updating the modification count, and saving.
;; 2. Update `bookmarkp-latest-bookmark-alist' to reflect the deletions.
;;
;;;###autoload
(defun bookmark-bmenu-execute-deletions () ; `x' in menu list
  "Delete bookmarks marked with \\<Buffer-menu-mode-map>\\[Buffer-menu-delete] commands."
  (interactive)
  (message "Deleting bookmarks...")
  (let ((hiding-file-names-p  bookmark-bmenu-toggle-filenames)
        (o-point              (point))
        (o-str                (save-excursion
                                (beginning-of-line)
                                (if (looking-at "^D")
                                    nil
                                  (buffer-substring (point) (progn (end-of-line) (point))))))
        (o-col                (current-column)))
    (when hiding-file-names-p (bookmark-bmenu-hide-filenames))
    (setq bookmark-bmenu-toggle-filenames  nil)
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward "^D" (point-max) t)
      (let ((bmk  (bookmark-bmenu-bookmark))) 
        (bookmark-delete bmk 'batch)    ; pass BATCH arg
        (setq bookmarkp-latest-bookmark-alist
              (delete (assoc bmk bookmarkp-latest-bookmark-alist)
                      bookmarkp-latest-bookmark-alist))))
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (setq bookmark-bmenu-toggle-filenames  hiding-file-names-p)
    (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-toggle-filenames 'show))
    (if (not o-str)
        (goto-char o-point)
      (goto-char (point-min))
      (search-forward o-str)
      (beginning-of-line)
      (forward-char o-col))
    (beginning-of-line)
    (message "Deleting bookmarks...done")))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Do not call `bookmark-bmenu-list' (it was already called).
;; Do not call `bookmark-bmenu-check-position' (done by `bookmark-bmenu-bookmark').
;;
;;;###autoload
(defun bookmark-bmenu-rename ()         ; `r' in menu list
  "Rename bookmark on current line.  Prompts for a new name."
  (interactive)
  (let ((new-name  (bookmark-rename (bookmark-bmenu-bookmark))))
    (when (or (search-forward new-name (point-max) t) (search-backward new-name (point-min) t))
      (beginning-of-line))))
 
;;(@* "Bookmark+ Functions (`bookmarkp-*')")
;;; Bookmark+ Functions (`bookmarkp-*') ------------------------------

(defun bookmarkp-maybe-save-bookmark ()
  "Increment save counter and maybe save `bookmark-alist'."
  (setq bookmark-alist-modification-count  (1+ bookmark-alist-modification-count))
  (when (bookmark-time-to-save-p) (bookmark-save)))

(defun bookmarkp-edit-bookmark (bookmark-name)
  "Edit bookmark's name and file name, and maybe save them.
BOOKMARK-NAME is the current (old) name of the bookmark to be renamed."
  (let* ((bookmark-filename  (bookmark-get-filename bookmark-name))
         (new-bmk-name       (read-from-minibuffer "Name: " nil nil nil nil bookmark-name))
         (new-filename       (read-from-minibuffer "FileName: " nil nil nil nil
                                                   bookmark-filename)))
    (when (and (not (equal new-bmk-name "")) (not (equal new-filename ""))
               (y-or-n-p "Save changes?"))
      (bookmark-rename bookmark-name new-bmk-name 'batch)
      (bookmark-set-filename new-bmk-name new-filename)
      (bookmarkp-maybe-save-bookmark)
      (list new-bmk-name new-filename))))

(defun bookmarkp-increment-visits (bookmark &optional batch)
  "Increment visits count of BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
If bookmark has no `visits' entry, add one with a 0 count."
  (let ((cur-val  (bookmark-prop-get bookmark 'visits)))
    (if cur-val
        (bookmark-prop-set bookmark 'visits (1+ cur-val))
      (bookmark-prop-set bookmark 'visits 0)))
  (unless batch (bookmark-bmenu-surreptitiously-rebuild-list))
  (bookmarkp-maybe-save-bookmark))

(defun bookmarkp-current-sec-time ()
  "Return current time in seconds."
  (if (fboundp 'float-time)
      (float-time)
    (let ((ct   (current-time)))
      (+ (* (float (nth 0 ct)) (expt 2 16)) (nth 1 ct)))))

(defun bookmarkp-add-or-update-time (bookmark &optional batch)
  "Update `time' entry of BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
If it has no time entry, then add one, using the current time in seconds."
  (bookmark-prop-set bookmark 'time (bookmarkp-current-sec-time))
  (unless batch (bookmark-bmenu-surreptitiously-rebuild-list))
  (bookmarkp-maybe-save-bookmark))


;; Other Menu-List Commands

;;;###autoload
(defun bookmarkp-bmenu-edit-bookmark () ; `E' in menu list
  "Edit the bookmark under the cursor."
  (interactive)
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
         (isremote      (and isfile  (if (fboundp 'file-remote-p)
                                         (file-remote-p isfile)
                                       (if (fboundp 'ffap-file-remote-p)
                                           (ffap-file-remote-p isfile)))))
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
     (cond ((or (eq ishandler 'Info-bookmark-jump) (string= isbuf "*info*")) ; Info
            (append (bookmarkp-face-prop 'bookmarkp-info)
                    '(mouse-face highlight follow-link t
                      help-echo "mouse-2: Go to this Info buffer")))
           (isgnus                      ; Gnus
            (append (bookmarkp-face-prop 'bookmarkp-gnus)
                    '(mouse-face highlight follow-link t
                      help-echo "mouse-2: Go to this Gnus buffer")))
           (isw3m                       ; W3m
            (append (bookmarkp-face-prop 'bookmarkp-w3m)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2 Goto URL: `%s'" ,isfile))))
           ((and issu (not (bookmarkp-root-or-sudo-logged-p))) ; Root/sudo not logged
            (append (bookmarkp-face-prop 'bookmarkp-su-or-sudo)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2 Goto file: `%s'" ,isfile))))
           ((and isremote (not issu))   ; Remote file (ssh, ftp)
            (append (bookmarkp-face-prop 'bookmarkp-remote-file)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2 Goto remote file: `%s'" ,isfile))))
           ((and isfile (file-directory-p isfile)) ; Local directory
            (append (bookmarkp-face-prop 'bookmarkp-local-directory)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2 Goto dired: `%s'" ,isfile))))
           ((and isfile (file-exists-p isfile) isregion) ; Local file with region
            (append (bookmarkp-face-prop 'bookmarkp-local-file-with-region)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2 Find region in file: `%s'" ,isfile))))
           ((and isfile (file-exists-p isfile)) ; Local file without region
            (append (bookmarkp-face-prop 'bookmarkp-local-file-without-region)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2 Goto file: `%s'" ,isfile))))
           ((and isbuf (or (not isfile)
                           (equal isfile "   - no file -")
                           (not (file-exists-p isfile)))) ; Buffer
            (append (bookmarkp-face-prop 'bookmarkp-non-file)
                    `(mouse-face highlight follow-link t
                      help-echo (format "mouse-2 Goto buffer: `%s'" ,isbuf))))
           (t (append (bookmarkp-face-prop 'bookmarkp-bad-bookmark)
                      `(mouse-face highlight follow-link t
                        help-echo (format "mouse-2 BAD BOOKMARK (maybe): `%s'" ,isfile))))))))

;;;###autoload
(defun bookmarkp-bmenu-quit ()          ; `q' in menu list
  "Reset the marked bookmark lists and quit."
  (interactive)
  (setq bookmarkp-bookmark-marked-alist             ()
        bookmarkp-bmenu-before-hide-marked-alist    ()
        bookmarkp-bmenu-before-hide-unmarked-alist  ())
  (quit-window))


;;; *-bmenu-* Filter Commands

;;;###autoload
(defun bookmarkp-bmenu-show-only-files (arg) ; `F' in menu list
  "Display a list of file and directory bookmarks (only).
With a prefix argument, do not include remote files or directories."
  (interactive "P")
  (let ((bookmark-alist                        (bookmarkp-file-alist-only arg))
        (bookmarkp-bmenu-called-from-inside-p  t))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list "% File and Directory Bookmarks" 'filteredp))
  (message "Only file bookmarks are shown"))

;;;###autoload
(defun bookmarkp-bmenu-show-only-non-files () ; `B' in menu list
  "Display (only) the non-file bookmarks."
  (interactive)
  (let ((bookmark-alist                        (bookmarkp-non-file-alist-only))
        (bookmarkp-bmenu-called-from-inside-p  t))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list "% Non-File Bookmarks" 'filteredp))
  (message "Only non-file bookmarks are shown"))
    
;;;###autoload
(defun bookmarkp-bmenu-show-only-info-nodes () ; `I' in menu list
  "Display (only) the Info bookmarks."
  (interactive)
  (let ((bookmark-alist                        (bookmarkp-info-alist-only))
        (bookmarkp-bmenu-called-from-inside-p  t))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list "% Info Bookmarks" 'filteredp))
  (message "Only Info bookmarks are shown"))

;;;###autoload
(defun bookmarkp-bmenu-show-only-w3m-urls () ; `W' in menu list
  "Display (only) the w3m bookmarks."
  (interactive)
  (let ((bookmark-alist                        (bookmarkp-w3m-alist-only))
        (bookmarkp-bmenu-called-from-inside-p  t))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list "% W3M Bookmarks" 'filteredp))
  (message "Only W3m bookmarks are shown"))

;;;###autoload
(defun bookmarkp-bmenu-show-only-gnus () ; `G' in menu list
  "Display (only) the Gnus bookmarks."
  (interactive)
  (let ((bookmark-alist                        (bookmarkp-gnus-alist-only))
        (bookmarkp-bmenu-called-from-inside-p  t))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list "% Gnus Bookmarks" 'filteredp))
  (message "Only Gnus bookmarks are shown"))

;;;###autoload
(defun bookmarkp-bmenu-show-only-regions () ; `R' in menu list
  "Display (only) the bookmarks that record a region."
  (interactive)
  (let ((bookmark-alist                        (bookmarkp-region-alist-only))
        (bookmarkp-bmenu-called-from-inside-p  t))
    (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
    (bookmark-bmenu-list "% Region Bookmarks" 'filteredp))
  (message "Only bookmarks with regions are shown"))

;;;###autoload
(defun bookmarkp-bmenu-toggle-show-only-unmarked () ; `<' in menu list
  "Hide all marked bookmarks."
  (interactive)
  (if (or (bookmarkp-some-marked-p bookmarkp-latest-bookmark-alist)
          (bookmarkp-some-marked-p bookmarkp-bmenu-before-hide-marked-alist))
      (let ((hiding-file-names-p                   bookmark-bmenu-toggle-filenames)
            (bookmark-alist                        bookmarkp-latest-bookmark-alist)
            (bookmarkp-bmenu-called-from-inside-p  t)
            status)
        (when hiding-file-names-p (bookmark-bmenu-hide-filenames))
        (setq bookmark-bmenu-toggle-filenames  nil)
        (if bookmarkp-bmenu-before-hide-marked-alist
            (setq bookmark-alist  bookmarkp-bmenu-before-hide-marked-alist
                  bookmarkp-bmenu-before-hide-marked-alist  ()
                  bookmarkp-latest-bookmark-alist           bookmark-alist
                  status                                    'shown)
          (setq bookmarkp-bmenu-before-hide-marked-alist  bookmarkp-latest-bookmark-alist
                bookmark-alist                            (bookmarkp-non-marked-bookmarks-only)
                bookmarkp-latest-bookmark-alist           bookmark-alist
                status                                    'hidden))
        (bookmark-bmenu-surreptitiously-rebuild-list)
        (cond ((eq status 'hidden)
               (bookmark-bmenu-check-position)
               (message "Marked bookmarks are hidden"))
              (t
               (goto-char (point-min))
               (when (re-search-forward "^>" (point-max) t)  (forward-line 0))
               (message "Marked and unmarked bookmarks are shown")))
        (setq bookmark-bmenu-toggle-filenames  hiding-file-names-p)
        (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-toggle-filenames 'shown)))
    (message "No marked bookmarks to hide"))
  (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window)))

;;;###autoload
(defun bookmarkp-bmenu-toggle-show-only-marked () ; `>' in menu list
  "Hide all unmarked bookmarks."
  (interactive)
  (if (or (bookmarkp-some-unmarked-p bookmarkp-latest-bookmark-alist)
          (bookmarkp-some-unmarked-p bookmarkp-bmenu-before-hide-unmarked-alist))
      (let ((hiding-file-names-p                   bookmark-bmenu-toggle-filenames)
            (bookmark-alist                        bookmarkp-latest-bookmark-alist)
            (bookmarkp-bmenu-called-from-inside-p  t)
            status)
        (when hiding-file-names-p  (bookmark-bmenu-hide-filenames))
        (setq bookmark-bmenu-toggle-filenames  nil)
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
               (message "Unmarked bookmarks are hidden"))
              (t
               (goto-char (point-min))
               (when (re-search-forward "^>" (point-max) t)  (forward-line 0))
               (message "Marked and unmarked bookmarks are shown")))
        (setq bookmark-bmenu-toggle-filenames  hiding-file-names-p)
        (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-toggle-filenames 'shown)))
    (message "No unmarked bookmarks to hide"))
  (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window)))

;;;###autoload
(defun bookmarkp-bmenu-show-all ()      ; `.' in menu list
  "Show all bookmarks already known to the bookmarks menu list.
This does not revert the menu list, to bring it up to date.
To revert the list, use `\\<bookmark-bmenu-mode-map>\\[bookmarkp-bmenu-refresh-menu-list]'."
  (interactive)
  (let ((bookmarkp-bmenu-called-from-inside-p  t))
    (bookmark-bmenu-list))
  (message "All bookmarks are shown"))

;;;###autoload
(defun bookmarkp-bmenu-refresh-menu-list () ; `g' in menu list
  "Refresh (revert) bookmarks menu list, bringing it up to date.
This does not change the current filtering or sorting."
  (interactive)
  (when (equal (buffer-name (current-buffer)) "*Bookmark List*")
    (let ((bookmarkp-bmenu-called-from-inside-p  t))
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (bookmark-bmenu-check-position))))


;;; *-bmenu-* Commands and functions for marked bookmarks

;;;###autoload
(defun bookmarkp-bmenu-mark-all ()      ; `M-m' in menu list
  "Mark all bookmarks, using mark `>'."
  (interactive)
  (with-current-buffer "*Bookmark List*"
    (save-excursion  
      (let ((hiding-file-names-p  bookmark-bmenu-toggle-filenames)
            (count                0))
        (when hiding-file-names-p (bookmark-bmenu-hide-filenames))
        (setq bookmark-bmenu-toggle-filenames  nil)
        (goto-char (point-min))
        (forward-line 2)
        (while (not (eobp)) (bookmark-bmenu-mark) (setq count  (1+ count)))
        (setq bookmark-bmenu-toggle-filenames  hiding-file-names-p)
        (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-toggle-filenames 'show))
        (message (if (= count 1) "1 marked, 0 unmarked" "%d marked, 0 unmarked") count)))))

;; This is very similar to `dired-unmark-all-files'.
;;;###autoload
(defun bookmarkp-bmenu-unmark-all (mark &optional arg) ; `M-DEL', `U' in menu list
  "Remove a mark from each bookmark.
Hit the mark character (`>' or `D') to remove those marks,
 or hit `RET' to remove all marks (both `>' and `D').
With a prefix arg, you are queried to unmark each marked bookmark.
Use `\\[help-command]' during querying for help."
  (interactive "cRemove marks (RET means all): \nP")
  (require 'dired-aux)
  (with-current-buffer "*Bookmark List*"
    (save-excursion
      (let* ((hiding-file-names-p  bookmark-bmenu-toggle-filenames)
             (count                0)
             (inhibit-read-only    t)
             (case-fold-search     nil)
             (query                nil)
             (string               (format "\n%c" mark))
             (help-form            "\
Type SPC or `y' to unmark one bookmark, DEL or `n' to skip to next,
`!' to unmark all remaining bookmarks with no more questions."))
        (when hiding-file-names-p (bookmark-bmenu-hide-filenames))
        (setq bookmark-bmenu-toggle-filenames  nil)
        (goto-char (point-min))
        (forward-line 1)                ; Only one, because STRING starts with a newline.
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
        (setq bookmark-bmenu-toggle-filenames  hiding-file-names-p)
        (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-toggle-filenames 'show))
        (message (if (= count 1) "1 unmarked" "%d unmarked") count)))))

;;;###autoload
(defun bookmarkp-bmenu-regexp-mark (regexp) ; `% m' in menu list
  "Mark bookmarks that match REGEXP."
  (interactive "sRegexp: ")
  (let ((hiding-file-names-p  bookmark-bmenu-toggle-filenames))
    (when hiding-file-names-p (bookmark-bmenu-hide-filenames))
    (setq bookmark-bmenu-toggle-filenames  nil)
    (with-current-buffer "*Bookmark List*"
      (save-excursion
        (goto-char (point-min))
        (forward-line 2)
        (while (and (not (eobp)) (re-search-forward regexp (point-max) t))
          (bookmark-bmenu-mark))))
    (setq bookmark-bmenu-toggle-filenames  hiding-file-names-p)
    (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-toggle-filenames 'show))))

;;;###autoload
(defun bookmarkp-bmenu-toggle-marks ()  ; `t' in menu list
  "Toggle marks: Unmark all marked bookmarks; mark all unmarked bookmarks.
This affects only the `>' mark, not the `D' flag."
  (interactive)
  (let ((hiding-file-names-p  bookmark-bmenu-toggle-filenames)
        (marked-count         0)
        (unmarked-count       0))
    (when hiding-file-names-p (bookmark-bmenu-hide-filenames))
    (setq bookmark-bmenu-toggle-filenames  nil)
    (with-current-buffer "*Bookmark List*"
      (save-excursion
        (goto-char (point-min))
        (forward-line 2)
        (if (not (bookmarkp-some-marked-p bookmarkp-latest-bookmark-alist))
            (bookmarkp-bmenu-mark-all)
          (while (not (eobp))
            (cond ((member (bookmark-bmenu-bookmark) bookmarkp-bookmark-marked-alist)
                   (bookmark-bmenu-unmark)
                   (setq unmarked-count  (1+ unmarked-count)))
                  (t
                   (bookmark-bmenu-mark)
                   (setq marked-count  (1+ marked-count)))))
          (message "%d marked, %d unmarked" marked-count unmarked-count))))
    (setq bookmark-bmenu-toggle-filenames  hiding-file-names-p)
    (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-toggle-filenames 'show))))


;; Predicates --------------------------------------------------------

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
  "Return non-nil if BOOKMARK is a W3m bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-w3m))

(defun bookmarkp-info-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is an Info bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'Info-bookmark-jump))

(defun bookmarkp-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a file or directory.
BOOKMARK is a bookmark name or a bookmark record.
This excludes bookmarks of a more specific kind (Info, Gnus, and W3m)."
  (let* ((filename   (bookmark-get-filename bookmark))
         (isnonfile  (equal filename bookmarkp-non-file-filename))) 
    (and filename (not isnonfile) (not (bookmark-get-handler bookmark)))))

(defun bookmarkp-non-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a non-file bookmark (e.g *scratch*).
This excludes bookmarks of a more specific kind (Info, Gnus, and W3m)."
  (let* ((filename   (bookmark-get-filename bookmark))
         (isnonfile  (equal filename bookmarkp-non-file-filename))) 
    (and isnonfile (not (bookmark-get-handler bookmark)))))

(defun bookmarkp-remote-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a remote file or directory.
BOOKMARK is a bookmark name or a bookmark record."
  (let* ((file      (bookmark-get-filename bookmark))
         (rem-file  (and file           ; Do not pass nil to `file-remote-p'
                         (if (fboundp 'file-remote-p)
                             (file-remote-p file)
                           (and (fboundp 'ffap-file-remote-p) (ffap-file-remote-p file))))))
    (and rem-file  (not (bookmark-get-handler bookmark))  rem-file)))

(defun bookmarkp-local-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a local file or directory.
BOOKMARK is a bookmark name or a bookmark record.
This excludes bookmarks of a more specific kind (Info, Gnus, and W3m)."
  (and (bookmarkp-file-bookmark-p bookmark)
       (not (bookmarkp-remote-file-bookmark-p bookmark))))

(defun bookmarkp-local-directory-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a local directory.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((file  (bookmark-get-filename bookmark)))
    (and (bookmarkp-local-file-bookmark-p bookmark) (file-directory-p file))))

(defun bookmarkp-bookmark-marked-p (bookmark)
  "Return non-nil if BOOKMARK is a marked bookmark."
  (member (car bookmark) bookmarkp-bookmark-marked-alist))


;; Filter Functions --------------------------------------------------

(defun bookmarkp-region-alist-only ()
  "`bookmark-alist', filtered to retain only bookmarks that have regions.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-region-bookmark-p bookmark-alist))

(defun bookmarkp-gnus-alist-only ()
  "`bookmark-alist', filtered to retain only Gnus bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-gnus-bookmark-p bookmark-alist))

(defun bookmarkp-w3m-alist-only ()
  "`bookmark-alist', filtered to retain only W3m bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-w3m-bookmark-p bookmark-alist))

(defun bookmarkp-info-alist-only ()
  "`bookmark-alist', filtered to retain only Info bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-info-bookmark-p bookmark-alist))

(defun bookmarkp-remote-file-alist-only ()
  "`bookmark-alist', filtered to retain only remote-file bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-remote-file-bookmark-p bookmark-alist))

(defun bookmarkp-local-file-alist-only ()
  "`bookmark-alist', filtered to retain only local-file bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-local-file-bookmark-p bookmark-alist))

(defun bookmarkp-file-alist-only (&optional hide-remote)
  "`bookmark-alist', filtered to retain only file and directory bookmarks.
This excludes bookmarks that might contain file information but are
particular in some way - for example, Info bookmarks or Gnus bookmarks.

Non-nil argument HIDE-REMOTE means do not include remote file or
directory bookmarks.

A new list is returned (no side effects)."
  (bookmarkp-remove-if #'(lambda (bookmark)
                           (or (bookmarkp-non-file-bookmark-p bookmark)
                               (bookmarkp-gnus-bookmark-p bookmark)
                               (bookmarkp-w3m-bookmark-p bookmark)
                               (bookmarkp-info-bookmark-p bookmark)
                               (and hide-remote (bookmarkp-remote-file-bookmark-p bookmark))))
                       bookmark-alist))

(defun bookmarkp-non-file-alist-only ()
  "`bookmark-alist', filtered to retain only non-file bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-non-file-bookmark-p bookmark-alist))


;;; Marked bookmarks

(defun bookmarkp-marked-bookmarks-only ()
  "Return the list of marked bookmarks."
  (bookmarkp-remove-if-not #'bookmarkp-bookmark-marked-p bookmark-alist))

(defun bookmarkp-non-marked-bookmarks-only ()
  "Return the list of not marked bookmarks."
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


;; General Utility Functions -----------------------------------------

(defun bookmarkp-remove-assoc-dups (alist)
  "Copy of ALIST with elements that have duplicate keys removed.
Only the first element of those with the same key is kept.
Keys are compared using `equal'."
  (let ((tail  alist)
        (new   ()))
    (while tail
      (unless (assoc (caar tail) new) (push (car tail) new))
      (pop tail))
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

(defun bookmarkp-upcase (string)
  "`upcase', but in case of error, return original STRING.
This works around an Emacs 20 problem that occurs if STRING contains
binary data (weird chars)."
  (condition-case nil (upcase string) (error string)))

(defun bookmarkp-replace-regexp-in-string (regexp rep string
                                           &optional fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING and return STRING."
  (if (fboundp 'replace-regexp-in-string) ; Emacs > 20.
      (replace-regexp-in-string regexp rep string fixedcase literal subexp start)
    (if (string-match regexp string)    ; Emacs 20
        (replace-match rep nil nil string)
      string)))

(defun bookmarkp-face-prop (value)
  "Return a list with elements `face' or `font-lock-face' and VALUE.
Starting with Emacs 22, the first element is `font-lock-face'."
  (list (if (> emacs-major-version 21) 'font-lock-face 'face) value))  

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

;; Other Functions ---------------------------------------------------


;;; Bookmark Entry Access Functions

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
  (bookmark-prop-get bookmark 'time))


;;; Sorting

(defun bookmarkp-sort-and-remove-dups (list)
  "Return a copy of LIST, sorted and with no duplicate keys.
Only the first element with a given key is kept.
Keys are compared using `equal'.
Sorting is done using using `bookmarkp-sort-function'.
Do nothing if `bookmarkp-sort-function' is nil.
If `bookmarkp-reverse-sort-p' is non-nil, then reverse the sort order."
  (let ((newlist  (bookmarkp-remove-assoc-dups list)))
    (when bookmarkp-sort-function
      (setq newlist
            (sort newlist (if bookmarkp-reverse-sort-p
                              (lambda (a b) (not (funcall bookmarkp-sort-function a b)))
                            bookmarkp-sort-function))))
    newlist))

;; Sort Commands

;; The order of the macro calls here defines the reverse order of
;; `bookmarkp-sort-functions-alist'.  The first here is also the default sort order.
;; Entries are traversed by `S S', in `bookmarkp-sort-functions-alist' order.

(bookmarkp-define-sort-command          ; `S A' in menu list
 "alphabetically"                       ; `bookmarkp-bmenu-sort-alphabetically'
 bookmarkp-alpha-p
 "Toggle sorting of bookmarks alphabetically, respecting `case-fold-search'.
A prefix arg reverses the sort direction.")

(bookmarkp-define-sort-command          ; `S V' in menu list
 "by visit frequency"                   ; `bookmarkp-bmenu-sort-by-visit-frequency'
 bookmarkp-visited-more-p
 "Toggle sorting of bookmarks by the number of times visited.
A prefix arg reverses the sort direction.")

(bookmarkp-define-sort-command          ; `S T' in menu list
 "by last visit time"                   ; `bookmarkp-bmenu-sort-by-last-visit-time'
 bookmarkp-visited-more-recently-p
 "Toggle sorting of bookmarks by the time of their last visit.
A prefix arg reverses the sort direction.")

(bookmarkp-define-sort-command
 "turned OFF" nil                       ; `bookmarkp-bmenu-sort-turned-OFF'
  "Do not sort bookmarks.")

(defun bookmarkp-repeat-command (command)
  "Repeat COMMAND."
 (let ((repeat-previous-repeated-command  command)
       (repeat-message-function           'ignore)
       (last-repeatable-command           'repeat))
   (repeat nil)))

(defun bookmarkp-bmenu-change-sort-order-repeat (arg) ; `S S'... in menu list
  "Cycle to the next sort order.
With a prefix arg, reverse current sort order.
This is a repeatable version of `bookmarkp-bmenu-change-sort-order'."
  (interactive "P")
  (bookmarkp-repeat-command 'bookmarkp-bmenu-change-sort-order))

(defun bookmarkp-bmenu-change-sort-order (&optional arg)
  "Cycle to the next sort order.
With a prefix arg, reverse the current sort order."
  (interactive "P")
  (setq bookmarkp-sort-functions-alist  (delq nil bookmarkp-sort-functions-alist))
  (if arg
      (bookmarkp-reverse-sort-order)
    (let (next-order)
      (let ((orders  (mapcar #'car bookmarkp-sort-functions-alist)))
        (setq next-order  (or (cadr (memq (bookmarkp-current-sort-order) orders))
                              (car orders))
              bookmarkp-sort-function  (cdr (assoc next-order
                                                   bookmarkp-sort-functions-alist))))
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (message "Sorting %s%s" next-order
               (if (and bookmarkp-sort-function bookmarkp-reverse-sort-p) ", REVERSED" "")))))

(defun bookmarkp-current-sort-order ()
  "Current sort order, or nil if sorting is inactive."
  (car (rassq bookmarkp-sort-function bookmarkp-sort-functions-alist)))

(defun bookmarkp-reverse-sort-order ()
  "Reverse the current sort order."
  (interactive)
  (setq bookmarkp-reverse-sort-p  (not bookmarkp-reverse-sort-p))
  (bookmark-bmenu-surreptitiously-rebuild-list)
  (message "Sorting %s%s" (bookmarkp-current-sort-order)
           (if (and bookmarkp-sort-function bookmarkp-reverse-sort-p) ", REVERSED" "")))


;; Sort Predicates

(defun bookmarkp-visited-more-p (s1 s2)
  "Return non-nil if bookmark S1 was visited more often than S2.
Also: S1 < S2 if S1 was visited but S2 was not.
      S1 < S2 if S1 precedes S2 alphabetically and
              neither was visited or both were visited equally."
  (let ((v1  (bookmarkp-get-visits-count s1))
        (v2  (bookmarkp-get-visits-count s2)))
    (cond ((and v1 v2)
           (or (> v1 v2)  (and (= v1 v2) (bookmarkp-alpha-p s1 s2))))
          (v1 t)
          (v2 nil)
          (t (bookmarkp-alpha-p s1 s2)))))

(defun bookmarkp-visited-more-recently-p (s1 s2)
  "Return non-nil if bookmark S1 was visited more recently than S2.
Also: S1 < S2 if S1 was visited but S2 was not.
      S1 < S2 if S1 precedes S2 alphabetically and
              either neither was visited
              or the last visit of each was at the same time."
  (let ((t1  (bookmarkp-get-visit-time s1))
        (t2  (bookmarkp-get-visit-time s2)))
    (cond ((and t1 t2)
           (or (> t1 t2)  (and (= t1 t2) (bookmarkp-alpha-p s1 s2))))
          (t1 t)
          (t2 nil)
          (t (bookmarkp-alpha-p s1 s2)))))

(defun bookmarkp-alpha-p (b1 b2)
  "Return non-nil if bookmark B1 sorts alphabetically before B2.
The bookmark names that are compared, respecting `case-fold-search'."
  (let ((s1  (car b1))
        (s2  (car b2)))
    (when case-fold-search
      (setq s1  (bookmarkp-upcase s1)
            s2  (bookmarkp-upcase s2)))
    (string-lessp s1 s2)))


;; Menu-List Functions (`bookmarkp-bmenu-*') -------------------------

(defun bookmarkp-root-or-sudo-logged-p ()
  "Return t if the user logged in using Tramp as `root' or `sudo'.
Otherwise, return nil."
  (catch 'break
    (dolist (ii  (mapcar #'buffer-name (buffer-list)))
      (when (string-match "*tramp/\\(su\\|sudo\\) ." ii) (throw 'break t)))))

(defun bookmarkp-region-record-front-context-string (breg ereg)
  "Return the region prefix, at BREG.
Return at most `bookmarkp-region-search-size' or (- EREG BREG) chars."
  (buffer-substring-no-properties
   breg (+ breg (min bookmarkp-region-search-size (- ereg breg)))))

(defun bookmarkp-record-front-context-string (breg)
  "Return `bookmark-search-size' chars, starting at position BREG.
Return nil if there are not that many chars."
  (and (>= (- (point-max) breg) bookmark-search-size)
       (buffer-substring-no-properties breg (+ breg bookmark-search-size))))

(defun bookmarkp-region-record-rear-context-string (breg)
  "Return the text preceding the region beginning, BREG.
Return at most `bookmarkp-region-search-size' chars."
  (buffer-substring-no-properties
   (max (- breg bookmarkp-region-search-size) (point-min))
   breg))

(defun bookmarkp-record-rear-context-string (breg)
  "Return `bookmark-search-size' chars that precede BREG (inclusive).
Return nil if there are not that many chars."
  (and (>= (- breg (point-min)) bookmark-search-size)
       (buffer-substring-no-properties breg (- breg bookmark-search-size))))

(defun bookmarkp-record-front-context-region-string (breg ereg)
  "Return the region suffix, ending at EREG.
Return at most `bookmarkp-region-search-size' or (- EREG BREG) chars."
  (buffer-substring-no-properties
   (- ereg (min bookmarkp-region-search-size (- ereg breg)))
   ereg))

(defun bookmarkp-record-end-context-region-string (ereg)
  "Return the text following the region end, EREG.
Return at most `bookmarkp-region-search-size' chars."
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
                            (bookmarkp-region-record-front-context-string beg end))
         (bookmark-prop-set bookmark 'rear-context-string
                            (bookmarkp-region-record-rear-context-string beg))
         (bookmark-prop-set bookmark 'front-context-region-string
                            (bookmarkp-record-front-context-region-string beg end))
         (bookmark-prop-set bookmark 'rear-context-region-string
                            (bookmarkp-record-end-context-region-string end))
         (bookmark-prop-set bookmark 'position beg)
         (bookmark-prop-set bookmark 'end-position end)
         (bookmarkp-maybe-save-bookmark)
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
             (let ((end-win  (save-excursion
                               (goto-line (+ (bookmarkp-line-number-at-pos) (window-height)))
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
  "Jump to W3m bookmark BOOKMARK, setting a new tab."
  (let ((buf   (bookmarkp-w3m-set-new-buffer-name)))
    (w3m-browse-url (bookmark-prop-get bookmark 'filename) 'newsession)
    (while (not (get-buffer buf)) (sit-for 1)) ; Be sure we have the W3m buffer.
    (with-current-buffer buf
      (goto-char (point-min))
      ;; Wait until data arrives in buffer, before setting region.
      (while (eq (line-beginning-position) (line-end-position)) (sit-for 1)))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

(defun bookmarkp-jump-w3m-only-one-tab (bookmark)
  "Close all W3m sessions and jump to BOOKMARK in a new W3m buffer."
  (w3m-quit 'force)                     ; Be sure we start with an empty W3m buffer.
  (w3m-browse-url (bookmark-prop-get bookmark 'filename))
  (with-current-buffer "*w3m*" (while (eq (point-min) (point-max)) (sit-for 1)))
  (bookmark-default-handler `("" (buffer . ,(buffer-name (current-buffer))) .
                              ,(bookmark-get-bookmark-record bookmark))))

(defun bookmarkp-jump-w3m (bookmark)
  "Handler function for record returned by `bookmarkp-make-w3m-record'.
BOOKMARK is a bookmark name or a bookmark record.
Use multi-tabs in W3m if `bookmarkp-w3m-allow-multi-tabs' is non-nil."
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

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'bookmark+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bookmark+.el ends here

